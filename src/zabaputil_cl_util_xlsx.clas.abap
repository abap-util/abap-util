"! <p class="shorttext synchronized">util - xlsx reader and writer</p>
"! An Excel workbook (.xlsx, Office Open XML) written from internal tables and
"! read into them - no abap2xlsx, no XCO. What the class needs of the system
"! (ZIP, UTF-8, the XML reader, RTTI) it takes from zabaputil_cl_util_context,
"! so it runs on every release down to 7.02 and on ABAP Cloud.
"!
"! Writing - one sheet per internal table, the file as an xstring:
"!
"!     DATA(lv_file) = zabaputil_cl_util_xlsx=>factory(
"!         )->add_sheet( val  = lt_orders
"!                       name = `Orders`
"!         )->add_sheet( val    = lt_items
"!                       name   = `Items`
"!                       layout = VALUE #( freeze_header = abap_true
"!                                         table_style   = `TableStyleMedium2` )
"!         )->get_xlsx( ).
"!
"! A number is a number cell, a date a date, a time a time, a timestamp a
"! date with its time, a flag (abap_bool) a boolean; a NUMC keeps its leading
"! zeros as text. A component that is a structure, a table or a reference is
"! left out.
"!
"! Reading - into a typed table by the header row, into a table of strings
"! whose columns are the header row, or cell by cell:
"!
"!     DATA(lo_xlsx) = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
"!     lo_xlsx->get_table( EXPORTING sheet  = `Orders`
"!                         IMPORTING result = lt_orders ).
"!     DATA(lr_any) = lo_xlsx->get_table_ref( index = 2 ).
"!     DATA(lt_cell) = lo_xlsx->get_cells( ).
"!
"! The layout of a sheet also places it (start_cell), names and styles its
"! Excel table, gives it formula columns, a totals row, styles, links and
"! validations by column, and sets the sheet up for the screen and for
"! printing - see ty_s_layout and ty_s_column.
"!
"! Around the tables - a title, a second table, a footer, a sheet without a
"! table - the sheet added last takes single cells, merged ranges, rows and
"! columns of their own, validations and names:
"!
"!     zabaputil_cl_util_xlsx=>factory(
"!         )->add_sheet( name = `Report`
"!         )->set_cell( row   = 1
"!                      col   = 1
"!                      val   = `Orders 2024`
"!                      style = VALUE #( bold = abap_true size = 14 )
"!         )->merge( `A1:F1`
"!         )->add_table( val    = lt_orders
"!                       layout = VALUE #( start_cell = `A3` ) ).
"!
"! A sheet stays open until the next add_sheet( ) or get_xlsx( ) and is
"! kept until then as its rows, rendered to text - not as the tables.
"!
"! Not in it: conditional formatting, comments, images, charts, a
"! protected sheet, an .xls file of Excel 97-2003, a workbook with a
"! password, changing a workbook that was read.
CLASS zabaputil_cl_util_xlsx DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.

    TYPES:
      "! how a cell looks - initial: as Excel shows a cell. A colour is RGB
      "! in six hex digits (`C00000`); a border is thin, medium, thick,
      "! dashed, dotted, double or hair
      BEGIN OF ty_s_style,
        bold          TYPE abap_bool,
        italic        TYPE abap_bool,
        underline     TYPE abap_bool,
        strike        TYPE abap_bool,
        "! the font size in points - 0: 11
        size          TYPE i,
        "! the font - empty: Calibri
        font          TYPE string,
        "! the colour of the text
        color         TYPE string,
        "! the colour of the background
        fill          TYPE string,
        "! the border on all four sides
        border        TYPE string,
        "! the border on one side - instead of border there
        border_top    TYPE string,
        border_bottom TYPE string,
        border_left   TYPE string,
        border_right  TYPE string,
        border_color  TYPE string,
        "! left, center, right, fill, justify or centerContinuous
        halign        TYPE string,
        "! top, center or bottom
        valign        TYPE string,
        "! the text wraps in the width of the column
        wrap          TYPE abap_bool,
        indent        TYPE i,
        "! the text turned by degrees: 1 to 90 upwards, 91 to 180 downwards
        "! (91 is one degree down), 255 letters stacked
        rotation      TYPE i,
      END OF ty_s_style.

    TYPES:
      "! what a cell takes when it is typed in Excel (a data validation)
      BEGIN OF ty_s_validation,
        "! list, whole, decimal, date, time, textLength or custom
        type        TYPE string,
        "! between (initial), notBetween, equal, notEqual, greaterThan,
        "! lessThan, greaterThanOrEqual or lessThanOrEqual - not for a list
        "! or custom
        operator    TYPE string,
        "! the values of a list - together at most 255 characters
        t_value     TYPE string_table,
        "! a value or formula as Excel writes it: the cells of a list
        "! (`Lists!$A$1:$A$9`), the bound of a number, a date as its
        "! serial number or `DATE(2024,1,31)`, the formula of custom
        formula1    TYPE string,
        "! the upper bound of between and notBetween
        formula2    TYPE string,
        "! an empty cell is not taken
        no_blank    TYPE abap_bool,
        "! a list without its dropdown button
        no_dropdown TYPE abap_bool,
        "! the message shown while the cell is selected
        prompt_title TYPE string,
        prompt      TYPE string,
        "! the message shown for a value that is not taken
        error_title TYPE string,
        error       TYPE string,
        "! stop (initial: the value is refused), warning or information
        error_style TYPE string,
      END OF ty_s_validation.

    TYPES:
      "! the document properties of a workbook (File - Info in Excel)
      BEGIN OF ty_s_properties,
        title       TYPE string,
        subject     TYPE string,
        creator     TYPE string,
        keywords    TYPE string,
        description TYPE string,
        category    TYPE string,
        company     TYPE string,
        "! empty: abap-util
        application TYPE string,
        "! a time stamp YYYYMMDDhhmmss in UTC - 0: now
        created     TYPE p LENGTH 8 DECIMALS 0,
      END OF ty_s_properties.

    TYPES:
      "! a column of a sheet that is written, or the header of a column that
      "! is read into a component of another name
      BEGIN OF ty_s_column,
        "! the component of the row
        name    TYPE string,
        "! the text of the header cell - empty: the component name
        header  TYPE string,
        "! a number format as Excel writes it (`#,##0.00`, `0.0%`,
        "! `dd.mm.yyyy`) - empty: by the type of the component
        format  TYPE string,
        "! the width in characters - 0: by the content
        width   TYPE i,
        "! a formula instead of a component: every cell of the column holds
        "! it, with `{row}` for the number of its row (`C{row}*D{row}`). Such
        "! a column needs no name, and Excel works its values out when the
        "! file is opened
        formula TYPE string,
        "! the cell of the column in a totals row under the rows: `sum`,
        "! `average`, `count`, `countNums`, `min`, `max`, `stdDev` or `var` -
        "! any other text is a label (`Total`). One column with it gives
        "! the sheet the row
        total   TYPE string,
        "! how the cells of the column look - a number format stays format
        style   TYPE ty_s_style,
        "! the column is hidden
        hidden  TYPE abap_bool,
        "! the column one level down in an outline (1 to 7), to be folded
        outline_level TYPE i,
        "! the component that holds the target of a link from the cell: a
        "! URL (`https://...`, `mailto:...`) or a place in the workbook
        "! (`#Sheet2!A1`). The column's own name makes the value its link
        link    TYPE string,
        "! what a cell of the column takes when it is typed in Excel
        validation TYPE ty_s_validation,
      END OF ty_s_column.
    TYPES ty_t_column TYPE STANDARD TABLE OF ty_s_column WITH EMPTY KEY.

    TYPES:
      "! how a sheet is written - initial: every elementary component in its
      "! order under a bold header row of the component names
      BEGIN OF ty_s_layout,
        "! the columns and their order - empty: every elementary component
        t_column       TYPE ty_t_column,
        "! no header row, the data starts in the first row
        no_header      TYPE abap_bool,
        "! the header from the data element (medium text) where a component
        "! has one and the column names no header itself
        ddic_header    TYPE abap_bool,
        "! the header row stays in place while the rows scroll
        freeze_header  TYPE abap_bool,
        "! filter buttons on the header row
        auto_filter    TYPE abap_bool,
        "! the rows as an Excel table of that style (`TableStyleMedium2`,
        "! `TableStyleLight9`, ...), with stripes and filter buttons. Needs
        "! the header row and at least one row
        table_style    TYPE string,
        "! the name of the Excel table, as a formula names it - empty:
        "! Table1, Table2, ... What Excel does not take in a name is made
        "! valid
        table_name     TYPE string,
        "! the Excel table without the stripes of its rows
        no_row_stripes TYPE abap_bool,
        "! the Excel table with stripes down its columns
        column_stripes TYPE abap_bool,
        "! the Excel table without its filter buttons
        no_filter      TYPE abap_bool,
        "! the cell the header row starts in (`B3`) - empty: A1
        start_cell     TYPE string,
        "! how the header row looks - bold where the sheet has no Excel table
        header_style   TYPE ty_s_style,
        "! the height of the rows in points - 0: as Excel sets it
        row_height     TYPE i,
        "! the rows one level down in an outline (1 to 7), to be folded
        outline_level  TYPE i,
        "! a timestamp is written in this time zone (an SAP key: `CET`,
        "! `EST`) - empty: as it is stored, in UTC
        time_zone      TYPE string,

        " -- the sheet: of the layout add_sheet( ) gets, not add_table( )

        "! the rows above and the columns left of this cell (`B2`) stay in
        "! place while the sheet scrolls - instead of freeze_header
        freeze_cell    TYPE string,
        "! the sheet is hidden
        hidden         TYPE abap_bool,
        "! the colour of the sheet's tab, RGB in six hex digits
        tab_color      TYPE string,
        "! no gridlines on the screen
        no_gridlines   TYPE abap_bool,
        "! the zoom in percent, 10 to 400 - 0: 100
        zoom           TYPE i,
        "! printed across the page
        landscape      TYPE abap_bool,
        "! the paper as Excel numbers it: 9 A4, 8 A3, 1 Letter, 5 Legal -
        "! 0: the printer's
        paper_size     TYPE i,
        "! printed on this many pages across - 0 with fit_height set: as many
        "! as it takes
        fit_width      TYPE i,
        "! printed on this many pages down - 0 with fit_width set: as many
        "! as it takes
        fit_height     TYPE i,
        "! the margin on all four sides in millimetres - 0: Excel's
        margin         TYPE i,
        "! what is printed (`A1:F40`) - empty: every cell that has a value
        print_area     TYPE string,
        "! the header row of the table is printed on top of every page
        repeat_header  TYPE abap_bool,
        "! the text on top of every printed page, with Excel's codes: &L,
        "! &C and &R in front of the left, centre and right part, &P the
        "! page, &N the pages, &D the date, &A the sheet. Without one of
        "! &L &C &R it stands in the centre
        header_text    TYPE string,
        "! the text at the bottom of every printed page - as header_text
        footer_text    TYPE string,
      END OF ty_s_layout.

    TYPES:
      "! a cell as get_cells( ) reads it. value by kind: a number without
      "! exponent and with a period (`-12.5`), a date `YYYY-MM-DD`, a time
      "! `HH:MM:SS`, a timestamp `YYYY-MM-DDTHH:MM:SS`, a boolean `X` or
      "! empty, an error as Excel shows it (`#DIV/0!`)
      BEGIN OF ty_s_cell,
        row   TYPE i,
        col   TYPE i,
        kind  TYPE string,
        value TYPE string,
      END OF ty_s_cell.
    TYPES ty_t_cell TYPE STANDARD TABLE OF ty_s_cell WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF cs_kind,
        text      TYPE string VALUE `S` ##NO_TEXT,
        number    TYPE string VALUE `N` ##NO_TEXT,
        date      TYPE string VALUE `D` ##NO_TEXT,
        time      TYPE string VALUE `T` ##NO_TEXT,
        timestamp TYPE string VALUE `M` ##NO_TEXT,
        boolean   TYPE string VALUE `B` ##NO_TEXT,
        error     TYPE string VALUE `E` ##NO_TEXT,
      END OF cs_kind.

    "! the MIME type of an .xlsx file - for a download or a mail attachment
    CONSTANTS mime_type TYPE string
      VALUE `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` ##NO_TEXT.

    "! a new workbook, to be filled with add_sheet( )
    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! the workbook of a file, to be read - zabaputil_cx_util_error when the
    "! file is no .xlsx workbook
    CLASS-METHODS factory_by_xlsx
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! a workbook of one sheet, in one call
    CLASS-METHODS xlsx_by_itab
      IMPORTING
        val           TYPE ANY TABLE
        layout        TYPE ty_s_layout OPTIONAL
      RETURNING
        VALUE(result) TYPE xstring.

    "! the first sheet of a file into a table, by its header row - see
    "! get_table( )
    CLASS-METHODS itab_by_xlsx
      IMPORTING
        val    TYPE xstring
      EXPORTING
        result TYPE ANY TABLE.

    "! One more sheet, with the rows of val where it is given - rendered
    "! at once, so val is not kept. The methods below (add_table( ),
    "! set_cell( ), merge( ), ...) write to the sheet added last. A name
    "! Excel does not take (longer than 31 characters, one of \ / ? * [ ] :,
    "! a name the workbook has) is made valid.
    "! @parameter val | the rows - not given: a sheet without a table
    "! @parameter name | empty: Sheet1, Sheet2, ...
    "! @parameter layout | the table and the sheet (the fields below "the sheet")
    METHODS add_sheet
      IMPORTING
        val           TYPE ANY TABLE   OPTIONAL
        name          TYPE clike       OPTIONAL
        layout        TYPE ty_s_layout OPTIONAL
      PREFERRED PARAMETER val
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! One more table on the sheet added last, at layout-start_cell (A1
    "! when it is empty): a second table, or one under a title. The fields
    "! of the layout below "the sheet" are taken from add_sheet( ) only; the
    "! first table with a header is the one freeze_header and repeat_header
    "! go by, and a sheet has one filter - an auto_filter on a second table
    "! raises, as does a cell of the table that is taken already.
    METHODS add_table
      IMPORTING
        val           TYPE ANY TABLE
        layout        TYPE ty_s_layout OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! One cell of the sheet added last - a title, a label, a note under a
    "! table. The value is written by its type like a cell of a table (a
    "! number, a date, a flag); raises when the cell is taken already.
    "! @parameter row | 1 to 1048576
    "! @parameter col | 1 (A) to 16384 (XFD) - column_to_number( `C` ) gives it
    "! @parameter val | the value - not given: an empty cell with a style or a formula
    "! @parameter formula | a formula instead of a value (`SUM(B2:B9)`), without =
    "! @parameter format | a number format (`#,##0.00`)
    "! @parameter link | a URL or a place in the workbook (`#Sheet2!A1`)
    METHODS set_cell
      IMPORTING
        row           TYPE i
        col           TYPE i
        val           TYPE any         OPTIONAL
        formula       TYPE clike       OPTIONAL
        format        TYPE clike       OPTIONAL
        style         TYPE ty_s_style  OPTIONAL
        link          TYPE clike       OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! The height, visibility or outline level of a row of the sheet added
    "! last - instead of what its table set
    "! @parameter height | in points - 0: as Excel sets it
    METHODS set_row
      IMPORTING
        row           TYPE i
        height        TYPE i         OPTIONAL
        hidden        TYPE abap_bool OPTIONAL
        outline_level TYPE i         OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! The width, visibility or outline level of a column of the sheet
    "! added last - instead of what its table set
    "! @parameter width | in characters - 0: as the table set it, or Excel's
    METHODS set_column
      IMPORTING
        col           TYPE i
        width         TYPE i         OPTIONAL
        hidden        TYPE abap_bool OPTIONAL
        outline_level TYPE i         OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! The cells of a range (`A1:D1`) as one cell of the sheet added last,
    "! which shows the value of the first. Raises for a range that overlaps
    "! one merged already
    METHODS merge
      IMPORTING
        range         TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! What the cells of a range (`B2:B100`) of the sheet added last take
    "! when they are typed in Excel
    METHODS add_validation
      IMPORTING
        range         TYPE clike
        validation    TYPE ty_s_validation
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! A name for a range, as a formula names it (`Rates` for
    "! `'Rates'!$A$2:$B$20`). Raises for a name that is none, or one the
    "! workbook has
    "! @parameter ref | what the name stands for, as Excel writes it
    "! @parameter local | the name holds on the sheet added last only
    METHODS add_name
      IMPORTING
        name          TYPE clike
        ref           TYPE clike
        local         TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! the title, the author and the other document properties
    METHODS set_properties
      IMPORTING
        val           TYPE ty_s_properties
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! the file of the workbook built with add_sheet( )
    METHODS get_xlsx
      RETURNING
        VALUE(result) TYPE xstring.

    "! the names of the sheets, in the order of the workbook
    METHODS get_sheet_names
      RETURNING
        VALUE(result) TYPE string_table.

    "! Every cell of a sheet that holds a value, row by row.
    "! @parameter sheet | the name of the sheet - empty: the sheet at index
    METHODS get_cells
      IMPORTING
        sheet         TYPE clike OPTIONAL
        index         TYPE i     DEFAULT 1
      RETURNING
        VALUE(result) TYPE ty_t_cell.

    "! A sheet into a table. A column goes to the component its header
    "! names - the component name, or the header t_column gives for it;
    "! upper and lower case and the blanks around make no difference, a
    "! column without a component is left out. Every value is converted to
    "! the type of its component; a cell that does not fit (a text in a
    "! number, a number in a date) raises with its address.
    "! @parameter header_row | 0: no header, the n-th column goes to the n-th component
    METHODS get_table
      IMPORTING
        sheet      TYPE clike       OPTIONAL
        index      TYPE i           DEFAULT 1
        header_row TYPE i           DEFAULT 1
        t_column   TYPE ty_t_column OPTIONAL
      EXPORTING
        result     TYPE ANY TABLE.

    "! A sheet as a table of strings whose components are named after the
    "! header row (`Order Id` becomes ORDER_ID; a column without a header,
    "! or all of them with header_row 0, after its letters A, B, ...).
    "! Values as get_cells( ) gives them. Unbound for a sheet without a cell
    METHODS get_table_ref
      IMPORTING
        sheet         TYPE clike OPTIONAL
        index         TYPE i     DEFAULT 1
        header_row    TYPE i     DEFAULT 1
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! 1 gives A, 27 gives AA, 16384 gives XFD
    CLASS-METHODS column_to_letters
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! A gives 1, AA gives 27 - of a cell address (`$AB$12`) the column
    CLASS-METHODS column_to_number
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

    " what a component is for a cell - cs_kind, and two more that are
    " text in the file but not in ABAP
    CONSTANTS:
      BEGIN OF cs_cat,
        text      TYPE string VALUE `S` ##NO_TEXT,
        number    TYPE string VALUE `N` ##NO_TEXT,
        date      TYPE string VALUE `D` ##NO_TEXT,
        time      TYPE string VALUE `T` ##NO_TEXT,
        timestamp TYPE string VALUE `M` ##NO_TEXT,
        boolean   TYPE string VALUE `B` ##NO_TEXT,
        numc      TYPE string VALUE `C` ##NO_TEXT,
        hex       TYPE string VALUE `X` ##NO_TEXT,
      END OF cs_cat.

    " the number formats Excel has built in (styles.xml names them by id
    " only). 14 and 22 show a date the way the user's Excel is set up
    CONSTANTS:
      BEGIN OF cs_numfmt,
        general  TYPE i VALUE 0,
        date     TYPE i VALUE 14,
        time     TYPE i VALUE 21,
        datetime TYPE i VALUE 22,
        custom   TYPE i VALUE 164,
      END OF cs_numfmt.

    CONSTANTS:
      BEGIN OF cs_ns,
        main    TYPE string VALUE `http://schemas.openxmlformats.org/spreadsheetml/2006/main` ##NO_TEXT,
        rel     TYPE string VALUE `http://schemas.openxmlformats.org/officeDocument/2006/relationships` ##NO_TEXT,
        pkg_rel TYPE string VALUE `http://schemas.openxmlformats.org/package/2006/relationships` ##NO_TEXT,
        types   TYPE string VALUE `http://schemas.openxmlformats.org/package/2006/content-types` ##NO_TEXT,
        ct      TYPE string VALUE `application/vnd.openxmlformats-officedocument.spreadsheetml` ##NO_TEXT,
      END OF cs_ns.

    CONSTANTS cv_xml_head TYPE string VALUE `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>` ##NO_TEXT.
    CONSTANTS cv_letters  TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZ` ##NO_TEXT.
    CONSTANTS cv_hex      TYPE string VALUE `0123456789ABCDEF` ##NO_TEXT.
    CONSTANTS cv_digits   TYPE string VALUE `0123456789` ##NO_TEXT.
    CONSTANTS cv_max_row  TYPE i VALUE 1048576.
    CONSTANTS cv_max_col  TYPE i VALUE 16384.
    " day 0 of the Excel date system: with it the serial number of every
    " date from 1900-03-01 on is the difference in days (Excel counts a
    " 1900-02-29 that never was)
    CONSTANTS cv_date_base TYPE d VALUE '18991230'.
    CONSTANTS cv_date_1900 TYPE d VALUE '19000101'.
    CONSTANTS cv_date_march TYPE d VALUE '19000301'.
    CONSTANTS cv_date_1904 TYPE d VALUE '19040101'.
    CONSTANTS cv_date_max  TYPE d VALUE '99991231'.

    " a component of the row, by name and by what it is for a cell
    TYPES:
      BEGIN OF ty_s_field,
        name     TYPE string,
        cat      TYPE string,
        rollname TYPE string,
      END OF ty_s_field.
    TYPES ty_t_field TYPE STANDARD TABLE OF ty_s_field WITH EMPTY KEY.

    " a column while its sheet is written
    TYPES:
      BEGIN OF ty_s_out_col,
        name        TYPE string,
        header      TYPE string,
        letters     TYPE string,
        cat         TYPE string,
        style       TYPE string,
        width       TYPE i,
        len         TYPE i,
        formula     TYPE string,
        " the totals row: the function and its number in SUBTOTAL( ), or a
        " label - and what the rows add up to, for the value the cell
        " shows before Excel has worked the formula out
        total_fn    TYPE string,
        total_code  TYPE i,
        total_label TYPE string,
        sum         TYPE decfloat34,
        low         TYPE decfloat34,
        high        TYPE decfloat34,
        cells       TYPE i,
        numbers     TYPE i,
        " the component with the target of a link, and the style of the
        " cell that has one
        link        TYPE string,
        link_style  TYPE string,
        " the style without a number format - of a count in the totals row
        plain_style TYPE string,
        hidden      TYPE abap_bool,
        level       TYPE i,
        validation  TYPE ty_s_validation,
      END OF ty_s_out_col.
    TYPES ty_t_out_col TYPE STANDARD TABLE OF ty_s_out_col WITH EMPTY KEY.

    " a part that belongs to a sheet (an Excel table)
    TYPES:
      BEGIN OF ty_s_out_part,
        id  TYPE i,
        xml TYPE xstring,
      END OF ty_s_out_part.
    TYPES ty_t_out_part TYPE STANDARD TABLE OF ty_s_out_part WITH EMPTY KEY.

    " a sheet that is written: its part, its relationships and its tables
    TYPES:
      BEGIN OF ty_s_out_sheet,
        name    TYPE string,
        hidden  TYPE abap_bool,
        xml     TYPE xstring,
        rels    TYPE xstring,
        t_table TYPE ty_t_out_part,
      END OF ty_s_out_sheet.

    " the cells of one row of the open sheet that stand together - the
    " rows of a table, one free cell - rendered already. The rows of a
    " sheet are kept as these strings until it is closed
    TYPES:
      BEGIN OF ty_s_seg,
        row    TYPE i,
        col    TYPE i,
        col_to TYPE i,
        attr   TYPE string,
        xml    TYPE string,
      END OF ty_s_seg.
    TYPES ty_t_seg TYPE SORTED TABLE OF ty_s_seg WITH UNIQUE KEY row col.

    " a row or a column of the open sheet with what set_row( ),
    " set_column( ) or a table set for it
    TYPES:
      BEGIN OF ty_s_line,
        pos    TYPE i,
        size   TYPE i,
        len    TYPE i,
        auto   TYPE abap_bool,
        hidden TYPE abap_bool,
        level  TYPE i,
      END OF ty_s_line.
    TYPES ty_t_line TYPE SORTED TABLE OF ty_s_line WITH UNIQUE KEY pos.

    TYPES:
      BEGIN OF ty_s_area,
        row1 TYPE i,
        col1 TYPE i,
        row2 TYPE i,
        col2 TYPE i,
      END OF ty_s_area.
    TYPES ty_t_area TYPE STANDARD TABLE OF ty_s_area WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_link,
        ref    TYPE string,
        target TYPE string,
      END OF ty_s_link.
    TYPES ty_t_link TYPE STANDARD TABLE OF ty_s_link WITH EMPTY KEY.

    " the sheet that is written now
    TYPES:
      BEGIN OF ty_s_cur,
        name        TYPE string,
        layout      TYPE ty_s_layout,
        t_seg       TYPE ty_t_seg,
        t_row       TYPE ty_t_line,
        t_col       TYPE ty_t_line,
        col_min     TYPE i,
        col_max     TYPE i,
        pane        TYPE string,
        filter      TYPE string,
        t_merge     TYPE ty_t_area,
        t_valid     TYPE string_table,
        t_link      TYPE ty_t_link,
        t_table     TYPE ty_t_out_part,
        header_rows TYPE string,
        row_level   TYPE i,
        col_level   TYPE i,
      END OF ty_s_cur.

    " a defined name of the workbook - sheet 0: of the workbook
    TYPES:
      BEGIN OF ty_s_name,
        name  TYPE string,
        sheet TYPE i,
        ref   TYPE string,
      END OF ty_s_name.

    " a cell format of styles.xml - the row index less one is what a cell
    " names in its s attribute. font, fill and border are indexes of the
    " lists behind them
    TYPES:
      BEGIN OF ty_s_xf,
        numfmt   TYPE i,
        font     TYPE i,
        fill     TYPE i,
        border   TYPE i,
        halign   TYPE string,
        valign   TYPE string,
        wrap     TYPE abap_bool,
        indent   TYPE i,
        rotation TYPE i,
      END OF ty_s_xf.

    TYPES:
      BEGIN OF ty_s_font,
        bold      TYPE abap_bool,
        italic    TYPE abap_bool,
        underline TYPE abap_bool,
        strike    TYPE abap_bool,
        size      TYPE i,
        name      TYPE string,
        color     TYPE string,
      END OF ty_s_font.

    TYPES:
      BEGIN OF ty_s_border,
        top    TYPE string,
        bottom TYPE string,
        left   TYPE string,
        right  TYPE string,
        color  TYPE string,
      END OF ty_s_border.

    TYPES:
      BEGIN OF ty_s_sheet,
        name TYPE string,
        path TYPE string,
      END OF ty_s_sheet.

    TYPES:
      BEGIN OF ty_s_rel,
        id     TYPE string,
        type   TYPE string,
        target TYPE string,
      END OF ty_s_rel.
    TYPES ty_t_rel TYPE STANDARD TABLE OF ty_s_rel WITH EMPTY KEY.

    " writing
    DATA mt_out_sheet TYPE STANDARD TABLE OF ty_s_out_sheet WITH EMPTY KEY.
    DATA mt_xf        TYPE STANDARD TABLE OF ty_s_xf WITH EMPTY KEY.
    DATA mt_font      TYPE STANDARD TABLE OF ty_s_font WITH EMPTY KEY.
    DATA mt_fill      TYPE string_table.
    DATA mt_border    TYPE STANDARD TABLE OF ty_s_border WITH EMPTY KEY.
    DATA mt_numfmt    TYPE string_table.
    DATA mv_tables    TYPE i.
    DATA mt_table_name TYPE string_table.
    DATA mv_formulas  TYPE abap_bool.
    DATA mv_open      TYPE abap_bool.
    DATA ms_cur       TYPE ty_s_cur.
    DATA mt_name      TYPE STANDARD TABLE OF ty_s_name WITH EMPTY KEY.
    DATA mv_props     TYPE abap_bool.
    DATA ms_props     TYPE ty_s_properties.

    " reading
    DATA mv_loaded     TYPE abap_bool.
    DATA mt_part       TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA mt_sheet      TYPE STANDARD TABLE OF ty_s_sheet WITH EMPTY KEY.
    DATA mt_shared     TYPE string_table.
    DATA mt_style_kind TYPE string_table.
    DATA mv_date1904   TYPE abap_bool.

    " ----- writing

    "! the elementary components of a row - of a row that is no structure
    "! the one field TABLE_LINE
    METHODS fields_get
      IMPORTING
        row           TYPE any
      EXPORTING
        elementary    TYPE abap_bool
        result        TYPE ty_t_field.

    "! what a type is for a cell - empty for one that is none (a structure,
    "! a table, a reference)
    METHODS category_get
      IMPORTING
        type_kind     TYPE clike
        name          TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    METHODS columns_get
      IMPORTING
        t_field       TYPE ty_t_field
        layout        TYPE ty_s_layout
        offset        TYPE i
      RETURNING
        VALUE(result) TYPE ty_t_out_col.

    "! the field of that name - initial when the row has none
    CLASS-METHODS field_find
      IMPORTING
        t_field       TYPE ty_t_field
        name          TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_field.

    "! a function of a totals row and its number in SUBTOTAL( ) - code 0
    "! for a text that is none, a label
    CLASS-METHODS total_function
      IMPORTING
        val  TYPE string
      EXPORTING
        name TYPE string
        code TYPE i.

    "! what a totals cell shows before Excel has worked its formula out -
    "! empty for a function that is not worked out here
    METHODS total_value
      IMPORTING
        col           TYPE ty_s_out_col
      RETURNING
        VALUE(result) TYPE string.

    "! a name Excel takes for a table, and that no other table of the
    "! workbook has
    METHODS table_name_get
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! the index of a cell format, added when the workbook has none like it
    METHODS style_get
      IMPORTING
        id            TYPE i          OPTIONAL
        code          TYPE string     OPTIONAL
        style         TYPE ty_s_style OPTIONAL
      RETURNING
        VALUE(result) TYPE i.

    "! the s attribute of a cell in that format - empty for format 0
    METHODS style_attr
      IMPORTING
        id            TYPE i          OPTIONAL
        code          TYPE string     OPTIONAL
        style         TYPE ty_s_style OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! the number format a cell of that category is shown in by default
    CLASS-METHODS numfmt_by_cat
      IMPORTING
        cat           TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS font_get
      IMPORTING
        val           TYPE ty_s_font
      RETURNING
        VALUE(result) TYPE i.

    METHODS fill_get
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS border_get
      IMPORTING
        val           TYPE ty_s_border
      RETURNING
        VALUE(result) TYPE i.

    "! a colour as styles.xml names it (`FFC00000`) - raises for one that
    "! is no six hex digits
    CLASS-METHODS color_get
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! a border style Excel knows - raises for another
    CLASS-METHODS border_style
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    " ----- the open sheet

    METHODS sheet_open
      IMPORTING
        name   TYPE clike
        layout TYPE ty_s_layout.

    "! the open sheet rendered into mt_out_sheet
    METHODS sheet_close.

    "! raises when no sheet is open
    METHODS sheet_check.

    "! the rows of a table into the open sheet
    METHODS table_write
      IMPORTING
        val    TYPE ANY TABLE
        layout TYPE ty_s_layout.

    "! the c element of a value by its category - xml empty for a value
    "! that is no cell (an initial date). text is what the cell shows, for
    "! the width of its column
    METHODS cell_xml
      IMPORTING
        address   TYPE string
        cat       TYPE string
        style     TYPE string
        val       TYPE any
        time_zone TYPE string OPTIONAL
      EXPORTING
        xml       TYPE string
        text      TYPE string.

    "! cells of one row into the open sheet
    METHODS seg_add
      IMPORTING
        row    TYPE i
        col    TYPE i
        col_to TYPE i
        attr   TYPE string OPTIONAL
        xml    TYPE string.

    "! raises when a cell of the area holds a value of the open sheet
    METHODS seg_check
      IMPORTING
        area TYPE ty_s_area.

    "! the content width of a column of the open sheet, as a table sees it
    METHODS col_len
      IMPORTING
        col  TYPE i
        len  TYPE i
        size TYPE i OPTIONAL.

    "! a range (`A1:D5`, `$B$2`) as its corners - raises for one that is none
    CLASS-METHODS area_parse
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_area.

    "! `A1:D5`, `A1` for a single cell, `$A$1:$D$5` absolute
    CLASS-METHODS area_text
      IMPORTING
        val           TYPE ty_s_area
        absolute      TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! a sheet name as a formula names it: `'Q1 2024'`
    CLASS-METHODS sheet_quote
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! the attributes of a row element: height, hidden, outline level
    CLASS-METHODS line_attr
      IMPORTING
        val           TYPE ty_s_line
      RETURNING
        VALUE(result) TYPE string.

    "! raises for a row or a column that is no row or column of a sheet
    CLASS-METHODS cell_check
      IMPORTING
        row TYPE i
        col TYPE i.

    "! a validation as a dataValidation element over sqref
    CLASS-METHODS validation_xml
      IMPORTING
        sqref         TYPE string
        val           TYPE ty_s_validation
      RETURNING
        VALUE(result) TYPE string.

    "! a name Excel takes for a defined name, raises for one it does not
    METHODS name_check
      IMPORTING
        name          TYPE string
        sheet         TYPE i.

    "! the parts docProps/core.xml and docProps/app.xml
    METHODS props_xml
      EXPORTING
        core TYPE xstring
        app  TYPE xstring.

    "! a time stamp YYYYMMDDhhmmss as W3CDTF: 2024-03-15T10:30:00Z
    CLASS-METHODS iso_by_stamp
      IMPORTING
        val           TYPE p
      RETURNING
        VALUE(result) TYPE string.

    "! a number of the layout in bounds - raises for one out of them
    CLASS-METHODS bounds_check
      IMPORTING
        val  TYPE i
        low  TYPE i
        high TYPE i
        name TYPE string.

    METHODS sheet_name_get
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    METHODS cell_text
      IMPORTING
        address       TYPE string
        val           TYPE string
        style         TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS styles_xml
      RETURNING
        VALUE(result) TYPE string.

    METHODS table_xml
      IMPORTING
        id            TYPE i
        name          TYPE string
        range         TYPE string
        filter        TYPE string
        layout        TYPE ty_s_layout
        totals        TYPE abap_bool
        t_col         TYPE ty_t_out_col
        first_row     TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! the serial number of a date - 0 for one Excel has none for (initial,
    "! before 1900, no date at all)
    CLASS-METHODS date_to_serial
      IMPORTING
        val           TYPE d
      RETURNING
        VALUE(result) TYPE i.

    " ----- reading

    METHODS load
      IMPORTING
        val TYPE xstring.

    METHODS part_get
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS rels_read
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_rel.

    METHODS shared_read
      IMPORTING
        val TYPE xstring.

    METHODS styles_read
      IMPORTING
        val TYPE xstring.

    "! the part of a sheet, by name or by position
    METHODS sheet_part
      IMPORTING
        sheet         TYPE clike
        index         TYPE i
      RETURNING
        VALUE(result) TYPE xstring.

    "! The cells of the next row that has any. row goes in as the row
    "! before and comes back as the row that was read
    METHODS sheet_read_row
      IMPORTING
        reader TYPE REF TO object
      EXPORTING
        t_cell TYPE ty_t_cell
        eof    TYPE abap_bool
      CHANGING
        row    TYPE i.

    "! what a c element holds: its text by the type the element names
    METHODS cell_read
      IMPORTING
        cell_type TYPE string
        style     TYPE string
        text      TYPE string
      EXPORTING
        kind      TYPE string
        value     TYPE string.

    "! a number cell by the format it is shown in: a number, a date, a time
    METHODS number_cell
      IMPORTING
        val   TYPE string
        style TYPE string
      EXPORTING
        kind  TYPE string
        value TYPE string.

    METHODS cell_to_field
      IMPORTING
        cell  TYPE ty_s_cell
        cat   TYPE string
      CHANGING
        field TYPE any.

    "! a path of the package: target as seen from the folder of base
    CLASS-METHODS path_resolve
      IMPORTING
        base          TYPE string
        target        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! the folder a part lies in, with the slash behind it
    CLASS-METHODS path_folder
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS attr
      IMPORTING
        t_attribute   TYPE zabaputil_cl_util_context=>ty_t_xml_attribute
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! what a number format shows: D a date, T a time, M both, L a time
    "! that runs past 24 hours, empty a number
    CLASS-METHODS format_kind
      IMPORTING
        id            TYPE i
        code          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! `1.5E-3` gives `0.0015`, `0.30000000000000004` gives `0.3`, `12.50`
    "! gives `12.5` - a number as it is stored in the file, without
    "! exponent and without the digits a binary float adds. Empty for what
    "! is no number
    "! @parameter digits | the digits that count: 15 are what a cell shows, 17 what it stores
    CLASS-METHODS number_plain
      IMPORTING
        val           TYPE string
        digits        TYPE i DEFAULT 15
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS serial_to_date
      IMPORTING
        val           TYPE i
        date1904      TYPE abap_bool
      RETURNING
        VALUE(result) TYPE d.

    "! `_x000D_` and its like as the character it stands for - how a file
    "! holds a character XML cannot
    CLASS-METHODS text_decode
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS char_by_code
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS hex_to_int
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE i.

    "! a byte, 0 to 255, as its two hex digits
    CLASS-METHODS byte_to_hex
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS two_digits
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! a valid component name out of a header text
    CLASS-METHODS name_by_header
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS digits_only
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zabaputil_cl_util_xlsx IMPLEMENTATION.

  METHOD factory.

    result = NEW #( ).

  ENDMETHOD.

  METHOD factory_by_xlsx.

    result = NEW #( ).
    result->load( val ).

  ENDMETHOD.

  METHOD xlsx_by_itab.

    result = factory( )->add_sheet( val    = val
                                    layout = layout )->get_xlsx( ).

  ENDMETHOD.

  METHOD itab_by_xlsx.

    factory_by_xlsx( val )->get_table( IMPORTING result = result ).

  ENDMETHOD.

  METHOD add_sheet.

    IF mv_loaded = abap_true.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_READ_ONLY - a workbook that was read takes no sheet`.
    ENDIF.

    sheet_close( ).
    sheet_open( name   = name
                layout = layout ).
    IF val IS SUPPLIED.
      table_write( val    = val
                   layout = layout ).
    ENDIF.

    result = me.

  ENDMETHOD.

  METHOD add_table.

    sheet_check( ).
    table_write( val    = val
                 layout = layout ).
    result = me.

  ENDMETHOD.

  METHOD set_cell.

    DATA lt_field TYPE ty_t_field.
    DATA lv_xml   TYPE string.
    DATA lv_cat   TYPE string.

    sheet_check( ).
    cell_check( row = row
                col = col ).
    seg_check( VALUE #( row1 = row
                        col1 = col
                        row2 = row
                        col2 = col ) ).
    DATA(lv_ref) = |{ column_to_letters( col ) }{ row }|.
    DATA(ls_style) = style.
    IF link IS NOT INITIAL AND ls_style-color IS INITIAL.
      ls_style-color = `0563C1`.
      ls_style-underline = abap_true.
    ENDIF.
    DATA(lv_code) = CONV string( format ).

    IF formula IS NOT INITIAL.
      DATA(lv_formula) = CONV string( formula ).
      IF lv_formula(1) = `=`.
        lv_formula = substring( val = lv_formula
                                off = 1 ).
      ENDIF.
      lv_formula = zabaputil_cl_util_context=>c_escape_xml( lv_formula ).
      DATA(lv_style) = style_attr( code  = lv_code
                                   style = ls_style ).
      lv_xml = |<c r="{ lv_ref }"{ lv_style }><f>{ lv_formula }</f></c>|.
      mv_formulas = abap_true.
    ELSEIF val IS SUPPLIED.
      fields_get( EXPORTING row    = val
                  IMPORTING result = lt_field ).
      IF lt_field IS INITIAL.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_CELL_VALUE - the value for { lv_ref } is no elementary value|.
      ENDIF.
      lv_cat = lt_field[ 1 ]-cat.
      IF lv_cat <> cs_cat-number AND lv_cat <> cs_cat-date AND lv_cat <> cs_cat-time
          AND lv_cat <> cs_cat-timestamp.
        CLEAR lv_code.
      ENDIF.
      lv_style = style_attr( id    = numfmt_by_cat( lv_cat )
                             code  = lv_code
                             style = ls_style ).
      cell_xml( EXPORTING address   = lv_ref
                          cat       = lv_cat
                          style     = lv_style
                          val       = val
                          time_zone = ms_cur-layout-time_zone
                IMPORTING xml       = lv_xml ).
    ENDIF.

    " a cell without a value that still looks like something: a border, a
    " fill in a merged title
    IF lv_xml IS INITIAL AND ( ls_style IS NOT INITIAL OR lv_code IS NOT INITIAL ).
      lv_style = style_attr( code  = lv_code
                             style = ls_style ).
      lv_xml = |<c r="{ lv_ref }"{ lv_style }/>|.
    ENDIF.
    IF link IS NOT INITIAL.
      INSERT VALUE #( ref    = lv_ref
                      target = link ) INTO TABLE ms_cur-t_link.
    ENDIF.
    IF lv_xml IS NOT INITIAL.
      seg_add( row    = row
               col    = col
               col_to = col
               xml    = lv_xml ).
    ENDIF.

    result = me.

  ENDMETHOD.

  METHOD set_row.

    FIELD-SYMBOLS <line> TYPE ty_s_line.

    sheet_check( ).
    cell_check( row = row
                col = 1 ).
    bounds_check( val  = height
                  low  = 0
                  high = 409
                  name = `row height` ).
    bounds_check( val  = outline_level
                  low  = 0
                  high = 7
                  name = `outline level` ).

    UNASSIGN <line>.
    ASSIGN ms_cur-t_row[ pos = row ] TO <line>.
    IF <line> IS NOT ASSIGNED.
      INSERT VALUE #( pos = row ) INTO TABLE ms_cur-t_row ASSIGNING <line>.
    ENDIF.
    <line>-size = height.
    <line>-hidden = hidden.
    <line>-level = outline_level.
    IF outline_level > ms_cur-row_level.
      ms_cur-row_level = outline_level.
    ENDIF.

    result = me.

  ENDMETHOD.

  METHOD set_column.

    FIELD-SYMBOLS <line> TYPE ty_s_line.

    sheet_check( ).
    cell_check( row = 1
                col = col ).
    bounds_check( val  = width
                  low  = 0
                  high = 255
                  name = `column width` ).
    bounds_check( val  = outline_level
                  low  = 0
                  high = 7
                  name = `outline level` ).

    UNASSIGN <line>.
    ASSIGN ms_cur-t_col[ pos = col ] TO <line>.
    IF <line> IS NOT ASSIGNED.
      INSERT VALUE #( pos = col ) INTO TABLE ms_cur-t_col ASSIGNING <line>.
    ENDIF.
    IF width > 0.
      <line>-size = width.
    ENDIF.
    <line>-hidden = hidden.
    <line>-level = outline_level.
    IF outline_level > ms_cur-col_level.
      ms_cur-col_level = outline_level.
    ENDIF.

    result = me.

  ENDMETHOD.

  METHOD merge.

    sheet_check( ).
    DATA(ls_area) = area_parse( range ).
    IF ls_area-row1 = ls_area-row2 AND ls_area-col1 = ls_area-col2.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_MERGE - { range } is one cell, nothing to merge|.
    ENDIF.
    LOOP AT ms_cur-t_merge REFERENCE INTO DATA(lr_merge)
         WHERE row1 <= ls_area-row2 AND row2 >= ls_area-row1
           AND col1 <= ls_area-col2 AND col2 >= ls_area-col1.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_MERGE - { range } overlaps { area_text( lr_merge->* ) }, merged already|.
    ENDLOOP.
    INSERT ls_area INTO TABLE ms_cur-t_merge.

    result = me.

  ENDMETHOD.

  METHOD add_validation.

    sheet_check( ).
    DATA(lv_ref) = area_text( area_parse( range ) ).
    INSERT validation_xml( sqref = lv_ref
                           val   = validation ) INTO TABLE ms_cur-t_valid.
    result = me.

  ENDMETHOD.

  METHOD add_name.

    DATA ls_name TYPE ty_s_name.

    IF local = abap_true.
      sheet_check( ).
      ls_name-sheet = lines( mt_out_sheet ) + 1.
    ENDIF.
    ls_name-name = zabaputil_cl_util_context=>c_trim( name ).
    ls_name-ref = ref.
    IF ls_name-ref IS NOT INITIAL AND ls_name-ref(1) = `=`.
      ls_name-ref = substring( val = ls_name-ref
                               off = 1 ).
    ENDIF.
    IF ls_name-ref IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_NAME - the name { ls_name-name } stands for nothing|.
    ENDIF.
    name_check( name  = ls_name-name
                sheet = ls_name-sheet ).
    INSERT ls_name INTO TABLE mt_name.

    result = me.

  ENDMETHOD.

  METHOD set_properties.

    ms_props = val.
    mv_props = abap_true.
    result = me.

  ENDMETHOD.

  METHOD sheet_check.

    IF mv_open = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NO_SHEET - add_sheet( ) first: there is no sheet to write to`.
    ENDIF.

  ENDMETHOD.

  METHOD sheet_open.

    DATA ls_area TYPE ty_s_area.

    CLEAR ms_cur.
    ms_cur-name = sheet_name_get( name ).
    ms_cur-layout = layout.
    mv_open = abap_true.

    bounds_check( val  = layout-zoom
                  low  = 0
                  high = 400
                  name = `zoom` ).
    IF layout-zoom > 0 AND layout-zoom < 10.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_LAYOUT - a zoom of { layout-zoom } percent is none, Excel takes 10 to 400|.
    ENDIF.
    bounds_check( val  = layout-fit_width
                  low  = 0
                  high = 32767
                  name = `fit_width` ).
    bounds_check( val  = layout-fit_height
                  low  = 0
                  high = 32767
                  name = `fit_height` ).
    bounds_check( val  = layout-margin
                  low  = 0
                  high = 500
                  name = `margin` ).
    IF layout-tab_color IS NOT INITIAL.
      color_get( layout-tab_color ).
    ENDIF.

    " frozen rows and columns: what lies above and left of the cell
    IF layout-freeze_cell IS NOT INITIAL.
      ls_area = area_parse( layout-freeze_cell ).
      DATA(lv_x) = ls_area-col1 - 1.
      DATA(lv_y) = ls_area-row1 - 1.
      DATA(lv_cell) = area_text( ls_area ).
      DATA(lv_split) = ``.
      IF lv_x > 0.
        lv_split = | xSplit="{ lv_x }"|.
      ENDIF.
      IF lv_y > 0.
        lv_split = |{ lv_split } ySplit="{ lv_y }"|.
      ENDIF.
      DATA(lv_pane) = `bottomRight`.
      IF lv_x = 0.
        lv_pane = `bottomLeft`.
      ENDIF.
      IF lv_y = 0.
        lv_pane = `topRight`.
      ENDIF.
      IF lv_split IS NOT INITIAL.
        ms_cur-pane = |<pane{ lv_split } topLeftCell="{ lv_cell }" activePane="{ lv_pane }" state="frozen"/>|.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD table_write.

    DATA lr_line    TYPE REF TO data.
    DATA lt_field   TYPE ty_t_field.
    DATA lv_cells   TYPE string.
    DATA lv_text    TYPE string.
    DATA lv_value   TYPE string.
    DATA lv_xml     TYPE string.
    DATA lv_ref     TYPE string.
    DATA lv_row     TYPE i.
    DATA lv_col0    TYPE i.
    DATA lv_row0    TYPE i.
    DATA lv_number  TYPE decfloat34.
    DATA lv_elementary TYPE abap_bool.
    DATA lv_totals  TYPE abap_bool.
    DATA lv_table_name TYPE string.
    DATA ls_area    TYPE ty_s_area.
    FIELD-SYMBOLS <line> TYPE any.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <cell> TYPE any.
    FIELD-SYMBOLS <link> TYPE any.
    FIELD-SYMBOLS <col>  TYPE ty_s_out_col.

    " where the header row starts: A1, or the cell the layout names
    IF layout-start_cell IS NOT INITIAL.
      DATA(lv_start_row) = digits_only( layout-start_cell ).
      lv_col0 = column_to_number( layout-start_cell ) - 1.
      IF lv_col0 < 0 OR lv_start_row IS INITIAL OR strlen( lv_start_row ) > 7.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_START_CELL - { layout-start_cell } is no cell of a sheet|.
      ENDIF.
      lv_row0 = lv_start_row.
      lv_row0 = lv_row0 - 1.
      IF lv_row0 < 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_START_CELL - { layout-start_cell } is no cell of a sheet|.
      ENDIF.
    ENDIF.
    " the header and the totals row are two more
    IF lines( val ) + lv_row0 + 2 > cv_max_row.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_TOO_MANY_ROWS - a sheet holds { cv_max_row } rows, the table has { lines( val ) }|.
    ENDIF.
    bounds_check( val  = layout-row_height
                  low  = 0
                  high = 409
                  name = `row height` ).
    bounds_check( val  = layout-outline_level
                  low  = 0
                  high = 7
                  name = `outline level` ).

    CREATE DATA lr_line LIKE LINE OF val.
    ASSIGN lr_line->* TO <line>.
    fields_get( EXPORTING row        = <line>
                IMPORTING elementary = lv_elementary
                          result     = lt_field ).
    DATA(lt_col) = columns_get( t_field = lt_field
                                layout  = layout
                                offset  = lv_col0 ).
    IF lt_col IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_table) = xsdbool( layout-table_style IS NOT INITIAL AND layout-no_header = abap_false
                              AND val IS NOT INITIAL ).
    IF lv_table = abap_true.
      lv_table_name = table_name_get( layout-table_name ).
    ENDIF.
    LOOP AT lt_col ASSIGNING <col>.
      IF <col>-total_fn IS NOT INITIAL OR <col>-total_label IS NOT INITIAL.
        " a totals row under no rows adds up nothing
        lv_totals = xsdbool( val IS NOT INITIAL ).
      ENDIF.
    ENDLOOP.

    " the cells the table takes must be free - the totals row included
    ls_area-row1 = lv_row0 + 1.
    ls_area-col1 = lv_col0 + 1.
    ls_area-row2 = lv_row0 + lines( val ).
    IF layout-no_header = abap_false.
      ls_area-row2 = ls_area-row2 + 1.
    ENDIF.
    IF lv_totals = abap_true.
      ls_area-row2 = ls_area-row2 + 1.
    ENDIF.
    ls_area-col2 = lv_col0 + lines( lt_col ).
    IF ls_area-row2 >= ls_area-row1.
      seg_check( ls_area ).
    ENDIF.

    " an Excel table formats its header and its totals row itself
    DATA(ls_head) = layout-header_style.
    IF lv_table = abap_false.
      ls_head-bold = abap_true.
    ENDIF.
    DATA(lv_head_style) = style_attr( style = ls_head ).

    " the height and the outline level of the rows
    DATA(lv_attr) = line_attr( VALUE #( size  = layout-row_height
                                        level = layout-outline_level ) ).
    IF layout-outline_level > ms_cur-row_level.
      ms_cur-row_level = layout-outline_level.
    ENDIF.

    lv_row = lv_row0.
    IF layout-no_header = abap_false.
      lv_row = lv_row + 1.
      LOOP AT lt_col ASSIGNING <col>.
        lv_cells = lv_cells && cell_text( address = |{ <col>-letters }{ lv_row }|
                                          val     = <col>-header
                                          style   = lv_head_style ).
        " the filter button takes its room in the header cell
        <col>-len = strlen( <col>-header ) + 3.
      ENDLOOP.
      seg_add( row    = lv_row
               col    = ls_area-col1
               col_to = ls_area-col2
               xml    = lv_cells ).
    ENDIF.
    DATA(lv_first_data) = lv_row + 1.

    LOOP AT val ASSIGNING <row>.
      lv_row = lv_row + 1.
      CLEAR lv_cells.
      LOOP AT lt_col ASSIGNING <col>.
        lv_ref = |{ <col>-letters }{ lv_row }|.

        IF <col>-formula IS NOT INITIAL.
          " no value with it: Excel works the formula out when it opens
          " the file (get_xlsx asks it to)
          lv_value = replace( val  = <col>-formula
                              sub  = `{row}`
                              with = |{ lv_row }|
                              occ  = 0 ).
          lv_value = zabaputil_cl_util_context=>c_escape_xml( lv_value ).
          lv_cells = |{ lv_cells }<c r="{ lv_ref }"{ <col>-style }><f>{ lv_value }</f></c>|.
          mv_formulas = abap_true.
          CONTINUE.
        ENDIF.

        IF lv_elementary = abap_true.
          ASSIGN <row> TO <cell>.
        ELSE.
          ASSIGN COMPONENT <col>-name OF STRUCTURE <row> TO <cell>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        " a link from the cell, to what the row holds in the link component
        DATA(lv_style) = <col>-style.
        IF <col>-link IS NOT INITIAL.
          UNASSIGN <link>.
          IF lv_elementary = abap_true.
            ASSIGN <row> TO <link>.
          ELSE.
            ASSIGN COMPONENT <col>-link OF STRUCTURE <row> TO <link>.
          ENDIF.
          IF <link> IS ASSIGNED AND <link> IS NOT INITIAL.
            INSERT VALUE #( ref    = lv_ref
                            target = |{ <link> }| ) INTO TABLE ms_cur-t_link.
            lv_style = <col>-link_style.
          ENDIF.
        ENDIF.

        cell_xml( EXPORTING address   = lv_ref
                            cat       = <col>-cat
                            style     = lv_style
                            val       = <cell>
                            time_zone = layout-time_zone
                  IMPORTING xml       = lv_xml
                            text      = lv_text ).
        lv_cells = lv_cells && lv_xml.

        IF <col>-cat = cs_cat-number AND <col>-total_fn IS NOT INITIAL.
          lv_number = <cell>.
          <col>-sum = <col>-sum + lv_number.
          IF <col>-numbers = 0 OR lv_number < <col>-low.
            <col>-low = lv_number.
          ENDIF.
          IF <col>-numbers = 0 OR lv_number > <col>-high.
            <col>-high = lv_number.
          ENDIF.
          <col>-numbers = <col>-numbers + 1.
        ENDIF.
        IF lv_text IS NOT INITIAL.
          <col>-cells = <col>-cells + 1.
        ENDIF.
        IF strlen( lv_text ) > <col>-len.
          <col>-len = strlen( lv_text ).
        ENDIF.
      ENDLOOP.
      IF lv_cells IS NOT INITIAL OR lv_attr IS NOT INITIAL.
        seg_add( row    = lv_row
                 col    = ls_area-col1
                 col_to = ls_area-col2
                 attr   = lv_attr
                 xml    = lv_cells ).
      ENDIF.
    ENDLOOP.
    DATA(lv_last_data) = lv_row.

    IF lv_totals = abap_true.
      " a formula over the rows: SUBTOTAL( ) leaves out what a filter
      " hides. In an Excel table it names the column, else its cells
      lv_row = lv_row + 1.
      CLEAR lv_cells.
      LOOP AT lt_col ASSIGNING <col>.
        lv_ref = |{ <col>-letters }{ lv_row }|.
        IF <col>-total_fn IS NOT INITIAL.
          IF lv_table = abap_true.
            " in the name of a column, what a structured reference reads
            " as its own syntax is marked with an apostrophe
            lv_text = <col>-header.
            lv_text = replace( val  = lv_text
                               sub  = `'`
                               with = `''`
                               occ  = 0 ).
            lv_text = replace( val  = lv_text
                               sub  = `[`
                               with = `'[`
                               occ  = 0 ).
            lv_text = replace( val  = lv_text
                               sub  = `]`
                               with = `']`
                               occ  = 0 ).
            lv_text = replace( val  = lv_text
                               sub  = `#`
                               with = `'#`
                               occ  = 0 ).
            lv_value = |SUBTOTAL({ <col>-total_code },{ lv_table_name }[{ lv_text }])|.
          ELSE.
            lv_value = |SUBTOTAL({ <col>-total_code },{ <col>-letters }{ lv_first_data }:{ <col>-letters }{ lv_last_data })|.
          ENDIF.
          lv_value = zabaputil_cl_util_context=>c_escape_xml( lv_value ).
          " a count is a number whatever the column holds
          DATA(lv_total_style) = <col>-style.
          IF <col>-total_fn = `count` OR <col>-total_fn = `countNums`.
            lv_total_style = <col>-plain_style.
          ENDIF.
          lv_text = total_value( <col> ).
          IF lv_text IS NOT INITIAL.
            lv_text = |<v>{ lv_text }</v>|.
          ENDIF.
          lv_cells = |{ lv_cells }<c r="{ lv_ref }"{ lv_total_style }><f>{ lv_value }</f>{ lv_text }</c>|.
          mv_formulas = abap_true.
        ELSEIF <col>-total_label IS NOT INITIAL.
          lv_cells = lv_cells && cell_text( address = lv_ref
                                            val     = <col>-total_label
                                            style   = lv_head_style ).
        ENDIF.
      ENDLOOP.
      seg_add( row    = lv_row
               col    = ls_area-col1
               col_to = ls_area-col2
               xml    = lv_cells ).
    ENDIF.

    " the range of the table, and of its filter, which ends above the
    " totals row
    ls_area-row2 = lv_row.
    IF ls_area-row2 < ls_area-row1.
      ls_area-row2 = ls_area-row1.
    ENDIF.
    DATA(lv_range) = area_text( ls_area ).
    DATA(ls_filter) = ls_area.
    ls_filter-row2 = nmax( val1 = lv_last_data
                           val2 = lv_row0 + 1 ).
    DATA(lv_filter) = area_text( ls_filter ).

    " the widths, and what the layout says of the columns
    LOOP AT lt_col ASSIGNING <col>.
      DATA(lv_index) = lv_col0 + sy-tabix.
      col_len( col  = lv_index
               len  = <col>-len
               size = <col>-width ).
      IF <col>-hidden = abap_true OR <col>-level > 0.
        set_column( col           = lv_index
                    hidden        = <col>-hidden
                    outline_level = <col>-level ).
      ENDIF.
      IF <col>-validation IS NOT INITIAL AND lv_last_data >= lv_first_data.
        lv_value = |{ <col>-letters }{ lv_first_data }:{ <col>-letters }{ lv_last_data }|.
        INSERT validation_xml( sqref = lv_value
                               val   = <col>-validation ) INTO TABLE ms_cur-t_valid.
      ENDIF.
    ENDLOOP.

    " the first table with a header places the frozen pane and the rows
    " printed on every page
    IF layout-no_header = abap_false.
      IF layout-freeze_header = abap_true AND ms_cur-pane IS INITIAL.
        ms_cur-pane = |<pane ySplit="{ lv_row0 + 1 }" topLeftCell="A{ lv_row0 + 2 }" activePane="bottomLeft" state="frozen"/>|.
      ENDIF.
      IF ms_cur-header_rows IS INITIAL.
        ms_cur-header_rows = |${ lv_row0 + 1 }:${ lv_row0 + 1 }|.
      ENDIF.
    ENDIF.

    IF lv_table = abap_true.
      mv_tables = mv_tables + 1.
      IF layout-no_filter = abap_true.
        CLEAR lv_filter.
      ENDIF.
      lv_xml = table_xml( id        = mv_tables
                          name      = lv_table_name
                          range     = lv_range
                          filter    = lv_filter
                          layout    = layout
                          totals    = lv_totals
                          t_col     = lt_col
                          first_row = lv_first_data ).
      INSERT VALUE #( id  = mv_tables
                      xml = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_xml ) )
             INTO TABLE ms_cur-t_table.
    ELSEIF layout-auto_filter = abap_true AND layout-no_header = abap_false.
      IF ms_cur-filter IS NOT INITIAL.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_FILTER - the sheet has a filter on { ms_cur-filter } already, it takes one|.
      ENDIF.
      ms_cur-filter = lv_filter.
    ENDIF.

  ENDMETHOD.

  METHOD cell_xml.

    DATA lv_date    TYPE d.
    DATA lv_time    TYPE t.
    DATA lv_serial  TYPE i.
    DATA lv_seconds TYPE i.
    DATA lv_number  TYPE decfloat34.
    DATA lv_value   TYPE string.
    DATA lv_stampl  TYPE timestampl.

    CLEAR xml.
    CLEAR text.

    CASE cat.

      WHEN cs_cat-number.
        " a string template writes the sign in front and a period as
        " decimal separator, whatever the user's settings
        text = |{ val }|.
        xml = |<c r="{ address }"{ style }><v>{ text }</v></c>|.

      WHEN cs_cat-date.
        lv_date = val.
        lv_serial = date_to_serial( lv_date ).
        IF lv_serial > 0.
          text = `0000-00-00`.
          xml = |<c r="{ address }"{ style }><v>{ lv_serial }</v></c>|.
        ELSEIF lv_date IS NOT INITIAL.
          " a date Excel has no number for - before 1900, or none at
          " all - stays readable as text
          text = |{ lv_date(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|.
          xml = cell_text( address = address
                           val     = text ).
        ENDIF.

      WHEN cs_cat-time.
        lv_time = val.
        lv_seconds = lv_time(2) * 3600 + lv_time+2(2) * 60 + lv_time+4(2).
        " the fraction of a day with every digit the cell stores: cut
        " shorter, 23:59:59 comes out a hair below itself, and a reader
        " that cuts off the rest shows 23:59:58
        lv_number = lv_seconds.
        lv_number = round( val = lv_number / 86400
                           dec = 18 ).
        lv_value = number_plain( val    = |{ lv_number }|
                                 digits = 17 ).
        text = `00:00:00`.
        xml = |<c r="{ address }"{ style }><v>{ lv_value }</v></c>|.

      WHEN cs_cat-timestamp.
        " YYYYMMDDHHMMSS, with or without a fraction of a second - in the
        " time zone the layout names
        text = |{ val }|.
        IF time_zone IS NOT INITIAL AND val IS NOT INITIAL.
          lv_stampl = val.
          zabaputil_cl_util_context=>time_stampl_to_tz( EXPORTING val  = lv_stampl
                                                                  tz   = time_zone
                                                        IMPORTING date = lv_date
                                                                  time = lv_time ).
          text = |{ lv_date }{ lv_time }|.
        ENDIF.
        IF text CS `.`.
          text = substring_before( val = text
                                   sub = `.` ).
        ENDIF.
        IF strlen( text ) = 14 AND text CO cv_digits.
          lv_date = text(8).
          lv_serial = date_to_serial( lv_date ).
        ELSE.
          lv_serial = 0.
        ENDIF.
        IF lv_serial > 0.
          lv_seconds = text+8(2) * 3600 + text+10(2) * 60 + text+12(2).
          lv_number = lv_seconds.
          lv_number = lv_serial + round( val = lv_number / 86400
                                         dec = 18 ).
          lv_value = number_plain( val    = |{ lv_number }|
                                   digits = 17 ).
          text = `0000-00-00 00:00`.
          xml = |<c r="{ address }"{ style }><v>{ lv_value }</v></c>|.
        ELSE.
          CLEAR text.
        ENDIF.

      WHEN cs_cat-boolean.
        " a flag that is not set is an empty cell, not a FALSE in every
        " row
        IF val IS NOT INITIAL.
          text = `TRUE`.
          xml = |<c r="{ address }"{ style } t="b"><v>1</v></c>|.
        ENDIF.

      WHEN OTHERS.
        " text, and what is text in the file: a NUMC with its leading
        " zeros, bytes as hex digits
        IF val IS NOT INITIAL OR cat = cs_cat-numc.
          text = |{ val }|.
          xml = cell_text( address = address
                           val     = text
                           style   = style ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD seg_add.

    INSERT VALUE #( row    = row
                    col    = col
                    col_to = col_to
                    attr   = attr
                    xml    = xml ) INTO TABLE ms_cur-t_seg.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_CELL_TAKEN - { column_to_letters( col ) }{ row } holds a value already|.
    ENDIF.
    IF ms_cur-col_min = 0 OR col < ms_cur-col_min.
      ms_cur-col_min = col.
    ENDIF.
    IF col_to > ms_cur-col_max.
      ms_cur-col_max = col_to.
    ENDIF.

  ENDMETHOD.

  METHOD seg_check.

    DATA lv_taken TYPE abap_bool.

    IF ms_cur-t_seg IS INITIAL.
      RETURN.
    ENDIF.

    " one row by the key; the rows of a table in one pass
    IF area-row1 = area-row2.
      LOOP AT ms_cur-t_seg REFERENCE INTO DATA(lr_seg) WHERE row = area-row1.
        IF lr_seg->col <= area-col2 AND lr_seg->col_to >= area-col1.
          lv_taken = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT ms_cur-t_seg REFERENCE INTO lr_seg
           WHERE row >= area-row1 AND row <= area-row2
             AND col <= area-col2 AND col_to >= area-col1.
        lv_taken = abap_true.
        EXIT.
      ENDLOOP.
    ENDIF.
    IF lv_taken = abap_true.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_CELL_TAKEN - { area_text( area ) } overlaps cells that hold a value already|.
    ENDIF.

  ENDMETHOD.

  METHOD col_len.

    FIELD-SYMBOLS <line> TYPE ty_s_line.

    UNASSIGN <line>.
    ASSIGN ms_cur-t_col[ pos = col ] TO <line>.
    IF <line> IS NOT ASSIGNED.
      INSERT VALUE #( pos = col ) INTO TABLE ms_cur-t_col ASSIGNING <line>.
    ENDIF.
    <line>-auto = abap_true.
    IF len > <line>-len.
      <line>-len = len.
    ENDIF.
    IF size > 0 AND <line>-size = 0.
      <line>-size = size.
    ENDIF.

  ENDMETHOD.

  METHOD line_attr.

    IF val-size > 0.
      result = | ht="{ val-size }" customHeight="1"|.
    ENDIF.
    IF val-hidden = abap_true.
      result = |{ result } hidden="1"|.
    ENDIF.
    IF val-level > 0.
      result = |{ result } outlineLevel="{ val-level }"|.
    ENDIF.

  ENDMETHOD.

  METHOD sheet_close.

    DATA lt_head  TYPE string_table.
    DATA lt_xml   TYPE string_table.
    DATA lt_rels  TYPE string_table.
    DATA lt_link  TYPE string_table.
    DATA lt_cols  TYPE string_table.
    DATA lv_cells TYPE string.
    DATA lv_row   TYPE i.
    DATA lv_rid   TYPE i.
    DATA ls_sheet TYPE ty_s_out_sheet.
    DATA ls_area  TYPE ty_s_area.

    IF mv_open = abap_false.
      RETURN.
    ENDIF.
    mv_open = abap_false.
    DATA(ls_layout) = ms_cur-layout.
    DATA(lv_index) = lines( mt_out_sheet ) + 1.

    " a row with a height or an outline level and no cell is a row too
    LOOP AT ms_cur-t_row INTO DATA(ls_line).
      DATA(lv_found) = abap_false.
      LOOP AT ms_cur-t_seg TRANSPORTING NO FIELDS WHERE row = ls_line-pos.
        lv_found = abap_true.
        EXIT.
      ENDLOOP.
      IF lv_found = abap_false.
        INSERT VALUE #( row = ls_line-pos ) INTO TABLE ms_cur-t_seg.
      ENDIF.
    ENDLOOP.

    " the rows, each with its cells in the order of their columns
    LOOP AT ms_cur-t_seg REFERENCE INTO DATA(lr_seg).
      IF lr_seg->row <> lv_row.
        IF lv_row > 0.
          INSERT |{ lv_cells }</row>| INTO TABLE lt_xml.
        ENDIF.
        lv_row = lr_seg->row.
        DATA(lv_attr) = lr_seg->attr.
        IF ms_cur-t_row IS NOT INITIAL AND line_exists( ms_cur-t_row[ pos = lv_row ] ).
          lv_attr = line_attr( ms_cur-t_row[ pos = lv_row ] ).
        ENDIF.
        lv_cells = |<row r="{ lv_row }"{ lv_attr }>|.
      ENDIF.
      lv_cells = lv_cells && lr_seg->xml.
    ENDLOOP.
    IF lv_row > 0.
      INSERT |{ lv_cells }</row>| INTO TABLE lt_xml.
    ENDIF.
    CLEAR lv_cells.

    " the range of the sheet
    DATA(lv_range) = `A1`.
    IF ms_cur-col_max > 0.
      ls_area-row1 = ms_cur-t_seg[ 1 ]-row.
      ls_area-row2 = ms_cur-t_seg[ lines( ms_cur-t_seg ) ]-row.
      ls_area-col1 = ms_cur-col_min.
      ls_area-col2 = ms_cur-col_max.
      lv_range = area_text( ls_area ).
    ENDIF.

    " the first sheet that is not hidden is the one Excel shows
    DATA(lv_view) = ``.
    IF ls_layout-hidden = abap_false.
      lv_view = ` tabSelected="1"`.
      LOOP AT mt_out_sheet TRANSPORTING NO FIELDS WHERE hidden = abap_false.
        CLEAR lv_view.
        EXIT.
      ENDLOOP.
    ENDIF.
    IF ls_layout-no_gridlines = abap_true.
      lv_view = |{ lv_view } showGridLines="0"|.
    ENDIF.
    IF ls_layout-zoom > 0.
      lv_view = |{ lv_view } zoomScale="{ ls_layout-zoom }" zoomScaleNormal="{ ls_layout-zoom }"|.
    ENDIF.

    DATA(lv_pr) = ``.
    IF ls_layout-tab_color IS NOT INITIAL.
      lv_pr = |<tabColor rgb="{ color_get( ls_layout-tab_color ) }"/>|.
    ENDIF.
    IF ls_layout-fit_width > 0 OR ls_layout-fit_height > 0.
      lv_pr = |{ lv_pr }<pageSetUpPr fitToPage="1"/>|.
    ENDIF.

    INSERT |{ cv_xml_head }<worksheet xmlns="{ cs_ns-main }" xmlns:r="{ cs_ns-rel }">| INTO TABLE lt_head.
    IF lv_pr IS NOT INITIAL.
      INSERT |<sheetPr>{ lv_pr }</sheetPr>| INTO TABLE lt_head.
    ENDIF.
    INSERT |<dimension ref="{ lv_range }"/>| INTO TABLE lt_head.
    INSERT |<sheetViews><sheetView workbookViewId="0"{ lv_view }>{ ms_cur-pane }</sheetView></sheetViews>|
           INTO TABLE lt_head.
    DATA(lv_format) = ``.
    IF ms_cur-row_level > 0.
      lv_format = | outlineLevelRow="{ ms_cur-row_level }"|.
    ENDIF.
    IF ms_cur-col_level > 0.
      lv_format = |{ lv_format } outlineLevelCol="{ ms_cur-col_level }"|.
    ENDIF.
    INSERT |<sheetFormatPr defaultRowHeight="15"{ lv_format }/>| INTO TABLE lt_head.

    " the widths: by the longest content, in bounds - a long text does not
    " push every other column off the screen
    LOOP AT ms_cur-t_col INTO ls_line.
      DATA(lv_width) = ls_line-size.
      IF lv_width <= 0 AND ls_line-auto = abap_true.
        lv_width = nmin( val1 = nmax( val1 = ls_line-len + 2
                                      val2 = 6 )
                         val2 = 60 ).
      ELSEIF lv_width <= 0.
        lv_width = 9.
      ENDIF.
      lv_attr = line_attr( VALUE #( hidden = ls_line-hidden
                                    level  = ls_line-level ) ).
      lv_cells = |<col min="{ ls_line-pos }" max="{ ls_line-pos }" width="{ lv_width }"|.
      lv_cells = |{ lv_cells } customWidth="1"{ lv_attr }/>|.
      INSERT lv_cells INTO TABLE lt_cols.
    ENDLOOP.
    CLEAR lv_cells.
    IF lt_cols IS NOT INITIAL.
      INSERT `<cols>` INTO TABLE lt_head.
      INSERT LINES OF lt_cols INTO TABLE lt_head.
      INSERT `</cols>` INTO TABLE lt_head.
    ENDIF.
    INSERT `<sheetData>` INTO TABLE lt_head.

    INSERT `</sheetData>` INTO TABLE lt_xml.
    IF ms_cur-filter IS NOT INITIAL.
      INSERT |<autoFilter ref="{ ms_cur-filter }"/>| INTO TABLE lt_xml.
    ENDIF.
    IF ms_cur-t_merge IS NOT INITIAL.
      INSERT |<mergeCells count="{ lines( ms_cur-t_merge ) }">| INTO TABLE lt_xml.
      LOOP AT ms_cur-t_merge INTO ls_area.
        INSERT |<mergeCell ref="{ area_text( ls_area ) }"/>| INTO TABLE lt_xml.
      ENDLOOP.
      INSERT `</mergeCells>` INTO TABLE lt_xml.
    ENDIF.
    IF ms_cur-t_valid IS NOT INITIAL.
      INSERT |<dataValidations count="{ lines( ms_cur-t_valid ) }">| INTO TABLE lt_xml.
      INSERT LINES OF ms_cur-t_valid INTO TABLE lt_xml.
      INSERT `</dataValidations>` INTO TABLE lt_xml.
    ENDIF.

    " the relationships: the tables first, then the targets of the links -
    " each target once
    INSERT |{ cv_xml_head }<Relationships xmlns="{ cs_ns-pkg_rel }">| INTO TABLE lt_rels.
    LOOP AT ms_cur-t_table REFERENCE INTO DATA(lr_table).
      lv_rid = lv_rid + 1.
      INSERT |<Relationship Id="rId{ lv_rid }" Type="{ cs_ns-rel }/table"| INTO TABLE lt_rels.
      INSERT | Target="../tables/table{ lr_table->id }.xml"/>| INTO TABLE lt_rels.
    ENDLOOP.
    IF ms_cur-t_link IS NOT INITIAL.
      INSERT `<hyperlinks>` INTO TABLE lt_xml.
      LOOP AT ms_cur-t_link REFERENCE INTO DATA(lr_link).
        IF lr_link->target(1) = `#`.
          DATA(lv_target) = zabaputil_cl_util_context=>c_escape_xml(
                                substring( val = lr_link->target
                                           off = 1 ) ).
          INSERT |<hyperlink ref="{ lr_link->ref }" location="{ lv_target }"/>| INTO TABLE lt_xml.
          CONTINUE.
        ENDIF.
        DATA(lv_link) = 0.
        LOOP AT lt_link INTO lv_target.
          IF lv_target = lr_link->target.
            lv_link = sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF lv_link = 0.
          INSERT lr_link->target INTO TABLE lt_link.
          lv_link = lines( lt_link ).
          lv_target = zabaputil_cl_util_context=>c_escape_xml( lr_link->target ).
          INSERT |<Relationship Id="rId{ lv_rid + lv_link }" Type="{ cs_ns-rel }/hyperlink"| INTO TABLE lt_rels.
          INSERT | Target="{ lv_target }" TargetMode="External"/>| INTO TABLE lt_rels.
        ENDIF.
        INSERT |<hyperlink ref="{ lr_link->ref }" r:id="rId{ lv_rid + lv_link }"/>| INTO TABLE lt_xml.
      ENDLOOP.
      INSERT `</hyperlinks>` INTO TABLE lt_xml.
    ENDIF.
    INSERT `</Relationships>` INTO TABLE lt_rels.

    " printing
    IF ls_layout-margin > 0.
      " inches, as the file counts
      DATA(lv_inch) = CONV decfloat34( ls_layout-margin ).
      lv_inch = round( val = lv_inch * 10 / 254
                       dec = 4 ).
      DATA(lv_margin) = |{ lv_inch }|.
      INSERT |<pageMargins left="{ lv_margin }" right="{ lv_margin }" top="{ lv_margin }"| INTO TABLE lt_xml.
      INSERT | bottom="{ lv_margin }" header="0.3" footer="0.3"/>| INTO TABLE lt_xml.
    ENDIF.
    DATA(lv_setup) = ``.
    IF ls_layout-paper_size > 0.
      lv_setup = | paperSize="{ ls_layout-paper_size }"|.
    ENDIF.
    IF ls_layout-landscape = abap_true.
      lv_setup = |{ lv_setup } orientation="landscape"|.
    ENDIF.
    IF ls_layout-fit_width > 0 OR ls_layout-fit_height > 0.
      lv_setup = |{ lv_setup } fitToWidth="{ ls_layout-fit_width }" fitToHeight="{ ls_layout-fit_height }"|.
    ENDIF.
    IF lv_setup IS NOT INITIAL.
      INSERT |<pageSetup{ lv_setup }/>| INTO TABLE lt_xml.
    ENDIF.
    IF ls_layout-header_text IS NOT INITIAL OR ls_layout-footer_text IS NOT INITIAL.
      INSERT `<headerFooter>` INTO TABLE lt_xml.
      IF ls_layout-header_text IS NOT INITIAL.
        DATA(lv_text) = ls_layout-header_text.
        IF lv_text NS `&L` AND lv_text NS `&C` AND lv_text NS `&R`.
          lv_text = `&C` && lv_text.
        ENDIF.
        lv_text = zabaputil_cl_util_context=>c_escape_xml( lv_text ).
        INSERT |<oddHeader>{ lv_text }</oddHeader>| INTO TABLE lt_xml.
      ENDIF.
      IF ls_layout-footer_text IS NOT INITIAL.
        lv_text = ls_layout-footer_text.
        IF lv_text NS `&L` AND lv_text NS `&C` AND lv_text NS `&R`.
          lv_text = `&C` && lv_text.
        ENDIF.
        lv_text = zabaputil_cl_util_context=>c_escape_xml( lv_text ).
        INSERT |<oddFooter>{ lv_text }</oddFooter>| INTO TABLE lt_xml.
      ENDIF.
      INSERT `</headerFooter>` INTO TABLE lt_xml.
    ENDIF.
    IF ms_cur-t_table IS NOT INITIAL.
      INSERT |<tableParts count="{ lines( ms_cur-t_table ) }">| INTO TABLE lt_xml.
      DO lines( ms_cur-t_table ) TIMES.
        INSERT |<tablePart r:id="rId{ sy-index }"/>| INTO TABLE lt_xml.
      ENDDO.
      INSERT `</tableParts>` INTO TABLE lt_xml.
    ENDIF.
    INSERT `</worksheet>` INTO TABLE lt_xml.

    " the names of the sheet: what is printed, and the rows on every page
    DATA(lv_sheet) = sheet_quote( ms_cur-name ).
    IF ls_layout-print_area IS NOT INITIAL.
      INSERT VALUE #( name  = `_xlnm.Print_Area`
                      sheet = lv_index
                      ref   = |{ lv_sheet }!{ area_text( val      = area_parse( ls_layout-print_area )
                                                         absolute = abap_true ) }| ) INTO TABLE mt_name.
    ENDIF.
    IF ls_layout-repeat_header = abap_true AND ms_cur-header_rows IS NOT INITIAL.
      INSERT VALUE #( name  = `_xlnm.Print_Titles`
                      sheet = lv_index
                      ref   = |{ lv_sheet }!{ ms_cur-header_rows }| ) INTO TABLE mt_name.
    ENDIF.

    ls_sheet-name = ms_cur-name.
    ls_sheet-hidden = ls_layout-hidden.
    ls_sheet-t_table = ms_cur-t_table.
    IF lv_rid > 0 OR lt_link IS NOT INITIAL.
      ls_sheet-rels = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_rels ) ).
    ENDIF.
    CLEAR lt_rels.
    CLEAR ms_cur.
    ls_sheet-xml = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                       concat_lines_of( lt_head ) && concat_lines_of( lt_xml ) ).
    INSERT ls_sheet INTO TABLE mt_out_sheet.

  ENDMETHOD.

  METHOD area_parse.

    DATA lt_part TYPE string_table.
    DATA lv_row  TYPE i.
    DATA lv_col  TYPE i.

    DATA(lv_text) = to_upper( zabaputil_cl_util_context=>c_trim( val ) ).
    lv_text = replace( val  = lv_text
                       sub  = `$`
                       with = ``
                       occ  = 0 ).
    SPLIT lv_text AT `:` INTO TABLE lt_part.
    IF lines( lt_part ) = 1.
      INSERT lt_part[ 1 ] INTO TABLE lt_part.
    ENDIF.
    IF lines( lt_part ) <> 2.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_RANGE - { val } is no range of cells (A1:D5)|.
    ENDIF.

    LOOP AT lt_part INTO DATA(lv_part).
      DATA(lv_digits) = digits_only( lv_part ).
      lv_col = column_to_number( lv_part ).
      DATA(lv_letters) = column_to_letters( lv_col ).
      DATA(lv_cell) = |{ lv_letters }{ lv_digits }|.
      IF lv_digits IS INITIAL OR strlen( lv_digits ) > 7 OR lv_cell <> lv_part.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_RANGE - { val } is no range of cells (A1:D5)|.
      ENDIF.
      lv_row = lv_digits.
      IF lv_row < 1 OR lv_row > cv_max_row OR lv_col > cv_max_col.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_RANGE - { val } lies outside of a sheet|.
      ENDIF.
      IF sy-tabix = 1.
        result-row1 = lv_row.
        result-col1 = lv_col.
      ELSE.
        result-row2 = nmax( val1 = lv_row
                            val2 = result-row1 ).
        result-col2 = nmax( val1 = lv_col
                            val2 = result-col1 ).
        result-row1 = nmin( val1 = lv_row
                            val2 = result-row1 ).
        result-col1 = nmin( val1 = lv_col
                            val2 = result-col1 ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD area_text.

    DATA(lv_mark) = ``.
    IF absolute = abap_true.
      lv_mark = `$`.
    ENDIF.
    result = |{ lv_mark }{ column_to_letters( val-col1 ) }{ lv_mark }{ val-row1 }|.
    IF val-row2 <> val-row1 OR val-col2 <> val-col1.
      result = |{ result }:{ lv_mark }{ column_to_letters( val-col2 ) }{ lv_mark }{ val-row2 }|.
    ENDIF.

  ENDMETHOD.

  METHOD sheet_quote.

    result = replace( val  = val
                      sub  = `'`
                      with = `''`
                      occ  = 0 ).
    result = |'{ result }'|.

  ENDMETHOD.

  METHOD cell_check.

    IF row < 1 OR row > cv_max_row OR col < 1 OR col > cv_max_col.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_CELL - row { row }, column { col } is no cell of a sheet|.
    ENDIF.

  ENDMETHOD.

  METHOD bounds_check.

    IF val < low OR val > high.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_LAYOUT - { name } { val } is out of bounds, Excel takes { low } to { high }|.
    ENDIF.

  ENDMETHOD.

  METHOD validation_xml.

    DATA lv_list TYPE string.

    CASE val-type.
      WHEN `list` OR `whole` OR `decimal` OR `date` OR `time` OR `textLength` OR `custom`.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_VALIDATION - { val-type } is no type of a validation|.
    ENDCASE.

    result = |<dataValidation type="{ val-type }"|.
    IF val-operator IS NOT INITIAL AND val-type <> `list` AND val-type <> `custom`.
      CASE val-operator.
        WHEN `between` OR `notBetween` OR `equal` OR `notEqual` OR `greaterThan` OR `lessThan`.
        WHEN `greaterThanOrEqual` OR `lessThanOrEqual`.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_VALIDATION - { val-operator } is no operator of a validation|.
      ENDCASE.
      result = |{ result } operator="{ val-operator }"|.
    ENDIF.
    CASE val-error_style.
      WHEN ``.
      WHEN `stop` OR `warning` OR `information`.
        result = |{ result } errorStyle="{ val-error_style }"|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_VALIDATION - { val-error_style } is no error style of a validation|.
    ENDCASE.
    IF val-no_blank = abap_false.
      result = |{ result } allowBlank="1"|.
    ENDIF.
    " the attribute hides the button - its name says the opposite
    IF val-no_dropdown = abap_true.
      result = |{ result } showDropDown="1"|.
    ENDIF.
    IF val-prompt IS NOT INITIAL OR val-prompt_title IS NOT INITIAL.
      result = |{ result } showInputMessage="1"|.
    ENDIF.
    result = |{ result } showErrorMessage="1"|.
    " Excel takes a title of 32 characters and a message of 255
    IF val-prompt_title IS NOT INITIAL.
      DATA(lv_text) = val-prompt_title.
      IF strlen( lv_text ) > 32.
        lv_text = lv_text(32).
      ENDIF.
      result = |{ result } promptTitle="{ zabaputil_cl_util_context=>c_escape_xml( lv_text ) }"|.
    ENDIF.
    IF val-prompt IS NOT INITIAL.
      lv_text = val-prompt.
      IF strlen( lv_text ) > 255.
        lv_text = lv_text(255).
      ENDIF.
      result = |{ result } prompt="{ zabaputil_cl_util_context=>c_escape_xml( lv_text ) }"|.
    ENDIF.
    IF val-error_title IS NOT INITIAL.
      lv_text = val-error_title.
      IF strlen( lv_text ) > 32.
        lv_text = lv_text(32).
      ENDIF.
      result = |{ result } errorTitle="{ zabaputil_cl_util_context=>c_escape_xml( lv_text ) }"|.
    ENDIF.
    IF val-error IS NOT INITIAL.
      lv_text = val-error.
      IF strlen( lv_text ) > 255.
        lv_text = lv_text(255).
      ENDIF.
      result = |{ result } error="{ zabaputil_cl_util_context=>c_escape_xml( lv_text ) }"|.
    ENDIF.
    result = |{ result } sqref="{ sqref }">|.

    " a list of values: in quotes, separated by commas - a value can hold
    " neither, and the whole list 255 characters
    DATA(lv_formula1) = val-formula1.
    IF val-type = `list` AND val-t_value IS NOT INITIAL.
      LOOP AT val-t_value INTO DATA(lv_value).
        IF lv_value CA `,"`.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_VALIDATION - the value { lv_value } of a list holds a comma or a quote|.
        ENDIF.
        IF lv_list IS INITIAL.
          lv_list = lv_value.
        ELSE.
          lv_list = |{ lv_list },{ lv_value }|.
        ENDIF.
      ENDLOOP.
      IF strlen( lv_list ) > 255.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `XLSX_VALIDATION - a list of more than 255 characters: write the values to cells, name them in formula1`.
      ENDIF.
      lv_formula1 = |"{ lv_list }"|.
    ENDIF.
    IF lv_formula1 IS NOT INITIAL.
      result = |{ result }<formula1>{ zabaputil_cl_util_context=>c_escape_xml( lv_formula1 ) }</formula1>|.
    ENDIF.
    IF val-formula2 IS NOT INITIAL.
      result = |{ result }<formula2>{ zabaputil_cl_util_context=>c_escape_xml( val-formula2 ) }</formula2>|.
    ENDIF.
    result = |{ result }</dataValidation>|.

  ENDMETHOD.

  METHOD name_check.

    DATA(lv_len) = strlen( name ).
    IF lv_len = 0 OR lv_len > 255.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_NAME - { name } is no name Excel takes|.
    ENDIF.
    " a letter, an underscore or a backslash in front, then letters,
    " digits, underscores and periods
    DATA(lv_upper) = to_upper( name ).
    DATA(lv_first) = lv_upper(1).
    DATA(lv_ok) = xsdbool( lv_first CA `ABCDEFGHIJKLMNOPQRSTUVWXYZ_\` ).
    IF lv_ok = abap_true AND lv_upper NA ` !"#$%&'()*+,-/:;<=>?@[]^{|}~`.
      " a name that reads as a cell or as R and C of the R1C1 style is none
      DATA(lv_digits) = digits_only( lv_upper ).
      DATA(lv_col) = column_to_number( lv_upper ).
      DATA(lv_cell) = |{ column_to_letters( lv_col ) }{ lv_digits }|.
      IF lv_cell = lv_upper AND lv_digits IS NOT INITIAL AND lv_col <= cv_max_col.
        lv_ok = abap_false.
      ELSEIF lv_upper = `R` OR lv_upper = `C`.
        lv_ok = abap_false.
      ENDIF.
    ELSE.
      lv_ok = abap_false.
    ENDIF.
    IF lv_ok = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_NAME - { name } is no name Excel takes|.
    ENDIF.

    " once in its scope, whatever its case
    LOOP AT mt_name REFERENCE INTO DATA(lr_name) WHERE sheet = sheet.
      DATA(lv_other) = to_upper( lr_name->name ).
      IF lv_other = lv_upper.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_NAME - the workbook has the name { name } already|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD props_xml.

    DATA lt_xml TYPE string_table.

    DATA(lv_created) = ms_props-created.
    IF lv_created IS INITIAL.
      lv_created = zabaputil_cl_util_context=>time_get_timestamp( ).
    ENDIF.
    DATA(lv_time) = iso_by_stamp( lv_created ).

    INSERT |{ cv_xml_head }<cp:coreProperties| INTO TABLE lt_xml.
    INSERT ` xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"` INTO TABLE lt_xml.
    INSERT ` xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/"` INTO TABLE lt_xml.
    INSERT ` xmlns:dcmitype="http://purl.org/dc/dcmitype/"` INTO TABLE lt_xml.
    INSERT ` xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">` INTO TABLE lt_xml.
    IF ms_props-title IS NOT INITIAL.
      INSERT |<dc:title>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-title ) }</dc:title>| INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-subject IS NOT INITIAL.
      INSERT |<dc:subject>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-subject ) }</dc:subject>|
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-creator IS NOT INITIAL.
      INSERT |<dc:creator>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-creator ) }</dc:creator>|
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-keywords IS NOT INITIAL.
      INSERT |<cp:keywords>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-keywords ) }</cp:keywords>|
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-description IS NOT INITIAL.
      INSERT |<dc:description>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-description ) }</dc:description>|
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-category IS NOT INITIAL.
      INSERT |<cp:category>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-category ) }</cp:category>|
             INTO TABLE lt_xml.
    ENDIF.
    INSERT |<dcterms:created xsi:type="dcterms:W3CDTF">{ lv_time }</dcterms:created>| INTO TABLE lt_xml.
    INSERT |<dcterms:modified xsi:type="dcterms:W3CDTF">{ lv_time }</dcterms:modified>| INTO TABLE lt_xml.
    INSERT `</cp:coreProperties>` INTO TABLE lt_xml.
    core = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_xml ) ).

    CLEAR lt_xml.
    DATA(lv_app) = ms_props-application.
    IF lv_app IS INITIAL.
      lv_app = `abap-util`.
    ENDIF.
    INSERT |{ cv_xml_head }<Properties| INTO TABLE lt_xml.
    INSERT ` xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"` INTO TABLE lt_xml.
    INSERT ` xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">` INTO TABLE lt_xml.
    INSERT |<Application>{ zabaputil_cl_util_context=>c_escape_xml( lv_app ) }</Application>| INTO TABLE lt_xml.
    IF ms_props-company IS NOT INITIAL.
      INSERT |<Company>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-company ) }</Company>| INTO TABLE lt_xml.
    ENDIF.
    INSERT `</Properties>` INTO TABLE lt_xml.
    app = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_xml ) ).

  ENDMETHOD.

  METHOD iso_by_stamp.

    DATA lv_stamp TYPE p LENGTH 8 DECIMALS 0.

    lv_stamp = val.
    DATA(lv_text) = |{ lv_stamp }|.
    IF strlen( lv_text ) <> 14 OR lv_text CN cv_digits.
      lv_text = `19000101000000`.
    ENDIF.
    result = |{ lv_text(4) }-{ lv_text+4(2) }-{ lv_text+6(2) }T{ lv_text+8(2) }:{ lv_text+10(2) }:{ lv_text+12(2) }Z|.

  ENDMETHOD.

  METHOD fields_get.

    DATA ls_field TYPE ty_s_field.

    CLEAR elementary.
    CLEAR result.

    DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( row ).
    IF lv_kind <> zabaputil_cl_util_context=>cv_typedescr_typekind_struct1
        AND lv_kind <> zabaputil_cl_util_context=>cv_typedescr_typekind_struct2.
      elementary = abap_true.
      ls_field-name = `TABLE_LINE`.
      ls_field-cat = category_get( type_kind = lv_kind
                                   name      = zabaputil_cl_util_context=>rtti_get_type_name( row ) ).
      IF ls_field-cat IS NOT INITIAL.
        INSERT ls_field INTO TABLE result.
      ENDIF.
      RETURN.
    ENDIF.

    " the included view: an include gives its fields, not one column
    DATA(lt_comp) = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( row ).
    LOOP AT lt_comp REFERENCE INTO DATA(lr_comp).
      IF lr_comp->type->kind <> zabaputil_cl_util_context=>cv_typedescr_kind_elem.
        CONTINUE.
      ENDIF.
      CLEAR ls_field.
      ls_field-name = lr_comp->name.
      DATA(lv_name) = lr_comp->type->get_relative_name( ).
      ls_field-cat = category_get( type_kind = lr_comp->type->type_kind
                                   name      = lv_name ).
      IF ls_field-cat IS INITIAL.
        CONTINUE.
      ENDIF.
      IF lr_comp->type->is_ddic_type( ) = abap_true.
        ls_field-rollname = lv_name.
      ENDIF.
      INSERT ls_field INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD category_get.

    DATA(lv_name) = to_upper( name ).

    IF type_kind = zabaputil_cl_util_context=>cv_typedescr_typekind_date.
      result = cs_cat-date.
    ELSEIF type_kind = zabaputil_cl_util_context=>cv_typedescr_typekind_time.
      result = cs_cat-time.
    ELSEIF zabaputil_cl_util_context=>rtti_check_typekind_number( type_kind ) = abap_true.
      " a timestamp is a packed number that says what it is by its name only
      IF lv_name = `TIMESTAMP` OR lv_name = `TIMESTAMPL` OR lv_name = `TZNTSTMPS` OR lv_name = `TZNTSTMPL`.
        result = cs_cat-timestamp.
      ELSE.
        result = cs_cat-number.
      ENDIF.
    ELSEIF type_kind = zabaputil_cl_util_context=>cv_typedescr_typekind_char.
      IF zabaputil_cl_util_context=>boolean_check_by_name( lv_name ) = abap_true.
        result = cs_cat-boolean.
      ELSE.
        result = cs_cat-text.
      ENDIF.
    ELSEIF type_kind = zabaputil_cl_util_context=>cv_typedescr_typekind_string.
      result = cs_cat-text.
    ELSEIF type_kind = zabaputil_cl_util_context=>cv_typedescr_typekind_num.
      result = cs_cat-numc.
    ELSEIF type_kind = zabaputil_cl_util_context=>cv_typedescr_typekind_hex
        OR type_kind = zabaputil_cl_util_context=>cv_typedescr_typekind_xstring.
      result = cs_cat-hex.
    ELSEIF type_kind <> zabaputil_cl_util_context=>cv_typedescr_typekind_table
        AND type_kind <> zabaputil_cl_util_context=>cv_typedescr_typekind_dref
        AND type_kind <> zabaputil_cl_util_context=>cv_typedescr_typekind_oref
        AND type_kind <> zabaputil_cl_util_context=>cv_typedescr_typekind_struct1
        AND type_kind <> zabaputil_cl_util_context=>cv_typedescr_typekind_struct2.
      " an elementary type a later release added (utclong) - written the
      " way a string template shows it
      result = cs_cat-text.
    ENDIF.

  ENDMETHOD.

  METHOD columns_get.

    DATA ls_col   TYPE ty_s_out_col.
    DATA lt_taken TYPE string_table.
    DATA ls_field TYPE ty_s_field.

    " the columns the layout names, in its order - or every field
    DATA(lt_column) = layout-t_column.
    IF lt_column IS INITIAL.
      LOOP AT t_field INTO ls_field.
        INSERT VALUE #( name = ls_field-name ) INTO TABLE lt_column.
      ENDLOOP.
    ENDIF.
    IF lines( lt_column ) + offset > cv_max_col.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_TOO_MANY_COLUMNS - a sheet holds { cv_max_col } columns|.
    ENDIF.

    LOOP AT lt_column REFERENCE INTO DATA(lr_column).
      DATA(lv_position) = lines( result ) + 1.
      DATA(lv_name) = zabaputil_cl_util_context=>c_trim_upper( lr_column->name ).

      CLEAR ls_col.
      CLEAR ls_field.
      IF lr_column->formula IS NOT INITIAL.
        " a formula column has no component behind it: a number, whatever
        " the formula gives
        ls_field-name = lv_name.
        ls_field-cat = cs_cat-number.
        ls_col-formula = lr_column->formula.
        IF ls_col-formula(1) = `=`.
          ls_col-formula = substring( val = ls_col-formula
                                      off = 1 ).
        ENDIF.
      ELSE.
        ls_field = field_find( t_field = t_field
                               name    = lv_name ).
        IF ls_field IS INITIAL.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_COLUMN_UNKNOWN - the row has no elementary component { lv_name }|.
        ENDIF.
      ENDIF.

      ls_col-name = ls_field-name.
      ls_col-cat = ls_field-cat.
      ls_col-width = lr_column->width.
      ls_col-letters = column_to_letters( offset + lv_position ).

      ls_col-header = lr_column->header.
      IF ls_col-header IS INITIAL AND layout-ddic_header = abap_true AND ls_field-rollname IS NOT INITIAL.
        TRY.
            DATA(ls_text) = zabaputil_cl_util_context=>rtti_get_data_element_texts( ls_field-rollname ).
            ls_col-header = ls_text-medium.
            IF ls_col-header IS INITIAL.
              ls_col-header = ls_text-long.
            ENDIF.
          CATCH cx_root ##NO_HANDLER.
            " no text to be had - the component name below
        ENDTRY.
      ENDIF.
      IF ls_col-header IS INITIAL.
        ls_col-header = ls_col-name.
      ENDIF.
      IF ls_col-header IS INITIAL.
        " a formula column without a name and without a header
        ls_col-header = |Column{ lv_position }|.
      ENDIF.
      " an Excel table takes every column name once, and so does a reader
      " that goes by the header
      IF layout-table_style IS NOT INITIAL.
        DATA(lv_header) = ls_col-header.
        DATA(lv_suffix) = 1.
        DO.
          " in a variable: a function in the key of a table expression is
          " read as a method call on 7.02
          DATA(lv_upper) = to_upper( lv_header ).
          IF NOT line_exists( lt_taken[ table_line = lv_upper ] ).
            EXIT.
          ENDIF.
          lv_suffix = lv_suffix + 1.
          lv_header = |{ ls_col-header } { lv_suffix }|.
        ENDDO.
        ls_col-header = lv_header.
        INSERT lv_upper INTO TABLE lt_taken.
      ENDIF.

      IF lr_column->total IS NOT INITIAL.
        total_function( EXPORTING val  = lr_column->total
                        IMPORTING name = ls_col-total_fn
                                  code = ls_col-total_code ).
        IF ls_col-total_code = 0.
          ls_col-total_label = lr_column->total.
        ENDIF.
      ENDIF.

      " the number format by the type, or the one the column names - and
      " how the cells look
      DATA(lv_code) = ``.
      CASE ls_col-cat.
        WHEN cs_cat-number OR cs_cat-date OR cs_cat-time OR cs_cat-timestamp.
          lv_code = lr_column->format.
      ENDCASE.
      ls_col-style = style_attr( id    = numfmt_by_cat( ls_col-cat )
                                 code  = lv_code
                                 style = lr_column->style ).
      ls_col-plain_style = style_attr( style = lr_column->style ).

      " a link from every cell to what the row holds in a component
      IF lr_column->link IS NOT INITIAL.
        DATA(lv_link) = zabaputil_cl_util_context=>c_trim_upper( lr_column->link ).
        DATA(ls_link_field) = field_find( t_field = t_field
                                          name    = lv_link ).
        IF ls_link_field IS INITIAL.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_COLUMN_UNKNOWN - the row has no elementary component { lv_link } for a link|.
        ENDIF.
        ls_col-link = ls_link_field-name.
        DATA(ls_link_style) = lr_column->style.
        IF ls_link_style-color IS INITIAL.
          ls_link_style-color = `0563C1`.
        ENDIF.
        ls_link_style-underline = abap_true.
        ls_col-link_style = style_attr( id    = numfmt_by_cat( ls_col-cat )
                                        code  = lv_code
                                        style = ls_link_style ).
      ENDIF.

      ls_col-hidden = lr_column->hidden.
      ls_col-level = lr_column->outline_level.
      bounds_check( val  = ls_col-level
                    low  = 0
                    high = 7
                    name = `outline level` ).
      ls_col-validation = lr_column->validation.

      INSERT ls_col INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD field_find.

    LOOP AT t_field INTO result.
      IF result-name = name.
        RETURN.
      ENDIF.
    ENDLOOP.
    CLEAR result.

  ENDMETHOD.

  METHOD total_function.

    CLEAR name.
    CLEAR code.

    " the names a table part knows, and the numbers SUBTOTAL( ) takes for
    " them - the ones from 101 on, which leave out a hidden row
    " in a variable: 7.02 takes no function as the operand of a CASE
    DATA(lv_name) = to_lower( condense( val ) ).
    CASE lv_name.
      WHEN `sum`.
        name = `sum`.
        code = 109.
      WHEN `average`.
        name = `average`.
        code = 101.
      WHEN `count`.
        name = `count`.
        code = 103.
      WHEN `countnums`.
        name = `countNums`.
        code = 102.
      WHEN `max`.
        name = `max`.
        code = 104.
      WHEN `min`.
        name = `min`.
        code = 105.
      WHEN `stddev`.
        name = `stdDev`.
        code = 107.
      WHEN `var`.
        name = `var`.
        code = 110.
    ENDCASE.

  ENDMETHOD.

  METHOD total_value.

    DATA lv_number TYPE decfloat34.

    " a formula column has no values here to add up
    IF col-formula IS NOT INITIAL.
      RETURN.
    ENDIF.

    CASE col-total_fn.
      WHEN `count`.
        result = |{ col-cells }|.
        RETURN.
      WHEN `countNums`.
        result = |{ col-numbers }|.
        RETURN.
      WHEN `sum`.
        lv_number = col-sum.
      WHEN `average`.
        IF col-numbers = 0.
          RETURN.
        ENDIF.
        lv_number = col-sum / col-numbers.
      WHEN `min`.
        lv_number = col-low.
      WHEN `max`.
        lv_number = col-high.
      WHEN OTHERS.
        " stdDev, var: left to Excel
        RETURN.
    ENDCASE.
    result = number_plain( |{ lv_number }| ).

  ENDMETHOD.

  METHOD table_name_get.

    DATA lv_pos TYPE i.

    " letters, digits, underscores and periods, a letter or an underscore
    " in front
    DATA(lv_len) = strlen( val ).
    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = val
                                 off = lv_pos
                                 len = 1 ).
      IF lv_char CA `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.`.
        result = result && lv_char.
      ELSEIF lv_char <> ` `.
        result = result && `_`.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.
    IF result IS INITIAL.
      result = |Table{ mv_tables + 1 }|.
    ENDIF.
    IF result(1) CA `0123456789.`.
      result = `_` && result.
    ENDIF.
    " a name that reads as a cell - up to three letters, then digits only
    " (A1, XFD12) - is none
    lv_len = strlen( result ).
    lv_pos = 0.
    WHILE lv_pos < lv_len AND lv_pos < 4 AND result+lv_pos(1) NA cv_digits.
      lv_pos = lv_pos + 1.
    ENDWHILE.
    IF lv_pos >= 1 AND lv_pos <= 3 AND lv_pos < lv_len.
      DATA(lv_letters) = result(lv_pos).
      DATA(lv_rest) = result+lv_pos.
      IF lv_rest CO cv_digits AND lv_letters CO `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`.
        result = `_` && result.
      ENDIF.
    ENDIF.
    IF strlen( result ) > 200.
      result = result(200).
    ENDIF.

    " once in the workbook, whatever its case
    DATA(lv_base) = result.
    DATA(lv_suffix) = 1.
    DO.
      DATA(lv_upper) = to_upper( result ).
      IF NOT line_exists( mt_table_name[ table_line = lv_upper ] ).
        EXIT.
      ENDIF.
      lv_suffix = lv_suffix + 1.
      result = |{ lv_base }_{ lv_suffix }|.
    ENDDO.
    INSERT lv_upper INTO TABLE mt_table_name.

  ENDMETHOD.

  METHOD style_get.

    DATA ls_xf     TYPE ty_s_xf.
    DATA ls_font   TYPE ty_s_font.
    DATA ls_border TYPE ty_s_border.

    " index 0, the format of every cell that names none - and the font,
    " the fills and the border Excel expects to find first
    IF mt_xf IS INITIAL.
      INSERT ls_xf INTO TABLE mt_xf.
    ENDIF.
    IF mt_font IS INITIAL.
      INSERT VALUE #( size = 11
                      name = `Calibri` ) INTO TABLE mt_font.
      INSERT VALUE #( bold = abap_true
                      size = 11
                      name = `Calibri` ) INTO TABLE mt_font.
      INSERT VALUE #( ) INTO TABLE mt_border.
    ENDIF.

    ls_xf-numfmt = id.
    IF code IS NOT INITIAL.
      " a format of the caller: a custom one, named from 164 on. Looked
      " up in a loop: line_index( ) of a row that is not there comes out
      " of the 7.02 downport as the index of an earlier read
      DATA(lv_index) = 0.
      LOOP AT mt_numfmt INTO DATA(lv_code).
        IF lv_code = code.
          lv_index = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_index = 0.
        INSERT code INTO TABLE mt_numfmt.
        lv_index = lines( mt_numfmt ).
      ENDIF.
      ls_xf-numfmt = cs_numfmt-custom + lv_index - 1.
    ENDIF.

    ls_font-bold = style-bold.
    ls_font-italic = style-italic.
    ls_font-underline = style-underline.
    ls_font-strike = style-strike.
    ls_font-size = style-size.
    IF ls_font-size = 0.
      ls_font-size = 11.
    ENDIF.
    bounds_check( val  = ls_font-size
                  low  = 1
                  high = 409
                  name = `font size` ).
    ls_font-name = style-font.
    IF ls_font-name IS INITIAL.
      ls_font-name = `Calibri`.
    ENDIF.
    IF style-color IS NOT INITIAL.
      ls_font-color = color_get( style-color ).
    ENDIF.
    ls_xf-font = font_get( ls_font ).

    IF style-fill IS NOT INITIAL.
      ls_xf-fill = fill_get( color_get( style-fill ) ).
    ENDIF.

    " a side of its own, or the border of all four
    ls_border-top = style-border_top.
    IF ls_border-top IS INITIAL.
      ls_border-top = style-border.
    ENDIF.
    ls_border-bottom = style-border_bottom.
    IF ls_border-bottom IS INITIAL.
      ls_border-bottom = style-border.
    ENDIF.
    ls_border-left = style-border_left.
    IF ls_border-left IS INITIAL.
      ls_border-left = style-border.
    ENDIF.
    ls_border-right = style-border_right.
    IF ls_border-right IS INITIAL.
      ls_border-right = style-border.
    ENDIF.
    ls_border-top = border_style( ls_border-top ).
    ls_border-bottom = border_style( ls_border-bottom ).
    ls_border-left = border_style( ls_border-left ).
    ls_border-right = border_style( ls_border-right ).
    IF ls_border IS NOT INITIAL AND style-border_color IS NOT INITIAL.
      ls_border-color = color_get( style-border_color ).
    ENDIF.
    IF ls_border IS NOT INITIAL.
      ls_xf-border = border_get( ls_border ).
    ENDIF.

    CASE style-halign.
      WHEN `` OR `left` OR `center` OR `right` OR `fill` OR `justify` OR `centerContinuous`.
        ls_xf-halign = style-halign.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_STYLE - { style-halign } is no horizontal alignment|.
    ENDCASE.
    CASE style-valign.
      WHEN `` OR `top` OR `center` OR `bottom`.
        ls_xf-valign = style-valign.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_STYLE - { style-valign } is no vertical alignment|.
    ENDCASE.
    ls_xf-wrap = style-wrap.
    bounds_check( val  = style-indent
                  low  = 0
                  high = 250
                  name = `indent` ).
    ls_xf-indent = style-indent.
    IF style-rotation <> 255.
      bounds_check( val  = style-rotation
                    low  = 0
                    high = 180
                    name = `rotation` ).
    ENDIF.
    ls_xf-rotation = style-rotation.

    LOOP AT mt_xf REFERENCE INTO DATA(lr_xf).
      IF lr_xf->* = ls_xf.
        result = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF result = 0.
      INSERT ls_xf INTO TABLE mt_xf.
      result = lines( mt_xf ).
    ENDIF.
    result = result - 1.

  ENDMETHOD.

  METHOD style_attr.

    DATA(lv_index) = style_get( id    = id
                                code  = code
                                style = style ).
    IF lv_index > 0.
      result = | s="{ lv_index }"|.
    ENDIF.

  ENDMETHOD.

  METHOD numfmt_by_cat.

    CASE cat.
      WHEN cs_cat-date.
        result = cs_numfmt-date.
      WHEN cs_cat-time.
        result = cs_numfmt-time.
      WHEN cs_cat-timestamp.
        result = cs_numfmt-datetime.
      WHEN OTHERS.
        result = cs_numfmt-general.
    ENDCASE.

  ENDMETHOD.

  METHOD font_get.

    LOOP AT mt_font REFERENCE INTO DATA(lr_font).
      IF lr_font->* = val.
        result = sy-tabix - 1.
        RETURN.
      ENDIF.
    ENDLOOP.
    INSERT val INTO TABLE mt_font.
    result = lines( mt_font ) - 1.

  ENDMETHOD.

  METHOD fill_get.

    " the two fills every workbook starts with come first
    LOOP AT mt_fill INTO DATA(lv_fill).
      IF lv_fill = val.
        result = sy-tabix + 1.
        RETURN.
      ENDIF.
    ENDLOOP.
    INSERT val INTO TABLE mt_fill.
    result = lines( mt_fill ) + 1.

  ENDMETHOD.

  METHOD border_get.

    LOOP AT mt_border REFERENCE INTO DATA(lr_border).
      IF lr_border->* = val.
        result = sy-tabix - 1.
        RETURN.
      ENDIF.
    ENDLOOP.
    INSERT val INTO TABLE mt_border.
    result = lines( mt_border ) - 1.

  ENDMETHOD.

  METHOD color_get.

    DATA(lv_color) = to_upper( zabaputil_cl_util_context=>c_trim( val ) ).
    IF strlen( lv_color ) > 0 AND lv_color(1) = `#`.
      lv_color = substring( val = lv_color
                            off = 1 ).
    ENDIF.
    IF strlen( lv_color ) <> 6 OR lv_color CN cv_hex.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_STYLE - { val } is no colour of six hex digits (C00000)|.
    ENDIF.
    result = |FF{ lv_color }|.

  ENDMETHOD.

  METHOD border_style.

    CASE val.
      WHEN ``.
      WHEN `thin` OR `medium` OR `thick` OR `dashed` OR `dotted` OR `double` OR `hair`.
        result = val.
      WHEN `mediumDashed` OR `dashDot` OR `mediumDashDot` OR `dashDotDot` OR `slantDashDot`.
        result = val.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_STYLE - { val } is no border style (thin, medium, thick, ...)|.
    ENDCASE.

  ENDMETHOD.

  METHOD sheet_name_get.

    DATA lv_pos TYPE i.

    DATA(lv_name) = zabaputil_cl_util_context=>c_trim( val ).
    IF lv_name IS INITIAL.
      lv_name = |Sheet{ lines( mt_out_sheet ) + 1 }|.
    ENDIF.

    " what Excel does not take in a sheet name
    DATA(lv_len) = strlen( lv_name ).
    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = lv_name
                                 off = lv_pos
                                 len = 1 ).
      IF lv_char CA `\\/?*[]:`.
        lv_char = `_`.
      ENDIF.
      result = result && lv_char.
      lv_pos = lv_pos + 1.
    ENDWHILE.
    IF strlen( result ) > 31.
      result = substring( val = result
                          len = 31 ).
    ENDIF.

    " a name the workbook has, whatever its case
    DATA(lv_base) = result.
    DATA(lv_suffix) = 1.
    DO.
      DATA(lv_taken) = abap_false.
      LOOP AT mt_out_sheet REFERENCE INTO DATA(lr_sheet).
        IF to_upper( lr_sheet->name ) = to_upper( result ).
          lv_taken = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_taken = abap_false.
        EXIT.
      ENDIF.
      lv_suffix = lv_suffix + 1.
      DATA(lv_tail) = | ({ lv_suffix })|.
      DATA(lv_keep) = nmin( val1 = strlen( lv_base )
                            val2 = 31 - strlen( lv_tail ) ).
      result = substring( val = lv_base
                          len = lv_keep ) && lv_tail.
    ENDDO.

  ENDMETHOD.

  METHOD cell_text.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    " c_escape_xml drops the control characters XML forbids (Excel
    " "repairs" a workbook with one). `_x` in front of four hex digits is
    " how a file names a character by its number, so a text that reads like
    " that has its underscore named that way
    DATA(lv_text) = zabaputil_cl_util_context=>c_escape_xml( val ).
    IF lv_text CS `_x`.
      lv_text = replace( val  = lv_text
                         sub  = `_x`
                         with = `_x005F_x`
                         occ  = 0 ).
    ENDIF.
    " xml:space keeps the blanks in front and behind
    result = |<c r="{ address }"{ style } t="inlineStr"><is><t xml:space="preserve">{ lv_text }</t></is></c>|.

  ENDMETHOD.

  METHOD date_to_serial.

    IF val IS INITIAL OR val < cv_date_1900 OR val > cv_date_max.
      RETURN.
    ENDIF.
    result = val - cv_date_base.
    IF val < cv_date_march.
      result = result - 1.
    ENDIF.
    " what is no date at all counts as day 0 of the calendar
    IF result < 1.
      result = 0.
    ENDIF.

  ENDMETHOD.

  METHOD table_xml.

    DATA lt_xml TYPE string_table.

    DATA(lv_totals) = ` totalsRowShown="0"`.
    IF totals = abap_true.
      lv_totals = ` totalsRowCount="1"`.
    ENDIF.
    " one piece per INSERT, here and wherever a part is put together: the
    " 7.02 downport writes the operands of a `&&` in an INSERT on one line,
    " and a line of more than 255 characters is cut on import
    INSERT |{ cv_xml_head }<table xmlns="{ cs_ns-main }" id="{ id }" name="{ name }" displayName="{ name }"|
           INTO TABLE lt_xml.
    INSERT | ref="{ range }"{ lv_totals }>| INTO TABLE lt_xml.
    IF filter IS NOT INITIAL.
      INSERT |<autoFilter ref="{ filter }"/>| INTO TABLE lt_xml.
    ENDIF.
    INSERT |<tableColumns count="{ lines( t_col ) }">| INTO TABLE lt_xml.
    LOOP AT t_col REFERENCE INTO DATA(lr_col).
      DATA(lv_total) = ``.
      IF lr_col->total_fn IS NOT INITIAL.
        lv_total = | totalsRowFunction="{ lr_col->total_fn }"|.
      ELSEIF lr_col->total_label IS NOT INITIAL.
        lv_total = | totalsRowLabel="{ zabaputil_cl_util_context=>c_escape_xml( lr_col->total_label ) }"|.
      ENDIF.
      DATA(lv_column) = zabaputil_cl_util_context=>c_escape_xml( lr_col->header ).
      IF lr_col->formula IS INITIAL.
        INSERT |<tableColumn id="{ sy-tabix }" name="{ lv_column }"{ lv_total }/>| INTO TABLE lt_xml.
        CONTINUE.
      ENDIF.
      " a formula column: Excel fills a row added to the table with it -
      " written for the first row of the table
      DATA(lv_formula) = replace( val  = lr_col->formula
                                  sub  = `{row}`
                                  with = |{ first_row }|
                                  occ  = 0 ).
      lv_formula = zabaputil_cl_util_context=>c_escape_xml( lv_formula ).
      INSERT |<tableColumn id="{ sy-tabix }" name="{ lv_column }"{ lv_total }>| INTO TABLE lt_xml.
      INSERT |<calculatedColumnFormula>{ lv_formula }</calculatedColumnFormula></tableColumn>| INTO TABLE lt_xml.
    ENDLOOP.
    DATA(lv_rows) = `1`.
    IF layout-no_row_stripes = abap_true.
      lv_rows = `0`.
    ENDIF.
    DATA(lv_columns) = `0`.
    IF layout-column_stripes = abap_true.
      lv_columns = `1`.
    ENDIF.
    DATA(lv_style) = zabaputil_cl_util_context=>c_escape_xml( layout-table_style ).
    INSERT |</tableColumns><tableStyleInfo name="{ lv_style }" showFirstColumn="0" showLastColumn="0"|
           INTO TABLE lt_xml.
    INSERT | showRowStripes="{ lv_rows }" showColumnStripes="{ lv_columns }"/></table>| INTO TABLE lt_xml.
    result = concat_lines_of( lt_xml ).

  ENDMETHOD.

  METHOD styles_xml.

    DATA lt_xml TYPE string_table.

    IF mt_xf IS INITIAL.
      style_get( ).
    ENDIF.

    INSERT |{ cv_xml_head }<styleSheet xmlns="{ cs_ns-main }">| INTO TABLE lt_xml.
    IF mt_numfmt IS NOT INITIAL.
      INSERT |<numFmts count="{ lines( mt_numfmt ) }">| INTO TABLE lt_xml.
      LOOP AT mt_numfmt INTO DATA(lv_code).
        DATA(lv_id) = cs_numfmt-custom + sy-tabix - 1.
        INSERT |<numFmt numFmtId="{ lv_id }" formatCode="{ zabaputil_cl_util_context=>c_escape_xml( lv_code ) }"/>|
               INTO TABLE lt_xml.
      ENDLOOP.
      INSERT `</numFmts>` INTO TABLE lt_xml.
    ENDIF.

    " font 0 the default, font 1 the bold one of a header row
    INSERT |<fonts count="{ lines( mt_font ) }">| INTO TABLE lt_xml.
    LOOP AT mt_font REFERENCE INTO DATA(lr_font).
      DATA(lv_font) = `<font>`.
      IF lr_font->bold = abap_true.
        lv_font = lv_font && `<b/>`.
      ENDIF.
      IF lr_font->italic = abap_true.
        lv_font = lv_font && `<i/>`.
      ENDIF.
      IF lr_font->strike = abap_true.
        lv_font = lv_font && `<strike/>`.
      ENDIF.
      IF lr_font->underline = abap_true.
        lv_font = lv_font && `<u/>`.
      ENDIF.
      lv_font = |{ lv_font }<sz val="{ lr_font->size }"/>|.
      IF lr_font->color IS NOT INITIAL.
        lv_font = |{ lv_font }<color rgb="{ lr_font->color }"/>|.
      ENDIF.
      DATA(lv_name) = zabaputil_cl_util_context=>c_escape_xml( lr_font->name ).
      INSERT |{ lv_font }<name val="{ lv_name }"/><family val="2"/></font>| INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</fonts>` INTO TABLE lt_xml.

    " the two fills Excel expects to find, then the colours of the cells
    INSERT |<fills count="{ lines( mt_fill ) + 2 }"><fill><patternFill patternType="none"/></fill>|
           INTO TABLE lt_xml.
    INSERT `<fill><patternFill patternType="gray125"/></fill>` INTO TABLE lt_xml.
    LOOP AT mt_fill INTO DATA(lv_fill).
      INSERT |<fill><patternFill patternType="solid"><fgColor rgb="{ lv_fill }"/>| INTO TABLE lt_xml.
      INSERT `<bgColor indexed="64"/></patternFill></fill>` INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</fills>` INTO TABLE lt_xml.

    INSERT |<borders count="{ lines( mt_border ) }">| INTO TABLE lt_xml.
    LOOP AT mt_border REFERENCE INTO DATA(lr_border).
      DATA(lv_color) = ``.
      IF lr_border->color IS NOT INITIAL.
        lv_color = |<color rgb="{ lr_border->color }"/>|.
      ENDIF.
      DATA(lv_border) = `<border>`.
      IF lr_border->left IS INITIAL.
        lv_border = |{ lv_border }<left/>|.
      ELSE.
        lv_border = |{ lv_border }<left style="{ lr_border->left }">{ lv_color }</left>|.
      ENDIF.
      IF lr_border->right IS INITIAL.
        lv_border = |{ lv_border }<right/>|.
      ELSE.
        lv_border = |{ lv_border }<right style="{ lr_border->right }">{ lv_color }</right>|.
      ENDIF.
      IF lr_border->top IS INITIAL.
        lv_border = |{ lv_border }<top/>|.
      ELSE.
        lv_border = |{ lv_border }<top style="{ lr_border->top }">{ lv_color }</top>|.
      ENDIF.
      IF lr_border->bottom IS INITIAL.
        lv_border = |{ lv_border }<bottom/>|.
      ELSE.
        lv_border = |{ lv_border }<bottom style="{ lr_border->bottom }">{ lv_color }</bottom>|.
      ENDIF.
      INSERT |{ lv_border }<diagonal/></border>| INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</borders>` INTO TABLE lt_xml.

    INSERT `<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>`
           INTO TABLE lt_xml.
    INSERT |<cellXfs count="{ lines( mt_xf ) }">| INTO TABLE lt_xml.
    LOOP AT mt_xf REFERENCE INTO DATA(lr_xf).
      DATA(lv_apply) = ``.
      IF lr_xf->font <> 0.
        lv_apply = ` applyFont="1"`.
      ENDIF.
      IF lr_xf->numfmt <> cs_numfmt-general.
        lv_apply = lv_apply && ` applyNumberFormat="1"`.
      ENDIF.
      IF lr_xf->fill <> 0.
        lv_apply = lv_apply && ` applyFill="1"`.
      ENDIF.
      IF lr_xf->border <> 0.
        lv_apply = lv_apply && ` applyBorder="1"`.
      ENDIF.
      DATA(lv_align) = ``.
      IF lr_xf->halign IS NOT INITIAL.
        lv_align = | horizontal="{ lr_xf->halign }"|.
      ENDIF.
      IF lr_xf->valign IS NOT INITIAL.
        lv_align = |{ lv_align } vertical="{ lr_xf->valign }"|.
      ENDIF.
      IF lr_xf->wrap = abap_true.
        lv_align = |{ lv_align } wrapText="1"|.
      ENDIF.
      IF lr_xf->indent > 0.
        lv_align = |{ lv_align } indent="{ lr_xf->indent }"|.
      ENDIF.
      IF lr_xf->rotation > 0.
        lv_align = |{ lv_align } textRotation="{ lr_xf->rotation }"|.
      ENDIF.
      DATA(lv_xf) = |<xf numFmtId="{ lr_xf->numfmt }" fontId="{ lr_xf->font }" fillId="{ lr_xf->fill }"|.
      lv_xf = |{ lv_xf } borderId="{ lr_xf->border }" xfId="0"{ lv_apply }|.
      IF lv_align IS INITIAL.
        INSERT |{ lv_xf }/>| INTO TABLE lt_xml.
      ELSE.
        INSERT |{ lv_xf } applyAlignment="1"><alignment{ lv_align }/></xf>| INTO TABLE lt_xml.
      ENDIF.
    ENDLOOP.
    INSERT `</cellXfs><cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>`
           INTO TABLE lt_xml.
    INSERT `</styleSheet>` INTO TABLE lt_xml.
    result = concat_lines_of( lt_xml ).

  ENDMETHOD.

  METHOD get_xlsx.

    DATA lt_file  TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA lt_types TYPE string_table.
    DATA lt_book  TYPE string_table.
    DATA lt_rels  TYPE string_table.
    DATA lt_root  TYPE string_table.
    DATA lv_core  TYPE xstring.
    DATA lv_app   TYPE xstring.

    IF mv_loaded = abap_true.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_READ_ONLY - a workbook that was read is not written again`.
    ENDIF.
    sheet_close( ).
    " a workbook without a sheet is none Excel opens
    IF mt_out_sheet IS INITIAL.
      INSERT VALUE #( name = `Sheet1`
                      xml  = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                                 |{ cv_xml_head }<worksheet xmlns="{ cs_ns-main }"><sheetData/></worksheet>| ) )
             INTO TABLE mt_out_sheet.
    ENDIF.
    " nor one that hides every sheet - the first one Excel shows
    DATA(lv_active) = -1.
    LOOP AT mt_out_sheet REFERENCE INTO DATA(lr_sheet) WHERE hidden = abap_false.
      lv_active = sy-tabix - 1.
      EXIT.
    ENDLOOP.
    IF lv_active < 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_ALL_HIDDEN - a workbook shows one sheet at least, every sheet is hidden`.
    ENDIF.

    INSERT |{ cv_xml_head }<Types xmlns="{ cs_ns-types }">| INTO TABLE lt_types.
    INSERT `<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>`
           INTO TABLE lt_types.
    INSERT `<Default Extension="xml" ContentType="application/xml"/>` INTO TABLE lt_types.
    INSERT |<Override PartName="/xl/workbook.xml" ContentType="{ cs_ns-ct }.sheet.main+xml"/>|
           INTO TABLE lt_types.
    INSERT |<Override PartName="/xl/styles.xml" ContentType="{ cs_ns-ct }.styles+xml"/>|
           INTO TABLE lt_types.
    INSERT |{ cv_xml_head }<workbook xmlns="{ cs_ns-main }" xmlns:r="{ cs_ns-rel }">| INTO TABLE lt_book.
    IF lv_active = 0.
      INSERT `<bookViews><workbookView/></bookViews><sheets>` INTO TABLE lt_book.
    ELSE.
      INSERT |<bookViews><workbookView activeTab="{ lv_active }"/></bookViews><sheets>| INTO TABLE lt_book.
    ENDIF.
    INSERT |{ cv_xml_head }<Relationships xmlns="{ cs_ns-pkg_rel }">| INTO TABLE lt_rels.

    LOOP AT mt_out_sheet REFERENCE INTO lr_sheet.
      DATA(lv_index) = sy-tabix.
      INSERT |<Override PartName="/xl/worksheets/sheet{ lv_index }.xml" ContentType="{ cs_ns-ct }.worksheet+xml"/>|
             INTO TABLE lt_types.
      DATA(lv_name) = zabaputil_cl_util_context=>c_escape_xml( lr_sheet->name ).
      DATA(lv_state) = ``.
      IF lr_sheet->hidden = abap_true.
        lv_state = ` state="hidden"`.
      ENDIF.
      INSERT |<sheet name="{ lv_name }" sheetId="{ lv_index }"{ lv_state } r:id="rId{ lv_index }"/>| INTO TABLE lt_book.
      INSERT |<Relationship Id="rId{ lv_index }" Type="{ cs_ns-rel }/worksheet"| INTO TABLE lt_rels.
      INSERT | Target="worksheets/sheet{ lv_index }.xml"/>| INTO TABLE lt_rels.
      INSERT VALUE #( name    = |xl/worksheets/sheet{ lv_index }.xml|
                      content = lr_sheet->xml ) INTO TABLE lt_file.

      LOOP AT lr_sheet->t_table REFERENCE INTO DATA(lr_table).
        INSERT |<Override PartName="/xl/tables/table{ lr_table->id }.xml" ContentType="{ cs_ns-ct }.table+xml"/>|
               INTO TABLE lt_types.
        INSERT VALUE #( name    = |xl/tables/table{ lr_table->id }.xml|
                        content = lr_table->xml ) INTO TABLE lt_file.
      ENDLOOP.
      IF lr_sheet->rels IS NOT INITIAL.
        INSERT VALUE #( name    = |xl/worksheets/_rels/sheet{ lv_index }.xml.rels|
                        content = lr_sheet->rels ) INTO TABLE lt_file.
      ENDIF.
    ENDLOOP.

    INSERT `</sheets>` INTO TABLE lt_book.
    IF mt_name IS NOT INITIAL.
      INSERT `<definedNames>` INTO TABLE lt_book.
      LOOP AT mt_name REFERENCE INTO DATA(lr_name).
        DATA(lv_local) = ``.
        IF lr_name->sheet > 0.
          lv_local = | localSheetId="{ lr_name->sheet - 1 }"|.
        ENDIF.
        DATA(lv_ref) = zabaputil_cl_util_context=>c_escape_xml( lr_name->ref ).
        INSERT |<definedName name="{ lr_name->name }"{ lv_local }>{ lv_ref }</definedName>| INTO TABLE lt_book.
      ENDLOOP.
      INSERT `</definedNames>` INTO TABLE lt_book.
    ENDIF.
    IF mv_formulas = abap_true.
      " the formulas carry no value, or one worked out here: Excel works
      " them out when it opens the file
      INSERT `<calcPr fullCalcOnLoad="1"/>` INTO TABLE lt_book.
    ENDIF.
    INSERT `</workbook>` INTO TABLE lt_book.
    INSERT |<Relationship Id="rId{ lines( mt_out_sheet ) + 1 }" Type="{ cs_ns-rel }/styles" Target="styles.xml"/>|
           INTO TABLE lt_rels.
    INSERT `</Relationships>` INTO TABLE lt_rels.

    " the relationships of the package: the workbook, and the document
    " properties where they are set
    INSERT |{ cv_xml_head }<Relationships xmlns="{ cs_ns-pkg_rel }">| INTO TABLE lt_root.
    INSERT |<Relationship Id="rId1" Type="{ cs_ns-rel }/officeDocument" Target="xl/workbook.xml"/>| INTO TABLE lt_root.
    IF mv_props = abap_true.
      props_xml( IMPORTING core = lv_core
                           app  = lv_app ).
      INSERT `<Relationship Id="rId2" Target="docProps/core.xml"` INTO TABLE lt_root.
      INSERT | Type="{ cs_ns-pkg_rel }/metadata/core-properties"/>| INTO TABLE lt_root.
      INSERT |<Relationship Id="rId3" Type="{ cs_ns-rel }/extended-properties" Target="docProps/app.xml"/>|
             INTO TABLE lt_root.
      INSERT `<Override PartName="/docProps/core.xml"` INTO TABLE lt_types.
      INSERT ` ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>` INTO TABLE lt_types.
      INSERT `<Override PartName="/docProps/app.xml"` INTO TABLE lt_types.
      INSERT | ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>|
             INTO TABLE lt_types.
      INSERT VALUE #( name    = `docProps/core.xml`
                      content = lv_core ) INTO TABLE lt_file.
      INSERT VALUE #( name    = `docProps/app.xml`
                      content = lv_app ) INTO TABLE lt_file.
    ENDIF.
    INSERT `</Relationships>` INTO TABLE lt_root.
    INSERT `</Types>` INTO TABLE lt_types.

    INSERT VALUE #( name    = `[Content_Types].xml`
                    content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_types ) ) )
           INTO lt_file INDEX 1.
    INSERT VALUE #( name    = `_rels/.rels`
                    content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_root ) ) )
           INTO lt_file INDEX 2.
    INSERT VALUE #( name    = `xl/workbook.xml`
                    content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_book ) ) )
           INTO lt_file INDEX 3.
    INSERT VALUE #( name    = `xl/_rels/workbook.xml.rels`
                    content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_rels ) ) )
           INTO lt_file INDEX 4.
    INSERT VALUE #( name    = `xl/styles.xml`
                    content = zabaputil_cl_util_context=>conv_get_xstring_by_string( styles_xml( ) ) )
           INTO lt_file INDEX 5.

    result = zabaputil_cl_util_context=>zip_pack( lt_file ).

  ENDMETHOD.

  METHOD column_to_letters.

    DATA(lv_rest) = val.
    WHILE lv_rest > 0.
      DATA(lv_digit) = ( lv_rest - 1 ) MOD 26.
      result = substring( val = cv_letters
                          off = lv_digit
                          len = 1 ) && result.
      lv_rest = ( lv_rest - 1 ) DIV 26.
    ENDWHILE.

  ENDMETHOD.

  METHOD column_to_number.

    DATA lv_pos TYPE i.

    DATA(lv_text) = to_upper( val ).
    DATA(lv_len) = strlen( lv_text ).
    WHILE lv_pos < lv_len.
      DATA(lv_found) = find( val = cv_letters
                             sub = substring( val = lv_text
                                              off = lv_pos
                                              len = 1 ) ).
      lv_pos = lv_pos + 1.
      IF lv_found < 0.
        " the $ of an absolute address, and the row behind the letters
        IF result > 0.
          RETURN.
        ENDIF.
        CONTINUE.
      ENDIF.
      result = result * 26 + lv_found + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD load.

    DATA lv_kind TYPE string.
    DATA lv_name TYPE string.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.

    " a workbook is a ZIP archive, and one starts with PK. Looked at here:
    " what the ZIP class does with other bytes differs by release
    CONSTANTS lc_zip TYPE x LENGTH 2 VALUE '504B'.
    CONSTANTS lc_ole TYPE x LENGTH 4 VALUE 'D0CF11E0'.

    mv_loaded = abap_true.

    IF xstrlen( val ) >= 4 AND val(4) = lc_ole.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NO_WORKBOOK - the file is an .xls file of Excel 97-2003 or a workbook with a password`.
    ENDIF.
    IF xstrlen( val ) < 4 OR val(2) <> lc_zip.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NO_WORKBOOK - the file is no .xlsx workbook`.
    ENDIF.
    TRY.
        mt_part = zabaputil_cl_util_context=>zip_unpack( val ).
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `XLSX_NO_WORKBOOK - the file is no .xlsx workbook`
            previous = x.
    ENDTRY.

    " the workbook part is the one the package names as its document
    DATA(lv_book) = `xl/workbook.xml`.
    LOOP AT rels_read( `_rels/.rels` ) REFERENCE INTO DATA(lr_rel).
      IF zabaputil_cl_util_context=>c_ends_with( val    = lr_rel->type
                                                 suffix = `/officeDocument` ) = abap_true.
        lv_book = path_resolve( base   = ``
                                target = lr_rel->target ).
      ENDIF.
    ENDLOOP.
    DATA(lv_content) = part_get( lv_book ).
    IF lv_content IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NO_WORKBOOK - the file is no .xlsx workbook`.
    ENDIF.

    DATA(lv_folder) = path_folder( lv_book ).
    DATA(lt_rel) = rels_read( |{ lv_folder }_rels/{ substring( val = lv_book
                                                              off = strlen( lv_folder ) ) }.rels| ).

    DATA(lo_reader) = zabaputil_cl_util_context=>xml_reader_create( lv_content ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            t_attribute = lt_attr ).
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-end.
        EXIT.
      ENDIF.
      IF lv_kind <> zabaputil_cl_util_context=>cs_xml_node-open.
        CONTINUE.
      ENDIF.
      CASE lv_name.
        WHEN `workbookPr`.
          " the date system of Excel for Mac up to 2008: day 0 is 1904-01-01
          DATA(lv_1904) = attr( t_attribute = lt_attr
                                name        = `date1904` ).
          mv_date1904 = xsdbool( lv_1904 = `1` OR lv_1904 = `true` ).
        WHEN `sheet`.
          DATA(lv_id) = attr( t_attribute = lt_attr
                              name        = `id` ).
          LOOP AT lt_rel REFERENCE INTO lr_rel.
            IF lr_rel->id = lv_id.
              INSERT VALUE #( name = attr( t_attribute = lt_attr
                                           name        = `name` )
                              path = path_resolve( base   = lv_book
                                                   target = lr_rel->target ) ) INTO TABLE mt_sheet.
              EXIT.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDDO.

    LOOP AT lt_rel REFERENCE INTO lr_rel.
      DATA(lv_path) = path_resolve( base   = lv_book
                                    target = lr_rel->target ).
      IF zabaputil_cl_util_context=>c_ends_with( val    = lr_rel->type
                                                 suffix = `/sharedStrings` ) = abap_true.
        shared_read( part_get( lv_path ) ).
        " read once, and a large part - not kept twice
        DELETE mt_part WHERE name = lv_path.
      ELSEIF zabaputil_cl_util_context=>c_ends_with( val    = lr_rel->type
                                                     suffix = `/styles` ) = abap_true.
        styles_read( part_get( lv_path ) ).
        DELETE mt_part WHERE name = lv_path.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD part_get.

    LOOP AT mt_part REFERENCE INTO DATA(lr_part).
      IF lr_part->name = val.
        result = lr_part->content.
        RETURN.
      ENDIF.
    ENDLOOP.
    " a package names its parts without regard to case
    DATA(lv_name) = to_upper( val ).
    LOOP AT mt_part REFERENCE INTO lr_part.
      IF to_upper( lr_part->name ) = lv_name.
        result = lr_part->content.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD rels_read.

    DATA lv_kind TYPE string.
    DATA lv_name TYPE string.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.

    DATA(lv_content) = part_get( val ).
    IF lv_content IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_reader) = zabaputil_cl_util_context=>xml_reader_create( lv_content ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            t_attribute = lt_attr ).
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-end.
        EXIT.
      ENDIF.
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-open AND lv_name = `Relationship`.
        INSERT VALUE #( id     = attr( t_attribute = lt_attr
                                       name        = `Id` )
                        type   = attr( t_attribute = lt_attr
                                       name        = `Type` )
                        target = attr( t_attribute = lt_attr
                                       name        = `Target` ) ) INTO TABLE result.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD shared_read.

    DATA lv_kind  TYPE string.
    DATA lv_name  TYPE string.
    DATA lv_value TYPE string.
    DATA lv_text  TYPE string.
    DATA lv_take  TYPE abap_bool.
    DATA lv_ruby  TYPE abap_bool.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_reader) = zabaputil_cl_util_context=>xml_reader_create( val ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader = lo_reader
                                                  IMPORTING kind   = lv_kind
                                                            name   = lv_name
                                                            value  = lv_value ).
      CASE lv_kind.
        WHEN zabaputil_cl_util_context=>cs_xml_node-open.
          CASE lv_name.
            WHEN `si`.
              CLEAR lv_text.
            WHEN `rPh`.
              " the reading aid of a Japanese text is no part of the text
              lv_ruby = abap_true.
            WHEN `t`.
              " one t, or one per run of a text formatted in parts
              lv_take = xsdbool( lv_ruby = abap_false ).
          ENDCASE.
        WHEN zabaputil_cl_util_context=>cs_xml_node-value.
          IF lv_take = abap_true.
            lv_text = lv_text && lv_value.
          ENDIF.
        WHEN zabaputil_cl_util_context=>cs_xml_node-close.
          CASE lv_name.
            WHEN `t`.
              lv_take = abap_false.
            WHEN `rPh`.
              lv_ruby = abap_false.
            WHEN `si`.
              INSERT text_decode( lv_text ) INTO TABLE mt_shared.
          ENDCASE.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDDO.

  ENDMETHOD.

  METHOD styles_read.

    TYPES:
      BEGIN OF ty_s_fmt,
        id   TYPE i,
        code TYPE string,
      END OF ty_s_fmt.
    DATA lt_fmt  TYPE STANDARD TABLE OF ty_s_fmt WITH EMPTY KEY.
    DATA lv_kind TYPE string.
    DATA lv_name TYPE string.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.
    DATA lv_cell TYPE abap_bool.
    DATA lv_id   TYPE i.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_reader) = zabaputil_cl_util_context=>xml_reader_create( val ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            t_attribute = lt_attr ).
      CASE lv_kind.
        WHEN zabaputil_cl_util_context=>cs_xml_node-open.
          CASE lv_name.
            WHEN `numFmt`.
              lv_id = hex_to_int( attr( t_attribute = lt_attr
                                        name        = `numFmtId` ) ).
              INSERT VALUE #( id   = lv_id
                              code = attr( t_attribute = lt_attr
                                           name        = `formatCode` ) ) INTO TABLE lt_fmt.
            WHEN `cellXfs`.
              " the formats a cell names - cellStyleXfs holds xf elements too
              lv_cell = abap_true.
            WHEN `xf`.
              IF lv_cell = abap_false.
                CONTINUE.
              ENDIF.
              lv_id = hex_to_int( attr( t_attribute = lt_attr
                                        name        = `numFmtId` ) ).
              DATA(lv_code) = ``.
              LOOP AT lt_fmt REFERENCE INTO DATA(lr_fmt).
                IF lr_fmt->id = lv_id.
                  lv_code = lr_fmt->code.
                  EXIT.
                ENDIF.
              ENDLOOP.
              INSERT format_kind( id   = lv_id
                                  code = lv_code ) INTO TABLE mt_style_kind.
          ENDCASE.
        WHEN zabaputil_cl_util_context=>cs_xml_node-close.
          IF lv_name = `cellXfs`.
            EXIT.
          ENDIF.
        WHEN zabaputil_cl_util_context=>cs_xml_node-end.
          EXIT.
      ENDCASE.
    ENDDO.

  ENDMETHOD.

  METHOD format_kind.

    DATA lv_pos     TYPE i.
    DATA lv_bare    TYPE string.
    DATA lv_elapsed TYPE abap_bool.

    IF code IS INITIAL.
      " a built-in format: the ids of the dates and times of every locale
      IF id >= 14 AND id <= 17 OR id >= 27 AND id <= 36 OR id >= 50 AND id <= 58.
        result = cs_kind-date.
      ELSEIF id >= 18 AND id <= 21 OR id = 45 OR id = 47.
        result = cs_kind-time.
      ELSEIF id = 22.
        result = cs_kind-timestamp.
      ELSEIF id = 46.
        result = `L`.
      ENDIF.
      RETURN.
    ENDIF.

    " The letters that format the value: without the quoted text, the
    " character behind a backslash, an underscore or an asterisk, and
    " the sections in brackets (a colour, a locale, a condition) - but a
    " section of h, m or s only is a time that runs on past 24 hours
    DATA(lv_code) = to_lower( code ).
    DATA(lv_len) = strlen( lv_code ).
    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = lv_code
                                 off = lv_pos
                                 len = 1 ).
      lv_pos = lv_pos + 1.
      CASE lv_char.
        WHEN `"`.
          DATA(lv_end) = find( val = lv_code
                               sub = `"`
                               off = lv_pos ).
          IF lv_end < 0.
            EXIT.
          ENDIF.
          lv_pos = lv_end + 1.
        WHEN `\\` OR `_` OR `*`.
          lv_pos = lv_pos + 1.
        WHEN `[`.
          lv_end = find( val = lv_code
                         sub = `]`
                         off = lv_pos ).
          IF lv_end < 0.
            EXIT.
          ENDIF.
          DATA(lv_section) = substring( val = lv_code
                                        off = lv_pos
                                        len = lv_end - lv_pos ).
          IF lv_section IS NOT INITIAL AND ( lv_section CO `h` OR lv_section CO `m` OR lv_section CO `s` ).
            lv_elapsed = abap_true.
          ENDIF.
          lv_pos = lv_end + 1.
        WHEN OTHERS.
          lv_bare = lv_bare && lv_char.
      ENDCASE.
    ENDWHILE.

    DATA(lv_date) = xsdbool( lv_bare CA `dy` ).
    DATA(lv_time) = xsdbool( lv_elapsed = abap_true OR lv_bare CA `hs` ).
    IF lv_date = abap_false AND lv_time = abap_false AND lv_bare CA `m`.
      " months only (`mmmm`) - no number format has an m of its own
      lv_date = abap_true.
    ENDIF.

    IF lv_date = abap_true AND lv_time = abap_true.
      result = cs_kind-timestamp.
    ELSEIF lv_date = abap_true.
      result = cs_kind-date.
    ELSEIF lv_elapsed = abap_true.
      result = `L`.
    ELSEIF lv_time = abap_true.
      result = cs_kind-time.
    ENDIF.

  ENDMETHOD.

  METHOD get_sheet_names.

    IF mv_loaded = abap_true.
      LOOP AT mt_sheet REFERENCE INTO DATA(lr_sheet).
        INSERT lr_sheet->name INTO TABLE result.
      ENDLOOP.
    ELSE.
      LOOP AT mt_out_sheet REFERENCE INTO DATA(lr_out).
        INSERT lr_out->name INTO TABLE result.
      ENDLOOP.
      " the sheet that is written now
      IF mv_open = abap_true.
        INSERT ms_cur-name INTO TABLE result.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD sheet_part.

    DATA lv_path TYPE string.

    IF mv_loaded = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NOT_READ - a workbook built with factory( ) is written, not read`.
    ENDIF.

    IF sheet IS NOT INITIAL.
      DATA(lv_name) = to_upper( sheet ).
      LOOP AT mt_sheet REFERENCE INTO DATA(lr_sheet).
        IF to_upper( lr_sheet->name ) = lv_name.
          lv_path = lr_sheet->path.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_path IS INITIAL.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_SHEET_UNKNOWN - the workbook has no sheet { sheet }|.
      ENDIF.
    ELSE.
      DATA(lv_sheets) = lines( mt_sheet ).
      IF index < 1 OR index > lv_sheets.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_SHEET_UNKNOWN - the workbook has { lv_sheets } sheets, none at { index }|.
      ENDIF.
      lv_path = mt_sheet[ index ]-path.
    ENDIF.

    result = part_get( lv_path ).
    IF result IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_SHEET_UNKNOWN - the file has no part { lv_path }|.
    ENDIF.

  ENDMETHOD.

  METHOD sheet_read_row.

    DATA lv_kind   TYPE string.
    DATA lv_name   TYPE string.
    DATA lv_value  TYPE string.
    DATA lt_attr   TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.
    DATA lv_text   TYPE string.
    DATA lv_take   TYPE abap_bool.
    DATA lv_ruby   TYPE abap_bool.
    DATA lv_col    TYPE i.
    DATA lv_type   TYPE string.
    DATA lv_style  TYPE string.
    DATA lv_ref    TYPE string.
    DATA ls_cell   TYPE ty_s_cell.

    CLEAR t_cell.
    CLEAR eof.

    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            value       = lv_value
                                                            t_attribute = lt_attr ).
      CASE lv_kind.

        WHEN zabaputil_cl_util_context=>cs_xml_node-open.
          CASE lv_name.
            WHEN `row`.
              " r is optional: without it the row follows the one before
              lv_ref = attr( t_attribute = lt_attr
                             name        = `r` ).
              IF lv_ref IS NOT INITIAL AND lv_ref CO cv_digits.
                row = lv_ref.
              ELSE.
                row = row + 1.
              ENDIF.
              lv_col = 0.
            WHEN `c`.
              lv_ref = attr( t_attribute = lt_attr
                             name        = `r` ).
              IF lv_ref IS NOT INITIAL.
                lv_col = column_to_number( lv_ref ).
              ELSE.
                lv_col = lv_col + 1.
              ENDIF.
              lv_type = attr( t_attribute = lt_attr
                              name        = `t` ).
              lv_style = attr( t_attribute = lt_attr
                               name        = `s` ).
              CLEAR lv_text.
            WHEN `v`.
              lv_take = abap_true.
            WHEN `t`.
              " the text of an inline string, in one piece or in runs
              lv_take = xsdbool( lv_ruby = abap_false ).
            WHEN `rPh`.
              lv_ruby = abap_true.
          ENDCASE.

        WHEN zabaputil_cl_util_context=>cs_xml_node-value.
          IF lv_take = abap_true.
            lv_text = lv_text && lv_value.
          ENDIF.

        WHEN zabaputil_cl_util_context=>cs_xml_node-close.
          CASE lv_name.
            WHEN `v` OR `t`.
              lv_take = abap_false.
            WHEN `rPh`.
              lv_ruby = abap_false.
            WHEN `c`.
              CLEAR ls_cell.
              ls_cell-row = row.
              ls_cell-col = lv_col.
              cell_read( EXPORTING cell_type = lv_type
                                   style     = lv_style
                                   text      = lv_text
                         IMPORTING kind      = ls_cell-kind
                                   value     = ls_cell-value ).
              " a FALSE is a value: its cell is there, and empty
              IF ls_cell-value IS NOT INITIAL OR ls_cell-kind = cs_kind-boolean.
                INSERT ls_cell INTO TABLE t_cell.
              ENDIF.
            WHEN `row`.
              IF t_cell IS NOT INITIAL.
                RETURN.
              ENDIF.
            WHEN `sheetData`.
              eof = abap_true.
              RETURN.
          ENDCASE.

        WHEN OTHERS.
          eof = abap_true.
          RETURN.

      ENDCASE.
    ENDDO.

  ENDMETHOD.

  METHOD cell_read.

    CLEAR kind.
    CLEAR value.

    CASE cell_type.
      WHEN `s`.
        " the index of the text in the shared strings, from 0
        kind = cs_kind-text.
        IF text IS NOT INITIAL AND text CO cv_digits.
          DATA(lv_index) = CONV i( text ) + 1.
          DATA(lv_texts) = lines( mt_shared ).
          IF lv_index <= lv_texts.
            value = mt_shared[ lv_index ].
          ENDIF.
        ENDIF.
      WHEN `inlineStr` OR `str`.
        kind = cs_kind-text.
        value = text_decode( text ).
      WHEN `b`.
        kind = cs_kind-boolean.
        IF text = `1`.
          value = `X`.
        ENDIF.
      WHEN `e`.
        kind = cs_kind-error.
        value = text.
      WHEN `d`.
        " a date in ISO 8601, as a strict workbook writes it
        value = text.
        IF strlen( text ) > 10.
          kind = cs_kind-timestamp.
          IF strlen( text ) > 19.
            value = substring( val = text
                               len = 19 ).
          ENDIF.
        ELSE.
          kind = cs_kind-date.
        ENDIF.
      WHEN OTHERS.
        IF text IS NOT INITIAL.
          number_cell( EXPORTING val   = text
                                 style = style
                       IMPORTING kind  = kind
                                 value = value ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD number_cell.

    DATA lv_fraction TYPE decfloat34.
    DATA lv_seconds  TYPE i.
    DATA lv_days     TYPE i.

    CLEAR kind.
    CLEAR value.

    DATA(lv_number) = number_plain( val ).
    IF lv_number IS INITIAL.
      " no number after all - handed on as it stands
      kind = cs_kind-text.
      value = val.
      RETURN.
    ENDIF.
    kind = cs_kind-number.
    value = lv_number.

    " the format the cell is shown in says whether the number is a date
    DATA(lv_format) = ``.
    IF style IS NOT INITIAL AND style CO cv_digits.
      DATA(lv_index) = CONV i( style ) + 1.
      DATA(lv_styles) = lines( mt_style_kind ).
      IF lv_index <= lv_styles.
        lv_format = mt_style_kind[ lv_index ].
      ENDIF.
    ENDIF.
    IF lv_format IS INITIAL OR lv_number(1) = `-`.
      RETURN.
    ENDIF.

    " whole days and the fraction of a day, to the second
    DATA(lv_whole) = lv_number.
    DATA(lv_part) = `0`.
    IF lv_number CS `.`.
      lv_whole = substring_before( val = lv_number
                                   sub = `.` ).
      lv_part = substring_after( val = lv_number
                                 sub = `.` ).
    ENDIF.
    IF strlen( lv_whole ) > 7.
      RETURN.
    ENDIF.
    lv_days = lv_whole.
    lv_fraction = |0.{ lv_part }|.
    lv_seconds = round( val = lv_fraction * 86400
                        dec = 0 ).
    IF lv_seconds >= 86400.
      lv_seconds = lv_seconds - 86400.
      lv_days = lv_days + 1.
    ENDIF.
    DATA(lv_time) = |{ two_digits( lv_seconds DIV 3600 ) }:{ two_digits( lv_seconds MOD 3600 DIV 60 ) }|
                 && |:{ two_digits( lv_seconds MOD 60 ) }|.

    IF lv_format = `L`.
      " a duration: the hours are not wrapped at 24
      kind = cs_kind-time.
      IF lv_days > 0.
        kind = cs_kind-text.
        lv_time = |{ lv_days * 24 + lv_seconds DIV 3600 }{ substring( val = lv_time
                                                                      off = 2 ) }|.
      ENDIF.
      value = lv_time.
      RETURN.
    ENDIF.
    IF lv_format = cs_kind-time.
      kind = cs_kind-time.
      value = lv_time.
      RETURN.
    ENDIF.

    DATA(lv_date) = serial_to_date( val      = lv_days
                                    date1904 = mv_date1904 ).
    IF lv_date IS INITIAL.
      " day 0 or beyond the year 9999 - no date, the number stays
      RETURN.
    ENDIF.
    value = |{ lv_date(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|.
    IF lv_format = cs_kind-date.
      kind = cs_kind-date.
    ELSE.
      kind = cs_kind-timestamp.
      value = |{ value }T{ lv_time }|.
    ENDIF.

  ENDMETHOD.

  METHOD serial_to_date.

    IF val > 2958465.
      RETURN.
    ENDIF.
    IF date1904 = abap_true.
      result = cv_date_1904 + val.
    ELSEIF val >= 61.
      result = cv_date_base + val.
    ELSEIF val >= 1.
      " up to 1900-02-28 Excel is one day ahead, and its 1900-02-29 (60)
      " is the first of March here
      result = cv_date_base + val + 1.
    ENDIF.

  ENDMETHOD.

  METHOD number_plain.

    DATA lv_exp   TYPE i.
    DATA lv_point TYPE i.
    DATA lv_round TYPE p LENGTH 16 DECIMALS 0.

    DATA(lv_text) = to_upper( condense( val ) ).
    DATA(lv_sign) = ``.
    IF lv_text IS INITIAL.
      RETURN.
    ENDIF.
    CASE lv_text(1).
      WHEN `-`.
        lv_sign = `-`.
        lv_text = substring( val = lv_text
                             off = 1 ).
      WHEN `+`.
        lv_text = substring( val = lv_text
                             off = 1 ).
    ENDCASE.

    " mantissa and exponent
    IF lv_text CS `E`.
      DATA(lv_exp_text) = substring_after( val = lv_text
                                           sub = `E` ).
      lv_text = substring_before( val = lv_text
                                  sub = `E` ).
      DATA(lv_exp_sign) = 1.
      IF lv_exp_text IS NOT INITIAL AND lv_exp_text(1) = `-`.
        lv_exp_sign = -1.
        lv_exp_text = substring( val = lv_exp_text
                                 off = 1 ).
      ELSEIF lv_exp_text IS NOT INITIAL AND lv_exp_text(1) = `+`.
        lv_exp_text = substring( val = lv_exp_text
                                 off = 1 ).
      ENDIF.
      IF lv_exp_text IS INITIAL OR lv_exp_text CN cv_digits OR strlen( lv_exp_text ) > 4.
        RETURN.
      ENDIF.
      lv_exp = lv_exp_text.
      lv_exp = lv_exp * lv_exp_sign.
    ENDIF.

    " the digits, and where the point stands in them
    DATA(lv_whole) = lv_text.
    DATA(lv_part) = ``.
    IF lv_text CS `.`.
      lv_whole = substring_before( val = lv_text
                                   sub = `.` ).
      lv_part = substring_after( val = lv_text
                                 sub = `.` ).
    ENDIF.
    DATA(lv_digits) = lv_whole && lv_part.
    IF lv_digits IS INITIAL OR lv_digits CN cv_digits.
      RETURN.
    ENDIF.
    lv_point = strlen( lv_whole ) + lv_exp.

    " without the zeros in front
    DATA(lv_zeros) = 0.
    DATA(lv_len) = strlen( lv_digits ).
    WHILE lv_zeros < lv_len AND lv_digits+lv_zeros(1) = `0`.
      lv_zeros = lv_zeros + 1.
    ENDWHILE.
    IF lv_zeros = lv_len.
      result = `0`.
      RETURN.
    ENDIF.
    lv_digits = substring( val = lv_digits
                           off = lv_zeros ).
    lv_point = lv_point - lv_zeros.

    " 15 digits are what a cell shows - the 16th and 17th are the noise
    " of the binary number behind it (0.30000000000000004)
    IF strlen( lv_digits ) > digits.
      DATA(lv_next) = lv_digits+digits(1).
      lv_digits = lv_digits(digits).
      IF lv_next >= `5`.
        lv_round = lv_digits.
        lv_round = lv_round + 1.
        lv_digits = |{ lv_round }|.
        IF strlen( lv_digits ) > digits.
          lv_digits = lv_digits(digits).
          lv_point = lv_point + 1.
        ENDIF.
      ENDIF.
    ENDIF.

    " without the zeros behind
    lv_len = strlen( lv_digits ).
    WHILE lv_len > 1 AND substring( val = lv_digits
                                    off = lv_len - 1
                                    len = 1 ) = `0`.
      lv_len = lv_len - 1.
    ENDWHILE.
    lv_digits = lv_digits(lv_len).

    IF lv_point <= 0.
      result = |0.{ repeat( val = `0`
                            occ = 0 - lv_point ) }{ lv_digits }|.
    ELSEIF lv_point >= lv_len.
      result = |{ lv_digits }{ repeat( val = `0`
                                       occ = lv_point - lv_len ) }|.
    ELSE.
      result = |{ lv_digits(lv_point) }.{ lv_digits+lv_point }|.
    ENDIF.
    result = lv_sign && result.

  ENDMETHOD.

  METHOD get_cells.

    DATA lt_row TYPE ty_t_cell.
    DATA lv_eof TYPE abap_bool.
    DATA lv_row TYPE i.

    DATA(lo_reader) = zabaputil_cl_util_context=>xml_reader_create( sheet_part( sheet = sheet
                                                                                index = index ) ).
    DO.
      sheet_read_row( EXPORTING reader = lo_reader
                      IMPORTING t_cell = lt_row
                                eof    = lv_eof
                      CHANGING  row    = lv_row ).
      IF lv_eof = abap_true.
        EXIT.
      ENDIF.
      INSERT LINES OF lt_row INTO TABLE result.
    ENDDO.

  ENDMETHOD.

  METHOD get_table.

    TYPES:
      BEGIN OF ty_s_map,
        name TYPE string,
        cat  TYPE string,
      END OF ty_s_map.
    DATA lt_map     TYPE STANDARD TABLE OF ty_s_map WITH EMPTY KEY.
    DATA lr_line    TYPE REF TO data.
    DATA lt_field   TYPE ty_t_field.
    DATA lt_row     TYPE ty_t_cell.
    DATA lv_eof     TYPE abap_bool.
    DATA lv_row     TYPE i.
    DATA lv_mapped  TYPE abap_bool.
    DATA lv_any     TYPE abap_bool.
    DATA lv_elementary TYPE abap_bool.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <cell>  TYPE ty_s_cell.
    FIELD-SYMBOLS <map>   TYPE ty_s_map.

    CLEAR result.
    CREATE DATA lr_line LIKE LINE OF result.
    ASSIGN lr_line->* TO <line>.
    fields_get( EXPORTING row        = <line>
                IMPORTING elementary = lv_elementary
                          result     = lt_field ).

    IF header_row <= 0.
      " no header: the n-th column is the n-th component
      LOOP AT lt_field REFERENCE INTO DATA(lr_field).
        INSERT VALUE #( name = lr_field->name
                        cat  = lr_field->cat ) INTO TABLE lt_map.
      ENDLOOP.
      lv_mapped = abap_true.
    ENDIF.

    DATA(lo_reader) = zabaputil_cl_util_context=>xml_reader_create( sheet_part( sheet = sheet
                                                                                index = index ) ).
    DO.
      sheet_read_row( EXPORTING reader = lo_reader
                      IMPORTING t_cell = lt_row
                                eof    = lv_eof
                      CHANGING  row    = lv_row ).
      IF lv_eof = abap_true.
        EXIT.
      ENDIF.
      IF lv_row < header_row.
        CONTINUE.
      ENDIF.

      IF lv_mapped = abap_false.
        lv_mapped = abap_true.
        IF lv_row > header_row.
          " the header row is empty - no column has a component
          EXIT.
        ENDIF.
        LOOP AT lt_row ASSIGNING <cell>.
          DATA(lv_header) = zabaputil_cl_util_context=>c_trim_upper( <cell>-value ).
          DATA(lv_name) = lv_header.
          " the header the caller gives for a component goes first
          LOOP AT t_column REFERENCE INTO DATA(lr_column).
            IF lr_column->header IS NOT INITIAL
                AND zabaputil_cl_util_context=>c_trim_upper( lr_column->header ) = lv_header.
              lv_name = zabaputil_cl_util_context=>c_trim_upper( lr_column->name ).
              EXIT.
            ENDIF.
          ENDLOOP.
          DATA(ls_field) = field_find( t_field = lt_field
                                       name    = lv_name ).
          IF ls_field IS INITIAL.
            " `Order Id` for ORDER_ID
            ls_field = field_find( t_field = lt_field
                                   name    = name_by_header( lv_header ) ).
          ENDIF.
          IF ls_field IS INITIAL.
            CONTINUE.
          ENDIF.
          WHILE lines( lt_map ) < <cell>-col.
            INSERT VALUE #( ) INTO TABLE lt_map.
          ENDWHILE.
          ASSIGN lt_map[ <cell>-col ] TO <map>.
          <map>-name = ls_field-name.
          <map>-cat = ls_field-cat.
        ENDLOOP.
        CONTINUE.
      ENDIF.

      CLEAR <line>.
      lv_any = abap_false.
      DATA(lv_columns) = lines( lt_map ).
      LOOP AT lt_row ASSIGNING <cell>.
        IF <cell>-col > lv_columns.
          CONTINUE.
        ENDIF.
        ASSIGN lt_map[ <cell>-col ] TO <map>.
        IF <map>-name IS INITIAL.
          CONTINUE.
        ENDIF.
        IF lv_elementary = abap_true.
          ASSIGN <line> TO <field>.
        ELSE.
          ASSIGN COMPONENT <map>-name OF STRUCTURE <line> TO <field>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        cell_to_field( EXPORTING cell  = <cell>
                                 cat   = <map>-cat
                       CHANGING  field = <field> ).
        lv_any = abap_true.
      ENDLOOP.
      IF lv_any = abap_true.
        INSERT <line> INTO TABLE result.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD cell_to_field.

    DATA lv_date    TYPE d.
    DATA lv_time    TYPE t.
    DATA lv_number  TYPE string.
    DATA lv_serial  TYPE i.
    DATA lv_seconds TYPE i.
    DATA lv_fraction TYPE decfloat34.

    DATA(lv_value) = cell-value.
    DATA(lv_address) = |{ column_to_letters( cell-col ) }{ cell-row }|.

    IF cell-kind = cs_kind-error.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_CELL_ERROR - cell { lv_address } holds the error { lv_value }|.
    ENDIF.

    CASE cat.

      WHEN cs_cat-text.
        field = lv_value.

      WHEN cs_cat-numc.
        " a NUMC takes the digits and fills up with zeros in front
        field = digits_only( lv_value ).

      WHEN cs_cat-hex.
        TRY.
            field = lv_value.
          CATCH cx_root.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |XLSX_CELL_TYPE - cell { lv_address }: { lv_value } is no hex value|.
        ENDTRY.

      WHEN cs_cat-boolean.
        DATA(lv_flag) = to_upper( condense( lv_value ) ).
        field = xsdbool( lv_flag = `X` OR lv_flag = `TRUE` OR lv_flag = `1` ).

      WHEN cs_cat-date.
        CASE cell-kind.
          WHEN cs_kind-date OR cs_kind-timestamp.
            " YYYY-MM-DD in front, whatever follows
            lv_number = digits_only( lv_value ).
            IF strlen( lv_number ) > 8.
              lv_number = lv_number(8).
            ENDIF.
          WHEN cs_kind-number.
            " a number shown as a number: YYYYMMDD, or the day Excel counts
            lv_number = lv_value.
            IF lv_number CS `.`.
              lv_number = substring_before( val = lv_number
                                            sub = `.` ).
            ENDIF.
            IF lv_number CO cv_digits AND strlen( lv_number ) < 8.
              lv_serial = lv_number.
              lv_date = serial_to_date( val      = lv_serial
                                        date1904 = mv_date1904 ).
              lv_number = lv_date.
            ENDIF.
          WHEN OTHERS.
            " a text: 2024-03-15, 20240315, 15.03.2024
            lv_number = condense( lv_value ).
            IF strlen( lv_number ) = 10 AND lv_number+2(1) = `.` AND lv_number+5(1) = `.`.
              lv_number = lv_number+6(4) && lv_number+3(2) && lv_number(2).
            ELSE.
              IF strlen( lv_number ) > 10.
                lv_number = lv_number(10).
              ENDIF.
              lv_number = digits_only( lv_number ).
            ENDIF.
        ENDCASE.
        IF strlen( lv_number ) <> 8 OR lv_number CN cv_digits.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_CELL_TYPE - cell { lv_address }: { lv_value } is no date|.
        ENDIF.
        field = lv_number.

      WHEN cs_cat-time.
        CASE cell-kind.
          WHEN cs_kind-timestamp.
            " the HH:MM:SS behind the date
            lv_number = digits_only( lv_value ).
            IF strlen( lv_number ) >= 14.
              lv_number = lv_number+8(6).
            ELSE.
              CLEAR lv_number.
            ENDIF.
          WHEN cs_kind-number.
            " the fraction of a day
            lv_fraction = lv_value.
            lv_fraction = frac( lv_fraction ).
            lv_seconds = round( val = lv_fraction * 86400
                                dec = 0 ).
            IF lv_seconds >= 86400.
              lv_seconds = 0.
            ENDIF.
            lv_number = |{ two_digits( lv_seconds DIV 3600 ) }{ two_digits( lv_seconds MOD 3600 DIV 60 ) }|
                     && |{ two_digits( lv_seconds MOD 60 ) }|.
          WHEN OTHERS.
            " 10:30:00, 10:30, 103000
            lv_number = digits_only( lv_value ).
            IF strlen( lv_number ) = 4.
              lv_number = lv_number && `00`.
            ENDIF.
        ENDCASE.
        IF strlen( lv_number ) <> 6 OR lv_number CN cv_digits.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_CELL_TYPE - cell { lv_address }: { lv_value } is no time|.
        ENDIF.
        lv_time = lv_number.
        field = lv_time.

      WHEN OTHERS.
        " a number, and a timestamp as the number it is in ABAP
        CASE cell-kind.
          WHEN cs_kind-boolean.
            lv_number = `1`.
          WHEN cs_kind-date OR cs_kind-time OR cs_kind-timestamp.
            lv_number = digits_only( lv_value ).
          WHEN OTHERS.
            lv_number = number_plain( lv_value ).
            IF lv_number IS INITIAL AND lv_value CA cv_digits AND lv_value CO `0123456789.,-+ `.
              " a text that reads as a number in another notation
              " (1.234,56) is one; anything else is not, and the parser
              " would hand back a 0 for it
              TRY.
                  DATA(lv_parsed) = zabaputil_cl_util_context=>conv_string_to_number( lv_value ).
                  lv_number = number_plain( |{ lv_parsed }| ).
                CATCH cx_root.
                  CLEAR lv_number.
              ENDTRY.
            ENDIF.
        ENDCASE.
        IF lv_number IS INITIAL.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_CELL_TYPE - cell { lv_address }: { lv_value } is no number|.
        ENDIF.
        TRY.
            field = lv_number.
          CATCH cx_root INTO DATA(x).
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val      = |XLSX_CELL_TYPE - cell { lv_address }: { lv_value } does not fit the component|
                previous = x.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.

  METHOD get_table_ref.

    DATA lt_name TYPE string_table.
    DATA lv_cols TYPE i.
    DATA lr_line TYPE REF TO data.
    DATA lv_row  TYPE i.
    FIELD-SYMBOLS <tab>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <cell>  TYPE ty_s_cell.
    FIELD-SYMBOLS <name>  TYPE string.

    DATA(lt_cell) = get_cells( sheet = sheet
                               index = index ).
    DELETE lt_cell WHERE row < header_row.
    LOOP AT lt_cell ASSIGNING <cell>.
      IF <cell>-col > lv_cols.
        lv_cols = <cell>-col.
      ENDIF.
    ENDLOOP.
    IF lv_cols = 0.
      RETURN.
    ENDIF.

    " a component per column: after the header where there is one, after
    " the letters of the column else - and every name once
    DO lv_cols TIMES.
      INSERT `` INTO TABLE lt_name.
    ENDDO.
    IF header_row > 0.
      LOOP AT lt_cell ASSIGNING <cell> WHERE row = header_row. "#EC CI_SORTSEQ
        ASSIGN lt_name[ <cell>-col ] TO <name>.
        <name> = name_by_header( <cell>-value ).
      ENDLOOP.
    ENDIF.
    LOOP AT lt_name ASSIGNING <name>.
      DATA(lv_tabix) = sy-tabix.
      IF <name> IS INITIAL.
        <name> = column_to_letters( lv_tabix ).
      ENDIF.
      DATA(lv_base) = <name>.
      DATA(lv_suffix) = 1.
      DO.
        " the first column of that name - this one, when no earlier has it
        DATA(lv_first) = 0.
        LOOP AT lt_name INTO DATA(lv_other).
          IF lv_other = <name>.
            lv_first = sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF lv_first = lv_tabix.
          EXIT.
        ENDIF.
        lv_suffix = lv_suffix + 1.
        DATA(lv_tail) = |_{ lv_suffix }|.
        <name> = substring( val = lv_base
                            len = nmin( val1 = strlen( lv_base )
                                        val2 = 30 - strlen( lv_tail ) ) ) && lv_tail.
      ENDDO.
    ENDLOOP.

    result = zabaputil_cl_util_context=>rtti_create_tab_by_names( lt_name ).
    ASSIGN result->* TO <tab>.
    CREATE DATA lr_line LIKE LINE OF <tab>.
    ASSIGN lr_line->* TO <line>.

    LOOP AT lt_cell ASSIGNING <cell> WHERE row > header_row. "#EC CI_SORTSEQ
      IF <cell>-row <> lv_row.
        IF lv_row > 0.
          INSERT <line> INTO TABLE <tab>.
        ENDIF.
        CLEAR <line>.
        lv_row = <cell>-row.
      ENDIF.
      ASSIGN COMPONENT <cell>-col OF STRUCTURE <line> TO <field>.
      IF sy-subrc = 0.
        <field> = <cell>-value.
      ENDIF.
    ENDLOOP.
    IF lv_row > 0.
      INSERT <line> INTO TABLE <tab>.
    ENDIF.

  ENDMETHOD.

  METHOD name_by_header.

    DATA lv_pos TYPE i.

    DATA(lv_name) = zabaputil_cl_util_context=>c_trim_upper( val ).
    DATA(lv_len) = strlen( lv_name ).
    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = lv_name
                                 off = lv_pos
                                 len = 1 ).
      IF lv_char CA `ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_`.
        result = result && lv_char.
      ELSE.
        result = result && `_`.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    IF result IS INITIAL.
      RETURN.
    ENDIF.
    IF result(1) CA cv_digits.
      result = `C_` && result.
    ENDIF.
    IF strlen( result ) > 30.
      result = result(30).
    ENDIF.

  ENDMETHOD.

  METHOD digits_only.

    DATA lv_pos TYPE i.

    DATA(lv_len) = strlen( val ).
    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = val
                                 off = lv_pos
                                 len = 1 ).
      IF lv_char CA cv_digits.
        result = result && lv_char.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD two_digits.

    result = |{ val }|.
    IF strlen( result ) < 2.
      result = `0` && result.
    ENDIF.

  ENDMETHOD.

  METHOD attr.

    LOOP AT t_attribute REFERENCE INTO DATA(lr_attr).
      IF lr_attr->name = name.
        result = lr_attr->value.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD path_folder.

    DATA(lv_pos) = find( val = val
                         sub = `/`
                         occ = -1 ).
    IF lv_pos >= 0.
      result = substring( val = val
                          len = lv_pos + 1 ).
    ENDIF.

  ENDMETHOD.

  METHOD path_resolve.

    DATA lt_done TYPE string_table.

    " from the root of the package when it starts with a slash, from the
    " folder of the part that names it else
    DATA(lv_path) = target.
    IF lv_path IS NOT INITIAL AND lv_path(1) = `/`.
      lv_path = substring( val = lv_path
                           off = 1 ).
    ELSE.
      lv_path = path_folder( base ) && lv_path.
    ENDIF.

    SPLIT lv_path AT `/` INTO TABLE DATA(lt_step).
    LOOP AT lt_step INTO DATA(lv_step).
      IF lv_step = `..`.
        IF lt_done IS NOT INITIAL.
          DELETE lt_done INDEX lines( lt_done ).
        ENDIF.
      ELSEIF lv_step <> `.` AND lv_step IS NOT INITIAL.
        INSERT lv_step INTO TABLE lt_done.
      ENDIF.
    ENDLOOP.
    result = concat_lines_of( table = lt_done
                              sep   = `/` ).

  ENDMETHOD.

  METHOD text_decode.

    DATA lv_off TYPE i.

    result = val.
    IF result NS `_x`.
      RETURN.
    ENDIF.

    CLEAR result.
    DATA(lv_len) = strlen( val ).
    DO.
      DATA(lv_pos) = find( val = val
                           sub = `_x`
                           off = lv_off ).
      IF lv_pos < 0 OR lv_pos + 7 > lv_len.
        EXIT.
      ENDIF.
      DATA(lv_code) = to_upper( substring( val = val
                                           off = lv_pos + 2
                                           len = 4 ) ).
      IF lv_code CO cv_hex AND substring( val = val
                                          off = lv_pos + 6
                                          len = 1 ) = `_`.
        result = result && substring( val = val
                                      off = lv_off
                                      len = lv_pos - lv_off ) && char_by_code( hex_to_int( lv_code ) ).
        lv_off = lv_pos + 7.
      ELSE.
        result = result && substring( val = val
                                      off = lv_off
                                      len = lv_pos + 2 - lv_off ).
        lv_off = lv_pos + 2.
      ENDIF.
    ENDDO.
    result = result && substring( val = val
                                  off = lv_off ).

  ENDMETHOD.

  METHOD hex_to_int.

    DATA lv_pos TYPE i.

    " digits only is a decimal number as well: an id of styles.xml
    DATA(lv_text) = to_upper( val ).
    DATA(lv_len) = strlen( lv_text ).
    DATA(lv_base) = 16.
    IF lv_text CO cv_digits AND lv_len <> 4.
      lv_base = 10.
    ENDIF.
    WHILE lv_pos < lv_len.
      DATA(lv_digit) = find( val = cv_hex
                             sub = substring( val = lv_text
                                              off = lv_pos
                                              len = 1 ) ).
      IF lv_digit < 0.
        result = 0.
        RETURN.
      ENDIF.
      result = result * lv_base + lv_digit.
      lv_pos = lv_pos + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD byte_to_hex.

    result = substring( val = cv_hex
                        off = val DIV 16
                        len = 1 ) && substring( val = cv_hex
                                                off = val MOD 16
                                                len = 1 ).

  ENDMETHOD.

  METHOD char_by_code.

    DATA lv_hex TYPE string.

    CASE val.
      WHEN 9.
        result = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
      WHEN 10.
        result = zabaputil_cl_util_context=>cv_char_util_newline.
      WHEN 13.
        result = zabaputil_cl_util_context=>cv_char_util_cr_lf(1).
      WHEN OTHERS.
        IF val < 32.
          " a control character no text has a use for
          RETURN.
        ENDIF.
        " the character as its UTF-8 bytes, which the context turns into
        " a string on every release
        IF val < 128.
          lv_hex = byte_to_hex( val ).
        ELSEIF val < 2048.
          lv_hex = byte_to_hex( 192 + val DIV 64 ) && byte_to_hex( 128 + val MOD 64 ).
        ELSE.
          lv_hex = byte_to_hex( 224 + val DIV 4096 ) && byte_to_hex( 128 + val DIV 64 MOD 64 )
                && byte_to_hex( 128 + val MOD 64 ).
        ENDIF.
        TRY.
            result = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( lv_hex ) ).
          CATCH cx_root ##NO_HANDLER.
            " no converter - the text without that character
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

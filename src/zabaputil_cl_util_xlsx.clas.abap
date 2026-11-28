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
"! A read can be restricted to a range, to the first rows, to what is not
"! hidden, or go by an Excel table or a defined name (table_name); a sheet
"! too large for a table is read row by row (rows_open( ), rows_next( )).
"! get_table( ) with t_error lists the cells that do not fit instead of
"! raising at the first. A cell gives its formula besides its value; the
"! merged ranges, the links, the names and the Excel tables of a workbook
"! are read on their own (get_merged( ), get_hyperlinks( ), ...).
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
"! A sheet also takes rules of conditional formatting, comments, pictures
"! and protection; factory( abap_true ) writes the texts as shared strings.
"!
"! Not in it: charts, pivot tables, an .xls file of Excel 97-2003, a
"! workbook encrypted with a password, changing a workbook that was read
"! (a template).
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
        "! the cell can be changed on a protected sheet
        unlocked      TYPE abap_bool,
      END OF ty_s_style.

    TYPES:
      "! a rule of conditional formatting - how a cell looks by its value
      BEGIN OF ty_s_rule,
        "! cellIs (by operator and formula1/2), containsText (text),
        "! expression (formula1, true for the first cell of the range),
        "! top10 (rank), duplicateValues, uniqueValues - shown in style;
        "! colorScale (color_min, color_mid, color_max), dataBar (color_max),
        "! iconSet (icon_set)
        type      TYPE string,
        "! of cellIs: between, notBetween, equal, notEqual, greaterThan,
        "! lessThan, greaterThanOrEqual, lessThanOrEqual
        operator  TYPE string,
        formula1  TYPE string,
        formula2  TYPE string,
        text      TYPE string,
        "! of top10: the number of cells (or percent), from the bottom
        rank      TYPE i,
        percent   TYPE abap_bool,
        bottom    TYPE abap_bool,
        "! the font, the fill and the border of the cells the rule takes
        style     TYPE ty_s_style,
        "! the colours of a colour scale - without mid a scale of two
        color_min TYPE string,
        color_mid TYPE string,
        color_max TYPE string,
        "! 3Arrows, 3TrafficLights1, 3Symbols, 4Rating, 5Quarters, ...
        icon_set  TYPE string,
        "! no rule after this one when it takes the cell
        stop      TYPE abap_bool,
      END OF ty_s_rule.

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
        "! how a cell of the column looks by its value
        rule    TYPE ty_s_rule,
        "! a conversion exit: written in its external form (ALPHA: without the
        "! leading zeros), read in its internal one
        conv_exit TYPE string,
        "! the text of a fixed value of the domain instead of the value
        fixed_value_text TYPE abap_bool,
        "! the component that holds the currency key of an amount: the
        "! cell shows the decimals of the currency (JPY 0, EUR 2, KWD 3),
        "! and the amount is written as WRITE ... CURRENCY shows it
        currency TYPE string,
        "! the component that holds the unit of a quantity: the cell shows
        "! the decimals of the unit
        unit     TYPE string,
      END OF ty_s_column.
    TYPES ty_t_column TYPE STANDARD TABLE OF ty_s_column WITH DEFAULT KEY.

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
        "! the sheet is protected: a cell is changed only where its style
        "! says unlocked
        protect        TYPE abap_bool,
        "! the password that lifts the protection - at most 15 characters.
        "! It keeps a user from changing the sheet by mistake, not a reader
        "! from the data: Excel's legacy hash is no encryption
        password       TYPE string,
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
        "! the formula of the cell, as the file holds it (without =) - a
        "! shared formula worked out for the cell
        formula TYPE string,
      END OF ty_s_cell.
    TYPES ty_t_cell TYPE STANDARD TABLE OF ty_s_cell WITH DEFAULT KEY.

    TYPES:
      "! a cell get_table( ) could not convert
      BEGIN OF ty_s_error,
        sheet     TYPE string,
        "! the address of the cell (`C12`)
        cell      TYPE string,
        row       TYPE i,
        col       TYPE i,
        value     TYPE string,
        "! the component the value was for
        component TYPE string,
        message   TYPE string,
      END OF ty_s_error.
    TYPES ty_t_error TYPE STANDARD TABLE OF ty_s_error WITH DEFAULT KEY.

    TYPES:
      "! a link from a cell - to a URL, or to a place in the workbook
      "! (`#Sheet2!A1`)
      BEGIN OF ty_s_hyperlink,
        cell    TYPE string,
        target  TYPE string,
        display TYPE string,
      END OF ty_s_hyperlink.
    TYPES ty_t_hyperlink TYPE STANDARD TABLE OF ty_s_hyperlink WITH DEFAULT KEY.

    TYPES:
      "! a defined name of a workbook that was read
      BEGIN OF ty_s_defined_name,
        name  TYPE string,
        "! the sheet the name holds on - empty: the whole workbook
        sheet TYPE string,
        "! what it stands for, as Excel writes it (`'Data'!$A$1:$C$9`)
        ref   TYPE string,
      END OF ty_s_defined_name.
    TYPES ty_t_defined_name TYPE STANDARD TABLE OF ty_s_defined_name WITH DEFAULT KEY.

    TYPES:
      "! an Excel table of a workbook that was read
      BEGIN OF ty_s_excel_table,
        name        TYPE string,
        sheet       TYPE string,
        "! the cells of the table, header and totals row included (`A1:D20`)
        ref         TYPE string,
        header_rows TYPE i,
        totals_rows TYPE i,
      END OF ty_s_excel_table.
    TYPES ty_t_excel_table TYPE STANDARD TABLE OF ty_s_excel_table WITH DEFAULT KEY.

    TYPES:
      "! how a cell of a workbook that was read is shown - a cell with a
      "! format of its own. A colour is RGB in six hex digits; one of the
      "! theme of the workbook is left empty
      BEGIN OF ty_s_cell_format,
        row       TYPE i,
        col       TYPE i,
        "! the number format (`#,##0.00`, `dd.mm.yyyy`, `General`)
        format    TYPE string,
        bold      TYPE abap_bool,
        italic    TYPE abap_bool,
        underline TYPE abap_bool,
        color     TYPE string,
        fill      TYPE string,
        halign    TYPE string,
        wrap      TYPE abap_bool,
      END OF ty_s_cell_format.
    TYPES ty_t_cell_format TYPE STANDARD TABLE OF ty_s_cell_format WITH DEFAULT KEY.

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
    "! @parameter shared_strings | every text once in a table of the workbook, not in its cell:
    "!                             a smaller file where texts repeat, what some readers expect
    CLASS-METHODS factory
      IMPORTING
        shared_strings TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)  TYPE REF TO zabaputil_cl_util_xlsx.

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

    "! A rule of conditional formatting for the cells of a range
    "! (`C2:C100`) of the sheet added last
    METHODS add_rule
      IMPORTING
        range         TYPE clike
        rule          TYPE ty_s_rule
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! A note on a cell of the sheet added last, shown when the mouse is
    "! on it
    METHODS add_comment
      IMPORTING
        row           TYPE i
        col           TYPE i
        text          TYPE clike
        author        TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! A picture on the sheet added last, its top left corner in a cell
    "! @parameter image | the bytes of a PNG or JPEG file
    "! @parameter width | in pixels - 0: as the picture is
    "! @parameter height | in pixels - 0: as the picture is, or by the width
    METHODS add_image
      IMPORTING
        row           TYPE i
        col           TYPE i
        image         TYPE xstring
        width         TYPE i OPTIONAL
        height        TYPE i OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! The sheets of the workbook can be neither added, deleted, moved nor
    "! renamed - with Excel's legacy hash of the password, no encryption
    METHODS protect_workbook
      IMPORTING
        password      TYPE clike OPTIONAL
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
    "! @parameter no_hidden | without the sheets that are hidden
    METHODS get_sheet_names
      IMPORTING
        no_hidden     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE string_table.

    "! Every cell of a sheet that holds a value, row by row.
    "! @parameter sheet | the name of the sheet - empty: the sheet at index
    "! @parameter range | only the cells of this range (`B3:F200`)
    "! @parameter max_rows | only the first rows that hold a value - 0: all
    "! @parameter skip_hidden | without the hidden rows and columns
    "! @parameter fill_merged | every cell of a merged range with the value of its first
    METHODS get_cells
      IMPORTING
        sheet         TYPE clike     OPTIONAL
        index         TYPE i         DEFAULT 1
        range         TYPE clike     OPTIONAL
        max_rows      TYPE i         OPTIONAL
        skip_hidden   TYPE abap_bool OPTIONAL
        fill_merged   TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_cell.

    "! A sheet into a table. A column goes to the component its header
    "! names - the component name, or the header t_column gives for it;
    "! upper and lower case and the blanks around make no difference, a
    "! column without a component is left out. Every value is converted to
    "! the type of its component; a cell that does not fit (a text in a
    "! number, a number in a date) raises with its address.
    "! @parameter header_row | 0: no header, the n-th column goes to the n-th component
    "! @parameter range | only the cells of this range (`B3:F200`) - the header in its first row
    "! @parameter max_rows | only the first rows - 0: all
    "! @parameter skip_hidden | without the hidden rows and columns
    "! @parameter table_name | an Excel table or a defined name instead of sheet and range
    "! @parameter date_order | of a date as text with day and month in front: DMY or MDY
    "! @parameter t_error | given: the cells that do not fit are listed here, not raised
    METHODS get_table
      IMPORTING
        sheet       TYPE clike       OPTIONAL
        index       TYPE i           DEFAULT 1
        header_row  TYPE i           DEFAULT 1
        t_column    TYPE ty_t_column OPTIONAL
        range       TYPE clike       OPTIONAL
        max_rows    TYPE i           OPTIONAL
        skip_hidden TYPE abap_bool   OPTIONAL
        table_name  TYPE clike       OPTIONAL
        date_order  TYPE clike       DEFAULT `DMY`
      EXPORTING
        result      TYPE ANY TABLE
        t_error     TYPE ty_t_error.

    "! A sheet as a table of strings whose components are named after the
    "! header row (`Order Id` becomes ORDER_ID; a column without a header,
    "! or all of them with header_row 0, after its letters A, B, ...).
    "! Values as get_cells( ) gives them. Unbound for a sheet without a cell
    METHODS get_table_ref
      IMPORTING
        sheet         TYPE clike     OPTIONAL
        index         TYPE i         DEFAULT 1
        header_row    TYPE i         DEFAULT 1
        range         TYPE clike     OPTIONAL
        max_rows      TYPE i         OPTIONAL
        skip_hidden   TYPE abap_bool OPTIONAL
        table_name    TYPE clike     OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! The rows of a sheet one at a time, for a sheet that is too large
    "! for a table: rows_open( ), then rows_next( ) until eof
    METHODS rows_open
      IMPORTING
        sheet         TYPE clike     OPTIONAL
        index         TYPE i         DEFAULT 1
        range         TYPE clike     OPTIONAL
        skip_hidden   TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xlsx.

    "! the cells of the next row that holds a value - eof when there is none
    METHODS rows_next
      EXPORTING
        t_cell TYPE ty_t_cell
        eof    TYPE abap_bool.

    "! the merged ranges of a sheet (`A1:D1`)
    METHODS get_merged
      IMPORTING
        sheet         TYPE clike OPTIONAL
        index         TYPE i     DEFAULT 1
      RETURNING
        VALUE(result) TYPE string_table.

    "! the links of the cells of a sheet
    METHODS get_hyperlinks
      IMPORTING
        sheet         TYPE clike OPTIONAL
        index         TYPE i     DEFAULT 1
      RETURNING
        VALUE(result) TYPE ty_t_hyperlink.

    "! The formats of the cells of a sheet that have one of their own - read
    "! on their own, so that a read of the values does not pay for them
    METHODS get_cell_formats
      IMPORTING
        sheet         TYPE clike OPTIONAL
        index         TYPE i     DEFAULT 1
      RETURNING
        VALUE(result) TYPE ty_t_cell_format.

    "! the defined names of the workbook
    METHODS get_defined_names
      RETURNING
        VALUE(result) TYPE ty_t_defined_name.

    "! the Excel tables of the workbook, sheet by sheet
    METHODS get_excel_tables
      RETURNING
        VALUE(result) TYPE ty_t_excel_table.

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
        decimals TYPE i,
      END OF ty_s_field.
    TYPES ty_t_field TYPE STANDARD TABLE OF ty_s_field WITH DEFAULT KEY.

    " a fixed value of a domain and its text
    TYPES:
      BEGIN OF ty_s_fix,
        low  TYPE string,
        text TYPE string,
      END OF ty_s_fix.
    TYPES ty_t_fix TYPE STANDARD TABLE OF ty_s_fix WITH DEFAULT KEY.

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
        rule        TYPE ty_s_rule,
        conv_exit   TYPE string,
        t_fix       TYPE ty_t_fix,
        " the component with the currency or the unit, the decimals of the
        " field, the format the decimals go into
        currency    TYPE string,
        unit        TYPE string,
        decimals    TYPE i,
        format      TYPE string,
        style_base  TYPE ty_s_style,
      END OF ty_s_out_col.
    TYPES ty_t_out_col TYPE STANDARD TABLE OF ty_s_out_col WITH DEFAULT KEY.

    " a part that belongs to a sheet (an Excel table)
    TYPES:
      BEGIN OF ty_s_out_part,
        id  TYPE i,
        xml TYPE xstring,
      END OF ty_s_out_part.
    TYPES ty_t_out_part TYPE STANDARD TABLE OF ty_s_out_part WITH DEFAULT KEY.

    " a part of the package that belongs to a sheet besides its tables: a
    " comments part, a drawing, a picture
    TYPES:
      BEGIN OF ty_s_out_file,
        name  TYPE string,
        ctype TYPE string,
        xml   TYPE xstring,
      END OF ty_s_out_file.
    TYPES ty_t_out_file TYPE STANDARD TABLE OF ty_s_out_file WITH DEFAULT KEY.

    " a sheet that is written: its part, its relationships and its tables
    TYPES:
      BEGIN OF ty_s_out_sheet,
        name    TYPE string,
        hidden  TYPE abap_bool,
        xml     TYPE xstring,
        rels    TYPE xstring,
        t_table TYPE ty_t_out_part,
        t_file  TYPE ty_t_out_file,
      END OF ty_s_out_sheet.

    TYPES:
      BEGIN OF ty_s_comment,
        row    TYPE i,
        col    TYPE i,
        text   TYPE string,
        author TYPE string,
      END OF ty_s_comment.
    TYPES ty_t_comment TYPE STANDARD TABLE OF ty_s_comment WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_image,
        row    TYPE i,
        col    TYPE i,
        ext    TYPE string,
        width  TYPE i,
        height TYPE i,
        data   TYPE xstring,
      END OF ty_s_image.
    TYPES ty_t_image TYPE STANDARD TABLE OF ty_s_image WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_sst,
        text  TYPE string,
        index TYPE i,
      END OF ty_s_sst.

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
    TYPES ty_t_area TYPE STANDARD TABLE OF ty_s_area WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_link,
        ref    TYPE string,
        target TYPE string,
      END OF ty_s_link.
    TYPES ty_t_link TYPE STANDARD TABLE OF ty_s_link WITH DEFAULT KEY.

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
        t_rule      TYPE string_table,
        priority    TYPE i,
        t_comment   TYPE ty_t_comment,
        t_image     TYPE ty_t_image,
      END OF ty_s_cur.

    " a defined name of the workbook - sheet 0: of the workbook
    TYPES:
      BEGIN OF ty_s_def,
        name  TYPE string,
        sheet TYPE i,
        ref   TYPE string,
      END OF ty_s_def.

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
        unlocked TYPE abap_bool,
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
        name   TYPE string,
        path   TYPE string,
        hidden TYPE abap_bool,
      END OF ty_s_sheet.

    " the master of a shared formula: where it stands, what it says
    TYPES:
      BEGIN OF ty_s_formula,
        si      TYPE string,
        formula TYPE string,
        row     TYPE i,
        col     TYPE i,
      END OF ty_s_formula.
    TYPES ty_t_formula TYPE STANDARD TABLE OF ty_s_formula WITH DEFAULT KEY.

    " a sheet while it is read row by row
    TYPES:
      BEGIN OF ty_s_read,
        reader    TYPE REF TO object,
        sheet     TYPE string,
        row       TYPE i,
        area      TYPE ty_s_area,
        skip      TYPE abap_bool,
        t_hidden  TYPE ty_t_area,
        t_formula TYPE ty_t_formula,
        eof       TYPE abap_bool,
      END OF ty_s_read.

    TYPES:
      BEGIN OF ty_s_rel,
        id     TYPE string,
        type   TYPE string,
        target TYPE string,
      END OF ty_s_rel.
    TYPES ty_t_rel TYPE STANDARD TABLE OF ty_s_rel WITH DEFAULT KEY.

    " writing
    TYPES temp1_d771c50ac2 TYPE STANDARD TABLE OF ty_s_out_sheet WITH DEFAULT KEY.
DATA mt_out_sheet TYPE temp1_d771c50ac2.
    TYPES temp2_d771c50ac2 TYPE STANDARD TABLE OF ty_s_xf WITH DEFAULT KEY.
DATA mt_xf        TYPE temp2_d771c50ac2.
    TYPES temp3_d771c50ac2 TYPE STANDARD TABLE OF ty_s_font WITH DEFAULT KEY.
DATA mt_font      TYPE temp3_d771c50ac2.
    DATA mt_fill      TYPE string_table.
    TYPES temp4_d771c50ac2 TYPE STANDARD TABLE OF ty_s_border WITH DEFAULT KEY.
DATA mt_border    TYPE temp4_d771c50ac2.
    DATA mt_numfmt    TYPE string_table.
    DATA mv_tables    TYPE i.
    DATA mt_table_name TYPE string_table.
    DATA mv_formulas  TYPE abap_bool.
    DATA mv_open      TYPE abap_bool.
    DATA ms_cur       TYPE ty_s_cur.
    TYPES temp5_d771c50ac2 TYPE STANDARD TABLE OF ty_s_def WITH DEFAULT KEY.
DATA mt_name      TYPE temp5_d771c50ac2.
    DATA mv_props     TYPE abap_bool.
    DATA ms_props     TYPE ty_s_properties.
    DATA mt_dxf       TYPE string_table.
    DATA mv_sst       TYPE abap_bool.
    DATA mt_sst       TYPE string_table.
    TYPES temp6_d771c50ac2 TYPE HASHED TABLE OF ty_s_sst WITH UNIQUE KEY text.
DATA mt_sst_index TYPE temp6_d771c50ac2.
    DATA mv_book_lock TYPE abap_bool.
    DATA mv_book_hash TYPE string.
    DATA mv_comments  TYPE i.
    DATA mv_drawings  TYPE i.
    DATA mv_images    TYPE i.
    " the decimals of the currencies and units read so far: C or U and the key
    TYPES temp7_d771c50ac2 TYPE HASHED TABLE OF ty_s_sst WITH UNIQUE KEY text.
DATA mt_decimals  TYPE temp7_d771c50ac2.

    " reading
    DATA mv_loaded     TYPE abap_bool.
    " the archive, opened: a part is unpacked when it is read
    DATA mo_zip        TYPE REF TO object.
    DATA mt_part       TYPE string_table.
    TYPES temp8_d771c50ac2 TYPE STANDARD TABLE OF ty_s_sheet WITH DEFAULT KEY.
DATA mt_sheet      TYPE temp8_d771c50ac2.
    DATA mt_shared     TYPE string_table.
    DATA mt_style_kind TYPE string_table.
    DATA mv_date1904   TYPE abap_bool.
    DATA mt_defname    TYPE ty_t_defined_name.
    DATA ms_rows       TYPE ty_s_read.

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

    "! a rule as a cfRule element, its style added to the dxfs
    METHODS rule_xml
      IMPORTING
        sqref         TYPE string
        val           TYPE ty_s_rule
      RETURNING
        VALUE(result) TYPE string.

    "! the index of a format of conditional formatting (dxf)
    METHODS dxf_get
      IMPORTING
        val           TYPE ty_s_style
      RETURNING
        VALUE(result) TYPE i.

    "! the decimals of a currency (C and its key) or a unit (U and its key),
    "! read once per workbook
    METHODS decimals_get
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE i.

    "! the legacy hash of a password Excel keeps for a protection (CC3D)
    CLASS-METHODS password_hash
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! a bitwise exclusive or of two numbers of 16 bits
    CLASS-METHODS bit_xor
      IMPORTING
        a             TYPE i
        b             TYPE i
      RETURNING
        VALUE(result) TYPE i.

    "! the comments part and the VML drawing Excel shows them with
    METHODS comments_xml
      IMPORTING
        t_comment TYPE ty_t_comment
        id        TYPE i
      EXPORTING
        comments  TYPE string
        vml       TYPE string.

    "! the drawing of the pictures of a sheet, its relationships, its media
    METHODS drawing_xml
      IMPORTING
        t_image TYPE ty_t_image
      EXPORTING
        drawing TYPE string
        rels    TYPE string
        t_media TYPE ty_t_out_file.

    "! the size of a PNG or JPEG picture in pixels, and its kind (png, jpeg)
    CLASS-METHODS image_info
      IMPORTING
        val    TYPE xstring
      EXPORTING
        ext    TYPE string
        width  TYPE i
        height TYPE i.

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

    "! the sheet by name or by position
    METHODS sheet_get
      IMPORTING
        sheet         TYPE clike
        index         TYPE i
      RETURNING
        VALUE(result) TYPE ty_s_sheet.

    "! the part of a sheet - raises when the file has none
    METHODS sheet_xml
      IMPORTING
        val           TYPE ty_s_sheet
      RETURNING
        VALUE(result) TYPE xstring.

    "! a sheet to be read with read_next( ), restricted to a range
    METHODS read_start
      IMPORTING
        sheet         TYPE ty_s_sheet
        range         TYPE clike     OPTIONAL
        skip_hidden   TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_read.

    "! the next row of the range that holds a value
    METHODS read_next
      EXPORTING
        t_cell TYPE ty_t_cell
        eof    TYPE abap_bool
      CHANGING
        state  TYPE ty_s_read.

    "! The cells of the next row that has any, and whether it is hidden.
    "! state-row comes back as the row that was read
    METHODS sheet_read_row
      EXPORTING
        t_cell TYPE ty_t_cell
        eof    TYPE abap_bool
        hidden TYPE abap_bool
      CHANGING
        state  TYPE ty_s_read.

    "! a table name or a defined name as the sheet and the range it stands for
    METHODS source_get
      IMPORTING
        name   TYPE clike
      EXPORTING
        sheet  TYPE ty_s_sheet
        area   TYPE ty_s_area
        header TYPE abap_bool.

    "! a reference with a sheet (`'Q1 2024'!$A$1:$C$9`) as the sheet and the area
    METHODS ref_split
      IMPORTING
        val   TYPE string
      EXPORTING
        sheet TYPE ty_s_sheet
        area  TYPE ty_s_area.

    "! what follows the cells of a sheet: its merged ranges and its links
    METHODS sheet_tail
      IMPORTING
        sheet   TYPE ty_s_sheet
      EXPORTING
        t_merge TYPE string_table
        t_link  TYPE ty_t_hyperlink.

    "! a formula of a cell moved by rows and columns, as a shared formula
    "! is for the cells after its first: the references without a $ move
    CLASS-METHODS formula_shift
      IMPORTING
        val           TYPE string
        rows          TYPE i
        cols          TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! the length of a cell reference at pos (`$B$12`) - 0 for none
    CLASS-METHODS ref_at
      IMPORTING
        val     TYPE string
        pos     TYPE i
      EXPORTING
        len     TYPE i
        col     TYPE i
        row     TYPE i
        abs_col TYPE abap_bool
        abs_row TYPE abap_bool.

    "! a date as text with separators (15.03.2024, 3/15/24, 2024-03-15)
    "! as YYYYMMDD - empty for none
    CLASS-METHODS date_by_text
      IMPORTING
        val           TYPE string
        date_order    TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! a number as text with what a person writes around it - a currency,
    "! a percent sign, a sign behind (1.234,56-) - as a plain number
    CLASS-METHODS number_by_text
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

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
        cell       TYPE ty_s_cell
        cat        TYPE string
        date_order TYPE string DEFAULT `DMY`
      CHANGING
        field      TYPE any.

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

    CREATE OBJECT result.
    result->mv_sst = shared_strings.

  ENDMETHOD.

  METHOD factory_by_xlsx.

    CREATE OBJECT result.
    result->load( val ).

  ENDMETHOD.

  METHOD xlsx_by_itab.

    result = factory( )->add_sheet( val    = val
                                    layout = layout )->get_xlsx( ).

  ENDMETHOD.

  METHOD itab_by_xlsx.

    DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    temp1 = factory_by_xlsx( val ).
    temp1->get_table( IMPORTING result = result ).

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
    DATA temp229 TYPE zabaputil_cl_util_xlsx=>ty_s_area.
    DATA lv_ref TYPE string.
    DATA ls_style LIKE style.
    DATA temp230 TYPE string.
    DATA lv_code LIKE temp230.
      DATA temp231 TYPE string.
      DATA lv_formula LIKE temp231.
      DATA lv_style TYPE string.
      FIELD-SYMBOLS <temp232> LIKE LINE OF lt_field.
      DATA temp233 LIKE sy-tabix.
      DATA temp234 TYPE zabaputil_cl_util_xlsx=>ty_s_link.

    sheet_check( ).
    cell_check( row = row
                col = col ).
    
    CLEAR temp229.
    temp229-row1 = row.
    temp229-col1 = col.
    temp229-row2 = row.
    temp229-col2 = col.
    seg_check( temp229 ).
    
    lv_ref = |{ column_to_letters( col ) }{ row }|.
    
    ls_style = style.
    IF link IS NOT INITIAL AND ls_style-color IS INITIAL.
      ls_style-color = `0563C1`.
      ls_style-underline = abap_true.
    ENDIF.
    
    temp230 = format.
    
    lv_code = temp230.

    IF formula IS NOT INITIAL.
      
      temp231 = formula.
      
      lv_formula = temp231.
      IF lv_formula(1) = `=`.
        lv_formula = substring( val = lv_formula
                                off = 1 ).
      ENDIF.
      lv_formula = zabaputil_cl_util_context=>c_escape_xml( lv_formula ).
      
      lv_style = style_attr( code  = lv_code
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
      
      
      temp233 = sy-tabix.
      READ TABLE lt_field INDEX 1 ASSIGNING <temp232>.
      sy-tabix = temp233.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_cat = <temp232>-cat.
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
      
      CLEAR temp234.
      temp234-ref = lv_ref.
      temp234-target = link.
      INSERT temp234 INTO TABLE ms_cur-t_link.
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
      DATA temp235 TYPE zabaputil_cl_util_xlsx=>ty_s_line.

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
    READ TABLE ms_cur-t_row WITH KEY pos = row ASSIGNING <line>.
    IF <line> IS NOT ASSIGNED.
      
      CLEAR temp235.
      temp235-pos = row.
      INSERT temp235 INTO TABLE ms_cur-t_row ASSIGNING <line>.
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
      DATA temp236 TYPE zabaputil_cl_util_xlsx=>ty_s_line.

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
    READ TABLE ms_cur-t_col WITH KEY pos = col ASSIGNING <line>.
    IF <line> IS NOT ASSIGNED.
      
      CLEAR temp236.
      temp236-pos = col.
      INSERT temp236 INTO TABLE ms_cur-t_col ASSIGNING <line>.
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
    DATA ls_area TYPE zabaputil_cl_util_xlsx=>ty_s_area.
    DATA temp237 LIKE LINE OF ms_cur-t_merge.
    DATA lr_merge LIKE REF TO temp237.

    sheet_check( ).
    
    ls_area = area_parse( range ).
    IF ls_area-row1 = ls_area-row2 AND ls_area-col1 = ls_area-col2.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_MERGE - { range } is one cell, nothing to merge|.
    ENDIF.
    
    
    LOOP AT ms_cur-t_merge REFERENCE INTO lr_merge
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
    DATA lv_ref TYPE string.

    sheet_check( ).
    
    lv_ref = area_text( area_parse( range ) ).
    INSERT validation_xml( sqref = lv_ref
                           val   = validation ) INTO TABLE ms_cur-t_valid.
    result = me.

  ENDMETHOD.

  METHOD add_name.

    DATA ls_name TYPE ty_s_def.

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

  METHOD add_rule.
    DATA lv_ref TYPE string.

    sheet_check( ).
    
    lv_ref = area_text( area_parse( range ) ).
    INSERT rule_xml( sqref = lv_ref
                     val   = rule ) INTO TABLE ms_cur-t_rule.
    result = me.

  ENDMETHOD.

  METHOD add_comment.
    DATA temp238 TYPE zabaputil_cl_util_xlsx=>ty_s_comment.

    sheet_check( ).
    cell_check( row = row
                col = col ).
    LOOP AT ms_cur-t_comment TRANSPORTING NO FIELDS WHERE row = row AND col = col.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_COMMENT - { column_to_letters( col ) }{ row } has a comment already|.
    ENDLOOP.
    
    CLEAR temp238.
    temp238-row = row.
    temp238-col = col.
    temp238-text = text.
    temp238-author = author.
    INSERT temp238 INTO TABLE ms_cur-t_comment.
    result = me.

  ENDMETHOD.

  METHOD add_image.

    DATA ls_image TYPE ty_s_image.

    sheet_check( ).
    cell_check( row = row
                col = col ).
    image_info( EXPORTING val    = image
                IMPORTING ext    = ls_image-ext
                          width  = ls_image-width
                          height = ls_image-height ).
    IF ls_image-ext IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_IMAGE - the picture is no PNG and no JPEG file`.
    ENDIF.
    " the size asked for - one side given, the other by the proportions
    IF width > 0 AND height > 0.
      ls_image-width = width.
      ls_image-height = height.
    ELSEIF width > 0 AND ls_image-width > 0.
      ls_image-height = ls_image-height * width / ls_image-width.
      ls_image-width = width.
    ELSEIF height > 0 AND ls_image-height > 0.
      ls_image-width = ls_image-width * height / ls_image-height.
      ls_image-height = height.
    ENDIF.
    IF ls_image-width <= 0 OR ls_image-height <= 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_IMAGE - the size of the picture is unknown, give width and height`.
    ENDIF.
    ls_image-row = row.
    ls_image-col = col.
    ls_image-data = image.
    INSERT ls_image INTO TABLE ms_cur-t_image.
    result = me.

  ENDMETHOD.

  METHOD protect_workbook.
      DATA temp239 TYPE string.

    IF strlen( password ) > 15.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_PASSWORD - Excel's legacy protection takes a password of 15 characters at most`.
    ENDIF.
    mv_book_lock = abap_true.
    IF password IS NOT INITIAL.
      
      temp239 = password.
      mv_book_hash = password_hash( temp239 ).
    ENDIF.
    result = me.

  ENDMETHOD.

  METHOD rule_xml.

    DATA lv_cfvo TYPE string.
    DATA lv_head TYPE string.
    DATA ls_area TYPE zabaputil_cl_util_xlsx=>ty_s_area.
    DATA lv_first TYPE string.
        DATA lv_text TYPE string.
        DATA lv_formula TYPE string.
        DATA lv_rank LIKE val-rank.
        DATA lv_min TYPE string.
        DATA lv_max TYPE string.
          DATA lv_mid TYPE string.
        DATA lv_bar TYPE string.
        DATA lv_set LIKE val-icon_set.
        DATA lv_icons TYPE i.

    ms_cur-priority = ms_cur-priority + 1.
    
    lv_head = |<conditionalFormatting sqref="{ sqref }"><cfRule type="{ val-type }"|.
    lv_head = |{ lv_head } priority="{ ms_cur-priority }"|.
    IF val-stop = abap_true.
      lv_head = |{ lv_head } stopIfTrue="1"|.
    ENDIF.
    " the first cell of the range, which a formula of the rule is written for
    
    ls_area = area_parse( sqref ).
    
    lv_first = |{ column_to_letters( ls_area-col1 ) }{ ls_area-row1 }|.

    CASE val-type.

      WHEN `cellIs`.
        CASE val-operator.
          WHEN `between` OR `notBetween` OR `equal` OR `notEqual` OR `greaterThan` OR `lessThan`.
          WHEN `greaterThanOrEqual` OR `lessThanOrEqual`.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |XLSX_RULE - { val-operator } is no operator of a rule|.
        ENDCASE.
        result = |{ lv_head } dxfId="{ dxf_get( val-style ) }" operator="{ val-operator }">|.
        result = |{ result }<formula>{ zabaputil_cl_util_context=>c_escape_xml( val-formula1 ) }</formula>|.
        IF val-formula2 IS NOT INITIAL.
          result = |{ result }<formula>{ zabaputil_cl_util_context=>c_escape_xml( val-formula2 ) }</formula>|.
        ENDIF.

      WHEN `containsText`.
        
        lv_text = replace( val  = val-text
                                 sub  = `"`
                                 with = `""`
                                 occ  = 0 ).
        
        lv_formula = |NOT(ISERROR(SEARCH("{ lv_text }",{ lv_first })))|.
        result = |{ lv_head } dxfId="{ dxf_get( val-style ) }" operator="containsText"|.
        result = |{ result } text="{ zabaputil_cl_util_context=>c_escape_xml( val-text ) }">|.
        result = |{ result }<formula>{ zabaputil_cl_util_context=>c_escape_xml( lv_formula ) }</formula>|.

      WHEN `expression`.
        result = |{ lv_head } dxfId="{ dxf_get( val-style ) }">|.
        result = |{ result }<formula>{ zabaputil_cl_util_context=>c_escape_xml( val-formula1 ) }</formula>|.

      WHEN `top10`.
        
        lv_rank = val-rank.
        IF lv_rank <= 0.
          lv_rank = 10.
        ENDIF.
        result = |{ lv_head } dxfId="{ dxf_get( val-style ) }" rank="{ lv_rank }"|.
        IF val-percent = abap_true.
          result = |{ result } percent="1"|.
        ENDIF.
        IF val-bottom = abap_true.
          result = |{ result } bottom="1"|.
        ENDIF.
        result = |{ result }>|.

      WHEN `duplicateValues` OR `uniqueValues`.
        result = |{ lv_head } dxfId="{ dxf_get( val-style ) }">|.

      WHEN `colorScale`.
        
        lv_min = color_get( val-color_min ).
        
        lv_max = color_get( val-color_max ).
        result = |{ lv_head }><colorScale><cfvo type="min"/>|.
        IF val-color_mid IS NOT INITIAL.
          
          lv_mid = color_get( val-color_mid ).
          result = |{ result }<cfvo type="percentile" val="50"/><cfvo type="max"/>|.
          result = |{ result }<color rgb="{ lv_min }"/><color rgb="{ lv_mid }"/><color rgb="{ lv_max }"/>|.
        ELSE.
          result = |{ result }<cfvo type="max"/><color rgb="{ lv_min }"/><color rgb="{ lv_max }"/>|.
        ENDIF.
        result = |{ result }</colorScale>|.

      WHEN `dataBar`.
        
        lv_bar = `FF638EC6`.
        IF val-color_max IS NOT INITIAL.
          lv_bar = color_get( val-color_max ).
        ENDIF.
        result = |{ lv_head }><dataBar><cfvo type="min"/><cfvo type="max"/><color rgb="{ lv_bar }"/></dataBar>|.

      WHEN `iconSet`.
        " as many thresholds as the set has icons: 3Arrows has three
        
        lv_set = val-icon_set.
        IF lv_set IS INITIAL.
          lv_set = `3TrafficLights1`.
        ENDIF.
        
        lv_icons = 0.
        IF lv_set(1) CA `345`.
          lv_icons = lv_set(1).
        ELSE.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_RULE - { lv_set } is no icon set (3Arrows, 4Rating, 5Quarters, ...)|.
        ENDIF.
        DO lv_icons TIMES.
          lv_cfvo = |{ lv_cfvo }<cfvo type="percent" val="{ ( sy-index - 1 ) * 100 DIV lv_icons }"/>|.
        ENDDO.
        result = |{ lv_head }><iconSet iconSet="{ zabaputil_cl_util_context=>c_escape_xml( lv_set ) }">|.
        result = |{ result }{ lv_cfvo }</iconSet>|.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_RULE - { val-type } is no type of a rule|.

    ENDCASE.
    result = |{ result }</cfRule></conditionalFormatting>|.

  ENDMETHOD.

  METHOD dxf_get.

    DATA lv_xml TYPE string.

    " the parts of a style a rule changes: font, fill, border
    DATA lv_font TYPE string.
      DATA lv_style TYPE string.
      DATA lv_color TYPE string.
      DATA lv_side TYPE string.
    DATA lv_dxf LIKE LINE OF mt_dxf.
    lv_font = ``.
    IF val-bold = abap_true.
      lv_font = `<b/>`.
    ENDIF.
    IF val-italic = abap_true.
      lv_font = |{ lv_font }<i/>|.
    ENDIF.
    IF val-strike = abap_true.
      lv_font = |{ lv_font }<strike/>|.
    ENDIF.
    IF val-underline = abap_true.
      lv_font = |{ lv_font }<u/>|.
    ENDIF.
    IF val-color IS NOT INITIAL.
      lv_font = |{ lv_font }<color rgb="{ color_get( val-color ) }"/>|.
    ENDIF.
    IF lv_font IS NOT INITIAL.
      lv_xml = |<font>{ lv_font }</font>|.
    ENDIF.
    IF val-fill IS NOT INITIAL.
      lv_xml = |{ lv_xml }<fill><patternFill><bgColor rgb="{ color_get( val-fill ) }"/></patternFill></fill>|.
    ENDIF.
    IF val-border IS NOT INITIAL.
      
      lv_style = border_style( val-border ).
      
      lv_color = ``.
      IF val-border_color IS NOT INITIAL.
        lv_color = |<color rgb="{ color_get( val-border_color ) }"/>|.
      ENDIF.
      
      lv_side = |style="{ lv_style }">{ lv_color }|.
      lv_xml = |{ lv_xml }<border><left { lv_side }</left><right { lv_side }</right>|.
      lv_xml = |{ lv_xml }<top { lv_side }</top><bottom { lv_side }</bottom></border>|.
    ENDIF.
    lv_xml = |<dxf>{ lv_xml }</dxf>|.

    
    LOOP AT mt_dxf INTO lv_dxf.
      IF lv_dxf = lv_xml.
        result = sy-tabix - 1.
        RETURN.
      ENDIF.
    ENDLOOP.
    INSERT lv_xml INTO TABLE mt_dxf.
    result = lines( mt_dxf ) - 1.

  ENDMETHOD.

  METHOD decimals_get.

    FIELD-SYMBOLS <dec> TYPE ty_s_sst.
      DATA lv_key TYPE string.
      DATA temp240 TYPE zabaputil_cl_util_xlsx=>ty_s_sst.

    UNASSIGN <dec>.
    READ TABLE mt_decimals WITH KEY text = val ASSIGNING <dec>.
    IF <dec> IS NOT ASSIGNED.
      
      lv_key = substring( val = val
                                off = 1 ).
      IF val(1) = `C`.
        result = zabaputil_cl_util_context=>currency_get_decimals( lv_key ).
      ELSE.
        result = zabaputil_cl_util_context=>unit_get_decimals( lv_key ).
      ENDIF.
      
      CLEAR temp240.
      temp240-text = val.
      temp240-index = result.
      INSERT temp240 INTO TABLE mt_decimals.
      RETURN.
    ENDIF.
    result = <dec>-index.

  ENDMETHOD.

  METHOD password_hash.

    DATA lv_hash  TYPE i.
    DATA lv_value TYPE p LENGTH 16 DECIMALS 0.
    DATA lv_rot   TYPE i.
    DATA lv_low   TYPE i.

    " ECMA-376 part 4, 14.7.1: every character shifted by its position,
    " the bits shifted out of the 15 brought back in, all of it xor-ed
    DATA lt_code TYPE zabaputil_cl_util_context=>ty_t_codepoint.
    DATA lv_code LIKE LINE OF lt_code.
    lt_code = zabaputil_cl_util_context=>c_get_codepoints( val ).
    
    LOOP AT lt_code INTO lv_code.
      lv_value = lv_code MOD 256.
      DO sy-tabix TIMES.
        lv_value = lv_value * 2.
      ENDDO.
      lv_rot = lv_value DIV 32768.
      lv_low = lv_value MOD 32768.
      " the bits brought back lie below the shifted ones: an or is a sum
      lv_hash = bit_xor( a = lv_hash
                         b = lv_low + lv_rot ).
    ENDLOOP.
    lv_hash = bit_xor( a = lv_hash
                       b = lines( lt_code ) ).
    lv_hash = bit_xor( a = lv_hash
                       b = 52811 ).

    " four hex digits
    DO 4 TIMES.
      result = substring( val = cv_hex
                          off = lv_hash MOD 16
                          len = 1 ) && result.
      lv_hash = lv_hash DIV 16.
    ENDDO.

  ENDMETHOD.

  METHOD bit_xor.

    DATA lv_a LIKE a.
    DATA lv_b LIKE b.
    DATA lv_bit TYPE i.
    lv_a = a.
    
    lv_b = b.
    
    lv_bit = 1.
    DO 16 TIMES.
      IF lv_a MOD 2 <> lv_b MOD 2.
        result = result + lv_bit.
      ENDIF.
      lv_a = lv_a DIV 2.
      lv_b = lv_b DIV 2.
      lv_bit = lv_bit * 2.
    ENDDO.

  ENDMETHOD.

  METHOD comments_xml.

    DATA lt_author TYPE string_table.
    DATA lt_xml    TYPE string_table.
    DATA lt_vml    TYPE string_table.

    DATA temp241 LIKE LINE OF t_comment.
    DATA lr_comment LIKE REF TO temp241.
      DATA lv_known LIKE abap_false.
    DATA temp242 LIKE LINE OF lt_xml.
    DATA lv_author LIKE LINE OF lt_author.
      DATA temp243 LIKE LINE OF lt_xml.
    DATA temp244 LIKE LINE OF lt_vml.
      DATA lv_index LIKE sy-tabix.
      DATA lv_ref TYPE string.
      DATA lv_author_id TYPE i.
      DATA lv_text TYPE string.
      DATA temp245 LIKE LINE OF lt_xml.
      DATA temp246 LIKE LINE OF lt_xml.
      DATA lv_shape TYPE i.
      DATA temp247 LIKE LINE OF lt_vml.
      DATA temp248 LIKE LINE OF lt_vml.
      DATA lv_col TYPE zabaputil_cl_util_xlsx=>ty_s_comment-col.
      DATA lv_row TYPE zabaputil_cl_util_xlsx=>ty_s_comment-row.
      DATA temp249 LIKE LINE OF lt_vml.
      DATA temp250 LIKE LINE OF lt_vml.
      DATA temp251 LIKE LINE OF lt_vml.
    LOOP AT t_comment REFERENCE INTO lr_comment.
      
      lv_known = abap_false.
      LOOP AT lt_author TRANSPORTING NO FIELDS WHERE table_line = lr_comment->author.
        lv_known = abap_true.
      ENDLOOP.
      IF lv_known = abap_false.
        INSERT lr_comment->author INTO TABLE lt_author.
      ENDIF.
    ENDLOOP.

    
    temp242 = |{ cv_xml_head }<comments xmlns="{ cs_ns-main }"><authors>|.
    INSERT temp242 INTO TABLE lt_xml.
    
    LOOP AT lt_author INTO lv_author.
      
      temp243 = |<author>{ zabaputil_cl_util_context=>c_escape_xml( lv_author ) }</author>|.
      INSERT temp243 INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</authors><commentList>` INTO TABLE lt_xml.

    " the shapes Excel draws the notes with: hidden until the mouse is on
    " the cell, anchored right of it
    INSERT `<xml xmlns:v="urn:schemas-microsoft-com:vml" xmlns:o="urn:schemas-microsoft-com:office:office"`
           INTO TABLE lt_vml.
    INSERT ` xmlns:x="urn:schemas-microsoft-com:office:excel">` INTO TABLE lt_vml.
    
    temp244 = |<o:shapelayout v:ext="edit"><o:idmap v:ext="edit" data="{ id }"/></o:shapelayout>|.
    INSERT temp244 INTO TABLE lt_vml.
    INSERT `<v:shapetype id="_x0000_t202" coordsize="21600,21600" o:spt="202" path="m,l,21600r21600,l21600,xe">`
           INTO TABLE lt_vml.
    INSERT `<v:stroke joinstyle="miter"/><v:path gradientshapeok="t" o:connecttype="rect"/></v:shapetype>`
           INTO TABLE lt_vml.

    LOOP AT t_comment REFERENCE INTO lr_comment.
      
      lv_index = sy-tabix.
      
      lv_ref = |{ column_to_letters( lr_comment->col ) }{ lr_comment->row }|.
      
      lv_author_id = 0.
      LOOP AT lt_author INTO lv_author.
        IF lv_author = lr_comment->author.
          lv_author_id = sy-tabix - 1.
          EXIT.
        ENDIF.
      ENDLOOP.
      
      lv_text = zabaputil_cl_util_context=>c_escape_xml( lr_comment->text ).
      
      temp245 = |<comment ref="{ lv_ref }" authorId="{ lv_author_id }"><text><r>|.
      INSERT temp245 INTO TABLE lt_xml.
      
      temp246 = |<t xml:space="preserve">{ lv_text }</t></r></text></comment>|.
      INSERT temp246 INTO TABLE lt_xml.

      
      lv_shape = id * 1024 + lv_index.
      
      temp247 = |<v:shape id="_x0000_s{ lv_shape }" type="#_x0000_t202"|.
      INSERT temp247 INTO TABLE lt_vml.
      INSERT ` style="position:absolute;margin-left:59.25pt;margin-top:1.5pt;width:108pt;height:59.25pt;`
             INTO TABLE lt_vml.
      
      temp248 = |z-index:{ lv_index };visibility:hidden" fillcolor="#ffffe1" o:insetmode="auto">|.
      INSERT temp248 INTO TABLE lt_vml.
      INSERT `<v:fill color2="#ffffe1"/><v:shadow on="t" color="black" obscured="t"/>` INTO TABLE lt_vml.
      INSERT `<v:path o:connecttype="none"/><v:textbox style="mso-direction-alt:auto">` INTO TABLE lt_vml.
      INSERT `<div style="text-align:left"></div></v:textbox><x:ClientData ObjectType="Note">` INTO TABLE lt_vml.
      
      lv_col = lr_comment->col - 1.
      
      lv_row = lr_comment->row - 1.
      
      temp249 = |<x:MoveWithCells/><x:SizeWithCells/><x:Anchor>{ lv_col + 1 }, 15, { lv_row }, 10,|.
      INSERT temp249 INTO TABLE lt_vml.
      
      temp250 = | { lv_col + 3 }, 15, { lv_row + 4 }, 4</x:Anchor><x:AutoFill>False</x:AutoFill>|.
      INSERT temp250 INTO TABLE lt_vml.
      
      temp251 = |<x:Row>{ lv_row }</x:Row><x:Column>{ lv_col }</x:Column></x:ClientData></v:shape>|.
      INSERT temp251 INTO TABLE lt_vml.
    ENDLOOP.
    INSERT `</commentList></comments>` INTO TABLE lt_xml.
    INSERT `</xml>` INTO TABLE lt_vml.
    comments = concat_lines_of( lt_xml ).
    vml = concat_lines_of( lt_vml ).

  ENDMETHOD.

  METHOD drawing_xml.

    DATA lt_xml  TYPE string_table.
    DATA lt_rels TYPE string_table.
    DATA temp252 LIKE LINE OF lt_xml.
    DATA temp253 LIKE LINE OF lt_xml.
    DATA temp254 LIKE LINE OF lt_rels.
    DATA temp255 LIKE LINE OF t_image.
    DATA lr_image LIKE REF TO temp255.
      DATA lv_index LIKE sy-tabix.
      DATA lv_media TYPE string.
      DATA temp256 TYPE zabaputil_cl_util_xlsx=>ty_s_out_file.
      DATA temp257 LIKE LINE OF lt_rels.
      DATA lv_cx TYPE zabaputil_cl_util_xlsx=>ty_s_image-width.
      DATA lv_cy TYPE zabaputil_cl_util_xlsx=>ty_s_image-height.
      DATA temp258 LIKE LINE OF lt_xml.
      DATA temp259 LIKE LINE OF lt_xml.
      DATA temp260 LIKE LINE OF lt_xml.
      DATA temp261 LIKE LINE OF lt_xml.
      DATA temp262 LIKE LINE OF lt_xml.
      DATA temp263 LIKE LINE OF lt_xml.

    CLEAR t_media.
    
    temp252 = |{ cv_xml_head }<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing"|.
    INSERT temp252
           INTO TABLE lt_xml.
    
    temp253 = | xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="{ cs_ns-rel }">|.
    INSERT temp253
           INTO TABLE lt_xml.
    
    temp254 = |{ cv_xml_head }<Relationships xmlns="{ cs_ns-pkg_rel }">|.
    INSERT temp254 INTO TABLE lt_rels.

    
    
    LOOP AT t_image REFERENCE INTO lr_image.
      
      lv_index = sy-tabix.
      mv_images = mv_images + 1.
      
      lv_media = |image{ mv_images }.{ lr_image->ext }|.
      
      CLEAR temp256.
      temp256-name = |xl/media/{ lv_media }|.
      temp256-xml = lr_image->data.
      INSERT temp256 INTO TABLE t_media.
      
      temp257 = |<Relationship Id="rId{ lv_index }" Type="{ cs_ns-rel }/image" Target="../media/{ lv_media }"/>|.
      INSERT temp257
             INTO TABLE lt_rels.
      " English metric units: 9525 to a pixel
      
      lv_cx = lr_image->width * 9525.
      
      lv_cy = lr_image->height * 9525.
      
      temp258 = |<xdr:oneCellAnchor><xdr:from><xdr:col>{ lr_image->col - 1 }</xdr:col><xdr:colOff>0</xdr:colOff>|.
      INSERT temp258
             INTO TABLE lt_xml.
      
      temp259 = |<xdr:row>{ lr_image->row - 1 }</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>|.
      INSERT temp259 INTO TABLE lt_xml.
      
      temp260 = |<xdr:ext cx="{ lv_cx }" cy="{ lv_cy }"/><xdr:pic><xdr:nvPicPr>|.
      INSERT temp260 INTO TABLE lt_xml.
      
      temp261 = |<xdr:cNvPr id="{ lv_index + 1 }" name="Picture { lv_index }"/>|.
      INSERT temp261 INTO TABLE lt_xml.
      INSERT `<xdr:cNvPicPr><a:picLocks noChangeAspect="1"/></xdr:cNvPicPr></xdr:nvPicPr>` INTO TABLE lt_xml.
      
      temp262 = |<xdr:blipFill><a:blip r:embed="rId{ lv_index }"/><a:stretch><a:fillRect/></a:stretch></xdr:blipFill>|.
      INSERT temp262
             INTO TABLE lt_xml.
      
      temp263 = |<xdr:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="{ lv_cx }" cy="{ lv_cy }"/></a:xfrm>|.
      INSERT temp263 INTO TABLE lt_xml.
      INSERT `<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></xdr:spPr></xdr:pic><xdr:clientData/>` INTO TABLE lt_xml.
      INSERT `</xdr:oneCellAnchor>` INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</xdr:wsDr>` INTO TABLE lt_xml.
    INSERT `</Relationships>` INTO TABLE lt_rels.
    drawing = concat_lines_of( lt_xml ).
    rels = concat_lines_of( lt_rels ).

  ENDMETHOD.

  METHOD image_info.

    CONSTANTS lc_png  TYPE x LENGTH 8 VALUE '89504E470D0A1A0A'.
    CONSTANTS lc_jpeg TYPE x LENGTH 2 VALUE 'FFD8'.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_word TYPE x LENGTH 2.
    DATA lv_long TYPE x LENGTH 4.
    DATA lv_len TYPE i.
    DATA lv_pos TYPE i.
      DATA lv_off TYPE i.
      DATA temp264 TYPE i.
      DATA lv_segment LIKE temp264.

    CLEAR ext.
    CLEAR width.
    CLEAR height.
    
    lv_len = xstrlen( val ).

    " PNG: the header chunk names width and height, four bytes each
    IF lv_len >= 24 AND val(8) = lc_png.
      ext = `png`.
      lv_long = val+16(4).
      width = lv_long.
      lv_long = val+20(4).
      height = lv_long.
      RETURN.
    ENDIF.

    " JPEG: the size is in the frame header, behind the segments before it
    IF lv_len < 4 OR val(2) <> lc_jpeg.
      RETURN.
    ENDIF.
    ext = `jpeg`.
    
    lv_pos = 2.
    WHILE lv_pos + 9 < lv_len.
      lv_byte = val+lv_pos(1).
      IF lv_byte <> 'FF'.
        RETURN.
      ENDIF.
      
      lv_off = lv_pos + 1.
      lv_byte = val+lv_off(1).
      " SOF0 to SOF15 without DHT (C4), JPG (C8) and DAC (CC)
      IF lv_byte >= 'C0' AND lv_byte <= 'CF' AND lv_byte <> 'C4' AND lv_byte <> 'C8' AND lv_byte <> 'CC'.
        lv_off = lv_pos + 5.
        lv_word = val+lv_off(2).
        height = lv_word.
        lv_off = lv_pos + 7.
        lv_word = val+lv_off(2).
        width = lv_word.
        RETURN.
      ENDIF.
      lv_off = lv_pos + 2.
      lv_word = val+lv_off(2).
      
      temp264 = lv_word.
      
      lv_segment = temp264.
      lv_pos = lv_pos + 2 + lv_segment.
    ENDWHILE.

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
      DATA lv_x TYPE zabaputil_cl_util_xlsx=>ty_s_area-col1.
      DATA lv_y TYPE zabaputil_cl_util_xlsx=>ty_s_area-row1.
      DATA lv_cell TYPE string.
      DATA lv_split TYPE string.
      DATA lv_pane TYPE string.

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
      
      lv_x = ls_area-col1 - 1.
      
      lv_y = ls_area-row1 - 1.
      
      lv_cell = area_text( ls_area ).
      
      lv_split = ``.
      IF lv_x > 0.
        lv_split = | xSplit="{ lv_x }"|.
      ENDIF.
      IF lv_y > 0.
        lv_split = |{ lv_split } ySplit="{ lv_y }"|.
      ENDIF.
      
      lv_pane = `bottomRight`.
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
    FIELD-SYMBOLS <sst>  TYPE ty_s_sst.
    " the style of an amount by the decimals of its currency, per column
    TYPES temp9 TYPE HASHED TABLE OF ty_s_sst WITH UNIQUE KEY text.
DATA lt_dec_style TYPE temp9.
      DATA lv_start_row TYPE string.
    DATA lt_col TYPE zabaputil_cl_util_xlsx=>ty_t_out_col.
    DATA lv_table TYPE abap_bool.
    DATA temp1 TYPE xsdboolean.
        DATA temp2 TYPE xsdboolean.
    DATA ls_head LIKE layout-header_style.
    DATA lv_head_style TYPE string.
    DATA temp265 TYPE zabaputil_cl_util_xlsx=>ty_s_line.
    DATA lv_attr TYPE string.
    DATA lv_first_data TYPE i.
        DATA lv_style LIKE <col>-style.
            DATA temp266 TYPE zabaputil_cl_util_xlsx=>ty_s_link.
          DATA lv_dec TYPE i.
            DATA lv_code LIKE <col>-format.
            DATA temp267 TYPE zabaputil_cl_util_xlsx=>ty_s_sst.
          DATA temp268 LIKE LINE OF <col>-t_fix.
          DATA lr_fix LIKE REF TO temp268.
    DATA lv_last_data LIKE lv_row.
          DATA lv_total_style LIKE <col>-style.
    DATA lv_range TYPE string.
    DATA ls_filter LIKE ls_area.
    DATA lv_filter TYPE string.
      DATA lv_index TYPE i.
      DATA temp269 TYPE zabaputil_cl_util_xlsx=>ty_s_out_part.

    " where the header row starts: A1, or the cell the layout names
    IF layout-start_cell IS NOT INITIAL.
      
      lv_start_row = digits_only( layout-start_cell ).
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
    
    lt_col = columns_get( t_field = lt_field
                                layout  = layout
                                offset  = lv_col0 ).
    IF lt_col IS INITIAL.
      RETURN.
    ENDIF.

    
    
    temp1 = boolc( layout-table_style IS NOT INITIAL AND layout-no_header = abap_false AND val IS NOT INITIAL ).
    lv_table = temp1.
    IF lv_table = abap_true.
      lv_table_name = table_name_get( layout-table_name ).
    ENDIF.
    LOOP AT lt_col ASSIGNING <col>.
      IF <col>-total_fn IS NOT INITIAL OR <col>-total_label IS NOT INITIAL.
        " a totals row under no rows adds up nothing
        
        temp2 = boolc( val IS NOT INITIAL ).
        lv_totals = temp2.
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
    
    ls_head = layout-header_style.
    IF lv_table = abap_false.
      ls_head-bold = abap_true.
    ENDIF.
    
    lv_head_style = style_attr( style = ls_head ).

    " the height and the outline level of the rows
    
    CLEAR temp265.
    temp265-size = layout-row_height.
    temp265-level = layout-outline_level.
    
    lv_attr = line_attr( temp265 ).
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
    
    lv_first_data = lv_row + 1.

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
        
        lv_style = <col>-style.
        IF <col>-link IS NOT INITIAL.
          UNASSIGN <link>.
          IF lv_elementary = abap_true.
            ASSIGN <row> TO <link>.
          ELSE.
            ASSIGN COMPONENT <col>-link OF STRUCTURE <row> TO <link>.
          ENDIF.
          IF <link> IS ASSIGNED AND <link> IS NOT INITIAL.
            
            CLEAR temp266.
            temp266-ref = lv_ref.
            temp266-target = |{ <link> }|.
            INSERT temp266 INTO TABLE ms_cur-t_link.
            lv_style = <col>-link_style.
          ENDIF.
        ENDIF.

        IF <col>-currency IS NOT INITIAL OR <col>-unit IS NOT INITIAL.
          " an amount or a quantity: the decimals of its currency or unit
          UNASSIGN <link>.
          IF <col>-currency IS NOT INITIAL.
            ASSIGN COMPONENT <col>-currency OF STRUCTURE <row> TO <link>.
            lv_value = |C{ <link> }|.
          ELSE.
            ASSIGN COMPONENT <col>-unit OF STRUCTURE <row> TO <link>.
            lv_value = |U{ <link> }|.
          ENDIF.
          
          lv_dec = decimals_get( lv_value ).
          lv_number = <cell>.
          IF <col>-currency IS NOT INITIAL.
            " as WRITE ... CURRENCY: the field's decimals are the currency's
            DO <col>-decimals - lv_dec TIMES.
              lv_number = lv_number * 10.
            ENDDO.
            DO lv_dec - <col>-decimals TIMES.
              lv_number = lv_number / 10.
            ENDDO.
          ELSEIF lv_dec < 0.
            lv_dec = <col>-decimals.
          ENDIF.
          lv_value = |{ <col>-letters }:{ lv_dec }|.
          UNASSIGN <sst>.
          READ TABLE lt_dec_style WITH KEY text = lv_value ASSIGNING <sst>.
          IF <sst> IS NOT ASSIGNED.
            
            lv_code = <col>-format.
            IF lv_code IS INITIAL AND lv_dec > 0.
              lv_code = `#,##0.` && repeat( val = `0`
                                            occ = lv_dec ).
            ELSEIF lv_code IS INITIAL.
              lv_code = `#,##0`.
            ENDIF.
            
            CLEAR temp267.
            temp267-text = lv_value.
            temp267-index = style_get( code = lv_code style = <col>-style_base ).
            INSERT temp267 INTO TABLE lt_dec_style ASSIGNING <sst>.
          ENDIF.
          lv_text = number_plain( |{ lv_number }| ).
          IF <sst>-index > 0.
            lv_xml = |<c r="{ lv_ref }" s="{ <sst>-index }"><v>{ lv_text }</v></c>|.
          ELSE.
            lv_xml = |<c r="{ lv_ref }"><v>{ lv_text }</v></c>|.
          ENDIF.
        ELSEIF <col>-conv_exit IS INITIAL AND <col>-t_fix IS INITIAL.
          cell_xml( EXPORTING address   = lv_ref
                              cat       = <col>-cat
                              style     = lv_style
                              val       = <cell>
                              time_zone = layout-time_zone
                    IMPORTING xml       = lv_xml
                              text      = lv_text ).
        ELSE.
          " the external form: by the conversion exit, the text of the
          " fixed value - a text
          lv_value = |{ <cell> }|.
          IF <col>-conv_exit IS NOT INITIAL.
            TRY.
                zabaputil_cl_util_context=>conv_exit( EXPORTING convexit = <col>-conv_exit
                                                                output   = abap_true
                                                                from     = <cell>
                                                      CHANGING  value    = lv_value ).
              CATCH cx_root ##NO_HANDLER.
                " the exit refuses the value: written as it is
            ENDTRY.
            " an exit fills its output field up with blanks
            lv_value = zabaputil_cl_util_context=>c_trim( lv_value ).
          ENDIF.
          
          
          LOOP AT <col>-t_fix REFERENCE INTO lr_fix WHERE low = lv_value.
            lv_value = lr_fix->text.
          ENDLOOP.
          cell_xml( EXPORTING address = lv_ref
                              cat     = cs_cat-text
                              style   = lv_style
                              val     = lv_value
                    IMPORTING xml     = lv_xml
                              text    = lv_text ).
        ENDIF.
        lv_cells = lv_cells && lv_xml.

        IF <col>-cat = cs_cat-number AND <col>-total_fn IS NOT INITIAL.
          IF <col>-currency IS INITIAL AND <col>-unit IS INITIAL.
            lv_number = <cell>.
          ENDIF.
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
    
    lv_last_data = lv_row.

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
          
          lv_total_style = <col>-style.
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
    
    lv_range = area_text( ls_area ).
    
    ls_filter = ls_area.
    ls_filter-row2 = nmax( val1 = lv_last_data
                           val2 = lv_row0 + 1 ).
    
    lv_filter = area_text( ls_filter ).

    " the widths, and what the layout says of the columns
    LOOP AT lt_col ASSIGNING <col>.
      
      lv_index = lv_col0 + sy-tabix.
      col_len( col  = lv_index
               len  = <col>-len
               size = <col>-width ).
      IF <col>-hidden = abap_true OR <col>-level > 0.
        set_column( col           = lv_index
                    hidden        = <col>-hidden
                    outline_level = <col>-level ).
      ENDIF.
      lv_value = |{ <col>-letters }{ lv_first_data }:{ <col>-letters }{ lv_last_data }|.
      IF <col>-validation IS NOT INITIAL AND lv_last_data >= lv_first_data.
        INSERT validation_xml( sqref = lv_value
                               val   = <col>-validation ) INTO TABLE ms_cur-t_valid.
      ENDIF.
      IF <col>-rule IS NOT INITIAL AND lv_last_data >= lv_first_data.
        INSERT rule_xml( sqref = lv_value
                         val   = <col>-rule ) INTO TABLE ms_cur-t_rule.
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
      
      CLEAR temp269.
      temp269-id = mv_tables.
      temp269-xml = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_xml ).
      INSERT temp269
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
        " a utclong as a template writes it: 2024-03-15 10:30:00.1234567
        IF text CA `-:`.
          text = digits_only( text ).
          IF strlen( text ) > 14.
            text = text(14).
          ENDIF.
        ENDIF.
        IF time_zone IS NOT INITIAL AND val IS NOT INITIAL AND strlen( text ) >= 14.
          lv_stampl = text.
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

    DATA temp270 TYPE zabaputil_cl_util_xlsx=>ty_s_seg.
    CLEAR temp270.
    temp270-row = row.
    temp270-col = col.
    temp270-col_to = col_to.
    temp270-attr = attr.
    temp270-xml = xml.
    INSERT temp270 INTO TABLE ms_cur-t_seg.
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
      DATA temp271 LIKE LINE OF ms_cur-t_seg.
      DATA lr_seg LIKE REF TO temp271.

    IF ms_cur-t_seg IS INITIAL.
      RETURN.
    ENDIF.

    " one row by the key; the rows of a table in one pass
    IF area-row1 = area-row2.
      
      
      LOOP AT ms_cur-t_seg REFERENCE INTO lr_seg WHERE row = area-row1.
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
      DATA temp272 TYPE zabaputil_cl_util_xlsx=>ty_s_line.

    UNASSIGN <line>.
    READ TABLE ms_cur-t_col WITH KEY pos = col ASSIGNING <line>.
    IF <line> IS NOT ASSIGNED.
      
      CLEAR temp272.
      temp272-pos = col.
      INSERT temp272 INTO TABLE ms_cur-t_col ASSIGNING <line>.
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
    DATA ls_layout LIKE ms_cur-layout.
    DATA lv_index TYPE i.
    DATA ls_line LIKE LINE OF ms_cur-t_row.
      DATA lv_found LIKE abap_false.
        DATA temp273 TYPE zabaputil_cl_util_xlsx=>ty_s_seg.
    DATA temp274 LIKE LINE OF ms_cur-t_seg.
    DATA lr_seg LIKE REF TO temp274.
          DATA temp275 LIKE LINE OF lt_xml.
        DATA lv_attr LIKE lr_seg->attr.
        DATA temp276 LIKE sy-subrc.
        DATA temp277 LIKE sy-tabix.
          FIELD-SYMBOLS <temp278> LIKE LINE OF ms_cur-t_row.
          DATA temp279 LIKE sy-tabix.
      DATA temp280 LIKE LINE OF lt_xml.
    DATA lv_range TYPE string.
      FIELD-SYMBOLS <temp281> LIKE LINE OF ms_cur-t_seg.
      DATA temp282 LIKE sy-tabix.
      FIELD-SYMBOLS <temp283> LIKE LINE OF ms_cur-t_seg.
      DATA temp284 LIKE sy-tabix.
    DATA lv_view TYPE string.
    DATA lv_pr TYPE string.
    DATA temp285 LIKE LINE OF lt_head.
      DATA temp286 LIKE LINE OF lt_head.
    DATA temp287 LIKE LINE OF lt_head.
    DATA temp288 LIKE LINE OF lt_head.
    DATA lv_format TYPE string.
    DATA temp289 LIKE LINE OF lt_head.
      DATA lv_width LIKE ls_line-size.
      DATA temp290 TYPE zabaputil_cl_util_xlsx=>ty_s_line.
      DATA lv_protect TYPE string.
      DATA temp291 LIKE LINE OF lt_xml.
      DATA temp292 LIKE LINE OF lt_xml.
      DATA temp293 LIKE LINE OF lt_xml.
        DATA temp294 LIKE LINE OF lt_xml.
      DATA temp295 LIKE LINE OF lt_xml.
    DATA temp296 LIKE LINE OF lt_rels.
    DATA temp297 LIKE LINE OF ms_cur-t_table.
    DATA lr_table LIKE REF TO temp297.
      DATA temp298 LIKE LINE OF lt_rels.
      DATA temp299 LIKE LINE OF lt_rels.
      DATA temp300 LIKE LINE OF ms_cur-t_link.
      DATA lr_link LIKE REF TO temp300.
          DATA lv_target TYPE string.
          DATA temp301 LIKE LINE OF lt_xml.
        DATA lv_link TYPE i.
          DATA temp302 LIKE LINE OF lt_rels.
          DATA temp303 LIKE LINE OF lt_rels.
        DATA temp304 LIKE LINE OF lt_xml.
    DATA lv_drawing TYPE string.
      DATA lv_drawing_xml TYPE string.
      DATA lv_drawing_rels TYPE string.
      DATA lt_media TYPE zabaputil_cl_util_xlsx=>ty_t_out_file.
      DATA temp305 TYPE zabaputil_cl_util_xlsx=>ty_s_out_file.
      DATA temp306 TYPE zabaputil_cl_util_xlsx=>ty_s_out_file.
      DATA temp307 LIKE LINE OF lt_rels.
      DATA temp308 LIKE LINE OF lt_rels.
    DATA lv_legacy TYPE string.
      DATA lv_comments_xml TYPE string.
      DATA lv_vml TYPE string.
      DATA temp309 TYPE zabaputil_cl_util_xlsx=>ty_s_out_file.
      DATA temp310 TYPE zabaputil_cl_util_xlsx=>ty_s_out_file.
      DATA temp311 LIKE LINE OF lt_rels.
      DATA temp312 LIKE LINE OF lt_rels.
      DATA temp313 LIKE LINE OF lt_rels.
      DATA temp314 LIKE LINE OF lt_rels.
      DATA temp315 TYPE decfloat34.
      DATA lv_inch LIKE temp315.
      DATA lv_margin TYPE string.
      DATA temp316 LIKE LINE OF lt_xml.
      DATA temp317 LIKE LINE OF lt_xml.
    DATA lv_setup TYPE string.
      DATA temp318 LIKE LINE OF lt_xml.
        DATA lv_text LIKE ls_layout-header_text.
        DATA temp319 LIKE LINE OF lt_xml.
        DATA temp320 LIKE LINE OF lt_xml.
      DATA temp321 LIKE LINE OF lt_xml.
        DATA temp322 LIKE LINE OF lt_xml.
    DATA lv_sheet TYPE string.
      DATA temp323 TYPE zabaputil_cl_util_xlsx=>ty_s_def.
      DATA temp324 TYPE zabaputil_cl_util_xlsx=>ty_s_def.

    IF mv_open = abap_false.
      RETURN.
    ENDIF.
    mv_open = abap_false.
    
    ls_layout = ms_cur-layout.
    
    lv_index = lines( mt_out_sheet ) + 1.

    " a row with a height or an outline level and no cell is a row too
    
    LOOP AT ms_cur-t_row INTO ls_line.
      
      lv_found = abap_false.
      LOOP AT ms_cur-t_seg TRANSPORTING NO FIELDS WHERE row = ls_line-pos.
        lv_found = abap_true.
        EXIT.
      ENDLOOP.
      IF lv_found = abap_false.
        
        CLEAR temp273.
        temp273-row = ls_line-pos.
        INSERT temp273 INTO TABLE ms_cur-t_seg.
      ENDIF.
    ENDLOOP.

    " the rows, each with its cells in the order of their columns
    
    
    LOOP AT ms_cur-t_seg REFERENCE INTO lr_seg.
      IF lr_seg->row <> lv_row.
        IF lv_row > 0.
          
          temp275 = |{ lv_cells }</row>|.
          INSERT temp275 INTO TABLE lt_xml.
        ENDIF.
        lv_row = lr_seg->row.
        
        lv_attr = lr_seg->attr.
        
        
        temp277 = sy-tabix.
        READ TABLE ms_cur-t_row WITH KEY pos = lv_row TRANSPORTING NO FIELDS.
        temp276 = sy-subrc.
        sy-tabix = temp277.
        IF ms_cur-t_row IS NOT INITIAL AND temp276 = 0.
          
          
          temp279 = sy-tabix.
          READ TABLE ms_cur-t_row WITH KEY pos = lv_row ASSIGNING <temp278>.
          sy-tabix = temp279.
          IF sy-subrc <> 0.
            ASSERT 1 = 0.
          ENDIF.
          lv_attr = line_attr( <temp278> ).
        ENDIF.
        lv_cells = |<row r="{ lv_row }"{ lv_attr }>|.
      ENDIF.
      lv_cells = lv_cells && lr_seg->xml.
    ENDLOOP.
    IF lv_row > 0.
      
      temp280 = |{ lv_cells }</row>|.
      INSERT temp280 INTO TABLE lt_xml.
    ENDIF.
    CLEAR lv_cells.

    " the range of the sheet
    
    lv_range = `A1`.
    IF ms_cur-col_max > 0.
      
      
      temp282 = sy-tabix.
      READ TABLE ms_cur-t_seg INDEX 1 ASSIGNING <temp281>.
      sy-tabix = temp282.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      ls_area-row1 = <temp281>-row.
      
      
      temp284 = sy-tabix.
      READ TABLE ms_cur-t_seg INDEX lines( ms_cur-t_seg ) ASSIGNING <temp283>.
      sy-tabix = temp284.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      ls_area-row2 = <temp283>-row.
      ls_area-col1 = ms_cur-col_min.
      ls_area-col2 = ms_cur-col_max.
      lv_range = area_text( ls_area ).
    ENDIF.

    " the first sheet that is not hidden is the one Excel shows
    
    lv_view = ``.
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

    
    lv_pr = ``.
    IF ls_layout-tab_color IS NOT INITIAL.
      lv_pr = |<tabColor rgb="{ color_get( ls_layout-tab_color ) }"/>|.
    ENDIF.
    IF ls_layout-fit_width > 0 OR ls_layout-fit_height > 0.
      lv_pr = |{ lv_pr }<pageSetUpPr fitToPage="1"/>|.
    ENDIF.

    
    temp285 = |{ cv_xml_head }<worksheet xmlns="{ cs_ns-main }" xmlns:r="{ cs_ns-rel }">|.
    INSERT temp285 INTO TABLE lt_head.
    IF lv_pr IS NOT INITIAL.
      
      temp286 = |<sheetPr>{ lv_pr }</sheetPr>|.
      INSERT temp286 INTO TABLE lt_head.
    ENDIF.
    
    temp287 = |<dimension ref="{ lv_range }"/>|.
    INSERT temp287 INTO TABLE lt_head.
    
    temp288 = |<sheetViews><sheetView workbookViewId="0"{ lv_view }>{ ms_cur-pane }</sheetView></sheetViews>|.
    INSERT temp288
           INTO TABLE lt_head.
    
    lv_format = ``.
    IF ms_cur-row_level > 0.
      lv_format = | outlineLevelRow="{ ms_cur-row_level }"|.
    ENDIF.
    IF ms_cur-col_level > 0.
      lv_format = |{ lv_format } outlineLevelCol="{ ms_cur-col_level }"|.
    ENDIF.
    
    temp289 = |<sheetFormatPr defaultRowHeight="15"{ lv_format }/>|.
    INSERT temp289 INTO TABLE lt_head.

    " the widths: by the longest content, in bounds - a long text does not
    " push every other column off the screen
    LOOP AT ms_cur-t_col INTO ls_line.
      
      lv_width = ls_line-size.
      IF lv_width <= 0 AND ls_line-auto = abap_true.
        lv_width = nmin( val1 = nmax( val1 = ls_line-len + 2
                                      val2 = 6 )
                         val2 = 60 ).
      ELSEIF lv_width <= 0.
        lv_width = 9.
      ENDIF.
      
      CLEAR temp290.
      temp290-hidden = ls_line-hidden.
      temp290-level = ls_line-level.
      lv_attr = line_attr( temp290 ).
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
    IF ls_layout-protect = abap_true.
      
      lv_protect = `<sheetProtection sheet="1" objects="1" scenarios="1"`.
      IF ls_layout-password IS NOT INITIAL.
        lv_protect = |{ lv_protect } password="{ password_hash( ls_layout-password ) }"|.
      ENDIF.
      " a filter stays at hand on a protected sheet
      IF ms_cur-filter IS NOT INITIAL OR ms_cur-t_table IS NOT INITIAL.
        lv_protect = |{ lv_protect } autoFilter="0" sort="0"|.
      ENDIF.
      
      temp291 = |{ lv_protect }/>|.
      INSERT temp291 INTO TABLE lt_xml.
    ENDIF.
    IF ms_cur-filter IS NOT INITIAL.
      
      temp292 = |<autoFilter ref="{ ms_cur-filter }"/>|.
      INSERT temp292 INTO TABLE lt_xml.
    ENDIF.
    IF ms_cur-t_merge IS NOT INITIAL.
      
      temp293 = |<mergeCells count="{ lines( ms_cur-t_merge ) }">|.
      INSERT temp293 INTO TABLE lt_xml.
      LOOP AT ms_cur-t_merge INTO ls_area.
        
        temp294 = |<mergeCell ref="{ area_text( ls_area ) }"/>|.
        INSERT temp294 INTO TABLE lt_xml.
      ENDLOOP.
      INSERT `</mergeCells>` INTO TABLE lt_xml.
    ENDIF.
    INSERT LINES OF ms_cur-t_rule INTO TABLE lt_xml.
    IF ms_cur-t_valid IS NOT INITIAL.
      
      temp295 = |<dataValidations count="{ lines( ms_cur-t_valid ) }">|.
      INSERT temp295 INTO TABLE lt_xml.
      INSERT LINES OF ms_cur-t_valid INTO TABLE lt_xml.
      INSERT `</dataValidations>` INTO TABLE lt_xml.
    ENDIF.

    " the relationships: the tables first, then the targets of the links -
    " each target once
    
    temp296 = |{ cv_xml_head }<Relationships xmlns="{ cs_ns-pkg_rel }">|.
    INSERT temp296 INTO TABLE lt_rels.
    
    
    LOOP AT ms_cur-t_table REFERENCE INTO lr_table.
      lv_rid = lv_rid + 1.
      
      temp298 = |<Relationship Id="rId{ lv_rid }" Type="{ cs_ns-rel }/table"|.
      INSERT temp298 INTO TABLE lt_rels.
      
      temp299 = | Target="../tables/table{ lr_table->id }.xml"/>|.
      INSERT temp299 INTO TABLE lt_rels.
    ENDLOOP.
    IF ms_cur-t_link IS NOT INITIAL.
      INSERT `<hyperlinks>` INTO TABLE lt_xml.
      
      
      LOOP AT ms_cur-t_link REFERENCE INTO lr_link.
        IF lr_link->target(1) = `#`.
          
          lv_target = zabaputil_cl_util_context=>c_escape_xml(
                                substring( val = lr_link->target
                                           off = 1 ) ).
          
          temp301 = |<hyperlink ref="{ lr_link->ref }" location="{ lv_target }"/>|.
          INSERT temp301 INTO TABLE lt_xml.
          CONTINUE.
        ENDIF.
        
        lv_link = 0.
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
          
          temp302 = |<Relationship Id="rId{ lv_rid + lv_link }" Type="{ cs_ns-rel }/hyperlink"|.
          INSERT temp302 INTO TABLE lt_rels.
          
          temp303 = | Target="{ lv_target }" TargetMode="External"/>|.
          INSERT temp303 INTO TABLE lt_rels.
        ENDIF.
        
        temp304 = |<hyperlink ref="{ lr_link->ref }" r:id="rId{ lv_rid + lv_link }"/>|.
        INSERT temp304 INTO TABLE lt_xml.
      ENDLOOP.
      INSERT `</hyperlinks>` INTO TABLE lt_xml.
    ENDIF.
    lv_rid = lv_rid + lines( lt_link ).

    " the pictures: a drawing with its own relationships and media
    
    lv_drawing = ``.
    IF ms_cur-t_image IS NOT INITIAL.
      mv_drawings = mv_drawings + 1.
      
      
      
      drawing_xml( EXPORTING t_image = ms_cur-t_image
                   IMPORTING drawing = lv_drawing_xml
                             rels    = lv_drawing_rels
                             t_media = lt_media ).
      
      CLEAR temp305.
      temp305-name = |xl/drawings/drawing{ mv_drawings }.xml|.
      temp305-ctype = `application/vnd.openxmlformats-officedocument.drawing+xml`.
      temp305-xml = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_drawing_xml ).
      INSERT temp305
             INTO TABLE ls_sheet-t_file.
      
      CLEAR temp306.
      temp306-name = |xl/drawings/_rels/drawing{ mv_drawings }.xml.rels|.
      temp306-xml = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_drawing_rels ).
      INSERT temp306
             INTO TABLE ls_sheet-t_file.
      INSERT LINES OF lt_media INTO TABLE ls_sheet-t_file.
      lv_rid = lv_rid + 1.
      
      temp307 = |<Relationship Id="rId{ lv_rid }" Type="{ cs_ns-rel }/drawing"|.
      INSERT temp307 INTO TABLE lt_rels.
      
      temp308 = | Target="../drawings/drawing{ mv_drawings }.xml"/>|.
      INSERT temp308 INTO TABLE lt_rels.
      lv_drawing = |<drawing r:id="rId{ lv_rid }"/>|.
    ENDIF.

    " the comments: their part, and the VML drawing Excel shows them with
    
    lv_legacy = ``.
    IF ms_cur-t_comment IS NOT INITIAL.
      mv_comments = mv_comments + 1.
      
      
      comments_xml( EXPORTING t_comment = ms_cur-t_comment
                              id        = mv_comments
                    IMPORTING comments  = lv_comments_xml
                              vml       = lv_vml ).
      
      CLEAR temp309.
      temp309-name = |xl/comments{ mv_comments }.xml|.
      temp309-ctype = |{ cs_ns-ct }.comments+xml|.
      temp309-xml = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_comments_xml ).
      INSERT temp309
             INTO TABLE ls_sheet-t_file.
      
      CLEAR temp310.
      temp310-name = |xl/drawings/vmlDrawing{ mv_comments }.vml|.
      temp310-xml = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_vml ).
      INSERT temp310
             INTO TABLE ls_sheet-t_file.
      lv_rid = lv_rid + 1.
      
      temp311 = |<Relationship Id="rId{ lv_rid }" Type="{ cs_ns-rel }/comments"|.
      INSERT temp311 INTO TABLE lt_rels.
      
      temp312 = | Target="../comments{ mv_comments }.xml"/>|.
      INSERT temp312 INTO TABLE lt_rels.
      lv_rid = lv_rid + 1.
      
      temp313 = |<Relationship Id="rId{ lv_rid }" Type="{ cs_ns-rel }/vmlDrawing"|.
      INSERT temp313 INTO TABLE lt_rels.
      
      temp314 = | Target="../drawings/vmlDrawing{ mv_comments }.vml"/>|.
      INSERT temp314 INTO TABLE lt_rels.
      lv_legacy = |<legacyDrawing r:id="rId{ lv_rid }"/>|.
    ENDIF.
    INSERT `</Relationships>` INTO TABLE lt_rels.

    " printing
    IF ls_layout-margin > 0.
      " inches, as the file counts
      
      temp315 = ls_layout-margin.
      
      lv_inch = temp315.
      lv_inch = round( val = lv_inch * 10 / 254
                       dec = 4 ).
      
      lv_margin = |{ lv_inch }|.
      
      temp316 = |<pageMargins left="{ lv_margin }" right="{ lv_margin }" top="{ lv_margin }"|.
      INSERT temp316 INTO TABLE lt_xml.
      
      temp317 = | bottom="{ lv_margin }" header="0.3" footer="0.3"/>|.
      INSERT temp317 INTO TABLE lt_xml.
    ENDIF.
    
    lv_setup = ``.
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
      
      temp318 = |<pageSetup{ lv_setup }/>|.
      INSERT temp318 INTO TABLE lt_xml.
    ENDIF.
    IF ls_layout-header_text IS NOT INITIAL OR ls_layout-footer_text IS NOT INITIAL.
      INSERT `<headerFooter>` INTO TABLE lt_xml.
      IF ls_layout-header_text IS NOT INITIAL.
        
        lv_text = ls_layout-header_text.
        IF lv_text NS `&L` AND lv_text NS `&C` AND lv_text NS `&R`.
          lv_text = `&C` && lv_text.
        ENDIF.
        lv_text = zabaputil_cl_util_context=>c_escape_xml( lv_text ).
        
        temp319 = |<oddHeader>{ lv_text }</oddHeader>|.
        INSERT temp319 INTO TABLE lt_xml.
      ENDIF.
      IF ls_layout-footer_text IS NOT INITIAL.
        lv_text = ls_layout-footer_text.
        IF lv_text NS `&L` AND lv_text NS `&C` AND lv_text NS `&R`.
          lv_text = `&C` && lv_text.
        ENDIF.
        lv_text = zabaputil_cl_util_context=>c_escape_xml( lv_text ).
        
        temp320 = |<oddFooter>{ lv_text }</oddFooter>|.
        INSERT temp320 INTO TABLE lt_xml.
      ENDIF.
      INSERT `</headerFooter>` INTO TABLE lt_xml.
    ENDIF.
    IF lv_drawing IS NOT INITIAL.
      INSERT lv_drawing INTO TABLE lt_xml.
    ENDIF.
    IF lv_legacy IS NOT INITIAL.
      INSERT lv_legacy INTO TABLE lt_xml.
    ENDIF.
    IF ms_cur-t_table IS NOT INITIAL.
      
      temp321 = |<tableParts count="{ lines( ms_cur-t_table ) }">|.
      INSERT temp321 INTO TABLE lt_xml.
      DO lines( ms_cur-t_table ) TIMES.
        
        temp322 = |<tablePart r:id="rId{ sy-index }"/>|.
        INSERT temp322 INTO TABLE lt_xml.
      ENDDO.
      INSERT `</tableParts>` INTO TABLE lt_xml.
    ENDIF.
    INSERT `</worksheet>` INTO TABLE lt_xml.

    " the names of the sheet: what is printed, and the rows on every page
    
    lv_sheet = sheet_quote( ms_cur-name ).
    IF ls_layout-print_area IS NOT INITIAL.
      
      CLEAR temp323.
      temp323-name = `_xlnm.Print_Area`.
      temp323-sheet = lv_index.
      temp323-ref = |{ lv_sheet }!{ area_text( val = area_parse( ls_layout-print_area ) absolute = abap_true ) }|.
      INSERT temp323 INTO TABLE mt_name.
    ENDIF.
    IF ls_layout-repeat_header = abap_true AND ms_cur-header_rows IS NOT INITIAL.
      
      CLEAR temp324.
      temp324-name = `_xlnm.Print_Titles`.
      temp324-sheet = lv_index.
      temp324-ref = |{ lv_sheet }!{ ms_cur-header_rows }|.
      INSERT temp324 INTO TABLE mt_name.
    ENDIF.

    ls_sheet-name = ms_cur-name.
    ls_sheet-hidden = ls_layout-hidden.
    ls_sheet-t_table = ms_cur-t_table.
    IF lv_rid > 0.
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

    DATA lv_text TYPE string.
      FIELD-SYMBOLS <temp325> LIKE LINE OF lt_part.
      DATA temp326 LIKE sy-tabix.
    DATA lv_part LIKE LINE OF lt_part.
      DATA lv_digits TYPE string.
      DATA lv_letters TYPE string.
      DATA lv_cell TYPE string.
    lv_text = to_upper( zabaputil_cl_util_context=>c_trim( val ) ).
    lv_text = replace( val  = lv_text
                       sub  = `$`
                       with = ``
                       occ  = 0 ).
    SPLIT lv_text AT `:` INTO TABLE lt_part.
    IF lines( lt_part ) = 1.
      
      
      temp326 = sy-tabix.
      READ TABLE lt_part INDEX 1 ASSIGNING <temp325>.
      sy-tabix = temp326.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      INSERT <temp325> INTO TABLE lt_part.
    ENDIF.
    IF lines( lt_part ) <> 2.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_RANGE - { val } is no range of cells (A1:D5)|.
    ENDIF.

    
    LOOP AT lt_part INTO lv_part.
      
      lv_digits = digits_only( lv_part ).
      lv_col = column_to_number( lv_part ).
      
      lv_letters = column_to_letters( lv_col ).
      
      lv_cell = |{ lv_letters }{ lv_digits }|.
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

    DATA lv_mark TYPE string.
    lv_mark = ``.
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
      DATA lv_text LIKE val-prompt_title.
    DATA lv_formula1 LIKE val-formula1.
      DATA lv_value LIKE LINE OF val-t_value.

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
      
      lv_text = val-prompt_title.
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
    
    lv_formula1 = val-formula1.
    IF val-type = `list` AND val-t_value IS NOT INITIAL.
      
      LOOP AT val-t_value INTO lv_value.
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

    DATA lv_len TYPE i.
    DATA lv_upper TYPE string.
    DATA lv_first TYPE string.
    DATA lv_ok TYPE abap_bool.
    DATA temp3 TYPE xsdboolean.
      DATA lv_digits TYPE string.
      DATA lv_col TYPE i.
      DATA lv_cell TYPE string.
    DATA temp327 LIKE LINE OF mt_name.
    DATA lr_name LIKE REF TO temp327.
      DATA lv_other TYPE string.
    lv_len = strlen( name ).
    IF lv_len = 0 OR lv_len > 255.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_NAME - { name } is no name Excel takes|.
    ENDIF.
    " a letter, an underscore or a backslash in front, then letters,
    " digits, underscores and periods
    
    lv_upper = to_upper( name ).
    
    lv_first = lv_upper(1).
    
    
    temp3 = boolc( lv_first CA `ABCDEFGHIJKLMNOPQRSTUVWXYZ_\` ).
    lv_ok = temp3.
    IF lv_ok = abap_true AND lv_upper NA ` !"#$%&'()*+,-/:;<=>?@[]^{|}~`.
      " a name that reads as a cell or as R and C of the R1C1 style is none
      
      lv_digits = digits_only( lv_upper ).
      
      lv_col = column_to_number( lv_upper ).
      
      lv_cell = |{ column_to_letters( lv_col ) }{ lv_digits }|.
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
    
    
    LOOP AT mt_name REFERENCE INTO lr_name WHERE sheet = sheet.
      
      lv_other = to_upper( lr_name->name ).
      IF lv_other = lv_upper.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |XLSX_NAME - the workbook has the name { name } already|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD props_xml.

    DATA lt_xml TYPE string_table.

    DATA lv_created LIKE ms_props-created.
    DATA lv_time TYPE string.
    DATA temp328 LIKE LINE OF lt_xml.
      DATA temp329 LIKE LINE OF lt_xml.
      DATA temp330 LIKE LINE OF lt_xml.
      DATA temp331 LIKE LINE OF lt_xml.
      DATA temp332 LIKE LINE OF lt_xml.
      DATA temp333 LIKE LINE OF lt_xml.
      DATA temp334 LIKE LINE OF lt_xml.
    DATA temp335 LIKE LINE OF lt_xml.
    DATA temp336 LIKE LINE OF lt_xml.
    DATA lv_app LIKE ms_props-application.
    DATA temp337 LIKE LINE OF lt_xml.
    DATA temp338 LIKE LINE OF lt_xml.
      DATA temp339 LIKE LINE OF lt_xml.
    lv_created = ms_props-created.
    IF lv_created IS INITIAL.
      lv_created = zabaputil_cl_util_context=>time_get_timestamp( ).
    ENDIF.
    
    lv_time = iso_by_stamp( lv_created ).

    
    temp328 = |{ cv_xml_head }<cp:coreProperties|.
    INSERT temp328 INTO TABLE lt_xml.
    INSERT ` xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"` INTO TABLE lt_xml.
    INSERT ` xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/"` INTO TABLE lt_xml.
    INSERT ` xmlns:dcmitype="http://purl.org/dc/dcmitype/"` INTO TABLE lt_xml.
    INSERT ` xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">` INTO TABLE lt_xml.
    IF ms_props-title IS NOT INITIAL.
      
      temp329 = |<dc:title>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-title ) }</dc:title>|.
      INSERT temp329 INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-subject IS NOT INITIAL.
      
      temp330 = |<dc:subject>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-subject ) }</dc:subject>|.
      INSERT temp330
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-creator IS NOT INITIAL.
      
      temp331 = |<dc:creator>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-creator ) }</dc:creator>|.
      INSERT temp331
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-keywords IS NOT INITIAL.
      
      temp332 = |<cp:keywords>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-keywords ) }</cp:keywords>|.
      INSERT temp332
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-description IS NOT INITIAL.
      
      temp333 = |<dc:description>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-description ) }</dc:description>|.
      INSERT temp333
             INTO TABLE lt_xml.
    ENDIF.
    IF ms_props-category IS NOT INITIAL.
      
      temp334 = |<cp:category>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-category ) }</cp:category>|.
      INSERT temp334
             INTO TABLE lt_xml.
    ENDIF.
    
    temp335 = |<dcterms:created xsi:type="dcterms:W3CDTF">{ lv_time }</dcterms:created>|.
    INSERT temp335 INTO TABLE lt_xml.
    
    temp336 = |<dcterms:modified xsi:type="dcterms:W3CDTF">{ lv_time }</dcterms:modified>|.
    INSERT temp336 INTO TABLE lt_xml.
    INSERT `</cp:coreProperties>` INTO TABLE lt_xml.
    core = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_xml ) ).

    CLEAR lt_xml.
    
    lv_app = ms_props-application.
    IF lv_app IS INITIAL.
      lv_app = `abap-util`.
    ENDIF.
    
    temp337 = |{ cv_xml_head }<Properties|.
    INSERT temp337 INTO TABLE lt_xml.
    INSERT ` xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"` INTO TABLE lt_xml.
    INSERT ` xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">` INTO TABLE lt_xml.
    
    temp338 = |<Application>{ zabaputil_cl_util_context=>c_escape_xml( lv_app ) }</Application>|.
    INSERT temp338 INTO TABLE lt_xml.
    IF ms_props-company IS NOT INITIAL.
      
      temp339 = |<Company>{ zabaputil_cl_util_context=>c_escape_xml( ms_props-company ) }</Company>|.
      INSERT temp339 INTO TABLE lt_xml.
    ENDIF.
    INSERT `</Properties>` INTO TABLE lt_xml.
    app = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_xml ) ).

  ENDMETHOD.

  METHOD iso_by_stamp.

    DATA lv_stamp TYPE p LENGTH 8 DECIMALS 0.
    DATA lv_text TYPE string.

    lv_stamp = val.
    
    lv_text = |{ lv_stamp }|.
    IF strlen( lv_text ) <> 14 OR lv_text CN cv_digits.
      lv_text = `19000101000000`.
    ENDIF.
    result = |{ lv_text(4) }-{ lv_text+4(2) }-{ lv_text+6(2) }T{ lv_text+8(2) }:{ lv_text+10(2) }:{ lv_text+12(2) }Z|.

  ENDMETHOD.

  METHOD fields_get.

    DATA ls_field TYPE ty_s_field.
    DATA lv_kind TYPE string.
    DATA lt_comp TYPE abap_component_tab.
    DATA temp340 LIKE LINE OF lt_comp.
    DATA lr_comp LIKE REF TO temp340.
      DATA lv_name TYPE string.

    CLEAR elementary.
    CLEAR result.

    
    lv_kind = zabaputil_cl_util_context=>rtti_get_type_kind( row ).
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
    
    lt_comp = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( row ).
    
    
    LOOP AT lt_comp REFERENCE INTO lr_comp.
      IF lr_comp->type->kind <> zabaputil_cl_util_context=>cv_typedescr_kind_elem.
        CONTINUE.
      ENDIF.
      CLEAR ls_field.
      ls_field-name = lr_comp->name.
      
      lv_name = lr_comp->type->get_relative_name( ).
      ls_field-cat = category_get( type_kind = lr_comp->type->type_kind
                                   name      = lv_name ).
      IF ls_field-cat IS INITIAL.
        CONTINUE.
      ENDIF.
      IF lr_comp->type->is_ddic_type( ) = abap_true.
        ls_field-rollname = lv_name.
      ENDIF.
      ls_field-decimals = lr_comp->type->decimals.
      INSERT ls_field INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD category_get.

    DATA lv_name TYPE string.
    lv_name = to_upper( name ).

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
    ELSEIF type_kind = `p`.
      " utclong (cl_abap_typedescr=>typekind_utclong, a constant 7.02 does
      " not have): a date with its time
      result = cs_cat-timestamp.
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
    DATA lt_column LIKE layout-t_column.
        DATA temp341 TYPE zabaputil_cl_util_xlsx=>ty_s_column.
    DATA temp342 LIKE LINE OF lt_column.
    DATA lr_column LIKE REF TO temp342.
      DATA lv_position TYPE i.
      DATA lv_name TYPE string.
            DATA ls_text TYPE zabaputil_cl_util_context=>ty_s_data_element_text.
        DATA lv_header LIKE ls_col-header.
        DATA lv_suffix TYPE i.
          DATA lv_upper TYPE string.
          DATA temp343 LIKE sy-subrc.
      DATA lv_code TYPE string.
        DATA lv_link TYPE string.
        DATA ls_link_field TYPE zabaputil_cl_util_xlsx=>ty_s_field.
        DATA ls_link_style LIKE lr_column->style.
        DATA lv_key_field TYPE string.
            DATA lt_fix TYPE zabaputil_cl_util_context=>ty_t_fix_val.
            FIELD-SYMBOLS <fix> LIKE LINE OF lt_fix.
              FIELD-SYMBOLS <low> TYPE any.
              FIELD-SYMBOLS <text> TYPE any.
              DATA temp344 TYPE zabaputil_cl_util_xlsx=>ty_s_fix.
    lt_column = layout-t_column.
    IF lt_column IS INITIAL.
      LOOP AT t_field INTO ls_field.
        
        CLEAR temp341.
        temp341-name = ls_field-name.
        INSERT temp341 INTO TABLE lt_column.
      ENDLOOP.
    ENDIF.
    IF lines( lt_column ) + offset > cv_max_col.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_TOO_MANY_COLUMNS - a sheet holds { cv_max_col } columns|.
    ENDIF.

    
    
    LOOP AT lt_column REFERENCE INTO lr_column.
      
      lv_position = lines( result ) + 1.
      
      lv_name = zabaputil_cl_util_context=>c_trim_upper( lr_column->name ).

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
            
            ls_text = zabaputil_cl_util_context=>rtti_get_data_element_texts( ls_field-rollname ).
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
        
        lv_header = ls_col-header.
        
        lv_suffix = 1.
        DO.
          " in a variable: a function in the key of a table expression is
          " read as a method call on 7.02
          
          lv_upper = to_upper( lv_header ).
          
          READ TABLE lt_taken WITH KEY table_line = lv_upper TRANSPORTING NO FIELDS.
          temp343 = sy-subrc.
          IF NOT temp343 = 0.
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
      
      lv_code = ``.
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
        
        lv_link = zabaputil_cl_util_context=>c_trim_upper( lr_column->link ).
        
        ls_link_field = field_find( t_field = t_field
                                          name    = lv_link ).
        IF ls_link_field IS INITIAL.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_COLUMN_UNKNOWN - the row has no elementary component { lv_link } for a link|.
        ENDIF.
        ls_col-link = ls_link_field-name.
        
        ls_link_style = lr_column->style.
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
      ls_col-rule = lr_column->rule.
      IF ( lr_column->currency IS NOT INITIAL OR lr_column->unit IS NOT INITIAL ) AND ls_col-cat = cs_cat-number.
        ls_col-currency = zabaputil_cl_util_context=>c_trim_upper( lr_column->currency ).
        ls_col-unit = zabaputil_cl_util_context=>c_trim_upper( lr_column->unit ).
        
        lv_key_field = ls_col-currency && ls_col-unit.
        IF field_find( t_field = t_field
                       name    = lv_key_field ) IS INITIAL.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_COLUMN_UNKNOWN - the row has no elementary component { lv_key_field } for a currency or unit|.
        ENDIF.
        ls_col-decimals = ls_field-decimals.
        ls_col-format = lr_column->format.
        ls_col-style_base = lr_column->style.
      ENDIF.
      ls_col-conv_exit = zabaputil_cl_util_context=>c_trim_upper( lr_column->conv_exit ).
      IF lr_column->fixed_value_text = abap_true AND ls_field-rollname IS NOT INITIAL.
        TRY.
            " the value first, its text third - by position, as a context
            " class of its own may name the components otherwise
            
            lt_fix = zabaputil_cl_util_context=>rtti_get_t_ddic_fixed_values( ls_field-rollname ).
            
            LOOP AT lt_fix ASSIGNING <fix>.
              
              ASSIGN COMPONENT 1 OF STRUCTURE <fix> TO <low>.
              
              ASSIGN COMPONENT 3 OF STRUCTURE <fix> TO <text>.
              
              CLEAR temp344.
              temp344-low = <low>.
              temp344-text = <text>.
              INSERT temp344 INTO TABLE ls_col-t_fix.
            ENDLOOP.
          CATCH cx_root ##NO_HANDLER.
            " no fixed values to be had - the values as they are
        ENDTRY.
      ENDIF.

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
    DATA lv_name TYPE string.

    CLEAR name.
    CLEAR code.

    " the names a table part knows, and the numbers SUBTOTAL( ) takes for
    " them - the ones from 101 on, which leave out a hidden row
    " in a variable: 7.02 takes no function as the operand of a CASE
    
    lv_name = to_lower( condense( val ) ).
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
    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
      DATA lv_letters TYPE string.
      DATA lv_rest TYPE string.
    DATA lv_base LIKE result.
    DATA lv_suffix TYPE i.
      DATA lv_upper TYPE string.
      DATA temp345 LIKE sy-subrc.
    lv_len = strlen( val ).
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = val
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
      
      lv_letters = result(lv_pos).
      
      lv_rest = result+lv_pos.
      IF lv_rest CO cv_digits AND lv_letters CO `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`.
        result = `_` && result.
      ENDIF.
    ENDIF.
    IF strlen( result ) > 200.
      result = result(200).
    ENDIF.

    " once in the workbook, whatever its case
    
    lv_base = result.
    
    lv_suffix = 1.
    DO.
      
      lv_upper = to_upper( result ).
      
      READ TABLE mt_table_name WITH KEY table_line = lv_upper TRANSPORTING NO FIELDS.
      temp345 = sy-subrc.
      IF NOT temp345 = 0.
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
      DATA temp346 TYPE zabaputil_cl_util_xlsx=>ty_s_font.
      DATA temp347 TYPE zabaputil_cl_util_xlsx=>ty_s_font.
      DATA temp348 TYPE zabaputil_cl_util_xlsx=>ty_s_border.
      DATA lv_index TYPE i.
      DATA lv_code LIKE LINE OF mt_numfmt.
    DATA temp349 LIKE LINE OF mt_xf.
    DATA lr_xf LIKE REF TO temp349.

    " index 0, the format of every cell that names none - and the font,
    " the fills and the border Excel expects to find first
    IF mt_xf IS INITIAL.
      INSERT ls_xf INTO TABLE mt_xf.
    ENDIF.
    IF mt_font IS INITIAL.
      
      CLEAR temp346.
      temp346-size = 11.
      temp346-name = `Calibri`.
      INSERT temp346 INTO TABLE mt_font.
      
      CLEAR temp347.
      temp347-bold = abap_true.
      temp347-size = 11.
      temp347-name = `Calibri`.
      INSERT temp347 INTO TABLE mt_font.
      
      CLEAR temp348.
      INSERT temp348 INTO TABLE mt_border.
    ENDIF.

    ls_xf-numfmt = id.
    IF code IS NOT INITIAL.
      " a format of the caller: a custom one, named from 164 on. Looked
      " up in a loop: line_index( ) of a row that is not there comes out
      " of the 7.02 downport as the index of an earlier read
      
      lv_index = 0.
      
      LOOP AT mt_numfmt INTO lv_code.
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
    ls_xf-unlocked = style-unlocked.

    
    
    LOOP AT mt_xf REFERENCE INTO lr_xf.
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

    DATA lv_index TYPE i.
    lv_index = style_get( id    = id
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

    DATA temp350 LIKE LINE OF mt_font.
    DATA lr_font LIKE REF TO temp350.
    LOOP AT mt_font REFERENCE INTO lr_font.
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
    DATA lv_fill LIKE LINE OF mt_fill.
    LOOP AT mt_fill INTO lv_fill.
      IF lv_fill = val.
        result = sy-tabix + 1.
        RETURN.
      ENDIF.
    ENDLOOP.
    INSERT val INTO TABLE mt_fill.
    result = lines( mt_fill ) + 1.

  ENDMETHOD.

  METHOD border_get.

    DATA temp351 LIKE LINE OF mt_border.
    DATA lr_border LIKE REF TO temp351.
    LOOP AT mt_border REFERENCE INTO lr_border.
      IF lr_border->* = val.
        result = sy-tabix - 1.
        RETURN.
      ENDIF.
    ENDLOOP.
    INSERT val INTO TABLE mt_border.
    result = lines( mt_border ) - 1.

  ENDMETHOD.

  METHOD color_get.

    DATA lv_color TYPE string.
    lv_color = to_upper( zabaputil_cl_util_context=>c_trim( val ) ).
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

    DATA lv_name TYPE string.
    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
    DATA lv_base LIKE result.
    DATA lv_suffix TYPE i.
      DATA lv_taken LIKE abap_false.
      DATA temp352 LIKE LINE OF mt_out_sheet.
      DATA lr_sheet LIKE REF TO temp352.
      DATA lv_tail TYPE string.
      DATA lv_keep TYPE i.
    lv_name = zabaputil_cl_util_context=>c_trim( val ).
    IF lv_name IS INITIAL.
      lv_name = |Sheet{ lines( mt_out_sheet ) + 1 }|.
    ENDIF.

    " what Excel does not take in a sheet name
    
    lv_len = strlen( lv_name ).
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = lv_name
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
    
    lv_base = result.
    
    lv_suffix = 1.
    DO.
      
      lv_taken = abap_false.
      
      
      LOOP AT mt_out_sheet REFERENCE INTO lr_sheet.
        IF to_upper( lr_sheet->name ) = to_upper( result ).
          lv_taken = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_taken = abap_false.
        EXIT.
      ENDIF.
      lv_suffix = lv_suffix + 1.
      
      lv_tail = | ({ lv_suffix })|.
      
      lv_keep = nmin( val1 = strlen( lv_base )
                            val2 = 31 - strlen( lv_tail ) ).
      result = substring( val = lv_base
                          len = lv_keep ) && lv_tail.
    ENDDO.

  ENDMETHOD.

  METHOD cell_text.
    DATA lv_text TYPE string.
      FIELD-SYMBOLS <sst> TYPE ty_s_sst.
        DATA temp353 TYPE zabaputil_cl_util_xlsx=>ty_s_sst.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    " c_escape_xml drops the control characters XML forbids (Excel
    " "repairs" a workbook with one). `_x` in front of four hex digits is
    " how a file names a character by its number, so a text that reads like
    " that has its underscore named that way
    
    lv_text = zabaputil_cl_util_context=>c_escape_xml( val ).
    IF lv_text CS `_x`.
      lv_text = replace( val  = lv_text
                         sub  = `_x`
                         with = `_x005F_x`
                         occ  = 0 ).
    ENDIF.
    " a text of the shared strings: by its index, each text once
    IF mv_sst = abap_true.
      
      UNASSIGN <sst>.
      READ TABLE mt_sst_index WITH KEY text = lv_text ASSIGNING <sst>.
      IF <sst> IS NOT ASSIGNED.
        INSERT lv_text INTO TABLE mt_sst.
        
        CLEAR temp353.
        temp353-text = lv_text.
        temp353-index = lines( mt_sst ) - 1.
        INSERT temp353 INTO TABLE mt_sst_index ASSIGNING <sst>.
      ENDIF.
      result = |<c r="{ address }"{ style } t="s"><v>{ <sst>-index }</v></c>|.
      RETURN.
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

    DATA lv_totals TYPE string.
    DATA temp354 LIKE LINE OF lt_xml.
    DATA temp355 LIKE LINE OF lt_xml.
      DATA temp356 LIKE LINE OF lt_xml.
    DATA temp357 LIKE LINE OF lt_xml.
    DATA temp358 LIKE LINE OF t_col.
    DATA lr_col LIKE REF TO temp358.
      DATA lv_total TYPE string.
      DATA lv_column TYPE string.
        DATA temp359 LIKE LINE OF lt_xml.
      DATA lv_formula TYPE string.
      DATA temp360 LIKE LINE OF lt_xml.
      DATA temp361 LIKE LINE OF lt_xml.
    DATA lv_rows TYPE string.
    DATA lv_columns TYPE string.
    DATA lv_style TYPE string.
    DATA temp362 LIKE LINE OF lt_xml.
    DATA temp363 LIKE LINE OF lt_xml.
    lv_totals = ` totalsRowShown="0"`.
    IF totals = abap_true.
      lv_totals = ` totalsRowCount="1"`.
    ENDIF.
    " one piece per INSERT, here and wherever a part is put together: the
    " 7.02 downport writes the operands of a `&&` in an INSERT on one line,
    " and a line of more than 255 characters is cut on import
    
    temp354 = |{ cv_xml_head }<table xmlns="{ cs_ns-main }" id="{ id }" name="{ name }" displayName="{ name }"|.
    INSERT temp354
           INTO TABLE lt_xml.
    
    temp355 = | ref="{ range }"{ lv_totals }>|.
    INSERT temp355 INTO TABLE lt_xml.
    IF filter IS NOT INITIAL.
      
      temp356 = |<autoFilter ref="{ filter }"/>|.
      INSERT temp356 INTO TABLE lt_xml.
    ENDIF.
    
    temp357 = |<tableColumns count="{ lines( t_col ) }">|.
    INSERT temp357 INTO TABLE lt_xml.
    
    
    LOOP AT t_col REFERENCE INTO lr_col.
      
      lv_total = ``.
      IF lr_col->total_fn IS NOT INITIAL.
        lv_total = | totalsRowFunction="{ lr_col->total_fn }"|.
      ELSEIF lr_col->total_label IS NOT INITIAL.
        lv_total = | totalsRowLabel="{ zabaputil_cl_util_context=>c_escape_xml( lr_col->total_label ) }"|.
      ENDIF.
      
      lv_column = zabaputil_cl_util_context=>c_escape_xml( lr_col->header ).
      IF lr_col->formula IS INITIAL.
        
        temp359 = |<tableColumn id="{ sy-tabix }" name="{ lv_column }"{ lv_total }/>|.
        INSERT temp359 INTO TABLE lt_xml.
        CONTINUE.
      ENDIF.
      " a formula column: Excel fills a row added to the table with it -
      " written for the first row of the table
      
      lv_formula = replace( val  = lr_col->formula
                                  sub  = `{row}`
                                  with = |{ first_row }|
                                  occ  = 0 ).
      lv_formula = zabaputil_cl_util_context=>c_escape_xml( lv_formula ).
      
      temp360 = |<tableColumn id="{ sy-tabix }" name="{ lv_column }"{ lv_total }>|.
      INSERT temp360 INTO TABLE lt_xml.
      
      temp361 = |<calculatedColumnFormula>{ lv_formula }</calculatedColumnFormula></tableColumn>|.
      INSERT temp361 INTO TABLE lt_xml.
    ENDLOOP.
    
    lv_rows = `1`.
    IF layout-no_row_stripes = abap_true.
      lv_rows = `0`.
    ENDIF.
    
    lv_columns = `0`.
    IF layout-column_stripes = abap_true.
      lv_columns = `1`.
    ENDIF.
    
    lv_style = zabaputil_cl_util_context=>c_escape_xml( layout-table_style ).
    
    temp362 = |</tableColumns><tableStyleInfo name="{ lv_style }" showFirstColumn="0" showLastColumn="0"|.
    INSERT temp362
           INTO TABLE lt_xml.
    
    temp363 = | showRowStripes="{ lv_rows }" showColumnStripes="{ lv_columns }"/></table>|.
    INSERT temp363 INTO TABLE lt_xml.
    result = concat_lines_of( lt_xml ).

  ENDMETHOD.

  METHOD styles_xml.

    DATA lt_xml TYPE string_table.
    DATA temp364 LIKE LINE OF lt_xml.
      DATA temp365 LIKE LINE OF lt_xml.
      DATA lv_code LIKE LINE OF mt_numfmt.
        DATA lv_id TYPE i.
        DATA temp366 LIKE LINE OF lt_xml.
    DATA temp367 LIKE LINE OF lt_xml.
    DATA temp368 LIKE LINE OF mt_font.
    DATA lr_font LIKE REF TO temp368.
      DATA lv_font TYPE string.
      DATA lv_name TYPE string.
      DATA temp369 LIKE LINE OF lt_xml.
    DATA temp370 LIKE LINE OF lt_xml.
    DATA lv_fill LIKE LINE OF mt_fill.
      DATA temp371 LIKE LINE OF lt_xml.
    DATA temp372 LIKE LINE OF lt_xml.
    DATA temp373 LIKE LINE OF mt_border.
    DATA lr_border LIKE REF TO temp373.
      DATA lv_color TYPE string.
      DATA lv_border TYPE string.
      DATA temp374 LIKE LINE OF lt_xml.
    DATA temp375 LIKE LINE OF lt_xml.
    DATA temp376 LIKE LINE OF mt_xf.
    DATA lr_xf LIKE REF TO temp376.
      DATA lv_apply TYPE string.
      DATA lv_align TYPE string.
      DATA lv_xf TYPE string.
      DATA lv_inner TYPE string.
        DATA temp377 LIKE LINE OF lt_xml.
        DATA temp378 LIKE LINE OF lt_xml.
      DATA temp379 LIKE LINE OF lt_xml.

    IF mt_xf IS INITIAL.
      style_get( ).
    ENDIF.

    
    temp364 = |{ cv_xml_head }<styleSheet xmlns="{ cs_ns-main }">|.
    INSERT temp364 INTO TABLE lt_xml.
    IF mt_numfmt IS NOT INITIAL.
      
      temp365 = |<numFmts count="{ lines( mt_numfmt ) }">|.
      INSERT temp365 INTO TABLE lt_xml.
      
      LOOP AT mt_numfmt INTO lv_code.
        
        lv_id = cs_numfmt-custom + sy-tabix - 1.
        
        temp366 = |<numFmt numFmtId="{ lv_id }" formatCode="{ zabaputil_cl_util_context=>c_escape_xml( lv_code ) }"/>|.
        INSERT temp366
               INTO TABLE lt_xml.
      ENDLOOP.
      INSERT `</numFmts>` INTO TABLE lt_xml.
    ENDIF.

    " font 0 the default, font 1 the bold one of a header row
    
    temp367 = |<fonts count="{ lines( mt_font ) }">|.
    INSERT temp367 INTO TABLE lt_xml.
    
    
    LOOP AT mt_font REFERENCE INTO lr_font.
      
      lv_font = `<font>`.
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
      
      lv_name = zabaputil_cl_util_context=>c_escape_xml( lr_font->name ).
      
      temp369 = |{ lv_font }<name val="{ lv_name }"/><family val="2"/></font>|.
      INSERT temp369 INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</fonts>` INTO TABLE lt_xml.

    " the two fills Excel expects to find, then the colours of the cells
    
    temp370 = |<fills count="{ lines( mt_fill ) + 2 }"><fill><patternFill patternType="none"/></fill>|.
    INSERT temp370
           INTO TABLE lt_xml.
    INSERT `<fill><patternFill patternType="gray125"/></fill>` INTO TABLE lt_xml.
    
    LOOP AT mt_fill INTO lv_fill.
      
      temp371 = |<fill><patternFill patternType="solid"><fgColor rgb="{ lv_fill }"/>|.
      INSERT temp371 INTO TABLE lt_xml.
      INSERT `<bgColor indexed="64"/></patternFill></fill>` INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</fills>` INTO TABLE lt_xml.

    
    temp372 = |<borders count="{ lines( mt_border ) }">|.
    INSERT temp372 INTO TABLE lt_xml.
    
    
    LOOP AT mt_border REFERENCE INTO lr_border.
      
      lv_color = ``.
      IF lr_border->color IS NOT INITIAL.
        lv_color = |<color rgb="{ lr_border->color }"/>|.
      ENDIF.
      
      lv_border = `<border>`.
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
      
      temp374 = |{ lv_border }<diagonal/></border>|.
      INSERT temp374 INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</borders>` INTO TABLE lt_xml.

    INSERT `<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>`
           INTO TABLE lt_xml.
    
    temp375 = |<cellXfs count="{ lines( mt_xf ) }">|.
    INSERT temp375 INTO TABLE lt_xml.
    
    
    LOOP AT mt_xf REFERENCE INTO lr_xf.
      
      lv_apply = ``.
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
      
      lv_align = ``.
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
      
      lv_xf = |<xf numFmtId="{ lr_xf->numfmt }" fontId="{ lr_xf->font }" fillId="{ lr_xf->fill }"|.
      lv_xf = |{ lv_xf } borderId="{ lr_xf->border }" xfId="0"{ lv_apply }|.
      
      lv_inner = ``.
      IF lv_align IS NOT INITIAL.
        lv_xf = |{ lv_xf } applyAlignment="1"|.
        lv_inner = |<alignment{ lv_align }/>|.
      ENDIF.
      IF lr_xf->unlocked = abap_true.
        lv_xf = |{ lv_xf } applyProtection="1"|.
        lv_inner = |{ lv_inner }<protection locked="0"/>|.
      ENDIF.
      IF lv_inner IS INITIAL.
        
        temp377 = |{ lv_xf }/>|.
        INSERT temp377 INTO TABLE lt_xml.
      ELSE.
        
        temp378 = |{ lv_xf }>{ lv_inner }</xf>|.
        INSERT temp378 INTO TABLE lt_xml.
      ENDIF.
    ENDLOOP.
    INSERT `</cellXfs><cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>`
           INTO TABLE lt_xml.
    " the formats of conditional formatting
    IF mt_dxf IS NOT INITIAL.
      
      temp379 = |<dxfs count="{ lines( mt_dxf ) }">|.
      INSERT temp379 INTO TABLE lt_xml.
      INSERT LINES OF mt_dxf INTO TABLE lt_xml.
      INSERT `</dxfs>` INTO TABLE lt_xml.
    ENDIF.
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
    DATA lv_vml   TYPE abap_bool.
    DATA lv_png   TYPE abap_bool.
    DATA lv_jpeg  TYPE abap_bool.
      DATA temp380 TYPE zabaputil_cl_util_xlsx=>ty_s_out_sheet.
    DATA lv_active TYPE i.
    DATA temp381 LIKE LINE OF mt_out_sheet.
    DATA lr_sheet LIKE REF TO temp381.
    DATA temp382 LIKE LINE OF lt_types.
    DATA temp383 LIKE LINE OF lt_types.
    DATA temp384 LIKE LINE OF lt_types.
    DATA temp385 LIKE LINE OF lt_book.
      DATA lv_lock TYPE string.
      DATA temp386 LIKE LINE OF lt_book.
      DATA temp387 LIKE LINE OF lt_book.
    DATA temp388 LIKE LINE OF lt_rels.
      DATA lv_index LIKE sy-tabix.
      DATA temp389 LIKE LINE OF lt_types.
      DATA lv_name TYPE string.
      DATA lv_state TYPE string.
      DATA temp390 LIKE LINE OF lt_book.
      DATA temp391 LIKE LINE OF lt_rels.
      DATA temp392 LIKE LINE OF lt_rels.
      DATA temp393 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA temp394 LIKE LINE OF lr_sheet->t_table.
      DATA lr_table LIKE REF TO temp394.
        DATA temp395 LIKE LINE OF lt_types.
        DATA temp396 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
        DATA temp397 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA temp398 LIKE LINE OF lr_sheet->t_file.
      DATA lr_file LIKE REF TO temp398.
        DATA temp399 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
          DATA temp400 LIKE LINE OF lt_types.
      DATA temp401 TYPE string_table.
      DATA temp74 LIKE LINE OF temp401.
      DATA lt_sst LIKE temp401.
      DATA temp403 LIKE LINE OF lt_sst.
      DATA lv_sst LIKE LINE OF mt_sst.
        DATA temp404 LIKE LINE OF lt_sst.
      DATA temp405 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA temp406 LIKE LINE OF lt_types.
      DATA temp407 LIKE LINE OF lt_rels.
      DATA temp408 LIKE LINE OF lt_rels.
      DATA temp409 LIKE LINE OF mt_name.
      DATA lr_name LIKE REF TO temp409.
        DATA lv_local TYPE string.
        DATA lv_ref TYPE string.
        DATA temp410 LIKE LINE OF lt_book.
    DATA temp411 LIKE LINE OF lt_rels.
    DATA temp412 LIKE LINE OF lt_root.
    DATA temp413 LIKE LINE OF lt_root.
      DATA temp414 LIKE LINE OF lt_root.
      DATA temp415 LIKE LINE OF lt_root.
      DATA temp416 LIKE LINE OF lt_types.
      DATA temp417 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA temp418 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
    DATA temp419 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
    DATA temp420 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
    DATA temp421 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
    DATA temp422 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
    DATA temp423 TYPE zabaputil_cl_util_context=>ty_s_zip_file.

    IF mv_loaded = abap_true.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_READ_ONLY - a workbook that was read is not written again`.
    ENDIF.
    sheet_close( ).
    " a workbook without a sheet is none Excel opens
    IF mt_out_sheet IS INITIAL.
      
      CLEAR temp380.
      temp380-name = `Sheet1`.
      temp380-xml = zabaputil_cl_util_context=>conv_get_xstring_by_string( |{ cv_xml_head }<worksheet xmlns="{ cs_ns-main }"><sheetData/></worksheet>| ).
      INSERT temp380
             INTO TABLE mt_out_sheet.
    ENDIF.
    " nor one that hides every sheet - the first one Excel shows
    
    lv_active = -1.
    
    
    LOOP AT mt_out_sheet REFERENCE INTO lr_sheet WHERE hidden = abap_false.
      lv_active = sy-tabix - 1.
      EXIT.
    ENDLOOP.
    IF lv_active < 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_ALL_HIDDEN - a workbook shows one sheet at least, every sheet is hidden`.
    ENDIF.

    
    temp382 = |{ cv_xml_head }<Types xmlns="{ cs_ns-types }">|.
    INSERT temp382 INTO TABLE lt_types.
    INSERT `<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>`
           INTO TABLE lt_types.
    INSERT `<Default Extension="xml" ContentType="application/xml"/>` INTO TABLE lt_types.
    
    temp383 = |<Override PartName="/xl/workbook.xml" ContentType="{ cs_ns-ct }.sheet.main+xml"/>|.
    INSERT temp383
           INTO TABLE lt_types.
    
    temp384 = |<Override PartName="/xl/styles.xml" ContentType="{ cs_ns-ct }.styles+xml"/>|.
    INSERT temp384
           INTO TABLE lt_types.
    
    temp385 = |{ cv_xml_head }<workbook xmlns="{ cs_ns-main }" xmlns:r="{ cs_ns-rel }">|.
    INSERT temp385 INTO TABLE lt_book.
    IF mv_book_lock = abap_true.
      
      lv_lock = `<workbookProtection lockStructure="1"`.
      IF mv_book_hash IS NOT INITIAL.
        lv_lock = |{ lv_lock } workbookPassword="{ mv_book_hash }"|.
      ENDIF.
      
      temp386 = |{ lv_lock }/>|.
      INSERT temp386 INTO TABLE lt_book.
    ENDIF.
    IF lv_active = 0.
      INSERT `<bookViews><workbookView/></bookViews><sheets>` INTO TABLE lt_book.
    ELSE.
      
      temp387 = |<bookViews><workbookView activeTab="{ lv_active }"/></bookViews><sheets>|.
      INSERT temp387 INTO TABLE lt_book.
    ENDIF.
    
    temp388 = |{ cv_xml_head }<Relationships xmlns="{ cs_ns-pkg_rel }">|.
    INSERT temp388 INTO TABLE lt_rels.

    LOOP AT mt_out_sheet REFERENCE INTO lr_sheet.
      
      lv_index = sy-tabix.
      
      temp389 = |<Override PartName="/xl/worksheets/sheet{ lv_index }.xml" ContentType="{ cs_ns-ct }.worksheet+xml"/>|.
      INSERT temp389
             INTO TABLE lt_types.
      
      lv_name = zabaputil_cl_util_context=>c_escape_xml( lr_sheet->name ).
      
      lv_state = ``.
      IF lr_sheet->hidden = abap_true.
        lv_state = ` state="hidden"`.
      ENDIF.
      
      temp390 = |<sheet name="{ lv_name }" sheetId="{ lv_index }"{ lv_state } r:id="rId{ lv_index }"/>|.
      INSERT temp390 INTO TABLE lt_book.
      
      temp391 = |<Relationship Id="rId{ lv_index }" Type="{ cs_ns-rel }/worksheet"|.
      INSERT temp391 INTO TABLE lt_rels.
      
      temp392 = | Target="worksheets/sheet{ lv_index }.xml"/>|.
      INSERT temp392 INTO TABLE lt_rels.
      
      CLEAR temp393.
      temp393-name = |xl/worksheets/sheet{ lv_index }.xml|.
      temp393-content = lr_sheet->xml.
      INSERT temp393 INTO TABLE lt_file.

      
      
      LOOP AT lr_sheet->t_table REFERENCE INTO lr_table.
        
        temp395 = |<Override PartName="/xl/tables/table{ lr_table->id }.xml" ContentType="{ cs_ns-ct }.table+xml"/>|.
        INSERT temp395
               INTO TABLE lt_types.
        
        CLEAR temp396.
        temp396-name = |xl/tables/table{ lr_table->id }.xml|.
        temp396-content = lr_table->xml.
        INSERT temp396 INTO TABLE lt_file.
      ENDLOOP.
      IF lr_sheet->rels IS NOT INITIAL.
        
        CLEAR temp397.
        temp397-name = |xl/worksheets/_rels/sheet{ lv_index }.xml.rels|.
        temp397-content = lr_sheet->rels.
        INSERT temp397 INTO TABLE lt_file.
      ENDIF.
      " comments, drawings, pictures - a part of a kind of its own named
      " by its content type, the others by their extension
      
      
      LOOP AT lr_sheet->t_file REFERENCE INTO lr_file.
        
        CLEAR temp399.
        temp399-name = lr_file->name.
        temp399-content = lr_file->xml.
        INSERT temp399 INTO TABLE lt_file.
        IF lr_file->ctype IS NOT INITIAL.
          
          temp400 = |<Override PartName="/{ lr_file->name }" ContentType="{ lr_file->ctype }"/>|.
          INSERT temp400 INTO TABLE lt_types.
        ELSEIF zabaputil_cl_util_context=>c_ends_with( val    = lr_file->name
                                                       suffix = `.vml` ) = abap_true.
          lv_vml = abap_true.
        ELSEIF zabaputil_cl_util_context=>c_ends_with( val    = lr_file->name
                                                       suffix = `.png` ) = abap_true.
          lv_png = abap_true.
        ELSEIF zabaputil_cl_util_context=>c_ends_with( val    = lr_file->name
                                                       suffix = `.jpeg` ) = abap_true.
          lv_jpeg = abap_true.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF lv_vml = abap_true.
      INSERT `<Default Extension="vml" ContentType="application/vnd.openxmlformats-officedocument.vmlDrawing"/>`
             INTO lt_types INDEX 2.
    ENDIF.
    IF lv_png = abap_true.
      INSERT `<Default Extension="png" ContentType="image/png"/>` INTO lt_types INDEX 2.
    ENDIF.
    IF lv_jpeg = abap_true.
      INSERT `<Default Extension="jpeg" ContentType="image/jpeg"/>` INTO lt_types INDEX 2.
    ENDIF.

    " the texts of the cells, each once
    IF mv_sst = abap_true.
      
      CLEAR temp401.
      
      temp74 = |{ cv_xml_head }<sst xmlns="{ cs_ns-main }"|.
      INSERT temp74 INTO TABLE temp401.
      
      lt_sst = temp401.
      
      temp403 = | count="{ lines( mt_sst ) }" uniqueCount="{ lines( mt_sst ) }">|.
      INSERT temp403 INTO TABLE lt_sst.
      
      LOOP AT mt_sst INTO lv_sst.
        
        temp404 = |<si><t xml:space="preserve">{ lv_sst }</t></si>|.
        INSERT temp404 INTO TABLE lt_sst.
      ENDLOOP.
      INSERT `</sst>` INTO TABLE lt_sst.
      
      CLEAR temp405.
      temp405-name = `xl/sharedStrings.xml`.
      temp405-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_sst ) ).
      INSERT temp405
             INTO TABLE lt_file.
      
      temp406 = |<Override PartName="/xl/sharedStrings.xml" ContentType="{ cs_ns-ct }.sharedStrings+xml"/>|.
      INSERT temp406
             INTO TABLE lt_types.
      
      temp407 = |<Relationship Id="rId{ lines( mt_out_sheet ) + 2 }" Type="{ cs_ns-rel }/sharedStrings"|.
      INSERT temp407 INTO TABLE lt_rels.
      
      temp408 = | Target="sharedStrings.xml"/>|.
      INSERT temp408 INTO TABLE lt_rels.
    ENDIF.

    INSERT `</sheets>` INTO TABLE lt_book.
    IF mt_name IS NOT INITIAL.
      INSERT `<definedNames>` INTO TABLE lt_book.
      
      
      LOOP AT mt_name REFERENCE INTO lr_name.
        
        lv_local = ``.
        IF lr_name->sheet > 0.
          lv_local = | localSheetId="{ lr_name->sheet - 1 }"|.
        ENDIF.
        
        lv_ref = zabaputil_cl_util_context=>c_escape_xml( lr_name->ref ).
        
        temp410 = |<definedName name="{ lr_name->name }"{ lv_local }>{ lv_ref }</definedName>|.
        INSERT temp410 INTO TABLE lt_book.
      ENDLOOP.
      INSERT `</definedNames>` INTO TABLE lt_book.
    ENDIF.
    IF mv_formulas = abap_true.
      " the formulas carry no value, or one worked out here: Excel works
      " them out when it opens the file
      INSERT `<calcPr fullCalcOnLoad="1"/>` INTO TABLE lt_book.
    ENDIF.
    INSERT `</workbook>` INTO TABLE lt_book.
    
    temp411 = |<Relationship Id="rId{ lines( mt_out_sheet ) + 1 }" Type="{ cs_ns-rel }/styles" Target="styles.xml"/>|.
    INSERT temp411
           INTO TABLE lt_rels.
    INSERT `</Relationships>` INTO TABLE lt_rels.

    " the relationships of the package: the workbook, and the document
    " properties where they are set
    
    temp412 = |{ cv_xml_head }<Relationships xmlns="{ cs_ns-pkg_rel }">|.
    INSERT temp412 INTO TABLE lt_root.
    
    temp413 = |<Relationship Id="rId1" Type="{ cs_ns-rel }/officeDocument" Target="xl/workbook.xml"/>|.
    INSERT temp413 INTO TABLE lt_root.
    IF mv_props = abap_true.
      props_xml( IMPORTING core = lv_core
                           app  = lv_app ).
      INSERT `<Relationship Id="rId2" Target="docProps/core.xml"` INTO TABLE lt_root.
      
      temp414 = | Type="{ cs_ns-pkg_rel }/metadata/core-properties"/>|.
      INSERT temp414 INTO TABLE lt_root.
      
      temp415 = |<Relationship Id="rId3" Type="{ cs_ns-rel }/extended-properties" Target="docProps/app.xml"/>|.
      INSERT temp415
             INTO TABLE lt_root.
      INSERT `<Override PartName="/docProps/core.xml"` INTO TABLE lt_types.
      INSERT ` ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>` INTO TABLE lt_types.
      INSERT `<Override PartName="/docProps/app.xml"` INTO TABLE lt_types.
      
      temp416 = | ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>|.
      INSERT temp416
             INTO TABLE lt_types.
      
      CLEAR temp417.
      temp417-name = `docProps/core.xml`.
      temp417-content = lv_core.
      INSERT temp417 INTO TABLE lt_file.
      
      CLEAR temp418.
      temp418-name = `docProps/app.xml`.
      temp418-content = lv_app.
      INSERT temp418 INTO TABLE lt_file.
    ENDIF.
    INSERT `</Relationships>` INTO TABLE lt_root.
    INSERT `</Types>` INTO TABLE lt_types.

    
    CLEAR temp419.
    temp419-name = `[Content_Types].xml`.
    temp419-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_types ) ).
    INSERT temp419
           INTO lt_file INDEX 1.
    
    CLEAR temp420.
    temp420-name = `_rels/.rels`.
    temp420-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_root ) ).
    INSERT temp420
           INTO lt_file INDEX 2.
    
    CLEAR temp421.
    temp421-name = `xl/workbook.xml`.
    temp421-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_book ) ).
    INSERT temp421
           INTO lt_file INDEX 3.
    
    CLEAR temp422.
    temp422-name = `xl/_rels/workbook.xml.rels`.
    temp422-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( concat_lines_of( lt_rels ) ).
    INSERT temp422
           INTO lt_file INDEX 4.
    
    CLEAR temp423.
    temp423-name = `xl/styles.xml`.
    temp423-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( styles_xml( ) ).
    INSERT temp423
           INTO lt_file INDEX 5.

    result = zabaputil_cl_util_context=>zip_pack( lt_file ).

  ENDMETHOD.

  METHOD column_to_letters.

    DATA lv_rest LIKE val.
      DATA lv_digit TYPE i.
    lv_rest = val.
    WHILE lv_rest > 0.
      
      lv_digit = ( lv_rest - 1 ) MOD 26.
      result = substring( val = cv_letters
                          off = lv_digit
                          len = 1 ) && result.
      lv_rest = ( lv_rest - 1 ) DIV 26.
    ENDWHILE.

  ENDMETHOD.

  METHOD column_to_number.

    DATA lv_pos TYPE i.

    DATA lv_text TYPE string.
    DATA lv_len TYPE i.
      DATA lv_found TYPE i.
    lv_text = to_upper( val ).
    
    lv_len = strlen( lv_text ).
    WHILE lv_pos < lv_len.
      
      lv_found = find( val = cv_letters
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
    DATA lv_value TYPE string.
    DATA lv_in_name TYPE abap_bool.
    DATA ls_defname TYPE ty_s_defined_name.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.

    " a workbook is a ZIP archive, and one starts with PK. Looked at here:
    " what the ZIP class does with other bytes differs by release
    CONSTANTS lc_zip TYPE x LENGTH 2 VALUE '504B'.
    CONSTANTS lc_ole TYPE x LENGTH 4 VALUE 'D0CF11E0'.
        DATA x TYPE REF TO cx_root.
    DATA lv_book TYPE string.
    DATA temp424 TYPE zabaputil_cl_util_xlsx=>ty_t_rel.
    DATA temp75 LIKE LINE OF temp424.
    DATA lr_rel LIKE REF TO temp75.
    DATA lv_content TYPE xstring.
    DATA lv_folder TYPE string.
    DATA lt_rel TYPE zabaputil_cl_util_xlsx=>ty_t_rel.
    DATA lo_reader TYPE REF TO object.
          DATA lv_local TYPE string.
            DATA temp425 TYPE i.
            DATA lv_local_index TYPE i.
            DATA lv_sheet_count TYPE i.
              FIELD-SYMBOLS <temp426> LIKE LINE OF mt_sheet.
              DATA temp427 LIKE sy-tabix.
          DATA lv_1904 TYPE string.
          DATA temp4 TYPE xsdboolean.
          DATA lv_id TYPE string.
          DATA lv_state TYPE string.
              DATA temp428 TYPE zabaputil_cl_util_xlsx=>ty_s_sheet.
              DATA temp5 TYPE xsdboolean.
      DATA lv_path TYPE string.

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
        mo_zip = zabaputil_cl_util_context=>zip_load( val ).
        mt_part = zabaputil_cl_util_context=>zip_names( mo_zip ).
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `XLSX_NO_WORKBOOK - the file is no .xlsx workbook`
            previous = x.
    ENDTRY.

    " the workbook part is the one the package names as its document
    
    lv_book = `xl/workbook.xml`.
    
    temp424 = rels_read( `_rels/.rels` ).
    
    
    LOOP AT temp424 REFERENCE INTO lr_rel.
      IF zabaputil_cl_util_context=>c_ends_with( val    = lr_rel->type
                                                 suffix = `/officeDocument` ) = abap_true.
        lv_book = path_resolve( base   = ``
                                target = lr_rel->target ).
      ENDIF.
    ENDLOOP.
    
    lv_content = part_get( lv_book ).
    IF lv_content IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NO_WORKBOOK - the file is no .xlsx workbook`.
    ENDIF.

    
    lv_folder = path_folder( lv_book ).
    
    lt_rel = rels_read( |{ lv_folder }_rels/{ substring( val = lv_book
                                                              off = strlen( lv_folder ) ) }.rels| ).

    
    lo_reader = zabaputil_cl_util_context=>xml_reader_create( lv_content ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            value       = lv_value
                                                            t_attribute = lt_attr ).
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-end.
        EXIT.
      ENDIF.
      " the text of a defined name
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-value AND lv_in_name = abap_true.
        ls_defname-ref = ls_defname-ref && lv_value.
        CONTINUE.
      ENDIF.
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-close AND lv_name = `definedName`.
        lv_in_name = abap_false.
        INSERT ls_defname INTO TABLE mt_defname.
        CONTINUE.
      ENDIF.
      IF lv_kind <> zabaputil_cl_util_context=>cs_xml_node-open.
        CONTINUE.
      ENDIF.
      CASE lv_name.
        WHEN `definedName`.
          CLEAR ls_defname.
          lv_in_name = abap_true.
          ls_defname-name = attr( t_attribute = lt_attr
                                  name        = `name` ).
          
          lv_local = attr( t_attribute = lt_attr
                                 name        = `localSheetId` ).
          IF lv_local IS NOT INITIAL AND lv_local CO cv_digits.
            
            temp425 = lv_local.
            
            lv_local_index = temp425 + 1.
            
            lv_sheet_count = lines( mt_sheet ).
            IF lv_local_index <= lv_sheet_count.
              
              
              temp427 = sy-tabix.
              READ TABLE mt_sheet INDEX lv_local_index ASSIGNING <temp426>.
              sy-tabix = temp427.
              IF sy-subrc <> 0.
                ASSERT 1 = 0.
              ENDIF.
              ls_defname-sheet = <temp426>-name.
            ENDIF.
          ENDIF.
        WHEN `workbookPr`.
          " the date system of Excel for Mac up to 2008: day 0 is 1904-01-01
          
          lv_1904 = attr( t_attribute = lt_attr
                                name        = `date1904` ).
          
          temp4 = boolc( lv_1904 = `1` OR lv_1904 = `true` ).
          mv_date1904 = temp4.
        WHEN `sheet`.
          
          lv_id = attr( t_attribute = lt_attr
                              name        = `id` ).
          
          lv_state = attr( t_attribute = lt_attr
                                 name        = `state` ).
          LOOP AT lt_rel REFERENCE INTO lr_rel.
            IF lr_rel->id = lv_id.
              
              CLEAR temp428.
              temp428-name = attr( t_attribute = lt_attr name = `name` ).
              temp428-path = path_resolve( base = lv_book target = lr_rel->target ).
              
              temp5 = boolc( lv_state = `hidden` OR lv_state = `veryHidden` ).
              temp428-hidden = temp5.
              INSERT temp428 INTO TABLE mt_sheet.
              EXIT.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDDO.

    LOOP AT lt_rel REFERENCE INTO lr_rel.
      
      lv_path = path_resolve( base   = lv_book
                                    target = lr_rel->target ).
      IF zabaputil_cl_util_context=>c_ends_with( val    = lr_rel->type
                                                 suffix = `/sharedStrings` ) = abap_true.
        shared_read( part_get( lv_path ) ).
      ELSEIF zabaputil_cl_util_context=>c_ends_with( val    = lr_rel->type
                                                     suffix = `/styles` ) = abap_true.
        styles_read( part_get( lv_path ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD part_get.

    " a package names its parts without regard to case; a part is unpacked
    " when it is read, not with the archive
    DATA lv_name TYPE string.
    DATA lv_part LIKE LINE OF mt_part.
      DATA lv_other TYPE string.
    lv_name = to_upper( val ).
    
    LOOP AT mt_part INTO lv_part.
      
      lv_other = to_upper( lv_part ).
      IF lv_other = lv_name.
        result = zabaputil_cl_util_context=>zip_get( zip  = mo_zip
                                                     name = lv_part ).
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD rels_read.

    DATA lv_kind TYPE string.
    DATA lv_name TYPE string.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.

    DATA lv_content TYPE xstring.
    DATA lo_reader TYPE REF TO object.
        DATA temp429 TYPE zabaputil_cl_util_xlsx=>ty_s_rel.
    lv_content = part_get( val ).
    IF lv_content IS INITIAL.
      RETURN.
    ENDIF.

    
    lo_reader = zabaputil_cl_util_context=>xml_reader_create( lv_content ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            t_attribute = lt_attr ).
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-end.
        EXIT.
      ENDIF.
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-open AND lv_name = `Relationship`.
        
        CLEAR temp429.
        temp429-id = attr( t_attribute = lt_attr name = `Id` ).
        temp429-type = attr( t_attribute = lt_attr name = `Type` ).
        temp429-target = attr( t_attribute = lt_attr name = `Target` ).
        INSERT temp429 INTO TABLE result.
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
    DATA lo_reader TYPE REF TO object.
              DATA temp6 TYPE xsdboolean.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    
    lo_reader = zabaputil_cl_util_context=>xml_reader_create( val ).
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
              
              temp6 = boolc( lv_ruby = abap_false ).
              lv_take = temp6.
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
    TYPES temp10 TYPE STANDARD TABLE OF ty_s_fmt WITH DEFAULT KEY.
DATA lt_fmt  TYPE temp10.
    DATA lv_kind TYPE string.
    DATA lv_name TYPE string.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.
    DATA lv_cell TYPE abap_bool.
    DATA lv_id   TYPE i.
    DATA lo_reader TYPE REF TO object.
              DATA temp430 TYPE ty_s_fmt.
              DATA lv_code TYPE string.
              DATA temp431 LIKE LINE OF lt_fmt.
              DATA lr_fmt LIKE REF TO temp431.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    
    lo_reader = zabaputil_cl_util_context=>xml_reader_create( val ).
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
              
              CLEAR temp430.
              temp430-id = lv_id.
              temp430-code = attr( t_attribute = lt_attr name = `formatCode` ).
              INSERT temp430 INTO TABLE lt_fmt.
            WHEN `cellXfs`.
              " the formats a cell names - cellStyleXfs holds xf elements too
              lv_cell = abap_true.
            WHEN `xf`.
              IF lv_cell = abap_false.
                CONTINUE.
              ENDIF.
              lv_id = hex_to_int( attr( t_attribute = lt_attr
                                        name        = `numFmtId` ) ).
              
              lv_code = ``.
              
              
              LOOP AT lt_fmt REFERENCE INTO lr_fmt.
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
    DATA lv_code TYPE string.
    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
          DATA lv_end TYPE i.
          DATA lv_section TYPE string.
    DATA lv_date TYPE abap_bool.
    DATA temp7 TYPE xsdboolean.
    DATA lv_time TYPE abap_bool.
    DATA temp8 TYPE xsdboolean.

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
    
    lv_code = to_lower( code ).
    
    lv_len = strlen( lv_code ).
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = lv_code
                                 off = lv_pos
                                 len = 1 ).
      lv_pos = lv_pos + 1.
      CASE lv_char.
        WHEN `"`.
          
          lv_end = find( val = lv_code
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
          
          lv_section = substring( val = lv_code
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

    
    
    temp7 = boolc( lv_bare CA `dy` ).
    lv_date = temp7.
    
    
    temp8 = boolc( lv_elapsed = abap_true OR lv_bare CA `hs` ).
    lv_time = temp8.
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
      DATA temp432 LIKE LINE OF mt_sheet.
      DATA lr_sheet LIKE REF TO temp432.
      DATA temp433 LIKE LINE OF mt_out_sheet.
      DATA lr_out LIKE REF TO temp433.

    IF mv_loaded = abap_true.
      
      
      LOOP AT mt_sheet REFERENCE INTO lr_sheet.
        IF no_hidden = abap_true AND lr_sheet->hidden = abap_true.
          CONTINUE.
        ENDIF.
        INSERT lr_sheet->name INTO TABLE result.
      ENDLOOP.
    ELSE.
      
      
      LOOP AT mt_out_sheet REFERENCE INTO lr_out.
        IF no_hidden = abap_true AND lr_out->hidden = abap_true.
          CONTINUE.
        ENDIF.
        INSERT lr_out->name INTO TABLE result.
      ENDLOOP.
      " the sheet that is written now
      IF mv_open = abap_true AND ( no_hidden = abap_false OR ms_cur-layout-hidden = abap_false ).
        INSERT ms_cur-name INTO TABLE result.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD sheet_get.
      DATA lv_name TYPE string.
      DATA temp434 LIKE LINE OF mt_sheet.
      DATA lr_sheet LIKE REF TO temp434.
        DATA lv_other TYPE string.
    DATA lv_sheets TYPE i.
    FIELD-SYMBOLS <temp435> LIKE LINE OF mt_sheet.
    DATA temp436 LIKE sy-tabix.

    IF mv_loaded = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NOT_READ - a workbook built with factory( ) is written, not read`.
    ENDIF.

    IF sheet IS NOT INITIAL.
      
      lv_name = to_upper( sheet ).
      
      
      LOOP AT mt_sheet REFERENCE INTO lr_sheet.
        
        lv_other = to_upper( lr_sheet->name ).
        IF lv_other = lv_name.
          result = lr_sheet->*.
          RETURN.
        ENDIF.
      ENDLOOP.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_SHEET_UNKNOWN - the workbook has no sheet { sheet }|.
    ENDIF.

    
    lv_sheets = lines( mt_sheet ).
    IF index < 1 OR index > lv_sheets.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_SHEET_UNKNOWN - the workbook has { lv_sheets } sheets, none at { index }|.
    ENDIF.
    
    
    temp436 = sy-tabix.
    READ TABLE mt_sheet INDEX index ASSIGNING <temp435>.
    sy-tabix = temp436.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    result = <temp435>.

  ENDMETHOD.

  METHOD sheet_xml.

    result = part_get( val-path ).
    IF result IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_SHEET_UNKNOWN - the file has no part { val-path }|.
    ENDIF.

  ENDMETHOD.

  METHOD read_start.

    result-reader = zabaputil_cl_util_context=>xml_reader_create( sheet_xml( sheet ) ).
    result-sheet = sheet-name.
    result-skip = skip_hidden.
    IF range IS NOT INITIAL.
      result-area = area_parse( range ).
    ENDIF.

  ENDMETHOD.

  METHOD read_next.

    DATA lv_hidden TYPE abap_bool.
        DATA temp437 LIKE LINE OF state-t_hidden.
        DATA lr_hidden LIKE REF TO temp437.

    CLEAR t_cell.
    eof = state-eof.
    WHILE eof = abap_false.
      sheet_read_row( IMPORTING t_cell = t_cell
                                eof    = eof
                                hidden = lv_hidden
                      CHANGING  state  = state ).
      IF eof = abap_false AND state-area-row2 > 0 AND state-row > state-area-row2.
        " behind the range: nothing more to read
        eof = abap_true.
      ENDIF.
      IF eof = abap_true.
        CLEAR t_cell.
        EXIT.
      ENDIF.
      IF state-row < state-area-row1 OR ( state-skip = abap_true AND lv_hidden = abap_true ).
        CONTINUE.
      ENDIF.
      IF state-area-col1 > 0.
        DELETE t_cell WHERE col < state-area-col1 OR col > state-area-col2.
      ENDIF.
      IF state-skip = abap_true.
        
        
        LOOP AT state-t_hidden REFERENCE INTO lr_hidden.
          DELETE t_cell WHERE col >= lr_hidden->col1 AND col <= lr_hidden->col2.
        ENDLOOP.
      ENDIF.
      IF t_cell IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDWHILE.
    state-eof = eof.

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
    DATA lv_formula TYPE string.
    DATA lv_in_f   TYPE abap_bool.
    DATA lv_f_type TYPE string.
    DATA lv_f_si   TYPE string.
    DATA ls_cell   TYPE ty_s_cell.
              DATA temp9 TYPE xsdboolean.
              DATA temp10 TYPE xsdboolean.
                DATA temp438 TYPE zabaputil_cl_util_xlsx=>ty_s_area.
                DATA temp439 TYPE zabaputil_cl_util_xlsx=>ty_s_formula.
                DATA temp440 LIKE LINE OF state-t_formula.
                DATA lr_formula LIKE REF TO temp440.

    CLEAR t_cell.
    CLEAR eof.
    CLEAR hidden.

    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = state-reader
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
                state-row = lv_ref.
              ELSE.
                state-row = state-row + 1.
              ENDIF.
              lv_col = 0.
              lv_ref = attr( t_attribute = lt_attr
                             name        = `hidden` ).
              
              temp9 = boolc( lv_ref = `1` OR lv_ref = `true` ).
              hidden = temp9.
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
              CLEAR lv_formula.
              CLEAR lv_f_type.
              CLEAR lv_f_si.
            WHEN `v`.
              lv_take = abap_true.
            WHEN `f`.
              lv_in_f = abap_true.
              lv_f_type = attr( t_attribute = lt_attr
                                name        = `t` ).
              lv_f_si = attr( t_attribute = lt_attr
                              name        = `si` ).
            WHEN `t`.
              " the text of an inline string, in one piece or in runs
              
              temp10 = boolc( lv_ruby = abap_false ).
              lv_take = temp10.
            WHEN `rPh`.
              lv_ruby = abap_true.
            WHEN `col`.
              " the hidden columns, ahead of the rows
              lv_ref = attr( t_attribute = lt_attr
                             name        = `hidden` ).
              IF lv_ref = `1` OR lv_ref = `true`.
                
                CLEAR temp438.
                temp438-col1 = attr( t_attribute = lt_attr name = `min` ).
                temp438-col2 = attr( t_attribute = lt_attr name = `max` ).
                INSERT temp438 INTO TABLE state-t_hidden.
              ENDIF.
          ENDCASE.

        WHEN zabaputil_cl_util_context=>cs_xml_node-value.
          IF lv_take = abap_true.
            lv_text = lv_text && lv_value.
          ELSEIF lv_in_f = abap_true.
            lv_formula = lv_formula && lv_value.
          ENDIF.

        WHEN zabaputil_cl_util_context=>cs_xml_node-close.
          CASE lv_name.
            WHEN `v` OR `t`.
              lv_take = abap_false.
            WHEN `f`.
              lv_in_f = abap_false.
            WHEN `rPh`.
              lv_ruby = abap_false.
            WHEN `c`.
              CLEAR ls_cell.
              ls_cell-row = state-row.
              ls_cell-col = lv_col.
              cell_read( EXPORTING cell_type = lv_type
                                   style     = lv_style
                                   text      = lv_text
                         IMPORTING kind      = ls_cell-kind
                                   value     = ls_cell-value ).
              " a shared formula: the first cell holds it, the others move it
              IF lv_f_type = `shared` AND lv_formula IS NOT INITIAL.
                
                CLEAR temp439.
                temp439-si = lv_f_si.
                temp439-formula = lv_formula.
                temp439-row = state-row.
                temp439-col = lv_col.
                INSERT temp439 INTO TABLE state-t_formula.
              ELSEIF lv_f_type = `shared`.
                
                
                LOOP AT state-t_formula REFERENCE INTO lr_formula WHERE si = lv_f_si.
                  lv_formula = formula_shift( val  = lr_formula->formula
                                              rows = state-row - lr_formula->row
                                              cols = lv_col - lr_formula->col ).
                ENDLOOP.
              ENDIF.
              ls_cell-formula = lv_formula.
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

  METHOD rows_open.

    ms_rows = read_start( sheet       = sheet_get( sheet = sheet
                                                   index = index )
                          range       = range
                          skip_hidden = skip_hidden ).
    result = me.

  ENDMETHOD.

  METHOD rows_next.

    IF ms_rows-reader IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NOT_OPEN - rows_open( ) first: no sheet is read row by row`.
    ENDIF.
    read_next( IMPORTING t_cell = t_cell
                         eof    = eof
               CHANGING  state  = ms_rows ).

  ENDMETHOD.

  METHOD source_get.

    DATA lv_header TYPE i.
    DATA lv_totals TYPE i.
    DATA lv_name TYPE string.
    DATA temp441 TYPE zabaputil_cl_util_xlsx=>ty_t_excel_table.
    DATA temp76 LIKE LINE OF temp441.
    DATA lr_table LIKE REF TO temp76.
      DATA lv_table TYPE string.
      DATA temp11 TYPE xsdboolean.
    DATA temp442 LIKE LINE OF mt_defname.
    DATA lr_name LIKE REF TO temp442.
      DATA lv_other TYPE string.

    CLEAR sheet.
    CLEAR area.
    CLEAR header.
    
    lv_name = to_upper( zabaputil_cl_util_context=>c_trim( name ) ).

    " an Excel table: its range without the totals row, with its header
    
    temp441 = get_excel_tables( ).
    
    
    LOOP AT temp441 REFERENCE INTO lr_table.
      
      lv_table = to_upper( lr_table->name ).
      IF lv_table <> lv_name.
        CONTINUE.
      ENDIF.
      sheet = sheet_get( sheet = lr_table->sheet
                         index = 0 ).
      area = area_parse( lr_table->ref ).
      lv_header = lr_table->header_rows.
      lv_totals = lr_table->totals_rows.
      area-row2 = area-row2 - lv_totals.
      
      temp11 = boolc( lv_header > 0 ).
      header = temp11.
      RETURN.
    ENDLOOP.

    " a defined name: its range, the header in its first row
    
    
    LOOP AT mt_defname REFERENCE INTO lr_name.
      
      lv_other = to_upper( lr_name->name ).
      IF lv_other <> lv_name.
        CONTINUE.
      ENDIF.
      ref_split( EXPORTING val   = lr_name->ref
                 IMPORTING sheet = sheet
                           area  = area ).
      header = abap_true.
      RETURN.
    ENDLOOP.

    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = |XLSX_NAME_UNKNOWN - the workbook has no table and no name { name }|.

  ENDMETHOD.

  METHOD ref_split.
    DATA lv_pos TYPE i.
    DATA lv_sheet TYPE string.
    DATA lv_range TYPE string.

    CLEAR sheet.
    CLEAR area.
    
    lv_pos = find( val  = val
                         sub  = `!`
                         occ  = -1 ).
    IF lv_pos <= 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |XLSX_RANGE - { val } names no sheet and range|.
    ENDIF.
    
    lv_sheet = substring( val = val
                                len = lv_pos ).
    
    lv_range = substring( val = val
                                off = lv_pos + 1 ).
    IF strlen( lv_sheet ) >= 2 AND lv_sheet(1) = `'`.
      lv_sheet = substring( val = lv_sheet
                            off = 1
                            len = strlen( lv_sheet ) - 2 ).
      lv_sheet = replace( val  = lv_sheet
                          sub  = `''`
                          with = `'`
                          occ  = 0 ).
    ENDIF.
    sheet = sheet_get( sheet = lv_sheet
                       index = 0 ).
    area = area_parse( lv_range ).

  ENDMETHOD.

  METHOD sheet_tail.

    DATA lv_kind TYPE string.
    DATA lv_name TYPE string.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.
    DATA ls_link TYPE ty_s_hyperlink.
    DATA lt_rel  TYPE ty_t_rel.
    DATA lv_folder TYPE string.
    DATA lv_file TYPE string.
    DATA lo_reader TYPE REF TO object.
          DATA lv_location TYPE string.
          DATA lv_id TYPE string.
            DATA temp443 LIKE LINE OF lt_rel.
            DATA lr_rel LIKE REF TO temp443.

    CLEAR t_merge.
    CLEAR t_link.

    
    lv_folder = path_folder( sheet-path ).
    
    lv_file = substring( val = sheet-path
                               off = strlen( lv_folder ) ).
    
    lo_reader = zabaputil_cl_util_context=>xml_reader_create( sheet_xml( sheet ) ).
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
        WHEN `mergeCell`.
          INSERT attr( t_attribute = lt_attr
                       name        = `ref` ) INTO TABLE t_merge.
        WHEN `hyperlink`.
          CLEAR ls_link.
          ls_link-cell = attr( t_attribute = lt_attr
                               name        = `ref` ).
          ls_link-display = attr( t_attribute = lt_attr
                                  name        = `display` ).
          
          lv_location = attr( t_attribute = lt_attr
                                    name        = `location` ).
          
          lv_id = attr( t_attribute = lt_attr
                              name        = `id` ).
          IF lv_id IS NOT INITIAL.
            IF lt_rel IS INITIAL.
              lt_rel = rels_read( |{ lv_folder }_rels/{ lv_file }.rels| ).
            ENDIF.
            
            
            LOOP AT lt_rel REFERENCE INTO lr_rel WHERE id = lv_id.
              ls_link-target = lr_rel->target.
            ENDLOOP.
          ENDIF.
          IF lv_location IS NOT INITIAL.
            ls_link-target = |{ ls_link-target }#{ lv_location }|.
          ENDIF.
          INSERT ls_link INTO TABLE t_link.
      ENDCASE.
    ENDDO.

  ENDMETHOD.

  METHOD get_merged.

    sheet_tail( EXPORTING sheet   = sheet_get( sheet = sheet
                                               index = index )
                IMPORTING t_merge = result ).

  ENDMETHOD.

  METHOD get_hyperlinks.

    sheet_tail( EXPORTING sheet  = sheet_get( sheet = sheet
                                              index = index )
                IMPORTING t_link = result ).

  ENDMETHOD.

  METHOD get_cell_formats.

    TYPES:
      BEGIN OF ty_s_code,
        id   TYPE i,
        code TYPE string,
      END OF ty_s_code.
    TYPES temp11 TYPE STANDARD TABLE OF ty_s_code WITH DEFAULT KEY.
DATA lt_code   TYPE temp11.
    DATA lt_font   TYPE ty_t_cell_format.
    DATA lt_fill   TYPE string_table.
    DATA lt_xf     TYPE ty_t_cell_format.
    DATA ls_format TYPE ty_s_cell_format.
    DATA lv_kind   TYPE string.
    DATA lv_name   TYPE string.
    DATA lt_attr   TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.
    DATA lv_part   TYPE string.
    DATA lv_row    TYPE i.
    FIELD-SYMBOLS <font> TYPE ty_s_cell_format.
    FIELD-SYMBOLS <xf>   TYPE ty_s_cell_format.

    DATA ls_sheet TYPE zabaputil_cl_util_xlsx=>ty_s_sheet.
    DATA temp444 LIKE lt_code.
    DATA temp445 LIKE LINE OF temp444.
    DATA lv_styles TYPE string.
    DATA lv_path LIKE LINE OF mt_part.
      DATA lo_reader TYPE REF TO object.
        DATA lv_rgb TYPE string.
            DATA temp446 TYPE ty_s_code.
            DATA temp447 TYPE zabaputil_cl_util_xlsx=>ty_s_cell_format.
              FIELD-SYMBOLS <temp448> LIKE LINE OF lt_fill.
              DATA temp449 LIKE sy-tabix.
            DATA lv_id TYPE i.
            DATA temp450 LIKE LINE OF lt_code.
            DATA lr_code LIKE REF TO temp450.
            DATA lv_number TYPE string.
            DATA lv_font TYPE i.
            DATA lv_fonts TYPE i.
              DATA ls_font LIKE LINE OF lt_font.
              FIELD-SYMBOLS <temp77> LIKE LINE OF lt_font.
              DATA temp78 LIKE sy-tabix.
            DATA lv_fill TYPE i.
            DATA lv_fills TYPE i.
              FIELD-SYMBOLS <temp451> LIKE LINE OF lt_fill.
              DATA temp452 LIKE sy-tabix.
              DATA lv_wrap TYPE string.
              DATA temp12 TYPE xsdboolean.
      DATA lv_ref TYPE string.
      DATA lv_style TYPE string.
      DATA temp453 TYPE i.
      DATA lv_xf TYPE i.
      DATA lv_xfs TYPE i.
      FIELD-SYMBOLS <temp454> LIKE LINE OF lt_xf.
      DATA temp455 LIKE sy-tabix.
    ls_sheet = sheet_get( sheet = sheet
                                index = index ).

    " the built-in formats a file names by number only
    
    CLEAR temp444.
    
    temp445-id = 0.
    temp445-code = `General`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 1.
    temp445-code = `0`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 2.
    temp445-code = `0.00`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 3.
    temp445-code = `#,##0`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 4.
    temp445-code = `#,##0.00`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 9.
    temp445-code = `0%`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 10.
    temp445-code = `0.00%`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 11.
    temp445-code = `0.00E+00`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 14.
    temp445-code = `mm-dd-yy`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 18.
    temp445-code = `h:mm AM/PM`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 20.
    temp445-code = `h:mm`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 21.
    temp445-code = `h:mm:ss`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 22.
    temp445-code = `m/d/yy h:mm`.
    INSERT temp445 INTO TABLE temp444.
    temp445-id = 49.
    temp445-code = `@`.
    INSERT temp445 INTO TABLE temp444.
    lt_code = temp444.

    " styles.xml: the formats, the fonts, the fills, and the cell formats
    " made of them
    
    lv_styles = ``.
    
    LOOP AT mt_part INTO lv_path.
      IF zabaputil_cl_util_context=>c_ends_with( val    = to_lower( lv_path )
                                                 suffix = `styles.xml` ) = abap_true.
        lv_styles = lv_path.
      ENDIF.
    ENDLOOP.
    IF lv_styles IS NOT INITIAL.
      
      lo_reader = zabaputil_cl_util_context=>xml_reader_create( part_get( lv_styles ) ).
      DO.
        zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                    IMPORTING kind        = lv_kind
                                                              name        = lv_name
                                                              t_attribute = lt_attr ).
        IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-end.
          EXIT.
        ELSEIF lv_kind = zabaputil_cl_util_context=>cs_xml_node-close.
          CASE lv_name.
            WHEN `fonts` OR `fills` OR `cellXfs` OR `cellStyleXfs`.
              CLEAR lv_part.
          ENDCASE.
          CONTINUE.
        ELSEIF lv_kind <> zabaputil_cl_util_context=>cs_xml_node-open.
          CONTINUE.
        ENDIF.
        
        lv_rgb = attr( t_attribute = lt_attr
                             name        = `rgb` ).
        IF strlen( lv_rgb ) = 8.
          lv_rgb = lv_rgb+2(6).
        ENDIF.
        CASE lv_name.
          WHEN `fonts` OR `fills` OR `cellXfs` OR `cellStyleXfs`.
            lv_part = lv_name.
          WHEN `numFmt`.
            
            CLEAR temp446.
            temp446-id = hex_to_int( attr( t_attribute = lt_attr name = `numFmtId` ) ).
            temp446-code = attr( t_attribute = lt_attr name = `formatCode` ).
            INSERT temp446 INTO TABLE lt_code.
          WHEN `font`.
            
            CLEAR temp447.
            INSERT temp447 INTO TABLE lt_font ASSIGNING <font>.
          WHEN `b` OR `i` OR `u` OR `color`.
            IF lv_part = `fonts` AND <font> IS ASSIGNED.
              CASE lv_name.
                WHEN `b`.
                  <font>-bold = abap_true.
                WHEN `i`.
                  <font>-italic = abap_true.
                WHEN `u`.
                  <font>-underline = abap_true.
                WHEN OTHERS.
                  <font>-color = lv_rgb.
              ENDCASE.
            ENDIF.
          WHEN `fill`.
            INSERT `` INTO TABLE lt_fill.
          WHEN `fgColor`.
            IF lv_part = `fills` AND lt_fill IS NOT INITIAL.
              
              
              temp449 = sy-tabix.
              READ TABLE lt_fill INDEX lines( lt_fill ) ASSIGNING <temp448>.
              sy-tabix = temp449.
              IF sy-subrc <> 0.
                ASSERT 1 = 0.
              ENDIF.
              <temp448> = lv_rgb.
            ENDIF.
          WHEN `xf`.
            IF lv_part <> `cellXfs`.
              CONTINUE.
            ENDIF.
            CLEAR ls_format.
            
            lv_id = hex_to_int( attr( t_attribute = lt_attr
                                            name        = `numFmtId` ) ).
            
            
            LOOP AT lt_code REFERENCE INTO lr_code WHERE id = lv_id.
              ls_format-format = lr_code->code.
            ENDLOOP.
            
            lv_number = attr( t_attribute = lt_attr
                                    name        = `fontId` ).
            
            lv_font = 1.
            IF lv_number CO cv_digits.
              lv_font = lv_number + 1.
            ENDIF.
            
            lv_fonts = lines( lt_font ).
            IF lv_font <= lv_fonts.
              
              
              
              temp78 = sy-tabix.
              READ TABLE lt_font INDEX lv_font ASSIGNING <temp77>.
              sy-tabix = temp78.
              IF sy-subrc <> 0.
                ASSERT 1 = 0.
              ENDIF.
              ls_font = <temp77>.
              ls_format-bold = ls_font-bold.
              ls_format-italic = ls_font-italic.
              ls_format-underline = ls_font-underline.
              ls_format-color = ls_font-color.
            ENDIF.
            lv_number = attr( t_attribute = lt_attr
                              name        = `fillId` ).
            
            lv_fill = 1.
            IF lv_number CO cv_digits.
              lv_fill = lv_number + 1.
            ENDIF.
            
            lv_fills = lines( lt_fill ).
            IF lv_fill <= lv_fills.
              
              
              temp452 = sy-tabix.
              READ TABLE lt_fill INDEX lv_fill ASSIGNING <temp451>.
              sy-tabix = temp452.
              IF sy-subrc <> 0.
                ASSERT 1 = 0.
              ENDIF.
              ls_format-fill = <temp451>.
            ENDIF.
            INSERT ls_format INTO TABLE lt_xf ASSIGNING <xf>.
          WHEN `alignment`.
            IF lv_part = `cellXfs` AND <xf> IS ASSIGNED.
              <xf>-halign = attr( t_attribute = lt_attr
                                  name        = `horizontal` ).
              
              lv_wrap = attr( t_attribute = lt_attr
                                    name        = `wrapText` ).
              
              temp12 = boolc( lv_wrap = `1` OR lv_wrap = `true` ).
              <xf>-wrap = temp12.
            ENDIF.
        ENDCASE.
      ENDDO.
    ENDIF.

    " the cells that name a format other than the first
    lo_reader = zabaputil_cl_util_context=>xml_reader_create( sheet_xml( ls_sheet ) ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            t_attribute = lt_attr ).
      IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-end
          OR ( lv_kind = zabaputil_cl_util_context=>cs_xml_node-close AND lv_name = `sheetData` ).
        EXIT.
      ENDIF.
      IF lv_kind <> zabaputil_cl_util_context=>cs_xml_node-open.
        CONTINUE.
      ENDIF.
      
      lv_ref = attr( t_attribute = lt_attr
                           name        = `r` ).
      CASE lv_name.
        WHEN `row`.
          IF lv_ref CO cv_digits AND lv_ref IS NOT INITIAL.
            lv_row = lv_ref.
          ELSE.
            lv_row = lv_row + 1.
          ENDIF.
          CONTINUE.
        WHEN `c`.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
      
      lv_style = attr( t_attribute = lt_attr
                             name        = `s` ).
      IF lv_style IS INITIAL OR lv_style CN cv_digits.
        CONTINUE.
      ENDIF.
      
      temp453 = lv_style.
      
      lv_xf = temp453 + 1.
      
      lv_xfs = lines( lt_xf ).
      IF lv_xf <= 1 OR lv_xf > lv_xfs.
        CONTINUE.
      ENDIF.
      
      
      temp455 = sy-tabix.
      READ TABLE lt_xf INDEX lv_xf ASSIGNING <temp454>.
      sy-tabix = temp455.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      ls_format = <temp454>.
      ls_format-row = lv_row.
      ls_format-col = column_to_number( lv_ref ).
      INSERT ls_format INTO TABLE result.
    ENDDO.

  ENDMETHOD.

  METHOD get_defined_names.

    IF mv_loaded = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NOT_READ - a workbook built with factory( ) is written, not read`.
    ENDIF.
    result = mt_defname.

  ENDMETHOD.

  METHOD get_excel_tables.

    DATA lv_kind TYPE string.
    DATA lv_name TYPE string.
    DATA lt_attr TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.
    DATA ls_table TYPE ty_s_excel_table.
    DATA temp456 LIKE LINE OF mt_sheet.
    DATA lr_sheet LIKE REF TO temp456.
      DATA lv_folder TYPE string.
      DATA lv_file TYPE string.
      DATA temp457 TYPE zabaputil_cl_util_xlsx=>ty_t_rel.
      DATA temp79 LIKE LINE OF temp457.
      DATA lr_rel LIKE REF TO temp79.
        DATA lv_part TYPE xstring.
        DATA lo_reader TYPE REF TO object.
        DATA lv_count TYPE string.

    IF mv_loaded = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `XLSX_NOT_READ - a workbook built with factory( ) is written, not read`.
    ENDIF.

    
    
    LOOP AT mt_sheet REFERENCE INTO lr_sheet.
      
      lv_folder = path_folder( lr_sheet->path ).
      
      lv_file = substring( val = lr_sheet->path
                                 off = strlen( lv_folder ) ).
      
      temp457 = rels_read( |{ lv_folder }_rels/{ lv_file }.rels| ).
      
      
      LOOP AT temp457 REFERENCE INTO lr_rel.
        IF zabaputil_cl_util_context=>c_ends_with( val    = lr_rel->type
                                                   suffix = `/table` ) = abap_false.
          CONTINUE.
        ENDIF.
        
        lv_part = part_get( path_resolve( base   = lr_sheet->path
                                                target = lr_rel->target ) ).
        IF lv_part IS INITIAL.
          CONTINUE.
        ENDIF.
        " the table element is the first one of the part
        
        lo_reader = zabaputil_cl_util_context=>xml_reader_create( lv_part ).
        DO.
          zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                      IMPORTING kind        = lv_kind
                                                                name        = lv_name
                                                                t_attribute = lt_attr ).
          IF lv_kind = zabaputil_cl_util_context=>cs_xml_node-end
              OR ( lv_kind = zabaputil_cl_util_context=>cs_xml_node-open AND lv_name = `table` ).
            EXIT.
          ENDIF.
        ENDDO.
        IF lv_name <> `table`.
          CONTINUE.
        ENDIF.
        CLEAR ls_table.
        ls_table-sheet = lr_sheet->name.
        ls_table-name = attr( t_attribute = lt_attr
                              name        = `displayName` ).
        IF ls_table-name IS INITIAL.
          ls_table-name = attr( t_attribute = lt_attr
                                name        = `name` ).
        ENDIF.
        ls_table-ref = attr( t_attribute = lt_attr
                             name        = `ref` ).
        " a header row unless the table says it has none
        
        lv_count = attr( t_attribute = lt_attr
                               name        = `headerRowCount` ).
        ls_table-header_rows = 1.
        IF lv_count IS NOT INITIAL AND lv_count CO cv_digits.
          ls_table-header_rows = lv_count.
        ENDIF.
        lv_count = attr( t_attribute = lt_attr
                         name        = `totalsRowCount` ).
        IF lv_count IS NOT INITIAL AND lv_count CO cv_digits.
          ls_table-totals_rows = lv_count.
        ENDIF.
        INSERT ls_table INTO TABLE result.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD formula_shift.

    DATA lv_len     TYPE i.
    DATA lv_col     TYPE i.
    DATA lv_row     TYPE i.
    DATA lv_abs_col TYPE abap_bool.
    DATA lv_abs_row TYPE abap_bool.
    DATA lv_pos     TYPE i.
    DATA lv_quote   TYPE abap_bool.
    DATA lv_bracket TYPE i.

    " the parts of a formula that are no reference: a text in quotes, a
    " sheet name in apostrophes, a structured reference in brackets, a name
    DATA lv_end TYPE i.
      DATA lv_char TYPE string.
        DATA temp13 TYPE xsdboolean.
          DATA lv_mark_col TYPE string.
          DATA lv_mark_row TYPE string.
    lv_end = strlen( val ).
    WHILE lv_pos < lv_end.
      
      lv_char = val+lv_pos(1).
      IF lv_char = `"` OR lv_char = `'`.
        
        temp13 = boolc( lv_quote = abap_false ).
        lv_quote = temp13.
      ELSEIF lv_quote = abap_false AND lv_char = `[`.
        lv_bracket = lv_bracket + 1.
      ELSEIF lv_quote = abap_false AND lv_char = `]`.
        lv_bracket = lv_bracket - 1.
      ELSEIF lv_quote = abap_false AND lv_bracket = 0.
        ref_at( EXPORTING val     = val
                          pos     = lv_pos
                IMPORTING len     = lv_len
                          col     = lv_col
                          row     = lv_row
                          abs_col = lv_abs_col
                          abs_row = lv_abs_row ).
        IF lv_len > 0.
          IF lv_abs_col = abap_false.
            lv_col = lv_col + cols.
          ENDIF.
          IF lv_abs_row = abap_false.
            lv_row = lv_row + rows.
          ENDIF.
          
          lv_mark_col = ``.
          IF lv_abs_col = abap_true.
            lv_mark_col = `$`.
          ENDIF.
          
          lv_mark_row = ``.
          IF lv_abs_row = abap_true.
            lv_mark_row = `$`.
          ENDIF.
          result = |{ result }{ lv_mark_col }{ column_to_letters( lv_col ) }{ lv_mark_row }{ lv_row }|.
          lv_pos = lv_pos + lv_len.
          CONTINUE.
        ENDIF.
      ENDIF.
      result = result && lv_char.
      lv_pos = lv_pos + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD ref_at.

    CONSTANTS lc_word TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.`.
    DATA lv_letters TYPE string.
    DATA lv_digits  TYPE string.
      DATA lv_before TYPE string.
    DATA lv_end TYPE i.
    DATA lv_pos LIKE pos.
      DATA lv_after TYPE string.

    CLEAR len.
    CLEAR col.
    CLEAR row.
    CLEAR abs_col.
    CLEAR abs_row.

    " a reference starts where no name goes on
    IF pos > 0.
      
      lv_before = substring( val = val
                                   off = pos - 1
                                   len = 1 ).
      IF lv_before CA lc_word.
        RETURN.
      ENDIF.
    ENDIF.

    
    lv_end = strlen( val ).
    
    lv_pos = pos.
    IF lv_pos < lv_end AND val+lv_pos(1) = `$`.
      abs_col = abap_true.
      lv_pos = lv_pos + 1.
    ENDIF.
    WHILE lv_pos < lv_end AND strlen( lv_letters ) < 4 AND val+lv_pos(1) CA cv_letters.
      lv_letters = lv_letters && val+lv_pos(1).
      lv_pos = lv_pos + 1.
    ENDWHILE.
    IF lv_pos < lv_end AND val+lv_pos(1) = `$`.
      abs_row = abap_true.
      lv_pos = lv_pos + 1.
    ENDIF.
    WHILE lv_pos < lv_end AND strlen( lv_digits ) < 8 AND val+lv_pos(1) CA cv_digits.
      lv_digits = lv_digits && val+lv_pos(1).
      lv_pos = lv_pos + 1.
    ENDWHILE.
    " A1 to XFD1048576, and not the start of a name or a function (LOG10()
    IF lv_letters IS INITIAL OR strlen( lv_letters ) > 3 OR lv_digits IS INITIAL OR strlen( lv_digits ) > 7.
      RETURN.
    ENDIF.
    IF lv_pos < lv_end.
      
      lv_after = substring( val = val
                                  off = lv_pos
                                  len = 1 ).
      IF lv_after CA lc_word OR lv_after = `(` OR lv_after = `!`.
        RETURN.
      ENDIF.
    ENDIF.
    col = column_to_number( lv_letters ).
    row = lv_digits.
    IF col > cv_max_col OR row < 1 OR row > cv_max_row.
      CLEAR col.
      CLEAR row.
      RETURN.
    ENDIF.
    len = lv_pos - pos.

  ENDMETHOD.

  METHOD date_by_text.

    DATA lt_part TYPE string_table.
    DATA lv_day   TYPE string.
    DATA lv_month TYPE string.
    DATA lv_year  TYPE string.

    " the date in front of a time
    DATA lv_text TYPE string.
    DATA lv_part LIKE LINE OF lt_part.
    FIELD-SYMBOLS <temp458> LIKE LINE OF lt_part.
    DATA temp459 LIKE sy-tabix.
      FIELD-SYMBOLS <temp460> LIKE LINE OF lt_part.
      DATA temp461 LIKE sy-tabix.
      FIELD-SYMBOLS <temp462> LIKE LINE OF lt_part.
      DATA temp463 LIKE sy-tabix.
      FIELD-SYMBOLS <temp464> LIKE LINE OF lt_part.
      DATA temp465 LIKE sy-tabix.
      FIELD-SYMBOLS <temp466> LIKE LINE OF lt_part.
      DATA temp467 LIKE sy-tabix.
      FIELD-SYMBOLS <temp468> LIKE LINE OF lt_part.
      DATA temp469 LIKE sy-tabix.
      FIELD-SYMBOLS <temp470> LIKE LINE OF lt_part.
      DATA temp471 LIKE sy-tabix.
      FIELD-SYMBOLS <temp472> LIKE LINE OF lt_part.
      DATA temp473 LIKE sy-tabix.
      FIELD-SYMBOLS <temp474> LIKE LINE OF lt_part.
      DATA temp475 LIKE sy-tabix.
      FIELD-SYMBOLS <temp476> LIKE LINE OF lt_part.
      DATA temp477 LIKE sy-tabix.
    DATA temp478 TYPE i.
    DATA temp80 TYPE i.
    lv_text = condense( val ).
    IF lv_text CS ` `.
      lv_text = substring_before( val = lv_text
                                  sub = ` ` ).
    ENDIF.
    lv_text = translate( val  = lv_text
                         from = `/-`
                         to   = `..` ).
    SPLIT lv_text AT `.` INTO TABLE lt_part.
    IF lines( lt_part ) <> 3.
      RETURN.
    ENDIF.
    
    LOOP AT lt_part INTO lv_part.
      IF lv_part IS INITIAL OR lv_part CN cv_digits OR strlen( lv_part ) > 4.
        RETURN.
      ENDIF.
    ENDLOOP.

    
    
    temp459 = sy-tabix.
    READ TABLE lt_part INDEX 1 ASSIGNING <temp458>.
    sy-tabix = temp459.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    IF strlen( <temp458> ) = 4.
      
      
      temp461 = sy-tabix.
      READ TABLE lt_part INDEX 1 ASSIGNING <temp460>.
      sy-tabix = temp461.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_year = <temp460>.
      
      
      temp463 = sy-tabix.
      READ TABLE lt_part INDEX 2 ASSIGNING <temp462>.
      sy-tabix = temp463.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_month = <temp462>.
      
      
      temp465 = sy-tabix.
      READ TABLE lt_part INDEX 3 ASSIGNING <temp464>.
      sy-tabix = temp465.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_day = <temp464>.
    ELSEIF date_order = `MDY`.
      
      
      temp467 = sy-tabix.
      READ TABLE lt_part INDEX 1 ASSIGNING <temp466>.
      sy-tabix = temp467.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_month = <temp466>.
      
      
      temp469 = sy-tabix.
      READ TABLE lt_part INDEX 2 ASSIGNING <temp468>.
      sy-tabix = temp469.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_day = <temp468>.
      
      
      temp471 = sy-tabix.
      READ TABLE lt_part INDEX 3 ASSIGNING <temp470>.
      sy-tabix = temp471.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_year = <temp470>.
    ELSE.
      
      
      temp473 = sy-tabix.
      READ TABLE lt_part INDEX 1 ASSIGNING <temp472>.
      sy-tabix = temp473.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_day = <temp472>.
      
      
      temp475 = sy-tabix.
      READ TABLE lt_part INDEX 2 ASSIGNING <temp474>.
      sy-tabix = temp475.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_month = <temp474>.
      
      
      temp477 = sy-tabix.
      READ TABLE lt_part INDEX 3 ASSIGNING <temp476>.
      sy-tabix = temp477.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_year = <temp476>.
    ENDIF.
    " a year of two digits as Excel reads it: up to 29 this century
    IF strlen( lv_year ) = 2.
      IF lv_year < `30`.
        lv_year = `20` && lv_year.
      ELSE.
        lv_year = `19` && lv_year.
      ENDIF.
    ENDIF.
    IF strlen( lv_year ) <> 4 OR strlen( lv_month ) > 2 OR strlen( lv_day ) > 2.
      RETURN.
    ENDIF.
    
    temp478 = lv_month.
    
    temp80 = lv_day.
    result = |{ lv_year }{ two_digits( temp478 ) }{ two_digits( temp80 ) }|.

  ENDMETHOD.

  METHOD number_by_text.

    DATA lv_minus TYPE abap_bool.
    DATA lv_percent TYPE abap_bool.

    DATA lv_text TYPE string.
    DATA lv_len TYPE i.
    DATA lv_sign TYPE string.
        DATA temp14 TYPE xsdboolean.
    DATA lv_number TYPE string.
          DATA lv_parsed TYPE decfloat34.
    DATA temp479 TYPE decfloat34.
    DATA lv_value LIKE temp479.
    lv_text = condense( val ).
    " a sign behind the number, as SAP writes it
    
    lv_len = strlen( lv_text ).
    IF lv_len > 1 AND substring( val = lv_text
                                 off = lv_len - 1 ) = `-`.
      lv_minus = abap_true.
      lv_text = substring( val = lv_text
                           len = lv_len - 1 ).
    ENDIF.
    IF lv_text CS `%`.
      lv_percent = abap_true.
      lv_text = replace( val  = lv_text
                         sub  = `%`
                         with = ``
                         occ  = 0 ).
    ENDIF.
    " a currency in front or behind (EUR 12.50, 12,50 EUR, $12.50): what is
    " no digit at either end goes, a sign in front stays
    lv_text = condense( lv_text ).
    WHILE strlen( lv_text ) > 0 AND lv_text(1) NA `0123456789+-.,`.
      lv_text = condense( substring( val = lv_text
                                     off = 1 ) ).
    ENDWHILE.
    lv_len = strlen( lv_text ).
    WHILE lv_len > 0 AND substring( val = lv_text
                                    off = lv_len - 1 ) NA cv_digits.
      lv_text = condense( substring( val = lv_text
                                     len = lv_len - 1 ) ).
      lv_len = strlen( lv_text ).
    ENDWHILE.
    IF lv_text IS INITIAL OR lv_text CN `0123456789.,-+ ` OR lv_text NA cv_digits.
      RETURN.
    ENDIF.
    
    lv_sign = lv_text(1).
    CASE lv_sign.
      WHEN `-`.
        
        temp14 = boolc( lv_minus = abap_false ).
        lv_minus = temp14.
        lv_text = substring( val = lv_text
                             off = 1 ).
      WHEN `+`.
        lv_text = substring( val = lv_text
                             off = 1 ).
    ENDCASE.
    IF lv_text CA `+-`.
      RETURN.
    ENDIF.

    
    lv_number = number_plain( lv_text ).
    IF lv_number IS INITIAL.
      TRY.
          
          lv_parsed = zabaputil_cl_util_context=>conv_string_to_number( lv_text ).
          lv_number = number_plain( |{ lv_parsed }| ).
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.
    IF lv_number IS INITIAL.
      RETURN.
    ENDIF.
    
    temp479 = lv_number.
    
    lv_value = temp479.
    IF lv_percent = abap_true.
      lv_value = lv_value / 100.
    ENDIF.
    IF lv_minus = abap_true.
      lv_value = - lv_value.
    ENDIF.
    result = number_plain( |{ lv_value }| ).

  ENDMETHOD.

  METHOD cell_read.
          DATA temp480 TYPE i.
          DATA lv_index TYPE i.
          DATA lv_texts TYPE i.
            FIELD-SYMBOLS <temp481> LIKE LINE OF mt_shared.
            DATA temp482 LIKE sy-tabix.

    CLEAR kind.
    CLEAR value.

    CASE cell_type.
      WHEN `s`.
        " the index of the text in the shared strings, from 0
        kind = cs_kind-text.
        IF text IS NOT INITIAL AND text CO cv_digits.
          
          temp480 = text.
          
          lv_index = temp480 + 1.
          
          lv_texts = lines( mt_shared ).
          IF lv_index <= lv_texts.
            
            
            temp482 = sy-tabix.
            READ TABLE mt_shared INDEX lv_index ASSIGNING <temp481>.
            sy-tabix = temp482.
            IF sy-subrc <> 0.
              ASSERT 1 = 0.
            ENDIF.
            value = <temp481>.
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
    DATA lv_number TYPE string.
    DATA lv_format TYPE string.
      DATA temp483 TYPE i.
      DATA lv_index TYPE i.
      DATA lv_styles TYPE i.
        FIELD-SYMBOLS <temp484> LIKE LINE OF mt_style_kind.
        DATA temp485 LIKE sy-tabix.
    DATA lv_whole LIKE lv_number.
    DATA lv_part TYPE string.
    DATA lv_time TYPE string.
    DATA lv_date TYPE d.

    CLEAR kind.
    CLEAR value.

    
    lv_number = number_plain( val ).
    IF lv_number IS INITIAL.
      " no number after all - handed on as it stands
      kind = cs_kind-text.
      value = val.
      RETURN.
    ENDIF.
    kind = cs_kind-number.
    value = lv_number.

    " the format the cell is shown in says whether the number is a date
    
    lv_format = ``.
    IF style IS NOT INITIAL AND style CO cv_digits.
      
      temp483 = style.
      
      lv_index = temp483 + 1.
      
      lv_styles = lines( mt_style_kind ).
      IF lv_index <= lv_styles.
        
        
        temp485 = sy-tabix.
        READ TABLE mt_style_kind INDEX lv_index ASSIGNING <temp484>.
        sy-tabix = temp485.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_format = <temp484>.
      ENDIF.
    ENDIF.
    IF lv_format IS INITIAL OR lv_number(1) = `-`.
      RETURN.
    ENDIF.

    " whole days and the fraction of a day, to the second
    
    lv_whole = lv_number.
    
    lv_part = `0`.
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
    
    lv_time = |{ two_digits( lv_seconds DIV 3600 ) }:{ two_digits( lv_seconds MOD 3600 DIV 60 ) }|
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

    
    lv_date = serial_to_date( val      = lv_days
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

    DATA lv_text TYPE string.
    DATA lv_sign TYPE string.
      DATA lv_exp_text TYPE string.
      DATA lv_exp_sign TYPE i.
    DATA lv_whole LIKE lv_text.
    DATA lv_part TYPE string.
    DATA lv_digits TYPE string.
    DATA lv_zeros TYPE i.
    DATA lv_len TYPE i.
      DATA lv_next TYPE string.
    lv_text = to_upper( condense( val ) ).
    
    lv_sign = ``.
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
      
      lv_exp_text = substring_after( val = lv_text
                                           sub = `E` ).
      lv_text = substring_before( val = lv_text
                                  sub = `E` ).
      
      lv_exp_sign = 1.
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
    
    lv_whole = lv_text.
    
    lv_part = ``.
    IF lv_text CS `.`.
      lv_whole = substring_before( val = lv_text
                                   sub = `.` ).
      lv_part = substring_after( val = lv_text
                                 sub = `.` ).
    ENDIF.
    
    lv_digits = lv_whole && lv_part.
    IF lv_digits IS INITIAL OR lv_digits CN cv_digits.
      RETURN.
    ENDIF.
    lv_point = strlen( lv_whole ) + lv_exp.

    " without the zeros in front
    
    lv_zeros = 0.
    
    lv_len = strlen( lv_digits ).
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
      
      lv_next = lv_digits+digits(1).
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

    DATA lt_row   TYPE ty_t_cell.
    DATA lv_eof   TYPE abap_bool.
    DATA lv_rows  TYPE i.
    DATA lt_merge TYPE string_table.
    DATA ls_cell  TYPE ty_s_cell.

    DATA ls_sheet TYPE zabaputil_cl_util_xlsx=>ty_s_sheet.
    DATA ls_read TYPE zabaputil_cl_util_xlsx=>ty_s_read.
    DATA lv_merge LIKE LINE OF lt_merge.
      DATA ls_area TYPE zabaputil_cl_util_xlsx=>ty_s_area.
    ls_sheet = sheet_get( sheet = sheet
                                index = index ).
    
    ls_read = read_start( sheet       = ls_sheet
                                range       = range
                                skip_hidden = skip_hidden ).
    DO.
      IF max_rows > 0 AND lv_rows >= max_rows.
        EXIT.
      ENDIF.
      read_next( IMPORTING t_cell = lt_row
                           eof    = lv_eof
                 CHANGING  state  = ls_read ).
      IF lv_eof = abap_true.
        EXIT.
      ENDIF.
      INSERT LINES OF lt_row INTO TABLE result.
      lv_rows = lv_rows + 1.
    ENDDO.

    IF fill_merged = abap_false.
      RETURN.
    ENDIF.
    " every cell of a merged range as its first one - within what was read
    sheet_tail( EXPORTING sheet   = ls_sheet
                IMPORTING t_merge = lt_merge ).
    
    LOOP AT lt_merge INTO lv_merge.
      
      ls_area = area_parse( lv_merge ).
      CLEAR ls_cell.
      LOOP AT result INTO ls_cell WHERE row = ls_area-row1 AND col = ls_area-col1.
        EXIT.
      ENDLOOP.
      IF ls_cell-row <> ls_area-row1 OR ls_cell-col <> ls_area-col1.
        CONTINUE.
      ENDIF.
      DELETE result WHERE row >= ls_area-row1 AND row <= ls_area-row2
                      AND col >= ls_area-col1 AND col <= ls_area-col2.
      DO ls_area-row2 - ls_area-row1 + 1 TIMES.
        ls_cell-row = ls_area-row1 + sy-index - 1.
        DO ls_area-col2 - ls_area-col1 + 1 TIMES.
          ls_cell-col = ls_area-col1 + sy-index - 1.
          INSERT ls_cell INTO TABLE result.
        ENDDO.
      ENDDO.
    ENDLOOP.
    SORT result BY row col.

  ENDMETHOD.

  METHOD get_table.

    TYPES:
      BEGIN OF ty_s_map,
        name TYPE string,
        cat  TYPE string,
        conv TYPE string,
      END OF ty_s_map.
    TYPES temp12 TYPE STANDARD TABLE OF ty_s_map WITH DEFAULT KEY.
DATA lt_map     TYPE temp12.
    DATA lr_line    TYPE REF TO data.
    DATA lt_field   TYPE ty_t_field.
    DATA lt_row     TYPE ty_t_cell.
    DATA lv_eof     TYPE abap_bool.
    DATA lv_mapped  TYPE abap_bool.
    DATA lv_any     TYPE abap_bool.
    DATA lv_elementary TYPE abap_bool.
    DATA ls_sheet   TYPE ty_s_sheet.
    DATA ls_read    TYPE ty_s_read.
    DATA ls_area    TYPE ty_s_area.
    DATA lv_has_header TYPE abap_bool.
    DATA lv_rows    TYPE i.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <cell>  TYPE ty_s_cell.
    FIELD-SYMBOLS <map>   TYPE ty_s_map.
    DATA lv_collect TYPE abap_bool.
    DATA temp15 TYPE xsdboolean.
    DATA lv_order TYPE string.
    DATA lv_header_row LIKE header_row.
        DATA temp486 TYPE ty_s_map.
      DATA temp487 LIKE LINE OF lt_field.
      DATA lr_field LIKE REF TO temp487.
        DATA temp488 TYPE ty_s_map.
          DATA lv_header TYPE string.
          DATA lv_name LIKE lv_header.
          DATA temp489 LIKE LINE OF t_column.
          DATA lr_column LIKE REF TO temp489.
          DATA ls_field TYPE zabaputil_cl_util_xlsx=>ty_s_field.
            DATA temp490 TYPE ty_s_map.
            DATA lv_conv_name TYPE string.
      DATA lv_columns TYPE i.
            DATA x TYPE REF TO zabaputil_cx_util_error.
            DATA temp491 TYPE zabaputil_cl_util_xlsx=>ty_s_error.

    CLEAR result.
    CLEAR t_error.
    
    
    temp15 = boolc( t_error IS SUPPLIED ).
    lv_collect = temp15.
    
    lv_order = to_upper( date_order ).

    " where the rows are: a sheet, a range of it, or a table or name - the
    " header in the first row of a range unless the caller says otherwise
    
    lv_header_row = header_row.
    IF table_name IS NOT INITIAL.
      source_get( EXPORTING name   = table_name
                  IMPORTING sheet  = ls_sheet
                            area   = ls_area
                            header = lv_has_header ).
      ls_read = read_start( sheet       = ls_sheet
                            range       = area_text( ls_area )
                            skip_hidden = skip_hidden ).
      IF header_row IS NOT SUPPLIED OR header_row > 0.
        lv_header_row = ls_area-row1.
      ENDIF.
      IF lv_has_header = abap_false.
        lv_header_row = 0.
      ENDIF.
    ELSE.
      ls_sheet = sheet_get( sheet = sheet
                            index = index ).
      ls_read = read_start( sheet       = ls_sheet
                            range       = range
                            skip_hidden = skip_hidden ).
      IF range IS NOT INITIAL AND ( header_row IS NOT SUPPLIED OR header_row > 0 ).
        lv_header_row = ls_read-area-row1.
      ENDIF.
    ENDIF.
    CREATE DATA lr_line LIKE LINE OF result.
    ASSIGN lr_line->* TO <line>.
    fields_get( EXPORTING row        = <line>
                IMPORTING elementary = lv_elementary
                          result     = lt_field ).

    IF lv_header_row <= 0.
      " no header: the n-th column (of the range) is the n-th component
      DO ls_read-area-col1 - 1 TIMES.
        
        CLEAR temp486.
        INSERT temp486 INTO TABLE lt_map.
      ENDDO.
      
      
      LOOP AT lt_field REFERENCE INTO lr_field.
        
        CLEAR temp488.
        temp488-name = lr_field->name.
        temp488-cat = lr_field->cat.
        INSERT temp488 INTO TABLE lt_map.
      ENDLOOP.
      lv_mapped = abap_true.
    ENDIF.

    DO.
      IF max_rows > 0 AND lv_rows >= max_rows.
        EXIT.
      ENDIF.
      read_next( IMPORTING t_cell = lt_row
                           eof    = lv_eof
                 CHANGING  state  = ls_read ).
      IF lv_eof = abap_true.
        EXIT.
      ENDIF.
      IF ls_read-row < lv_header_row.
        CONTINUE.
      ENDIF.

      IF lv_mapped = abap_false.
        lv_mapped = abap_true.
        IF ls_read-row > lv_header_row.
          " the header row is empty - no column has a component
          EXIT.
        ENDIF.
        LOOP AT lt_row ASSIGNING <cell>.
          
          lv_header = zabaputil_cl_util_context=>c_trim_upper( <cell>-value ).
          
          lv_name = lv_header.
          " the header the caller gives for a component goes first
          
          
          LOOP AT t_column REFERENCE INTO lr_column.
            IF lr_column->header IS NOT INITIAL
                AND zabaputil_cl_util_context=>c_trim_upper( lr_column->header ) = lv_header.
              lv_name = zabaputil_cl_util_context=>c_trim_upper( lr_column->name ).
              EXIT.
            ENDIF.
          ENDLOOP.
          
          ls_field = field_find( t_field = lt_field
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
            
            CLEAR temp490.
            INSERT temp490 INTO TABLE lt_map.
          ENDWHILE.
          READ TABLE lt_map INDEX <cell>-col ASSIGNING <map>.
          <map>-name = ls_field-name.
          <map>-cat = ls_field-cat.
        ENDLOOP.
        " a conversion exit the caller names for a component
        LOOP AT lt_map ASSIGNING <map> WHERE name IS NOT INITIAL.
          LOOP AT t_column REFERENCE INTO lr_column WHERE conv_exit IS NOT INITIAL.
            
            lv_conv_name = zabaputil_cl_util_context=>c_trim_upper( lr_column->name ).
            IF lv_conv_name = <map>-name.
              <map>-conv = zabaputil_cl_util_context=>c_trim_upper( lr_column->conv_exit ).
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        CONTINUE.
      ENDIF.

      CLEAR <line>.
      lv_any = abap_false.
      
      lv_columns = lines( lt_map ).
      LOOP AT lt_row ASSIGNING <cell>.
        IF <cell>-col > lv_columns.
          CONTINUE.
        ENDIF.
        READ TABLE lt_map INDEX <cell>-col ASSIGNING <map>.
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
        TRY.
            IF <map>-conv IS INITIAL.
              cell_to_field( EXPORTING cell       = <cell>
                                       cat        = <map>-cat
                                       date_order = lv_order
                             CHANGING  field      = <field> ).
            ELSE.
              " the internal form by the conversion exit (ALPHA: 42 to 0000000042)
              zabaputil_cl_util_context=>conv_exit( EXPORTING convexit = <map>-conv
                                                              from     = <cell>-value
                                                    CHANGING  value    = <field> ).
            ENDIF.
            
          CATCH zabaputil_cx_util_error INTO x.
            IF lv_collect = abap_false.
              RAISE EXCEPTION x.
            ENDIF.
            " the cell is listed, the row read on
            
            CLEAR temp491.
            temp491-sheet = ls_sheet-name.
            temp491-cell = |{ column_to_letters( <cell>-col ) }{ <cell>-row }|.
            temp491-row = <cell>-row.
            temp491-col = <cell>-col.
            temp491-value = <cell>-value.
            temp491-component = <map>-name.
            temp491-message = x->get_text( ).
            INSERT temp491 INTO TABLE t_error.
        ENDTRY.
        lv_any = abap_true.
      ENDLOOP.
      IF lv_any = abap_true.
        INSERT <line> INTO TABLE result.
        lv_rows = lv_rows + 1.
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

    DATA lv_value LIKE cell-value.
    DATA lv_address TYPE string.
        DATA lv_flag TYPE string.
        DATA temp16 TYPE xsdboolean.
            DATA x TYPE REF TO cx_root.
    lv_value = cell-value.
    
    lv_address = |{ column_to_letters( cell-col ) }{ cell-row }|.

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
        
        lv_flag = to_upper( condense( lv_value ) ).
        
        temp16 = boolc( lv_flag = `X` OR lv_flag = `TRUE` OR lv_flag = `1` ).
        field = temp16.

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
            " a text: 2024-03-15, 15.03.2024, 3/15/2024 by the order of
            " day and month, 20240315
            lv_number = date_by_text( val        = lv_value
                                      date_order = date_order ).
            IF lv_number IS INITIAL.
              lv_number = condense( lv_value ).
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
            IF lv_number IS INITIAL.
              " a text that reads as a number in another notation
              " (1.234,56, 12 %, EUR 5, 7-) is one; anything else is not
              lv_number = number_by_text( lv_value ).
            ENDIF.
        ENDCASE.
        IF lv_number IS INITIAL.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |XLSX_CELL_TYPE - cell { lv_address }: { lv_value } is no number|.
        ENDIF.
        " a utclong takes the timestamp as ISO text, not as a number
        IF cat = cs_cat-timestamp AND zabaputil_cl_util_context=>rtti_get_type_kind( field ) = `p`
            AND strlen( lv_number ) >= 14.
          lv_number = |{ lv_number(4) }-{ lv_number+4(2) }-{ lv_number+6(2) }T{ lv_number+8(2) }:|
                   && |{ lv_number+10(2) }:{ lv_number+12(2) }|.
        ENDIF.
        TRY.
            field = lv_number.
            
          CATCH cx_root INTO x.
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

    DATA lt_cell TYPE ty_t_cell.
    DATA ls_sheet TYPE ty_s_sheet.
    DATA ls_area TYPE ty_s_area.
    DATA lv_header TYPE abap_bool.

    " a sheet, a range of it, or a table or a name - the header in the
    " first row of a range unless the caller says otherwise
    DATA lv_header_row LIKE header_row.
    DATA temp492 TYPE string.
    DATA lv_range LIKE temp492.
      DATA lv_cut TYPE i.
      DATA lv_tabix LIKE sy-tabix.
      DATA lv_base LIKE <name>.
      DATA lv_suffix TYPE i.
        DATA lv_first TYPE i.
        DATA lv_other LIKE LINE OF lt_name.
        DATA lv_tail TYPE string.
    lv_header_row = header_row.
    
    temp492 = range.
    
    lv_range = temp492.
    IF table_name IS NOT INITIAL.
      source_get( EXPORTING name   = table_name
                  IMPORTING sheet  = ls_sheet
                            area   = ls_area
                            header = lv_header ).
      lv_range = area_text( ls_area ).
      lt_cell = get_cells( sheet       = ls_sheet-name
                           range       = lv_range
                           skip_hidden = skip_hidden ).
      lv_header_row = ls_area-row1.
      IF lv_header = abap_false OR ( header_row IS SUPPLIED AND header_row <= 0 ).
        lv_header_row = 0.
      ENDIF.
    ELSE.
      lt_cell = get_cells( sheet       = sheet
                           index       = index
                           range       = range
                           skip_hidden = skip_hidden ).
      IF range IS NOT INITIAL.
        ls_area = area_parse( range ).
        IF header_row IS NOT SUPPLIED OR header_row > 0.
          lv_header_row = ls_area-row1.
        ENDIF.
      ENDIF.
    ENDIF.
    DELETE lt_cell WHERE row < lv_header_row.
    " the rows after the header, as many as asked for
    IF max_rows > 0.
      
      lv_cut = 0.
      LOOP AT lt_cell ASSIGNING <cell> WHERE row > lv_header_row.
        IF <cell>-row <> lv_row.
          lv_row = <cell>-row.
          lv_cols = lv_cols + 1.
        ENDIF.
        IF lv_cols > max_rows.
          lv_cut = lv_row.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_cut > 0.
        DELETE lt_cell WHERE row >= lv_cut.
      ENDIF.
      CLEAR lv_row.
      CLEAR lv_cols.
    ENDIF.
    " the columns from the first of the range on
    IF ls_area-col1 > 1.
      LOOP AT lt_cell ASSIGNING <cell>.
        <cell>-col = <cell>-col - ls_area-col1 + 1.
      ENDLOOP.
    ENDIF.
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
    IF lv_header_row > 0.
      LOOP AT lt_cell ASSIGNING <cell> WHERE row = lv_header_row. "#EC CI_SORTSEQ
        READ TABLE lt_name INDEX <cell>-col ASSIGNING <name>.
        <name> = name_by_header( <cell>-value ).
      ENDLOOP.
    ENDIF.
    LOOP AT lt_name ASSIGNING <name>.
      
      lv_tabix = sy-tabix.
      IF <name> IS INITIAL.
        <name> = column_to_letters( lv_tabix ).
      ENDIF.
      
      lv_base = <name>.
      
      lv_suffix = 1.
      DO.
        " the first column of that name - this one, when no earlier has it
        
        lv_first = 0.
        
        LOOP AT lt_name INTO lv_other.
          IF lv_other = <name>.
            lv_first = sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF lv_first = lv_tabix.
          EXIT.
        ENDIF.
        lv_suffix = lv_suffix + 1.
        
        lv_tail = |_{ lv_suffix }|.
        <name> = substring( val = lv_base
                            len = nmin( val1 = strlen( lv_base )
                                        val2 = 30 - strlen( lv_tail ) ) ) && lv_tail.
      ENDDO.
    ENDLOOP.

    result = zabaputil_cl_util_context=>rtti_create_tab_by_names( lt_name ).
    ASSIGN result->* TO <tab>.
    CREATE DATA lr_line LIKE LINE OF <tab>.
    ASSIGN lr_line->* TO <line>.

    LOOP AT lt_cell ASSIGNING <cell> WHERE row > lv_header_row. "#EC CI_SORTSEQ
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

    DATA lv_name TYPE string.
    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
    lv_name = zabaputil_cl_util_context=>c_trim_upper( val ).
    
    lv_len = strlen( lv_name ).
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = lv_name
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

    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
    lv_len = strlen( val ).
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = val
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

    DATA temp493 LIKE LINE OF t_attribute.
    DATA lr_attr LIKE REF TO temp493.
    LOOP AT t_attribute REFERENCE INTO lr_attr.
      IF lr_attr->name = name.
        result = lr_attr->value.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD path_folder.

    DATA lv_pos TYPE i.
    lv_pos = find( val = val
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
    DATA lv_path LIKE target.
    TYPES temp13 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_step TYPE temp13.
    DATA lv_step LIKE LINE OF lt_step.
    lv_path = target.
    IF lv_path IS NOT INITIAL AND lv_path(1) = `/`.
      lv_path = substring( val = lv_path
                           off = 1 ).
    ELSE.
      lv_path = path_folder( base ) && lv_path.
    ENDIF.

    

    SPLIT lv_path AT `/` INTO TABLE lt_step.
    
    LOOP AT lt_step INTO lv_step.
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
    DATA lv_len TYPE i.
      DATA lv_pos TYPE i.
      DATA lv_code TYPE string.

    result = val.
    IF result NS `_x`.
      RETURN.
    ENDIF.

    CLEAR result.
    
    lv_len = strlen( val ).
    DO.
      
      lv_pos = find( val = val
                           sub = `_x`
                           off = lv_off ).
      IF lv_pos < 0 OR lv_pos + 7 > lv_len.
        EXIT.
      ENDIF.
      
      lv_code = to_upper( substring( val = val
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
    DATA lv_text TYPE string.
    DATA lv_len TYPE i.
    DATA lv_base TYPE i.
      DATA lv_digit TYPE i.
    lv_text = to_upper( val ).
    
    lv_len = strlen( lv_text ).
    
    lv_base = 16.
    IF lv_text CO cv_digits AND lv_len <> 4.
      lv_base = 10.
    ENDIF.
    WHILE lv_pos < lv_len.
      
      lv_digit = find( val = cv_hex
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
            DATA temp494 TYPE xstring.

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
            
            temp494 = lv_hex.
            result = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp494 ).
          CATCH cx_root ##NO_HANDLER.
            " no converter - the text without that character
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

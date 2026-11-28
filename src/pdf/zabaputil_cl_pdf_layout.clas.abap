CLASS zabaputil_cl_pdf_layout DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Flowing content on top of zabaputil_cl_pdf: paragraphs that wrap at
    " the margin, tables that continue on the next page with their header
    " row repeated, a header and a footer line with page numbers. It keeps
    " a cursor - the y position of the next line - and adds a page when
    " the content reaches the bottom margin.
    "
    "   DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( lo_pdf ).
    "   lo_layout->set_font( font = lo_pdf->font( ) size = 10 ).
    "   lo_layout->text( `A paragraph ...` ).
    "   lo_layout->table( columns = lt_columns rows = lt_rows ).
    "   lo_layout->set_footer( `Page {page} of {pages}` ).
    "   lo_layout->finish( ).
    "
    " Lengths are in the unit of the document, font sizes in points. One
    " font per paragraph: no bold or italic words inside a line.

    TYPES:
      BEGIN OF ty_s_column,
        "! the component of the row the cell comes from; empty for the
        "! component at the position of the column
        name  TYPE string,
        "! the text of the header row
        title TYPE string,
        "! the width, 0 for an equal share of what the others leave
        width TYPE f,
        "! L, C or R
        align TYPE string,
      END OF ty_s_column.
    TYPES ty_t_column TYPE STANDARD TABLE OF ty_s_column WITH EMPTY KEY.
    "! the rows of a table as texts, a string_table per row
    TYPES ty_t_text_row TYPE STANDARD TABLE OF string_table WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF cs_border,
        "! the lines of every cell, the default
        grid TYPE string VALUE `GRID`,
        "! a line below every row only
        rows TYPE string VALUE `ROWS`,
        "! no lines
        none TYPE string VALUE `NONE`,
      END OF cs_border.

    "! A header or a footer: texts at the left margin, in the middle and at
    "! the right margin, and an image. A line feed in a text starts another
    "! line; the lines are centred in the margin. {page} is the number of
    "! the page, {pages} their count
    TYPES:
      BEGIN OF ty_s_area,
        left        TYPE string,
        center      TYPE string,
        right       TYPE string,
        "! the font size in points; 0 for 8
        size        TYPE f,
        "! an image, centred in the margin
        image       TYPE REF TO zabaputil_cl_pdf_image,
        "! L, C or R; L without it
        image_align TYPE string,
        "! the width of the image; 0 for what its height makes of it
        image_w     TYPE f,
        "! the height of the image; 0 for what its width makes of it, or
        "! the image at 72 dpi without both
        image_h     TYPE f,
      END OF ty_s_area.

    "! How a table looks and what it adds up - all of it off by default
    TYPES:
      BEGIN OF ty_s_table_options,
        "! the components whose values are added up: a total row at the end.
        "! Only table( ) adds up - it knows the values, table_text( ) the texts
        t_sum         TYPE string_table,
        "! the text of the total row, in its first column; `Total` without it
        total_text    TYPE string,
        "! a component: a subtotal row whenever its value changes
        group         TYPE string,
        "! the text of a subtotal row; `Subtotal` without it
        subtotal_text TYPE string,
        "! the sums so far at the bottom of a page and at the top of the next one
        carry         TYPE abap_bool,
        "! `Carried forward` without it
        carry_text    TYPE string,
        "! `Brought forward` without it
        brought_text  TYPE string,
        "! the fill colour of every second row, RRGGBB; empty for none
        zebra         TYPE string,
        "! one of cs_border; empty for GRID
        border        TYPE string,
        "! the colour of the lines, RRGGBB; empty for 999999
        border_color  TYPE string,
        "! the fill colour of the header row, RRGGBB; empty for E8E8E8
        header_fill   TYPE string,
      END OF ty_s_table_options.

    "! @parameter pdf | the document; the margins start at 20 mm on every side
    METHODS constructor
      IMPORTING
        pdf TYPE REF TO zabaputil_cl_pdf.

    "! The margins of the pages that follow, in the unit of the document
    METHODS set_margins
      IMPORTING
        left          TYPE numeric
        top           TYPE numeric
        right         TYPE numeric
        bottom        TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! The font of the content that follows
    "! @parameter font | from zabaputil_cl_pdf=&gt;font or =&gt;font_ttf
    "! @parameter size | the font size in points
    METHODS set_font
      IMPORTING
        font          TYPE REF TO zabaputil_if_pdf_font
        size          TYPE numeric DEFAULT 10
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! The text colour of the content that follows, `RRGGBB`
    METHODS set_color
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! The distance of two baselines as a multiple of the font size, 1.25
    "! to begin with
    METHODS set_line_height
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! A text, wrapped at the right margin; a line feed in it starts a new
    "! line. It continues on the next page when this one is full
    "! @parameter val   | the text
    "! @parameter align | L, C or R
    METHODS text
      IMPORTING
        val           TYPE clike
        align         TYPE clike DEFAULT `L`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! Empty space
    "! @parameter val | its height; 0 for one line of the current font
    METHODS add_space
      IMPORTING
        val           TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! A horizontal line from margin to margin
    METHODS rule
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! Continue on a new page
    METHODS page_break
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! A table with a header row and one row per line of an internal
    "! table. A cell wraps inside its column, a row is cut in two only when
    "! it is higher than a page, and the header row is repeated on every
    "! page the table reaches.
    "! A value is written as a string template writes it - format dates
    "! and amounts into text components first. A component that is no
    "! single value (a structure, a table, a reference) is an empty cell
    "! @parameter columns     | the columns, from left to right
    "! @parameter rows        | an internal table of structures
    "! @parameter header_font | the font of the header row and of the sums; the current font without it
    "! @parameter options     | sums, carry-over, zebra stripes, lines
    METHODS table
      IMPORTING
        columns       TYPE ty_t_column
        rows          TYPE ANY TABLE
        header_font   TYPE REF TO zabaputil_if_pdf_font OPTIONAL
        options       TYPE ty_s_table_options OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! The same table from texts: one row per line of rows, one text per
    "! column - for cells that are formatted already, or come from
    "! somewhere else than the components of a structure
    "! @parameter columns     | the columns, from left to right; name is not used
    "! @parameter rows        | the rows, each with the texts of its cells
    "! @parameter header_font | the font of the header row; the current font without it
    "! @parameter options     | zebra stripes and lines; the sums are table( )'s
    METHODS table_text
      IMPORTING
        columns       TYPE ty_t_column
        rows          TYPE ty_t_text_row
        header_font   TYPE REF TO zabaputil_if_pdf_font OPTIONAL
        options       TYPE ty_s_table_options OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! A line in the top margin of every page of this layout, written by
    "! finish( ). `{page}` is the number of the page, `{pages}` their count
    "! @parameter val   | the text
    "! @parameter align | L, C or R
    "! @parameter size  | the font size in points
    METHODS set_header
      IMPORTING
        val           TYPE clike
        align         TYPE clike DEFAULT `L`
        size          TYPE numeric DEFAULT 8
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! A line in the bottom margin of every page of this layout, written
    "! by finish( ). `{page}` is the number of the page, `{pages}` their count
    "! @parameter val   | the text
    "! @parameter align | L, C or R
    "! @parameter size  | the font size in points
    METHODS set_footer
      IMPORTING
        val           TYPE clike
        align         TYPE clike DEFAULT `C`
        size          TYPE numeric DEFAULT 8
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! A header of several lines and parts, with an image, in the top margin
    "! of every page - or of the first page only. It replaces what
    "! set_header( ) set
    "! @parameter val        | the texts and the image
    "! @parameter first_page | for the first page, which then has this one
    "!                         instead of the header of the others - an
    "!                         empty one for none
    METHODS set_header_area
      IMPORTING
        val           TYPE ty_s_area
        first_page    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! A footer of several lines and parts, with an image, in the bottom
    "! margin, as set_header_area( )
    METHODS set_footer_area
      IMPORTING
        val           TYPE ty_s_area
        first_page    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! Write header and footer onto every page - call it once, after the
    "! last content, when the number of pages is known
    METHODS finish.

    "! The page the cursor is on, for drawing at a fixed position; the
    "! first call creates the first page
    METHODS page
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The cursor: where the next line starts, from the top of the page
    METHODS y
      RETURNING
        VALUE(result) TYPE f.

    "! Move the cursor
    METHODS set_y
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! The lines a text breaks into at a width, in the current font. A
    "! word that is wider than the line is cut
    "! @parameter val   | the text, without line feeds
    "! @parameter width | the width of a line
    METHODS wrap
      IMPORTING
        val           TYPE clike
        width         TYPE numeric
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_cell,
        t_line TYPE string_table,
      END OF ty_s_cell.
    TYPES ty_t_cell TYPE STANDARD TABLE OF ty_s_cell WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF cs_kind,
        data     TYPE c LENGTH 1 VALUE `D`,
        header   TYPE c LENGTH 1 VALUE `H`,
        subtotal TYPE c LENGTH 1 VALUE `S`,
        total    TYPE c LENGTH 1 VALUE `T`,
        carry    TYPE c LENGTH 1 VALUE `C`,
      END OF cs_kind.

    "! a row of a table as it is printed: a row of the data or a sum
    TYPES:
      BEGIN OF ty_s_line,
        kind    TYPE c LENGTH 1,
        t_cell  TYPE string_table,
        "! of a data row: the sums before it, a cell per column - what a
        "! page that ends in front of it carries forward
        t_carry TYPE string_table,
      END OF ty_s_line.
    TYPES ty_t_line TYPE STANDARD TABLE OF ty_s_line WITH EMPTY KEY.

    " the lines of a rule and of the table cells: half a point, grey
    CONSTANTS c_line_width TYPE f VALUE '0.5'.
    CONSTANTS c_line_color TYPE string VALUE `999999`.

    DATA mo_pdf          TYPE REF TO zabaputil_cl_pdf.
    DATA mo_page         TYPE REF TO zabaputil_cl_pdf_page.
    DATA mt_page         TYPE STANDARD TABLE OF REF TO zabaputil_cl_pdf_page WITH EMPTY KEY.
    DATA mv_scale        TYPE f.
    DATA mv_left         TYPE f.
    DATA mv_top          TYPE f.
    DATA mv_right        TYPE f.
    DATA mv_bottom       TYPE f.
    DATA mv_y            TYPE f.
    DATA mo_font         TYPE REF TO zabaputil_if_pdf_font.
    DATA mv_size         TYPE f.
    DATA mv_factor       TYPE f.
    DATA mv_color        TYPE string.
    DATA ms_header       TYPE ty_s_area.
    DATA ms_footer       TYPE ty_s_area.
    DATA ms_header_first TYPE ty_s_area.
    DATA ms_footer_first TYPE ty_s_area.
    DATA mv_header_first TYPE abap_bool.
    DATA mv_footer_first TYPE abap_bool.
    DATA mv_finished     TYPE abap_bool.
    " the table that is being written: its columns with their final
    " widths, and the font of its header row - a page break inside the
    " table repeats the header
    DATA mt_column       TYPE ty_t_column.
    DATA mo_header_font  TYPE REF TO zabaputil_if_pdf_font.
    DATA ms_options      TYPE ty_s_table_options.
    " the sums before the row that is being written, for a carry-over
    DATA mt_carry        TYPE string_table.
    " data rows written, for the zebra stripes
    DATA mv_data_rows    TYPE i.

    METHODS new_page.

    "! Make sure a block of this height fits: a new page when it does
    "! not, unless the cursor is at the top of a page already
    METHODS ensure
      IMPORTING
        height TYPE f.

    "! The distance of two baselines, in the unit of the document
    METHODS line_height
      RETURNING
        VALUE(result) TYPE f.

    "! From the top of a line to its baseline: the letters are centred
    "! in the line
    METHODS baseline
      IMPORTING
        font          TYPE REF TO zabaputil_if_pdf_font
      RETURNING
        VALUE(result) TYPE f.

    METHODS font_check.

    METHODS wrap_font
      IMPORTING
        font          TYPE REF TO zabaputil_if_pdf_font
        val           TYPE string
        width         TYPE f
      RETURNING
        VALUE(result) TYPE string_table.

    METHODS table_header.

    "! The rows of a table, its sums among them
    METHODS table_lines
      IMPORTING
        columns     TYPE ty_t_column
        lines       TYPE ty_t_line
        header_font TYPE REF TO zabaputil_if_pdf_font OPTIONAL
        options     TYPE ty_s_table_options.

    METHODS table_row
      IMPORTING
        cells     TYPE string_table
        font      TYPE REF TO zabaputil_if_pdf_font
        is_header TYPE abap_bool DEFAULT abap_false
        kind      TYPE c DEFAULT `D`.

    "! The height a row of these cells needs
    METHODS row_height
      IMPORTING
        cells         TYPE string_table
        font          TYPE REF TO zabaputil_if_pdf_font
      RETURNING
        VALUE(result) TYPE f.

    "! The row of a carry-over: its text in the first column, the sums
    METHODS carry_cells
      IMPORTING
        text          TYPE string
      RETURNING
        VALUE(result) TYPE string_table.

    "! The table goes on on the next page: the sums so far below the last
    "! row, the header row and the sums again on the new page
    METHODS table_page_break
      IMPORTING
        with_carry TYPE abap_bool.

    "! One piece of a row: the lines from first to last of every cell
    METHODS row_piece
      IMPORTING
        cells     TYPE ty_t_cell
        first     TYPE i
        last      TYPE i
        font      TYPE REF TO zabaputil_if_pdf_font
        kind      TYPE c.

    "! A text that is the value of a component of a row, as a string
    "! template writes it; empty for a structure, a table or a reference
    CLASS-METHODS cell_text
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    METHODS margin_line
      IMPORTING
        page  TYPE REF TO zabaputil_cl_pdf_page
        val   TYPE string
        align TYPE string
        size  TYPE f
        y     TYPE f.

    "! A header or a footer on a page
    "! @parameter middle | the middle of the margin, from the top of the page
    METHODS margin_area
      IMPORTING
        page   TYPE REF TO zabaputil_cl_pdf_page
        area   TYPE ty_s_area
        number TYPE i
        middle TYPE f.

    CLASS-METHODS area_is_empty
      IMPORTING
        area          TYPE ty_s_area
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS zabaputil_cl_pdf_layout IMPLEMENTATION.

  METHOD constructor.

    mo_pdf = pdf.
    mv_scale = pdf->scale( ).

    " 20 mm, in the unit of the document
    mv_left = 20 * 720.
    mv_left = mv_left / 254 / mv_scale.
    mv_top = mv_left.
    mv_right = mv_left.
    mv_bottom = mv_left.

    mv_size = 10.
    mv_factor = 5.
    mv_factor = mv_factor / 4.
    mv_color = `000000`.

  ENDMETHOD.

  METHOD set_margins.

    mv_left = left.
    mv_top = top.
    mv_right = right.
    mv_bottom = bottom.
    result = me.

  ENDMETHOD.

  METHOD set_font.

    mo_font = font.
    mv_size = size.
    result = me.

  ENDMETHOD.

  METHOD set_color.

    mv_color = val.
    result = me.

  ENDMETHOD.

  METHOD set_line_height.

    mv_factor = val.
    result = me.

  ENDMETHOD.

  METHOD font_check.

    IF mo_font IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: set a font before content is added to the layout (set_font)`.
    ENDIF.

  ENDMETHOD.

  METHOD new_page.

    mo_page = mo_pdf->add_page( ).
    INSERT mo_page INTO TABLE mt_page.
    mv_y = mv_top.

  ENDMETHOD.

  METHOD ensure.

    CONSTANTS lc_tolerance TYPE f VALUE '0.001'.

    IF mo_page IS NOT BOUND.
      new_page( ).
      RETURN.
    ENDIF.
    IF mv_y + height > mo_page->height( ) - mv_bottom + lc_tolerance AND mv_y > mv_top + lc_tolerance.
      new_page( ).
    ENDIF.

  ENDMETHOD.

  METHOD line_height.

    result = mv_size * mv_factor / mv_scale.

  ENDMETHOD.

  METHOD baseline.

    DATA(lv_ascent) = font->ascent( mv_size ) / mv_scale.
    DATA(lv_descent) = font->descent( mv_size ) / mv_scale.
    result = ( line_height( ) - lv_ascent - lv_descent ) / 2 + lv_ascent.

  ENDMETHOD.

  METHOD page.

    IF mo_page IS NOT BOUND.
      new_page( ).
    ENDIF.
    result = mo_page.

  ENDMETHOD.

  METHOD y.

    result = mv_y.

  ENDMETHOD.

  METHOD set_y.

    page( ).
    mv_y = val.
    result = me.

  ENDMETHOD.

  METHOD page_break.

    " an empty first page is a page, too
    page( ).
    new_page( ).
    result = me.

  ENDMETHOD.

  METHOD add_space.

    DATA lv_height TYPE f.

    lv_height = val.
    IF lv_height <= 0.
      lv_height = line_height( ).
    ENDIF.
    page( ).
    mv_y = mv_y + lv_height.
    result = me.

  ENDMETHOD.

  METHOD rule.

    DATA(lv_gap) = line_height( ) / 2.

    ensure( lv_gap * 2 ).
    mv_y = mv_y + lv_gap.
    mo_page->set_draw_color( c_line_color ).
    mo_page->set_line_width( c_line_width ).
    mo_page->line( x1 = mv_left
                   y1 = mv_y
                   x2 = mo_page->width( ) - mv_right
                   y2 = mv_y ).
    mv_y = mv_y + lv_gap.
    result = me.

  ENDMETHOD.

  METHOD wrap.

    DATA lv_width TYPE f.

    font_check( ).
    lv_width = width.
    result = wrap_font( font  = mo_font
                        val   = CONV string( val )
                        width = lv_width ).

  ENDMETHOD.

  METHOD wrap_font.

    DATA lv_line       TYPE string.
    DATA lv_line_width TYPE f.
    DATA lv_word_width TYPE f.
    DATA lv_char       TYPE string.
    DATA lv_char_width TYPE f.
    DATA lv_off        TYPE i.

    " widths in points; a word is measured once, and a line is the sum
    " of its words and the blanks between them
    DATA(lv_max) = width * mv_scale.
    DATA(lv_blank) = font->width( val  = ` `
                                  size = mv_size ).

    SPLIT val AT ` ` INTO TABLE DATA(lt_word).
    LOOP AT lt_word INTO DATA(lv_word).

      IF lv_word IS INITIAL.
        CONTINUE.
      ENDIF.
      lv_word_width = font->width( val  = lv_word
                                   size = mv_size ).

      IF lv_word_width > lv_max.
        " wider than a line of its own: cut it, character by character
        IF lv_line IS NOT INITIAL.
          INSERT lv_line INTO TABLE result.
        ENDIF.
        CLEAR lv_line.
        lv_line_width = 0.
        lv_off = 0.
        WHILE lv_off < strlen( lv_word ).
          lv_char = substring( val = lv_word
                               off = lv_off
                               len = 1 ).
          lv_char_width = font->width( val  = lv_char
                                       size = mv_size ).
          IF lv_line_width + lv_char_width > lv_max AND lv_line IS NOT INITIAL.
            INSERT lv_line INTO TABLE result.
            CLEAR lv_line.
            lv_line_width = 0.
          ENDIF.
          lv_line = lv_line && lv_char.
          lv_line_width = lv_line_width + lv_char_width.
          lv_off = lv_off + 1.
        ENDWHILE.
        CONTINUE.
      ENDIF.

      IF lv_line IS INITIAL.
        lv_line = lv_word.
        lv_line_width = lv_word_width.
      ELSEIF lv_line_width + lv_blank + lv_word_width <= lv_max.
        lv_line = lv_line && ` ` && lv_word.
        lv_line_width = lv_line_width + lv_blank + lv_word_width.
      ELSE.
        INSERT lv_line INTO TABLE result.
        lv_line = lv_word.
        lv_line_width = lv_word_width.
      ENDIF.

    ENDLOOP.

    IF lv_line IS NOT INITIAL.
      INSERT lv_line INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD text.

    DATA lv_x TYPE f.

    result = me.
    font_check( ).

    DATA(lv_text) = CONV string( val ).
    DATA(lv_line_feed) = |\n|.
    lv_text = replace( val  = lv_text
                       sub  = |\r\n|
                       with = lv_line_feed
                       occ  = 0 ).
    SPLIT lv_text AT lv_line_feed INTO TABLE DATA(lt_paragraph).

    DATA(lv_align) = to_upper( align ).
    DATA(lv_height) = line_height( ).

    LOOP AT lt_paragraph INTO DATA(lv_paragraph).

      " the width of this page - a page break inside the text may change it
      page( ).
      DATA(lt_line) = wrap_font( font  = mo_font
                                 val   = lv_paragraph
                                 width = mo_page->width( ) - mv_left - mv_right ).
      IF lt_line IS INITIAL.
        INSERT `` INTO TABLE lt_line.
      ENDIF.

      LOOP AT lt_line INTO DATA(lv_line).
        ensure( lv_height ).
        CASE lv_align.
          WHEN `R`.
            lv_x = mo_page->width( ) - mv_right.
          WHEN `C`.
            lv_x = ( mv_left + mo_page->width( ) - mv_right ) / 2.
          WHEN OTHERS.
            lv_x = mv_left.
        ENDCASE.
        mo_page->set_font( font = mo_font
                           size = mv_size ).
        mo_page->set_text_color( mv_color ).
        mo_page->text( x     = lv_x
                       y     = mv_y + baseline( mo_font )
                       val   = lv_line
                       align = lv_align ).
        mv_y = mv_y + lv_height.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD table.

    DATA lt_line   TYPE ty_t_line.
    DATA ls_line   TYPE ty_s_line.
    DATA ls_sum    TYPE ty_s_line.
    DATA lv_pass   TYPE i.
    DATA lv_index  TYPE i.
    DATA lv_group  TYPE string.
    DATA lv_last   TYPE string.
    DATA lv_first  TYPE abap_bool VALUE abap_true.
    DATA lt_sum    TYPE STANDARD TABLE OF REF TO data WITH EMPTY KEY.
    DATA lt_sub    TYPE STANDARD TABLE OF REF TO data WITH EMPTY KEY.
    DATA lr_sum    TYPE REF TO data.
    DATA lt_is_sum TYPE STANDARD TABLE OF abap_bool WITH EMPTY KEY.
    DATA lv_any    TYPE abap_bool.

    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <value> TYPE any.
    FIELD-SYMBOLS <sum>   TYPE any.
    FIELD-SYMBOLS <sub>   TYPE any.
    FIELD-SYMBOLS <group> TYPE any.

    " the columns that are added up: by the name of their component
    LOOP AT columns ASSIGNING FIELD-SYMBOL(<column>).
      DATA(lv_sum) = abap_false.
      LOOP AT options-t_sum INTO DATA(lv_name).
        IF to_upper( condense( lv_name ) ) = to_upper( condense( <column>-name ) ) AND <column>-name IS NOT INITIAL.
          lv_sum = abap_true.
        ENDIF.
      ENDLOOP.
      INSERT lv_sum INTO TABLE lt_is_sum.
      IF lv_sum = abap_true.
        lv_any = abap_true.
      ENDIF.
      INSERT lr_sum INTO TABLE lt_sum.
      INSERT lr_sum INTO TABLE lt_sub.
    ENDLOOP.

    LOOP AT rows ASSIGNING <row>.
      CLEAR ls_line.
      ls_line-kind = cs_kind-data.

      " a new group: the subtotal of the one before
      IF options-group IS NOT INITIAL AND lv_any = abap_true.
        UNASSIGN <group>.
        ASSIGN COMPONENT to_upper( options-group ) OF STRUCTURE <row> TO <group>.
        CLEAR lv_group.
        IF <group> IS ASSIGNED.
          lv_group = cell_text( <group> ).
        ENDIF.
        IF lv_first = abap_false AND lv_group <> lv_last.
          CLEAR ls_sum.
          ls_sum-kind = cs_kind-subtotal.
          LOOP AT lt_sub INTO lr_sum.
            lv_index = sy-tabix.
            IF lr_sum IS BOUND.
              ASSIGN lr_sum->* TO <sub>.
              INSERT cell_text( <sub> ) INTO TABLE ls_sum-t_cell.
              CLEAR <sub>.
            ELSE.
              INSERT `` INTO TABLE ls_sum-t_cell.
            ENDIF.
            lr_sum = lt_sum[ lv_index ].
            IF lr_sum IS BOUND.
              ASSIGN lr_sum->* TO <sum>.
              INSERT cell_text( <sum> ) INTO TABLE ls_sum-t_carry.
            ELSE.
              INSERT `` INTO TABLE ls_sum-t_carry.
            ENDIF.
          ENDLOOP.
          IF lt_is_sum[ 1 ] = abap_false.
            ls_sum-t_cell[ 1 ] = COND #( WHEN options-subtotal_text IS INITIAL
                                         THEN `Subtotal` ELSE options-subtotal_text ).
          ENDIF.
          INSERT ls_sum INTO TABLE lt_line.
        ENDIF.
        lv_last = lv_group.
        lv_first = abap_false.
      ENDIF.

      " the sums before the row, for the page that ends in front of it
      IF lv_any = abap_true.
        LOOP AT lt_sum INTO lr_sum.
          IF lr_sum IS BOUND.
            ASSIGN lr_sum->* TO <sum>.
            INSERT cell_text( <sum> ) INTO TABLE ls_line-t_carry.
          ELSE.
            INSERT `` INTO TABLE ls_line-t_carry.
          ENDIF.
        ENDLOOP.
      ENDIF.

      LOOP AT columns ASSIGNING <column>.
        lv_index = sy-tabix.
        UNASSIGN <value>.
        IF <column>-name IS INITIAL.
          ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO <value>.
        ELSE.
          ASSIGN COMPONENT <column>-name OF STRUCTURE <row> TO <value>.
        ENDIF.
        IF <value> IS ASSIGNED.
          INSERT cell_text( <value> ) INTO TABLE ls_line-t_cell.
        ELSE.
          INSERT `` INTO TABLE ls_line-t_cell.
        ENDIF.

        " added up in a field of the type of the value: the sum is written
        " as the values are
        IF <value> IS ASSIGNED AND lt_is_sum[ lv_index ] = abap_true.
          DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( <value> ).
          IF lv_kind CA `bsI8PFae`.
            IF lt_sum[ lv_index ] IS NOT BOUND.
              CREATE DATA lr_sum LIKE <value>.
              lt_sum[ lv_index ] = lr_sum.
              CREATE DATA lr_sum LIKE <value>.
              lt_sub[ lv_index ] = lr_sum.
            ENDIF.
            lr_sum = lt_sum[ lv_index ].
            ASSIGN lr_sum->* TO <sum>.
            <sum> = <sum> + <value>.
            lr_sum = lt_sub[ lv_index ].
            ASSIGN lr_sum->* TO <sub>.
            <sub> = <sub> + <value>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      INSERT ls_line INTO TABLE lt_line.
    ENDLOOP.

    IF lv_any = abap_true AND lines( lt_line ) > 0.
      " the subtotal of the last group, and the total
      DO 2 TIMES.
        lv_pass = sy-index.
        IF lv_pass = 1 AND options-group IS INITIAL.
          CONTINUE.
        ENDIF.
        CLEAR ls_line.
        ls_line-kind = COND #( WHEN lv_pass = 1 THEN cs_kind-subtotal ELSE cs_kind-total ).
        LOOP AT columns ASSIGNING <column>.
          lv_index = sy-tabix.
          IF lv_pass = 1.
            lr_sum = lt_sub[ lv_index ].
          ELSE.
            lr_sum = lt_sum[ lv_index ].
          ENDIF.
          IF lr_sum IS BOUND.
            ASSIGN lr_sum->* TO <sum>.
            INSERT cell_text( <sum> ) INTO TABLE ls_line-t_cell.
          ELSE.
            INSERT `` INTO TABLE ls_line-t_cell.
          ENDIF.
          lr_sum = lt_sum[ lv_index ].
          IF lr_sum IS BOUND.
            ASSIGN lr_sum->* TO <sum>.
            INSERT cell_text( <sum> ) INTO TABLE ls_line-t_carry.
          ELSE.
            INSERT `` INTO TABLE ls_line-t_carry.
          ENDIF.
        ENDLOOP.
        IF lt_is_sum[ 1 ] = abap_false.
          IF ls_line-kind = cs_kind-total.
            ls_line-t_cell[ 1 ] = COND #( WHEN options-total_text IS INITIAL THEN `Total` ELSE options-total_text ).
          ELSE.
            ls_line-t_cell[ 1 ] = COND #( WHEN options-subtotal_text IS INITIAL THEN `Subtotal` ELSE options-subtotal_text ).
          ENDIF.
        ENDIF.
        INSERT ls_line INTO TABLE lt_line.
      ENDDO.
    ENDIF.

    table_lines( columns     = columns
                 lines       = lt_line
                 header_font = header_font
                 options     = options ).
    result = me.

  ENDMETHOD.

  METHOD cell_text.

    " a component that is a structure, a table or a reference is no
    " text - and a string template of one is a runtime error
    DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( val ).
    IF lv_kind = `u` OR lv_kind = `v` OR lv_kind = `h` OR lv_kind = `l` OR lv_kind = `r`.
      RETURN.
    ENDIF.
    result = |{ val }|.

  ENDMETHOD.

  METHOD table_text.

    DATA lt_line TYPE ty_t_line.

    LOOP AT rows ASSIGNING FIELD-SYMBOL(<row>).
      INSERT VALUE #( kind   = cs_kind-data
                      t_cell = <row> ) INTO TABLE lt_line.
    ENDLOOP.
    table_lines( columns     = columns
                 lines       = lt_line
                 header_font = header_font
                 options     = options ).
    result = me.

  ENDMETHOD.

  METHOD table_lines.

    DATA lv_fixed TYPE f.
    DATA lv_open  TYPE i.
    DATA lv_share TYPE f.
    DATA lv_title TYPE abap_bool.

    font_check( ).
    IF columns IS INITIAL.
      RETURN.
    ENDIF.
    page( ).

    " a column without a width takes an equal share of what is left
    mt_column = columns.
    LOOP AT mt_column ASSIGNING FIELD-SYMBOL(<column>).
      IF <column>-width > 0.
        lv_fixed = lv_fixed + <column>-width.
      ELSE.
        lv_open = lv_open + 1.
      ENDIF.
      IF <column>-title IS NOT INITIAL.
        lv_title = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_open > 0.
      lv_share = ( mo_page->width( ) - mv_left - mv_right - lv_fixed ) / lv_open.
      IF lv_share < 0.
        lv_share = 0.
      ENDIF.
      LOOP AT mt_column ASSIGNING <column>.
        IF <column>-width <= 0.
          <column>-width = lv_share.
        ENDIF.
      ENDLOOP.
    ENDIF.

    ms_options = options.
    ms_options-border = to_upper( ms_options-border ).
    mv_data_rows = 0.
    CLEAR mt_carry.

    CLEAR mo_header_font.
    IF lv_title = abap_true.
      mo_header_font = header_font.
      IF mo_header_font IS NOT BOUND.
        mo_header_font = mo_font.
      ENDIF.
      table_header( ).
    ENDIF.

    DATA(lo_sum_font) = header_font.
    IF lo_sum_font IS NOT BOUND.
      lo_sum_font = mo_font.
    ENDIF.

    LOOP AT lines ASSIGNING FIELD-SYMBOL(<line>).
      mt_carry = <line>-t_carry.
      IF <line>-kind = cs_kind-data.
        table_row( cells = <line>-t_cell
                   font  = mo_font
                   kind  = <line>-kind ).
      ELSE.
        table_row( cells = <line>-t_cell
                   font  = lo_sum_font
                   kind  = <line>-kind ).
      ENDIF.
    ENDLOOP.

    CLEAR mt_column.
    CLEAR mo_header_font.
    CLEAR ms_options.
    CLEAR mt_carry.

  ENDMETHOD.

  METHOD table_header.

    DATA lt_cell TYPE string_table.

    LOOP AT mt_column ASSIGNING FIELD-SYMBOL(<column>).
      INSERT <column>-title INTO TABLE lt_cell.
    ENDLOOP.
    table_row( cells     = lt_cell
               font      = mo_header_font
               is_header = abap_true
               kind      = cs_kind-header ).

  ENDMETHOD.

  METHOD carry_cells.

    LOOP AT mt_column ASSIGNING FIELD-SYMBOL(<column>).
      IF sy-tabix <= lines( mt_carry ).
        INSERT mt_carry[ sy-tabix ] INTO TABLE result.
      ELSE.
        INSERT `` INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF result IS NOT INITIAL AND result[ 1 ] IS INITIAL.
      result[ 1 ] = text.
    ENDIF.

  ENDMETHOD.

  METHOD row_height.

    DATA lv_lines TYPE i VALUE 1.
    DATA lv_cell  TYPE string.

    DATA(lv_pad) = 3 / mv_scale.
    LOOP AT mt_column ASSIGNING FIELD-SYMBOL(<column>).
      CLEAR lv_cell.
      IF sy-tabix <= lines( cells ).
        lv_cell = cells[ sy-tabix ].
      ENDIF.
      DATA(lt_wrapped) = wrap_font( font  = font
                                    val   = lv_cell
                                    width = <column>-width - lv_pad * 2 ).
      IF lines( lt_wrapped ) > lv_lines.
        lv_lines = lines( lt_wrapped ).
      ENDIF.
    ENDLOOP.
    result = lv_lines * line_height( ) + lv_pad * 2.

  ENDMETHOD.

  METHOD table_page_break.

    DATA(lo_sum_font) = mo_header_font.
    IF lo_sum_font IS NOT BOUND.
      lo_sum_font = mo_font.
    ENDIF.

    IF with_carry = abap_true.
      table_row( cells = carry_cells( COND #( WHEN ms_options-carry_text IS INITIAL
                                              THEN `Carried forward` ELSE ms_options-carry_text ) )
                 font  = lo_sum_font
                 kind  = cs_kind-carry ).
    ENDIF.
    new_page( ).
    IF mo_header_font IS BOUND.
      table_header( ).
    ENDIF.
    IF with_carry = abap_true.
      table_row( cells = carry_cells( COND #( WHEN ms_options-brought_text IS INITIAL
                                              THEN `Brought forward` ELSE ms_options-brought_text ) )
                 font  = lo_sum_font
                 kind  = cs_kind-carry ).
    ENDIF.

  ENDMETHOD.

  METHOD table_row.

    DATA lt_wrapped TYPE ty_t_cell.
    DATA ls_wrapped TYPE ty_s_cell.
    DATA lv_lines   TYPE i VALUE 1.
    DATA lv_index   TYPE i.
    DATA lv_cell    TYPE string.
    DATA lv_first   TYPE i VALUE 1.
    DATA lv_fit     TYPE i.
    DATA lv_reserve TYPE f.
    DATA lv_fresh   TYPE f.

    CONSTANTS lc_tolerance TYPE f VALUE '0.001'.

    " three points between the text and the lines of the cell
    DATA(lv_pad) = 3 / mv_scale.
    DATA(lv_height) = line_height( ).

    LOOP AT mt_column ASSIGNING FIELD-SYMBOL(<column>).
      lv_index = sy-tabix.
      CLEAR lv_cell.
      IF lv_index <= lines( cells ).
        lv_cell = cells[ lv_index ].
      ENDIF.
      ls_wrapped-t_line = wrap_font( font  = font
                                     val   = lv_cell
                                     width = <column>-width - lv_pad * 2 ).
      IF lines( ls_wrapped-t_line ) > lv_lines.
        lv_lines = lines( ls_wrapped-t_line ).
      ENDIF.
      INSERT ls_wrapped INTO TABLE lt_wrapped.
    ENDLOOP.

    " a data row leaves room for the carry-over below it
    DATA(lv_carry) = xsdbool( ms_options-carry = abap_true AND mt_carry IS NOT INITIAL
                              AND kind <> cs_kind-header AND kind <> cs_kind-carry ).
    IF lv_carry = abap_true.
      lv_reserve = row_height( cells = carry_cells( ms_options-carry_text )
                               font  = COND #( WHEN mo_header_font IS BOUND THEN mo_header_font ELSE mo_font ) ).
    ENDIF.

    " a row stays in one piece: when it does not fit, the table goes on
    " at the top of the next page, under a header row of its own. Only a
    " row higher than a page is cut, between two of its lines
    DO.
      DATA(lv_rest) = lv_lines - lv_first + 1.
      DATA(lv_bottom) = mo_page->height( ) - mv_bottom - lv_reserve.
      " the header row goes to the next page whole, the row of a carry-over
      " has its room
      IF is_header = abap_true AND mv_y + lv_rest * lv_height + lv_pad * 2 > lv_bottom + lc_tolerance
          AND mv_y > mv_top + lc_tolerance.
        new_page( ).
      ENDIF.
      IF mv_y + lv_rest * lv_height + lv_pad * 2 <= lv_bottom + lc_tolerance OR is_header = abap_true
          OR kind = cs_kind-carry.
        row_piece( cells = lt_wrapped
                   first = lv_first
                   last  = lv_lines
                   font  = font
                   kind  = kind ).
        EXIT.
      ENDIF.

      " what a fresh page has room for, below its header row
      lv_fresh = mo_page->height( ) - mv_bottom - lv_reserve - mv_top.
      IF mo_header_font IS BOUND.
        lv_fresh = lv_fresh - row_height( cells = VALUE #( FOR c IN mt_column ( c-title ) )
                                          font  = mo_header_font ).
      ENDIF.
      IF lv_carry = abap_true.
        lv_fresh = lv_fresh - lv_reserve.
      ENDIF.
      IF mv_y > mv_top + lc_tolerance AND lv_rest * lv_height + lv_pad * 2 <= lv_fresh + lc_tolerance.
        table_page_break( lv_carry ).
        CONTINUE.
      ENDIF.

      " higher than a page: as many lines as fit here, the rest on the next
      lv_fit = ( lv_bottom - mv_y - lv_pad * 2 + lc_tolerance ) / lv_height - 1 / 2.
      IF lv_fit < 1 AND mv_y > mv_top + lc_tolerance.
        table_page_break( lv_carry ).
        CONTINUE.
      ENDIF.
      IF lv_fit < 1.
        lv_fit = 1.
      ENDIF.
      row_piece( cells = lt_wrapped
                 first = lv_first
                 last  = lv_first + lv_fit - 1
                 font  = font
                 kind  = kind ).
      lv_first = lv_first + lv_fit.
      table_page_break( lv_carry ).
    ENDDO.

    IF kind = cs_kind-data.
      mv_data_rows = mv_data_rows + 1.
    ENDIF.

  ENDMETHOD.

  METHOD row_piece.

    DATA lv_x      TYPE f.
    DATA lv_text_x TYPE f.
    DATA lv_text_y TYPE f.
    DATA lv_index  TYPE i.
    DATA lv_align  TYPE string.
    DATA lv_fill   TYPE string.
    DATA lv_width  TYPE f.

    DATA(lv_pad) = 3 / mv_scale.
    DATA(lv_height) = line_height( ).
    DATA(lv_row_height) = ( last - first + 1 ) * lv_height + lv_pad * 2.

    DATA(lv_color) = ms_options-border_color.
    IF lv_color IS INITIAL.
      lv_color = c_line_color.
    ENDIF.
    mo_page->set_draw_color( lv_color ).
    mo_page->set_line_width( c_line_width ).
    mo_page->set_font( font = font
                       size = mv_size ).
    mo_page->set_text_color( mv_color ).

    " the header row is grey, every second data row striped when asked
    IF kind = cs_kind-header.
      lv_fill = ms_options-header_fill.
      IF lv_fill IS INITIAL.
        lv_fill = `E8E8E8`.
      ENDIF.
    ELSEIF kind = cs_kind-data AND ms_options-zebra IS NOT INITIAL AND mv_data_rows MOD 2 = 1.
      lv_fill = ms_options-zebra.
    ENDIF.

    lv_x = mv_left.
    LOOP AT mt_column ASSIGNING FIELD-SYMBOL(<column>).
      lv_index = sy-tabix.

      IF lv_fill IS NOT INITIAL.
        mo_page->set_fill_color( lv_fill ).
      ENDIF.
      IF ms_options-border = cs_border-grid OR ms_options-border IS INITIAL.
        mo_page->rect( x     = lv_x
                       y     = mv_y
                       w     = <column>-width
                       h     = lv_row_height
                       style = COND #( WHEN lv_fill IS INITIAL THEN `D` ELSE `DF` ) ).
      ELSEIF lv_fill IS NOT INITIAL.
        mo_page->rect( x     = lv_x
                       y     = mv_y
                       w     = <column>-width
                       h     = lv_row_height
                       style = `F` ).
      ENDIF.

      lv_align = to_upper( <column>-align ).
      CASE lv_align.
        WHEN `R`.
          lv_text_x = lv_x + <column>-width - lv_pad.
        WHEN `C`.
          lv_text_x = lv_x + <column>-width / 2.
        WHEN OTHERS.
          lv_text_x = lv_x + lv_pad.
      ENDCASE.

      lv_text_y = mv_y + lv_pad + baseline( font ).
      ASSIGN cells[ lv_index ] TO FIELD-SYMBOL(<wrapped>).
      LOOP AT <wrapped>-t_line INTO DATA(lv_line) FROM first TO last.
        mo_page->text( x     = lv_text_x
                       y     = lv_text_y
                       val   = lv_line
                       align = lv_align ).
        lv_text_y = lv_text_y + lv_height.
      ENDLOOP.

      lv_x = lv_x + <column>-width.
      lv_width = lv_width + <column>-width.
    ENDLOOP.

    IF ms_options-border = cs_border-rows.
      mo_page->line( x1 = mv_left
                     y1 = mv_y + lv_row_height
                     x2 = mv_left + lv_width
                     y2 = mv_y + lv_row_height ).
    ENDIF.

    mv_y = mv_y + lv_row_height.

  ENDMETHOD.

  METHOD set_header.

    " one line in one place: a header area of one part
    CLEAR ms_header.
    DATA(lv_align) = to_upper( align ).
    CASE lv_align.
      WHEN `R`.
        ms_header-right = val.
      WHEN `C`.
        ms_header-center = val.
      WHEN OTHERS.
        ms_header-left = val.
    ENDCASE.
    ms_header-size = size.
    result = me.

  ENDMETHOD.

  METHOD set_footer.

    CLEAR ms_footer.
    DATA(lv_align) = to_upper( align ).
    CASE lv_align.
      WHEN `R`.
        ms_footer-right = val.
      WHEN `C`.
        ms_footer-center = val.
      WHEN OTHERS.
        ms_footer-left = val.
    ENDCASE.
    ms_footer-size = size.
    result = me.

  ENDMETHOD.

  METHOD set_header_area.

    IF first_page = abap_true.
      ms_header_first = val.
      mv_header_first = abap_true.
    ELSE.
      ms_header = val.
    ENDIF.
    result = me.

  ENDMETHOD.

  METHOD set_footer_area.

    IF first_page = abap_true.
      ms_footer_first = val.
      mv_footer_first = abap_true.
    ELSE.
      ms_footer = val.
    ENDIF.
    result = me.

  ENDMETHOD.

  METHOD area_is_empty.

    result = xsdbool( area-left IS INITIAL AND area-center IS INITIAL AND area-right IS INITIAL
                      AND area-image IS NOT BOUND ).

  ENDMETHOD.

  METHOD finish.

    DATA lv_number TYPE i.
    DATA ls_header TYPE ty_s_area.
    DATA ls_footer TYPE ty_s_area.

    IF mv_finished = abap_true.
      RETURN.
    ENDIF.
    mv_finished = abap_true.

    IF area_is_empty( ms_header ) = abap_true AND area_is_empty( ms_footer ) = abap_true
        AND area_is_empty( ms_header_first ) = abap_true AND area_is_empty( ms_footer_first ) = abap_true.
      RETURN.
    ENDIF.
    font_check( ).

    LOOP AT mt_page INTO DATA(lo_page).
      lv_number = sy-tabix.

      ls_header = ms_header.
      ls_footer = ms_footer.
      IF lv_number = 1 AND mv_header_first = abap_true.
        ls_header = ms_header_first.
      ENDIF.
      IF lv_number = 1 AND mv_footer_first = abap_true.
        ls_footer = ms_footer_first.
      ENDIF.

      margin_area( page   = lo_page
                   area   = ls_header
                   number = lv_number
                   middle = mv_top / 2 ).
      margin_area( page   = lo_page
                   area   = ls_footer
                   number = lv_number
                   middle = lo_page->height( ) - mv_bottom / 2 ).

    ENDLOOP.

  ENDMETHOD.

  METHOD margin_area.

    DATA lv_text   TYPE string.
    DATA lv_y      TYPE f.
    DATA lv_size   TYPE f.
    DATA lv_height TYPE f.
    DATA lv_w      TYPE f.
    DATA lv_h      TYPE f.
    DATA lv_x      TYPE f.
    DATA lv_align  TYPE string.

    IF area_is_empty( area ) = abap_true.
      RETURN.
    ENDIF.

    lv_size = area-size.
    IF lv_size <= 0.
      lv_size = 8.
    ENDIF.
    lv_height = lv_size * mv_factor / mv_scale.

    " the image first: the texts are written over it
    IF area-image IS BOUND.
      lv_w = area-image_w.
      lv_h = area-image_h.
      IF lv_w <= 0 AND lv_h <= 0.
        lv_w = area-image->width( ) / mv_scale.
        lv_h = area-image->height( ) / mv_scale.
      ELSEIF lv_h <= 0.
        lv_h = lv_w * area-image->height( ) / area-image->width( ).
      ELSEIF lv_w <= 0.
        lv_w = lv_h * area-image->width( ) / area-image->height( ).
      ENDIF.
      lv_align = to_upper( area-image_align ).
      CASE lv_align.
        WHEN `R`.
          lv_x = page->width( ) - mv_right - lv_w.
        WHEN `C`.
          lv_x = ( mv_left + page->width( ) - mv_right - lv_w ) / 2.
        WHEN OTHERS.
          lv_x = mv_left.
      ENDCASE.
      page->image( image = area-image
                   x     = lv_x
                   y     = middle - lv_h / 2
                   w     = lv_w
                   h     = lv_h ).
    ENDIF.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_text = area-left.
          lv_align = `L`.
        WHEN 2.
          lv_text = area-center.
          lv_align = `C`.
        WHEN 3.
          lv_text = area-right.
          lv_align = `R`.
      ENDCASE.
      IF lv_text IS INITIAL.
        CONTINUE.
      ENDIF.
      lv_text = replace( val  = lv_text
                         sub  = `{pages}`
                         with = |{ lines( mt_page ) }|
                         occ  = 0 ).
      lv_text = replace( val  = lv_text
                         sub  = `{page}`
                         with = |{ number }|
                         occ  = 0 ).
      lv_text = replace( val  = lv_text
                         sub  = |\r\n|
                         with = |\n|
                         occ  = 0 ).
      SPLIT lv_text AT |\n| INTO TABLE DATA(lt_line).

      " the lines centred around the middle of the margin: one line has
      " its baseline there
      lv_y = middle - ( lines( lt_line ) - 1 ) * lv_height / 2.
      LOOP AT lt_line INTO DATA(lv_line).
        margin_line( page  = page
                     val   = lv_line
                     align = lv_align
                     size  = lv_size
                     y     = lv_y ).
        lv_y = lv_y + lv_height.
      ENDLOOP.
    ENDDO.

  ENDMETHOD.

  METHOD margin_line.

    DATA lv_x TYPE f.

    CASE align.
      WHEN `R`.
        lv_x = page->width( ) - mv_right.
      WHEN `C`.
        lv_x = ( mv_left + page->width( ) - mv_right ) / 2.
      WHEN OTHERS.
        lv_x = mv_left.
    ENDCASE.

    page->set_font( font = mo_font
                    size = size ).
    page->set_text_color( mv_color ).
    page->text( x     = lv_x
                y     = y
                val   = val
                align = align ).

  ENDMETHOD.

ENDCLASS.

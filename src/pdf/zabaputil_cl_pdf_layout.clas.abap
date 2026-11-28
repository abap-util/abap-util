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
    "! table. A cell wraps inside its column, a row is never cut in two,
    "! and the header row is repeated on every page the table reaches.
    "! A value is written as a string template writes it - format dates
    "! and amounts into text components first. A component that is no
    "! single value (a structure, a table, a reference) is an empty cell
    "! @parameter columns     | the columns, from left to right
    "! @parameter rows        | an internal table of structures
    "! @parameter header_font | the font of the header row; the current font without it
    METHODS table
      IMPORTING
        columns       TYPE ty_t_column
        rows          TYPE ANY TABLE
        header_font   TYPE REF TO zabaputil_if_pdf_font OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_layout.

    "! The same table from texts: one row per line of rows, one text per
    "! column - for cells that are formatted already, or come from
    "! somewhere else than the components of a structure
    "! @parameter columns     | the columns, from left to right; name is not used
    "! @parameter rows        | the rows, each with the texts of its cells
    "! @parameter header_font | the font of the header row; the current font without it
    METHODS table_text
      IMPORTING
        columns       TYPE ty_t_column
        rows          TYPE ty_t_text_row
        header_font   TYPE REF TO zabaputil_if_pdf_font OPTIONAL
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
    DATA mv_header       TYPE string.
    DATA mv_header_align TYPE string.
    DATA mv_header_size  TYPE f.
    DATA mv_footer       TYPE string.
    DATA mv_footer_align TYPE string.
    DATA mv_footer_size  TYPE f.
    DATA mv_finished     TYPE abap_bool.
    " the table that is being written: its columns with their final
    " widths, and the font of its header row - a page break inside the
    " table repeats the header
    DATA mt_column       TYPE ty_t_column.
    DATA mo_header_font  TYPE REF TO zabaputil_if_pdf_font.

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

    METHODS table_row
      IMPORTING
        cells     TYPE string_table
        font      TYPE REF TO zabaputil_if_pdf_font
        is_header TYPE abap_bool DEFAULT abap_false.

    METHODS margin_line
      IMPORTING
        page  TYPE REF TO zabaputil_cl_pdf_page
        val   TYPE string
        align TYPE string
        size  TYPE f
        y     TYPE f.

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

    DATA lt_row   TYPE ty_t_text_row.
    DATA lt_cell  TYPE string_table.
    DATA lv_index TYPE i.
    DATA lv_kind  TYPE string.

    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <value> TYPE any.

    LOOP AT rows ASSIGNING <row>.
      CLEAR lt_cell.
      LOOP AT columns ASSIGNING FIELD-SYMBOL(<column>).
        lv_index = sy-tabix.
        UNASSIGN <value>.
        IF <column>-name IS INITIAL.
          ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO <value>.
        ELSE.
          ASSIGN COMPONENT <column>-name OF STRUCTURE <row> TO <value>.
        ENDIF.
        " a component that is a structure, a table or a reference is no
        " text - and a string template of one is a runtime error
        IF <value> IS ASSIGNED.
          lv_kind = zabaputil_cl_util_context=>rtti_get_type_kind( <value> ).
          IF lv_kind = `u` OR lv_kind = `v` OR lv_kind = `h` OR lv_kind = `l` OR lv_kind = `r`.
            UNASSIGN <value>.
          ENDIF.
        ENDIF.
        IF <value> IS ASSIGNED.
          INSERT |{ <value> }| INTO TABLE lt_cell.
        ELSE.
          INSERT `` INTO TABLE lt_cell.
        ENDIF.
      ENDLOOP.
      INSERT lt_cell INTO TABLE lt_row.
    ENDLOOP.

    result = table_text( columns     = columns
                         rows        = lt_row
                         header_font = header_font ).

  ENDMETHOD.

  METHOD table_text.

    DATA lv_fixed TYPE f.
    DATA lv_open  TYPE i.
    DATA lv_share TYPE f.
    DATA lv_title TYPE abap_bool.

    result = me.
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

    CLEAR mo_header_font.
    IF lv_title = abap_true.
      mo_header_font = header_font.
      IF mo_header_font IS NOT BOUND.
        mo_header_font = mo_font.
      ENDIF.
      table_header( ).
    ENDIF.

    LOOP AT rows ASSIGNING FIELD-SYMBOL(<row>).
      table_row( cells = <row>
                 font  = mo_font ).
    ENDLOOP.

    CLEAR mt_column.
    CLEAR mo_header_font.

  ENDMETHOD.

  METHOD table_header.

    DATA lt_cell TYPE string_table.

    LOOP AT mt_column ASSIGNING FIELD-SYMBOL(<column>).
      INSERT <column>-title INTO TABLE lt_cell.
    ENDLOOP.
    table_row( cells     = lt_cell
               font      = mo_header_font
               is_header = abap_true ).

  ENDMETHOD.

  METHOD table_row.

    DATA lt_wrapped TYPE ty_t_cell.
    DATA ls_wrapped TYPE ty_s_cell.
    DATA lv_lines   TYPE i VALUE 1.
    DATA lv_x       TYPE f.
    DATA lv_text_x  TYPE f.
    DATA lv_text_y  TYPE f.
    DATA lv_index   TYPE i.
    DATA lv_align   TYPE string.
    DATA lv_cell    TYPE string.

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

    " a row stays in one piece: when it does not fit, the table goes on
    " at the top of the next page, under a header row of its own
    DATA(lv_row_height) = lv_lines * lv_height + lv_pad * 2.
    IF mv_y + lv_row_height > mo_page->height( ) - mv_bottom + lc_tolerance AND mv_y > mv_top + lc_tolerance.
      new_page( ).
      IF is_header = abap_false AND mo_header_font IS BOUND.
        table_header( ).
      ENDIF.
    ENDIF.

    mo_page->set_draw_color( c_line_color ).
    mo_page->set_line_width( c_line_width ).
    mo_page->set_font( font = font
                       size = mv_size ).
    mo_page->set_text_color( mv_color ).

    lv_x = mv_left.
    LOOP AT mt_column ASSIGNING <column>.
      lv_index = sy-tabix.

      IF is_header = abap_true.
        mo_page->set_fill_color( `E8E8E8` ).
        mo_page->rect( x     = lv_x
                       y     = mv_y
                       w     = <column>-width
                       h     = lv_row_height
                       style = `DF` ).
      ELSE.
        mo_page->rect( x = lv_x
                       y = mv_y
                       w = <column>-width
                       h = lv_row_height ).
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
      ASSIGN lt_wrapped[ lv_index ] TO FIELD-SYMBOL(<wrapped>).
      LOOP AT <wrapped>-t_line INTO DATA(lv_line).
        mo_page->text( x     = lv_text_x
                       y     = lv_text_y
                       val   = lv_line
                       align = lv_align ).
        lv_text_y = lv_text_y + lv_height.
      ENDLOOP.

      lv_x = lv_x + <column>-width.
    ENDLOOP.

    mv_y = mv_y + lv_row_height.

  ENDMETHOD.

  METHOD set_header.

    mv_header = val.
    mv_header_align = to_upper( align ).
    mv_header_size = size.
    result = me.

  ENDMETHOD.

  METHOD set_footer.

    mv_footer = val.
    mv_footer_align = to_upper( align ).
    mv_footer_size = size.
    result = me.

  ENDMETHOD.

  METHOD finish.

    DATA lv_number TYPE i.
    DATA lv_text   TYPE string.

    IF mv_finished = abap_true.
      RETURN.
    ENDIF.
    mv_finished = abap_true.

    IF mv_header IS INITIAL AND mv_footer IS INITIAL.
      RETURN.
    ENDIF.
    font_check( ).

    LOOP AT mt_page INTO DATA(lo_page).
      lv_number = sy-tabix.

      IF mv_header IS NOT INITIAL.
        lv_text = replace( val  = mv_header
                           sub  = `{pages}`
                           with = |{ lines( mt_page ) }|
                           occ  = 0 ).
        lv_text = replace( val  = lv_text
                           sub  = `{page}`
                           with = |{ lv_number }|
                           occ  = 0 ).
        margin_line( page  = lo_page
                     val   = lv_text
                     align = mv_header_align
                     size  = mv_header_size
                     y     = mv_top / 2 ).
      ENDIF.

      IF mv_footer IS NOT INITIAL.
        lv_text = replace( val  = mv_footer
                           sub  = `{pages}`
                           with = |{ lines( mt_page ) }|
                           occ  = 0 ).
        lv_text = replace( val  = lv_text
                           sub  = `{page}`
                           with = |{ lv_number }|
                           occ  = 0 ).
        margin_line( page  = lo_page
                     val   = lv_text
                     align = mv_footer_align
                     size  = mv_footer_size
                     y     = lo_page->height( ) - mv_bottom / 2 ).
      ENDIF.

    ENDLOOP.

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

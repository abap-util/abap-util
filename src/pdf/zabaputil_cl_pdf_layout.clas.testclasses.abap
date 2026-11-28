CLASS ltcl_layout DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_row,
        name TYPE string,
        qty  TYPE i,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    DATA mo_pdf    TYPE REF TO zabaputil_cl_pdf.
    DATA mo_layout TYPE REF TO zabaputil_cl_pdf_layout.
    DATA mo_font   TYPE REF TO zabaputil_if_pdf_font.

    METHODS setup.

    "! A new document and layout, as every test begins with
    METHODS fresh.

    METHODS wrap_at_width           FOR TESTING.
    METHODS wrap_exact_fit          FOR TESTING.
    METHODS wrap_long_word          FOR TESTING.
    METHODS wrap_blanks             FOR TESTING.
    METHODS default_margins         FOR TESTING.
    METHODS text_first_line         FOR TESTING.
    METHODS text_aligned            FOR TESTING.
    METHODS text_wraps              FOR TESTING.
    METHODS text_line_feeds         FOR TESTING.
    METHODS text_page_break         FOR TESTING.
    METHODS text_without_font       FOR TESTING.
    METHODS text_color              FOR TESTING.
    METHODS line_height_factor      FOR TESTING.
    METHODS space_and_cursor        FOR TESTING.
    METHODS page_break_explicit     FOR TESTING.
    METHODS rule_line               FOR TESTING.
    METHODS table_cells             FOR TESTING.
    METHODS table_cell_wraps        FOR TESTING.
    METHODS table_page_break        FOR TESTING.
    METHODS table_by_position       FOR TESTING.
    METHODS table_header_font       FOR TESTING.
    METHODS table_without_columns   FOR TESTING.
    METHODS table_from_texts        FOR TESTING.
    METHODS table_nested_component  FOR TESTING.
    METHODS table_total             FOR TESTING.
    METHODS table_subtotal          FOR TESTING.
    METHODS table_carry_over        FOR TESTING.
    METHODS table_row_higher_page   FOR TESTING.
    METHODS table_zebra             FOR TESTING.
    METHODS table_border_none       FOR TESTING.
    METHODS table_border_rows       FOR TESTING.
    METHODS area_lines_and_parts    FOR TESTING.
    METHODS area_first_page         FOR TESTING.
    METHODS area_image              FOR TESTING.
    METHODS markup_wraps            FOR TESTING.
    METHODS markup_plain_brackets   FOR TESTING.
    METHODS markup_fonts_and_lines  FOR TESTING.
    METHODS markup_font_styles      FOR TESTING.
    METHODS justified               FOR TESTING.
    METHODS justified_markup        FOR TESTING.
    METHODS paragraph_spacing       FOR TESTING.
    METHODS keep_lines_orphan       FOR TESTING.
    METHODS keep_lines_widow        FOR TESTING.
    METHODS keep_lines_together     FOR TESTING.

    "! Words of four characters, aa01 aa02 ...: 16 make a line
    METHODS words
      IMPORTING
        val           TYPE i
        from          TYPE i DEFAULT 1
      RETURNING
        VALUE(result) TYPE string.
    METHODS finish_page_numbers     FOR TESTING.
    METHODS finish_header           FOR TESTING.
    METHODS finish_once             FOR TESTING.
    METHODS finish_nothing_to_do    FOR TESTING.

    METHODS num
      IMPORTING
        val           TYPE f
      RETURNING
        VALUE(result) TYPE string.

    "! How often a text is shown on a page
    METHODS shown
      IMPORTING
        page          TYPE i
        text          TYPE string
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.

CLASS ltcl_layout IMPLEMENTATION.

  METHOD setup.
    fresh( ).
  ENDMETHOD.

  METHOD fresh.
    " A4 in points with a margin of 50: the lines are 495.28 wide and end
    " at 791.89. Courier at 10 points: 6 points per character, a line
    " every 12.5 points, 59 lines per page
    mo_pdf = zabaputil_cl_pdf=>create( ).
    mo_font = mo_pdf->font( `Courier` ).
    mo_layout = NEW #( mo_pdf ).
    mo_layout->set_margins( left   = 50
                            top    = 50
                            right  = 50
                            bottom = 50 ).
    mo_layout->set_font( mo_font ).
  ENDMETHOD.

  METHOD num.
    result = zabaputil_cl_pdf_writer=>num( val ).
  ENDMETHOD.

  METHOD shown.
    result = count( val = mo_pdf->page( page )->content( )
                    sub = |{ mo_font->encode( text ) } Tj| ).
  ENDMETHOD.

  METHOD wrap_at_width.
    " 60 points are ten characters
    DATA(lt_line) = mo_layout->wrap( val   = `aaaa bbbb cccc`
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_line ) ).
    cl_abap_unit_assert=>assert_equals( exp = `aaaa bbbb` act = lt_line[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `cccc` act = lt_line[ 2 ] ).
  ENDMETHOD.

  METHOD wrap_exact_fit.
    DATA(lt_line) = mo_layout->wrap( val   = `aaaa bbbbb`
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_line ) ).
    cl_abap_unit_assert=>assert_equals( exp = `aaaa bbbbb` act = lt_line[ 1 ] ).

    lt_line = mo_layout->wrap( val   = `aaaa bbbbbb`
                               width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_line ) ).
  ENDMETHOD.

  METHOD wrap_long_word.
    " a word wider than the line is cut; what follows goes on behind its rest
    DATA(lt_line) = mo_layout->wrap( val   = `ab xxxxxxxxxxxxxxxxxxxxxxxxx yy`
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( lt_line ) ).
    cl_abap_unit_assert=>assert_equals( exp = `ab` act = lt_line[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `xxxxxxxxxx` act = lt_line[ 2 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `xxxxxxxxxx` act = lt_line[ 3 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `xxxxx yy` act = lt_line[ 4 ] ).
  ENDMETHOD.

  METHOD wrap_blanks.
    DATA(lt_line) = mo_layout->wrap( val   = `  a   b  `
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_line ) ).
    cl_abap_unit_assert=>assert_equals( exp = `a b` act = lt_line[ 1 ] ).
    cl_abap_unit_assert=>assert_initial( mo_layout->wrap( val   = ``
                                                          width = 60 ) ).
    cl_abap_unit_assert=>assert_initial( mo_layout->wrap( val   = `   `
                                                          width = 60 ) ).
  ENDMETHOD.

  METHOD default_margins.
    " 20 mm on every side, in the unit of the document
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( unit = `mm` ).
    DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( lo_pdf ).
    lo_layout->page( ).
    cl_abap_unit_assert=>assert_equals( exp = `20` act = num( lo_layout->y( ) ) ).

    lo_pdf = zabaputil_cl_pdf=>create( ).
    lo_layout = NEW zabaputil_cl_pdf_layout( lo_pdf ).
    lo_layout->page( ).
    cl_abap_unit_assert=>assert_equals( exp = `56.693` act = num( lo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD text_first_line.
    " nothing is there until content arrives
    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_pdf->page_count( ) ).
    mo_layout->text( `Hi` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = mo_pdf->page_count( ) ).
    " the line is 12.5 high, the letters (6.29 above and 1.57 below the
    " baseline) are centred in it: the baseline is 8.61 below the top
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 10 Tf 50 783.28 Td <4869> Tj ET`
                                        act = mo_pdf->page( 1 )->content( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `62.5` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD text_aligned.
    " twelve points wide: it ends at 545.28, or is centred around 297.64
    mo_layout->text( val   = `Hi`
                     align = `R` ).
    mo_layout->text( val   = `Hi`
                     align = `c` ).
    cl_abap_unit_assert=>assert_equals(
        exp = |BT /F1 10 Tf 533.28 783.28 Td <4869> Tj ET\nBT /F1 10 Tf 291.64 770.78 Td <4869> Tj ET|
        act = mo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD text_wraps.
    " 82 characters fit into 495.28 points, the 83rd starts a new line
    DATA lv_text TYPE string.
    DO 20 TIMES.
      lv_text = lv_text && `abcd `.
    ENDDO.
    mo_layout->text( lv_text ).
    cl_abap_unit_assert=>assert_equals( exp = `75` act = num( mo_layout->y( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `abcd abcd abcd abcd` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = shown( page = 1
                     text = `abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd` ) ).
  ENDMETHOD.

  METHOD text_line_feeds.
    " four lines, the third one empty; a Windows line end is one line end
    mo_layout->text( |a\nb\n\nc| ).
    cl_abap_unit_assert=>assert_equals( exp = `100` act = num( mo_layout->y( ) ) ).
    mo_layout->text( |d\r\ne| ).
    cl_abap_unit_assert=>assert_equals( exp = `125` act = num( mo_layout->y( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `e` ) ).
  ENDMETHOD.

  METHOD text_page_break.
    DO 59 TIMES.
      mo_layout->text( `x` ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = mo_pdf->page_count( ) ).
    mo_layout->text( `y` ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `62.5` act = num( mo_layout->y( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 59
                                        act = shown( page = 1
                                                     text = `x` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 10 Tf 50 783.28 Td <79> Tj ET`
                                        act = mo_pdf->page( 2 )->content( ) ).
  ENDMETHOD.

  METHOD text_without_font.
    DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( mo_pdf ).
    TRY.
        lo_layout->text( `Hi` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        lo_layout->wrap( val   = `Hi`
                         width = 10 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_pdf->page_count( ) ).
  ENDMETHOD.

  METHOD text_color.
    mo_layout->set_color( `0000FF` ).
    mo_layout->text( `Hi` ).
    cl_abap_unit_assert=>assert_equals( exp = `q 0 0 1 rg BT /F1 10 Tf 50 783.28 Td <4869> Tj ET Q`
                                        act = mo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD line_height_factor.
    mo_layout->set_line_height( 2 ).
    mo_layout->text( |a\nb| ).
    cl_abap_unit_assert=>assert_equals( exp = `90` act = num( mo_layout->y( ) ) ).

    " and the font size counts
    mo_layout->set_font( font = mo_font
                         size = 20 ).
    mo_layout->text( `c` ).
    cl_abap_unit_assert=>assert_equals( exp = `130` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD space_and_cursor.
    mo_layout->add_space( 20 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `70` act = num( mo_layout->y( ) ) ).
    " without a height it is one line
    mo_layout->add_space( ).
    cl_abap_unit_assert=>assert_equals( exp = `82.5` act = num( mo_layout->y( ) ) ).
    mo_layout->set_y( 300 ).
    cl_abap_unit_assert=>assert_equals( exp = `300` act = num( mo_layout->y( ) ) ).
    mo_layout->text( `Hi` ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 10 Tf 50 533.28 Td <4869> Tj ET`
                                        act = mo_layout->page( )->content( ) ).
  ENDMETHOD.

  METHOD page_break_explicit.
    mo_layout->text( `a` ).
    mo_layout->page_break( ).
    mo_layout->text( `b` ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 2
                                                     text = `b` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( mo_layout->page( ) = mo_pdf->page( 2 ) ) ).

    " as the first thing it leaves an empty first page
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( lo_pdf ).
    lo_layout->page_break( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_pdf->page_count( ) ).
  ENDMETHOD.

  METHOD rule_line.
    " half a line of space, the line from margin to margin, half a line
    mo_layout->rule( ).
    cl_abap_unit_assert=>assert_equals( exp = `62.5` act = num( mo_layout->y( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = |0.6 0.6 0.6 RG\n0.5 w\n50 785.64 m 545.28 785.64 l S|
                                        act = mo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD table_cells.
    DATA(lt_row) = VALUE ty_t_row( ( name = `Nut`  qty = 7 )
                                   ( name = `Bolt` qty = 12 ) ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` )
                                         ( name = `QTY`  title = `Qty`  width = 0   align = `R` ) )
                      rows    = lt_row ).

    " a row is one line and three points above and below it: 18.5. The
    " second column takes the rest of the 495.28 points
    cl_abap_unit_assert=>assert_equals( exp = `105.5` act = num( mo_layout->y( ) ) ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).

    " the header row is filled, the rows below are outlined
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |0.91 0.91 0.91 rg\n50 773.39 100 18.5 re B| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `150 773.39 395.28 18.5 re B` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `50 754.89 100 18.5 re S` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `150 736.39 395.28 18.5 re S` ) ).

    " left: three points from the line. Right: it ends three points before
    " the line at 545.28 - `7` is 6 wide, `12` is 12 wide
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 53 780.28 Td <4E616D65> Tj ET Q` ) ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 53 761.78 Td <4E7574> Tj ET Q` ) ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 536.28 761.78 Td <37> Tj ET Q` ) ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 530.28 743.28 Td <3132> Tj ET Q` ) ).
  ENDMETHOD.

  METHOD table_cell_wraps.
    " the cell is 94 points wide inside: 15 characters, one word per line
    DATA(lt_row) = VALUE ty_t_row( ( name = `aaaaaaaaaa bbbbbbbbbb cccccccccc` qty = 1 ) ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `` width = 100 align = `L` )
                                         ( name = `QTY`  title = `` width = 50  align = `C` ) )
                      rows    = lt_row ).
    " no titles, no header row: one row of three lines
    cl_abap_unit_assert=>assert_equals( exp = `93.5` act = num( mo_layout->y( ) ) ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `50 748.39 100 43.5 re S` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `150 748.39 50 43.5 re S` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `bbbbbbbbbb` ) ).
    " centred in its column, in the first line of the row
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 10 Tf 172 780.28 Td <31> Tj ET` ) ).
  ENDMETHOD.

  METHOD table_page_break.
    " 40 rows of 18.5 fit on a page: the header and 39 rows, then the
    " header again and the other 11
    DATA lt_row TYPE ty_t_row.
    DO 50 TIMES.
      INSERT VALUE #( name = |Row { sy-index }|
                      qty  = sy-index ) INTO TABLE lt_row.
    ENDDO.
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` )
                                         ( name = `QTY`  title = `Qty`  width = 100 align = `R` ) )
                      rows    = lt_row ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `Name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 2
                                                     text = `Name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `Row 39` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = shown( page = 1
                                                     text = `Row 40` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 2
                                                     text = `Row 40` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `272` act = num( mo_layout->y( ) ) ).

    " what follows the table is not a row of it
    mo_layout->page_break( ).
    mo_layout->text( `after` ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = shown( page = 3
                                                     text = `Name` ) ).
  ENDMETHOD.

  METHOD table_by_position.
    " without a name the column takes the component at its position; a
    " name the row does not have is an empty cell
    DATA(lt_row) = VALUE ty_t_row( ( name = `Nut` qty = 7 ) ).
    mo_layout->table( columns = VALUE #( ( name = ``     title = `` width = 100 align = `` )
                                         ( name = ``     title = `` width = 100 align = `` )
                                         ( name = `NONE` title = `` width = 100 align = `` ) )
                      rows    = lt_row ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `Nut` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `7` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = count( val = mo_pdf->page( 1 )->content( )
                                                     sub = ` Tj` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = count( val = mo_pdf->page( 1 )->content( )
                                                     sub = ` re S` ) ).
  ENDMETHOD.

  METHOD table_header_font.
    DATA(lo_bold) = mo_pdf->font( `Courier-Bold` ).
    DATA(lt_row) = VALUE ty_t_row( ( name = `Nut` qty = 7 ) ).
    mo_layout->table( columns     = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` ) )
                      rows        = lt_row
                      header_font = lo_bold ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    " the bold cut reaches 6.26 above and 1.42 below the baseline
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F2 10 Tf 53 780.22 Td <4E616D65> Tj ET` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 10 Tf 53 761.78 Td <4E7574> Tj ET` ) ).
  ENDMETHOD.

  METHOD table_without_columns.
    DATA lt_row TYPE ty_t_row.
    DATA lt_column TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    mo_layout->table( columns = lt_column
                      rows    = lt_row ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_pdf->page_count( ) ).

    " columns and no rows: the header row alone
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` ) )
                      rows    = lt_row ).
    cl_abap_unit_assert=>assert_equals( exp = `68.5` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD table_nested_component.
    " a component that is a table is an empty cell, not a dump
    TYPES:
      BEGIN OF ty_s_deep,
        name  TYPE string,
        t_sub TYPE string_table,
      END OF ty_s_deep.
    DATA lt_deep TYPE STANDARD TABLE OF ty_s_deep WITH EMPTY KEY.
    INSERT VALUE #( name  = `Nut`
                    t_sub = VALUE #( ( `x` ) ) ) INTO TABLE lt_deep.
    mo_layout->table( columns = VALUE #( ( name = `NAME`  title = `` width = 100 align = `L` )
                                         ( name = `T_SUB` title = `` width = 100 align = `L` ) )
                      rows    = lt_deep ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = count( val = mo_pdf->page( 1 )->content( )
                                                     sub = ` Tj` ) ).
  ENDMETHOD.

  METHOD table_from_texts.
    " cells that are texts already; a row with fewer cells than columns
    " has empty ones at its end
    DATA lt_row TYPE zabaputil_cl_pdf_layout=>ty_t_text_row.
    INSERT VALUE #( ( `Nut` ) ( `7` ) ) INTO TABLE lt_row.
    INSERT VALUE #( ( `Bolt` ) ) INTO TABLE lt_row.
    mo_layout->table_text( columns = VALUE #( ( name = `` title = `Name` width = 100 align = `L` )
                                              ( name = `` title = `Qty`  width = 100 align = `R` ) )
                           rows    = lt_row ).
    cl_abap_unit_assert=>assert_equals( exp = `105.5` act = num( mo_layout->y( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `Bolt` ) ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 241 761.78 Td <37> Tj ET Q` ) ).
    " header 2, first row 2, second row 1
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = count( val = lv_content
                                                     sub = ` Tj` ) ).
  ENDMETHOD.

  METHOD finish_page_numbers.
    mo_layout->set_footer( `{page}/{pages}` ).
    mo_layout->text( `a` ).
    mo_layout->page_break( ).
    mo_layout->text( `b` ).
    mo_layout->finish( ).

    " in the middle of the bottom margin, 8 points, centred: three
    " characters are 14.4 wide
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 8 Tf 290.44 25 Td <312F32> Tj ET` ) ).
    lv_content = mo_pdf->page( 2 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 8 Tf 290.44 25 Td <322F32> Tj ET` ) ).
  ENDMETHOD.

  METHOD finish_header.
    mo_layout->set_header( val   = `Page {page}`
                           align = `r`
                           size  = 10 ).
    mo_layout->text( `a` ).
    mo_layout->finish( ).
    " in the middle of the top margin, ending at the right margin: six
    " characters are 36 wide
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 10 Tf 509.28 816.89 Td <506167652031> Tj ET` ) ).
  ENDMETHOD.

  METHOD finish_once.
    mo_layout->set_footer( `x` ).
    mo_layout->text( `a` ).
    mo_layout->finish( ).
    mo_layout->finish( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `x` ) ).
  ENDMETHOD.

  METHOD finish_nothing_to_do.
    " no header, no footer, not even a font: nothing happens
    DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( mo_pdf ).
    lo_layout->finish( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_pdf->page_count( ) ).

    " a footer needs a font
    lo_layout = NEW zabaputil_cl_pdf_layout( mo_pdf ).
    lo_layout->set_footer( `x` ).
    lo_layout->page( ).
    TRY.
        lo_layout->finish( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD table_total.
    DATA(lt_row) = VALUE ty_t_row( ( name = `Nut`  qty = 7 )
                                   ( name = `Bolt` qty = 12 ) ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` )
                                         ( name = `QTY`  title = `Qty`  width = 100 align = `R` ) )
                      rows    = lt_row
                      options = VALUE #( t_sum = VALUE #( ( `qty` ) ) ) ).
    " the header, two rows and the total: four rows of 18.5
    cl_abap_unit_assert=>assert_equals( exp = `124` act = num( mo_layout->y( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `Total` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `19` ) ).

    " a text of its own
    fresh( ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` )
                                         ( name = `QTY`  title = `Qty`  width = 100 align = `R` ) )
                      rows    = lt_row
                      options = VALUE #( t_sum = VALUE #( ( `QTY` ) ) total_text = `Summe` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `Summe` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( page = 1 text = `Total` ) ).

    " nothing to add up, no total
    fresh( ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` ) )
                      rows    = lt_row
                      options = VALUE #( t_sum = VALUE #( ( `QTY` ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( page = 1 text = `Total` ) ).
  ENDMETHOD.

  METHOD table_subtotal.
    DATA(lt_row) = VALUE ty_t_row( ( name = `Nut`  qty = 1 )
                                   ( name = `Nut`  qty = 2 )
                                   ( name = `Bolt` qty = 5 ) ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` )
                                         ( name = `QTY`  title = `Qty`  width = 100 align = `R` ) )
                      rows    = lt_row
                      options = VALUE #( t_sum = VALUE #( ( `QTY` ) ) group = `name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = shown( page = 1 text = `Subtotal` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `3` ) ).
    " the subtotal of Bolt and its row
    cl_abap_unit_assert=>assert_equals( exp = 2 act = shown( page = 1 text = `5` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `8` ) ).
    " the header, three rows, two subtotals and the total
    cl_abap_unit_assert=>assert_equals( exp = `179.5` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD table_carry_over.
    " the rows of a page leave room for the carry-over: 38 rows below the
    " header, then the sum of 1 to 38 carried to the next page
    DATA lt_row TYPE ty_t_row.
    DO 50 TIMES.
      INSERT VALUE #( name = |Row { sy-index }|
                      qty  = sy-index ) INTO TABLE lt_row.
    ENDDO.
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` )
                                         ( name = `QTY`  title = `Qty`  width = 100 align = `R` ) )
                      rows    = lt_row
                      options = VALUE #( t_sum = VALUE #( ( `QTY` ) ) carry = abap_true ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `Row 38` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `Carried forward` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `741` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = `Name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = `Brought forward` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = `741` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = `Row 39` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = `1275` ) ).
    " the carry-over is below the last row, above the bottom margin
    cl_abap_unit_assert=>assert_true(
        xsdbool( mo_pdf->page( 1 )->content( ) CS `50 51.89 100 18.5 re S` ) ).
  ENDMETHOD.

  METHOD table_row_higher_page.
    " 80 words of ten characters, one per line: 1000 points, more than a
    " page. 58 lines fit on the first page, 22 go on on the second
    DATA lv_name TYPE string.
    DO 80 TIMES.
      lv_name = |{ lv_name } aaaaaaaaaa|.
    ENDDO.
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `` width = 100 align = `L` ) )
                      rows    = VALUE ty_t_row( ( name = lv_name ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 58 act = shown( page = 1 text = `aaaaaaaaaa` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 22 act = shown( page = 2 text = `aaaaaaaaaa` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `331` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD table_zebra.
    DATA(lt_row) = VALUE ty_t_row( ( name = `A` ) ( name = `B` ) ( name = `C` ) ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` ) )
                      rows    = lt_row
                      options = VALUE #( zebra = `CCCCCC` ) ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    " the second row is filled, the first and the third are not
    cl_abap_unit_assert=>assert_equals( exp = 1 act = count( val = lv_content sub = `0.8 0.8 0.8 rg` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |0.8 0.8 0.8 rg\n50 736.39 100 18.5 re B| ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = count( val = lv_content sub = `re S` ) ).
  ENDMETHOD.

  METHOD table_border_none.
    DATA(lt_row) = VALUE ty_t_row( ( name = `A` ) ( name = `B` ) ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `Name` width = 100 align = `L` ) )
                      rows    = lt_row
                      options = VALUE #( border = `none` zebra = `CCCCCC` header_fill = `FFFFFF` ) ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    " filled without an outline, no lines at all
    cl_abap_unit_assert=>assert_equals( exp = 0 act = count( val = lv_content sub = `re S` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = count( val = lv_content sub = `re B` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = count( val = lv_content sub = `re f` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |1 1 1 rg\n50 773.39 100 18.5 re f| ) ).
  ENDMETHOD.

  METHOD table_border_rows.
    DATA(lt_row) = VALUE ty_t_row( ( name = `A` ) ( name = `B` ) ).
    mo_layout->table( columns = VALUE #( ( name = `NAME` title = `` width = 100 align = `L` ) )
                      rows    = lt_row
                      options = VALUE #( border = zabaputil_cl_pdf_layout=>cs_border-rows border_color = `FF0000` ) ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    " a red line below each row, no cells
    cl_abap_unit_assert=>assert_equals( exp = 0 act = count( val = lv_content sub = `re S` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `1 0 0 RG` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `50 773.39 m 150 773.39 l S` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `50 754.89 m 150 754.89 l S` ) ).
  ENDMETHOD.

  METHOD area_lines_and_parts.
    mo_layout->set_header_area( VALUE #( left  = |A\nB|
                                         right = `C{page}` ) ).
    mo_layout->text( `a` ).
    mo_layout->finish( ).
    " 8 points, a line every 10: two lines around the middle of the top
    " margin, at 20 and 30 from the top; one line in the middle at 25
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 8 Tf 50 821.89 Td <41> Tj ET` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 8 Tf 50 811.89 Td <42> Tj ET` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 8 Tf 535.68 816.89 Td <4331> Tj ET` ) ).
  ENDMETHOD.

  METHOD area_first_page.
    mo_layout->set_footer_area( VALUE #( center = `other` ) ).
    mo_layout->set_footer_area( val        = VALUE #( center = `first` )
                                first_page = abap_true ).
    mo_layout->set_header( `header` ).
    " the first page without a header
    mo_layout->set_header_area( val        = VALUE #( )
                                first_page = abap_true ).
    mo_layout->text( `a` ).
    mo_layout->page_break( ).
    mo_layout->text( `b` ).
    mo_layout->finish( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = `first` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( page = 1 text = `other` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( page = 1 text = `header` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( page = 2 text = `first` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = `other` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = `header` ) ).
  ENDMETHOD.

  METHOD area_image.
    " a PNG of 2 x 2 pixels, 20 high: 20 wide, at the right margin, centred
    " in the top margin - from 15 to 35 from the top
    DATA lv_hex TYPE string.
    lv_hex = `89504E470D0A1A0A0000000D4948445200000002000000020802000000FDD49A73000000054944415478DA63F8CF51E86AD7` &&
             `0000000D49444154C0C000C20CFF8100001FEE05FB61FE8DCC0000000049454E44AE426082`.
    DATA lv_png TYPE xstring.
    lv_png = lv_hex.
    mo_layout->set_header_area( VALUE #( image       = mo_pdf->image( lv_png )
                                         image_align = `R`
                                         image_h     = 20 ) ).
    mo_layout->text( `a` ).
    mo_layout->finish( ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( mo_pdf->page( 1 )->content( ) CS `q 20 0 0 20 525.28 806.89 cm /I1 Do Q` ) ).
  ENDMETHOD.

  METHOD markup_wraps.
    " the blanks between the words keep their style; a line is markup of
    " its own
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table( ( `a <b>bold</b> c` ) )
        act = mo_layout->wrap_markup( val   = `a <b>bold</b> c`
                                      width = 500 ) ).
    " ten characters a line - Courier-Bold is as wide as Courier
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table( ( `<b>aaaa</b> bbbb` ) ( `<i>cc</i>cc` ) )
        act = mo_layout->wrap_markup( val   = `<b>aaaa</b> bbbb <i>cc</i>cc`
                                      width = 60 ) ).
    " nested, and a colour inside of it
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table( ( `<b>x</b><b><i>y</i></b><color=FF0000><b><u>z</u></b></color>` ) )
        act = mo_layout->wrap_markup( val   = `<B>x<i>y</i><color=ff0000><u>z</u></color></b>`
                                      width = 500 ) ).
  ENDMETHOD.

  METHOD markup_plain_brackets.
    " a < that opens no markup is a <, a tag that is never closed lasts to
    " the end
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table( ( `a < b <x> c<d` ) )
        act = mo_layout->wrap_markup( val   = `a < b <x> c<d`
                                      width = 500 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table( ( `<u>open</u>` ) )
        act = mo_layout->wrap_markup( val   = `<u>open`
                                      width = 500 ) ).
  ENDMETHOD.

  METHOD markup_fonts_and_lines.
    mo_layout->rich_text( `x <b>y</b> <u>u</u> <color=FF0000>r</color>` ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    " the bold cut of Courier is the second font of the document
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |BT /F1 10 Tf 50 783.28 Td { mo_font->encode( `x ` ) } Tj ET| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |BT /F2 10 Tf 62 783.28 Td { mo_font->encode( `y` ) } Tj ET| ) ).
    " underlined: a line a point below the baseline, from 74 to 80
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `74 782.28 m 80 782.28 l S` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |q 1 0 0 rg BT /F1 10 Tf 86 783.28 Td { mo_font->encode( `r` ) } Tj ET Q| ) ).
    cl_abap_unit_assert=>assert_equals( exp = `62.5` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD markup_font_styles.
    " a cut of its own: here Times-Roman for the bold text
    mo_layout->set_font_styles( bold = mo_pdf->font( `Times-Roman` ) ).
    mo_layout->rich_text( `<b>t</b>` ).
    cl_abap_unit_assert=>assert_true( xsdbool( mo_pdf->page( 1 )->content( ) CS `/F2 10 Tf` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Times-Roman`
                                        act = zabaputil_cl_pdf_font_std=>styled( name   = `Times-BoldItalic`
                                                                                 bold   = abap_false
                                                                                 italic = abap_false ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Helvetica-BoldOblique`
                                        act = zabaputil_cl_pdf_font_std=>styled( name   = `Helvetica`
                                                                                 bold   = abap_true
                                                                                 italic = abap_true ) ).
  ENDMETHOD.

  METHOD words.
    DATA lv_number TYPE n LENGTH 2.
    DO val TIMES.
      lv_number = from + sy-index - 1.
      result = |{ result } aa{ lv_number }|.
    ENDDO.
    result = condense( result ).
  ENDMETHOD.

  METHOD justified.
    " 16 words of 24 points on the first line leave 111.28 points for 15
    " gaps: the last word ends at the right margin. The last line of the
    " paragraph is not stretched
    mo_layout->text( val   = words( 20 )
                     align = `J` ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |50 783.28 Td { mo_font->encode( `aa01` ) } Tj| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |521.28 783.28 Td { mo_font->encode( `aa16` ) } Tj| ) ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_content CS |50 770.78 Td { mo_font->encode( `aa17 aa18 aa19 aa20` ) } Tj| ) ).
  ENDMETHOD.

  METHOD justified_markup.
    mo_layout->rich_text( val   = |<b>{ words( 20 ) }</b>|
                          align = `j` ).
    DATA(lv_content) = mo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |/F2 10 Tf 521.28 783.28 Td { mo_font->encode( `aa16` ) } Tj| ) ).
  ENDMETHOD.

  METHOD paragraph_spacing.
    " nothing before the first paragraph at the top of the page: 12.5 + 5,
    " then 10 + 12.5 + 5
    mo_layout->set_paragraph_spacing( before = 10
                                      after  = 5 ).
    mo_layout->text( |a\nb| ).
    cl_abap_unit_assert=>assert_equals( exp = `95` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD keep_lines_orphan.
    " one line is left on the page; a paragraph of three lines does not
    " leave one of them there
    mo_layout->set_keep_lines( min = 2 ).
    mo_layout->set_y( `776.89` ).
    mo_layout->text( words( 48 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = count( val = mo_pdf->page( 1 )->content( ) sub = `Tj` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `87.5` act = num( mo_layout->y( ) ) ).
  ENDMETHOD.

  METHOD keep_lines_widow.
    " three lines are left; the fourth would be alone on the next page, so
    " the third goes along. Two lines left would leave the first one alone:
    " then the whole paragraph goes
    mo_layout->set_keep_lines( min = 2 ).
    mo_layout->set_y( `751.89` ).
    mo_layout->text( words( 64 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = words( 16 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = words( val = 16 from = 17 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = words( val = 16 from = 33 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = words( val = 16 from = 49 ) ) ).

    " without the rule, three lines here and one there
    fresh( ).
    mo_layout->set_y( `751.89` ).
    mo_layout->text( words( 64 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 1 text = words( val = 16 from = 33 ) ) ).
  ENDMETHOD.

  METHOD keep_lines_together.
    mo_layout->set_keep_lines( min      = 1
                               together = abap_true ).
    mo_layout->set_y( `764.39` ).
    mo_layout->text( words( 48 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = count( val = mo_pdf->page( 1 )->content( ) sub = `Tj` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( page = 2 text = words( 16 ) ) ).
  ENDMETHOD.

ENDCLASS.

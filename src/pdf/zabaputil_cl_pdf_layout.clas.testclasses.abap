CLASS ltcl_layout DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_row,
        name TYPE string,
        qty  TYPE i,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

    DATA mo_pdf    TYPE REF TO zabaputil_cl_pdf.
    DATA mo_layout TYPE REF TO zabaputil_cl_pdf_layout.
    DATA mo_font   TYPE REF TO zabaputil_if_pdf_font.

    METHODS setup.

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
    " A4 in points with a margin of 50: the lines are 495.28 wide and end
    " at 791.89. Courier at 10 points: 6 points per character, a line
    " every 12.5 points, 59 lines per page
    mo_pdf = zabaputil_cl_pdf=>create( ).
    mo_font = mo_pdf->font( `Courier` ).
    CREATE OBJECT mo_layout EXPORTING PDF = mo_pdf.
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
    DATA lt_line TYPE string_table.
    FIELD-SYMBOLS <temp1> LIKE LINE OF lt_line.
    DATA temp2 LIKE sy-tabix.
    FIELD-SYMBOLS <temp3> LIKE LINE OF lt_line.
    DATA temp4 LIKE sy-tabix.
    lt_line = mo_layout->wrap( val   = `aaaa bbbb cccc`
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_line ) ).
    
    
    temp2 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp1>.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `aaaa bbbb` act = <temp1> ).
    
    
    temp4 = sy-tabix.
    READ TABLE lt_line INDEX 2 ASSIGNING <temp3>.
    sy-tabix = temp4.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `cccc` act = <temp3> ).
  ENDMETHOD.

  METHOD wrap_exact_fit.
    DATA lt_line TYPE string_table.
    FIELD-SYMBOLS <temp5> LIKE LINE OF lt_line.
    DATA temp6 LIKE sy-tabix.
    lt_line = mo_layout->wrap( val   = `aaaa bbbbb`
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_line ) ).
    
    
    temp6 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp5>.
    sy-tabix = temp6.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `aaaa bbbbb` act = <temp5> ).

    lt_line = mo_layout->wrap( val   = `aaaa bbbbbb`
                               width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_line ) ).
  ENDMETHOD.

  METHOD wrap_long_word.
    " a word wider than the line is cut; what follows goes on behind its rest
    DATA lt_line TYPE string_table.
    FIELD-SYMBOLS <temp7> LIKE LINE OF lt_line.
    DATA temp8 LIKE sy-tabix.
    FIELD-SYMBOLS <temp9> LIKE LINE OF lt_line.
    DATA temp10 LIKE sy-tabix.
    FIELD-SYMBOLS <temp11> LIKE LINE OF lt_line.
    DATA temp12 LIKE sy-tabix.
    FIELD-SYMBOLS <temp13> LIKE LINE OF lt_line.
    DATA temp14 LIKE sy-tabix.
    lt_line = mo_layout->wrap( val   = `ab xxxxxxxxxxxxxxxxxxxxxxxxx yy`
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( lt_line ) ).
    
    
    temp8 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp7>.
    sy-tabix = temp8.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ab` act = <temp7> ).
    
    
    temp10 = sy-tabix.
    READ TABLE lt_line INDEX 2 ASSIGNING <temp9>.
    sy-tabix = temp10.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `xxxxxxxxxx` act = <temp9> ).
    
    
    temp12 = sy-tabix.
    READ TABLE lt_line INDEX 3 ASSIGNING <temp11>.
    sy-tabix = temp12.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `xxxxxxxxxx` act = <temp11> ).
    
    
    temp14 = sy-tabix.
    READ TABLE lt_line INDEX 4 ASSIGNING <temp13>.
    sy-tabix = temp14.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `xxxxx yy` act = <temp13> ).
  ENDMETHOD.

  METHOD wrap_blanks.
    DATA lt_line TYPE string_table.
    FIELD-SYMBOLS <temp15> LIKE LINE OF lt_line.
    DATA temp16 LIKE sy-tabix.
    lt_line = mo_layout->wrap( val   = `  a   b  `
                                     width = 60 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_line ) ).
    
    
    temp16 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp15>.
    sy-tabix = temp16.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a b` act = <temp15> ).
    cl_abap_unit_assert=>assert_initial( mo_layout->wrap( val   = ``
                                                          width = 60 ) ).
    cl_abap_unit_assert=>assert_initial( mo_layout->wrap( val   = `   `
                                                          width = 60 ) ).
  ENDMETHOD.

  METHOD default_margins.
    " 20 mm on every side, in the unit of the document
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA lo_layout TYPE REF TO zabaputil_cl_pdf_layout.
    lo_pdf = zabaputil_cl_pdf=>create( unit = `mm` ).
    
    CREATE OBJECT lo_layout TYPE zabaputil_cl_pdf_layout EXPORTING PDF = lo_pdf.
    lo_layout->page( ).
    cl_abap_unit_assert=>assert_equals( exp = `20` act = num( lo_layout->y( ) ) ).

    lo_pdf = zabaputil_cl_pdf=>create( ).
    CREATE OBJECT lo_layout TYPE zabaputil_cl_pdf_layout EXPORTING PDF = lo_pdf.
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
    DATA lo_layout TYPE REF TO zabaputil_cl_pdf_layout.
    CREATE OBJECT lo_layout TYPE zabaputil_cl_pdf_layout EXPORTING PDF = mo_pdf.
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
    DATA temp1 TYPE xsdboolean.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA lo_layout TYPE REF TO zabaputil_cl_pdf_layout.
    mo_layout->text( `a` ).
    mo_layout->page_break( ).
    mo_layout->text( `b` ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = mo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 2
                                                     text = `b` ) ).
    
    temp1 = boolc( mo_layout->page( ) = mo_pdf->page( 2 ) ).
    cl_abap_unit_assert=>assert_true( temp1 ).

    " as the first thing it leaves an empty first page
    
    lo_pdf = zabaputil_cl_pdf=>create( ).
    
    CREATE OBJECT lo_layout TYPE zabaputil_cl_pdf_layout EXPORTING PDF = lo_pdf.
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
    DATA temp17 TYPE ty_t_row.
    DATA temp18 LIKE LINE OF temp17.
    DATA lt_row LIKE temp17.
    DATA temp19 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp20 LIKE LINE OF temp19.
    DATA lv_content TYPE string.
    DATA temp2 TYPE xsdboolean.
    DATA temp3 TYPE xsdboolean.
    DATA temp4 TYPE xsdboolean.
    DATA temp5 TYPE xsdboolean.
    DATA temp6 TYPE xsdboolean.
    DATA temp7 TYPE xsdboolean.
    DATA temp8 TYPE xsdboolean.
    DATA temp9 TYPE xsdboolean.
    CLEAR temp17.
    
    temp18-name = `Nut`.
    temp18-qty = 7.
    INSERT temp18 INTO TABLE temp17.
    temp18-name = `Bolt`.
    temp18-qty = 12.
    INSERT temp18 INTO TABLE temp17.
    
    lt_row = temp17.
    
    CLEAR temp19.
    
    temp20-name = `NAME`.
    temp20-title = `Name`.
    temp20-width = 100.
    temp20-align = `L`.
    INSERT temp20 INTO TABLE temp19.
    temp20-name = `QTY`.
    temp20-title = `Qty`.
    temp20-width = 0.
    temp20-align = `R`.
    INSERT temp20 INTO TABLE temp19.
    mo_layout->table( columns = temp19
                      rows    = lt_row ).

    " a row is one line and three points above and below it: 18.5. The
    " second column takes the rest of the 495.28 points
    cl_abap_unit_assert=>assert_equals( exp = `105.5` act = num( mo_layout->y( ) ) ).
    
    lv_content = mo_pdf->page( 1 )->content( ).

    " the header row is filled, the rows below are outlined
    
    temp2 = boolc( lv_content CS |0.91 0.91 0.91 rg\n50 773.39 100 18.5 re B| ).
    cl_abap_unit_assert=>assert_true( temp2 ).
    
    temp3 = boolc( lv_content CS `150 773.39 395.28 18.5 re B` ).
    cl_abap_unit_assert=>assert_true( temp3 ).
    
    temp4 = boolc( lv_content CS `50 754.89 100 18.5 re S` ).
    cl_abap_unit_assert=>assert_true( temp4 ).
    
    temp5 = boolc( lv_content CS `150 736.39 395.28 18.5 re S` ).
    cl_abap_unit_assert=>assert_true( temp5 ).

    " left: three points from the line. Right: it ends three points before
    " the line at 545.28 - `7` is 6 wide, `12` is 12 wide
    
    temp6 = boolc( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 53 780.28 Td <4E616D65> Tj ET Q` ).
    cl_abap_unit_assert=>assert_true(
        temp6 ).
    
    temp7 = boolc( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 53 761.78 Td <4E7574> Tj ET Q` ).
    cl_abap_unit_assert=>assert_true(
        temp7 ).
    
    temp8 = boolc( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 536.28 761.78 Td <37> Tj ET Q` ).
    cl_abap_unit_assert=>assert_true(
        temp8 ).
    
    temp9 = boolc( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 530.28 743.28 Td <3132> Tj ET Q` ).
    cl_abap_unit_assert=>assert_true(
        temp9 ).
  ENDMETHOD.

  METHOD table_cell_wraps.
    " the cell is 94 points wide inside: 15 characters, one word per line
    DATA temp21 TYPE ty_t_row.
    DATA temp22 LIKE LINE OF temp21.
    DATA lt_row LIKE temp21.
    DATA temp23 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp24 LIKE LINE OF temp23.
    DATA lv_content TYPE string.
    DATA temp10 TYPE xsdboolean.
    DATA temp11 TYPE xsdboolean.
    DATA temp12 TYPE xsdboolean.
    CLEAR temp21.
    
    temp22-name = `aaaaaaaaaa bbbbbbbbbb cccccccccc`.
    temp22-qty = 1.
    INSERT temp22 INTO TABLE temp21.
    
    lt_row = temp21.
    
    CLEAR temp23.
    
    temp24-name = `NAME`.
    temp24-title = ``.
    temp24-width = 100.
    temp24-align = `L`.
    INSERT temp24 INTO TABLE temp23.
    temp24-name = `QTY`.
    temp24-title = ``.
    temp24-width = 50.
    temp24-align = `C`.
    INSERT temp24 INTO TABLE temp23.
    mo_layout->table( columns = temp23
                      rows    = lt_row ).
    " no titles, no header row: one row of three lines
    cl_abap_unit_assert=>assert_equals( exp = `93.5` act = num( mo_layout->y( ) ) ).
    
    lv_content = mo_pdf->page( 1 )->content( ).
    
    temp10 = boolc( lv_content CS `50 748.39 100 43.5 re S` ).
    cl_abap_unit_assert=>assert_true( temp10 ).
    
    temp11 = boolc( lv_content CS `150 748.39 50 43.5 re S` ).
    cl_abap_unit_assert=>assert_true( temp11 ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `bbbbbbbbbb` ) ).
    " centred in its column, in the first line of the row
    
    temp12 = boolc( lv_content CS `BT /F1 10 Tf 172 780.28 Td <31> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp12 ).
  ENDMETHOD.

  METHOD table_page_break.
    " 40 rows of 18.5 fit on a page: the header and 39 rows, then the
    " header again and the other 11
    DATA lt_row TYPE ty_t_row.
      DATA temp25 TYPE ltcl_layout=>ty_s_row.
    DATA temp26 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp27 LIKE LINE OF temp26.
    DO 50 TIMES.
      
      CLEAR temp25.
      temp25-name = |Row { sy-index }|.
      temp25-qty = sy-index.
      INSERT temp25 INTO TABLE lt_row.
    ENDDO.
    
    CLEAR temp26.
    
    temp27-name = `NAME`.
    temp27-title = `Name`.
    temp27-width = 100.
    temp27-align = `L`.
    INSERT temp27 INTO TABLE temp26.
    temp27-name = `QTY`.
    temp27-title = `Qty`.
    temp27-width = 100.
    temp27-align = `R`.
    INSERT temp27 INTO TABLE temp26.
    mo_layout->table( columns = temp26
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
    DATA temp28 TYPE ty_t_row.
    DATA temp29 LIKE LINE OF temp28.
    DATA lt_row LIKE temp28.
    DATA temp30 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp31 LIKE LINE OF temp30.
    CLEAR temp28.
    
    temp29-name = `Nut`.
    temp29-qty = 7.
    INSERT temp29 INTO TABLE temp28.
    
    lt_row = temp28.
    
    CLEAR temp30.
    
    temp31-name = ``.
    temp31-title = ``.
    temp31-width = 100.
    temp31-align = ``.
    INSERT temp31 INTO TABLE temp30.
    temp31-name = ``.
    temp31-title = ``.
    temp31-width = 100.
    temp31-align = ``.
    INSERT temp31 INTO TABLE temp30.
    temp31-name = `NONE`.
    temp31-title = ``.
    temp31-width = 100.
    temp31-align = ``.
    INSERT temp31 INTO TABLE temp30.
    mo_layout->table( columns = temp30
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
    DATA lo_bold TYPE REF TO zabaputil_if_pdf_font.
    DATA temp32 TYPE ty_t_row.
    DATA temp33 LIKE LINE OF temp32.
    DATA lt_row LIKE temp32.
    DATA temp34 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp35 LIKE LINE OF temp34.
    DATA lv_content TYPE string.
    DATA temp13 TYPE xsdboolean.
    DATA temp14 TYPE xsdboolean.
    lo_bold = mo_pdf->font( `Courier-Bold` ).
    
    CLEAR temp32.
    
    temp33-name = `Nut`.
    temp33-qty = 7.
    INSERT temp33 INTO TABLE temp32.
    
    lt_row = temp32.
    
    CLEAR temp34.
    
    temp35-name = `NAME`.
    temp35-title = `Name`.
    temp35-width = 100.
    temp35-align = `L`.
    INSERT temp35 INTO TABLE temp34.
    mo_layout->table( columns     = temp34
                      rows        = lt_row
                      header_font = lo_bold ).
    
    lv_content = mo_pdf->page( 1 )->content( ).
    " the bold cut reaches 6.26 above and 1.42 below the baseline
    
    temp13 = boolc( lv_content CS `BT /F2 10 Tf 53 780.22 Td <4E616D65> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp13 ).
    
    temp14 = boolc( lv_content CS `BT /F1 10 Tf 53 761.78 Td <4E7574> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp14 ).
  ENDMETHOD.

  METHOD table_without_columns.
    DATA lt_row TYPE ty_t_row.
    DATA lt_column TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp36 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp37 LIKE LINE OF temp36.
    mo_layout->table( columns = lt_column
                      rows    = lt_row ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_pdf->page_count( ) ).

    " columns and no rows: the header row alone
    
    CLEAR temp36.
    
    temp37-name = `NAME`.
    temp37-title = `Name`.
    temp37-width = 100.
    temp37-align = `L`.
    INSERT temp37 INTO TABLE temp36.
    mo_layout->table( columns = temp36
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
    TYPES temp2 TYPE STANDARD TABLE OF ty_s_deep WITH DEFAULT KEY.
DATA lt_deep TYPE temp2.
    DATA temp38 TYPE ty_s_deep.
    DATA temp1 TYPE string_table.
    DATA temp39 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp40 LIKE LINE OF temp39.
    CLEAR temp38.
    temp38-name = `Nut`.
    
    CLEAR temp1.
    INSERT `x` INTO TABLE temp1.
    temp38-t_sub = temp1.
    INSERT temp38 INTO TABLE lt_deep.
    
    CLEAR temp39.
    
    temp40-name = `NAME`.
    temp40-title = ``.
    temp40-width = 100.
    temp40-align = `L`.
    INSERT temp40 INTO TABLE temp39.
    temp40-name = `T_SUB`.
    temp40-title = ``.
    temp40-width = 100.
    temp40-align = `L`.
    INSERT temp40 INTO TABLE temp39.
    mo_layout->table( columns = temp39
                      rows    = lt_deep ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = count( val = mo_pdf->page( 1 )->content( )
                                                     sub = ` Tj` ) ).
  ENDMETHOD.

  METHOD table_from_texts.
    " cells that are texts already; a row with fewer cells than columns
    " has empty ones at its end
    DATA lt_row TYPE zabaputil_cl_pdf_layout=>ty_t_text_row.
    DATA temp41 TYPE string_table.
    DATA temp43 TYPE string_table.
    DATA temp45 TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA temp46 LIKE LINE OF temp45.
    DATA lv_content TYPE string.
    DATA temp15 TYPE xsdboolean.
    CLEAR temp41.
    INSERT `Nut` INTO TABLE temp41.
    INSERT `7` INTO TABLE temp41.
    INSERT temp41 INTO TABLE lt_row.
    
    CLEAR temp43.
    INSERT `Bolt` INTO TABLE temp43.
    INSERT temp43 INTO TABLE lt_row.
    
    CLEAR temp45.
    
    temp46-name = ``.
    temp46-title = `Name`.
    temp46-width = 100.
    temp46-align = `L`.
    INSERT temp46 INTO TABLE temp45.
    temp46-name = ``.
    temp46-title = `Qty`.
    temp46-width = 100.
    temp46-align = `R`.
    INSERT temp46 INTO TABLE temp45.
    mo_layout->table_text( columns = temp45
                           rows    = lt_row ).
    cl_abap_unit_assert=>assert_equals( exp = `105.5` act = num( mo_layout->y( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( page = 1
                                                     text = `Bolt` ) ).
    
    lv_content = mo_pdf->page( 1 )->content( ).
    
    temp15 = boolc( lv_content CS `q 0 0 0 rg BT /F1 10 Tf 241 761.78 Td <37> Tj ET Q` ).
    cl_abap_unit_assert=>assert_true(
        temp15 ).
    " header 2, first row 2, second row 1
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = count( val = lv_content
                                                     sub = ` Tj` ) ).
  ENDMETHOD.

  METHOD finish_page_numbers.
    DATA lv_content TYPE string.
    DATA temp16 TYPE xsdboolean.
    DATA temp17 TYPE xsdboolean.
    mo_layout->set_footer( `{page}/{pages}` ).
    mo_layout->text( `a` ).
    mo_layout->page_break( ).
    mo_layout->text( `b` ).
    mo_layout->finish( ).

    " in the middle of the bottom margin, 8 points, centred: three
    " characters are 14.4 wide
    
    lv_content = mo_pdf->page( 1 )->content( ).
    
    temp16 = boolc( lv_content CS `BT /F1 8 Tf 290.44 25 Td <312F32> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp16 ).
    lv_content = mo_pdf->page( 2 )->content( ).
    
    temp17 = boolc( lv_content CS `BT /F1 8 Tf 290.44 25 Td <322F32> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp17 ).
  ENDMETHOD.

  METHOD finish_header.
    DATA lv_content TYPE string.
    DATA temp18 TYPE xsdboolean.
    mo_layout->set_header( val   = `Page {page}`
                           align = `r`
                           size  = 10 ).
    mo_layout->text( `a` ).
    mo_layout->finish( ).
    " in the middle of the top margin, ending at the right margin: six
    " characters are 36 wide
    
    lv_content = mo_pdf->page( 1 )->content( ).
    
    temp18 = boolc( lv_content CS `BT /F1 10 Tf 509.28 816.89 Td <506167652031> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp18 ).
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
    DATA lo_layout TYPE REF TO zabaputil_cl_pdf_layout.
    CREATE OBJECT lo_layout TYPE zabaputil_cl_pdf_layout EXPORTING PDF = mo_pdf.
    lo_layout->finish( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_pdf->page_count( ) ).

    " a footer needs a font
    CREATE OBJECT lo_layout TYPE zabaputil_cl_pdf_layout EXPORTING PDF = mo_pdf.
    lo_layout->set_footer( `x` ).
    lo_layout->page( ).
    TRY.
        lo_layout->finish( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

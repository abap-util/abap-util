"! An object as the data of a form: what is public is printed, the rest is not
CLASS lcl_data DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_head,
        city TYPE string,
      END OF ty_s_head.

    CONSTANTS c_fixed TYPE string VALUE `constant`.
    CLASS-DATA gv_static TYPE string.
    DATA shown  TYPE string.
    DATA head   TYPE ty_s_head.
    DATA t_rows TYPE string_table.
    DATA other  TYPE REF TO lcl_data.

    METHODS constructor.

  PRIVATE SECTION.

    DATA hidden TYPE string.

ENDCLASS.

CLASS lcl_data IMPLEMENTATION.

  METHOD constructor.
    shown = `visible`.
    hidden = `secret`.
    head-city = `Bonn`.
    gv_static = `static`.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_form DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_item,
        pos  TYPE i,
        text TYPE string,
      END OF ty_s_item.
    TYPES:
      BEGIN OF ty_s_head,
        city TYPE string,
      END OF ty_s_head.
    TYPES:
      BEGIN OF ty_s_data,
        name    TYPE string,
        number  TYPE i,
        head    TYPE ty_s_head,
        t_items TYPE STANDARD TABLE OF ty_s_item WITH DEFAULT KEY,
        logo    TYPE xstring,
      END OF ty_s_data.

    DATA ms_form TYPE zabaputil_cl_pdf_form=>ty_s_form.
    DATA ms_data TYPE ty_s_data.

    METHODS setup.

    METHODS new_form_defaults       FOR TESTING.
    METHODS resolve_components      FOR TESTING.
    METHODS resolve_unknown_stays   FOR TESTING.
    METHODS resolve_without_data    FOR TESTING.
    METHODS empty_form_one_page     FOR TESTING.
    METHODS page_format             FOR TESTING.
    METHODS flow_text               FOR TESTING.
    METHODS flow_text_placeholders  FOR TESTING.
    METHODS flow_space_and_break    FOR TESTING.
    METHODS flow_rule               FOR TESTING.
    METHODS flow_table_rows         FOR TESTING.
    METHODS flow_table_sample_rows  FOR TESTING.
    METHODS flow_table_no_columns   FOR TESTING.
    METHODS fixed_text_position     FOR TESTING.
    METHODS fixed_text_wraps        FOR TESTING.
    METHODS fixed_first_or_every    FOR TESTING.
    METHODS fixed_line_and_rect     FOR TESTING.
    METHODS image_of_the_form       FOR TESTING.
    METHODS image_from_data         FOR TESTING.
    METHODS header_and_footer       FOR TESTING.
    METHODS title_of_the_document   FOR TESTING.
    METHODS render_file             FOR TESTING.
    METHODS unknown_type_is_skipped FOR TESTING.
    METHODS object_public_attributes FOR TESTING.
    METHODS object_through_reference FOR TESTING.
    METHODS object_table_rows       FOR TESTING.
    METHODS fields_of_a_structure   FOR TESTING.
    METHODS fields_of_an_object     FOR TESTING.
    METHODS fields_stop_at_a_cycle  FOR TESTING.
    METHODS data_sample_of_a_class  FOR TESTING.
    METHODS data_sample_refused     FOR TESTING.
    METHODS form_with_its_data_class FOR TESTING.
    METHODS sample_by_type          FOR TESTING.
    METHODS sample_keeps_what_is_set FOR TESTING.
    METHODS text_of_a_form          FOR TESTING.
    METHODS text_without_initial    FOR TESTING.
    METHODS text_read_tolerantly    FOR TESTING.
    METHODS text_that_is_none       FOR TESTING.
    METHODS form_of_a_data_class    FOR TESTING.
    METHODS print_a_data_class      FOR TESTING.
    METHODS absolute_text_position  FOR TESTING.
    METHODS absolute_has_no_flow    FOR TESTING.
    METHODS absolute_table_position FOR TESTING.
    METHODS absolute_below_a_table  FOR TESTING.
    METHODS absolute_table_pages    FOR TESTING.
    METHODS absolute_pushed_over    FOR TESTING.
    METHODS absolute_every_page     FOR TESTING.
    METHODS text_keeps_blanks       FOR TESTING.
    METHODS to_absolute_places      FOR TESTING.
    METHODS to_absolute_with_data   FOR TESTING.
    METHODS to_absolute_unchanged   FOR TESTING.
    METHODS barcode_fixed           FOR TESTING.
    METHODS barcode_in_the_flow     FOR TESTING.
    METHODS barcode_absolute        FOR TESTING.

    "! the table of the test data with as many rows as asked for
    METHODS items
      IMPORTING
        val TYPE i.

    " a PNG file of 2 x 2 pixels
    METHODS image_rgb
      RETURNING
        VALUE(result) TYPE xstring.

    "! How often a text is shown on a page, in Helvetica
    METHODS shown
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        page          TYPE i
        text          TYPE string
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.

CLASS ltcl_form IMPLEMENTATION.

  METHOD image_rgb.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D4948445200000002000000020802000000FDD49A73000000054944415478DA63F8CF51E86AD7` &&
             `0000000D49444154C0C000C20CFF8100001FEE05FB61FE8DCC0000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD setup.
    DATA temp1 TYPE ltcl_form=>ty_s_data-t_items.
    DATA temp2 LIKE LINE OF temp1.
    " A4 with 20 mm margins: the flow starts 20 mm from the top and from
    " the left, in Helvetica at 10 points
    ms_form = zabaputil_cl_pdf_form=>new_form( ).
    ms_data-name = `Ada`.
    ms_data-number = 4711.
    ms_data-head-city = `Bonn`.
    
    CLEAR temp1.
    
    temp2-pos = 10.
    temp2-text = `Nut`.
    INSERT temp2 INTO TABLE temp1.
    temp2-pos = 20.
    temp2-text = `Bolt`.
    INSERT temp2 INTO TABLE temp1.
    ms_data-t_items = temp1.
  ENDMETHOD.

  METHOD shown.
    result = count( val = pdf->page( page )->content( )
                    sub = |{ pdf->font( `Helvetica` )->encode( text ) } Tj| ).
  ENDMETHOD.

  METHOD new_form_defaults.
    cl_abap_unit_assert=>assert_equals( exp = `A4` act = ms_form-format ).
    cl_abap_unit_assert=>assert_equals( exp = `P` act = ms_form-orientation ).
    cl_abap_unit_assert=>assert_equals( exp = 20 act = ms_form-margin_left ).
    cl_abap_unit_assert=>assert_equals( exp = 20 act = ms_form-margin_bottom ).
    cl_abap_unit_assert=>assert_initial( ms_form-t_element ).
  ENDMETHOD.

  METHOD resolve_components.
    cl_abap_unit_assert=>assert_equals(
        exp = `Dear Ada, order 4711 to Bonn.`
        act = zabaputil_cl_pdf_form=>resolve( val  = `Dear {NAME}, order {number} to {HEAD-CITY}.`
                                              data = ms_data ) ).
    " a dot separates like a hyphen, blanks around the name do not count
    cl_abap_unit_assert=>assert_equals(
        exp = `Bonn/Ada`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{head.city}/{ name }`
                                              data = ms_data ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `no placeholder`
        act = zabaputil_cl_pdf_form=>resolve( val  = `no placeholder`
                                              data = ms_data ) ).
  ENDMETHOD.

  METHOD resolve_unknown_stays.
    " a name the data does not have, a table, a structure, a path through
    " a value, an open brace: all stay as they are written
    cl_abap_unit_assert=>assert_equals(
        exp = `{NONE} {T_ITEMS} {HEAD} {NAME-X} {} {NAME`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{NONE} {T_ITEMS} {HEAD} {NAME-X} {} {NAME`
                                              data = ms_data ) ).
    " the data is no structure at all
    cl_abap_unit_assert=>assert_equals(
        exp = `{NAME}`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{NAME}`
                                              data = `text` ) ).
  ENDMETHOD.

  METHOD resolve_without_data.
    cl_abap_unit_assert=>assert_equals( exp = `Dear {NAME}`
                                        act = zabaputil_cl_pdf_form=>resolve( `Dear {NAME}` ) ).
  ENDMETHOD.

  METHOD empty_form_one_page.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_initial( lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD page_format.
    DATA lo_page TYPE REF TO zabaputil_cl_pdf_page.
    ms_form-format = `A5`.
    ms_form-orientation = `L`.
    
    lo_page = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = zabaputil_cl_pdf_writer=>num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `419.53` act = zabaputil_cl_pdf_writer=>num( lo_page->height_pt( ) ) ).

    " a form that names no format is A4 portrait
    CLEAR ms_form-format.
    CLEAR ms_form-orientation.
    lo_page = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = zabaputil_cl_pdf_writer=>num( lo_page->width_pt( ) ) ).
  ENDMETHOD.

  METHOD flow_text.
    DATA temp3 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp4 LIKE LINE OF temp3.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CLEAR temp3.
    
    temp4-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp4-text = `Hi`.
    temp4-font = `Courier`.
    temp4-size = 12.
    temp4-color = `FF0000`.
    INSERT temp4 INTO TABLE temp3.
    ms_form-t_element = temp3.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    " 20 mm are 56.693 points; the line is 15 high, the baseline of
    " Courier at 12 points 10.332 below its top
    cl_abap_unit_assert=>assert_equals( exp = `q 1 0 0 rg BT /F2 12 Tf 56.693 774.865 Td <4869> Tj ET Q`
                                        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD flow_text_placeholders.
    DATA temp5 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp6 LIKE LINE OF temp5.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CLEAR temp5.
    
    temp6-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp6-text = `Dear {NAME}`.
    INSERT temp6 INTO TABLE temp5.
    ms_form-t_element = temp5.
    " with data the component is printed, without it the placeholder
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Dear Ada` ) ).
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Dear {NAME}` ) ).
  ENDMETHOD.

  METHOD flow_space_and_break.
    DATA temp7 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp8 LIKE LINE OF temp7.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA lv_content TYPE string.
    DATA temp1 TYPE xsdboolean.
    DATA temp2 TYPE xsdboolean.
    CLEAR temp7.
    
    temp8-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp8-text = `a`.
    temp8-h = 0.
    INSERT temp8 INTO TABLE temp7.
    temp8-type = zabaputil_cl_pdf_form=>cs_type-space.
    temp8-text = ``.
    temp8-h = 30.
    INSERT temp8 INTO TABLE temp7.
    temp8-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp8-text = `b`.
    temp8-h = 0.
    INSERT temp8 INTO TABLE temp7.
    temp8-type = zabaputil_cl_pdf_form=>cs_type-page_break.
    temp8-text = ``.
    temp8-h = 0.
    INSERT temp8 INTO TABLE temp7.
    temp8-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp8-text = `c`.
    temp8-h = 0.
    INSERT temp8 INTO TABLE temp7.
    ms_form-t_element = temp7.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_pdf->page_count( ) ).
    " a starts at 20 mm, its baseline 8.805 points below the top of its
    " line; b one line (12.5 points) and 30 mm (85.039) further down
    
    lv_content = lo_pdf->page( 1 )->content( ).
    
    temp1 = boolc( lv_content CS `BT /F1 10 Tf 56.693 776.392 Td <61> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp1 ).
    
    temp2 = boolc( lv_content CS `BT /F1 10 Tf 56.693 678.853 Td <62> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp2 ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 10 Tf 56.693 776.392 Td <63> Tj ET`
                                        act = lo_pdf->page( 2 )->content( ) ).
  ENDMETHOD.

  METHOD flow_rule.
    DATA temp9 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp10 LIKE LINE OF temp9.
    DATA lv_content TYPE string.
    DATA temp3 TYPE xsdboolean.
    CLEAR temp9.
    
    temp10-type = zabaputil_cl_pdf_form=>cs_type-line.
    INSERT temp10 INTO TABLE temp9.
    ms_form-t_element = temp9.
    
    lv_content = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ).
    " from margin to margin
    
    temp3 = boolc( lv_content CS `56.693 778.947 m 538.587 778.947 l S` ).
    cl_abap_unit_assert=>assert_true( temp3 ).
  ENDMETHOD.

  METHOD flow_table_rows.
    DATA temp11 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp12 LIKE LINE OF temp11.
    DATA temp1 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp2 LIKE LINE OF temp1.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA lv_content TYPE string.
    DATA temp4 TYPE xsdboolean.
    CLEAR temp11.
    
    temp12-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp12-source = `t_items`.
    
    CLEAR temp1.
    
    temp2-name = `pos`.
    temp2-title = `Pos`.
    temp2-width = 20.
    temp2-align = `R`.
    INSERT temp2 INTO TABLE temp1.
    temp2-name = `text`.
    temp2-title = `Item`.
    temp2-width = 0.
    temp2-align = `L`.
    INSERT temp2 INTO TABLE temp1.
    temp12-t_column = temp1.
    INSERT temp12 INTO TABLE temp11.
    ms_form-t_element = temp11.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Nut` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `20` ) ).
    " the header row is in the bold cut of the font
    
    lv_content = lo_pdf->page( 1 )->content( ).
    
    temp4 = boolc( lv_content CS |{ lo_pdf->font( `Helvetica-Bold` )->encode( `Item` ) } Tj| ).
    cl_abap_unit_assert=>assert_true(
        temp4 ).
    " header, two rows, two columns
    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = count( val = lv_content
                                                     sub = ` Tj` ) ).
  ENDMETHOD.

  METHOD flow_table_sample_rows.
    DATA temp13 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp14 LIKE LINE OF temp13.
    DATA temp3 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp4 LIKE LINE OF temp3.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    FIELD-SYMBOLS <temp15> LIKE LINE OF ms_form-t_element.
    DATA temp16 LIKE sy-tabix.
    FIELD-SYMBOLS <temp17> LIKE LINE OF ms_form-t_element.
    DATA temp18 LIKE sy-tabix.
    CLEAR temp13.
    
    temp14-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp14-source = `t_items`.
    
    CLEAR temp3.
    
    temp4-name = `pos`.
    temp4-title = `Pos`.
    temp4-width = 20.
    temp4-align = `R`.
    INSERT temp4 INTO TABLE temp3.
    temp4-name = `text`.
    temp4-title = `Item`.
    temp4-width = 0.
    temp4-align = `L`.
    INSERT temp4 INTO TABLE temp3.
    temp14-t_column = temp3.
    INSERT temp14 INTO TABLE temp13.
    ms_form-t_element = temp13.
    " printed without data: three rows that name the components
    
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `{TEXT}` ) ).
    " and with data that has no such table
    
    
    temp16 = sy-tabix.
    READ TABLE ms_form-t_element INDEX 1 ASSIGNING <temp15>.
    sy-tabix = temp16.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp15>-source = `none`.
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                           data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `{POS}` ) ).
    " a component that is no table
    
    
    temp18 = sy-tabix.
    READ TABLE ms_form-t_element INDEX 1 ASSIGNING <temp17>.
    sy-tabix = temp18.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp17>-source = `name`.
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                           data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `{POS}` ) ).
  ENDMETHOD.

  METHOD flow_table_no_columns.
    DATA temp19 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp20 LIKE LINE OF temp19.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CLEAR temp19.
    
    temp20-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp20-source = `t_items`.
    INSERT temp20 INTO TABLE temp19.
    ms_form-t_element = temp19.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_initial( lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD fixed_text_position.
    " y is the top of the line: the baseline of Helvetica at 10 points is
    " 7.18 points below it. 10 mm from the left, 30 mm from the top
    DATA temp21 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp22 LIKE LINE OF temp21.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CLEAR temp21.
    
    temp22-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp22-fixed = abap_true.
    temp22-x = 10.
    temp22-y = 30.
    temp22-text = `No. {NUMBER}`.
    INSERT temp22 INTO TABLE temp21.
    ms_form-t_element = temp21.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals(
        exp = |BT /F1 10 Tf 28.346 749.671 Td { lo_pdf->font( `Helvetica` )->encode( `No. 4711` ) } Tj ET|
        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD fixed_text_wraps.
    " 20 mm are 56.693 points: `aaaa bbbb` in Courier at 10 points is 54
    " wide and fits, the third word does not. Right aligned in the box
    DATA temp23 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp24 LIKE LINE OF temp23.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA lv_content TYPE string.
    DATA temp5 TYPE xsdboolean.
    DATA temp6 TYPE xsdboolean.
    DATA temp7 TYPE xsdboolean.
    CLEAR temp23.
    
    temp24-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp24-fixed = abap_true.
    temp24-x = 10.
    temp24-y = 30.
    temp24-w = 20.
    temp24-font = `Courier`.
    temp24-align = `R`.
    temp24-text = |aaaa bbbb cccc\nd|.
    INSERT temp24 INTO TABLE temp23.
    ms_form-t_element = temp23.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    
    lv_content = lo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = count( val = lv_content
                                                     sub = ` Tj` ) ).
    " the box ends at 30 mm = 85.039 points; the lines are 12.5 points apart
    
    temp5 = boolc( lv_content CS `BT /F2 10 Tf 31.039 750.561 Td <616161612062626262> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp5 ).
    
    temp6 = boolc( lv_content CS `BT /F2 10 Tf 61.039 738.061 Td <63636363> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp6 ).
    
    temp7 = boolc( lv_content CS `BT /F2 10 Tf 79.039 725.561 Td <64> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp7 ).
  ENDMETHOD.

  METHOD fixed_first_or_every.
    DATA temp25 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp26 LIKE LINE OF temp25.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CLEAR temp25.
    
    temp26-type = zabaputil_cl_pdf_form=>cs_type-page_break.
    temp26-fixed = abap_false.
    temp26-every_page = abap_false.
    temp26-text = ``.
    INSERT temp26 INTO TABLE temp25.
    temp26-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp26-fixed = abap_true.
    temp26-every_page = abap_false.
    temp26-text = `first`.
    INSERT temp26 INTO TABLE temp25.
    temp26-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp26-fixed = abap_true.
    temp26-every_page = abap_true.
    temp26-text = `every`.
    INSERT temp26 INTO TABLE temp25.
    ms_form-t_element = temp25.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `first` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = shown( pdf  = lo_pdf
                                                     page = 2
                                                     text = `first` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `every` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 2
                                                     text = `every` ) ).
  ENDMETHOD.

  METHOD fixed_line_and_rect.
    " in points: 10 mm = 28.346, 20 mm = 56.693, 100 mm = 283.465
    DATA temp27 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp28 LIKE LINE OF temp27.
    DATA lv_content TYPE string.
    CLEAR temp27.
    
    temp28-type = zabaputil_cl_pdf_form=>cs_type-line.
    temp28-fixed = abap_true.
    temp28-x = 10.
    temp28-y = 10.
    temp28-w = 100.
    temp28-h = 0.
    temp28-size = 2.
    temp28-color = `FF0000`.
    temp28-fill = ``.
    INSERT temp28 INTO TABLE temp27.
    temp28-type = zabaputil_cl_pdf_form=>cs_type-rect.
    temp28-fixed = abap_false.
    temp28-x = 10.
    temp28-y = 10.
    temp28-w = 100.
    temp28-h = 20.
    temp28-size = 0.
    temp28-color = ``.
    temp28-fill = ``.
    INSERT temp28 INTO TABLE temp27.
    temp28-type = zabaputil_cl_pdf_form=>cs_type-rect.
    temp28-fixed = abap_true.
    temp28-x = 10.
    temp28-y = 10.
    temp28-w = 100.
    temp28-h = 20.
    temp28-size = 0.
    temp28-color = ``.
    temp28-fill = `0000FF`.
    INSERT temp28 INTO TABLE temp27.
    ms_form-t_element = temp27.
    
    lv_content = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals(
        exp = |1 0 0 RG\n2 w\n28.346 813.544 m 311.811 813.544 l S\n| &&
              |0 0 0 RG\n0.5 w\n28.346 756.851 283.465 56.693 re S\n| &&
              |0 0 0 RG\n0.5 w\n0 0 1 rg\n28.346 756.851 283.465 56.693 re B|
        act = lv_content ).
  ENDMETHOD.

  METHOD image_of_the_form.
    " fixed: 40 mm wide, the height follows from 2 x 2 pixels. In the
    " flow: at the left margin, and the text after it starts below it
    DATA temp29 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp30 LIKE LINE OF temp29.
    DATA lv_content TYPE string.
    DATA temp8 TYPE xsdboolean.
    DATA temp9 TYPE xsdboolean.
    DATA temp10 TYPE xsdboolean.
    DATA temp31 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp32 LIKE LINE OF temp31.
    CLEAR temp29.
    
    temp30-type = zabaputil_cl_pdf_form=>cs_type-image.
    temp30-fixed = abap_true.
    temp30-x = 10.
    temp30-y = 10.
    temp30-w = 40.
    temp30-text = ``.
    temp30-image = image_rgb( ).
    INSERT temp30 INTO TABLE temp29.
    temp30-type = zabaputil_cl_pdf_form=>cs_type-image.
    temp30-fixed = abap_false.
    temp30-x = 0.
    temp30-y = 0.
    temp30-w = 10.
    temp30-text = ``.
    temp30-image = image_rgb( ).
    INSERT temp30 INTO TABLE temp29.
    temp30-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp30-fixed = abap_false.
    temp30-x = 0.
    temp30-y = 0.
    temp30-w = 0.
    temp30-text = `a`.
    temp30-image = ``.
    INSERT temp30 INTO TABLE temp29.
    ms_form-t_element = temp29.
    
    lv_content = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ).
    " the flow is written first, so its image is the first one of the file;
    " 10 mm of image move the text below it down by 28.346 points
    
    temp8 = boolc( lv_content CS `q 28.346 0 0 28.346 56.693 756.851 cm /I1 Do Q` ).
    cl_abap_unit_assert=>assert_true( temp8 ).
    
    temp9 = boolc( lv_content CS `BT /F1 10 Tf 56.693 748.046 Td <61> Tj ET` ).
    cl_abap_unit_assert=>assert_true( temp9 ).
    
    temp10 = boolc( lv_content CS `q 113.386 0 0 113.386 28.346 700.158 cm /I2 Do Q` ).
    cl_abap_unit_assert=>assert_true( temp10 ).

    " an element without an image prints nothing
    
    CLEAR temp31.
    
    temp32-type = zabaputil_cl_pdf_form=>cs_type-image.
    temp32-fixed = abap_true.
    INSERT temp32 INTO TABLE temp31.
    ms_form-t_element = temp31.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD image_from_data.
    DATA temp33 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp34 LIKE LINE OF temp33.
    DATA lv_content TYPE string.
    ms_data-logo = image_rgb( ).
    
    CLEAR temp33.
    
    temp34-type = zabaputil_cl_pdf_form=>cs_type-image.
    temp34-fixed = abap_true.
    temp34-x = 10.
    temp34-y = 10.
    temp34-w = 40.
    temp34-source = `logo`.
    INSERT temp34 INTO TABLE temp33.
    ms_form-t_element = temp33.
    
    lv_content = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                     data = ms_data )->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals( exp = `q 113.386 0 0 113.386 28.346 700.158 cm /I1 Do Q`
                                        act = lv_content ).
    " without data there is nothing to show
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD header_and_footer.
    DATA temp35 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp36 LIKE LINE OF temp35.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    ms_form-header = `Order {NUMBER}`.
    ms_form-footer = `{page}/{pages}`.
    
    CLEAR temp35.
    
    temp36-type = zabaputil_cl_pdf_form=>cs_type-page_break.
    INSERT temp36 INTO TABLE temp35.
    ms_form-t_element = temp35.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 2
                                                     text = `Order 4711` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `1/2` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 2
                                                     text = `2/2` ) ).
  ENDMETHOD.

  METHOD title_of_the_document.
    DATA lv_hex TYPE string.
    DATA temp11 TYPE xsdboolean.
    ms_form-title = `Ti`.
    
    lv_hex = zabaputil_cl_pdf_form=>render( ms_form ).
    
    temp11 = boolc( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/Title <FEFF00540069>` ) ).
    cl_abap_unit_assert=>assert_true( temp11 ).
  ENDMETHOD.

  METHOD render_file.
    DATA temp37 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp38 LIKE LINE OF temp37.
    DATA lv_hex TYPE string.
    DATA temp12 TYPE xsdboolean.
    CLEAR temp37.
    
    temp38-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp38-text = `Dear {NAME}`.
    INSERT temp38 INTO TABLE temp37.
    ms_form-t_element = temp37.
    
    lv_hex = zabaputil_cl_pdf_form=>render( val  = ms_form
                                            data = ms_data ).
    " %PDF-1.4 at the start, %%EOF and a line feed at the end
    cl_abap_unit_assert=>assert_equals( exp = `255044462D312E34`
                                        act = substring( val = lv_hex
                                                         len = 16 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2525454F460A`
                                        act = substring( val = lv_hex
                                                         off = strlen( lv_hex ) - 12 ) ).
    
    temp12 = boolc( xstrlen( zabaputil_cl_pdf_form=>render( ms_form ) ) > 300 ).
    cl_abap_unit_assert=>assert_true( temp12 ).
  ENDMETHOD.

  METHOD unknown_type_is_skipped.
    DATA temp39 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp40 LIKE LINE OF temp39.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CLEAR temp39.
    
    temp40-type = `CHART`.
    temp40-fixed = abap_false.
    temp40-text = `x`.
    INSERT temp40 INTO TABLE temp39.
    temp40-type = `CHART`.
    temp40-fixed = abap_true.
    temp40-text = `x`.
    INSERT temp40 INTO TABLE temp39.
    temp40-type = ``.
    temp40-fixed = abap_false.
    temp40-text = `x`.
    INSERT temp40 INTO TABLE temp39.
    ms_form-t_element = temp39.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_initial( lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD object_public_attributes.
    DATA lo_data TYPE REF TO lcl_data.
    CREATE OBJECT lo_data TYPE lcl_data.
    cl_abap_unit_assert=>assert_equals(
        exp = `visible in Bonn`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{SHOWN} in {head-city}`
                                              data = lo_data ) ).
    " private, static, a constant, a method name, nothing at all: no part
    " of what the object hands to a form
    cl_abap_unit_assert=>assert_equals(
        exp = `{HIDDEN} {GV_STATIC} {C_FIXED} {CONSTRUCTOR} {NONE}`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{HIDDEN} {GV_STATIC} {C_FIXED} {CONSTRUCTOR} {NONE}`
                                              data = lo_data ) ).
    " a table and an object are no text
    cl_abap_unit_assert=>assert_equals(
        exp = `{T_ROWS} {OTHER}`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{T_ROWS} {OTHER}`
                                              data = lo_data ) ).
  ENDMETHOD.

  METHOD object_through_reference.
    " an attribute that is an object is a level of the path like a structure
    DATA lo_data TYPE REF TO lcl_data.
    CREATE OBJECT lo_data TYPE lcl_data.
    CREATE OBJECT lo_data->other TYPE lcl_data.
    lo_data->other->shown = `inner`.
    cl_abap_unit_assert=>assert_equals(
        exp = `inner Bonn`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{OTHER-SHOWN} {OTHER-HEAD-CITY}`
                                              data = lo_data ) ).
    " an initial reference leads nowhere
    CLEAR lo_data->other.
    cl_abap_unit_assert=>assert_equals(
        exp = `{OTHER-SHOWN}`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{OTHER-SHOWN}`
                                              data = lo_data ) ).
  ENDMETHOD.

  METHOD object_table_rows.
    DATA lo_demo TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA temp41 TYPE zabaputil_cl_pdf_data_demo=>ty_t_item.
    DATA temp42 LIKE LINE OF temp41.
    DATA temp43 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp44 LIKE LINE OF temp43.
    DATA temp5 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp6 LIKE LINE OF temp5.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CREATE OBJECT lo_demo TYPE zabaputil_cl_pdf_data_demo.
    lo_demo->vbeln = `80004711`.
    lo_demo->address-city = `Bonn`.
    
    CLEAR temp41.
    
    temp42-posnr = 10.
    temp42-arktx = `Hex nut M8`.
    INSERT temp42 INTO TABLE temp41.
    temp42-posnr = 20.
    temp42-arktx = `Hex bolt M8 x 40`.
    INSERT temp42 INTO TABLE temp41.
    temp42-posnr = 30.
    temp42-arktx = `Washer 8.4 mm`.
    INSERT temp42 INTO TABLE temp41.
    temp42-posnr = 40.
    temp42-arktx = `Thread locking fluid`.
    INSERT temp42 INTO TABLE temp41.
    lo_demo->t_items = temp41.
    
    CLEAR temp43.
    
    CLEAR temp44.
    temp44-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp44-text = `Delivery {VBELN} to {ADDRESS-CITY}`.
    INSERT temp44 INTO TABLE temp43.
    CLEAR temp44.
    temp44-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp44-source = `t_items`.
    
    CLEAR temp5.
    
    temp6-name = `posnr`.
    temp6-title = `Item`.
    temp6-width = 20.
    temp6-align = `R`.
    INSERT temp6 INTO TABLE temp5.
    temp6-name = `arktx`.
    temp6-title = `Material`.
    temp6-width = 0.
    temp6-align = `L`.
    INSERT temp6 INTO TABLE temp5.
    temp44-t_column = temp5.
    INSERT temp44 INTO TABLE temp43.
    ms_form-t_element = temp43.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = lo_demo ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Delivery 80004711 to Bonn` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Washer 8.4 mm` ) ).
    " the text, a header row and four rows of two cells
    cl_abap_unit_assert=>assert_equals( exp = 11
                                        act = count( val = lo_pdf->page( 1 )->content( )
                                                     sub = ` Tj` ) ).
  ENDMETHOD.

  METHOD fields_of_a_structure.
    DATA lt_field TYPE zabaputil_cl_pdf_form=>ty_t_field.
    DATA temp45 TYPE zabaputil_cl_pdf_form=>ty_s_field.
    FIELD-SYMBOLS <temp7> LIKE LINE OF lt_field.
    DATA temp8 LIKE sy-tabix.
    DATA temp46 TYPE zabaputil_cl_pdf_form=>ty_s_field.
    FIELD-SYMBOLS <temp9> LIKE LINE OF lt_field.
    DATA temp10 LIKE sy-tabix.
    DATA temp47 TYPE zabaputil_cl_pdf_form=>ty_s_field.
    FIELD-SYMBOLS <temp11> LIKE LINE OF lt_field.
    DATA temp12 LIKE sy-tabix.
    DATA temp48 TYPE zabaputil_cl_pdf_form=>ty_s_field.
    FIELD-SYMBOLS <temp13> LIKE LINE OF lt_field.
    DATA temp14 LIKE sy-tabix.
    lt_field = zabaputil_cl_pdf_form=>fields( ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 7 act = lines( lt_field ) ).
    
    CLEAR temp45.
    temp45-path = `NAME`.
    temp45-kind = `VALUE`.
    
    
    temp8 = sy-tabix.
    READ TABLE lt_field INDEX 1 ASSIGNING <temp7>.
    sy-tabix = temp8.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp45
        act = <temp7> ).
    " a structure is a level of the path, not a field of its own
    
    CLEAR temp46.
    temp46-path = `HEAD-CITY`.
    temp46-kind = `VALUE`.
    
    
    temp10 = sy-tabix.
    READ TABLE lt_field INDEX 3 ASSIGNING <temp9>.
    sy-tabix = temp10.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp46
        act = <temp9> ).
    " a table, then one row per component of its rows
    
    CLEAR temp47.
    temp47-path = `T_ITEMS`.
    temp47-kind = `TABLE`.
    
    
    temp12 = sy-tabix.
    READ TABLE lt_field INDEX 4 ASSIGNING <temp11>.
    sy-tabix = temp12.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp47
        act = <temp11> ).
    
    CLEAR temp48.
    temp48-path = `TEXT`.
    temp48-kind = `COLUMN`.
    temp48-parent = `T_ITEMS`.
    
    
    temp14 = sy-tabix.
    READ TABLE lt_field INDEX 6 ASSIGNING <temp13>.
    sy-tabix = temp14.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp48
        act = <temp13> ).
    " a text has no fields
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>fields( `text` ) ).
  ENDMETHOD.

  METHOD fields_of_an_object.
    DATA lo_demo TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA lt_field TYPE zabaputil_cl_pdf_form=>ty_t_field.
    DATA temp13 TYPE xsdboolean.
    DATA temp1 LIKE sy-subrc.
    DATA temp14 TYPE xsdboolean.
    DATA temp2 LIKE sy-subrc.
    DATA temp16 TYPE xsdboolean.
    DATA temp3 LIKE sy-subrc.
    DATA temp17 TYPE xsdboolean.
    DATA temp4 LIKE sy-subrc.
    DATA lt_local TYPE zabaputil_cl_pdf_form=>ty_t_field.
    DATA temp15 TYPE REF TO lcl_data.
    DATA temp18 TYPE xsdboolean.
    DATA temp5 LIKE sy-subrc.
    DATA temp19 TYPE xsdboolean.
    DATA temp6 LIKE sy-subrc.
    DATA temp20 TYPE xsdboolean.
    DATA temp7 LIKE sy-subrc.
    DATA temp21 TYPE xsdboolean.
    DATA temp8 LIKE sy-subrc.
    DATA temp22 TYPE xsdboolean.
    DATA temp9 LIKE sy-subrc.
    DATA temp23 TYPE xsdboolean.
    DATA temp10 LIKE sy-subrc.
    CREATE OBJECT lo_demo TYPE zabaputil_cl_pdf_data_demo.
    
    lt_field = zabaputil_cl_pdf_form=>fields( lo_demo ).
    
    
    READ TABLE lt_field WITH KEY path = `VBELN` kind = `VALUE` TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.
    temp13 = boolc( temp1 = 0 ).
    cl_abap_unit_assert=>assert_true( temp13 ).
    
    
    READ TABLE lt_field WITH KEY path = `ADDRESS-CITY` kind = `VALUE` TRANSPORTING NO FIELDS.
    temp2 = sy-subrc.
    temp14 = boolc( temp2 = 0 ).
    cl_abap_unit_assert=>assert_true( temp14 ).
    
    
    READ TABLE lt_field WITH KEY path = `T_ITEMS` kind = `TABLE` TRANSPORTING NO FIELDS.
    temp3 = sy-subrc.
    temp16 = boolc( temp3 = 0 ).
    cl_abap_unit_assert=>assert_true( temp16 ).
    
    
    READ TABLE lt_field WITH KEY path = `MATNR` kind = `COLUMN` parent = `T_ITEMS` TRANSPORTING NO FIELDS.
    temp4 = sy-subrc.
    temp17 = boolc( temp4 = 0 ).
    cl_abap_unit_assert=>assert_true(
        temp17 ).
    " four values, three of the address, the table and its five columns
    cl_abap_unit_assert=>assert_equals( exp = 13 act = lines( lt_field ) ).

    " only what is public: no private attribute, no constant, no static
    
    
    CREATE OBJECT temp15 TYPE lcl_data.
    lt_local = zabaputil_cl_pdf_form=>fields( temp15 ).
    
    
    READ TABLE lt_local WITH KEY path = `SHOWN` TRANSPORTING NO FIELDS.
    temp5 = sy-subrc.
    temp18 = boolc( temp5 = 0 ).
    cl_abap_unit_assert=>assert_true( temp18 ).
    
    
    READ TABLE lt_local WITH KEY path = `HIDDEN` TRANSPORTING NO FIELDS.
    temp6 = sy-subrc.
    temp19 = boolc( temp6 = 0 ).
    cl_abap_unit_assert=>assert_false( temp19 ).
    
    
    READ TABLE lt_local WITH KEY path = `C_FIXED` TRANSPORTING NO FIELDS.
    temp7 = sy-subrc.
    temp20 = boolc( temp7 = 0 ).
    cl_abap_unit_assert=>assert_false( temp20 ).
    
    
    READ TABLE lt_local WITH KEY path = `GV_STATIC` TRANSPORTING NO FIELDS.
    temp8 = sy-subrc.
    temp21 = boolc( temp8 = 0 ).
    cl_abap_unit_assert=>assert_false( temp21 ).
    " rows that are no structure: a table without columns
    
    
    READ TABLE lt_local WITH KEY path = `T_ROWS` kind = `TABLE` TRANSPORTING NO FIELDS.
    temp9 = sy-subrc.
    temp22 = boolc( temp9 = 0 ).
    cl_abap_unit_assert=>assert_true( temp22 ).
    
    
    READ TABLE lt_local WITH KEY parent = `T_ROWS` TRANSPORTING NO FIELDS.
    temp10 = sy-subrc.
    temp23 = boolc( temp10 = 0 ).
    cl_abap_unit_assert=>assert_false( temp23 ).
  ENDMETHOD.

  METHOD fields_stop_at_a_cycle.
    " an object that points at itself is followed three levels deep
    DATA lo_data TYPE REF TO lcl_data.
    DATA lt_field TYPE zabaputil_cl_pdf_form=>ty_t_field.
    DATA temp24 TYPE xsdboolean.
    DATA temp11 LIKE sy-subrc.
    DATA temp25 TYPE xsdboolean.
    DATA temp12 LIKE sy-subrc.
    CREATE OBJECT lo_data TYPE lcl_data.
    lo_data->other = lo_data.
    
    lt_field = zabaputil_cl_pdf_form=>fields( lo_data ).
    
    
    READ TABLE lt_field WITH KEY path = `OTHER-OTHER-SHOWN` TRANSPORTING NO FIELDS.
    temp11 = sy-subrc.
    temp24 = boolc( temp11 = 0 ).
    cl_abap_unit_assert=>assert_true( temp24 ).
    
    
    READ TABLE lt_field WITH KEY path = `OTHER-OTHER-OTHER-SHOWN` TRANSPORTING NO FIELDS.
    temp12 = sy-subrc.
    temp25 = boolc( temp12 = 0 ).
    cl_abap_unit_assert=>assert_false( temp25 ).
  ENDMETHOD.

  METHOD data_sample_of_a_class.
    " the name from a reference typed with the class: in a renamed copy of
    " this repository it is the renamed class
    DATA lo_demo TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA li_data TYPE REF TO zabaputil_if_pdf_data.
    FIELD-SYMBOLS <temp49> LIKE LINE OF lo_demo->t_items.
    DATA temp50 LIKE sy-tabix.
    FIELD-SYMBOLS <temp51> LIKE LINE OF lo_demo->t_items.
    DATA temp52 LIKE sy-tabix.
    FIELD-SYMBOLS <temp53> LIKE LINE OF lo_demo->t_items.
    DATA temp54 LIKE sy-tabix.
    li_data = zabaputil_cl_pdf_form=>data_sample( zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_demo ) ).
    cl_abap_unit_assert=>assert_bound( li_data ).
    " the class sets nothing: a text shows its name, a table has three rows
    cl_abap_unit_assert=>assert_equals( exp = `NAME in CITY`
                                        act = zabaputil_cl_pdf_form=>resolve( val  = `{NAME} in {ADDRESS-CITY}`
                                                                              data = li_data ) ).
    lo_demo ?= li_data.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lo_demo->t_items ) ).
    
    
    temp50 = sy-tabix.
    READ TABLE lo_demo->t_items INDEX 2 ASSIGNING <temp49>.
    sy-tabix = temp50.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 20 act = <temp49>-posnr ).
    
    
    temp52 = sy-tabix.
    READ TABLE lo_demo->t_items INDEX 2 ASSIGNING <temp51>.
    sy-tabix = temp52.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `MATNR 2` act = <temp51>-matnr ).
    
    
    temp54 = sy-tabix.
    READ TABLE lo_demo->t_items INDEX 3 ASSIGNING <temp53>.
    sy-tabix = temp54.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `37.50` act = |{ <temp53>-lfimg }| ).
  ENDMETHOD.

  METHOD data_sample_refused.
    " a class that is no data class, a name that is no class, no name
    DATA lt_name TYPE string_table.
    DATA lo_writer TYPE REF TO zabaputil_cl_pdf_writer.
    DATA li_data   TYPE REF TO zabaputil_if_pdf_data.
    DATA temp55 TYPE string_table.
    DATA lv_name LIKE LINE OF lt_name.
    CLEAR temp55.
    INSERT zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_writer ) INTO TABLE temp55.
    INSERT zabaputil_cl_util_context=>rtti_get_ref_type_name( li_data ) INTO TABLE temp55.
    INSERT `ZABAPUTIL_NO_SUCH_CLASS` INTO TABLE temp55.
    INSERT `` INTO TABLE temp55.
    lt_name = temp55.
    
    LOOP AT lt_name INTO lv_name.
      TRY.
          zabaputil_cl_pdf_form=>data_sample( lv_name ).
          cl_abap_unit_assert=>fail( lv_name ).
        CATCH zabaputil_cx_util_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD form_with_its_data_class.
    " what an editor does for its preview: the form names its class
    DATA lo_demo TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA temp57 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp58 LIKE LINE OF temp57.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    ms_form-data_class = zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_demo ).
    
    CLEAR temp57.
    
    temp58-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp58-text = `Order {BSTNK}`.
    INSERT temp58 INTO TABLE temp57.
    ms_form-t_element = temp57.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = zabaputil_cl_pdf_form=>data_sample( ms_form-data_class ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Order BSTNK` ) ).
  ENDMETHOD.

  METHOD items.
      DATA temp59 TYPE ltcl_form=>ty_s_item.
    CLEAR ms_data-t_items.
    DO val TIMES.
      
      CLEAR temp59.
      temp59-pos = sy-index.
      temp59-text = `Nut`.
      INSERT temp59 INTO TABLE ms_data-t_items.
    ENDDO.
  ENDMETHOD.

  METHOD absolute_text_position.
    DATA temp60 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp61 LIKE LINE OF temp60.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    " an element of an absolute form stands at its position without being
    " marked as fixed - the same line fixed_text_position expects
    ms_form-absolute = abap_true.
    
    CLEAR temp60.
    
    temp61-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp61-x = 10.
    temp61-y = 30.
    temp61-text = `No. {NUMBER}`.
    INSERT temp61 INTO TABLE temp60.
    ms_form-t_element = temp60.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals(
        exp = |BT /F1 10 Tf 28.346 749.671 Td { lo_pdf->font( `Helvetica` )->encode( `No. 4711` ) } Tj ET|
        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD absolute_has_no_flow.
    DATA temp62 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp63 LIKE LINE OF temp62.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    " a space and a page break mean nothing on a canvas
    ms_form-absolute = abap_true.
    
    CLEAR temp62.
    
    CLEAR temp63.
    temp63-type = zabaputil_cl_pdf_form=>cs_type-space.
    temp63-h = 50.
    INSERT temp63 INTO TABLE temp62.
    CLEAR temp63.
    temp63-type = zabaputil_cl_pdf_form=>cs_type-page_break.
    INSERT temp63 INTO TABLE temp62.
    CLEAR temp63.
    temp63-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp63-x = 10.
    temp63-y = 30.
    temp63-text = `Here`.
    INSERT temp63 INTO TABLE temp62.
    ms_form-t_element = temp62.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |BT /F1 10 Tf 28.346 749.671 Td { lo_pdf->font( `Helvetica` )->encode( `Here` ) } Tj ET|
        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD absolute_table_position.
    " a table 30 mm from the left and 100 mm from the top is the table of
    " a flow that has this left margin and 80 mm of space above it
    DATA temp64 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp65 LIKE LINE OF temp64.
    DATA lt_column LIKE temp64.
    DATA ls_flow LIKE ms_form.
    DATA temp66 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp67 LIKE LINE OF temp66.
    DATA lo_flow TYPE REF TO zabaputil_cl_pdf.
    DATA temp68 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp69 LIKE LINE OF temp68.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    FIELD-SYMBOLS <temp70> LIKE LINE OF ms_form-t_element.
    DATA temp71 LIKE sy-tabix.
    DATA lv_content TYPE string.
    DATA temp26 TYPE xsdboolean.
    DATA temp27 TYPE xsdboolean.
    CLEAR temp64.
    
    temp65-name = `pos`.
    temp65-title = `Pos`.
    temp65-width = 20.
    temp65-align = `R`.
    INSERT temp65 INTO TABLE temp64.
    temp65-name = `text`.
    temp65-title = `Item`.
    temp65-width = 0.
    temp65-align = `L`.
    INSERT temp65 INTO TABLE temp64.
    
    lt_column = temp64.

    
    ls_flow = ms_form.
    ls_flow-margin_left = 30.
    
    CLEAR temp66.
    
    CLEAR temp67.
    temp67-type = zabaputil_cl_pdf_form=>cs_type-space.
    temp67-h = 80.
    INSERT temp67 INTO TABLE temp66.
    CLEAR temp67.
    temp67-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp67-source = `t_items`.
    temp67-t_column = lt_column.
    INSERT temp67 INTO TABLE temp66.
    ls_flow-t_element = temp66.
    
    lo_flow = zabaputil_cl_pdf_form=>build( val  = ls_flow
                                                  data = ms_data ).

    " without a width it ends at the right margin of the form
    ms_form-absolute = abap_true.
    
    CLEAR temp68.
    
    temp69-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp69-x = 30.
    temp69-y = 100.
    temp69-h = 20.
    temp69-source = `t_items`.
    temp69-t_column = lt_column.
    INSERT temp69 INTO TABLE temp68.
    ms_form-t_element = temp68.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = lo_flow->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).

    " 100 mm wide: 20 mm for the first column, 80 mm (226.772 points) for
    " the second, which starts 50 mm (141.732 points) from the left. The
    " header row is 18.5 points high, its top edge 100 mm from the top
    
    
    temp71 = sy-tabix.
    READ TABLE ms_form-t_element INDEX 1 ASSIGNING <temp70>.
    sy-tabix = temp71.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp70>-w = 100.
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                           data = ms_data ).
    
    lv_content = lo_pdf->page( 1 )->content( ).
    
    temp26 = boolc( lv_content CS `85.039 539.925 56.693 18.5 re B` ).
    cl_abap_unit_assert=>assert_true( temp26 ).
    
    temp27 = boolc( lv_content CS `141.732 539.925 226.772 18.5 re B` ).
    cl_abap_unit_assert=>assert_true( temp27 ).
  ENDMETHOD.

  METHOD absolute_below_a_table.
    " what stands below a table keeps its distance to the end of the
    " table, however high the table was drawn on the canvas; what stands
    " beside it stays where it is
    DATA temp72 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp73 LIKE LINE OF temp72.
    DATA lt_column LIKE temp72.
    DATA temp74 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp75 LIKE LINE OF temp74.
    DATA lo_low TYPE REF TO zabaputil_cl_pdf.
    FIELD-SYMBOLS <temp76> LIKE LINE OF ms_form-t_element.
    DATA temp77 LIKE sy-tabix.
    FIELD-SYMBOLS <temp78> LIKE LINE OF ms_form-t_element.
    DATA temp79 LIKE sy-tabix.
    DATA lo_high TYPE REF TO zabaputil_cl_pdf.
    DATA temp28 TYPE xsdboolean.
    DATA lo_empty TYPE REF TO zabaputil_cl_pdf.
    CLEAR temp72.
    
    temp73-name = `pos`.
    temp73-title = `Pos`.
    temp73-width = 20.
    temp73-align = `R`.
    INSERT temp73 INTO TABLE temp72.
    temp73-name = `text`.
    temp73-title = `Item`.
    temp73-width = 0.
    temp73-align = `L`.
    INSERT temp73 INTO TABLE temp72.
    
    lt_column = temp72.

    ms_form-absolute = abap_true.
    
    CLEAR temp74.
    
    CLEAR temp75.
    temp75-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp75-x = 20.
    temp75-y = 115.
    temp75-text = `Below`.
    INSERT temp75 INTO TABLE temp74.
    CLEAR temp75.
    temp75-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp75-x = 150.
    temp75-y = 102.
    temp75-text = `Beside`.
    INSERT temp75 INTO TABLE temp74.
    CLEAR temp75.
    temp75-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp75-x = 20.
    temp75-y = 100.
    temp75-w = 100.
    temp75-h = 10.
    temp75-source = `t_items`.
    temp75-t_column = lt_column.
    INSERT temp75 INTO TABLE temp74.
    ms_form-t_element = temp74.
    
    lo_low = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).

    
    
    temp77 = sy-tabix.
    READ TABLE ms_form-t_element INDEX 1 ASSIGNING <temp76>.
    sy-tabix = temp77.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp76>-y = 135.
    
    
    temp79 = sy-tabix.
    READ TABLE ms_form-t_element INDEX 3 ASSIGNING <temp78>.
    sy-tabix = temp79.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    <temp78>-h = 30.
    
    lo_high = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                  data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = lo_low->page( 1 )->content( )
                                        act = lo_high->page( 1 )->content( ) ).

    " Beside: 150 mm from the left, the top of the line 102 mm from the top
    
    temp28 = boolc( lo_low->page( 1 )->content( ) CS |425.197 545.576 Td { lo_low->font( `Helvetica` )->encode( `Beside` ) } Tj| ).
    cl_abap_unit_assert=>assert_true(
        temp28 ).

    " without rows the table is its header row, and Below moves up with it
    CLEAR ms_data-t_items.
    
    lo_empty = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                   data = ms_data ).
    cl_abap_unit_assert=>assert_differs( exp = lo_high->page( 1 )->content( )
                                         act = lo_empty->page( 1 )->content( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_empty
                                                     page = 1
                                                     text = `Below` ) ).
  ENDMETHOD.

  METHOD absolute_table_pages.
    DATA temp80 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp81 LIKE LINE OF temp80.
    DATA temp16 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp17 LIKE LINE OF temp16.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA lv_last TYPE i.
    DATA temp29 TYPE xsdboolean.
    " a table that goes on on the next page takes what is below it along
    items( 80 ).
    ms_form-absolute = abap_true.
    
    CLEAR temp80.
    
    CLEAR temp81.
    temp81-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp81-x = 20.
    temp81-y = 30.
    temp81-text = `Above`.
    INSERT temp81 INTO TABLE temp80.
    CLEAR temp81.
    temp81-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp81-x = 20.
    temp81-y = 115.
    temp81-text = `Below`.
    INSERT temp81 INTO TABLE temp80.
    CLEAR temp81.
    temp81-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp81-x = 20.
    temp81-y = 100.
    temp81-w = 100.
    temp81-h = 10.
    temp81-source = `t_items`.
    
    CLEAR temp16.
    
    temp17-name = `pos`.
    temp17-title = `Pos`.
    temp17-width = 20.
    temp17-align = `R`.
    INSERT temp17 INTO TABLE temp16.
    temp81-t_column = temp16.
    INSERT temp81 INTO TABLE temp80.
    ms_form-t_element = temp80.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    
    lv_last = lo_pdf->page_count( ).
    
    temp29 = boolc( lv_last > 1 ).
    cl_abap_unit_assert=>assert_true( temp29 ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf = lo_pdf page = 1 text = `Above` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = shown( pdf = lo_pdf page = 1 text = `Below` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf = lo_pdf page = lv_last text = `Below` ) ).
  ENDMETHOD.

  METHOD absolute_pushed_over.
    DATA temp82 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp83 LIKE LINE OF temp82.
    DATA temp18 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp19 LIKE LINE OF temp18.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA ls_fixed TYPE zabaputil_cl_pdf_form=>ty_s_form.
    DATA temp84 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp85 LIKE LINE OF temp84.
    DATA lo_fixed TYPE REF TO zabaputil_cl_pdf.
    " an element the table pushed over the bottom margin starts the next
    " page at the top margin, and what was below it follows at its distance
    items( 10 ).
    ms_form-absolute = abap_true.
    
    CLEAR temp82.
    
    CLEAR temp83.
    temp83-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp83-x = 10.
    temp83-y = 115.
    temp83-h = 160.
    temp83-text = `Box`.
    INSERT temp83 INTO TABLE temp82.
    CLEAR temp83.
    temp83-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp83-x = 10.
    temp83-y = 125.
    temp83-text = `After`.
    INSERT temp83 INTO TABLE temp82.
    CLEAR temp83.
    temp83-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp83-x = 20.
    temp83-y = 100.
    temp83-w = 100.
    temp83-h = 10.
    temp83-source = `t_items`.
    
    CLEAR temp18.
    
    temp19-name = `pos`.
    temp19-title = `Pos`.
    temp19-width = 20.
    temp19-align = `R`.
    INSERT temp19 INTO TABLE temp18.
    temp83-t_column = temp18.
    INSERT temp83 INTO TABLE temp82.
    ms_form-t_element = temp82.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lo_pdf->page_count( ) ).

    " the second page is the page of two fixed texts, 20 and 30 mm from the top
    
    ls_fixed = zabaputil_cl_pdf_form=>new_form( ).
    
    CLEAR temp84.
    
    temp85-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp85-fixed = abap_true.
    temp85-x = 10.
    temp85-y = 20.
    temp85-text = `Box`.
    INSERT temp85 INTO TABLE temp84.
    temp85-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp85-fixed = abap_true.
    temp85-x = 10.
    temp85-y = 30.
    temp85-text = `After`.
    INSERT temp85 INTO TABLE temp84.
    ls_fixed-t_element = temp84.
    
    lo_fixed = zabaputil_cl_pdf_form=>build( ls_fixed ).
    cl_abap_unit_assert=>assert_equals( exp = lo_fixed->page( 1 )->content( )
                                        act = lo_pdf->page( 2 )->content( ) ).
  ENDMETHOD.

  METHOD absolute_every_page.
    DATA temp86 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp87 LIKE LINE OF temp86.
    DATA temp20 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp21 LIKE LINE OF temp20.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA temp30 TYPE xsdboolean.
    items( 80 ).
    ms_form-absolute = abap_true.
    
    CLEAR temp86.
    
    CLEAR temp87.
    temp87-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp87-every_page = abap_true.
    temp87-x = 150.
    temp87-y = 10.
    temp87-text = `Letterhead`.
    INSERT temp87 INTO TABLE temp86.
    CLEAR temp87.
    temp87-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp87-x = 20.
    temp87-y = 100.
    temp87-w = 100.
    temp87-h = 10.
    temp87-source = `t_items`.
    
    CLEAR temp20.
    
    temp21-name = `pos`.
    temp21-title = `Pos`.
    temp21-width = 20.
    temp21-align = `R`.
    INSERT temp21 INTO TABLE temp20.
    temp87-t_column = temp20.
    INSERT temp87 INTO TABLE temp86.
    ms_form-t_element = temp86.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    
    temp30 = boolc( lo_pdf->page_count( ) > 1 ).
    cl_abap_unit_assert=>assert_true( temp30 ).
    DO lo_pdf->page_count( ) TIMES.
      cl_abap_unit_assert=>assert_equals( exp = 1
                                          act = shown( pdf  = lo_pdf
                                                       page = sy-index
                                                       text = `Letterhead` ) ).
    ENDDO.
  ENDMETHOD.

  METHOD text_keeps_blanks.
    " blanks in a row, at the start and at the end of a text are the text's
    " own; what stands around the form is not
    DATA temp88 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp89 LIKE LINE OF temp88.
    DATA lv_text TYPE string.
    CLEAR temp88.
    
    temp89-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp89-text = `  Aisle {AISLE}    Level {LEVEL}  `.
    INSERT temp89 INTO TABLE temp88.
    ms_form-t_element = temp88.
    
    lv_text = zabaputil_cl_pdf_form=>stringify( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = ms_form
                                        act = zabaputil_cl_pdf_form=>parse( lv_text ) ).
    cl_abap_unit_assert=>assert_equals( exp = ms_form
                                        act = zabaputil_cl_pdf_form=>parse( |\n  { lv_text }\n| ) ).
  ENDMETHOD.

  METHOD to_absolute_places.
    " the flow laid out once more: each element where it began, as wide as
    " the margins leave, as high as it was; a space is the distance between
    " two, a page break begins the page again at its top margin
    DATA temp90 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp91 LIKE LINE OF temp90.
    DATA ls_absolute TYPE zabaputil_cl_pdf_form=>ty_s_form.
    DATA ls_a TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp22> LIKE LINE OF ls_absolute-t_element.
    DATA temp23 LIKE sy-tabix.
    DATA ls_b TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp24> LIKE LINE OF ls_absolute-t_element.
    DATA temp25 LIKE sy-tabix.
    DATA ls_rule TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp26> LIKE LINE OF ls_absolute-t_element.
    DATA temp27 LIKE sy-tabix.
    DATA ls_box TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp28> LIKE LINE OF ls_absolute-t_element.
    DATA temp29 LIKE sy-tabix.
    DATA ls_c TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp30> LIKE LINE OF ls_absolute-t_element.
    DATA temp31 LIKE sy-tabix.
    DATA temp32 TYPE xsdboolean.
    CLEAR temp90.
    
    CLEAR temp91.
    temp91-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp91-name = `a`.
    temp91-text = `a`.
    INSERT temp91 INTO TABLE temp90.
    CLEAR temp91.
    temp91-type = zabaputil_cl_pdf_form=>cs_type-space.
    temp91-h = 30.
    INSERT temp91 INTO TABLE temp90.
    CLEAR temp91.
    temp91-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp91-name = `b`.
    temp91-text = `b`.
    temp91-align = `R`.
    INSERT temp91 INTO TABLE temp90.
    CLEAR temp91.
    temp91-type = zabaputil_cl_pdf_form=>cs_type-line.
    temp91-name = `rule`.
    INSERT temp91 INTO TABLE temp90.
    CLEAR temp91.
    temp91-type = zabaputil_cl_pdf_form=>cs_type-rect.
    temp91-name = `box`.
    temp91-x = 5.
    temp91-y = 6.
    temp91-w = 7.
    temp91-h = 8.
    INSERT temp91 INTO TABLE temp90.
    CLEAR temp91.
    temp91-type = zabaputil_cl_pdf_form=>cs_type-page_break.
    INSERT temp91 INTO TABLE temp90.
    CLEAR temp91.
    temp91-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp91-name = `c`.
    temp91-text = `c`.
    INSERT temp91 INTO TABLE temp90.
    ms_form-t_element = temp90.
    
    ls_absolute = zabaputil_cl_pdf_form=>to_absolute( ms_form ).

    cl_abap_unit_assert=>assert_true( ls_absolute-absolute ).
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( ls_absolute-t_element ) ).
    
    
    
    temp23 = sy-tabix.
    READ TABLE ls_absolute-t_element INDEX 1 ASSIGNING <temp22>.
    sy-tabix = temp23.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_a = <temp22>.
    
    
    
    temp25 = sy-tabix.
    READ TABLE ls_absolute-t_element INDEX 2 ASSIGNING <temp24>.
    sy-tabix = temp25.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_b = <temp24>.
    
    
    
    temp27 = sy-tabix.
    READ TABLE ls_absolute-t_element INDEX 3 ASSIGNING <temp26>.
    sy-tabix = temp27.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_rule = <temp26>.
    
    
    
    temp29 = sy-tabix.
    READ TABLE ls_absolute-t_element INDEX 4 ASSIGNING <temp28>.
    sy-tabix = temp29.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_box = <temp28>.
    
    
    
    temp31 = sy-tabix.
    READ TABLE ls_absolute-t_element INDEX 5 ASSIGNING <temp30>.
    sy-tabix = temp31.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_c = <temp30>.

    " one line of 10 points is 12.5 points high: 4.41 mm
    cl_abap_unit_assert=>assert_equals( exp = `a 20.00 20.00 170.00 4.41`
                                        act = |{ ls_a-name } { ls_a-x } { ls_a-y } { ls_a-w } { ls_a-h }| ).
    cl_abap_unit_assert=>assert_equals( exp = `b 20.00 54.41 170.00 4.41 R`
                                        act = |{ ls_b-name } { ls_b-x } { ls_b-y } { ls_b-w } { ls_b-h } { ls_b-align }| ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = ls_b-fixed ).
    " a line of no height across the margins, below b
    
    temp32 = boolc( ls_rule-y > ls_b-y + ls_b-h AND ls_rule-y < ls_b-y + ls_b-h + 5 ).
    cl_abap_unit_assert=>assert_true( temp32 ).
    cl_abap_unit_assert=>assert_equals( exp = `rule 20.00 170.00 0.00`
                                        act = |{ ls_rule-name } { ls_rule-x } { ls_rule-w } { ls_rule-h }| ).
    " what had a position keeps it
    cl_abap_unit_assert=>assert_equals( exp = `box 5.00 6.00 7.00 8.00`
                                        act = |{ ls_box-name } { ls_box-x } { ls_box-y } { ls_box-w } { ls_box-h }| ).
    " c is the first line of page 2
    cl_abap_unit_assert=>assert_equals( exp = `c 20.00 20.00`
                                        act = |{ ls_c-name } { ls_c-x } { ls_c-y }| ).
  ENDMETHOD.

  METHOD to_absolute_with_data.
    " a table is as high as its rows with the data, the text below follows
    DATA temp92 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp93 LIKE LINE OF temp92.
    DATA temp32 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp33 LIKE LINE OF temp32.
    DATA ls_one TYPE zabaputil_cl_pdf_form=>ty_s_form.
    DATA ls_more LIKE ms_data.
    DATA ls_two TYPE zabaputil_cl_pdf_form=>ty_s_form.
    DATA ls_table TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp34> LIKE LINE OF ls_one-t_element.
    DATA temp35 LIKE sy-tabix.
    DATA ls_below TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp36> LIKE LINE OF ls_one-t_element.
    DATA temp37 LIKE sy-tabix.
    DATA temp34 TYPE xsdboolean.
    FIELD-SYMBOLS <temp94> LIKE LINE OF ls_two-t_element.
    DATA temp95 LIKE sy-tabix.
    DATA temp36 TYPE xsdboolean.
    CLEAR temp92.
    
    CLEAR temp93.
    temp93-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp93-name = `items`.
    temp93-source = `t_items`.
    
    CLEAR temp32.
    
    temp33-name = `pos`.
    temp33-title = `Pos`.
    temp33-width = 20.
    temp33-align = `R`.
    INSERT temp33 INTO TABLE temp32.
    temp33-name = `text`.
    temp33-title = `Item`.
    temp33-width = 0.
    temp33-align = `L`.
    INSERT temp33 INTO TABLE temp32.
    temp93-t_column = temp32.
    INSERT temp93 INTO TABLE temp92.
    CLEAR temp93.
    temp93-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp93-name = `below`.
    temp93-text = `Total {NUMBER}`.
    INSERT temp93 INTO TABLE temp92.
    ms_form-t_element = temp92.

    
    ls_one = zabaputil_cl_pdf_form=>to_absolute( val  = ms_form
                                                       data = ms_data ).
    
    ls_more = ms_data.
    INSERT LINES OF ms_data-t_items INTO TABLE ls_more-t_items.
    
    ls_two = zabaputil_cl_pdf_form=>to_absolute( val  = ms_form
                                                       data = ls_more ).

    
    
    
    temp35 = sy-tabix.
    READ TABLE ls_one-t_element INDEX 1 ASSIGNING <temp34>.
    sy-tabix = temp35.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_table = <temp34>.
    
    
    
    temp37 = sy-tabix.
    READ TABLE ls_one-t_element INDEX 2 ASSIGNING <temp36>.
    sy-tabix = temp37.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_below = <temp36>.
    cl_abap_unit_assert=>assert_equals( exp = `items T_ITEMS`
                                        act = |{ ls_table-name } { to_upper( ls_table-source ) }| ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( ls_table-t_column ) ).
    
    temp34 = boolc( ls_table-h > 0 ).
    cl_abap_unit_assert=>assert_true( temp34 ).
    cl_abap_unit_assert=>assert_equals( exp = ls_table-y + ls_table-h
                                        act = ls_below-y ).
    " twice the rows: a higher table
    
    
    temp95 = sy-tabix.
    READ TABLE ls_two-t_element INDEX 1 ASSIGNING <temp94>.
    sy-tabix = temp95.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    temp36 = boolc( <temp94>-h > ls_table-h ).
    cl_abap_unit_assert=>assert_true( temp36 ).
  ENDMETHOD.

  METHOD to_absolute_unchanged.
    DATA temp96 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp97 LIKE LINE OF temp96.
    ms_form-absolute = abap_true.
    
    CLEAR temp96.
    
    temp97-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp97-x = 10.
    temp97-y = 30.
    temp97-text = `Here`.
    INSERT temp97 INTO TABLE temp96.
    ms_form-t_element = temp96.
    cl_abap_unit_assert=>assert_equals( exp = ms_form
                                        act = zabaputil_cl_pdf_form=>to_absolute( ms_form ) ).
  ENDMETHOD.

  METHOD barcode_fixed.
    " the bars of the resolved text, 40 mm wide, and the text below them
    DATA temp98 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp99 LIKE LINE OF temp98.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    DATA lv_content TYPE string.
    DATA temp37 TYPE xsdboolean.
    CLEAR temp98.
    
    temp99-type = zabaputil_cl_pdf_form=>cs_type-barcode.
    temp99-fixed = abap_true.
    temp99-x = 20.
    temp99-y = 30.
    temp99-w = 40.
    temp99-h = 20.
    temp99-size = 8.
    temp99-text = `{NUMBER}`.
    INSERT temp99 INTO TABLE temp98.
    ms_form-t_element = temp98.
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    
    lv_content = lo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals( exp = 16
                                        act = count( val = lv_content
                                                     sub = ` re f` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `4711` ) ).
    " the first bar at the left edge: two of 57 modules of 40 mm
    
    temp37 = boolc( lv_content CS `56.693 ` ).
    cl_abap_unit_assert=>assert_true( temp37 ).
  ENDMETHOD.

  METHOD barcode_in_the_flow.
    " 0.33 mm a module without a width, 15 mm high without a height: the
    " text behind it starts 15 mm lower
    DATA temp100 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp101 LIKE LINE OF temp100.
    DATA ls_absolute TYPE zabaputil_cl_pdf_form=>ty_s_form.
    DATA ls_bar TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp38> LIKE LINE OF ls_absolute-t_element.
    DATA temp39 LIKE sy-tabix.
    FIELD-SYMBOLS <temp102> LIKE LINE OF ls_absolute-t_element.
    DATA temp103 LIKE sy-tabix.
    CLEAR temp100.
    
    CLEAR temp101.
    temp101-type = zabaputil_cl_pdf_form=>cs_type-barcode.
    temp101-text = `4711`.
    INSERT temp101 INTO TABLE temp100.
    CLEAR temp101.
    temp101-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp101-name = `after`.
    temp101-text = `after`.
    INSERT temp101 INTO TABLE temp100.
    ms_form-t_element = temp100.
    
    ls_absolute = zabaputil_cl_pdf_form=>to_absolute( ms_form ).
    
    
    
    temp39 = sy-tabix.
    READ TABLE ls_absolute-t_element INDEX 1 ASSIGNING <temp38>.
    sy-tabix = temp39.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_bar = <temp38>.
    cl_abap_unit_assert=>assert_equals( exp = `20.00 20.00 18.81 15.00`
                                        act = |{ ls_bar-x } { ls_bar-y } { ls_bar-w } { ls_bar-h }| ).
    
    
    temp103 = sy-tabix.
    READ TABLE ls_absolute-t_element INDEX 2 ASSIGNING <temp102>.
    sy-tabix = temp103.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `35.00`
                                        act = |{ <temp102>-y }| ).
    cl_abap_unit_assert=>assert_equals(
        exp = 16
        act = count( val = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( )
                     sub = ` re f` ) ).
  ENDMETHOD.

  METHOD barcode_absolute.
    DATA temp104 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp105 LIKE LINE OF temp104.
    DATA lv_content TYPE string.
    DATA temp38 TYPE xsdboolean.
    ms_form-absolute = abap_true.
    
    CLEAR temp104.
    
    temp105-type = zabaputil_cl_pdf_form=>cs_type-barcode.
    temp105-x = 100.
    temp105-y = 50.
    temp105-h = 10.
    temp105-text = `(00)340123450000000018`.
    INSERT temp105 INTO TABLE temp104.
    ms_form-t_element = temp104.
    
    lv_content = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ).
    
    temp38 = boolc( count( val = lv_content sub = ` re f` ) > 20 ).
    cl_abap_unit_assert=>assert_true( temp38 ).
  ENDMETHOD.

  METHOD sample_by_type.
    TYPES:
      BEGIN OF ty_s_inner,
        text TYPE string,
      END OF ty_s_inner.
    TYPES:
      BEGIN OF ty_s_row,
        key  TYPE i,
        text TYPE c LENGTH 10,
      END OF ty_s_row.
    TYPES:
      BEGIN OF ty_s_sample,
        name     TYPE string,
        code     TYPE c LENGTH 3,
        number   TYPE n LENGTH 4,
        day      TYPE d,
        time     TYPE t,
        count    TYPE i,
        amount   TYPE p LENGTH 8 DECIMALS 2,
        digit    TYPE p LENGTH 1 DECIMALS 0,
        bytes    TYPE xstring,
        inner    TYPE ty_s_inner,
        t_text   TYPE string_table,
        t_sorted TYPE SORTED TABLE OF ty_s_row WITH UNIQUE KEY key,
      END OF ty_s_sample.
    DATA ls_sample TYPE ty_s_sample.
    DATA temp106 TYPE string_table.
    FIELD-SYMBOLS <temp108> LIKE LINE OF ls_sample-t_sorted.
    DATA temp109 LIKE sy-tabix.

    zabaputil_cl_pdf_form=>sample( CHANGING data = ls_sample ).
    cl_abap_unit_assert=>assert_equals( exp = `NAME` act = ls_sample-name ).
    " a short field takes what fits of its name
    cl_abap_unit_assert=>assert_equals( exp = `COD` act = ls_sample-code ).
    cl_abap_unit_assert=>assert_equals( exp = `0001` act = ls_sample-number ).
    cl_abap_unit_assert=>assert_equals( exp = `20260131` act = ls_sample-day ).
    cl_abap_unit_assert=>assert_equals( exp = `120000` act = ls_sample-time ).
    cl_abap_unit_assert=>assert_equals( exp = 10 act = ls_sample-count ).
    cl_abap_unit_assert=>assert_equals( exp = `12.50` act = |{ ls_sample-amount }| ).
    " 12.5 does not fit one digit: the field stays empty rather than wrong
    cl_abap_unit_assert=>assert_initial( ls_sample-digit ).
    cl_abap_unit_assert=>assert_initial( ls_sample-bytes ).
    cl_abap_unit_assert=>assert_equals( exp = `TEXT` act = ls_sample-inner-text ).
    " a table: three rows, each field with the number of its row
    
    CLEAR temp106.
    INSERT `T_TEXT 1` INTO TABLE temp106.
    INSERT `T_TEXT 2` INTO TABLE temp106.
    INSERT `T_TEXT 3` INTO TABLE temp106.
    cl_abap_unit_assert=>assert_equals( exp = temp106
                                        act = ls_sample-t_text ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_sample-t_sorted ) ).
    
    
    temp109 = sy-tabix.
    READ TABLE ls_sample-t_sorted WITH KEY key = 20 ASSIGNING <temp108>.
    sy-tabix = temp109.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `TEXT 2` act = <temp108>-text ).
  ENDMETHOD.

  METHOD sample_keeps_what_is_set.
    " an object: its public attributes, and only the ones that are initial
    DATA lo_data TYPE REF TO lcl_data.
    CREATE OBJECT lo_data TYPE lcl_data.
    zabaputil_cl_pdf_form=>sample( CHANGING data = lo_data ).
    cl_abap_unit_assert=>assert_equals( exp = `visible` act = lo_data->shown ).
    cl_abap_unit_assert=>assert_equals( exp = `Bonn` act = lo_data->head-city ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lo_data->t_rows ) ).
    cl_abap_unit_assert=>assert_not_bound( lo_data->other ).
    cl_abap_unit_assert=>assert_equals( exp = `{HIDDEN}`
                                        act = zabaputil_cl_pdf_form=>resolve( val  = `{HIDDEN}`
                                                                              data = lo_data ) ).
  ENDMETHOD.

  METHOD text_of_a_form.
    DATA temp110 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp111 LIKE LINE OF temp110.
    DATA temp40 TYPE zabaputil_cl_pdf_form=>ty_t_column.
    DATA temp41 LIKE LINE OF temp40.
    DATA lv_text TYPE string.
    DATA temp39 TYPE xsdboolean.
    " a form comes back from its text as it went in - with everything a
    " text can hold that XML or an ABAP literal has a meaning for
    ms_form-name = `NOTE`.
    ms_form-absolute = abap_true.
    ms_form-margin_left = `12.5`.
    
    CLEAR temp110.
    
    CLEAR temp111.
    temp111-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp111-name = `Odd`.
    temp111-x = 20.
    temp111-y = `95.25`.
    temp111-size = 16.
    temp111-text = |a < b & c > d, <FILL/> "quoted" `tick` 'single' \{X\}\nsecond line\r\n\ttab, &#10; end|.
    INSERT temp111 INTO TABLE temp110.
    CLEAR temp111.
    temp111-type = zabaputil_cl_pdf_form=>cs_type-table.
    temp111-source = `T_ITEMS`.
    temp111-every_page = abap_true.
    
    CLEAR temp40.
    
    CLEAR temp41.
    temp41-name = `POS`.
    temp41-title = `Pos`.
    temp41-width = 20.
    temp41-align = `R`.
    INSERT temp41 INTO TABLE temp40.
    CLEAR temp41.
    temp41-name = `TEXT`.
    INSERT temp41 INTO TABLE temp40.
    temp111-t_column = temp40.
    INSERT temp111 INTO TABLE temp110.
    CLEAR temp111.
    temp111-type = zabaputil_cl_pdf_form=>cs_type-image.
    temp111-image = image_rgb( ).
    INSERT temp111 INTO TABLE temp110.
    CLEAR temp111.
    temp111-type = zabaputil_cl_pdf_form=>cs_type-page_break.
    INSERT temp111 INTO TABLE temp110.
    ms_form-t_element = temp110.
    
    lv_text = zabaputil_cl_pdf_form=>stringify( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = ms_form
                                        act = zabaputil_cl_pdf_form=>parse( lv_text ) ).
    " one line of printable characters: it is kept in ABAP literals
    
    temp39 = boolc( lv_text CA |\n\r\t| ).
    cl_abap_unit_assert=>assert_false( temp39 ).
  ENDMETHOD.

  METHOD text_without_initial.
    " what is initial is not written: the text is kept in a source code
    DATA temp112 TYPE zabaputil_cl_pdf_form=>ty_t_element.
    DATA temp113 LIKE LINE OF temp112.
    DATA lv_text TYPE string.
    DATA temp40 TYPE xsdboolean.
    DATA temp41 TYPE xsdboolean.
    DATA temp42 TYPE xsdboolean.
    DATA temp43 TYPE xsdboolean.
    DATA temp44 TYPE xsdboolean.
    DATA temp45 TYPE xsdboolean.
    CLEAR temp112.
    
    temp113-type = zabaputil_cl_pdf_form=>cs_type-text.
    temp113-text = `Hello`.
    INSERT temp113 INTO TABLE temp112.
    ms_form-t_element = temp112.
    
    lv_text = zabaputil_cl_pdf_form=>stringify( ms_form ).
    
    temp40 = boolc( lv_text CS `<TEXT>Hello</TEXT>` ).
    cl_abap_unit_assert=>assert_true( temp40 ).
    
    temp41 = boolc( lv_text CS `<FORMAT>A4</FORMAT>` ).
    cl_abap_unit_assert=>assert_true( temp41 ).
    
    temp42 = boolc( lv_text CS `<FILL` ).
    cl_abap_unit_assert=>assert_false( temp42 ).
    
    temp43 = boolc( lv_text CS `<T_COLUMN` ).
    cl_abap_unit_assert=>assert_false( temp43 ).
    
    temp44 = boolc( lv_text CS `<HEADER` ).
    cl_abap_unit_assert=>assert_false( temp44 ).
    
    temp45 = boolc( lv_text CS `/>` ).
    cl_abap_unit_assert=>assert_false( temp45 ).
    " the whole text: the page, and the one thing the element has
    cl_abap_unit_assert=>assert_equals(
        exp = `<form version="1"><FORMAT>A4</FORMAT><ORIENTATION>P</ORIENTATION>` &&
              `<MARGIN_LEFT>20.00</MARGIN_LEFT><MARGIN_TOP>20.00</MARGIN_TOP>` &&
              `<MARGIN_RIGHT>20.00</MARGIN_RIGHT><MARGIN_BOTTOM>20.00</MARGIN_BOTTOM>` &&
              `<T_ELEMENT><item><TYPE>TEXT</TYPE><TEXT>Hello</TEXT></item></T_ELEMENT></form>`
        act = lv_text ).
  ENDMETHOD.

  METHOD text_read_tolerantly.
    " written by hand, or by another release: line breaks between the
    " elements, an empty element, a component and a whole table the form
    " does not have, a row without anything
    DATA lv_text TYPE string.
    DATA ls_form TYPE zabaputil_cl_pdf_form=>ty_s_form.
    FIELD-SYMBOLS <temp114> LIKE LINE OF ls_form-t_element.
    DATA temp115 LIKE sy-tabix.
    FIELD-SYMBOLS <temp116> LIKE LINE OF ls_form-t_element.
    DATA temp117 LIKE sy-tabix.
    FIELD-SYMBOLS <temp118> LIKE LINE OF ls_form-t_element.
    DATA temp119 LIKE sy-tabix.
    DATA temp46 TYPE xsdboolean.
    lv_text = |<form version="9">\n  <FORMAT>A5</FORMAT>\n  <HEADER/>\n  <LATER>new</LATER>\n| &&
                    |  <T_LATER><item><A>1</A><T_B><item>2</item></T_B></item></T_LATER>\n| &&
                    |  <MARGIN_TOP>-7.5</MARGIN_TOP>\n| &&
                    |  <T_ELEMENT>\n    <item><TYPE>TEXT</TYPE><TEXT>a &lt; b &amp;amp; c</TEXT><X>12</X></item>\n| &&
                    |    <item/>\n    <item></item>\n  </T_ELEMENT>\n</form>\n|.
    
    ls_form = zabaputil_cl_pdf_form=>parse( lv_text ).
    cl_abap_unit_assert=>assert_equals( exp = `A5` act = ls_form-format ).
    cl_abap_unit_assert=>assert_initial( ls_form-header ).
    cl_abap_unit_assert=>assert_equals( exp = `-7.50` act = |{ ls_form-margin_top }| ).
    " what the text does not name is initial - not the 20 mm of a new form
    cl_abap_unit_assert=>assert_initial( ls_form-margin_left ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_form-t_element ) ).
    
    
    temp115 = sy-tabix.
    READ TABLE ls_form-t_element INDEX 1 ASSIGNING <temp114>.
    sy-tabix = temp115.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a < b &amp; c` act = <temp114>-text ).
    
    
    temp117 = sy-tabix.
    READ TABLE ls_form-t_element INDEX 1 ASSIGNING <temp116>.
    sy-tabix = temp117.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 12 act = <temp116>-x ).
    
    
    temp119 = sy-tabix.
    READ TABLE ls_form-t_element INDEX 3 ASSIGNING <temp118>.
    sy-tabix = temp119.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp118> ).
    " a negative number is written with its sign in front, and read again
    
    temp46 = boolc( zabaputil_cl_pdf_form=>stringify( ls_form ) CS `<MARGIN_TOP>-7.50</MARGIN_TOP>` ).
    cl_abap_unit_assert=>assert_true(
        temp46 ).
  ENDMETHOD.

  METHOD text_that_is_none.
    DATA temp120 TYPE string_table.
    DATA lt_text LIKE temp120.
    DATA lv_text LIKE LINE OF lt_text.
    " no text is a form to start on
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_form=>new_form( )
                                        act = zabaputil_cl_pdf_form=>parse( `` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_form=>new_form( )
                                        act = zabaputil_cl_pdf_form=>parse( `   ` ) ).
    " no XML, another document, one that ends too early, a tag left open,
    " a text where a number belongs
    
    CLEAR temp120.
    INSERT `this is no form` INTO TABLE temp120.
    INSERT `<asx:abap><FORMAT>A4</FORMAT></asx:abap>` INTO TABLE temp120.
    INSERT `<form><FORMAT>A4` INTO TABLE temp120.
    INSERT `<form><FORMAT>A4</FORMAT` INTO TABLE temp120.
    INSERT `<form><T_ELEMENT><item><TYPE>TEXT</TYPE>` INTO TABLE temp120.
    INSERT `<form><MARGIN_LEFT>wide</MARGIN_LEFT></form>` INTO TABLE temp120.
    
    lt_text = temp120.
    
    LOOP AT lt_text INTO lv_text.
      TRY.
          zabaputil_cl_pdf_form=>parse( lv_text ).
          cl_abap_unit_assert=>fail( lv_text ).
        CATCH zabaputil_cx_util_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD form_of_a_data_class.
    " the demo carries a form: a delivery note with a table of five columns
    DATA lo_name TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA lo_demo TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA ls_form TYPE zabaputil_cl_pdf_form=>ty_s_form.
    DATA ls_table TYPE zabaputil_cl_pdf_form=>ty_s_element.
    FIELD-SYMBOLS <temp42> LIKE LINE OF ls_form-t_element.
    DATA temp43 LIKE sy-tabix.
    DATA li_none TYPE REF TO zabaputil_if_pdf_data.
    CREATE OBJECT lo_demo TYPE zabaputil_cl_pdf_data_demo.
    
    ls_form = zabaputil_cl_pdf_form=>form_of( lo_demo ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_name )
                                        act = ls_form-data_class ).
    cl_abap_unit_assert=>assert_equals( exp = `A4` act = ls_form-format ).
    cl_abap_unit_assert=>assert_equals( exp = 11 act = lines( ls_form-t_element ) ).
    
    
    
    temp43 = sy-tabix.
    READ TABLE ls_form-t_element WITH KEY type = zabaputil_cl_pdf_form=>cs_type-table ASSIGNING <temp42>.
    sy-tabix = temp43.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_table = <temp42>.
    cl_abap_unit_assert=>assert_equals( exp = `T_ITEMS` act = ls_table-source ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = lines( ls_table-t_column ) ).
    " what the class returns is what stringify( ) writes of it
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_form=>parse( lo_demo->zabaputil_if_pdf_data~get( ) )
                                        act = zabaputil_cl_pdf_form=>parse(
                                                  zabaputil_cl_pdf_form=>stringify(
                                                      zabaputil_cl_pdf_form=>parse(
                                                          lo_demo->zabaputil_if_pdf_data~get( ) ) ) ) ).

    
    TRY.
        zabaputil_cl_pdf_form=>form_of( li_none ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD print_a_data_class.
    " one object is the whole form: its attributes and its layout
    DATA lo_demo TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA lv_file TYPE xstring.
    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.
    CREATE OBJECT lo_demo TYPE zabaputil_cl_pdf_data_demo.
    lo_demo->vbeln = `80004711`.
    
    lv_file = zabaputil_cl_pdf_form=>print( lo_demo ).
    cl_abap_unit_assert=>assert_equals( exp = `255044462D` act = |{ lv_file(5) }| ).
    
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = zabaputil_cl_pdf_form=>form_of( lo_demo )
                                                 data = lo_demo ).
    " the header line of the page and the headline
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Delivery note 80004711` ) ).
  ENDMETHOD.

ENDCLASS.

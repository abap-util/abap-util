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
        t_items TYPE STANDARD TABLE OF ty_s_item WITH EMPTY KEY,
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
    " A4 with 20 mm margins: the flow starts 20 mm from the top and from
    " the left, in Helvetica at 10 points
    ms_form = zabaputil_cl_pdf_form=>new_form( ).
    ms_data-name = `Ada`.
    ms_data-number = 4711.
    ms_data-head-city = `Bonn`.
    ms_data-t_items = VALUE #( ( pos = 10 text = `Nut` )
                               ( pos = 20 text = `Bolt` ) ).
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
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_initial( lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD page_format.
    ms_form-format = `A5`.
    ms_form-orientation = `L`.
    DATA(lo_page) = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = zabaputil_cl_pdf_writer=>num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `419.53` act = zabaputil_cl_pdf_writer=>num( lo_page->height_pt( ) ) ).

    " a form that names no format is A4 portrait
    CLEAR ms_form-format.
    CLEAR ms_form-orientation.
    lo_page = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = zabaputil_cl_pdf_writer=>num( lo_page->width_pt( ) ) ).
  ENDMETHOD.

  METHOD flow_text.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-text
                                   text = `Hi`
                                   font = `Courier`
                                   size = 12
                                   color = `FF0000` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    " 20 mm are 56.693 points; the line is 15 high, the baseline of
    " Courier at 12 points 10.332 below its top
    cl_abap_unit_assert=>assert_equals( exp = `q 1 0 0 rg BT /F2 12 Tf 56.693 774.865 Td <4869> Tj ET Q`
                                        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD flow_text_placeholders.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-text
                                   text = `Dear {NAME}` ) ).
    " with data the component is printed, without it the placeholder
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
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
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-text       text = `a` h = 0 )
                                 ( type = zabaputil_cl_pdf_form=>cs_type-space      text = ``  h = 30 )
                                 ( type = zabaputil_cl_pdf_form=>cs_type-text       text = `b` h = 0 )
                                 ( type = zabaputil_cl_pdf_form=>cs_type-page_break text = ``  h = 0 )
                                 ( type = zabaputil_cl_pdf_form=>cs_type-text       text = `c` h = 0 ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_pdf->page_count( ) ).
    " a starts at 20 mm, its baseline 8.805 points below the top of its
    " line; b one line (12.5 points) and 30 mm (85.039) further down
    DATA(lv_content) = lo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 10 Tf 56.693 776.392 Td <61> Tj ET` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 10 Tf 56.693 678.853 Td <62> Tj ET` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 10 Tf 56.693 776.392 Td <63> Tj ET`
                                        act = lo_pdf->page( 2 )->content( ) ).
  ENDMETHOD.

  METHOD flow_rule.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-line ) ).
    DATA(lv_content) = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ).
    " from margin to margin
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `56.693 778.947 m 538.587 778.947 l S` ) ).
  ENDMETHOD.

  METHOD flow_table_rows.
    ms_form-t_element = VALUE #(
        ( type     = zabaputil_cl_pdf_form=>cs_type-table
          source   = `t_items`
          t_column = VALUE #( ( name = `pos`  title = `Pos`  width = 20 align = `R` )
                              ( name = `text` title = `Item` width = 0  align = `L` ) ) ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
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
    DATA(lv_content) = lo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_content CS |{ lo_pdf->font( `Helvetica-Bold` )->encode( `Item` ) } Tj| ) ).
    " header, two rows, two columns
    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = count( val = lv_content
                                                     sub = ` Tj` ) ).
  ENDMETHOD.

  METHOD flow_table_sample_rows.
    ms_form-t_element = VALUE #(
        ( type     = zabaputil_cl_pdf_form=>cs_type-table
          source   = `t_items`
          t_column = VALUE #( ( name = `pos`  title = `Pos`  width = 20 align = `R` )
                              ( name = `text` title = `Item` width = 0  align = `L` ) ) ) ).
    " printed without data: three rows that name the components
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `{TEXT}` ) ).
    " and with data that has no such table
    ms_form-t_element[ 1 ]-source = `none`.
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                           data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `{POS}` ) ).
    " a component that is no table
    ms_form-t_element[ 1 ]-source = `name`.
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                           data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `{POS}` ) ).
  ENDMETHOD.

  METHOD flow_table_no_columns.
    ms_form-t_element = VALUE #( ( type   = zabaputil_cl_pdf_form=>cs_type-table
                                   source = `t_items` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_initial( lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD fixed_text_position.
    " y is the top of the line: the baseline of Helvetica at 10 points is
    " 7.18 points below it. 10 mm from the left, 30 mm from the top
    ms_form-t_element = VALUE #( ( type  = zabaputil_cl_pdf_form=>cs_type-text
                                   fixed = abap_true
                                   x     = 10
                                   y     = 30
                                   text  = `No. {NUMBER}` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals(
        exp = |BT /F1 10 Tf 28.346 749.671 Td { lo_pdf->font( `Helvetica` )->encode( `No. 4711` ) } Tj ET|
        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD fixed_text_wraps.
    " 20 mm are 56.693 points: `aaaa bbbb` in Courier at 10 points is 54
    " wide and fits, the third word does not. Right aligned in the box
    ms_form-t_element = VALUE #( ( type  = zabaputil_cl_pdf_form=>cs_type-text
                                   fixed = abap_true
                                   x     = 10
                                   y     = 30
                                   w     = 20
                                   font  = `Courier`
                                   align = `R`
                                   text  = |aaaa bbbb cccc\nd| ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    DATA(lv_content) = lo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = count( val = lv_content
                                                     sub = ` Tj` ) ).
    " the box ends at 30 mm = 85.039 points; the lines are 12.5 points apart
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F2 10 Tf 31.039 750.561 Td <616161612062626262> Tj ET` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F2 10 Tf 61.039 738.061 Td <63636363> Tj ET` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F2 10 Tf 79.039 725.561 Td <64> Tj ET` ) ).
  ENDMETHOD.

  METHOD fixed_first_or_every.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-page_break fixed = abap_false every_page = abap_false text = `` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text       fixed = abap_true  every_page = abap_false text = `first` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text       fixed = abap_true  every_page = abap_true  text = `every` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
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
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-line fixed = abap_true x = 10 y = 10 w = 100 h = 0  size = 2 color = `FF0000` fill = `` )
        ( type = zabaputil_cl_pdf_form=>cs_type-rect fixed = abap_false x = 10 y = 10 w = 100 h = 20 size = 0 color = ``       fill = `` )
        ( type = zabaputil_cl_pdf_form=>cs_type-rect fixed = abap_true  x = 10 y = 10 w = 100 h = 20 size = 0 color = ``       fill = `0000FF` ) ).
    DATA(lv_content) = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals(
        exp = |1 0 0 RG\n2 w\n28.346 813.544 m 311.811 813.544 l S\n| &&
              |0 0 0 RG\n0.5 w\n28.346 756.851 283.465 56.693 re S\n| &&
              |0 0 0 RG\n0.5 w\n0 0 1 rg\n28.346 756.851 283.465 56.693 re B|
        act = lv_content ).
  ENDMETHOD.

  METHOD image_of_the_form.
    " fixed: 40 mm wide, the height follows from 2 x 2 pixels. In the
    " flow: at the left margin, and the text after it starts below it
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-image fixed = abap_true  x = 10 y = 10 w = 40 text = ``  image = image_rgb( ) )
        ( type = zabaputil_cl_pdf_form=>cs_type-image fixed = abap_false x = 0  y = 0  w = 10 text = ``  image = image_rgb( ) )
        ( type = zabaputil_cl_pdf_form=>cs_type-text  fixed = abap_false x = 0  y = 0  w = 0  text = `a` image = `` ) ).
    DATA(lv_content) = zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ).
    " the flow is written first, so its image is the first one of the file;
    " 10 mm of image move the text below it down by 28.346 points
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `q 28.346 0 0 28.346 56.693 756.851 cm /I1 Do Q` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `BT /F1 10 Tf 56.693 748.046 Td <61> Tj ET` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `q 113.386 0 0 113.386 28.346 700.158 cm /I2 Do Q` ) ).

    " an element without an image prints nothing
    ms_form-t_element = VALUE #( ( type  = zabaputil_cl_pdf_form=>cs_type-image
                                   fixed = abap_true ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD image_from_data.
    ms_data-logo = image_rgb( ).
    ms_form-t_element = VALUE #( ( type   = zabaputil_cl_pdf_form=>cs_type-image
                                   fixed  = abap_true
                                   x      = 10
                                   y      = 10
                                   w      = 40
                                   source = `logo` ) ).
    DATA(lv_content) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                     data = ms_data )->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals( exp = `q 113.386 0 0 113.386 28.346 700.158 cm /I1 Do Q`
                                        act = lv_content ).
    " without data there is nothing to show
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>build( ms_form )->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD header_and_footer.
    ms_form-header = `Order {NUMBER}`.
    ms_form-footer = `{page}/{pages}`.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-page_break ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
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
    ms_form-title = `Ti`.
    DATA lv_hex TYPE string.
    lv_hex = zabaputil_cl_pdf_form=>render( ms_form ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/Title <FEFF00540069>` ) ) ).
  ENDMETHOD.

  METHOD render_file.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-text
                                   text = `Dear {NAME}` ) ).
    DATA lv_hex TYPE string.
    lv_hex = zabaputil_cl_pdf_form=>render( val  = ms_form
                                            data = ms_data ).
    " %PDF-1.4 at the start, %%EOF and a line feed at the end
    cl_abap_unit_assert=>assert_equals( exp = `255044462D312E34`
                                        act = substring( val = lv_hex
                                                         len = 16 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2525454F460A`
                                        act = substring( val = lv_hex
                                                         off = strlen( lv_hex ) - 12 ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( xstrlen( zabaputil_cl_pdf_form=>render( ms_form ) ) > 300 ) ).
  ENDMETHOD.

  METHOD unknown_type_is_skipped.
    ms_form-t_element = VALUE #( ( type = `BARCODE` fixed = abap_false text = `x` )
                                 ( type = `BARCODE` fixed = abap_true  text = `x` )
                                 ( type = ``        fixed = abap_false text = `x` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_initial( lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

ENDCLASS.

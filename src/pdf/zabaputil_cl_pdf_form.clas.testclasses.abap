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
    METHODS resolve_formats         FOR TESTING.
    METHODS resolve_format_unknown  FOR TESTING.
    METHODS placeholders_of_a_text  FOR TESTING.
    METHODS check_consistent_form   FOR TESTING.
    METHODS check_reports_unknown   FOR TESTING.
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
    METHODS absolute_text_position  FOR TESTING.
    METHODS absolute_has_no_flow    FOR TESTING.
    METHODS absolute_table_position FOR TESTING.
    METHODS absolute_below_a_table  FOR TESTING.
    METHODS absolute_table_pages    FOR TESTING.
    METHODS absolute_pushed_over    FOR TESTING.
    METHODS absolute_every_page     FOR TESTING.
    METHODS absolute_text_grows     FOR TESTING.
    METHODS absolute_text_next_page FOR TESTING.
    METHODS condition_flow          FOR TESTING.
    METHODS condition_absolute      FOR TESTING.
    METHODS repeat_flow             FOR TESTING.
    METHODS repeat_flow_without_data FOR TESTING.
    METHODS repeat_absolute         FOR TESTING.
    METHODS repeat_absolute_pages   FOR TESTING.
    METHODS check_block_and_condition FOR TESTING.

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

  METHOD resolve_formats.

    TYPES ty_p1 TYPE p LENGTH 16 DECIMALS 1.
    TYPES ty_p2 TYPE p LENGTH 16 DECIMALS 2.
    TYPES:
      BEGIN OF ty_s_order,
        bstdk TYPE d,
        uzeit TYPE t,
        netwr TYPE p LENGTH 8 DECIMALS 2,
        waers TYPE c LENGTH 5,
        menge TYPE p LENGTH 8 DECIMALS 3,
        meins TYPE c LENGTH 3,
        rate  TYPE p LENGTH 8 DECIMALS 3,
        datum TYPE string,
      END OF ty_s_order.
    DATA ls_order TYPE ty_s_order.

    ls_order-bstdk = '20261128'.
    ls_order-uzeit = '134501'.
    ls_order-netwr = '1234.50'.
    ls_order-waers = `EUR`.
    ls_order-menge = '12.500'.
    ls_order-meins = `Q#9`.
    ls_order-rate = '0.125'.
    ls_order-datum = `20261231`.

    cl_abap_unit_assert=>assert_equals(
        exp = |{ ls_order-bstdk DATE = USER } { ls_order-uzeit TIME = USER }|
        act = zabaputil_cl_pdf_form=>resolve( val  = `{BSTDK:date} {uzeit:TIME}`
                                              data = ls_order ) ).
    " a date in a text field is a date all the same
    cl_abap_unit_assert=>assert_equals(
        exp = |{ CONV d( '20261231' ) DATE = USER }|
        act = zabaputil_cl_pdf_form=>resolve( val  = `{DATUM:date}`
                                              data = ls_order ) ).
    " the currency from a component or as it is written
    cl_abap_unit_assert=>assert_equals(
        exp = |{ CONV ty_p2( '1234.50' ) NUMBER = USER } EUR|
        act = zabaputil_cl_pdf_form=>resolve( val  = `{NETWR:amount:WAERS}`
                                              data = ls_order ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |{ CONV ty_p2( '1234.50' ) NUMBER = USER } EUR|
        act = zabaputil_cl_pdf_form=>resolve( val  = `{NETWR:amount:EUR}`
                                              data = ls_order ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |{ CONV ty_p1( '12.5' ) NUMBER = USER } Q#9|
        act = zabaputil_cl_pdf_form=>resolve( val  = `{MENGE:quantity:MEINS}`
                                              data = ls_order ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |{ CONV ty_p1( '0.1' ) NUMBER = USER }|
        act = zabaputil_cl_pdf_form=>resolve( val  = `{RATE:number:1}`
                                              data = ls_order ) ).

  ENDMETHOD.

  METHOD resolve_format_unknown.
    " a format this class does not know, decimals that are no number, a
    " date that is none: the placeholder stays as it is written
    cl_abap_unit_assert=>assert_equals(
        exp = `{NAME:upper} {NUMBER:number:x} {NAME:date} {NONE:date}`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{NAME:upper} {NUMBER:number:x} {NAME:date} {NONE:date}`
                                              data = ms_data ) ).
  ENDMETHOD.

  METHOD placeholders_of_a_text.
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table( ( `NAME` ) ( `NETWR:amount:WAERS` ) ( `` ) )
        act = zabaputil_cl_pdf_form=>placeholders( `Dear {NAME}, {NETWR:amount:WAERS} {} {open` ) ).
  ENDMETHOD.

  METHOD check_consistent_form.
    ms_form-header = `{NAME} {page} of {PAGES}`.
    ms_form-footer = `{HEAD-CITY}`.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text  text = `{NAME} {number:number:0} {HEAD.CITY}` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table source = `t_items`
          t_column = VALUE #( ( name = `POS` ) ( name = `text` ) ) )
        ( type = zabaputil_cl_pdf_form=>cs_type-image source = `LOGO` )
        ( type = zabaputil_cl_pdf_form=>cs_type-image image = image_rgb( ) ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>check( val  = ms_form
                                                                       data = ms_data ) ).
    " a table without rows has its columns all the same
    CLEAR ms_data-t_items.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>check( val  = ms_form
                                                                       data = ms_data ) ).
  ENDMETHOD.

  METHOD check_reports_unknown.

    DATA lt_exp TYPE zabaputil_cl_pdf_form=>ty_t_unknown.

    ms_form-header = `{NONE} {page}`.
    ms_form-footer = `{page}`.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text  name = `greeting` text = `{NAME} {NAMEX} {NAME:upper}` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table name = `items` source = `T_ITEMS`
          t_column = VALUE #( ( name = `POS` ) ( name = `PRICE` ) ) )
        ( type = zabaputil_cl_pdf_form=>cs_type-table name = `no rows` source = `NAME` )
        ( type = zabaputil_cl_pdf_form=>cs_type-image name = `logo` source = `PICTURE` )
        " the page numbers are the header's and the footer's only
        ( type = zabaputil_cl_pdf_form=>cs_type-text  text = `{page}` ) ).

    lt_exp = VALUE #(
        ( name = `NONE`       part = zabaputil_cl_pdf_form=>cs_part-header element = 0 )
        ( name = `NAMEX`      part = zabaputil_cl_pdf_form=>cs_part-text   element = 1 label = `greeting` )
        ( name = `NAME:upper` part = zabaputil_cl_pdf_form=>cs_part-text   element = 1 label = `greeting` )
        ( name = `PRICE`      part = zabaputil_cl_pdf_form=>cs_part-column element = 2 label = `items` )
        ( name = `NAME`       part = zabaputil_cl_pdf_form=>cs_part-source element = 3 label = `no rows` )
        ( name = `PICTURE`    part = zabaputil_cl_pdf_form=>cs_part-source element = 4 label = `logo` )
        ( name = `page`       part = zabaputil_cl_pdf_form=>cs_part-text   element = 5 ) ).
    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = zabaputil_cl_pdf_form=>check( val  = ms_form
                                                                            data = ms_data ) ).

    " an object as the data: what it keeps to itself is unknown to a form
    ms_form-header = ``.
    ms_form-footer = ``.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-text text = `{SHOWN} {HIDDEN}` ) ).
    lt_exp = VALUE #( ( name = `HIDDEN` part = zabaputil_cl_pdf_form=>cs_part-text element = 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = zabaputil_cl_pdf_form=>check( val  = ms_form
                                                                            data = NEW lcl_data( ) ) ).

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

  METHOD object_public_attributes.
    DATA(lo_data) = NEW lcl_data( ).
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
    DATA(lo_data) = NEW lcl_data( ).
    lo_data->other = NEW lcl_data( ).
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
    DATA(lo_demo) = NEW zabaputil_cl_pdf_data_demo( ).
    lo_demo->zabaputil_if_pdf_data~fill_sample( ).
    ms_form-t_element = VALUE #(
        ( type     = zabaputil_cl_pdf_form=>cs_type-text
          text     = `Delivery {VBELN} to {ADDRESS-CITY}` )
        ( type     = zabaputil_cl_pdf_form=>cs_type-table
          source   = `t_items`
          t_column = VALUE #( ( name = `posnr` title = `Item`     width = 20 align = `R` )
                              ( name = `arktx` title = `Material` width = 0  align = `L` ) ) ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
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
    DATA(lt_field) = zabaputil_cl_pdf_form=>fields( ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 7 act = lines( lt_field ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_pdf_form=>ty_s_field( path = `NAME` kind = `VALUE` )
        act = lt_field[ 1 ] ).
    " a structure is a level of the path, not a field of its own
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_pdf_form=>ty_s_field( path = `HEAD-CITY` kind = `VALUE` )
        act = lt_field[ 3 ] ).
    " a table, then one row per component of its rows
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_pdf_form=>ty_s_field( path = `T_ITEMS` kind = `TABLE` )
        act = lt_field[ 4 ] ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_pdf_form=>ty_s_field( path = `TEXT` kind = `COLUMN` parent = `T_ITEMS` )
        act = lt_field[ 6 ] ).
    " a text has no fields
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_form=>fields( `text` ) ).
  ENDMETHOD.

  METHOD fields_of_an_object.
    DATA(lo_demo) = NEW zabaputil_cl_pdf_data_demo( ).
    DATA(lt_field) = zabaputil_cl_pdf_form=>fields( lo_demo ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_field[ path = `VBELN` kind = `VALUE` ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_field[ path = `ADDRESS-CITY` kind = `VALUE` ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_field[ path = `T_ITEMS` kind = `TABLE` ] ) ) ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( line_exists( lt_field[ path = `MATNR` kind = `COLUMN` parent = `T_ITEMS` ] ) ) ).
    " four values, three of the address, the table and its five columns
    cl_abap_unit_assert=>assert_equals( exp = 13 act = lines( lt_field ) ).

    " only what is public: no private attribute, no constant, no static
    DATA(lt_local) = zabaputil_cl_pdf_form=>fields( NEW lcl_data( ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_local[ path = `SHOWN` ] ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_local[ path = `HIDDEN` ] ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_local[ path = `C_FIXED` ] ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_local[ path = `GV_STATIC` ] ) ) ).
    " rows that are no structure: a table without columns
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_local[ path = `T_ROWS` kind = `TABLE` ] ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_local[ parent = `T_ROWS` ] ) ) ).
  ENDMETHOD.

  METHOD fields_stop_at_a_cycle.
    " an object that points at itself is followed three levels deep
    DATA(lo_data) = NEW lcl_data( ).
    lo_data->other = lo_data.
    DATA(lt_field) = zabaputil_cl_pdf_form=>fields( lo_data ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_field[ path = `OTHER-OTHER-SHOWN` ] ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_field[ path = `OTHER-OTHER-OTHER-SHOWN` ] ) ) ).
  ENDMETHOD.

  METHOD data_sample_of_a_class.
    " the name from a reference typed with the class: in a renamed copy of
    " this repository it is the renamed class
    DATA lo_demo TYPE REF TO zabaputil_cl_pdf_data_demo.
    DATA(li_data) = zabaputil_cl_pdf_form=>data_sample( zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_demo ) ).
    cl_abap_unit_assert=>assert_bound( li_data ).
    cl_abap_unit_assert=>assert_equals( exp = `Ada Lovelace`
                                        act = zabaputil_cl_pdf_form=>resolve( val  = `{NAME}`
                                                                              data = li_data ) ).
  ENDMETHOD.

  METHOD data_sample_refused.
    " a class that is no data class, a name that is no class, no name
    DATA lt_name TYPE string_table.
    DATA lo_writer TYPE REF TO zabaputil_cl_pdf_writer.
    DATA li_data   TYPE REF TO zabaputil_if_pdf_data.
    lt_name = VALUE #( ( zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_writer ) )
                       ( zabaputil_cl_util_context=>rtti_get_ref_type_name( li_data ) )
                       ( `ZABAPUTIL_NO_SUCH_CLASS` )
                       ( `` ) ).
    LOOP AT lt_name INTO DATA(lv_name).
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
    ms_form-data_class = zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_demo ).
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-text
                                   text = `Order {BSTNK}` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = zabaputil_cl_pdf_form=>data_sample( ms_form-data_class ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_pdf
                                                     page = 1
                                                     text = `Order PO-2026-118` ) ).
  ENDMETHOD.

  METHOD items.
    CLEAR ms_data-t_items.
    DO val TIMES.
      INSERT VALUE #( pos  = sy-index
                      text = `Nut` ) INTO TABLE ms_data-t_items.
    ENDDO.
  ENDMETHOD.

  METHOD absolute_text_position.
    " an element of an absolute form stands at its position without being
    " marked as fixed - the same line fixed_text_position expects
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-text
                                   x    = 10
                                   y    = 30
                                   text = `No. {NUMBER}` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals(
        exp = |BT /F1 10 Tf 28.346 749.671 Td { lo_pdf->font( `Helvetica` )->encode( `No. 4711` ) } Tj ET|
        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD absolute_has_no_flow.
    " a space and a page break mean nothing on a canvas
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-space h = 50 )
                                 ( type = zabaputil_cl_pdf_form=>cs_type-page_break )
                                 ( type = zabaputil_cl_pdf_form=>cs_type-text x = 10 y = 30 text = `Here` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |BT /F1 10 Tf 28.346 749.671 Td { lo_pdf->font( `Helvetica` )->encode( `Here` ) } Tj ET|
        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD absolute_table_position.
    " a table 30 mm from the left and 100 mm from the top is the table of
    " a flow that has this left margin and 80 mm of space above it
    DATA(lt_column) = VALUE zabaputil_cl_pdf_form=>ty_t_column(
        ( name = `pos`  title = `Pos`  width = 20 align = `R` )
        ( name = `text` title = `Item` width = 0  align = `L` ) ).

    DATA(ls_flow) = ms_form.
    ls_flow-margin_left = 30.
    ls_flow-t_element = VALUE #( ( type = zabaputil_cl_pdf_form=>cs_type-space h = 80 )
                                 ( type     = zabaputil_cl_pdf_form=>cs_type-table
                                   source   = `t_items`
                                   t_column = lt_column ) ).
    DATA(lo_flow) = zabaputil_cl_pdf_form=>build( val  = ls_flow
                                                  data = ms_data ).

    " without a width it ends at the right margin of the form
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #( ( type     = zabaputil_cl_pdf_form=>cs_type-table
                                   x        = 30
                                   y        = 100
                                   h        = 20
                                   source   = `t_items`
                                   t_column = lt_column ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = lo_flow->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).

    " 100 mm wide: 20 mm for the first column, 80 mm (226.772 points) for
    " the second, which starts 50 mm (141.732 points) from the left. The
    " header row is 18.5 points high, its top edge 100 mm from the top
    ms_form-t_element[ 1 ]-w = 100.
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                           data = ms_data ).
    DATA(lv_content) = lo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `85.039 539.925 56.693 18.5 re B` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `141.732 539.925 226.772 18.5 re B` ) ).
  ENDMETHOD.

  METHOD absolute_below_a_table.
    " what stands below a table keeps its distance to the end of the
    " table, however high the table was drawn on the canvas; what stands
    " beside it stays where it is
    DATA(lt_column) = VALUE zabaputil_cl_pdf_form=>ty_t_column(
        ( name = `pos`  title = `Pos`  width = 20 align = `R` )
        ( name = `text` title = `Item` width = 0  align = `L` ) ).

    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 115 text = `Below` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 150 y = 102 text = `Beside` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table x = 20 y = 100 w = 100 h = 10
          source = `t_items` t_column = lt_column ) ).
    DATA(lo_low) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).

    ms_form-t_element[ 1 ]-y = 135.
    ms_form-t_element[ 3 ]-h = 30.
    DATA(lo_high) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                  data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = lo_low->page( 1 )->content( )
                                        act = lo_high->page( 1 )->content( ) ).

    " Beside: 150 mm from the left, the top of the line 102 mm from the top
    cl_abap_unit_assert=>assert_true(
        xsdbool( lo_low->page( 1 )->content( ) CS
                 |425.197 545.576 Td { lo_low->font( `Helvetica` )->encode( `Beside` ) } Tj| ) ).

    " without rows the table is its header row, and Below moves up with it
    CLEAR ms_data-t_items.
    DATA(lo_empty) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                   data = ms_data ).
    cl_abap_unit_assert=>assert_differs( exp = lo_high->page( 1 )->content( )
                                         act = lo_empty->page( 1 )->content( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf  = lo_empty
                                                     page = 1
                                                     text = `Below` ) ).
  ENDMETHOD.

  METHOD absolute_table_pages.
    " a table that goes on on the next page takes what is below it along
    items( 80 ).
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 30 text = `Above` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 115 text = `Below` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table x = 20 y = 100 w = 100 h = 10
          source = `t_items`
          t_column = VALUE #( ( name = `pos` title = `Pos` width = 20 align = `R` ) ) ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    DATA(lv_last) = lo_pdf->page_count( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_last > 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf = lo_pdf page = 1 text = `Above` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = shown( pdf = lo_pdf page = 1 text = `Below` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = shown( pdf = lo_pdf page = lv_last text = `Below` ) ).
  ENDMETHOD.

  METHOD absolute_text_grows.
    " 14.4 points: a line is 6.35 mm high, four lines 25.4 mm. The text is
    " drawn 5 mm high: what stands below it keeps its distance to where
    " its lines end, what stands beside it stays where it is
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 60 text = `Below` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 150 y = 52 text = `Beside` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 50 h = 5 size = `14.4`
          text = |A\nB\nC\nD| ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).

    DATA(ls_fixed) = zabaputil_cl_pdf_form=>new_form( ).
    ls_fixed-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 20 y = 50 size = `14.4`
          text = |A\nB\nC\nD| )
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 150 y = 52 text = `Beside` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 20 y = `80.4` text = `Below` ) ).
    DATA(lo_fixed) = zabaputil_cl_pdf_form=>build( ls_fixed ).
    cl_abap_unit_assert=>assert_equals( exp = lo_fixed->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).

    " drawn high enough, nothing moves
    ms_form-t_element[ 3 ]-h = 30.
    ms_form-t_element[ 1 ]-y = 85.
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    ls_fixed-t_element[ 3 ]-y = 85.
    lo_fixed = zabaputil_cl_pdf_form=>build( ls_fixed ).
    cl_abap_unit_assert=>assert_equals( exp = lo_fixed->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD absolute_text_next_page.
    " from 250 mm on, four lines of 6.35 mm fit above the bottom margin at
    " 277 mm; the other four go on at the top margin of the next page, and
    " what stood 5 mm below the text follows them at that distance
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 250 h = 5 size = `14.4`
          text = |A\nB\nC\nD\nE\nF\nG\nH| )
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 260 text = `Below` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lo_pdf->page_count( ) ).

    DATA(ls_fixed) = zabaputil_cl_pdf_form=>new_form( ).
    ls_fixed-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 20 y = 250 size = `14.4`
          text = |A\nB\nC\nD| ) ).
    DATA(lo_fixed) = zabaputil_cl_pdf_form=>build( ls_fixed ).
    cl_abap_unit_assert=>assert_equals( exp = lo_fixed->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).

    ls_fixed-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 20 y = 20 size = `14.4`
          text = |E\nF\nG\nH| )
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 20 y = `50.4` text = `Below` ) ).
    lo_fixed = zabaputil_cl_pdf_form=>build( ls_fixed ).
    cl_abap_unit_assert=>assert_equals( exp = lo_fixed->page( 1 )->content( )
                                        act = lo_pdf->page( 2 )->content( ) ).
  ENDMETHOD.

  METHOD condition_flow.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Name` condition = `NAME` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `City` condition = `head.city` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Items` condition = `T_ITEMS` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `No items` condition = `!T_ITEMS` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Unknown` condition = `NONE` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Not unknown` condition = `! NONE` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `City` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Items` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( pdf = lo_pdf page = 1 text = `No items` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( pdf = lo_pdf page = 1 text = `Unknown` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Not unknown` ) ).

    " what is shown moves up: Items is where Name was without Name
    CLEAR ms_data-name.
    DATA(lo_without) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                     data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( pdf = lo_without page = 1 text = `Name` ) ).

    " without data a form shows all it has
    lo_pdf = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `No items` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Unknown` ) ).
  ENDMETHOD.

  METHOD condition_absolute.
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 30 text = `Name` condition = `NAME` )
        ( type = zabaputil_cl_pdf_form=>cs_type-rect x = 20 y = 40 w = 10 h = 10 condition = `!NAME` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 50 text = `Every` every_page = abap_true
          condition = `NONE` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    DATA(ls_fixed) = zabaputil_cl_pdf_form=>new_form( ).
    ls_fixed-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 20 y = 30 text = `Name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_form=>build( ls_fixed )->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD repeat_flow.
    INSERT VALUE #( pos = 30 ) INTO TABLE ms_data-t_items.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Before` )
        ( type = zabaputil_cl_pdf_form=>cs_type-repeat name = `item` source = `t_items` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `After` )
        " the block: anywhere in the form, printed where its REPEAT element is
        ( type = zabaputil_cl_pdf_form=>cs_type-text block = `ITEM` text = `Item {POS}: {TEXT} for {NAME}` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text block = `item` text = `({TEXT})` condition = `TEXT` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Item 10: Nut for Ada` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Item 20: Bolt for Ada` ) ).
    " the wrapping of the flow joins the words with one blank
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Item 30: for Ada` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `(Nut)` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `(Bolt)` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( pdf = lo_pdf page = 1 text = `()` ) ).

    " in the flow, in the order of the rows, between Before and After
    DATA(ls_flow) = zabaputil_cl_pdf_form=>new_form( ).
    ls_flow-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Before` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Item 10: Nut for Ada` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `(Nut)` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Item 20: Bolt for Ada` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `(Bolt)` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `Item 30:  for Ada` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `After` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_form=>build( ls_flow )->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD repeat_flow_without_data.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-repeat name = `item` source = `T_ITEMS` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text block = `item` text = `Item {POS}` ) ).
    " without data the block once, with its placeholders
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( ms_form ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Item {POS}` ) ).
    " a table without rows: no block
    CLEAR ms_data-t_items.
    lo_pdf = zabaputil_cl_pdf_form=>build( val  = ms_form
                                           data = ms_data ).
    cl_abap_unit_assert=>assert_initial( lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD repeat_absolute.
    " a block of 10 mm per row from 100 mm on; its elements are placed
    " from the top of the block. What stood 5 mm below the first block
    " follows the last one at that distance
    items( 3 ).
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 20 y = 115 text = `Below` )
        ( type = zabaputil_cl_pdf_form=>cs_type-repeat name = `rows` x = 20 y = 100 h = 10 source = `T_ITEMS` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text block = `rows` x = 30 y = 2 text = `Item {POS}` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).

    DATA(ls_fixed) = zabaputil_cl_pdf_form=>new_form( ).
    ls_fixed-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 30 y = 102 text = `Item 1` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 30 y = 112 text = `Item 2` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 30 y = 122 text = `Item 3` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 20 y = 135 text = `Below` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_form=>build( ls_fixed )->page( 1 )->content( )
                                        act = lo_pdf->page( 1 )->content( ) ).
  ENDMETHOD.

  METHOD repeat_absolute_pages.
    " blocks of 10 mm from 100 mm on: 17 fit above the bottom margin at
    " 277 mm, the 18th starts the next page at the top margin
    items( 30 ).
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-repeat name = `rows` x = 20 y = 100 h = 10 source = `T_ITEMS` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text block = `rows` x = 30 y = 2 text = `Item {POS}` ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 1 text = `Item 17` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = shown( pdf = lo_pdf page = 1 text = `Item 18` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 2 text = `Item 18` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = shown( pdf = lo_pdf page = 2 text = `Item 30` ) ).

    DATA(ls_fixed) = zabaputil_cl_pdf_form=>new_form( ).
    ls_fixed-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 30 y = 22 text = `Item 18` ) ).
    DATA(lv_first) = zabaputil_cl_pdf_form=>build( ls_fixed )->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals( exp = lv_first
                                        act = substring( val = lo_pdf->page( 2 )->content( )
                                                         len = strlen( lv_first ) ) ).
  ENDMETHOD.

  METHOD check_block_and_condition.

    DATA lt_exp TYPE zabaputil_cl_pdf_form=>ty_t_unknown.

    " a block names the components of a row - also when the table is
    " empty - and those of the data
    CLEAR ms_data-t_items.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-repeat name = `item` source = `T_ITEMS` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text block = `item` text = `{POS} {TEXT} {NAME} {PRICE}`
          condition = `!TEXT` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text block = `lost` text = `x` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text text = `{POS}` condition = `HEAD-NONE` )
        ( type = zabaputil_cl_pdf_form=>cs_type-repeat name = `bad` source = `NAME` ) ).
    lt_exp = VALUE #(
        ( name = `PRICE`     part = zabaputil_cl_pdf_form=>cs_part-text      element = 2 )
        ( name = `lost`      part = zabaputil_cl_pdf_form=>cs_part-block     element = 3 )
        ( name = `HEAD-NONE` part = zabaputil_cl_pdf_form=>cs_part-condition element = 4 )
        ( name = `POS`       part = zabaputil_cl_pdf_form=>cs_part-text      element = 4 )
        ( name = `NAME`      part = zabaputil_cl_pdf_form=>cs_part-source    element = 5 label = `bad` ) ).
    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = zabaputil_cl_pdf_form=>check( val  = ms_form
                                                                            data = ms_data ) ).

  ENDMETHOD.

  METHOD absolute_pushed_over.
    " an element the table pushed over the bottom margin starts the next
    " page at the top margin, and what was below it follows at its distance
    items( 10 ).
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 10 y = 115 h = 160 text = `Box` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text x = 10 y = 125 text = `After` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table x = 20 y = 100 w = 100 h = 10
          source = `t_items`
          t_column = VALUE #( ( name = `pos` title = `Pos` width = 20 align = `R` ) ) ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lo_pdf->page_count( ) ).

    " the second page is the page of two fixed texts, 20 and 30 mm from the top
    DATA(ls_fixed) = zabaputil_cl_pdf_form=>new_form( ).
    ls_fixed-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 10 y = 20 text = `Box` )
        ( type = zabaputil_cl_pdf_form=>cs_type-text fixed = abap_true x = 10 y = 30 text = `After` ) ).
    DATA(lo_fixed) = zabaputil_cl_pdf_form=>build( ls_fixed ).
    cl_abap_unit_assert=>assert_equals( exp = lo_fixed->page( 1 )->content( )
                                        act = lo_pdf->page( 2 )->content( ) ).
  ENDMETHOD.

  METHOD absolute_every_page.
    items( 80 ).
    ms_form-absolute = abap_true.
    ms_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text every_page = abap_true x = 150 y = 10 text = `Letterhead` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table x = 20 y = 100 w = 100 h = 10
          source = `t_items`
          t_column = VALUE #( ( name = `pos` title = `Pos` width = 20 align = `R` ) ) ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ms_form
                                                 data = ms_data ).
    cl_abap_unit_assert=>assert_true( xsdbool( lo_pdf->page_count( ) > 1 ) ).
    DO lo_pdf->page_count( ) TIMES.
      cl_abap_unit_assert=>assert_equals( exp = 1
                                          act = shown( pdf  = lo_pdf
                                                       page = sy-index
                                                       text = `Letterhead` ) ).
    ENDDO.
  ENDMETHOD.

ENDCLASS.

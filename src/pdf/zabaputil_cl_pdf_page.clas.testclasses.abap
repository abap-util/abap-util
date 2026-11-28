CLASS ltcl_page DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    DATA mo_page TYPE REF TO zabaputil_cl_pdf_page.
    DATA mo_font TYPE REF TO zabaputil_if_pdf_font.

    METHODS setup.
    "! A fresh page - what setup( ) does, for a test that needs a second one
    METHODS fresh_page.

    METHODS size_in_units           FOR TESTING.
    METHODS text_at_position        FOR TESTING.
    METHODS text_in_units           FOR TESTING.
    METHODS text_aligned            FOR TESTING.
    METHODS text_width_of           FOR TESTING.
    METHODS text_empty              FOR TESTING.
    METHODS text_without_font       FOR TESTING.
    METHODS text_color_bracketed    FOR TESTING.
    METHODS text_color_like_fill    FOR TESTING.
    METHODS colors                  FOR TESTING.
    METHODS color_invalid           FOR TESTING.
    METHODS line_and_width          FOR TESTING.
    METHODS dash_pattern            FOR TESTING.
    METHODS rect_styles             FOR TESTING.
    METHODS ellipse_curves          FOR TESTING.
    METHODS image_sizes             FOR TESTING.
    METHODS link_annotation         FOR TESTING.
    METHODS raw_and_order           FOR TESTING.
    METHODS fluent                  FOR TESTING.
    METHODS rotate_around_point     FOR TESTING.
    METHODS clip_and_state          FOR TESTING.
    METHODS alpha_states            FOR TESTING.
    METHODS alpha_in_resources      FOR TESTING.
    METHODS rounded_rectangle       FOR TESTING.
    METHODS free_path               FOR TESTING.
    METHODS rotated_text            FOR TESTING.

    " a PNG file of 2 x 2 pixels
    METHODS image_rgb
      RETURNING
        VALUE(result) TYPE xstring.

ENDCLASS.

CLASS ltcl_page IMPLEMENTATION.

  METHOD image_rgb.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D4948445200000002000000020802000000FDD49A73000000054944415478DA63F8CF51E86AD7` &&
             `0000000D49444154C0C000C20CFF8100001FEE05FB61FE8DCC0000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD setup.
    fresh_page( ).
  ENDMETHOD.

  METHOD fresh_page.
    " 600 x 800 points, one unit is one point; Courier is 600 wide per
    " character, so a text of n characters at 10 points is n * 6 wide
    mo_page = NEW #( width  = 600
                     height = 800
                     scale  = 1 ).
    mo_font = NEW zabaputil_cl_pdf_font_std( name     = `Courier`
                                             resource = `F1` ).
  ENDMETHOD.

  METHOD size_in_units.
    DATA lv_scale  TYPE f.
    DATA lv_width  TYPE f.
    DATA lv_height TYPE f.
    lv_scale = 720.
    lv_scale = lv_scale / 254.
    lv_width = `595.28`.
    lv_height = `841.89`.
    DATA(lo_page) = NEW zabaputil_cl_pdf_page( width  = lv_width
                                               height = lv_height
                                               scale  = lv_scale ).
    " A4 is rounded to hundredths of a point: 595.28 are a bit more than 210 mm
    cl_abap_unit_assert=>assert_equals( exp = `210.002` act = zabaputil_cl_pdf_writer=>num( lo_page->width( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `297` act = zabaputil_cl_pdf_writer=>num( lo_page->height( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = zabaputil_cl_pdf_writer=>num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `841.89` act = zabaputil_cl_pdf_writer=>num( lo_page->height_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `600` act = zabaputil_cl_pdf_writer=>num( mo_page->width( ) ) ).
  ENDMETHOD.

  METHOD text_at_position.
    " y is counted from the top, PDF counts from the bottom
    mo_page->set_font( mo_font ).
    mo_page->text( x   = 10
                   y   = 20
                   val = `Hi` ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 12 Tf 10 780 Td <4869> Tj ET`
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD text_in_units.
    " 10 mm are 28.346 points, 20 mm from the top are 785.197 from the bottom
    DATA lv_scale  TYPE f.
    DATA lv_width  TYPE f.
    DATA lv_height TYPE f.
    lv_scale = 720.
    lv_scale = lv_scale / 254.
    lv_width = `595.28`.
    lv_height = `841.89`.
    DATA(lo_page) = NEW zabaputil_cl_pdf_page( width  = lv_width
                                               height = lv_height
                                               scale  = lv_scale ).
    lo_page->set_font( font = mo_font
                       size = 9 ).
    lo_page->text( x   = 10
                   y   = 20
                   val = `Hi` ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 9 Tf 28.346 785.197 Td <4869> Tj ET`
                                        act = lo_page->content( ) ).
  ENDMETHOD.

  METHOD text_aligned.
    mo_page->set_font( font = mo_font
                       size = 10 ).
    mo_page->text( x     = 100
                   y     = 20
                   val   = `Hi`
                   align = `R` ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 10 Tf 88 780 Td <4869> Tj ET`
                                        act = mo_page->content( ) ).

    fresh_page( ).
    mo_page->set_font( font = mo_font
                       size = 10 ).
    mo_page->text( x     = 100
                   y     = 20
                   val   = `Hi`
                   align = `c` ).
    cl_abap_unit_assert=>assert_equals( exp = `BT /F1 10 Tf 94 780 Td <4869> Tj ET`
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD text_width_of.
    mo_page->set_font( font = mo_font
                       size = 10 ).
    cl_abap_unit_assert=>assert_equals( exp = `12` act = zabaputil_cl_pdf_writer=>num( mo_page->text_width( `Hi` ) ) ).

    " in the unit of the page: 12 points are half a unit of 24 points
    DATA(lo_page) = NEW zabaputil_cl_pdf_page( width  = 600
                                               height = 800
                                               scale  = 24 ).
    lo_page->set_font( font = mo_font
                       size = 10 ).
    cl_abap_unit_assert=>assert_equals( exp = `0.5` act = zabaputil_cl_pdf_writer=>num( lo_page->text_width( `Hi` ) ) ).
  ENDMETHOD.

  METHOD text_empty.
    mo_page->set_font( mo_font ).
    mo_page->text( x   = 10
                   y   = 20
                   val = `` ).
    cl_abap_unit_assert=>assert_initial( mo_page->content( ) ).
  ENDMETHOD.

  METHOD text_without_font.
    TRY.
        mo_page->text( x   = 10
                       y   = 20
                       val = `Hi` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        mo_page->text_width( `Hi` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD text_color_bracketed.
    " text and fill share one colour operator: a text in its own colour
    " saves and restores the state, so the fill colour is still there
    mo_page->set_font( mo_font ).
    mo_page->set_text_color( `FF0000` ).
    mo_page->text( x   = 10
                   y   = 20
                   val = `Hi` ).
    cl_abap_unit_assert=>assert_equals( exp = `q 1 0 0 rg BT /F1 12 Tf 10 780 Td <4869> Tj ET Q`
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD text_color_like_fill.
    mo_page->set_font( mo_font ).
    mo_page->set_fill_color( `FF0000` ).
    mo_page->set_text_color( `#ff0000` ).
    mo_page->text( x   = 10
                   y   = 20
                   val = `Hi` ).
    cl_abap_unit_assert=>assert_equals( exp = |1 0 0 rg\nBT /F1 12 Tf 10 780 Td <4869> Tj ET|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD colors.
    mo_page->set_fill_color( `#FF8000` ).
    mo_page->set_draw_color( `0000ff` ).
    mo_page->set_fill_color( `FFFFFF` ).
    cl_abap_unit_assert=>assert_equals( exp = |1 0.502 0 rg\n0 0 1 RG\n1 1 1 rg|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD color_invalid.
    DATA lt_bad TYPE string_table.
    lt_bad = VALUE #( ( `12345` ) ( `GGGGGG` ) ( `` ) ( `#1234567` ) ( `red` ) ).
    LOOP AT lt_bad INTO DATA(lv_bad).
      TRY.
          mo_page->set_fill_color( lv_bad ).
          cl_abap_unit_assert=>fail( lv_bad ).
        CATCH zabaputil_cx_util_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
    cl_abap_unit_assert=>assert_initial( mo_page->content( ) ).
  ENDMETHOD.

  METHOD line_and_width.
    mo_page->set_line_width( 2 ).
    mo_page->line( x1 = 10
                   y1 = 20
                   x2 = 110
                   y2 = 30 ).
    cl_abap_unit_assert=>assert_equals( exp = |2 w\n10 780 m 110 770 l S|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD dash_pattern.
    mo_page->set_dash( dash = 3
                       gap  = 2 ).
    mo_page->set_dash( dash = 4 ).
    mo_page->set_dash( ).
    cl_abap_unit_assert=>assert_equals( exp = |[3 2] 0 d\n[4 4] 0 d\n[] 0 d|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD rect_styles.
    " PDF wants the lower left corner: 800 - 20 - 50
    mo_page->rect( x = 10
                   y = 20
                   w = 100
                   h = 50 ).
    mo_page->rect( x     = 10
                   y     = 20
                   w     = 100
                   h     = 50
                   style = `F` ).
    mo_page->rect( x     = 10
                   y     = 20
                   w     = 100
                   h     = 50
                   style = `df` ).
    cl_abap_unit_assert=>assert_equals(
        exp = |10 730 100 50 re S\n10 730 100 50 re f\n10 730 100 50 re B|
        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD ellipse_curves.
    " a circle of radius 50 around ( 100, 100 ): four curves, the control
    " points 27.615 from the ends
    mo_page->ellipse( x  = 100
                      y  = 100
                      rx = 50 ).
    cl_abap_unit_assert=>assert_equals(
        exp = `150 700 m 150 727.615 127.615 750 100 750 c 72.385 750 50 727.615 50 700 c ` &&
              `50 672.385 72.385 650 100 650 c 127.615 650 150 672.385 150 700 c S`
        act = mo_page->content( ) ).

    fresh_page( ).
    mo_page->ellipse( x     = 100
                      y     = 100
                      rx    = 50
                      ry    = 10
                      style = `F` ).
    DATA(lv_content) = mo_page->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS `150 700 m ` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS ` 100 710 c ` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS ` 100 690 c ` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CP `* c f` ) ).
  ENDMETHOD.

  METHOD image_sizes.
    DATA(lo_image) = NEW zabaputil_cl_pdf_image( content  = image_rgb( )
                                                 resource = `I1` ).
    " the width given: the height follows from 2 x 2 pixels
    mo_page->image( image = lo_image
                    x     = 10
                    y     = 20
                    w     = 40 ).
    " the height given
    mo_page->image( image = lo_image
                    x     = 10
                    y     = 20
                    h     = 30 ).
    " both given: stretched
    mo_page->image( image = lo_image
                    x     = 10
                    y     = 20
                    w     = 40
                    h     = 10 ).
    " none: a pixel is a point
    mo_page->image( image = lo_image
                    x     = 10
                    y     = 20 ).
    cl_abap_unit_assert=>assert_equals(
        exp = |q 40 0 0 40 10 740 cm /I1 Do Q\nq 30 0 0 30 10 750 cm /I1 Do Q\n| &&
              |q 40 0 0 10 10 770 cm /I1 Do Q\nq 2 0 0 2 10 778 cm /I1 Do Q|
        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD link_annotation.
    mo_page->link( x   = 10
                   y   = 20
                   w   = 100
                   h   = 10
                   url = `https://a.b/c` ).
    cl_abap_unit_assert=>assert_initial( mo_page->content( ) ).
    DATA(lt_annotation) = mo_page->annotations( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_annotation ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `<< /Type /Annot /Subtype /Link /Rect [10 770 110 780] /Border [0 0 0] ` &&
              `/A << /S /URI /URI <68747470733A2F2F612E622F63> >> >>`
        act = lt_annotation[ 1 ] ).
  ENDMETHOD.

  METHOD raw_and_order.
    mo_page->raw( `q` ).
    mo_page->line( x1 = 0
                   y1 = 0
                   x2 = 1
                   y2 = 1 ).
    mo_page->raw( `Q` ).
    cl_abap_unit_assert=>assert_equals( exp = |q\n0 800 m 1 799 l S\nQ|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD fluent.
    DATA(lo_same) = mo_page->set_font( mo_font )->set_text_color( `000000` )->set_fill_color( `000000`
        )->set_draw_color( `000000` )->set_line_width( 1 )->set_dash( )->text( x   = 1
                                                                                y   = 1
                                                                                val = `x`
        )->line( x1 = 0
                 y1 = 0
                 x2 = 1
                 y2 = 1 )->rect( x = 0
                                 y = 0
                                 w = 1
                                 h = 1 )->ellipse( x  = 5
                                                   y  = 5
                                                   rx = 1 )->raw( `q Q` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lo_same = mo_page ) ).
  ENDMETHOD.

  METHOD rotate_around_point.
    " a quarter turn around 100, 100 - 100, 700 with y upwards
    mo_page->save( )->rotate( angle = 90
                              x     = 100
                              y     = 100 )->restore( ).
    cl_abap_unit_assert=>assert_equals( exp = |q\n0 1 -1 0 800 600 cm\nQ|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD clip_and_state.
    mo_page->save( )->clip_rect( x = 10
                                 y = 60
                                 w = 100
                                 h = 50 )->restore( ).
    cl_abap_unit_assert=>assert_equals( exp = |q\n10 690 100 50 re W n\nQ|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD alpha_states.
    " one graphics state per pair of values, named after them
    mo_page->set_alpha( `0.5` ).
    mo_page->set_alpha( fill   = `0.25`
                        stroke = 1 ).
    mo_page->set_alpha( `0.5` ).
    mo_page->set_alpha( 3 ).
    cl_abap_unit_assert=>assert_equals( exp = |/GS500_500 gs\n/GS250_1000 gs\n/GS500_500 gs\n/GS1000_1000 gs|
                                        act = mo_page->content( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE string_table( ( `GS500_500 0.5 0.5` )
                                                                  ( `GS250_1000 0.25 1` )
                                                                  ( `GS1000_1000 1 1` ) )
                                        act = mo_page->alphas( ) ).
  ENDMETHOD.

  METHOD alpha_in_resources.
    " the file in hex: its first bytes are no UTF-8
    DATA lv_hex TYPE string.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( )->set_alpha( `0.3` ).
    lv_hex = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `<< /Type /ExtGState /ca 0.3 /CA 0.3 >>` ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/ExtGState << /GS300_300 ` ) ) ).
    " without transparency the resources have no graphics states
    lo_pdf = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( ).
    lv_hex = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `ExtGState` ) ) ).
  ENDMETHOD.

  METHOD rounded_rectangle.
    " four straight edges and four quarter circles, closed and filled
    mo_page->rounded_rect( x     = 10
                           y     = 10
                           w     = 100
                           h     = 50
                           r     = 10
                           style = `F` ).
    DATA(lv_content) = mo_page->content( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CP |20 790 m*| ) ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = count( val = lv_content sub = ` c` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = count( val = lv_content sub = ` l` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |100 790 l\n105.523 790 110 785.523 110 780 c| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |h\nf| ) ).
    " no radius: a rectangle
    fresh_page( ).
    mo_page->rounded_rect( x = 10
                           y = 10
                           w = 100
                           h = 50
                           r = 0 ).
    cl_abap_unit_assert=>assert_equals( exp = `10 740 100 50 re S` act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD free_path.
    mo_page->move_to( x = 10
                      y = 10 )->line_to( x = 50
                                         y = 10 )->curve_to( x1 = 60
                                                             y1 = 10
                                                             x2 = 60
                                                             y2 = 30
                                                             x3 = 50
                                                             y3 = 30 )->close_path( )->draw_path( `DF` ).
    cl_abap_unit_assert=>assert_equals( exp = |10 790 m\n50 790 l\n60 790 60 770 50 770 c\nh\nB|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

  METHOD rotated_text.
    " up the page, centred: the start is moved back by half of its 12
    " points along its direction
    mo_page->set_font( font = mo_font
                       size = 10 ).
    mo_page->text_rotated( x     = 100
                           y     = 100
                           val   = `AB`
                           angle = 90
                           align = `C` ).
    cl_abap_unit_assert=>assert_equals( exp = |BT /F1 10 Tf 0 1 -1 0 100 694 Tm { mo_font->encode( `AB` ) } Tj ET|
                                        act = mo_page->content( ) ).
  ENDMETHOD.

ENDCLASS.

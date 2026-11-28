CLASS ltcl_pdf DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS unit_invalid            FOR TESTING.
    METHODS format_invalid          FOR TESTING.
    METHODS scale_of_units          FOR TESTING.
    METHODS page_default            FOR TESTING.
    METHODS page_formats            FOR TESTING.
    METHODS page_turned             FOR TESTING.
    METHODS page_custom_size        FOR TESTING.
    METHODS page_by_number          FOR TESTING.
    METHODS font_once_per_name      FOR TESTING.
    METHODS font_unknown            FOR TESTING.
    METHODS resources_numbered      FOR TESTING.
    METHODS render_structure        FOR TESTING.
    METHODS render_content          FOR TESTING.
    METHODS render_info             FOR TESTING.
    METHODS render_without_page     FOR TESTING.
    METHODS render_twice            FOR TESTING.
    METHODS render_link             FOR TESTING.
    METHODS render_all_resources    FOR TESTING.
    METHODS render_deflated         FOR TESTING.
    METHODS bookmarks_outline       FOR TESTING.
    METHODS link_to_page            FOR TESTING.
    METHODS attachments             FOR TESTING.
    METHODS invoice                 FOR TESTING.
    METHODS form_fields             FOR TESTING.

    " a font and an image made for the tests of their classes
    METHODS font_format_4
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_rgb
      RETURNING
        VALUE(result) TYPE xstring.

    "! The file in hex holds this ASCII text
    METHODS has
      IMPORTING
        file          TYPE xstring
        text          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Every entry of the cross-reference table points at the object it
    "! numbers, and startxref at the table
    METHODS check_xref
      IMPORTING
        file TYPE xstring.

    METHODS num
      IMPORTING
        val           TYPE f
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS ltcl_pdf IMPLEMENTATION.

  METHOD font_format_4.

    DATA lv_hex TYPE string.

    lv_hex = `00010000000A0080000300204F532F32652660B7000000AC00000060636D6170000C00740000010C0000002C676C79660A09` &&
             `0940000001380000004468656164620C43A10000017C00000036686865610610028E000001B400000024686D747807080096` &&
             `000001D80000000C6C6F636100110022000001E4000000086D61787000050005000001EC000000206E616D65062715D20000` &&
             `020C0000006E706F7374FF9F00320000027C0000002000020258019000050000028A02BC0000008C028A02BC000001E00032` &&
             `00FA00000000000000000000000000000001000000000000000000000000544553540040004100420320FF38005A032000C8` &&
             `000000010000000001F402BC00000020000100000001000300010000000C00040020000000040002000000020042FFFF0000` &&
             `0041FFFFFFC0000100000000000100320000022602BC00030000010101010032000001F40000000002BC0000FD4400010032` &&
             `0000028A02BC00030000010101010032000002580000000002BC0000FD440001000000010000DD210C395F0F3CF5000B03E8` &&
             `000000000000000000000000000000000032000002BC02BC000000080002000000000000000100000320FF38000003200032` &&
             `003202BC00010000000000000000000000000000000301F400320258003202BC003200000000001100220001000000030004` &&
             `0001000000000001000000000000000000000000000000000000000400360003000104090001000800000003000104090002` &&
             `000E000800030001040900040012001600030001040900060010002800540065007300740052006500670075006C00610072` &&
             `005400650073007400200046006F006E007400540065007300740046006F006E007400000003000000000000FF9C00320000` &&
             `000000000000000000000000000000000000`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_rgb.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D4948445200000002000000020802000000FDD49A73000000054944415478DA63F8CF51E86AD7` &&
             `0000000D49444154C0C000C20CFF8100001FEE05FB61FE8DCC0000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD has.

    DATA lv_hex TYPE string.

    lv_hex = file.
    result = xsdbool( find( val = lv_hex
                            sub = zabaputil_cl_pdf_writer=>hex( text ) ) >= 0 ).

  ENDMETHOD.

  METHOD check_xref.

    DATA lv_hex    TYPE string.
    DATA lv_tail   TYPE string.
    DATA lv_x      TYPE xstring.
    DATA lv_count  TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_id     TYPE i.
    DATA lv_expect TYPE string.

    lv_hex = file.
    DATA(lv_pos) = find( val = lv_hex
                         sub = zabaputil_cl_pdf_writer=>hex( |\nxref\n| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_pos > 0 ) ).

    " from the table to the end the file is ASCII
    lv_x = substring( val = lv_hex
                      off = lv_pos + 2 ).
    lv_tail = zabaputil_cl_util_context=>conv_get_string_by_xstring( lv_x ).
    SPLIT lv_tail AT |\n| INTO TABLE DATA(lt_line).

    cl_abap_unit_assert=>assert_equals( exp = `xref`
                                        act = lt_line[ 1 ] ).
    DATA(lv_range) = lt_line[ 2 ].
    SPLIT lv_range AT ` ` INTO DATA(lv_first) DATA(lv_size).
    cl_abap_unit_assert=>assert_equals( exp = `0`
                                        act = lv_first ).
    lv_count = lv_size.
    cl_abap_unit_assert=>assert_equals( exp = `0000000000 65535 f `
                                        act = lt_line[ 3 ] ).

    lv_id = 1.
    WHILE lv_id < lv_count.
      DATA(lv_entry) = lt_line[ lv_id + 3 ].
      cl_abap_unit_assert=>assert_equals( exp = 19
                                          act = strlen( lv_entry ) ).
      lv_offset = substring( val = lv_entry
                             len = 10 ).
      lv_expect = zabaputil_cl_pdf_writer=>hex( |{ lv_id } 0 obj\n| ).
      cl_abap_unit_assert=>assert_equals( exp = lv_expect
                                          act = substring( val = lv_hex
                                                           off = lv_offset * 2
                                                           len = strlen( lv_expect ) ) ).
      lv_id = lv_id + 1.
    ENDWHILE.

    cl_abap_unit_assert=>assert_equals( exp = `trailer`
                                        act = lt_line[ lv_count + 3 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `startxref`
                                        act = lt_line[ lv_count + 5 ] ).
    lv_offset = lt_line[ lv_count + 6 ].
    cl_abap_unit_assert=>assert_equals( exp = lv_pos DIV 2 + 1
                                        act = lv_offset ).
    cl_abap_unit_assert=>assert_equals( exp = `%%EOF`
                                        act = lt_line[ lv_count + 7 ] ).

  ENDMETHOD.

  METHOD num.
    result = zabaputil_cl_pdf_writer=>num( val ).
  ENDMETHOD.

  METHOD unit_invalid.
    TRY.
        zabaputil_cl_pdf=>create( unit = `px` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD format_invalid.
    TRY.
        zabaputil_cl_pdf=>create( format = `B5` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        zabaputil_cl_pdf=>create( )->add_page( format = `B5` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD scale_of_units.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = num( zabaputil_cl_pdf=>create( )->scale( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2.835` act = num( zabaputil_cl_pdf=>create( unit = `mm` )->scale( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `28.346` act = num( zabaputil_cl_pdf=>create( unit = `CM` )->scale( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `72` act = num( zabaputil_cl_pdf=>create( unit = `in` )->scale( ) ) ).
  ENDMETHOD.

  METHOD page_default.
    DATA(lo_page) = zabaputil_cl_pdf=>create( )->add_page( ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `841.89` act = num( lo_page->height_pt( ) ) ).

    lo_page = zabaputil_cl_pdf=>create( format      = `letter`
                                        orientation = `l` )->add_page( ).
    cl_abap_unit_assert=>assert_equals( exp = `792` act = num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `612` act = num( lo_page->height_pt( ) ) ).
  ENDMETHOD.

  METHOD page_formats.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    DATA(lo_page) = lo_pdf->add_page( format = `A5` ).
    cl_abap_unit_assert=>assert_equals( exp = `419.53` act = num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = num( lo_page->height_pt( ) ) ).
    lo_page = lo_pdf->add_page( format      = `A3`
                                orientation = `L` ).
    cl_abap_unit_assert=>assert_equals( exp = `1190.55` act = num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `841.89` act = num( lo_page->height_pt( ) ) ).
    lo_page = lo_pdf->add_page( format = `Legal` ).
    cl_abap_unit_assert=>assert_equals( exp = `1008` act = num( lo_page->height_pt( ) ) ).
  ENDMETHOD.

  METHOD page_turned.
    " the orientation alone turns the format of the document
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    DATA(lo_page) = lo_pdf->add_page( orientation = `L` ).
    cl_abap_unit_assert=>assert_equals( exp = `841.89` act = num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = num( lo_page->height_pt( ) ) ).
    lo_page = lo_pdf->add_page( orientation = `P` ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = num( lo_page->width_pt( ) ) ).

    lo_pdf = zabaputil_cl_pdf=>create( orientation = `L` ).
    lo_page = lo_pdf->add_page( orientation = `P` ).
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = num( lo_page->width_pt( ) ) ).
    lo_page = lo_pdf->add_page( orientation = `L` ).
    cl_abap_unit_assert=>assert_equals( exp = `841.89` act = num( lo_page->width_pt( ) ) ).
  ENDMETHOD.

  METHOD page_custom_size.
    " 100 mm x 50 mm in points
    DATA(lo_page) = zabaputil_cl_pdf=>create( unit = `mm` )->add_page( width  = 100
                                                                     height = 50 ).
    cl_abap_unit_assert=>assert_equals( exp = `283.465` act = num( lo_page->width_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `141.732` act = num( lo_page->height_pt( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `100` act = num( lo_page->width( ) ) ).
  ENDMETHOD.

  METHOD page_by_number.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_pdf->page_count( ) ).
    DATA(lo_first) = lo_pdf->add_page( ).
    DATA(lo_second) = lo_pdf->add_page( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lo_pdf->page( 1 ) = lo_first ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lo_pdf->page( 2 ) = lo_second ) ).
    TRY.
        lo_pdf->page( 0 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        lo_pdf->page( 3 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD font_once_per_name.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    DATA(lo_font) = lo_pdf->font( ).
    cl_abap_unit_assert=>assert_equals( exp = `F1` act = lo_font->resource( ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lo_pdf->font( `Helvetica` ) = lo_font ) ).
    cl_abap_unit_assert=>assert_equals( exp = `F2` act = lo_pdf->font( `Courier` )->resource( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `F1` act = lo_pdf->font( `Helvetica` )->resource( ) ).
  ENDMETHOD.

  METHOD font_unknown.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    TRY.
        lo_pdf->font( `Arial` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    " a font that could not be created takes no number
    cl_abap_unit_assert=>assert_equals( exp = `F1` act = lo_pdf->font( `Courier` )->resource( ) ).
  ENDMETHOD.

  METHOD resources_numbered.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    lo_pdf->font( ).
    cl_abap_unit_assert=>assert_equals( exp = `F2` act = lo_pdf->font_ttf( font_format_4( ) )->resource( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `F3` act = lo_pdf->font_ttf( font_format_4( ) )->resource( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `I1` act = lo_pdf->image( image_rgb( ) )->resource( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `I2` act = lo_pdf->image( image_rgb( ) )->resource( ) ).
  ENDMETHOD.

  METHOD render_structure.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( ).
    lo_pdf->add_page( orientation = `L` ).
    DATA(lv_file) = lo_pdf->render( ).
    check_xref( lv_file ).

    " resources 1, page tree 2, a content stream and a page per page,
    " catalog 7, information 8
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Font << >> /XObject << >> >>` ) ).
    cl_abap_unit_assert=>assert_true(
        has( file = lv_file
             text = `<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595.28 841.89] /Resources 1 0 R /Contents 3 0 R >>` ) ).
    cl_abap_unit_assert=>assert_true(
        has( file = lv_file
             text = `<< /Type /Page /Parent 2 0 R /MediaBox [0 0 841.89 595.28] /Resources 1 0 R /Contents 5 0 R >>` ) ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `<< /Type /Pages /Kids [ 4 0 R 6 0 R ] /Count 2 >>` ) ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `<< /Type /Catalog /Pages 2 0 R >>` ) ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Size 9 /Root 7 0 R /Info 8 0 R >>` ) ).
  ENDMETHOD.

  METHOD render_content.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    DATA(lo_page) = lo_pdf->add_page( ).
    lo_page->set_font( lo_pdf->font( `Courier` ) ).
    lo_page->text( x   = 10
                   y   = 41
                   val = `Hi` ).
    DATA(lv_file) = lo_pdf->render( ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true(
        has( file = lv_file
             text = `<< /Type /Font /Subtype /Type1 /BaseFont /Courier /Encoding /WinAnsiEncoding >>` ) ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Font << /F1 1 0 R >>` ) ).
    cl_abap_unit_assert=>assert_true(
        has( file = lv_file
             text = |<< /Length 38 >>\nstream\nBT /F1 12 Tf 10 800.89 Td <4869> Tj ET\nendstream| ) ).
  ENDMETHOD.

  METHOD render_info.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->set_info( title  = `Ti`
                      author = `A` ).
    " set again: the later value counts, an empty one changes nothing
    lo_pdf->set_info( author = `B` ).
    DATA(lv_file) = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_true(
        has( file = lv_file
             text = `<< /Title <FEFF00540069> /Author <FEFF0042> /Producer (abap-util) >>` ) ).

    lv_file = zabaputil_cl_pdf=>create( compress = abap_false )->render( ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `<< /Producer (abap-util) >>` ) ).
  ENDMETHOD.

  METHOD render_without_page.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    DATA(lv_file) = lo_pdf->render( ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_pdf->page_count( ) ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Count 1 >>` ) ).
  ENDMETHOD.

  METHOD render_twice.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    DATA(lo_page) = lo_pdf->add_page( ).
    lo_page->set_font( lo_pdf->font_ttf( font_format_4( ) ) ).
    lo_page->text( x   = 10
                   y   = 20
                   val = `AB` ).
    DATA(lv_first) = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_equals( exp = lv_first
                                        act = lo_pdf->render( ) ).
  ENDMETHOD.

  METHOD render_link.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    DATA(lo_page) = lo_pdf->add_page( ).
    lo_page->link( x   = 0
                   y   = 0
                   w   = 10
                   h   = 10
                   url = `https://a.b` ).
    lo_page->link( x   = 0
                   y   = 20
                   w   = 10
                   h   = 10
                   url = `https://c.d` ).
    DATA(lv_file) = lo_pdf->render( ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Contents 3 0 R /Annots [ 4 0 R 5 0 R ] >>` ) ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = |4 0 obj\n<< /Type /Annot /Subtype /Link | ) ).
  ENDMETHOD.

  METHOD render_all_resources.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    DATA(lo_page) = lo_pdf->add_page( ).
    lo_page->set_font( lo_pdf->font( ) )->text( x   = 10
                                                y   = 20
                                                val = `a` ).
    lo_page->set_font( lo_pdf->font_ttf( font_format_4( ) ) )->text( x   = 10
                                                                    y   = 40
                                                                    val = `AB` ).
    lo_page->image( image = lo_pdf->image( image_rgb( ) )
                    x     = 10
                    y     = 60 ).
    DATA(lv_file) = lo_pdf->render( ).
    check_xref( lv_file ).
    " Helvetica is object 1, the embedded font ends with object 6, the image is 7
    cl_abap_unit_assert=>assert_true(
        has( file = lv_file
             text = `/Font << /F1 1 0 R /F2 6 0 R >> /XObject << /I1 7 0 R >> >>` ) ).
  ENDMETHOD.

  METHOD render_deflated.
    " the default: streams are deflated where CL_ABAP_GZIP is there
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( ).
    DATA(lo_page) = lo_pdf->add_page( ).
    lo_page->set_font( lo_pdf->font( ) ).
    DO 50 TIMES.
      lo_page->text( x   = 10
                     y   = sy-index * 12
                     val = `The same line again and again` ).
    ENDDO.
    DATA(lv_file) = lo_pdf->render( ).
    check_xref( lv_file ).
    TRY.
        zabaputil_cl_util_context=>gzip_compress( CONV xstring( `00` ) ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Filter /FlateDecode` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( xstrlen( lv_file ) < xstrlen(
        zabaputil_cl_util_context=>conv_get_xstring_by_string( lo_page->content( ) ) ) ) ).
  ENDMETHOD.

  METHOD bookmarks_outline.
    " in hex: the file starts with bytes that are no UTF-8
    DATA lv_hex TYPE string.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( ).
    lo_pdf->add_page( ).
    lo_pdf->add_bookmark( `One` ).
    lo_pdf->add_bookmark( title = `Deeper`
                          level = 3
                          y     = 100 ).
    lo_pdf->add_bookmark( title = `Two`
                          page  = 1 ).
    lv_hex = lo_pdf->render( ).
    " 1 the resources, 2 the page tree, 3 and 4 the pages reserved before
    " their contents 5 and 6; then the outline 7 and its entries 8 to 10
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        `<< /Type /Outlines /First 8 0 R /Last 10 0 R /Count 3 >>` ) ) ).
    " One: the last page so far, Deeper one level below (not two), Two after it
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        |/Parent 7 0 R /Next 10 0 R /First 9 0 R /Last 9 0 R /Count 1 /Dest [4 0 R /XYZ 0 841.89 null]| ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        |/Parent 8 0 R /Dest [4 0 R /XYZ 0 741.89 null]| ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        |/Parent 7 0 R /Prev 8 0 R /Dest [3 0 R /XYZ 0 841.89 null]| ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        `/Outlines 7 0 R /PageMode /UseOutlines >>` ) ) ).
  ENDMETHOD.

  METHOD link_to_page.
    DATA lv_hex TYPE string.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( )->link_page( x        = 10
                                    y        = 10
                                    w        = 100
                                    h        = 20
                                    page     = 2
                                    target_y = 41 ).
    lo_pdf->add_page( ).
    lv_hex = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        `<< /Type /Annot /Subtype /Link /Rect [10 811.89 110 831.89] /Border [0 0 0] /Dest [4 0 R /XYZ 0 800.89 null] >>` ) ) ).
    " a page the document does not have: no link
    lo_pdf = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( )->link_page( x    = 10
                                    y    = 10
                                    w    = 100
                                    h    = 20
                                    page = 5 ).
    lv_hex = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/Dest` ) ) ).
  ENDMETHOD.

  METHOD attachments.
    DATA lv_hex TYPE string.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( ).
    lo_pdf->attach( name         = `b.xml`
                    content      = zabaputil_cl_util_context=>conv_get_xstring_by_string( `<x/>` )
                    mime         = `text/xml`
                    relationship = `Alternative` ).
    lo_pdf->attach( name    = `a.txt`
                    content = zabaputil_cl_util_context=>conv_get_xstring_by_string( `hello` ) ).
    lv_hex = lo_pdf->render( ).
    " the file as it is, its media type a PDF name
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        |<< /Type /EmbeddedFile /Subtype /text#2Fxml /Params << /Size 4 /ModDate (D:| ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( |stream\n<x/>\nendstream| ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        |/UF <{ zabaputil_cl_pdf_writer=>hex_utf16( `b.xml` ) }> /AFRelationship /Alternative /EF << /F | ) ) ).
    " in the catalog: the names in their order, a.txt first
    DATA(lv_a) = zabaputil_cl_pdf_writer=>hex_utf16( `a.txt` ).
    DATA(lv_b) = zabaputil_cl_pdf_writer=>hex_utf16( `b.xml` ).
    DATA(lv_names) = zabaputil_cl_pdf_writer=>hex(
        |/Names << /EmbeddedFiles << /Names [ <{ lv_a }> 6 0 R <{ lv_b }> 8 0 R ] >> >> /AF [ 6 0 R 8 0 R ] >>| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_names ) ).
    " one name once: attached again, it is replaced
    lo_pdf->attach( name    = `a.txt`
                    content = zabaputil_cl_util_context=>conv_get_xstring_by_string( `again` ) ).
    lv_hex = lo_pdf->render( ).
    DATA(lv_octets) = zabaputil_cl_pdf_writer=>hex( `/Type /EmbeddedFile /Subtype /application#2Foctet-stream` ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = count( val = lv_hex
                                                     sub = lv_octets ) ).
  ENDMETHOD.

  METHOD invoice.
    " PDF/A-3 with the XML and the metadata of Factur-X; a page without
    " text needs no font
    DATA lv_hex TYPE string.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( ).
    lo_pdf->attach_invoice( zabaputil_cl_util_context=>conv_get_xstring_by_string( `<rsm:CrossIndustryInvoice/>` ) ).
    lv_hex = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `<pdfaid:part>3</pdfaid:part>` ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        `<fx:DocumentFileName>factur-x.xml</fx:DocumentFileName><fx:Version>1.0</fx:Version>` &&
        `<fx:ConformanceLevel>EN 16931</fx:ConformanceLevel>` ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        `<pdfaSchema:prefix>fx</pdfaSchema:prefix>` ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/AFRelationship /Alternative` ) ) ).

    " XRechnung: its own file name; MINIMUM: data beside the document
    lo_pdf = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( ).
    lo_pdf->attach_invoice( xml   = zabaputil_cl_util_context=>conv_get_xstring_by_string( `<x/>` )
                            level = `xrechnung` ).
    lv_hex = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex(
        `<fx:DocumentFileName>xrechnung.xml</fx:DocumentFileName>` ) ) ).
    lo_pdf = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( ).
    lo_pdf->attach_invoice( xml   = zabaputil_cl_util_context=>conv_get_xstring_by_string( `<x/>` )
                            level = `MINIMUM` ).
    lv_hex = lo_pdf->render( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/AFRelationship /Data` ) ) ).

    TRY.
        lo_pdf->attach_invoice( xml   = zabaputil_cl_util_context=>conv_get_xstring_by_string( `<x/>` )
                                level = `COMFORT` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD form_fields.
    DATA lv_hex TYPE string.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( compress = abap_false ).
    lo_pdf->add_page( )->field_text( name  = `name`
                                     x     = 100
                                     y     = 100
                                     w     = 200
                                     h     = 20
                                     value = `Ada` )->field_checkbox( name    = `ok`
                                                                      x       = 100
                                                                      y       = 130
                                                                      w       = 10
                                                                      checked = abap_true ).
    lv_hex = lo_pdf->render( ).
    " the form in the catalog, with its fonts
    DATA(lv_form) = zabaputil_cl_pdf_writer=>hex( `/AcroForm << /Fields [ 7 0 R 10 0 R ] /DA (/Helv 0 Tf 0 g) ` &&
                                                  `/DR << /Font << /Helv 4 0 R /ZaDb 5 0 R >> >> >>` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_form ) ).
    " a text field: its name and value in UTF-16, its appearance with the value
    DATA(lv_name) = zabaputil_cl_pdf_writer=>hex_utf16( `name` ).
    DATA(lv_value) = zabaputil_cl_pdf_writer=>hex_utf16( `Ada` ).
    DATA(lv_widget) = zabaputil_cl_pdf_writer=>hex(
        |<< /Type /Annot /Subtype /Widget /F 4 /Rect [100 721.89 300 741.89] /T <{ lv_name }> | &&
        |/FT /Tx /V <{ lv_value }> /DA (/Helv 10 Tf 0 g)| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_widget ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `Tf 0 g 2 7 Td <416461> Tj ET Q EMC` ) ) ).
    " a check box: on, two appearances
    DATA(lv_box) = zabaputil_cl_pdf_writer=>hex( `/FT /Btn /V /Yes /AS /Yes /DA (/ZaDb 0 Tf 0 g) /MK << /CA (4) /BC [0.5] >> ` &&
                                                 `/AP << /N << /Yes 8 0 R /Off 9 0 R >> >> >>` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_box ) ).
    " the widgets are the annotations of the page
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/Annots [ 7 0 R 10 0 R ]` ) ) ).
  ENDMETHOD.

ENDCLASS.

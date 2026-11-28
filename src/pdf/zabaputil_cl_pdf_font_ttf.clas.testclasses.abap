CLASS ltcl_font_ttf DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS name_from_table         FOR TESTING.
    METHODS glyph_format_4          FOR TESTING.
    METHODS glyph_format_12         FOR TESTING.
    METHODS encode_glyphs           FOR TESTING.
    METHODS encode_missing          FOR TESTING.
    METHODS width_of_text           FOR TESTING.
    METHODS ascent_descent          FOR TESTING.
    METHODS write_objects           FOR TESTING.
    METHODS write_used_glyphs_only  FOR TESTING.
    METHODS write_astral            FOR TESTING.
    METHODS subset_used_glyphs      FOR TESTING.
    METHODS subset_name_and_file    FOR TESTING.
    METHODS not_a_font              FOR TESTING.
    METHODS postscript_outlines     FOR TESTING.
    METHODS collection              FOR TESTING.
    METHODS truncated               FOR TESTING.

    " two fonts built by hand for this test, 1000 units per em, ascender
    " 800, descender -200: the missing glyph (500 wide), A (600), B (700)
    " - the second one with U+1F600 (800) and a format 12 character map
    METHODS font_format_4
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS font_format_12
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS font
      IMPORTING
        content       TYPE xstring
        subset        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_font.

    METHODS rendered
      IMPORTING
        font          TYPE REF TO zabaputil_if_pdf_font
      RETURNING
        VALUE(result) TYPE string.

    METHODS num
      IMPORTING
        val           TYPE f
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS ltcl_font_ttf IMPLEMENTATION.

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

  METHOD font_format_12.

    DATA lv_hex TYPE string.

    lv_hex = `00010000000A0080000300204F532F32652660B7000000AC00000060636D6170001EED6C0000010C0000005C676C79660DC7` &&
             `1084000001680000006668656164620C43A1000001D000000036686865610610028F0000020800000024686D74780A2800C8` &&
             `0000022C000000106C6F6361004400220000023C0000000A6D6178700006000500000248000000206E616D65062715D20000` &&
             `02680000006E706F7374FF9F0032000002D80000002000020258019000050000028A02BC0000008C028A02BC000001E00032` &&
             `00FA00000000000000000000000000000001000000000000000000000000544553540040004100420320FF38005A032000C8` &&
             `000000010000000001F402BC0000002000010000000200030001000000140003000A00000034000400200000000400020000` &&
             `00020042FFFF00000041FFFFFFC0000100000000000C00000000002800000000000000020000004100000042000000010001` &&
             `F6000001F60000000003000100320000022602BC00030000010101010032000001F40000000002BC0000FD44000100320000` &&
             `028A02BC00030000010101010032000002580000000002BC0000FD4400010064006402BC0258000300000101010100640000` &&
             `02580000006401F40000FE0C00000001000000010000CED7206B5F0F3CF5000B03E800000000000000000000000000000000` &&
             `0032000002BC02BC000000080002000000000000000100000320FF38000003200032003202BC000100000000000000000000` &&
             `00000000000401F400320258003202BC00320320003200000000001100220033000000010000000400040001000000000001` &&
             `000000000000000000000000000000000000000400360003000104090001000800000003000104090002000E000800030001` &&
             `040900040012001600030001040900060010002800540065007300740052006500670075006C006100720054006500730074` &&
             `00200046006F006E007400540065007300740046006F006E007400000003000000000000FF9C003200000000000000000000` &&
             `00000000000000000000`.
    result = lv_hex.

  ENDMETHOD.

  METHOD font.
    result = NEW zabaputil_cl_pdf_font_ttf( content  = content
                                            resource = `F1`
                                            subset   = subset ).
  ENDMETHOD.

  METHOD rendered.
    " the file with the objects of the font, not deflated, in hex
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( abap_false ).
    result = lo_writer->render( font->write( lo_writer ) ).
  ENDMETHOD.

  METHOD num.
    result = zabaputil_cl_pdf_writer=>num( val ).
  ENDMETHOD.

  METHOD name_from_table.
    DATA(lo_font) = NEW zabaputil_cl_pdf_font_ttf( content  = font_format_4( )
                                                   resource = `F3` ).
    cl_abap_unit_assert=>assert_equals( exp = `TestFont` act = lo_font->name( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `F3` act = lo_font->zabaputil_if_pdf_font~resource( ) ).
  ENDMETHOD.

  METHOD glyph_format_4.
    DATA(lo_font) = NEW zabaputil_cl_pdf_font_ttf( content  = font_format_4( )
                                                   resource = `F1` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_font->glyph( 65 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_font->glyph( 66 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 64 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 67 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 8364 ) ).
    " outside the Basic Multilingual Plane a format 4 map has nothing
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 128512 ) ).
  ENDMETHOD.

  METHOD glyph_format_12.
    DATA(lo_font) = NEW zabaputil_cl_pdf_font_ttf( content  = font_format_12( )
                                                   resource = `F1` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_font->glyph( 65 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_font->glyph( 66 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lo_font->glyph( 128512 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 67 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 128513 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 32 ) ).
  ENDMETHOD.

  METHOD encode_glyphs.
    cl_abap_unit_assert=>assert_equals( exp = `<000100020001>` act = font( font_format_4( ) )->encode( `ABA` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `<>` act = font( font_format_4( ) )->encode( `` ) ).
  ENDMETHOD.

  METHOD encode_missing.
    " a character the font does not have is the missing glyph, number 0
    cl_abap_unit_assert=>assert_equals( exp = `<000100000002>` act = font( font_format_4( ) )->encode( `AxB` ) ).
  ENDMETHOD.

  METHOD width_of_text.
    " A 600 + B 700, and 500 for the missing glyph
    cl_abap_unit_assert=>assert_equals( exp = `13`
                                        act = num( font( font_format_4( ) )->width( val  = `AB`
                                                                                    size = 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `11`
                                        act = num( font( font_format_4( ) )->width( val  = `Ax`
                                                                                    size = 10 ) ) ).
    DATA(lv_text) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `F09F9880` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `8`
                                        act = num( font( font_format_12( ) )->width( val  = lv_text
                                                                                     size = 10 ) ) ).
  ENDMETHOD.

  METHOD ascent_descent.
    cl_abap_unit_assert=>assert_equals( exp = `8` act = num( font( font_format_4( ) )->ascent( 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2` act = num( font( font_format_4( ) )->descent( 10 ) ) ).
  ENDMETHOD.

  METHOD write_objects.
    DATA(lo_font) = font( font_format_4( ) ).
    lo_font->encode( `AB` ).
    DATA(lv_hex) = rendered( lo_font ).

    " the file, its descriptor, the font of the glyphs, the map back to
    " Unicode and the composite font - the last one is the font dictionary
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex( |1 0 obj\n<< /Length1 668 /Length 668 >>\nstream\n| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        `<< /Type /FontDescriptor /FontName /TestFont /Flags 4 /FontBBox [50 0 700 700] ` &&
        `/ItalicAngle 0 /Ascent 800 /Descent -200 /CapHeight 700 /StemV 80 /FontFile2 1 0 R >>` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        `/FontDescriptor 2 0 R /DW 1000 /W [ 1 [600] 2 [700] ] /CIDToGIDMap /Identity >>` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex( |2 beginbfchar\n<0001> <0041>\n<0002> <0042>\nendbfchar\n| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        |5 0 obj\n<< /Type /Font /Subtype /Type0 /BaseFont /TestFont /Encoding /Identity-H | &&
        |/DescendantFonts [3 0 R] /ToUnicode 4 0 R >>| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    " the font file itself is in the PDF, byte for byte
    DATA lv_file TYPE string.
    lv_file = font_format_4( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_file ) ).
  ENDMETHOD.

  METHOD write_used_glyphs_only.
    " B twice, a missing character and no A: one width for B, one for
    " the missing glyph - and the missing glyph maps to no character
    DATA(lo_font) = font( font_format_4( ) ).
    lo_font->encode( `BxB` ).
    DATA(lv_hex) = rendered( lo_font ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex( `/W [ 0 [500] 2 [700] ]` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex( |1 beginbfchar\n<0002> <0042>\nendbfchar\n| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
  ENDMETHOD.

  METHOD write_astral.
    DATA(lv_text) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `F09F9880` ) ).
    DATA(lo_font) = font( font_format_12( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `<0003>` act = lo_font->encode( lv_text ) ).
    DATA(lv_hex) = rendered( lo_font ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex( |<0003> <D83DDE00>\n| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex( `/W [ 3 [800] ]` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
  ENDMETHOD.

  METHOD subset_used_glyphs.
    " A only: the missing glyph and A are kept, B is empty - and the file
    " is a font that reads as the original did
    DATA lo_ttf TYPE REF TO zabaputil_cl_pdf_font_ttf.
    lo_ttf ?= font( content = font_format_4( )
                    subset  = abap_true ).
    lo_ttf->zabaputil_if_pdf_font~encode( `A` ).
    DATA(lv_file) = lo_ttf->file( ).
    cl_abap_unit_assert=>assert_true( xsdbool( xstrlen( lv_file ) < xstrlen( font_format_4( ) ) ) ).

    DATA(lo_again) = NEW zabaputil_cl_pdf_font_ttf( content  = lv_file
                                                    resource = `F2` ).
    cl_abap_unit_assert=>assert_equals( exp = `TestFont` act = lo_again->name( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_again->glyph( 65 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_again->glyph( 66 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `6`
                                        act = num( lo_again->zabaputil_if_pdf_font~width( val  = `A`
                                                                                          size = 10 ) ) ).
    " B has its glyph number still, but nothing to draw: its subset is
    " the one of A without B
    lo_ttf ?= font( content = font_format_4( )
                    subset  = abap_true ).
    lo_ttf->zabaputil_if_pdf_font~encode( `AB` ).
    cl_abap_unit_assert=>assert_true( xsdbool( xstrlen( lo_ttf->file( ) ) > xstrlen( lv_file ) ) ).
    " the 4-byte aligned tables: the file is a multiple of four
    cl_abap_unit_assert=>assert_equals( exp = 0 act = xstrlen( lv_file ) MOD 4 ).
  ENDMETHOD.

  METHOD subset_name_and_file.
    " a subset has a name of its own: six capitals, a plus, the name
    DATA(lo_font) = font( content = font_format_4( )
                          subset  = abap_true ).
    lo_font->encode( `A` ).
    DATA(lv_hex) = rendered( lo_font ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `+TestFont /Encoding /Identity-H` ) ) ).
    cl_abap_unit_assert=>assert_false(
        xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/BaseFont /TestFont` ) ) ).
    " without subsetting the file is embedded as it is
    DATA lo_ttf TYPE REF TO zabaputil_cl_pdf_font_ttf.
    lo_ttf ?= font( font_format_4( ) ).
    cl_abap_unit_assert=>assert_equals( exp = font_format_4( ) act = lo_ttf->file( ) ).
  ENDMETHOD.

  METHOD not_a_font.
    TRY.
        font( CONV xstring( `255044462D312E340A25E2E3CFD30A00` ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        font( CONV xstring( `0001` ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD postscript_outlines.
    " OTTO: an OpenType font with CFF outlines
    TRY.
        font( CONV xstring( `4F54544F000A008000030020` ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error INTO DATA(lx).
        cl_abap_unit_assert=>assert_true( xsdbool( lx->get_text( ) CS `.otf` ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD collection.
    TRY.
        font( CONV xstring( `74746366000200000000000200000014` ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD truncated.
    " the directory is there, the tables behind it are not
    DATA(lv_font) = font_format_4( ).
    DATA(lv_head) = lv_font(200).
    TRY.
        font( lv_head ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

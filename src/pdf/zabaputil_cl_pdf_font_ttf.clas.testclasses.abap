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
    CREATE OBJECT result TYPE zabaputil_cl_pdf_font_ttf EXPORTING content = content resource = `F1`.
  ENDMETHOD.

  METHOD rendered.
    " the file with the objects of the font, not deflated, in hex
    DATA lo_writer TYPE REF TO zabaputil_cl_pdf_writer.
    CREATE OBJECT lo_writer TYPE zabaputil_cl_pdf_writer EXPORTING COMPRESS = abap_false.
    result = lo_writer->render( font->write( lo_writer ) ).
  ENDMETHOD.

  METHOD num.
    result = zabaputil_cl_pdf_writer=>num( val ).
  ENDMETHOD.

  METHOD name_from_table.
    DATA lo_font TYPE REF TO zabaputil_cl_pdf_font_ttf.
    CREATE OBJECT lo_font TYPE zabaputil_cl_pdf_font_ttf EXPORTING content = font_format_4( ) resource = `F3`.
    cl_abap_unit_assert=>assert_equals( exp = `TestFont` act = lo_font->name( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `F3` act = lo_font->zabaputil_if_pdf_font~resource( ) ).
  ENDMETHOD.

  METHOD glyph_format_4.
    DATA lo_font TYPE REF TO zabaputil_cl_pdf_font_ttf.
    CREATE OBJECT lo_font TYPE zabaputil_cl_pdf_font_ttf EXPORTING content = font_format_4( ) resource = `F1`.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_font->glyph( 65 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_font->glyph( 66 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 64 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 67 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 8364 ) ).
    " outside the Basic Multilingual Plane a format 4 map has nothing
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_font->glyph( 128512 ) ).
  ENDMETHOD.

  METHOD glyph_format_12.
    DATA lo_font TYPE REF TO zabaputil_cl_pdf_font_ttf.
    CREATE OBJECT lo_font TYPE zabaputil_cl_pdf_font_ttf EXPORTING content = font_format_12( ) resource = `F1`.
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
    DATA temp1 TYPE xstring.
    DATA lv_text TYPE string.
    " A 600 + B 700, and 500 for the missing glyph
    cl_abap_unit_assert=>assert_equals( exp = `13`
                                        act = num( font( font_format_4( ) )->width( val  = `AB`
                                                                                    size = 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `11`
                                        act = num( font( font_format_4( ) )->width( val  = `Ax`
                                                                                    size = 10 ) ) ).
    
    temp1 = `F09F9880`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp1 ).
    cl_abap_unit_assert=>assert_equals( exp = `8`
                                        act = num( font( font_format_12( ) )->width( val  = lv_text
                                                                                     size = 10 ) ) ).
  ENDMETHOD.

  METHOD ascent_descent.
    cl_abap_unit_assert=>assert_equals( exp = `8` act = num( font( font_format_4( ) )->ascent( 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2` act = num( font( font_format_4( ) )->descent( 10 ) ) ).
  ENDMETHOD.

  METHOD write_objects.
    DATA lo_font TYPE REF TO zabaputil_if_pdf_font.
    DATA lv_hex TYPE string.
    DATA lv_expect TYPE string.
    DATA temp1 TYPE xsdboolean.
    DATA temp2 TYPE xsdboolean.
    DATA temp3 TYPE xsdboolean.
    DATA temp4 TYPE xsdboolean.
    DATA temp5 TYPE xsdboolean.
    DATA lv_file TYPE string.
    DATA temp6 TYPE xsdboolean.
    lo_font = font( font_format_4( ) ).
    lo_font->encode( `AB` ).
    
    lv_hex = rendered( lo_font ).

    " the file, its descriptor, the font of the glyphs, the map back to
    " Unicode and the composite font - the last one is the font dictionary
    
    lv_expect = zabaputil_cl_pdf_writer=>hex( |1 0 obj\n<< /Length1 668 /Length 668 >>\nstream\n| ).
    
    temp1 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp1 ).
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        `<< /Type /FontDescriptor /FontName /TestFont /Flags 4 /FontBBox [50 0 700 700] ` &&
        `/ItalicAngle 0 /Ascent 800 /Descent -200 /CapHeight 700 /StemV 80 /FontFile2 1 0 R >>` ).
    
    temp2 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp2 ).
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        `/FontDescriptor 2 0 R /DW 1000 /W [ 1 [600] 2 [700] ] /CIDToGIDMap /Identity >>` ).
    
    temp3 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp3 ).
    lv_expect = zabaputil_cl_pdf_writer=>hex( |2 beginbfchar\n<0001> <0041>\n<0002> <0042>\nendbfchar\n| ).
    
    temp4 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp4 ).
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        |5 0 obj\n<< /Type /Font /Subtype /Type0 /BaseFont /TestFont /Encoding /Identity-H | &&
        |/DescendantFonts [3 0 R] /ToUnicode 4 0 R >>| ).
    
    temp5 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp5 ).
    " the font file itself is in the PDF, byte for byte
    
    lv_file = font_format_4( ).
    
    temp6 = boolc( lv_hex CS lv_file ).
    cl_abap_unit_assert=>assert_true( temp6 ).
  ENDMETHOD.

  METHOD write_used_glyphs_only.
    " B twice, a missing character and no A: one width for B, one for
    " the missing glyph - and the missing glyph maps to no character
    DATA lo_font TYPE REF TO zabaputil_if_pdf_font.
    DATA lv_hex TYPE string.
    DATA lv_expect TYPE string.
    DATA temp7 TYPE xsdboolean.
    DATA temp8 TYPE xsdboolean.
    lo_font = font( font_format_4( ) ).
    lo_font->encode( `BxB` ).
    
    lv_hex = rendered( lo_font ).
    
    lv_expect = zabaputil_cl_pdf_writer=>hex( `/W [ 0 [500] 2 [700] ]` ).
    
    temp7 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp7 ).
    lv_expect = zabaputil_cl_pdf_writer=>hex( |1 beginbfchar\n<0002> <0042>\nendbfchar\n| ).
    
    temp8 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp8 ).
  ENDMETHOD.

  METHOD write_astral.
    DATA temp2 TYPE xstring.
    DATA lv_text TYPE string.
    DATA lo_font TYPE REF TO zabaputil_if_pdf_font.
    DATA lv_hex TYPE string.
    DATA lv_expect TYPE string.
    DATA temp9 TYPE xsdboolean.
    DATA temp10 TYPE xsdboolean.
    temp2 = `F09F9880`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp2 ).
    
    lo_font = font( font_format_12( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `<0003>` act = lo_font->encode( lv_text ) ).
    
    lv_hex = rendered( lo_font ).
    
    lv_expect = zabaputil_cl_pdf_writer=>hex( |<0003> <D83DDE00>\n| ).
    
    temp9 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp9 ).
    lv_expect = zabaputil_cl_pdf_writer=>hex( `/W [ 3 [800] ]` ).
    
    temp10 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp10 ).
  ENDMETHOD.

  METHOD not_a_font.
        DATA temp3 TYPE xstring.
        DATA temp4 TYPE xstring.
    TRY.
        
        temp3 = `255044462D312E340A25E2E3CFD30A00`.
        font( temp3 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        
        temp4 = `0001`.
        font( temp4 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD postscript_outlines.
        DATA temp5 TYPE xstring.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA temp11 TYPE xsdboolean.
    " OTTO: an OpenType font with CFF outlines
    TRY.
        
        temp5 = `4F54544F000A008000030020`.
        font( temp5 ).
        cl_abap_unit_assert=>fail( ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        
        temp11 = boolc( lx->get_text( ) CS `.otf` ).
        cl_abap_unit_assert=>assert_true( temp11 ).
    ENDTRY.
  ENDMETHOD.

  METHOD collection.
        DATA temp6 TYPE xstring.
    TRY.
        
        temp6 = `74746366000200000000000200000014`.
        font( temp6 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD truncated.
    " the directory is there, the tables behind it are not
    DATA lv_font TYPE xstring.
    DATA lv_head TYPE xstring.
    lv_font = font_format_4( ).
    
    lv_head = lv_font(200).
    TRY.
        font( lv_head ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

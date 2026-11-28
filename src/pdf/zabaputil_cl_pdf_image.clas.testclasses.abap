CLASS ltcl_image DEFINITION DEFERRED.
CLASS zabaputil_cl_pdf_image DEFINITION LOCAL FRIENDS ltcl_image.

CLASS ltcl_image DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS jpeg_size               FOR TESTING.
    METHODS jpeg_write              FOR TESTING.
    METHODS jpeg_gray               FOR TESTING.
    METHODS png_size                FOR TESTING.
    METHODS png_rgb_write           FOR TESTING.
    METHODS png_palette_write       FOR TESTING.
    METHODS png_gray_key            FOR TESTING.
    METHODS png_rgb_key             FOR TESTING.
    METHODS png_alpha_split         FOR TESTING.
    METHODS png_alpha_split_16      FOR TESTING.
    METHODS png_alpha_write         FOR TESTING.
    METHODS png_interlaced          FOR TESTING.
    METHODS png_without_data        FOR TESTING.
    METHODS not_an_image            FOR TESTING.
    METHODS truncated               FOR TESTING.

    " tiny files made for this test. JPEG: 4 x 3 RGB and 5 x 2 grey. PNG,
    " every row with filter 0: 2 x 2 RGB in two IDAT chunks, 2 x 2 RGBA,
    " 2 x 1 grey with alpha in 16 bit, 2 x 2 palette of four colours with
    " the second one transparent, 2 x 1 grey and 1 x 1 RGB with a colour
    " key, 1 x 1 interlaced
    METHODS image_jpeg
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_jpeggray
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_rgb
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_rgba
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_ga16
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_pal
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_gray
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_rgbkey
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS image_inter
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS image
      IMPORTING
        content       TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_image.

    METHODS rendered
      IMPORTING
        content       TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

    METHODS has_gzip
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS ltcl_image IMPLEMENTATION.

  METHOD image_jpeg.

    DATA lv_hex TYPE string.

    lv_hex = `FFD8FFE000104A46494600010100000100010000FFDB0043001B12141714111B1716171E1C1B2028422B28252528513A3D30` &&
             `42605565645F555D5B6A7899816A7190735B5D85B586909EA3ABADAB6780BCC9BAA6C799A8ABA4FFDB0043011C1E1E282328` &&
             `4E2B2B4EA46E5D6EA4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4` &&
             `A4A4A4A4A4A4A4A4FFC00011080003000403012200021101031101FFC4001500010100000000000000000000000000000004` &&
             `FFC40014100100000000000000000000000000000000FFC4001501010100000000000000000000000000000405FFC4001411` &&
             `0100000000000000000000000000000000FFDA000C03010002110311003F008401D61FFFD9`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_jpeggray.

    DATA lv_hex TYPE string.

    lv_hex = `FFD8FFE000104A46494600010100000100010000FFDB0043001B12141714111B1716171E1C1B2028422B28252528513A3D30` &&
             `42605565645F555D5B6A7899816A7190735B5D85B586909EA3ABADAB6780BCC9BAA6C799A8ABA4FFC0000B08000200050101` &&
             `1100FFC40014000100000000000000000000000000000004FFC40014100100000000000000000000000000000000FFDA0008` &&
             `010100003F0023FFD9`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_rgb.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D4948445200000002000000020802000000FDD49A73000000054944415478DA63F8CF51E86AD7` &&
             `0000000D49444154C0C000C20CFF8100001FEE05FB61FE8DCC0000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_rgba.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D494844520000000200000002080600000072B60D24000000164944415478DA63F8CFC0F01F08` &&
             `1B1840B480828103003721051D77F1B9990000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_ga16.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D49484452000000020000000110040000000EBB6B42000000114944415478DA631032F9FF7FF5` &&
             `59860600128D043D81E0DE150000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_pal.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D49484452000000020000000202030000000FD8E5B70000000C504C5445FF000000FF000000FF` &&
             `FFFF00D6028F7B0000000274524E53FF00E5B7304A0000000C4944415478DA639060D8000000FC00C963EE414E0000000049` &&
             `454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_gray.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D4948445200000002000000010800000000D14920560000000274524E5300C8E32C87BA000000` &&
             `0B4944415478DA636038010000CB00C9FA6CB48B0000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_rgbkey.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D4948445200000001000000010802000000907753DE0000000674524E53000100020003C94BAB` &&
             `F50000000C4944415478DA636064620600000E0007E99237D40000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image_inter.

    DATA lv_hex TYPE string.

    lv_hex = `89504E470D0A1A0A0000000D4948445200000001000000010802000001E77063480000000C4944415478DA63606462060000` &&
             `0E0007E99237D40000000049454E44AE426082`.
    result = lv_hex.

  ENDMETHOD.

  METHOD image.
    result = NEW #( content  = content
                    resource = `I1` ).
  ENDMETHOD.

  METHOD rendered.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( abap_false ).
    result = lo_writer->render( image( content )->write( lo_writer ) ).
  ENDMETHOD.

  METHOD has_gzip.
    TRY.
        zabaputil_cl_util_context=>gzip_compress( CONV xstring( `00` ) ).
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD jpeg_size.
    DATA(lo_image) = image( image_jpeg( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `JPEG` act = lo_image->format( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lo_image->width( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lo_image->height( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `I1` act = lo_image->resource( ) ).
  ENDMETHOD.

  METHOD jpeg_write.
    DATA(lv_hex) = rendered( image_jpeg( ) ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex(
        `<< /Type /XObject /Subtype /Image /Width 4 /Height 3 /ColorSpace /DeviceRGB ` &&
        |/BitsPerComponent 8 /Filter /DCTDecode /Length 287 >>\nstream\n| ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    " the file goes in as it is
    DATA lv_file TYPE string.
    lv_file = image_jpeg( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_file ) ).
  ENDMETHOD.

  METHOD jpeg_gray.
    DATA(lo_image) = image( image_jpeggray( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = lo_image->width( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_image->height( ) ).
    DATA(lv_hex) = rendered( image_jpeggray( ) ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex( `/ColorSpace /DeviceGray /BitsPerComponent 8 /Filter /DCTDecode` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
  ENDMETHOD.

  METHOD png_size.
    DATA(lo_image) = image( image_rgb( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `PNG` act = lo_image->format( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_image->width( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_image->height( ) ).
    lo_image = image( image_ga16( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_image->width( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_image->height( ) ).
  ENDMETHOD.

  METHOD png_rgb_write.
    " the pixel data of both IDAT chunks, joined and not unpacked
    DATA(lv_hex) = rendered( image_rgb( ) ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex(
        `<< /Type /XObject /Subtype /Image /Width 2 /Height 2 /ColorSpace /DeviceRGB ` &&
        `/BitsPerComponent 8 /Filter /FlateDecode ` &&
        `/DecodeParms << /Predictor 15 /Colors 3 /BitsPerComponent 8 /Columns 2 >> /Length ` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/SMask` ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_hex CS zabaputil_cl_pdf_writer=>hex( `/Mask` ) ) ).

    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.
    " two rows: filter byte 0, then red and green, blue and white
    DATA(lo_image) = image( image_rgb( ) ).
    DATA lv_idat TYPE xstring.
    lv_idat = lo_image->mv_idat.
    cl_abap_unit_assert=>assert_equals( exp = CONV xstring( `00FF000000FF00000000FFFFFFFF` )
                                        act = zabaputil_cl_util_context=>zlib_decompress( lv_idat ) ).
  ENDMETHOD.

  METHOD png_palette_write.
    DATA(lv_hex) = rendered( image_pal( ) ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex(
        `/ColorSpace [/Indexed /DeviceRGB 3 <FF000000FF000000FFFFFF00>] /BitsPerComponent 2 ` &&
        `/Filter /FlateDecode /DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent 2 /Columns 2 >> ` &&
        `/Mask [1 1] /Length ` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
  ENDMETHOD.

  METHOD png_gray_key.
    DATA(lv_hex) = rendered( image_gray( ) ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex( `/ColorSpace /DeviceGray /BitsPerComponent 8 ` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex( `/Columns 2 >> /Mask [200 200] /Length ` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
  ENDMETHOD.

  METHOD png_rgb_key.
    DATA(lv_hex) = rendered( image_rgbkey( ) ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex( `/Columns 1 >> /Mask [1 1 2 2 3 3 ] /Length ` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
  ENDMETHOD.

  METHOD png_alpha_split.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.
    " rows of red (opaque) and green (half), blue (clear) and 10 20 30 (40)
    DATA lv_color TYPE xstring.
    DATA lv_alpha TYPE xstring.
    image( image_rgba( ) )->png_split_alpha( IMPORTING color = lv_color
                                                       alpha = lv_alpha ).
    cl_abap_unit_assert=>assert_equals( exp = CONV xstring( `00FF000000FF00000000FF102030` )
                                        act = lv_color ).
    cl_abap_unit_assert=>assert_equals( exp = CONV xstring( `00FF80000040` )
                                        act = lv_alpha ).
  ENDMETHOD.

  METHOD png_alpha_split_16.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.
    " one row of two pixels, grey and alpha two bytes each
    DATA lv_color TYPE xstring.
    DATA lv_alpha TYPE xstring.
    image( image_ga16( ) )->png_split_alpha( IMPORTING color = lv_color
                                                       alpha = lv_alpha ).
    cl_abap_unit_assert=>assert_equals( exp = CONV xstring( `001234ABCD` )
                                        act = lv_color ).
    cl_abap_unit_assert=>assert_equals( exp = CONV xstring( `00FFFF0080` )
                                        act = lv_alpha ).
  ENDMETHOD.

  METHOD png_alpha_write.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.
    " the transparency first, as a grey image the colour image points at
    DATA(lv_hex) = rendered( image_rgba( ) ).
    DATA(lv_expect) = zabaputil_cl_pdf_writer=>hex(
        |1 0 obj\n<< /Type /XObject /Subtype /Image /Width 2 /Height 2 /ColorSpace /DeviceGray | &&
        `/BitsPerComponent 8 /Filter /FlateDecode ` &&
        `/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent 8 /Columns 2 >> /Length ` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        `/DecodeParms << /Predictor 15 /Colors 3 /BitsPerComponent 8 /Columns 2 >> /SMask 1 0 R /Length ` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_hex CS lv_expect ) ).
  ENDMETHOD.

  METHOD png_interlaced.
    TRY.
        image( image_inter( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error INTO DATA(lx).
        cl_abap_unit_assert=>assert_true( xsdbool( lx->get_text( ) CS `interlaced` ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD png_without_data.
    " the signature and the header chunk, nothing after it
    DATA(lv_file) = image_rgb( ).
    DATA(lv_head) = lv_file(33).
    TRY.
        image( lv_head ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD not_an_image.
    TRY.
        image( CONV xstring( `474946383961010001000000` ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    DATA lv_empty TYPE xstring.
    TRY.
        image( lv_empty ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD truncated.
    " a JPEG that ends before its frame header has no size
    DATA(lv_file) = image_jpeg( ).
    DATA(lv_head) = lv_file(20).
    TRY.
        image( lv_head ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

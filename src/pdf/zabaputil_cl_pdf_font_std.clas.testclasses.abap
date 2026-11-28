CLASS ltcl_font_std DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS unknown_name            FOR TESTING.
    METHODS every_name              FOR TESTING.
    METHODS resource_name           FOR TESTING.
    METHODS encode_ascii            FOR TESTING.
    METHODS encode_latin            FOR TESTING.
    METHODS encode_outside          FOR TESTING.
    METHODS width_helvetica         FOR TESTING.
    METHODS width_bold_is_wider     FOR TESTING.
    METHODS width_times             FOR TESTING.
    METHODS width_courier           FOR TESTING.
    METHODS width_latin             FOR TESTING.
    METHODS ascent_descent          FOR TESTING.
    METHODS winansi_codes           FOR TESTING.
    METHODS write_object            FOR TESTING.

    METHODS font
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_font.

    METHODS num
      IMPORTING
        val           TYPE f
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS ltcl_font_std IMPLEMENTATION.

  METHOD font.
    CREATE OBJECT result TYPE zabaputil_cl_pdf_font_std EXPORTING name = name resource = `F1`.
  ENDMETHOD.

  METHOD num.
    result = zabaputil_cl_pdf_writer=>num( val ).
  ENDMETHOD.

  METHOD unknown_name.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA temp1 TYPE xsdboolean.
    TRY.
        font( `Arial` ).
        cl_abap_unit_assert=>fail( ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        
        temp1 = boolc( lx->get_text( ) CS `Arial` ).
        cl_abap_unit_assert=>assert_true( temp1 ).
    ENDTRY.
  ENDMETHOD.

  METHOD every_name.
    DATA lt_name TYPE string_table.
    DATA temp1 TYPE string_table.
    DATA lv_name LIKE LINE OF lt_name.
      DATA lo_font TYPE REF TO zabaputil_if_pdf_font.
      DATA temp2 TYPE xsdboolean.
      DATA temp3 TYPE xsdboolean.
    CLEAR temp1.
    INSERT `Helvetica` INTO TABLE temp1.
    INSERT `Helvetica-Bold` INTO TABLE temp1.
    INSERT `Helvetica-Oblique` INTO TABLE temp1.
    INSERT `Helvetica-BoldOblique` INTO TABLE temp1.
    INSERT `Times-Roman` INTO TABLE temp1.
    INSERT `Times-Bold` INTO TABLE temp1.
    INSERT `Times-Italic` INTO TABLE temp1.
    INSERT `Times-BoldItalic` INTO TABLE temp1.
    INSERT `Courier` INTO TABLE temp1.
    INSERT `Courier-Bold` INTO TABLE temp1.
    INSERT `Courier-Oblique` INTO TABLE temp1.
    INSERT `Courier-BoldOblique` INTO TABLE temp1.
    lt_name = temp1.
    
    LOOP AT lt_name INTO lv_name.
      
      lo_font = font( lv_name ).
      " every font has a blank and a capital letter with a width
      
      temp2 = boolc( lo_font->width( val = ` ` size = 10 ) > 0 ).
      cl_abap_unit_assert=>assert_true( temp2 ).
      
      temp3 = boolc( lo_font->width( val = `M` size = 10 ) > 5 ).
      cl_abap_unit_assert=>assert_true( temp3 ).
    ENDLOOP.
  ENDMETHOD.

  METHOD resource_name.
    DATA lo_font TYPE REF TO zabaputil_if_pdf_font.
    CREATE OBJECT lo_font TYPE zabaputil_cl_pdf_font_std EXPORTING name = `Courier` resource = `F7`.
    cl_abap_unit_assert=>assert_equals( exp = `F7` act = lo_font->resource( ) ).
  ENDMETHOD.

  METHOD encode_ascii.
    cl_abap_unit_assert=>assert_equals( exp = `<417A2028295C>` act = font( `Helvetica` )->encode( `Az ()\` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `<>` act = font( `Helvetica` )->encode( `` ) ).
  ENDMETHOD.

  METHOD encode_latin.
    " a-umlaut, sharp s, the euro sign, an en dash, a low double quote
    DATA temp3 TYPE xstring.
    DATA lv_text TYPE string.
    temp3 = `C3A4C39FE282ACE28093E2809E`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_xstring(
                        temp3 ).
    cl_abap_unit_assert=>assert_equals( exp = `<E4DF809684>` act = font( `Times-Roman` )->encode( lv_text ) ).
  ENDMETHOD.

  METHOD encode_outside.
    " a Greek alpha and a Cyrillic letter are not in WinAnsiEncoding
    DATA temp4 TYPE xstring.
    DATA lv_text TYPE string.
    temp4 = `41CEB1D0964A`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp4 ).
    cl_abap_unit_assert=>assert_equals( exp = `<413F3F4A>` act = font( `Helvetica` )->encode( lv_text ) ).
  ENDMETHOD.

  METHOD width_helvetica.
    " A 667, Hello = 722 + 556 + 222 + 222 + 556 = 2278
    cl_abap_unit_assert=>assert_equals( exp = `667`
                                        act = num( font( `Helvetica` )->width( val  = `A`
                                                                               size = 1000 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `22.78`
                                        act = num( font( `Helvetica` )->width( val  = `Hello`
                                                                               size = 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `22.78`
                                        act = num( font( `Helvetica-Oblique` )->width( val  = `Hello`
                                                                                       size = 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `0`
                                        act = num( font( `Helvetica` )->width( val  = ``
                                                                               size = 10 ) ) ).
  ENDMETHOD.

  METHOD width_bold_is_wider.
    " Hello = 722 + 556 + 278 + 278 + 611 = 2445
    cl_abap_unit_assert=>assert_equals( exp = `24.45`
                                        act = num( font( `Helvetica-Bold` )->width( val  = `Hello`
                                                                                    size = 10 ) ) ).
  ENDMETHOD.

  METHOD width_times.
    " the blank is 250 in every Times cut; Hello = 722 + 444 + 278 + 278 + 500 = 2222
    cl_abap_unit_assert=>assert_equals( exp = `2.5`
                                        act = num( font( `Times-Roman` )->width( val  = ` `
                                                                                 size = 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2.5`
                                        act = num( font( `Times-BoldItalic` )->width( val  = ` `
                                                                                      size = 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `22.22`
                                        act = num( font( `Times-Roman` )->width( val  = `Hello`
                                                                                 size = 10 ) ) ).
  ENDMETHOD.

  METHOD width_courier.
    cl_abap_unit_assert=>assert_equals( exp = `18`
                                        act = num( font( `Courier` )->width( val  = `iW.`
                                                                             size = 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `18`
                                        act = num( font( `Courier-BoldOblique` )->width( val  = `iW.`
                                                                                         size = 10 ) ) ).
  ENDMETHOD.

  METHOD width_latin.
    " the euro sign is 556 wide in Helvetica, a-umlaut as wide as a
    DATA temp5 TYPE xstring.
    DATA lv_text TYPE string.
    temp5 = `E282ACC3A4`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp5 ).
    cl_abap_unit_assert=>assert_equals( exp = `11.12`
                                        act = num( font( `Helvetica` )->width( val  = lv_text
                                                                               size = 10 ) ) ).
  ENDMETHOD.

  METHOD ascent_descent.
    cl_abap_unit_assert=>assert_equals( exp = `7.18` act = num( font( `Helvetica` )->ascent( 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2.07` act = num( font( `Helvetica` )->descent( 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `6.83` act = num( font( `Times-Roman` )->ascent( 10 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = `1.57` act = num( font( `Courier` )->descent( 10 ) ) ).
  ENDMETHOD.

  METHOD winansi_codes.
    cl_abap_unit_assert=>assert_equals( exp = 65 act = zabaputil_cl_pdf_font_std=>winansi( 65 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 228 act = zabaputil_cl_pdf_font_std=>winansi( 228 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 128 act = zabaputil_cl_pdf_font_std=>winansi( 8364 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 153 act = zabaputil_cl_pdf_font_std=>winansi( 8482 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = zabaputil_cl_pdf_font_std=>winansi( 945 ) ).
    " the control characters and the gap of Windows-1252 are no characters
    cl_abap_unit_assert=>assert_equals( exp = 0 act = zabaputil_cl_pdf_font_std=>winansi( 10 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = zabaputil_cl_pdf_font_std=>winansi( 129 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = zabaputil_cl_pdf_font_std=>winansi( 127 ) ).
  ENDMETHOD.

  METHOD write_object.
    DATA lo_writer TYPE REF TO zabaputil_cl_pdf_writer.
    DATA lv_id TYPE i.
    DATA lv_hex TYPE string.
    DATA lv_expect TYPE string.
    DATA temp4 TYPE xsdboolean.
    CREATE OBJECT lo_writer TYPE zabaputil_cl_pdf_writer EXPORTING COMPRESS = abap_false.
    
    lv_id = font( `Times-BoldItalic` )->write( lo_writer ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lv_id ).
    
    lv_hex = lo_writer->render( lv_id ).
    
    lv_expect = zabaputil_cl_pdf_writer=>hex(
        `<< /Type /Font /Subtype /Type1 /BaseFont /Times-BoldItalic /Encoding /WinAnsiEncoding >>` ).
    
    temp4 = boolc( lv_hex CS lv_expect ).
    cl_abap_unit_assert=>assert_true( temp4 ).
  ENDMETHOD.

ENDCLASS.

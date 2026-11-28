CLASS ltcl_barcode DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  " The expected modules are what an independent port of the encoder wrote,
  " and what zxing-wasm read back as the same text - for the GS1 data with
  " the symbology identifier ]C1 of GS1-128.

  PRIVATE SECTION.
    METHODS digits_in_code_c      FOR TESTING.
    METHODS text_in_code_b        FOR TESTING.
    METHODS a_blank_is_a_symbol   FOR TESTING.
    METHODS gs1_sscc              FOR TESTING.
    METHODS gs1_variable_length   FOR TESTING.
    METHODS no_such_character     FOR TESTING.
    METHODS nothing_to_encode     FOR TESTING.
    METHODS no_gs1_data           FOR TESTING.
    METHODS bars_drawn            FOR TESTING.

ENDCLASS.


CLASS ltcl_barcode IMPLEMENTATION.

  METHOD digits_in_code_c.
    " start C, 47, 11, check character, stop: 4 x 11 + 13 modules
    cl_abap_unit_assert=>assert_equals(
        exp = `110100111001000111011011000100100100110100001100011101011`
        act = zabaputil_cl_pdf_barcode=>code128( `4711` ) ).
  ENDMETHOD.

  METHOD text_in_code_b.
    cl_abap_unit_assert=>assert_equals(
        exp = `1101001000010100011000100010110001000100011010011011100100111001101100111001011001011100101100001001100011101011`
        act = zabaputil_cl_pdf_barcode=>code128( `ABC-123` ) ).
  ENDMETHOD.

  METHOD a_blank_is_a_symbol.
    cl_abap_unit_assert=>assert_equals(
        exp = `11010010000101000110001101100110010011100110100111100101100011101011`
        act = zabaputil_cl_pdf_barcode=>code128( `A 1` ) ).
  ENDMETHOD.

  METHOD gs1_sscc.
    " start C, FNC1, 00 and the 18 digits of the SSCC in pairs
    cl_abap_unit_assert=>assert_equals(
        exp = `110100111001111010111011011001100100010110001100110110011101101110101110110001101100110011011001100110110011001101100110011001110010100110000101100011101011`
        act = zabaputil_cl_pdf_barcode=>code128( `(00)340123450000000018` ) ).
  ENDMETHOD.

  METHOD gs1_variable_length.
    " (37) has a value of its own length: FNC1 behind it, none behind the
    " fixed (02) and none at the end
    cl_abap_unit_assert=>assert_equals(
        exp = `11010011100111101011101100110011010010001100110011011001110110111010111011000110110011001101100110011001001000100011010001` &&
              `11010011001111010111011001000100101111011101000110111010001110110110111000101100100111011001110010110000100101100011101011`
        act = zabaputil_cl_pdf_barcode=>code128( `(02)04012345000009(37)24(10)LOT42` ) ).
  ENDMETHOD.

  METHOD no_such_character.
    TRY.
        zabaputil_cl_pdf_barcode=>code128( |A{ cl_abap_char_utilities=>horizontal_tab }B| ).
        cl_abap_unit_assert=>fail( `a tab is no character of Code 128 B` ).
      CATCH zabaputil_cx_util_error INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp( exp = `*position 2*`
                                             act = lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD nothing_to_encode.
    TRY.
        zabaputil_cl_pdf_barcode=>code128( `` ).
        cl_abap_unit_assert=>fail( `an empty text has no symbol` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD no_gs1_data.
    TRY.
        zabaputil_cl_pdf_barcode=>code128( `(AB)12` ).
        cl_abap_unit_assert=>fail( `an identifier is a number` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD bars_drawn.
    " 4711: one filled rectangle per bar - start C has three, every symbol
    " three, the stop four: 3 + 3 + 3 + 3 + 4 = 16
    DATA(lo_page) = NEW zabaputil_cl_pdf_page( width  = 100
                                               height = 100
                                               scale  = 1 ).
    zabaputil_cl_pdf_barcode=>draw( page    = lo_page
                                    modules = zabaputil_cl_pdf_barcode=>code128( `4711` )
                                    x       = 10
                                    y       = 20
                                    w       = 57
                                    h       = 30 ).
    DATA(lv_content) = lo_page->content( ).
    cl_abap_unit_assert=>assert_equals( exp = 16
                                        act = count( val = lv_content
                                                     sub = ` re f` ) ).
    " the first bar: two modules of 1 at the left edge, 30 high
    cl_abap_unit_assert=>assert_char_cp( exp = `10 50 2 30 re f*`
                                         act = lv_content ).
  ENDMETHOD.

ENDCLASS.

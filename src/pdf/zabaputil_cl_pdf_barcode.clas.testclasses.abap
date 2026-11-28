CLASS ltcl_barcode DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " The expected modules were checked out of band: the linear codes
    " against python-barcode, every symbol read back with zxing-cpp - the
    " QR codes and Data Matrices without one corrected codeword

    METHODS code128_set_b           FOR TESTING.
    METHODS code128_set_c           FOR TESTING.
    METHODS code128_mixed           FOR TESTING.
    METHODS code128_refused         FOR TESTING.
    METHODS ean13_modules           FOR TESTING.
    METHODS ean13_check_digit       FOR TESTING.
    METHODS qr_version_1            FOR TESTING.
    METHODS qr_sizes                FOR TESTING.
    METHODS qr_refused              FOR TESTING.
    METHODS datamatrix_ascii        FOR TESTING.
    METHODS datamatrix_digits       FOR TESTING.
    METHODS datamatrix_sizes        FOR TESTING.
    METHODS modules_by_type         FOR TESTING.
    METHODS draw_runs               FOR TESTING.

ENDCLASS.

CLASS ltcl_barcode IMPLEMENTATION.

  METHOD code128_set_b.
    DATA lv_exp TYPE string.
    " start B, H e l l o, check value 21, stop
    lv_exp = `110100100001100010100010110010000110010100001100101000010001111010110010100001100011101011`.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = zabaputil_cl_pdf_barcode=>code128( `Hello` ) ).
  ENDMETHOD.

  METHOD code128_set_c.
    DATA lv_exp TYPE string.
    " start C and three pairs of digits
    lv_exp = `11010011100101100111001000101100011100010110100011011101100011101011`.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = zabaputil_cl_pdf_barcode=>code128( `123456` ) ).
  ENDMETHOD.

  METHOD code128_mixed.
    DATA lv_exp TYPE string.
    " B, an odd run of seven digits (one in B, three pairs in C), B again
    lv_exp = `110100100001011101100010000110100111100100101011001000010000100110110110011001001110011010` &&
             `111011110111011011101011101100010000101100101111011101101100110010011110100100101100001000` &&
             `011010011001010000110110011001110010110011100101100101100100001100011101011`.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = zabaputil_cl_pdf_barcode=>code128( `Mixed 1234567 tail 99` ) ).
  ENDMETHOD.

  METHOD code128_refused.
    TRY.
        zabaputil_cl_pdf_barcode=>code128( |a\tb| ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        zabaputil_cl_pdf_barcode=>code128( `` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD ean13_modules.
    DATA lv_exp TYPE string.
    lv_exp = `101000110101001110101111011110100010010110011010101000010100001010000101110100100001011001` &&
             `10101`.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = zabaputil_cl_pdf_barcode=>ean13( `400638133393` ) ).
    " with its check digit: the same
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = zabaputil_cl_pdf_barcode=>ean13( `4006381333931` ) ).
  ENDMETHOD.

  METHOD ean13_check_digit.
    TRY.
        zabaputil_cl_pdf_barcode=>ean13( `4006381333932` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error INTO DATA(lx).
        cl_abap_unit_assert=>assert_true( xsdbool( lx->get_text( ) CS `is 1` ) ).
    ENDTRY.
    TRY.
        zabaputil_cl_pdf_barcode=>ean13( `12345` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD qr_version_1.
    " HELLO WORLD in byte mode, error correction M, mask 0
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table(
        ( `111111100110101111111` )
        ( `100000101100101000001` )
        ( `101110100000101011101` )
        ( `101110100011001011101` )
        ( `101110101100101011101` )
        ( `100000100100101000001` )
        ( `111111101010101111111` )
        ( `000000000011100000000` )
        ( `101010100101000010010` )
        ( `101001000110001100010` )
        ( `100010111110110111111` )
        ( `101100011110000010010` )
        ( `101100111000111110100` )
        ( `000000001111010000110` )
        ( `111111100011000110111` )
        ( `100000100111100100001` )
        ( `101110101111001010100` )
        ( `101110100001001110110` )
        ( `101110101010101010101` )
        ( `100000100011000010010` )
        ( `111111101101101100111` ) )
        act = zabaputil_cl_pdf_barcode=>qr( val  = `HELLO WORLD`
                                            ecc  = `M`
                                            mask = 0 ) ).
  ENDMETHOD.

  METHOD qr_sizes.
    DATA lv_text TYPE string.
    " the smallest version: 21 + 4 modules per version
    cl_abap_unit_assert=>assert_equals( exp = 21 act = lines( zabaputil_cl_pdf_barcode=>qr( `abc` ) ) ).
    lv_text = repeat( val = `x`
                      occ = 300 ).
    cl_abap_unit_assert=>assert_equals( exp = 69
                                        act = lines( zabaputil_cl_pdf_barcode=>qr( val = lv_text
                                                                                   ecc = `M` ) ) ).
    " a higher error correction needs more room
    cl_abap_unit_assert=>assert_equals( exp = 25
                                        act = lines( zabaputil_cl_pdf_barcode=>qr( val = `HELLO WORLD`
                                                                                   ecc = `H` ) ) ).
  ENDMETHOD.

  METHOD qr_refused.
    TRY.
        zabaputil_cl_pdf_barcode=>qr( val = `x`
                                      ecc = `X` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        zabaputil_cl_pdf_barcode=>qr( val = repeat( val = `x`
                                                    occ = 3000 )
                                      ecc = `H` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD datamatrix_ascii.
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table(
        ( `1010101010` )
        ( `1101100011` )
        ( `1000110100` )
        ( `1001101011` )
        ( `1001010000` )
        ( `1001001011` )
        ( `1101001100` )
        ( `1100111101` )
        ( `1100001000` )
        ( `1111111111` ) )
        act = zabaputil_cl_pdf_barcode=>datamatrix( `A` ) ).
  ENDMETHOD.

  METHOD datamatrix_digits.
    " two digits a codeword: three codewords, the smallest size
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE string_table(
        ( `1010101010` )
        ( `1100101101` )
        ( `1100000100` )
        ( `1100011101` )
        ( `1100001000` )
        ( `1000001111` )
        ( `1110110000` )
        ( `1111011001` )
        ( `1001110100` )
        ( `1111111111` ) )
        act = zabaputil_cl_pdf_barcode=>datamatrix( `123456` ) ).
  ENDMETHOD.

  METHOD datamatrix_sizes.
    " 40 characters need 40 codewords: 26 x 26 has 44; 500 need 88 x 88
    cl_abap_unit_assert=>assert_equals( exp = 26
                                        act = lines( zabaputil_cl_pdf_barcode=>datamatrix( repeat( val = `x`
                                                                                                    occ = 40 ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 88
                                        act = lines( zabaputil_cl_pdf_barcode=>datamatrix( repeat( val = `q`
                                                                                                    occ = 500 ) ) ) ).
  ENDMETHOD.

  METHOD modules_by_type.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( zabaputil_cl_pdf_barcode=>modules( type = `code128`
                                                                                        val  = `x` ) ) ).
    TRY.
        zabaputil_cl_pdf_barcode=>modules( type = `PDF417`
                                           val  = `x` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD draw_runs.
    " 95 modules over 95 points: a bar of three modules is one rectangle
    DATA(lo_page) = NEW zabaputil_cl_pdf_page( width  = 600
                                               height = 800
                                               scale  = 1 ).
    zabaputil_cl_pdf_barcode=>draw( page = lo_page
                                    type = zabaputil_cl_pdf_barcode=>cs_type-ean13
                                    val  = `400638133393`
                                    x    = 10
                                    y    = 10
                                    w    = 95
                                    h    = 30 ).
    DATA(lv_content) = lo_page->content( ).
    " the start guard: a bar at 10, one at 12; black inside its own state
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CP |q\n0 0 0 rg\n10 760 1 30 re f\n12 760 1 30 re f*| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CP |*re f\nQ| ) ).
    " 30 bars in an EAN-13
    cl_abap_unit_assert=>assert_equals( exp = 30 act = count( val = lv_content sub = `re f` ) ).
  ENDMETHOD.

ENDCLASS.

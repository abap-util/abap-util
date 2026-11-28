CLASS ltcl_writer DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS num_integer             FOR TESTING.
    METHODS num_decimals            FOR TESTING.
    METHODS num_negative            FOR TESTING.
    METHODS num_rounds              FOR TESTING.
    METHODS hex_ascii               FOR TESTING.
    METHODS hex_word_values         FOR TESTING.
    METHODS hex_utf16_bmp           FOR TESTING.
    METHODS hex_utf16_astral        FOR TESTING.
    METHODS render_header           FOR TESTING.
    METHODS render_xref             FOR TESTING.
    METHODS render_trailer_info     FOR TESTING.
    METHODS reserve_numbers         FOR TESTING.
    METHODS reserved_unwritten      FOR TESTING.
    METHODS object_not_reserved     FOR TESTING.
    METHODS stream_plain            FOR TESTING.
    METHODS stream_deflated         FOR TESTING.
    METHODS stream_not_smaller      FOR TESTING.
    METHODS stream_own_filter       FOR TESTING.
    METHODS stream_empty            FOR TESTING.

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

ENDCLASS.

CLASS ltcl_writer IMPLEMENTATION.

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

  METHOD num_integer.
    cl_abap_unit_assert=>assert_equals( exp = `12` act = zabaputil_cl_pdf_writer=>num( 12 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `0` act = zabaputil_cl_pdf_writer=>num( 0 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `1000` act = zabaputil_cl_pdf_writer=>num( 1000 ) ).
  ENDMETHOD.

  METHOD num_decimals.
    DATA lv_f TYPE f.
    lv_f = `0.5`.
    cl_abap_unit_assert=>assert_equals( exp = `0.5` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    lv_f = `595.28`.
    cl_abap_unit_assert=>assert_equals( exp = `595.28` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    lv_f = `3.125`.
    cl_abap_unit_assert=>assert_equals( exp = `3.125` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    lv_f = `7.05`.
    cl_abap_unit_assert=>assert_equals( exp = `7.05` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    lv_f = `7.007`.
    cl_abap_unit_assert=>assert_equals( exp = `7.007` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    " a packed number is taken as it is
    DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
    lv_p = `2.50`.
    cl_abap_unit_assert=>assert_equals( exp = `2.5` act = zabaputil_cl_pdf_writer=>num( lv_p ) ).
  ENDMETHOD.

  METHOD num_negative.
    DATA lv_f TYPE f.
    lv_f = `-3.25`.
    cl_abap_unit_assert=>assert_equals( exp = `-3.25` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    cl_abap_unit_assert=>assert_equals( exp = `-7` act = zabaputil_cl_pdf_writer=>num( -7 ) ).
    " what rounds to zero has no sign
    lv_f = `-0.0001`.
    cl_abap_unit_assert=>assert_equals( exp = `0` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
  ENDMETHOD.

  METHOD num_rounds.
    DATA lv_f TYPE f.
    lv_f = `1.23456`.
    cl_abap_unit_assert=>assert_equals( exp = `1.235` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    lv_f = `0.9996`.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
    lv_f = `0.0004`.
    cl_abap_unit_assert=>assert_equals( exp = `0` act = zabaputil_cl_pdf_writer=>num( lv_f ) ).
  ENDMETHOD.

  METHOD hex_ascii.
    cl_abap_unit_assert=>assert_equals( exp = `4142200A` act = zabaputil_cl_pdf_writer=>hex( |AB \n| ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_writer=>hex( `` ) ).
  ENDMETHOD.

  METHOD hex_word_values.
    cl_abap_unit_assert=>assert_equals( exp = `0041` act = zabaputil_cl_pdf_writer=>hex_word( 65 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `0000` act = zabaputil_cl_pdf_writer=>hex_word( 0 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `FFFF` act = zabaputil_cl_pdf_writer=>hex_word( 65535 ) ).
  ENDMETHOD.

  METHOD hex_utf16_bmp.
    " A, a-umlaut, the euro sign
    DATA(lv_text) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `41C3A4E282AC` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `FEFF004100E420AC`
                                        act = zabaputil_cl_pdf_writer=>hex_utf16( lv_text ) ).
    cl_abap_unit_assert=>assert_equals( exp = `FEFF`
                                        act = zabaputil_cl_pdf_writer=>hex_utf16( `` ) ).
  ENDMETHOD.

  METHOD hex_utf16_astral.
    " U+1F600 is the surrogate pair D83D DE00
    DATA(lv_text) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `F09F9880` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `FEFFD83DDE00`
                                        act = zabaputil_cl_pdf_writer=>hex_utf16( lv_text ) ).
  ENDMETHOD.

  METHOD render_header.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( abap_false ).
    DATA(lv_root) = lo_writer->add_object( `<< /Type /Catalog >>` ).
    DATA lv_hex TYPE string.
    lv_hex = lo_writer->render( lv_root ).
    " %PDF-1.4, a line feed, a comment of four bytes above 127
    cl_abap_unit_assert=>assert_equals( exp = `255044462D312E340A25E2E3CFD30A`
                                        act = substring( val = lv_hex
                                                         len = 30 ) ).
  ENDMETHOD.

  METHOD render_xref.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( abap_false ).
    DATA(lv_pages) = lo_writer->reserve( ).
    DATA(lv_root) = lo_writer->add_object( |<< /Type /Catalog /Pages { lv_pages } 0 R >>| ).
    lo_writer->add_stream( CONV xstring( `00FF80C2B5` ) ).
    lo_writer->set_object( id  = lv_pages
                           val = `<< /Type /Pages /Kids [] /Count 0 >>` ).
    DATA(lv_file) = lo_writer->render( lv_root ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = |1 0 obj\n<< /Type /Pages /Kids [] /Count 0 >>\nendobj\n| ) ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Size 4 /Root 2 0 R >>` ) ).
  ENDMETHOD.

  METHOD render_trailer_info.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( abap_false ).
    DATA(lv_root) = lo_writer->add_object( `<< /Type /Catalog >>` ).
    DATA(lv_info) = lo_writer->add_object( `<< /Producer (x) >>` ).
    DATA(lv_file) = lo_writer->render( root = lv_root
                                       info = lv_info ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `/Size 3 /Root 1 0 R /Info 2 0 R >>` ) ).
  ENDMETHOD.

  METHOD reserve_numbers.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_writer->reserve( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_writer->add_object( `1` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lo_writer->reserve( ) ).
  ENDMETHOD.

  METHOD reserved_unwritten.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( abap_false ).
    lo_writer->reserve( ).
    DATA(lv_root) = lo_writer->add_object( `<< /Type /Catalog >>` ).
    DATA(lv_file) = lo_writer->render( lv_root ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = |1 0 obj\nnull\nendobj\n| ) ).
  ENDMETHOD.

  METHOD object_not_reserved.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( ).
    TRY.
        lo_writer->set_object( id  = 5
                               val = `1` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        lo_writer->set_stream( id      = 0
                               content = CONV xstring( `00` ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD stream_plain.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( abap_false ).
    DATA(lv_id) = lo_writer->add_stream( dict    = `/Type /X`
                                         content = CONV xstring( `414243` ) ).
    DATA(lv_file) = lo_writer->render( lv_id ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true(
        has( file = lv_file
             text = |1 0 obj\n<< /Type /X /Length 3 >>\nstream\nABC\nendstream\nendobj\n| ) ).
  ENDMETHOD.

  METHOD stream_deflated.
    TRY.
        zabaputil_cl_util_context=>gzip_compress( CONV xstring( `00` ) ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    DATA lv_hex TYPE string.
    DO 400 TIMES.
      lv_hex = lv_hex && `41`.
    ENDDO.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( ).
    DATA(lv_id) = lo_writer->add_stream( CONV xstring( lv_hex ) ).
    DATA(lv_file) = lo_writer->render( lv_id ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = ` /Filter /FlateDecode >>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( xstrlen( lv_file ) < 400 ) ).
  ENDMETHOD.

  METHOD stream_not_smaller.
    " three bytes do not shrink: the stream stays as it is
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( ).
    DATA(lv_id) = lo_writer->add_stream( CONV xstring( `414243` ) ).
    DATA(lv_file) = lo_writer->render( lv_id ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = |<< /Length 3 >>\nstream\nABC\nendstream| ) ).
  ENDMETHOD.

  METHOD stream_own_filter.
    DATA lv_hex TYPE string.
    DO 400 TIMES.
      lv_hex = lv_hex && `41`.
    ENDDO.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( ).
    DATA(lv_id) = lo_writer->add_stream( dict     = `/Filter /DCTDecode`
                                         content  = CONV xstring( lv_hex )
                                         compress = abap_false ).
    DATA(lv_file) = lo_writer->render( lv_id ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = `<< /Filter /DCTDecode /Length 400 >>` ) ).
  ENDMETHOD.

  METHOD stream_empty.
    DATA lv_empty TYPE xstring.
    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( ).
    DATA(lv_id) = lo_writer->add_stream( lv_empty ).
    DATA(lv_file) = lo_writer->render( lv_id ).
    check_xref( lv_file ).
    cl_abap_unit_assert=>assert_true( has( file = lv_file
                                           text = |<< /Length 0 >>\nstream\n\nendstream| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS zabaputil_cl_pdf_writer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " The file level of a PDF: numbered objects, streams, the cross-reference
    " table and the trailer. It knows nothing about pages, fonts or text -
    " zabaputil_cl_pdf hands it the objects.
    "
    " The file is collected in hex and converted to bytes once, in render( ):
    " an xstring has no append operator, and every offset of the
    " cross-reference table is a byte offset - half the length of the hex
    " text, whatever the objects hold.

    "! @parameter compress | deflate the streams (FlateDecode); a stream that
    "!                       does not get smaller, and every stream on a system
    "!                       without CL_ABAP_GZIP, is written as it is
    METHODS constructor
      IMPORTING
        compress TYPE abap_bool DEFAULT abap_true.

    "! Take the next object number, for an object that is referenced before
    "! it can be written (the page tree by its pages)
    METHODS reserve
      RETURNING
        VALUE(result) TYPE i.

    "! Write the body of a reserved object
    "! @parameter id  | the object number
    "! @parameter val | the body - a dictionary, an array, a number; 7-bit ASCII
    METHODS set_object
      IMPORTING
        id  TYPE i
        val TYPE clike.

    "! A new object, numbered here
    "! @parameter val    | the body - a dictionary, an array, a number; 7-bit ASCII
    "! @parameter result | the object number
    METHODS add_object
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE i.

    "! Write a reserved object as a stream. Length and, when the stream is
    "! deflated here, Filter are added to the dictionary
    "! @parameter id       | the object number
    "! @parameter dict     | the entries of the stream dictionary, without the brackets
    "! @parameter content  | the bytes of the stream
    "! @parameter compress | abap_false for bytes that carry a filter of their
    "!                       own (a JPEG, the IDAT data of a PNG)
    METHODS set_stream
      IMPORTING
        id       TYPE i
        dict     TYPE clike OPTIONAL
        content  TYPE xstring
        compress TYPE abap_bool DEFAULT abap_true.

    "! A new stream object, numbered here
    "! @parameter dict     | the entries of the stream dictionary, without the brackets
    "! @parameter content  | the bytes of the stream
    "! @parameter compress | abap_false for bytes that carry a filter of their own
    "! @parameter result   | the object number
    METHODS add_stream
      IMPORTING
        dict          TYPE clike OPTIONAL
        content       TYPE xstring
        compress      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE i.

    "! The file: header, the objects in the order of their numbers,
    "! cross-reference table and trailer
    "! @parameter root | the object number of the catalog
    "! @parameter info | the object number of the information dictionary, 0 for none
    METHODS render
      IMPORTING
        root          TYPE i
        info          TYPE i DEFAULT 0
        version       TYPE clike DEFAULT `1.4`
        id            TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE xstring.

    "! A number as a PDF operand: no exponent, at most three decimals, no
    "! trailing zeros - 12, 0.5, -3.125
    CLASS-METHODS num
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    "! The bytes of a 7-bit ASCII text in hex, two characters per byte
    CLASS-METHODS hex
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! A text as a PDF text string in hex, UTF-16BE with its byte order mark -
    "! the form the information dictionary takes for a title in any script
    CLASS-METHODS hex_utf16
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! An integer as two bytes in hex, 0041 for 65
    CLASS-METHODS hex_word
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_object,
        hex TYPE string,
      END OF ty_s_object.

    " the row index is the object number
    DATA mt_object   TYPE STANDARD TABLE OF ty_s_object WITH EMPTY KEY.
    DATA mv_compress TYPE abap_bool.

    METHODS object_put
      IMPORTING
        id  TYPE i
        hex TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_writer IMPLEMENTATION.

  METHOD constructor.

    mv_compress = compress.

  ENDMETHOD.

  METHOD reserve.

    INSERT VALUE #( ) INTO TABLE mt_object.
    result = lines( mt_object ).

  ENDMETHOD.

  METHOD set_object.

    object_put( id  = id
                hex = hex( |{ id } 0 obj\n{ val }\nendobj\n| ) ).

  ENDMETHOD.

  METHOD add_object.

    result = reserve( ).
    set_object( id  = result
                val = val ).

  ENDMETHOD.

  METHOD set_stream.

    DATA lv_bytes  TYPE xstring.
    DATA lv_packed TYPE xstring.
    DATA lv_filter TYPE string.
    DATA lv_dict   TYPE string.

    lv_bytes = content.
    IF mv_compress = abap_true AND compress = abap_true AND xstrlen( content ) > 0.
      TRY.
          lv_packed = zabaputil_cl_util_context=>zlib_compress( content ).
          IF xstrlen( lv_packed ) < xstrlen( content ).
            lv_bytes = lv_packed.
            lv_filter = ` /Filter /FlateDecode`.
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
          " no CL_ABAP_GZIP here: the stream is valid without a filter
      ENDTRY.
    ENDIF.

    lv_dict = dict.
    IF lv_dict IS NOT INITIAL.
      lv_dict = lv_dict && ` `.
    ENDIF.

    object_put( id  = id
                hex = hex( |{ id } 0 obj\n<< { lv_dict }/Length { xstrlen( lv_bytes ) }{ lv_filter } >>\nstream\n| ) &&
                      CONV string( lv_bytes ) &&
                      hex( |\nendstream\nendobj\n| ) ).

  ENDMETHOD.

  METHOD add_stream.

    result = reserve( ).
    set_stream( id       = result
                dict     = dict
                content  = content
                compress = compress ).

  ENDMETHOD.

  METHOD object_put.

    IF id < 1 OR id > lines( mt_object ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: object { id } is not reserved|.
    ENDIF.
    ASSIGN mt_object[ id ] TO FIELD-SYMBOL(<object>).
    <object>-hex = hex.

  ENDMETHOD.

  METHOD render.

    DATA lt_hex    TYPE string_table.
    DATA lt_xref   TYPE string_table.
    DATA lv_id     TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_info   TYPE string.
    DATA lv_head   TYPE string.

    " the parts of the file in a table, joined once at the end: a string
    " that grows by every object is copied again and again
    " the second line is a comment of four bytes above 127: it tells a
    " transfer program that the file is binary
    lv_head = hex( |%PDF-{ version }\n%| ) && `E2E3CFD3` && `0A`.
    INSERT lv_head INTO TABLE lt_hex.
    lv_offset = strlen( lv_head ) DIV 2.

    INSERT |xref\n0 { lines( mt_object ) + 1 }\n0000000000 65535 f \n| INTO TABLE lt_xref.
    LOOP AT mt_object ASSIGNING FIELD-SYMBOL(<object>).
      lv_id = sy-tabix.
      IF <object>-hex IS INITIAL.
        " reserved and never written: an object has to be there
        <object>-hex = hex( |{ lv_id } 0 obj\nnull\nendobj\n| ).
      ENDIF.
      INSERT zabaputil_cl_util_context=>c_pad_left( val = |{ lv_offset }|
                                                    len = 10 ) && | 00000 n \n| INTO TABLE lt_xref.
      INSERT <object>-hex INTO TABLE lt_hex.
      lv_offset = lv_offset + strlen( <object>-hex ) DIV 2.
    ENDLOOP.

    IF info > 0.
      lv_info = | /Info { info } 0 R|.
    ENDIF.
    " the identifier of the file: twice the same, a new file has no other
    IF id IS NOT INITIAL.
      lv_info = |{ lv_info } /ID [<{ id }> <{ id }>]|.
    ENDIF.
    INSERT hex( concat_lines_of( lt_xref ) ) INTO TABLE lt_hex.
    INSERT hex( |trailer\n<< /Size { lines( mt_object ) + 1 } /Root { root } 0 R{ lv_info } >>\n| ) INTO TABLE lt_hex.
    INSERT hex( |startxref\n{ lv_offset }\n%%EOF\n| ) INTO TABLE lt_hex.

    result = concat_lines_of( lt_hex ).

  ENDMETHOD.

  METHOD num.

    DATA lv_f     TYPE f.
    DATA lv_milli TYPE i.
    DATA lv_int   TYPE i.
    DATA lv_frac  TYPE i.
    DATA lv_dec   TYPE string.

    " thousandths as an integer: the conversion rounds, and no format
    " option of a float is involved
    lv_f = val.
    lv_milli = abs( lv_f ) * 1000.
    lv_int = lv_milli DIV 1000.
    lv_frac = lv_milli MOD 1000.

    result = |{ lv_int }|.
    IF lv_frac > 0.
      lv_dec = zabaputil_cl_util_context=>c_pad_left( val = |{ lv_frac }|
                                                      len = 3 ).
      IF lv_frac MOD 100 = 0.
        lv_dec = substring( val = lv_dec
                            len = 1 ).
      ELSEIF lv_frac MOD 10 = 0.
        lv_dec = substring( val = lv_dec
                            len = 2 ).
      ENDIF.
      result = result && `.` && lv_dec.
    ENDIF.
    IF lv_f < 0 AND lv_milli > 0.
      result = `-` && result.
    ENDIF.

  ENDMETHOD.

  METHOD hex.

    result = CONV string( zabaputil_cl_util_context=>conv_get_xstring_by_string( CONV string( val ) ) ).

  ENDMETHOD.

  METHOD hex_utf16.

    DATA lv_cp TYPE i.

    result = `FEFF`.
    LOOP AT zabaputil_cl_util_context=>c_get_codepoints( val ) INTO lv_cp.
      IF lv_cp < 65536.
        result = result && hex_word( lv_cp ).
      ELSE.
        lv_cp = lv_cp - 65536.
        result = result && hex_word( 55296 + lv_cp DIV 1024 ) && hex_word( 56320 + lv_cp MOD 1024 ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD hex_word.

    DATA lv_x TYPE x LENGTH 2.

    lv_x = val.
    result = lv_x.

  ENDMETHOD.

ENDCLASS.

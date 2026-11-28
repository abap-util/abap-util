CLASS zabaputil_cl_pdf_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " A page of an existing PDF file, to be drawn on the pages of a new one
    " - a letterhead, a form printed elsewhere. zabaputil_cl_pdf=>import_page
    " creates it, zabaputil_cl_pdf_page=>place and =>background draw it.
    "
    " The page goes into the new file as a form XObject: its content,
    " decoded and joined, and its resources with every object they refer
    " to, copied byte for byte with new object numbers. The reader knows
    " cross-reference tables and streams, object streams and incremental
    " updates; FlateDecode is the one filter it decodes (the content of
    " the page, cross-reference and object streams) - the streams it only
    " copies may have any filter. Not supported: encrypted files.

    "! @parameter content  | the bytes of the PDF file
    "! @parameter page     | the page to import, from 1
    "! @parameter resource | the name in the resource dictionary, `P1`
    METHODS constructor
      IMPORTING
        content  TYPE xstring
        page     TYPE i DEFAULT 1
        resource TYPE clike DEFAULT `P1`.

    "! The name of the page in the resource dictionary of the pages
    METHODS resource
      RETURNING
        VALUE(result) TYPE string.

    "! The number of pages of the file
    METHODS page_count
      RETURNING
        VALUE(result) TYPE i.

    "! The width of the imported page, in points
    METHODS width
      RETURNING
        VALUE(result) TYPE f.

    "! The height of the imported page, in points
    METHODS height
      RETURNING
        VALUE(result) TYPE f.

    "! Write the page as a form XObject, and the objects it refers to
    "! @parameter result | the object number of the form XObject
    METHODS write
      IMPORTING
        writer        TYPE REF TO zabaputil_cl_pdf_writer
      RETURNING
        VALUE(result) TYPE i.

    "! A byte range of the file as text, one character per byte: the
    "! printable ASCII as it is, line feeds and returns as they are, every
    "! other byte a blank - for reading the structure, not the content
    METHODS text
      IMPORTING
        off           TYPE i
        len           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! The same of any bytes
    CLASS-METHODS text_of
      IMPORTING
        data          TYPE xstring
        off           TYPE i
        len           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! Where the box of the page starts, in points: its left and lower edge
    METHODS origin_x
      RETURNING
        VALUE(result) TYPE f.

    METHODS origin_y
      RETURNING
        VALUE(result) TYPE f.

    "! The value of a key of a dictionary, as it is written: `12 0 R`,
    "! `[0 0 595 842]`, `<< ... >>`; empty when the dictionary has none
    "! @parameter dict | the dictionary, with or without its brackets
    "! @parameter key  | the key without the slash
    CLASS-METHODS dict_value
      IMPORTING
        dict          TYPE string
        key           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_entry,
        num    TYPE i,
        "! 1 at an offset, 2 in an object stream
        kind   TYPE i,
        offset TYPE i,
        stream TYPE i,
        index  TYPE i,
      END OF ty_s_entry.

    TYPES:
      BEGIN OF ty_s_token,
        "! N number, M name, S string, H hex string, D << , E >>, A [ , B ], K keyword
        kind TYPE c LENGTH 1,
        off  TYPE i,
        len  TYPE i,
        text TYPE string,
      END OF ty_s_token.
    TYPES ty_t_token TYPE STANDARD TABLE OF ty_s_token WITH EMPTY KEY.

    "! an object as it is in the file
    TYPES:
      BEGIN OF ty_s_object,
        "! the value, a dictionary of a stream without the stream
        text      TYPE string,
        "! the same bytes in hex
        hex       TYPE string,
        is_stream TYPE abap_bool,
        "! the bytes of the stream, still encoded
        data      TYPE xstring,
      END OF ty_s_object.

    DATA mv_content  TYPE xstring.
    DATA mv_resource TYPE string.
    DATA mv_page     TYPE i.
    DATA mt_entry    TYPE HASHED TABLE OF ty_s_entry WITH UNIQUE KEY num.
    DATA mv_root     TYPE i.
    DATA mt_page     TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA mv_box      TYPE string.
    DATA mv_width    TYPE f.
    DATA mv_height   TYPE f.
    DATA mv_x0       TYPE f.
    DATA mv_y0       TYPE f.

    TYPES:
      BEGIN OF ty_s_map,
        old TYPE i,
        new TYPE i,
      END OF ty_s_map.
    " while the page is written: the objects copied so far, and the ones
    " still to copy
    DATA mt_map      TYPE HASHED TABLE OF ty_s_map WITH UNIQUE KEY old.
    DATA mt_queue    TYPE STANDARD TABLE OF ty_s_map WITH EMPTY KEY.

    CLASS-METHODS tokens
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_token.

    "! The object number of a reference `12 0 R`; 0 for anything else
    CLASS-METHODS ref
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_bytes
      IMPORTING
        val           TYPE string
        off           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS read_xref
      IMPORTING
        offset TYPE i.

    METHODS object
      IMPORTING
        num           TYPE i
      RETURNING
        VALUE(result) TYPE ty_s_object.

    METHODS object_at
      IMPORTING
        offset        TYPE i
      RETURNING
        VALUE(result) TYPE ty_s_object.

    "! The decoded bytes of a stream
    METHODS decode
      IMPORTING
        object        TYPE ty_s_object
      RETURNING
        VALUE(result) TYPE xstring.

    "! A value that may be a reference: the value of the object then
    METHODS resolve
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS collect_pages
      IMPORTING
        num   TYPE i
        depth TYPE i.

    "! A value of the page or of the pages above it
    METHODS inherited
      IMPORTING
        num           TYPE i
        key           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The hex of a text with its references renumbered; new objects to
    "! copy are added to the queue
    METHODS renumber
      IMPORTING
        hex           TYPE string
        text          TYPE string
        writer        TYPE REF TO zabaputil_cl_pdf_writer
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_reader IMPLEMENTATION.

  METHOD constructor.

    DATA lv_start TYPE i.

    mv_content = content.
    mv_resource = resource.
    mv_page = page.

    IF xstrlen( mv_content ) < 16 OR text( off = 0 len = 5 ) <> `%PDF-`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: not a PDF file`.
    ENDIF.

    " startxref near the end: where the last cross-reference section is
    DATA(lv_tail) = nmin( val1 = 1024
                          val2 = xstrlen( mv_content ) ).
    DATA(lv_end) = text( off = xstrlen( mv_content ) - lv_tail
                         len = lv_tail ).
    DATA(lv_pos) = find( val = lv_end
                         sub = `startxref`
                         occ = -1 ).
    IF lv_pos < 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the file has no startxref`.
    ENDIF.
    DATA(lt_token) = tokens( substring( val = lv_end
                                        off = lv_pos + 9 ) ).
    IF lt_token IS INITIAL OR lt_token[ 1 ]-kind <> `N`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the file has no startxref`.
    ENDIF.
    lv_start = lt_token[ 1 ]-text.
    read_xref( lv_start ).

    IF mv_root = 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the file has no catalog`.
    ENDIF.

    " the pages in their order
    DATA(lv_pages) = ref( dict_value( dict = object( mv_root )-text
                                      key  = `Pages` ) ).
    collect_pages( num   = lv_pages
                   depth = 0 ).
    IF mv_page < 1 OR mv_page > lines( mt_page ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: the file has no page { mv_page } - it has { lines( mt_page ) }|.
    ENDIF.

    " the size: the media box, of the page or the pages above it
    mv_box = resolve( inherited( num = mt_page[ mv_page ]
                                 key = `MediaBox` ) ).
    DATA lt_number TYPE STANDARD TABLE OF f WITH EMPTY KEY.
    LOOP AT tokens( mv_box ) INTO DATA(ls_token) WHERE kind = `N`.
      INSERT CONV f( ls_token-text ) INTO TABLE lt_number.
    ENDLOOP.
    IF lines( lt_number ) <> 4.
      " US Letter, what a viewer assumes
      CLEAR lt_number.
      INSERT CONV f( 0 ) INTO TABLE lt_number.
      INSERT CONV f( 0 ) INTO TABLE lt_number.
      INSERT CONV f( 612 ) INTO TABLE lt_number.
      INSERT CONV f( 792 ) INTO TABLE lt_number.
      mv_box = `[0 0 612 792]`.
    ENDIF.
    mv_x0 = lt_number[ 1 ].
    mv_y0 = lt_number[ 2 ].
    mv_width = lt_number[ 3 ] - lt_number[ 1 ].
    mv_height = lt_number[ 4 ] - lt_number[ 2 ].
    " written anew: the numbers, whatever the file wrote around them
    mv_box = |[{ zabaputil_cl_pdf_writer=>num( lt_number[ 1 ] ) } { zabaputil_cl_pdf_writer=>num( lt_number[ 2 ] ) } | &&
             |{ zabaputil_cl_pdf_writer=>num( lt_number[ 3 ] ) } { zabaputil_cl_pdf_writer=>num( lt_number[ 4 ] ) }]|.

  ENDMETHOD.

  METHOD resource.

    result = mv_resource.

  ENDMETHOD.

  METHOD page_count.

    result = lines( mt_page ).

  ENDMETHOD.

  METHOD width.

    result = mv_width.

  ENDMETHOD.

  METHOD height.

    result = mv_height.

  ENDMETHOD.

  METHOD text.

    result = text_of( data = mv_content
                      off  = off
                      len  = len ).

  ENDMETHOD.

  METHOD origin_x.

    result = mv_x0.

  ENDMETHOD.

  METHOD origin_y.

    result = mv_y0.

  ENDMETHOD.

  METHOD text_of.

    DATA lv_x    TYPE x LENGTH 1.
    DATA lv_code TYPE i.
    DATA lt_char TYPE string_table.

    " the printable characters of ASCII, from the blank on
    CONSTANTS lc_ascii TYPE string VALUE ` !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_``abcdefghijklmnopqrstuvwxyz{|}~`.

    DATA(lv_end) = nmin( val1 = off + len
                         val2 = xstrlen( data ) ).
    DATA(lv_off) = nmax( val1 = off
                         val2 = 0 ).
    WHILE lv_off < lv_end.
      lv_x = data+lv_off(1).
      lv_code = lv_x.
      IF lv_code >= 32 AND lv_code <= 126.
        INSERT substring( val = lc_ascii
                          off = lv_code - 32
                          len = 1 ) INTO TABLE lt_char.
      ELSEIF lv_code = 10.
        INSERT |\n| INTO TABLE lt_char.
      ELSEIF lv_code = 13.
        INSERT |\r| INTO TABLE lt_char.
      ELSE.
        INSERT ` ` INTO TABLE lt_char.
      ENDIF.
      lv_off = lv_off + 1.
    ENDWHILE.
    result = concat_lines_of( lt_char ).

  ENDMETHOD.

  METHOD tokens.

    DATA ls_token TYPE ty_s_token.
    DATA lv_depth TYPE i.
    DATA lv_char  TYPE string.
    DATA lv_end   TYPE i.

    DATA(lv_length) = strlen( val ).
    DATA(lv_off) = 0.
    WHILE lv_off < lv_length.
      lv_char = val+lv_off(1).
      CLEAR ls_token.
      ls_token-off = lv_off.

      " white space, a comment
      IF lv_char = ` ` OR lv_char = |\n| OR lv_char = |\r| OR lv_char = |\t|.
        lv_off = lv_off + 1.
        CONTINUE.
      ENDIF.
      IF lv_char = `%`.
        WHILE lv_off < lv_length AND val+lv_off(1) <> |\n| AND val+lv_off(1) <> |\r|.
          lv_off = lv_off + 1.
        ENDWHILE.
        CONTINUE.
      ENDIF.

      IF lv_char = `(`.
        " a literal string: balanced brackets, a backslash escapes
        ls_token-kind = `S`.
        lv_depth = 0.
        WHILE lv_off < lv_length.
          lv_char = val+lv_off(1).
          IF lv_char = `\`.
            lv_off = lv_off + 2.
            CONTINUE.
          ELSEIF lv_char = `(`.
            lv_depth = lv_depth + 1.
          ELSEIF lv_char = `)`.
            lv_depth = lv_depth - 1.
            IF lv_depth = 0.
              lv_off = lv_off + 1.
              EXIT.
            ENDIF.
          ENDIF.
          lv_off = lv_off + 1.
        ENDWHILE.
      ELSEIF lv_char = `<` AND lv_off + 1 < lv_length AND val+lv_off(2) = `<<`.
        ls_token-kind = `D`.
        lv_off = lv_off + 2.
      ELSEIF lv_char = `>` AND lv_off + 1 < lv_length AND val+lv_off(2) = `>>`.
        ls_token-kind = `E`.
        lv_off = lv_off + 2.
      ELSEIF lv_char = `<`.
        ls_token-kind = `H`.
        lv_end = find( val = val
                       sub = `>`
                       off = lv_off ).
        lv_off = COND #( WHEN lv_end < 0 THEN lv_length ELSE lv_end + 1 ).
      ELSEIF lv_char = `[`.
        ls_token-kind = `A`.
        lv_off = lv_off + 1.
      ELSEIF lv_char = `]`.
        ls_token-kind = `B`.
        lv_off = lv_off + 1.
      ELSE.
        " a name, a number or a keyword: up to a delimiter
        ls_token-kind = COND #( WHEN lv_char = `/` THEN `M` ELSE `K` ).
        lv_off = lv_off + 1.
        WHILE lv_off < lv_length.
          lv_char = val+lv_off(1).
          IF lv_char CA ` ()<>[]{}/%` OR lv_char = |\n| OR lv_char = |\r| OR lv_char = |\t|.
            EXIT.
          ENDIF.
          lv_off = lv_off + 1.
        ENDWHILE.
        IF ls_token-kind = `K` AND substring( val = val
                                              off = ls_token-off
                                              len = lv_off - ls_token-off ) CO `0123456789.+-`.
          ls_token-kind = `N`.
        ENDIF.
      ENDIF.

      ls_token-len = lv_off - ls_token-off.
      ls_token-text = substring( val = val
                                 off = ls_token-off
                                 len = ls_token-len ).
      INSERT ls_token INTO TABLE result.
    ENDWHILE.

  ENDMETHOD.

  METHOD dict_value.

    DATA lv_depth TYPE i.
    DATA lv_start TYPE i.
    DATA lv_index TYPE i.

    DATA(lt_token) = tokens( dict ).
    " inside the outer brackets, at their level: a name, then its value
    DATA(lv_level) = COND i( WHEN lt_token IS NOT INITIAL AND lt_token[ 1 ]-kind = `D` THEN 1 ELSE 0 ).
    LOOP AT lt_token INTO DATA(ls_token).
      lv_index = sy-tabix.
      IF lv_depth = lv_level AND ls_token-kind = `M` AND ls_token-text = |/{ key }|
          AND lv_index < lines( lt_token ).
        " the value: one token, a reference of three, or a bracketed one
        DATA(ls_first) = lt_token[ lv_index + 1 ].
        lv_start = ls_first-off.
        IF ls_first-kind = `D` OR ls_first-kind = `A`.
          DATA(lv_inner) = 0.
          LOOP AT lt_token INTO DATA(ls_inner) FROM lv_index + 1.
            IF ls_inner-kind = `D` OR ls_inner-kind = `A`.
              lv_inner = lv_inner + 1.
            ELSEIF ls_inner-kind = `E` OR ls_inner-kind = `B`.
              lv_inner = lv_inner - 1.
              IF lv_inner = 0.
                result = substring( val = dict
                                    off = lv_start
                                    len = ls_inner-off + ls_inner-len - lv_start ).
                RETURN.
              ENDIF.
            ENDIF.
          ENDLOOP.
          RETURN.
        ENDIF.
        IF ls_first-kind = `N` AND lv_index + 3 <= lines( lt_token ).
          IF lt_token[ lv_index + 2 ]-kind = `N` AND lt_token[ lv_index + 3 ]-text = `R`.
            result = substring( val = dict
                                off = lv_start
                                len = lt_token[ lv_index + 3 ]-off + 1 - lv_start ).
            RETURN.
          ENDIF.
        ENDIF.
        result = ls_first-text.
        RETURN.
      ENDIF.
      IF ls_token-kind = `D` OR ls_token-kind = `A`.
        lv_depth = lv_depth + 1.
      ELSEIF ls_token-kind = `E` OR ls_token-kind = `B`.
        lv_depth = lv_depth - 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD ref.

    DATA(lt_token) = tokens( val ).
    IF lines( lt_token ) = 3 AND lt_token[ 1 ]-kind = `N` AND lt_token[ 2 ]-kind = `N` AND lt_token[ 3 ]-text = `R`.
      result = lt_token[ 1 ]-text.
    ENDIF.

  ENDMETHOD.

  METHOD find_bytes.

    " in the hex of the file, on a byte boundary
    DATA(lv_hex) = zabaputil_cl_pdf_writer=>hex( val ).
    DATA lv_file TYPE string.
    lv_file = mv_content.
    result = off * 2.
    DO.
      result = find( val = lv_file
                     sub = lv_hex
                     off = result ).
      IF result < 0 OR result MOD 2 = 0.
        EXIT.
      ENDIF.
      result = result + 1.
    ENDDO.
    IF result >= 0.
      result = result DIV 2.
    ENDIF.

  ENDMETHOD.

  METHOD read_xref.

    DATA ls_entry TYPE ty_s_entry.
    DATA lv_trailer TYPE string.
    DATA lv_num   TYPE i.
    DATA lv_count TYPE i.

    IF offset <= 0 OR offset >= xstrlen( mv_content ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the cross-reference of the file is damaged`.
    ENDIF.

    IF text( off = offset len = 4 ) = `xref`.
      " a table: sections of a first number and a count, 20 bytes an entry
      DATA(lv_end) = find_bytes( val = `trailer`
                                 off = offset ).
      IF lv_end < 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: the cross-reference table has no trailer`.
      ENDIF.
      DATA(lt_token) = tokens( text( off = offset + 4 len = lv_end - offset - 4 ) ).
      DATA(lv_index) = 1.
      WHILE lv_index + 1 <= lines( lt_token ).
        lv_num = lt_token[ lv_index ]-text.
        lv_count = lt_token[ lv_index + 1 ]-text.
        lv_index = lv_index + 2.
        DO lv_count TIMES.
          IF lv_index + 2 > lines( lt_token ).
            EXIT.
          ENDIF.
          " a newer section came first: it wins
          DATA(lv_kind) = lt_token[ lv_index + 2 ]-text.
          DATA(lv_known) = xsdbool( line_exists( mt_entry[ num = lv_num ] ) ).
          IF lv_kind = `n` AND lv_known = abap_false.
            CLEAR ls_entry.
            ls_entry-num = lv_num.
            ls_entry-kind = 1.
            ls_entry-offset = lt_token[ lv_index ]-text.
            INSERT ls_entry INTO TABLE mt_entry.
          ELSEIF lv_kind = `f` AND lv_known = abap_false.
            INSERT VALUE #( num = lv_num kind = 0 ) INTO TABLE mt_entry.
          ENDIF.
          lv_index = lv_index + 3.
          lv_num = lv_num + 1.
        ENDDO.
      ENDWHILE.
      DATA(lv_next) = find_bytes( val = `startxref`
                                  off = lv_end ).
      lv_trailer = text( off = lv_end + 7
                         len = COND #( WHEN lv_next > 0 THEN lv_next - lv_end - 7 ELSE 4096 ) ).
      " a hybrid file: a stream with the compressed objects as well
      DATA(lv_stream) = dict_value( dict = lv_trailer
                                    key  = `XRefStm` ).
      IF lv_stream IS NOT INITIAL.
        read_xref( CONV i( lv_stream ) ).
      ENDIF.
    ELSE.
      " a cross-reference stream
      DATA(ls_object) = object_at( offset ).
      lv_trailer = ls_object-text.
      IF dict_value( dict = lv_trailer
                     key  = `Type` ) <> `/XRef`.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: the cross-reference of the file is damaged`.
      ENDIF.
      DATA(lv_data) = decode( ls_object ).
      DATA lt_width TYPE STANDARD TABLE OF i WITH EMPTY KEY.
      LOOP AT tokens( dict_value( dict = lv_trailer
                                  key  = `W` ) ) INTO DATA(ls_w) WHERE kind = `N`.
        INSERT CONV i( ls_w-text ) INTO TABLE lt_width.
      ENDLOOP.
      DATA(lv_index_text) = dict_value( dict = lv_trailer
                                        key  = `Index` ).
      IF lv_index_text IS INITIAL.
        lv_index_text = |[0 { dict_value( dict = lv_trailer key = `Size` ) }]|.
      ENDIF.
      DATA lt_section TYPE STANDARD TABLE OF i WITH EMPTY KEY.
      LOOP AT tokens( lv_index_text ) INTO DATA(ls_i) WHERE kind = `N`.
        INSERT CONV i( ls_i-text ) INTO TABLE lt_section.
      ENDLOOP.
      IF lines( lt_width ) <> 3.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: the cross-reference stream is damaged`.
      ENDIF.
      DATA(lv_row) = lt_width[ 1 ] + lt_width[ 2 ] + lt_width[ 3 ].
      DATA(lv_pos) = 0.
      DATA(lv_section) = 1.
      WHILE lv_section + 1 <= lines( lt_section ).
        lv_num = lt_section[ lv_section ].
        lv_count = lt_section[ lv_section + 1 ].
        DO lv_count TIMES.
          IF lv_pos + lv_row > xstrlen( lv_data ).
            EXIT.
          ENDIF.
          " the fields, big-endian; a type of width 0 is 1
          DATA lv_field TYPE i.
          DATA lt_field TYPE STANDARD TABLE OF i WITH EMPTY KEY.
          CLEAR lt_field.
          LOOP AT lt_width INTO DATA(lv_width).
            lv_field = 0.
            DO lv_width TIMES.
              DATA lv_byte TYPE x LENGTH 1.
              lv_byte = lv_data+lv_pos(1).
              lv_field = lv_field * 256 + lv_byte.
              lv_pos = lv_pos + 1.
            ENDDO.
            IF sy-tabix = 1 AND lv_width = 0.
              lv_field = 1.
            ENDIF.
            INSERT lv_field INTO TABLE lt_field.
          ENDLOOP.
          IF NOT line_exists( mt_entry[ num = lv_num ] ).
            CLEAR ls_entry.
            ls_entry-num = lv_num.
            ls_entry-kind = lt_field[ 1 ].
            IF ls_entry-kind = 1.
              ls_entry-offset = lt_field[ 2 ].
            ELSEIF ls_entry-kind = 2.
              ls_entry-stream = lt_field[ 2 ].
              ls_entry-index = lt_field[ 3 ].
            ENDIF.
            INSERT ls_entry INTO TABLE mt_entry.
          ENDIF.
          lv_num = lv_num + 1.
        ENDDO.
        lv_section = lv_section + 2.
      ENDWHILE.
    ENDIF.

    IF dict_value( dict = lv_trailer
                   key  = `Encrypt` ) IS NOT INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: an encrypted file cannot be imported`.
    ENDIF.
    IF mv_root = 0.
      mv_root = ref( dict_value( dict = lv_trailer
                                 key  = `Root` ) ).
    ENDIF.
    " an incremental update: the sections before it
    DATA(lv_prev) = dict_value( dict = lv_trailer
                                key  = `Prev` ).
    IF lv_prev IS NOT INITIAL AND CONV i( lv_prev ) <> offset.
      read_xref( CONV i( lv_prev ) ).
    ENDIF.

  ENDMETHOD.

  METHOD object_at.

    DATA lv_hex TYPE string.

    " n g obj, the value, then stream or endobj
    DATA(lv_stream) = find_bytes( val = `stream`
                                  off = offset ).
    DATA(lv_end) = find_bytes( val = `endobj`
                               off = offset ).
    IF lv_end < 0.
      lv_end = xstrlen( mv_content ).
    ENDIF.
    DATA(lv_head) = text( off = offset
                          len = nmin( val1 = 64
                                      val2 = lv_end - offset ) ).
    DATA(lv_obj) = find( val = lv_head
                         sub = `obj` ).
    IF lv_obj < 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: there is no object at { offset }|.
    ENDIF.
    DATA(lv_start) = offset + lv_obj + 3.

    IF lv_stream >= 0 AND lv_stream < lv_end.
      result-is_stream = abap_true.
      result-text = text( off = lv_start
                          len = lv_stream - lv_start ).
      DATA(lv_dict_length) = lv_stream - lv_start.
      lv_hex = mv_content+lv_start(lv_dict_length).
      result-hex = lv_hex.
      " the data starts after the end of the line of the keyword
      DATA(lv_data) = lv_stream + 6.
      IF text( off = lv_data len = 1 ) = |\r|.
        lv_data = lv_data + 1.
      ENDIF.
      IF text( off = lv_data len = 1 ) = |\n|.
        lv_data = lv_data + 1.
      ENDIF.
      DATA(lv_length_text) = resolve( dict_value( dict = result-text
                                                  key  = `Length` ) ).
      DATA lv_length TYPE i.
      lv_length_text = condense( lv_length_text ).
      IF lv_length_text CO `0123456789` AND lv_length_text IS NOT INITIAL.
        lv_length = lv_length_text.
      ENDIF.
      IF lv_length <= 0 OR lv_data + lv_length > xstrlen( mv_content ).
        " a wrong length: up to endstream
        DATA(lv_endstream) = find_bytes( val = `endstream`
                                         off = lv_data ).
        lv_length = lv_endstream - lv_data.
      ENDIF.
      result-data = mv_content+lv_data(lv_length).
    ELSE.
      result-text = text( off = lv_start
                          len = lv_end - lv_start ).
      DATA(lv_value_length) = lv_end - lv_start.
      lv_hex = mv_content+lv_start(lv_value_length).
      result-hex = lv_hex.
    ENDIF.

  ENDMETHOD.

  METHOD object.

    DATA lv_first TYPE i.

    ASSIGN mt_entry[ num = num ] TO FIELD-SYMBOL(<entry>).
    IF sy-subrc <> 0 OR <entry>-kind = 0.
      result-text = `null`.
      result-hex = zabaputil_cl_pdf_writer=>hex( `null` ).
      RETURN.
    ENDIF.
    IF <entry>-kind = 1.
      result = object_at( <entry>-offset ).
      RETURN.
    ENDIF.

    " in an object stream: pairs of number and offset, then the objects
    DATA(ls_stream) = object( <entry>-stream ).
    DATA(lv_data) = decode( ls_stream ).
    lv_first = dict_value( dict = ls_stream-text
                           key  = `First` ).
    DATA(lt_head) = tokens( text_of( data = lv_data
                                     off  = 0
                                     len  = lv_first ) ).
    DATA(lv_pair) = <entry>-index * 2 + 1.
    IF lv_pair + 1 > lines( lt_head ).
      result-text = `null`.
      result-hex = zabaputil_cl_pdf_writer=>hex( `null` ).
      RETURN.
    ENDIF.
    DATA(lv_off) = lv_first + CONV i( lt_head[ lv_pair + 1 ]-text ).
    DATA(lv_next) = xstrlen( lv_data ).
    IF lv_pair + 3 <= lines( lt_head ).
      lv_next = lv_first + CONV i( lt_head[ lv_pair + 3 ]-text ).
    ENDIF.
    result-text = text_of( data = lv_data
                           off  = lv_off
                           len  = lv_next - lv_off ).
    DATA lv_hex TYPE string.
    DATA(lv_object_length) = lv_next - lv_off.
    lv_hex = lv_data+lv_off(lv_object_length).
    result-hex = lv_hex.

  ENDMETHOD.

  METHOD decode.

    DATA lv_row     TYPE i.
    DATA lv_columns TYPE i.

    result = object-data.
    DATA(lv_filter) = dict_value( dict = object-text
                                  key  = `Filter` ).
    IF lv_filter IS INITIAL.
      RETURN.
    ENDIF.
    lv_filter = condense( replace( val  = replace( val = lv_filter sub = `[` with = `` )
                                   sub  = `]`
                                   with = `` ) ).
    IF lv_filter <> `/FlateDecode`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: a stream with the filter { lv_filter } cannot be read|.
    ENDIF.
    result = zabaputil_cl_util_context=>zlib_decompress( result ).

    " a PNG predictor: every row starts with its filter byte
    DATA(lv_parms) = dict_value( dict = object-text
                                 key  = `DecodeParms` ).
    lv_parms = condense( replace( val  = replace( val = lv_parms sub = `[` with = `` )
                                  sub  = `]`
                                  with = `` ) ).
    IF lv_parms IS INITIAL OR dict_value( dict = lv_parms key = `Predictor` ) IS INITIAL
        OR CONV i( dict_value( dict = lv_parms key = `Predictor` ) ) < 10.
      RETURN.
    ENDIF.
    lv_columns = 1.
    IF dict_value( dict = lv_parms key = `Columns` ) IS NOT INITIAL.
      lv_columns = dict_value( dict = lv_parms key = `Columns` ).
    ENDIF.
    DATA lt_prev TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lt_now  TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lt_out  TYPE string_table.
    DATA lv_x    TYPE x LENGTH 1.
    DATA lv_filter_type TYPE i.
    DATA lv_value TYPE i.
    DATA lv_a TYPE i.
    DATA lv_b TYPE i.
    DATA lv_c TYPE i.
    DO lv_columns TIMES.
      INSERT 0 INTO TABLE lt_prev.
    ENDDO.
    DATA(lv_pos) = 0.
    WHILE lv_pos + 1 + lv_columns <= xstrlen( result ).
      lv_x = result+lv_pos(1).
      lv_filter_type = lv_x.
      lv_pos = lv_pos + 1.
      CLEAR lt_now.
      DATA lv_line TYPE string.
      CLEAR lv_line.
      lv_row = 0.
      WHILE lv_row < lv_columns.
        lv_x = result+lv_pos(1).
        lv_value = lv_x.
        lv_a = COND #( WHEN lv_row > 0 THEN lt_now[ lv_row ] ELSE 0 ).
        lv_b = lt_prev[ lv_row + 1 ].
        lv_c = COND #( WHEN lv_row > 0 THEN lt_prev[ lv_row ] ELSE 0 ).
        CASE lv_filter_type.
          WHEN 1.
            lv_value = lv_value + lv_a.
          WHEN 2.
            lv_value = lv_value + lv_b.
          WHEN 3.
            lv_value = lv_value + ( lv_a + lv_b ) DIV 2.
          WHEN 4.
            DATA(lv_p) = lv_a + lv_b - lv_c.
            IF abs( lv_p - lv_a ) <= abs( lv_p - lv_b ) AND abs( lv_p - lv_a ) <= abs( lv_p - lv_c ).
              lv_value = lv_value + lv_a.
            ELSEIF abs( lv_p - lv_b ) <= abs( lv_p - lv_c ).
              lv_value = lv_value + lv_b.
            ELSE.
              lv_value = lv_value + lv_c.
            ENDIF.
        ENDCASE.
        lv_value = lv_value MOD 256.
        INSERT lv_value INTO TABLE lt_now.
        lv_x = lv_value.
        lv_line = lv_line && lv_x.
        lv_pos = lv_pos + 1.
        lv_row = lv_row + 1.
      ENDWHILE.
      INSERT lv_line INTO TABLE lt_out.
      lt_prev = lt_now.
    ENDWHILE.
    result = concat_lines_of( lt_out ).

  ENDMETHOD.

  METHOD resolve.

    DATA(lv_num) = ref( val ).
    IF lv_num = 0.
      result = val.
      RETURN.
    ENDIF.
    result = condense( object( lv_num )-text ).

  ENDMETHOD.

  METHOD collect_pages.

    " a tree that points back at itself must not be followed for ever
    IF depth > 32 OR num = 0.
      RETURN.
    ENDIF.
    DATA(lv_text) = object( num )-text.
    DATA(lv_type) = dict_value( dict = lv_text
                                key  = `Type` ).
    IF lv_type = `/Page`.
      INSERT num INTO TABLE mt_page.
      RETURN.
    ENDIF.
    DATA(lt_token) = tokens( resolve( dict_value( dict = lv_text
                                                  key  = `Kids` ) ) ).
    DATA(lv_index) = 1.
    WHILE lv_index + 2 <= lines( lt_token ).
      IF lt_token[ lv_index ]-kind = `N` AND lt_token[ lv_index + 2 ]-text = `R`.
        collect_pages( num   = CONV i( lt_token[ lv_index ]-text )
                       depth = depth + 1 ).
        lv_index = lv_index + 3.
      ELSE.
        lv_index = lv_index + 1.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD inherited.

    DATA(lv_num) = num.
    DO 32 TIMES.
      DATA(lv_text) = object( lv_num )-text.
      result = dict_value( dict = lv_text
                           key  = key ).
      IF result IS NOT INITIAL.
        RETURN.
      ENDIF.
      lv_num = ref( dict_value( dict = lv_text
                                key  = `Parent` ) ).
      IF lv_num = 0.
        RETURN.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD renumber.

    DATA lt_part TYPE string_table.
    DATA lv_last TYPE i.
    DATA ls_map  TYPE ty_s_map.

    " every n g R: the number in the new file, a new one for an object
    " not copied yet
    DATA(lt_token) = tokens( text ).
    DATA(lv_index) = 1.
    WHILE lv_index + 2 <= lines( lt_token ).
      IF lt_token[ lv_index ]-kind = `N` AND lt_token[ lv_index + 1 ]-kind = `N` AND lt_token[ lv_index + 2 ]-text = `R`.
        DATA(lv_old) = CONV i( lt_token[ lv_index ]-text ).
        IF line_exists( mt_map[ old = lv_old ] ).
          ls_map = mt_map[ old = lv_old ].
        ELSE.
          ls_map-old = lv_old.
          ls_map-new = writer->reserve( ).
          INSERT ls_map INTO TABLE mt_map.
          INSERT ls_map INTO TABLE mt_queue.
        ENDIF.
        DATA(lv_from) = lt_token[ lv_index ]-off.
        DATA(lv_to) = lt_token[ lv_index + 2 ]-off + 1.
        INSERT substring( val = hex
                          off = lv_last * 2
                          len = ( lv_from - lv_last ) * 2 ) INTO TABLE lt_part.
        INSERT zabaputil_cl_pdf_writer=>hex( |{ ls_map-new } 0 R| ) INTO TABLE lt_part.
        lv_last = lv_to.
        lv_index = lv_index + 3.
      ELSE.
        lv_index = lv_index + 1.
      ENDIF.
    ENDWHILE.
    INSERT substring( val = hex
                      off = lv_last * 2 ) INTO TABLE lt_part.
    result = concat_lines_of( lt_part ).

  ENDMETHOD.

  METHOD write.

    DATA lt_data  TYPE string_table.
    DATA lv_hex   TYPE string.

    DATA(lv_page) = mt_page[ mv_page ].

    " the content: one stream or several, decoded and joined
    DATA(lv_contents) = dict_value( dict = object( lv_page )-text
                                    key  = `Contents` ).
    DATA(lt_token) = tokens( resolve( lv_contents ) ).
    IF ref( lv_contents ) > 0 AND object( ref( lv_contents ) )-is_stream = abap_true.
      lt_token = tokens( lv_contents ).
    ENDIF.
    DATA(lv_index) = 1.
    WHILE lv_index + 2 <= lines( lt_token ).
      IF lt_token[ lv_index ]-kind = `N` AND lt_token[ lv_index + 2 ]-text = `R`.
        DATA(ls_content) = object( CONV i( lt_token[ lv_index ]-text ) ).
        IF ls_content-is_stream = abap_true.
          lv_hex = decode( ls_content ).
          INSERT lv_hex INTO TABLE lt_data.
          INSERT zabaputil_cl_pdf_writer=>hex( |\n| ) INTO TABLE lt_data.
        ENDIF.
        lv_index = lv_index + 3.
      ELSE.
        lv_index = lv_index + 1.
      ENDIF.
    ENDWHILE.
    DATA lv_content TYPE xstring.
    lv_content = concat_lines_of( lt_data ).

    " the resources, of the page or the pages above it, with what they
    " refer to - copied with new numbers
    DATA(lv_resources) = inherited( num = lv_page
                                    key = `Resources` ).
    IF ref( lv_resources ) > 0.
      lv_resources = condense( object( ref( lv_resources ) )-text ).
    ENDIF.
    IF lv_resources IS INITIAL.
      lv_resources = `<< >>`.
    ENDIF.
    CLEAR mt_map.
    CLEAR mt_queue.
    DATA(lv_resources_hex) = renumber( hex    = zabaputil_cl_pdf_writer=>hex( lv_resources )
                                       text   = lv_resources
                                       writer = writer ).

    WHILE mt_queue IS NOT INITIAL.
      DATA(ls_next) = mt_queue[ 1 ].
      DELETE mt_queue INDEX 1.
      DATA(ls_object) = object( ls_next-old ).
      " the page tree is not copied: a page of the old file is no page here
      DATA(lv_type) = dict_value( dict = ls_object-text
                                  key  = `Type` ).
      IF lv_type = `/Page` OR lv_type = `/Pages` OR lv_type = `/Catalog`.
        writer->set_raw( id  = ls_next-new
                         hex = zabaputil_cl_pdf_writer=>hex( `null` ) ).
        CONTINUE.
      ENDIF.
      lv_hex = renumber( hex    = ls_object-hex
                         text   = ls_object-text
                         writer = writer ).
      IF ls_object-is_stream = abap_true.
        DATA lv_bytes TYPE string.
        lv_bytes = ls_object-data.
        lv_hex = lv_hex && zabaputil_cl_pdf_writer=>hex( |stream\n| ) && lv_bytes &&
                 zabaputil_cl_pdf_writer=>hex( |\nendstream| ).
      ENDIF.
      writer->set_raw( id  = ls_next-new
                       hex = lv_hex ).
    ENDWHILE.

    " the form, in the size of the page; its resources as copied
    DATA(lv_resources_text) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( lv_resources_hex ) ).
    result = writer->add_stream( dict    = |/Type /XObject /Subtype /Form /BBox { mv_box } /Resources { lv_resources_text }|
                                 content = lv_content ).

  ENDMETHOD.

ENDCLASS.

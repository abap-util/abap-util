CLASS zabaputil_cl_util_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS delete_by_handle
      IMPORTING
        uname        TYPE clike OPTIONAL
        handle       TYPE clike OPTIONAL
        handle2      TYPE clike OPTIONAL
        handle3      TYPE clike OPTIONAL
        check_commit TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS save
      IMPORTING
        uname         TYPE clike OPTIONAL
        handle        TYPE clike OPTIONAL
        handle2       TYPE clike OPTIONAL
        handle3       TYPE clike OPTIONAL
        data          TYPE any
        check_commit  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS load_by_id
      IMPORTING
        id            TYPE clike OPTIONAL
      EXPORTING
        VALUE(result) TYPE any.

    CLASS-METHODS load_by_handle
      IMPORTING
        uname         TYPE clike OPTIONAL
        handle        TYPE clike OPTIONAL
        handle2       TYPE clike OPTIONAL
        handle3       TYPE clike OPTIONAL
      EXPORTING
        VALUE(result) TYPE any.

    TYPES ty_s_db TYPE zabaputil_t_91.
    TYPES ty_t_db TYPE STANDARD TABLE OF ty_s_db WITH DEFAULT KEY.

    " only supplied parameters filter the selection, initial values of
    " unsupplied parameters do not restrict the result
    CLASS-METHODS load_multi_by_handle
      IMPORTING
        uname         TYPE clike OPTIONAL
        handle        TYPE clike OPTIONAL
        handle2       TYPE clike OPTIONAL
        handle3       TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_db.

  PROTECTED SECTION.
  PRIVATE SECTION.

    " The id of a new row, derived from its handles - see save
    CLASS-METHODS get_id_by_handle
      IMPORTING
        is_db         TYPE ty_s_db
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zabaputil_cl_util_db IMPLEMENTATION.



  METHOD delete_by_handle.

    DELETE FROM zabaputil_t_91
        WHERE
           uname = uname
            AND handle = handle
            AND handle2 = handle2
            AND handle3 = handle3.

    IF check_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD load_by_handle.

    DATA lv_data TYPE zabaputil_t_91-data.
    SELECT SINGLE data
      FROM zabaputil_t_91 INTO lv_data
       WHERE
        uname = uname
        AND handle = handle
        AND handle2 = handle2
        AND handle3 = handle3 "#EC WARNOK
      .
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `NO_ENTRY_FOR_HANDLE_EXISTS`.
    ENDIF.

    zabaputil_cl_util_context=>xml_parse(
      EXPORTING
        xml = lv_data
      IMPORTING
        any = result ).

  ENDMETHOD.


  METHOD load_multi_by_handle.

    DATA lr_uname   TYPE RANGE OF zabaputil_t_91-uname.
    DATA lr_handle  TYPE RANGE OF zabaputil_t_91-handle.
    DATA lr_handle2 TYPE RANGE OF zabaputil_t_91-handle2.
    DATA lr_handle3 TYPE RANGE OF zabaputil_t_91-handle3.
      DATA temp13 LIKE lr_uname.
      DATA temp14 LIKE LINE OF temp13.
      DATA temp15 LIKE lr_handle.
      DATA temp16 LIKE LINE OF temp15.
      DATA temp17 LIKE lr_handle2.
      DATA temp18 LIKE LINE OF temp17.
      DATA temp19 LIKE lr_handle3.
      DATA temp20 LIKE LINE OF temp19.

    IF uname IS SUPPLIED.
      
      CLEAR temp13.
      
      temp14-sign = `I`.
      temp14-option = `EQ`.
      temp14-low = uname.
      INSERT temp14 INTO TABLE temp13.
      lr_uname = temp13.
    ENDIF.
    IF handle IS SUPPLIED.
      
      CLEAR temp15.
      
      temp16-sign = `I`.
      temp16-option = `EQ`.
      temp16-low = handle.
      INSERT temp16 INTO TABLE temp15.
      lr_handle = temp15.
    ENDIF.
    IF handle2 IS SUPPLIED.
      
      CLEAR temp17.
      
      temp18-sign = `I`.
      temp18-option = `EQ`.
      temp18-low = handle2.
      INSERT temp18 INTO TABLE temp17.
      lr_handle2 = temp17.
    ENDIF.
    IF handle3 IS SUPPLIED.
      
      CLEAR temp19.
      
      temp20-sign = `I`.
      temp20-option = `EQ`.
      temp20-low = handle3.
      INSERT temp20 INTO TABLE temp19.
      lr_handle3 = temp19.
    ENDIF.

    SELECT * FROM zabaputil_t_91 INTO CORRESPONDING FIELDS OF TABLE result
      
      WHERE uname   IN lr_uname
        AND handle  IN lr_handle
        AND handle2 IN lr_handle2
        AND handle3 IN lr_handle3 "#EC WARNOK
      .

  ENDMETHOD.


  METHOD load_by_id.

    DATA lv_data TYPE zabaputil_t_91-data.
    SELECT SINGLE data
      FROM zabaputil_t_91 INTO lv_data
      WHERE id = id  "#EC WARNOK
      .
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |NO_ENTRY_FOR_ID_EXISTS: { id }|.
    ENDIF.

    zabaputil_cl_util_context=>xml_parse(
      EXPORTING
        xml = lv_data
      IMPORTING
        any = result ).

  ENDMETHOD.


  METHOD save.

    DATA lv_id TYPE zabaputil_t_91-id.
    DATA temp21 TYPE zabaputil_t_91.
    DATA ls_db LIKE temp21.
        DATA x TYPE REF TO cx_transformation_error.
    SELECT SINGLE id
      FROM zabaputil_t_91 INTO lv_id
       WHERE
        uname = uname
        AND handle = handle
        AND handle2 = handle2 "#EC WARNOK
        AND handle3 = handle3
       ##SUBRC_OK.

    
    CLEAR temp21.
    temp21-uname = uname.
    temp21-handle = handle.
    temp21-handle2 = handle2.
    temp21-handle3 = handle3.
    
    ls_db = temp21.
    " the declared static exception of xml_stringify (data it cannot
    " serialize) left save( ) as CX_SY_NO_HANDLER
    TRY.
        ls_db-data = zabaputil_cl_util_context=>xml_stringify( data ).
        
      CATCH cx_transformation_error INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val      = `DB_SAVE_FAILED - the data cannot be serialized`
                    previous = x.
    ENDTRY.

    " The table's key is the id alone, and nothing locks the handles: two
    " saves of the same handles running in parallel both found no row,
    " each inserted one under a random id, and from then on save and
    " load_by_handle picked either row - a lost update that looked like
    " random data. An id derived from the handles makes the second MODIFY
    " an update of the first. A row that already exists keeps its id
    IF lv_id IS NOT INITIAL.
      ls_db-id = lv_id.
    ELSE.
      ls_db-id = get_id_by_handle( ls_db ).
    ENDIF.

    MODIFY zabaputil_t_91 FROM ls_db.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `DB_SAVE_FAILED`.
    ENDIF.

    IF check_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    result = ls_db-id.

  ENDMETHOD.


  METHOD get_id_by_handle.

    " each handle with its length in front, so that no two tuples give the
    " same text; SHA-256 in hex, its first 32 characters fill the id
    DATA lv_key TYPE string.
    DATA lv_hash TYPE string.
    lv_key = |{ strlen( is_db-uname ) }:{ is_db-uname }|
                && |{ strlen( is_db-handle ) }:{ is_db-handle }|
                && |{ strlen( is_db-handle2 ) }:{ is_db-handle2 }|
                && |{ strlen( is_db-handle3 ) }:{ is_db-handle3 }|.
    
    lv_hash = to_upper( zabaputil_cl_util_context=>hash_string( lv_key ) ).

    " no digest class answers: hash_string gives the text back - a random
    " id then, as before
    IF strlen( lv_hash ) = 64 AND lv_hash CO `0123456789ABCDEF`.
      result = lv_hash(32).
    ELSE.
      result = zabaputil_cl_util_context=>uuid_get_c32( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

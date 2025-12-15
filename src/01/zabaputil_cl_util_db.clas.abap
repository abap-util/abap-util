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

  PROTECTED SECTION.
  PRIVATE SECTION.
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

    DATA lt_db TYPE STANDARD TABLE OF zabaputil_t_91 WITH DEFAULT KEY.

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

    zabaputil_cl_util=>xml_parse(
      EXPORTING
        xml = lv_data
      IMPORTING
        any = result ).

  ENDMETHOD.


  METHOD load_by_id.

    DATA lt_db TYPE STANDARD TABLE OF zabaputil_t_91 WITH DEFAULT KEY.

    DATA lv_data TYPE zabaputil_t_91-data.
    SELECT SINGLE data
      FROM zabaputil_t_91 INTO lv_data
      WHERE id = id  "#EC WARNOK
      .
    ASSERT sy-subrc = 0.

    zabaputil_cl_util=>xml_parse(
      EXPORTING
        xml = lv_data
      IMPORTING
        any = result ).

  ENDMETHOD.


  METHOD save.

    DATA lt_db TYPE STANDARD TABLE OF zabaputil_t_91 WITH DEFAULT KEY.
    DATA lv_id TYPE zabaputil_t_91-id.
    SELECT SINGLE id
      FROM zabaputil_t_91 INTO lv_id
       WHERE
        uname = uname
        AND handle = handle
        AND handle2 = handle2 "#EC WARNOK
        AND handle3 = handle3
       ##SUBRC_OK.

    DATA temp1 TYPE zabaputil_t_91.
    CLEAR temp1.
    temp1-uname = uname.
    temp1-handle = handle.
    temp1-handle2 = handle2.
    temp1-handle3 = handle3.
    temp1-data = zabaputil_cl_util=>xml_stringify( data ).
    DATA ls_db LIKE temp1.
    ls_db = temp1.

    IF lv_id IS NOT INITIAL.
      ls_db-id = lv_id.
    ELSE.
      ls_db-id = zabaputil_cl_util=>uuid_get_c32( ).
    ENDIF.

    MODIFY zabaputil_t_91 FROM ls_db.
    ASSERT sy-subrc = 0.

    IF check_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    result = ls_db-id.

  ENDMETHOD.

ENDCLASS.

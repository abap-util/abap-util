CLASS zabaputil_cl_util_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_log.

    METHODS info
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_log.

    METHODS error
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_log.

    METHODS warning
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_log.

    METHODS success
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_log.

    METHODS clear
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_log.

    METHODS has_error
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS count
      RETURNING
        VALUE(result) TYPE i.

    METHODS bal_save
      IMPORTING
        object    TYPE clike
        subobject TYPE clike
        id        TYPE clike.

    METHODS bal_read
      IMPORTING
        object    TYPE clike
        subobject TYPE clike
        id        TYPE clike.

    METHODS to_csv
      RETURNING
        VALUE(result) TYPE string.

    " the log as an Excel workbook (conv_get_xlsx_by_itab). An xstring
    " since 2026-11: it was declared as a string while the method always
    " raised "not implemented", so no caller can have used its value
    METHODS to_xlsx
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS to_msg
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_t_msg.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
    DATA mt_log TYPE zabaputil_cl_util_context=>ty_t_msg.

  PRIVATE SECTION.
ENDCLASS.


CLASS zabaputil_cl_util_log IMPLEMENTATION.

  METHOD add.

    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
    lt_msg = zabaputil_cl_util_context=>msg_get_t( val ).
    INSERT LINES OF lt_msg INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD info.

    DATA temp17 TYPE zabaputil_cl_util_context=>ty_s_msg.
    CLEAR temp17.
    temp17-type = `I`.
    temp17-text = val.
    INSERT temp17 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD error.

    DATA temp18 TYPE zabaputil_cl_util_context=>ty_s_msg.
    CLEAR temp18.
    temp18-type = `E`.
    temp18-text = val.
    INSERT temp18 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD warning.

    DATA temp19 TYPE zabaputil_cl_util_context=>ty_s_msg.
    CLEAR temp19.
    temp19-type = `W`.
    temp19-text = val.
    INSERT temp19 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD success.

    DATA temp20 TYPE zabaputil_cl_util_context=>ty_s_msg.
    CLEAR temp20.
    temp20-type = `S`.
    temp20-text = val.
    INSERT temp20 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD clear.

    CLEAR mt_log.
    result = me.

  ENDMETHOD.

  METHOD has_error.

    " A (abort) and X (exit) are errors as well - a log that holds only
    " an abort message reported "no error"
    DATA temp21 LIKE sy-subrc.
    DATA temp3 LIKE sy-subrc.
    DATA temp1 LIKE sy-subrc.
    DATA temp2 TYPE xsdboolean.
    READ TABLE mt_log WITH KEY type = `E` TRANSPORTING NO FIELDS.
    temp21 = sy-subrc.
    
    READ TABLE mt_log WITH KEY type = `A` TRANSPORTING NO FIELDS.
    temp3 = sy-subrc.
    
    READ TABLE mt_log WITH KEY type = `X` TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.
    
    temp2 = boolc( temp21 = 0 OR temp3 = 0 OR temp1 = 0 ).
    result = temp2.

  ENDMETHOD.

  METHOD count.

    result = lines( mt_log ).

  ENDMETHOD.

  METHOD bal_read.

    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
    lt_msg = zabaputil_cl_util_context=>bal_read(
         object    = object
         subobject = subobject
         id        = id ).
    INSERT LINES OF lt_msg INTO TABLE mt_log.

  ENDMETHOD.

  METHOD bal_save.

    zabaputil_cl_util_context=>bal_create(
        object    = object
        subobject = subobject
        id        = id
        t_log     = mt_log ).

  ENDMETHOD.

  METHOD to_csv.

    result = zabaputil_cl_util_context=>itab_get_csv_by_itab( mt_log ).

  ENDMETHOD.

  METHOD to_xlsx.

    result = zabaputil_cl_util_context=>conv_get_xlsx_by_itab( mt_log ).

  ENDMETHOD.

  METHOD to_msg.

    result = mt_log.

  ENDMETHOD.

  METHOD to_string.

    DATA ls_msg LIKE LINE OF mt_log.
    LOOP AT mt_log INTO ls_msg.
      IF result IS NOT INITIAL.
        result = |{ result }\n|.
      ENDIF.
      result = |{ result }[{ ls_msg-type }] { ls_msg-text }|.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

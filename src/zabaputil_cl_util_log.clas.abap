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

    METHODS to_xlsx
      RETURNING
        VALUE(result) TYPE string.

    METHODS to_msg
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util=>ty_t_msg.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
    DATA mt_log TYPE zabaputil_cl_util=>ty_t_msg.

  PRIVATE SECTION.
ENDCLASS.


CLASS zabaputil_cl_util_log IMPLEMENTATION.

  METHOD add.

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = zabaputil_cl_util=>msg_get_t( val ).
    INSERT LINES OF lt_msg INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD info.

    DATA temp1 TYPE zabaputil_cl_util=>ty_s_msg.
    CLEAR temp1.
    temp1-type = `I`.
    temp1-text = val.
    INSERT temp1 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD error.

    DATA temp2 TYPE zabaputil_cl_util=>ty_s_msg.
    CLEAR temp2.
    temp2-type = `E`.
    temp2-text = val.
    INSERT temp2 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD warning.

    DATA temp3 TYPE zabaputil_cl_util=>ty_s_msg.
    CLEAR temp3.
    temp3-type = `W`.
    temp3-text = val.
    INSERT temp3 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD success.

    DATA temp4 TYPE zabaputil_cl_util=>ty_s_msg.
    CLEAR temp4.
    temp4-type = `S`.
    temp4-text = val.
    INSERT temp4 INTO TABLE mt_log.
    result = me.

  ENDMETHOD.

  METHOD clear.

    CLEAR mt_log.
    result = me.

  ENDMETHOD.

  METHOD has_error.

    DATA temp5 LIKE sy-subrc.
    READ TABLE mt_log WITH KEY type = `E` TRANSPORTING NO FIELDS.
    temp5 = sy-subrc.
    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( temp5 = 0 ).
    result = temp1.

  ENDMETHOD.

  METHOD count.

    result = lines( mt_log ).

  ENDMETHOD.

  METHOD bal_read.

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = zabaputil_cl_util=>bal_read(
         object    = object
         subobject = subobject
         id        = id ).
    INSERT LINES OF lt_msg INTO TABLE mt_log.

  ENDMETHOD.

  METHOD bal_save.

    zabaputil_cl_util=>bal_save(
        object    = object
        subobject = subobject
        id        = id
        t_log     = mt_log ).

  ENDMETHOD.

  METHOD to_csv.

    result = zabaputil_cl_util=>itab_get_csv_by_itab( mt_log ).

  ENDMETHOD.

  METHOD to_xlsx.

    result = zabaputil_cl_util=>conv_get_xlsx_by_itab( mt_log ).

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

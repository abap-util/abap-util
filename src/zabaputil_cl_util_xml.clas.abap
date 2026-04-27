CLASS zabaputil_cl_util_xml DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.

    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS constructor.

    METHODS __
      IMPORTING
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util=>ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS _
      IMPORTING
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util=>ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS _if
      IMPORTING
        when          TYPE abap_bool
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util=>ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS __if
      IMPORTING
        when          TYPE abap_bool
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util=>ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS p
      IMPORTING
        n             TYPE clike
        v             TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS n
      IMPORTING
        name          TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS n_prev
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS n_root
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS stringify
      IMPORTING
        from_root     TYPE abap_bool DEFAULT abap_true
        indent        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

    DATA mv_name   TYPE string.
    DATA mv_ns     TYPE string.
    DATA mt_prop   TYPE SORTED TABLE OF zabaputil_cl_util=>ty_s_name_value WITH NON-UNIQUE KEY n.

    DATA mo_root   TYPE REF TO zabaputil_cl_util_xml.
    DATA mo_previous TYPE REF TO zabaputil_cl_util_xml.
    DATA mo_parent TYPE REF TO zabaputil_cl_util_xml.
    DATA mt_child  TYPE STANDARD TABLE OF REF TO zabaputil_cl_util_xml WITH DEFAULT KEY.

    METHODS xml_get_parts
      CHANGING
        ct_parts TYPE string_table.

    METHODS xml_get_parts_indent
      IMPORTING
        iv_depth TYPE i DEFAULT 0
      CHANGING
        ct_parts TYPE string_table.

  PRIVATE SECTION.

ENDCLASS.


CLASS zabaputil_cl_util_xml IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

  METHOD factory.

    CREATE OBJECT result.

    result->mo_root   = result.
    result->mo_parent = result.

  ENDMETHOD.

  METHOD __.

    DATA lo_child TYPE REF TO zabaputil_cl_util_xml.
    CREATE OBJECT lo_child TYPE zabaputil_cl_util_xml.
    lo_child->mv_name   = n.
    lo_child->mv_ns     = ns.
    lo_child->mt_prop   = p.
    IF a IS NOT INITIAL.
      DATA temp1 TYPE zabaputil_cl_util=>ty_s_name_value.
      CLEAR temp1.
      temp1-n = a.
      temp1-v = v.
      INSERT temp1 INTO TABLE lo_child->mt_prop.
    ENDIF.
    lo_child->mo_parent = me.
    lo_child->mo_root   = mo_root.
    INSERT lo_child INTO TABLE mt_child.

    mo_root->mo_previous = lo_child.
    result = lo_child.

  ENDMETHOD.

  METHOD _.

    result = me.
    __( n  = n
        ns = ns
        a  = a
        v  = v
        p  = p ).

  ENDMETHOD.

  METHOD _if.

    IF when = abap_true.
      result = __( n  = n
                   ns = ns
                   a  = a
                   v  = v
                   p  = p ).
    ELSE.
      result = me.
    ENDIF.

  ENDMETHOD.

  METHOD __if.

    IF when = abap_true.
      _( n  = n
         ns = ns
         a  = a
         v  = v
         p  = p ).
    ENDIF.
    result = me.

  ENDMETHOD.

  METHOD p.

    DATA temp2 TYPE zabaputil_cl_util=>ty_s_name_value.
    CLEAR temp2.
    temp2-n = n.
    temp2-v = v.
    INSERT temp2 INTO TABLE mt_prop.
    result = me.

  ENDMETHOD.

  METHOD n.

    IF name IS INITIAL.
      result = mo_parent.
      RETURN.
    ENDIF.

    IF mo_parent->mv_name = name.
      result = mo_parent.
    ELSEIF me = mo_root.
      result = me.
    ELSE.
      result = mo_parent->n( name ).
    ENDIF.

  ENDMETHOD.

  METHOD n_prev.

    result = mo_root->mo_previous.

  ENDMETHOD.

  METHOD n_root.

    result = mo_root.

  ENDMETHOD.

  METHOD stringify.

    DATA lt_parts TYPE string_table.
    IF indent = abap_true.
      IF from_root = abap_true.
        mo_root->xml_get_parts_indent( CHANGING ct_parts = lt_parts ).
      ELSE.
        xml_get_parts_indent( CHANGING ct_parts = lt_parts ).
      ENDIF.
      result = concat_lines_of( table = lt_parts sep = |\n| ).
    ELSE.
      IF from_root = abap_true.
        mo_root->xml_get_parts( CHANGING ct_parts = lt_parts ).
      ELSE.
        xml_get_parts( CHANGING ct_parts = lt_parts ).
      ENDIF.
      result = concat_lines_of( lt_parts ).
    ENDIF.

  ENDMETHOD.

  METHOD xml_get_parts.

    IF mv_name IS INITIAL.
      DATA lr_root LIKE LINE OF mt_child.
      LOOP AT mt_child INTO lr_root.
        DATA temp3 TYPE REF TO zabaputil_cl_util_xml.
        temp3 ?= lr_root.
        temp3->xml_get_parts( CHANGING ct_parts = ct_parts ).
      ENDLOOP.
      RETURN.
    ENDIF.

    DATA temp4 TYPE string.
    IF mv_ns <> ``.
      temp4 = |{ mv_ns }:|.
    ELSE.
      CLEAR temp4.
    ENDIF.
    DATA lv_tmp2 LIKE temp4.
    lv_tmp2 = temp4.
    DATA temp5 TYPE string.
    DATA val TYPE string.
    val = ``.
    DATA row LIKE LINE OF mt_prop.
    LOOP AT mt_prop INTO row WHERE v <> ``.
      DATA temp1 TYPE string.
      IF row-v = abap_true.
        temp1 = `true`.
      ELSE.
        temp1 = row-v.
      ENDIF.
      val = |{ val } { row-n }="{ escape( val = temp1 format = cl_abap_format=>e_xml_attr ) }"|.
    ENDLOOP.
    temp5 = val.
    DATA lv_tmp3 LIKE temp5.
    lv_tmp3 = temp5.

    IF mt_child IS INITIAL.
      DATA temp6 LIKE LINE OF ct_parts.
      temp6 = | <{ lv_tmp2 }{ mv_name }{ lv_tmp3 }/>|.
      APPEND temp6 TO ct_parts.
      RETURN.
    ENDIF.

    DATA temp7 LIKE LINE OF ct_parts.
    temp7 = | <{ lv_tmp2 }{ mv_name }{ lv_tmp3 }>|.
    APPEND temp7 TO ct_parts.

    DATA lr_child LIKE LINE OF mt_child.
    LOOP AT mt_child INTO lr_child.
      DATA temp8 TYPE REF TO zabaputil_cl_util_xml.
      temp8 ?= lr_child.
      temp8->xml_get_parts( CHANGING ct_parts = ct_parts ).
    ENDLOOP.

    DATA temp9 LIKE LINE OF ct_parts.
    temp9 = |</{ lv_tmp2 }{ mv_name }>|.
    APPEND temp9 TO ct_parts.

  ENDMETHOD.

  METHOD xml_get_parts_indent.

    IF mv_name IS INITIAL.
      DATA lr_root LIKE LINE OF mt_child.
      LOOP AT mt_child INTO lr_root.
        DATA temp10 TYPE REF TO zabaputil_cl_util_xml.
        temp10 ?= lr_root.
        temp10->xml_get_parts_indent( EXPORTING iv_depth = iv_depth
                                                                  CHANGING ct_parts = ct_parts ).
      ENDLOOP.
      RETURN.
    ENDIF.

    DATA lv_pad TYPE string.
    lv_pad  = repeat( val = ` ` occ = iv_depth * 2 ).
    DATA temp11 TYPE string.
    IF mv_ns <> ``.
      temp11 = |{ mv_ns }:|.
    ELSE.
      CLEAR temp11.
    ENDIF.
    DATA lv_ns LIKE temp11.
    lv_ns = temp11.
    DATA temp12 TYPE string.
    DATA val TYPE string.
    val = ``.
    DATA row LIKE LINE OF mt_prop.
    LOOP AT mt_prop INTO row WHERE v <> ``.
      DATA temp2 TYPE string.
      IF row-v = abap_true.
        temp2 = `true`.
      ELSE.
        temp2 = row-v.
      ENDIF.
      val = |{ val } { row-n }="{ escape( val = temp2 format = cl_abap_format=>e_xml_attr ) }"|.
    ENDLOOP.
    temp12 = val.
    DATA lv_attr LIKE temp12.
    lv_attr = temp12.

    IF mt_child IS INITIAL.
      DATA temp13 LIKE LINE OF ct_parts.
      temp13 = |{ lv_pad }<{ lv_ns }{ mv_name }{ lv_attr }/>|.
      APPEND temp13 TO ct_parts.
      RETURN.
    ENDIF.

    DATA temp14 LIKE LINE OF ct_parts.
    temp14 = |{ lv_pad }<{ lv_ns }{ mv_name }{ lv_attr }>|.
    APPEND temp14 TO ct_parts.

    DATA lr_child LIKE LINE OF mt_child.
    LOOP AT mt_child INTO lr_child.
      DATA temp15 TYPE REF TO zabaputil_cl_util_xml.
      temp15 ?= lr_child.
      temp15->xml_get_parts_indent( EXPORTING iv_depth = iv_depth + 1
                                                                 CHANGING ct_parts = ct_parts ).
    ENDLOOP.

    DATA temp16 LIKE LINE OF ct_parts.
    temp16 = |{ lv_pad }</{ lv_ns }{ mv_name }>|.
    APPEND temp16 TO ct_parts.

  ENDMETHOD.

ENDCLASS.

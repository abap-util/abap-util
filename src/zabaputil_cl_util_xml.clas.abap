CLASS zabaputil_cl_util_xml DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.

    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS constructor.

    " One node below this one. Its attributes are p, then a = v; an
    " attribute name set twice (in p, by a, or by a later p( )) keeps the
    " last value - XML allows each name once per element. Attributes are
    " written in the order of their names, not in the order they were set,
    " and one with an empty value is left out. The value `X` (abap_true) is
    " written as `true`: the builder targets UI5 XML views, where a flag is
    " a boolean - a literal `X` cannot be written through it
    METHODS __
      IMPORTING
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util_context=>ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS _
      IMPORTING
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util_context=>ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS _if
      IMPORTING
        when          TYPE abap_bool
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util_context=>ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

    METHODS __if
      IMPORTING
        when          TYPE abap_bool
        n             TYPE clike
        ns            TYPE clike                           OPTIONAL
        a             TYPE clike                           OPTIONAL
        v             TYPE clike                           OPTIONAL
        p             TYPE zabaputil_cl_util_context=>ty_t_name_value OPTIONAL
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
    DATA mt_prop   TYPE SORTED TABLE OF zabaputil_cl_util_context=>ty_s_name_value WITH NON-UNIQUE KEY n.

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

    " sets an attribute: a name that is there already gets the new value
    " instead of a second entry - two of them made the element invalid XML,
    " and the XMLView parser refused the whole view
    METHODS prop_set
      IMPORTING
        n TYPE clike
        v TYPE clike.

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
    DATA temp29 LIKE LINE OF p.
    DATA lr_prop LIKE REF TO temp29.
    CREATE OBJECT lo_child TYPE zabaputil_cl_util_xml.
    lo_child->mv_name   = n.
    lo_child->mv_ns     = ns.
    
    
    LOOP AT p REFERENCE INTO lr_prop.
      lo_child->prop_set( n = lr_prop->n
                          v = lr_prop->v ).
    ENDLOOP.
    IF a IS NOT INITIAL.
      lo_child->prop_set( n = a
                          v = v ).
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
      __( n  = n
          ns = ns
          a  = a
          v  = v
          p  = p ).
    ENDIF.
    result = me.

  ENDMETHOD.

  METHOD __if.

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

  METHOD p.

    prop_set( n = n
              v = v ).
    result = me.

  ENDMETHOD.

  METHOD prop_set.
    DATA temp30 TYPE zabaputil_cl_util_context=>ty_s_name_value.

    DELETE mt_prop WHERE n = n.
    
    CLEAR temp30.
    temp30-n = n.
    temp30-v = v.
    INSERT temp30 INTO TABLE mt_prop.

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
      DATA lr_root LIKE LINE OF mt_child.
        DATA temp31 TYPE REF TO zabaputil_cl_util_xml.
    DATA temp32 TYPE string.
    DATA lv_tmp2 LIKE temp32.
    DATA temp33 TYPE string.
    DATA val TYPE string.
    DATA row LIKE LINE OF mt_prop.
      DATA temp1 TYPE string.
    DATA lv_tmp3 LIKE temp33.
      DATA temp34 LIKE LINE OF ct_parts.
    DATA temp35 LIKE LINE OF ct_parts.
    DATA lr_child LIKE LINE OF mt_child.
      DATA temp36 TYPE REF TO zabaputil_cl_util_xml.
    DATA temp37 LIKE LINE OF ct_parts.

    IF mv_name IS INITIAL.
      
      LOOP AT mt_child INTO lr_root.
        
        temp31 ?= lr_root.
        temp31->xml_get_parts( CHANGING ct_parts = ct_parts ).
      ENDLOOP.
      RETURN.
    ENDIF.

    
    IF mv_ns <> ``.
      temp32 = |{ mv_ns }:|.
    ELSE.
      CLEAR temp32.
    ENDIF.
    
    lv_tmp2 = temp32.
    
    
    val = ``.
    
    LOOP AT mt_prop INTO row WHERE v <> ``.
      
      IF row-v = abap_true.
        temp1 = `true`.
      ELSE.
        temp1 = row-v.
      ENDIF.
      val = |{ val } { row-n }="{ escape( val = temp1 format = zabaputil_cl_util_context=>cv_format_e_xml_attr ) }"|.
    ENDLOOP.
    temp33 = val.
    
    lv_tmp3 = temp33.

    IF mt_child IS INITIAL.
      
      temp34 = | <{ lv_tmp2 }{ mv_name }{ lv_tmp3 }/>|.
      INSERT temp34 INTO TABLE ct_parts.
      RETURN.
    ENDIF.

    
    temp35 = | <{ lv_tmp2 }{ mv_name }{ lv_tmp3 }>|.
    INSERT temp35 INTO TABLE ct_parts.

    
    LOOP AT mt_child INTO lr_child.
      
      temp36 ?= lr_child.
      temp36->xml_get_parts( CHANGING ct_parts = ct_parts ).
    ENDLOOP.

    
    temp37 = |</{ lv_tmp2 }{ mv_name }>|.
    INSERT temp37 INTO TABLE ct_parts.

  ENDMETHOD.

  METHOD xml_get_parts_indent.
      DATA lr_root LIKE LINE OF mt_child.
        DATA temp38 TYPE REF TO zabaputil_cl_util_xml.
    DATA lv_pad TYPE string.
    DATA temp39 TYPE string.
    DATA lv_ns LIKE temp39.
    DATA temp40 TYPE string.
    DATA val TYPE string.
    DATA row LIKE LINE OF mt_prop.
      DATA temp2 TYPE string.
    DATA lv_attr LIKE temp40.
      DATA temp41 LIKE LINE OF ct_parts.
    DATA temp42 LIKE LINE OF ct_parts.
    DATA lr_child LIKE LINE OF mt_child.
      DATA temp43 TYPE REF TO zabaputil_cl_util_xml.
    DATA temp44 LIKE LINE OF ct_parts.

    IF mv_name IS INITIAL.
      
      LOOP AT mt_child INTO lr_root.
        
        temp38 ?= lr_root.
        temp38->xml_get_parts_indent( EXPORTING iv_depth = iv_depth
                                                                  CHANGING ct_parts = ct_parts ).
      ENDLOOP.
      RETURN.
    ENDIF.

    
    lv_pad  = repeat( val = ` ` occ = iv_depth * 2 ).
    
    IF mv_ns <> ``.
      temp39 = |{ mv_ns }:|.
    ELSE.
      CLEAR temp39.
    ENDIF.
    
    lv_ns = temp39.
    
    
    val = ``.
    
    LOOP AT mt_prop INTO row WHERE v <> ``.
      
      IF row-v = abap_true.
        temp2 = `true`.
      ELSE.
        temp2 = row-v.
      ENDIF.
      val = |{ val } { row-n }="{ escape( val = temp2 format = zabaputil_cl_util_context=>cv_format_e_xml_attr ) }"|.
    ENDLOOP.
    temp40 = val.
    
    lv_attr = temp40.

    IF mt_child IS INITIAL.
      
      temp41 = |{ lv_pad }<{ lv_ns }{ mv_name }{ lv_attr }/>|.
      INSERT temp41 INTO TABLE ct_parts.
      RETURN.
    ENDIF.

    
    temp42 = |{ lv_pad }<{ lv_ns }{ mv_name }{ lv_attr }>|.
    INSERT temp42 INTO TABLE ct_parts.

    
    LOOP AT mt_child INTO lr_child.
      
      temp43 ?= lr_child.
      temp43->xml_get_parts_indent( EXPORTING iv_depth = iv_depth + 1
                                                                 CHANGING ct_parts = ct_parts ).
    ENDLOOP.

    
    temp44 = |{ lv_pad }</{ lv_ns }{ mv_name }>|.
    INSERT temp44 INTO TABLE ct_parts.

  ENDMETHOD.

ENDCLASS.

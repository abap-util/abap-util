CLASS zabaputil_cl_util DEFINITION
  PUBLIC
  INHERITING FROM zabaputil_cl_util_api
  CREATE PUBLIC.

  PUBLIC SECTION.

    " abap-toolkit - Utility Functions for ABAP Cloud & Standard ABAP
    " version: `0.0.1`.
    " origin: https://github.com/oblomov-dev/abap-toolkit
    " author: https://github.com/oblomov-dev
    " license: MIT.

    CONSTANTS:
      BEGIN OF cs_ui5_msg_type,
        e TYPE string VALUE `Error` ##NO_TEXT,
        s TYPE string VALUE `Success` ##NO_TEXT,
        w TYPE string VALUE `Warning` ##NO_TEXT,
        i TYPE string VALUE `Information` ##NO_TEXT,
      END OF cs_ui5_msg_type.

    TYPES:
      BEGIN OF ty_s_name_value,
        n TYPE string,
        v TYPE string,
      END OF ty_s_name_value.
    TYPES ty_t_name_value TYPE STANDARD TABLE OF ty_s_name_value WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_token,
        key      TYPE string,
        text     TYPE string,
        visible  TYPE abap_bool,
        selkz    TYPE abap_bool,
        editable TYPE abap_bool,
      END OF ty_s_token.
    TYPES ty_t_token TYPE STANDARD TABLE OF ty_s_token WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_range,
        sign   TYPE c LENGTH 1,
        option TYPE c LENGTH 2,
        low    TYPE string,
        high   TYPE string,
      END OF ty_s_range.
    TYPES ty_t_range TYPE STANDARD TABLE OF ty_s_range WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_filter_multi,
        name            TYPE string,
        t_range         TYPE ty_t_range,
        t_token         TYPE ty_t_token,
        t_token_added   TYPE ty_t_token,
        t_token_removed TYPE ty_t_token,
      END OF ty_s_filter_multi.
    TYPES ty_t_filter_multi TYPE STANDARD TABLE OF ty_s_filter_multi WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_sql,
        tabname        TYPE string,
        check_autoload TYPE abap_bool,
        layout_name    TYPE string,
        layout_id      TYPE string,
        count          TYPE i,
        t_ref          TYPE REF TO data,
        where          TYPE string,
        t_filter       TYPE ty_t_filter_multi,
      END OF ty_s_sql.

    TYPES:
      BEGIN OF ty_s_msg,
        text       TYPE string,
        id         TYPE string,
        no         TYPE string,
        type       TYPE string,
        v1         TYPE string,
        v2         TYPE string,
        v3         TYPE string,
        v4         TYPE string,
        timestampl TYPE timestampl,
      END OF ty_s_msg,
      ty_t_msg TYPE STANDARD TABLE OF ty_s_msg WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_msg_box,
        text    TYPE string,
        type    TYPE string,
        title   TYPE string,
        details TYPE string,
        skip    TYPE abap_bool,
      END OF ty_s_msg_box.

    CLASS-METHODS ui5_get_msg_type
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS ui5_msg_box_format
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_s_msg_box.

    CLASS-METHODS rtti_check_serializable
      IMPORTING
        val           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS msg_get_t
      IMPORTING
        VALUE(val)    TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS rtti_get_data_element_text_l
      IMPORTING
        VALUE(val)    TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get
      IMPORTING
        VALUE(val)    TYPE any
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    CLASS-METHODS msg_get_by_msg
      IMPORTING
        id            TYPE any
        no            TYPE any
        v1            TYPE any OPTIONAL
        v2            TYPE any OPTIONAL
        v3            TYPE any OPTIONAL
        v4            TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    CLASS-METHODS rtti_get_t_attri_by_include
      IMPORTING
        !type         TYPE REF TO cl_abap_datadescr
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    CLASS-METHODS rtti_get_t_ddic_fixed_values
      IMPORTING
        rollname      TYPE clike
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_api=>ty_t_fix_val ##NEEDED.

    CLASS-METHODS source_get_method2
      IMPORTING
        iv_classname  TYPE clike
        iv_methodname TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS check_bound_a_not_initial
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_unassign_initial
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS unassign_object
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE REF TO object.

    CLASS-METHODS unassign_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS conv_get_as_data_ref
      IMPORTING
        val           TYPE data
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS source_method_to_file
      IMPORTING
        it_source     TYPE string_table
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS itab_get_itab_by_csv
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS itab_get_csv_by_itab
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS filter_itab
      IMPORTING
        filter TYPE ty_t_filter_multi
      CHANGING
        val     TYPE STANDARD TABLE.

    CLASS-METHODS filter_get_multi_by_data
      IMPORTING
        val           TYPE data
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS filter_get_data_by_multi
      IMPORTING
        val           TYPE ty_t_filter_multi
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS filter_get_sql_where
      IMPORTING
        val           TYPE ty_t_filter_multi
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS filter_get_sql_by_sql_string
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_sql.

    CLASS-METHODS url_param_get
      IMPORTING
        val           TYPE string
        url           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_param_create_url
      IMPORTING
        t_params      TYPE ty_t_name_value
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_param_set
      IMPORTING
        url           TYPE string
        !name         TYPE string
        !value        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_classname_by_ref
      IMPORTING
        !in           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_intfname_by_ref
      IMPORTING
        !in           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS x_get_last_t100
      IMPORTING
        val           TYPE REF TO cx_root
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS x_check_raise
      IMPORTING
        v     TYPE clike DEFAULT `CX_SY_SUBRC`
        !when TYPE abap_bool.

    CLASS-METHODS x_raise
      IMPORTING
        v TYPE clike DEFAULT `CX_SY_SUBRC`.

    CLASS-METHODS json_stringify
      IMPORTING
        !any          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS xml_parse
      IMPORTING
        !xml TYPE clike
      EXPORTING
        !any TYPE any.

    CLASS-METHODS xml_stringify
      IMPORTING
        !any          TYPE any
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_xslt_serialization_error.

    CLASS-METHODS boolean_check_by_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS boolean_abap_2_json
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS json_parse
      IMPORTING
        val   TYPE any
      CHANGING
        !data TYPE any.

    CLASS-METHODS c_trim_upper
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS xml_srtti_stringify
      IMPORTING
        !data         TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS xml_srtti_parse
      IMPORTING
        rtti_data     TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS time_get_timestampl
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS time_subtract_seconds
      IMPORTING
        !time         TYPE timestampl
        !seconds      TYPE i
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS c_trim
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_trim_lower
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_param_get_tab
      IMPORTING
        i_val            TYPE clike
      RETURNING
        VALUE(rt_params) TYPE ty_t_name_value.

    CLASS-METHODS rtti_get_t_attri_by_oref
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_attrdescr_tab.

    CLASS-METHODS rtti_get_t_attri_by_any
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS rtti_get_t_attri_by_table_name
      IMPORTING
        table_name    TYPE any
      RETURNING
        VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS rtti_get_type_name
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_class_exists
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_create_tab_by_name
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS rtti_check_type_kind_dref
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_get_type_kind
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_ref_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS boolean_check_by_name
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS filter_update_tokens
      IMPORTING
        val           TYPE ty_t_filter_multi
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS filter_get_range_t_by_token_t
      IMPORTING
        val           TYPE ty_t_token
      RETURNING
        VALUE(result) TYPE ty_t_range.

    CLASS-METHODS filter_get_range_by_token
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_range.

    CLASS-METHODS filter_get_token_t_by_range_t
      IMPORTING
        val           TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE ty_t_token ##NEEDED.

    CLASS-METHODS filter_get_token_range_mapping
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS itab_corresponding
      IMPORTING
        val  TYPE STANDARD TABLE
      CHANGING
        !tab TYPE STANDARD TABLE.

    CLASS-METHODS itab_filter_by_val
      IMPORTING
        val  TYPE clike
      CHANGING
        !tab TYPE STANDARD TABLE.

    CLASS-METHODS itab_get_by_struc
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util=>ty_t_name_value.

    CLASS-METHODS itab_filter_by_t_range
      IMPORTING
        val  TYPE ty_t_filter_multi
      CHANGING
        !tab TYPE STANDARD TABLE.

    CLASS-METHODS time_get_time_by_stampl
      IMPORTING
        val           TYPE timestampl
      RETURNING
        VALUE(result) TYPE t.

    CLASS-METHODS time_get_date_by_stampl
      IMPORTING
        val           TYPE timestampl
      RETURNING
        VALUE(result) TYPE d.

    CLASS-METHODS conv_copy_ref_data
      IMPORTING
        !from         TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS source_get_file_types
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS rtti_tab_get_relative_name
      IMPORTING
        !table        TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_clike
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_contains
      IMPORTING
        val           TYPE clike
        sub           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_starts_with
      IMPORTING
        val           TYPE clike
        prefix        TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_ends_with
      IMPORTING
        val           TYPE clike
        suffix        TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_split
      IMPORTING
        val           TYPE clike
        sep           TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS c_join
      IMPORTING
        tab           TYPE string_table
        sep           TYPE clike DEFAULT ``
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_table
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_check_structure
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_check_numeric
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS time_add_seconds
      IMPORTING
        !time         TYPE timestampl
        !seconds      TYPE i
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS time_get_stampl_by_date_time
      IMPORTING
        !date         TYPE d
        !time         TYPE t
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS time_diff_seconds
      IMPORTING
        !time_from    TYPE timestampl
        !time_to      TYPE timestampl
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS conv_string_to_date
      IMPORTING
        val           TYPE clike
        format        TYPE clike DEFAULT `YYYY-MM-DD`
      RETURNING
        VALUE(result) TYPE d.

    CLASS-METHODS conv_date_to_string
      IMPORTING
        val           TYPE d
        format        TYPE clike DEFAULT `YYYY-MM-DD`
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS app_get_url
      IMPORTING
        VALUE(classname) TYPE clike
        !origin          TYPE clike
        !pathname        TYPE clike
        !search          TYPE clike
        !hash            TYPE clike OPTIONAL
      RETURNING
        VALUE(result)    TYPE string.

    CLASS-METHODS app_get_url_source_code
      IMPORTING
        !classname    TYPE clike
        !origin       TYPE clike
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_bool_cache,
        absolute_name TYPE string,
        is_bool       TYPE abap_bool,
      END OF ty_s_bool_cache.

    CLASS-DATA mt_bool_cache TYPE HASHED TABLE OF ty_s_bool_cache WITH UNIQUE KEY absolute_name.

ENDCLASS.



CLASS zabaputil_cl_util IMPLEMENTATION.


  METHOD boolean_abap_2_json.

    IF boolean_check_by_data( val ) IS NOT INITIAL.
      DATA temp1 TYPE string.
      IF val = abap_true.
        temp1 = `true`.
      ELSE.
        temp1 = `false`.
      ENDIF.
      result = temp1.
    ELSE.
      result = val.
    ENDIF.

  ENDMETHOD.


  METHOD boolean_check_by_data.

    TRY.
        DATA lo_descr TYPE REF TO cl_abap_typedescr.
        lo_descr = cl_abap_elemdescr=>describe_by_data( val ).
        DATA temp2 TYPE string.
        temp2 = lo_descr->absolute_name.
        DATA lv_abs_name LIKE temp2.
        lv_abs_name = temp2.

        DATA temp3 LIKE sy-subrc.
        READ TABLE mt_bool_cache WITH KEY absolute_name = lv_abs_name TRANSPORTING NO FIELDS.
        temp3 = sy-subrc.
        IF temp3 = 0.
          DATA temp4 LIKE LINE OF mt_bool_cache.
          DATA temp5 LIKE sy-tabix.
          temp5 = sy-tabix.
          READ TABLE mt_bool_cache WITH KEY absolute_name = lv_abs_name INTO temp4.
          sy-tabix = temp5.
          IF sy-subrc <> 0.
            ASSERT 1 = 0.
          ENDIF.
          result = temp4-is_bool.
          RETURN.
        ENDIF.

        DATA temp6 TYPE REF TO cl_abap_elemdescr.
        temp6 ?= lo_descr.
        DATA lo_ele LIKE temp6.
        lo_ele = temp6.
        result = boolean_check_by_name( lo_ele->get_relative_name( ) ).

        DATA temp7 TYPE zabaputil_cl_util=>ty_s_bool_cache.
        CLEAR temp7.
        temp7-absolute_name = lv_abs_name.
        temp7-is_bool = result.
        INSERT temp7 INTO TABLE mt_bool_cache.

        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        DATA lv_error TYPE string.
        lv_error = x->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD boolean_check_by_name.

    CASE val.
      WHEN `ABAP_BOOL`
          OR `XSDBOOLEAN`
          OR `FLAG`
          OR `XFLAG`
          OR `XFELD`
          OR `ABAP_BOOLEAN`
          OR `WDY_BOOLEAN`
          OR `BOOLE_D`
          OR `OS_BOOLEAN`.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD check_bound_a_not_initial.

    IF val IS NOT BOUND.
      result = abap_false.
      RETURN.
    ENDIF.
    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( check_unassign_initial( val ) = abap_false ).
    result = temp1.

  ENDMETHOD.


  METHOD check_unassign_initial.

    IF val IS INITIAL.
      result = abap_true.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <any> TYPE data.
    ASSIGN val->* TO <any>.

    DATA temp2 TYPE xsdboolean.
    temp2 = boolc( <any> IS INITIAL ).
    result = temp2.

  ENDMETHOD.


  METHOD conv_copy_ref_data.

    FIELD-SYMBOLS <from>   TYPE data.
    FIELD-SYMBOLS <result> TYPE data.

    IF rtti_check_ref_data( from ) IS NOT INITIAL.
      ASSIGN from->* TO <from>.
    ELSE.
      ASSIGN from TO <from>.
    ENDIF.
    CREATE DATA result LIKE <from>.
    ASSIGN result->* TO <result>.

    <result> = <from>.

  ENDMETHOD.


  METHOD conv_get_as_data_ref.

    GET REFERENCE OF val INTO result.

  ENDMETHOD.


  METHOD c_trim.

    DATA temp8 TYPE string.
    temp8 = val.
    result = shift_left( shift_right( temp8 ) ).
    result = shift_right( val = result
                          sub = cl_abap_char_utilities=>horizontal_tab ).
    result = shift_left( val = result
                         sub = cl_abap_char_utilities=>horizontal_tab ).
    result = shift_left( shift_right( result ) ).

  ENDMETHOD.


  METHOD c_trim_lower.

    DATA temp9 TYPE string.
    temp9 = val.
    result = to_lower( c_trim( temp9 ) ).

  ENDMETHOD.


  METHOD c_trim_upper.

    DATA temp10 TYPE string.
    temp10 = val.
    result = to_upper( c_trim( temp10 ) ).

  ENDMETHOD.


  METHOD filter_itab.

    DATA ref TYPE REF TO data.

    LOOP AT val REFERENCE INTO ref.

      DATA ls_filter LIKE LINE OF filter.
      LOOP AT filter INTO ls_filter.

        FIELD-SYMBOLS <field> TYPE any.
        ASSIGN ref->(ls_filter-name) TO <field>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        IF <field> NOT IN ls_filter-t_range.
          DELETE val.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD filter_get_multi_by_data.

    DATA temp11 TYPE abap_component_tab.
    temp11 = rtti_get_t_attri_by_any( val ).
    DATA temp1 LIKE LINE OF temp11.
    DATA lr_comp LIKE REF TO temp1.
    LOOP AT temp11 REFERENCE INTO lr_comp.
      DATA temp12 TYPE zabaputil_cl_util=>ty_s_filter_multi.
      CLEAR temp12.
      temp12-name = lr_comp->name.
      INSERT temp12 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_get_range_by_token.

    DATA lv_value LIKE val.
    lv_value = val.
    DATA lv_length TYPE i.
    lv_length = strlen( lv_value ) - 1.

    CASE lv_value(1).

      WHEN `=`.
        CLEAR result.
        result-sign = `I`.
        result-option = `EQ`.
        result-low = lv_value+1.
      WHEN `<`.
        IF lv_value+1(1) = `=`.
          CLEAR result.
          result-sign = `I`.
          result-option = `LE`.
          result-low = lv_value+2.
        ELSE.
          CLEAR result.
          result-sign = `I`.
          result-option = `LT`.
          result-low = lv_value+1.
        ENDIF.
      WHEN `>`.
        IF lv_value+1(1) = `=`.
          CLEAR result.
          result-sign = `I`.
          result-option = `GE`.
          result-low = lv_value+2.
        ELSE.
          CLEAR result.
          result-sign = `I`.
          result-option = `GT`.
          result-low = lv_value+1.
        ENDIF.

      WHEN `*`.
        IF lv_value+lv_length(1) = `*`.
          lv_value = substring( val = lv_value off = 1 len = lv_length - 1 ).
          CLEAR result.
          result-sign = `I`.
          result-option = `CP`.
          result-low = lv_value.
        ENDIF.

      WHEN OTHERS.
        IF lv_value CS `...`.
          SPLIT lv_value AT `...` INTO result-low result-high.
          result-sign   = `I`.
          result-option = `BT`.
        ELSE.
          CLEAR result.
          result-sign = `I`.
          result-option = `EQ`.
          result-low = lv_value.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD filter_update_tokens.

    result = val.
    FIELD-SYMBOLS <temp13> TYPE zabaputil_cl_util=>ty_s_filter_multi.
    READ TABLE result WITH KEY name = name ASSIGNING <temp13>.
IF sy-subrc <> 0.
  ASSERT 1 = 0.
ENDIF.
DATA lr_filter LIKE REF TO <temp13>.
GET REFERENCE OF <temp13> INTO lr_filter.
    DATA ls_token LIKE LINE OF lr_filter->t_token_removed.
    LOOP AT lr_filter->t_token_removed INTO ls_token.
      DELETE lr_filter->t_token WHERE key = ls_token-key.
    ENDLOOP.

    LOOP AT lr_filter->t_token_added INTO ls_token.
      DATA temp14 TYPE zabaputil_cl_util=>ty_s_token.
      CLEAR temp14.
      temp14-key = ls_token-key.
      temp14-text = ls_token-text.
      temp14-visible = abap_true.
      temp14-editable = abap_true.
      INSERT temp14 INTO TABLE lr_filter->t_token.
    ENDLOOP.

    CLEAR lr_filter->t_token_removed.
    CLEAR lr_filter->t_token_added.

    DATA lt_range TYPE zabaputil_cl_util=>ty_t_range.
    DATA temp2 LIKE LINE OF result.
    DATA temp3 LIKE sy-tabix.
    temp3 = sy-tabix.
    READ TABLE result WITH KEY name = name INTO temp2.
    sy-tabix = temp3.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lt_range = zabaputil_cl_util=>filter_get_range_t_by_token_t( temp2-t_token ).
    lr_filter->t_range = lt_range.

  ENDMETHOD.


  METHOD filter_get_range_t_by_token_t.

    DATA ls_token LIKE LINE OF val.
    LOOP AT val INTO ls_token.
      INSERT filter_get_range_by_token( ls_token-text ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_get_token_range_mapping.

    DATA temp15 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp15.
    DATA temp16 LIKE LINE OF temp15.
    temp16-n = `EQ`.
    temp16-v = `={LOW}`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `LT`.
    temp16-v = `<{LOW}`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `LE`.
    temp16-v = `<={LOW}`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `GT`.
    temp16-v = `>{LOW}`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `GE`.
    temp16-v = `>={LOW}`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `CP`.
    temp16-v = `*{LOW}*`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `BT`.
    temp16-v = `{LOW}...{HIGH}`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `NB`.
    temp16-v = `!({LOW}...{HIGH})`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `NE`.
    temp16-v = `!(={LOW})`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `NP`.
    temp16-v = `!(*{LOW}*)`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `!<leer>`.
    temp16-v = `!(<leer>)`.
    INSERT temp16 INTO TABLE temp15.
    temp16-n = `<leer>`.
    temp16-v = `<leer>`.
    INSERT temp16 INTO TABLE temp15.
    result = temp15.

  ENDMETHOD.


  METHOD filter_get_token_t_by_range_t.

    DATA lt_mapping TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_mapping = filter_get_token_range_mapping( ).

    DATA temp17 TYPE ty_t_range.
    CLEAR temp17.
    DATA lt_tab LIKE temp17.
    lt_tab = temp17.

    itab_corresponding( EXPORTING val = val
                        CHANGING  tab = lt_tab
    ).

    DATA temp18 LIKE LINE OF lt_tab.
    DATA lr_row LIKE REF TO temp18.
    LOOP AT lt_tab REFERENCE INTO lr_row.

      DATA lv_value TYPE zabaputil_cl_util=>ty_s_name_value-v.
      DATA temp4 LIKE LINE OF lt_mapping.
      DATA temp5 LIKE sy-tabix.
      temp5 = sy-tabix.
      READ TABLE lt_mapping WITH KEY n = lr_row->option INTO temp4.
      sy-tabix = temp5.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      lv_value = temp4-v.
      REPLACE `{LOW}`  IN lv_value WITH lr_row->low.
      REPLACE `{HIGH}` IN lv_value WITH lr_row->high.

      DATA temp19 TYPE zabaputil_cl_util=>ty_s_token.
      CLEAR temp19.
      temp19-key = lv_value.
      temp19-text = lv_value.
      temp19-visible = abap_true.
      temp19-editable = abap_true.
      INSERT temp19 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD itab_filter_by_val.

    FIELD-SYMBOLS <row> TYPE any.

    LOOP AT tab ASSIGNING <row>.
      DATA lv_row TYPE string.
      lv_row = ``.
      DATA lv_index TYPE i.
      lv_index = 1.
      DO.
        FIELD-SYMBOLS <field> TYPE any.
        ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO <field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_row = lv_row && <field>.
        lv_index = lv_index + 1.
      ENDDO.

      IF lv_row NS val.
        DELETE tab.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD itab_get_csv_by_itab.

    FIELD-SYMBOLS <tab> TYPE table.
    DATA lt_lines TYPE string_table.
    DATA lv_line TYPE string.

    ASSIGN val TO <tab>.
    DATA temp20 TYPE REF TO cl_abap_tabledescr.
    temp20 ?= cl_abap_typedescr=>describe_by_data( <tab> ).
    DATA tab LIKE temp20.
    tab = temp20.

    DATA temp21 TYPE REF TO cl_abap_structdescr.
    temp21 ?= tab->get_table_line_type( ).
    DATA struc LIKE temp21.
    struc = temp21.

    CLEAR lv_line.
    DATA temp22 TYPE abap_component_tab.
    temp22 = struc->get_components( ).
    DATA temp6 LIKE LINE OF temp22.
    DATA lr_comp LIKE REF TO temp6.
    LOOP AT temp22 REFERENCE INTO lr_comp.
      lv_line = |{ lv_line }{ lr_comp->name };|.
    ENDLOOP.
    INSERT lv_line INTO TABLE lt_lines.

    DATA lr_row TYPE REF TO data.
    LOOP AT <tab> REFERENCE INTO lr_row.

      CLEAR lv_line.
      DATA lv_index TYPE i.
      lv_index = 1.
      DO.
        FIELD-SYMBOLS <row> TYPE data.
        ASSIGN lr_row->* TO <row>.
        FIELD-SYMBOLS <field> TYPE any.
        ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO <field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_index = lv_index + 1.
        lv_line = |{ lv_line }{ <field> };|.
      ENDDO.
      INSERT lv_line INTO TABLE lt_lines.
    ENDLOOP.

    result = concat_lines_of( table = lt_lines sep = cl_abap_char_utilities=>cr_lf ).

  ENDMETHOD.




  METHOD itab_get_itab_by_csv.

    DATA lt_comp TYPE cl_abap_structdescr=>component_table.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    DATA lr_row TYPE REF TO data.

    TYPES temp1 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_rows TYPE temp1.
    SPLIT val AT cl_abap_char_utilities=>newline INTO TABLE lt_rows.
    TYPES temp2 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_cols TYPE temp2.
    DATA temp7 LIKE LINE OF lt_rows.
    DATA temp8 LIKE sy-tabix.
    temp8 = sy-tabix.
    READ TABLE lt_rows INDEX 1 INTO temp7.
    sy-tabix = temp8.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    SPLIT temp7 AT `;` INTO TABLE lt_cols.

    DATA temp23 LIKE LINE OF lt_cols.
    DATA lr_col LIKE REF TO temp23.
    LOOP AT lt_cols REFERENCE INTO lr_col.

      DATA lv_name TYPE string.
      lv_name = c_trim_upper( lr_col->* ).
      REPLACE ` ` IN lv_name WITH `_`.

      DATA temp24 TYPE abap_componentdescr.
      CLEAR temp24.
      temp24-name = lv_name.
      temp24-type = cl_abap_elemdescr=>get_c( 40 ).
      INSERT temp24 INTO TABLE lt_comp.
    ENDLOOP.

    DATA struc TYPE REF TO cl_abap_structdescr.
    struc = cl_abap_structdescr=>get( lt_comp ).
    DATA temp25 TYPE REF TO cl_abap_datadescr.
    temp25 ?= struc.
    DATA data LIKE temp25.
    data = temp25.
    DATA o_table_desc TYPE REF TO cl_abap_tabledescr.
    o_table_desc = cl_abap_tabledescr=>create( p_line_type  = data
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false ).

    CREATE DATA result TYPE HANDLE o_table_desc.
    ASSIGN result->* TO <tab>.
    DELETE lt_rows WHERE table_line IS INITIAL.

    DATA temp26 LIKE LINE OF lt_rows.
    DATA lr_rows LIKE REF TO temp26.
    LOOP AT lt_rows REFERENCE INTO lr_rows FROM 2.

      SPLIT lr_rows->* AT `;` INTO TABLE lt_cols.
      CREATE DATA lr_row TYPE HANDLE struc.

      LOOP AT lt_cols REFERENCE INTO lr_col.
        FIELD-SYMBOLS <row> TYPE data.
        ASSIGN lr_row->* TO <row>.
        FIELD-SYMBOLS <field> TYPE any.
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <row> TO <field>.
        ASSERT sy-subrc = 0.
        <field> = lr_col->*.
      ENDLOOP.

      INSERT <row> INTO TABLE <tab>.
    ENDLOOP.

  ENDMETHOD.


  METHOD json_parse.
    TRY.

        zabaputil_cl_ajson=>parse( val )->to_abap( EXPORTING iv_corresponding = abap_true
                                               IMPORTING ev_container     = data ).

        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        ASSERT x IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.


  METHOD json_stringify.
    TRY.

        DATA temp27 TYPE REF TO zabaputil_if_ajson.
        temp27 ?= zabaputil_cl_ajson=>create_empty( ).
        DATA li_ajson LIKE temp27.
        li_ajson = temp27.
        result = li_ajson->set( iv_path = `/`
                                iv_val  = any )->stringify( ).

        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        ASSERT x IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.


  METHOD rtti_check_class_exists.

    TRY.
        cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = val
                                              EXCEPTIONS type_not_found = 1 ).
        IF sy-subrc = 0.
          result = abap_true.
        ENDIF.

        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        DATA lv_error TYPE string.
        lv_error = x->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD rtti_check_ref_data.

    TRY.
        DATA lo_typdescr TYPE REF TO cl_abap_typedescr.
        lo_typdescr = cl_abap_typedescr=>describe_by_data( val ).
        DATA temp28 TYPE REF TO cl_abap_refdescr.
        temp28 ?= lo_typdescr.
        DATA lo_ref LIKE temp28.
        lo_ref = temp28.
        result = abap_true.
        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        DATA lv_error TYPE string.
        lv_error = x->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD rtti_check_type_kind_dref.

    DATA lv_type_kind TYPE abap_typekind.
    lv_type_kind = cl_abap_datadescr=>get_data_type_kind( val ).
    DATA temp3 TYPE xsdboolean.
    temp3 = boolc( lv_type_kind = cl_abap_typedescr=>typekind_dref ).
    result = temp3.

  ENDMETHOD.


  METHOD rtti_get_classname_by_ref.

    DATA lv_classname TYPE abap_abstypename.
    lv_classname = cl_abap_classdescr=>get_class_name( in ).
    result = substring_after( val = lv_classname
                              sub = `\CLASS=` ).

  ENDMETHOD.


  METHOD rtti_get_intfname_by_ref.

    DATA rtti TYPE REF TO cl_abap_typedescr.
    rtti = cl_abap_typedescr=>describe_by_data( in  ).
    DATA temp29 TYPE REF TO cl_abap_refdescr.
    temp29 ?= rtti.
    DATA ref LIKE temp29.
    ref = temp29.
    DATA name TYPE abap_abstypename.
    name = ref->get_referenced_type( )->absolute_name.
    result = substring_after( val = name
                              sub = `\INTERFACE=` ).

  ENDMETHOD.


  METHOD rtti_get_type_kind.

    result = cl_abap_datadescr=>get_data_type_kind( val ).

  ENDMETHOD.


  METHOD rtti_get_type_name.
    TRY.

        DATA lo_descr TYPE REF TO cl_abap_typedescr.
        lo_descr = cl_abap_elemdescr=>describe_by_data( val ).
        DATA temp30 TYPE REF TO cl_abap_elemdescr.
        temp30 ?= lo_descr.
        DATA lo_ele LIKE temp30.
        lo_ele = temp30.
        result = lo_ele->get_relative_name( ).

        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        DATA lv_error TYPE string.
        lv_error = x->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD rtti_get_t_attri_by_include.

    TRY.

        DATA type_desc TYPE REF TO cl_abap_typedescr.
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = type->absolute_name
                                             RECEIVING  p_descr_ref    = type_desc
                                             EXCEPTIONS type_not_found = 1 ).

        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.
    DATA temp31 TYPE REF TO cl_abap_structdescr.
    temp31 ?= type_desc.
    DATA sdescr LIKE temp31.
    sdescr = temp31.
    DATA comps TYPE abap_component_tab.
    comps = sdescr->get_components( ).

    DATA temp32 LIKE LINE OF comps.
    DATA lr_comp LIKE REF TO temp32.
    LOOP AT comps REFERENCE INTO lr_comp.

      IF lr_comp->as_include = abap_true.

        DATA incl_comps TYPE abap_component_tab.
        incl_comps = rtti_get_t_attri_by_include( lr_comp->type ).

        DATA temp33 LIKE LINE OF incl_comps.
        DATA lr_incl_comp LIKE REF TO temp33.
        LOOP AT incl_comps REFERENCE INTO lr_incl_comp.
          APPEND lr_incl_comp->* TO result.
        ENDLOOP.

      ELSE.

        APPEND lr_comp->* TO result.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD rtti_get_t_attri_by_oref.

    DATA lo_obj_ref TYPE REF TO cl_abap_typedescr.
    lo_obj_ref = cl_abap_objectdescr=>describe_by_object_ref( val ).
    DATA temp34 TYPE REF TO cl_abap_classdescr.
    temp34 ?= lo_obj_ref.
    result = temp34->attributes.

  ENDMETHOD.


  METHOD rtti_get_t_attri_by_any.

    DATA lo_struct TYPE REF TO cl_abap_structdescr.
    DATA lo_type   TYPE REF TO cl_abap_typedescr.

    TRY.
        lo_type = cl_abap_typedescr=>describe_by_data( val ).
        IF lo_type->kind = cl_abap_typedescr=>kind_ref.
          lo_type = cl_abap_typedescr=>describe_by_data_ref( val ).
        ENDIF.
      CATCH cx_root.
        TRY.
            lo_type = cl_abap_typedescr=>describe_by_data_ref( val ).
          CATCH cx_root.
            lo_type = cl_abap_structdescr=>describe_by_name( val ).
        ENDTRY.
    ENDTRY.

    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        DATA temp35 TYPE REF TO cl_abap_structdescr.
        temp35 ?= lo_type.
        lo_struct = temp35.
      WHEN cl_abap_typedescr=>kind_table.
        DATA temp36 TYPE REF TO cl_abap_structdescr.
        DATA temp9 TYPE REF TO cl_abap_tabledescr.
        temp9 ?= lo_type.
        temp36 ?= temp9->get_table_line_type( ).
        lo_struct = temp36.
      WHEN OTHERS.
        lo_struct ?= lo_type.
    ENDCASE.

    DATA comps TYPE abap_component_tab.
    comps = lo_struct->get_components( ).

    DATA temp37 LIKE LINE OF comps.
    DATA lr_comp LIKE REF TO temp37.
    LOOP AT comps REFERENCE INTO lr_comp.

      IF lr_comp->as_include = abap_false.
        APPEND lr_comp->* TO result.
      ELSE.
        DATA lt_attri TYPE abap_component_tab.
        lt_attri = rtti_get_t_attri_by_include( lr_comp->type ).
        APPEND LINES OF lt_attri TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD rtti_get_t_ddic_fixed_values.

    IF rollname IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        DATA temp38 TYPE string.
        temp38 = rollname.
        DATA typedescr TYPE REF TO cl_abap_typedescr.
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = temp38
                                             RECEIVING  p_descr_ref    = typedescr
                                             EXCEPTIONS type_not_found = 1
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DATA temp39 TYPE REF TO cl_abap_elemdescr.
        temp39 ?= typedescr.
        DATA elemdescr LIKE temp39.
        elemdescr = temp39.

        result = rtti_get_t_fixvalues( elemdescr = elemdescr
                                       langu     = langu ).

        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        DATA lv_error TYPE string.
        lv_error = x->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD rtti_tab_get_relative_name.

    FIELD-SYMBOLS <table> TYPE any.

    TRY.
        DATA typedesc TYPE REF TO cl_abap_typedescr.
        typedesc = cl_abap_typedescr=>describe_by_data( table ).

        CASE typedesc->kind.

          WHEN cl_abap_typedescr=>kind_table.
            DATA temp40 TYPE REF TO cl_abap_tabledescr.
            temp40 ?= typedesc.
            DATA tabledesc LIKE temp40.
            tabledesc = temp40.
            DATA temp41 TYPE REF TO cl_abap_structdescr.
            temp41 ?= tabledesc->get_table_line_type( ).
            DATA structdesc LIKE temp41.
            structdesc = temp41.
            result = structdesc->get_relative_name( ).
            RETURN.

          WHEN typedesc->kind_ref.

            ASSIGN table->* TO <table>.
            result = rtti_tab_get_relative_name( <table> ).

        ENDCASE.
        DATA x TYPE REF TO cx_root.
      CATCH cx_root INTO x.
        DATA lv_error TYPE string.
        lv_error = x->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD source_get_file_types.

    DATA lv_types TYPE string.
    lv_types = |abap, abc, actionscript, ada, apache_conf, applescript, asciidoc, assembly_x86, autohotkey, batchfile, bro, c9search, c_cpp, cirru, clojure, cobol, coffee, coldfusion, csharp, css, curly, d, dart, diff, django, dockerfile, | &&
|dot, drools, eiffel, yaml, ejs, elixir, elm, erlang, forth, fortran, ftl, gcode, gherkin, gitignore, glsl, gobstones, golang, groovy, haml, handlebars, haskell, haskell_cabal, haxe, hjson, html, html_elixir, html_ruby, ini, io, jack, jade, java, ja| &&
      |vascri| &&
|pt, json, jsoniq, jsp, jsx, julia, kotlin, latex, lean, less, liquid, lisp, live_script, livescript, logiql, lsl, lua, luapage, lucene, makefile, markdown, mask, matlab, mavens_mate_log, maze, mel, mips_assembler, mipsassembler, mushcode, mysql, ni| &&
|x, nsis, objectivec, ocaml, pascal, perl, pgsql, php, plain_text, powershell, praat, prolog, properties, protobuf, python, r, razor, rdoc, rhtml, rst, ruby, rust, sass, scad, scala, scheme, scss, sh, sjs, smarty, snippets, soy_template, space, sql,| &&
      | sqlserver, stylus, svg, swift, swig, tcl, tex, text, textile, toml, tsx, twig, typescript, vala, vbscript, velocity, verilog, vhdl, wollok, xml, xquery, terraform, slim, redshift, red, puppet, php_laravel_blade, mixal, jssm, fsharp, edifact,| &&
      | csp, cssound_score, cssound_orchestra, cssound_document| ##NO_TEXT.
    SPLIT lv_types AT `,` INTO TABLE result.

  ENDMETHOD.


  METHOD source_get_method2.

    DATA lt_source TYPE string_table.
    lt_source = source_get_method( iv_classname  = iv_classname
                                         iv_methodname = iv_methodname ).

    result = source_method_to_file( lt_source ).

  ENDMETHOD.


  METHOD source_method_to_file.

    DATA lv_source LIKE LINE OF it_source.
    LOOP AT it_source INTO lv_source.
      TRY.
          result = result && lv_source+1 && cl_abap_char_utilities=>newline.
          DATA x TYPE REF TO cx_root.
        CATCH cx_root INTO x.
          DATA lv_error TYPE string.
          lv_error = x->get_text( ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_get_sql_by_sql_string.

    DATA temp42 TYPE string.
    temp42 = val.
    DATA lv_sql LIKE temp42.
    lv_sql = temp42.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_sql WITH ``.
    lv_sql = to_upper( lv_sql ).
    DATA lv_dummy TYPE string.
    DATA lv_tab TYPE string.
    SPLIT lv_sql AT `SELECTFROM` INTO lv_dummy lv_tab.
    SPLIT lv_tab AT `FIELDS` INTO lv_tab lv_dummy.

    result-tabname = lv_tab.

  ENDMETHOD.


  METHOD time_get_date_by_stampl.
    DATA ls_sy TYPE zabaputil_cl_util_api=>ty_syst.
    ls_sy = zabaputil_cl_util=>context_get_sy( ).
    DATA lv_dummy TYPE t.
    CONVERT TIME STAMP val TIME ZONE ls_sy-zonlo INTO DATE result TIME lv_dummy.
  ENDMETHOD.


  METHOD time_get_timestampl.
    GET TIME STAMP FIELD result.
  ENDMETHOD.


  METHOD time_get_time_by_stampl.
    DATA ls_sy TYPE zabaputil_cl_util_api=>ty_syst.
    ls_sy = zabaputil_cl_util=>context_get_sy( ).
    DATA lv_dummy TYPE d.
    CONVERT TIME STAMP val TIME ZONE ls_sy-zonlo INTO DATE lv_dummy TIME result.
  ENDMETHOD.


  METHOD time_subtract_seconds.

    result = cl_abap_tstmp=>subtractsecs( tstmp = time
                                          secs  = seconds ).

  ENDMETHOD.


  METHOD unassign_data.

    FIELD-SYMBOLS <unassign> TYPE any.

    ASSIGN val->* TO <unassign>.
    result = <unassign>.

  ENDMETHOD.


  METHOD unassign_object.

    FIELD-SYMBOLS <unassign> TYPE any.

    ASSIGN val->* TO <unassign>.
    result = <unassign>.

  ENDMETHOD.


  METHOD url_param_create_url.

    DATA ls_param LIKE LINE OF t_params.
    LOOP AT t_params INTO ls_param.
      result = |{ result }{ ls_param-n }={ ls_param-v }&|.
    ENDLOOP.
    result = shift_right( val = result
                          sub = `&` ).

  ENDMETHOD.


  METHOD url_param_get.

    DATA lt_params TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_params = url_param_get_tab( url ).
    DATA lv_val TYPE string.
    lv_val = c_trim_lower( val ).
    DATA temp43 TYPE string.
    CLEAR temp43.
    DATA temp44 TYPE zabaputil_cl_util=>ty_s_name_value.
    READ TABLE lt_params INTO temp44 WITH KEY n = lv_val.
    IF sy-subrc = 0.
      temp43 = temp44-v.
    ENDIF.
    result = temp43.

  ENDMETHOD.


  METHOD url_param_get_tab.

    DATA lv_search TYPE string.
    lv_search = replace( val  = i_val
                               sub  = `%3D`
                               with = `=`
                               occ  = 0 ).

    lv_search = replace( val  = lv_search
                         sub  = `%26`
                         with = `&`
                         occ  = 0 ).

    lv_search = shift_left( val = lv_search
                            sub = `?` ).

    DATA lv_search2 TYPE string.
    lv_search2 = substring_after( val = lv_search
                                        sub = `&sap-startup-params=` ).
    DATA temp45 TYPE string.
    IF lv_search2 IS NOT INITIAL.
      temp45 = lv_search2.
    ELSE.
      temp45 = lv_search.
    ENDIF.
    lv_search = temp45.

    lv_search2 = substring_after( val = c_trim_lower( lv_search )
                                  sub = `?` ).
    IF lv_search2 IS NOT INITIAL.
      lv_search = lv_search2.
    ENDIF.

    TYPES temp3 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_param TYPE temp3.
    SPLIT lv_search AT `&` INTO TABLE lt_param.

    DATA temp46 LIKE LINE OF lt_param.
    DATA lr_param LIKE REF TO temp46.
    LOOP AT lt_param REFERENCE INTO lr_param.
      DATA lv_name TYPE string.
      DATA lv_value TYPE string.
      SPLIT lr_param->* AT `=` INTO lv_name lv_value.
      DATA temp47 TYPE zabaputil_cl_util=>ty_s_name_value.
      CLEAR temp47.
      temp47-n = lv_name.
      temp47-v = lv_value.
      INSERT temp47 INTO TABLE rt_params.
    ENDLOOP.

  ENDMETHOD.


  METHOD url_param_set.

    DATA lt_params TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_params = url_param_get_tab( url ).
    DATA lv_n TYPE string.
    lv_n = c_trim_lower( name ).

    DATA temp48 LIKE LINE OF lt_params.
    DATA lr_params LIKE REF TO temp48.
    LOOP AT lt_params REFERENCE INTO lr_params
         WHERE n = lv_n.
      lr_params->v = c_trim_lower( value ).
    ENDLOOP.
    IF sy-subrc <> 0.
      DATA temp49 TYPE zabaputil_cl_util=>ty_s_name_value.
      CLEAR temp49.
      temp49-n = lv_n.
      temp49-v = c_trim_lower( value ).
      INSERT temp49 INTO TABLE lt_params.
    ENDIF.

    result = url_param_create_url( lt_params ).

  ENDMETHOD.


  METHOD xml_parse.

    IF xml IS INITIAL.
      CLEAR any.
      RETURN.
    ENDIF.

    CALL TRANSFORMATION id
         SOURCE XML xml
         RESULT data = any.

  ENDMETHOD.


  METHOD xml_srtti_parse.

    DATA srtti TYPE REF TO object.
    CALL TRANSFORMATION id SOURCE XML rtti_data RESULT srtti = srtti.

    DATA rtti_type TYPE REF TO cl_abap_typedescr.
    CALL METHOD srtti->(`GET_RTTI`)
      RECEIVING
        rtti = rtti_type.

    DATA lo_datadescr TYPE REF TO cl_abap_datadescr.
    lo_datadescr ?= rtti_type.

    CREATE DATA result TYPE HANDLE lo_datadescr.
    FIELD-SYMBOLS <variable> TYPE data.
    ASSIGN result->* TO <variable>.
    CALL TRANSFORMATION id SOURCE XML rtti_data RESULT dobj = <variable>.

  ENDMETHOD.


  METHOD xml_srtti_stringify.

    IF rtti_check_class_exists( `ZCL_SRTTI_TYPEDESCR` ) = abap_true.

      DATA srtti TYPE REF TO object.
      DATA lv_classname TYPE string.
      lv_classname = `ZCL_SRTTI_TYPEDESCR`.
      CALL METHOD (lv_classname)=>(`CREATE_BY_DATA_OBJECT`)
        EXPORTING
          data_object = data
        RECEIVING
          srtti       = srtti.
      CALL TRANSFORMATION id SOURCE srtti = srtti dobj = data RESULT XML result.

    ELSE.

      TRY.
          CALL METHOD zabaputil_cl_srt_typedescr=>(`CREATE_BY_DATA_OBJECT`)
            EXPORTING
              data_object = data
            RECEIVING
              srtti       = srtti.
          CALL TRANSFORMATION id SOURCE srtti = srtti dobj = data RESULT XML result.

        CATCH cx_root.

          DATA lv_text TYPE string.
          lv_text = `UNSUPPORTED_FEATURE`.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lv_text.

      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD xml_stringify.

    CALL TRANSFORMATION id
         SOURCE data = any
         RESULT XML result
         OPTIONS data_refs = `heap-or-create`.

  ENDMETHOD.


  METHOD x_check_raise.

    IF when = abap_true.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = v.
    ENDIF.

  ENDMETHOD.


  METHOD x_get_last_t100.

    DATA x LIKE val.
    x = val.
    DO.

      IF x->previous IS BOUND.
        x = x->previous.
        CONTINUE.
      ENDIF.

      EXIT.
    ENDDO.

    result = x->get_text( ).

  ENDMETHOD.


  METHOD x_raise.

    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = v.

  ENDMETHOD.


  METHOD rtti_get_t_attri_by_table_name.

    IF table_name IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `TABLE_NAME_INITIAL_ERROR`.
    ENDIF.

    TRY.
        DATA lo_obj TYPE REF TO cl_abap_typedescr.
        cl_abap_structdescr=>describe_by_name( EXPORTING  p_name         = table_name
                                               RECEIVING  p_descr_ref    = lo_obj
                                               EXCEPTIONS type_not_found = 1
                                                          OTHERS         = 2
            ).

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |TABLE_NOT_FOUD_NAME___{ table_name }|.
        ENDIF.
        DATA temp50 TYPE REF TO cl_abap_structdescr.
        temp50 ?= lo_obj.
        DATA lo_struct LIKE temp50.
        lo_struct = temp50.

      CATCH cx_root.

        TRY.
            cl_abap_structdescr=>describe_by_name( EXPORTING  p_name         = table_name
                                                   RECEIVING  p_descr_ref    = lo_obj
                                                   EXCEPTIONS type_not_found = 1
                                                              OTHERS         = 2
            ).
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zabaputil_cx_util_error
                EXPORTING
                  val = |TABLE_NOT_FOUD_NAME___{ table_name }|.
            ENDIF.

            DATA temp51 TYPE REF TO cl_abap_tabledescr.
            temp51 ?= lo_obj.
            DATA lo_tab LIKE temp51.
            lo_tab = temp51.
            DATA temp52 TYPE REF TO cl_abap_structdescr.
            temp52 ?= lo_tab->get_table_line_type( ).
            lo_struct = temp52.
          CATCH cx_root.
            RETURN.
        ENDTRY.

    ENDTRY.

    result = lo_struct->get_components( ).

    DATA temp53 LIKE LINE OF result.
    DATA lr_comp LIKE REF TO temp53.
    LOOP AT result REFERENCE INTO lr_comp
         WHERE as_include = abap_true.

      DATA lt_attri TYPE abap_component_tab.
      lt_attri = rtti_get_t_attri_by_include( lr_comp->type ).

      DELETE result.
      INSERT LINES OF lt_attri INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD itab_corresponding.

    FIELD-SYMBOLS <row_in>  TYPE any.
    FIELD-SYMBOLS <row_out> TYPE any.

    LOOP AT val ASSIGNING <row_in>.
      APPEND INITIAL LINE TO tab ASSIGNING <row_out>.
      MOVE-CORRESPONDING <row_in> TO <row_out>.
    ENDLOOP.

  ENDMETHOD.


  METHOD itab_get_by_struc.

    DATA lt_attri TYPE abap_component_tab.
    lt_attri = zabaputil_cl_util=>rtti_get_t_attri_by_any( val ).
    DATA temp54 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp54.
    LOOP AT lt_attri REFERENCE INTO lr_attri.

      FIELD-SYMBOLS <component> TYPE any.
      ASSIGN COMPONENT lr_attri->name OF STRUCTURE val TO <component>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE zabaputil_cl_util=>rtti_get_type_kind( <component> ).

        WHEN cl_abap_typedescr=>typekind_table.

        WHEN OTHERS.
          DATA temp55 TYPE zabaputil_cl_util=>ty_s_name_value.
          CLEAR temp55.
          temp55-n = lr_attri->name.
          temp55-v = <component>.
          INSERT temp55 INTO TABLE result.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD itab_filter_by_t_range.

    DATA ref TYPE REF TO data.

    LOOP AT tab REFERENCE INTO ref.
      DATA ls_filter LIKE LINE OF val.
      LOOP AT val INTO ls_filter.

        IF ls_filter-t_range IS INITIAL.
          CONTINUE.
        ENDIF.

        FIELD-SYMBOLS <field> TYPE any.
        ASSIGN ref->(ls_filter-name) TO <field>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        IF <field> NOT IN ls_filter-t_range.
          DELETE tab.
          EXIT.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_get_data_by_multi.

    DATA ls_filter LIKE LINE OF val.
    LOOP AT val INTO ls_filter.
      IF lines( ls_filter-t_range ) > 0
        OR lines( ls_filter-t_token ) > 0.
        INSERT ls_filter INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_get_sql_where.


    IF context_check_abap_cloud( ) IS NOT INITIAL.


    ELSE.

      TYPES: BEGIN OF ty_rscedst,
               fnam   TYPE c LENGTH 30, " Field Name
               sign   TYPE c LENGTH 1,  " Selection criteria: SIGN
               option TYPE c LENGTH 2,  " Selection criteria: OPTION
               low    TYPE c LENGTH 45, " From value
               high   TYPE c LENGTH 45, " To value
             END OF ty_rscedst.

      TYPES temp4 TYPE STANDARD TABLE OF ty_rscedst.
DATA lt_range TYPE temp4.

      DATA ls_filter LIKE LINE OF val.
      LOOP AT val INTO ls_filter.
        DATA ls_range LIKE LINE OF ls_filter-t_range.
        LOOP AT ls_filter-t_range INTO ls_range.

          DATA temp56 TYPE ty_rscedst.
          CLEAR temp56.
          temp56-fnam = ls_filter-name.
          temp56-sign = ls_range-sign.
          temp56-option = ls_range-option.
          temp56-low = ls_range-low.
          temp56-high = ls_range-high.
          INSERT temp56 INTO TABLE lt_range.

        ENDLOOP.
      ENDLOOP.

      DATA lv_fm TYPE string.
      lv_fm = `RSDS_RANGE_TO_WHERE`.
      CALL FUNCTION lv_fm
        EXPORTING
          i_t_range      = lt_range
*         i_th_range     =
*         i_r_renderer   =
        IMPORTING
          e_where        = result
*         e_t_where      = lt_where
        EXCEPTIONS
          internal_error = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = zabaputil_cl_util=>context_get_sy( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD msg_get_t.

    result = zabaputil_cl_util_msg=>msg_get( val ).

  ENDMETHOD.


  METHOD rtti_check_clike.

    DATA lv_type TYPE string.
    lv_type = rtti_get_type_kind( val ).
    CASE lv_type.
      WHEN cl_abap_datadescr=>typekind_char OR
          cl_abap_datadescr=>typekind_clike OR
          cl_abap_datadescr=>typekind_csequence OR
          cl_abap_datadescr=>typekind_string.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD ui5_get_msg_type.

    DATA temp57 TYPE string.
    CASE val.
      WHEN `E`.
        temp57 = cs_ui5_msg_type-e.
      WHEN `S`.
        temp57 = cs_ui5_msg_type-s.
      WHEN `W`.
        temp57 = cs_ui5_msg_type-w.
      WHEN OTHERS.
        temp57 = cs_ui5_msg_type-i.
    ENDCASE.
    result = temp57.

  ENDMETHOD.


  METHOD rtti_create_tab_by_name.

    DATA struct_desc TYPE REF TO cl_abap_typedescr.
    struct_desc = cl_abap_structdescr=>describe_by_name( val ).
    DATA temp58 TYPE REF TO cl_abap_datadescr.
    temp58 ?= struct_desc.
    DATA data_desc LIKE temp58.
    data_desc = temp58.
    DATA gr_dyntable_typ TYPE REF TO cl_abap_tabledescr.
    gr_dyntable_typ = cl_abap_tabledescr=>create( data_desc ).
    CREATE DATA result TYPE HANDLE gr_dyntable_typ.

  ENDMETHOD.


  METHOD msg_get.

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = msg_get_t( val ).
    DATA temp59 LIKE LINE OF lt_msg.
    DATA temp60 LIKE sy-tabix.
    temp60 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp59.
    sy-tabix = temp60.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    result = temp59.

  ENDMETHOD.


  METHOD rtti_get_data_element_text_l.

    result = rtti_get_data_element_texts( val )-long.

  ENDMETHOD.


  METHOD msg_get_by_msg.

    DATA temp61 TYPE ty_s_msg.
    CLEAR temp61.
    temp61-id = id.
    temp61-no = no.
    temp61-v1 = v1.
    temp61-v2 = v2.
    temp61-v3 = v3.
    temp61-v4 = v4.
    DATA ls_msg LIKE temp61.
    ls_msg = temp61.
    result = msg_get( ls_msg ).

  ENDMETHOD.


  METHOD c_contains.

    DATA temp62 TYPE string.
    temp62 = val.
    DATA temp4 TYPE xsdboolean.
    temp4 = boolc( temp62 CS sub ).
    result = temp4.

  ENDMETHOD.


  METHOD c_starts_with.

    DATA temp63 TYPE string.
    temp63 = val.
    DATA lv_val LIKE temp63.
    lv_val = temp63.
    DATA temp64 TYPE string.
    temp64 = prefix.
    DATA lv_prefix LIKE temp64.
    lv_prefix = temp64.
    DATA lv_len TYPE i.
    lv_len = strlen( lv_prefix ).

    IF strlen( lv_val ) < lv_len.
      result = abap_false.
      RETURN.
    ENDIF.

    DATA temp5 TYPE xsdboolean.
    temp5 = boolc( lv_val(lv_len) = lv_prefix ).
    result = temp5.

  ENDMETHOD.


  METHOD c_ends_with.

    DATA temp65 TYPE string.
    temp65 = val.
    DATA lv_val LIKE temp65.
    lv_val = temp65.
    DATA temp66 TYPE string.
    temp66 = suffix.
    DATA lv_suffix LIKE temp66.
    lv_suffix = temp66.
    DATA lv_len_suffix TYPE i.
    lv_len_suffix = strlen( lv_suffix ).
    DATA lv_len_val TYPE i.
    lv_len_val = strlen( lv_val ).

    IF lv_len_val < lv_len_suffix.
      result = abap_false.
      RETURN.
    ENDIF.

    DATA lv_off TYPE i.
    lv_off = lv_len_val - lv_len_suffix.
    DATA temp6 TYPE xsdboolean.
    temp6 = boolc( lv_val+lv_off(lv_len_suffix) = lv_suffix ).
    result = temp6.

  ENDMETHOD.


  METHOD c_split.

    SPLIT val AT sep INTO TABLE result.

  ENDMETHOD.


  METHOD c_join.

    DATA lv_line LIKE LINE OF tab.
    LOOP AT tab INTO lv_line.
      IF sy-tabix > 1.
        result = result && sep.
      ENDIF.
      result = result && lv_line.
    ENDLOOP.

  ENDMETHOD.


  METHOD rtti_check_table.

    DATA lv_type_kind TYPE abap_typekind.
    lv_type_kind = cl_abap_datadescr=>get_data_type_kind( val ).
    DATA temp7 TYPE xsdboolean.
    temp7 = boolc( lv_type_kind = cl_abap_typedescr=>typekind_table ).
    result = temp7.

  ENDMETHOD.


  METHOD rtti_check_structure.

    TRY.
        DATA lo_type TYPE REF TO cl_abap_typedescr.
        lo_type = cl_abap_typedescr=>describe_by_data( val ).
        DATA temp8 TYPE xsdboolean.
        temp8 = boolc( lo_type->kind = cl_abap_typedescr=>kind_struct ).
        result = temp8.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD rtti_check_numeric.

    DATA lv_type_kind TYPE abap_typekind.
    lv_type_kind = cl_abap_datadescr=>get_data_type_kind( val ).
    CASE lv_type_kind.
      WHEN cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR cl_abap_typedescr=>typekind_packed
          OR cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_decfloat
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34
          OR cl_abap_typedescr=>typekind_num.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD time_add_seconds.

    result = cl_abap_tstmp=>add( tstmp = time
                                 secs  = seconds ).

  ENDMETHOD.


  METHOD time_get_stampl_by_date_time.

    DATA ls_sy TYPE zabaputil_cl_util_api=>ty_syst.
    ls_sy = zabaputil_cl_util=>context_get_sy( ).
    CONVERT DATE date TIME time INTO TIME STAMP result TIME ZONE ls_sy-zonlo.

  ENDMETHOD.


  METHOD time_diff_seconds.

    DATA lv_diff TYPE i.
    lv_diff = cl_abap_tstmp=>subtract( tstmp1 = time_to
                                              tstmp2 = time_from ).
    result = lv_diff.

  ENDMETHOD.


  METHOD conv_string_to_date.

    DATA temp67 TYPE string.
    temp67 = val.
    DATA lv_val LIKE temp67.
    lv_val = temp67.
    DATA temp68 TYPE string.
    temp68 = format.
    DATA lv_fmt LIKE temp68.
    lv_fmt = temp68.
    DATA lv_yyyy_off TYPE i.
    lv_yyyy_off = find( val = lv_fmt sub = `YYYY` ).
    DATA lv_mm_off TYPE i.
    lv_mm_off   = find( val = lv_fmt sub = `MM` ).
    DATA lv_dd_off TYPE i.
    lv_dd_off   = find( val = lv_fmt sub = `DD` ).

    DATA lv_clean TYPE string.
    lv_clean = ``.
    DATA lv_i TYPE i.
    lv_i = 0.
    WHILE lv_i < strlen( lv_val ).
      DATA lv_c TYPE string.
      lv_c = lv_val+lv_i(1).
      IF lv_c >= `0` AND lv_c <= `9`.
        lv_clean = lv_clean && lv_c.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

    DATA lv_fmt_clean TYPE string.
    lv_fmt_clean = ``.
    lv_i = 0.
    WHILE lv_i < strlen( lv_fmt ).
      lv_c = lv_fmt+lv_i(1).
      IF lv_c = `Y` OR lv_c = `M` OR lv_c = `D`.
        lv_fmt_clean = lv_fmt_clean && lv_c.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

    DATA lv_year TYPE string.
    lv_year  = ``.
    DATA lv_month TYPE string.
    lv_month = ``.
    DATA lv_day TYPE string.
    lv_day   = ``.

    DATA lv_pos TYPE i.
    lv_pos = 0.
    lv_i = 0.
    WHILE lv_i < strlen( lv_fmt_clean ).
      lv_c = lv_fmt_clean+lv_i(1).
      CASE lv_c.
        WHEN `Y`.
          lv_year = lv_year && lv_clean+lv_pos(1).
        WHEN `M`.
          lv_month = lv_month && lv_clean+lv_pos(1).
        WHEN `D`.
          lv_day = lv_day && lv_clean+lv_pos(1).
      ENDCASE.
      lv_pos = lv_pos + 1.
      lv_i = lv_i + 1.
    ENDWHILE.

    result = lv_year && lv_month && lv_day.

  ENDMETHOD.


  METHOD conv_date_to_string.

    DATA temp69 TYPE string.
    temp69 = format.
    DATA lv_fmt LIKE temp69.
    lv_fmt = temp69.
    DATA temp70 TYPE string.
    temp70 = val.
    DATA lv_date LIKE temp70.
    lv_date = temp70.

    DATA lv_year TYPE string.
    lv_year  = lv_date(4).
    DATA lv_month TYPE string.
    lv_month = lv_date+4(2).
    DATA lv_day TYPE string.
    lv_day   = lv_date+6(2).

    result = lv_fmt.
    REPLACE `YYYY` IN result WITH lv_year.
    REPLACE `MM`   IN result WITH lv_month.
    REPLACE `DD`   IN result WITH lv_day.

  ENDMETHOD.


  METHOD ui5_msg_box_format.

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = msg_get_t( val ).

    IF lines( lt_msg ) = 1.
      DATA temp71 LIKE LINE OF lt_msg.
      DATA temp72 LIKE sy-tabix.
      temp72 = sy-tabix.
      READ TABLE lt_msg INDEX 1 INTO temp71.
      sy-tabix = temp72.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result-text  = temp71-text.
      DATA temp73 LIKE LINE OF lt_msg.
      DATA temp74 LIKE sy-tabix.
      temp74 = sy-tabix.
      READ TABLE lt_msg INDEX 1 INTO temp73.
      sy-tabix = temp74.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result-type  = to_lower( ui5_get_msg_type( temp73-type ) ).
      DATA temp75 LIKE LINE OF lt_msg.
      DATA temp76 LIKE sy-tabix.
      temp76 = sy-tabix.
      READ TABLE lt_msg INDEX 1 INTO temp75.
      sy-tabix = temp76.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result-title = ui5_get_msg_type( temp75-type ).

    ELSEIF lines( lt_msg ) > 1.
      result-text = | { lines( lt_msg ) } Messages found: |.
      DATA lt_detail_items TYPE string_table.
      DATA temp77 LIKE LINE OF lt_msg.
      DATA lr_msg LIKE REF TO temp77.
      LOOP AT lt_msg REFERENCE INTO lr_msg.
        DATA temp78 LIKE LINE OF lt_detail_items.
        temp78 = |<li>{ lr_msg->text }</li>|.
        INSERT temp78 INTO TABLE lt_detail_items.
      ENDLOOP.
      result-details = `<ul>` && concat_lines_of( lt_detail_items ) && `</ul>`.
      DATA temp79 LIKE LINE OF lt_msg.
      DATA temp80 LIKE sy-tabix.
      temp80 = sy-tabix.
      READ TABLE lt_msg INDEX 1 INTO temp79.
      sy-tabix = temp80.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result-title   = ui5_get_msg_type( temp79-type ).
      DATA temp81 LIKE LINE OF lt_msg.
      DATA temp82 LIKE sy-tabix.
      temp82 = sy-tabix.
      READ TABLE lt_msg INDEX 1 INTO temp81.
      sy-tabix = temp82.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result-type    = ui5_get_msg_type( temp81-type ).

    ELSE.
      result-skip = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD rtti_check_serializable.

    IF val IS NOT BOUND.
      result = abap_true.
      RETURN.
    ENDIF.
    TRY.
        DATA temp83 TYPE REF TO if_serializable_object.
        temp83 ?= val.
        DATA lo_dummy LIKE temp83.
        lo_dummy = temp83.
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD app_get_url.

    DATA lt_param TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_param = url_param_get_tab( search ).
    DELETE lt_param WHERE n = `app_start`.
    DATA temp84 TYPE zabaputil_cl_util=>ty_s_name_value.
    CLEAR temp84.
    temp84-n = `app_start`.
    temp84-v = to_lower( classname ).
    INSERT temp84 INTO TABLE lt_param.

    result = |{ origin }{ pathname }?| && url_param_create_url( lt_param ) && hash.

  ENDMETHOD.


  METHOD app_get_url_source_code.

    result = |{ origin }/sap/bc/adt/oo/classes/{ classname }/source/main|.

  ENDMETHOD.
ENDCLASS.

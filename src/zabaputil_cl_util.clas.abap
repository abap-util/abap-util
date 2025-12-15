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

    CLASS-METHODS ui5_get_msg_type
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

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

    CLASS-METHODS check_bound_a_not_inital
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_unassign_inital
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
        !filter TYPE ty_t_filter_multi
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
        v TYPE clike DEFAULT `CX_SY_SUBRC`
          PREFERRED PARAMETER v.

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

    CLASS-METHODS time_substract_seconds
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

  PROTECTED SECTION.

  PRIVATE SECTION.

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
        DATA lv_type_name TYPE string.
        lv_type_name = rtti_get_type_name( val ).
        result = boolean_check_by_name( lv_type_name ).
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


  METHOD check_bound_a_not_inital.

    IF val IS NOT BOUND.
      result = abap_false.
      RETURN.
    ENDIF.
    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( check_unassign_inital( val ) = abap_false ).
    result = temp1.

  ENDMETHOD.


  METHOD check_unassign_inital.

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

*    GET REFERENCE OF val INTO result.
    GET REFERENCE OF val INTO result.

  ENDMETHOD.


  METHOD c_trim.

    DATA temp2 TYPE string.
    temp2 = val.
    result = shift_left( shift_right( temp2 ) ).
    result = shift_right( val = result
                          sub = cl_abap_char_utilities=>horizontal_tab ).
    result = shift_left( val = result
                         sub = cl_abap_char_utilities=>horizontal_tab ).
    result = shift_left( shift_right( result ) ).

  ENDMETHOD.


  METHOD c_trim_lower.

    DATA temp3 TYPE string.
    temp3 = val.
    result = to_lower( c_trim( temp3 ) ).

  ENDMETHOD.


  METHOD c_trim_upper.

    DATA temp4 TYPE string.
    temp4 = val.
    result = to_upper( c_trim( temp4 ) ).

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

    DATA temp5 TYPE abap_component_tab.
    temp5 = rtti_get_t_attri_by_any( val ).
    DATA temp1 LIKE LINE OF temp5.
    DATA lr_comp LIKE REF TO temp1.
    LOOP AT temp5 REFERENCE INTO lr_comp.
      DATA temp6 TYPE zabaputil_cl_util=>ty_s_filter_multi.
      CLEAR temp6.
      temp6-name = lr_comp->name.
      INSERT temp6 INTO TABLE result.
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
          SHIFT lv_value RIGHT DELETING TRAILING `*`.
          SHIFT lv_value LEFT DELETING LEADING `*`.
          CLEAR result.
          result-sign = `I`.
          result-option = `CP`.
          result-low = lv_value.
        ENDIF.

      WHEN OTHERS.
        IF lv_value CP `...`.
          SPLIT lv_value AT `...` INTO result-low result-high.
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
    FIELD-SYMBOLS <temp7> TYPE zabaputil_cl_util=>ty_s_filter_multi.
    READ TABLE result WITH KEY name = name ASSIGNING <temp7>.
IF sy-subrc <> 0.
  ASSERT 1 = 0.
ENDIF.
DATA lr_filter LIKE REF TO <temp7>.
GET REFERENCE OF <temp7> INTO lr_filter.
    DATA ls_token LIKE LINE OF lr_filter->t_token_removed.
    LOOP AT lr_filter->t_token_removed INTO ls_token.
      DELETE lr_filter->t_token WHERE key = ls_token-key.
    ENDLOOP.

    LOOP AT lr_filter->t_token_added INTO ls_token.
      DATA temp8 TYPE zabaputil_cl_util=>ty_s_token.
      CLEAR temp8.
      temp8-key = ls_token-key.
      temp8-text = ls_token-text.
      temp8-visible = abap_true.
      temp8-editable = abap_true.
      INSERT temp8 INTO TABLE lr_filter->t_token.
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

    DATA temp9 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp9.
    DATA temp10 LIKE LINE OF temp9.
    temp10-n = `EQ`.
    temp10-v = `={LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `LT`.
    temp10-v = `<{LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `LE`.
    temp10-v = `<={LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `GT`.
    temp10-v = `>{LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `GE`.
    temp10-v = `>={LOW}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `CP`.
    temp10-v = `*{LOW}*`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `BT`.
    temp10-v = `{LOW}...{HIGH}`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `NE`.
    temp10-v = `!(={LOW})`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `NE`.
    temp10-v = `!(<leer>)`.
    INSERT temp10 INTO TABLE temp9.
    temp10-n = `<leer>`.
    temp10-v = `<leer>`.
    INSERT temp10 INTO TABLE temp9.
    result = temp9.

  ENDMETHOD.


  METHOD filter_get_token_t_by_range_t.

    DATA lt_mapping TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_mapping = filter_get_token_range_mapping( ).

    DATA temp11 TYPE ty_t_range.
    CLEAR temp11.
    DATA lt_tab LIKE temp11.
    lt_tab = temp11.

    itab_corresponding( EXPORTING val = val
                        CHANGING  tab = lt_tab
    ).

    DATA temp12 LIKE LINE OF lt_tab.
    DATA lr_row LIKE REF TO temp12.
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

      DATA temp13 TYPE zabaputil_cl_util=>ty_s_token.
      CLEAR temp13.
      temp13-key = lv_value.
      temp13-text = lv_value.
      temp13-visible = abap_true.
      temp13-editable = abap_true.
      INSERT temp13 INTO TABLE result.
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

    ASSIGN val TO <tab>.
    DATA temp14 TYPE REF TO cl_abap_tabledescr.
    temp14 ?= cl_abap_typedescr=>describe_by_data( <tab> ).
    DATA tab LIKE temp14.
    tab = temp14.

    DATA temp15 TYPE REF TO cl_abap_structdescr.
    temp15 ?= tab->get_table_line_type( ).
    DATA struc LIKE temp15.
    struc = temp15.

    DATA temp16 TYPE abap_component_tab.
    temp16 = struc->get_components( ).
    DATA temp6 LIKE LINE OF temp16.
    DATA lr_comp LIKE REF TO temp6.
    LOOP AT temp16 REFERENCE INTO lr_comp.
      result = |{ result }{ lr_comp->name };|.
    ENDLOOP.

    result = result && cl_abap_char_utilities=>cr_lf.

    DATA lr_row TYPE REF TO data.
    LOOP AT <tab> REFERENCE INTO lr_row.

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
        result = |{ result }{ <field> };|.
      ENDDO.
      result = result && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.

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

    DATA temp17 LIKE LINE OF lt_cols.
    DATA lr_col LIKE REF TO temp17.
    LOOP AT lt_cols REFERENCE INTO lr_col.

      DATA lv_name TYPE string.
      lv_name = c_trim_upper( lr_col->* ).
      REPLACE ` ` IN lv_name WITH `_`.

      DATA temp18 TYPE abap_componentdescr.
      CLEAR temp18.
      temp18-name = lv_name.
      temp18-type = cl_abap_elemdescr=>get_c( 40 ).
      INSERT temp18 INTO TABLE lt_comp.
    ENDLOOP.

    DATA struc TYPE REF TO cl_abap_structdescr.
    struc = cl_abap_structdescr=>get( lt_comp ).
    DATA temp19 TYPE REF TO cl_abap_datadescr.
    temp19 ?= struc.
    DATA data LIKE temp19.
    data = temp19.
    DATA o_table_desc TYPE REF TO cl_abap_tabledescr.
    o_table_desc = cl_abap_tabledescr=>create( p_line_type  = data
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false ).

    CREATE DATA result TYPE HANDLE o_table_desc.
    ASSIGN result->* TO <tab>.
    DELETE lt_rows WHERE table_line IS INITIAL.

    DATA temp20 LIKE LINE OF lt_rows.
    DATA lr_rows LIKE REF TO temp20.
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

        DATA temp21 TYPE REF TO zabaputil_if_ajson.
        temp21 ?= zabaputil_cl_ajson=>create_empty( ).
        DATA li_ajson LIKE temp21.
        li_ajson = temp21.
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
        DATA temp22 TYPE REF TO cl_abap_refdescr.
        temp22 ?= lo_typdescr.
        DATA lo_ref LIKE temp22.
        lo_ref = temp22.
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
    DATA temp23 TYPE REF TO cl_abap_refdescr.
    temp23 ?= rtti.
    DATA ref LIKE temp23.
    ref = temp23.
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
        DATA temp24 TYPE REF TO cl_abap_elemdescr.
        temp24 ?= lo_descr.
        DATA lo_ele LIKE temp24.
        lo_ele = temp24.
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
    DATA temp25 TYPE REF TO cl_abap_structdescr.
    temp25 ?= type_desc.
    DATA sdescr LIKE temp25.
    sdescr = temp25.
    DATA comps TYPE abap_component_tab.
    comps = sdescr->get_components( ).

    DATA temp26 LIKE LINE OF comps.
    DATA lr_comp LIKE REF TO temp26.
    LOOP AT comps REFERENCE INTO lr_comp.

      IF lr_comp->as_include = abap_true.

        DATA incl_comps TYPE abap_component_tab.
        incl_comps = rtti_get_t_attri_by_include( lr_comp->type ).

        DATA temp27 LIKE LINE OF incl_comps.
        DATA lr_incl_comp LIKE REF TO temp27.
        LOOP AT incl_comps REFERENCE INTO lr_incl_comp.
          lr_incl_comp->name = lr_incl_comp->name.
          APPEND lr_incl_comp->* TO result.
        ENDLOOP.

      ELSE.

        lr_comp->name = lr_comp->name.
        APPEND lr_comp->* TO result.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD rtti_get_t_attri_by_oref.

    DATA lo_obj_ref TYPE REF TO cl_abap_typedescr.
    lo_obj_ref = cl_abap_objectdescr=>describe_by_object_ref( val ).
    DATA temp28 TYPE REF TO cl_abap_classdescr.
    temp28 ?= lo_obj_ref.
    result = temp28->attributes.

  ENDMETHOD.


  METHOD rtti_get_t_attri_by_any.

    TRY.
        DATA lo_type TYPE REF TO cl_abap_typedescr.
        lo_type = cl_abap_typedescr=>describe_by_data( val ).
        DATA temp29 TYPE REF TO cl_abap_structdescr.
        temp29 ?= lo_type.
        DATA lo_struct LIKE temp29.
        lo_struct = temp29.
      CATCH cx_root.
        TRY.
            DATA temp30 TYPE REF TO cl_abap_tabledescr.
            temp30 ?= lo_type.
            DATA lo_tab LIKE temp30.
            lo_tab = temp30.
            DATA temp31 TYPE REF TO cl_abap_structdescr.
            temp31 ?= lo_tab->get_table_line_type( ).
            lo_struct = temp31.
          CATCH cx_root.
            TRY.
                DATA lo_ref TYPE REF TO cl_abap_typedescr.
                lo_ref = cl_abap_typedescr=>describe_by_data_ref( val ).
                DATA temp32 TYPE REF TO cl_abap_structdescr.
                temp32 ?= lo_ref.
                lo_struct = temp32.
              CATCH cx_root.
                TRY.
                    DATA temp33 TYPE REF TO cl_abap_tabledescr.
                    temp33 ?= lo_ref.
                    lo_tab = temp33.
                    DATA temp34 TYPE REF TO cl_abap_structdescr.
                    temp34 ?= lo_tab->get_table_line_type( ).
                    lo_struct = temp34.
                  CATCH cx_root.
                    lo_struct ?= cl_abap_structdescr=>describe_by_name( val ).
                ENDTRY.
            ENDTRY.
        ENDTRY.
    ENDTRY.

    DATA comps TYPE abap_component_tab.
    comps = lo_struct->get_components( ).

    DATA temp35 LIKE LINE OF comps.
    DATA lr_comp LIKE REF TO temp35.
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

        DATA temp36 TYPE string.
        temp36 = rollname.
        DATA typedescr TYPE REF TO cl_abap_typedescr.
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = temp36
                                             RECEIVING  p_descr_ref    = typedescr
                                             EXCEPTIONS type_not_found = 1
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DATA temp37 TYPE REF TO cl_abap_elemdescr.
        temp37 ?= typedescr.
        DATA elemdescr LIKE temp37.
        elemdescr = temp37.

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
            DATA temp38 TYPE REF TO cl_abap_tabledescr.
            temp38 ?= typedesc.
            DATA tabledesc LIKE temp38.
            tabledesc = temp38.
            DATA temp39 TYPE REF TO cl_abap_structdescr.
            temp39 ?= tabledesc->get_table_line_type( ).
            DATA structdesc LIKE temp39.
            structdesc = temp39.
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

    DATA temp40 TYPE string.
    temp40 = val.
    DATA lv_sql LIKE temp40.
    lv_sql = temp40.
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


  METHOD time_substract_seconds.

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
    DATA temp41 TYPE string.
    CLEAR temp41.
    DATA temp42 TYPE zabaputil_cl_util=>ty_s_name_value.
    READ TABLE lt_params INTO temp42 WITH KEY n = lv_val.
    IF sy-subrc = 0.
      temp41 = temp42-v.
    ENDIF.
    result = temp41.

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
*    lv_search = c_trim_lower( lv_search ).

    DATA lv_search2 TYPE string.
    lv_search2 = substring_after( val = lv_search
                                        sub = `&sap-startup-params=` ).
    DATA temp43 TYPE string.
    IF lv_search2 IS NOT INITIAL.
      temp43 = lv_search2.
    ELSE.
      temp43 = lv_search.
    ENDIF.
    lv_search = temp43.

    lv_search2 = substring_after( val = c_trim_lower( lv_search )
                                  sub = `?` ).
    IF lv_search2 IS NOT INITIAL.
      lv_search = lv_search2.
    ENDIF.

    TYPES temp3 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_param TYPE temp3.
    SPLIT lv_search AT `&` INTO TABLE lt_param.

    DATA temp44 LIKE LINE OF lt_param.
    DATA lr_param LIKE REF TO temp44.
    LOOP AT lt_param REFERENCE INTO lr_param.
      DATA lv_name TYPE string.
      DATA lv_value TYPE string.
      SPLIT lr_param->* AT `=` INTO lv_name lv_value.
*      INSERT VALUE #( n = c_trim_lower( lv_name )
*                      v = c_trim_lower( lv_value ) ) INTO TABLE rt_params.
      DATA temp45 TYPE zabaputil_cl_util=>ty_s_name_value.
      CLEAR temp45.
      temp45-n = lv_name.
      temp45-v = lv_value.
      INSERT temp45 INTO TABLE rt_params.
    ENDLOOP.

  ENDMETHOD.


  METHOD url_param_set.

    DATA lt_params TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_params = url_param_get_tab( url ).
    DATA lv_n TYPE string.
    lv_n = c_trim_lower( name ).

    DATA temp46 LIKE LINE OF lt_params.
    DATA lr_params LIKE REF TO temp46.
    LOOP AT lt_params REFERENCE INTO lr_params
         WHERE n = lv_n.
      lr_params->v = c_trim_lower( value ).
    ENDLOOP.
    IF sy-subrc <> 0.
      DATA temp47 TYPE zabaputil_cl_util=>ty_s_name_value.
      CLEAR temp47.
      temp47-n = lv_n.
      temp47-v = c_trim_lower( value ).
      INSERT temp47 INTO TABLE lt_params.
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
        DATA temp48 TYPE REF TO cl_abap_structdescr.
        temp48 ?= lo_obj.
        DATA lo_struct LIKE temp48.
        lo_struct = temp48.

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

            DATA temp49 TYPE REF TO cl_abap_tabledescr.
            temp49 ?= lo_obj.
            DATA lo_tab LIKE temp49.
            lo_tab = temp49.
            DATA temp50 TYPE REF TO cl_abap_structdescr.
            temp50 ?= lo_tab->get_table_line_type( ).
            lo_struct = temp50.
          CATCH cx_root.
            RETURN.
        ENDTRY.

    ENDTRY.

    result = lo_struct->get_components( ).

    DATA temp51 LIKE LINE OF result.
    DATA lr_comp LIKE REF TO temp51.
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

      IF lines( tab ) = 0.
        DATA lv_lines TYPE i.
        lv_lines = 1.
      ELSE.
        lv_lines = lines( tab ).
      ENDIF.

      INSERT INITIAL LINE INTO tab ASSIGNING <row_out> INDEX lv_lines.
      CLEAR: <row_out>.
      MOVE-CORRESPONDING <row_in> TO <row_out>.

    ENDLOOP.

  ENDMETHOD.


  METHOD itab_get_by_struc.

    DATA lt_attri TYPE abap_component_tab.
    lt_attri = zabaputil_cl_util=>rtti_get_t_attri_by_any( val ).
*    result  = VALUE z2ui5_cl_util=>ty_t_name_value( ).
    DATA temp52 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp52.
    LOOP AT lt_attri REFERENCE INTO lr_attri.

      FIELD-SYMBOLS <component> TYPE any.
      ASSIGN COMPONENT lr_attri->name OF STRUCTURE val TO <component>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE zabaputil_cl_util=>rtti_get_type_kind( <component> ).

        WHEN cl_abap_typedescr=>typekind_table.

        WHEN OTHERS.
          DATA temp53 TYPE zabaputil_cl_util=>ty_s_name_value.
          CLEAR temp53.
          temp53-n = lr_attri->name.
          temp53-v = <component>.
          INSERT temp53 INTO TABLE result.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD itab_filter_by_t_range.

  ENDMETHOD.


  METHOD filter_get_data_by_multi.

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

          DATA temp54 TYPE ty_rscedst.
          CLEAR temp54.
          temp54-fnam = ls_filter-name.
          temp54-sign = ls_range-sign.
          temp54-option = ls_range-option.
          temp54-low = ls_range-low.
          temp54-high = ls_range-high.
          INSERT temp54 INTO TABLE lt_range.

        ENDLOOP.
      ENDLOOP.

*      DATA result TYPE string.
*    DATA lt_where TYPE rsdmd_t_where.
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

    DATA temp55 TYPE string.
    CASE val.
      WHEN `E`.
        temp55 = cs_ui5_msg_type-e.
      WHEN `S`.
        temp55 = cs_ui5_msg_type-s.
      WHEN `W`.
        temp55 = cs_ui5_msg_type-w.
      WHEN OTHERS.
        temp55 = cs_ui5_msg_type-i.
    ENDCASE.
    result = temp55.

  ENDMETHOD.


  METHOD rtti_create_tab_by_name.

    DATA struct_desc TYPE REF TO cl_abap_typedescr.
    struct_desc = cl_abap_structdescr=>describe_by_name( val ).
    DATA temp56 TYPE REF TO cl_abap_datadescr.
    temp56 ?= struct_desc.
    DATA data_desc LIKE temp56.
    data_desc = temp56.
    DATA gr_dyntable_typ TYPE REF TO cl_abap_tabledescr.
    gr_dyntable_typ = cl_abap_tabledescr=>create( data_desc ).
    CREATE DATA result TYPE HANDLE gr_dyntable_typ.

  ENDMETHOD.


  METHOD msg_get.

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = msg_get_t( val ).
    DATA temp57 LIKE LINE OF lt_msg.
    DATA temp58 LIKE sy-tabix.
    temp58 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp57.
    sy-tabix = temp58.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    result = temp57.

  ENDMETHOD.


  METHOD rtti_get_data_element_text_l.

    result = zabaputil_cl_util=>rtti_get_data_element_texts( val )-long.

  ENDMETHOD.


  METHOD msg_get_by_msg.

    DATA temp59 TYPE zabaputil_cl_util=>ty_s_msg.
    CLEAR temp59.
    temp59-id = id.
    temp59-no = no.
    temp59-v1 = v1.
    temp59-v2 = v2.
    temp59-v3 = v3.
    temp59-v4 = v4.
    DATA ls_msg LIKE temp59.
    ls_msg = temp59.
    result = msg_get( ls_msg ).

  ENDMETHOD.
ENDCLASS.

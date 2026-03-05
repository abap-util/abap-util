
CLASS ltcl_test_app DEFINITION FOR TESTING.

  PUBLIC SECTION.

    INTERFACES if_serializable_object.

    TYPES:
      BEGIN OF ty_row,
        title    TYPE string,
        value    TYPE string,
        descr    TYPE string,
        icon     TYPE string,
        info     TYPE string,
        selected TYPE abap_bool,
        checkbox TYPE abap_bool,
      END OF ty_row.

    CONSTANTS sv_status TYPE string VALUE `test` ##NEEDED.

    CLASS-DATA sv_var TYPE string.
    CLASS-DATA ss_tab TYPE ty_row.
    CLASS-DATA st_tab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.

    CLASS-METHODS class_constructor.

    DATA mv_val TYPE string ##NEEDED.
    DATA ms_tab TYPE ty_row ##NEEDED.
    TYPES temp1_f9908b1ee3 TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA mt_tab TYPE temp1_f9908b1ee3 ##NEEDED.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS ltcl_test_app IMPLEMENTATION.
  METHOD class_constructor.

    sv_var = 1.
    DATA temp60 TYPE ltcl_test_app=>ty_row.
    CLEAR temp60.
    ss_tab = temp60.
    DATA temp61 LIKE st_tab.
    CLEAR temp61.
    st_tab = temp61.

  ENDMETHOD.
ENDCLASS.


CLASS ltcl_unit_test_open_abap DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS check_input
      IMPORTING
        val           TYPE data
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS test_assign            FOR TESTING RAISING cx_static_check.
    METHODS test_eledescr_rel_name FOR TESTING RAISING cx_static_check.
    METHODS test_classdescr        FOR TESTING RAISING cx_static_check.
    METHODS test_substring_after   FOR TESTING RAISING cx_static_check.
    METHODS test_substring_before  FOR TESTING RAISING cx_static_check.
    METHODS test_string_shift      FOR TESTING RAISING cx_static_check.
    METHODS test_string_replace    FOR TESTING RAISING cx_static_check.
    METHODS test_raise_error       FOR TESTING RAISING cx_static_check.
    METHODS test_xsdbool           FOR TESTING RAISING cx_static_check.
    METHODS test_xsdbool_nested    FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION MEDIUM.

  PRIVATE SECTION.

    METHODS test_create                    FOR TESTING RAISING cx_static_check.

    METHODS test_boolean_abap_2_json       FOR TESTING RAISING cx_static_check.
    METHODS test_boolean_check             FOR TESTING RAISING cx_static_check.

    METHODS test_c_trim                    FOR TESTING RAISING cx_static_check.
    METHODS test_c_trim_lower              FOR TESTING RAISING cx_static_check.
    METHODS test_c_trim_upper              FOR TESTING RAISING cx_static_check.
    METHODS test_c_trim_horizontal_tab     FOR TESTING RAISING cx_static_check.

    METHODS test_time_get_timestampl       FOR TESTING RAISING cx_static_check.
    METHODS test_time_substract_seconds    FOR TESTING RAISING cx_static_check.
    METHODS test_func_get_user_tech        FOR TESTING RAISING cx_static_check.

    METHODS test_rtti_get_t_attri_by_incl  FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_get_classname_by_ref FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_get_type_name        FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_get_type_kind        FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_type_kind      FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_get_t_attri_by_obj   FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_get_t_comp_by_struc  FOR TESTING RAISING cx_static_check.

    METHODS test_trans_json_any_2__w_struc FOR TESTING RAISING cx_static_check.
    METHODS test_trans_xml_any_2__w_obj    FOR TESTING RAISING cx_static_check.
    METHODS test_trans_xml_any_2__w_data   FOR TESTING RAISING cx_static_check.
    METHODS test_trans_xml_2_any__w_obj    FOR TESTING RAISING cx_static_check.
    METHODS test_trans_xml_2_any__w_data   FOR TESTING RAISING cx_static_check.

    METHODS test_url_param_create_url      FOR TESTING RAISING cx_static_check.
    METHODS test_url_param_get             FOR TESTING RAISING cx_static_check.
    METHODS test_url_param_get_tab         FOR TESTING RAISING cx_static_check.
    METHODS test_url_param_set             FOR TESTING RAISING cx_static_check.

    METHODS test_x_check_raise             FOR TESTING RAISING cx_static_check.
    METHODS test_x_check_raise_not         FOR TESTING RAISING cx_static_check.
    METHODS test_x_raise                   FOR TESTING RAISING cx_static_check.
    METHODS test_check_unassign_inital     FOR TESTING RAISING cx_static_check.
    METHODS conv_copy_ref_data             FOR TESTING RAISING cx_static_check.
    METHODS rtti_check_ref_data            FOR TESTING RAISING cx_static_check.
    METHODS test_check_bound_a_not_inital  FOR TESTING RAISING cx_static_check.
    METHODS test_sql_get_by_string         FOR TESTING RAISING cx_static_check.
    METHODS test_get_token_t_by_r_t        FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test_open_abap IMPLEMENTATION.

  METHOD test_assign.

    DATA lo_app TYPE REF TO ltcl_test_app.
    CREATE OBJECT lo_app TYPE ltcl_test_app.
    FIELD-SYMBOLS <any> TYPE any.

    lo_app->mv_val = `ABC`.

    DATA lv_assign TYPE string.
    lv_assign = |MV_VAL|.
    ASSIGN lo_app->(lv_assign) TO <any>.
    ASSERT sy-subrc = 0.

    cl_abap_unit_assert=>assert_equals( exp = `ABC`
                                        act = <any> ).

  ENDMETHOD.

  METHOD test_classdescr.

    DATA lo_app TYPE REF TO ltcl_test_app.
    CREATE OBJECT lo_app TYPE ltcl_test_app.

    DATA temp62 TYPE REF TO cl_abap_classdescr.
    temp62 ?= cl_abap_objectdescr=>describe_by_object_ref( lo_app ).
    DATA lt_attri LIKE temp62->attributes.
    lt_attri = temp62->attributes.

    DATA lv_test LIKE LINE OF lt_attri.
    DATA temp9 LIKE LINE OF lt_attri.
    DATA temp10 LIKE sy-tabix.
    temp10 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `MS_TAB` INTO temp9.
    sy-tabix = temp10.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp9.
    DATA temp63 LIKE LINE OF lt_attri.
    DATA temp64 LIKE sy-tabix.
    temp64 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `MT_TAB` INTO temp63.
    sy-tabix = temp64.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp63.
    DATA temp65 LIKE LINE OF lt_attri.
    DATA temp66 LIKE sy-tabix.
    temp66 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `MV_VAL` INTO temp65.
    sy-tabix = temp66.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp65.
    DATA temp67 LIKE LINE OF lt_attri.
    DATA temp68 LIKE sy-tabix.
    temp68 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `SS_TAB` INTO temp67.
    sy-tabix = temp68.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp67.
    DATA temp69 LIKE LINE OF lt_attri.
    DATA temp70 LIKE sy-tabix.
    temp70 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `ST_TAB` INTO temp69.
    sy-tabix = temp70.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp69.
    DATA temp71 LIKE LINE OF lt_attri.
    DATA temp72 LIKE sy-tabix.
    temp72 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `SV_STATUS` INTO temp71.
    sy-tabix = temp72.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp71.
    DATA temp73 LIKE LINE OF lt_attri.
    DATA temp74 LIKE sy-tabix.
    temp74 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `SV_VAR` INTO temp73.
    sy-tabix = temp74.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp73.

  ENDMETHOD.

  METHOD test_eledescr_rel_name.

    DATA temp75 TYPE REF TO cl_abap_elemdescr.
    temp75 ?= cl_abap_elemdescr=>describe_by_data( abap_true ).
    DATA lo_ele LIKE temp75.
    lo_ele = temp75.

    cl_abap_unit_assert=>assert_equals( exp = `ABAP_BOOL`
                                        act = lo_ele->get_relative_name( ) ).

  ENDMETHOD.

  METHOD test_substring_after.

    cl_abap_unit_assert=>assert_equals( exp = ` string`
                                        act = substring_after( val = `this is a string`
                                                               sub = `a` ) ).

  ENDMETHOD.

  METHOD test_substring_before.

    cl_abap_unit_assert=>assert_equals( exp = `this is `
                                        act = substring_before( val = `this is a string`
                                                                sub = `a` ) ).

  ENDMETHOD.

  METHOD test_string_shift.

    cl_abap_unit_assert=>assert_equals( exp = `string`
                                        act = shift_left( shift_right( val = `   string   `
                                                                       sub = ` ` ) ) ).

  ENDMETHOD.

  METHOD test_string_replace.

    DATA lv_search TYPE string.
    lv_search = replace( val  = `one two three`
                               sub  = `two`
                               with = `ABC`
                               occ  = 0 ) ##NEEDED.

    cl_abap_unit_assert=>assert_equals( exp = `one ABC three`
                                        act = replace( val  = `one two three`
                                                       sub  = `two`
                                                       with = `ABC`
                                                       occ  = 0 ) ).

  ENDMETHOD.

  METHOD test_raise_error.

    TRY.
        IF 1 = 1.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error.
        ENDIF.
        cl_abap_unit_assert=>fail( ).

        DATA lx TYPE REF TO zabaputil_cx_util_error.
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_bound( lx ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_xsdbool.

    DATA lv_xsdbool TYPE abap_bool.
    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( 1 = 1 ).
    lv_xsdbool = temp1.
    IF lv_xsdbool = abap_false.
      cl_abap_unit_assert=>assert_false( lv_xsdbool ).
    ENDIF.

    DATA temp2 TYPE xsdboolean.
    temp2 = boolc( 1 = 1 ).
    IF temp2 = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_xsdbool_nested.

    DATA lv_xsdbool TYPE abap_bool.
    DATA temp3 TYPE xsdboolean.
    temp3 = boolc( 1 = 1 ).
    lv_xsdbool = check_input( temp3 ).
    IF lv_xsdbool = abap_false.
      cl_abap_unit_assert=>assert_false( lv_xsdbool ).
    ENDIF.

    IF check_input( abap_false ) IS NOT INITIAL.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp4 TYPE xsdboolean.
    temp4 = boolc( 1 = 1 ).
    IF check_input( temp4 ) = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD check_input.

    result = val.

  ENDMETHOD.
ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.
  METHOD test_boolean_check.

    DATA lv_bool TYPE abap_bool.
    DATA temp5 TYPE xsdboolean.
    temp5 = boolc( 1 = 1 ).
    lv_bool = temp5.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_data( lv_bool ) ).

    DATA temp6 TYPE xsdboolean.
    temp6 = boolc( 1 = 2 ).
    lv_bool = temp6.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_data( lv_bool ) ).

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_data( abap_true ) ).

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_data( abap_false ) ).

  ENDMETHOD.

  METHOD test_sql_get_by_string.

    DATA lv_test TYPE string.
    lv_test = ``.
    DATA ls_sql TYPE zabaputil_cl_util=>ty_s_sql.
    ls_sql = zabaputil_cl_util=>filter_get_sql_by_sql_string( lv_test ) ##NEEDED.

  ENDMETHOD.

  METHOD test_create.

    DATA lo_test TYPE REF TO zabaputil_cl_util.
    CREATE OBJECT lo_test TYPE zabaputil_cl_util.

  ENDMETHOD.

  METHOD test_rtti_get_classname_by_ref.

    DATA lo_test2 TYPE REF TO ltcl_test_app.
    CREATE OBJECT lo_test2 TYPE ltcl_test_app.
    DATA lv_name2 TYPE string.
    lv_name2 = zabaputil_cl_util=>rtti_get_classname_by_ref( lo_test2 ).
    cl_abap_unit_assert=>assert_equals( exp = `LTCL_TEST_APP`
                                        act = lv_name2 ).

  ENDMETHOD.

  METHOD test_check_bound_a_not_inital.

    DATA lv_test TYPE string.
    lv_test = `test`.
    DATA lr_test LIKE REF TO lv_test.
    GET REFERENCE OF lv_test INTO lr_test.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>check_bound_a_not_inital( lr_test ) ).
    CLEAR lv_test.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>check_bound_a_not_inital( lr_test ) ).
    CLEAR lr_test.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>check_bound_a_not_inital( lr_test ) ).

  ENDMETHOD.

  METHOD test_check_unassign_inital.

    DATA lv_test TYPE string.
    lv_test = `test`.
    DATA lr_test LIKE REF TO lv_test.
    GET REFERENCE OF lv_test INTO lr_test.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>check_unassign_inital( lr_test ) ).

    CLEAR lv_test.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>check_unassign_inital( lr_test ) ).

  ENDMETHOD.

  METHOD rtti_check_ref_data.

    DATA lv_test TYPE string.
    lv_test = `test`.
    DATA lr_data TYPE REF TO data.
*    GET REFERENCE OF lv_test INTO lr_data.
    GET REFERENCE OF lv_test INTO lr_data.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>rtti_check_ref_data( lr_data ) ).

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>rtti_check_ref_data( lv_test ) ).

  ENDMETHOD.

  METHOD conv_copy_ref_data.

    DATA lv_test TYPE string.
    lv_test = `test`.
    DATA lr_data TYPE REF TO data.
*    GET REFERENCE OF lv_test INTO lr_data.
    GET REFERENCE OF lv_test INTO lr_data.

    DATA lr_test2 TYPE REF TO data.
    lr_test2 = zabaputil_cl_util=>conv_copy_ref_data( lr_data ).

    FIELD-SYMBOLS <result> TYPE data.
    ASSIGN lr_test2->* TO <result>.

    cl_abap_unit_assert=>assert_equals( exp = lv_test
                                        act = <result> ).

  ENDMETHOD.

  METHOD test_boolean_abap_2_json.

    cl_abap_unit_assert=>assert_equals( exp = `{ABCD}`
                                        act = zabaputil_cl_util=>boolean_abap_2_json( `{ABCD}` ) ).

  ENDMETHOD.

  METHOD test_time_get_timestampl.

    DATA lv_time TYPE timestampl.
    lv_time = zabaputil_cl_util=>time_get_timestampl( ).

    DATA lv_time2 TYPE timestampl.
    lv_time2 = zabaputil_cl_util=>time_substract_seconds( time    = lv_time
                                                            seconds = 60 * 60 * 4 ).

    IF lv_time IS INITIAL OR lv_time2 IS INITIAL.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    IF lv_time < lv_time2.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_time_substract_seconds.

    DATA lv_time TYPE timestampl.
    lv_time = zabaputil_cl_util=>time_get_timestampl( ).
    DATA lv_time2 TYPE timestampl.
    lv_time2 = zabaputil_cl_util=>time_get_timestampl( ).

    IF lv_time IS INITIAL OR lv_time2 IS INITIAL.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    IF lv_time2 < lv_time.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_c_trim.

    cl_abap_unit_assert=>assert_equals( exp = `JsadfHHs`
                                        act = zabaputil_cl_util=>c_trim( ` JsadfHHs  ` ) ).

  ENDMETHOD.

  METHOD test_c_trim_lower.

    cl_abap_unit_assert=>assert_equals( exp = `jsadfhhs`
                                        act = zabaputil_cl_util=>c_trim_lower( ` JsadfHHs  ` ) ).

  ENDMETHOD.

  METHOD test_c_trim_upper.

    cl_abap_unit_assert=>assert_equals( exp = `JSADFHHS`
                                        act = zabaputil_cl_util=>c_trim_upper( ` JsadfHHs  ` ) ).

  ENDMETHOD.

  METHOD test_func_get_user_tech.

    IF sy-sysid = `ABC`.
      RETURN.
    ENDIF.

    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_util=>context_get_user_tech( )
                                        act = sy-uname ).

    cl_abap_unit_assert=>assert_not_initial( zabaputil_cl_util=>context_get_user_tech( ) ).

  ENDMETHOD.

  METHOD test_x_raise.

    TRY.
        zabaputil_cl_util=>x_raise( ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD test_x_check_raise.

    TRY.
        DATA temp7 TYPE xsdboolean.
        temp7 = boolc( 1 = 1 ).
        zabaputil_cl_util=>x_check_raise( temp7 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    TRY.
        DATA temp8 TYPE xsdboolean.
        temp8 = boolc( 1 = 3 ).
        zabaputil_cl_util=>x_check_raise( temp8 ).
      CATCH cx_root.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_trans_json_any_2__w_struc.

    TYPES:
      BEGIN OF ty_row,
        title    TYPE string,
        value    TYPE string,
        selected TYPE abap_bool,
      END OF ty_row.

    DATA temp76 TYPE ty_row.
    CLEAR temp76.
    temp76-title = `test`.
    DATA ls_row LIKE temp76.
    ls_row = temp76.

    cl_abap_unit_assert=>assert_equals( exp = `{"selected":false,"title":"test","value":""}`
                                        act = zabaputil_cl_util=>json_stringify( ls_row ) ).

  ENDMETHOD.

  METHOD test_url_param_create_url.

    DATA lt_param TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_param = zabaputil_cl_util=>url_param_get_tab(
                         `https://url.com/rvice_for_ui?sap-client=100&app_start=z2ui5_cl_app_hello_world` ).
    DATA lv_url TYPE string.
    lv_url = zabaputil_cl_util=>url_param_create_url( lt_param ).

    cl_abap_unit_assert=>assert_equals( exp = `sap-client=100&app_start=z2ui5_cl_app_hello_world`
                                        act = lv_url ).

  ENDMETHOD.

  METHOD test_url_param_get.

    DATA lv_param TYPE string.
    lv_param = zabaputil_cl_util=>url_param_get(
                         val = `app_start`
                         url = `https://url.com/rvice_for_ui?sap-client=100&app_start=z2ui5_cl_app_hello_world` ).

    cl_abap_unit_assert=>assert_equals( exp = `z2ui5_cl_app_hello_world`
                                        act = lv_param ).

  ENDMETHOD.

  METHOD test_url_param_get_tab.

    DATA lt_param TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_param = zabaputil_cl_util=>url_param_get_tab(
                         `https://url.com/rvice_for_ui?sap-client=100&app_start=z2ui5_cl_app_hello_world` ).

    DATA temp77 LIKE LINE OF lt_param.
    DATA temp78 LIKE sy-tabix.
    temp78 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `sap-client` INTO temp77.
    sy-tabix = temp78.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `100`
                                        act = temp77-v ).

    DATA temp79 LIKE LINE OF lt_param.
    DATA temp80 LIKE sy-tabix.
    temp80 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `app_start` INTO temp79.
    sy-tabix = temp80.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `z2ui5_cl_app_hello_world`
                                        act = temp79-v ).

  ENDMETHOD.

  METHOD test_url_param_set.

    DATA lv_param TYPE string.
    lv_param = zabaputil_cl_util=>url_param_set(
                         name  = `app_start`
                         value = `z2ui5_cl_app_hello_world2`
                         url   = `https://url.com/rvice_for_ui?sap-client=100&app_start=z2ui5_cl_app_hello_world` ).

    cl_abap_unit_assert=>assert_equals( exp = `sap-client=100&app_start=z2ui5_cl_app_hello_world2`
                                        act = lv_param ).

  ENDMETHOD.

  METHOD test_x_check_raise_not.

    TRY.
        DATA temp9 TYPE xsdboolean.
        temp9 = boolc( 1 = 2 ).
        zabaputil_cl_util=>x_check_raise( temp9 ).
      CATCH zabaputil_cx_util_error.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_rtti_get_type_name.

    DATA temp81 TYPE xsdboolean.
    CLEAR temp81.
    DATA lv_xsdbool LIKE temp81.
    lv_xsdbool = temp81.
    DATA lv_name TYPE string.
    lv_name = zabaputil_cl_util=>rtti_get_type_name( lv_xsdbool ).
    cl_abap_unit_assert=>assert_equals( exp = `XSDBOOLEAN`
                                        act = lv_name ).

  ENDMETHOD.

  METHOD test_rtti_get_type_kind.

    DATA temp82 TYPE string.
    CLEAR temp82.
    DATA lv_string LIKE temp82.
    lv_string = temp82.

    DATA lv_type_kind TYPE string.
    lv_type_kind = zabaputil_cl_util=>rtti_get_type_kind( lv_string ).
    DATA lr_string TYPE REF TO string.
    cl_abap_unit_assert=>assert_equals( exp = cl_abap_typedescr=>typekind_string
                                        act = lv_type_kind ).

    CREATE DATA lr_string.
    lv_type_kind = zabaputil_cl_util=>rtti_get_type_kind( lr_string ).
    cl_abap_unit_assert=>assert_equals( exp = cl_abap_typedescr=>typekind_dref
                                        act = lv_type_kind ).

  ENDMETHOD.

  METHOD test_rtti_check_type_kind.

    DATA temp83 TYPE string.
    CLEAR temp83.
    DATA lv_string LIKE temp83.
    lv_string = temp83.
    DATA lr_string TYPE REF TO string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>rtti_check_type_kind_dref( lv_string ) ).

    CREATE DATA lr_string.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>rtti_check_type_kind_dref( lr_string ) ).

  ENDMETHOD.

  METHOD test_rtti_get_t_attri_by_obj.

    DATA lo_obj TYPE REF TO ltcl_test_app.
    CREATE OBJECT lo_obj TYPE ltcl_test_app.
    DATA lt_attri TYPE abap_attrdescr_tab.
    lt_attri = zabaputil_cl_util=>rtti_get_t_attri_by_oref( lo_obj ).

    IF lines( lt_attri ) <> 7.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp84 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `MS_TAB` TRANSPORTING NO FIELDS.
    temp84 = sy-subrc.
    IF NOT temp84 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp85 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `SS_TAB` type_kind = `v` TRANSPORTING NO FIELDS.
    temp85 = sy-subrc.
    IF NOT temp85 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp86 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `SV_VAR` type_kind = `g` is_class = abap_true TRANSPORTING NO FIELDS.
    temp86 = sy-subrc.
    IF NOT temp86 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp87 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `SV_STATUS` type_kind = `g` is_class = abap_true is_constant = `X` TRANSPORTING NO FIELDS.
    temp87 = sy-subrc.
    IF NOT temp87 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_rtti_get_t_comp_by_struc.

    TYPES:
      BEGIN OF ty_row,
        title    TYPE string,
        value    TYPE string,
        descr    TYPE string,
        icon     TYPE string,
        info     TYPE string,
        selected TYPE abap_bool,
        checkbox TYPE abap_bool,
      END OF ty_row.

    DATA temp88 TYPE ty_row.
    CLEAR temp88.
    DATA ls_row LIKE temp88.
    ls_row = temp88.

    DATA lt_comp TYPE abap_component_tab.
    lt_comp = zabaputil_cl_util=>rtti_get_t_attri_by_any( ls_row ).

    IF lines( lt_comp ) <> 7.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp89 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `TITLE` TRANSPORTING NO FIELDS.
    temp89 = sy-subrc.
    IF NOT temp89 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp90 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `VALUE` TRANSPORTING NO FIELDS.
    temp90 = sy-subrc.
    IF NOT temp90 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp91 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `SELECTED` TRANSPORTING NO FIELDS.
    temp91 = sy-subrc.
    IF NOT temp91 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp92 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `CHECKBOX` TRANSPORTING NO FIELDS.
    temp92 = sy-subrc.
    IF NOT temp92 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA ls_title LIKE LINE OF lt_comp.
    DATA temp11 LIKE LINE OF lt_comp.
    DATA temp12 LIKE sy-tabix.
    temp12 = sy-tabix.
    READ TABLE lt_comp INDEX 1 INTO temp11.
    sy-tabix = temp12.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_title = temp11.

    IF ls_title-type->type_kind <> `g`.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_trans_xml_any_2__w_obj.

    DATA lo_obj TYPE REF TO ltcl_test_app.
    CREATE OBJECT lo_obj TYPE ltcl_test_app.
    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util=>xml_stringify( lo_obj ).

    IF lv_xml IS INITIAL.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
  ENDMETHOD.

  METHOD test_trans_xml_2_any__w_obj.

    DATA lo_obj TYPE REF TO ltcl_test_app.
    CREATE OBJECT lo_obj TYPE ltcl_test_app.
    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util=>xml_stringify( lo_obj ).

    CLEAR lo_obj.
    zabaputil_cl_util=>xml_parse( EXPORTING xml = lv_xml
                              IMPORTING any = lo_obj ).

    IF lo_obj IS NOT BOUND.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_trans_xml_any_2__w_data.

    TYPES:
      BEGIN OF ty_row,
        title    TYPE string,
        value    TYPE string,
        descr    TYPE string,
        icon     TYPE string,
        info     TYPE string,
        selected TYPE abap_bool,
        checkbox TYPE abap_bool,
      END OF ty_row.

    DATA temp93 TYPE ty_row.
    CLEAR temp93.
    DATA ls_row LIKE temp93.
    ls_row = temp93.
    ls_row-value = `test`.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util=>xml_stringify( ls_row ).

    IF lv_xml IS INITIAL.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_trans_xml_2_any__w_data.

    TYPES:
      BEGIN OF ty_row,
        title    TYPE string,
        value    TYPE string,
        descr    TYPE string,
        icon     TYPE string,
        info     TYPE string,
        selected TYPE abap_bool,
        checkbox TYPE abap_bool,
      END OF ty_row.

    DATA temp94 TYPE ty_row.
    CLEAR temp94.
    DATA ls_row LIKE temp94.
    ls_row = temp94.
    DATA temp95 TYPE ty_row.
    CLEAR temp95.
    DATA ls_row2 LIKE temp95.
    ls_row2 = temp95.
    ls_row-value = `test`.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util=>xml_stringify( ls_row ).

    zabaputil_cl_util=>xml_parse( EXPORTING xml = lv_xml
                              IMPORTING any = ls_row2 ).

    cl_abap_unit_assert=>assert_equals( exp = ls_row2
                                        act = ls_row ).

  ENDMETHOD.

  METHOD test_c_trim_horizontal_tab.

    IF zabaputil_cl_util=>c_trim( |{ cl_abap_char_utilities=>horizontal_tab }|
                                && |JsadfHHs|
                                && |{ cl_abap_char_utilities=>horizontal_tab }| ) <> `JsadfHHs`.
      cl_abap_unit_assert=>fail( ).

    ENDIF.

  ENDMETHOD.

  METHOD test_get_token_t_by_r_t.

    DATA temp96 TYPE zabaputil_cl_util=>ty_t_range.
    CLEAR temp96.
    DATA temp97 LIKE LINE OF temp96.
    temp97-sign = `I`.
    temp97-option = `EQ`.
    temp97-low = `table`.
    temp97-high = ``.
    INSERT temp97 INTO TABLE temp96.
    DATA lt_range LIKE temp96.
    lt_range = temp96.

    DATA lt_result TYPE zabaputil_cl_util=>ty_t_token.
    lt_result = zabaputil_cl_util=>filter_get_token_t_by_range_t( lt_range ).

    DATA temp98 TYPE zabaputil_cl_util=>ty_t_token.
    CLEAR temp98.
    DATA temp99 LIKE LINE OF temp98.
    temp99-key = `=table`.
    temp99-text = `=table`.
    temp99-visible = `X`.
    temp99-selkz = ``.
    temp99-editable = `X`.
    INSERT temp99 INTO TABLE temp98.
    DATA lt_exp LIKE temp98.
    lt_exp = temp98.

    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = lt_result
    ).

  ENDMETHOD.

  METHOD test_rtti_get_t_attri_by_incl.

    IF sy-sysid = `ABC`.
      RETURN.
    ENDIF.

    TYPES:
      BEGIN OF ty_struc_incl,
        incl_title  TYPE string,
        incl_value  TYPE string,
        incl_value2 TYPE string,
      END OF ty_struc_incl.

    TYPES:
      BEGIN OF ty_struc,
        title  TYPE string,
        value  TYPE string,
        value2 TYPE string,
      END OF ty_struc.

    DATA BEGIN OF ms_struc2.
    INCLUDE TYPE ty_struc.
    INCLUDE TYPE ty_struc_incl.
    DATA END OF ms_struc2.

    DATA lo_datadescr TYPE REF TO cl_abap_typedescr.
    lo_datadescr = cl_abap_typedescr=>describe_by_data( ms_struc2 ).

    IF zabaputil_cl_util=>context_check_abap_cloud( ) IS NOT INITIAL.
      RETURN.
    ENDIF.
    DATA temp100 TYPE REF TO cl_abap_datadescr.
    temp100 ?= lo_datadescr.
    DATA lt_attri TYPE abap_component_tab.
    lt_attri = zabaputil_cl_util=>rtti_get_t_attri_by_include( temp100 ).

    IF lines( lt_attri ) <> 6.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

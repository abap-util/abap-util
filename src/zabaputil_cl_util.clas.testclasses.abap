
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
    DATA temp85 TYPE ltcl_test_app=>ty_row.
    CLEAR temp85.
    ss_tab = temp85.
    DATA temp86 LIKE st_tab.
    CLEAR temp86.
    st_tab = temp86.

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
    METHODS test_time_subtract_seconds    FOR TESTING RAISING cx_static_check.
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
    METHODS test_check_unassign_initial     FOR TESTING RAISING cx_static_check.
    METHODS conv_copy_ref_data             FOR TESTING RAISING cx_static_check.
    METHODS rtti_check_ref_data            FOR TESTING RAISING cx_static_check.
    METHODS test_check_bound_a_not_initial  FOR TESTING RAISING cx_static_check.
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

    DATA temp87 TYPE REF TO cl_abap_classdescr.
    temp87 ?= cl_abap_objectdescr=>describe_by_object_ref( lo_app ).
    DATA lt_attri LIKE temp87->attributes.
    lt_attri = temp87->attributes.

    DATA lv_test LIKE LINE OF lt_attri.
    DATA temp10 LIKE LINE OF lt_attri.
    DATA temp11 LIKE sy-tabix.
    temp11 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `MS_TAB` INTO temp10.
    sy-tabix = temp11.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp10.
    DATA temp88 LIKE LINE OF lt_attri.
    DATA temp89 LIKE sy-tabix.
    temp89 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `MT_TAB` INTO temp88.
    sy-tabix = temp89.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp88.
    DATA temp90 LIKE LINE OF lt_attri.
    DATA temp91 LIKE sy-tabix.
    temp91 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `MV_VAL` INTO temp90.
    sy-tabix = temp91.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp90.
    DATA temp92 LIKE LINE OF lt_attri.
    DATA temp93 LIKE sy-tabix.
    temp93 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `SS_TAB` INTO temp92.
    sy-tabix = temp93.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp92.
    DATA temp94 LIKE LINE OF lt_attri.
    DATA temp95 LIKE sy-tabix.
    temp95 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `ST_TAB` INTO temp94.
    sy-tabix = temp95.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp94.
    DATA temp96 LIKE LINE OF lt_attri.
    DATA temp97 LIKE sy-tabix.
    temp97 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `SV_STATUS` INTO temp96.
    sy-tabix = temp97.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp96.
    DATA temp98 LIKE LINE OF lt_attri.
    DATA temp99 LIKE sy-tabix.
    temp99 = sy-tabix.
    READ TABLE lt_attri WITH KEY name = `SV_VAR` INTO temp98.
    sy-tabix = temp99.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_test = temp98.

  ENDMETHOD.

  METHOD test_eledescr_rel_name.

    DATA temp100 TYPE REF TO cl_abap_elemdescr.
    temp100 ?= cl_abap_elemdescr=>describe_by_data( abap_true ).
    DATA lo_ele LIKE temp100.
    lo_ele = temp100.

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

  METHOD test_check_bound_a_not_initial.

    DATA lv_test TYPE string.
    lv_test = `test`.
    DATA lr_test LIKE REF TO lv_test.
    GET REFERENCE OF lv_test INTO lr_test.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>check_bound_a_not_initial( lr_test ) ).
    CLEAR lv_test.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>check_bound_a_not_initial( lr_test ) ).
    CLEAR lr_test.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>check_bound_a_not_initial( lr_test ) ).

  ENDMETHOD.

  METHOD test_check_unassign_initial.

    DATA lv_test TYPE string.
    lv_test = `test`.
    DATA lr_test LIKE REF TO lv_test.
    GET REFERENCE OF lv_test INTO lr_test.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>check_unassign_initial( lr_test ) ).

    CLEAR lv_test.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>check_unassign_initial( lr_test ) ).

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
    lv_time2 = zabaputil_cl_util=>time_subtract_seconds( time    = lv_time
                                                            seconds = 60 * 60 * 4 ).

    IF lv_time IS INITIAL OR lv_time2 IS INITIAL.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    IF lv_time < lv_time2.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

  METHOD test_time_subtract_seconds.

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

    DATA temp101 TYPE ty_row.
    CLEAR temp101.
    temp101-title = `test`.
    DATA ls_row LIKE temp101.
    ls_row = temp101.

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

    DATA temp102 LIKE LINE OF lt_param.
    DATA temp103 LIKE sy-tabix.
    temp103 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `sap-client` INTO temp102.
    sy-tabix = temp103.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `100`
                                        act = temp102-v ).

    DATA temp104 LIKE LINE OF lt_param.
    DATA temp105 LIKE sy-tabix.
    temp105 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `app_start` INTO temp104.
    sy-tabix = temp105.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `z2ui5_cl_app_hello_world`
                                        act = temp104-v ).

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

    DATA temp106 TYPE xsdboolean.
    CLEAR temp106.
    DATA lv_xsdbool LIKE temp106.
    lv_xsdbool = temp106.
    DATA lv_name TYPE string.
    lv_name = zabaputil_cl_util=>rtti_get_type_name( lv_xsdbool ).
    cl_abap_unit_assert=>assert_equals( exp = `XSDBOOLEAN`
                                        act = lv_name ).

  ENDMETHOD.

  METHOD test_rtti_get_type_kind.

    DATA temp107 TYPE string.
    CLEAR temp107.
    DATA lv_string LIKE temp107.
    lv_string = temp107.

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

    DATA temp108 TYPE string.
    CLEAR temp108.
    DATA lv_string LIKE temp108.
    lv_string = temp108.
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

    DATA temp109 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `MS_TAB` TRANSPORTING NO FIELDS.
    temp109 = sy-subrc.
    IF NOT temp109 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp110 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `SS_TAB` type_kind = `v` TRANSPORTING NO FIELDS.
    temp110 = sy-subrc.
    IF NOT temp110 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp111 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `SV_VAR` type_kind = `g` is_class = abap_true TRANSPORTING NO FIELDS.
    temp111 = sy-subrc.
    IF NOT temp111 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp112 LIKE sy-subrc.
    READ TABLE lt_attri WITH KEY name = `SV_STATUS` type_kind = `g` is_class = abap_true is_constant = `X` TRANSPORTING NO FIELDS.
    temp112 = sy-subrc.
    IF NOT temp112 = 0.
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

    DATA temp113 TYPE ty_row.
    CLEAR temp113.
    DATA ls_row LIKE temp113.
    ls_row = temp113.

    DATA lt_comp TYPE abap_component_tab.
    lt_comp = zabaputil_cl_util=>rtti_get_t_attri_by_any( ls_row ).

    IF lines( lt_comp ) <> 7.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp114 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `TITLE` TRANSPORTING NO FIELDS.
    temp114 = sy-subrc.
    IF NOT temp114 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp115 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `VALUE` TRANSPORTING NO FIELDS.
    temp115 = sy-subrc.
    IF NOT temp115 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp116 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `SELECTED` TRANSPORTING NO FIELDS.
    temp116 = sy-subrc.
    IF NOT temp116 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA temp117 LIKE sy-subrc.
    READ TABLE lt_comp WITH KEY name = `CHECKBOX` TRANSPORTING NO FIELDS.
    temp117 = sy-subrc.
    IF NOT temp117 = 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    DATA ls_title LIKE LINE OF lt_comp.
    DATA temp12 LIKE LINE OF lt_comp.
    DATA temp13 LIKE sy-tabix.
    temp13 = sy-tabix.
    READ TABLE lt_comp INDEX 1 INTO temp12.
    sy-tabix = temp13.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_title = temp12.

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

    DATA temp118 TYPE ty_row.
    CLEAR temp118.
    DATA ls_row LIKE temp118.
    ls_row = temp118.
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

    DATA temp119 TYPE ty_row.
    CLEAR temp119.
    DATA ls_row LIKE temp119.
    ls_row = temp119.
    DATA temp120 TYPE ty_row.
    CLEAR temp120.
    DATA ls_row2 LIKE temp120.
    ls_row2 = temp120.
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

    DATA temp121 TYPE zabaputil_cl_util=>ty_t_range.
    CLEAR temp121.
    DATA temp122 LIKE LINE OF temp121.
    temp122-sign = `I`.
    temp122-option = `EQ`.
    temp122-low = `table`.
    temp122-high = ``.
    INSERT temp122 INTO TABLE temp121.
    DATA lt_range LIKE temp121.
    lt_range = temp121.

    DATA lt_result TYPE zabaputil_cl_util=>ty_t_token.
    lt_result = zabaputil_cl_util=>filter_get_token_t_by_range_t( lt_range ).

    DATA temp123 TYPE zabaputil_cl_util=>ty_t_token.
    CLEAR temp123.
    DATA temp124 LIKE LINE OF temp123.
    temp124-key = `=table`.
    temp124-text = `=table`.
    temp124-visible = `X`.
    temp124-selkz = ``.
    temp124-editable = `X`.
    INSERT temp124 INTO TABLE temp123.
    DATA lt_exp LIKE temp123.
    lt_exp = temp123.

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
    DATA temp125 TYPE REF TO cl_abap_datadescr.
    temp125 ?= lo_datadescr.
    DATA lt_attri TYPE abap_component_tab.
    lt_attri = zabaputil_cl_util=>rtti_get_t_attri_by_include( temp125 ).

    IF lines( lt_attri ) <> 6.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.


CLASS ltcl_unit_test_strings DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_c_contains_found       FOR TESTING RAISING cx_static_check.
    METHODS test_c_contains_not_found   FOR TESTING RAISING cx_static_check.
    METHODS test_c_starts_with_true     FOR TESTING RAISING cx_static_check.
    METHODS test_c_starts_with_false    FOR TESTING RAISING cx_static_check.
    METHODS test_c_starts_with_longer   FOR TESTING RAISING cx_static_check.
    METHODS test_c_ends_with_true       FOR TESTING RAISING cx_static_check.
    METHODS test_c_ends_with_false      FOR TESTING RAISING cx_static_check.
    METHODS test_c_ends_with_longer     FOR TESTING RAISING cx_static_check.
    METHODS test_c_split                FOR TESTING RAISING cx_static_check.
    METHODS test_c_split_single         FOR TESTING RAISING cx_static_check.
    METHODS test_c_join                 FOR TESTING RAISING cx_static_check.
    METHODS test_c_join_with_sep        FOR TESTING RAISING cx_static_check.
    METHODS test_c_join_empty           FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test_strings IMPLEMENTATION.

  METHOD test_c_contains_found.

    cl_abap_unit_assert=>assert_true(
      zabaputil_cl_util=>c_contains( val = `Hello World`
                                  sub = `World` ) ).

  ENDMETHOD.

  METHOD test_c_contains_not_found.

    cl_abap_unit_assert=>assert_false(
      zabaputil_cl_util=>c_contains( val = `Hello World`
                                  sub = `xyz` ) ).

  ENDMETHOD.

  METHOD test_c_starts_with_true.

    cl_abap_unit_assert=>assert_true(
      zabaputil_cl_util=>c_starts_with( val    = `Hello World`
                                     prefix = `Hello` ) ).

  ENDMETHOD.

  METHOD test_c_starts_with_false.

    cl_abap_unit_assert=>assert_false(
      zabaputil_cl_util=>c_starts_with( val    = `Hello World`
                                     prefix = `World` ) ).

  ENDMETHOD.

  METHOD test_c_starts_with_longer.

    cl_abap_unit_assert=>assert_false(
      zabaputil_cl_util=>c_starts_with( val    = `Hi`
                                     prefix = `Hello World` ) ).

  ENDMETHOD.

  METHOD test_c_ends_with_true.

    cl_abap_unit_assert=>assert_true(
      zabaputil_cl_util=>c_ends_with( val    = `Hello World`
                                   suffix = `World` ) ).

  ENDMETHOD.

  METHOD test_c_ends_with_false.

    cl_abap_unit_assert=>assert_false(
      zabaputil_cl_util=>c_ends_with( val    = `Hello World`
                                   suffix = `Hello` ) ).

  ENDMETHOD.

  METHOD test_c_ends_with_longer.

    cl_abap_unit_assert=>assert_false(
      zabaputil_cl_util=>c_ends_with( val    = `Hi`
                                   suffix = `Hello World` ) ).

  ENDMETHOD.

  METHOD test_c_split.

    DATA lt_result TYPE string_table.
    lt_result = zabaputil_cl_util=>c_split( val = `one;two;three`
                                               sep = `;` ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_result ) ).
    DATA temp126 LIKE LINE OF lt_result.
    DATA temp127 LIKE sy-tabix.
    temp127 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp126.
    sy-tabix = temp127.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `one`   act = temp126 ).
    DATA temp128 LIKE LINE OF lt_result.
    DATA temp129 LIKE sy-tabix.
    temp129 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp128.
    sy-tabix = temp129.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `two`   act = temp128 ).
    DATA temp130 LIKE LINE OF lt_result.
    DATA temp131 LIKE sy-tabix.
    temp131 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp130.
    sy-tabix = temp131.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `three` act = temp130 ).

  ENDMETHOD.

  METHOD test_c_split_single.

    DATA lt_result TYPE string_table.
    lt_result = zabaputil_cl_util=>c_split( val = `nosep`
                                               sep = `;` ).

    cl_abap_unit_assert=>assert_equals( exp = 1       act = lines( lt_result ) ).
    DATA temp132 LIKE LINE OF lt_result.
    DATA temp133 LIKE sy-tabix.
    temp133 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp132.
    sy-tabix = temp133.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `nosep` act = temp132 ).

  ENDMETHOD.

  METHOD test_c_join.

    DATA temp134 TYPE string_table.
    CLEAR temp134.
    INSERT `one` INTO TABLE temp134.
    INSERT `two` INTO TABLE temp134.
    INSERT `three` INTO TABLE temp134.
    DATA lt_tab LIKE temp134.
    lt_tab = temp134.
    DATA lv_result TYPE string.
    lv_result = zabaputil_cl_util=>c_join( lt_tab ).

    cl_abap_unit_assert=>assert_equals( exp = `onetwothree`
                                        act = lv_result ).

  ENDMETHOD.

  METHOD test_c_join_with_sep.

    DATA temp136 TYPE string_table.
    CLEAR temp136.
    INSERT `one` INTO TABLE temp136.
    INSERT `two` INTO TABLE temp136.
    INSERT `three` INTO TABLE temp136.
    DATA lt_tab LIKE temp136.
    lt_tab = temp136.
    DATA lv_result TYPE string.
    lv_result = zabaputil_cl_util=>c_join( tab = lt_tab
                                              sep = `-` ).

    cl_abap_unit_assert=>assert_equals( exp = `one-two-three`
                                        act = lv_result ).

  ENDMETHOD.

  METHOD test_c_join_empty.

    DATA temp138 TYPE string_table.
    CLEAR temp138.
    DATA lt_tab LIKE temp138.
    lt_tab = temp138.
    DATA lv_result TYPE string.
    lv_result = zabaputil_cl_util=>c_join( lt_tab ).

    cl_abap_unit_assert=>assert_initial( lv_result ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_unit_test_rtti DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_rtti_check_table_true    FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_table_false   FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_struc_true    FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_struc_false   FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_numeric_int   FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_numeric_str   FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_clike_str     FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_clike_int     FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_class_exists  FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_check_class_no      FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_tab_rel_name        FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test_rtti IMPLEMENTATION.

  METHOD test_rtti_check_table_true.

    DATA lt_tab TYPE string_table.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>rtti_check_table( lt_tab ) ).

  ENDMETHOD.

  METHOD test_rtti_check_table_false.

    DATA lv_string TYPE string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>rtti_check_table( lv_string ) ).

  ENDMETHOD.

  METHOD test_rtti_check_struc_true.

    TYPES:
      BEGIN OF ty_s,
        a TYPE string,
        b TYPE i,
      END OF ty_s.

    DATA temp139 TYPE ty_s.
    CLEAR temp139.
    DATA ls_struc LIKE temp139.
    ls_struc = temp139.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>rtti_check_structure( ls_struc ) ).

  ENDMETHOD.

  METHOD test_rtti_check_struc_false.

    DATA lv_string TYPE string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>rtti_check_structure( lv_string ) ).

  ENDMETHOD.

  METHOD test_rtti_check_numeric_int.

    DATA lv_int TYPE i.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>rtti_check_numeric( lv_int ) ).

  ENDMETHOD.

  METHOD test_rtti_check_numeric_str.

    DATA lv_string TYPE string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>rtti_check_numeric( lv_string ) ).

  ENDMETHOD.

  METHOD test_rtti_check_clike_str.

    DATA lv_string TYPE string.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>rtti_check_clike( lv_string ) ).

  ENDMETHOD.

  METHOD test_rtti_check_clike_int.

    DATA lv_int TYPE i.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>rtti_check_clike( lv_int ) ).

  ENDMETHOD.

  METHOD test_rtti_check_class_exists.

    cl_abap_unit_assert=>assert_true(
      zabaputil_cl_util=>rtti_check_class_exists( `Z2UI5_CL_UTIL` ) ).

  ENDMETHOD.

  METHOD test_rtti_check_class_no.

    cl_abap_unit_assert=>assert_false(
      zabaputil_cl_util=>rtti_check_class_exists( `Z2UI5_CL_DOES_NOT_EXIST_XYZ` ) ).

  ENDMETHOD.

  METHOD test_rtti_tab_rel_name.

    TYPES:
      BEGIN OF ty_row,
        title TYPE string,
        value TYPE string,
      END OF ty_row.

    TYPES temp2 TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA lt_tab TYPE temp2.

    DATA lv_name TYPE string.
    lv_name = zabaputil_cl_util=>rtti_tab_get_relative_name( lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = `TY_ROW`
                                        act = lv_name ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_unit_test_boolean DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_bool_check_name_true    FOR TESTING RAISING cx_static_check.
    METHODS test_bool_check_name_false   FOR TESTING RAISING cx_static_check.
    METHODS test_bool_abap2json_true     FOR TESTING RAISING cx_static_check.
    METHODS test_bool_abap2json_false    FOR TESTING RAISING cx_static_check.
    METHODS test_bool_abap2json_nonbool  FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_msg_type_error      FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_msg_type_success    FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_msg_type_warning    FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_msg_type_info       FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_msg_type_other      FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test_boolean IMPLEMENTATION.

  METHOD test_bool_check_name_true.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `ABAP_BOOL` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `XSDBOOLEAN` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `FLAG` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `XFLAG` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `XFELD` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `ABAP_BOOLEAN` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `WDY_BOOLEAN` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `BOOLE_D` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util=>boolean_check_by_name( `OS_BOOLEAN` ) ).

  ENDMETHOD.

  METHOD test_bool_check_name_false.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>boolean_check_by_name( `STRING` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>boolean_check_by_name( `INT4` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util=>boolean_check_by_name( `` ) ).

  ENDMETHOD.

  METHOD test_bool_abap2json_true.

    cl_abap_unit_assert=>assert_equals( exp = `true`
                                        act = zabaputil_cl_util=>boolean_abap_2_json( abap_true ) ).

  ENDMETHOD.

  METHOD test_bool_abap2json_false.

    cl_abap_unit_assert=>assert_equals( exp = `false`
                                        act = zabaputil_cl_util=>boolean_abap_2_json( abap_false ) ).

  ENDMETHOD.

  METHOD test_bool_abap2json_nonbool.

    cl_abap_unit_assert=>assert_equals( exp = `some_value`
                                        act = zabaputil_cl_util=>boolean_abap_2_json( `some_value` ) ).

  ENDMETHOD.

  METHOD test_ui5_msg_type_error.

    cl_abap_unit_assert=>assert_equals( exp = `Error`
                                        act = zabaputil_cl_util=>ui5_get_msg_type( `E` ) ).

  ENDMETHOD.

  METHOD test_ui5_msg_type_success.

    cl_abap_unit_assert=>assert_equals( exp = `Success`
                                        act = zabaputil_cl_util=>ui5_get_msg_type( `S` ) ).

  ENDMETHOD.

  METHOD test_ui5_msg_type_warning.

    cl_abap_unit_assert=>assert_equals( exp = `Warning`
                                        act = zabaputil_cl_util=>ui5_get_msg_type( `W` ) ).

  ENDMETHOD.

  METHOD test_ui5_msg_type_info.

    cl_abap_unit_assert=>assert_equals( exp = `Information`
                                        act = zabaputil_cl_util=>ui5_get_msg_type( `I` ) ).

  ENDMETHOD.

  METHOD test_ui5_msg_type_other.

    cl_abap_unit_assert=>assert_equals( exp = `Information`
                                        act = zabaputil_cl_util=>ui5_get_msg_type( `X` ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_unit_test_filter DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_range_by_token_eq      FOR TESTING RAISING cx_static_check.
    METHODS test_range_by_token_lt      FOR TESTING RAISING cx_static_check.
    METHODS test_range_by_token_le      FOR TESTING RAISING cx_static_check.
    METHODS test_range_by_token_gt      FOR TESTING RAISING cx_static_check.
    METHODS test_range_by_token_ge      FOR TESTING RAISING cx_static_check.
    METHODS test_range_by_token_cp      FOR TESTING RAISING cx_static_check.
    METHODS test_range_by_token_bt      FOR TESTING RAISING cx_static_check.
    METHODS test_range_by_token_plain   FOR TESTING RAISING cx_static_check.
    METHODS test_token_range_mapping    FOR TESTING RAISING cx_static_check.
    METHODS test_range_t_by_token_t     FOR TESTING RAISING cx_static_check.
    METHODS test_filter_get_multi       FOR TESTING RAISING cx_static_check.
    METHODS test_filter_get_data        FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test_filter IMPLEMENTATION.

  METHOD test_range_by_token_eq.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `=ABC` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`   act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ`  act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `ABC` act = ls_range-low ).

  ENDMETHOD.

  METHOD test_range_by_token_lt.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `<100` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`   act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `LT`  act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `100` act = ls_range-low ).

  ENDMETHOD.

  METHOD test_range_by_token_le.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `<=200` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`   act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `LE`  act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `200` act = ls_range-low ).

  ENDMETHOD.

  METHOD test_range_by_token_gt.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `>50` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`  act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `GT` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `50` act = ls_range-low ).

  ENDMETHOD.

  METHOD test_range_by_token_ge.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `>=75` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`  act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `GE` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `75` act = ls_range-low ).

  ENDMETHOD.

  METHOD test_range_by_token_cp.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `*test*` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`    act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `CP`   act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `test` act = zabaputil_cl_util=>c_trim( ls_range-low ) ).

  ENDMETHOD.

  METHOD test_range_by_token_bt.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `100...500` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`   act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `BT`  act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `100` act = ls_range-low ).
    cl_abap_unit_assert=>assert_equals( exp = `500` act = ls_range-high ).

  ENDMETHOD.

  METHOD test_range_by_token_plain.

    DATA ls_range TYPE zabaputil_cl_util=>ty_s_range.
    ls_range = zabaputil_cl_util=>filter_get_range_by_token( `hello` ).

    cl_abap_unit_assert=>assert_equals( exp = `I`     act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ`    act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `hello` act = ls_range-low ).

  ENDMETHOD.

  METHOD test_token_range_mapping.

    DATA lt_mapping TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_mapping = zabaputil_cl_util=>filter_get_token_range_mapping( ).

    cl_abap_unit_assert=>assert_not_initial( lt_mapping ).
    DATA temp140 LIKE LINE OF lt_mapping.
    DATA temp141 LIKE sy-tabix.
    temp141 = sy-tabix.
    READ TABLE lt_mapping WITH KEY n = `EQ` INTO temp140.
    sy-tabix = temp141.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `={LOW}`
                                        act = temp140-v ).
    DATA temp142 LIKE LINE OF lt_mapping.
    DATA temp143 LIKE sy-tabix.
    temp143 = sy-tabix.
    READ TABLE lt_mapping WITH KEY n = `LT` INTO temp142.
    sy-tabix = temp143.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `<{LOW}`
                                        act = temp142-v ).
    DATA temp144 LIKE LINE OF lt_mapping.
    DATA temp145 LIKE sy-tabix.
    temp145 = sy-tabix.
    READ TABLE lt_mapping WITH KEY n = `GT` INTO temp144.
    sy-tabix = temp145.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `>{LOW}`
                                        act = temp144-v ).

  ENDMETHOD.

  METHOD test_range_t_by_token_t.

    DATA temp146 TYPE zabaputil_cl_util=>ty_t_token.
    CLEAR temp146.
    DATA temp147 LIKE LINE OF temp146.
    temp147-key = `=100`.
    temp147-text = `=100`.
    temp147-visible = abap_true.
    temp147-editable = abap_true.
    INSERT temp147 INTO TABLE temp146.
    temp147-key = `>50`.
    temp147-text = `>50`.
    temp147-visible = abap_true.
    temp147-editable = abap_true.
    INSERT temp147 INTO TABLE temp146.
    DATA lt_tokens LIKE temp146.
    lt_tokens = temp146.

    DATA lt_range TYPE zabaputil_cl_util=>ty_t_range.
    lt_range = zabaputil_cl_util=>filter_get_range_t_by_token_t( lt_tokens ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_range ) ).
    DATA temp148 LIKE LINE OF lt_range.
    DATA temp149 LIKE sy-tabix.
    temp149 = sy-tabix.
    READ TABLE lt_range INDEX 1 INTO temp148.
    sy-tabix = temp149.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = temp148-option ).
    DATA temp150 LIKE LINE OF lt_range.
    DATA temp151 LIKE sy-tabix.
    temp151 = sy-tabix.
    READ TABLE lt_range INDEX 2 INTO temp150.
    sy-tabix = temp151.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `GT` act = temp150-option ).

  ENDMETHOD.

  METHOD test_filter_get_multi.

    TYPES:
      BEGIN OF ty_row,
        name  TYPE string,
        value TYPE string,
      END OF ty_row.

    DATA temp152 TYPE ty_row.
    CLEAR temp152.
    DATA ls_row LIKE temp152.
    ls_row = temp152.
    DATA lt_filter TYPE zabaputil_cl_util=>ty_t_filter_multi.
    lt_filter = zabaputil_cl_util=>filter_get_multi_by_data( ls_row ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_filter ) ).
    DATA temp153 LIKE LINE OF lt_filter.
    DATA temp154 LIKE sy-tabix.
    temp154 = sy-tabix.
    READ TABLE lt_filter INDEX 1 INTO temp153.
    sy-tabix = temp154.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NAME`  act = temp153-name ).
    DATA temp155 LIKE LINE OF lt_filter.
    DATA temp156 LIKE sy-tabix.
    temp156 = sy-tabix.
    READ TABLE lt_filter INDEX 2 INTO temp155.
    sy-tabix = temp156.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `VALUE` act = temp155-name ).

  ENDMETHOD.

  METHOD test_filter_get_data.

    DATA lt_filter TYPE zabaputil_cl_util=>ty_t_filter_multi.
    DATA ls_filter TYPE zabaputil_cl_util=>ty_s_filter_multi.

    ls_filter-name = `F1`.
    DATA temp157 TYPE zabaputil_cl_util=>ty_t_range.
    CLEAR temp157.
    DATA temp158 LIKE LINE OF temp157.
    temp158-sign = `I`.
    temp158-option = `EQ`.
    temp158-low = `A`.
    INSERT temp158 INTO TABLE temp157.
    ls_filter-t_range = temp157.
    INSERT ls_filter INTO TABLE lt_filter.

    CLEAR ls_filter.
    ls_filter-name = `F2`.
    INSERT ls_filter INTO TABLE lt_filter.

    CLEAR ls_filter.
    ls_filter-name = `F3`.
    DATA temp159 TYPE zabaputil_cl_util=>ty_t_token.
    CLEAR temp159.
    DATA temp160 LIKE LINE OF temp159.
    temp160-key = `=B`.
    temp160-text = `=B`.
    INSERT temp160 INTO TABLE temp159.
    ls_filter-t_token = temp159.
    INSERT ls_filter INTO TABLE lt_filter.

    DATA lt_result TYPE zabaputil_cl_util=>ty_t_filter_multi.
    lt_result = zabaputil_cl_util=>filter_get_data_by_multi( lt_filter ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_result ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_unit_test_conversion DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_json_roundtrip          FOR TESTING RAISING cx_static_check.
    METHODS test_csv_roundtrip           FOR TESTING RAISING cx_static_check.
    METHODS test_conv_get_as_data_ref    FOR TESTING RAISING cx_static_check.
    METHODS test_conv_string_to_date     FOR TESTING RAISING cx_static_check.
    METHODS test_conv_date_to_string     FOR TESTING RAISING cx_static_check.
    METHODS test_conv_date_roundtrip     FOR TESTING RAISING cx_static_check.
    METHODS test_itab_get_by_struc       FOR TESTING RAISING cx_static_check.
    METHODS test_time_add_seconds        FOR TESTING RAISING cx_static_check.
    METHODS test_time_diff_seconds       FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test_conversion IMPLEMENTATION.

  METHOD test_json_roundtrip.

    TYPES:
      BEGIN OF ty_data,
        name  TYPE string,
        value TYPE i,
        flag  TYPE abap_bool,
      END OF ty_data.

    DATA temp161 TYPE ty_data.
    CLEAR temp161.
    temp161-name = `test`.
    temp161-value = 42.
    temp161-flag = abap_true.
    DATA ls_in LIKE temp161.
    ls_in = temp161.

    DATA lv_json TYPE string.
    lv_json = zabaputil_cl_util=>json_stringify( ls_in ).
    cl_abap_unit_assert=>assert_not_initial( lv_json ).

    DATA ls_out TYPE ty_data.
    zabaputil_cl_util=>json_parse( EXPORTING val = lv_json
                                CHANGING data = ls_out ).

    cl_abap_unit_assert=>assert_equals( exp = `test` act = ls_out-name ).
    cl_abap_unit_assert=>assert_equals( exp = 42     act = ls_out-value ).
    cl_abap_unit_assert=>assert_true( ls_out-flag ).

  ENDMETHOD.

  METHOD test_csv_roundtrip.

    TYPES:
      BEGIN OF ty_row,
        col1 TYPE string,
        col2 TYPE string,
      END OF ty_row.

    TYPES temp3 TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA lt_in TYPE temp3.
    DATA temp162 LIKE lt_in.
    CLEAR temp162.
    DATA temp163 LIKE LINE OF temp162.
    temp163-col1 = `A`.
    temp163-col2 = `B`.
    INSERT temp163 INTO TABLE temp162.
    temp163-col1 = `C`.
    temp163-col2 = `D`.
    INSERT temp163 INTO TABLE temp162.
    lt_in = temp162.

    DATA lv_csv TYPE string.
    lv_csv = zabaputil_cl_util=>itab_get_csv_by_itab( lt_in ).
    cl_abap_unit_assert=>assert_not_initial( lv_csv ).

    DATA temp10 TYPE xsdboolean.
    temp10 = boolc( lv_csv CS `COL1` ).
    cl_abap_unit_assert=>assert_true(
      temp10 ).
    DATA temp11 TYPE xsdboolean.
    temp11 = boolc( lv_csv CS `COL2` ).
    cl_abap_unit_assert=>assert_true(
      temp11 ).

  ENDMETHOD.

  METHOD test_conv_get_as_data_ref.

    DATA lv_val TYPE string.
    lv_val = `test_value`.
    DATA lr_ref TYPE REF TO data.
    lr_ref = zabaputil_cl_util=>conv_get_as_data_ref( lv_val ).

    cl_abap_unit_assert=>assert_bound( lr_ref ).

    FIELD-SYMBOLS <any> TYPE any.
    ASSIGN lr_ref->* TO <any>.
    cl_abap_unit_assert=>assert_equals( exp = `test_value`
                                        act = <any> ).

  ENDMETHOD.

  METHOD test_conv_string_to_date.

    DATA lv_date TYPE d.
    lv_date = zabaputil_cl_util=>conv_string_to_date( `2024-03-15` ).
    cl_abap_unit_assert=>assert_equals( exp = `20240315`
                                        act = lv_date ).

  ENDMETHOD.

  METHOD test_conv_date_to_string.

    DATA temp164 TYPE d.
    temp164 = `20240315`.
    DATA lv_str TYPE string.
    lv_str = zabaputil_cl_util=>conv_date_to_string( temp164 ).
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15`
                                        act = lv_str ).

  ENDMETHOD.

  METHOD test_conv_date_roundtrip.

    DATA lv_original TYPE string.
    lv_original = `2025-12-31`.
    DATA lv_date TYPE d.
    lv_date = zabaputil_cl_util=>conv_string_to_date( lv_original ).
    DATA lv_back TYPE string.
    lv_back = zabaputil_cl_util=>conv_date_to_string( lv_date ).

    cl_abap_unit_assert=>assert_equals( exp = lv_original
                                        act = lv_back ).

  ENDMETHOD.

  METHOD test_itab_get_by_struc.

    TYPES:
      BEGIN OF ty_s,
        name  TYPE string,
        value TYPE string,
      END OF ty_s.

    DATA temp165 TYPE ty_s.
    CLEAR temp165.
    temp165-name = `test_name`.
    temp165-value = `test_value`.
    DATA ls_struc LIKE temp165.
    ls_struc = temp165.
    DATA lt_nv TYPE zabaputil_cl_util=>ty_t_name_value.
    lt_nv = zabaputil_cl_util=>itab_get_by_struc( ls_struc ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_nv ) ).
    DATA temp166 LIKE LINE OF lt_nv.
    DATA temp167 LIKE sy-tabix.
    temp167 = sy-tabix.
    READ TABLE lt_nv INDEX 1 INTO temp166.
    sy-tabix = temp167.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NAME`       act = temp166-n ).
    DATA temp168 LIKE LINE OF lt_nv.
    DATA temp169 LIKE sy-tabix.
    temp169 = sy-tabix.
    READ TABLE lt_nv INDEX 1 INTO temp168.
    sy-tabix = temp169.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `test_name`  act = temp168-v ).
    DATA temp170 LIKE LINE OF lt_nv.
    DATA temp171 LIKE sy-tabix.
    temp171 = sy-tabix.
    READ TABLE lt_nv INDEX 2 INTO temp170.
    sy-tabix = temp171.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `VALUE`      act = temp170-n ).
    DATA temp172 LIKE LINE OF lt_nv.
    DATA temp173 LIKE sy-tabix.
    temp173 = sy-tabix.
    READ TABLE lt_nv INDEX 2 INTO temp172.
    sy-tabix = temp173.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `test_value` act = temp172-v ).

  ENDMETHOD.

  METHOD test_time_add_seconds.

    DATA lv_time TYPE timestampl.
    lv_time = zabaputil_cl_util=>time_get_timestampl( ).
    DATA lv_later TYPE timestampl.
    lv_later = zabaputil_cl_util=>time_add_seconds( time    = lv_time
                                                       seconds = 3600 ).

    cl_abap_unit_assert=>assert_not_initial( lv_later ).

    IF lv_later <= lv_time.
      cl_abap_unit_assert=>fail( `time_add_seconds should return a later timestamp` ).
    ENDIF.

  ENDMETHOD.

  METHOD test_time_diff_seconds.

    DATA lv_time TYPE timestampl.
    lv_time = zabaputil_cl_util=>time_get_timestampl( ).
    DATA lv_later TYPE timestampl.
    lv_later = zabaputil_cl_util=>time_add_seconds( time    = lv_time
                                                       seconds = 120 ).

    DATA lv_diff TYPE i.
    lv_diff = zabaputil_cl_util=>time_diff_seconds( time_from = lv_time
                                                       time_to   = lv_later ).

    cl_abap_unit_assert=>assert_equals( exp = 120
                                        act = lv_diff ).

  ENDMETHOD.

ENDCLASS.

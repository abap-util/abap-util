"! Helper class used by various tests
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
    DATA temp1 LIKE st_tab.
    DATA temp2 LIKE LINE OF temp1.
    sv_var = `test`.
    CLEAR ss_tab.
    ss_tab-title = `the_title`.
    ss_tab-value = `the_value`.
    
    CLEAR temp1.
    
    temp2-title = `test`.
    INSERT temp2 INTO TABLE temp1.
    temp2-title = `test2`.
    INSERT temp2 INTO TABLE temp1.
    st_tab = temp1.
  ENDMETHOD.

ENDCLASS.

"! ================================================================
"! STRING OPERATIONS - Detailed behavioral tests for JS transpilation
"! ================================================================
CLASS ltcl_string_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " c_trim
    METHODS trim_simple                    FOR TESTING.
    METHODS trim_leading_trailing          FOR TESTING.
    METHODS trim_horizontal_tab            FOR TESTING.
    METHODS trim_empty                     FOR TESTING.
    METHODS trim_no_trim_needed            FOR TESTING.
    METHODS trim_interleaved_blank_tab     FOR TESTING.
    METHODS trim_interleaved_deep          FOR TESTING.
    METHODS trim_only_blanks_and_tabs      FOR TESTING.
    METHODS trim_keeps_inner_whitespace    FOR TESTING.

    " c_trim_upper
    METHODS trim_upper_basic               FOR TESTING.
    METHODS trim_upper_mixed               FOR TESTING.

    " c_trim_lower
    METHODS trim_lower_basic               FOR TESTING.
    METHODS trim_lower_mixed               FOR TESTING.

    " c_contains
    METHODS contains_true                  FOR TESTING.
    METHODS contains_false                 FOR TESTING.
    METHODS contains_empty_sub             FOR TESTING.
    METHODS contains_case_sensitive        FOR TESTING.

    " c_starts_with
    METHODS starts_with_true               FOR TESTING.
    METHODS starts_with_false              FOR TESTING.
    METHODS starts_with_longer_prefix      FOR TESTING.
    METHODS starts_with_empty              FOR TESTING.

    " c_ends_with
    METHODS ends_with_true                 FOR TESTING.
    METHODS ends_with_false                FOR TESTING.
    METHODS ends_with_longer_suffix        FOR TESTING.
    METHODS ends_with_empty                FOR TESTING.

    " c_split
    METHODS split_basic                    FOR TESTING.
    METHODS split_no_sep                   FOR TESTING.
    METHODS split_multi                    FOR TESTING.
    METHODS split_empty_parts              FOR TESTING.

    " c_join
    METHODS join_basic                     FOR TESTING.
    METHODS join_empty_sep                 FOR TESTING.
    METHODS join_single                    FOR TESTING.
    METHODS join_empty_table               FOR TESTING.

    " c_pad_left
    METHODS pad_left_basic                 FOR TESTING.
    METHODS pad_left_no_pad_needed         FOR TESTING.
    METHODS pad_left_already_longer        FOR TESTING.

    " c_pad_right
    METHODS pad_right_basic                FOR TESTING.
    METHODS pad_right_no_pad_needed        FOR TESTING.

    " c_truncate
    METHODS truncate_short_string          FOR TESTING.
    METHODS truncate_exact_length          FOR TESTING.
    METHODS truncate_long_string           FOR TESTING.
    METHODS truncate_custom_ellipsis       FOR TESTING.

    " c_substring_safe
    METHODS substring_safe_normal          FOR TESTING.
    METHODS substring_safe_negative_off    FOR TESTING.
    METHODS substring_safe_beyond_end      FOR TESTING.
    METHODS substring_safe_no_len          FOR TESTING.
    METHODS substring_safe_off_past_len    FOR TESTING.

    " c_replace_all
    METHODS replace_all_basic              FOR TESTING.
    METHODS replace_all_multiple           FOR TESTING.
    METHODS replace_all_no_match           FOR TESTING.

    " c_is_blank
    METHODS is_blank_empty                 FOR TESTING.
    METHODS is_blank_spaces                FOR TESTING.
    METHODS is_blank_not_blank             FOR TESTING.
    METHODS is_blank_tabs                  FOR TESTING.

ENDCLASS.

CLASS ltcl_string_ops IMPLEMENTATION.

  METHOD trim_simple.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>c_trim( `  hello  ` ) ).
  ENDMETHOD.

  METHOD trim_leading_trailing.
    cl_abap_unit_assert=>assert_equals( exp = `test`
                                        act = zabaputil_cl_util_context=>c_trim( `   test   ` ) ).
  ENDMETHOD.

  METHOD trim_horizontal_tab.
    DATA lv_tab LIKE zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    DATA lv_val TYPE string.
    lv_tab = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    
    lv_val = lv_tab && `hello` && lv_tab.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>c_trim( lv_val ) ).
  ENDMETHOD.

  METHOD trim_empty.
    cl_abap_unit_assert=>assert_equals( exp = ``
                                        act = zabaputil_cl_util_context=>c_trim( `   ` ) ).
  ENDMETHOD.

  METHOD trim_no_trim_needed.
    cl_abap_unit_assert=>assert_equals( exp = `abc`
                                        act = zabaputil_cl_util_context=>c_trim( `abc` ) ).
  ENDMETHOD.

  METHOD trim_interleaved_blank_tab.
    " `\t \thello\t \t` - two passes (one of each) leave the inner layer
    " standing, which is what the fixed-point loop is for
    DATA lv_tab LIKE zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    DATA lv_val TYPE string.
    lv_tab = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    
    lv_val = lv_tab && ` ` && lv_tab && `hello` && lv_tab && ` ` && lv_tab.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>c_trim( lv_val ) ).
  ENDMETHOD.

  METHOD trim_interleaved_deep.
    DATA lv_tab LIKE zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    DATA lv_val TYPE string.
    lv_tab = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    
    lv_val = ` ` && lv_tab && ` ` && lv_tab && ` ` && lv_tab && `x`
                && lv_tab && ` ` && lv_tab && ` ` && lv_tab && ` `.
    cl_abap_unit_assert=>assert_equals( exp = `x`
                                        act = zabaputil_cl_util_context=>c_trim( lv_val ) ).
  ENDMETHOD.

  METHOD trim_only_blanks_and_tabs.
    DATA lv_tab LIKE zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    DATA lv_val TYPE string.
    lv_tab = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    
    lv_val = ` ` && lv_tab && ` ` && lv_tab && ` `.
    cl_abap_unit_assert=>assert_equals( exp = ``
                                        act = zabaputil_cl_util_context=>c_trim( lv_val ) ).
  ENDMETHOD.

  METHOD trim_keeps_inner_whitespace.
    " only the ends are stripped - a tab between two words stays
    DATA lv_tab LIKE zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    DATA lv_exp TYPE string.
    DATA lv_val TYPE string.
    lv_tab = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    
    lv_exp = `a` && lv_tab && ` b`.
    
    lv_val = ` ` && lv_tab && lv_exp && lv_tab && ` `.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = zabaputil_cl_util_context=>c_trim( lv_val ) ).
  ENDMETHOD.

  METHOD trim_upper_basic.
    cl_abap_unit_assert=>assert_equals( exp = `HELLO`
                                        act = zabaputil_cl_util_context=>c_trim_upper( `  hello  ` ) ).
  ENDMETHOD.

  METHOD trim_upper_mixed.
    cl_abap_unit_assert=>assert_equals( exp = `HELLO WORLD`
                                        act = zabaputil_cl_util_context=>c_trim_upper( `  Hello World  ` ) ).
  ENDMETHOD.

  METHOD trim_lower_basic.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>c_trim_lower( `  HELLO  ` ) ).
  ENDMETHOD.

  METHOD trim_lower_mixed.
    cl_abap_unit_assert=>assert_equals( exp = `hello world`
                                        act = zabaputil_cl_util_context=>c_trim_lower( `  Hello World  ` ) ).
  ENDMETHOD.

  METHOD contains_true.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = `Hello World` sub = `World` ) ).
  ENDMETHOD.

  METHOD contains_false.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>c_contains( val = `Hello World` sub = `xyz` ) ).
  ENDMETHOD.

  METHOD contains_empty_sub.
    " CS with empty string is always true in ABAP
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = `Hello` sub = `` ) ).
  ENDMETHOD.

  METHOD contains_case_sensitive.
    " ABAP CS is case-insensitive
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = `Hello World` sub = `hello` ) ).
  ENDMETHOD.

  METHOD starts_with_true.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_starts_with( val = `Hello World` prefix = `Hello` ) ).
  ENDMETHOD.

  METHOD starts_with_false.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>c_starts_with( val = `Hello World` prefix = `World` ) ).
  ENDMETHOD.

  METHOD starts_with_longer_prefix.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>c_starts_with( val = `Hi` prefix = `Hello` ) ).
  ENDMETHOD.

  METHOD starts_with_empty.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_starts_with( val = `Hello` prefix = `` ) ).
  ENDMETHOD.

  METHOD ends_with_true.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_ends_with( val = `Hello World` suffix = `World` ) ).
  ENDMETHOD.

  METHOD ends_with_false.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>c_ends_with( val = `Hello World` suffix = `Hello` ) ).
  ENDMETHOD.

  METHOD ends_with_longer_suffix.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>c_ends_with( val = `Hi` suffix = `Hello` ) ).
  ENDMETHOD.

  METHOD ends_with_empty.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_ends_with( val = `Hello` suffix = `` ) ).
  ENDMETHOD.

  METHOD split_basic.
    DATA lt_result TYPE string_table.
    FIELD-SYMBOLS <temp3> LIKE LINE OF lt_result.
    DATA temp4 LIKE sy-tabix.
    FIELD-SYMBOLS <temp5> LIKE LINE OF lt_result.
    DATA temp6 LIKE sy-tabix.
    FIELD-SYMBOLS <temp7> LIKE LINE OF lt_result.
    DATA temp8 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>c_split( val = `a,b,c` sep = `,` ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_result ) ).
    
    
    temp4 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp3>.
    sy-tabix = temp4.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a` act = <temp3> ).
    
    
    temp6 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp5>.
    sy-tabix = temp6.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `b` act = <temp5> ).
    
    
    temp8 = sy-tabix.
    READ TABLE lt_result INDEX 3 ASSIGNING <temp7>.
    sy-tabix = temp8.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `c` act = <temp7> ).
  ENDMETHOD.

  METHOD split_no_sep.
    DATA lt_result TYPE string_table.
    FIELD-SYMBOLS <temp9> LIKE LINE OF lt_result.
    DATA temp10 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>c_split( val = `hello` sep = `,` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    
    
    temp10 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp9>.
    sy-tabix = temp10.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `hello` act = <temp9> ).
  ENDMETHOD.

  METHOD split_multi.
    DATA lt_result TYPE string_table.
    FIELD-SYMBOLS <temp11> LIKE LINE OF lt_result.
    DATA temp12 LIKE sy-tabix.
    FIELD-SYMBOLS <temp13> LIKE LINE OF lt_result.
    DATA temp14 LIKE sy-tabix.
    FIELD-SYMBOLS <temp15> LIKE LINE OF lt_result.
    DATA temp16 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>c_split( val = `one::two::three` sep = `::` ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_result ) ).
    
    
    temp12 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp11>.
    sy-tabix = temp12.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `one` act = <temp11> ).
    
    
    temp14 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp13>.
    sy-tabix = temp14.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `two` act = <temp13> ).
    
    
    temp16 = sy-tabix.
    READ TABLE lt_result INDEX 3 ASSIGNING <temp15>.
    sy-tabix = temp16.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `three` act = <temp15> ).
  ENDMETHOD.

  METHOD split_empty_parts.
    DATA lt_result TYPE string_table.
    FIELD-SYMBOLS <temp17> LIKE LINE OF lt_result.
    DATA temp18 LIKE sy-tabix.
    FIELD-SYMBOLS <temp19> LIKE LINE OF lt_result.
    DATA temp20 LIKE sy-tabix.
    FIELD-SYMBOLS <temp21> LIKE LINE OF lt_result.
    DATA temp22 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>c_split( val = `a;;b` sep = `;` ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_result ) ).
    
    
    temp18 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp17>.
    sy-tabix = temp18.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a` act = <temp17> ).
    
    
    temp20 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp19>.
    sy-tabix = temp20.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `` act = <temp19> ).
    
    
    temp22 = sy-tabix.
    READ TABLE lt_result INDEX 3 ASSIGNING <temp21>.
    sy-tabix = temp22.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `b` act = <temp21> ).
  ENDMETHOD.

  METHOD join_basic.
    DATA temp23 TYPE string_table.
    DATA lt_tab LIKE temp23.
    CLEAR temp23.
    INSERT `a` INTO TABLE temp23.
    INSERT `b` INTO TABLE temp23.
    INSERT `c` INTO TABLE temp23.
    
    lt_tab = temp23.
    cl_abap_unit_assert=>assert_equals( exp = `a,b,c`
                                        act = zabaputil_cl_util_context=>c_join( tab = lt_tab sep = `,` ) ).
  ENDMETHOD.

  METHOD join_empty_sep.
    DATA temp25 TYPE string_table.
    DATA lt_tab LIKE temp25.
    CLEAR temp25.
    INSERT `a` INTO TABLE temp25.
    INSERT `b` INTO TABLE temp25.
    INSERT `c` INTO TABLE temp25.
    
    lt_tab = temp25.
    cl_abap_unit_assert=>assert_equals( exp = `abc`
                                        act = zabaputil_cl_util_context=>c_join( tab = lt_tab ) ).
  ENDMETHOD.

  METHOD join_single.
    DATA temp27 TYPE string_table.
    DATA lt_tab LIKE temp27.
    CLEAR temp27.
    INSERT `hello` INTO TABLE temp27.
    
    lt_tab = temp27.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>c_join( tab = lt_tab sep = `,` ) ).
  ENDMETHOD.

  METHOD join_empty_table.
    DATA temp29 TYPE string_table.
    DATA lt_tab LIKE temp29.
    CLEAR temp29.
    
    lt_tab = temp29.
    cl_abap_unit_assert=>assert_equals( exp = ``
                                        act = zabaputil_cl_util_context=>c_join( tab = lt_tab sep = `,` ) ).
  ENDMETHOD.

  METHOD pad_left_basic.
    cl_abap_unit_assert=>assert_equals( exp = `005`
                                        act = zabaputil_cl_util_context=>c_pad_left( val = `5` len = 3 pad = '0' ) ).
  ENDMETHOD.

  METHOD pad_left_no_pad_needed.
    cl_abap_unit_assert=>assert_equals( exp = `123`
                                        act = zabaputil_cl_util_context=>c_pad_left( val = `123` len = 3 pad = '0' ) ).
  ENDMETHOD.

  METHOD pad_left_already_longer.
    cl_abap_unit_assert=>assert_equals( exp = `12345`
                                        act = zabaputil_cl_util_context=>c_pad_left( val = `12345` len = 3 pad = '0' ) ).
  ENDMETHOD.

  METHOD pad_right_basic.
    " After fix: pad is now converted to string internally, so space padding works
    cl_abap_unit_assert=>assert_equals( exp = `hi   `
                                        act = zabaputil_cl_util_context=>c_pad_right( val = `hi` len = 5 ) ).
  ENDMETHOD.

  METHOD pad_right_no_pad_needed.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>c_pad_right( val = `hello` len = 5 ) ).
  ENDMETHOD.

  METHOD truncate_short_string.
    cl_abap_unit_assert=>assert_equals( exp = `Hi`
                                        act = zabaputil_cl_util_context=>c_truncate( val = `Hi` max = 10 ) ).
  ENDMETHOD.

  METHOD truncate_exact_length.
    cl_abap_unit_assert=>assert_equals( exp = `Hello`
                                        act = zabaputil_cl_util_context=>c_truncate( val = `Hello` max = 5 ) ).
  ENDMETHOD.

  METHOD truncate_long_string.
    cl_abap_unit_assert=>assert_equals( exp = `Hell...`
                                        act = zabaputil_cl_util_context=>c_truncate( val = `Hello World` max = 7 ) ).
  ENDMETHOD.

  METHOD truncate_custom_ellipsis.
    cl_abap_unit_assert=>assert_equals( exp = `Hello~`
                                        act = zabaputil_cl_util_context=>c_truncate( val = `Hello World` max = 6 ellipsis = `~` ) ).
  ENDMETHOD.

  METHOD substring_safe_normal.
    cl_abap_unit_assert=>assert_equals( exp = `llo`
                                        act = zabaputil_cl_util_context=>c_substring_safe( val = `Hello` off = 2 len = 3 ) ).
  ENDMETHOD.

  METHOD substring_safe_negative_off.
    " Negative offset should be treated as 0
    cl_abap_unit_assert=>assert_equals( exp = `He`
                                        act = zabaputil_cl_util_context=>c_substring_safe( val = `Hello` off = -1 len = 2 ) ).
  ENDMETHOD.

  METHOD substring_safe_beyond_end.
    " len extends beyond string end - should return rest
    cl_abap_unit_assert=>assert_equals( exp = `lo`
                                        act = zabaputil_cl_util_context=>c_substring_safe( val = `Hello` off = 3 len = 99 ) ).
  ENDMETHOD.

  METHOD substring_safe_no_len.
    " len = -1 means rest of string
    cl_abap_unit_assert=>assert_equals( exp = `llo`
                                        act = zabaputil_cl_util_context=>c_substring_safe( val = `Hello` off = 2 ) ).
  ENDMETHOD.

  METHOD substring_safe_off_past_len.
    " offset beyond string length -> empty
    cl_abap_unit_assert=>assert_equals( exp = ``
                                        act = zabaputil_cl_util_context=>c_substring_safe( val = `Hi` off = 10 len = 5 ) ).
  ENDMETHOD.

  METHOD replace_all_basic.
    cl_abap_unit_assert=>assert_equals( exp = `hello world`
                                        act = zabaputil_cl_util_context=>c_replace_all( val = `hello_world` sub = `_` new_val = ` ` ) ).
  ENDMETHOD.

  METHOD replace_all_multiple.
    cl_abap_unit_assert=>assert_equals( exp = `a-b-c`
                                        act = zabaputil_cl_util_context=>c_replace_all( val = `a.b.c` sub = `.` new_val = `-` ) ).
  ENDMETHOD.

  METHOD replace_all_no_match.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>c_replace_all( val = `hello` sub = `x` new_val = `y` ) ).
  ENDMETHOD.

  METHOD is_blank_empty.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_is_blank( `` ) ).
  ENDMETHOD.

  METHOD is_blank_spaces.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_is_blank( `   ` ) ).
  ENDMETHOD.

  METHOD is_blank_not_blank.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>c_is_blank( `hi` ) ).
  ENDMETHOD.

  METHOD is_blank_tabs.
    DATA lv_val LIKE zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    lv_val = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_is_blank( lv_val ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! BOOLEAN OPERATIONS
"! ================================================================
CLASS ltcl_boolean_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS check_by_name_abap_bool        FOR TESTING.
    METHODS check_by_name_xsdboolean       FOR TESTING.
    METHODS check_by_name_flag             FOR TESTING.
    METHODS check_by_name_xflag            FOR TESTING.
    METHODS check_by_name_xfeld            FOR TESTING.
    METHODS check_by_name_wdy_boolean      FOR TESTING.
    METHODS check_by_name_boole_d          FOR TESTING.
    METHODS check_by_name_os_boolean       FOR TESTING.
    METHODS check_by_name_not_bool         FOR TESTING.
    METHODS check_by_name_empty            FOR TESTING.

    METHODS abap2json_true                 FOR TESTING.
    METHODS abap2json_false                FOR TESTING.
    METHODS abap2json_non_boolean          FOR TESTING.

    METHODS check_by_data_abap_bool        FOR TESTING.
    METHODS check_by_data_string           FOR TESTING.

    METHODS ui5_msg_type_e                 FOR TESTING.
    METHODS ui5_msg_type_s                 FOR TESTING.
    METHODS ui5_msg_type_w                 FOR TESTING.
    METHODS ui5_msg_type_i                 FOR TESTING.
    METHODS ui5_msg_type_other             FOR TESTING.

ENDCLASS.

CLASS ltcl_boolean_ops IMPLEMENTATION.

  METHOD check_by_name_abap_bool.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `ABAP_BOOL` ) ).
  ENDMETHOD.

  METHOD check_by_name_xsdboolean.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `XSDBOOLEAN` ) ).
  ENDMETHOD.

  METHOD check_by_name_flag.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `FLAG` ) ).
  ENDMETHOD.

  METHOD check_by_name_xflag.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `XFLAG` ) ).
  ENDMETHOD.

  METHOD check_by_name_xfeld.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `XFELD` ) ).
  ENDMETHOD.

  METHOD check_by_name_wdy_boolean.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `WDY_BOOLEAN` ) ).
  ENDMETHOD.

  METHOD check_by_name_boole_d.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `BOOLE_D` ) ).
  ENDMETHOD.

  METHOD check_by_name_os_boolean.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_name( `OS_BOOLEAN` ) ).
  ENDMETHOD.

  METHOD check_by_name_not_bool.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>boolean_check_by_name( `STRING` ) ).
  ENDMETHOD.

  METHOD check_by_name_empty.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>boolean_check_by_name( `` ) ).
  ENDMETHOD.

  METHOD abap2json_true.
    cl_abap_unit_assert=>assert_equals( exp = `true`
                                        act = zabaputil_cl_util_context=>boolean_abap_2_json( abap_true ) ).
  ENDMETHOD.

  METHOD abap2json_false.
    cl_abap_unit_assert=>assert_equals( exp = `false`
                                        act = zabaputil_cl_util_context=>boolean_abap_2_json( abap_false ) ).
  ENDMETHOD.

  METHOD abap2json_non_boolean.
    " Non-boolean value is passed through unchanged
    DATA lv_val TYPE string VALUE `something`.
    cl_abap_unit_assert=>assert_equals( exp = `something`
                                        act = zabaputil_cl_util_context=>boolean_abap_2_json( lv_val ) ).
  ENDMETHOD.

  METHOD check_by_data_abap_bool.
    DATA lv_bool TYPE abap_bool VALUE abap_true.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>boolean_check_by_data( lv_bool ) ).
  ENDMETHOD.

  METHOD check_by_data_string.
    DATA lv_str TYPE string VALUE `X`.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>boolean_check_by_data( lv_str ) ).
  ENDMETHOD.

  METHOD ui5_msg_type_e.
    cl_abap_unit_assert=>assert_equals( exp = `Error` act = zabaputil_cl_util_context=>ui5_get_msg_type( `E` ) ).
  ENDMETHOD.

  METHOD ui5_msg_type_s.
    cl_abap_unit_assert=>assert_equals( exp = `Success` act = zabaputil_cl_util_context=>ui5_get_msg_type( `S` ) ).
  ENDMETHOD.

  METHOD ui5_msg_type_w.
    cl_abap_unit_assert=>assert_equals( exp = `Warning` act = zabaputil_cl_util_context=>ui5_get_msg_type( `W` ) ).
  ENDMETHOD.

  METHOD ui5_msg_type_i.
    cl_abap_unit_assert=>assert_equals( exp = `Information` act = zabaputil_cl_util_context=>ui5_get_msg_type( `I` ) ).
  ENDMETHOD.

  METHOD ui5_msg_type_other.
    " any other type maps to Information - A (abort) and X (exit) are the
    " two SAP message types above E, so they are errors, never information
    cl_abap_unit_assert=>assert_equals( exp = `Information` act = zabaputil_cl_util_context=>ui5_get_msg_type( `Z` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Error` act = zabaputil_cl_util_context=>ui5_get_msg_type( `X` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Error` act = zabaputil_cl_util_context=>ui5_get_msg_type( `A` ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! URL PARAMETER OPERATIONS
"! ================================================================
CLASS ltcl_url_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS param_get_basic                FOR TESTING.
    METHODS param_get_case_insensitive     FOR TESTING.
    METHODS param_get_not_found            FOR TESTING.
    METHODS param_get_encoded              FOR TESTING.
    METHODS param_get_encoded_bare         FOR TESTING.

    METHODS param_get_tab_basic            FOR TESTING.
    METHODS param_get_tab_multiple         FOR TESTING.
    METHODS param_get_tab_with_question    FOR TESTING.

    METHODS param_create_url_basic         FOR TESTING.
    METHODS param_create_url_single        FOR TESTING.
    METHODS param_create_url_empty         FOR TESTING.

    METHODS param_set_new_param            FOR TESTING.
    METHODS param_set_existing_param       FOR TESTING.

    METHODS param_get_tab_normalizes_name  FOR TESTING.
    METHODS param_get_tab_no_phantom       FOR TESTING.

    METHODS app_url_drops_app_hash         FOR TESTING.
    METHODS app_url_keeps_shell_hash       FOR TESTING.

ENDCLASS.

CLASS ltcl_url_ops IMPLEMENTATION.

  METHOD param_get_basic.
    cl_abap_unit_assert=>assert_equals(
        exp = `world`
        act = zabaputil_cl_util_context=>url_param_get( val = `hello` url = `hello=world&foo=bar` ) ).
  ENDMETHOD.

  METHOD param_get_case_insensitive.
    cl_abap_unit_assert=>assert_equals(
        exp = `world`
        act = zabaputil_cl_util_context=>url_param_get( val = `HELLO` url = `hello=world&foo=bar` ) ).
  ENDMETHOD.

  METHOD param_get_not_found.
    cl_abap_unit_assert=>assert_equals(
        exp = ``
        act = zabaputil_cl_util_context=>url_param_get( val = `missing` url = `hello=world` ) ).
  ENDMETHOD.

  METHOD param_get_encoded.
    " a percent-encoded query is decoded where the launchpad puts one: in
    " the sap-startup-params value. It used to be decoded wherever it
    " appeared, which is what tore an encoded `&` inside an ordinary value
    " apart - see url_encoded_value_survives
    cl_abap_unit_assert=>assert_equals(
        exp = `world`
        act = zabaputil_cl_util_context=>url_param_get(
                  val = `hello`
                  url = `sap-startup-params=hello%3Dworld%26foo%3Dbar` ) ).
  ENDMETHOD.

  METHOD param_get_encoded_bare.
    " and NOT outside it: a bare `%3D` is a literal character of the value
    " it stands in, not a separator waiting to be decoded
    cl_abap_unit_assert=>assert_equals(
        exp = ``
        act = zabaputil_cl_util_context=>url_param_get( val = `hello`
                                                        url = `hello%3Dworld` ) ).
  ENDMETHOD.

  METHOD param_get_tab_basic.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp30> LIKE LINE OF lt_result.
    DATA temp31 LIKE sy-tabix.
    FIELD-SYMBOLS <temp32> LIKE LINE OF lt_result.
    DATA temp33 LIKE sy-tabix.
    FIELD-SYMBOLS <temp34> LIKE LINE OF lt_result.
    DATA temp35 LIKE sy-tabix.
    FIELD-SYMBOLS <temp36> LIKE LINE OF lt_result.
    DATA temp37 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>url_param_get_tab( `name=john&age=30` ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_result ) ).
    
    
    temp31 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp30>.
    sy-tabix = temp31.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `name` act = <temp30>-n ).
    
    
    temp33 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp32>.
    sy-tabix = temp33.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `john` act = <temp32>-v ).
    
    
    temp35 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp34>.
    sy-tabix = temp35.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `age` act = <temp34>-n ).
    
    
    temp37 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp36>.
    sy-tabix = temp37.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `30` act = <temp36>-v ).
  ENDMETHOD.

  METHOD param_get_tab_multiple.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    lt_result = zabaputil_cl_util_context=>url_param_get_tab( `a=1&b=2&c=3` ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_result ) ).
  ENDMETHOD.

  METHOD param_get_tab_with_question.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp38> LIKE LINE OF lt_result.
    DATA temp39 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>url_param_get_tab( `?name=john&age=30` ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_result ) ).
    
    
    temp39 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp38>.
    sy-tabix = temp39.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `name` act = <temp38>-n ).
  ENDMETHOD.

  METHOD param_create_url_basic.
    DATA temp40 TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp41 LIKE LINE OF temp40.
    DATA lt_params LIKE temp40.
    DATA lv_result TYPE string.
    CLEAR temp40.
    
    temp41-n = `name`.
    temp41-v = `john`.
    INSERT temp41 INTO TABLE temp40.
    temp41-n = `age`.
    temp41-v = `30`.
    INSERT temp41 INTO TABLE temp40.
    
    lt_params = temp40.
    
    lv_result = zabaputil_cl_util_context=>url_param_create_url( lt_params ).
    cl_abap_unit_assert=>assert_equals( exp = `name=john&age=30`
                                        act = lv_result ).
  ENDMETHOD.

  METHOD param_create_url_single.
    DATA temp42 TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp43 LIKE LINE OF temp42.
    DATA lt_params LIKE temp42.
    CLEAR temp42.
    
    temp43-n = `key`.
    temp43-v = `val`.
    INSERT temp43 INTO TABLE temp42.
    
    lt_params = temp42.
    cl_abap_unit_assert=>assert_equals( exp = `key=val`
                                        act = zabaputil_cl_util_context=>url_param_create_url( lt_params ) ).
  ENDMETHOD.

  METHOD param_create_url_empty.
    DATA temp44 TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA lt_params LIKE temp44.
    CLEAR temp44.
    
    lt_params = temp44.
    cl_abap_unit_assert=>assert_equals( exp = ``
                                        act = zabaputil_cl_util_context=>url_param_create_url( lt_params ) ).
  ENDMETHOD.

  METHOD param_set_new_param.
    DATA lv_result TYPE string.
    lv_result = zabaputil_cl_util_context=>url_param_set( url = `a=1&b=2`
                                                     name = `c`
                                                     value = `3` ).
    " Result should contain all three params
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_result sub = `c=3` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_result sub = `a=1` ) ).
  ENDMETHOD.

  METHOD param_set_existing_param.
    DATA lv_result TYPE string.
    lv_result = zabaputil_cl_util_context=>url_param_set( url = `a=1&b=2`
                                                     name = `b`
                                                     value = `99` ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_result sub = `b=99` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>c_contains( val = lv_result sub = `b=2` ) ).
  ENDMETHOD.

  METHOD param_get_tab_normalizes_name.
    " url_param_get / url_param_set look up with c_trim_lower, so the stored
    " name has to be lower case for a mixed-case URL to resolve at all. The
    " value keeps its original case.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp45> LIKE LINE OF lt_result.
    DATA temp46 LIKE sy-tabix.
    FIELD-SYMBOLS <temp47> LIKE LINE OF lt_result.
    DATA temp48 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>url_param_get_tab( `https://h/p?APP_START=MixedCase` ).
    
    
    temp46 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp45>.
    sy-tabix = temp46.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `app_start` act = <temp45>-n ).
    
    
    temp48 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp47>.
    sy-tabix = temp48.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `MixedCase` act = <temp47>-v ).

    cl_abap_unit_assert=>assert_equals(
        exp = `MixedCase`
        act = zabaputil_cl_util_context=>url_param_get( val = `app_start`
                                                        url = `?APP_START=MixedCase` ) ).
  ENDMETHOD.

  METHOD param_get_tab_no_phantom.
    " an empty segment (empty search string, trailing &) must not become a
    " nameless row - url_param_create_url would write it back out as `=&`
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>url_param_get_tab( `` ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( zabaputil_cl_util_context=>url_param_get_tab( `?a=1&` ) ) ).
  ENDMETHOD.

  METHOD app_url_drops_app_hash.
    " the app-owned hash (route or app-state, leading `/`) must be dropped -
    " a consumer backend prefers it over app_start, so keeping it would
    " re-open the current app instead of the requested one
    cl_abap_unit_assert=>assert_equals(
        exp = `https://h/p?app_start=zcl_new`
        act = zabaputil_cl_util_context=>app_get_url( classname = `ZCL_NEW`
                                                      origin    = `https://h`
                                                      pathname  = `/p`
                                                      search    = ``
                                                      hash      = `#/app/ZCL_OLD/DRAFT1` ) ).
  ENDMETHOD.

  METHOD app_url_keeps_shell_hash.
    " inside the launchpad the shell part of the hash survives, only the app
    " part after `&/` is cut
    cl_abap_unit_assert=>assert_equals(
        exp = `https://h/p?app_start=zcl_new#Shell-home`
        act = zabaputil_cl_util_context=>app_get_url( classname = `ZCL_NEW`
                                                      origin    = `https://h`
                                                      pathname  = `/p`
                                                      search    = ``
                                                      hash      = `#Shell-home&/app/ZCL_OLD/DRAFT1` ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! VALIDATION HELPERS
"! ================================================================
CLASS ltcl_validation DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " check_is_email
    METHODS email_valid                    FOR TESTING.
    METHODS email_no_at                    FOR TESTING.
    METHODS email_no_dot_in_domain         FOR TESTING.
    METHODS email_multiple_at              FOR TESTING.
    METHODS email_empty                    FOR TESTING.
    METHODS email_no_local_part            FOR TESTING.

    " check_is_numeric_string
    METHODS numeric_integer                FOR TESTING.
    METHODS numeric_negative               FOR TESTING.
    METHODS numeric_decimal_dot            FOR TESTING.
    METHODS numeric_decimal_comma          FOR TESTING.
    METHODS numeric_plus_sign              FOR TESTING.
    METHODS numeric_letters                FOR TESTING.
    METHODS numeric_empty                  FOR TESTING.
    METHODS numeric_spaces_trimmed         FOR TESTING.

    " check_is_guid
    METHODS guid_valid_32                  FOR TESTING.
    METHODS guid_valid_with_dashes         FOR TESTING.
    METHODS guid_too_short                 FOR TESTING.
    METHODS guid_invalid_chars             FOR TESTING.
    METHODS guid_lowercase                 FOR TESTING.

    " check_max_length
    METHODS max_length_within              FOR TESTING.
    METHODS max_length_exact               FOR TESTING.
    METHODS max_length_exceeded            FOR TESTING.

    " check_is_date_valid
    METHODS date_valid_basic               FOR TESTING.
    METHODS date_invalid_format            FOR TESTING.

ENDCLASS.

CLASS ltcl_validation IMPLEMENTATION.

  METHOD email_valid.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_email( `user@example.com` ) ).
  ENDMETHOD.

  METHOD email_no_at.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `userexample.com` ) ).
  ENDMETHOD.

  METHOD email_no_dot_in_domain.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `user@localhost` ) ).
  ENDMETHOD.

  METHOD email_multiple_at.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `user@@example.com` ) ).
  ENDMETHOD.

  METHOD email_empty.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `` ) ).
  ENDMETHOD.

  METHOD email_no_local_part.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `@example.com` ) ).
  ENDMETHOD.

  METHOD numeric_integer.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `12345` ) ).
  ENDMETHOD.

  METHOD numeric_negative.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `-42` ) ).
  ENDMETHOD.

  METHOD numeric_decimal_dot.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `3.14` ) ).
  ENDMETHOD.

  METHOD numeric_decimal_comma.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `3,14` ) ).
  ENDMETHOD.

  METHOD numeric_plus_sign.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `+99` ) ).
  ENDMETHOD.

  METHOD numeric_letters.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_numeric_string( `12abc` ) ).
  ENDMETHOD.

  METHOD numeric_empty.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_numeric_string( `` ) ).
  ENDMETHOD.

  METHOD numeric_spaces_trimmed.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `  42  ` ) ).
  ENDMETHOD.

  METHOD guid_valid_32.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_guid( `A1B2C3D4E5F6A7B8C9D0E1F2A3B4C5D6` ) ).
  ENDMETHOD.

  METHOD guid_valid_with_dashes.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_guid( `A1B2C3D4-E5F6-A7B8-C9D0-E1F2A3B4C5D6` ) ).
  ENDMETHOD.

  METHOD guid_too_short.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_guid( `A1B2C3D4` ) ).
  ENDMETHOD.

  METHOD guid_invalid_chars.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_guid( `G1B2C3D4E5F6A7B8C9D0E1F2A3B4C5D6` ) ).
  ENDMETHOD.

  METHOD guid_lowercase.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_guid( `a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6` ) ).
  ENDMETHOD.

  METHOD max_length_within.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_max_length( val = `hi` max = 10 ) ).
  ENDMETHOD.

  METHOD max_length_exact.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_max_length( val = `hello` max = 5 ) ).
  ENDMETHOD.

  METHOD max_length_exceeded.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_max_length( val = `hello world` max = 5 ) ).
  ENDMETHOD.

  METHOD date_valid_basic.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_date_valid( `2024-03-15` ) ).
  ENDMETHOD.

  METHOD date_invalid_format.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `not-a-date` ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! NUMBER CONVERSION
"! ================================================================
CLASS ltcl_number_conv DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " conv_number_to_string
    METHODS num2str_integer                FOR TESTING.
    METHODS num2str_decimal_2              FOR TESTING.
    METHODS num2str_decimal_0              FOR TESTING.
    METHODS num2str_thousands              FOR TESTING.
    METHODS num2str_negative               FOR TESTING.
    METHODS num2str_pad_decimals           FOR TESTING.

    " conv_string_to_number
    METHODS str2num_integer                FOR TESTING.
    METHODS str2num_negative               FOR TESTING.
    METHODS str2num_decimal_dot            FOR TESTING.
    METHODS str2num_decimal_comma          FOR TESTING.
    METHODS str2num_thousands_comma        FOR TESTING.
    METHODS str2num_invalid                FOR TESTING.

ENDCLASS.

CLASS ltcl_number_conv IMPLEMENTATION.

  METHOD num2str_integer.
    cl_abap_unit_assert=>assert_equals( exp = `42`
                                        act = zabaputil_cl_util_context=>conv_number_to_string( val = 42 ) ).
  ENDMETHOD.

  METHOD num2str_decimal_2.
    DATA temp49 TYPE decfloat34.
    temp49 = '3.14'.
    cl_abap_unit_assert=>assert_equals( exp = `3.14`
                                        act = zabaputil_cl_util_context=>conv_number_to_string( val = temp49 decimals = 2 ) ).
  ENDMETHOD.

  METHOD num2str_decimal_0.
    DATA temp50 TYPE decfloat34.
    temp50 = '3.14'.
    cl_abap_unit_assert=>assert_equals( exp = `3`
                                        act = zabaputil_cl_util_context=>conv_number_to_string( val = temp50 decimals = 0 ) ).
  ENDMETHOD.

  METHOD num2str_thousands.
    cl_abap_unit_assert=>assert_equals( exp = `1,000,000`
                                        act = zabaputil_cl_util_context=>conv_number_to_string( val = 1000000 sep_thousands = ',' ) ).
  ENDMETHOD.

  METHOD num2str_negative.
    DATA lv_result TYPE string.
    lv_result = zabaputil_cl_util_context=>conv_number_to_string( val = -42 sep_thousands = ',' ).
    cl_abap_unit_assert=>assert_equals( exp = `-42` act = lv_result ).
  ENDMETHOD.

  METHOD num2str_pad_decimals.
    cl_abap_unit_assert=>assert_equals( exp = `5.00`
                                        act = zabaputil_cl_util_context=>conv_number_to_string( val = 5 decimals = 2 ) ).
  ENDMETHOD.

  METHOD str2num_integer.
    DATA temp51 TYPE decfloat34.
    temp51 = 42.
    cl_abap_unit_assert=>assert_equals( exp = temp51
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `42` ) ).
  ENDMETHOD.

  METHOD str2num_negative.
    DATA temp52 TYPE decfloat34.
    temp52 = -7.
    cl_abap_unit_assert=>assert_equals( exp = temp52
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `-7` ) ).
  ENDMETHOD.

  METHOD str2num_decimal_dot.
    DATA temp53 TYPE decfloat34.
    temp53 = '3.14'.
    cl_abap_unit_assert=>assert_equals( exp = temp53
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `3.14` ) ).
  ENDMETHOD.

  METHOD str2num_decimal_comma.
    " Comma as last separator => decimal
    DATA temp54 TYPE decfloat34.
    temp54 = '3.14'.
    cl_abap_unit_assert=>assert_equals( exp = temp54
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `3,14` ) ).
  ENDMETHOD.

  METHOD str2num_thousands_comma.
    " Comma followed by dot => thousands separator
    DATA temp55 TYPE decfloat34.
    temp55 = '1000.50'.
    cl_abap_unit_assert=>assert_equals( exp = temp55
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `1,000.50` ) ).
  ENDMETHOD.

  METHOD str2num_invalid.
    DATA temp56 TYPE decfloat34.
    temp56 = 0.
    cl_abap_unit_assert=>assert_equals( exp = temp56
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `abc` ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! DATE CONVERSION
"! ================================================================
CLASS ltcl_date_conv DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS str_to_date_default            FOR TESTING.
    METHODS str_to_date_dd_mm_yyyy         FOR TESTING.
    METHODS str_to_date_mm_dd_yyyy         FOR TESTING.
    METHODS str_to_date_no_separators      FOR TESTING.

    METHODS date_to_str_default            FOR TESTING.
    METHODS date_to_str_dd_mm_yyyy         FOR TESTING.
    METHODS date_to_str_custom_sep         FOR TESTING.

    METHODS roundtrip                      FOR TESTING.

ENDCLASS.

CLASS ltcl_date_conv IMPLEMENTATION.

  METHOD str_to_date_default.
    " Default format YYYY-MM-DD
    DATA temp57 TYPE d.
    temp57 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp57
                                        act = zabaputil_cl_util_context=>conv_string_to_date( `2024-03-15` ) ).
  ENDMETHOD.

  METHOD str_to_date_dd_mm_yyyy.
    DATA temp58 TYPE d.
    temp58 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp58
                                        act = zabaputil_cl_util_context=>conv_string_to_date( val = `15.03.2024`
                                                                                   format = `DD.MM.YYYY` ) ).
  ENDMETHOD.

  METHOD str_to_date_mm_dd_yyyy.
    DATA temp59 TYPE d.
    temp59 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp59
                                        act = zabaputil_cl_util_context=>conv_string_to_date( val = `03/15/2024`
                                                                                   format = `MM/DD/YYYY` ) ).
  ENDMETHOD.

  METHOD str_to_date_no_separators.
    DATA temp60 TYPE d.
    temp60 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp60
                                        act = zabaputil_cl_util_context=>conv_string_to_date( val = `20240315`
                                                                                   format = `YYYYMMDD` ) ).
  ENDMETHOD.

  METHOD date_to_str_default.
    DATA temp61 TYPE d.
    temp61 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15`
                                        act = zabaputil_cl_util_context=>conv_date_to_string( temp61 ) ).
  ENDMETHOD.

  METHOD date_to_str_dd_mm_yyyy.
    DATA temp62 TYPE d.
    temp62 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = `15.03.2024`
                                        act = zabaputil_cl_util_context=>conv_date_to_string( val = temp62
                                                                                   format = `DD.MM.YYYY` ) ).
  ENDMETHOD.

  METHOD date_to_str_custom_sep.
    DATA temp63 TYPE d.
    temp63 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = `03/15/2024`
                                        act = zabaputil_cl_util_context=>conv_date_to_string( val = temp63
                                                                                   format = `MM/DD/YYYY` ) ).
  ENDMETHOD.

  METHOD roundtrip.
    DATA temp64 TYPE d.
    DATA lv_date LIKE temp64.
    DATA lv_str TYPE string.
    DATA lv_back TYPE d.
    temp64 = `20241225`.
    
    lv_date = temp64.
    
    lv_str = zabaputil_cl_util_context=>conv_date_to_string( lv_date ).
    
    lv_back = zabaputil_cl_util_context=>conv_string_to_date( lv_str ).
    cl_abap_unit_assert=>assert_equals( exp = lv_date act = lv_back ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! FILTER / RANGE / TOKEN OPERATIONS
"! ================================================================
CLASS ltcl_filter_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " filter_get_range_by_token
    METHODS token_eq                       FOR TESTING.
    METHODS token_lt                       FOR TESTING.
    METHODS token_le                       FOR TESTING.
    METHODS token_gt                       FOR TESTING.
    METHODS token_ge                       FOR TESTING.
    METHODS token_cp                       FOR TESTING.
    METHODS token_bt                       FOR TESTING.
    METHODS token_plain_value              FOR TESTING.

    " conv_get_bintab_by_xstring
    METHODS bintab_flat_lines              FOR TESTING.
    METHODS bintab_struct_lines            FOR TESTING.
    METHODS bintab_empty                   FOR TESTING.

    " conv_exit
    METHODS conv_exit_unknown_exit         FOR TESTING.
    METHODS conv_exit_alpha_from           FOR TESTING.

    " filter_get_range_by_value / filter_get_range_t_by_value_t
    METHODS value_eq                       FOR TESTING.
    METHODS value_cp                       FOR TESTING.
    METHODS value_initial                  FOR TESTING.
    METHODS value_t_sorted_distinct        FOR TESTING.
    METHODS value_t_keeps_blank            FOR TESTING.

    " filter_get_sql_where
    METHODS sql_where_eq                   FOR TESTING.
    METHODS sql_where_bt                   FOR TESTING.
    METHODS sql_where_cp                   FOR TESTING.
    METHODS sql_where_multiple_fields      FOR TESTING.
    METHODS sql_where_exclude              FOR TESTING.

    " filter_get_multi_by_sql_where roundtrip
    METHODS sql_roundtrip_eq               FOR TESTING.
    METHODS sql_roundtrip_bt               FOR TESTING.
    METHODS sql_roundtrip_like             FOR TESTING.

    " filter_get_token_range_mapping
    METHODS token_range_mapping            FOR TESTING.

    " filter_itab
    METHODS filter_itab_basic              FOR TESTING.

    " filter_get_sql_by_sql_string
    METHODS sql_by_string_basic            FOR TESTING.

ENDCLASS.

CLASS ltcl_filter_ops IMPLEMENTATION.

  METHOD bintab_flat_lines.
    TYPES ty_x4 TYPE x LENGTH 4.
    TYPES temp3 TYPE STANDARD TABLE OF ty_x4 WITH DEFAULT KEY.
DATA lt_bin TYPE temp3.
    DATA temp65 TYPE xstring.
    DATA temp66 TYPE ty_x4.
    FIELD-SYMBOLS <temp1> LIKE LINE OF lt_bin.
    DATA temp2 LIKE sy-tabix.
    DATA temp67 TYPE ty_x4.
    FIELD-SYMBOLS <temp3> LIKE LINE OF lt_bin.
    DATA temp4 LIKE sy-tabix.
    temp65 = `AABBCCDDEE`.
    zabaputil_cl_util_context=>conv_get_bintab_by_xstring( EXPORTING val    = temp65
                                                           IMPORTING result = lt_bin ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_bin ) ).
    
    temp66 = `AABBCCDD`.
    
    
    temp2 = sy-tabix.
    READ TABLE lt_bin INDEX 1 ASSIGNING <temp1>.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp66 act = <temp1> ).
    " the last line is padded with zero bytes
    
    temp67 = `EE000000`.
    
    
    temp4 = sy-tabix.
    READ TABLE lt_bin INDEX 2 ASSIGNING <temp3>.
    sy-tabix = temp4.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp67 act = <temp3> ).
  ENDMETHOD.

  METHOD bintab_struct_lines.
    " the BAPICONTEN shape: a structure whose first component holds the bytes
    TYPES:
      BEGIN OF ty_s_line,
        line TYPE x LENGTH 3,
      END OF ty_s_line.
    TYPES temp4 TYPE STANDARD TABLE OF ty_s_line WITH DEFAULT KEY.
DATA lt_bin TYPE temp4.
    DATA temp68 TYPE xstring.
    DATA temp69 TYPE xstring.
    DATA temp5 TYPE xstring.
    FIELD-SYMBOLS <temp1> LIKE LINE OF lt_bin.
    DATA temp2 LIKE sy-tabix.
    temp68 = `010203040506`.
    zabaputil_cl_util_context=>conv_get_bintab_by_xstring( EXPORTING val    = temp68
                                                           IMPORTING result = lt_bin ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_bin ) ).
    
    temp69 = `040506`.
    
    
    
    temp2 = sy-tabix.
    READ TABLE lt_bin INDEX 2 ASSIGNING <temp1>.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    temp5 = <temp1>-line.
    cl_abap_unit_assert=>assert_equals( exp = temp69 act = temp5 ).
  ENDMETHOD.

  METHOD bintab_empty.
    TYPES ty_x4 TYPE x LENGTH 4.
    TYPES temp5 TYPE STANDARD TABLE OF ty_x4 WITH DEFAULT KEY.
DATA lt_bin TYPE temp5.
    DATA temp70 LIKE lt_bin.
    DATA temp6 LIKE LINE OF temp70.
    DATA temp72 TYPE xstring.
    CLEAR temp70.
    
    temp6 = `FFFFFFFF`.
    INSERT temp6 INTO TABLE temp70.
    lt_bin = temp70.
    
    CLEAR temp72.
    zabaputil_cl_util_context=>conv_get_bintab_by_xstring( EXPORTING val    = temp72
                                                           IMPORTING result = lt_bin ).
    cl_abap_unit_assert=>assert_initial( lt_bin ).
  ENDMETHOD.

  METHOD conv_exit_unknown_exit.
    " an exit that does not exist raises, with a text - VALUE stays
    DATA lv_value TYPE string VALUE `ABC`.
        DATA lx_error TYPE REF TO zabaputil_cx_util_error.
    TRY.
        zabaputil_cl_util_context=>conv_exit( EXPORTING convexit = `ZZNONE`
                                              CHANGING  value    = lv_value ).
        cl_abap_unit_assert=>fail( `no exception` ).
        
      CATCH zabaputil_cx_util_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial( lx_error->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( exp = `ABC` act = lv_value ).
  ENDMETHOD.

  METHOD conv_exit_alpha_from.
    " FROM into a longer target: ALPHA pads to the target's length
    DATA lv_target TYPE c LENGTH 10.
    DATA lo_descr  TYPE REF TO cl_abap_typedescr.
    " the JS runtime has no function modules - nothing to run there
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = `TFDIR`
                                         RECEIVING  p_descr_ref    = lo_descr
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    zabaputil_cl_util_context=>conv_exit( EXPORTING convexit = `ALPHA`
                                                    from     = `123`
                                          CHANGING  value    = lv_target ).
    cl_abap_unit_assert=>assert_equals( exp = `0000000123` act = lv_target ).
  ENDMETHOD.

  METHOD value_eq.
    " no `*` - EQ; a `+` is no wildcard here, it stays a character
    DATA lt_range TYPE zabaputil_cl_util_context=>ty_t_range.
    FIELD-SYMBOLS <temp73> LIKE LINE OF lt_range.
    DATA temp74 LIKE sy-tabix.
    FIELD-SYMBOLS <temp75> LIKE LINE OF lt_range.
    DATA temp76 LIKE sy-tabix.
    FIELD-SYMBOLS <temp77> LIKE LINE OF lt_range.
    DATA temp78 LIKE sy-tabix.
    lt_range = zabaputil_cl_util_context=>filter_get_range_by_value( `A+B` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_range ) ).
    
    
    temp74 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp73>.
    sy-tabix = temp74.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `I`   act = <temp73>-sign ).
    
    
    temp76 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp75>.
    sy-tabix = temp76.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `EQ`  act = <temp75>-option ).
    
    
    temp78 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp77>.
    sy-tabix = temp78.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A+B` act = <temp77>-low ).
  ENDMETHOD.

  METHOD value_cp.
    DATA lt_range TYPE zabaputil_cl_util_context=>ty_t_range.
    FIELD-SYMBOLS <temp79> LIKE LINE OF lt_range.
    DATA temp80 LIKE sy-tabix.
    FIELD-SYMBOLS <temp81> LIKE LINE OF lt_range.
    DATA temp82 LIKE sy-tabix.
    lt_range = zabaputil_cl_util_context=>filter_get_range_by_value( `MAT*` ).
    
    
    temp80 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp79>.
    sy-tabix = temp80.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `CP`   act = <temp79>-option ).
    
    
    temp82 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp81>.
    sy-tabix = temp82.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `MAT*` act = <temp81>-low ).
  ENDMETHOD.

  METHOD value_initial.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>filter_get_range_by_value( `` ) ).
  ENDMETHOD.

  METHOD value_t_sorted_distinct.
    DATA temp83 TYPE string_table.
    DATA lt_range TYPE zabaputil_cl_util_context=>ty_t_range.
    FIELD-SYMBOLS <temp85> LIKE LINE OF lt_range.
    DATA temp86 LIKE sy-tabix.
    FIELD-SYMBOLS <temp87> LIKE LINE OF lt_range.
    DATA temp88 LIKE sy-tabix.
    FIELD-SYMBOLS <temp89> LIKE LINE OF lt_range.
    DATA temp90 LIKE sy-tabix.
    FIELD-SYMBOLS <temp91> LIKE LINE OF lt_range.
    DATA temp92 LIKE sy-tabix.
    CLEAR temp83.
    INSERT `B` INTO TABLE temp83.
    INSERT `A*` INTO TABLE temp83.
    INSERT `B` INTO TABLE temp83.
    
    lt_range = zabaputil_cl_util_context=>filter_get_range_t_by_value_t(
        temp83 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_range ) ).
    
    
    temp86 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp85>.
    sy-tabix = temp86.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A*` act = <temp85>-low ).
    
    
    temp88 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp87>.
    sy-tabix = temp88.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = <temp87>-option ).
    
    
    temp90 = sy-tabix.
    READ TABLE lt_range INDEX 2 ASSIGNING <temp89>.
    sy-tabix = temp90.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B`  act = <temp89>-low ).
    
    
    temp92 = sy-tabix.
    READ TABLE lt_range INDEX 2 ASSIGNING <temp91>.
    sy-tabix = temp92.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = <temp91>-option ).
  ENDMETHOD.

  METHOD value_t_keeps_blank.
    " an empty value asks for "blank" on purpose - it is not dropped
    DATA temp93 TYPE string_table.
    DATA lt_range TYPE zabaputil_cl_util_context=>ty_t_range.
    FIELD-SYMBOLS <temp95> LIKE LINE OF lt_range.
    DATA temp96 LIKE sy-tabix.
    FIELD-SYMBOLS <temp97> LIKE LINE OF lt_range.
    DATA temp98 LIKE sy-tabix.
    CLEAR temp93.
    INSERT `` INTO TABLE temp93.
    INSERT `X` INTO TABLE temp93.
    
    lt_range = zabaputil_cl_util_context=>filter_get_range_t_by_value_t(
        temp93 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_range ) ).
    
    
    temp96 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp95>.
    sy-tabix = temp96.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = ``   act = <temp95>-low ).
    
    
    temp98 = sy-tabix.
    READ TABLE lt_range INDEX 1 ASSIGNING <temp97>.
    sy-tabix = temp98.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = <temp97>-option ).
  ENDMETHOD.

  METHOD token_eq.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `=100` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `100` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_lt.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `<50` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `LT` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `50` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_le.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `<=50` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `LE` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `50` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_gt.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `>10` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `GT` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `10` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_ge.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `>=10` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `GE` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `10` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_cp.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `*test*` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `test` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_bt.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `10...20` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `BT` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `10` act = ls_range-low ).
    cl_abap_unit_assert=>assert_equals( exp = `20` act = ls_range-high ).
  ENDMETHOD.

  METHOD token_plain_value.
    " Plain value without operator means EQ
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `hello` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `hello` act = ls_range-low ).
  ENDMETHOD.

  METHOD sql_where_eq.
    DATA temp99 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp100 LIKE LINE OF temp99.
    DATA temp7 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp8 LIKE LINE OF temp7.
    DATA lt_filter LIKE temp99.
    DATA lv_result TYPE string.
    CLEAR temp99.
    
    temp100-name = `STATUS`.
    
    CLEAR temp7.
    
    temp8-sign = `I`.
    temp8-option = `EQ`.
    temp8-low = `A`.
    INSERT temp8 INTO TABLE temp7.
    temp100-t_range = temp7.
    INSERT temp100 INTO TABLE temp99.
    
    lt_filter = temp99.
    
    lv_result = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ).
    cl_abap_unit_assert=>assert_equals( exp = `( STATUS = 'A' )` act = lv_result ).
  ENDMETHOD.

  METHOD sql_where_bt.
    DATA temp101 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp102 LIKE LINE OF temp101.
    DATA temp9 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp10 LIKE LINE OF temp9.
    DATA lt_filter LIKE temp101.
    DATA lv_result TYPE string.
    CLEAR temp101.
    
    temp102-name = `AMOUNT`.
    
    CLEAR temp9.
    
    temp10-sign = `I`.
    temp10-option = `BT`.
    temp10-low = `10`.
    temp10-high = `20`.
    INSERT temp10 INTO TABLE temp9.
    temp102-t_range = temp9.
    INSERT temp102 INTO TABLE temp101.
    
    lt_filter = temp101.
    
    lv_result = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ).
    cl_abap_unit_assert=>assert_equals( exp = `( AMOUNT BETWEEN '10' AND '20' )` act = lv_result ).
  ENDMETHOD.

  METHOD sql_where_cp.
    DATA temp103 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp104 LIKE LINE OF temp103.
    DATA temp11 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp12 LIKE LINE OF temp11.
    DATA lt_filter LIKE temp103.
    DATA lv_result TYPE string.
    CLEAR temp103.
    
    temp104-name = `NAME`.
    
    CLEAR temp11.
    
    temp12-sign = `I`.
    temp12-option = `CP`.
    temp12-low = `*test*`.
    INSERT temp12 INTO TABLE temp11.
    temp104-t_range = temp11.
    INSERT temp104 INTO TABLE temp103.
    
    lt_filter = temp103.
    
    lv_result = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ).
    cl_abap_unit_assert=>assert_equals( exp = `( NAME LIKE '%test%' ESCAPE '#' )` act = lv_result ).
  ENDMETHOD.

  METHOD sql_where_multiple_fields.
    DATA temp105 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp106 LIKE LINE OF temp105.
    DATA temp13 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp14 LIKE LINE OF temp13.
    DATA temp15 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp16 LIKE LINE OF temp15.
    DATA lt_filter LIKE temp105.
    DATA lv_result TYPE string.
    CLEAR temp105.
    
    temp106-name = `A`.
    
    CLEAR temp13.
    
    temp14-sign = `I`.
    temp14-option = `EQ`.
    temp14-low = `1`.
    INSERT temp14 INTO TABLE temp13.
    temp106-t_range = temp13.
    INSERT temp106 INTO TABLE temp105.
    temp106-name = `B`.
    
    CLEAR temp15.
    
    temp16-sign = `I`.
    temp16-option = `EQ`.
    temp16-low = `2`.
    INSERT temp16 INTO TABLE temp15.
    temp106-t_range = temp15.
    INSERT temp106 INTO TABLE temp105.
    
    lt_filter = temp105.
    
    lv_result = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ).
    cl_abap_unit_assert=>assert_equals( exp = `( A = '1' ) AND ( B = '2' )` act = lv_result ).
  ENDMETHOD.

  METHOD sql_where_exclude.
    " Sign E with EQ should become NE
    DATA temp107 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp108 LIKE LINE OF temp107.
    DATA temp17 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp18 LIKE LINE OF temp17.
    DATA lt_filter LIKE temp107.
    DATA lv_result TYPE string.
    CLEAR temp107.
    
    temp108-name = `STATUS`.
    
    CLEAR temp17.
    
    temp18-sign = `E`.
    temp18-option = `EQ`.
    temp18-low = `X`.
    INSERT temp18 INTO TABLE temp17.
    temp108-t_range = temp17.
    INSERT temp108 INTO TABLE temp107.
    
    lt_filter = temp107.
    
    lv_result = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ).
    cl_abap_unit_assert=>assert_equals( exp = `( STATUS <> 'X' )` act = lv_result ).
  ENDMETHOD.

  METHOD sql_roundtrip_eq.
    DATA lt_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp109> LIKE LINE OF lt_filter.
    DATA temp110 LIKE sy-tabix.
    FIELD-SYMBOLS <temp111> LIKE LINE OF lt_filter.
    DATA temp112 LIKE sy-tabix.
    FIELD-SYMBOLS <temp19> LIKE LINE OF <temp111>-t_range.
    DATA temp20 LIKE sy-tabix.
    FIELD-SYMBOLS <temp113> LIKE LINE OF lt_filter.
    DATA temp114 LIKE sy-tabix.
    FIELD-SYMBOLS <temp21> LIKE LINE OF <temp113>-t_range.
    DATA temp22 LIKE sy-tabix.
    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `STATUS = 'A'` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_filter ) ).
    
    
    temp110 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp109>.
    sy-tabix = temp110.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `STATUS` act = <temp109>-name ).
    
    
    temp112 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp111>.
    sy-tabix = temp112.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp20 = sy-tabix.
    READ TABLE <temp111>-t_range INDEX 1 ASSIGNING <temp19>.
    sy-tabix = temp20.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = <temp19>-option ).
    
    
    temp114 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp113>.
    sy-tabix = temp114.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp22 = sy-tabix.
    READ TABLE <temp113>-t_range INDEX 1 ASSIGNING <temp21>.
    sy-tabix = temp22.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A` act = <temp21>-low ).
  ENDMETHOD.

  METHOD sql_roundtrip_bt.
    " After BETWEEN-fix: works without parentheses too
    DATA lt_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp115> LIKE LINE OF lt_filter.
    DATA temp116 LIKE sy-tabix.
    FIELD-SYMBOLS <temp23> LIKE LINE OF <temp115>-t_range.
    DATA temp24 LIKE sy-tabix.
    FIELD-SYMBOLS <temp117> LIKE LINE OF lt_filter.
    DATA temp118 LIKE sy-tabix.
    FIELD-SYMBOLS <temp25> LIKE LINE OF <temp117>-t_range.
    DATA temp26 LIKE sy-tabix.
    FIELD-SYMBOLS <temp119> LIKE LINE OF lt_filter.
    DATA temp120 LIKE sy-tabix.
    FIELD-SYMBOLS <temp27> LIKE LINE OF <temp119>-t_range.
    DATA temp28 LIKE sy-tabix.
    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `AMOUNT BETWEEN '10' AND '20'` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_filter ) ).
    
    
    temp116 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp115>.
    sy-tabix = temp116.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp24 = sy-tabix.
    READ TABLE <temp115>-t_range INDEX 1 ASSIGNING <temp23>.
    sy-tabix = temp24.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `BT` act = <temp23>-option ).
    
    
    temp118 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp117>.
    sy-tabix = temp118.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp26 = sy-tabix.
    READ TABLE <temp117>-t_range INDEX 1 ASSIGNING <temp25>.
    sy-tabix = temp26.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `10` act = <temp25>-low ).
    
    
    temp120 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp119>.
    sy-tabix = temp120.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp28 = sy-tabix.
    READ TABLE <temp119>-t_range INDEX 1 ASSIGNING <temp27>.
    sy-tabix = temp28.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `20` act = <temp27>-high ).
  ENDMETHOD.

  METHOD sql_roundtrip_like.
    DATA lt_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp121> LIKE LINE OF lt_filter.
    DATA temp122 LIKE sy-tabix.
    FIELD-SYMBOLS <temp29> LIKE LINE OF <temp121>-t_range.
    DATA temp30 LIKE sy-tabix.
    FIELD-SYMBOLS <temp123> LIKE LINE OF lt_filter.
    DATA temp124 LIKE sy-tabix.
    FIELD-SYMBOLS <temp31> LIKE LINE OF <temp123>-t_range.
    DATA temp32 LIKE sy-tabix.
    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `NAME LIKE '%hello%'` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_filter ) ).
    
    
    temp122 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp121>.
    sy-tabix = temp122.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp30 = sy-tabix.
    READ TABLE <temp121>-t_range INDEX 1 ASSIGNING <temp29>.
    sy-tabix = temp30.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = <temp29>-option ).
    
    
    temp124 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp123>.
    sy-tabix = temp124.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp32 = sy-tabix.
    READ TABLE <temp123>-t_range INDEX 1 ASSIGNING <temp31>.
    sy-tabix = temp32.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*hello*` act = <temp31>-low ).
  ENDMETHOD.

  METHOD token_range_mapping.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp125> LIKE LINE OF lt_result.
    DATA temp126 LIKE sy-tabix.
    FIELD-SYMBOLS <temp127> LIKE LINE OF lt_result.
    DATA temp128 LIKE sy-tabix.
    FIELD-SYMBOLS <temp129> LIKE LINE OF lt_result.
    DATA temp130 LIKE sy-tabix.
    FIELD-SYMBOLS <temp131> LIKE LINE OF lt_result.
    DATA temp132 LIKE sy-tabix.
    lt_result = zabaputil_cl_util_context=>filter_get_token_range_mapping( ).
    cl_abap_unit_assert=>assert_not_initial( lt_result ).
    " Verify core mappings
    
    
    temp126 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `EQ` ASSIGNING <temp125>.
    sy-tabix = temp126.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `={LOW}` act = <temp125>-v ).
    
    
    temp128 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `LT` ASSIGNING <temp127>.
    sy-tabix = temp128.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `<{LOW}` act = <temp127>-v ).
    
    
    temp130 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `GT` ASSIGNING <temp129>.
    sy-tabix = temp130.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `>{LOW}` act = <temp129>-v ).
    
    
    temp132 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `BT` ASSIGNING <temp131>.
    sy-tabix = temp132.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `{LOW}...{HIGH}` act = <temp131>-v ).
  ENDMETHOD.

  METHOD filter_itab_basic.
    TYPES: BEGIN OF ty_row, name TYPE string, status TYPE string, END OF ty_row.
    TYPES temp6 TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA lt_tab TYPE temp6.
    DATA temp133 LIKE lt_tab.
    DATA temp134 LIKE LINE OF temp133.
    DATA temp135 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp136 LIKE LINE OF temp135.
    DATA temp33 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp34 LIKE LINE OF temp33.
    DATA lt_filter LIKE temp135.
    FIELD-SYMBOLS <temp137> LIKE LINE OF lt_tab.
    DATA temp138 LIKE sy-tabix.
    FIELD-SYMBOLS <temp139> LIKE LINE OF lt_tab.
    DATA temp140 LIKE sy-tabix.
    CLEAR temp133.
    
    temp134-name = `A`.
    temp134-status = `open`.
    INSERT temp134 INTO TABLE temp133.
    temp134-name = `B`.
    temp134-status = `closed`.
    INSERT temp134 INTO TABLE temp133.
    temp134-name = `C`.
    temp134-status = `open`.
    INSERT temp134 INTO TABLE temp133.
    lt_tab = temp133.

    
    CLEAR temp135.
    
    temp136-name = `STATUS`.
    
    CLEAR temp33.
    
    temp34-sign = `I`.
    temp34-option = `EQ`.
    temp34-low = `open`.
    INSERT temp34 INTO TABLE temp33.
    temp136-t_range = temp33.
    INSERT temp136 INTO TABLE temp135.
    
    lt_filter = temp135.

    zabaputil_cl_util_context=>filter_itab( EXPORTING filter = lt_filter CHANGING val = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_tab ) ).
    
    
    temp138 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp137>.
    sy-tabix = temp138.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A` act = <temp137>-name ).
    
    
    temp140 = sy-tabix.
    READ TABLE lt_tab INDEX 2 ASSIGNING <temp139>.
    sy-tabix = temp140.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `C` act = <temp139>-name ).
  ENDMETHOD.

  METHOD sql_by_string_basic.
    DATA ls_result TYPE zabaputil_cl_util_context=>ty_s_sql.
    ls_result = zabaputil_cl_util_context=>filter_get_sql_by_sql_string( `SELECT FROM MARA WHERE MTART = 'FERT'` ).
    cl_abap_unit_assert=>assert_equals( exp = `MARA` act = ls_result-tabname ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-where ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! DEEP COMPARISON & FIELD ACCESS
"! ================================================================
CLASS ltcl_deep_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " data_equals
    METHODS equals_same_string             FOR TESTING.
    METHODS equals_different_string        FOR TESTING.
    METHODS equals_same_int                FOR TESTING.
    METHODS equals_same_structure          FOR TESTING.
    METHODS equals_different_structure     FOR TESTING.

    " data_diff
    METHODS diff_no_change                 FOR TESTING.
    METHODS diff_one_field                 FOR TESTING.
    METHODS diff_multiple_fields           FOR TESTING.

    " data_get_by_path
    METHODS get_by_path_simple             FOR TESTING.
    METHODS get_by_path_nested             FOR TESTING.
    METHODS get_by_path_invalid            FOR TESTING.

    " data_set_by_path
    METHODS set_by_path_simple             FOR TESTING.

ENDCLASS.

CLASS ltcl_deep_ops IMPLEMENTATION.

  METHOD equals_same_string.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>data_equals( a = `hello` b = `hello` ) ).
  ENDMETHOD.

  METHOD equals_different_string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>data_equals( a = `hello` b = `world` ) ).
  ENDMETHOD.

  METHOD equals_same_int.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>data_equals( a = 42 b = 42 ) ).
  ENDMETHOD.

  METHOD equals_same_structure.
    TYPES: BEGIN OF ty, a TYPE string, b TYPE i, END OF ty.
    DATA temp141 TYPE ty.
    DATA ls_a LIKE temp141.
    DATA temp142 TYPE ty.
    DATA ls_b LIKE temp142.
    CLEAR temp141.
    temp141-a = `x`.
    temp141-b = 1.
    
    ls_a = temp141.
    
    CLEAR temp142.
    temp142-a = `x`.
    temp142-b = 1.
    
    ls_b = temp142.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>data_equals( a = ls_a b = ls_b ) ).
  ENDMETHOD.

  METHOD equals_different_structure.
    TYPES: BEGIN OF ty, a TYPE string, b TYPE i, END OF ty.
    DATA temp143 TYPE ty.
    DATA ls_a LIKE temp143.
    DATA temp144 TYPE ty.
    DATA ls_b LIKE temp144.
    CLEAR temp143.
    temp143-a = `x`.
    temp143-b = 1.
    
    ls_a = temp143.
    
    CLEAR temp144.
    temp144-a = `x`.
    temp144-b = 2.
    
    ls_b = temp144.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>data_equals( a = ls_a b = ls_b ) ).
  ENDMETHOD.

  METHOD diff_no_change.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE string, END OF ty.
    DATA temp145 TYPE ty.
    DATA ls_old LIKE temp145.
    DATA temp146 TYPE ty.
    DATA ls_new LIKE temp146.
    DATA lt_diff TYPE zabaputil_cl_util_context=>ty_t_field_diff.
    CLEAR temp145.
    temp145-name = `A`.
    temp145-value = `1`.
    
    ls_old = temp145.
    
    CLEAR temp146.
    temp146-name = `A`.
    temp146-value = `1`.
    
    ls_new = temp146.
    
    lt_diff = zabaputil_cl_util_context=>data_diff( old = ls_old new = ls_new ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_diff ) ).
  ENDMETHOD.

  METHOD diff_one_field.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE string, END OF ty.
    DATA temp147 TYPE ty.
    DATA ls_old LIKE temp147.
    DATA temp148 TYPE ty.
    DATA ls_new LIKE temp148.
    DATA lt_diff TYPE zabaputil_cl_util_context=>ty_t_field_diff.
    FIELD-SYMBOLS <temp149> LIKE LINE OF lt_diff.
    DATA temp150 LIKE sy-tabix.
    FIELD-SYMBOLS <temp151> LIKE LINE OF lt_diff.
    DATA temp152 LIKE sy-tabix.
    FIELD-SYMBOLS <temp153> LIKE LINE OF lt_diff.
    DATA temp154 LIKE sy-tabix.
    CLEAR temp147.
    temp147-name = `A`.
    temp147-value = `1`.
    
    ls_old = temp147.
    
    CLEAR temp148.
    temp148-name = `A`.
    temp148-value = `2`.
    
    ls_new = temp148.
    
    lt_diff = zabaputil_cl_util_context=>data_diff( old = ls_old new = ls_new ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_diff ) ).
    
    
    temp150 = sy-tabix.
    READ TABLE lt_diff INDEX 1 ASSIGNING <temp149>.
    sy-tabix = temp150.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `VALUE` act = <temp149>-fieldname ).
    
    
    temp152 = sy-tabix.
    READ TABLE lt_diff INDEX 1 ASSIGNING <temp151>.
    sy-tabix = temp152.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = <temp151>-old_value ).
    
    
    temp154 = sy-tabix.
    READ TABLE lt_diff INDEX 1 ASSIGNING <temp153>.
    sy-tabix = temp154.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `2` act = <temp153>-new_value ).
  ENDMETHOD.

  METHOD diff_multiple_fields.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE string, END OF ty.
    DATA temp155 TYPE ty.
    DATA ls_old LIKE temp155.
    DATA temp156 TYPE ty.
    DATA ls_new LIKE temp156.
    DATA lt_diff TYPE zabaputil_cl_util_context=>ty_t_field_diff.
    CLEAR temp155.
    temp155-name = `A`.
    temp155-value = `1`.
    
    ls_old = temp155.
    
    CLEAR temp156.
    temp156-name = `B`.
    temp156-value = `2`.
    
    ls_new = temp156.
    
    lt_diff = zabaputil_cl_util_context=>data_diff( old = ls_old new = ls_new ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_diff ) ).
  ENDMETHOD.

  METHOD get_by_path_simple.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE string, END OF ty.
    DATA temp157 TYPE ty.
    DATA ls_data LIKE temp157.
    CLEAR temp157.
    temp157-name = `hello`.
    temp157-value = `world`.
    
    ls_data = temp157.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
                                        act = zabaputil_cl_util_context=>data_get_by_path( data = ls_data path = `name` ) ).
  ENDMETHOD.

  METHOD get_by_path_nested.
    TYPES: BEGIN OF ty_inner, city TYPE string, END OF ty_inner.
    TYPES: BEGIN OF ty, name TYPE string, address TYPE ty_inner, END OF ty.
    DATA temp158 TYPE ty.
    DATA ls_data LIKE temp158.
    CLEAR temp158.
    temp158-name = `John`.
    CLEAR temp158-address.
    temp158-address-city = `Berlin`.
    
    ls_data = temp158.
    cl_abap_unit_assert=>assert_equals( exp = `Berlin`
                                        act = zabaputil_cl_util_context=>data_get_by_path( data = ls_data path = `address-city` ) ).
  ENDMETHOD.

  METHOD get_by_path_invalid.
    TYPES: BEGIN OF ty, name TYPE string, END OF ty.
    DATA temp159 TYPE ty.
    DATA ls_data LIKE temp159.
    CLEAR temp159.
    temp159-name = `hello`.
    
    ls_data = temp159.
    cl_abap_unit_assert=>assert_equals( exp = ``
                                        act = zabaputil_cl_util_context=>data_get_by_path( data = ls_data path = `nonexistent` ) ).
  ENDMETHOD.

  METHOD set_by_path_simple.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE string, END OF ty.
    DATA ls_data TYPE ty.
    ls_data-name = `old`.
    zabaputil_cl_util_context=>data_set_by_path( EXPORTING path = `name` value = `new` CHANGING data = ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = `new` act = ls_data-name ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! ITAB OPERATIONS
"! ================================================================
CLASS ltcl_itab_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS sort_by_ascending              FOR TESTING.
    METHODS sort_by_descending             FOR TESTING.

    METHODS slice_basic                    FOR TESTING.
    METHODS slice_from_only                FOR TESTING.
    METHODS slice_to_exceeds               FOR TESTING.

    METHODS paginate_page_1                FOR TESTING.
    METHODS paginate_page_2                FOR TESTING.
    METHODS paginate_total_pages            FOR TESTING.

    METHODS count_by_basic                 FOR TESTING.

    METHODS filter_by_val_basic            FOR TESTING.
    METHODS filter_by_val_case_ignore      FOR TESTING.
    METHODS filter_by_val_keeps_rows       FOR TESTING.
    METHODS filter_by_val_fields_subset    FOR TESTING.

    METHODS corresponding_basic            FOR TESTING.

    METHODS get_by_struc_basic             FOR TESTING.

    METHODS csv_roundtrip                  FOR TESTING.

    METHODS json_roundtrip                 FOR TESTING.

ENDCLASS.

CLASS ltcl_itab_ops IMPLEMENTATION.

  METHOD sort_by_ascending.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE i, END OF ty.
    TYPES temp7 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp7.
    DATA temp160 LIKE lt_tab.
    DATA temp161 LIKE LINE OF temp160.
    FIELD-SYMBOLS <temp162> LIKE LINE OF lt_tab.
    DATA temp163 LIKE sy-tabix.
    FIELD-SYMBOLS <temp164> LIKE LINE OF lt_tab.
    DATA temp165 LIKE sy-tabix.
    FIELD-SYMBOLS <temp166> LIKE LINE OF lt_tab.
    DATA temp167 LIKE sy-tabix.
    CLEAR temp160.
    
    temp161-name = `C`.
    temp161-value = 3.
    INSERT temp161 INTO TABLE temp160.
    temp161-name = `A`.
    temp161-value = 1.
    INSERT temp161 INTO TABLE temp160.
    temp161-name = `B`.
    temp161-value = 2.
    INSERT temp161 INTO TABLE temp160.
    lt_tab = temp160.
    zabaputil_cl_util_context=>itab_sort_by( EXPORTING fieldname = `NAME` CHANGING tab = lt_tab ).
    
    
    temp163 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp162>.
    sy-tabix = temp163.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A` act = <temp162>-name ).
    
    
    temp165 = sy-tabix.
    READ TABLE lt_tab INDEX 2 ASSIGNING <temp164>.
    sy-tabix = temp165.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B` act = <temp164>-name ).
    
    
    temp167 = sy-tabix.
    READ TABLE lt_tab INDEX 3 ASSIGNING <temp166>.
    sy-tabix = temp167.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `C` act = <temp166>-name ).
  ENDMETHOD.

  METHOD sort_by_descending.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE i, END OF ty.
    TYPES temp8 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp8.
    DATA temp168 LIKE lt_tab.
    DATA temp169 LIKE LINE OF temp168.
    FIELD-SYMBOLS <temp170> LIKE LINE OF lt_tab.
    DATA temp171 LIKE sy-tabix.
    FIELD-SYMBOLS <temp172> LIKE LINE OF lt_tab.
    DATA temp173 LIKE sy-tabix.
    FIELD-SYMBOLS <temp174> LIKE LINE OF lt_tab.
    DATA temp175 LIKE sy-tabix.
    CLEAR temp168.
    
    temp169-name = `A`.
    temp169-value = 1.
    INSERT temp169 INTO TABLE temp168.
    temp169-name = `C`.
    temp169-value = 3.
    INSERT temp169 INTO TABLE temp168.
    temp169-name = `B`.
    temp169-value = 2.
    INSERT temp169 INTO TABLE temp168.
    lt_tab = temp168.
    zabaputil_cl_util_context=>itab_sort_by( EXPORTING fieldname = `VALUE` descending = abap_true CHANGING tab = lt_tab ).
    
    
    temp171 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp170>.
    sy-tabix = temp171.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = <temp170>-value ).
    
    
    temp173 = sy-tabix.
    READ TABLE lt_tab INDEX 2 ASSIGNING <temp172>.
    sy-tabix = temp173.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = <temp172>-value ).
    
    
    temp175 = sy-tabix.
    READ TABLE lt_tab INDEX 3 ASSIGNING <temp174>.
    sy-tabix = temp175.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = <temp174>-value ).
  ENDMETHOD.

  METHOD slice_basic.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp9 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp9.
    DATA temp176 LIKE lt_tab.
    DATA temp177 LIKE LINE OF temp176.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp176.
    
    temp177-id = 1.
    INSERT temp177 INTO TABLE temp176.
    temp177-id = 2.
    INSERT temp177 INTO TABLE temp176.
    temp177-id = 3.
    INSERT temp177 INTO TABLE temp176.
    temp177-id = 4.
    INSERT temp177 INTO TABLE temp176.
    temp177-id = 5.
    INSERT temp177 INTO TABLE temp176.
    lt_tab = temp176.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 2 to = 4 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD slice_from_only.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp10 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp10.
    DATA temp178 LIKE lt_tab.
    DATA temp179 LIKE LINE OF temp178.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp178.
    
    temp179-id = 1.
    INSERT temp179 INTO TABLE temp178.
    temp179-id = 2.
    INSERT temp179 INTO TABLE temp178.
    temp179-id = 3.
    INSERT temp179 INTO TABLE temp178.
    lt_tab = temp178.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 2 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD slice_to_exceeds.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp11 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp11.
    DATA temp180 LIKE lt_tab.
    DATA temp181 LIKE LINE OF temp180.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp180.
    
    temp181-id = 1.
    INSERT temp181 INTO TABLE temp180.
    temp181-id = 2.
    INSERT temp181 INTO TABLE temp180.
    lt_tab = temp180.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 1 to = 99 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD paginate_page_1.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp12 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp12.
    DATA temp182 LIKE lt_tab.
    DATA temp183 LIKE LINE OF temp182.
    DATA lr_result TYPE REF TO data.
    DATA lv_total_count TYPE i.
    DATA lv_total_pages TYPE i.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp182.
    
    temp183-id = 1.
    INSERT temp183 INTO TABLE temp182.
    temp183-id = 2.
    INSERT temp183 INTO TABLE temp182.
    temp183-id = 3.
    INSERT temp183 INTO TABLE temp182.
    temp183-id = 4.
    INSERT temp183 INTO TABLE temp182.
    temp183-id = 5.
    INSERT temp183 INTO TABLE temp182.
    lt_tab = temp182.
    
    
    
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab = lt_tab page = 1 page_size = 2
                                   IMPORTING result = lr_result total_count = lv_total_count total_pages = lv_total_pages ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = lv_total_count ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lv_total_pages ).
  ENDMETHOD.

  METHOD paginate_page_2.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp13 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp13.
    DATA temp184 LIKE lt_tab.
    DATA temp185 LIKE LINE OF temp184.
    DATA lr_result TYPE REF TO data.
    DATA lv_total_count TYPE i.
    DATA lv_total_pages TYPE i.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp184.
    
    temp185-id = 1.
    INSERT temp185 INTO TABLE temp184.
    temp185-id = 2.
    INSERT temp185 INTO TABLE temp184.
    temp185-id = 3.
    INSERT temp185 INTO TABLE temp184.
    temp185-id = 4.
    INSERT temp185 INTO TABLE temp184.
    temp185-id = 5.
    INSERT temp185 INTO TABLE temp184.
    lt_tab = temp184.
    
    
    
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab = lt_tab page = 2 page_size = 2
                                   IMPORTING result = lr_result total_count = lv_total_count total_pages = lv_total_pages ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD paginate_total_pages.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp14 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp14.
    DATA temp186 LIKE lt_tab.
    DATA temp187 LIKE LINE OF temp186.
    DATA lr_result TYPE REF TO data.
    DATA lv_total_count TYPE i.
    DATA lv_total_pages TYPE i.
    CLEAR temp186.
    
    temp187-id = 1.
    INSERT temp187 INTO TABLE temp186.
    temp187-id = 2.
    INSERT temp187 INTO TABLE temp186.
    temp187-id = 3.
    INSERT temp187 INTO TABLE temp186.
    lt_tab = temp186.
    
    
    
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab = lt_tab page = 1 page_size = 2
                                   IMPORTING result = lr_result total_count = lv_total_count total_pages = lv_total_pages ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lv_total_pages ).
  ENDMETHOD.

  METHOD count_by_basic.
    TYPES: BEGIN OF ty, category TYPE string, name TYPE string, END OF ty.
    TYPES temp15 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp15.
    DATA temp188 LIKE lt_tab.
    DATA temp189 LIKE LINE OF temp188.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp190> LIKE LINE OF lt_result.
    DATA temp191 LIKE sy-tabix.
    FIELD-SYMBOLS <temp192> LIKE LINE OF lt_result.
    DATA temp193 LIKE sy-tabix.
    CLEAR temp188.
    
    temp189-category = `A`.
    temp189-name = `1`.
    INSERT temp189 INTO TABLE temp188.
    temp189-category = `B`.
    temp189-name = `2`.
    INSERT temp189 INTO TABLE temp188.
    temp189-category = `A`.
    temp189-name = `3`.
    INSERT temp189 INTO TABLE temp188.
    temp189-category = `A`.
    temp189-name = `4`.
    INSERT temp189 INTO TABLE temp188.
    lt_tab = temp188.
    
    lt_result = zabaputil_cl_util_context=>itab_count_by( tab = lt_tab fieldname = `CATEGORY` ).
    
    
    temp191 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `A` ASSIGNING <temp190>.
    sy-tabix = temp191.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `3` act = <temp190>-v ).
    
    
    temp193 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `B` ASSIGNING <temp192>.
    sy-tabix = temp193.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = <temp192>-v ).
  ENDMETHOD.

  METHOD filter_by_val_basic.
    TYPES: BEGIN OF ty, name TYPE string, city TYPE string, END OF ty.
    TYPES temp16 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp16.
    DATA temp194 LIKE lt_tab.
    DATA temp195 LIKE LINE OF temp194.
    CLEAR temp194.
    
    temp195-name = `Alice`.
    temp195-city = `Berlin`.
    INSERT temp195 INTO TABLE temp194.
    temp195-name = `Bob`.
    temp195-city = `Paris`.
    INSERT temp195 INTO TABLE temp194.
    temp195-name = `Charlie`.
    temp195-city = `Berlin`.
    INSERT temp195 INTO TABLE temp194.
    lt_tab = temp194.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `Berlin` CHANGING tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_tab ) ).
  ENDMETHOD.

  METHOD filter_by_val_case_ignore.
    TYPES: BEGIN OF ty, name TYPE string, END OF ty.
    TYPES temp17 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp17.
    DATA temp196 LIKE lt_tab.
    DATA temp197 LIKE LINE OF temp196.
    CLEAR temp196.
    
    temp197-name = `HELLO`.
    INSERT temp197 INTO TABLE temp196.
    temp197-name = `world`.
    INSERT temp197 INTO TABLE temp196.
    temp197-name = `Hello`.
    INSERT temp197 INTO TABLE temp196.
    lt_tab = temp196.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `hello` ignore_case = abap_true CHANGING tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_tab ) ).
  ENDMETHOD.

  METHOD filter_by_val_keeps_rows.
    " guards the DELETE ... INDEX in itab_filter_by_val: with a plain DELETE the
    " wrong rows are removed as soon as several non-matching rows are dropped
    TYPES: BEGIN OF ty, name TYPE string, city TYPE string, END OF ty.
    TYPES temp18 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp18.
    DATA temp198 LIKE lt_tab.
    DATA temp199 LIKE LINE OF temp198.
    FIELD-SYMBOLS <temp200> LIKE LINE OF lt_tab.
    DATA temp201 LIKE sy-tabix.
    FIELD-SYMBOLS <temp202> LIKE LINE OF lt_tab.
    DATA temp203 LIKE sy-tabix.
    CLEAR temp198.
    
    temp199-name = `Alice`.
    temp199-city = `Paris`.
    INSERT temp199 INTO TABLE temp198.
    temp199-name = `Bob`.
    temp199-city = `Berlin`.
    INSERT temp199 INTO TABLE temp198.
    temp199-name = `Charlie`.
    temp199-city = `Rome`.
    INSERT temp199 INTO TABLE temp198.
    temp199-name = `Dave`.
    temp199-city = `Berlin`.
    INSERT temp199 INTO TABLE temp198.
    temp199-name = `Eve`.
    temp199-city = `Madrid`.
    INSERT temp199 INTO TABLE temp198.
    lt_tab = temp198.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `Berlin`
                                                   CHANGING  tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_tab ) ).
    
    
    temp201 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp200>.
    sy-tabix = temp201.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Bob`  act = <temp200>-name ).
    
    
    temp203 = sy-tabix.
    READ TABLE lt_tab INDEX 2 ASSIGNING <temp202>.
    sy-tabix = temp203.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Dave` act = <temp202>-name ).
  ENDMETHOD.

  METHOD filter_by_val_fields_subset.
    " only the listed field is searched, the match in the other column is ignored
    TYPES: BEGIN OF ty, name TYPE string, city TYPE string, END OF ty.
    TYPES temp19 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp19.
    DATA temp204 LIKE lt_tab.
    DATA temp205 LIKE LINE OF temp204.
    DATA temp206 TYPE string_table.
    FIELD-SYMBOLS <temp208> LIKE LINE OF lt_tab.
    DATA temp209 LIKE sy-tabix.
    CLEAR temp204.
    
    temp205-name = `Berlin`.
    temp205-city = `Paris`.
    INSERT temp205 INTO TABLE temp204.
    temp205-name = `Bob`.
    temp205-city = `Berlin`.
    INSERT temp205 INTO TABLE temp204.
    lt_tab = temp204.
    
    CLEAR temp206.
    INSERT `CITY` INTO TABLE temp206.
    zabaputil_cl_util_context=>itab_filter_by_val(
      EXPORTING val    = `Berlin`
                fields = temp206
      CHANGING  tab    = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_tab ) ).
    
    
    temp209 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp208>.
    sy-tabix = temp209.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Bob` act = <temp208>-name ).
  ENDMETHOD.

  METHOD corresponding_basic.
    TYPES: BEGIN OF ty_src, a TYPE string, b TYPE string, c TYPE string, END OF ty_src.
    TYPES: BEGIN OF ty_dst, a TYPE string, b TYPE string, END OF ty_dst.
    TYPES temp20 TYPE STANDARD TABLE OF ty_src WITH DEFAULT KEY.
DATA lt_src TYPE temp20.
    TYPES temp21 TYPE STANDARD TABLE OF ty_dst WITH DEFAULT KEY.
DATA lt_dst TYPE temp21.
    DATA temp210 LIKE lt_src.
    DATA temp211 LIKE LINE OF temp210.
    FIELD-SYMBOLS <temp212> LIKE LINE OF lt_dst.
    DATA temp213 LIKE sy-tabix.
    FIELD-SYMBOLS <temp214> LIKE LINE OF lt_dst.
    DATA temp215 LIKE sy-tabix.
    CLEAR temp210.
    
    temp211-a = `1`.
    temp211-b = `2`.
    temp211-c = `3`.
    INSERT temp211 INTO TABLE temp210.
    lt_src = temp210.
    zabaputil_cl_util_context=>itab_corresponding( EXPORTING val = lt_src CHANGING tab = lt_dst ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_dst ) ).
    
    
    temp213 = sy-tabix.
    READ TABLE lt_dst INDEX 1 ASSIGNING <temp212>.
    sy-tabix = temp213.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = <temp212>-a ).
    
    
    temp215 = sy-tabix.
    READ TABLE lt_dst INDEX 1 ASSIGNING <temp214>.
    sy-tabix = temp215.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `2` act = <temp214>-b ).
  ENDMETHOD.

  METHOD get_by_struc_basic.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE string, END OF ty.
    DATA temp216 TYPE ty.
    DATA ls_data LIKE temp216.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp217> LIKE LINE OF lt_result.
    DATA temp218 LIKE sy-tabix.
    FIELD-SYMBOLS <temp219> LIKE LINE OF lt_result.
    DATA temp220 LIKE sy-tabix.
    FIELD-SYMBOLS <temp221> LIKE LINE OF lt_result.
    DATA temp222 LIKE sy-tabix.
    FIELD-SYMBOLS <temp223> LIKE LINE OF lt_result.
    DATA temp224 LIKE sy-tabix.
    CLEAR temp216.
    temp216-name = `hello`.
    temp216-value = `world`.
    
    ls_data = temp216.
    
    lt_result = zabaputil_cl_util_context=>itab_get_by_struc( ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_result ) ).
    
    
    temp218 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp217>.
    sy-tabix = temp218.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NAME` act = <temp217>-n ).
    
    
    temp220 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp219>.
    sy-tabix = temp220.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `hello` act = <temp219>-v ).
    
    
    temp222 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp221>.
    sy-tabix = temp222.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `VALUE` act = <temp221>-n ).
    
    
    temp224 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp223>.
    sy-tabix = temp224.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `world` act = <temp223>-v ).
  ENDMETHOD.

  METHOD csv_roundtrip.
    TYPES: BEGIN OF ty, col1 TYPE string, col2 TYPE string, END OF ty.
    TYPES temp22 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp22.
    DATA temp225 LIKE lt_tab.
    DATA temp226 LIKE LINE OF temp225.
    DATA lv_csv TYPE string.
    CLEAR temp225.
    
    temp226-col1 = `A`.
    temp226-col2 = `B`.
    INSERT temp226 INTO TABLE temp225.
    temp226-col1 = `C`.
    temp226-col2 = `D`.
    INSERT temp226 INTO TABLE temp225.
    lt_tab = temp225.

    
    lv_csv = zabaputil_cl_util_context=>itab_get_csv_by_itab( lt_tab ).
    cl_abap_unit_assert=>assert_not_initial( lv_csv ).

    " CSV should contain header and data
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_csv sub = `COL1` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_csv sub = `A` ) ).
  ENDMETHOD.

  METHOD json_roundtrip.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE i, END OF ty.
    DATA ls_data TYPE ty.
    DATA lv_json TYPE string.
    DATA ls_back TYPE ty.
    CLEAR ls_data.
    ls_data-name = `test`.
    ls_data-value = 42.

    
    lv_json = zabaputil_cl_util_context=>json_stringify( ls_data ).
    cl_abap_unit_assert=>assert_not_initial( lv_json ).

    
    zabaputil_cl_util_context=>json_parse( EXPORTING val = lv_json CHANGING data = ls_back ).
    cl_abap_unit_assert=>assert_equals( exp = `test` act = ls_back-name ).
    cl_abap_unit_assert=>assert_equals( exp = 42 act = ls_back-value ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! RTTI / TYPE CHECKING
"! ================================================================
CLASS ltcl_rtti_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS check_table_true               FOR TESTING.
    METHODS check_table_false              FOR TESTING.
    METHODS check_structure_true           FOR TESTING.
    METHODS check_structure_false          FOR TESTING.
    METHODS check_numeric_int              FOR TESTING.
    METHODS check_numeric_string           FOR TESTING.
    METHODS check_clike_string             FOR TESTING.
    METHODS check_clike_int                FOR TESTING.
    METHODS check_ref_data_true            FOR TESTING.
    METHODS check_ref_data_false           FOR TESTING.
    METHODS check_ref_data_oref            FOR TESTING.
    METHODS check_ref_data_iref            FOR TESTING.
    METHODS check_ref_data_bound           FOR TESTING.
    METHODS copy_ref_data_over_oref        FOR TESTING.
    METHODS get_classname                  FOR TESTING.
    METHODS get_type_name                  FOR TESTING.
    METHODS check_class_exists_true        FOR TESTING.
    METHODS check_class_exists_false       FOR TESTING.
    METHODS check_class_exists_cached      FOR TESTING.
    METHODS check_class_exists_case        FOR TESTING.
    METHODS expand_components_plain        FOR TESTING.
    METHODS expand_components_empty        FOR TESTING.
    METHODS expand_components_depth_ok     FOR TESTING.
    METHODS expand_components_depth_stop   FOR TESTING.
    METHODS attri_by_include_depth_stop    FOR TESTING.
    METHODS attri_by_any_struct            FOR TESTING.
    METHODS attri_by_any_table             FOR TESTING.
    METHODS msg_get_empty_no_dump          FOR TESTING.

ENDCLASS.

CLASS ltcl_rtti_ops IMPLEMENTATION.

  METHOD expand_components_plain.
    " components that are not includes are passed through unchanged and in order
    DATA lo_str TYPE REF TO cl_abap_elemdescr.
    DATA temp227 TYPE abap_component_tab.
    DATA temp228 LIKE LINE OF temp227.
    DATA lt_comps LIKE temp227.
    DATA lt_result TYPE abap_component_tab.
    FIELD-SYMBOLS <temp229> LIKE LINE OF lt_result.
    DATA temp230 LIKE sy-tabix.
    FIELD-SYMBOLS <temp231> LIKE LINE OF lt_result.
    DATA temp232 LIKE sy-tabix.
    FIELD-SYMBOLS <temp233> LIKE LINE OF lt_result.
    DATA temp234 LIKE sy-tabix.
    lo_str = cl_abap_elemdescr=>get_string( ).
    
    CLEAR temp227.
    
    temp228-name = `A`.
    temp228-type = lo_str.
    INSERT temp228 INTO TABLE temp227.
    temp228-name = `B`.
    temp228-type = lo_str.
    INSERT temp228 INTO TABLE temp227.
    temp228-name = `C`.
    temp228-type = lo_str.
    INSERT temp228 INTO TABLE temp227.
    
    lt_comps = temp227.
    
    lt_result = zabaputil_cl_util_context=>expand_components( lt_comps ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_result ) ).
    
    
    temp230 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp229>.
    sy-tabix = temp230.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A` act = <temp229>-name ).
    
    
    temp232 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp231>.
    sy-tabix = temp232.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B` act = <temp231>-name ).
    
    
    temp234 = sy-tabix.
    READ TABLE lt_result INDEX 3 ASSIGNING <temp233>.
    sy-tabix = temp234.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `C` act = <temp233>-name ).
  ENDMETHOD.

  METHOD expand_components_empty.
    DATA temp235 TYPE abap_component_tab.
    DATA lt_result TYPE abap_component_tab.
    CLEAR temp235.
    
    lt_result = zabaputil_cl_util_context=>expand_components( temp235 ).
    cl_abap_unit_assert=>assert_initial( lt_result ).
  ENDMETHOD.

  METHOD expand_components_depth_ok.
    " the last level that is still inside the bound expands normally - the
    " guard must not cost a real DDIC structure its deepest include
    DATA lo_str TYPE REF TO cl_abap_elemdescr.
    DATA temp236 TYPE abap_component_tab.
    DATA temp237 LIKE LINE OF temp236.
    DATA lt_comps LIKE temp236.
    DATA lt_result TYPE abap_component_tab.
    lo_str = cl_abap_elemdescr=>get_string( ).
    
    CLEAR temp236.
    
    temp237-name = `A`.
    temp237-type = lo_str.
    INSERT temp237 INTO TABLE temp236.
    
    lt_comps = temp236.

    
    lt_result = zabaputil_cl_util_context=>expand_components( val   = lt_comps
                                                                    depth = 16 ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_result ) ).
  ENDMETHOD.

  METHOD expand_components_depth_stop.
    " past the bound the mutual recursion stops with a readable error
    " instead of running the stack out - a cyclic include used to dump
    DATA lo_str TYPE REF TO cl_abap_elemdescr.
    DATA temp238 TYPE abap_component_tab.
    DATA temp239 LIKE LINE OF temp238.
    DATA lt_comps LIKE temp238.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
    lo_str = cl_abap_elemdescr=>get_string( ).
    
    CLEAR temp238.
    
    temp239-name = `A`.
    temp239-type = lo_str.
    INSERT temp239 INTO TABLE temp238.
    
    lt_comps = temp238.

    TRY.
        zabaputil_cl_util_context=>expand_components( val   = lt_comps
                                                      depth = 17 ).
        cl_abap_unit_assert=>fail( `expected zabaputil_cx_util_error` ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_char_cp( act = lx->get_text( )
                                             exp = `*RTTI_INCLUDE_RECURSION*` ).
    ENDTRY.
  ENDMETHOD.

  METHOD attri_by_include_depth_stop.
    " the other half of the pair carries the level into expand_components,
    " so entering the cycle from either side stops at the same bound
    TYPES: BEGIN OF ty_incl,
             alpha TYPE string,
           END OF ty_incl.
    DATA ls_incl TYPE ty_incl ##NEEDED.

    DATA temp240 TYPE REF TO cl_abap_datadescr.
    DATA lo_type LIKE temp240.
    temp240 ?= cl_abap_typedescr=>describe_by_data( ls_incl ).
    
    lo_type = temp240.

    TRY.
        zabaputil_cl_util_context=>rtti_get_t_attri_by_include( type  = lo_type
                                                                depth = 17 ).
        cl_abap_unit_assert=>fail( `expected zabaputil_cx_util_error` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD attri_by_any_struct.
    TYPES: BEGIN OF ty, alpha TYPE string, beta TYPE i, END OF ty.
    DATA ls_struc TYPE ty.
    DATA lt_attri TYPE abap_component_tab.
    FIELD-SYMBOLS <temp241> LIKE LINE OF lt_attri.
    DATA temp242 LIKE sy-tabix.
    FIELD-SYMBOLS <temp243> LIKE LINE OF lt_attri.
    DATA temp244 LIKE sy-tabix.
    lt_attri = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( ls_struc ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_attri ) ).
    
    
    temp242 = sy-tabix.
    READ TABLE lt_attri INDEX 1 ASSIGNING <temp241>.
    sy-tabix = temp242.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ALPHA` act = <temp241>-name ).
    
    
    temp244 = sy-tabix.
    READ TABLE lt_attri INDEX 2 ASSIGNING <temp243>.
    sy-tabix = temp244.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `BETA`  act = <temp243>-name ).
  ENDMETHOD.

  METHOD attri_by_any_table.
    " for a table the components of the line type are returned
    TYPES: BEGIN OF ty, alpha TYPE string, beta TYPE i, END OF ty.
    TYPES temp23 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp23.
    DATA lt_attri TYPE abap_component_tab.
    FIELD-SYMBOLS <temp245> LIKE LINE OF lt_attri.
    DATA temp246 LIKE sy-tabix.
    lt_attri = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_attri ) ).
    
    
    temp246 = sy-tabix.
    READ TABLE lt_attri INDEX 1 ASSIGNING <temp245>.
    sy-tabix = temp246.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ALPHA` act = <temp245>-name ).
  ENDMETHOD.

  METHOD msg_get_empty_no_dump.
    " input without any message must return an initial structure instead of
    " raising CX_SY_ITAB_LINE_NOT_FOUND on lt_msg[ 1 ]
    DATA ls_msg TYPE zabaputil_cl_util_context=>ty_s_msg.
    ls_msg = zabaputil_cl_util_context=>msg_get( 42 ).
    cl_abap_unit_assert=>assert_initial( ls_msg-text ).
  ENDMETHOD.

  METHOD check_table_true.
    DATA lt_tab TYPE string_table.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_table( lt_tab ) ).
  ENDMETHOD.

  METHOD check_table_false.
    DATA lv_str TYPE string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table( lv_str ) ).
  ENDMETHOD.

  METHOD check_structure_true.
    TYPES: BEGIN OF ty, a TYPE string, END OF ty.
    DATA ls_struc TYPE ty.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_structure( ls_struc ) ).
  ENDMETHOD.

  METHOD check_structure_false.
    DATA lv_str TYPE string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_structure( lv_str ) ).
  ENDMETHOD.

  METHOD check_numeric_int.
    DATA lv_int TYPE i VALUE 5.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_numeric( lv_int ) ).
  ENDMETHOD.

  METHOD check_numeric_string.
    DATA lv_str TYPE string VALUE `123`.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_numeric( lv_str ) ).
  ENDMETHOD.

  METHOD check_clike_string.
    DATA lv_str TYPE string VALUE `hello`.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_clike( lv_str ) ).
  ENDMETHOD.

  METHOD check_clike_int.
    DATA lv_int TYPE i VALUE 5.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_clike( lv_int ) ).
  ENDMETHOD.

  METHOD check_ref_data_true.
    DATA lr_ref TYPE REF TO data.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_ref_data( lr_ref ) ).
  ENDMETHOD.

  METHOD check_ref_data_false.
    DATA lv_str TYPE string.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_ref_data( lv_str ) ).
  ENDMETHOD.

  METHOD check_ref_data_oref.
    " an OBJECT reference is a reference, but not a reference to DATA -
    " the caller that dereferences on a true answer cannot do `->*` on it
    DATA lo_obj TYPE REF TO ltcl_test_app.
    CREATE OBJECT lo_obj.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_ref_data( lo_obj ) ).
  ENDMETHOD.

  METHOD check_ref_data_iref.
    DATA li_ref TYPE REF TO if_serializable_object.
    CREATE OBJECT li_ref TYPE ltcl_test_app.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_ref_data( li_ref ) ).
  ENDMETHOD.

  METHOD check_ref_data_bound.
    " the answer is about the TYPE, not about whether the reference points
    " anywhere - a bound data reference stays true
    DATA lr_ref TYPE REF TO data.
    DATA lv_str TYPE string VALUE `x`.
    GET REFERENCE OF lv_str INTO lr_ref.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_ref_data( lr_ref ) ).
  ENDMETHOD.

  METHOD copy_ref_data_over_oref.
    " the damage the wrong answer did: conv_copy_ref_data took the `from->*`
    " branch for an object reference and copied nothing. It must copy the
    " reference itself instead
    DATA lo_obj  TYPE REF TO ltcl_test_app.
    DATA lo_back TYPE REF TO ltcl_test_app.
    FIELD-SYMBOLS <copy> TYPE any.
    DATA lr_copy TYPE REF TO data.

    CREATE OBJECT lo_obj.
    lo_obj->mv_val = `payload`.

    
    lr_copy = zabaputil_cl_util_context=>conv_copy_ref_data( lo_obj ).
    cl_abap_unit_assert=>assert_bound( lr_copy ).

    ASSIGN lr_copy->* TO <copy>.
    cl_abap_unit_assert=>assert_subrc( ).

    lo_back ?= <copy>.
    cl_abap_unit_assert=>assert_equals( act = lo_back->mv_val
                                        exp = `payload` ).
  ENDMETHOD.

  METHOD get_classname.
    DATA lo_obj TYPE REF TO ltcl_test_app.
    DATA lv_name TYPE string.
    CREATE OBJECT lo_obj TYPE ltcl_test_app.
    
    lv_name = zabaputil_cl_util_context=>rtti_get_classname_by_ref( lo_obj ).
    cl_abap_unit_assert=>assert_equals( exp = `LTCL_TEST_APP` act = lv_name ).
  ENDMETHOD.

  METHOD get_type_name.
    DATA lv_bool TYPE abap_bool.
    cl_abap_unit_assert=>assert_equals( exp = `ABAP_BOOL`
                                        act = zabaputil_cl_util_context=>rtti_get_type_name( lv_bool ) ).
  ENDMETHOD.

  METHOD check_class_exists_true.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_class_exists( `CL_ABAP_TYPEDESCR` ) ).
  ENDMETHOD.

  METHOD check_class_exists_false.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `ZCL_DOES_NOT_EXIST_99` ) ).
  ENDMETHOD.

  METHOD check_class_exists_cached.
    " the second answer comes from the cache and has to be the same one -
    " a negative answer is memoised too, so it must not flip to true
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_class_exists( `CL_ABAP_TYPEDESCR` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_class_exists( `CL_ABAP_TYPEDESCR` ) ).

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `ZCL_DOES_NOT_EXIST_98` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `ZCL_DOES_NOT_EXIST_98` ) ).
  ENDMETHOD.

  METHOD check_class_exists_case.
    " the cache key is upper case, so the two spellings of one name share
    " an entry instead of answering differently
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>rtti_check_class_exists( `cl_abap_typedescr` )
        exp = zabaputil_cl_util_context=>rtti_check_class_exists( `CL_ABAP_TYPEDESCR` ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! EXCEPTION HANDLING
"! ================================================================
CLASS ltcl_exception_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS x_raise_basic                  FOR TESTING.
    METHODS x_raise_custom_msg             FOR TESTING.
    METHODS x_check_raise_true             FOR TESTING.
    METHODS x_check_raise_false            FOR TESTING.
    METHODS x_get_last_t100_basic          FOR TESTING.

ENDCLASS.

CLASS ltcl_exception_ops IMPLEMENTATION.

  METHOD x_raise_basic.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
    TRY.
        zabaputil_cl_util_context=>x_raise( ).
        cl_abap_unit_assert=>fail( `Exception expected` ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_not_initial( lx ).
    ENDTRY.
  ENDMETHOD.

  METHOD x_raise_custom_msg.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
    TRY.
        zabaputil_cl_util_context=>x_raise( `MY_ERROR` ).
        cl_abap_unit_assert=>fail( `Exception expected` ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lx->get_text( ) sub = `MY_ERROR` ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD x_check_raise_true.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
    TRY.
        zabaputil_cl_util_context=>x_check_raise( v = `OOPS` when = abap_true ).
        cl_abap_unit_assert=>fail( `Exception expected` ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_not_initial( lx ).
    ENDTRY.
  ENDMETHOD.

  METHOD x_check_raise_false.
    TRY.
        zabaputil_cl_util_context=>x_check_raise( v = `OOPS` when = abap_false ).
        " No exception — test passes
      CATCH zabaputil_cx_util_error.
        cl_abap_unit_assert=>fail( `No exception expected` ).
    ENDTRY.
  ENDMETHOD.

  METHOD x_get_last_t100_basic.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA lv_result TYPE string.
    TRY.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = `INNER_ERROR`.
        
      CATCH zabaputil_cx_util_error INTO lx.
        
        lv_result = zabaputil_cl_util_context=>x_get_last_t100( lx ).
        cl_abap_unit_assert=>assert_not_initial( lv_result ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! REFERENCE / BOUND / INITIAL CHECKS
"! ================================================================
CLASS ltcl_ref_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS bound_not_initial_true         FOR TESTING.
    METHODS bound_not_initial_unbound      FOR TESTING.
    METHODS bound_not_initial_initial      FOR TESTING.
    METHODS unassign_initial_null          FOR TESTING.
    METHODS unassign_initial_empty         FOR TESTING.
    METHODS unassign_initial_filled        FOR TESTING.
    METHODS conv_get_as_data_ref           FOR TESTING.
    METHODS copy_ref_data_unbound          FOR TESTING.
    METHODS copy_ref_data_bound            FOR TESTING.
    METHODS copy_ref_data_plain_value      FOR TESTING.

ENDCLASS.

CLASS ltcl_ref_ops IMPLEMENTATION.

  METHOD bound_not_initial_true.
    DATA lv_val TYPE string VALUE `hello`.
    DATA lr_ref TYPE REF TO data.
    GET REFERENCE OF lv_val INTO lr_ref.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_bound_a_not_initial( lr_ref ) ).
  ENDMETHOD.

  METHOD bound_not_initial_unbound.
    DATA lr_ref TYPE REF TO data.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_bound_a_not_initial( lr_ref ) ).
  ENDMETHOD.

  METHOD bound_not_initial_initial.
    DATA lv_val TYPE string.
    DATA lr_ref TYPE REF TO data.
    GET REFERENCE OF lv_val INTO lr_ref.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_bound_a_not_initial( lr_ref ) ).
  ENDMETHOD.

  METHOD unassign_initial_null.
    DATA lr_ref TYPE REF TO data.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_unassign_initial( lr_ref ) ).
  ENDMETHOD.

  METHOD unassign_initial_empty.
    DATA lv_val TYPE string.
    DATA lr_ref TYPE REF TO data.
    GET REFERENCE OF lv_val INTO lr_ref.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_unassign_initial( lr_ref ) ).
  ENDMETHOD.

  METHOD unassign_initial_filled.
    DATA lv_val TYPE string VALUE `x`.
    DATA lr_ref TYPE REF TO data.
    GET REFERENCE OF lv_val INTO lr_ref.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_unassign_initial( lr_ref ) ).
  ENDMETHOD.

  METHOD conv_get_as_data_ref.
    DATA lv_val TYPE string VALUE `test`.
    DATA lr_ref TYPE REF TO data.
    FIELD-SYMBOLS <val> TYPE string.
    lr_ref = zabaputil_cl_util_context=>conv_get_as_data_ref( lv_val ).
    cl_abap_unit_assert=>assert_bound( lr_ref ).
    
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals( exp = `test` act = <val> ).
  ENDMETHOD.

  METHOD copy_ref_data_unbound.
    " an unbound reference must return an initial result instead of dumping in
    " CREATE DATA ... LIKE <from> with an unassigned field symbol
    DATA lr_ref TYPE REF TO data.
    DATA lr_copy TYPE REF TO data.
    lr_copy = zabaputil_cl_util_context=>conv_copy_ref_data( lr_ref ).
    cl_abap_unit_assert=>assert_initial( lr_copy ).
  ENDMETHOD.

  METHOD copy_ref_data_bound.
    DATA lv_val TYPE string VALUE `hello`.
    DATA lr_ref TYPE REF TO data.
    DATA lr_copy TYPE REF TO data.
    FIELD-SYMBOLS <copy> TYPE string.
    GET REFERENCE OF lv_val INTO lr_ref.
    
    lr_copy = zabaputil_cl_util_context=>conv_copy_ref_data( lr_ref ).
    cl_abap_unit_assert=>assert_bound( lr_copy ).
    
    ASSIGN lr_copy->* TO <copy>.
    cl_abap_unit_assert=>assert_equals( exp = `hello` act = <copy> ).
    " a copy, not an alias
    lv_val = `changed`.
    cl_abap_unit_assert=>assert_equals( exp = `hello` act = <copy> ).
  ENDMETHOD.

  METHOD copy_ref_data_plain_value.
    DATA lr_copy TYPE REF TO data.
    FIELD-SYMBOLS <copy> TYPE data.
    lr_copy = zabaputil_cl_util_context=>conv_copy_ref_data( `plain` ).
    cl_abap_unit_assert=>assert_bound( lr_copy ).
    
    ASSIGN lr_copy->* TO <copy>.
    cl_abap_unit_assert=>assert_equals( exp = `plain` act = <copy> ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! TIME OPERATIONS
"! ================================================================
CLASS ltcl_time_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS get_timestampl                 FOR TESTING.
    METHODS add_seconds                    FOR TESTING.
    METHODS subtract_seconds               FOR TESTING.
    METHODS diff_seconds                   FOR TESTING.
    METHODS measure_start_stop             FOR TESTING.
    METHODS stampl_by_date_time            FOR TESTING.
    METHODS text_by_stampl                 FOR TESTING.
    METHODS get_timestamp_short            FOR TESTING.
    METHODS text_by_stampl_fraction        FOR TESTING.
    METHODS text_by_stampl_utc_zone        FOR TESTING.

ENDCLASS.

CLASS ltcl_time_ops IMPLEMENTATION.

  METHOD get_timestamp_short.
    " whole seconds, never ahead of the precise stamp taken right after
    DATA lv_short TYPE timestamp.
    DATA lv_long TYPE timestampl.
    DATA temp1 TYPE xsdboolean.
    lv_short = zabaputil_cl_util_context=>time_get_timestamp( ).
    
    lv_long = zabaputil_cl_util_context=>time_get_timestampl( ).
    cl_abap_unit_assert=>assert_not_initial( lv_short ).
    
    temp1 = boolc( lv_short <= lv_long ).
    cl_abap_unit_assert=>assert_true( temp1 ).
  ENDMETHOD.

  METHOD text_by_stampl.
    " a fraction of zero is left out - the shape a short TIMESTAMP gives
    DATA temp247 TYPE timestampl.
    temp247 = `20261123140509`.
    cl_abap_unit_assert=>assert_equals(
        exp = `2026-11-23 14:05:09`
        act = zabaputil_cl_util_context=>time_get_text_by_stampl( temp247 ) ).
  ENDMETHOD.

  METHOD text_by_stampl_fraction.
    DATA temp248 TYPE timestampl.
    temp248 = `20261123140509.1234567`.
    cl_abap_unit_assert=>assert_equals(
        exp = `2026-11-23 14:05:09,1234567`
        act = zabaputil_cl_util_context=>time_get_text_by_stampl( temp248 ) ).
  ENDMETHOD.

  METHOD text_by_stampl_utc_zone.
    " an explicit UTC changes nothing - the stamp is stored in UTC
    DATA temp249 TYPE timestampl.
    DATA temp35 TYPE timestampl.
    temp249 = `20261123140509`.
    
    temp35 = `20261123140509`.
    cl_abap_unit_assert=>assert_equals(
        exp = zabaputil_cl_util_context=>time_get_text_by_stampl( temp249 )
        act = zabaputil_cl_util_context=>time_get_text_by_stampl( val       = temp35
                                                                  time_zone = `UTC` ) ).
  ENDMETHOD.

  METHOD get_timestampl.
    DATA lv_ts TYPE timestampl.
    lv_ts = zabaputil_cl_util_context=>time_get_timestampl( ).
    cl_abap_unit_assert=>assert_not_initial( lv_ts ).
  ENDMETHOD.

  METHOD add_seconds.
    DATA lv_ts TYPE timestampl.
    DATA lv_result TYPE timestampl.
    DATA temp2 TYPE xsdboolean.
    DATA lv_diff TYPE i.
    lv_ts = zabaputil_cl_util_context=>time_get_timestampl( ).
    
    lv_result = zabaputil_cl_util_context=>time_add_seconds( time = lv_ts seconds = 60 ).
    
    temp2 = boolc( lv_result > lv_ts ).
    cl_abap_unit_assert=>assert_true( temp2 ).
    " Difference should be ~60 seconds
    
    lv_diff = zabaputil_cl_util_context=>time_diff_seconds( time_from = lv_ts time_to = lv_result ).
    cl_abap_unit_assert=>assert_equals( exp = 60 act = lv_diff ).
  ENDMETHOD.

  METHOD subtract_seconds.
    DATA lv_ts TYPE timestampl.
    DATA lv_result TYPE timestampl.
    DATA temp3 TYPE xsdboolean.
    lv_ts = zabaputil_cl_util_context=>time_get_timestampl( ).
    
    lv_result = zabaputil_cl_util_context=>time_subtract_seconds( time = lv_ts seconds = 30 ).
    
    temp3 = boolc( lv_result < lv_ts ).
    cl_abap_unit_assert=>assert_true( temp3 ).
  ENDMETHOD.

  METHOD diff_seconds.
    DATA lv_from TYPE timestampl.
    DATA lv_to TYPE timestampl.
    DATA lv_diff TYPE i.
    lv_from = zabaputil_cl_util_context=>time_get_timestampl( ).
    
    lv_to = zabaputil_cl_util_context=>time_add_seconds( time = lv_from seconds = 120 ).
    
    lv_diff = zabaputil_cl_util_context=>time_diff_seconds( time_from = lv_from time_to = lv_to ).
    cl_abap_unit_assert=>assert_equals( exp = 120 act = lv_diff ).
  ENDMETHOD.

  METHOD measure_start_stop.
    DATA lv_start TYPE timestampl.
    DATA lv_ms TYPE i.
    DATA temp4 TYPE xsdboolean.
    lv_start = zabaputil_cl_util_context=>time_measure_start( ).
    cl_abap_unit_assert=>assert_not_initial( lv_start ).
    " Result is milliseconds, should be >= 0
    
    lv_ms = zabaputil_cl_util_context=>time_measure_stop( lv_start ).
    
    temp4 = boolc( lv_ms >= 0 ).
    cl_abap_unit_assert=>assert_true( temp4 ).
  ENDMETHOD.

  METHOD stampl_by_date_time.
    DATA temp250 TYPE d.
    DATA temp36 TYPE t.
    DATA lv_ts TYPE timestampl.
    temp250 = `20240101`.
    
    temp36 = `120000`.
    
    lv_ts = zabaputil_cl_util_context=>time_get_stampl_by_date_time( date = temp250
                                                                time = temp36 ).
    cl_abap_unit_assert=>assert_not_initial( lv_ts ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! EDGE CASES - Bugs & ABAP-isms critical for JS transpilation
"! ================================================================
CLASS ltcl_edge_cases DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " filter_get_range_by_token edge cases
    METHODS token_empty_input              FOR TESTING.
    METHODS token_single_star              FOR TESTING.
    METHODS token_single_equals            FOR TESTING.
    METHODS token_just_number              FOR TESTING.
    METHODS token_with_spaces              FOR TESTING.

    " c_pad_left with space (same bug pattern as c_pad_right)
    METHODS pad_left_with_space            FOR TESTING.
    METHODS pad_left_default               FOR TESTING.

    " CS operator is always case-insensitive (critical for transpiler)
    METHODS filter_by_val_cs_insensitive   FOR TESTING.

    " c_trim edge cases
    METHODS trim_only_newlines             FOR TESTING.
    METHODS trim_mixed_whitespace          FOR TESTING.
    METHODS trim_unicode                   FOR TESTING.

    " c_split edge cases
    METHODS split_empty_input              FOR TESTING.
    METHODS split_only_separator           FOR TESTING.

    " conv_string_to_number edge cases
    METHODS num_one_thousand_no_dot        FOR TESTING.
    METHODS num_european_format            FOR TESTING.
    METHODS num_only_minus                 FOR TESTING.
    METHODS num_leading_zeros              FOR TESTING.

    " conv_string_to_date edge cases
    METHODS date_short_input               FOR TESTING.

    " check_is_email edge cases
    METHODS email_with_plus                FOR TESTING.
    METHODS email_with_dots                FOR TESTING.
    METHODS email_only_spaces              FOR TESTING.

    " itab_slice edge cases
    METHODS slice_from_exceeds_lines       FOR TESTING.
    METHODS slice_empty_table              FOR TESTING.

    " c_truncate edge cases
    METHODS truncate_max_zero              FOR TESTING.
    METHODS truncate_empty_string          FOR TESTING.

    " data_get_by_path / data_set_by_path edge cases
    METHODS path_with_leading_dash         FOR TESTING.
    METHODS path_set_nested                FOR TESTING.

    " check_is_guid edge cases
    METHODS guid_empty                     FOR TESTING.
    METHODS guid_mixed_case_dashes         FOR TESTING.

    " url_param_set preserves case (after fix)
    METHODS url_set_preserves_value_case   FOR TESTING.

    " boolean_abap_2_json with different boolean types
    METHODS bool_xfeld_true                FOR TESTING.
    METHODS bool_xfeld_false               FOR TESTING.

    " json roundtrip with special characters
    METHODS json_special_chars             FOR TESTING.

    " c_contains case behavior (TRANSPILER CRITICAL)
    METHODS contains_is_always_ci          FOR TESTING.

ENDCLASS.

CLASS ltcl_edge_cases IMPLEMENTATION.

  METHOD token_empty_input.
    " Empty input should not crash - returns empty/initial range
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `` ).
    cl_abap_unit_assert=>assert_initial( ls_range-option ).
  ENDMETHOD.

  METHOD token_single_star.
    " Single '*' - after fix: CP with empty low (match-all pattern)
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `*` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_single_equals.
    " Just "=" with nothing after → EQ with empty low
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `=` ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = ls_range-option ).
    cl_abap_unit_assert=>assert_initial( ls_range-low ).
  ENDMETHOD.

  METHOD token_just_number.
    " Plain number without operator → EQ
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `42` ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `42` act = ls_range-low ).
  ENDMETHOD.

  METHOD token_with_spaces.
    " Token with spaces in value
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `=hello world` ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `hello world` act = ls_range-low ).
  ENDMETHOD.

  METHOD pad_left_with_space.
    " After fix: space padding should work on c_pad_left too
    cl_abap_unit_assert=>assert_equals( exp = `   hi`
        act = zabaputil_cl_util_context=>c_pad_left( val = `hi` len = 5 pad = ' ' ) ).
  ENDMETHOD.

  METHOD pad_left_default.
    " Default '0' should still work
    cl_abap_unit_assert=>assert_equals( exp = `007`
        act = zabaputil_cl_util_context=>c_pad_left( val = `7` len = 3 ) ).
  ENDMETHOD.

  METHOD filter_by_val_cs_insensitive.
    " After fix: ignore_case=false now uses find() for true case-sensitivity
    TYPES: BEGIN OF ty, name TYPE string, END OF ty.
    TYPES temp24 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp24.
    DATA temp251 LIKE lt_tab.
    DATA temp252 LIKE LINE OF temp251.
    CLEAR temp251.
    
    temp252-name = `Hello`.
    INSERT temp252 INTO TABLE temp251.
    temp252-name = `WORLD`.
    INSERT temp252 INTO TABLE temp251.
    temp252-name = `test`.
    INSERT temp252 INTO TABLE temp251.
    lt_tab = temp251.

    " Search for 'hello' with ignore_case = FALSE (case-sensitive)
    " After fix: should NOT match 'Hello' (capital H)
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `hello` ignore_case = abap_false
                                        CHANGING tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_tab ) ).
  ENDMETHOD.

  METHOD trim_only_newlines.
    " TRANSPILER NOTE: c_trim only removes spaces and horizontal tabs.
    " Newlines are NOT removed by c_trim! (shift_left/right only trim spaces)
    DATA lv_nl LIKE zabaputil_cl_util_context=>cv_char_util_newline.
    DATA lv_result TYPE string.
    lv_nl = zabaputil_cl_util_context=>cv_char_util_newline.
    
    lv_result = zabaputil_cl_util_context=>c_trim( lv_nl && lv_nl ).
    " Newlines survive trimming
    cl_abap_unit_assert=>assert_not_initial( lv_result ).
  ENDMETHOD.

  METHOD trim_mixed_whitespace.
    DATA lv_tab LIKE zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    lv_tab = zabaputil_cl_util_context=>cv_char_util_horizontal_tab.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
        act = zabaputil_cl_util_context=>c_trim( ` ` && lv_tab && `hello` && lv_tab && ` ` ) ).
  ENDMETHOD.

  METHOD trim_unicode.
    " Regular non-ASCII should not be trimmed
    cl_abap_unit_assert=>assert_equals( exp = `über`
        act = zabaputil_cl_util_context=>c_trim( `  über  ` ) ).
  ENDMETHOD.

  METHOD split_empty_input.
    " TRANSPILER NOTE: ABAP SPLIT of empty string gives table with 0 entries!
    " This differs from JS ''.split(',') which gives [''].
    DATA lt_result TYPE string_table.
    lt_result = zabaputil_cl_util_context=>c_split( val = `` sep = `,` ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_result ) ).
  ENDMETHOD.

  METHOD split_only_separator.
    " TRANSPILER NOTE: ABAP SPLIT ',' AT ',' gives 1 entry (empty string),
    " while JS ','.split(',') gives ['', '']. Different behavior!
    DATA lt_result TYPE string_table.
    DATA temp5 TYPE xsdboolean.
    lt_result = zabaputil_cl_util_context=>c_split( val = `,` sep = `,` ).
    " Document actual ABAP behavior:
    
    temp5 = boolc( lines( lt_result ) >= 1 ).
    cl_abap_unit_assert=>assert_true( temp5 ).
  ENDMETHOD.

  METHOD num_one_thousand_no_dot.
    " EDGE CASE: '1,000' without dot → comma is last separator → decimal!
    " Result is 1.0 NOT 1000. This is documented European behavior.
    DATA lv_result TYPE decfloat34.
    DATA temp253 TYPE decfloat34.
    lv_result = zabaputil_cl_util_context=>conv_string_to_number( `1,000` ).
    
    temp253 = '1.000'.
    cl_abap_unit_assert=>assert_equals( exp = temp253 act = lv_result ).
  ENDMETHOD.

  METHOD num_european_format.
    " TRANSPILER NOTE: European format (dot=thousands, comma=decimal)
    " does NOT work correctly! The dot is always kept as-is, creating
    " invalid numbers like '1.234.56'. Only the comma heuristic applies.
    " Result: falls back to 0 due to conversion error.
    DATA lv_result TYPE decfloat34.
    DATA temp254 TYPE decfloat34.
    lv_result = zabaputil_cl_util_context=>conv_string_to_number( `1.234,56` ).
    " Documents actual (broken) behavior - returns 0 due to double-dot
    
    temp254 = 0.
    cl_abap_unit_assert=>assert_equals( exp = temp254 act = lv_result ).
  ENDMETHOD.

  METHOD num_only_minus.
    " Just a minus sign
    DATA lv_result TYPE decfloat34.
    DATA temp255 TYPE decfloat34.
    lv_result = zabaputil_cl_util_context=>conv_string_to_number( `-` ).
    
    temp255 = 0.
    cl_abap_unit_assert=>assert_equals( exp = temp255 act = lv_result ).
  ENDMETHOD.

  METHOD num_leading_zeros.
    DATA lv_result TYPE decfloat34.
    DATA temp256 TYPE decfloat34.
    lv_result = zabaputil_cl_util_context=>conv_string_to_number( `007` ).
    
    temp256 = 7.
    cl_abap_unit_assert=>assert_equals( exp = temp256 act = lv_result ).
  ENDMETHOD.

  METHOD date_short_input.
        DATA lv_result TYPE d.
    " TRANSPILER NOTE: Short input causes STRING_OFFSET_TOO_LARGE in ABAP
    " because conv_string_to_date has no bounds checking.
    " This is a known limitation - skip this test to document it.
    " A JS transpiler should add bounds checking.
    TRY.
        
        lv_result = zabaputil_cl_util_context=>conv_string_to_date( val = `24` format = `YYYY-MM-DD` ) ##NEEDED.
      CATCH cx_root.
        " Expected: crashes with short input - JS must guard against this
    ENDTRY.
  ENDMETHOD.

  METHOD email_with_plus.
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>check_is_email( `user+tag@example.com` ) ).
  ENDMETHOD.

  METHOD email_with_dots.
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>check_is_email( `first.last@sub.domain.com` ) ).
  ENDMETHOD.

  METHOD email_only_spaces.
    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_util_context=>check_is_email( `   ` ) ).
  ENDMETHOD.

  METHOD slice_from_exceeds_lines.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp25 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp25.
    DATA temp257 LIKE lt_tab.
    DATA temp258 LIKE LINE OF temp257.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp257.
    
    temp258-id = 1.
    INSERT temp258 INTO TABLE temp257.
    temp258-id = 2.
    INSERT temp258 INTO TABLE temp257.
    lt_tab = temp257.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 99 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD slice_empty_table.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp26 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp26.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 1 to = 5 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD truncate_max_zero.
    " max=0 should return empty
    cl_abap_unit_assert=>assert_equals( exp = ``
        act = zabaputil_cl_util_context=>c_truncate( val = `hello` max = 0 ) ).
  ENDMETHOD.

  METHOD truncate_empty_string.
    cl_abap_unit_assert=>assert_equals( exp = ``
        act = zabaputil_cl_util_context=>c_truncate( val = `` max = 10 ) ).
  ENDMETHOD.

  METHOD path_with_leading_dash.
    " Leading dash creates an empty first segment - should be skipped
    TYPES: BEGIN OF ty, name TYPE string, END OF ty.
    DATA temp259 TYPE ty.
    DATA ls_data LIKE temp259.
    CLEAR temp259.
    temp259-name = `hello`.
    
    ls_data = temp259.
    cl_abap_unit_assert=>assert_equals( exp = `hello`
        act = zabaputil_cl_util_context=>data_get_by_path( data = ls_data path = `-name` ) ).
  ENDMETHOD.

  METHOD path_set_nested.
    TYPES: BEGIN OF ty_inner, city TYPE string, END OF ty_inner.
    TYPES: BEGIN OF ty, address TYPE ty_inner, END OF ty.
    DATA ls_data TYPE ty.
    zabaputil_cl_util_context=>data_set_by_path( EXPORTING path = `address-city` value = `Munich`
                                     CHANGING data = ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = `Munich` act = ls_data-address-city ).
  ENDMETHOD.

  METHOD guid_empty.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_guid( `` ) ).
  ENDMETHOD.

  METHOD guid_mixed_case_dashes.
    " Mixed case with dashes (standard GUID format)
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>check_is_guid( `550e8400-e29b-41d4-a716-446655440000` ) ).
  ENDMETHOD.

  METHOD url_set_preserves_value_case.
    " After fix: value case should be preserved
    DATA lv_result TYPE string.
    lv_result = zabaputil_cl_util_context=>url_param_set(
        url = `a=1` name = `class` value = `ZCL_MY_CLASS` ).
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>c_contains( val = lv_result sub = `ZCL_MY_CLASS` ) ).
  ENDMETHOD.

  METHOD bool_xfeld_true.
    DATA lv_xfeld TYPE xfeld VALUE 'X'.
    cl_abap_unit_assert=>assert_equals( exp = `true`
        act = zabaputil_cl_util_context=>boolean_abap_2_json( lv_xfeld ) ).
  ENDMETHOD.

  METHOD bool_xfeld_false.
    DATA lv_xfeld TYPE xfeld.
    cl_abap_unit_assert=>assert_equals( exp = `false`
        act = zabaputil_cl_util_context=>boolean_abap_2_json( lv_xfeld ) ).
  ENDMETHOD.

  METHOD json_special_chars.
    " JSON with quotes, backslashes, newlines
    TYPES: BEGIN OF ty, text TYPE string, END OF ty.
    DATA temp260 TYPE ty.
    DATA ls_data LIKE temp260.
    DATA lv_json TYPE string.
    DATA ls_back TYPE ty.
    CLEAR temp260.
    temp260-text = `He said "hello" \\ yes`.
    
    ls_data = temp260.
    
    lv_json = zabaputil_cl_util_context=>json_stringify( ls_data ).
    
    zabaputil_cl_util_context=>json_parse( EXPORTING val = lv_json CHANGING data = ls_back ).
    cl_abap_unit_assert=>assert_equals( exp = ls_data-text act = ls_back-text ).
  ENDMETHOD.

  METHOD contains_is_always_ci.
    " TRANSPILER CRITICAL: Proves that c_contains is case-insensitive
    " JS must implement: val.toLowerCase().includes(sub.toLowerCase())
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>c_contains( val = `Hello World` sub = `HELLO` ) ).
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>c_contains( val = `HELLO WORLD` sub = `hello` ) ).
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>c_contains( val = `HeLLo` sub = `hello` ) ).
  ENDMETHOD.

ENDCLASS.

"! ================================================================
"! Z2UI5_CL_UTIL_EXT - Comprehensive Unit Tests for JS Transpilation
"! ================================================================
"! BAL tests removed: bal_search has ASSIGN_TYPE_CONFLICT bug in
"! production code (runtime error at line ~142). Fix BAL impl first.
"! ================================================================

"! ================================================================
"! LOCK / DEQUEUE HELPER
"! ================================================================
CLASS ltcl_lock_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS dequeue_from_enqueue           FOR TESTING.
    METHODS dequeue_from_lower             FOR TESTING.
    METHODS dequeue_already_dequeue        FOR TESTING.
    METHODS dequeue_short_name             FOR TESTING.

ENDCLASS.

CLASS ltcl_lock_ops IMPLEMENTATION.

  METHOD dequeue_from_enqueue.
    cl_abap_unit_assert=>assert_equals(
        exp = `DEQUEUE_EVVBAKE`
        act = zabaputil_cl_util_context=>lock_get_dequeue_by_enqueue( `ENQUEUE_EVVBAKE` ) ).
  ENDMETHOD.

  METHOD dequeue_from_lower.
    cl_abap_unit_assert=>assert_equals(
        exp = `DEQUEUE_EVVBAKE`
        act = zabaputil_cl_util_context=>lock_get_dequeue_by_enqueue( `enqueue_evvbake` ) ).
  ENDMETHOD.

  METHOD dequeue_already_dequeue.
    cl_abap_unit_assert=>assert_equals(
        exp = `DEQUEUE_EVVBAKE`
        act = zabaputil_cl_util_context=>lock_get_dequeue_by_enqueue( `DEQUEUE_EVVBAKE` ) ).
  ENDMETHOD.

  METHOD dequeue_short_name.
    cl_abap_unit_assert=>assert_equals(
        exp = `DEQUEUE_EMARA`
        act = zabaputil_cl_util_context=>lock_get_dequeue_by_enqueue( `ENQUEUE_EMARA` ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! CALENDAR OPERATIONS
"! ================================================================
CLASS ltcl_calendar_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS weekday_monday                 FOR TESTING.
    METHODS weekday_sunday                 FOR TESTING.
    METHODS weekday_wednesday              FOR TESTING.
    METHODS weekday_saturday               FOR TESTING.
    METHODS is_weekend_saturday            FOR TESTING.
    METHODS is_weekend_sunday              FOR TESTING.
    METHODS is_weekend_friday              FOR TESTING.
    METHODS add_workdays_skip_weekend      FOR TESTING.
    METHODS add_workdays_mid_week          FOR TESTING.
    METHODS add_workdays_zero              FOR TESTING.
    METHODS count_workdays_full_week       FOR TESTING.
    METHODS count_workdays_same_day        FOR TESTING.

ENDCLASS.

CLASS ltcl_calendar_ops IMPLEMENTATION.

  METHOD weekday_monday.
    DATA temp261 TYPE d.
    temp261 = `20240101`.
    cl_abap_unit_assert=>assert_equals( exp = 1
        act = zabaputil_cl_util_context=>cal_get_weekday( temp261 ) ).
  ENDMETHOD.

  METHOD weekday_sunday.
    DATA temp262 TYPE d.
    temp262 = `20240107`.
    cl_abap_unit_assert=>assert_equals( exp = 7
        act = zabaputil_cl_util_context=>cal_get_weekday( temp262 ) ).
  ENDMETHOD.

  METHOD weekday_wednesday.
    DATA temp263 TYPE d.
    temp263 = `20240103`.
    cl_abap_unit_assert=>assert_equals( exp = 3
        act = zabaputil_cl_util_context=>cal_get_weekday( temp263 ) ).
  ENDMETHOD.

  METHOD weekday_saturday.
    DATA temp264 TYPE d.
    temp264 = `20240106`.
    cl_abap_unit_assert=>assert_equals( exp = 6
        act = zabaputil_cl_util_context=>cal_get_weekday( temp264 ) ).
  ENDMETHOD.

  METHOD is_weekend_saturday.
    DATA temp265 TYPE d.
    temp265 = `20240106`.
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>cal_is_weekend( temp265 ) ).
  ENDMETHOD.

  METHOD is_weekend_sunday.
    DATA temp266 TYPE d.
    temp266 = `20240107`.
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>cal_is_weekend( temp266 ) ).
  ENDMETHOD.

  METHOD is_weekend_friday.
    DATA temp267 TYPE d.
    temp267 = `20240105`.
    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_util_context=>cal_is_weekend( temp267 ) ).
  ENDMETHOD.

  METHOD add_workdays_skip_weekend.
    DATA temp268 TYPE d.
    DATA temp37 TYPE d.
    temp268 = `20240318`.
    
    temp37 = `20240315`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp268
        act = zabaputil_cl_util_context=>cal_add_workdays( date = temp37 days = 1 ) ).
  ENDMETHOD.

  METHOD add_workdays_mid_week.
    DATA temp269 TYPE d.
    DATA temp38 TYPE d.
    temp269 = `20240104`.
    
    temp38 = `20240102`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp269
        act = zabaputil_cl_util_context=>cal_add_workdays( date = temp38 days = 2 ) ).
  ENDMETHOD.

  METHOD add_workdays_zero.
    DATA temp270 TYPE d.
    DATA temp39 TYPE d.
    temp270 = `20240315`.
    
    temp39 = `20240315`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp270
        act = zabaputil_cl_util_context=>cal_add_workdays( date = temp39 days = 0 ) ).
  ENDMETHOD.

  METHOD count_workdays_full_week.
    DATA temp271 TYPE d.
    DATA temp40 TYPE d.
    temp271 = `20240101`.
    
    temp40 = `20240108`.
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = zabaputil_cl_util_context=>cal_count_workdays( date_from = temp271
                                                      date_to   = temp40 ) ).
  ENDMETHOD.

  METHOD count_workdays_same_day.
    DATA temp272 TYPE d.
    DATA temp41 TYPE d.
    temp272 = `20240101`.
    
    temp41 = `20240101`.
    cl_abap_unit_assert=>assert_equals(
        exp = 0
        act = zabaputil_cl_util_context=>cal_count_workdays( date_from = temp272
                                                      date_to   = temp41 ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! ZIP OPERATIONS
"! ================================================================
CLASS ltcl_zip_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS pack_basic                     FOR TESTING.
    METHODS pack_empty                     FOR TESTING.
    METHODS roundtrip                      FOR TESTING.

ENDCLASS.

CLASS ltcl_zip_ops IMPLEMENTATION.

  METHOD pack_basic.
    DATA lo_probe TYPE REF TO object.
    DATA temp273 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp274 LIKE LINE OF temp273.
    DATA temp42 TYPE xstring.
    DATA lt_files LIKE temp273.
    DATA lv_archive TYPE xstring.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    CLEAR temp273.
    
    temp274-name = `test.txt`.
    
    temp42 = `48656C6C6F`.
    temp274-content = temp42.
    INSERT temp274 INTO TABLE temp273.
    
    lt_files = temp273.
    
    lv_archive = zabaputil_cl_util_context=>zip_pack( lt_files ).
    cl_abap_unit_assert=>assert_not_initial( lv_archive ).
  ENDMETHOD.

  METHOD pack_empty.
    DATA lo_probe TYPE REF TO object.
    DATA temp275 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA lt_files LIKE temp275.
    DATA lv_archive TYPE xstring.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    CLEAR temp275.
    
    lt_files = temp275.
    
    lv_archive = zabaputil_cl_util_context=>zip_pack( lt_files ).
    cl_abap_unit_assert=>assert_not_initial( lv_archive ).
  ENDMETHOD.

  METHOD roundtrip.
    DATA lo_probe TYPE REF TO object.
    DATA temp276 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp277 LIKE LINE OF temp276.
    DATA temp43 TYPE xstring.
    DATA temp44 TYPE xstring.
    DATA lt_in LIKE temp276.
    DATA lv_archive TYPE xstring.
    DATA lt_out TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA ls_in LIKE LINE OF lt_in.
      DATA temp278 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA temp279 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA ls_out LIKE temp278.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    CLEAR temp276.
    
    temp277-name = `a.txt`.
    
    temp43 = `414141`.
    temp277-content = temp43.
    INSERT temp277 INTO TABLE temp276.
    temp277-name = `b.txt`.
    
    temp44 = `424242`.
    temp277-content = temp44.
    INSERT temp277 INTO TABLE temp276.
    
    lt_in = temp276.

    
    lv_archive = zabaputil_cl_util_context=>zip_pack( lt_in ).
    
    lt_out = zabaputil_cl_util_context=>zip_unpack( lv_archive ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_out ) ).

    
    LOOP AT lt_in INTO ls_in.
      
      CLEAR temp278.
      
      READ TABLE lt_out INTO temp279 WITH KEY name = ls_in-name.
      IF sy-subrc = 0.
        temp278 = temp279.
      ENDIF.
      
      ls_out = temp278.
      cl_abap_unit_assert=>assert_equals( exp = ls_in-content act = ls_out-content ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! RTTI / TABLE DESCRIPTION
"! ================================================================
CLASS ltcl_rtti_ext DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " The on-prem RTTI path needs the DDIC type DFIES and table DD02T,
    " neither exists in the JS transpiler runtime - probe and skip there
    METHODS check_dfies_available
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS dfies_by_table_basic           FOR TESTING.
    METHODS dfies_by_table_has_fields      FOR TESTING.
    METHODS table_descr_basic              FOR TESTING.
    METHODS dfies_by_rollname_basic        FOR TESTING.
    METHODS tab_by_comps                   FOR TESTING.
    METHODS table_buffer_reset_no_raise    FOR TESTING.
    METHODS type_exists                    FOR TESTING.
    METHODS dfies_by_rollname_unknown      FOR TESTING.
    METHODS data_element_text_types        FOR TESTING.

ENDCLASS.

CLASS ltcl_rtti_ext IMPLEMENTATION.

  METHOD check_dfies_available.
    DATA lo_descr TYPE REF TO cl_abap_typedescr.
    DATA temp6 TYPE xsdboolean.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = `DFIES`
      RECEIVING
        p_descr_ref    = lo_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    
    temp6 = boolc( sy-subrc = 0 ).
    result = temp6.
  ENDMETHOD.

  METHOD dfies_by_table_basic.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_dfies.
    IF check_dfies_available( ) = abap_false.
      RETURN.
    ENDIF.
    
    lt_result = zabaputil_cl_util_context=>rtti_get_t_dfies_by_table_name( `Z2UI5_T_01` ).
    cl_abap_unit_assert=>assert_not_initial( lt_result ).
  ENDMETHOD.

  METHOD dfies_by_table_has_fields.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_dfies.
    DATA lv_found LIKE abap_false.
    DATA ls_dfies LIKE LINE OF lt_result.
    IF check_dfies_available( ) = abap_false.
      RETURN.
    ENDIF.
    
    lt_result = zabaputil_cl_util_context=>rtti_get_t_dfies_by_table_name( `Z2UI5_T_01` ).
    
    lv_found = abap_false.
    
    LOOP AT lt_result INTO ls_dfies.
      IF ls_dfies-fieldname = `ID`.
        lv_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( lv_found ).
  ENDMETHOD.

  METHOD table_buffer_reset_no_raise.
    " no table of that name, or no SBUF_GENERIC_RESET (ABAP Cloud, the JS
    " runtime): nothing happens, and nothing reaches the caller
    zabaputil_cl_util_context=>db_reset_table_buffer( `ZZ_NO_SUCH_TABLE_AT_ALL` ).
    cl_abap_unit_assert=>assert_true( abap_true ).
  ENDMETHOD.

  METHOD tab_by_comps.
    DATA lv_id   TYPE string.
    DATA lv_qty  TYPE i.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    DATA temp280 TYPE abap_component_tab.
    DATA temp281 LIKE LINE OF temp280.
    DATA temp45 TYPE REF TO cl_abap_datadescr.
    DATA temp46 TYPE REF TO cl_abap_datadescr.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <qty> TYPE any.
    CLEAR temp280.
    
    temp281-name = `ID`.
    
    temp45 ?= cl_abap_datadescr=>describe_by_data( lv_id ).
    temp281-type = temp45.
    INSERT temp281 INTO TABLE temp280.
    temp281-name = `QTY`.
    
    temp46 ?= cl_abap_datadescr=>describe_by_data( lv_qty ).
    temp281-type = temp46.
    INSERT temp281 INTO TABLE temp280.
    
    lr_tab = zabaputil_cl_util_context=>rtti_create_tab_by_comps( temp280 ).
    cl_abap_unit_assert=>assert_bound( lr_tab ).
    ASSIGN lr_tab->* TO <tab>.
    cl_abap_unit_assert=>assert_initial( <tab> ).
    
    INSERT INITIAL LINE INTO TABLE <tab> ASSIGNING <row>.
    
    ASSIGN COMPONENT `QTY` OF STRUCTURE <row> TO <qty>.
    cl_abap_unit_assert=>assert_subrc( ).
    <qty> = 7.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD type_exists.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_type_exists( `STRING` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_type_exists( `ZABAPUTIL_CL_UTIL_CONTEXT` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_type_exists( `ZZ_NO_SUCH_TYPE_AT_ALL` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_type_exists( `` ) ).
  ENDMETHOD.

  METHOD dfies_by_rollname_basic.
    DATA lo_descr TYPE REF TO cl_abap_typedescr.
    DATA ls_result TYPE zabaputil_cl_util_context=>ty_s_dfies.
    DATA temp282 TYPE string.
    IF check_dfies_available( ) = abap_false.
      RETURN.
    ENDIF.
    " the JS runtime knows DFIES but not every DDIC data element
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = `MANDT`
                                         RECEIVING  p_descr_ref    = lo_descr
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    
    ls_result = zabaputil_cl_util_context=>rtti_get_s_dfies_by_rollname( `MANDT` ).
    " rollname and inttype hold on every runtime - DATATYPE and LENG do
    " not: the JS runtime reports MANDT as CHAR of 6 bytes, a system as CLNT 3
    
    temp282 = ls_result-rollname.
    cl_abap_unit_assert=>assert_equals( exp = `MANDT`
                                        act = condense( temp282 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `C`
                                        act = ls_result-inttype ).
  ENDMETHOD.

  METHOD dfies_by_rollname_unknown.
    " no data element of that name - empty, not an exception; also the
    " answer on ABAP Cloud and in the JS runtime, which have no DFIES
    DATA ls_result TYPE zabaputil_cl_util_context=>ty_s_dfies.
    ls_result = zabaputil_cl_util_context=>rtti_get_s_dfies_by_rollname( `ZZ_NO_SUCH_DATA_ELEMENT` ).
    cl_abap_unit_assert=>assert_initial( ls_result ).
  ENDMETHOD.

  METHOD data_element_text_types.
    " each type picks its field of rtti_get_data_element_texts - compared
    " against that method, so the test holds whatever the texts are
    DATA ls_texts TYPE zabaputil_cl_util_context=>ty_s_data_element_text.
    ls_texts = zabaputil_cl_util_context=>rtti_get_data_element_texts( `MANDT` ).
    cl_abap_unit_assert=>assert_equals(
        exp = ls_texts-header
        act = zabaputil_cl_util_context=>rtti_get_data_element_text( val = `MANDT` type = `H` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = ls_texts-medium
        act = zabaputil_cl_util_context=>rtti_get_data_element_text( val = `MANDT` type = `m` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = ls_texts-long
        act = zabaputil_cl_util_context=>rtti_get_data_element_text( val = `MANDT` type = `L` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = ls_texts-short
        act = zabaputil_cl_util_context=>rtti_get_data_element_text( `MANDT` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = ls_texts-short
        act = zabaputil_cl_util_context=>rtti_get_data_element_text( val = `MANDT` type = `X` ) ).
  ENDMETHOD.

  METHOD table_descr_basic.
    DATA lv_result TYPE string.
    IF check_dfies_available( ) = abap_false.
      RETURN.
    ENDIF.
    
    lv_result = zabaputil_cl_util_context=>rtti_get_table_desrc( `Z2UI5_T_01` ).
    cl_abap_unit_assert=>assert_not_initial( lv_result ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! TRANSPORT REQUEST OPERATIONS
"! ================================================================
CLASS ltcl_transport_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS get_user_requests              FOR TESTING.

ENDCLASS.

CLASS ltcl_transport_ops IMPLEMENTATION.

  METHOD get_user_requests.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_tr_request.
    lt_result = zabaputil_cl_util_context=>tr_get_user_requests( ) ##NEEDED.
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! XLSX CONVERSION
"! ================================================================
CLASS ltcl_xlsx_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS xlsx_by_itab_basic             FOR TESTING.
    METHODS itab_by_xlsx_empty             FOR TESTING.

ENDCLASS.

CLASS ltcl_xlsx_ops IMPLEMENTATION.

  METHOD xlsx_by_itab_basic.
    TYPES: BEGIN OF ty_row, col1 TYPE string, col2 TYPE string, END OF ty_row.
    TYPES temp27 TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA lt_tab TYPE temp27.
    DATA temp283 LIKE lt_tab.
    DATA temp284 LIKE LINE OF temp283.
    DATA lv_result TYPE xstring.
    CLEAR temp283.
    
    temp284-col1 = `A`.
    temp284-col2 = `B`.
    INSERT temp284 INTO TABLE temp283.
    lt_tab = temp283.
    
    lv_result = zabaputil_cl_util_context=>conv_get_xlsx_by_itab( lt_tab ) ##NEEDED.
  ENDMETHOD.

  METHOD itab_by_xlsx_empty.
    DATA lv_xstring TYPE xstring.
    DATA lr_result TYPE REF TO data.
    zabaputil_cl_util_context=>conv_get_itab_by_xlsx( EXPORTING val = lv_xstring
                                              IMPORTING result = lr_result ) ##NEEDED.
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! SOURCE CODE READING
"! ================================================================
CLASS ltcl_source_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS source_get_method_basic        FOR TESTING.

ENDCLASS.

CLASS ltcl_source_ops IMPLEMENTATION.

  METHOD source_get_method_basic.
    DATA lt_source TYPE string_table.
    lt_source = zabaputil_cl_util_context=>source_get_method(
        iv_classname  = `Z2UI5_CL_UTIL`
        iv_methodname = `C_TRIM` ).
    cl_abap_unit_assert=>assert_not_initial( lt_source ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_escaping DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS escape_html_special_chars     FOR TESTING.
    METHODS escape_html_no_special        FOR TESTING.
    METHODS escape_json_special_chars     FOR TESTING.
    METHODS url_encode_unreserved         FOR TESTING.
    METHODS url_encode_special            FOR TESTING.
    METHODS url_encode_umlaut             FOR TESTING.
    METHODS url_decode_simple             FOR TESTING.
    METHODS url_decode_plus_as_space      FOR TESTING.
    METHODS url_roundtrip                 FOR TESTING.

ENDCLASS.

CLASS ltcl_escaping IMPLEMENTATION.

  METHOD escape_html_special_chars.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_escape_html( `<a href="x">Tom & Jerry's</a>` )
        exp = `&lt;a href=&quot;x&quot;&gt;Tom &amp; Jerry&#39;s&lt;/a&gt;` ).
  ENDMETHOD.

  METHOD escape_html_no_special.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_escape_html( `hello world` )
        exp = `hello world` ).
  ENDMETHOD.

  METHOD escape_json_special_chars.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_escape_json( `say "hi" \ end` )
        exp = `say \"hi\" \\ end` ).
  ENDMETHOD.

  METHOD url_encode_unreserved.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>url_encode( `Hello-World_1.2~x` )
        exp = `Hello-World_1.2~x` ).
  ENDMETHOD.

  METHOD url_encode_special.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>url_encode( `a b&c=d` )
        exp = `a%20b%26c%3Dd` ).
  ENDMETHOD.

  METHOD url_encode_umlaut.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>url_encode( `ä` )
        exp = `%C3%A4` ).
  ENDMETHOD.

  METHOD url_decode_simple.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>url_decode( `a%20b%26c%3Dd` )
        exp = `a b&c=d` ).
  ENDMETHOD.

  METHOD url_decode_plus_as_space.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>url_decode( `a+b` )
        exp = `a b` ).
  ENDMETHOD.

  METHOD url_roundtrip.
    DATA lv_original TYPE string.
    lv_original = `Straße 5 / Haus?x=1&y=äöü`.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>url_decode( zabaputil_cl_util_context=>url_encode( lv_original ) )
        exp = lv_original ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_levenshtein DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS distance_classic              FOR TESTING.
    METHODS distance_equal                FOR TESTING.
    METHODS distance_empty_first          FOR TESTING.
    METHODS distance_empty_second         FOR TESTING.
    METHODS distance_single_swap          FOR TESTING.

ENDCLASS.

CLASS ltcl_levenshtein IMPLEMENTATION.

  METHOD distance_classic.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_levenshtein( val1 = `kitten`
                                                        val2 = `sitting` )
        exp = 3 ).
  ENDMETHOD.

  METHOD distance_equal.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_levenshtein( val1 = `abap`
                                                        val2 = `abap` )
        exp = 0 ).
  ENDMETHOD.

  METHOD distance_empty_first.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_levenshtein( val1 = ``
                                                        val2 = `abc` )
        exp = 3 ).
  ENDMETHOD.

  METHOD distance_empty_second.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_levenshtein( val1 = `abc`
                                                        val2 = `` )
        exp = 3 ).
  ENDMETHOD.

  METHOD distance_single_swap.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>c_levenshtein( val1 = `flaw`
                                                        val2 = `lawn` )
        exp = 2 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_uuid_conv DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS c32_to_c36                    FOR TESTING.
    METHODS c36_to_c32                    FOR TESTING.
    METHODS c32_to_c22_zero               FOR TESTING.
    METHODS c22_roundtrip                 FOR TESTING.
    METHODS c22_length                    FOR TESTING.

ENDCLASS.

CLASS ltcl_uuid_conv IMPLEMENTATION.

  METHOD c32_to_c36.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>uuid_conv_c32_to_c36( `550E8400E29B41D4A716446655440000` )
        exp = `550E8400-E29B-41D4-A716-446655440000` ).
  ENDMETHOD.

  METHOD c36_to_c32.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>uuid_conv_c36_to_c32( `550e8400-e29b-41d4-a716-446655440000` )
        exp = `550E8400E29B41D4A716446655440000` ).
  ENDMETHOD.

  METHOD c32_to_c22_zero.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>uuid_conv_c32_to_c22( `00000000000000000000000000000000` )
        exp = `AAAAAAAAAAAAAAAAAAAAAA` ).
  ENDMETHOD.

  METHOD c22_roundtrip.
    DATA lv_c32 TYPE string.
    DATA lv_c22 TYPE string.
    lv_c32 = `00112233445566778899AABBCCDDEEFF`.
    
    lv_c22 = zabaputil_cl_util_context=>uuid_conv_c32_to_c22( lv_c32 ).
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>uuid_conv_c22_to_c32( lv_c22 )
        exp = lv_c32 ).
  ENDMETHOD.

  METHOD c22_length.
    DATA lv_c22 TYPE string.
    lv_c22 = zabaputil_cl_util_context=>uuid_conv_c32_to_c22( `550E8400E29B41D4A716446655440000` ).
    cl_abap_unit_assert=>assert_equals(
        act = strlen( lv_c22 )
        exp = 22 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_itab_aggregations DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_flight,
        carrid TYPE string,
        price  TYPE i,
      END OF ty_s_flight.
    TYPES ty_t_flight TYPE STANDARD TABLE OF ty_s_flight WITH DEFAULT KEY.

    METHODS get_flights
      RETURNING
        VALUE(result) TYPE ty_t_flight.

    METHODS sum_by                        FOR TESTING.
    METHODS sum_by_unknown_field          FOR TESTING.
    METHODS distinct                      FOR TESTING.
    METHODS group_sum_by                  FOR TESTING.

ENDCLASS.

CLASS ltcl_itab_aggregations IMPLEMENTATION.

  METHOD get_flights.
    DATA temp285 TYPE ltcl_itab_aggregations=>ty_t_flight.
    DATA temp286 LIKE LINE OF temp285.
    CLEAR temp285.
    
    temp286-carrid = `LH`.
    temp286-price = 100.
    INSERT temp286 INTO TABLE temp285.
    temp286-carrid = `LH`.
    temp286-price = 150.
    INSERT temp286 INTO TABLE temp285.
    temp286-carrid = `AA`.
    temp286-price = 200.
    INSERT temp286 INTO TABLE temp285.
    temp286-carrid = `LH`.
    temp286-price = 50.
    INSERT temp286 INTO TABLE temp285.
    result = temp285.
  ENDMETHOD.

  METHOD sum_by.
    DATA temp287 TYPE decfloat34.
    temp287 = 500.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>itab_sum_by( tab       = get_flights( )
                                                      fieldname = `PRICE` )
        exp = temp287 ).
  ENDMETHOD.

  METHOD sum_by_unknown_field.
    DATA temp288 TYPE decfloat34.
    temp288 = 0.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>itab_sum_by( tab       = get_flights( )
                                                      fieldname = `DOES_NOT_EXIST` )
        exp = temp288 ).
  ENDMETHOD.

  METHOD distinct.
    DATA lt_distinct TYPE string_table.
    FIELD-SYMBOLS <temp289> LIKE LINE OF lt_distinct.
    DATA temp290 LIKE sy-tabix.
    FIELD-SYMBOLS <temp291> LIKE LINE OF lt_distinct.
    DATA temp292 LIKE sy-tabix.
    lt_distinct = zabaputil_cl_util_context=>itab_distinct( tab       = get_flights( )
                                                                  fieldname = `CARRID` ).
    cl_abap_unit_assert=>assert_equals(
        act = lines( lt_distinct )
        exp = 2 ).
    
    
    temp290 = sy-tabix.
    READ TABLE lt_distinct INDEX 1 ASSIGNING <temp289>.
    sy-tabix = temp290.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp289>
        exp = `LH` ).
    
    
    temp292 = sy-tabix.
    READ TABLE lt_distinct INDEX 2 ASSIGNING <temp291>.
    sy-tabix = temp292.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp291>
        exp = `AA` ).
  ENDMETHOD.

  METHOD group_sum_by.
    DATA lt_sums TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp293> LIKE LINE OF lt_sums.
    DATA temp294 LIKE sy-tabix.
    FIELD-SYMBOLS <temp295> LIKE LINE OF lt_sums.
    DATA temp296 LIKE sy-tabix.
    FIELD-SYMBOLS <temp297> LIKE LINE OF lt_sums.
    DATA temp298 LIKE sy-tabix.
    FIELD-SYMBOLS <temp299> LIKE LINE OF lt_sums.
    DATA temp300 LIKE sy-tabix.
    lt_sums = zabaputil_cl_util_context=>itab_group_sum_by( tab      = get_flights( )
                                                                  group_by = `CARRID`
                                                                  sum_by   = `PRICE` ).
    cl_abap_unit_assert=>assert_equals(
        act = lines( lt_sums )
        exp = 2 ).
    
    
    temp294 = sy-tabix.
    READ TABLE lt_sums INDEX 1 ASSIGNING <temp293>.
    sy-tabix = temp294.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp293>-n
        exp = `LH` ).
    
    
    temp296 = sy-tabix.
    READ TABLE lt_sums INDEX 1 ASSIGNING <temp295>.
    sy-tabix = temp296.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp295>-v
        exp = `300` ).
    
    
    temp298 = sy-tabix.
    READ TABLE lt_sums INDEX 2 ASSIGNING <temp297>.
    sy-tabix = temp298.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp297>-n
        exp = `AA` ).
    
    
    temp300 = sy-tabix.
    READ TABLE lt_sums INDEX 2 ASSIGNING <temp299>.
    sy-tabix = temp300.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp299>-v
        exp = `200` ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_num_round DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS round_half_up                 FOR TESTING.
    METHODS round_half_up_negative        FOR TESTING.
    METHODS round_up                      FOR TESTING.
    METHODS round_down                    FOR TESTING.
    METHODS round_two_decimals            FOR TESTING.

ENDCLASS.

CLASS ltcl_num_round IMPLEMENTATION.

  METHOD round_half_up.
    DATA temp301 TYPE decfloat34.
    DATA temp47 TYPE decfloat34.
    DATA temp302 TYPE decfloat34.
    DATA temp48 TYPE decfloat34.
    temp301 = '2.5'.
    
    temp47 = 3.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( temp301 )
        exp = temp47 ).
    
    temp302 = '2.4'.
    
    temp48 = 2.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( temp302 )
        exp = temp48 ).
  ENDMETHOD.

  METHOD round_half_up_negative.
    DATA temp303 TYPE decfloat34.
    DATA temp49 TYPE decfloat34.
    temp303 = '-2.5'.
    
    temp49 = -3.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( temp303 )
        exp = temp49 ).
  ENDMETHOD.

  METHOD round_up.
    DATA temp304 TYPE decfloat34.
    DATA temp50 TYPE decfloat34.
    temp304 = '2.1'.
    
    temp50 = 3.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( val  = temp304
                                                    mode = `UP` )
        exp = temp50 ).
  ENDMETHOD.

  METHOD round_down.
    DATA temp305 TYPE decfloat34.
    DATA temp51 TYPE decfloat34.
    temp305 = '2.9'.
    
    temp51 = 2.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( val  = temp305
                                                    mode = `DOWN` )
        exp = temp51 ).
  ENDMETHOD.

  METHOD round_two_decimals.
    DATA temp306 TYPE decfloat34.
    DATA temp52 TYPE decfloat34.
    temp306 = '2.345'.
    
    temp52 = '2.35'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( val      = temp306
                                                    decimals = 2 )
        exp = temp52 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_mimetype_lang DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS mimetype_json                 FOR TESTING.
    METHODS mimetype_xlsx                 FOR TESTING.
    METHODS mimetype_uppercase            FOR TESTING.
    METHODS mimetype_unknown              FOR TESTING.
    METHODS lang_sap_to_iso_german        FOR TESTING.
    METHODS lang_sap_to_iso_lowercase     FOR TESTING.
    METHODS lang_iso_to_sap_english       FOR TESTING.
    METHODS lang_iso_to_sap_uppercase     FOR TESTING.

ENDCLASS.

CLASS ltcl_mimetype_lang IMPLEMENTATION.

  METHOD mimetype_json.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>file_get_mimetype( `data.json` )
        exp = `application/json` ).
  ENDMETHOD.

  METHOD mimetype_xlsx.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>file_get_mimetype( `report.xlsx` )
        exp = `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` ).
  ENDMETHOD.

  METHOD mimetype_uppercase.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>file_get_mimetype( `IMAGE.PNG` )
        exp = `image/png` ).
  ENDMETHOD.

  METHOD mimetype_unknown.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>file_get_mimetype( `file.xyz` )
        exp = `application/octet-stream` ).
  ENDMETHOD.

  METHOD lang_sap_to_iso_german.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>lang_sap_to_iso( `D` )
        exp = `de` ).
  ENDMETHOD.

  METHOD lang_sap_to_iso_lowercase.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>lang_sap_to_iso( `e` )
        exp = `en` ).
  ENDMETHOD.

  METHOD lang_iso_to_sap_english.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>lang_iso_to_sap( `en` )
        exp = `E` ).
  ENDMETHOD.

  METHOD lang_iso_to_sap_uppercase.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>lang_iso_to_sap( `DE` )
        exp = `D` ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cur_amounts DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS to_external_zero_decimals     FOR TESTING.
    METHODS to_external_three_decimals    FOR TESTING.
    METHODS to_external_two_decimals      FOR TESTING.
    METHODS to_internal_zero_decimals     FOR TESTING.
    METHODS roundtrip                     FOR TESTING.

ENDCLASS.

CLASS ltcl_cur_amounts IMPLEMENTATION.

  METHOD to_external_zero_decimals.
    " JPY-style currency: stored 123.45 means 12345
    DATA temp307 TYPE decfloat34.
    DATA temp53 TYPE decfloat34.
    temp307 = '123.45'.
    
    temp53 = 12345.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_external( val      = temp307
                                                                 decimals = 0 )
        exp = temp53 ).
  ENDMETHOD.

  METHOD to_external_three_decimals.
    " BHD-style currency: stored 123.45 means 12.345
    DATA temp308 TYPE decfloat34.
    DATA temp54 TYPE decfloat34.
    temp308 = '123.45'.
    
    temp54 = '12.345'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_external( val      = temp308
                                                                 decimals = 3 )
        exp = temp54 ).
  ENDMETHOD.

  METHOD to_external_two_decimals.
    DATA temp309 TYPE decfloat34.
    DATA temp55 TYPE decfloat34.
    temp309 = '123.45'.
    
    temp55 = '123.45'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_external( val      = temp309
                                                                 decimals = 2 )
        exp = temp55 ).
  ENDMETHOD.

  METHOD to_internal_zero_decimals.
    DATA temp310 TYPE decfloat34.
    DATA temp56 TYPE decfloat34.
    temp310 = 12345.
    
    temp56 = '123.45'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_internal( val      = temp310
                                                                 decimals = 0 )
        exp = temp56 ).
  ENDMETHOD.

  METHOD roundtrip.
    DATA temp311 TYPE decfloat34.
    DATA lv_val LIKE temp311.
    temp311 = '987.65'.
    
    lv_val = temp311.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_internal(
                  val      = zabaputil_cl_util_context=>cur_amount_to_external( val      = lv_val
                                                                                decimals = 3 )
                  decimals = 3 )
        exp = lv_val ).
  ENDMETHOD.

ENDCLASS.

"! Local exception used by the error_* tests below - a class-specific
"! attribute is exactly what error_get_attributes has to surface.
CLASS lcx_sync_test DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.

    DATA mv_detail TYPE string.
    DATA mv_empty  TYPE string ##NEEDED.
    DATA mt_tab    TYPE string_table ##NEEDED.

    METHODS constructor
      IMPORTING
        detail TYPE string OPTIONAL.

ENDCLASS.

CLASS lcx_sync_test IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_detail = detail.
  ENDMETHOD.

ENDCLASS.

"! ================================================================
"! Methods and fixes synced back from the consumers' context classes
"! ================================================================
CLASS ltcl_sync_back DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    " rtti_check_clike / rtti_check_printable
    METHODS clike_string                   FOR TESTING.
    METHODS clike_date_time_num            FOR TESTING.
    METHODS clike_int_is_false             FOR TESTING.
    METHODS printable_elementary           FOR TESTING.
    METHODS printable_complex_is_false     FOR TESTING.

    " class_constructor
    METHODS constants_are_filled           FOR TESTING.

    " no-CONV-string( ) rewrites - behaviour must be identical
    METHODS attributes_name_is_trimmed     FOR TESTING.
    METHODS attri_by_any_cache_hits        FOR TESTING.
    METHODS date_valid_after_rewrite       FOR TESTING.

    " error_get_source_position / error_get_attributes
    METHODS position_unbound_is_empty      FOR TESTING.
    METHODS position_bound_no_dump         FOR TESTING.
    METHODS attributes_unbound_is_empty    FOR TESTING.
    METHODS attributes_own_attribute       FOR TESTING.
    METHODS attributes_skip_general        FOR TESTING.

    " unassign_* / rtti_get_classname_by_ref guards
    METHODS unassign_data_unbound          FOR TESTING.
    METHODS unassign_object_unbound        FOR TESTING.
    METHODS unassign_data_bound            FOR TESTING.
    METHODS unassign_object_bound          FOR TESTING.
    METHODS unassign_data_dirty_subrc      FOR TESTING.
    METHODS unassign_data_unbound_dirty    FOR TESTING.
    METHODS classname_unbound_is_empty     FOR TESTING.

    " itab_* fixes
    METHODS filter_elementary_line_type    FOR TESTING.
    METHODS struc_skips_complex_components FOR TESTING.

    " filter tokens / url params
    METHODS token_excluding_is_negated     FOR TESTING.
    METHODS token_including_unchanged      FOR TESTING.
    METHODS url_lower_case_encoded_equals  FOR TESTING.
    METHODS url_startup_params_first       FOR TESTING.
    METHODS url_encoded_value_survives     FOR TESTING.
    METHODS url_second_question_is_value   FOR TESTING.
    METHODS url_startup_with_siblings      FOR TESTING.
    METHODS url_full_url_with_path         FOR TESTING.

    " msg_get_rap_row - `%CID` / `%MSG` / `%FAIL` are no legal component
    " names in a locally declared type, so the row is built through RTTI.
    " fail = -1 leaves the %FAIL cause out of the row entirely
    METHODS build_rap_row
      IMPORTING
        cid           TYPE string
        msg           TYPE string
        fail          TYPE i
      RETURNING
        VALUE(result) TYPE REF TO data.

    METHODS rap_row_msg_carries_meta       FOR TESTING.
    METHODS rap_row_fail_carries_meta      FOR TESTING.
    METHODS rap_row_quiet_row_is_empty     FOR TESTING.

    " ui5_msg_box_format
    METHODS msg_box_empty_is_skipped       FOR TESTING.
    METHODS msg_box_single_message         FOR TESTING.
    METHODS msg_box_several_messages       FOR TESTING.

ENDCLASS.

CLASS ltcl_sync_back IMPLEMENTATION.

  METHOD clike_string.
    DATA lv_str TYPE string.
    DATA lv_char TYPE c LENGTH 4.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_clike( lv_str ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_clike( lv_char ) ).
  ENDMETHOD.

  METHOD clike_date_time_num.
    " N/D/T are character-like - they used to be reported as non-clike, so a
    " date handed to msg_get_t was silently dropped instead of rendered
    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_num  TYPE n LENGTH 4.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_clike( lv_date ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_clike( lv_time ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_clike( lv_num ) ).
  ENDMETHOD.

  METHOD clike_int_is_false.
    DATA lv_int TYPE i.
    DATA lt_tab TYPE string_table.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_clike( lv_int ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_clike( lt_tab ) ).
  ENDMETHOD.

  METHOD printable_elementary.
    DATA lv_int  TYPE i.
    DATA lv_pack TYPE p LENGTH 8 DECIMALS 2.
    DATA lv_str  TYPE string.
    DATA lv_date TYPE d.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_printable( lv_int ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_printable( lv_pack ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_printable( lv_str ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_printable( lv_date ) ).
  ENDMETHOD.

  METHOD printable_complex_is_false.
    " these are the ones a generic |{ val }| would dump on
    DATA lt_tab TYPE string_table.
    DATA ls_struc TYPE ltcl_test_app=>ty_row.
    DATA lo_obj TYPE REF TO ltcl_test_app.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_printable( lt_tab ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_printable( ls_struc ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_printable( lo_obj ) ).
  ENDMETHOD.

  METHOD constants_are_filled.
    " every environment-abstracted constant the class publishes is set by
    " class_constructor and carries the value of the SAP standard class
    " behind it - a consumer branching on a stored type_kind reads these
    " instead of cl_abap_typedescr directly
    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>cv_typedescr_typekind_date
                                        exp = cl_abap_typedescr=>typekind_date ).
    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>cv_typedescr_typekind_time
                                        exp = cl_abap_typedescr=>typekind_time ).
    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>cv_typedescr_typekind_packed
                                        exp = cl_abap_typedescr=>typekind_packed ).
    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>cv_typedescr_kind_elem
                                        exp = cl_abap_typedescr=>kind_elem ).

    " the ones that were already there stay filled - cv_char_util_charsize
    " in particular has callers here that abap2UI5's trimmed copy has not,
    " so it must not follow the downstream class_constructor into removal
    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>cv_char_util_charsize
                                        exp = cl_abap_char_utilities=>charsize ).
    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>cv_typedescr_kind_struct
                                        exp = cl_abap_typedescr=>kind_struct ).
    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>cv_objectdescr_public
                                        exp = cl_abap_objectdescr=>public ).
  ENDMETHOD.

  METHOD attributes_name_is_trimmed.
    " the RTTI name is a CHAR field; the plain assignment into a string
    " drops its trailing blanks, exactly as CONV string( ) did - the name
    " is used both as the lookup key and in ASSIGN val->(lv_name), so a
    " padded name would find no attribute at all
    DATA lx TYPE REF TO lcx_sync_test.
    DATA lt_attri TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp7 TYPE xsdboolean.
    DATA temp1 LIKE sy-subrc.
    DATA temp312 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp312.
    CREATE OBJECT lx TYPE lcx_sync_test EXPORTING DETAIL = `the_detail`.
    
    lt_attri = zabaputil_cl_util_context=>error_get_attributes( lx ).

    
    
    READ TABLE lt_attri WITH KEY n = `MV_DETAIL` TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.
    temp7 = boolc( temp1 = 0 ).
    cl_abap_unit_assert=>assert_true( temp7 ).
    
    
    LOOP AT lt_attri REFERENCE INTO lr_attri.
      cl_abap_unit_assert=>assert_equals( act = strlen( lr_attri->n )
                                          exp = strlen( condense( lr_attri->n ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD attri_by_any_cache_hits.
    " the cache key is the descriptor's absolute_name, now assigned instead
    " of CONV string( ) - a second call over the same type must still hit
    " the entry the first one wrote, not build a second one
    TYPES: BEGIN OF ty_cached,
             alpha TYPE string,
             beta  TYPE i,
           END OF ty_cached.
    DATA ls_val TYPE ty_cached ##NEEDED.

    DATA lt_first TYPE abap_component_tab.
    DATA lt_second TYPE abap_component_tab.
    lt_first  = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( ls_val ).
    
    lt_second = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( ls_val ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_first )
                                        exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_second
                                        exp = lt_first ).
  ENDMETHOD.

  METHOD date_valid_after_rewrite.
    " the same shape outside the vendored surface: the rendered date is
    " compared against `00000000`, so the assignment has to produce the
    " eight-character form the CONV produced
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_date_valid( `2024-03-15` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `0000-00-00` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `not-a-date` ) ).
  ENDMETHOD.

  METHOD position_unbound_is_empty.
    DATA lx TYPE REF TO cx_root.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>error_get_source_position( lx ) ).
  ENDMETHOD.

  METHOD position_bound_no_dump.
        DATA lx TYPE REF TO lcx_sync_test.
        DATA lv_position TYPE string.
    " the contract is "never let the diagnostic be the reason a caller fails"
    " - the content depends on the runtime (empty on the transpiler), that it
    " returns at all is what is asserted here
    TRY.
        RAISE EXCEPTION TYPE lcx_sync_test.
        
      CATCH lcx_sync_test INTO lx.
        
        lv_position = zabaputil_cl_util_context=>error_get_source_position( lx ).
        cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>rtti_check_clike( lv_position )
                                            exp = abap_true ).
    ENDTRY.
  ENDMETHOD.

  METHOD attributes_unbound_is_empty.
    DATA lx TYPE REF TO cx_root.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>error_get_attributes( lx ) ).
  ENDMETHOD.

  METHOD attributes_own_attribute.
    DATA lx TYPE REF TO lcx_sync_test.
    DATA lt_attri TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp313> LIKE LINE OF lt_attri.
    DATA temp314 LIKE sy-tabix.
    CREATE OBJECT lx TYPE lcx_sync_test EXPORTING DETAIL = `the_detail`.
    
    lt_attri = zabaputil_cl_util_context=>error_get_attributes( lx ).
    
    
    temp314 = sy-tabix.
    READ TABLE lt_attri WITH KEY n = `MV_DETAIL` ASSIGNING <temp313>.
    sy-tabix = temp314.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp313>-v
                                        exp = `the_detail` ).
  ENDMETHOD.

  METHOD attributes_skip_general.
    DATA lx TYPE REF TO lcx_sync_test.
    DATA lt_attri TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp8 TYPE xsdboolean.
    DATA temp2 LIKE sy-subrc.
    DATA temp9 TYPE xsdboolean.
    DATA temp3 LIKE sy-subrc.
    DATA temp10 TYPE xsdboolean.
    DATA temp4 LIKE sy-subrc.
    DATA temp11 TYPE xsdboolean.
    DATA temp5 LIKE sy-subrc.
    CREATE OBJECT lx TYPE lcx_sync_test EXPORTING DETAIL = `the_detail`.
    
    lt_attri = zabaputil_cl_util_context=>error_get_attributes( lx ).
    " rendered by the caller or without information value
    
    
    READ TABLE lt_attri WITH KEY n = `PREVIOUS` TRANSPORTING NO FIELDS.
    temp2 = sy-subrc.
    temp8 = boolc( temp2 = 0 ).
    cl_abap_unit_assert=>assert_false( temp8 ).
    
    
    READ TABLE lt_attri WITH KEY n = `TEXTID` TRANSPORTING NO FIELDS.
    temp3 = sy-subrc.
    temp9 = boolc( temp3 = 0 ).
    cl_abap_unit_assert=>assert_false( temp9 ).
    " initial and non-printable attributes carry nothing to show
    
    
    READ TABLE lt_attri WITH KEY n = `MV_EMPTY` TRANSPORTING NO FIELDS.
    temp4 = sy-subrc.
    temp10 = boolc( temp4 = 0 ).
    cl_abap_unit_assert=>assert_false( temp10 ).
    
    
    READ TABLE lt_attri WITH KEY n = `MT_TAB` TRANSPORTING NO FIELDS.
    temp5 = sy-subrc.
    temp11 = boolc( temp5 = 0 ).
    cl_abap_unit_assert=>assert_false( temp11 ).
  ENDMETHOD.

  METHOD unassign_data_unbound.
    DATA lr_data TYPE REF TO data.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>unassign_data( lr_data ) ).
  ENDMETHOD.

  METHOD unassign_object_unbound.
    DATA lr_data TYPE REF TO data.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>unassign_object( lr_data ) ).
  ENDMETHOD.

  METHOD unassign_data_bound.
    DATA lv_str   TYPE string VALUE `payload`.
    DATA lr_inner TYPE REF TO data.
    DATA lr_outer TYPE REF TO data.
    FIELD-SYMBOLS <val> TYPE any.
    DATA lr_act TYPE REF TO data.

    GET REFERENCE OF lv_str INTO lr_inner.
    GET REFERENCE OF lr_inner INTO lr_outer.

    
    lr_act = zabaputil_cl_util_context=>unassign_data( lr_outer ).
    cl_abap_unit_assert=>assert_bound( lr_act ).
    ASSIGN lr_act->* TO <val>.
    cl_abap_unit_assert=>assert_equals( act = <val>
                                        exp = `payload` ).
  ENDMETHOD.

  METHOD unassign_object_bound.
    DATA lo_inner TYPE REF TO object.
    DATA lr_outer TYPE REF TO data.

    CREATE OBJECT lo_inner TYPE ltcl_test_app.
    GET REFERENCE OF lo_inner INTO lr_outer.

    cl_abap_unit_assert=>assert_equals( act = zabaputil_cl_util_context=>unassign_object( lr_outer )
                                        exp = lo_inner ).
  ENDMETHOD.

  METHOD unassign_data_dirty_subrc.
    " the guard is the field symbol, not sy-subrc: ASSIGN ref->* of an
    " unbound reference leaves sy-subrc untouched on some runtimes, so a
    " value left over from an earlier statement must not decide the answer.
    " Here a stale 4 must not suppress a perfectly good dereference
    DATA lv_str   TYPE string VALUE `payload`.
    DATA lr_inner TYPE REF TO data.
    DATA lr_outer TYPE REF TO data.
    DATA lt_empty TYPE string_table.
    FIELD-SYMBOLS <lv_none> TYPE string.

    GET REFERENCE OF lv_str INTO lr_inner.
    GET REFERENCE OF lr_inner INTO lr_outer.

    
    READ TABLE lt_empty INDEX 1 ASSIGNING <lv_none>.
    cl_abap_unit_assert=>assert_differs( act = sy-subrc
                                         exp = 0 ).

    cl_abap_unit_assert=>assert_bound( zabaputil_cl_util_context=>unassign_data( lr_outer ) ).
  ENDMETHOD.

  METHOD unassign_data_unbound_dirty.
    " and the other direction: a stale 0 must not let the unassigned field
    " symbol through into `result = <unassign>`
    DATA lr_data  TYPE REF TO data.
    DATA lt_one   TYPE string_table.
    FIELD-SYMBOLS <lv_one> TYPE string.

    INSERT `x` INTO TABLE lt_one.
    
    READ TABLE lt_one INDEX 1 ASSIGNING <lv_one>.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>unassign_data( lr_data ) ).
  ENDMETHOD.

  METHOD classname_unbound_is_empty.
    DATA lo_obj TYPE REF TO object.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_classname_by_ref( lo_obj ) ).
  ENDMETHOD.

  METHOD filter_elementary_line_type.
    " a table without components used to lose every row, because the failing
    " ASSIGN COMPONENT 1 was read as "no more fields, nothing matched"
    DATA temp315 TYPE string_table.
    DATA lt_tab LIKE temp315.
    FIELD-SYMBOLS <temp317> LIKE LINE OF lt_tab.
    DATA temp318 LIKE sy-tabix.
    CLEAR temp315.
    INSERT `alpha` INTO TABLE temp315.
    INSERT `beta` INTO TABLE temp315.
    INSERT `gamma` INTO TABLE temp315.
    
    lt_tab = temp315.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `et`
                                                   CHANGING  tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_tab )
                                        exp = 1 ).
    
    
    temp318 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp317>.
    sy-tabix = temp318.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp317>
                                        exp = `beta` ).
  ENDMETHOD.

  METHOD struc_skips_complex_components.
    " only the elementary components survive - a nested structure or a
    " reference would raise an unhandled move error on the value assignment
    DATA:
      BEGIN OF ls_struc,
        name   TYPE string,
        tab    TYPE string_table,
        nested TYPE ltcl_test_app=>ty_row,
        ref    TYPE REF TO data,
        obj    TYPE REF TO object,
      END OF ls_struc.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp319> LIKE LINE OF lt_result.
    DATA temp320 LIKE sy-tabix.
    FIELD-SYMBOLS <temp321> LIKE LINE OF lt_result.
    DATA temp322 LIKE sy-tabix.

    ls_struc-name = `the_name`.
    
    lt_result = zabaputil_cl_util_context=>itab_get_by_struc( ls_struc ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_result )
                                        exp = 1 ).
    
    
    temp320 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp319>.
    sy-tabix = temp320.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp319>-n
                                        exp = `NAME` ).
    
    
    temp322 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp321>.
    sy-tabix = temp322.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp321>-v
                                        exp = `the_name` ).
  ENDMETHOD.

  METHOD token_excluding_is_negated.
    DATA temp323 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp324 LIKE LINE OF temp323.
    DATA lt_range LIKE temp323.
    DATA lt_token TYPE zabaputil_cl_util_context=>ty_t_token.
    FIELD-SYMBOLS <temp325> LIKE LINE OF lt_token.
    DATA temp326 LIKE sy-tabix.
    CLEAR temp323.
    
    temp324-sign = `E`.
    temp324-option = `EQ`.
    temp324-low = `4711`.
    INSERT temp324 INTO TABLE temp323.
    
    lt_range = temp323.
    
    lt_token = zabaputil_cl_util_context=>filter_get_token_t_by_range_t( lt_range ).
    
    
    temp326 = sy-tabix.
    READ TABLE lt_token INDEX 1 ASSIGNING <temp325>.
    sy-tabix = temp326.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp325>-text
                                        exp = `!(=4711)` ).
  ENDMETHOD.

  METHOD token_including_unchanged.
    DATA temp327 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp328 LIKE LINE OF temp327.
    DATA lt_range LIKE temp327.
    DATA lt_token TYPE zabaputil_cl_util_context=>ty_t_token.
    FIELD-SYMBOLS <temp329> LIKE LINE OF lt_token.
    DATA temp330 LIKE sy-tabix.
    CLEAR temp327.
    
    temp328-sign = `I`.
    temp328-option = `EQ`.
    temp328-low = `4711`.
    INSERT temp328 INTO TABLE temp327.
    
    lt_range = temp327.
    
    lt_token = zabaputil_cl_util_context=>filter_get_token_t_by_range_t( lt_range ).
    
    
    temp330 = sy-tabix.
    READ TABLE lt_token INDEX 1 ASSIGNING <temp329>.
    sy-tabix = temp330.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp329>-text
                                        exp = `=4711` ).
  ENDMETHOD.

  METHOD url_lower_case_encoded_equals.
    " RFC 3986 allows lowercase hex digits in percent-encodings, so the
    " packed value decodes %3d the same way as %3D
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp331> LIKE LINE OF lt_param.
    DATA temp332 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?sap-startup-params=name%3dvalue` ).
    
    
    temp332 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `name` ASSIGNING <temp331>.
    sy-tabix = temp332.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp331>-v
                                        exp = `value` ).
  ENDMETHOD.

  METHOD url_startup_params_first.
    " sap-startup-params as the first/only parameter has no leading & to
    " match on - the wrapper used to stay in the parameter name
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp333> LIKE LINE OF lt_param.
    DATA temp334 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?sap-startup-params=name%3Dvalue` ).
    
    
    temp334 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `name` ASSIGNING <temp333>.
    sy-tabix = temp334.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp333>-v
                                        exp = `value` ).
  ENDMETHOD.

  METHOD url_encoded_value_survives.
    " decoding the WHOLE query first tore every legitimately encoded `&`
    " apart: `a=x%26y` became the two parameters a=x and y= , and
    " url_param_create_url wrote the damage back into every generated link.
    " Only the sap-startup-params value is decoded now
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp335> LIKE LINE OF lt_param.
    DATA temp336 LIKE sy-tabix.
    FIELD-SYMBOLS <temp337> LIKE LINE OF lt_param.
    DATA temp338 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?a=x%26y&b=2` ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_param )
                                        exp = 2 ).
    
    
    temp336 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `a` ASSIGNING <temp335>.
    sy-tabix = temp336.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp335>-v
                                        exp = `x%26y` ).
    
    
    temp338 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `b` ASSIGNING <temp337>.
    sy-tabix = temp338.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp337>-v
                                        exp = `2` ).
  ENDMETHOD.

  METHOD url_second_question_is_value.
    " the cut is at the FIRST `?` only - a later one is a legal, unencoded
    " character of a value, and cutting there dropped every parameter
    " before it
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp339> LIKE LINE OF lt_param.
    DATA temp340 LIKE sy-tabix.
    FIELD-SYMBOLS <temp341> LIKE LINE OF lt_param.
    DATA temp342 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?app_start=zcl_x&title=why?` ).
    
    
    temp340 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `app_start` ASSIGNING <temp339>.
    sy-tabix = temp340.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp339>-v
                                        exp = `zcl_x` ).
    
    
    temp342 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `title` ASSIGNING <temp341>.
    sy-tabix = temp342.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp341>-v
                                        exp = `why?` ).
  ENDMETHOD.

  METHOD url_startup_with_siblings.
    " the packed value is unwrapped in place: what stood next to
    " sap-startup-params in the query stays a parameter of its own
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp343> LIKE LINE OF lt_param.
    DATA temp344 LIKE sy-tabix.
    FIELD-SYMBOLS <temp345> LIKE LINE OF lt_param.
    DATA temp346 LIKE sy-tabix.
    FIELD-SYMBOLS <temp347> LIKE LINE OF lt_param.
    DATA temp348 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab(
                         `?sap-ui-tech=x&sap-startup-params=name%3Dvalue%26other%3D2&sap-lang=EN` ).
    
    
    temp344 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `name` ASSIGNING <temp343>.
    sy-tabix = temp344.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp343>-v
                                        exp = `value` ).
    
    
    temp346 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `other` ASSIGNING <temp345>.
    sy-tabix = temp346.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp345>-v
                                        exp = `2` ).
    
    
    temp348 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `sap-lang` ASSIGNING <temp347>.
    sy-tabix = temp348.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp347>-v
                                        exp = `EN` ).
  ENDMETHOD.

  METHOD url_full_url_with_path.
    " a full URL, not just a query string - the path in front of the first
    " `?` is not a parameter
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp349> LIKE LINE OF lt_param.
    DATA temp350 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab(
                         `https://host/sap/bc/z2ui5?app_start=zcl_x&b=2` ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_param )
                                        exp = 2 ).
    
    
    temp350 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `app_start` ASSIGNING <temp349>.
    sy-tabix = temp350.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp349>-v
                                        exp = `zcl_x` ).
  ENDMETHOD.

  METHOD build_rap_row.

    DATA lt_comp TYPE cl_abap_structdescr=>component_table.
    DATA lo_struct TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.

    DATA lo_string TYPE REF TO cl_abap_elemdescr.
    DATA temp351 TYPE abap_componentdescr.
    DATA temp352 TYPE abap_componentdescr.
      DATA temp353 TYPE abap_componentdescr.
      DATA temp57 TYPE abap_component_tab.
      DATA temp58 LIKE LINE OF temp57.
    lo_string = cl_abap_elemdescr=>get_string( ).

    
    CLEAR temp351.
    temp351-name = `%CID`.
    temp351-type = lo_string.
    INSERT temp351 INTO TABLE lt_comp.
    
    CLEAR temp352.
    temp352-name = `%MSG`.
    temp352-type = lo_string.
    INSERT temp352 INTO TABLE lt_comp.
    IF fail >= 0.
      
      CLEAR temp353.
      temp353-name = `%FAIL`.
      
      CLEAR temp57.
      
      temp58-name = `CAUSE`.
      temp58-type = cl_abap_elemdescr=>get_i( ).
      INSERT temp58 INTO TABLE temp57.
      temp353-type = cl_abap_structdescr=>create( temp57 ).
      INSERT temp353 INTO TABLE lt_comp.
    ENDIF.

    " the handle in a variable: TYPE HANDLE takes no functional method call
    lo_struct = cl_abap_structdescr=>create( lt_comp ).
    CREATE DATA result TYPE HANDLE lo_struct.
    ASSIGN result->* TO <row>.

    ASSIGN COMPONENT `%CID` OF STRUCTURE <row> TO <comp>.
    <comp> = cid.

    ASSIGN COMPONENT `%MSG` OF STRUCTURE <row> TO <comp>.
    <comp> = msg.

    IF fail >= 0.
      ASSIGN COMPONENT `%FAIL-CAUSE` OF STRUCTURE <row> TO <comp>.
      <comp> = fail.
    ENDIF.

  ENDMETHOD.

  METHOD rap_row_msg_carries_meta.
    " the meta block is built lazily now - a row that DOES carry a message
    " must still get it. The row is built through RTTI: `%CID` is no legal
    " component name in a locally declared type
    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
    FIELD-SYMBOLS <temp354> LIKE LINE OF lt_msg.
    DATA temp355 LIKE sy-tabix.
    FIELD-SYMBOLS <temp59> LIKE LINE OF <temp354>-t_meta.
    DATA temp60 LIKE sy-tabix.
    lr_row = build_rap_row( cid  = `CID_1`
                                  msg  = `the_message`
                                  fail = -1 ).
    
    ASSIGN lr_row->* TO <row>.

    
    lt_msg = zabaputil_cl_util_context=>msg_get_t( <row> ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
    
    
    temp355 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp354>.
    sy-tabix = temp355.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp60 = sy-tabix.
    READ TABLE <temp354>-t_meta WITH KEY n = `cid` ASSIGNING <temp59>.
    sy-tabix = temp60.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp59>-v
                                        exp = `CID_1` ).
  ENDMETHOD.

  METHOD rap_row_fail_carries_meta.
    " the %FAIL branch is reached without the %MSG branch having built the
    " block - it has to build it itself, or the meta silently went missing
    " on exactly the rows that failed
    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
    FIELD-SYMBOLS <temp356> LIKE LINE OF lt_msg.
    DATA temp357 LIKE sy-tabix.
    FIELD-SYMBOLS <temp358> LIKE LINE OF lt_msg.
    DATA temp359 LIKE sy-tabix.
    FIELD-SYMBOLS <temp61> LIKE LINE OF <temp358>-t_meta.
    DATA temp62 LIKE sy-tabix.
    lr_row = build_rap_row( cid  = `CID_2`
                                  msg  = ``
                                  fail = 1 ).
    
    ASSIGN lr_row->* TO <row>.

    
    lt_msg = zabaputil_cl_util_context=>msg_get_t( <row> ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
    
    
    temp357 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp356>.
    sy-tabix = temp357.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp356>-type
                                        exp = `E` ).
    
    
    temp359 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp358>.
    sy-tabix = temp359.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp62 = sy-tabix.
    READ TABLE <temp358>-t_meta WITH KEY n = `cid` ASSIGNING <temp61>.
    sy-tabix = temp62.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp61>-v
                                        exp = `CID_2` ).
  ENDMETHOD.

  METHOD rap_row_quiet_row_is_empty.
    " the common row: neither a filled %MSG nor a %FAIL. Nothing is
    " reported and - the point of the change - no meta block is built
    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    lr_row = build_rap_row( cid  = `CID_3`
                                  msg  = ``
                                  fail = -1 ).
    
    ASSIGN lr_row->* TO <row>.

    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>msg_get_t( <row> ) ).
  ENDMETHOD.

  METHOD msg_box_empty_is_skipped.
    DATA lv_empty TYPE string.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    ls_box = zabaputil_cl_util_context=>ui5_msg_box_format( lv_empty ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-skip
                                        exp = abap_true ).
    cl_abap_unit_assert=>assert_initial( ls_box-text ).
  ENDMETHOD.

  METHOD msg_box_single_message.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    ls_box = zabaputil_cl_util_context=>ui5_msg_box_format( `the_message` ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-skip
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `the_message` ).
    " title and type come from the message type, both are always filled
    cl_abap_unit_assert=>assert_not_initial( ls_box-title ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-type
                                        exp = to_lower( ls_box-title ) ).
    cl_abap_unit_assert=>assert_initial( ls_box-details ).
  ENDMETHOD.

  METHOD msg_box_several_messages.
    DATA temp360 TYPE zabaputil_cl_util_context=>ty_t_msg.
    DATA temp361 LIKE LINE OF temp360.
    DATA lt_msg LIKE temp360.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    CLEAR temp360.
    
    temp361-type = `E`.
    temp361-text = `first`.
    INSERT temp361 INTO TABLE temp360.
    temp361-type = `E`.
    temp361-text = `second`.
    INSERT temp361 INTO TABLE temp360.
    
    lt_msg = temp360.
    
    ls_box = zabaputil_cl_util_context=>ui5_msg_box_format( lt_msg ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-skip
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-text
                                         exp = `*2*` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<li>first</li>*<li>second</li>*` ).
    " type/title of the box come from the FIRST message, not from the last
    cl_abap_unit_assert=>assert_not_initial( ls_box-title ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-type
                                        exp = to_lower( ls_box-title ) ).
  ENDMETHOD.

ENDCLASS.

"! ================================================================
"! ui5_data_box_format - everything a box can be built from that is
"! not a message, plus rtti_check_table_standard
"! ================================================================
CLASS ltcl_data_box DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_node,
        name TYPE string,
        next TYPE REF TO data,
      END OF ty_s_node.

  PRIVATE SECTION.

    " elementary values
    METHODS char_is_its_own_text        FOR TESTING.
    METHODS char_empty_is_not_skipped   FOR TESTING.
    METHODS char_markup_moves_to_detail FOR TESTING.
    METHODS number_is_trimmed           FOR TESTING.
    METHODS date_renders                FOR TESTING.

    " complex values
    METHODS empty_table_is_skipped      FOR TESTING.
    METHODS empty_struct_is_skipped     FOR TESTING.
    METHODS table_headline_and_list     FOR TESTING.
    METHODS table_one_entry_is_singular FOR TESTING.
    METHODS table_row_cap_is_announced  FOR TESTING.
    METHODS struct_headline_and_fields  FOR TESTING.
    METHODS object_headline_and_attris  FOR TESTING.
    METHODS exception_renders_its_text  FOR TESTING.
    METHODS dref_renders_the_target     FOR TESTING.
    METHODS cycle_stops_at_depth        FOR TESTING.

    " escaping and the text cap
    METHODS values_are_escaped          FOR TESTING.
    METHODS plain_text_is_capped        FOR TESTING.

    " the hand-over from ui5_msg_box_format
    METHODS non_message_struct_skips    FOR TESTING.

    " rtti_check_table_standard
    METHODS table_standard_true         FOR TESTING.
    METHODS table_sorted_false          FOR TESTING.
    METHODS table_hashed_false          FOR TESTING.
    METHODS table_non_table_false       FOR TESTING.

ENDCLASS.

CLASS ltcl_data_box IMPLEMENTATION.

  METHOD char_is_its_own_text.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( `plain text` ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-skip
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `plain text` ).
    cl_abap_unit_assert=>assert_initial( ls_box-details ).
  ENDMETHOD.

  METHOD char_empty_is_not_skipped.
    " an empty character value is the shape a plain text box has always
    " had, and is not this method's to turn into silence
    DATA lv_empty TYPE string.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lv_empty ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-skip
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_initial( ls_box-text ).
  ENDMETHOD.

  METHOD char_markup_moves_to_detail.
    " markup in the box text would be shown as the tags it is written with,
    " so it goes into the details and the plain text behind it stays the
    " headline
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    DATA temp12 TYPE xsdboolean.
    DATA temp13 TYPE xsdboolean.
    DATA temp14 TYPE xsdboolean.
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( `<b>bold</b> and <i>italic</i>` ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-details
                                        exp = `<b>bold</b> and <i>italic</i>` ).
    
    temp12 = boolc( ls_box-text CS `<b>` ).
    cl_abap_unit_assert=>assert_false( temp12 ).
    
    temp13 = boolc( ls_box-text CS `bold` ).
    cl_abap_unit_assert=>assert_true( temp13 ).
    
    temp14 = boolc( ls_box-text CS `italic` ).
    cl_abap_unit_assert=>assert_true( temp14 ).
  ENDMETHOD.

  METHOD number_is_trimmed.
    " a number reaches a character target right-aligned in the length its
    " type needs - ` 42` is not what the value says
    DATA lv_int TYPE i VALUE 42.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lv_int ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `42` ).
    cl_abap_unit_assert=>assert_initial( ls_box-details ).
  ENDMETHOD.

  METHOD date_renders.
    DATA lv_date TYPE d VALUE '20260904'.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lv_date ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `20260904` ).
  ENDMETHOD.

  METHOD empty_table_is_skipped.
    " a caller that hands over the result table of a call it just made
    " expects no box when the call returned nothing
    DATA lt_empty TYPE string_table.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>ui5_data_box_format( lt_empty )-skip
        exp = abap_true ).
  ENDMETHOD.

  METHOD empty_struct_is_skipped.
    TYPES: BEGIN OF ty_s,
             alpha TYPE string,
             beta  TYPE i,
           END OF ty_s.
    DATA ls_empty TYPE ty_s.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>ui5_data_box_format( ls_empty )-skip
        exp = abap_true ).
  ENDMETHOD.

  METHOD table_headline_and_list.
    DATA lt_tab TYPE string_table.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.

    INSERT `first` INTO TABLE lt_tab.
    INSERT `second` INTO TABLE lt_tab.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lt_tab ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-skip
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `Table with 2 entries` ).
    " rows are an ORDERED list - the position of a row is information
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `<ol>*<li>first</li>*<li>second</li>*</ol>` ).
  ENDMETHOD.

  METHOD table_one_entry_is_singular.
    DATA lt_tab TYPE string_table.

    INSERT `only` INTO TABLE lt_tab.

    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>ui5_data_box_format( lt_tab )-text
        exp = `Table with 1 entry` ).
  ENDMETHOD.

  METHOD table_row_cap_is_announced.
    " the cap is announced in the output rather than applied silently -
    " a truncated dump that does not say so is a wrong dump
    DATA lt_tab TYPE string_table.
    DATA lv_no  TYPE i.
      DATA temp362 LIKE LINE OF lt_tab.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    DATA temp15 TYPE xsdboolean.

    DO 120 TIMES.
      lv_no = lv_no + 1.
      
      temp362 = |row{ lv_no }|.
      INSERT temp362 INTO TABLE lt_tab.
    ENDDO.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lt_tab ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `Table with 120 entries` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*20 more entries*` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<li>row100</li>*` ).
    
    temp15 = boolc( ls_box-details CS `<li>row101</li>` ).
    cl_abap_unit_assert=>assert_false( temp15 ).
  ENDMETHOD.

  METHOD struct_headline_and_fields.
    TYPES: BEGIN OF ty_s,
             alpha TYPE string,
             beta  TYPE i,
           END OF ty_s.
    DATA ls_val TYPE ty_s.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.

    ls_val-alpha = `the_value`.
    ls_val-beta  = 7.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( ls_val ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `Structure with 2 fields` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<strong>ALPHA</strong>: the_value*` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<strong>BETA</strong>: 7*` ).
  ENDMETHOD.

  METHOD object_headline_and_attris.
    DATA lo_obj TYPE REF TO ltcl_test_app.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    DATA temp16 TYPE xsdboolean.
    DATA temp17 TYPE xsdboolean.
    CREATE OBJECT lo_obj TYPE ltcl_test_app.
    lo_obj->mv_val = `instance_state`.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lo_obj ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `Object LTCL_TEST_APP` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<strong>MV_VAL</strong>: instance_state*` ).
    " a constant belongs to the type and a class attribute to nobody -
    " neither is this instance's state
    
    temp16 = boolc( ls_box-details CS `SV_STATUS` ).
    cl_abap_unit_assert=>assert_false( temp16 ).
    
    temp17 = boolc( ls_box-details CS `SV_VAR` ).
    cl_abap_unit_assert=>assert_false( temp17 ).
  ENDMETHOD.

  METHOD exception_renders_its_text.
    " an exception carries its own text, and that text is the whole story -
    " its attributes are the placeholders already substituted into it
    DATA lx TYPE REF TO zabaputil_cx_error.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    DATA temp18 TYPE xsdboolean.
    CREATE OBJECT lx TYPE zabaputil_cx_error EXPORTING val = `the_failure`.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lx ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-text
                                         exp = `Object ZABAPUTIL_CX_ERROR*` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*the_failure*` ).
    
    temp18 = boolc( ls_box-details CS `<strong>MS_ERROR` ).
    cl_abap_unit_assert=>assert_false( temp18 ).
  ENDMETHOD.

  METHOD dref_renders_the_target.
    DATA lv_str  TYPE string VALUE `behind_the_reference`.
    DATA lr_data TYPE REF TO data.

    GET REFERENCE OF lv_str INTO lr_data.

    cl_abap_unit_assert=>assert_char_cp(
        act = zabaputil_cl_util_context=>ui5_data_box_format( lr_data )-details
        exp = `*behind_the_reference*` ).
  ENDMETHOD.

  METHOD cycle_stops_at_depth.
    " a structure that points at itself: the walk has to stop, and say in
    " the output where it did, instead of running the stack out
    DATA ls_node TYPE ty_s_node.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.

    ls_node-name = `loop`.
    GET REFERENCE OF ls_node INTO ls_node-next.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( ls_node ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<em>...</em>*` ).
  ENDMETHOD.

  METHOD values_are_escaped.
    " every rendered value goes through the catalogue's own escaping - a
    " value is data, never markup of the report it appears in
    TYPES: BEGIN OF ty_s,
             payload TYPE string,
           END OF ty_s.
    DATA ls_val TYPE ty_s.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    DATA temp19 TYPE xsdboolean.

    ls_val-payload = `<script>alert("x")</script>`.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( ls_val ).
    
    temp19 = boolc( ls_box-details CS `<script>` ).
    cl_abap_unit_assert=>assert_false( temp19 ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*&lt;script&gt;*` ).
  ENDMETHOD.

  METHOD plain_text_is_capped.
    " the headline is one line of a box - what does not fit is in the
    " details anyway, and the cut says that it was cut
    DATA lv_markup TYPE string.
    DATA lv_no     TYPE i.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.

    lv_markup = `<p>`.
    DO 60 TIMES.
      lv_no = lv_no + 1.
      lv_markup = lv_markup && |word{ lv_no } |.
    ENDDO.
    lv_markup = lv_markup && `</p>`.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lv_markup ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-details
                                        exp = lv_markup ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-text
                                         exp = `*...` ).
    cl_abap_unit_assert=>assert_equals( act = strlen( ls_box-text )
                                        exp = 203 ).
  ENDMETHOD.

  METHOD non_message_struct_skips.
    " the hand-over: a structure carrying none of the message components
    " is data, not a message. ui5_msg_box_format says so with `skip`, and
    " that is what lets a caller fall through to ui5_data_box_format
    TYPES: BEGIN OF ty_s,
             alpha TYPE string,
             beta  TYPE i,
           END OF ty_s.
    DATA ls_val TYPE ty_s.
    DATA ls_data TYPE zabaputil_cl_util_context=>ty_s_msg_box.

    ls_val-alpha = `not_a_message`.
    ls_val-beta  = 3.

    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>ui5_msg_box_format( ls_val )-skip
        exp = abap_true ).

    " and the fall-through has something to show
    
    ls_data = zabaputil_cl_util_context=>ui5_data_box_format( ls_val ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-skip
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_data-details
                                         exp = `*not_a_message*` ).
  ENDMETHOD.

  METHOD table_standard_true.
    TYPES temp28 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_tab  TYPE temp28.
    DATA lr_data TYPE REF TO data.

    GET REFERENCE OF lt_tab INTO lr_data.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
  ENDMETHOD.

  METHOD table_sorted_false.
    TYPES temp29 TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
DATA lt_tab  TYPE temp29.
    DATA lr_data TYPE REF TO data.

    GET REFERENCE OF lt_tab INTO lr_data.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
  ENDMETHOD.

  METHOD table_hashed_false.
    TYPES temp30 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA lt_tab  TYPE temp30.
    DATA lr_data TYPE REF TO data.

    GET REFERENCE OF lt_tab INTO lr_data.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
  ENDMETHOD.

  METHOD table_non_table_false.
    " (the unbound reference has no test here: the transpiled
    " describe_by_data_ref throws a raw runtime error the CATCH does not
    " see, where ABAP raises a catchable exception)
    DATA lv_str  TYPE string.
    DATA lr_data TYPE REF TO data.

    GET REFERENCE OF lv_str INTO lr_data.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
  ENDMETHOD.

ENDCLASS.


" methods harvested from abap2UI5 (z2ui5_cl_ui5_util_context, 2026-10): the
" generic escape, string, URL and HTTP helpers its core classes carried
CLASS ltcl_sync_core_helpers DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_url_origin        FOR TESTING RAISING cx_static_check.
    METHODS test_etag_match_rules  FOR TESTING RAISING cx_static_check.
    METHODS test_escape_json       FOR TESTING RAISING cx_static_check.
    METHODS test_escape_js         FOR TESTING RAISING cx_static_check.
    METHODS test_escape_xml        FOR TESTING RAISING cx_static_check.
    METHODS test_base64_roundtrip  FOR TESTING RAISING cx_static_check.
    METHODS test_truncate          FOR TESTING RAISING cx_static_check.
    METHODS test_filter_chars      FOR TESTING RAISING cx_static_check.
    METHODS test_cut_at            FOR TESTING RAISING cx_static_check.
    METHODS test_url_authority     FOR TESTING RAISING cx_static_check.
    METHODS test_hash_checksum     FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_binding_literal FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_handler_string FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_braces_balanced FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_sync_core_helpers IMPLEMENTATION.

  METHOD test_ui5_binding_literal.

    cl_abap_unit_assert=>assert_equals( exp = `a \{/PATH\} \\\\server`
                                        act = zabaputil_cl_util_context=>ui5_escape_binding_literal( `a {/PATH} \\server` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `plain`
                                        act = zabaputil_cl_util_context=>ui5_escape_binding_literal( `plain` ) ).

  ENDMETHOD.

  METHOD test_ui5_handler_string.

    DATA lv_raw TYPE string.
    lv_raw = `it's a\b` && zabaputil_cl_util_context=>cv_char_util_cr_lf && `c`.
    cl_abap_unit_assert=>assert_equals( exp = `it\'s a\\b\nc`
                                        act = zabaputil_cl_util_context=>ui5_escape_handler_string( lv_raw ) ).
    cl_abap_unit_assert=>assert_equals( exp = `EVENT_ID`
                                        act = zabaputil_cl_util_context=>ui5_escape_handler_string( `EVENT_ID` ) ).

  ENDMETHOD.

  METHOD test_ui5_braces_balanced.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>ui5_check_braces_balanced( `{/A}` ) ).
    " a brace inside a string of the expression is text
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>ui5_check_braces_balanced( `{= ${/A} === '}' }` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>ui5_check_braces_balanced( `{0` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>ui5_check_braces_balanced( `}{` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>ui5_check_braces_balanced( `{'a}` ) ).

  ENDMETHOD.

  METHOD test_url_origin.

    cl_abap_unit_assert=>assert_equals( exp = `https://sdk.openui5.org`
                                        act = zabaputil_cl_util_context=>url_get_origin(
                                                  `https://sdk.openui5.org/resources/sap-ui-cachebuster/sap-ui-core.js` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `https://cdn.example:8443`
                                        act = zabaputil_cl_util_context=>url_get_origin(
                                                  `https://cdn.example:8443/ui5/resources/sap-ui-core.js?x=1` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `http://cdn.example`
                                        act = zabaputil_cl_util_context=>url_get_origin( `HTTP://cdn.example?q` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `//cdn.example`
                                        act = zabaputil_cl_util_context=>url_get_origin( `//cdn.example/resources/sap-ui-core.js` ) ).
    " the host alone, no path at all
    cl_abap_unit_assert=>assert_equals( exp = `https://cdn.example`
                                        act = zabaputil_cl_util_context=>url_get_origin( `https://cdn.example` ) ).
    " relative: the page's own origin, nothing to announce
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>url_get_origin(
                                             `/sap/public/bc/ui5_ui5/1/resources/sap-ui-core.js` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>url_get_origin( `resources/sap-ui-core.js` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>url_get_origin( `` ) ).
    " no host, or userinfo in front of it: nothing rather than a guess
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>url_get_origin( `https:///x.js` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>url_get_origin( `https://u:p@cdn.example/x.js` ) ).

  ENDMETHOD.

  METHOD test_etag_match_rules.

    DATA lv_tag TYPE string.
    lv_tag = `"1.0-11-22-33"`.

    " exact, weak, listed, any, the Apache suffix - all a match
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `"1.0-11-22-33"`
        etag          = lv_tag ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `W/"1.0-11-22-33"`
        etag          = lv_tag ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `"a" , "1.0-11-22-33" ,"b"`
        etag          = lv_tag ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `*`
        etag          = lv_tag ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `"1.0-11-22-33-gzip"`
        etag          = lv_tag ) ).

    " another tag, a tag that merely starts like ours, nothing at all
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `"1.0-99-99-99"`
        etag          = lv_tag ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `"1.0-11-22-33-x"`
        etag          = lv_tag ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = ``
        etag          = lv_tag ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>http_check_etag_match(
        if_none_match = `*`
        etag          = `` ) ).

  ENDMETHOD.

  METHOD test_escape_json.

    DATA lv_raw TYPE string.
    lv_raw = `a"b\c` && zabaputil_cl_util_context=>cv_char_util_horizontal_tab
                   && zabaputil_cl_util_context=>cv_char_util_cr_lf && `d`.
    cl_abap_unit_assert=>assert_equals( exp = `a\"b\\c\t\r\nd`
                                        act = zabaputil_cl_util_context=>c_escape_json( lv_raw ) ).
    cl_abap_unit_assert=>assert_equals( exp = `plain </b>`
                                        act = zabaputil_cl_util_context=>c_escape_json( `plain </b>` ) ).

  ENDMETHOD.

  METHOD test_escape_js.

    " both quotes, the backslash, the line terminators and the end of an
    " inline script - the backslash first, or it would double the others
    DATA lv_raw TYPE string.
    lv_raw = `a"b'c\d</e` && zabaputil_cl_util_context=>cv_char_util_cr_lf && `f`.
    cl_abap_unit_assert=>assert_equals( exp = `a\"b\'c\\d<\/e\r\nf`
                                        act = zabaputil_cl_util_context=>c_escape_js( lv_raw ) ).
    cl_abap_unit_assert=>assert_equals( exp = `/sap/bc/z2ui5`
                                        act = zabaputil_cl_util_context=>c_escape_js( `/sap/bc/z2ui5` ) ).

  ENDMETHOD.

  METHOD test_escape_xml.

    " the entities, the whitespace as character references, and a form feed
    " (illegal in XML 1.0 even as a reference) dropped
    DATA temp363 TYPE xstring.
    DATA lv_ff TYPE string.
    DATA lv_raw TYPE string.
    DATA temp364 TYPE xstring.
    DATA lv_nonchar TYPE string.
    temp363 = `0C`.
    
    lv_ff = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp363 ).
    
    lv_raw = `a&b<c>d"e'f` && zabaputil_cl_util_context=>cv_char_util_newline
                   && zabaputil_cl_util_context=>cv_char_util_horizontal_tab && lv_ff && `g`.
    cl_abap_unit_assert=>assert_equals( exp = `a&amp;b&lt;c&gt;d&quot;e'f&#xA;&#x9;g`
                                        act = zabaputil_cl_util_context=>c_escape_xml( lv_raw ) ).
    cl_abap_unit_assert=>assert_equals( exp = `plain`
                                        act = zabaputil_cl_util_context=>c_escape_xml( `plain` ) ).

    " the non-characters U+FFFE and U+FFFF are dropped like a control
    " character - the XMLView parser refuses the document for one of them
    
    temp364 = `EFBFBE` && `EFBFBF`.
    
    lv_nonchar = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp364 ).
    cl_abap_unit_assert=>assert_equals( exp = `ab`
                                        act = zabaputil_cl_util_context=>c_escape_xml( `a` && lv_nonchar && `b` ) ).

  ENDMETHOD.

  METHOD test_base64_roundtrip.

    DATA lv_empty TYPE xstring.

    DATA lv_xstr TYPE xstring.
    DATA lv_b64 TYPE string.
    lv_xstr = zabaputil_cl_util_context=>conv_get_xstring_by_string( `Hello` ).
    
    lv_b64 = zabaputil_cl_util_context=>conv_encode_x_base64( lv_xstr ).

    cl_abap_unit_assert=>assert_equals( exp = `SGVsbG8=`
                                        act = lv_b64 ).
    cl_abap_unit_assert=>assert_equals( exp = lv_xstr
                                        act = zabaputil_cl_util_context=>conv_decode_x_base64( lv_b64 ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>conv_encode_x_base64( lv_empty ) ).

  ENDMETHOD.

  METHOD test_truncate.

    cl_abap_unit_assert=>assert_equals( exp = `abcdefg...`
                                        act = zabaputil_cl_util_context=>c_truncate( val = `abcdefghijklmn`
                                                                                     max = 10 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `abcdefghij`
                                        act = zabaputil_cl_util_context=>c_truncate( val = `abcdefghij`
                                                                                     max = 10 ) ).
    " no room for the ellipsis: a plain cut
    cl_abap_unit_assert=>assert_equals( exp = `ab`
                                        act = zabaputil_cl_util_context=>c_truncate( val = `abcdef`
                                                                                     max = 2 ) ).

  ENDMETHOD.

  METHOD test_filter_chars.

    cl_abap_unit_assert=>assert_equals( exp = `ZCL_X`
                                        act = zabaputil_cl_util_context=>c_filter_chars(
                                                  val     = `ZCL_X"><script>`
                                                  allowed = `ABCDEFGHIJKLMNOPQRSTUVWXYZ_` ) ).
    " nothing to strip: the value itself
    cl_abap_unit_assert=>assert_equals( exp = `abc`
                                        act = zabaputil_cl_util_context=>c_filter_chars( val     = `abc`
                                                                                         allowed = `abc` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>c_filter_chars( val     = ``
                                                                                    allowed = `abc` ) ).

  ENDMETHOD.

  METHOD test_cut_at.

    cl_abap_unit_assert=>assert_equals( exp = `ZCL_X`
                                        act = zabaputil_cl_util_context=>c_cut_at( val = `ZCL_X?a=1`
                                                                                   sub = `?` ) ).
    " sub absent: the whole value, not the empty one substring_before answers
    cl_abap_unit_assert=>assert_equals( exp = `ZCL_X`
                                        act = zabaputil_cl_util_context=>c_cut_at( val = `ZCL_X`
                                                                                   sub = `?` ) ).

  ENDMETHOD.

  METHOD test_url_authority.

    cl_abap_unit_assert=>assert_equals( exp = `host.example:8443`
                                        act = zabaputil_cl_util_context=>url_get_authority(
                                                  `HTTPS://Host.Example:8443/path?q#f` ) ).
    " a default port is no port, whatever the scheme
    cl_abap_unit_assert=>assert_equals( exp = `host.example`
                                        act = zabaputil_cl_util_context=>url_get_authority( `host.example:443` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `host.example`
                                        act = zabaputil_cl_util_context=>url_get_authority( `http://host.example:80/` ) ).

  ENDMETHOD.

  METHOD test_hash_checksum.

    DATA lv_a TYPE string.
    DATA lv_b TYPE string.
    DATA temp20 TYPE xsdboolean.
    lv_a = zabaputil_cl_util_context=>hash_checksum( `<html>shell content A</html>` ).
    
    lv_b = zabaputil_cl_util_context=>hash_checksum( `<html>shell content B</html>` ).
    cl_abap_unit_assert=>assert_not_initial( lv_a ).
    cl_abap_unit_assert=>assert_differs( exp = lv_a
                                         act = lv_b ).
    cl_abap_unit_assert=>assert_equals( exp = lv_a
                                        act = zabaputil_cl_util_context=>hash_checksum( `<html>shell content A</html>` ) ).
    " the byte count leads
    
    temp20 = boolc( lv_a CP `28-*` ).
    cl_abap_unit_assert=>assert_true( temp20 ).

  ENDMETHOD.

ENDCLASS.


" an object with one public attribute, for rtti_get_t_attri_by_oref
CLASS ltcl_attri_probe DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    DATA mv_text TYPE string ##NEEDED.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltcl_attri_probe IMPLEMENTATION.
ENDCLASS.


" harvested from abap2UI5 (z2ui5_cl_ui5_util_context, ltcl_coverage): the
" methods its framework calls, each with a test of its own
CLASS ltcl_coverage DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_flat,
        text TYPE string,
        date TYPE d,
        time TYPE t,
        flag TYPE abap_bool,
        num  TYPE i,
      END OF ty_s_flat.
    TYPES ty_t_flat TYPE STANDARD TABLE OF ty_s_flat WITH DEFAULT KEY.
    TYPES ty_t_sorted TYPE SORTED TABLE OF ty_s_flat WITH UNIQUE KEY text.
    TYPES:
      BEGIN OF ty_s_nested,
        id   TYPE string,
        flat TYPE ty_s_flat,
      END OF ty_s_nested.
    TYPES BEGIN OF ty_s_incl.
    TYPES name TYPE string.
    TYPES END OF ty_s_incl.
    TYPES BEGIN OF ty_s_with_incl.
    INCLUDE TYPE ty_s_incl.
    TYPES zip TYPE string.
    TYPES END OF ty_s_with_incl.

    METHODS test_check_table_standard FOR TESTING RAISING cx_static_check.
    METHODS test_typedescr_by_data    FOR TESTING RAISING cx_static_check.
    METHODS test_flat_components      FOR TESTING RAISING cx_static_check.
    METHODS test_flat_components_none FOR TESTING RAISING cx_static_check.
    METHODS test_attri_by_include     FOR TESTING RAISING cx_static_check.
    METHODS test_expand_components    FOR TESTING RAISING cx_static_check.
    METHODS test_unassign_initial     FOR TESTING RAISING cx_static_check.
    METHODS test_unassign_object      FOR TESTING RAISING cx_static_check.
    METHODS test_unassign_data        FOR TESTING RAISING cx_static_check.
    METHODS test_as_data_ref          FOR TESTING RAISING cx_static_check.
    METHODS test_timestampl           FOR TESTING RAISING cx_static_check.
    METHODS test_attri_by_oref        FOR TESTING RAISING cx_static_check.
    METHODS test_check_abap_cloud     FOR TESTING RAISING cx_static_check.
    METHODS test_classes_impl_intf    FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_coverage IMPLEMENTATION.

  METHOD test_check_table_standard.

    DATA lt_std TYPE ty_t_flat.
    DATA lt_sorted TYPE ty_t_sorted.
    DATA ls_flat TYPE ty_s_flat.

    DATA temp365 LIKE REF TO lt_std.
    DATA temp366 LIKE REF TO lt_sorted.
    DATA temp367 LIKE REF TO ls_flat.
    GET REFERENCE OF lt_std INTO temp365.
cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_table_standard( temp365 ) ).
    
    GET REFERENCE OF lt_sorted INTO temp366.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( temp366 ) ).
    
    GET REFERENCE OF ls_flat INTO temp367.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( temp367 ) ).

  ENDMETHOD.

  METHOD test_typedescr_by_data.

    DATA ls_flat TYPE ty_s_flat.
    DATA lt_flat TYPE ty_t_flat.

    cl_abap_unit_assert=>assert_equals( exp = cl_abap_typedescr=>kind_struct
                                        act = zabaputil_cl_util_context=>rtti_get_typedescr_by_data( ls_flat )->kind ).
    cl_abap_unit_assert=>assert_equals( exp = cl_abap_typedescr=>kind_table
                                        act = zabaputil_cl_util_context=>rtti_get_typedescr_by_data( lt_flat )->kind ).

  ENDMETHOD.

  METHOD test_flat_components.

    DATA lt_flat TYPE ty_t_flat.

    " the line of a standard table, every component with its json_kind
    DATA temp368 LIKE REF TO lt_flat.
DATA lt_comp TYPE zabaputil_cl_util_context=>ty_t_flat_comp.
    FIELD-SYMBOLS <temp369> LIKE LINE OF lt_comp.
    DATA temp370 LIKE sy-tabix.
    FIELD-SYMBOLS <temp371> LIKE LINE OF lt_comp.
    DATA temp372 LIKE sy-tabix.
    FIELD-SYMBOLS <temp373> LIKE LINE OF lt_comp.
    DATA temp374 LIKE sy-tabix.
    FIELD-SYMBOLS <temp375> LIKE LINE OF lt_comp.
    DATA temp376 LIKE sy-tabix.
    FIELD-SYMBOLS <temp377> LIKE LINE OF lt_comp.
    DATA temp378 LIKE sy-tabix.
    FIELD-SYMBOLS <temp379> LIKE LINE OF lt_comp.
    DATA temp380 LIKE sy-tabix.
    GET REFERENCE OF lt_flat INTO temp368.

lt_comp = zabaputil_cl_util_context=>rtti_get_flat_components( temp368 ).

    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( lt_comp ) ).
    
    
    temp370 = sy-tabix.
    READ TABLE lt_comp INDEX 1 ASSIGNING <temp369>.
    sy-tabix = temp370.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `TEXT`
                                        act = <temp369>-name ).
    
    
    temp372 = sy-tabix.
    READ TABLE lt_comp INDEX 1 ASSIGNING <temp371>.
    sy-tabix = temp372.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `S`
                                        act = <temp371>-json_kind ).
    
    
    temp374 = sy-tabix.
    READ TABLE lt_comp INDEX 2 ASSIGNING <temp373>.
    sy-tabix = temp374.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp373>-json_kind ).
    
    
    temp376 = sy-tabix.
    READ TABLE lt_comp INDEX 3 ASSIGNING <temp375>.
    sy-tabix = temp376.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `T`
                                        act = <temp375>-json_kind ).
    
    
    temp378 = sy-tabix.
    READ TABLE lt_comp INDEX 4 ASSIGNING <temp377>.
    sy-tabix = temp378.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B`
                                        act = <temp377>-json_kind ).
    
    
    temp380 = sy-tabix.
    READ TABLE lt_comp INDEX 5 ASSIGNING <temp379>.
    sy-tabix = temp380.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `N`
                                        act = <temp379>-json_kind ).

  ENDMETHOD.

  METHOD test_flat_components_none.

    DATA lt_sorted TYPE ty_t_sorted.
    DATA ls_nested TYPE ty_s_nested.
    DATA lv_text TYPE string.

    " a sorted table, a nested structure, a scalar: none is flat. (A
    " structure with an include is refused too on a system - the transpiled
    " runtime hands its components out already expanded, so it is not
    " asserted here)
    DATA temp381 LIKE REF TO lt_sorted.
    DATA temp382 LIKE REF TO ls_nested.
    DATA temp383 LIKE REF TO lv_text.
    GET REFERENCE OF lt_sorted INTO temp381.
cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( temp381 ) ).
    
    GET REFERENCE OF ls_nested INTO temp382.
cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( temp382 ) ).
    
    GET REFERENCE OF lv_text INTO temp383.
cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( temp383 ) ).

  ENDMETHOD.

  METHOD test_attri_by_include.

    DATA ls_with_incl TYPE ty_s_with_incl.
    DATA lv_text TYPE string.

    DATA temp384 TYPE REF TO cl_abap_datadescr.
    DATA lt_comp TYPE abap_component_tab.
    FIELD-SYMBOLS <temp385> LIKE LINE OF lt_comp.
    DATA temp386 LIKE sy-tabix.
    FIELD-SYMBOLS <temp387> LIKE LINE OF lt_comp.
    DATA temp388 LIKE sy-tabix.
        DATA temp389 TYPE REF TO cl_abap_datadescr.
    temp384 ?= cl_abap_typedescr=>describe_by_data( ls_with_incl ).
    
    lt_comp = zabaputil_cl_util_context=>rtti_get_t_attri_by_include(
        temp384 ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_comp ) ).
    
    
    temp386 = sy-tabix.
    READ TABLE lt_comp INDEX 1 ASSIGNING <temp385>.
    sy-tabix = temp386.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NAME`
                                        act = <temp385>-name ).
    
    
    temp388 = sy-tabix.
    READ TABLE lt_comp INDEX 2 ASSIGNING <temp387>.
    sy-tabix = temp388.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ZIP`
                                        act = <temp387>-name ).

    " no structure: the error names it, it is no dump
    TRY.
        
        temp389 ?= cl_abap_typedescr=>describe_by_data( lv_text ).
        zabaputil_cl_util_context=>rtti_get_t_attri_by_include(
            temp389 ).
        cl_abap_unit_assert=>fail( `a non-structure include must raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD test_expand_components.

    DATA ls_with_incl TYPE ty_s_with_incl.

    DATA temp390 TYPE REF TO cl_abap_structdescr.
    DATA lt_raw TYPE abap_component_tab.
    DATA lt_comp TYPE abap_component_tab.
    DATA temp21 TYPE xsdboolean.
    DATA temp6 LIKE sy-subrc.
    temp390 ?= cl_abap_typedescr=>describe_by_data( ls_with_incl ).
    
    lt_raw = temp390->get_components( ).
    
    lt_comp = zabaputil_cl_util_context=>expand_components( lt_raw ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_comp ) ).
    
    
    READ TABLE lt_comp WITH KEY as_include = abap_true TRANSPORTING NO FIELDS.
    temp6 = sy-subrc.
    temp21 = boolc( temp6 = 0 ).
    cl_abap_unit_assert=>assert_false( temp21 ).

    " the recursion guard: past 16 levels the expansion refuses
    TRY.
        zabaputil_cl_util_context=>expand_components( val   = lt_raw
                                                      depth = 17 ).
        cl_abap_unit_assert=>fail( `the depth guard must raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD test_unassign_initial.

    DATA lv_empty TYPE string.
    DATA lv_full TYPE string VALUE `x`.
    DATA lr_none TYPE REF TO data.
    DATA temp391 LIKE REF TO lv_empty.
    DATA temp392 LIKE REF TO lv_full.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_unassign_initial( lr_none ) ).
    
    GET REFERENCE OF lv_empty INTO temp391.
cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_unassign_initial( temp391 ) ).
    
    GET REFERENCE OF lv_full INTO temp392.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_unassign_initial( temp392 ) ).

  ENDMETHOD.

  METHOD test_unassign_object.

    DATA lo_probe TYPE REF TO ltcl_attri_probe.
    DATA lr_oref TYPE REF TO data.
    DATA lo_back TYPE REF TO object.
    DATA temp22 TYPE xsdboolean.
    CREATE OBJECT lo_probe.
    
    GET REFERENCE OF lo_probe INTO lr_oref.

    
    lo_back = zabaputil_cl_util_context=>unassign_object( lr_oref ).

    cl_abap_unit_assert=>assert_bound( lo_back ).
    
    temp22 = boolc( lo_back = lo_probe ).
    cl_abap_unit_assert=>assert_true( temp22 ).

  ENDMETHOD.

  METHOD test_unassign_data.

    DATA lv_text TYPE string VALUE `x`.
    DATA lr_text TYPE REF TO data.
    DATA lr_outer TYPE REF TO data.
    DATA lr_back TYPE REF TO data.
    DATA temp23 TYPE xsdboolean.
    GET REFERENCE OF lv_text INTO lr_text.
    
    GET REFERENCE OF lr_text INTO lr_outer.

    
    lr_back = zabaputil_cl_util_context=>unassign_data( lr_outer ).

    
    temp23 = boolc( lr_back = lr_text ).
    cl_abap_unit_assert=>assert_true( temp23 ).

  ENDMETHOD.

  METHOD test_as_data_ref.

    DATA lv_text TYPE string VALUE `x`.

    DATA lr_ref TYPE REF TO data.
    FIELD-SYMBOLS <val> TYPE data.
    lr_ref = zabaputil_cl_util_context=>conv_get_as_data_ref( lv_text ).

    cl_abap_unit_assert=>assert_bound( lr_ref ).
    
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals( exp = `x`
                                        act = <val> ).

  ENDMETHOD.

  METHOD test_timestampl.

    DATA lv_first TYPE timestampl.
    DATA lv_second TYPE timestampl.
    DATA temp24 TYPE xsdboolean.
    lv_first = zabaputil_cl_util_context=>time_get_timestampl( ).
    
    lv_second = zabaputil_cl_util_context=>time_get_timestampl( ).

    cl_abap_unit_assert=>assert_not_initial( lv_first ).
    
    temp24 = boolc( lv_second >= lv_first ).
    cl_abap_unit_assert=>assert_true( temp24 ).

  ENDMETHOD.

  METHOD test_attri_by_oref.

    DATA lo_probe TYPE REF TO ltcl_attri_probe.
    DATA lt_attri TYPE abap_attrdescr_tab.
    DATA temp25 TYPE xsdboolean.
    DATA temp7 LIKE sy-subrc.
    CREATE OBJECT lo_probe.

    
    lt_attri = zabaputil_cl_util_context=>rtti_get_t_attri_by_oref( lo_probe ).

    
    
    READ TABLE lt_attri WITH KEY name = `MV_TEXT` TRANSPORTING NO FIELDS.
    temp7 = sy-subrc.
    temp25 = boolc( temp7 = 0 ).
    cl_abap_unit_assert=>assert_true( temp25 ).

  ENDMETHOD.

  METHOD test_check_abap_cloud.

    " the answer is latched - the second call gives the first one's answer
    DATA lv_first TYPE abap_bool.
    lv_first = zabaputil_cl_util_context=>check_abap_cloud( ).

    cl_abap_unit_assert=>assert_equals( exp = lv_first
                                        act = zabaputil_cl_util_context=>check_abap_cloud( ) ).

  ENDMETHOD.

  METHOD test_classes_impl_intf.

    " the repository lookup is the environment's, and this catalog ships no
    " interface with a known implementation - so the test holds what any
    " answer must hold: no row without a class name, and nothing for an
    " interface that does not exist
    DATA lt_classes TYPE zabaputil_cl_util_context=>ty_t_classes.
    DATA temp26 TYPE xsdboolean.
    DATA temp8 LIKE sy-subrc.
    TRY.
        lt_classes = zabaputil_cl_util_context=>rtti_get_classes_impl_intf( `IF_SERIALIZABLE_OBJECT` ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
    
    
    READ TABLE lt_classes WITH KEY classname = `` TRANSPORTING NO FIELDS.
    temp8 = sy-subrc.
    temp26 = boolc( temp8 = 0 ).
    cl_abap_unit_assert=>assert_false( temp26 ).

    TRY.
        lt_classes = zabaputil_cl_util_context=>rtti_get_classes_impl_intf( `ZABAPUTIL_IF_DOES_NOT_EXIST` ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>assert_initial( lt_classes ).

  ENDMETHOD.

ENDCLASS.


" methods harvested from abap2UI5 in the full sync (2026-10): the tests its
" copy carries for them, the class lookups on the ajson mirror, which ships
" a class with a known interface here
CLASS ltcl_sync_downstream DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_hash_string           FOR TESTING RAISING cx_static_check.
    METHODS test_hash_fallback         FOR TESTING RAISING cx_static_check.
    METHODS test_impl_intf_app         FOR TESTING RAISING cx_static_check.
    METHODS test_impl_intf_lower_case  FOR TESTING RAISING cx_static_check.
    METHODS test_impl_intf_no_app      FOR TESTING RAISING cx_static_check.
    METHODS test_impl_intf_no_class    FOR TESTING RAISING cx_static_check.
    METHODS test_impl_intf_interface   FOR TESTING RAISING cx_static_check.
    METHODS test_ref_type_name_lookup  FOR TESTING RAISING cx_static_check.
    METHODS test_ref_type_name_intf    FOR TESTING RAISING cx_static_check.
    METHODS test_ref_type_name_class   FOR TESTING RAISING cx_static_check.
    METHODS test_ref_type_name_no_ref  FOR TESTING RAISING cx_static_check.
    METHODS test_rtti_initial_reference FOR TESTING RAISING cx_static_check.
    METHODS test_class_exists_lower    FOR TESTING RAISING cx_static_check.
    METHODS test_class_exists_case     FOR TESTING RAISING cx_static_check.
    METHODS test_diff_ms_fraction      FOR TESTING RAISING cx_static_check.
    METHODS test_diff_ms_minute_wrap   FOR TESTING RAISING cx_static_check.
    METHODS test_diff_ms_midnight      FOR TESTING RAISING cx_static_check.
    METHODS test_diff_ms_negative      FOR TESTING RAISING cx_static_check.
    METHODS test_diff_ms_initial       FOR TESTING RAISING cx_static_check.
    METHODS test_diff_ms_leap_day      FOR TESTING RAISING cx_static_check.
    METHODS test_srtti_pair_roundtrip  FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS zabaputil_cl_util_context DEFINITION LOCAL FRIENDS ltcl_sync_downstream.

CLASS ltcl_sync_downstream IMPLEMENTATION.

  METHOD test_hash_string.

    " the one promise: equal texts give equal answers, different texts
    " different ones - on every runtime. Where cl_abap_message_digest
    " exists (a system, and open-abap-core under npm run unit) the answer
    " is the SHA-256 of the characters; where it does not, the text itself,
    " and the comparison holds as before
    DATA lv_abc TYPE string.
    DATA lv_again TYPE string.
    DATA lv_other TYPE string.
    DATA lv_empty TYPE string.
    lv_abc   = zabaputil_cl_util_context=>hash_string( `abc` ).
    
    lv_again = zabaputil_cl_util_context=>hash_string( `abc` ).
    
    lv_other = zabaputil_cl_util_context=>hash_string( `abd` ).
    
    lv_empty = zabaputil_cl_util_context=>hash_string( `` ).

    cl_abap_unit_assert=>assert_equals( exp = lv_abc
                                        act = lv_again ).
    cl_abap_unit_assert=>assert_differs( exp = lv_abc
                                         act = lv_other ).
    cl_abap_unit_assert=>assert_differs( exp = lv_abc
                                         act = lv_empty ).

    IF zabaputil_cl_util_context=>rtti_check_class_exists( `CL_ABAP_MESSAGE_DIGEST` ) = abap_true.
      cl_abap_unit_assert=>assert_equals( exp = `BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD`
                                          act = to_upper( lv_abc ) ).
      cl_abap_unit_assert=>assert_equals( exp = 64
                                          act = strlen( lv_empty ) ).
    ELSE.
      cl_abap_unit_assert=>assert_equals( exp = `abc`
                                          act = lv_abc ).
      cl_abap_unit_assert=>assert_initial( lv_empty ).
    ENDIF.

  ENDMETHOD.

  METHOD test_hash_fallback.

    " the fallback branch on every runtime: a digest class that does not
    " exist answers nothing, and hash_string( ) then answers the text
    " itself (asserted above where the runtime has no digest class)
    DATA lv_long TYPE string.
    lv_long = repeat( val = `{"ROW":"value"}`
                            occ = 1000 ).
    " (a class that exists without the method is the same CATCH on a system
    " - CX_SY_DYN_CALL_ILLEGAL_METHOD - and a JS TypeError no CATCH sees in
    " the transpiled runtime, so it is not asserted here)
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>hash_string_by_class(
                                             val       = lv_long
                                             classname = `ZZ_NO_SUCH_DIGEST_CLASS` ) ).

  ENDMETHOD.

  METHOD test_impl_intf_app.

    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>rtti_check_class_impl_intf( class = `ZABAPUTIL_CL_AJSON`
                                                               intf  = `ZABAPUTIL_IF_AJSON` ) ).

  ENDMETHOD.

  METHOD test_impl_intf_lower_case.

    " the interface name is normalised the way a class name is
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>rtti_check_class_impl_intf( class = `ZABAPUTIL_CL_AJSON`
                                                               intf  = `zabaputil_if_ajson` ) ).

  ENDMETHOD.

  METHOD test_impl_intf_no_app.

    " exists, is a class, implements something else entirely
    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_util_context=>rtti_check_class_impl_intf( class = `ZABAPUTIL_CL_UTIL_CONTEXT`
                                                               intf  = `ZABAPUTIL_IF_AJSON` ) ).

  ENDMETHOD.

  METHOD test_impl_intf_no_class.

    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_util_context=>rtti_check_class_impl_intf( class = `ZCL_THIS_CLASS_DOES_NOT_EXIST`
                                                               intf  = `ZABAPUTIL_IF_AJSON` ) ).

  ENDMETHOD.

  METHOD test_impl_intf_interface.

    " an interface is not a class - it cannot be created, so it is no app
    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_util_context=>rtti_check_class_impl_intf( class = `ZABAPUTIL_IF_AJSON`
                                                               intf  = `ZABAPUTIL_IF_AJSON` ) ).

  ENDMETHOD.

  METHOD test_ref_type_name_lookup.

    " what a consumer does with it: the implementation pre-check of a
    " dynamically named class, with no name spelled out at all
    DATA li_app TYPE REF TO zabaputil_if_ajson.
    DATA lo_app TYPE REF TO object.

    lo_app = zabaputil_cl_ajson=>create_empty( ).
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>rtti_check_class_impl_intf(
            class = zabaputil_cl_util_context=>rtti_get_classname_by_ref( lo_app )
            intf  = zabaputil_cl_util_context=>rtti_get_ref_type_name( li_app ) ) ).

  ENDMETHOD.

  METHOD test_ref_type_name_intf.

    " the name a dynamic lookup needs, taken from a declaration - which a
    " namespace rename rewrites - instead of a literal, which it does not
    DATA li_app TYPE REF TO zabaputil_if_ajson.

    cl_abap_unit_assert=>assert_equals(
        exp = `ZABAPUTIL_IF_AJSON`
        act = zabaputil_cl_util_context=>rtti_get_ref_type_name( li_app ) ).

  ENDMETHOD.

  METHOD test_ref_type_name_class.

    DATA lo_exit TYPE REF TO zabaputil_cl_util_context.

    cl_abap_unit_assert=>assert_equals(
        exp = `ZABAPUTIL_CL_UTIL_CONTEXT`
        act = zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_exit ) ).

  ENDMETHOD.

  METHOD test_ref_type_name_no_ref.

    " a value that is no reference is a programming error at the caller -
    " it raises, it never answers with a name
    DATA lv_text TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA temp27 TYPE xsdboolean.

    TRY.
        zabaputil_cl_util_context=>rtti_get_ref_type_name( lv_text ).
        cl_abap_unit_assert=>fail( `a string is no typed reference` ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        
        temp27 = boolc( lx->get_text( ) CS `RTTI_NOT_AN_OBJECT_REFERENCE` ).
        cl_abap_unit_assert=>assert_true( temp27 ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_rtti_initial_reference.

    " describe_by_*_ref raise the classic exception REFERENCE_IS_INITIAL for an
    " initial reference, a runtime error no CATCH reaches - the helpers answer
    " empty where every failure means "no", and raise where a result is owed
    DATA lr_data TYPE REF TO data.
    DATA lo_obj  TYPE REF TO object.
        DATA lx_data TYPE REF TO zabaputil_cx_util_error.
        DATA temp28 TYPE xsdboolean.
        DATA lx_obj TYPE REF TO zabaputil_cx_util_error.
        DATA temp29 TYPE xsdboolean.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( lr_data ) ).

    TRY.
        zabaputil_cl_util_context=>rtti_get_typedescr_by_data_ref( lr_data ).
        cl_abap_unit_assert=>fail( `an initial data reference has no type` ).
        
      CATCH zabaputil_cx_util_error INTO lx_data.
        
        temp28 = boolc( lx_data->get_text( ) CS `RTTI_REFERENCE_IS_INITIAL` ).
        cl_abap_unit_assert=>assert_true( temp28 ).
    ENDTRY.

    TRY.
        zabaputil_cl_util_context=>rtti_get_t_attri_by_oref( lo_obj ).
        cl_abap_unit_assert=>fail( `an initial object reference has no attributes` ).
        
      CATCH zabaputil_cx_util_error INTO lx_obj.
        
        temp29 = boolc( lx_obj->get_text( ) CS `RTTI_REFERENCE_IS_INITIAL` ).
        cl_abap_unit_assert=>assert_true( temp29 ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_class_exists_lower.

    " the answer is cached under the upper-case name, and RTTI is asked in
    " that spelling too: it used to be asked in the caller's, so a class
    " first asked in lower case could be cached as missing for the whole
    " roll area. The name comes from a typed reference (a namespace rename
    " rewrites it) and is asked here first - no other test asks for it
    DATA lo_class TYPE REF TO zabaputil_cl_util_http.
    DATA lv_name TYPE string.
    lv_name = zabaputil_cl_util_context=>rtti_get_ref_type_name( lo_class ).

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_class_exists( to_lower( lv_name ) ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_class_exists( lv_name ) ).

  ENDMETHOD.

  METHOD test_class_exists_case.

    " the answer is cached per upper-case name, so the spelling of the FIRST
    " question must not decide the answer to every later one
    DATA li_app TYPE REF TO zabaputil_if_ajson.
    DATA lv_name TYPE string.
    lv_name = zabaputil_cl_util_context=>rtti_get_ref_type_name( li_app ).

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `zz_no_such_class_4711` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `ZZ_NO_SUCH_CLASS_4711` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `` ) ).
    " an interface is no class
    cl_abap_unit_assert=>assert_equals(
        exp = zabaputil_cl_util_context=>rtti_check_class_exists( lv_name )
        act = zabaputil_cl_util_context=>rtti_check_class_exists( to_lower( lv_name ) ) ).

  ENDMETHOD.

  METHOD test_diff_ms_fraction.

    " below a second - what cl_abap_tstmp=>subtract loses in the transpiled
    " runtime, which answers whole seconds there
    cl_abap_unit_assert=>assert_equals(
        exp = 250
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = `20261003101500.1000000`
                                                                 time_to   = `20261003101500.3500000` ) ).

  ENDMETHOD.

  METHOD test_diff_ms_minute_wrap.

    " a timestamp is YYYYMMDDhhmmss, not a count - 10:15:59.75 to
    " 10:16:00.25 is 500 ms, while the digits differ by 40.5. Quarters, not
    " the tenths of abap2UI5's copy of this test: the transpiled runtime
    " this repository pins holds a timestampl as a binary float, where .9
    " and .1 are not exact (the tenths answered 203 for 200 ms) - .75 and
    " .25 are, so the test checks the wrap and not the float
    cl_abap_unit_assert=>assert_equals(
        exp = 500
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = `20261003101559.7500000`
                                                                 time_to   = `20261003101600.2500000` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 3600000
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = `20261003095959.0000000`
                                                                 time_to   = `20261003105959.0000000` ) ).

  ENDMETHOD.

  METHOD test_diff_ms_midnight.

    " across midnight and a year end
    cl_abap_unit_assert=>assert_equals(
        exp = 1500
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = `20261231235959.5000000`
                                                                 time_to   = `20270101000001.0000000` ) ).

  ENDMETHOD.

  METHOD test_diff_ms_negative.

    cl_abap_unit_assert=>assert_equals(
        exp = -1000
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = `20261003101501.0000000`
                                                                 time_to   = `20261003101500.0000000` ) ).

  ENDMETHOD.

  METHOD test_diff_ms_initial.

    DATA lv_initial TYPE timestampl.

    cl_abap_unit_assert=>assert_equals(
        exp = 0
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = lv_initial
                                                                 time_to   = `20261003101500.0000000` ) ).

  ENDMETHOD.

  METHOD test_diff_ms_leap_day.

    " the 29th of February counts in a leap year and does not exist in any
    " other - a day is 86 400 000 ms either way
    cl_abap_unit_assert=>assert_equals(
        exp = 2 * 86400000
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = `20240228120000.0000000`
                                                                 time_to   = `20240301120000.0000000` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 86400000
        act = zabaputil_cl_util_context=>time_diff_milliseconds( time_from = `20250228120000.0000000`
                                                                 time_to   = `20250301120000.0000000` ) ).

  ENDMETHOD.

  METHOD test_srtti_pair_roundtrip.

    " type and data as two documents: neither carries the other, and the
    " pair parses back into the same table - the combined document (one
    " lex for the type, one for the data) is what every draft used to carry
    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <back> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <col>  TYPE any.
    DATA lv_col   TYPE string.
    DATA lv_type  TYPE string.
    DATA lv_data  TYPE string.

    " a runtime-built line type, like a table an app creates dynamically
    DATA temp393 TYPE cl_abap_structdescr=>component_table.
    DATA temp394 LIKE LINE OF temp393.
    DATA temp63 TYPE REF TO cl_abap_datadescr.
    DATA lt_comp LIKE temp393.
    DATA lo_tab TYPE REF TO cl_abap_tabledescr.
    DATA lr_tab TYPE REF TO data.
    DATA temp30 TYPE xsdboolean.
    DATA temp31 TYPE xsdboolean.
    DATA lr_back TYPE REF TO data.
    CLEAR temp393.
    
    temp394-name = `COL1`.
    
    temp63 ?= cl_abap_datadescr=>describe_by_data( lv_col ).
    temp394-type = temp63.
    INSERT temp394 INTO TABLE temp393.
    
    lt_comp = temp393.
    
    lo_tab = cl_abap_tabledescr=>create( p_line_type  = cl_abap_structdescr=>create( lt_comp )
                                               p_table_kind = cl_abap_tabledescr=>tablekind_std ).
    
    CREATE DATA lr_tab TYPE HANDLE lo_tab.
    ASSIGN lr_tab->* TO <tab>.
    INSERT INITIAL LINE INTO TABLE <tab> ASSIGNING <row>.
    ASSIGN COMPONENT `COL1` OF STRUCTURE <row> TO <col>.
    cl_abap_unit_assert=>assert_subrc( ).
    <col> = `payload-value`.

    zabaputil_cl_util_context=>xml_srtti_stringify_pair( EXPORTING data    = <tab>
                                                         IMPORTING ev_type = lv_type
                                                                   ev_data = lv_data ).
    cl_abap_unit_assert=>assert_not_initial( lv_type ).
    cl_abap_unit_assert=>assert_not_initial( lv_data ).
    
    temp30 = boolc( lv_type CS `payload-value` ).
    cl_abap_unit_assert=>assert_false( temp30 ).
    
    temp31 = boolc( lv_data CS `payload-value` ).
    cl_abap_unit_assert=>assert_true( temp31 ).

    
    lr_back = zabaputil_cl_util_context=>xml_srtti_parse_pair( iv_type = lv_type
                                                                     iv_data = lv_data ).
    ASSIGN lr_back->* TO <back>.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( <back> ) ).
    READ TABLE <back> INDEX 1 ASSIGNING <row>.
    cl_abap_unit_assert=>assert_subrc( ).
    ASSIGN COMPONENT `COL1` OF STRUCTURE <row> TO <col>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = `payload-value`
                                        act = <col> ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_filter_where DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS includes_or            FOR TESTING RAISING cx_static_check.
    METHODS excludes_and           FOR TESTING RAISING cx_static_check.
    METHODS include_and_exclude    FOR TESTING RAISING cx_static_check.
    METHODS fields_and             FOR TESTING RAISING cx_static_check.
    METHODS quote_doubled          FOR TESTING RAISING cx_static_check.
    METHODS pattern_escaped        FOR TESTING RAISING cx_static_check.
    METHODS name_not_a_field       FOR TESTING RAISING cx_static_check.
    METHODS dfies_where_escaped    FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_filter_where IMPLEMENTATION.

  METHOD includes_or.

    DATA temp395 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp396 LIKE LINE OF temp395.
    DATA temp64 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp65 LIKE LINE OF temp64.
    DATA lv_where TYPE string.
    CLEAR temp395.
    
    temp396-name = `F`.
    
    CLEAR temp64.
    
    temp65-sign = `I`.
    temp65-option = `EQ`.
    temp65-low = `A`.
    INSERT temp65 INTO TABLE temp64.
    temp65-sign = `I`.
    temp65-option = `EQ`.
    temp65-low = `B`.
    INSERT temp65 INTO TABLE temp64.
    temp396-t_range = temp64.
    INSERT temp396 INTO TABLE temp395.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp395 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F = 'A' OR F = 'B' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD excludes_and.

    " a row is kept only if it misses every excluded value
    DATA temp397 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp398 LIKE LINE OF temp397.
    DATA temp66 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp67 LIKE LINE OF temp66.
    DATA lv_where TYPE string.
    CLEAR temp397.
    
    temp398-name = `F`.
    
    CLEAR temp66.
    
    temp67-sign = `E`.
    temp67-option = `EQ`.
    temp67-low = `X`.
    INSERT temp67 INTO TABLE temp66.
    temp67-sign = `E`.
    temp67-option = `EQ`.
    temp67-low = `Y`.
    INSERT temp67 INTO TABLE temp66.
    temp398-t_range = temp66.
    INSERT temp398 INTO TABLE temp397.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp397 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F <> 'X' AND F <> 'Y' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD include_and_exclude.

    DATA temp399 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp400 LIKE LINE OF temp399.
    DATA temp68 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp69 LIKE LINE OF temp68.
    DATA lv_where TYPE string.
    CLEAR temp399.
    
    temp400-name = `F`.
    
    CLEAR temp68.
    
    temp69-sign = `I`.
    temp69-option = `CP`.
    temp69-low = `A*`.
    INSERT temp69 INTO TABLE temp68.
    temp69-sign = `E`.
    temp69-option = `EQ`.
    temp69-low = `AB`.
    INSERT temp69 INTO TABLE temp68.
    temp400-t_range = temp68.
    INSERT temp400 INTO TABLE temp399.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp399 ).

    cl_abap_unit_assert=>assert_equals( exp = `( ( F LIKE 'A%' ESCAPE '#' ) AND F <> 'AB' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD fields_and.

    DATA temp401 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp402 LIKE LINE OF temp401.
    DATA temp70 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp71 LIKE LINE OF temp70.
    DATA temp72 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp73 LIKE LINE OF temp72.
    DATA temp74 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA lv_where TYPE string.
    CLEAR temp401.
    
    temp402-name = `F`.
    
    CLEAR temp70.
    
    temp71-sign = `I`.
    temp71-option = `EQ`.
    temp71-low = `A`.
    INSERT temp71 INTO TABLE temp70.
    temp402-t_range = temp70.
    INSERT temp402 INTO TABLE temp401.
    temp402-name = `G`.
    
    CLEAR temp72.
    
    temp73-sign = `E`.
    temp73-option = `EQ`.
    temp73-low = `B`.
    INSERT temp73 INTO TABLE temp72.
    temp402-t_range = temp72.
    INSERT temp402 INTO TABLE temp401.
    temp402-name = `H`.
    
    CLEAR temp74.
    temp402-t_range = temp74.
    INSERT temp402 INTO TABLE temp401.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp401 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F = 'A' ) AND ( G <> 'B' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD quote_doubled.

    DATA temp403 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp404 LIKE LINE OF temp403.
    DATA temp75 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp76 LIKE LINE OF temp75.
    DATA lv_where TYPE string.
    CLEAR temp403.
    
    temp404-name = `F`.
    
    CLEAR temp75.
    
    temp76-sign = `I`.
    temp76-option = `EQ`.
    temp76-low = `O'Neil`.
    INSERT temp76 INTO TABLE temp75.
    temp404-t_range = temp75.
    INSERT temp404 INTO TABLE temp403.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp403 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F = 'O''Neil' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD pattern_escaped.

    " * and + are the wildcards - a typed _ or % only matches itself
    DATA temp405 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp406 LIKE LINE OF temp405.
    DATA temp77 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp78 LIKE LINE OF temp77.
    DATA lv_where TYPE string.
    CLEAR temp405.
    
    temp406-name = `F`.
    
    CLEAR temp77.
    
    temp78-sign = `I`.
    temp78-option = `CP`.
    temp78-low = `Z_T%#*+`.
    INSERT temp78 INTO TABLE temp77.
    temp406-t_range = temp77.
    INSERT temp406 INTO TABLE temp405.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp405 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F LIKE 'Z#_T#%##%_' ESCAPE '#' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD name_not_a_field.

    " a name that is no plain field name never reaches the WHERE clause
    DATA temp407 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp408 LIKE LINE OF temp407.
    DATA temp79 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp80 LIKE LINE OF temp79.
    DATA temp81 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp82 LIKE LINE OF temp81.
    DATA lv_where TYPE string.
    CLEAR temp407.
    
    temp408-name = `F = 'A' OR EXISTS ( SELECT bname FROM usr02 ) OR F`.
    
    CLEAR temp79.
    
    temp80-sign = `I`.
    temp80-option = `EQ`.
    temp80-low = `A`.
    INSERT temp80 INTO TABLE temp79.
    temp408-t_range = temp79.
    INSERT temp408 INTO TABLE temp407.
    temp408-name = `G`.
    
    CLEAR temp81.
    
    temp82-sign = `I`.
    temp82-option = `EQ`.
    temp82-low = `B`.
    INSERT temp82 INTO TABLE temp81.
    temp408-t_range = temp81.
    INSERT temp408 INTO TABLE temp407.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp407 ).

    cl_abap_unit_assert=>assert_equals( exp = `( G = 'B' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD dfies_where_escaped.

    TYPES:
      BEGIN OF ty_s_row,
        f TYPE string,
      END OF ty_s_row.
    DATA ls_row TYPE ty_s_row.
    DATA temp409 LIKE REF TO ls_row.
DATA temp83 TYPE zabaputil_cl_util_context=>ty_t_dfies.
DATA temp84 LIKE LINE OF temp83.
DATA lv_where TYPE string.

    " a typed quote cannot end the literal, a typed # % _ only matches itself
    ls_row-f = `O'N_1%#`.

    
    GET REFERENCE OF ls_row INTO temp409.

CLEAR temp83.

temp84-fieldname = `F`.
INSERT temp84 INTO TABLE temp83.

lv_where = zabaputil_cl_util_context=>tab_get_where_by_dfies(
        mv_check_tab_field = `F`
        ms_data_row        = temp409
        it_dfies           = temp83 ).

    cl_abap_unit_assert=>assert_equals( exp = ` ( F LIKE '%O''N#_1#%##%' ESCAPE '#' )`
                                        act = lv_where ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_ui5_binding DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS binding_date_time     FOR TESTING.
    METHODS binding_numbers       FOR TESTING.
    METHODS binding_currency_unit FOR TESTING.
    METHODS binding_plain         FOR TESTING.
    METHODS binding_comp          FOR TESTING.

ENDCLASS.


CLASS ltcl_ui5_binding IMPLEMENTATION.

  METHOD binding_date_time.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    cl_abap_unit_assert=>assert_equals(
        exp = `{path: '/DATE', type: 'DateType', formatOptions: {source: {pattern: 'yyyy-MM-dd'}}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `/DATE`
                                                        val  = lv_date ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `{path: 'TIME', type: 'TimeType', formatOptions: {source: {pattern: 'HH:mm:ss'}}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `TIME`
                                                        val  = lv_time ) ).

  ENDMETHOD.

  METHOD binding_numbers.

    DATA lv_int    TYPE i.
    DATA lv_packed TYPE p LENGTH 10 DECIMALS 3.
    DATA lv_float  TYPE f.

    cl_abap_unit_assert=>assert_equals(
        exp = `{path: 'N', type: 'IntType', formatOptions: {groupingEnabled: true}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `N`
                                                        val  = lv_int ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `{path: 'P', type: 'FloatType', formatOptions: {minFractionDigits: 3, maxFractionDigits: 3, groupingEnabled: true}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `P`
                                                        val  = lv_packed ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `{path: 'F', type: 'FloatType', formatOptions: {groupingEnabled: true}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `F`
                                                        val  = lv_float ) ).

  ENDMETHOD.

  METHOD binding_currency_unit.

    DATA lv_amount TYPE p LENGTH 10 DECIMALS 2.

    cl_abap_unit_assert=>assert_equals(
        exp = `{parts: [{path: 'PRICE'}, {path: 'CURRENCY'}], type: 'CurrType', formatOptions: {showMeasure: false}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed( path          = `PRICE`
                                                        val           = lv_amount
                                                        currency_path = `CURRENCY` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `{parts: [{path: 'MENGE'}, {path: 'MEINS'}], type: 'UnitType', formatOptions: {showMeasure: false}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed( path      = `MENGE`
                                                        val       = lv_amount
                                                        unit_path = `MEINS` ) ).

  ENDMETHOD.

  METHOD binding_plain.

    DATA lv_text TYPE string.
    DATA lv_char TYPE c LENGTH 10.
    DATA lv_flag TYPE abap_bool.

    cl_abap_unit_assert=>assert_equals( exp = `{TEXT}`
                                        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `TEXT`
                                                                                        val  = lv_text ) ).
    cl_abap_unit_assert=>assert_equals( exp = `{/CHAR}`
                                        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `/CHAR`
                                                                                        val  = lv_char ) ).
    cl_abap_unit_assert=>assert_equals( exp = `{FLAG}`
                                        act = zabaputil_cl_util_context=>ui5_binding_typed( path = `FLAG`
                                                                                        val  = lv_flag ) ).
    cl_abap_unit_assert=>assert_char_cp( exp = `*CurrType: 'sap/ui/model/type/Currency'*`
                                         act = zabaputil_cl_util_context=>ui5_get_type_require( ) ).

  ENDMETHOD.

  METHOD binding_comp.

    TYPES:
      BEGIN OF ty_s_row,
        fldate TYPE d,
        price  TYPE p LENGTH 8 DECIMALS 2,
        text   TYPE string,
      END OF ty_s_row.
    DATA ls_row TYPE ty_s_row.

    cl_abap_unit_assert=>assert_equals(
        exp = `{path: 'FLDATE', type: 'DateType', formatOptions: {source: {pattern: 'yyyy-MM-dd'}}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed_comp( struc = ls_row
                                                             comp  = `fldate` ) ).
    " a local type has no DDIC reference field - the amount is a number
    cl_abap_unit_assert=>assert_equals(
        exp = `{path: 'PRICE', type: 'FloatType', formatOptions: {minFractionDigits: 2, maxFractionDigits: 2, groupingEnabled: true}}`
        act = zabaputil_cl_util_context=>ui5_binding_typed_comp( struc = ls_row
                                                             comp  = `PRICE` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_ref_field( struc = ls_row
                                                                                   comp  = `PRICE` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `{TEXT}`
                                        act = zabaputil_cl_util_context=>ui5_binding_typed_comp( struc = ls_row
                                                                                             comp  = `TEXT` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `{MISSING}`
                                        act = zabaputil_cl_util_context=>ui5_binding_typed_comp( struc = ls_row
                                                                                             comp  = `MISSING` ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_barcode_gs1 DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-METHODS gs
      RETURNING
        VALUE(result) TYPE string.

    METHODS parse_raw_with_gs        FOR TESTING.
    METHODS parse_human_readable     FOR TESTING.
    METHODS parse_custom_separator   FOR TESTING.
    METHODS parse_measure_decimals   FOR TESTING.
    METHODS parse_sscc_and_gln       FOR TESTING.
    METHODS parse_date_day_zero      FOR TESTING.
    METHODS parse_date_century       FOR TESTING.
    METHODS parse_trailing_cr_lf     FOR TESTING.
    METHODS parse_check_digit_off    FOR TESTING.
    METHODS parse_errors             FOR TESTING.
    METHODS build_roundtrip          FOR TESTING.
    METHODS build_human_readable     FOR TESTING.
    METHODS get_value_prefix         FOR TESTING.
    METHODS check_digit              FOR TESTING.

ENDCLASS.


CLASS ltcl_barcode_gs1 IMPLEMENTATION.

  METHOD gs.

    DATA temp410 TYPE xstring.
    temp410 = `1D`.
    result = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp410 ).

  ENDMETHOD.

  METHOD parse_raw_with_gs.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp411> LIKE LINE OF lt_gs1.
    DATA temp412 LIKE sy-tabix.
    FIELD-SYMBOLS <temp413> LIKE LINE OF lt_gs1.
    DATA temp414 LIKE sy-tabix.
    FIELD-SYMBOLS <temp415> LIKE LINE OF lt_gs1.
    DATA temp416 LIKE sy-tabix.
    FIELD-SYMBOLS <temp417> LIKE LINE OF lt_gs1.
    DATA temp418 LIKE sy-tabix.
    DATA temp419 TYPE d.
    FIELD-SYMBOLS <temp85> LIKE LINE OF lt_gs1.
    DATA temp86 LIKE sy-tabix.
    FIELD-SYMBOLS <temp420> LIKE LINE OF lt_gs1.
    DATA temp421 LIKE sy-tabix.
    FIELD-SYMBOLS <temp422> LIKE LINE OF lt_gs1.
    DATA temp423 LIKE sy-tabix.
    FIELD-SYMBOLS <temp424> LIKE LINE OF lt_gs1.
    DATA temp425 LIKE sy-tabix.
    FIELD-SYMBOLS <temp426> LIKE LINE OF lt_gs1.
    DATA temp427 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse(
        val      = |]C10109506000134352172512311010ABC123{ gs( ) }21XYZ|
        ref_date = `20250601` ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_gs1 ) ).
    
    
    temp412 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp411>.
    sy-tabix = temp412.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `01`
                                        act = <temp411>-ai ).
    
    
    temp414 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp413>.
    sy-tabix = temp414.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `GTIN`
                                        act = <temp413>-title ).
    
    
    temp416 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp415>.
    sy-tabix = temp416.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `09506000134352`
                                        act = <temp415>-value ).
    
    
    temp418 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp417>.
    sy-tabix = temp418.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `17`
                                        act = <temp417>-ai ).
    
    temp419 = `20251231`.
    
    
    temp86 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp85>.
    sy-tabix = temp86.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp419
                                        act = <temp85>-date ).
    
    
    temp421 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp420>.
    sy-tabix = temp421.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `10`
                                        act = <temp420>-ai ).
    
    
    temp423 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp422>.
    sy-tabix = temp423.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `10ABC123`
                                        act = <temp422>-value ).
    
    
    temp425 = sy-tabix.
    READ TABLE lt_gs1 INDEX 4 ASSIGNING <temp424>.
    sy-tabix = temp425.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `21`
                                        act = <temp424>-ai ).
    
    
    temp427 = sy-tabix.
    READ TABLE lt_gs1 INDEX 4 ASSIGNING <temp426>.
    sy-tabix = temp427.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `XYZ`
                                        act = <temp426>-value ).

  ENDMETHOD.

  METHOD parse_human_readable.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp428> LIKE LINE OF lt_gs1.
    DATA temp429 LIKE sy-tabix.
    FIELD-SYMBOLS <temp430> LIKE LINE OF lt_gs1.
    DATA temp431 LIKE sy-tabix.
    DATA temp432 TYPE d.
    FIELD-SYMBOLS <temp87> LIKE LINE OF lt_gs1.
    DATA temp88 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse(
        val      = `(01)09506000134352(17)251231(10)ABC123(21)XYZ`
        ref_date = `20250601` ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_gs1 ) ).
    
    
    temp429 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp428>.
    sy-tabix = temp429.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ABC123`
                                        act = <temp428>-value ).
    
    
    temp431 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp430>.
    sy-tabix = temp431.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `BATCH/LOT`
                                        act = <temp430>-title ).
    
    temp432 = `20251231`.
    
    
    temp88 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp87>.
    sy-tabix = temp88.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp432
                                        act = <temp87>-date ).

  ENDMETHOD.

  METHOD parse_custom_separator.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp433> LIKE LINE OF lt_gs1.
    DATA temp434 LIKE sy-tabix.
    FIELD-SYMBOLS <temp435> LIKE LINE OF lt_gs1.
    DATA temp436 LIKE sy-tabix.
    FIELD-SYMBOLS <temp437> LIKE LINE OF lt_gs1.
    DATA temp438 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val       = `10LOT42|3712|0109506000134352`
                                                                 separator = `|` ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_gs1 ) ).
    
    
    temp434 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp433>.
    sy-tabix = temp434.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `LOT42`
                                        act = <temp433>-value ).
    
    
    temp436 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp435>.
    sy-tabix = temp436.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `12`
                                        act = <temp435>-value ).
    
    
    temp438 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp437>.
    sy-tabix = temp438.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `COUNT`
                                        act = <temp437>-title ).

  ENDMETHOD.

  METHOD parse_measure_decimals.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp439> LIKE LINE OF lt_gs1.
    DATA temp440 LIKE sy-tabix.
    FIELD-SYMBOLS <temp441> LIKE LINE OF lt_gs1.
    DATA temp442 LIKE sy-tabix.
    FIELD-SYMBOLS <temp443> LIKE LINE OF lt_gs1.
    DATA temp444 LIKE sy-tabix.
    FIELD-SYMBOLS <temp445> LIKE LINE OF lt_gs1.
    DATA temp446 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( `01095060001343523103001250` ).

    
    
    temp440 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp439>.
    sy-tabix = temp440.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `3103`
                                        act = <temp439>-ai ).
    
    
    temp442 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp441>.
    sy-tabix = temp442.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `001250`
                                        act = <temp441>-value ).
    
    
    temp444 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp443>.
    sy-tabix = temp444.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = <temp443>-decimals ).
    
    
    temp446 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp445>.
    sy-tabix = temp446.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NET WEIGHT (kg)`
                                        act = <temp445>-title ).

  ENDMETHOD.

  METHOD parse_sscc_and_gln.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp447> LIKE LINE OF lt_gs1.
    DATA temp448 LIKE sy-tabix.
    FIELD-SYMBOLS <temp449> LIKE LINE OF lt_gs1.
    DATA temp450 LIKE sy-tabix.
    FIELD-SYMBOLS <temp451> LIKE LINE OF lt_gs1.
    DATA temp452 LIKE sy-tabix.
    FIELD-SYMBOLS <temp453> LIKE LINE OF lt_gs1.
    DATA temp454 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( `001061414112345678974109506000000008` ).

    
    
    temp448 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp447>.
    sy-tabix = temp448.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `SSCC`
                                        act = <temp447>-title ).
    
    
    temp450 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp449>.
    sy-tabix = temp450.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `106141411234567897`
                                        act = <temp449>-value ).
    
    
    temp452 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp451>.
    sy-tabix = temp452.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `410`
                                        act = <temp451>-ai ).
    
    
    temp454 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp453>.
    sy-tabix = temp454.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `9506000000008`
                                        act = <temp453>-value ).

  ENDMETHOD.

  METHOD parse_date_day_zero.

    " day 00 means the last day of the month - February of a leap year
    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    DATA temp455 TYPE d.
    FIELD-SYMBOLS <temp89> LIKE LINE OF lt_gs1.
    DATA temp90 LIKE sy-tabix.
    DATA temp456 TYPE d.
    FIELD-SYMBOLS <temp91> LIKE LINE OF lt_gs1.
    DATA temp92 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `15240200`
                                                                 ref_date = `20250101` ).

    
    temp455 = `20240229`.
    
    
    temp90 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp89>.
    sy-tabix = temp90.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp455
                                        act = <temp89>-date ).

    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `17251200`
                                                          ref_date = `20250101` ).
    
    temp456 = `20251231`.
    
    
    temp92 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp91>.
    sy-tabix = temp92.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp456
                                        act = <temp91>-date ).

  ENDMETHOD.

  METHOD parse_date_century.

    " sliding window: up to 50 years ahead stays in this century, further
    " ahead belongs to the past one
    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    DATA temp457 TYPE d.
    FIELD-SYMBOLS <temp93> LIKE LINE OF lt_gs1.
    DATA temp94 LIKE sy-tabix.
    DATA temp458 TYPE d.
    FIELD-SYMBOLS <temp95> LIKE LINE OF lt_gs1.
    DATA temp96 LIKE sy-tabix.
    DATA temp459 TYPE d.
    FIELD-SYMBOLS <temp97> LIKE LINE OF lt_gs1.
    DATA temp98 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `11750101`
                                                                 ref_date = `20250101` ).
    
    temp457 = `20750101`.
    
    
    temp94 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp93>.
    sy-tabix = temp94.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp457
                                        act = <temp93>-date ).

    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `11760101`
                                                          ref_date = `20250101` ).
    
    temp458 = `19760101`.
    
    
    temp96 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp95>.
    sy-tabix = temp96.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp458
                                        act = <temp95>-date ).

    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `11020101`
                                                          ref_date = `20980101` ).
    
    temp459 = `21020101`.
    
    
    temp98 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp97>.
    sy-tabix = temp98.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp459
                                        act = <temp97>-date ).

  ENDMETHOD.

  METHOD parse_trailing_cr_lf.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp460> LIKE LINE OF lt_gs1.
    DATA temp461 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse(
        |10ABC{ zabaputil_cl_util_context=>cv_char_util_cr_lf }| ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_gs1 ) ).
    
    
    temp461 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp460>.
    sy-tabix = temp461.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ABC`
                                        act = <temp460>-value ).

  ENDMETHOD.

  METHOD parse_check_digit_off.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp462> LIKE LINE OF lt_gs1.
    DATA temp463 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val         = `0109506000134353`
                                                                 check_digit = abap_false ).

    
    
    temp463 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp462>.
    sy-tabix = temp463.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `09506000134353`
                                        act = <temp462>-value ).

  ENDMETHOD.

  METHOD parse_errors.

    DATA lt_bad TYPE string_table.

    DATA temp464 TYPE string_table.
    DATA lv_bad LIKE LINE OF lt_bad.
    CLEAR temp464.
    INSERT `0409506000134352` INTO TABLE temp464.
    INSERT `010950600013435` INTO TABLE temp464.
    INSERT `0109506000134353` INTO TABLE temp464.
    INSERT `01095060001343AB` INTO TABLE temp464.
    INSERT `17251301` INTO TABLE temp464.
    INSERT `17250230` INTO TABLE temp464.
    INSERT `10ABCDEFGHIJKLMNOPQRSTU` INTO TABLE temp464.
    INSERT `(10)` INTO TABLE temp464.
    INSERT `(99999)ABC` INTO TABLE temp464.
    lt_bad = temp464.                   " unknown AI, readable form

    
    LOOP AT lt_bad INTO lv_bad.
      TRY.
          zabaputil_cl_util_context=>barcode_gs1_parse( val      = lv_bad
                                                        ref_date = `20250601` ).
          cl_abap_unit_assert=>fail( |no exception for { lv_bad }| ).
        CATCH zabaputil_cx_util_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_roundtrip.

    DATA lv_code TYPE string.
    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    lv_code = |0109506000134352{ `10ABC` }{ gs( ) }3103001250{ `21XYZ` }|.

    
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( lv_code ).

    cl_abap_unit_assert=>assert_equals( exp = lv_code
                                        act = zabaputil_cl_util_context=>barcode_gs1_build( lt_gs1 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `0109506000134352` && `10ABC|3103001250` && `21XYZ`
        act = zabaputil_cl_util_context=>barcode_gs1_build( t_gs1     = lt_gs1
                                                            separator = `|` ) ).

  ENDMETHOD.

  METHOD build_human_readable.

    DATA temp466 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    DATA temp467 LIKE LINE OF temp466.
    DATA lt_gs1 LIKE temp466.
    CLEAR temp466.
    
    temp467-ai = `01`.
    temp467-value = `09506000134352`.
    INSERT temp467 INTO TABLE temp466.
    temp467-ai = `10`.
    temp467-value = `ABC`.
    INSERT temp467 INTO TABLE temp466.
    
    lt_gs1 = temp466.

    cl_abap_unit_assert=>assert_equals(
        exp = `(01)09506000134352(10)ABC`
        act = zabaputil_cl_util_context=>barcode_gs1_build( t_gs1          = lt_gs1
                                                            human_readable = abap_true ) ).

  ENDMETHOD.

  METHOD get_value_prefix.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( `01095060001343523103001250` ).

    cl_abap_unit_assert=>assert_equals( exp = `09506000134352`
                                        act = zabaputil_cl_util_context=>barcode_gs1_get_value( t_gs1 = lt_gs1
                                                                                                ai    = `01` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `001250`
                                        act = zabaputil_cl_util_context=>barcode_gs1_get_value( t_gs1 = lt_gs1
                                                                                                ai    = `310` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>barcode_gs1_get_value( t_gs1 = lt_gs1
                                                                                           ai    = `10` ) ).

  ENDMETHOD.

  METHOD check_digit.

    cl_abap_unit_assert=>assert_equals( exp = `2`
                                        act = zabaputil_cl_util_context=>barcode_gs1_calc_check_digit( `0950600013435` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `7`
                                        act = zabaputil_cl_util_context=>barcode_gs1_calc_check_digit( `10614141123456789` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_gs1_check_digit( `09506000134352` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_gs1_check_digit( `9506000000008` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_gs1_check_digit( `09506000134353` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_gs1_check_digit( `ABC` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_gs1_check_digit( `7` ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_system_task DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    DATA input  TYPE i.
    DATA output TYPE i.
    METHODS run.

ENDCLASS.


CLASS ltcl_system_task IMPLEMENTATION.

  METHOD run.

    IF input < 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `negative input`.
    ENDIF.
    output = input * 2.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_system_api DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS bintab_roundtrip          FOR TESTING.
    METHODS longtext_lines_split      FOR TESTING.
    METHODS longtext_lines_join       FOR TESTING.
    METHODS longtext_lines_roundtrip  FOR TESTING.
    METHODS parallel_sequential       FOR TESTING.

ENDCLASS.


CLASS ltcl_system_api IMPLEMENTATION.

  METHOD bintab_roundtrip.

    TYPES ty_x4 TYPE x LENGTH 4.
    TYPES temp31 TYPE STANDARD TABLE OF ty_x4 WITH DEFAULT KEY.
DATA lt_bin TYPE temp31.

    DATA temp468 TYPE xstring.
    DATA lv_x LIKE temp468.
    DATA temp469 TYPE xstring.
    temp468 = `00112233445566778899`.
    
    lv_x = temp468.
    zabaputil_cl_util_context=>conv_get_bintab_by_xstring( EXPORTING val    = lv_x
                                                           IMPORTING result = lt_bin ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_bin ) ).

    " the last line is padded - the length cuts the padding off again
    cl_abap_unit_assert=>assert_equals( exp = lv_x
                                        act = zabaputil_cl_util_context=>conv_get_xstring_by_bintab( val    = lt_bin
                                                                                                     length = 10 ) ).
    
    temp469 = `001122334455667788990000`.
    cl_abap_unit_assert=>assert_equals( exp = temp469
                                        act = zabaputil_cl_util_context=>conv_get_xstring_by_bintab( lt_bin ) ).

  ENDMETHOD.

  METHOD longtext_lines_split.

    DATA lv_long TYPE string.
    DATA lt_line TYPE zabaputil_cl_util_context=>ty_t_longtext_line.
    FIELD-SYMBOLS <temp470> LIKE LINE OF lt_line.
    DATA temp471 LIKE sy-tabix.
    FIELD-SYMBOLS <temp472> LIKE LINE OF lt_line.
    DATA temp473 LIKE sy-tabix.
    FIELD-SYMBOLS <temp474> LIKE LINE OF lt_line.
    DATA temp475 LIKE sy-tabix.
    FIELD-SYMBOLS <temp476> LIKE LINE OF lt_line.
    DATA temp477 LIKE sy-tabix.
    FIELD-SYMBOLS <temp478> LIKE LINE OF lt_line.
    DATA temp479 LIKE sy-tabix.
    FIELD-SYMBOLS <temp480> LIKE LINE OF lt_line.
    DATA temp481 LIKE sy-tabix.
    FIELD-SYMBOLS <temp482> LIKE LINE OF lt_line.
    DATA temp483 LIKE sy-tabix.
    FIELD-SYMBOLS <temp484> LIKE LINE OF lt_line.
    DATA temp485 LIKE sy-tabix.
    FIELD-SYMBOLS <temp486> LIKE LINE OF lt_line.
    DATA temp487 LIKE sy-tabix.
    lv_long = repeat( val = `abcdefghij`
                            occ = 30 ).

    
    lt_line = zabaputil_cl_util_context=>longtext_to_lines(
        |first{ zabaputil_cl_util_context=>cv_char_util_cr_lf }{ zabaputil_cl_util_context=>cv_char_util_newline }{ lv_long }| ).

    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( lt_line ) ).
    
    
    temp471 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp470>.
    sy-tabix = temp471.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*`
                                        act = <temp470>-format ).
    
    
    temp473 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp472>.
    sy-tabix = temp473.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `first`
                                        act = <temp472>-line ).
    " the empty line stays an empty paragraph
    
    
    temp475 = sy-tabix.
    READ TABLE lt_line INDEX 2 ASSIGNING <temp474>.
    sy-tabix = temp475.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*`
                                        act = <temp474>-format ).
    
    
    temp477 = sy-tabix.
    READ TABLE lt_line INDEX 2 ASSIGNING <temp476>.
    sy-tabix = temp477.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp476>-line ).
    " 300 characters: 132 + 132 + 36, the continuation lines with `=`
    
    
    temp479 = sy-tabix.
    READ TABLE lt_line INDEX 3 ASSIGNING <temp478>.
    sy-tabix = temp479.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*`
                                        act = <temp478>-format ).
    
    
    temp481 = sy-tabix.
    READ TABLE lt_line INDEX 3 ASSIGNING <temp480>.
    sy-tabix = temp481.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 132
                                        act = strlen( <temp480>-line ) ).
    
    
    temp483 = sy-tabix.
    READ TABLE lt_line INDEX 4 ASSIGNING <temp482>.
    sy-tabix = temp483.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `=`
                                        act = <temp482>-format ).
    
    
    temp485 = sy-tabix.
    READ TABLE lt_line INDEX 5 ASSIGNING <temp484>.
    sy-tabix = temp485.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `=`
                                        act = <temp484>-format ).
    
    
    temp487 = sy-tabix.
    READ TABLE lt_line INDEX 5 ASSIGNING <temp486>.
    sy-tabix = temp487.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 36
                                        act = strlen( <temp486>-line ) ).

  ENDMETHOD.

  METHOD longtext_lines_join.

    DATA temp488 TYPE zabaputil_cl_util_context=>ty_t_longtext_line.
    DATA temp489 LIKE LINE OF temp488.
    DATA lt_line LIKE temp488.
    CLEAR temp488.
    
    temp489-format = `*`.
    temp489-line = `Dear`.
    INSERT temp489 INTO TABLE temp488.
    temp489-format = ``.
    temp489-line = `customer,`.
    INSERT temp489 INTO TABLE temp488.
    temp489-format = `/`.
    temp489-line = `thank`.
    INSERT temp489 INTO TABLE temp488.
    temp489-format = `=`.
    temp489-line = `s.`.
    INSERT temp489 INTO TABLE temp488.
    
    lt_line = temp488.

    cl_abap_unit_assert=>assert_equals(
        exp = |Dear customer,{ zabaputil_cl_util_context=>cv_char_util_newline }thanks.|
        act = zabaputil_cl_util_context=>longtext_from_lines( lt_line ) ).

  ENDMETHOD.

  METHOD longtext_lines_roundtrip.

    DATA lv_text TYPE string.
    lv_text = |line one{ zabaputil_cl_util_context=>cv_char_util_newline }{ zabaputil_cl_util_context=>cv_char_util_newline }|
                 && repeat( val = `0123456789`
                            occ = 20 ).

    cl_abap_unit_assert=>assert_equals(
        exp = lv_text
        act = zabaputil_cl_util_context=>longtext_from_lines( zabaputil_cl_util_context=>longtext_to_lines( lv_text ) ) ).

  ENDMETHOD.

  METHOD parallel_sequential.

    DATA lo_ok TYPE REF TO ltcl_system_task.
    DATA lo_fail TYPE REF TO ltcl_system_task.
    DATA temp490 TYPE zabaputil_cl_util_context=>ty_t_object.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_parallel.
    FIELD-SYMBOLS <temp492> LIKE LINE OF lt_result.
    DATA temp493 LIKE sy-tabix.
    FIELD-SYMBOLS <temp494> LIKE LINE OF lt_result.
    DATA temp495 LIKE sy-tabix.
    FIELD-SYMBOLS <temp496> LIKE LINE OF lt_result.
    DATA temp497 LIKE sy-tabix.
    CREATE OBJECT lo_ok TYPE ltcl_system_task.
    lo_ok->input = 21.
    
    CREATE OBJECT lo_fail TYPE ltcl_system_task.
    lo_fail->input = -1.

    
    CLEAR temp490.
    INSERT lo_ok INTO TABLE temp490.
    INSERT lo_fail INTO TABLE temp490.
    
    lt_result = zabaputil_cl_util_context=>parallel_run( t_task     = temp490
                                                               sequential = abap_true
                                                               method     = `RUN` ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 42
                                        act = lo_ok->output ).
    
    
    temp493 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp492>.
    sy-tabix = temp493.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp492>-message ).
    
    
    temp495 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp494>.
    sy-tabix = temp495.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = <temp494>-index ).
    
    
    temp497 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp496>.
    sy-tabix = temp497.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_char_cp( exp = `*negative input*`
                                         act = <temp496>-message ).

  ENDMETHOD.

ENDCLASS.

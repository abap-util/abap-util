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
    METHODS token_empty_value              FOR TESTING.

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
    " the pattern keeps its asterisks - they make it a pattern for IN and LIKE
    cl_abap_unit_assert=>assert_equals( exp = `*test*` act = ls_range-low ).
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

  METHOD token_empty_value.
    " the empty-value token of filter_get_token_range_mapping - it was read
    " as LT `leer>`
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `<leer>` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = ls_range-option ).
    cl_abap_unit_assert=>assert_initial( ls_range-low ).

    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `!(<leer>)` ).
    cl_abap_unit_assert=>assert_equals( exp = `E` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = ls_range-option ).
    cl_abap_unit_assert=>assert_initial( ls_range-low ).
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
    METHODS sort_by_stable                 FOR TESTING.

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

    METHODS csv_deep_column                FOR TESTING.
    METHODS csv_include_columns            FOR TESTING.

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

  METHOD sort_by_stable.
    " rows with the same key keep their order, in both directions
    TYPES: BEGIN OF ty, name TYPE string, value TYPE i, END OF ty.
    TYPES temp8 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp8.
    DATA temp168 LIKE lt_tab.
    DATA temp169 LIKE LINE OF temp168.
    DATA temp35 TYPE string_table.
    DATA temp172 LIKE LINE OF lt_tab.
    DATA temp37 TYPE string_table.
    DATA r LIKE LINE OF lt_tab.
    CLEAR temp168.
    
    temp169-name = `b1`.
    temp169-value = 2.
    INSERT temp169 INTO TABLE temp168.
    temp169-name = `a1`.
    temp169-value = 1.
    INSERT temp169 INTO TABLE temp168.
    temp169-name = `b2`.
    temp169-value = 2.
    INSERT temp169 INTO TABLE temp168.
    temp169-name = `a2`.
    temp169-value = 1.
    INSERT temp169 INTO TABLE temp168.
    temp169-name = `b3`.
    temp169-value = 2.
    INSERT temp169 INTO TABLE temp168.
    lt_tab = temp168.

    zabaputil_cl_util_context=>itab_sort_by( EXPORTING fieldname = `VALUE` CHANGING tab = lt_tab ).
    
    CLEAR temp35.
    
    LOOP AT lt_tab INTO temp172.
      INSERT temp172-name INTO TABLE temp35.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( exp = `a1 a2 b1 b2 b3`
                                        act = concat_lines_of( table = temp35
                                                               sep   = ` ` ) ).

    zabaputil_cl_util_context=>itab_sort_by( EXPORTING fieldname  = `VALUE`
                                                       descending = abap_true
                                             CHANGING  tab        = lt_tab ).
    
    CLEAR temp37.
    
    LOOP AT lt_tab INTO r.
      INSERT r-name INTO TABLE temp37.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( exp = `b1 b2 b3 a1 a2`
                                        act = concat_lines_of( table = temp37
                                                               sep   = ` ` ) ).
  ENDMETHOD.

  METHOD sort_by_descending.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE i, END OF ty.
    TYPES temp9 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp9.
    DATA temp39 LIKE lt_tab.
    DATA temp40 LIKE LINE OF temp39.
    FIELD-SYMBOLS <temp41> LIKE LINE OF lt_tab.
    DATA temp42 LIKE sy-tabix.
    FIELD-SYMBOLS <temp43> LIKE LINE OF lt_tab.
    DATA temp44 LIKE sy-tabix.
    FIELD-SYMBOLS <temp45> LIKE LINE OF lt_tab.
    DATA temp46 LIKE sy-tabix.
    CLEAR temp39.
    
    temp40-name = `A`.
    temp40-value = 1.
    INSERT temp40 INTO TABLE temp39.
    temp40-name = `C`.
    temp40-value = 3.
    INSERT temp40 INTO TABLE temp39.
    temp40-name = `B`.
    temp40-value = 2.
    INSERT temp40 INTO TABLE temp39.
    lt_tab = temp39.
    zabaputil_cl_util_context=>itab_sort_by( EXPORTING fieldname = `VALUE` descending = abap_true CHANGING tab = lt_tab ).
    
    
    temp42 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp41>.
    sy-tabix = temp42.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = <temp41>-value ).
    
    
    temp44 = sy-tabix.
    READ TABLE lt_tab INDEX 2 ASSIGNING <temp43>.
    sy-tabix = temp44.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = <temp43>-value ).
    
    
    temp46 = sy-tabix.
    READ TABLE lt_tab INDEX 3 ASSIGNING <temp45>.
    sy-tabix = temp46.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = <temp45>-value ).
  ENDMETHOD.

  METHOD slice_basic.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp10 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp10.
    DATA temp47 LIKE lt_tab.
    DATA temp48 LIKE LINE OF temp47.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp47.
    
    temp48-id = 1.
    INSERT temp48 INTO TABLE temp47.
    temp48-id = 2.
    INSERT temp48 INTO TABLE temp47.
    temp48-id = 3.
    INSERT temp48 INTO TABLE temp47.
    temp48-id = 4.
    INSERT temp48 INTO TABLE temp47.
    temp48-id = 5.
    INSERT temp48 INTO TABLE temp47.
    lt_tab = temp47.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 2 to = 4 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD slice_from_only.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp11 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp11.
    DATA temp49 LIKE lt_tab.
    DATA temp50 LIKE LINE OF temp49.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp49.
    
    temp50-id = 1.
    INSERT temp50 INTO TABLE temp49.
    temp50-id = 2.
    INSERT temp50 INTO TABLE temp49.
    temp50-id = 3.
    INSERT temp50 INTO TABLE temp49.
    lt_tab = temp49.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 2 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD slice_to_exceeds.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp12 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp12.
    DATA temp51 LIKE lt_tab.
    DATA temp52 LIKE LINE OF temp51.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp51.
    
    temp52-id = 1.
    INSERT temp52 INTO TABLE temp51.
    temp52-id = 2.
    INSERT temp52 INTO TABLE temp51.
    lt_tab = temp51.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 1 to = 99 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD paginate_page_1.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp13 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp13.
    DATA temp53 LIKE lt_tab.
    DATA temp54 LIKE LINE OF temp53.
    DATA lr_result TYPE REF TO data.
    DATA lv_total_count TYPE i.
    DATA lv_total_pages TYPE i.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp53.
    
    temp54-id = 1.
    INSERT temp54 INTO TABLE temp53.
    temp54-id = 2.
    INSERT temp54 INTO TABLE temp53.
    temp54-id = 3.
    INSERT temp54 INTO TABLE temp53.
    temp54-id = 4.
    INSERT temp54 INTO TABLE temp53.
    temp54-id = 5.
    INSERT temp54 INTO TABLE temp53.
    lt_tab = temp53.
    
    
    
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab = lt_tab page = 1 page_size = 2
                                   IMPORTING result = lr_result total_count = lv_total_count total_pages = lv_total_pages ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = lv_total_count ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lv_total_pages ).
  ENDMETHOD.

  METHOD paginate_page_2.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp14 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp14.
    DATA temp55 LIKE lt_tab.
    DATA temp56 LIKE LINE OF temp55.
    DATA lr_result TYPE REF TO data.
    DATA lv_total_count TYPE i.
    DATA lv_total_pages TYPE i.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp55.
    
    temp56-id = 1.
    INSERT temp56 INTO TABLE temp55.
    temp56-id = 2.
    INSERT temp56 INTO TABLE temp55.
    temp56-id = 3.
    INSERT temp56 INTO TABLE temp55.
    temp56-id = 4.
    INSERT temp56 INTO TABLE temp55.
    temp56-id = 5.
    INSERT temp56 INTO TABLE temp55.
    lt_tab = temp55.
    
    
    
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab = lt_tab page = 2 page_size = 2
                                   IMPORTING result = lr_result total_count = lv_total_count total_pages = lv_total_pages ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD paginate_total_pages.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp15 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp15.
    DATA temp57 LIKE lt_tab.
    DATA temp58 LIKE LINE OF temp57.
    DATA lr_result TYPE REF TO data.
    DATA lv_total_count TYPE i.
    DATA lv_total_pages TYPE i.
    CLEAR temp57.
    
    temp58-id = 1.
    INSERT temp58 INTO TABLE temp57.
    temp58-id = 2.
    INSERT temp58 INTO TABLE temp57.
    temp58-id = 3.
    INSERT temp58 INTO TABLE temp57.
    lt_tab = temp57.
    
    
    
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab = lt_tab page = 1 page_size = 2
                                   IMPORTING result = lr_result total_count = lv_total_count total_pages = lv_total_pages ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lv_total_pages ).
  ENDMETHOD.

  METHOD count_by_basic.
    TYPES: BEGIN OF ty, category TYPE string, name TYPE string, END OF ty.
    TYPES temp16 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp16.
    DATA temp59 LIKE lt_tab.
    DATA temp60 LIKE LINE OF temp59.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp61> LIKE LINE OF lt_result.
    DATA temp62 LIKE sy-tabix.
    FIELD-SYMBOLS <temp63> LIKE LINE OF lt_result.
    DATA temp64 LIKE sy-tabix.
    CLEAR temp59.
    
    temp60-category = `A`.
    temp60-name = `1`.
    INSERT temp60 INTO TABLE temp59.
    temp60-category = `B`.
    temp60-name = `2`.
    INSERT temp60 INTO TABLE temp59.
    temp60-category = `A`.
    temp60-name = `3`.
    INSERT temp60 INTO TABLE temp59.
    temp60-category = `A`.
    temp60-name = `4`.
    INSERT temp60 INTO TABLE temp59.
    lt_tab = temp59.
    
    lt_result = zabaputil_cl_util_context=>itab_count_by( tab = lt_tab fieldname = `CATEGORY` ).
    
    
    temp62 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `A` ASSIGNING <temp61>.
    sy-tabix = temp62.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `3` act = <temp61>-v ).
    
    
    temp64 = sy-tabix.
    READ TABLE lt_result WITH KEY n = `B` ASSIGNING <temp63>.
    sy-tabix = temp64.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = <temp63>-v ).
  ENDMETHOD.

  METHOD filter_by_val_basic.
    TYPES: BEGIN OF ty, name TYPE string, city TYPE string, END OF ty.
    TYPES temp17 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp17.
    DATA temp65 LIKE lt_tab.
    DATA temp66 LIKE LINE OF temp65.
    CLEAR temp65.
    
    temp66-name = `Alice`.
    temp66-city = `Berlin`.
    INSERT temp66 INTO TABLE temp65.
    temp66-name = `Bob`.
    temp66-city = `Paris`.
    INSERT temp66 INTO TABLE temp65.
    temp66-name = `Charlie`.
    temp66-city = `Berlin`.
    INSERT temp66 INTO TABLE temp65.
    lt_tab = temp65.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `Berlin` CHANGING tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_tab ) ).
  ENDMETHOD.

  METHOD filter_by_val_case_ignore.
    TYPES: BEGIN OF ty, name TYPE string, END OF ty.
    TYPES temp18 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp18.
    DATA temp67 LIKE lt_tab.
    DATA temp68 LIKE LINE OF temp67.
    CLEAR temp67.
    
    temp68-name = `HELLO`.
    INSERT temp68 INTO TABLE temp67.
    temp68-name = `world`.
    INSERT temp68 INTO TABLE temp67.
    temp68-name = `Hello`.
    INSERT temp68 INTO TABLE temp67.
    lt_tab = temp67.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `hello` ignore_case = abap_true CHANGING tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_tab ) ).
  ENDMETHOD.

  METHOD filter_by_val_keeps_rows.
    " guards the DELETE ... INDEX in itab_filter_by_val: with a plain DELETE the
    " wrong rows are removed as soon as several non-matching rows are dropped
    TYPES: BEGIN OF ty, name TYPE string, city TYPE string, END OF ty.
    TYPES temp19 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp19.
    DATA temp69 LIKE lt_tab.
    DATA temp70 LIKE LINE OF temp69.
    FIELD-SYMBOLS <temp71> LIKE LINE OF lt_tab.
    DATA temp72 LIKE sy-tabix.
    FIELD-SYMBOLS <temp73> LIKE LINE OF lt_tab.
    DATA temp74 LIKE sy-tabix.
    CLEAR temp69.
    
    temp70-name = `Alice`.
    temp70-city = `Paris`.
    INSERT temp70 INTO TABLE temp69.
    temp70-name = `Bob`.
    temp70-city = `Berlin`.
    INSERT temp70 INTO TABLE temp69.
    temp70-name = `Charlie`.
    temp70-city = `Rome`.
    INSERT temp70 INTO TABLE temp69.
    temp70-name = `Dave`.
    temp70-city = `Berlin`.
    INSERT temp70 INTO TABLE temp69.
    temp70-name = `Eve`.
    temp70-city = `Madrid`.
    INSERT temp70 INTO TABLE temp69.
    lt_tab = temp69.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `Berlin`
                                                   CHANGING  tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_tab ) ).
    
    
    temp72 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp71>.
    sy-tabix = temp72.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Bob`  act = <temp71>-name ).
    
    
    temp74 = sy-tabix.
    READ TABLE lt_tab INDEX 2 ASSIGNING <temp73>.
    sy-tabix = temp74.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Dave` act = <temp73>-name ).
  ENDMETHOD.

  METHOD filter_by_val_fields_subset.
    " only the listed field is searched, the match in the other column is ignored
    TYPES: BEGIN OF ty, name TYPE string, city TYPE string, END OF ty.
    TYPES temp20 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp20.
    DATA temp75 LIKE lt_tab.
    DATA temp76 LIKE LINE OF temp75.
    DATA temp77 TYPE string_table.
    FIELD-SYMBOLS <temp79> LIKE LINE OF lt_tab.
    DATA temp80 LIKE sy-tabix.
    CLEAR temp75.
    
    temp76-name = `Berlin`.
    temp76-city = `Paris`.
    INSERT temp76 INTO TABLE temp75.
    temp76-name = `Bob`.
    temp76-city = `Berlin`.
    INSERT temp76 INTO TABLE temp75.
    lt_tab = temp75.
    
    CLEAR temp77.
    INSERT `CITY` INTO TABLE temp77.
    zabaputil_cl_util_context=>itab_filter_by_val(
      EXPORTING val    = `Berlin`
                fields = temp77
      CHANGING  tab    = lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_tab ) ).
    
    
    temp80 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp79>.
    sy-tabix = temp80.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Bob` act = <temp79>-name ).
  ENDMETHOD.

  METHOD corresponding_basic.
    TYPES: BEGIN OF ty_src, a TYPE string, b TYPE string, c TYPE string, END OF ty_src.
    TYPES: BEGIN OF ty_dst, a TYPE string, b TYPE string, END OF ty_dst.
    TYPES temp21 TYPE STANDARD TABLE OF ty_src WITH DEFAULT KEY.
DATA lt_src TYPE temp21.
    TYPES temp22 TYPE STANDARD TABLE OF ty_dst WITH DEFAULT KEY.
DATA lt_dst TYPE temp22.
    DATA temp81 LIKE lt_src.
    DATA temp82 LIKE LINE OF temp81.
    FIELD-SYMBOLS <temp83> LIKE LINE OF lt_dst.
    DATA temp84 LIKE sy-tabix.
    FIELD-SYMBOLS <temp85> LIKE LINE OF lt_dst.
    DATA temp86 LIKE sy-tabix.
    CLEAR temp81.
    
    temp82-a = `1`.
    temp82-b = `2`.
    temp82-c = `3`.
    INSERT temp82 INTO TABLE temp81.
    lt_src = temp81.
    zabaputil_cl_util_context=>itab_corresponding( EXPORTING val = lt_src CHANGING tab = lt_dst ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_dst ) ).
    
    
    temp84 = sy-tabix.
    READ TABLE lt_dst INDEX 1 ASSIGNING <temp83>.
    sy-tabix = temp84.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = <temp83>-a ).
    
    
    temp86 = sy-tabix.
    READ TABLE lt_dst INDEX 1 ASSIGNING <temp85>.
    sy-tabix = temp86.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `2` act = <temp85>-b ).
  ENDMETHOD.

  METHOD get_by_struc_basic.
    TYPES: BEGIN OF ty, name TYPE string, value TYPE string, END OF ty.
    DATA temp87 TYPE ty.
    DATA ls_data LIKE temp87.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp88> LIKE LINE OF lt_result.
    DATA temp89 LIKE sy-tabix.
    FIELD-SYMBOLS <temp90> LIKE LINE OF lt_result.
    DATA temp91 LIKE sy-tabix.
    FIELD-SYMBOLS <temp92> LIKE LINE OF lt_result.
    DATA temp93 LIKE sy-tabix.
    FIELD-SYMBOLS <temp94> LIKE LINE OF lt_result.
    DATA temp95 LIKE sy-tabix.
    CLEAR temp87.
    temp87-name = `hello`.
    temp87-value = `world`.
    
    ls_data = temp87.
    
    lt_result = zabaputil_cl_util_context=>itab_get_by_struc( ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_result ) ).
    
    
    temp89 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp88>.
    sy-tabix = temp89.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NAME` act = <temp88>-n ).
    
    
    temp91 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp90>.
    sy-tabix = temp91.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `hello` act = <temp90>-v ).
    
    
    temp93 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp92>.
    sy-tabix = temp93.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `VALUE` act = <temp92>-n ).
    
    
    temp95 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp94>.
    sy-tabix = temp95.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `world` act = <temp94>-v ).
  ENDMETHOD.

  METHOD csv_roundtrip.
    TYPES: BEGIN OF ty, col1 TYPE string, col2 TYPE string, END OF ty.
    TYPES temp23 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp23.
    DATA temp96 LIKE lt_tab.
    DATA temp97 LIKE LINE OF temp96.
    DATA lv_csv TYPE string.
    CLEAR temp96.
    
    temp97-col1 = `A`.
    temp97-col2 = `B`.
    INSERT temp97 INTO TABLE temp96.
    temp97-col1 = `C`.
    temp97-col2 = `D`.
    INSERT temp97 INTO TABLE temp96.
    lt_tab = temp96.

    
    lv_csv = zabaputil_cl_util_context=>itab_get_csv_by_itab( lt_tab ).
    cl_abap_unit_assert=>assert_not_initial( lv_csv ).

    " CSV should contain header and data
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_csv sub = `COL1` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_csv sub = `A` ) ).
  ENDMETHOD.

  METHOD csv_deep_column.
    " a table inside a row has no text - its cell stays empty instead of
    " ending the call
    TYPES: BEGIN OF ty, col1 TYPE string, sub TYPE string_table, col2 TYPE string, END OF ty.
    TYPES temp24 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp24.
    DATA temp98 LIKE lt_tab.
    DATA temp99 LIKE LINE OF temp98.
    DATA temp3 TYPE string_table.
    DATA lv_csv TYPE string.
    CLEAR temp98.
    
    temp99-col1 = `A`.
    
    CLEAR temp3.
    INSERT `x` INTO TABLE temp3.
    temp99-sub = temp3.
    temp99-col2 = `B`.
    INSERT temp99 INTO TABLE temp98.
    lt_tab = temp98.

    
    lv_csv = zabaputil_cl_util_context=>itab_get_csv_by_itab( lt_tab ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>c_contains( val = lv_csv sub = `A;;B` ) ).
  ENDMETHOD.

  METHOD csv_include_columns.
    " an include is one nameless component of get_components( ) - the header
    " lost its fields while the rows wrote them, so every column behind it
    " shifted, also on the way back in
    TYPES: BEGIN OF ty_inc, b TYPE string, c TYPE string, END OF ty_inc.
    TYPES BEGIN OF ty.
    TYPES a TYPE string.
    INCLUDE TYPE ty_inc.
    TYPES d TYPE string.
    TYPES END OF ty.
    TYPES temp25 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp25.
    FIELD-SYMBOLS <back> TYPE STANDARD TABLE.
    DATA temp100 LIKE lt_tab.
    DATA temp101 LIKE LINE OF temp100.
    DATA lv_csv TYPE string.
    DATA lr_back TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <c> TYPE any.
    FIELD-SYMBOLS <d> TYPE any.
    CLEAR temp100.
    
    temp101-a = `1`.
    temp101-b = `2`.
    temp101-c = `3`.
    temp101-d = `4`.
    INSERT temp101 INTO TABLE temp100.
    lt_tab = temp100.

    
    lv_csv = zabaputil_cl_util_context=>itab_get_csv_by_itab( lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = |A;B;C;D\r\n1;2;3;4|
                                        act = lv_csv ).

    
    lr_back = zabaputil_cl_util_context=>itab_get_itab_by_csv( lv_csv ).
    ASSIGN lr_back->* TO <back>.
    
    READ TABLE <back> INDEX 1 ASSIGNING <row>.
    
    ASSIGN COMPONENT `C` OF STRUCTURE <row> TO <c>.
    cl_abap_unit_assert=>assert_equals( exp = `3`
                                        act = <c> ).
    
    ASSIGN COMPONENT `D` OF STRUCTURE <row> TO <d>.
    cl_abap_unit_assert=>assert_equals( exp = `4`
                                        act = <d> ).
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
    METHODS check_class_exists_no_class    FOR TESTING.
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
    DATA temp102 TYPE abap_component_tab.
    DATA temp103 LIKE LINE OF temp102.
    DATA lt_comps LIKE temp102.
    DATA lt_result TYPE abap_component_tab.
    FIELD-SYMBOLS <temp104> LIKE LINE OF lt_result.
    DATA temp105 LIKE sy-tabix.
    FIELD-SYMBOLS <temp106> LIKE LINE OF lt_result.
    DATA temp107 LIKE sy-tabix.
    FIELD-SYMBOLS <temp108> LIKE LINE OF lt_result.
    DATA temp109 LIKE sy-tabix.
    lo_str = cl_abap_elemdescr=>get_string( ).
    
    CLEAR temp102.
    
    temp103-name = `A`.
    temp103-type = lo_str.
    INSERT temp103 INTO TABLE temp102.
    temp103-name = `B`.
    temp103-type = lo_str.
    INSERT temp103 INTO TABLE temp102.
    temp103-name = `C`.
    temp103-type = lo_str.
    INSERT temp103 INTO TABLE temp102.
    
    lt_comps = temp102.
    
    lt_result = zabaputil_cl_util_context=>expand_components( lt_comps ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_result ) ).
    
    
    temp105 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp104>.
    sy-tabix = temp105.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A` act = <temp104>-name ).
    
    
    temp107 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp106>.
    sy-tabix = temp107.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B` act = <temp106>-name ).
    
    
    temp109 = sy-tabix.
    READ TABLE lt_result INDEX 3 ASSIGNING <temp108>.
    sy-tabix = temp109.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `C` act = <temp108>-name ).
  ENDMETHOD.

  METHOD expand_components_empty.
    DATA temp110 TYPE abap_component_tab.
    DATA lt_result TYPE abap_component_tab.
    CLEAR temp110.
    
    lt_result = zabaputil_cl_util_context=>expand_components( temp110 ).
    cl_abap_unit_assert=>assert_initial( lt_result ).
  ENDMETHOD.

  METHOD expand_components_depth_ok.
    " the last level that is still inside the bound expands normally - the
    " guard must not cost a real DDIC structure its deepest include
    DATA lo_str TYPE REF TO cl_abap_elemdescr.
    DATA temp111 TYPE abap_component_tab.
    DATA temp112 LIKE LINE OF temp111.
    DATA lt_comps LIKE temp111.
    DATA lt_result TYPE abap_component_tab.
    lo_str = cl_abap_elemdescr=>get_string( ).
    
    CLEAR temp111.
    
    temp112-name = `A`.
    temp112-type = lo_str.
    INSERT temp112 INTO TABLE temp111.
    
    lt_comps = temp111.

    
    lt_result = zabaputil_cl_util_context=>expand_components( val   = lt_comps
                                                                    depth = 16 ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_result ) ).
  ENDMETHOD.

  METHOD expand_components_depth_stop.
    " past the bound the mutual recursion stops with a readable error
    " instead of running the stack out - a cyclic include used to dump
    DATA lo_str TYPE REF TO cl_abap_elemdescr.
    DATA temp113 TYPE abap_component_tab.
    DATA temp114 LIKE LINE OF temp113.
    DATA lt_comps LIKE temp113.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
    lo_str = cl_abap_elemdescr=>get_string( ).
    
    CLEAR temp113.
    
    temp114-name = `A`.
    temp114-type = lo_str.
    INSERT temp114 INTO TABLE temp113.
    
    lt_comps = temp113.

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

    DATA temp115 TYPE REF TO cl_abap_datadescr.
    DATA lo_type LIKE temp115.
    temp115 ?= cl_abap_typedescr=>describe_by_data( ls_incl ).
    
    lo_type = temp115.

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
    FIELD-SYMBOLS <temp116> LIKE LINE OF lt_attri.
    DATA temp117 LIKE sy-tabix.
    FIELD-SYMBOLS <temp118> LIKE LINE OF lt_attri.
    DATA temp119 LIKE sy-tabix.
    lt_attri = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( ls_struc ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_attri ) ).
    
    
    temp117 = sy-tabix.
    READ TABLE lt_attri INDEX 1 ASSIGNING <temp116>.
    sy-tabix = temp117.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ALPHA` act = <temp116>-name ).
    
    
    temp119 = sy-tabix.
    READ TABLE lt_attri INDEX 2 ASSIGNING <temp118>.
    sy-tabix = temp119.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `BETA`  act = <temp118>-name ).
  ENDMETHOD.

  METHOD attri_by_any_table.
    " for a table the components of the line type are returned
    TYPES: BEGIN OF ty, alpha TYPE string, beta TYPE i, END OF ty.
    TYPES temp26 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp26.
    DATA lt_attri TYPE abap_component_tab.
    FIELD-SYMBOLS <temp120> LIKE LINE OF lt_attri.
    DATA temp121 LIKE sy-tabix.
    lt_attri = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( lt_tab ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_attri ) ).
    
    
    temp121 = sy-tabix.
    READ TABLE lt_attri INDEX 1 ASSIGNING <temp120>.
    sy-tabix = temp121.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ALPHA` act = <temp120>-name ).
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

  METHOD check_class_exists_no_class.
    " a table, a built-in type, an interface: types, but no class
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `T100` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `STRING` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_class_exists( `IF_MESSAGE` ) ).
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
    METHODS stampl_tz_roundtrip            FOR TESTING.
    METHODS stampl_tz_no_sap_key           FOR TESTING.

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
    DATA temp122 TYPE timestampl.
    temp122 = `20261123140509`.
    cl_abap_unit_assert=>assert_equals(
        exp = `2026-11-23 14:05:09`
        act = zabaputil_cl_util_context=>time_get_text_by_stampl( temp122 ) ).
  ENDMETHOD.

  METHOD text_by_stampl_fraction.
    DATA temp123 TYPE timestampl.
    temp123 = `20261123140509.1234567`.
    cl_abap_unit_assert=>assert_equals(
        exp = `2026-11-23 14:05:09,1234567`
        act = zabaputil_cl_util_context=>time_get_text_by_stampl( temp123 ) ).
  ENDMETHOD.

  METHOD text_by_stampl_utc_zone.
    " an explicit UTC changes nothing - the stamp is stored in UTC
    DATA temp124 TYPE timestampl.
    DATA temp5 TYPE timestampl.
    temp124 = `20261123140509`.
    
    temp5 = `20261123140509`.
    cl_abap_unit_assert=>assert_equals(
        exp = zabaputil_cl_util_context=>time_get_text_by_stampl( temp124 )
        act = zabaputil_cl_util_context=>time_get_text_by_stampl( val       = temp5
                                                                  time_zone = `UTC` ) ).
  ENDMETHOD.

  METHOD stampl_tz_roundtrip.
    DATA temp125 TYPE timestampl.
    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA temp126 TYPE d.
    DATA temp127 TYPE t.
    DATA temp128 TYPE timestampl.
    temp125 = `20261123140509`.
    
    
    zabaputil_cl_util_context=>time_stampl_to_tz( EXPORTING val  = temp125
                                                  IMPORTING date = lv_date
                                                            time = lv_time ).
    
    temp126 = `20261123`.
    cl_abap_unit_assert=>assert_equals( exp = temp126 act = lv_date ).
    
    temp127 = `140509`.
    cl_abap_unit_assert=>assert_equals( exp = temp127 act = lv_time ).
    
    temp128 = `20261123140509`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp128
        act = zabaputil_cl_util_context=>time_stampl_from_tz( date = lv_date
                                                              time = lv_time ) ).
  ENDMETHOD.

  METHOD stampl_tz_no_sap_key.
        DATA temp129 TYPE timestampl.
        DATA lv_date TYPE d.
    " an IANA name was cut to `Europe` and gave an empty date and time
    TRY.
        
        temp129 = `20261123140509`.
        
        zabaputil_cl_util_context=>time_stampl_to_tz( EXPORTING val  = temp129
                                                                tz   = `Europe/Berlin`
                                                      IMPORTING date = lv_date ).
        cl_abap_unit_assert=>fail( |no exception, date { lv_date }| ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
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
    DATA temp130 TYPE d.
    DATA temp6 TYPE t.
    DATA lv_ts TYPE timestampl.
    temp130 = `20240101`.
    
    temp6 = `120000`.
    
    lv_ts = zabaputil_cl_util_context=>time_get_stampl_by_date_time( date = temp130
                                                                time = temp6 ).
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
    METHODS num_thousands_only             FOR TESTING.
    METHODS num_only_minus                 FOR TESTING.
    METHODS num_leading_zeros              FOR TESTING.

    " conv_string_to_date edge cases
    METHODS date_short_input               FOR TESTING.
    METHODS date_single_digit_parts        FOR TESTING.

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
    " Single '*' - the match-all pattern, kept as it was typed
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `*` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `*` act = ls_range-low ).
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
    TYPES temp27 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp27.
    DATA temp131 LIKE lt_tab.
    DATA temp132 LIKE LINE OF temp131.
    CLEAR temp131.
    
    temp132-name = `Hello`.
    INSERT temp132 INTO TABLE temp131.
    temp132-name = `WORLD`.
    INSERT temp132 INTO TABLE temp131.
    temp132-name = `test`.
    INSERT temp132 INTO TABLE temp131.
    lt_tab = temp131.

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
    " a single separator is the decimal one: '1,000' is 1 (European
    " convention) - see conv_string_to_number
    DATA lv_result TYPE decfloat34.
    DATA temp133 TYPE decfloat34.
    lv_result = zabaputil_cl_util_context=>conv_string_to_number( `1,000` ).
    
    temp133 = '1.000'.
    cl_abap_unit_assert=>assert_equals( exp = temp133 act = lv_result ).
  ENDMETHOD.

  METHOD num_european_format.
    " both separators: the last one is the decimal separator
    DATA temp134 TYPE decfloat34.
    DATA temp135 TYPE decfloat34.
    DATA temp136 TYPE decfloat34.
    temp134 = '1234.56'.
    cl_abap_unit_assert=>assert_equals( exp = temp134
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `1.234,56` ) ).
    
    temp135 = '1234567.89'.
    cl_abap_unit_assert=>assert_equals( exp = temp135
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `1.234.567,89` ) ).
    
    temp136 = '1234567.89'.
    cl_abap_unit_assert=>assert_equals( exp = temp136
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `1,234,567.89` ) ).
  ENDMETHOD.

  METHOD num_thousands_only.
    " one kind of separator, several times: thousands separators
    DATA temp137 TYPE decfloat34.
    DATA temp138 TYPE decfloat34.
    DATA temp139 TYPE decfloat34.
    temp137 = 1234567.
    cl_abap_unit_assert=>assert_equals( exp = temp137
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `1.234.567` ) ).
    
    temp138 = 1000000.
    cl_abap_unit_assert=>assert_equals( exp = temp138
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `1,000,000` ) ).
    
    temp139 = -1000000.
    cl_abap_unit_assert=>assert_equals( exp = temp139
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `-1,000,000` ) ).
  ENDMETHOD.

  METHOD num_only_minus.
    " Just a minus sign
    DATA lv_result TYPE decfloat34.
    DATA temp140 TYPE decfloat34.
    lv_result = zabaputil_cl_util_context=>conv_string_to_number( `-` ).
    
    temp140 = 0.
    cl_abap_unit_assert=>assert_equals( exp = temp140 act = lv_result ).
  ENDMETHOD.

  METHOD num_leading_zeros.
    DATA lv_result TYPE decfloat34.
    DATA temp141 TYPE decfloat34.
    lv_result = zabaputil_cl_util_context=>conv_string_to_number( `007` ).
    
    temp141 = 7.
    cl_abap_unit_assert=>assert_equals( exp = temp141 act = lv_result ).
  ENDMETHOD.

  METHOD date_short_input.
    " fewer digits than the format asks for: no date, and no exception
    DATA lv_result TYPE d.
    lv_result = zabaputil_cl_util_context=>conv_string_to_date( val    = `24`
                                                                      format = `YYYY-MM-DD` ).
    cl_abap_unit_assert=>assert_initial( lv_result ).
  ENDMETHOD.

  METHOD date_single_digit_parts.
    " a day or a month written with one digit is read by its part, not
    " digit by digit - `5.3.2024` was an initial date, with a time behind
    " it the year 2410
    DATA temp142 TYPE d.
    DATA temp143 TYPE d.
    DATA temp144 TYPE d.
    temp142 = `20240305`.
    cl_abap_unit_assert=>assert_equals( exp = temp142
                                        act = zabaputil_cl_util_context=>conv_string_to_date( val    = `5.3.2024`
                                                                                              format = `DD.MM.YYYY` ) ).
    
    temp143 = `20240305`.
    cl_abap_unit_assert=>assert_equals( exp = temp143
                                        act = zabaputil_cl_util_context=>conv_string_to_date( val    = `5.3.2024 10:30`
                                                                                              format = `DD.MM.YYYY` ) ).
    
    temp144 = `20240105`.
    cl_abap_unit_assert=>assert_equals( exp = temp144
                                        act = zabaputil_cl_util_context=>conv_string_to_date( `2024-1-5` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_date_valid( `2024-1-5` ) ).
    " a part longer than the format allows, or a year that is too short
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>conv_string_to_date( val    = `15.003.2024`
                                                                                         format = `DD.MM.YYYY` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>conv_string_to_date( val    = `5.3.24`
                                                                                         format = `DD.MM.YYYY` ) ).
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
    TYPES temp28 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp28.
    DATA temp145 LIKE lt_tab.
    DATA temp146 LIKE LINE OF temp145.
    DATA lr_result TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CLEAR temp145.
    
    temp146-id = 1.
    INSERT temp146 INTO TABLE temp145.
    temp146-id = 2.
    INSERT temp146 INTO TABLE temp145.
    lt_tab = temp145.
    
    lr_result = zabaputil_cl_util_context=>itab_slice( tab = lt_tab from = 99 ).
    
    ASSIGN lr_result->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( <tab> ) ).
  ENDMETHOD.

  METHOD slice_empty_table.
    TYPES: BEGIN OF ty, id TYPE i, END OF ty.
    TYPES temp29 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp29.
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
    DATA temp147 TYPE ty.
    DATA ls_data LIKE temp147.
    CLEAR temp147.
    temp147-name = `hello`.
    
    ls_data = temp147.
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
    DATA temp148 TYPE ty.
    DATA ls_data LIKE temp148.
    DATA lv_json TYPE string.
    DATA ls_back TYPE ty.
    CLEAR temp148.
    temp148-text = `He said "hello" \\ yes`.
    
    ls_data = temp148.
    
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
"! Few BAL tests: the BAL function modules and CL_BALI_* do not exist in
"! the transpiled runtime, and every bal_* method raises when they fail.
"! (The ASSIGN_TYPE_CONFLICT of bal_search is fixed - the BAL tables are
"! typed generically.) What runs here is what happens before any of them
"! is called.
"! ================================================================
CLASS ltcl_bal_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS delete_without_keys FOR TESTING.

ENDCLASS.

CLASS ltcl_bal_ops IMPLEMENTATION.

  METHOD delete_without_keys.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
    " no keys are no filter - they would delete every log of the client
    " (or of every object), so nothing is searched and nothing deleted:
    " no call that could fail is made
    TRY.
        zabaputil_cl_util_context=>bal_delete( object    = ``
                                               subobject = ``
                                               id        = `` ).
        zabaputil_cl_util_context=>bal_delete_before( `` ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>fail( lx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_dyn_call DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS interface_method FOR TESTING.
    METHODS missing_member   FOR TESTING.

ENDCLASS.

CLASS zabaputil_cl_util_context DEFINITION LOCAL FRIENDS ltcl_dyn_call.

CLASS ltcl_dyn_call IMPLEMENTATION.

  METHOD interface_method.
    " the cloud branches of tr_* reach XCO this way: GET_NODE_TYPE is a
    " method of the interface only (no alias in the class), so it is
    " called as ZABAPUTIL_IF_AJSON~GET_NODE_TYPE, its one importing
    " parameter filled from arg
    FIELD-SYMBOLS <value> TYPE any.
    DATA lo_json TYPE REF TO zabaputil_cl_ajson.
    DATA lr_value TYPE REF TO data.
    lo_json = zabaputil_cl_ajson=>parse( `{"a":"x"}` ).
    
    lr_value = zabaputil_cl_util_context=>dyn_call( obj  = lo_json
                                                         name = `get_node_type`
                                                         arg  = `/a` ).
    ASSIGN lr_value->* TO <value>.
    cl_abap_unit_assert=>assert_equals( exp = `str`
                                        act = <value> ).
  ENDMETHOD.

  METHOD missing_member.
    TRY.
        zabaputil_cl_util_context=>dyn_call( obj  = zabaputil_cl_ajson=>create_empty( )
                                             name = `NO_SUCH_MEMBER` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

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
    METHODS count_workdays_invalid_date    FOR TESTING.
    METHODS count_workdays_backwards       FOR TESTING.

ENDCLASS.

CLASS ltcl_calendar_ops IMPLEMENTATION.

  METHOD weekday_monday.
    DATA temp149 TYPE d.
    temp149 = `20240101`.
    cl_abap_unit_assert=>assert_equals( exp = 1
        act = zabaputil_cl_util_context=>cal_get_weekday( temp149 ) ).
  ENDMETHOD.

  METHOD weekday_sunday.
    DATA temp150 TYPE d.
    temp150 = `20240107`.
    cl_abap_unit_assert=>assert_equals( exp = 7
        act = zabaputil_cl_util_context=>cal_get_weekday( temp150 ) ).
  ENDMETHOD.

  METHOD weekday_wednesday.
    DATA temp151 TYPE d.
    temp151 = `20240103`.
    cl_abap_unit_assert=>assert_equals( exp = 3
        act = zabaputil_cl_util_context=>cal_get_weekday( temp151 ) ).
  ENDMETHOD.

  METHOD weekday_saturday.
    DATA temp152 TYPE d.
    temp152 = `20240106`.
    cl_abap_unit_assert=>assert_equals( exp = 6
        act = zabaputil_cl_util_context=>cal_get_weekday( temp152 ) ).
  ENDMETHOD.

  METHOD is_weekend_saturday.
    DATA temp153 TYPE d.
    temp153 = `20240106`.
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>cal_is_weekend( temp153 ) ).
  ENDMETHOD.

  METHOD is_weekend_sunday.
    DATA temp154 TYPE d.
    temp154 = `20240107`.
    cl_abap_unit_assert=>assert_true(
        zabaputil_cl_util_context=>cal_is_weekend( temp154 ) ).
  ENDMETHOD.

  METHOD is_weekend_friday.
    DATA temp155 TYPE d.
    temp155 = `20240105`.
    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_util_context=>cal_is_weekend( temp155 ) ).
  ENDMETHOD.

  METHOD add_workdays_skip_weekend.
    DATA temp156 TYPE d.
    DATA temp7 TYPE d.
    temp156 = `20240318`.
    
    temp7 = `20240315`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp156
        act = zabaputil_cl_util_context=>cal_add_workdays( date = temp7 days = 1 ) ).
  ENDMETHOD.

  METHOD add_workdays_mid_week.
    DATA temp157 TYPE d.
    DATA temp8 TYPE d.
    temp157 = `20240104`.
    
    temp8 = `20240102`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp157
        act = zabaputil_cl_util_context=>cal_add_workdays( date = temp8 days = 2 ) ).
  ENDMETHOD.

  METHOD add_workdays_zero.
    DATA temp158 TYPE d.
    DATA temp9 TYPE d.
    temp158 = `20240315`.
    
    temp9 = `20240315`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp158
        act = zabaputil_cl_util_context=>cal_add_workdays( date = temp9 days = 0 ) ).
  ENDMETHOD.

  METHOD count_workdays_full_week.
    DATA temp159 TYPE d.
    DATA temp10 TYPE d.
    temp159 = `20240101`.
    
    temp10 = `20240108`.
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = zabaputil_cl_util_context=>cal_count_workdays( date_from = temp159
                                                      date_to   = temp10 ) ).
  ENDMETHOD.

  METHOD count_workdays_invalid_date.
        DATA temp160 TYPE d.
        DATA temp11 TYPE d.
        DATA temp161 TYPE d.
        DATA temp12 TYPE d.
    " 30 February: the loop never met it and ran forever
    TRY.
        
        temp160 = `20240101`.
        
        temp11 = `20240230`.
        zabaputil_cl_util_context=>cal_count_workdays( date_from = temp160
                                                       date_to   = temp11 ).
        cl_abap_unit_assert=>fail( `invalid date - must raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        
        CLEAR temp161.
        
        temp12 = `20240101`.
        zabaputil_cl_util_context=>cal_count_workdays( date_from = temp12
                                                       date_to   = temp161 ).
        cl_abap_unit_assert=>fail( `initial date - must raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD count_workdays_backwards.
    DATA temp162 TYPE d.
    DATA temp13 TYPE d.
    temp162 = `20240108`.
    
    temp13 = `20240101`.
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = zabaputil_cl_util_context=>cal_count_workdays( date_from = temp162
                                                             date_to   = temp13 ) ).
  ENDMETHOD.

  METHOD count_workdays_same_day.
    DATA temp163 TYPE d.
    DATA temp14 TYPE d.
    temp163 = `20240101`.
    
    temp14 = `20240101`.
    cl_abap_unit_assert=>assert_equals(
        exp = 0
        act = zabaputil_cl_util_context=>cal_count_workdays( date_from = temp163
                                                      date_to   = temp14 ) ).
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
    METHODS unpack_no_archive              FOR TESTING.
    METHODS one_file_at_a_time             FOR TESTING.

ENDCLASS.

CLASS ltcl_zip_ops IMPLEMENTATION.

  METHOD one_file_at_a_time.
    DATA lo_probe TYPE REF TO object.
    DATA temp164 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp165 LIKE LINE OF temp164.
    DATA temp15 TYPE xstring.
    DATA temp16 TYPE xstring.
    DATA lv_zip TYPE xstring.
    DATA lo_zip TYPE REF TO object.
    DATA temp166 TYPE string_table.
    DATA temp168 TYPE xstring.
        DATA temp169 TYPE xstring.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    CLEAR temp164.
    
    temp165-name = `a.txt`.
    
    temp15 = `41`.
    temp165-content = temp15.
    INSERT temp165 INTO TABLE temp164.
    temp165-name = `dir/b.txt`.
    
    temp16 = `4242`.
    temp165-content = temp16.
    INSERT temp165 INTO TABLE temp164.
    
    lv_zip = zabaputil_cl_util_context=>zip_pack( temp164 ).
    
    lo_zip = zabaputil_cl_util_context=>zip_load( lv_zip ).
    
    CLEAR temp166.
    INSERT `a.txt` INTO TABLE temp166.
    INSERT `dir/b.txt` INTO TABLE temp166.
    cl_abap_unit_assert=>assert_equals( exp = temp166
                                        act = zabaputil_cl_util_context=>zip_names( lo_zip ) ).
    
    temp168 = `4242`.
    cl_abap_unit_assert=>assert_equals( exp = temp168
                                        act = zabaputil_cl_util_context=>zip_get( zip  = lo_zip
                                                                                  name = `dir/b.txt` ) ).
    TRY.
        zabaputil_cl_util_context=>zip_get( zip  = lo_zip
                                            name = `c.txt` ).
        cl_abap_unit_assert=>fail( `a file the archive does not have has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        
        temp169 = `48656C6C6F`.
        zabaputil_cl_util_context=>zip_load( temp169 ).
        cl_abap_unit_assert=>fail( `data that is no archive has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD unpack_no_archive.
    DATA lo_probe TYPE REF TO object.
        DATA temp170 TYPE xstring.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    " bytes that are no archive raise the one exception of this
    " repository - the ZIP class reports them with a classic exception,
    " which nobody took: a dump
    TRY.
        
        temp170 = `48656C6C6F2C20776F726C6421`.
        zabaputil_cl_util_context=>zip_unpack( temp170 ).
        cl_abap_unit_assert=>fail( `data that is no archive has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD pack_basic.
    DATA lo_probe TYPE REF TO object.
    DATA temp171 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp172 LIKE LINE OF temp171.
    DATA temp17 TYPE xstring.
    DATA lt_files LIKE temp171.
    DATA lv_archive TYPE xstring.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    CLEAR temp171.
    
    temp172-name = `test.txt`.
    
    temp17 = `48656C6C6F`.
    temp172-content = temp17.
    INSERT temp172 INTO TABLE temp171.
    
    lt_files = temp171.
    
    lv_archive = zabaputil_cl_util_context=>zip_pack( lt_files ).
    cl_abap_unit_assert=>assert_not_initial( lv_archive ).
  ENDMETHOD.

  METHOD pack_empty.
    DATA lo_probe TYPE REF TO object.
    DATA temp173 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA lt_files LIKE temp173.
    DATA lv_archive TYPE xstring.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    CLEAR temp173.
    
    lt_files = temp173.
    
    lv_archive = zabaputil_cl_util_context=>zip_pack( lt_files ).
    cl_abap_unit_assert=>assert_not_initial( lv_archive ).
  ENDMETHOD.

  METHOD roundtrip.
    DATA lo_probe TYPE REF TO object.
    DATA temp174 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp175 LIKE LINE OF temp174.
    DATA temp18 TYPE xstring.
    DATA temp19 TYPE xstring.
    DATA lt_in LIKE temp174.
    DATA lv_archive TYPE xstring.
    DATA lt_out TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA ls_in LIKE LINE OF lt_in.
      DATA temp176 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA temp177 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA ls_out LIKE temp176.
    TRY.
        CREATE OBJECT lo_probe TYPE ('CL_ABAP_ZIP').
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    CLEAR temp174.
    
    temp175-name = `a.txt`.
    
    temp18 = `414141`.
    temp175-content = temp18.
    INSERT temp175 INTO TABLE temp174.
    temp175-name = `b.txt`.
    
    temp19 = `424242`.
    temp175-content = temp19.
    INSERT temp175 INTO TABLE temp174.
    
    lt_in = temp174.

    
    lv_archive = zabaputil_cl_util_context=>zip_pack( lt_in ).
    
    lt_out = zabaputil_cl_util_context=>zip_unpack( lv_archive ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_out ) ).

    
    LOOP AT lt_in INTO ls_in.
      
      CLEAR temp176.
      
      READ TABLE lt_out INTO temp177 WITH KEY name = ls_in-name.
      IF sy-subrc = 0.
        temp176 = temp177.
      ENDIF.
      
      ls_out = temp176.
      cl_abap_unit_assert=>assert_equals( exp = ls_in-content act = ls_out-content ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! DEFLATE / ZLIB / ADLER-32 / CODE POINTS
"! ================================================================
CLASS ltcl_gzip_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS gzip_roundtrip                 FOR TESTING.
    METHODS gzip_shrinks                   FOR TESTING.
    METHODS zlib_roundtrip                 FOR TESTING.
    METHODS zlib_header_and_checksum       FOR TESTING.
    METHODS zlib_decompress_foreign        FOR TESTING.
    METHODS zlib_decompress_no_stream      FOR TESTING.
    METHODS adler32_known                  FOR TESTING.
    METHODS adler32_empty                  FOR TESTING.
    METHODS adler32_long                   FOR TESTING.
    METHODS codepoints_ascii               FOR TESTING.
    METHODS codepoints_multibyte           FOR TESTING.
    METHODS codepoints_empty               FOR TESTING.

    METHODS has_gzip
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS ltcl_gzip_ops IMPLEMENTATION.

  METHOD has_gzip.
        DATA temp178 TYPE xstring.
    TRY.
        
        temp178 = `00`.
        zabaputil_cl_util_context=>gzip_compress( temp178 ).
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD gzip_roundtrip.
    DATA temp179 TYPE xstring.
    DATA lv_in LIKE temp179.
    DATA lv_packed TYPE xstring.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.

    
    temp179 = `48656C6C6F2C2048656C6C6F2C2048656C6C6F21`.
    
    lv_in = temp179.
    
    lv_packed = zabaputil_cl_util_context=>gzip_compress( lv_in ).
    cl_abap_unit_assert=>assert_not_initial( lv_packed ).
    cl_abap_unit_assert=>assert_equals( exp = lv_in
                                        act = zabaputil_cl_util_context=>gzip_decompress( lv_packed ) ).
  ENDMETHOD.

  METHOD gzip_shrinks.
    DATA lv_hex TYPE string.
    DATA temp180 TYPE xstring.
    DATA lv_in LIKE temp180.
    DATA lv_packed TYPE xstring.
    DATA temp6 TYPE xsdboolean.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.

    
    DO 500 TIMES.
      lv_hex = lv_hex && `41424344`.
    ENDDO.
    
    temp180 = lv_hex.
    
    lv_in = temp180.
    
    lv_packed = zabaputil_cl_util_context=>gzip_compress( lv_in ).
    
    temp6 = boolc( xstrlen( lv_packed ) < 200 ).
    cl_abap_unit_assert=>assert_true( temp6 ).
  ENDMETHOD.

  METHOD zlib_roundtrip.
    DATA temp181 TYPE xstring.
    DATA lv_in LIKE temp181.
    DATA lv_packed TYPE xstring.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.

    
    temp181 = `000102030405060708090A0B0C0D0E0FFFFEFDFC`.
    
    lv_in = temp181.
    
    lv_packed = zabaputil_cl_util_context=>zlib_compress( lv_in ).
    cl_abap_unit_assert=>assert_equals( exp = lv_in
                                        act = zabaputil_cl_util_context=>zlib_decompress( lv_packed ) ).
  ENDMETHOD.

  METHOD zlib_header_and_checksum.
    DATA temp182 TYPE xstring.
    DATA lv_packed TYPE xstring.
    DATA lv_len TYPE i.
    DATA lv_off TYPE i.
    DATA lv_head TYPE xstring.
    DATA lv_tail TYPE xstring.
    DATA temp183 TYPE xstring.
    DATA temp184 TYPE xstring.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.

    " `Wikipedia` in ASCII, Adler-32 11E60398
    
    temp182 = `57696B697065646961`.
    
    lv_packed = zabaputil_cl_util_context=>zlib_compress( temp182 ).
    
    lv_len = xstrlen( lv_packed ).
    
    lv_off = lv_len - 4.
    
    lv_head = lv_packed(2).
    
    lv_tail = lv_packed+lv_off(4).
    
    temp183 = `789C`.
    cl_abap_unit_assert=>assert_equals( exp = temp183
                                        act = lv_head ).
    
    temp184 = `11E60398`.
    cl_abap_unit_assert=>assert_equals( exp = temp184
                                        act = lv_tail ).
  ENDMETHOD.

  METHOD zlib_decompress_foreign.
    DATA temp185 TYPE xstring.
    DATA lv_packed LIKE temp185.
    DATA temp186 TYPE xstring.
    IF has_gzip( ) = abap_false.
      RETURN.
    ENDIF.

    " `hello hello hello hello`, packed by another implementation with the
    " best compression - the header is 78 DA there, not 78 9C
    
    temp185 = `78DACB48CDC9C957C8402701680308B1`.
    
    lv_packed = temp185.
    
    temp186 = `68656C6C6F2068656C6C6F2068656C6C6F2068656C6C6F`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp186
        act = zabaputil_cl_util_context=>zlib_decompress( lv_packed ) ).
  ENDMETHOD.

  METHOD zlib_decompress_no_stream.
        DATA temp187 TYPE xstring.
        DATA temp188 TYPE xstring.
    TRY.
        
        temp187 = `0102030405060708`.
        zabaputil_cl_util_context=>zlib_decompress( temp187 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

    TRY.
        
        temp188 = `789C`.
        zabaputil_cl_util_context=>zlib_decompress( temp188 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD adler32_known.
    DATA temp189 TYPE xstring.
    DATA temp20 TYPE xstring.
    temp189 = `11E60398`.
    
    temp20 = `57696B697065646961`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp189
        act = zabaputil_cl_util_context=>hash_adler32( temp20 ) ).
  ENDMETHOD.

  METHOD adler32_empty.
    DATA lv_empty TYPE xstring.
    DATA temp190 TYPE xstring.
    temp190 = `00000001`.
    cl_abap_unit_assert=>assert_equals( exp = temp190
                                        act = zabaputil_cl_util_context=>hash_adler32( lv_empty ) ).
  ENDMETHOD.

  METHOD adler32_long.
    " 5000 bytes FF, past the deferred modulo - the value another
    " implementation computes for them
    DATA lv_hex TYPE string.
    DATA temp191 TYPE xstring.
    DATA temp21 TYPE xstring.
    DO 5000 TIMES.
      lv_hex = lv_hex && `FF`.
    ENDDO.
    
    temp191 = `54B27596`.
    
    temp21 = lv_hex.
    cl_abap_unit_assert=>assert_equals( exp = temp191
                                        act = zabaputil_cl_util_context=>hash_adler32( temp21 ) ).
  ENDMETHOD.

  METHOD codepoints_ascii.
    DATA lt_cp TYPE zabaputil_cl_util_context=>ty_t_codepoint.
    FIELD-SYMBOLS <temp192> LIKE LINE OF lt_cp.
    DATA temp193 LIKE sy-tabix.
    FIELD-SYMBOLS <temp194> LIKE LINE OF lt_cp.
    DATA temp195 LIKE sy-tabix.
    FIELD-SYMBOLS <temp196> LIKE LINE OF lt_cp.
    DATA temp197 LIKE sy-tabix.
    FIELD-SYMBOLS <temp198> LIKE LINE OF lt_cp.
    DATA temp199 LIKE sy-tabix.
    lt_cp = zabaputil_cl_util_context=>c_get_codepoints( `Az 0` ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( lt_cp ) ).
    
    
    temp193 = sy-tabix.
    READ TABLE lt_cp INDEX 1 ASSIGNING <temp192>.
    sy-tabix = temp193.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 65 act = <temp192> ).
    
    
    temp195 = sy-tabix.
    READ TABLE lt_cp INDEX 2 ASSIGNING <temp194>.
    sy-tabix = temp195.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 122 act = <temp194> ).
    
    
    temp197 = sy-tabix.
    READ TABLE lt_cp INDEX 3 ASSIGNING <temp196>.
    sy-tabix = temp197.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 32 act = <temp196> ).
    
    
    temp199 = sy-tabix.
    READ TABLE lt_cp INDEX 4 ASSIGNING <temp198>.
    sy-tabix = temp199.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 48 act = <temp198> ).
  ENDMETHOD.

  METHOD codepoints_multibyte.
    " a-umlaut (C3 A4), the euro sign (E2 82 AC) and U+1F600 (F0 9F 98 80)
    DATA temp200 TYPE xstring.
    DATA lv_text TYPE string.
    DATA lt_cp TYPE zabaputil_cl_util_context=>ty_t_codepoint.
    FIELD-SYMBOLS <temp201> LIKE LINE OF lt_cp.
    DATA temp202 LIKE sy-tabix.
    FIELD-SYMBOLS <temp203> LIKE LINE OF lt_cp.
    DATA temp204 LIKE sy-tabix.
    FIELD-SYMBOLS <temp205> LIKE LINE OF lt_cp.
    DATA temp206 LIKE sy-tabix.
    temp200 = `C3A4E282ACF09F9880`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_xstring(
                        temp200 ).
    
    lt_cp = zabaputil_cl_util_context=>c_get_codepoints( lv_text ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_cp ) ).
    
    
    temp202 = sy-tabix.
    READ TABLE lt_cp INDEX 1 ASSIGNING <temp201>.
    sy-tabix = temp202.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 228 act = <temp201> ).
    
    
    temp204 = sy-tabix.
    READ TABLE lt_cp INDEX 2 ASSIGNING <temp203>.
    sy-tabix = temp204.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 8364 act = <temp203> ).
    
    
    temp206 = sy-tabix.
    READ TABLE lt_cp INDEX 3 ASSIGNING <temp205>.
    sy-tabix = temp206.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 128512 act = <temp205> ).
  ENDMETHOD.

  METHOD codepoints_empty.
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>c_get_codepoints( `` ) ).
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
    DATA temp7 TYPE xsdboolean.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = `DFIES`
      RECEIVING
        p_descr_ref    = lo_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    
    temp7 = boolc( sy-subrc = 0 ).
    result = temp7.
  ENDMETHOD.

  METHOD dfies_by_table_basic.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_dfies.
    IF check_dfies_available( ) = abap_false.
      RETURN.
    ENDIF.
    
    lt_result = zabaputil_cl_util_context=>rtti_get_t_dfies_by_table_name( `ZABAPUTIL_T_91` ).
    cl_abap_unit_assert=>assert_not_initial( lt_result ).
  ENDMETHOD.

  METHOD dfies_by_table_has_fields.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_dfies.
    DATA lv_found LIKE abap_false.
    DATA ls_dfies LIKE LINE OF lt_result.
    IF check_dfies_available( ) = abap_false.
      RETURN.
    ENDIF.
    
    lt_result = zabaputil_cl_util_context=>rtti_get_t_dfies_by_table_name( `ZABAPUTIL_T_91` ).
    
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
    DATA temp207 TYPE abap_component_tab.
    DATA temp208 LIKE LINE OF temp207.
    DATA temp22 TYPE REF TO cl_abap_datadescr.
    DATA temp23 TYPE REF TO cl_abap_datadescr.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <qty> TYPE any.
    CLEAR temp207.
    
    temp208-name = `ID`.
    
    temp22 ?= cl_abap_datadescr=>describe_by_data( lv_id ).
    temp208-type = temp22.
    INSERT temp208 INTO TABLE temp207.
    temp208-name = `QTY`.
    
    temp23 ?= cl_abap_datadescr=>describe_by_data( lv_qty ).
    temp208-type = temp23.
    INSERT temp208 INTO TABLE temp207.
    
    lr_tab = zabaputil_cl_util_context=>rtti_create_tab_by_comps( temp207 ).
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
    DATA temp209 TYPE string.
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
    
    temp209 = ls_result-rollname.
    cl_abap_unit_assert=>assert_equals( exp = `MANDT`
                                        act = condense( temp209 ) ).
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
    
    lv_result = zabaputil_cl_util_context=>rtti_get_table_desrc( `ZABAPUTIL_T_91` ).
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
"! TABLE OF STRINGS BY NAMES, TYPE KINDS
"! ================================================================
CLASS ltcl_tab_by_names DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS string_columns                 FOR TESTING.
    METHODS type_kinds                     FOR TESTING.

ENDCLASS.

CLASS ltcl_tab_by_names IMPLEMENTATION.

  METHOD string_columns.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    DATA temp210 TYPE string_table.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <text> TYPE any.
    CLEAR temp210.
    INSERT `ORDER_ID` INTO TABLE temp210.
    INSERT `TEXT` INTO TABLE temp210.
    
    lr_tab = zabaputil_cl_util_context=>rtti_create_tab_by_names(
                       temp210 ).
    ASSIGN lr_tab->* TO <tab>.
    cl_abap_unit_assert=>assert_initial( <tab> ).

    
    INSERT INITIAL LINE INTO TABLE <tab> ASSIGNING <row>.
    
    ASSIGN COMPONENT `TEXT` OF STRUCTURE <row> TO <text>.
    cl_abap_unit_assert=>assert_subrc( ).
    " a string: nothing of a long value is cut off
    <text> = repeat( val = `x`
                     occ = 500 ).
    cl_abap_unit_assert=>assert_equals( exp = 500
                                        act = strlen( <text> ) ).
    " two names, two components
    ASSIGN COMPONENT 3 OF STRUCTURE <row> TO <text>.
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).
  ENDMETHOD.

  METHOD type_kinds.
    DATA lv_char   TYPE c LENGTH 3.
    DATA lv_numc   TYPE n LENGTH 3.
    DATA lv_string TYPE string.
    DATA lv_hex    TYPE x LENGTH 2.
    DATA lv_bytes  TYPE xstring.
    DATA lv_packed TYPE p LENGTH 8 DECIMALS 2.

    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_util_context=>cv_typedescr_typekind_char
                                        act = zabaputil_cl_util_context=>rtti_get_type_kind( lv_char ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_util_context=>cv_typedescr_typekind_num
                                        act = zabaputil_cl_util_context=>rtti_get_type_kind( lv_numc ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_util_context=>cv_typedescr_typekind_string
                                        act = zabaputil_cl_util_context=>rtti_get_type_kind( lv_string ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_util_context=>cv_typedescr_typekind_hex
                                        act = zabaputil_cl_util_context=>rtti_get_type_kind( lv_hex ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_util_context=>cv_typedescr_typekind_xstring
                                        act = zabaputil_cl_util_context=>rtti_get_type_kind( lv_bytes ) ).

    " a number by its type kind - a NUMC and a text are none
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_typekind_number(
                                          zabaputil_cl_util_context=>rtti_get_type_kind( lv_packed ) ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_typekind_number(
                                           zabaputil_cl_util_context=>rtti_get_type_kind( lv_numc ) ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_typekind_number(
                                           zabaputil_cl_util_context=>rtti_get_type_kind( lv_string ) ) ).
  ENDMETHOD.

ENDCLASS.

"! ================================================================
"! XML PULL READER
"! ================================================================
CLASS ltcl_xml_reader DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS read
      IMPORTING
        xml           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS nodes_in_order                 FOR TESTING.
    METHODS attributes                     FOR TESTING.
    METHODS entities_and_blanks            FOR TESTING.
    METHODS namespace_prefix               FOR TESTING.
    METHODS broken_document                FOR TESTING.

ENDCLASS.

CLASS ltcl_xml_reader IMPLEMENTATION.

  METHOD read.
    " every node as one token: +name[attr=value] for an opening, =value for
    " a text, -name for an end
    DATA lv_kind  TYPE string.
    DATA lv_name  TYPE string.
    DATA lv_value TYPE string.
    DATA lt_attr  TYPE zabaputil_cl_util_context=>ty_t_xml_attribute.

    DATA lo_reader TYPE REF TO object.
          DATA ls_attr LIKE LINE OF lt_attr.
    lo_reader = zabaputil_cl_util_context=>xml_reader_create(
                          zabaputil_cl_util_context=>conv_get_xstring_by_string( xml ) ).
    DO.
      zabaputil_cl_util_context=>xml_reader_next( EXPORTING reader      = lo_reader
                                                  IMPORTING kind        = lv_kind
                                                            name        = lv_name
                                                            value       = lv_value
                                                            t_attribute = lt_attr ).
      CASE lv_kind.
        WHEN zabaputil_cl_util_context=>cs_xml_node-open.
          result = |{ result }+{ lv_name }|.
          
          LOOP AT lt_attr INTO ls_attr.
            result = |{ result }[{ ls_attr-name }={ ls_attr-value }]|.
          ENDLOOP.
        WHEN zabaputil_cl_util_context=>cs_xml_node-value.
          result = |{ result }={ lv_value }|.
        WHEN zabaputil_cl_util_context=>cs_xml_node-close.
          result = |{ result }-{ lv_name }|.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDDO.
  ENDMETHOD.

  METHOD nodes_in_order.
    cl_abap_unit_assert=>assert_equals( exp = `+a+b=1-b+c-c-a`
                                        act = read( `<a><b>1</b><c/></a>` ) ).
  ENDMETHOD.

  METHOD attributes.
    cl_abap_unit_assert=>assert_equals(
        exp = `+row[r=2][spans=1:3]+c[r=A2][t=s]-c-row`
        act = read( `<?xml version="1.0" encoding="UTF-8"?><row r="2" spans="1:3"><c r="A2" t="s"/></row>` ) ).
  ENDMETHOD.

  METHOD entities_and_blanks.
    " an entity arrives decoded, and a text of blanks only is kept
    cl_abap_unit_assert=>assert_equals( exp = `+a+t=a&b<c-t+t=  -t-a`
                                        act = read( `<a><t>a&amp;b&lt;c</t><t>  </t></a>` ) ).
  ENDMETHOD.

  METHOD namespace_prefix.
    " the name without its prefix, on an element and on an attribute
    DATA lv_act TYPE string.
    DATA temp8 TYPE xsdboolean.
    lv_act = read( `<x:a xmlns:x="urn:x" xmlns:r="urn:r"><x:b r:id="rId1"/></x:a>` ).
    
    temp8 = boolc( lv_act CS `+b[id=rId1]-b-a` ).
    cl_abap_unit_assert=>assert_true( temp8 ).
    cl_abap_unit_assert=>assert_equals( exp = `+a`
                                        act = substring( val = lv_act
                                                         len = 2 ) ).
  ENDMETHOD.

  METHOD broken_document.
    TRY.
        read( `<a><b></a>` ).
        cl_abap_unit_assert=>fail( `a document that is not well-formed has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

"! ================================================================
"! XLSX CONVERSION
"! ================================================================
CLASS ltcl_xlsx_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS xlsx_by_itab_basic             FOR TESTING.
    METHODS xlsx_by_itab_elementary        FOR TESTING.
    METHODS itab_by_xlsx_empty             FOR TESTING.

ENDCLASS.

CLASS ltcl_xlsx_ops IMPLEMENTATION.

  METHOD xlsx_by_itab_basic.
    TYPES: BEGIN OF ty_row,
             text   TYPE string,
             num    TYPE i,
             amount TYPE p LENGTH 8 DECIMALS 2,
             date   TYPE d,
             numc   TYPE n LENGTH 4,
             sub    TYPE string_table,
           END OF ty_row.
    TYPES temp30 TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA lt_tab TYPE temp30.
    DATA temp212 LIKE lt_tab.
    DATA temp213 LIKE LINE OF temp212.
    DATA lt_files TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp9 TYPE xsdboolean.
    DATA temp1 LIKE sy-subrc.
    DATA temp2 LIKE sy-subrc.
    DATA temp3 LIKE sy-tabix.
    DATA temp10 TYPE xsdboolean.
    DATA temp4 LIKE sy-subrc.
    DATA temp5 LIKE sy-subrc.
    DATA temp6 LIKE sy-tabix.
    DATA lv_sheet TYPE string.
    FIELD-SYMBOLS <temp24> LIKE LINE OF lt_files.
    DATA temp25 LIKE sy-tabix.
    DATA lv_text TYPE string.
    DATA lv_end TYPE string.
    DATA lv_exp TYPE string.
    CLEAR temp212.
    
    CLEAR temp213.
    temp213-text = `  a&b<c`.
    temp213-num = -5.
    temp213-amount = `-12.5`.
    temp213-date = `20240315`.
    temp213-numc = `0042`.
    INSERT temp213 INTO TABLE temp212.
    CLEAR temp213.
    temp213-text = |x{ cl_abap_char_utilities=>form_feed }y|.
    INSERT temp213 INTO TABLE temp212.
    lt_tab = temp212.

    
    lt_files = zabaputil_cl_util_context=>zip_unpack( zabaputil_cl_util_context=>conv_get_xlsx_by_itab( lt_tab ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( lt_files ) ).
    
    
    
    
    temp2 = sy-subrc.
    temp3 = sy-tabix.
    READ TABLE lt_files WITH KEY name = `[Content_Types].xml` TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.
    sy-subrc = temp2.
    sy-tabix = temp3.
    temp9 = boolc( temp1 = 0 ).
    cl_abap_unit_assert=>assert_true( temp9 ).
    
    
    
    
    temp5 = sy-subrc.
    temp6 = sy-tabix.
    READ TABLE lt_files WITH KEY name = `xl/workbook.xml` TRANSPORTING NO FIELDS.
    temp4 = sy-subrc.
    sy-subrc = temp5.
    sy-tabix = temp6.
    temp10 = boolc( temp4 = 0 ).
    cl_abap_unit_assert=>assert_true( temp10 ).

    
    
    
    temp25 = sy-tabix.
    READ TABLE lt_files WITH KEY name = `xl/worksheets/sheet1.xml` ASSIGNING <temp24>.
    sy-tabix = temp25.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_sheet = zabaputil_cl_util_context=>conv_get_string_by_xstring(
                         <temp24>-content ).
    " a number as value, leading blanks kept, & and < escaped, the form
    " feed XML forbids dropped, a NUMC as text, a date in ISO form, a table
    " column and an initial date empty
    
    lv_text = `<c t="inlineStr"><is><t xml:space="preserve">`.
    
    lv_end = `</t></is></c>`.
    
    lv_exp = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
                && `<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>`
                && |<row>{ lv_text }TEXT{ lv_end }{ lv_text }NUM{ lv_end }{ lv_text }AMOUNT{ lv_end }|
                && |{ lv_text }DATE{ lv_end }{ lv_text }NUMC{ lv_end }{ lv_text }SUB{ lv_end }</row>|
                && |<row>{ lv_text }  a&amp;b&lt;c{ lv_end }<c><v>-5</v></c><c><v>-12.50</v></c>|
                && |{ lv_text }2024-03-15{ lv_end }{ lv_text }0042{ lv_end }<c/></row>|
                && |<row>{ lv_text }xy{ lv_end }<c><v>0</v></c><c><v>0.00</v></c><c/>{ lv_text }0000{ lv_end }<c/></row>|
                && `</sheetData></worksheet>`.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                        act = lv_sheet ).
  ENDMETHOD.

  METHOD xlsx_by_itab_elementary.
    " a table without components is one column
    DATA temp214 TYPE string_table.
    DATA lt_files TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA lv_sheet TYPE string.
    FIELD-SYMBOLS <temp26> LIKE LINE OF lt_files.
    DATA temp27 LIKE sy-tabix.
    DATA temp11 TYPE xsdboolean.
    CLEAR temp214.
    INSERT `a` INTO TABLE temp214.
    
    lt_files = zabaputil_cl_util_context=>zip_unpack(
                         zabaputil_cl_util_context=>conv_get_xlsx_by_itab( temp214 ) ).
    
    
    
    temp27 = sy-tabix.
    READ TABLE lt_files WITH KEY name = `xl/worksheets/sheet1.xml` ASSIGNING <temp26>.
    sy-tabix = temp27.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_sheet = zabaputil_cl_util_context=>conv_get_string_by_xstring(
                         <temp26>-content ).
    
    temp11 = boolc( lv_sheet CS `<row><c t="inlineStr"><is><t xml:space="preserve">a</t></is></c></row>` ).
    cl_abap_unit_assert=>assert_true( temp11 ).
  ENDMETHOD.

  METHOD itab_by_xlsx_empty.
    DATA lv_xstring TYPE xstring.
    DATA lr_result TYPE REF TO data.
    TRY.
        zabaputil_cl_util_context=>conv_get_itab_by_xlsx( EXPORTING val    = lv_xstring
                                                          IMPORTING result = lr_result ) ##NEEDED.
        cl_abap_unit_assert=>fail( `not implemented - must raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
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
    METHODS url_encode_emoji              FOR TESTING.
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

  METHOD url_encode_emoji.
    " a character outside the BMP is two UTF-16 code units - encoded as one
    " UTF-8 sequence, not as two broken halves. Built from its bytes: the
    " source stays 7-bit
    DATA temp216 TYPE xstring.
    DATA lv_emoji TYPE string.
    temp216 = `F09F9880`.
    
    lv_emoji = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp216 ).
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>url_encode( |a{ lv_emoji }b| )
        exp = `a%F0%9F%98%80b` ).
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
    METHODS group_negative_numbers        FOR TESTING.

ENDCLASS.

CLASS ltcl_itab_aggregations IMPLEMENTATION.

  METHOD get_flights.
    DATA temp217 TYPE ltcl_itab_aggregations=>ty_t_flight.
    DATA temp218 LIKE LINE OF temp217.
    CLEAR temp217.
    
    temp218-carrid = `LH`.
    temp218-price = 100.
    INSERT temp218 INTO TABLE temp217.
    temp218-carrid = `LH`.
    temp218-price = 150.
    INSERT temp218 INTO TABLE temp217.
    temp218-carrid = `AA`.
    temp218-price = 200.
    INSERT temp218 INTO TABLE temp217.
    temp218-carrid = `LH`.
    temp218-price = 50.
    INSERT temp218 INTO TABLE temp217.
    result = temp217.
  ENDMETHOD.

  METHOD sum_by.
    DATA temp219 TYPE decfloat34.
    temp219 = 500.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>itab_sum_by( tab       = get_flights( )
                                                      fieldname = `PRICE` )
        exp = temp219 ).
  ENDMETHOD.

  METHOD sum_by_unknown_field.
    DATA temp220 TYPE decfloat34.
    temp220 = 0.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>itab_sum_by( tab       = get_flights( )
                                                      fieldname = `DOES_NOT_EXIST` )
        exp = temp220 ).
  ENDMETHOD.

  METHOD distinct.
    DATA lt_distinct TYPE string_table.
    FIELD-SYMBOLS <temp221> LIKE LINE OF lt_distinct.
    DATA temp222 LIKE sy-tabix.
    FIELD-SYMBOLS <temp223> LIKE LINE OF lt_distinct.
    DATA temp224 LIKE sy-tabix.
    lt_distinct = zabaputil_cl_util_context=>itab_distinct( tab       = get_flights( )
                                                                  fieldname = `CARRID` ).
    cl_abap_unit_assert=>assert_equals(
        act = lines( lt_distinct )
        exp = 2 ).
    
    
    temp222 = sy-tabix.
    READ TABLE lt_distinct INDEX 1 ASSIGNING <temp221>.
    sy-tabix = temp222.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp221>
        exp = `LH` ).
    
    
    temp224 = sy-tabix.
    READ TABLE lt_distinct INDEX 2 ASSIGNING <temp223>.
    sy-tabix = temp224.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp223>
        exp = `AA` ).
  ENDMETHOD.

  METHOD group_negative_numbers.
    " a negative number is written `-5` by distinct, group_sum_by and
    " count_by alike - a plain move wrote `5-`
    TYPES: BEGIN OF ty, grp TYPE i, val TYPE i, END OF ty.
    TYPES temp31 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_tab TYPE temp31.
    DATA temp225 LIKE lt_tab.
    DATA temp226 LIKE LINE OF temp225.
    DATA lt_distinct TYPE string_table.
    DATA temp227 TYPE string_table.
    DATA lt_sums TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp229> LIKE LINE OF lt_sums.
    DATA temp230 LIKE sy-tabix.
    FIELD-SYMBOLS <temp231> LIKE LINE OF lt_sums.
    DATA temp232 LIKE sy-tabix.
    DATA lt_count TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp233> LIKE LINE OF lt_count.
    DATA temp234 LIKE sy-tabix.
    FIELD-SYMBOLS <temp235> LIKE LINE OF lt_count.
    DATA temp236 LIKE sy-tabix.
    FIELD-SYMBOLS <temp237> LIKE LINE OF lt_count.
    DATA temp238 LIKE sy-tabix.
    CLEAR temp225.
    
    temp226-grp = -5.
    temp226-val = 1.
    INSERT temp226 INTO TABLE temp225.
    temp226-grp = 3.
    temp226-val = 2.
    INSERT temp226 INTO TABLE temp225.
    temp226-grp = -5.
    temp226-val = 4.
    INSERT temp226 INTO TABLE temp225.
    lt_tab = temp225.

    
    lt_distinct = zabaputil_cl_util_context=>itab_distinct( tab       = lt_tab
                                                                  fieldname = `GRP` ).
    
    CLEAR temp227.
    INSERT `-5` INTO TABLE temp227.
    INSERT `3` INTO TABLE temp227.
    cl_abap_unit_assert=>assert_equals( exp = temp227
                                        act = lt_distinct ).

    
    lt_sums = zabaputil_cl_util_context=>itab_group_sum_by( tab      = lt_tab
                                                                  group_by = `GRP`
                                                                  sum_by   = `VAL` ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_sums ) ).
    
    
    temp230 = sy-tabix.
    READ TABLE lt_sums INDEX 1 ASSIGNING <temp229>.
    sy-tabix = temp230.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `-5`
                                        act = <temp229>-n ).
    
    
    temp232 = sy-tabix.
    READ TABLE lt_sums INDEX 1 ASSIGNING <temp231>.
    sy-tabix = temp232.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `5`
                                        act = <temp231>-v ).

    
    lt_count = zabaputil_cl_util_context=>itab_count_by( tab       = lt_tab
                                                               fieldname = `GRP` ).
    
    
    temp234 = sy-tabix.
    READ TABLE lt_count INDEX 1 ASSIGNING <temp233>.
    sy-tabix = temp234.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `-5`
                                        act = <temp233>-n ).
    
    
    temp236 = sy-tabix.
    READ TABLE lt_count INDEX 1 ASSIGNING <temp235>.
    sy-tabix = temp236.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `2`
                                        act = <temp235>-v ).
    
    
    temp238 = sy-tabix.
    READ TABLE lt_count INDEX 2 ASSIGNING <temp237>.
    sy-tabix = temp238.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `3`
                                        act = <temp237>-n ).
  ENDMETHOD.

  METHOD group_sum_by.
    DATA lt_sums TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp239> LIKE LINE OF lt_sums.
    DATA temp240 LIKE sy-tabix.
    FIELD-SYMBOLS <temp241> LIKE LINE OF lt_sums.
    DATA temp242 LIKE sy-tabix.
    FIELD-SYMBOLS <temp243> LIKE LINE OF lt_sums.
    DATA temp244 LIKE sy-tabix.
    FIELD-SYMBOLS <temp245> LIKE LINE OF lt_sums.
    DATA temp246 LIKE sy-tabix.
    lt_sums = zabaputil_cl_util_context=>itab_group_sum_by( tab      = get_flights( )
                                                                  group_by = `CARRID`
                                                                  sum_by   = `PRICE` ).
    cl_abap_unit_assert=>assert_equals(
        act = lines( lt_sums )
        exp = 2 ).
    
    
    temp240 = sy-tabix.
    READ TABLE lt_sums INDEX 1 ASSIGNING <temp239>.
    sy-tabix = temp240.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp239>-n
        exp = `LH` ).
    
    
    temp242 = sy-tabix.
    READ TABLE lt_sums INDEX 1 ASSIGNING <temp241>.
    sy-tabix = temp242.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp241>-v
        exp = `300` ).
    
    
    temp244 = sy-tabix.
    READ TABLE lt_sums INDEX 2 ASSIGNING <temp243>.
    sy-tabix = temp244.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp243>-n
        exp = `AA` ).
    
    
    temp246 = sy-tabix.
    READ TABLE lt_sums INDEX 2 ASSIGNING <temp245>.
    sy-tabix = temp246.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = <temp245>-v
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
    DATA temp247 TYPE decfloat34.
    DATA temp28 TYPE decfloat34.
    DATA temp248 TYPE decfloat34.
    DATA temp29 TYPE decfloat34.
    temp247 = '2.5'.
    
    temp28 = 3.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( temp247 )
        exp = temp28 ).
    
    temp248 = '2.4'.
    
    temp29 = 2.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( temp248 )
        exp = temp29 ).
  ENDMETHOD.

  METHOD round_half_up_negative.
    DATA temp249 TYPE decfloat34.
    DATA temp30 TYPE decfloat34.
    temp249 = '-2.5'.
    
    temp30 = -3.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( temp249 )
        exp = temp30 ).
  ENDMETHOD.

  METHOD round_up.
    DATA temp250 TYPE decfloat34.
    DATA temp31 TYPE decfloat34.
    temp250 = '2.1'.
    
    temp31 = 3.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( val  = temp250
                                                    mode = `UP` )
        exp = temp31 ).
  ENDMETHOD.

  METHOD round_down.
    DATA temp251 TYPE decfloat34.
    DATA temp32 TYPE decfloat34.
    temp251 = '2.9'.
    
    temp32 = 2.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( val  = temp251
                                                    mode = `DOWN` )
        exp = temp32 ).
  ENDMETHOD.

  METHOD round_two_decimals.
    DATA temp252 TYPE decfloat34.
    DATA temp33 TYPE decfloat34.
    temp252 = '2.345'.
    
    temp33 = '2.35'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>num_round( val      = temp252
                                                    decimals = 2 )
        exp = temp33 ).
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
    " SAP language keys are case-sensitive - a lower-case key is another
    " language than its upper-case twin (c Catalan vs C Czech), never English
    cl_abap_unit_assert=>assert_differs(
        act = zabaputil_cl_util_context=>lang_sap_to_iso( `e` )
        exp = `en` ).
    cl_abap_unit_assert=>assert_differs(
        act = zabaputil_cl_util_context=>lang_sap_to_iso( `c` )
        exp = `cs` ).
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
    METHODS decimals_not_listed           FOR TESTING.

ENDCLASS.

CLASS ltcl_cur_amounts IMPLEMENTATION.

  METHOD decimals_not_listed.
    " what the system does not list - or no table to read, as in the
    " transpiled runtime: a currency has two, a unit is unknown
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = zabaputil_cl_util_context=>currency_get_decimals( `XYZ` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = zabaputil_cl_util_context=>currency_get_decimals( `` ) ).
    cl_abap_unit_assert=>assert_equals( exp = -1
                                        act = zabaputil_cl_util_context=>unit_get_decimals( `XYZ` ) ).
  ENDMETHOD.

  METHOD to_external_zero_decimals.
    " JPY-style currency: stored 123.45 means 12345
    DATA temp253 TYPE decfloat34.
    DATA temp34 TYPE decfloat34.
    temp253 = '123.45'.
    
    temp34 = 12345.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_external( val      = temp253
                                                                 decimals = 0 )
        exp = temp34 ).
  ENDMETHOD.

  METHOD to_external_three_decimals.
    " BHD-style currency: stored 123.45 means 12.345
    DATA temp254 TYPE decfloat34.
    DATA temp35 TYPE decfloat34.
    temp254 = '123.45'.
    
    temp35 = '12.345'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_external( val      = temp254
                                                                 decimals = 3 )
        exp = temp35 ).
  ENDMETHOD.

  METHOD to_external_two_decimals.
    DATA temp255 TYPE decfloat34.
    DATA temp36 TYPE decfloat34.
    temp255 = '123.45'.
    
    temp36 = '123.45'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_external( val      = temp255
                                                                 decimals = 2 )
        exp = temp36 ).
  ENDMETHOD.

  METHOD to_internal_zero_decimals.
    DATA temp256 TYPE decfloat34.
    DATA temp37 TYPE decfloat34.
    temp256 = 12345.
    
    temp37 = '123.45'.
    cl_abap_unit_assert=>assert_equals(
        act = zabaputil_cl_util_context=>cur_amount_to_internal( val      = temp256
                                                                 decimals = 0 )
        exp = temp37 ).
  ENDMETHOD.

  METHOD roundtrip.
    DATA temp257 TYPE decfloat34.
    DATA lv_val LIKE temp257.
    temp257 = '987.65'.
    
    lv_val = temp257.
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
    METHODS token_negated_roundtrip        FOR TESTING.
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
    DATA temp12 TYPE xsdboolean.
    DATA temp7 LIKE sy-subrc.
    DATA temp258 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp258.
    CREATE OBJECT lx TYPE lcx_sync_test EXPORTING DETAIL = `the_detail`.
    
    lt_attri = zabaputil_cl_util_context=>error_get_attributes( lx ).

    
    
    READ TABLE lt_attri WITH KEY n = `MV_DETAIL` TRANSPORTING NO FIELDS.
    temp7 = sy-subrc.
    temp12 = boolc( temp7 = 0 ).
    cl_abap_unit_assert=>assert_true( temp12 ).
    
    
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
    FIELD-SYMBOLS <temp259> LIKE LINE OF lt_attri.
    DATA temp260 LIKE sy-tabix.
    CREATE OBJECT lx TYPE lcx_sync_test EXPORTING DETAIL = `the_detail`.
    
    lt_attri = zabaputil_cl_util_context=>error_get_attributes( lx ).
    
    
    temp260 = sy-tabix.
    READ TABLE lt_attri WITH KEY n = `MV_DETAIL` ASSIGNING <temp259>.
    sy-tabix = temp260.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp259>-v
                                        exp = `the_detail` ).
  ENDMETHOD.

  METHOD attributes_skip_general.
    DATA lx TYPE REF TO lcx_sync_test.
    DATA lt_attri TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp13 TYPE xsdboolean.
    DATA temp8 LIKE sy-subrc.
    DATA temp14 TYPE xsdboolean.
    DATA temp9 LIKE sy-subrc.
    DATA temp15 TYPE xsdboolean.
    DATA temp10 LIKE sy-subrc.
    DATA temp16 TYPE xsdboolean.
    DATA temp11 LIKE sy-subrc.
    CREATE OBJECT lx TYPE lcx_sync_test EXPORTING DETAIL = `the_detail`.
    
    lt_attri = zabaputil_cl_util_context=>error_get_attributes( lx ).
    " rendered by the caller or without information value
    
    
    READ TABLE lt_attri WITH KEY n = `PREVIOUS` TRANSPORTING NO FIELDS.
    temp8 = sy-subrc.
    temp13 = boolc( temp8 = 0 ).
    cl_abap_unit_assert=>assert_false( temp13 ).
    
    
    READ TABLE lt_attri WITH KEY n = `TEXTID` TRANSPORTING NO FIELDS.
    temp9 = sy-subrc.
    temp14 = boolc( temp9 = 0 ).
    cl_abap_unit_assert=>assert_false( temp14 ).
    " initial and non-printable attributes carry nothing to show
    
    
    READ TABLE lt_attri WITH KEY n = `MV_EMPTY` TRANSPORTING NO FIELDS.
    temp10 = sy-subrc.
    temp15 = boolc( temp10 = 0 ).
    cl_abap_unit_assert=>assert_false( temp15 ).
    
    
    READ TABLE lt_attri WITH KEY n = `MT_TAB` TRANSPORTING NO FIELDS.
    temp11 = sy-subrc.
    temp16 = boolc( temp11 = 0 ).
    cl_abap_unit_assert=>assert_false( temp16 ).
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
    DATA temp261 TYPE string_table.
    DATA lt_tab LIKE temp261.
    FIELD-SYMBOLS <temp263> LIKE LINE OF lt_tab.
    DATA temp264 LIKE sy-tabix.
    CLEAR temp261.
    INSERT `alpha` INTO TABLE temp261.
    INSERT `beta` INTO TABLE temp261.
    INSERT `gamma` INTO TABLE temp261.
    
    lt_tab = temp261.
    zabaputil_cl_util_context=>itab_filter_by_val( EXPORTING val = `et`
                                                   CHANGING  tab = lt_tab ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_tab )
                                        exp = 1 ).
    
    
    temp264 = sy-tabix.
    READ TABLE lt_tab INDEX 1 ASSIGNING <temp263>.
    sy-tabix = temp264.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp263>
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
    FIELD-SYMBOLS <temp265> LIKE LINE OF lt_result.
    DATA temp266 LIKE sy-tabix.
    FIELD-SYMBOLS <temp267> LIKE LINE OF lt_result.
    DATA temp268 LIKE sy-tabix.

    ls_struc-name = `the_name`.
    
    lt_result = zabaputil_cl_util_context=>itab_get_by_struc( ls_struc ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_result )
                                        exp = 1 ).
    
    
    temp266 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp265>.
    sy-tabix = temp266.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp265>-n
                                        exp = `NAME` ).
    
    
    temp268 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp267>.
    sy-tabix = temp268.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp267>-v
                                        exp = `the_name` ).
  ENDMETHOD.

  METHOD token_excluding_is_negated.
    DATA temp269 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp270 LIKE LINE OF temp269.
    DATA lt_range LIKE temp269.
    DATA lt_token TYPE zabaputil_cl_util_context=>ty_t_token.
    FIELD-SYMBOLS <temp271> LIKE LINE OF lt_token.
    DATA temp272 LIKE sy-tabix.
    CLEAR temp269.
    
    temp270-sign = `E`.
    temp270-option = `EQ`.
    temp270-low = `4711`.
    INSERT temp270 INTO TABLE temp269.
    
    lt_range = temp269.
    
    lt_token = zabaputil_cl_util_context=>filter_get_token_t_by_range_t( lt_range ).
    
    
    temp272 = sy-tabix.
    READ TABLE lt_token INDEX 1 ASSIGNING <temp271>.
    sy-tabix = temp272.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp271>-text
                                        exp = `!(=4711)` ).
  ENDMETHOD.

  METHOD token_negated_roundtrip.
    " every negated token comes back as the range it was rendered from (or
    " its equivalent: I NE x is E EQ x)
    DATA temp273 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp274 LIKE LINE OF temp273.
    DATA lt_range LIKE temp273.
    DATA lt_token TYPE zabaputil_cl_util_context=>ty_t_token.
    DATA lt_back TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp275 TYPE zabaputil_cl_util_context=>ty_s_range.
    FIELD-SYMBOLS <temp38> LIKE LINE OF lt_back.
    DATA temp39 LIKE sy-tabix.
    DATA temp276 TYPE zabaputil_cl_util_context=>ty_s_range.
    FIELD-SYMBOLS <temp40> LIKE LINE OF lt_back.
    DATA temp41 LIKE sy-tabix.
    DATA temp277 TYPE zabaputil_cl_util_context=>ty_s_range.
    FIELD-SYMBOLS <temp42> LIKE LINE OF lt_back.
    DATA temp43 LIKE sy-tabix.
    DATA temp278 TYPE zabaputil_cl_util_context=>ty_s_range.
    FIELD-SYMBOLS <temp44> LIKE LINE OF lt_back.
    DATA temp45 LIKE sy-tabix.
    DATA temp279 TYPE zabaputil_cl_util_context=>ty_s_range.
    FIELD-SYMBOLS <temp46> LIKE LINE OF lt_back.
    DATA temp47 LIKE sy-tabix.
    CLEAR temp273.
    
    CLEAR temp274.
    temp274-sign = `E`.
    temp274-option = `EQ`.
    temp274-low = `4711`.
    INSERT temp274 INTO TABLE temp273.
    CLEAR temp274.
    temp274-sign = `I`.
    temp274-option = `NE`.
    temp274-low = `4711`.
    INSERT temp274 INTO TABLE temp273.
    CLEAR temp274.
    temp274-sign = `I`.
    temp274-option = `NB`.
    temp274-low = `1`.
    temp274-high = `9`.
    INSERT temp274 INTO TABLE temp273.
    CLEAR temp274.
    temp274-sign = `I`.
    temp274-option = `NP`.
    temp274-low = `ab*`.
    INSERT temp274 INTO TABLE temp273.
    CLEAR temp274.
    temp274-sign = `E`.
    temp274-option = `NE`.
    temp274-low = `5`.
    INSERT temp274 INTO TABLE temp273.
    
    lt_range = temp273.
    
    lt_token = zabaputil_cl_util_context=>filter_get_token_t_by_range_t( lt_range ).
    
    lt_back = zabaputil_cl_util_context=>filter_get_range_t_by_token_t( lt_token ).

    
    CLEAR temp275.
    temp275-sign = `E`.
    temp275-option = `EQ`.
    temp275-low = `4711`.
    
    
    temp39 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp38>.
    sy-tabix = temp39.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp275
                                        act = <temp38> ).
    
    CLEAR temp276.
    temp276-sign = `E`.
    temp276-option = `EQ`.
    temp276-low = `4711`.
    
    
    temp41 = sy-tabix.
    READ TABLE lt_back INDEX 2 ASSIGNING <temp40>.
    sy-tabix = temp41.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp276
                                        act = <temp40> ).
    
    CLEAR temp277.
    temp277-sign = `E`.
    temp277-option = `BT`.
    temp277-low = `1`.
    temp277-high = `9`.
    
    
    temp43 = sy-tabix.
    READ TABLE lt_back INDEX 3 ASSIGNING <temp42>.
    sy-tabix = temp43.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp277
                                        act = <temp42> ).
    
    CLEAR temp278.
    temp278-sign = `E`.
    temp278-option = `CP`.
    temp278-low = `ab*`.
    
    
    temp45 = sy-tabix.
    READ TABLE lt_back INDEX 4 ASSIGNING <temp44>.
    sy-tabix = temp45.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp278
                                        act = <temp44> ).
    
    CLEAR temp279.
    temp279-sign = `I`.
    temp279-option = `EQ`.
    temp279-low = `5`.
    
    
    temp47 = sy-tabix.
    READ TABLE lt_back INDEX 5 ASSIGNING <temp46>.
    sy-tabix = temp47.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp279
                                        act = <temp46> ).
  ENDMETHOD.

  METHOD token_including_unchanged.
    DATA temp280 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp281 LIKE LINE OF temp280.
    DATA lt_range LIKE temp280.
    DATA lt_token TYPE zabaputil_cl_util_context=>ty_t_token.
    FIELD-SYMBOLS <temp282> LIKE LINE OF lt_token.
    DATA temp283 LIKE sy-tabix.
    CLEAR temp280.
    
    temp281-sign = `I`.
    temp281-option = `EQ`.
    temp281-low = `4711`.
    INSERT temp281 INTO TABLE temp280.
    
    lt_range = temp280.
    
    lt_token = zabaputil_cl_util_context=>filter_get_token_t_by_range_t( lt_range ).
    
    
    temp283 = sy-tabix.
    READ TABLE lt_token INDEX 1 ASSIGNING <temp282>.
    sy-tabix = temp283.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp282>-text
                                        exp = `=4711` ).
  ENDMETHOD.

  METHOD url_lower_case_encoded_equals.
    " RFC 3986 allows lowercase hex digits in percent-encodings, so the
    " packed value decodes %3d the same way as %3D
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp284> LIKE LINE OF lt_param.
    DATA temp285 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?sap-startup-params=name%3dvalue` ).
    
    
    temp285 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `name` ASSIGNING <temp284>.
    sy-tabix = temp285.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp284>-v
                                        exp = `value` ).
  ENDMETHOD.

  METHOD url_startup_params_first.
    " sap-startup-params as the first/only parameter has no leading & to
    " match on - the wrapper used to stay in the parameter name
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp286> LIKE LINE OF lt_param.
    DATA temp287 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?sap-startup-params=name%3Dvalue` ).
    
    
    temp287 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `name` ASSIGNING <temp286>.
    sy-tabix = temp287.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp286>-v
                                        exp = `value` ).
  ENDMETHOD.

  METHOD url_encoded_value_survives.
    " decoding the WHOLE query first tore every legitimately encoded `&`
    " apart: `a=x%26y` became the two parameters a=x and y= , and
    " url_param_create_url wrote the damage back into every generated link.
    " Only the sap-startup-params value is decoded now
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp288> LIKE LINE OF lt_param.
    DATA temp289 LIKE sy-tabix.
    FIELD-SYMBOLS <temp290> LIKE LINE OF lt_param.
    DATA temp291 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?a=x%26y&b=2` ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_param )
                                        exp = 2 ).
    
    
    temp289 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `a` ASSIGNING <temp288>.
    sy-tabix = temp289.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp288>-v
                                        exp = `x%26y` ).
    
    
    temp291 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `b` ASSIGNING <temp290>.
    sy-tabix = temp291.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp290>-v
                                        exp = `2` ).
  ENDMETHOD.

  METHOD url_second_question_is_value.
    " the cut is at the FIRST `?` only - a later one is a legal, unencoded
    " character of a value, and cutting there dropped every parameter
    " before it
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp292> LIKE LINE OF lt_param.
    DATA temp293 LIKE sy-tabix.
    FIELD-SYMBOLS <temp294> LIKE LINE OF lt_param.
    DATA temp295 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab( `?app_start=zcl_x&title=why?` ).
    
    
    temp293 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `app_start` ASSIGNING <temp292>.
    sy-tabix = temp293.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp292>-v
                                        exp = `zcl_x` ).
    
    
    temp295 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `title` ASSIGNING <temp294>.
    sy-tabix = temp295.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp294>-v
                                        exp = `why?` ).
  ENDMETHOD.

  METHOD url_startup_with_siblings.
    " the packed value is unwrapped in place: what stood next to
    " sap-startup-params in the query stays a parameter of its own
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp296> LIKE LINE OF lt_param.
    DATA temp297 LIKE sy-tabix.
    FIELD-SYMBOLS <temp298> LIKE LINE OF lt_param.
    DATA temp299 LIKE sy-tabix.
    FIELD-SYMBOLS <temp300> LIKE LINE OF lt_param.
    DATA temp301 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab(
                         `?sap-ui-tech=x&sap-startup-params=name%3Dvalue%26other%3D2&sap-lang=EN` ).
    
    
    temp297 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `name` ASSIGNING <temp296>.
    sy-tabix = temp297.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp296>-v
                                        exp = `value` ).
    
    
    temp299 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `other` ASSIGNING <temp298>.
    sy-tabix = temp299.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp298>-v
                                        exp = `2` ).
    
    
    temp301 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `sap-lang` ASSIGNING <temp300>.
    sy-tabix = temp301.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp300>-v
                                        exp = `EN` ).
  ENDMETHOD.

  METHOD url_full_url_with_path.
    " a full URL, not just a query string - the path in front of the first
    " `?` is not a parameter
    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    FIELD-SYMBOLS <temp302> LIKE LINE OF lt_param.
    DATA temp303 LIKE sy-tabix.
    lt_param = zabaputil_cl_util_context=>url_param_get_tab(
                         `https://host/sap/bc/z2ui5?app_start=zcl_x&b=2` ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_param )
                                        exp = 2 ).
    
    
    temp303 = sy-tabix.
    READ TABLE lt_param WITH KEY n = `app_start` ASSIGNING <temp302>.
    sy-tabix = temp303.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp302>-v
                                        exp = `zcl_x` ).
  ENDMETHOD.

  METHOD build_rap_row.

    DATA lt_comp TYPE cl_abap_structdescr=>component_table.
    DATA lo_struct TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.

    DATA lo_string TYPE REF TO cl_abap_elemdescr.
    DATA temp304 TYPE abap_componentdescr.
    DATA temp305 TYPE abap_componentdescr.
      DATA temp306 TYPE abap_componentdescr.
      DATA temp48 TYPE abap_component_tab.
      DATA temp49 LIKE LINE OF temp48.
    lo_string = cl_abap_elemdescr=>get_string( ).

    
    CLEAR temp304.
    temp304-name = `%CID`.
    temp304-type = lo_string.
    INSERT temp304 INTO TABLE lt_comp.
    
    CLEAR temp305.
    temp305-name = `%MSG`.
    temp305-type = lo_string.
    INSERT temp305 INTO TABLE lt_comp.
    IF fail >= 0.
      
      CLEAR temp306.
      temp306-name = `%FAIL`.
      
      CLEAR temp48.
      
      temp49-name = `CAUSE`.
      temp49-type = cl_abap_elemdescr=>get_i( ).
      INSERT temp49 INTO TABLE temp48.
      temp306-type = cl_abap_structdescr=>create( temp48 ).
      INSERT temp306 INTO TABLE lt_comp.
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
    FIELD-SYMBOLS <temp307> LIKE LINE OF lt_msg.
    DATA temp308 LIKE sy-tabix.
    FIELD-SYMBOLS <temp50> LIKE LINE OF <temp307>-t_meta.
    DATA temp51 LIKE sy-tabix.
    lr_row = build_rap_row( cid  = `CID_1`
                                  msg  = `the_message`
                                  fail = -1 ).
    
    ASSIGN lr_row->* TO <row>.

    
    lt_msg = zabaputil_cl_util_context=>msg_get_t( <row> ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
    
    
    temp308 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp307>.
    sy-tabix = temp308.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp51 = sy-tabix.
    READ TABLE <temp307>-t_meta WITH KEY n = `cid` ASSIGNING <temp50>.
    sy-tabix = temp51.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp50>-v
                                        exp = `CID_1` ).
  ENDMETHOD.

  METHOD rap_row_fail_carries_meta.
    " the %FAIL branch is reached without the %MSG branch having built the
    " block - it has to build it itself, or the meta silently went missing
    " on exactly the rows that failed
    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
    FIELD-SYMBOLS <temp309> LIKE LINE OF lt_msg.
    DATA temp310 LIKE sy-tabix.
    FIELD-SYMBOLS <temp311> LIKE LINE OF lt_msg.
    DATA temp312 LIKE sy-tabix.
    FIELD-SYMBOLS <temp52> LIKE LINE OF <temp311>-t_meta.
    DATA temp53 LIKE sy-tabix.
    lr_row = build_rap_row( cid  = `CID_2`
                                  msg  = ``
                                  fail = 1 ).
    
    ASSIGN lr_row->* TO <row>.

    
    lt_msg = zabaputil_cl_util_context=>msg_get_t( <row> ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
    
    
    temp310 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp309>.
    sy-tabix = temp310.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp309>-type
                                        exp = `E` ).
    
    
    temp312 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp311>.
    sy-tabix = temp312.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp53 = sy-tabix.
    READ TABLE <temp311>-t_meta WITH KEY n = `cid` ASSIGNING <temp52>.
    sy-tabix = temp53.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = <temp52>-v
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
    DATA temp313 TYPE zabaputil_cl_util_context=>ty_t_msg.
    DATA temp314 LIKE LINE OF temp313.
    DATA lt_msg LIKE temp313.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    CLEAR temp313.
    
    temp314-type = `E`.
    temp314-text = `first`.
    INSERT temp314 INTO TABLE temp313.
    temp314-type = `E`.
    temp314-text = `second`.
    INSERT temp314 INTO TABLE temp313.
    
    lt_msg = temp313.
    
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
    DATA temp17 TYPE xsdboolean.
    DATA temp18 TYPE xsdboolean.
    DATA temp19 TYPE xsdboolean.
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( `<b>bold</b> and <i>italic</i>` ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-details
                                        exp = `<b>bold</b> and <i>italic</i>` ).
    
    temp17 = boolc( ls_box-text CS `<b>` ).
    cl_abap_unit_assert=>assert_false( temp17 ).
    
    temp18 = boolc( ls_box-text CS `bold` ).
    cl_abap_unit_assert=>assert_true( temp18 ).
    
    temp19 = boolc( ls_box-text CS `italic` ).
    cl_abap_unit_assert=>assert_true( temp19 ).
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
      DATA temp315 LIKE LINE OF lt_tab.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    DATA temp20 TYPE xsdboolean.

    DO 120 TIMES.
      lv_no = lv_no + 1.
      
      temp315 = |row{ lv_no }|.
      INSERT temp315 INTO TABLE lt_tab.
    ENDDO.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lt_tab ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `Table with 120 entries` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*20 more entries*` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<li>row100</li>*` ).
    
    temp20 = boolc( ls_box-details CS `<li>row101</li>` ).
    cl_abap_unit_assert=>assert_false( temp20 ).
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
    DATA temp21 TYPE xsdboolean.
    DATA temp22 TYPE xsdboolean.
    CREATE OBJECT lo_obj TYPE ltcl_test_app.
    lo_obj->mv_val = `instance_state`.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lo_obj ).
    cl_abap_unit_assert=>assert_equals( act = ls_box-text
                                        exp = `Object LTCL_TEST_APP` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*<strong>MV_VAL</strong>: instance_state*` ).
    " a constant belongs to the type and a class attribute to nobody -
    " neither is this instance's state
    
    temp21 = boolc( ls_box-details CS `SV_STATUS` ).
    cl_abap_unit_assert=>assert_false( temp21 ).
    
    temp22 = boolc( ls_box-details CS `SV_VAR` ).
    cl_abap_unit_assert=>assert_false( temp22 ).
  ENDMETHOD.

  METHOD exception_renders_its_text.
    " an exception carries its own text, and that text is the whole story -
    " its attributes are the placeholders already substituted into it
    DATA lx TYPE REF TO zabaputil_cx_util_error.
    DATA ls_box TYPE zabaputil_cl_util_context=>ty_s_msg_box.
    DATA temp23 TYPE xsdboolean.
    CREATE OBJECT lx TYPE zabaputil_cx_util_error EXPORTING val = `the_failure`.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( lx ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-text
                                         exp = `Object ZABAPUTIL_CX_UTIL_ERROR*` ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_box-details
                                         exp = `*the_failure*` ).
    
    temp23 = boolc( ls_box-details CS `<strong>MS_ERROR` ).
    cl_abap_unit_assert=>assert_false( temp23 ).
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
    DATA temp24 TYPE xsdboolean.

    ls_val-payload = `<script>alert("x")</script>`.

    
    ls_box = zabaputil_cl_util_context=>ui5_data_box_format( ls_val ).
    
    temp24 = boolc( ls_box-details CS `<script>` ).
    cl_abap_unit_assert=>assert_false( temp24 ).
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
    TYPES temp32 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_tab  TYPE temp32.
    DATA lr_data TYPE REF TO data.

    GET REFERENCE OF lt_tab INTO lr_data.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
  ENDMETHOD.

  METHOD table_sorted_false.
    TYPES temp33 TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
DATA lt_tab  TYPE temp33.
    DATA lr_data TYPE REF TO data.

    GET REFERENCE OF lt_tab INTO lr_data.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
  ENDMETHOD.

  METHOD table_hashed_false.
    TYPES temp34 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA lt_tab  TYPE temp34.
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
    DATA temp316 TYPE xstring.
    DATA lv_ff TYPE string.
    DATA lv_raw TYPE string.
    DATA temp317 TYPE xstring.
    DATA lv_nonchar TYPE string.
    temp316 = `0C`.
    
    lv_ff = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp316 ).
    
    lv_raw = `a&b<c>d"e'f` && zabaputil_cl_util_context=>cv_char_util_newline
                   && zabaputil_cl_util_context=>cv_char_util_horizontal_tab && lv_ff && `g`.
    cl_abap_unit_assert=>assert_equals( exp = `a&amp;b&lt;c&gt;d&quot;e'f&#xA;&#x9;g`
                                        act = zabaputil_cl_util_context=>c_escape_xml( lv_raw ) ).
    cl_abap_unit_assert=>assert_equals( exp = `plain`
                                        act = zabaputil_cl_util_context=>c_escape_xml( `plain` ) ).

    " the non-characters U+FFFE and U+FFFF are dropped like a control
    " character - the XMLView parser refuses the document for one of them
    
    temp317 = `EFBFBE` && `EFBFBF`.
    
    lv_nonchar = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp317 ).
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
    DATA temp25 TYPE xsdboolean.
    lv_a = zabaputil_cl_util_context=>hash_checksum( `<html>shell content A</html>` ).
    
    lv_b = zabaputil_cl_util_context=>hash_checksum( `<html>shell content B</html>` ).
    cl_abap_unit_assert=>assert_not_initial( lv_a ).
    cl_abap_unit_assert=>assert_differs( exp = lv_a
                                         act = lv_b ).
    cl_abap_unit_assert=>assert_equals( exp = lv_a
                                        act = zabaputil_cl_util_context=>hash_checksum( `<html>shell content A</html>` ) ).
    " the byte count leads
    
    temp25 = boolc( lv_a CP `28-*` ).
    cl_abap_unit_assert=>assert_true( temp25 ).

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
    TYPES BEGIN OF ty_s_with_suffix.
    TYPES zip TYPE string.
    INCLUDE TYPE ty_s_flat AS s_flat RENAMING WITH SUFFIX _x.
    TYPES END OF ty_s_with_suffix.
    TYPES BEGIN OF ty_s_with_deep.
    TYPES zip TYPE string.
    INCLUDE TYPE ty_s_nested AS s_nested RENAMING WITH SUFFIX _x.
    TYPES END OF ty_s_with_deep.
    TYPES:
      BEGIN OF ty_s_stamps,
        ts  TYPE timestamp,
        tsl TYPE timestampl,
      END OF ty_s_stamps.

    METHODS test_check_table_standard FOR TESTING RAISING cx_static_check.
    METHODS test_typedescr_by_data    FOR TESTING RAISING cx_static_check.
    METHODS test_flat_components      FOR TESTING RAISING cx_static_check.
    METHODS test_flat_components_none FOR TESTING RAISING cx_static_check.
    METHODS test_flat_components_incl FOR TESTING RAISING cx_static_check.
    METHODS test_flat_components_stamp FOR TESTING RAISING cx_static_check.
    METHODS test_iso_by_stamp         FOR TESTING RAISING cx_static_check.
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
    METHODS test_sy_get               FOR TESTING RAISING cx_static_check.
    METHODS test_class_load_error     FOR TESTING RAISING cx_static_check.
    METHODS test_namespaced           FOR TESTING RAISING cx_static_check.
    METHODS test_int_digits           FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_coverage IMPLEMENTATION.

  METHOD test_check_table_standard.

    DATA lt_std TYPE ty_t_flat.
    DATA lt_sorted TYPE ty_t_sorted.
    DATA ls_flat TYPE ty_s_flat.

    DATA temp318 LIKE REF TO lt_std.
    DATA temp319 LIKE REF TO lt_sorted.
    DATA temp320 LIKE REF TO ls_flat.
    GET REFERENCE OF lt_std INTO temp318.
cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>rtti_check_table_standard( temp318 ) ).
    
    GET REFERENCE OF lt_sorted INTO temp319.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( temp319 ) ).
    
    GET REFERENCE OF ls_flat INTO temp320.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( temp320 ) ).

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
    DATA temp321 LIKE REF TO lt_flat.
DATA lt_comp TYPE zabaputil_cl_util_context=>ty_t_flat_comp.
    FIELD-SYMBOLS <temp322> LIKE LINE OF lt_comp.
    DATA temp323 LIKE sy-tabix.
    FIELD-SYMBOLS <temp324> LIKE LINE OF lt_comp.
    DATA temp325 LIKE sy-tabix.
    FIELD-SYMBOLS <temp326> LIKE LINE OF lt_comp.
    DATA temp327 LIKE sy-tabix.
    FIELD-SYMBOLS <temp328> LIKE LINE OF lt_comp.
    DATA temp329 LIKE sy-tabix.
    FIELD-SYMBOLS <temp330> LIKE LINE OF lt_comp.
    DATA temp331 LIKE sy-tabix.
    FIELD-SYMBOLS <temp332> LIKE LINE OF lt_comp.
    DATA temp333 LIKE sy-tabix.
    GET REFERENCE OF lt_flat INTO temp321.

lt_comp = zabaputil_cl_util_context=>rtti_get_flat_components( temp321 ).

    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( lt_comp ) ).
    
    
    temp323 = sy-tabix.
    READ TABLE lt_comp INDEX 1 ASSIGNING <temp322>.
    sy-tabix = temp323.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `TEXT`
                                        act = <temp322>-name ).
    
    
    temp325 = sy-tabix.
    READ TABLE lt_comp INDEX 1 ASSIGNING <temp324>.
    sy-tabix = temp325.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `S`
                                        act = <temp324>-json_kind ).
    
    
    temp327 = sy-tabix.
    READ TABLE lt_comp INDEX 2 ASSIGNING <temp326>.
    sy-tabix = temp327.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp326>-json_kind ).
    
    
    temp329 = sy-tabix.
    READ TABLE lt_comp INDEX 3 ASSIGNING <temp328>.
    sy-tabix = temp329.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `T`
                                        act = <temp328>-json_kind ).
    
    
    temp331 = sy-tabix.
    READ TABLE lt_comp INDEX 4 ASSIGNING <temp330>.
    sy-tabix = temp331.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B`
                                        act = <temp330>-json_kind ).
    
    
    temp333 = sy-tabix.
    READ TABLE lt_comp INDEX 5 ASSIGNING <temp332>.
    sy-tabix = temp333.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `N`
                                        act = <temp332>-json_kind ).

  ENDMETHOD.

  METHOD test_flat_components_none.

    DATA lt_sorted TYPE ty_t_sorted.
    DATA ls_nested TYPE ty_s_nested.
    DATA lv_text TYPE string.

    " a sorted table, a nested structure, a scalar: none is flat
    DATA temp334 LIKE REF TO lt_sorted.
    DATA temp335 LIKE REF TO ls_nested.
    DATA temp336 LIKE REF TO lv_text.
    GET REFERENCE OF lt_sorted INTO temp334.
cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( temp334 ) ).
    
    GET REFERENCE OF ls_nested INTO temp335.
cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( temp335 ) ).
    
    GET REFERENCE OF lv_text INTO temp336.
cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( temp336 ) ).

  ENDMETHOD.

  METHOD test_attri_by_include.

    DATA ls_with_incl TYPE ty_s_with_incl.
    DATA lv_text TYPE string.

    DATA temp337 TYPE REF TO cl_abap_datadescr.
    DATA lt_comp TYPE abap_component_tab.
    FIELD-SYMBOLS <temp338> LIKE LINE OF lt_comp.
    DATA temp339 LIKE sy-tabix.
    FIELD-SYMBOLS <temp340> LIKE LINE OF lt_comp.
    DATA temp341 LIKE sy-tabix.
        DATA temp342 TYPE REF TO cl_abap_datadescr.
    temp337 ?= cl_abap_typedescr=>describe_by_data( ls_with_incl ).
    
    lt_comp = zabaputil_cl_util_context=>rtti_get_t_attri_by_include(
        temp337 ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_comp ) ).
    
    
    temp339 = sy-tabix.
    READ TABLE lt_comp INDEX 1 ASSIGNING <temp338>.
    sy-tabix = temp339.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NAME`
                                        act = <temp338>-name ).
    
    
    temp341 = sy-tabix.
    READ TABLE lt_comp INDEX 2 ASSIGNING <temp340>.
    sy-tabix = temp341.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ZIP`
                                        act = <temp340>-name ).

    " no structure: the error names it, it is no dump
    TRY.
        
        temp342 ?= cl_abap_typedescr=>describe_by_data( lv_text ).
        zabaputil_cl_util_context=>rtti_get_t_attri_by_include(
            temp342 ).
        cl_abap_unit_assert=>fail( `a non-structure include must raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD test_expand_components.

    DATA ls_with_incl TYPE ty_s_with_incl.

    DATA temp343 TYPE REF TO cl_abap_structdescr.
    DATA lt_raw TYPE abap_component_tab.
    DATA lt_comp TYPE abap_component_tab.
    DATA temp26 TYPE xsdboolean.
    DATA temp12 LIKE sy-subrc.
    temp343 ?= cl_abap_typedescr=>describe_by_data( ls_with_incl ).
    
    lt_raw = temp343->get_components( ).
    
    lt_comp = zabaputil_cl_util_context=>expand_components( lt_raw ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_comp ) ).
    
    
    READ TABLE lt_comp WITH KEY as_include = abap_true TRANSPORTING NO FIELDS.
    temp12 = sy-subrc.
    temp26 = boolc( temp12 = 0 ).
    cl_abap_unit_assert=>assert_false( temp26 ).

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
    DATA temp344 LIKE REF TO lv_empty.
    DATA temp345 LIKE REF TO lv_full.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_unassign_initial( lr_none ) ).
    
    GET REFERENCE OF lv_empty INTO temp344.
cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_unassign_initial( temp344 ) ).
    
    GET REFERENCE OF lv_full INTO temp345.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_unassign_initial( temp345 ) ).

  ENDMETHOD.

  METHOD test_unassign_object.

    DATA lo_probe TYPE REF TO ltcl_attri_probe.
    DATA lr_oref TYPE REF TO data.
    DATA lo_back TYPE REF TO object.
    DATA temp27 TYPE xsdboolean.
    CREATE OBJECT lo_probe.
    
    GET REFERENCE OF lo_probe INTO lr_oref.

    
    lo_back = zabaputil_cl_util_context=>unassign_object( lr_oref ).

    cl_abap_unit_assert=>assert_bound( lo_back ).
    
    temp27 = boolc( lo_back = lo_probe ).
    cl_abap_unit_assert=>assert_true( temp27 ).

  ENDMETHOD.

  METHOD test_unassign_data.

    DATA lv_text TYPE string VALUE `x`.
    DATA lr_text TYPE REF TO data.
    DATA lr_outer TYPE REF TO data.
    DATA lr_back TYPE REF TO data.
    DATA temp28 TYPE xsdboolean.
    GET REFERENCE OF lv_text INTO lr_text.
    
    GET REFERENCE OF lr_text INTO lr_outer.

    
    lr_back = zabaputil_cl_util_context=>unassign_data( lr_outer ).

    
    temp28 = boolc( lr_back = lr_text ).
    cl_abap_unit_assert=>assert_true( temp28 ).

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
    DATA temp29 TYPE xsdboolean.
    lv_first = zabaputil_cl_util_context=>time_get_timestampl( ).
    
    lv_second = zabaputil_cl_util_context=>time_get_timestampl( ).

    cl_abap_unit_assert=>assert_not_initial( lv_first ).
    
    temp29 = boolc( lv_second >= lv_first ).
    cl_abap_unit_assert=>assert_true( temp29 ).

  ENDMETHOD.

  METHOD test_attri_by_oref.

    DATA lo_probe TYPE REF TO ltcl_attri_probe.
    DATA lt_attri TYPE abap_attrdescr_tab.
    DATA temp30 TYPE xsdboolean.
    DATA temp13 LIKE sy-subrc.
    CREATE OBJECT lo_probe.

    
    lt_attri = zabaputil_cl_util_context=>rtti_get_t_attri_by_oref( lo_probe ).

    
    
    READ TABLE lt_attri WITH KEY name = `MV_TEXT` TRANSPORTING NO FIELDS.
    temp13 = sy-subrc.
    temp30 = boolc( temp13 = 0 ).
    cl_abap_unit_assert=>assert_true( temp30 ).

  ENDMETHOD.

  METHOD test_check_abap_cloud.

    " the answer is latched - the second call gives the first one's answer
    DATA lv_first TYPE abap_bool.
    lv_first = zabaputil_cl_util_context=>check_abap_cloud( ).

    cl_abap_unit_assert=>assert_equals( exp = lv_first
                                        act = zabaputil_cl_util_context=>check_abap_cloud( ) ).

  ENDMETHOD.

  METHOD test_flat_components_incl.

    DATA ls_incl   TYPE ty_s_with_incl.
    DATA ls_suffix TYPE ty_s_with_suffix.
    DATA ls_deep   TYPE ty_s_with_deep.

    " the fields of an include in its place, under the names the structure
    " addresses them by - the name of the include itself is none of them
    DATA temp346 LIKE REF TO ls_incl.
DATA lt_comp TYPE zabaputil_cl_util_context=>ty_t_flat_comp.
    DATA temp31 TYPE xsdboolean.
    DATA temp14 LIKE sy-subrc.
    DATA temp15 LIKE sy-subrc.
    DATA temp16 LIKE sy-tabix.
    DATA temp32 TYPE xsdboolean.
    DATA temp17 LIKE sy-subrc.
    DATA temp18 LIKE sy-subrc.
    DATA temp19 LIKE sy-tabix.
    DATA temp347 LIKE REF TO ls_suffix.
    FIELD-SYMBOLS <temp348> LIKE LINE OF lt_comp.
    DATA temp349 LIKE sy-tabix.
    FIELD-SYMBOLS <temp350> LIKE LINE OF lt_comp.
    DATA temp351 LIKE sy-tabix.
    FIELD-SYMBOLS <temp352> LIKE LINE OF lt_comp.
    DATA temp353 LIKE sy-tabix.
    FIELD-SYMBOLS <temp354> LIKE LINE OF lt_comp.
    DATA temp355 LIKE sy-tabix.
    DATA temp33 TYPE xsdboolean.
    DATA temp20 LIKE sy-subrc.
    FIELD-SYMBOLS <cell> TYPE any.
    DATA temp356 LIKE LINE OF lt_comp.
    DATA lr_comp LIKE REF TO temp356.
      DATA temp34 TYPE xsdboolean.
    DATA temp357 LIKE REF TO ls_deep.
    GET REFERENCE OF ls_incl INTO temp346.

lt_comp = zabaputil_cl_util_context=>rtti_get_flat_components( temp346 ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_comp ) ).
    
    
    
    
    temp15 = sy-subrc.
    temp16 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `NAME` TRANSPORTING NO FIELDS.
    temp14 = sy-subrc.
    sy-subrc = temp15.
    sy-tabix = temp16.
    temp31 = boolc( temp14 = 0 ).
    cl_abap_unit_assert=>assert_true( temp31 ).
    
    
    
    
    temp18 = sy-subrc.
    temp19 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `ZIP` TRANSPORTING NO FIELDS.
    temp17 = sy-subrc.
    sy-subrc = temp18.
    sy-tabix = temp19.
    temp32 = boolc( temp17 = 0 ).
    cl_abap_unit_assert=>assert_true( temp32 ).

    
    GET REFERENCE OF ls_suffix INTO temp347.
lt_comp = zabaputil_cl_util_context=>rtti_get_flat_components( temp347 ).
    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = lines( lt_comp ) ).
    
    
    temp349 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `TEXT_X` ASSIGNING <temp348>.
    sy-tabix = temp349.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `S`
                                        act = <temp348>-json_kind ).
    
    
    temp351 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `DATE_X` ASSIGNING <temp350>.
    sy-tabix = temp351.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `D`
                                        act = <temp350>-json_kind ).
    
    
    temp353 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `FLAG_X` ASSIGNING <temp352>.
    sy-tabix = temp353.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B`
                                        act = <temp352>-json_kind ).
    
    
    temp355 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `NUM_X` ASSIGNING <temp354>.
    sy-tabix = temp355.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `N`
                                        act = <temp354>-json_kind ).
    
    
    READ TABLE lt_comp WITH KEY name = `S_FLAT` TRANSPORTING NO FIELDS.
    temp20 = sy-subrc.
    temp33 = boolc( temp20 = 0 ).
    cl_abap_unit_assert=>assert_false( temp33 ).
    " every name is one the row can be read by
    
    
    
    LOOP AT lt_comp REFERENCE INTO lr_comp.
      UNASSIGN <cell>.
      ASSIGN COMPONENT lr_comp->name OF STRUCTURE ls_suffix TO <cell>.
      
      temp34 = boolc( <cell> IS ASSIGNED ).
      cl_abap_unit_assert=>assert_true( act = temp34
                                        msg = lr_comp->name ).
    ENDLOOP.

    " an include that brings a nested structure: not flat
    
    GET REFERENCE OF ls_deep INTO temp357.
cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( temp357 ) ).

  ENDMETHOD.

  METHOD test_flat_components_stamp.

    DATA ls_stamps TYPE ty_s_stamps.

    DATA temp358 LIKE REF TO ls_stamps.
DATA lt_comp TYPE zabaputil_cl_util_context=>ty_t_flat_comp.
    FIELD-SYMBOLS <temp359> LIKE LINE OF lt_comp.
    DATA temp360 LIKE sy-tabix.
    FIELD-SYMBOLS <temp361> LIKE LINE OF lt_comp.
    DATA temp362 LIKE sy-tabix.
    GET REFERENCE OF ls_stamps INTO temp358.

lt_comp = zabaputil_cl_util_context=>rtti_get_flat_components( temp358 ).

    
    
    temp360 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `TS` ASSIGNING <temp359>.
    sy-tabix = temp360.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `U`
                                        act = <temp359>-json_kind ).
    
    
    temp362 = sy-tabix.
    READ TABLE lt_comp WITH KEY name = `TSL` ASSIGNING <temp361>.
    sy-tabix = temp362.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `L`
                                        act = <temp361>-json_kind ).

  ENDMETHOD.

  METHOD test_iso_by_stamp.

    DATA lv_ts  TYPE timestamp.
    DATA lv_tsl TYPE timestampl.

    cl_abap_unit_assert=>assert_equals( exp = `0000-00-00T00:00:00Z`
                                        act = zabaputil_cl_util_context=>time_get_iso_by_stamp( lv_ts ) ).
    cl_abap_unit_assert=>assert_equals( exp = `0000-00-00T00:00:00.0Z`
                                        act = zabaputil_cl_util_context=>time_get_iso_by_stampl( lv_tsl ) ).

    lv_ts  = '20240229230000'.
    lv_tsl = '20240229230000.5'.
    cl_abap_unit_assert=>assert_equals( exp = `2024-02-29T23:00:00Z`
                                        act = zabaputil_cl_util_context=>time_get_iso_by_stamp( lv_ts ) ).
    cl_abap_unit_assert=>assert_equals( exp = `2024-02-29T23:00:00.5Z`
                                        act = zabaputil_cl_util_context=>time_get_iso_by_stampl( lv_tsl ) ).
    " set from a literal, so exact on every runtime - every digit of the
    " fraction, and the trailing zeros of a shorter one dropped
    lv_tsl = '20240229235959.1234567'.
    cl_abap_unit_assert=>assert_equals( exp = `2024-02-29T23:59:59.1234567Z`
                                        act = zabaputil_cl_util_context=>time_get_iso_by_stampl( lv_tsl ) ).
    lv_tsl = '20240229235959.1200000'.
    cl_abap_unit_assert=>assert_equals( exp = `2024-02-29T23:59:59.12Z`
                                        act = zabaputil_cl_util_context=>time_get_iso_by_stampl( lv_tsl ) ).

  ENDMETHOD.

  METHOD test_sy_get.

    " the one reader of the system fields hands out what SYST holds - the
    " user the draft service binds a draft to, and a date and a time the
    " error context can print
    DATA ls_sy TYPE zabaputil_cl_util_context=>ty_s_sy.
    ls_sy = zabaputil_cl_util_context=>sy_get( ).

    cl_abap_unit_assert=>assert_equals( exp = sy-uname
                                        act = ls_sy-uname ).
    cl_abap_unit_assert=>assert_equals( exp = sy-sysid
                                        act = ls_sy-sysid ).
    cl_abap_unit_assert=>assert_not_initial( ls_sy-datum ).
    cl_abap_unit_assert=>assert_not_initial( ls_sy-saprl ).

  ENDMETHOD.

  METHOD test_class_load_error.

    " a class that describes fine and one that does not exist at all have
    " no load error - only a class that exists and fails to load has one
    cl_abap_unit_assert=>assert_not_bound(
        zabaputil_cl_util_context=>rtti_get_class_load_error( `ZABAPUTIL_CL_UTIL_CONTEXT` ) ).
    cl_abap_unit_assert=>assert_not_bound(
        zabaputil_cl_util_context=>rtti_get_class_load_error( `ZCL_THIS_CLASS_DOES_NOT_EXIST` ) ).

  ENDMETHOD.

  METHOD test_namespaced.

    DATA ls_flat   TYPE ty_s_flat.
    DATA lt_flat   TYPE ty_t_flat.
    DATA ls_suffix TYPE ty_s_with_suffix.
    DATA lr_none   TYPE REF TO data.

    " no component name starts with a namespace - not even one of an
    " include, nor one in a table line; an initial reference is no type
    DATA temp363 LIKE REF TO ls_flat.
    DATA temp364 LIKE REF TO lt_flat.
    DATA temp365 LIKE REF TO ls_suffix.
    GET REFERENCE OF ls_flat INTO temp363.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_namespaced( temp363 ) ).
    
    GET REFERENCE OF lt_flat INTO temp364.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_namespaced( temp364 ) ).
    
    GET REFERENCE OF ls_suffix INTO temp365.
cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_namespaced( temp365 ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_namespaced( lr_none ) ).

  ENDMETHOD.

  METHOD test_int_digits.

    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_int_digits( `42` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_int_digits( ` 007 ` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_int_digits( `999999999` ) ).
    " what would round, carry a sign or an exponent, overflow, or is empty
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_int_digits( `1.5` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_int_digits( `-1` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_int_digits( `1e3` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_int_digits( `1 000` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_int_digits( `1234567890` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_int_digits( `  ` ) ).

  ENDMETHOD.

  METHOD test_classes_impl_intf.

    " the repository lookup is the environment's, and this catalog ships no
    " interface with a known implementation - so the test holds what any
    " answer must hold: no row without a class name, and nothing for an
    " interface that does not exist
    DATA lt_classes TYPE zabaputil_cl_util_context=>ty_t_classes.
    DATA temp35 TYPE xsdboolean.
    DATA temp21 LIKE sy-subrc.
    TRY.
        lt_classes = zabaputil_cl_util_context=>rtti_get_classes_impl_intf( `IF_SERIALIZABLE_OBJECT` ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
    
    
    READ TABLE lt_classes WITH KEY classname = `` TRANSPORTING NO FIELDS.
    temp21 = sy-subrc.
    temp35 = boolc( temp21 = 0 ).
    cl_abap_unit_assert=>assert_false( temp35 ).

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
        DATA temp36 TYPE xsdboolean.

    TRY.
        zabaputil_cl_util_context=>rtti_get_ref_type_name( lv_text ).
        cl_abap_unit_assert=>fail( `a string is no typed reference` ).
        
      CATCH zabaputil_cx_util_error INTO lx.
        
        temp36 = boolc( lx->get_text( ) CS `RTTI_NOT_AN_OBJECT_REFERENCE` ).
        cl_abap_unit_assert=>assert_true( temp36 ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_rtti_initial_reference.

    " describe_by_*_ref raise the classic exception REFERENCE_IS_INITIAL for an
    " initial reference, a runtime error no CATCH reaches - the helpers answer
    " empty where every failure means "no", and raise where a result is owed
    DATA lr_data TYPE REF TO data.
    DATA lo_obj  TYPE REF TO object.
        DATA lx_data TYPE REF TO zabaputil_cx_util_error.
        DATA temp37 TYPE xsdboolean.
        DATA lx_obj TYPE REF TO zabaputil_cx_util_error.
        DATA temp38 TYPE xsdboolean.

    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>rtti_check_table_standard( lr_data ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>rtti_get_flat_components( lr_data ) ).

    TRY.
        zabaputil_cl_util_context=>rtti_get_typedescr_by_data_ref( lr_data ).
        cl_abap_unit_assert=>fail( `an initial data reference has no type` ).
        
      CATCH zabaputil_cx_util_error INTO lx_data.
        
        temp37 = boolc( lx_data->get_text( ) CS `RTTI_REFERENCE_IS_INITIAL` ).
        cl_abap_unit_assert=>assert_true( temp37 ).
    ENDTRY.

    TRY.
        zabaputil_cl_util_context=>rtti_get_t_attri_by_oref( lo_obj ).
        cl_abap_unit_assert=>fail( `an initial object reference has no attributes` ).
        
      CATCH zabaputil_cx_util_error INTO lx_obj.
        
        temp38 = boolc( lx_obj->get_text( ) CS `RTTI_REFERENCE_IS_INITIAL` ).
        cl_abap_unit_assert=>assert_true( temp38 ).
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
    DATA temp366 TYPE cl_abap_structdescr=>component_table.
    DATA temp367 LIKE LINE OF temp366.
    DATA temp54 TYPE REF TO cl_abap_datadescr.
    DATA lt_comp LIKE temp366.
    DATA lo_tab TYPE REF TO cl_abap_tabledescr.
    DATA lr_tab TYPE REF TO data.
    DATA temp39 TYPE xsdboolean.
    DATA temp40 TYPE xsdboolean.
    DATA lr_back TYPE REF TO data.
    CLEAR temp366.
    
    temp367-name = `COL1`.
    
    temp54 ?= cl_abap_datadescr=>describe_by_data( lv_col ).
    temp367-type = temp54.
    INSERT temp367 INTO TABLE temp366.
    
    lt_comp = temp366.
    
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
    
    temp39 = boolc( lv_type CS `payload-value` ).
    cl_abap_unit_assert=>assert_false( temp39 ).
    
    temp40 = boolc( lv_data CS `payload-value` ).
    cl_abap_unit_assert=>assert_true( temp40 ).

    
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

    DATA temp368 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp369 LIKE LINE OF temp368.
    DATA temp55 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp56 LIKE LINE OF temp55.
    DATA lv_where TYPE string.
    CLEAR temp368.
    
    temp369-name = `F`.
    
    CLEAR temp55.
    
    temp56-sign = `I`.
    temp56-option = `EQ`.
    temp56-low = `A`.
    INSERT temp56 INTO TABLE temp55.
    temp56-sign = `I`.
    temp56-option = `EQ`.
    temp56-low = `B`.
    INSERT temp56 INTO TABLE temp55.
    temp369-t_range = temp55.
    INSERT temp369 INTO TABLE temp368.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp368 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F = 'A' OR F = 'B' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD excludes_and.

    " a row is kept only if it misses every excluded value
    DATA temp370 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp371 LIKE LINE OF temp370.
    DATA temp57 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp58 LIKE LINE OF temp57.
    DATA lv_where TYPE string.
    CLEAR temp370.
    
    temp371-name = `F`.
    
    CLEAR temp57.
    
    temp58-sign = `E`.
    temp58-option = `EQ`.
    temp58-low = `X`.
    INSERT temp58 INTO TABLE temp57.
    temp58-sign = `E`.
    temp58-option = `EQ`.
    temp58-low = `Y`.
    INSERT temp58 INTO TABLE temp57.
    temp371-t_range = temp57.
    INSERT temp371 INTO TABLE temp370.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp370 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F <> 'X' AND F <> 'Y' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD include_and_exclude.

    DATA temp372 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp373 LIKE LINE OF temp372.
    DATA temp59 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp60 LIKE LINE OF temp59.
    DATA lv_where TYPE string.
    CLEAR temp372.
    
    temp373-name = `F`.
    
    CLEAR temp59.
    
    temp60-sign = `I`.
    temp60-option = `CP`.
    temp60-low = `A*`.
    INSERT temp60 INTO TABLE temp59.
    temp60-sign = `E`.
    temp60-option = `EQ`.
    temp60-low = `AB`.
    INSERT temp60 INTO TABLE temp59.
    temp373-t_range = temp59.
    INSERT temp373 INTO TABLE temp372.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp372 ).

    cl_abap_unit_assert=>assert_equals( exp = `( ( F LIKE 'A%' ESCAPE '#' ) AND F <> 'AB' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD fields_and.

    DATA temp374 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp375 LIKE LINE OF temp374.
    DATA temp61 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp62 LIKE LINE OF temp61.
    DATA temp63 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp64 LIKE LINE OF temp63.
    DATA temp65 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA lv_where TYPE string.
    CLEAR temp374.
    
    temp375-name = `F`.
    
    CLEAR temp61.
    
    temp62-sign = `I`.
    temp62-option = `EQ`.
    temp62-low = `A`.
    INSERT temp62 INTO TABLE temp61.
    temp375-t_range = temp61.
    INSERT temp375 INTO TABLE temp374.
    temp375-name = `G`.
    
    CLEAR temp63.
    
    temp64-sign = `E`.
    temp64-option = `EQ`.
    temp64-low = `B`.
    INSERT temp64 INTO TABLE temp63.
    temp375-t_range = temp63.
    INSERT temp375 INTO TABLE temp374.
    temp375-name = `H`.
    
    CLEAR temp65.
    temp375-t_range = temp65.
    INSERT temp375 INTO TABLE temp374.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp374 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F = 'A' ) AND ( G <> 'B' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD quote_doubled.

    DATA temp376 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp377 LIKE LINE OF temp376.
    DATA temp66 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp67 LIKE LINE OF temp66.
    DATA lv_where TYPE string.
    CLEAR temp376.
    
    temp377-name = `F`.
    
    CLEAR temp66.
    
    temp67-sign = `I`.
    temp67-option = `EQ`.
    temp67-low = `O'Neil`.
    INSERT temp67 INTO TABLE temp66.
    temp377-t_range = temp66.
    INSERT temp377 INTO TABLE temp376.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp376 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F = 'O''Neil' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD pattern_escaped.

    " * and + are the wildcards - a typed _ or % only matches itself, and
    " the CP escape #* is a plain star (no wildcard, nothing to escape in LIKE)
    DATA temp378 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp379 LIKE LINE OF temp378.
    DATA temp68 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp69 LIKE LINE OF temp68.
    DATA lv_where TYPE string.
    CLEAR temp378.
    
    temp379-name = `F`.
    
    CLEAR temp68.
    
    temp69-sign = `I`.
    temp69-option = `CP`.
    temp69-low = `Z_T%#*+`.
    INSERT temp69 INTO TABLE temp68.
    temp379-t_range = temp68.
    INSERT temp379 INTO TABLE temp378.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp378 ).

    cl_abap_unit_assert=>assert_equals( exp = `( F LIKE 'Z#_T#%*_' ESCAPE '#' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD name_not_a_field.

    " a name that is no plain field name never reaches the WHERE clause
    DATA temp380 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp381 LIKE LINE OF temp380.
    DATA temp70 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp71 LIKE LINE OF temp70.
    DATA temp72 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp73 LIKE LINE OF temp72.
    DATA lv_where TYPE string.
    CLEAR temp380.
    
    temp381-name = `F = 'A' OR EXISTS ( SELECT bname FROM usr02 ) OR F`.
    
    CLEAR temp70.
    
    temp71-sign = `I`.
    temp71-option = `EQ`.
    temp71-low = `A`.
    INSERT temp71 INTO TABLE temp70.
    temp381-t_range = temp70.
    INSERT temp381 INTO TABLE temp380.
    temp381-name = `G`.
    
    CLEAR temp72.
    
    temp73-sign = `I`.
    temp73-option = `EQ`.
    temp73-low = `B`.
    INSERT temp73 INTO TABLE temp72.
    temp381-t_range = temp72.
    INSERT temp381 INTO TABLE temp380.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( temp380 ).

    cl_abap_unit_assert=>assert_equals( exp = `( G = 'B' )`
                                        act = lv_where ).

  ENDMETHOD.

  METHOD dfies_where_escaped.

    TYPES:
      BEGIN OF ty_s_row,
        f TYPE string,
      END OF ty_s_row.
    DATA ls_row TYPE ty_s_row.
    DATA temp382 LIKE REF TO ls_row.
DATA temp74 TYPE zabaputil_cl_util_context=>ty_t_dfies.
DATA temp75 LIKE LINE OF temp74.
DATA lv_where TYPE string.

    " a typed quote cannot end the literal, a typed # % _ only matches itself
    ls_row-f = `O'N_1%#`.

    
    GET REFERENCE OF ls_row INTO temp382.

CLEAR temp74.

temp75-fieldname = `F`.
INSERT temp75 INTO TABLE temp74.

lv_where = zabaputil_cl_util_context=>tab_get_where_by_dfies(
        mv_check_tab_field = `F`
        ms_data_row        = temp382
        it_dfies           = temp74 ).

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

    DATA temp383 TYPE xstring.
    temp383 = `1D`.
    result = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp383 ).

  ENDMETHOD.

  METHOD parse_raw_with_gs.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp384> LIKE LINE OF lt_gs1.
    DATA temp385 LIKE sy-tabix.
    FIELD-SYMBOLS <temp386> LIKE LINE OF lt_gs1.
    DATA temp387 LIKE sy-tabix.
    FIELD-SYMBOLS <temp388> LIKE LINE OF lt_gs1.
    DATA temp389 LIKE sy-tabix.
    FIELD-SYMBOLS <temp390> LIKE LINE OF lt_gs1.
    DATA temp391 LIKE sy-tabix.
    DATA temp392 TYPE d.
    FIELD-SYMBOLS <temp76> LIKE LINE OF lt_gs1.
    DATA temp77 LIKE sy-tabix.
    FIELD-SYMBOLS <temp393> LIKE LINE OF lt_gs1.
    DATA temp394 LIKE sy-tabix.
    FIELD-SYMBOLS <temp395> LIKE LINE OF lt_gs1.
    DATA temp396 LIKE sy-tabix.
    FIELD-SYMBOLS <temp397> LIKE LINE OF lt_gs1.
    DATA temp398 LIKE sy-tabix.
    FIELD-SYMBOLS <temp399> LIKE LINE OF lt_gs1.
    DATA temp400 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse(
        val      = |]C10109506000134352172512311010ABC123{ gs( ) }21XYZ|
        ref_date = `20250601` ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_gs1 ) ).
    
    
    temp385 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp384>.
    sy-tabix = temp385.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `01`
                                        act = <temp384>-ai ).
    
    
    temp387 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp386>.
    sy-tabix = temp387.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `GTIN`
                                        act = <temp386>-title ).
    
    
    temp389 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp388>.
    sy-tabix = temp389.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `09506000134352`
                                        act = <temp388>-value ).
    
    
    temp391 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp390>.
    sy-tabix = temp391.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `17`
                                        act = <temp390>-ai ).
    
    temp392 = `20251231`.
    
    
    temp77 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp76>.
    sy-tabix = temp77.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp392
                                        act = <temp76>-date ).
    
    
    temp394 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp393>.
    sy-tabix = temp394.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `10`
                                        act = <temp393>-ai ).
    
    
    temp396 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp395>.
    sy-tabix = temp396.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `10ABC123`
                                        act = <temp395>-value ).
    
    
    temp398 = sy-tabix.
    READ TABLE lt_gs1 INDEX 4 ASSIGNING <temp397>.
    sy-tabix = temp398.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `21`
                                        act = <temp397>-ai ).
    
    
    temp400 = sy-tabix.
    READ TABLE lt_gs1 INDEX 4 ASSIGNING <temp399>.
    sy-tabix = temp400.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `XYZ`
                                        act = <temp399>-value ).

  ENDMETHOD.

  METHOD parse_human_readable.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp401> LIKE LINE OF lt_gs1.
    DATA temp402 LIKE sy-tabix.
    FIELD-SYMBOLS <temp403> LIKE LINE OF lt_gs1.
    DATA temp404 LIKE sy-tabix.
    DATA temp405 TYPE d.
    FIELD-SYMBOLS <temp78> LIKE LINE OF lt_gs1.
    DATA temp79 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse(
        val      = `(01)09506000134352(17)251231(10)ABC123(21)XYZ`
        ref_date = `20250601` ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_gs1 ) ).
    
    
    temp402 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp401>.
    sy-tabix = temp402.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ABC123`
                                        act = <temp401>-value ).
    
    
    temp404 = sy-tabix.
    READ TABLE lt_gs1 INDEX 3 ASSIGNING <temp403>.
    sy-tabix = temp404.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `BATCH/LOT`
                                        act = <temp403>-title ).
    
    temp405 = `20251231`.
    
    
    temp79 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp78>.
    sy-tabix = temp79.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp405
                                        act = <temp78>-date ).

  ENDMETHOD.

  METHOD parse_custom_separator.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp406> LIKE LINE OF lt_gs1.
    DATA temp407 LIKE sy-tabix.
    FIELD-SYMBOLS <temp408> LIKE LINE OF lt_gs1.
    DATA temp409 LIKE sy-tabix.
    FIELD-SYMBOLS <temp410> LIKE LINE OF lt_gs1.
    DATA temp411 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val       = `10LOT42|3712|0109506000134352`
                                                                 separator = `|` ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_gs1 ) ).
    
    
    temp407 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp406>.
    sy-tabix = temp407.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `LOT42`
                                        act = <temp406>-value ).
    
    
    temp409 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp408>.
    sy-tabix = temp409.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `12`
                                        act = <temp408>-value ).
    
    
    temp411 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp410>.
    sy-tabix = temp411.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `COUNT`
                                        act = <temp410>-title ).

  ENDMETHOD.

  METHOD parse_measure_decimals.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp412> LIKE LINE OF lt_gs1.
    DATA temp413 LIKE sy-tabix.
    FIELD-SYMBOLS <temp414> LIKE LINE OF lt_gs1.
    DATA temp415 LIKE sy-tabix.
    FIELD-SYMBOLS <temp416> LIKE LINE OF lt_gs1.
    DATA temp417 LIKE sy-tabix.
    FIELD-SYMBOLS <temp418> LIKE LINE OF lt_gs1.
    DATA temp419 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( `01095060001343523103001250` ).

    
    
    temp413 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp412>.
    sy-tabix = temp413.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `3103`
                                        act = <temp412>-ai ).
    
    
    temp415 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp414>.
    sy-tabix = temp415.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `001250`
                                        act = <temp414>-value ).
    
    
    temp417 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp416>.
    sy-tabix = temp417.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = <temp416>-decimals ).
    
    
    temp419 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp418>.
    sy-tabix = temp419.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NET WEIGHT (kg)`
                                        act = <temp418>-title ).

  ENDMETHOD.

  METHOD parse_sscc_and_gln.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp420> LIKE LINE OF lt_gs1.
    DATA temp421 LIKE sy-tabix.
    FIELD-SYMBOLS <temp422> LIKE LINE OF lt_gs1.
    DATA temp423 LIKE sy-tabix.
    FIELD-SYMBOLS <temp424> LIKE LINE OF lt_gs1.
    DATA temp425 LIKE sy-tabix.
    FIELD-SYMBOLS <temp426> LIKE LINE OF lt_gs1.
    DATA temp427 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( `001061414112345678974109506000000008` ).

    
    
    temp421 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp420>.
    sy-tabix = temp421.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `SSCC`
                                        act = <temp420>-title ).
    
    
    temp423 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp422>.
    sy-tabix = temp423.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `106141411234567897`
                                        act = <temp422>-value ).
    
    
    temp425 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp424>.
    sy-tabix = temp425.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `410`
                                        act = <temp424>-ai ).
    
    
    temp427 = sy-tabix.
    READ TABLE lt_gs1 INDEX 2 ASSIGNING <temp426>.
    sy-tabix = temp427.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `9506000000008`
                                        act = <temp426>-value ).

  ENDMETHOD.

  METHOD parse_date_day_zero.

    " day 00 means the last day of the month - February of a leap year
    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    DATA temp428 TYPE d.
    FIELD-SYMBOLS <temp80> LIKE LINE OF lt_gs1.
    DATA temp81 LIKE sy-tabix.
    DATA temp429 TYPE d.
    FIELD-SYMBOLS <temp82> LIKE LINE OF lt_gs1.
    DATA temp83 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `15240200`
                                                                 ref_date = `20250101` ).

    
    temp428 = `20240229`.
    
    
    temp81 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp80>.
    sy-tabix = temp81.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp428
                                        act = <temp80>-date ).

    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `17251200`
                                                          ref_date = `20250101` ).
    
    temp429 = `20251231`.
    
    
    temp83 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp82>.
    sy-tabix = temp83.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp429
                                        act = <temp82>-date ).

  ENDMETHOD.

  METHOD parse_date_century.

    " sliding window: up to 50 years ahead stays in this century, further
    " ahead belongs to the past one
    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    DATA temp430 TYPE d.
    FIELD-SYMBOLS <temp84> LIKE LINE OF lt_gs1.
    DATA temp85 LIKE sy-tabix.
    DATA temp431 TYPE d.
    FIELD-SYMBOLS <temp86> LIKE LINE OF lt_gs1.
    DATA temp87 LIKE sy-tabix.
    DATA temp432 TYPE d.
    FIELD-SYMBOLS <temp88> LIKE LINE OF lt_gs1.
    DATA temp89 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `11750101`
                                                                 ref_date = `20250101` ).
    
    temp430 = `20750101`.
    
    
    temp85 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp84>.
    sy-tabix = temp85.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp430
                                        act = <temp84>-date ).

    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `11760101`
                                                          ref_date = `20250101` ).
    
    temp431 = `19760101`.
    
    
    temp87 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp86>.
    sy-tabix = temp87.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp431
                                        act = <temp86>-date ).

    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val      = `11020101`
                                                          ref_date = `20980101` ).
    
    temp432 = `21020101`.
    
    
    temp89 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp88>.
    sy-tabix = temp89.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = temp432
                                        act = <temp88>-date ).

  ENDMETHOD.

  METHOD parse_trailing_cr_lf.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp433> LIKE LINE OF lt_gs1.
    DATA temp434 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse(
        |10ABC{ zabaputil_cl_util_context=>cv_char_util_cr_lf }| ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_gs1 ) ).
    
    
    temp434 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp433>.
    sy-tabix = temp434.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `ABC`
                                        act = <temp433>-value ).

  ENDMETHOD.

  METHOD parse_check_digit_off.

    DATA lt_gs1 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    FIELD-SYMBOLS <temp435> LIKE LINE OF lt_gs1.
    DATA temp436 LIKE sy-tabix.
    lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val         = `0109506000134353`
                                                                 check_digit = abap_false ).

    
    
    temp436 = sy-tabix.
    READ TABLE lt_gs1 INDEX 1 ASSIGNING <temp435>.
    sy-tabix = temp436.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `09506000134353`
                                        act = <temp435>-value ).

  ENDMETHOD.

  METHOD parse_errors.

    DATA lt_bad TYPE string_table.

    DATA temp437 TYPE string_table.
    DATA lv_bad LIKE LINE OF lt_bad.
    CLEAR temp437.
    INSERT `0409506000134352` INTO TABLE temp437.
    INSERT `010950600013435` INTO TABLE temp437.
    INSERT `0109506000134353` INTO TABLE temp437.
    INSERT `01095060001343AB` INTO TABLE temp437.
    INSERT `17251301` INTO TABLE temp437.
    INSERT `17250230` INTO TABLE temp437.
    INSERT `10ABCDEFGHIJKLMNOPQRSTU` INTO TABLE temp437.
    INSERT `(10)` INTO TABLE temp437.
    INSERT `(99999)ABC` INTO TABLE temp437.
    lt_bad = temp437.                   " unknown AI, readable form

    
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

    DATA temp439 TYPE zabaputil_cl_util_context=>ty_t_gs1.
    DATA temp440 LIKE LINE OF temp439.
    DATA lt_gs1 LIKE temp439.
    CLEAR temp439.
    
    temp440-ai = `01`.
    temp440-value = `09506000134352`.
    INSERT temp440 INTO TABLE temp439.
    temp440-ai = `10`.
    temp440-value = `ABC`.
    INSERT temp440 INTO TABLE temp439.
    
    lt_gs1 = temp439.

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

CLASS ltcl_barcode_split DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-METHODS gs
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS to_text
      IMPORTING
        t_value       TYPE zabaputil_cl_util_context=>ty_t_barcode_value
      RETURNING
        VALUE(result) TYPE string.

    METHODS separator_in_order      FOR TESTING.
    METHODS separator_optional_tail FOR TESTING.
    METHODS separator_trailing_crlf FOR TESTING.
    METHODS separator_errors        FOR TESTING.
    METHODS position_fields         FOR TESTING.
    METHODS position_to_the_end     FOR TESTING.
    METHODS position_padding        FOR TESTING.
    METHODS position_errors         FOR TESTING.
    METHODS prefix_single_field     FOR TESTING.
    METHODS prefix_several_with_gs  FOR TESTING.
    METHODS prefix_own_separator    FOR TESTING.
    METHODS prefix_longest_wins     FOR TESTING.
    METHODS prefix_result_order     FOR TESTING.
    METHODS prefix_errors           FOR TESTING.

ENDCLASS.


CLASS ltcl_barcode_split IMPLEMENTATION.

  METHOD gs.

    DATA temp441 TYPE xstring.
    temp441 = `1D`.
    result = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp441 ).

  ENDMETHOD.

  METHOD to_text.

    DATA ls_value LIKE LINE OF t_value.
    LOOP AT t_value INTO ls_value.
      result = |{ result }{ ls_value-name }={ ls_value-value };|.
    ENDLOOP.

  ENDMETHOD.

  METHOD separator_in_order.

    DATA temp442 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp443 LIKE LINE OF temp442.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp442.
    
    temp443-name = `MATERIAL`.
    INSERT temp443 INTO TABLE temp442.
    temp443-name = `BATCH`.
    INSERT temp443 INTO TABLE temp442.
    temp443-name = `QUANTITY`.
    INSERT temp443 INTO TABLE temp442.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_separator(
                         val       = `4711|LOT42|12`
                         separator = `|`
                         t_field   = temp442 ).

    cl_abap_unit_assert=>assert_equals( exp = `MATERIAL=4711;BATCH=LOT42;QUANTITY=12;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD separator_optional_tail.

    " an optional field may be missing at the end or empty in the middle
    DATA temp444 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp445 LIKE LINE OF temp444.
    DATA lt_field LIKE temp444.
    CLEAR temp444.
    
    CLEAR temp445.
    temp445-name = `MATERIAL`.
    INSERT temp445 INTO TABLE temp444.
    CLEAR temp445.
    temp445-name = `BATCH`.
    temp445-optional = abap_true.
    INSERT temp445 INTO TABLE temp444.
    CLEAR temp445.
    temp445-name = `QUANTITY`.
    temp445-optional = abap_true.
    INSERT temp445 INTO TABLE temp444.
    
    lt_field = temp444.

    cl_abap_unit_assert=>assert_equals(
        exp = `MATERIAL=4711;`
        act = to_text( zabaputil_cl_util_context=>barcode_split_separator( val       = `4711`
                                                                           separator = `;`
                                                                           t_field   = lt_field ) ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `MATERIAL=4711;QUANTITY=12;`
        act = to_text( zabaputil_cl_util_context=>barcode_split_separator( val       = `4711;;12`
                                                                           separator = `;`
                                                                           t_field   = lt_field ) ) ).

  ENDMETHOD.

  METHOD separator_trailing_crlf.

    DATA temp446 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp447 LIKE LINE OF temp446.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp446.
    
    temp447-name = `MATERIAL`.
    INSERT temp447 INTO TABLE temp446.
    temp447-name = `BATCH`.
    INSERT temp447 INTO TABLE temp446.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_separator(
                         val       = |4711-LOT42{ zabaputil_cl_util_context=>cv_char_util_cr_lf }|
                         separator = `-`
                         t_field   = temp446 ).

    cl_abap_unit_assert=>assert_equals( exp = `MATERIAL=4711;BATCH=LOT42;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD separator_errors.

    DATA temp448 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp449 LIKE LINE OF temp448.
    DATA lt_field LIKE temp448.
    DATA lt_bad TYPE string_table.
    DATA temp450 TYPE string_table.
    DATA lv_bad LIKE LINE OF lt_bad.
    CLEAR temp448.
    
    CLEAR temp449.
    temp449-name = `MATERIAL`.
    temp449-len = 6.
    INSERT temp449 INTO TABLE temp448.
    CLEAR temp449.
    temp449-name = `BATCH`.
    INSERT temp449 INTO TABLE temp448.
    
    lt_field = temp448.
    

    
    CLEAR temp450.
    INSERT `4711|LOT42|12` INTO TABLE temp450.
    INSERT `4711` INTO TABLE temp450.
    INSERT `4711|` INTO TABLE temp450.
    INSERT `1234567|LOT42` INTO TABLE temp450.
    lt_bad = temp450.                " material longer than 6

    
    LOOP AT lt_bad INTO lv_bad.
      TRY.
          zabaputil_cl_util_context=>barcode_split_separator( val       = lv_bad
                                                              separator = `|`
                                                              t_field   = lt_field ).
          cl_abap_unit_assert=>fail( |no exception for { lv_bad }| ).
        CATCH zabaputil_cx_util_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    TRY.
        zabaputil_cl_util_context=>barcode_split_separator( val       = `4711|LOT42`
                                                            separator = ``
                                                            t_field   = lt_field ).
        cl_abap_unit_assert=>fail( `no exception without a separator` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD position_fields.

    DATA temp452 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp453 LIKE LINE OF temp452.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp452.
    
    temp453-name = `MATERIAL`.
    temp453-pos = 1.
    temp453-len = 7.
    INSERT temp453 INTO TABLE temp452.
    temp453-name = `BATCH`.
    temp453-pos = 8.
    temp453-len = 6.
    INSERT temp453 INTO TABLE temp452.
    temp453-name = `QUANTITY`.
    temp453-pos = 14.
    temp453-len = 3.
    INSERT temp453 INTO TABLE temp452.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_position(
                         val     = `0004711LOT42A012`
                         t_field = temp452 ).

    cl_abap_unit_assert=>assert_equals( exp = `MATERIAL=0004711;BATCH=LOT42A;QUANTITY=012;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD position_to_the_end.

    " len 0 takes the rest, and a field may end before its len does
    DATA temp454 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp455 LIKE LINE OF temp454.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp454.
    
    CLEAR temp455.
    temp455-name = `TYPE`.
    temp455-pos = 1.
    temp455-len = 2.
    INSERT temp455 INTO TABLE temp454.
    CLEAR temp455.
    temp455-name = `NUMBER`.
    temp455-pos = 3.
    INSERT temp455 INTO TABLE temp454.
    CLEAR temp455.
    temp455-name = `TAIL`.
    temp455-pos = 6.
    temp455-len = 10.
    INSERT temp455 INTO TABLE temp454.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_position(
                         val     = `AB12345`
                         t_field = temp454 ).

    cl_abap_unit_assert=>assert_equals( exp = `TYPE=AB;NUMBER=12345;TAIL=45;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD position_padding.

    DATA temp456 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp457 LIKE LINE OF temp456.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp456.
    
    CLEAR temp457.
    temp457-name = `MATERIAL`.
    temp457-pos = 1.
    temp457-len = 10.
    INSERT temp457 INTO TABLE temp456.
    CLEAR temp457.
    temp457-name = `BATCH`.
    temp457-pos = 11.
    temp457-len = 6.
    INSERT temp457 INTO TABLE temp456.
    CLEAR temp457.
    temp457-name = `NOTE`.
    temp457-pos = 17.
    temp457-len = 4.
    temp457-optional = abap_true.
    INSERT temp457 INTO TABLE temp456.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_position(
                         val     = `4711      LOT42 `
                         t_field = temp456 ).

    cl_abap_unit_assert=>assert_equals( exp = `MATERIAL=4711;BATCH=LOT42;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD position_errors.

    DATA lt_field TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
        DATA temp458 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
        DATA temp459 LIKE LINE OF temp458.
        DATA temp460 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
        DATA temp461 LIKE LINE OF temp460.

    " a field beyond the end of the barcode
    TRY.
        
        CLEAR temp458.
        
        temp459-name = `MATERIAL`.
        temp459-pos = 1.
        temp459-len = 4.
        INSERT temp459 INTO TABLE temp458.
        temp459-name = `BATCH`.
        temp459-pos = 5.
        temp459-len = 4.
        INSERT temp459 INTO TABLE temp458.
        lt_field = temp458.
        zabaputil_cl_util_context=>barcode_split_position( val     = `4711`
                                                           t_field = lt_field ).
        cl_abap_unit_assert=>fail( `no exception for a field beyond the end` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

    " a field that holds blanks only
    TRY.
        zabaputil_cl_util_context=>barcode_split_position( val     = `4711    `
                                                           t_field = lt_field ).
        cl_abap_unit_assert=>fail( `no exception for an empty field` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

    " a field without a position
    TRY.
        
        CLEAR temp460.
        
        temp461-name = `MATERIAL`.
        temp461-len = 4.
        INSERT temp461 INTO TABLE temp460.
        lt_field = temp460.
        zabaputil_cl_util_context=>barcode_split_position( val     = `4711`
                                                           t_field = lt_field ).
        cl_abap_unit_assert=>fail( `no exception for a field without pos` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD prefix_single_field.

    " one scan, one field - the prefix says which one
    DATA temp462 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp463 LIKE LINE OF temp462.
    DATA lt_field LIKE temp462.
    CLEAR temp462.
    
    temp463-name = `MATERIAL`.
    temp463-prefix = `P`.
    temp463-optional = abap_true.
    INSERT temp463 INTO TABLE temp462.
    temp463-name = `QUANTITY`.
    temp463-prefix = `Q`.
    temp463-optional = abap_true.
    INSERT temp463 INTO TABLE temp462.
    
    lt_field = temp462.

    cl_abap_unit_assert=>assert_equals(
        exp = `QUANTITY=12;`
        act = to_text( zabaputil_cl_util_context=>barcode_split_prefix( val     = `Q12`
                                                                        t_field = lt_field ) ) ).

  ENDMETHOD.

  METHOD prefix_several_with_gs.

    DATA temp464 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp465 LIKE LINE OF temp464.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp464.
    
    temp465-name = `MATERIAL`.
    temp465-prefix = `P`.
    INSERT temp465 INTO TABLE temp464.
    temp465-name = `QUANTITY`.
    temp465-prefix = `Q`.
    INSERT temp465 INTO TABLE temp464.
    temp465-name = `BATCH`.
    temp465-prefix = `1T`.
    INSERT temp465 INTO TABLE temp464.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_prefix(
                         val     = |P4711{ gs( ) }Q12{ gs( ) }1TLOT42|
                         t_field = temp464 ).

    cl_abap_unit_assert=>assert_equals( exp = `MATERIAL=4711;QUANTITY=12;BATCH=LOT42;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD prefix_own_separator.

    DATA temp466 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp467 LIKE LINE OF temp466.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp466.
    
    temp467-name = `MATERIAL`.
    temp467-prefix = `P`.
    INSERT temp467 INTO TABLE temp466.
    temp467-name = `QUANTITY`.
    temp467-prefix = `Q`.
    INSERT temp467 INTO TABLE temp466.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_prefix(
                         val       = `P4711~Q12`
                         separator = `~`
                         t_field   = temp466 ).

    cl_abap_unit_assert=>assert_equals( exp = `MATERIAL=4711;QUANTITY=12;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD prefix_longest_wins.

    " `1T` and `1` both fit `1TLOT42` - the longer prefix is the field
    DATA temp468 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp469 LIKE LINE OF temp468.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp468.
    
    temp469-name = `SUPPLIER`.
    temp469-prefix = `1`.
    temp469-optional = abap_true.
    INSERT temp469 INTO TABLE temp468.
    temp469-name = `BATCH`.
    temp469-prefix = `1T`.
    temp469-optional = abap_true.
    INSERT temp469 INTO TABLE temp468.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_prefix(
                         val     = `1TLOT42`
                         t_field = temp468 ).

    cl_abap_unit_assert=>assert_equals( exp = `BATCH=LOT42;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD prefix_result_order.

    " the result follows t_field, whatever order the label has
    DATA temp470 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp471 LIKE LINE OF temp470.
    DATA lt_value TYPE zabaputil_cl_util_context=>ty_t_barcode_value.
    CLEAR temp470.
    
    temp471-name = `MATERIAL`.
    temp471-prefix = `P`.
    INSERT temp471 INTO TABLE temp470.
    temp471-name = `QUANTITY`.
    temp471-prefix = `Q`.
    INSERT temp471 INTO TABLE temp470.
    
    lt_value = zabaputil_cl_util_context=>barcode_split_prefix(
                         val       = `Q12|P4711`
                         separator = `|`
                         t_field   = temp470 ).

    cl_abap_unit_assert=>assert_equals( exp = `MATERIAL=4711;QUANTITY=12;`
                                        act = to_text( lt_value ) ).

  ENDMETHOD.

  METHOD prefix_errors.

    DATA temp472 TYPE zabaputil_cl_util_context=>ty_t_barcode_field.
    DATA temp473 LIKE LINE OF temp472.
    DATA lt_field LIKE temp472.
    DATA lt_bad TYPE string_table.
    DATA temp474 TYPE string_table.
    DATA lv_bad LIKE LINE OF lt_bad.
    CLEAR temp472.
    
    CLEAR temp473.
    temp473-name = `MATERIAL`.
    temp473-prefix = `P`.
    temp473-len = 6.
    INSERT temp473 INTO TABLE temp472.
    CLEAR temp473.
    temp473-name = `QUANTITY`.
    temp473-prefix = `Q`.
    temp473-optional = abap_true.
    INSERT temp473 INTO TABLE temp472.
    
    lt_field = temp472.
    

    
    CLEAR temp474.
    INSERT `X4711` INTO TABLE temp474.
    INSERT `Q12` INTO TABLE temp474.
    INSERT `P` INTO TABLE temp474.
    INSERT `P4711|P4712` INTO TABLE temp474.
    INSERT `P1234567` INTO TABLE temp474.
    lt_bad = temp474.                     " material longer than 6

    
    LOOP AT lt_bad INTO lv_bad.
      TRY.
          zabaputil_cl_util_context=>barcode_split_prefix( val       = lv_bad
                                                           separator = `|`
                                                           t_field   = lt_field ).
          cl_abap_unit_assert=>fail( |no exception for { lv_bad }| ).
        CATCH zabaputil_cx_util_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

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
    METHODS longtext_lines_blank_cut  FOR TESTING.
    METHODS parallel_sequential       FOR TESTING.

ENDCLASS.


CLASS ltcl_system_api IMPLEMENTATION.

  METHOD bintab_roundtrip.

    TYPES ty_x4 TYPE x LENGTH 4.
    TYPES temp35 TYPE STANDARD TABLE OF ty_x4 WITH DEFAULT KEY.
DATA lt_bin TYPE temp35.

    DATA temp476 TYPE xstring.
    DATA lv_x LIKE temp476.
    DATA temp477 TYPE xstring.
    temp476 = `00112233445566778899`.
    
    lv_x = temp476.
    zabaputil_cl_util_context=>conv_get_bintab_by_xstring( EXPORTING val    = lv_x
                                                           IMPORTING result = lt_bin ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_bin ) ).

    " the last line is padded - the length cuts the padding off again
    cl_abap_unit_assert=>assert_equals( exp = lv_x
                                        act = zabaputil_cl_util_context=>conv_get_xstring_by_bintab( val    = lt_bin
                                                                                                     length = 10 ) ).
    
    temp477 = `001122334455667788990000`.
    cl_abap_unit_assert=>assert_equals( exp = temp477
                                        act = zabaputil_cl_util_context=>conv_get_xstring_by_bintab( lt_bin ) ).

  ENDMETHOD.

  METHOD longtext_lines_split.

    DATA lv_long TYPE string.
    DATA lt_line TYPE zabaputil_cl_util_context=>ty_t_longtext_line.
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
    FIELD-SYMBOLS <temp488> LIKE LINE OF lt_line.
    DATA temp489 LIKE sy-tabix.
    FIELD-SYMBOLS <temp490> LIKE LINE OF lt_line.
    DATA temp491 LIKE sy-tabix.
    FIELD-SYMBOLS <temp492> LIKE LINE OF lt_line.
    DATA temp493 LIKE sy-tabix.
    FIELD-SYMBOLS <temp494> LIKE LINE OF lt_line.
    DATA temp495 LIKE sy-tabix.
    lv_long = repeat( val = `abcdefghij`
                            occ = 30 ).

    
    lt_line = zabaputil_cl_util_context=>longtext_to_lines(
        |first{ zabaputil_cl_util_context=>cv_char_util_cr_lf }{ zabaputil_cl_util_context=>cv_char_util_newline }{ lv_long }| ).

    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( lt_line ) ).
    
    
    temp479 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp478>.
    sy-tabix = temp479.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*`
                                        act = <temp478>-format ).
    
    
    temp481 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp480>.
    sy-tabix = temp481.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `first`
                                        act = <temp480>-line ).
    " the empty line stays an empty paragraph
    
    
    temp483 = sy-tabix.
    READ TABLE lt_line INDEX 2 ASSIGNING <temp482>.
    sy-tabix = temp483.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*`
                                        act = <temp482>-format ).
    
    
    temp485 = sy-tabix.
    READ TABLE lt_line INDEX 2 ASSIGNING <temp484>.
    sy-tabix = temp485.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp484>-line ).
    " 300 characters: 132 + 132 + 36, the continuation lines with `=`
    
    
    temp487 = sy-tabix.
    READ TABLE lt_line INDEX 3 ASSIGNING <temp486>.
    sy-tabix = temp487.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*`
                                        act = <temp486>-format ).
    
    
    temp489 = sy-tabix.
    READ TABLE lt_line INDEX 3 ASSIGNING <temp488>.
    sy-tabix = temp489.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 132
                                        act = strlen( <temp488>-line ) ).
    
    
    temp491 = sy-tabix.
    READ TABLE lt_line INDEX 4 ASSIGNING <temp490>.
    sy-tabix = temp491.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `=`
                                        act = <temp490>-format ).
    
    
    temp493 = sy-tabix.
    READ TABLE lt_line INDEX 5 ASSIGNING <temp492>.
    sy-tabix = temp493.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `=`
                                        act = <temp492>-format ).
    
    
    temp495 = sy-tabix.
    READ TABLE lt_line INDEX 5 ASSIGNING <temp494>.
    sy-tabix = temp495.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 36
                                        act = strlen( <temp494>-line ) ).

  ENDMETHOD.

  METHOD longtext_lines_join.

    DATA temp496 TYPE zabaputil_cl_util_context=>ty_t_longtext_line.
    DATA temp497 LIKE LINE OF temp496.
    DATA lt_line LIKE temp496.
    CLEAR temp496.
    
    temp497-format = `*`.
    temp497-line = `Dear`.
    INSERT temp497 INTO TABLE temp496.
    temp497-format = ``.
    temp497-line = `customer,`.
    INSERT temp497 INTO TABLE temp496.
    temp497-format = `/`.
    temp497-line = `thank`.
    INSERT temp497 INTO TABLE temp496.
    temp497-format = `=`.
    temp497-line = `s.`.
    INSERT temp497 INTO TABLE temp496.
    
    lt_line = temp496.

    cl_abap_unit_assert=>assert_equals(
        exp = |Dear customer,{ zabaputil_cl_util_context=>cv_char_util_newline }thanks.|
        act = zabaputil_cl_util_context=>longtext_from_lines( lt_line ) ).

  ENDMETHOD.

  METHOD longtext_lines_blank_cut.

    " a blank exactly at the 132-character boundary
    DATA lv_text TYPE string.
    DATA lt_line TYPE zabaputil_cl_util_context=>ty_t_longtext_line.
    FIELD-SYMBOLS <temp498> LIKE LINE OF lt_line.
    DATA temp499 LIKE sy-tabix.
    FIELD-SYMBOLS <temp500> LIKE LINE OF lt_line.
    DATA temp501 LIKE sy-tabix.
    DATA lv_tdline TYPE c LENGTH 132.
    DATA temp502 LIKE LINE OF lt_line.
    DATA lr_line LIKE REF TO temp502.
    lv_text = repeat( val = `a`
                            occ = 131 ) && ` next word`.
    
    lt_line = zabaputil_cl_util_context=>longtext_to_lines( lv_text ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_line ) ).
    " no chunk ends in a blank - TDLINE (CHAR132) would drop it
    
    
    temp499 = sy-tabix.
    READ TABLE lt_line INDEX 1 ASSIGNING <temp498>.
    sy-tabix = temp499.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 131
                                        act = strlen( <temp498>-line ) ).
    
    
    temp501 = sy-tabix.
    READ TABLE lt_line INDEX 2 ASSIGNING <temp500>.
    sy-tabix = temp501.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = ` next word`
                                        act = <temp500>-line ).

    " as stored: every line through CHAR132 and back
    
    
    
    LOOP AT lt_line REFERENCE INTO lr_line.
      lv_tdline = lr_line->line.
      lr_line->line = lv_tdline.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( exp = lv_text
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
    DATA temp503 TYPE zabaputil_cl_util_context=>ty_t_object.
    DATA lt_result TYPE zabaputil_cl_util_context=>ty_t_parallel.
    FIELD-SYMBOLS <temp505> LIKE LINE OF lt_result.
    DATA temp506 LIKE sy-tabix.
    FIELD-SYMBOLS <temp507> LIKE LINE OF lt_result.
    DATA temp508 LIKE sy-tabix.
    FIELD-SYMBOLS <temp509> LIKE LINE OF lt_result.
    DATA temp510 LIKE sy-tabix.
    CREATE OBJECT lo_ok TYPE ltcl_system_task.
    lo_ok->input = 21.
    
    CREATE OBJECT lo_fail TYPE ltcl_system_task.
    lo_fail->input = -1.

    
    CLEAR temp503.
    INSERT lo_ok INTO TABLE temp503.
    INSERT lo_fail INTO TABLE temp503.
    
    lt_result = zabaputil_cl_util_context=>parallel_run( t_task     = temp503
                                                               sequential = abap_true
                                                               method     = `RUN` ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 42
                                        act = lo_ok->output ).
    
    
    temp506 = sy-tabix.
    READ TABLE lt_result INDEX 1 ASSIGNING <temp505>.
    sy-tabix = temp506.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp505>-message ).
    
    
    temp508 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp507>.
    sy-tabix = temp508.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = <temp507>-index ).
    
    
    temp510 = sy-tabix.
    READ TABLE lt_result INDEX 2 ASSIGNING <temp509>.
    sy-tabix = temp510.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_char_cp( exp = `*negative input*`
                                         act = <temp509>-message ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_bapi_ops DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    " the components of BAPIRET2 the bapi_* methods read and write - no
    " DDIC type, which is what makes the methods generic
    TYPES:
      BEGIN OF ty_s_ret,
        type       TYPE c LENGTH 1,
        id         TYPE c LENGTH 20,
        number     TYPE n LENGTH 3,
        message    TYPE c LENGTH 220,
        message_v1 TYPE c LENGTH 50,
        message_v2 TYPE c LENGTH 50,
        message_v3 TYPE c LENGTH 50,
        message_v4 TYPE c LENGTH 50,
      END OF ty_s_ret,
      ty_t_ret TYPE STANDARD TABLE OF ty_s_ret WITH DEFAULT KEY.

    METHODS check_is_error          FOR TESTING.
    METHODS check_is_error_msgty    FOR TESTING.
    METHODS finish_without_commit   FOR TESTING.
    METHODS finish_keeps_error_flag FOR TESTING.
    METHODS msg_add_text            FOR TESTING.
    METHODS msg_add_exception       FOR TESTING.
    METHODS msg_add_bapiret2        FOR TESTING.
    METHODS msg_add_bapiret2_empty  FOR TESTING.
    METHODS msg_add_table_prott     FOR TESTING.
    METHODS msg_add_table_mesg      FOR TESTING.
    METHODS set_material_short      FOR TESTING.
    METHODS set_material_long       FOR TESTING.
    METHODS set_material_none       FOR TESTING.
    METHODS alpha_in                FOR TESTING.

ENDCLASS.


CLASS ltcl_bapi_ops IMPLEMENTATION.

  METHOD check_is_error.

    DATA lt_ret TYPE ty_t_ret.

    DATA temp511 TYPE ltcl_bapi_ops=>ty_t_ret.
    DATA temp512 LIKE LINE OF temp511.
    DATA temp513 TYPE ltcl_bapi_ops=>ty_t_ret.
    DATA temp514 LIKE LINE OF temp513.
    DATA temp515 TYPE ltcl_bapi_ops=>ty_t_ret.
    DATA temp516 LIKE LINE OF temp515.
    DATA temp517 TYPE ltcl_bapi_ops=>ty_t_ret.
    DATA temp518 LIKE LINE OF temp517.
    CLEAR temp511.
    
    temp512-type = `S`.
    INSERT temp512 INTO TABLE temp511.
    temp512-type = `W`.
    INSERT temp512 INTO TABLE temp511.
    lt_ret = temp511.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>bapi_check_is_error( lt_ret ) ).

    
    CLEAR temp513.
    
    temp514-type = `S`.
    INSERT temp514 INTO TABLE temp513.
    temp514-type = `E`.
    INSERT temp514 INTO TABLE temp513.
    lt_ret = temp513.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>bapi_check_is_error( lt_ret ) ).

    
    CLEAR temp515.
    
    temp516-type = `A`.
    INSERT temp516 INTO TABLE temp515.
    lt_ret = temp515.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>bapi_check_is_error( lt_ret ) ).

    
    CLEAR temp517.
    
    temp518-type = `X`.
    INSERT temp518 INTO TABLE temp517.
    lt_ret = temp517.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>bapi_check_is_error( lt_ret ) ).

    CLEAR lt_ret.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>bapi_check_is_error( lt_ret ) ).

  ENDMETHOD.

  METHOD check_is_error_msgty.

    TYPES:
      BEGIN OF ty_s_prot,
        msgty TYPE c LENGTH 1,
      END OF ty_s_prot.
    TYPES temp36 TYPE STANDARD TABLE OF ty_s_prot WITH DEFAULT KEY.
DATA lt_prot TYPE temp36.

    DATA temp519 LIKE lt_prot.
    DATA temp520 LIKE LINE OF temp519.
    CLEAR temp519.
    
    temp520-msgty = `I`.
    INSERT temp520 INTO TABLE temp519.
    temp520-msgty = `E`.
    INSERT temp520 INTO TABLE temp519.
    lt_prot = temp519.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>bapi_check_is_error( lt_prot ) ).

  ENDMETHOD.

  METHOD finish_without_commit.

    DATA lt_ret      TYPE ty_t_ret.
    DATA lv_is_error TYPE abap_bool.

    DATA temp521 TYPE ltcl_bapi_ops=>ty_t_ret.
    DATA temp522 LIKE LINE OF temp521.
    DATA temp523 TYPE ltcl_bapi_ops=>ty_t_ret.
    DATA temp524 LIKE LINE OF temp523.
    CLEAR temp521.
    
    temp522-type = `S`.
    temp522-message = `ok`.
    INSERT temp522 INTO TABLE temp521.
    lt_ret = temp521.
    zabaputil_cl_util_context=>bapi_finish( CHANGING ct_bapiret  = lt_ret
                                                     cv_is_error = lv_is_error ).
    cl_abap_unit_assert=>assert_false( lv_is_error ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_ret ) ).

    
    CLEAR temp523.
    
    temp524-type = `E`.
    temp524-message = `failed`.
    INSERT temp524 INTO TABLE temp523.
    lt_ret = temp523.
    zabaputil_cl_util_context=>bapi_finish( CHANGING ct_bapiret  = lt_ret
                                                     cv_is_error = lv_is_error ).
    cl_abap_unit_assert=>assert_true( lv_is_error ).

  ENDMETHOD.

  METHOD finish_keeps_error_flag.

    DATA lt_ret      TYPE ty_t_ret.
    DATA lv_is_error TYPE abap_bool.
    DATA temp525 TYPE ltcl_bapi_ops=>ty_t_ret.
    DATA temp526 LIKE LINE OF temp525.

    " an error flagged by the caller itself survives success messages
    lv_is_error = abap_true.
    
    CLEAR temp525.
    
    temp526-type = `S`.
    INSERT temp526 INTO TABLE temp525.
    lt_ret = temp525.
    zabaputil_cl_util_context=>bapi_finish( CHANGING ct_bapiret  = lt_ret
                                                     cv_is_error = lv_is_error ).
    cl_abap_unit_assert=>assert_true( lv_is_error ).

  ENDMETHOD.

  METHOD msg_add_text.

    DATA lt_ret TYPE ty_t_ret.
    FIELD-SYMBOLS <temp527> LIKE LINE OF lt_ret.
    DATA temp528 LIKE sy-tabix.
    FIELD-SYMBOLS <temp529> LIKE LINE OF lt_ret.
    DATA temp530 LIKE sy-tabix.
    FIELD-SYMBOLS <temp531> LIKE LINE OF lt_ret.
    DATA temp532 LIKE sy-tabix.

    zabaputil_cl_util_context=>bapi_msg_add_text( EXPORTING iv_text    = `not found`
                                                  CHANGING  ct_bapiret = lt_ret ).
    zabaputil_cl_util_context=>bapi_msg_add_text( EXPORTING iv_text    = `just saying`
                                                            iv_type    = `I`
                                                  CHANGING  ct_bapiret = lt_ret ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_ret ) ).
    
    
    temp528 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp527>.
    sy-tabix = temp528.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp527>-type ).
    
    
    temp530 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp529>.
    sy-tabix = temp530.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `not found`
                                        act = <temp529>-message ).
    
    
    temp532 = sy-tabix.
    READ TABLE lt_ret INDEX 2 ASSIGNING <temp531>.
    sy-tabix = temp532.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `I`
                                        act = <temp531>-type ).

  ENDMETHOD.

  METHOD msg_add_exception.

    DATA lt_ret TYPE ty_t_ret.
        DATA lx_error TYPE REF TO cx_root.
    FIELD-SYMBOLS <temp533> LIKE LINE OF lt_ret.
    DATA temp534 LIKE sy-tabix.
    FIELD-SYMBOLS <temp535> LIKE LINE OF lt_ret.
    DATA temp536 LIKE sy-tabix.

    TRY.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `the error text`.
        
      CATCH cx_root INTO lx_error.
        zabaputil_cl_util_context=>bapi_msg_add_exception( EXPORTING ix_error   = lx_error
                                                           CHANGING  ct_bapiret = lt_ret ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_ret ) ).
    
    
    temp534 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp533>.
    sy-tabix = temp534.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp533>-type ).
    
    
    temp536 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp535>.
    sy-tabix = temp536.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `the error text`
                                        act = <temp535>-message ).

  ENDMETHOD.

  METHOD msg_add_bapiret2.

    DATA lt_ret TYPE ty_t_ret.

    DATA temp537 TYPE ty_s_ret.
    FIELD-SYMBOLS <temp538> LIKE LINE OF lt_ret.
    DATA temp539 LIKE sy-tabix.
    FIELD-SYMBOLS <temp540> LIKE LINE OF lt_ret.
    DATA temp541 LIKE sy-tabix.
    CLEAR temp537.
    temp537-type = `W`.
    temp537-id = `V1`.
    temp537-number = `123`.
    temp537-message = `careful`.
    zabaputil_cl_util_context=>bapi_msg_add_bapiret2(
      EXPORTING is_return  = temp537
      CHANGING  ct_bapiret = lt_ret ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_ret ) ).
    
    
    temp539 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp538>.
    sy-tabix = temp539.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `V1`
                                        act = <temp538>-id ).
    
    
    temp541 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp540>.
    sy-tabix = temp541.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `123`
                                        act = <temp540>-number ).

  ENDMETHOD.

  METHOD msg_add_bapiret2_empty.

    DATA lt_ret TYPE ty_t_ret.

    DATA temp542 TYPE ty_s_ret.
    CLEAR temp542.
    zabaputil_cl_util_context=>bapi_msg_add_bapiret2( EXPORTING is_return  = temp542
                                                      CHANGING  ct_bapiret = lt_ret ).
    cl_abap_unit_assert=>assert_initial( lt_ret ).

  ENDMETHOD.

  METHOD msg_add_table_prott.

    TYPES:
      BEGIN OF ty_s_prot,
        msgty TYPE c LENGTH 1,
        msgid TYPE c LENGTH 20,
        msgno TYPE n LENGTH 3,
        msgv1 TYPE c LENGTH 50,
      END OF ty_s_prot.
    TYPES temp37 TYPE STANDARD TABLE OF ty_s_prot WITH DEFAULT KEY.
DATA lt_prot TYPE temp37.
    DATA lt_ret  TYPE ty_t_ret.

    DATA temp543 LIKE lt_prot.
    DATA temp544 LIKE LINE OF temp543.
    FIELD-SYMBOLS <temp545> LIKE LINE OF lt_ret.
    DATA temp546 LIKE sy-tabix.
    FIELD-SYMBOLS <temp547> LIKE LINE OF lt_ret.
    DATA temp548 LIKE sy-tabix.
    FIELD-SYMBOLS <temp549> LIKE LINE OF lt_ret.
    DATA temp550 LIKE sy-tabix.
    FIELD-SYMBOLS <temp551> LIKE LINE OF lt_ret.
    DATA temp552 LIKE sy-tabix.
    CLEAR temp543.
    
    temp544-msgty = `E`.
    temp544-msgid = `00`.
    temp544-msgno = `001`.
    temp544-msgv1 = `value`.
    INSERT temp544 INTO TABLE temp543.
    lt_prot = temp543.
    zabaputil_cl_util_context=>bapi_msg_add_table( EXPORTING it_table   = lt_prot
                                                   CHANGING  ct_bapiret = lt_ret ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_ret ) ).
    
    
    temp546 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp545>.
    sy-tabix = temp546.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = <temp545>-type ).
    
    
    temp548 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp547>.
    sy-tabix = temp548.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `00`
                                        act = <temp547>-id ).
    
    
    temp550 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp549>.
    sy-tabix = temp550.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `001`
                                        act = <temp549>-number ).
    
    
    temp552 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp551>.
    sy-tabix = temp552.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `value`
                                        act = <temp551>-message_v1 ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>bapi_check_is_error( lt_ret ) ).

  ENDMETHOD.

  METHOD msg_add_table_mesg.

    " MESG names the message class ARBGB and the number TXTNR
    TYPES:
      BEGIN OF ty_s_mesg,
        msgty TYPE c LENGTH 1,
        arbgb TYPE c LENGTH 20,
        txtnr TYPE c LENGTH 3,
        text  TYPE c LENGTH 73,
      END OF ty_s_mesg.
    TYPES temp38 TYPE STANDARD TABLE OF ty_s_mesg WITH DEFAULT KEY.
DATA lt_mesg TYPE temp38.
    DATA lt_ret  TYPE ty_t_ret.

    DATA temp553 LIKE lt_mesg.
    DATA temp554 LIKE LINE OF temp553.
    FIELD-SYMBOLS <temp555> LIKE LINE OF lt_ret.
    DATA temp556 LIKE sy-tabix.
    FIELD-SYMBOLS <temp557> LIKE LINE OF lt_ret.
    DATA temp558 LIKE sy-tabix.
    FIELD-SYMBOLS <temp559> LIKE LINE OF lt_ret.
    DATA temp560 LIKE sy-tabix.
    FIELD-SYMBOLS <temp561> LIKE LINE OF lt_ret.
    DATA temp562 LIKE sy-tabix.
    CLEAR temp553.
    
    temp554-msgty = `W`.
    temp554-arbgb = `VL`.
    temp554-txtnr = `042`.
    temp554-text = `a warning`.
    INSERT temp554 INTO TABLE temp553.
    lt_mesg = temp553.
    zabaputil_cl_util_context=>bapi_msg_add_table( EXPORTING it_table   = lt_mesg
                                                   CHANGING  ct_bapiret = lt_ret ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_ret ) ).
    
    
    temp556 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp555>.
    sy-tabix = temp556.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `W`
                                        act = <temp555>-type ).
    
    
    temp558 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp557>.
    sy-tabix = temp558.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `VL`
                                        act = <temp557>-id ).
    
    
    temp560 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp559>.
    sy-tabix = temp560.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `042`
                                        act = <temp559>-number ).
    
    
    temp562 = sy-tabix.
    READ TABLE lt_ret INDEX 1 ASSIGNING <temp561>.
    sy-tabix = temp562.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a warning`
                                        act = <temp561>-message ).

  ENDMETHOD.

  METHOD set_material_short.

    TYPES:
      BEGIN OF ty_s_item,
        material TYPE c LENGTH 18,
        plant    TYPE c LENGTH 4,
      END OF ty_s_item.
    DATA ls_item TYPE ty_s_item.

    zabaputil_cl_util_context=>bapi_set_material( EXPORTING iv_matnr = `MAT-1`
                                                  CHANGING  cs_data  = ls_item ).
    cl_abap_unit_assert=>assert_equals( exp = `MAT-1`
                                        act = ls_item-material ).

  ENDMETHOD.

  METHOD set_material_long.

    TYPES:
      BEGIN OF ty_s_item,
        material      TYPE c LENGTH 18,
        material_long TYPE c LENGTH 40,
      END OF ty_s_item.
    DATA ls_item TYPE ty_s_item.

    zabaputil_cl_util_context=>bapi_set_material( EXPORTING iv_matnr = `A-MATERIAL-NUMBER-LONGER-THAN-18`
                                                  CHANGING  cs_data  = ls_item ).
    cl_abap_unit_assert=>assert_equals( exp = `A-MATERIAL-NUMBER-LONGER-THAN-18`
                                        act = ls_item-material_long ).
    cl_abap_unit_assert=>assert_initial( ls_item-material ).

  ENDMETHOD.

  METHOD set_material_none.

    TYPES:
      BEGIN OF ty_s_item,
        plant TYPE c LENGTH 4,
      END OF ty_s_item.
    DATA ls_item TYPE ty_s_item.

    zabaputil_cl_util_context=>bapi_set_material( EXPORTING iv_matnr = `MAT-1`
                                                  CHANGING  cs_data  = ls_item ).
    cl_abap_unit_assert=>assert_initial( ls_item ).

  ENDMETHOD.

  METHOD alpha_in.

    cl_abap_unit_assert=>assert_equals( exp = `0080001234`
                                        act = zabaputil_cl_util_context=>conv_alpha_in( `80001234` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `000000000000004711`
                                        act = zabaputil_cl_util_context=>conv_alpha_in( iv_value  = ` 4711 `
                                                                                        iv_length = 18 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `MAT-1`
                                        act = zabaputil_cl_util_context=>conv_alpha_in( `MAT-1` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>conv_alpha_in( `` ) ).

  ENDMETHOD.

ENDCLASS.

"! Regression tests for defects found by comparing the behaviour with what
"! the method names promise - each test is the input that went wrong.
CLASS ltcl_fixes DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_row,
        col1 TYPE string,
        col2 TYPE string,
      END OF ty_s_row,
      ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

    METHODS date_impossible          FOR TESTING.
    METHODS date_leap_year           FOR TESTING.
    METHODS paginate_exact_pages     FOR TESTING.
    METHODS token_operator_alone     FOR TESTING.
    METHODS token_patterns           FOR TESTING.
    METHODS token_pattern_roundtrip  FOR TESTING.
    METHODS numeric_needs_digit      FOR TESTING.
    METHODS number_trailing_minus    FOR TESTING.
    METHODS pad_multi_char           FOR TESTING.
    METHODS mimetype_empty_name      FOR TESTING.
    METHODS round_modes              FOR TESTING.
    METHODS number_string_rounds     FOR TESTING.
    METHODS email_malformed          FOR TESTING.
    METHODS language_codes           FOR TESTING.
    METHODS json_control_chars       FOR TESTING.
    METHODS url_decode_invalid_hex   FOR TESTING.
    METHODS data_uri_base64          FOR TESTING.
    METHODS data_uri_percent         FOR TESTING.
    METHODS data_uri_mime_by_name    FOR TESTING.
    METHODS file_name_by_path        FOR TESTING.
    METHODS xml_unescape             FOR TESTING.
    METHODS csv_quotes_special_cells FOR TESTING.
    METHODS csv_reads_own_output     FOR TESTING.
    METHODS csv_reads_quoted_cells   FOR TESTING.
    METHODS csv_quote_edge_cases     FOR TESTING.
    METHODS csv_empty_input          FOR TESTING.
    METHODS csv_byte_order_mark      FOR TESTING.
    METHODS csv_fallback_name_taken  FOR TESTING.
    METHODS sql_like_escape          FOR TESTING.
    METHODS sql_like_cp_escape       FOR TESTING.
    METHODS sql_lowercase_keywords   FOR TESTING.
    METHODS sql_keyword_in_literal   FOR TESTING.
    METHODS sql_exclude_roundtrip    FOR TESTING.
    METHODS sql_groups_keep_parens   FOR TESTING.
    METHODS sql_tabname_with_keyword FOR TESTING.
    METHODS sql_roundtrip_kept       FOR TESTING.
    METHODS sql_not_a_filter         FOR TESTING.

ENDCLASS.

CLASS ltcl_fixes IMPLEMENTATION.

  METHOD date_impossible.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `2024-02-30` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `2024-13-45` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `2024-04-31` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `2024-00-10` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `24` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_date_valid( `2024-12-31` ) ).
  ENDMETHOD.

  METHOD date_leap_year.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_date_valid( `2024-02-29` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `2023-02-29` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_date_valid( `1900-02-29` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_date_valid( `2000-02-29` ) ).
  ENDMETHOD.

  METHOD paginate_exact_pages.
    DATA lv_pages TYPE i.
    DATA temp563 TYPE ty_t_row.
    DATA temp564 LIKE LINE OF temp563.
    DATA lt_tab LIKE temp563.
    CLEAR temp563.
    
    temp564-col1 = `1`.
    INSERT temp564 INTO TABLE temp563.
    temp564-col1 = `2`.
    INSERT temp564 INTO TABLE temp563.
    temp564-col1 = `3`.
    INSERT temp564 INTO TABLE temp563.
    temp564-col1 = `4`.
    INSERT temp564 INTO TABLE temp563.
    temp564-col1 = `5`.
    INSERT temp564 INTO TABLE temp563.
    temp564-col1 = `6`.
    INSERT temp564 INTO TABLE temp563.
    temp564-col1 = `7`.
    INSERT temp564 INTO TABLE temp563.
    
    lt_tab = temp563.
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab         = lt_tab
                                                        page_size   = 4
                                              IMPORTING total_pages = lv_pages ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lv_pages ).

    DELETE lt_tab FROM 5.
    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab         = lt_tab
                                                        page_size   = 2
                                              IMPORTING total_pages = lv_pages ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lv_pages ).

    zabaputil_cl_util_context=>itab_paginate( EXPORTING tab         = lt_tab
                                                        page_size   = 3
                                              IMPORTING total_pages = lv_pages ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lv_pages ).
  ENDMETHOD.

  METHOD token_operator_alone.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `<` ).
    cl_abap_unit_assert=>assert_equals( exp = `LT` act = ls_range-option ).
    cl_abap_unit_assert=>assert_initial( ls_range-low ).
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `>` ).
    cl_abap_unit_assert=>assert_equals( exp = `GT` act = ls_range-option ).
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `<=` ).
    cl_abap_unit_assert=>assert_equals( exp = `LE` act = ls_range-option ).
    cl_abap_unit_assert=>assert_initial( ls_range-low ).
  ENDMETHOD.

  METHOD token_patterns.
    DATA ls_range TYPE zabaputil_cl_util_context=>ty_s_range.
    TYPES temp39 TYPE RANGE OF string.
DATA lt_range TYPE temp39.
    DATA temp565 LIKE lt_range.
    DATA temp566 LIKE LINE OF temp565.
    DATA temp41 TYPE xsdboolean.
    DATA temp42 TYPE xsdboolean.
    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `*abc` ).
    cl_abap_unit_assert=>assert_equals( exp = `I` act = ls_range-sign ).
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `*abc` act = ls_range-low ).

    ls_range = zabaputil_cl_util_context=>filter_get_range_by_token( `abc*` ).
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = ls_range-option ).
    cl_abap_unit_assert=>assert_equals( exp = `abc*` act = ls_range-low ).

    " the pattern works as one: the range selects by it
    

    
    CLEAR temp565.
    
    temp566-sign = `I`.
    temp566-option = `CP`.
    temp566-low = ls_range-low.
    INSERT temp566 INTO TABLE temp565.
    lt_range = temp565.
    
    temp41 = boolc( `abcdef` IN lt_range ).
    cl_abap_unit_assert=>assert_true( temp41 ).
    
    temp42 = boolc( `xabc` IN lt_range ).
    cl_abap_unit_assert=>assert_false( temp42 ).
  ENDMETHOD.

  METHOD token_pattern_roundtrip.
    DATA temp567 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp568 LIKE LINE OF temp567.
    DATA lt_range LIKE temp567.
    DATA lt_token TYPE zabaputil_cl_util_context=>ty_t_token.
    FIELD-SYMBOLS <temp569> LIKE LINE OF lt_token.
    DATA temp570 LIKE sy-tabix.
    FIELD-SYMBOLS <temp571> LIKE LINE OF lt_token.
    DATA temp572 LIKE sy-tabix.
    CLEAR temp567.
    
    temp568-sign = `I`.
    temp568-option = `CP`.
    temp568-low = `abc*`.
    INSERT temp568 INTO TABLE temp567.
    temp568-sign = `I`.
    temp568-option = `CP`.
    temp568-low = `xyz`.
    INSERT temp568 INTO TABLE temp567.
    
    lt_range = temp567.
    
    lt_token = zabaputil_cl_util_context=>filter_get_token_t_by_range_t( lt_range ).
    
    
    temp570 = sy-tabix.
    READ TABLE lt_token INDEX 1 ASSIGNING <temp569>.
    sy-tabix = temp570.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `abc*` act = <temp569>-key ).
    " a CP row without an asterisk keeps the rendering it always had
    
    
    temp572 = sy-tabix.
    READ TABLE lt_token INDEX 2 ASSIGNING <temp571>.
    sy-tabix = temp572.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `*xyz*` act = <temp571>-key ).
  ENDMETHOD.

  METHOD numeric_needs_digit.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_numeric_string( `-` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_numeric_string( `+` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_numeric_string( `.` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_numeric_string( `-.` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_numeric_string( `-5-` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `-1.5` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_numeric_string( `123-` ) ).
  ENDMETHOD.

  METHOD number_trailing_minus.
    DATA temp573 TYPE decfloat34.
    DATA temp574 TYPE decfloat34.
    DATA temp575 TYPE decfloat34.
    temp573 = '-123'.
    cl_abap_unit_assert=>assert_equals( exp = temp573
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `123-` ) ).
    
    temp574 = '-1.5'.
    cl_abap_unit_assert=>assert_equals( exp = temp574
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `1,5-` ) ).
    
    temp575 = '-7'.
    cl_abap_unit_assert=>assert_equals( exp = temp575
                                        act = zabaputil_cl_util_context=>conv_string_to_number( `-7` ) ).
  ENDMETHOD.

  METHOD pad_multi_char.
    cl_abap_unit_assert=>assert_equals( exp = `aba5`
                                        act = zabaputil_cl_util_context=>c_pad_left( val = `5` len = 4 pad = 'ab' ) ).
    cl_abap_unit_assert=>assert_equals( exp = `5aba`
                                        act = zabaputil_cl_util_context=>c_pad_right( val = `5` len = 4 pad = 'ab' ) ).
    cl_abap_unit_assert=>assert_equals( exp = `005`
                                        act = zabaputil_cl_util_context=>c_pad_left( val = `5` len = 3 ) ).
  ENDMETHOD.

  METHOD mimetype_empty_name.
    cl_abap_unit_assert=>assert_equals( exp = `application/octet-stream`
                                        act = zabaputil_cl_util_context=>file_get_mimetype( `` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `application/pdf`
                                        act = zabaputil_cl_util_context=>file_get_mimetype( `a.b.PDF` ) ).
  ENDMETHOD.

  METHOD round_modes.
    DATA temp576 TYPE decfloat34.
    DATA temp90 TYPE decfloat34.
    DATA temp577 TYPE decfloat34.
    DATA temp91 TYPE decfloat34.
    DATA temp578 TYPE decfloat34.
    DATA temp92 TYPE decfloat34.
    DATA temp579 TYPE decfloat34.
    DATA temp93 TYPE decfloat34.
    DATA temp580 TYPE decfloat34.
    DATA temp94 TYPE decfloat34.
    DATA temp581 TYPE decfloat34.
    DATA temp95 TYPE decfloat34.
    DATA temp582 TYPE decfloat34.
    DATA temp96 TYPE decfloat34.
    DATA temp583 TYPE decfloat34.
    DATA temp97 TYPE decfloat34.
    temp576 = '2'.
    
    temp90 = '2.5'.
    cl_abap_unit_assert=>assert_equals( exp = temp576
      act = zabaputil_cl_util_context=>num_round( val = temp90 mode = `HALF_EVEN` ) ).
    
    temp577 = '4'.
    
    temp91 = '3.5'.
    cl_abap_unit_assert=>assert_equals( exp = temp577
      act = zabaputil_cl_util_context=>num_round( val = temp91 mode = `HALF_EVEN` ) ).
    
    temp578 = '2'.
    
    temp92 = '2.5'.
    cl_abap_unit_assert=>assert_equals( exp = temp578
      act = zabaputil_cl_util_context=>num_round( val = temp92 mode = `HALF_DOWN` ) ).
    
    temp579 = '3'.
    
    temp93 = '2.1'.
    cl_abap_unit_assert=>assert_equals( exp = temp579
      act = zabaputil_cl_util_context=>num_round( val = temp93 mode = `CEILING` ) ).
    
    temp580 = '-2'.
    
    temp94 = '-2.9'.
    cl_abap_unit_assert=>assert_equals( exp = temp580
      act = zabaputil_cl_util_context=>num_round( val = temp94 mode = `CEILING` ) ).
    
    temp581 = '-3'.
    
    temp95 = '-2.1'.
    cl_abap_unit_assert=>assert_equals( exp = temp581
      act = zabaputil_cl_util_context=>num_round( val = temp95 mode = `FLOOR` ) ).
    
    temp582 = '2'.
    
    temp96 = '2.9'.
    cl_abap_unit_assert=>assert_equals( exp = temp582
      act = zabaputil_cl_util_context=>num_round( val = temp96 mode = `FLOOR` ) ).
    
    temp583 = '2.35'.
    
    temp97 = '2.345'.
    cl_abap_unit_assert=>assert_equals( exp = temp583
      act = zabaputil_cl_util_context=>num_round( val = temp97 decimals = 2 ) ).
  ENDMETHOD.

  METHOD number_string_rounds.
    DATA temp584 TYPE decfloat34.
    DATA temp585 TYPE decfloat34.
    DATA temp586 TYPE decfloat34.
    DATA temp587 TYPE decfloat34.
    temp584 = '1.999'.
    cl_abap_unit_assert=>assert_equals( exp = `2.00`
      act = zabaputil_cl_util_context=>conv_number_to_string( val = temp584 decimals = 2 ) ).
    
    temp585 = '0.5'.
    cl_abap_unit_assert=>assert_equals( exp = `1`
      act = zabaputil_cl_util_context=>conv_number_to_string( val = temp585 decimals = 0 ) ).
    
    temp586 = '-0.4'.
    cl_abap_unit_assert=>assert_equals( exp = `0`
      act = zabaputil_cl_util_context=>conv_number_to_string( val = temp586 decimals = 0 ) ).
    
    temp587 = '1234.567'.
    cl_abap_unit_assert=>assert_equals( exp = `1,234.57`
      act = zabaputil_cl_util_context=>conv_number_to_string( val           = temp587
                                                              decimals      = 2
                                                              sep_thousands = ',' ) ).
  ENDMETHOD.

  METHOD email_malformed.
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `a@.` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `a@b.` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `a@.com` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `john doe@x.y` ) ).
    cl_abap_unit_assert=>assert_false( zabaputil_cl_util_context=>check_is_email( `a@b..c` ) ).
    cl_abap_unit_assert=>assert_true( zabaputil_cl_util_context=>check_is_email( `first.last@sub.example.org` ) ).
  ENDMETHOD.

  METHOD language_codes.
    cl_abap_unit_assert=>assert_equals( exp = `cs` act = zabaputil_cl_util_context=>lang_sap_to_iso( 'C' ) ).
    cl_abap_unit_assert=>assert_equals( exp = `ar` act = zabaputil_cl_util_context=>lang_sap_to_iso( 'A' ) ).
    cl_abap_unit_assert=>assert_equals( exp = `zh` act = zabaputil_cl_util_context=>lang_sap_to_iso( '1' ) ).
    cl_abap_unit_assert=>assert_equals( exp = `1` act = zabaputil_cl_util_context=>lang_iso_to_sap( `zh` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `C` act = zabaputil_cl_util_context=>lang_iso_to_sap( `cs` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `D` act = zabaputil_cl_util_context=>lang_iso_to_sap( `de` ) ).
  ENDMETHOD.

  METHOD json_control_chars.
    DATA lv_text TYPE string.
    DATA temp588 TYPE xstring.
    DATA lv_ctrl TYPE string.
    DATA temp589 TYPE xstring.
    DATA lv_nul TYPE string.
    lv_text = |a{ cl_abap_char_utilities=>form_feed }b{ cl_abap_char_utilities=>backspace }c|.
    cl_abap_unit_assert=>assert_equals( exp = `a\fb\bc`
                                        act = zabaputil_cl_util_context=>c_escape_json( lv_text ) ).
    
    temp588 = `411F42`.
    
    lv_ctrl = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp588 ).
    cl_abap_unit_assert=>assert_equals( exp = `A\u001FB`
                                        act = zabaputil_cl_util_context=>c_escape_json( lv_ctrl ) ).
    
    temp589 = `410042`.
    
    lv_nul = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp589 ).
    cl_abap_unit_assert=>assert_equals( exp = `A\u0000B`
                                        act = zabaputil_cl_util_context=>c_escape_json( lv_nul ) ).
    cl_abap_unit_assert=>assert_equals( exp = `say \"hi\"\n`
                                        act = zabaputil_cl_util_context=>c_escape_json( |say "hi"\n| ) ).
    cl_abap_unit_assert=>assert_equals( exp = `plain`
                                        act = zabaputil_cl_util_context=>c_escape_json( `plain` ) ).
  ENDMETHOD.

  METHOD data_uri_base64.
    " `;base64` in the header, or no header at all: base64
    DATA temp590 TYPE xstring.
    DATA temp591 TYPE xstring.
    DATA ls_file TYPE zabaputil_cl_util_context=>ty_s_data_uri.
    temp590 = `414243`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp590
        act = zabaputil_cl_util_context=>conv_get_xstring_by_data_uri( `data:text/plain;charset=utf-8;base64,QUJD` ) ).
    
    temp591 = `414243`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp591
        act = zabaputil_cl_util_context=>conv_get_xstring_by_data_uri( `QUJD` ) ).
    
    ls_file = zabaputil_cl_util_context=>conv_get_file_by_data_uri( `data:Image/PNG;base64,QUJD` ).
    cl_abap_unit_assert=>assert_equals( exp = `image/png`
                                        act = ls_file-mime_type ).
  ENDMETHOD.

  METHOD data_uri_percent.
    " no `;base64`: the payload is percent-encoded - it was base64-decoded
    " all the same and came out as garbage. `+` stays a plus
    DATA temp592 TYPE xstring.
    DATA ls_file TYPE zabaputil_cl_util_context=>ty_s_data_uri.
    DATA temp593 TYPE xstring.
    temp592 = `4120422B43FF`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp592
        act = zabaputil_cl_util_context=>conv_get_xstring_by_data_uri( `data:text/plain,A%20B+C%ff` ) ).
    
    ls_file = zabaputil_cl_util_context=>conv_get_file_by_data_uri( `data:,x` ).
    
    temp593 = `78`.
    cl_abap_unit_assert=>assert_equals( exp = temp593
                                        act = ls_file-content ).
    cl_abap_unit_assert=>assert_initial( ls_file-mime_type ).
  ENDMETHOD.

  METHOD data_uri_mime_by_name.
    " no MIME type, or only octet-stream, in the URI: the file name decides
    DATA ls_file TYPE zabaputil_cl_util_context=>ty_s_data_uri.
    ls_file = zabaputil_cl_util_context=>conv_get_file_by_data_uri(
                        val       = `data:application/octet-stream;base64,QUJD`
                        file_name = `notes.txt` ).
    cl_abap_unit_assert=>assert_equals( exp = `text/plain`
                                        act = ls_file-mime_type ).
    ls_file = zabaputil_cl_util_context=>conv_get_file_by_data_uri( val       = `data:image/png;base64,QUJD`
                                                                    file_name = `notes.txt` ).
    cl_abap_unit_assert=>assert_equals( exp = `image/png`
                                        act = ls_file-mime_type ).
  ENDMETHOD.

  METHOD xml_unescape.
    DATA temp594 TYPE xstring.
    cl_abap_unit_assert=>assert_equals(
        exp = `<a href="x">'&'</a>`
        act = zabaputil_cl_util_context=>c_unescape_xml( `&lt;a href=&quot;x&quot;&gt;&apos;&amp;&apos;&lt;/a&gt;` ) ).
    " an escaped reference stays a reference
    cl_abap_unit_assert=>assert_equals( exp = `&lt;`
                                        act = zabaputil_cl_util_context=>c_unescape_xml( `&amp;lt;` ) ).
    " character references, decimal and hex, as c_escape_xml writes them
    cl_abap_unit_assert=>assert_equals( exp = |a\nb\tc\rd A|
                                        act = zabaputil_cl_util_context=>c_unescape_xml( `a&#xA;b&#9;c&#xD;d&#32;&#x41;` ) ).
    
    temp594 = `C3A9E282AC`.
    cl_abap_unit_assert=>assert_equals(
        exp = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp594 )
        act = zabaputil_cl_util_context=>c_unescape_xml( `&#xE9;&#8364;` ) ).
    " what is no reference, or names no character XML allows, stays
    cl_abap_unit_assert=>assert_equals( exp = `a & b &unknown; &lt &#0; &#xZZ; &#;`
                                        act = zabaputil_cl_util_context=>c_unescape_xml( `a & b &unknown; &lt &#0; &#xZZ; &#;` ) ).
    " the round trip of c_escape_xml
    cl_abap_unit_assert=>assert_equals(
        exp = |x < "y" & z\nnext|
        act = zabaputil_cl_util_context=>c_unescape_xml( zabaputil_cl_util_context=>c_escape_xml( |x < "y" & z\nnext| ) ) ).
  ENDMETHOD.

  METHOD file_name_by_path.
    cl_abap_unit_assert=>assert_equals(
        exp = `report.xlsx`
        act = zabaputil_cl_util_context=>file_get_name_by_path( `C:\fakepath\report.xlsx` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `b.txt`
        act = zabaputil_cl_util_context=>file_get_name_by_path( `/home/a\b.txt` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = `plain.csv`
        act = zabaputil_cl_util_context=>file_get_name_by_path( `plain.csv` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_util_context=>file_get_name_by_path( `dir/` ) ).
  ENDMETHOD.

  METHOD url_decode_invalid_hex.
    cl_abap_unit_assert=>assert_equals( exp = `a%ZZb`
                                        act = zabaputil_cl_util_context=>url_decode( `a%ZZb` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `a b`
                                        act = zabaputil_cl_util_context=>url_decode( `a%20b` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `100%`
                                        act = zabaputil_cl_util_context=>url_decode( `100%` ) ).
    " escapes that are no UTF-8 stay as they were written
    cl_abap_unit_assert=>assert_equals( exp = `a%FFb`
                                        act = zabaputil_cl_util_context=>url_decode( `a%FFb` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `a%C3`
                                        act = zabaputil_cl_util_context=>url_decode( `a%C3` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `a b%e9`
                                        act = zabaputil_cl_util_context=>url_decode( `a%20b%e9` ) ).
  ENDMETHOD.

  METHOD csv_quotes_special_cells.
    DATA temp595 TYPE ty_t_row.
    DATA temp596 LIKE LINE OF temp595.
    DATA lt_tab LIKE temp595.
    DATA lv_csv TYPE string.
    DATA lv_exp TYPE string.
    CLEAR temp595.
    
    temp596-col1 = `a;b`.
    temp596-col2 = `say "hi"`.
    INSERT temp596 INTO TABLE temp595.
    temp596-col1 = |line1\nline2|.
    temp596-col2 = `plain`.
    INSERT temp596 INTO TABLE temp595.
    
    lt_tab = temp595.
    
    lv_csv = zabaputil_cl_util_context=>itab_get_csv_by_itab( lt_tab ).
    
    lv_exp = |COL1;COL2\r\n"a;b";"say ""hi"""\r\n"line1\nline2";plain|.
    cl_abap_unit_assert=>assert_equals( exp = lv_exp act = lv_csv ).
  ENDMETHOD.

  METHOD csv_reads_own_output.
    FIELD-SYMBOLS <lt_back> TYPE STANDARD TABLE.
    DATA temp597 TYPE ty_t_row.
    DATA temp598 LIKE LINE OF temp597.
    DATA lt_tab LIKE temp597.
    DATA lr_back TYPE REF TO data.
    DATA lt_back TYPE ty_t_row.
    FIELD-SYMBOLS <temp1> TYPE any.
    FIELD-SYMBOLS <temp2> LIKE LINE OF lt_back.
    CLEAR temp597.
    
    temp598-col1 = `a;b`.
    temp598-col2 = `say "hi"`.
    INSERT temp598 INTO TABLE temp597.
    temp598-col1 = |line1\nline2|.
    temp598-col2 = ``.
    INSERT temp598 INTO TABLE temp597.
    temp598-col1 = `a value that is longer than forty characters in total`.
    temp598-col2 = `x`.
    INSERT temp598 INTO TABLE temp597.
    
    lt_tab = temp597.
    
    lr_back = zabaputil_cl_util_context=>itab_get_itab_by_csv(
                      zabaputil_cl_util_context=>itab_get_csv_by_itab( lt_tab ) ).
    ASSIGN lr_back->* TO <lt_back>.
    
    
    
    CLEAR lt_back.
    LOOP AT <lt_back> ASSIGNING <temp1>.
      APPEND INITIAL LINE TO lt_back ASSIGNING <temp2>.
      MOVE-CORRESPONDING <temp1> TO <temp2>.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( exp = lt_tab act = lt_back ).
  ENDMETHOD.

  METHOD csv_reads_quoted_cells.
    FIELD-SYMBOLS <lt_back> TYPE STANDARD TABLE.
    " LF only, a separator behind the last column, a blank and a digit in the header
    DATA lr_back TYPE REF TO data.
    lr_back = zabaputil_cl_util_context=>itab_get_itab_by_csv( |First Name;2nd;\nx;"p;q";\n\ny;z;\n| ).
    ASSIGN lr_back->* TO <lt_back>.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <lt_back> ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = `[{"c_2nd":"p;q","first_name":"x"},{"c_2nd":"z","first_name":"y"}]`
      act = zabaputil_cl_util_context=>json_stringify( <lt_back> ) ).
  ENDMETHOD.

  METHOD csv_quote_edge_cases.
    FIELD-SYMBOLS <lt_back> TYPE STANDARD TABLE.
    " CR alone ends a row; text behind a closing quote belongs to the cell;
    " a quote inside a cell is text; an empty quoted cell; a quote that is
    " never closed runs to the end of the text
    DATA lr_back TYPE REF TO data.
    lr_back = zabaputil_cl_util_context=>itab_get_itab_by_csv( |H1;H2\r"a""b"x;c"d\r"";\n"open;end| ).
    ASSIGN lr_back->* TO <lt_back>.
    cl_abap_unit_assert=>assert_equals(
      exp = `[{"h1":"a\"bx","h2":"c\"d"},{"h1":"","h2":""},{"h1":"open;end","h2":""}]`
      act = zabaputil_cl_util_context=>json_stringify( <lt_back> ) ).
  ENDMETHOD.

  METHOD csv_fallback_name_taken.
    " the name a repeated column falls back to is in the header already
    FIELD-SYMBOLS <back> TYPE STANDARD TABLE.
    DATA lr_back TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    lr_back = zabaputil_cl_util_context=>itab_get_itab_by_csv( |A;COL3;A\nx;y;z| ).
    ASSIGN lr_back->* TO <back>.
    
    READ TABLE <back> INDEX 1 ASSIGNING <row>.
    
    ASSIGN COMPONENT `COL3_2` OF STRUCTURE <row> TO <field>.
    cl_abap_unit_assert=>assert_subrc( exp = 0 ).
    cl_abap_unit_assert=>assert_equals( exp = `z`
                                        act = <field> ).
  ENDMETHOD.

  METHOD csv_byte_order_mark.
    FIELD-SYMBOLS <lt_back> TYPE STANDARD TABLE.
    DATA temp599 TYPE xstring.
    DATA lv_bom TYPE string.
    DATA lr_back TYPE REF TO data.
    temp599 = `EFBBBF`.
    
    lv_bom = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp599 ).
    
    lr_back = zabaputil_cl_util_context=>itab_get_itab_by_csv( |{ lv_bom }NAME;CITY\r\nx;y\r\n| ).
    ASSIGN lr_back->* TO <lt_back>.
    cl_abap_unit_assert=>assert_equals( exp = `[{"city":"y","name":"x"}]`
                                        act = zabaputil_cl_util_context=>json_stringify( <lt_back> ) ).
  ENDMETHOD.

  METHOD csv_empty_input.
    DATA lr_back TYPE REF TO data.
    lr_back = zabaputil_cl_util_context=>itab_get_itab_by_csv( `` ).
    cl_abap_unit_assert=>assert_not_bound( lr_back ).
  ENDMETHOD.

  METHOD sql_like_escape.
    DATA temp600 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp601 LIKE LINE OF temp600.
    DATA temp98 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp99 LIKE LINE OF temp98.
    DATA lt_filter LIKE temp600.
    DATA lv_where TYPE string.
    DATA lt_back TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp602> LIKE LINE OF lt_back.
    DATA temp603 LIKE sy-tabix.
    FIELD-SYMBOLS <temp604> LIKE LINE OF lt_filter.
    DATA temp605 LIKE sy-tabix.
    FIELD-SYMBOLS <temp100> LIKE LINE OF lt_back.
    DATA temp101 LIKE sy-tabix.
    CLEAR temp600.
    
    temp601-name = `F`.
    
    CLEAR temp98.
    
    temp99-sign = `I`.
    temp99-option = `CP`.
    temp99-low = `A_50%*`.
    INSERT temp99 INTO TABLE temp98.
    temp601-t_range = temp98.
    INSERT temp601 INTO TABLE temp600.
    
    lt_filter = temp600.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ).
    
    lt_back = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( lv_where ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_back ) ).
    
    
    temp603 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp602>.
    sy-tabix = temp603.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `F` act = <temp602>-name ).
    
    
    temp605 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp604>.
    sy-tabix = temp605.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp101 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp100>.
    sy-tabix = temp101.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = <temp604>-t_range act = <temp100>-t_range ).
  ENDMETHOD.

  METHOD sql_like_cp_escape.
    " # makes the next CP character plain: #* is a star, ## a hash
    DATA temp606 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp607 LIKE LINE OF temp606.
    DATA temp102 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp103 LIKE LINE OF temp102.
    DATA lv_cond TYPE string.
    DATA temp43 TYPE xsdboolean.
    DATA temp608 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp609 LIKE LINE OF temp608.
    DATA temp104 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp105 LIKE LINE OF temp104.
    DATA lt_filter LIKE temp608.
    DATA lt_back TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp610> LIKE LINE OF lt_filter.
    DATA temp611 LIKE sy-tabix.
    FIELD-SYMBOLS <temp106> LIKE LINE OF lt_back.
    DATA temp107 LIKE sy-tabix.
    CLEAR temp606.
    
    temp607-name = `F`.
    
    CLEAR temp102.
    
    temp103-sign = `I`.
    temp103-option = `CP`.
    temp103-low = `A#*B##C*`.
    INSERT temp103 INTO TABLE temp102.
    temp607-t_range = temp102.
    INSERT temp607 INTO TABLE temp606.
    
    lv_cond = zabaputil_cl_util_context=>filter_get_sql_where( temp606 ).
    
    temp43 = boolc( lv_cond CS `LIKE 'A*B##C%' ESCAPE '#'` ).
    cl_abap_unit_assert=>assert_true( temp43 ).
    
    CLEAR temp608.
    
    temp609-name = `F`.
    
    CLEAR temp104.
    
    temp105-sign = `I`.
    temp105-option = `CP`.
    temp105-low = `A#*B##C+D*`.
    INSERT temp105 INTO TABLE temp104.
    temp609-t_range = temp104.
    INSERT temp609 INTO TABLE temp608.
    
    lt_filter = temp608.
    
    lt_back = zabaputil_cl_util_context=>filter_get_multi_by_sql_where(
                      zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ) ).
    
    
    temp611 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp610>.
    sy-tabix = temp611.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp107 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp106>.
    sy-tabix = temp107.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = <temp610>-t_range act = <temp106>-t_range ).
  ENDMETHOD.

  METHOD sql_lowercase_keywords.
    DATA lt_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp612> LIKE LINE OF lt_filter.
    DATA temp613 LIKE sy-tabix.
    FIELD-SYMBOLS <temp614> LIKE LINE OF lt_filter.
    DATA temp615 LIKE sy-tabix.
    FIELD-SYMBOLS <temp108> LIKE LINE OF <temp614>-t_range.
    DATA temp109 LIKE sy-tabix.
    FIELD-SYMBOLS <temp616> LIKE LINE OF lt_filter.
    DATA temp617 LIKE sy-tabix.
    FIELD-SYMBOLS <temp618> LIKE LINE OF lt_filter.
    DATA temp619 LIKE sy-tabix.
    FIELD-SYMBOLS <temp110> LIKE LINE OF <temp618>-t_range.
    DATA temp111 LIKE sy-tabix.
    FIELD-SYMBOLS <temp620> LIKE LINE OF lt_filter.
    DATA temp621 LIKE sy-tabix.
    FIELD-SYMBOLS <temp112> LIKE LINE OF <temp620>-t_range.
    DATA temp113 LIKE sy-tabix.
    FIELD-SYMBOLS <temp622> LIKE LINE OF lt_filter.
    DATA temp623 LIKE sy-tabix.
    FIELD-SYMBOLS <temp114> LIKE LINE OF <temp622>-t_range.
    DATA temp115 LIKE sy-tabix.
    FIELD-SYMBOLS <temp624> LIKE LINE OF lt_filter.
    DATA temp625 LIKE sy-tabix.
    FIELD-SYMBOLS <temp116> LIKE LINE OF <temp624>-t_range.
    DATA temp117 LIKE sy-tabix.
    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where(
                        `a = '1' and b like 'X%' and c between '1' and '5'` ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lt_filter ) ).
    
    
    temp613 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp612>.
    sy-tabix = temp613.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a` act = <temp612>-name ).
    
    
    temp615 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp614>.
    sy-tabix = temp615.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp109 = sy-tabix.
    READ TABLE <temp614>-t_range INDEX 1 ASSIGNING <temp108>.
    sy-tabix = temp109.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = <temp108>-low ).
    
    
    temp617 = sy-tabix.
    READ TABLE lt_filter INDEX 2 ASSIGNING <temp616>.
    sy-tabix = temp617.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `b` act = <temp616>-name ).
    
    
    temp619 = sy-tabix.
    READ TABLE lt_filter INDEX 2 ASSIGNING <temp618>.
    sy-tabix = temp619.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp111 = sy-tabix.
    READ TABLE <temp618>-t_range INDEX 1 ASSIGNING <temp110>.
    sy-tabix = temp111.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `CP` act = <temp110>-option ).
    
    
    temp621 = sy-tabix.
    READ TABLE lt_filter INDEX 2 ASSIGNING <temp620>.
    sy-tabix = temp621.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp113 = sy-tabix.
    READ TABLE <temp620>-t_range INDEX 1 ASSIGNING <temp112>.
    sy-tabix = temp113.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `X*` act = <temp112>-low ).
    
    
    temp623 = sy-tabix.
    READ TABLE lt_filter INDEX 3 ASSIGNING <temp622>.
    sy-tabix = temp623.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp115 = sy-tabix.
    READ TABLE <temp622>-t_range INDEX 1 ASSIGNING <temp114>.
    sy-tabix = temp115.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `BT` act = <temp114>-option ).
    
    
    temp625 = sy-tabix.
    READ TABLE lt_filter INDEX 3 ASSIGNING <temp624>.
    sy-tabix = temp625.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp117 = sy-tabix.
    READ TABLE <temp624>-t_range INDEX 1 ASSIGNING <temp116>.
    sy-tabix = temp117.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `5` act = <temp116>-high ).
  ENDMETHOD.

  METHOD sql_keyword_in_literal.
    DATA lt_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp626> LIKE LINE OF lt_filter.
    DATA temp627 LIKE sy-tabix.
    FIELD-SYMBOLS <temp628> LIKE LINE OF lt_filter.
    DATA temp629 LIKE sy-tabix.
    FIELD-SYMBOLS <temp118> LIKE LINE OF <temp628>-t_range.
    DATA temp119 LIKE sy-tabix.
    FIELD-SYMBOLS <temp630> LIKE LINE OF lt_filter.
    DATA temp631 LIKE sy-tabix.
    FIELD-SYMBOLS <temp120> LIKE LINE OF <temp630>-t_range.
    DATA temp121 LIKE sy-tabix.
    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `TEXT = 'A LIKE B <> C'` ).
    
    
    temp627 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp626>.
    sy-tabix = temp627.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `TEXT` act = <temp626>-name ).
    
    
    temp629 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp628>.
    sy-tabix = temp629.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp119 = sy-tabix.
    READ TABLE <temp628>-t_range INDEX 1 ASSIGNING <temp118>.
    sy-tabix = temp119.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `EQ` act = <temp118>-option ).
    
    
    temp631 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp630>.
    sy-tabix = temp631.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp121 = sy-tabix.
    READ TABLE <temp630>-t_range INDEX 1 ASSIGNING <temp120>.
    sy-tabix = temp121.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A LIKE B <> C` act = <temp120>-low ).
  ENDMETHOD.

  METHOD sql_exclude_roundtrip.
    DATA temp632 TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA temp633 LIKE LINE OF temp632.
    DATA temp122 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp123 LIKE LINE OF temp122.
    DATA temp124 TYPE zabaputil_cl_util_context=>ty_t_range.
    DATA temp125 LIKE LINE OF temp124.
    DATA lt_filter LIKE temp632.
    DATA lv_where TYPE string.
    DATA lt_back TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp634> LIKE LINE OF lt_filter.
    DATA temp635 LIKE sy-tabix.
    FIELD-SYMBOLS <temp126> LIKE LINE OF lt_back.
    DATA temp127 LIKE sy-tabix.
    FIELD-SYMBOLS <temp636> LIKE LINE OF lt_filter.
    DATA temp637 LIKE sy-tabix.
    FIELD-SYMBOLS <temp128> LIKE LINE OF lt_back.
    DATA temp129 LIKE sy-tabix.
    CLEAR temp632.
    
    temp633-name = `F`.
    
    CLEAR temp122.
    
    CLEAR temp123.
    temp123-sign = `I`.
    temp123-option = `CP`.
    temp123-low = `A*`.
    INSERT temp123 INTO TABLE temp122.
    CLEAR temp123.
    temp123-sign = `I`.
    temp123-option = `EQ`.
    temp123-low = `B`.
    INSERT temp123 INTO TABLE temp122.
    CLEAR temp123.
    temp123-sign = `E`.
    temp123-option = `EQ`.
    temp123-low = `AB`.
    INSERT temp123 INTO TABLE temp122.
    CLEAR temp123.
    temp123-sign = `E`.
    temp123-option = `BT`.
    temp123-low = `AC`.
    temp123-high = `AD`.
    INSERT temp123 INTO TABLE temp122.
    temp633-t_range = temp122.
    INSERT temp633 INTO TABLE temp632.
    temp633-name = `G`.
    
    CLEAR temp124.
    
    temp125-sign = `E`.
    temp125-option = `EQ`.
    temp125-low = `1`.
    INSERT temp125 INTO TABLE temp124.
    temp125-sign = `E`.
    temp125-option = `EQ`.
    temp125-low = `2`.
    INSERT temp125 INTO TABLE temp124.
    temp633-t_range = temp124.
    INSERT temp633 INTO TABLE temp632.
    
    lt_filter = temp632.
    
    lv_where = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ).
    
    lt_back = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( lv_where ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_back ) ).
    
    
    temp635 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp634>.
    sy-tabix = temp635.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp127 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp126>.
    sy-tabix = temp127.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = <temp634>-t_range act = <temp126>-t_range ).
    
    
    temp637 = sy-tabix.
    READ TABLE lt_filter INDEX 2 ASSIGNING <temp636>.
    sy-tabix = temp637.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp129 = sy-tabix.
    READ TABLE lt_back INDEX 2 ASSIGNING <temp128>.
    sy-tabix = temp129.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = <temp636>-t_range act = <temp128>-t_range ).
  ENDMETHOD.

  METHOD sql_groups_keep_parens.
    DATA lt_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    FIELD-SYMBOLS <temp638> LIKE LINE OF lt_filter.
    DATA temp639 LIKE sy-tabix.
    FIELD-SYMBOLS <temp640> LIKE LINE OF lt_filter.
    DATA temp641 LIKE sy-tabix.
    FIELD-SYMBOLS <temp642> LIKE LINE OF lt_filter.
    DATA temp643 LIKE sy-tabix.
    FIELD-SYMBOLS <temp130> LIKE LINE OF <temp642>-t_range.
    DATA temp131 LIKE sy-tabix.
    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `( A = '1' ) OR ( A = '2' )` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_filter ) ).
    
    
    temp639 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp638>.
    sy-tabix = temp639.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A` act = <temp638>-name ).
    
    
    temp641 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp640>.
    sy-tabix = temp641.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <temp640>-t_range ) ).
    
    
    temp643 = sy-tabix.
    READ TABLE lt_filter INDEX 1 ASSIGNING <temp642>.
    sy-tabix = temp643.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp131 = sy-tabix.
    READ TABLE <temp642>-t_range INDEX 2 ASSIGNING <temp130>.
    sy-tabix = temp131.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `2` act = <temp130>-low ).
  ENDMETHOD.

  METHOD sql_roundtrip_kept.
    " each WHERE comes back from its filters as a WHERE of the same rows
    DATA lt_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
    DATA ls_sql TYPE zabaputil_cl_util_context=>ty_s_sql.
    FIELD-SYMBOLS <temp644> LIKE LINE OF ls_sql-t_filter.
    DATA temp645 LIKE sy-tabix.
    FIELD-SYMBOLS <temp132> LIKE LINE OF <temp644>-t_range.
    DATA temp133 LIKE sy-tabix.
    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `A IN ('1','2') AND B = '3'` ).
    cl_abap_unit_assert=>assert_equals( exp = `( A = '1' OR A = '2' ) AND ( B = '3' )`
                                        act = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ) ).

    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `A='1' AND B >= '3'` ).
    cl_abap_unit_assert=>assert_equals( exp = `( A = '1' ) AND ( B >= '3' )`
                                        act = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ) ).

    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `ZBETWEEN = 'X' AND F = 'Y'` ).
    cl_abap_unit_assert=>assert_equals( exp = `( ZBETWEEN = 'X' ) AND ( F = 'Y' )`
                                        act = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ) ).

    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `A NOT IN ( '1', '2' )` ).
    cl_abap_unit_assert=>assert_equals( exp = `( A <> '1' AND A <> '2' )`
                                        act = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ) ).

    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `( A = '1' OR A = '2' ) AND ( A = '3' OR A = '4' )` ).
    cl_abap_unit_assert=>assert_equals( exp = `( A = '1' OR A = '2' ) AND ( A = '3' OR A = '4' )`
                                        act = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ) ).

    lt_filter = zabaputil_cl_util_context=>filter_get_multi_by_sql_where( `( A = '1' AND A <> '2' )` ).
    cl_abap_unit_assert=>assert_equals( exp = `( A = '1' AND A <> '2' )`
                                        act = zabaputil_cl_util_context=>filter_get_sql_where( lt_filter ) ).

    
    ls_sql = zabaputil_cl_util_context=>filter_get_sql_by_sql_string( `SELECT * FROM ztab WHERE a = '1' ORDER BY b` ).
    cl_abap_unit_assert=>assert_equals( exp = `a = '1'` act = ls_sql-where ).
    
    
    temp645 = sy-tabix.
    READ TABLE ls_sql-t_filter INDEX 1 ASSIGNING <temp644>.
    sy-tabix = temp645.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    
    temp133 = sy-tabix.
    READ TABLE <temp644>-t_range INDEX 1 ASSIGNING <temp132>.
    sy-tabix = temp133.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1` act = <temp132>-low ).
  ENDMETHOD.

  METHOD sql_not_a_filter.
    " a condition a filter cannot say raises - it was dropped or rewritten
    DATA temp646 TYPE string_table.
    DATA lt_where LIKE temp646.
    DATA lv_where LIKE LINE OF lt_where.
    CLEAR temp646.
    INSERT `A = '1' OR B = '2'` INTO TABLE temp646.
    INSERT `NOT A = '1' AND B = '2'` INTO TABLE temp646.
    INSERT `( A = '1' OR A IS NULL ) AND B = '1'` INTO TABLE temp646.
    INSERT `( A = '1' AND B = '2' ) OR C = '3'` INTO TABLE temp646.
    INSERT `A = '1' OR A = '2' AND A = '3'` INTO TABLE temp646.
    INSERT `A = '1' ORDER BY b` INTO TABLE temp646.
    INSERT `A IN ( SELECT b FROM c )` INTO TABLE temp646.
    INSERT `A NOT IN ('1') OR A = '2'` INTO TABLE temp646.
    INSERT `A = 'open` INTO TABLE temp646.
    
    lt_where = temp646.
    
    LOOP AT lt_where INTO lv_where.
      TRY.
          zabaputil_cl_util_context=>filter_get_multi_by_sql_where( lv_where ).
          cl_abap_unit_assert=>fail( |no exception for { lv_where }| ).
        CATCH zabaputil_cx_util_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD sql_tabname_with_keyword.
    DATA ls_sql TYPE zabaputil_cl_util_context=>ty_s_sql.
    ls_sql = zabaputil_cl_util_context=>filter_get_sql_by_sql_string( `SELECT FROM zmy_fields WHERE a = '1'` ).
    cl_abap_unit_assert=>assert_equals( exp = `ZMY_FIELDS` act = ls_sql-tabname ).
    ls_sql = zabaputil_cl_util_context=>filter_get_sql_by_sql_string( `select from zwhereused` ).
    cl_abap_unit_assert=>assert_equals( exp = `ZWHEREUSED` act = ls_sql-tabname ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_file_encoding DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS file_utf8         FOR TESTING.
    METHODS file_windows_1252 FOR TESTING.
    METHODS rap_cause_text    FOR TESTING.

ENDCLASS.

CLASS zabaputil_cl_util_context DEFINITION LOCAL FRIENDS ltcl_file_encoding.

CLASS ltcl_file_encoding IMPLEMENTATION.

  METHOD file_utf8.
    " H + a-umlaut in UTF-8 - hex, as a literal umlaut breaks the 7-bit rule
    DATA temp648 TYPE xstring.
    DATA lv_text TYPE string.
    DATA temp649 TYPE xstring.
    temp648 = `48C3A4`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_file( temp648 ).
    
    temp649 = `48C3A4`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp649
        act = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_text ) ).
  ENDMETHOD.

  METHOD file_windows_1252.
    " H + a-umlaut as Excel's plain "CSV" writes it on Windows: E4 alone is not UTF-8
    DATA temp650 TYPE xstring.
    DATA lv_text TYPE string.
    DATA temp651 TYPE xstring.
    temp650 = `48E4`.
    
    lv_text = zabaputil_cl_util_context=>conv_get_string_by_file( temp650 ).
    
    temp651 = `48C3A4`.
    cl_abap_unit_assert=>assert_equals(
        exp = temp651
        act = zabaputil_cl_util_context=>conv_get_xstring_by_string( lv_text ) ).
  ENDMETHOD.

  METHOD rap_cause_text.
    " the base values of IF_ABAP_BEHV=>T_FAIL_CAUSE, not a running number
    cl_abap_unit_assert=>assert_equals( exp = 404
                                        act = zabaputil_cl_util_context=>msg_get_rap_cause_value( `not_found` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 4221
                                        act = zabaputil_cl_util_context=>msg_get_rap_cause_value( `DISABLED` ) ).
    cl_abap_unit_assert=>assert_equals( exp = -1
                                        act = zabaputil_cl_util_context=>msg_get_rap_cause_value( `SOMETHING_NEW` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Entity not found`
                                        act = zabaputil_cl_util_context=>msg_get_rap_fail_text( 404 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Entity is locked`
                                        act = zabaputil_cl_util_context=>msg_get_rap_fail_text( 423 ) ).
  ENDMETHOD.

ENDCLASS.

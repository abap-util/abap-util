CLASS ltcl_unit_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_info              FOR TESTING RAISING cx_static_check.
    METHODS test_error             FOR TESTING RAISING cx_static_check.
    METHODS test_warning           FOR TESTING RAISING cx_static_check.
    METHODS test_success           FOR TESTING RAISING cx_static_check.
    METHODS test_chaining          FOR TESTING RAISING cx_static_check.
    METHODS test_has_error         FOR TESTING RAISING cx_static_check.
    METHODS test_has_no_error      FOR TESTING RAISING cx_static_check.
    METHODS test_count             FOR TESTING RAISING cx_static_check.
    METHODS test_clear             FOR TESTING RAISING cx_static_check.
    METHODS test_to_msg            FOR TESTING RAISING cx_static_check.
    METHODS test_to_string         FOR TESTING RAISING cx_static_check.
    METHODS test_add_cx            FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD test_info.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->info( `Test info` ).

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = lo_log->to_msg( ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_msg ) ).
    DATA temp6 LIKE LINE OF lt_msg.
    DATA temp7 LIKE sy-tabix.
    temp7 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp6.
    sy-tabix = temp7.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `I` act = temp6-type ).
    DATA temp8 LIKE LINE OF lt_msg.
    DATA temp9 LIKE sy-tabix.
    temp9 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp8.
    sy-tabix = temp9.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Test info` act = temp8-text ).

  ENDMETHOD.

  METHOD test_error.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->error( `Something failed` ).

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = lo_log->to_msg( ).

    DATA temp10 LIKE LINE OF lt_msg.
    DATA temp11 LIKE sy-tabix.
    temp11 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp10.
    sy-tabix = temp11.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E` act = temp10-type ).

  ENDMETHOD.

  METHOD test_warning.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->warning( `Be careful` ).

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = lo_log->to_msg( ).

    DATA temp12 LIKE LINE OF lt_msg.
    DATA temp13 LIKE sy-tabix.
    temp13 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp12.
    sy-tabix = temp13.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `W` act = temp12-type ).

  ENDMETHOD.

  METHOD test_success.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->success( `All good` ).

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = lo_log->to_msg( ).

    DATA temp14 LIKE LINE OF lt_msg.
    DATA temp15 LIKE sy-tabix.
    temp15 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp14.
    sy-tabix = temp15.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `S` act = temp14-type ).

  ENDMETHOD.

  METHOD test_chaining.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->info( `Step 1`
      )->info( `Step 2`
      )->error( `Step 3` ).

    cl_abap_unit_assert=>assert_equals( exp = 3 act = lo_log->count( ) ).

  ENDMETHOD.

  METHOD test_has_error.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->info( `OK` )->error( `Fail` ).

    cl_abap_unit_assert=>assert_true( lo_log->has_error( ) ).

  ENDMETHOD.

  METHOD test_has_no_error.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->info( `OK` )->success( `Done` ).

    cl_abap_unit_assert=>assert_false( lo_log->has_error( ) ).

  ENDMETHOD.

  METHOD test_count.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_log->count( ) ).

    lo_log->info( `One` )->info( `Two` ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_log->count( ) ).

  ENDMETHOD.

  METHOD test_clear.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->info( `One` )->info( `Two` )->clear( ).

    cl_abap_unit_assert=>assert_equals( exp = 0 act = lo_log->count( ) ).

  ENDMETHOD.

  METHOD test_to_msg.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->info( `A` )->error( `B` ).

    DATA lt_msg TYPE zabaputil_cl_util=>ty_t_msg.
    lt_msg = lo_log->to_msg( ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_msg ) ).
    DATA temp16 LIKE LINE OF lt_msg.
    DATA temp17 LIKE sy-tabix.
    temp17 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp16.
    sy-tabix = temp17.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A` act = temp16-text ).
    DATA temp18 LIKE LINE OF lt_msg.
    DATA temp19 LIKE sy-tabix.
    temp19 = sy-tabix.
    READ TABLE lt_msg INDEX 2 INTO temp18.
    sy-tabix = temp19.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B` act = temp18-text ).

  ENDMETHOD.

  METHOD test_to_string.

    DATA lo_log TYPE REF TO zabaputil_cl_util_log.
    CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
    lo_log->info( `Hello` )->error( `World` ).

    DATA lv_str TYPE string.
    lv_str = lo_log->to_string( ).

    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( lv_str CS `[I] Hello` ).
    cl_abap_unit_assert=>assert_true( temp1 ).
    DATA temp2 TYPE xsdboolean.
    temp2 = boolc( lv_str CS `[E] World` ).
    cl_abap_unit_assert=>assert_true( temp2 ).

  ENDMETHOD.

  METHOD test_add_cx.

    TRY.
        DATA lv_val TYPE i.
        lv_val = 1 / 0.
        DATA lx TYPE REF TO cx_root.
      CATCH cx_root INTO lx.
        DATA lo_log TYPE REF TO zabaputil_cl_util_log.
        CREATE OBJECT lo_log TYPE zabaputil_cl_util_log.
        lo_log->add( lx ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_log->count( ) ).
    cl_abap_unit_assert=>assert_true( lo_log->has_error( ) ).

  ENDMETHOD.

ENDCLASS.

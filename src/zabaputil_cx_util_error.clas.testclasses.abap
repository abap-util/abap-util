CLASS ltcl_unit_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_raise           FOR TESTING RAISING cx_static_check.
    METHODS test_raise_empty     FOR TESTING RAISING cx_static_check.
    METHODS test_raise_with_prev FOR TESTING RAISING cx_static_check.
    METHODS test_raise_with_cx   FOR TESTING RAISING cx_static_check.
    METHODS test_uuid_populated  FOR TESTING RAISING cx_static_check.
    METHODS test_chain_texts     FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.
  METHOD test_raise.

    TRY.

        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = `this is an error text`.

        DATA lx TYPE REF TO zabaputil_cx_util_error.
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_equals( exp = `this is an error text`
                                            act = lx->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_raise_empty.

    TRY.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_bound( lx ).
        cl_abap_unit_assert=>assert_not_initial( lx->ms_error-uuid ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_raise_with_prev.

    DATA lx_prev TYPE REF TO zabaputil_cx_util_error.
    CREATE OBJECT lx_prev TYPE zabaputil_cx_util_error EXPORTING val = `previous error`.

    TRY.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val      = `current error`
                    previous = lx_prev.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
      CATCH zabaputil_cx_util_error INTO lx.
        DATA lv_text TYPE string.
        lv_text = lx->get_text( ).
        DATA temp1 TYPE xsdboolean.
        temp1 = boolc( lv_text CS `current error` ).
        cl_abap_unit_assert=>assert_true(
          temp1 ).
        DATA temp2 TYPE xsdboolean.
        temp2 = boolc( lv_text CS `previous error` ).
        cl_abap_unit_assert=>assert_true(
          temp2 ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_raise_with_cx.

    TRY.
        DATA lv_val TYPE i.
        lv_val = 1 / 0.
        DATA lx_root TYPE REF TO cx_root.
      CATCH cx_root INTO lx_root.
    ENDTRY.

    TRY.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = lx_root.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_not_initial( lx->get_text( ) ).
        cl_abap_unit_assert=>assert_bound( lx->ms_error-x_root ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_uuid_populated.

    TRY.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = `test`.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
      CATCH zabaputil_cx_util_error INTO lx.
        cl_abap_unit_assert=>assert_not_initial( lx->ms_error-uuid ).
        cl_abap_unit_assert=>assert_equals( exp = 32
                                            act = strlen( lx->ms_error-uuid ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_chain_texts.

    DATA lx_inner TYPE REF TO zabaputil_cx_util_error.
    CREATE OBJECT lx_inner TYPE zabaputil_cx_util_error EXPORTING val = `inner`.
    DATA lx_middle TYPE REF TO zabaputil_cx_util_error.
    CREATE OBJECT lx_middle TYPE zabaputil_cx_util_error EXPORTING val = `middle` previous = lx_inner.
    DATA lx_outer TYPE REF TO zabaputil_cx_util_error.
    CREATE OBJECT lx_outer TYPE zabaputil_cx_util_error EXPORTING val = `outer` previous = lx_middle.

    DATA lv_text TYPE string.
    lv_text = lx_outer->get_text( ).
    DATA temp3 TYPE xsdboolean.
    temp3 = boolc( lv_text CS `outer` ).
    cl_abap_unit_assert=>assert_true( temp3 ).
    DATA temp4 TYPE xsdboolean.
    temp4 = boolc( lv_text CS `middle` ).
    cl_abap_unit_assert=>assert_true( temp4 ).
    DATA temp5 TYPE xsdboolean.
    temp5 = boolc( lv_text CS `inner` ).
    cl_abap_unit_assert=>assert_true( temp5 ).

  ENDMETHOD.
ENDCLASS.

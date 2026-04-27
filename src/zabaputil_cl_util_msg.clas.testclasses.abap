CLASS ltcl_unit_test_msg_mapper DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_cx             FOR TESTING RAISING cx_static_check.
    METHODS test_bapiret        FOR TESTING RAISING cx_static_check.
    METHODS test_bapirettab     FOR TESTING RAISING cx_static_check.
    METHODS test_sy             FOR TESTING RAISING cx_static_check.
    METHODS test_get_text_cx    FOR TESTING RAISING cx_static_check.
    METHODS test_get_text_str   FOR TESTING RAISING cx_static_check.
    METHODS test_get_text_empty FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_unit_test_msg_mapper IMPLEMENTATION.

  METHOD test_sy.

    IF sy-sysid = `ABC`.
      RETURN.
    ENDIF.

    DATA lv_msg_id TYPE string.
    lv_msg_id = `NET`.
    DATA lv_msg_text TYPE string.
    MESSAGE ID lv_msg_id TYPE `I` NUMBER `001` INTO lv_msg_text ##NEEDED.
    DATA lt_result TYPE zabaputil_cl_util=>ty_t_msg.
    lt_result = zabaputil_cl_util_msg=>msg_get_by_sy( ).

    DATA temp8 LIKE LINE OF lt_result.
    DATA temp9 LIKE sy-tabix.
    temp9 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp8.
    sy-tabix = temp9.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NET`
                                        act = temp8-id ).

    DATA temp10 LIKE LINE OF lt_result.
    DATA temp11 LIKE sy-tabix.
    temp11 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp10.
    sy-tabix = temp11.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `001`
                                        act = temp10-no ).

    DATA temp12 LIKE LINE OF lt_result.
    DATA temp13 LIKE sy-tabix.
    temp13 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp12.
    sy-tabix = temp13.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `I`
                                        act = temp12-type ).

  ENDMETHOD.

  METHOD test_bapiret.

    IF sy-sysid = `ABC`.
      RETURN.
    ENDIF.

    DATA temp14 TYPE bapirettab.
    CLEAR temp14.
    DATA temp15 LIKE LINE OF temp14.
    temp15-type = `E`.
    temp15-id = `MSG1`.
    temp15-number = `001`.
    temp15-message = `An empty Report field causes an empty XML Message to be sent`.
    INSERT temp15 INTO TABLE temp14.
    DATA lt_msg LIKE temp14.
    lt_msg = temp14.

    DATA lt_result TYPE zabaputil_cl_util=>ty_t_msg.
    DATA temp1 LIKE LINE OF lt_msg.
    DATA temp2 LIKE sy-tabix.
    temp2 = sy-tabix.
    READ TABLE lt_msg INDEX 1 INTO temp1.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lt_result = zabaputil_cl_util_msg=>msg_get( temp1 ).

    DATA temp16 LIKE LINE OF lt_result.
    DATA temp17 LIKE sy-tabix.
    temp17 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp16.
    sy-tabix = temp17.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `MSG1`
                                        act = temp16-id ).

    DATA temp18 LIKE LINE OF lt_result.
    DATA temp19 LIKE sy-tabix.
    temp19 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp18.
    sy-tabix = temp19.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `001`
                                        act = temp18-no ).

    DATA temp20 LIKE LINE OF lt_result.
    DATA temp21 LIKE sy-tabix.
    temp21 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp20.
    sy-tabix = temp21.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = temp20-type ).

  ENDMETHOD.

  METHOD test_bapirettab.

    IF sy-sysid = `ABC`.
      RETURN.
    ENDIF.

    DATA temp22 TYPE bapirettab.
    CLEAR temp22.
    DATA temp23 LIKE LINE OF temp22.
    temp23-type = `E`.
    temp23-id = `MSG1`.
    temp23-number = `001`.
    temp23-message = `An empty Report field causes an empty XML Message to be sent`.
    INSERT temp23 INTO TABLE temp22.
    temp23-type = `I`.
    temp23-id = `MSG2`.
    temp23-number = `002`.
    temp23-message = `Product already in use`.
    INSERT temp23 INTO TABLE temp22.
    DATA lt_msg LIKE temp22.
    lt_msg = temp22.

    DATA lt_result TYPE zabaputil_cl_util=>ty_t_msg.
    lt_result = zabaputil_cl_util_msg=>msg_get( lt_msg ).

    DATA temp24 LIKE LINE OF lt_result.
    DATA temp25 LIKE sy-tabix.
    temp25 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp24.
    sy-tabix = temp25.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `MSG1`
                                        act = temp24-id ).

    DATA temp26 LIKE LINE OF lt_result.
    DATA temp27 LIKE sy-tabix.
    temp27 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp26.
    sy-tabix = temp27.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `001`
                                        act = temp26-no ).

    DATA temp28 LIKE LINE OF lt_result.
    DATA temp29 LIKE sy-tabix.
    temp29 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp28.
    sy-tabix = temp29.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = temp28-type ).

  ENDMETHOD.

  METHOD test_cx.

    TRY.
        DATA lv_val TYPE i.
        lv_val = 1 / 0.
        DATA lx TYPE REF TO cx_root.
      CATCH cx_root INTO lx.
        DATA lt_result TYPE zabaputil_cl_util=>ty_t_msg.
        lt_result = zabaputil_cl_util_msg=>msg_get( lx ).
    ENDTRY.

    DATA temp30 LIKE LINE OF lt_result.
    DATA temp31 LIKE sy-tabix.
    temp31 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp30.
    sy-tabix = temp31.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `E`
                                        act = temp30-type ).

  ENDMETHOD.

  METHOD test_get_text_cx.

    TRY.
        DATA lv_val TYPE i.
        lv_val = 1 / 0.
        DATA lx TYPE REF TO cx_root.
      CATCH cx_root INTO lx.
        DATA lv_text TYPE string.
        lv_text = zabaputil_cl_util_msg=>msg_get_text( lx ).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_initial( lv_text ).

  ENDMETHOD.

  METHOD test_get_text_str.

    DATA lv_text TYPE string.
    lv_text = zabaputil_cl_util_msg=>msg_get_text( `Hello World` ).

    cl_abap_unit_assert=>assert_equals( exp = `Hello World` act = lv_text ).

  ENDMETHOD.

  METHOD test_get_text_empty.

    DATA ls_empty TYPE zabaputil_cl_util=>ty_s_msg.
    DATA lv_text TYPE string.
    lv_text = zabaputil_cl_util_msg=>msg_get_text( ls_empty ).

    cl_abap_unit_assert=>assert_initial( lv_text ).

  ENDMETHOD.

ENDCLASS.

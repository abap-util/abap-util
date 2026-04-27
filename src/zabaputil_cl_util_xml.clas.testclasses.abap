CLASS ltcl_unit_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION MEDIUM.

  PRIVATE SECTION.
    METHODS test_factory           FOR TESTING RAISING cx_static_check.
    METHODS test_factory_popup     FOR TESTING RAISING cx_static_check.
    METHODS test_shell_page        FOR TESTING RAISING cx_static_check.
    METHODS test_button            FOR TESTING RAISING cx_static_check.
    METHODS test_leaf              FOR TESTING RAISING cx_static_check.
    METHODS test_nested            FOR TESTING RAISING cx_static_check.
    METHODS test_nav_up            FOR TESTING RAISING cx_static_check.
    METHODS test_nav_named         FOR TESTING RAISING cx_static_check.
    METHODS test_nav_not_found     FOR TESTING RAISING cx_static_check.
    METHODS test_nav_prev          FOR TESTING RAISING cx_static_check.
    METHODS test_nav_root          FOR TESTING RAISING cx_static_check.
    METHODS test_namespace         FOR TESTING RAISING cx_static_check.
    METHODS test_table             FOR TESTING RAISING cx_static_check.
    METHODS test_simple_form       FOR TESTING RAISING cx_static_check.
    METHODS test_preferred_param   FOR TESTING RAISING cx_static_check.
    METHODS test_shortcut_av       FOR TESTING RAISING cx_static_check.
    METHODS test_stringify_subnode FOR TESTING RAISING cx_static_check.
    METHODS test_p                 FOR TESTING RAISING cx_static_check.
    METHODS test_if_true           FOR TESTING RAISING cx_static_check.
    METHODS test_if_false          FOR TESTING RAISING cx_static_check.
    METHODS test_leaf_if_true      FOR TESTING RAISING cx_static_check.
    METHODS test_leaf_if_false     FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_page_sample   FOR TESTING RAISING cx_static_check.
    METHODS test_ui5_page_sample_indent FOR TESTING RAISING cx_static_check.

    METHODS build_page_view
      RETURNING VALUE(result) TYPE REF TO zabaputil_cl_util_xml.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD test_factory.

    DATA lo_view TYPE REF TO zabaputil_cl_util_xml.
    lo_view = zabaputil_cl_util_xml=>factory( )->__( `View` ).
    DATA temp17 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp17.
    DATA temp18 LIKE LINE OF temp17.
    temp18-n = `title`.
    temp18-v = `test`.
    INSERT temp18 INTO TABLE temp17.
    DATA lv_xml TYPE string.
    lv_xml = lo_view->__( n = `Page`
                                p = temp17
                              )->stringify( ).

    cl_abap_unit_assert=>assert_not_initial( lv_xml ).
    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( lv_xml CS `Page` ).
    cl_abap_unit_assert=>assert_true( temp1 ).

  ENDMETHOD.

  METHOD test_factory_popup.

    DATA temp19 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp19.
    DATA temp20 LIKE LINE OF temp19.
    temp20-n = `xmlns`.
    temp20-v = `sap.m`.
    INSERT temp20 INTO TABLE temp19.
    temp20-n = `xmlns:core`.
    temp20-v = `sap.ui.core`.
    INSERT temp20 INTO TABLE temp19.
    DATA lo_popup TYPE REF TO zabaputil_cl_util_xml.
    lo_popup = zabaputil_cl_util_xml=>factory( )->__( n = `FragmentDefinition` ns = `core`
                       p = temp19 ).
    DATA temp21 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp21.
    DATA temp22 LIKE LINE OF temp21.
    temp22-n = `title`.
    temp22-v = `Test`.
    INSERT temp22 INTO TABLE temp21.
    DATA lv_xml TYPE string.
    lv_xml = lo_popup->__( n = `Dialog`
                                 p = temp21
                               )->stringify( ).

    cl_abap_unit_assert=>assert_not_initial( lv_xml ).
    DATA temp2 TYPE xsdboolean.
    temp2 = boolc( lv_xml CS `Dialog` ).
    cl_abap_unit_assert=>assert_true( temp2 ).
    DATA temp3 TYPE xsdboolean.
    temp3 = boolc( lv_xml CS `core:FragmentDefinition` ).
    cl_abap_unit_assert=>assert_true( temp3 ).

  ENDMETHOD.

  METHOD test_shell_page.

    DATA temp23 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp23.
    DATA temp24 LIKE LINE OF temp23.
    temp24-n = `title`.
    temp24-v = `My Page`.
    INSERT temp24 INTO TABLE temp23.
    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Shell`
      )->__( n = `Page`
            p = temp23
      )->stringify( ).

    DATA temp4 TYPE xsdboolean.
    temp4 = boolc( lv_xml CS `Shell` ).
    cl_abap_unit_assert=>assert_true( temp4 ).
    DATA temp5 TYPE xsdboolean.
    temp5 = boolc( lv_xml CS `Page` ).
    cl_abap_unit_assert=>assert_true( temp5 ).
    DATA temp6 TYPE xsdboolean.
    temp6 = boolc( lv_xml CS `My Page` ).
    cl_abap_unit_assert=>assert_true( temp6 ).

  ENDMETHOD.

  METHOD test_button.

    DATA temp25 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp25.
    DATA temp26 LIKE LINE OF temp25.
    temp26-n = `text`.
    temp26-v = `Click Me`.
    INSERT temp26 INTO TABLE temp25.
    temp26-n = `press`.
    temp26-v = `onPress`.
    INSERT temp26 INTO TABLE temp25.
    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( n = `Page` a = `title` v = `Test`
      )->_( n = `Button`
             p = temp25
      )->stringify( ).

    DATA temp7 TYPE xsdboolean.
    temp7 = boolc( lv_xml CS `Button` ).
    cl_abap_unit_assert=>assert_true( temp7 ).
    DATA temp8 TYPE xsdboolean.
    temp8 = boolc( lv_xml CS `Click Me` ).
    cl_abap_unit_assert=>assert_true( temp8 ).

  ENDMETHOD.

  METHOD test_leaf.

    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( n = `Page` a = `title` v = `Test` ).

    lo_page->_( n = `Button` a = `text` v = `Btn1` ).
    lo_page->_( n = `Button` a = `text` v = `Btn2` ).

    DATA lv_xml TYPE string.
    lv_xml = lo_page->stringify( ).

    DATA temp9 TYPE xsdboolean.
    temp9 = boolc( lv_xml CS `Btn1` ).
    cl_abap_unit_assert=>assert_true( temp9 ).
    DATA temp10 TYPE xsdboolean.
    temp10 = boolc( lv_xml CS `Btn2` ).
    cl_abap_unit_assert=>assert_true( temp10 ).

  ENDMETHOD.

  METHOD test_nested.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Shell`
      )->__( n = `Page` a = `title` v = `Test`
      )->__( `VBox`
      )->__( `HBox`
      )->_( n = `Text` a = `text` v = `Nested`
      )->stringify( ).

    DATA temp11 TYPE xsdboolean.
    temp11 = boolc( lv_xml CS `VBox` ).
    cl_abap_unit_assert=>assert_true( temp11 ).
    DATA temp12 TYPE xsdboolean.
    temp12 = boolc( lv_xml CS `HBox` ).
    cl_abap_unit_assert=>assert_true( temp12 ).
    DATA temp13 TYPE xsdboolean.
    temp13 = boolc( lv_xml CS `Nested` ).
    cl_abap_unit_assert=>assert_true( temp13 ).

  ENDMETHOD.

  METHOD test_nav_up.

    DATA lo_root TYPE REF TO zabaputil_cl_util_xml.
    lo_root = zabaputil_cl_util_xml=>factory( ).
    DATA lo_view TYPE REF TO zabaputil_cl_util_xml.
    lo_view = lo_root->__( n = `View` ns = `mvc` ).
    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = lo_view->__( n = `Page` a = `title` v = `Test` ).
    DATA lo_vbox TYPE REF TO zabaputil_cl_util_xml.
    lo_vbox = lo_page->__( `VBox` ).
    DATA lo_parent TYPE REF TO zabaputil_cl_util_xml.
    lo_parent = lo_vbox->n( ).

    cl_abap_unit_assert=>assert_equals( exp = lo_page act = lo_parent ).

  ENDMETHOD.

  METHOD test_nav_named.

    DATA lo_root TYPE REF TO zabaputil_cl_util_xml.
    lo_root = zabaputil_cl_util_xml=>factory( ).
    DATA lo_view TYPE REF TO zabaputil_cl_util_xml.
    lo_view = lo_root->__( n = `View` ns = `mvc` ).
    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = lo_view->__( n = `Page` a = `title` v = `Test` ).
    lo_page->__( `VBox`
      )->__( `HBox` ).

    DATA lo_result TYPE REF TO zabaputil_cl_util_xml.
    lo_result = lo_page->__( `VBox`
      )->__( `HBox`
      )->n( `Page` ).

    cl_abap_unit_assert=>assert_equals( exp = lo_page act = lo_result ).

  ENDMETHOD.

  METHOD test_nav_not_found.

    DATA lo_root TYPE REF TO zabaputil_cl_util_xml.
    lo_root = zabaputil_cl_util_xml=>factory( ).
    DATA lo_view TYPE REF TO zabaputil_cl_util_xml.
    lo_view = lo_root->__( n = `View` ns = `mvc` ).
    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = lo_view->__( n = `Page` a = `title` v = `Test` ).

    DATA lo_result TYPE REF TO zabaputil_cl_util_xml.
    lo_result = lo_page->__( `VBox`
      )->__( `HBox`
      )->n( `NotExisting` ).

    cl_abap_unit_assert=>assert_bound( lo_result ).

  ENDMETHOD.

  METHOD test_nav_prev.

    DATA lo_root TYPE REF TO zabaputil_cl_util_xml.
    lo_root = zabaputil_cl_util_xml=>factory( ).
    DATA lo_view TYPE REF TO zabaputil_cl_util_xml.
    lo_view = lo_root->__( n = `View` ns = `mvc` ).
    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = lo_view->__( n = `Page` a = `title` v = `Test` ).
    lo_page->_( n = `Button` a = `text` v = `Btn` ).

    DATA lo_prev TYPE REF TO zabaputil_cl_util_xml.
    lo_prev = lo_root->n_prev( ).

    cl_abap_unit_assert=>assert_bound( lo_prev ).

  ENDMETHOD.

  METHOD test_nav_root.

    DATA lo_root TYPE REF TO zabaputil_cl_util_xml.
    lo_root = zabaputil_cl_util_xml=>factory( ).
    DATA lo_deep TYPE REF TO zabaputil_cl_util_xml.
    lo_deep = lo_root->__( n = `View` ns = `mvc`
      )->__( `Shell`
      )->__( `Page`
      )->__( `VBox` ).

    DATA lo_result TYPE REF TO zabaputil_cl_util_xml.
    lo_result = lo_deep->n_root( ).

    cl_abap_unit_assert=>assert_equals( exp = lo_root act = lo_result ).

  ENDMETHOD.

  METHOD test_namespace.

    DATA temp27 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp27.
    DATA temp28 LIKE LINE OF temp27.
    temp28-n = `xmlns:f`.
    temp28-v = `sap.f`.
    INSERT temp28 INTO TABLE temp27.
    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
            p = temp27
      )->__( n  = `DynamicPage`
            ns = `f`
      )->__( n  = `DynamicPageTitle`
            ns = `f`
      )->stringify( ).

    DATA temp14 TYPE xsdboolean.
    temp14 = boolc( lv_xml CS `f:DynamicPage` ).
    cl_abap_unit_assert=>assert_true( temp14 ).
    DATA temp15 TYPE xsdboolean.
    temp15 = boolc( lv_xml CS `f:DynamicPageTitle` ).
    cl_abap_unit_assert=>assert_true( temp15 ).
    DATA temp16 TYPE xsdboolean.
    temp16 = boolc( lv_xml CS `xmlns:f` ).
    cl_abap_unit_assert=>assert_true( temp16 ).

  ENDMETHOD.

  METHOD test_table.

    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( n = `Page` a = `title` v = `Test` ).

    DATA temp29 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp29.
    DATA temp30 LIKE LINE OF temp29.
    temp30-n = `items`.
    temp30-v = `{/ITEMS}`.
    INSERT temp30 INTO TABLE temp29.
    temp30-n = `headerText`.
    temp30-v = `My Table`.
    INSERT temp30 INTO TABLE temp29.
    DATA lo_table TYPE REF TO zabaputil_cl_util_xml.
    lo_table = lo_page->__( n = `Table`
                                  p = temp29 ).

    lo_table->__( `columns`
      )->__( `Column`
      )->__( `header`
      )->_( n = `Text` a = `text` v = `Col1` ).

    lo_table->__( `items`
      )->__( `ColumnListItem`
      )->__( `cells`
      )->_( n = `Text` a = `text` v = `{COL1}` ).

    DATA lv_xml TYPE string.
    lv_xml = lo_page->stringify( ).

    DATA temp17 TYPE xsdboolean.
    temp17 = boolc( lv_xml CS `Table` ).
    cl_abap_unit_assert=>assert_true( temp17 ).
    DATA temp18 TYPE xsdboolean.
    temp18 = boolc( lv_xml CS `My Table` ).
    cl_abap_unit_assert=>assert_true( temp18 ).
    DATA temp19 TYPE xsdboolean.
    temp19 = boolc( lv_xml CS `columns` ).
    cl_abap_unit_assert=>assert_true( temp19 ).
    DATA temp20 TYPE xsdboolean.
    temp20 = boolc( lv_xml CS `ColumnListItem` ).
    cl_abap_unit_assert=>assert_true( temp20 ).

  ENDMETHOD.

  METHOD test_simple_form.

    DATA temp31 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp31.
    DATA temp32 LIKE LINE OF temp31.
    temp32-n = `xmlns:form`.
    temp32-v = `sap.ui.layout.form`.
    INSERT temp32 INTO TABLE temp31.
    DATA lo_form TYPE REF TO zabaputil_cl_util_xml.
    lo_form = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
            p = temp31
      )->__( n = `Page` a = `title` v = `Test`
      )->__( n  = `SimpleForm`
            ns = `form`
            a  = `editable`
            v  = `true` ).

    lo_form->__( n  = `content`
                ns = `form`
      )->_( n = `Label` a = `text` v = `Name`
      )->_( n = `Input` a = `value` v = `{/NAME}` ).

    DATA lv_xml TYPE string.
    lv_xml = lo_form->stringify( ).

    DATA temp21 TYPE xsdboolean.
    temp21 = boolc( lv_xml CS `SimpleForm` ).
    cl_abap_unit_assert=>assert_true( temp21 ).
    DATA temp22 TYPE xsdboolean.
    temp22 = boolc( lv_xml CS `Label` ).
    cl_abap_unit_assert=>assert_true( temp22 ).
    DATA temp23 TYPE xsdboolean.
    temp23 = boolc( lv_xml CS `Input` ).
    cl_abap_unit_assert=>assert_true( temp23 ).

  ENDMETHOD.

  METHOD test_preferred_param.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Shell`
      )->__( `Page`
      )->_( `Button`
      )->stringify( ).

    DATA temp24 TYPE xsdboolean.
    temp24 = boolc( lv_xml CS `Shell` ).
    cl_abap_unit_assert=>assert_true( temp24 ).
    DATA temp25 TYPE xsdboolean.
    temp25 = boolc( lv_xml CS `Page` ).
    cl_abap_unit_assert=>assert_true( temp25 ).
    DATA temp26 TYPE xsdboolean.
    temp26 = boolc( lv_xml CS `Button` ).
    cl_abap_unit_assert=>assert_true( temp26 ).

  ENDMETHOD.

  METHOD test_shortcut_av.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Shell`
      )->_( n = `Button` a = `text` v = `OK`
      )->stringify( ).

    DATA temp27 TYPE xsdboolean.
    temp27 = boolc( lv_xml CS `Button` ).
    cl_abap_unit_assert=>assert_true( temp27 ).
    DATA temp28 TYPE xsdboolean.
    temp28 = boolc( lv_xml CS `text="OK"` ).
    cl_abap_unit_assert=>assert_true( temp28 ).

  ENDMETHOD.

  METHOD test_stringify_subnode.

    DATA lo_root TYPE REF TO zabaputil_cl_util_xml.
    lo_root = zabaputil_cl_util_xml=>factory( ).
    DATA lo_view TYPE REF TO zabaputil_cl_util_xml.
    lo_view = lo_root->__( n = `View` ns = `mvc` ).
    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = lo_view->__( n = `Page` a = `title` v = `Test` ).
    lo_page->_( n = `Button` a = `text` v = `Click` ).

    DATA lv_full TYPE string.
    lv_full = lo_page->stringify( ).
    DATA lv_sub TYPE string.
    lv_sub  = lo_page->stringify( from_root = abap_false ).

    DATA temp29 TYPE xsdboolean.
    temp29 = boolc( lv_full CS `mvc:View` ).
    cl_abap_unit_assert=>assert_true( temp29 ).
    DATA temp30 TYPE xsdboolean.
    temp30 = boolc( lv_sub CS `mvc:View` ).
    cl_abap_unit_assert=>assert_false( temp30 ).
    DATA temp31 TYPE xsdboolean.
    temp31 = boolc( lv_sub CS `Page` ).
    cl_abap_unit_assert=>assert_true( temp31 ).
    DATA temp32 TYPE xsdboolean.
    temp32 = boolc( lv_sub CS `Button` ).
    cl_abap_unit_assert=>assert_true( temp32 ).

  ENDMETHOD.

  METHOD test_p.

    DATA lo_root TYPE REF TO zabaputil_cl_util_xml.
    lo_root = zabaputil_cl_util_xml=>factory( ).
    DATA lo_view TYPE REF TO zabaputil_cl_util_xml.
    lo_view = lo_root->__( n = `View` ns = `mvc` ).
    DATA lo_btn TYPE REF TO zabaputil_cl_util_xml.
    lo_btn  = lo_view->__( `Page`
      )->__( n = `Button` a = `text` v = `OK` ).

    lo_btn->p( n = `type` v = `Emphasized` ).
    lo_btn->p( n = `icon` v = `sap-icon://accept` ).

    DATA lv_xml TYPE string.
    lv_xml = lo_root->stringify( ).

    DATA temp33 TYPE xsdboolean.
    temp33 = boolc( lv_xml CS `text="OK"` ).
    cl_abap_unit_assert=>assert_true( temp33 ).
    DATA temp34 TYPE xsdboolean.
    temp34 = boolc( lv_xml CS `type="Emphasized"` ).
    cl_abap_unit_assert=>assert_true( temp34 ).
    DATA temp35 TYPE xsdboolean.
    temp35 = boolc( lv_xml CS `sap-icon://accept` ).
    cl_abap_unit_assert=>assert_true( temp35 ).

  ENDMETHOD.

  METHOD test_if_true.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Page`
      )->_if( when = abap_true
              n    = `Panel`
              a    = `headerText`
              v    = `Admin`
      )->_( n = `Text` a = `text` v = `Secret`
      )->n( `Page`
      )->stringify( ).

    DATA temp36 TYPE xsdboolean.
    temp36 = boolc( lv_xml CS `Panel` ).
    cl_abap_unit_assert=>assert_true( temp36 ).
    DATA temp37 TYPE xsdboolean.
    temp37 = boolc( lv_xml CS `Secret` ).
    cl_abap_unit_assert=>assert_true( temp37 ).

  ENDMETHOD.

  METHOD test_if_false.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Page`
      )->_if( when = abap_false
              n    = `Panel`
              a    = `headerText`
              v    = `Admin`
      )->_( n = `Button` a = `text` v = `Visible`
      )->stringify( ).

    DATA temp38 TYPE xsdboolean.
    temp38 = boolc( lv_xml CS `Panel` ).
    cl_abap_unit_assert=>assert_false( temp38 ).
    DATA temp39 TYPE xsdboolean.
    temp39 = boolc( lv_xml CS `Button` ).
    cl_abap_unit_assert=>assert_true( temp39 ).

  ENDMETHOD.

  METHOD test_leaf_if_true.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Page`
      )->__if( when = abap_true
               n    = `Button`
               a    = `text`
               v    = `Admin Only`
      )->stringify( ).

    DATA temp40 TYPE xsdboolean.
    temp40 = boolc( lv_xml CS `Admin Only` ).
    cl_abap_unit_assert=>assert_true( temp40 ).

  ENDMETHOD.

  METHOD test_leaf_if_false.

    DATA lv_xml TYPE string.
    lv_xml = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
      )->__( `Page`
      )->__if( when = abap_false
               n    = `Button`
               a    = `text`
               v    = `Hidden`
      )->_( n = `Text` a = `text` v = `Visible`
      )->stringify( ).

    DATA temp41 TYPE xsdboolean.
    temp41 = boolc( lv_xml CS `Hidden` ).
    cl_abap_unit_assert=>assert_false( temp41 ).
    DATA temp42 TYPE xsdboolean.
    temp42 = boolc( lv_xml CS `Visible` ).
    cl_abap_unit_assert=>assert_true( temp42 ).

  ENDMETHOD.

  METHOD build_page_view.

    DATA temp33 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp33.
    DATA temp34 LIKE LINE OF temp33.
    temp34-n = `height`.
    temp34-v = `100%`.
    INSERT temp34 INTO TABLE temp33.
    temp34-n = `xmlns:mvc`.
    temp34-v = `sap.ui.core.mvc`.
    INSERT temp34 INTO TABLE temp33.
    temp34-n = `xmlns`.
    temp34-v = `sap.m`.
    INSERT temp34 INTO TABLE temp33.
    result = zabaputil_cl_util_xml=>factory(
      )->__( n = `View` ns = `mvc`
            p = temp33 ).

    DATA temp35 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp35.
    DATA temp36 LIKE LINE OF temp35.
    temp36-n = `title`.
    temp36-v = `Title`.
    INSERT temp36 INTO TABLE temp35.
    temp36-n = `class`.
    temp36-v = `sapUiContentPadding sapUiResponsivePadding--header sapUiResponsivePadding--subHeader sapUiResponsivePadding--content sapUiResponsivePadding--footer`.
    INSERT temp36 INTO TABLE temp35.
    temp36-n = `showNavButton`.
    temp36-v = `true`.
    INSERT temp36 INTO TABLE temp35.
    DATA lo_page TYPE REF TO zabaputil_cl_util_xml.
    lo_page = result->__( n = `Page`
      p = temp35 ).

    DATA temp37 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp37.
    DATA temp38 LIKE LINE OF temp37.
    temp38-n = `icon`.
    temp38-v = `sap-icon://action`.
    INSERT temp38 INTO TABLE temp37.
    temp38-n = `tooltip`.
    temp38-v = `Share`.
    INSERT temp38 INTO TABLE temp37.
    lo_page->__( `headerContent`
      )->_( n = `Button`
             p = temp37 ).

    lo_page->__( `subHeader`
      )->__( `OverflowToolbar`
      )->_( `SearchField` ).

    lo_page->__( `content`
      )->__( `VBox`
      )->_( n = `Text` a = `text`
             v = `Lorem ipsum dolor st amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.`
           && ` At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.`
           && ` Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.`
           && ` Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat` ).

    DATA lo_footer_toolbar TYPE REF TO zabaputil_cl_util_xml.
    lo_footer_toolbar = lo_page->__( `footer`
      )->__( `OverflowToolbar` ).
    lo_footer_toolbar->_( `ToolbarSpacer` ).
    DATA temp39 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp39.
    DATA temp40 LIKE LINE OF temp39.
    temp40-n = `text`.
    temp40-v = `Accept`.
    INSERT temp40 INTO TABLE temp39.
    temp40-n = `type`.
    temp40-v = `Accept`.
    INSERT temp40 INTO TABLE temp39.
    lo_footer_toolbar->_( n = `Button` p = temp39 ).
    DATA temp41 TYPE zabaputil_cl_util=>ty_t_name_value.
    CLEAR temp41.
    DATA temp42 LIKE LINE OF temp41.
    temp42-n = `text`.
    temp42-v = `Reject`.
    INSERT temp42 INTO TABLE temp41.
    temp42-n = `type`.
    temp42-v = `Reject`.
    INSERT temp42 INTO TABLE temp41.
    lo_footer_toolbar->_( n = `Button` p = temp41 ).
    lo_footer_toolbar->_( n = `Button` a = `text` v = `Edit` ).
    lo_footer_toolbar->_( n = `Button` a = `text` v = `Delete` ).

  ENDMETHOD.

  METHOD test_ui5_page_sample.

    DATA lv_xml TYPE string.
    lv_xml = build_page_view( )->stringify( ).

    DATA lv_exp TYPE string.
    lv_exp = ` <mvc:View height="100%" xmlns="sap.m" xmlns:mvc="sap.ui.core.mvc">`
                && ` <Page class="sapUiContentPadding sapUiResponsivePadding--header sapUiResponsivePadding--subHeader sapUiResponsivePadding--content sapUiResponsivePadding--footer" showNavButton="true" title="Title">`
                && ` <headerContent>`
                && ` <Button icon="sap-icon://action" tooltip="Share"/>`
                && `</headerContent>`
                && ` <subHeader>`
                && ` <OverflowToolbar>`
                && ` <SearchField/>`
                && `</OverflowToolbar>`
                && `</subHeader>`
                && ` <content>`
                && ` <VBox>`
                && ` <Text text="Lorem ipsum dolor st amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.`
                && ` At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.`
                && ` Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.`
                && ` Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat"/>`
                && `</VBox>`
                && `</content>`
                && ` <footer>`
                && ` <OverflowToolbar>`
                && ` <ToolbarSpacer/>`
                && ` <Button text="Accept" type="Accept"/>`
                && ` <Button text="Reject" type="Reject"/>`
                && ` <Button text="Edit"/>`
                && ` <Button text="Delete"/>`
                && `</OverflowToolbar>`
                && `</footer>`
                && `</Page>`
                && `</mvc:View>`.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp act = lv_xml ).

  ENDMETHOD.

  METHOD test_ui5_page_sample_indent.

    DATA lv_xml TYPE string.
    lv_xml = build_page_view( )->stringify( indent = abap_true ).

    DATA lv_exp TYPE string.
    lv_exp = `<mvc:View height="100%" xmlns="sap.m" xmlns:mvc="sap.ui.core.mvc">` && |\n|
                && `  <Page class="sapUiContentPadding sapUiResponsivePadding--header sapUiResponsivePadding--subHeader sapUiResponsivePadding--content sapUiResponsivePadding--footer" showNavButton="true" title="Title">` && |\n|
                && `    <headerContent>` && |\n|
                && `      <Button icon="sap-icon://action" tooltip="Share"/>` && |\n|
                && `    </headerContent>` && |\n|
                && `    <subHeader>` && |\n|
                && `      <OverflowToolbar>` && |\n|
                && `        <SearchField/>` && |\n|
                && `      </OverflowToolbar>` && |\n|
                && `    </subHeader>` && |\n|
                && `    <content>` && |\n|
                && `      <VBox>` && |\n|
                && `        <Text text="Lorem ipsum dolor st amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.`
                && ` At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.`
                && ` Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.`
                && ` Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat"/>` && |\n|
                && `      </VBox>` && |\n|
                && `    </content>` && |\n|
                && `    <footer>` && |\n|
                && `      <OverflowToolbar>` && |\n|
                && `        <ToolbarSpacer/>` && |\n|
                && `        <Button text="Accept" type="Accept"/>` && |\n|
                && `        <Button text="Reject" type="Reject"/>` && |\n|
                && `        <Button text="Edit"/>` && |\n|
                && `        <Button text="Delete"/>` && |\n|
                && `      </OverflowToolbar>` && |\n|
                && `    </footer>` && |\n|
                && `  </Page>` && |\n|
                && `</mvc:View>`.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp act = lv_xml ).

  ENDMETHOD.

ENDCLASS.

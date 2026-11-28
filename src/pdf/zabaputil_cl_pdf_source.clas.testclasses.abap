CLASS ltcl_source DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    "! a class as somebody pastes it: comments, chains, types of its own
    DATA mv_class TYPE string.

    METHODS setup.

    METHODS name_of_the_class      FOR TESTING.
    METHODS fields_of_the_class    FOR TESTING.
    METHODS fields_declared_oddly  FOR TESTING.
    METHODS text_of_a_class        FOR TESTING.
    METHODS text_of_a_method       FOR TESTING.
    METHODS text_as_it_is          FOR TESTING.
    METHODS method_of_a_text       FOR TESTING.
    METHODS method_in_statements   FOR TESTING.
    METHODS class_with_another_form FOR TESTING.
    METHODS class_without_the_method FOR TESTING.
    METHODS class_without_a_body   FOR TESTING.
    METHODS class_of_a_form        FOR TESTING.
    METHODS class_of_an_empty_form FOR TESTING.
    METHODS definition_and_the_rest FOR TESTING.
    METHODS interface_of_a_form    FOR TESTING.
    METHODS sample_data_of_a_source FOR TESTING.
    METHODS sample_data_that_is_none FOR TESTING.
    METHODS call_of_a_class        FOR TESTING.
    METHODS call_for_702           FOR TESTING.
    METHODS numeric_text_length    FOR TESTING.
    METHODS check_a_good_class     FOR TESTING.
    METHODS check_finds_problems   FOR TESTING.
    METHODS type_of_the_dictionary FOR TESTING.

ENDCLASS.

CLASS ltcl_source IMPLEMENTATION.

  METHOD setup.
    mv_class =
      |CLASS zcl_my_note DEFINITION PUBLIC FINAL CREATE PUBLIC.\n| &&
      |\n| &&
      |  PUBLIC SECTION.\n| &&
      |\n| &&
      |    " a delivery note. With a period, and a `backquote` in the comment.\n| &&
      |    INTERFACES zmy_if_pdf_data.\n| &&
      |\n| &&
      |    TYPES:\n| &&
      |      BEGIN OF ty_s_address,\n| &&
      |        street TYPE string,\n| &&
      |        city   TYPE string,\n| &&
      |      END OF ty_s_address.\n| &&
      |    TYPES:\n| &&
      |      BEGIN OF ty_s_item,\n| &&
      |        posnr TYPE i,\n| &&
      |        arktx TYPE string,\n| &&
      |        lfimg TYPE p LENGTH 8 DECIMALS 2,\n| &&
      |      END OF ty_s_item.\n| &&
      |    TYPES ty_t_item TYPE STANDARD TABLE OF ty_s_item WITH EMPTY KEY.\n| &&
      |\n| &&
      |    CONSTANTS c_kind TYPE string VALUE `NOTE.`.\n| &&
      |    CLASS-DATA gv_count TYPE i.\n| &&
      |* a comment line. DATA not_this TYPE i.\n| &&
      |    DATA vbeln   TYPE vbak-vbeln. " the number\n| &&
      |    DATA name    TYPE string.\n| &&
      |    DATA address TYPE ty_s_address.\n| &&
      |    DATA t_items TYPE ty_t_item.\n| &&
      |\n| &&
      |    METHODS read IMPORTING vbeln TYPE clike.\n| &&
      |\n| &&
      |  PRIVATE SECTION.\n| &&
      |    DATA secret TYPE string.\n| &&
      |\n| &&
      |ENDCLASS.\n| &&
      |\n| &&
      |CLASS zcl_my_note IMPLEMENTATION.\n| &&
      |\n| &&
      |  METHOD read.\n| &&
      |    me->vbeln = vbeln.\n| &&
      |  ENDMETHOD.\n| &&
      |\n| &&
      |  METHOD zmy_if_pdf_data~get.\n| &&
      |\n| &&
      |    " written by the editor - a `backquote` here must not count\n| &&
      |    result = `<form version="1"><FORMAT>A5</FORMAT><T_ELEMENT><item><TYPE>TEXT</TYPE>` &&\n| &&
      |             `<TEXT>a ```` b</TEXT></item></T_ELEMENT></form>`.\n| &&
      |\n| &&
      |  ENDMETHOD.\n| &&
      |\n| &&
      |ENDCLASS.\n|.
  ENDMETHOD.

  METHOD name_of_the_class.
    cl_abap_unit_assert=>assert_equals( exp = `ZCL_MY_NOTE` act = zabaputil_cl_pdf_source=>class_name( mv_class ) ).
    " a method, a text, nothing: no class
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>class_name( `  METHOD x~get. ENDMETHOD.` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>class_name( `<form></form>` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>class_name( `` ) ).
    " a comment that says DEFINITION in front of the class
    cl_abap_unit_assert=>assert_equals(
        exp = `ZCL_B`
        act = zabaputil_cl_pdf_source=>class_name( |" the old DEFINITION was wrong\nclass zcl_b definition public.| ) ).
  ENDMETHOD.

  METHOD fields_of_the_class.
    " the public instance attributes, in the order they are declared: a
    " value, the components of a structure, a table with its columns
    DATA(lt_field) = zabaputil_cl_pdf_source=>fields( mv_class ).
    DATA(lt_expected) = VALUE zabaputil_cl_pdf_form=>ty_t_field(
        ( path = `VBELN`          kind = `VALUE` )
        ( path = `NAME`           kind = `VALUE` )
        ( path = `ADDRESS-STREET` kind = `VALUE` )
        ( path = `ADDRESS-CITY`   kind = `VALUE` )
        ( path = `T_ITEMS`        kind = `TABLE` )
        ( path = `POSNR`          kind = `COLUMN` parent = `T_ITEMS` )
        ( path = `ARKTX`          kind = `COLUMN` parent = `T_ITEMS` )
        ( path = `LFIMG`          kind = `COLUMN` parent = `T_ITEMS` ) ).
    cl_abap_unit_assert=>assert_equals( exp = lt_expected act = lt_field ).
    " no class, no fields
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>fields( `DATA x TYPE i.` ) ).
  ENDMETHOD.

  METHOD fields_declared_oddly.
    " a chain of attributes, a structure and a table declared with the
    " attribute, a structure in a structure, a type that names another, a
    " type of the class written with the class, a length in parentheses,
    " references, a table of a type the source does not know
    DATA(lv_class) =
      |class zcl_odd definition public.\n  public section.\n| &&
      |    types ty_name type string.\n| &&
      |    types: begin of ty_s_row, a type i, b type ty_name, end of ty_s_row,\n| &&
      |           ty_t_row type sorted table of ty_s_row with unique key a,\n| &&
      |           ty_t_alias type ty_t_row.\n| &&
      |    data: one type ty_name, two(10) type c,\n| &&
      |          begin of head, city type string,\n| &&
      |            begin of geo, lat type f, end of geo,\n| &&
      |          end of head.\n| &&
      |    data t_direct type standard table of zcl_odd=>ty_s_row with default key.\n| &&
      |    data t_alias type ty_t_alias.\n| &&
      |    data t_unknown type table of mara.\n| &&
      |    data t_ddic type ztt_any.\n| &&
      |    data o_ref type ref to object.\n| &&
      |    data t_refs type standard table of ref to object with empty key.\n| &&
      |endclass.\n|.
    DATA(lt_expected) = VALUE zabaputil_cl_pdf_form=>ty_t_field(
        ( path = `ONE`           kind = `VALUE` )
        ( path = `TWO`           kind = `VALUE` )
        ( path = `HEAD-CITY`     kind = `VALUE` )
        ( path = `HEAD-GEO-LAT`  kind = `VALUE` )
        ( path = `T_DIRECT`      kind = `TABLE` )
        ( path = `A`             kind = `COLUMN` parent = `T_DIRECT` )
        ( path = `B`             kind = `COLUMN` parent = `T_DIRECT` )
        ( path = `T_ALIAS`       kind = `TABLE` )
        ( path = `A`             kind = `COLUMN` parent = `T_ALIAS` )
        ( path = `B`             kind = `COLUMN` parent = `T_ALIAS` )
        ( path = `T_UNKNOWN`     kind = `TABLE` )
        ( path = `T_DDIC`        kind = `VALUE` )
        ( path = `T_REFS`        kind = `TABLE` ) ).
    cl_abap_unit_assert=>assert_equals( exp = lt_expected
                                        act = zabaputil_cl_pdf_source=>fields( lv_class ) ).
  ENDMETHOD.

  METHOD text_of_a_class.
    " the literals of get( ) - not those of the rest of the class, not the
    " backquotes of a comment - joined, a doubled backquote one
    cl_abap_unit_assert=>assert_equals(
        exp = '<form version="1"><FORMAT>A5</FORMAT><T_ELEMENT><item><TYPE>TEXT</TYPE><TEXT>a `` b</TEXT></item></T_ELEMENT></form>'
        act = zabaputil_cl_pdf_source=>form_text( mv_class ) ).
    " and it is a form
    DATA(ls_form) = zabaputil_cl_pdf_form=>parse( zabaputil_cl_pdf_source=>form_text( mv_class ) ).
    cl_abap_unit_assert=>assert_equals( exp = `A5` act = ls_form-format ).
    cl_abap_unit_assert=>assert_equals( exp = 'a `` b' act = ls_form-t_element[ 1 ]-text ).
  ENDMETHOD.

  METHOD text_of_a_method.
    DATA(lv_method) = |  METHOD zif_any~get.\n\n    result = `<form>` &&\n             `</form>`.\n\n  ENDMETHOD.\n|.
    cl_abap_unit_assert=>assert_equals( exp = `<form></form>`
                                        act = zabaputil_cl_pdf_source=>form_text( lv_method ) ).
    " a method that returns nothing, a class that has no method
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>form_text( |METHOD zif_any~get.\nENDMETHOD.| ) ).
    cl_abap_unit_assert=>assert_initial(
        zabaputil_cl_pdf_source=>form_text( |CLASS zcl_a DEFINITION.\n  PUBLIC SECTION.\n    DATA a TYPE string VALUE `x`.\nENDCLASS.| ) ).
  ENDMETHOD.

  METHOD text_as_it_is.
    cl_abap_unit_assert=>assert_equals( exp = `<form><A>1</A></form>`
                                        act = zabaputil_cl_pdf_source=>form_text( `<form><A>1</A></form>` ) ).
    " literals without a method around them
    cl_abap_unit_assert=>assert_equals( exp = `<form></form>`
                                        act = zabaputil_cl_pdf_source=>form_text( 'result = `<form>` && `</form>`.' ) ).
  ENDMETHOD.

  METHOD method_of_a_text.
    cl_abap_unit_assert=>assert_equals(
        exp = |  METHOD zif_any~get.\n\n    result = `<form>a ``b``</form>`.\n\n  ENDMETHOD.\n|
        act = zabaputil_cl_pdf_source=>method_get( text      = '<form>a `b`</form>'
                                               interface = `ZIF_ANY` ) ).
    " no text: a method that returns none
    cl_abap_unit_assert=>assert_equals(
        exp = |  METHOD zif_any~get.\n\n    result = ``.\n\n  ENDMETHOD.\n|
        act = zabaputil_cl_pdf_source=>method_get( text      = ``
                                               interface = `zif_any` ) ).
  ENDMETHOD.

  METHOD method_in_statements.
    " 100 characters a literal, 100 literals a statement - and the text
    " comes back out of it
    DATA(lv_text) = repeat( val = '0123456789`'
                            occ = 2000 ).
    DATA(lv_method) = zabaputil_cl_pdf_source=>method_get( text      = lv_text
                                                       interface = `zif_any` ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = count( val = lv_method
                                                     sub = `result = result &&` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = count( val = lv_method
                                                     sub = |`.\n| ) ).
    DATA(lv_line_feed) = |\n|.
    SPLIT lv_method AT lv_line_feed INTO TABLE DATA(lt_line).
    LOOP AT lt_line INTO DATA(lv_line).
      cl_abap_unit_assert=>assert_true( xsdbool( strlen( lv_line ) <= 220 ) ).
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( exp = lv_text
                                        act = zabaputil_cl_pdf_source=>form_text( lv_method ) ).
  ENDMETHOD.

  METHOD class_with_another_form.
    " the method the class has returns the new text; its first line - the
    " interface as this class names it - and everything else stay
    DATA(lv_class) = zabaputil_cl_pdf_source=>with_form( val       = mv_class
                                                     text      = `<form><FORMAT>A3</FORMAT></form>`
                                                     interface = `zabaputil_if_pdf_data` ).
    cl_abap_unit_assert=>assert_equals( exp = `<form><FORMAT>A3</FORMAT></form>`
                                        act = zabaputil_cl_pdf_source=>form_text( lv_class ) ).
    cl_abap_unit_assert=>assert_true(
        xsdbool( lv_class CS |  METHOD zmy_if_pdf_data~get.\n\n    result = `<form><FORMAT>A3</FORMAT></form>`.\n\n  ENDMETHOD.\n\nENDCLASS.| ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_class CS `A5` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_class CS |  METHOD read.\n    me->vbeln = vbeln.\n  ENDMETHOD.| ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_source=>fields( mv_class )
                                        act = zabaputil_cl_pdf_source=>fields( lv_class ) ).
    " and once more: what was written is read and replaced the same way
    DATA(lv_again) = zabaputil_cl_pdf_source=>with_form( val       = lv_class
                                                     text      = `<form></form>`
                                                     interface = `zabaputil_if_pdf_data` ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = count( val = lv_again sub = `~get.` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `<form></form>` act = zabaputil_cl_pdf_source=>form_text( lv_again ) ).
  ENDMETHOD.

  METHOD class_without_the_method.
    " an implementation without get( ): the method is added, for the
    " interface the class names
    DATA(lv_class) =
      |CLASS zcl_new DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    INTERFACES zmy_if_pdf_data.\n    DATA a TYPE string.\n| &&
      |ENDCLASS.\n\nCLASS zcl_new IMPLEMENTATION.\n\nENDCLASS.\n|.
    DATA(lv_result) = zabaputil_cl_pdf_source=>with_form( val       = lv_class
                                                      text      = `<form></form>`
                                                      interface = `zabaputil_if_pdf_data` ).
    cl_abap_unit_assert=>assert_equals(
        exp = |CLASS zcl_new DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    INTERFACES zmy_if_pdf_data.\n    DATA a TYPE string.\n| &&
              |ENDCLASS.\n\nCLASS zcl_new IMPLEMENTATION.\n\n| &&
              |  METHOD zmy_if_pdf_data~get.\n\n    result = `<form></form>`.\n\n  ENDMETHOD.\n\nENDCLASS.\n|
        act = lv_result ).
  ENDMETHOD.

  METHOD class_without_a_body.
    " a definition alone: an implementation with the method, for the
    " interface of the editor when the class names none
    DATA(lv_class) = |CLASS zcl_new DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    DATA a TYPE string.\nENDCLASS.|.
    DATA(lv_result) = zabaputil_cl_pdf_source=>with_form( val       = lv_class
                                                      text      = `<form></form>`
                                                      interface = `zabaputil_if_pdf_data` ).
    cl_abap_unit_assert=>assert_equals(
        exp = |CLASS zcl_new DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    DATA a TYPE string.\nENDCLASS.\n\n| &&
              |CLASS zcl_new IMPLEMENTATION.\n\n| &&
              |  METHOD zabaputil_if_pdf_data~get.\n\n    result = `<form></form>`.\n\n  ENDMETHOD.\n\nENDCLASS.\n|
        act = lv_result ).
  ENDMETHOD.

  METHOD class_of_a_form.
    " a form without a class: the fields it names are the attributes. A
    " field of a structure makes the structure, a table element the table,
    " an image that comes from the data its bytes; a name that is no name
    " and the numbers of the pages make nothing
    DATA(ls_form) = zabaputil_cl_pdf_form=>new_form( ).
    ls_form-header = `Note {VBELN}, page {page} of {pages}`.
    ls_form-t_element = VALUE #(
        ( type = zabaputil_cl_pdf_form=>cs_type-text
          text = `{NAME} in {address-city}, {ADDRESS-GEO-LAT} {VBELN} {1BAD} {a b} {open` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table source = `t_items`
          t_column = VALUE #( ( name = `posnr` ) ( name = `ARKTX` ) ( name = `` ) ( name = `posnr` ) ) )
        ( type = zabaputil_cl_pdf_form=>cs_type-image source = `LOGO` )
        ( type = zabaputil_cl_pdf_form=>cs_type-table source = `NAME` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |CLASS zcl_my_form DEFINITION\n  PUBLIC\n  FINAL\n  CREATE PUBLIC.\n\n| &&
              |  PUBLIC SECTION.\n\n| &&
              |    INTERFACES zabaputil_if_pdf_data.\n\n| &&
              |    TYPES:\n      BEGIN OF ty_s_address_geo,\n        lat TYPE string,\n      END OF ty_s_address_geo.\n| &&
              |    TYPES:\n      BEGIN OF ty_s_address,\n        city TYPE string,\n        geo TYPE ty_s_address_geo,\n| &&
              |      END OF ty_s_address.\n| &&
              |    TYPES:\n      BEGIN OF ty_s_t_items,\n        posnr TYPE string,\n        arktx TYPE string,\n| &&
              |      END OF ty_s_t_items.\n| &&
              |    TYPES ty_t_t_items TYPE STANDARD TABLE OF ty_s_t_items WITH DEFAULT KEY.\n\n| &&
              |    DATA vbeln TYPE string.\n| &&
              |    DATA name TYPE string.\n| &&
              |    DATA address TYPE ty_s_address.\n| &&
              |    DATA t_items TYPE ty_t_t_items.\n| &&
              |    DATA logo TYPE xstring.\n\n| &&
              |  PROTECTED SECTION.\n  PRIVATE SECTION.\nENDCLASS.\n\n\n| &&
              |CLASS zcl_my_form IMPLEMENTATION.\n\n| &&
              |  METHOD zabaputil_if_pdf_data~get.\n\n    result = `<form></form>`.\n\n  ENDMETHOD.\n\nENDCLASS.\n|
        act = zabaputil_cl_pdf_source=>class_of_form( name      = `ZCL_MY_FORM`
                                                  interface = `zabaputil_if_pdf_data`
                                                  form      = ls_form
                                                  text      = `<form></form>` ) ).
  ENDMETHOD.

  METHOD class_of_an_empty_form.
    " nothing named: a class without attributes, still a form
    DATA(lv_class) = zabaputil_cl_pdf_source=>class_of_form( name      = `zcl_empty`
                                                         interface = `zif_any`
                                                         form      = zabaputil_cl_pdf_form=>new_form( )
                                                         text      = `<form></form>` ).
    cl_abap_unit_assert=>assert_equals(
        exp = |CLASS zcl_empty DEFINITION\n  PUBLIC\n  FINAL\n  CREATE PUBLIC.\n\n  PUBLIC SECTION.\n\n| &&
              |    INTERFACES zif_any.\n\n  PROTECTED SECTION.\n  PRIVATE SECTION.\nENDCLASS.\n\n\n| &&
              |CLASS zcl_empty IMPLEMENTATION.\n\n  METHOD zif_any~get.\n\n    result = `<form></form>`.\n\n  ENDMETHOD.\n\nENDCLASS.\n|
        act = lv_class ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>fields( lv_class ) ).
  ENDMETHOD.

  METHOD definition_and_the_rest.
    " the definition part - what is maintained as the model - and what
    " follows it; together they are the class again
    DATA(lv_definition) = zabaputil_cl_pdf_source=>definition( mv_class ).
    DATA(lv_implementation) = zabaputil_cl_pdf_source=>implementation( mv_class ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = find( val = lv_definition
                                                    sub = `CLASS zcl_my_note DEFINITION PUBLIC FINAL CREATE PUBLIC.` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_definition CS |    DATA secret TYPE string.\n\nENDCLASS.| ) ).
    cl_abap_unit_assert=>assert_equals( exp = `ENDCLASS.`
                                        act = substring( val = lv_definition
                                                         off = strlen( lv_definition ) - 9 ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_definition CS `IMPLEMENTATION` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = find( val = lv_implementation
                                                    sub = `CLASS zcl_my_note IMPLEMENTATION.` ) ).
    cl_abap_unit_assert=>assert_equals( exp = mv_class
                                        act = |{ lv_definition }\n\n{ lv_implementation }| ).
    " the fields and the form are where they belong
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_source=>fields( mv_class )
                                        act = zabaputil_cl_pdf_source=>fields( lv_definition ) ).
    cl_abap_unit_assert=>assert_equals( exp = zabaputil_cl_pdf_source=>form_text( mv_class )
                                        act = zabaputil_cl_pdf_source=>form_text( lv_implementation ) ).

    " a definition alone, one that is not finished, a text that is none
    cl_abap_unit_assert=>assert_equals( exp = lv_definition
                                        act = zabaputil_cl_pdf_source=>definition( lv_definition ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>implementation( lv_definition ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |class zcl_a definition.\n  public section.\n    data a type |
        act = zabaputil_cl_pdf_source=>definition( |" first\nclass zcl_a definition.\n  public section.\n    data a type | ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>definition( `<form></form>` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>implementation( `<form></form>` ) ).
  ENDMETHOD.

  METHOD interface_of_a_form.
    cl_abap_unit_assert=>assert_true( zabaputil_cl_pdf_source=>has_interface( mv_class ) ).
    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_pdf_source=>has_interface( |CLASS zcl_a DEFINITION.\n  PUBLIC SECTION.\n    INTERFACES zif_other.\nENDCLASS.| ) ).
    " in a comment it is not named
    cl_abap_unit_assert=>assert_false(
        zabaputil_cl_pdf_source=>has_interface( |CLASS zcl_a DEFINITION.\n  PUBLIC SECTION.\n    " INTERFACES zabaputil_if_pdf_data.\nENDCLASS.| ) ).
  ENDMETHOD.

  METHOD sample_data_of_a_source.
    " a class nobody can create still prints with values: a structure of
    " its public attributes, filled like an instance would be
    DATA(lr_data) = zabaputil_cl_pdf_source=>sample_data( mv_class ).
    cl_abap_unit_assert=>assert_bound( lr_data ).
    ASSIGN lr_data->* TO FIELD-SYMBOL(<data>).
    cl_abap_unit_assert=>assert_equals(
        exp = `VBELN / NAME / CITY / {SECRET}`
        act = zabaputil_cl_pdf_form=>resolve( val  = `{VBELN} / {NAME} / {ADDRESS-CITY} / {SECRET}`
                                          data = <data> ) ).
    " the same fields the source lists
    DATA(lt_data) = zabaputil_cl_pdf_form=>fields( <data> ).
    DATA(lt_source) = zabaputil_cl_pdf_source=>fields( mv_class ).
    SORT lt_data BY parent path.
    SORT lt_source BY parent path.
    cl_abap_unit_assert=>assert_equals( exp = lt_source act = lt_data ).

    " and a form prints its table with three rows of it
    DATA(ls_form) = zabaputil_cl_pdf_form=>new_form( ).
    ls_form-t_element = VALUE #( ( type     = zabaputil_cl_pdf_form=>cs_type-table
                                   source   = `T_ITEMS`
                                   t_column = VALUE #( ( name = `POSNR` title = `Item` )
                                                       ( name = `LFIMG` title = `Quantity` ) ) ) ).
    DATA(lo_pdf) = zabaputil_cl_pdf_form=>build( val  = ls_form
                                             data = <data> ).
    DATA(lv_content) = lo_pdf->page( 1 )->content( ).
    cl_abap_unit_assert=>assert_equals( exp = 8
                                        act = count( val = lv_content
                                                     sub = ` Tj` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_content CS |{ lo_pdf->font( `Helvetica` )->encode( `37.50` ) } Tj| ) ).
  ENDMETHOD.

  METHOD sample_data_that_is_none.
    " nothing declared, no class, a source somebody is in the middle of
    cl_abap_unit_assert=>assert_not_bound(
        zabaputil_cl_pdf_source=>sample_data( |CLASS zcl_a DEFINITION.\n  PUBLIC SECTION.\nENDCLASS.| ) ).
    cl_abap_unit_assert=>assert_not_bound( zabaputil_cl_pdf_source=>sample_data( `` ) ).
    DATA(lr_data) = zabaputil_cl_pdf_source=>sample_data(
        |CLASS zcl_a DEFINITION.\n  PUBLIC SECTION.\n    DATA ok TYPE string.\n    DATA ok TYPE i.\n    DATA 1bad TYPE i.\n| &&
        |    DATA o_ref TYPE REF TO object.\n    TYPES: BEGIN OF ty_s_empty, END OF ty_s_empty.\n| &&
        |    DATA empty TYPE ty_s_empty.\n    DATA t_of TYPE STANDARD TABLE OF ty_s_empty.\n    DATA half TYPE | ).
    cl_abap_unit_assert=>assert_bound( lr_data ).
    ASSIGN lr_data->* TO FIELD-SYMBOL(<data>).
    cl_abap_unit_assert=>assert_equals( exp = `OK`
                                        act = zabaputil_cl_pdf_form=>resolve( val  = `{OK}`
                                                                          data = <data> ) ).
  ENDMETHOD.

  METHOD call_of_a_class.
    " how a program prints the form: a value per attribute, by its type
    cl_abap_unit_assert=>assert_equals(
        exp = |" Print the form zcl_my_note: an instance, its attributes, one call.\n| &&
              |DATA(lo_form) = NEW zcl_my_note( ).\n\n| &&
              |" the data of this print\n| &&
              |lo_form->vbeln = `...`.\n| &&
              |lo_form->name = `...`.\n| &&
              |lo_form->address-street = `...`.\n| &&
              |lo_form->address-city = `...`.\n| &&
              |INSERT VALUE #( posnr = 1\n                arktx = `...`\n                lfimg = 1 ) INTO TABLE lo_form->t_items.\n\n| &&
              |" the file, as an xstring - to download, to mail, to store\n| &&
              |DATA(lv_pdf) = zabaputil_cl_pdf_form=>print( lo_form ).\n|
        act = zabaputil_cl_pdf_source=>call_example( val        = mv_class
                                                 form_class = `ZABAPUTIL_CL_PDF_FORM` ) ).
    " no attributes: nothing to fill; no class: no call
    cl_abap_unit_assert=>assert_equals(
        exp = |" Print the form zcl_a: an instance, its attributes, one call.\nDATA(lo_form) = NEW zcl_a( ).\n\n| &&
              |" the file, as an xstring - to download, to mail, to store\nDATA(lv_pdf) = zcl_print=>print( lo_form ).\n|
        act = zabaputil_cl_pdf_source=>call_example( val        = |CLASS zcl_a DEFINITION.\n  PUBLIC SECTION.\nENDCLASS.|
                                                 form_class = `zcl_print` ) ).
    cl_abap_unit_assert=>assert_initial( zabaputil_cl_pdf_source=>call_example( val        = `<form></form>`
                                                                            form_class = `zcl_print` ) ).
  ENDMETHOD.

  METHOD numeric_text_length.
    " TYPE n LENGTH 18 is 18 digits in the preview: an SSCC a scanner reads
    DATA(lr_data) = zabaputil_cl_pdf_source=>sample_data(
        |CLASS zcl_label DEFINITION PUBLIC.\n  PUBLIC SECTION.\n| &&
        |    DATA sscc TYPE n LENGTH 18.\n    DATA plain TYPE n.\nENDCLASS.\n| ).
    ASSIGN lr_data->* TO FIELD-SYMBOL(<data>).
    ASSIGN COMPONENT `SSCC` OF STRUCTURE <data> TO FIELD-SYMBOL(<sscc>).
    cl_abap_unit_assert=>assert_equals( exp = `000000000000000001` act = |{ <sscc> }| ).
    ASSIGN COMPONENT `PLAIN` OF STRUCTURE <data> TO FIELD-SYMBOL(<plain>).
    cl_abap_unit_assert=>assert_equals( exp = 10 act = strlen( |{ <plain> }| ) ).
    " and the call of the class gives it a number
    cl_abap_unit_assert=>assert_true( xsdbool( zabaputil_cl_pdf_source=>call_example(
        val        = |CLASS zcl_label DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    DATA sscc TYPE n LENGTH 18.\nENDCLASS.\n|
        form_class = `zcl_form` ) CS |lo_form->sscc = 1.\n| ) ).
  ENDMETHOD.

  METHOD call_for_702.
    " CREATE OBJECT, declarations up front, a work area per table
    DATA(lv_call) = zabaputil_cl_pdf_source=>call_example( val        = mv_class
                                                       form_class = `zcl_pdf_form`
                                                       classic    = abap_true ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_call CS |DATA lo_form TYPE REF TO zcl_my_note.\n| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_call CS |DATA ls_t_items LIKE LINE OF lo_form->t_items.\n| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_call CS |CREATE OBJECT lo_form.\n| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_call CS |CLEAR ls_t_items.\nls_t_items-posnr = 1.\nls_t_items-arktx = `...`.\n| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_call CS |APPEND ls_t_items TO lo_form->t_items.\n| ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_call CS |lv_pdf = zcl_pdf_form=>print( lo_form ).\n| ) ).
    " the declarations come before the first statement
    cl_abap_unit_assert=>assert_true( xsdbool( find( val = lv_call sub = `DATA ls_t_items` )
                                                 < find( val = lv_call sub = `CREATE OBJECT` ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_call CS `VALUE #` OR lv_call CS `NEW ` OR lv_call CS `DATA(` ) ).
  ENDMETHOD.

  METHOD check_a_good_class.
    " a class with the interface of a form: only what the reader does not
    " know - vbak-vbeln is no type it could look up
    LOOP AT zabaputil_cl_pdf_source=>check( mv_class ) INTO DATA(ls_message).
      cl_abap_unit_assert=>assert_equals( exp = `Information`
                                          act = ls_message-type
                                          msg = ls_message-text ).
    ENDLOOP.
  ENDMETHOD.

  METHOD type_of_the_dictionary.
    FIELD-SYMBOLS <data> TYPE any.
    FIELD-SYMBOLS <id>   TYPE any.

    " a structure of the dictionary - a table of this repository - has its
    " fields there
    DATA(lv_class) = |CLASS zcl_x DEFINITION PUBLIC.\n  PUBLIC SECTION.\n| &&
                     |    INTERFACES zif_pdf_data.\n    DATA row TYPE zabaputil_t_91.\nENDCLASS.\n|.
    DATA(lt_field) = zabaputil_cl_pdf_source=>fields( lv_class ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_field[ path = `ROW-ID`
                                                                      kind = `VALUE` ] ) ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_field[ path = `ROW` ] ) ) ).

    " the data of the preview has its type
    DATA(lr_data) = zabaputil_cl_pdf_source=>sample_data( lv_class ).
    cl_abap_unit_assert=>assert_bound( lr_data ).
    ASSIGN lr_data->* TO <data>.
    ASSIGN COMPONENT `ROW-ID` OF STRUCTURE <data> TO <id>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_not_initial( <id> ).

    " a value for every field, and nothing to report
    cl_abap_unit_assert=>assert_true( xsdbool( zabaputil_cl_pdf_source=>call_example(
        val        = lv_class
        form_class = `zcl_form` ) CS |lo_form->row-id = `...`.\n| ) ).
    LOOP AT zabaputil_cl_pdf_source=>check( lv_class ) INTO DATA(ls_message).
      cl_abap_unit_assert=>assert_char_np( act = ls_message-text
                                           exp = `*ZABAPUTIL_T_91*` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_finds_problems.
    DATA(lt_none) = zabaputil_cl_pdf_source=>check( `DATA x TYPE i.` ).
    cl_abap_unit_assert=>assert_equals( exp = `Error` act = lt_none[ 1 ]-type ).

    DATA(lt_message) = zabaputil_cl_pdf_source=>check(
        |CLASS zcl_x DEFINITION PUBLIC.\n  PUBLIC SECTION.\n| &&
        |    TYPES: BEGIN OF ty_s, a TYPE i.\n| &&
        |    DATA a TYPE ty_s.\n    DATA a TYPE string.\n    DATA b TYPE zno_such_type.\n| ).
    DATA(lv_all) = ``.
    LOOP AT lt_message INTO DATA(ls_message).
      lv_all = |{ lv_all }{ ls_message-type }: { ls_message-text }\n|.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( act = xsdbool( lv_all CS `Error: ENDCLASS. is missing` )
                                      msg = lv_all ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( lv_all CS `Warning: The public section names no interface` )
                                      msg = lv_all ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( lv_all CS `Error: A BEGIN OF has no END OF` )
                                      msg = lv_all ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( lv_all CS `Information: ZNO_SUCH_TYPE is no type of the class` )
                                      msg = lv_all ).

    " a structure that is closed: what is declared twice is found
    lt_message = zabaputil_cl_pdf_source=>check(
        |CLASS zcl_x DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    INTERFACES zif_pdf_data.\n| &&
        |    DATA a TYPE i.\n    DATA a TYPE string.\nENDCLASS.\n| ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_message ) ).
    cl_abap_unit_assert=>assert_equals( exp = `A is declared twice.` act = lt_message[ 1 ]-text ).
  ENDMETHOD.

ENDCLASS.

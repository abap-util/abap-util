CLASS zabaputil_cl_util_range DEFINITION PUBLIC
  FINAL CREATE PUBLIC.
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF signs,
        including TYPE string VALUE `I`,
        excluding TYPE string VALUE `E`,
      END OF signs.

    CONSTANTS:
      BEGIN OF options,
        equal                TYPE string VALUE `EQ`,
        not_equal            TYPE string VALUE `NE`,
        between              TYPE string VALUE `BT`,
        not_between          TYPE string VALUE `NB`,
        contains_pattern     TYPE string VALUE `CP`,
        not_contains_pattern TYPE string VALUE `NP`,
        greater_than         TYPE string VALUE `GT`,
        greater_equal        TYPE string VALUE `GE`,
        less_equal           TYPE string VALUE `LE`,
        less_than            TYPE string VALUE `LT`,
      END OF options.

    CLASS-METHODS eq
      IMPORTING
        val           TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS ne
      IMPORTING
        val           TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS bt
      IMPORTING
        low           TYPE clike
        high          TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS cp
      IMPORTING
        val           TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS gt
      IMPORTING
        val           TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS ge
      IMPORTING
        val           TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS lt
      IMPORTING
        val           TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS le
      IMPORTING
        val           TYPE clike
        sign          TYPE clike DEFAULT `I`
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_range.

    CLASS-METHODS get_sql_multi
      IMPORTING
        t_sql         TYPE string_table
      RETURNING
        VALUE(result) TYPE string.

    METHODS constructor
      IMPORTING
        iv_fieldname TYPE clike
        ir_range     TYPE REF TO data.

    METHODS get_sql
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

    DATA mv_fieldname TYPE string.
    DATA mr_range     TYPE REF TO data.

ENDCLASS.

CLASS zabaputil_cl_util_range IMPLEMENTATION.

  METHOD eq.

    result = VALUE #( sign = sign option = `EQ` low = val ).

  ENDMETHOD.

  METHOD ne.

    result = VALUE #( sign = sign option = `NE` low = val ).

  ENDMETHOD.

  METHOD bt.

    result = VALUE #( sign = sign option = `BT` low = low high = high ).

  ENDMETHOD.

  METHOD cp.

    result = VALUE #( sign = sign option = `CP` low = val ).

  ENDMETHOD.

  METHOD gt.

    result = VALUE #( sign = sign option = `GT` low = val ).

  ENDMETHOD.

  METHOD ge.

    result = VALUE #( sign = sign option = `GE` low = val ).

  ENDMETHOD.

  METHOD lt.

    result = VALUE #( sign = sign option = `LT` low = val ).

  ENDMETHOD.

  METHOD le.

    result = VALUE #( sign = sign option = `LE` low = val ).

  ENDMETHOD.

  METHOD get_sql_multi.

    LOOP AT t_sql INTO DATA(lv_sql).
      IF lv_sql IS INITIAL.
        CONTINUE.
      ENDIF.
      IF result IS NOT INITIAL.
        result = |{ result } AND |.
      ENDIF.
      result = |{ result }{ lv_sql }|.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    mr_range = ir_range.
    mv_fieldname = |{ to_upper( iv_fieldname ) }|.

  ENDMETHOD.

  METHOD get_sql.

    " the WHERE of the context class, not a second writer of its own: that
    " one ORed an excluding row with the including ones (I EQ A, E EQ B let
    " every row but B through), escaped nothing in a pattern, did not map
    " the + wildcard, and wrote the LIKE form of a pattern back into the
    " caller's range
    DATA lt_range TYPE zabaputil_cl_util_context=>ty_t_range.
    FIELD-SYMBOLS <lt_range> TYPE ANY TABLE.

    ASSIGN mr_range->* TO <lt_range>.
    IF <lt_range> IS NOT ASSIGNED OR <lt_range> IS INITIAL.
      RETURN.
    ENDIF.

    zabaputil_cl_util_context=>itab_corresponding( EXPORTING val = <lt_range>
                                                   CHANGING  tab = lt_range ).

    result = zabaputil_cl_util_context=>filter_get_sql_where(
                 VALUE #( ( name    = mv_fieldname
                            t_range = lt_range ) ) ).

  ENDMETHOD.

ENDCLASS.

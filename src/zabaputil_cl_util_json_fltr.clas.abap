CLASS zabaputil_cl_util_json_fltr DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zabaputil_if_ajson_filter.

    CLASS-METHODS create_no_empty_values
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_ajson_filter.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zabaputil_cl_util_json_fltr IMPLEMENTATION.

  METHOD create_no_empty_values.

    result = NEW zabaputil_cl_util_json_fltr( ).

  ENDMETHOD.

  METHOD zabaputil_if_ajson_filter~keep_node.

    rv_keep = abap_true.

    CASE iv_visit.

      WHEN zabaputil_if_ajson_filter=>visit_type-value.

        CASE is_node-type.
          WHEN zabaputil_if_ajson_types=>node_type-boolean.
            rv_keep = xsdbool( is_node-value <> `false` ).
          WHEN zabaputil_if_ajson_types=>node_type-number.
            rv_keep = xsdbool( is_node-value <> `0` ).
          WHEN zabaputil_if_ajson_types=>node_type-string.
            rv_keep = xsdbool( is_node-value <> `` ).
        ENDCASE.

      WHEN zabaputil_if_ajson_filter=>visit_type-close.
        rv_keep = xsdbool( is_node-children <> 0 ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

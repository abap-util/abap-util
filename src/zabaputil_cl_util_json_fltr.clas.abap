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

    CREATE OBJECT result TYPE zabaputil_cl_util_json_fltr.

  ENDMETHOD.

  METHOD zabaputil_if_ajson_filter~keep_node.

    rv_keep = abap_true.

    CASE iv_visit.

      WHEN zabaputil_if_ajson_filter=>visit_type-value.

        CASE is_node-type.
          WHEN zabaputil_if_ajson_types=>node_type-boolean.
            DATA temp1 TYPE xsdboolean.
            temp1 = boolc( is_node-value <> `false` ).
            rv_keep = temp1.
          WHEN zabaputil_if_ajson_types=>node_type-number.
            DATA temp2 TYPE xsdboolean.
            temp2 = boolc( is_node-value <> `0` ).
            rv_keep = temp2.
          WHEN zabaputil_if_ajson_types=>node_type-string.
            DATA temp3 TYPE xsdboolean.
            temp3 = boolc( is_node-value <> `` ).
            rv_keep = temp3.
        ENDCASE.

      WHEN zabaputil_if_ajson_filter=>visit_type-close.
        DATA temp4 TYPE xsdboolean.
        temp4 = boolc( is_node-children <> 0 ).
        rv_keep = temp4.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

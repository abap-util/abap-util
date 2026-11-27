CLASS zabaputil_cl_util_context DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    " abap-toolkit - Utility Functions for ABAP Cloud & Standard ABAP
    " version: `0.0.1`.
    " origin: https://github.com/oblomov-dev/abap-toolkit
    " author: https://github.com/oblomov-dev
    " license: MIT.

    CONSTANTS:
      BEGIN OF cs_ui5_msg_type,
        e TYPE string VALUE `Error` ##NO_TEXT,
        s TYPE string VALUE `Success` ##NO_TEXT,
        w TYPE string VALUE `Warning` ##NO_TEXT,
        i TYPE string VALUE `Information` ##NO_TEXT,
      END OF cs_ui5_msg_type.

    " Environment-abstracted character/format constants. Callers must read
    " these from z2ui5_cl_util instead of referencing cl_abap_char_utilities /
    " cl_abap_format directly, so the dependency on those SAP standard classes
    " lives in exactly one place (this class' class_constructor) and can be
    " ported once for non-ABAP runtimes (e.g. transpiled JS).
    CLASS-DATA cv_char_util_newline        TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_char_util_cr_lf          TYPE c LENGTH 2 READ-ONLY.
    CLASS-DATA cv_char_util_horizontal_tab TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_char_util_charsize       TYPE i          READ-ONLY.
    CLASS-DATA cv_format_e_xml_attr             TYPE i          READ-ONLY.

    " RTTI type-kind / kind / visibility constants, so callers can branch on
    " stored type_kind/kind fields without referencing cl_abap_typedescr /
    " cl_abap_objectdescr directly.
    CLASS-DATA cv_typedescr_typekind_table   TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_dref    TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_oref    TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_struct1 TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_struct2 TYPE c LENGTH 1 READ-ONLY.

    " the three elementary kinds whose JSON form is not their ABAP form
    " (ISO date/time strings, ISO timestamps) - what a caller converting a
    " scalar between the two representations has to branch on
    CLASS-DATA cv_typedescr_typekind_date    TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_time    TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_packed  TYPE c LENGTH 1 READ-ONLY.

    CLASS-DATA cv_typedescr_kind_struct      TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_kind_elem        TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_kind_ref         TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_objectdescr_public         TYPE c LENGTH 1 READ-ONLY.

    CLASS-METHODS class_constructor.

    " Wraps the ABAP `ROLLBACK WORK` statement so the dependency on the
    " database transaction control lives in one place and can be ported once
    " for non-ABAP runtimes (e.g. transpiled JS).
    CLASS-METHODS db_rollback.

    TYPES:
      BEGIN OF ty_s_name_value,
        n TYPE string,
        v TYPE string,
      END OF ty_s_name_value.
    TYPES ty_t_name_value TYPE STANDARD TABLE OF ty_s_name_value WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_token,
        key      TYPE string,
        text     TYPE string,
        visible  TYPE abap_bool,
        selkz    TYPE abap_bool,
        editable TYPE abap_bool,
      END OF ty_s_token.
    TYPES ty_t_token TYPE STANDARD TABLE OF ty_s_token WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_range,
        sign   TYPE c LENGTH 1,
        option TYPE c LENGTH 2,
        low    TYPE string,
        high   TYPE string,
      END OF ty_s_range.
    TYPES ty_t_range TYPE STANDARD TABLE OF ty_s_range WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_filter_multi,
        name            TYPE string,
        t_range         TYPE ty_t_range,
        t_token         TYPE ty_t_token,
        t_token_added   TYPE ty_t_token,
        t_token_removed TYPE ty_t_token,
      END OF ty_s_filter_multi.
    TYPES ty_t_filter_multi TYPE STANDARD TABLE OF ty_s_filter_multi WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_sql,
        tabname        TYPE string,
        check_autoload TYPE abap_bool,
        layout_name    TYPE string,
        layout_id      TYPE string,
        count          TYPE i,
        t_ref          TYPE REF TO data,
        where          TYPE string,
        t_filter       TYPE ty_t_filter_multi,
      END OF ty_s_sql.

    TYPES:
      BEGIN OF ty_s_msg,
        text       TYPE string,
        id         TYPE string,
        no         TYPE string,
        type       TYPE string,
        v1         TYPE string,
        v2         TYPE string,
        v3         TYPE string,
        v4         TYPE string,
        timestampl TYPE timestampl,
        t_meta     TYPE ty_t_name_value,
      END OF ty_s_msg,
      ty_t_msg TYPE STANDARD TABLE OF ty_s_msg WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_msg_box,
        text    TYPE string,
        type    TYPE string,
        title   TYPE string,
        details TYPE string,
        skip    TYPE abap_bool,
      END OF ty_s_msg_box.

    CLASS-METHODS ui5_get_msg_type
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS ui5_msg_box_format
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_s_msg_box.

    "! Everything a message box can be built from that is NOT a message.
    "! ui5_msg_box_format( ) is asked first and answers `skip` when nothing
    "! it was handed is a message - this is what runs then, so that a box
    "! the caller asked for always has something to show: a character value
    "! (its own text, markup moved into the details), a number, a table, a
    "! nested structure or tree, an object, a data reference. `text` gets a
    "! one-line headline, `details` the data itself rendered as HTML.
    "! Complex data that is initial answers `skip` - a call over an empty
    "! table stays as silent as it is over an empty message table.
    CLASS-METHODS ui5_data_box_format
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_s_msg_box.

    "! A text for a UI5 property that is to be shown as written, not read
    "! by the binding parser: backslash and both braces escaped, so `{/X}`
    "! stays text and `\\server` keeps both backslashes.
    CLASS-METHODS ui5_escape_binding_literal
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! A text for the body of a single-quoted string argument of a UI5 event
    "! handler expression (`press=".onPress('...')"`): backslash, quote, CR
    "! and LF escaped, so no value can close the literal.
    CLASS-METHODS ui5_escape_handler_string
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! Whether every brace of a UI5 binding or expression closes the one
    "! before it - braces inside its single- or double-quoted strings do
    "! not count. A value that does not balance breaks the parse of the
    "! whole attribute it sits in.
    CLASS-METHODS ui5_check_braces_balanced
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    TYPES:
      "! the DDIC reference field of an amount (CURR, referencing a CUKY)
      "! or a quantity (QUAN, referencing a UNIT): the component of the
      "! same structure that holds the currency or the unit
      BEGIN OF ty_s_ref_field,
        name     TYPE string,
        currency TYPE abap_bool,
      END OF ty_s_ref_field.

    "! the core:require of the types the ui5_binding_* methods name: the
    "! aliases DateType, TimeType, FloatType, IntType, CurrType and
    "! UnitType. Set it once on the View or on any control above the bound
    "! ones - the aliases work on UI5 1.71 and on 2.x, where the global
    "! names of the types are gone
    CLASS-METHODS ui5_get_type_require
      RETURNING
        VALUE(result) TYPE string.

    "! the type alias of ui5_get_type_require( ) for an ABAP type kind
    "! (cl_abap_typedescr=>typekind_*) - empty for a kind that is shown as
    "! it is (characters, NUMC, strings, ...). check_currency / check_unit
    "! choose CurrType / UnitType for an amount or a quantity whose
    "! currency or unit is bound beside it
    CLASS-METHODS ui5_get_model_type
      IMPORTING
        type_kind      TYPE clike
        check_currency TYPE abap_bool DEFAULT abap_false
        check_unit     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)  TYPE string.

    "! the formatOptions that go with ui5_get_model_type( ), as a binding
    "! object literal: the source pattern of a date or a time as the model
    "! carries it (yyyy-MM-dd, HH:mm:ss), the decimals of a packed number,
    "! the grouping of every number, no measure for an amount or a
    "! quantity. Empty when the kind has no type
    CLASS-METHODS ui5_get_format_options
      IMPORTING
        type_kind      TYPE clike
        decimals       TYPE i         DEFAULT 0
        check_currency TYPE abap_bool DEFAULT abap_false
        check_unit     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)  TYPE string.

    "! the binding of path with the type and the format options of an ABAP
    "! type kind - {path} when the kind has no type. currency_path /
    "! unit_path bind the currency or the unit as the second part of a
    "! Currency / Unit type. The types are the aliases of
    "! ui5_get_type_require( ), which the view has to carry
    CLASS-METHODS ui5_binding_by_kind
      IMPORTING
        path          TYPE clike
        type_kind     TYPE clike
        decimals      TYPE i     DEFAULT 0
        currency_path TYPE clike OPTIONAL
        unit_path     TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! the binding of path with the type of val - val is only described,
    "! never read: a field of the bound attribute, or of a row of the same
    "! type for a relative path in a table template. A TIMESTAMP or a
    "! TIMESTAMPL travels as an ISO text and stays {path}
    CLASS-METHODS ui5_binding_typed
      IMPORTING
        path          TYPE clike
        val           TYPE any
        currency_path TYPE clike OPTIONAL
        unit_path     TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! the binding of the component comp of struc with a path relative to
    "! the row (`PRICE`) - for a cell of a table template. An amount or a
    "! quantity whose reference field the DDIC names in the same structure
    "! is bound with it as Currency / Unit (rtti_get_ref_field)
    CLASS-METHODS ui5_binding_typed_comp
      IMPORTING
        struc         TYPE any
        comp          TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! the reference field of the component comp of struc as the DDIC has
    "! it - empty for a type that is no DDIC structure, a reference field
    "! outside struc, and always on ABAP Cloud and in the transpiled
    "! runtime (DDFIELDS is not released)
    CLASS-METHODS rtti_get_ref_field
      IMPORTING
        struc         TYPE any
        comp          TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_ref_field.

    CLASS-METHODS rtti_check_serializable
      IMPORTING
        val           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS msg_get_t
      IMPORTING
        !val          TYPE any
        !val2         TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS rtti_get_data_element_text_l
      IMPORTING
        !val          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    " Extracts the DDIC data element name (`\TYPE=...` part of the absolute
    " name) from a component's elementary type, so callers do not reference
    " cl_abap_elemdescr directly.
    CLASS-METHODS rtti_get_ddic_type_name
      IMPORTING
        type          TYPE REF TO cl_abap_datadescr
      RETURNING
        VALUE(result) TYPE string.

    " Thin wrappers around cl_abap_typedescr=>describe_by_data(_ref), so the
    " RTTI describe calls live in one place. Return cl_abap_typedescr, which
    " already carries type_kind / kind / absolute_name.
    CLASS-METHODS rtti_get_typedescr_by_data_ref
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_typedescr.

    "! Is the data object behind the reference a STANDARD table - false for
    "! a sorted or hashed table, for anything that is no table, and for a
    "! reference RTTI cannot describe
    CLASS-METHODS rtti_check_table_standard
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    " one component of a flat structure, with the one fact a JSON writer
    " decides by: json_kind is B for a boolean (abap_bool and its DDIC
    " twins, by type name - as ajson tells them apart), N for a number
    " (every numeric kind), S for a string (character, numeric text, string
    " and binary - all of which |{ }| renders as ajson does), D for a date
    " and T for a time (which ajson writes as ISO text, YYYY-MM-DD and
    " HH:MM:SS, and empty when initial), U for a packed field TYPED
    " timestamp and L for one typed timestampl (by type name as well -
    " ajson writes both as an ISO instant in UTC, time_get_iso_by_stamp
    " and time_get_iso_by_stampl), and empty for a kind no such writer
    " should take on - utclong and an enumeration
    TYPES:
      BEGIN OF ty_s_flat_comp,
        name      TYPE string,
        json_kind TYPE c LENGTH 1,
      END OF ty_s_flat_comp.
    TYPES ty_t_flat_comp TYPE STANDARD TABLE OF ty_s_flat_comp WITH EMPTY KEY.

    "! The components of the FLAT structure behind a data reference - the
    "! structure itself, or the line of the STANDARD table it points to -
    "! each with its json_kind (see the type). The components of an include
    "! are listed in its place, under the names the structure addresses
    "! them by (a RENAMING WITH SUFFIX applied, an include of an include
    "! resolved): the included view of the structure, which is what ajson
    "! walks. Empty when the reference points to anything else (a scalar, a
    "! sorted or hashed table, a table of scalars), when any component - an
    "! included one too - is no elementary one (a nested structure, a
    "! table, a reference), or when a component has no json_kind - so a
    "! caller that gets rows can write every cell of every row as a scalar,
    "! and one that gets none takes the general road
    CLASS-METHODS rtti_get_flat_components
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE ty_t_flat_comp.

    "! Whether the type of the data object behind a data reference names a
    "! component with a namespace (`/ABC/FIELD`) - in the structure itself,
    "! in the line of the table it is, and in every structure and table line
    "! below them (an include's components count, under the names the
    "! structure addresses them by). References are not followed, and a
    "! reference RTTI cannot describe answers false
    CLASS-METHODS rtti_check_namespaced
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_get_typedescr_by_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_typedescr.

    TYPES:
      BEGIN OF ty_s_sel_tab_type,
        tabledescr       TYPE REF TO cl_abap_tabledescr,
        check_table_line TYPE abap_bool,
      END OF ty_s_sel_tab_type.

    " Builds a working table type from the line type of the source table. If
    " the source line is elementary it is wrapped into a `TAB_LINE` component
    " (check_table_line reports whether that happened). By default no extra
    " column is added; when add_sel_field = abap_true an additional boolean
    " flag column named sel_field_name (default `ZZSELKZ`) is inserted.
    " Keeps the dynamic RTTI type construction (describe_by_data / create) in
    " one place so it can be ported once for non-ABAP runtimes.
    " `ir_tab` keeps its Hungarian name against the `val` convention used by
    " the rest of this class: abap2UI5's frozen src/99 package passes it as a
    " NAMED argument and cannot be edited, so renaming it there is blocked -
    " and a rename only in this catalog would show up as drift on every sync.
    " Rename it once that consumer drops the call.
    CLASS-METHODS rtti_create_sel_tab_type
      IMPORTING
        ir_tab         TYPE REF TO data
        add_sel_field  TYPE abap_bool DEFAULT abap_false
        sel_field_name TYPE clike     DEFAULT `ZZSELKZ`
      RETURNING
        VALUE(result)  TYPE ty_s_sel_tab_type.

    CLASS-METHODS msg_get
      IMPORTING
        VALUE(val)    TYPE any
        VALUE(val2)   TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    CLASS-METHODS msg_get_collect
      IMPORTING
        VALUE(val)    TYPE any
        VALUE(val2)   TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_by_msg
      IMPORTING
        id            TYPE any
        no            TYPE any
        v1            TYPE any OPTIONAL
        v2            TYPE any OPTIONAL
        v3            TYPE any OPTIONAL
        v4            TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    " depth guards the mutual recursion between these two (an include whose
    " expansion reaches itself would otherwise be an unbounded stack).
    " Optional and last in the list, so every existing caller is unaffected
    CLASS-METHODS rtti_get_t_attri_by_include
      IMPORTING
        !type         TYPE REF TO cl_abap_datadescr
        depth         TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    CLASS-METHODS expand_components
      IMPORTING
        val           TYPE abap_component_tab
        depth         TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    TYPES:
      BEGIN OF ty_s_fix_val,
        low   TYPE string,
        high  TYPE string,
        descr TYPE string,
      END OF ty_s_fix_val.
    TYPES ty_t_fix_val TYPE STANDARD TABLE OF ty_s_fix_val WITH DEFAULT KEY.

    CLASS-METHODS rtti_get_t_ddic_fixed_values
      IMPORTING
        rollname      TYPE clike
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE ty_t_fix_val ##NEEDED.

    CLASS-METHODS check_bound_a_not_initial
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_unassign_initial
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS unassign_object
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE REF TO object.

    CLASS-METHODS unassign_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS conv_get_as_data_ref
      IMPORTING
        val           TYPE data
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS itab_get_itab_by_csv
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS itab_get_csv_by_itab
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS filter_itab
      IMPORTING
        filter TYPE ty_t_filter_multi
      CHANGING
        val     TYPE STANDARD TABLE.

    CLASS-METHODS filter_get_multi_by_data
      IMPORTING
        val           TYPE data
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS filter_get_data_by_multi
      IMPORTING
        val           TYPE ty_t_filter_multi
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS filter_get_sql_where
      IMPORTING
        val           TYPE ty_t_filter_multi
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS filter_get_multi_by_sql_where
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS filter_get_sql_by_sql_string
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_sql.

    CLASS-METHODS url_param_get
      IMPORTING
        val           TYPE clike
        url           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_param_create_url
      IMPORTING
        t_params      TYPE ty_t_name_value
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_param_set
      IMPORTING
        url           TYPE string
        !name         TYPE string
        !value        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_classname_by_ref
      IMPORTING
        val           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_intfname_by_ref
      IMPORTING
        !in           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS x_get_last_t100
      IMPORTING
        val           TYPE REF TO cx_root
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS x_check_raise
      IMPORTING
        v     TYPE clike DEFAULT `CX_SY_SUBRC`
        !when TYPE abap_bool.

    CLASS-METHODS x_raise
      IMPORTING
        v TYPE clike DEFAULT `CX_SY_SUBRC`.

    CLASS-METHODS json_stringify
      IMPORTING
        !any          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS xml_parse
      IMPORTING
        !xml TYPE clike
      EXPORTING
        !any TYPE any.

    CLASS-METHODS xml_stringify
      IMPORTING
        !any          TYPE any
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_xslt_serialization_error.

    CLASS-METHODS boolean_check_by_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS boolean_abap_2_json
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS json_parse
      IMPORTING
        val   TYPE any
      CHANGING
        !data TYPE any.

    CLASS-METHODS c_trim_upper
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS xml_srtti_stringify
      IMPORTING
        !data         TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS xml_srtti_parse
      IMPORTING
        rtti_data     TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS time_get_timestampl
      RETURNING
        VALUE(result) TYPE timestampl.

    "! The current time stamp in whole seconds, as GET TIME STAMP fills a
    "! TIMESTAMP: truncated. time_get_timestampl assigned to a TIMESTAMP
    "! field rounds instead, and can land a second in the future.
    CLASS-METHODS time_get_timestamp
      RETURNING
        VALUE(result) TYPE timestamp.

    CLASS-METHODS time_subtract_seconds
      IMPORTING
        !time         TYPE timestampl
        !seconds      TYPE i
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS c_trim
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_trim_lower
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_param_get_tab
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS rtti_get_t_attri_by_oref
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_attrdescr_tab.

    CLASS-METHODS rtti_get_t_attri_by_any
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS rtti_get_t_attri_by_table_name
      IMPORTING
        table_name    TYPE any
      RETURNING
        VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS rtti_get_type_name
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_class_exists
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_create_tab_by_name
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! A standard table whose line is built from components - the
    "! structdescr, the tabledescr and the CREATE DATA in one call.
    "! cx_sy_struct_creation and cx_sy_table_creation reach the caller.
    "! @parameter val    | the components, e.g. from rtti_get_t_attri_by_table_name
    "! @parameter result | a reference to the new, empty table
    CLASS-METHODS rtti_create_tab_by_comps
      IMPORTING
        val           TYPE abap_component_tab
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! Whether a type of that name exists - a DDIC object, a class, an
    "! interface. Never raises.
    CLASS-METHODS rtti_check_type_exists
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_check_type_kind_dref
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_get_type_kind
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_ref_data
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS boolean_check_by_name
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS filter_update_tokens
      IMPORTING
        val           TYPE ty_t_filter_multi
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_filter_multi.

    CLASS-METHODS filter_get_range_t_by_token_t
      IMPORTING
        val           TYPE ty_t_token
      RETURNING
        VALUE(result) TYPE ty_t_range.

    CLASS-METHODS filter_get_range_by_token
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_range.

    "! A range for one value the user typed: CP when it holds a `*`, EQ
    "! otherwise - the select-option behaviour of a search field.
    "! @parameter val    | the value; initial gives an empty range, which selects everything
    "! @parameter result | one line, sign I
    CLASS-METHODS filter_get_range_by_value
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_range.

    "! A range for a list of values, one line per distinct value - CP for a
    "! value with a `*`, EQ otherwise. An empty value is kept as EQ space,
    "! so a list can ask for "blank" on purpose.
    "! @parameter val    | the values, in any order, duplicates allowed
    "! @parameter result | sorted by value, sign I; empty for an empty list
    CLASS-METHODS filter_get_range_t_by_value_t
      IMPORTING
        val           TYPE string_table
      RETURNING
        VALUE(result) TYPE ty_t_range.

    CLASS-METHODS filter_get_token_t_by_range_t
      IMPORTING
        val           TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE ty_t_token ##NEEDED.

    CLASS-METHODS filter_get_token_range_mapping
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS itab_corresponding
      IMPORTING
        val  TYPE STANDARD TABLE
      CHANGING
        !tab TYPE STANDARD TABLE.

    CLASS-METHODS itab_filter_by_val
      IMPORTING
        val         TYPE clike
        fields      TYPE string_table OPTIONAL
        ignore_case TYPE abap_bool DEFAULT abap_false
      CHANGING
        !tab        TYPE STANDARD TABLE.

    CLASS-METHODS itab_get_by_struc
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS itab_filter_by_t_range
      IMPORTING
        val  TYPE ty_t_filter_multi
      CHANGING
        !tab TYPE STANDARD TABLE.

    CLASS-METHODS time_get_time_by_stampl
      IMPORTING
        val           TYPE timestampl
      RETURNING
        VALUE(result) TYPE t.

    CLASS-METHODS time_get_date_by_stampl
      IMPORTING
        val           TYPE timestampl
      RETURNING
        VALUE(result) TYPE d.

    "! A time stamp as readable ISO text: `2026-11-23 14:05:09`, the fraction
    "! only when it is not zero (`2026-11-23 14:05:09,1234567`).
    "! @parameter val       | the time stamp - a short TIMESTAMP is passed as CONV timestampl( )
    "! @parameter time_zone | the zone to show it in; empty: UTC, as stored
    "! @parameter result    | the text
    CLASS-METHODS time_get_text_by_stampl
      IMPORTING
        val           TYPE timestampl
        time_zone     TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS conv_copy_ref_data
      IMPORTING
        !from         TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS conv_get_xstring_by_data_uri
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS rtti_tab_get_relative_name
      IMPORTING
        !table        TYPE any
      RETURNING
        VALUE(result) TYPE string.

    "! Run a conversion exit: CONVERSION_EXIT_&lt;convexit&gt;_INPUT, or _OUTPUT.
    "!
    "!   conv_exit( EXPORTING convexit = `MATN1` CHANGING value = lv_matnr ).
    "!   conv_exit( EXPORTING convexit = `ALPHA` from = i_vbeln CHANGING value = lv_docno ).
    "!
    "! A value the exit refuses - a unit that does not exist, a material
    "! number too long - raises zabaputil_cx_util_error with the exit's own
    "! message; VALUE is then left as it was. A caller that can live with
    "! the unconverted value catches it and carries on.
    "!
    "! @parameter convexit | the exit's name: ALPHA, MATN1, CUNIT, HUID, ...
    "! @parameter output   | abap_true: the _OUTPUT direction
    "! @parameter from     | the value to convert, when it is not VALUE itself. The
    "!                       target's type decides the result - ALPHA pads to its length
    "! @parameter value    | converted in place, or the target of FROM
    "! @raising zabaputil_cx_util_error | the exit refused the value, or does not exist
    CLASS-METHODS conv_exit
      IMPORTING
        convexit TYPE clike
        output   TYPE abap_bool DEFAULT abap_false
        from     TYPE any       OPTIONAL
      CHANGING
        value    TYPE data
      RAISING
        zabaputil_cx_util_error.

    CLASS-METHODS rtti_check_clike
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_contains
      IMPORTING
        val           TYPE clike
        sub           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_starts_with
      IMPORTING
        val           TYPE clike
        prefix        TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_ends_with
      IMPORTING
        val           TYPE clike
        suffix        TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS c_split
      IMPORTING
        val           TYPE clike
        sep           TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS c_join
      IMPORTING
        tab           TYPE string_table
        sep           TYPE clike DEFAULT ``
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_table
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_check_structure
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_check_numeric
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS time_add_seconds
      IMPORTING
        !time         TYPE timestampl
        !seconds      TYPE i
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS time_get_stampl_by_date_time
      IMPORTING
        !date         TYPE d
        !time         TYPE t
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS time_diff_seconds
      IMPORTING
        !time_from    TYPE timestampl
        !time_to      TYPE timestampl
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS conv_string_to_date
      IMPORTING
        val           TYPE clike
        format        TYPE clike DEFAULT `YYYY-MM-DD`
      RETURNING
        VALUE(result) TYPE d.

    CLASS-METHODS conv_date_to_string
      IMPORTING
        val           TYPE d
        format        TYPE clike DEFAULT `YYYY-MM-DD`
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS app_get_url
      IMPORTING
        VALUE(classname) TYPE clike
        !origin          TYPE clike
        !pathname        TYPE clike
        !search          TYPE clike
        !hash            TYPE clike OPTIONAL
      RETURNING
        VALUE(result)    TYPE string.

    CLASS-METHODS app_get_url_source_code
      IMPORTING
        !classname    TYPE clike
        !origin       TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " ========== String Extras ==========

    CLASS-METHODS c_pad_left
      IMPORTING
        val           TYPE clike
        len           TYPE i
        pad           TYPE c DEFAULT '0'
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_pad_right
      IMPORTING
        val           TYPE clike
        len           TYPE i
        pad           TYPE c DEFAULT ' '
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_truncate
      IMPORTING
        val           TYPE clike
        max           TYPE i
        ellipsis      TYPE string DEFAULT `...`
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_substring_safe
      IMPORTING
        val           TYPE clike
        off           TYPE i DEFAULT 0
        len           TYPE i DEFAULT -1
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_replace_all
      IMPORTING
        val           TYPE clike
        sub           TYPE clike
        new_val       TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_is_blank
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    " ========== Number Formatting ==========

    CLASS-METHODS conv_number_to_string
      IMPORTING
        val           TYPE numeric
        decimals      TYPE i DEFAULT -1
        sep_thousands TYPE c DEFAULT ''
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS conv_string_to_number
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE decfloat34.

    " ========== i18n / Text Resolution ==========

    " ========== Itab Extras ==========

    CLASS-METHODS itab_sort_by
      IMPORTING
        fieldname  TYPE clike
        descending TYPE abap_bool DEFAULT abap_false
      CHANGING
        tab        TYPE STANDARD TABLE.

    CLASS-METHODS itab_slice
      IMPORTING
        tab           TYPE STANDARD TABLE
        !from         TYPE i DEFAULT 1
        !to           TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS itab_paginate
      IMPORTING
        tab           TYPE STANDARD TABLE
        page          TYPE i DEFAULT 1
        page_size     TYPE i DEFAULT 20
      EXPORTING
        VALUE(result) TYPE REF TO data
        total_count   TYPE i
        total_pages   TYPE i.

    CLASS-METHODS itab_to_json
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS itab_from_json
      IMPORTING
        val  TYPE clike
      CHANGING
        data TYPE any.

    CLASS-METHODS itab_count_by
      IMPORTING
        tab           TYPE STANDARD TABLE
        fieldname     TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    " ========== Validation Helpers ==========

    CLASS-METHODS check_is_email
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_is_numeric_string
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Is val, surrounding blanks aside, one to nine digits - a text that
    "! converts into an i without rounding (`1.5`), a sign (`+1`, `-1`), an
    "! exponent (`1e3`) or an overflow. For a number that comes from outside
    "! (a browser, a request) and must never end it with a dump.
    CLASS-METHODS check_int_digits
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_is_date_valid
      IMPORTING
        val           TYPE clike
        format        TYPE clike DEFAULT `YYYY-MM-DD`
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_is_guid
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_max_length
      IMPORTING
        val           TYPE clike
        max           TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.

    " ========== Barcodes (GS1) ==========

    TYPES:
      "! one element string of a GS1 barcode (GS1-128, GS1 DataMatrix, GS1 QR)
      BEGIN OF ty_s_gs1,
        "! application identifier, e.g. `01`, `10`, `3103`
        ai       TYPE string,
        "! GS1 data title, e.g. `GTIN`, `BATCH/LOT`, `NET WEIGHT (kg)`
        title    TYPE string,
        "! the data field as scanned, without AI and separator
        value    TYPE string,
        "! implied decimal places of a measure or amount AI (31nn-36nn, 39nn)
        decimals TYPE i,
        "! the value as a date for the date AIs (11, 12, 13, 15, 16, 17, 7006)
        date     TYPE d,
      END OF ty_s_gs1.
    TYPES ty_t_gs1 TYPE STANDARD TABLE OF ty_s_gs1 WITH EMPTY KEY.

    "! Splits a GS1 barcode into its element strings. Accepts the raw scanner
    "! output (optional symbology identifier such as `]C1`/`]d2`/`]Q3`,
    "! variable-length fields terminated by the group separator GS, ASCII 29)
    "! and the human-readable form `(01)09506000134352(10)ABC123`.
    "! Raises zabaputil_cx_util_error for an unknown AI, a field that is too
    "! short or too long, non-digits in a numeric field, an invalid date or
    "! (check_digit = abap_true) a wrong check digit in SSCC, GTIN and GLN.
    "! @parameter separator | additional group separator a scanner sends
    "!                        instead of GS, e.g. `|` or `~`
    "! @parameter ref_date  | reference date for the century of YYMMDD dates
    "!                        (GS1 sliding window), default today
    CLASS-METHODS barcode_gs1_parse
      IMPORTING
        val           TYPE clike
        separator     TYPE clike     OPTIONAL
        check_digit   TYPE abap_bool DEFAULT abap_true
        ref_date      TYPE d         OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_gs1.

    "! Builds a GS1 element string from AI/value pairs - the inverse of
    "! barcode_gs1_parse. A group separator is inserted after every
    "! variable-length field that is followed by another field.
    "! @parameter separator      | separator instead of GS (ASCII 29)
    "! @parameter human_readable | abap_true: `(01)...(10)...` for the label text
    CLASS-METHODS barcode_gs1_build
      IMPORTING
        t_gs1          TYPE ty_t_gs1
        separator      TYPE clike     OPTIONAL
        human_readable TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)  TYPE string.

    "! The value of an AI in a parsed barcode, initial if it is missing.
    "! A three-digit AI matches the four-digit measure AIs, so `310` finds
    "! `3103` whatever the decimal places are.
    CLASS-METHODS barcode_gs1_get_value
      IMPORTING
        t_gs1         TYPE ty_t_gs1
        ai            TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The GS1 check digit (modulo 10, weights 3-1 from the right) for the
    "! digits of a GTIN-8/12/13/14, GLN, SSCC etc. without the check digit.
    CLASS-METHODS barcode_gs1_calc_check_digit
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! abap_true if the last digit of val is the correct GS1 check digit
    CLASS-METHODS check_is_gs1_check_digit
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    " ========== Barcodes (own formats) ==========

    TYPES:
      "! one field of a barcode format that is not GS1 - a label of one's own
      "! or of a supplier. Which components count depends on the method:
      "! barcode_split_separator reads the fields in their order,
      "! barcode_split_position by pos and len, barcode_split_prefix by prefix
      BEGIN OF ty_s_barcode_field,
        "! name of the field in the result
        name     TYPE string,
        "! what the field starts with (barcode_split_prefix)
        prefix   TYPE string,
        "! first character of the field, counted from 1 (barcode_split_position)
        pos      TYPE i,
        "! number of characters. barcode_split_position: 0 = to the end;
        "! the other two: the most the field may hold, 0 = any number
        len      TYPE i,
        "! the field may be missing or empty
        optional TYPE abap_bool,
      END OF ty_s_barcode_field.
    TYPES ty_t_barcode_field TYPE STANDARD TABLE OF ty_s_barcode_field WITH EMPTY KEY.

    TYPES:
      "! one field of a split barcode
      BEGIN OF ty_s_barcode_value,
        name  TYPE string,
        value TYPE string,
      END OF ty_s_barcode_value.
    TYPES ty_t_barcode_value TYPE STANDARD TABLE OF ty_s_barcode_value WITH EMPTY KEY.

    "! Splits a barcode whose fields follow each other with a separator
    "! between them: `4711|LOT42|12` with the fields material, batch and
    "! quantity. The n-th part is the n-th field. A trailing CR/LF of a
    "! scanner in keyboard mode is dropped.
    "! Raises zabaputil_cx_util_error for more parts than fields, a missing
    "! field that is not optional and a value longer than len.
    "! @parameter result | one row per field of t_field that has a value
    CLASS-METHODS barcode_split_separator
      IMPORTING
        val           TYPE clike
        separator     TYPE clike
        t_field       TYPE ty_t_barcode_field
      RETURNING
        VALUE(result) TYPE ty_t_barcode_value.

    "! Splits a barcode of fixed layout: every field has its place, given by
    "! pos (from 1) and len (0 = to the end). Blanks around a value - the
    "! padding of a fixed layout - are dropped.
    "! Raises zabaputil_cx_util_error for a field without pos and for a field
    "! that is not optional and lies beyond the end of the barcode or is
    "! empty.
    "! @parameter result | one row per field of t_field that has a value
    CLASS-METHODS barcode_split_position
      IMPORTING
        val           TYPE clike
        t_field       TYPE ty_t_barcode_field
      RETURNING
        VALUE(result) TYPE ty_t_barcode_value.

    "! Splits a barcode whose fields say what they are by a prefix: `P4711`
    "! is a material, `Q12` a quantity. One scan may hold one field or
    "! several with a separator between them - the group separator GS
    "! (ASCII 29) unless another one is named. The longest matching prefix
    "! wins, so `1T` is not read as `1`.
    "! Raises zabaputil_cx_util_error for a part that starts with no known
    "! prefix, a field that comes twice, a missing field that is not
    "! optional and a value longer than len.
    "! @parameter separator | separator a scanner sends instead of GS
    "! @parameter result    | one row per field found, in the order of t_field
    CLASS-METHODS barcode_split_prefix
      IMPORTING
        val           TYPE clike
        t_field       TYPE ty_t_barcode_field
        separator     TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_barcode_value.

    " ========== Binary tables ==========

    "! The bytes of a binary table (SOLIX, BAPICONTEN, W3MIME, TLINE holding
    "! raw bytes, ...) as one xstring - the inverse of conv_get_bintab_by_xstring.
    "! @parameter length | number of bytes to keep, default all lines in full
    CLASS-METHODS conv_get_xstring_by_bintab
      IMPORTING
        val           TYPE STANDARD TABLE
        length        TYPE i OPTIONAL
      RETURNING
        VALUE(result) TYPE xstring.

    " ========== Parallel Processing ==========

    TYPES ty_t_object TYPE STANDARD TABLE OF REF TO object WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_parallel,
        "! position of the task in t_task
        index   TYPE i,
        "! the task after it ran - in parallel mode the copy that came back
        "! from the parallel session, holding the results it set
        task    TYPE REF TO object,
        "! error text if the task failed, initial otherwise
        message TYPE string,
      END OF ty_s_parallel.
    TYPES ty_t_parallel TYPE STANDARD TABLE OF ty_s_parallel WITH EMPTY KEY.

    "! Runs tasks in parallel sessions. Every task is an object implementing
    "! IF_ABAP_PARALLEL (method DO, attributes carry input and result); it is
    "! serialized into a parallel session and comes back with its results.
    "! Uses CL_ABAP_PARALLEL (ABAP Cloud and newer Standard ABAP releases).
    "! Where that class is missing, or with sequential = abap_true (debugging),
    "! the tasks run one after the other in this session by calling `method`.
    "! @parameter tasks      | maximum number of parallel tasks, default by the system
    "! @parameter timeout    | timeout in seconds, default by the system
    "! @parameter sequential | abap_true: run in this session, one after the other
    "! @parameter method     | the method called in sequential mode
    CLASS-METHODS parallel_run
      IMPORTING
        t_task        TYPE ty_t_object
        tasks         TYPE i         DEFAULT 0
        timeout       TYPE i         DEFAULT 0
        sequential    TYPE abap_bool DEFAULT abap_false
        method        TYPE clike     DEFAULT `IF_ABAP_PARALLEL~DO`
      RETURNING
        VALUE(result) TYPE ty_t_parallel.

    " ========== Long Texts (SAPscript) ==========

    TYPES:
      BEGIN OF ty_s_longtext_line,
        "! paragraph format: `*` new paragraph, `/` new line, `=` continuous,
        "! initial = continuation separated by a blank
        format TYPE string,
        line   TYPE string,
      END OF ty_s_longtext_line.
    TYPES ty_t_longtext_line TYPE STANDARD TABLE OF ty_s_longtext_line WITH EMPTY KEY.

    "! A long text (READ_TEXT) as one string, lines separated by newline.
    "! Raises zabaputil_cx_util_error if the text does not exist. Standard ABAP only.
    "! @parameter object | text object, e.g. `VBBK`, `MATERIAL`
    "! @parameter id     | text id, e.g. `0001`, `GRUN`
    "! @parameter name   | text name, usually the object key
    "! @parameter langu  | SAP or ISO language
    CLASS-METHODS longtext_read
      IMPORTING
        object        TYPE clike
        id            TYPE clike
        name          TYPE clike
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE string.

    "! Saves a long text (SAVE_TEXT), creating or replacing it. Newlines start
    "! a new paragraph, longer lines are split at 132 characters. The text is
    "! written with the next COMMIT WORK. Standard ABAP only.
    CLASS-METHODS longtext_save
      IMPORTING
        object TYPE clike
        id     TYPE clike
        name   TYPE clike
        val    TYPE clike
        langu  TYPE clike DEFAULT sy-langu.

    "! Deletes a long text (DELETE_TEXT); a missing text is no error. Standard ABAP only.
    CLASS-METHODS longtext_delete
      IMPORTING
        object TYPE clike
        id     TYPE clike
        name   TYPE clike
        langu  TYPE clike DEFAULT sy-langu.

    "! A string as SAPscript lines: every newline starts a paragraph (`*`),
    "! lines longer than 132 characters continue with format `=`
    CLASS-METHODS longtext_to_lines
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_longtext_line.

    "! SAPscript lines as a string - the inverse of longtext_to_lines
    CLASS-METHODS longtext_from_lines
      IMPORTING
        val           TYPE ty_t_longtext_line
      RETURNING
        VALUE(result) TYPE string.

    " ========== RFC ==========

    "! Calls a function module by name, locally or via RFC, with its parameters
    "! as JSON: `{"IV_MATNR":"4711","IT_ITEMS":[{...}]}` - names are the
    "! parameter names, optional importing parameters that are not given keep
    "! their default. Returns the exporting, changing and tables parameters as
    "! JSON. A classic exception raises zabaputil_cx_util_error with its name.
    "! The parameter types must exist in this system. Standard ABAP only.
    "! @parameter params      | importing, changing and tables parameters as JSON object
    "! @parameter destination | RFC destination, initial: call in this session
    CLASS-METHODS rfc_call_dynamic
      IMPORTING
        function      TYPE clike
        params        TYPE clike OPTIONAL
        destination   TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! Pings an RFC destination (RFC_PING) and raises zabaputil_cx_util_error
    "! with the communication or system failure text if it cannot be reached.
    "! On ABAP Cloud `destination` is the name of a BTP destination.
    CLASS-METHODS rfc_ping
      IMPORTING
        destination TYPE clike.

    "! abap_true if the RFC destination can be reached - see rfc_ping
    CLASS-METHODS rfc_check_destination
      IMPORTING
        destination   TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    " ========== BAPI ==========
    " Calling BAPIs and function modules: one message table, one LUW
    " contract. The message table (ct_bapiret / it_bapiret) is ANY table
    " whose rows carry the components of BAPIRET2 - TYPE, ID, NUMBER,
    " MESSAGE, MESSAGE_V1 to MESSAGE_V4 (BAPIRET2, BAPIRET1, BAPIRETURN1 and
    " the like; a component the row type does not have stays out). No DDIC
    " type in a signature and every module called by name, so the class
    " compiles everywhere - the modules themselves are Standard ABAP.

    "! Ends a call of a BAPI: sets cv_is_error when ct_bapiret holds a
    "! message of type E, A or X, and with iv_commit = abap_true commits
    "! (BAPI_TRANSACTION_COMMIT, wait) on success or rolls back
    "! (BAPI_TRANSACTION_ROLLBACK) on error. Without iv_commit the caller
    "! owns the LUW and nothing is committed. cv_is_error is only ever set,
    "! never cleared - an error flagged before survives success messages.
    "! A commit that cannot be called (ABAP Cloud) is added as error.
    CLASS-METHODS bapi_finish
      IMPORTING
        iv_commit   TYPE abap_bool DEFAULT abap_false
      CHANGING
        ct_bapiret  TYPE ANY TABLE
        cv_is_error TYPE abap_bool.

    "! abap_true if the message table holds a message of type E, A or X
    CLASS-METHODS bapi_check_is_error
      IMPORTING
        it_bapiret      TYPE ANY TABLE
      RETURNING
        VALUE(rv_error) TYPE abap_bool.

    "! Adds the message in sy-msgid/sy-msgno/sy-msgv1..4, as left by a function
    "! module call with EXCEPTIONS error_message - as an error whatever its
    "! type, A and X kept. Without one, an error naming iv_context.
    CLASS-METHODS bapi_msg_add_sy
      IMPORTING
        iv_context TYPE clike OPTIONAL
      CHANGING
        ct_bapiret TYPE ANY TABLE.

    "! Adds a message with a free text
    CLASS-METHODS bapi_msg_add_text
      IMPORTING
        iv_text    TYPE clike
        iv_type    TYPE clike DEFAULT 'E'
      CHANGING
        ct_bapiret TYPE ANY TABLE.

    "! Adds an exception as error message, with the texts of its previous chain
    CLASS-METHODS bapi_msg_add_exception
      IMPORTING
        ix_error   TYPE REF TO cx_root
      CHANGING
        ct_bapiret TYPE ANY TABLE.

    "! Adds a single return structure (a BAPI's RETURN parameter), if it is
    "! filled - its components move by name into a new row
    CLASS-METHODS bapi_msg_add_bapiret2
      IMPORTING
        is_return  TYPE any
      CHANGING
        ct_bapiret TYPE ANY TABLE.

    "! Adds the rows of any message table: BAPIRET1/2, BAPI_CORU_RETURN,
    "! BAPI_MATRETURN2, PROTT, MESG and the like - the fields are found by
    "! name (TYPE/MSGTY, ID/MSGID/ARBGB, NUMBER/MSGNO/TXTNR, MESSAGE_V1/MSGV1, ...)
    "! by msg_get_t
    CLASS-METHODS bapi_msg_add_table
      IMPORTING
        it_table   TYPE ANY TABLE
      CHANGING
        ct_bapiret TYPE ANY TABLE.

    "! Calls a function module by name with a parameter table - for modules
    "! whose types are not in every system (EWM). A classic exception is
    "! added as message from sy-msg, a runtime error (unknown module or
    "! parameter) as exception text.
    CLASS-METHODS bapi_call_function
      IMPORTING
        iv_name    TYPE clike
        it_param   TYPE abap_func_parmbind_tab
      CHANGING
        ct_bapiret TYPE ANY TABLE.

    "! Fills a material number into a BAPI structure: into &lt;field&gt;_LONG
    "! where the structure has it (S/4HANA, 40 characters), else into &lt;field&gt;
    CLASS-METHODS bapi_set_material
      IMPORTING
        iv_matnr TYPE clike
        iv_field TYPE clike DEFAULT 'MATERIAL'
      CHANGING
        cs_data  TYPE any.

    "! Resets the buffers of the logistics function modules, so that several
    "! postings in one session do not see stale data. Nothing happens where
    "! a module does not exist (ABAP Cloud).
    "! @parameter iv_area | DELIVERY, HU, SERIAL or ALL
    CLASS-METHODS bapi_refresh_buffers
      IMPORTING
        iv_area TYPE clike DEFAULT 'ALL'.

    "! A document or object number in the internal format: a number padded
    "! with leading zeros to iv_length, anything else unchanged - the
    "! conversion exit ALPHA (input) with a variable length, without the
    "! module (7.02 takes no ALPHA = IN WIDTH with a variable width)
    CLASS-METHODS conv_alpha_in
      IMPORTING
        iv_value         TYPE clike
        iv_length        TYPE i DEFAULT 10
      RETURNING
        VALUE(rv_result) TYPE string.

    " ========== MIME Repository ==========

    "! Reads a file of the MIME repository, e.g. `/SAP/PUBLIC/ZMY/logo.png`.
    "! Standard ABAP only.
    CLASS-METHODS mime_read
      IMPORTING
        url           TYPE clike
      RETURNING
        VALUE(result) TYPE xstring.

    "! Creates or replaces a file in the MIME repository. Standard ABAP only.
    "! @parameter package   | package of a new file, default `$TMP`
    "! @parameter transport | transport request, needed outside local packages
    CLASS-METHODS mime_save
      IMPORTING
        url       TYPE clike
        content   TYPE xstring
        package   TYPE clike DEFAULT `$TMP`
        transport TYPE clike OPTIONAL.

    " ========== Spool & Forms ==========

    "! A spool request as PDF: SAPscript/Smart Forms (OTF), ABAP lists and
    "! Adobe forms (ADS). Standard ABAP only.
    CLASS-METHODS spool_get_pdf
      IMPORTING
        spool_id      TYPE i
      RETURNING
        VALUE(result) TYPE xstring.

    "! Renders an Adobe form (PDF-based print form, SFP) to PDF. The components
    "! of `data` are passed to the form interface parameters of the same name.
    "! Standard ABAP only.
    "! @parameter form_name | name of the form
    "! @parameter data      | structure with one component per interface parameter
    "! @parameter langu     | form language
    CLASS-METHODS form_get_pdf
      IMPORTING
        form_name     TYPE clike
        data          TYPE any OPTIONAL
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE xstring.

    " ========== Report Variants ==========

    TYPES:
      BEGIN OF ty_s_variant,
        variant TYPE string,
        text    TYPE string,
      END OF ty_s_variant.
    TYPES ty_t_variant TYPE STANDARD TABLE OF ty_s_variant WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_variant_value,
        "! name of the parameter or select-option
        selname TYPE string,
        "! `P` parameter, `S` select-option
        kind    TYPE string,
        sign    TYPE string,
        option  TYPE string,
        low     TYPE string,
        high    TYPE string,
      END OF ty_s_variant_value.
    TYPES ty_t_variant_value TYPE STANDARD TABLE OF ty_s_variant_value WITH EMPTY KEY.

    "! The variants of a report with their texts in langu. Standard ABAP only.
    CLASS-METHODS variant_list
      IMPORTING
        report        TYPE clike
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE ty_t_variant.

    "! The values of a report variant (RS_VARIANT_CONTENTS). Standard ABAP only.
    CLASS-METHODS variant_read
      IMPORTING
        report        TYPE clike
        variant       TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_variant_value.

    " ========== ABAP Messaging Channels ==========

    "! Sends a text message to an ABAP Messaging Channel - subscribers (e.g.
    "! a WebSocket via ABAP Push Channel) get it at once. Standard ABAP only.
    "! @parameter application_id       | AMC application, e.g. `ZMY_AMC`
    "! @parameter channel_id           | channel, e.g. `/updates`
    "! @parameter channel_extension_id | optional extension, e.g. a document number
    CLASS-METHODS amc_send
      IMPORTING
        application_id       TYPE clike
        channel_id           TYPE clike
        message              TYPE clike
        channel_extension_id TYPE clike OPTIONAL.

    " ========== Attachments (Generic Object Services) ==========

    TYPES:
      BEGIN OF ty_s_gos,
        doc_id      TYPE string,
        file_name   TYPE string,
        description TYPE string,
        file_ext    TYPE string,
        size        TYPE i,
        created_by  TYPE string,
        created_on  TYPE d,
      END OF ty_s_gos.
    TYPES ty_t_gos TYPE STANDARD TABLE OF ty_s_gos WITH EMPTY KEY.

    "! Attaches a file to a business object (GOS attachment list), e.g.
    "! object_type `BUS2012` with a purchase order number. Commits the
    "! relationship (BINARY_RELATION_CREATE_COMMIT). Standard ABAP only.
    CLASS-METHODS gos_attach
      IMPORTING
        object_type   TYPE clike
        object_key    TYPE clike
        file_name     TYPE clike
        content       TYPE xstring
        description   TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! The GOS attachments of a business object. Standard ABAP only.
    CLASS-METHODS gos_list
      IMPORTING
        object_type   TYPE clike
        object_key    TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_gos.

    "! The content of a GOS attachment (doc_id from gos_list). Standard ABAP only.
    CLASS-METHODS gos_read
      IMPORTING
        doc_id        TYPE clike
      RETURNING
        VALUE(result) TYPE xstring.

    " ========== Deep Comparison ==========

    CLASS-METHODS data_equals
      IMPORTING
        a             TYPE any
        b             TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    TYPES:
      BEGIN OF ty_s_field_diff,
        fieldname TYPE string,
        old_value TYPE string,
        new_value TYPE string,
      END OF ty_s_field_diff.
    TYPES ty_t_field_diff TYPE STANDARD TABLE OF ty_s_field_diff WITH EMPTY KEY.

    CLASS-METHODS data_diff
      IMPORTING
        old           TYPE any
        new           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_field_diff.

    " ========== Stopwatch ==========

    CLASS-METHODS time_measure_start
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS time_measure_stop
      IMPORTING
        start_time    TYPE timestampl
      RETURNING
        VALUE(result) TYPE i.

    " ========== Authorization Check ==========

    " ========== Enum/Domain Helpers ==========

    CLASS-METHODS enum_to_text
      IMPORTING
        domain        TYPE clike
        !value        TYPE clike
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS enum_get_all
      IMPORTING
        domain        TYPE clike
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    " ========== Deep Field Access ==========

    CLASS-METHODS data_get_by_path
      IMPORTING
        data          TYPE any
        path          TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS data_set_by_path
      IMPORTING
        path  TYPE clike
        !value TYPE clike
      CHANGING
        data  TYPE any.

    " ========== Lock Extensions ==========

    " ========== Number Range ==========

    " ========== Change Documents ==========

    " ========== Background Job ==========

    " ========== Email ==========

    TYPES:
      BEGIN OF ty_syst,
        index TYPE i,
        pagno TYPE i,
        tabix TYPE i,
        tfill TYPE i,
        tlopc TYPE i,
        tmaxl TYPE i,
        toccu TYPE i,
        ttabc TYPE i,
        tstis TYPE i,
        ttabi TYPE i,
        dbcnt TYPE i,
        fdpos TYPE i,
        colno TYPE i,
        linct TYPE i,
        linno TYPE i,
        linsz TYPE i,
        pagct TYPE i,
        macol TYPE i,
        marow TYPE i,
        tleng TYPE i,
        sfoff TYPE i,
        willi TYPE i,
        lilli TYPE i,
        subrc TYPE i,
        fleng TYPE i,
        cucol TYPE i,
        curow TYPE i,
        lsind TYPE i,
        listi TYPE i,
        stepl TYPE i,
        tpagi TYPE i,
        winx1 TYPE i,
        winy1 TYPE i,
        winx2 TYPE i,
        winy2 TYPE i,
        winco TYPE i,
        winro TYPE i,
        windi TYPE i,
        srows TYPE i,
        scols TYPE i,
        loopc TYPE i,
        folen TYPE i,
        fodec TYPE i,
        tzone TYPE i,
        dayst TYPE c LENGTH 1,
        ftype TYPE c LENGTH 1,
        appli TYPE x LENGTH 2,
        fdayw TYPE int1,
        ccurs TYPE p LENGTH 9 DECIMALS 0,
        ccurt TYPE p LENGTH 9 DECIMALS 0,
        debug TYPE c LENGTH 1,
        ctype TYPE c LENGTH 1,
        input TYPE c LENGTH 1,
        langu TYPE c LENGTH 1,
        modno TYPE i,
        batch TYPE c LENGTH 1,
        binpt TYPE c LENGTH 1,
        calld TYPE c LENGTH 1,
        dynnr TYPE c LENGTH 4,
        dyngr TYPE c LENGTH 4,
        newpa TYPE c LENGTH 1,
        pri40 TYPE c LENGTH 1,
        rstrt TYPE c LENGTH 1,
        wtitl TYPE c LENGTH 1,
        cpage TYPE i,
        dbnam TYPE c LENGTH 20,
        mandt TYPE c LENGTH 3,
        prefx TYPE c LENGTH 3,
        fmkey TYPE c LENGTH 3,
        pexpi TYPE n LENGTH 1,
        prini TYPE n LENGTH 1,
        primm TYPE c LENGTH 1,
        prrel TYPE c LENGTH 1,
        playo TYPE c LENGTH 5,
        prbig TYPE c LENGTH 1,
        playp TYPE c LENGTH 1,
        prnew TYPE c LENGTH 1,
        prlog TYPE c LENGTH 1,
        pdest TYPE c LENGTH 4,
        plist TYPE c LENGTH 12,
        pauth TYPE n LENGTH 2,
        prdsn TYPE c LENGTH 6,
        pnwpa TYPE c LENGTH 1,
        callr TYPE c LENGTH 8,
        repi2 TYPE c LENGTH 40,
        rtitl TYPE c LENGTH 70,
        prrec TYPE c LENGTH 12,
        prtxt TYPE c LENGTH 68,
        prabt TYPE c LENGTH 12,
        lpass TYPE c LENGTH 4,
        nrpag TYPE c LENGTH 1,
        paart TYPE c LENGTH 16,
        prcop TYPE n LENGTH 3,
        batzs TYPE c LENGTH 1,
        bspld TYPE c LENGTH 1,
        brep4 TYPE c LENGTH 4,
        batzo TYPE c LENGTH 1,
        batzd TYPE c LENGTH 1,
        batzw TYPE c LENGTH 1,
        batzm TYPE c LENGTH 1,
        ctabl TYPE c LENGTH 4,
        dbsys TYPE c LENGTH 10,
        dcsys TYPE c LENGTH 4,
        macdb TYPE c LENGTH 4,
        sysid TYPE c LENGTH 8,
        opsys TYPE c LENGTH 10,
        pfkey TYPE c LENGTH 20,
        saprl TYPE c LENGTH 4,
        tcode TYPE c LENGTH 20,
        ucomm TYPE c LENGTH 70,
        cfwae TYPE c LENGTH 5,
        chwae TYPE c LENGTH 5,
        spono TYPE n LENGTH 10,
        sponr TYPE n LENGTH 10,
        waers TYPE c LENGTH 5,
        cdate TYPE d,
        datum TYPE d,
        slset TYPE c LENGTH 14,
        subty TYPE x LENGTH 1,
        subcs TYPE c LENGTH 1,
        group TYPE c LENGTH 1,
        ffile TYPE c LENGTH 8,
        uzeit TYPE t,
        dsnam TYPE c LENGTH 8,
        tabid TYPE c LENGTH 8,
        tfdsn TYPE c LENGTH 8,
        uname TYPE c LENGTH 12,
        lstat TYPE c LENGTH 16,
        abcde TYPE c LENGTH 26,
        marky TYPE c LENGTH 1,
        sfnam TYPE c LENGTH 30,
        tname TYPE c LENGTH 30,
        msgli TYPE c LENGTH 60,
        title TYPE c LENGTH 70,
        entry TYPE c LENGTH 72,
        lisel TYPE c LENGTH 255,
        uline TYPE c LENGTH 255,
        xcode TYPE c LENGTH 70,
        cprog TYPE c LENGTH 40,
        xprog TYPE c LENGTH 40,
        xform TYPE c LENGTH 30,
        ldbpg TYPE c LENGTH 40,
        tvar0 TYPE c LENGTH 20,
        tvar1 TYPE c LENGTH 20,
        tvar2 TYPE c LENGTH 20,
        tvar3 TYPE c LENGTH 20,
        tvar4 TYPE c LENGTH 20,
        tvar5 TYPE c LENGTH 20,
        tvar6 TYPE c LENGTH 20,
        tvar7 TYPE c LENGTH 20,
        tvar8 TYPE c LENGTH 20,
        tvar9 TYPE c LENGTH 20,
        msgid TYPE c LENGTH 20,
        msgty TYPE c LENGTH 1,
        msgno TYPE n LENGTH 3,
        msgv1 TYPE c LENGTH 50,
        msgv2 TYPE c LENGTH 50,
        msgv3 TYPE c LENGTH 50,
        msgv4 TYPE c LENGTH 50,
        oncom TYPE c LENGTH 1,
        vline TYPE c LENGTH 1,
        winsl TYPE c LENGTH 79,
        staco TYPE i,
        staro TYPE i,
        datar TYPE c LENGTH 1,
        host  TYPE c LENGTH 32,
        locdb TYPE c LENGTH 1,
        locop TYPE c LENGTH 1,
        datlo TYPE d,
        timlo TYPE t,
        zonlo TYPE c LENGTH 6,
      END OF ty_syst.

    TYPES:
      BEGIN OF ty_s_data_element_text,
        header TYPE string,
        short  TYPE string,
        medium TYPE string,
        long   TYPE string,
      END OF ty_s_data_element_text.

    TYPES:
      BEGIN OF ty_s_class_descr,
        classname   TYPE string,
        description TYPE string,
      END OF ty_s_class_descr.
    TYPES ty_t_classes TYPE STANDARD TABLE OF ty_s_class_descr WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_stack,
        class   TYPE string,
        include TYPE string,
        method  TYPE string,
        line    TYPE string,
      END OF ty_s_stack.
    TYPES ty_t_stack TYPE STANDARD TABLE OF ty_s_stack WITH DEFAULT KEY.

    CLASS-METHODS context_get_callstack
      RETURNING
        VALUE(result) TYPE ty_t_stack.

    CLASS-METHODS context_get_tenant
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS context_get_sy
      RETURNING
        VALUE(result) TYPE ty_syst.

    CLASS-METHODS check_abap_cloud
      RETURNING
        VALUE(result) TYPE abap_bool.

    " the system fields sy_get hands out, in the types SYST gives them
    TYPES:
      BEGIN OF ty_s_sy,
        datum TYPE d,
        uzeit TYPE t,
        uname TYPE c LENGTH 12,
        sysid TYPE c LENGTH 8,
        saprl TYPE c LENGTH 4,
      END OF ty_s_sy.

    " THE one place that reads a system field for its VALUE - date, time,
    " user, system id, release. Every other class asks here
    " (sy_get( )-uname), for two reasons: ABAP for Cloud Development reports
    " the old system fields on every statement that names one ("The old
    " variant of SY-DATUM should not be used ..."), so one reader is one
    " finding per field instead of one per use; and the day one of them
    " has to come from another API, it changes here and nowhere else.
    " Not meant: sy-subrc, sy-tabix and sy-index - those are the result of
    " the statement before them and have to be read where it stands
    CLASS-METHODS sy_get
      RETURNING
        VALUE(result) TYPE ty_s_sy.

    CLASS-METHODS context_get_user_tech
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS uuid_get_c32
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS uuid_get_c22
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_data_element_texts
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_data_element_text.

    "! One text of a data element, picked from rtti_get_data_element_texts.
    "! @parameter val    | data element name
    "! @parameter type   | H header, S short, M medium, L long; anything else is short
    "! @parameter result | the text
    CLASS-METHODS rtti_get_data_element_text
      IMPORTING
        val           TYPE clike
        type          TYPE clike DEFAULT `S`
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS conv_decode_x_base64
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS conv_encode_x_base64
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS conv_get_string_by_xstring
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS conv_get_xstring_by_string
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    "! Cut an XSTRING into the lines of a binary table, as
    "! SCMS_XSTRING_TO_BINARY does - without the function module, so it
    "! runs on ABAP Cloud too. The line length comes from the table: a line
    "! of one byte field (x255, SOLIX) or a structure whose first component
    "! is one (BAPICONTEN-LINE, x1022). The last line is padded with zero
    "! bytes; xstrlen( val ) is the real length.
    "! @parameter val    | the bytes
    "! @parameter result | the binary table, any of the two line shapes
    CLASS-METHODS conv_get_bintab_by_xstring
      IMPORTING
        val    TYPE xstring
      EXPORTING
        result TYPE STANDARD TABLE.

    " read_description defaults to off: filling it costs one repository
    " read PER implementing class (SEO_CLASS_READ / XCO), and the common
    " caller - interface implementer discovery - only ever reads the
    " classname. Optional and last in the list, so it is additive
    CLASS-METHODS rtti_get_classes_impl_intf
      IMPORTING
        val              TYPE clike
        read_description TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE ty_t_classes.

    " abap_true for values that can be rendered into a text without a
    " conversion dump - the elementary types. Anything structured (struct,
    " table, reference) must be excluded before a generic `|{ val }|`.
    CLASS-METHODS rtti_check_printable
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    " The source-code position an exception was raised at, as
    " `<program> / <include> / line <n>`; empty when the runtime supplies
    " none. Wraps cx_root->get_source_position so the dependency on the SAP
    " standard exception API stays inside this class. This is what identifies
    " the failing method for exceptions that carry no text of their own -
    " a CX_SY_MOVE_CAST_ERROR only says which types did not match, the
    " position says where.
    CLASS-METHODS error_get_source_position
      IMPORTING
        val           TYPE REF TO cx_root
      RETURNING
        VALUE(result) TYPE string.

    " Every public, non-static, non-constant attribute of an exception that
    " has a printable value - the class-specific payload the text alone does
    " not carry (source/target type of a cast error, the offending value of a
    " conversion error, DB object of an SQL error, ...). The general cx_root
    " attributes (PREVIOUS, TEXTID, IS_RESUMABLE, KERNEL_ERRID) are skipped -
    " the caller renders them itself or they carry no information.
    CLASS-METHODS error_get_attributes
      IMPORTING
        val           TYPE REF TO cx_root
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS rtti_get_t_fixvalues
      IMPORTING
        elemdescr     TYPE REF TO cl_abap_elemdescr
        langu         TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_fix_val.

    CLASS-METHODS rtti_get_table_desrc
      IMPORTING
        tabname       TYPE clike
        langu         TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string ##NEEDED.


    CLASS-METHODS msg_get_text
      IMPORTING
        val           TYPE any
        val2          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_by_sy
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS msg_map
      IMPORTING
        name          TYPE clike
        val           TYPE data
        msg           TYPE ty_s_msg
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    TYPES: BEGIN OF ty_usr01,
             mandt         TYPE c LENGTH 3,     " Client
             bname         TYPE c LENGTH 12,    " User Name
             stcod         TYPE c LENGTH 20,    " Start Menu (old)
             spld          TYPE c LENGTH 4,     " Output Device
             splg          TYPE c LENGTH 1,     " Print Parameter 1
             spdb          TYPE c LENGTH 1,     " Print Parameter 2
             spda          TYPE c LENGTH 1,     " Print Parameter 3
             datfm         TYPE c LENGTH 1,     " Date Format
             dcpfm         TYPE c LENGTH 1,     " Decimal Format
             hdest         TYPE c LENGTH 8,     " Host Destination
             hmand         TYPE c LENGTH 3,     " Default Host Client
             hname         TYPE c LENGTH 12,    " Default Host Username
             menon         TYPE c LENGTH 1,     " Automatic Start
             menue         TYPE c LENGTH 20,    " Menu Name
             strtt         TYPE c LENGTH 20,    " Start Menu (again)
             langu         TYPE c LENGTH 1,     " Logon Language
             cattkennz     TYPE c LENGTH 1,     " CATT Test Status
             timefm        TYPE c LENGTH 1,     " Time Format (12h/24h)
             ianatzonecode TYPE n LENGTH 4,     " IANA Timezone Code
           END OF ty_usr01.

    TYPES:
      BEGIN OF ty_s_dfies,
        tabname     TYPE c LENGTH 30,
        fieldname   TYPE c LENGTH 30,
        langu       TYPE string,
        position    TYPE n LENGTH 4,
        offset      TYPE n LENGTH 6,
        domname     TYPE c LENGTH 30,
        rollname    TYPE c LENGTH 30,
        checktable  TYPE c LENGTH 30,
        leng        TYPE n LENGTH 6,
        intlen      TYPE n LENGTH 6,
        outputlen   TYPE n LENGTH 6,
        decimals    TYPE n LENGTH 6,
        datatype    TYPE c LENGTH 4,
        inttype     TYPE c LENGTH 1,
        reftable    TYPE c LENGTH 30,
        reffield    TYPE c LENGTH 30,
        precfield   TYPE c LENGTH 30,
        authorid    TYPE c LENGTH 3,
        memoryid    TYPE c LENGTH 20,
        logflag     TYPE c LENGTH 1,
        mask        TYPE c LENGTH 20,
        masklen     TYPE n LENGTH 4,
        convexit    TYPE c LENGTH 5,
        headlen     TYPE n LENGTH 2,
        scrlen1     TYPE n LENGTH 2,
        scrlen2     TYPE n LENGTH 2,
        scrlen3     TYPE n LENGTH 2,
        fieldtext   TYPE c LENGTH 60,
        reptext     TYPE c LENGTH 55,
        scrtext_s   TYPE c LENGTH 10,
        scrtext_m   TYPE c LENGTH 20,
        scrtext_l   TYPE c LENGTH 40,
        keyflag     TYPE c LENGTH 1,
        lowercase   TYPE c LENGTH 1,
        mac         TYPE c LENGTH 1,
        genkey      TYPE c LENGTH 1,
        noforkey    TYPE c LENGTH 1,
        valexi      TYPE c LENGTH 1,
        noauthch    TYPE c LENGTH 1,
        sign        TYPE c LENGTH 1,
        dynpfld     TYPE c LENGTH 1,
        f4availabl  TYPE c LENGTH 1,
        comptype    TYPE c LENGTH 1,
        lfieldname  TYPE c LENGTH 132,
        ltrflddis   TYPE c LENGTH 1,
        bidictrlc   TYPE c LENGTH 1,
        outputstyle TYPE n LENGTH 2,
        nohistory   TYPE c LENGTH 1,
        ampmformat  TYPE c LENGTH 1,
      END OF ty_s_dfies,
      ty_t_dfies TYPE STANDARD TABLE OF ty_s_dfies WITH DEFAULT KEY.

    CLASS-METHODS convexit_ext
      IMPORTING
        name   TYPE ty_s_dfies
        val    TYPE data
      CHANGING
        result TYPE data.

    CLASS-METHODS rtti_get_t_dfies_by_table_name
      IMPORTING
        table_name    TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_dfies.

    "! The DDIC field description of a data element - length, type,
    "! conversion exit, texts. Empty on ABAP Cloud, which has no DFIES,
    "! and for a name that is no DDIC data element.
    "! @parameter val    | data element name
    "! @parameter result | the description
    CLASS-METHODS rtti_get_s_dfies_by_rollname
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_dfies.

    CLASS-METHODS tab_get_where_by_dfies
      IMPORTING
        mv_check_tab_field TYPE string
        ms_data_row        TYPE REF TO data
        it_dfies           TYPE ty_t_dfies
      RETURNING
        VALUE(result)      TYPE string.

    TYPES trobj_name     TYPE c LENGTH 120.
    TYPES sxco_transport TYPE c LENGTH 20.
    TYPES:
      BEGIN OF ty_s_transport,
        short_description TYPE string,
        transport         TYPE sxco_transport,
        task              TYPE sxco_transport,
        selkz             TYPE abap_bool,
        locl              TYPE abap_bool,
      END OF ty_s_transport.

    TYPES ty_t_data TYPE STANDARD TABLE OF ty_s_transport WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_dfies_2,
        tabname     TYPE c LENGTH 30,  " Table Name
        fieldname   TYPE c LENGTH 30,  " Field Name
        langu       TYPE c LENGTH 1,   " Language Key
        position    TYPE n LENGTH 4,   " Position of the field in the table
        offset      TYPE n LENGTH 6,   " Offset of a field
        domname     TYPE c LENGTH 30,  " Domain name
        rollname    TYPE c LENGTH 30,  " Data element (semantic domain)
        checktable  TYPE c LENGTH 30,  " Check Table
        leng        TYPE n LENGTH 6,   " Length (Characters)
        intlen      TYPE n LENGTH 6,   " Internal Length (Bytes)
        outputlen   TYPE n LENGTH 6,   " Output Length
        decimals    TYPE n LENGTH 6,   " Number of Decimal Places
        datatype    TYPE c LENGTH 4,   " Dynpro Data Type
        inttype     TYPE c LENGTH 1,   " ABAP Data Type (C,D,N,...)
        reftable    TYPE c LENGTH 30,  " Reference Table
        reffield    TYPE c LENGTH 30,  " Reference Field
        precfield   TYPE c LENGTH 30,  " Included Table Name
        authorid    TYPE c LENGTH 3,   " Authorization Class
        memoryid    TYPE c LENGTH 20,  " Set/Get Parameter ID
        logflag     TYPE c LENGTH 1,   " Change Documents Indicator
        mask        TYPE c LENGTH 20,  " Template
        masklen     TYPE n LENGTH 4,   " Template Length
        convexit    TYPE c LENGTH 5,   " Conversion Routine
        headlen     TYPE n LENGTH 2,   " Heading Length
        scrlen1     TYPE n LENGTH 2,   " Short Field Label Length
        scrlen2     TYPE n LENGTH 2,   " Medium Field Label Length
        scrlen3     TYPE n LENGTH 2,   " Long Field Label Length
        fieldtext   TYPE c LENGTH 60,  " Short Description
        reptext     TYPE c LENGTH 55,  " Heading
        scrtext_s   TYPE c LENGTH 10,  " Short Field Label
        scrtext_m   TYPE c LENGTH 20,  " Medium Field Label
        scrtext_l   TYPE c LENGTH 40,  " Long Field Label
        keyflag     TYPE c LENGTH 1,   " Key Field Indicator
        lowercase   TYPE c LENGTH 1,   " Lowercase Allowed
        mac         TYPE c LENGTH 1,   " Search Help Attached
        genkey      TYPE c LENGTH 1,   " Flag (X or Blank)
        noforkey    TYPE c LENGTH 1,   " Flag (X or Blank)
        valexi      TYPE c LENGTH 1,   " Fixed Values Exist
        noauthch    TYPE c LENGTH 1,   " Flag (X or Blank)
        sign        TYPE c LENGTH 1,   " Sign Flag
        dynpfld     TYPE c LENGTH 1,   " Field Displayed on Dynpro
        f4availabl  TYPE c LENGTH 1,   " Input Help Available
        comptype    TYPE c LENGTH 1,   " Component Type
        lfieldname  TYPE c LENGTH 132, " Long Field Name
        ltrflddis   TYPE c LENGTH 1,   " Left-to-Right Write Direction
        bidictrlc   TYPE c LENGTH 1,   " No BIDI Character Filtering
        outputstyle TYPE n LENGTH 2,   " Output Style (Decfloat Types)
        nohistory   TYPE c LENGTH 1,   " Input History Deactivated
        ampmformat  TYPE c LENGTH 1,   " AM/PM Time Format Indicator
      END OF ty_s_dfies_2.
    TYPES ty_t_dfies_2 TYPE STANDARD TABLE OF ty_s_dfies_2 WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_shlp_intdescr,
        issimple         TYPE c LENGTH 1,   " Elementary Search Help Flag
        hotkey           TYPE c LENGTH 1,   " Hot Key
        selmtype         TYPE c LENGTH 1,   " Category of Selection Method
        selmethod        TYPE c LENGTH 30,  " Selection Method Name
        texttab          TYPE c LENGTH 30,  " Text Table Name
        selmexit         TYPE c LENGTH 30,  " Search Help Exit
        dialogtype       TYPE c LENGTH 1,   " Dialog Type
        ddlanguage       TYPE c LENGTH 1,   " Language Key
        ddtext           TYPE c LENGTH 60,  " Short Text
        dialoginfo       TYPE c LENGTH 1,   " Flag: SELFIELDS/LISTFIELDS read
        f4state          TYPE c LENGTH 1,   " Internal Usage Only
        tabname          TYPE c LENGTH 30,  " Table Name
        fieldname        TYPE c LENGTH 30,  " Field Name
        title            TYPE c LENGTH 60,  " Title Text
        history          TYPE c LENGTH 1,   " Deprecated Usage
        handle           TYPE int4,         " Reference Handle (int4)
        autosuggest      TYPE c LENGTH 1,   " Autosuggest Flag
        fuzzy_search     TYPE c LENGTH 1,   " Fuzzy Search Flag
        fuzzy_similarity TYPE p DECIMALS 1 LENGTH 2, " Accuracy for Fuzzy Search (DEC 2,1)
      END OF ty_shlp_intdescr.

    TYPES:
      BEGIN OF ty_ddshiface,
        shlpfield  TYPE c LENGTH 30,  " Field Name for Pass by Value to F4 Help
        valtabname TYPE c LENGTH 30,  " Structure Name for Input Help Assignment
        valfield   TYPE c LENGTH 132, " Field for Input Help Assignment
        value      TYPE c LENGTH 132, " Field Content from Dynpro
        internal   TYPE c LENGTH 1,   " Flag: Internal Representation
        dispfield  TYPE c LENGTH 1,   " Display-Only Field Flag
        f4field    TYPE c LENGTH 1,   " F4 Pressed Flag
        topshlpnam TYPE c LENGTH 30,  " Higher-Level Search Help Name
        topshlpfld TYPE c LENGTH 30,  " Field of Higher-Level Search Help
      END OF ty_ddshiface.

    TYPES:
      BEGIN OF ty_ddshfprop,
        fieldname  TYPE c LENGTH 30, " Name of Search Help Parameter
        shlpinput  TYPE c LENGTH 1,  " IMPORT Parameter Flag
        shlpoutput TYPE c LENGTH 1,  " EXPORT Parameter Flag
        shlpselpos TYPE n LENGTH 2,  " Position in Dialog Box
        shlplispos TYPE n LENGTH 2,  " Position in Hit List
        shlpseldis TYPE c LENGTH 1,  " Display Field in Dialog Box
        defaultval TYPE c LENGTH 21, " Default Value
      END OF ty_ddshfprop.

    TYPES:
      BEGIN OF ty_ddshselopt,
        shlpname  TYPE c LENGTH 30, " Name of Search Help
        shlpfield TYPE c LENGTH 30, " Name of Search Help Parameter
        sign      TYPE c LENGTH 1,  " Include/Exclude Flag (I/E)
        option    TYPE c LENGTH 2,  " Selection Option (EQ/BT/CP/..)
        low       TYPE c LENGTH 45, " Low Value for Selection
        high      TYPE c LENGTH 45, " High Value for Selection
      END OF ty_ddshselopt.

    TYPES:
      BEGIN OF ty_ddshtextsearch_field,
        fieldname TYPE c LENGTH 30, " Name of a searchable field (freely chosen, not specified by SAP)
      END OF ty_ddshtextsearch_field.

    TYPES tt_ddshtextsearch_fields TYPE STANDARD TABLE OF ty_ddshtextsearch_field WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_ddshtextsearch,
        request TYPE c LENGTH 60,              " Text Search Request
        fields  TYPE tt_ddshtextsearch_fields, " Fields eligible for text search
      END OF ty_ddshtextsearch.

    TYPES:
      BEGIN OF ty_shlp_descr,
        shlpname   TYPE c LENGTH 30,       " Name of a Search Help
        shlptype   TYPE c LENGTH 2,        " Type of an input help (fixed values)
        intdescr   TYPE ty_shlp_intdescr,  " Placeholder for Internal Info of Search Help
        interface  TYPE STANDARD TABLE OF ty_ddshiface WITH EMPTY KEY,                      " Placeholder for Interface of Search Help
        fielddescr TYPE STANDARD TABLE OF ty_s_dfies_2 WITH EMPTY KEY,
        fieldprop  TYPE STANDARD TABLE OF ty_ddshfprop WITH EMPTY KEY,
        selopt     TYPE STANDARD TABLE OF ty_ddshselopt WITH EMPTY KEY,
        textsearch TYPE ty_ddshtextsearch,
      END OF ty_shlp_descr.

    CLASS-METHODS bus_tr_read
      RETURNING
        VALUE(mt_data) TYPE ty_t_data.

    CLASS-METHODS bus_tr_add
      IMPORTING
        ir_data      TYPE REF TO data
        iv_tabname   TYPE string
        is_transport TYPE ty_s_transport
        iv_commit    TYPE abap_bool DEFAULT abap_true.

    "! iv_max_rows - at most this many rows (F4IF_SELECT_VALUES-MAXRECORDS);
    "! 0 reads all of them, as before the parameter existed
    CLASS-METHODS bus_search_help_read
      IMPORTING
        iv_max_rows    TYPE i DEFAULT 0
      CHANGING
        ms_shlp        TYPE ty_shlp_descr
        mv_fname       TYPE string
        mv_table       TYPE string
        mr_data        TYPE REF TO data
        mt_result_desc TYPE ty_t_dfies_2
        mv_shlpfield   TYPE string
        mt_data        TYPE REF TO data
        ms_data_row    TYPE REF TO data.

    " ========== Business Application Log (BAL) ==========

    TYPES:
      BEGIN OF ty_s_bal_header,
        log_handle  TYPE string,
        object      TYPE string,
        subobject   TYPE string,
        external_id TYPE string,
        log_date    TYPE d,
        log_time    TYPE t,
        user        TYPE string,
        msg_count   TYPE i,
      END OF ty_s_bal_header.
    TYPES ty_t_bal_header TYPE STANDARD TABLE OF ty_s_bal_header WITH EMPTY KEY.

    CLASS-METHODS bal_search
      IMPORTING
        object        TYPE clike OPTIONAL
        subobject     TYPE clike OPTIONAL
        id            TYPE clike OPTIONAL
        date_from     TYPE d OPTIONAL
        date_to       TYPE d OPTIONAL
        !user         TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_bal_header.

    CLASS-METHODS bal_read_latest
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    CLASS-METHODS bal_delete_before
      IMPORTING
        object    TYPE clike
        subobject TYPE clike OPTIONAL
        !days     TYPE i DEFAULT 30.

    CLASS-METHODS bal_read_by_type
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
        msg_type      TYPE clike DEFAULT `E`
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS bal_count
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS bal_read
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS bal_create
      IMPORTING
        object    TYPE clike
        subobject TYPE clike
        id        TYPE clike
        t_log     TYPE ty_t_msg.

    CLASS-METHODS bal_update
      IMPORTING
        object    TYPE clike
        subobject TYPE clike
        id        TYPE clike
        t_log     TYPE ty_t_msg.

    CLASS-METHODS bal_delete
      IMPORTING
        object    TYPE clike
        subobject TYPE clike
        id        TYPE clike.

    " ========== Transport Requests ==========

    TYPES:
      BEGIN OF ty_s_tr_object,
        pgmid    TYPE string,
        object   TYPE string,
        obj_name TYPE string,
      END OF ty_s_tr_object.
    TYPES ty_t_tr_object TYPE STANDARD TABLE OF ty_s_tr_object WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_tr_request,
        trkorr      TYPE string,
        description TYPE string,
        owner       TYPE string,
        status      TYPE string,
        type        TYPE string,
      END OF ty_s_tr_request.
    TYPES ty_t_tr_request TYPE STANDARD TABLE OF ty_s_tr_request WITH EMPTY KEY.

    CLASS-METHODS tr_get_objects
      IMPORTING
        trkorr        TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_tr_object.

    CLASS-METHODS tr_get_user_requests
      IMPORTING
        !user         TYPE clike DEFAULT sy-uname
        request_type  TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_tr_request.

    CLASS-METHODS tr_get_description
      IMPORTING
        trkorr        TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS tr_is_released
      IMPORTING
        trkorr        TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS tr_add_object
      IMPORTING
        trkorr  TYPE clike
        pgmid   TYPE clike DEFAULT 'R3TR'
        object  TYPE clike
        obj_name TYPE clike.

    CLASS-METHODS tr_create
      IMPORTING
        text          TYPE clike
        target        TYPE clike
        type          TYPE clike DEFAULT `T`
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS tr_release
      IMPORTING
        trkorr       TYPE clike
        ignore_locks TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS tr_copy_objects
      IMPORTING
        source      TYPE clike
        destination TYPE clike.

    CLASS-METHODS tr_import
      IMPORTING
        trkorr         TYPE clike
        target_system  TYPE clike
        client         TYPE clike OPTIONAL
        ignore_version TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result)  TYPE i.

    CLASS-METHODS tr_check_status
      IMPORTING
        trkorr   TYPE clike
        system   TYPE clike
      EXPORTING
        imported TYPE abap_bool
        rc       TYPE i.

    " ========== Enqueue/Dequeue Locks ==========

    TYPES:
      BEGIN OF ty_s_lock_param,
        name  TYPE c LENGTH 30,
        value TYPE string,
      END OF ty_s_lock_param.
    TYPES ty_t_lock_param TYPE STANDARD TABLE OF ty_s_lock_param WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_lock,
        lock_object TYPE string,
        argument    TYPE string,
        user        TYPE string,
        mode        TYPE string,
        client      TYPE string,
        date        TYPE d,
        time        TYPE t,
        owner       TYPE string,
        owner_vb    TYPE string,
      END OF ty_s_lock.
    TYPES ty_t_lock TYPE STANDARD TABLE OF ty_s_lock WITH DEFAULT KEY.

    CLASS-METHODS lock_set
      IMPORTING
        val           TYPE clike
        t_param       TYPE ty_t_lock_param OPTIONAL
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS lock_set_wait
      IMPORTING
        val           TYPE clike
        t_param       TYPE ty_t_lock_param OPTIONAL
        retries       TYPE i DEFAULT 5
        delay_ms      TYPE i DEFAULT 500
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS lock_is_locked
      IMPORTING
        val           TYPE clike
        t_param       TYPE ty_t_lock_param OPTIONAL
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS lock_get_owner
      IMPORTING
        val           TYPE clike
        t_param       TYPE ty_t_lock_param OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS lock_get_dequeue_by_enqueue
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS lock_read
      IMPORTING
        lock_object   TYPE clike OPTIONAL
        !user         TYPE clike OPTIONAL
        client        TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_lock.

    CLASS-METHODS lock_delete
      IMPORTING
        val           TYPE clike
        t_param       TYPE ty_t_lock_param OPTIONAL
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS lock_delete_entries
      IMPORTING
        t_lock        TYPE ty_t_lock
      RETURNING
        VALUE(result) TYPE abap_bool.

    " ========== System Services ==========

    TYPES:
      BEGIN OF ty_s_changdoc,
        changenr  TYPE string,
        username  TYPE string,
        udate     TYPE d,
        utime     TYPE t,
        tcode     TYPE string,
        fieldname TYPE string,
        old_value TYPE string,
        new_value TYPE string,
        tabname   TYPE string,
        chngind   TYPE string,
      END OF ty_s_changdoc.
    TYPES ty_t_changdoc TYPE STANDARD TABLE OF ty_s_changdoc WITH EMPTY KEY.

    CLASS-METHODS auth_check
      IMPORTING
        object        TYPE clike
        field         TYPE clike
        !value        TYPE clike
        activity      TYPE clike DEFAULT '03'
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS text_get
      IMPORTING
        msgid         TYPE clike
        msgno         TYPE clike
        v1            TYPE clike OPTIONAL
        v2            TYPE clike OPTIONAL
        v3            TYPE clike OPTIONAL
        v4            TYPE clike OPTIONAL
        langu         TYPE clike DEFAULT sy-langu
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS mail_send
      IMPORTING
        !to      TYPE string
        subject  TYPE string
        body     TYPE string
        html     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS job_submit_report
      IMPORTING
        report          TYPE clike
        variant         TYPE clike OPTIONAL
        start_immediate TYPE abap_bool DEFAULT abap_true
        job_name        TYPE clike OPTIONAL
      RETURNING
        VALUE(result)   TYPE string.

    CLASS-METHODS numrange_get_next
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike DEFAULT `01`
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS changdoc_read
      IMPORTING
        objectclass   TYPE clike
        objectid      TYPE clike
        date_from     TYPE d DEFAULT '19000101'
        date_to       TYPE d DEFAULT '99991231'
      RETURNING
        VALUE(result) TYPE ty_t_changdoc.

    " ========== Source Code / Dev Tools ==========

    CLASS-METHODS source_get_method
      IMPORTING
        iv_classname  TYPE clike
        iv_methodname TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS source_get_method2
      IMPORTING
        iv_classname  TYPE clike
        iv_methodname TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS source_get_file_types
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS source_method_to_file
      IMPORTING
        it_source     TYPE string_table
      RETURNING
        VALUE(result) TYPE string.

    " ========== Calendar / Workdays ==========

    CLASS-METHODS cal_get_weekday
      IMPORTING
        !date         TYPE d
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS cal_is_weekend
      IMPORTING
        !date         TYPE d
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS cal_is_workday
      IMPORTING
        !date         TYPE d
        !calendar_id  TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS cal_add_workdays
      IMPORTING
        !date         TYPE d
        !days         TYPE i
        !calendar_id  TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE d.

    CLASS-METHODS cal_count_workdays
      IMPORTING
        !date_from    TYPE d
        !date_to      TYPE d
        !calendar_id  TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE i.

    " ========== XLSX / ZIP ==========

    TYPES:
      BEGIN OF ty_s_zip_file,
        name    TYPE string,
        content TYPE xstring,
      END OF ty_s_zip_file.
    TYPES ty_t_zip_file TYPE STANDARD TABLE OF ty_s_zip_file WITH EMPTY KEY.

    CLASS-METHODS conv_get_xlsx_by_itab
      IMPORTING
        val           TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS conv_get_itab_by_xlsx
      IMPORTING
        val    TYPE xstring
      EXPORTING
        result TYPE REF TO data.

    CLASS-METHODS zip_pack
      IMPORTING
        !files        TYPE ty_t_zip_file
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS zip_unpack
      IMPORTING
        !val          TYPE xstring
      RETURNING
        VALUE(result) TYPE ty_t_zip_file.

    TYPES:
      BEGIN OF ty_s_http_response,
        code   TYPE i,
        reason TYPE string,
        body   TYPE string,
      END OF ty_s_http_response.

    TYPES:
      BEGIN OF ty_s_user_info,
        uname          TYPE string,
        name_formatted TYPE string,
        email          TYPE string,
        langu          TYPE string,
        timezone       TYPE string,
        date_format    TYPE string,
        decimal_format TYPE string,
      END OF ty_s_user_info.

    CLASS-METHODS c_escape_html
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_escape_json
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! Escape a text for a JavaScript string literal in a script - single-
    "! or double-quoted alike: backslash, both quotes, CR and LF, and `</`
    "! so the text cannot end an inline script element.
    CLASS-METHODS c_escape_js
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! Escape a text for an XML attribute value: `&`, `<`, `>`, `"` as
    "! entities, LF, CR and TAB as character references (attribute-value
    "! normalization would turn them into blanks), and the control
    "! characters XML 1.0 forbids outright dropped. Degrades when the
    "! codepage API is missing - see the method.
    CLASS-METHODS c_escape_xml
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The characters of val that occur in allowed, in their order - an
    "! allow-list strip for a value that is reflected back somewhere.
    CLASS-METHODS c_filter_chars
      IMPORTING
        val           TYPE clike
        allowed       TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! val up to the first occurrence of sub - val itself when sub does
    "! not occur (unlike substring_before( ), which answers empty then).
    CLASS-METHODS c_cut_at
      IMPORTING
        val           TYPE clike
        sub           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_levenshtein
      IMPORTING
        val1          TYPE clike
        val2          TYPE clike
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS url_encode
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS url_decode
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The origin (scheme, host, port) of an absolute URL -
    "! `https://host[:port]` or the protocol-relative `//host` - and ``
    "! for a relative one, for one without a host and for one carrying
    "! userinfo. The scheme comes back lower-cased.
    CLASS-METHODS url_get_origin
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The bare host[:port] authority of an Origin, Referer or Host value -
    "! lower-cased, scheme and path/query/fragment dropped, a default port
    "! (:443, :80) dropped too - so two spellings of one authority compare
    "! equal.
    CLASS-METHODS url_get_authority
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! Whether an If-None-Match header matches the current ETag - RFC 7232
    "! weak comparison over a list, `*`, and the `-gzip` suffix Apache's
    "! mod_deflate writes into a tag (see the method).
    CLASS-METHODS http_check_etag_match
      IMPORTING
        if_none_match TYPE clike
        etag          TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS regex_match
      IMPORTING
        val           TYPE clike
        regex         TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS regex_find_all
      IMPORTING
        val           TYPE clike
        regex         TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS regex_replace_all
      IMPORTING
        val           TYPE clike
        regex         TYPE clike
        new_val       TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS uuid_get_c36
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS uuid_conv_c32_to_c36
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS uuid_conv_c36_to_c32
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The URL-safe base64 form (A-Z a-z 0-9 - _) of a c32 UUID. NOT the
    "! alphabet of uuid_get_c22 (the kernel's c22, which uses other
    "! characters and is post-processed there): a c22 from uuid_get_c22 does
    "! not convert back with uuid_conv_c22_to_c32, and the c22 of a c32 here
    "! differs from the kernel's for the same UUID - convert only between
    "! the forms of these two methods
    CLASS-METHODS uuid_conv_c32_to_c22
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The inverse of uuid_conv_c32_to_c22 - same alphabet, see there
    CLASS-METHODS uuid_conv_c22_to_c32
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS itab_sum_by
      IMPORTING
        tab           TYPE ANY TABLE
        fieldname     TYPE clike
      RETURNING
        VALUE(result) TYPE decfloat34.

    CLASS-METHODS itab_distinct
      IMPORTING
        tab           TYPE ANY TABLE
        fieldname     TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS itab_group_sum_by
      IMPORTING
        tab           TYPE ANY TABLE
        group_by      TYPE clike
        sum_by        TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS num_round
      IMPORTING
        val           TYPE decfloat34
        decimals      TYPE i DEFAULT 0
        mode          TYPE clike DEFAULT `HALF_UP`
      RETURNING
        VALUE(result) TYPE decfloat34.

    CLASS-METHODS file_get_mimetype
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS lang_sap_to_iso
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS lang_iso_to_sap
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS time_get_user_timezone
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS time_stampl_to_tz
      IMPORTING
        val  TYPE timestampl
        tz   TYPE clike DEFAULT `UTC`
      EXPORTING
        date TYPE d
        time TYPE t.

    CLASS-METHODS time_stampl_from_tz
      IMPORTING
        date          TYPE d
        time          TYPE t
        tz            TYPE clike DEFAULT `UTC`
      RETURNING
        VALUE(result) TYPE timestampl.

    CLASS-METHODS context_get_user_info
      IMPORTING
        uname         TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_user_info.

    CLASS-METHODS cur_get_decimals
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS cur_amount_to_external
      IMPORTING
        val           TYPE decfloat34
        currency      TYPE clike OPTIONAL
        decimals      TYPE i DEFAULT -1
      RETURNING
        VALUE(result) TYPE decfloat34.

    CLASS-METHODS cur_amount_to_internal
      IMPORTING
        val           TYPE decfloat34
        currency      TYPE clike OPTIONAL
        decimals      TYPE i DEFAULT -1
      RETURNING
        VALUE(result) TYPE decfloat34.

    CLASS-METHODS unit_convert
      IMPORTING
        val           TYPE decfloat34
        unit_from     TYPE clike
        unit_to       TYPE clike
      RETURNING
        VALUE(result) TYPE decfloat34.

    CLASS-METHODS hash_calculate
      IMPORTING
        val           TYPE clike
        algorithm     TYPE clike DEFAULT `SHA256`
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS hash_hmac
      IMPORTING
        val           TYPE clike
        key           TYPE clike
        algorithm     TYPE clike DEFAULT `SHA256`
      RETURNING
        VALUE(result) TYPE string.

    "! A checksum of a text for a cache validator, not a cryptographic hash:
    "! `bytes-h1-h2-h3`, three modular accumulators over the UTF-8
    "! bytes plus their count. Plain ABAP, so it downports and transpiles;
    "! `` where the byte conversion is unavailable.
    CLASS-METHODS hash_checksum
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS http_get
      IMPORTING
        url           TYPE clike
        t_header      TYPE ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_http_response.

    CLASS-METHODS http_post
      IMPORTING
        url           TYPE clike
        body          TYPE clike
        content_type  TYPE clike DEFAULT `application/json`
        t_header      TYPE ty_t_name_value OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_http_response.

    "! Reset the table buffer of a database table on the application server,
    "! so a buffered customizing table shows a change at once and not only
    "! after the buffer synchronisation. Nothing happens on ABAP Cloud, which
    "! has no SBUF_GENERIC_RESET, nor for an unbuffered table.
    "! @parameter val | the database table
    CLASS-METHODS db_reset_table_buffer
      IMPORTING
        val TYPE clike.

    CLASS-METHODS db_check_table_exists
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS db_select_by_name
      IMPORTING
        tabname       TYPE clike
        where         TYPE clike OPTIONAL
        max_rows      TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS db_count_by_name
      IMPORTING
        tabname       TYPE clike
        where         TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS param_get_user_default
      IMPORTING
        val           TYPE clike
        uname         TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS job_get_status
      IMPORTING
        jobname       TYPE clike
        jobcount      TYPE clike
      RETURNING
        VALUE(result) TYPE string.


    "! A digest of a text for EQUALITY comparisons only - a fixed 64
    "! characters standing in for a text nobody has to read back (the
    "! client-model snapshot a draft carries, z2ui5_cl_ui5_app_cont). SHA-256
    "! over the characters through cl_abap_message_digest (7.02 and up,
    "! released in ABAP Cloud, open-abap-core has it too; named dynamically);
    "! where the class is absent or cannot answer, the text itself is the
    "! answer, so a comparison still holds. The same input always gives the
    "! same answer within one system; it is never stored for longer than a
    "! draft lives
    CLASS-METHODS hash_string
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " abap_true when one component of a row holds the search text - the
    " per-field half of itab_filter_by_val. A component that is not printable
    " (a table of children, a reference) holds no text and answers false
    " FROZEN-ONLY: no caller in src/00 - src/02, kept for src/99
    CLASS-METHODS itab_filter_check_field
      IMPORTING
        field         TYPE any
        search        TYPE string
        ignore_case   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE abap_bool.

    " abap_true when the class exists AND implements the interface. The
    " question to ask before a CREATE OBJECT ... TYPE (name) whose name came
    " from outside (a URL parameter, a hash route, a typed-in class name):
    " answered from the class descriptor, which loads and instantiates
    " nothing, so a class that is no app is refused before its class pool
    " is ever touched. The descriptor's list carries the interfaces a
    " superclass implements as well, so an app that inherits its
    " z2ui5_if_app passes. abap_false for a name that is not a class at all
    " (an interface, a data type, nothing)
    CLASS-METHODS rtti_check_class_impl_intf
      IMPORTING
        class         TYPE clike
        intf          TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    " why a class cannot be described - the exception describe_by_name
    " raised for a class that exists but cannot be loaded (a syntax error in
    " it, a missing dependency); unbound when the class describes fine or
    " does not exist at all. The follow-up question to a "no" of
    " rtti_check_class_impl_intf, so a caller can say "broken" instead of
    " "does not exist". Not cached: it is asked once, after a failure
    CLASS-METHODS rtti_get_class_load_error
      IMPORTING
        class         TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO cx_root.

    " the name of the class or interface an UNBOUND object reference
    " variable is typed with, upper case: `DATA li TYPE REF TO zif_any.`
    " answers `ZIF_ANY`. The way to hand an object name to a dynamic lookup
    " without spelling it as a string literal - the declaration is a
    " reference the compiler, abaplint and a namespace rename (`abaplint
    " --rename`) all see and rewrite, a literal is none of the three. Pass
    " the variable unbound: the transpiled runtime describes a bound one by
    " the class of the object it points to
    CLASS-METHODS rtti_get_ref_type_name
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    "! A UTC time stamp as an ISO 8601 instant, `YYYY-MM-DDTHH:MM:SSZ` -
    "! `0000-00-00T00:00:00Z` for an initial one and for one that is no
    "! valid point in time. Statement for statement what
    "! z2ui5_cl_ajson's lcl_abap_to_json=>format_timestamp does, because a
    "! caller writes it into a document next to what ajson wrote
    "! (z2ui5_cl_ui5_srv_model=>json_row): the two must not differ in a
    "! single character, whatever the platform makes of an odd value
    CLASS-METHODS time_get_iso_by_stamp
      IMPORTING
        val           TYPE timestamp
      RETURNING
        VALUE(result) TYPE string.

    "! The same for a long time stamp, with its fraction -
    "! `YYYY-MM-DDTHH:MM:SS.fZ`, the trailing zeros of the fraction dropped
    "! and a single 0 left of an empty one (format_timestampl of ajson)
    CLASS-METHODS time_get_iso_by_stampl
      IMPORTING
        val           TYPE timestampl
      RETURNING
        VALUE(result) TYPE string.

    "! Milliseconds from time_from to time_to - two time_get_timestampl( )
    "! readings - negative when time_to lies before time_from, 0 when either
    "! is initial. Meant for intervals: one beyond the integer range (about
    "! 24 days) raises a conversion overflow.
    CLASS-METHODS time_diff_milliseconds
      IMPORTING
        time_from     TYPE timestampl
        time_to       TYPE timestampl
      RETURNING
        VALUE(result) TYPE i.

    "! the S-RTTI descriptor object of a data object - the shipped mirror, or
    "! the system's own S-RTTI where it is installed
    CLASS-METHODS xml_srtti_descr
      IMPORTING
        !data         TYPE any
      RETURNING
        VALUE(result) TYPE REF TO object.

    "! The way back for xml_srtti_stringify_pair: one parse per document
    CLASS-METHODS xml_srtti_parse_pair
      IMPORTING
        iv_type       TYPE clike
        iv_data       TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! The S-RTTI descriptor graph and the data of a dynamically typed object
    "! as TWO asXML documents. The former single-document writer put both
    "! into ONE, and the way back then had to lex the whole document twice - once for the
    "! type (skipping every byte of the data), once for the data. Written per
    "! generic reference on every draft save and read back on every draft
    "! load, so a multi-MB table behind a REF TO data paid a redundant full
    "! lex per roundtrip in a stateless session. Same content, split.
    CLASS-METHODS xml_srtti_stringify_pair
      IMPORTING
        !data   TYPE any
      EXPORTING
        ev_type TYPE string
        ev_data TYPE string.

    " An uploaded text file as a string: UTF-8, and Windows-1252 when the
    " bytes are not valid UTF-8 - what Excel's plain "CSV" export writes on
    " Windows. Raises when it is neither.
    CLASS-METHODS conv_get_string_by_file
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

    CLASS-METHODS rtti_get_class_descr_on_cloud
      IMPORTING
        classname     TYPE clike
      RETURNING
        VALUE(result) TYPE string.


    CLASS-METHODS msg_get_internal
      IMPORTING
        val           TYPE any
        " the RAP-shape check of a structure is a property of its TYPE - a
        " table decides it once for its rows (msg_get_internal_tab) and hands
        " the answer down, instead of asking for every row
        iv_check_rap  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS msg_get_by_oref
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS check_is_rap_struct
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS msg_get_rap
      IMPORTING
        val           TYPE any
        entity_name   TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS msg_get_rap_row
      IMPORTING
        val         TYPE any
        entity_name TYPE string OPTIONAL
      EXPORTING
        messages    TYPE ty_t_msg
        is_row      TYPE abap_bool.

    CLASS-METHODS msg_get_rap_element
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_state_area
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_action
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_pid
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_cid
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_tky
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_get_rap_flatten
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_comp_str
      IMPORTING
        val           TYPE any
        comp          TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS scan_flag_prefix
      IMPORTING
        val           TYPE any
        prefix        TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS msg_get_rap_meta
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_name_value.

    CLASS-METHODS msg_get_rap_fail_text
      IMPORTING
        cause         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    " The base value of an IF_ABAP_BEHV=>T_FAIL_CAUSE value, from its name;
    " -1 for a name this release does not know.
    CLASS-METHODS msg_get_rap_cause_value
      IMPORTING
        name          TYPE clike
      RETURNING
        VALUE(result) TYPE i.

    " What a rendered box stays inside. A data dump is a diagnostic, not a
    " report: a message box that carries 10.000 rows or follows a reference
    " graph until it runs out of stack helps nobody, and the roundtrip pays
    " for every character of it. Both limits are announced in the output
    " rather than applied silently.
    CONSTANTS cv_data_max_rows  TYPE i VALUE 100.
    CONSTANTS cv_data_max_depth TYPE i VALUE 5.
    " the headline is one line of a message box - what does not fit there is
    " in the details anyway
    CONSTANTS cv_data_max_text  TYPE i VALUE 200.

    "! Render any value into the HTML fragment the box details show. Depth
    "! is the recursion level, checked against cv_data_max_depth.
    CLASS-METHODS data_render
      IMPORTING
        val           TYPE any
        depth         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS data_render_tab
      IMPORTING
        val           TYPE any
        depth         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS data_render_struc
      IMPORTING
        val           TYPE any
        depth         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS data_render_oref
      IMPORTING
        val           TYPE any
        depth         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS data_render_dref
      IMPORTING
        val           TYPE any
        depth         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! One named value as a list item - the shape a component of a
    "! structure and an attribute of an object both take.
    CLASS-METHODS data_render_item
      IMPORTING
        name          TYPE clike
        val           TYPE any
        depth         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! The one line the box shows without the details being opened.
    CLASS-METHODS data_get_headline
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    "! Any elementary value as its text, and never an exception.
    CLASS-METHODS data_get_string
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    "! The text of an exception object, empty for every other object.
    CLASS-METHODS data_get_exc_text
      IMPORTING
        val           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS html_get_list
      IMPORTING
        items         TYPE string_table
        ordered       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string.

    "! Does this text carry HTML markup?
    CLASS-METHODS html_check
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! HTML as the plain text behind it, capped at cv_data_max_text.
    CLASS-METHODS html_get_plain
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS _set_e071k
      IMPORTING
        ir_data       TYPE REF TO data
        iv_tabname    TYPE string
        is_transport  TYPE ty_s_transport
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS _set_e071
      IMPORTING
        iv_tabname    TYPE string
        is_transport  TYPE ty_s_transport
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS _get_e071k_tabkey
      IMPORTING
        line             TYPE any
        dfies            TYPE ty_t_dfies
      RETURNING
        VALUE(rv_tabkey) TYPE trobj_name.

    CLASS-METHODS _read_e070
      CHANGING
        mt_data TYPE ty_t_data.

    CLASS-METHODS set_mandt
      IMPORTING
        ir_data TYPE REF TO data.

    CLASS-METHODS rtti_get_t_attri_on_prem
      IMPORTING
        tabname       TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_dfies.

    CLASS-METHODS rtti_get_t_attri_on_cloud
      IMPORTING
        tabname       TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_dfies ##NEEDED.

  PRIVATE SECTION.

    " SAP language key : ISO 639-1 code of the common languages, read by
    " lang_sap_to_iso and lang_iso_to_sap before T002 is asked. The SAP key
    " is case-sensitive - lower-case keys are other languages (c Catalan,
    " i Indonesian, ...) and must never be upper-cased onto this list
    CONSTANTS cv_lang_map TYPE string VALUE `0:sr;1:zh;2:th;3:ko;4:ro;5:sl;6:hr;7:ms;8:uk;9:et;A:ar;B:he;C:cs;D:de;E:en;F:fr;G:el;H:hu;I:it;J:ja;K:da;L:pl;M:zf;N:nl;O:no;P:pt;Q:sk;R:ru;S:es;T:tr;U:fi;V:sv;W:bg;X:lt;Y:lv`.

    " one message of the message model as a new row of a BAPIRET-like
    " table, iv_type in place of the message's own type when given
    CLASS-METHODS bapi_msg_add
      IMPORTING
        it_msg     TYPE ty_t_msg
        iv_type    TYPE clike OPTIONAL
      CHANGING
        ct_bapiret TYPE ANY TABLE.

    " the message type of a message row: TYPE or MSGTY
    CLASS-METHODS bapi_get_type
      IMPORTING
        is_data        TYPE any
      RETURNING
        VALUE(rv_type) TYPE string.

    " the first of the components (a blank-separated list) the structure has
    CLASS-METHODS bapi_assign_component
      IMPORTING
        iv_names  TYPE string
        is_data   TYPE any
      RETURNING
        VALUE(rr) TYPE REF TO data.

    " barcode_gs1_*: one row per application identifier (or per three-digit
    " prefix of the four-digit measure/amount AIs)
    TYPES:
      BEGIN OF ty_s_gs1_rule,
        prefix   TYPE string,
        ai_len   TYPE i,
        " fixed length of the data field, or its maximum when var = abap_true
        len      TYPE i,
        var      TYPE abap_bool,
        numeric  TYPE abap_bool,
        title    TYPE string,
        is_date  TYPE abap_bool,
        " the fourth AI digit is the number of implied decimal places
        decimals TYPE abap_bool,
        " the data field is a GS1 key ending with a check digit
        check    TYPE abap_bool,
      END OF ty_s_gs1_rule.
    TYPES ty_t_gs1_rule TYPE STANDARD TABLE OF ty_s_gs1_rule WITH EMPTY KEY.

    CLASS-DATA gt_gs1_rule TYPE ty_t_gs1_rule.

    " a function module parameter as RFC_GET_FUNCTION_INTERFACE describes it
    TYPES:
      BEGIN OF ty_s_fm_param,
        " I importing, E exporting, C changing, T tables, X exception
        paramclass TYPE string,
        parameter  TYPE string,
        typename   TYPE string,
        optional   TYPE abap_bool,
      END OF ty_s_fm_param.
    TYPES ty_t_fm_param TYPE STANDARD TABLE OF ty_s_fm_param WITH EMPTY KEY.

    " raises "<method>: not available on ABAP Cloud" there
    CLASS-METHODS x_raise_on_cloud
      IMPORTING
        method TYPE clike.

    CLASS-METHODS sys_fm_get_interface
      IMPORTING
        function      TYPE clike
        destination   TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_t_fm_param.

    " a data object of the type of one function module parameter
    CLASS-METHODS sys_fm_create_param
      IMPORTING
        param         TYPE ty_s_fm_param
      RETURNING
        VALUE(result) TYPE REF TO data.

    " CALL FUNCTION with parameter and exception table, via RFC when
    " destination is given - returns sy-subrc
    CLASS-METHODS sys_fm_call
      IMPORTING
        function      TYPE clike
        destination   TYPE clike OPTIONAL
        t_param       TYPE abap_func_parmbind_tab
        t_exception   TYPE abap_func_excpbind_tab
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS sys_langu
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " the name of a method's returning parameter - it differs by release
    CLASS-METHODS sys_method_returning
      IMPORTING
        type_name     TYPE clike
        method        TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " a data object of the type of a method parameter, optionally filled
    CLASS-METHODS sys_method_param
      IMPORTING
        type_name     TYPE clike
        method        TYPE clike
        parameter     TYPE clike
        value         TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO data.

    " barcode_split_*: the scan as a string without the CR/LF a scanner in
    " keyboard mode ends it with
    CLASS-METHODS barcode_split_get_scan
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " barcode_split_*: raises for a value that is missing though the field is
    " not optional or that is longer than the field allows; adds the others
    CLASS-METHODS barcode_split_add_value
      IMPORTING
        field  TYPE ty_s_barcode_field
        value  TYPE string
        found  TYPE abap_bool
      CHANGING
        result TYPE ty_t_barcode_value.

    CLASS-METHODS barcode_gs1_get_rules
      RETURNING
        VALUE(result) TYPE ty_t_gs1_rule.

    " the rule of the AI val starts with, initial if there is none
    CLASS-METHODS barcode_gs1_find_rule
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_gs1_rule.

    CLASS-METHODS barcode_gs1_get_element
      IMPORTING
        rule          TYPE ty_s_gs1_rule
        ai            TYPE string
        value         TYPE string
        check_digit   TYPE abap_bool
        ref_date      TYPE d
      RETURNING
        VALUE(result) TYPE ty_s_gs1.

    " YYMMDD to a date: century by the GS1 sliding window, day 00 = last day
    CLASS-METHODS barcode_gs1_get_date
      IMPORTING
        val           TYPE string
        ref_date      TYPE d
      RETURNING
        VALUE(result) TYPE d.

    TYPES ty_p_seconds TYPE p LENGTH 16 DECIMALS 7.

    " hash_string: the dynamic call raised once in this roll area, so the
    " class is absent here - the text answers from then on without a
    " raise per call (two per roundtrip)
    CLASS-DATA gv_hash_absent TYPE abap_bool.

    " rtti_check_class_impl_intf, cached per class and interface the way
    " the existence check is: it is asked on every app start, and each
    " answer was TWO describes of the same class (the existence check and
    " its own) - one describe now, and none on the next start of the same
    " app in this roll area
    TYPES:
      BEGIN OF ty_s_class_impl_intf,
        name   TYPE string,
        intf   TYPE string,
        result TYPE abap_bool,
      END OF ty_s_class_impl_intf.
    CLASS-DATA gt_class_impl_intf TYPE HASHED TABLE OF ty_s_class_impl_intf WITH UNIQUE KEY name intf.

    " whether a type kind is a number: i, int1, int2, int8, p, f,
    " decfloat16, decfloat34 - the one list the number checks share. int8
    " by its value `8`, a constant lower releases do not have
    CLASS-METHODS rtti_check_typekind_number
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    " the json_kind of one elementary type (see ty_s_flat_comp)
    CLASS-METHODS rtti_get_json_kind
      IMPORTING
        val           TYPE REF TO cl_abap_datadescr
      RETURNING
        VALUE(result) TYPE ty_s_flat_comp-json_kind.

    " the type half of rtti_check_namespaced: a structure, the line of a
    " table, and what is below them - depth-bounded like expand_components
    CLASS-METHODS rtti_check_namespaced_type
      IMPORTING
        type          TYPE REF TO cl_abap_typedescr
        depth         TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE abap_bool.

    " the character set c_escape_xml guards against, concatenated once per
    " roll area instead of once per attribute value. Lazily filled on first
    " use - a public class_constructor is the alternative and a known trap
    CLASS-DATA gv_xml_escape_specials TYPE string.
    CLASS-DATA gv_xml_escape_controls TYPE string.

    " PCRE wherever CL_ABAP_REGEX=>CREATE_PCRE exists (7.55 and ABAP
    " Cloud), POSIX only below - one pattern means the same everywhere
    CLASS-METHODS regex_create_matcher
      IMPORTING
        val           TYPE clike
        regex         TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO object.

    TYPES:
      BEGIN OF ty_s_regex_match,
        off TYPE i,
        len TYPE i,
      END OF ty_s_regex_match.
    TYPES ty_t_regex_match TYPE STANDARD TABLE OF ty_s_regex_match WITH EMPTY KEY.

    " every non-empty match of regex in val, as offset and length - the
    " loop regex_find_all and regex_replace_all share
    CLASS-METHODS regex_get_matches
      IMPORTING
        val           TYPE string
        regex         TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_regex_match.

    CLASS-METHODS http_execute
      IMPORTING
        method        TYPE string
        url           TYPE string
        body          TYPE string
        content_type  TYPE string
        t_header      TYPE ty_t_name_value
      RETURNING
        VALUE(result) TYPE ty_s_http_response.

    CLASS-METHODS rtti_get_classes_intf_cloud
      IMPORTING
        val              TYPE clike
        read_description TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE ty_t_classes.

    CLASS-METHODS rtti_get_classes_intf_std
      IMPORTING
        val              TYPE clike
        read_description TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE ty_t_classes.

    CLASS-METHODS rtti_get_dtel_texts_by_ddic
      IMPORTING
        name        TYPE string
      EXPORTING
        texts       TYPE ty_s_data_element_text
        do_fallback TYPE abap_bool.

    CLASS-METHODS rtti_get_dtel_texts_by_xco
      IMPORTING
        name        TYPE string
      EXPORTING
        texts       TYPE ty_s_data_element_text
        do_fallback TYPE abap_bool.

    TYPES:
      BEGIN OF ty_s_bool_cache,
        typedescr TYPE REF TO cl_abap_typedescr,
        is_bool   TYPE abap_bool,
      END OF ty_s_bool_cache.

    CLASS-DATA mt_bool_cache TYPE HASHED TABLE OF ty_s_bool_cache WITH UNIQUE KEY typedescr.

    TYPES:
      BEGIN OF ty_s_attri_cache,
        absolute_name TYPE string,
        o_struct      TYPE REF TO cl_abap_structdescr,
        t_attri       TYPE cl_abap_structdescr=>component_table,
      END OF ty_s_attri_cache.

    CLASS-DATA mt_attri_cache TYPE HASHED TABLE OF ty_s_attri_cache WITH UNIQUE KEY absolute_name.

    " answers of rtti_check_class_exists, per roll area - the repository
    " lookup behind it is not free and the same names are asked again and
    " again by callers that probe for an optional class on every call.
    " A class created at runtime after a negative answer is the one caller
    " this cache can mislead, within one roll area only - same trade as
    " gv_check_cloud below
    TYPES:
      BEGIN OF ty_s_class_exists,
        name   TYPE string,
        exists TYPE abap_bool,
      END OF ty_s_class_exists.

    CLASS-DATA gt_class_exists TYPE HASHED TABLE OF ty_s_class_exists WITH UNIQUE KEY name.

    CLASS-METHODS filter_get_sql_cond_by_range
      IMPORTING
        fieldname     TYPE clike
        range         TYPE ty_s_range
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS filter_get_range_by_sql_cond
      IMPORTING
        val       TYPE clike
      EXPORTING
        fieldname TYPE string
        range     TYPE ty_s_range.

    CLASS-METHODS filter_sql_split_top_level
      IMPORTING
        val           TYPE clike
        sep           TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS filter_sql_strip_quotes
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-DATA gv_check_cloud TYPE abap_bool.
    CLASS-DATA gv_check_cloud_cached TYPE abap_bool.

    " Guards uuid_get_c32 against re-entering itself through the exception it
    " raises (zabaputil_cx_util_error reads a uuid for its full report).
    CLASS-DATA gv_uuid_failed TYPE abap_bool.

    CLASS-METHODS bal_cloud_add_items
      IMPORTING
        log   TYPE REF TO object
        t_log TYPE ty_t_msg.

    CLASS-METHODS bal_cloud_build_filter
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO object.

    CLASS-METHODS bal_std_msg_add
      IMPORTING
        handle TYPE any
        t_log  TYPE ty_t_msg.

    CLASS-METHODS bal_std_load_handles
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS bal_std_build_filter
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO data.

    " adds `I <option> value [high]` to the range component comp of a
    " BAL_S_LFIL filter - an initial value adds nothing
    CLASS-METHODS bal_std_filter_add
      IMPORTING
        comp   TYPE clike
        value  TYPE clike
        option TYPE clike DEFAULT `EQ`
        high   TYPE clike OPTIONAL
      CHANGING
        filter TYPE any.

    CLASS-METHODS bal_std_map_msg
      IMPORTING
        msg           TYPE any
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    " calls a lock module by name: result = it succeeded. subrc: 0, 1 for
    " FOREIGN_LOCK of an ENQUEUE_ module, 4 for any other exception it
    " raised, 99 when the call itself failed (no such module, a parameter
    " it does not have)
    CLASS-METHODS lock_call_function
      IMPORTING
        val           TYPE clike
        t_param       TYPE ty_t_lock_param OPTIONAL
      EXPORTING
        subrc         TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_msg_container
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    " a token template of filter_get_token_range_mapping with {LOW} and
    " {HIGH} filled in as written
    CLASS-METHODS filter_token_fill
      IMPORTING
        template      TYPE string
        low           TYPE string
        high          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    " hash_string with the digest class named by the caller, INITIAL when
    " that class cannot answer - the test class reaches that branch through
    " a class that does not exist
    CLASS-METHODS hash_string_by_class
      IMPORTING
        val           TYPE clike
        classname     TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! the public attributes of an object that carry a message part (TEXT,
    "! TYPE, MSGV1 ...), mapped over is_msg - see msg_map
    CLASS-METHODS msg_get_by_oref_attri
      IMPORTING
        io_obj        TYPE REF TO object
        is_msg        TYPE ty_s_msg
      RETURNING
        VALUE(result) TYPE ty_s_msg.

    "! the rows of a table, the RAP shape decided on the first structured row
    CLASS-METHODS msg_get_internal_tab
      IMPORTING
        it_tab        TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    " a timestamp as seconds since 1970-01-01 - see time_diff_milliseconds
    CLASS-METHODS time_get_seconds
      IMPORTING
        !time         TYPE timestampl
      RETURNING
        VALUE(result) TYPE ty_p_seconds.

    " The control characters below U+0020 that c_escape_json has no short
    " escape for (all but tab, line feed and carriage return), and every
    " character it escapes at all - built once, on first use: the control
    " half needs the codepage API, see gv_xml_escape_specials.
    CLASS-DATA gv_json_ctrl     TYPE string.
    CLASS-DATA gv_json_specials TYPE string.

    " Splits CSV text into rows of cells, RFC 4180: a cell may be wrapped in
    " double quotes and then carry the separator, a line break and a doubled
    " quote. Rows end with CRLF, LF or CR. Empty rows are left out.
    TYPES ty_t_csv_rows TYPE STANDARD TABLE OF string_table WITH EMPTY KEY.

    CLASS-METHODS csv_split_rows
      IMPORTING
        val           TYPE string
        separator     TYPE string DEFAULT `;`
      RETURNING
        VALUE(result) TYPE ty_t_csv_rows.

    " Reads a quoted cell: pos stands on the opening quote and is moved
    " behind the closing one; a doubled quote inside is one quote.
    CLASS-METHODS csv_read_quoted
      IMPORTING
        val  TYPE string
      CHANGING
        pos  TYPE i
        cell TYPE string.

    " The text without the byte order mark Excel's "CSV UTF-8" starts a
    " file with.
    CLASS-METHODS csv_strip_bom
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    " A header cell as a component name: letters, digits and underscores,
    " not empty, not starting with a digit, at most 30 characters.
    CLASS-METHODS csv_get_column_name
      IMPORTING
        val           TYPE string
        index         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    " One CSV cell: wrapped in double quotes when it carries the separator,
    " a quote or a line break, quotes doubled.
    CLASS-METHODS csv_escape_cell
      IMPORTING
        val           TYPE string
        separator     TYPE string DEFAULT `;`
      RETURNING
        VALUE(result) TYPE string.

    " Offset of a keyword in a condition, outside of quoted literals and
    " whatever its case; -1 when it is not there.
    CLASS-METHODS filter_sql_find
      IMPORTING
        val           TYPE string
        keyword       TYPE string
      RETURNING
        VALUE(result) TYPE i.

    " Removes the parentheses around a condition, as long as the first one
    " closes at the very end: `( A ) OR ( B )` keeps its own.
    CLASS-METHODS filter_sql_strip_parens
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    " The ranges of conditions joined with OR; the name is the field of the
    " first one.
    CLASS-METHODS filter_sql_get_or_group
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_filter_multi.

    " A LIKE pattern as a CP pattern: % and _ become * and +, a plain *, +
    " or # gets the CP escape # in front.
    CLASS-METHODS filter_sql_like_to_pattern
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    " The option that selects the complement: EQ <-> NE, LT <-> GE, LE <-> GT,
    " CP <-> NP, BT <-> NB. Any other option comes back as it is.
    CLASS-METHODS filter_option_negate
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " A value as an Open SQL character literal for a dynamic WHERE: in
    " single quotes, a quote inside doubled.
    CLASS-METHODS filter_sql_quote
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " codepage: the IANA name CL_ABAP_CONV_CODEPAGE takes; encoding: the
    " name or SAP code page number CL_ABAP_CONV_IN_CE takes. invalid is set
    " when a converter ran and rejected the bytes; no converter at all
    " raises UNSUPPORTED_CODEPAGE_API.
    CLASS-METHODS conv_get_string_by_xstring_cp
      IMPORTING
        val      TYPE xstring
        codepage TYPE string
        encoding TYPE string
      EXPORTING
        result   TYPE string
        invalid  TYPE abap_bool.

    " the text half of itab_filter_check_field: abap_true when text holds
    " search - upper case already when ignore_case is set
    CLASS-METHODS itab_filter_check_text
      IMPORTING
        !text         TYPE string
        search        TYPE string
        ignore_case   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE abap_bool.

    " a run of percent escapes of url_decode as text: the bytes as UTF-8,
    " or the escapes as they were written (raw) when they are no UTF-8
    CLASS-METHODS url_decode_bytes
      IMPORTING
        hex           TYPE string
        raw           TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS zabaputil_cl_util_context IMPLEMENTATION.

  METHOD class_constructor.

    cv_char_util_newline        = cl_abap_char_utilities=>newline.
    cv_char_util_cr_lf          = cl_abap_char_utilities=>cr_lf.
    cv_char_util_horizontal_tab = cl_abap_char_utilities=>horizontal_tab.
    cv_char_util_charsize       = cl_abap_char_utilities=>charsize.
    cv_format_e_xml_attr        = cl_abap_format=>e_xml_attr.

    cv_typedescr_typekind_table      = cl_abap_typedescr=>typekind_table.
    cv_typedescr_typekind_dref       = cl_abap_typedescr=>typekind_dref.
    cv_typedescr_typekind_oref       = cl_abap_typedescr=>typekind_oref.
    cv_typedescr_typekind_struct1    = cl_abap_typedescr=>typekind_struct1.
    cv_typedescr_typekind_struct2    = cl_abap_typedescr=>typekind_struct2.
    cv_typedescr_typekind_date       = cl_abap_typedescr=>typekind_date.
    cv_typedescr_typekind_time       = cl_abap_typedescr=>typekind_time.
    cv_typedescr_typekind_packed     = cl_abap_typedescr=>typekind_packed.
    cv_typedescr_kind_struct         = cl_abap_typedescr=>kind_struct.
    cv_typedescr_kind_elem           = cl_abap_typedescr=>kind_elem.
    cv_typedescr_kind_ref            = cl_abap_typedescr=>kind_ref.
    cv_objectdescr_public            = cl_abap_objectdescr=>public.

  ENDMETHOD.

  METHOD db_rollback.

    ROLLBACK WORK.

  ENDMETHOD.

  METHOD boolean_abap_2_json.

    IF boolean_check_by_data( val ).
      result = COND #( WHEN val = abap_true THEN `true` ELSE `false` ).
    ELSE.
      result = val.
    ENDIF.

  ENDMETHOD.

  METHOD boolean_check_by_data.

    TRY.
        DATA(lo_descr) = cl_abap_elemdescr=>describe_by_data( val ).

        " all supported boolean types are character-like flags, this check
        " filters out every other type before the cache lookup
        IF lo_descr->type_kind <> cl_abap_typedescr=>typekind_char.
          RETURN.
        ENDIF.

        " type descriptors are singletons, so the reference identifies the
        " type without converting/hashing the absolute name on every call
        ASSIGN mt_bool_cache[ typedescr = lo_descr ] TO FIELD-SYMBOL(<ls_bool_cache>).
        IF sy-subrc = 0.
          result = <ls_bool_cache>-is_bool.
          RETURN.
        ENDIF.

        result = boolean_check_by_name( lo_descr->get_relative_name( ) ).

        INSERT VALUE #( typedescr = lo_descr is_bool = result ) INTO TABLE mt_bool_cache.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD boolean_check_by_name.

    CASE val.
      WHEN `ABAP_BOOL`
          OR `XSDBOOLEAN`
          OR `FLAG`
          OR `XFLAG`
          OR `XFELD`
          OR `ABAP_BOOLEAN`
          OR `WDY_BOOLEAN`
          OR `BOOLE_D`
          OR `OS_BOOLEAN`.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.

  METHOD check_bound_a_not_initial.

    result = xsdbool( check_unassign_initial( val ) = abap_false ).

  ENDMETHOD.

  METHOD check_unassign_initial.

    IF val IS INITIAL.
      result = abap_true.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <any> TYPE data.
    ASSIGN val->* TO <any>.

    result = xsdbool( <any> IS INITIAL ).

  ENDMETHOD.

  METHOD conv_copy_ref_data.

    FIELD-SYMBOLS <from>   TYPE data.
    FIELD-SYMBOLS <result> TYPE data.

    IF rtti_check_ref_data( from ).
      ASSIGN from->* TO <from>.
      IF <from> IS NOT ASSIGNED.
        " unbound data reference - nothing to copy, return an initial reference
        RETURN.
      ENDIF.
    ELSE.
      ASSIGN from TO <from>.
    ENDIF.
    CREATE DATA result LIKE <from>.
    ASSIGN result->* TO <result>.

    <result> = <from>.

  ENDMETHOD.

  METHOD conv_get_as_data_ref.

    " REF #( ), not GET REFERENCE OF: the statement form is not released for
    " ABAP Cloud, and neither check:cloud nor the transpiled unit run sees
    " it (abap-check skill, section 3). The target is a typed REF TO data,
    " so `#` infers - the "Unexpected operator REF" trap of that section
    " only bites a TYPE any field symbol target
    result = REF #( val ).

  ENDMETHOD.

  METHOD conv_get_xstring_by_data_uri.

    DATA lv_metadata TYPE string ##NEEDED.
    DATA lv_base64   TYPE string.

    IF val CS `,`.
      SPLIT val AT `,` INTO lv_metadata lv_base64.
    ELSE.
      " a bare base64 payload, without the `data:...;base64,` prefix - the
      " SPLIT put the whole value into the metadata half and the payload
      " decoded to an empty xstring without a word
      lv_base64 = val.
    ENDIF.
    result = conv_decode_x_base64( lv_base64 ).

  ENDMETHOD.

  METHOD c_trim.

    result = val.
    " spaces and tabs alternate at either end (`\t \tx`) - one pass of each
    " leaves the inner layer standing, so strip until nothing changes. No
    " fixed number of passes: ten left the eleventh layer of a padded value
    " standing. Every pass that changes something shortens the string, so
    " the loop ends
    DO.
      DATA(lv_before) = result.
      result = shift_left( shift_right( result ) ).
      result = shift_right( val = result
                            sub = cv_char_util_horizontal_tab ).
      result = shift_left( val = result
                           sub = cv_char_util_horizontal_tab ).
      IF result = lv_before.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD c_trim_lower.

    result = to_lower( c_trim( val ) ).

  ENDMETHOD.

  METHOD c_trim_upper.

    result = to_upper( c_trim( val ) ).

  ENDMETHOD.

  METHOD filter_itab.

    " the same filter as itab_filter_by_t_range - a field whose range is
    " empty keeps every row there too
    itab_filter_by_t_range( EXPORTING val = filter
                            CHANGING  tab = val ).

  ENDMETHOD.

  METHOD filter_get_multi_by_data.

    LOOP AT rtti_get_t_attri_by_any( val ) REFERENCE INTO DATA(lr_comp).
      INSERT VALUE #( name = lr_comp->name ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_range_by_value.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    " one value is a list of one - CP for a pattern, EQ otherwise
    DATA lv_val TYPE string.
    lv_val = val.
    result = filter_get_range_t_by_value_t( VALUE #( ( lv_val ) ) ).

  ENDMETHOD.

  METHOD filter_get_range_t_by_value_t.

    DATA lt_val   TYPE string_table.
    DATA lv_val   TYPE string.
    DATA ls_range TYPE ty_s_range.

    lt_val = val.
    SORT lt_val.
    DELETE ADJACENT DUPLICATES FROM lt_val.

    LOOP AT lt_val INTO lv_val.
      CLEAR ls_range.
      ls_range-sign = `I`.
      ls_range-low  = lv_val.
      IF lv_val CA `*`.
        ls_range-option = `CP`.
      ELSE.
        ls_range-option = `EQ`.
      ENDIF.
      INSERT ls_range INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_range_by_token.
    DATA(lv_value) = val.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_length) = strlen( lv_value ).

    " `!(...)`: the negated token filter_get_token_t_by_range_t renders for
    " an excluding row and for NE / NB / NP - the inner token with the sign
    " flipped (I NE x comes back as the equivalent E EQ x, E NE x as I EQ x)
    IF lv_length > 3 AND lv_value(2) = `!(` AND substring( val = lv_value
                                                           off = lv_length - 1 ) = `)`.
      result = filter_get_range_by_token( substring( val = lv_value
                                                     off = 2
                                                     len = lv_length - 3 ) ).
      result-sign = COND #( WHEN result-sign = `E` THEN `I` ELSE `E` ).
      RETURN.
    ENDIF.

    result-sign = `I`.

    IF lv_length >= 2 AND ( lv_value(2) = `<=` OR lv_value(2) = `>=` ).
      result-option = COND #( WHEN lv_value(1) = `<` THEN `LE` ELSE `GE` ).
      result-low    = substring( val = lv_value
                                 off = 2 ).

    ELSEIF lv_value(1) = `=`.
      result-option = `EQ`.
      result-low    = substring( val = lv_value
                                 off = 1 ).

    ELSEIF lv_value(1) = `<`.
      result-option = `LT`.
      result-low    = substring( val = lv_value
                                 off = 1 ).

    ELSEIF lv_value(1) = `>`.
      result-option = `GT`.
      result-low    = substring( val = lv_value
                                 off = 1 ).

    ELSEIF lv_value CA `*`.
      " the pattern as it was typed: `*abc*` contains, `abc*` starts with,
      " `*abc` ends with. LOW keeps the asterisks - they are what makes a CP
      " row a pattern, for IN as well as for the LIKE built from it
      result-option = `CP`.
      result-low    = lv_value.

    ELSEIF lv_value CS `...`.
      SPLIT lv_value AT `...` INTO result-low result-high.
      result-option = `BT`.

    ELSE.
      result-option = `EQ`.
      result-low    = lv_value.
    ENDIF.
  ENDMETHOD.

  METHOD filter_update_tokens.

    result = val.
    DATA(lr_filter) = REF #( result[ name = name ] ).
    LOOP AT lr_filter->t_token_removed INTO DATA(ls_token).
      DELETE lr_filter->t_token WHERE key = ls_token-key.
    ENDLOOP.

    LOOP AT lr_filter->t_token_added INTO ls_token.
      INSERT VALUE #( key      = ls_token-key
                      text     = ls_token-text
                      visible  = abap_true
                      editable = abap_true ) INTO TABLE lr_filter->t_token.
    ENDLOOP.

    CLEAR lr_filter->t_token_removed.
    CLEAR lr_filter->t_token_added.

    DATA(lt_range) = filter_get_range_t_by_token_t( result[ name = name ]-t_token ).
    lr_filter->t_range = lt_range.

  ENDMETHOD.

  METHOD filter_get_range_t_by_token_t.

    LOOP AT val INTO DATA(ls_token).
      INSERT filter_get_range_by_token( ls_token-text ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_token_range_mapping.

    result = VALUE #( (   n = `EQ`      v = `={LOW}` )
                      (   n = `LT`      v = `<{LOW}` )
                      (   n = `LE`      v = `<={LOW}` )
                      (   n = `GT`      v = `>{LOW}` )
                      (   n = `GE`      v = `>={LOW}` )
                      (   n = `CP`      v = `*{LOW}*` )
                      (   n = `BT`      v = `{LOW}...{HIGH}` )
                      (   n = `NB`      v = `!({LOW}...{HIGH})` )
                      (   n = `NE`      v = `!(={LOW})` )
                      (   n = `NP`      v = `!(*{LOW}*)` )
                      (   n = `!<leer>` v = `!(<leer>)` )
                      (   n = `<leer>`  v = `<leer>` ) ).

  ENDMETHOD.

  METHOD filter_get_token_t_by_range_t.

    DATA(lt_mapping) = filter_get_token_range_mapping( ).

    DATA lt_tab TYPE ty_t_range.

    itab_corresponding( EXPORTING val = val
                        CHANGING  tab = lt_tab ).

    " LOW / HIGH again from the source row, through data_get_string: the
    " move into the string fields above wrote a number the way a MOVE does
    " - sign behind it and padded (`5- `), so a select-option on an integer
    " field showed `=42 ` and `5-...10 `
    " UNASSIGN + IS ASSIGNED, not sy-subrc, in the loop (#1937)
    FIELD-SYMBOLS <lt_source> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_range>  TYPE ty_s_range.
    FIELD-SYMBOLS <lv_part>   TYPE any.
    ASSIGN val TO <lt_source>.
    IF <lt_source> IS ASSIGNED.
      DATA(lv_index) = 0.
      LOOP AT <lt_source> ASSIGNING FIELD-SYMBOL(<ls_source>).
        lv_index = lv_index + 1.
        UNASSIGN <ls_range>.
        ASSIGN lt_tab[ lv_index ] TO <ls_range>.
        IF <ls_range> IS NOT ASSIGNED.
          EXIT.
        ENDIF.
        UNASSIGN <lv_part>.
        ASSIGN COMPONENT `LOW` OF STRUCTURE <ls_source> TO <lv_part>.
        IF <lv_part> IS ASSIGNED.
          <ls_range>-low = data_get_string( <lv_part> ).
        ENDIF.
        UNASSIGN <lv_part>.
        ASSIGN COMPONENT `HIGH` OF STRUCTURE <ls_source> TO <lv_part>.
        IF <lv_part> IS ASSIGNED.
          <ls_range>-high = data_get_string( <lv_part> ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_tab REFERENCE INTO DATA(lr_row).

      " an option the mapping does not know - initial (a row appended with
      " sign and low only) or lower-case - used to raise a raw
      " CX_SY_ITAB_LINE_NOT_FOUND out of the whole call; it renders as the
      " equality token, which is what a range row without an option means
      DATA lv_option TYPE string.
      DATA lv_value  TYPE string.
      lv_option = to_upper( lr_row->option ).
      lv_value = VALUE #( lt_mapping[ n = lv_option ]-v OPTIONAL ). "#EC CI_SORTSEQ
      IF lv_value IS INITIAL.
        lv_value = lt_mapping[ n = `EQ` ]-v. "#EC CI_SORTSEQ
      ENDIF.
      IF ( lv_option = `CP` OR lv_option = `NP` ) AND lr_row->low CA `*`.
        " LOW is a pattern already - wrapping it in the template's asterisks
        " would turn `abc*` into `*abc**`
        lv_value = COND #( WHEN lv_option = `CP` THEN lr_row->low
                           ELSE |!({ lr_row->low })| ).
      ELSE.
        lv_value = filter_token_fill( template = lv_value
                                      low      = lr_row->low
                                      high     = lr_row->high ).
      ENDIF.

      " an excluding row must not render like its including twin - negate the
      " token so the MultiInput shows the filter's real meaning
      IF lr_row->sign = `E`.
        lv_value = |!({ lv_value })|.
      ENDIF.

      INSERT VALUE #( key      = lv_value
                      text     = lv_value
                      visible  = abap_true
                      editable = abap_true ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_filter_by_val.
    " TRANSPILER NOTE: ABAP CS is always case-insensitive, so the two match
    " branches below differ deliberately: ignore_case = abap_true uses
    " to_upper + CS (JS: toLowerCase().includes(toLowerCase())), the default
    " uses find( ) for a genuinely case-sensitive match (JS: plain includes()).
    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <kept>  TYPE STANDARD TABLE.
    " the components that hold text: by position without a field list, by
    " name with one
    DATA lt_index TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lt_name  TYPE string_table.
    DATA lv_whole_row TYPE abap_bool.
    DATA lr_kept TYPE REF TO data.

    DATA(lv_search) = COND string( WHEN ignore_case = abap_true
                                   THEN to_upper( val )
                                   ELSE val ).
    " an empty search is no filter: every row stays. Said here rather than
    " left to the match below - whether find( ) or CS reports a hit for an
    " empty substring is not something to build on, and a row with nothing
    " printable in it (only a table of children, only a reference) matched
    " nothing and was deleted
    IF lv_search IS INITIAL.
      RETURN.
    ENDIF.

    " Which component holds text is a property of the line type: decided
    " once, on the first row, instead of describing every cell of every
    " row - this runs on every search keystroke over the whole table. A
    " table, a deep structure or a reference cannot go into a string
    " template - such a component holds no text to match
    ASSIGN tab[ 1 ] TO <row>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF fields IS INITIAL.
      DATA(lv_index) = 1.
      DO.
        ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO <field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF rtti_check_printable( <field> ) = abap_true.
          INSERT lv_index INTO TABLE lt_index.
        ENDIF.
        lv_index = lv_index + 1.
      ENDDO.
      " elementary line type (e.g. string_table) has no components -
      " match against the whole line instead of deleting every row
      IF lv_index = 1.
        lv_whole_row = rtti_check_printable( <row> ).
      ENDIF.
    ELSE.
      LOOP AT fields REFERENCE INTO DATA(lr_field).
        ASSIGN COMPONENT lr_field->* OF STRUCTURE <row> TO <field>.
        IF sy-subrc = 0 AND rtti_check_printable( <field> ) = abap_true.
          INSERT lr_field->* INTO TABLE lt_name.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " the matching rows go into a copy - a DELETE ... INDEX per dropped row
    " shifted the rest of the table every time
    CREATE DATA lr_kept LIKE tab.
    ASSIGN lr_kept->* TO <kept>.

    LOOP AT tab ASSIGNING <row>.

      DATA(lv_check_found) = abap_false.
      IF lv_whole_row = abap_true.
        lv_check_found = itab_filter_check_text( text        = |{ <row> }|
                                                 search      = lv_search
                                                 ignore_case = ignore_case ).
      ENDIF.
      LOOP AT lt_index REFERENCE INTO DATA(lr_index).
        ASSIGN COMPONENT lr_index->* OF STRUCTURE <row> TO <field>.
        IF itab_filter_check_text( text        = |{ <field> }|
                                   search      = lv_search
                                   ignore_case = ignore_case ) = abap_true.
          lv_check_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      LOOP AT lt_name REFERENCE INTO DATA(lr_name).
        ASSIGN COMPONENT lr_name->* OF STRUCTURE <row> TO <field>.
        IF itab_filter_check_text( text        = |{ <field> }|
                                   search      = lv_search
                                   ignore_case = ignore_case ) = abap_true.
          lv_check_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_check_found = abap_true.
        INSERT <row> INTO TABLE <kept>.
      ENDIF.

    ENDLOOP.

    IF lines( <kept> ) <> lines( tab ).
      tab = <kept>.
    ENDIF.

  ENDMETHOD.

  METHOD itab_get_csv_by_itab.
    FIELD-SYMBOLS <tab>   TYPE table.
    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    DATA lt_lines TYPE string_table.
    DATA lt_cells TYPE string_table.
    " the component per column, empty for a column without text
    DATA lt_names TYPE string_table.

    ASSIGN val TO <tab>.
    DATA(tab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <tab> ) ).

    DATA(struc) = CAST cl_abap_structdescr( tab->get_table_line_type( ) ).

    " the included view, not get_components( ): an include (a DDIC
    " table's .INCLUDE, an append, INCLUDE TYPE) is ONE nameless component
    " there, so the header had an empty cell where the rows had one cell
    " per field of the include, and every column behind it shifted. The
    " kind is a property of the line type - decided here once per column,
    " not described again for every cell. A table, a structure or a
    " reference has no text - in a string template it is a runtime error,
    " so its cell stays empty
    LOOP AT struc->get_included_view( ) REFERENCE INTO DATA(lr_comp).
      INSERT csv_escape_cell( lr_comp->name ) INTO TABLE lt_cells.
      INSERT COND string( WHEN lr_comp->type->kind = cl_abap_typedescr=>kind_elem
                          THEN lr_comp->name ) INTO TABLE lt_names.
    ENDLOOP.
    INSERT concat_lines_of( table = lt_cells
                            sep   = `;` ) INTO TABLE lt_lines.

    LOOP AT <tab> ASSIGNING <row>.

      CLEAR lt_cells.
      LOOP AT lt_names REFERENCE INTO DATA(lr_name).
        IF lr_name->* IS INITIAL.
          INSERT `` INTO TABLE lt_cells.
          CONTINUE.
        ENDIF.
        ASSIGN COMPONENT lr_name->* OF STRUCTURE <row> TO <field>.
        " the value as it is - a separator, a quote or a line break inside
        " it is quoted, not replaced
        INSERT csv_escape_cell( |{ <field> }| ) INTO TABLE lt_cells.
      ENDLOOP.
      INSERT concat_lines_of( table = lt_cells
                              sep   = `;` ) INTO TABLE lt_lines.
    ENDLOOP.

    result = concat_lines_of( table = lt_lines
                              sep   = cv_char_util_cr_lf ).
  ENDMETHOD.

  METHOD itab_get_itab_by_csv.
    DATA lt_comp TYPE cl_abap_structdescr=>component_table.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    DATA lr_row TYPE REF TO data.
    DATA lt_names TYPE string_table.
    FIELD-SYMBOLS <field> TYPE any.

    DATA(lt_rows) = csv_split_rows( csv_strip_bom( val ) ).
    " no row, no header to build a table from
    IF lt_rows IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_cols) = lt_rows[ 1 ].
    " a header written with a separator behind the last column
    DATA(lv_last) = lines( lt_cols ).
    IF lv_last > 1 AND lt_cols[ lv_last ] IS INITIAL.
      DELETE lt_cols INDEX lv_last.
    ENDIF.

    LOOP AT lt_cols REFERENCE INTO DATA(lr_col).
      DATA(lv_tabix) = sy-tabix.

      DATA(lv_name) = csv_get_column_name( val   = lr_col->*
                                           index = lv_tabix ).
      " a name the header carries twice - and the fallback name may be
      " taken too (`A;COL3;A`), which made the structure creation below
      " raise CX_SY_STRUCT_COMP_NAME out of the method
      IF line_exists( lt_names[ table_line = lv_name ] ).
        lv_name = |COL{ lv_tabix }|.
        " DO with an EXIT, not WHILE line_exists( ): the 7.02 downport
        " evaluates a WHILE condition with a table expression once, in
        " front of the loop, and the loop never ended
        DATA(lv_suffix) = 1.
        DO.
          IF NOT line_exists( lt_names[ table_line = lv_name ] ).
            EXIT.
          ENDIF.
          lv_suffix = lv_suffix + 1.
          lv_name = |COL{ lv_tabix }_{ lv_suffix }|.
        ENDDO.
      ENDIF.
      INSERT lv_name INTO TABLE lt_names.

      " a string per column - a fixed length cut every longer value
      INSERT VALUE #( name = lv_name
                      type = cl_abap_elemdescr=>get_string( ) ) INTO TABLE lt_comp.
    ENDLOOP.

    DATA(struc) = cl_abap_structdescr=>create( lt_comp ).
    DATA(data) = CAST cl_abap_datadescr( struc ).
    DATA(o_table_desc) = cl_abap_tabledescr=>create( p_line_type  = data
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false ).

    CREATE DATA result TYPE HANDLE o_table_desc.
    ASSIGN result->* TO <tab>.

    LOOP AT lt_rows REFERENCE INTO DATA(lr_rows) FROM 2.

      CREATE DATA lr_row TYPE HANDLE struc.
      ASSIGN lr_row->* TO FIELD-SYMBOL(<row>).

      DATA(lv_index) = 0.
      LOOP AT lr_rows->* REFERENCE INTO lr_col.
        lv_index = lv_index + 1.
        UNASSIGN <field>.
        ASSIGN COMPONENT lv_index OF STRUCTURE <row> TO <field>.
        IF <field> IS NOT ASSIGNED.
          EXIT.
        ENDIF.
        <field> = lr_col->*.
      ENDLOOP.

      INSERT <row> INTO TABLE <tab>.
    ENDLOOP.
  ENDMETHOD.

  METHOD json_parse.
    TRY.

        zabaputil_cl_ajson=>parse( val )->to_abap( EXPORTING iv_corresponding = abap_true
                                               IMPORTING ev_container     = data ).

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.
  ENDMETHOD.

  METHOD json_stringify.
    TRY.

        DATA(li_ajson) = CAST zabaputil_if_ajson( zabaputil_cl_ajson=>create_empty( ) ).
        result = li_ajson->set( iv_path = `/`
                                iv_val  = any )->stringify( ).

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.
  ENDMETHOD.

  METHOD rtti_check_class_exists.

    " cached per name - see gt_class_exists at the declaration
    DATA lv_name TYPE string.
    lv_name = to_upper( val ).

    ASSIGN gt_class_exists[ name = lv_name ] TO FIELD-SYMBOL(<ls_hit>).
    IF sy-subrc = 0.
      result = <ls_hit>-exists.
      RETURN.
    ENDIF.

    TRY.
        " the cached spelling, not the caller's: the answer is stored under
        " the upper-case name, so the question has to be asked in it too -
        " otherwise the first caller's spelling decides every later answer
        cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = lv_name
                                              EXCEPTIONS type_not_found = 1 ).
        IF sy-subrc = 0.
          result = abap_true.
        ENDIF.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    INSERT VALUE #( name   = lv_name
                    exists = result ) INTO TABLE gt_class_exists.

  ENDMETHOD.

  METHOD rtti_check_ref_data.

    TRY.
        " a kind comparison, not a CAST probe: the cast raised
        " CX_SY_MOVE_CAST_ERROR for every NON-reference value, and the
        " non-reference is the COMMON case on this path (every value routed
        " through conv_copy_ref_data asks) - deciding a type question by
        " exception is orders of magnitude more expensive than comparing.
        " cl_abap_refdescr covers data and object references alike, and so
        " does kind_ref
        DATA(lo_typdescr) = cl_abap_typedescr=>describe_by_data( val ).
        IF lo_typdescr->kind <> cl_abap_typedescr=>kind_ref.
          RETURN.
        ENDIF.
        " kind_ref covers object references too, and the one caller that
        " dereferences on a true answer (conv_copy_ref_data: `from->*`)
        " cannot do that to an object reference. Only a reference to DATA
        " answers true
        DATA(lo_referenced) = CAST cl_abap_refdescr( lo_typdescr )->get_referenced_type( ).
        result = xsdbool( lo_referenced->kind <> cl_abap_typedescr=>kind_class
                      AND lo_referenced->kind <> cl_abap_typedescr=>kind_intf ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_check_type_kind_dref.

    DATA(lv_type_kind) = cl_abap_datadescr=>get_data_type_kind( val ).
    result = xsdbool( lv_type_kind = cl_abap_typedescr=>typekind_dref ).

  ENDMETHOD.

  METHOD rtti_get_classname_by_ref.

    " an unbound reference has no class - answer with an empty name instead
    " of letting the RTTI call fail. Callers ask this while rendering (error
    " context, response header), where a half-built object graph is normal
    IF val IS NOT BOUND.
      RETURN.
    ENDIF.

    result = substring_after( val = cl_abap_classdescr=>get_class_name( val )
                              sub = `\CLASS=` ).

  ENDMETHOD.

  METHOD rtti_get_intfname_by_ref.

    DATA(rtti) = cl_abap_typedescr=>describe_by_data( in  ).
    DATA(ref) = CAST cl_abap_refdescr( rtti ).
    DATA(name) = ref->get_referenced_type( )->absolute_name.
    result = substring_after( val = name
                              sub = `\INTERFACE=` ).

  ENDMETHOD.

  METHOD rtti_get_type_kind.

    result = cl_abap_datadescr=>get_data_type_kind( val ).

  ENDMETHOD.

  METHOD rtti_get_type_name.
    TRY.

        DATA(lo_descr) = cl_abap_elemdescr=>describe_by_data( val ).
        DATA(lo_ele) = CAST cl_abap_elemdescr( lo_descr ).
        result = lo_ele->get_relative_name( ).

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD rtti_get_t_attri_by_include.

    " `type` IS the include's descriptor - the component the caller took it
    " from (as_include = abap_true) carries the structure descriptor, and
    " RTTI descriptors are singletons. It used to be re-described by name,
    " an RTTI lookup per include on the binding path that could only hand
    " back the object already in hand - or fail for a local include type
    " whose absolute name does not resolve, where the direct cast cannot
    DATA sdescr TYPE REF TO cl_abap_structdescr.
    TRY.
        sdescr ?= type.
      CATCH cx_sy_move_cast_error INTO DATA(lx_cast).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |Include type '{ type->absolute_name }' is not a structure|
            previous = lx_cast.
    ENDTRY.
    result = expand_components( val   = sdescr->get_components( )
                                depth = depth ).

  ENDMETHOD.

  METHOD expand_components.

    " see the declaration: bounded so a cyclic include chain surfaces as a
    " readable error instead of a stack-overflow dump. 16 nested include
    " levels is far beyond any real DDIC structure
    IF depth > 16.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `RTTI_INCLUDE_RECURSION - include expansion exceeded 16 levels (cyclic include?)`.
    ENDIF.

    LOOP AT val REFERENCE INTO DATA(lr_comp).
      IF lr_comp->as_include = abap_true.
        " INCLUDE ... RENAMING WITH SUFFIX: the structure's components are
        " named <component><suffix>, and every nested level adds its own -
        " the bare include names do not exist on the structure, so a
        " binding path or ASSIGN COMPONENT built from them fails
        DATA(lt_include) = rtti_get_t_attri_by_include( type  = lr_comp->type
                                                        depth = depth + 1 ).
        IF lr_comp->suffix IS NOT INITIAL.
          LOOP AT lt_include REFERENCE INTO DATA(lr_include).
            lr_include->name = lr_include->name && lr_comp->suffix.
          ENDLOOP.
        ENDIF.
        INSERT LINES OF lt_include INTO TABLE result.
      ELSE.
        INSERT lr_comp->* INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_oref.

    DATA lo_descr TYPE REF TO cl_abap_typedescr.

    " describe_by_object_ref raises the classic exception REFERENCE_IS_INITIAL
    " for an initial reference - called functionally that is a runtime error no
    " CATCH reaches
    cl_abap_objectdescr=>describe_by_object_ref( EXPORTING  p_object_ref         = val
                                                 RECEIVING p_descr_ref           = lo_descr
                                                 EXCEPTIONS reference_is_initial = 1
                                                            OTHERS               = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `RTTI_REFERENCE_IS_INITIAL`.
    ENDIF.
    result = CAST cl_abap_classdescr( lo_descr )->attributes.

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_any.

    DATA lo_struct        TYPE REF TO cl_abap_structdescr.
    DATA lo_type          TYPE REF TO cl_abap_typedescr.
    " declared, not CONV string( ) - see error_get_attributes
    DATA lv_absolute_name TYPE string.

    " describe_by_data never fails for a data object, and the reference case
    " is the one branch. The fallback chain that used to sit here - a second
    " describe_by_data_ref, then describe_by_name( val ) - could rescue no
    " input: the first only re-raised what the branch above had raised, and
    " a data object is no type name
    lo_type = cl_abap_typedescr=>describe_by_data( val ).
    IF lo_type->kind = cl_abap_typedescr=>kind_ref.
      " REFERENCE_IS_INITIAL - see rtti_get_t_attri_by_oref
      cl_abap_typedescr=>describe_by_data_ref( EXPORTING  p_data_ref           = val
                                               RECEIVING p_descr_ref           = lo_type
                                               EXCEPTIONS reference_is_initial = 1
                                                          OTHERS               = 2 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `RTTI_REFERENCE_IS_INITIAL`.
      ENDIF.
    ENDIF.

    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        lo_struct = CAST #( lo_type ).
      WHEN cl_abap_typedescr=>kind_table.
        lo_struct = CAST #( CAST cl_abap_tabledescr( lo_type )->get_table_line_type( ) ).
      WHEN OTHERS.
        lo_struct ?= lo_type.
    ENDCASE.

    " descriptor instances are singletons per type, so the identity check
    " guards against absolute names reused by other (local/anonymous) types
    lv_absolute_name = lo_struct->absolute_name.
    ASSIGN mt_attri_cache[ absolute_name = lv_absolute_name ] TO FIELD-SYMBOL(<ls_attri_cache>).
    IF <ls_attri_cache> IS ASSIGNED AND <ls_attri_cache>-o_struct = lo_struct.
      result = <ls_attri_cache>-t_attri.
      RETURN.
    ENDIF.

    result = expand_components( lo_struct->get_components( ) ).

    IF <ls_attri_cache> IS ASSIGNED.
      <ls_attri_cache>-o_struct = lo_struct.
      <ls_attri_cache>-t_attri  = result.
    ELSE.
      INSERT VALUE #( absolute_name = lv_absolute_name
                      o_struct      = lo_struct
                      t_attri       = result ) INTO TABLE mt_attri_cache.
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_t_ddic_fixed_values.

    IF rollname IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = CONV string( rollname )
                                             RECEIVING  p_descr_ref    = DATA(typedescr)
                                             EXCEPTIONS type_not_found = 1
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DATA(elemdescr) = CAST cl_abap_elemdescr( typedescr ).

        result = rtti_get_t_fixvalues( elemdescr = elemdescr
                                       langu     = langu ).

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_tab_get_relative_name.

    FIELD-SYMBOLS <table> TYPE any.

    TRY.
        DATA(typedesc) = cl_abap_typedescr=>describe_by_data( table ).

        CASE typedesc->kind.

          WHEN cl_abap_typedescr=>kind_table.
            DATA(tabledesc) = CAST cl_abap_tabledescr( typedesc ).
            DATA(structdesc) = CAST cl_abap_structdescr( tabledesc->get_table_line_type( ) ).
            result = structdesc->get_relative_name( ).
            RETURN.

          WHEN typedesc->kind_ref.

            ASSIGN table->* TO <table>.
            result = rtti_tab_get_relative_name( <table> ).

        ENDCASE.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_exit.

    DATA lv_message TYPE string.
    DATA lx_error   TYPE REF TO cx_root.

    DATA(conex) = COND string( WHEN output = abap_true
                               THEN |CONVERSION_EXIT_{ convexit }_OUTPUT|
                               ELSE |CONVERSION_EXIT_{ convexit }_INPUT| ).

    TRY.
        " CUNIT reads the unit in a language; the others take no such parameter
        IF from IS SUPPLIED AND convexit = `CUNIT`.
          CALL FUNCTION conex
            EXPORTING
              input    = from
              language = sy-langu
            IMPORTING
              output   = value
            EXCEPTIONS
              " a plain MESSAGE TYPE E of the exit ends the request without it
              error_message = 98
              OTHERS        = 99.
        ELSEIF from IS SUPPLIED.
          CALL FUNCTION conex
            EXPORTING
              input  = from
            IMPORTING
              output = value
            EXCEPTIONS
              error_message = 98
              OTHERS        = 99.
        ELSEIF convexit = `CUNIT`.
          CALL FUNCTION conex
            EXPORTING
              input    = value
              language = sy-langu
            IMPORTING
              output   = value
            EXCEPTIONS
              error_message = 98
              OTHERS        = 99.
        ELSE.
          CALL FUNCTION conex
            EXPORTING
              input  = value
            IMPORTING
              output = value
            EXCEPTIONS
              error_message = 98
              OTHERS        = 99.
        ENDIF.

      CATCH cx_root INTO lx_error.
        " no such exit, or a parameter it does not accept
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_error.
    ENDTRY.

    IF sy-subrc <> 0.
      " the exit raised its exception with MESSAGE ... RAISING, or sent a
      " plain MESSAGE TYPE E (error_message) - both leave it in sy-msg*
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE `E` NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
      ELSE.
        lv_message = |{ conex } failed|.
      ENDIF.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = lv_message.
    ENDIF.

  ENDMETHOD.

  METHOD convexit_ext.

    IF check_abap_cloud( ).

    ELSE.

      DATA(conv) = |CONVERSION_EXIT_{ name-convexit }_INPUT|.
      DATA conex TYPE c LENGTH 30.
      DATA(lv_tab) = 'TFDIR'.

      SELECT SINGLE funcname FROM (lv_tab)
        WHERE funcname = @conv
        INTO @conex.

      IF sy-subrc = 0.

        CALL FUNCTION conex
          EXPORTING
            input         = val
          IMPORTING
            output        = result
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD filter_get_sql_by_sql_string.

    DATA(lv_sql) = CONV string( val ).

    DATA(lv_squished) = lv_sql.
    lv_squished = replace( val = lv_squished sub = ` ` with = `` occ = 0 ).
    lv_squished = to_upper( lv_squished ).
    SPLIT lv_squished AT `SELECTFROM` INTO DATA(lv_dummy) DATA(lv_tab).
    SPLIT lv_tab AT `FIELDS` INTO lv_tab lv_dummy.
    SPLIT lv_tab AT `WHERE` INTO lv_tab lv_dummy.

    result-tabname = lv_tab.

    " the word behind FROM, where the statement has one: cutting the squished
    " text at FIELDS and WHERE took `ZMY_` out of `zmy_fields`
    DATA(lv_words) = | { condense( to_upper( lv_sql ) ) } |.
    DATA(lv_from) = find( val = lv_words
                          sub = ` FROM ` ).
    IF lv_from >= 0.
      SPLIT substring( val = lv_words
                       off = lv_from + 6 ) AT ` ` INTO lv_tab lv_dummy.
      IF lv_tab IS NOT INITIAL.
        result-tabname = lv_tab.
      ENDIF.
    ENDIF.

    DATA(lv_upper) = to_upper( lv_sql ).
    IF lv_upper CS ` WHERE `.
      DATA(lv_pos) = sy-fdpos + 7.
      result-where = c_trim( substring( val = lv_sql
                                        off = lv_pos ) ).
      result-t_filter = filter_get_multi_by_sql_where( result-where ).
    ENDIF.

  ENDMETHOD.

  METHOD time_get_text_by_stampl.

    DATA lv_zone TYPE c LENGTH 6.

    IF time_zone IS INITIAL.
      result = |{ val TIMESTAMP = ISO }|.
    ELSE.
      lv_zone = time_zone.
      result = |{ val TIMESTAMP = ISO TIMEZONE = lv_zone }|.
    ENDIF.

    " 2026-11-23T14:05:09,0000000 -> 2026-11-23 14:05:09
    result = replace( val = result sub = `T` with = ` ` ).
    result = replace( val = result sub = `,0000000` with = `` ).

  ENDMETHOD.

  METHOD time_get_date_by_stampl.
    DATA(ls_sy) = context_get_sy( ).
    CONVERT TIME STAMP val TIME ZONE ls_sy-zonlo INTO DATE result TIME DATA(lv_dummy).
  ENDMETHOD.

  METHOD time_get_timestamp.
    GET TIME STAMP FIELD result.
  ENDMETHOD.

  METHOD time_get_timestampl.
    GET TIME STAMP FIELD result.
  ENDMETHOD.

  METHOD time_get_time_by_stampl.
    DATA(ls_sy) = context_get_sy( ).
    CONVERT TIME STAMP val TIME ZONE ls_sy-zonlo INTO DATE DATA(lv_dummy) TIME result.
  ENDMETHOD.

  METHOD time_subtract_seconds.

    result = cl_abap_tstmp=>subtractsecs( tstmp = time
                                          secs  = seconds ).

  ENDMETHOD.

  METHOD unassign_data.

    FIELD-SYMBOLS <unassign> TYPE any.

    " IS ASSIGNED, not sy-subrc: on some runtimes ASSIGN ref->* of an
    " unbound reference leaves sy-subrc untouched, so a stale value from an
    " earlier statement decided the answer - a stale 4 read as "not
    " assigned" and a stale 0 let the initial field symbol through. The
    " symbol is declared fresh in this method and assigned exactly once, so
    " no UNASSIGN is needed before the test
    ASSIGN val->* TO <unassign>.
    IF <unassign> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    result = <unassign>.

  ENDMETHOD.

  METHOD unassign_object.

    FIELD-SYMBOLS <unassign> TYPE any.

    " same reasoning as unassign_data directly above
    ASSIGN val->* TO <unassign>.
    IF <unassign> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    result = <unassign>.

  ENDMETHOD.

  METHOD url_param_create_url.

    LOOP AT t_params INTO DATA(ls_param).
      result = |{ result }{ ls_param-n }={ ls_param-v }&|.
    ENDLOOP.
    result = shift_right( val = result
                          sub = `&` ).

  ENDMETHOD.

  METHOD url_param_get.

    DATA(lt_params) = url_param_get_tab( url ).
    DATA(lv_val) = c_trim_lower( val ).
    result = VALUE #( lt_params[ n = lv_val ]-v OPTIONAL ).

  ENDMETHOD.

  METHOD url_param_get_tab.

    " a full URL or a request URI carries a path before its query: cut at
    " the FIRST `?` only. A later `?` is a legal, unencoded character of a
    " value (`title=why?`) - cutting there dropped every parameter before
    " it, app_start included, and the shell fell back to the start page
    " declared, not inline: DATA( ) from a generic CLIKE parameter is
    " "fixed type STRING used for generic type CLIKE" in the extended check
    DATA lv_search TYPE string.
    lv_search = val.
    IF lv_search CS `?`.
      lv_search = substring_after( val = lv_search
                                   sub = `?` ).
    ENDIF.

    " The FLP packs the target's own parameters into ONE value,
    " sap-startup-params, with its `=` and `&` percent-encoded. Prepend &
    " before searching so it is also unwrapped when it is the first/only
    " query parameter (typical FLP target mapping). Only THAT value is
    " decoded: decoding the whole query first tore every legitimately
    " encoded `&` or `=` in any other value apart (`a=x%26y` became the
    " two parameters a=x and y=), and app_get_url wrote the damage back
    " into every generated link
    DATA(lv_startup) = substring_after( val = |&{ lv_search }|
                                        sub = `&sap-startup-params=` ).
    IF lv_startup IS NOT INITIAL.
      " what stands BEFORE the wrapper is as much a parameter as what follows
      " it: `?app_start=x&sap-startup-params=...` used to lose app_start, and
      " a `sap-client` in front of the wrapper was gone from every link
      " app_get_url rebuilt from this table. lv_before keeps the prepended
      " `&`, so the empty first segment it produces below is skipped like a
      " trailing one
      DATA(lv_before) = substring_before( val = |&{ lv_search }|
                                          sub = `&sap-startup-params=` ).
      SPLIT lv_startup AT `&` INTO DATA(lv_packed) DATA(lv_rest).
      lv_packed = replace( val  = lv_packed
                           sub  = `%3D`
                           with = `=`
                           occ  = 0 ).
      " RFC 3986 allows lowercase hex digits in percent-encodings, so decode
      " %3d the same way as %3D (%26 contains no letters and needs no twin)
      lv_packed = replace( val  = lv_packed
                           sub  = `%3d`
                           with = `=`
                           occ  = 0 ).
      lv_packed = replace( val  = lv_packed
                           sub  = `%26`
                           with = `&`
                           occ  = 0 ).
      lv_search = lv_packed.
      IF lv_rest IS NOT INITIAL.
        lv_search = |{ lv_packed }&{ lv_rest }|.
      ENDIF.
      IF lv_before IS NOT INITIAL.
        lv_search = |{ lv_before }&{ lv_search }|.
      ENDIF.
    ENDIF.

    SPLIT lv_search AT `&` INTO TABLE DATA(lt_param).

    LOOP AT lt_param REFERENCE INTO DATA(lr_param).
      SPLIT lr_param->* AT `=` INTO DATA(lv_name) DATA(lv_value).
      " an empty segment (empty search string, trailing &) would otherwise
      " produce a phantom nameless parameter that url_param_create_url
      " writes back out as a stray `=&`
      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.
      " normalize the name so lookups are case-insensitive on every input
      " shape (with or without a leading path/question mark) - the value
      " keeps its original case
      INSERT VALUE #( n = c_trim_lower( lv_name )
                      v = lv_value ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD url_param_set.

    DATA(lt_params) = url_param_get_tab( url ).
    DATA(lv_n) = c_trim_lower( name ).
    DATA(lv_v) = c_trim( value ).

    LOOP AT lt_params REFERENCE INTO DATA(lr_params)
         WHERE n = lv_n.
      lr_params->v = lv_v.
    ENDLOOP.
    IF sy-subrc <> 0.
      INSERT VALUE #( n = lv_n
                      v = lv_v ) INTO TABLE lt_params.
    ENDIF.

    result = url_param_create_url( lt_params ).

  ENDMETHOD.

  METHOD xml_parse.

    IF xml IS INITIAL.
      CLEAR any.
      RETURN.
    ENDIF.

    " a broken document, or one written for a type that has changed since,
    " raises a static transformation error - out of a method without RAISING
    " it reached the caller as CX_SY_NO_HANDLER, which no CATCH of this
    " repository's one exception class sees
    TRY.
        CALL TRANSFORMATION id
             SOURCE XML xml
             RESULT data = any.
      CATCH cx_transformation_error INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val      = `XML_PARSE_ERROR - the XML does not fit the target`
                    previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD xml_srtti_parse.

    " the one combined document holds both elements, so it is the pair
    " parse with the same document on both sides - one copy of the
    " GET_RTTI / cast / CREATE DATA TYPE HANDLE sequence instead of two
    " that drift apart. Only a draft written before the split still comes
    " here (z2ui5_cl_ui5_srv_model=>attri_srtti_parse)
    result = xml_srtti_parse_pair( iv_type = rtti_data
                                   iv_data = rtti_data ).

  ENDMETHOD.

  METHOD xml_srtti_stringify.

    IF rtti_check_class_exists( `ZCL_SRTTI_TYPEDESCR` ) = abap_true.

      DATA srtti TYPE REF TO object.
      DATA(lv_classname) = `ZCL_SRTTI_TYPEDESCR`.
      CALL METHOD (lv_classname)=>(`CREATE_BY_DATA_OBJECT`)
        EXPORTING
          data_object = data
        RECEIVING
          srtti       = srtti.
      CALL TRANSFORMATION id SOURCE srtti = srtti dobj = data RESULT XML result.

    ELSE.

      TRY.
          CALL METHOD zabaputil_cl_srt_typedescr=>(`CREATE_BY_DATA_OBJECT`)
            EXPORTING
              data_object = data
            RECEIVING
              srtti       = srtti.
          CALL TRANSFORMATION id SOURCE srtti = srtti dobj = data RESULT XML result.

        CATCH cx_root INTO DATA(lx_srtti).

          " keep the root cause - a transformation error on the caller's own
          " data must not be masked behind a bare UNSUPPORTED_FEATURE
          DATA(lv_text) = `UNSUPPORTED_FEATURE`.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val      = lv_text
              previous = lx_srtti.

      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD xml_stringify.

    CALL TRANSFORMATION id
         SOURCE data = any
         RESULT XML result
         OPTIONS data_refs = `heap-or-create`.

  ENDMETHOD.

  METHOD x_check_raise.

    IF when = abap_true.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = v.
    ENDIF.

  ENDMETHOD.

  METHOD x_get_last_t100.

    DATA(x) = val.
    DO.

      IF x->previous IS BOUND.
        x = x->previous.
        CONTINUE.
      ENDIF.

      EXIT.
    ENDDO.

    result = x->get_text( ).

  ENDMETHOD.

  METHOD x_raise.

    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = v.

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_table_name.

    DATA lo_obj    TYPE REF TO cl_abap_typedescr.
    DATA lo_struct TYPE REF TO cl_abap_structdescr.

    IF table_name IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `TABLE_NAME_INITIAL_ERROR`.
    ENDIF.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = table_name
                                         RECEIVING  p_descr_ref    = lo_obj
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0.
      " an unknown name has no columns - the result stays empty, as it always
      " did (the former TABLE_NOT_FOUND raise sat inside its own CATCH cx_root)
      RETURN.
    ENDIF.

    " one describe: a structure (DDIC table, structure, view) or a table
    " type, whose line type is the structure. Anything else has no columns
    TRY.
        CASE lo_obj->kind.
          WHEN cl_abap_typedescr=>kind_struct.
            lo_struct = CAST #( lo_obj ).
          WHEN cl_abap_typedescr=>kind_table.
            lo_struct = CAST #( CAST cl_abap_tabledescr( lo_obj )->get_table_line_type( ) ).
          WHEN OTHERS.
            RETURN.
        ENDCASE.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    " expand_components also applies the RENAMING WITH SUFFIX of an include
    result = expand_components( lo_struct->get_components( ) ).

  ENDMETHOD.

  METHOD itab_corresponding.

    FIELD-SYMBOLS <row_in>  TYPE any.
    FIELD-SYMBOLS <row_out> TYPE any.

    LOOP AT val ASSIGNING <row_in>.
      INSERT INITIAL LINE INTO TABLE tab ASSIGNING <row_out>.
      MOVE-CORRESPONDING <row_in> TO <row_out>.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_get_by_struc.

    DATA(lt_attri) = rtti_get_t_attri_by_any( val ).
    LOOP AT lt_attri REFERENCE INTO DATA(lr_attri).

      ASSIGN COMPONENT lr_attri->name OF STRUCTURE val TO FIELD-SYMBOL(<component>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE rtti_get_type_kind( <component> ).

        " skip components that cannot be moved into the string value: tables,
        " nested structures and references would raise an unhandled move
        " error at runtime
        WHEN cl_abap_typedescr=>typekind_table OR
             cl_abap_typedescr=>typekind_struct1 OR
             cl_abap_typedescr=>typekind_struct2 OR
             cl_abap_typedescr=>typekind_dref OR
             cl_abap_typedescr=>typekind_oref.

        WHEN OTHERS.
          " through data_get_string, not a plain MOVE: that writes the sign
          " BEHIND a number, so -5 read `5-` and a packed -12.50 `12.50-`
          INSERT VALUE #(
            n = lr_attri->name
            v = data_get_string( <component> )
            ) INTO TABLE result.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD itab_filter_by_t_range.

    DATA ref TYPE REF TO data.

    LOOP AT tab REFERENCE INTO ref.
      LOOP AT val INTO DATA(ls_filter).

        IF ls_filter-t_range IS INITIAL.
          CONTINUE.
        ENDIF.

        ASSIGN ref->(ls_filter-name) TO FIELD-SYMBOL(<field>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        IF <field> NOT IN ls_filter-t_range.
          DELETE tab.
          EXIT.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_data_by_multi.

    LOOP AT val INTO DATA(ls_filter).
      IF lines( ls_filter-t_range ) > 0
        OR lines( ls_filter-t_token ) > 0.
        INSERT ls_filter INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_sql_where.

    LOOP AT val INTO DATA(ls_filter).

      IF ls_filter-t_range IS INITIAL.
        CONTINUE.
      ENDIF.

      " the select-options are bound to the view, so the name comes back from
      " the browser - only a plain field name goes into the WHERE clause, never
      " a condition or a subquery on another table
      " a character set instead of a regex: PCRE is not on 7.02, POSIX not
      " on ABAP Cloud
      IF ls_filter-name IS INITIAL
          OR ls_filter-name CN `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_/`.
        CONTINUE.
      ENDIF.

      " select-option semantics: a row matches one of the including ranges
      " and none of the excluding ones. An excluding range arrives here
      " already negated (filter_get_sql_cond_by_range), so the includes are
      " joined with OR and the excludes with AND - OR-ing them too let
      " every row through that missed a single exclude
      DATA(lv_include) = ``.
      DATA(lv_exclude) = ``.
      LOOP AT ls_filter-t_range INTO DATA(ls_range).
        DATA(lv_cond) = filter_get_sql_cond_by_range( fieldname = ls_filter-name
                                                      range     = ls_range ).
        IF lv_cond IS INITIAL.
          CONTINUE.
        ENDIF.
        IF ls_range-sign = `E`.
          lv_exclude = COND #( WHEN lv_exclude IS INITIAL THEN lv_cond
                               ELSE |{ lv_exclude } AND { lv_cond }| ).
        ELSE.
          lv_include = COND #( WHEN lv_include IS INITIAL THEN lv_cond
                               ELSE |{ lv_include } OR { lv_cond }| ).
        ENDIF.
      ENDLOOP.

      DATA(lv_field_where) = COND string(
        WHEN lv_include IS NOT INITIAL AND lv_exclude IS NOT INITIAL
        THEN |( { lv_include } ) AND { lv_exclude }|
        WHEN lv_include IS NOT INITIAL
        THEN lv_include
        ELSE lv_exclude ).

      IF lv_field_where IS INITIAL.
        CONTINUE.
      ENDIF.

      IF result IS INITIAL.
        result = |( { lv_field_where } )|.
      ELSE.
        result = |{ result } AND ( { lv_field_where } )|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_sql_cond_by_range.

    DATA(lv_low) = replace( val  = range-low
                            sub  = `'`
                            with = `''`
                            occ  = 0 ).
    DATA(lv_high) = replace( val  = range-high
                             sub  = `'`
                             with = `''`
                             occ  = 0 ).
    DATA(lv_option) = range-option.

    IF range-sign = `E`.
      lv_option = filter_option_negate( lv_option ).
    ENDIF.

    " CP: * and + are the wildcards and # makes the next character a plain
    " one (`#*` is a star), as in an IN range. A _ or % the user typed is a
    " plain character, so it is escaped for LIKE
    DATA(lv_like) = ``.
    DATA(lv_pos) = 0.
    DATA(lv_len) = strlen( lv_low ).
    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = lv_low
                                 off = lv_pos
                                 len = 1 ).
      IF lv_char = `#` AND lv_pos + 1 < lv_len.
        lv_pos = lv_pos + 1.
        lv_char = substring( val = lv_low
                             off = lv_pos
                             len = 1 ).
        IF lv_char CA `#%_`.
          lv_like = lv_like && `#` && lv_char.
        ELSE.
          lv_like = lv_like && lv_char.
        ENDIF.
      ELSEIF lv_char = `*`.
        lv_like = lv_like && `%`.
      ELSEIF lv_char = `+`.
        lv_like = lv_like && `_`.
      ELSEIF lv_char CA `#%_`.
        lv_like = lv_like && `#` && lv_char.
      ELSE.
        lv_like = lv_like && lv_char.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    CASE lv_option.
      WHEN `EQ`.
        result = |{ fieldname } = '{ lv_low }'|.
      WHEN `NE`.
        result = |{ fieldname } <> '{ lv_low }'|.
      WHEN `LT`.
        result = |{ fieldname } < '{ lv_low }'|.
      WHEN `LE`.
        result = |{ fieldname } <= '{ lv_low }'|.
      WHEN `GT`.
        result = |{ fieldname } > '{ lv_low }'|.
      WHEN `GE`.
        result = |{ fieldname } >= '{ lv_low }'|.
      WHEN `CP`.
        result = |{ fieldname } LIKE '{ lv_like }' ESCAPE '#'|.
      WHEN `NP`.
        result = |{ fieldname } NOT LIKE '{ lv_like }' ESCAPE '#'|.
      WHEN `BT`.
        result = |{ fieldname } BETWEEN '{ lv_low }' AND '{ lv_high }'|.
      WHEN `NB`.
        result = |{ fieldname } NOT BETWEEN '{ lv_low }' AND '{ lv_high }'|.
    ENDCASE.

  ENDMETHOD.

  METHOD filter_get_multi_by_sql_where.
    DATA(lv_where) = c_trim( CONV string( val ) ).
    IF lv_where IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_groups) = filter_sql_split_top_level( val = lv_where
                                                  sep = ` AND ` ).

    LOOP AT lt_groups INTO DATA(lv_group).

      lv_group = filter_sql_strip_parens( lv_group ).

      DATA(lt_parts) = filter_sql_split_top_level( val = lv_group
                                                   sep = ` AND ` ).

      IF lines( lt_parts ) <= 1.
        DATA(ls_filter) = filter_sql_get_or_group( lv_group ).
        IF ls_filter-name IS NOT INITIAL.
          INSERT ls_filter INTO TABLE result.
        ENDIF.
        CONTINUE.
      ENDIF.

      " several conditions joined with AND inside one group. On one field
      " that is the shape filter_get_sql_where writes for excluding ranges:
      " `( includes ) AND exclude AND exclude`, or the excludes alone. The
      " excludes were negated on the way out and are turned back here.
      " On different fields they are filters of their own
      DATA lt_sub TYPE ty_t_filter_multi.
      DATA lv_first_wrapped TYPE abap_bool.
      DATA lv_same_field TYPE abap_bool.
      CLEAR lt_sub.
      lv_same_field = abap_true.
      LOOP AT lt_parts INTO DATA(lv_part).
        DATA(lv_tabix) = sy-tabix.
        lv_part = c_trim( lv_part ).
        IF lv_tabix = 1.
          lv_first_wrapped = xsdbool( lv_part IS NOT INITIAL AND lv_part(1) = `(` ).
        ENDIF.
        DATA(ls_sub) = filter_sql_get_or_group( filter_sql_strip_parens( lv_part ) ).
        IF ls_sub-name IS INITIAL.
          CONTINUE.
        ENDIF.
        IF lt_sub IS NOT INITIAL AND to_upper( lt_sub[ 1 ]-name ) <> to_upper( ls_sub-name ).
          lv_same_field = abap_false.
        ENDIF.
        INSERT ls_sub INTO TABLE lt_sub.
      ENDLOOP.

      IF lv_same_field = abap_false.
        INSERT LINES OF lt_sub INTO TABLE result.
        CONTINUE.
      ENDIF.

      CLEAR ls_filter.
      LOOP AT lt_sub INTO ls_sub.
        lv_tabix = sy-tabix.
        ls_filter-name = lt_sub[ 1 ]-name.
        IF lv_tabix = 1 AND lv_first_wrapped = abap_true.
          INSERT LINES OF ls_sub-t_range INTO TABLE ls_filter-t_range.
          CONTINUE.
        ENDIF.
        LOOP AT ls_sub-t_range INTO DATA(ls_range).
          ls_range-sign = `E`.
          ls_range-option = filter_option_negate( ls_range-option ).
          INSERT ls_range INTO TABLE ls_filter-t_range.
        ENDLOOP.
      ENDLOOP.
      IF ls_filter-name IS NOT INITIAL.
        INSERT ls_filter INTO TABLE result.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD filter_get_range_by_sql_cond.
    CLEAR range.
    CLEAR fieldname.

    DATA(lv_cond) = CONV string( val ).
    range-sign = `I`.

    DATA lv_rest TYPE string.
    DATA lv_off  TYPE i.
    DATA lv_and  TYPE i.
    DATA lv_esc  TYPE i.

    " the keywords are looked for outside of the literals and whatever their
    " case: `TEXT = 'A LIKE B'` is an equality, `name like 'A%'` a pattern
    lv_off = filter_sql_find( val     = lv_cond
                              keyword = ` NOT BETWEEN ` ).
    IF lv_off < 0.
      lv_off = filter_sql_find( val     = lv_cond
                                keyword = ` BETWEEN ` ).
      IF lv_off >= 0.
        range-option = `BT`.
        lv_rest = substring( val = lv_cond
                             off = lv_off + 9 ).
      ENDIF.
    ELSE.
      range-option = `NB`.
      lv_rest = substring( val = lv_cond
                           off = lv_off + 13 ).
    ENDIF.
    IF lv_off >= 0.
      fieldname = c_trim( substring( val = lv_cond
                                     len = lv_off ) ).
      lv_and = filter_sql_find( val     = lv_rest
                                keyword = ` AND ` ).
      IF lv_and < 0.
        range-low = filter_sql_strip_quotes( lv_rest ).
      ELSE.
        range-low  = filter_sql_strip_quotes( substring( val = lv_rest
                                                         len = lv_and ) ).
        range-high = filter_sql_strip_quotes( substring( val = lv_rest
                                                         off = lv_and + 5 ) ).
      ENDIF.
      RETURN.
    ENDIF.

    lv_off = filter_sql_find( val     = lv_cond
                              keyword = ` NOT LIKE ` ).
    IF lv_off < 0.
      lv_off = filter_sql_find( val     = lv_cond
                                keyword = ` LIKE ` ).
      IF lv_off >= 0.
        range-option = `CP`.
        lv_rest = substring( val = lv_cond
                             off = lv_off + 6 ).
      ENDIF.
    ELSE.
      range-option = `NP`.
      lv_rest = substring( val = lv_cond
                           off = lv_off + 10 ).
    ENDIF.
    IF lv_off >= 0.
      fieldname = c_trim( substring( val = lv_cond
                                     len = lv_off ) ).
      " `'A#%%' ESCAPE '#'`: the pattern, and the character that makes the
      " next one a plain character
      lv_esc = filter_sql_find( val     = lv_rest
                                keyword = ` ESCAPE ` ).
      IF lv_esc < 0.
        range-low = filter_sql_like_to_pattern( filter_sql_strip_quotes( lv_rest ) ).
      ELSE.
        DATA(lv_escape) = filter_sql_strip_quotes( substring( val = lv_rest
                                                              off = lv_esc + 8 ) ).
        DATA(lv_like) = filter_sql_strip_quotes( substring( val = lv_rest
                                                            len = lv_esc ) ).
        DATA(lv_pos) = 0.
        DATA(lv_plain) = ``.
        DATA(lv_segment) = ``.
        " the escaped characters are taken out before % and _ are mapped
        WHILE lv_pos < strlen( lv_like ).
          DATA(lv_char) = substring( val = lv_like
                                     off = lv_pos
                                     len = 1 ).
          IF lv_escape IS NOT INITIAL AND lv_char = lv_escape AND lv_pos + 1 < strlen( lv_like ).
            " a plain character of the LIKE pattern - in CP a #, * and +
            " need the # in front to stay plain
            DATA(lv_escaped) = substring( val = lv_like
                                          off = lv_pos + 1
                                          len = 1 ).
            IF lv_escaped CA `#*+`.
              lv_escaped = `#` && lv_escaped.
            ENDIF.
            lv_plain = lv_plain && filter_sql_like_to_pattern( lv_segment ) && lv_escaped.
            CLEAR lv_segment.
            lv_pos = lv_pos + 2.
            CONTINUE.
          ENDIF.
          lv_segment = lv_segment && lv_char.
          lv_pos = lv_pos + 1.
        ENDWHILE.
        range-low = lv_plain && filter_sql_like_to_pattern( lv_segment ).
      ENDIF.
      RETURN.
    ENDIF.

    " the two-character operators before the ones they contain
    DATA lt_operator TYPE ty_t_name_value.
    lt_operator = VALUE #( ( n = `<>` v = `NE` )
                           ( n = `<=` v = `LE` )
                           ( n = `>=` v = `GE` )
                           ( n = `<`  v = `LT` )
                           ( n = `>`  v = `GT` )
                           ( n = `=`  v = `EQ` ) ).
    LOOP AT lt_operator INTO DATA(ls_operator).
      lv_off = filter_sql_find( val     = lv_cond
                                keyword = | { ls_operator-n } | ).
      IF lv_off < 0.
        CONTINUE.
      ENDIF.
      fieldname    = c_trim( substring( val = lv_cond
                                        len = lv_off ) ).
      range-option = ls_operator-v.
      range-low    = filter_sql_strip_quotes( substring( val = lv_cond
                                                         off = lv_off + strlen( ls_operator-n ) + 2 ) ).
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD filter_sql_split_top_level.

    DATA(lv_val) = CONV string( val ).
    DATA(lv_sep) = to_upper( CONV string( sep ) ).
    DATA(lv_len) = strlen( lv_val ).
    DATA(lv_sep_len) = strlen( lv_sep ).
    DATA(lv_depth) = 0.
    DATA(lv_start) = 0.
    DATA(lv_pos) = 0.
    DATA(lv_in_quote) = abap_false.
    DATA(lv_in_between) = abap_false.

    IF lv_val IS INITIAL.
      RETURN.
    ENDIF.

    IF lv_sep_len = 0.
      INSERT lv_val INTO TABLE result.
      RETURN.
    ENDIF.

    WHILE lv_pos < lv_len.

      DATA(lv_char) = lv_val+lv_pos(1).

      IF lv_char = `'`.
        IF lv_in_quote = abap_false.
          lv_in_quote = abap_true.
        ELSE.
          lv_in_quote = abap_false.
        ENDIF.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      IF lv_in_quote = abap_true.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      IF lv_char = `(`.
        lv_depth = lv_depth + 1.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      IF lv_char = `)`.
        lv_depth = lv_depth - 1.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      " Track BETWEEN ... AND to avoid splitting at the AND inside BETWEEN
      " Only check when current char is B or b (performance: skip to_upper on every pos)
      IF lv_depth = 0 AND lv_in_between = abap_false
         AND ( lv_char = `B` OR lv_char = `b` )
         AND lv_pos + 8 <= lv_len
         AND to_upper( lv_val+lv_pos(8) ) = `BETWEEN `.
        lv_in_between = abap_true.
        lv_pos = lv_pos + 8.
        CONTINUE.
      ENDIF.

      " The first AND after BETWEEN is the range AND, not a logical AND
      " Only check when current char is space (the ` AND ` pattern starts with space)
      IF lv_in_between = abap_true
         AND lv_char = ` `
         AND lv_pos + 5 <= lv_len
         AND to_upper( lv_val+lv_pos(5) ) = ` AND `.
        lv_in_between = abap_false.
        lv_pos = lv_pos + 5.
        CONTINUE.
      ENDIF.

      " whatever the case of the keyword - `and` separates like `AND`
      IF lv_depth = 0
         AND lv_in_between = abap_false
         AND lv_pos + lv_sep_len <= lv_len
         AND to_upper( lv_val+lv_pos(lv_sep_len) ) = lv_sep.
        INSERT substring( val = lv_val
                          off = lv_start
                          len = lv_pos - lv_start ) INTO TABLE result.
        lv_pos = lv_pos + lv_sep_len.
        lv_start = lv_pos.
        CONTINUE.
      ENDIF.

      lv_pos = lv_pos + 1.

    ENDWHILE.

    INSERT substring( val = lv_val
                      off = lv_start ) INTO TABLE result.

  ENDMETHOD.

  METHOD filter_sql_strip_quotes.

    result = c_trim( val ).
    DATA(lv_len) = strlen( result ).

    IF lv_len >= 2
       AND result(1) = `'`
       AND substring( val = result
                      off = lv_len - 1
                      len = 1 ) = `'`.
      result = substring( val = result
                          off = 1
                          len = lv_len - 2 ).
      result = replace( val  = result
                        sub  = `''`
                        with = `'`
                        occ  = 0 ).
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_t.

    result = msg_get_internal( val ).
    IF result IS INITIAL AND val2 IS NOT INITIAL.
      result = msg_get_internal( val2 ).
    ENDIF.

  ENDMETHOD.

  METHOD rtti_check_clike.

    CASE rtti_get_type_kind( val ).
      " typekind_clike/_csequence are generic kinds of formal parameters and
      " can never be returned for a concrete data object - list the concrete
      " character-like kinds (c, string, n, d, t) instead
      WHEN cl_abap_datadescr=>typekind_char OR
          cl_abap_datadescr=>typekind_string OR
          cl_abap_datadescr=>typekind_num OR
          cl_abap_datadescr=>typekind_date OR
          cl_abap_datadescr=>typekind_time.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.

  METHOD rtti_check_printable.

    IF rtti_check_clike( val ) = abap_true.
      result = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_type_kind) = rtti_get_type_kind( val ).
    result = xsdbool( lv_type_kind = cl_abap_datadescr=>typekind_hex
                      OR rtti_check_typekind_number( lv_type_kind ) = abap_true ).

  ENDMETHOD.

  METHOD error_get_source_position.

    " typed like the EXPORTING parameters of get_source_position (syrepid is
    " CHAR40) - they are passed by reference, so a string would not be
    " type-compatible here
    DATA lv_program TYPE c LENGTH 40.
    DATA lv_include TYPE c LENGTH 40.
    DATA lv_line    TYPE i.

    IF val IS NOT BOUND.
      RETURN.
    ENDIF.

    " open-abap - the transpiled JS runtime behind the node tests - implements
    " get_source_position by reading a field that only the RAISE statement
    " writes, and fails with a JS TypeError for every other exception (a
    " runtime-thrown one, or one built with NEW). That error is not an ABAP
    " exception, so the TRY below cannot intercept it and the caller would go
    " down instead of reporting the error it was diagnosing. sy-saprl is
    " `OPEN` on that runtime and a real release everywhere else.
    IF sy_get( )-saprl = `OPEN`.
      RETURN.
    ENDIF.

    TRY.
        val->get_source_position( IMPORTING program_name = lv_program
                                            include_name = lv_include
                                            source_line  = lv_line ).
      CATCH cx_root ##NO_HANDLER.
        " never let the diagnostic be the reason a caller fails
    ENDTRY.

    IF lv_program IS INITIAL AND lv_line IS INITIAL.
      RETURN.
    ENDIF.

    result = c_trim( lv_program ).
    " the include is the interesting part for a class (...CM001 = the method
    " that raised); repeating it when it equals the program adds nothing
    IF lv_include IS NOT INITIAL AND lv_include <> lv_program.
      result = |{ result } / { c_trim( lv_include ) }|.
    ENDIF.
    IF lv_line IS NOT INITIAL.
      result = |{ result } / line { lv_line }|.
    ENDIF.

  ENDMETHOD.

  METHOD error_get_attributes.

    " declared, NOT `DATA(lv_name) = CONV string( lr_attri->name )`: a CONV
    " that is the whole right-hand side of an assignment is "Redundant
    " conversion for type STRING" in SLIN - the assignment converts by
    " itself. The variable stays a string on purpose: the RTTI name is a
    " CHAR field whose trailing blanks have no business in the rendered name
    DATA lv_name TYPE string.

    FIELD-SYMBOLS <comp> TYPE any.

    IF val IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(lt_attri) = rtti_get_t_attri_by_oref( val ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    LOOP AT lt_attri REFERENCE INTO DATA(lr_attri) "#EC CI_SORTSEQ
         WHERE visibility  = cv_objectdescr_public
           AND is_constant = abap_false
           AND is_class    = abap_false.

      CASE lr_attri->name.
          " rendered by the caller (PREVIOUS as the next chain entry,
          " KERNEL_ERRID as its own line) or without information value
        WHEN `PREVIOUS` OR `TEXTID` OR `IS_RESUMABLE` OR `KERNEL_ERRID`.
          CONTINUE.
      ENDCASE.

      lv_name = lr_attri->name.
      " UNASSIGN + IS ASSIGNED, not sy-subrc - see unassign_data for the
      " release behaviour (#1937). INSIDE A LOOP the stale-subrc reading is
      " not just "skipped one attribute": a failed ASSIGN leaves the symbol
      " on the PREVIOUS iteration's attribute, so a stale 0 renders that
      " value under this attribute's name - a wrong value in the error dump,
      " which is the one place a reader has nothing else to go by
      UNASSIGN <comp>.
      ASSIGN val->(lv_name) TO <comp>.
      IF <comp> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      IF rtti_check_printable( <comp> ) = abap_false OR <comp> IS INITIAL.
        CONTINUE.
      ENDIF.

      INSERT VALUE #( n = lv_name
                      v = c_trim( |{ <comp> }| ) ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD ui5_get_msg_type.

    " A (abort) and X (exit) are the two SAP message types above E - a
    " BAPIRET2 or SYMSG row of either is an error, never an information
    CASE val.
      WHEN `E` OR `A` OR `X`.
        result = cs_ui5_msg_type-e.
      WHEN `S`.
        result = cs_ui5_msg_type-s.
      WHEN `W`.
        result = cs_ui5_msg_type-w.
      WHEN OTHERS.
        result = cs_ui5_msg_type-i.
    ENDCASE.

  ENDMETHOD.

  METHOD ui5_escape_binding_literal.

    result = val.

    " nothing to escape: no brace, no backslash. Note that the shortcut is
    " NOT "no brace": UI5's XMLTemplateProcessor hands EVERY string property
    " to the binding parser with unescaping on, brace or not, and the
    " parser unescapes `\\`, `\{` and `\}` even when it finds no binding
    " (OpenUI5 1.71 BindingParser.complexParser, the bUnescaped tail). A
    " lone backslash is left alone, so `C:\temp\file` rendered fine either
    " way - but `\\server\share` lost one backslash per pair until the
    " backslash was escaped whenever present (2026-09-05)
    IF result NA `{}\`.
      RETURN.
    ENDIF.

    " the backslash first (UI5 unescapes `\\` back to one), then the braces,
    " which would otherwise start a binding
    result = replace( val  = result
                      sub  = `\`
                      with = `\\`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `{`
                      with = `\{`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `}`
                      with = `\}`
                      occ  = 0 ).

  ENDMETHOD.

  METHOD ui5_escape_handler_string.

    " same fast path as c_escape_xml: the five
    " REPLACE ALL passes below each copy the whole string even when nothing
    " matches, and the common value here - an event name, a plain argument -
    " contains none of these characters. Runs once per _event( ) per render,
    " plus once per quoted argument. NOTE the backslash IS in this set
    " (c_escape_xml's set has none)
    result = val.
    IF result NA `\'` AND result NA cv_char_util_cr_lf.
      RETURN.
    ENDIF.
    result = replace( val = result sub = `\` with = `\\` occ = 0 ).
    result = replace( val = result sub = `'` with = `\'` occ = 0 ).
    result = replace( val = result sub = cv_char_util_cr_lf with = `\n` occ = 0 ).
    result = replace( val = result sub = cv_char_util_newline with = `\n` occ = 0 ).
    " a standalone CR (not part of CR+LF, already collapsed above) is a JS
    " line terminator too and would break the '...' literal
    result = replace( val  = result
                      sub  = cv_char_util_cr_lf(1)
                      with = `\r`
                      occ  = 0 ).

  ENDMETHOD.

  METHOD ui5_check_braces_balanced.

    DATA lv_depth TYPE i.
    DATA lv_quote TYPE string.
    DATA lv_char TYPE string.

    DATA lv_val TYPE string.
    lv_val = val.
    DATA(lv_len) = strlen( lv_val ).
    DATA(lv_off) = 0.
    WHILE lv_off < lv_len.
      lv_char = lv_val+lv_off(1).
      IF lv_quote IS NOT INITIAL.
        " inside a string of the expression ({= ${/A} === '}' }) a brace is
        " text; a backslash takes the next character with it
        CASE lv_char.
          WHEN `\`.
            lv_off = lv_off + 1.
          WHEN lv_quote.
            CLEAR lv_quote.
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        CASE lv_char.
          WHEN `'` OR `"`.
            lv_quote = lv_char.
          WHEN `{`.
            lv_depth = lv_depth + 1.
          WHEN `}`.
            lv_depth = lv_depth - 1.
            IF lv_depth < 0.
              RETURN.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
      lv_off = lv_off + 1.
    ENDWHILE.

    result = xsdbool( lv_depth = 0 AND lv_quote IS INITIAL ).

  ENDMETHOD.

  METHOD rtti_create_tab_by_comps.

    DATA lo_struct TYPE REF TO cl_abap_structdescr.
    DATA lo_table  TYPE REF TO cl_abap_tabledescr.

    lo_struct = cl_abap_structdescr=>create( val ).
    lo_table = cl_abap_tabledescr=>create( p_line_type  = lo_struct
                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).
    CREATE DATA result TYPE HANDLE lo_table.

  ENDMETHOD.

  METHOD rtti_check_type_exists.

    DATA lv_name TYPE string.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    lv_name = val.
    " the non-functional form: an unknown name comes back as sy-subrc,
    " where the functional one ends in a classic exception
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = lv_name
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD rtti_create_tab_by_name.

    " type_not_found is a classic exception: called functionally, an unknown
    " name ends in a short dump no CATCH stops - so it is handled here and
    " the caller gets an unbound reference instead
    DATA lo_type TYPE REF TO cl_abap_typedescr.
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = val
                                         RECEIVING  p_descr_ref    = lo_type
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    " a class or an interface of that name is no data type - the kind, not
    " IS INSTANCE OF, which the 7.02 downport cannot express
    IF lo_type->kind = cl_abap_typedescr=>kind_class OR lo_type->kind = cl_abap_typedescr=>kind_intf.
      RETURN.
    ENDIF.
    DATA(data_desc) = CAST cl_abap_datadescr( lo_type ).
    DATA(gr_dyntable_typ) = cl_abap_tabledescr=>create( data_desc ).
    CREATE DATA result TYPE HANDLE gr_dyntable_typ.

  ENDMETHOD.

  METHOD msg_get.

    DATA(lt_msg) = msg_get_t( val  = val
                              val2 = val2 ).
    result = VALUE #( lt_msg[ 1 ] OPTIONAL ).

  ENDMETHOD.

  METHOD msg_get_collect.

    result = concat_lines_of(
      table = VALUE string_table( FOR <r> IN msg_get_t( val = val val2 = val2 ) ( |- { <r>-text }| ) )
      sep   = cv_char_util_newline ).

  ENDMETHOD.

  METHOD rtti_get_data_element_text_l.

    result = rtti_get_data_element_texts( val )-long.

  ENDMETHOD.

  METHOD rtti_get_ddic_type_name.

    result = substring_after( val = CAST cl_abap_elemdescr( type )->absolute_name
                              sub = `\TYPE=` ).

  ENDMETHOD.

  METHOD rtti_get_typedescr_by_data_ref.

    " REFERENCE_IS_INITIAL - see rtti_get_t_attri_by_oref
    cl_abap_typedescr=>describe_by_data_ref( EXPORTING  p_data_ref           = val
                                             RECEIVING p_descr_ref           = result
                                             EXCEPTIONS reference_is_initial = 1
                                                        OTHERS               = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `RTTI_REFERENCE_IS_INITIAL`.
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_typedescr_by_data.

    result = cl_abap_typedescr=>describe_by_data( val ).

  ENDMETHOD.

  METHOD rtti_create_sel_tab_type.

    DATA lt_comp TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    " an unbound reference left <tab> unassigned and describe_by_data dumped
    " with GETWA_NOT_ASSIGNED; a reference to a scalar failed the cast below.
    " Both are the caller's mistake and answer as the framework's exception
    IF ir_tab IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `RTTI_CREATE_SEL_TAB_TYPE: no table reference supplied`.
    ENDIF.
    ASSIGN ir_tab->* TO <tab>.

    DATA lo_table TYPE REF TO cl_abap_tabledescr.
    TRY.
        lo_table = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <tab> ) ).
      CATCH cx_root INTO DATA(lx_cast).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_cast.
    ENDTRY.

    " decide the line kind by RTTI instead of by a failing cast: the CATCH
    " branch cast to cl_abap_elemdescr sat outside any TRY, so a table whose
    " line is itself a table or a reference raised CX_SY_MOVE_CAST_ERROR
    DATA(lo_line) = lo_table->get_table_line_type( ).
    CASE lo_line->kind.
      WHEN cv_typedescr_kind_struct.
        DATA(lo_struct) = CAST cl_abap_structdescr( lo_line ).
        lt_comp = lo_struct->get_components( ).
      WHEN cv_typedescr_kind_elem.
        result-check_table_line = abap_true.
        DATA(lo_elem) = CAST cl_abap_elemdescr( lo_line ).
        INSERT VALUE #( name = `TAB_LINE`
                        type = lo_elem ) INTO TABLE lt_comp.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `RTTI_CREATE_SEL_TAB_TYPE: a table of tables or references has no selectable line`.
    ENDCASE.

    IF add_sel_field = abap_true
        AND NOT line_exists( lt_comp[ name = sel_field_name ] ). "#EC CI_SORTSEQ
      DATA(lo_type_bool) = cl_abap_typedescr=>describe_by_name( `ABAP_BOOL` ).
      INSERT VALUE #( name = sel_field_name
                      type = CAST #( lo_type_bool ) ) INTO TABLE lt_comp.
    ENDIF.

    DATA(lo_line_type) = cl_abap_structdescr=>create( lt_comp ).
    result-tabledescr = cl_abap_tabledescr=>create( lo_line_type ).

  ENDMETHOD.

  METHOD msg_get_by_msg.

    DATA(ls_msg) = VALUE ty_s_msg(
      id         = id
      no         = no
      v1         = v1
      v2         = v2
      v3         = v3
      v4         = v4 ).
    result = msg_get( ls_msg ).

  ENDMETHOD.

  METHOD c_contains.
    " Note: ABAP CS operator is CASE-INSENSITIVE.
    " JS transpilation must use a case-insensitive comparison
    " (e.g. val.toLowerCase().includes(sub.toLowerCase())).
    result = xsdbool( CONV string( val ) CS sub ).

  ENDMETHOD.

  METHOD c_starts_with.

    DATA(lv_val) = CONV string( val ).
    DATA(lv_prefix) = CONV string( prefix ).
    DATA(lv_len) = strlen( lv_prefix ).

    IF strlen( lv_val ) < lv_len.
      result = abap_false.
      RETURN.
    ENDIF.

    result = xsdbool( lv_val(lv_len) = lv_prefix ).

  ENDMETHOD.

  METHOD c_ends_with.

    DATA(lv_val) = CONV string( val ).
    DATA(lv_suffix) = CONV string( suffix ).
    DATA(lv_len_suffix) = strlen( lv_suffix ).
    DATA(lv_len_val) = strlen( lv_val ).

    IF lv_len_val < lv_len_suffix.
      result = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_off) = lv_len_val - lv_len_suffix.
    result = xsdbool( lv_val+lv_off(lv_len_suffix) = lv_suffix ).

  ENDMETHOD.

  METHOD c_split.

    SPLIT val AT sep INTO TABLE result.

  ENDMETHOD.

  METHOD c_join.

    LOOP AT tab INTO DATA(lv_line).
      IF sy-tabix > 1.
        result = result && sep.
      ENDIF.
      result = result && lv_line.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_check_table.

    result = xsdbool( rtti_get_type_kind( val ) = cl_abap_typedescr=>typekind_table ).

  ENDMETHOD.

  METHOD rtti_check_structure.

    TRY.
        DATA(lo_type) = cl_abap_typedescr=>describe_by_data( val ).
        result = xsdbool( lo_type->kind = cl_abap_typedescr=>kind_struct ).
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_check_numeric.

    DATA(lv_type_kind) = cl_abap_datadescr=>get_data_type_kind( val ).
    " numeric text (N) and the generic decfloat kind count as well
    result = xsdbool( lv_type_kind = cl_abap_typedescr=>typekind_decfloat
                      OR lv_type_kind = cl_abap_typedescr=>typekind_num
                      OR rtti_check_typekind_number( lv_type_kind ) = abap_true ).

  ENDMETHOD.

  METHOD rtti_check_typekind_number.

    CASE val.
      WHEN cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR cl_abap_typedescr=>typekind_packed
          OR cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34
          OR `8`.
        result = abap_true.
    ENDCASE.

  ENDMETHOD.

  METHOD time_add_seconds.

    result = cl_abap_tstmp=>add( tstmp = time
                                 secs  = seconds ).

  ENDMETHOD.

  METHOD time_get_stampl_by_date_time.

    DATA(ls_sy) = context_get_sy( ).
    CONVERT DATE date TIME time INTO TIME STAMP result TIME ZONE ls_sy-zonlo.

  ENDMETHOD.

  METHOD time_diff_seconds.

    DATA(lv_diff) = cl_abap_tstmp=>subtract( tstmp1 = time_to
                                              tstmp2 = time_from ).
    result = lv_diff.

  ENDMETHOD.

  METHOD conv_string_to_date.

    DATA lt_val_groups TYPE string_table.
    DATA lt_fmt_groups TYPE string_table.
    DATA lv_group      TYPE string.
    DATA lv_index      TYPE i.

    DATA(lv_val) = CONV string( val ).
    DATA(lv_fmt) = CONV string( format ).

    " the digits of the value, all of them and in their runs
    DATA(lv_clean) = ``.
    DATA(lv_i) = 0.
    WHILE lv_i < strlen( lv_val ).
      DATA(lv_c) = lv_val+lv_i(1).
      IF lv_c >= `0` AND lv_c <= `9`.
        lv_clean = lv_clean && lv_c.
        lv_group = lv_group && lv_c.
      ELSEIF lv_group IS NOT INITIAL.
        INSERT lv_group INTO TABLE lt_val_groups.
        CLEAR lv_group.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
    IF lv_group IS NOT INITIAL.
      INSERT lv_group INTO TABLE lt_val_groups.
      CLEAR lv_group.
    ENDIF.

    " the letters of the format, all of them and in runs of one letter
    DATA(lv_fmt_clean) = ``.
    lv_i = 0.
    WHILE lv_i < strlen( lv_fmt ).
      lv_c = lv_fmt+lv_i(1).
      IF lv_c = `Y` OR lv_c = `M` OR lv_c = `D`.
        lv_fmt_clean = lv_fmt_clean && lv_c.
        IF lv_group IS NOT INITIAL AND lv_group(1) <> lv_c.
          INSERT lv_group INTO TABLE lt_fmt_groups.
          CLEAR lv_group.
        ENDIF.
        lv_group = lv_group && lv_c.
      ELSEIF lv_group IS NOT INITIAL.
        INSERT lv_group INTO TABLE lt_fmt_groups.
        CLEAR lv_group.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
    IF lv_group IS NOT INITIAL.
      INSERT lv_group INTO TABLE lt_fmt_groups.
    ENDIF.

    DATA(lv_year)  = ``.
    DATA(lv_month) = ``.
    DATA(lv_day)   = ``.

    " a value written with separators is read by its parts, in the order
    " the format names them: `5.3.2024` is the 5th of March, and a time
    " behind the date (`5.3.2024 10:30`) is left out. Read digit by digit,
    " the first value came out as an initial date and the second as the
    " year 2410. A day or a month may have one digit, a year its full
    " length; a part longer than the format allows is no date
    IF lines( lt_val_groups ) > 1 AND lines( lt_val_groups ) >= lines( lt_fmt_groups ).
      LOOP AT lt_fmt_groups REFERENCE INTO DATA(lr_fmt).
        lv_index = lv_index + 1.
        lv_group = lt_val_groups[ lv_index ].
        DATA(lv_len) = strlen( lr_fmt->* ).
        IF strlen( lv_group ) > lv_len OR ( lr_fmt->*(1) = `Y` AND strlen( lv_group ) < lv_len ).
          RETURN.
        ENDIF.
        WHILE strlen( lv_group ) < lv_len.
          lv_group = `0` && lv_group.
        ENDWHILE.
        " a letter the format names twice (`DD.MM.YYYY HH:MM`) counts once
        CASE lr_fmt->*(1).
          WHEN `Y`.
            IF lv_year IS INITIAL.
              lv_year = lv_group.
            ENDIF.
          WHEN `M`.
            IF lv_month IS INITIAL.
              lv_month = lv_group.
            ENDIF.
          WHEN `D`.
            IF lv_day IS INITIAL.
              lv_day = lv_group.
            ENDIF.
        ENDCASE.
      ENDLOOP.
      result = lv_year && lv_month && lv_day.
      RETURN.
    ENDIF.

    " fewer digits than the format asks for: no date, instead of reading
    " behind the end of the value
    IF strlen( lv_clean ) < strlen( lv_fmt_clean ).
      RETURN.
    ENDIF.

    DATA(lv_pos) = 0.
    lv_i = 0.
    WHILE lv_i < strlen( lv_fmt_clean ).
      lv_c = lv_fmt_clean+lv_i(1).
      CASE lv_c.
        WHEN `Y`.
          lv_year = lv_year && lv_clean+lv_pos(1).
        WHEN `M`.
          lv_month = lv_month && lv_clean+lv_pos(1).
        WHEN `D`.
          lv_day = lv_day && lv_clean+lv_pos(1).
      ENDCASE.
      lv_pos = lv_pos + 1.
      lv_i = lv_i + 1.
    ENDWHILE.

    result = lv_year && lv_month && lv_day.

  ENDMETHOD.

  METHOD conv_date_to_string.

    DATA(lv_fmt) = CONV string( format ).
    DATA(lv_date) = CONV string( val ).

    DATA(lv_year)  = lv_date(4).
    DATA(lv_month) = lv_date+4(2).
    DATA(lv_day)   = lv_date+6(2).

    result = lv_fmt.
    result = replace( val = result sub = `YYYY` with = lv_year ).
    result = replace( val = result sub = `MM` with = lv_month ).
    result = replace( val = result sub = `DD` with = lv_day ).

  ENDMETHOD.

  METHOD ui5_msg_box_format.

    DATA(lt_msg) = msg_get_t( val ).

    " a structure that carries none of the message components maps to an
    " entry with no text at all - that is not a message, it is data, and it
    " used to reach the box as a popup with a blank line in it. Dropping it
    " here is what lets `skip` mean "nothing in here is a message" and lets
    " the caller fall back to ui5_data_box_format( )
    DELETE lt_msg WHERE text IS INITIAL.

    DATA(lv_lines) = lines( lt_msg ).

    IF lv_lines = 0.
      result-skip = abap_true.
      RETURN.
    ENDIF.

    " the box takes its type/title from the MOST SEVERE message, also when
    " several are collapsed into one box below: a warning followed by an
    " error is an error box (it took the first message's type until
    " 2026-10, which opened [W, E] as a warning). Error over warning over
    " success and information, which rank the same - the first of them wins
    DATA(lv_type) = ``.
    DATA(lv_rank) = -1.
    LOOP AT lt_msg REFERENCE INTO DATA(lr_type).
      DATA(lv_type_row) = ui5_get_msg_type( lr_type->type ).
      DATA(lv_rank_row) = COND i( WHEN lv_type_row = cs_ui5_msg_type-e THEN 2
                                  WHEN lv_type_row = cs_ui5_msg_type-w THEN 1
                                  ELSE 0 ).
      IF lv_rank_row > lv_rank.
        lv_rank = lv_rank_row.
        lv_type = lv_type_row.
      ENDIF.
    ENDLOOP.
    result-title = lv_type.
    result-type  = to_lower( lv_type ).

    IF lv_lines = 1.
      result-text = lt_msg[ 1 ]-text.
      RETURN.
    ENDIF.

    " several messages: a counting headline plus every text as a bullet. No
    " blank around it - the box shows the text as written, and the template
    " used to carry one on either side
    result-text = |{ lv_lines } Messages found:|.
    DATA lt_detail_items TYPE string_table.
    " the texts are data inside markup, escaped like every sibling renderer
    " escapes its values: unescaped, `Enter a value for <MATNR>` lost its
    " token to the frontend sanitizer, which drops an unknown element
    LOOP AT lt_msg REFERENCE INTO DATA(lr_msg).
      INSERT |<li>{ c_escape_html( lr_msg->text ) }</li>| INTO TABLE lt_detail_items.
    ENDLOOP.
    result-details = html_get_list( lt_detail_items ).

  ENDMETHOD.

  METHOD ui5_data_box_format.

    " a character value is its own text, whatever is in it - an empty one
    " included, which is the shape message_box_display( lv_text ) has always
    " had and is not this method's to change
    IF rtti_check_clike( val ) = abap_true.
      IF html_check( val ) = abap_true.
        " markup in the box text would be shown as the tags it is written
        " with - a message box renders its text as plain text and its
        " DETAILS as formatted text, so that is where HTML belongs. The
        " plain text behind it stays as the headline
        result-text    = html_get_plain( val ).
        result-details = val.
      ELSE.
        result-text = val.
      ENDIF.
      RETURN.
    ENDIF.

    " a number, a date, a hex value: shown the way the runtime writes it
    IF rtti_check_printable( val ) = abap_true.
      result-text = data_get_string( val ).
      RETURN.
    ENDIF.

    " complex data that is initial stays silent. A caller that hands over
    " the result table of a call it just made expects no box when the call
    " returned nothing
    IF val IS INITIAL.
      result-skip = abap_true.
      RETURN.
    ENDIF.

    result-text    = data_get_headline( val ).
    result-details = data_render( val   = val
                                  depth = 0 ).

    IF result-text IS INITIAL AND result-details IS INITIAL.
      result-skip = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD data_render.

    " the recursion stops here rather than in every branch below, and says
    " in the output that it did
    IF depth > cv_data_max_depth.
      result = `<em>...</em>`.
      RETURN.
    ENDIF.

    CASE rtti_get_type_kind( val ).
      WHEN cl_abap_datadescr=>typekind_table.
        result = data_render_tab( val   = val
                                  depth = depth ).
      WHEN cl_abap_datadescr=>typekind_struct1 OR cl_abap_datadescr=>typekind_struct2.
        result = data_render_struc( val   = val
                                    depth = depth ).
      WHEN cl_abap_datadescr=>typekind_oref.
        result = data_render_oref( val   = val
                                   depth = depth ).
      WHEN cl_abap_datadescr=>typekind_dref.
        result = data_render_dref( val   = val
                                   depth = depth ).
      WHEN OTHERS.
        result = c_escape_html( data_get_string( val ) ).
    ENDCASE.

  ENDMETHOD.

  METHOD data_render_tab.

    DATA lt_item TYPE string_table.
    DATA lv_no   TYPE i.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    " IS ASSIGNED, not sy-subrc - the unassign_data rule: on some releases a
    " successful ASSIGN leaves sy-subrc as it was, and the caller chain
    " (box_resolve -> ui5_msg_box_format's DELETE ... WHERE with no hit ->
    " here) arrives with a stale 4, which rendered "Table with N entries"
    " over an empty body. The symbol is declared fresh and assigned once
    ASSIGN val TO <tab>.
    IF <tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      lv_no = lv_no + 1.
      IF lv_no > cv_data_max_rows.
        INSERT |<li><em>... { lines( <tab> ) - cv_data_max_rows } more entries</em></li>|
               INTO TABLE lt_item.
        EXIT.
      ENDIF.
      INSERT |<li>{ data_render( val   = <row>
                                 depth = depth + 1 ) }</li>| INTO TABLE lt_item.
    ENDLOOP.

    result = html_get_list( items   = lt_item
                            ordered = abap_true ).

  ENDMETHOD.

  METHOD data_render_struc.

    DATA lt_item TYPE string_table.
    " declared, not CONV string( ) - see error_get_attributes
    DATA lv_name TYPE string.

    FIELD-SYMBOLS <comp> TYPE any.

    TRY.
        DATA(lt_attri) = rtti_get_t_attri_by_any( val ).
      CATCH cx_root.
        " a structure RTTI cannot describe still has a value - the caller
        " keeps the headline, the details stay empty
        RETURN.
    ENDTRY.

    LOOP AT lt_attri REFERENCE INTO DATA(lr_attri).
      lv_name = lr_attri->name.
      ASSIGN COMPONENT lv_name OF STRUCTURE val TO <comp>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      INSERT data_render_item( name  = lv_name
                               val   = <comp>
                               depth = depth ) INTO TABLE lt_item.
    ENDLOOP.

    result = html_get_list( lt_item ).

  ENDMETHOD.

  METHOD data_render_oref.

    DATA lo_obj  TYPE REF TO object.
    DATA lt_item TYPE string_table.
    " declared, not CONV string( ) - see error_get_attributes
    DATA lv_name TYPE string.

    FIELD-SYMBOLS <comp> TYPE any.

    TRY.
        lo_obj = val.
      CATCH cx_root.
        RETURN.
    ENDTRY.
    IF lo_obj IS NOT BOUND.
      RETURN.
    ENDIF.

    " an exception carries its own text, and that text is the whole story -
    " its attributes are the placeholders that are already substituted in it
    result = data_get_exc_text( lo_obj ).
    IF result IS NOT INITIAL.
      result = c_escape_html( result ).
      RETURN.
    ENDIF.

    TRY.
        DATA(lt_attri) = rtti_get_t_attri_by_oref( lo_obj ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    " the public state is what an object can show of itself; a constant is
    " the type's, not this instance's, and a class attribute is nobody's
    LOOP AT lt_attri REFERENCE INTO DATA(lr_attri)                  "#EC CI_SORTSEQ
         WHERE visibility  = cv_objectdescr_public
           AND is_constant = abap_false
           AND is_class    = abap_false.
      lv_name = lr_attri->name.
      " UNASSIGN + IS ASSIGNED, not sy-subrc - the loop-carried case, see
      " error_get_attributes for why the difference matters here
      UNASSIGN <comp>.
      ASSIGN lo_obj->(lv_name) TO <comp>.
      IF <comp> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      INSERT data_render_item( name  = lv_name
                               val   = <comp>
                               depth = depth ) INTO TABLE lt_item.
    ENDLOOP.

    result = html_get_list( lt_item ).

  ENDMETHOD.

  METHOD data_render_dref.

    DATA lr_data TYPE REF TO data.

    FIELD-SYMBOLS <val> TYPE any.

    TRY.
        lr_data = val.
      CATCH cx_root.
        RETURN.
    ENDTRY.
    IF lr_data IS NOT BOUND.
      RETURN.
    ENDIF.

    " IS ASSIGNED, not sy-subrc - see data_render_tab and unassign_data
    ASSIGN lr_data->* TO <val>.
    IF <val> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " a reference is no level of its own for the reader, but it is one for
    " the recursion: a structure that points at itself would otherwise
    " never reach the depth limit
    result = data_render( val   = <val>
                          depth = depth + 1 ).

  ENDMETHOD.

  METHOD data_render_item.

    DATA(lv_val) = data_render( val   = val
                                depth = depth + 1 ).

    result = |<li><strong>{ c_escape_html( name ) }</strong>: { lv_val }</li>|.

  ENDMETHOD.

  METHOD data_get_headline.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    CASE rtti_get_type_kind( val ).

      WHEN cl_abap_datadescr=>typekind_table.
        " guarded like data_render_tab, and for the same caller chain: a
        " table the ASSIGN cannot take leaves the symbol unassigned, and
        " lines( ) on it is a GETWA_NOT_ASSIGNED dump - the headline of a
        " message box is no reason to end a request. No headline then; the
        " box falls back the way it does for an undescribable structure
        ASSIGN val TO <tab>.
        IF <tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.
        DATA(lv_lines) = lines( <tab> ).
        IF lv_lines = 1.
          result = `Table with 1 entry`.
        ELSE.
          result = |Table with { lv_lines } entries|.
        ENDIF.

      WHEN cl_abap_datadescr=>typekind_struct1 OR cl_abap_datadescr=>typekind_struct2.
        TRY.
            DATA(lt_attri) = rtti_get_t_attri_by_any( val ).
            result = |Structure with { lines( lt_attri ) } fields|.
          CATCH cx_root.
            result = `Structure`.
        ENDTRY.

      WHEN cl_abap_datadescr=>typekind_oref.
        DATA lo_obj TYPE REF TO object.
        TRY.
            lo_obj = val.
            result = |Object { rtti_get_classname_by_ref( lo_obj ) }|.
          CATCH cx_root.
            result = `Object`.
        ENDTRY.

      WHEN OTHERS.
        result = `Data`.

    ENDCASE.

  ENDMETHOD.

  METHOD data_get_string.

    " the plain assignment is what turns an elementary value into its text
    " - the same trick get_comp_str( ) uses. A value the runtime refuses to
    " convert must not be the reason a box does not appear
    TRY.
        " a number through a string template: the assignment puts the sign
        " BEHIND it - -5 came out as `5-`, a packed -12.50 as `12.50-`
        IF rtti_check_typekind_number( rtti_get_type_kind( val ) ) = abap_true.
          result = |{ val }|.
        ELSE.
          result = val.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    " a number reaches a character target right-aligned in the length its
    " type needs (` 12` for an i on some runtimes), and a CHAR field brings
    " its padding along - neither is what the value says
    result = c_trim( result ).

  ENDMETHOD.

  METHOD data_get_exc_text.

    TRY.
        result = CAST cx_root( val )->get_text( ).
      CATCH cx_root ##NO_HANDLER.
        " not an exception, or one that cannot render itself
    ENDTRY.

  ENDMETHOD.

  METHOD html_get_list.

    IF items IS INITIAL.
      RETURN.
    ENDIF.

    IF ordered = abap_true.
      result = `<ol>` && concat_lines_of( items ) && `</ol>`.
    ELSE.
      result = `<ul>` && concat_lines_of( items ) && `</ul>`.
    ENDIF.

  ENDMETHOD.

  METHOD html_check.

    " a closing tag is the one shape a business text does not produce by
    " accident; the void elements are the ones that have none. CS ignores
    " case, so `</DIV>` and `<BR/>` are covered with it
    result = xsdbool( val CS `</`
                   OR val CS `<br`
                   OR val CS `<hr`
                   OR val CS `<img` ).

  ENDMETHOD.

  METHOD html_get_plain.

    " one pass over the text: every tag becomes a blank (so that
    " `<td>a</td><td>b</td>` does not read as `ab`), and an unclosed `<`
    " takes the rest of the text with it. The loop this used to be re-sliced
    " the remainder once per tag - quadratic over a long body, in the path
    " that renders an HTML message for the box
    result = val.
    result = replace( val   = result
                      regex = `<[^>]*>`
                      with  = ` `
                      occ   = 0 ) ##REGEX_POSIX.
    DATA(lv_open) = find( val = result
                          sub = `<` ).
    IF lv_open >= 0.
      result = substring( val = result
                          len = lv_open ) && ` `.
    ENDIF.

    " the entities the stripped text would otherwise show as written
    result = replace( val = result sub = `&nbsp;` with = ` ` occ = 0 ).
    result = replace( val = result sub = `&lt;` with = `<` occ = 0 ).
    result = replace( val = result sub = `&gt;` with = `>` occ = 0 ).
    result = replace( val = result sub = `&amp;` with = `&` occ = 0 ).
    result = condense( result ).

    IF strlen( result ) > cv_data_max_text.
      result = substring( val = result
                          len = cv_data_max_text ) && `...`.
    ENDIF.

  ENDMETHOD.

  METHOD rtti_check_table_standard.

    DATA lo_type TYPE REF TO cl_abap_typedescr.

    " REFERENCE_IS_INITIAL is a classic exception, the CATCH below cannot see it
    cl_abap_typedescr=>describe_by_data_ref( EXPORTING  p_data_ref           = val
                                             RECEIVING p_descr_ref           = lo_type
                                             EXCEPTIONS reference_is_initial = 1
                                                        OTHERS               = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    TRY.
        IF lo_type->kind <> cl_abap_typedescr=>kind_table.
          RETURN.
        ENDIF.
        result = xsdbool( CAST cl_abap_tabledescr( lo_type )->table_kind = cl_abap_tabledescr=>tablekind_std ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_flat_components.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    " REFERENCE_IS_INITIAL is a classic exception, the CATCH below cannot see it
    cl_abap_typedescr=>describe_by_data_ref( EXPORTING  p_data_ref           = val
                                             RECEIVING p_descr_ref           = lo_type
                                             EXCEPTIONS reference_is_initial = 1
                                                        OTHERS               = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    TRY.
        IF lo_type->kind = cl_abap_typedescr=>kind_table.
          DATA(lo_table) = CAST cl_abap_tabledescr( lo_type ).
          IF lo_table->table_kind <> cl_abap_tabledescr=>tablekind_std.
            RETURN.
          ENDIF.
          lo_type = lo_table->get_table_line_type( ).
        ENDIF.
        IF lo_type->kind <> cl_abap_typedescr=>kind_struct.
          RETURN.
        ENDIF.
        " the included view, not get_components( ): an include arrives
        " there as ONE component (as_include) that no cell can be read
        " by, and a row type with an include - an append, a reuse
        " structure, a DDIC table's .INCLUDE - is what many bound tables
        " are made of. The view lists every field under the name
        " ASSIGN COMPONENT takes, suffix applied and nested includes
        " resolved, and it is the very call ajson's convert_struc makes -
        " so the names are ajson's on every release, by construction. A
        " NESTED structure stays one component of a structure kind and
        " ends the fast path below, as before
        DATA(lt_comp) = CAST cl_abap_structdescr( lo_type )->get_included_view( ).
        LOOP AT lt_comp REFERENCE INTO DATA(lr_comp).
          DATA(ls_flat) = VALUE ty_s_flat_comp( name = lr_comp->name ).
          IF lr_comp->type->kind <> cl_abap_typedescr=>kind_elem.
            CLEAR result.
            RETURN.
          ENDIF.
          ls_flat-json_kind = rtti_get_json_kind( lr_comp->type ).
          IF ls_flat-json_kind IS INITIAL.
            CLEAR result.
            RETURN.
          ENDIF.
          INSERT ls_flat INTO TABLE result.
        ENDLOOP.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_json_kind.

    " the same distinctions ajson's lcl_abap_to_json=>convert_value
    " makes, in the same order - a boolean by its type name first, so an
    " abap_bool (a CHAR 1) is not a string
    CASE val->absolute_name.
      WHEN `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL` OR `\TYPE=ABAP_BOOLEAN` OR `\TYPE=XSDBOOLEAN`
          OR `\TYPE=FLAG` OR `\TYPE=XFELD`.
        result = `B`.
        RETURN.
      WHEN `\TYPE=TIMESTAMP`.
        result = `U`.
        RETURN.
      WHEN `\TYPE=TIMESTAMPL`.
        result = `L`.
        RETURN.
    ENDCASE.
    CASE val->type_kind.
      WHEN cl_abap_typedescr=>typekind_char OR cl_abap_typedescr=>typekind_num
          OR cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_hex
          OR cl_abap_typedescr=>typekind_xstring.
        result = `S`.
      WHEN cl_abap_typedescr=>typekind_date.
        result = `D`.
      WHEN cl_abap_typedescr=>typekind_time.
        result = `T`.
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34 OR `8`.
        " `8`: typekind_int8, a constant lower releases do not have
        result = `N`.
    ENDCASE.

  ENDMETHOD.

  METHOD rtti_check_namespaced.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    " REFERENCE_IS_INITIAL is a classic exception, the CATCH below cannot see it
    cl_abap_typedescr=>describe_by_data_ref( EXPORTING  p_data_ref           = val
                                             RECEIVING p_descr_ref           = lo_type
                                             EXCEPTIONS reference_is_initial = 1
                                                        OTHERS               = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    TRY.
        result = rtti_check_namespaced_type( lo_type ).
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_check_namespaced_type.

    " no type nests deeper than this without a reference, which is not
    " followed - the bound is a guard, not a limit an app meets
    IF depth > 32.
      RETURN.
    ENDIF.
    DATA(lo_type) = type.
    IF lo_type->kind = cl_abap_typedescr=>kind_table.
      lo_type = CAST cl_abap_tabledescr( lo_type )->get_table_line_type( ).
    ENDIF.
    IF lo_type->kind <> cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.
    " the included view, as rtti_get_flat_components reads it: the name of
    " an include's component is the one the structure addresses it by
    DATA(lt_comp) = CAST cl_abap_structdescr( lo_type )->get_included_view( ).
    LOOP AT lt_comp REFERENCE INTO DATA(lr_comp).
      IF strlen( lr_comp->name ) > 1 AND lr_comp->name(1) = `/`.
        result = abap_true.
        RETURN.
      ENDIF.
      IF ( lr_comp->type->kind = cl_abap_typedescr=>kind_struct
           OR lr_comp->type->kind = cl_abap_typedescr=>kind_table )
          AND rtti_check_namespaced_type( type  = lr_comp->type
                                          depth = depth + 1 ) = abap_true.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_check_serializable.

    IF val IS NOT BOUND.
      result = abap_true.
      RETURN.
    ENDIF.
    TRY.
        DATA(lo_dummy) = CAST if_serializable_object( val ) ##NEEDED.
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD app_get_url.

    DATA(lt_param) = url_param_get_tab( search ).
    DELETE lt_param WHERE n = `app_start`.
    INSERT VALUE #( n = `app_start`
                    v = to_lower( classname ) ) INTO TABLE lt_param.

    " keep only the launchpad shell part of the hash: the app-owned part
    " (leading `/` standalone, or everything after `&/` inside the FLP)
    " carries THIS app's route/app-state, which the backend prefers over
    " app_start - appending it verbatim would re-open the current app
    " instead of the requested one
    DATA(lv_hash) = CONV string( hash ).
    IF lv_hash IS NOT INITIAL.
      DATA(lv_content) = lv_hash.
      IF lv_content(1) = `#`.
        lv_content = substring( val = lv_content
                                off = 1 ).
      ENDIF.
      IF lv_content IS INITIAL OR lv_content(1) = `/`.
        " pure app hash (route or app-state) - drop it entirely
        lv_hash = ``.
      ELSE.
        " inside the FLP keep the shell part, cut the app part after `&/`
        DATA(lv_off) = find( val = lv_content
                             sub = `&/` ).
        IF lv_off = 0.
          lv_hash = ``.
        ELSEIF lv_off > 0.
          lv_hash = |#{ lv_content(lv_off) }|.
        ELSE.
          lv_hash = |#{ lv_content }|.
        ENDIF.
      ENDIF.
    ENDIF.

    result = |{ origin }{ pathname }?| && url_param_create_url( lt_param ) && lv_hash.

  ENDMETHOD.

  METHOD app_get_url_source_code.

    result = |{ origin }/sap/bc/adt/oo/classes/{ classname }/source/main|.

  ENDMETHOD.

  " ========== String Extras ==========

  METHOD c_pad_left.
    result = val.
    DATA(lv_missing) = len - strlen( result ).
    IF lv_missing <= 0.
      RETURN.
    ENDIF.
    " pad is TYPE c - space value would be trimmed by && (same as c_pad_right fix)
    DATA(lv_pad) = COND string( WHEN pad IS INITIAL THEN ` ` ELSE CONV #( pad ) ).
    " exactly the missing characters, also for a pad of several characters
    result = substring( val = repeat( val = lv_pad
                                      occ = lv_missing )
                        len = lv_missing ) && result.
  ENDMETHOD.

  METHOD c_pad_right.
    result = val.
    DATA(lv_missing) = len - strlen( result ).
    IF lv_missing <= 0.
      RETURN.
    ENDIF.
    " pad is TYPE c - a space value IS INITIAL in ABAP and CONV string trims it.
    " Preserve the space explicitly via COND.
    DATA(lv_pad) = COND string( WHEN pad IS INITIAL THEN ` ` ELSE CONV #( pad ) ).
    " exactly the missing characters, also for a pad of several characters
    result = result && substring( val = repeat( val = lv_pad
                                                occ = lv_missing )
                                  len = lv_missing ).
  ENDMETHOD.

  METHOD c_truncate.

    result = val.
    IF strlen( result ) <= max.
      RETURN.
    ENDIF.
    DATA(lv_ellipsis_len) = strlen( ellipsis ).
    IF max > lv_ellipsis_len.
      result = substring( val = result
                          len = max - lv_ellipsis_len ) && ellipsis.
    ELSEIF max > 0.
      result = substring( val = result
                          len = max ).
    ELSE.
      CLEAR result.
    ENDIF.

  ENDMETHOD.

  METHOD c_substring_safe.

    DATA(lv_val) = CONV string( val ).
    DATA(lv_strlen) = strlen( lv_val ).

    DATA(lv_off) = off.
    IF lv_off < 0.
      lv_off = 0.
    ENDIF.
    IF lv_off >= lv_strlen.
      result = ``.
      RETURN.
    ENDIF.

    DATA(lv_len) = len.
    IF lv_len < 0 OR lv_off + lv_len > lv_strlen.
      lv_len = lv_strlen - lv_off.
    ENDIF.

    result = substring( val = lv_val off = lv_off len = lv_len ).

  ENDMETHOD.

  METHOD c_replace_all.

    result = val.
    result = replace( val = result sub = sub with = new_val occ = 0 ).

  ENDMETHOD.

  METHOD c_is_blank.

    DATA(lv_val) = CONV string( val ).
    result = xsdbool( c_trim( lv_val ) IS INITIAL ).

  ENDMETHOD.

  " ========== Number Formatting ==========

  METHOD conv_number_to_string.

    DATA lv_str TYPE string.

    IF decimals >= 0.
      " rounded to the decimals asked for - cutting the digits off turned
      " 1.999 into 1.99
      DATA(lv_fmt) = |{ num_round( val      = CONV decfloat34( val )
                                   decimals = decimals ) }|.
      DATA(lv_dot_pos) = find( val = lv_fmt sub = `.` ).
      IF lv_dot_pos < 0.
        lv_str = lv_fmt.
        IF decimals > 0.
          lv_str = lv_str && `.` && repeat( val = `0` occ = decimals ).
        ENDIF.
      ELSE.
        DATA(lv_int_part) = lv_fmt(lv_dot_pos).
        DATA(lv_dec_part) = substring( val = lv_fmt off = lv_dot_pos + 1 ).
        IF strlen( lv_dec_part ) > decimals.
          lv_dec_part = lv_dec_part(decimals).
        ELSEIF strlen( lv_dec_part ) < decimals.
          lv_dec_part = lv_dec_part && repeat( val = `0` occ = decimals - strlen( lv_dec_part ) ).
        ENDIF.
        lv_str = COND #( WHEN decimals > 0 THEN lv_int_part && `.` && lv_dec_part
                                           ELSE lv_int_part ).
      ENDIF.
    ELSE.
      lv_str = |{ val }|.
    ENDIF.

    IF sep_thousands IS NOT INITIAL.
      DATA(lv_dot) = find( val = lv_str sub = `.` ).
      DATA(lv_integer) = COND string( WHEN lv_dot >= 0 THEN lv_str(lv_dot)
                                                       ELSE lv_str ).
      DATA(lv_decimal) = COND string( WHEN lv_dot >= 0 THEN substring( val = lv_str off = lv_dot ) ).
      DATA(lv_negative) = ``.
      IF strlen( lv_integer ) > 0 AND lv_integer(1) = `-`.
        lv_negative = `-`.
        lv_integer = substring( val = lv_integer off = 1 ).
      ENDIF.
      DATA(lv_result) = ``.
      DATA(lv_count) = 0.
      DATA(lv_i) = strlen( lv_integer ) - 1.
      WHILE lv_i >= 0.
        IF lv_count > 0 AND lv_count MOD 3 = 0.
          lv_result = sep_thousands && lv_result.
        ENDIF.
        lv_result = lv_integer+lv_i(1) && lv_result.
        lv_count = lv_count + 1.
        lv_i = lv_i - 1.
      ENDWHILE.
      lv_str = lv_negative && lv_result && lv_decimal.
    ENDIF.

    result = lv_str.

  ENDMETHOD.

  METHOD conv_string_to_number.

    DATA(lv_val) = c_trim( CONV string( val ) ).

    " Which separator is the decimal one:
    " - both a dot and a comma occur: the one that occurs LAST
    "   (`1.234,56` and `1,234.56` are both 1234.56)
    " - only one kind, more than once: thousands separators, no decimals
    "   (`1.234.567`, `1,000,000`)
    " - only one kind, once: the decimal separator (`3,14`, `3.14`). So
    "   `1,000` is 1 (European convention) - a single separator cannot be
    "   told apart from a thousands separator, the decimal reading wins
    " Every other separator is a thousands separator and is skipped.
    DATA lv_dot_count   TYPE i.
    DATA lv_comma_count TYPE i.
    DATA lv_decimal     TYPE string.
    DATA(lv_i) = 0.
    WHILE lv_i < strlen( lv_val ).
      CASE lv_val+lv_i(1).
        WHEN `.`.
          lv_dot_count = lv_dot_count + 1.
          lv_decimal = `.`.
        WHEN `,`.
          lv_comma_count = lv_comma_count + 1.
          lv_decimal = `,`.
      ENDCASE.
      lv_i = lv_i + 1.
    ENDWHILE.
    " lv_decimal holds the last separator - right when both kinds occur
    IF lv_dot_count = 0 OR lv_comma_count = 0.
      IF lv_dot_count > 1 OR lv_comma_count > 1.
        CLEAR lv_decimal.
      ENDIF.
    ENDIF.

    " Normalize: keep only digits, minus, and the decimal separator as `.`
    DATA(lv_clean) = ``.
    lv_i = 0.
    WHILE lv_i < strlen( lv_val ).
      DATA(lv_c) = lv_val+lv_i(1).
      IF lv_c >= `0` AND lv_c <= `9`.
        lv_clean = lv_clean && lv_c.
      ELSEIF lv_c = `-` AND lv_i = 0.
        lv_clean = lv_clean && lv_c.
      ELSEIF lv_c = `-` AND lv_i = strlen( lv_val ) - 1 AND lv_val(1) <> `-`.
        " the sign behind the number, as a MOVE and WRITE put it
        lv_clean = `-` && lv_clean.
      ELSEIF lv_c = lv_decimal.
        lv_clean = lv_clean && `.`.
      ENDIF.
      " any other separator is a thousands separator - skipped
      lv_i = lv_i + 1.
    ENDWHILE.

    " Validate the normalized value explicitly: it needs at least one
    " digit and at most one decimal point (`1,234.5.6` keeps two). Return 0
    " for invalid input - the JS transpiler converts such strings leniently
    " instead of raising
    DATA(lv_point_count) = count( val = lv_clean
                                  sub = `.` ).
    IF lv_point_count > 1 OR lv_clean NA `0123456789`.
      result = 0.
      RETURN.
    ENDIF.

    TRY.
        result = lv_clean.
      CATCH cx_root.
        result = 0.
    ENDTRY.

  ENDMETHOD.

  " ========== i18n / Text Resolution ==========

  " ========== Itab Extras ==========

  METHOD itab_sort_by.

    " SORT itab BY (dynamic) is not supported by the JS transpiler used
    " for the Node unit tests - extract the sort key per row into a
    " helper table, sort that statically and rebuild the table
    TYPES: BEGIN OF ty_s_sort_key,
             key_str TYPE string,
             key_num TYPE decfloat34,
             idx     TYPE i,
           END OF ty_s_sort_key.
    DATA lt_key TYPE STANDARD TABLE OF ty_s_sort_key WITH EMPTY KEY.
    DATA lv_numeric TYPE abap_bool.

    DATA(lv_field) = to_upper( fieldname ).

    LOOP AT tab ASSIGNING FIELD-SYMBOL(<row>).
      DATA(lv_tabix) = sy-tabix.
      ASSIGN COMPONENT lv_field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      IF lv_tabix = 1.
        lv_numeric = rtti_check_numeric( <val> ).
      ENDIF.
      INSERT INITIAL LINE INTO TABLE lt_key ASSIGNING FIELD-SYMBOL(<key>).
      IF lv_numeric = abap_true.
        <key>-key_num = <val>.
      ELSE.
        <key>-key_str = <val>.
      ENDIF.
      <key>-idx = lv_tabix.
    ENDLOOP.

    " the row index as the last key, in both directions: rows with equal
    " keys keep their order (a stable sort) - without it they came back in
    " any order, jumped on every re-sort and a sort by a second column did
    " not keep the order of the first
    IF descending = abap_true.
      SORT lt_key BY key_num DESCENDING key_str DESCENDING idx ASCENDING.
    ELSE.
      SORT lt_key BY key_num ASCENDING key_str ASCENDING idx ASCENDING.
    ENDIF.

    DATA lr_copy TYPE REF TO data.
    CREATE DATA lr_copy LIKE tab.
    FIELD-SYMBOLS <tab_copy> TYPE STANDARD TABLE.
    ASSIGN lr_copy->* TO <tab_copy>.
    <tab_copy> = tab.

    CLEAR tab.
    LOOP AT lt_key ASSIGNING <key>.
      ASSIGN <tab_copy>[ <key>-idx ] TO FIELD-SYMBOL(<src>).
      IF sy-subrc = 0.
        INSERT <src> INTO TABLE tab.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_slice.

    " Create a copy of the source table (same type) and return the slice.
    DATA(lo_tabledescr) = CAST cl_abap_tabledescr(
      cl_abap_typedescr=>describe_by_data( tab ) ).
    CREATE DATA result TYPE HANDLE lo_tabledescr.

    FIELD-SYMBOLS <result> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row>    TYPE any.
    ASSIGN result->* TO <result>.

    DATA(lv_lines) = lines( tab ).
    DATA(lv_to) = COND i( WHEN to <= 0 OR to > lv_lines THEN lv_lines ELSE to ).
    DATA(lv_from) = COND i( WHEN from < 1 THEN 1 ELSE from ).

    LOOP AT tab ASSIGNING <row> FROM lv_from TO lv_to.
      INSERT <row> INTO TABLE <result>.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_paginate.

    total_count = lines( tab ).
    total_pages = COND #( WHEN page_size <= 0 THEN 1
                          ELSE ( total_count + page_size - 1 ) DIV page_size ).

    DATA(lv_from) = ( page - 1 ) * page_size + 1.
    DATA(lv_to)   = page * page_size.

    result = itab_slice( tab  = tab
                         from = lv_from
                         to   = lv_to ).

  ENDMETHOD.

  METHOD itab_to_json.

    result = json_stringify( val ).

  ENDMETHOD.

  METHOD itab_from_json.

    json_parse( EXPORTING val = val
                CHANGING data = data ).

  ENDMETHOD.

  METHOD itab_count_by.

    TYPES:
      BEGIN OF ty_s_count,
        n     TYPE string,
        count TYPE i,
      END OF ty_s_count.

    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <count> TYPE ty_s_count.
    " counted in a hashed table as integers - a key read on result scanned
    " every value for every row, and converted the count from text and back
    DATA lt_count TYPE HASHED TABLE OF ty_s_count WITH UNIQUE KEY n.
    " the values in the order they first came, which is the order of result
    DATA lt_order TYPE string_table.

    LOOP AT tab ASSIGNING <row>.
      ASSIGN COMPONENT fieldname OF STRUCTURE <row> TO <field>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DATA(lv_val) = |{ <field> }|.
      ASSIGN lt_count[ n = lv_val ] TO <count>.
      IF sy-subrc = 0.
        <count>-count = <count>-count + 1.
      ELSE.
        INSERT VALUE #( n     = lv_val
                        count = 1 ) INTO TABLE lt_count.
        INSERT lv_val INTO TABLE lt_order.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_order REFERENCE INTO DATA(lr_val).
      INSERT VALUE #( n = lr_val->*
                      v = |{ lt_count[ n = lr_val->* ]-count }| ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  " ========== Validation Helpers ==========

  METHOD check_is_email.
    DATA(lv_val) = c_trim( CONV string( val ) ).

    IF lv_val IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    " Basic email validation: no blank, exactly one @, text before and after,
    " a dot inside the domain - not at its start or end, and not doubled
    IF lv_val CA | \t|.
      RETURN.
    ENDIF.

    SPLIT lv_val AT `@` INTO DATA(lv_local) DATA(lv_domain) DATA(lv_extra).
    IF lv_extra IS NOT INITIAL OR lv_local IS INITIAL OR lv_domain IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    IF lv_domain NA `.` OR lv_domain CS `..` OR lv_local CS `..`.
      result = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_last) = strlen( lv_domain ) - 1.
    IF lv_domain(1) = `.` OR lv_domain+lv_last(1) = `.`.
      result = abap_false.
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.

  METHOD check_is_numeric_string.
    DATA(lv_val) = c_trim( CONV string( val ) ).
    DATA(lv_len) = strlen( lv_val ).

    IF lv_val IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_i) = 0.
    DATA(lv_has_dot) = abap_false.
    DATA(lv_has_digit) = abap_false.
    DATA(lv_has_sign) = abap_false.
    WHILE lv_i < lv_len.
      DATA(lv_c) = lv_val+lv_i(1).
      IF lv_c >= `0` AND lv_c <= `9`.
        lv_has_digit = abap_true.
      ELSEIF ( lv_c = `-` OR lv_c = `+` ) AND lv_i = 0.
        lv_has_sign = abap_true.
      ELSEIF lv_c = `-` AND lv_i = lv_len - 1 AND lv_has_sign = abap_false.
        " the sign behind the number, as a MOVE and WRITE put it
      ELSEIF ( lv_c = `.` OR lv_c = `,` ) AND lv_has_dot = abap_false.
        lv_has_dot = abap_true.
      ELSE.
        result = abap_false.
        RETURN.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

    " a sign or a separator alone is no number
    result = lv_has_digit.
  ENDMETHOD.

  METHOD check_int_digits.

    " condensed first: `1 000` would pass a check that allows blanks and
    " then dump in a conversion, and a blanks-only value is no number
    DATA(lv_val) = condense( val ).
    result = xsdbool( lv_val IS NOT INITIAL AND lv_val CO `0123456789` AND strlen( lv_val ) <= 9 ).

  ENDMETHOD.

  METHOD check_is_date_valid.
    DATA lv_date  TYPE d.
    DATA lv_str   TYPE string.
    DATA lv_year  TYPE i.
    DATA lv_month TYPE i.
    DATA lv_day   TYPE i.
    DATA lv_max   TYPE i.

    TRY.
        lv_date = conv_string_to_date( val    = val
                                       format = format ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    " a date field takes any eight characters - `20240230` is as welcome as
    " `20240229`, nothing checks the calendar on assignment. So the month
    " and the day are checked here
    lv_str = lv_date.
    IF strlen( lv_str ) <> 8 OR lv_str CN `0123456789`.
      RETURN.
    ENDIF.
    lv_year  = lv_str(4).
    lv_month = lv_str+4(2).
    lv_day   = lv_str+6(2).
    IF lv_year < 1 OR lv_month < 1 OR lv_month > 12 OR lv_day < 1.
      RETURN.
    ENDIF.

    CASE lv_month.
      WHEN 4 OR 6 OR 9 OR 11.
        lv_max = 30.
      WHEN 2.
        IF ( lv_year MOD 4 = 0 AND lv_year MOD 100 <> 0 ) OR lv_year MOD 400 = 0.
          lv_max = 29.
        ELSE.
          lv_max = 28.
        ENDIF.
      WHEN OTHERS.
        lv_max = 31.
    ENDCASE.

    result = xsdbool( lv_day <= lv_max ).
  ENDMETHOD.

  METHOD check_is_guid.

    DATA(lv_val) = c_trim( CONV string( val ) ).
    DATA(lv_clean) = c_replace_all( val = lv_val sub = `-` new_val = `` ).
    DATA(lv_len) = strlen( lv_clean ).

    " Accept 32 chars (raw) or 36 chars (with dashes)
    IF lv_len <> 32.
      result = abap_false.
      RETURN.
    ENDIF.

    " All characters must be hex digits
    DATA(lv_upper) = to_upper( lv_clean ).
    DATA(lv_i) = 0.
    WHILE lv_i < 32.
      DATA(lv_c) = lv_upper+lv_i(1).
      IF NOT ( ( lv_c >= `0` AND lv_c <= `9` ) OR ( lv_c >= `A` AND lv_c <= `F` ) ).
        result = abap_false.
        RETURN.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

    result = abap_true.

  ENDMETHOD.

  METHOD check_max_length.

    DATA(lv_val) = CONV string( val ).
    result = xsdbool( strlen( lv_val ) <= max ).

  ENDMETHOD.

  " ========== Deep Comparison ==========

  METHOD data_equals.

    TRY.
        result = xsdbool( a = b ).
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD data_diff.

    FIELD-SYMBOLS <old_field> TYPE any.
    FIELD-SYMBOLS <new_field> TYPE any.

    DATA(lt_comps) = rtti_get_t_attri_by_any( old ).

    LOOP AT lt_comps REFERENCE INTO DATA(lr_comp).
      ASSIGN COMPONENT lr_comp->name OF STRUCTURE old TO <old_field>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lr_comp->name OF STRUCTURE new TO <new_field>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <old_field> <> <new_field>.
        INSERT VALUE #( fieldname = lr_comp->name
                        old_value = |{ <old_field> }|
                        new_value = |{ <new_field> }| ) INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  " ========== Stopwatch ==========

  METHOD time_measure_start.

    GET TIME STAMP FIELD result.

  ENDMETHOD.

  METHOD time_measure_stop.

    " cl_abap_tstmp=>subtract gave whole seconds on the transpiled runtime,
    " so every measurement below a second came back as 0
    result = time_diff_milliseconds( time_from = start_time
                                     time_to   = time_get_timestampl( ) ).

  ENDMETHOD.

  " ========== Authorization Check ==========

  " ========== Enum/Domain Helpers ==========

  METHOD enum_to_text.

    DATA(lt_all) = enum_get_all( domain = domain langu = langu ).
    DATA(lv_val) = CONV string( value ).
    result = VALUE #( lt_all[ n = lv_val ]-v OPTIONAL ).

  ENDMETHOD.

  METHOD enum_get_all.

    TRY.
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = CONV string( domain )
                                             RECEIVING  p_descr_ref    = DATA(lo_type)
                                             EXCEPTIONS type_not_found = 1
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DATA(lo_elem) = CAST cl_abap_elemdescr( lo_type ).
        DATA(lt_fix) = rtti_get_t_fixvalues( elemdescr = lo_elem
                                              langu     = langu ).

        LOOP AT lt_fix INTO DATA(ls_fix).
          INSERT VALUE #( n = ls_fix-low
                          v = ls_fix-descr ) INTO TABLE result.
        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  " ========== Deep Field Access ==========

  METHOD data_get_by_path.

    FIELD-SYMBOLS <current> TYPE any.
    FIELD-SYMBOLS <field>   TYPE any.

    ASSIGN data TO <current>.

    SPLIT path AT `-` INTO TABLE DATA(lt_segments).

    LOOP AT lt_segments INTO DATA(lv_segment).
      DATA(lv_seg) = c_trim_upper( lv_segment ).
      IF lv_seg IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_seg OF STRUCTURE <current> TO <field>.
      IF sy-subrc <> 0.
        result = ``.
        RETURN.
      ENDIF.
      ASSIGN <field> TO <current>.
    ENDLOOP.

    result = |{ <current> }|.

  ENDMETHOD.

  METHOD data_set_by_path.

    FIELD-SYMBOLS <current> TYPE any.
    FIELD-SYMBOLS <field>   TYPE any.

    ASSIGN data TO <current>.

    SPLIT path AT `-` INTO TABLE DATA(lt_segments).
    DATA(lv_last_idx) = lines( lt_segments ).

    LOOP AT lt_segments INTO DATA(lv_segment).
      DATA(lv_seg) = c_trim_upper( lv_segment ).
      IF lv_seg IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_seg OF STRUCTURE <current> TO <field>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      IF sy-tabix = lv_last_idx.
        <field> = value.
      ELSE.
        ASSIGN <field> TO <current>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  " ========== BAL Extensions ==========

  " ========== Transport Extensions ==========

  " ========== Lock Extensions ==========

  " ========== Number Range ==========

  " ========== Change Documents ==========

  " ========== Background Job ==========

  " ========== Email ==========

  METHOD context_get_user_tech.

    TRY.

        DATA(lv_result) = VALUE string( ).
        DATA(lv_class) = `CL_ABAP_CONTEXT_INFO`.

        IF check_abap_cloud( ).
          CALL METHOD (lv_class)=>(`GET_USER_TECHNICAL_NAME`)
            RECEIVING
              rv_technical_name = lv_result.
        ELSE.
          " the technical name, as the cloud branch answers it - this branch
          " asked for the business partner id, which is another value and
          " needs a class that older releases do not have
          lv_result = sy-uname.
        ENDIF.

        result = lv_result.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD check_abap_cloud.

    IF gv_check_cloud_cached = abap_true.
      result = gv_check_cloud.
      RETURN.
    ENDIF.

    TRY.
        cl_abap_typedescr=>describe_by_name( `T100` ).
        gv_check_cloud = abap_false.
      CATCH cx_root.
        gv_check_cloud = abap_true.
    ENDTRY.
    gv_check_cloud_cached = abap_true.
    result = gv_check_cloud.

  ENDMETHOD.

  METHOD sy_get.

    result-datum = sy-datum.
    result-uzeit = sy-uzeit.
    result-uname = sy-uname.
    result-sysid = sy-sysid.
    result-saprl = sy-saprl.

  ENDMETHOD.

  METHOD rtti_get_t_fixvalues.

    TYPES:
      BEGIN OF fixvalue,
        low        TYPE c LENGTH 10,
        high       TYPE c LENGTH 10,
        option     TYPE c LENGTH 2,
        ddlanguage TYPE c LENGTH 1,
        ddtext     TYPE c LENGTH 60,
      END OF fixvalue.
    TYPES fixvalues TYPE STANDARD TABLE OF fixvalue WITH DEFAULT KEY.
    DATA lt_values TYPE fixvalues.

    DATA lv_langu  TYPE c LENGTH 1.
    DATA temp1     LIKE LINE OF lt_values.
    DATA lr_fix    LIKE REF TO temp1.
    DATA temp2     TYPE ty_s_fix_val.

    lv_langu = ` `.
    lv_langu = langu.

    CALL METHOD elemdescr->(`GET_DDIC_FIXED_VALUES`)
      EXPORTING
        p_langu        = lv_langu
      RECEIVING
        p_fixed_values = lt_values
      EXCEPTIONS
        not_found      = 1
        no_ddic_type   = 2
        OTHERS         = 3.

    LOOP AT lt_values REFERENCE INTO lr_fix.

      CLEAR temp2.
      temp2-low   = lr_fix->low.
      temp2-high  = lr_fix->high.
      temp2-descr = lr_fix->ddtext.
      INSERT temp2
             INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.

  METHOD conv_decode_x_base64.
    DATA lv_web_http_name TYPE c LENGTH 19.
    DATA classname        TYPE c LENGTH 15.
    DATA lx_first         TYPE REF TO cx_root.

    TRY.

        lv_web_http_name = `CL_WEB_HTTP_UTILITY`.
        CALL METHOD (lv_web_http_name)=>(`DECODE_X_BASE64`)
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " the cause is chained below if the classic API fails too - the
        " two-TRY shape of conv_get_string_by_xstring, reasoning there
    ENDTRY.

    TRY.

        classname = `CL_HTTP_UTILITY`.
        CALL METHOD (classname)=>(`DECODE_X_BASE64`)
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `UNSUPPORTED_BASE64_API - neither CL_WEB_HTTP_UTILITY nor CL_HTTP_UTILITY could decode here`
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_encode_x_base64.
    DATA lv_web_http_name TYPE c LENGTH 19.
    DATA classname        TYPE c LENGTH 15.
    DATA lx_first         TYPE REF TO cx_root.

    TRY.

        lv_web_http_name = `CL_WEB_HTTP_UTILITY`.
        CALL METHOD (lv_web_http_name)=>(`ENCODE_X_BASE64`)
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " see conv_decode_x_base64
    ENDTRY.

    TRY.

        classname = `CL_HTTP_UTILITY`.
        CALL METHOD (classname)=>(`ENCODE_X_BASE64`)
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `UNSUPPORTED_BASE64_API - neither CL_WEB_HTTP_UTILITY nor CL_HTTP_UTILITY could encode here`
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

  METHOD conv_get_string_by_xstring.

    DATA lv_invalid TYPE abap_bool.

    " UTF-8 through the code-page method: one implementation of the two
    " conversion APIs and their fallback, not two
    conv_get_string_by_xstring_cp( EXPORTING val      = val
                                             codepage = `UTF-8`
                                             encoding = `UTF-8`
                                   IMPORTING result   = result
                                             invalid  = lv_invalid ).
    IF lv_invalid = abap_true.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `INVALID_UTF8 - the bytes are no valid UTF-8`.
    ENDIF.

  ENDMETHOD.

  METHOD conv_get_bintab_by_xstring.

    DATA lr_line   TYPE REF TO data.
    DATA lv_len    TYPE i.
    DATA lv_total  TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_chunk  TYPE i.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <field> TYPE any.

    CLEAR result.

    CREATE DATA lr_line LIKE LINE OF result.
    ASSIGN lr_line->* TO <line>.

    " BAPICONTEN-LINE and the like: the bytes go into the first component
    ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field>.
    IF sy-subrc <> 0.
      ASSIGN <line> TO <field>.
    ENDIF.

    " the byte length - DESCRIBE FIELD is no statement of ABAP Cloud
    lv_len = cl_abap_typedescr=>describe_by_data( <field> )->length.
    IF lv_len <= 0.
      RETURN.
    ENDIF.

    lv_total = xstrlen( val ).
    WHILE lv_offset < lv_total.
      lv_chunk = nmin( val1 = lv_len
                       val2 = lv_total - lv_offset ).
      CLEAR <line>.
      <field> = val+lv_offset(lv_chunk).
      INSERT <line> INTO TABLE result.
      lv_offset = lv_offset + lv_chunk.
    ENDWHILE.

  ENDMETHOD.

  METHOD conv_get_xstring_by_string.

    DATA conv           TYPE REF TO object.
    DATA conv_codepage  TYPE c LENGTH 21.
    DATA conv_out_class TYPE c LENGTH 19.
    DATA lx_first       TYPE REF TO cx_root.

    TRY.

        conv_codepage = `CL_ABAP_CONV_CODEPAGE`.
        CALL METHOD (conv_codepage)=>create_out
          RECEIVING
            instance = conv.

        CALL METHOD conv->(`IF_ABAP_CONV_OUT~CONVERT`)
          EXPORTING
            source = val
          RECEIVING
            result = result.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " deliberate, same as conv_get_string_by_xstring: the capture is the
        " handling, the fall-through is the point
    ENDTRY.

    " Guarded for the same reason as its counterpart above: an unguarded
    " fallback let a raw dynamic-call exception out of a utility method.
    TRY.

        conv_out_class = `CL_ABAP_CONV_OUT_CE`.
        CALL METHOD (conv_out_class)=>create
          EXPORTING
            encoding = `UTF-8`
          RECEIVING
            conv     = conv.

        CALL METHOD conv->(`CONVERT`)
          EXPORTING
            data   = val
          IMPORTING
            buffer = result.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |UNSUPPORTED_CODEPAGE_API - neither CL_ABAP_CONV_CODEPAGE | &&
                       |nor CL_ABAP_CONV_OUT_CE could convert here|
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_classes_impl_intf.

    IF check_abap_cloud( ).
      result = rtti_get_classes_intf_cloud( val              = val
                                            read_description = read_description ).
    ELSE.
      result = rtti_get_classes_intf_std( val              = val
                                          read_description = read_description ).
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_classes_intf_cloud.

    DATA obj TYPE REF TO object.
    FIELD-SYMBOLS <any> TYPE any.
    DATA lt_implementation_names TYPE string_table.
    DATA BEGIN OF ls_clskey.
    DATA clsname TYPE c LENGTH 30.
    DATA END OF ls_clskey.
    DATA xco_cp_abap         TYPE c LENGTH 11.
    DATA implementation_name LIKE LINE OF lt_implementation_names.
    DATA ls_class            LIKE LINE OF result.

    ls_clskey-clsname = val.

    xco_cp_abap = `XCO_CP_ABAP`.
    CALL METHOD (xco_cp_abap)=>interface
      EXPORTING
        iv_name      = ls_clskey-clsname
      RECEIVING
        ro_interface = obj.

    " an XCO object without the attribute knows no implementations: the
    " answer is the empty list the on-prem twin gives for an interface
    " nobody implements - not a kernel exception raised by hand
    " UNASSIGN + IS ASSIGNED, not sy-subrc - see unassign_data (#1937). The
    " SECOND hop is the one that bites: the symbol still holds the first
    " attribute when its ASSIGN fails, so a stale 0 would hand the WRONG
    " object to the GET_NAMES call below - and the caller reads the empty or
    " failed answer as "no user exit configured anywhere", the #2564 damage
    " picture, with nothing to point at
    UNASSIGN <any>.
    ASSIGN obj->(`IF_XCO_AO_INTERFACE~IMPLEMENTATIONS`) TO <any>.
    IF <any> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    obj = <any>.

    UNASSIGN <any>.
    ASSIGN obj->(`IF_XCO_INTF_IMPLEMENTATIONS_FC~ALL`) TO <any>.
    IF <any> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    obj = <any>.

    CALL METHOD obj->(`IF_XCO_INTF_IMPLEMENTATIONS~GET_NAMES`)
      RECEIVING
        rt_names = lt_implementation_names.

    LOOP AT lt_implementation_names INTO implementation_name.

      CLEAR ls_class.
      ls_class-classname = implementation_name.
      IF read_description = abap_true.
        " a class the XCO layer cannot describe (deleted mid-list, locked)
        " keeps its slot with a blank description - one broken implementer
        " must not hide every other one from the caller
        TRY.
            ls_class-description = rtti_get_class_descr_on_cloud( implementation_name ).
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
      INSERT ls_class INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_get_classes_intf_std.

    TYPES BEGIN OF ty_s_impl.
    TYPES clsname    TYPE c LENGTH 30.
    TYPES refclsname TYPE c LENGTH 30.
    TYPES END OF ty_s_impl.
    " DEFAULT KEY on purpose: this table is passed to the classic function
    " module SEO_INTERFACE_IMPLEM_GET_ALL (impkeys), whose formal parameter is
    " a STANDARD TABLE WITH DEFAULT KEY. WITH EMPTY KEY makes the table type
    " incompatible, so the CALL FUNCTION fails and no implementers are returned
    " (silently breaking user-exit discovery). Never change this key type.
    DATA lt_impl TYPE STANDARD TABLE OF ty_s_impl WITH DEFAULT KEY.
    "#EC DEFAULT_KEY
    TYPES BEGIN OF ty_s_key.
    TYPES intkey TYPE c LENGTH 30.
    TYPES END OF ty_s_key.
    DATA ls_key TYPE ty_s_key.
    DATA BEGIN OF ls_clskey.
    DATA clsname TYPE c LENGTH 30.
    DATA END OF ls_clskey.
    DATA class    TYPE REF TO data.
    DATA type     TYPE c LENGTH 12.
    FIELD-SYMBOLS <class> TYPE data.
    DATA lr_impl  TYPE REF TO ty_s_impl.
    FIELD-SYMBOLS <description> TYPE any.
    DATA ls_class TYPE ty_s_class_descr.
    DATA lv_fm    TYPE string.

    ls_key-intkey = val.

    lv_fm = `SEO_INTERFACE_IMPLEM_GET_ALL`.
    CALL FUNCTION lv_fm
      EXPORTING
        intkey        = ls_key
      IMPORTING
        impkeys       = lt_impl
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_impl REFERENCE INTO lr_impl.

      CLEAR ls_class.
      ls_class-classname = lr_impl->clsname.

      IF read_description = abap_true.

        " the repository structure only when a description is asked for -
        " the user-exit lookup (get_user_exit_class) never asks, and in a
        " stateless ICF session it runs once per request, so the dynamic
        " CREATE DATA of SEOC_CLASS_R was paid on every request for nothing
        IF class IS NOT BOUND.
          type = `SEOC_CLASS_R`.
          CREATE DATA class TYPE (type).
          ASSIGN class->* TO <class>.
        ENDIF.

        CLEAR <class>.
        ls_clskey-clsname = lr_impl->clsname.

        lv_fm = `SEO_CLASS_READ`.
        CALL FUNCTION lv_fm
          EXPORTING
            clskey        = ls_clskey
          IMPORTING
            class         = <class>
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        " a class the repository cannot read (deleted mid-list, inactive,
        " locked) keeps its slot with a blank description. This used to
        " RAISE, which turned ONE broken implementer into an empty result
        " for the caller - and the framework caller swallows that into
        " "no user exit configured anywhere", the #2564 damage picture,
        " with no error to point at
        IF sy-subrc = 0.
          ASSIGN
            COMPONENT `DESCRIPT`
            OF STRUCTURE <class>
            TO <description>.
          " ...and the same for a structure that does not carry the
          " component: skipped, blank description, not an ASSERT. An
          " ASSERTION_FAILED is a short dump no TRY can take (the reasoning
          " is written out at z2ui5_cl_ui5_view_builder=>a), so it would
          " bypass the single top-level catch in
          " z2ui5_cl_ui5_http_handler=>_main and answer the request with
          " nothing at all - to report that one description is missing.
          " sy-subrc is the right check HERE: ASSIGN COMPONENT sets it to
          " tell "component not found" from "assigned", which is the one
          " ASSIGN shape the #1937 rule does not cover (see unassign_data)
          IF sy-subrc = 0.
            ls_class-description = <description>.
          ENDIF.
        ENDIF.

      ENDIF.

      INSERT
        ls_class
        INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_get_data_element_text.

    DATA texts TYPE ty_s_data_element_text.
    DATA kind  TYPE string.

    texts = rtti_get_data_element_texts( val ).
    kind = to_upper( type ).

    CASE kind.
      WHEN `H`.
        result = texts-header.
      WHEN `M`.
        result = texts-medium.
      WHEN `L`.
        result = texts-long.
      WHEN OTHERS.
        result = texts-short.
    ENDCASE.

  ENDMETHOD.

  METHOD rtti_get_data_element_texts.

    DATA data_element_name TYPE string.
    DATA lv_do_fallback    TYPE abap_bool.

    data_element_name = val.

    TRY.
        rtti_get_dtel_texts_by_ddic( EXPORTING name        = data_element_name
                                     IMPORTING texts       = result
                                               do_fallback = lv_do_fallback ).
      CATCH cx_root.
        rtti_get_dtel_texts_by_xco( EXPORTING name        = data_element_name
                                    IMPORTING texts       = result
                                              do_fallback = lv_do_fallback ).
    ENDTRY.

    IF lv_do_fallback = abap_true AND result IS INITIAL.
      result-header = val.
      result-long = val.
      result-medium = val.
      result-short = val.
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_dtel_texts_by_ddic.

    DATA ddic_ref TYPE REF TO data.
    DATA: BEGIN OF ddic,
            reptext   TYPE string,
            scrtext_s TYPE string,
            scrtext_m TYPE string,
            scrtext_l TYPE string,
          END OF ddic.
    DATA struct_descr TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <ddic> TYPE data.
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.

    CLEAR texts.
    do_fallback = abap_false.

    cl_abap_typedescr=>describe_by_name( `T100` ).

    struct_descr ?= cl_abap_structdescr=>describe_by_name( `DFIES` ).

    CREATE DATA ddic_ref TYPE HANDLE struct_descr.

    " IS ASSIGNED, not sy-subrc, and a RETURN rather than an ASSERT: the
    " symbol is the unassign_data shape (#1937), and an ASSERTION_FAILED
    " here is a short dump the caller's own TRY cannot take - so the XCO
    " fallback it exists for would never run. The two branches below answer
    " a failure the same way
    ASSIGN ddic_ref->* TO <ddic>.
    IF <ddic> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    cl_abap_elemdescr=>describe_by_name( EXPORTING  p_name     = name
                                         RECEIVING p_descr_ref = lo_typedescr
                                         EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL METHOD lo_typedescr->(`GET_DDIC_FIELD`)
      RECEIVING
        p_flddescr   = <ddic>
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING <ddic> TO ddic.
    texts-header = ddic-reptext.
    texts-short  = ddic-scrtext_s.
    texts-medium = ddic-scrtext_m.
    texts-long   = ddic-scrtext_l.
    do_fallback  = abap_true.

  ENDMETHOD.

  METHOD rtti_get_dtel_texts_by_xco.

    DATA data_element TYPE REF TO object.
    DATA content      TYPE REF TO object.
    DATA exists       TYPE abap_bool.
    DATA lv_xco_cp_abap_dictionary TYPE string.
    " A dynamic call checks its actual parameters against the formal ones at
    " runtime and converts nothing, so both sides are typed exactly as XCO
    " declares them. The name is sxco_ad_object_name (CHAR30), passed by
    " reference - a string there raised CX_SY_DYN_CALL_ILLEGAL_TYPE. Each
    " label is if_xco_dtel_content=>ts_field_label, a text (CHAR60) and its
    " length (NUMC2) - received into a string, every one raised the same.
    " Both ended in the CATCH below, so on ABAP Cloud every data element came
    " back with empty texts (or its own name, as the caller's fallback). The
    " shapes are written out because the XCO types exist on cloud only
    DATA lv_name TYPE c LENGTH 30.
    DATA: BEGIN OF ls_label,
            text   TYPE c LENGTH 60,
            length TYPE n LENGTH 2,
          END OF ls_label.

    CLEAR texts.
    do_fallback = abap_false.

    TRY.
        lv_name = name.
        lv_xco_cp_abap_dictionary = `XCO_CP_ABAP_DICTIONARY`.
        CALL METHOD (lv_xco_cp_abap_dictionary)=>(`DATA_ELEMENT`)
          EXPORTING
            iv_name         = lv_name
          RECEIVING
            ro_data_element = data_element.

        CALL METHOD data_element->(`IF_XCO_AD_DATA_ELEMENT~EXISTS`)
          RECEIVING
            rv_exists = exists.

        IF exists = abap_false.
          RETURN.
        ENDIF.

        CALL METHOD data_element->(`IF_XCO_AD_DATA_ELEMENT~CONTENT`)
          RECEIVING
            ro_content = content.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_HEADING_FIELD_LABEL`)
          RECEIVING
            rs_heading_field_label = ls_label.
        texts-header = ls_label-text.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_SHORT_FIELD_LABEL`)
          RECEIVING
            rs_short_field_label = ls_label.
        texts-short = ls_label-text.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_MEDIUM_FIELD_LABEL`)
          RECEIVING
            rs_medium_field_label = ls_label.
        texts-medium = ls_label-text.

        CALL METHOD content->(`IF_XCO_DTEL_CONTENT~GET_LONG_FIELD_LABEL`)
          RECEIVING
            rs_long_field_label = ls_label.
        texts-long = ls_label-text.

        do_fallback = abap_true.

      CATCH cx_root.
        do_fallback = abap_true.
    ENDTRY.

  ENDMETHOD.

  METHOD uuid_get_c22.

    DATA lv_uuid      TYPE c LENGTH 22.
    DATA lv_classname TYPE string.
    DATA lv_fm        TYPE string.

    TRY.

        TRY.

            lv_classname = `CL_SYSTEM_UUID`.
            CALL METHOD (lv_classname)=>if_system_uuid_static~create_uuid_c22
              RECEIVING
                uuid = lv_uuid.

          CATCH cx_root.
            " as in uuid_get_c32: any failure of the class - not only its
            " absence - falls back to the function module

            lv_fm = `GUID_CREATE`.
            " error_message: a message the module sends would otherwise end
            " the request, which no CATCH reaches
            CALL FUNCTION lv_fm
              IMPORTING
                ev_guid_22    = lv_uuid
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zabaputil_cx_util_error
                EXPORTING
                  val = `GUID_CREATE_FAILED`.
            ENDIF.

        ENDTRY.

        result = lv_uuid.

      CATCH cx_root INTO DATA(lx_uuid).
        " both mechanisms failed - a catchable error, not the uncatchable
        " ASSERTION_FAILED. No re-entrancy guard as in uuid_get_c32: the
        " error report reads a c32, never a c22
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_uuid.
    ENDTRY.

    result = replace( val  = result
                      sub  = `}`
                      with = `0`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `{`
                      with = `0`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `"`
                      with = `0`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `'`
                      with = `0`
                      occ  = 0 ).

  ENDMETHOD.

  METHOD uuid_get_c32.
    DATA lv_uuid      TYPE c LENGTH 32.
    DATA lv_classname TYPE string.
    DATA lv_fm        TYPE string.

    TRY.

        TRY.

            lv_classname = `CL_SYSTEM_UUID`.
            CALL METHOD (lv_classname)=>if_system_uuid_static~create_uuid_c32
              RECEIVING
                uuid = lv_uuid.

          CATCH cx_root.

            lv_fm = `GUID_CREATE`.
            " error_message: a message the module sends would otherwise end
            " the request, which no CATCH reaches
            CALL FUNCTION lv_fm
              IMPORTING
                ev_guid_32    = lv_uuid
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zabaputil_cx_util_error
                EXPORTING
                  val = `GUID_CREATE_FAILED`.
            ENDIF.

        ENDTRY.

        result = lv_uuid.

      CATCH cx_root INTO DATA(lx_uuid).
        " both UUID mechanisms failed - raise the framework exception so the
        " consumer's single top-level catch can turn it into a handled error.
        " ASSERT would raise the uncatchable ASSERTION_FAILED and bypass that
        " catch (short dump instead of a handled error response).
        " zabaputil_cx_util_error reads a uuid when it renders its full report,
        " so a renderer running while the raise below is on its way would
        " re-enter this method. gv_uuid_failed makes that nested call return an
        " empty UUID instead of raising again.
        IF gv_uuid_failed = abap_true.
          RETURN.
        ENDIF.
        gv_uuid_failed = abap_true.
        TRY.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = lx_uuid.
          CLEANUP.
            CLEAR gv_uuid_failed.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD rtti_get_class_descr_on_cloud.
    TRY.

        DATA obj          TYPE REF TO object.
        DATA content      TYPE REF TO object.
        DATA lv_classname   TYPE c LENGTH 30.
        DATA xco_cp_abap    TYPE c LENGTH 11.
        DATA lv_description TYPE c LENGTH 60.

        lv_classname = classname.

        xco_cp_abap = `XCO_CP_ABAP`.
        CALL METHOD (xco_cp_abap)=>(`CLASS`)
          EXPORTING
            iv_name  = lv_classname
          RECEIVING
            ro_class = obj.

        CALL METHOD obj->(`IF_XCO_AO_CLASS~CONTENT`)
          RECEIVING
            ro_content = content.

        " sxco_ar_short_description, CHAR60 - typed exactly, for the same
        " reason as the labels in rtti_get_dtel_texts_by_xco
        CALL METHOD content->(`IF_XCO_CLAS_CONTENT~GET_SHORT_DESCRIPTION`)
          RECEIVING
            rv_short_description = lv_description.
        result = lv_description.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD context_get_tenant.

    "DATA(tenant_info) = xco_cp=>current->tenant( ).
    "DATA(account_id) = tenant_info->get_global_account_id( ).

    " not implemented: the result was silently empty, which reads like
    " a success - the XCO calls above are the template
    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = `CONTEXT_GET_TENANT is not implemented`.

  ENDMETHOD.

  METHOD context_get_callstack.

    IF check_abap_cloud( ).

      DATA current_obj TYPE REF TO object.
      DATA stack TYPE REF TO object.
      DATA full_stack TYPE REF TO object.
      DATA format_source TYPE REF TO object.
      DATA format_obj2 TYPE REF TO object.
      DATA format_obj3 TYPE REF TO object.
      DATA text_obj TYPE REF TO object.
      DATA lv_xco_cp TYPE c LENGTH 6.
      DATA ro_lines TYPE REF TO object.
      FIELD-SYMBOLS <current> TYPE any.
      FIELD-SYMBOLS <any> TYPE any.
      FIELD-SYMBOLS <call_stack> TYPE any.
      FIELD-SYMBOLS <format> TYPE any.
      FIELD-SYMBOLS <format2> TYPE any.

      "1 format source
      DATA(lv_assign) = `XCO_CP_CALL_STACK=>LINE_NUMBER_FLAVOR->SOURCE`.
      ASSIGN (lv_assign) TO <format>.

      lv_assign = `XCO_CP_CALL_STACK=>FORMAT`.
      ASSIGN (lv_assign) TO <format2>.
      format_obj2 = <format2>.

      CALL METHOD format_obj2->(`IF_XCO_CP_CS_FORMAT_FACTORY~ADT`)
        RECEIVING
          ro_adt = format_obj3.

      CALL METHOD format_obj3->(`WITH_LINE_NUMBER_FLAVOR`)
        EXPORTING
          io_line_number_flavor = <format>
        RECEIVING
          ro_me                 = format_source.

      lv_xco_cp = `XCO_CP`.
      ASSIGN (lv_xco_cp)=>(`CURRENT`) TO <current>.
      current_obj = <current>.

      ASSIGN current_obj->(`IF_XCO_CP_STD_CURRENT~CALL_STACK`) TO <call_stack>.
      stack = <call_stack>.

      CALL METHOD stack->(`IF_XCO_CP_STD_CUR_API_CLL_STCK~FULL`)
        RECEIVING
          ro_full = full_stack.

      DATA r TYPE REF TO data.
      CREATE DATA r TYPE REF TO (`IF_XCO_CS_FORMAT`).
      ASSIGN r->* TO <any>.
      <any> ?= format_source.

      CALL METHOD full_stack->(`IF_XCO_CP_CALL_STACK~AS_TEXT`)
        EXPORTING
          io_format = <any>
        RECEIVING
          ro_text   = text_obj.

      CALL METHOD text_obj->(`IF_XCO_TEXT~GET_LINES`)
        RECEIVING
          ro_lines = ro_lines.

      FIELD-SYMBOLS <lt_lines> TYPE string_table.
      ASSIGN ro_lines->(`IF_XCO_STRINGS~VALUE`) TO <lt_lines>.

      LOOP AT <lt_lines> INTO DATA(text).
        DATA(ls_stack) = VALUE ty_s_stack( ).
        SPLIT text AT ` ` INTO ls_stack-class ls_stack-include ls_stack-method.
        INSERT ls_stack INTO TABLE result.
      ENDLOOP.

      DELETE result INDEX 1.

    ENDIF.

  ENDMETHOD.

  METHOD context_get_sy.

    result = CORRESPONDING #( sy ).

  ENDMETHOD.


  METHOD msg_get_text.

    DATA(lt_msg) = msg_get_t( val = val val2 = val2 ).
    IF lt_msg IS NOT INITIAL.
      result = lt_msg[ 1 ]-text.
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_by_sy.

    result = msg_get_t( context_get_sy( ) ).

  ENDMETHOD.

  METHOD msg_get_internal.

    CASE rtti_get_type_kind( val ).

      WHEN cl_abap_datadescr=>typekind_table.
        FIELD-SYMBOLS <tab> TYPE ANY TABLE.
        " guarded like data_render_tab and data_get_headline - handing an
        " UNASSIGNED symbol to the method below is a GETWA_NOT_ASSIGNED
        " dump. No messages then, which is what "nothing in here is a
        " message" means to the caller: it falls back to the data renderer
        ASSIGN val TO <tab>.
        IF <tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.
        result = msg_get_internal_tab( <tab> ).

      WHEN cl_abap_datadescr=>typekind_struct1 OR cl_abap_datadescr=>typekind_struct2.

        IF val IS INITIAL.
          RETURN.
        ENDIF.

        IF iv_check_rap = abap_true AND check_is_rap_struct( val ) = abap_true.
          result = msg_get_rap( val ).
          RETURN.
        ENDIF.

        DATA(lt_attri) = rtti_get_t_attri_by_any( val ).

        DATA(ls_result) = VALUE ty_s_msg( ).
        LOOP AT lt_attri REFERENCE INTO DATA(ls_attri).
          ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO FIELD-SYMBOL(<comp>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " only an ITEM that can HOLD messages is the envelope's payload -
          " a table, a structure or an object (the BALI item). A plain
          " ITEM field (a position number) became the whole message and
          " dropped the structure's other fields
          IF ls_attri->name = `ITEM` AND check_msg_container( <comp> ) = abap_true.
            result = msg_get_internal( <comp> ).
            RETURN.
          ENDIF.
          ls_result = msg_map( name = ls_attri->name
                               val  = <comp>
                               msg  = ls_result ).

        ENDLOOP.
        " a T100 message is a class AND a number. A component called ID
        " alone is the key column of any business row - built into a
        " message, it gave `I:0001:` (a missing class still answers a text)
        " and the data never reached the data renderer it was meant for
        IF ls_result-text IS INITIAL AND ls_result-id IS NOT INITIAL AND ls_result-no IS NOT INITIAL.
          ls_result-id = to_upper( ls_result-id ).
          MESSAGE ID ls_result-id TYPE `I` NUMBER ls_result-no
                  WITH ls_result-v1 ls_result-v2 ls_result-v3 ls_result-v4
                  INTO ls_result-text.
        ENDIF.
        INSERT ls_result INTO TABLE result.

      WHEN cl_abap_datadescr=>typekind_oref.
        " an unbound reference carries no message - guarded like the struct
        " and table branches guard theirs. Unguarded it fell through the
        " three handlers of msg_get_by_oref into a describe on the null
        " reference, and message_box_display( unbound ) ended in a 500
        " instead of the documented silence
        IF val IS NOT INITIAL.
          result = msg_get_by_oref( val ).
        ENDIF.

      WHEN OTHERS.

        " skip an empty character value like the struct branch does -
        " otherwise msg_get_t's val2 fallback can never take over and the
        " caller renders a message box with blank text
        IF rtti_check_clike( val ) AND val IS NOT INITIAL.
          INSERT VALUE #( text = val ) INTO TABLE result.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD msg_get_by_oref.

    " four shapes, tried in turn and each on its own level - the nested
    " TRY/CATCH pyramid this used to be carried the attribute mapping twice,
    " three CATCH blocks deep, one fix away from drifting. Every attempt
    " that fails falls through to the next; the last one cannot fail
    " quietly, it maps whatever public attributes the object has

    " 1. an exception: its text, and its public attributes over it
    TRY.
        DATA(lx) = CAST cx_root( val ).
        INSERT msg_get_by_oref_attri( io_obj = lx
                                      is_msg = VALUE #( type = `E`
                                                        text = lx->get_text( ) ) ) INTO TABLE result.
        RETURN.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    DATA obj TYPE REF TO object.
    obj = val.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE any.

    " 2. an application log (IF_BALI_LOG)
    TRY.
        CREATE DATA lr_tab TYPE (`if_bali_log=>ty_item_table`).
        ASSIGN lr_tab->* TO <tab>.
        CALL METHOD obj->(`IF_BALI_LOG~GET_ALL_ITEMS`)
          RECEIVING
            item_table = <tab>.
        INSERT LINES OF msg_get_internal( <tab> ) INTO TABLE result.
        RETURN.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    " 3. a logger (ZIF_LOGGER)
    TRY.
        CREATE DATA lr_tab TYPE (`BAPIRETTAB`).
        ASSIGN lr_tab->* TO <tab>.
        CALL METHOD obj->(`ZIF_LOGGER~EXPORT_TO_TABLE`)
          RECEIVING
            rt_bapiret = <tab>.
        INSERT LINES OF msg_get_internal( <tab> ) INTO TABLE result.
        RETURN.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    " 4. any object: whatever public attributes carry a message part
    INSERT msg_get_by_oref_attri( io_obj = obj
                                  is_msg = VALUE #( ) ) INTO TABLE result.

  ENDMETHOD.

  METHOD msg_map.

    result = msg.

    " The value is a component of an ARBITRARY structure: msg_get_internal
    " walks whatever an app hands to message_box_display( ) and maps every
    " component BY NAME. A business structure with a component that happens
    " to be called TEXT, ID, TYPE or V1 and is a TABLE, a nested structure
    " or a reference is not a message part - and the assignments below are
    " no class-based exception for it, they are a MOVE type conflict
    " (OBJECTS_MOVE_NOT_SUPPORTED on a system, "table, no header line" in
    " the transpiled runtime). So the box that was supposed to REPORT a
    " problem was the crash: nothing between here and the app's own main( )
    " catches it, and the roundtrip ended as a 500.
    " Decided BEFORE anything is assigned, which is the same rule
    " z2ui5_cl_ui5_srv_model=>delta_apply_field follows for the same reason
    " - a runtime error cannot be caught after the fact. A skipped component
    " simply is not a message part: the entry ends up without a text,
    " ui5_msg_box_format drops it, and the caller falls back to the DATA
    " renderer, which shows the whole structure including that component
    IF rtti_check_printable( val ) = abap_false.
      RETURN.
    ENDIF.

    CASE name.
      WHEN `ID` OR `MSGID` OR `ARBGB`.
        result-id = val.
      WHEN `NO` OR `NUMBER` OR `MSGNO` OR `TXTNR`.
        result-no = val.
      WHEN `MESSAGE` OR `TEXT`.
        result-text = val.
      WHEN `TYPE` OR `MSGTY` OR `M_SEVERITY`.
        result-type = val.
      WHEN `MESSAGE_V1` OR `MSGV1` OR `V1`.
        result-v1 = val.
      WHEN `MESSAGE_V2` OR `MSGV2` OR `V2`.
        result-v2 = val.
      WHEN `MESSAGE_V3` OR `MSGV3` OR `V3`.
        result-v3 = val.
      WHEN `MESSAGE_V4` OR `MSGV4` OR `V4`.
        result-v4 = val.
      WHEN `TIME_STMP`.
        " the one target that is not a string: a printable but non-numeric
        " value (`2026-09-16 10:00` in a char field of that name) converts
        " into the packed timestamp with CX_SY_CONVERSION_NO_NUMBER, which
        " is catchable where the MOVE conflict above is not - caught, and
        " the entry keeps no timestamp rather than costing the whole box
        TRY.
            result-timestampl = val.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD check_is_rap_struct.

    DATA(lt_attri) = rtti_get_t_attri_by_any( val ).

    LOOP AT lt_attri REFERENCE INTO DATA(ls_attri).
      CASE ls_attri->name.
        WHEN `%MSG` OR `%FAIL` OR `%OTHER`.
          result = abap_true.
          RETURN.
      ENDCASE.
    ENDLOOP.

    LOOP AT lt_attri REFERENCE INTO ls_attri.
      " the component's descriptor is already in the attribute table - no
      " ASSIGN and no describe_by_data per component
      IF ls_attri->type->kind <> cl_abap_typedescr=>kind_table.
        CONTINUE.
      ENDIF.

      TRY.
          DATA(lo_line) = CAST cl_abap_tabledescr( ls_attri->type )->get_table_line_type( ).
          IF lo_line->kind <> cl_abap_typedescr=>kind_struct.
            CONTINUE.
          ENDIF.
          DATA(lt_comps) = CAST cl_abap_structdescr( lo_line )->get_components( ).
          LOOP AT lt_comps REFERENCE INTO DATA(ls_comp).
            IF ls_comp->name = `%MSG` OR ls_comp->name = `%FAIL`.
              result = abap_true.
              RETURN.
            ENDIF.
          ENDLOOP.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_rap.

    DATA(lv_kind) = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.

    msg_get_rap_row( EXPORTING val         = val
                               entity_name = entity_name
                     IMPORTING messages    = result
                               is_row      = DATA(lv_is_row) ).
    IF lv_is_row = abap_true.
      RETURN.
    ENDIF.

    DATA(lt_attri) = rtti_get_t_attri_by_any( val ).
    LOOP AT lt_attri REFERENCE INTO DATA(ls_attri).
      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO FIELD-SYMBOL(<tab>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF rtti_get_type_kind( <tab> ) <> cl_abap_datadescr=>typekind_table.
        CONTINUE.
      ENDIF.

      " guarded like data_render_tab, msg_get_internal and
      " data_get_headline - and here the symbol is LOOP-CARRIED, so an
      " ASSIGN that fails leaves it on the PREVIOUS component's table and
      " the rows below would be read out of that one
      FIELD-SYMBOLS <ftab> TYPE ANY TABLE.
      UNASSIGN <ftab>.
      ASSIGN <tab> TO <ftab>.
      IF <ftab> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      " the row kind is a property of the table TYPE - decided once per
      " table, not asked of every row (same idea as msg_get_internal_tab)
      DATA(lo_line) = CAST cl_abap_tabledescr( ls_attri->type )->get_table_line_type( ).
      LOOP AT <ftab> ASSIGNING FIELD-SYMBOL(<row>).
        IF lo_line->type_kind = cl_abap_datadescr=>typekind_oref.
          IF <row> IS NOT INITIAL.
            TRY.
                INSERT LINES OF msg_get_t( <row> ) INTO TABLE result.
              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
          ENDIF.
        ELSE.
          INSERT LINES OF msg_get_rap( val         = <row>
                                       entity_name = ls_attri->name ) INTO TABLE result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_rap_row.
    CLEAR messages.
    is_row = abap_false.

    " the meta block is built LAZILY: msg_get_rap_meta walks the row's
    " components three times over (element/action/tky scans), and the
    " common row in a RAP response table carries neither a filled %MSG nor
    " a %FAIL - building the block up front threw that work away per row
    DATA lv_meta_built TYPE abap_bool.
    DATA lt_meta TYPE ty_t_name_value.

    ASSIGN COMPONENT `%MSG` OF STRUCTURE val TO FIELD-SYMBOL(<msg>).
    IF sy-subrc = 0.
      is_row = abap_true.
      IF <msg> IS NOT INITIAL.
        lt_meta = msg_get_rap_meta( val ).
        lv_meta_built = abap_true.
        TRY.
            DATA(lt_one) = msg_get_t( <msg> ).
            LOOP AT lt_one ASSIGNING FIELD-SYMBOL(<m>).
              <m>-t_meta = lt_meta.
            ENDLOOP.
            INSERT LINES OF lt_one INTO TABLE messages.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT `%FAIL` OF STRUCTURE val TO FIELD-SYMBOL(<fail>).
    IF sy-subrc = 0.
      is_row = abap_true.
      ASSIGN COMPONENT `CAUSE` OF STRUCTURE <fail> TO FIELD-SYMBOL(<cause>).
      IF sy-subrc = 0.
        IF lv_meta_built = abap_false.
          lt_meta = msg_get_rap_meta( val ).
        ENDIF.
        " %FAIL-CAUSE is the enumeration IF_ABAP_BEHV=>T_FAIL_CAUSE (base
        " type i). An enumerated value does not convert to i - a plain MOVE
        " is the runtime error UC_OBJECTS_NOT_CONVERTIBLE - so it is read by
        " its name (a string template writes the name) and mapped to the
        " base value. Type kind `k` is the enumeration (literal: the
        " constant typekind_enum does not exist on 7.02).
        DATA lv_cause TYPE i.
        IF cl_abap_typedescr=>describe_by_data( <cause> )->type_kind = `k`.
          lv_cause = msg_get_rap_cause_value( |{ <cause> }| ).
        ELSE.
          lv_cause = <cause>.
        ENDIF.
        DATA(lv_text) = msg_get_rap_fail_text( lv_cause ).
        IF entity_name IS NOT INITIAL.
          lv_text = |{ entity_name }: { lv_text }|.
        ENDIF.
        INSERT VALUE #( type   = `E`
                        text   = lv_text
                        t_meta = lt_meta ) INTO TABLE messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_comp_str.

    " printable first, then the assignment: the component comes out of a
    " structure this class did not declare, and a table or a nested
    " structure into a string is a MOVE type conflict, not an exception -
    " see msg_map, which learnt it the expensive way.
    " One chained AND and not the nested IFs z2ui5_cl_ui5_handler=>
    " request_context_info argues for: abaplint's if_in_if refuses the
    " nesting here (nothing sits between the two tests), and the second
    " operand is safe on a failed ASSIGN either way - ABAP evaluates a
    " logical expression left to right and stops once the result is decided
    ASSIGN COMPONENT comp OF STRUCTURE val TO FIELD-SYMBOL(<comp>).
    IF sy-subrc = 0 AND rtti_check_printable( <comp> ) = abap_true.
      result = <comp>.
    ENDIF.

  ENDMETHOD.

  METHOD scan_flag_prefix.

    DATA(lv_kind) = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.

    " a `-` in the prefix is a path, not part of a component name - no
    " component name can contain one: %ELEMENT-<field> and
    " %OP-%ACTION-<name> are components of the NESTED structures %ELEMENT
    " and %OP-%ACTION. Compared against the row's own component names, the
    " two RAP prefixes never matched, and no message ever carried its
    " element or action
    DATA(lv_prefix) = CONV string( prefix ).
    DATA(lv_off) = find( val = lv_prefix
                         sub = `-` ).
    IF lv_off >= 0.
      ASSIGN COMPONENT lv_prefix(lv_off) OF STRUCTURE val TO FIELD-SYMBOL(<sub>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      result = scan_flag_prefix( val    = <sub>
                                 prefix = substring( val = lv_prefix
                                                     off = lv_off + 1 ) ).
      RETURN.
    ENDIF.

    DATA(lv_len) = strlen( lv_prefix ).
    DATA(lt_attri) = rtti_get_t_attri_by_any( val ).
    LOOP AT lt_attri REFERENCE INTO DATA(ls_attri).
      IF strlen( ls_attri->name ) <= lv_len.
        CONTINUE.
      ENDIF.
      IF NOT ( lv_len = 0 OR ls_attri->name(lv_len) = lv_prefix ).
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO FIELD-SYMBOL(<flag>).
      IF sy-subrc <> 0 OR <flag> IS INITIAL.
        CONTINUE.
      ENDIF.
      INSERT ls_attri->name+lv_len INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_rap_element.

    DATA(lt_suffix) = scan_flag_prefix( val       = val
                                        prefix = `%ELEMENT-` ).
    result = concat_lines_of( table = lt_suffix
                              sep   = `, ` ).

  ENDMETHOD.

  METHOD msg_get_rap_state_area.

    result = get_comp_str( val     = val
                           comp = `%STATE_AREA` ).

  ENDMETHOD.

  METHOD msg_get_rap_action.

    DATA(lt_suffix) = scan_flag_prefix( val       = val
                                        prefix = `%OP-%ACTION-` ).
    result = VALUE #( lt_suffix[ 1 ] OPTIONAL ).

  ENDMETHOD.

  METHOD msg_get_rap_pid.

    result = get_comp_str( val     = val
                           comp = `%PID` ).

  ENDMETHOD.

  METHOD msg_get_rap_cid.

    result = get_comp_str( val     = val
                           comp = `%CID` ).

  ENDMETHOD.

  METHOD msg_get_rap_tky.

    ASSIGN COMPONENT `%TKY` OF STRUCTURE val TO FIELD-SYMBOL(<tky>).
    IF sy-subrc <> 0 OR <tky> IS INITIAL.
      RETURN.
    ENDIF.
    result = msg_get_rap_flatten( <tky> ).

  ENDMETHOD.

  METHOD msg_get_rap_flatten.

    DATA(lv_kind) = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.

    DATA(lt_attri) = rtti_get_t_attri_by_any( val ).
    DATA lt_part TYPE string_table.

    LOOP AT lt_attri REFERENCE INTO DATA(ls_attri).
      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO FIELD-SYMBOL(<comp>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      DATA(lv_sub_kind) = rtti_get_type_kind( <comp> ).
      IF lv_sub_kind = cl_abap_datadescr=>typekind_struct1
          OR lv_sub_kind = cl_abap_datadescr=>typekind_struct2.
        DATA(lv_sub) = msg_get_rap_flatten( <comp> ).
        IF lv_sub IS NOT INITIAL.
          INSERT lv_sub INTO TABLE lt_part.
        ENDIF.
      ELSEIF <comp> IS NOT INITIAL AND rtti_check_printable( <comp> ) = abap_true.
        " the printable test does what the TRY around this could not: a
        " TABLE component into the string below is a MOVE type conflict and
        " therefore a runtime error, which no CATCH takes (see msg_map). The
        " TRY stays for the conversions that ARE class-based - a character
        " value the packed/numeric target refuses
        TRY.
            DATA lv_str TYPE string.
            lv_str = <comp>.
            INSERT |{ ls_attri->name }={ lv_str }| INTO TABLE lt_part.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    result = concat_lines_of( table = lt_part
                              sep   = `, ` ).

  ENDMETHOD.

  METHOD msg_get_rap_meta.

    DATA lv TYPE string.

    lv = msg_get_rap_element( val ).
    IF lv IS NOT INITIAL.
      INSERT VALUE #( n = `element` v = lv ) INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_state_area( val ).
    IF lv IS NOT INITIAL.
      INSERT VALUE #( n = `state_area` v = lv ) INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_action( val ).
    IF lv IS NOT INITIAL.
      INSERT VALUE #( n = `action` v = lv ) INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_pid( val ).
    IF lv IS NOT INITIAL.
      INSERT VALUE #( n = `pid` v = lv ) INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_cid( val ).
    IF lv IS NOT INITIAL.
      INSERT VALUE #( n = `cid` v = lv ) INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_tky( val ).
    IF lv IS NOT INITIAL.
      INSERT VALUE #( n = `tky` v = lv ) INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_rap_fail_text.
    " the base values of IF_ABAP_BEHV=>T_FAIL_CAUSE, read on an S/4HANA
    " system (2026-11-13) - HTTP-like codes, not a running number
    result = SWITCH string( cause
      WHEN 0    THEN `Operation failed`
      WHEN 401  THEN `Authorization failure`
      WHEN 404  THEN `Entity not found`
      WHEN 409  THEN `Concurrent modification`
      WHEN 423  THEN `Entity is locked`
      WHEN 424  THEN `Dependent operation failed`
      WHEN 501  THEN `Operation not implemented`
      WHEN 4221 THEN `Operation disabled`
      WHEN 4222 THEN `Entity is read-only`
      ELSE |Operation failed (cause code { cause })| ).
  ENDMETHOD.

  METHOD rtti_get_t_attri_on_prem.

    DATA structdescr TYPE REF TO cl_abap_structdescr.
    DATA dfies       TYPE REF TO data.
    DATA s_dfies     TYPE ty_s_dfies.

    FIELD-SYMBOLS <dfies> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.

    DATA temp9           TYPE cl_abap_structdescr=>component_table.
    DATA comps           LIKE temp9.
    DATA temp10          TYPE REF TO cl_abap_structdescr.
    DATA lo_struct       LIKE temp10.
    DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
    DATA new_table_desc  TYPE REF TO cl_abap_tabledescr.
    DATA comp            LIKE LINE OF comps.
    FIELD-SYMBOLS <value>      TYPE any.
    FIELD-SYMBOLS <value_dest> TYPE any.

    comps = temp9.

    temp10 ?= cl_abap_structdescr=>describe_by_name( `DFIES` ).

    lo_struct = temp10.
    comps = lo_struct->get_components( ).

    TRY.

        new_struct_desc = cl_abap_structdescr=>create( comps ).

        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA dfies TYPE HANDLE new_table_desc.

        ASSIGN dfies->* TO <dfies>.
        IF <dfies> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        IF tabname IS INITIAL.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `RTTI_BY_NAME_TAB_INITIAL`.
        ENDIF.

        structdescr ?= cl_abap_structdescr=>describe_by_name( tabname ).
        <dfies> = structdescr->get_ddic_field_list( ).

        LOOP AT <dfies> ASSIGNING <line>.

          LOOP AT comps INTO comp.

            UNASSIGN <value>.
            ASSIGN COMPONENT comp-name OF STRUCTURE <line> TO <value>.
            IF <value> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            UNASSIGN <value_dest>.
            ASSIGN COMPONENT comp-name OF STRUCTURE s_dfies TO <value_dest>.
            IF <value_dest> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            <value_dest> = <value>.

            UNASSIGN <value>.
            UNASSIGN <value_dest>.

          ENDLOOP.

          INSERT s_dfies INTO TABLE result.
          CLEAR s_dfies.

        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_t_attri_on_cloud.

    DATA obj TYPE REF TO object.
    DATA lv_tabname TYPE c LENGTH 16.
    DATA lr_ddfields TYPE REF TO data.
    TYPES ty_c30 TYPE c LENGTH 30.
    DATA names TYPE STANDARD TABLE OF ty_c30 WITH EMPTY KEY.
    FIELD-SYMBOLS <any> TYPE any.
    FIELD-SYMBOLS <field> TYPE simple.
    FIELD-SYMBOLS <ddfields> TYPE ANY TABLE.

* convert to correct type,
    lv_tabname = tabname.

    TRY.
        TRY.
            DATA(lv_method2) = `XCO_CP_ABAP_DICTIONARY`.
            CALL METHOD (lv_method2)=>(`DATABASE_TABLE`)
              EXPORTING
                iv_name           = lv_tabname
              RECEIVING
                ro_database_table = obj.
            ASSIGN obj->(`IF_XCO_DATABASE_TABLE~FIELDS->IF_XCO_DBT_FIELDS_FACTORY~KEY`) TO <any>.
            IF sy-subrc  <> 0.
* fallback to RTTI, KEY field does not exist in S/4 2020
              RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
            ENDIF.
            obj = <any>.
            CALL METHOD obj->(`IF_XCO_DBT_FIELDS~GET_NAMES`)
              RECEIVING
                rt_names = names.
          CATCH cx_sy_dyn_call_illegal_class.
            DATA(workaround) = `DDFIELDS`.
            CREATE DATA lr_ddfields TYPE (workaround).
            ASSIGN lr_ddfields->* TO <ddfields>.
            ASSERT sy-subrc = 0.
            <ddfields> = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name(
              lv_tabname ) )->get_ddic_field_list( ).
            LOOP AT <ddfields> ASSIGNING <any>.
              ASSIGN COMPONENT `KEYFLAG` OF STRUCTURE <any> TO <field>.
              IF sy-subrc <> 0 OR <field> <> abap_true.
                CONTINUE.
              ENDIF.
              ASSIGN COMPONENT `FIELDNAME` OF STRUCTURE <any> TO <field>.
              ASSERT sy-subrc = 0.
              INSERT <field> INTO TABLE names.
            ENDLOOP.
        ENDTRY.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.


    " by NAME: rtti_get_t_attri_by_any( tabname ) described the string
    " holding the name, and the cast to a structure failed every time
    DATA(lt_comp) = rtti_get_t_attri_by_table_name( tabname ).
    LOOP AT lt_comp REFERENCE INTO DATA(lr_comp).

      DATA(lv_check_key) = abap_false.
      IF line_exists( names[ table_line = lr_comp->name ] ).
        lv_check_key = abap_true.
      ENDIF.

      INSERT VALUE #(
          fieldname = lr_comp->name
          rollname  = lr_comp->name
          keyflag = lv_check_key
        scrtext_s =  lr_comp->name
        scrtext_m =  lr_comp->name
        scrtext_l =  lr_comp->name
       ) INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_get_s_dfies_by_rollname.

    DATA lo_descr TYPE REF TO cl_abap_typedescr.
    DATA lr_dfies TYPE REF TO data.
    FIELD-SYMBOLS <dfies> TYPE any.

    " DFIES and get_ddic_field( ) do not exist on ABAP Cloud - both are
    " reached by name, so the class compiles there too
    IF check_abap_cloud( ) = abap_true.
      RETURN.
    ENDIF.

    TRY.
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = val
                                             RECEIVING  p_descr_ref    = lo_descr
                                             EXCEPTIONS type_not_found = 1
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0 OR lo_descr->kind <> cl_abap_typedescr=>kind_elem.
          RETURN.
        ENDIF.

        CREATE DATA lr_dfies TYPE (`DFIES`).
        ASSIGN lr_dfies->* TO <dfies>.

        CALL METHOD lo_descr->(`GET_DDIC_FIELD`)
          RECEIVING
            p_flddescr   = <dfies>
          EXCEPTIONS
            not_found    = 1
            no_ddic_type = 2
            OTHERS       = 3.
        IF sy-subrc = 0.
          result = CORRESPONDING #( BASE ( result ) <dfies> ).
        ENDIF.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_t_dfies_by_table_name.

    IF check_abap_cloud( ) IS NOT INITIAL.
      result = rtti_get_t_attri_on_cloud( table_name ).
    ELSE.
      result = rtti_get_t_attri_on_prem( table_name ).
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_table_desrc.

    DATA ddtext TYPE c LENGTH 60.

    IF langu IS NOT SUPPLIED.
      DATA(lan) = sy-langu.
    ELSE.
      lan = langu.
    ENDIF.

    IF check_abap_cloud( ).

      ddtext = tabname.

    ELSE.

      TRY.
          DATA(lv_tabname) = `dd02t`.
          SELECT SINGLE ddtext
            FROM (lv_tabname)
            WHERE tabname    = @tabname
              AND ddlanguage = @lan
            INTO @ddtext.
        CATCH cx_root ##NO_HANDLER.
          " DD02T not available (e.g. JS transpiler runtime) - fall
          " back to the table name below
      ENDTRY.

    ENDIF.

    IF ddtext IS NOT INITIAL.
      result = ddtext.
    ELSE.
      result = tabname.
    ENDIF.

  ENDMETHOD.

  METHOD bus_search_help_read.

    DATA lt_result_tab TYPE TABLE OF string.
    DATA ls_comp       TYPE abap_componentdescr.
    DATA lt_comps      TYPE abap_component_tab.
    DATA lo_datadescr  TYPE REF TO cl_abap_datadescr.
    DATA lr_line       TYPE REF TO data.

    " data ls_shlp type shlp_descr.

    DATA lr_shlp       TYPE REF TO data.

    DATA(lv_type) = `SHLP_DESCR`.
    CREATE DATA lr_shlp TYPE (lv_type).
    FIELD-SYMBOLS <shlp> TYPE any.
    ASSIGN lr_shlp->* TO <shlp>.

    DATA lv_tabname   TYPE c LENGTH 30.
    DATA lv_fieldname TYPE c LENGTH 30.
    lv_tabname = mv_table.
    lv_fieldname = mv_fname.

    IF ms_shlp IS INITIAL.
      " Suchhilfe lesen
      DATA(lv_fm) = `F4IF_DETERMINE_SEARCHHELP`.
      CALL FUNCTION lv_fm
        EXPORTING
          tabname           = lv_tabname
          fieldname         = lv_fieldname
        IMPORTING
          shlp              = <shlp>
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          error_message     = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |F4IF_DETERMINE_SEARCHHELP failed for { lv_tabname }-{ lv_fieldname }|.
      ENDIF.
      ms_shlp = CORRESPONDING #( <shlp> ).

      IF ms_shlp-intdescr-issimple = abap_false.

        DATA lr_t_shlp TYPE REF TO data.
        DATA(lv_type2) = `SHLP_DESCT`.
        CREATE DATA lr_t_shlp TYPE (lv_type2).
        FIELD-SYMBOLS <shlp2> TYPE STANDARD TABLE.
        ASSIGN lr_t_shlp->* TO <shlp2>.

        lv_fm = `F4IF_EXPAND_SEARCHHELP`.
        " shlp_top is typed SHLP_DESCR - pass the DDIC-typed result of
        " F4IF_DETERMINE_SEARCHHELP, not the local rebuild ms_shlp: a
        " function module called by name wants the exact type
        CALL FUNCTION lv_fm
          EXPORTING
            shlp_top      = <shlp>
          IMPORTING
            shlp_tab      = <shlp2>
          EXCEPTIONS
            " a MESSAGE TYPE E would end the request - on failure <shlp2>
            " stays empty and the collective help is used as it is
            error_message = 1
            OTHERS        = 2.

        FIELD-SYMBOLS <row2> TYPE any.
        ASSIGN <shlp2>[ 1 ] TO <row2>.
        IF sy-subrc = 0.
          ms_shlp = CORRESPONDING #( <row2> ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF mr_data IS BOUND.
      " Values from Caller app to Interface Values
      LOOP AT ms_shlp-interface REFERENCE INTO DATA(r_interface) WHERE value IS INITIAL.

        FIELD-SYMBOLS <any> TYPE any.
        ASSIGN mr_data->* TO <any>.
        FIELD-SYMBOLS <value> TYPE any.
        ASSIGN COMPONENT r_interface->shlpfield OF STRUCTURE <any> TO <value>.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        r_interface->value = <value>.

      ENDLOOP.
    ENDIF.

    " Interface Fixed Values to Selopt
    LOOP AT ms_shlp-interface INTO DATA(interface).

      " Match the name of the SH Field to the Input field name
      IF interface-valfield = mv_fname.
        mv_shlpfield = interface-shlpfield.
      ENDIF.

      IF interface-value IS NOT INITIAL.

        ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                  ( shlpfield = interface-shlpfield
                                    shlpname  = interface-valtabname
                                    option    = COND #( WHEN interface-value CA `*` THEN `CP` ELSE `EQ` )
                                    sign      = `I`
                                    low       = interface-value  ) ).

      ENDIF.

    ENDLOOP.

    LOOP AT ms_shlp-fieldprop INTO DATA(fieldrop).

      IF fieldrop-defaultval IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(default_value) = fieldrop-defaultval.
      default_value = replace( val = default_value sub = `'` with = `` occ = 0 ).

      ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                ( shlpfield = fieldrop-fieldname
                                  option    = COND #( WHEN fieldrop-defaultval CA `*` THEN `CP` ELSE `EQ` )
                                  sign      = `I`
                                  low       = default_value  ) ).

    ENDLOOP.

    CREATE DATA lr_shlp TYPE (lv_type).
    ASSIGN lr_shlp->* TO <shlp>.
    CLEAR: <shlp>.
    MOVE-CORRESPONDING ms_shlp TO <shlp>.

    lv_fm = `F4IF_SELECT_VALUES`.
    CALL FUNCTION lv_fm
      EXPORTING
        shlp           = <shlp>
        maxrecords     = iv_max_rows
        sort           = space
        call_shlp_exit = abap_true
      TABLES
        record_tab     = lt_result_tab
        recdescr_tab   = mt_result_desc
      EXCEPTIONS
        " a search help exit may send MESSAGE TYPE E - without this the
        " request ends there
        error_message  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |F4IF_SELECT_VALUES failed for { ms_shlp-shlpname }|.
    ENDIF.

    SORT ms_shlp-fieldprop BY shlplispos ASCENDING.

    LOOP AT ms_shlp-fieldprop INTO DATA(field_props) WHERE shlplispos IS NOT INITIAL.

      DATA(descption) = VALUE #( mt_result_desc[ fieldname = field_props-fieldname ] OPTIONAL ).

      ls_comp-name  = descption-fieldname.
      ls_comp-type ?= cl_abap_datadescr=>describe_by_name( descption-rollname ).
      INSERT ls_comp INTO TABLE lt_comps.

    ENDLOOP.

    IF NOT line_exists( lt_comps[ name = `ROW_ID` ] ).
      lo_datadescr ?= cl_abap_datadescr=>describe_by_name( `INT4` ).
      ls_comp-name  = `ROW_ID`.
      ls_comp-type ?= lo_datadescr.
      INSERT ls_comp INTO TABLE lt_comps.
    ENDIF.

    DATA(strucdescr) = cl_abap_structdescr=>create( lt_comps ).

    DATA(tabdescr) = cl_abap_tabledescr=>create( strucdescr ).

    IF mt_data IS NOT BOUND.
      CREATE DATA mt_data TYPE HANDLE tabdescr.
    ENDIF.

    FIELD-SYMBOLS <fs_target_tab> TYPE STANDARD TABLE.
    ASSIGN mt_data->* TO <fs_target_tab>.

    CLEAR <fs_target_tab>.

    " we don`t want to lose all inputs in row ...
    IF ms_data_row IS NOT BOUND.
      CREATE DATA ms_data_row TYPE HANDLE strucdescr.
    ENDIF.

    LOOP AT lt_result_tab INTO DATA(result_line).

      CREATE DATA lr_line TYPE HANDLE strucdescr.
      ASSIGN lr_line->* TO FIELD-SYMBOL(<fs_line>).

      LOOP AT mt_result_desc INTO DATA(result_desc).

        ASSIGN COMPONENT result_desc-fieldname OF STRUCTURE <fs_line>
               TO FIELD-SYMBOL(<line_content>).

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF result_desc-leng < result_desc-intlen.
          " internal and external representation differ
          " Unicode: halve the offset
          result_desc-offset = result_desc-offset / 2.
        ENDIF.

        TRY.
            <line_content> = result_line+result_desc-offset(result_desc-outputlen).
          CATCH cx_root.

            TRY.
                " String table will crash if value length <> outputlen
                <line_content> = result_line+result_desc-offset.
              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
        ENDTRY.

      ENDLOOP.

      INSERT <fs_line> INTO TABLE <fs_target_tab>.

    ENDLOOP.

    " Set default values
    LOOP AT ms_shlp-interface INTO interface.

      IF interface-value IS NOT INITIAL.

        UNASSIGN <any>.
        ASSIGN ms_data_row->* TO <any>.
        ASSIGN COMPONENT interface-shlpfield OF STRUCTURE <any> TO <value>.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        <value> = interface-value.

      ENDIF.

    ENDLOOP.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.

    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      FIELD-SYMBOLS <row> TYPE any.
      UNASSIGN <row>.
      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO <row>.
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD tab_get_where_by_dfies.

    DATA val TYPE string.

    LOOP AT it_dfies REFERENCE INTO DATA(dfies).

      IF NOT ( dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field ).
        CONTINUE.
      ENDIF.

      ASSIGN ms_data_row->* TO FIELD-SYMBOL(<row>).

      FIELD-SYMBOLS <value> TYPE any.
      UNASSIGN <value>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.

      IF result IS NOT INITIAL.
        DATA(and) = ` AND `.
      ENDIF.

      val = <value>.

      " the value goes into a literal of a dynamic WHERE: the escape character
      " first, then the LIKE wildcards, so that they match only themselves,
      " and a quote is doubled so that it cannot end the literal
      val = replace( val = val sub = `#` with = `##` occ = 0 ).
      val = replace( val = val sub = `%` with = `#%` occ = 0 ).
      val = replace( val = val sub = `_` with = `#_` occ = 0 ).
      val = replace( val = val sub = `'` with = `''` occ = 0 ).

      result = |{ result }{ and } ( { dfies->fieldname } LIKE '%{ val }%' ESCAPE '#' )|.

    ENDLOOP.

  ENDMETHOD.

  METHOD _get_e071k_tabkey.

    DATA lv_type       TYPE c LENGTH 1.
    DATA lv_tabkey     TYPE c LENGTH 999.
    DATA lv_tabkey_len TYPE i.
    DATA lv_field_len  TYPE i.

    LOOP AT dfies INTO DATA(s_dfies) WHERE keyflag = abap_true.

      FIELD-SYMBOLS <value> TYPE any.
      UNASSIGN <value>.
      ASSIGN COMPONENT s_dfies-fieldname OF STRUCTURE line TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      DATA(lo_value_type) = cl_abap_typedescr=>describe_by_data( <value> ).
      lv_type = lo_value_type->type_kind.

      IF lv_type NA 'CDNT'.
        lv_tabkey+lv_tabkey_len = '*'.
        rv_tabkey = lv_tabkey.
        RETURN.
      ENDIF.

      lv_field_len = lo_value_type->length / cv_char_util_charsize.
      lv_tabkey+lv_tabkey_len(lv_field_len) = <value>.
      lv_tabkey_len = lv_tabkey_len + lv_field_len.

    ENDLOOP.

    " TABKEY is CHAR120: a longer key becomes generic at its last position.
    " Cutting at the first `_` anywhere in the key put the `*` outside the
    " 120 characters when that `_` came late, and the key was not generic
    IF lv_tabkey_len > 119.
      lv_tabkey+119 = '*'.
    ENDIF.
    rv_tabkey = lv_tabkey.
  ENDMETHOD.

  METHOD bus_tr_add.
    IF check_abap_cloud( ).

    ELSE.

      FIELD-SYMBOLS <e071>    TYPE any.
      FIELD-SYMBOLS <t_e071k> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <t_e071>  TYPE STANDARD TABLE.

      " We need to set the MANDT is necessary
      set_mandt( ir_data ).

      DATA(r_e071k) = _set_e071k( ir_data      = ir_data
                                  iv_tabname   = iv_tabname
                                  is_transport = is_transport ).
      ASSIGN r_e071k->* TO <e071>.
      IF <e071> IS INITIAL.
        RETURN.
      ENDIF.

      DATA(r_e071) = _set_e071( iv_tabname   = iv_tabname
                                is_transport = is_transport ).

      ASSIGN r_e071k->* TO <t_e071k>.
      ASSIGN r_e071->* TO <t_e071>.

      DATA(fb1) = 'TR_APPEND_TO_COMM_OBJS_KEYS'.
      CALL FUNCTION fb1
        EXPORTING
          " the task: the keys belong to it, E071/E071K carry its number
          wi_trkorr     = is_transport-task
          iv_dialog     = abap_false
        TABLES
          wt_e071       = <t_e071>
          wt_e071k      = <t_e071k>
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |{ fb1 }: { sy-msgid } { sy-msgno } { sy-msgv1 } { sy-msgv2 } { sy-msgv3 } { sy-msgv4 }|.
      ENDIF.

      DATA(fb2) = 'TR_SORT_AND_COMPRESS_COMM'.
      CALL FUNCTION fb2
        EXPORTING
          iv_trkorr     = is_transport-task
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |{ fb2 }: { sy-msgid } { sy-msgno } { sy-msgv1 } { sy-msgv2 } { sy-msgv3 } { sy-msgv4 }|.
      ELSEIF iv_commit = abap_true.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD _set_e071k.
    DATA t_e071k TYPE REF TO data.
    DATA s_e071k TYPE REF TO data.

    FIELD-SYMBOLS <t_e071k> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <s_e071k> TYPE any.
    FIELD-SYMBOLS <value>   TYPE any.
    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>    TYPE any.

    DATA(t_comp) = rtti_get_t_attri_by_table_name( 'E071K' ).

    TRY.

        DATA(struct_desc) = cl_abap_structdescr=>create( t_comp ).

        DATA(table_desc) = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                       p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_e071k TYPE HANDLE table_desc.
        CREATE DATA s_e071k TYPE HANDLE struct_desc.

        ASSIGN t_e071k->* TO <t_e071k>.
        ASSIGN s_e071k->* TO <s_e071k>.

      CATCH cx_root INTO DATA(x).
        " without the E071K line type nothing below can be filled - say why
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |E071K line type could not be built: { x->get_text( ) }|
            previous = x.
    ENDTRY.

    DATA(dfies) = rtti_get_t_dfies_by_table_name( iv_tabname ).

*   is_transport-transport = assign_value( component = 'TRKORR'
*                                          structure = <s_e071k> ).                                         )

    ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = is_transport-task.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'PGMID' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'R3TR'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'MASTERTYPE' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'TABU'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJECT' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'TABU'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'MASTERNAME' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = iv_tabname.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJNAME' OF STRUCTURE <s_e071k> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = iv_tabname.
    ENDIF.
    UNASSIGN <value>.

    ASSIGN ir_data->* TO <tab>.

*    IF <tab> IS INITIAL.
*      RETURN.
*    ENDIF.

    LOOP AT <tab> ASSIGNING <line>.

      UNASSIGN <value>.
      ASSIGN COMPONENT 'TABKEY' OF STRUCTURE <s_e071k> TO <value>.
      IF <value> IS NOT ASSIGNED.
        RETURN.
      ELSE.
        <value> = _get_e071k_tabkey( dfies = dfies
                                     line  = <line> ).
      ENDIF.

      INSERT <s_e071k> INTO TABLE <t_e071k>.

    ENDLOOP.

    result = t_e071k.
  ENDMETHOD.

  METHOD _set_e071.

    DATA t_e071 TYPE REF TO data.
    DATA s_e071 TYPE REF TO data.

    FIELD-SYMBOLS <t_e071> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <s_e071> TYPE any.
    FIELD-SYMBOLS <value>  TYPE any.

    DATA(t_comp) = rtti_get_t_attri_by_table_name( 'E071' ).

    TRY.

        DATA(struct_desc_new) = cl_abap_structdescr=>create( t_comp ).

        DATA(table_desc_new) = cl_abap_tabledescr=>create( p_line_type  = struct_desc_new
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_e071 TYPE HANDLE table_desc_new.
        CREATE DATA s_e071 TYPE HANDLE struct_desc_new.

        ASSIGN t_e071->* TO <t_e071>.
        ASSIGN s_e071->* TO <s_e071>.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = is_transport-task.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'PGMID' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'R3TR'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJECT' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'TABU'.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJ_NAME' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = iv_tabname.
    ENDIF.
    UNASSIGN <value>.
    ASSIGN COMPONENT 'OBJFUNC' OF STRUCTURE <s_e071> TO <value>.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ELSE.
      <value> = 'K'.
    ENDIF.
    UNASSIGN <value>.

    INSERT <s_e071> INTO TABLE <t_e071>.

    result = t_e071.

  ENDMETHOD.

  METHOD _read_e070.

    DATA lo_tab  TYPE REF TO data.
    DATA lo_line TYPE REF TO data.
    DATA ls_data TYPE ty_s_transport.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <value> TYPE any.

    DATA(table_name) = 'E070'.

    TRY.
        DATA(t_comp) = rtti_get_t_attri_by_table_name( table_name ).

        DATA(new_struct_desc) = cl_abap_structdescr=>create( t_comp ).

        DATA(new_table_desc) = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA lo_tab TYPE HANDLE new_table_desc.
        CREATE DATA lo_line TYPE HANDLE new_struct_desc.

        ASSIGN lo_tab->* TO <table>.
        ASSIGN lo_line->* TO <line>.

        DATA(where) =
        |( TRFUNCTION EQ 'Q' ) AND ( TRSTATUS EQ 'D' ) AND ( KORRDEV EQ 'CUST' ) AND ( AS4USER EQ '{ sy-uname }' )|.

        SELECT trkorr,
               trfunction,
               trstatus,
               tarsystem,
               korrdev,
               as4user,
               as4date,
               as4time,
               strkorr
          FROM (table_name)
          WHERE (where)
          INTO TABLE @<table>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
    IF <table> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " the rows are TASKS (TRFUNCTION Q): TRKORR is the task, STRKORR the
    " request it belongs to - the other way round, as it was, bus_tr_add
    " wrote the keys under the request number
    LOOP AT <table> INTO <line>.

      UNASSIGN <value>.
      ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <line> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ELSE.
        ls_data-task = <value>.
      ENDIF.

      UNASSIGN <value>.

      ASSIGN COMPONENT 'STRKORR' OF STRUCTURE <line> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ELSE.
        ls_data-transport = <value>.
      ENDIF.

      UNASSIGN <value>.

      INSERT ls_data INTO TABLE mt_data.

    ENDLOOP.

  ENDMETHOD.

  METHOD bus_tr_read.

    IF check_abap_cloud( ).

*          data(lo_current_user) = xco_cp=>sy->user( ).
*
*    DATA(lo_kind_filter) = xco_cp_transport=>filter->kind( xco_cp_transport=>kind->task ).
*    DATA(lo_owner_filter) = xco_cp_transport=>filter->owner( xco_cp_abap_sql=>constraint->equal( lo_current_user->name ) ).
*    DATA(lo_status_filter) = xco_cp_transport=>filter->status( xco_cp_transport=>status->modifiable ).
*    DATA(lo_type_filter) = xco_cp_transport=>filter->type( io_type = xco_cp_transport=>type->customizing_task ).
*    DATA(lt_transports) = xco_cp_cts=>transports->where( VALUE #( ( lo_kind_filter )
*                                                                  ( lo_owner_filter )
*                                                                  ( lo_status_filter )
*                                                                  ( lo_type_filter ) )
*    )->resolve( xco_cp_transport=>resolution->request ).
*
*    LOOP AT lt_transports INTO DATA(lo_transport).
*      DATA(lo_transport_request) = lo_transport->get_request( ).
*
*      DATA(prop) = lo_transport_request->properties( )->get( ).
*
*      DATA(tasks) = lo_transport_request->get_tasks( ).
*
*      LOOP AT tasks INTO DATA(task).
*
*        IF lo_current_user->name = task->properties( )->get_owner( )->name.
*
*          DATA(data) = VALUE ty_s_data( short_description = prop-short_description
*                                        transport         = lo_transport_request->value
*                                        task              = task->value ).
*          APPEND data TO mt_data.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDLOOP.

    ELSE.

      DATA lo_tab  TYPE REF TO data.
      DATA lo_line TYPE REF TO data.

      FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <line>  TYPE any.
      FIELD-SYMBOLS <value> TYPE any.

      _read_e070( CHANGING mt_data = mt_data ).
      " no task - an empty dynamic WHERE would read every text of E07T
      IF mt_data IS INITIAL.
        RETURN.
      ENDIF.

      DATA(table_name) = 'E07T'.

      TRY.
          DATA(t_comp) = rtti_get_t_attri_by_table_name( table_name ).

          DATA(new_struct_desc) = cl_abap_structdescr=>create( t_comp ).

          DATA(new_table_desc) = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                             p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA lo_tab TYPE HANDLE new_table_desc.
          CREATE DATA lo_line TYPE HANDLE new_struct_desc.

          ASSIGN lo_tab->* TO <table>.
          ASSIGN lo_line->* TO <line>.

          DATA(index) = 0.

          " the description shown is the one of the request
          LOOP AT mt_data INTO DATA(line).
            index = index + 1.
            IF index = 1.
              DATA(where) = |TRKORR EQ { filter_sql_quote( line-transport ) }|.
            ELSE.
              where = |{ where } OR TRKORR EQ { filter_sql_quote( line-transport ) }|.
            ENDIF.
            where = |( { where } )|.
          ENDLOOP.

          SELECT trkorr,
                 langu,
                 as4text
            FROM (table_name)
            WHERE (where)
            INTO TABLE @<table>.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      IF <table> IS NOT ASSIGNED.
        RETURN.
      ENDIF.

      " every task of a request gets its text - the logon language wins,
      " another language only fills a text that is still empty
      LOOP AT <table> INTO <line>.

        UNASSIGN <value>.
        ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <line> TO <value>.
        IF <value> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        DATA(lv_trkorr) = CONV sxco_transport( <value> ).

        UNASSIGN <value>.
        ASSIGN COMPONENT 'LANGU' OF STRUCTURE <line> TO <value>.
        DATA(lv_logon_langu) = xsdbool( <value> IS ASSIGNED AND <value> = sy-langu ).

        UNASSIGN <value>.
        ASSIGN COMPONENT 'AS4TEXT' OF STRUCTURE <line> TO <value>.
        IF <value> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        LOOP AT mt_data ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE transport = lv_trkorr.
          IF <ls_data>-short_description IS INITIAL OR lv_logon_langu = abap_true.
            <ls_data>-short_description = <value>.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD set_mandt.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.

    ASSIGN ir_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      FIELD-SYMBOLS <row> TYPE any.
      UNASSIGN <row>.
      ASSIGN COMPONENT `MANDT` OF STRUCTURE <line> TO <row>.
      IF <row> IS ASSIGNED.

        TRY.
            <row> = sy-mandt.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD bal_search.

    IF check_abap_cloud( ).
      " Cloud: use CL_BALI_LOG_FILTER + CL_BALI_LOG_DB
      DATA lo_filter TYPE REF TO object.
      DATA lo_db     TYPE REF TO object.
      DATA lt_logs   TYPE STANDARD TABLE OF REF TO object.
      DATA lv_class  TYPE string.

      TRY.
          DATA(lv_obj_f) = COND string( WHEN object IS NOT INITIAL THEN object ELSE `` ).
          DATA(lv_sub_f) = COND string( WHEN subobject IS NOT INITIAL THEN subobject ELSE `` ).
          DATA(lv_id_f)  = COND string( WHEN id IS NOT INITIAL THEN id ELSE `` ).
          lo_filter = bal_cloud_build_filter( object    = lv_obj_f
                                              subobject = lv_sub_f
                                              id        = lv_id_f ).

          IF date_from IS NOT INITIAL OR date_to IS NOT INITIAL.
            DATA(lv_from) = COND d( WHEN date_from IS NOT INITIAL THEN date_from ELSE '19000101' ).
            DATA(lv_to)   = COND d( WHEN date_to IS NOT INITIAL THEN date_to ELSE sy-datum ).
            CALL METHOD lo_filter->(`SET_CREATE_DATE`)
              EXPORTING
                from_date = lv_from
                to_date   = lv_to.
          ENDIF.

          lv_class = `CL_BALI_LOG_DB`.
          CALL METHOD (lv_class)=>(`GET_INSTANCE`)
            RECEIVING
              db_handler = lo_db.

          CALL METHOD lo_db->(`LOAD_LOGS_VIA_FILTER`)
            EXPORTING
              filter           = lo_filter
              read_only_header = abap_true
            RECEIVING
              log_table        = lt_logs.

          LOOP AT lt_logs INTO DATA(lo_log).
            DATA(ls_hdr_c) = VALUE ty_s_bal_header( ).
            TRY.
                DATA lo_header TYPE REF TO object.
                CALL METHOD lo_log->(`GET_HEADER`)
                  RECEIVING
                    header = lo_header.
                CALL METHOD lo_header->(`GET_OBJECT`)
                  RECEIVING
                    object = ls_hdr_c-object.
                CALL METHOD lo_header->(`GET_SUBOBJECT`)
                  RECEIVING
                    subobject = ls_hdr_c-subobject.
                CALL METHOD lo_header->(`GET_EXTERNAL_ID`)
                  RECEIVING
                    external_id = ls_hdr_c-external_id.
              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
            INSERT ls_hdr_c INTO TABLE result.
          ENDLOOP.

        CATCH cx_root INTO DATA(lx_search_c).
          " a failed search is no empty result - the on-premise branch and
          " bal_read raise the same way
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_search_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use BAL_DB_SEARCH
    DATA lv_fm      TYPE string.
    DATA lr_filter  TYPE REF TO data.
    DATA lr_headers TYPE REF TO data.
    FIELD-SYMBOLS <filter>  TYPE any.
    " generic: BALHDR_T is read by bal_std_load_handles the same way
    FIELD-SYMBOLS <headers> TYPE ANY TABLE.
    FIELD-SYMBOLS <header>  TYPE any.
    FIELD-SYMBOLS <comp>    TYPE any.

    TRY.
        lr_filter = bal_std_build_filter( object    = object
                                          subobject = subobject
                                          id        = id ).
        ASSIGN lr_filter->* TO <filter>.

        IF date_from IS NOT INITIAL OR date_to IS NOT INITIAL.
          bal_std_filter_add( EXPORTING comp   = `ALDATE`
                                        value  = COND d( WHEN date_from IS NOT INITIAL THEN date_from ELSE '19000101' )
                                        option = `BT`
                                        high   = COND d( WHEN date_to IS NOT INITIAL THEN date_to ELSE sy-datum )
                              CHANGING  filter = <filter> ).
        ENDIF.
        bal_std_filter_add( EXPORTING comp   = `ALUSER`
                                      value  = user
                            CHANGING  filter = <filter> ).

        CREATE DATA lr_headers TYPE ('BALHDR_T').
        ASSIGN lr_headers->* TO <headers>.

        lv_fm = `BAL_DB_SEARCH`.
        CALL FUNCTION lv_fm
          EXPORTING
            i_s_log_filter = <filter>
          IMPORTING
            e_t_log_header = <headers>
          EXCEPTIONS
            error_message  = 2
            OTHERS         = 1.
        IF sy-subrc <> 0.
          " LOG_NOT_FOUND: nothing matches the filter
          RETURN.
        ENDIF.

        LOOP AT <headers> ASSIGNING <header>.
          DATA(ls_hdr) = VALUE ty_s_bal_header( ).
          ASSIGN COMPONENT `LOG_HANDLE` OF STRUCTURE <header> TO <comp>.
          IF sy-subrc = 0. ls_hdr-log_handle = <comp>. ENDIF.
          ASSIGN COMPONENT `OBJECT` OF STRUCTURE <header> TO <comp>.
          IF sy-subrc = 0. ls_hdr-object = <comp>. ENDIF.
          ASSIGN COMPONENT `SUBOBJECT` OF STRUCTURE <header> TO <comp>.
          IF sy-subrc = 0. ls_hdr-subobject = <comp>. ENDIF.
          ASSIGN COMPONENT `EXTNUMBER` OF STRUCTURE <header> TO <comp>.
          IF sy-subrc = 0. ls_hdr-external_id = <comp>. ENDIF.
          ASSIGN COMPONENT `ALDATE` OF STRUCTURE <header> TO <comp>.
          IF sy-subrc = 0. ls_hdr-log_date = <comp>. ENDIF.
          ASSIGN COMPONENT `ALTIME` OF STRUCTURE <header> TO <comp>.
          IF sy-subrc = 0. ls_hdr-log_time = <comp>. ENDIF.
          ASSIGN COMPONENT `ALUSER` OF STRUCTURE <header> TO <comp>.
          IF sy-subrc = 0. ls_hdr-user = <comp>. ENDIF.
          INSERT ls_hdr INTO TABLE result.
        ENDLOOP.

      CATCH cx_root INTO DATA(lx_search).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_search.
    ENDTRY.

  ENDMETHOD.

  METHOD bal_read_latest.

    DATA(lt_msgs) = bal_read( object    = object
                              subobject = subobject
                              id        = id ).
    IF lt_msgs IS NOT INITIAL.
      result = lt_msgs[ lines( lt_msgs ) ].
    ENDIF.

  ENDMETHOD.

  METHOD bal_delete_before.

    DATA(lv_cutoff) = CONV d( sy-datum - days ).

    IF check_abap_cloud( ).
      " Cloud: use CL_BALI_LOG_DB to delete via filter
      DATA lo_filter_c TYPE REF TO object.
      DATA lo_db_c     TYPE REF TO object.
      DATA lt_logs_c   TYPE STANDARD TABLE OF REF TO object.
      DATA lv_cls      TYPE string.

      TRY.
          DATA(lv_sub_c) = COND string( WHEN subobject IS NOT INITIAL THEN subobject ELSE `` ).
          lo_filter_c = bal_cloud_build_filter( object    = object
                                                subobject = lv_sub_c
                                                id        = `` ).

          CALL METHOD lo_filter_c->(`SET_CREATE_DATE`)
            EXPORTING
              from_date = CONV d( '19000101' )
              to_date   = lv_cutoff.

          lv_cls = `CL_BALI_LOG_DB`.
          CALL METHOD (lv_cls)=>(`GET_INSTANCE`)
            RECEIVING
              db_handler = lo_db_c.

          CALL METHOD lo_db_c->(`LOAD_LOGS_VIA_FILTER`)
            EXPORTING
              filter    = lo_filter_c
            RECEIVING
              log_table = lt_logs_c.

          LOOP AT lt_logs_c INTO DATA(lo_log_c).
            CALL METHOD lo_db_c->(`DELETE_LOG`)
              EXPORTING
                log = lo_log_c.
          ENDLOOP.

          COMMIT WORK AND WAIT.

        CATCH cx_root INTO DATA(lx_delete_c).
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_delete_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use BAL_DB_DELETE
    DATA lv_fm     TYPE string.
    DATA lr_filter TYPE REF TO data.
    FIELD-SYMBOLS <filter> TYPE any.

    " the filter always restricted OBJECT - an initial object matched no
    " log. bal_std_build_filter leaves an initial value out, which would
    " delete the old logs of EVERY object instead
    IF object IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lr_filter = bal_std_build_filter( object    = object
                                          subobject = subobject
                                          id        = `` ).
        ASSIGN lr_filter->* TO <filter>.

        bal_std_filter_add( EXPORTING comp   = `ALDATE`
                                      value  = CONV d( '19000101' )
                                      option = `BT`
                                      high   = lv_cutoff
                            CHANGING  filter = <filter> ).

        lv_fm = `BAL_DB_DELETE`.
        CALL FUNCTION lv_fm
          EXPORTING
            i_s_log_filter = <filter>
          EXCEPTIONS
            error_message  = 2
            OTHERS         = 1.
        " sy-subrc <> 0: no log older than the cutoff - nothing to delete
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.

      CATCH cx_root INTO DATA(lx_delete).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_delete.
    ENDTRY.

  ENDMETHOD.

  METHOD bal_read_by_type.

    DATA(lt_all) = bal_read( object    = object
                             subobject = subobject
                             id        = id ).

    LOOP AT lt_all INTO DATA(ls_msg)
         WHERE type = msg_type.
      INSERT ls_msg INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD bal_count.

    DATA(lt_msgs) = bal_read( object    = object
                              subobject = subobject
                              id        = id ).
    result = lines( lt_msgs ).

  ENDMETHOD.

  METHOD bal_read.

    IF check_abap_cloud( ).

      " Load the persisted logs (incl. items) via the released filter API and map
      " each item back to the framework's z2ui5_cl_util=>ty_s_msg structure with full metadata.
      TYPES:
        BEGIN OF ty_item,
          log_item_number TYPE i,
          item            TYPE REF TO object,
        END OF ty_item.
      DATA lt_items    TYPE STANDARD TABLE OF ty_item.
      DATA lo_filter   TYPE REF TO object.
      DATA lo_db       TYPE REF TO object.
      DATA lt_logs     TYPE STANDARD TABLE OF REF TO object.
      DATA lv_text     TYPE string.
      DATA lv_class    TYPE string.
      DATA lv_severity TYPE c LENGTH 1.
      DATA lv_msgid    TYPE string.
      DATA lv_msgno    TYPE string.
      DATA lv_msgv1    TYPE string.
      DATA lv_msgv2    TYPE string.
      DATA lv_msgv3    TYPE string.
      DATA lv_msgv4    TYPE string.

      TRY.
          lo_filter = bal_cloud_build_filter( object    = object
                                              subobject = subobject
                                              id        = id ).

          lv_class = `CL_BALI_LOG_DB`.
          CALL METHOD (lv_class)=>(`GET_INSTANCE`)
            RECEIVING
              db_handler = lo_db.

          CALL METHOD lo_db->(`LOAD_LOGS_VIA_FILTER`)
            EXPORTING
              filter    = lo_filter
            RECEIVING
              log_table = lt_logs.

          LOOP AT lt_logs INTO DATA(lo_log).

            lt_items = VALUE #( ).
            CALL METHOD lo_log->(`GET_ALL_ITEMS`)
              RECEIVING
                item_table = lt_items.

            LOOP AT lt_items INTO DATA(ls_item).
              IF ls_item-item IS NOT BOUND.
                CONTINUE.
              ENDIF.

              DATA(ls_msg) = VALUE ty_s_msg( ).

              lv_text = ``.
              CALL METHOD ls_item-item->(`GET_MESSAGE_TEXT`)
                RECEIVING
                  message_text = lv_text.
              ls_msg-text = lv_text.

              TRY.
                  CALL METHOD ls_item-item->(`GET_SEVERITY`)
                    RECEIVING
                      severity = lv_severity.
                  ls_msg-type = lv_severity.
                CATCH cx_root ##NO_HANDLER.
              ENDTRY.

              TRY.
                  CALL METHOD ls_item-item->(`GET_MESSAGE_ID`)
                    RECEIVING
                      id = lv_msgid.
                  ls_msg-id = lv_msgid.
                CATCH cx_root ##NO_HANDLER.
              ENDTRY.

              TRY.
                  CALL METHOD ls_item-item->(`GET_MESSAGE_NUMBER`)
                    RECEIVING
                      number = lv_msgno.
                  ls_msg-no = lv_msgno.
                CATCH cx_root ##NO_HANDLER.
              ENDTRY.

              TRY.
                  CALL METHOD ls_item-item->(`GET_MESSAGE_VARIABLE_1`)
                    RECEIVING
                      variable_1 = lv_msgv1.
                  ls_msg-v1 = lv_msgv1.
                  CALL METHOD ls_item-item->(`GET_MESSAGE_VARIABLE_2`)
                    RECEIVING
                      variable_2 = lv_msgv2.
                  ls_msg-v2 = lv_msgv2.
                  CALL METHOD ls_item-item->(`GET_MESSAGE_VARIABLE_3`)
                    RECEIVING
                      variable_3 = lv_msgv3.
                  ls_msg-v3 = lv_msgv3.
                  CALL METHOD ls_item-item->(`GET_MESSAGE_VARIABLE_4`)
                    RECEIVING
                      variable_4 = lv_msgv4.
                  ls_msg-v4 = lv_msgv4.
                CATCH cx_root ##NO_HANDLER.
              ENDTRY.

              INSERT ls_msg INTO TABLE result.
            ENDLOOP.

          ENDLOOP.

        CATCH cx_root INTO DATA(lx_read_c).
          " a failed read is no empty log - as in the on-premise branch
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_read_c.
      ENDTRY.

    ELSE.

      DATA lv_fm      TYPE string.
      DATA lr_handles TYPE REF TO data.
      DATA lr_single  TYPE REF TO data.
      DATA lr_msgh    TYPE REF TO data.
      DATA lr_msg     TYPE REF TO data.
      " generic: the table kinds of the DDIC types BAL_T_LOGH / BAL_T_MSGH
      " are not ours to assume - a SORTED vs STANDARD mismatch is a dump
      FIELD-SYMBOLS <handles> TYPE ANY TABLE.
      FIELD-SYMBOLS <handle>  TYPE any.
      FIELD-SYMBOLS <single>  TYPE ANY TABLE.
      FIELD-SYMBOLS <msgh>    TYPE ANY TABLE.
      FIELD-SYMBOLS <mh>      TYPE any.
      FIELD-SYMBOLS <msg>     TYPE any.

      TRY.
          lr_handles = bal_std_load_handles( object    = object
                                             subobject = subobject
                                             id        = id ).
          IF lr_handles IS NOT BOUND.
            RETURN.
          ENDIF.
          ASSIGN lr_handles->* TO <handles>.

          CREATE DATA lr_single TYPE ('BAL_T_LOGH').
          ASSIGN lr_single->* TO <single>.
          CREATE DATA lr_msgh TYPE ('BAL_T_MSGH').
          ASSIGN lr_msgh->* TO <msgh>.
          CREATE DATA lr_msg TYPE ('BAL_S_MSG').
          ASSIGN lr_msg->* TO <msg>.

          LOOP AT <handles> ASSIGNING <handle>.

            CLEAR <single>.
            INSERT <handle> INTO TABLE <single>.

            CLEAR <msgh>.
            lv_fm = `BAL_GLB_SEARCH_MSG`.
            CALL FUNCTION lv_fm
              EXPORTING
                i_t_log_handle = <single>
              IMPORTING
                e_t_msg_handle = <msgh>
              EXCEPTIONS
                error_message  = 2
                OTHERS         = 1.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            LOOP AT <msgh> ASSIGNING <mh>.
              CLEAR <msg>.
              lv_fm = `BAL_LOG_MSG_READ`.
              CALL FUNCTION lv_fm
                EXPORTING
                  i_s_msg_handle = <mh>
                IMPORTING
                  e_s_msg        = <msg>
                EXCEPTIONS
                  error_message  = 2
                  OTHERS         = 1.
              IF sy-subrc = 0.
                INSERT bal_std_map_msg( <msg> ) INTO TABLE result.
              ENDIF.
            ENDLOOP.

          ENDLOOP.

        CATCH cx_root INTO DATA(lx_read).
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_read.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD bal_create.

    IF check_abap_cloud( ).

      " ABAP Cloud: released Business Application Log API (cl_bali_*).
      " All access is dynamic so this class still compiles on lower releases.
      " The class names are kept in variables - a string literal inside the
      " dynamic component selector ( '...' )=>( '...' ) is not valid ABAP.
      " Returning parameter names follow the released API (header/log/db_handler).
      DATA lo_header TYPE REF TO object.
      DATA lo_log    TYPE REF TO object.
      DATA lo_db     TYPE REF TO object.
      DATA lv_class  TYPE string.

      TRY.
          lv_class = `CL_BALI_HEADER_SETTER`.
          CALL METHOD (lv_class)=>(`CREATE`)
            EXPORTING
              object      = object
              subobject   = subobject
              external_id = id
            RECEIVING
              header      = lo_header.

          lv_class = `CL_BALI_LOG`.
          CALL METHOD (lv_class)=>(`CREATE`)
            RECEIVING
              log = lo_log.

          CALL METHOD lo_log->(`SET_HEADER`)
            EXPORTING
              header = lo_header.

          bal_cloud_add_items( log   = lo_log
                               t_log = t_log ).

          lv_class = `CL_BALI_LOG_DB`.
          CALL METHOD (lv_class)=>(`GET_INSTANCE`)
            RECEIVING
              db_handler = lo_db.

          CALL METHOD lo_db->(`SAVE_LOG`)
            EXPORTING
              log = lo_log.

          COMMIT WORK AND WAIT.

        CATCH cx_root INTO DATA(lx_create_c).
          " the log was not written - the caller must not see a success
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_create_c.
      ENDTRY.

    ELSE.

      " Standard ABAP / on-premise: classic Business Application Log function modules.
      DATA lv_fm      TYPE string.
      DATA lr_log     TYPE REF TO data.
      DATA lr_handle  TYPE REF TO data.
      DATA lr_handles TYPE REF TO data.
      FIELD-SYMBOLS <log>     TYPE any.
      FIELD-SYMBOLS <handle>  TYPE any.
      FIELD-SYMBOLS <handles> TYPE ANY TABLE.
      FIELD-SYMBOLS <comp>    TYPE any.

      TRY.
          CREATE DATA lr_log TYPE ('BAL_S_LOG').
          ASSIGN lr_log->* TO <log>.
          ASSIGN COMPONENT `OBJECT` OF STRUCTURE <log> TO <comp>.
          <comp> = object.
          ASSIGN COMPONENT `SUBOBJECT` OF STRUCTURE <log> TO <comp>.
          <comp> = subobject.
          ASSIGN COMPONENT `EXTNUMBER` OF STRUCTURE <log> TO <comp>.
          <comp> = id.

          CREATE DATA lr_handle TYPE ('BALLOGHNDL').
          ASSIGN lr_handle->* TO <handle>.

          lv_fm = `BAL_LOG_CREATE`.
          CALL FUNCTION lv_fm
            EXPORTING
              i_s_log      = <log>
            IMPORTING
              e_log_handle = <handle>
            EXCEPTIONS
              error_message = 2
              OTHERS        = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |BAL_LOG_CREATE failed (sy-subrc { sy-subrc })|.
          ENDIF.

          bal_std_msg_add( handle = <handle>
                           t_log  = t_log ).

          CREATE DATA lr_handles TYPE ('BAL_T_LOGH').
          ASSIGN lr_handles->* TO <handles>.
          INSERT <handle> INTO TABLE <handles>.

          lv_fm = `BAL_DB_SAVE`.
          CALL FUNCTION lv_fm
            EXPORTING
              i_t_log_handle = <handles>
            EXCEPTIONS
              error_message  = 2
              OTHERS         = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |BAL_DB_SAVE failed (sy-subrc { sy-subrc })|.
          ENDIF.
          COMMIT WORK AND WAIT.

        CATCH cx_root INTO DATA(lx_create).
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_create.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD bal_update.

    IF check_abap_cloud( ).

      " Load the existing log and append items. If no log exists, create a new one.
      DATA lo_filter TYPE REF TO object.
      DATA lo_db     TYPE REF TO object.
      DATA lt_logs   TYPE STANDARD TABLE OF REF TO object.
      DATA lv_class  TYPE string.

      TRY.
          lo_filter = bal_cloud_build_filter( object    = object
                                              subobject = subobject
                                              id        = id ).

          lv_class = `CL_BALI_LOG_DB`.
          CALL METHOD (lv_class)=>(`GET_INSTANCE`)
            RECEIVING
              db_handler = lo_db.

          CALL METHOD lo_db->(`LOAD_LOGS_VIA_FILTER`)
            EXPORTING
              filter    = lo_filter
            RECEIVING
              log_table = lt_logs.

          IF lt_logs IS INITIAL.
            bal_create( object    = object
                        subobject = subobject
                        id        = id
                        t_log     = t_log ).
            RETURN.
          ENDIF.

          " Append to the first (most recent) log
          DATA(lo_log) = lt_logs[ 1 ].
          bal_cloud_add_items( log   = lo_log
                               t_log = t_log ).

          CALL METHOD lo_db->(`SAVE_LOG`)
            EXPORTING
              log = lo_log.

          COMMIT WORK AND WAIT.

        CATCH cx_root.
          " Fallback: create new log if update fails
          bal_create( object    = object
                      subobject = subobject
                      id        = id
                      t_log     = t_log ).
      ENDTRY.

    ELSE.

      " Append the given messages to an already persisted log; create a new one if none exists.
      DATA lv_fm      TYPE string.
      DATA lr_handles TYPE REF TO data.
      FIELD-SYMBOLS <handles> TYPE INDEX TABLE.
      FIELD-SYMBOLS <handle>  TYPE any.

      TRY.
          lr_handles = bal_std_load_handles( object    = object
                                             subobject = subobject
                                             id        = id ).
          IF lr_handles IS NOT BOUND.
            bal_create( object    = object
                        subobject = subobject
                        id        = id
                        t_log     = t_log ).
            RETURN.
          ENDIF.

          ASSIGN lr_handles->* TO <handles>.
          IF <handles> IS INITIAL.
            bal_create( object    = object
                        subobject = subobject
                        id        = id
                        t_log     = t_log ).
            RETURN.
          ENDIF.

          ASSIGN <handles>[ 1 ] TO <handle>.
          bal_std_msg_add( handle = <handle>
                           t_log  = t_log ).

          lv_fm = `BAL_DB_SAVE`.
          CALL FUNCTION lv_fm
            EXPORTING
              i_t_log_handle = <handles>
            EXCEPTIONS
              error_message  = 2
              OTHERS         = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |BAL_DB_SAVE failed (sy-subrc { sy-subrc })|.
          ENDIF.
          COMMIT WORK AND WAIT.

        CATCH cx_root INTO DATA(lx_update).
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_update.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD bal_delete.

    IF check_abap_cloud( ).

      DATA lo_filter TYPE REF TO object.
      DATA lo_db     TYPE REF TO object.
      DATA lt_logs   TYPE STANDARD TABLE OF REF TO object.
      DATA lv_class  TYPE string.

      TRY.
          lo_filter = bal_cloud_build_filter( object    = object
                                              subobject = subobject
                                              id        = id ).

          lv_class = `CL_BALI_LOG_DB`.
          CALL METHOD (lv_class)=>(`GET_INSTANCE`)
            RECEIVING
              db_handler = lo_db.

          CALL METHOD lo_db->(`LOAD_LOGS_VIA_FILTER`)
            EXPORTING
              filter    = lo_filter
            RECEIVING
              log_table = lt_logs.

          LOOP AT lt_logs INTO DATA(lo_log).
            CALL METHOD lo_db->(`DELETE_LOG`)
              EXPORTING
                log = lo_log.
          ENDLOOP.

          COMMIT WORK AND WAIT.

        CATCH cx_root INTO DATA(lx_delete_c).
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_delete_c.
      ENDTRY.

    ELSE.

      DATA lv_fm     TYPE string.
      DATA lr_filter TYPE REF TO data.
      FIELD-SYMBOLS <filter> TYPE any.

      TRY.
          lr_filter = bal_std_build_filter( object    = object
                                            subobject = subobject
                                            id        = id ).
          ASSIGN lr_filter->* TO <filter>.

          lv_fm = `BAL_DB_DELETE`.
          CALL FUNCTION lv_fm
            EXPORTING
              i_s_log_filter = <filter>
            EXCEPTIONS
              error_message  = 2
              OTHERS         = 1.
          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.
          ENDIF.

        CATCH cx_root INTO DATA(lx_delete).
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_delete.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD tr_get_objects.

    IF check_abap_cloud( ).
      " Cloud: use XCO_CP_CTS
      TRY.
          DATA lo_transport TYPE REF TO object.
          DATA lt_objects_c TYPE STANDARD TABLE OF REF TO object.
          DATA(lv_xco) = `XCO_CP_CTS`.
          DATA(lv_trkorr_c) = CONV string( trkorr ).

          CALL METHOD (lv_xco)=>(`TRANSPORT`)
            EXPORTING
              iv_transport = lv_trkorr_c
            RECEIVING
              ro_transport = lo_transport.

          DATA lo_objects_api TYPE REF TO object.
          CALL METHOD lo_transport->(`OBJECTS`)
            RECEIVING
              ro_objects = lo_objects_api.

          DATA lo_all TYPE REF TO object.
          CALL METHOD lo_objects_api->(`ALL`)
            RECEIVING
              ro_all = lo_all.

          CALL METHOD lo_all->(`GET`)
            RECEIVING
              rt_objects = lt_objects_c.

          LOOP AT lt_objects_c INTO DATA(lo_obj).
            DATA ls_obj_c TYPE ty_s_tr_object.
            CLEAR ls_obj_c.
            TRY.
                CALL METHOD lo_obj->(`GET_PGMID`)
                  RECEIVING
                    rv_pgmid = ls_obj_c-pgmid.
                CALL METHOD lo_obj->(`GET_TYPE`)
                  RECEIVING
                    rv_type = ls_obj_c-object.
                CALL METHOD lo_obj->(`GET_NAME`)
                  RECEIVING
                    rv_name = ls_obj_c-obj_name.
              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
            INSERT ls_obj_c INTO TABLE result.
          ENDLOOP.

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use TR_GET_OBJECTS_OF_REQ_AN_TASKS
    DATA lr_objects TYPE REF TO data.
    DATA lr_header  TYPE REF TO data.
    FIELD-SYMBOLS <objects> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <object>  TYPE any.
    FIELD-SYMBOLS <header>  TYPE any.
    FIELD-SYMBOLS <comp>    TYPE any.
    DATA lv_fm TYPE string.

    TRY.
        CREATE DATA lr_objects TYPE STANDARD TABLE OF (`E071`).
        ASSIGN lr_objects->* TO <objects>.

        CREATE DATA lr_header TYPE (`TRWBO_REQUEST_HEADER`).
        ASSIGN lr_header->* TO <header>.
        ASSIGN COMPONENT `TRKORR` OF STRUCTURE <header> TO <comp>.
        <comp> = trkorr.

        lv_fm = `TR_GET_OBJECTS_OF_REQ_AN_TASKS`.
        CALL FUNCTION lv_fm
          EXPORTING
            is_request_header = <header>
          IMPORTING
            et_objects        = <objects>
          EXCEPTIONS
            error_message     = 1
            OTHERS            = 1.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        LOOP AT <objects> ASSIGNING <object>.
          DATA(ls_obj) = VALUE ty_s_tr_object( ).
          ASSIGN COMPONENT `PGMID` OF STRUCTURE <object> TO <comp>.
          IF sy-subrc = 0. ls_obj-pgmid = <comp>. ENDIF.
          ASSIGN COMPONENT `OBJECT` OF STRUCTURE <object> TO <comp>.
          IF sy-subrc = 0. ls_obj-object = <comp>. ENDIF.
          ASSIGN COMPONENT `OBJ_NAME` OF STRUCTURE <object> TO <comp>.
          IF sy-subrc = 0. ls_obj-obj_name = <comp>. ENDIF.
          INSERT ls_obj INTO TABLE result.
        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_get_user_requests.

    IF check_abap_cloud( ).
      " Cloud: use XCO_CP_CTS transport filter
      TRY.
          DATA(lv_xco) = `XCO_CP_CTS`.
          DATA lo_filter_tr  TYPE REF TO object.
          DATA lo_status_f   TYPE REF TO object.
          DATA lo_owner_f    TYPE REF TO object.
          DATA lt_transports TYPE STANDARD TABLE OF REF TO object.

          DATA(lv_user_c) = CONV string( COND #( WHEN user IS NOT INITIAL THEN user ELSE sy-uname ) ).

          " Get modifiable transports for user
          CALL METHOD (lv_xco)=>(`TRANSPORTS`)
            RECEIVING
              ro_transports = lo_filter_tr.

          DATA lo_where TYPE REF TO object.
          CALL METHOD lo_filter_tr->(`ALL`)
            RECEIVING
              ro_all = lo_where.

          CALL METHOD lo_where->(`GET`)
            RECEIVING
              rt_transports = lt_transports.

          LOOP AT lt_transports INTO DATA(lo_tr).
            DATA ls_req_c TYPE ty_s_tr_request.
            CLEAR ls_req_c.
            TRY.
                DATA lo_props TYPE REF TO object.
                CALL METHOD lo_tr->(`PROPERTIES`)
                  RECEIVING
                    ro_properties = lo_props.
                DATA ls_prop TYPE REF TO data.
                CALL METHOD lo_props->(`GET`)
                  RECEIVING
                    rs_properties = ls_prop.
                FIELD-SYMBOLS <prop> TYPE any.
                FIELD-SYMBOLS <pcomp> TYPE any.
                ASSIGN ls_prop->* TO <prop>.
                ASSIGN COMPONENT `OWNER` OF STRUCTURE <prop> TO <pcomp>.
                IF sy-subrc = 0. ls_req_c-owner = <pcomp>. ENDIF.
                IF ls_req_c-owner <> lv_user_c.
                  CONTINUE.
                ENDIF.
                ASSIGN COMPONENT `SHORT_DESCRIPTION` OF STRUCTURE <prop> TO <pcomp>.
                IF sy-subrc = 0. ls_req_c-description = <pcomp>. ENDIF.
                ASSIGN COMPONENT `STATUS` OF STRUCTURE <prop> TO <pcomp>.
                IF sy-subrc = 0. ls_req_c-status = <pcomp>. ENDIF.
                ASSIGN COMPONENT `TYPE` OF STRUCTURE <prop> TO <pcomp>.
                IF sy-subrc = 0. ls_req_c-type = <pcomp>. ENDIF.

                DATA lv_tr_value TYPE string.
                CALL METHOD lo_tr->(`GET_VALUE`)
                  RECEIVING
                    rv_value = lv_tr_value.
                ls_req_c-trkorr = lv_tr_value.

              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
            IF ls_req_c-trkorr IS NOT INITIAL.
              INSERT ls_req_c INTO TABLE result.
            ENDIF.
          ENDLOOP.

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use dynamic SELECT from E070/E07T
    DATA lv_user  TYPE c LENGTH 12.
    DATA lv_type  TYPE c LENGTH 1.
    DATA lr_data  TYPE REF TO data.
    FIELD-SYMBOLS <tab>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <comp>  TYPE any.

    TRY.
        lv_user = user.
        lv_type = request_type.

        DATA(lv_tab1) = `E070`.
        DATA(lv_tab2) = `E07T`.
        DATA(lv_where) = |AS4USER = { filter_sql_quote( lv_user ) } AND TRSTATUS IN ('D','L')|.

        " First read transports from E070
        DATA(lt_comp) = rtti_get_t_attri_by_table_name( lv_tab1 ).
        DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
        DATA(lo_table) = cl_abap_tabledescr=>create( lo_struct ).
        CREATE DATA lr_data TYPE HANDLE lo_table.
        ASSIGN lr_data->* TO <tab>.

        SELECT trkorr, as4user, trstatus, trfunction
          FROM (lv_tab1)
          WHERE (lv_where)
          INTO CORRESPONDING FIELDS OF TABLE @<tab>.

        LOOP AT <tab> ASSIGNING <row>.
          DATA(ls_req) = VALUE ty_s_tr_request( ).
          ASSIGN COMPONENT `TRKORR` OF STRUCTURE <row> TO <comp>.
          IF sy-subrc = 0. ls_req-trkorr = <comp>. ENDIF.
          ASSIGN COMPONENT `AS4USER` OF STRUCTURE <row> TO <comp>.
          IF sy-subrc = 0. ls_req-owner = <comp>. ENDIF.
          ASSIGN COMPONENT `TRSTATUS` OF STRUCTURE <row> TO <comp>.
          IF sy-subrc = 0. ls_req-status = <comp>. ENDIF.
          ASSIGN COMPONENT `TRFUNCTION` OF STRUCTURE <row> TO <comp>.
          IF sy-subrc = 0. ls_req-type = <comp>. ENDIF.

          IF lv_type IS NOT INITIAL AND ls_req-type <> lv_type.
            CONTINUE.
          ENDIF.

          " Get description from E07T
          ls_req-description = tr_get_description( ls_req-trkorr ).
          INSERT ls_req INTO TABLE result.
        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_get_description.

    IF check_abap_cloud( ).
      " Cloud: use XCO_CP_CTS
      TRY.
          DATA lo_tr_d TYPE REF TO object.
          DATA(lv_xco_d) = `XCO_CP_CTS`.
          CALL METHOD (lv_xco_d)=>(`TRANSPORT`)
            EXPORTING
              iv_transport = CONV string( trkorr )
            RECEIVING
              ro_transport = lo_tr_d.
          DATA lo_props_d TYPE REF TO object.
          CALL METHOD lo_tr_d->(`PROPERTIES`)
            RECEIVING
              ro_properties = lo_props_d.
          CALL METHOD lo_props_d->(`GET_SHORT_DESCRIPTION`)
            RECEIVING
              rv_short_description = result.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard: dynamic SELECT from E07T
    DATA lv_trkorr TYPE c LENGTH 20.
    lv_trkorr = trkorr.

    TRY.
        DATA(lv_tab) = `E07T`.
        DATA(lv_where) = |TRKORR = { filter_sql_quote( lv_trkorr ) } AND LANGU = { filter_sql_quote( sy-langu ) }|.

        SELECT SINGLE as4text
          FROM (lv_tab)
          WHERE (lv_where)
          INTO @result.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_is_released.

    IF check_abap_cloud( ).
      " Cloud: use XCO_CP_CTS
      TRY.
          DATA lo_tr_r TYPE REF TO object.
          DATA(lv_xco_r) = `XCO_CP_CTS`.
          CALL METHOD (lv_xco_r)=>(`TRANSPORT`)
            EXPORTING
              iv_transport = CONV string( trkorr )
            RECEIVING
              ro_transport = lo_tr_r.
          DATA lo_props_r TYPE REF TO object.
          CALL METHOD lo_tr_r->(`PROPERTIES`)
            RECEIVING
              ro_properties = lo_props_r.
          DATA lv_status_c TYPE string.
          CALL METHOD lo_props_r->(`GET_STATUS`)
            RECEIVING
              rv_status = lv_status_c.
          result = xsdbool( lv_status_c = `RELEASED` OR lv_status_c = `R` ).
        CATCH cx_root.
          result = abap_false.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard: dynamic SELECT from E070
    DATA lv_trkorr TYPE c LENGTH 20.
    DATA lv_status TYPE c LENGTH 1.
    lv_trkorr = trkorr.

    TRY.
        DATA(lv_tab) = `E070`.
        DATA(lv_where) = |TRKORR = { filter_sql_quote( lv_trkorr ) }|.

        SELECT SINGLE trstatus
          FROM (lv_tab)
          WHERE (lv_where)
          INTO @lv_status.
        result = xsdbool( lv_status = `R` ).
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_add_object.

    DATA lv_fm       TYPE string.
    DATA lv_trkorr   TYPE c LENGTH 20.
    DATA lv_pgmid    TYPE c LENGTH 4.
    DATA lv_object   TYPE c LENGTH 4.
    DATA lv_obj_name TYPE c LENGTH 120.

    lv_trkorr   = trkorr.
    lv_pgmid    = pgmid.
    lv_object   = object.
    lv_obj_name = obj_name.

    TRY.
        lv_fm = `TR_ORDER_CHOICE_CORRECTION`.
        CALL FUNCTION lv_fm
          EXPORTING
            iv_category    = lv_pgmid
            iv_object      = lv_object
            iv_obj_name    = lv_obj_name
            iv_order       = lv_trkorr
          EXCEPTIONS
            error_message  = 1
            OTHERS         = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `TR_ADD_OBJECT failed`.
        ENDIF.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_create.

    " Create an empty transport request (default type `T` = transport of copies).
    " Uses the released class CL_ADT_CTS_MANAGEMENT, available on standard ABAP
    " (>= 7.51) and ABAP Cloud. Called dynamically so the source stays compilable
    " on all targets - on releases where the class is missing a clean
    " z2ui5_cx_util_error is raised instead of a short dump.
    TRY.

        DATA lr_header TYPE REF TO data.
        FIELD-SYMBOLS <header> TYPE any.
        FIELD-SYMBOLS <trkorr> TYPE any.
        DATA lv_class TYPE string.

        CREATE DATA lr_header TYPE (`TRWBO_REQUEST_HEADER`).
        ASSIGN lr_header->* TO <header>.

        lv_class = `CL_ADT_CTS_MANAGEMENT`.
        CALL METHOD (lv_class)=>(`CREATE_EMPTY_REQUEST`)
          EXPORTING
            iv_type           = type
            iv_text           = text
            iv_target         = target
          IMPORTING
            es_request_header = <header>.

        ASSIGN COMPONENT `TRKORR` OF STRUCTURE <header> TO <trkorr>.
        result = <trkorr>.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_release.

    " Release a transport request via the released CTS REST API
    " (CL_CTS_REST_API_FACTORY). Works on standard ABAP and ABAP Cloud.
    TRY.

        DATA lo_api   TYPE REF TO object.
        DATA lv_class TYPE string.

        lv_class = `CL_CTS_REST_API_FACTORY`.
        CALL METHOD (lv_class)=>(`CREATE_INSTANCE`)
          RECEIVING
            result = lo_api.

        CALL METHOD lo_api->(`RELEASE`)
          EXPORTING
            iv_trkorr       = trkorr
            iv_ignore_locks = ignore_locks.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_copy_objects.

    " Copying objects between requests relies on the classic transport
    " functions (TR_COPY_COMM) which are not released on ABAP Cloud.
    IF check_abap_cloud( ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `tr_copy_objects is not supported on ABAP Cloud`.
    ENDIF.

    " Copy all objects/commands of a source request (and its tasks) into a
    " target request via the classic transport functions. Called dynamically
    " so the statement stays compilable on ABAP Cloud as well, even though the
    " function modules only exist on-premise.
    TRY.

        DATA lr_headers TYPE REF TO data.
        FIELD-SYMBOLS <headers> TYPE ANY TABLE.
        FIELD-SYMBOLS <header>  TYPE any.
        FIELD-SYMBOLS <trkorr>  TYPE any.
        FIELD-SYMBOLS <strkorr> TYPE any.
        DATA lv_fm TYPE string.

        CREATE DATA lr_headers TYPE (`TRWBO_REQUEST_HEADERS`).
        ASSIGN lr_headers->* TO <headers>.

        lv_fm = `TR_READ_REQUEST_WITH_TASKS`.
        CALL FUNCTION lv_fm
          EXPORTING
            iv_trkorr          = source
          IMPORTING
            et_request_headers = <headers>
          EXCEPTIONS
            invalid_input      = 1
            error_message      = 2
            OTHERS             = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `TR_READ_REQUEST_WITH_TASKS failed`.
        ENDIF.

        LOOP AT <headers> ASSIGNING <header>.

          ASSIGN COMPONENT `TRKORR`  OF STRUCTURE <header> TO <trkorr>.
          ASSIGN COMPONENT `STRKORR` OF STRUCTURE <header> TO <strkorr>.
          IF <trkorr> <> source AND <strkorr> <> source.
            CONTINUE.
          ENDIF.

          lv_fm = `TR_COPY_COMM`.
          CALL FUNCTION lv_fm
            EXPORTING
              wi_dialog                = abap_false
              wi_trkorr_from           = <trkorr>
              wi_trkorr_to             = destination
              wi_without_documentation = abap_false
            EXCEPTIONS
              error_message            = 1
              OTHERS                   = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = `TR_COPY_COMM failed`.
          ENDIF.

        ENDLOOP.

      CATCH zabaputil_cx_util_error INTO DATA(lx_known).
        RAISE EXCEPTION lx_known.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_import.

    " Importing transports via TMS (TMS_MGR_*) is not available on ABAP Cloud.
    IF check_abap_cloud( ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `tr_import is not supported on ABAP Cloud`.
    ENDIF.

    " Import a transport request into a target system via TMS. The target
    " system may be given as `SYSTEM` or `SYSTEM.CLIENT`; an explicit client
    " parameter takes precedence, otherwise the current client is used.
    TRY.

        DATA lv_system  TYPE c LENGTH 8.
        DATA lv_client  TYPE c LENGTH 3.
        DATA lv_retcode TYPE c LENGTH 4.
        DATA lr_exc     TYPE REF TO data.
        FIELD-SYMBOLS <exc> TYPE any.
        DATA lv_fm TYPE string.

        SPLIT target_system AT `.` INTO lv_system lv_client.
        IF lv_client IS INITIAL.
          IF client IS NOT INITIAL.
            lv_client = client.
          ELSE.
            lv_client = sy-mandt.
          ENDIF.
        ENDIF.

        CREATE DATA lr_exc TYPE (`STMSCALERT`).
        ASSIGN lr_exc->* TO <exc>.

        lv_fm = `TMS_MGR_REFRESH_IMPORT_QUEUES`.
        CALL FUNCTION lv_fm
          EXPORTING
            iv_system    = lv_system
            iv_monitor   = abap_true
            iv_verbose   = abap_true
          IMPORTING
            es_exception = <exc>
          EXCEPTIONS
            error_message = 99
            OTHERS        = 99.
        IF sy-subrc = 99.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `TMS_MGR_REFRESH_IMPORT_QUEUES failed`.
        ENDIF.

        lv_fm = `TMS_MGR_IMPORT_TR_REQUEST`.
        CALL FUNCTION lv_fm
          EXPORTING
            iv_system                  = lv_system
            iv_request                 = trkorr
            iv_client                  = lv_client
            iv_ignore_cvers            = ignore_version
          IMPORTING
            ev_tp_ret_code             = lv_retcode
          EXCEPTIONS
            read_config_failed         = 1
            table_of_requests_is_empty = 2
            error_message              = 3
            OTHERS                     = 3.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `TMS_MGR_IMPORT_TR_REQUEST failed`.
        ENDIF.

        result = lv_retcode.

      CATCH zabaputil_cx_util_error INTO DATA(lx_known).
        RAISE EXCEPTION lx_known.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_check_status.

    " Reading the transport log (TR_READ_GLOBAL_INFO_OF_REQUEST) is not
    " available on ABAP Cloud.
    IF check_abap_cloud( ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `tr_check_status is not supported on ABAP Cloud`.
    ENDIF.

    " Read the import status (imported flag + return code) of a request in a
    " given system via the classic transport log API.
    TRY.

        DATA lr_settings TYPE REF TO data.
        DATA lr_cofile   TYPE REF TO data.
        DATA lr_sysline  TYPE REF TO data.
        FIELD-SYMBOLS <settings> TYPE any.
        FIELD-SYMBOLS <systems>  TYPE ANY TABLE.
        FIELD-SYMBOLS <sysline>  TYPE any.
        FIELD-SYMBOLS <cofile>   TYPE any.
        FIELD-SYMBOLS <comp>     TYPE any.
        DATA lv_fm TYPE string.

        CREATE DATA lr_settings TYPE (`CTSLG_SETTINGS`).
        ASSIGN lr_settings->* TO <settings>.
        ASSIGN COMPONENT `SYSTEMS` OF STRUCTURE <settings> TO <systems>.

        CREATE DATA lr_sysline LIKE LINE OF <systems>.
        ASSIGN lr_sysline->* TO <sysline>.
        <sysline> = system.
        INSERT <sysline> INTO TABLE <systems>.

        CREATE DATA lr_cofile TYPE (`CTSLG_COFILE`).
        ASSIGN lr_cofile->* TO <cofile>.

        lv_fm = `TR_READ_GLOBAL_INFO_OF_REQUEST`.
        CALL FUNCTION lv_fm
          EXPORTING
            iv_trkorr   = trkorr
            is_settings = <settings>
          IMPORTING
            es_cofile   = <cofile>.

        ASSIGN COMPONENT `EXISTS` OF STRUCTURE <cofile> TO <comp>.
        IF <comp> = abap_false.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `request does not exist in target system`.
        ENDIF.

        ASSIGN COMPONENT `IMPORTED` OF STRUCTURE <cofile> TO <comp>.
        imported = <comp>.
        ASSIGN COMPONENT `RC` OF STRUCTURE <cofile> TO <comp>.
        rc = <comp>.

      CATCH zabaputil_cx_util_error INTO DATA(lx_known).
        RAISE EXCEPTION lx_known.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD bal_cloud_add_items.

    DATA lo_item  TYPE REF TO object.
    DATA lv_msgty TYPE c LENGTH 1.
    DATA lv_class TYPE string.

    LOOP AT t_log INTO DATA(ls_log).

      lv_msgty = ls_log-type.

      IF ls_log-id IS NOT INITIAL AND ls_log-no IS NOT INITIAL.
        lv_class = `CL_BALI_MESSAGE_SETTER`.
        CALL METHOD (lv_class)=>(`CREATE`)
          EXPORTING
            severity   = lv_msgty
            id         = ls_log-id
            number     = ls_log-no
            variable_1 = ls_log-v1
            variable_2 = ls_log-v2
            variable_3 = ls_log-v3
            variable_4 = ls_log-v4
          RECEIVING
            message    = lo_item.
      ELSE.
        lv_class = `CL_BALI_FREE_TEXT_SETTER`.
        CALL METHOD (lv_class)=>(`CREATE`)
          EXPORTING
            severity  = lv_msgty
            text      = ls_log-text
          RECEIVING
            free_text = lo_item.
      ENDIF.

      CALL METHOD log->(`ADD_ITEM`)
        EXPORTING
          item = lo_item.

    ENDLOOP.

  ENDMETHOD.

  METHOD bal_cloud_build_filter.

    DATA lo_filter TYPE REF TO object.
    DATA lv_class  TYPE string.

    lv_class = `CL_BALI_LOG_FILTER`.
    CALL METHOD (lv_class)=>(`CREATE`)
      RECEIVING
        filter = lo_filter.

    CALL METHOD lo_filter->(`SET_DESCRIPTOR`)
      EXPORTING
        object      = object
        subobject   = subobject
        external_id = id.

    result = lo_filter.

  ENDMETHOD.

  METHOD bal_std_msg_add.

    DATA lv_fm    TYPE string.
    DATA lr_msg   TYPE REF TO data.
    DATA lv_msgty TYPE c LENGTH 1.
    DATA lv_text  TYPE c LENGTH 200.
    FIELD-SYMBOLS <msg>  TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.

    LOOP AT t_log INTO DATA(ls_log).

      IF ls_log-id IS NOT INITIAL AND ls_log-no IS NOT INITIAL.

        CREATE DATA lr_msg TYPE ('BAL_S_MSG').
        ASSIGN lr_msg->* TO <msg>.
        ASSIGN COMPONENT `MSGTY` OF STRUCTURE <msg> TO <comp>.
        <comp> = ls_log-type.
        ASSIGN COMPONENT `MSGID` OF STRUCTURE <msg> TO <comp>.
        <comp> = ls_log-id.
        ASSIGN COMPONENT `MSGNO` OF STRUCTURE <msg> TO <comp>.
        <comp> = ls_log-no.
        ASSIGN COMPONENT `MSGV1` OF STRUCTURE <msg> TO <comp>.
        <comp> = ls_log-v1.
        ASSIGN COMPONENT `MSGV2` OF STRUCTURE <msg> TO <comp>.
        <comp> = ls_log-v2.
        ASSIGN COMPONENT `MSGV3` OF STRUCTURE <msg> TO <comp>.
        <comp> = ls_log-v3.
        ASSIGN COMPONENT `MSGV4` OF STRUCTURE <msg> TO <comp>.
        <comp> = ls_log-v4.

        lv_fm = `BAL_LOG_MSG_ADD`.
        CALL FUNCTION lv_fm
          EXPORTING
            i_log_handle = handle
            i_s_msg      = <msg>
          EXCEPTIONS
            error_message = 2
            OTHERS        = 1.

      ELSE.

        lv_msgty = ls_log-type.
        lv_text  = ls_log-text.
        lv_fm    = `BAL_LOG_MSG_ADD_FREE_TEXT`.
        CALL FUNCTION lv_fm
          EXPORTING
            i_log_handle = handle
            i_msgty      = lv_msgty
            i_text       = lv_text
          EXCEPTIONS
            error_message = 2
            OTHERS        = 1.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD bal_std_load_handles.

    DATA lv_fm      TYPE string.
    DATA lr_filter  TYPE REF TO data.
    DATA lr_headers TYPE REF TO data.
    FIELD-SYMBOLS <filter>  TYPE any.
    " generic: BALHDR_T and BAL_T_LOGH are typed STANDARD in bal_search and
    " bal_read - with SORTED here one of the two sides could only dump
    FIELD-SYMBOLS <headers> TYPE ANY TABLE.
    FIELD-SYMBOLS <handles> TYPE ANY TABLE.

    lr_filter = bal_std_build_filter( object    = object
                                      subobject = subobject
                                      id        = id ).
    ASSIGN lr_filter->* TO <filter>.

    CREATE DATA lr_headers TYPE ('BALHDR_T').
    ASSIGN lr_headers->* TO <headers>.

    lv_fm = `BAL_DB_SEARCH`.
    CALL FUNCTION lv_fm
      EXPORTING
        i_s_log_filter = <filter>
      IMPORTING
        e_t_log_header = <headers>
      EXCEPTIONS
        error_message  = 2
        OTHERS         = 1.
    IF sy-subrc <> 0 OR <headers> IS INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA result TYPE ('BAL_T_LOGH').
    ASSIGN result->* TO <handles>.

    lv_fm = `BAL_DB_LOAD`.
    CALL FUNCTION lv_fm
      EXPORTING
        i_t_log_header = <headers>
      IMPORTING
        e_t_log_handle = <handles>
      EXCEPTIONS
        error_message  = 2
        OTHERS         = 1.

  ENDMETHOD.

  METHOD bal_std_build_filter.

    FIELD-SYMBOLS <filter> TYPE any.

    CREATE DATA result TYPE ('BAL_S_LFIL').
    ASSIGN result->* TO <filter>.

    bal_std_filter_add( EXPORTING comp   = `OBJECT`
                                  value  = object
                        CHANGING  filter = <filter> ).
    bal_std_filter_add( EXPORTING comp   = `SUBOBJECT`
                                  value  = subobject
                        CHANGING  filter = <filter> ).
    bal_std_filter_add( EXPORTING comp   = `EXTNUMBER`
                                  value  = id
                        CHANGING  filter = <filter> ).

  ENDMETHOD.

  METHOD bal_std_filter_add.

    DATA lr_line TYPE REF TO data.
    FIELD-SYMBOLS <range> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <comp>  TYPE any.

    IF value IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT comp OF STRUCTURE filter TO <range>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE DATA lr_line LIKE LINE OF <range>.
    ASSIGN lr_line->* TO <line>.
    ASSIGN COMPONENT `SIGN` OF STRUCTURE <line> TO <comp>.
    <comp> = `I`.
    ASSIGN COMPONENT `OPTION` OF STRUCTURE <line> TO <comp>.
    <comp> = option.
    ASSIGN COMPONENT `LOW` OF STRUCTURE <line> TO <comp>.
    <comp> = value.
    IF high IS NOT INITIAL.
      ASSIGN COMPONENT `HIGH` OF STRUCTURE <line> TO <comp>.
      <comp> = high.
    ENDIF.
    INSERT <line> INTO TABLE <range>.

  ENDMETHOD.

  METHOD bal_std_map_msg.

    DATA lv_fm   TYPE string.
    DATA lv_text TYPE c LENGTH 200.
    FIELD-SYMBOLS <comp> TYPE any.

    ASSIGN COMPONENT `MSGTY` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-type = <comp>.
    ENDIF.
    ASSIGN COMPONENT `MSGID` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-id = <comp>.
    ENDIF.
    ASSIGN COMPONENT `MSGNO` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-no = <comp>.
    ENDIF.
    ASSIGN COMPONENT `MSGV1` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-v1 = <comp>.
    ENDIF.
    ASSIGN COMPONENT `MSGV2` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-v2 = <comp>.
    ENDIF.
    ASSIGN COMPONENT `MSGV3` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-v3 = <comp>.
    ENDIF.
    ASSIGN COMPONENT `MSGV4` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-v4 = <comp>.
    ENDIF.
    ASSIGN COMPONENT `TIME_STMP` OF STRUCTURE msg TO <comp>.
    IF sy-subrc = 0.
      result-timestampl = <comp>.
    ENDIF.

    TRY.
        lv_fm = `MESSAGE_TEXT_BUILD`.
        CALL FUNCTION lv_fm
          EXPORTING
            msgid               = result-id
            msgnr               = result-no
            msgv1               = result-v1
            msgv2               = result-v2
            msgv3               = result-v3
            msgv4               = result-v4
          IMPORTING
            message_text_output = lv_text.
        result-text = lv_text.
      CATCH cx_root.
        result-text = result-v1.
    ENDTRY.

  ENDMETHOD.

  METHOD lock_set.

    result = lock_call_function( val     = val
                                 t_param = t_param ).

  ENDMETHOD.

  METHOD lock_set_wait.

    DATA lv_seconds TYPE i.
    DATA lv_fm      TYPE string.

    DATA(lv_remaining) = retries.

    WHILE lv_remaining > 0.
      result = lock_set( val     = val
                          t_param = t_param ).
      IF result = abap_true.
        RETURN.
      ENDIF.
      lv_remaining = lv_remaining - 1.
      IF lv_remaining > 0.
        " WAIT UP TO is no statement of ABAP Cloud, where this class has to
        " activate as well: the pause is a function module called by name.
        " ENQUE_SLEEP takes whole seconds - delay_ms is rounded up, so a
        " caller never waits less than it asked for. Where the module cannot
        " be called the tries follow without a pause.
        lv_seconds = ( delay_ms + 999 ) DIV 1000.
        TRY.
            lv_fm = `ENQUE_SLEEP`.
            CALL FUNCTION lv_fm
              EXPORTING
                seconds = lv_seconds.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD lock_is_locked.

    DATA lv_subrc TYPE i.
    DATA lv_set   TYPE abap_bool.

    " Try to set the lock - FOREIGN_LOCK means someone else holds it. Any
    " other failure (a wrong module name, a parameter it does not have, a
    " SYSTEM_FAILURE) says nothing about the lock and is raised - it used
    " to answer "locked"
    lock_call_function( EXPORTING val     = val
                                  t_param = t_param
                        IMPORTING subrc   = lv_subrc
                        RECEIVING result  = lv_set ).
    IF lv_set = abap_true.
      " We got it - release immediately
      lock_delete( val     = val
                   t_param = t_param ).
      result = abap_false.
    ELSEIF lv_subrc = 1.
      result = abap_true.
    ELSE.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |LOCK_IS_LOCKED: { val } failed (sy-subrc { lv_subrc })|.
    ENDIF.

  ENDMETHOD.

  METHOD lock_get_owner.

    TRY.
        DATA(lt_locks) = lock_read( ).

        " Build the lock argument from params for matching
        DATA(lv_arg) = ``.
        LOOP AT t_param INTO DATA(ls_param).
          lv_arg = lv_arg && ls_param-value.
        ENDLOOP.

        DATA(lv_name) = c_trim_upper( val ).
        lv_name = replace( val = lv_name sub = `ENQUEUE_` with = `` ).

        " SEQG3-GNAME (lock_object) holds the TABLE that is locked, not the
        " name of the lock object - the tables of a lock object are in
        " DD26E. Where DD26E cannot be read the names are compared as before
        TYPES ty_c30 TYPE c LENGTH 30.
        DATA lt_tabname TYPE STANDARD TABLE OF ty_c30 WITH EMPTY KEY.
        DATA lv_lock_object TYPE ty_c30.
        lv_lock_object = lv_name.
        TRY.
            DATA(lv_dd26e) = `DD26E`.
            SELECT tabname FROM (lv_dd26e)
              WHERE viewname = @lv_lock_object
              INTO TABLE @lt_tabname.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

        LOOP AT lt_locks INTO DATA(ls_lock).
          IF lt_tabname IS INITIAL.
            IF ls_lock-lock_object NS lv_name.
              CONTINUE.
            ENDIF.
          ELSEIF NOT line_exists( lt_tabname[ table_line = ls_lock-lock_object ] ).
            CONTINUE.
          ENDIF.
          IF lv_arg IS INITIAL OR ls_lock-argument CS lv_arg.
            result = ls_lock-user.
            RETURN.
          ENDIF.
        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD lock_get_dequeue_by_enqueue.

    result = replace( val  = c_trim_upper( val )
                      sub  = `ENQUEUE_`
                      with = `DEQUEUE_` ).

  ENDMETHOD.

  METHOD lock_read.

    DATA lr_enq TYPE REF TO data.
    FIELD-SYMBOLS <lt_enq>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_enq>   TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.
    DATA lv_client     TYPE c LENGTH 3.
    DATA lv_name       TYPE c LENGTH 30.
    DATA lv_uname      TYPE c LENGTH 12.
    DATA lt_param      TYPE abap_func_parmbind_tab.
    DATA ls_param      TYPE abap_func_parmbind.
    DATA lt_exception  TYPE abap_func_excpbind_tab.
    DATA ls_exception  TYPE abap_func_excpbind.
    DATA lv_function   TYPE string.
    DATA ls_lock       TYPE ty_s_lock.

    TRY.
        CREATE DATA lr_enq TYPE STANDARD TABLE OF (`SEQG3`).
        ASSIGN lr_enq->* TO <lt_enq>.

        IF client IS INITIAL.
          lv_client = context_get_sy( )-mandt.
        ELSE.
          lv_client = client.
        ENDIF.
        lv_name  = lock_object.
        lv_uname = user.

        ls_param-name = `GCLIENT`.
        ls_param-kind = abap_func_exporting.
        ls_param-value = REF #( lv_client ).
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `GNAME`.
        ls_param-value = REF #( lv_name ).
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `GUNAME`.
        ls_param-value = REF #( lv_uname ).
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `ENQ`.
        ls_param-kind = abap_func_tables.
        ls_param-value = REF #( <lt_enq> ).
        INSERT ls_param INTO TABLE lt_param.

        ls_exception-name  = `OTHERS`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.

        lv_function = `ENQUEUE_READ`.
        CALL FUNCTION lv_function
          PARAMETER-TABLE lt_param
          EXCEPTION-TABLE lt_exception.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = `LOCK_READ_FAILED`.
        ENDIF.

        LOOP AT <lt_enq> ASSIGNING <ls_enq>.
          CLEAR ls_lock.
          ASSIGN COMPONENT `GNAME` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-lock_object = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GARG` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-argument = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GUNAME` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-user = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GMODE` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-mode = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GCLIENT` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-client = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GTDATE` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-date = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GTTIME` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-time = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GUSR` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-owner = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT `GUSRVB` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            ls_lock-owner_vb = <lv_value>.
          ENDIF.
          INSERT ls_lock INTO TABLE result.
        ENDLOOP.

      CATCH zabaputil_cx_util_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = lx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lock_delete.

    result = lock_call_function( val     = lock_get_dequeue_by_enqueue( val )
                                 t_param = t_param ).

  ENDMETHOD.

  METHOD lock_delete_entries.

    DATA lr_enq TYPE REF TO data.
    FIELD-SYMBOLS <lt_enq>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_enq>   TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.
    DATA lr_row        TYPE REF TO data.
    DATA ls_lock       TYPE ty_s_lock.
    DATA lv_check_upd  TYPE i.
    DATA lv_subrc      TYPE sy-subrc.
    DATA lt_param      TYPE abap_func_parmbind_tab.
    DATA ls_param      TYPE abap_func_parmbind.
    DATA lt_exception  TYPE abap_func_excpbind_tab.
    DATA ls_exception  TYPE abap_func_excpbind.
    DATA lv_function   TYPE string.

    TRY.
        CREATE DATA lr_enq TYPE STANDARD TABLE OF (`SEQG3`).
        ASSIGN lr_enq->* TO <lt_enq>.
        CREATE DATA lr_row TYPE (`SEQG3`).
        ASSIGN lr_row->* TO <ls_enq>.

        LOOP AT t_lock INTO ls_lock.
          CLEAR <ls_enq>.
          ASSIGN COMPONENT `GNAME` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = ls_lock-lock_object.
          ENDIF.
          ASSIGN COMPONENT `GARG` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = ls_lock-argument.
          ENDIF.
          ASSIGN COMPONENT `GUNAME` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = ls_lock-user.
          ENDIF.
          ASSIGN COMPONENT `GMODE` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = ls_lock-mode.
          ENDIF.
          ASSIGN COMPONENT `GCLIENT` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = ls_lock-client.
          ENDIF.
          ASSIGN COMPONENT `GUSR` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = ls_lock-owner.
          ENDIF.
          ASSIGN COMPONENT `GUSRVB` OF STRUCTURE <ls_enq> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = ls_lock-owner_vb.
          ENDIF.
          INSERT <ls_enq> INTO TABLE <lt_enq>.
        ENDLOOP.

        IF <lt_enq> IS INITIAL.
          result = abap_true.
          RETURN.
        ENDIF.

        ls_param-name = `CHECK_UPD_REQUESTS`.
        ls_param-kind = abap_func_exporting.
        ls_param-value = REF #( lv_check_upd ).
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `SUBRC`.
        ls_param-kind = abap_func_importing.
        ls_param-value = REF #( lv_subrc ).
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `ENQ`.
        ls_param-kind = abap_func_tables.
        ls_param-value = REF #( <lt_enq> ).
        INSERT ls_param INTO TABLE lt_param.

        ls_exception-name  = `OTHERS`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.

        lv_function = `ENQUE_DELETE`.
        CALL FUNCTION lv_function
          PARAMETER-TABLE lt_param
          EXCEPTION-TABLE lt_exception.

        result = xsdbool( sy-subrc = 0 AND lv_subrc = 0 ).

      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD auth_check.

    DATA lv_object   TYPE c LENGTH 10.
    DATA lv_field    TYPE c LENGTH 10.
    DATA lv_value    TYPE c LENGTH 40.
    DATA lv_activity TYPE c LENGTH 2.

    lv_object   = object.
    lv_field    = field.
    lv_value    = value.
    lv_activity = activity.

    AUTHORITY-CHECK OBJECT lv_object
      ID lv_field FIELD lv_value
      ID 'ACTVT' FIELD lv_activity.

    result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD text_get.

    DATA lv_msgid TYPE c LENGTH 20.
    DATA lv_msgno TYPE n LENGTH 3.
    DATA lv_msgv1 TYPE c LENGTH 50.
    DATA lv_msgv2 TYPE c LENGTH 50.
    DATA lv_msgv3 TYPE c LENGTH 50.
    DATA lv_msgv4 TYPE c LENGTH 50.
    DATA lv_text  TYPE c LENGTH 200.

    lv_msgid = msgid.
    lv_msgno = msgno.
    lv_msgv1 = v1.
    lv_msgv2 = v2.
    lv_msgv3 = v3.
    lv_msgv4 = v4.

    TRY.
        DATA(lv_fm) = `MESSAGE_TEXT_BUILD`.
        CALL FUNCTION lv_fm
          EXPORTING
            msgid               = lv_msgid
            msgnr               = lv_msgno
            msgv1               = lv_msgv1
            msgv2               = lv_msgv2
            msgv3               = lv_msgv3
            msgv4               = lv_msgv4
          IMPORTING
            message_text_output = lv_text.
        result = lv_text.
      CATCH cx_root.
        result = |{ lv_msgid } { lv_msgno }: { lv_msgv1 } { lv_msgv2 } { lv_msgv3 } { lv_msgv4 }|.
    ENDTRY.

  ENDMETHOD.

  METHOD mail_send.

    IF check_abap_cloud( ).
      " Cloud: use CL_BCS_MAIL_MESSAGE (released cloud mail API)
      DATA lo_mail_c TYPE REF TO object.
      DATA lv_cls_c  TYPE string.

      TRY.
          lv_cls_c = `CL_BCS_MAIL_MESSAGE`.
          CALL METHOD (lv_cls_c)=>(`CREATE_INSTANCE`)
            RECEIVING
              result = lo_mail_c.

          CALL METHOD lo_mail_c->(`SET_SENDER`)
            EXPORTING
              iv_address = context_get_user_tech( ) && `@placeholder.local`.

          CALL METHOD lo_mail_c->(`ADD_RECIPIENT`)
            EXPORTING
              iv_address = to.

          CALL METHOD lo_mail_c->(`SET_SUBJECT`)
            EXPORTING
              iv_subject = subject.

          IF html = abap_true.
            CALL METHOD lo_mail_c->(`SET_MAIN`)
              EXPORTING
                iv_content_type = `text/html`
                iv_content_text = body.
          ELSE.
            CALL METHOD lo_mail_c->(`SET_MAIN`)
              EXPORTING
                iv_content_type = `text/plain`
                iv_content_text = body.
          ENDIF.

          CALL METHOD lo_mail_c->(`SEND`)
            RECEIVING
              result = result.

        CATCH cx_root.
          result = abap_false.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use CL_BCS
    DATA lo_mail      TYPE REF TO object.
    DATA lo_sender    TYPE REF TO object.
    DATA lo_recipient TYPE REF TO object.
    DATA lo_doc       TYPE REF TO object.
    DATA lr_body      TYPE REF TO data.
    DATA lr_line      TYPE REF TO data.
    DATA lv_class     TYPE string.
    DATA lv_subject   TYPE c LENGTH 50.
    DATA lv_type      TYPE c LENGTH 3.
    DATA lv_address   TYPE c LENGTH 241.
    FIELD-SYMBOLS <body>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <field> TYPE any.

    lv_subject = subject.
    lv_address = to.
    lv_type    = COND #( WHEN html = abap_true THEN `HTM` ELSE `RAW` ).

    TRY.
        " Create BCS instance
        lv_class = `CL_BCS`.
        CALL METHOD (lv_class)=>(`CREATE_PERSISTENT`)
          RECEIVING
            result = lo_mail.

        " Sender
        lv_class = `CL_SAPUSER_BCS`.
        CALL METHOD (lv_class)=>(`CREATE`)
          EXPORTING
            i_user = sy-uname
          RECEIVING
            result = lo_sender.
        CALL METHOD lo_mail->(`SET_SENDER`)
          EXPORTING
            i_sender = lo_sender.

        " Recipient
        lv_class = `CL_CAM_ADDRESS_BCS`.
        CALL METHOD (lv_class)=>(`CREATE_INTERNET_ADDRESS`)
          EXPORTING
            i_address_string = lv_address
          RECEIVING
            result           = lo_recipient.
        CALL METHOD lo_mail->(`ADD_RECIPIENT`)
          EXPORTING
            i_recipient = lo_recipient.

        " Build body text table
        CREATE DATA lr_body TYPE (`BCSY_TEXT`).
        ASSIGN lr_body->* TO <body>.
        CREATE DATA lr_line TYPE (`SOLI`).
        ASSIGN lr_line->* TO <line>.

        DATA(lt_lines) = c_split( val = body sep = cv_char_util_newline ).
        LOOP AT lt_lines INTO DATA(lv_body_line).
          ASSIGN COMPONENT `LINE` OF STRUCTURE <line> TO <field>.
          <field> = lv_body_line.
          INSERT <line> INTO TABLE <body>.
        ENDLOOP.

        " Create document
        lv_class = `CL_DOCUMENT_BCS`.
        CALL METHOD (lv_class)=>(`CREATE_DOCUMENT`)
          EXPORTING
            i_type    = lv_type
            i_text    = <body>
            i_subject = lv_subject
          RECEIVING
            result    = lo_doc.

        CALL METHOD lo_mail->(`SET_DOCUMENT`)
          EXPORTING
            i_document = lo_doc.
        CALL METHOD lo_mail->(`SET_SEND_IMMEDIATELY`)
          EXPORTING
            i_send_immediately = abap_true.

        CALL METHOD lo_mail->(`SEND`)
          RECEIVING
            result = result.
        COMMIT WORK AND WAIT.

      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD job_submit_report.

    IF check_abap_cloud( ).
      " Cloud: Application Jobs have a different architecture (job catalog + templates).
      " Direct report submission is not available. Raise informative exception.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `job_submit_report: On ABAP Cloud use CL_APJ_RT_API with a registered job catalog entry instead`.
    ENDIF.

    " Standard ABAP: JOB_OPEN / JOB_SUBMIT / JOB_CLOSE
    DATA lv_fm       TYPE string.
    DATA lv_jobname  TYPE c LENGTH 32.
    DATA lv_jobcount TYPE c LENGTH 8.
    DATA lv_report   TYPE c LENGTH 40.
    DATA lv_variant  TYPE c LENGTH 14.

    lv_report = report.
    lv_variant = variant.
    lv_jobname = COND #( WHEN job_name IS NOT INITIAL THEN job_name
                         ELSE |Z2UI5_{ sy-datum }{ sy-uzeit }| ).

    TRY.
        lv_fm = `JOB_OPEN`.
        CALL FUNCTION lv_fm
          EXPORTING
            jobname  = lv_jobname
          IMPORTING
            jobcount = lv_jobcount
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `JOB_OPEN failed`.
        ENDIF.

        lv_fm = `JOB_SUBMIT`.
        CALL FUNCTION lv_fm
          EXPORTING
            authcknam = sy-uname
            jobcount  = lv_jobcount
            jobname   = lv_jobname
            report    = lv_report
            variant   = lv_variant
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `JOB_SUBMIT failed`.
        ENDIF.

        lv_fm = `JOB_CLOSE`.
        IF start_immediate = abap_true.
          CALL FUNCTION lv_fm
            EXPORTING
              jobcount  = lv_jobcount
              jobname   = lv_jobname
              strtimmed = abap_true
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
        ELSE.
          CALL FUNCTION lv_fm
            EXPORTING
              jobcount  = lv_jobcount
              jobname   = lv_jobname
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
        ENDIF.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `JOB_CLOSE failed`.
        ENDIF.

        result = lv_jobname.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD numrange_get_next.

    DATA lv_object  TYPE c LENGTH 10.
    DATA lv_nr_sub  TYPE c LENGTH 2.
    DATA lv_number  TYPE c LENGTH 20.

    lv_object = object.
    lv_nr_sub = subobject.

    TRY.
        IF check_abap_cloud( ).
          " Cloud: use CL_NUMBERRANGE_RUNTIME
          DATA(lv_cls) = `CL_NUMBERRANGE_RUNTIME`.
          CALL METHOD (lv_cls)=>(`NUMBER_GET`)
            EXPORTING
              nr_range_nr = lv_nr_sub
              object      = lv_object
            IMPORTING
              number      = lv_number.
        ELSE.
          " Standard: use NUMBER_GET_NEXT FM
          DATA(lv_fm) = `NUMBER_GET_NEXT`.
          CALL FUNCTION lv_fm
            EXPORTING
              nr_range_nr = lv_nr_sub
              object      = lv_object
            IMPORTING
              number      = lv_number
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |NUMBER_GET_NEXT failed for { lv_object }/{ lv_nr_sub }|.
          ENDIF.
        ENDIF.

        result = lv_number.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD changdoc_read.

    IF check_abap_cloud( ).
      " Cloud: use released CDS view I_ChangeDocument
      TRY.
          DATA(lv_cds) = `I_CHANGEDOCUMENTITEM`.
          " the values quoted - a quote inside them would end the literal
          DATA(lv_where_c) = |OBJECTCLASS = { filter_sql_quote( objectclass ) } AND OBJECTID = { filter_sql_quote( objectid ) }|.
          IF date_from IS NOT INITIAL.
            lv_where_c = |{ lv_where_c } AND CREATIONDATE >= '{ date_from }'|.
          ENDIF.
          IF date_to IS NOT INITIAL AND date_to <> '99991231'.
            lv_where_c = |{ lv_where_c } AND CREATIONDATE <= '{ date_to }'|.
          ENDIF.

          FIELD-SYMBOLS <cds_tab> TYPE STANDARD TABLE.
          FIELD-SYMBOLS <cds_row> TYPE any.
          FIELD-SYMBOLS <cds_fld> TYPE any.
          DATA lr_cds_tab TYPE REF TO data.

          DATA(lt_comp_c) = rtti_get_t_attri_by_table_name( lv_cds ).
          DATA(lo_struct_c) = cl_abap_structdescr=>create( lt_comp_c ).
          DATA(lo_table_c) = cl_abap_tabledescr=>create( lo_struct_c ).
          CREATE DATA lr_cds_tab TYPE HANDLE lo_table_c.
          ASSIGN lr_cds_tab->* TO <cds_tab>.

          SELECT *
            FROM (lv_cds)
            WHERE (lv_where_c)
            INTO CORRESPONDING FIELDS OF TABLE @<cds_tab>.

          LOOP AT <cds_tab> ASSIGNING <cds_row>.
            DATA(ls_doc_c) = VALUE ty_s_changdoc( ).
            ASSIGN COMPONENT `CHANGEDOCUMENT` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-changenr = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CREATEDBYUSER` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-username = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CREATIONDATE` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-udate = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CREATIONTIME` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-utime = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `TRANSACTIONCODE` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-tcode = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CHNGEDOCITEMFIELDNAME` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-fieldname = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CHNGEDOCITEMNEWVALUE` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-new_value = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CHNGEDOCITEMOLDVALUE` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-old_value = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CHANGEDOCITEMTABLENAME` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-tabname = <cds_fld>. ENDIF.
            ASSIGN COMPONENT `CHNGEDOCITEMCHNGIND` OF STRUCTURE <cds_row> TO <cds_fld>.
            IF sy-subrc = 0. ls_doc_c-chngind = <cds_fld>. ENDIF.
            INSERT ls_doc_c INTO TABLE result.
          ENDLOOP.

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use CHANGEDOCUMENT_READ_HEADERS/POSITIONS FMs
    DATA lv_fm         TYPE string.
    DATA lv_objectclas TYPE c LENGTH 15.
    DATA lv_objectid   TYPE c LENGTH 90.
    DATA lr_headers    TYPE REF TO data.
    DATA lr_positions  TYPE REF TO data.
    FIELD-SYMBOLS <headers>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <positions> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <hdr>       TYPE any.
    FIELD-SYMBOLS <pos>       TYPE any.
    FIELD-SYMBOLS <comp>      TYPE any.

    lv_objectclas = objectclass.
    lv_objectid   = objectid.

    TRY.
        CREATE DATA lr_headers TYPE STANDARD TABLE OF (`CDHDR`).
        CREATE DATA lr_positions TYPE STANDARD TABLE OF (`CDPOS`).
        ASSIGN lr_headers->* TO <headers>.
        ASSIGN lr_positions->* TO <positions>.

        lv_fm = `CHANGEDOCUMENT_READ_HEADERS`.
        CALL FUNCTION lv_fm
          EXPORTING
            objectclass    = lv_objectclas
            objectid       = lv_objectid
            date_of_change = date_from
            " the upper bound the cloud branch filters on as well
            date_until     = date_to
          TABLES
            i_cdhdr        = <headers>
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        LOOP AT <headers> ASSIGNING <hdr>.

          DATA(ls_doc) = VALUE ty_s_changdoc( ).
          ASSIGN COMPONENT `CHANGENR` OF STRUCTURE <hdr> TO <comp>.
          IF sy-subrc = 0. ls_doc-changenr = <comp>. ENDIF.
          ASSIGN COMPONENT `USERNAME` OF STRUCTURE <hdr> TO <comp>.
          IF sy-subrc = 0. ls_doc-username = <comp>. ENDIF.
          ASSIGN COMPONENT `UDATE` OF STRUCTURE <hdr> TO <comp>.
          IF sy-subrc = 0. ls_doc-udate = <comp>. ENDIF.
          ASSIGN COMPONENT `UTIME` OF STRUCTURE <hdr> TO <comp>.
          IF sy-subrc = 0. ls_doc-utime = <comp>. ENDIF.
          ASSIGN COMPONENT `TCODE` OF STRUCTURE <hdr> TO <comp>.
          IF sy-subrc = 0. ls_doc-tcode = <comp>. ENDIF.

          " Read positions for this change number
          CLEAR <positions>.
          lv_fm = `CHANGEDOCUMENT_READ_POSITIONS`.
          CALL FUNCTION lv_fm
            EXPORTING
              changenumber = ls_doc-changenr
            TABLES
              editpos      = <positions>
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.

          IF <positions> IS INITIAL.
            INSERT ls_doc INTO TABLE result.
          ELSE.
            LOOP AT <positions> ASSIGNING <pos>.
              DATA(ls_pos) = ls_doc.
              ASSIGN COMPONENT `FNAME` OF STRUCTURE <pos> TO <comp>.
              IF sy-subrc = 0. ls_pos-fieldname = <comp>. ENDIF.
              ASSIGN COMPONENT `VALUE_OLD` OF STRUCTURE <pos> TO <comp>.
              IF sy-subrc = 0. ls_pos-old_value = <comp>. ENDIF.
              ASSIGN COMPONENT `VALUE_NEW` OF STRUCTURE <pos> TO <comp>.
              IF sy-subrc = 0. ls_pos-new_value = <comp>. ENDIF.
              ASSIGN COMPONENT `TABNAME` OF STRUCTURE <pos> TO <comp>.
              IF sy-subrc = 0. ls_pos-tabname = <comp>. ENDIF.
              ASSIGN COMPONENT `CHNGIND` OF STRUCTURE <pos> TO <comp>.
              IF sy-subrc = 0. ls_pos-chngind = <comp>. ENDIF.
              INSERT ls_pos INTO TABLE result.
            ENDLOOP.
          ENDIF.

        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD source_get_method.

    DATA object TYPE REF TO object.
    FIELD-SYMBOLS <any> TYPE any.
    DATA lt_source       TYPE string_table.
    DATA lt_string       TYPE string_table.
    DATA lv_class        TYPE string.
    DATA lv_method       TYPE string.
    DATA xco_cp_abap     TYPE c LENGTH 11.
    DATA lv_name         TYPE c LENGTH 13.
    DATA lv_check_method LIKE abap_false.
    DATA lv_source       LIKE LINE OF lt_source.
    DATA lv_source_upper TYPE string.

    TRY.

        lv_class  = to_upper( iv_classname ).
        lv_method = to_upper( iv_methodname ).

        xco_cp_abap = `XCO_CP_ABAP`.
        CALL METHOD (xco_cp_abap)=>(`CLASS`)
          EXPORTING
            iv_name  = lv_class
          RECEIVING
            ro_class = object.

        ASSIGN object->(`IF_XCO_AO_CLASS~IMPLEMENTATION`) TO <any>.
        ASSERT sy-subrc = 0.
        object = <any>.

        CALL METHOD object->(`IF_XCO_CLAS_IMPLEMENTATION~METHOD`)
          EXPORTING
            iv_name   = lv_method
          RECEIVING
            ro_method = object.

        CALL METHOD object->(`IF_XCO_CLAS_I_METHOD~CONTENT`)
          RECEIVING
            ro_content = object.

        CALL METHOD object->(`IF_XCO_CLAS_I_METHOD_CONTENT~GET_SOURCE`)
          RECEIVING
            rt_source = result.

      CATCH cx_root.

        lv_name = `CL_OO_FACTORY`.
        CALL METHOD (lv_name)=>(`CREATE_INSTANCE`)
          RECEIVING
            result = object.

        CALL METHOD object->(`IF_OO_CLIF_SOURCE_FACTORY~CREATE_CLIF_SOURCE`)
          EXPORTING
            clif_name = lv_class
          RECEIVING
            result    = object.

        CALL METHOD object->(`IF_OO_CLIF_SOURCE~GET_SOURCE`)
          IMPORTING
            source = lt_source.

        lv_check_method = abap_false.

        LOOP AT lt_source INTO lv_source.

          lv_source_upper = to_upper( lv_source ).

          IF lv_source_upper CS `ENDMETHOD`.
            lv_check_method = abap_false.
          ENDIF.

          IF lv_source_upper CS |METHOD { lv_method }|.
            lv_check_method = abap_true.
            CONTINUE.
          ENDIF.

          IF lv_check_method = abap_true.
            INSERT lv_source INTO TABLE lt_string.
          ENDIF.

        ENDLOOP.

        result = lt_string.

    ENDTRY.

  ENDMETHOD.

  METHOD source_get_method2.

    DATA(lt_source) = source_get_method( iv_classname  = iv_classname
                                         iv_methodname = iv_methodname ).

    result = source_method_to_file( lt_source ).

  ENDMETHOD.

  METHOD source_get_file_types.

    DATA(lv_types) = |abap, abc, actionscript, ada, apache_conf, applescript, asciidoc, assembly_x86, autohotkey, batchfile, bro, c9search, c_cpp, cirru, clojure, cobol, coffee, coldfusion, csharp, css, curly, d, dart, diff, django, dockerfile, | &&
|dot, drools, eiffel, yaml, ejs, elixir, elm, erlang, forth, fortran, ftl, gcode, gherkin, gitignore, glsl, gobstones, golang, groovy, haml, handlebars, haskell, haskell_cabal, haxe, hjson, html, html_elixir, html_ruby, ini, io, jack, jade, java, ja| &&
      |vascri| &&
|pt, json, jsoniq, jsp, jsx, julia, kotlin, latex, lean, less, liquid, lisp, live_script, livescript, logiql, lsl, lua, luapage, lucene, makefile, markdown, mask, matlab, mavens_mate_log, maze, mel, mips_assembler, mipsassembler, mushcode, mysql, ni| &&
|x, nsis, objectivec, ocaml, pascal, perl, pgsql, php, plain_text, powershell, praat, prolog, properties, protobuf, python, r, razor, rdoc, rhtml, rst, ruby, rust, sass, scad, scala, scheme, scss, sh, sjs, smarty, snippets, soy_template, space, sql,| &&
      | sqlserver, stylus, svg, swift, swig, tcl, tex, text, textile, toml, tsx, twig, typescript, vala, vbscript, velocity, verilog, vhdl, wollok, xml, xquery, terraform, slim, redshift, red, puppet, php_laravel_blade, mixal, jssm, fsharp, edifact,| &&
      | csp, cssound_score, cssound_orchestra, cssound_document| ##NO_TEXT.
    SPLIT lv_types AT `,` INTO TABLE result.

  ENDMETHOD.

  METHOD source_method_to_file.

    LOOP AT it_source INTO DATA(lv_source).
      IF strlen( lv_source ) > 1.
        result = result && lv_source+1 && cv_char_util_newline.
      ELSE.
        result = result && cv_char_util_newline.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lock_call_function.

    DATA lt_param      TYPE abap_func_parmbind_tab.
    DATA ls_param      TYPE abap_func_parmbind.
    DATA ls_lock_param TYPE ty_s_lock_param.
    DATA lr_value      TYPE REF TO string.
    DATA lt_exception  TYPE abap_func_excpbind_tab.
    DATA ls_exception  TYPE abap_func_excpbind.
    DATA lv_function   TYPE string.

    TRY.
        LOOP AT t_param INTO ls_lock_param.
          ls_param-name = ls_lock_param-name.
          ls_param-kind = abap_func_exporting.
          CREATE DATA lr_value.
          lr_value->* = ls_lock_param-value.
          ls_param-value = lr_value.
          INSERT ls_param INTO TABLE lt_param.
        ENDLOOP.

        lv_function = c_trim_upper( val ).
        " every generated ENQUEUE_ module has FOREIGN_LOCK - the one
        " failure that means "someone else holds it"
        IF lv_function CP `ENQUEUE_*`.
          ls_exception-name  = `FOREIGN_LOCK`.
          ls_exception-value = 1.
          INSERT ls_exception INTO TABLE lt_exception.
        ENDIF.
        ls_exception-name  = `OTHERS`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.

        CALL FUNCTION lv_function
          PARAMETER-TABLE lt_param
          EXCEPTION-TABLE lt_exception.

        subrc = sy-subrc.
        result = xsdbool( subrc = 0 ).

      CATCH cx_root.
        subrc = 99.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD cal_get_weekday.

    " 1900-01-01 was a Monday, so the day distance modulo 7 yields the weekday
    DATA(lv_days) = date - CONV d( `19000101` ).
    result = lv_days MOD 7 + 1.

  ENDMETHOD.

  METHOD cal_is_weekend.

    result = xsdbool( cal_get_weekday( date ) >= 6 ).

  ENDMETHOD.

  METHOD cal_is_workday.

    IF calendar_id IS NOT INITIAL.
      x_raise( `cal_is_workday: factory calendar support is not yet implemented` ).
    ENDIF.

    result = xsdbool( cal_is_weekend( date ) = abap_false ).

  ENDMETHOD.

  METHOD cal_add_workdays.

    DATA(lv_remaining) = abs( days ).
    DATA(lv_step) = COND i( WHEN days < 0 THEN -1 ELSE 1 ).

    result = date.
    WHILE lv_remaining > 0.
      result = result + lv_step.
      IF cal_is_workday( date = result calendar_id = calendar_id ) = abap_true.
        lv_remaining = lv_remaining - 1.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD cal_count_workdays.

    " a date field takes any eight characters: with 20240230 or an initial
    " date the loop below never met date_to and ran forever, or for
    " hundreds of thousands of days
    IF check_is_date_valid( |{ date_from(4) }-{ date_from+4(2) }-{ date_from+6(2) }| ) = abap_false
        OR check_is_date_valid( |{ date_to(4) }-{ date_to+4(2) }-{ date_to+6(2) }| ) = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |CAL_COUNT_WORKDAYS: invalid date { date_from } or { date_to }|.
    ENDIF.

    DATA(lv_date) = date_from.
    IF date_to >= date_from.
      WHILE lv_date < date_to.
        lv_date = lv_date + 1.
        IF cal_is_workday( date = lv_date calendar_id = calendar_id ) = abap_true.
          result = result + 1.
        ENDIF.
      ENDWHILE.
    ELSE.
      WHILE lv_date > date_to.
        lv_date = lv_date - 1.
        IF cal_is_workday( date = lv_date calendar_id = calendar_id ) = abap_true.
          result = result + 1.
        ENDIF.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.

  METHOD conv_get_xlsx_by_itab.

*    DATA(write_access) = xco_cp_xlsx=>document->empty( )->write_access( ).
*    DATA(worksheet) = write_access->get_workbook( )->worksheet->at_position( 1 ).
*    DATA(selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).
*    worksheet->select( selection_pattern
*               )->row_stream(
*               )->operation->write_from( REF #( val )
*               )->execute( ).
*    result = write_access->get_file_content( ).

    " not implemented: the result was silently empty, which reads like
    " a success - the XCO version above is the template
    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = `CONV_GET_XLSX_BY_ITAB is not implemented`.

  ENDMETHOD.

  METHOD conv_get_itab_by_xlsx.

*    CLEAR result.
*    DATA(document) = xco_cp_xlsx=>document->for_file_content( val )->read_access( ).
*    DATA(sheet) = document->get_workbook( )->worksheet->at_position( 1 ).
*    DATA(pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).
*    sheet->select( pattern
*            )->row_stream(
*            )->operation->write_to( REF #( result )
*            )->set_value_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
*            )->execute( ).

    " not implemented: the result was silently empty, which reads like
    " a success - the XCO version above is the template
    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = `CONV_GET_ITAB_BY_XLSX is not implemented`.

  ENDMETHOD.

  METHOD zip_pack.

    DATA lo_zip TYPE REF TO object.

    TRY.

        CREATE OBJECT lo_zip TYPE ('CL_ABAP_ZIP').
        LOOP AT files INTO DATA(ls_file).
          CALL METHOD lo_zip->('ADD')
            EXPORTING
              name    = ls_file-name
              content = ls_file-content.
        ENDLOOP.
        CALL METHOD lo_zip->('SAVE')
          RECEIVING
            zip = result.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD zip_unpack.

    DATA lo_zip    TYPE REF TO object.
    DATA lv_name   TYPE string.
    DATA ls_result LIKE LINE OF result.

    FIELD-SYMBOLS <files> TYPE ANY TABLE.
    FIELD-SYMBOLS <file>  TYPE any.
    FIELD-SYMBOLS <name>  TYPE any.

    TRY.

        CREATE OBJECT lo_zip TYPE ('CL_ABAP_ZIP').
        CALL METHOD lo_zip->('LOAD')
          EXPORTING
            zip = val.

        ASSIGN lo_zip->('FILES') TO <files>.
        LOOP AT <files> ASSIGNING <file>.
          ASSIGN COMPONENT `NAME` OF STRUCTURE <file> TO <name>.
          lv_name = <name>.

          ls_result = VALUE #( name = lv_name ).
          CALL METHOD lo_zip->('GET')
            EXPORTING
              name    = lv_name
            IMPORTING
              content = ls_result-content.
          INSERT ls_result INTO TABLE result.
        ENDLOOP.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD c_escape_html.

    " the ampersand first: escaping it afterwards would hit the ones the
    " replacements below have just written. The common value carries none
    " of the five characters - skip the copies then
    result = val.
    IF result NA `&<>"'`.
      RETURN.
    ENDIF.
    result = replace( val = result sub = `&` with = `&amp;` occ = 0 ).
    result = replace( val = result sub = `<` with = `&lt;` occ = 0 ).
    result = replace( val = result sub = `>` with = `&gt;` occ = 0 ).
    result = replace( val = result sub = `"` with = `&quot;` occ = 0 ).
    result = replace( val = result sub = `'` with = `&#39;` occ = 0 ).

  ENDMETHOD.

  METHOD c_escape_json.
    " U+0000 included - RFC 8259 asks for every character below U+0020, and
    " a raw NUL out of a binary-to-text conversion made JSON.parse refuse
    " the whole document
    CONSTANTS lc_ctrl_hex TYPE string VALUE `0001020304050607080B0C0E0F101112131415161718191A1B1C1D1E1F`.

    IF gv_json_specials IS INITIAL.
      TRY.
          gv_json_ctrl = conv_get_string_by_xstring( CONV xstring( lc_ctrl_hex ) ).
        CATCH zabaputil_cx_util_error.
          " no codepage API here: the control characters pass as they did
          " before, the five common characters are escaped all the same -
          " an escape must not fail where it used to work
          CLEAR gv_json_ctrl.
      ENDTRY.
      gv_json_specials = |"\\\t\n\r| && gv_json_ctrl.
    ENDIF.

    " the backslash first: escaping it afterwards would double the ones the
    " replacements below have just written. The common value carries none
    " of these characters - one scan, and the copies are skipped
    result = val.
    IF result NA gv_json_specials.
      RETURN.
    ENDIF.
    result = replace( val  = result
                      sub  = `\`
                      with = `\\`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = |\n|
                      with = `\n`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = |\r|
                      with = `\r`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = |\t|
                      with = `\t`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `"`
                      with = `\"`
                      occ  = 0 ).

    " every other control character has to be escaped too - a raw form feed
    " or 0x01 (legacy long texts carry them) makes the JSON unparseable
    IF gv_json_ctrl IS INITIAL OR result NA gv_json_ctrl.
      RETURN.
    ENDIF.
    DATA(lv_i) = 0.
    WHILE lv_i < strlen( gv_json_ctrl ).
      DATA(lv_code) = substring( val = lc_ctrl_hex
                                 off = lv_i * 2
                                 len = 2 ).
      DATA(lv_escape) = SWITCH string( lv_code
                                       WHEN `08` THEN `\b`
                                       WHEN `0C` THEN `\f`
                                       ELSE `\u00` && lv_code ).
      result = replace( val  = result
                        sub  = substring( val = gv_json_ctrl
                                          off = lv_i
                                          len = 1 )
                        with = lv_escape
                        occ  = 0 ).
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD c_escape_js.

    " Backslash and the quotes are what can end the literal, CR and LF are
    " line terminators a literal must not contain, and </ is escaped so the
    " text stays harmless should it ever end up in an inline script. The
    " common value carries none of these - skip the copies then
    result = val.
    IF result NA |\\"'<\n\r|.
      RETURN.
    ENDIF.
    result = replace( val  = result
                      sub  = `\`
                      with = `\\`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `"`
                      with = `\"`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `'`
                      with = `\'`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `</`
                      with = `<\/`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = |\n|
                      with = `\n`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = |\r|
                      with = `\r`
                      occ  = 0 ).

  ENDMETHOD.

  METHOD c_escape_xml.

    " one CA scan up front: replace( occ = 0 ) copies the whole string on
    " every call even when nothing matches, and a value without any special
    " character is the COMMON case - that made every attribute value pay for
    " seven full copies. A value that does carry one still runs all seven
    " replaces below, unchanged
    IF gv_xml_escape_specials IS INITIAL.
      " the XML-illegal control characters as one string, so the CA scan
      " stays a single statement; built from their UTF-8 bytes. Behind them
      " the two non-characters U+FFFE and U+FFFF (EF BF BE, EF BF BF): a
      " legacy long text through a broken codepage conversion carries them,
      " and the XMLView parser refuses the whole document for one of them
      TRY.
          gv_xml_escape_controls = conv_get_string_by_xstring(
              CONV xstring( `000102030405060708` && `0B0C` && `0E0F101112131415161718191A1B1C1D1E1F`
                            && `EFBFBE` && `EFBFBF` ) ).
        CATCH zabaputil_cx_util_error.
          " No codepage API here (UNSUPPORTED_CODEPAGE_API). Degrade instead of
          " failing: the set stays empty, the CA scan below then matches no
          " control character and the drop loop never runs, while `&`, `<`, `>`,
          " `"`, newline, CR and tab are escaped exactly as before. Dropping
          " those 30 exotic bytes is a repair for legacy long texts, not a
          " correctness requirement of the view - so losing it must not cost
          " the render. Before this, a missing class raised out of the utility
          " and took down every view on the first escape of the process.
          CLEAR gv_xml_escape_controls.
      ENDTRY.
      gv_xml_escape_specials = `&<>"`
          && cv_char_util_newline
          && cv_char_util_cr_lf(1)
          && cv_char_util_horizontal_tab
          && gv_xml_escape_controls.
    ENDIF.
    result = val.
    IF result NA gv_xml_escape_specials.
      RETURN.
    ENDIF.

    result = replace( val  = result
                      sub  = `&`
                      with = `&amp;`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `<`
                      with = `&lt;`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `>`
                      with = `&gt;`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = `"`
                      with = `&quot;`
                      occ  = 0 ).
    " whitespace as character references - a literal LF/CR/TAB in an attribute
    " value is turned into a plain space by XML attribute-value normalization,
    " so line breaks (e.g. a two-line noDataText) would silently disappear
    result = replace( val  = result
                      sub  = cv_char_util_newline
                      with = `&#xA;`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = cv_char_util_cr_lf(1)
                      with = `&#xD;`
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = cv_char_util_horizontal_tab
                      with = `&#x9;`
                      occ  = 0 ).

    " what is left of the control range is illegal in XML 1.0 outright -
    " U+0000-U+0008, U+000B, U+000C, U+000E-U+001F cannot even be written
    " as character references, and UI5's XMLView parser rejects the whole
    " document over one such byte (a form feed or record separator out of a
    " legacy long text blanks the view). They carry no meaning in an
    " attribute value, so they are dropped. Only reached for a value that
    " passed the CA scan above - see gv_xml_escape_specials
    IF result CA gv_xml_escape_controls.
      " one replace per character, no regex: [[:cntrl:]] is not a class every
      " runtime this code runs on knows (the transpiled one left every byte
      " in place), and this branch is the rare one
      DATA(lv_off) = 0.
      DATA(lv_len) = strlen( gv_xml_escape_controls ).
      WHILE lv_off < lv_len.
        result = replace( val  = result
                          sub  = gv_xml_escape_controls+lv_off(1)
                          with = ``
                          occ  = 0 ).
        lv_off = lv_off + 1.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.

  METHOD c_filter_chars.

    " a character loop instead of a (deprecated) POSIX regex - the values
    " this strips are short, and the transpiled runtime knows no character
    " classes. Both operands as strings: a CO against a char field would
    " count its trailing blanks as allowed
    DATA(lv_val) = CONV string( val ).
    DATA(lv_allowed) = CONV string( allowed ).
    IF lv_val CO lv_allowed.
      result = lv_val.
      RETURN.
    ENDIF.
    DATA(lv_len) = strlen( lv_val ).
    DATA(lv_off) = 0.
    WHILE lv_off < lv_len.
      IF lv_val+lv_off(1) CO lv_allowed.
        result = result && lv_val+lv_off(1).
      ENDIF.
      lv_off = lv_off + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD c_cut_at.

    result = val.
    DATA(lv_off) = find( val = result
                         sub = sub ).
    IF lv_off >= 0.
      result = substring( val = result
                          len = lv_off ).
    ENDIF.

  ENDMETHOD.

  METHOD url_get_origin.

    " Only an absolute URL names a host: `https://host[:port]/...`, or the
    " protocol-relative `//host/...`. A relative one is the page's own
    " origin - nothing to answer
    DATA(lv_url) = CONV string( val ).
    DATA(lv_scheme) = ``.
    IF lv_url CP `http://*` OR lv_url CP `https://*`.
      SPLIT lv_url AT `//` INTO lv_scheme lv_url.
      lv_scheme = to_lower( lv_scheme ) && `//`.
    ELSEIF lv_url CP `//*`.
      lv_url    = lv_url+2.
      lv_scheme = `//`.
    ELSE.
      RETURN.
    ENDIF.
    " the host ends at the path, the query or the fragment, whichever is
    " first; userinfo (user:pw@host) is no origin to repeat
    DATA(lv_end) = strlen( lv_url ).
    DATA(lv_off) = 0.
    WHILE lv_off < lv_end.
      IF lv_url+lv_off(1) CA `/?#`.
        lv_end = lv_off.
      ELSE.
        lv_off = lv_off + 1.
      ENDIF.
    ENDWHILE.
    DATA(lv_host) = lv_url(lv_end).
    IF lv_host IS INITIAL OR lv_host CA `@`.
      RETURN.
    ENDIF.
    result = lv_scheme && lv_host.

  ENDMETHOD.

  METHOD url_get_authority.

    result = to_lower( val ).

    " drop the scheme (e.g. `https://`)
    IF result CS `://`.
      result = substring_after( val = result
                                sub = `://` ).
    ENDIF.

    " the authority ends at the first path / query / fragment separator
    SPLIT result AT `/` INTO result DATA(lv_rest).
    SPLIT result AT `?` INTO result lv_rest.
    SPLIT result AT `#` INTO result lv_rest.

    " a default port is no port: a browser writes `Origin: https://host`
    " while a proxy forwards `host:443`, and the two are the same
    " authority. Both defaults are stripped whatever the scheme was: a Host
    " value carries none to compare with
    DATA lv_len  TYPE i.
    DATA lv_tail TYPE string.
    lv_len = strlen( result ).
    IF lv_len > 4.
      lv_tail = substring( val = result
                           off = lv_len - 4 ).
      IF lv_tail = `:443`.
        result = substring( val = result
                            len = lv_len - 4 ).
        RETURN.
      ENDIF.
    ENDIF.
    IF lv_len > 3.
      lv_tail = substring( val = result
                           off = lv_len - 3 ).
      IF lv_tail = `:80`.
        result = substring( val = result
                            len = lv_len - 3 ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD http_check_etag_match.

    " RFC 7232: If-None-Match is a LIST of validators, compared weakly - a
    " proxy that recompresses the page weak-marks the tag (nginx gzip sends
    " W/"tag" back), a browser may echo several, and `*` matches whatever is
    " current. Apache mod_deflate (DeflateAlterETag AddSuffix, its default)
    " instead appends `-gzip` INSIDE the quotes and passes the browser's
    " echo through untouched, so that suffix is accepted too. Exact string
    " equality answered none of the four, and behind such a proxy the
    " conditional GET quietly degraded to a full transfer on every reload
    DATA(lv_etag) = CONV string( etag ).
    DATA(lv_header) = CONV string( if_none_match ).
    IF lv_header IS INITIAL OR lv_etag IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_gzip) = substring( val = lv_etag
                               len = strlen( lv_etag ) - 1 ) && `-gzip"`.
    SPLIT lv_header AT `,` INTO TABLE DATA(lt_candidate).
    LOOP AT lt_candidate INTO DATA(lv_candidate).
      lv_candidate = condense( lv_candidate ).
      IF strlen( lv_candidate ) > 2 AND substring( val = lv_candidate
                                                   len = 2 ) = `W/`.
        lv_candidate = substring( val = lv_candidate
                                  off = 2 ).
      ENDIF.
      IF lv_candidate = `*` OR lv_candidate = lv_etag OR lv_candidate = lv_gzip.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD hash_checksum.

    " Three accumulators, not two: with the byte count equal (two inputs of
    " the same length), two 16-bit accumulators alone left ~2^32 states -
    " enough for a same-length collision. The third mixes with a larger
    " modulus AND weighs every byte by its position, so a pair that collides
    " in h1/h2 still separates in h3 unless it also collides under a
    " structurally different mix - implausible for a cache validator's job,
    " still no cryptographic claim
    DATA lv_xstr TYPE xstring.
    TRY.
        lv_xstr = conv_get_xstring_by_string( CONV string( val ) ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    DATA(lv_len) = xstrlen( lv_xstr ).
    DATA lv_h1 TYPE i VALUE 5381.
    DATA lv_h2 TYPE i VALUE 17.
    DATA lv_h3 TYPE i VALUE 104729.
    DATA lv_byte TYPE i.
    DATA lv_off TYPE i.
    WHILE lv_off < lv_len.
      lv_byte = lv_xstr+lv_off(1).
      lv_h1 = ( lv_h1 * 33 + lv_byte ) MOD 65521.
      lv_h2 = ( lv_h2 * 31 + lv_byte ) MOD 65519.
      lv_h3 = ( lv_h3 * 131 + lv_byte * ( lv_off MOD 251 + 1 ) ) MOD 1000003.
      lv_off = lv_off + 1.
    ENDWHILE.

    result = |{ lv_len }-{ lv_h1 }-{ lv_h2 }-{ lv_h3 }|.

  ENDMETHOD.

  METHOD c_levenshtein.

    TYPES ty_t_int TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA(lv_a) = CONV string( val1 ).
    DATA(lv_b) = CONV string( val2 ).
    DATA(lv_la) = strlen( lv_a ).
    DATA(lv_lb) = strlen( lv_b ).

    IF lv_la = 0.
      result = lv_lb.
      RETURN.
    ENDIF.
    IF lv_lb = 0.
      result = lv_la.
      RETURN.
    ENDIF.

    DATA lt_prev TYPE ty_t_int.
    DATA lt_curr TYPE ty_t_int.
    DATA lv_j TYPE i.

    WHILE lv_j <= lv_lb.
      INSERT lv_j INTO TABLE lt_prev.
      lv_j = lv_j + 1.
    ENDWHILE.

    DATA lv_i TYPE i.
    WHILE lv_i < lv_la.

      CLEAR lt_curr.
      INSERT lv_i + 1 INTO TABLE lt_curr.

      lv_j = 0.
      WHILE lv_j < lv_lb.

        DATA(lv_cost) = COND i( WHEN lv_a+lv_i(1) = lv_b+lv_j(1) THEN 0 ELSE 1 ).
        DATA(lv_min) = lt_curr[ lv_j + 1 ] + 1.
        DATA(lv_del) = lt_prev[ lv_j + 2 ] + 1.
        IF lv_del < lv_min.
          lv_min = lv_del.
        ENDIF.
        DATA(lv_sub) = lt_prev[ lv_j + 1 ] + lv_cost.
        IF lv_sub < lv_min.
          lv_min = lv_sub.
        ENDIF.
        INSERT lv_min INTO TABLE lt_curr.

        lv_j = lv_j + 1.
      ENDWHILE.

      lt_prev = lt_curr.
      lv_i = lv_i + 1.
    ENDWHILE.

    result = lt_prev[ lv_lb + 1 ].

  ENDMETHOD.

  METHOD url_encode.

    CONSTANTS lc_unreserved TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~`.

    DATA(lv_val) = CONV string( val ).
    DATA(lv_len) = strlen( lv_val ).
    DATA lv_i    TYPE i.
    DATA lv_run  TYPE string.
    DATA lv_j    TYPE i.
    DATA lv_hex  TYPE c LENGTH 2.

    " the characters to encode are collected and converted to UTF-8 as one
    " run: one at a time split a character outside the BMP (an emoji) into
    " its two UTF-16 halves, and each half alone is no valid UTF-8
    WHILE lv_i <= lv_len.

      IF lv_i < lv_len.
        DATA(lv_char) = substring( val = lv_val
                                   off = lv_i
                                   len = 1 ).
        IF lv_char = ` ` OR lv_char CN lc_unreserved.
          lv_run = lv_run && lv_char.
          lv_i = lv_i + 1.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF lv_run IS NOT INITIAL.
        DATA(lv_x) = conv_get_xstring_by_string( lv_run ).
        lv_j = 0.
        WHILE lv_j < xstrlen( lv_x ).
          lv_hex = lv_x+lv_j(1).
          result = result && `%` && lv_hex.
          lv_j = lv_j + 1.
        ENDWHILE.
        CLEAR lv_run.
      ENDIF.

      IF lv_i < lv_len.
        result = result && lv_char.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD url_decode.

    DATA(lv_val) = CONV string( val ).
    DATA(lv_len) = strlen( lv_val ).
    DATA lv_i TYPE i.
    " the pending bytes as hex digits - a byte string has no expression
    " that appends to it, the hex text converts back in one step
    DATA lv_xbuf TYPE string.
    " the same escapes as written - what comes out when they are no UTF-8
    DATA lv_raw TYPE string.

    WHILE lv_i < lv_len.

      DATA(lv_char) = substring( val = lv_val
                                 off = lv_i
                                 len = 1 ).

      " only two hex digits make an escape - `%ZZ` stays as it is instead
      " of becoming a NUL byte
      DATA(lv_hex) = ``.
      IF lv_char = `%` AND lv_i + 2 < lv_len.
        lv_hex = to_upper( substring( val = lv_val
                                      off = lv_i + 1
                                      len = 2 ) ).
      ENDIF.
      IF lv_hex IS NOT INITIAL AND lv_hex CO `0123456789ABCDEF`.
        DATA lv_x TYPE x LENGTH 1.
        lv_x = lv_hex.
        lv_xbuf = |{ lv_xbuf }{ lv_x }|.
        lv_raw = lv_raw && substring( val = lv_val
                                      off = lv_i
                                      len = 3 ).
        lv_i = lv_i + 3.
        CONTINUE.
      ENDIF.

      IF lv_xbuf IS NOT INITIAL.
        result = result && url_decode_bytes( hex = lv_xbuf
                                             raw = lv_raw ).
        CLEAR lv_xbuf.
        CLEAR lv_raw.
      ENDIF.

      IF lv_char = `+`.
        result = result && ` `.
      ELSE.
        result = result && lv_char.
      ENDIF.

      lv_i = lv_i + 1.
    ENDWHILE.

    IF lv_xbuf IS NOT INITIAL.
      result = result && url_decode_bytes( hex = lv_xbuf
                                           raw = lv_raw ).
    ENDIF.

  ENDMETHOD.

  METHOD url_decode_bytes.

    DATA lv_invalid TYPE abap_bool.

    " `%E9` (a Latin-1 client), `%FF` or a cut `%C3` raised INVALID_UTF8
    " out of user-controlled input - such a run stays as it was written,
    " like `%ZZ` does
    conv_get_string_by_xstring_cp( EXPORTING val      = CONV xstring( hex )
                                             codepage = `UTF-8`
                                             encoding = `UTF-8`
                                   IMPORTING result   = result
                                             invalid  = lv_invalid ).
    IF lv_invalid = abap_true.
      result = raw.
    ENDIF.

  ENDMETHOD.

  METHOD regex_create_matcher.

    DATA lo_regex TYPE REF TO object.
    DATA lv_class TYPE string.

    " PCRE first, by name: CREATE_PCRE exists from 7.55 on and on ABAP
    " Cloud. On-premise used to take POSIX even where PCRE was there, so a
    " pattern (\d, lazy quantifiers, lookaheads) behaved differently on the
    " two targets. An invalid pattern is raised, not retried as POSIX
    TRY.
        lv_class = `CL_ABAP_REGEX`.
        CALL METHOD (lv_class)=>create_pcre
          EXPORTING
            pattern = regex
          RECEIVING
            regex   = lo_regex.

        CALL METHOD lo_regex->(`CREATE_MATCHER`)
          EXPORTING
            text    = val
          RECEIVING
            matcher = result.
        RETURN.

      CATCH cx_sy_dyn_call_illegal_method cx_sy_dyn_call_illegal_class.
        IF check_abap_cloud( ) = abap_true.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `REGEX_PCRE_NOT_AVAILABLE`.
        ENDIF.
    ENDTRY.

    " below 7.55: POSIX
    lv_class = `CL_ABAP_MATCHER`.
    CALL METHOD (lv_class)=>create
      EXPORTING
        pattern = regex
        text    = val
      RECEIVING
        matcher = result.

  ENDMETHOD.

  METHOD regex_match.

    DATA(lo_matcher) = regex_create_matcher( val   = val
                                             regex = regex ).

    CALL METHOD lo_matcher->(`MATCH`)
      RECEIVING
        success = result.

  ENDMETHOD.

  METHOD regex_find_all.

    DATA(lv_val) = CONV string( val ).
    LOOP AT regex_get_matches( val   = lv_val
                               regex = regex ) INTO DATA(ls_match).
      INSERT substring( val = lv_val
                        off = ls_match-off
                        len = ls_match-len ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD regex_replace_all.

    DATA lv_pos TYPE i.

    DATA(lv_val) = CONV string( val ).
    LOOP AT regex_get_matches( val   = lv_val
                               regex = regex ) INTO DATA(ls_match).
      result = result && substring( val = lv_val
                                    off = lv_pos
                                    len = ls_match-off - lv_pos ) && new_val.
      lv_pos = ls_match-off + ls_match-len.
    ENDLOOP.

    result = result && substring( val = lv_val
                                  off = lv_pos ).

  ENDMETHOD.

  METHOD regex_get_matches.

    DATA lv_success   TYPE abap_bool.
    DATA lv_empty_off TYPE i VALUE -1.
    DATA lv_off       TYPE i.
    DATA lv_len       TYPE i.

    DATA(lo_matcher) = regex_create_matcher( val   = val
                                             regex = regex ).

    DO.
      CALL METHOD lo_matcher->(`FIND_NEXT`)
        RECEIVING
          success = lv_success.
      IF lv_success = abap_false.
        EXIT.
      ENDIF.

      CALL METHOD lo_matcher->(`GET_OFFSET`)
        RECEIVING
          offset = lv_off.
      CALL METHOD lo_matcher->(`GET_LENGTH`)
        RECEIVING
          length = lv_len.

      " a match of length zero is skipped, not the end: `a*` on `baaa`
      " matches nothing at offset 0 and `aaa` after it. Two empty matches
      " at the same offset mean the matcher does not move on - stop then
      IF lv_len = 0.
        IF lv_off = lv_empty_off.
          EXIT.
        ENDIF.
        lv_empty_off = lv_off.
        CONTINUE.
      ENDIF.

      INSERT VALUE #( off = lv_off
                      len = lv_len ) INTO TABLE result.
    ENDDO.

  ENDMETHOD.

  METHOD uuid_get_c36.

    result = uuid_conv_c32_to_c36( uuid_get_c32( ) ).

  ENDMETHOD.

  METHOD uuid_conv_c32_to_c36.

    DATA(lv_c32) = to_upper( c_trim( val ) ).

    IF strlen( lv_c32 ) <> 32.
      x_raise( `INVALID_UUID_C32` ).
    ENDIF.

    result = lv_c32(8) && `-` && lv_c32+8(4) && `-` && lv_c32+12(4) && `-` && lv_c32+16(4) && `-` && lv_c32+20(12).

  ENDMETHOD.

  METHOD uuid_conv_c36_to_c32.

    result = to_upper( c_trim( val ) ).
    result = replace( val = result sub = `-` with = `` occ = 0 ).

    IF strlen( result ) <> 32.
      x_raise( `INVALID_UUID_C36` ).
    ENDIF.

  ENDMETHOD.

  METHOD uuid_conv_c32_to_c22.

    CONSTANTS lc_b64 TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_`.

    DATA(lv_c32) = to_upper( c_trim( val ) ).

    IF strlen( lv_c32 ) <> 32.
      x_raise( `INVALID_UUID_C32` ).
    ENDIF.

    DATA lv_x TYPE xstring.
    lv_x = lv_c32.

    DATA lv_i TYPE i.
    DATA(lv_xlen) = xstrlen( lv_x ).

    WHILE lv_i < lv_xlen.

      DATA(lv_b1) = CONV i( lv_x+lv_i(1) ).
      DATA lv_b2 TYPE i.
      DATA lv_b3 TYPE i.
      lv_b2 = -1.
      lv_b3 = -1.
      IF lv_i + 1 < lv_xlen.
        lv_b2 = lv_x+lv_i(2) MOD 256.
      ENDIF.
      IF lv_i + 2 < lv_xlen.
        DATA(lv_i2) = lv_i + 2.
        lv_b3 = CONV i( lv_x+lv_i2(1) ).
      ENDIF.

      DATA(lv_n) = lv_b1 * 65536.
      IF lv_b2 >= 0.
        lv_n = lv_n + lv_b2 * 256.
      ENDIF.
      IF lv_b3 >= 0.
        lv_n = lv_n + lv_b3.
      ENDIF.

      result = result && substring( val = lc_b64
                                    off = lv_n DIV 262144
                                    len = 1 )
                      && substring( val = lc_b64
                                    off = ( lv_n DIV 4096 ) MOD 64
                                    len = 1 ).
      IF lv_b2 >= 0.
        result = result && substring( val = lc_b64
                                      off = ( lv_n DIV 64 ) MOD 64
                                      len = 1 ).
      ENDIF.
      IF lv_b3 >= 0.
        result = result && substring( val = lc_b64
                                      off = lv_n MOD 64
                                      len = 1 ).
      ENDIF.

      lv_i = lv_i + 3.
    ENDWHILE.

  ENDMETHOD.

  METHOD uuid_conv_c22_to_c32.

    CONSTANTS lc_b64 TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_`.

    DATA(lv_c22) = c_trim( val ).
    lv_c22 = replace( val = lv_c22 sub = `+` with = `-` occ = 0 ).
    lv_c22 = replace( val = lv_c22 sub = `/` with = `_` occ = 0 ).

    IF strlen( lv_c22 ) <> 22.
      x_raise( `INVALID_UUID_C22` ).
    ENDIF.

    " the 16 bytes as 32 hex digits, built byte by byte
    DATA lv_hex TYPE string.
    DATA lv_xb TYPE x LENGTH 1.
    DATA lv_i TYPE i.
    DATA(lv_len) = strlen( lv_c22 ).

    WHILE lv_i < lv_len.

      DATA(lv_take) = lv_len - lv_i.
      IF lv_take > 4.
        lv_take = 4.
      ENDIF.

      DATA lv_n TYPE i.
      DATA lv_k TYPE i.
      lv_n = 0.
      lv_k = 0.
      WHILE lv_k < lv_take.
        DATA(lv_pos) = lv_i + lv_k.
        DATA(lv_char) = substring( val = lv_c22
                                   off = lv_pos
                                   len = 1 ).
        DATA(lv_idx) = find( val = lc_b64
                             sub = lv_char ).
        IF lv_idx < 0.
          x_raise( `INVALID_UUID_C22` ).
        ENDIF.
        lv_n = lv_n * 64 + lv_idx.
        lv_k = lv_k + 1.
      ENDWHILE.

      CASE lv_take.
        WHEN 4.
          lv_xb = lv_n DIV 65536.
          lv_hex = |{ lv_hex }{ lv_xb }|.
          lv_xb = ( lv_n DIV 256 ) MOD 256.
          lv_hex = |{ lv_hex }{ lv_xb }|.
          lv_xb = lv_n MOD 256.
          lv_hex = |{ lv_hex }{ lv_xb }|.
        WHEN 3.
          lv_xb = lv_n DIV 1024.
          lv_hex = |{ lv_hex }{ lv_xb }|.
          lv_xb = ( lv_n DIV 4 ) MOD 256.
          lv_hex = |{ lv_hex }{ lv_xb }|.
        WHEN 2.
          lv_xb = lv_n DIV 16.
          lv_hex = |{ lv_hex }{ lv_xb }|.
      ENDCASE.

      lv_i = lv_i + lv_take.
    ENDWHILE.

    IF strlen( lv_hex ) <> 32.
      x_raise( `INVALID_UUID_C22` ).
    ENDIF.

    result = lv_hex.

  ENDMETHOD.

  METHOD itab_sum_by.

    FIELD-SYMBOLS <row> TYPE any.
    DATA(lv_fieldname) = to_upper( fieldname ).

    LOOP AT tab ASSIGNING <row>.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      IF sy-subrc = 0.
        result = result + <val>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_distinct.

    FIELD-SYMBOLS <row> TYPE any.
    " the values already in result - a line_exists( ) on result itself made
    " a near-unique column of n rows cost n * n / 2 comparisons
    DATA lt_seen TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    DATA(lv_fieldname) = to_upper( fieldname ).

    LOOP AT tab ASSIGNING <row>.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      " a string template, not CONV: a plain move writes the sign of a
      " number behind it (`5-`), where itab_count_by and the UI write `-5`
      DATA(lv_str) = |{ <val> }|.
      INSERT lv_str INTO TABLE lt_seen.
      IF sy-subrc = 0.
        INSERT lv_str INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_group_sum_by.

    TYPES:
      BEGIN OF ty_s_sum,
        n   TYPE string,
        sum TYPE decfloat34,
      END OF ty_s_sum.
    TYPES:
      BEGIN OF ty_s_index,
        n   TYPE string,
        idx TYPE i,
      END OF ty_s_index.

    DATA lt_sum TYPE STANDARD TABLE OF ty_s_sum WITH EMPTY KEY.
    " group -> its row in lt_sum: a key read on lt_sum itself scanned every
    " group for every row
    DATA lt_index TYPE HASHED TABLE OF ty_s_index WITH UNIQUE KEY n.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <sum> TYPE ty_s_sum.

    DATA(lv_group_by) = to_upper( group_by ).
    DATA(lv_sum_by) = to_upper( sum_by ).

    LOOP AT tab ASSIGNING <row>.

      ASSIGN COMPONENT lv_group_by OF STRUCTURE <row> TO FIELD-SYMBOL(<group>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_sum_by OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " a string template, not CONV - see itab_distinct
      DATA(lv_group) = |{ <group> }|.
      ASSIGN lt_index[ n = lv_group ] TO FIELD-SYMBOL(<index>).
      IF sy-subrc = 0.
        ASSIGN lt_sum[ <index>-idx ] TO <sum>.
      ELSE.
        INSERT VALUE #( n = lv_group ) INTO TABLE lt_sum ASSIGNING <sum>.
        INSERT VALUE #( n   = lv_group
                        idx = lines( lt_sum ) ) INTO TABLE lt_index.
      ENDIF.
      <sum>-sum = <sum>-sum + <val>.

    ENDLOOP.

    LOOP AT lt_sum ASSIGNING <sum>.
      INSERT VALUE #( n = <sum>-n
                      v = |{ <sum>-sum }| ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD num_round.

    DATA lv_factor TYPE decfloat34 VALUE 1.

    DO decimals TIMES.
      lv_factor = lv_factor * 10.
    ENDDO.

    DATA lv_sign TYPE decfloat34 VALUE 1.
    IF val < 0.
      lv_sign = -1.
    ENDIF.

    DATA(lv_abs) = val * lv_sign * lv_factor.
    DATA lv_rounded TYPE decfloat34.
    DATA lv_half TYPE decfloat34 VALUE '0.5'.
    DATA(lv_mode) = to_upper( CONV string( mode ) ).

    lv_rounded = trunc( lv_abs ).

    DATA(lv_frac) = frac( lv_abs ).

    CASE lv_mode.
      WHEN `UP`.
        IF lv_frac > 0.
          lv_rounded = lv_rounded + 1.
        ENDIF.
      WHEN `DOWN`.
        " truncation is already rounding toward zero
      WHEN `CEILING`.
        " toward plus infinity: away from zero for a positive value only
        IF lv_frac > 0 AND lv_sign > 0.
          lv_rounded = lv_rounded + 1.
        ENDIF.
      WHEN `FLOOR`.
        " toward minus infinity: away from zero for a negative value only
        IF lv_frac > 0 AND lv_sign < 0.
          lv_rounded = lv_rounded + 1.
        ENDIF.
      WHEN `HALF_DOWN`.
        IF lv_frac > lv_half.
          lv_rounded = lv_rounded + 1.
        ENDIF.
      WHEN `HALF_EVEN`.
        " the tie goes to the even neighbour
        IF lv_frac > lv_half
            OR ( lv_frac = lv_half AND frac( lv_rounded / 2 ) <> 0 ).
          lv_rounded = lv_rounded + 1.
        ENDIF.
      WHEN OTHERS.
        IF lv_frac >= lv_half.
          lv_rounded = lv_rounded + 1.
        ENDIF.
    ENDCASE.

    " no minus sign on a result that rounded to zero
    IF lv_rounded = 0.
      result = 0.
      RETURN.
    ENDIF.

    result = lv_rounded * lv_sign / lv_factor.

  ENDMETHOD.

  METHOD file_get_mimetype.

    DATA(lv_name) = to_lower( c_trim( val ) ).
    SPLIT lv_name AT `.` INTO TABLE DATA(lt_parts).
    " no name, no extension - an empty name splits into no part at all
    IF lt_parts IS INITIAL.
      result = `application/octet-stream`.
      RETURN.
    ENDIF.
    DATA(lv_ext) = lt_parts[ lines( lt_parts ) ].

    CASE lv_ext.
      WHEN `txt` OR `text` OR `log` OR `md`.
        result = `text/plain`.
      WHEN `html` OR `htm`.
        result = `text/html`.
      WHEN `css`.
        result = `text/css`.
      WHEN `csv`.
        result = `text/csv`.
      WHEN `js` OR `mjs`.
        result = `text/javascript`.
      WHEN `json`.
        result = `application/json`.
      WHEN `xml`.
        result = `application/xml`.
      WHEN `pdf`.
        result = `application/pdf`.
      WHEN `zip`.
        result = `application/zip`.
      WHEN `png`.
        result = `image/png`.
      WHEN `jpg` OR `jpeg`.
        result = `image/jpeg`.
      WHEN `gif`.
        result = `image/gif`.
      WHEN `svg`.
        result = `image/svg+xml`.
      WHEN `webp`.
        result = `image/webp`.
      WHEN `ico`.
        result = `image/x-icon`.
      WHEN `mp3`.
        result = `audio/mpeg`.
      WHEN `mp4`.
        result = `video/mp4`.
      WHEN `woff`.
        result = `font/woff`.
      WHEN `woff2`.
        result = `font/woff2`.
      WHEN `ttf`.
        result = `font/ttf`.
      WHEN `otf`.
        result = `font/otf`.
      WHEN `xlsx`.
        result = `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`.
      WHEN `xls`.
        result = `application/vnd.ms-excel`.
      WHEN `docx`.
        result = `application/vnd.openxmlformats-officedocument.wordprocessingml.document`.
      WHEN `doc`.
        result = `application/msword`.
      WHEN `pptx`.
        result = `application/vnd.openxmlformats-officedocument.presentationml.presentation`.
      WHEN `ppt`.
        result = `application/vnd.ms-powerpoint`.
      WHEN OTHERS.
        result = `application/octet-stream`.
    ENDCASE.

  ENDMETHOD.

  METHOD lang_sap_to_iso.

    " not upper-cased: SAP language keys are case-sensitive (c is Catalan,
    " C Czech) - the lookup and the T002 read use the key as given
    DATA(lv_sap) = c_trim( val ).

    SPLIT cv_lang_map AT `;` INTO TABLE DATA(lt_pairs).
    LOOP AT lt_pairs INTO DATA(lv_pair).
      SPLIT lv_pair AT `:` INTO DATA(lv_key) DATA(lv_iso).
      IF lv_key = lv_sap.
        result = lv_iso.
        RETURN.
      ENDIF.
    ENDLOOP.

    TRY.
        DATA(lv_tab) = `T002`.
        SELECT SINGLE laiso FROM (lv_tab) WHERE spras = @lv_sap INTO @result.
        IF sy-subrc = 0.
          result = to_lower( result ).
        ELSE.
          CLEAR result.
        ENDIF.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD lang_iso_to_sap.

    " ISO 639-1 codes are case-insensitive, the SAP key comes back as T002
    " or the map has it
    DATA(lv_iso) = to_lower( c_trim( val ) ).

    SPLIT cv_lang_map AT `;` INTO TABLE DATA(lt_pairs).
    LOOP AT lt_pairs INTO DATA(lv_pair).
      SPLIT lv_pair AT `:` INTO DATA(lv_key) DATA(lv_map_iso).
      IF lv_map_iso = lv_iso.
        result = lv_key.
        RETURN.
      ENDIF.
    ENDLOOP.

    TRY.
        DATA(lv_tab) = `T002`.
        DATA(lv_laiso) = to_upper( lv_iso ).
        SELECT SINGLE spras FROM (lv_tab) WHERE laiso = @lv_laiso INTO @result.
        IF sy-subrc <> 0.
          CLEAR result.
        ENDIF.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD time_get_user_timezone.

    DATA lv_class TYPE string.
    FIELD-SYMBOLS <zonlo> TYPE any.

    TRY.
        lv_class = `CL_ABAP_CONTEXT_INFO`.
        CALL METHOD (lv_class)=>(`GET_USER_TIME_ZONE`)
          RECEIVING
            rv_time_zone = result.
      CATCH cx_root.
        ASSIGN (`SY-ZONLO`) TO <zonlo>.
        IF sy-subrc = 0.
          result = <zonlo>.
        ENDIF.
    ENDTRY.

  ENDMETHOD.

  METHOD time_stampl_to_tz.

    DATA lv_tz TYPE c LENGTH 6.
    lv_tz = tz.

    CONVERT TIME STAMP val TIME ZONE lv_tz INTO DATE date TIME time.

  ENDMETHOD.

  METHOD time_stampl_from_tz.

    DATA lv_tz TYPE c LENGTH 6.
    lv_tz = tz.

    CONVERT DATE date TIME time INTO TIME STAMP result TIME ZONE lv_tz.

  ENDMETHOD.

  METHOD context_get_user_info.

    DATA lv_class TYPE string.
    DATA lv_tab   TYPE string.

    DATA(lv_uname) = COND string( WHEN uname IS NOT INITIAL
                                  THEN to_upper( c_trim( uname ) )
                                  ELSE CONV string( sy-uname ) ).

    result-uname    = lv_uname.
    result-langu    = sy-langu.
    result-timezone = time_get_user_timezone( ).

    IF check_abap_cloud( ) = abap_true.

      lv_class = `CL_ABAP_CONTEXT_INFO`.
      TRY.
          CALL METHOD (lv_class)=>(`GET_USER_FORMATTED_NAME`)
            RECEIVING
              rv_formatted_name = result-name_formatted.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      TRY.
          CALL METHOD (lv_class)=>(`GET_USER_DATE_FORMAT`)
            RECEIVING
              rv_date_format = result-date_format.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      TRY.
          CALL METHOD (lv_class)=>(`GET_USER_DECIMAL_FORMAT`)
            RECEIVING
              rv_decimal_format = result-decimal_format.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

    ELSE.

      TRY.
          lv_tab = `USER_ADDR`.
          SELECT SINGLE name_textc FROM (lv_tab) WHERE bname = @lv_uname INTO @result-name_formatted.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      TRY.
          lv_tab = `USR01`.
          SELECT SINGLE datfm FROM (lv_tab) WHERE bname = @lv_uname INTO @result-date_format.
          SELECT SINGLE dcpfm FROM (lv_tab) WHERE bname = @lv_uname INTO @result-decimal_format.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      TRY.
          DATA lv_persnumber TYPE c LENGTH 10.
          DATA lv_addrnumber TYPE c LENGTH 10.
          lv_tab = `USR21`.
          SELECT SINGLE persnumber FROM (lv_tab) WHERE bname = @lv_uname INTO @lv_persnumber.
          SELECT SINGLE addrnumber FROM (lv_tab) WHERE bname = @lv_uname INTO @lv_addrnumber.
          IF sy-subrc = 0.
            lv_tab = `ADR6`.
            SELECT SINGLE smtp_addr FROM (lv_tab) WHERE persnumber = @lv_persnumber AND addrnumber = @lv_addrnumber INTO @result-email.
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD cur_get_decimals.

    DATA lv_tab TYPE string.

    DATA(lv_curr) = to_upper( c_trim( val ) ).
    result = 2.

    TRY.
        lv_tab = `TCURX`.
        SELECT SINGLE currdec FROM (lv_tab) WHERE currkey = @lv_curr INTO @result.
        IF sy-subrc <> 0.
          result = 2.
        ENDIF.
      CATCH cx_root.
        TRY.
            lv_tab = `I_CURRENCY`.
            SELECT SINGLE decimals FROM (lv_tab) WHERE currency = @lv_curr INTO @result.
            IF sy-subrc <> 0.
              result = 2.
            ENDIF.
          CATCH cx_root.
            result = 2.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.

  METHOD cur_amount_to_external.

    DATA(lv_dec) = decimals.
    IF lv_dec < 0.
      lv_dec = cur_get_decimals( currency ).
    ENDIF.

    DATA lv_factor TYPE decfloat34 VALUE 1.

    IF lv_dec < 2.
      DO 2 - lv_dec TIMES.
        lv_factor = lv_factor * 10.
      ENDDO.
      result = val * lv_factor.
    ELSEIF lv_dec > 2.
      DO lv_dec - 2 TIMES.
        lv_factor = lv_factor * 10.
      ENDDO.
      result = val / lv_factor.
    ELSE.
      result = val * lv_factor.
    ENDIF.

  ENDMETHOD.

  METHOD cur_amount_to_internal.

    DATA(lv_dec) = decimals.
    IF lv_dec < 0.
      lv_dec = cur_get_decimals( currency ).
    ENDIF.

    DATA lv_factor TYPE decfloat34 VALUE 1.

    IF lv_dec < 2.
      DO 2 - lv_dec TIMES.
        lv_factor = lv_factor * 10.
      ENDDO.
      result = val / lv_factor.
    ELSEIF lv_dec > 2.
      DO lv_dec - 2 TIMES.
        lv_factor = lv_factor * 10.
      ENDDO.
      result = val * lv_factor.
    ELSE.
      result = val * lv_factor.
    ENDIF.

  ENDMETHOD.

  METHOD unit_convert.

    DATA lv_fm  TYPE string.
    DATA lv_tab TYPE string.

    IF unit_from = unit_to.
      result = val.
      RETURN.
    ENDIF.

    IF check_abap_cloud( ) = abap_false.

      DATA lv_in  TYPE f.
      DATA lv_out TYPE f.
      DATA lv_ui  TYPE c LENGTH 3.
      DATA lv_uo  TYPE c LENGTH 3.

      lv_in = val.
      lv_ui = to_upper( c_trim( unit_from ) ).
      lv_uo = to_upper( c_trim( unit_to ) ).

      lv_fm = `UNIT_CONVERSION_SIMPLE`.
      CALL FUNCTION lv_fm
        EXPORTING
          input         = lv_in
          unit_in       = lv_ui
          unit_out      = lv_uo
        IMPORTING
          output        = lv_out
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        x_raise( `UNIT_CONVERSION_FAILED` ).
      ENDIF.

      result = lv_out.

    ELSE.

      TRY.
          TYPES:
            BEGIN OF ty_s_uom,
              unitofmeasuresinumerator   TYPE decfloat34,
              unitofmeasuresidenominator TYPE decfloat34,
              unitofmeasuresiexponent    TYPE i,
            END OF ty_s_uom.
          DATA ls_uom_from TYPE ty_s_uom.
          DATA ls_uom_to   TYPE ty_s_uom.
          DATA lv_num_from TYPE decfloat34.
          DATA lv_den_from TYPE decfloat34.
          DATA lv_exp_from TYPE i.
          DATA lv_num_to   TYPE decfloat34.
          DATA lv_den_to   TYPE decfloat34.
          DATA lv_exp_to   TYPE i.

          DATA(lv_from) = to_upper( c_trim( unit_from ) ).
          DATA(lv_to) = to_upper( c_trim( unit_to ) ).

          lv_tab = `I_UNITOFMEASURE`.
          SELECT SINGLE unitofmeasuresinumerator, unitofmeasuresidenominator, unitofmeasuresiexponent
            FROM (lv_tab)
            WHERE unitofmeasure = @lv_from
            INTO CORRESPONDING FIELDS OF @ls_uom_from.
          IF sy-subrc <> 0.
            x_raise( `UNIT_CONVERSION_FAILED` ).
          ENDIF.
          SELECT SINGLE unitofmeasuresinumerator, unitofmeasuresidenominator, unitofmeasuresiexponent
            FROM (lv_tab)
            WHERE unitofmeasure = @lv_to
            INTO CORRESPONDING FIELDS OF @ls_uom_to.
          IF sy-subrc <> 0.
            x_raise( `UNIT_CONVERSION_FAILED` ).
          ENDIF.

          lv_num_from = ls_uom_from-unitofmeasuresinumerator.
          lv_den_from = ls_uom_from-unitofmeasuresidenominator.
          lv_exp_from = ls_uom_from-unitofmeasuresiexponent.
          lv_num_to   = ls_uom_to-unitofmeasuresinumerator.
          lv_den_to   = ls_uom_to-unitofmeasuresidenominator.
          lv_exp_to   = ls_uom_to-unitofmeasuresiexponent.

          DATA lv_factor_from TYPE decfloat34.
          DATA lv_factor_to   TYPE decfloat34.
          DATA lv_ten         TYPE decfloat34 VALUE 10.

          lv_factor_from = lv_num_from / lv_den_from.
          DO lv_exp_from TIMES.
            lv_factor_from = lv_factor_from * lv_ten.
          ENDDO.
          DO lv_exp_from * -1 TIMES.
            lv_factor_from = lv_factor_from / lv_ten.
          ENDDO.

          lv_factor_to = lv_num_to / lv_den_to.
          DO lv_exp_to TIMES.
            lv_factor_to = lv_factor_to * lv_ten.
          ENDDO.
          DO lv_exp_to * -1 TIMES.
            lv_factor_to = lv_factor_to / lv_ten.
          ENDDO.

          result = val * lv_factor_from / lv_factor_to.

        CATCH cx_root.
          x_raise( `UNIT_CONVERSION_FAILED` ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD hash_calculate.

    DATA lv_class TYPE string.
    DATA lv_alg   TYPE c LENGTH 10.
    DATA lv_data  TYPE string.

    lv_alg = to_upper( c_trim( algorithm ) ).
    lv_data = val.

    TRY.
        lv_class = `CL_ABAP_MESSAGE_DIGEST`.
        CALL METHOD (lv_class)=>(`CALCULATE_HASH_FOR_CHAR`)
          EXPORTING
            if_algorithm  = lv_alg
            if_data       = lv_data
          IMPORTING
            ef_hashstring = result.
      CATCH cx_root.
        x_raise( `HASH_CALCULATION_FAILED` ).
    ENDTRY.

  ENDMETHOD.

  METHOD hash_hmac.

    DATA lv_class TYPE string.
    DATA lv_alg   TYPE c LENGTH 10.
    DATA lv_data  TYPE string.

    lv_alg = to_upper( c_trim( algorithm ) ).
    lv_data = val.
    DATA(lv_key) = conv_get_xstring_by_string( CONV string( key ) ).

    TRY.
        lv_class = `CL_ABAP_HMAC`.
        CALL METHOD (lv_class)=>(`CALCULATE_HMAC_FOR_CHAR`)
          EXPORTING
            if_algorithm  = lv_alg
            if_key        = lv_key
            if_data       = lv_data
          IMPORTING
            ef_hmacstring = result.
      CATCH cx_root.
        x_raise( `HMAC_CALCULATION_FAILED` ).
    ENDTRY.

  ENDMETHOD.

  METHOD http_get.

    result = http_execute( method       = `GET`
                           url          = CONV string( url )
                           body         = ``
                           content_type = ``
                           t_header     = t_header ).

  ENDMETHOD.

  METHOD http_post.

    result = http_execute( method       = `POST`
                           url          = CONV string( url )
                           body         = CONV string( body )
                           content_type = CONV string( content_type )
                           t_header     = t_header ).

  ENDMETHOD.

  METHOD http_execute.

    " the one HTTP client of this catalog, zabaputil_cl_util_http - the two
    " branches that sat here were a second client: no CLOSE when the call
    " failed, no UTF-8 decoding of a body without a charset, and the cloud
    " request read into the wrong parameter
    DATA(ls_req) = VALUE zabaputil_cl_util_http=>ty_s_client_req( method   = method
                                                                  url      = url
                                                                  body     = body
                                                                  t_header = t_header ).
    IF content_type IS NOT INITIAL
        AND NOT line_exists( ls_req-t_header[ n = `content-type` ] )
        AND NOT line_exists( ls_req-t_header[ n = `Content-Type` ] ).
      INSERT VALUE #( n = `content-type`
                      v = content_type ) INTO TABLE ls_req-t_header.
    ENDIF.

    DATA(ls_res) = zabaputil_cl_util_http=>client_request( ls_req ).
    result-code   = ls_res-status_code.
    result-reason = ls_res-status_reason.
    result-body   = ls_res-body.

  ENDMETHOD.

  METHOD db_reset_table_buffer.

    DATA lv_fm      TYPE c LENGTH 30 VALUE `SBUF_GENERIC_RESET`.
    DATA lv_tabname TYPE c LENGTH 30.

    IF check_abap_cloud( ) = abap_true.
      RETURN.
    ENDIF.

    lv_tabname = val.
    " called by name, so the class compiles where the module does not exist
    TRY.
        CALL FUNCTION lv_fm
          EXPORTING
            sync    = abap_true
            tabname = lv_tabname
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD db_check_table_exists.

    DATA lr_data TYPE REF TO data.

    DATA(lv_name) = to_upper( c_trim( val ) ).

    TRY.
        CREATE DATA lr_data TYPE (lv_name).
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD db_select_by_name.

    DATA(lv_tab) = to_upper( c_trim( tabname ) ).
    DATA(lv_where) = CONV string( where ).

    result = rtti_create_tab_by_name( lv_tab ).
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN result->* TO <tab>.
    " rtti_create_tab_by_name answers an unknown name with an unbound
    " reference - the SELECT below would dump on the unassigned target
    IF <tab> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |DB_TABLE_NOT_FOUND - no table or view { lv_tab }|.
    ENDIF.

    TRY.
        IF max_rows > 0.
          " INTO and UP TO ahead of WHERE: the 702 downport moves INTO in front of
          " WHERE but leaves a trailing UP TO behind, which no release parses
          SELECT * FROM (lv_tab) INTO TABLE @<tab> UP TO @max_rows ROWS WHERE (lv_where).
        ELSE.
          SELECT * FROM (lv_tab) WHERE (lv_where) INTO TABLE @<tab>.
        ENDIF.
      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD db_count_by_name.

    DATA(lv_tab) = to_upper( c_trim( tabname ) ).
    DATA(lv_where) = CONV string( where ).

    TRY.
        SELECT COUNT(*) FROM (lv_tab) WHERE (lv_where) INTO @result.
      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD param_get_user_default.

    DATA lv_tab TYPE string.

    DATA(lv_uname) = COND string( WHEN uname IS NOT INITIAL
                                  THEN to_upper( c_trim( uname ) )
                                  ELSE CONV string( sy-uname ) ).
    DATA(lv_parid) = to_upper( c_trim( val ) ).

    TRY.
        lv_tab = `USR05`.
        SELECT SINGLE parva FROM (lv_tab) WHERE bname = @lv_uname AND parid = @lv_parid INTO @result.
        IF sy-subrc <> 0.
          CLEAR result.
        ENDIF.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD job_get_status.

    DATA lv_tab TYPE string.

    DATA(lv_jobname) = to_upper( c_trim( jobname ) ).
    DATA(lv_jobcount) = c_trim( jobcount ).

    TRY.
        lv_tab = `TBTCO`.
        SELECT SINGLE status FROM (lv_tab) WHERE jobname = @lv_jobname AND jobcount = @lv_jobcount INTO @result.
        IF sy-subrc <> 0.
          CLEAR result.
        ENDIF.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.


  METHOD check_msg_container.

    DATA(lv_kind) = rtti_get_type_kind( val ).
    result = xsdbool( lv_kind = cl_abap_datadescr=>typekind_table
                   OR lv_kind = cl_abap_datadescr=>typekind_struct1
                   OR lv_kind = cl_abap_datadescr=>typekind_struct2
                   OR lv_kind = cl_abap_datadescr=>typekind_oref ).

  ENDMETHOD.

  METHOD filter_token_fill.

    " built by concatenation, not REPLACE ... WITH: LOW and HIGH are data,
    " and the transpiled runtime reads a WITH text like a regex replacement
    " ( `$&`, `$$`, `\{` change - backlog item runtime-replace-with-literal ).
    " HIGH first: a LOW that spells `{HIGH}` is then never substituted a
    " second time ( {LOW} stands before {HIGH} in every template )
    DATA lv_off  TYPE i.
    DATA lv_rest TYPE i.

    result = template.
    lv_off = find( val = result
                   sub = `{HIGH}` ).
    IF lv_off >= 0.
      lv_rest = lv_off + 6.
      result = substring( val = result
                          len = lv_off ) && high && substring( val = result
                                                               off = lv_rest ).
    ENDIF.
    lv_off = find( val = result
                   sub = `{LOW}` ).
    IF lv_off >= 0.
      lv_rest = lv_off + 5.
      result = substring( val = result
                          len = lv_off ) && low && substring( val = result
                                                              off = lv_rest ).
    ENDIF.

  ENDMETHOD.

  METHOD hash_string.

    IF gv_hash_absent = abap_true.
      result = val.
      RETURN.
    ENDIF.
    result = hash_string_by_class( val       = val
                                   classname = `CL_ABAP_MESSAGE_DIGEST` ).
    IF result IS INITIAL.
      " no digest class, or one that cannot answer: the text itself, which
      " compares exactly as before
      gv_hash_absent = abap_true.
      result = val.
    ENDIF.

  ENDMETHOD.

  METHOD hash_string_by_class.

    DATA lv_classname TYPE string.
    DATA lv_data      TYPE string.
    DATA lv_algorithm TYPE string.

    lv_classname = classname.
    lv_data      = val.
    lv_algorithm = `SHA256`.
    TRY.
        CALL METHOD (lv_classname)=>calculate_hash_for_char
          EXPORTING
            if_algorithm  = lv_algorithm
            if_data       = lv_data
          IMPORTING
            ef_hashstring = result.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD itab_filter_check_field.

    " a table, a deep structure or a reference cannot go into a string
    " template - with no field list every component is visited, and a
    " master-detail row (a table of children inside the row) dumped the
    " whole filter. Such a component holds no text to match
    IF rtti_check_printable( field ) = abap_false.
      RETURN.
    ENDIF.

    result = itab_filter_check_text( text        = |{ field }|
                                     search      = search
                                     ignore_case = ignore_case ).

  ENDMETHOD.

  METHOD itab_filter_check_text.

    IF ignore_case = abap_true.
      IF to_upper( text ) CS search.
        result = abap_true.
      ENDIF.
    ELSEIF find( val = text
                 sub = search ) >= 0.
      result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_by_oref_attri.

    FIELD-SYMBOLS <comp> TYPE any.

    result = is_msg.
    DATA(lt_attri_o) = rtti_get_t_attri_by_oref( io_obj ).
    LOOP AT lt_attri_o REFERENCE INTO DATA(ls_attri_o) "#EC CI_SORTSEQ
         WHERE visibility = cv_objectdescr_public.
      " UNASSIGN + IS ASSIGNED, not sy-subrc - the loop-carried case (see
      " error_get_attributes). Here a stale 0 maps the PREVIOUS attribute's
      " value onto this one's name, i.e. a message id, number or placeholder
      " taken from the wrong attribute
      UNASSIGN <comp>.
      ASSIGN io_obj->(ls_attri_o->name) TO <comp>.
      IF <comp> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      result = msg_map( name = ls_attri_o->name
                        val  = <comp>
                        msg  = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_internal_tab.

    " the RAP shape is a property of the ROW TYPE: decided on the first
    " structured row and handed down, not asked of every row - the check
    " describes every table component of a row, which a business table of a
    " few thousand rows paid a few thousand times before the box even
    " decided it carries no message at all
    DATA(lv_rap)   = abap_false.
    DATA(lv_first) = abap_true.
    LOOP AT it_tab ASSIGNING FIELD-SYMBOL(<row>).
      IF lv_first = abap_true.
        lv_first = abap_false.
        DATA(lv_row_kind) = rtti_get_type_kind( <row> ).
        IF lv_row_kind = cl_abap_datadescr=>typekind_struct1
            OR lv_row_kind = cl_abap_datadescr=>typekind_struct2.
          lv_rap = check_is_rap_struct( <row> ).
        ENDIF.
      ENDIF.
      IF lv_rap = abap_true AND <row> IS NOT INITIAL.
        INSERT LINES OF msg_get_rap( <row> ) INTO TABLE result.
      ELSE.
        INSERT LINES OF msg_get_internal( val          = <row>
                                          iv_check_rap = abap_false ) INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_check_class_impl_intf.

    DATA lo_typedescr  TYPE REF TO cl_abap_typedescr.
    DATA lo_classdescr TYPE REF TO cl_abap_classdescr.
    DATA lv_class      TYPE string.
    DATA lv_intf       TYPE string.

    " cached per pair - see gt_class_impl_intf at the declaration
    lv_class = to_upper( class ).
    lv_intf  = to_upper( intf ).

    ASSIGN gt_class_impl_intf[ name = lv_class
                               intf = lv_intf ] TO FIELD-SYMBOL(<ls_hit>).
    IF sy-subrc = 0.
      result = <ls_hit>-result.
      RETURN.
    ENDIF.

    " ONE describe: the non-functional form answers "no such class" with
    " sy-subrc rather than an exception, so the separate existence check
    " that used to run first (its own describe of the same class) is not
    " needed here
    TRY.
        cl_abap_classdescr=>describe_by_name( EXPORTING p_name          = lv_class
                                              RECEIVING p_descr_ref     = lo_typedescr
                                              EXCEPTIONS type_not_found = 1 ).
        IF sy-subrc = 0.
          " an interface or a data type carries the name too - the cast is
          " what says "class", and a failed one is a plain abap_false
          lo_classdescr ?= lo_typedescr.
          " a handful of rows per class - the read is not the cost here
          result = xsdbool( line_exists( lo_classdescr->interfaces[ name = lv_intf ] ) ). "#EC CI_SORTSEQ
        ENDIF.

      CATCH cx_sy_move_cast_error ##NO_HANDLER.
        " an interface or a data type - no class, a plain abap_false
      CATCH cx_root.
        " the descriptor failed - not cached, so a class fixed meanwhile is
        " checked anew; rtti_get_class_load_error( ) says why
        RETURN.
    ENDTRY.

    INSERT VALUE #( name   = lv_class
                    intf   = lv_intf
                    result = result ) INTO TABLE gt_class_impl_intf.

  ENDMETHOD.

  METHOD rtti_get_class_load_error.

    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.

    TRY.
        cl_abap_classdescr=>describe_by_name( EXPORTING p_name          = to_upper( class )
                                              RECEIVING p_descr_ref     = lo_typedescr
                                              EXCEPTIONS type_not_found = 1 ).
        IF sy-subrc <> 0.
          " no such type at all - that is no load error
          CLEAR result.
        ENDIF.
      CATCH cx_root INTO result ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_ref_type_name.

    DATA lo_refdescr TYPE REF TO cl_abap_refdescr.

    TRY.
        " the relative name, not the absolute one: `\INTERFACE=ZIF_ANY` on a
        " system, `\CLASS=ZIF_ANY` in the transpiled runtime - the relative
        " name is `ZIF_ANY` on both
        lo_refdescr ?= cl_abap_typedescr=>describe_by_data( val ).
        result = lo_refdescr->get_referenced_type( )->get_relative_name( ).
      CATCH cx_root INTO DATA(lx).
        " not a reference variable - a programming error at the caller
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `RTTI_NOT_AN_OBJECT_REFERENCE`
            previous = lx.
    ENDTRY.

  ENDMETHOD.

  METHOD time_diff_milliseconds.

    " Plain arithmetic on the digits, and deliberately neither of the two
    " obvious candidates. cl_abap_tstmp=>subtract answers whole seconds in
    " open-abap - the runtime of the transpiled backend (npm run unit, the
    " node runtime) - so every interval under a second read 0 there. GET RUN
    " TIME counts microseconds since the first call of the internal session
    " on a system, but milliseconds since the PREVIOUS call in the transpiled
    " runtime, so the difference of two readings means something else there.
    " GET TIME STAMP plus the arithmetic below means the same everywhere -
    " standard ABAP, ABAP Cloud, 7.02 and the transpiled runtime, at whatever
    " precision the platform's clock has (milliseconds in the last one).
    IF time_from IS INITIAL OR time_to IS INITIAL.
      RETURN.
    ENDIF.

    result = ( time_get_seconds( time_to ) - time_get_seconds( time_from ) ) * 1000.

  ENDMETHOD.

  METHOD time_get_iso_by_stamp.

    " the statements of ajson's format_timestamp, unchanged - the zone as
    " a C(6) constant, the date and the time initial before the
    " conversion, which leaves them alone for a time stamp it refuses
    CONSTANTS lc_utc TYPE c LENGTH 6 VALUE 'UTC'.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    CONVERT TIME STAMP val TIME ZONE lc_utc
      INTO DATE lv_date TIME lv_time.

    result = lv_date+0(4) && `-` && lv_date+4(2) && `-` && lv_date+6(2) &&
             `T` &&
             lv_time+0(2) && `:` && lv_time+2(2) && `:` && lv_time+4(2) &&
             `Z`.

  ENDMETHOD.

  METHOD time_get_iso_by_stampl.

    " the statements of ajson's format_timestampl, unchanged - see
    " time_get_iso_by_stamp; the fraction is taken from the rendered
    " number, not computed
    CONSTANTS lc_utc TYPE c LENGTH 6 VALUE 'UTC'.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_frac TYPE string.
    DATA lv_int  TYPE string.

    CONVERT TIME STAMP val TIME ZONE lc_utc
      INTO DATE lv_date TIME lv_time.

    SPLIT |{ val }| AT `.` INTO lv_int lv_frac.
    lv_frac = shift_right( val = lv_frac
                           sub = `0` ).
    lv_frac = shift_left( val = lv_frac
                          sub = ` ` ).
    IF lv_frac IS INITIAL.
      lv_frac = `0`.
    ENDIF.

    result = lv_date+0(4) && `-` && lv_date+4(2) && `-` && lv_date+6(2) &&
             `T` &&
             lv_time+0(2) && `:` && lv_time+2(2) && `:` && lv_time+4(2) &&
             `.` && lv_frac &&
             `Z`.

  ENDMETHOD.

  METHOD time_get_seconds.

    CONSTANTS lc_epoch TYPE d VALUE '19700101'.
    DATA lv_text TYPE string.
    DATA lv_frac TYPE string.
    DATA lv_date TYPE d.
    DATA lv_hour TYPE i.
    DATA lv_min  TYPE i.
    DATA lv_sec  TYPE i.

    " Read off the digits of YYYYMMDDhhmmss.fffffff, not computed with DIV
    " and MOD on the number: the transpiled runtime holds a packed value as
    " a JavaScript double, and fourteen integer digits leave it a resolution
    " of about 4 ms - 20261003101500.35 is 20261003101500.34765625 there -
    " while its string conversion renders the declared decimals exactly. A
    " system computes exactly either way. The date goes through the date
    " type, which then counts days - a NUMBER assigned to a date would be
    " read as days since 0001-01-01, not as YYYYMMDD
    lv_text = |{ time }|.
    SPLIT lv_text AT `.` INTO lv_text lv_frac.
    IF strlen( lv_text ) <> 14.
      RETURN.
    ENDIF.

    lv_date = lv_text.
    lv_hour = substring( val = lv_text
                         off = 8
                         len = 2 ).
    lv_min  = substring( val = lv_text
                         off = 10
                         len = 2 ).
    lv_sec  = substring( val = lv_text
                         off = 12
                         len = 2 ).
    lv_frac = substring( val = lv_frac && `0000000`
                         len = 7 ).

    result = ( lv_date - lc_epoch ) * 86400 + lv_hour * 3600 + lv_min * 60 + lv_sec.
    result = result + lv_frac / 10000000.

  ENDMETHOD.

  METHOD xml_srtti_descr.

    " the shipped S-RTTI copy is named through a typed reference, not a
    " literal, so a namespace rename carries it along (rtti_get_ref_type_name).
    " ZCL_SRTTI_TYPEDESCR is the stand-alone S-RTTI project, installed
    " separately or not at all - a name this repository does not own
    DATA lo_shipped TYPE REF TO zabaputil_cl_srt_typedescr.
    DATA(lv_classname) = COND string( WHEN rtti_check_class_exists( `ZCL_SRTTI_TYPEDESCR` ) = abap_true
                                      THEN `ZCL_SRTTI_TYPEDESCR`
                                      ELSE rtti_get_ref_type_name( lo_shipped ) ).
    CALL METHOD (lv_classname)=>(`CREATE_BY_DATA_OBJECT`)
      EXPORTING
        data_object = data
      RECEIVING
        srtti       = result.

  ENDMETHOD.

  METHOD xml_srtti_parse_pair.

    DATA srtti TYPE REF TO object.
    CALL TRANSFORMATION id SOURCE XML iv_type RESULT srtti = srtti.

    DATA rtti_type TYPE REF TO cl_abap_typedescr.
    CALL METHOD srtti->(`GET_RTTI`)
      RECEIVING
        rtti = rtti_type.

    DATA lo_datadescr TYPE REF TO cl_abap_datadescr.
    lo_datadescr ?= rtti_type.

    CREATE DATA result TYPE HANDLE lo_datadescr.
    ASSIGN result->* TO FIELD-SYMBOL(<variable>).
    CALL TRANSFORMATION id SOURCE XML iv_data RESULT dobj = <variable>.

  ENDMETHOD.

  METHOD xml_srtti_stringify_pair.

    TRY.
        DATA(lo_srtti) = xml_srtti_descr( data ).
        CALL TRANSFORMATION id SOURCE srtti = lo_srtti RESULT XML ev_type.
        CALL TRANSFORMATION id SOURCE dobj = data RESULT XML ev_data.
      CATCH cx_root INTO DATA(lx_srtti).
        " keep the root cause - a transformation error on the app's own
        " data must not be masked behind a bare UNSUPPORTED_FEATURE
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `UNSUPPORTED_FEATURE`
            previous = lx_srtti.
    ENDTRY.

  ENDMETHOD.

  METHOD ui5_get_type_require.

    result = `{DateType: 'sap/ui/model/type/Date', TimeType: 'sap/ui/model/type/Time', `
          && `FloatType: 'sap/ui/model/type/Float', IntType: 'sap/ui/model/type/Integer', `
          && `CurrType: 'sap/ui/model/type/Currency', UnitType: 'sap/ui/model/type/Unit'}`.

  ENDMETHOD.

  METHOD ui5_get_model_type.

    IF check_currency = abap_true.
      result = `CurrType`.
      RETURN.
    ENDIF.
    IF check_unit = abap_true.
      result = `UnitType`.
      RETURN.
    ENDIF.

    CASE type_kind.
      WHEN cl_abap_typedescr=>typekind_date.
        result = `DateType`.
      WHEN cl_abap_typedescr=>typekind_time.
        result = `TimeType`.
      WHEN cl_abap_typedescr=>typekind_int
          OR cl_abap_typedescr=>typekind_int1
          OR cl_abap_typedescr=>typekind_int2
          OR `8`.
        " `8`: typekind_int8, a constant lower releases do not have
        result = `IntType`.
      WHEN cl_abap_typedescr=>typekind_packed
          OR cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34.
        result = `FloatType`.
    ENDCASE.

  ENDMETHOD.

  METHOD ui5_get_format_options.

    IF check_currency = abap_true OR check_unit = abap_true.
      " the digits come with the currency or the unit (CLDR), the code is
      " a column or a field of its own
      result = `{showMeasure: false}`.
      RETURN.
    ENDIF.

    CASE ui5_get_model_type( type_kind ).
      WHEN `DateType`.
        result = `{source: {pattern: 'yyyy-MM-dd'}}`.
      WHEN `TimeType`.
        result = `{source: {pattern: 'HH:mm:ss'}}`.
      WHEN `IntType`.
        result = `{groupingEnabled: true}`.
      WHEN `FloatType`.
        IF type_kind = cl_abap_typedescr=>typekind_packed.
          result = |\{minFractionDigits: { decimals }, maxFractionDigits: { decimals }, groupingEnabled: true\}|.
        ELSE.
          result = `{groupingEnabled: true}`.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD ui5_binding_by_kind.

    DATA(lv_currency) = xsdbool( currency_path IS NOT INITIAL ).
    DATA(lv_unit) = xsdbool( lv_currency = abap_false AND unit_path IS NOT INITIAL ).

    DATA(lv_type) = ui5_get_model_type( type_kind      = type_kind
                                        check_currency = lv_currency
                                        check_unit     = lv_unit ).
    IF lv_type IS INITIAL.
      result = |\{{ path }\}|.
      RETURN.
    ENDIF.

    DATA(lv_options) = ui5_get_format_options( type_kind      = type_kind
                                               decimals       = decimals
                                               check_currency = lv_currency
                                               check_unit     = lv_unit ).

    IF lv_currency = abap_true.
      result = |\{parts: [\{path: '{ path }'\}, \{path: '{ currency_path }'\}], type: '{ lv_type }', formatOptions: { lv_options }\}|.
    ELSEIF lv_unit = abap_true.
      result = |\{parts: [\{path: '{ path }'\}, \{path: '{ unit_path }'\}], type: '{ lv_type }', formatOptions: { lv_options }\}|.
    ELSE.
      result = |\{path: '{ path }', type: '{ lv_type }', formatOptions: { lv_options }\}|.
    ENDIF.

  ENDMETHOD.

  METHOD ui5_binding_typed.

    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( val ).
    " a TIMESTAMP / TIMESTAMPL is a packed number here and an ISO text in
    " the model - a FloatType would refuse to parse it
    IF lo_descr->kind <> cl_abap_typedescr=>kind_elem
        OR lo_descr->absolute_name = `\TYPE=TIMESTAMP`
        OR lo_descr->absolute_name = `\TYPE=TIMESTAMPL`.
      result = |\{{ path }\}|.
      RETURN.
    ENDIF.

    DATA(lo_elem) = CAST cl_abap_elemdescr( lo_descr ).
    result = ui5_binding_by_kind( path          = path
                                  type_kind     = lo_descr->type_kind
                                  decimals      = lo_elem->decimals
                                  currency_path = currency_path
                                  unit_path     = unit_path ).

  ENDMETHOD.

  METHOD ui5_binding_typed_comp.

    DATA lv_currency TYPE string.
    DATA lv_unit     TYPE string.
    FIELD-SYMBOLS <val> TYPE any.

    DATA(lv_comp) = to_upper( condense( comp ) ).
    ASSIGN COMPONENT lv_comp OF STRUCTURE struc TO <val>.
    IF sy-subrc <> 0.
      result = |\{{ lv_comp }\}|.
      RETURN.
    ENDIF.

    DATA(ls_ref) = rtti_get_ref_field( struc = struc
                                       comp  = lv_comp ).
    IF ls_ref-currency = abap_true.
      lv_currency = ls_ref-name.
    ELSE.
      lv_unit = ls_ref-name.
    ENDIF.

    result = ui5_binding_typed( path          = lv_comp
                                val           = <val>
                                currency_path = lv_currency
                                unit_path     = lv_unit ).

  ENDMETHOD.

  METHOD rtti_get_ref_field.

    DATA lr_fields TYPE REF TO data.
    DATA lo_type   TYPE REF TO cl_abap_datadescr.
    DATA lv_ref    TYPE string.
    FIELD-SYMBOLS <fields>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <field>    TYPE any.
    FIELD-SYMBOLS <name>     TYPE any.
    FIELD-SYMBOLS <reffield> TYPE any.
    FIELD-SYMBOLS <datatype> TYPE any.
    FIELD-SYMBOLS <ref>      TYPE any.

    DATA(lv_comp) = to_upper( condense( comp ) ).

    TRY.
        DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( struc ).
        IF lo_descr->kind <> cl_abap_typedescr=>kind_struct OR lo_descr->is_ddic_type( ) = abap_false.
          RETURN.
        ENDIF.
        DATA(lo_struct) = CAST cl_abap_structdescr( lo_descr ).

        " DDFIELDS is not released on ABAP Cloud - there the lookup raises
        " and the field has no reference field
        lo_type ?= cl_abap_typedescr=>describe_by_name( `DDFIELDS` ).
        CREATE DATA lr_fields TYPE HANDLE lo_type.
        ASSIGN lr_fields->* TO <fields>.

        CALL METHOD lo_struct->(`GET_DDIC_FIELD_LIST`)
          RECEIVING
            p_field_list = <fields>
          EXCEPTIONS
            not_found    = 1
            no_ddic_type = 2
            OTHERS       = 3.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        LOOP AT <fields> ASSIGNING <field>.
          UNASSIGN: <name>, <reffield>, <datatype>.
          ASSIGN COMPONENT `FIELDNAME` OF STRUCTURE <field> TO <name>.
          IF <name> IS NOT ASSIGNED.
            RETURN.
          ENDIF.
          IF to_upper( condense( CONV string( <name> ) ) ) <> lv_comp.
            CONTINUE.
          ENDIF.

          ASSIGN COMPONENT `REFFIELD` OF STRUCTURE <field> TO <reffield>.
          ASSIGN COMPONENT `DATATYPE` OF STRUCTURE <field> TO <datatype>.
          IF <reffield> IS NOT ASSIGNED OR <datatype> IS NOT ASSIGNED.
            RETURN.
          ENDIF.
          IF <datatype> <> `CURR` AND <datatype> <> `QUAN`.
            RETURN.
          ENDIF.
          lv_ref = to_upper( condense( CONV string( <reffield> ) ) ).
          IF lv_ref IS INITIAL.
            RETURN.
          ENDIF.
          " the DDIC may name a field of another table (T001-WAERS) - only
          " a field of the same row can be bound beside the amount
          ASSIGN COMPONENT lv_ref OF STRUCTURE struc TO <ref>.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.

          result-name     = lv_ref.
          result-currency = xsdbool( <datatype> = `CURR` ).
          RETURN.
        ENDLOOP.

      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD barcode_gs1_parse.

    DATA lv_gs TYPE string.
    DATA lv_value TYPE string.
    DATA lv_ai TYPE string.

    lv_gs = conv_get_string_by_xstring( CONV xstring( `1D` ) ).
    DATA(lv_val) = CONV string( val ).
    DATA(lv_sep) = CONV string( separator ).
    IF lv_sep IS NOT INITIAL.
      lv_val = replace( val  = lv_val
                        sub  = lv_sep
                        with = lv_gs
                        occ  = 0 ).
    ENDIF.
    " scanners in keyboard mode end the scan with CR/LF
    lv_val = replace( val  = lv_val
                      sub  = cv_char_util_cr_lf
                      with = ``
                      occ  = 0 ).
    lv_val = replace( val  = lv_val
                      sub  = cv_char_util_newline
                      with = ``
                      occ  = 0 ).
    lv_val = c_trim( lv_val ).

    " symbology identifier: ]C1 GS1-128, ]e0 DataBar, ]d2 DataMatrix, ]Q3 QR
    IF strlen( lv_val ) >= 3 AND lv_val(1) = `]`.
      lv_val = substring( val = lv_val
                          off = 3 ).
    ENDIF.

    DATA(lv_ref_date) = COND d( WHEN ref_date IS INITIAL THEN sy-datum ELSE ref_date ).

    IF strlen( lv_val ) > 0 AND lv_val(1) = `(`.

      " human-readable form (AI)value(AI)value
      SPLIT lv_val AT `(` INTO TABLE DATA(lt_part).
      LOOP AT lt_part INTO DATA(lv_part).
        IF lv_part IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA(lv_close) = find( val = lv_part
                               sub = `)` ).
        IF lv_close <= 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |GS1: missing closing bracket after ({ lv_part }|.
        ENDIF.
        lv_ai = lv_part(lv_close).
        lv_value = substring( val = lv_part
                              off = lv_close + 1 ).
        lv_value = replace( val  = lv_value
                            sub  = lv_gs
                            with = ``
                            occ  = 0 ).
        DATA(ls_rule) = barcode_gs1_find_rule( lv_ai ).
        IF ls_rule IS INITIAL OR ls_rule-ai_len <> strlen( lv_ai ) OR lv_ai CN `0123456789`.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |GS1: unknown application identifier ({ lv_ai })|.
        ENDIF.
        IF ls_rule-var = abap_false AND strlen( lv_value ) <> ls_rule-len.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |GS1: AI ({ lv_ai }) needs exactly { ls_rule-len } characters|.
        ENDIF.
        INSERT barcode_gs1_get_element( rule        = ls_rule
                                        ai          = lv_ai
                                        value       = lv_value
                                        check_digit = check_digit
                                        ref_date    = lv_ref_date ) INTO TABLE result.
      ENDLOOP.
      RETURN.

    ENDIF.

    DATA(lv_len) = strlen( lv_val ).
    DATA(lv_pos) = 0.
    WHILE lv_pos < lv_len.

      " a separator at the start (FNC1 sent as GS) or after a fixed-length
      " field is allowed and carries no data
      IF substring( val = lv_val
                    off = lv_pos
                    len = 1 ) = lv_gs.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      DATA(lv_rest) = substring( val = lv_val
                                 off = lv_pos ).
      ls_rule = barcode_gs1_find_rule( lv_rest ).
      IF ls_rule IS INITIAL OR strlen( lv_rest ) < ls_rule-ai_len.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |GS1: unknown application identifier at position { lv_pos + 1 }: { c_truncate( val = lv_rest
                                                                                              max = 8 ) }|.
      ENDIF.
      lv_ai = lv_rest(ls_rule-ai_len).
      IF lv_ai CN `0123456789`.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |GS1: unknown application identifier ({ lv_ai })|.
      ENDIF.
      DATA(lv_data) = substring( val = lv_rest
                                 off = ls_rule-ai_len ).

      IF ls_rule-var = abap_false.
        IF strlen( lv_data ) < ls_rule-len.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |GS1: AI ({ lv_ai }) needs { ls_rule-len } characters, only { strlen( lv_data ) } left|.
        ENDIF.
        lv_value = lv_data(ls_rule-len).
      ELSE.
        DATA(lv_end) = find( val = lv_data
                             sub = lv_gs ).
        IF lv_end < 0.
          lv_value = lv_data.
        ELSE.
          lv_value = lv_data(lv_end).
        ENDIF.
      ENDIF.

      INSERT barcode_gs1_get_element( rule        = ls_rule
                                      ai          = lv_ai
                                      value       = lv_value
                                      check_digit = check_digit
                                      ref_date    = lv_ref_date ) INTO TABLE result.
      lv_pos = lv_pos + ls_rule-ai_len + strlen( lv_value ).

    ENDWHILE.

  ENDMETHOD.

  METHOD barcode_gs1_get_element.

    DATA(lv_len) = strlen( value ).
    IF lv_len = 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: AI ({ ai }) has no value|.
    ENDIF.
    IF lv_len > rule-len.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: AI ({ ai }) allows at most { rule-len } characters, got { lv_len }|.
    ENDIF.
    IF rule-numeric = abap_true AND value CN `0123456789`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: AI ({ ai }) allows digits only: { value }|.
    ENDIF.
    IF rule-check = abap_true AND check_digit = abap_true AND check_is_gs1_check_digit( value ) = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: wrong check digit in AI ({ ai }): { value }|.
    ENDIF.

    result-ai    = ai.
    result-title = rule-title.
    result-value = value.
    IF rule-decimals = abap_true.
      result-decimals = substring( val = ai
                                   off = 3
                                   len = 1 ).
    ENDIF.
    IF rule-is_date = abap_true.
      result-date = barcode_gs1_get_date( val      = value
                                          ref_date = ref_date ).
    ENDIF.

  ENDMETHOD.

  METHOD barcode_gs1_get_date.

    DATA lv_next TYPE d.

    DATA(lv_yy) = CONV i( val(2) ).
    DATA(lv_mm) = CONV i( val+2(2) ).
    DATA(lv_dd) = CONV i( val+4(2) ).

    " GS1 General Specifications 7.12: a year more than 50 years ahead is in
    " the past century, one more than 49 years back in the next century
    DATA(lv_ref_year) = CONV i( ref_date(4) ).
    DATA(lv_century) = lv_ref_year - lv_ref_year MOD 100.
    DATA(lv_diff) = lv_yy - lv_ref_year MOD 100.
    IF lv_diff >= 51.
      lv_century = lv_century - 100.
    ELSEIF lv_diff <= -50.
      lv_century = lv_century + 100.
    ENDIF.
    DATA(lv_year) = lv_century + lv_yy.

    IF lv_mm < 1 OR lv_mm > 12.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: invalid date { val }|.
    ENDIF.

    " last day of the month = first day of the next month - 1
    IF lv_mm = 12.
      lv_next = |{ lv_year + 1 WIDTH = 4 ALIGN = RIGHT PAD = `0` }0101|.
    ELSE.
      lv_next = |{ lv_year WIDTH = 4 ALIGN = RIGHT PAD = `0` }{ lv_mm + 1 WIDTH = 2 ALIGN = RIGHT PAD = `0` }01|.
    ENDIF.
    DATA(lv_last) = CONV d( lv_next - 1 ).
    DATA(lv_last_day) = CONV i( lv_last+6(2) ).

    IF lv_dd = 0.
      result = lv_last.
    ELSEIF lv_dd > lv_last_day.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: invalid date { val }|.
    ELSE.
      result = |{ lv_year WIDTH = 4 ALIGN = RIGHT PAD = `0` }{ val+2(4) }|.
    ENDIF.

  ENDMETHOD.

  METHOD barcode_gs1_build.

    DATA lv_need_sep TYPE abap_bool.

    DATA(lv_sep) = CONV string( separator ).
    IF lv_sep IS INITIAL.
      lv_sep = conv_get_string_by_xstring( CONV xstring( `1D` ) ).
    ENDIF.

    LOOP AT t_gs1 INTO DATA(ls_gs1).
      IF human_readable = abap_true.
        result = |{ result }({ ls_gs1-ai }){ ls_gs1-value }|.
        CONTINUE.
      ENDIF.
      DATA(ls_rule) = barcode_gs1_find_rule( ls_gs1-ai ).
      IF ls_rule IS INITIAL OR ls_rule-ai_len <> strlen( ls_gs1-ai ).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |GS1: unknown application identifier ({ ls_gs1-ai })|.
      ENDIF.
      IF lv_need_sep = abap_true.
        result = result && lv_sep.
      ENDIF.
      result = result && ls_gs1-ai && ls_gs1-value.
      lv_need_sep = ls_rule-var.
    ENDLOOP.

  ENDMETHOD.

  METHOD barcode_gs1_get_value.

    DATA(lv_ai) = CONV string( ai ).

    LOOP AT t_gs1 INTO DATA(ls_gs1) WHERE ai = lv_ai.
      result = ls_gs1-value.
      RETURN.
    ENDLOOP.

    DATA(lv_len) = strlen( lv_ai ).
    LOOP AT t_gs1 INTO ls_gs1.
      IF strlen( ls_gs1-ai ) > lv_len AND substring( val = ls_gs1-ai
                                                     len = lv_len ) = lv_ai.
        result = ls_gs1-value.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD barcode_split_get_scan.

    result = replace( val  = CONV string( val )
                      sub  = cv_char_util_cr_lf
                      with = ``
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = cv_char_util_newline
                      with = ``
                      occ  = 0 ).

  ENDMETHOD.

  METHOD barcode_split_add_value.

    IF found = abap_false OR value IS INITIAL.
      IF field-optional = abap_false.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: field { field-name } is missing|.
      ENDIF.
      RETURN.
    ENDIF.

    IF field-len > 0 AND strlen( value ) > field-len.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |Barcode: field { field-name } allows at most { field-len } characters, got { strlen( value ) }|.
    ENDIF.

    INSERT VALUE #( name  = field-name
                    value = value ) INTO TABLE result.

  ENDMETHOD.

  METHOD barcode_split_separator.

    DATA lv_value TYPE string.

    DATA(lv_sep) = CONV string( separator ).
    IF lv_sep IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `Barcode: no separator given`.
    ENDIF.

    DATA(lt_part) = c_split( val = barcode_split_get_scan( val )
                             sep = lv_sep ).
    IF lines( lt_part ) > lines( t_field ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |Barcode: { lines( lt_part ) } parts, but only { lines( t_field ) } fields|.
    ENDIF.

    LOOP AT t_field INTO DATA(ls_field).
      DATA(lv_tabix) = sy-tabix.
      CLEAR lv_value.
      DATA(lv_found) = abap_false.
      IF lv_tabix <= lines( lt_part ).
        lv_value = lt_part[ lv_tabix ].
        lv_found = abap_true.
      ENDIF.
      barcode_split_add_value( EXPORTING field  = ls_field
                                         value  = lv_value
                                         found  = lv_found
                               CHANGING  result = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD barcode_split_position.

    DATA lv_value TYPE string.

    DATA(lv_val) = barcode_split_get_scan( val ).
    DATA(lv_len) = strlen( lv_val ).

    LOOP AT t_field INTO DATA(ls_field).
      IF ls_field-pos < 1 OR ls_field-len < 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: field { ls_field-name } needs a position from 1 on|.
      ENDIF.

      CLEAR lv_value.
      DATA(lv_found) = abap_false.
      DATA(lv_off) = ls_field-pos - 1.
      IF lv_off < lv_len.
        lv_found = abap_true.
        DATA(lv_take) = lv_len - lv_off.
        IF ls_field-len > 0 AND ls_field-len < lv_take.
          lv_take = ls_field-len.
        ENDIF.
        " the padding of a fixed layout is no part of the value
        lv_value = c_trim( substring( val = lv_val
                                      off = lv_off
                                      len = lv_take ) ).
      ENDIF.

      " len is where the field ends here, not a limit to check a value against
      DATA(ls_check) = ls_field.
      ls_check-len = 0.
      barcode_split_add_value( EXPORTING field  = ls_check
                                         value  = lv_value
                                         found  = lv_found
                               CHANGING  result = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD barcode_split_prefix.

    DATA lt_found TYPE ty_t_barcode_value.

    DATA(lv_sep) = CONV string( separator ).
    IF lv_sep IS INITIAL.
      lv_sep = conv_get_string_by_xstring( CONV xstring( `1D` ) ).
    ENDIF.

    LOOP AT c_split( val = barcode_split_get_scan( val )
                     sep = lv_sep ) INTO DATA(lv_part).
      IF lv_part IS INITIAL.
        CONTINUE.
      ENDIF.

      " the longest prefix that fits: `1T` must not be read as `1`
      DATA(ls_hit) = VALUE ty_s_barcode_field( ).
      DATA(lv_hit) = abap_false.
      LOOP AT t_field INTO DATA(ls_field) WHERE prefix IS NOT INITIAL.
        DATA(lv_plen) = strlen( ls_field-prefix ).
        IF strlen( lv_part ) < lv_plen OR substring( val = lv_part
                                                     len = lv_plen ) <> ls_field-prefix.
          CONTINUE.
        ENDIF.
        IF lv_hit = abap_false OR lv_plen > strlen( ls_hit-prefix ).
          ls_hit = ls_field.
          lv_hit = abap_true.
        ENDIF.
      ENDLOOP.
      IF lv_hit = abap_false.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: no field starts like { c_truncate( val = lv_part
                                                               max = 20 ) }|.
      ENDIF.

      IF line_exists( lt_found[ name = ls_hit-name ] ).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: field { ls_hit-name } comes twice|.
      ENDIF.
      INSERT VALUE #( name  = ls_hit-name
                      value = substring( val = lv_part
                                         off = strlen( ls_hit-prefix ) ) ) INTO TABLE lt_found.
    ENDLOOP.

    LOOP AT t_field INTO ls_field WHERE prefix IS NOT INITIAL.
      DATA(lv_value) = ``.
      DATA(lv_found) = abap_false.
      IF line_exists( lt_found[ name = ls_field-name ] ).
        lv_value = lt_found[ name = ls_field-name ]-value.
        lv_found = abap_true.
      ENDIF.
      barcode_split_add_value( EXPORTING field  = ls_field
                                         value  = lv_value
                                         found  = lv_found
                               CHANGING  result = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD barcode_gs1_calc_check_digit.

    DATA lv_sum TYPE i.

    DATA(lv_val) = c_trim( CONV string( val ) ).
    IF lv_val IS INITIAL OR lv_val CN `0123456789`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: check digit needs digits only: { lv_val }|.
    ENDIF.

    " weights 3, 1, 3, 1, ... starting with the rightmost digit
    DATA(lv_weight) = 3.
    DATA(lv_pos) = strlen( lv_val ) - 1.
    WHILE lv_pos >= 0.
      lv_sum = lv_sum + CONV i( substring( val = lv_val
                                           off = lv_pos
                                           len = 1 ) ) * lv_weight.
      lv_weight = 4 - lv_weight.
      lv_pos = lv_pos - 1.
    ENDWHILE.

    result = |{ ( 10 - lv_sum MOD 10 ) MOD 10 }|.

  ENDMETHOD.

  METHOD check_is_gs1_check_digit.

    DATA(lv_val) = c_trim( CONV string( val ) ).
    DATA(lv_len) = strlen( lv_val ).
    IF lv_len < 2 OR lv_val CN `0123456789`.
      result = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_body) = substring( val = lv_val
                               len = lv_len - 1 ).
    DATA(lv_digit) = substring( val = lv_val
                                off = lv_len - 1 ).
    result = xsdbool( barcode_gs1_calc_check_digit( lv_body ) = lv_digit ).

  ENDMETHOD.

  METHOD barcode_gs1_find_rule.

    IF gt_gs1_rule IS INITIAL.
      gt_gs1_rule = barcode_gs1_get_rules( ).
    ENDIF.

    " the longest prefix first: 4-digit AIs (7003), then the 3-digit ones and
    " the 3-digit prefixes of the measure AIs (310n), then the 2-digit AIs
    DATA(lv_len) = 4.
    WHILE lv_len >= 2.
      IF strlen( val ) >= lv_len.
        DATA(lv_prefix) = substring( val = val
                                     len = lv_len ).
        LOOP AT gt_gs1_rule INTO result WHERE prefix = lv_prefix.
          RETURN.
        ENDLOOP.
      ENDIF.
      lv_len = lv_len - 1.
    ENDWHILE.
    CLEAR result.

  ENDMETHOD.

  METHOD barcode_gs1_get_rules.

    " GS1 General Specifications, section 3 (application identifiers):
    " len = fixed length, or the maximum length with var = abap_true
    result = VALUE #(
      ( prefix = `00`   ai_len = 2 len = 18 numeric = abap_true check = abap_true title = `SSCC` )
      ( prefix = `01`   ai_len = 2 len = 14 numeric = abap_true check = abap_true title = `GTIN` )
      ( prefix = `02`   ai_len = 2 len = 14 numeric = abap_true check = abap_true title = `CONTENT` )
      ( prefix = `03`   ai_len = 2 len = 14 numeric = abap_true check = abap_true title = `MTO GTIN` )
      ( prefix = `10`   ai_len = 2 len = 20 var = abap_true title = `BATCH/LOT` )
      ( prefix = `11`   ai_len = 2 len = 6 numeric = abap_true is_date = abap_true title = `PROD DATE` )
      ( prefix = `12`   ai_len = 2 len = 6 numeric = abap_true is_date = abap_true title = `DUE DATE` )
      ( prefix = `13`   ai_len = 2 len = 6 numeric = abap_true is_date = abap_true title = `PACK DATE` )
      ( prefix = `15`   ai_len = 2 len = 6 numeric = abap_true is_date = abap_true title = `BEST BEFORE or BEST BY` )
      ( prefix = `16`   ai_len = 2 len = 6 numeric = abap_true is_date = abap_true title = `SELL BY` )
      ( prefix = `17`   ai_len = 2 len = 6 numeric = abap_true is_date = abap_true title = `USE BY or EXPIRY` )
      ( prefix = `20`   ai_len = 2 len = 2 numeric = abap_true title = `VARIANT` )
      ( prefix = `21`   ai_len = 2 len = 20 var = abap_true title = `SERIAL` )
      ( prefix = `22`   ai_len = 2 len = 20 var = abap_true title = `CPV` )
      ( prefix = `235`  ai_len = 3 len = 28 var = abap_true title = `TPX` )
      ( prefix = `240`  ai_len = 3 len = 30 var = abap_true title = `ADDITIONAL ID` )
      ( prefix = `241`  ai_len = 3 len = 30 var = abap_true title = `CUST. PART No.` )
      ( prefix = `242`  ai_len = 3 len = 6 var = abap_true numeric = abap_true title = `MTO VARIANT` )
      ( prefix = `243`  ai_len = 3 len = 20 var = abap_true title = `PCN` )
      ( prefix = `250`  ai_len = 3 len = 30 var = abap_true title = `SECONDARY SERIAL` )
      ( prefix = `251`  ai_len = 3 len = 30 var = abap_true title = `REF. TO SOURCE` )
      ( prefix = `253`  ai_len = 3 len = 30 var = abap_true title = `GDTI` )
      ( prefix = `254`  ai_len = 3 len = 20 var = abap_true title = `GLN EXTENSION COMPONENT` )
      ( prefix = `255`  ai_len = 3 len = 25 var = abap_true numeric = abap_true title = `GCN` )
      ( prefix = `30`   ai_len = 2 len = 8 var = abap_true numeric = abap_true title = `VAR. COUNT` )
      ( prefix = `310`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET WEIGHT (kg)` )
      ( prefix = `311`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (m)` )
      ( prefix = `312`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (m)` )
      ( prefix = `313`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (m)` )
      ( prefix = `314`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (m2)` )
      ( prefix = `315`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET VOLUME (l)` )
      ( prefix = `316`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET VOLUME (m3)` )
      ( prefix = `320`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET WEIGHT (lb)` )
      ( prefix = `321`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (in)` )
      ( prefix = `322`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (ft)` )
      ( prefix = `323`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (yd)` )
      ( prefix = `324`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (in)` )
      ( prefix = `325`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (ft)` )
      ( prefix = `326`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (yd)` )
      ( prefix = `327`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (in)` )
      ( prefix = `328`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (ft)` )
      ( prefix = `329`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (yd)` )
      ( prefix = `330`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `GROSS WEIGHT (kg)` )
      ( prefix = `331`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (m), log` )
      ( prefix = `332`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (m), log` )
      ( prefix = `333`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (m), log` )
      ( prefix = `334`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (m2), log` )
      ( prefix = `335`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (l), log` )
      ( prefix = `336`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (m3), log` )
      ( prefix = `337`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `KG PER m2` )
      ( prefix = `340`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `GROSS WEIGHT (lb)` )
      ( prefix = `341`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (in), log` )
      ( prefix = `342`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (ft), log` )
      ( prefix = `343`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `LENGTH (yd), log` )
      ( prefix = `344`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (in), log` )
      ( prefix = `345`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (ft), log` )
      ( prefix = `346`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `WIDTH (yd), log` )
      ( prefix = `347`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (in), log` )
      ( prefix = `348`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (ft), log` )
      ( prefix = `349`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `HEIGHT (yd), log` )
      ( prefix = `350`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (in2)` )
      ( prefix = `351`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (ft2)` )
      ( prefix = `352`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (yd2)` )
      ( prefix = `353`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (in2), log` )
      ( prefix = `354`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (ft2), log` )
      ( prefix = `355`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `AREA (yd2), log` )
      ( prefix = `356`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET WEIGHT (t oz)` )
      ( prefix = `357`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET VOLUME (oz)` )
      ( prefix = `360`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET VOLUME (qt)` )
      ( prefix = `361`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `NET VOLUME (g)` )
      ( prefix = `362`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (qt), log` )
      ( prefix = `363`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (g), log` )
      ( prefix = `364`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (in3)` )
      ( prefix = `365`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (ft3)` )
      ( prefix = `366`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (yd3)` )
      ( prefix = `367`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (in3), log` )
      ( prefix = `368`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (ft3), log` )
      ( prefix = `369`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `VOLUME (yd3), log` )
      ( prefix = `37`   ai_len = 2 len = 8 var = abap_true numeric = abap_true title = `COUNT` )
      ( prefix = `390`  ai_len = 4 len = 15 var = abap_true numeric = abap_true decimals = abap_true title = `AMOUNT` )
      ( prefix = `391`  ai_len = 4 len = 18 var = abap_true numeric = abap_true decimals = abap_true title = `AMOUNT` )
      ( prefix = `392`  ai_len = 4 len = 15 var = abap_true numeric = abap_true decimals = abap_true title = `PRICE` )
      ( prefix = `393`  ai_len = 4 len = 18 var = abap_true numeric = abap_true decimals = abap_true title = `PRICE` )
      ( prefix = `394`  ai_len = 4 len = 4 numeric = abap_true decimals = abap_true title = `PRCNT OFF` )
      ( prefix = `395`  ai_len = 4 len = 6 numeric = abap_true decimals = abap_true title = `PRICE/UoM` )
      ( prefix = `400`  ai_len = 3 len = 30 var = abap_true title = `ORDER NUMBER` )
      ( prefix = `401`  ai_len = 3 len = 30 var = abap_true title = `GINC` )
      ( prefix = `402`  ai_len = 3 len = 17 numeric = abap_true title = `GSIN` )
      ( prefix = `403`  ai_len = 3 len = 30 var = abap_true title = `ROUTE` )
      ( prefix = `410`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `SHIP TO LOC` )
      ( prefix = `411`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `BILL TO` )
      ( prefix = `412`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `PURCHASE FROM` )
      ( prefix = `413`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `SHIP FOR LOC` )
      ( prefix = `414`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `LOC No.` )
      ( prefix = `415`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `PAY TO` )
      ( prefix = `416`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `PROD/SERV LOC` )
      ( prefix = `417`  ai_len = 3 len = 13 numeric = abap_true check = abap_true title = `PARTY` )
      ( prefix = `420`  ai_len = 3 len = 20 var = abap_true title = `SHIP TO POST` )
      ( prefix = `421`  ai_len = 3 len = 12 var = abap_true title = `SHIP TO POST` )
      ( prefix = `422`  ai_len = 3 len = 3 numeric = abap_true title = `ORIGIN` )
      ( prefix = `423`  ai_len = 3 len = 15 var = abap_true numeric = abap_true title = `COUNTRY - INITIAL PROCESS.` )
      ( prefix = `424`  ai_len = 3 len = 3 numeric = abap_true title = `COUNTRY - PROCESS.` )
      ( prefix = `425`  ai_len = 3 len = 15 var = abap_true numeric = abap_true title = `COUNTRY - DISASSEMBLY` )
      ( prefix = `426`  ai_len = 3 len = 3 numeric = abap_true title = `COUNTRY - FULL PROCESS` )
      ( prefix = `7001` ai_len = 4 len = 13 numeric = abap_true title = `NSN` )
      ( prefix = `7002` ai_len = 4 len = 30 var = abap_true title = `MEAT CUT` )
      ( prefix = `7003` ai_len = 4 len = 10 numeric = abap_true title = `EXPIRY TIME` )
      ( prefix = `7004` ai_len = 4 len = 4 var = abap_true numeric = abap_true title = `ACTIVE POTENCY` )
      ( prefix = `7005` ai_len = 4 len = 12 var = abap_true title = `CATCH AREA` )
      ( prefix = `7006` ai_len = 4 len = 6 numeric = abap_true is_date = abap_true title = `FIRST FREEZE DATE` )
      ( prefix = `7007` ai_len = 4 len = 12 var = abap_true numeric = abap_true title = `HARVEST DATE` )
      ( prefix = `7008` ai_len = 4 len = 3 var = abap_true title = `AQUATIC SPECIES` )
      ( prefix = `7009` ai_len = 4 len = 10 var = abap_true title = `FISHING GEAR TYPE` )
      ( prefix = `7010` ai_len = 4 len = 2 var = abap_true title = `PROD METHOD` )
      ( prefix = `8001` ai_len = 4 len = 14 numeric = abap_true title = `DIMENSIONS` )
      ( prefix = `8002` ai_len = 4 len = 20 var = abap_true title = `CMT No.` )
      ( prefix = `8003` ai_len = 4 len = 30 var = abap_true title = `GRAI` )
      ( prefix = `8004` ai_len = 4 len = 30 var = abap_true title = `GIAI` )
      ( prefix = `8005` ai_len = 4 len = 6 numeric = abap_true title = `PRICE PER UNIT` )
      ( prefix = `8006` ai_len = 4 len = 18 numeric = abap_true title = `ITIP` )
      ( prefix = `8007` ai_len = 4 len = 34 var = abap_true title = `IBAN` )
      ( prefix = `8008` ai_len = 4 len = 12 var = abap_true numeric = abap_true title = `PROD TIME` )
      ( prefix = `8010` ai_len = 4 len = 30 var = abap_true title = `CPID` )
      ( prefix = `8011` ai_len = 4 len = 12 var = abap_true numeric = abap_true title = `CPID SERIAL` )
      ( prefix = `8012` ai_len = 4 len = 20 var = abap_true title = `VERSION` )
      ( prefix = `8013` ai_len = 4 len = 25 var = abap_true title = `GMN` )
      ( prefix = `8017` ai_len = 4 len = 18 numeric = abap_true check = abap_true title = `GSRN - PROVIDER` )
      ( prefix = `8018` ai_len = 4 len = 18 numeric = abap_true check = abap_true title = `GSRN - RECIPIENT` )
      ( prefix = `8019` ai_len = 4 len = 10 var = abap_true numeric = abap_true title = `SRIN` )
      ( prefix = `8020` ai_len = 4 len = 25 var = abap_true title = `REF No.` )
      ( prefix = `8200` ai_len = 4 len = 70 var = abap_true title = `PRODUCT URL` )
      ( prefix = `90`   ai_len = 2 len = 30 var = abap_true title = `INTERNAL` )
      ( prefix = `91`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `92`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `93`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `94`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `95`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `96`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `97`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `98`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` )
      ( prefix = `99`   ai_len = 2 len = 90 var = abap_true title = `INTERNAL` ) ).

  ENDMETHOD.

  METHOD conv_get_xstring_by_bintab.

    DATA lv_hex TYPE string.
    FIELD-SYMBOLS <line> TYPE any.
    FIELD-SYMBOLS <x>    TYPE x.

    " the bytes of every line in hex, then one conversion: an xstring has no
    " append operator, and CONCATENATE ... IN BYTE MODE is not used here
    LOOP AT val ASSIGNING <line>.
      ASSIGN <line> TO <x> CASTING.
      lv_hex = lv_hex && CONV string( <x> ).
    ENDLOOP.

    IF length > 0 AND strlen( lv_hex ) > length * 2.
      lv_hex = substring( val = lv_hex
                          len = length * 2 ).
    ENDIF.
    result = lv_hex.

  ENDMETHOD.

  METHOD x_raise_on_cloud.

    IF check_abap_cloud( ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |{ method }: not available on ABAP Cloud|.
    ENDIF.

  ENDMETHOD.

  METHOD sys_langu.

    " a two-letter ISO code (EN, DE) becomes the one-letter SAP key
    DATA(lv_val) = c_trim( CONV string( val ) ).
    IF strlen( lv_val ) = 2.
      result = lang_iso_to_sap( lv_val ).
    ELSE.
      result = lv_val.
    ENDIF.

  ENDMETHOD.

  METHOD sys_fm_call.

    DATA lv_function    TYPE c LENGTH 30.
    DATA lv_destination TYPE c LENGTH 32.

    lv_function    = c_trim_upper( function ).
    lv_destination = destination.

    IF lv_destination IS INITIAL.
      CALL FUNCTION lv_function
        PARAMETER-TABLE t_param
        EXCEPTION-TABLE t_exception.
    ELSE.
      CALL FUNCTION lv_function
        DESTINATION lv_destination
        PARAMETER-TABLE t_param
        EXCEPTION-TABLE t_exception.
    ENDIF.
    result = sy-subrc.

  ENDMETHOD.

  METHOD sys_fm_get_interface.

    DATA lr_params TYPE REF TO data.
    DATA lv_funcname TYPE c LENGTH 30.
    FIELD-SYMBOLS <params> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <param>  TYPE any.
    FIELD-SYMBOLS <field>  TYPE any.

    lv_funcname = c_trim_upper( function ).
    CREATE DATA lr_params TYPE STANDARD TABLE OF (`RFC_FUNINT`).
    ASSIGN lr_params->* TO <params>.

    DATA(lt_param) = VALUE abap_func_parmbind_tab(
        ( name = `FUNCNAME` kind = abap_func_exporting value = REF #( lv_funcname ) )
        ( name = `PARAMS`   kind = abap_func_tables    value = lr_params ) ).
    DATA(lt_exception) = VALUE abap_func_excpbind_tab( ( name = `FU_NOT_FOUND` value = 1 )
                                                        ( name = `OTHERS`       value = 2 ) ).
    IF sys_fm_call( function    = `RFC_GET_FUNCTION_INTERFACE`
                    destination = destination
                    t_param     = lt_param
                    t_exception = lt_exception ) <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |function module { lv_funcname } not found|.
    ENDIF.

    LOOP AT <params> ASSIGNING <param>.
      DATA(ls_param) = VALUE ty_s_fm_param( ).
      ASSIGN COMPONENT `PARAMCLASS` OF STRUCTURE <param> TO <field>.
      ls_param-paramclass = <field>.
      ASSIGN COMPONENT `PARAMETER` OF STRUCTURE <param> TO <field>.
      ls_param-parameter = c_trim( CONV string( <field> ) ).
      ASSIGN COMPONENT `OPTIONAL` OF STRUCTURE <param> TO <field>.
      ls_param-optional = xsdbool( <field> IS NOT INITIAL ).

      ASSIGN COMPONENT `TABNAME` OF STRUCTURE <param> TO <field>.
      DATA(lv_tabname) = c_trim( CONV string( <field> ) ).
      ASSIGN COMPONENT `FIELDNAME` OF STRUCTURE <param> TO <field>.
      DATA(lv_fieldname) = c_trim( CONV string( <field> ) ).
      IF lv_tabname IS NOT INITIAL AND lv_fieldname IS NOT INITIAL.
        ls_param-typename = |{ lv_tabname }-{ lv_fieldname }|.
      ELSEIF lv_tabname IS NOT INITIAL.
        ls_param-typename = lv_tabname.
      ELSE.
        " a built-in type: EXID is the ABAP type id
        ASSIGN COMPONENT `EXID` OF STRUCTURE <param> TO <field>.
        CASE CONV string( <field> ).
          WHEN `y`.
            ls_param-typename = `XSTRING`.
          WHEN `I`.
            ls_param-typename = `INT4`.
          WHEN OTHERS.
            ls_param-typename = `STRING`.
        ENDCASE.
      ENDIF.

      INSERT ls_param INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD sys_fm_create_param.

    IF param-paramclass = `T`.
      CREATE DATA result TYPE STANDARD TABLE OF (param-typename).
    ELSE.
      CREATE DATA result TYPE (param-typename).
    ENDIF.

  ENDMETHOD.

  METHOD parallel_run.

    DATA lo_parallel TYPE REF TO object.
    DATA lr_in       TYPE REF TO data.
    DATA lr_out      TYPE REF TO data.
    FIELD-SYMBOLS <in>    TYPE STANDARD TABLE.
    FIELD-SYMBOLS <out>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row>   TYPE any.
    FIELD-SYMBOLS <field> TYPE any.

    DATA(lv_sequential) = sequential.
    IF lv_sequential = abap_false.
      TRY.
          cl_abap_typedescr=>describe_by_name( `CL_ABAP_PARALLEL` ).
        CATCH cx_root.
          lv_sequential = abap_true.
      ENDTRY.
    ENDIF.

    IF lv_sequential = abap_true.
      DATA(lv_method) = c_trim_upper( method ).
      LOOP AT t_task INTO DATA(lo_task).
        DATA(ls_result) = VALUE ty_s_parallel( index = sy-tabix
                                               task  = lo_task ).
        TRY.
            CALL METHOD lo_task->(lv_method).
          CATCH cx_root INTO DATA(lx_task).
            ls_result-message = lx_task->get_text( ).
        ENDTRY.
        INSERT ls_result INTO TABLE result.
      ENDLOOP.
      RETURN.
    ENDIF.

    TRY.
        DATA(lt_param) = VALUE abap_parmbind_tab( ).
        IF tasks > 0.
          INSERT VALUE #( name  = `P_NUM_TASKS`
                          kind  = cl_abap_objectdescr=>exporting
                          value = REF #( tasks ) ) INTO TABLE lt_param.
        ENDIF.
        IF timeout > 0.
          INSERT VALUE #( name  = `P_TIMEOUT`
                          kind  = cl_abap_objectdescr=>exporting
                          value = REF #( timeout ) ) INTO TABLE lt_param.
        ENDIF.
        DATA(lv_class) = `CL_ABAP_PARALLEL`.
        CREATE OBJECT lo_parallel TYPE (lv_class)
          PARAMETER-TABLE lt_param.

        CREATE DATA lr_in TYPE (`CL_ABAP_PARALLEL=>T_IN_INST_TAB`).
        ASSIGN lr_in->* TO <in>.
        CREATE DATA lr_out TYPE (`CL_ABAP_PARALLEL=>T_OUT_INST_TAB`).
        ASSIGN lr_out->* TO <out>.

        LOOP AT t_task INTO lo_task.
          INSERT INITIAL LINE INTO TABLE <in> ASSIGNING <row>.
          <row> ?= lo_task.
        ENDLOOP.

        CALL METHOD lo_parallel->(`RUN_INST`)
          EXPORTING
            p_in_tab  = <in>
          IMPORTING
            p_out_tab = <out>.

        LOOP AT <out> ASSIGNING <row>.
          ls_result = VALUE #( index = sy-tabix ).
          ASSIGN COMPONENT `INDEX` OF STRUCTURE <row> TO <field>.
          IF sy-subrc = 0.
            ls_result-index = <field>.
          ENDIF.
          ASSIGN COMPONENT `INST` OF STRUCTURE <row> TO <field>.
          IF sy-subrc = 0.
            ls_result-task = <field>.
          ENDIF.
          ASSIGN COMPONENT `MESSAGE` OF STRUCTURE <row> TO <field>.
          IF sy-subrc = 0.
            ls_result-message = <field>.
          ENDIF.
          INSERT ls_result INTO TABLE result.
        ENDLOOP.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD longtext_to_lines.

    DATA(lv_val) = replace( val  = CONV string( val )
                            sub  = cv_char_util_cr_lf
                            with = cv_char_util_newline
                            occ  = 0 ).
    SPLIT lv_val AT cv_char_util_newline INTO TABLE DATA(lt_paragraph).

    LOOP AT lt_paragraph INTO DATA(lv_paragraph).
      IF lv_paragraph IS INITIAL.
        INSERT VALUE #( format = `*` ) INTO TABLE result.
        CONTINUE.
      ENDIF.
      DATA(lv_format) = `*`.
      WHILE lv_paragraph IS NOT INITIAL.
        DATA(lv_len) = nmin( val1 = 132
                             val2 = strlen( lv_paragraph ) ).
        " TDLINE is CHAR132: a blank at the end of a chunk is gone when the
        " line is read back, and `=` joins the chunks without one - `word
        " next` came back as `wordnext`. The blanks move to the next chunk
        IF lv_len < strlen( lv_paragraph ).
          DATA(lv_cut) = lv_len.
          WHILE lv_cut > 0 AND substring( val = lv_paragraph
                                          off = lv_cut - 1
                                          len = 1 ) = ` `.
            lv_cut = lv_cut - 1.
          ENDWHILE.
          IF lv_cut > 0.
            lv_len = lv_cut.
          ENDIF.
        ENDIF.
        INSERT VALUE #( format = lv_format
                        line   = substring( val = lv_paragraph
                                            len = lv_len ) ) INTO TABLE result.
        lv_paragraph = substring( val = lv_paragraph
                                  off = lv_len ).
        lv_format = `=`.
      ENDWHILE.
    ENDLOOP.

  ENDMETHOD.

  METHOD longtext_from_lines.

    LOOP AT val INTO DATA(ls_line).
      IF sy-tabix = 1.
        result = ls_line-line.
        CONTINUE.
      ENDIF.
      CASE ls_line-format.
        WHEN `=`.
          result = result && ls_line-line.
        WHEN ``.
          " a continuation line, joined with a blank
          result = |{ result } { ls_line-line }|.
        WHEN OTHERS.
          result = result && cv_char_util_newline && ls_line-line.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD longtext_read.

    DATA lr_lines   TYPE REF TO data.
    DATA lv_object  TYPE c LENGTH 10.
    DATA lv_id      TYPE c LENGTH 4.
    DATA lv_name    TYPE c LENGTH 70.
    DATA lv_langu   TYPE c LENGTH 1.
    DATA lt_line    TYPE ty_t_longtext_line.
    FIELD-SYMBOLS <lines> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <field> TYPE any.

    x_raise_on_cloud( `longtext_read` ).

    lv_object = object.
    lv_id     = id.
    lv_name   = name.
    lv_langu  = sys_langu( langu ).

    TRY.
        CREATE DATA lr_lines TYPE STANDARD TABLE OF (`TLINE`).
        ASSIGN lr_lines->* TO <lines>.

        DATA(lv_fm) = `READ_TEXT`.
        CALL FUNCTION lv_fm
          EXPORTING
            id        = lv_id
            language  = lv_langu
            name      = lv_name
            object    = lv_object
          TABLES
            lines     = <lines>
          EXCEPTIONS
            not_found     = 1
            error_message = 2
            OTHERS        = 2.
        IF sy-subrc = 1.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |longtext_read: text { lv_object } { lv_id } { lv_name } ({ lv_langu }) not found|.
        ELSEIF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |longtext_read: READ_TEXT failed for { lv_object } { lv_id } { lv_name }|.
        ENDIF.

        LOOP AT <lines> ASSIGNING <line>.
          DATA(ls_line) = VALUE ty_s_longtext_line( ).
          ASSIGN COMPONENT `TDFORMAT` OF STRUCTURE <line> TO <field>.
          ls_line-format = c_trim( CONV string( <field> ) ).
          ASSIGN COMPONENT `TDLINE` OF STRUCTURE <line> TO <field>.
          ls_line-line = <field>.
          INSERT ls_line INTO TABLE lt_line.
        ENDLOOP.
        result = longtext_from_lines( lt_line ).

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD longtext_save.

    DATA lr_header TYPE REF TO data.
    DATA lr_lines  TYPE REF TO data.
    FIELD-SYMBOLS <header> TYPE any.
    FIELD-SYMBOLS <lines>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>   TYPE any.
    FIELD-SYMBOLS <field>  TYPE any.

    x_raise_on_cloud( `longtext_save` ).

    TRY.
        CREATE DATA lr_header TYPE (`THEAD`).
        ASSIGN lr_header->* TO <header>.
        ASSIGN COMPONENT `TDOBJECT` OF STRUCTURE <header> TO <field>.
        <field> = object.
        ASSIGN COMPONENT `TDID` OF STRUCTURE <header> TO <field>.
        <field> = id.
        ASSIGN COMPONENT `TDNAME` OF STRUCTURE <header> TO <field>.
        <field> = name.
        ASSIGN COMPONENT `TDSPRAS` OF STRUCTURE <header> TO <field>.
        <field> = sys_langu( langu ).

        CREATE DATA lr_lines TYPE STANDARD TABLE OF (`TLINE`).
        ASSIGN lr_lines->* TO <lines>.
        LOOP AT longtext_to_lines( val ) INTO DATA(ls_line).
          INSERT INITIAL LINE INTO TABLE <lines> ASSIGNING <line>.
          ASSIGN COMPONENT `TDFORMAT` OF STRUCTURE <line> TO <field>.
          <field> = ls_line-format.
          ASSIGN COMPONENT `TDLINE` OF STRUCTURE <line> TO <field>.
          <field> = ls_line-line.
        ENDLOOP.

        " insert = space: SAVE_TEXT finds out itself whether the text exists
        DATA(lv_fm) = `SAVE_TEXT`.
        CALL FUNCTION lv_fm
          EXPORTING
            header          = <header>
            savemode_direct = abap_true
          TABLES
            lines           = <lines>
          EXCEPTIONS
            error_message   = 1
            OTHERS          = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |longtext_save: SAVE_TEXT failed for { object } { id } { name }|.
        ENDIF.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD longtext_delete.

    DATA lv_object TYPE c LENGTH 10.
    DATA lv_id     TYPE c LENGTH 4.
    DATA lv_name   TYPE c LENGTH 70.
    DATA lv_langu  TYPE c LENGTH 1.

    x_raise_on_cloud( `longtext_delete` ).

    lv_object = object.
    lv_id     = id.
    lv_name   = name.
    lv_langu  = sys_langu( langu ).

    TRY.
        DATA(lv_fm) = `DELETE_TEXT`.
        CALL FUNCTION lv_fm
          EXPORTING
            id              = lv_id
            language        = lv_langu
            name            = lv_name
            object          = lv_object
            savemode_direct = abap_true
          EXCEPTIONS
            not_found       = 1
            error_message   = 2
            OTHERS          = 2.
        IF sy-subrc = 2.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |longtext_delete: DELETE_TEXT failed for { lv_object } { lv_id } { lv_name }|.
        ENDIF.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD rfc_call_dynamic.

    DATA lt_comp      TYPE cl_abap_structdescr=>component_table.
    DATA lr_all       TYPE REF TO data.
    DATA lt_param     TYPE abap_func_parmbind_tab.
    DATA lt_exception TYPE abap_func_excpbind_tab.
    DATA lv_json      TYPE string.
    FIELD-SYMBOLS <all>  TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.

    x_raise_on_cloud( `rfc_call_dynamic` ).

    DATA(lv_function) = c_trim_upper( function ).
    DATA(lv_params) = c_trim( CONV string( params ) ).

    TRY.
        DATA(lt_interface) = sys_fm_get_interface( function    = lv_function
                                                   destination = destination ).

        " one structure with a component per parameter: the JSON is mapped
        " into it in one go, and the parameter table points into it
        LOOP AT lt_interface INTO DATA(ls_interface) WHERE paramclass <> `X`.
          DATA(lr_type) = sys_fm_create_param( ls_interface ).
          INSERT VALUE #( name = ls_interface-parameter
                          type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data_ref( lr_type ) ) )
                 INTO TABLE lt_comp.
        ENDLOOP.

        IF lt_comp IS NOT INITIAL.
          DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
          CREATE DATA lr_all TYPE HANDLE lo_struct.
          ASSIGN lr_all->* TO <all>.
        ENDIF.

        DATA li_json TYPE REF TO zabaputil_if_ajson.
        IF lv_params IS NOT INITIAL AND <all> IS ASSIGNED.
          li_json = zabaputil_cl_ajson=>parse( lv_params ).
          li_json->to_abap( EXPORTING iv_corresponding = abap_true
                            IMPORTING ev_container     = <all> ).
        ENDIF.

        DATA(lv_count) = 0.
        LOOP AT lt_interface INTO ls_interface.
          IF ls_interface-paramclass = `X`.
            lv_count = lv_count + 1.
            INSERT VALUE #( name = ls_interface-parameter value = lv_count ) INTO TABLE lt_exception.
            CONTINUE.
          ENDIF.

          " an optional importing parameter the caller did not give keeps its default
          IF ls_interface-paramclass = `I` AND ls_interface-optional = abap_true.
            IF li_json IS NOT BOUND.
              CONTINUE.
            ENDIF.
            IF li_json->exists( |/{ ls_interface-parameter }| ) = abap_false
                AND li_json->exists( |/{ to_lower( ls_interface-parameter ) }| ) = abap_false.
              CONTINUE.
            ENDIF.
          ENDIF.

          ASSIGN COMPONENT ls_interface-parameter OF STRUCTURE <all> TO <comp>.
          INSERT VALUE #( name  = ls_interface-parameter
                          kind  = SWITCH #( ls_interface-paramclass
                                            WHEN `I` THEN abap_func_exporting
                                            WHEN `E` THEN abap_func_importing
                                            WHEN `C` THEN abap_func_changing
                                            ELSE abap_func_tables )
                          value = REF #( <comp> ) ) INTO TABLE lt_param.
        ENDLOOP.

        IF destination IS NOT INITIAL.
          INSERT VALUE #( name = `SYSTEM_FAILURE`        value = lv_count + 1 ) INTO TABLE lt_exception.
          INSERT VALUE #( name = `COMMUNICATION_FAILURE` value = lv_count + 2 ) INTO TABLE lt_exception.
        ENDIF.
        INSERT VALUE #( name = `OTHERS` value = lv_count + 3 ) INTO TABLE lt_exception.

        DATA(lv_subrc) = sys_fm_call( function    = lv_function
                                      destination = destination
                                      t_param     = lt_param
                                      t_exception = lt_exception ).
        IF lv_subrc <> 0.
          DATA(lv_exception) = `OTHERS`.
          LOOP AT lt_exception INTO DATA(ls_exception) WHERE value = lv_subrc.
            lv_exception = ls_exception-name.
          ENDLOOP.
          DATA(lv_msg) = ``.
          IF sy-msgid IS NOT INITIAL.
            lv_msg = |: { text_get( msgid = sy-msgid
                                    msgno = sy-msgno
                                    v1    = sy-msgv1
                                    v2    = sy-msgv2
                                    v3    = sy-msgv3
                                    v4    = sy-msgv4 ) }|.
          ENDIF.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |{ lv_function } raised { lv_exception }{ lv_msg }|.
        ENDIF.

        LOOP AT lt_interface INTO ls_interface WHERE paramclass = `E` OR paramclass = `C` OR paramclass = `T`.
          ASSIGN COMPONENT ls_interface-parameter OF STRUCTURE <all> TO <comp>.
          IF lv_json IS NOT INITIAL.
            lv_json = lv_json && `,`.
          ENDIF.
          lv_json = |{ lv_json }"{ ls_interface-parameter }":{ json_stringify( <comp> ) }|.
        ENDLOOP.
        result = |\{{ lv_json }\}|.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD rfc_ping.

    DATA lv_destination TYPE c LENGTH 32.
    DATA lv_msg         TYPE c LENGTH 255.
    DATA lo_provider    TYPE REF TO object.

    lv_destination = c_trim( CONV string( destination ) ).

    TRY.
        IF check_abap_cloud( ).
          " a BTP destination: the provider hands out the RFC destination name
          DATA(lv_class) = `CL_RFC_DESTINATION_PROVIDER`.
          DATA(lv_create) = `CREATE_BY_CLOUD_DESTINATION`.
          CALL METHOD (lv_class)=>(lv_create)
            PARAMETER-TABLE VALUE abap_parmbind_tab(
              ( name  = `I_NAME`
                kind  = cl_abap_objectdescr=>exporting
                value = sys_method_param( type_name = lv_class
                                          method    = lv_create
                                          parameter = `I_NAME`
                                          value     = lv_destination ) )
              ( name  = sys_method_returning( type_name = lv_class
                                              method    = lv_create )
                kind  = cl_abap_objectdescr=>receiving
                value = REF #( lo_provider ) ) ).
          DATA(lv_returning) = sys_method_returning( type_name = `IF_RFC_DEST`
                                                     method    = `GET_DESTINATION_NAME` ).
          DATA(lr_name) = sys_method_param( type_name = `IF_RFC_DEST`
                                            method    = `GET_DESTINATION_NAME`
                                            parameter = lv_returning ).
          CALL METHOD lo_provider->(`IF_RFC_DEST~GET_DESTINATION_NAME`)
            PARAMETER-TABLE VALUE abap_parmbind_tab( ( name  = lv_returning
                                                       kind  = cl_abap_objectdescr=>receiving
                                                       value = lr_name ) ).
          ASSIGN lr_name->* TO FIELD-SYMBOL(<name>).
          lv_destination = <name>.
        ENDIF.

        DATA(lv_fm) = `RFC_PING`.
        CALL FUNCTION lv_fm
          DESTINATION lv_destination
          EXCEPTIONS
            system_failure        = 1 MESSAGE lv_msg
            communication_failure = 2 MESSAGE lv_msg
            error_message         = 3
            OTHERS                = 3.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |rfc_ping { destination }: { COND string( WHEN lv_msg IS INITIAL THEN |sy-subrc { sy-subrc }| ELSE lv_msg ) }|.
        ENDIF.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD rfc_check_destination.

    TRY.
        rfc_ping( destination ).
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD mime_read.

    DATA lo_api TYPE REF TO object.

    x_raise_on_cloud( `mime_read` ).

    TRY.
        DATA(lv_class) = `CL_MIME_REPOSITORY_API`.
        CALL METHOD (lv_class)=>(`GET_API`)
          PARAMETER-TABLE VALUE abap_parmbind_tab( ( name  = sys_method_returning( type_name = lv_class
                                                                                   method    = `GET_API` )
                                                     kind  = cl_abap_objectdescr=>receiving
                                                     value = REF #( lo_api ) ) ).

        CALL METHOD lo_api->(`IF_MR_API~GET`)
          EXPORTING
            i_url              = CONV string( url )
          IMPORTING
            e_content          = result
          EXCEPTIONS
            parameter_missing  = 1
            error_occured      = 2
            not_found          = 3
            permission_failure = 4
            OTHERS             = 5.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |mime_read: { url } { COND string( WHEN sy-subrc = 3 THEN `not found`
                                                       WHEN sy-subrc = 4 THEN `not authorized`
                                                       ELSE `could not be read` ) }|.
        ENDIF.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD mime_save.

    DATA lo_api       TYPE REF TO object.
    DATA lt_exception TYPE abap_excpbind_tab.

    x_raise_on_cloud( `mime_save` ).

    TRY.
        DATA(lv_class) = `CL_MIME_REPOSITORY_API`.
        CALL METHOD (lv_class)=>(`GET_API`)
          PARAMETER-TABLE VALUE abap_parmbind_tab( ( name  = sys_method_returning( type_name = lv_class
                                                                                   method    = `GET_API` )
                                                     kind  = cl_abap_objectdescr=>receiving
                                                     value = REF #( lo_api ) ) ).

        DATA(lv_url) = CONV string( url ).
        DATA(lv_package) = CONV string( package ).
        DATA(lv_transport) = CONV string( transport ).
        DATA(lv_true) = abap_true.
        DATA(lv_content) = content.
        DATA(lt_param) = VALUE abap_parmbind_tab(
            ( name = `I_URL`                     kind = cl_abap_objectdescr=>exporting value = REF #( lv_url ) )
            ( name = `I_CONTENT`                 kind = cl_abap_objectdescr=>exporting value = REF #( lv_content ) )
            ( name = `I_DEV_PACKAGE`             kind = cl_abap_objectdescr=>exporting value = REF #( lv_package ) )
            ( name = `I_SUPPRESS_DIALOGS`        kind = cl_abap_objectdescr=>exporting value = REF #( lv_true ) )
            ( name = `I_SUPPRESS_PACKAGE_DIALOG` kind = cl_abap_objectdescr=>exporting value = REF #( lv_true ) ) ).
        IF lv_transport IS NOT INITIAL.
          INSERT VALUE #( name  = `I_CORR_NUMBER`
                          kind  = cl_abap_objectdescr=>exporting
                          value = REF #( lv_transport ) ) INTO TABLE lt_param.
        ENDIF.
        lt_exception = VALUE #( ( name = `OTHERS` value = 1 ) ).

        CALL METHOD lo_api->(`IF_MR_API~PUT`)
          PARAMETER-TABLE lt_param
          EXCEPTION-TABLE lt_exception.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |mime_save: { url } could not be saved|.
        ENDIF.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD spool_get_pdf.

    DATA lv_doctype TYPE c LENGTH 4.
    DATA lv_spool   TYPE i.
    DATA lv_bytes   TYPE i.
    DATA lr_pdf     TYPE REF TO data.
    FIELD-SYMBOLS <pdf> TYPE STANDARD TABLE.

    x_raise_on_cloud( `spool_get_pdf` ).

    lv_spool = spool_id.

    TRY.
        DATA(lv_table) = `TSP01`.
        SELECT SINGLE rqdoctype FROM (lv_table) WHERE rqident = @lv_spool INTO @lv_doctype.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |spool_get_pdf: spool request { lv_spool } not found|.
        ENDIF.

        IF lv_doctype = `ADSP`.
          " an Adobe form in the spool already is a PDF
          DATA(lt_interface) = sys_fm_get_interface( `FPCOMP_CREATE_PDF_FROM_SPOOL` ).
          DATA(lr_spool) = sys_fm_create_param( VALUE #( lt_interface[ parameter = `I_SPOOLID` ] OPTIONAL ) ).
          DATA(lr_part) = sys_fm_create_param( VALUE #( lt_interface[ parameter = `I_PARTNUM` ] OPTIONAL ) ).
          ASSIGN lr_spool->* TO FIELD-SYMBOL(<spool>).
          <spool> = lv_spool.
          ASSIGN lr_part->* TO FIELD-SYMBOL(<part>).
          <part> = 1.
          DATA(lt_param) = VALUE abap_func_parmbind_tab(
              ( name = `I_SPOOLID` kind = abap_func_exporting value = lr_spool )
              ( name = `I_PARTNUM` kind = abap_func_exporting value = lr_part )
              ( name = `E_PDF`     kind = abap_func_importing value = REF #( result ) ) ).
          IF sys_fm_call( function    = `FPCOMP_CREATE_PDF_FROM_SPOOL`
                          t_param     = lt_param
                          t_exception = VALUE #( ( name = `OTHERS` value = 1 ) ) ) <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |spool_get_pdf: spool request { lv_spool } could not be read|.
          ENDIF.
          RETURN.
        ENDIF.

        " OTF (SAPscript, Smart Forms) or an ABAP list: the PDF bytes come
        " packed into the memory of a TLINE table
        CREATE DATA lr_pdf TYPE STANDARD TABLE OF (`TLINE`).
        ASSIGN lr_pdf->* TO <pdf>.
        DATA(lv_fm) = COND string( WHEN lv_doctype = `OTF` THEN `CONVERT_OTFSPOOLJOB_2_PDF`
                                   ELSE `CONVERT_ABAPSPOOLJOB_2_PDF` ).
        CALL FUNCTION lv_fm
          EXPORTING
            src_spoolid   = lv_spool
            no_dialog     = abap_true
          IMPORTING
            pdf_bytecount = lv_bytes
          TABLES
            pdf           = <pdf>
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |spool_get_pdf: spool request { lv_spool } could not be converted|.
        ENDIF.
        result = conv_get_xstring_by_bintab( val    = <pdf>
                                             length = lv_bytes ).

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD form_get_pdf.

    DATA lv_form       TYPE c LENGTH 30.
    DATA lv_function   TYPE c LENGTH 30.
    DATA lr_output     TYPE REF TO data.
    DATA lr_docparams  TYPE REF TO data.
    DATA lr_formoutput TYPE REF TO data.
    DATA lv_job_open   TYPE abap_bool.
    FIELD-SYMBOLS <output>     TYPE any.
    FIELD-SYMBOLS <docparams>  TYPE any.
    FIELD-SYMBOLS <formoutput> TYPE any.
    FIELD-SYMBOLS <field>      TYPE any.

    x_raise_on_cloud( `form_get_pdf` ).

    lv_form = c_trim_upper( form_name ).

    TRY.
        DATA(lv_fm) = `FP_FUNCTION_MODULE_NAME`.
        CALL FUNCTION lv_fm
          EXPORTING
            i_name     = lv_form
          IMPORTING
            e_funcname = lv_function.

        " no dialog, no printer: the PDF comes back in the form output
        CREATE DATA lr_output TYPE (`SFPOUTPUTPARAMS`).
        ASSIGN lr_output->* TO <output>.
        ASSIGN COMPONENT `GETPDF` OF STRUCTURE <output> TO <field>.
        <field> = abap_true.
        ASSIGN COMPONENT `NODIALOG` OF STRUCTURE <output> TO <field>.
        <field> = abap_true.

        lv_fm = `FP_JOB_OPEN`.
        CALL FUNCTION lv_fm
          CHANGING
            ie_outputparams = <output>
          EXCEPTIONS
            error_message   = 1
            OTHERS          = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `form_get_pdf: FP_JOB_OPEN failed`.
        ENDIF.
        lv_job_open = abap_true.

        CREATE DATA lr_docparams TYPE (`SFPDOCPARAMS`).
        ASSIGN lr_docparams->* TO <docparams>.
        ASSIGN COMPONENT `LANGU` OF STRUCTURE <docparams> TO <field>.
        <field> = sys_langu( langu ).
        CREATE DATA lr_formoutput TYPE (`FPFORMOUTPUT`).
        ASSIGN lr_formoutput->* TO <formoutput>.

        DATA(lt_param) = VALUE abap_func_parmbind_tab(
            ( name = `/1BCDWB/DOCPARAMS`  kind = abap_func_exporting value = lr_docparams )
            ( name = `/1BCDWB/FORMOUTPUT` kind = abap_func_importing value = lr_formoutput ) ).
        IF data IS SUPPLIED.
          DATA(lo_struct) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( data ) ).
          LOOP AT lo_struct->components INTO DATA(ls_comp).
            ASSIGN COMPONENT ls_comp-name OF STRUCTURE data TO <field>.
            INSERT VALUE #( name  = ls_comp-name
                            kind  = abap_func_exporting
                            value = REF #( <field> ) ) INTO TABLE lt_param.
          ENDLOOP.
        ENDIF.

        IF sys_fm_call( function    = lv_function
                        t_param     = lt_param
                        t_exception = VALUE #( ( name = `OTHERS` value = 1 ) ) ) <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |form_get_pdf: form { lv_form } could not be rendered|.
        ENDIF.

        ASSIGN COMPONENT `PDF` OF STRUCTURE <formoutput> TO <field>.
        result = <field>.

        lv_fm = `FP_JOB_CLOSE`.
        CALL FUNCTION lv_fm
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        lv_job_open = abap_false.

      CATCH cx_root INTO DATA(x).
        IF lv_job_open = abap_true.
          lv_fm = `FP_JOB_CLOSE`.
          CALL FUNCTION lv_fm
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
        ENDIF.
        IF x IS INSTANCE OF zabaputil_cx_util_error.
          DATA(lx_util) = CAST zabaputil_cx_util_error( x ).
          RAISE EXCEPTION lx_util.
        ENDIF.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD variant_list.

    TYPES:
      BEGIN OF ty_s_text,
        variant TYPE c LENGTH 14,
        vtext   TYPE c LENGTH 30,
      END OF ty_s_text.
    DATA lt_variant TYPE STANDARD TABLE OF ty_s_text WITH EMPTY KEY.
    DATA lt_text    TYPE STANDARD TABLE OF ty_s_text WITH EMPTY KEY.
    DATA lv_report  TYPE c LENGTH 40.
    DATA lv_langu   TYPE c LENGTH 1.

    x_raise_on_cloud( `variant_list` ).

    lv_report = c_trim_upper( report ).
    lv_langu  = sys_langu( langu ).

    TRY.
        DATA(lv_table) = `VARID`.
        SELECT variant FROM (lv_table) WHERE report = @lv_report
          INTO CORRESPONDING FIELDS OF TABLE @lt_variant.
        lv_table = `VARIT`.
        SELECT variant, vtext FROM (lv_table) WHERE report = @lv_report AND langu = @lv_langu
          INTO CORRESPONDING FIELDS OF TABLE @lt_text.

        LOOP AT lt_variant INTO DATA(ls_variant).
          INSERT VALUE #( variant = c_trim( CONV string( ls_variant-variant ) )
                          text    = c_trim( CONV string( VALUE #( lt_text[ variant = ls_variant-variant ]-vtext OPTIONAL ) ) ) )
                 INTO TABLE result.
        ENDLOOP.
        SORT result BY variant.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD variant_read.

    DATA lv_report  TYPE c LENGTH 40.
    DATA lv_variant TYPE c LENGTH 14.
    DATA lr_values  TYPE REF TO data.
    FIELD-SYMBOLS <values> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <value>  TYPE any.
    FIELD-SYMBOLS <field>  TYPE any.

    x_raise_on_cloud( `variant_read` ).

    lv_report  = c_trim_upper( report ).
    lv_variant = c_trim_upper( variant ).

    TRY.
        CREATE DATA lr_values TYPE STANDARD TABLE OF (`RSPARAMS`).
        ASSIGN lr_values->* TO <values>.

        DATA(lv_fm) = `RS_VARIANT_CONTENTS`.
        CALL FUNCTION lv_fm
          EXPORTING
            report               = lv_report
            variant              = lv_variant
          TABLES
            valutab              = <values>
          EXCEPTIONS
            variant_non_existent = 1
            variant_obsolete     = 2
            error_message        = 3
            OTHERS               = 3.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |variant_read: variant { lv_variant } of { lv_report } { COND string( WHEN sy-subrc = 1 THEN `does not exist`
                                                                                          WHEN sy-subrc = 2 THEN `is obsolete`
                                                                                          ELSE `could not be read` ) }|.
        ENDIF.

        LOOP AT <values> ASSIGNING <value>.
          DATA(ls_result) = VALUE ty_s_variant_value( ).
          ASSIGN COMPONENT `SELNAME` OF STRUCTURE <value> TO <field>.
          ls_result-selname = c_trim( CONV string( <field> ) ).
          ASSIGN COMPONENT `KIND` OF STRUCTURE <value> TO <field>.
          ls_result-kind = <field>.
          ASSIGN COMPONENT `SIGN` OF STRUCTURE <value> TO <field>.
          ls_result-sign = <field>.
          ASSIGN COMPONENT `OPTION` OF STRUCTURE <value> TO <field>.
          ls_result-option = c_trim( CONV string( <field> ) ).
          ASSIGN COMPONENT `LOW` OF STRUCTURE <value> TO <field>.
          ls_result-low = c_trim( CONV string( <field> ) ).
          ASSIGN COMPONENT `HIGH` OF STRUCTURE <value> TO <field>.
          ls_result-high = c_trim( CONV string( <field> ) ).
          INSERT ls_result INTO TABLE result.
        ENDLOOP.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD amc_send.

    DATA lo_producer TYPE REF TO object.
    DATA lr_value    TYPE REF TO data.
    FIELD-SYMBOLS <value> TYPE any.

    x_raise_on_cloud( `amc_send` ).

    TRY.
        " the parameter types are read from the class: they differ by release
        DATA(lv_class) = `CL_AMC_CHANNEL_MANAGER`.
        DATA(lo_class) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( lv_class ) ).
        DATA(lt_param) = VALUE abap_parmbind_tab( ).
        DATA(lt_value) = VALUE ty_t_name_value( ( n = `I_APPLICATION_ID` v = application_id )
                                                ( n = `I_CHANNEL_ID`     v = channel_id ) ).
        IF channel_extension_id IS NOT INITIAL.
          INSERT VALUE #( n = `I_CHANNEL_EXTENSION_ID` v = channel_extension_id ) INTO TABLE lt_value.
        ENDIF.
        LOOP AT lt_value INTO DATA(ls_value).
          DATA(lo_type) = lo_class->get_method_parameter_type( p_method_name    = `CREATE_MESSAGE_PRODUCER`
                                                               p_parameter_name = ls_value-n ).
          CREATE DATA lr_value TYPE HANDLE lo_type.
          ASSIGN lr_value->* TO <value>.
          <value> = ls_value-v.
          INSERT VALUE #( name  = ls_value-n
                          kind  = cl_abap_objectdescr=>exporting
                          value = lr_value ) INTO TABLE lt_param.
        ENDLOOP.
        INSERT VALUE #( name  = sys_method_returning( type_name = lv_class
                                                      method    = `CREATE_MESSAGE_PRODUCER` )
                        kind  = cl_abap_objectdescr=>receiving
                        value = REF #( lo_producer ) ) INTO TABLE lt_param.

        CALL METHOD (lv_class)=>(`CREATE_MESSAGE_PRODUCER`)
          PARAMETER-TABLE lt_param.

        CALL METHOD lo_producer->(`IF_AMC_MESSAGE_PRODUCER_TEXT~SEND`)
          EXPORTING
            i_message = CONV string( message ).

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD sys_method_returning.

    DATA(lo_descr) = CAST cl_abap_objectdescr( cl_abap_typedescr=>describe_by_name( type_name ) ).
    DATA(lv_method) = to_upper( method ).
    LOOP AT lo_descr->methods INTO DATA(ls_method) WHERE name = lv_method.
      LOOP AT ls_method-parameters INTO DATA(ls_parameter) WHERE parm_kind = cl_abap_objectdescr=>returning.
        result = ls_parameter-name.
        RETURN.
      ENDLOOP.
    ENDLOOP.
    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = |{ type_name }=>{ method } has no returning parameter|.

  ENDMETHOD.

  METHOD gos_attach.

    DATA lr_folder   TYPE REF TO data.
    DATA lr_docdata  TYPE REF TO data.
    DATA lr_docinfo  TYPE REF TO data.
    DATA lr_header   TYPE REF TO data.
    DATA lr_hex      TYPE REF TO data.
    DATA lr_object_a TYPE REF TO data.
    DATA lr_object_b TYPE REF TO data.
    DATA lv_doctype  TYPE c LENGTH 3.
    DATA lv_reltype  TYPE c LENGTH 4 VALUE `ATTA`.
    FIELD-SYMBOLS <folder>   TYPE any.
    FIELD-SYMBOLS <docdata>  TYPE any.
    FIELD-SYMBOLS <docinfo>  TYPE any.
    FIELD-SYMBOLS <header>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <hex>      TYPE STANDARD TABLE.
    FIELD-SYMBOLS <object_a> TYPE any.
    FIELD-SYMBOLS <object_b> TYPE any.
    FIELD-SYMBOLS <line>     TYPE any.
    FIELD-SYMBOLS <field>    TYPE any.

    x_raise_on_cloud( `gos_attach` ).

    DATA(lv_file) = c_trim( CONV string( file_name ) ).
    DATA(lv_ext) = ``.
    DATA(lv_dot) = find( val = lv_file
                         sub = `.`
                         occ = -1 ).
    IF lv_dot >= 0.
      lv_ext = to_upper( substring( val = lv_file
                                    off = lv_dot + 1 ) ).
    ENDIF.
    lv_doctype = lv_ext.
    DATA(lv_descr) = COND string( WHEN description IS NOT INITIAL THEN description
                                  WHEN lv_dot > 0 THEN substring( val = lv_file
                                                                  len = lv_dot )
                                  ELSE lv_file ).

    TRY.
        " the attachment is an office document in the user's root folder
        DATA(lt_interface) = sys_fm_get_interface( `SO_DOCUMENT_INSERT_API1` ).
        lr_folder = sys_fm_create_param( VALUE #( lt_interface[ parameter = `FOLDER_ID` ] OPTIONAL ) ).
        ASSIGN lr_folder->* TO <folder>.
        CREATE DATA lr_object_a TYPE (`SOODK`).
        ASSIGN lr_object_a->* TO <object_a>.
        DATA(lv_fm) = `SO_FOLDER_ROOT_ID_GET`.
        CALL FUNCTION lv_fm
          EXPORTING
            region    = `B`
          IMPORTING
            folder_id = <object_a>
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = `gos_attach: no office root folder`.
        ENDIF.
        <folder> = <object_a>.

        CREATE DATA lr_docdata TYPE (`SODOCCHGI1`).
        ASSIGN lr_docdata->* TO <docdata>.
        ASSIGN COMPONENT `OBJ_DESCR` OF STRUCTURE <docdata> TO <field>.
        <field> = lv_descr.
        ASSIGN COMPONENT `OBJ_NAME` OF STRUCTURE <docdata> TO <field>.
        <field> = `MESSAGE`.
        ASSIGN COMPONENT `DOC_SIZE` OF STRUCTURE <docdata> TO <field>.
        <field> = xstrlen( content ).

        CREATE DATA lr_header TYPE STANDARD TABLE OF (`SOLISTI1`).
        ASSIGN lr_header->* TO <header>.
        INSERT INITIAL LINE INTO TABLE <header> ASSIGNING <line>.
        ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field>.
        <field> = |&SO_FILENAME={ lv_file }|.
        INSERT INITIAL LINE INTO TABLE <header> ASSIGNING <line>.
        ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field>.
        <field> = `&SO_FORMAT=BIN`.

        CREATE DATA lr_hex TYPE STANDARD TABLE OF (`SOLIX`).
        ASSIGN lr_hex->* TO <hex>.
        conv_get_bintab_by_xstring( EXPORTING val    = content
                                    IMPORTING result = <hex> ).

        CREATE DATA lr_docinfo TYPE (`SOFOLENTI1`).
        ASSIGN lr_docinfo->* TO <docinfo>.
        lv_fm = `SO_DOCUMENT_INSERT_API1`.
        CALL FUNCTION lv_fm
          EXPORTING
            folder_id     = <folder>
            document_data = <docdata>
            document_type = lv_doctype
          IMPORTING
            document_info = <docinfo>
          TABLES
            object_header = <header>
            contents_hex  = <hex>
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |gos_attach: { lv_file } could not be stored|.
        ENDIF.
        ASSIGN COMPONENT `DOC_ID` OF STRUCTURE <docinfo> TO <field>.
        result = c_trim( CONV string( <field> ) ).

        " link it to the business object as an attachment
        CREATE DATA lr_object_a TYPE (`BORIDENT`).
        ASSIGN lr_object_a->* TO <object_a>.
        ASSIGN COMPONENT `OBJKEY` OF STRUCTURE <object_a> TO <field>.
        <field> = object_key.
        ASSIGN COMPONENT `OBJTYPE` OF STRUCTURE <object_a> TO <field>.
        <field> = object_type.
        CREATE DATA lr_object_b TYPE (`BORIDENT`).
        ASSIGN lr_object_b->* TO <object_b>.
        ASSIGN COMPONENT `OBJKEY` OF STRUCTURE <object_b> TO <field>.
        <field> = result.
        ASSIGN COMPONENT `OBJTYPE` OF STRUCTURE <object_b> TO <field>.
        <field> = `MESSAGE`.

        lv_fm = `BINARY_RELATION_CREATE_COMMIT`.
        CALL FUNCTION lv_fm
          EXPORTING
            obj_rolea    = <object_a>
            obj_roleb    = <object_b>
            relationtype = lv_reltype
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |gos_attach: { lv_file } could not be linked to { object_type } { object_key }|.
        ENDIF.

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD gos_list.

    DATA lr_object  TYPE REF TO data.
    DATA lr_options TYPE REF TO data.
    DATA lr_links   TYPE REF TO data.
    DATA lr_docdata TYPE REF TO data.
    DATA lr_header  TYPE REF TO data.
    DATA lv_doc_id  TYPE c LENGTH 46.
    FIELD-SYMBOLS <object>  TYPE any.
    FIELD-SYMBOLS <options> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <links>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <link>    TYPE any.
    FIELD-SYMBOLS <docdata> TYPE any.
    FIELD-SYMBOLS <header>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>    TYPE any.
    FIELD-SYMBOLS <field>   TYPE any.

    x_raise_on_cloud( `gos_list` ).

    TRY.
        " the parameter types are read from the class
        DATA(lv_class) = `CL_BINARY_RELATION`.
        DATA(lv_method) = `READ_LINKS_OF_BINRELS`.
        DATA(lo_class) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( lv_class ) ).
        DATA(lo_type) = lo_class->get_method_parameter_type( p_method_name    = lv_method
                                                             p_parameter_name = `IS_OBJECT` ).
        CREATE DATA lr_object TYPE HANDLE lo_type.
        ASSIGN lr_object->* TO <object>.
        ASSIGN COMPONENT `INSTID` OF STRUCTURE <object> TO <field>.
        <field> = object_key.
        ASSIGN COMPONENT `TYPEID` OF STRUCTURE <object> TO <field>.
        <field> = object_type.
        ASSIGN COMPONENT `CATID` OF STRUCTURE <object> TO <field>.
        <field> = `BO`.

        lo_type = lo_class->get_method_parameter_type( p_method_name    = lv_method
                                                       p_parameter_name = `IT_RELATION_OPTIONS` ).
        CREATE DATA lr_options TYPE HANDLE lo_type.
        ASSIGN lr_options->* TO <options>.
        INSERT INITIAL LINE INTO TABLE <options> ASSIGNING <line>.
        ASSIGN COMPONENT `SIGN` OF STRUCTURE <line> TO <field>.
        <field> = `I`.
        ASSIGN COMPONENT `OPTION` OF STRUCTURE <line> TO <field>.
        <field> = `EQ`.
        ASSIGN COMPONENT `LOW` OF STRUCTURE <line> TO <field>.
        <field> = `ATTA`.

        lo_type = lo_class->get_method_parameter_type( p_method_name    = lv_method
                                                       p_parameter_name = `ET_LINKS` ).
        CREATE DATA lr_links TYPE HANDLE lo_type.
        ASSIGN lr_links->* TO <links>.

        DATA(lt_param) = VALUE abap_parmbind_tab(
            ( name = `IS_OBJECT`           kind = cl_abap_objectdescr=>exporting value = lr_object )
            ( name = `IT_RELATION_OPTIONS` kind = cl_abap_objectdescr=>exporting value = lr_options )
            ( name = `ET_LINKS`            kind = cl_abap_objectdescr=>importing value = lr_links ) ).
        TRY.
            CALL METHOD (lv_class)=>(lv_method)
              PARAMETER-TABLE lt_param.
          CATCH cx_root.
            " no links at all is reported as an exception
            RETURN.
        ENDTRY.

        CREATE DATA lr_docdata TYPE (`SOFOLENTI1`).
        ASSIGN lr_docdata->* TO <docdata>.
        CREATE DATA lr_header TYPE STANDARD TABLE OF (`SOLISTI1`).
        ASSIGN lr_header->* TO <header>.

        LOOP AT <links> ASSIGNING <link>.
          DATA(ls_result) = VALUE ty_s_gos( ).
          ASSIGN COMPONENT `INSTID_B` OF STRUCTURE <link> TO <field>.
          ls_result-doc_id = c_trim( CONV string( <field> ) ).

          CLEAR <docdata>.
          CLEAR <header>.
          DATA(lv_fm) = `SO_DOCUMENT_READ_API1`.
          lv_doc_id = ls_result-doc_id.
          CALL FUNCTION lv_fm
            EXPORTING
              document_id   = lv_doc_id
            IMPORTING
              document_data = <docdata>
            TABLES
              object_header = <header>
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
          IF sy-subrc = 0.
            ASSIGN COMPONENT `OBJ_DESCR` OF STRUCTURE <docdata> TO <field>.
            IF sy-subrc = 0.
              ls_result-description = c_trim( CONV string( <field> ) ).
            ENDIF.
            ASSIGN COMPONENT `DOC_SIZE` OF STRUCTURE <docdata> TO <field>.
            IF sy-subrc = 0.
              ls_result-size = <field>.
            ENDIF.
            ASSIGN COMPONENT `CREAT_NAME` OF STRUCTURE <docdata> TO <field>.
            IF sy-subrc = 0.
              ls_result-created_by = c_trim( CONV string( <field> ) ).
            ENDIF.
            ASSIGN COMPONENT `CREAT_DATE` OF STRUCTURE <docdata> TO <field>.
            IF sy-subrc = 0.
              ls_result-created_on = <field>.
            ENDIF.
            LOOP AT <header> ASSIGNING <line>.
              ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field>.
              DATA(lv_header) = c_trim( CONV string( <field> ) ).
              IF c_starts_with( val    = lv_header
                                prefix = `&SO_FILENAME=` ).
                ls_result-file_name = substring( val = lv_header
                                                 off = 13 ).
              ENDIF.
            ENDLOOP.
          ENDIF.

          DATA(lv_dot) = find( val = ls_result-file_name
                               sub = `.`
                               occ = -1 ).
          IF lv_dot >= 0.
            ls_result-file_ext = to_upper( substring( val = ls_result-file_name
                                                      off = lv_dot + 1 ) ).
          ENDIF.
          INSERT ls_result INTO TABLE result.
        ENDLOOP.

      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD gos_read.

    DATA lr_docdata TYPE REF TO data.
    DATA lr_hex     TYPE REF TO data.
    DATA lr_content TYPE REF TO data.
    DATA lv_size    TYPE i.
    DATA lv_text    TYPE string.
    DATA lv_doc_id  TYPE c LENGTH 46.
    FIELD-SYMBOLS <docdata> TYPE any.
    FIELD-SYMBOLS <hex>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <content> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line>    TYPE any.
    FIELD-SYMBOLS <field>   TYPE any.

    x_raise_on_cloud( `gos_read` ).

    TRY.
        CREATE DATA lr_docdata TYPE (`SOFOLENTI1`).
        ASSIGN lr_docdata->* TO <docdata>.
        CREATE DATA lr_hex TYPE STANDARD TABLE OF (`SOLIX`).
        ASSIGN lr_hex->* TO <hex>.
        CREATE DATA lr_content TYPE STANDARD TABLE OF (`SOLISTI1`).
        ASSIGN lr_content->* TO <content>.

        DATA(lv_fm) = `SO_DOCUMENT_READ_API1`.
        lv_doc_id = doc_id.
        CALL FUNCTION lv_fm
          EXPORTING
            document_id    = lv_doc_id
          IMPORTING
            document_data  = <docdata>
          TABLES
            object_content = <content>
            contents_hex   = <hex>
          EXCEPTIONS
            error_message  = 1
            OTHERS         = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |gos_read: document { doc_id } not found|.
        ENDIF.

        ASSIGN COMPONENT `DOC_SIZE` OF STRUCTURE <docdata> TO <field>.
        IF sy-subrc = 0.
          lv_size = <field>.
        ENDIF.

        IF <hex> IS NOT INITIAL.
          result = conv_get_xstring_by_bintab( val    = <hex>
                                               length = lv_size ).
          RETURN.
        ENDIF.

        " a text document (notes): the content comes as text lines
        LOOP AT <content> ASSIGNING <line>.
          ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field>.
          lv_text = COND #( WHEN sy-tabix = 1 THEN <field>
                            ELSE lv_text && cv_char_util_newline && <field> ).
        ENDLOOP.
        result = conv_get_xstring_by_string( lv_text ).

      CATCH zabaputil_cx_util_error INTO DATA(lx).
        RAISE EXCEPTION lx.
      CATCH cx_root INTO DATA(x).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD sys_method_param.

    DATA(lo_descr) = CAST cl_abap_objectdescr( cl_abap_typedescr=>describe_by_name( type_name ) ).
    DATA(lo_type) = lo_descr->get_method_parameter_type( p_method_name    = to_upper( method )
                                                         p_parameter_name = to_upper( parameter ) ).
    CREATE DATA result TYPE HANDLE lo_type.
    IF value IS SUPPLIED.
      ASSIGN result->* TO FIELD-SYMBOL(<value>).
      <value> = value.
    ENDIF.

  ENDMETHOD.

  METHOD bapi_finish.

    DATA lv_fm     TYPE c LENGTH 30.
    DATA lr_return TYPE REF TO data.
    FIELD-SYMBOLS <ls_return> TYPE any.

    IF bapi_check_is_error( ct_bapiret ) = abap_true.
      cv_is_error = abap_true.
    ENDIF.

    IF iv_commit = abap_false.
      RETURN.
    ENDIF.

    " called by name, so the class compiles where the modules do not exist
    TRY.
        IF cv_is_error = abap_true.
          lv_fm = `BAPI_TRANSACTION_ROLLBACK`.
          CALL FUNCTION lv_fm.
          RETURN.
        ENDIF.

        CREATE DATA lr_return TYPE (`BAPIRET2`).
        ASSIGN lr_return->* TO <ls_return>.
        lv_fm = `BAPI_TRANSACTION_COMMIT`.
        CALL FUNCTION lv_fm
          EXPORTING
            wait   = abap_true
          IMPORTING
            return = <ls_return>.
        DATA(lv_type) = bapi_get_type( <ls_return> ).
        IF lv_type CA `EAX` AND lv_type IS NOT INITIAL.
          bapi_msg_add_bapiret2( EXPORTING is_return  = <ls_return>
                                 CHANGING  ct_bapiret = ct_bapiret ).
          cv_is_error = abap_true.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        bapi_msg_add_exception( EXPORTING ix_error   = lx_error
                                CHANGING  ct_bapiret = ct_bapiret ).
        cv_is_error = abap_true.
    ENDTRY.

  ENDMETHOD.

  METHOD bapi_check_is_error.

    FIELD-SYMBOLS <ls_row> TYPE any.

    LOOP AT it_bapiret ASSIGNING <ls_row>.
      DATA(lv_type) = bapi_get_type( <ls_row> ).
      IF lv_type CA `EAX` AND lv_type IS NOT INITIAL.
        rv_error = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD bapi_msg_add_sy.

    DATA(ls_sy) = context_get_sy( ).

    IF ls_sy-msgid IS INITIAL.
      bapi_msg_add_text( EXPORTING iv_text    = |{ iv_context } failed|
                         CHANGING  ct_bapiret = ct_bapiret ).
      RETURN.
    ENDIF.

    " a message left by EXCEPTIONS error_message is an error, whatever its type
    bapi_msg_add( EXPORTING it_msg     = msg_get_t( ls_sy )
                            iv_type    = COND string( WHEN ls_sy-msgty CA `AX` AND ls_sy-msgty IS NOT INITIAL
                                                      THEN ls_sy-msgty
                                                      ELSE `E` )
                  CHANGING  ct_bapiret = ct_bapiret ).

  ENDMETHOD.

  METHOD bapi_msg_add_text.

    bapi_msg_add( EXPORTING it_msg     = VALUE #( ( type = iv_type
                                                    text = iv_text ) )
                  CHANGING  ct_bapiret = ct_bapiret ).

  ENDMETHOD.

  METHOD bapi_msg_add_exception.

    bapi_msg_add( EXPORTING it_msg     = msg_get_t( ix_error )
                            iv_type    = `E`
                  CHANGING  ct_bapiret = ct_bapiret ).

  ENDMETHOD.

  METHOD bapi_msg_add_bapiret2.

    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <ls_row> TYPE any.

    IF is_return IS INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA lr_row LIKE LINE OF ct_bapiret.
    ASSIGN lr_row->* TO <ls_row>.
    MOVE-CORRESPONDING is_return TO <ls_row>.
    INSERT <ls_row> INTO TABLE ct_bapiret.

  ENDMETHOD.

  METHOD bapi_msg_add_table.

    bapi_msg_add( EXPORTING it_msg     = msg_get_t( it_table )
                  CHANGING  ct_bapiret = ct_bapiret ).

  ENDMETHOD.

  METHOD bapi_msg_add.

    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <ls_row> TYPE any.
    FIELD-SYMBOLS <lv_val> TYPE any.

    LOOP AT it_msg INTO DATA(ls_msg).
      CREATE DATA lr_row LIKE LINE OF ct_bapiret.
      ASSIGN lr_row->* TO <ls_row>.

      DATA(lt_value) = VALUE ty_t_name_value(
        ( n = `TYPE MSGTY`           v = COND #( WHEN iv_type IS NOT INITIAL THEN iv_type ELSE to_upper( ls_msg-type ) ) )
        ( n = `ID MSGID`             v = ls_msg-id )
        ( n = `NUMBER MSGNO`         v = ls_msg-no )
        ( n = `MESSAGE`              v = ls_msg-text )
        ( n = `MESSAGE_V1 MSGV1`     v = ls_msg-v1 )
        ( n = `MESSAGE_V2 MSGV2`     v = ls_msg-v2 )
        ( n = `MESSAGE_V3 MSGV3`     v = ls_msg-v3 )
        ( n = `MESSAGE_V4 MSGV4`     v = ls_msg-v4 ) ).

      LOOP AT lt_value INTO DATA(ls_value) WHERE v IS NOT INITIAL.
        DATA(lr_val) = bapi_assign_component( iv_names = ls_value-n
                                              is_data  = <ls_row> ).
        IF lr_val IS BOUND.
          ASSIGN lr_val->* TO <lv_val>.
          <lv_val> = ls_value-v.
        ENDIF.
      ENDLOOP.

      IF <ls_row> IS NOT INITIAL.
        INSERT <ls_row> INTO TABLE ct_bapiret.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD bapi_get_type.

    FIELD-SYMBOLS <lv_type> TYPE any.

    DATA(lr_type) = bapi_assign_component( iv_names = `TYPE MSGTY`
                                           is_data  = is_data ).
    IF lr_type IS BOUND.
      ASSIGN lr_type->* TO <lv_type>.
      rv_type = <lv_type>.
    ENDIF.

  ENDMETHOD.

  METHOD bapi_assign_component.

    FIELD-SYMBOLS <lv_comp> TYPE any.

    SPLIT iv_names AT ` ` INTO TABLE DATA(lt_name).
    LOOP AT lt_name INTO DATA(lv_name).
      ASSIGN COMPONENT lv_name OF STRUCTURE is_data TO <lv_comp>.
      IF sy-subrc = 0.
        rr = REF #( <lv_comp> ).
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD bapi_call_function.

    DATA lv_name TYPE string.

    lv_name = c_trim_upper( iv_name ).
    TRY.
        IF sys_fm_call( function    = lv_name
                        t_param     = it_param
                        t_exception = VALUE #( ( name = `ERROR_MESSAGE` value = 98 )
                                               ( name = `OTHERS`        value = 99 ) ) ) <> 0.
          bapi_msg_add_sy( EXPORTING iv_context = lv_name
                           CHANGING  ct_bapiret = ct_bapiret ).
        ENDIF.
      CATCH cx_root INTO DATA(lx_error).
        bapi_msg_add_exception( EXPORTING ix_error   = lx_error
                                CHANGING  ct_bapiret = ct_bapiret ).
    ENDTRY.

  ENDMETHOD.

  METHOD bapi_set_material.

    DATA(lv_field) = to_upper( iv_field ).
    DATA(lr_value) = bapi_assign_component( iv_names = |{ lv_field }_LONG { lv_field }|
                                            is_data  = cs_data ).
    IF lr_value IS BOUND.
      FIELD-SYMBOLS <lv_value> TYPE any.
      ASSIGN lr_value->* TO <lv_value>.
      <lv_value> = iv_matnr.
    ENDIF.

  ENDMETHOD.

  METHOD bapi_refresh_buffers.

    DATA lv_fm TYPE c LENGTH 30.

    DATA(lv_area) = to_upper( iv_area ).
    DATA(lt_fm) = VALUE string_table( ).

    IF lv_area = `HU` OR lv_area = `DELIVERY` OR lv_area = `ALL`.
      INSERT `HU_PACKING_REFRESH` INTO TABLE lt_fm.
    ENDIF.
    IF lv_area = `DELIVERY` OR lv_area = `ALL`.
      INSERT `LE_DELIVERY_REFRESH_BUFFER` INTO TABLE lt_fm.
    ENDIF.
    IF lv_area = `SERIAL` OR lv_area = `DELIVERY` OR lv_area = `ALL`.
      INSERT `SERIAL_INTTAB_REFRESH` INTO TABLE lt_fm.
    ENDIF.

    " called by name, so the class compiles where the modules do not exist;
    " a buffer that cannot be reset here is no error of the caller
    LOOP AT lt_fm INTO DATA(lv_name).
      lv_fm = lv_name.
      TRY.
          CALL FUNCTION lv_fm
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD conv_alpha_in.

    rv_result = condense( CONV string( iv_value ) ).
    IF rv_result CO `0123456789` AND rv_result IS NOT INITIAL.
      rv_result = c_pad_left( val = rv_result
                              len = iv_length ).
    ENDIF.

  ENDMETHOD.

  METHOD csv_escape_cell.

    result = val.
    IF result NA |"\n\r| AND find( val = result
                                   sub = separator ) < 0.
      RETURN.
    ENDIF.
    result = replace( val  = result
                      sub  = `"`
                      with = `""`
                      occ  = 0 ).
    result = |"{ result }"|.

  ENDMETHOD.

  METHOD csv_split_rows.
    DATA lt_cells TYPE string_table.
    DATA lv_cell  TYPE string.
    " the cell was opened with a quote - an empty quoted cell is content
    DATA lv_was_quoted TYPE abap_bool.
    DATA lv_pos   TYPE i.
    DATA lv_next  TYPE string.
    " the next separator, CR and LF at or behind lv_pos, lv_len for none:
    " the text between two of them goes into the cell in one piece - one
    " concatenation per character made a long cell cost its length squared.
    " Each is searched again only when lv_pos has passed it
    DATA lv_next_sep TYPE i VALUE -1.
    DATA lv_next_cr  TYPE i VALUE -1.
    DATA lv_next_lf  TYPE i VALUE -1.

    DATA(lv_len) = strlen( val ).
    DATA(lv_sep_len) = strlen( separator ).
    IF lv_sep_len = 0.
      RETURN.
    ENDIF.

    WHILE lv_pos < lv_len.

      DATA(lv_char) = substring( val = val
                                 off = lv_pos
                                 len = 1 ).

      IF lv_char = `"` AND lv_cell IS INITIAL AND lv_was_quoted = abap_false.
        csv_read_quoted( EXPORTING val  = val
                         CHANGING  pos  = lv_pos
                                   cell = lv_cell ).
        lv_was_quoted = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_next_sep < lv_pos.
        lv_next_sep = find( val = val
                            sub = separator
                            off = lv_pos ).
        IF lv_next_sep < 0.
          lv_next_sep = lv_len.
        ENDIF.
      ENDIF.
      IF lv_next_cr < lv_pos.
        lv_next_cr = find( val = val
                           sub = |\r|
                           off = lv_pos ).
        IF lv_next_cr < 0.
          lv_next_cr = lv_len.
        ENDIF.
      ENDIF.
      IF lv_next_lf < lv_pos.
        lv_next_lf = find( val = val
                           sub = |\n|
                           off = lv_pos ).
        IF lv_next_lf < 0.
          lv_next_lf = lv_len.
        ENDIF.
      ENDIF.

      IF lv_next_sep = lv_pos.
        INSERT lv_cell INTO TABLE lt_cells.
        CLEAR lv_cell.
        lv_was_quoted = abap_false.
        lv_pos = lv_pos + lv_sep_len.
        CONTINUE.
      ENDIF.

      IF lv_char <> |\n| AND lv_char <> |\r|.
        DATA(lv_end) = nmin( val1 = lv_next_sep
                             val2 = nmin( val1 = lv_next_cr
                                          val2 = lv_next_lf ) ).
        lv_cell = lv_cell && substring( val = val
                                        off = lv_pos
                                        len = lv_end - lv_pos ).
        lv_pos = lv_end.
        CONTINUE.
      ENDIF.

      " the end of a row - CRLF is one line end. An empty row is left out
      lv_next = substring( val = val
                           off = lv_pos + 1
                           len = nmin( val1 = 1
                                       val2 = lv_len - lv_pos - 1 ) ).
      IF lv_char = |\r| AND lv_next = |\n|.
        lv_pos = lv_pos + 1.
      ENDIF.
      lv_pos = lv_pos + 1.
      IF lt_cells IS NOT INITIAL OR lv_cell IS NOT INITIAL OR lv_was_quoted = abap_true.
        INSERT lv_cell INTO TABLE lt_cells.
        INSERT lt_cells INTO TABLE result.
      ENDIF.
      CLEAR lt_cells.
      CLEAR lv_cell.
      lv_was_quoted = abap_false.

    ENDWHILE.

    IF lt_cells IS NOT INITIAL OR lv_cell IS NOT INITIAL OR lv_was_quoted = abap_true.
      INSERT lv_cell INTO TABLE lt_cells.
      INSERT lt_cells INTO TABLE result.
    ENDIF.
  ENDMETHOD.

  METHOD filter_sql_find.

    DATA lv_pos      TYPE i.
    DATA lv_in_quote TYPE abap_bool.

    result = -1.
    DATA(lv_upper) = to_upper( val ).
    DATA(lv_keyword) = to_upper( keyword ).
    DATA(lv_len) = strlen( lv_upper ).
    DATA(lv_key_len) = strlen( lv_keyword ).
    IF lv_key_len = 0.
      RETURN.
    ENDIF.

    WHILE lv_pos + lv_key_len <= lv_len.
      IF substring( val = lv_upper
                    off = lv_pos
                    len = 1 ) = `'`.
        lv_in_quote = xsdbool( lv_in_quote = abap_false ).
      ELSEIF lv_in_quote = abap_false AND substring( val = lv_upper
                                                     off = lv_pos
                                                     len = lv_key_len ) = lv_keyword.
        result = lv_pos.
        RETURN.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD filter_sql_strip_parens.

    DATA lv_pos      TYPE i.
    DATA lv_depth    TYPE i.
    DATA lv_in_quote TYPE abap_bool.
    DATA lv_wrapped  TYPE abap_bool.

    result = c_trim( val ).

    DO.
      DATA(lv_len) = strlen( result ).
      IF lv_len < 2 OR result(1) <> `(`.
        RETURN.
      ENDIF.

      " does the first parenthesis close at the last character?
      lv_pos = 0.
      lv_depth = 0.
      lv_in_quote = abap_false.
      lv_wrapped = abap_false.
      WHILE lv_pos < lv_len.
        DATA(lv_char) = substring( val = result
                                   off = lv_pos
                                   len = 1 ).
        IF lv_char = `'`.
          lv_in_quote = xsdbool( lv_in_quote = abap_false ).
        ELSEIF lv_in_quote = abap_false AND lv_char = `(`.
          lv_depth = lv_depth + 1.
        ELSEIF lv_in_quote = abap_false AND lv_char = `)`.
          lv_depth = lv_depth - 1.
          IF lv_depth = 0.
            lv_wrapped = xsdbool( lv_pos = lv_len - 1 ).
            EXIT.
          ENDIF.
        ENDIF.
        lv_pos = lv_pos + 1.
      ENDWHILE.

      IF lv_wrapped = abap_false.
        RETURN.
      ENDIF.
      result = c_trim( substring( val = result
                                  off = 1
                                  len = lv_len - 2 ) ).
    ENDDO.

  ENDMETHOD.

  METHOD filter_sql_get_or_group.

    DATA ls_range     TYPE ty_s_range.
    DATA lv_fieldname TYPE string.

    DATA(lt_conds) = filter_sql_split_top_level( val = val
                                                 sep = ` OR ` ).

    LOOP AT lt_conds INTO DATA(lv_cond).

      CLEAR ls_range.
      CLEAR lv_fieldname.

      filter_get_range_by_sql_cond(
        EXPORTING val       = filter_sql_strip_parens( lv_cond )
        IMPORTING fieldname = lv_fieldname
                  range     = ls_range ).

      IF ls_range-option IS INITIAL.
        CONTINUE.
      ENDIF.

      IF result-name IS INITIAL.
        result-name = lv_fieldname.
      ENDIF.
      INSERT ls_range INTO TABLE result-t_range.

    ENDLOOP.

  ENDMETHOD.

  METHOD filter_sql_like_to_pattern.

    " % and _ are the LIKE wildcards; a *, + or # in a LIKE pattern is a
    " plain character, which CP writes with the escape # in front
    DATA(lv_pos) = 0.
    DATA(lv_len) = strlen( val ).
    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = val
                                 off = lv_pos
                                 len = 1 ).
      CASE lv_char.
        WHEN `%`.
          result = result && `*`.
        WHEN `_`.
          result = result && `+`.
        WHEN `*` OR `+` OR `#`.
          result = result && `#` && lv_char.
        WHEN OTHERS.
          result = result && lv_char.
      ENDCASE.
      lv_pos = lv_pos + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD filter_option_negate.

    result = val.
    CASE result.
      WHEN `EQ`. result = `NE`.
      WHEN `NE`. result = `EQ`.
      WHEN `LT`. result = `GE`.
      WHEN `LE`. result = `GT`.
      WHEN `GT`. result = `LE`.
      WHEN `GE`. result = `LT`.
      WHEN `CP`. result = `NP`.
      WHEN `NP`. result = `CP`.
      WHEN `BT`. result = `NB`.
      WHEN `NB`. result = `BT`.
    ENDCASE.

  ENDMETHOD.

  METHOD filter_sql_quote.

    result = |'{ replace( val  = CONV string( val )
                          sub  = `'`
                          with = `''`
                          occ  = 0 ) }'|.

  ENDMETHOD.

  METHOD csv_read_quoted.

    " the text up to the next quote in one piece, not one concatenation
    " per character
    DATA(lv_len) = strlen( val ).
    pos = pos + 1.

    WHILE pos < lv_len.
      DATA(lv_quote) = find( val = val
                             sub = `"`
                             off = pos ).
      " a quote that is never closed: the cell runs to the end of the text
      IF lv_quote < 0.
        cell = cell && substring( val = val
                                  off = pos ).
        pos = lv_len.
        RETURN.
      ENDIF.
      cell = cell && substring( val = val
                                off = pos
                                len = lv_quote - pos ).
      pos = lv_quote + 1.
      " a quote: doubled it is one quote of the text, alone it closes the cell
      IF pos >= lv_len OR substring( val = val
                                     off = pos
                                     len = 1 ) <> `"`.
        RETURN.
      ENDIF.
      cell = cell && `"`.
      pos = pos + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD csv_strip_bom.

    " the UTF-8 decoding keeps the mark EF BB BF as the character U+FEFF in
    " front of the first column name. Built at run time from its bytes - a
    " literal U+FEFF in the source breaks the 7-bit rule and the downport.
    " Where the decoding drops it, it is empty
    result = val.
    TRY.
        DATA(lv_bom) = conv_get_string_by_xstring( CONV xstring( `EFBBBF` ) ).
        IF lv_bom IS NOT INITIAL AND find( val = result
                                           sub = lv_bom ) = 0.
          result = substring( val = result
                              off = strlen( lv_bom ) ).
        ENDIF.
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
        " no codepage API: the text cannot have come through one either
    ENDTRY.

  ENDMETHOD.

  METHOD csv_get_column_name.

    DATA lv_pos TYPE i.

    DATA(lv_name) = c_trim_upper( val ).
    DATA(lv_len) = strlen( lv_name ).

    WHILE lv_pos < lv_len.
      DATA(lv_char) = substring( val = lv_name
                                 off = lv_pos
                                 len = 1 ).
      IF lv_char CA `ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_`.
        result = result && lv_char.
      ELSE.
        result = result && `_`.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    IF result IS INITIAL.
      result = |COL{ index }|.
    ELSEIF result(1) CA `0123456789`.
      result = `C_` && result.
    ENDIF.
    IF strlen( result ) > 30.
      result = result(30).
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_rap_cause_value.

    " the name of an IF_ABAP_BEHV=>T_FAIL_CAUSE value to its base value;
    " -1 for a name this release does not know, which the text keeps
    " (the upper-case name in a variable first: the downport turns the
    " SWITCH into a CASE, whose operand takes no built-in function on 7.02)
    DATA lv_name TYPE string.
    lv_name = to_upper( name ).
    result = SWITCH i( lv_name
      WHEN `UNSPECIFIC`      THEN 0
      WHEN `UNAUTHORIZED`    THEN 401
      WHEN `NOT_FOUND`       THEN 404
      WHEN `CONFLICT`        THEN 409
      WHEN `LOCKED`          THEN 423
      WHEN `DEPENDENCY`      THEN 424
      WHEN `NOT_IMPLEMENTED` THEN 501
      WHEN `DISABLED`        THEN 4221
      WHEN `READONLY`        THEN 4222
      ELSE -1 ).

  ENDMETHOD.

  METHOD conv_get_string_by_file.

    DATA lv_invalid TYPE abap_bool.

    conv_get_string_by_xstring_cp( EXPORTING val      = val
                                             codepage = `UTF-8`
                                             encoding = `UTF-8`
                                   IMPORTING result   = result
                                             invalid  = lv_invalid ).
    IF lv_invalid = abap_false.
      RETURN.
    ENDIF.

    " Not valid UTF-8: Excel's plain "CSV" export on Windows writes
    " Windows-1252 (SAP code page 1160), and an umlaut is then a single byte
    " no UTF-8 decoder accepts. Only a conversion error gets here - a
    " missing codepage API left conv_get_string_by_xstring_cp with its own
    " UNSUPPORTED_CODEPAGE_API above
    TRY.
        conv_get_string_by_xstring_cp( EXPORTING val      = val
                                                 codepage = `windows-1252`
                                                 encoding = `1160`
                                       IMPORTING result   = result
                                                 invalid  = lv_invalid ).
      CATCH zabaputil_cx_util_error.
        " neither API knows Windows-1252 here
        lv_invalid = abap_true.
    ENDTRY.

    IF lv_invalid = abap_true.
      CLEAR result.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |UNSUPPORTED_ENCODING - the file is neither UTF-8 nor Windows-1252, | &&
                |save it as "CSV UTF-8" and upload it again|.
    ENDIF.

  ENDMETHOD.

  METHOD conv_get_string_by_xstring_cp.

    DATA conv          TYPE REF TO object.
    DATA conv_codepage TYPE c LENGTH 21.
    DATA conv_in_class TYPE c LENGTH 18.
    DATA lv_encoding   TYPE c LENGTH 20.
    DATA lx_first      TYPE REF TO cx_root.

    CLEAR result.
    invalid = abap_false.

    TRY.

        conv_codepage = `CL_ABAP_CONV_CODEPAGE`.
        CALL METHOD (conv_codepage)=>create_in
          EXPORTING
            codepage = codepage
          RECEIVING
            instance = conv.

        CALL METHOD conv->(`IF_ABAP_CONV_IN~CONVERT`)
          EXPORTING
            source = val
          RECEIVING
            result = result.
        RETURN.

      CATCH cx_sy_conversion_codepage.
        " the API is there and the bytes are not valid in this code page -
        " the classic API would reject them too
        CLEAR result.
        invalid = abap_true.
        RETURN.

      CATCH cx_root INTO lx_first ##NO_HANDLER.
        " deliberate: capturing the cause IS the handling - it is chained into
        " UNSUPPORTED_CODEPAGE_API below if the classic API fails too. Falling
        " through is the point; the modern class is absent on older releases,
        " which is the whole reason both paths exist. A code page the modern
        " class does not know (CX_PARAMETER_INVALID_RANGE) falls through too
    ENDTRY.

    " The classic API. Guarded by its own TRY since 2026-09: it used to be the
    " BODY of the CATCH above, so when it failed too the dynamic-call
    " exception left this method raw. On a release that has neither class -
    " and in any runtime that is not an SAP system, where a dynamic
    " CALL METHOD resolves nothing - that reached the caller as
    " CX_SY_DYN_CALL_ILLEGAL_CLASS, which nothing here catches by name.
    " The view builder is one of those callers and builds its
    " control-character set through this method, so a missing codepage class
    " took down every view render with an exception that named a kernel
    " class rather than the framework.
    TRY.

        conv_in_class = `CL_ABAP_CONV_IN_CE`.
        lv_encoding = encoding.
        CALL METHOD (conv_in_class)=>create
          EXPORTING
            encoding = lv_encoding
          RECEIVING
            conv     = conv.

        CALL METHOD conv->(`CONVERT`)
          EXPORTING
            input = val
          IMPORTING
            data  = result.

      CATCH cx_sy_conversion_codepage.
        CLEAR result.
        invalid = abap_true.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |UNSUPPORTED_CODEPAGE_API - neither CL_ABAP_CONV_CODEPAGE | &&
                       |nor CL_ABAP_CONV_IN_CE could convert here|
            previous = lx_first.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

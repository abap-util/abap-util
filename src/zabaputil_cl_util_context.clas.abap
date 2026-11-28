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

    " the elementary kinds a caller has to tell apart to write a value into
    " a file and to read it back (the XLSX reader and writer): text, a
    " NUMC with its leading zeros, bytes
    CLASS-DATA cv_typedescr_typekind_char    TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_num     TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_string  TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_hex     TYPE c LENGTH 1 READ-ONLY.
    CLASS-DATA cv_typedescr_typekind_xstring TYPE c LENGTH 1 READ-ONLY.

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
    TYPES ty_t_name_value TYPE STANDARD TABLE OF ty_s_name_value WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_token,
        key      TYPE string,
        text     TYPE string,
        visible  TYPE abap_bool,
        selkz    TYPE abap_bool,
        editable TYPE abap_bool,
      END OF ty_s_token.
    TYPES ty_t_token TYPE STANDARD TABLE OF ty_s_token WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_range,
        sign   TYPE c LENGTH 1,
        option TYPE c LENGTH 2,
        low    TYPE string,
        high   TYPE string,
      END OF ty_s_range.
    TYPES ty_t_range TYPE STANDARD TABLE OF ty_s_range WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_filter_multi,
        name            TYPE string,
        t_range         TYPE ty_t_range,
        t_token         TYPE ty_t_token,
        t_token_added   TYPE ty_t_token,
        t_token_removed TYPE ty_t_token,
      END OF ty_s_filter_multi.
    TYPES ty_t_filter_multi TYPE STANDARD TABLE OF ty_s_filter_multi WITH DEFAULT KEY.

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
      ty_t_msg TYPE STANDARD TABLE OF ty_s_msg WITH DEFAULT KEY.

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
    TYPES ty_t_flat_comp TYPE STANDARD TABLE OF ty_s_flat_comp WITH DEFAULT KEY.

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

    "! The filters of a WHERE clause written as filter_get_sql_where writes
    "! it: conditions on one field joined with OR, those groups joined with
    "! AND. A condition: =, <>, <, <=, >, >= (with or without blanks),
    "! [NOT] BETWEEN, [NOT] LIKE [ESCAPE], [NOT] IN ( list ). Anything a
    "! filter cannot say - an OR across fields, IS NULL, NOT in front, a
    "! subquery - raises zabaputil_cx_util_error instead of being dropped.
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

    "! A standard table whose line has one string component per name - the
    "! table for a file whose columns are only known when it is read (CSV,
    "! XLSX). The names have to be valid and unique component names.
    "! @parameter val    | the component names, in the order of the columns
    "! @parameter result | a reference to the new, empty table
    CLASS-METHODS rtti_create_tab_by_names
      IMPORTING
        val           TYPE string_table
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

    "! The content of a data URI (data:[mime][;base64],payload) - see
    "! conv_get_file_by_data_uri. A bare base64 payload without the
    "! `data:...,` prefix is decoded as base64.
    CLASS-METHODS conv_get_xstring_by_data_uri
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE xstring.

    TYPES:
      BEGIN OF ty_s_data_uri,
        mime_type TYPE string,
        content   TYPE xstring,
      END OF ty_s_data_uri.

    "! A data URI (RFC 2397, data:[mime][;charset=...][;base64],payload) as
    "! MIME type and content: the payload base64-decoded when the header
    "! says `;base64`, otherwise percent-decoded to its bytes (text between
    "! the escapes as UTF-8). The MIME type is lower case; when the URI
    "! names none, or only application/octet-stream, it is taken from
    "! file_name (file_get_mimetype) if one is given. A bare base64 payload
    "! without the `data:...,` prefix is decoded as base64.
    CLASS-METHODS conv_get_file_by_data_uri
      IMPORTING
        val           TYPE clike
        file_name     TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_data_uri.

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

    "! whether a type kind is a number: i, int1, int2, int8, p, f,
    "! decfloat16, decfloat34 - the one list the number checks share. int8
    "! by its value `8`, a constant lower releases do not have. Public for
    "! a caller that holds the type kind of a component, not its value
    CLASS-METHODS rtti_check_typekind_number
      IMPORTING
        val           TYPE clike
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
    TYPES ty_t_gs1 TYPE STANDARD TABLE OF ty_s_gs1 WITH DEFAULT KEY.

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
    TYPES ty_t_barcode_field TYPE STANDARD TABLE OF ty_s_barcode_field WITH DEFAULT KEY.

    TYPES:
      "! one field of a split barcode
      BEGIN OF ty_s_barcode_value,
        name  TYPE string,
        value TYPE string,
      END OF ty_s_barcode_value.
    TYPES ty_t_barcode_value TYPE STANDARD TABLE OF ty_s_barcode_value WITH DEFAULT KEY.

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

    TYPES ty_t_object TYPE STANDARD TABLE OF REF TO object WITH DEFAULT KEY.
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
    TYPES ty_t_parallel TYPE STANDARD TABLE OF ty_s_parallel WITH DEFAULT KEY.

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
    TYPES ty_t_longtext_line TYPE STANDARD TABLE OF ty_s_longtext_line WITH DEFAULT KEY.

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
    "! Adobe forms (ADS). Standard ABAP only. Any spool number is read -
    "! a caller that takes it from a user checks that the user may see it,
    "! or passes owner_only to accept only the current user's requests
    "! (TSP01-RQOWNER).
    CLASS-METHODS spool_get_pdf
      IMPORTING
        spool_id      TYPE i
        owner_only    TYPE abap_bool DEFAULT abap_false
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
    TYPES ty_t_variant TYPE STANDARD TABLE OF ty_s_variant WITH DEFAULT KEY.
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
    TYPES ty_t_variant_value TYPE STANDARD TABLE OF ty_s_variant_value WITH DEFAULT KEY.

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
    TYPES ty_t_gos TYPE STANDARD TABLE OF ty_s_gos WITH DEFAULT KEY.

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

    "! The content of a GOS attachment (doc_id from gos_list). Standard ABAP
    "! only. Any SAPoffice document is read, with no authorization check - a
    "! caller that takes doc_id from a user checks that the user may see the
    "! business object it belongs to.
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
    TYPES ty_t_field_diff TYPE STANDARD TABLE OF ty_s_field_diff WITH DEFAULT KEY.

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

    TYPES ty_t_data TYPE STANDARD TABLE OF ty_s_transport WITH DEFAULT KEY.

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
    TYPES ty_t_dfies_2 TYPE STANDARD TABLE OF ty_s_dfies_2 WITH DEFAULT KEY.

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
        interface  TYPE STANDARD TABLE OF ty_ddshiface WITH DEFAULT KEY,                      " Placeholder for Interface of Search Help
        fielddescr TYPE STANDARD TABLE OF ty_s_dfies_2 WITH DEFAULT KEY,
        fieldprop  TYPE STANDARD TABLE OF ty_ddshfprop WITH DEFAULT KEY,
        selopt     TYPE STANDARD TABLE OF ty_ddshselopt WITH DEFAULT KEY,
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
    TYPES ty_t_bal_header TYPE STANDARD TABLE OF ty_s_bal_header WITH DEFAULT KEY.

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

    "! with_commit: COMMIT WORK after the delete (the default, as before
    "! the parameter existed) - that commits the caller's open work too.
    "! abap_false leaves the commit to the caller: a RAP handler must not
    "! commit, a program that may still roll back should not.
    "! An initial object deletes nothing.
    CLASS-METHODS bal_delete_before
      IMPORTING
        object      TYPE clike
        subobject   TYPE clike OPTIONAL
        !days       TYPE i DEFAULT 30
        with_commit TYPE abap_bool DEFAULT abap_true.

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

    "! with_commit: COMMIT WORK after the log is saved (the default, as
    "! before the parameter existed) - that commits the caller's open work
    "! too. abap_false leaves the commit to the caller, whose LUW then
    "! writes the log: a RAP handler must not commit, a program that may
    "! still roll back should not.
    CLASS-METHODS bal_create
      IMPORTING
        object      TYPE clike
        subobject   TYPE clike
        id          TYPE clike
        t_log       TYPE ty_t_msg
        with_commit TYPE abap_bool DEFAULT abap_true.

    "! Appends to the newest log of object, subobject and id (on-premise;
    "! on ABAP Cloud the first one the log API returns), or creates one.
    "! with_commit as in bal_create.
    CLASS-METHODS bal_update
      IMPORTING
        object      TYPE clike
        subobject   TYPE clike
        id          TYPE clike
        t_log       TYPE ty_t_msg
        with_commit TYPE abap_bool DEFAULT abap_true.

    "! Deletes the logs of object, subobject and id - all three initial
    "! deletes nothing, never every log of the client. with_commit as in
    "! bal_create.
    CLASS-METHODS bal_delete
      IMPORTING
        object      TYPE clike
        subobject   TYPE clike
        id          TYPE clike
        with_commit TYPE abap_bool DEFAULT abap_true.

    " ========== Transport Requests ==========

    TYPES:
      BEGIN OF ty_s_tr_object,
        pgmid    TYPE string,
        object   TYPE string,
        obj_name TYPE string,
      END OF ty_s_tr_object.
    TYPES ty_t_tr_object TYPE STANDARD TABLE OF ty_s_tr_object WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_tr_request,
        trkorr      TYPE string,
        description TYPE string,
        owner       TYPE string,
        status      TYPE string,
        type        TYPE string,
      END OF ty_s_tr_request.
    TYPES ty_t_tr_request TYPE STANDARD TABLE OF ty_s_tr_request WITH DEFAULT KEY.

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

    "! The user holding the lock of the ENQUEUE_ module val for the values
    "! in t_param. The lock argument is matched from its start, each value
    "! at its key field's width (a key that is not given matches any
    "! value); where the module's interface cannot be read, the values are
    "! looked for anywhere in the argument, as before.
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
    TYPES ty_t_changdoc TYPE STANDARD TABLE OF ty_s_changdoc WITH DEFAULT KEY.

    CLASS-METHODS auth_check
      IMPORTING
        object        TYPE clike
        field         TYPE clike
        !value        TYPE clike
        activity      TYPE clike DEFAULT '03'
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! The text of a message with its variables. langu: the language of
    "! the text on-premise (read from T100); on ABAP Cloud the text is the
    "! one of the logon language whatever langu says. A message that has
    "! no text comes back as `ID NO: v1 v2 v3 v4`.
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

    "! Sends a mail to one recipient. result: the mail system accepted it;
    "! a mail that cannot be built or handed over raises
    "! zabaputil_cx_util_error. sender: the sender address - initial is the
    "! current user on-premise and the configured default sender on ABAP
    "! Cloud. with_commit: COMMIT WORK after sending on-premise (the
    "! default, as before the parameter existed) - that commits the
    "! caller's open work too; abap_false leaves it to the caller, and the
    "! mail leaves with the caller's commit.
    CLASS-METHODS mail_send
      IMPORTING
        !to           TYPE string
        subject       TYPE string
        body          TYPE string
        html          TYPE abap_bool DEFAULT abap_false
        sender        TYPE string OPTIONAL
        with_commit   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! The name of the job started - job_get_status needs its count as
    "! well, which job_submit returns.
    CLASS-METHODS job_submit_report
      IMPORTING
        report          TYPE clike
        variant         TYPE clike OPTIONAL
        start_immediate TYPE abap_bool DEFAULT abap_true
        job_name        TYPE clike OPTIONAL
      RETURNING
        VALUE(result)   TYPE string.

    TYPES:
      BEGIN OF ty_s_job,
        jobname  TYPE string,
        jobcount TYPE string,
      END OF ty_s_job.

    "! Starts the report as a background job (JOB_OPEN, JOB_SUBMIT,
    "! JOB_CLOSE) and returns its name and count, the key job_get_status
    "! takes. job_name: initial is JOB_<date><time>. A job that cannot be
    "! submitted or closed is deleted again and raises
    "! zabaputil_cx_util_error. Not on ABAP Cloud (application jobs).
    CLASS-METHODS job_submit
      IMPORTING
        report          TYPE clike
        variant         TYPE clike OPTIONAL
        start_immediate TYPE abap_bool DEFAULT abap_true
        job_name        TYPE clike OPTIONAL
      RETURNING
        VALUE(result)   TYPE ty_s_job.

    "! The next number of a number range object. subobject: the number
    "! range INTERVAL (NR_RANGE_NR) - the name is kept for the callers that
    "! pass it. nr_subobject and toyear: the subobject and the year of an
    "! object that depends on them (a company code, a fiscal year), passed
    "! on as SUBOBJECT and TOYEAR.
    CLASS-METHODS numrange_get_next
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike DEFAULT `01`
        nr_subobject  TYPE clike OPTIONAL
        toyear        TYPE clike OPTIONAL
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
    TYPES ty_t_zip_file TYPE STANDARD TABLE OF ty_s_zip_file WITH DEFAULT KEY.

    "! An Excel workbook (Office Open XML) of one sheet: a header row with
    "! the component names, then one row per line of val. A number is a
    "! number cell; a date (YYYY-MM-DD), a time (HH:MM:SS) and everything
    "! else are text; a table, a structure or a reference stays empty. No
    "! abap2xlsx, no XCO - CL_ABAP_ZIP only, so it runs on every release
    "! down to 7.02 and on ABAP Cloud
    CLASS-METHODS conv_get_xlsx_by_itab
      IMPORTING
        val           TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE xstring.

    "! Deprecated - raises always. A workbook is read with
    "! zabaputil_cl_util_xlsx (itab_by_xlsx( ), get_table_ref( )): this
    "! class cannot call the class that calls it. Kept so that a caller of
    "! the old signature still activates; nothing in the catalog or its
    "! consumers calls it.
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

    "! A ZIP archive opened, its files unpacked one at a time with
    "! zip_get( ) - for an archive of which a few files are read. Raises,
    "! like zip_unpack( ), for data that is no ZIP archive.
    CLASS-METHODS zip_load
      IMPORTING
        !val          TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO object.

    "! the names of the files of an archive opened with zip_load( )
    CLASS-METHODS zip_names
      IMPORTING
        !zip          TYPE REF TO object
      RETURNING
        VALUE(result) TYPE string_table.

    "! one file of an archive opened with zip_load( ), unpacked - raises
    "! for a name the archive does not have, or a file that cannot be unpacked
    CLASS-METHODS zip_get
      IMPORTING
        !zip          TYPE REF TO object
        !name         TYPE clike
      RETURNING
        VALUE(result) TYPE xstring.

    " ========== XML pull reader ==========

    TYPES:
      BEGIN OF ty_s_xml_attribute,
        name  TYPE string,
        value TYPE string,
      END OF ty_s_xml_attribute.
    TYPES ty_t_xml_attribute TYPE STANDARD TABLE OF ty_s_xml_attribute WITH DEFAULT KEY.

    CONSTANTS:
      "! the kinds of node xml_reader_next( ) hands out - `end` (initial) when
      "! the document is read to its end
      BEGIN OF cs_xml_node,
        open  TYPE string VALUE `O` ##NO_TEXT,
        close TYPE string VALUE `C` ##NO_TEXT,
        value TYPE string VALUE `V` ##NO_TEXT,
        end   TYPE string VALUE `` ##NO_TEXT,
      END OF cs_xml_node.

    "! A reader over an XML document, node by node and without building a
    "! tree - what a large document (the sheet of a workbook) needs. The
    "! handle goes to xml_reader_next( ); it is the sXML reader, which is
    "! there on every release down to 7.02 and released on ABAP Cloud, and
    "! this class is the only one that names it. A text of blanks only
    "! arrives where it is all an element holds; the indentation between
    "! two elements is not reported
    CLASS-METHODS xml_reader_create
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO object.

    "! The next node of the document: an element opening (name, and its
    "! attributes when t_attribute is asked for), its text (value) or its
    "! end (name). Names come without their namespace prefix. kind is
    "! initial (cs_xml_node-end) behind the last node
    CLASS-METHODS xml_reader_next
      IMPORTING
        reader      TYPE REF TO object
      EXPORTING
        kind        TYPE string
        name        TYPE string
        value       TYPE string
        t_attribute TYPE ty_t_xml_attribute.

    "! DEFLATE (RFC 1951) of bytes, without any header - what CL_ABAP_GZIP
    "! writes. The class is released on ABAP Cloud and there on every
    "! release down to 7.02
    CLASS-METHODS gzip_compress
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring.

    "! The bytes of a DEFLATE stream (RFC 1951, no header)
    CLASS-METHODS gzip_decompress
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring.

    "! A zlib stream (RFC 1950): the two header bytes 78 9C, the DEFLATE
    "! data and the Adler-32 of the input. This is what a PDF stream with
    "! the filter FlateDecode and the IDAT chunks of a PNG hold
    CLASS-METHODS zlib_compress
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring.

    "! The bytes of a zlib stream (RFC 1950). The checksum at its end is
    "! not verified
    CLASS-METHODS zlib_decompress
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring.

    "! Adler-32 of bytes, as four bytes in network byte order - the
    "! checksum that ends a zlib stream. Plain ABAP
    CLASS-METHODS hash_adler32
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring.

    TYPES ty_t_codepoint TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    "! The Unicode code points of a text, one row per character. A
    "! character outside the Basic Multilingual Plane, which a string
    "! holds as two UTF-16 code units, is one row
    CLASS-METHODS c_get_codepoints
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_codepoint.

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

    "! The text of an XML text node or attribute value with its references
    "! resolved: the five named entities (&lt; &gt; &amp; &quot; &apos;)
    "! and every character reference, decimal (&#10;) or hex (&#xA;) - the
    "! counterpart of c_escape_xml. One pass from left to right, so an
    "! escaped reference stays what it was (`&amp;lt;` is `&lt;`). A
    "! reference it does not know, or one that names no character XML
    "! allows, stays as it is written.
    CLASS-METHODS c_unescape_xml
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

    "! The file name of a path as a browser or an upload hands it over:
    "! what follows the last `/` or `\` (`C:\fakepath\report.xlsx` gives
    "! `report.xlsx`), the value itself when it has neither.
    CLASS-METHODS file_get_name_by_path
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

    "! tz: an SAP time zone key (TTZZ, at most six characters: `UTC`,
    "! `CET`, `EST`) - not an IANA name like Europe/Berlin. A zone that is
    "! no key, or a conversion that fails, raises zabaputil_cx_util_error;
    "! it returned an empty or wrong date and time.
    CLASS-METHODS time_stampl_to_tz
      IMPORTING
        val  TYPE timestampl
        tz   TYPE clike DEFAULT `UTC`
      EXPORTING
        date TYPE d
        time TYPE t.

    "! tz as in time_stampl_to_tz
    CLASS-METHODS time_stampl_from_tz
      IMPORTING
        date          TYPE d
        time          TYPE t
        tz            TYPE clike DEFAULT `UTC`
      RETURNING
        VALUE(result) TYPE timestampl.

    "! Name, e-mail, language, time zone and formats of a user - initial
    "! uname is the current one. On-premise every field is the given
    "! user's (USR01, USR02, USER_ADDR, ADR6). ABAP Cloud only answers for
    "! the current user: another uname raises zabaputil_cx_util_error -
    "! the fields were the caller's under the other user's name.
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

    "! val in unit_from as a value in unit_to. On ABAP Cloud from the SI
    "! conversion of I_UnitOfMeasure: value_SI = value * rate + additive
    "! value, so temperatures convert too; two units of different
    "! dimensions raise, as UNIT_CONVERSION_SIMPLE does on-premise.
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

    "! The decimals of a currency: TCURX on premise, I_Currency on ABAP
    "! Cloud - 2 for one the system does not list (SAP's rule: a currency
    "! without an entry has two), and for one that cannot be read.
    CLASS-METHODS currency_get_decimals
      IMPORTING
        currency      TYPE clike
      RETURNING
        VALUE(result) TYPE i.

    "! The decimals a unit of measure is shown with: T006-DECAN on premise,
    "! I_UnitOfMeasure on ABAP Cloud - -1 for a unit the system does not
    "! list, or that cannot be read.
    CLASS-METHODS unit_get_decimals
      IMPORTING
        unit          TYPE clike
      RETURNING
        VALUE(result) TYPE i.

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
    TYPES ty_t_gs1_rule TYPE STANDARD TABLE OF ty_s_gs1_rule WITH DEFAULT KEY.

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
    TYPES ty_t_fm_param TYPE STANDARD TABLE OF ty_s_fm_param WITH DEFAULT KEY.

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
    TYPES ty_t_regex_match TYPE STANDARD TABLE OF ty_s_regex_match WITH DEFAULT KEY.

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

    " The value of a literal in a condition: a quoted literal (a doubled
    " quote inside is one quote) or one word without blanks (a number, a
    " host variable). Anything else - a literal followed by more text, an
    " unclosed quote - raises zabaputil_cx_util_error.
    CLASS-METHODS filter_sql_get_literal
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    " Raises zabaputil_cx_util_error unless the name is a plain field name -
    " the characters filter_get_sql_where accepts.
    CLASS-METHODS filter_sql_check_name
      IMPORTING
        val TYPE clike.

    CLASS-DATA gv_check_cloud TYPE abap_bool.
    CLASS-DATA gv_check_cloud_cached TYPE abap_bool.

    " Guards uuid_get_c32 against re-entering itself through the exception it
    " raises (zabaputil_cx_util_error reads a uuid for its full report).
    CLASS-DATA gv_uuid_failed TYPE abap_bool.

    " A member of an object - or, without obj, a static member of the class
    " class_name - reached dynamically, as XCO and the cloud mail API need
    " it: the method or attribute name, found on the class itself or on one
    " of the interfaces it implements (IF_...~NAME) - a dynamic call by the
    " bare name of an interface method only works where the class declares
    " an alias. result: a reference to the RETURNING value of the method,
    " or to the attribute. arg: the value of the importing parameter
    " arg_name - without arg_name of the first mandatory one not in
    " t_param, so its name need not be known - converted to its type (a
    " table line by line: object references cast, structures by their
    " component names). t_param: further parameters, passed as they are. A
    " member the object does not have raises zabaputil_cx_util_error.
    CLASS-METHODS dyn_call
      IMPORTING
        obj           TYPE REF TO object OPTIONAL
        class_name    TYPE clike OPTIONAL
        name          TYPE clike
        arg           TYPE any OPTIONAL
        arg_name      TYPE clike OPTIONAL
        t_param       TYPE abap_parmbind_tab OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO data.

    " dyn_call for a member that is an object: the object
    CLASS-METHODS dyn_call_obj
      IMPORTING
        obj           TYPE REF TO object OPTIONAL
        class_name    TYPE clike OPTIONAL
        name          TYPE clike
        arg           TYPE any OPTIONAL
        arg_name      TYPE clike OPTIONAL
        t_param       TYPE abap_parmbind_tab OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO object.

    " the object in a static attribute, `XCO_CP_CTS=>TRANSPORT` - an XCO
    " factory is an attribute, not a method
    CLASS-METHODS dyn_static
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO object.

    " XCO_CP_CTS=>TRANSPORT->FOR( trkorr )
    CLASS-METHODS xco_transport
      IMPORTING
        trkorr        TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO object.

    " the text of an XCO value: an object gives its VALUE (or NAME)
    " attribute, anything else its own value
    CLASS-METHODS xco_text
      IMPORTING
        val           TYPE REF TO data
      RETURNING
        VALUE(result) TYPE string.

    " the SI conversion of a unit from I_UnitOfMeasure (value_SI = value *
    " factor + additive) and its dimension. The columns are looked up by
    " the names the view may carry (SIUnitCnvrsnRate..., UnitOfMeasureSI...)
    " - one fixed name was a failed SELECT where the view names it
    " otherwise. An unknown unit, or one without a rate, raises.
    CLASS-METHODS unit_get_si
      IMPORTING
        unit      TYPE clike
      EXPORTING
        factor    TYPE decfloat34
        additive  TYPE decfloat34
        dimension TYPE string.

    " the zone trimmed - raises when it is longer than the six characters
    " of an SAP time zone key: `Europe/Berlin` was cut to `Europe`
    CLASS-METHODS time_check_zone
      IMPORTING
        tz            TYPE clike
      RETURNING
        VALUE(result) TYPE string.

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

    " the handles (BAL_T_LOGH) of the logs loaded for the keys - unbound
    " when none matches. latest_only: only the newest one (ALDATE, ALTIME,
    " LOGNUMBER)
    CLASS-METHODS bal_std_load_handles
      IMPORTING
        object        TYPE clike
        subobject     TYPE clike
        id            TYPE clike
        latest_only   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO data.

    " the headers (BALHDR_T) BAL_DB_SEARCH finds for a BAL_S_LFIL filter -
    " unbound when none matches
    CLASS-METHODS bal_std_search
      IMPORTING
        filter        TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

    " deletes the logs of the headers BAL_DB_SEARCH found - BAL_DB_DELETE
    " takes the headers, it has no filter parameter
    CLASS-METHODS bal_std_delete
      IMPORTING
        headers     TYPE ANY TABLE
        with_commit TYPE abap_bool.

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
    " it does not have). Each value is typed like the module's parameter
    " (a STRING for a VBAK-VBELN parameter is a CALL_FUNCTION_CONFLICT_TYPE).
    " ABAP Cloud: the lock object behind the name through
    " CL_ABAP_LOCK_OBJECT_FACTORY, ENQUEUE or DEQUEUE with the values as
    " parameters - CX_ABAP_FOREIGN_LOCK is subrc 1
    CLASS-METHODS lock_call_function
      IMPORTING
        val           TYPE clike
        t_param       TYPE ty_t_lock_param OPTIONAL
      EXPORTING
        subrc         TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.

    " the importing parameters of a lock module that make the lock
    " argument (SEQG3-GARG), in their order: the client and the key
    " fields, without MODE_, X_ and the _SCOPE / _WAIT / _COLLECT ones.
    " Empty when the interface cannot be read
    CLASS-METHODS lock_get_arg_params
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_fm_param.

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
    TYPES ty_t_csv_rows TYPE STANDARD TABLE OF string_table WITH DEFAULT KEY.

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

    " The ranges of conditions joined with OR - all on one field, which is
    " the name. A condition on another field, a condition joined with AND
    " inside the OR, NOT IN inside an OR or a condition no range can hold
    " raises zabaputil_cx_util_error: a filter cannot say it.
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

    " the kind of an XLSX column (conv_get_xlsx_by_itab): N number, D date,
    " T time, S text, empty for a type without text
    CLASS-METHODS xlsx_get_column_kind
      IMPORTING
        !type         TYPE REF TO cl_abap_datadescr
      RETURNING
        VALUE(result) TYPE string.

    " one inline-text cell of the sheet, `<c/>` for an empty text
    CLASS-METHODS xlsx_get_text_cell
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    " the bytes of a percent-encoded text (a data URI payload): an escape is
    " its byte, the text between escapes its UTF-8 bytes; `+` is a plus
    CLASS-METHODS conv_get_xstring_by_percent
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    " the character one reference of c_unescape_xml stands for - name is
    " what stands between `&` and `;` - empty when it names none
    CLASS-METHODS c_unescape_xml_reference
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

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
    cv_typedescr_typekind_char       = cl_abap_typedescr=>typekind_char.
    cv_typedescr_typekind_num        = cl_abap_typedescr=>typekind_num.
    cv_typedescr_typekind_string     = cl_abap_typedescr=>typekind_string.
    cv_typedescr_typekind_hex        = cl_abap_typedescr=>typekind_hex.
    cv_typedescr_typekind_xstring    = cl_abap_typedescr=>typekind_xstring.
    cv_typedescr_kind_struct         = cl_abap_typedescr=>kind_struct.
    cv_typedescr_kind_elem           = cl_abap_typedescr=>kind_elem.
    cv_typedescr_kind_ref            = cl_abap_typedescr=>kind_ref.
    cv_objectdescr_public            = cl_abap_objectdescr=>public.

  ENDMETHOD.

  METHOD db_rollback.

    ROLLBACK WORK.

  ENDMETHOD.

  METHOD boolean_abap_2_json.
      DATA temp173 TYPE string.

    IF boolean_check_by_data( val ) IS NOT INITIAL.
      
      IF val = abap_true.
        temp173 = `true`.
      ELSE.
        temp173 = `false`.
      ENDIF.
      result = temp173.
    ELSE.
      result = val.
    ENDIF.

  ENDMETHOD.

  METHOD boolean_check_by_data.
        DATA lo_descr TYPE REF TO cl_abap_typedescr.
        FIELD-SYMBOLS <ls_bool_cache> TYPE zabaputil_cl_util_context=>ty_s_bool_cache.
        DATA temp174 TYPE zabaputil_cl_util_context=>ty_s_bool_cache.

    TRY.
        
        lo_descr = cl_abap_elemdescr=>describe_by_data( val ).

        " all supported boolean types are character-like flags, this check
        " filters out every other type before the cache lookup
        IF lo_descr->type_kind <> cl_abap_typedescr=>typekind_char.
          RETURN.
        ENDIF.

        " type descriptors are singletons, so the reference identifies the
        " type without converting/hashing the absolute name on every call
        
        READ TABLE mt_bool_cache WITH KEY typedescr = lo_descr ASSIGNING <ls_bool_cache>.
        IF sy-subrc = 0.
          result = <ls_bool_cache>-is_bool.
          RETURN.
        ENDIF.

        result = boolean_check_by_name( lo_descr->get_relative_name( ) ).

        
        CLEAR temp174.
        temp174-typedescr = lo_descr.
        temp174-is_bool = result.
        INSERT temp174 INTO TABLE mt_bool_cache.

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

    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( check_unassign_initial( val ) = abap_false ).
    result = temp1.

  ENDMETHOD.

  METHOD check_unassign_initial.
    FIELD-SYMBOLS <any> TYPE data.
    DATA temp2 TYPE xsdboolean.

    IF val IS INITIAL.
      result = abap_true.
      RETURN.
    ENDIF.

    
    ASSIGN val->* TO <any>.

    
    temp2 = boolc( <any> IS INITIAL ).
    result = temp2.

  ENDMETHOD.

  METHOD conv_copy_ref_data.

    FIELD-SYMBOLS <from>   TYPE data.
    FIELD-SYMBOLS <result> TYPE data.

    IF rtti_check_ref_data( from ) IS NOT INITIAL.
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
    GET REFERENCE OF val INTO result.

  ENDMETHOD.

  METHOD conv_get_xstring_by_data_uri.

    result = conv_get_file_by_data_uri( val )-content.

  ENDMETHOD.

  METHOD conv_get_file_by_data_uri.

    DATA lv_header TYPE string.
    DATA lv_data   TYPE string.

    DATA temp175 TYPE string.
    DATA lv_val LIKE temp175.
    DATA lv_base64 LIKE abap_true.
      TYPES temp1 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_part TYPE temp1.
      DATA temp176 LIKE LINE OF lt_part.
      DATA lr_part LIKE REF TO temp176.
        DATA lv_part TYPE string.
    temp175 = val.
    
    lv_val = temp175.
    IF lv_val NS `,`.
      " a bare base64 payload, without the `data:...;base64,` prefix - the
      " SPLIT put the whole value into the metadata half and the payload
      " decoded to an empty xstring without a word
      result-content = conv_decode_x_base64( lv_val ).
      RETURN.
    ENDIF.
    SPLIT lv_val AT `,` INTO lv_header lv_data.

    " data:[<mime type>][;charset=...][;base64] - the payload used to be
    " base64-decoded whatever the header said, so a percent-encoded one
    " (`data:text/plain,a%20b`) came out as garbage
    
    lv_base64 = abap_true.
    IF strlen( lv_header ) >= 5 AND to_lower( lv_header(5) ) = `data:`.
      lv_base64 = abap_false.
      

      SPLIT substring( val = lv_header
                       off = 5 ) AT `;` INTO TABLE lt_part.
      
      
      LOOP AT lt_part REFERENCE INTO lr_part.
        
        lv_part = to_lower( c_trim( lr_part->* ) ).
        IF sy-tabix = 1.
          result-mime_type = lv_part.
        ELSEIF lv_part = `base64`.
          lv_base64 = abap_true.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lv_base64 = abap_true.
      result-content = conv_decode_x_base64( lv_data ).
    ELSE.
      result-content = conv_get_xstring_by_percent( lv_data ).
    ENDIF.

    IF ( result-mime_type IS INITIAL OR result-mime_type = `application/octet-stream` )
        AND file_name IS NOT INITIAL.
      result-mime_type = file_get_mimetype( file_name ).
    ENDIF.

  ENDMETHOD.

  METHOD conv_get_xstring_by_percent.

    " the bytes as hex digits, converted in one step at the end - a byte
    " string has no expression that appends to it (see url_decode)
    DATA lv_hex TYPE string.
    DATA lv_run TYPE string.
    DATA lv_x   TYPE x LENGTH 1.

    DATA lv_len TYPE i.
    DATA lv_pos TYPE i.
      DATA lv_char TYPE string.
      DATA lv_code TYPE string.
          DATA lv_run_x TYPE xstring.
    lv_len = strlen( val ).
    
    lv_pos = 0.
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = val
                                 off = lv_pos
                                 len = 1 ).
      
      lv_code = ``.
      IF lv_char = `%` AND lv_pos + 2 < lv_len.
        lv_code = to_upper( substring( val = val
                                       off = lv_pos + 1
                                       len = 2 ) ).
      ENDIF.
      IF lv_code IS NOT INITIAL AND lv_code CO `0123456789ABCDEF`.
        IF lv_run IS NOT INITIAL.
          
          lv_run_x = conv_get_xstring_by_string( lv_run ).
          lv_hex = |{ lv_hex }{ lv_run_x }|.
          CLEAR lv_run.
        ENDIF.
        lv_x = lv_code.
        lv_hex = |{ lv_hex }{ lv_x }|.
        lv_pos = lv_pos + 3.
        CONTINUE.
      ENDIF.
      lv_run = lv_run && lv_char.
      lv_pos = lv_pos + 1.
    ENDWHILE.
    IF lv_run IS NOT INITIAL.
      lv_run_x = conv_get_xstring_by_string( lv_run ).
      lv_hex = |{ lv_hex }{ lv_run_x }|.
    ENDIF.

    result = lv_hex.

  ENDMETHOD.

  METHOD c_trim.
      DATA lv_before LIKE result.

    result = val.
    " spaces and tabs alternate at either end (`\t \tx`) - one pass of each
    " leaves the inner layer standing, so strip until nothing changes. No
    " fixed number of passes: ten left the eleventh layer of a padded value
    " standing. Every pass that changes something shortens the string, so
    " the loop ends
    DO.
      
      lv_before = result.
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

    DATA temp177 TYPE abap_component_tab.
    DATA temp652 LIKE LINE OF temp177.
    DATA lr_comp LIKE REF TO temp652.
      DATA temp178 TYPE zabaputil_cl_util_context=>ty_s_filter_multi.
    temp177 = rtti_get_t_attri_by_any( val ).
    
    
    LOOP AT temp177 REFERENCE INTO lr_comp.
      
      CLEAR temp178.
      temp178-name = lr_comp->name.
      INSERT temp178 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_range_by_value.
    DATA lv_val TYPE string.
    DATA temp179 TYPE string_table.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    " one value is a list of one - CP for a pattern, EQ otherwise
    
    lv_val = val.
    
    CLEAR temp179.
    INSERT lv_val INTO TABLE temp179.
    result = filter_get_range_t_by_value_t( temp179 ).

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
    DATA lv_value LIKE val.
    DATA lv_length TYPE i.
      DATA temp181 TYPE zabaputil_cl_util_context=>ty_s_range-sign.
      DATA temp182 TYPE zabaputil_cl_util_context=>ty_s_range-option.
    lv_value = val.
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.
    
    lv_length = strlen( lv_value ).

    " `!(...)`: the negated token filter_get_token_t_by_range_t renders for
    " an excluding row and for NE / NB / NP - the inner token with the sign
    " flipped (I NE x comes back as the equivalent E EQ x, E NE x as I EQ x)
    IF lv_length > 3 AND lv_value(2) = `!(` AND substring( val = lv_value
                                                           off = lv_length - 1 ) = `)`.
      result = filter_get_range_by_token( substring( val = lv_value
                                                     off = 2
                                                     len = lv_length - 3 ) ).
      
      IF result-sign = `E`.
        temp181 = `I`.
      ELSE.
        temp181 = `E`.
      ENDIF.
      result-sign = temp181.
      RETURN.
    ENDIF.

    result-sign = `I`.

    " the empty-value token of filter_get_token_range_mapping - read as `<`
    " it was LT `leer>`
    IF lv_value = `<leer>`.
      result-option = `EQ`.
      RETURN.
    ENDIF.

    IF lv_length >= 2 AND ( lv_value(2) = `<=` OR lv_value(2) = `>=` ).
      
      IF lv_value(1) = `<`.
        temp182 = `LE`.
      ELSE.
        temp182 = `GE`.
      ENDIF.
      result-option = temp182.
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
    FIELD-SYMBOLS <temp183> TYPE zabaputil_cl_util_context=>ty_s_filter_multi.
DATA lr_filter LIKE REF TO <temp183>.
    DATA ls_token LIKE LINE OF lr_filter->t_token_removed.
      DATA temp184 TYPE zabaputil_cl_util_context=>ty_s_token.
    DATA lt_range TYPE zabaputil_cl_util_context=>ty_t_range.
    FIELD-SYMBOLS <temp653> LIKE LINE OF result.
    DATA temp654 LIKE sy-tabix.

    result = val.
    
    READ TABLE result WITH KEY name = name ASSIGNING <temp183>.
IF sy-subrc <> 0.
  ASSERT 1 = 0.
ENDIF.

GET REFERENCE OF <temp183> INTO lr_filter.
    
    LOOP AT lr_filter->t_token_removed INTO ls_token.
      DELETE lr_filter->t_token WHERE key = ls_token-key.
    ENDLOOP.

    LOOP AT lr_filter->t_token_added INTO ls_token.
      
      CLEAR temp184.
      temp184-key = ls_token-key.
      temp184-text = ls_token-text.
      temp184-visible = abap_true.
      temp184-editable = abap_true.
      INSERT temp184 INTO TABLE lr_filter->t_token.
    ENDLOOP.

    CLEAR lr_filter->t_token_removed.
    CLEAR lr_filter->t_token_added.

    
    
    
    temp654 = sy-tabix.
    READ TABLE result WITH KEY name = name ASSIGNING <temp653>.
    sy-tabix = temp654.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lt_range = filter_get_range_t_by_token_t( <temp653>-t_token ).
    lr_filter->t_range = lt_range.

  ENDMETHOD.

  METHOD filter_get_range_t_by_token_t.

    DATA ls_token LIKE LINE OF val.
    LOOP AT val INTO ls_token.
      INSERT filter_get_range_by_token( ls_token-text ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_token_range_mapping.

    DATA temp185 TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp186 LIKE LINE OF temp185.
    CLEAR temp185.
    
    temp186-n = `EQ`.
    temp186-v = `={LOW}`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `LT`.
    temp186-v = `<{LOW}`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `LE`.
    temp186-v = `<={LOW}`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `GT`.
    temp186-v = `>{LOW}`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `GE`.
    temp186-v = `>={LOW}`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `CP`.
    temp186-v = `*{LOW}*`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `BT`.
    temp186-v = `{LOW}...{HIGH}`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `NB`.
    temp186-v = `!({LOW}...{HIGH})`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `NE`.
    temp186-v = `!(={LOW})`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `NP`.
    temp186-v = `!(*{LOW}*)`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `!<leer>`.
    temp186-v = `!(<leer>)`.
    INSERT temp186 INTO TABLE temp185.
    temp186-n = `<leer>`.
    temp186-v = `<leer>`.
    INSERT temp186 INTO TABLE temp185.
    result = temp185.

  ENDMETHOD.

  METHOD filter_get_token_t_by_range_t.

    DATA lt_mapping TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA lt_tab TYPE ty_t_range.
    FIELD-SYMBOLS <lt_source> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_range> TYPE ty_s_range.
    FIELD-SYMBOLS <lv_part> TYPE any.
      DATA lv_index TYPE i.
      FIELD-SYMBOLS <ls_source> TYPE ANY.
    DATA temp187 LIKE LINE OF lt_tab.
    DATA lr_row LIKE REF TO temp187.
      DATA lv_option TYPE string.
      DATA lv_value TYPE string.
      DATA temp188 TYPE string.
      DATA temp189 TYPE zabaputil_cl_util_context=>ty_s_name_value.
        FIELD-SYMBOLS <temp190> LIKE LINE OF lt_mapping.
        DATA temp191 LIKE sy-tabix.
        DATA temp192 TYPE string.
      DATA temp193 TYPE zabaputil_cl_util_context=>ty_s_token.
    lt_mapping = filter_get_token_range_mapping( ).

    

    itab_corresponding( EXPORTING val = val
                        CHANGING  tab = lt_tab ).

    " LOW / HIGH again from the source row, through data_get_string: the
    " move into the string fields above wrote a number the way a MOVE does
    " - sign behind it and padded (`5- `), so a select-option on an integer
    " field showed `=42 ` and `5-...10 `
    " UNASSIGN + IS ASSIGNED, not sy-subrc, in the loop (#1937)
    
    
    
    ASSIGN val TO <lt_source>.
    IF <lt_source> IS ASSIGNED.
      
      lv_index = 0.
      
      LOOP AT <lt_source> ASSIGNING <ls_source>.
        lv_index = lv_index + 1.
        UNASSIGN <ls_range>.
        READ TABLE lt_tab INDEX lv_index ASSIGNING <ls_range>.
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

    
    
    LOOP AT lt_tab REFERENCE INTO lr_row.

      " an option the mapping does not know - initial (a row appended with
      " sign and low only) or lower-case - used to raise a raw
      " CX_SY_ITAB_LINE_NOT_FOUND out of the whole call; it renders as the
      " equality token, which is what a range row without an option means
      
      
      lv_option = to_upper( lr_row->option ).
      
      CLEAR temp188.
      
      READ TABLE lt_mapping INTO temp189 WITH KEY n = lv_option.
      IF sy-subrc = 0.
        temp188 = temp189-v.
      ENDIF.
      lv_value = temp188. "#EC CI_SORTSEQ
      IF lv_value IS INITIAL.
        
        
        temp191 = sy-tabix.
        READ TABLE lt_mapping WITH KEY n = `EQ` ASSIGNING <temp190>.
        sy-tabix = temp191.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_value = <temp190>-v. "#EC CI_SORTSEQ
      ENDIF.
      IF ( lv_option = `CP` OR lv_option = `NP` ) AND lr_row->low CA `*`.
        " LOW is a pattern already - wrapping it in the template's asterisks
        " would turn `abc*` into `*abc**`
        
        IF lv_option = `CP`.
          temp192 = lr_row->low.
        ELSE.
          temp192 = |!({ lr_row->low })|.
        ENDIF.
        lv_value = temp192.
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

      
      CLEAR temp193.
      temp193-key = lv_value.
      temp193-text = lv_value.
      temp193-visible = abap_true.
      temp193-editable = abap_true.
      INSERT temp193 INTO TABLE result.
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
    TYPES temp2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lt_index TYPE temp2.
    DATA lt_name  TYPE string_table.
    DATA lv_whole_row TYPE abap_bool.
    DATA lr_kept TYPE REF TO data.

    DATA temp194 TYPE string.
    DATA lv_search LIKE temp194.
      DATA lv_index TYPE i.
      DATA temp195 LIKE LINE OF fields.
      DATA lr_field LIKE REF TO temp195.
      DATA lv_check_found LIKE abap_false.
      DATA temp196 LIKE LINE OF lt_index.
      DATA lr_index LIKE REF TO temp196.
      DATA temp197 LIKE LINE OF lt_name.
      DATA lr_name LIKE REF TO temp197.
    IF ignore_case = abap_true.
      temp194 = to_upper( val ).
    ELSE.
      temp194 = val.
    ENDIF.
    
    lv_search = temp194.
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
    READ TABLE tab INDEX 1 ASSIGNING <row>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF fields IS INITIAL.
      
      lv_index = 1.
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
      
      
      LOOP AT fields REFERENCE INTO lr_field.
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

      
      lv_check_found = abap_false.
      IF lv_whole_row = abap_true.
        lv_check_found = itab_filter_check_text( text        = |{ <row> }|
                                                 search      = lv_search
                                                 ignore_case = ignore_case ).
      ENDIF.
      
      
      LOOP AT lt_index REFERENCE INTO lr_index.
        ASSIGN COMPONENT lr_index->* OF STRUCTURE <row> TO <field>.
        IF itab_filter_check_text( text        = |{ <field> }|
                                   search      = lv_search
                                   ignore_case = ignore_case ) = abap_true.
          lv_check_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      
      
      LOOP AT lt_name REFERENCE INTO lr_name.
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
    DATA temp198 TYPE REF TO cl_abap_tabledescr.
    DATA tab LIKE temp198.
    DATA temp199 TYPE REF TO cl_abap_structdescr.
    DATA struc LIKE temp199.
    DATA temp200 TYPE abap_component_view_tab.
    DATA temp655 LIKE LINE OF temp200.
    DATA lr_comp LIKE REF TO temp655.
      DATA temp201 TYPE string.
    DATA temp1 TYPE string.
      DATA temp202 LIKE LINE OF lt_names.
      DATA lr_name LIKE REF TO temp202.
      DATA temp2 TYPE string.

    ASSIGN val TO <tab>.
    
    temp198 ?= cl_abap_typedescr=>describe_by_data( <tab> ).
    
    tab = temp198.

    
    temp199 ?= tab->get_table_line_type( ).
    
    struc = temp199.

    " the included view, not get_components( ): an include (a DDIC
    " table's .INCLUDE, an append, INCLUDE TYPE) is ONE nameless component
    " there, so the header had an empty cell where the rows had one cell
    " per field of the include, and every column behind it shifted. The
    " kind is a property of the line type - decided here once per column,
    " not described again for every cell. A table, a structure or a
    " reference has no text - in a string template it is a runtime error,
    " so its cell stays empty
    
    temp200 = struc->get_included_view( ).
    
    
    LOOP AT temp200 REFERENCE INTO lr_comp.
      INSERT csv_escape_cell( lr_comp->name ) INTO TABLE lt_cells.
      
      IF lr_comp->type->kind = cl_abap_typedescr=>kind_elem.
        temp201 = lr_comp->name.
      ELSE.
        CLEAR temp201.
      ENDIF.
      INSERT temp201 INTO TABLE lt_names.
    ENDLOOP.
    
    temp1 = concat_lines_of( table = lt_cells sep = `;` ).
    INSERT temp1 INTO TABLE lt_lines.

    LOOP AT <tab> ASSIGNING <row>.

      CLEAR lt_cells.
      
      
      LOOP AT lt_names REFERENCE INTO lr_name.
        IF lr_name->* IS INITIAL.
          INSERT `` INTO TABLE lt_cells.
          CONTINUE.
        ENDIF.
        ASSIGN COMPONENT lr_name->* OF STRUCTURE <row> TO <field>.
        " the value as it is - a separator, a quote or a line break inside
        " it is quoted, not replaced
        INSERT csv_escape_cell( |{ <field> }| ) INTO TABLE lt_cells.
      ENDLOOP.
      
      temp2 = concat_lines_of( table = lt_cells sep = `;` ).
      INSERT temp2 INTO TABLE lt_lines.
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

    DATA lt_rows TYPE zabaputil_cl_util_context=>ty_t_csv_rows.
    DATA lt_cols LIKE LINE OF lt_rows.
    FIELD-SYMBOLS <temp656> LIKE LINE OF lt_rows.
    DATA temp657 LIKE sy-tabix.
    DATA lv_last TYPE i.
    FIELD-SYMBOLS <temp203> LIKE LINE OF lt_cols.
    DATA temp204 LIKE sy-tabix.
    DATA temp205 LIKE LINE OF lt_cols.
    DATA lr_col LIKE REF TO temp205.
      DATA lv_tabix LIKE sy-tabix.
      DATA lv_name TYPE string.
      DATA temp206 LIKE sy-subrc.
      DATA temp207 LIKE sy-tabix.
        DATA lv_suffix TYPE i.
          DATA temp208 LIKE sy-subrc.
          DATA temp209 LIKE sy-tabix.
      DATA temp210 TYPE abap_componentdescr.
    DATA struc TYPE REF TO cl_abap_structdescr.
    DATA temp211 TYPE REF TO cl_abap_datadescr.
    DATA data LIKE temp211.
    DATA o_table_desc TYPE REF TO cl_abap_tabledescr.
    DATA temp212 LIKE LINE OF lt_rows.
    DATA lr_rows LIKE REF TO temp212.
      FIELD-SYMBOLS <row> TYPE data.
      DATA lv_index TYPE i.
    lt_rows = csv_split_rows( csv_strip_bom( val ) ).
    " no row, no header to build a table from
    IF lt_rows IS INITIAL.
      RETURN.
    ENDIF.

    
    
    
    temp657 = sy-tabix.
    READ TABLE lt_rows INDEX 1 ASSIGNING <temp656>.
    sy-tabix = temp657.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lt_cols = <temp656>.
    " a header written with a separator behind the last column
    
    lv_last = lines( lt_cols ).
    
    
    IF lv_last > 1.
      temp204 = sy-tabix.
      READ TABLE lt_cols INDEX lv_last ASSIGNING <temp203>.
      sy-tabix = temp204.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
    ENDIF.
    IF lv_last > 1 AND <temp203> IS INITIAL.
      DELETE lt_cols INDEX lv_last.
    ENDIF.

    
    
    LOOP AT lt_cols REFERENCE INTO lr_col.
      
      lv_tabix = sy-tabix.

      
      lv_name = csv_get_column_name( val   = lr_col->*
                                           index = lv_tabix ).
      " a name the header carries twice - and the fallback name may be
      " taken too (`A;COL3;A`), which made the structure creation below
      " raise CX_SY_STRUCT_COMP_NAME out of the method
      
      
      temp207 = sy-tabix.
      READ TABLE lt_names WITH KEY table_line = lv_name TRANSPORTING NO FIELDS.
      temp206 = sy-subrc.
      sy-tabix = temp207.
      IF temp206 = 0.
        lv_name = |COL{ lv_tabix }|.
        " DO with an EXIT, not WHILE line_exists( ): the 7.02 downport
        " evaluates a WHILE condition with a table expression once, in
        " front of the loop, and the loop never ended
        
        lv_suffix = 1.
        DO.
          
          
          temp209 = sy-tabix.
          READ TABLE lt_names WITH KEY table_line = lv_name TRANSPORTING NO FIELDS.
          temp208 = sy-subrc.
          sy-tabix = temp209.
          IF NOT temp208 = 0.
            EXIT.
          ENDIF.
          lv_suffix = lv_suffix + 1.
          lv_name = |COL{ lv_tabix }_{ lv_suffix }|.
        ENDDO.
      ENDIF.
      INSERT lv_name INTO TABLE lt_names.

      " a string per column - a fixed length cut every longer value
      
      CLEAR temp210.
      temp210-name = lv_name.
      temp210-type = cl_abap_elemdescr=>get_string( ).
      INSERT temp210 INTO TABLE lt_comp.
    ENDLOOP.

    
    struc = cl_abap_structdescr=>create( lt_comp ).
    
    temp211 ?= struc.
    
    data = temp211.
    
    o_table_desc = cl_abap_tabledescr=>create( p_line_type  = data
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false ).

    CREATE DATA result TYPE HANDLE o_table_desc.
    ASSIGN result->* TO <tab>.

    
    
    LOOP AT lt_rows REFERENCE INTO lr_rows FROM 2.

      CREATE DATA lr_row TYPE HANDLE struc.
      
      ASSIGN lr_row->* TO <row>.

      
      lv_index = 0.
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
        DATA temp1 TYPE REF TO zabaputil_cl_ajson.
        DATA x TYPE REF TO cx_root.
    TRY.

        
        temp1 = zabaputil_cl_ajson=>parse( val ).
        temp1->to_abap( EXPORTING iv_corresponding = abap_true
                                               IMPORTING ev_container     = data ).

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.
  ENDMETHOD.

  METHOD json_stringify.
        DATA temp213 TYPE REF TO zabaputil_if_ajson.
        DATA li_ajson LIKE temp213.
        DATA x TYPE REF TO cx_root.
    TRY.

        
        temp213 ?= zabaputil_cl_ajson=>create_empty( ).
        
        li_ajson = temp213.
        result = li_ajson->set( iv_path = `/`
                                iv_val  = any )->stringify( ).

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.
  ENDMETHOD.

  METHOD rtti_check_class_exists.

    " cached per name - see gt_class_exists at the declaration
    DATA lv_name TYPE string.
    FIELD-SYMBOLS <ls_hit> TYPE zabaputil_cl_util_context=>ty_s_class_exists.
        DATA lo_type TYPE REF TO cl_abap_typedescr.
          DATA temp3 TYPE xsdboolean.
    DATA temp214 TYPE zabaputil_cl_util_context=>ty_s_class_exists.
    lv_name = to_upper( val ).

    
    READ TABLE gt_class_exists WITH KEY name = lv_name ASSIGNING <ls_hit>.
    IF sy-subrc = 0.
      result = <ls_hit>-exists.
      RETURN.
    ENDIF.

    TRY.
        " the cached spelling, not the caller's: the answer is stored under
        " the upper-case name, so the question has to be asked in it too -
        " otherwise the first caller's spelling decides every later answer
        " describe_by_name knows every type - a DDIC table, a structure, a
        " data element of that name is no class
        
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = lv_name
                                             RECEIVING  p_descr_ref    = lo_type
                                             EXCEPTIONS type_not_found = 1 ).
        IF sy-subrc = 0 AND lo_type IS BOUND.
          
          temp3 = boolc( lo_type->kind = cl_abap_typedescr=>kind_class ).
          result = temp3.
        ENDIF.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    
    CLEAR temp214.
    temp214-name = lv_name.
    temp214-exists = result.
    INSERT temp214 INTO TABLE gt_class_exists.

  ENDMETHOD.

  METHOD rtti_check_ref_data.
        DATA lo_typdescr TYPE REF TO cl_abap_typedescr.
        DATA temp215 TYPE REF TO cl_abap_refdescr.
        DATA lo_referenced TYPE REF TO cl_abap_typedescr.
        DATA temp4 TYPE xsdboolean.

    TRY.
        " a kind comparison, not a CAST probe: the cast raised
        " CX_SY_MOVE_CAST_ERROR for every NON-reference value, and the
        " non-reference is the COMMON case on this path (every value routed
        " through conv_copy_ref_data asks) - deciding a type question by
        " exception is orders of magnitude more expensive than comparing.
        " cl_abap_refdescr covers data and object references alike, and so
        " does kind_ref
        
        lo_typdescr = cl_abap_typedescr=>describe_by_data( val ).
        IF lo_typdescr->kind <> cl_abap_typedescr=>kind_ref.
          RETURN.
        ENDIF.
        " kind_ref covers object references too, and the one caller that
        " dereferences on a true answer (conv_copy_ref_data: `from->*`)
        " cannot do that to an object reference. Only a reference to DATA
        " answers true
        
        temp215 ?= lo_typdescr.
        
        lo_referenced = temp215->get_referenced_type( ).
        
        temp4 = boolc( lo_referenced->kind <> cl_abap_typedescr=>kind_class AND lo_referenced->kind <> cl_abap_typedescr=>kind_intf ).
        result = temp4.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_check_type_kind_dref.

    DATA lv_type_kind TYPE abap_typekind.
    DATA temp5 TYPE xsdboolean.
    lv_type_kind = cl_abap_datadescr=>get_data_type_kind( val ).
    
    temp5 = boolc( lv_type_kind = cl_abap_typedescr=>typekind_dref ).
    result = temp5.

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

    DATA rtti TYPE REF TO cl_abap_typedescr.
    DATA temp216 TYPE REF TO cl_abap_refdescr.
    DATA ref LIKE temp216.
    DATA name TYPE abap_abstypename.
    rtti = cl_abap_typedescr=>describe_by_data( in  ).
    
    temp216 ?= rtti.
    
    ref = temp216.
    
    name = ref->get_referenced_type( )->absolute_name.
    result = substring_after( val = name
                              sub = `\INTERFACE=` ).

  ENDMETHOD.

  METHOD rtti_get_type_kind.

    result = cl_abap_datadescr=>get_data_type_kind( val ).

  ENDMETHOD.

  METHOD rtti_get_type_name.
        DATA lo_descr TYPE REF TO cl_abap_typedescr.
        DATA temp217 TYPE REF TO cl_abap_elemdescr.
        DATA lo_ele LIKE temp217.
    TRY.

        
        lo_descr = cl_abap_elemdescr=>describe_by_data( val ).
        
        temp217 ?= lo_descr.
        
        lo_ele = temp217.
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
        DATA lx_cast TYPE REF TO cx_sy_move_cast_error.
    TRY.
        sdescr ?= type.
        
      CATCH cx_sy_move_cast_error INTO lx_cast.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |Include type '{ type->absolute_name }' is not a structure|
            previous = lx_cast.
    ENDTRY.
    result = expand_components( val   = sdescr->get_components( )
                                depth = depth ).

  ENDMETHOD.

  METHOD expand_components.
    DATA temp218 LIKE LINE OF val.
    DATA lr_comp LIKE REF TO temp218.
        DATA lt_include TYPE abap_component_tab.
          DATA temp219 LIKE LINE OF lt_include.
          DATA lr_include LIKE REF TO temp219.

    " see the declaration: bounded so a cyclic include chain surfaces as a
    " readable error instead of a stack-overflow dump. 16 nested include
    " levels is far beyond any real DDIC structure
    IF depth > 16.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `RTTI_INCLUDE_RECURSION - include expansion exceeded 16 levels (cyclic include?)`.
    ENDIF.

    
    
    LOOP AT val REFERENCE INTO lr_comp.
      IF lr_comp->as_include = abap_true.
        " INCLUDE ... RENAMING WITH SUFFIX: the structure's components are
        " named <component><suffix>, and every nested level adds its own -
        " the bare include names do not exist on the structure, so a
        " binding path or ASSIGN COMPONENT built from them fails
        
        lt_include = rtti_get_t_attri_by_include( type  = lr_comp->type
                                                        depth = depth + 1 ).
        IF lr_comp->suffix IS NOT INITIAL.
          
          
          LOOP AT lt_include REFERENCE INTO lr_include.
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
    DATA temp220 TYPE REF TO cl_abap_classdescr.

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
    
    temp220 ?= lo_descr.
    result = temp220->attributes.

  ENDMETHOD.

  METHOD rtti_get_t_attri_by_any.

    DATA lo_struct        TYPE REF TO cl_abap_structdescr.
    DATA lo_type          TYPE REF TO cl_abap_typedescr.
    " declared, not CONV string( ) - see error_get_attributes
    DATA lv_absolute_name TYPE string.
        DATA temp221 TYPE REF TO cl_abap_structdescr.
        DATA temp222 TYPE REF TO cl_abap_structdescr.
        DATA temp658 TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <ls_attri_cache> TYPE zabaputil_cl_util_context=>ty_s_attri_cache.
      DATA temp223 TYPE zabaputil_cl_util_context=>ty_s_attri_cache.

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
        
        temp221 ?= lo_type.
        lo_struct = temp221.
      WHEN cl_abap_typedescr=>kind_table.
        
        
        temp658 ?= lo_type.
        temp222 ?= temp658->get_table_line_type( ).
        lo_struct = temp222.
      WHEN OTHERS.
        lo_struct ?= lo_type.
    ENDCASE.

    " descriptor instances are singletons per type, so the identity check
    " guards against absolute names reused by other (local/anonymous) types
    lv_absolute_name = lo_struct->absolute_name.
    
    READ TABLE mt_attri_cache WITH KEY absolute_name = lv_absolute_name ASSIGNING <ls_attri_cache>.
    IF <ls_attri_cache> IS ASSIGNED AND <ls_attri_cache>-o_struct = lo_struct.
      result = <ls_attri_cache>-t_attri.
      RETURN.
    ENDIF.

    result = expand_components( lo_struct->get_components( ) ).

    IF <ls_attri_cache> IS ASSIGNED.
      <ls_attri_cache>-o_struct = lo_struct.
      <ls_attri_cache>-t_attri  = result.
    ELSE.
      
      CLEAR temp223.
      temp223-absolute_name = lv_absolute_name.
      temp223-o_struct = lo_struct.
      temp223-t_attri = result.
      INSERT temp223 INTO TABLE mt_attri_cache.
    ENDIF.

  ENDMETHOD.

  METHOD rtti_get_t_ddic_fixed_values.
        DATA temp224 TYPE string.
        DATA typedescr TYPE REF TO cl_abap_typedescr.
        DATA temp225 TYPE REF TO cl_abap_elemdescr.
        DATA elemdescr LIKE temp225.

    IF rollname IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        
        temp224 = rollname.
        
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = temp224
                                             RECEIVING  p_descr_ref    = typedescr
                                             EXCEPTIONS type_not_found = 1
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        
        temp225 ?= typedescr.
        
        elemdescr = temp225.

        result = rtti_get_t_fixvalues( elemdescr = elemdescr
                                       langu     = langu ).

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_tab_get_relative_name.

    FIELD-SYMBOLS <table> TYPE any.
        DATA typedesc TYPE REF TO cl_abap_typedescr.
            DATA temp226 TYPE REF TO cl_abap_tabledescr.
            DATA tabledesc LIKE temp226.
            DATA temp227 TYPE REF TO cl_abap_structdescr.
            DATA structdesc LIKE temp227.

    TRY.
        
        typedesc = cl_abap_typedescr=>describe_by_data( table ).

        CASE typedesc->kind.

          WHEN cl_abap_typedescr=>kind_table.
            
            temp226 ?= typedesc.
            
            tabledesc = temp226.
            
            temp227 ?= tabledesc->get_table_line_type( ).
            
            structdesc = temp227.
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

    DATA temp228 TYPE string.
    DATA conex LIKE temp228.
    IF output = abap_true.
      temp228 = |CONVERSION_EXIT_{ convexit }_OUTPUT|.
    ELSE.
      temp228 = |CONVERSION_EXIT_{ convexit }_INPUT|.
    ENDIF.
    
    conex = temp228.

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
      DATA conv TYPE string.
      DATA conex TYPE c LENGTH 30.
      DATA lv_tab TYPE c LENGTH 5.

    IF check_abap_cloud( ) IS NOT INITIAL.

    ELSE.

      
      conv = |CONVERSION_EXIT_{ name-convexit }_INPUT|.
      
      
      lv_tab = 'TFDIR'.

      SELECT SINGLE funcname FROM (lv_tab) INTO conex
        WHERE funcname = conv
        .

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

    DATA temp229 TYPE string.
    DATA lv_sql LIKE temp229.
    DATA lv_squished LIKE lv_sql.
    DATA lv_dummy TYPE string.
    DATA lv_tab TYPE string.
    DATA lv_words TYPE string.
    DATA lv_from TYPE i.
    DATA lv_upper TYPE string.
      DATA lv_pos TYPE i.
      DATA lv_end TYPE i.
      DATA temp230 TYPE string_table.
      DATA lt_clause LIKE temp230.
      DATA lv_clause LIKE LINE OF lt_clause.
        DATA lv_clause_off TYPE i.
    temp229 = val.
    
    lv_sql = temp229.

    
    lv_squished = lv_sql.
    lv_squished = replace( val = lv_squished sub = ` ` with = `` occ = 0 ).
    lv_squished = to_upper( lv_squished ).
    
    
    SPLIT lv_squished AT `SELECTFROM` INTO lv_dummy lv_tab.
    SPLIT lv_tab AT `FIELDS` INTO lv_tab lv_dummy.
    SPLIT lv_tab AT `WHERE` INTO lv_tab lv_dummy.

    result-tabname = lv_tab.

    " the word behind FROM, where the statement has one: cutting the squished
    " text at FIELDS and WHERE took `ZMY_` out of `zmy_fields`
    
    lv_words = | { condense( to_upper( lv_sql ) ) } |.
    
    lv_from = find( val = lv_words
                          sub = ` FROM ` ).
    IF lv_from >= 0.
      SPLIT substring( val = lv_words
                       off = lv_from + 6 ) AT ` ` INTO lv_tab lv_dummy.
      IF lv_tab IS NOT INITIAL.
        result-tabname = lv_tab.
      ENDIF.
    ENDIF.

    
    lv_upper = to_upper( lv_sql ).
    IF lv_upper CS ` WHERE `.
      
      lv_pos = sy-fdpos + 7.
      result-where = c_trim( substring( val = lv_sql
                                        off = lv_pos ) ).
      " the clauses behind the WHERE are no part of it - `ORDER BY b` was
      " read into the value of the last condition
      
      lv_end = strlen( result-where ).
      
      CLEAR temp230.
      INSERT ` ORDER BY ` INTO TABLE temp230.
      INSERT ` GROUP BY ` INTO TABLE temp230.
      INSERT ` HAVING ` INTO TABLE temp230.
      INSERT ` UP TO ` INTO TABLE temp230.
      INSERT ` INTO ` INTO TABLE temp230.
      INSERT ` OFFSET ` INTO TABLE temp230.
      INSERT ` OPTIONS ` INTO TABLE temp230.
      INSERT ` UNION ` INTO TABLE temp230.
      
      lt_clause = temp230.
      
      LOOP AT lt_clause INTO lv_clause.
        
        lv_clause_off = filter_sql_find( val     = result-where
                                               keyword = lv_clause ).
        IF lv_clause_off >= 0 AND lv_clause_off < lv_end.
          lv_end = lv_clause_off.
        ENDIF.
      ENDLOOP.
      result-where = c_trim( substring( val = result-where
                                        len = lv_end ) ).
      lv_end = strlen( result-where ).
      IF lv_end > 0 AND substring( val = result-where
                                   off = lv_end - 1 ) = `.`.
        result-where = c_trim( substring( val = result-where
                                          len = lv_end - 1 ) ).
      ENDIF.
      result-t_filter = filter_get_multi_by_sql_where( result-where ).
    ENDIF.

  ENDMETHOD.

  METHOD time_get_text_by_stampl.

    DATA lv_zone TYPE c LENGTH 6.

    IF time_zone IS INITIAL.
      result = |{ val TIMESTAMP = ISO }|.
    ELSE.
      lv_zone = time_check_zone( time_zone ).
      result = |{ val TIMESTAMP = ISO TIMEZONE = lv_zone }|.
    ENDIF.

    " 2026-11-23T14:05:09,0000000 -> 2026-11-23 14:05:09
    result = replace( val = result sub = `T` with = ` ` ).
    result = replace( val = result sub = `,0000000` with = `` ).

  ENDMETHOD.

  METHOD time_get_date_by_stampl.
    DATA ls_sy TYPE zabaputil_cl_util_context=>ty_syst.
    DATA lv_dummy TYPE t.
    ls_sy = context_get_sy( ).
    
    CONVERT TIME STAMP val TIME ZONE ls_sy-zonlo INTO DATE result TIME lv_dummy.
  ENDMETHOD.

  METHOD time_get_timestamp.
    GET TIME STAMP FIELD result.
  ENDMETHOD.

  METHOD time_get_timestampl.
    GET TIME STAMP FIELD result.
  ENDMETHOD.

  METHOD time_get_time_by_stampl.
    DATA ls_sy TYPE zabaputil_cl_util_context=>ty_syst.
    DATA lv_dummy TYPE d.
    ls_sy = context_get_sy( ).
    
    CONVERT TIME STAMP val TIME ZONE ls_sy-zonlo INTO DATE lv_dummy TIME result.
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

    DATA ls_param LIKE LINE OF t_params.
    LOOP AT t_params INTO ls_param.
      result = |{ result }{ ls_param-n }={ ls_param-v }&|.
    ENDLOOP.
    result = shift_right( val = result
                          sub = `&` ).

  ENDMETHOD.

  METHOD url_param_get.

    DATA lt_params TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA lv_val TYPE string.
    DATA temp232 TYPE string.
    DATA temp233 TYPE zabaputil_cl_util_context=>ty_s_name_value.
    lt_params = url_param_get_tab( url ).
    
    lv_val = c_trim_lower( val ).
    
    CLEAR temp232.
    
    READ TABLE lt_params INTO temp233 WITH KEY n = lv_val.
    IF sy-subrc = 0.
      temp232 = temp233-v.
    ENDIF.
    result = temp232.

  ENDMETHOD.

  METHOD url_param_get_tab.

    " a full URL or a request URI carries a path before its query: cut at
    " the FIRST `?` only. A later `?` is a legal, unencoded character of a
    " value (`title=why?`) - cutting there dropped every parameter before
    " it, app_start included, and the shell fell back to the start page
    " declared, not inline: DATA( ) from a generic CLIKE parameter is
    " "fixed type STRING used for generic type CLIKE" in the extended check
    DATA lv_search TYPE string.
    DATA lv_startup TYPE string.
      DATA lv_before TYPE string.
      DATA lv_packed TYPE string.
      DATA lv_rest TYPE string.
    TYPES temp3 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_param TYPE temp3.
    DATA temp234 LIKE LINE OF lt_param.
    DATA lr_param LIKE REF TO temp234.
      DATA lv_name TYPE string.
      DATA lv_value TYPE string.
      DATA temp235 TYPE zabaputil_cl_util_context=>ty_s_name_value.
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
    
    lv_startup = substring_after( val = |&{ lv_search }|
                                        sub = `&sap-startup-params=` ).
    IF lv_startup IS NOT INITIAL.
      " what stands BEFORE the wrapper is as much a parameter as what follows
      " it: `?app_start=x&sap-startup-params=...` used to lose app_start, and
      " a `sap-client` in front of the wrapper was gone from every link
      " app_get_url rebuilt from this table. lv_before keeps the prepended
      " `&`, so the empty first segment it produces below is skipped like a
      " trailing one
      
      lv_before = substring_before( val = |&{ lv_search }|
                                          sub = `&sap-startup-params=` ).
      
      
      SPLIT lv_startup AT `&` INTO lv_packed lv_rest.
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

    

    SPLIT lv_search AT `&` INTO TABLE lt_param.

    
    
    LOOP AT lt_param REFERENCE INTO lr_param.
      
      
      SPLIT lr_param->* AT `=` INTO lv_name lv_value.
      " an empty segment (empty search string, trailing &) would otherwise
      " produce a phantom nameless parameter that url_param_create_url
      " writes back out as a stray `=&`
      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.
      " normalize the name so lookups are case-insensitive on every input
      " shape (with or without a leading path/question mark) - the value
      " keeps its original case
      
      CLEAR temp235.
      temp235-n = c_trim_lower( lv_name ).
      temp235-v = lv_value.
      INSERT temp235 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD url_param_set.

    DATA lt_params TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA lv_n TYPE string.
    DATA lv_v TYPE string.
    DATA temp236 LIKE LINE OF lt_params.
    DATA lr_params LIKE REF TO temp236.
      DATA temp237 TYPE zabaputil_cl_util_context=>ty_s_name_value.
    lt_params = url_param_get_tab( url ).
    
    lv_n = c_trim_lower( name ).
    
    lv_v = c_trim( value ).

    
    
    LOOP AT lt_params REFERENCE INTO lr_params
         WHERE n = lv_n.
      lr_params->v = lv_v.
    ENDLOOP.
    IF sy-subrc <> 0.
      
      CLEAR temp237.
      temp237-n = lv_n.
      temp237-v = lv_v.
      INSERT temp237 INTO TABLE lt_params.
    ENDIF.

    result = url_param_create_url( lt_params ).

  ENDMETHOD.

  METHOD xml_parse.
        DATA x TYPE REF TO cx_transformation_error.

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
        
      CATCH cx_transformation_error INTO x.
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
      DATA srtti TYPE REF TO object.
      DATA lv_classname TYPE string.
          DATA lx_srtti TYPE REF TO cx_root.
          DATA lv_text TYPE string.

    IF rtti_check_class_exists( `ZCL_SRTTI_TYPEDESCR` ) = abap_true.

      
      
      lv_classname = `ZCL_SRTTI_TYPEDESCR`.
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

          
        CATCH cx_root INTO lx_srtti.

          " keep the root cause - a transformation error on the caller's own
          " data must not be masked behind a bare UNSUPPORTED_FEATURE
          
          lv_text = `UNSUPPORTED_FEATURE`.
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

    DATA x LIKE val.
    x = val.
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
            DATA temp238 TYPE REF TO cl_abap_structdescr.
            DATA temp239 TYPE REF TO cl_abap_structdescr.
            DATA temp659 TYPE REF TO cl_abap_tabledescr.

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
            
            temp238 ?= lo_obj.
            lo_struct = temp238.
          WHEN cl_abap_typedescr=>kind_table.
            
            
            temp659 ?= lo_obj.
            temp239 ?= temp659->get_table_line_type( ).
            lo_struct = temp239.
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

    DATA lt_attri TYPE abap_component_tab.
    DATA temp240 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp240.
      FIELD-SYMBOLS <component> TYPE any.
          DATA temp241 TYPE zabaputil_cl_util_context=>ty_s_name_value.
    lt_attri = rtti_get_t_attri_by_any( val ).
    
    
    LOOP AT lt_attri REFERENCE INTO lr_attri.

      
      ASSIGN COMPONENT lr_attri->name OF STRUCTURE val TO <component>.
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
          
          CLEAR temp241.
          temp241-n = lr_attri->name.
          temp241-v = data_get_string( <component> ).
          INSERT temp241 INTO TABLE result.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD itab_filter_by_t_range.

    DATA ref TYPE REF TO data.
      DATA ls_filter LIKE LINE OF val.
        FIELD-SYMBOLS <field> TYPE any.

    LOOP AT tab REFERENCE INTO ref.
      
      LOOP AT val INTO ls_filter.

        IF ls_filter-t_range IS INITIAL.
          CONTINUE.
        ENDIF.

        
        ASSIGN ref->(ls_filter-name) TO <field>.
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

    DATA ls_filter LIKE LINE OF val.
    LOOP AT val INTO ls_filter.
      IF lines( ls_filter-t_range ) > 0
        OR lines( ls_filter-t_token ) > 0.
        INSERT ls_filter INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_get_sql_where.

    DATA ls_filter LIKE LINE OF val.
      DATA lv_include TYPE string.
      DATA lv_exclude TYPE string.
      DATA ls_range LIKE LINE OF ls_filter-t_range.
        DATA lv_cond TYPE string.
          DATA temp242 TYPE string.
          DATA temp243 TYPE string.
      DATA temp244 TYPE string.
      DATA lv_field_where LIKE temp244.
    LOOP AT val INTO ls_filter.

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
      
      lv_include = ``.
      
      lv_exclude = ``.
      
      LOOP AT ls_filter-t_range INTO ls_range.
        
        lv_cond = filter_get_sql_cond_by_range( fieldname = ls_filter-name
                                                      range     = ls_range ).
        IF lv_cond IS INITIAL.
          CONTINUE.
        ENDIF.
        IF ls_range-sign = `E`.
          
          IF lv_exclude IS INITIAL.
            temp242 = lv_cond.
          ELSE.
            temp242 = |{ lv_exclude } AND { lv_cond }|.
          ENDIF.
          lv_exclude = temp242.
        ELSE.
          
          IF lv_include IS INITIAL.
            temp243 = lv_cond.
          ELSE.
            temp243 = |{ lv_include } OR { lv_cond }|.
          ENDIF.
          lv_include = temp243.
        ENDIF.
      ENDLOOP.

      
      IF lv_include IS NOT INITIAL AND lv_exclude IS NOT INITIAL.
        temp244 = |( { lv_include } ) AND { lv_exclude }|.
      ELSEIF lv_include IS NOT INITIAL.
        temp244 = lv_include.
      ELSE.
        temp244 = lv_exclude.
      ENDIF.
      
      lv_field_where = temp244.

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

    DATA lv_low TYPE string.
    DATA lv_high TYPE string.
    DATA lv_option LIKE range-option.
    DATA lv_like TYPE string.
    DATA lv_pos TYPE i.
    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
    lv_low = replace( val  = range-low
                            sub  = `'`
                            with = `''`
                            occ  = 0 ).
    
    lv_high = replace( val  = range-high
                             sub  = `'`
                             with = `''`
                             occ  = 0 ).
    
    lv_option = range-option.

    IF range-sign = `E`.
      lv_option = filter_option_negate( lv_option ).
    ENDIF.

    " CP: * and + are the wildcards and # makes the next character a plain
    " one (`#*` is a star), as in an IN range. A _ or % the user typed is a
    " plain character, so it is escaped for LIKE
    
    lv_like = ``.
    
    lv_pos = 0.
    
    lv_len = strlen( lv_low ).
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = lv_low
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
    DATA temp245 TYPE string.
    DATA lv_where TYPE string.
    DATA lt_groups TYPE string_table.
    DATA lv_group LIKE LINE OF lt_groups.
      DATA lt_parts TYPE string_table.
      DATA lt_sub TYPE ty_t_filter_multi.
      DATA lv_first_wrapped TYPE abap_bool.
      DATA lv_same_field TYPE abap_bool.
      DATA lv_part LIKE LINE OF lt_parts.
        DATA lv_tabix LIKE sy-tabix.
          DATA temp6 TYPE xsdboolean.
        DATA lt_part_filter TYPE zabaputil_cl_util_context=>ty_t_filter_multi.
        DATA ls_sub LIKE LINE OF lt_part_filter.
          FIELD-SYMBOLS <temp246> LIKE LINE OF lt_sub.
          DATA temp247 LIKE sy-tabix.
          FIELD-SYMBOLS <temp248> LIKE LINE OF ls_sub-t_range.
          DATA temp249 LIKE sy-tabix.
      DATA ls_filter TYPE ty_s_filter_multi.
        FIELD-SYMBOLS <temp250> LIKE LINE OF lt_sub.
        DATA temp251 LIKE sy-tabix.
        DATA ls_range LIKE LINE OF ls_sub-t_range.
    temp245 = val.
    
    lv_where = c_trim( temp245 ).
    IF lv_where IS INITIAL.
      RETURN.
    ENDIF.

    " an OR on the top level binds weaker than every AND: the whole clause is
    " one OR group then - splitting at the AND first read `A OR B AND C` as
    " `( A OR B ) AND C`
    IF lines( filter_sql_split_top_level( val = lv_where
                                          sep = ` OR ` ) ) > 1.
      INSERT filter_sql_get_or_group( lv_where ) INTO TABLE result.
      RETURN.
    ENDIF.

    
    lt_groups = filter_sql_split_top_level( val = lv_where
                                                  sep = ` AND ` ).

    
    LOOP AT lt_groups INTO lv_group.

      lv_group = filter_sql_strip_parens( lv_group ).

      
      lt_parts = filter_sql_split_top_level( val = lv_group
                                                   sep = ` AND ` ).

      IF lines( lt_parts ) <= 1
          OR lines( filter_sql_split_top_level( val = lv_group
                                                sep = ` OR ` ) ) > 1.
        INSERT filter_sql_get_or_group( lv_group ) INTO TABLE result.
        CONTINUE.
      ENDIF.

      " several conditions joined with AND inside one group. On one field
      " that is the shape filter_get_sql_where writes for excluding ranges:
      " `( includes ) AND exclude AND exclude`, or the excludes alone. The
      " excludes were negated on the way out and are turned back here - one
      " condition each, an OR group or an IN list cannot be one exclude.
      " Otherwise every part is a filter of its own: filters are joined with
      " AND too
      
      
      
      CLEAR lt_sub.
      lv_same_field = abap_true.
      
      LOOP AT lt_parts INTO lv_part.
        
        lv_tabix = sy-tabix.
        lv_part = c_trim( lv_part ).
        IF lv_tabix = 1.
          
          temp6 = boolc( lv_part IS NOT INITIAL AND lv_part(1) = `(` ).
          lv_first_wrapped = temp6.
        ENDIF.
        
        lt_part_filter = filter_get_multi_by_sql_where( lv_part ).
        IF lines( lt_part_filter ) <> 1.
          lv_same_field = abap_false.
        ENDIF.
        
        LOOP AT lt_part_filter INTO ls_sub.
          
          
          IF lt_sub IS NOT INITIAL.
            temp247 = sy-tabix.
            READ TABLE lt_sub INDEX 1 ASSIGNING <temp246>.
            sy-tabix = temp247.
            IF sy-subrc <> 0.
              ASSERT 1 = 0.
            ENDIF.
          ENDIF.
          IF lt_sub IS NOT INITIAL AND to_upper( <temp246>-name ) <> to_upper( ls_sub-name ).
            lv_same_field = abap_false.
          ENDIF.
          
          
          IF ( lv_tabix > 1 OR lv_first_wrapped = abap_false ) AND NOT ( lines( ls_sub-t_range ) <> 1 ).
            temp249 = sy-tabix.
            READ TABLE ls_sub-t_range INDEX 1 ASSIGNING <temp248>.
            sy-tabix = temp249.
            IF sy-subrc <> 0.
              ASSERT 1 = 0.
            ENDIF.
          ENDIF.
          IF ( lv_tabix > 1 OR lv_first_wrapped = abap_false )
              AND ( lines( ls_sub-t_range ) <> 1 OR <temp248>-sign <> `I` ).
            lv_same_field = abap_false.
          ENDIF.
          INSERT ls_sub INTO TABLE lt_sub.
        ENDLOOP.
      ENDLOOP.

      IF lv_same_field = abap_false.
        INSERT LINES OF lt_sub INTO TABLE result.
        CONTINUE.
      ENDIF.

      
      CLEAR ls_filter.
      LOOP AT lt_sub INTO ls_sub.
        lv_tabix = sy-tabix.
        
        
        temp251 = sy-tabix.
        READ TABLE lt_sub INDEX 1 ASSIGNING <temp250>.
        sy-tabix = temp251.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        ls_filter-name = <temp250>-name.
        IF lv_tabix = 1 AND lv_first_wrapped = abap_true.
          INSERT LINES OF ls_sub-t_range INTO TABLE ls_filter-t_range.
          CONTINUE.
        ENDIF.
        
        LOOP AT ls_sub-t_range INTO ls_range.
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
    DATA temp252 TYPE string.
    DATA lv_cond LIKE temp252.
    DATA lv_rest TYPE string.
    DATA lv_off TYPE i.
    DATA lv_and TYPE i.
    DATA lv_esc TYPE i.
        DATA lv_escape TYPE string.
        DATA lv_like TYPE string.
        DATA lv_pos TYPE i.
        DATA lv_plain TYPE string.
        DATA lv_segment TYPE string.
          DATA lv_char TYPE string.
            DATA lv_escaped TYPE string.
    DATA lv_in_quote TYPE abap_bool.
    DATA lv_op_len TYPE i.
    DATA lv_len TYPE i.
        DATA temp7 TYPE xsdboolean.
      DATA lv_op TYPE string.
    DATA temp253 TYPE zabaputil_cl_util_context=>ty_s_range-option.
    CLEAR range.
    CLEAR fieldname.

    
    temp252 = val.
    
    lv_cond = temp252.
    range-sign = `I`.

    
    
    
    

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
      " the AND of BETWEEN, not of another condition
      lv_and = filter_sql_find( val     = lv_rest
                                keyword = ` AND ` ).
      filter_sql_check_name( fieldname ).
      IF lv_and < 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |{ lv_cond }: BETWEEN without AND|.
      ENDIF.
      range-low  = filter_sql_get_literal( substring( val = lv_rest
                                                      len = lv_and ) ).
      range-high = filter_sql_get_literal( substring( val = lv_rest
                                                      off = lv_and + 5 ) ).
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
      filter_sql_check_name( fieldname ).
      " `'A#%%' ESCAPE '#'`: the pattern, and the character that makes the
      " next one a plain character
      lv_esc = filter_sql_find( val     = lv_rest
                                keyword = ` ESCAPE ` ).
      IF lv_esc < 0.
        range-low = filter_sql_like_to_pattern( filter_sql_get_literal( lv_rest ) ).
      ELSE.
        
        lv_escape = filter_sql_get_literal( substring( val = lv_rest
                                                             off = lv_esc + 8 ) ).
        
        lv_like = filter_sql_get_literal( substring( val = lv_rest
                                                           len = lv_esc ) ).
        
        lv_pos = 0.
        
        lv_plain = ``.
        
        lv_segment = ``.
        " the escaped characters are taken out before % and _ are mapped
        WHILE lv_pos < strlen( lv_like ).
          
          lv_char = substring( val = lv_like
                                     off = lv_pos
                                     len = 1 ).
          IF lv_escape IS NOT INITIAL AND lv_char = lv_escape AND lv_pos + 1 < strlen( lv_like ).
            " a plain character of the LIKE pattern - in CP a #, * and +
            " need the # in front to stay plain
            
            lv_escaped = substring( val = lv_like
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

    " the comparison: the first <, > or = outside of a literal, with or
    " without blanks around it (`A='1'`) - the operator only counted with a
    " blank on each side, and a condition without one was dropped
    
    
    
    lv_len = strlen( lv_cond ).
    lv_off = -1.
    lv_pos = 0.
    WHILE lv_pos < lv_len.
      lv_char = substring( val = lv_cond
                           off = lv_pos
                           len = 1 ).
      IF lv_char = `'`.
        
        temp7 = boolc( lv_in_quote = abap_false ).
        lv_in_quote = temp7.
      ELSEIF lv_in_quote = abap_false AND lv_char CA `<>=`.
        lv_off = lv_pos.
        EXIT.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    IF lv_off < 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |{ lv_cond }: no condition a range can hold (=, <>, <, <=, >, >=, BETWEEN, LIKE, IN)|.
    ENDIF.

    lv_op_len = 1.
    IF lv_off + 1 < lv_len.
      
      lv_op = substring( val = lv_cond
                               off = lv_off
                               len = 2 ).
      IF lv_op = `<>` OR lv_op = `<=` OR lv_op = `>=`.
        lv_op_len = 2.
      ENDIF.
    ENDIF.
    IF lv_op_len = 1.
      lv_op = substring( val = lv_cond
                         off = lv_off
                         len = 1 ).
    ENDIF.
    
    CASE lv_op.
      WHEN `<>`.
        temp253 = `NE`.
      WHEN `<=`.
        temp253 = `LE`.
      WHEN `>=`.
        temp253 = `GE`.
      WHEN `<`.
        temp253 = `LT`.
      WHEN `>`.
        temp253 = `GT`.
      WHEN OTHERS.
        temp253 = `EQ`.
    ENDCASE.
    range-option = temp253.
    fieldname = c_trim( substring( val = lv_cond
                                   len = lv_off ) ).
    filter_sql_check_name( fieldname ).
    range-low = filter_sql_get_literal( substring( val = lv_cond
                                                   off = lv_off + lv_op_len ) ).
  ENDMETHOD.

  METHOD filter_sql_split_top_level.

    DATA temp254 TYPE string.
    DATA lv_val LIKE temp254.
    DATA temp255 TYPE string.
    DATA lv_sep TYPE string.
    DATA lv_len TYPE i.
    DATA lv_sep_len TYPE i.
    DATA lv_depth TYPE i.
    DATA lv_start TYPE i.
    DATA lv_pos TYPE i.
    DATA lv_in_quote LIKE abap_false.
    DATA lv_in_between LIKE abap_false.
      DATA lv_char TYPE string.
      DATA lv_word_start LIKE abap_true.
        DATA temp8 TYPE xsdboolean.
        DATA temp1 TYPE string.
    DATA temp2 TYPE string.
    temp254 = val.
    
    lv_val = temp254.
    
    temp255 = sep.
    
    lv_sep = to_upper( temp255 ).
    
    lv_len = strlen( lv_val ).
    
    lv_sep_len = strlen( lv_sep ).
    
    lv_depth = 0.
    
    lv_start = 0.
    
    lv_pos = 0.
    
    lv_in_quote = abap_false.
    
    lv_in_between = abap_false.

    IF lv_val IS INITIAL.
      RETURN.
    ENDIF.

    IF lv_sep_len = 0.
      INSERT lv_val INTO TABLE result.
      RETURN.
    ENDIF.

    WHILE lv_pos < lv_len.

      
      lv_char = lv_val+lv_pos(1).

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
      " a word of its own - `ZBETWEEN = 'X' AND F = 'Y'` is no BETWEEN
      
      lv_word_start = abap_true.
      IF ( lv_char = `B` OR lv_char = `b` ) AND lv_pos > 0.
        
        temp8 = boolc( substring( val = lv_val off = lv_pos - 1 len = 1 ) CA ` (` ).
        lv_word_start = temp8.
      ENDIF.
      IF lv_depth = 0 AND lv_in_between = abap_false
         AND ( lv_char = `B` OR lv_char = `b` )
         AND lv_word_start = abap_true
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
        
        temp1 = substring( val = lv_val off = lv_start len = lv_pos - lv_start ).
        INSERT temp1 INTO TABLE result.
        lv_pos = lv_pos + lv_sep_len.
        lv_start = lv_pos.
        CONTINUE.
      ENDIF.

      lv_pos = lv_pos + 1.

    ENDWHILE.

    
    temp2 = substring( val = lv_val off = lv_start ).
    INSERT temp2 INTO TABLE result.

  ENDMETHOD.

  METHOD filter_sql_get_literal.

    DATA lv_val TYPE string.
    DATA lv_len TYPE i.
    DATA lv_pos TYPE i.
    lv_val = c_trim( val ).
    
    lv_len = strlen( lv_val ).
    
    lv_pos = 1.

    IF lv_len = 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `A condition without a value`.
    ENDIF.

    IF lv_val(1) <> `'`.
      " a number or a host variable - one word. More words are another
      " clause or a condition that a range cannot hold
      IF lv_val CA ` '()`.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |{ lv_val }: no single value|.
      ENDIF.
      result = lv_val.
      RETURN.
    ENDIF.

    " the literal ends at the first quote that is not doubled - and that has
    " to be the last character: `'1' ORDER BY b` is no value
    WHILE lv_pos < lv_len.
      IF substring( val = lv_val
                    off = lv_pos
                    len = 1 ) = `'`.
        IF lv_pos + 1 < lv_len AND substring( val = lv_val
                                               off = lv_pos + 1
                                               len = 1 ) = `'`.
          result = result && `'`.
          lv_pos = lv_pos + 2.
          CONTINUE.
        ENDIF.
        IF lv_pos <> lv_len - 1.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |{ lv_val }: text behind the literal|.
        ENDIF.
        RETURN.
      ENDIF.
      result = result && substring( val = lv_val
                                    off = lv_pos
                                    len = 1 ).
      lv_pos = lv_pos + 1.
    ENDWHILE.

    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = |{ lv_val }: the literal is not closed|.

  ENDMETHOD.

  METHOD filter_sql_check_name.

    " a character set instead of a regex: PCRE is not on 7.02, POSIX not on
    " ABAP Cloud - the same set filter_get_sql_where accepts
    IF val IS INITIAL
        OR val CN `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_/`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |{ val }: no field name - NOT in front, an expression or an alias is no filter|.
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
    DATA lv_type_kind TYPE string.
    DATA temp9 TYPE xsdboolean.

    IF rtti_check_clike( val ) = abap_true.
      result = abap_true.
      RETURN.
    ENDIF.

    
    lv_type_kind = rtti_get_type_kind( val ).
    
    temp9 = boolc( lv_type_kind = cl_abap_datadescr=>typekind_hex OR rtti_check_typekind_number( lv_type_kind ) = abap_true ).
    result = temp9.

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
        DATA lt_attri TYPE abap_attrdescr_tab.
    DATA temp256 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp256.
      DATA temp257 TYPE zabaputil_cl_util_context=>ty_s_name_value.

    IF val IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        
        lt_attri = rtti_get_t_attri_by_oref( val ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    
    LOOP AT lt_attri REFERENCE INTO lr_attri "#EC CI_SORTSEQ
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

      
      CLEAR temp257.
      temp257-n = lv_name.
      temp257-v = c_trim( |{ <comp> }| ).
      INSERT temp257 INTO TABLE result.
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
    DATA lv_len TYPE i.
    DATA lv_off TYPE i.
    DATA temp10 TYPE xsdboolean.
    lv_val = val.
    
    lv_len = strlen( lv_val ).
    
    lv_off = 0.
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

    
    temp10 = boolc( lv_depth = 0 AND lv_quote IS INITIAL ).
    result = temp10.

  ENDMETHOD.

  METHOD rtti_create_tab_by_names.

    DATA lt_comp TYPE abap_component_tab.

    DATA lv_name LIKE LINE OF val.
      DATA temp258 TYPE abap_componentdescr.
    LOOP AT val INTO lv_name.
      " a string per column - a fixed length would cut every longer value
      
      CLEAR temp258.
      temp258-name = lv_name.
      temp258-type = cl_abap_elemdescr=>get_string( ).
      INSERT temp258 INTO TABLE lt_comp.
    ENDLOOP.
    result = rtti_create_tab_by_comps( lt_comp ).

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
    DATA temp11 TYPE xsdboolean.

    IF val IS INITIAL.
      RETURN.
    ENDIF.

    lv_name = val.
    " the non-functional form: an unknown name comes back as sy-subrc,
    " where the functional one ends in a classic exception
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = lv_name
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    
    temp11 = boolc( sy-subrc = 0 ).
    result = temp11.

  ENDMETHOD.

  METHOD rtti_create_tab_by_name.

    " type_not_found is a classic exception: called functionally, an unknown
    " name ends in a short dump no CATCH stops - so it is handled here and
    " the caller gets an unbound reference instead
    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA temp259 TYPE REF TO cl_abap_datadescr.
    DATA data_desc LIKE temp259.
    DATA gr_dyntable_typ TYPE REF TO cl_abap_tabledescr.
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
    
    temp259 ?= lo_type.
    
    data_desc = temp259.
    
    gr_dyntable_typ = cl_abap_tabledescr=>create( data_desc ).
    CREATE DATA result TYPE HANDLE gr_dyntable_typ.

  ENDMETHOD.

  METHOD msg_get.

    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
    DATA temp260 TYPE zabaputil_cl_util_context=>ty_s_msg.
    DATA temp261 TYPE zabaputil_cl_util_context=>ty_s_msg.
    lt_msg = msg_get_t( val  = val
                              val2 = val2 ).
    
    CLEAR temp260.
    
    READ TABLE lt_msg INTO temp261 INDEX 1.
    IF sy-subrc = 0.
      temp260 = temp261.
    ENDIF.
    result = temp260.

  ENDMETHOD.

  METHOD msg_get_collect.

    DATA temp262 TYPE string_table.
    DATA temp660 TYPE zabaputil_cl_util_context=>ty_t_msg.
    FIELD-SYMBOLS <r> LIKE LINE OF temp660.
      DATA temp661 LIKE LINE OF temp262.
    CLEAR temp262.
    
    temp660 = msg_get_t( val = val val2 = val2 ).
    
    LOOP AT temp660 ASSIGNING <r>.
      
      temp661 = |- { <r>-text }|.
      INSERT temp661 INTO TABLE temp262.
    ENDLOOP.
    result = concat_lines_of(
      table = temp262
      sep   = cv_char_util_newline ).

  ENDMETHOD.

  METHOD rtti_get_data_element_text_l.

    result = rtti_get_data_element_texts( val )-long.

  ENDMETHOD.

  METHOD rtti_get_ddic_type_name.

    DATA temp264 TYPE REF TO cl_abap_elemdescr.
    temp264 ?= type.
    result = substring_after( val = temp264->absolute_name
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
    DATA lo_table TYPE REF TO cl_abap_tabledescr.
        DATA temp265 TYPE REF TO cl_abap_tabledescr.
        DATA lx_cast TYPE REF TO cx_root.
    DATA lo_line TYPE REF TO cl_abap_datadescr.
        DATA temp266 TYPE REF TO cl_abap_structdescr.
        DATA lo_struct LIKE temp266.
        DATA temp267 TYPE REF TO cl_abap_elemdescr.
        DATA lo_elem LIKE temp267.
        DATA temp268 TYPE abap_componentdescr.
    DATA temp269 LIKE sy-subrc.
      DATA lo_type_bool TYPE REF TO cl_abap_typedescr.
      DATA temp270 TYPE abap_componentdescr.
      DATA temp662 TYPE REF TO cl_abap_datadescr.
    DATA lo_line_type TYPE REF TO cl_abap_structdescr.

    " an unbound reference left <tab> unassigned and describe_by_data dumped
    " with GETWA_NOT_ASSIGNED; a reference to a scalar failed the cast below.
    " Both are the caller's mistake and answer as the framework's exception
    IF ir_tab IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `RTTI_CREATE_SEL_TAB_TYPE: no table reference supplied`.
    ENDIF.
    ASSIGN ir_tab->* TO <tab>.

    
    TRY.
        
        temp265 ?= cl_abap_typedescr=>describe_by_data( <tab> ).
        lo_table = temp265.
        
      CATCH cx_root INTO lx_cast.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_cast.
    ENDTRY.

    " decide the line kind by RTTI instead of by a failing cast: the CATCH
    " branch cast to cl_abap_elemdescr sat outside any TRY, so a table whose
    " line is itself a table or a reference raised CX_SY_MOVE_CAST_ERROR
    
    lo_line = lo_table->get_table_line_type( ).
    CASE lo_line->kind.
      WHEN cv_typedescr_kind_struct.
        
        temp266 ?= lo_line.
        
        lo_struct = temp266.
        lt_comp = lo_struct->get_components( ).
      WHEN cv_typedescr_kind_elem.
        result-check_table_line = abap_true.
        
        temp267 ?= lo_line.
        
        lo_elem = temp267.
        
        CLEAR temp268.
        temp268-name = `TAB_LINE`.
        temp268-type = lo_elem.
        INSERT temp268 INTO TABLE lt_comp.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `RTTI_CREATE_SEL_TAB_TYPE: a table of tables or references has no selectable line`.
    ENDCASE.

    
    READ TABLE lt_comp WITH KEY name = sel_field_name TRANSPORTING NO FIELDS.
    temp269 = sy-subrc.
    IF add_sel_field = abap_true
        AND NOT temp269 = 0. "#EC CI_SORTSEQ
      
      lo_type_bool = cl_abap_typedescr=>describe_by_name( `ABAP_BOOL` ).
      
      CLEAR temp270.
      temp270-name = sel_field_name.
      
      temp662 ?= lo_type_bool.
      temp270-type = temp662.
      INSERT temp270 INTO TABLE lt_comp.
    ENDIF.

    
    lo_line_type = cl_abap_structdescr=>create( lt_comp ).
    result-tabledescr = cl_abap_tabledescr=>create( lo_line_type ).

  ENDMETHOD.

  METHOD msg_get_by_msg.

    DATA temp271 TYPE ty_s_msg.
    DATA ls_msg LIKE temp271.
    CLEAR temp271.
    temp271-id = id.
    temp271-no = no.
    temp271-v1 = v1.
    temp271-v2 = v2.
    temp271-v3 = v3.
    temp271-v4 = v4.
    
    ls_msg = temp271.
    result = msg_get( ls_msg ).

  ENDMETHOD.

  METHOD c_contains.
    " Note: ABAP CS operator is CASE-INSENSITIVE.
    " JS transpilation must use a case-insensitive comparison
    " (e.g. val.toLowerCase().includes(sub.toLowerCase())).
    DATA temp272 TYPE string.
    DATA temp12 TYPE xsdboolean.
    temp272 = val.
    
    temp12 = boolc( temp272 CS sub ).
    result = temp12.

  ENDMETHOD.

  METHOD c_starts_with.

    DATA temp273 TYPE string.
    DATA lv_val LIKE temp273.
    DATA temp274 TYPE string.
    DATA lv_prefix LIKE temp274.
    DATA lv_len TYPE i.
    DATA temp13 TYPE xsdboolean.
    temp273 = val.
    
    lv_val = temp273.
    
    temp274 = prefix.
    
    lv_prefix = temp274.
    
    lv_len = strlen( lv_prefix ).

    IF strlen( lv_val ) < lv_len.
      result = abap_false.
      RETURN.
    ENDIF.

    
    temp13 = boolc( lv_val(lv_len) = lv_prefix ).
    result = temp13.

  ENDMETHOD.

  METHOD c_ends_with.

    DATA temp275 TYPE string.
    DATA lv_val LIKE temp275.
    DATA temp276 TYPE string.
    DATA lv_suffix LIKE temp276.
    DATA lv_len_suffix TYPE i.
    DATA lv_len_val TYPE i.
    DATA lv_off TYPE i.
    DATA temp14 TYPE xsdboolean.
    temp275 = val.
    
    lv_val = temp275.
    
    temp276 = suffix.
    
    lv_suffix = temp276.
    
    lv_len_suffix = strlen( lv_suffix ).
    
    lv_len_val = strlen( lv_val ).

    IF lv_len_val < lv_len_suffix.
      result = abap_false.
      RETURN.
    ENDIF.

    
    lv_off = lv_len_val - lv_len_suffix.
    
    temp14 = boolc( lv_val+lv_off(lv_len_suffix) = lv_suffix ).
    result = temp14.

  ENDMETHOD.

  METHOD c_split.

    SPLIT val AT sep INTO TABLE result.

  ENDMETHOD.

  METHOD c_join.

    DATA lv_line LIKE LINE OF tab.
    LOOP AT tab INTO lv_line.
      IF sy-tabix > 1.
        result = result && sep.
      ENDIF.
      result = result && lv_line.
    ENDLOOP.

  ENDMETHOD.

  METHOD rtti_check_table.

    DATA temp15 TYPE xsdboolean.
    temp15 = boolc( rtti_get_type_kind( val ) = cl_abap_typedescr=>typekind_table ).
    result = temp15.

  ENDMETHOD.

  METHOD rtti_check_structure.
        DATA lo_type TYPE REF TO cl_abap_typedescr.
        DATA temp16 TYPE xsdboolean.

    TRY.
        
        lo_type = cl_abap_typedescr=>describe_by_data( val ).
        
        temp16 = boolc( lo_type->kind = cl_abap_typedescr=>kind_struct ).
        result = temp16.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_check_numeric.

    DATA lv_type_kind TYPE abap_typekind.
    DATA temp17 TYPE xsdboolean.
    lv_type_kind = cl_abap_datadescr=>get_data_type_kind( val ).
    " numeric text (N) and the generic decfloat kind count as well
    
    temp17 = boolc( lv_type_kind = cl_abap_typedescr=>typekind_decfloat OR lv_type_kind = cl_abap_typedescr=>typekind_num OR rtti_check_typekind_number( lv_type_kind ) = abap_true ).
    result = temp17.

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

    DATA ls_sy TYPE zabaputil_cl_util_context=>ty_syst.
    ls_sy = context_get_sy( ).
    CONVERT DATE date TIME time INTO TIME STAMP result TIME ZONE ls_sy-zonlo.

  ENDMETHOD.

  METHOD time_diff_seconds.

    DATA lv_diff TYPE tzntstmpl.
    lv_diff = cl_abap_tstmp=>subtract( tstmp1 = time_to
                                              tstmp2 = time_from ).
    result = lv_diff.

  ENDMETHOD.

  METHOD conv_string_to_date.

    DATA lt_val_groups TYPE string_table.
    DATA lt_fmt_groups TYPE string_table.
    DATA lv_group      TYPE string.
    DATA lv_index      TYPE i.

    DATA temp277 TYPE string.
    DATA lv_val LIKE temp277.
    DATA temp278 TYPE string.
    DATA lv_fmt LIKE temp278.
    DATA lv_clean TYPE string.
    DATA lv_i TYPE i.
      DATA lv_c TYPE string.
    DATA lv_fmt_clean TYPE string.
    DATA lv_year TYPE string.
    DATA lv_month TYPE string.
    DATA lv_day TYPE string.
      DATA temp279 LIKE LINE OF lt_fmt_groups.
      DATA lr_fmt LIKE REF TO temp279.
        FIELD-SYMBOLS <temp280> LIKE LINE OF lt_val_groups.
        DATA temp281 LIKE sy-tabix.
        DATA lv_len TYPE i.
    DATA lv_pos TYPE i.
    temp277 = val.
    
    lv_val = temp277.
    
    temp278 = format.
    
    lv_fmt = temp278.

    " the digits of the value, all of them and in their runs
    
    lv_clean = ``.
    
    lv_i = 0.
    WHILE lv_i < strlen( lv_val ).
      
      lv_c = lv_val+lv_i(1).
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
    
    lv_fmt_clean = ``.
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

    
    lv_year  = ``.
    
    lv_month = ``.
    
    lv_day   = ``.

    " a value written with separators is read by its parts, in the order
    " the format names them: `5.3.2024` is the 5th of March, and a time
    " behind the date (`5.3.2024 10:30`) is left out. Read digit by digit,
    " the first value came out as an initial date and the second as the
    " year 2410. A day or a month may have one digit, a year its full
    " length; a part longer than the format allows is no date
    IF lines( lt_val_groups ) > 1 AND lines( lt_val_groups ) >= lines( lt_fmt_groups ).
      
      
      LOOP AT lt_fmt_groups REFERENCE INTO lr_fmt.
        lv_index = lv_index + 1.
        
        
        temp281 = sy-tabix.
        READ TABLE lt_val_groups INDEX lv_index ASSIGNING <temp280>.
        sy-tabix = temp281.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_group = <temp280>.
        
        lv_len = strlen( lr_fmt->* ).
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

    
    lv_pos = 0.
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

    DATA temp282 TYPE string.
    DATA lv_fmt LIKE temp282.
    DATA temp283 TYPE string.
    DATA lv_date LIKE temp283.
    DATA lv_year TYPE string.
    DATA lv_month TYPE string.
    DATA lv_day TYPE string.
    temp282 = format.
    
    lv_fmt = temp282.
    
    temp283 = val.
    
    lv_date = temp283.

    
    lv_year  = lv_date(4).
    
    lv_month = lv_date+4(2).
    
    lv_day   = lv_date+6(2).

    result = lv_fmt.
    result = replace( val = result sub = `YYYY` with = lv_year ).
    result = replace( val = result sub = `MM` with = lv_month ).
    result = replace( val = result sub = `DD` with = lv_day ).

  ENDMETHOD.

  METHOD ui5_msg_box_format.

    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
    DATA lv_lines TYPE i.
    DATA lv_type TYPE string.
    DATA lv_rank TYPE i.
    DATA temp284 LIKE LINE OF lt_msg.
    DATA lr_type LIKE REF TO temp284.
      DATA lv_type_row TYPE string.
      DATA temp285 TYPE i.
      DATA lv_rank_row LIKE temp285.
      FIELD-SYMBOLS <temp286> LIKE LINE OF lt_msg.
      DATA temp287 LIKE sy-tabix.
    DATA lt_detail_items TYPE string_table.
    DATA temp288 LIKE LINE OF lt_msg.
    DATA lr_msg LIKE REF TO temp288.
      DATA temp289 LIKE LINE OF lt_detail_items.
    lt_msg = msg_get_t( val ).

    " a structure that carries none of the message components maps to an
    " entry with no text at all - that is not a message, it is data, and it
    " used to reach the box as a popup with a blank line in it. Dropping it
    " here is what lets `skip` mean "nothing in here is a message" and lets
    " the caller fall back to ui5_data_box_format( )
    DELETE lt_msg WHERE text IS INITIAL.

    
    lv_lines = lines( lt_msg ).

    IF lv_lines = 0.
      result-skip = abap_true.
      RETURN.
    ENDIF.

    " the box takes its type/title from the MOST SEVERE message, also when
    " several are collapsed into one box below: a warning followed by an
    " error is an error box (it took the first message's type until
    " 2026-10, which opened [W, E] as a warning). Error over warning over
    " success and information, which rank the same - the first of them wins
    
    lv_type = ``.
    
    lv_rank = -1.
    
    
    LOOP AT lt_msg REFERENCE INTO lr_type.
      
      lv_type_row = ui5_get_msg_type( lr_type->type ).
      
      IF lv_type_row = cs_ui5_msg_type-e.
        temp285 = 2.
      ELSEIF lv_type_row = cs_ui5_msg_type-w.
        temp285 = 1.
      ELSE.
        temp285 = 0.
      ENDIF.
      
      lv_rank_row = temp285.
      IF lv_rank_row > lv_rank.
        lv_rank = lv_rank_row.
        lv_type = lv_type_row.
      ENDIF.
    ENDLOOP.
    result-title = lv_type.
    result-type  = to_lower( lv_type ).

    IF lv_lines = 1.
      
      
      temp287 = sy-tabix.
      READ TABLE lt_msg INDEX 1 ASSIGNING <temp286>.
      sy-tabix = temp287.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result-text = <temp286>-text.
      RETURN.
    ENDIF.

    " several messages: a counting headline plus every text as a bullet. No
    " blank around it - the box shows the text as written, and the template
    " used to carry one on either side
    result-text = |{ lv_lines } Messages found:|.
    
    " the texts are data inside markup, escaped like every sibling renderer
    " escapes its values: unescaped, `Enter a value for <MATNR>` lost its
    " token to the frontend sanitizer, which drops an unknown element
    
    
    LOOP AT lt_msg REFERENCE INTO lr_msg.
      
      temp289 = |<li>{ c_escape_html( lr_msg->text ) }</li>|.
      INSERT temp289 INTO TABLE lt_detail_items.
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
    FIELD-SYMBOLS <row> TYPE ANY.
        DATA temp290 LIKE LINE OF lt_item.
      DATA temp291 LIKE LINE OF lt_item.

    " IS ASSIGNED, not sy-subrc - the unassign_data rule: on some releases a
    " successful ASSIGN leaves sy-subrc as it was, and the caller chain
    " (box_resolve -> ui5_msg_box_format's DELETE ... WHERE with no hit ->
    " here) arrives with a stale 4, which rendered "Table with N entries"
    " over an empty body. The symbol is declared fresh and assigned once
    ASSIGN val TO <tab>.
    IF <tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    
    LOOP AT <tab> ASSIGNING <row>.
      lv_no = lv_no + 1.
      IF lv_no > cv_data_max_rows.
        
        temp290 = |<li><em>... { lines( <tab> ) - cv_data_max_rows } more entries</em></li>|.
        INSERT temp290
               INTO TABLE lt_item.
        EXIT.
      ENDIF.
      
      temp291 = |<li>{ data_render( val = <row> depth = depth + 1 ) }</li>|.
      INSERT temp291 INTO TABLE lt_item.
    ENDLOOP.

    result = html_get_list( items   = lt_item
                            ordered = abap_true ).

  ENDMETHOD.

  METHOD data_render_struc.

    DATA lt_item TYPE string_table.
    " declared, not CONV string( ) - see error_get_attributes
    DATA lv_name TYPE string.

    FIELD-SYMBOLS <comp> TYPE any.
        DATA lt_attri TYPE abap_component_tab.
    DATA temp292 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp292.

    TRY.
        
        lt_attri = rtti_get_t_attri_by_any( val ).
      CATCH cx_root.
        " a structure RTTI cannot describe still has a value - the caller
        " keeps the headline, the details stay empty
        RETURN.
    ENDTRY.

    
    
    LOOP AT lt_attri REFERENCE INTO lr_attri.
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
        DATA lt_attri TYPE abap_attrdescr_tab.
    DATA temp293 LIKE LINE OF lt_attri.
    DATA lr_attri LIKE REF TO temp293.

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
        
        lt_attri = rtti_get_t_attri_by_oref( lo_obj ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    " the public state is what an object can show of itself; a constant is
    " the type's, not this instance's, and a class attribute is nobody's
    
    
    LOOP AT lt_attri REFERENCE INTO lr_attri                  "#EC CI_SORTSEQ
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

    DATA lv_val TYPE string.
    lv_val = data_render( val   = val
                                depth = depth + 1 ).

    result = |<li><strong>{ c_escape_html( name ) }</strong>: { lv_val }</li>|.

  ENDMETHOD.

  METHOD data_get_headline.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
        DATA lv_lines TYPE i.
            DATA lt_attri TYPE abap_component_tab.
        DATA lo_obj TYPE REF TO object.

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
        
        lv_lines = lines( <tab> ).
        IF lv_lines = 1.
          result = `Table with 1 entry`.
        ELSE.
          result = |Table with { lv_lines } entries|.
        ENDIF.

      WHEN cl_abap_datadescr=>typekind_struct1 OR cl_abap_datadescr=>typekind_struct2.
        TRY.
            
            lt_attri = rtti_get_t_attri_by_any( val ).
            result = |Structure with { lines( lt_attri ) } fields|.
          CATCH cx_root.
            result = `Structure`.
        ENDTRY.

      WHEN cl_abap_datadescr=>typekind_oref.
        
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
        DATA temp294 TYPE REF TO cx_root.

    TRY.
        
        temp294 ?= val.
        result = temp294->get_text( ).
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
    DATA temp18 TYPE xsdboolean.
    temp18 = boolc( val CS `</` OR val CS `<br` OR val CS `<hr` OR val CS `<img` ).
    result = temp18.

  ENDMETHOD.

  METHOD html_get_plain.
    DATA lv_open TYPE i.

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
    
    lv_open = find( val = result
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
        DATA temp295 TYPE REF TO cl_abap_tabledescr.
        DATA temp19 TYPE xsdboolean.

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
        
        temp295 ?= lo_type.
        
        temp19 = boolc( temp295->table_kind = cl_abap_tabledescr=>tablekind_std ).
        result = temp19.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_flat_components.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
          DATA temp296 TYPE REF TO cl_abap_tabledescr.
          DATA lo_table LIKE temp296.
        DATA temp297 TYPE REF TO cl_abap_structdescr.
        DATA lt_comp TYPE abap_component_view_tab.
        DATA temp298 LIKE LINE OF lt_comp.
        DATA lr_comp LIKE REF TO temp298.
          DATA temp299 TYPE ty_s_flat_comp.
          DATA ls_flat LIKE temp299.
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
          
          temp296 ?= lo_type.
          
          lo_table = temp296.
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
        
        temp297 ?= lo_type.
        
        lt_comp = temp297->get_included_view( ).
        
        
        LOOP AT lt_comp REFERENCE INTO lr_comp.
          
          CLEAR temp299.
          temp299-name = lr_comp->name.
          
          ls_flat = temp299.
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
    DATA lo_type LIKE type.
      DATA temp300 TYPE REF TO cl_abap_tabledescr.
    DATA temp301 TYPE REF TO cl_abap_structdescr.
    DATA lt_comp TYPE abap_component_view_tab.
    DATA temp302 LIKE LINE OF lt_comp.
    DATA lr_comp LIKE REF TO temp302.

    " no type nests deeper than this without a reference, which is not
    " followed - the bound is a guard, not a limit an app meets
    IF depth > 32.
      RETURN.
    ENDIF.
    
    lo_type = type.
    IF lo_type->kind = cl_abap_typedescr=>kind_table.
      
      temp300 ?= lo_type.
      lo_type = temp300->get_table_line_type( ).
    ENDIF.
    IF lo_type->kind <> cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.
    " the included view, as rtti_get_flat_components reads it: the name of
    " an include's component is the one the structure addresses it by
    
    temp301 ?= lo_type.
    
    lt_comp = temp301->get_included_view( ).
    
    
    LOOP AT lt_comp REFERENCE INTO lr_comp.
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
        DATA temp303 TYPE REF TO if_serializable_object.
        DATA lo_dummy LIKE temp303.

    IF val IS NOT BOUND.
      result = abap_true.
      RETURN.
    ENDIF.
    TRY.
        
        temp303 ?= val.
        
        lo_dummy = temp303.
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD app_get_url.

    DATA lt_param TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp304 TYPE zabaputil_cl_util_context=>ty_s_name_value.
    DATA temp305 TYPE string.
    DATA lv_hash LIKE temp305.
      DATA lv_content LIKE lv_hash.
        DATA lv_off TYPE i.
    lt_param = url_param_get_tab( search ).
    DELETE lt_param WHERE n = `app_start`.
    
    CLEAR temp304.
    temp304-n = `app_start`.
    temp304-v = to_lower( classname ).
    INSERT temp304 INTO TABLE lt_param.

    " keep only the launchpad shell part of the hash: the app-owned part
    " (leading `/` standalone, or everything after `&/` inside the FLP)
    " carries THIS app's route/app-state, which the backend prefers over
    " app_start - appending it verbatim would re-open the current app
    " instead of the requested one
    
    temp305 = hash.
    
    lv_hash = temp305.
    IF lv_hash IS NOT INITIAL.
      
      lv_content = lv_hash.
      IF lv_content(1) = `#`.
        lv_content = substring( val = lv_content
                                off = 1 ).
      ENDIF.
      IF lv_content IS INITIAL OR lv_content(1) = `/`.
        " pure app hash (route or app-state) - drop it entirely
        lv_hash = ``.
      ELSE.
        " inside the FLP keep the shell part, cut the app part after `&/`
        
        lv_off = find( val = lv_content
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
    DATA lv_missing TYPE i.
    DATA temp306 TYPE string.
    DATA temp663 TYPE string.
    DATA lv_pad LIKE temp663.
    result = val.
    
    lv_missing = len - strlen( result ).
    IF lv_missing <= 0.
      RETURN.
    ENDIF.
    " pad is TYPE c - space value would be trimmed by && (same as c_pad_right fix)
    
    temp306 = pad.
    
    IF pad IS INITIAL.
      temp663 = ` `.
    ELSE.
      temp663 = temp306.
    ENDIF.
    
    lv_pad = temp663.
    " exactly the missing characters, also for a pad of several characters
    result = substring( val = repeat( val = lv_pad
                                      occ = lv_missing )
                        len = lv_missing ) && result.
  ENDMETHOD.

  METHOD c_pad_right.
    DATA lv_missing TYPE i.
    DATA temp307 TYPE string.
    DATA temp664 TYPE string.
    DATA lv_pad LIKE temp664.
    result = val.
    
    lv_missing = len - strlen( result ).
    IF lv_missing <= 0.
      RETURN.
    ENDIF.
    " pad is TYPE c - a space value IS INITIAL in ABAP and CONV string trims it.
    " Preserve the space explicitly via COND.
    
    temp307 = pad.
    
    IF pad IS INITIAL.
      temp664 = ` `.
    ELSE.
      temp664 = temp307.
    ENDIF.
    
    lv_pad = temp664.
    " exactly the missing characters, also for a pad of several characters
    result = result && substring( val = repeat( val = lv_pad
                                                occ = lv_missing )
                                  len = lv_missing ).
  ENDMETHOD.

  METHOD c_truncate.
    DATA lv_ellipsis_len TYPE i.

    result = val.
    IF strlen( result ) <= max.
      RETURN.
    ENDIF.
    
    lv_ellipsis_len = strlen( ellipsis ).
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

    DATA temp308 TYPE string.
    DATA lv_val LIKE temp308.
    DATA lv_strlen TYPE i.
    DATA lv_off LIKE off.
    DATA lv_len LIKE len.
    temp308 = val.
    
    lv_val = temp308.
    
    lv_strlen = strlen( lv_val ).

    
    lv_off = off.
    IF lv_off < 0.
      lv_off = 0.
    ENDIF.
    IF lv_off >= lv_strlen.
      result = ``.
      RETURN.
    ENDIF.

    
    lv_len = len.
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

    DATA temp309 TYPE string.
    DATA lv_val LIKE temp309.
    DATA temp20 TYPE xsdboolean.
    temp309 = val.
    
    lv_val = temp309.
    
    temp20 = boolc( c_trim( lv_val ) IS INITIAL ).
    result = temp20.

  ENDMETHOD.

  " ========== Number Formatting ==========

  METHOD conv_number_to_string.

    DATA lv_str TYPE string.
      DATA temp310 TYPE decfloat34.
      DATA lv_fmt TYPE string.
      DATA lv_dot_pos TYPE i.
        DATA lv_int_part TYPE string.
        DATA lv_dec_part TYPE string.
        DATA temp311 TYPE string.
      DATA lv_dot TYPE i.
      DATA temp312 TYPE string.
      DATA lv_integer LIKE temp312.
      DATA temp313 TYPE string.
      DATA lv_decimal LIKE temp313.
      DATA lv_negative TYPE string.
      DATA lv_result TYPE string.
      DATA lv_count TYPE i.
      DATA lv_i TYPE i.

    IF decimals >= 0.
      " rounded to the decimals asked for - cutting the digits off turned
      " 1.999 into 1.99
      
      temp310 = val.
      
      lv_fmt = |{ num_round( val      = temp310
                                   decimals = decimals ) }|.
      
      lv_dot_pos = find( val = lv_fmt sub = `.` ).
      IF lv_dot_pos < 0.
        lv_str = lv_fmt.
        IF decimals > 0.
          lv_str = lv_str && `.` && repeat( val = `0` occ = decimals ).
        ENDIF.
      ELSE.
        
        lv_int_part = lv_fmt(lv_dot_pos).
        
        lv_dec_part = substring( val = lv_fmt off = lv_dot_pos + 1 ).
        IF strlen( lv_dec_part ) > decimals.
          lv_dec_part = lv_dec_part(decimals).
        ELSEIF strlen( lv_dec_part ) < decimals.
          lv_dec_part = lv_dec_part && repeat( val = `0` occ = decimals - strlen( lv_dec_part ) ).
        ENDIF.
        
        IF decimals > 0.
          temp311 = lv_int_part && `.` && lv_dec_part.
        ELSE.
          temp311 = lv_int_part.
        ENDIF.
        lv_str = temp311.
      ENDIF.
    ELSE.
      lv_str = |{ val }|.
    ENDIF.

    IF sep_thousands IS NOT INITIAL.
      
      lv_dot = find( val = lv_str sub = `.` ).
      
      IF lv_dot >= 0.
        temp312 = lv_str(lv_dot).
      ELSE.
        temp312 = lv_str.
      ENDIF.
      
      lv_integer = temp312.
      
      IF lv_dot >= 0.
        temp313 = substring( val = lv_str off = lv_dot ).
      ELSE.
        CLEAR temp313.
      ENDIF.
      
      lv_decimal = temp313.
      
      lv_negative = ``.
      IF strlen( lv_integer ) > 0 AND lv_integer(1) = `-`.
        lv_negative = `-`.
        lv_integer = substring( val = lv_integer off = 1 ).
      ENDIF.
      
      lv_result = ``.
      
      lv_count = 0.
      
      lv_i = strlen( lv_integer ) - 1.
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

    DATA temp314 TYPE string.
    DATA lv_val TYPE string.
    DATA lv_dot_count TYPE i.
    DATA lv_comma_count TYPE i.
    DATA lv_decimal TYPE string.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.
    DATA lv_last TYPE i.
    DATA lv_clean TYPE string.
      DATA lv_c TYPE string.
    DATA lv_point_count TYPE i.
    temp314 = val.
    
    lv_val = c_trim( temp314 ).

    " Which separator is the decimal one:
    " - both a dot and a comma occur: the one that occurs LAST
    "   (`1.234,56` and `1,234.56` are both 1234.56)
    " - only one kind, more than once: thousands separators, no decimals
    "   (`1.234.567`, `1,000,000`)
    " - only one kind, once: the decimal separator (`3,14`, `3.14`). So
    "   `1,000` is 1 (European convention) - a single separator cannot be
    "   told apart from a thousands separator, the decimal reading wins
    " Every other separator is a thousands separator and is skipped.
    
    
    
    
    lv_i = 0.
    
    lv_len = strlen( lv_val ).
    
    lv_last = lv_len - 1.
    WHILE lv_i < lv_len.
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
    IF ( lv_dot_count = 0 OR lv_comma_count = 0 ) AND ( lv_dot_count > 1 OR lv_comma_count > 1 ).
      CLEAR lv_decimal.
    ENDIF.

    " Normalize: keep only digits, minus, and the decimal separator as `.`
    
    lv_clean = ``.
    lv_i = 0.
    WHILE lv_i < lv_len.
      
      lv_c = lv_val+lv_i(1).
      IF lv_c >= `0` AND lv_c <= `9`.
        lv_clean = lv_clean && lv_c.
      ELSEIF lv_c = `-` AND lv_i = 0.
        lv_clean = lv_clean && lv_c.
      ELSEIF lv_c = `-` AND lv_i = lv_last AND lv_val(1) <> `-`.
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
    
    lv_point_count = count( val = lv_clean
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
    TYPES temp4 TYPE STANDARD TABLE OF ty_s_sort_key WITH DEFAULT KEY.
DATA lt_key TYPE temp4.
    DATA lv_numeric TYPE abap_bool.

    DATA lv_field TYPE string.
    FIELD-SYMBOLS <row> TYPE ANY.
      DATA lv_tabix LIKE sy-tabix.
      FIELD-SYMBOLS <val> TYPE any.
      FIELD-SYMBOLS <key> TYPE ty_s_sort_key.
    DATA lr_copy TYPE REF TO data.
    FIELD-SYMBOLS <tab_copy> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <src> TYPE any.
    lv_field = to_upper( fieldname ).

    
    LOOP AT tab ASSIGNING <row>.
      
      lv_tabix = sy-tabix.
      
      ASSIGN COMPONENT lv_field OF STRUCTURE <row> TO <val>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      IF lv_tabix = 1.
        lv_numeric = rtti_check_numeric( <val> ).
      ENDIF.
      
      INSERT INITIAL LINE INTO TABLE lt_key ASSIGNING <key>.
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

    
    CREATE DATA lr_copy LIKE tab.
    
    ASSIGN lr_copy->* TO <tab_copy>.
    <tab_copy> = tab.

    CLEAR tab.
    LOOP AT lt_key ASSIGNING <key>.
      
      READ TABLE <tab_copy> INDEX <key>-idx ASSIGNING <src>.
      IF sy-subrc = 0.
        INSERT <src> INTO TABLE tab.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_slice.

    " Create a copy of the source table (same type) and return the slice.
    DATA temp315 TYPE REF TO cl_abap_tabledescr.
    DATA lo_tabledescr LIKE temp315.
    FIELD-SYMBOLS <result> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row> TYPE any.
    DATA lv_lines TYPE i.
    DATA temp316 TYPE i.
    DATA lv_to LIKE temp316.
    DATA temp317 TYPE i.
    DATA lv_from LIKE temp317.
    temp315 ?= cl_abap_typedescr=>describe_by_data( tab ).
    
    lo_tabledescr = temp315.
    CREATE DATA result TYPE HANDLE lo_tabledescr.

    
    
    ASSIGN result->* TO <result>.

    
    lv_lines = lines( tab ).
    
    IF to <= 0 OR to > lv_lines.
      temp316 = lv_lines.
    ELSE.
      temp316 = to.
    ENDIF.
    
    lv_to = temp316.
    
    IF from < 1.
      temp317 = 1.
    ELSE.
      temp317 = from.
    ENDIF.
    
    lv_from = temp317.

    LOOP AT tab ASSIGNING <row> FROM lv_from TO lv_to.
      INSERT <row> INTO TABLE <result>.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_paginate.
    DATA temp318 TYPE i.
    DATA lv_from TYPE i.
    DATA lv_to TYPE i.

    total_count = lines( tab ).
    
    IF page_size <= 0.
      temp318 = 1.
    ELSE.
      temp318 = ( total_count + page_size - 1 ) DIV page_size.
    ENDIF.
    total_pages = temp318.

    
    lv_from = ( page - 1 ) * page_size + 1.
    
    lv_to   = page * page_size.

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
    TYPES temp5 TYPE HASHED TABLE OF ty_s_count WITH UNIQUE KEY n.
DATA lt_count TYPE temp5.
    " the values in the order they first came, which is the order of result
    DATA lt_order TYPE string_table.
      DATA lv_val TYPE string.
        DATA temp319 TYPE ty_s_count.
    DATA temp320 LIKE LINE OF lt_order.
    DATA lr_val LIKE REF TO temp320.
      DATA temp321 TYPE zabaputil_cl_util_context=>ty_s_name_value.
      FIELD-SYMBOLS <temp665> LIKE LINE OF lt_count.
      DATA temp666 LIKE sy-tabix.

    LOOP AT tab ASSIGNING <row>.
      ASSIGN COMPONENT fieldname OF STRUCTURE <row> TO <field>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      
      lv_val = |{ <field> }|.
      READ TABLE lt_count WITH KEY n = lv_val ASSIGNING <count>.
      IF sy-subrc = 0.
        <count>-count = <count>-count + 1.
      ELSE.
        
        CLEAR temp319.
        temp319-n = lv_val.
        temp319-count = 1.
        INSERT temp319 INTO TABLE lt_count.
        INSERT lv_val INTO TABLE lt_order.
      ENDIF.
    ENDLOOP.

    
    
    LOOP AT lt_order REFERENCE INTO lr_val.
      
      CLEAR temp321.
      temp321-n = lr_val->*.
      
      
      temp666 = sy-tabix.
      READ TABLE lt_count WITH KEY n = lr_val->* ASSIGNING <temp665>.
      sy-tabix = temp666.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      temp321-v = |{ <temp665>-count }|.
      INSERT temp321 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  " ========== Validation Helpers ==========

  METHOD check_is_email.
    DATA temp322 TYPE string.
    DATA lv_val TYPE string.
    DATA lv_local TYPE string.
    DATA lv_domain TYPE string.
    DATA lv_extra TYPE string.
    DATA lv_last TYPE i.
    temp322 = val.
    
    lv_val = c_trim( temp322 ).

    IF lv_val IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    " Basic email validation: no blank, exactly one @, text before and after,
    " a dot inside the domain - not at its start or end, and not doubled
    IF lv_val CA | \t|.
      RETURN.
    ENDIF.

    
    
    
    SPLIT lv_val AT `@` INTO lv_local lv_domain lv_extra.
    IF lv_extra IS NOT INITIAL OR lv_local IS INITIAL OR lv_domain IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    IF lv_domain NA `.` OR lv_domain CS `..` OR lv_local CS `..`.
      result = abap_false.
      RETURN.
    ENDIF.

    
    lv_last = strlen( lv_domain ) - 1.
    IF lv_domain(1) = `.` OR lv_domain+lv_last(1) = `.`.
      result = abap_false.
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.

  METHOD check_is_numeric_string.
    DATA temp323 TYPE string.
    DATA lv_val TYPE string.
    DATA lv_len TYPE i.
    DATA lv_i TYPE i.
    DATA lv_has_dot LIKE abap_false.
    DATA lv_has_digit LIKE abap_false.
    DATA lv_has_sign LIKE abap_false.
      DATA lv_c TYPE string.
    temp323 = val.
    
    lv_val = c_trim( temp323 ).
    
    lv_len = strlen( lv_val ).

    IF lv_val IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    
    lv_i = 0.
    
    lv_has_dot = abap_false.
    
    lv_has_digit = abap_false.
    
    lv_has_sign = abap_false.
    WHILE lv_i < lv_len.
      
      lv_c = lv_val+lv_i(1).
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
    DATA lv_val TYPE string.
    DATA temp21 TYPE xsdboolean.
    lv_val = condense( val ).
    
    temp21 = boolc( lv_val IS NOT INITIAL AND lv_val CO `0123456789` AND strlen( lv_val ) <= 9 ).
    result = temp21.

  ENDMETHOD.

  METHOD check_is_date_valid.
    DATA lv_date  TYPE d.
    DATA lv_str   TYPE string.
    DATA lv_year  TYPE i.
    DATA lv_month TYPE i.
    DATA lv_day   TYPE i.
    DATA lv_max   TYPE i.
    DATA temp22 TYPE xsdboolean.

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

    
    temp22 = boolc( lv_day <= lv_max ).
    result = temp22.
  ENDMETHOD.

  METHOD check_is_guid.

    DATA temp324 TYPE string.
    DATA lv_val TYPE string.
    DATA lv_clean TYPE string.
    DATA lv_len TYPE i.
    DATA lv_upper TYPE string.
    DATA lv_i TYPE i.
      DATA lv_c TYPE string.
    temp324 = val.
    
    lv_val = c_trim( temp324 ).
    
    lv_clean = c_replace_all( val = lv_val sub = `-` new_val = `` ).
    
    lv_len = strlen( lv_clean ).

    " Accept 32 chars (raw) or 36 chars (with dashes)
    IF lv_len <> 32.
      result = abap_false.
      RETURN.
    ENDIF.

    " All characters must be hex digits
    
    lv_upper = to_upper( lv_clean ).
    
    lv_i = 0.
    WHILE lv_i < 32.
      
      lv_c = lv_upper+lv_i(1).
      IF NOT ( ( lv_c >= `0` AND lv_c <= `9` ) OR ( lv_c >= `A` AND lv_c <= `F` ) ).
        result = abap_false.
        RETURN.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

    result = abap_true.

  ENDMETHOD.

  METHOD check_max_length.

    DATA temp325 TYPE string.
    DATA lv_val LIKE temp325.
    DATA temp23 TYPE xsdboolean.
    temp325 = val.
    
    lv_val = temp325.
    
    temp23 = boolc( strlen( lv_val ) <= max ).
    result = temp23.

  ENDMETHOD.

  " ========== Deep Comparison ==========

  METHOD data_equals.
        DATA temp24 TYPE xsdboolean.

    TRY.
        
        temp24 = boolc( a = b ).
        result = temp24.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD data_diff.

    FIELD-SYMBOLS <old_field> TYPE any.
    FIELD-SYMBOLS <new_field> TYPE any.

    DATA lt_comps TYPE abap_component_tab.
    DATA temp326 LIKE LINE OF lt_comps.
    DATA lr_comp LIKE REF TO temp326.
        DATA temp327 TYPE zabaputil_cl_util_context=>ty_s_field_diff.
    lt_comps = rtti_get_t_attri_by_any( old ).

    
    
    LOOP AT lt_comps REFERENCE INTO lr_comp.
      ASSIGN COMPONENT lr_comp->name OF STRUCTURE old TO <old_field>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lr_comp->name OF STRUCTURE new TO <new_field>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <old_field> <> <new_field>.
        
        CLEAR temp327.
        temp327-fieldname = lr_comp->name.
        temp327-old_value = |{ <old_field> }|.
        temp327-new_value = |{ <new_field> }|.
        INSERT temp327 INTO TABLE result.
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

    DATA lt_all TYPE zabaputil_cl_util_context=>ty_t_name_value.
    DATA temp328 TYPE string.
    DATA lv_val LIKE temp328.
    DATA temp329 TYPE string.
    DATA temp330 TYPE zabaputil_cl_util_context=>ty_s_name_value.
    lt_all = enum_get_all( domain = domain langu = langu ).
    
    temp328 = value.
    
    lv_val = temp328.
    
    CLEAR temp329.
    
    READ TABLE lt_all INTO temp330 WITH KEY n = lv_val.
    IF sy-subrc = 0.
      temp329 = temp330-v.
    ENDIF.
    result = temp329.

  ENDMETHOD.

  METHOD enum_get_all.
        DATA temp331 TYPE string.
        DATA lo_type TYPE REF TO cl_abap_typedescr.
        DATA temp332 TYPE REF TO cl_abap_elemdescr.
        DATA lo_elem LIKE temp332.
        DATA lt_fix TYPE zabaputil_cl_util_context=>ty_t_fix_val.
        DATA ls_fix LIKE LINE OF lt_fix.
          DATA temp333 TYPE zabaputil_cl_util_context=>ty_s_name_value.

    TRY.
        
        temp331 = domain.
        
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = temp331
                                             RECEIVING  p_descr_ref    = lo_type
                                             EXCEPTIONS type_not_found = 1
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        
        temp332 ?= lo_type.
        
        lo_elem = temp332.
        
        lt_fix = rtti_get_t_fixvalues( elemdescr = lo_elem
                                              langu     = langu ).

        
        LOOP AT lt_fix INTO ls_fix.
          
          CLEAR temp333.
          temp333-n = ls_fix-low.
          temp333-v = ls_fix-descr.
          INSERT temp333 INTO TABLE result.
        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  " ========== Deep Field Access ==========

  METHOD data_get_by_path.

    FIELD-SYMBOLS <current> TYPE any.
    FIELD-SYMBOLS <field>   TYPE any.
    TYPES temp6 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_segments TYPE temp6.
    DATA lv_segment LIKE LINE OF lt_segments.
      DATA lv_seg TYPE string.

    ASSIGN data TO <current>.

    

    SPLIT path AT `-` INTO TABLE lt_segments.

    
    LOOP AT lt_segments INTO lv_segment.
      
      lv_seg = c_trim_upper( lv_segment ).
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
    TYPES temp7 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_segments TYPE temp7.
    DATA lv_last_idx TYPE i.
    DATA lv_segment LIKE LINE OF lt_segments.
      DATA lv_seg TYPE string.

    ASSIGN data TO <current>.

    

    SPLIT path AT `-` INTO TABLE lt_segments.
    
    lv_last_idx = lines( lt_segments ).

    
    LOOP AT lt_segments INTO lv_segment.
      
      lv_seg = c_trim_upper( lv_segment ).
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
        DATA temp334 TYPE string.
        DATA lv_result LIKE temp334.
        DATA lv_class TYPE string.
        DATA x TYPE REF TO cx_root.

    TRY.

        
        CLEAR temp334.
        
        lv_result = temp334.
        
        lv_class = `CL_ABAP_CONTEXT_INFO`.

        IF check_abap_cloud( ) IS NOT INITIAL.
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

        
      CATCH cx_root INTO x.
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

    IF check_abap_cloud( ) IS NOT INITIAL.
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
    TYPES temp8 TYPE STANDARD TABLE OF ty_s_impl WITH DEFAULT KEY.
DATA lt_impl TYPE temp8.
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
        DATA lx_uuid TYPE REF TO cx_root.

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

        
      CATCH cx_root INTO lx_uuid.
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
        DATA lx_uuid TYPE REF TO cx_root.

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

        
      CATCH cx_root INTO lx_uuid.
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
        DATA obj TYPE REF TO object.
        DATA content TYPE REF TO object.
        DATA lv_classname TYPE c LENGTH 30.
        DATA xco_cp_abap TYPE c LENGTH 11.
        DATA lv_description TYPE c LENGTH 60.
    TRY.

        
        
        
        
        

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
      DATA lv_assign TYPE string.
      DATA r TYPE REF TO data.
      FIELD-SYMBOLS <lt_lines> TYPE string_table.
      DATA text LIKE LINE OF <lt_lines>.
        DATA temp335 TYPE ty_s_stack.
        DATA ls_stack LIKE temp335.
      DATA lr_stack TYPE REF TO data.
      FIELD-SYMBOLS <lt_stack> TYPE ANY TABLE.
      FIELD-SYMBOLS <ls_stack> TYPE any.
      FIELD-SYMBOLS <lv_comp> TYPE any.
          DATA lv_fm TYPE string.
            DATA temp336 TYPE ty_s_stack.
            DATA ls_frame LIKE temp336.
              DATA temp337 TYPE string.
              DATA lv_rest TYPE string.
          DATA lx_stack TYPE REF TO zabaputil_cx_util_error.
          DATA lx_root TYPE REF TO cx_root.

    IF check_abap_cloud( ) IS NOT INITIAL.

      
      
      
      
      
      
      
      
      
      
      
      
      
      

      "1 format source
      
      lv_assign = `XCO_CP_CALL_STACK=>LINE_NUMBER_FLAVOR->SOURCE`.
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

      
      ASSIGN ro_lines->(`IF_XCO_STRINGS~VALUE`) TO <lt_lines>.

      
      LOOP AT <lt_lines> INTO text.
        
        CLEAR temp335.
        
        ls_stack = temp335.
        SPLIT text AT ` ` INTO ls_stack-class ls_stack-include ls_stack-method.
        INSERT ls_stack INTO TABLE result.
      ENDLOOP.

      DELETE result INDEX 1.

    ELSE.

      " Standard ABAP and 7.02: SYSTEM_CALLSTACK - the result was empty
      " there, which read like a call from nowhere
      
      
      
      
      TRY.
          CREATE DATA lr_stack TYPE (`ABAP_CALLSTACK`).
          ASSIGN lr_stack->* TO <lt_stack>.
          
          lv_fm = `SYSTEM_CALLSTACK`.
          CALL FUNCTION lv_fm
            IMPORTING
              callstack     = <lt_stack>
            EXCEPTIONS
              error_message = 2
              OTHERS        = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |SYSTEM_CALLSTACK failed (sy-subrc { sy-subrc })|.
          ENDIF.
          LOOP AT <lt_stack> ASSIGNING <ls_stack>.
            
            CLEAR temp336.
            
            ls_frame = temp336.
            ASSIGN COMPONENT `MAINPROGRAM` OF STRUCTURE <ls_stack> TO <lv_comp>.
            IF sy-subrc = 0.
              " ZCL_X=====...CP: the class of a class pool
              
              temp337 = <lv_comp>.
              
              SPLIT temp337 AT `=` INTO ls_frame-class lv_rest.
            ENDIF.
            ASSIGN COMPONENT `INCLUDE` OF STRUCTURE <ls_stack> TO <lv_comp>.
            IF sy-subrc = 0.
              ls_frame-include = <lv_comp>.
            ENDIF.
            ASSIGN COMPONENT `EVENT` OF STRUCTURE <ls_stack> TO <lv_comp>.
            IF sy-subrc = 0.
              ls_frame-method = <lv_comp>.
            ENDIF.
            ASSIGN COMPONENT `LINE` OF STRUCTURE <ls_stack> TO <lv_comp>.
            IF sy-subrc = 0.
              ls_frame-line = |{ <lv_comp> }|.
            ENDIF.
            INSERT ls_frame INTO TABLE result.
          ENDLOOP.
          
        CATCH zabaputil_cx_util_error INTO lx_stack.
          RAISE EXCEPTION lx_stack.
          
        CATCH cx_root INTO lx_root.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_root.
      ENDTRY.

      " this method itself, as on ABAP Cloud
      DELETE result INDEX 1.

    ENDIF.

  ENDMETHOD.

  METHOD context_get_sy.

    MOVE-CORRESPONDING sy TO result.

  ENDMETHOD.


  METHOD msg_get_text.

    DATA lt_msg TYPE zabaputil_cl_util_context=>ty_t_msg.
      FIELD-SYMBOLS <temp338> LIKE LINE OF lt_msg.
      DATA temp339 LIKE sy-tabix.
    lt_msg = msg_get_t( val = val val2 = val2 ).
    IF lt_msg IS NOT INITIAL.
      
      
      temp339 = sy-tabix.
      READ TABLE lt_msg INDEX 1 ASSIGNING <temp338>.
      sy-tabix = temp339.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result = <temp338>-text.
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_by_sy.

    result = msg_get_t( context_get_sy( ) ).

  ENDMETHOD.

  METHOD msg_get_internal.
        FIELD-SYMBOLS <tab> TYPE ANY TABLE.
        DATA lt_attri TYPE abap_component_tab.
        DATA temp340 TYPE ty_s_msg.
        DATA ls_result LIKE temp340.
        DATA temp341 LIKE LINE OF lt_attri.
        DATA ls_attri LIKE REF TO temp341.
          FIELD-SYMBOLS <comp> TYPE any.
          DATA temp342 TYPE zabaputil_cl_util_context=>ty_s_msg.

    CASE rtti_get_type_kind( val ).

      WHEN cl_abap_datadescr=>typekind_table.
        
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

        
        lt_attri = rtti_get_t_attri_by_any( val ).

        
        CLEAR temp340.
        
        ls_result = temp340.
        
        
        LOOP AT lt_attri REFERENCE INTO ls_attri.
          
          ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <comp>.
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
        IF rtti_check_clike( val ) IS NOT INITIAL AND val IS NOT INITIAL.
          
          CLEAR temp342.
          temp342-text = val.
          INSERT temp342 INTO TABLE result.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD msg_get_by_oref.
        DATA temp343 TYPE REF TO cx_root.
        DATA lx LIKE temp343.
        DATA temp344 TYPE zabaputil_cl_util_context=>ty_s_msg.
    DATA obj TYPE REF TO object.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE any.
    DATA temp345 TYPE zabaputil_cl_util_context=>ty_s_msg.

    " four shapes, tried in turn and each on its own level - the nested
    " TRY/CATCH pyramid this used to be carried the attribute mapping twice,
    " three CATCH blocks deep, one fix away from drifting. Every attempt
    " that fails falls through to the next; the last one cannot fail
    " quietly, it maps whatever public attributes the object has

    " 1. an exception: its text, and its public attributes over it
    TRY.
        
        temp343 ?= val.
        
        lx = temp343.
        
        CLEAR temp344.
        temp344-type = `E`.
        temp344-text = lx->get_text( ).
        INSERT msg_get_by_oref_attri( io_obj = lx
                                      is_msg = temp344 ) INTO TABLE result.
        RETURN.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    
    obj = val.
    
    

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
    
    CLEAR temp345.
    INSERT msg_get_by_oref_attri( io_obj = obj
                                  is_msg = temp345 ) INTO TABLE result.

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

    DATA lt_attri TYPE abap_component_tab.
    DATA temp346 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp346.
          DATA temp347 TYPE REF TO cl_abap_tabledescr.
          DATA lo_line TYPE REF TO cl_abap_datadescr.
          DATA temp348 TYPE REF TO cl_abap_structdescr.
          DATA lt_comps TYPE abap_component_tab.
          DATA temp349 LIKE LINE OF lt_comps.
          DATA ls_comp LIKE REF TO temp349.
    lt_attri = rtti_get_t_attri_by_any( val ).

    
    
    LOOP AT lt_attri REFERENCE INTO ls_attri.
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
          
          temp347 ?= ls_attri->type.
          
          lo_line = temp347->get_table_line_type( ).
          IF lo_line->kind <> cl_abap_typedescr=>kind_struct.
            CONTINUE.
          ENDIF.
          
          temp348 ?= lo_line.
          
          lt_comps = temp348->get_components( ).
          
          
          LOOP AT lt_comps REFERENCE INTO ls_comp.
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

    DATA lv_kind TYPE string.
    DATA lv_is_row TYPE abap_bool.
    DATA lt_attri TYPE abap_component_tab.
    DATA temp350 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp350.
      FIELD-SYMBOLS <tab> TYPE any.
      FIELD-SYMBOLS <ftab> TYPE ANY TABLE.
      DATA temp351 TYPE REF TO cl_abap_tabledescr.
      DATA lo_line TYPE REF TO cl_abap_datadescr.
      FIELD-SYMBOLS <row> TYPE ANY.
    lv_kind = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.

    
    msg_get_rap_row( EXPORTING val         = val
                               entity_name = entity_name
                     IMPORTING messages    = result
                               is_row      = lv_is_row ).
    IF lv_is_row = abap_true.
      RETURN.
    ENDIF.

    
    lt_attri = rtti_get_t_attri_by_any( val ).
    
    
    LOOP AT lt_attri REFERENCE INTO ls_attri.
      
      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <tab>.
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
      
      UNASSIGN <ftab>.
      ASSIGN <tab> TO <ftab>.
      IF <ftab> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      " the row kind is a property of the table TYPE - decided once per
      " table, not asked of every row (same idea as msg_get_internal_tab)
      
      temp351 ?= ls_attri->type.
      
      lo_line = temp351->get_table_line_type( ).
      
      LOOP AT <ftab> ASSIGNING <row>.
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
    DATA lv_meta_built TYPE abap_bool.
    DATA lt_meta TYPE ty_t_name_value.
    FIELD-SYMBOLS <msg> TYPE any.
            DATA lt_one TYPE zabaputil_cl_util_context=>ty_t_msg.
            FIELD-SYMBOLS <m> LIKE LINE OF lt_one.
    FIELD-SYMBOLS <fail> TYPE any.
      FIELD-SYMBOLS <cause> TYPE any.
        DATA lv_cause TYPE i.
        DATA lv_text TYPE string.
        DATA temp352 TYPE zabaputil_cl_util_context=>ty_s_msg.
    CLEAR messages.
    is_row = abap_false.

    " the meta block is built LAZILY: msg_get_rap_meta walks the row's
    " components three times over (element/action/tky scans), and the
    " common row in a RAP response table carries neither a filled %MSG nor
    " a %FAIL - building the block up front threw that work away per row
    
    

    
    ASSIGN COMPONENT `%MSG` OF STRUCTURE val TO <msg>.
    IF sy-subrc = 0.
      is_row = abap_true.
      IF <msg> IS NOT INITIAL.
        lt_meta = msg_get_rap_meta( val ).
        lv_meta_built = abap_true.
        TRY.
            
            lt_one = msg_get_t( <msg> ).
            
            LOOP AT lt_one ASSIGNING <m>.
              <m>-t_meta = lt_meta.
            ENDLOOP.
            INSERT LINES OF lt_one INTO TABLE messages.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDIF.

    
    ASSIGN COMPONENT `%FAIL` OF STRUCTURE val TO <fail>.
    IF sy-subrc = 0.
      is_row = abap_true.
      
      ASSIGN COMPONENT `CAUSE` OF STRUCTURE <fail> TO <cause>.
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
        
        IF cl_abap_typedescr=>describe_by_data( <cause> )->type_kind = `k`.
          lv_cause = msg_get_rap_cause_value( |{ <cause> }| ).
        ELSE.
          lv_cause = <cause>.
        ENDIF.
        
        lv_text = msg_get_rap_fail_text( lv_cause ).
        IF entity_name IS NOT INITIAL.
          lv_text = |{ entity_name }: { lv_text }|.
        ENDIF.
        
        CLEAR temp352.
        temp352-type = `E`.
        temp352-text = lv_text.
        temp352-t_meta = lt_meta.
        INSERT temp352 INTO TABLE messages.
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
    FIELD-SYMBOLS <comp> TYPE any.
    ASSIGN COMPONENT comp OF STRUCTURE val TO <comp>.
    IF sy-subrc = 0 AND rtti_check_printable( <comp> ) = abap_true.
      result = <comp>.
    ENDIF.

  ENDMETHOD.

  METHOD scan_flag_prefix.

    DATA lv_kind TYPE string.
    DATA temp353 TYPE string.
    DATA lv_prefix LIKE temp353.
    DATA lv_off TYPE i.
      FIELD-SYMBOLS <sub> TYPE any.
    DATA lv_len TYPE i.
    DATA lt_attri TYPE abap_component_tab.
    DATA temp354 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp354.
      FIELD-SYMBOLS <flag> TYPE any.
    lv_kind = rtti_get_type_kind( val ).
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
    
    temp353 = prefix.
    
    lv_prefix = temp353.
    
    lv_off = find( val = lv_prefix
                         sub = `-` ).
    IF lv_off >= 0.
      
      ASSIGN COMPONENT lv_prefix(lv_off) OF STRUCTURE val TO <sub>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      result = scan_flag_prefix( val    = <sub>
                                 prefix = substring( val = lv_prefix
                                                     off = lv_off + 1 ) ).
      RETURN.
    ENDIF.

    
    lv_len = strlen( lv_prefix ).
    
    lt_attri = rtti_get_t_attri_by_any( val ).
    
    
    LOOP AT lt_attri REFERENCE INTO ls_attri.
      IF strlen( ls_attri->name ) <= lv_len.
        CONTINUE.
      ENDIF.
      IF NOT ( lv_len = 0 OR ls_attri->name(lv_len) = lv_prefix ).
        CONTINUE.
      ENDIF.
      
      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <flag>.
      IF sy-subrc <> 0 OR <flag> IS INITIAL.
        CONTINUE.
      ENDIF.
      INSERT ls_attri->name+lv_len INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD msg_get_rap_element.

    DATA lt_suffix TYPE string_table.
    lt_suffix = scan_flag_prefix( val       = val
                                        prefix = `%ELEMENT-` ).
    result = concat_lines_of( table = lt_suffix
                              sep   = `, ` ).

  ENDMETHOD.

  METHOD msg_get_rap_state_area.

    result = get_comp_str( val     = val
                           comp = `%STATE_AREA` ).

  ENDMETHOD.

  METHOD msg_get_rap_action.

    DATA lt_suffix TYPE string_table.
    DATA temp355 TYPE string.
    DATA temp356 TYPE string.
    lt_suffix = scan_flag_prefix( val       = val
                                        prefix = `%OP-%ACTION-` ).
    
    CLEAR temp355.
    
    READ TABLE lt_suffix INTO temp356 INDEX 1.
    IF sy-subrc = 0.
      temp355 = temp356.
    ENDIF.
    result = temp355.

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

    FIELD-SYMBOLS <tky> TYPE any.
    ASSIGN COMPONENT `%TKY` OF STRUCTURE val TO <tky>.
    IF sy-subrc <> 0 OR <tky> IS INITIAL.
      RETURN.
    ENDIF.
    result = msg_get_rap_flatten( <tky> ).

  ENDMETHOD.

  METHOD msg_get_rap_flatten.

    DATA lv_kind TYPE string.
    DATA lt_attri TYPE abap_component_tab.
    DATA lt_part TYPE string_table.
    DATA temp357 LIKE LINE OF lt_attri.
    DATA ls_attri LIKE REF TO temp357.
      FIELD-SYMBOLS <comp> TYPE any.
      DATA lv_sub_kind TYPE string.
        DATA lv_sub TYPE string.
            DATA lv_str TYPE string.
            DATA temp358 LIKE LINE OF lt_part.
    lv_kind = rtti_get_type_kind( val ).
    IF lv_kind <> cl_abap_datadescr=>typekind_struct1
        AND lv_kind <> cl_abap_datadescr=>typekind_struct2.
      RETURN.
    ENDIF.

    
    lt_attri = rtti_get_t_attri_by_any( val ).
    

    
    
    LOOP AT lt_attri REFERENCE INTO ls_attri.
      
      ASSIGN COMPONENT ls_attri->name OF STRUCTURE val TO <comp>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      
      lv_sub_kind = rtti_get_type_kind( <comp> ).
      IF lv_sub_kind = cl_abap_datadescr=>typekind_struct1
          OR lv_sub_kind = cl_abap_datadescr=>typekind_struct2.
        
        lv_sub = msg_get_rap_flatten( <comp> ).
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
            
            lv_str = <comp>.
            
            temp358 = |{ ls_attri->name }={ lv_str }|.
            INSERT temp358 INTO TABLE lt_part.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    result = concat_lines_of( table = lt_part
                              sep   = `, ` ).

  ENDMETHOD.

  METHOD msg_get_rap_meta.

    DATA lv TYPE string.
      DATA temp359 TYPE zabaputil_cl_util_context=>ty_s_name_value.
      DATA temp360 TYPE zabaputil_cl_util_context=>ty_s_name_value.
      DATA temp361 TYPE zabaputil_cl_util_context=>ty_s_name_value.
      DATA temp362 TYPE zabaputil_cl_util_context=>ty_s_name_value.
      DATA temp363 TYPE zabaputil_cl_util_context=>ty_s_name_value.
      DATA temp364 TYPE zabaputil_cl_util_context=>ty_s_name_value.

    lv = msg_get_rap_element( val ).
    IF lv IS NOT INITIAL.
      
      CLEAR temp359.
      temp359-n = `element`.
      temp359-v = lv.
      INSERT temp359 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_state_area( val ).
    IF lv IS NOT INITIAL.
      
      CLEAR temp360.
      temp360-n = `state_area`.
      temp360-v = lv.
      INSERT temp360 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_action( val ).
    IF lv IS NOT INITIAL.
      
      CLEAR temp361.
      temp361-n = `action`.
      temp361-v = lv.
      INSERT temp361 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_pid( val ).
    IF lv IS NOT INITIAL.
      
      CLEAR temp362.
      temp362-n = `pid`.
      temp362-v = lv.
      INSERT temp362 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_cid( val ).
    IF lv IS NOT INITIAL.
      
      CLEAR temp363.
      temp363-n = `cid`.
      temp363-v = lv.
      INSERT temp363 INTO TABLE result.
    ENDIF.

    lv = msg_get_rap_tky( val ).
    IF lv IS NOT INITIAL.
      
      CLEAR temp364.
      temp364-n = `tky`.
      temp364-v = lv.
      INSERT temp364 INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD msg_get_rap_fail_text.
    " the base values of IF_ABAP_BEHV=>T_FAIL_CAUSE, read on an S/4HANA
    " system (2026-11-13) - HTTP-like codes, not a running number
    DATA temp365 TYPE string.
    CASE cause.
      WHEN 0.
        temp365 = `Operation failed`.
      WHEN 401.
        temp365 = `Authorization failure`.
      WHEN 404.
        temp365 = `Entity not found`.
      WHEN 409.
        temp365 = `Concurrent modification`.
      WHEN 423.
        temp365 = `Entity is locked`.
      WHEN 424.
        temp365 = `Dependent operation failed`.
      WHEN 501.
        temp365 = `Operation not implemented`.
      WHEN 4221.
        temp365 = `Operation disabled`.
      WHEN 4222.
        temp365 = `Entity is read-only`.
      WHEN OTHERS.
        temp365 = |Operation failed (cause code { cause })|.
    ENDCASE.
    result = temp365.
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
    TYPES temp9 TYPE STANDARD TABLE OF ty_c30 WITH DEFAULT KEY.
DATA names TYPE temp9.
    FIELD-SYMBOLS <any> TYPE any.
    FIELD-SYMBOLS <field> TYPE simple.
    FIELD-SYMBOLS <ddfields> TYPE ANY TABLE.
            DATA lv_method2 TYPE string.
            DATA workaround TYPE string.
            DATA temp366 TYPE REF TO cl_abap_structdescr.
    DATA lt_comp TYPE abap_component_tab.
    DATA temp367 LIKE LINE OF lt_comp.
    DATA lr_comp LIKE REF TO temp367.
      DATA lv_check_key LIKE abap_false.
      DATA temp368 LIKE sy-subrc.
      DATA temp369 TYPE zabaputil_cl_util_context=>ty_s_dfies.

* convert to correct type,
    lv_tabname = tabname.

    TRY.
        TRY.
            
            lv_method2 = `XCO_CP_ABAP_DICTIONARY`.
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
            
            workaround = `DDFIELDS`.
            CREATE DATA lr_ddfields TYPE (workaround).
            ASSIGN lr_ddfields->* TO <ddfields>.
            ASSERT sy-subrc = 0.
            
            temp366 ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
            <ddfields> = temp366->get_ddic_field_list( ).
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
    
    lt_comp = rtti_get_t_attri_by_table_name( tabname ).
    
    
    LOOP AT lt_comp REFERENCE INTO lr_comp.

      
      lv_check_key = abap_false.
      
      READ TABLE names WITH KEY table_line = lr_comp->name TRANSPORTING NO FIELDS.
      temp368 = sy-subrc.
      IF temp368 = 0.
        lv_check_key = abap_true.
      ENDIF.

      
      CLEAR temp369.
      temp369-fieldname = lr_comp->name.
      temp369-rollname = lr_comp->name.
      temp369-keyflag = lv_check_key.
      temp369-scrtext_s = lr_comp->name.
      temp369-scrtext_m = lr_comp->name.
      temp369-scrtext_l = lr_comp->name.
      INSERT temp369 INTO TABLE result.

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
          result = result.
MOVE-CORRESPONDING <dfies> TO result.
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
      DATA lan LIKE sy-langu.
          DATA lv_tabname TYPE string.

    IF langu IS NOT SUPPLIED.
      
      lan = sy-langu.
    ELSE.
      lan = langu.
    ENDIF.

    IF check_abap_cloud( ) IS NOT INITIAL.

      ddtext = tabname.

    ELSE.

      TRY.
          
          lv_tabname = `dd02t`.
          SELECT SINGLE ddtext
            FROM (lv_tabname) INTO ddtext
            WHERE tabname    = tabname
              AND ddlanguage = lan
            .
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

    TYPES temp10 TYPE TABLE OF string.
DATA lt_result_tab TYPE temp10.
    DATA ls_comp       TYPE abap_componentdescr.
    DATA lt_comps      TYPE abap_component_tab.
    DATA lo_datadescr  TYPE REF TO cl_abap_datadescr.
    DATA lr_line       TYPE REF TO data.

    " data ls_shlp type shlp_descr.

    DATA lr_shlp       TYPE REF TO data.

    DATA lv_type TYPE string.
    FIELD-SYMBOLS <shlp> TYPE any.
    DATA lv_tabname TYPE c LENGTH 30.
    DATA lv_fieldname TYPE c LENGTH 30.
      DATA lv_fm TYPE string.
        DATA lr_t_shlp TYPE REF TO data.
        DATA lv_type2 TYPE string.
        FIELD-SYMBOLS <shlp2> TYPE STANDARD TABLE.
        FIELD-SYMBOLS <row2> TYPE any.
      DATA temp370 LIKE LINE OF ms_shlp-interface.
      DATA r_interface LIKE REF TO temp370.
        FIELD-SYMBOLS <any> TYPE any.
        FIELD-SYMBOLS <value> TYPE any.
    DATA interface LIKE LINE OF ms_shlp-interface.
        DATA temp371 TYPE zabaputil_cl_util_context=>ty_shlp_descr-selopt.
        DATA temp372 LIKE LINE OF temp371.
        DATA temp667 TYPE zabaputil_cl_util_context=>ty_ddshselopt-option.
    DATA fieldrop LIKE LINE OF ms_shlp-fieldprop.
      DATA default_value LIKE fieldrop-defaultval.
      DATA temp373 TYPE zabaputil_cl_util_context=>ty_shlp_descr-selopt.
      DATA temp374 LIKE LINE OF temp373.
      DATA temp668 TYPE zabaputil_cl_util_context=>ty_ddshselopt-option.
    DATA field_props LIKE LINE OF ms_shlp-fieldprop.
      DATA temp375 TYPE zabaputil_cl_util_context=>ty_s_dfies_2.
      DATA temp376 TYPE zabaputil_cl_util_context=>ty_s_dfies_2.
      DATA descption LIKE temp375.
    DATA temp377 LIKE sy-subrc.
    DATA temp378 LIKE sy-subrc.
    DATA temp379 LIKE sy-tabix.
    DATA strucdescr TYPE REF TO cl_abap_structdescr.
    DATA tabdescr TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <fs_target_tab> TYPE STANDARD TABLE.
    DATA result_line LIKE LINE OF lt_result_tab.
      FIELD-SYMBOLS <fs_line> TYPE data.
      DATA result_desc LIKE LINE OF mt_result_desc.
        FIELD-SYMBOLS <line_content> TYPE any.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <row> TYPE any.
    lv_type = `SHLP_DESCR`.
    CREATE DATA lr_shlp TYPE (lv_type).
    
    ASSIGN lr_shlp->* TO <shlp>.

    
    
    lv_tabname = mv_table.
    lv_fieldname = mv_fname.

    IF ms_shlp IS INITIAL.
      " Suchhilfe lesen
      
      lv_fm = `F4IF_DETERMINE_SEARCHHELP`.
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
      MOVE-CORRESPONDING <shlp> TO ms_shlp.

      IF ms_shlp-intdescr-issimple = abap_false.

        
        
        lv_type2 = `SHLP_DESCT`.
        CREATE DATA lr_t_shlp TYPE (lv_type2).
        
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

        
        READ TABLE <shlp2> INDEX 1 ASSIGNING <row2>.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <row2> TO ms_shlp.
        ENDIF.
      ENDIF.
    ENDIF.

    IF mr_data IS BOUND.
      " Values from Caller app to Interface Values
      
      
      LOOP AT ms_shlp-interface REFERENCE INTO r_interface WHERE value IS INITIAL.

        
        ASSIGN mr_data->* TO <any>.
        
        ASSIGN COMPONENT r_interface->shlpfield OF STRUCTURE <any> TO <value>.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        r_interface->value = <value>.

      ENDLOOP.
    ENDIF.

    " Interface Fixed Values to Selopt
    
    LOOP AT ms_shlp-interface INTO interface.

      " Match the name of the SH Field to the Input field name
      IF interface-valfield = mv_fname.
        mv_shlpfield = interface-shlpfield.
      ENDIF.

      IF interface-value IS NOT INITIAL.

        
        CLEAR temp371.
        temp371 = ms_shlp-selopt.
        
        temp372-shlpfield = interface-shlpfield.
        temp372-shlpname = interface-valtabname.
        
        IF interface-value CA `*`.
          temp667 = `CP`.
        ELSE.
          temp667 = `EQ`.
        ENDIF.
        temp372-option = temp667.
        temp372-sign = `I`.
        temp372-low = interface-value.
        INSERT temp372 INTO TABLE temp371.
        ms_shlp-selopt = temp371.

      ENDIF.

    ENDLOOP.

    
    LOOP AT ms_shlp-fieldprop INTO fieldrop.

      IF fieldrop-defaultval IS INITIAL.
        CONTINUE.
      ENDIF.

      
      default_value = fieldrop-defaultval.
      default_value = replace( val = default_value sub = `'` with = `` occ = 0 ).

      
      CLEAR temp373.
      temp373 = ms_shlp-selopt.
      
      temp374-shlpfield = fieldrop-fieldname.
      
      IF fieldrop-defaultval CA `*`.
        temp668 = `CP`.
      ELSE.
        temp668 = `EQ`.
      ENDIF.
      temp374-option = temp668.
      temp374-sign = `I`.
      temp374-low = default_value.
      INSERT temp374 INTO TABLE temp373.
      ms_shlp-selopt = temp373.

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

    
    LOOP AT ms_shlp-fieldprop INTO field_props WHERE shlplispos IS NOT INITIAL.

      
      CLEAR temp375.
      
      READ TABLE mt_result_desc INTO temp376 WITH KEY fieldname = field_props-fieldname.
      IF sy-subrc = 0.
        temp375 = temp376.
      ENDIF.
      
      descption = temp375.

      ls_comp-name  = descption-fieldname.
      ls_comp-type ?= cl_abap_datadescr=>describe_by_name( descption-rollname ).
      INSERT ls_comp INTO TABLE lt_comps.

    ENDLOOP.

    
    
    
    temp378 = sy-subrc.
    temp379 = sy-tabix.
    READ TABLE lt_comps WITH KEY name = `ROW_ID` TRANSPORTING NO FIELDS.
    temp377 = sy-subrc.
    sy-subrc = temp378.
    sy-tabix = temp379.
    IF NOT temp377 = 0.
      lo_datadescr ?= cl_abap_datadescr=>describe_by_name( `INT4` ).
      ls_comp-name  = `ROW_ID`.
      ls_comp-type ?= lo_datadescr.
      INSERT ls_comp INTO TABLE lt_comps.
    ENDIF.

    
    strucdescr = cl_abap_structdescr=>create( lt_comps ).

    
    tabdescr = cl_abap_tabledescr=>create( strucdescr ).

    IF mt_data IS NOT BOUND.
      CREATE DATA mt_data TYPE HANDLE tabdescr.
    ENDIF.

    
    ASSIGN mt_data->* TO <fs_target_tab>.

    CLEAR <fs_target_tab>.

    " we don`t want to lose all inputs in row ...
    IF ms_data_row IS NOT BOUND.
      CREATE DATA ms_data_row TYPE HANDLE strucdescr.
    ENDIF.

    
    LOOP AT lt_result_tab INTO result_line.

      CREATE DATA lr_line TYPE HANDLE strucdescr.
      
      ASSIGN lr_line->* TO <fs_line>.

      
      LOOP AT mt_result_desc INTO result_desc.

        
        ASSIGN COMPONENT result_desc-fieldname OF STRUCTURE <fs_line>
               TO <line_content>.

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

    
    

    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      
      UNASSIGN <row>.
      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO <row>.
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD tab_get_where_by_dfies.

    DATA val TYPE string.

    DATA temp380 LIKE LINE OF it_dfies.
    DATA dfies LIKE REF TO temp380.
      FIELD-SYMBOLS <row> TYPE data.
      FIELD-SYMBOLS <value> TYPE any.
        DATA and TYPE string.
    LOOP AT it_dfies REFERENCE INTO dfies.

      IF NOT ( dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field ).
        CONTINUE.
      ENDIF.

      
      ASSIGN ms_data_row->* TO <row>.

      
      UNASSIGN <value>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.

      IF result IS NOT INITIAL.
        
        and = ` AND `.
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

    DATA s_dfies LIKE LINE OF dfies.
      FIELD-SYMBOLS <value> TYPE any.
      DATA lo_value_type TYPE REF TO cl_abap_typedescr.
    LOOP AT dfies INTO s_dfies WHERE keyflag = abap_true.

      
      UNASSIGN <value>.
      ASSIGN COMPONENT s_dfies-fieldname OF STRUCTURE line TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      
      lo_value_type = cl_abap_typedescr=>describe_by_data( <value> ).
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
      FIELD-SYMBOLS <e071> TYPE any.
      FIELD-SYMBOLS <t_e071k> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <t_e071> TYPE STANDARD TABLE.
      DATA r_e071k TYPE REF TO data.
      DATA r_e071 TYPE REF TO data.
      DATA fb1 TYPE c LENGTH 27.
      DATA fb2 TYPE c LENGTH 25.
    IF check_abap_cloud( ) IS NOT INITIAL.

    ELSE.

      
      
      

      " We need to set the MANDT is necessary
      set_mandt( ir_data ).

      
      r_e071k = _set_e071k( ir_data      = ir_data
                                  iv_tabname   = iv_tabname
                                  is_transport = is_transport ).
      ASSIGN r_e071k->* TO <e071>.
      IF <e071> IS INITIAL.
        RETURN.
      ENDIF.

      
      r_e071 = _set_e071( iv_tabname   = iv_tabname
                                is_transport = is_transport ).

      ASSIGN r_e071k->* TO <t_e071k>.
      ASSIGN r_e071->* TO <t_e071>.

      
      fb1 = 'TR_APPEND_TO_COMM_OBJS_KEYS'.
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

      
      fb2 = 'TR_SORT_AND_COMPRESS_COMM'.
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

    DATA t_comp TYPE abap_component_tab.
        DATA struct_desc TYPE REF TO cl_abap_structdescr.
        DATA table_desc TYPE REF TO cl_abap_tabledescr.
        DATA x TYPE REF TO cx_root.
    DATA dfies TYPE zabaputil_cl_util_context=>ty_t_dfies.
    t_comp = rtti_get_t_attri_by_table_name( 'E071K' ).

    TRY.

        
        struct_desc = cl_abap_structdescr=>create( t_comp ).

        
        table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                       p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_e071k TYPE HANDLE table_desc.
        CREATE DATA s_e071k TYPE HANDLE struct_desc.

        ASSIGN t_e071k->* TO <t_e071k>.
        ASSIGN s_e071k->* TO <s_e071k>.

        
      CATCH cx_root INTO x.
        " without the E071K line type nothing below can be filled - say why
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |E071K line type could not be built: { x->get_text( ) }|
            previous = x.
    ENDTRY.

    
    dfies = rtti_get_t_dfies_by_table_name( iv_tabname ).

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

    DATA t_comp TYPE abap_component_tab.
        DATA struct_desc_new TYPE REF TO cl_abap_structdescr.
        DATA table_desc_new TYPE REF TO cl_abap_tabledescr.
    t_comp = rtti_get_t_attri_by_table_name( 'E071' ).

    TRY.

        
        struct_desc_new = cl_abap_structdescr=>create( t_comp ).

        
        table_desc_new = cl_abap_tabledescr=>create( p_line_type  = struct_desc_new
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

    DATA table_name TYPE c LENGTH 4.
        DATA t_comp TYPE abap_component_tab.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
        DATA where TYPE string.
    table_name = 'E070'.

    TRY.
        
        t_comp = rtti_get_t_attri_by_table_name( table_name ).

        
        new_struct_desc = cl_abap_structdescr=>create( t_comp ).

        
        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA lo_tab TYPE HANDLE new_table_desc.
        CREATE DATA lo_line TYPE HANDLE new_struct_desc.

        ASSIGN lo_tab->* TO <table>.
        ASSIGN lo_line->* TO <line>.

        
        where =
        |( TRFUNCTION EQ 'Q' ) AND ( TRSTATUS EQ 'D' ) AND ( KORRDEV EQ 'CUST' ) AND ( AS4USER EQ '{ sy-uname }' )|.

        SELECT trkorr
               trfunction
               trstatus
               tarsystem
               korrdev
               as4user
               as4date
               as4time
               strkorr
          FROM (table_name) INTO TABLE <table>
          WHERE (where)
          .
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
      DATA lo_tab TYPE REF TO data.
      DATA lo_line TYPE REF TO data.
      FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <value> TYPE any.
      DATA table_name TYPE c LENGTH 4.
          DATA t_comp TYPE abap_component_tab.
          DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
          DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
          DATA index TYPE i.
          DATA line LIKE LINE OF mt_data.
              DATA where TYPE string.
        DATA temp381 TYPE sxco_transport.
        DATA lv_trkorr LIKE temp381.
        DATA lv_logon_langu TYPE abap_bool.
        DATA temp25 TYPE xsdboolean.
        FIELD-SYMBOLS <ls_data> LIKE LINE OF mt_data.

    IF check_abap_cloud( ) IS NOT INITIAL.

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

      
      

      
      
      

      _read_e070( CHANGING mt_data = mt_data ).
      " no task - an empty dynamic WHERE would read every text of E07T
      IF mt_data IS INITIAL.
        RETURN.
      ENDIF.

      
      table_name = 'E07T'.

      TRY.
          
          t_comp = rtti_get_t_attri_by_table_name( table_name ).

          
          new_struct_desc = cl_abap_structdescr=>create( t_comp ).

          
          new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                             p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA lo_tab TYPE HANDLE new_table_desc.
          CREATE DATA lo_line TYPE HANDLE new_struct_desc.

          ASSIGN lo_tab->* TO <table>.
          ASSIGN lo_line->* TO <line>.

          
          index = 0.

          " the description shown is the one of the request
          
          LOOP AT mt_data INTO line.
            index = index + 1.
            IF index = 1.
              
              where = |TRKORR EQ { filter_sql_quote( line-transport ) }|.
            ELSE.
              where = |{ where } OR TRKORR EQ { filter_sql_quote( line-transport ) }|.
            ENDIF.
            where = |( { where } )|.
          ENDLOOP.

          SELECT trkorr
                 langu
                 as4text
            FROM (table_name) INTO TABLE <table>
            WHERE (where)
            .
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
        
        temp381 = <value>.
        
        lv_trkorr = temp381.

        UNASSIGN <value>.
        ASSIGN COMPONENT 'LANGU' OF STRUCTURE <line> TO <value>.
        
        
        temp25 = boolc( <value> IS ASSIGNED AND <value> = sy-langu ).
        lv_logon_langu = temp25.

        UNASSIGN <value>.
        ASSIGN COMPONENT 'AS4TEXT' OF STRUCTURE <line> TO <value>.
        IF <value> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        
        LOOP AT mt_data ASSIGNING <ls_data> WHERE transport = lv_trkorr.
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
      FIELD-SYMBOLS <row> TYPE any.

    ASSIGN ir_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      
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
      DATA lo_filter TYPE REF TO object.
      DATA lo_db TYPE REF TO object.
      TYPES temp11 TYPE STANDARD TABLE OF REF TO object.
DATA lt_logs TYPE temp11.
      DATA lv_class TYPE string.
          DATA temp382 TYPE string.
          DATA lv_obj_f LIKE temp382.
          DATA temp383 TYPE string.
          DATA lv_sub_f LIKE temp383.
          DATA temp384 TYPE string.
          DATA lv_id_f LIKE temp384.
            DATA temp385 TYPE d.
            DATA lv_from LIKE temp385.
            DATA temp386 TYPE d.
            DATA lv_to LIKE temp386.
          DATA lo_log LIKE LINE OF lt_logs.
            DATA temp387 TYPE ty_s_bal_header.
            DATA ls_hdr_c LIKE temp387.
                DATA lo_header TYPE REF TO object.
          DATA lx_search_c TYPE REF TO cx_root.
    DATA lr_filter TYPE REF TO data.
    DATA lr_headers TYPE REF TO data.
    FIELD-SYMBOLS <filter> TYPE any.
    FIELD-SYMBOLS <headers> TYPE ANY TABLE.
    FIELD-SYMBOLS <header> TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.
          DATA temp388 TYPE d.
          DATA temp669 TYPE d.
          DATA temp389 TYPE ty_s_bal_header.
          DATA ls_hdr LIKE temp389.
        DATA lx_search TYPE REF TO cx_root.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: use CL_BALI_LOG_FILTER + CL_BALI_LOG_DB
      
      
      

      

      TRY.
          
          IF object IS NOT INITIAL.
            temp382 = object.
          ELSE.
            temp382 = ``.
          ENDIF.
          
          lv_obj_f = temp382.
          
          IF subobject IS NOT INITIAL.
            temp383 = subobject.
          ELSE.
            temp383 = ``.
          ENDIF.
          
          lv_sub_f = temp383.
          
          IF id IS NOT INITIAL.
            temp384 = id.
          ELSE.
            temp384 = ``.
          ENDIF.
          
          lv_id_f = temp384.
          lo_filter = bal_cloud_build_filter( object    = lv_obj_f
                                              subobject = lv_sub_f
                                              id        = lv_id_f ).

          IF date_from IS NOT INITIAL OR date_to IS NOT INITIAL.
            
            IF date_from IS NOT INITIAL.
              temp385 = date_from.
            ELSE.
              temp385 = '19000101'.
            ENDIF.
            
            lv_from = temp385.
            
            IF date_to IS NOT INITIAL.
              temp386 = date_to.
            ELSE.
              temp386 = sy-datum.
            ENDIF.
            
            lv_to = temp386.
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

          
          LOOP AT lt_logs INTO lo_log.
            
            CLEAR temp387.
            
            ls_hdr_c = temp387.
            TRY.
                
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

          
        CATCH cx_root INTO lx_search_c.
          " a failed search is no empty result - the on-premise branch and
          " bal_read raise the same way
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_search_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use BAL_DB_SEARCH
    
    
    
    " generic: BALHDR_T is read by bal_std_load_handles the same way
    
    
    

    TRY.
        lr_filter = bal_std_build_filter( object    = object
                                          subobject = subobject
                                          id        = id ).
        ASSIGN lr_filter->* TO <filter>.

        IF date_from IS NOT INITIAL OR date_to IS NOT INITIAL.
          
          IF date_from IS NOT INITIAL.
            temp388 = date_from.
          ELSE.
            temp388 = '19000101'.
          ENDIF.
          
          IF date_to IS NOT INITIAL.
            temp669 = date_to.
          ELSE.
            temp669 = sy-datum.
          ENDIF.
          bal_std_filter_add( EXPORTING comp   = `ALDATE`
                                        value  = temp388
                                        option = `BT`
                                        high   = temp669
                              CHANGING  filter = <filter> ).
        ENDIF.
        bal_std_filter_add( EXPORTING comp   = `ALUSER`
                                      value  = user
                            CHANGING  filter = <filter> ).

        lr_headers = bal_std_search( <filter> ).
        " LOG_NOT_FOUND: nothing matches the filter
        IF lr_headers IS NOT BOUND.
          RETURN.
        ENDIF.
        ASSIGN lr_headers->* TO <headers>.

        LOOP AT <headers> ASSIGNING <header>.
          
          CLEAR temp389.
          
          ls_hdr = temp389.
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

        
      CATCH cx_root INTO lx_search.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_search.
    ENDTRY.

  ENDMETHOD.

  METHOD bal_read_latest.

    DATA lt_msgs TYPE zabaputil_cl_util_context=>ty_t_msg.
      FIELD-SYMBOLS <temp390> LIKE LINE OF lt_msgs.
      DATA temp391 LIKE sy-tabix.
    lt_msgs = bal_read( object    = object
                              subobject = subobject
                              id        = id ).
    " the logs come in the order of their handles, not of their age: the
    " newest message is the one with the latest time stamp (filled
    " on-premise - on ABAP Cloud the order stays as read)
    SORT lt_msgs STABLE BY timestampl.
    IF lt_msgs IS NOT INITIAL.
      
      
      temp391 = sy-tabix.
      READ TABLE lt_msgs INDEX lines( lt_msgs ) ASSIGNING <temp390>.
      sy-tabix = temp391.
      IF sy-subrc <> 0.
        ASSERT 1 = 0.
      ENDIF.
      result = <temp390>.
    ENDIF.

  ENDMETHOD.

  METHOD bal_delete_before.

    DATA temp392 TYPE d.
    DATA lv_cutoff LIKE temp392.
      DATA lo_filter_c TYPE REF TO object.
      DATA lo_db_c TYPE REF TO object.
      TYPES temp12 TYPE STANDARD TABLE OF REF TO object.
DATA lt_logs_c TYPE temp12.
      DATA lv_cls TYPE string.
          DATA temp393 TYPE string.
          DATA lv_sub_c LIKE temp393.
          DATA temp394 TYPE d.
          DATA lo_log_c LIKE LINE OF lt_logs_c.
          DATA lx_delete_c TYPE REF TO cx_root.
    DATA lr_filter TYPE REF TO data.
    DATA lr_headers TYPE REF TO data.
    FIELD-SYMBOLS <filter> TYPE any.
    FIELD-SYMBOLS <headers> TYPE ANY TABLE.
        DATA temp395 TYPE d.
        DATA lx_delete TYPE REF TO cx_root.
    temp392 = sy-datum - days.
    
    lv_cutoff = temp392.

    " the filter always restricted OBJECT - an initial object matched no
    " log. Without it the old logs of EVERY object would go
    IF object IS INITIAL.
      RETURN.
    ENDIF.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: use CL_BALI_LOG_DB to delete via filter
      
      
      

      

      TRY.
          
          IF subobject IS NOT INITIAL.
            temp393 = subobject.
          ELSE.
            temp393 = ``.
          ENDIF.
          
          lv_sub_c = temp393.
          lo_filter_c = bal_cloud_build_filter( object    = object
                                                subobject = lv_sub_c
                                                id        = `` ).

          
          temp394 = '19000101'.
          CALL METHOD lo_filter_c->(`SET_CREATE_DATE`)
            EXPORTING
              from_date = temp394
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

          
          LOOP AT lt_logs_c INTO lo_log_c.
            CALL METHOD lo_db_c->(`DELETE_LOG`)
              EXPORTING
                log = lo_log_c.
          ENDLOOP.

          IF with_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

          
        CATCH cx_root INTO lx_delete_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_delete_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: BAL_DB_SEARCH, then BAL_DB_DELETE with the headers
    
    
    
    

    TRY.
        lr_filter = bal_std_build_filter( object    = object
                                          subobject = subobject
                                          id        = `` ).
        ASSIGN lr_filter->* TO <filter>.

        
        temp395 = '19000101'.
        bal_std_filter_add( EXPORTING comp   = `ALDATE`
                                      value  = temp395
                                      option = `BT`
                                      high   = lv_cutoff
                            CHANGING  filter = <filter> ).

        lr_headers = bal_std_search( <filter> ).
        " no log older than the cutoff - nothing to delete
        IF lr_headers IS NOT BOUND.
          RETURN.
        ENDIF.
        ASSIGN lr_headers->* TO <headers>.
        bal_std_delete( headers     = <headers>
                        with_commit = with_commit ).

        
      CATCH cx_root INTO lx_delete.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_delete.
    ENDTRY.

  ENDMETHOD.

  METHOD bal_read_by_type.

    DATA lt_all TYPE zabaputil_cl_util_context=>ty_t_msg.
    DATA ls_msg LIKE LINE OF lt_all.
    lt_all = bal_read( object    = object
                             subobject = subobject
                             id        = id ).

    
    LOOP AT lt_all INTO ls_msg
         WHERE type = msg_type.
      INSERT ls_msg INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD bal_count.

    DATA lt_msgs TYPE zabaputil_cl_util_context=>ty_t_msg.
    lt_msgs = bal_read( object    = object
                              subobject = subobject
                              id        = id ).
    result = lines( lt_msgs ).

  ENDMETHOD.

  METHOD bal_read.
TYPES BEGIN OF ty_item.
TYPES log_item_number TYPE i.
TYPES item TYPE REF TO object.
TYPES END OF ty_item.
      TYPES temp13 TYPE STANDARD TABLE OF ty_item.
DATA lt_items TYPE temp13.
      DATA lo_filter TYPE REF TO object.
      DATA lo_db TYPE REF TO object.
      TYPES temp14 TYPE STANDARD TABLE OF REF TO object.
DATA lt_logs TYPE temp14.
      DATA lv_text TYPE string.
      DATA lv_class TYPE string.
      DATA lv_severity TYPE c LENGTH 1.
      DATA lv_msgid TYPE string.
      DATA lv_msgno TYPE string.
      DATA lv_msgv1 TYPE string.
      DATA lv_msgv2 TYPE string.
      DATA lv_msgv3 TYPE string.
      DATA lv_msgv4 TYPE string.
          DATA lo_log LIKE LINE OF lt_logs.
            DATA temp396 LIKE lt_items.
            DATA ls_item LIKE LINE OF lt_items.
              DATA temp397 TYPE ty_s_msg.
              DATA ls_msg LIKE temp397.
          DATA lx_read_c TYPE REF TO cx_root.
      DATA lv_fm TYPE string.
      DATA lr_handles TYPE REF TO data.
      DATA lr_single TYPE REF TO data.
      DATA lr_msgh TYPE REF TO data.
      DATA lr_msg TYPE REF TO data.
      FIELD-SYMBOLS <handles> TYPE ANY TABLE.
      FIELD-SYMBOLS <handle> TYPE any.
      FIELD-SYMBOLS <single> TYPE ANY TABLE.
      FIELD-SYMBOLS <msgh> TYPE ANY TABLE.
      FIELD-SYMBOLS <mh> TYPE any.
      FIELD-SYMBOLS <msg> TYPE any.
          DATA lx_read TYPE REF TO cx_root.

    IF check_abap_cloud( ) IS NOT INITIAL.

      " Load the persisted logs (incl. items) via the released filter API and map
      " each item back to the framework's z2ui5_cl_util=>ty_s_msg structure with full metadata.
      
      

      
      
      

      
      
      
      
      
      
      
      
      

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

          
          LOOP AT lt_logs INTO lo_log.

            
            CLEAR temp396.
            lt_items = temp396.
            CALL METHOD lo_log->(`GET_ALL_ITEMS`)
              RECEIVING
                item_table = lt_items.

            
            LOOP AT lt_items INTO ls_item.
              IF ls_item-item IS NOT BOUND.
                CONTINUE.
              ENDIF.

              
              CLEAR temp397.
              
              ls_msg = temp397.

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

          
        CATCH cx_root INTO lx_read_c.
          " a failed read is no empty log - as in the on-premise branch
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_read_c.
      ENDTRY.

    ELSE.

      
      
      
      
      
      " generic: the table kinds of the DDIC types BAL_T_LOGH / BAL_T_MSGH
      " are not ours to assume - a SORTED vs STANDARD mismatch is a dump
      
      
      
      
      
      

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

          
        CATCH cx_root INTO lx_read.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_read.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD bal_create.
      DATA lo_header TYPE REF TO object.
      DATA lo_log TYPE REF TO object.
      DATA lo_db TYPE REF TO object.
      DATA lv_class TYPE string.
          DATA lx_create_c TYPE REF TO cx_root.
      DATA lv_fm TYPE string.
      DATA lr_log TYPE REF TO data.
      DATA lr_handle TYPE REF TO data.
      DATA lr_handles TYPE REF TO data.
      FIELD-SYMBOLS <log> TYPE any.
      FIELD-SYMBOLS <handle> TYPE any.
      FIELD-SYMBOLS <handles> TYPE ANY TABLE.
      FIELD-SYMBOLS <comp> TYPE any.
          DATA lx_create TYPE REF TO cx_root.

    IF check_abap_cloud( ) IS NOT INITIAL.

      " ABAP Cloud: released Business Application Log API (cl_bali_*).
      " All access is dynamic so this class still compiles on lower releases.
      " The class names are kept in variables - a string literal inside the
      " dynamic component selector ( '...' )=>( '...' ) is not valid ABAP.
      " Returning parameter names follow the released API (header/log/db_handler).
      
      
      
      

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

          IF with_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

          
        CATCH cx_root INTO lx_create_c.
          " the log was not written - the caller must not see a success
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_create_c.
      ENDTRY.

    ELSE.

      " Standard ABAP / on-premise: classic Business Application Log function modules.
      
      
      
      
      
      
      
      

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
          IF with_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

          
        CATCH cx_root INTO lx_create.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_create.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD bal_update.
      DATA lo_filter TYPE REF TO object.
      DATA lo_db TYPE REF TO object.
      TYPES temp15 TYPE STANDARD TABLE OF REF TO object.
DATA lt_logs TYPE temp15.
      DATA lv_class TYPE string.
          DATA lx_load_c TYPE REF TO cx_root.
          DATA lo_log LIKE LINE OF lt_logs.
          FIELD-SYMBOLS <temp670> LIKE LINE OF lt_logs.
          DATA temp671 LIKE sy-tabix.
          DATA lx_update_c TYPE REF TO cx_root.
      DATA lv_fm TYPE string.
      DATA lr_handles TYPE REF TO data.
      FIELD-SYMBOLS <handles> TYPE ANY TABLE.
      FIELD-SYMBOLS <handle> TYPE any.
          DATA lv_found LIKE abap_false.
            DATA temp26 TYPE xsdboolean.
          DATA lx_update TYPE REF TO cx_root.

    IF check_abap_cloud( ) IS NOT INITIAL.

      " Load the existing log and append items. If no log exists, create a new one.
      
      
      

      

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

          
        CATCH cx_root INTO lx_load_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_load_c.
      ENDTRY.

      IF lt_logs IS INITIAL.
        bal_create( object      = object
                    subobject   = subobject
                    id          = id
                    t_log       = t_log
                    with_commit = with_commit ).
        RETURN.
      ENDIF.

      TRY.
          " the first log the API returns - its order is not documented
          
          
          
          temp671 = sy-tabix.
          READ TABLE lt_logs INDEX 1 ASSIGNING <temp670>.
          sy-tabix = temp671.
          IF sy-subrc <> 0.
            ASSERT 1 = 0.
          ENDIF.
          lo_log = <temp670>.
          bal_cloud_add_items( log   = lo_log
                               t_log = t_log ).

          CALL METHOD lo_db->(`SAVE_LOG`)
            EXPORTING
              log = lo_log.

          IF with_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

          
        CATCH cx_root INTO lx_update_c.
          " no second log with the same messages: the items may be in the
          " first one already - the caller sees the failure instead
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_update_c.
      ENDTRY.

    ELSE.

      " Append the given messages to the newest persisted log; create a new one if none exists.
      
      
      
      

      TRY.
          lr_handles = bal_std_load_handles( object      = object
                                             subobject   = subobject
                                             id          = id
                                             latest_only = abap_true ).
          
          lv_found = abap_false.
          IF lr_handles IS BOUND.
            ASSIGN lr_handles->* TO <handles>.
            
            temp26 = boolc( <handles> IS NOT INITIAL ).
            lv_found = temp26.
          ENDIF.
          IF lv_found = abap_false.
            bal_create( object      = object
                        subobject   = subobject
                        id          = id
                        t_log       = t_log
                        with_commit = with_commit ).
            RETURN.
          ENDIF.

          LOOP AT <handles> ASSIGNING <handle>.
            EXIT.
          ENDLOOP.
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
          IF with_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

          
        CATCH cx_root INTO lx_update.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_update.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD bal_delete.
      DATA lo_filter TYPE REF TO object.
      DATA lo_db TYPE REF TO object.
      TYPES temp16 TYPE STANDARD TABLE OF REF TO object.
DATA lt_logs TYPE temp16.
      DATA lv_class TYPE string.
          DATA lo_log LIKE LINE OF lt_logs.
          DATA lx_delete_c TYPE REF TO cx_root.
      DATA lr_filter TYPE REF TO data.
      DATA lr_headers TYPE REF TO data.
      FIELD-SYMBOLS <filter> TYPE any.
      FIELD-SYMBOLS <headers> TYPE ANY TABLE.
          DATA lx_delete TYPE REF TO cx_root.

    " three initial keys are no filter at all - they matched every log of
    " the client
    IF object IS INITIAL AND subobject IS INITIAL AND id IS INITIAL.
      RETURN.
    ENDIF.

    IF check_abap_cloud( ) IS NOT INITIAL.

      
      
      

      

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

          
          LOOP AT lt_logs INTO lo_log.
            CALL METHOD lo_db->(`DELETE_LOG`)
              EXPORTING
                log = lo_log.
          ENDLOOP.

          IF with_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

          
        CATCH cx_root INTO lx_delete_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_delete_c.
      ENDTRY.

    ELSE.

      " BAL_DB_DELETE has no filter parameter: the logs are searched first
      " and deleted by their headers
      
      
      
      

      TRY.
          lr_filter = bal_std_build_filter( object    = object
                                            subobject = subobject
                                            id        = id ).
          ASSIGN lr_filter->* TO <filter>.

          lr_headers = bal_std_search( <filter> ).
          IF lr_headers IS NOT BOUND.
            RETURN.
          ENDIF.
          ASSIGN lr_headers->* TO <headers>.
          bal_std_delete( headers     = <headers>
                          with_commit = with_commit ).

          
        CATCH cx_root INTO lx_delete.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_delete.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD tr_get_objects.
      FIELD-SYMBOLS <lt_objects> TYPE ANY TABLE.
      FIELD-SYMBOLS <lo_object> TYPE any.
      DATA lo_entry TYPE REF TO object.
          DATA lr_objects_c TYPE REF TO data.
            DATA temp398 TYPE ty_s_tr_object.
            DATA ls_obj_c LIKE temp398.
            DATA lr_key TYPE REF TO data.
            FIELD-SYMBOLS <key> TYPE any.
            FIELD-SYMBOLS <kcmp> TYPE any.
              DATA temp399 TYPE REF TO data.
              DATA temp400 TYPE REF TO data.
              DATA temp401 TYPE REF TO data.
          DATA lx_objects_c TYPE REF TO cx_root.
    DATA lr_objects TYPE REF TO data.
    DATA lr_header TYPE REF TO data.
    FIELD-SYMBOLS <objects> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <object> TYPE any.
    FIELD-SYMBOLS <header> TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.
    DATA lv_fm TYPE string.
          DATA temp402 TYPE ty_s_tr_object.
          DATA ls_obj LIKE temp402.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: XCO_CP_CTS=>TRANSPORT->FOR( )->OBJECTS->ALL->GET( ). The
      " factories are attributes and the methods interface methods - a
      " failure raises instead of reading like a request without objects
      
      
      
      TRY.
          
          lr_objects_c = dyn_call( obj  = dyn_call_obj( obj  = dyn_call_obj( obj  = xco_transport( trkorr )
                                                                                  name = `OBJECTS` )
                                                             name = `ALL` )
                                         name = `GET` ).
          ASSIGN lr_objects_c->* TO <lt_objects>.

          LOOP AT <lt_objects> ASSIGNING <lo_object>.
            lo_entry ?= <lo_object>.
            
            CLEAR temp398.
            
            ls_obj_c = temp398.
            " the key of the entry: PGMID, OBJECT, OBJ_NAME as in E071
            
            lr_key = dyn_call( obj  = lo_entry
                                     name = `GET_KEY` ).
            
            
            ASSIGN lr_key->* TO <key>.
            ASSIGN COMPONENT `PGMID` OF STRUCTURE <key> TO <kcmp>.
            IF sy-subrc = 0.
              
GET REFERENCE OF <kcmp> INTO temp399.
ls_obj_c-pgmid = xco_text( temp399 ).
            ENDIF.
            ASSIGN COMPONENT `OBJECT` OF STRUCTURE <key> TO <kcmp>.
            IF sy-subrc = 0.
              
GET REFERENCE OF <kcmp> INTO temp400.
ls_obj_c-object = xco_text( temp400 ).
            ENDIF.
            ASSIGN COMPONENT `OBJ_NAME` OF STRUCTURE <key> TO <kcmp>.
            IF sy-subrc = 0.
              
GET REFERENCE OF <kcmp> INTO temp401.
ls_obj_c-obj_name = xco_text( temp401 ).
            ENDIF.
            INSERT ls_obj_c INTO TABLE result.
          ENDLOOP.

          
        CATCH cx_root INTO lx_objects_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_objects_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use TR_GET_OBJECTS_OF_REQ_AN_TASKS
    
    
    
    
    
    
    

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
          
          CLEAR temp402.
          
          ls_obj = temp402.
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
      TYPES temp17 TYPE STANDARD TABLE OF REF TO object WITH DEFAULT KEY.
DATA lt_filter TYPE temp17.
      DATA lo_tr TYPE REF TO object.
      FIELD-SYMBOLS <lt_transports> TYPE ANY TABLE.
      FIELD-SYMBOLS <lo_tr> TYPE any.
      FIELD-SYMBOLS <prop> TYPE any.
      FIELD-SYMBOLS <pcomp> TYPE any.
          DATA lv_user_c TYPE c LENGTH 12.
          DATA temp403 LIKE lv_user_c.
          DATA lo_tr_filter TYPE REF TO object.
          DATA lo_constraint TYPE REF TO object.
          DATA lo_query TYPE REF TO object.
          DATA lr_transports TYPE REF TO data.
            DATA temp404 TYPE ty_s_tr_request.
            DATA ls_req_c LIKE temp404.
            DATA lr_prop TYPE REF TO data.
          DATA lx_requests_c TYPE REF TO cx_root.
    DATA lv_user TYPE c LENGTH 12.
    DATA lv_type TYPE c LENGTH 1.
    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.
        DATA lv_tab1 TYPE string.
        DATA lv_tab2 TYPE string.
        DATA lv_where TYPE string.
        DATA lt_comp TYPE abap_component_tab.
        DATA lo_struct TYPE REF TO cl_abap_structdescr.
        DATA lo_table TYPE REF TO cl_abap_tabledescr.
          DATA temp405 TYPE ty_s_tr_request.
          DATA ls_req LIKE temp405.
TYPES BEGIN OF ty_s_text.
TYPES trkorr TYPE c LENGTH 20.
TYPES as4text TYPE c LENGTH 60.
TYPES END OF ty_s_text.
        TYPES temp18 TYPE STANDARD TABLE OF ty_s_text WITH DEFAULT KEY.
DATA lt_text TYPE temp18.
        TYPES temp19 TYPE STANDARD TABLE OF ty_s_text WITH DEFAULT KEY.
DATA lt_part TYPE temp19.
        DATA lv_keys TYPE string.
        DATA lv_count TYPE i.
        DATA lv_index TYPE i.
          DATA temp406 TYPE string.
        DATA temp407 LIKE LINE OF result.
        DATA lr_req LIKE REF TO temp407.
          DATA ls_text LIKE LINE OF lt_text.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: XCO_CP_CTS=>TRANSPORTS->WHERE( owner, status modifiable )
      " ->RESOLVE( request ), as the template in bus_tr_read
      

      
      
      
      
      
      TRY.
          
          
          IF user IS NOT INITIAL.
            temp403 = user.
          ELSE.
            temp403 = sy-uname.
          ENDIF.
          lv_user_c = temp403.

          
          lo_tr_filter = dyn_static( `XCO_CP_TRANSPORT=>FILTER` ).
          
          lo_constraint = dyn_call_obj( obj  = dyn_static( `XCO_CP_ABAP_SQL=>CONSTRAINT` )
                                              name = `EQUAL`
                                              arg  = lv_user_c ).
          INSERT dyn_call_obj( obj  = lo_tr_filter
                               name = `OWNER`
                               arg  = lo_constraint ) INTO TABLE lt_filter.
          INSERT dyn_call_obj( obj  = lo_tr_filter
                               name = `STATUS`
                               arg  = dyn_call_obj( obj  = dyn_static( `XCO_CP_TRANSPORT=>STATUS` )
                                                    name = `MODIFIABLE` ) ) INTO TABLE lt_filter.

          
          lo_query = dyn_call_obj( obj  = dyn_static( `XCO_CP_CTS=>TRANSPORTS` )
                                         name = `WHERE`
                                         arg  = lt_filter ).
          
          lr_transports = dyn_call( obj  = lo_query
                                          name = `RESOLVE`
                                          arg  = dyn_call_obj( obj  = dyn_static( `XCO_CP_TRANSPORT=>RESOLUTION` )
                                                               name = `REQUEST` ) ).
          ASSIGN lr_transports->* TO <lt_transports>.

          LOOP AT <lt_transports> ASSIGNING <lo_tr>.
            lo_tr ?= <lo_tr>.
            
            CLEAR temp404.
            temp404-owner = lv_user_c.
            temp404-status = `D`.
            
            ls_req_c = temp404.
            ls_req_c-trkorr = xco_text( dyn_call( obj  = lo_tr
                                                  name = `VALUE` ) ).
            
            lr_prop = dyn_call( obj  = dyn_call_obj( obj  = lo_tr
                                                           name = `PROPERTIES` )
                                      name = `GET` ).
            ASSIGN lr_prop->* TO <prop>.
            ASSIGN COMPONENT `SHORT_DESCRIPTION` OF STRUCTURE <prop> TO <pcomp>.
            IF sy-subrc = 0.
              ls_req_c-description = <pcomp>.
            ENDIF.
            ls_req_c-type = xco_text( dyn_call( obj  = lo_tr
                                                name = `GET_TYPE` ) ).
            IF request_type IS NOT INITIAL AND ls_req_c-type <> request_type.
              CONTINUE.
            ENDIF.
            INSERT ls_req_c INTO TABLE result.
          ENDLOOP.

          
        CATCH cx_root INTO lx_requests_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_requests_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: dynamic SELECT from E070, the texts from E07T
    
    
    
    
    
    

    TRY.
        lv_user = user.
        lv_type = request_type.

        
        lv_tab1 = `E070`.
        
        lv_tab2 = `E07T`.
        
        lv_where = |AS4USER = { filter_sql_quote( lv_user ) } AND TRSTATUS IN ('D','L')|.

        " First read transports from E070
        
        lt_comp = rtti_get_t_attri_by_table_name( lv_tab1 ).
        
        lo_struct = cl_abap_structdescr=>create( lt_comp ).
        
        lo_table = cl_abap_tabledescr=>create( lo_struct ).
        CREATE DATA lr_data TYPE HANDLE lo_table.
        ASSIGN lr_data->* TO <tab>.

        SELECT trkorr as4user trstatus trfunction
          FROM (lv_tab1) INTO CORRESPONDING FIELDS OF TABLE <tab>
          WHERE (lv_where)
          .

        LOOP AT <tab> ASSIGNING <row>.
          
          CLEAR temp405.
          
          ls_req = temp405.
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

          INSERT ls_req INTO TABLE result.
        ENDLOOP.

        " the texts in one SELECT per 50 requests instead of one per request
        
        

        

        
        lv_keys = ``.
        
        lv_count = 0.
        
        lv_index = 0.
        LOOP AT result INTO ls_req.
          lv_index = lv_index + 1.
          
          IF lv_keys IS INITIAL.
            temp406 = |TRKORR = { filter_sql_quote( ls_req-trkorr ) }|.
          ELSE.
            temp406 = |{ lv_keys } OR TRKORR = { filter_sql_quote( ls_req-trkorr ) }|.
          ENDIF.
          lv_keys = temp406.
          lv_count = lv_count + 1.
          IF lv_count = 50 OR lv_index = lines( result ).
            lv_where = |LANGU = { filter_sql_quote( sy-langu ) } AND ( { lv_keys } )|.
            CLEAR lt_part.
            SELECT trkorr as4text
              FROM (lv_tab2) INTO CORRESPONDING FIELDS OF TABLE lt_part
              WHERE (lv_where)
              .
            INSERT LINES OF lt_part INTO TABLE lt_text.
            CLEAR lv_keys.
            lv_count = 0.
          ENDIF.
        ENDLOOP.

        
        
        LOOP AT result REFERENCE INTO lr_req.
          
          LOOP AT lt_text INTO ls_text WHERE trkorr = lr_req->trkorr.
            lr_req->description = ls_text-as4text.
            EXIT.
          ENDLOOP.
        ENDLOOP.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_get_description.
      FIELD-SYMBOLS <prop_d> TYPE any.
      FIELD-SYMBOLS <pcomp_d> TYPE any.
          DATA lr_prop_d TYPE REF TO data.
          DATA lx_descr_c TYPE REF TO cx_root.
    DATA lv_trkorr TYPE c LENGTH 20.
        DATA lv_tab TYPE string.
        DATA lv_where TYPE string.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: XCO_CP_CTS=>TRANSPORT->FOR( )->PROPERTIES( )->GET( )
      
      
      TRY.
          
          lr_prop_d = dyn_call( obj  = dyn_call_obj( obj  = xco_transport( trkorr )
                                                           name = `PROPERTIES` )
                                      name = `GET` ).
          ASSIGN lr_prop_d->* TO <prop_d>.
          ASSIGN COMPONENT `SHORT_DESCRIPTION` OF STRUCTURE <prop_d> TO <pcomp_d>.
          IF sy-subrc = 0.
            result = <pcomp_d>.
          ENDIF.
          
        CATCH cx_root INTO lx_descr_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_descr_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard: dynamic SELECT from E07T
    
    lv_trkorr = trkorr.

    TRY.
        
        lv_tab = `E07T`.
        
        lv_where = |TRKORR = { filter_sql_quote( lv_trkorr ) } AND LANGU = { filter_sql_quote( sy-langu ) }|.

        SELECT SINGLE as4text
          FROM (lv_tab) INTO result
          WHERE (lv_where)
          .
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_is_released.
          DATA lo_status_r TYPE REF TO object.
          DATA lo_released TYPE REF TO object.
          DATA temp27 TYPE xsdboolean.
                DATA temp408 LIKE REF TO lo_released.
DATA temp672 LIKE REF TO lo_status_r.
DATA temp28 TYPE xsdboolean.
          DATA lx_status_c TYPE REF TO cx_root.
    DATA lv_trkorr TYPE c LENGTH 20.
    DATA lv_status TYPE c LENGTH 1.
        DATA lv_tab TYPE string.
        DATA lv_where TYPE string.
        DATA temp29 TYPE xsdboolean.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: XCO_CP_CTS=>TRANSPORT->FOR( )->GET_STATUS( ), compared with
      " XCO_CP_TRANSPORT=>STATUS->RELEASED. A failure raises - it read as
      " "not released"
      TRY.
          
          lo_status_r = dyn_call_obj( obj  = xco_transport( trkorr )
                                            name = `GET_STATUS` ).
          
          lo_released = dyn_call_obj( obj  = dyn_static( `XCO_CP_TRANSPORT=>STATUS` )
                                            name = `RELEASED` ).
          
          temp27 = boolc( lo_status_r = lo_released ).
          result = temp27.
          IF result = abap_false.
            " not the same instance - the same value then
            TRY.
                
                GET REFERENCE OF lo_released INTO temp408.

GET REFERENCE OF lo_status_r INTO temp672.

temp28 = boolc( xco_text( temp672 ) = xco_text( temp408 ) ).
result = temp28.
              CATCH zabaputil_cx_util_error ##NO_HANDLER.
            ENDTRY.
          ENDIF.
          
        CATCH cx_root INTO lx_status_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_status_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard: dynamic SELECT from E070
    
    
    lv_trkorr = trkorr.

    TRY.
        
        lv_tab = `E070`.
        
        lv_where = |TRKORR = { filter_sql_quote( lv_trkorr ) }|.

        SELECT SINGLE trstatus
          FROM (lv_tab) INTO lv_status
          WHERE (lv_where)
          .
        
        temp29 = boolc( lv_status = `R` ).
        result = temp29.
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
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

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

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_create.
        DATA lr_header TYPE REF TO data.
        FIELD-SYMBOLS <header> TYPE any.
        FIELD-SYMBOLS <trkorr> TYPE any.
        DATA lv_class TYPE string.
        DATA x TYPE REF TO cx_root.

    " Create an empty transport request (default type `T` = transport of copies).
    " Uses the released class CL_ADT_CTS_MANAGEMENT, available on standard ABAP
    " (>= 7.51) and ABAP Cloud. Called dynamically so the source stays compilable
    " on all targets - on releases where the class is missing a clean
    " z2ui5_cx_util_error is raised instead of a short dump.
    TRY.

        
        
        
        

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

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_release.
        DATA lo_api TYPE REF TO object.
        DATA lv_class TYPE string.
        DATA x TYPE REF TO cx_root.

    " Release a transport request via the released CTS REST API
    " (CL_CTS_REST_API_FACTORY). Works on standard ABAP and ABAP Cloud.
    TRY.

        
        

        lv_class = `CL_CTS_REST_API_FACTORY`.
        CALL METHOD (lv_class)=>(`CREATE_INSTANCE`)
          RECEIVING
            result = lo_api.

        CALL METHOD lo_api->(`RELEASE`)
          EXPORTING
            iv_trkorr       = trkorr
            iv_ignore_locks = ignore_locks.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_copy_objects.
        DATA lr_headers TYPE REF TO data.
        FIELD-SYMBOLS <headers> TYPE ANY TABLE.
        FIELD-SYMBOLS <header> TYPE any.
        FIELD-SYMBOLS <trkorr> TYPE any.
        FIELD-SYMBOLS <strkorr> TYPE any.
        DATA lv_fm TYPE string.
        DATA lx_known TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    " Copying objects between requests relies on the classic transport
    " functions (TR_COPY_COMM) which are not released on ABAP Cloud.
    IF check_abap_cloud( ) IS NOT INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `tr_copy_objects is not supported on ABAP Cloud`.
    ENDIF.

    " Copy all objects/commands of a source request (and its tasks) into a
    " target request via the classic transport functions. Called dynamically
    " so the statement stays compilable on ABAP Cloud as well, even though the
    " function modules only exist on-premise.
    TRY.

        
        
        
        
        
        

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

        
      CATCH zabaputil_cx_util_error INTO lx_known.
        RAISE EXCEPTION lx_known.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_import.
        DATA lv_system TYPE c LENGTH 8.
        DATA lv_client TYPE c LENGTH 3.
        DATA lv_retcode TYPE c LENGTH 4.
        DATA lr_exc TYPE REF TO data.
        FIELD-SYMBOLS <exc> TYPE any.
        DATA lv_fm TYPE string.
        DATA lx_known TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    " Importing transports via TMS (TMS_MGR_*) is not available on ABAP Cloud.
    IF check_abap_cloud( ) IS NOT INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `tr_import is not supported on ABAP Cloud`.
    ENDIF.

    " Import a transport request into a target system via TMS. The target
    " system may be given as `SYSTEM` or `SYSTEM.CLIENT`; an explicit client
    " parameter takes precedence, otherwise the current client is used.
    TRY.

        
        
        
        
        
        

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

        
      CATCH zabaputil_cx_util_error INTO lx_known.
        RAISE EXCEPTION lx_known.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD tr_check_status.
        DATA lr_settings TYPE REF TO data.
        DATA lr_cofile TYPE REF TO data.
        DATA lr_sysline TYPE REF TO data.
        FIELD-SYMBOLS <settings> TYPE any.
        FIELD-SYMBOLS <systems> TYPE ANY TABLE.
        FIELD-SYMBOLS <sysline> TYPE any.
        FIELD-SYMBOLS <cofile> TYPE any.
        FIELD-SYMBOLS <comp> TYPE any.
        DATA lv_fm TYPE string.
        DATA lx_known TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    " Reading the transport log (TR_READ_GLOBAL_INFO_OF_REQUEST) is not
    " available on ABAP Cloud.
    IF check_abap_cloud( ) IS NOT INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `tr_check_status is not supported on ABAP Cloud`.
    ENDIF.

    " Read the import status (imported flag + return code) of a request in a
    " given system via the classic transport log API.
    TRY.

        
        
        
        
        
        
        
        
        

        CREATE DATA lr_settings TYPE (`CTSLG_SETTINGS`).
        ASSIGN lr_settings->* TO <settings>.
        ASSIGN COMPONENT `SYSTEMS` OF STRUCTURE <settings> TO <systems>.

        CREATE DATA lr_sysline LIKE LINE OF <systems>.
        ASSIGN lr_sysline->* TO <sysline>.
        <sysline> = system.
        INSERT <sysline> INTO TABLE <systems>.

        CREATE DATA lr_cofile TYPE (`CTSLG_COFILE`).
        ASSIGN lr_cofile->* TO <cofile>.

        " ERROR_MESSAGE: a MESSAGE TYPE E of the module would end the
        " request where no CATCH reaches it
        lv_fm = `TR_READ_GLOBAL_INFO_OF_REQUEST`.
        CALL FUNCTION lv_fm
          EXPORTING
            iv_trkorr     = trkorr
            is_settings   = <settings>
          IMPORTING
            es_cofile     = <cofile>
          EXCEPTIONS
            error_message = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |TR_READ_GLOBAL_INFO_OF_REQUEST failed (sy-subrc { sy-subrc })|.
        ENDIF.

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

        
      CATCH zabaputil_cx_util_error INTO lx_known.
        RAISE EXCEPTION lx_known.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD dyn_call.

    DATA lo_class  TYPE REF TO cl_abap_objectdescr.
    DATA lo_owner  TYPE REF TO cl_abap_objectdescr.
    DATA lo_type   TYPE REF TO cl_abap_datadescr.
    DATA lo_table  TYPE REF TO cl_abap_tabledescr.
    DATA lv_member TYPE string.
    DATA lt_param  TYPE abap_parmbind_tab.
    DATA lr_arg    TYPE REF TO data.
    DATA lr_line   TYPE REF TO data.
    FIELD-SYMBOLS <ls_method> TYPE abap_methdescr.
    FIELD-SYMBOLS <ls_param>  TYPE abap_parmdescr.
    FIELD-SYMBOLS <attr>      TYPE any.
    FIELD-SYMBOLS <arg>       TYPE any.
    FIELD-SYMBOLS <lt_arg>    TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_in>     TYPE ANY TABLE.
    FIELD-SYMBOLS <line>      TYPE any.

    DATA lv_name TYPE string.
    DATA temp409 TYPE string.
    DATA lv_class_name TYPE string.
      DATA ls_intf LIKE LINE OF lo_class->interfaces.
        DATA lv_static TYPE string.
          DATA lv_attr TYPE string.
      DATA temp410 LIKE sy-subrc.
        DATA temp411 TYPE abap_parmbind.
                DATA lo_line_type TYPE REF TO cl_abap_datadescr.
                FIELD-SYMBOLS <ls_in> TYPE ANY.
        DATA temp412 TYPE abap_parmbind.
    lv_name = to_upper( name ).
    
    temp409 = class_name.
    
    lv_class_name = to_upper( temp409 ).
    IF obj IS BOUND.
      lo_class ?= cl_abap_typedescr=>describe_by_object_ref( obj ).
    ELSE.
      lo_class ?= cl_abap_typedescr=>describe_by_name( lv_class_name ).
    ENDIF.
    lo_owner = lo_class.

    READ TABLE lo_class->methods WITH KEY name = lv_name ASSIGNING <ls_method>.
    IF sy-subrc = 0.
      lv_member = lv_name.
    ELSE.
      
      LOOP AT lo_class->interfaces INTO ls_intf.
        lo_owner ?= cl_abap_typedescr=>describe_by_name( ls_intf-name ).
        READ TABLE lo_owner->methods WITH KEY name = lv_name ASSIGNING <ls_method>.
        IF sy-subrc = 0.
          lv_member = |{ ls_intf-name }~{ lv_name }|.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lv_member IS INITIAL.
      " no method - an attribute (XCO: `transport->objects->all`)
      IF obj IS BOUND.
        ASSIGN obj->(lv_name) TO <attr>.
      ELSE.
        
        lv_static = |{ lv_class_name }=>{ lv_name }|.
        ASSIGN (lv_static) TO <attr>.
      ENDIF.
      IF sy-subrc <> 0 AND obj IS BOUND.
        LOOP AT lo_class->interfaces INTO ls_intf.
          
          lv_attr = |{ ls_intf-name }~{ lv_name }|.
          ASSIGN obj->(lv_attr) TO <attr>.
          IF sy-subrc = 0.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF <attr> IS NOT ASSIGNED.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |{ lo_class->get_relative_name( ) } has no member { lv_name }|.
      ENDIF.
      GET REFERENCE OF <attr> INTO result.
      RETURN.
    ENDIF.

    lt_param = t_param.
    LOOP AT <ls_method>-parameters ASSIGNING <ls_param>.

      
      READ TABLE lt_param WITH KEY name = <ls_param>-name TRANSPORTING NO FIELDS.
      temp410 = sy-subrc.
      IF temp410 = 0.
        CONTINUE.
      ENDIF.

      IF <ls_param>-parm_kind = cl_abap_objectdescr=>returning.
        lo_type = lo_owner->get_method_parameter_type( p_method_name    = <ls_method>-name
                                                       p_parameter_name = <ls_param>-name ).
        CREATE DATA result TYPE HANDLE lo_type.
        
        CLEAR temp411.
        temp411-name = <ls_param>-name.
        temp411-kind = cl_abap_objectdescr=>receiving.
        temp411-value = result.
        INSERT temp411 INTO TABLE lt_param.

      ELSEIF <ls_param>-parm_kind = cl_abap_objectdescr=>importing
          AND arg IS SUPPLIED
          AND lr_arg IS NOT BOUND
          AND ( ( arg_name IS INITIAL AND <ls_param>-is_optional = abap_false )
             OR <ls_param>-name = to_upper( arg_name ) ).
        lo_type = lo_owner->get_method_parameter_type( p_method_name    = <ls_method>-name
                                                       p_parameter_name = <ls_param>-name ).
        TRY.
            CREATE DATA lr_arg TYPE HANDLE lo_type.
            ASSIGN lr_arg->* TO <arg>.
            CASE lo_type->kind.
              WHEN cl_abap_typedescr=>kind_ref.
                <arg> ?= arg.
              WHEN cl_abap_typedescr=>kind_table.
                " a table of filters and the like: one cast per line
                lo_table ?= lo_type.
                
                lo_line_type = lo_table->get_table_line_type( ).
                ASSIGN <arg> TO <lt_arg>.
                ASSIGN arg TO <lt_in>.
                
                LOOP AT <lt_in> ASSIGNING <ls_in>.
                  CREATE DATA lr_line TYPE HANDLE lo_line_type.
                  ASSIGN lr_line->* TO <line>.
                  IF lo_line_type->kind = cl_abap_typedescr=>kind_ref.
                    <line> ?= <ls_in>.
                  ELSE.
                    MOVE-CORRESPONDING <ls_in> TO <line>.
                  ENDIF.
                  INSERT <line> INTO TABLE <lt_arg>.
                ENDLOOP.
              WHEN OTHERS.
                <arg> = arg.
            ENDCASE.
          CATCH cx_sy_create_data_error.
            " a generic parameter (CLIKE, ANY): the value as it is
            GET REFERENCE OF arg INTO lr_arg.
        ENDTRY.
        
        CLEAR temp412.
        temp412-name = <ls_param>-name.
        temp412-kind = cl_abap_objectdescr=>exporting.
        temp412-value = lr_arg.
        INSERT temp412 INTO TABLE lt_param.
      ENDIF.

    ENDLOOP.

    IF obj IS BOUND.
      CALL METHOD obj->(lv_member)
        PARAMETER-TABLE lt_param.
    ELSE.
      CALL METHOD (lv_class_name)=>(lv_member)
        PARAMETER-TABLE lt_param.
    ENDIF.

  ENDMETHOD.

  METHOD dyn_call_obj.

    FIELD-SYMBOLS <obj> TYPE any.
      DATA lr_value TYPE REF TO data.

    IF arg IS SUPPLIED.
      
      lr_value = dyn_call( obj        = obj
                                 class_name = class_name
                                 name       = name
                                 arg        = arg
                                 arg_name   = arg_name
                                 t_param    = t_param ).
    ELSE.
      lr_value = dyn_call( obj        = obj
                           class_name = class_name
                           name       = name
                           t_param    = t_param ).
    ENDIF.
    IF lr_value IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |{ name } returns nothing|.
    ENDIF.
    ASSIGN lr_value->* TO <obj>.
    result ?= <obj>.

  ENDMETHOD.

  METHOD dyn_static.

    FIELD-SYMBOLS <obj> TYPE any.

    ASSIGN (val) TO <obj>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |{ val } is not available|.
    ENDIF.
    result ?= <obj>.

  ENDMETHOD.

  METHOD xco_transport.

    DATA lv_trkorr TYPE c LENGTH 20.

    lv_trkorr = trkorr.
    result = dyn_call_obj( obj  = dyn_static( `XCO_CP_CTS=>TRANSPORT` )
                           name = `FOR`
                           arg  = lv_trkorr ).

  ENDMETHOD.

  METHOD xco_text.

    DATA lo_obj TYPE REF TO object.
    FIELD-SYMBOLS <value> TYPE any.

    IF val IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN val->* TO <value>.

    IF cl_abap_typedescr=>describe_by_data( <value> )->kind <> cl_abap_typedescr=>kind_ref.
      result = <value>.
      RETURN.
    ENDIF.

    lo_obj ?= <value>.
    IF lo_obj IS NOT BOUND.
      RETURN.
    ENDIF.
    TRY.
        result = xco_text( dyn_call( obj  = lo_obj
                                     name = `VALUE` ) ).
      CATCH zabaputil_cx_util_error.
        result = xco_text( dyn_call( obj  = lo_obj
                                     name = `NAME` ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD bal_cloud_add_items.

    DATA lo_item  TYPE REF TO object.
    DATA lv_msgty TYPE c LENGTH 1.
    DATA lv_class TYPE string.

    DATA ls_log LIKE LINE OF t_log.
    LOOP AT t_log INTO ls_log.

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

    DATA ls_log LIKE LINE OF t_log.
    LOOP AT t_log INTO ls_log.

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
    DATA lr_latest  TYPE REF TO data.
    DATA lr_newest  TYPE REF TO data.
    DATA lv_newest  TYPE string.
    DATA lv_key     TYPE string.
    FIELD-SYMBOLS <filter>  TYPE any.
    " generic: BALHDR_T and BAL_T_LOGH are typed STANDARD in bal_search and
    " bal_read - with SORTED here one of the two sides could only dump
    FIELD-SYMBOLS <headers> TYPE ANY TABLE.
    FIELD-SYMBOLS <latest>  TYPE ANY TABLE.
    FIELD-SYMBOLS <header>  TYPE any.
    FIELD-SYMBOLS <comp>    TYPE any.
    FIELD-SYMBOLS <handles> TYPE ANY TABLE.

    lr_filter = bal_std_build_filter( object    = object
                                      subobject = subobject
                                      id        = id ).
    ASSIGN lr_filter->* TO <filter>.

    lr_headers = bal_std_search( <filter> ).
    IF lr_headers IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_headers->* TO <headers>.

    " the newest log by date, time and number - BAL_DB_SEARCH does not
    " return them by age, and the first one was an arbitrary old log
    IF latest_only = abap_true AND lines( <headers> ) > 1.
      LOOP AT <headers> ASSIGNING <header>.
        CLEAR lv_key.
        ASSIGN COMPONENT `ALDATE` OF STRUCTURE <header> TO <comp>.
        IF sy-subrc = 0.
          lv_key = |{ <comp> }|.
        ENDIF.
        ASSIGN COMPONENT `ALTIME` OF STRUCTURE <header> TO <comp>.
        IF sy-subrc = 0.
          lv_key = |{ lv_key }{ <comp> }|.
        ENDIF.
        ASSIGN COMPONENT `LOGNUMBER` OF STRUCTURE <header> TO <comp>.
        IF sy-subrc = 0.
          lv_key = |{ lv_key }{ <comp> }|.
        ENDIF.
        IF lr_newest IS NOT BOUND OR lv_key > lv_newest.
          lv_newest = lv_key.
          GET REFERENCE OF <header> INTO lr_newest.
        ENDIF.
      ENDLOOP.
      CREATE DATA lr_latest TYPE ('BALHDR_T').
      ASSIGN lr_latest->* TO <latest>.
      ASSIGN lr_newest->* TO <header>.
      INSERT <header> INTO TABLE <latest>.
      ASSIGN lr_latest->* TO <headers>.
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

  METHOD bal_std_search.

    DATA lv_fm TYPE string.
    FIELD-SYMBOLS <headers> TYPE ANY TABLE.

    CREATE DATA result TYPE ('BALHDR_T').
    ASSIGN result->* TO <headers>.

    lv_fm = `BAL_DB_SEARCH`.
    CALL FUNCTION lv_fm
      EXPORTING
        i_s_log_filter = filter
      IMPORTING
        e_t_log_header = <headers>
      EXCEPTIONS
        error_message  = 2
        OTHERS         = 1.
    " LOG_NOT_FOUND: nothing matches the filter
    IF sy-subrc <> 0 OR <headers> IS INITIAL.
      CLEAR result.
    ENDIF.

  ENDMETHOD.

  METHOD bal_std_delete.

    DATA lv_fm TYPE string.

    lv_fm = `BAL_DB_DELETE`.
    CALL FUNCTION lv_fm
      EXPORTING
        i_t_logs_to_delete = headers
      EXCEPTIONS
        error_message      = 2
        OTHERS             = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |BAL_DB_DELETE failed (sy-subrc { sy-subrc })|.
    ENDIF.
    IF with_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

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

    " text_get types the variables as the message needs them - the
    " module got the strings of ty_s_msg
    IF result-id IS INITIAL.
      result-text = result-v1.
    ELSE.
      result-text = text_get( msgid = result-id
                              msgno = result-no
                              v1    = result-v1
                              v2    = result-v2
                              v3    = result-v3
                              v4    = result-v4 ).
    ENDIF.

  ENDMETHOD.

  METHOD lock_set.

    result = lock_call_function( val     = val
                                 t_param = t_param ).

  ENDMETHOD.

  METHOD lock_set_wait.

    DATA lv_seconds TYPE i.
    DATA lv_fm      TYPE string.

    DATA lv_remaining LIKE retries.
    lv_remaining = retries.

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
                seconds       = lv_seconds
              EXCEPTIONS
                error_message = 2
                OTHERS        = 1.
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

    DATA lr_value TYPE REF TO data.
    FIELD-SYMBOLS <value> TYPE any.
        DATA lt_locks TYPE zabaputil_cl_util_context=>ty_t_lock.
        DATA lv_pattern TYPE string.
        DATA lv_arg TYPE string.
        DATA lt_arg_param TYPE zabaputil_cl_util_context=>ty_t_fm_param.
        DATA lv_use_pattern TYPE abap_bool.
        DATA temp30 TYPE xsdboolean.
        DATA ls_arg_param LIKE LINE OF lt_arg_param.
          DATA lo_type TYPE REF TO cl_abap_typedescr.
          DATA lv_width TYPE i.
          DATA lv_given LIKE abap_false.
          DATA ls_param LIKE LINE OF t_param.
            DATA temp413 TYPE string.
            DATA lv_piece LIKE temp413.
            DATA temp414 TYPE string.
            DATA lv_blanks TYPE i.
        DATA lv_name TYPE string.
        TYPES ty_c30 TYPE c LENGTH 30.
        TYPES temp20 TYPE STANDARD TABLE OF ty_c30 WITH DEFAULT KEY.
DATA lt_tabname TYPE temp20.
        DATA lv_lock_object TYPE ty_c30.
            DATA lv_dd26e TYPE string.
        DATA ls_lock LIKE LINE OF lt_locks.
          DATA temp415 LIKE sy-subrc.

    TRY.
        
        lt_locks = lock_read( ).

        " the lock argument SEQG3-GARG is the client and the key fields one
        " after the other, each at its full width: `A`, `B` of two CHAR10
        " keys are `A         B         `, which never contained `AB`, and
        " a short `1` was found in every argument holding a 1. The pattern
        " has each value at its width (a key not given: + per character)
        " and is matched from the start
        
        lv_pattern = ``.
        
        lv_arg = ``.
        
        lt_arg_param = lock_get_arg_params( val ).
        
        
        temp30 = boolc( lt_arg_param IS NOT INITIAL ).
        lv_use_pattern = temp30.
        
        LOOP AT lt_arg_param INTO ls_arg_param.
          CLEAR lr_value.
          TRY.
              lr_value = sys_fm_create_param( ls_arg_param ).
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
          IF lr_value IS NOT BOUND.
            " a key of unknown width - the pattern cannot be built
            lv_use_pattern = abap_false.
            EXIT.
          ENDIF.
          ASSIGN lr_value->* TO <value>.
          
          lo_type = cl_abap_typedescr=>describe_by_data( <value> ).
          
          lv_width = lo_type->length / cv_char_util_charsize.
          
          lv_given = abap_false.
          
          LOOP AT t_param INTO ls_param
               WHERE name = ls_arg_param-parameter.
            <value> = ls_param-value.
            lv_given = abap_true.
            EXIT.
          ENDLOOP.
          IF lv_given = abap_false AND ls_arg_param-parameter = `MANDT`.
            <value> = context_get_sy( )-mandt.
            lv_given = abap_true.
          ENDIF.
          IF lv_given = abap_true.
            
            temp413 = <value>.
            
            lv_piece = temp413.
            lv_piece = replace( val = lv_piece sub = `#` with = `##` occ = 0 ).
            lv_piece = replace( val = lv_piece sub = `*` with = `#*` occ = 0 ).
            lv_piece = replace( val = lv_piece sub = `+` with = `#+` occ = 0 ).
            
            temp414 = <value>.
            
            lv_blanks = lv_width - strlen( temp414 ).
            lv_pattern = |{ lv_pattern }{ lv_piece }{ repeat( val = ` `
                                                              occ = nmax( val1 = lv_blanks val2 = 0 ) ) }|.
          ELSE.
            lv_pattern = lv_pattern && repeat( val = `+`
                                               occ = lv_width ).
          ENDIF.
        ENDLOOP.
        IF lv_use_pattern = abap_true.
          lv_pattern = lv_pattern && `*`.
        ELSE.
          CLEAR lv_pattern.
          " the module's interface could not be read: the values anywhere
          LOOP AT t_param INTO ls_param.
            lv_arg = lv_arg && ls_param-value.
          ENDLOOP.
        ENDIF.

        
        lv_name = c_trim_upper( val ).
        lv_name = replace( val = lv_name sub = `ENQUEUE_` with = `` ).

        " SEQG3-GNAME (lock_object) holds the TABLE that is locked, not the
        " name of the lock object - the tables of a lock object are in
        " DD26E. Where DD26E cannot be read the names are compared as before
        
        

        
        lv_lock_object = lv_name.
        TRY.
            
            lv_dd26e = `DD26E`.
            SELECT tabname FROM (lv_dd26e) INTO TABLE lt_tabname
              WHERE viewname = lv_lock_object
              .
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

        
        LOOP AT lt_locks INTO ls_lock.
          
          READ TABLE lt_tabname WITH KEY table_line = ls_lock-lock_object TRANSPORTING NO FIELDS.
          temp415 = sy-subrc.
          IF lt_tabname IS INITIAL.
            IF ls_lock-lock_object NS lv_name.
              CONTINUE.
            ENDIF.
          ELSEIF NOT temp415 = 0.
            CONTINUE.
          ENDIF.
          IF lv_pattern IS NOT INITIAL.
            " the argument with its trailing blanks - a CP on the CHAR
            " field would cut them
            IF |{ ls_lock-argument }{ repeat( val = ` ` occ = 120 ) }| NP lv_pattern.
              CONTINUE.
            ENDIF.
          ELSEIF lv_arg IS NOT INITIAL AND ls_lock-argument NS lv_arg.
            CONTINUE.
          ENDIF.
          result = ls_lock-user.
          RETURN.
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
        DATA lx_error TYPE REF TO zabaputil_cx_util_error.
        DATA lx_root TYPE REF TO cx_root.

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
        GET REFERENCE OF lv_client INTO ls_param-value.
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `GNAME`.
        GET REFERENCE OF lv_name INTO ls_param-value.
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `GUNAME`.
        GET REFERENCE OF lv_uname INTO ls_param-value.
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `ENQ`.
        ls_param-kind = abap_func_tables.
        GET REFERENCE OF <lt_enq> INTO ls_param-value.
        INSERT ls_param INTO TABLE lt_param.

        ls_exception-name  = `ERROR_MESSAGE`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.
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

        
      CATCH zabaputil_cx_util_error INTO lx_error.
        RAISE EXCEPTION lx_error.
        
      CATCH cx_root INTO lx_root.
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
        DATA temp31 TYPE xsdboolean.

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
        GET REFERENCE OF lv_check_upd INTO ls_param-value.
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `SUBRC`.
        ls_param-kind = abap_func_importing.
        GET REFERENCE OF lv_subrc INTO ls_param-value.
        INSERT ls_param INTO TABLE lt_param.

        ls_param-name = `ENQ`.
        ls_param-kind = abap_func_tables.
        GET REFERENCE OF <lt_enq> INTO ls_param-value.
        INSERT ls_param INTO TABLE lt_param.

        ls_exception-name  = `ERROR_MESSAGE`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.
        ls_exception-name  = `OTHERS`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.

        lv_function = `ENQUE_DELETE`.
        CALL FUNCTION lv_function
          PARAMETER-TABLE lt_param
          EXCEPTION-TABLE lt_exception.

        
        temp31 = boolc( sy-subrc = 0 AND lv_subrc = 0 ).
        result = temp31.

      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD auth_check.

    DATA lv_object   TYPE c LENGTH 10.
    DATA lv_field    TYPE c LENGTH 10.
    DATA lv_value    TYPE c LENGTH 40.
    DATA lv_activity TYPE c LENGTH 2.
    DATA temp32 TYPE xsdboolean.

    lv_object   = object.
    lv_field    = field.
    lv_value    = value.
    lv_activity = activity.

    AUTHORITY-CHECK OBJECT lv_object
      ID lv_field FIELD lv_value
      ID 'ACTVT' FIELD lv_activity.

    
    temp32 = boolc( sy-subrc = 0 ).
    result = temp32.

  ENDMETHOD.

  METHOD text_get.

    DATA lv_msgid TYPE c LENGTH 20.
    DATA lv_msgno TYPE n LENGTH 3.
    DATA lv_msgv1 TYPE c LENGTH 50.
    DATA lv_msgv2 TYPE c LENGTH 50.
    DATA lv_msgv3 TYPE c LENGTH 50.
    DATA lv_msgv4 TYPE c LENGTH 50.
    DATA lv_langu TYPE c LENGTH 1.
    DATA lv_text  TYPE c LENGTH 73.
          DATA lv_tab TYPE string.
            DATA temp416 TYPE string_table.
            DATA temp1 TYPE string.
            DATA temp2 TYPE string.
            DATA temp3 TYPE string.
            DATA temp4 TYPE string.
            DATA lt_var LIKE temp416.
              DATA lv_var LIKE LINE OF lt_var.

    lv_msgid = msgid.
    lv_msgno = msgno.
    lv_msgv1 = v1.
    lv_msgv2 = v2.
    lv_msgv3 = v3.
    lv_msgv4 = v4.
    lv_langu = langu.

    " another language than the logon one: the text from T100, the
    " variables put in as MESSAGE does (&1..&4, or & in their order)
    IF lv_langu IS NOT INITIAL AND lv_langu <> sy-langu AND check_abap_cloud( ) = abap_false.
      TRY.
          
          lv_tab = `T100`.
          SELECT SINGLE text FROM (lv_tab) INTO lv_text
            WHERE sprsl = lv_langu AND arbgb = lv_msgid AND msgnr = lv_msgno
            .
          IF sy-subrc = 0.
            result = lv_text.
            
            CLEAR temp416.
            
            temp1 = condense( lv_msgv1 ).
            INSERT temp1 INTO TABLE temp416.
            
            temp2 = condense( lv_msgv2 ).
            INSERT temp2 INTO TABLE temp416.
            
            temp3 = condense( lv_msgv3 ).
            INSERT temp3 INTO TABLE temp416.
            
            temp4 = condense( lv_msgv4 ).
            INSERT temp4 INTO TABLE temp416.
            
            lt_var = temp416.
            IF result CS `&1` OR result CS `&2` OR result CS `&3` OR result CS `&4`.
              
              LOOP AT lt_var INTO lv_var.
                result = replace( val  = result
                                  sub  = |&{ sy-tabix }|
                                  with = lv_var
                                  occ  = 0 ).
              ENDLOOP.
            ELSE.
              LOOP AT lt_var INTO lv_var.
                result = replace( val  = result
                                  sub  = `&`
                                  with = lv_var ).
              ENDLOOP.
            ENDIF.
            RETURN.
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    " MESSAGE ... INTO builds the text on every release and on ABAP Cloud,
    " where MESSAGE_TEXT_BUILD is not released
    TRY.
        MESSAGE ID lv_msgid TYPE `I` NUMBER lv_msgno
                WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4
                INTO result.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    IF result IS INITIAL.
      result = |{ lv_msgid } { lv_msgno }: { lv_msgv1 } { lv_msgv2 } { lv_msgv3 } { lv_msgv4 }|.
    ENDIF.

  ENDMETHOD.

  METHOD mail_send.

    DATA temp418 TYPE string.
    DATA lv_content_type LIKE temp418.
      DATA lo_mail_c TYPE REF TO object.
      DATA lr_status_c TYPE REF TO data.
      FIELD-SYMBOLS <lt_status_c> TYPE ANY TABLE.
      FIELD-SYMBOLS <ls_status_c> TYPE any.
      FIELD-SYMBOLS <lv_status_c> TYPE any.
          DATA temp419 LIKE REF TO lv_content_type.
DATA temp673 TYPE abap_parmbind_tab.
DATA temp674 LIKE LINE OF temp673.
DATA lo_part_c TYPE REF TO object.
          DATA temp420 TYPE REF TO cl_abap_objectdescr.
          DATA lo_descr_c LIKE temp420.
          DATA lo_status_type_c TYPE REF TO cl_abap_datadescr.
          DATA lx_mail_c TYPE REF TO cx_root.
    DATA lo_mail TYPE REF TO object.
    DATA lo_sender TYPE REF TO object.
    DATA lo_recipient TYPE REF TO object.
    DATA lo_doc TYPE REF TO object.
    DATA lr_body TYPE REF TO data.
    DATA lr_line TYPE REF TO data.
    DATA lv_class TYPE string.
    DATA lv_subject TYPE c LENGTH 50.
    DATA lv_type TYPE c LENGTH 3.
    DATA lv_address TYPE c LENGTH 241.
    FIELD-SYMBOLS <body> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    DATA temp421 LIKE lv_type.
        DATA temp422 TYPE string_table.
        DATA temp675 TYPE string_table.
        DATA lt_lines LIKE temp675.
        DATA lv_body_line LIKE LINE OF lt_lines.
        DATA lx_mail TYPE REF TO cx_root.
    IF html = abap_true.
      temp418 = `text/html`.
    ELSE.
      temp418 = `text/plain`.
    ENDIF.
    
    lv_content_type = temp418.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: CL_BCS_MAIL_MESSAGE - the body is a CL_BCS_MAIL_TEXTPART, and
      " SEND reports per recipient in ET_STATUS (it has no RETURNING value)
      
      
      
      
      

      TRY.
          lo_mail_c = dyn_call_obj( class_name = `CL_BCS_MAIL_MESSAGE`
                                    name       = `CREATE_INSTANCE` ).

          " without a sender the mail configuration sets it - a made-up
          " address was rejected
          IF sender IS NOT INITIAL.
            dyn_call( obj  = lo_mail_c
                      name = `SET_SENDER`
                      arg  = sender ).
          ENDIF.
          dyn_call( obj  = lo_mail_c
                    name = `ADD_RECIPIENT`
                    arg  = to ).
          dyn_call( obj  = lo_mail_c
                    name = `SET_SUBJECT`
                    arg  = subject ).

          
          GET REFERENCE OF lv_content_type INTO temp419.

CLEAR temp673.

temp674-name = `IV_CONTENT_TYPE`.
temp674-kind = cl_abap_objectdescr=>exporting.
temp674-value = temp419.
INSERT temp674 INTO TABLE temp673.

lo_part_c = dyn_call_obj(
                                class_name = `CL_BCS_MAIL_TEXTPART`
                                name       = `CREATE_INSTANCE`
                                arg        = body
                                t_param    = temp673 ).
          dyn_call( obj  = lo_mail_c
                    name = `SET_MAIN`
                    arg  = lo_part_c ).

          
          temp420 ?= cl_abap_typedescr=>describe_by_object_ref( lo_mail_c ).
          
          lo_descr_c = temp420.
          
          lo_status_type_c = lo_descr_c->get_method_parameter_type( p_method_name    = `SEND`
                                                                         p_parameter_name = `ET_STATUS` ).
          CREATE DATA lr_status_c TYPE HANDLE lo_status_type_c.
          ASSIGN lr_status_c->* TO <lt_status_c>.
          CALL METHOD lo_mail_c->(`SEND`)
            IMPORTING
              et_status = <lt_status_c>.

          " sent unless a recipient came back with an error
          result = abap_true.
          LOOP AT <lt_status_c> ASSIGNING <ls_status_c>.
            ASSIGN COMPONENT `STATUS` OF STRUCTURE <ls_status_c> TO <lv_status_c>.
            IF sy-subrc = 0 AND <lv_status_c> = `E`.
              result = abap_false.
            ENDIF.
          ENDLOOP.

          
        CATCH cx_root INTO lx_mail_c.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = lx_mail_c.
      ENDTRY.
      RETURN.
    ENDIF.

    " Standard ABAP: use CL_BCS
    
    
    
    
    
    
    
    
    
    
    
    
    

    lv_subject = subject.
    
    IF html = abap_true.
      temp421 = `HTM`.
    ELSE.
      temp421 = `RAW`.
    ENDIF.
    lv_type    = temp421.

    TRY.
        " Create BCS instance
        lv_class = `CL_BCS`.
        CALL METHOD (lv_class)=>(`CREATE_PERSISTENT`)
          RECEIVING
            result = lo_mail.

        " Sender
        IF sender IS INITIAL.
          lv_class = `CL_SAPUSER_BCS`.
          CALL METHOD (lv_class)=>(`CREATE`)
            EXPORTING
              i_user = sy-uname
            RECEIVING
              result = lo_sender.
        ELSE.
          lv_address = sender.
          lv_class = `CL_CAM_ADDRESS_BCS`.
          CALL METHOD (lv_class)=>(`CREATE_INTERNET_ADDRESS`)
            EXPORTING
              i_address_string = lv_address
            RECEIVING
              result           = lo_sender.
        ENDIF.
        CALL METHOD lo_mail->(`SET_SENDER`)
          EXPORTING
            i_sender = lo_sender.

        " Recipient
        lv_address = to.
        lv_class = `CL_CAM_ADDRESS_BCS`.
        CALL METHOD (lv_class)=>(`CREATE_INTERNET_ADDRESS`)
          EXPORTING
            i_address_string = lv_address
          RECEIVING
            result           = lo_recipient.
        CALL METHOD lo_mail->(`ADD_RECIPIENT`)
          EXPORTING
            i_recipient = lo_recipient.

        " the body as SOLI lines (CHAR255). HTML is one text: cut into
        " pieces of 255 with its line breaks kept as characters - one line
        " per row cut every longer line, and an HTML body is mostly one
        " line. Plain text keeps one row per line, a longer line goes on in
        " the next row instead of being cut off
        CREATE DATA lr_body TYPE (`BCSY_TEXT`).
        ASSIGN lr_body->* TO <body>.
        CREATE DATA lr_line TYPE (`SOLI`).
        ASSIGN lr_line->* TO <line>.
        ASSIGN COMPONENT `LINE` OF STRUCTURE <line> TO <field>.

        
        CLEAR temp422.
        INSERT body INTO TABLE temp422.
        
        IF html = abap_true.
          temp675 = temp422.
        ELSE.
          temp675 = c_split( val = body sep = cv_char_util_newline ).
        ENDIF.
        
        lt_lines = temp675.
        
        LOOP AT lt_lines INTO lv_body_line.
          DO.
            <field> = lv_body_line.
            INSERT <line> INTO TABLE <body>.
            IF strlen( lv_body_line ) <= 255.
              EXIT.
            ENDIF.
            lv_body_line = substring( val = lv_body_line
                                      off = 255 ).
          ENDDO.
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

        " the document subject is CHAR50 - a longer one goes on the mail
        " itself, where the release has the method
        IF strlen( subject ) > 50.
          TRY.
              CALL METHOD lo_mail->(`SET_MESSAGE_SUBJECT`)
                EXPORTING
                  ip_subject = subject.
            CATCH cx_sy_dyn_call_error ##NO_HANDLER.
          ENDTRY.
        ENDIF.

        CALL METHOD lo_mail->(`SET_SEND_IMMEDIATELY`)
          EXPORTING
            i_send_immediately = abap_true.

        CALL METHOD lo_mail->(`SEND`)
          RECEIVING
            result = result.
        IF with_commit = abap_true.
          COMMIT WORK AND WAIT.
        ENDIF.

        
      CATCH cx_root INTO lx_mail.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_mail.
    ENDTRY.

  ENDMETHOD.

  METHOD job_submit_report.

    result = job_submit( report          = report
                         variant         = variant
                         start_immediate = start_immediate
                         job_name        = job_name )-jobname.

  ENDMETHOD.

  METHOD job_submit.
    DATA lv_fm TYPE string.
    DATA lv_jobname TYPE c LENGTH 32.
    DATA lv_jobcount TYPE c LENGTH 8.
    DATA lv_report TYPE c LENGTH 40.
    DATA lv_variant TYPE c LENGTH 14.
    DATA lx_failed TYPE REF TO cx_root.
    DATA lx_util TYPE REF TO zabaputil_cx_util_error.
    DATA temp424 LIKE lv_jobname.
        DATA lv_open_subrc LIKE sy-subrc.
        DATA x_open TYPE REF TO cx_root.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: Application Jobs have a different architecture (job catalog + templates).
      " Direct report submission is not available. Raise informative exception.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `job_submit_report: On ABAP Cloud use CL_APJ_RT_API with a registered job catalog entry instead`.
    ENDIF.

    " Standard ABAP: JOB_OPEN / JOB_SUBMIT / JOB_CLOSE
    
    
    
    
    
    
    

    lv_report = report.
    lv_variant = variant.
    
    IF job_name IS NOT INITIAL.
      temp424 = job_name.
    ELSE.
      temp424 = |JOB_{ sy-datum }{ sy-uzeit }|.
    ENDIF.
    lv_jobname = temp424.

    lv_fm = `JOB_OPEN`.
    TRY.
        CALL FUNCTION lv_fm
          EXPORTING
            jobname  = lv_jobname
          IMPORTING
            jobcount = lv_jobcount
          EXCEPTIONS
            error_message = 1
            OTHERS        = 1.
        
        lv_open_subrc = sy-subrc.
        
      CATCH cx_root INTO x_open.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x_open.
    ENDTRY.
    IF lv_open_subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `JOB_OPEN failed`.
    ENDIF.

    TRY.
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

        result-jobname  = lv_jobname.
        result-jobcount = lv_jobcount.

      CATCH cx_root INTO lx_failed.
    ENDTRY.

    IF lx_failed IS BOUND.
      " the job JOB_OPEN created would stay in SM37 without a step or
      " without a start
      TRY.
          lv_fm = `BP_JOB_DELETE`.
          CALL FUNCTION lv_fm
            EXPORTING
              jobcount = lv_jobcount
              jobname  = lv_jobname
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      TRY.
          lx_util ?= lx_failed.
        CATCH cx_sy_move_cast_error ##NO_HANDLER.
      ENDTRY.
      IF lx_util IS BOUND.
        RAISE EXCEPTION lx_util.
      ENDIF.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = lx_failed.
    ENDIF.

  ENDMETHOD.

  METHOD numrange_get_next.

    DATA lv_object  TYPE c LENGTH 10.
    DATA lv_nr_sub  TYPE c LENGTH 2.
    DATA lv_sub     TYPE c LENGTH 6.
    DATA lv_toyear  TYPE n LENGTH 4.
    DATA lv_number  TYPE c LENGTH 20.
          DATA lv_cls TYPE string.
          DATA temp425 LIKE REF TO lv_number.
DATA temp676 LIKE REF TO lv_object.
DATA temp134 LIKE REF TO lv_nr_sub.
DATA temp1 TYPE abap_parmbind_tab.
DATA temp2 LIKE LINE OF temp1.
DATA lt_param_c LIKE temp1.
            DATA temp426 LIKE REF TO lv_sub.
DATA temp677 TYPE abap_parmbind.
            DATA temp427 LIKE REF TO lv_toyear.
DATA temp678 TYPE abap_parmbind.
          DATA lv_fm TYPE string.
          DATA temp428 LIKE REF TO lv_number.
DATA temp679 LIKE REF TO lv_object.
DATA temp135 LIKE REF TO lv_nr_sub.
DATA temp3 TYPE abap_func_parmbind_tab.
DATA temp4 LIKE LINE OF temp3.
DATA lt_param LIKE temp3.
            DATA temp429 LIKE REF TO lv_sub.
DATA temp680 TYPE abap_func_parmbind.
            DATA temp430 LIKE REF TO lv_toyear.
DATA temp681 TYPE abap_func_parmbind.
          DATA temp431 TYPE abap_func_excpbind_tab.
          DATA temp432 LIKE LINE OF temp431.
          DATA lt_exception LIKE temp431.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    lv_object = object.
    lv_nr_sub = subobject.
    lv_sub    = nr_subobject.
    lv_toyear = toyear.

    TRY.
        IF check_abap_cloud( ) IS NOT INITIAL.
          " Cloud: use CL_NUMBERRANGE_RUNTIME
          
          lv_cls = `CL_NUMBERRANGE_RUNTIME`.
          
          GET REFERENCE OF lv_number INTO temp425.

GET REFERENCE OF lv_object INTO temp676.

GET REFERENCE OF lv_nr_sub INTO temp134.

CLEAR temp1.

temp2-name = `NR_RANGE_NR`.
temp2-kind = cl_abap_objectdescr=>exporting.
temp2-value = temp134.
INSERT temp2 INTO TABLE temp1.
temp2-name = `OBJECT`.
temp2-kind = cl_abap_objectdescr=>exporting.
temp2-value = temp676.
INSERT temp2 INTO TABLE temp1.
temp2-name = `NUMBER`.
temp2-kind = cl_abap_objectdescr=>importing.
temp2-value = temp425.
INSERT temp2 INTO TABLE temp1.

lt_param_c = temp1.
          IF lv_sub IS NOT INITIAL.
            
            GET REFERENCE OF lv_sub INTO temp426.

CLEAR temp677.
temp677-name = `SUBOBJECT`.
temp677-kind = cl_abap_objectdescr=>exporting.
temp677-value = temp426.
INSERT temp677 INTO TABLE lt_param_c.
          ENDIF.
          IF lv_toyear IS NOT INITIAL.
            
            GET REFERENCE OF lv_toyear INTO temp427.

CLEAR temp678.
temp678-name = `TOYEAR`.
temp678-kind = cl_abap_objectdescr=>exporting.
temp678-value = temp427.
INSERT temp678 INTO TABLE lt_param_c.
          ENDIF.
          CALL METHOD (lv_cls)=>(`NUMBER_GET`)
            PARAMETER-TABLE lt_param_c.
        ELSE.
          " Standard: use NUMBER_GET_NEXT FM
          
          lv_fm = `NUMBER_GET_NEXT`.
          
          GET REFERENCE OF lv_number INTO temp428.

GET REFERENCE OF lv_object INTO temp679.

GET REFERENCE OF lv_nr_sub INTO temp135.

CLEAR temp3.

temp4-name = `NR_RANGE_NR`.
temp4-kind = abap_func_exporting.
temp4-value = temp135.
INSERT temp4 INTO TABLE temp3.
temp4-name = `OBJECT`.
temp4-kind = abap_func_exporting.
temp4-value = temp679.
INSERT temp4 INTO TABLE temp3.
temp4-name = `NUMBER`.
temp4-kind = abap_func_importing.
temp4-value = temp428.
INSERT temp4 INTO TABLE temp3.

lt_param = temp3.
          IF lv_sub IS NOT INITIAL.
            
            GET REFERENCE OF lv_sub INTO temp429.

CLEAR temp680.
temp680-name = `SUBOBJECT`.
temp680-kind = abap_func_exporting.
temp680-value = temp429.
INSERT temp680 INTO TABLE lt_param.
          ENDIF.
          IF lv_toyear IS NOT INITIAL.
            
            GET REFERENCE OF lv_toyear INTO temp430.

CLEAR temp681.
temp681-name = `TOYEAR`.
temp681-kind = abap_func_exporting.
temp681-value = temp430.
INSERT temp681 INTO TABLE lt_param.
          ENDIF.
          
          CLEAR temp431.
          
          temp432-name = `ERROR_MESSAGE`.
          temp432-value = 2.
          INSERT temp432 INTO TABLE temp431.
          temp432-name = `OTHERS`.
          temp432-value = 1.
          INSERT temp432 INTO TABLE temp431.
          
          lt_exception = temp431.
          CALL FUNCTION lv_fm
            PARAMETER-TABLE lt_param
            EXCEPTION-TABLE lt_exception.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |NUMBER_GET_NEXT failed for { lv_object }/{ lv_nr_sub }|.
          ENDIF.
        ENDIF.

        result = lv_number.

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD changdoc_read.
          DATA lv_cds TYPE string.
          DATA lv_where_c TYPE string.
          FIELD-SYMBOLS <cds_tab> TYPE STANDARD TABLE.
          FIELD-SYMBOLS <cds_row> TYPE any.
          FIELD-SYMBOLS <cds_fld> TYPE any.
          DATA lr_cds_tab TYPE REF TO data.
          DATA lt_comp_c TYPE abap_component_tab.
          DATA lo_struct_c TYPE REF TO cl_abap_structdescr.
          DATA lo_table_c TYPE REF TO cl_abap_tabledescr.
            DATA temp433 TYPE ty_s_changdoc.
            DATA ls_doc_c LIKE temp433.
    DATA lv_fm TYPE string.
    DATA lv_objectclas TYPE c LENGTH 15.
    DATA lv_objectid TYPE c LENGTH 90.
    DATA lr_headers TYPE REF TO data.
    DATA lr_positions TYPE REF TO data.
    FIELD-SYMBOLS <headers> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <positions> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <hdr> TYPE any.
    FIELD-SYMBOLS <pos> TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.
          DATA temp434 TYPE ty_s_changdoc.
          DATA ls_doc LIKE temp434.
              DATA ls_pos LIKE ls_doc.

    IF check_abap_cloud( ) IS NOT INITIAL.
      " Cloud: use released CDS view I_ChangeDocument
      TRY.
          
          lv_cds = `I_CHANGEDOCUMENTITEM`.
          " the values quoted - a quote inside them would end the literal
          
          lv_where_c = |OBJECTCLASS = { filter_sql_quote( objectclass ) } AND OBJECTID = { filter_sql_quote( objectid ) }|.
          IF date_from IS NOT INITIAL.
            lv_where_c = |{ lv_where_c } AND CREATIONDATE >= '{ date_from }'|.
          ENDIF.
          IF date_to IS NOT INITIAL AND date_to <> '99991231'.
            lv_where_c = |{ lv_where_c } AND CREATIONDATE <= '{ date_to }'|.
          ENDIF.

          
          
          
          

          
          lt_comp_c = rtti_get_t_attri_by_table_name( lv_cds ).
          
          lo_struct_c = cl_abap_structdescr=>create( lt_comp_c ).
          
          lo_table_c = cl_abap_tabledescr=>create( lo_struct_c ).
          CREATE DATA lr_cds_tab TYPE HANDLE lo_table_c.
          ASSIGN lr_cds_tab->* TO <cds_tab>.

          SELECT *
            FROM (lv_cds) INTO CORRESPONDING FIELDS OF TABLE <cds_tab>
            WHERE (lv_where_c)
            .

          LOOP AT <cds_tab> ASSIGNING <cds_row>.
            
            CLEAR temp433.
            
            ls_doc_c = temp433.
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

          
          CLEAR temp434.
          
          ls_doc = temp434.
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
              
              ls_pos = ls_doc.
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

          " the statement at the start of the line, outside of comments:
          " `CS |METHOD GET|` also started at GET_DATA, GET_TEXT and at a
          " comment naming the method, and stopped at one naming ENDMETHOD
          lv_source_upper = condense( to_upper( lv_source ) ).
          IF ( lv_source IS NOT INITIAL AND lv_source(1) = `*` ) OR lv_source_upper CP `"*`.
            IF lv_check_method = abap_true.
              INSERT lv_source INTO TABLE lt_string.
            ENDIF.
            CONTINUE.
          ENDIF.

          IF lv_source_upper CP `ENDMETHOD*`.
            lv_check_method = abap_false.
          ENDIF.

          IF lv_source_upper CP |METHOD { lv_method }.*| OR lv_source_upper CP |METHOD { lv_method } *|.
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

    DATA lt_source TYPE string_table.
    lt_source = source_get_method( iv_classname  = iv_classname
                                         iv_methodname = iv_methodname ).

    result = source_method_to_file( lt_source ).

  ENDMETHOD.

  METHOD source_get_file_types.

    DATA lv_types TYPE string.
    lv_types = |abap, abc, actionscript, ada, apache_conf, applescript, asciidoc, assembly_x86, autohotkey, batchfile, bro, c9search, c_cpp, cirru, clojure, cobol, coffee, coldfusion, csharp, css, curly, d, dart, diff, django, dockerfile, | &&
|dot, drools, eiffel, yaml, ejs, elixir, elm, erlang, forth, fortran, ftl, gcode, gherkin, gitignore, glsl, gobstones, golang, groovy, haml, handlebars, haskell, haskell_cabal, haxe, hjson, html, html_elixir, html_ruby, ini, io, jack, jade, java, ja| &&
      |vascri| &&
|pt, json, jsoniq, jsp, jsx, julia, kotlin, latex, lean, less, liquid, lisp, live_script, livescript, logiql, lsl, lua, luapage, lucene, makefile, markdown, mask, matlab, mavens_mate_log, maze, mel, mips_assembler, mipsassembler, mushcode, mysql, ni| &&
|x, nsis, objectivec, ocaml, pascal, perl, pgsql, php, plain_text, powershell, praat, prolog, properties, protobuf, python, r, razor, rdoc, rhtml, rst, ruby, rust, sass, scad, scala, scheme, scss, sh, sjs, smarty, snippets, soy_template, space, sql,| &&
      | sqlserver, stylus, svg, swift, swig, tcl, tex, text, textile, toml, tsx, twig, typescript, vala, vbscript, velocity, verilog, vhdl, wollok, xml, xquery, terraform, slim, redshift, red, puppet, php_laravel_blade, mixal, jssm, fsharp, edifact,| &&
      | csp, cssound_score, cssound_orchestra, cssound_document| ##NO_TEXT.
    SPLIT lv_types AT `,` INTO TABLE result.

  ENDMETHOD.

  METHOD source_method_to_file.

    DATA lv_source LIKE LINE OF it_source.
    LOOP AT it_source INTO lv_source.
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
    DATA lr_value      TYPE REF TO data.
    DATA lt_exception  TYPE abap_func_excpbind_tab.
    DATA ls_exception  TYPE abap_func_excpbind.
    DATA lv_function   TYPE string.
    DATA lt_interface  TYPE ty_t_fm_param.
    FIELD-SYMBOLS <value> TYPE any.
TYPES BEGIN OF ty_s_cloud_param.
TYPES name TYPE string.
TYPES value TYPE REF TO data.
TYPES END OF ty_s_cloud_param.
      TYPES temp21 TYPE STANDARD TABLE OF ty_s_cloud_param WITH DEFAULT KEY.
DATA lt_cloud_param TYPE temp21.
          DATA lv_lock_object TYPE string.
            DATA temp435 TYPE ty_s_cloud_param.
          DATA lo_lock TYPE REF TO object.
          DATA temp436 TYPE string.
          DATA lx_cloud TYPE REF TO cx_root.
          DATA temp437 TYPE i.
      DATA temp33 TYPE xsdboolean.
          DATA ls_interface LIKE LINE OF lt_interface.
        DATA temp34 TYPE xsdboolean.

    lv_function = c_trim_upper( val ).

    IF check_abap_cloud( ) = abap_true.
      " the function modules are not released - the lock object API is
      
      

      TRY.
          
          lv_lock_object = substring( val = lv_function
                                            off = 8 ).
          LOOP AT t_param INTO ls_lock_param.
            CREATE DATA lr_value TYPE string.
            ASSIGN lr_value->* TO <value>.
            <value> = ls_lock_param-value.
            
            CLEAR temp435.
            temp435-name = c_trim_upper( ls_lock_param-name ).
            temp435-value = lr_value.
            INSERT temp435 INTO TABLE lt_cloud_param.
          ENDLOOP.
          
          lo_lock = dyn_call_obj( class_name = `CL_ABAP_LOCK_OBJECT_FACTORY`
                                        name       = `GET_INSTANCE`
                                        arg        = lv_lock_object ).
          
          IF lv_function CP `DEQUEUE_*`.
            temp436 = `DEQUEUE`.
          ELSE.
            temp436 = `ENQUEUE`.
          ENDIF.
          dyn_call( obj      = lo_lock
                    name     = temp436
                    arg      = lt_cloud_param
                    arg_name = `IT_PARAMETER` ).
          subrc = 0.
          
        CATCH cx_root INTO lx_cloud.
          
          IF cl_abap_classdescr=>get_class_name( lx_cloud ) CS `FOREIGN_LOCK`.
            temp437 = 1.
          ELSEIF cl_abap_classdescr=>get_class_name( lx_cloud ) CS `LOCK`.
            temp437 = 4.
          ELSE.
            temp437 = 99.
          ENDIF.
          subrc = temp437.
      ENDTRY.
      
      temp33 = boolc( subrc = 0 ).
      result = temp33.
      RETURN.
    ENDIF.

    TRY.
        " the types of the module's parameters - where they cannot be read
        " the values go as strings, as before
        TRY.
            lt_interface = sys_fm_get_interface( lv_function ).
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

        LOOP AT t_param INTO ls_lock_param.
          ls_param-name = c_trim_upper( ls_lock_param-name ).
          ls_param-kind = abap_func_exporting.
          CLEAR lr_value.
          
          LOOP AT lt_interface INTO ls_interface
               WHERE parameter = ls_param-name AND paramclass = `I`.
            TRY.
                lr_value = sys_fm_create_param( ls_interface ).
              CATCH cx_root ##NO_HANDLER.
            ENDTRY.
            EXIT.
          ENDLOOP.
          IF lr_value IS NOT BOUND.
            CREATE DATA lr_value TYPE string.
          ENDIF.
          ASSIGN lr_value->* TO <value>.
          <value> = ls_lock_param-value.
          ls_param-value = lr_value.
          INSERT ls_param INTO TABLE lt_param.
        ENDLOOP.

        " every generated ENQUEUE_ module has FOREIGN_LOCK - the one
        " failure that means "someone else holds it"
        IF lv_function CP `ENQUEUE_*`.
          ls_exception-name  = `FOREIGN_LOCK`.
          ls_exception-value = 1.
          INSERT ls_exception INTO TABLE lt_exception.
        ENDIF.
        " a MESSAGE TYPE E of the module would end the request
        ls_exception-name  = `ERROR_MESSAGE`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.
        ls_exception-name  = `OTHERS`.
        ls_exception-value = 4.
        INSERT ls_exception INTO TABLE lt_exception.

        CALL FUNCTION lv_function
          PARAMETER-TABLE lt_param
          EXCEPTION-TABLE lt_exception.

        subrc = sy-subrc.
        
        temp34 = boolc( subrc = 0 ).
        result = temp34.

      CATCH cx_root.
        subrc = 99.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD lock_get_arg_params.
        DATA lv_function TYPE string.
        DATA temp438 TYPE zabaputil_cl_util_context=>ty_t_fm_param.
        DATA ls_param LIKE LINE OF temp438.

    TRY.
        
        lv_function = c_trim_upper( val ).
        IF lv_function NP `ENQUEUE_*`.
          lv_function = |ENQUEUE_{ lv_function }|.
        ENDIF.
        
        temp438 = sys_fm_get_interface( lv_function ).
        
        LOOP AT temp438 INTO ls_param
             WHERE paramclass = `I`.
          IF ls_param-parameter CP `MODE_*` OR ls_param-parameter CP `X_*` OR ls_param-parameter CP `#_*`.
            CONTINUE.
          ENDIF.
          INSERT ls_param INTO TABLE result.
        ENDLOOP.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD cal_get_weekday.

    " 1900-01-01 was a Monday, so the day distance modulo 7 yields the weekday
    DATA temp439 TYPE d.
    DATA lv_days TYPE i.
    temp439 = `19000101`.
    
    lv_days = date - temp439.
    result = lv_days MOD 7 + 1.

  ENDMETHOD.

  METHOD cal_is_weekend.

    DATA temp35 TYPE xsdboolean.
    temp35 = boolc( cal_get_weekday( date ) >= 6 ).
    result = temp35.

  ENDMETHOD.

  METHOD cal_is_workday.
    DATA temp36 TYPE xsdboolean.

    IF calendar_id IS NOT INITIAL.
      x_raise( `cal_is_workday: factory calendar support is not yet implemented` ).
    ENDIF.

    
    temp36 = boolc( cal_is_weekend( date ) = abap_false ).
    result = temp36.

  ENDMETHOD.

  METHOD cal_add_workdays.

    DATA lv_remaining TYPE i.
    DATA temp440 TYPE i.
    DATA lv_step LIKE temp440.
    lv_remaining = abs( days ).
    
    IF days < 0.
      temp440 = -1.
    ELSE.
      temp440 = 1.
    ENDIF.
    
    lv_step = temp440.

    result = date.
    WHILE lv_remaining > 0.
      result = result + lv_step.
      IF cal_is_workday( date = result calendar_id = calendar_id ) = abap_true.
        lv_remaining = lv_remaining - 1.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD cal_count_workdays.
    DATA lv_date LIKE date_from.

    " a date field takes any eight characters: with 20240230 or an initial
    " date the loop below never met date_to and ran forever, or for
    " hundreds of thousands of days
    IF check_is_date_valid( |{ date_from(4) }-{ date_from+4(2) }-{ date_from+6(2) }| ) = abap_false
        OR check_is_date_valid( |{ date_to(4) }-{ date_to+4(2) }-{ date_to+6(2) }| ) = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |CAL_COUNT_WORKDAYS: invalid date { date_from } or { date_to }|.
    ENDIF.

    
    lv_date = date_from.
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

    " One worksheet as XML, zipped with the four parts a workbook needs
    " (zip_pack: CL_ABAP_ZIP, released on ABAP Cloud and there on every
    " release down to 7.02). Ported from abap-cloud-gui's ALV export, with
    " its escaping: c_escape_xml drops the control characters XML 1.0
    " forbids - a legacy text carries them, and Excel "repairs" a workbook
    " with one - and xml:space keeps leading and trailing blanks
    CONSTANTS lc_xml      TYPE string VALUE `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`.
    CONSTANTS lc_main     TYPE string VALUE `http://schemas.openxmlformats.org/spreadsheetml/2006/main`.
    CONSTANTS lc_rel      TYPE string VALUE `http://schemas.openxmlformats.org/officeDocument/2006/relationships`.
    CONSTANTS lc_pkg_rel  TYPE string VALUE `http://schemas.openxmlformats.org/package/2006/relationships`.
    CONSTANTS lc_ct       TYPE string VALUE `application/vnd.openxmlformats-officedocument.spreadsheetml`.
    TYPES:
      BEGIN OF ty_s_col,
        name TYPE string,
        kind TYPE string,
      END OF ty_s_col.
    TYPES temp22 TYPE STANDARD TABLE OF ty_s_col WITH DEFAULT KEY.
DATA lt_col  TYPE temp22.
    DATA lt_xml  TYPE string_table.
    DATA lt_cell TYPE string_table.
    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_text TYPE string.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <cell> TYPE any.

    " the columns, decided once from the line type: the included view, so
    " that an include gives its fields, not one nameless column
    DATA temp441 TYPE REF TO cl_abap_tabledescr.
    DATA lo_table LIKE temp441.
    DATA lo_line TYPE REF TO cl_abap_datadescr.
    DATA lv_elementary TYPE abap_bool.
    DATA temp37 TYPE xsdboolean.
      DATA temp442 TYPE ty_s_col.
      DATA temp443 TYPE REF TO cl_abap_structdescr.
      DATA lt_comp TYPE abap_component_view_tab.
      DATA temp444 LIKE LINE OF lt_comp.
      DATA lr_comp LIKE REF TO temp444.
        DATA temp445 TYPE ty_s_col.
    DATA temp446 LIKE LINE OF lt_xml.
    DATA temp447 LIKE LINE OF lt_col.
    DATA lr_col LIKE REF TO temp447.
    DATA temp448 LIKE LINE OF lt_xml.
            DATA temp449 LIKE LINE OF lt_cell.
      DATA temp450 LIKE LINE OF lt_xml.
    DATA temp451 TYPE ty_t_zip_file.
    DATA temp452 LIKE LINE OF temp451.
    DATA lt_files LIKE temp451.
    temp441 ?= cl_abap_typedescr=>describe_by_data( val ).
    
    lo_table = temp441.
    
    lo_line = lo_table->get_table_line_type( ).
    
    
    temp37 = boolc( lo_line->kind <> cl_abap_typedescr=>kind_struct ).
    lv_elementary = temp37.
    IF lv_elementary = abap_true.
      
      CLEAR temp442.
      temp442-name = `TABLE_LINE`.
      temp442-kind = xlsx_get_column_kind( lo_line ).
      INSERT temp442 INTO TABLE lt_col.
    ELSE.
      
      temp443 ?= lo_line.
      
      lt_comp = temp443->get_included_view( ).
      
      
      LOOP AT lt_comp REFERENCE INTO lr_comp.
        
        CLEAR temp445.
        temp445-name = lr_comp->name.
        temp445-kind = xlsx_get_column_kind( lr_comp->type ).
        INSERT temp445 INTO TABLE lt_col.
      ENDLOOP.
    ENDIF.

    
    temp446 = |{ lc_xml }<worksheet xmlns="{ lc_main }"><sheetData>|.
    INSERT temp446 INTO TABLE lt_xml.
    
    
    LOOP AT lt_col REFERENCE INTO lr_col.
      INSERT xlsx_get_text_cell( lr_col->name ) INTO TABLE lt_cell.
    ENDLOOP.
    
    temp448 = |<row>{ concat_lines_of( lt_cell ) }</row>|.
    INSERT temp448 INTO TABLE lt_xml.

    LOOP AT val ASSIGNING <row>.
      CLEAR lt_cell.
      LOOP AT lt_col REFERENCE INTO lr_col.
        IF lr_col->kind IS INITIAL.
          INSERT `<c/>` INTO TABLE lt_cell.
          CONTINUE.
        ENDIF.
        IF lv_elementary = abap_true.
          ASSIGN <row> TO <cell>.
        ELSE.
          ASSIGN COMPONENT lr_col->name OF STRUCTURE <row> TO <cell>.
        ENDIF.
        CASE lr_col->kind.
          WHEN `N`.
            " a string template writes the sign in front and a period
            " as decimal separator, without thousands separators - the
            " number format of the file, whatever the user's settings
            
            temp449 = |<c><v>{ <cell> }</v></c>|.
            INSERT temp449 INTO TABLE lt_cell.
          WHEN `D`.
            lv_date = <cell>.
            CLEAR lv_text.
            IF lv_date IS NOT INITIAL.
              lv_text = |{ lv_date(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|.
            ENDIF.
            INSERT xlsx_get_text_cell( lv_text ) INTO TABLE lt_cell.
          WHEN `T`.
            lv_time = <cell>.
            INSERT xlsx_get_text_cell( |{ lv_time(2) }:{ lv_time+2(2) }:{ lv_time+4(2) }| ) INTO TABLE lt_cell.
          WHEN OTHERS.
            INSERT xlsx_get_text_cell( |{ <cell> }| ) INTO TABLE lt_cell.
        ENDCASE.
      ENDLOOP.
      
      temp450 = |<row>{ concat_lines_of( lt_cell ) }</row>|.
      INSERT temp450 INTO TABLE lt_xml.
    ENDLOOP.
    INSERT `</sheetData></worksheet>` INTO TABLE lt_xml.

    
    CLEAR temp451.
    
    temp452-name = `[Content_Types].xml`.
    temp452-content = conv_get_xstring_by_string(
|{ lc_xml }<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">|
&& |<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>|
&& |<Default Extension="xml" ContentType="application/xml"/>|
&& |<Override PartName="/xl/workbook.xml" ContentType="{ lc_ct }.sheet.main+xml"/>|
&& |<Override PartName="/xl/worksheets/sheet1.xml" ContentType="{ lc_ct }.worksheet+xml"/>|
&& |</Types>| ).
    INSERT temp452 INTO TABLE temp451.
    temp452-name = `_rels/.rels`.
    temp452-content = conv_get_xstring_by_string(
|{ lc_xml }<Relationships xmlns="{ lc_pkg_rel }">|
&& |<Relationship Id="rId1" Type="{ lc_rel }/officeDocument" Target="xl/workbook.xml"/>|
&& |</Relationships>| ).
    INSERT temp452 INTO TABLE temp451.
    temp452-name = `xl/workbook.xml`.
    temp452-content = conv_get_xstring_by_string(
|{ lc_xml }<workbook xmlns="{ lc_main }" xmlns:r="{ lc_rel }">|
&& |<sheets><sheet name="Sheet1" sheetId="1" r:id="rId1"/></sheets></workbook>| ).
    INSERT temp452 INTO TABLE temp451.
    temp452-name = `xl/_rels/workbook.xml.rels`.
    temp452-content = conv_get_xstring_by_string(
|{ lc_xml }<Relationships xmlns="{ lc_pkg_rel }">|
&& |<Relationship Id="rId1" Type="{ lc_rel }/worksheet" Target="worksheets/sheet1.xml"/>|
&& |</Relationships>| ).
    INSERT temp452 INTO TABLE temp451.
    temp452-name = `xl/worksheets/sheet1.xml`.
    temp452-content = conv_get_xstring_by_string( concat_lines_of( lt_xml ) ).
    INSERT temp452 INTO TABLE temp451.
    
    lt_files = temp451.

    result = zip_pack( lt_files ).

  ENDMETHOD.

  METHOD xlsx_get_column_kind.

    IF type->kind <> cl_abap_typedescr=>kind_elem.
      RETURN.
    ENDIF.
    IF type->type_kind = cl_abap_typedescr=>typekind_date.
      result = `D`.
    ELSEIF type->type_kind = cl_abap_typedescr=>typekind_time.
      result = `T`.
    ELSEIF rtti_check_typekind_number( type->type_kind ) = abap_true.
      result = `N`.
    ELSE.
      " a NUMC is text - its leading zeros are the point of it
      result = `S`.
    ENDIF.

  ENDMETHOD.

  METHOD xlsx_get_text_cell.

    IF val IS INITIAL.
      result = `<c/>`.
      RETURN.
    ENDIF.
    result = |<c t="inlineStr"><is><t xml:space="preserve">{ c_escape_xml( val ) }</t></is></c>|.

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
    " a success - the XCO version above is the template. The reader of
    " this catalog is zabaputil_cl_util_xlsx (get_table, get_table_ref),
    " which calls this class and so cannot be called from here
    RAISE EXCEPTION TYPE zabaputil_cx_util_error
      EXPORTING
        val = `CONV_GET_ITAB_BY_XLSX is not implemented`.

  ENDMETHOD.

  METHOD zip_pack.

    DATA lo_zip TYPE REF TO object.
        DATA ls_file LIKE LINE OF files.
        DATA x TYPE REF TO cx_root.

    TRY.

        CREATE OBJECT lo_zip TYPE ('CL_ABAP_ZIP').
        
        LOOP AT files INTO ls_file.
          CALL METHOD lo_zip->('ADD')
            EXPORTING
              name    = ls_file-name
              content = ls_file-content.
        ENDLOOP.
        CALL METHOD lo_zip->('SAVE')
          RECEIVING
            zip = result.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD xml_reader_create.
        DATA x TYPE REF TO cx_root.

    TRY.
        " no option set: as it comes, the reader reports a text of blanks
        " only where it is the whole content of an element and leaves out
        " the indentation between two elements - what a caller wants
        result = cl_sxml_string_reader=>create( val ).
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `XML_READER_ERROR - the document cannot be read`
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD xml_reader_next.

    DATA lo_reader TYPE REF TO if_sxml_reader.
                DATA temp453 TYPE zabaputil_cl_util_context=>ty_s_xml_attribute.
        DATA x TYPE REF TO cx_root.

    CLEAR kind.
    CLEAR name.
    CLEAR value.
    CLEAR t_attribute.

    TRY.
        lo_reader ?= reader.
        lo_reader->next_node( ).

        CASE lo_reader->node_type.
          WHEN if_sxml_node=>co_nt_element_open.
            kind = cs_xml_node-open.
            name = lo_reader->name.
            IF t_attribute IS SUPPLIED.
              " next_attribute( ) walks the attributes of the element the
              " reader stands on and leaves the attribute kind behind the
              " last one - the way that is there since 7.02
              DO.
                lo_reader->next_attribute( ).
                IF lo_reader->node_type <> if_sxml_node=>co_nt_attribute.
                  EXIT.
                ENDIF.
                
                CLEAR temp453.
                temp453-name = lo_reader->name.
                temp453-value = lo_reader->value.
                INSERT temp453 INTO TABLE t_attribute.
              ENDDO.
            ENDIF.
          WHEN if_sxml_node=>co_nt_element_close.
            kind = cs_xml_node-close.
            name = lo_reader->name.
          WHEN if_sxml_node=>co_nt_value.
            kind = cs_xml_node-value.
            value = lo_reader->value.
          WHEN OTHERS.
            " co_nt_final, and whatever a later release may add
            kind = cs_xml_node-end.
        ENDCASE.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `XML_READER_ERROR - the document is not well-formed XML`
            previous = x.
    ENDTRY.

  ENDMETHOD.

  METHOD zip_unpack.

    CONSTANTS lc_signature TYPE x LENGTH 2 VALUE '504B'.
    DATA lo_zip    TYPE REF TO object.
    DATA lv_name   TYPE string.
    DATA lv_error  TYPE string.
    DATA ls_result LIKE LINE OF result.

    FIELD-SYMBOLS <files> TYPE ANY TABLE.
    FIELD-SYMBOLS <file>  TYPE any.
    FIELD-SYMBOLS <name>  TYPE any.
        DATA x TYPE REF TO cx_root.

    " every ZIP archive starts with PK. Checked here, for the same answer
    " on every runtime: what the ZIP class does with other bytes differs,
    " and the transpiled one takes them without a word
    IF xstrlen( val ) < 4 OR val(2) <> lc_signature.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `ZIP_PARSE_ERROR - the data is no ZIP archive`.
    ENDIF.

    TRY.

        CREATE OBJECT lo_zip TYPE ('CL_ABAP_ZIP').
        " LOAD and GET report a broken archive with a classic exception -
        " not taken here, it ends the program with a dump, and a file a
        " user uploads is broken often enough (cut off, or damaged)
        CALL METHOD lo_zip->('LOAD')
          EXPORTING
            zip             = val
          EXCEPTIONS
            zip_parse_error = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
          lv_error = `ZIP_PARSE_ERROR - the data is no ZIP archive`.
        ELSE.
          ASSIGN lo_zip->('FILES') TO <files>.
          LOOP AT <files> ASSIGNING <file>.
            ASSIGN COMPONENT `NAME` OF STRUCTURE <file> TO <name>.
            lv_name = <name>.

            CLEAR ls_result.
            ls_result-name = lv_name.
            CALL METHOD lo_zip->('GET')
              EXPORTING
                name                    = lv_name
              IMPORTING
                content                 = ls_result-content
              EXCEPTIONS
                zip_index_error         = 1
                zip_decompression_error = 2
                OTHERS                  = 3.
            IF sy-subrc <> 0.
              lv_error = |ZIP_PARSE_ERROR - the file { lv_name } of the archive cannot be unpacked|.
              EXIT.
            ENDIF.
            INSERT ls_result INTO TABLE result.
          ENDLOOP.
        ENDIF.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `ZIP_PARSE_ERROR - the ZIP archive cannot be read`
            previous = x.
    ENDTRY.

    " raised behind the TRY: in it, the handler above would wrap it again
    IF lv_error IS NOT INITIAL.
      CLEAR result.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = lv_error.
    ENDIF.

  ENDMETHOD.

  METHOD zip_load.

    CONSTANTS lc_signature TYPE x LENGTH 2 VALUE '504B'.
    DATA lv_error TYPE string.
        DATA x TYPE REF TO cx_root.

    IF xstrlen( val ) < 4 OR val(2) <> lc_signature.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `ZIP_PARSE_ERROR - the data is no ZIP archive`.
    ENDIF.

    TRY.
        CREATE OBJECT result TYPE ('CL_ABAP_ZIP').
        " a broken archive is reported with a classic exception, which
        " would end the program with a dump
        CALL METHOD result->('LOAD')
          EXPORTING
            zip             = val
          EXCEPTIONS
            zip_parse_error = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
          lv_error = `ZIP_PARSE_ERROR - the data is no ZIP archive`.
        ENDIF.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = `ZIP_PARSE_ERROR - the ZIP archive cannot be read`
            previous = x.
    ENDTRY.
    IF lv_error IS NOT INITIAL.
      CLEAR result.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = lv_error.
    ENDIF.

  ENDMETHOD.

  METHOD zip_names.

    FIELD-SYMBOLS <files> TYPE ANY TABLE.
    FIELD-SYMBOLS <file>  TYPE any.
    FIELD-SYMBOLS <name>  TYPE any.
      DATA temp454 LIKE LINE OF result.

    ASSIGN zip->('FILES') TO <files>.
    IF <files> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    LOOP AT <files> ASSIGNING <file>.
      ASSIGN COMPONENT `NAME` OF STRUCTURE <file> TO <name>.
      
      temp454 = <name>.
      INSERT temp454 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD zip_get.

    DATA lv_name TYPE string.
        DATA lv_subrc LIKE sy-subrc.
        DATA x TYPE REF TO cx_root.

    lv_name = name.
    TRY.
        CALL METHOD zip->('GET')
          EXPORTING
            name                    = lv_name
          IMPORTING
            content                 = result
          EXCEPTIONS
            zip_index_error         = 1
            zip_decompression_error = 2
            OTHERS                  = 3.
        
        lv_subrc = sy-subrc.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |ZIP_PARSE_ERROR - the file { lv_name } of the archive cannot be unpacked|
            previous = x.
    ENDTRY.
    IF lv_subrc <> 0.
      CLEAR result.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |ZIP_PARSE_ERROR - the file { lv_name } of the archive cannot be unpacked|.
    ENDIF.

  ENDMETHOD.

  METHOD gzip_compress.

    DATA lv_class TYPE c LENGTH 12.
        DATA x TYPE REF TO cx_root.

    TRY.

        lv_class = `CL_ABAP_GZIP`.
        CALL METHOD (lv_class)=>compress_binary
          EXPORTING
            raw_in   = val
          IMPORTING
            gzip_out = result.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD gzip_decompress.

    DATA lv_class TYPE c LENGTH 12.
        DATA x TYPE REF TO cx_root.

    TRY.

        lv_class = `CL_ABAP_GZIP`.
        CALL METHOD (lv_class)=>decompress_binary
          EXPORTING
            gzip_in = val
          IMPORTING
            raw_out = result.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error EXPORTING val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD zlib_compress.

    " an xstring has no append operator: the three parts in hex, then one
    " conversion (conv_get_xstring_by_bintab does the same)
    DATA lv_hex TYPE string.

    DATA temp455 TYPE string.
    DATA temp682 TYPE string.
    temp455 = gzip_compress( val ).
    
    temp682 = hash_adler32( val ).
    lv_hex = `789C` && temp455 && temp682.
    result = lv_hex.

  ENDMETHOD.

  METHOD zlib_decompress.

    DATA lv_cmf TYPE i.
    DATA lv_flg TYPE i.
    DATA lv_off TYPE i VALUE 2.

    DATA lv_len TYPE i.
    DATA lv_deflate TYPE xstring.
    lv_len = xstrlen( val ).
    IF lv_len < 6.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `ZLIB_DECOMPRESS: not a zlib stream`.
    ENDIF.

    " CMF: the low four bits name the method, 8 is DEFLATE. FLG: bit 5
    " announces a preset dictionary id of four bytes, and the two bytes
    " together are a multiple of 31
    lv_cmf = val(1).
    lv_flg = val+1(1).
    IF lv_cmf MOD 16 <> 8 OR ( lv_cmf * 256 + lv_flg ) MOD 31 <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `ZLIB_DECOMPRESS: not a zlib stream`.
    ENDIF.
    IF ( lv_flg DIV 32 ) MOD 2 = 1.
      lv_off = 6.
    ENDIF.

    lv_len = lv_len - lv_off - 4.
    IF lv_len <= 0.
      RETURN.
    ENDIF.
    
    lv_deflate = val+lv_off(lv_len).
    result = gzip_decompress( lv_deflate ).

  ENDMETHOD.

  METHOD hash_adler32.

    DATA lv_a    TYPE i VALUE 1.
    DATA lv_b    TYPE i.
    DATA lv_byte TYPE i.
    DATA lv_off  TYPE i.
    DATA lv_xa   TYPE x LENGTH 2.
    DATA lv_xb   TYPE x LENGTH 2.
    DATA lv_hex  TYPE string.

    " the modulo only every 2000 bytes: until then both sums stay below
    " 2^31 (b grows by at most 2000 * 575520), and the loop is half as long
    DATA lv_len TYPE i.
    DATA temp456 TYPE string.
    DATA temp683 TYPE string.
    lv_len = xstrlen( val ).
    WHILE lv_off < lv_len.
      lv_byte = val+lv_off(1).
      lv_a = lv_a + lv_byte.
      lv_b = lv_b + lv_a.
      lv_off = lv_off + 1.
      IF lv_off MOD 2000 = 0.
        lv_a = lv_a MOD 65521.
        lv_b = lv_b MOD 65521.
      ENDIF.
    ENDWHILE.

    lv_xa = lv_a MOD 65521.
    lv_xb = lv_b MOD 65521.
    
    temp456 = lv_xb.
    
    temp683 = lv_xa.
    lv_hex = temp456 && temp683.
    result = lv_hex.

  ENDMETHOD.

  METHOD c_get_codepoints.

    DATA lv_x     TYPE xstring.
    DATA lv_byte  TYPE i.
    DATA lv_cp    TYPE i.
    DATA lv_more  TYPE i.
    DATA lv_off   TYPE i.
    DATA lv_high  TYPE i.
    DATA lv_lines TYPE i.

    " the UTF-8 bytes, decoded by hand: the one conversion every release
    " and the transpiler have, and no character-code API is needed
    DATA temp457 TYPE string.
    DATA lv_len TYPE i.
    temp457 = val.
    lv_x = conv_get_xstring_by_string( temp457 ).
    
    lv_len = xstrlen( lv_x ).

    WHILE lv_off < lv_len.

      lv_byte = lv_x+lv_off(1).
      lv_off = lv_off + 1.
      IF lv_byte < 128.
        lv_cp = lv_byte.
        lv_more = 0.
      ELSEIF lv_byte < 224.
        lv_cp = lv_byte MOD 32.
        lv_more = 1.
      ELSEIF lv_byte < 240.
        lv_cp = lv_byte MOD 16.
        lv_more = 2.
      ELSE.
        lv_cp = lv_byte MOD 8.
        lv_more = 3.
      ENDIF.

      DO lv_more TIMES.
        IF lv_off >= lv_len.
          EXIT.
        ENDIF.
        lv_byte = lv_x+lv_off(1).
        lv_off = lv_off + 1.
        lv_cp = lv_cp * 64 + ( lv_byte MOD 64 ).
      ENDDO.

      " a converter that writes a surrogate pair as two three-byte
      " sequences (CESU-8): the low half joins the high half before it
      IF lv_cp >= 56320 AND lv_cp <= 57343 AND lv_high > 0.
        lv_cp = 65536 + ( lv_high - 55296 ) * 1024 + ( lv_cp - 56320 ).
        lv_lines = lines( result ).
        DELETE result INDEX lv_lines.
      ENDIF.
      IF lv_cp >= 55296 AND lv_cp <= 56319.
        lv_high = lv_cp.
      ELSE.
        lv_high = 0.
      ENDIF.

      INSERT lv_cp INTO TABLE result.

    ENDWHILE.

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
          DATA temp458 TYPE xstring.
    DATA lv_i TYPE i.
      DATA lv_code TYPE string.
      DATA temp459 TYPE string.
      DATA lv_escape LIKE temp459.

    IF gv_json_specials IS INITIAL.
      TRY.
          
          temp458 = lc_ctrl_hex.
          gv_json_ctrl = conv_get_string_by_xstring( temp458 ).
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
    
    lv_i = 0.
    WHILE lv_i < strlen( gv_json_ctrl ).
      
      lv_code = substring( val = lc_ctrl_hex
                                 off = lv_i * 2
                                 len = 2 ).
      
      CASE lv_code.
        WHEN `08`.
          temp459 = `\b`.
        WHEN `0C`.
          temp459 = `\f`.
        WHEN OTHERS.
          temp459 = `\u00` && lv_code.
      ENDCASE.
      
      lv_escape = temp459.
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

  METHOD c_unescape_xml.

    DATA lv_pos TYPE i.

    DATA temp460 TYPE string.
    DATA lv_val LIKE temp460.
    DATA lv_len TYPE i.
      DATA lv_amp TYPE i.
      DATA lv_semi TYPE i.
      DATA lv_char TYPE string.
    temp460 = val.
    
    lv_val = temp460.
    IF lv_val NS `&`.
      result = lv_val.
      RETURN.
    ENDIF.

    
    lv_len = strlen( lv_val ).
    WHILE lv_pos < lv_len.
      
      lv_amp = find( val = lv_val
                           sub = `&`
                           off = lv_pos ).
      IF lv_amp < 0.
        result = result && substring( val = lv_val
                                      off = lv_pos ).
        RETURN.
      ENDIF.
      result = result && substring( val = lv_val
                                    off = lv_pos
                                    len = lv_amp - lv_pos ).

      " a reference is short: `&#x10FFFF;` is the longest one there is
      
      lv_semi = find( val = lv_val
                            sub = `;`
                            off = lv_amp ).
      
      lv_char = ``.
      IF lv_semi > lv_amp + 1 AND lv_semi - lv_amp <= 10.
        lv_char = c_unescape_xml_reference( substring( val = lv_val
                                                       off = lv_amp + 1
                                                       len = lv_semi - lv_amp - 1 ) ).
      ENDIF.
      IF lv_char IS INITIAL.
        result = result && `&`.
        lv_pos = lv_amp + 1.
      ELSE.
        result = result && lv_char.
        lv_pos = lv_semi + 1.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD c_unescape_xml_reference.

    DATA lv_code   TYPE i.
    DATA lv_digits TYPE string.
    DATA lv_hex    TYPE string.
    DATA lv_byte   TYPE i.
    DATA lv_x      TYPE x LENGTH 1.
    TYPES temp23 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lt_byte   TYPE temp23.
      DATA lv_i TYPE i.
      DATA temp461 LIKE LINE OF lt_byte.
      DATA temp462 LIKE LINE OF lt_byte.
      DATA temp463 LIKE LINE OF lt_byte.
      DATA temp464 LIKE LINE OF lt_byte.
      DATA temp465 LIKE LINE OF lt_byte.
      DATA temp466 LIKE LINE OF lt_byte.
      DATA temp467 LIKE LINE OF lt_byte.
      DATA temp468 LIKE LINE OF lt_byte.
      DATA temp469 LIKE LINE OF lt_byte.
        DATA temp470 TYPE xstring.

    CASE name.
      WHEN `lt`.
        result = `<`.
        RETURN.
      WHEN `gt`.
        result = `>`.
        RETURN.
      WHEN `amp`.
        result = `&`.
        RETURN.
      WHEN `quot`.
        result = `"`.
        RETURN.
      WHEN `apos`.
        result = `'`.
        RETURN.
    ENDCASE.

    IF strlen( name ) < 2 OR name(1) <> `#`.
      RETURN.
    ENDIF.
    IF name+1(1) = `x` OR name+1(1) = `X`.
      lv_digits = to_upper( substring( val = name
                                       off = 2 ) ).
      IF lv_digits IS INITIAL OR lv_digits CN `0123456789ABCDEF` OR strlen( lv_digits ) > 6.
        RETURN.
      ENDIF.
      
      lv_i = 0.
      WHILE lv_i < strlen( lv_digits ).
        lv_code = lv_code * 16 + find( val = `0123456789ABCDEF`
                                       sub = lv_digits+lv_i(1) ).
        lv_i = lv_i + 1.
      ENDWHILE.
    ELSE.
      lv_digits = substring( val = name
                             off = 1 ).
      IF lv_digits CN `0123456789` OR strlen( lv_digits ) > 7.
        RETURN.
      ENDIF.
      lv_code = lv_digits.
    ENDIF.

    " what XML 1.0 allows: tab, LF, CR, from U+0020 on - no surrogate
    " halves, no U+FFFE and U+FFFF, nothing above U+10FFFF
    IF lv_code < 32 AND lv_code <> 9 AND lv_code <> 10 AND lv_code <> 13.
      RETURN.
    ENDIF.
    IF ( lv_code >= 55296 AND lv_code <= 57343 ) OR lv_code = 65534 OR lv_code = 65535 OR lv_code > 1114111.
      RETURN.
    ENDIF.

    " the code point as UTF-8 bytes, decoded by the codepage API
    IF lv_code < 128.
      INSERT lv_code INTO TABLE lt_byte.
    ELSEIF lv_code < 2048.
      
      temp461 = 192 + lv_code DIV 64.
      INSERT temp461 INTO TABLE lt_byte.
      
      temp462 = 128 + lv_code MOD 64.
      INSERT temp462 INTO TABLE lt_byte.
    ELSEIF lv_code < 65536.
      
      temp463 = 224 + lv_code DIV 4096.
      INSERT temp463 INTO TABLE lt_byte.
      
      temp464 = 128 + ( lv_code DIV 64 ) MOD 64.
      INSERT temp464 INTO TABLE lt_byte.
      
      temp465 = 128 + lv_code MOD 64.
      INSERT temp465 INTO TABLE lt_byte.
    ELSE.
      
      temp466 = 240 + lv_code DIV 262144.
      INSERT temp466 INTO TABLE lt_byte.
      
      temp467 = 128 + ( lv_code DIV 4096 ) MOD 64.
      INSERT temp467 INTO TABLE lt_byte.
      
      temp468 = 128 + ( lv_code DIV 64 ) MOD 64.
      INSERT temp468 INTO TABLE lt_byte.
      
      temp469 = 128 + lv_code MOD 64.
      INSERT temp469 INTO TABLE lt_byte.
    ENDIF.
    LOOP AT lt_byte INTO lv_byte.
      lv_x = lv_byte.
      lv_hex = |{ lv_hex }{ lv_x }|.
    ENDLOOP.
    TRY.
        
        temp470 = lv_hex.
        result = conv_get_string_by_xstring( temp470 ).
      CATCH zabaputil_cx_util_error.
        " no codepage API: the reference stays as it is written
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD c_escape_xml.
          DATA temp471 TYPE xstring.
      DATA lv_off TYPE i.
      DATA lv_len TYPE i.

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
          
          temp471 = `000102030405060708` && `0B0C` && `0E0F101112131415161718191A1B1C1D1E1F` && `EFBFBE` && `EFBFBF`.
          gv_xml_escape_controls = conv_get_string_by_xstring(
              temp471 ).
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
      
      lv_off = 0.
      
      lv_len = strlen( gv_xml_escape_controls ).
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
    DATA temp472 TYPE string.
    DATA lv_val LIKE temp472.
    DATA temp473 TYPE string.
    DATA lv_allowed LIKE temp473.
    DATA lv_len TYPE i.
    DATA lv_off TYPE i.
    temp472 = val.
    
    lv_val = temp472.
    
    temp473 = allowed.
    
    lv_allowed = temp473.
    IF lv_val CO lv_allowed.
      result = lv_val.
      RETURN.
    ENDIF.
    
    lv_len = strlen( lv_val ).
    
    lv_off = 0.
    WHILE lv_off < lv_len.
      IF lv_val+lv_off(1) CO lv_allowed.
        result = result && lv_val+lv_off(1).
      ENDIF.
      lv_off = lv_off + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD c_cut_at.
    DATA lv_off TYPE i.

    result = val.
    
    lv_off = find( val = result
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
    DATA temp474 TYPE string.
    DATA lv_url LIKE temp474.
    DATA lv_scheme TYPE string.
    DATA lv_end TYPE i.
    DATA lv_off TYPE i.
    DATA lv_host TYPE string.
    temp474 = val.
    
    lv_url = temp474.
    
    lv_scheme = ``.
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
    
    lv_end = strlen( lv_url ).
    
    lv_off = 0.
    WHILE lv_off < lv_end.
      IF lv_url+lv_off(1) CA `/?#`.
        lv_end = lv_off.
      ELSE.
        lv_off = lv_off + 1.
      ENDIF.
    ENDWHILE.
    
    lv_host = lv_url(lv_end).
    IF lv_host IS INITIAL OR lv_host CA `@`.
      RETURN.
    ENDIF.
    result = lv_scheme && lv_host.

  ENDMETHOD.

  METHOD url_get_authority.
    DATA lv_rest TYPE string.
    DATA lv_len TYPE i.
    DATA lv_tail TYPE string.

    result = to_lower( val ).

    " drop the scheme (e.g. `https://`)
    IF result CS `://`.
      result = substring_after( val = result
                                sub = `://` ).
    ENDIF.

    " the authority ends at the first path / query / fragment separator
    
    SPLIT result AT `/` INTO result lv_rest.
    SPLIT result AT `?` INTO result lv_rest.
    SPLIT result AT `#` INTO result lv_rest.

    " a default port is no port: a browser writes `Origin: https://host`
    " while a proxy forwards `host:443`, and the two are the same
    " authority. Both defaults are stripped whatever the scheme was: a Host
    " value carries none to compare with
    
    
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
    DATA temp475 TYPE string.
    DATA lv_etag LIKE temp475.
    DATA temp476 TYPE string.
    DATA lv_header LIKE temp476.
    DATA lv_gzip TYPE string.
    TYPES temp24 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_candidate TYPE temp24.
    DATA lv_candidate LIKE LINE OF lt_candidate.
    temp475 = etag.
    
    lv_etag = temp475.
    
    temp476 = if_none_match.
    
    lv_header = temp476.
    IF lv_header IS INITIAL OR lv_etag IS INITIAL.
      RETURN.
    ENDIF.
    
    lv_gzip = substring( val = lv_etag
                               len = strlen( lv_etag ) - 1 ) && `-gzip"`.
    

    SPLIT lv_header AT `,` INTO TABLE lt_candidate.
    
    LOOP AT lt_candidate INTO lv_candidate.
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
        DATA temp477 TYPE string.
    DATA lv_len TYPE i.
    DATA lv_h1 TYPE i VALUE 5381.
    DATA lv_h2 TYPE i VALUE 17.
    DATA lv_h3 TYPE i VALUE 104729.
    DATA lv_byte TYPE i.
    DATA lv_off TYPE i.
    TRY.
        
        temp477 = val.
        lv_xstr = conv_get_xstring_by_string( temp477 ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    
    lv_len = xstrlen( lv_xstr ).
    
    
    
    
    
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

    TYPES ty_t_int TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DATA temp478 TYPE string.
    DATA lv_a LIKE temp478.
    DATA temp479 TYPE string.
    DATA lv_b LIKE temp479.
    DATA lv_la TYPE i.
    DATA lv_lb TYPE i.
    DATA lt_prev TYPE ty_t_int.
    DATA lt_curr TYPE ty_t_int.
    DATA lv_j TYPE i.
    DATA lv_i TYPE i.
      DATA temp480 LIKE LINE OF lt_curr.
        DATA temp481 TYPE i.
        DATA lv_cost LIKE temp481.
        DATA lv_min TYPE i.
        FIELD-SYMBOLS <temp684> LIKE LINE OF lt_curr.
        DATA temp685 LIKE sy-tabix.
        DATA lv_del TYPE i.
        FIELD-SYMBOLS <temp686> LIKE LINE OF lt_prev.
        DATA temp687 LIKE sy-tabix.
        DATA lv_sub TYPE i.
        FIELD-SYMBOLS <temp688> LIKE LINE OF lt_prev.
        DATA temp689 LIKE sy-tabix.
    FIELD-SYMBOLS <temp482> LIKE LINE OF lt_prev.
    DATA temp483 LIKE sy-tabix.
    temp478 = val1.
    
    lv_a = temp478.
    
    temp479 = val2.
    
    lv_b = temp479.
    
    lv_la = strlen( lv_a ).
    
    lv_lb = strlen( lv_b ).

    IF lv_la = 0.
      result = lv_lb.
      RETURN.
    ENDIF.
    IF lv_lb = 0.
      result = lv_la.
      RETURN.
    ENDIF.

    
    
    

    WHILE lv_j <= lv_lb.
      INSERT lv_j INTO TABLE lt_prev.
      lv_j = lv_j + 1.
    ENDWHILE.

    
    WHILE lv_i < lv_la.

      CLEAR lt_curr.
      
      temp480 = lv_i + 1.
      INSERT temp480 INTO TABLE lt_curr.

      lv_j = 0.
      WHILE lv_j < lv_lb.

        
        IF lv_a+lv_i(1) = lv_b+lv_j(1).
          temp481 = 0.
        ELSE.
          temp481 = 1.
        ENDIF.
        
        lv_cost = temp481.
        
        
        
        temp685 = sy-tabix.
        READ TABLE lt_curr INDEX lv_j + 1 ASSIGNING <temp684>.
        sy-tabix = temp685.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_min = <temp684> + 1.
        
        
        
        temp687 = sy-tabix.
        READ TABLE lt_prev INDEX lv_j + 2 ASSIGNING <temp686>.
        sy-tabix = temp687.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_del = <temp686> + 1.
        IF lv_del < lv_min.
          lv_min = lv_del.
        ENDIF.
        
        
        
        temp689 = sy-tabix.
        READ TABLE lt_prev INDEX lv_j + 1 ASSIGNING <temp688>.
        sy-tabix = temp689.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_sub = <temp688> + lv_cost.
        IF lv_sub < lv_min.
          lv_min = lv_sub.
        ENDIF.
        INSERT lv_min INTO TABLE lt_curr.

        lv_j = lv_j + 1.
      ENDWHILE.

      lt_prev = lt_curr.
      lv_i = lv_i + 1.
    ENDWHILE.

    
    
    temp483 = sy-tabix.
    READ TABLE lt_prev INDEX lv_lb + 1 ASSIGNING <temp482>.
    sy-tabix = temp483.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    result = <temp482>.

  ENDMETHOD.

  METHOD url_encode.

    CONSTANTS lc_unreserved TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~`.

    DATA temp484 TYPE string.
    DATA lv_val LIKE temp484.
    DATA lv_len TYPE i.
    DATA lv_i TYPE i.
    DATA lv_run TYPE string.
    DATA lv_j TYPE i.
    DATA lv_hex TYPE c LENGTH 2.
        DATA lv_char TYPE string.
        DATA lv_x TYPE xstring.
    temp484 = val.
    
    lv_val = temp484.
    
    lv_len = strlen( lv_val ).
    
    
    
    

    " the characters to encode are collected and converted to UTF-8 as one
    " run: one at a time split a character outside the BMP (an emoji) into
    " its two UTF-16 halves, and each half alone is no valid UTF-8
    WHILE lv_i <= lv_len.

      IF lv_i < lv_len.
        
        lv_char = substring( val = lv_val
                                   off = lv_i
                                   len = 1 ).
        IF lv_char = ` ` OR lv_char CN lc_unreserved.
          lv_run = lv_run && lv_char.
          lv_i = lv_i + 1.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF lv_run IS NOT INITIAL.
        
        lv_x = conv_get_xstring_by_string( lv_run ).
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

    DATA temp485 TYPE string.
    DATA lv_val LIKE temp485.
    DATA lv_len TYPE i.
    DATA lv_i TYPE i.
    DATA lv_xbuf TYPE string.
    DATA lv_raw TYPE string.
      DATA lv_char TYPE string.
      DATA lv_hex TYPE string.
        DATA lv_x TYPE x LENGTH 1.
    temp485 = val.
    
    lv_val = temp485.
    
    lv_len = strlen( lv_val ).
    
    " the pending bytes as hex digits - a byte string has no expression
    " that appends to it, the hex text converts back in one step
    
    " the same escapes as written - what comes out when they are no UTF-8
    

    WHILE lv_i < lv_len.

      
      lv_char = substring( val = lv_val
                                 off = lv_i
                                 len = 1 ).

      " only two hex digits make an escape - `%ZZ` stays as it is instead
      " of becoming a NUL byte
      
      lv_hex = ``.
      IF lv_char = `%` AND lv_i + 2 < lv_len.
        lv_hex = to_upper( substring( val = lv_val
                                      off = lv_i + 1
                                      len = 2 ) ).
      ENDIF.
      IF lv_hex IS NOT INITIAL AND lv_hex CO `0123456789ABCDEF`.
        
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
    DATA temp486 TYPE xstring.
    temp486 = hex.
    conv_get_string_by_xstring_cp( EXPORTING val      = temp486
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

    DATA lo_matcher TYPE REF TO object.
    lo_matcher = regex_create_matcher( val   = val
                                             regex = regex ).

    CALL METHOD lo_matcher->(`MATCH`)
      RECEIVING
        success = result.

  ENDMETHOD.

  METHOD regex_find_all.

    DATA temp487 TYPE string.
    DATA lv_val LIKE temp487.
    DATA temp488 TYPE zabaputil_cl_util_context=>ty_t_regex_match.
    DATA ls_match LIKE LINE OF temp488.
      DATA temp1 TYPE string.
    temp487 = val.
    
    lv_val = temp487.
    
    temp488 = regex_get_matches( val = lv_val regex = regex ).
    
    LOOP AT temp488 INTO ls_match.
      
      temp1 = substring( val = lv_val off = ls_match-off len = ls_match-len ).
      INSERT temp1 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD regex_replace_all.

    DATA lv_pos TYPE i.

    DATA temp489 TYPE string.
    DATA lv_val LIKE temp489.
    DATA temp490 TYPE zabaputil_cl_util_context=>ty_t_regex_match.
    DATA ls_match LIKE LINE OF temp490.
    temp489 = val.
    
    lv_val = temp489.
    
    temp490 = regex_get_matches( val = lv_val regex = regex ).
    
    LOOP AT temp490 INTO ls_match.
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

    DATA lo_matcher TYPE REF TO object.
      DATA temp491 TYPE zabaputil_cl_util_context=>ty_s_regex_match.
    lo_matcher = regex_create_matcher( val   = val
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

      
      CLEAR temp491.
      temp491-off = lv_off.
      temp491-len = lv_len.
      INSERT temp491 INTO TABLE result.
    ENDDO.

  ENDMETHOD.

  METHOD uuid_get_c36.

    result = uuid_conv_c32_to_c36( uuid_get_c32( ) ).

  ENDMETHOD.

  METHOD uuid_conv_c32_to_c36.

    DATA lv_c32 TYPE string.
    lv_c32 = to_upper( c_trim( val ) ).

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

    DATA lv_c32 TYPE string.
    DATA lv_x TYPE xstring.
    DATA lv_i TYPE i.
    DATA lv_xlen TYPE i.
      DATA temp492 TYPE i.
      DATA lv_b1 LIKE temp492.
      DATA lv_b2 TYPE i.
      DATA lv_b3 TYPE i.
        DATA lv_i2 TYPE i.
        DATA temp493 TYPE i.
      DATA lv_n TYPE i.
    lv_c32 = to_upper( c_trim( val ) ).

    IF strlen( lv_c32 ) <> 32.
      x_raise( `INVALID_UUID_C32` ).
    ENDIF.

    
    lv_x = lv_c32.

    
    
    lv_xlen = xstrlen( lv_x ).

    WHILE lv_i < lv_xlen.

      
      temp492 = lv_x+lv_i(1).
      
      lv_b1 = temp492.
      
      
      lv_b2 = -1.
      lv_b3 = -1.
      IF lv_i + 1 < lv_xlen.
        lv_b2 = lv_x+lv_i(2) MOD 256.
      ENDIF.
      IF lv_i + 2 < lv_xlen.
        
        lv_i2 = lv_i + 2.
        
        temp493 = lv_x+lv_i2(1).
        lv_b3 = temp493.
      ENDIF.

      
      lv_n = lv_b1 * 65536.
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

    DATA lv_c22 TYPE string.
    DATA lv_hex TYPE string.
    DATA lv_xb TYPE x LENGTH 1.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.
      DATA lv_take TYPE i.
      DATA lv_n TYPE i.
      DATA lv_k TYPE i.
        DATA lv_pos TYPE i.
        DATA lv_char TYPE string.
        DATA lv_idx TYPE i.
    lv_c22 = c_trim( val ).
    lv_c22 = replace( val = lv_c22 sub = `+` with = `-` occ = 0 ).
    lv_c22 = replace( val = lv_c22 sub = `/` with = `_` occ = 0 ).

    IF strlen( lv_c22 ) <> 22.
      x_raise( `INVALID_UUID_C22` ).
    ENDIF.

    " the 16 bytes as 32 hex digits, built byte by byte
    
    
    
    
    lv_len = strlen( lv_c22 ).

    WHILE lv_i < lv_len.

      
      lv_take = lv_len - lv_i.
      IF lv_take > 4.
        lv_take = 4.
      ENDIF.

      
      
      lv_n = 0.
      lv_k = 0.
      WHILE lv_k < lv_take.
        
        lv_pos = lv_i + lv_k.
        
        lv_char = substring( val = lv_c22
                                   off = lv_pos
                                   len = 1 ).
        
        lv_idx = find( val = lc_b64
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
    DATA lv_fieldname TYPE string.
      FIELD-SYMBOLS <val> TYPE any.
    lv_fieldname = to_upper( fieldname ).

    LOOP AT tab ASSIGNING <row>.
      
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <row> TO <val>.
      IF sy-subrc = 0.
        result = result + <val>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD itab_distinct.

    FIELD-SYMBOLS <row> TYPE any.
    " the values already in result - a line_exists( ) on result itself made
    " a near-unique column of n rows cost n * n / 2 comparisons
    TYPES temp25 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA lt_seen TYPE temp25.
    DATA lv_fieldname TYPE string.
      FIELD-SYMBOLS <val> TYPE any.
      DATA lv_str TYPE string.
    lv_fieldname = to_upper( fieldname ).

    LOOP AT tab ASSIGNING <row>.
      
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <row> TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      " a string template, not CONV: a plain move writes the sign of a
      " number behind it (`5-`), where itab_count_by and the UI write `-5`
      
      lv_str = |{ <val> }|.
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

    TYPES temp26 TYPE STANDARD TABLE OF ty_s_sum WITH DEFAULT KEY.
DATA lt_sum TYPE temp26.
    " group -> its row in lt_sum: a key read on lt_sum itself scanned every
    " group for every row
    TYPES temp27 TYPE HASHED TABLE OF ty_s_index WITH UNIQUE KEY n.
DATA lt_index TYPE temp27.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <sum> TYPE ty_s_sum.

    DATA lv_group_by TYPE string.
    DATA lv_sum_by TYPE string.
      FIELD-SYMBOLS <group> TYPE any.
      FIELD-SYMBOLS <val> TYPE any.
      DATA lv_group TYPE string.
      FIELD-SYMBOLS <index> TYPE ty_s_index.
        DATA temp494 TYPE ty_s_sum.
        DATA temp495 TYPE ty_s_index.
      DATA temp496 TYPE zabaputil_cl_util_context=>ty_s_name_value.
    lv_group_by = to_upper( group_by ).
    
    lv_sum_by = to_upper( sum_by ).

    LOOP AT tab ASSIGNING <row>.

      
      ASSIGN COMPONENT lv_group_by OF STRUCTURE <row> TO <group>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      
      ASSIGN COMPONENT lv_sum_by OF STRUCTURE <row> TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " a string template, not CONV - see itab_distinct
      
      lv_group = |{ <group> }|.
      
      READ TABLE lt_index WITH KEY n = lv_group ASSIGNING <index>.
      IF sy-subrc = 0.
        READ TABLE lt_sum INDEX <index>-idx ASSIGNING <sum>.
      ELSE.
        
        CLEAR temp494.
        temp494-n = lv_group.
        INSERT temp494 INTO TABLE lt_sum ASSIGNING <sum>.
        
        CLEAR temp495.
        temp495-n = lv_group.
        temp495-idx = lines( lt_sum ).
        INSERT temp495 INTO TABLE lt_index.
      ENDIF.
      <sum>-sum = <sum>-sum + <val>.

    ENDLOOP.

    LOOP AT lt_sum ASSIGNING <sum>.
      
      CLEAR temp496.
      temp496-n = <sum>-n.
      temp496-v = |{ <sum>-sum }|.
      INSERT temp496 INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD num_round.

    DATA lv_factor TYPE decfloat34 VALUE 1.
    DATA lv_sign TYPE decfloat34 VALUE 1.
    DATA lv_abs TYPE decfloat34.
    DATA lv_rounded TYPE decfloat34.
    DATA lv_half TYPE decfloat34 VALUE '0.5'.
    DATA temp497 TYPE string.
    DATA lv_mode TYPE string.
    DATA lv_frac TYPE decfloat34.

    DO decimals TIMES.
      lv_factor = lv_factor * 10.
    ENDDO.

    
    IF val < 0.
      lv_sign = -1.
    ENDIF.

    
    lv_abs = val * lv_sign * lv_factor.
    
    
    
    temp497 = mode.
    
    lv_mode = to_upper( temp497 ).

    lv_rounded = trunc( lv_abs ).

    
    lv_frac = frac( lv_abs ).

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

  METHOD file_get_name_by_path.

    " a browser hands over `C:\fakepath\name` on Windows, a path with `/`
    " elsewhere - both separators end a folder
    DATA temp498 TYPE string.
    DATA lv_slash TYPE i.
    temp498 = val.
    result = replace( val  = temp498
                      sub  = `\`
                      with = `/`
                      occ  = 0 ).
    
    lv_slash = find( val = result
                           sub = `/`
                           occ = -1 ).
    IF lv_slash >= 0.
      result = substring( val = result
                          off = lv_slash + 1 ).
    ENDIF.

  ENDMETHOD.

  METHOD file_get_mimetype.

    DATA lv_name TYPE string.
    TYPES temp28 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_parts TYPE temp28.
    DATA lv_ext LIKE LINE OF lt_parts.
    FIELD-SYMBOLS <temp690> LIKE LINE OF lt_parts.
    DATA temp691 LIKE sy-tabix.
    lv_name = to_lower( c_trim( val ) ).
    

    SPLIT lv_name AT `.` INTO TABLE lt_parts.
    " no name, no extension - an empty name splits into no part at all
    IF lt_parts IS INITIAL.
      result = `application/octet-stream`.
      RETURN.
    ENDIF.
    
    
    
    temp691 = sy-tabix.
    READ TABLE lt_parts INDEX lines( lt_parts ) ASSIGNING <temp690>.
    sy-tabix = temp691.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    lv_ext = <temp690>.

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
    DATA lv_sap TYPE string.
    TYPES temp29 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_pairs TYPE temp29.
    DATA lv_pair LIKE LINE OF lt_pairs.
      DATA lv_key TYPE string.
      DATA lv_iso TYPE string.
        DATA lv_tab TYPE string.
    lv_sap = c_trim( val ).

    

    SPLIT cv_lang_map AT `;` INTO TABLE lt_pairs.
    
    LOOP AT lt_pairs INTO lv_pair.
      
      
      SPLIT lv_pair AT `:` INTO lv_key lv_iso.
      IF lv_key = lv_sap.
        result = lv_iso.
        RETURN.
      ENDIF.
    ENDLOOP.

    TRY.
        
        lv_tab = `T002`.
        SELECT SINGLE laiso FROM (lv_tab) INTO result WHERE spras = lv_sap .
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
    DATA lv_iso TYPE string.
    TYPES temp30 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_pairs TYPE temp30.
    DATA lv_pair LIKE LINE OF lt_pairs.
      DATA lv_key TYPE string.
      DATA lv_map_iso TYPE string.
        DATA lv_tab TYPE string.
        DATA lv_laiso TYPE string.
    lv_iso = to_lower( c_trim( val ) ).

    

    SPLIT cv_lang_map AT `;` INTO TABLE lt_pairs.
    
    LOOP AT lt_pairs INTO lv_pair.
      
      
      SPLIT lv_pair AT `:` INTO lv_key lv_map_iso.
      IF lv_map_iso = lv_iso.
        result = lv_key.
        RETURN.
      ENDIF.
    ENDLOOP.

    TRY.
        
        lv_tab = `T002`.
        
        lv_laiso = to_upper( lv_iso ).
        SELECT SINGLE spras FROM (lv_tab) INTO result WHERE laiso = lv_laiso .
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

    CLEAR date.
    CLEAR time.
    lv_tz = time_check_zone( tz ).

    " 8: the zone is not in TTZZ, 12: the time stamp is no valid one
    CONVERT TIME STAMP val TIME ZONE lv_tz INTO DATE date TIME time.
    IF sy-subrc >= 8.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |TIME_STAMPL_TO_TZ: { val } cannot be converted to time zone { lv_tz } (sy-subrc { sy-subrc })|.
    ENDIF.

  ENDMETHOD.

  METHOD time_stampl_from_tz.

    DATA lv_tz TYPE c LENGTH 6.

    lv_tz = time_check_zone( tz ).

    " 8: the zone is not in TTZZ, 12: date or time is no valid one
    CONVERT DATE date TIME time INTO TIME STAMP result TIME ZONE lv_tz.
    IF sy-subrc >= 8.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |TIME_STAMPL_FROM_TZ: { date } { time } cannot be converted from time zone { lv_tz } (sy-subrc { sy-subrc })|.
    ENDIF.

  ENDMETHOD.

  METHOD time_check_zone.

    result = c_trim( tz ).
    IF strlen( result ) > 6.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |{ result } is no SAP time zone key (TTZZ, at most six characters)|.
    ENDIF.

  ENDMETHOD.

  METHOD context_get_user_info.

    DATA lv_class TYPE string.
    DATA lv_tab   TYPE string.

    DATA temp499 TYPE string.
    DATA temp692 TYPE string.
    DATA lv_uname LIKE temp692.
    DATA lv_current TYPE abap_bool.
    DATA temp38 TYPE xsdboolean.
            DATA lv_langu TYPE c LENGTH 1.
            DATA lv_tzone TYPE c LENGTH 6.
          DATA lv_persnumber TYPE c LENGTH 10.
          DATA lv_addrnumber TYPE c LENGTH 10.
    temp499 = sy-uname.
    
    IF uname IS NOT INITIAL.
      temp692 = to_upper( c_trim( uname ) ).
    ELSE.
      temp692 = temp499.
    ENDIF.
    
    lv_uname = temp692.

    result-uname = lv_uname.
    
    
    temp38 = boolc( lv_uname = sy-uname ).
    lv_current = temp38.
    IF lv_current = abap_true.
      result-langu    = sy-langu.
      result-timezone = time_get_user_timezone( ).
    ENDIF.

    IF check_abap_cloud( ) = abap_true.

      IF lv_current = abap_false.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |CONTEXT_GET_USER_INFO: on ABAP Cloud only for the current user, not { lv_uname }|.
      ENDIF.

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
          SELECT SINGLE name_textc FROM (lv_tab) INTO result-name_formatted WHERE bname = lv_uname .
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      TRY.
          lv_tab = `USR01`.
          SELECT SINGLE datfm FROM (lv_tab) INTO result-date_format WHERE bname = lv_uname .
          SELECT SINGLE dcpfm FROM (lv_tab) INTO result-decimal_format WHERE bname = lv_uname .
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      " another user's language and time zone, not the caller's
      IF lv_current = abap_false.
        TRY.
            
            
            lv_tab = `USR01`.
            SELECT SINGLE langu FROM (lv_tab) INTO lv_langu WHERE bname = lv_uname .
            result-langu = lv_langu.
            lv_tab = `USR02`.
            SELECT SINGLE tzone FROM (lv_tab) INTO lv_tzone WHERE bname = lv_uname .
            result-timezone = lv_tzone.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      TRY.
          
          
          lv_tab = `USR21`.
          SELECT SINGLE persnumber FROM (lv_tab) INTO lv_persnumber WHERE bname = lv_uname .
          SELECT SINGLE addrnumber FROM (lv_tab) INTO lv_addrnumber WHERE bname = lv_uname .
          IF sy-subrc = 0.
            lv_tab = `ADR6`.
            SELECT SINGLE smtp_addr FROM (lv_tab) INTO result-email WHERE persnumber = lv_persnumber AND addrnumber = lv_addrnumber .
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD cur_get_decimals.

    DATA lv_tab TYPE string.

    DATA lv_curr TYPE string.
    lv_curr = to_upper( c_trim( val ) ).
    result = 2.

    TRY.
        lv_tab = `TCURX`.
        SELECT SINGLE currdec FROM (lv_tab) INTO result WHERE currkey = lv_curr .
        IF sy-subrc <> 0.
          result = 2.
        ENDIF.
      CATCH cx_root.
        TRY.
            lv_tab = `I_CURRENCY`.
            SELECT SINGLE decimals FROM (lv_tab) INTO result WHERE currency = lv_curr .
            IF sy-subrc <> 0.
              result = 2.
            ENDIF.
          CATCH cx_root.
            result = 2.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.

  METHOD cur_amount_to_external.

    DATA lv_dec LIKE decimals.
    DATA lv_factor TYPE decfloat34 VALUE 1.
    lv_dec = decimals.
    IF lv_dec < 0.
      lv_dec = cur_get_decimals( currency ).
    ENDIF.

    

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

    DATA lv_dec LIKE decimals.
    DATA lv_factor TYPE decfloat34 VALUE 1.
    lv_dec = decimals.
    IF lv_dec < 0.
      lv_dec = cur_get_decimals( currency ).
    ENDIF.

    

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

    DATA lv_fm TYPE string.
      DATA lv_in TYPE f.
      DATA lv_out TYPE f.
      DATA lv_ui TYPE c LENGTH 3.
      DATA lv_uo TYPE c LENGTH 3.
      DATA lv_factor_from TYPE decfloat34.
      DATA lv_factor_to TYPE decfloat34.
      DATA lv_add_from TYPE decfloat34.
      DATA lv_add_to TYPE decfloat34.
      DATA lv_dim_from TYPE string.
      DATA lv_dim_to TYPE string.

    IF unit_from = unit_to.
      result = val.
      RETURN.
    ENDIF.

    IF check_abap_cloud( ) = abap_false.

      
      
      
      

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

      
      
      
      
      
      

      unit_get_si( EXPORTING unit      = unit_from
                   IMPORTING factor    = lv_factor_from
                             additive  = lv_add_from
                             dimension = lv_dim_from ).
      unit_get_si( EXPORTING unit      = unit_to
                   IMPORTING factor    = lv_factor_to
                             additive  = lv_add_to
                             dimension = lv_dim_to ).

      " KG into M is no conversion - UNIT_CONVERSION_SIMPLE refuses it
      IF lv_dim_from <> lv_dim_to.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |UNIT_CONVERSION_FAILED: { unit_from } ({ lv_dim_from }) and { unit_to } ({ lv_dim_to }) are of different dimensions|.
      ENDIF.

      " through the SI unit, with the additive value of temperatures
      result = ( val * lv_factor_from + lv_add_from - lv_add_to ) / lv_factor_to.

    ENDIF.

  ENDMETHOD.

  METHOD currency_get_decimals.

    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.
    DATA lv_key TYPE string.
          DATA lv_tab TYPE string.

    result = 2.
    
    lv_key = to_upper( c_trim( currency ) ).
    IF lv_key IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        IF check_abap_cloud( ) = abap_true.
          
          lv_tab = `I_CURRENCY`.
        ELSE.
          lv_tab = `TCURX`.
        ENDIF.
        CREATE DATA lr_row TYPE (lv_tab).
        ASSIGN lr_row->* TO <row>.
        IF check_abap_cloud( ) = abap_true.
          SELECT SINGLE *
            FROM (lv_tab) INTO CORRESPONDING FIELDS OF <row>
            WHERE currency = lv_key
            .
        ELSE.
          SELECT SINGLE *
            FROM (lv_tab) INTO CORRESPONDING FIELDS OF <row>
            WHERE currkey = lv_key
            .
        ENDIF.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        ASSIGN COMPONENT `DECIMALS` OF STRUCTURE <row> TO <comp>.
        IF <comp> IS NOT ASSIGNED.
          ASSIGN COMPONENT `CURRDEC` OF STRUCTURE <row> TO <comp>.
        ENDIF.
        IF <comp> IS ASSIGNED.
          result = <comp>.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
        " no table to read - the rule of the system: two
    ENDTRY.

  ENDMETHOD.

  METHOD unit_get_decimals.

    DATA lr_row TYPE REF TO data.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.
    DATA lv_key TYPE string.
          DATA lv_tab TYPE string.

    result = -1.
    
    lv_key = to_upper( c_trim( unit ) ).
    IF lv_key IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        IF check_abap_cloud( ) = abap_true.
          
          lv_tab = `I_UNITOFMEASURE`.
        ELSE.
          lv_tab = `T006`.
        ENDIF.
        CREATE DATA lr_row TYPE (lv_tab).
        ASSIGN lr_row->* TO <row>.
        IF check_abap_cloud( ) = abap_true.
          SELECT SINGLE *
            FROM (lv_tab) INTO CORRESPONDING FIELDS OF <row>
            WHERE unitofmeasure = lv_key
            .
        ELSE.
          SELECT SINGLE *
            FROM (lv_tab) INTO CORRESPONDING FIELDS OF <row>
            WHERE msehi = lv_key
            .
        ENDIF.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        " the decimals for display - the view names them one way, T006 another
        ASSIGN COMPONENT `UNITOFMEASUREDSPNMBROFDCMLS` OF STRUCTURE <row> TO <comp>.
        IF <comp> IS NOT ASSIGNED.
          ASSIGN COMPONENT `DECAN` OF STRUCTURE <row> TO <comp>.
        ENDIF.
        IF <comp> IS ASSIGNED.
          result = <comp>.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
        " no table to read - unknown
    ENDTRY.

  ENDMETHOD.

  METHOD unit_get_si.

    DATA lr_row TYPE REF TO data.
    DATA lv_num TYPE decfloat34.
    DATA lv_den TYPE decfloat34.
    DATA lv_exp TYPE i.
    FIELD-SYMBOLS <row>  TYPE any.
    FIELD-SYMBOLS <comp> TYPE any.
    DATA lv_unit TYPE string.
        DATA lv_tab TYPE string.
        DATA lo_struct TYPE REF TO cl_abap_structdescr.
        DATA lv_subrc LIKE sy-subrc.
        DATA x TYPE REF TO cx_root.
    DATA temp500 TYPE string_table.
    DATA lt_names LIKE temp500.
    DATA lv_found TYPE string.
    DATA lv_entry LIKE LINE OF lt_names.
      DATA lv_what TYPE string.
      DATA lv_column TYPE string.

    CLEAR factor.
    CLEAR additive.
    CLEAR dimension.

    
    lv_unit = to_upper( c_trim( unit ) ).

    TRY.
        
        lv_tab = `I_UNITOFMEASURE`.
        
        lo_struct = cl_abap_structdescr=>create( rtti_get_t_attri_by_table_name( lv_tab ) ).
        CREATE DATA lr_row TYPE HANDLE lo_struct.
        ASSIGN lr_row->* TO <row>.
        SELECT SINGLE *
          FROM (lv_tab) INTO CORRESPONDING FIELDS OF <row>
          WHERE unitofmeasure = lv_unit
          .
        
        lv_subrc = sy-subrc.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.
    IF lv_subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |UNIT_CONVERSION_FAILED: unit { lv_unit } not found|.
    ENDIF.

    " numerator, denominator, exponent, additive value, dimension - each
    " under the first of its names the view has
    
    CLEAR temp500.
    INSERT `NUM:SIUNITCNVRSNRATENUMERATOR` INTO TABLE temp500.
    INSERT `NUM:UNITOFMEASURESINUMERATOR` INTO TABLE temp500.
    INSERT `DEN:SIUNITCNVRSNRATEDENOMINATOR` INTO TABLE temp500.
    INSERT `DEN:UNITOFMEASURESIDENOMINATOR` INTO TABLE temp500.
    INSERT `EXP:SIUNITCNVRSNRATEEXPONENT` INTO TABLE temp500.
    INSERT `EXP:UNITOFMEASURESIEXPONENT` INTO TABLE temp500.
    INSERT `ADD:SIUNITCNVRSNADDITIVEVALUE` INTO TABLE temp500.
    INSERT `ADD:UNITOFMEASURESIADDITIVEVALUE` INTO TABLE temp500.
    INSERT `DIM:UNITOFMEASUREDIMENSION` INTO TABLE temp500.
    
    lt_names = temp500.
    
    lv_found = ``.
    
    LOOP AT lt_names INTO lv_entry.
      
      
      SPLIT lv_entry AT `:` INTO lv_what lv_column.
      IF lv_found CS lv_what.
        CONTINUE.
      ENDIF.
      UNASSIGN <comp>.
      ASSIGN COMPONENT lv_column OF STRUCTURE <row> TO <comp>.
      IF <comp> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      lv_found = lv_found && lv_what.
      CASE lv_what.
        WHEN `NUM`.
          lv_num = <comp>.
        WHEN `DEN`.
          lv_den = <comp>.
        WHEN `EXP`.
          lv_exp = <comp>.
        WHEN `ADD`.
          additive = <comp>.
        WHEN `DIM`.
          dimension = <comp>.
      ENDCASE.
    ENDLOOP.

    IF lv_num = 0 OR lv_den = 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |UNIT_CONVERSION_FAILED: no SI conversion rate for unit { lv_unit }|.
    ENDIF.

    factor = lv_num / lv_den.
    DO abs( lv_exp ) TIMES.
      IF lv_exp > 0.
        factor = factor * 10.
      ELSE.
        factor = factor / 10.
      ENDIF.
    ENDDO.

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
    DATA temp502 TYPE string.
    DATA lv_key TYPE xstring.

    lv_alg = to_upper( c_trim( algorithm ) ).
    lv_data = val.
    
    temp502 = key.
    
    lv_key = conv_get_xstring_by_string( temp502 ).

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

    DATA temp503 TYPE string.
    temp503 = url.
    result = http_execute( method       = `GET`
                           url          = temp503
                           body         = ``
                           content_type = ``
                           t_header     = t_header ).

  ENDMETHOD.

  METHOD http_post.

    DATA temp504 TYPE string.
    DATA temp693 TYPE string.
    DATA temp136 TYPE string.
    temp504 = url.
    
    temp693 = body.
    
    temp136 = content_type.
    result = http_execute( method       = `POST`
                           url          = temp504
                           body         = temp693
                           content_type = temp136
                           t_header     = t_header ).

  ENDMETHOD.

  METHOD http_execute.

    " the one HTTP client of this catalog, zabaputil_cl_util_http - the two
    " branches that sat here were a second client: no CLOSE when the call
    " failed, no UTF-8 decoding of a body without a charset, and the cloud
    " request read into the wrong parameter
    DATA temp505 TYPE zabaputil_cl_util_http=>ty_s_client_req.
    DATA ls_req LIKE temp505.
    DATA temp506 LIKE sy-subrc.
    DATA temp694 LIKE sy-subrc.
      DATA temp507 TYPE zabaputil_cl_util_context=>ty_s_name_value.
    DATA ls_res TYPE zabaputil_cl_util_http=>ty_s_http_res.
    CLEAR temp505.
    temp505-method = method.
    temp505-url = url.
    temp505-body = body.
    temp505-t_header = t_header.
    
    ls_req = temp505.
    
    READ TABLE ls_req-t_header WITH KEY n = `content-type` TRANSPORTING NO FIELDS.
    temp506 = sy-subrc.
    
    READ TABLE ls_req-t_header WITH KEY n = `Content-Type` TRANSPORTING NO FIELDS.
    temp694 = sy-subrc.
    IF content_type IS NOT INITIAL
        AND NOT temp506 = 0
        AND NOT temp694 = 0.
      
      CLEAR temp507.
      temp507-n = `content-type`.
      temp507-v = content_type.
      INSERT temp507 INTO TABLE ls_req-t_header.
    ENDIF.

    
    ls_res = zabaputil_cl_util_http=>client_request( ls_req ).
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

    DATA lv_name TYPE string.
    lv_name = to_upper( c_trim( val ) ).

    TRY.
        CREATE DATA lr_data TYPE (lv_name).
        result = abap_true.
      CATCH cx_root.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD db_select_by_name.

    DATA lv_tab TYPE string.
    DATA temp508 TYPE string.
    DATA lv_where LIKE temp508.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        DATA lx_error TYPE REF TO cx_root.
    lv_tab = to_upper( c_trim( tabname ) ).
    
    temp508 = where.
    
    lv_where = temp508.

    result = rtti_create_tab_by_name( lv_tab ).
    
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
          SELECT * FROM (lv_tab) INTO TABLE <tab> UP TO max_rows ROWS WHERE (lv_where).
        ELSE.
          SELECT * FROM (lv_tab) INTO TABLE <tab> WHERE (lv_where) .
        ENDIF.
        
      CATCH cx_root INTO lx_error.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD db_count_by_name.

    DATA lv_tab TYPE string.
    DATA temp509 TYPE string.
    DATA lv_where LIKE temp509.
        DATA lx_error TYPE REF TO cx_root.
    lv_tab = to_upper( c_trim( tabname ) ).
    
    temp509 = where.
    
    lv_where = temp509.

    TRY.
        SELECT COUNT(*) FROM (lv_tab) INTO result WHERE (lv_where) .
        
      CATCH cx_root INTO lx_error.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD param_get_user_default.

    DATA lv_tab TYPE string.

    DATA temp510 TYPE string.
    DATA temp695 TYPE string.
    DATA lv_uname LIKE temp695.
    DATA lv_parid TYPE string.
    temp510 = sy-uname.
    
    IF uname IS NOT INITIAL.
      temp695 = to_upper( c_trim( uname ) ).
    ELSE.
      temp695 = temp510.
    ENDIF.
    
    lv_uname = temp695.
    
    lv_parid = to_upper( c_trim( val ) ).

    TRY.
        lv_tab = `USR05`.
        SELECT SINGLE parva FROM (lv_tab) INTO result WHERE bname = lv_uname AND parid = lv_parid .
        IF sy-subrc <> 0.
          CLEAR result.
        ENDIF.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD job_get_status.

    DATA lv_tab TYPE string.

    DATA lv_jobname TYPE string.
    DATA lv_jobcount TYPE string.
    lv_jobname = to_upper( c_trim( jobname ) ).
    
    lv_jobcount = c_trim( jobcount ).

    TRY.
        lv_tab = `TBTCO`.
        SELECT SINGLE status FROM (lv_tab) INTO result WHERE jobname = lv_jobname AND jobcount = lv_jobcount .
        IF sy-subrc <> 0.
          CLEAR result.
        ENDIF.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.


  METHOD check_msg_container.

    DATA lv_kind TYPE string.
    DATA temp39 TYPE xsdboolean.
    lv_kind = rtti_get_type_kind( val ).
    
    temp39 = boolc( lv_kind = cl_abap_datadescr=>typekind_table OR lv_kind = cl_abap_datadescr=>typekind_struct1 OR lv_kind = cl_abap_datadescr=>typekind_struct2 OR lv_kind = cl_abap_datadescr=>typekind_oref ).
    result = temp39.

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
    DATA lt_attri_o TYPE abap_attrdescr_tab.
    DATA temp511 LIKE LINE OF lt_attri_o.
    DATA ls_attri_o LIKE REF TO temp511.

    result = is_msg.
    
    lt_attri_o = rtti_get_t_attri_by_oref( io_obj ).
    
    
    LOOP AT lt_attri_o REFERENCE INTO ls_attri_o "#EC CI_SORTSEQ
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
    DATA lv_rap LIKE abap_false.
    DATA lv_first LIKE abap_true.
    FIELD-SYMBOLS <row> TYPE ANY.
        DATA lv_row_kind TYPE string.
    lv_rap = abap_false.
    
    lv_first = abap_true.
    
    LOOP AT it_tab ASSIGNING <row>.
      IF lv_first = abap_true.
        lv_first = abap_false.
        
        lv_row_kind = rtti_get_type_kind( <row> ).
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
    FIELD-SYMBOLS <ls_hit> TYPE zabaputil_cl_util_context=>ty_s_class_impl_intf.
          DATA temp512 LIKE sy-subrc.
          DATA temp40 TYPE xsdboolean.
    DATA temp513 TYPE zabaputil_cl_util_context=>ty_s_class_impl_intf.

    " cached per pair - see gt_class_impl_intf at the declaration
    lv_class = to_upper( class ).
    lv_intf  = to_upper( intf ).

    
    READ TABLE gt_class_impl_intf WITH KEY name = lv_class intf = lv_intf ASSIGNING <ls_hit>.
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
          
          READ TABLE lo_classdescr->interfaces WITH KEY name = lv_intf TRANSPORTING NO FIELDS.
          temp512 = sy-subrc.
          
          temp40 = boolc( temp512 = 0 ).
          result = temp40. "#EC CI_SORTSEQ
        ENDIF.

      CATCH cx_sy_move_cast_error ##NO_HANDLER.
        " an interface or a data type - no class, a plain abap_false
      CATCH cx_root.
        " the descriptor failed - not cached, so a class fixed meanwhile is
        " checked anew; rtti_get_class_load_error( ) says why
        RETURN.
    ENDTRY.

    
    CLEAR temp513.
    temp513-name = lv_class.
    temp513-intf = lv_intf.
    temp513-result = result.
    INSERT temp513 INTO TABLE gt_class_impl_intf.

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
        DATA lx TYPE REF TO cx_root.

    TRY.
        " the relative name, not the absolute one: `\INTERFACE=ZIF_ANY` on a
        " system, `\CLASS=ZIF_ANY` in the transpiled runtime - the relative
        " name is `ZIF_ANY` on both
        lo_refdescr ?= cl_abap_typedescr=>describe_by_data( val ).
        result = lo_refdescr->get_referenced_type( )->get_relative_name( ).
        
      CATCH cx_root INTO lx.
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
    DATA temp514 TYPE string.
    DATA lv_classname LIKE temp514.
    IF rtti_check_class_exists( `ZCL_SRTTI_TYPEDESCR` ) = abap_true.
      temp514 = `ZCL_SRTTI_TYPEDESCR`.
    ELSE.
      temp514 = rtti_get_ref_type_name( lo_shipped ).
    ENDIF.
    
    lv_classname = temp514.
    CALL METHOD (lv_classname)=>(`CREATE_BY_DATA_OBJECT`)
      EXPORTING
        data_object = data
      RECEIVING
        srtti       = result.

  ENDMETHOD.

  METHOD xml_srtti_parse_pair.

    DATA srtti TYPE REF TO object.
    DATA rtti_type TYPE REF TO cl_abap_typedescr.
    DATA lo_datadescr TYPE REF TO cl_abap_datadescr.
    FIELD-SYMBOLS <variable> TYPE data.
    CALL TRANSFORMATION id SOURCE XML iv_type RESULT srtti = srtti.

    
    CALL METHOD srtti->(`GET_RTTI`)
      RECEIVING
        rtti = rtti_type.

    
    lo_datadescr ?= rtti_type.

    CREATE DATA result TYPE HANDLE lo_datadescr.
    
    ASSIGN result->* TO <variable>.
    CALL TRANSFORMATION id SOURCE XML iv_data RESULT dobj = <variable>.

  ENDMETHOD.

  METHOD xml_srtti_stringify_pair.
        DATA lo_srtti TYPE REF TO object.
        DATA lx_srtti TYPE REF TO cx_root.

    TRY.
        
        lo_srtti = xml_srtti_descr( data ).
        CALL TRANSFORMATION id SOURCE srtti = lo_srtti RESULT XML ev_type.
        CALL TRANSFORMATION id SOURCE dobj = data RESULT XML ev_data.
        
      CATCH cx_root INTO lx_srtti.
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

    DATA lv_currency TYPE abap_bool.
    DATA temp41 TYPE xsdboolean.
    DATA lv_unit TYPE abap_bool.
    DATA temp42 TYPE xsdboolean.
    DATA lv_type TYPE string.
    DATA lv_options TYPE string.
    temp41 = boolc( currency_path IS NOT INITIAL ).
    lv_currency = temp41.
    
    
    temp42 = boolc( lv_currency = abap_false AND unit_path IS NOT INITIAL ).
    lv_unit = temp42.

    
    lv_type = ui5_get_model_type( type_kind      = type_kind
                                        check_currency = lv_currency
                                        check_unit     = lv_unit ).
    IF lv_type IS INITIAL.
      result = |\{{ path }\}|.
      RETURN.
    ENDIF.

    
    lv_options = ui5_get_format_options( type_kind      = type_kind
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

    DATA lo_descr TYPE REF TO cl_abap_typedescr.
    DATA temp515 TYPE REF TO cl_abap_elemdescr.
    DATA lo_elem LIKE temp515.
    lo_descr = cl_abap_typedescr=>describe_by_data( val ).
    " a TIMESTAMP / TIMESTAMPL is a packed number here and an ISO text in
    " the model - a FloatType would refuse to parse it
    IF lo_descr->kind <> cl_abap_typedescr=>kind_elem
        OR lo_descr->absolute_name = `\TYPE=TIMESTAMP`
        OR lo_descr->absolute_name = `\TYPE=TIMESTAMPL`.
      result = |\{{ path }\}|.
      RETURN.
    ENDIF.

    
    temp515 ?= lo_descr.
    
    lo_elem = temp515.
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

    DATA lv_comp TYPE string.
    DATA ls_ref TYPE zabaputil_cl_util_context=>ty_s_ref_field.
    lv_comp = to_upper( condense( comp ) ).
    ASSIGN COMPONENT lv_comp OF STRUCTURE struc TO <val>.
    IF sy-subrc <> 0.
      result = |\{{ lv_comp }\}|.
      RETURN.
    ENDIF.

    
    ls_ref = rtti_get_ref_field( struc = struc
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

    DATA lv_comp TYPE string.
        DATA lo_descr TYPE REF TO cl_abap_typedescr.
        DATA temp516 TYPE REF TO cl_abap_structdescr.
        DATA lo_struct LIKE temp516.
          DATA temp517 TYPE string.
          DATA temp518 TYPE string.
          DATA temp43 TYPE xsdboolean.
    lv_comp = to_upper( condense( comp ) ).

    TRY.
        
        lo_descr = cl_abap_typedescr=>describe_by_data( struc ).
        IF lo_descr->kind <> cl_abap_typedescr=>kind_struct OR lo_descr->is_ddic_type( ) = abap_false.
          RETURN.
        ENDIF.
        
        temp516 ?= lo_descr.
        
        lo_struct = temp516.

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
          
          temp517 = <name>.
          IF to_upper( condense( temp517 ) ) <> lv_comp.
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
          
          temp518 = <reffield>.
          lv_ref = to_upper( condense( temp518 ) ).
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
          
          temp43 = boolc( <datatype> = `CURR` ).
          result-currency = temp43.
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

    DATA temp519 TYPE xstring.
    DATA temp520 TYPE string.
    DATA lv_val LIKE temp520.
    DATA temp521 TYPE string.
    DATA lv_sep LIKE temp521.
    DATA temp522 TYPE d.
    DATA lv_ref_date LIKE temp522.
      TYPES temp31 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_part TYPE temp31.
      DATA lv_part LIKE LINE OF lt_part.
        DATA lv_close TYPE i.
        DATA ls_rule TYPE zabaputil_cl_util_context=>ty_s_gs1_rule.
    DATA lv_len TYPE i.
    DATA lv_pos TYPE i.
      DATA lv_rest TYPE string.
      DATA lv_data TYPE string.
        DATA lv_end TYPE i.
    temp519 = `1D`.
    lv_gs = conv_get_string_by_xstring( temp519 ).
    
    temp520 = val.
    
    lv_val = temp520.
    
    temp521 = separator.
    
    lv_sep = temp521.
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

    
    IF ref_date IS INITIAL.
      temp522 = sy-datum.
    ELSE.
      temp522 = ref_date.
    ENDIF.
    
    lv_ref_date = temp522.

    IF strlen( lv_val ) > 0 AND lv_val(1) = `(`.

      " human-readable form (AI)value(AI)value
      

      SPLIT lv_val AT `(` INTO TABLE lt_part.
      
      LOOP AT lt_part INTO lv_part.
        IF lv_part IS INITIAL.
          CONTINUE.
        ENDIF.
        
        lv_close = find( val = lv_part
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
        
        ls_rule = barcode_gs1_find_rule( lv_ai ).
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

    
    lv_len = strlen( lv_val ).
    
    lv_pos = 0.
    WHILE lv_pos < lv_len.

      " a separator at the start (FNC1 sent as GS) or after a fixed-length
      " field is allowed and carries no data
      IF substring( val = lv_val
                    off = lv_pos
                    len = 1 ) = lv_gs.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      
      lv_rest = substring( val = lv_val
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
      
      lv_data = substring( val = lv_rest
                                 off = ls_rule-ai_len ).

      IF ls_rule-var = abap_false.
        IF strlen( lv_data ) < ls_rule-len.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |GS1: AI ({ lv_ai }) needs { ls_rule-len } characters, only { strlen( lv_data ) } left|.
        ENDIF.
        lv_value = lv_data(ls_rule-len).
      ELSE.
        
        lv_end = find( val = lv_data
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

    DATA lv_len TYPE i.
    lv_len = strlen( value ).
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

    DATA temp523 TYPE i.
    DATA lv_yy LIKE temp523.
    DATA temp524 TYPE i.
    DATA lv_mm LIKE temp524.
    DATA temp525 TYPE i.
    DATA lv_dd LIKE temp525.
    DATA temp526 TYPE i.
    DATA lv_ref_year LIKE temp526.
    DATA lv_century TYPE i.
    DATA lv_diff TYPE i.
    DATA lv_year TYPE i.
    DATA temp527 TYPE d.
    DATA lv_last LIKE temp527.
    DATA temp528 TYPE i.
    DATA lv_last_day LIKE temp528.
    temp523 = val(2).
    
    lv_yy = temp523.
    
    temp524 = val+2(2).
    
    lv_mm = temp524.
    
    temp525 = val+4(2).
    
    lv_dd = temp525.

    " GS1 General Specifications 7.12: a year more than 50 years ahead is in
    " the past century, one more than 49 years back in the next century
    
    temp526 = ref_date(4).
    
    lv_ref_year = temp526.
    
    lv_century = lv_ref_year - lv_ref_year MOD 100.
    
    lv_diff = lv_yy - lv_ref_year MOD 100.
    IF lv_diff >= 51.
      lv_century = lv_century - 100.
    ELSEIF lv_diff <= -50.
      lv_century = lv_century + 100.
    ENDIF.
    
    lv_year = lv_century + lv_yy.

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
    
    temp527 = lv_next - 1.
    
    lv_last = temp527.
    
    temp528 = lv_last+6(2).
    
    lv_last_day = temp528.

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

    DATA temp529 TYPE string.
    DATA lv_sep LIKE temp529.
      DATA temp530 TYPE xstring.
    DATA ls_gs1 LIKE LINE OF t_gs1.
      DATA ls_rule TYPE zabaputil_cl_util_context=>ty_s_gs1_rule.
    temp529 = separator.
    
    lv_sep = temp529.
    IF lv_sep IS INITIAL.
      
      temp530 = `1D`.
      lv_sep = conv_get_string_by_xstring( temp530 ).
    ENDIF.

    
    LOOP AT t_gs1 INTO ls_gs1.
      IF human_readable = abap_true.
        result = |{ result }({ ls_gs1-ai }){ ls_gs1-value }|.
        CONTINUE.
      ENDIF.
      
      ls_rule = barcode_gs1_find_rule( ls_gs1-ai ).
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

    DATA temp531 TYPE string.
    DATA lv_ai LIKE temp531.
    DATA ls_gs1 LIKE LINE OF t_gs1.
    DATA lv_len TYPE i.
    temp531 = ai.
    
    lv_ai = temp531.

    
    LOOP AT t_gs1 INTO ls_gs1 WHERE ai = lv_ai.
      result = ls_gs1-value.
      RETURN.
    ENDLOOP.

    
    lv_len = strlen( lv_ai ).
    LOOP AT t_gs1 INTO ls_gs1.
      IF strlen( ls_gs1-ai ) > lv_len AND substring( val = ls_gs1-ai
                                                     len = lv_len ) = lv_ai.
        result = ls_gs1-value.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD barcode_split_get_scan.

    DATA temp532 TYPE string.
    temp532 = val.
    result = replace( val  = temp532
                      sub  = cv_char_util_cr_lf
                      with = ``
                      occ  = 0 ).
    result = replace( val  = result
                      sub  = cv_char_util_newline
                      with = ``
                      occ  = 0 ).

  ENDMETHOD.

  METHOD barcode_split_add_value.
    DATA temp533 TYPE zabaputil_cl_util_context=>ty_s_barcode_value.

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

    
    CLEAR temp533.
    temp533-name = field-name.
    temp533-value = value.
    INSERT temp533 INTO TABLE result.

  ENDMETHOD.

  METHOD barcode_split_separator.

    DATA lv_value TYPE string.

    DATA temp534 TYPE string.
    DATA lv_sep LIKE temp534.
    DATA lt_part TYPE string_table.
    DATA ls_field LIKE LINE OF t_field.
      DATA lv_tabix LIKE sy-tabix.
      DATA lv_found LIKE abap_false.
        FIELD-SYMBOLS <temp535> LIKE LINE OF lt_part.
        DATA temp536 LIKE sy-tabix.
    temp534 = separator.
    
    lv_sep = temp534.
    IF lv_sep IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `Barcode: no separator given`.
    ENDIF.

    
    lt_part = c_split( val = barcode_split_get_scan( val )
                             sep = lv_sep ).
    IF lines( lt_part ) > lines( t_field ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |Barcode: { lines( lt_part ) } parts, but only { lines( t_field ) } fields|.
    ENDIF.

    
    LOOP AT t_field INTO ls_field.
      
      lv_tabix = sy-tabix.
      CLEAR lv_value.
      
      lv_found = abap_false.
      IF lv_tabix <= lines( lt_part ).
        
        
        temp536 = sy-tabix.
        READ TABLE lt_part INDEX lv_tabix ASSIGNING <temp535>.
        sy-tabix = temp536.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_value = <temp535>.
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

    DATA lv_val TYPE string.
    DATA lv_len TYPE i.
    DATA ls_field LIKE LINE OF t_field.
      DATA lv_found LIKE abap_false.
      DATA lv_off TYPE zabaputil_cl_util_context=>ty_s_barcode_field-pos.
        DATA lv_take TYPE i.
      DATA ls_check LIKE ls_field.
    lv_val = barcode_split_get_scan( val ).
    
    lv_len = strlen( lv_val ).

    
    LOOP AT t_field INTO ls_field.
      IF ls_field-pos < 1 OR ls_field-len < 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: field { ls_field-name } needs a position from 1 on|.
      ENDIF.

      CLEAR lv_value.
      
      lv_found = abap_false.
      
      lv_off = ls_field-pos - 1.
      IF lv_off < lv_len.
        lv_found = abap_true.
        
        lv_take = lv_len - lv_off.
        IF ls_field-len > 0 AND ls_field-len < lv_take.
          lv_take = ls_field-len.
        ENDIF.
        " the padding of a fixed layout is no part of the value
        lv_value = c_trim( substring( val = lv_val
                                      off = lv_off
                                      len = lv_take ) ).
      ENDIF.

      " len is where the field ends here, not a limit to check a value against
      
      ls_check = ls_field.
      ls_check-len = 0.
      barcode_split_add_value( EXPORTING field  = ls_check
                                         value  = lv_value
                                         found  = lv_found
                               CHANGING  result = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD barcode_split_prefix.

    DATA lt_found TYPE ty_t_barcode_value.

    DATA temp537 TYPE string.
    DATA lv_sep LIKE temp537.
      DATA temp538 TYPE xstring.
    DATA temp539 TYPE string_table.
    DATA lv_part LIKE LINE OF temp539.
      DATA temp540 TYPE ty_s_barcode_field.
      DATA ls_hit LIKE temp540.
      DATA lv_hit LIKE abap_false.
      DATA ls_field LIKE LINE OF t_field.
        DATA lv_plen TYPE i.
      DATA temp541 LIKE sy-subrc.
      DATA temp542 TYPE zabaputil_cl_util_context=>ty_s_barcode_value.
      DATA lv_value TYPE string.
      DATA lv_found LIKE abap_false.
      DATA temp543 LIKE sy-subrc.
        FIELD-SYMBOLS <temp544> LIKE LINE OF lt_found.
        DATA temp545 LIKE sy-tabix.
    temp537 = separator.
    
    lv_sep = temp537.
    IF lv_sep IS INITIAL.
      
      temp538 = `1D`.
      lv_sep = conv_get_string_by_xstring( temp538 ).
    ENDIF.

    
    temp539 = c_split( val = barcode_split_get_scan( val ) sep = lv_sep ).
    
    LOOP AT temp539 INTO lv_part.
      IF lv_part IS INITIAL.
        CONTINUE.
      ENDIF.

      " the longest prefix that fits: `1T` must not be read as `1`
      
      CLEAR temp540.
      
      ls_hit = temp540.
      
      lv_hit = abap_false.
      
      LOOP AT t_field INTO ls_field WHERE prefix IS NOT INITIAL.
        
        lv_plen = strlen( ls_field-prefix ).
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

      
      READ TABLE lt_found WITH KEY name = ls_hit-name TRANSPORTING NO FIELDS.
      temp541 = sy-subrc.
      IF temp541 = 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: field { ls_hit-name } comes twice|.
      ENDIF.
      
      CLEAR temp542.
      temp542-name = ls_hit-name.
      temp542-value = substring( val = lv_part off = strlen( ls_hit-prefix ) ).
      INSERT temp542 INTO TABLE lt_found.
    ENDLOOP.

    LOOP AT t_field INTO ls_field WHERE prefix IS NOT INITIAL.
      
      lv_value = ``.
      
      lv_found = abap_false.
      
      READ TABLE lt_found WITH KEY name = ls_field-name TRANSPORTING NO FIELDS.
      temp543 = sy-subrc.
      IF temp543 = 0.
        
        
        temp545 = sy-tabix.
        READ TABLE lt_found WITH KEY name = ls_field-name ASSIGNING <temp544>.
        sy-tabix = temp545.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        lv_value = <temp544>-value.
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

    DATA temp546 TYPE string.
    DATA lv_val TYPE string.
    DATA lv_weight TYPE i.
    DATA lv_pos TYPE i.
      DATA temp547 TYPE i.
    temp546 = val.
    
    lv_val = c_trim( temp546 ).
    IF lv_val IS INITIAL OR lv_val CN `0123456789`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |GS1: check digit needs digits only: { lv_val }|.
    ENDIF.

    " weights 3, 1, 3, 1, ... starting with the rightmost digit
    
    lv_weight = 3.
    
    lv_pos = strlen( lv_val ) - 1.
    WHILE lv_pos >= 0.
      
      temp547 = substring( val = lv_val off = lv_pos len = 1 ).
      lv_sum = lv_sum + temp547 * lv_weight.
      lv_weight = 4 - lv_weight.
      lv_pos = lv_pos - 1.
    ENDWHILE.

    result = |{ ( 10 - lv_sum MOD 10 ) MOD 10 }|.

  ENDMETHOD.

  METHOD check_is_gs1_check_digit.

    DATA temp548 TYPE string.
    DATA lv_val TYPE string.
    DATA lv_len TYPE i.
    DATA lv_body TYPE string.
    DATA lv_digit TYPE string.
    DATA temp44 TYPE xsdboolean.
    temp548 = val.
    
    lv_val = c_trim( temp548 ).
    
    lv_len = strlen( lv_val ).
    IF lv_len < 2 OR lv_val CN `0123456789`.
      result = abap_false.
      RETURN.
    ENDIF.

    
    lv_body = substring( val = lv_val
                               len = lv_len - 1 ).
    
    lv_digit = substring( val = lv_val
                                off = lv_len - 1 ).
    
    temp44 = boolc( barcode_gs1_calc_check_digit( lv_body ) = lv_digit ).
    result = temp44.

  ENDMETHOD.

  METHOD barcode_gs1_find_rule.
    DATA lv_len TYPE i.
        DATA lv_prefix TYPE string.

    IF gt_gs1_rule IS INITIAL.
      gt_gs1_rule = barcode_gs1_get_rules( ).
    ENDIF.

    " the longest prefix first: 4-digit AIs (7003), then the 3-digit ones and
    " the 3-digit prefixes of the measure AIs (310n), then the 2-digit AIs
    
    lv_len = 4.
    WHILE lv_len >= 2.
      IF strlen( val ) >= lv_len.
        
        lv_prefix = substring( val = val
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
    DATA temp549 TYPE zabaputil_cl_util_context=>ty_t_gs1_rule.
    DATA temp550 LIKE LINE OF temp549.
    CLEAR temp549.
    
    CLEAR temp550.
    temp550-prefix = `00`.
    temp550-ai_len = 2.
    temp550-len = 18.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `SSCC`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `01`.
    temp550-ai_len = 2.
    temp550-len = 14.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `GTIN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `02`.
    temp550-ai_len = 2.
    temp550-len = 14.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `CONTENT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `03`.
    temp550-ai_len = 2.
    temp550-len = 14.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `MTO GTIN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `10`.
    temp550-ai_len = 2.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `BATCH/LOT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `11`.
    temp550-ai_len = 2.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-is_date = abap_true.
    temp550-title = `PROD DATE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `12`.
    temp550-ai_len = 2.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-is_date = abap_true.
    temp550-title = `DUE DATE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `13`.
    temp550-ai_len = 2.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-is_date = abap_true.
    temp550-title = `PACK DATE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `15`.
    temp550-ai_len = 2.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-is_date = abap_true.
    temp550-title = `BEST BEFORE or BEST BY`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `16`.
    temp550-ai_len = 2.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-is_date = abap_true.
    temp550-title = `SELL BY`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `17`.
    temp550-ai_len = 2.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-is_date = abap_true.
    temp550-title = `USE BY or EXPIRY`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `20`.
    temp550-ai_len = 2.
    temp550-len = 2.
    temp550-numeric = abap_true.
    temp550-title = `VARIANT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `21`.
    temp550-ai_len = 2.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `SERIAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `22`.
    temp550-ai_len = 2.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `CPV`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `235`.
    temp550-ai_len = 3.
    temp550-len = 28.
    temp550-var = abap_true.
    temp550-title = `TPX`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `240`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `ADDITIONAL ID`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `241`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `CUST. PART No.`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `242`.
    temp550-ai_len = 3.
    temp550-len = 6.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `MTO VARIANT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `243`.
    temp550-ai_len = 3.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `PCN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `250`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `SECONDARY SERIAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `251`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `REF. TO SOURCE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `253`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `GDTI`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `254`.
    temp550-ai_len = 3.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `GLN EXTENSION COMPONENT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `255`.
    temp550-ai_len = 3.
    temp550-len = 25.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `GCN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `30`.
    temp550-ai_len = 2.
    temp550-len = 8.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `VAR. COUNT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `310`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET WEIGHT (kg)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `311`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (m)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `312`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (m)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `313`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (m)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `314`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (m2)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `315`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET VOLUME (l)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `316`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET VOLUME (m3)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `320`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET WEIGHT (lb)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `321`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (in)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `322`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (ft)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `323`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (yd)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `324`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (in)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `325`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (ft)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `326`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (yd)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `327`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (in)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `328`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (ft)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `329`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (yd)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `330`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `GROSS WEIGHT (kg)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `331`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (m), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `332`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (m), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `333`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (m), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `334`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (m2), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `335`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (l), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `336`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (m3), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `337`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `KG PER m2`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `340`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `GROSS WEIGHT (lb)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `341`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (in), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `342`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (ft), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `343`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `LENGTH (yd), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `344`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (in), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `345`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (ft), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `346`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `WIDTH (yd), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `347`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (in), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `348`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (ft), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `349`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `HEIGHT (yd), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `350`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (in2)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `351`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (ft2)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `352`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (yd2)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `353`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (in2), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `354`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (ft2), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `355`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AREA (yd2), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `356`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET WEIGHT (t oz)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `357`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET VOLUME (oz)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `360`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET VOLUME (qt)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `361`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `NET VOLUME (g)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `362`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (qt), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `363`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (g), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `364`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (in3)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `365`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (ft3)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `366`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (yd3)`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `367`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (in3), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `368`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (ft3), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `369`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `VOLUME (yd3), log`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `37`.
    temp550-ai_len = 2.
    temp550-len = 8.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `COUNT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `390`.
    temp550-ai_len = 4.
    temp550-len = 15.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AMOUNT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `391`.
    temp550-ai_len = 4.
    temp550-len = 18.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `AMOUNT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `392`.
    temp550-ai_len = 4.
    temp550-len = 15.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `PRICE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `393`.
    temp550-ai_len = 4.
    temp550-len = 18.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `PRICE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `394`.
    temp550-ai_len = 4.
    temp550-len = 4.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `PRCNT OFF`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `395`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-decimals = abap_true.
    temp550-title = `PRICE/UoM`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `400`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `ORDER NUMBER`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `401`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `GINC`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `402`.
    temp550-ai_len = 3.
    temp550-len = 17.
    temp550-numeric = abap_true.
    temp550-title = `GSIN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `403`.
    temp550-ai_len = 3.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `ROUTE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `410`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `SHIP TO LOC`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `411`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `BILL TO`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `412`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `PURCHASE FROM`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `413`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `SHIP FOR LOC`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `414`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `LOC No.`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `415`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `PAY TO`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `416`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `PROD/SERV LOC`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `417`.
    temp550-ai_len = 3.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `PARTY`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `420`.
    temp550-ai_len = 3.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `SHIP TO POST`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `421`.
    temp550-ai_len = 3.
    temp550-len = 12.
    temp550-var = abap_true.
    temp550-title = `SHIP TO POST`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `422`.
    temp550-ai_len = 3.
    temp550-len = 3.
    temp550-numeric = abap_true.
    temp550-title = `ORIGIN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `423`.
    temp550-ai_len = 3.
    temp550-len = 15.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `COUNTRY - INITIAL PROCESS.`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `424`.
    temp550-ai_len = 3.
    temp550-len = 3.
    temp550-numeric = abap_true.
    temp550-title = `COUNTRY - PROCESS.`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `425`.
    temp550-ai_len = 3.
    temp550-len = 15.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `COUNTRY - DISASSEMBLY`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `426`.
    temp550-ai_len = 3.
    temp550-len = 3.
    temp550-numeric = abap_true.
    temp550-title = `COUNTRY - FULL PROCESS`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7001`.
    temp550-ai_len = 4.
    temp550-len = 13.
    temp550-numeric = abap_true.
    temp550-title = `NSN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7002`.
    temp550-ai_len = 4.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `MEAT CUT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7003`.
    temp550-ai_len = 4.
    temp550-len = 10.
    temp550-numeric = abap_true.
    temp550-title = `EXPIRY TIME`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7004`.
    temp550-ai_len = 4.
    temp550-len = 4.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `ACTIVE POTENCY`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7005`.
    temp550-ai_len = 4.
    temp550-len = 12.
    temp550-var = abap_true.
    temp550-title = `CATCH AREA`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7006`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-is_date = abap_true.
    temp550-title = `FIRST FREEZE DATE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7007`.
    temp550-ai_len = 4.
    temp550-len = 12.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `HARVEST DATE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7008`.
    temp550-ai_len = 4.
    temp550-len = 3.
    temp550-var = abap_true.
    temp550-title = `AQUATIC SPECIES`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7009`.
    temp550-ai_len = 4.
    temp550-len = 10.
    temp550-var = abap_true.
    temp550-title = `FISHING GEAR TYPE`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `7010`.
    temp550-ai_len = 4.
    temp550-len = 2.
    temp550-var = abap_true.
    temp550-title = `PROD METHOD`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8001`.
    temp550-ai_len = 4.
    temp550-len = 14.
    temp550-numeric = abap_true.
    temp550-title = `DIMENSIONS`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8002`.
    temp550-ai_len = 4.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `CMT No.`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8003`.
    temp550-ai_len = 4.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `GRAI`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8004`.
    temp550-ai_len = 4.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `GIAI`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8005`.
    temp550-ai_len = 4.
    temp550-len = 6.
    temp550-numeric = abap_true.
    temp550-title = `PRICE PER UNIT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8006`.
    temp550-ai_len = 4.
    temp550-len = 18.
    temp550-numeric = abap_true.
    temp550-title = `ITIP`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8007`.
    temp550-ai_len = 4.
    temp550-len = 34.
    temp550-var = abap_true.
    temp550-title = `IBAN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8008`.
    temp550-ai_len = 4.
    temp550-len = 12.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `PROD TIME`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8010`.
    temp550-ai_len = 4.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `CPID`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8011`.
    temp550-ai_len = 4.
    temp550-len = 12.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `CPID SERIAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8012`.
    temp550-ai_len = 4.
    temp550-len = 20.
    temp550-var = abap_true.
    temp550-title = `VERSION`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8013`.
    temp550-ai_len = 4.
    temp550-len = 25.
    temp550-var = abap_true.
    temp550-title = `GMN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8017`.
    temp550-ai_len = 4.
    temp550-len = 18.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `GSRN - PROVIDER`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8018`.
    temp550-ai_len = 4.
    temp550-len = 18.
    temp550-numeric = abap_true.
    temp550-check = abap_true.
    temp550-title = `GSRN - RECIPIENT`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8019`.
    temp550-ai_len = 4.
    temp550-len = 10.
    temp550-var = abap_true.
    temp550-numeric = abap_true.
    temp550-title = `SRIN`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8020`.
    temp550-ai_len = 4.
    temp550-len = 25.
    temp550-var = abap_true.
    temp550-title = `REF No.`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `8200`.
    temp550-ai_len = 4.
    temp550-len = 70.
    temp550-var = abap_true.
    temp550-title = `PRODUCT URL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `90`.
    temp550-ai_len = 2.
    temp550-len = 30.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `91`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `92`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `93`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `94`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `95`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `96`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `97`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `98`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    CLEAR temp550.
    temp550-prefix = `99`.
    temp550-ai_len = 2.
    temp550-len = 90.
    temp550-var = abap_true.
    temp550-title = `INTERNAL`.
    INSERT temp550 INTO TABLE temp549.
    result = temp549.

  ENDMETHOD.

  METHOD conv_get_xstring_by_bintab.

    DATA lv_hex TYPE string.
    FIELD-SYMBOLS <line> TYPE any.
    FIELD-SYMBOLS <x>    TYPE x.
      DATA temp551 TYPE string.

    " the bytes of every line in hex, then one conversion: an xstring has no
    " append operator, and CONCATENATE ... IN BYTE MODE is not used here
    LOOP AT val ASSIGNING <line>.
      ASSIGN <line> TO <x> CASTING.
      
      temp551 = <x>.
      lv_hex = lv_hex && temp551.
    ENDLOOP.

    IF length > 0 AND strlen( lv_hex ) > length * 2.
      lv_hex = substring( val = lv_hex
                          len = length * 2 ).
    ENDIF.
    result = lv_hex.

  ENDMETHOD.

  METHOD x_raise_on_cloud.

    IF check_abap_cloud( ) IS NOT INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |{ method }: not available on ABAP Cloud|.
    ENDIF.

  ENDMETHOD.

  METHOD sys_langu.

    " a two-letter ISO code (EN, DE) becomes the one-letter SAP key
    DATA temp552 TYPE string.
    DATA lv_val TYPE string.
    temp552 = val.
    
    lv_val = c_trim( temp552 ).
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
    DATA temp553 LIKE REF TO lv_funcname.
DATA temp696 TYPE abap_func_parmbind_tab.
DATA temp697 LIKE LINE OF temp696.
DATA lt_param LIKE temp696.
    DATA temp554 TYPE abap_func_excpbind_tab.
    DATA temp555 LIKE LINE OF temp554.
    DATA lt_exception LIKE temp554.
      DATA temp556 TYPE abap_func_excpbind.
      DATA temp557 TYPE ty_s_fm_param.
      DATA ls_param LIKE temp557.
      DATA temp558 TYPE string.
      DATA temp45 TYPE xsdboolean.
      DATA temp559 TYPE string.
      DATA lv_tabname TYPE string.
      DATA temp560 TYPE string.
      DATA lv_fieldname TYPE string.
        DATA temp561 TYPE string.

    lv_funcname = c_trim_upper( function ).
    CREATE DATA lr_params TYPE STANDARD TABLE OF (`RFC_FUNINT`).
    ASSIGN lr_params->* TO <params>.

    
    GET REFERENCE OF lv_funcname INTO temp553.

CLEAR temp696.

temp697-name = `FUNCNAME`.
temp697-kind = abap_func_exporting.
temp697-value = temp553.
INSERT temp697 INTO TABLE temp696.
temp697-name = `PARAMS`.
temp697-kind = abap_func_tables.
temp697-value = lr_params.
INSERT temp697 INTO TABLE temp696.

lt_param = temp696.
    
    CLEAR temp554.
    
    temp555-name = `FU_NOT_FOUND`.
    temp555-value = 1.
    INSERT temp555 INTO TABLE temp554.
    temp555-name = `OTHERS`.
    temp555-value = 2.
    INSERT temp555 INTO TABLE temp554.
    
    lt_exception = temp554.
    IF destination IS INITIAL.
      
      CLEAR temp556.
      temp556-name = `ERROR_MESSAGE`.
      temp556-value = 3.
      INSERT temp556 INTO TABLE lt_exception.
    ENDIF.
    IF sys_fm_call( function    = `RFC_GET_FUNCTION_INTERFACE`
                    destination = destination
                    t_param     = lt_param
                    t_exception = lt_exception ) <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |function module { lv_funcname } not found|.
    ENDIF.

    LOOP AT <params> ASSIGNING <param>.
      
      CLEAR temp557.
      
      ls_param = temp557.
      ASSIGN COMPONENT `PARAMCLASS` OF STRUCTURE <param> TO <field>.
      ls_param-paramclass = <field>.
      ASSIGN COMPONENT `PARAMETER` OF STRUCTURE <param> TO <field>.
      
      temp558 = <field>.
      ls_param-parameter = c_trim( temp558 ).
      ASSIGN COMPONENT `OPTIONAL` OF STRUCTURE <param> TO <field>.
      
      temp45 = boolc( <field> IS NOT INITIAL ).
      ls_param-optional = temp45.

      ASSIGN COMPONENT `TABNAME` OF STRUCTURE <param> TO <field>.
      
      temp559 = <field>.
      
      lv_tabname = c_trim( temp559 ).
      ASSIGN COMPONENT `FIELDNAME` OF STRUCTURE <param> TO <field>.
      
      temp560 = <field>.
      
      lv_fieldname = c_trim( temp560 ).
      IF lv_tabname IS NOT INITIAL AND lv_fieldname IS NOT INITIAL.
        ls_param-typename = |{ lv_tabname }-{ lv_fieldname }|.
      ELSEIF lv_tabname IS NOT INITIAL.
        ls_param-typename = lv_tabname.
      ELSE.
        " a built-in type: EXID is the ABAP type id
        ASSIGN COMPONENT `EXID` OF STRUCTURE <param> TO <field>.
        
        temp561 = <field>.
        CASE temp561.
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

    DATA lv_sequential LIKE sequential.
      DATA lv_method TYPE string.
      DATA lo_task LIKE LINE OF t_task.
        DATA temp562 TYPE ty_s_parallel.
        DATA ls_result LIKE temp562.
            DATA lx_task TYPE REF TO cx_root.
        DATA temp563 TYPE abap_parmbind_tab.
        DATA lt_param LIKE temp563.
          DATA temp564 LIKE REF TO tasks.
DATA temp698 TYPE abap_parmbind.
          DATA temp565 LIKE REF TO timeout.
DATA temp699 TYPE abap_parmbind.
        DATA lv_class TYPE string.
        DATA x TYPE REF TO cx_root.
    lv_sequential = sequential.
    IF lv_sequential = abap_false.
      TRY.
          cl_abap_typedescr=>describe_by_name( `CL_ABAP_PARALLEL` ).
        CATCH cx_root.
          lv_sequential = abap_true.
      ENDTRY.
    ENDIF.

    IF lv_sequential = abap_true.
      
      lv_method = c_trim_upper( method ).
      
      LOOP AT t_task INTO lo_task.
        
        CLEAR temp562.
        temp562-index = sy-tabix.
        temp562-task = lo_task.
        
        ls_result = temp562.
        TRY.
            CALL METHOD lo_task->(lv_method).
            
          CATCH cx_root INTO lx_task.
            ls_result-message = lx_task->get_text( ).
        ENDTRY.
        INSERT ls_result INTO TABLE result.
      ENDLOOP.
      RETURN.
    ENDIF.

    TRY.
        
        CLEAR temp563.
        
        lt_param = temp563.
        IF tasks > 0.
          
          GET REFERENCE OF tasks INTO temp564.

CLEAR temp698.
temp698-name = `P_NUM_TASKS`.
temp698-kind = cl_abap_objectdescr=>exporting.
temp698-value = temp564.
INSERT temp698 INTO TABLE lt_param.
        ENDIF.
        IF timeout > 0.
          
          GET REFERENCE OF timeout INTO temp565.

CLEAR temp699.
temp699-name = `P_TIMEOUT`.
temp699-kind = cl_abap_objectdescr=>exporting.
temp699-value = temp565.
INSERT temp699 INTO TABLE lt_param.
        ENDIF.
        
        lv_class = `CL_ABAP_PARALLEL`.
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
          CLEAR ls_result.
          ls_result-index = sy-tabix.
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

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD longtext_to_lines.

    DATA temp566 TYPE string.
    DATA lv_val TYPE string.
    TYPES temp32 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_paragraph TYPE temp32.
    DATA lv_paragraph LIKE LINE OF lt_paragraph.
        DATA temp567 TYPE zabaputil_cl_util_context=>ty_s_longtext_line.
      DATA lv_format TYPE string.
        DATA lv_len TYPE i.
          DATA lv_cut LIKE lv_len.
        DATA temp568 TYPE zabaputil_cl_util_context=>ty_s_longtext_line.
    temp566 = val.
    
    lv_val = replace( val  = temp566
                            sub  = cv_char_util_cr_lf
                            with = cv_char_util_newline
                            occ  = 0 ).
    

    SPLIT lv_val AT cv_char_util_newline INTO TABLE lt_paragraph.

    
    LOOP AT lt_paragraph INTO lv_paragraph.
      IF lv_paragraph IS INITIAL.
        
        CLEAR temp567.
        temp567-format = `*`.
        INSERT temp567 INTO TABLE result.
        CONTINUE.
      ENDIF.
      
      lv_format = `*`.
      WHILE lv_paragraph IS NOT INITIAL.
        
        lv_len = nmin( val1 = 132
                             val2 = strlen( lv_paragraph ) ).
        " TDLINE is CHAR132: a blank at the end of a chunk is gone when the
        " line is read back, and `=` joins the chunks without one - `word
        " next` came back as `wordnext`. The blanks move to the next chunk
        IF lv_len < strlen( lv_paragraph ).
          
          lv_cut = lv_len.
          WHILE lv_cut > 0 AND substring( val = lv_paragraph
                                          off = lv_cut - 1
                                          len = 1 ) = ` `.
            lv_cut = lv_cut - 1.
          ENDWHILE.
          IF lv_cut > 0.
            lv_len = lv_cut.
          ENDIF.
        ENDIF.
        
        CLEAR temp568.
        temp568-format = lv_format.
        temp568-line = substring( val = lv_paragraph len = lv_len ).
        INSERT temp568 INTO TABLE result.
        lv_paragraph = substring( val = lv_paragraph
                                  off = lv_len ).
        lv_format = `=`.
      ENDWHILE.
    ENDLOOP.

  ENDMETHOD.

  METHOD longtext_from_lines.

    DATA ls_line LIKE LINE OF val.
    LOOP AT val INTO ls_line.
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
        DATA lv_fm TYPE string.
          DATA temp569 TYPE ty_s_longtext_line.
          DATA ls_line LIKE temp569.
          DATA temp570 TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `longtext_read` ).

    lv_object = object.
    lv_id     = id.
    lv_name   = name.
    lv_langu  = sys_langu( langu ).

    TRY.
        CREATE DATA lr_lines TYPE STANDARD TABLE OF (`TLINE`).
        ASSIGN lr_lines->* TO <lines>.

        
        lv_fm = `READ_TEXT`.
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
          
          CLEAR temp569.
          
          ls_line = temp569.
          ASSIGN COMPONENT `TDFORMAT` OF STRUCTURE <line> TO <field>.
          
          temp570 = <field>.
          ls_line-format = c_trim( temp570 ).
          ASSIGN COMPONENT `TDLINE` OF STRUCTURE <line> TO <field>.
          ls_line-line = <field>.
          INSERT ls_line INTO TABLE lt_line.
        ENDLOOP.
        result = longtext_from_lines( lt_line ).

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
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
        DATA temp571 TYPE zabaputil_cl_util_context=>ty_t_longtext_line.
        DATA ls_line LIKE LINE OF temp571.
        DATA lv_fm TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

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
        
        temp571 = longtext_to_lines( val ).
        
        LOOP AT temp571 INTO ls_line.
          INSERT INITIAL LINE INTO TABLE <lines> ASSIGNING <line>.
          ASSIGN COMPONENT `TDFORMAT` OF STRUCTURE <line> TO <field>.
          <field> = ls_line-format.
          ASSIGN COMPONENT `TDLINE` OF STRUCTURE <line> TO <field>.
          <field> = ls_line-line.
        ENDLOOP.

        " insert = space: SAVE_TEXT finds out itself whether the text exists
        
        lv_fm = `SAVE_TEXT`.
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

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
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
        DATA lv_fm TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `longtext_delete` ).

    lv_object = object.
    lv_id     = id.
    lv_name   = name.
    lv_langu  = sys_langu( langu ).

    TRY.
        
        lv_fm = `DELETE_TEXT`.
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

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
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
    DATA lv_function TYPE string.
    DATA temp572 TYPE string.
    DATA lv_params TYPE string.
        DATA lt_interface TYPE zabaputil_cl_util_context=>ty_t_fm_param.
        DATA ls_interface LIKE LINE OF lt_interface.
          DATA lr_type TYPE REF TO data.
          DATA temp573 TYPE abap_componentdescr.
          DATA temp700 TYPE REF TO cl_abap_datadescr.
          DATA lo_struct TYPE REF TO cl_abap_structdescr.
        DATA li_json TYPE REF TO zabaputil_if_ajson.
        DATA lv_count TYPE i.
            DATA temp574 TYPE abap_func_excpbind.
          DATA temp575 TYPE REF TO data.
DATA temp701 TYPE abap_func_parmbind.
DATA temp137 LIKE temp701-kind.
          DATA temp576 TYPE abap_func_excpbind.
          DATA temp577 TYPE abap_func_excpbind.
          DATA temp578 TYPE abap_func_excpbind.
        DATA temp579 TYPE abap_func_excpbind.
        DATA lv_subrc TYPE i.
          DATA lv_exception TYPE string.
          DATA ls_exception LIKE LINE OF lt_exception.
          DATA lv_msg TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `rfc_call_dynamic` ).

    
    lv_function = c_trim_upper( function ).
    
    temp572 = params.
    
    lv_params = c_trim( temp572 ).

    TRY.
        
        lt_interface = sys_fm_get_interface( function    = lv_function
                                                   destination = destination ).

        " one structure with a component per parameter: the JSON is mapped
        " into it in one go, and the parameter table points into it
        
        LOOP AT lt_interface INTO ls_interface WHERE paramclass <> `X`.
          
          lr_type = sys_fm_create_param( ls_interface ).
          
          CLEAR temp573.
          temp573-name = ls_interface-parameter.
          
          temp700 ?= cl_abap_typedescr=>describe_by_data_ref( lr_type ).
          temp573-type = temp700.
          INSERT temp573
                 INTO TABLE lt_comp.
        ENDLOOP.

        IF lt_comp IS NOT INITIAL.
          
          lo_struct = cl_abap_structdescr=>create( lt_comp ).
          CREATE DATA lr_all TYPE HANDLE lo_struct.
          ASSIGN lr_all->* TO <all>.
        ENDIF.

        
        IF lv_params IS NOT INITIAL AND <all> IS ASSIGNED.
          li_json = zabaputil_cl_ajson=>parse( lv_params ).
          li_json->to_abap( EXPORTING iv_corresponding = abap_true
                            IMPORTING ev_container     = <all> ).
        ENDIF.

        
        lv_count = 0.
        LOOP AT lt_interface INTO ls_interface.
          IF ls_interface-paramclass = `X`.
            lv_count = lv_count + 1.
            
            CLEAR temp574.
            temp574-name = ls_interface-parameter.
            temp574-value = lv_count.
            INSERT temp574 INTO TABLE lt_exception.
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
          
GET REFERENCE OF <comp> INTO temp575.

CLEAR temp701.
temp701-name = ls_interface-parameter.

CASE ls_interface-paramclass.
  WHEN `I`.
    temp137 = abap_func_exporting.
  WHEN `E`.
    temp137 = abap_func_importing.
  WHEN `C`.
    temp137 = abap_func_changing.
  WHEN OTHERS.
    temp137 = abap_func_tables.
ENDCASE.
temp701-kind = temp137.
temp701-value = temp575.
INSERT temp701 INTO TABLE lt_param.
        ENDLOOP.

        IF destination IS NOT INITIAL.
          
          CLEAR temp576.
          temp576-name = `SYSTEM_FAILURE`.
          temp576-value = lv_count + 1.
          INSERT temp576 INTO TABLE lt_exception.
          
          CLEAR temp577.
          temp577-name = `COMMUNICATION_FAILURE`.
          temp577-value = lv_count + 2.
          INSERT temp577 INTO TABLE lt_exception.
        ELSE.
          " a MESSAGE TYPE E of a local module would end the request (via
          " RFC it arrives as SYSTEM_FAILURE)
          
          CLEAR temp578.
          temp578-name = `ERROR_MESSAGE`.
          temp578-value = lv_count + 4.
          INSERT temp578 INTO TABLE lt_exception.
        ENDIF.
        
        CLEAR temp579.
        temp579-name = `OTHERS`.
        temp579-value = lv_count + 3.
        INSERT temp579 INTO TABLE lt_exception.

        
        lv_subrc = sys_fm_call( function    = lv_function
                                      destination = destination
                                      t_param     = lt_param
                                      t_exception = lt_exception ).
        IF lv_subrc <> 0.
          
          lv_exception = `OTHERS`.
          
          LOOP AT lt_exception INTO ls_exception WHERE value = lv_subrc.
            lv_exception = ls_exception-name.
          ENDLOOP.
          
          lv_msg = ``.
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

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD rfc_ping.

    DATA lv_destination TYPE c LENGTH 32.
    DATA lv_msg         TYPE c LENGTH 255.
    DATA lo_provider    TYPE REF TO object.

    DATA temp580 TYPE string.
          DATA lv_class TYPE string.
          DATA lv_create TYPE string.
          DATA temp581 LIKE REF TO lo_provider.
DATA temp702 TYPE abap_parmbind_tab.
DATA temp703 LIKE LINE OF temp702.
          DATA lv_returning TYPE string.
          DATA lr_name TYPE REF TO data.
          DATA temp582 TYPE abap_parmbind_tab.
          DATA temp583 LIKE LINE OF temp582.
          FIELD-SYMBOLS <name> TYPE data.
        DATA lv_fm TYPE string.
          DATA temp584 TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.
    temp580 = destination.
    lv_destination = c_trim( temp580 ).

    TRY.
        IF check_abap_cloud( ) IS NOT INITIAL.
          " a BTP destination: the provider hands out the RFC destination name
          
          lv_class = `CL_RFC_DESTINATION_PROVIDER`.
          
          lv_create = `CREATE_BY_CLOUD_DESTINATION`.
          
          GET REFERENCE OF lo_provider INTO temp581.

CLEAR temp702.

temp703-name = `I_NAME`.
temp703-kind = cl_abap_objectdescr=>exporting.
temp703-value = sys_method_param( type_name = lv_class
method = lv_create
parameter = `I_NAME`
value = lv_destination ).
INSERT temp703 INTO TABLE temp702.
temp703-name = sys_method_returning( type_name = lv_class
method = lv_create ).
temp703-kind = cl_abap_objectdescr=>receiving.
temp703-value = temp581.
INSERT temp703 INTO TABLE temp702.
CALL METHOD (lv_class)=>(lv_create)
            PARAMETER-TABLE temp702.
          
          lv_returning = sys_method_returning( type_name = `IF_RFC_DEST`
                                                     method    = `GET_DESTINATION_NAME` ).
          
          lr_name = sys_method_param( type_name = `IF_RFC_DEST`
                                            method    = `GET_DESTINATION_NAME`
                                            parameter = lv_returning ).
          
          CLEAR temp582.
          
          temp583-name = lv_returning.
          temp583-kind = cl_abap_objectdescr=>receiving.
          temp583-value = lr_name.
          INSERT temp583 INTO TABLE temp582.
          CALL METHOD lo_provider->(`IF_RFC_DEST~GET_DESTINATION_NAME`)
            PARAMETER-TABLE temp582.
          
          ASSIGN lr_name->* TO <name>.
          lv_destination = <name>.
        ENDIF.

        
        lv_fm = `RFC_PING`.
        CALL FUNCTION lv_fm
          DESTINATION lv_destination
          EXCEPTIONS
            system_failure        = 1 MESSAGE lv_msg
            communication_failure = 2 MESSAGE lv_msg
            error_message         = 3
            OTHERS                = 3.
        IF sy-subrc <> 0.
          
          IF lv_msg IS INITIAL.
            temp584 = |sy-subrc { sy-subrc }|.
          ELSE.
            temp584 = lv_msg.
          ENDIF.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |rfc_ping { destination }: { temp584 }|.
        ENDIF.

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
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
        DATA lv_class TYPE string.
        DATA temp585 LIKE REF TO lo_api.
DATA temp704 TYPE abap_parmbind_tab.
DATA temp705 LIKE LINE OF temp704.
        DATA temp586 TYPE string.
          DATA temp587 TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `mime_read` ).

    TRY.
        
        lv_class = `CL_MIME_REPOSITORY_API`.
        
        GET REFERENCE OF lo_api INTO temp585.

CLEAR temp704.

temp705-name = sys_method_returning( type_name = lv_class
method = `GET_API` ).
temp705-kind = cl_abap_objectdescr=>receiving.
temp705-value = temp585.
INSERT temp705 INTO TABLE temp704.
CALL METHOD (lv_class)=>(`GET_API`)
          PARAMETER-TABLE temp704.

        
        temp586 = url.
        CALL METHOD lo_api->(`IF_MR_API~GET`)
          EXPORTING
            i_url              = temp586
          IMPORTING
            e_content          = result
          EXCEPTIONS
            parameter_missing  = 1
            error_occured      = 2
            not_found          = 3
            permission_failure = 4
            OTHERS             = 5.
        IF sy-subrc <> 0.
          
          IF sy-subrc = 3.
            temp587 = `not found`.
          ELSEIF sy-subrc = 4.
            temp587 = `not authorized`.
          ELSE.
            temp587 = `could not be read`.
          ENDIF.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |mime_read: { url } { temp587 }|.
        ENDIF.

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD mime_save.

    DATA lo_api       TYPE REF TO object.
    DATA lt_exception TYPE abap_excpbind_tab.
        DATA lv_class TYPE string.
        DATA temp588 LIKE REF TO lo_api.
DATA temp706 TYPE abap_parmbind_tab.
DATA temp707 LIKE LINE OF temp706.
        DATA temp589 TYPE string.
        DATA lv_url LIKE temp589.
        DATA temp590 TYPE string.
        DATA lv_package LIKE temp590.
        DATA temp591 TYPE string.
        DATA lv_transport LIKE temp591.
        DATA lv_true LIKE abap_true.
        DATA lv_content LIKE content.
        DATA temp592 LIKE REF TO lv_true.
DATA temp708 LIKE REF TO lv_true.
DATA temp138 LIKE REF TO lv_package.
DATA temp5 LIKE REF TO lv_content.
DATA temp40 LIKE REF TO lv_url.
DATA temp1 TYPE abap_parmbind_tab.
DATA temp2 LIKE LINE OF temp1.
DATA lt_param LIKE temp1.
          DATA temp593 LIKE REF TO lv_transport.
DATA temp709 TYPE abap_parmbind.
        DATA temp594 TYPE abap_excpbind_tab.
        DATA temp595 LIKE LINE OF temp594.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `mime_save` ).

    TRY.
        
        lv_class = `CL_MIME_REPOSITORY_API`.
        
        GET REFERENCE OF lo_api INTO temp588.

CLEAR temp706.

temp707-name = sys_method_returning( type_name = lv_class
method = `GET_API` ).
temp707-kind = cl_abap_objectdescr=>receiving.
temp707-value = temp588.
INSERT temp707 INTO TABLE temp706.
CALL METHOD (lv_class)=>(`GET_API`)
          PARAMETER-TABLE temp706.

        
        temp589 = url.
        
        lv_url = temp589.
        
        temp590 = package.
        
        lv_package = temp590.
        
        temp591 = transport.
        
        lv_transport = temp591.
        
        lv_true = abap_true.
        
        lv_content = content.
        
        GET REFERENCE OF lv_true INTO temp592.

GET REFERENCE OF lv_true INTO temp708.

GET REFERENCE OF lv_package INTO temp138.

GET REFERENCE OF lv_content INTO temp5.

GET REFERENCE OF lv_url INTO temp40.

CLEAR temp1.

temp2-name = `I_URL`.
temp2-kind = cl_abap_objectdescr=>exporting.
temp2-value = temp40.
INSERT temp2 INTO TABLE temp1.
temp2-name = `I_CONTENT`.
temp2-kind = cl_abap_objectdescr=>exporting.
temp2-value = temp5.
INSERT temp2 INTO TABLE temp1.
temp2-name = `I_DEV_PACKAGE`.
temp2-kind = cl_abap_objectdescr=>exporting.
temp2-value = temp138.
INSERT temp2 INTO TABLE temp1.
temp2-name = `I_SUPPRESS_DIALOGS`.
temp2-kind = cl_abap_objectdescr=>exporting.
temp2-value = temp708.
INSERT temp2 INTO TABLE temp1.
temp2-name = `I_SUPPRESS_PACKAGE_DIALOG`.
temp2-kind = cl_abap_objectdescr=>exporting.
temp2-value = temp592.
INSERT temp2 INTO TABLE temp1.

lt_param = temp1.
        IF lv_transport IS NOT INITIAL.
          
          GET REFERENCE OF lv_transport INTO temp593.

CLEAR temp709.
temp709-name = `I_CORR_NUMBER`.
temp709-kind = cl_abap_objectdescr=>exporting.
temp709-value = temp593.
INSERT temp709 INTO TABLE lt_param.
        ENDIF.
        
        CLEAR temp594.
        
        temp595-name = `OTHERS`.
        temp595-value = 1.
        INSERT temp595 INTO TABLE temp594.
        lt_exception = temp594.

        CALL METHOD lo_api->(`IF_MR_API~PUT`)
          PARAMETER-TABLE lt_param
          EXCEPTION-TABLE lt_exception.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |mime_save: { url } could not be saved|.
        ENDIF.

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
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
        DATA lv_owner TYPE c LENGTH 12.
        DATA lv_table TYPE string.
          DATA lt_interface TYPE zabaputil_cl_util_context=>ty_t_fm_param.
          DATA temp596 TYPE zabaputil_cl_util_context=>ty_s_fm_param.
          DATA temp597 TYPE zabaputil_cl_util_context=>ty_s_fm_param.
          DATA lr_spool TYPE REF TO data.
          DATA temp598 TYPE zabaputil_cl_util_context=>ty_s_fm_param.
          DATA temp599 TYPE zabaputil_cl_util_context=>ty_s_fm_param.
          DATA lr_part TYPE REF TO data.
          FIELD-SYMBOLS <spool> TYPE data.
          FIELD-SYMBOLS <part> TYPE data.
          DATA temp600 LIKE REF TO result.
DATA temp710 TYPE abap_func_parmbind_tab.
DATA temp711 LIKE LINE OF temp710.
DATA lt_param LIKE temp710.
          DATA temp601 TYPE abap_func_excpbind_tab.
          DATA temp602 LIKE LINE OF temp601.
        DATA temp603 TYPE string.
        DATA lv_fm LIKE temp603.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `spool_get_pdf` ).

    lv_spool = spool_id.

    TRY.
        
        
        lv_table = `TSP01`.
        SELECT SINGLE rqdoctype rqowner FROM (lv_table) INTO (lv_doctype, lv_owner) WHERE rqident = lv_spool .
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |spool_get_pdf: spool request { lv_spool } not found|.
        ENDIF.
        IF owner_only = abap_true AND lv_owner <> sy-uname.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |spool_get_pdf: spool request { lv_spool } belongs to another user|.
        ENDIF.

        IF lv_doctype = `ADSP`.
          " an Adobe form in the spool already is a PDF
          
          lt_interface = sys_fm_get_interface( `FPCOMP_CREATE_PDF_FROM_SPOOL` ).
          
          CLEAR temp596.
          
          READ TABLE lt_interface INTO temp597 WITH KEY parameter = `I_SPOOLID`.
          IF sy-subrc = 0.
            temp596 = temp597.
          ENDIF.
          
          lr_spool = sys_fm_create_param( temp596 ).
          
          CLEAR temp598.
          
          READ TABLE lt_interface INTO temp599 WITH KEY parameter = `I_PARTNUM`.
          IF sy-subrc = 0.
            temp598 = temp599.
          ENDIF.
          
          lr_part = sys_fm_create_param( temp598 ).
          
          ASSIGN lr_spool->* TO <spool>.
          <spool> = lv_spool.
          
          ASSIGN lr_part->* TO <part>.
          <part> = 1.
          
          GET REFERENCE OF result INTO temp600.

CLEAR temp710.

temp711-name = `I_SPOOLID`.
temp711-kind = abap_func_exporting.
temp711-value = lr_spool.
INSERT temp711 INTO TABLE temp710.
temp711-name = `I_PARTNUM`.
temp711-kind = abap_func_exporting.
temp711-value = lr_part.
INSERT temp711 INTO TABLE temp710.
temp711-name = `E_PDF`.
temp711-kind = abap_func_importing.
temp711-value = temp600.
INSERT temp711 INTO TABLE temp710.

lt_param = temp710.
          
          CLEAR temp601.
          
          temp602-name = `OTHERS`.
          temp602-value = 1.
          INSERT temp602 INTO TABLE temp601.
          IF sys_fm_call( function    = `FPCOMP_CREATE_PDF_FROM_SPOOL`
                          t_param     = lt_param
                          t_exception = temp601 ) <> 0.
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
        
        IF lv_doctype = `OTF`.
          temp603 = `CONVERT_OTFSPOOLJOB_2_PDF`.
        ELSE.
          temp603 = `CONVERT_ABAPSPOOLJOB_2_PDF`.
        ENDIF.
        
        lv_fm = temp603.
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

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
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
        DATA lv_fm TYPE string.
        DATA temp604 TYPE abap_func_parmbind_tab.
        DATA temp605 LIKE LINE OF temp604.
        DATA lt_param LIKE temp604.
          DATA temp606 TYPE REF TO cl_abap_structdescr.
          DATA lo_struct LIKE temp606.
          DATA ls_comp LIKE LINE OF lo_struct->components.
            DATA temp607 TYPE REF TO data.
DATA temp712 TYPE abap_func_parmbind.
        DATA temp608 TYPE abap_func_excpbind_tab.
        DATA temp609 LIKE LINE OF temp608.
        DATA x TYPE REF TO cx_root.
        DATA temp610 TYPE REF TO zabaputil_cx_util_error.
        DATA temp611 TYPE abap_bool.
          DATA temp612 TYPE REF TO zabaputil_cx_util_error.
          DATA lx_util LIKE temp612.

    x_raise_on_cloud( `form_get_pdf` ).

    lv_form = c_trim_upper( form_name ).

    TRY.
        " class-based exceptions (CX_FP_API) reach the CATCH below; only
        " ERROR_MESSAGE can be named next to them - a MESSAGE TYPE E would
        " end the request otherwise
        
        lv_fm = `FP_FUNCTION_MODULE_NAME`.
        CALL FUNCTION lv_fm
          EXPORTING
            i_name        = lv_form
          IMPORTING
            e_funcname    = lv_function
          EXCEPTIONS
            error_message = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |FP_FUNCTION_MODULE_NAME failed for { lv_form }|.
        ENDIF.

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

        
        CLEAR temp604.
        
        temp605-name = `/1BCDWB/DOCPARAMS`.
        temp605-kind = abap_func_exporting.
        temp605-value = lr_docparams.
        INSERT temp605 INTO TABLE temp604.
        temp605-name = `/1BCDWB/FORMOUTPUT`.
        temp605-kind = abap_func_importing.
        temp605-value = lr_formoutput.
        INSERT temp605 INTO TABLE temp604.
        
        lt_param = temp604.
        IF data IS SUPPLIED.
          
          temp606 ?= cl_abap_typedescr=>describe_by_data( data ).
          
          lo_struct = temp606.
          
          LOOP AT lo_struct->components INTO ls_comp.
            ASSIGN COMPONENT ls_comp-name OF STRUCTURE data TO <field>.
            
GET REFERENCE OF <field> INTO temp607.

CLEAR temp712.
temp712-name = ls_comp-name.
temp712-kind = abap_func_exporting.
temp712-value = temp607.
INSERT temp712 INTO TABLE lt_param.
          ENDLOOP.
        ENDIF.

        
        CLEAR temp608.
        
        temp609-name = `OTHERS`.
        temp609-value = 1.
        INSERT temp609 INTO TABLE temp608.
        IF sys_fm_call( function    = lv_function
                        t_param     = lt_param
                        t_exception = temp608 ) <> 0.
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

        
      CATCH cx_root INTO x.
        IF lv_job_open = abap_true.
          lv_fm = `FP_JOB_CLOSE`.
          CALL FUNCTION lv_fm
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
        ENDIF.
        
        
        TRY.
            temp610 ?= x.
            temp611 = boolc( temp610 IS BOUND ).
          CATCH cx_sy_move_cast_error.
            temp611 = abap_false.
        ENDTRY.
        IF temp611 = abap_true.
          
          temp612 ?= x.
          
          lx_util = temp612.
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
    TYPES temp33 TYPE STANDARD TABLE OF ty_s_text WITH DEFAULT KEY.
DATA lt_variant TYPE temp33.
    TYPES temp34 TYPE STANDARD TABLE OF ty_s_text WITH DEFAULT KEY.
DATA lt_text    TYPE temp34.
    DATA lv_report  TYPE c LENGTH 40.
    DATA lv_langu   TYPE c LENGTH 1.
        DATA lv_table TYPE string.
        DATA ls_variant LIKE LINE OF lt_variant.
          DATA temp613 TYPE zabaputil_cl_util_context=>ty_s_variant.
          DATA temp713 TYPE string.
          DATA temp714 TYPE string.
          DATA temp715 TYPE ty_s_text.
          DATA temp139 TYPE string.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `variant_list` ).

    lv_report = c_trim_upper( report ).
    lv_langu  = sys_langu( langu ).

    TRY.
        
        lv_table = `VARID`.
        SELECT variant FROM (lv_table) INTO CORRESPONDING FIELDS OF TABLE lt_variant WHERE report = lv_report
          .
        lv_table = `VARIT`.
        SELECT variant vtext FROM (lv_table) INTO CORRESPONDING FIELDS OF TABLE lt_text WHERE report = lv_report AND langu = lv_langu
          .

        
        LOOP AT lt_variant INTO ls_variant.
          
          CLEAR temp613.
          
          temp713 = ls_variant-variant.
          temp613-variant = c_trim( temp713 ).
          
          CLEAR temp714.
          
          READ TABLE lt_text INTO temp715 WITH KEY variant = ls_variant-variant.
          IF sy-subrc = 0.
            temp714 = temp715-vtext.
          ENDIF.
          
          temp139 = temp714.
          temp613-text = c_trim( temp139 ).
          INSERT temp613
                 INTO TABLE result.
        ENDLOOP.
        SORT result BY variant.

        
      CATCH cx_root INTO x.
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
        DATA lv_fm TYPE string.
          DATA temp614 TYPE string.
          DATA temp615 TYPE ty_s_variant_value.
          DATA ls_result LIKE temp615.
          DATA temp616 TYPE string.
          DATA temp617 TYPE string.
          DATA temp618 TYPE string.
          DATA temp619 TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `variant_read` ).

    lv_report  = c_trim_upper( report ).
    lv_variant = c_trim_upper( variant ).

    TRY.
        CREATE DATA lr_values TYPE STANDARD TABLE OF (`RSPARAMS`).
        ASSIGN lr_values->* TO <values>.

        
        lv_fm = `RS_VARIANT_CONTENTS`.
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
          
          IF sy-subrc = 1.
            temp614 = `does not exist`.
          ELSEIF sy-subrc = 2.
            temp614 = `is obsolete`.
          ELSE.
            temp614 = `could not be read`.
          ENDIF.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |variant_read: variant { lv_variant } of { lv_report } { temp614 }|.
        ENDIF.

        LOOP AT <values> ASSIGNING <value>.
          
          CLEAR temp615.
          
          ls_result = temp615.
          ASSIGN COMPONENT `SELNAME` OF STRUCTURE <value> TO <field>.
          
          temp616 = <field>.
          ls_result-selname = c_trim( temp616 ).
          ASSIGN COMPONENT `KIND` OF STRUCTURE <value> TO <field>.
          ls_result-kind = <field>.
          ASSIGN COMPONENT `SIGN` OF STRUCTURE <value> TO <field>.
          ls_result-sign = <field>.
          ASSIGN COMPONENT `OPTION` OF STRUCTURE <value> TO <field>.
          
          temp617 = <field>.
          ls_result-option = c_trim( temp617 ).
          ASSIGN COMPONENT `LOW` OF STRUCTURE <value> TO <field>.
          
          temp618 = <field>.
          ls_result-low = c_trim( temp618 ).
          ASSIGN COMPONENT `HIGH` OF STRUCTURE <value> TO <field>.
          
          temp619 = <field>.
          ls_result-high = c_trim( temp619 ).
          INSERT ls_result INTO TABLE result.
        ENDLOOP.

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD amc_send.

    DATA lo_producer TYPE REF TO object.
    DATA lr_value    TYPE REF TO data.
    FIELD-SYMBOLS <value> TYPE any.
        DATA lv_class TYPE string.
        DATA temp620 TYPE REF TO cl_abap_classdescr.
        DATA lo_class LIKE temp620.
        DATA temp621 TYPE abap_parmbind_tab.
        DATA lt_param LIKE temp621.
        DATA temp622 TYPE ty_t_name_value.
        DATA temp623 LIKE LINE OF temp622.
        DATA lt_value LIKE temp622.
          DATA temp624 TYPE zabaputil_cl_util_context=>ty_s_name_value.
        DATA ls_value LIKE LINE OF lt_value.
          DATA lo_type TYPE REF TO cl_abap_datadescr.
          DATA temp625 TYPE abap_parmbind.
        DATA temp626 LIKE REF TO lo_producer.
DATA temp716 TYPE abap_parmbind.
        DATA temp627 TYPE string.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `amc_send` ).

    TRY.
        " the parameter types are read from the class: they differ by release
        
        lv_class = `CL_AMC_CHANNEL_MANAGER`.
        
        temp620 ?= cl_abap_typedescr=>describe_by_name( lv_class ).
        
        lo_class = temp620.
        
        CLEAR temp621.
        
        lt_param = temp621.
        
        CLEAR temp622.
        
        temp623-n = `I_APPLICATION_ID`.
        temp623-v = application_id.
        INSERT temp623 INTO TABLE temp622.
        temp623-n = `I_CHANNEL_ID`.
        temp623-v = channel_id.
        INSERT temp623 INTO TABLE temp622.
        
        lt_value = temp622.
        IF channel_extension_id IS NOT INITIAL.
          
          CLEAR temp624.
          temp624-n = `I_CHANNEL_EXTENSION_ID`.
          temp624-v = channel_extension_id.
          INSERT temp624 INTO TABLE lt_value.
        ENDIF.
        
        LOOP AT lt_value INTO ls_value.
          
          lo_type = lo_class->get_method_parameter_type( p_method_name    = `CREATE_MESSAGE_PRODUCER`
                                                               p_parameter_name = ls_value-n ).
          CREATE DATA lr_value TYPE HANDLE lo_type.
          ASSIGN lr_value->* TO <value>.
          <value> = ls_value-v.
          
          CLEAR temp625.
          temp625-name = ls_value-n.
          temp625-kind = cl_abap_objectdescr=>exporting.
          temp625-value = lr_value.
          INSERT temp625 INTO TABLE lt_param.
        ENDLOOP.
        
        GET REFERENCE OF lo_producer INTO temp626.

CLEAR temp716.
temp716-name = sys_method_returning( type_name = lv_class method = `CREATE_MESSAGE_PRODUCER` ).
temp716-kind = cl_abap_objectdescr=>receiving.
temp716-value = temp626.
INSERT temp716 INTO TABLE lt_param.

        CALL METHOD (lv_class)=>(`CREATE_MESSAGE_PRODUCER`)
          PARAMETER-TABLE lt_param.

        
        temp627 = message.
        CALL METHOD lo_producer->(`IF_AMC_MESSAGE_PRODUCER_TEXT~SEND`)
          EXPORTING
            i_message = temp627.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD sys_method_returning.

    DATA temp628 TYPE REF TO cl_abap_objectdescr.
    DATA lo_descr LIKE temp628.
    DATA lv_method TYPE string.
    DATA ls_method LIKE LINE OF lo_descr->methods.
      DATA ls_parameter LIKE LINE OF ls_method-parameters.
    temp628 ?= cl_abap_typedescr=>describe_by_name( type_name ).
    
    lo_descr = temp628.
    
    lv_method = to_upper( method ).
    
    LOOP AT lo_descr->methods INTO ls_method WHERE name = lv_method.
      
      LOOP AT ls_method-parameters INTO ls_parameter WHERE parm_kind = cl_abap_objectdescr=>returning.
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
    DATA temp629 TYPE string.
    DATA lv_file TYPE string.
    DATA lv_ext TYPE string.
    DATA lv_dot TYPE i.
    DATA temp630 LIKE lv_doctype.
    DATA temp631 TYPE string.
    DATA lv_descr LIKE temp631.
        DATA lt_interface TYPE zabaputil_cl_util_context=>ty_t_fm_param.
        DATA temp632 TYPE zabaputil_cl_util_context=>ty_s_fm_param.
        DATA temp633 TYPE zabaputil_cl_util_context=>ty_s_fm_param.
        DATA lv_fm TYPE string.
        DATA temp634 TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `gos_attach` ).

    
    temp629 = file_name.
    
    lv_file = c_trim( temp629 ).
    
    lv_ext = ``.
    
    lv_dot = find( val = lv_file
                         sub = `.`
                         occ = -1 ).
    IF lv_dot >= 0.
      lv_ext = to_upper( substring( val = lv_file
                                    off = lv_dot + 1 ) ).
    ENDIF.
    " the document type is three characters (SOOD-OBJ_TP): DOCX, XLSX and
    " JPEG are stored as DOC, XLS and JPE - the full name, which the
    " attachment opens by, is in &SO_FILENAME. A file without an extension
    " was an empty type, which SO_DOCUMENT_INSERT_API1 rejects
    
    IF lv_ext IS INITIAL.
      temp630 = `BIN`.
    ELSE.
      temp630 = lv_ext.
    ENDIF.
    lv_doctype = temp630.
    
    IF description IS NOT INITIAL.
      temp631 = description.
    ELSEIF lv_dot > 0.
      temp631 = substring( val = lv_file len = lv_dot ).
    ELSE.
      temp631 = lv_file.
    ENDIF.
    
    lv_descr = temp631.

    TRY.
        " the attachment is an office document in the user's root folder
        
        lt_interface = sys_fm_get_interface( `SO_DOCUMENT_INSERT_API1` ).
        
        CLEAR temp632.
        
        READ TABLE lt_interface INTO temp633 WITH KEY parameter = `FOLDER_ID`.
        IF sy-subrc = 0.
          temp632 = temp633.
        ENDIF.
        lr_folder = sys_fm_create_param( temp632 ).
        ASSIGN lr_folder->* TO <folder>.
        CREATE DATA lr_object_a TYPE (`SOODK`).
        ASSIGN lr_object_a->* TO <object_a>.
        
        lv_fm = `SO_FOLDER_ROOT_ID_GET`.
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
        
        temp634 = <field>.
        result = c_trim( temp634 ).

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

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
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
        DATA lv_class TYPE string.
        DATA lv_method TYPE string.
        DATA temp635 TYPE REF TO cl_abap_classdescr.
        DATA lo_class LIKE temp635.
        DATA lo_type TYPE REF TO cl_abap_datadescr.
        DATA temp636 TYPE abap_parmbind_tab.
        DATA temp637 LIKE LINE OF temp636.
        DATA lt_param LIKE temp636.
          DATA temp638 TYPE ty_s_gos.
          DATA ls_result LIKE temp638.
          DATA temp639 TYPE string.
          DATA lv_fm TYPE string.
              DATA temp640 TYPE string.
              DATA temp641 TYPE string.
              DATA temp642 TYPE string.
              DATA lv_header TYPE string.
          DATA lv_dot TYPE i.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `gos_list` ).

    TRY.
        " the parameter types are read from the class
        
        lv_class = `CL_BINARY_RELATION`.
        
        lv_method = `READ_LINKS_OF_BINRELS`.
        
        temp635 ?= cl_abap_typedescr=>describe_by_name( lv_class ).
        
        lo_class = temp635.
        
        lo_type = lo_class->get_method_parameter_type( p_method_name    = lv_method
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

        
        CLEAR temp636.
        
        temp637-name = `IS_OBJECT`.
        temp637-kind = cl_abap_objectdescr=>exporting.
        temp637-value = lr_object.
        INSERT temp637 INTO TABLE temp636.
        temp637-name = `IT_RELATION_OPTIONS`.
        temp637-kind = cl_abap_objectdescr=>exporting.
        temp637-value = lr_options.
        INSERT temp637 INTO TABLE temp636.
        temp637-name = `ET_LINKS`.
        temp637-kind = cl_abap_objectdescr=>importing.
        temp637-value = lr_links.
        INSERT temp637 INTO TABLE temp636.
        
        lt_param = temp636.
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
          
          CLEAR temp638.
          
          ls_result = temp638.
          ASSIGN COMPONENT `INSTID_B` OF STRUCTURE <link> TO <field>.
          
          temp639 = <field>.
          ls_result-doc_id = c_trim( temp639 ).

          CLEAR <docdata>.
          CLEAR <header>.
          
          lv_fm = `SO_DOCUMENT_READ_API1`.
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
              
              temp640 = <field>.
              ls_result-description = c_trim( temp640 ).
            ENDIF.
            ASSIGN COMPONENT `DOC_SIZE` OF STRUCTURE <docdata> TO <field>.
            IF sy-subrc = 0.
              ls_result-size = <field>.
            ENDIF.
            ASSIGN COMPONENT `CREAT_NAME` OF STRUCTURE <docdata> TO <field>.
            IF sy-subrc = 0.
              
              temp641 = <field>.
              ls_result-created_by = c_trim( temp641 ).
            ENDIF.
            ASSIGN COMPONENT `CREAT_DATE` OF STRUCTURE <docdata> TO <field>.
            IF sy-subrc = 0.
              ls_result-created_on = <field>.
            ENDIF.
            LOOP AT <header> ASSIGNING <line>.
              ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field>.
              
              temp642 = <field>.
              
              lv_header = c_trim( temp642 ).
              IF c_starts_with( val    = lv_header
                                prefix = `&SO_FILENAME=` ) IS NOT INITIAL.
                ls_result-file_name = substring( val = lv_header
                                                 off = 13 ).
              ENDIF.
            ENDLOOP.
          ENDIF.

          
          lv_dot = find( val = ls_result-file_name
                               sub = `.`
                               occ = -1 ).
          IF lv_dot >= 0.
            ls_result-file_ext = to_upper( substring( val = ls_result-file_name
                                                      off = lv_dot + 1 ) ).
          ENDIF.
          INSERT ls_result INTO TABLE result.
        ENDLOOP.

        
      CATCH cx_root INTO x.
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
        DATA lv_fm TYPE string.
          DATA temp643 TYPE string.
        DATA lx TYPE REF TO zabaputil_cx_util_error.
        DATA x TYPE REF TO cx_root.

    x_raise_on_cloud( `gos_read` ).

    TRY.
        CREATE DATA lr_docdata TYPE (`SOFOLENTI1`).
        ASSIGN lr_docdata->* TO <docdata>.
        CREATE DATA lr_hex TYPE STANDARD TABLE OF (`SOLIX`).
        ASSIGN lr_hex->* TO <hex>.
        CREATE DATA lr_content TYPE STANDARD TABLE OF (`SOLISTI1`).
        ASSIGN lr_content->* TO <content>.

        
        lv_fm = `SO_DOCUMENT_READ_API1`.
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
          
          IF sy-tabix = 1.
            temp643 = <field>.
          ELSE.
            temp643 = lv_text && cv_char_util_newline && <field>.
          ENDIF.
          lv_text = temp643.
        ENDLOOP.
        result = conv_get_xstring_by_string( lv_text ).

        
      CATCH zabaputil_cx_util_error INTO lx.
        RAISE EXCEPTION lx.
        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD sys_method_param.

    DATA temp644 TYPE REF TO cl_abap_objectdescr.
    DATA lo_descr LIKE temp644.
    DATA lo_type TYPE REF TO cl_abap_datadescr.
      FIELD-SYMBOLS <value> TYPE data.
    temp644 ?= cl_abap_typedescr=>describe_by_name( type_name ).
    
    lo_descr = temp644.
    
    lo_type = lo_descr->get_method_parameter_type( p_method_name    = to_upper( method )
                                                         p_parameter_name = to_upper( parameter ) ).
    CREATE DATA result TYPE HANDLE lo_type.
    IF value IS SUPPLIED.
      
      ASSIGN result->* TO <value>.
      <value> = value.
    ENDIF.

  ENDMETHOD.

  METHOD bapi_finish.

    DATA lv_fm     TYPE c LENGTH 30.
    DATA lr_return TYPE REF TO data.
    FIELD-SYMBOLS <ls_return> TYPE any.
        DATA lv_type TYPE string.
        DATA lx_error TYPE REF TO cx_root.

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
        
        lv_type = bapi_get_type( <ls_return> ).
        IF lv_type CA `EAX` AND lv_type IS NOT INITIAL.
          bapi_msg_add_bapiret2( EXPORTING is_return  = <ls_return>
                                 CHANGING  ct_bapiret = ct_bapiret ).
          cv_is_error = abap_true.
        ENDIF.

        
      CATCH cx_root INTO lx_error.
        bapi_msg_add_exception( EXPORTING ix_error   = lx_error
                                CHANGING  ct_bapiret = ct_bapiret ).
        cv_is_error = abap_true.
    ENDTRY.

  ENDMETHOD.

  METHOD bapi_check_is_error.

    FIELD-SYMBOLS <ls_row> TYPE any.
      DATA lv_type TYPE string.

    LOOP AT it_bapiret ASSIGNING <ls_row>.
      
      lv_type = bapi_get_type( <ls_row> ).
      IF lv_type CA `EAX` AND lv_type IS NOT INITIAL.
        rv_error = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD bapi_msg_add_sy.

    DATA ls_sy TYPE zabaputil_cl_util_context=>ty_syst.
    DATA temp645 TYPE string.
    ls_sy = context_get_sy( ).

    IF ls_sy-msgid IS INITIAL.
      bapi_msg_add_text( EXPORTING iv_text    = |{ iv_context } failed|
                         CHANGING  ct_bapiret = ct_bapiret ).
      RETURN.
    ENDIF.

    " a message left by EXCEPTIONS error_message is an error, whatever its type
    
    IF ls_sy-msgty CA `AX` AND ls_sy-msgty IS NOT INITIAL.
      temp645 = ls_sy-msgty.
    ELSE.
      temp645 = `E`.
    ENDIF.
    bapi_msg_add( EXPORTING it_msg     = msg_get_t( ls_sy )
                            iv_type    = temp645
                  CHANGING  ct_bapiret = ct_bapiret ).

  ENDMETHOD.

  METHOD bapi_msg_add_text.

    DATA temp646 TYPE zabaputil_cl_util_context=>ty_t_msg.
    DATA temp647 LIKE LINE OF temp646.
    CLEAR temp646.
    
    temp647-type = iv_type.
    temp647-text = iv_text.
    INSERT temp647 INTO TABLE temp646.
    bapi_msg_add( EXPORTING it_msg     = temp646
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

    DATA ls_msg LIKE LINE OF it_msg.
      DATA temp648 TYPE ty_t_name_value.
      DATA temp649 LIKE LINE OF temp648.
      DATA temp717 TYPE zabaputil_cl_util_context=>ty_s_name_value-v.
      DATA lt_value LIKE temp648.
      DATA ls_value LIKE LINE OF lt_value.
        DATA lr_val TYPE REF TO data.
    LOOP AT it_msg INTO ls_msg.
      CREATE DATA lr_row LIKE LINE OF ct_bapiret.
      ASSIGN lr_row->* TO <ls_row>.

      
      CLEAR temp648.
      
      temp649-n = `TYPE MSGTY`.
      
      IF iv_type IS NOT INITIAL.
        temp717 = iv_type.
      ELSE.
        temp717 = to_upper( ls_msg-type ).
      ENDIF.
      temp649-v = temp717.
      INSERT temp649 INTO TABLE temp648.
      temp649-n = `ID MSGID`.
      temp649-v = ls_msg-id.
      INSERT temp649 INTO TABLE temp648.
      temp649-n = `NUMBER MSGNO`.
      temp649-v = ls_msg-no.
      INSERT temp649 INTO TABLE temp648.
      temp649-n = `MESSAGE`.
      temp649-v = ls_msg-text.
      INSERT temp649 INTO TABLE temp648.
      temp649-n = `MESSAGE_V1 MSGV1`.
      temp649-v = ls_msg-v1.
      INSERT temp649 INTO TABLE temp648.
      temp649-n = `MESSAGE_V2 MSGV2`.
      temp649-v = ls_msg-v2.
      INSERT temp649 INTO TABLE temp648.
      temp649-n = `MESSAGE_V3 MSGV3`.
      temp649-v = ls_msg-v3.
      INSERT temp649 INTO TABLE temp648.
      temp649-n = `MESSAGE_V4 MSGV4`.
      temp649-v = ls_msg-v4.
      INSERT temp649 INTO TABLE temp648.
      
      lt_value = temp648.

      
      LOOP AT lt_value INTO ls_value WHERE v IS NOT INITIAL.
        
        lr_val = bapi_assign_component( iv_names = ls_value-n
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

    DATA lr_type TYPE REF TO data.
    lr_type = bapi_assign_component( iv_names = `TYPE MSGTY`
                                           is_data  = is_data ).
    IF lr_type IS BOUND.
      ASSIGN lr_type->* TO <lv_type>.
      rv_type = <lv_type>.
    ENDIF.

  ENDMETHOD.

  METHOD bapi_assign_component.

    FIELD-SYMBOLS <lv_comp> TYPE any.

    TYPES temp35 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_name TYPE temp35.
    DATA lv_name LIKE LINE OF lt_name.
    SPLIT iv_names AT ` ` INTO TABLE lt_name.
    
    LOOP AT lt_name INTO lv_name.
      ASSIGN COMPONENT lv_name OF STRUCTURE is_data TO <lv_comp>.
      IF sy-subrc = 0.
        GET REFERENCE OF <lv_comp> INTO rr.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD bapi_call_function.

    DATA lv_name TYPE string.
        DATA temp650 TYPE abap_func_excpbind_tab.
        DATA temp651 LIKE LINE OF temp650.
        DATA lx_error TYPE REF TO cx_root.

    lv_name = c_trim_upper( iv_name ).
    TRY.
        
        CLEAR temp650.
        
        temp651-name = `ERROR_MESSAGE`.
        temp651-value = 98.
        INSERT temp651 INTO TABLE temp650.
        temp651-name = `OTHERS`.
        temp651-value = 99.
        INSERT temp651 INTO TABLE temp650.
        IF sys_fm_call( function    = lv_name
                        t_param     = it_param
                        t_exception = temp650 ) <> 0.
          bapi_msg_add_sy( EXPORTING iv_context = lv_name
                           CHANGING  ct_bapiret = ct_bapiret ).
        ENDIF.
        
      CATCH cx_root INTO lx_error.
        bapi_msg_add_exception( EXPORTING ix_error   = lx_error
                                CHANGING  ct_bapiret = ct_bapiret ).
    ENDTRY.

  ENDMETHOD.

  METHOD bapi_set_material.

    DATA lv_field TYPE string.
    DATA lr_value TYPE REF TO data.
      FIELD-SYMBOLS <lv_value> TYPE any.
    lv_field = to_upper( iv_field ).
    
    lr_value = bapi_assign_component( iv_names = |{ lv_field }_LONG { lv_field }|
                                            is_data  = cs_data ).
    IF lr_value IS BOUND.
      
      ASSIGN lr_value->* TO <lv_value>.
      <lv_value> = iv_matnr.
    ENDIF.

  ENDMETHOD.

  METHOD bapi_refresh_buffers.

    DATA lv_fm TYPE c LENGTH 30.

    DATA lv_area TYPE string.
    DATA temp652 TYPE string_table.
    DATA lt_fm LIKE temp652.
    DATA lv_name LIKE LINE OF lt_fm.
    lv_area = to_upper( iv_area ).
    
    CLEAR temp652.
    
    lt_fm = temp652.

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
    
    LOOP AT lt_fm INTO lv_name.
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

    DATA temp653 TYPE string.
    temp653 = iv_value.
    rv_result = condense( temp653 ).
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

    DATA lv_len TYPE i.
    DATA lv_sep_len TYPE i.
      DATA lv_char TYPE string.
        DATA lv_end TYPE i.
    lv_len = strlen( val ).
    
    lv_sep_len = strlen( separator ).
    IF lv_sep_len = 0.
      RETURN.
    ENDIF.

    WHILE lv_pos < lv_len.

      
      lv_char = substring( val = val
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
        
        lv_end = nmin( val1 = lv_next_sep
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
    DATA lv_upper TYPE string.
    DATA lv_keyword TYPE string.
    DATA lv_len TYPE i.
    DATA lv_key_len TYPE i.
        DATA temp46 TYPE xsdboolean.

    result = -1.
    
    lv_upper = to_upper( val ).
    
    lv_keyword = to_upper( keyword ).
    
    lv_len = strlen( lv_upper ).
    
    lv_key_len = strlen( lv_keyword ).
    IF lv_key_len = 0.
      RETURN.
    ENDIF.

    WHILE lv_pos + lv_key_len <= lv_len.
      IF substring( val = lv_upper
                    off = lv_pos
                    len = 1 ) = `'`.
        
        temp46 = boolc( lv_in_quote = abap_false ).
        lv_in_quote = temp46.
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
      DATA lv_len TYPE i.
        DATA lv_char TYPE string.
          DATA temp47 TYPE xsdboolean.
            DATA temp48 TYPE xsdboolean.

    result = c_trim( val ).

    DO.
      
      lv_len = strlen( result ).
      IF lv_len < 2 OR result(1) <> `(`.
        RETURN.
      ENDIF.

      " does the first parenthesis close at the last character?
      lv_pos = 0.
      lv_depth = 0.
      lv_in_quote = abap_false.
      lv_wrapped = abap_false.
      WHILE lv_pos < lv_len.
        
        lv_char = substring( val = result
                                   off = lv_pos
                                   len = 1 ).
        IF lv_char = `'`.
          
          temp47 = boolc( lv_in_quote = abap_false ).
          lv_in_quote = temp47.
        ELSEIF lv_in_quote = abap_false AND lv_char = `(`.
          lv_depth = lv_depth + 1.
        ELSEIF lv_in_quote = abap_false AND lv_char = `)`.
          lv_depth = lv_depth - 1.
          IF lv_depth = 0.
            
            temp48 = boolc( lv_pos = lv_len - 1 ).
            lv_wrapped = temp48.
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
    DATA lt_range     TYPE ty_t_range.

    DATA lt_conds TYPE string_table.
    DATA lv_cond LIKE LINE OF lt_conds.
        DATA ls_nested TYPE zabaputil_cl_util_context=>ty_s_filter_multi.
        DATA lv_in TYPE i.
          DATA lv_sign TYPE string.
          DATA lv_list TYPE string.
          DATA lv_items TYPE string.
          DATA temp654 TYPE string_table.
          DATA lv_item LIKE LINE OF temp654.
            DATA temp655 TYPE zabaputil_cl_util_context=>ty_s_range.
    lt_conds = filter_sql_split_top_level( val = val
                                                 sep = ` OR ` ).

    
    LOOP AT lt_conds INTO lv_cond.

      CLEAR lt_range.
      CLEAR lv_fieldname.
      lv_cond = filter_sql_strip_parens( lv_cond ).

      IF lines( filter_sql_split_top_level( val = lv_cond
                                            sep = ` OR ` ) ) > 1.
        " ( A = '1' OR A = '2' ) OR A = '3'
        
        ls_nested = filter_sql_get_or_group( lv_cond ).
        lv_fieldname = ls_nested-name.
        lt_range = ls_nested-t_range.

      ELSEIF lines( filter_sql_split_top_level( val = lv_cond
                                                sep = ` AND ` ) ) > 1.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |{ lv_cond }: conditions joined with AND inside an OR are no filter|.

      ELSE.
        " [NOT] IN ( list ): one EQ row per value, excluding for NOT IN
        
        lv_in = filter_sql_find( val     = lv_cond
                                       keyword = ` IN ` ).
        IF lv_in < 0.
          lv_in = filter_sql_find( val     = lv_cond
                                   keyword = ` IN(` ).
        ENDIF.
        IF lv_in >= 0.
          
          lv_sign = `I`.
          lv_fieldname = c_trim( substring( val = lv_cond
                                            len = lv_in ) ).
          IF to_upper( lv_fieldname ) CP `* NOT`.
            lv_sign = `E`.
            lv_fieldname = c_trim( substring( val = lv_fieldname
                                              len = strlen( lv_fieldname ) - 4 ) ).
          ENDIF.
          filter_sql_check_name( lv_fieldname ).
          
          lv_list = c_trim( substring( val = lv_cond
                                             off = lv_in + 3 ) ).
          
          lv_items = filter_sql_strip_parens( lv_list ).
          IF lv_items = lv_list.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |{ lv_cond }: IN without a list|.
          ENDIF.
          
          temp654 = filter_sql_split_top_level( val = lv_items sep = `,` ).
          
          LOOP AT temp654 INTO lv_item.
            
            CLEAR temp655.
            temp655-sign = lv_sign.
            temp655-option = `EQ`.
            temp655-low = filter_sql_get_literal( lv_item ).
            INSERT temp655 INTO TABLE lt_range.
          ENDLOOP.
        ELSE.
          filter_get_range_by_sql_cond( EXPORTING val       = lv_cond
                                        IMPORTING fieldname = lv_fieldname
                                                  range     = ls_range ).
          INSERT ls_range INTO TABLE lt_range.
        ENDIF.
      ENDIF.

      " one filter is one field: `A = '1' OR B = '2'` came back as A = '1'
      " OR A = '2'
      IF result-name IS INITIAL.
        result-name = lv_fieldname.
      ELSEIF to_upper( result-name ) <> to_upper( lv_fieldname ).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |{ val }: an OR across the fields { result-name } and { lv_fieldname } is no filter|.
      ENDIF.

      " the excluding rows of a filter are joined with AND, not with OR
      IF lines( lt_conds ) > 1.
        LOOP AT lt_range TRANSPORTING NO FIELDS WHERE sign = `E`.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |{ val }: NOT IN inside an OR is no filter|.
        ENDLOOP.
      ENDIF.

      INSERT LINES OF lt_range INTO TABLE result-t_range.

    ENDLOOP.

  ENDMETHOD.

  METHOD filter_sql_like_to_pattern.

    " % and _ are the LIKE wildcards; a *, + or # in a LIKE pattern is a
    " plain character, which CP writes with the escape # in front
    DATA lv_pos TYPE i.
    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
    lv_pos = 0.
    
    lv_len = strlen( val ).
    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = val
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

    DATA temp656 TYPE string.
    temp656 = val.
    result = |'{ replace( val  = temp656
                          sub  = `'`
                          with = `''`
                          occ  = 0 ) }'|.

  ENDMETHOD.

  METHOD csv_read_quoted.

    " the text up to the next quote in one piece, not one concatenation
    " per character
    DATA lv_len TYPE i.
      DATA lv_quote TYPE i.
    lv_len = strlen( val ).
    pos = pos + 1.

    WHILE pos < lv_len.
      
      lv_quote = find( val = val
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
        DATA temp657 TYPE xstring.
        DATA lv_bom TYPE string.

    " the UTF-8 decoding keeps the mark EF BB BF as the character U+FEFF in
    " front of the first column name. Built at run time from its bytes - a
    " literal U+FEFF in the source breaks the 7-bit rule and the downport.
    " Where the decoding drops it, it is empty
    result = val.
    TRY.
        
        temp657 = `EFBBBF`.
        
        lv_bom = conv_get_string_by_xstring( temp657 ).
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

    DATA lv_name TYPE string.
    DATA lv_len TYPE i.
      DATA lv_char TYPE string.
    lv_name = c_trim_upper( val ).
    
    lv_len = strlen( lv_name ).

    WHILE lv_pos < lv_len.
      
      lv_char = substring( val = lv_name
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
    DATA temp658 TYPE i.
    lv_name = to_upper( name ).
    
    CASE lv_name.
      WHEN `UNSPECIFIC`.
        temp658 = 0.
      WHEN `UNAUTHORIZED`.
        temp658 = 401.
      WHEN `NOT_FOUND`.
        temp658 = 404.
      WHEN `CONFLICT`.
        temp658 = 409.
      WHEN `LOCKED`.
        temp658 = 423.
      WHEN `DEPENDENCY`.
        temp658 = 424.
      WHEN `NOT_IMPLEMENTED`.
        temp658 = 501.
      WHEN `DISABLED`.
        temp658 = 4221.
      WHEN `READONLY`.
        temp658 = 4222.
      WHEN OTHERS.
        temp658 = -1.
    ENDCASE.
    result = temp658.

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

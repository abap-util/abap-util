CLASS zabaputil_cl_pdf_source DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " The class of a form as source code: read, and written again.
    "
    " A form is a class that implements zabaputil_if_pdf_data - its public
    " attributes are the data, its method get( ) returns the form as text.
    " Where the class exists, a program asks the class. Where it does not -
    " another system, a runtime without one - the source is all there is,
    " and somebody pastes it: this class finds in it the name of the class,
    " the fields a form can name (from the declarations of its public
    " section) and the text of the form (from the literals of get( )), and
    " puts a changed form back into the source - what a form editor needs.
    "
    " It reads declarations, not ABAP: DATA and TYPES with TYPE, BEGIN OF /
    " END OF and TABLE OF, in chains or one by one. A type the source does
    " not declare is looked up in the dictionary of the system - a data
    " element, a structure, a table type - through the context class; where
    " there is none (a runtime without a dictionary) it is a value.

    TYPES:
      BEGIN OF ty_s_message,
        "! Error, Warning or Information - the types of a MessageStrip
        type TYPE string,
        text TYPE string,
      END OF ty_s_message.
    TYPES ty_t_message TYPE STANDARD TABLE OF ty_s_message WITH EMPTY KEY.

    "! What keeps a definition from being the model of a form: no class,
    "! no public section, no interface of a form, a structure that is not
    "! closed, an attribute declared twice - and, as information, a type
    "! that is neither declared in the class nor known to the system, which
    "! is printed as a single value. Not a syntax check: what the reader of
    "! declarations does not see, the activation of the class finds
    CLASS-METHODS check
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_message.

    "! The name of the class a source defines, in upper case; empty when the
    "! text is no class
    CLASS-METHODS class_name
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The definition part of a class: CLASS ... DEFINITION up to its
    "! ENDCLASS. - the part somebody maintains as the model of a form
    CLASS-METHODS definition
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! What follows the definition part: the implementation, if the source
    "! has one
    CLASS-METHODS implementation
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The public section names an interface of a form (...IF_PDF_DATA)
    CLASS-METHODS has_interface
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Data to print a preview with, for a class nobody can create: a
    "! structure with the public attributes the source declares, filled with
    "! sample values. A type the source does not declare is a text. Unbound
    "! when the source declares nothing a structure can be made of
    CLASS-METHODS sample_data
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! How a program prints the form: an instance, a value for every
    "! attribute the source declares, one call
    "! @parameter val        | the source of the class
    "! @parameter form_class | the class that prints (zabaputil_cl_pdf_form)
    "! @parameter classic    | statements a release before 7.40 compiles:
    "!                         CREATE OBJECT, a work area per table row, no
    "!                         inline declaration
    CLASS-METHODS call_example
      IMPORTING
        val           TYPE string
        form_class    TYPE string
        classic       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string.

    "! What a form can name: the public instance attributes of the class,
    "! as zabaputil_cl_pdf_form=>fields( ) lists them for an instance
    CLASS-METHODS fields
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE zabaputil_cl_pdf_form=>ty_t_field.

    "! The text of the form in what somebody copied: the literals of the
    "! method get( ) of a class or of that method alone, or the text itself
    CLASS-METHODS form_text
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The method get( ) that returns a text
    "! @parameter text      | the text of a form (zabaputil_cl_pdf_form=>stringify)
    "! @parameter interface | the interface the method belongs to
    CLASS-METHODS method_get
      IMPORTING
        text          TYPE string
        interface     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The source of a class with another form: its method get( ) returns
    "! the text - replaced where the class has it, added where it does not
    "! @parameter val       | the source of the class
    "! @parameter text      | the text of the form
    "! @parameter interface | the interface, for a class that does not name it
    CLASS-METHODS with_form
      IMPORTING
        val           TYPE string
        text          TYPE string
        interface     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The source of a class for a form that has none: an attribute for every
    "! field the form names - a text for a placeholder, a table with a text
    "! per column for a table element, bytes for an image - and a get( )
    "! that returns the text
    "! @parameter name      | the name of the class
    "! @parameter interface | the interface of a form
    "! @parameter form      | the form
    "! @parameter text      | the text of the form
    CLASS-METHODS class_of_form
      IMPORTING
        name          TYPE string
        interface     TYPE string
        form          TYPE zabaputil_cl_pdf_form=>ty_s_form
        text          TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    " the characters of one literal, and the literals of one statement:
    " lines that fit a screen, statements a compiler takes
    CONSTANTS c_width TYPE i VALUE 100.
    CONSTANTS c_lines TYPE i VALUE 100.

    CONSTANTS c_name_characters TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_/`.
    "! the types of the language, and those every system has: not looked up
    CONSTANTS c_builtin TYPE string VALUE ` C N STRING I INT1 INT2 INT8 P F D T X XSTRING DECFLOAT16 DECFLOAT34 UTCLONG ABAP_BOOL `.

    " what the declarations of a class say, as far as a form needs it
    CONSTANTS:
      BEGIN OF cs_kind,
        structure TYPE string VALUE `S`,
        table     TYPE string VALUE `T`,
        alias     TYPE string VALUE `A`,
      END OF cs_kind.

    TYPES:
      BEGIN OF ty_s_type,
        name TYPE string,
        kind TYPE string,
        "! of a table the type of its rows, of an alias the type it names
        ref  TYPE string,
      END OF ty_s_type.
    TYPES ty_t_type TYPE STANDARD TABLE OF ty_s_type WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_component,
        owner TYPE string,
        name  TYPE string,
        type  TYPE string,
      END OF ty_s_component.
    TYPES ty_t_component TYPE STANDARD TABLE OF ty_s_component WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_declared,
        t_type      TYPE ty_t_type,
        t_component TYPE ty_t_component,
        "! the attributes, with the class as their owner
        t_attribute TYPE ty_t_component,
      END OF ty_s_declared.

    " where the method get( ) is in a source
    TYPES:
      BEGIN OF ty_s_method,
        found TYPE abap_bool,
        "! behind the period of METHOD ...~get.
        body  TYPE i,
        "! at ENDMETHOD
        end   TYPE i,
      END OF ty_s_method.

    "! The public section of the class a source defines, in upper case
    CLASS-METHODS public_section
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! Source as statements: without comments, a chain as the statements it
    "! stands for, one blank between two words
    CLASS-METHODS statements
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS declared
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_declared.

    "! The type a declaration names behind TYPE; a table type of its own is
    "! added to the types. No functional method: one with a CHANGING
    "! parameter does not compile before release 7.40
    CLASS-METHODS type_of
      IMPORTING
        tokens   TYPE string_table
      EXPORTING
        result   TYPE string
      CHANGING
        declared TYPE ty_s_declared.

    "! A declared type as nodes of the context class, for sample_data( ): a
    "! structure, a table, a type of the dictionary by its name, or - for
    "! everything else the source does not declare - a field
    "! @parameter parent | the node it belongs to
    "! @parameter name   | its name there
    CLASS-METHODS nodes_of
      IMPORTING
        declared TYPE ty_s_declared
        type     TYPE string
        depth    TYPE i
        parent   TYPE i
        name     TYPE string
      CHANGING
        nodes    TYPE zabaputil_cl_util_context=>ty_t_type_node.

    "! The statements that give a declared type a value, for call_example( )
    "! @parameter target  | what is assigned to: lo_form->address-city
    "! @parameter classic | a row through a work area, as 7.02 takes it
    CLASS-METHODS call_lines
      IMPORTING
        declared      TYPE ty_s_declared
        type          TYPE string
        target        TYPE string
        depth         TYPE i
        classic       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string.

    "! A type of the dictionary of the system by its name - a data element,
    "! a structure, a table type - as nodes of the context class. Empty for
    "! a type of the language, and where the system has no such type - a
    "! runtime without a system has none
    CLASS-METHODS ddic
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_t_type_node.

    "! The fields of a node of a type of the dictionary, as fields_of( )
    "! lists the declared ones
    CLASS-METHODS fields_of_ddic
      IMPORTING
        nodes  TYPE zabaputil_cl_util_context=>ty_t_type_node
        id     TYPE i
        path   TYPE string
        depth  TYPE i
      CHANGING
        result TYPE zabaputil_cl_pdf_form=>ty_t_field.

    "! The length of a numeric text as type_of( ) names it - N18 -, 0 for any
    "! other type
    CLASS-METHODS numeric_length
      IMPORTING
        type          TYPE string
      RETURNING
        VALUE(result) TYPE i.

    "! A literal for a field of a type of the language: `...`, 1, sy-datum
    CLASS-METHODS call_literal
      IMPORTING
        type          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! An attribute for a field a form names, with the structures on its path
    CLASS-METHODS declare_path
      IMPORTING
        path     TYPE string
        type     TYPE string
      CHANGING
        declared TYPE ty_s_declared.

    "! A name a declaration can have
    CLASS-METHODS is_name
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! The TYPES statements of a type and of the types it is made of, the
    "! inner ones first; a type is written once
    CLASS-METHODS types_source
      IMPORTING
        declared TYPE ty_s_declared
        type     TYPE string
      CHANGING
        done     TYPE string_table
        result   TYPE string.

    "! A class of declarations and a form
    CLASS-METHODS class_source
      IMPORTING
        name          TYPE string
        interface     TYPE string
        declared      TYPE ty_s_declared
        text          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS fields_of
      IMPORTING
        declared TYPE ty_s_declared
        type     TYPE string
        path     TYPE string
        depth    TYPE i
      CHANGING
        result   TYPE zabaputil_cl_pdf_form=>ty_t_field.

    "! A type as the source declares it: an alias followed to what it names.
    "! Empty for a type the source does not declare
    CLASS-METHODS resolve
      IMPORTING
        declared      TYPE ty_s_declared
        type          TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_type.

    CLASS-METHODS method_find
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE ty_s_method.

    "! The statements that return a text: result = `...` && `...`.
    CLASS-METHODS literals
      IMPORTING
        text          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The text the literals of a source make
    CLASS-METHODS literal_text
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_source IMPLEMENTATION.

  METHOD class_name.

    DATA lv_from TYPE i.

    " CLASS name DEFINITION - the word in front of the first DEFINITION that
    " has CLASS in front of it
    DATA(lv_upper) = to_upper( val ).
    DO 20 TIMES.
      DATA(lv_at) = find( val = lv_upper
                          sub = ` DEFINITION`
                          off = lv_from ).
      IF lv_at < 0.
        RETURN.
      ENDIF.
      lv_from = lv_at + 1.

      DATA(lv_before) = substring( val = lv_upper
                                   len = lv_at ).
      DATA(lv_class) = find( val = lv_before
                             sub = `CLASS `
                             occ = -1 ).
      IF lv_class < 0.
        CONTINUE.
      ENDIF.
      DATA(lv_name) = condense( substring( val = lv_before
                                           off = lv_class + 6 ) ).
      IF lv_name IS NOT INITIAL AND lv_name CO c_name_characters.
        result = lv_name.
        RETURN.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD public_section.

    DATA(lv_name) = class_name( val ).
    IF lv_name IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_upper) = to_upper( val ).
    DATA(lv_start) = find( val = lv_upper
                           sub = `PUBLIC SECTION` ).
    IF lv_start < 0.
      RETURN.
    ENDIF.
    lv_start = lv_start + 14.

    " it ends where the next section starts, or the definition
    DATA(lv_end) = strlen( lv_upper ).
    DATA(lt_end) = VALUE string_table( ( `PROTECTED SECTION` ) ( `PRIVATE SECTION` ) ( `ENDCLASS` ) ).
    LOOP AT lt_end INTO DATA(lv_word).
      DATA(lv_at) = find( val = lv_upper
                          sub = lv_word
                          off = lv_start ).
      IF lv_at >= 0 AND lv_at < lv_end.
        lv_end = lv_at.
      ENDIF.
    ENDLOOP.

    result = substring( val = lv_upper
                        off = lv_start
                        len = lv_end - lv_start ).

  ENDMETHOD.

  METHOD statements.

    DATA lv_statement TYPE string.
    DATA lv_quote     TYPE string.
    DATA lv_character TYPE string.
    DATA lt_raw       TYPE string_table.

    DATA(lv_line_feed) = |\n|.
    SPLIT val AT lv_line_feed INTO TABLE DATA(lt_line).
    LOOP AT lt_line INTO DATA(lv_line).
      " a line that starts with * is a comment
      IF lv_line IS INITIAL OR substring( val = lv_line
                                          len = 1 ) = `*`.
        CONTINUE.
      ENDIF.

      " character by character: a period ends a statement and a quotation
      " mark starts a comment - unless they are inside a literal
      DATA(lv_length) = strlen( lv_line ).
      DO lv_length TIMES.
        lv_character = substring( val = lv_line
                                  off = sy-index - 1
                                  len = 1 ).
        IF lv_quote IS NOT INITIAL.
          IF lv_character = lv_quote.
            CLEAR lv_quote.
          ENDIF.
        ELSEIF lv_character = `'` OR lv_character = '`' OR lv_character = `|`.
          lv_quote = lv_character.
        ELSEIF lv_character = `"`.
          EXIT.
        ELSEIF lv_character = `.`.
          INSERT lv_statement INTO TABLE lt_raw.
          CLEAR lv_statement.
          CONTINUE.
        ENDIF.
        lv_statement = lv_statement && lv_character.
      ENDDO.
      " a literal ends with its line, and a line break is a blank
      CLEAR lv_quote.
      lv_statement = lv_statement && ` `.
    ENDLOOP.

    LOOP AT lt_raw INTO lv_statement.
      lv_statement = condense( translate( val  = lv_statement
                                          from = |\r\t|
                                          to   = `  ` ) ).
      IF lv_statement IS INITIAL.
        CONTINUE.
      ENDIF.

      " DATA: a TYPE i, b TYPE string - a statement per part
      DATA(lv_colon) = find( val = lv_statement
                             sub = `:` ).
      IF lv_colon > 0.
        DATA(lv_keyword) = condense( substring( val = lv_statement
                                                len = lv_colon ) ).
        IF lv_keyword CO `ABCDEFGHIJKLMNOPQRSTUVWXYZ-`.
          DATA(lv_parts) = substring( val = lv_statement
                                      off = lv_colon + 1 ).
          SPLIT lv_parts AT `,` INTO TABLE DATA(lt_part).
          LOOP AT lt_part INTO DATA(lv_part).
            lv_part = condense( lv_part ).
            IF lv_part IS NOT INITIAL.
              INSERT |{ lv_keyword } { lv_part }| INTO TABLE result.
            ENDIF.
          ENDLOOP.
          CONTINUE.
        ENDIF.
      ENDIF.
      INSERT lv_statement INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD declared.

    DATA lt_open TYPE string_table.
    DATA lv_name TYPE string.
    DATA lv_type TYPE string.

    LOOP AT statements( public_section( val ) ) INTO DATA(lv_statement).
      SPLIT lv_statement AT ` ` INTO TABLE DATA(lt_token).
      DATA(lv_count) = lines( lt_token ).
      IF lv_count < 2.
        CONTINUE.
      ENDIF.
      DATA(lv_keyword) = lt_token[ 1 ].
      IF lv_keyword <> `DATA` AND lv_keyword <> `TYPES`.
        " constants and static attributes are not what a form prints
        CONTINUE.
      ENDIF.
      DATA(lv_depth) = lines( lt_open ).

      IF lv_count >= 4 AND lt_token[ 2 ] = `BEGIN` AND lt_token[ 3 ] = `OF`.
        " a structure: a type by its name, or - of an attribute, or inside
        " another structure - one that has no name of its own
        lv_name = lt_token[ 4 ].
        IF lv_keyword = `TYPES` AND lv_depth = 0.
          lv_type = lv_name.
        ELSEIF lv_depth = 0.
          lv_type = |%{ lv_name }|.
          INSERT VALUE #( name = lv_name
                          type = lv_type ) INTO TABLE result-t_attribute.
        ELSE.
          lv_type = |{ lt_open[ lv_depth ] }-{ lv_name }|.
          INSERT VALUE #( owner = lt_open[ lv_depth ]
                          name  = lv_name
                          type  = lv_type ) INTO TABLE result-t_component.
        ENDIF.
        INSERT VALUE #( name = lv_type
                        kind = cs_kind-structure ) INTO TABLE result-t_type.
        INSERT lv_type INTO TABLE lt_open.
        CONTINUE.
      ENDIF.

      IF lv_count >= 3 AND lt_token[ 2 ] = `END` AND lt_token[ 3 ] = `OF`.
        IF lv_depth > 0.
          DELETE lt_open INDEX lv_depth.
        ENDIF.
        CONTINUE.
      ENDIF.

      " name TYPE ... - with a length in parentheses the name ends there
      lv_name = lt_token[ 2 ].
      DATA(lv_parenthesis) = find( val = lv_name
                                   sub = `(` ).
      IF lv_parenthesis > 0.
        lv_name = substring( val = lv_name
                             len = lv_parenthesis ).
      ENDIF.
      type_of( EXPORTING tokens   = lt_token
               IMPORTING result   = lv_type
               CHANGING  declared = result ).

      IF lv_depth > 0.
        INSERT VALUE #( owner = lt_open[ lv_depth ]
                        name  = lv_name
                        type  = lv_type ) INTO TABLE result-t_component.
      ELSEIF lv_keyword = `TYPES`.
        INSERT VALUE #( name = lv_name
                        kind = cs_kind-alias
                        ref  = lv_type ) INTO TABLE result-t_type.
      ELSE.
        INSERT VALUE #( name = lv_name
                        type = lv_type ) INTO TABLE result-t_attribute.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD type_of.

    DATA lv_index TYPE i.

    " the word behind TYPE or LIKE; without one the field is a character
    CLEAR result.
    DATA(lv_count) = lines( tokens ).
    LOOP AT tokens INTO DATA(lv_token).
      IF sy-tabix > 2 AND ( lv_token = `TYPE` OR lv_token = `LIKE` ).
        lv_index = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_index = 0 OR lv_index >= lv_count.
      RETURN.
    ENDIF.

    " a reference is nothing a form can name
    IF tokens[ lv_index + 1 ] = `REF`.
      result = `%REF`.
      RETURN.
    ENDIF.

    " ... TABLE OF rows: a table type that has no name
    DATA(lv_first) = lv_index + 1.
    LOOP AT tokens INTO lv_token FROM lv_first.
      DATA(lv_at) = sy-tabix.
      IF lv_token = `TABLE` AND lv_at + 2 <= lv_count AND tokens[ lv_at + 1 ] = `OF`.
        DATA(lv_row) = tokens[ lv_at + 2 ].
        IF lv_row = `REF`.
          CLEAR lv_row.
        ENDIF.
        result = |%TABLE{ lines( declared-t_type ) + 1 }|.
        INSERT VALUE #( name = result
                        kind = cs_kind-table
                        ref  = substring_after( val = |=>{ lv_row }|
                                                sub = `=>`
                                                occ = -1 ) ) INTO TABLE declared-t_type.
        RETURN.
      ENDIF.
      " the row type is named within the next words, or it is no table
      IF lv_at > lv_index + 3.
        EXIT.
      ENDIF.
    ENDLOOP.

    " a type of the class itself may be written with the class in front
    result = substring_after( val = |=>{ tokens[ lv_index + 1 ] }|
                              sub = `=>`
                              occ = -1 ).

    " a numeric text has a length that matters - 18 digits of an SSCC, 14 of
    " a GTIN: N18, N14
    IF result = `N` AND lv_index + 3 <= lv_count AND tokens[ lv_index + 2 ] = `LENGTH`
        AND tokens[ lv_index + 3 ] CO `0123456789`.
      result = |N{ tokens[ lv_index + 3 ] }|.
    ENDIF.

  ENDMETHOD.

  METHOD resolve.

    DATA(lv_type) = type.
    DO 10 TIMES.
      CLEAR result.
      LOOP AT declared-t_type INTO DATA(ls_type) WHERE name = lv_type.
        result = ls_type.
        EXIT.
      ENDLOOP.
      IF result-kind <> cs_kind-alias.
        RETURN.
      ENDIF.
      lv_type = result-ref.
    ENDDO.
    CLEAR result.

  ENDMETHOD.

  METHOD fields.

    DATA(ls_declared) = declared( val ).
    LOOP AT ls_declared-t_attribute INTO DATA(ls_attribute).
      fields_of( EXPORTING declared = ls_declared
                           type     = ls_attribute-type
                           path     = ls_attribute-name
                           depth    = 0
                 CHANGING  result   = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD fields_of.

    IF type = `%REF`.
      RETURN.
    ENDIF.

    DATA(ls_type) = resolve( declared = declared
                             type     = type ).
    CASE ls_type-kind.

      WHEN cs_kind-structure.
        " three levels are what a form can reasonably name
        IF depth >= 3.
          RETURN.
        ENDIF.
        LOOP AT declared-t_component INTO DATA(ls_component) WHERE owner = ls_type-name.
          fields_of( EXPORTING declared = declared
                               type     = ls_component-type
                               path     = |{ path }-{ ls_component-name }|
                               depth    = depth + 1
                     CHANGING  result   = result ).
        ENDLOOP.

      WHEN cs_kind-table.
        INSERT VALUE #( path = path
                        kind = zabaputil_cl_pdf_form=>cs_field_kind-table ) INTO TABLE result.
        DATA(ls_row) = resolve( declared = declared
                                type     = ls_type-ref ).
        IF ls_row-kind = cs_kind-structure.
          LOOP AT declared-t_component INTO ls_component WHERE owner = ls_row-name.
            INSERT VALUE #( path   = ls_component-name
                            kind   = zabaputil_cl_pdf_form=>cs_field_kind-column
                            parent = path ) INTO TABLE result.
          ENDLOOP.
        ENDIF.

      WHEN OTHERS.
        " a type of the dictionary has its fields there
        DATA(lt_ddic) = ddic( type ).
        IF lt_ddic IS NOT INITIAL.
          fields_of_ddic( EXPORTING nodes  = lt_ddic
                                    id     = 1
                                    path   = path
                                    depth  = depth
                          CHANGING  result = result ).
          RETURN.
        ENDIF.
        INSERT VALUE #( path = path
                        kind = zabaputil_cl_pdf_form=>cs_field_kind-value ) INTO TABLE result.

    ENDCASE.

  ENDMETHOD.

  METHOD fields_of_ddic.

    DATA ls_node  TYPE zabaputil_cl_util_context=>ty_s_type_node.
    DATA ls_child TYPE zabaputil_cl_util_context=>ty_s_type_node.
    DATA ls_field TYPE zabaputil_cl_util_context=>ty_s_type_node.

    IF NOT line_exists( nodes[ id = id ] ).
      RETURN.
    ENDIF.
    ls_node = nodes[ id = id ].

    CASE ls_node-kind.

      WHEN `S`.
        IF depth >= 3.
          RETURN.
        ENDIF.
        " the components of an include are the context's components of the
        " structure already
        LOOP AT nodes INTO ls_child WHERE parent = id.
          fields_of_ddic( EXPORTING nodes  = nodes
                                    id     = ls_child-id
                                    path   = |{ path }-{ ls_child-name }|
                                    depth  = depth + 1
                          CHANGING  result = result ).
        ENDLOOP.

      WHEN `T`.
        INSERT VALUE #( path = path
                        kind = zabaputil_cl_pdf_form=>cs_field_kind-table ) INTO TABLE result.
        LOOP AT nodes INTO ls_child WHERE parent = id AND kind = `S`.
          LOOP AT nodes INTO ls_field WHERE parent = ls_child-id.
            INSERT VALUE #( path   = ls_field-name
                            kind   = zabaputil_cl_pdf_form=>cs_field_kind-column
                            parent = path ) INTO TABLE result.
          ENDLOOP.
        ENDLOOP.

      WHEN `E`.
        INSERT VALUE #( path = path
                        kind = zabaputil_cl_pdf_form=>cs_field_kind-value ) INTO TABLE result.

    ENDCASE.

  ENDMETHOD.
  METHOD ddic.

    IF is_name( name ) = abap_false OR c_builtin CS | { name } | OR numeric_length( name ) > 0.
      RETURN.
    ENDIF.
    result = zabaputil_cl_util_context=>rtti_get_t_type_node_by_name( name ).

  ENDMETHOD.
  METHOD numeric_length.

    IF strlen( type ) >= 2 AND type(1) = `N`.
      DATA(lv_digits) = substring( val = type
                                   off = 1 ).
      IF lv_digits CO `0123456789` AND strlen( lv_digits ) <= 3.
        result = lv_digits.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check.

    DATA lt_seen  TYPE string_table.
    DATA lt_typed TYPE string_table.
    DATA lv_open  TYPE i.

    IF class_name( val ) IS INITIAL.
      INSERT VALUE #( type = `Error`
                      text = `There is no CLASS ... DEFINITION: without a class a form has no fields.` ) INTO TABLE result.
      RETURN.
    ENDIF.

    DATA(lt_all) = statements( to_upper( val ) ).
    IF NOT line_exists( lt_all[ table_line = `ENDCLASS` ] ).
      INSERT VALUE #( type = `Error`
                      text = `ENDCLASS. is missing at the end of the definition.` ) INTO TABLE result.
    ENDIF.
    IF NOT line_exists( lt_all[ table_line = `PUBLIC SECTION` ] ).
      INSERT VALUE #( type = `Error`
                      text = `There is no PUBLIC SECTION: what a form prints are the public attributes.` ) INTO TABLE result.
      RETURN.
    ENDIF.
    IF has_interface( val ) = abap_false.
      INSERT VALUE #( type = `Warning`
                      text = `The public section names no interface of a form (INTERFACES ...if_pdf_data.): the class is no form.` )
             INTO TABLE result.
    ENDIF.

    LOOP AT statements( public_section( val ) ) INTO DATA(lv_statement).
      IF lv_statement CP `* BEGIN OF *`.
        lv_open = lv_open + 1.
      ELSEIF lv_statement CP `* END OF *`.
        lv_open = lv_open - 1.
      ENDIF.
    ENDLOOP.
    IF lv_open <> 0.
      INSERT VALUE #( type = `Error`
                      text = `A BEGIN OF has no END OF, or an END OF no BEGIN OF.` ) INTO TABLE result.
    ENDIF.

    DATA(ls_declared) = declared( val ).
    IF ls_declared-t_attribute IS INITIAL.
      INSERT VALUE #( type = `Warning`
                      text = `The public section declares no attribute: there is nothing a form could print.` ) INTO TABLE result.
    ENDIF.
    LOOP AT ls_declared-t_attribute INTO DATA(ls_attribute).
      IF line_exists( lt_seen[ table_line = ls_attribute-name ] ).
        INSERT VALUE #( type = `Error`
                        text = |{ ls_attribute-name } is declared twice.| ) INTO TABLE result.
      ENDIF.
      INSERT ls_attribute-name INTO TABLE lt_seen.
    ENDLOOP.

    " a type that is neither the class's nor the system's is a value
    DATA(lt_typed_by) = ls_declared-t_attribute.
    INSERT LINES OF ls_declared-t_component INTO TABLE lt_typed_by.
    LOOP AT lt_typed_by INTO DATA(ls_typed).
      DATA(lv_type) = ls_typed-type.
      IF lv_type IS INITIAL OR lv_type(1) = `%` OR c_builtin CS | { lv_type } | OR numeric_length( lv_type ) > 0
          OR line_exists( lt_typed[ table_line = lv_type ] ).
        CONTINUE.
      ENDIF.
      INSERT lv_type INTO TABLE lt_typed.
      IF resolve( declared = ls_declared
                  type     = lv_type ) IS INITIAL AND ddic( lv_type ) IS INITIAL.
        INSERT VALUE #( type = `Information`
                        text = |{ lv_type } is no type of the class, and the system does not know it: { ls_typed-name } prints as one value.| )
               INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD method_find.

    " METHOD interface~get. ... ENDMETHOD - the ~GET. that has METHOD and
    " nothing but a name in front of it
    DATA lv_from TYPE i.

    DATA(lv_upper) = to_upper( val ).
    DO 20 TIMES.
      DATA(lv_at) = find( val = lv_upper
                          sub = `~GET.`
                          off = lv_from ).
      IF lv_at < 0.
        RETURN.
      ENDIF.
      lv_from = lv_at + 1.

      DATA(lv_before) = substring( val = lv_upper
                                   len = lv_at ).
      DATA(lv_method) = find( val = lv_before
                              sub = `METHOD `
                              occ = -1 ).
      IF lv_method < 0.
        CONTINUE.
      ENDIF.
      DATA(lv_name) = condense( substring( val = lv_before
                                           off = lv_method + 7 ) ).
      IF lv_name IS INITIAL OR lv_name CN c_name_characters.
        CONTINUE.
      ENDIF.

      DATA(lv_end) = find( val = lv_upper
                           sub = `ENDMETHOD`
                           off = lv_at ).
      IF lv_end < 0.
        RETURN.
      ENDIF.
      result-found = abap_true.
      result-body = lv_at + 5.
      result-end = lv_end.
      RETURN.
    ENDDO.

  ENDMETHOD.

  METHOD form_text.

    DATA(lv_quote) = '`'.
    DATA(ls_method) = method_find( val ).
    IF ls_method-found = abap_true.
      " what the method returns - a method without a literal returns nothing
      DATA(lv_body) = substring( val = val
                                 off = ls_method-body
                                 len = ls_method-end - ls_method-body ).
      IF lv_body CS lv_quote.
        result = literal_text( lv_body ).
      ENDIF.
      RETURN.
    ENDIF.

    " a class that has no form yet
    IF class_name( val ) IS NOT INITIAL.
      RETURN.
    ENDIF.

    " no method around it: literals, or the text itself
    IF val CS lv_quote.
      result = literal_text( val ).
    ELSE.
      result = val.
    ENDIF.

  ENDMETHOD.

  METHOD literal_text.

    DATA lv_index  TYPE i.
    DATA lv_source TYPE string.

    " a comment line may hold anything - also a backquote
    DATA(lv_line_feed) = |\n|.
    SPLIT val AT lv_line_feed INTO TABLE DATA(lt_line).
    LOOP AT lt_line INTO DATA(lv_line).
      DATA(lv_trimmed) = condense( lv_line ).
      IF lv_trimmed IS NOT INITIAL AND ( substring( val = lv_trimmed
                                                    len = 1 ) = `"` OR substring( val = lv_line
                                                                                  len = 1 ) = `*` ).
        CONTINUE.
      ENDIF.
      lv_source = lv_source && lv_line && lv_line_feed.
    ENDLOOP.

    " Cut at the backquotes, every second piece is inside a literal. A
    " backquote a literal holds is written twice, which leaves an empty
    " piece between two pieces of the same literal - anything else between
    " two literals is at least the && that joins them
    DATA(lv_quote) = '`'.
    SPLIT lv_source AT lv_quote INTO TABLE DATA(lt_piece).
    DATA(lv_count) = lines( lt_piece ).
    LOOP AT lt_piece INTO DATA(lv_piece).
      lv_index = sy-tabix.
      IF lv_index MOD 2 = 0.
        result = result && lv_piece.
      ELSEIF lv_piece IS INITIAL AND lv_index > 1 AND lv_index < lv_count.
        result = result && lv_quote.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD literals.

    DATA lt_literal TYPE string_table.
    DATA lv_offset  TYPE i.
    DATA lv_length  TYPE i.
    DATA lv_line    TYPE i.

    " the text in literals a line of source holds - a backquote in it
    " doubled, which is how a literal writes one
    DATA(lv_quote) = '`'.
    DATA(lv_total) = strlen( text ).
    WHILE lv_offset < lv_total.
      lv_length = lv_total - lv_offset.
      IF lv_length > c_width.
        lv_length = c_width.
      ENDIF.
      DATA(lv_piece) = substring( val = text
                                  off = lv_offset
                                  len = lv_length ).
      lv_piece = replace( val  = lv_piece
                          sub  = lv_quote
                          with = lv_quote && lv_quote
                          occ  = 0 ).
      INSERT lv_quote && lv_piece && lv_quote INTO TABLE lt_literal.
      lv_offset = lv_offset + lv_length.
    ENDWHILE.
    IF lt_literal IS INITIAL.
      INSERT lv_quote && lv_quote INTO TABLE lt_literal.
    ENDIF.

    " one statement per c_lines literals: a form with an image is thousands
    " of them, more than one expression should chain
    DATA(lv_count) = lines( lt_literal ).
    LOOP AT lt_literal INTO DATA(lv_literal).
      lv_line = sy-tabix.
      DATA(lv_in_statement) = ( lv_line - 1 ) MOD c_lines.
      IF lv_line = 1.
        result = |{ result }    result = { lv_literal }|.
      ELSEIF lv_in_statement = 0.
        result = |{ result }    result = result &&\n             { lv_literal }|.
      ELSE.
        result = |{ result }             { lv_literal }|.
      ENDIF.
      IF lv_line = lv_count OR lv_in_statement = c_lines - 1.
        result = |{ result }.\n|.
      ELSE.
        result = |{ result } &&\n|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD method_get.

    result = |  METHOD { to_lower( interface ) }~get.\n\n{ literals( text ) }\n  ENDMETHOD.\n|.

  ENDMETHOD.

  METHOD with_form.

    " the class has the method: what is between its first line and its
    " ENDMETHOD is the form, everything else stays as it was written
    DATA(ls_method) = method_find( val ).
    IF ls_method-found = abap_true.
      DATA(lv_head) = substring( val = val
                                 len = ls_method-body ).
      DATA(lv_tail) = substring( val = val
                                 off = ls_method-end ).
      result = |{ lv_head }\n\n{ literals( text ) }\n  { lv_tail }|.
      RETURN.
    ENDIF.

    " it has none: the interface the class names, if it names one of a form
    DATA(lv_interface) = interface.
    LOOP AT statements( public_section( val ) ) INTO DATA(lv_statement).
      IF lv_statement CP `INTERFACES *IF_PDF_DATA`.
        lv_interface = substring_after( val = lv_statement
                                        sub = ` ` ).
        EXIT.
      ENDIF.
    ENDLOOP.
    DATA(lv_method) = method_get( text      = text
                                  interface = lv_interface ).

    " in front of the ENDCLASS of the implementation - or with one, where
    " the source is a definition alone
    DATA(lv_upper) = to_upper( val ).
    DATA(lv_implementation) = find( val = lv_upper
                                    sub = ` IMPLEMENTATION` ).
    DATA(lv_end) = -1.
    IF lv_implementation >= 0.
      lv_end = find( val = lv_upper
                     sub = `ENDCLASS`
                     off = lv_implementation ).
    ENDIF.
    IF lv_end >= 0.
      DATA(lv_before) = substring( val = val
                                   len = lv_end ).
      DATA(lv_behind) = substring( val = val
                                   off = lv_end ).
      result = |{ lv_before }{ lv_method }\n{ lv_behind }|.
    ELSE.
      DATA(lv_name) = to_lower( class_name( val ) ).
      result = |{ val }\n\nCLASS { lv_name } IMPLEMENTATION.\n\n{ lv_method }\nENDCLASS.\n|.
    ENDIF.

  ENDMETHOD.

  METHOD is_name.

    result = xsdbool( val IS NOT INITIAL AND val CO c_name_characters AND val(1) CN `0123456789` ).

  ENDMETHOD.

  METHOD declare_path.

    DATA lv_owner TYPE string.
    DATA lv_walk  TYPE string.

    " A-B-C: the attribute A is a structure with a structure B with a field C
    SPLIT to_upper( path ) AT `-` INTO TABLE DATA(lt_name).
    DATA(lv_count) = lines( lt_name ).
    LOOP AT lt_name INTO DATA(lv_name).
      IF is_name( lv_name ) = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_name INTO lv_name.
      DATA(lv_index) = sy-tabix.
      IF lv_walk IS INITIAL.
        lv_walk = lv_name.
      ELSE.
        lv_walk = |{ lv_walk }-{ lv_name }|.
      ENDIF.

      " the last name is the field, the ones in front of it are structures
      DATA(lv_type) = type.
      IF lv_index < lv_count.
        lv_type = to_upper( |TY_S_{ translate( val  = lv_walk
                                               from = `-`
                                               to   = `_` ) }| ).
        IF NOT line_exists( declared-t_type[ name = lv_type ] ).
          INSERT VALUE #( name = lv_type
                          kind = cs_kind-structure ) INTO TABLE declared-t_type.
        ENDIF.
      ENDIF.

      IF lv_index = 1.
        " named as a field and as a structure: it is the structure
        ASSIGN declared-t_attribute[ name = lv_name ] TO FIELD-SYMBOL(<attribute>).
        IF sy-subrc <> 0.
          INSERT VALUE #( name = lv_name
                          type = lv_type ) INTO TABLE declared-t_attribute.
        ELSEIF lv_index < lv_count.
          <attribute>-type = lv_type.
        ENDIF.
      ELSE.
        ASSIGN declared-t_component[ owner = lv_owner
                                     name  = lv_name ] TO FIELD-SYMBOL(<component>).
        IF sy-subrc <> 0.
          INSERT VALUE #( owner = lv_owner
                          name  = lv_name
                          type  = lv_type ) INTO TABLE declared-t_component.
        ELSEIF lv_index < lv_count.
          <component>-type = lv_type.
        ENDIF.
      ENDIF.
      lv_owner = lv_type.
    ENDLOOP.

  ENDMETHOD.

  METHOD class_of_form.

    DATA ls_declared TYPE ty_s_declared.
    DATA lt_text     TYPE string_table.

    " every text of the form may name a field
    INSERT form-header INTO TABLE lt_text.
    INSERT form-footer INTO TABLE lt_text.
    LOOP AT form-t_element ASSIGNING FIELD-SYMBOL(<element>).
      INSERT <element>-text INTO TABLE lt_text.
    ENDLOOP.
    LOOP AT lt_text INTO DATA(lv_text).
      SPLIT lv_text AT `{` INTO TABLE DATA(lt_piece).
      LOOP AT lt_piece INTO DATA(lv_piece) FROM 2.
        DATA(lv_end) = find( val = lv_piece
                             sub = `}` ).
        IF lv_end <= 0.
          CONTINUE.
        ENDIF.
        DATA(lv_path) = substring( val = lv_piece
                                   len = lv_end ).
        " the numbers of the pages are the layout's, not the data's
        IF lv_path = `page` OR lv_path = `pages`.
          CONTINUE.
        ENDIF.
        declare_path( EXPORTING path     = lv_path
                                type     = `string`
                      CHANGING  declared = ls_declared ).
      ENDLOOP.
    ENDLOOP.

    " a table element: a table of rows with a text per column; an image
    " that comes from the data: bytes
    LOOP AT form-t_element ASSIGNING <element>.
      DATA(lv_source) = to_upper( <element>-source ).
      IF is_name( lv_source ) = abap_false OR line_exists( ls_declared-t_attribute[ name = lv_source ] ).
        CONTINUE.
      ENDIF.
      CASE <element>-type.

        WHEN zabaputil_cl_pdf_form=>cs_type-table.
          DATA(lv_row) = |TY_S_{ lv_source }|.
          DATA(lv_table) = |TY_T_{ lv_source }|.
          INSERT VALUE #( name = lv_row
                          kind = cs_kind-structure ) INTO TABLE ls_declared-t_type.
          INSERT VALUE #( name = lv_table
                          kind = cs_kind-table
                          ref  = lv_row ) INTO TABLE ls_declared-t_type.
          LOOP AT <element>-t_column ASSIGNING FIELD-SYMBOL(<column>).
            DATA(lv_column) = to_upper( <column>-name ).
            IF is_name( lv_column ) = abap_true AND NOT line_exists( ls_declared-t_component[ owner = lv_row
                                                                                               name  = lv_column ] ).
              INSERT VALUE #( owner = lv_row
                              name  = lv_column
                              type  = `string` ) INTO TABLE ls_declared-t_component.
            ENDIF.
          ENDLOOP.
          INSERT VALUE #( name = lv_source
                          type = lv_table ) INTO TABLE ls_declared-t_attribute.

        WHEN zabaputil_cl_pdf_form=>cs_type-image.
          INSERT VALUE #( name = lv_source
                          type = `xstring` ) INTO TABLE ls_declared-t_attribute.

      ENDCASE.
    ENDLOOP.

    result = class_source( name      = name
                           interface = interface
                           declared  = ls_declared
                           text      = text ).

  ENDMETHOD.

  METHOD types_source.

    " what the source does not declare is a type of the language
    DATA(ls_type) = resolve( declared = declared
                             type     = type ).
    IF ls_type-name IS INITIAL OR line_exists( done[ table_line = ls_type-name ] ).
      RETURN.
    ENDIF.
    INSERT ls_type-name INTO TABLE done.
    DATA(lv_name) = to_lower( ls_type-name ).

    CASE ls_type-kind.

      WHEN cs_kind-structure.
        LOOP AT declared-t_component INTO DATA(ls_component) WHERE owner = ls_type-name.
          types_source( EXPORTING declared = declared
                                  type     = ls_component-type
                        CHANGING  done     = done
                                  result   = result ).
        ENDLOOP.
        result = |{ result }    TYPES:\n      BEGIN OF { lv_name },\n|.
        LOOP AT declared-t_component INTO ls_component WHERE owner = ls_type-name.
          DATA(ls_inner) = resolve( declared = declared
                                    type     = ls_component-type ).
          DATA(lv_type) = ls_component-type.
          IF ls_inner-name IS NOT INITIAL.
            lv_type = to_lower( ls_inner-name ).
          ENDIF.
          result = |{ result }        { to_lower( ls_component-name ) } TYPE { lv_type },\n|.
        ENDLOOP.
        result = |{ result }      END OF { lv_name }.\n|.

      WHEN cs_kind-table.
        types_source( EXPORTING declared = declared
                                type     = ls_type-ref
                      CHANGING  done     = done
                                result   = result ).
        DATA(ls_row) = resolve( declared = declared
                                type     = ls_type-ref ).
        DATA(lv_row) = ls_type-ref.
        IF ls_row-name IS NOT INITIAL.
          lv_row = to_lower( ls_row-name ).
        ELSEIF lv_row IS INITIAL.
          lv_row = `string`.
        ENDIF.
        " a default key: the one table declaration every release takes
        result = |{ result }    TYPES { lv_name } TYPE STANDARD TABLE OF { lv_row } WITH DEFAULT KEY.\n|.

    ENDCASE.

  ENDMETHOD.

  METHOD class_source.

    DATA lt_done  TYPE string_table.
    DATA lv_types TYPE string.
    DATA lv_data  TYPE string.

    LOOP AT declared-t_attribute INTO DATA(ls_attribute).
      types_source( EXPORTING declared = declared
                              type     = ls_attribute-type
                    CHANGING  done     = lt_done
                              result   = lv_types ).
      DATA(ls_type) = resolve( declared = declared
                               type     = ls_attribute-type ).
      DATA(lv_type) = ls_attribute-type.
      IF ls_type-name IS NOT INITIAL.
        lv_type = to_lower( ls_type-name ).
      ENDIF.
      lv_data = |{ lv_data }    DATA { to_lower( ls_attribute-name ) } TYPE { lv_type }.\n|.
    ENDLOOP.
    IF lv_types IS NOT INITIAL.
      lv_types = |{ lv_types }\n|.
    ENDIF.
    IF lv_data IS NOT INITIAL.
      lv_data = |{ lv_data }\n|.
    ENDIF.

    DATA(lv_class) = to_lower( name ).
    DATA(lv_interface) = to_lower( interface ).
    result = |CLASS { lv_class } DEFINITION\n  PUBLIC\n  FINAL\n  CREATE PUBLIC.\n\n| &&
             |  PUBLIC SECTION.\n\n| &&
             |    INTERFACES { lv_interface }.\n\n| &&
             lv_types &&
             lv_data &&
             |  PROTECTED SECTION.\n  PRIVATE SECTION.\nENDCLASS.\n\n\n| &&
             |CLASS { lv_class } IMPLEMENTATION.\n\n| &&
             method_get( text      = text
                         interface = interface ) &&
             |\nENDCLASS.\n|.

  ENDMETHOD.

  METHOD definition.

    IF class_name( val ) IS INITIAL.
      RETURN.
    ENDIF.
    " from the CLASS statement to the period of the first ENDCLASS
    DATA(lv_upper) = to_upper( val ).
    DATA(lv_keyword) = find( val = lv_upper
                             sub = ` DEFINITION` ).
    DATA(lv_before) = substring( val = lv_upper
                                 len = lv_keyword ).
    DATA(lv_start) = find( val = lv_before
                           sub = `CLASS `
                           occ = -1 ).
    DATA(lv_end) = find( val = lv_upper
                         sub = `ENDCLASS`
                         off = lv_keyword ).
    IF lv_start < 0.
      RETURN.
    ENDIF.
    IF lv_end < 0.
      " a definition somebody is still writing
      result = substring( val = val
                          off = lv_start ).
      RETURN.
    ENDIF.
    DATA(lv_period) = find( val = lv_upper
                            sub = `.`
                            off = lv_end ).
    IF lv_period < 0.
      lv_period = strlen( val ) - 1.
    ENDIF.
    result = substring( val = val
                        off = lv_start
                        len = lv_period - lv_start + 1 ).

  ENDMETHOD.

  METHOD implementation.

    DATA(lv_definition) = definition( val ).
    IF lv_definition IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_at) = find( val = val
                        sub = lv_definition ).
    IF lv_at < 0.
      RETURN.
    ENDIF.
    DATA(lv_rest) = substring( val = val
                               off = lv_at + strlen( lv_definition ) ).
    " only an implementation is one: CLASS ... IMPLEMENTATION
    DATA(lv_start) = find( val = to_upper( lv_rest )
                           sub = `CLASS ` ).
    IF lv_start < 0 OR to_upper( lv_rest ) NS ` IMPLEMENTATION`.
      RETURN.
    ENDIF.
    result = substring( val = lv_rest
                        off = lv_start ).

  ENDMETHOD.

  METHOD has_interface.

    LOOP AT statements( public_section( val ) ) INTO DATA(lv_statement).
      IF lv_statement CP `INTERFACES *IF_PDF_DATA`.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD nodes_of.

    DATA ls_node TYPE zabaputil_cl_util_context=>ty_s_type_node.

    ls_node-id     = lines( nodes ) + 1.
    ls_node-parent = parent.
    ls_node-name   = name.

    DATA(ls_type) = resolve( declared = declared
                             type     = type ).
    CASE ls_type-kind.

      WHEN cs_kind-structure.
        ls_node-kind = `S`.
        INSERT ls_node INTO TABLE nodes.
        IF depth < 4.
          LOOP AT declared-t_component INTO DATA(ls_declared) WHERE owner = ls_type-name.
            IF ls_declared-type = `%REF` OR is_name( ls_declared-name ) = abap_false
                OR line_exists( nodes[ parent = ls_node-id
                                       name   = ls_declared-name ] ).
              CONTINUE.
            ENDIF.
            nodes_of( EXPORTING declared = declared
                                type     = ls_declared-type
                                depth    = depth + 1
                                parent   = ls_node-id
                                name     = ls_declared-name
                      CHANGING  nodes    = nodes ).
          ENDLOOP.
        ENDIF.
        " a structure without a component is none: the context makes a text

      WHEN cs_kind-table.
        ls_node-kind = `T`.
        INSERT ls_node INTO TABLE nodes.
        nodes_of( EXPORTING declared = declared
                            type     = ls_type-ref
                            depth    = depth + 1
                            parent   = ls_node-id
                            name     = ``
                  CHANGING  nodes    = nodes ).

      WHEN OTHERS.
        ls_node-kind = `E`.
        IF ddic( type ) IS NOT INITIAL.
          " a type of the dictionary, where the system has it
          ls_node-kind = `D`.
          ls_node-type = type.
        ELSEIF numeric_length( type ) > 0.
          " a numeric text of its length
          ls_node-type   = `N`.
          ls_node-length = numeric_length( type ).
        ELSE.
          " a type of the language by its name; anything else - a type of
          " another class, a data element of another system - prints as a
          " text
          CASE type.
            WHEN `I` OR `INT8` OR `INT1` OR `INT2`.
              ls_node-type = `I`.
            WHEN `P` OR `DECFLOAT16` OR `DECFLOAT34` OR `F`.
              ls_node-type     = `P`.
              ls_node-length   = 8.
              ls_node-decimals = 2.
            WHEN `N`.
              ls_node-type   = `N`.
              ls_node-length = 10.
            WHEN `D` OR `T`.
              ls_node-type = type.
            WHEN `X` OR `XSTRING`.
              ls_node-type = `XSTRING`.
            WHEN OTHERS.
              ls_node-type = `STRING`.
          ENDCASE.
        ENDIF.
        INSERT ls_node INTO TABLE nodes.

    ENDCASE.

  ENDMETHOD.
  METHOD sample_data.

    DATA lt_node TYPE zabaputil_cl_util_context=>ty_t_type_node.

    FIELD-SYMBOLS <data> TYPE any.

    " a source somebody is typing may declare anything: whatever cannot be
    " made a structure of leaves the preview without data, not with a dump
    TRY.
        DATA(ls_declared) = declared( val ).
        INSERT VALUE #( id   = 1
                        kind = `S` ) INTO TABLE lt_node.
        LOOP AT ls_declared-t_attribute INTO DATA(ls_attribute).
          IF ls_attribute-type = `%REF` OR is_name( ls_attribute-name ) = abap_false
              OR line_exists( lt_node[ parent = 1
                                       name   = ls_attribute-name ] ).
            CONTINUE.
          ENDIF.
          nodes_of( EXPORTING declared = ls_declared
                              type     = ls_attribute-type
                              depth    = 0
                              parent   = 1
                              name     = ls_attribute-name
                    CHANGING  nodes    = lt_node ).
        ENDLOOP.
        IF NOT line_exists( lt_node[ parent = 1 ] ).
          RETURN.
        ENDIF.
        result = zabaputil_cl_util_context=>rtti_create_data_by_nodes( lt_node ).
        ASSIGN result->* TO <data>.
        zabaputil_cl_pdf_form=>sample( CHANGING data = <data> ).
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.
  METHOD call_literal.

    IF numeric_length( type ) > 0.
      result = `1`.
      RETURN.
    ENDIF.
    CASE type.
      WHEN `I` OR `INT8` OR `INT1` OR `INT2` OR `P` OR `F` OR `DECFLOAT16` OR `DECFLOAT34` OR `N`.
        result = `1`.
      WHEN `D`.
        result = `sy-datum`.
      WHEN `T`.
        result = `sy-uzeit`.
      WHEN `X` OR `XSTRING` OR `%REF` OR `UTCLONG`.
        " bytes and references have no literal: left to the program
        CLEAR result.
      WHEN OTHERS.
        result = '`...`'.
    ENDCASE.

  ENDMETHOD.

  METHOD call_lines.

    DATA lv_row TYPE string.

    DATA(ls_type) = resolve( declared = declared
                             type     = type ).
    CASE ls_type-kind.

      WHEN cs_kind-structure.
        IF depth >= 3.
          RETURN.
        ENDIF.
        LOOP AT declared-t_component INTO DATA(ls_component) WHERE owner = ls_type-name.
          result = result && call_lines( declared = declared
                                         type     = ls_component-type
                                         target   = |{ target }-{ to_lower( ls_component-name ) }|
                                         depth    = depth + 1
                                         classic  = classic ).
        ENDLOOP.

      WHEN cs_kind-table.
        " one row, to be repeated: a value per column, or the value itself
        DATA(ls_row) = resolve( declared = declared
                                type     = ls_type-ref ).
        IF ls_row-kind = cs_kind-structure.
          LOOP AT declared-t_component INTO ls_component WHERE owner = ls_row-name.
            DATA(lv_value) = call_literal( ls_component-type ).
            DATA(ls_inner) = resolve( declared = declared
                                      type     = ls_component-type ).
            IF lv_value IS INITIAL OR ls_inner-name IS NOT INITIAL.
              CONTINUE.
            ENDIF.
            IF lv_row IS NOT INITIAL.
              lv_row = |{ lv_row }\n                |.
            ENDIF.
            lv_row = |{ lv_row }{ to_lower( ls_component-name ) } = { lv_value }|.
          ENDLOOP.
          IF lv_row IS NOT INITIAL AND classic = abap_true.
            " a work area of the row type, filled, appended: named after the
            " table, ls_t_items for lo_form->t_items
            DATA(lv_area) = |ls_{ replace( val  = substring_after( val = target
                                                                    sub = `->` )
                                           sub  = `-`
                                           with = `_`
                                           occ  = 0 ) }|.
            lv_row = replace( val  = lv_row
                              sub  = |\n                |
                              with = |.\n{ lv_area }-|
                              occ  = 0 ).
            result = |CLEAR { lv_area }.\n{ lv_area }-{ lv_row }.\nAPPEND { lv_area } TO { target }.\n|.
          ELSEIF lv_row IS NOT INITIAL.
            result = |INSERT VALUE #( { lv_row } ) INTO TABLE { target }.\n|.
          ENDIF.
        ELSEIF ls_row-kind IS INITIAL.
          lv_value = call_literal( ls_type-ref ).
          IF lv_value IS NOT INITIAL.
            result = |INSERT { lv_value } INTO TABLE { target }.\n|.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        " a structure of the dictionary: its fields one by one
        DATA(lt_ddic) = ddic( type ).
        IF lt_ddic IS INITIAL.
          lv_value = call_literal( type ).
        ELSE.
          DATA(ls_ddic) = lt_ddic[ 1 ].
          CASE ls_ddic-kind.
            WHEN `S`.
              " its elementary components, those of an include among them
              " (of a kind the context has no name for, none)
              LOOP AT lt_ddic INTO DATA(ls_field) WHERE parent = ls_ddic-id AND kind = `E` AND type IS NOT INITIAL.
                lv_value = call_literal( ls_field-type ).
                IF lv_value IS NOT INITIAL.
                  result = result && |{ target }-{ to_lower( ls_field-name ) } = { lv_value }.\n|.
                ENDIF.
              ENDLOOP.
              RETURN.
            WHEN `T`.
              result = |" { target }: the rows of { to_lower( type ) }\n|.
              RETURN.
            WHEN OTHERS.
              IF ls_ddic-type IS NOT INITIAL.
                lv_value = call_literal( ls_ddic-type ).
              ENDIF.
          ENDCASE.
        ENDIF.
        IF lv_value IS NOT INITIAL.
          result = |{ target } = { lv_value }.\n|.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD call_example.

    DATA lv_lines TYPE string.
    DATA lv_rows  TYPE string.

    DATA(lv_class) = to_lower( class_name( val ) ).
    IF lv_class IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_declared) = declared( val ).
    LOOP AT ls_declared-t_attribute INTO DATA(ls_attribute).
      DATA(lv_attribute) = call_lines( declared = ls_declared
                                       type     = ls_attribute-type
                                       target   = |lo_form->{ to_lower( ls_attribute-name ) }|
                                       depth    = 0
                                       classic  = classic ).
      lv_lines = lv_lines && lv_attribute.
    ENDLOOP.

    " a work area for every table a row is appended to
    IF classic = abap_true.
      DATA(lv_line_feed) = |\n|.
      SPLIT lv_lines AT lv_line_feed INTO TABLE DATA(lt_line).
      LOOP AT lt_line INTO DATA(lv_line) WHERE table_line CP `APPEND *`.
        SPLIT lv_line AT ` ` INTO TABLE DATA(lt_word).
        IF lines( lt_word ) = 4.
          lv_rows = |{ lv_rows }DATA { lt_word[ 2 ] } LIKE LINE OF { replace( val  = lt_word[ 4 ]
                                                                            sub  = `.`
                                                                            with = `` ) }.\n|.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF lv_lines IS NOT INITIAL.
      lv_lines = |" the data of this print\n{ lv_lines }\n|.
    ENDIF.

    IF classic = abap_true.
      " for a release before 7.40: no NEW, no inline declaration, a work
      " area per row - and a table of two tables is one work area each
      result = |" Print the form { lv_class }: an instance, its attributes, one call.\n| &&
               |DATA lo_form TYPE REF TO { lv_class }.\n| &&
               |DATA lv_pdf TYPE xstring.\n| &&
               lv_rows &&
               |\nCREATE OBJECT lo_form.\n\n| &&
               lv_lines &&
               |" the file, as an xstring - to download, to mail, to store\n| &&
               |lv_pdf = { to_lower( form_class ) }=>print( lo_form ).\n|.
      RETURN.
    ENDIF.

    result = |" Print the form { lv_class }: an instance, its attributes, one call.\n| &&
             |DATA(lo_form) = NEW { lv_class }( ).\n\n| &&
             lv_lines &&
             |" the file, as an xstring - to download, to mail, to store\n| &&
             |DATA(lv_pdf) = { to_lower( form_class ) }=>print( lo_form ).\n|.

  ENDMETHOD.

ENDCLASS.

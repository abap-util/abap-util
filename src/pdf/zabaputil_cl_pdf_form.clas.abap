CLASS zabaputil_cl_pdf_form DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " A form: the description of a document as data, and the document it
    " makes of the data of one print. What Smart Forms and Adobe forms keep
    " in a repository object is one structure here (ty_s_form) - an editor
    " changes it, a table stores it, and build( ) or render( ) print it:
    "
    "   DATA(lv_pdf) = zabaputil_cl_pdf_form=>render( val  = ls_form
    "                                                data = ls_delivery ).
    "
    " A form is a list of elements. An element is either in the flow -
    " texts, tables and spaces follow each other down the page and go on
    " on the next one - or fixed at a position of the first or of every
    " page (a logo, an address, a frame). All lengths are millimetres from
    " the top left corner of the page; a font size is in points.
    "
    " A text may name components of the data in braces: `Dear {NAME}`,
    " `{HEADER-CITY}` for a component of a component. A name the data does
    " not have stays as it is written, so a form shows its placeholders
    " when it is printed without data.
    "
    " The data is a structure, or an object: its PUBLIC attributes are then
    " what the form prints. A form names the class of such an object as its
    " data class (zabaputil_if_pdf_data), which is how an editor knows the
    " fields it can offer and what to show in a preview.

    CONSTANTS:
      BEGIN OF cs_type,
        "! a text, wrapped at the margin (flow) or at its width (fixed)
        text       TYPE string VALUE `TEXT`,
        "! a table with one row per line of a table of the data (flow)
        table      TYPE string VALUE `TABLE`,
        "! a horizontal rule (flow), or a line from x, y to x + w, y + h (fixed)
        line       TYPE string VALUE `LINE`,
        "! a rectangle (fixed)
        rect       TYPE string VALUE `RECT`,
        "! a JPEG or PNG file
        image      TYPE string VALUE `IMAGE`,
        "! empty space of the height h (flow)
        space      TYPE string VALUE `SPACE`,
        "! the flow goes on on a new page
        page_break TYPE string VALUE `PAGE_BREAK`,
      END OF cs_type.

    TYPES ty_length TYPE p LENGTH 8 DECIMALS 2.

    TYPES:
      BEGIN OF ty_s_column,
        "! the component of the row
        name  TYPE string,
        "! the text of the header row
        title TYPE string,
        "! the width in mm, 0 for an equal share of the rest
        width TYPE ty_length,
        "! L, C or R
        align TYPE string,
      END OF ty_s_column.
    TYPES ty_t_column TYPE STANDARD TABLE OF ty_s_column WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_element,
        "! one of cs_type
        type       TYPE string,
        "! what the element is called in an editor - not printed
        name       TYPE string,
        "! at the position x, y instead of in the flow
        fixed      TYPE abap_bool,
        "! a fixed element on every page instead of on the first one
        every_page TYPE abap_bool,
        x          TYPE ty_length,
        y          TYPE ty_length,
        "! the width: of a fixed text (0: no wrapping), a line, a rectangle, an image
        w          TYPE ty_length,
        "! the height: of a space, a line, a rectangle, an image
        h          TYPE ty_length,
        "! the text, with the components of the data in braces
        text       TYPE string,
        "! a standard font: Helvetica, Times-Roman, Courier and their cuts
        font       TYPE string,
        "! the font size in points; the width of a line or outline in points
        size       TYPE ty_length,
        "! the colour of the text, the line or the outline, RRGGBB
        color      TYPE string,
        "! L, C or R
        align      TYPE string,
        "! the colour a rectangle is filled with, RRGGBB; empty for none
        fill       TYPE string,
        "! the component of the data: the rows of a table, the bytes of an image
        source     TYPE string,
        "! the columns of a table
        t_column   TYPE ty_t_column,
        "! the bytes of an image that belongs to the form (a logo)
        image      TYPE xstring,
      END OF ty_s_element.
    TYPES ty_t_element TYPE STANDARD TABLE OF ty_s_element WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_form,
        name          TYPE string,
        "! the title of the document, a property of the file
        title         TYPE string,
        "! A3, A4, A5, LETTER or LEGAL
        format        TYPE string,
        "! P portrait, L landscape
        orientation   TYPE string,
        margin_left   TYPE ty_length,
        margin_top    TYPE ty_length,
        margin_right  TYPE ty_length,
        margin_bottom TYPE ty_length,
        "! a line in the top margin of every page; {page} and {pages} are the page numbers
        header        TYPE string,
        "! a line in the bottom margin of every page
        footer        TYPE string,
        "! the class whose public attributes the form prints - it implements
        "! zabaputil_if_pdf_data; empty for a form that is printed from a structure
        data_class    TYPE string,
        t_element     TYPE ty_t_element,
      END OF ty_s_form.

    CONSTANTS:
      BEGIN OF cs_field_kind,
        "! a single value, to be named in a text
        value  TYPE string VALUE `VALUE`,
        "! an internal table, the rows of a table element
        table  TYPE string VALUE `TABLE`,
        "! a component of the rows of a table, a column
        column TYPE string VALUE `COLUMN`,
      END OF cs_field_kind.

    TYPES:
      BEGIN OF ty_s_field,
        "! how a form names it: NAME, HEADER-CITY, T_ITEMS - for a column the
        "! component of the row
        path  TYPE string,
        "! one of cs_field_kind
        kind  TYPE string,
        "! for a column: the path of its table
        table TYPE string,
      END OF ty_s_field.
    TYPES ty_t_field TYPE STANDARD TABLE OF ty_s_field WITH EMPTY KEY.

    "! An empty form: A4 portrait, 20 mm margins
    CLASS-METHODS new_form
      RETURNING
        VALUE(result) TYPE ty_s_form.

    "! The document of a form, to add to it or to look at its pages
    "! @parameter val  | the form
    "! @parameter data | a structure with the components the form names, or an
    "!                   object with them as public attributes
    CLASS-METHODS build
      IMPORTING
        val           TYPE ty_s_form
        data          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

    "! The PDF file of a form
    "! @parameter val  | the form
    "! @parameter data | a structure with the components the form names, or an
    "!                   object with them as public attributes
    CLASS-METHODS render
      IMPORTING
        val           TYPE ty_s_form
        data          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE xstring.

    "! What the data offers a form: its values, its tables and their columns
    "! @parameter data | a structure, or an object with public attributes
    CLASS-METHODS fields
      IMPORTING
        data          TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_field.

    "! An instance of a data class with the values for a preview. Raises
    "! when the name is no class that implements zabaputil_if_pdf_data, or
    "! one that cannot be created without parameters
    "! @parameter val | the name of the class, as a form carries it
    CLASS-METHODS data_sample
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_data.

    "! A text with its placeholders replaced by the components of the data
    "! @parameter val  | the text, `Dear {NAME}`
    "! @parameter data | a structure or an object; without it the text comes
    "!                   back as it is
    CLASS-METHODS resolve
      IMPORTING
        val           TYPE clike
        data          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_font  TYPE string VALUE `Helvetica`.
    CONSTANTS c_color TYPE string VALUE `000000`.
    CONSTANTS c_size  TYPE f VALUE 10.

    "! The value of a component of the data as text
    "! @parameter path  | NAME, or HEADER-NAME for a component of a component
    "! @parameter found | the data has it, and it is a single value
    CLASS-METHODS component_text
      IMPORTING
        path  TYPE string
        data  TYPE any
      EXPORTING
        found TYPE abap_bool
        text  TYPE string.

    "! A component of a structure, or a public attribute of an object
    "! @parameter node   | the structure or the object reference
    "! @parameter name   | the name, upper case
    "! @parameter result | the component; initial when the node has none of that name
    CLASS-METHODS component_of
      IMPORTING
        node          TYPE any
        name          TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! The names of the components of a structure, or of the public instance
    "! attributes of an object
    CLASS-METHODS component_names
      IMPORTING
        node          TYPE any
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS fields_of
      IMPORTING
        node   TYPE any
        prefix TYPE string
        depth  TYPE i
      CHANGING
        result TYPE ty_t_field.

    "! The font of an element
    CLASS-METHODS element_font
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        element       TYPE ty_s_element
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_font.

    "! The bold cut of the font of an element, for the header row of a table
    CLASS-METHODS element_font_bold
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        element       TYPE ty_s_element
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_font.

    CLASS-METHODS element_size
      IMPORTING
        element       TYPE ty_s_element
      RETURNING
        VALUE(result) TYPE f.

    CLASS-METHODS element_color
      IMPORTING
        element       TYPE ty_s_element
      RETURNING
        VALUE(result) TYPE string.

    "! The image of an element: from the data, or the one the form carries
    CLASS-METHODS element_image
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        element       TYPE ty_s_element
        data          TYPE any
        with_data     TYPE abap_bool
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_image.

    CLASS-METHODS flow_element
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        element   TYPE ty_s_element
        left      TYPE ty_length
        data      TYPE any
        with_data TYPE abap_bool.

    CLASS-METHODS flow_table
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        element   TYPE ty_s_element
        data      TYPE any
        with_data TYPE abap_bool.

    CLASS-METHODS fixed_element
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        page      TYPE REF TO zabaputil_cl_pdf_page
        element   TYPE ty_s_element
        data      TYPE any
        with_data TYPE abap_bool.

    CLASS-METHODS fixed_text
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        page      TYPE REF TO zabaputil_cl_pdf_page
        element   TYPE ty_s_element
        text      TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_form IMPLEMENTATION.

  METHOD new_form.

    result-format = `A4`.
    result-orientation = `P`.
    result-margin_left = 20.
    result-margin_top = 20.
    result-margin_right = 20.
    result-margin_bottom = 20.

  ENDMETHOD.

  METHOD render.

    DATA lo_pdf TYPE REF TO zabaputil_cl_pdf.

    IF data IS SUPPLIED.
      lo_pdf = build( val  = val
                      data = data ).
    ELSE.
      lo_pdf = build( val ).
    ENDIF.
    result = lo_pdf->render( ).

  ENDMETHOD.

  METHOD build.

    DATA lv_with_data TYPE abap_bool.
    DATA lv_format    TYPE string.
    DATA lv_number    TYPE i.
    DATA lv_header    TYPE string.
    DATA lv_footer    TYPE string.

    FIELD-SYMBOLS <data> TYPE any.

    " without data the form prints its placeholders: the form itself
    " stands in, a structure that has none of the names
    lv_with_data = xsdbool( data IS SUPPLIED ).
    IF lv_with_data = abap_true.
      ASSIGN data TO <data>.
    ELSE.
      ASSIGN val-name TO <data>.
    ENDIF.

    lv_format = val-format.
    IF lv_format IS INITIAL.
      lv_format = `A4`.
    ENDIF.
    result = zabaputil_cl_pdf=>create( unit        = `mm`
                                       format      = lv_format
                                       orientation = val-orientation ).
    IF val-title IS NOT INITIAL.
      result->set_info( title = val-title ).
    ENDIF.

    DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( result ).
    lo_layout->set_margins( left   = val-margin_left
                            top    = val-margin_top
                            right  = val-margin_right
                            bottom = val-margin_bottom ).
    lo_layout->set_font( font = result->font( c_font )
                         size = c_size ).

    LOOP AT val-t_element ASSIGNING FIELD-SYMBOL(<element>).
      IF <element>-fixed = abap_true OR <element>-type = cs_type-rect.
        CONTINUE.
      ENDIF.
      flow_element( pdf       = result
                    layout    = lo_layout
                    element   = <element>
                    left      = val-margin_left
                    data      = <data>
                    with_data = lv_with_data ).
    ENDLOOP.

    " a form without anything in the flow still has its first page
    lo_layout->page( ).

    " header and footer in the standard font, whatever the last element used
    lo_layout->set_font( font = result->font( c_font )
                         size = c_size ).
    lo_layout->set_color( c_color ).
    lv_header = val-header.
    lv_footer = val-footer.
    IF lv_with_data = abap_true.
      lv_header = resolve( val  = lv_header
                           data = <data> ).
      lv_footer = resolve( val  = lv_footer
                           data = <data> ).
    ENDIF.
    IF lv_header IS NOT INITIAL.
      lo_layout->set_header( val   = lv_header
                             align = `R` ).
    ENDIF.
    IF lv_footer IS NOT INITIAL.
      lo_layout->set_footer( lv_footer ).
    ENDIF.
    lo_layout->finish( ).

    " the fixed elements go over the flow, on the first or on every page
    LOOP AT val-t_element ASSIGNING <element>.
      IF <element>-fixed = abap_false AND <element>-type <> cs_type-rect.
        CONTINUE.
      ENDIF.
      lv_number = 1.
      WHILE lv_number <= result->page_count( ).
        fixed_element( pdf       = result
                       layout    = lo_layout
                       page      = result->page( lv_number )
                       element   = <element>
                       data      = <data>
                       with_data = lv_with_data ).
        IF <element>-every_page = abap_false.
          EXIT.
        ENDIF.
        lv_number = lv_number + 1.
      ENDWHILE.
    ENDLOOP.

  ENDMETHOD.

  METHOD resolve.

    DATA lv_rest  TYPE string.
    DATA lv_open  TYPE i.
    DATA lv_close TYPE i.
    DATA lv_name  TYPE string.
    DATA lv_text  TYPE string.
    DATA lv_found TYPE abap_bool.

    lv_rest = val.
    IF data IS NOT SUPPLIED.
      result = lv_rest.
      RETURN.
    ENDIF.

    DO.
      lv_open = find( val = lv_rest
                      sub = `{` ).
      IF lv_open < 0.
        EXIT.
      ENDIF.
      lv_close = find( val = lv_rest
                       sub = `}`
                       off = lv_open ).
      IF lv_close < 0.
        EXIT.
      ENDIF.

      lv_name = substring( val = lv_rest
                           off = lv_open + 1
                           len = lv_close - lv_open - 1 ).
      component_text( EXPORTING path  = lv_name
                                data  = data
                      IMPORTING found = lv_found
                                text  = lv_text ).
      IF lv_found = abap_false.
        lv_text = |\{{ lv_name }\}|.
      ENDIF.
      result = result && substring( val = lv_rest
                                    len = lv_open ) && lv_text.
      lv_rest = substring( val = lv_rest
                           off = lv_close + 1 ).
    ENDDO.

    result = result && lv_rest.

  ENDMETHOD.

  METHOD component_text.

    DATA lv_path TYPE string.
    DATA lr_node TYPE REF TO data.

    FIELD-SYMBOLS <node> TYPE any.

    CLEAR found.
    CLEAR text.

    lv_path = to_upper( condense( path ) ).
    lv_path = replace( val  = lv_path
                       sub  = `.`
                       with = `-`
                       occ  = 0 ).
    IF lv_path IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_path AT `-` INTO TABLE DATA(lt_part).

    ASSIGN data TO <node>.
    LOOP AT lt_part INTO DATA(lv_part).
      lr_node = component_of( node = <node>
                              name = lv_part ).
      IF lr_node IS NOT BOUND.
        RETURN.
      ENDIF.
      ASSIGN lr_node->* TO <node>.
    ENDLOOP.

    " a structure, a table or a reference is no text
    DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( <node> ).
    IF lv_kind = `u` OR lv_kind = `v` OR lv_kind = `h` OR lv_kind = `l` OR lv_kind = `r`.
      RETURN.
    ENDIF.
    text = |{ <node> }|.
    found = abap_true.

  ENDMETHOD.

  METHOD component_of.

    DATA lo_object TYPE REF TO object.

    FIELD-SYMBOLS <component> TYPE any.

    DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( node ).
    CASE lv_kind.

      WHEN `u` OR `v`.
        ASSIGN COMPONENT name OF STRUCTURE node TO <component>.

      WHEN `r`.
        " an attribute of an object - a public one: what a class keeps to
        " itself is no part of its interface to a form
        TRY.
            lo_object ?= node.
          CATCH cx_sy_move_cast_error.
            RETURN.
        ENDTRY.
        IF lo_object IS NOT BOUND.
          RETURN.
        ENDIF.
        DATA(lt_name) = component_names( lo_object ).
        IF NOT line_exists( lt_name[ table_line = name ] ).
          RETURN.
        ENDIF.
        ASSIGN lo_object->(name) TO <component>.

    ENDCASE.

    IF <component> IS ASSIGNED.
      result = REF #( <component> ).
    ENDIF.

  ENDMETHOD.

  METHOD component_names.

    DATA lo_object TYPE REF TO object.

    DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( node ).
    CASE lv_kind.

      WHEN `u` OR `v` OR `h`.
        " of a table: the components of its rows
        TRY.
            LOOP AT zabaputil_cl_util_context=>rtti_get_t_attri_by_any( node ) ASSIGNING FIELD-SYMBOL(<component>).
              INSERT CONV string( <component>-name ) INTO TABLE result.
            ENDLOOP.
          CATCH cx_root ##NO_HANDLER.
            " rows that are no structure have no components
        ENDTRY.

      WHEN `r`.
        TRY.
            lo_object ?= node.
          CATCH cx_sy_move_cast_error.
            RETURN.
        ENDTRY.
        IF lo_object IS NOT BOUND.
          RETURN.
        ENDIF.
        LOOP AT zabaputil_cl_util_context=>rtti_get_t_attri_by_oref( lo_object ) ASSIGNING FIELD-SYMBOL(<attribute>).
          IF <attribute>-visibility = zabaputil_cl_util_context=>cv_objectdescr_public
              AND <attribute>-is_class = abap_false AND <attribute>-is_constant = abap_false.
            INSERT CONV string( <attribute>-name ) INTO TABLE result.
          ENDIF.
        ENDLOOP.

    ENDCASE.

  ENDMETHOD.

  METHOD fields.

    fields_of( EXPORTING node   = data
                         prefix = ``
                         depth  = 1
               CHANGING  result = result ).

  ENDMETHOD.

  METHOD fields_of.

    DATA lr_component TYPE REF TO data.

    FIELD-SYMBOLS <component> TYPE any.

    LOOP AT component_names( node ) INTO DATA(lv_name).
      lr_component = component_of( node = node
                                   name = lv_name ).
      IF lr_component IS NOT BOUND.
        CONTINUE.
      ENDIF.
      ASSIGN lr_component->* TO <component>.
      DATA(lv_path) = prefix && lv_name.

      DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( <component> ).
      CASE lv_kind.

        WHEN `h`.
          INSERT VALUE #( path = lv_path
                          kind = cs_field_kind-table ) INTO TABLE result.
          LOOP AT component_names( <component> ) INTO DATA(lv_column).
            INSERT VALUE #( path  = lv_column
                            kind  = cs_field_kind-column
                            table = lv_path ) INTO TABLE result.
          ENDLOOP.

        WHEN `u` OR `v` OR `r`.
          " three levels are what a form can reasonably name, and an object
          " that points back at its owner must not be followed for ever
          IF depth < 3.
            fields_of( EXPORTING node   = <component>
                                 prefix = |{ lv_path }-|
                                 depth  = depth + 1
                       CHANGING  result = result ).
          ENDIF.

        WHEN `l`.
          " a data reference is no value a text can name
          CONTINUE.

        WHEN OTHERS.
          INSERT VALUE #( path = lv_path
                          kind = cs_field_kind-value ) INTO TABLE result.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD data_sample.

    DATA lo_object TYPE REF TO object.
    DATA li_data   TYPE REF TO zabaputil_if_pdf_data.

    " the name of the interface from a reference typed with it: a renamed
    " copy of this class asks for its own interface
    DATA(lv_interface) = zabaputil_cl_util_context=>rtti_get_ref_type_name( li_data ).
    DATA(lv_class) = to_upper( condense( val ) ).

    " asked of the class descriptor, before anything is created: a class
    " that is no data class is never instantiated from a name somebody typed
    IF lv_class IS INITIAL OR zabaputil_cl_util_context=>rtti_check_class_impl_intf( class = lv_class
                                                                                   intf  = lv_interface ) = abap_false.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: { lv_class } is no class that implements { lv_interface }|.
    ENDIF.

    TRY.
        CREATE OBJECT lo_object TYPE (lv_class).
        result ?= lo_object.
      CATCH cx_root INTO DATA(lx_create).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |PDF: { lv_class } cannot be created - a data class needs a constructor without parameters|
            previous = lx_create.
    ENDTRY.
    result->fill_sample( ).

  ENDMETHOD.

  METHOD element_font.

    DATA(lv_name) = element-font.
    IF lv_name IS INITIAL.
      lv_name = c_font.
    ENDIF.
    result = pdf->font( lv_name ).

  ENDMETHOD.

  METHOD element_font_bold.

    DATA(lv_name) = element-font.
    IF lv_name IS INITIAL.
      lv_name = c_font.
    ENDIF.
    CASE lv_name.
      WHEN `Helvetica` OR `Courier`.
        lv_name = lv_name && `-Bold`.
      WHEN `Helvetica-Oblique`.
        lv_name = `Helvetica-BoldOblique`.
      WHEN `Courier-Oblique`.
        lv_name = `Courier-BoldOblique`.
      WHEN `Times-Roman`.
        lv_name = `Times-Bold`.
      WHEN `Times-Italic`.
        lv_name = `Times-BoldItalic`.
    ENDCASE.
    result = pdf->font( lv_name ).

  ENDMETHOD.

  METHOD element_size.

    result = element-size.
    IF result <= 0.
      result = c_size.
    ENDIF.

  ENDMETHOD.

  METHOD element_color.

    result = element-color.
    IF result IS INITIAL.
      result = c_color.
    ENDIF.

  ENDMETHOD.

  METHOD element_image.

    DATA lv_bytes TYPE xstring.

    FIELD-SYMBOLS <component> TYPE any.

    lv_bytes = element-image.
    IF with_data = abap_true AND element-source IS NOT INITIAL.
      DATA(lr_component) = component_of( node = data
                                         name = to_upper( element-source ) ).
      IF lr_component IS BOUND.
        ASSIGN lr_component->* TO <component>.
        " an xstring, or raw bytes of a fixed length
        DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( <component> ).
        IF lv_kind = `y` OR lv_kind = `X`.
          lv_bytes = <component>.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_bytes IS INITIAL.
      RETURN.
    ENDIF.
    result = pdf->image( lv_bytes ).

  ENDMETHOD.

  METHOD flow_element.

    DATA lv_text   TYPE string.
    DATA lv_height TYPE f.

    CASE element-type.

      WHEN cs_type-text.
        lv_text = element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        layout->set_font( font = element_font( pdf     = pdf
                                               element = element )
                          size = element_size( element ) ).
        layout->set_color( element_color( element ) ).
        layout->text( val   = lv_text
                      align = element-align ).

      WHEN cs_type-table.
        flow_table( pdf       = pdf
                    layout    = layout
                    element   = element
                    data      = data
                    with_data = with_data ).

      WHEN cs_type-line.
        layout->rule( ).

      WHEN cs_type-space.
        layout->add_space( element-h ).

      WHEN cs_type-page_break.
        layout->page_break( ).

      WHEN cs_type-image.
        DATA(lo_image) = element_image( pdf       = pdf
                                        element   = element
                                        data      = data
                                        with_data = with_data ).
        IF lo_image IS NOT BOUND.
          RETURN.
        ENDIF.
        " at the left margin, where the flow is; its height moves the flow on.
        " A text wraps, an image does not: it is not moved to the next page
        DATA(lo_page) = layout->page( ).
        lv_height = element-h.
        IF lv_height <= 0 AND element-w > 0.
          lv_height = element-w * lo_image->height( ) / lo_image->width( ).
        ELSEIF lv_height <= 0.
          lv_height = lo_image->height( ) / pdf->scale( ).
        ENDIF.
        lo_page->image( image = lo_image
                        x     = left
                        y     = layout->y( )
                        w     = element-w
                        h     = element-h ).
        layout->set_y( layout->y( ) + lv_height ).

    ENDCASE.

  ENDMETHOD.

  METHOD flow_table.

    DATA lt_column TYPE zabaputil_cl_pdf_layout=>ty_t_column.
    DATA ls_column TYPE zabaputil_cl_pdf_layout=>ty_s_column.
    DATA lt_sample TYPE zabaputil_cl_pdf_layout=>ty_t_text_row.
    DATA lt_cell   TYPE string_table.

    FIELD-SYMBOLS <rows>      TYPE ANY TABLE.
    FIELD-SYMBOLS <component> TYPE any.

    IF element-t_column IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT element-t_column ASSIGNING FIELD-SYMBOL(<column>).
      ls_column-name = to_upper( <column>-name ).
      ls_column-title = <column>-title.
      ls_column-width = <column>-width.
      ls_column-align = <column>-align.
      INSERT ls_column INTO TABLE lt_column.
    ENDLOOP.

    layout->set_font( font = element_font( pdf     = pdf
                                           element = element )
                      size = element_size( element ) ).
    layout->set_color( element_color( element ) ).

    " the rows: the table the data has under the name of the source
    IF with_data = abap_true AND element-source IS NOT INITIAL.
      DATA(lr_component) = component_of( node = data
                                         name = to_upper( element-source ) ).
      IF lr_component IS BOUND.
        ASSIGN lr_component->* TO <component>.
        DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( <component> ).
        IF lv_kind = `h`.
          ASSIGN <component> TO <rows>.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <rows> IS ASSIGNED.
      layout->table( columns     = lt_column
                     rows        = <rows>
                     header_font = element_font_bold( pdf     = pdf
                                                      element = element ) ).
      RETURN.
    ENDIF.

    " printed without its rows: three rows that name their components
    LOOP AT lt_column INTO ls_column.
      INSERT |\{{ ls_column-name }\}| INTO TABLE lt_cell.
    ENDLOOP.
    DO 3 TIMES.
      INSERT lt_cell INTO TABLE lt_sample.
    ENDDO.
    layout->table_text( columns     = lt_column
                        rows        = lt_sample
                        header_font = element_font_bold( pdf     = pdf
                                                         element = element ) ).

  ENDMETHOD.

  METHOD fixed_element.

    DATA lv_text  TYPE string.
    DATA lv_width TYPE f.

    CASE element-type.

      WHEN cs_type-text.
        lv_text = element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        fixed_text( pdf     = pdf
                    layout  = layout
                    page    = page
                    element = element
                    text    = lv_text ).

      WHEN cs_type-line OR cs_type-rect.
        lv_width = element-size.
        IF lv_width <= 0.
          lv_width = 1.
          lv_width = lv_width / 2.
        ENDIF.
        page->set_draw_color( element_color( element ) ).
        page->set_line_width( lv_width ).
        IF element-type = cs_type-line.
          page->line( x1 = element-x
                      y1 = element-y
                      x2 = element-x + element-w
                      y2 = element-y + element-h ).
        ELSEIF element-fill IS INITIAL.
          page->rect( x = element-x
                      y = element-y
                      w = element-w
                      h = element-h ).
        ELSE.
          page->set_fill_color( element-fill ).
          page->rect( x     = element-x
                      y     = element-y
                      w     = element-w
                      h     = element-h
                      style = `DF` ).
        ENDIF.

      WHEN cs_type-image.
        DATA(lo_image) = element_image( pdf       = pdf
                                        element   = element
                                        data      = data
                                        with_data = with_data ).
        IF lo_image IS BOUND.
          page->image( image = lo_image
                       x     = element-x
                       y     = element-y
                       w     = element-w
                       h     = element-h ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD fixed_text.

    DATA lt_line   TYPE string_table.
    DATA lv_x      TYPE f.
    DATA lv_y      TYPE f.
    DATA lv_height TYPE f.

    DATA(lo_font) = element_font( pdf     = pdf
                                  element = element ).
    DATA(lv_size) = element_size( element ).
    DATA(lv_align) = to_upper( element-align ).

    " the layout measures: a text with a width wraps in it
    layout->set_font( font = lo_font
                      size = lv_size ).
    DATA(lv_line_feed) = |\n|.
    DATA(lv_text) = replace( val  = text
                             sub  = |\r\n|
                             with = lv_line_feed
                             occ  = 0 ).
    SPLIT lv_text AT lv_line_feed INTO TABLE DATA(lt_paragraph).
    LOOP AT lt_paragraph INTO DATA(lv_paragraph).
      IF element-w > 0.
        DATA(lt_wrapped) = layout->wrap( val   = lv_paragraph
                                         width = element-w ).
        IF lt_wrapped IS INITIAL.
          INSERT `` INTO TABLE lt_line.
        ELSE.
          INSERT LINES OF lt_wrapped INTO TABLE lt_line.
        ENDIF.
      ELSE.
        INSERT lv_paragraph INTO TABLE lt_line.
      ENDIF.
    ENDLOOP.

    " x is where the text starts, ends or is centred; with a width it is
    " the left edge of the box the text is aligned in
    lv_x = element-x.
    IF element-w > 0.
      CASE lv_align.
        WHEN `R`.
          lv_x = lv_x + element-w.
        WHEN `C`.
          lv_x = lv_x + element-w / 2.
      ENDCASE.
    ENDIF.

    " y is the top of the first line
    lv_height = lv_size * 5 / 4 / pdf->scale( ).
    lv_y = element-y + lo_font->ascent( lv_size ) / pdf->scale( ).

    page->set_font( font = lo_font
                    size = lv_size ).
    page->set_text_color( element_color( element ) ).
    LOOP AT lt_line INTO DATA(lv_line).
      page->text( x     = lv_x
                  y     = lv_y
                  val   = lv_line
                  align = lv_align ).
      lv_y = lv_y + lv_height.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

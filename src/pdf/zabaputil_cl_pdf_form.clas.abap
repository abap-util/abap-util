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
    " An absolute form (ty_s_form-absolute) has no flow: every element
    " stands where its x, y, w and h say, which is what a designer with a
    " canvas draws. A table is as high as its rows make it and goes on on
    " the next page; what stands below it on the canvas follows where it
    " ends, at the same distance.
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
    "
    " A data class carries its form itself, as the text its get( ) returns
    " (stringify( ) writes it, parse( ) reads it). Then one object is the
    " whole form, and print( ) is all a caller needs:
    "
    "   DATA(lv_pdf) = zabaputil_cl_pdf_form=>print( lo_delivery_note ).

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
        "! a Code 128 barcode of the text - GS1-128 for data in the notation
        "! (00)340123450000000018 - and the text below it when size is set
        barcode    TYPE string VALUE `BARCODE`,
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
        "! the font size in points; the width of a line or outline in points;
        "! of a barcode the size of the text below the bars, 0 for none
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
        "! every element at its x, y, w, h instead of in the flow; what stands
        "! below a table follows where the table ends
        absolute      TYPE abap_bool,
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
        path   TYPE string,
        "! one of cs_field_kind
        kind   TYPE string,
        "! for a column: the path of its table
        parent TYPE string,
      END OF ty_s_field.
    TYPES ty_t_field TYPE STANDARD TABLE OF ty_s_field WITH EMPTY KEY.

    "! An empty form: A4 portrait, 20 mm margins
    CLASS-METHODS new_form
      RETURNING
        VALUE(result) TYPE ty_s_form.

    "! The same form in the absolute layout: every element of the flow at the
    "! place and of the size the flow gives it, as an editor shows a form that
    "! is switched over to a canvas. A space and a page break are gone - their
    "! place is the distance between the others -, a fixed element keeps its
    "! position, and a form that is absolute already comes back as it is.
    "!
    "! The flow is laid out as build( ) lays it out. What the flow puts on a
    "! later page keeps the place it has there - an absolute form has one page
    "! to draw on, so it lands on the elements of the first one; a table that
    "! goes on over a page break is as high as its part on its first page.
    "! @parameter val  | the form
    "! @parameter data | the data the flow is laid out with - a text is as high
    "!                   as its lines with these values are. Without it, with
    "!                   the placeholders
    CLASS-METHODS to_absolute
      IMPORTING
        val           TYPE ty_s_form
        data          TYPE any OPTIONAL
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

    "! A form as text: what get( ) of a data class returns. XML, written the
    "! same on every release and runtime, without what is initial:
    "! <form version="1"><FORMAT>A4</FORMAT>...<T_ELEMENT><item>...</item></T_ELEMENT></form>
    "! @parameter val | the form
    CLASS-METHODS stringify
      IMPORTING
        val           TYPE ty_s_form
      RETURNING
        VALUE(result) TYPE string.

    "! The form of a text stringify( ) wrote. An empty text is an empty form
    "! (new_form( )); a component the text has and the form does not is read
    "! past, one it does not have is initial. Raises when the text is no form
    "! @parameter val | the text
    CLASS-METHODS parse
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_form.

    "! The form of a data class: what its get( ) returns, with the class as
    "! the form's data class
    "! @parameter data | an instance of the data class
    CLASS-METHODS form_of
      IMPORTING
        data          TYPE REF TO zabaputil_if_pdf_data
      RETURNING
        VALUE(result) TYPE ty_s_form.

    "! The PDF file of a data class: its form, printed with its attributes
    "! @parameter data | an instance of the data class, its attributes filled
    CLASS-METHODS print
      IMPORTING
        data          TYPE REF TO zabaputil_if_pdf_data
      RETURNING
        VALUE(result) TYPE xstring.

    "! An instance of a data class with values for a preview: what the class
    "! leaves initial gets a sample value - a text its name, a number a
    "! number, a table three rows. Raises when the name is no class that
    "! implements zabaputil_if_pdf_data, or one that cannot be created
    "! without parameters
    "! @parameter val | the name of the class, as a form carries it
    CLASS-METHODS data_sample
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_data.

    "! Sample values into what is initial of a structure, or of the public
    "! attributes of an object - what data_sample( ) does for a data class
    "! @parameter data | the structure or the object reference
    CLASS-METHODS sample
      CHANGING
        data TYPE any.

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

    "! the element the text of a form is
    CONSTANTS c_root  TYPE string VALUE `form`.
    CONSTANTS c_font  TYPE string VALUE `Helvetica`.
    CONSTANTS c_color TYPE string VALUE `000000`.
    CONSTANTS c_size  TYPE f VALUE 10.
    "! the module of a barcode without a width, in mm: 0.33, what a
    "! scanner reads from arm's length
    CONSTANTS c_module TYPE f VALUE '0.33'.
    "! the height of a barcode without one, in mm
    CONSTANTS c_bar_height TYPE f VALUE 15.

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

    "! Sample values into the components of a structure, or the public
    "! attributes of an object, that are initial
    "! @parameter row  | the row of a table the node is, 0 for none
    "! @parameter node | the structure or the object reference
    CLASS-METHODS sample_fill
      IMPORTING
        row   TYPE i
        depth TYPE i
      CHANGING
        node  TYPE any.

    "! The content of an element of the text of a form: the components of a
    "! structure that are not initial, the rows of a table, a value
    CLASS-METHODS xml_of
      IMPORTING
        node          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    "! The element that opens at or behind a position. No functional method:
    "! one with a CHANGING parameter does not compile before release 7.40
    "! @parameter name  | the name of the element
    "! @parameter empty | the element is written as <NAME/>
    "! @parameter pos   | where to look; behind the tag afterwards
    CLASS-METHODS xml_open
      IMPORTING
        xml   TYPE string
      EXPORTING
        name  TYPE string
        empty TYPE abap_bool
      CHANGING
        pos   TYPE i.

    "! What an element holds, into a structure, a table or a field
    "! @parameter pos  | behind the opening tag; behind the closing tag afterwards
    "! @parameter node | what the element is read into
    CLASS-METHODS xml_read
      IMPORTING
        xml  TYPE string
      CHANGING
        pos  TYPE i
        node TYPE any.

    "! A sample value for a single field, by its type
    "! @parameter name | what the field is called
    "! @parameter row  | the row of a table the field is in, 0 for none
    CLASS-METHODS sample_value
      IMPORTING
        name  TYPE string
        row   TYPE i
      CHANGING
        value TYPE any.

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

    "! The elements of an absolute form, each at its position: page by page
    "! what the tables above it make of it
    CLASS-METHODS absolute_elements
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        form      TYPE ty_s_form
        data      TYPE any
        with_data TYPE abap_bool.

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

    "! A barcode at x, y: its height comes back
    CLASS-METHODS draw_barcode
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        page          TYPE REF TO zabaputil_cl_pdf_page
        element       TYPE ty_s_element
        text          TYPE string
        x             TYPE numeric
        y             TYPE numeric
      RETURNING
        VALUE(result) TYPE f.

    "! The width of a barcode: the element's, or 0.33 mm a module
    CLASS-METHODS barcode_width
      IMPORTING
        element       TYPE ty_s_element
        text          TYPE string
      RETURNING
        VALUE(result) TYPE f.

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

    IF val-absolute = abap_true.
      absolute_elements( pdf       = result
                         layout    = lo_layout
                         form      = val
                         data      = <data>
                         with_data = lv_with_data ).
    ELSE.
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
    ENDIF.

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

    " the fixed elements go over the flow, on the first or on every page.
    " Of an absolute form the ones of every page are left: the others are
    " on the page the tables above them left them on
    LOOP AT val-t_element ASSIGNING <element>.
      IF val-absolute = abap_true.
        IF <element>-every_page = abap_false.
          CONTINUE.
        ENDIF.
      ELSEIF <element>-fixed = abap_false AND <element>-type <> cs_type-rect.
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

  METHOD to_absolute.

    DATA lv_with_data TYPE abap_bool.
    DATA lv_format    TYPE string.
    DATA ls_element   TYPE ty_s_element.
    DATA lv_page      TYPE i.
    DATA lv_top       TYPE f.
    DATA lv_bottom    TYPE f.
    DATA lv_width     TYPE f.
    DATA lv_height    TYPE f.

    FIELD-SYMBOLS <data> TYPE any.

    result = val.
    IF val-absolute = abap_true.
      RETURN.
    ENDIF.
    result-absolute = abap_true.
    CLEAR result-t_element.

    lv_with_data = xsdbool( data IS SUPPLIED ).
    IF lv_with_data = abap_true.
      ASSIGN data TO <data>.
    ELSE.
      ASSIGN val-name TO <data>.
    ENDIF.

    " the document and the layout of build( ): the same page, the same
    " margins and the same font to start with
    lv_format = val-format.
    IF lv_format IS INITIAL.
      lv_format = `A4`.
    ENDIF.
    DATA(lo_pdf) = zabaputil_cl_pdf=>create( unit        = `mm`
                                             format      = lv_format
                                             orientation = val-orientation ).
    DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( lo_pdf ).
    lo_layout->set_margins( left   = val-margin_left
                            top    = val-margin_top
                            right  = val-margin_right
                            bottom = val-margin_bottom ).
    lo_layout->set_font( font = lo_pdf->font( c_font )
                         size = c_size ).
    DATA(lo_page) = lo_layout->page( ).
    lv_width = lo_page->width( ) - val-margin_left - val-margin_right.
    lv_height = lo_page->height( ).

    LOOP AT val-t_element ASSIGNING FIELD-SYMBOL(<element>).
      ls_element = <element>.
      ls_element-fixed = abap_false.

      " what stands at a position already stays there
      IF <element>-fixed = abap_true OR <element>-type = cs_type-rect.
        INSERT ls_element INTO TABLE result-t_element.
        CONTINUE.
      ENDIF.

      lv_page = lo_pdf->page_count( ).
      lv_top = lo_layout->y( ).
      flow_element( pdf       = lo_pdf
                    layout    = lo_layout
                    element   = <element>
                    left      = val-margin_left
                    data      = <data>
                    with_data = lv_with_data ).
      lv_bottom = lo_layout->y( ).

      IF <element>-type = cs_type-space OR <element>-type = cs_type-page_break.
        CONTINUE.
      ENDIF.

      IF lo_pdf->page_count( ) > lv_page.
        IF <element>-type = cs_type-table.
          " went on over the page break: as high as its part on the page
          " it began on
          lv_bottom = lv_height - val-margin_bottom.
        ELSE.
          " did not fit and began the next page
          lv_top = val-margin_top.
        ENDIF.
      ENDIF.

      ls_element-x = val-margin_left.
      ls_element-y = lv_top.
      CASE <element>-type.
        WHEN cs_type-line.
          " the rule of the flow lies in the middle of the space it takes
          ls_element-y = ( lv_top + lv_bottom ) / 2.
          ls_element-w = lv_width.
          ls_element-h = 0.
        WHEN cs_type-image.
          ls_element-h = lv_bottom - lv_top.
          IF ls_element-w <= 0.
            ls_element-w = lv_width.
          ENDIF.
        WHEN cs_type-barcode.
          ls_element-h = lv_bottom - lv_top.
          ls_element-w = barcode_width( element = <element>
                                        text    = COND #( WHEN lv_with_data = abap_true
                                                          THEN resolve( val  = <element>-text
                                                                        data = <data> )
                                                          ELSE <element>-text ) ).
        WHEN OTHERS.
          ls_element-w = lv_width.
          ls_element-h = lv_bottom - lv_top.
      ENDCASE.
      INSERT ls_element INTO TABLE result-t_element.
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
            INSERT VALUE #( path   = lv_column
                            kind   = cs_field_kind-column
                            parent = lv_path ) INTO TABLE result.
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
    sample_fill( EXPORTING row   = 0
                           depth = 0
                 CHANGING  node  = result ).

  ENDMETHOD.

  METHOD sample.

    sample_fill( EXPORTING row   = 0
                           depth = 0
                 CHANGING  node  = data ).

  ENDMETHOD.

  METHOD sample_fill.

    DATA lr_row    TYPE REF TO data.
    DATA lv_number TYPE i.

    FIELD-SYMBOLS <component> TYPE any.
    FIELD-SYMBOLS <rows>      TYPE ANY TABLE.
    FIELD-SYMBOLS <row>       TYPE any.

    " a structure in a structure in a table: deep enough for a form
    IF depth > 3.
      RETURN.
    ENDIF.

    LOOP AT component_names( node ) INTO DATA(lv_name).
      DATA(lr_component) = component_of( node = node
                                         name = lv_name ).
      IF lr_component IS NOT BOUND.
        CONTINUE.
      ENDIF.
      ASSIGN lr_component->* TO <component>.
      " what the class set itself is what the preview shows
      IF <component> IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CASE zabaputil_cl_util_context=>rtti_get_type_kind( <component> ).

        WHEN `u` OR `v`.
          sample_fill( EXPORTING row   = row
                                 depth = depth + 1
                       CHANGING  node  = <component> ).

        WHEN `h`.
          " three rows, numbered - enough to see a table as a table
          ASSIGN <component> TO <rows>.
          CREATE DATA lr_row LIKE LINE OF <rows>.
          ASSIGN lr_row->* TO <row>.
          DO 3 TIMES.
            lv_number = sy-index.
            CLEAR <row>.
            DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( <row> ).
            IF lv_kind = `u` OR lv_kind = `v`.
              sample_fill( EXPORTING row   = lv_number
                                     depth = depth + 1
                           CHANGING  node  = <row> ).
            ELSE.
              sample_value( EXPORTING name  = lv_name
                                      row   = lv_number
                            CHANGING  value = <row> ).
            ENDIF.
            " a row a unique key refuses is one row less to look at
            INSERT <row> INTO TABLE <rows>.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.
          ENDDO.

        WHEN OTHERS.
          sample_value( EXPORTING name  = lv_name
                                  row   = row
                        CHANGING  value = <component> ).

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD sample_value.

    DATA lv_count  TYPE i.
    DATA lv_amount TYPE p LENGTH 8 DECIMALS 2.

    lv_count = row.
    IF lv_count < 1.
      lv_count = 1.
    ENDIF.

    " a value that does not fit its field - a number in one digit - leaves
    " the field as it is
    TRY.
        CASE zabaputil_cl_util_context=>rtti_get_type_kind( value ).

          WHEN `g` OR `C`.
            " the name of the field: a preview shows what is printed where
            IF row > 0.
              value = |{ name } { row }|.
            ELSE.
              value = name.
            ENDIF.

          WHEN `N`.
            value = lv_count.

          WHEN `D`.
            value = `20260131`.

          WHEN `T`.
            value = `120000`.

          WHEN `I` OR `b` OR `s` OR `8`.
            value = lv_count * 10.

          WHEN `P` OR `F` OR `a` OR `e`.
            lv_amount = lv_count * 25.
            value = lv_amount / 2.

        ENDCASE.
      CATCH cx_sy_conversion_error cx_sy_arithmetic_error.
        CLEAR value.
    ENDTRY.

  ENDMETHOD.

  METHOD stringify.

    " XML, but neither written nor read by the identity transformation:
    " what that writes and accepts differs between a system and the
    " transpiled runtime (an initial component as <FILL/>, as <FILL> </FILL>
    " or not at all; bytes as base64 or as hex), and a form is designed on
    " one and pasted into the other. xml_of( ) and xml_read( ) are plain
    " string handling, the same everywhere. What is initial is not written:
    " that is more than half of a form, and it is initial again when the
    " text is read.
    result = |<{ c_root } version="1">{ xml_of( val ) }</{ c_root }>|.

  ENDMETHOD.

  METHOD xml_of.

    DATA lv_text TYPE string.

    FIELD-SYMBOLS <component> TYPE any.
    FIELD-SYMBOLS <rows>      TYPE ANY TABLE.
    FIELD-SYMBOLS <row>       TYPE any.

    DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( node ).
    CASE lv_kind.

      WHEN `u` OR `v`.
        LOOP AT component_names( node ) INTO DATA(lv_name).
          ASSIGN COMPONENT lv_name OF STRUCTURE node TO <component>.
          IF sy-subrc <> 0 OR <component> IS INITIAL.
            CONTINUE.
          ENDIF.
          result = |{ result }<{ lv_name }>{ xml_of( <component> ) }</{ lv_name }>|.
        ENDLOOP.

      WHEN `h`.
        ASSIGN node TO <rows>.
        LOOP AT <rows> ASSIGNING <row>.
          result = |{ result }<item>{ xml_of( <row> ) }</item>|.
        ENDLOOP.

      WHEN `X` OR `y`.
        " bytes - an image - as base64
        result = zabaputil_cl_util_context=>conv_encode_x_base64( node ).

      WHEN `r` OR `l`.
        " a reference is nothing a text can hold
        RETURN.

      WHEN OTHERS.
        lv_text = node.
        " a number moved to a text has its sign behind it: a blank, or a minus
        IF lv_kind CA `IPbs8Fae`.
          lv_text = condense( lv_text ).
          IF strlen( lv_text ) > 1 AND substring( val = lv_text
                                                  off = strlen( lv_text ) - 1 ) = `-`.
            lv_text = `-` && substring( val = lv_text
                                        len = strlen( lv_text ) - 1 ).
          ENDIF.
        ENDIF.
        lv_text = replace( val  = lv_text
                           sub  = `&`
                           with = `&amp;`
                           occ  = 0 ).
        lv_text = replace( val  = lv_text
                           sub  = `<`
                           with = `&lt;`
                           occ  = 0 ).
        lv_text = replace( val  = lv_text
                           sub  = `>`
                           with = `&gt;`
                           occ  = 0 ).
        " a line break as a character reference: the text is kept in ABAP
        " literals, which end at the end of a line
        lv_text = replace( val  = lv_text
                           sub  = |\r|
                           with = `&#13;`
                           occ  = 0 ).
        lv_text = replace( val  = lv_text
                           sub  = |\n|
                           with = `&#10;`
                           occ  = 0 ).
        result = replace( val  = lv_text
                          sub  = |\t|
                          with = `&#9;`
                          occ  = 0 ).

    ENDCASE.

  ENDMETHOD.

  METHOD parse.

    DATA lv_pos  TYPE i.
    DATA lv_name TYPE string.

    " a class that has no form yet returns nothing: an empty page to start on.
    " The text itself is read as it is - blanks in a row are a text's own;
    " what stands in front of the first tag is skipped by xml_open( )
    DATA(lv_text) = CONV string( val ).
    DATA(lv_content) = condense( lv_text ).
    IF lv_content IS INITIAL.
      result = new_form( ).
      RETURN.
    ENDIF.

    xml_open( EXPORTING xml  = lv_text
              IMPORTING name = lv_name
              CHANGING  pos  = lv_pos ).
    IF lv_name <> c_root.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: the text is no form - it starts with <{ lv_name }>, not with <{ c_root }>|.
    ENDIF.
    xml_read( EXPORTING xml  = lv_text
              CHANGING  pos  = lv_pos
                        node = result ).

  ENDMETHOD.

  METHOD xml_open.

    " at an opening tag: its name, and `pos` behind its `>`
    DATA(lv_start) = find( val = xml
                           sub = `<`
                           off = pos ).
    DATA(lv_end) = -1.
    IF lv_start >= 0.
      lv_end = find( val = xml
                     sub = `>`
                     off = lv_start ).
    ENDIF.
    IF lv_start < 0 OR lv_end < 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: the text is no form - no element at position { pos }|.
    ENDIF.

    name = substring( val = xml
                      off = lv_start + 1
                      len = lv_end - lv_start - 1 ).
    pos = lv_end + 1.
    " <NAME/> is an element without content: nothing to read, nothing to close
    empty = xsdbool( strlen( name ) > 0 AND substring( val = name
                                                         off = strlen( name ) - 1 ) = `/` ).
    IF empty = abap_true.
      name = substring( val = name
                        len = strlen( name ) - 1 ).
    ENDIF.
    " attributes - the version of the form - are not part of the name
    DATA(lv_blank) = find( val = name
                           sub = ` ` ).
    IF lv_blank >= 0.
      name = substring( val = name
                        len = lv_blank ).
    ENDIF.

  ENDMETHOD.

  METHOD xml_read.

    DATA lr_row   TYPE REF TO data.
    DATA lv_empty TYPE abap_bool.
    DATA lv_text  TYPE string.
    DATA lv_skip  TYPE string.
    DATA lv_name  TYPE string.

    FIELD-SYMBOLS <component> TYPE any.
    FIELD-SYMBOLS <rows>      TYPE ANY TABLE.
    FIELD-SYMBOLS <row>       TYPE any.

    " `pos` is behind an opening tag: read what the element holds into the
    " node, and leave `pos` behind the closing tag
    DATA(lv_kind) = zabaputil_cl_util_context=>rtti_get_type_kind( node ).
    IF lv_kind = `h`.
      ASSIGN node TO <rows>.
    ENDIF.

    DO.
      DATA(lv_next) = find( val = xml
                            sub = `<`
                            off = pos ).
      IF lv_next < 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: the text is no form - it ends inside an element (position { pos })|.
      ENDIF.

      " the text up to the next tag is the value of a field
      IF lv_kind <> `u` AND lv_kind <> `v` AND lv_kind <> `h` AND lv_next > pos.
        lv_text = lv_text && substring( val = xml
                                        off = pos
                                        len = lv_next - pos ).
      ENDIF.
      pos = lv_next.

      IF strlen( xml ) > pos + 1 AND substring( val = xml
                                               off = pos + 1
                                               len = 1 ) = `/`.
        " the closing tag of this element
        DATA(lv_close) = find( val = xml
                               sub = `>`
                               off = pos ).
        IF lv_close < 0.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING
              val = |PDF: the text is no form - a tag is not closed (position { pos })|.
        ENDIF.
        pos = lv_close + 1.
        EXIT.
      ENDIF.

      " an element inside this one
      xml_open( EXPORTING xml   = xml
                IMPORTING name  = lv_name
                          empty = lv_empty
                CHANGING  pos   = pos ).
      CASE lv_kind.

        WHEN `u` OR `v`.
          " a component - one the structure does not have (a form written
          " by a newer release) is read past
          ASSIGN COMPONENT lv_name OF STRUCTURE node TO <component>.
          IF sy-subrc <> 0.
            ASSIGN lv_skip TO <component>.
          ENDIF.
          IF lv_empty = abap_false.
            xml_read( EXPORTING xml  = xml
                      CHANGING  pos  = pos
                                node = <component> ).
          ENDIF.

        WHEN `h`.
          " a row
          CREATE DATA lr_row LIKE LINE OF <rows>.
          ASSIGN lr_row->* TO <row>.
          IF lv_empty = abap_false.
            xml_read( EXPORTING xml  = xml
                      CHANGING  pos  = pos
                                node = <row> ).
          ENDIF.
          INSERT <row> INTO TABLE <rows>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

        WHEN OTHERS.
          " an element where a value is expected: read past it
          IF lv_empty = abap_false.
            xml_read( EXPORTING xml  = xml
                      CHANGING  pos  = pos
                                node = lv_skip ).
          ENDIF.

      ENDCASE.
    ENDDO.

    CASE lv_kind.
      WHEN `u` OR `v` OR `h` OR `r` OR `l`.
        RETURN.
    ENDCASE.

    lv_text = replace( val  = lv_text
                       sub  = `&lt;`
                       with = `<`
                       occ  = 0 ).
    lv_text = replace( val  = lv_text
                       sub  = `&gt;`
                       with = `>`
                       occ  = 0 ).
    lv_text = replace( val  = lv_text
                       sub  = `&#13;`
                       with = |\r|
                       occ  = 0 ).
    lv_text = replace( val  = lv_text
                       sub  = `&#10;`
                       with = |\n|
                       occ  = 0 ).
    lv_text = replace( val  = lv_text
                       sub  = `&#9;`
                       with = |\t|
                       occ  = 0 ).
    lv_text = replace( val  = lv_text
                       sub  = `&amp;`
                       with = `&`
                       occ  = 0 ).
    TRY.
        IF lv_kind = `X` OR lv_kind = `y`.
          node = zabaputil_cl_util_context=>conv_decode_x_base64( lv_text ).
        ELSE.
          node = lv_text.
        ENDIF.
      CATCH cx_root INTO DATA(lx_value).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val      = |PDF: the text is no form - "{ lv_text }" is no value of its field|
            previous = lx_value.
    ENDTRY.

  ENDMETHOD.

  METHOD form_of.

    IF data IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: no data class to take the form from`.
    ENDIF.
    result = parse( data->get( ) ).
    result-data_class = zabaputil_cl_util_context=>rtti_get_classname_by_ref( data ).

  ENDMETHOD.

  METHOD print.

    result = render( val  = form_of( data )
                     data = data ).

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

  METHOD absolute_elements.

    " where a table ended: what stood below its bottom edge on the canvas
    " is on that page, moved by as much as the table grew or shrank
    TYPES:
      BEGIN OF ty_s_anchor,
        bottom TYPE f,
        page   TYPE i,
        y      TYPE f,
      END OF ty_s_anchor.

    DATA lt_anchor  TYPE STANDARD TABLE OF ty_s_anchor WITH EMPTY KEY.
    DATA ls_anchor  TYPE ty_s_anchor.
    DATA lt_element TYPE ty_t_element.
    DATA ls_element TYPE ty_s_element.
    DATA lv_page    TYPE i.
    DATA lv_shift   TYPE f.
    DATA lv_top     TYPE f.
    DATA lv_width   TYPE f.
    DATA lv_height  TYPE f.
    DATA lv_left    TYPE f.
    DATA lv_right   TYPE f.

    DATA(lo_first) = layout->page( ).
    lv_width = lo_first->width( ).
    lv_height = lo_first->height( ).

    " from the top of the canvas down: a table is printed before what
    " stands below it
    LOOP AT form-t_element INTO ls_element.
      IF ls_element-every_page = abap_true.
        CONTINUE.
      ENDIF.
      CASE ls_element-type.
        WHEN cs_type-text OR cs_type-table OR cs_type-line OR cs_type-rect OR cs_type-image OR cs_type-barcode.
          INSERT ls_element INTO TABLE lt_element.
      ENDCASE.
    ENDLOOP.
    SORT lt_element STABLE BY y ASCENDING.

    LOOP AT lt_element INTO ls_element.

      " the last table that ended above the element
      lv_top = ls_element-y.
      lv_page = 1.
      lv_shift = 0.
      LOOP AT lt_anchor INTO ls_anchor.
        IF ls_anchor-bottom <= lv_top.
          lv_page = ls_anchor-page.
          lv_shift = ls_anchor-y - ls_anchor-bottom.
        ENDIF.
      ENDLOOP.
      ls_element-y = lv_top + lv_shift.

      IF ls_element-type = cs_type-table.
        " the flow of the layout, between the edges of the table and from
        " its top edge on; it goes on on the last page of the document
        lv_left = ls_element-x.
        lv_right = form-margin_right.
        IF ls_element-w > 0.
          lv_right = lv_width - ls_element-x - ls_element-w.
        ELSEIF lv_left <= 0.
          lv_left = form-margin_left.
        ENDIF.
        layout->set_margins( left   = lv_left
                             top    = form-margin_top
                             right  = lv_right
                             bottom = form-margin_bottom ).
        layout->set_y( ls_element-y ).
        flow_table( pdf       = pdf
                    layout    = layout
                    element   = ls_element
                    data      = data
                    with_data = with_data ).
        layout->set_margins( left   = form-margin_left
                             top    = form-margin_top
                             right  = form-margin_right
                             bottom = form-margin_bottom ).
        CLEAR ls_anchor.
        ls_anchor-bottom = lv_top + ls_element-h.
        ls_anchor-page = pdf->page_count( ).
        ls_anchor-y = layout->y( ).
        INSERT ls_anchor INTO TABLE lt_anchor.
        CONTINUE.
      ENDIF.

      " an element a table pushed over the bottom margin starts the next
      " page, and what stood below it on the canvas follows it there
      IF lv_shift > 0 AND ls_element-y + ls_element-h > lv_height - form-margin_bottom.
        IF lv_page >= pdf->page_count( ).
          layout->page_break( ).
        ENDIF.
        lv_page = lv_page + 1.
        ls_element-y = form-margin_top.
        CLEAR ls_anchor.
        ls_anchor-bottom = lv_top.
        ls_anchor-page = lv_page.
        ls_anchor-y = form-margin_top.
        INSERT ls_anchor INTO TABLE lt_anchor.
      ENDIF.

      fixed_element( pdf       = pdf
                     layout    = layout
                     page      = pdf->page( lv_page )
                     element   = ls_element
                     data      = data
                     with_data = with_data ).

    ENDLOOP.

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

      WHEN cs_type-barcode.
        " at the left margin, where the flow is, as an image is
        lv_text = element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        lv_height = draw_barcode( pdf     = pdf
                                  page    = layout->page( )
                                  element = element
                                  text    = lv_text
                                  x       = left
                                  y       = layout->y( ) ).
        layout->set_y( layout->y( ) + lv_height ).

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

      WHEN cs_type-barcode.
        lv_text = element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        draw_barcode( pdf     = pdf
                      page    = page
                      element = element
                      text    = lv_text
                      x       = element-x
                      y       = element-y ).

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

  METHOD draw_barcode.

    DATA lv_bars TYPE f.
    DATA lv_text TYPE f.

    DATA(lv_modules) = zabaputil_cl_pdf_barcode=>code128( text ).
    DATA(lv_width) = barcode_width( element = element
                                    text    = text ).
    result = element-h.
    IF result <= 0.
      result = c_bar_height.
    ENDIF.

    " the text below the bars takes a line of its size from the height, and
    " a quarter of one more as the gap between them
    lv_bars = result.
    IF element-size > 0.
      lv_text = element-size * 3 / 2 / pdf->scale( ).
      lv_bars = result - lv_text.
      IF lv_bars < result / 2.
        lv_bars = result / 2.
      ENDIF.
    ENDIF.

    page->set_fill_color( element_color( element ) ).
    zabaputil_cl_pdf_barcode=>draw( page    = page
                                    modules = lv_modules
                                    x       = x
                                    y       = y
                                    w       = lv_width
                                    h       = lv_bars ).

    IF element-size > 0.
      DATA(lo_font) = element_font( pdf     = pdf
                                    element = element ).
      page->set_font( font = lo_font
                      size = element-size ).
      page->set_text_color( element_color( element ) ).
      page->text( x     = x + lv_width / 2
                  y     = y + lv_bars + ( element-size / 4 + lo_font->ascent( element-size ) ) / pdf->scale( )
                  val   = text
                  align = `C` ).
    ENDIF.

  ENDMETHOD.

  METHOD barcode_width.

    result = element-w.
    IF result <= 0.
      result = strlen( zabaputil_cl_pdf_barcode=>code128( text ) ) * c_module.
    ENDIF.

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

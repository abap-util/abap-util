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
    " ends, at the same distance. So does what stands below a text whose
    " lines need more than its height h.
    "
    " A text may name components of the data in braces: `Dear {NAME}`,
    " `{HEADER-CITY}` for a component of a component. A name the data does
    " not have stays as it is written, so a form shows its placeholders
    " when it is printed without data.
    "
    " After a colon a placeholder says how its value is shown to the user
    " who prints it - in the user's date and number format:
    "   {BSTDK:date}  {UZEIT:time}  {RATE:number}  {RATE:number:2}
    "   {NETWR:amount:WAERS}    the amount with the decimals and the code
    "                           of the currency in the component WAERS
    "   {MENGE:quantity:MEINS}  the quantity with the decimals and the
    "                           unit in the component MEINS
    " Instead of a component the currency or the unit may be written as it
    " is: {NETWR:amount:EUR}. A placeholder without a format prints the
    " value as ABAP writes it into a string; one with a format this class
    " does not know stays as it is written.
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
        "! a barcode of the text: in the flow at the left margin, w wide and h
        "! high; at a fixed position in the box x, y, w, h
        barcode    TYPE string VALUE `BARCODE`,
        "! the elements of the block of its name, once per row of a table of
        "! the data (source): in the flow one after the other, in an absolute
        "! form every h mm from its y on
        repeat     TYPE string VALUE `REPEAT`,
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
        "! L, C or R; J for a text justified at its width (in the flow: the margins)
        align      TYPE string,
        "! the colour a rectangle is filled with, RRGGBB; empty for none
        fill       TYPE string,
        "! the component of the data: the rows of a table, the bytes of an image
        source     TYPE string,
        "! the columns of a table
        t_column   TYPE ty_t_column,
        "! the bytes of an image that belongs to the form (a logo)
        image      TYPE xstring,
        "! printed only when this component of the data is filled - a table
        "! when it has rows; `!NAME` only when it is empty
        condition  TYPE string,
        "! the name of the REPEAT element the element belongs to: printed
        "! once per row, its placeholders name the components of the row
        "! (and of the data, when the row has no component of that name);
        "! in an absolute form its y is from the top of the block
        block      TYPE string,
        "! the text has markup: <b>bold</b>, <i>italic</i>, <u>underlined</u>,
        "! <color=CC0000>red</color> - of the font of the element, its cuts
        markup     TYPE abap_bool,
        "! at a fixed position: turned counter-clockwise around x, y, in
        "! degrees - a watermark across the page
        angle      TYPE ty_length,
        "! at a fixed position: how opaque, between 0 and 1; 0 for opaque
        opacity    TYPE ty_length,
        "! the radius of the corners of a rectangle
        radius     TYPE ty_length,
        "! a text is an entry of the outline of the document at this level
        "! (1 the top level) - a heading; 0 for none
        bookmark   TYPE i,
        "! of a barcode: CODE128, EAN13, QR or DATAMATRIX
        "! (zabaputil_cl_pdf_barcode=>cs_type); its text is the content
        symbology  TYPE string,
        "! of a table: the components that are added up (a total row, a
        "! subtotal row per group, a carry-over at a page break), zebra
        "! stripes and the lines of the cells
        options    TYPE zabaputil_cl_pdf_layout=>ty_s_table_options,
      END OF ty_s_element.
    TYPES ty_t_element TYPE STANDARD TABLE OF ty_s_element WITH EMPTY KEY.

    "! A header or a footer in parts: texts at the left margin, in the
    "! middle and at the right margin - a line feed starts another line,
    "! {page} and {pages} are the page numbers - and an image (a logo)
    TYPES:
      BEGIN OF ty_s_area,
        left        TYPE string,
        center      TYPE string,
        right       TYPE string,
        "! the font size in points; 0 for 8
        size        TYPE ty_length,
        "! a JPEG or PNG file
        image       TYPE xstring,
        "! L, C or R
        image_align TYPE string,
        "! the width of the image in mm; 0 for what its height makes of it
        image_w     TYPE ty_length,
        "! the height of the image in mm; 0 for what its width makes of it
        image_h     TYPE ty_length,
      END OF ty_s_area.

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
        "! the header in parts, of several lines, with an image - instead of header
        header_area   TYPE ty_s_area,
        "! the footer in parts - instead of footer
        footer_area   TYPE ty_s_area,
        "! the first page has header_first and footer_first instead - an
        "! empty one for none, a letter's first page has a letterhead of its own
        first_page    TYPE abap_bool,
        header_first  TYPE ty_s_area,
        footer_first  TYPE ty_s_area,
        "! the class whose public attributes the form prints - it implements
        "! zabaputil_if_pdf_data; empty for a form that is printed from a structure
        data_class    TYPE string,
        "! every element at its x, y, w, h instead of in the flow; what stands
        "! below a table follows where the table ends
        absolute      TYPE abap_bool,
        "! space before and after a paragraph of a text in the flow, in mm
        space_before  TYPE ty_length,
        space_after   TYPE ty_length,
        "! the fewest lines of a paragraph in the flow at the bottom of a page
        "! and at the top of the next one; 0 or 1 for any
        keep_lines    TYPE i,
        "! a TrueType file (.ttf) for the characters the standard fonts do not
        "! have - Polish, Greek, Cyrillic, ...; without it they are question marks
        fallback_font TYPE xstring,
        "! a PDF file whose first page is under every page - a letterhead
        background       TYPE xstring,
        "! a PDF file whose first page is under the first page instead
        background_first TYPE xstring,
        "! write PDF/A-3b, for archiving: every text in the fallback font, as
        "! PDF/A embeds every font - the standard fonts are not
        pdfa          TYPE abap_bool,
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

    CONSTANTS:
      BEGIN OF cs_part,
        "! a placeholder in the text of an element
        text   TYPE string VALUE `TEXT`,
        "! the source of a table or an image
        source TYPE string VALUE `SOURCE`,
        "! a column of a table: no component of its rows
        column TYPE string VALUE `COLUMN`,
        "! a placeholder in the header line
        header TYPE string VALUE `HEADER`,
        "! a placeholder in the footer line
        footer TYPE string VALUE `FOOTER`,
        "! the condition of an element
        condition TYPE string VALUE `CONDITION`,
        "! an element of a block no REPEAT element has the name of
        block  TYPE string VALUE `BLOCK`,
      END OF cs_part.

    TYPES:
      BEGIN OF ty_s_unknown,
        "! what the form names, as it is written: NAME, NETWR:amount:WAERS, T_ITEMS
        name    TYPE string,
        "! one of cs_part
        part    TYPE string,
        "! the index of the element in t_element; 0 for the header and the footer
        element TYPE i,
        "! the name of the element, as an editor shows it
        label   TYPE string,
      END OF ty_s_unknown.
    TYPES ty_t_unknown TYPE STANDARD TABLE OF ty_s_unknown WITH EMPTY KEY.

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

    "! What a form names that the data does not have: placeholders, the
    "! sources of tables and images, the columns of tables. A placeholder
    "! with a format this class does not know is reported as well; {page}
    "! and {pages} in the header and the footer are not
    "! @parameter val  | the form
    "! @parameter data | a structure or an object, as for render( )
    CLASS-METHODS check
      IMPORTING
        val           TYPE ty_s_form
        data          TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_unknown.

    "! The placeholders of a text, as they are written between the braces
    CLASS-METHODS placeholders
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

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
    " while a PDF/A form is built: the one font of all its texts
    CLASS-DATA go_pdfa_font TYPE REF TO zabaputil_if_pdf_font.
    CONSTANTS c_color TYPE string VALUE `000000`.
    CONSTANTS c_size  TYPE f VALUE 10.

    "! The value of a component of the data as text
    "! @parameter path   | NAME, or HEADER-NAME for a component of a component
    "! @parameter format | empty, or date, time, number, amount, quantity
    "! @parameter option | the decimals of a number, the currency of an
    "!                     amount, the unit of a quantity
    "! @parameter found  | the data has it, it is a single value, and the
    "!                     format is one this class knows
    CLASS-METHODS component_text
      IMPORTING
        path   TYPE string
        data   TYPE any
        format TYPE string OPTIONAL
        option TYPE string OPTIONAL
      EXPORTING
        found  TYPE abap_bool
        text   TYPE string.

    "! The placeholders of a text that neither the data nor the outer
    "! data (of a block: the data of the form) has
    CLASS-METHODS check_text
      IMPORTING
        text    TYPE string
        data    TYPE any
        outer   TYPE any
        part    TYPE string
        element TYPE i
        label   TYPE string
      CHANGING
        result  TYPE ty_t_unknown.

    "! What one element names that the data does not have
    CLASS-METHODS check_element
      IMPORTING
        element TYPE ty_s_element
        index   TYPE i
        data    TYPE any
        outer   TYPE any
      CHANGING
        result  TYPE ty_t_unknown.

    "! A component of the data by its path: NAME, HEAD-CITY; initial when
    "! the data has none
    CLASS-METHODS component_ref
      IMPORTING
        data          TYPE any
        path          TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! The rows of a table element or a REPEAT element: the table the
    "! data has under the name of its source; initial when it has none
    CLASS-METHODS rows_of
      IMPORTING
        element       TYPE ty_s_element
        data          TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! The condition of an element holds - always without data, so that a
    "! form printed without data shows all of it
    CLASS-METHODS is_shown
      IMPORTING
        element       TYPE ty_s_element
        data          TYPE any
        outer         TYPE any
        with_data     TYPE abap_bool
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! An element of a block with its text resolved: first with the row,
    "! then with the data of the form
    CLASS-METHODS block_element
      IMPORTING
        element       TYPE ty_s_element
        row           TYPE any
        data          TYPE any
      RETURNING
        VALUE(result) TYPE ty_s_element.

    "! The elements of a block, in the order of the form
    CLASS-METHODS block_elements
      IMPORTING
        form          TYPE ty_s_form
        name          TYPE string
      RETURNING
        VALUE(result) TYPE ty_t_element.

    "! A REPEAT element in the flow: its block once per row
    CLASS-METHODS flow_repeat
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        form      TYPE ty_s_form
        element   TYPE ty_s_element
        data      TYPE any
        with_data TYPE abap_bool.

    "! A REPEAT element of an absolute form: its block every h mm, on the
    "! next page at the top margin when it does not fit above the bottom one
    "! @parameter page     | the page it starts on
    "! @parameter end_page | the page its last block is on
    "! @parameter end_y    | where its last block ends
    CLASS-METHODS absolute_repeat
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        form      TYPE ty_s_form
        element   TYPE ty_s_element
        page      TYPE i
        data      TYPE any
        with_data TYPE abap_bool
      EXPORTING
        end_page  TYPE i
        end_y     TYPE f.

    "! The data has a component of that path: NAME, HEAD-CITY
    CLASS-METHODS has_component
      IMPORTING
        data          TYPE any
        name          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! A header or a footer of the form for the layout, its placeholders
    "! resolved
    CLASS-METHODS layout_area
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        area          TYPE ty_s_area
        data          TYPE any
        with_data     TYPE abap_bool
      RETURNING
        VALUE(result) TYPE zabaputil_cl_pdf_layout=>ty_s_area.

    "! The value is a number of exactly that many digits
    CLASS-METHODS is_digits
      IMPORTING
        val           TYPE any
        len           TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! The value a format option names: a component of the data, or the
    "! option as it is written (a currency code, a unit)
    CLASS-METHODS option_value
      IMPORTING
        option        TYPE string
        data          TYPE any
      RETURNING
        VALUE(result) TYPE string.

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

    CLASS-METHODS fixed_text
      IMPORTING
        pdf       TYPE REF TO zabaputil_cl_pdf
        layout    TYPE REF TO zabaputil_cl_pdf_layout
        page      TYPE REF TO zabaputil_cl_pdf_page
        element   TYPE ty_s_element
        text      TYPE string.

    "! The lines of a fixed text: its paragraphs, wrapped at its width
    CLASS-METHODS text_lines
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        layout        TYPE REF TO zabaputil_cl_pdf_layout
        element       TYPE ty_s_element
        text          TYPE string
      RETURNING
        VALUE(result) TYPE string_table.

    "! The distance of two lines of a text, in mm
    CLASS-METHODS line_height
      IMPORTING
        pdf           TYPE REF TO zabaputil_cl_pdf
        element       TYPE ty_s_element
      RETURNING
        VALUE(result) TYPE f.

    "! Lines of a text, the first one with its top at y
    CLASS-METHODS text_draw
      IMPORTING
        pdf     TYPE REF TO zabaputil_cl_pdf
        layout  TYPE REF TO zabaputil_cl_pdf_layout
        page    TYPE REF TO zabaputil_cl_pdf_page
        element TYPE ty_s_element
        lines   TYPE string_table
        y       TYPE f.

    "! A text of an absolute form that is higher than it is drawn: its
    "! lines go on on the next page at the top margin
    "! @parameter page     | the page the text starts on
    "! @parameter end_page | the page it ends on
    "! @parameter end_y    | where its last line ends
    CLASS-METHODS absolute_text
      IMPORTING
        pdf      TYPE REF TO zabaputil_cl_pdf
        layout   TYPE REF TO zabaputil_cl_pdf_layout
        form     TYPE ty_s_form
        element  TYPE ty_s_element
        lines    TYPE string_table
        page     TYPE i
      EXPORTING
        end_page TYPE i
        end_y    TYPE f.

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
    CLEAR go_pdfa_font.
    IF val-pdfa = abap_true.
      IF val-fallback_font IS INITIAL.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF/A: the form needs a fallback_font - the standard fonts are not embedded`.
      ENDIF.
      result->set_pdfa( ).
      go_pdfa_font = result->font_ttf( val-fallback_font ).
    ELSEIF val-fallback_font IS NOT INITIAL.
      result->set_fallback_font( result->font_ttf( val-fallback_font ) ).
    ENDIF.

    DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( result ).
    lo_layout->set_margins( left   = val-margin_left
                            top    = val-margin_top
                            right  = val-margin_right
                            bottom = val-margin_bottom ).
    lo_layout->set_font( font = COND #( WHEN go_pdfa_font IS BOUND THEN go_pdfa_font ELSE result->font( c_font ) )
                         size = c_size ).
    IF val-space_before > 0 OR val-space_after > 0.
      lo_layout->set_paragraph_spacing( before = val-space_before
                                        after  = val-space_after ).
    ENDIF.
    IF val-keep_lines > 1.
      lo_layout->set_keep_lines( min = val-keep_lines ).
    ENDIF.

    IF val-absolute = abap_true.
      absolute_elements( pdf       = result
                         layout    = lo_layout
                         form      = val
                         data      = <data>
                         with_data = lv_with_data ).
    ELSE.
      LOOP AT val-t_element ASSIGNING FIELD-SYMBOL(<element>).
        IF <element>-fixed = abap_true OR <element>-type = cs_type-rect OR <element>-block IS NOT INITIAL.
          CONTINUE.
        ENDIF.
        IF is_shown( element   = <element>
                     data      = <data>
                     outer     = <data>
                     with_data = lv_with_data ) = abap_false.
          CONTINUE.
        ENDIF.
        IF <element>-type = cs_type-repeat.
          flow_repeat( pdf       = result
                       layout    = lo_layout
                       form      = val
                       element   = <element>
                       data      = <data>
                       with_data = lv_with_data ).
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
    lo_layout->set_font( font = COND #( WHEN go_pdfa_font IS BOUND THEN go_pdfa_font ELSE result->font( c_font ) )
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
    IF val-header_area IS NOT INITIAL.
      lo_layout->set_header_area( layout_area( pdf       = result
                                               area      = val-header_area
                                               data      = <data>
                                               with_data = lv_with_data ) ).
    ENDIF.
    IF val-footer_area IS NOT INITIAL.
      lo_layout->set_footer_area( layout_area( pdf       = result
                                               area      = val-footer_area
                                               data      = <data>
                                               with_data = lv_with_data ) ).
    ENDIF.
    IF val-first_page = abap_true.
      lo_layout->set_header_area( val        = layout_area( pdf       = result
                                                            area      = val-header_first
                                                            data      = <data>
                                                            with_data = lv_with_data )
                                  first_page = abap_true ).
      lo_layout->set_footer_area( val        = layout_area( pdf       = result
                                                            area      = val-footer_first
                                                            data      = <data>
                                                            with_data = lv_with_data )
                                  first_page = abap_true ).
    ENDIF.
    lo_layout->finish( ).

    " the letterhead under everything, once in the file however many pages
    IF val-background IS NOT INITIAL OR val-background_first IS NOT INITIAL.
      DATA lo_background TYPE REF TO zabaputil_cl_pdf_reader.
      DATA lo_first      TYPE REF TO zabaputil_cl_pdf_reader.
      IF val-background IS NOT INITIAL.
        lo_background = result->import_page( val-background ).
      ENDIF.
      IF val-background_first IS NOT INITIAL.
        lo_first = result->import_page( val-background_first ).
      ENDIF.
      lv_number = 1.
      WHILE lv_number <= result->page_count( ).
        IF lv_number = 1 AND lo_first IS BOUND.
          result->page( lv_number )->background( lo_first ).
        ELSEIF lo_background IS BOUND.
          result->page( lv_number )->background( lo_background ).
        ENDIF.
        lv_number = lv_number + 1.
      ENDWHILE.
    ENDIF.

    " the fixed elements go over the flow, on the first or on every page.
    " Of an absolute form the ones of every page are left: the others are
    " on the page the tables above them left them on
    LOOP AT val-t_element ASSIGNING <element>.
      IF <element>-block IS NOT INITIAL OR <element>-type = cs_type-repeat
          OR is_shown( element   = <element>
                       data      = <data>
                       outer     = <data>
                       with_data = lv_with_data ) = abap_false.
        CONTINUE.
      ENDIF.
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

  METHOD placeholders.

    DATA lv_rest  TYPE string.
    DATA lv_open  TYPE i.
    DATA lv_close TYPE i.

    lv_rest = val.
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
      INSERT substring( val = lv_rest
                        off = lv_open + 1
                        len = lv_close - lv_open - 1 ) INTO TABLE result.
      lv_rest = substring( val = lv_rest
                           off = lv_close + 1 ).
    ENDDO.

  ENDMETHOD.

  METHOD check.

    DATA lv_index TYPE i.
    DATA lr_rows  TYPE REF TO data.
    DATA lr_line  TYPE REF TO data.

    FIELD-SYMBOLS <rows>   TYPE ANY TABLE.
    FIELD-SYMBOLS <line>   TYPE any.
    FIELD-SYMBOLS <repeat> TYPE ty_s_element.

    check_text( EXPORTING text    = val-header
                          data    = data
                          outer   = data
                          part    = cs_part-header
                          element = 0
                          label   = ``
                CHANGING  result  = result ).
    check_text( EXPORTING text    = val-footer
                          data    = data
                          outer   = data
                          part    = cs_part-footer
                          element = 0
                          label   = ``
                CHANGING  result  = result ).

    " the parts of the header and footer areas
    DATA(lt_area) = VALUE string_table( ( val-header_area-left ) ( val-header_area-center ) ( val-header_area-right )
                                        ( val-header_first-left ) ( val-header_first-center ) ( val-header_first-right ) ).
    LOOP AT lt_area INTO DATA(lv_area).
      check_text( EXPORTING text    = lv_area
                            data    = data
                            outer   = data
                            part    = cs_part-header
                            element = 0
                            label   = ``
                  CHANGING  result  = result ).
    ENDLOOP.
    lt_area = VALUE #( ( val-footer_area-left ) ( val-footer_area-center ) ( val-footer_area-right )
                       ( val-footer_first-left ) ( val-footer_first-center ) ( val-footer_first-right ) ).
    LOOP AT lt_area INTO lv_area.
      check_text( EXPORTING text    = lv_area
                            data    = data
                            outer   = data
                            part    = cs_part-footer
                            element = 0
                            label   = ``
                  CHANGING  result  = result ).
    ENDLOOP.

    LOOP AT val-t_element ASSIGNING FIELD-SYMBOL(<element>).
      lv_index = sy-tabix.

      IF <element>-block IS INITIAL.
        check_element( EXPORTING element = <element>
                                 index   = lv_index
                                 data    = data
                                 outer   = data
                       CHANGING  result  = result ).
        CONTINUE.
      ENDIF.

      " an element of a block: its placeholders name a row of the table
      " of its REPEAT element, also when the table has no rows
      UNASSIGN <repeat>.
      LOOP AT val-t_element ASSIGNING <repeat>
          WHERE type = cs_type-repeat.
        IF to_upper( condense( <repeat>-name ) ) = to_upper( condense( <element>-block ) ).
          EXIT.
        ENDIF.
        UNASSIGN <repeat>.
      ENDLOOP.
      IF <repeat> IS NOT ASSIGNED.
        INSERT VALUE #( name    = <element>-block
                        part    = cs_part-block
                        element = lv_index
                        label   = <element>-name ) INTO TABLE result.
        CONTINUE.
      ENDIF.
      CLEAR lr_line.
      lr_rows = rows_of( element = <repeat>
                         data    = data ).
      IF lr_rows IS BOUND.
        ASSIGN lr_rows->* TO <rows>.
        CREATE DATA lr_line LIKE LINE OF <rows>.
      ENDIF.
      IF lr_line IS NOT BOUND.
        " the REPEAT element reports its source
        CONTINUE.
      ENDIF.
      ASSIGN lr_line->* TO <line>.
      check_element( EXPORTING element = <element>
                               index   = lv_index
                               data    = <line>
                               outer   = data
                     CHANGING  result  = result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD check_element.

    DATA lt_name TYPE string_table.

    FIELD-SYMBOLS <rows> TYPE any.

    IF element-condition IS NOT INITIAL.
      DATA(lv_condition) = condense( element-condition ).
      IF strlen( lv_condition ) > 0 AND lv_condition(1) = `!`.
        lv_condition = condense( substring( val = lv_condition
                                            off = 1 ) ).
      ENDIF.
      IF component_ref( data = data
                        path = lv_condition ) IS NOT BOUND AND component_ref( data = outer
                                                                             path = lv_condition ) IS NOT BOUND.
        INSERT VALUE #( name    = element-condition
                        part    = cs_part-condition
                        element = index
                        label   = element-name ) INTO TABLE result.
      ENDIF.
    ENDIF.

    CASE element-type.

      WHEN cs_type-text OR cs_type-barcode.
        check_text( EXPORTING text    = element-text
                              data    = data
                              outer   = outer
                              part    = cs_part-text
                              element = index
                              label   = element-name
                    CHANGING  result  = result ).

      WHEN cs_type-table OR cs_type-repeat.
        DATA(lr_rows) = rows_of( element = element
                                 data    = data ).
        IF lr_rows IS NOT BOUND.
          INSERT VALUE #( name    = element-source
                          part    = cs_part-source
                          element = index
                          label   = element-name ) INTO TABLE result.
          RETURN.
        ENDIF.
        " the columns: components of the rows, also of a table without rows
        ASSIGN lr_rows->* TO <rows>.
        lt_name = component_names( <rows> ).
        LOOP AT element-t_column ASSIGNING FIELD-SYMBOL(<column>).
          IF NOT line_exists( lt_name[ table_line = to_upper( condense( <column>-name ) ) ] ).
            INSERT VALUE #( name    = <column>-name
                            part    = cs_part-column
                            element = index
                            label   = element-name ) INTO TABLE result.
          ENDIF.
        ENDLOOP.

      WHEN cs_type-image.
        " an image the form carries needs no source
        IF element-source IS NOT INITIAL AND component_of( node = data
                                                           name = to_upper( element-source ) ) IS NOT BOUND.
          INSERT VALUE #( name    = element-source
                          part    = cs_part-source
                          element = index
                          label   = element-name ) INTO TABLE result.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD component_ref.

    FIELD-SYMBOLS <node> TYPE any.

    DATA(lv_path) = to_upper( condense( path ) ).
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
      result = component_of( node = <node>
                             name = lv_part ).
      IF result IS NOT BOUND.
        RETURN.
      ENDIF.
      ASSIGN result->* TO <node>.
    ENDLOOP.

  ENDMETHOD.

  METHOD rows_of.

    FIELD-SYMBOLS <component> TYPE any.

    IF element-source IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lr_component) = component_of( node = data
                                       name = to_upper( element-source ) ).
    IF lr_component IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_component->* TO <component>.
    IF zabaputil_cl_util_context=>rtti_get_type_kind( <component> ) = `h`.
      result = lr_component.
    ENDIF.

  ENDMETHOD.

  METHOD is_shown.

    DATA lv_negate TYPE abap_bool.
    DATA lv_filled TYPE abap_bool.

    FIELD-SYMBOLS <value> TYPE any.

    result = abap_true.
    DATA(lv_condition) = condense( element-condition ).
    IF lv_condition IS INITIAL OR with_data = abap_false.
      RETURN.
    ENDIF.
    IF lv_condition(1) = `!`.
      lv_negate = abap_true.
      lv_condition = condense( substring( val = lv_condition
                                          off = 1 ) ).
    ENDIF.

    DATA(lr_value) = component_ref( data = data
                                    path = lv_condition ).
    IF lr_value IS NOT BOUND.
      lr_value = component_ref( data = outer
                                path = lv_condition ).
    ENDIF.
    IF lr_value IS BOUND.
      ASSIGN lr_value->* TO <value>.
      lv_filled = xsdbool( <value> IS NOT INITIAL ).
    ENDIF.

    IF lv_negate = abap_true.
      result = xsdbool( lv_filled = abap_false ).
    ELSE.
      result = lv_filled.
    ENDIF.

  ENDMETHOD.

  METHOD block_element.

    result = element.
    result-text = resolve( val  = resolve( val  = element-text
                                           data = row )
                           data = data ).

  ENDMETHOD.

  METHOD block_elements.

    DATA(lv_name) = to_upper( condense( name ) ).
    IF lv_name IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT form-t_element ASSIGNING FIELD-SYMBOL(<element>).
      IF to_upper( condense( <element>-block ) ) = lv_name AND <element>-type <> cs_type-repeat.
        INSERT <element> INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD flow_repeat.

    FIELD-SYMBOLS <rows> TYPE ANY TABLE.

    DATA(lt_block) = block_elements( form = form
                                     name = element-name ).

    DATA(lr_rows) = rows_of( element = element
                             data    = data ).
    IF with_data = abap_false OR lr_rows IS NOT BOUND.
      " printed without its rows: the block once, with its placeholders
      LOOP AT lt_block ASSIGNING FIELD-SYMBOL(<element>) WHERE fixed = abap_false.
        flow_element( pdf       = pdf
                      layout    = layout
                      element   = <element>
                      left      = form-margin_left
                      data      = data
                      with_data = abap_false ).
      ENDLOOP.
      RETURN.
    ENDIF.

    ASSIGN lr_rows->* TO <rows>.
    LOOP AT <rows> ASSIGNING FIELD-SYMBOL(<row>).
      LOOP AT lt_block ASSIGNING <element> WHERE fixed = abap_false.
        IF is_shown( element   = <element>
                     data      = <row>
                     outer     = data
                     with_data = abap_true ) = abap_false.
          CONTINUE.
        ENDIF.
        flow_element( pdf       = pdf
                      layout    = layout
                      element   = block_element( element = <element>
                                                 row     = <row>
                                                 data    = data )
                      left      = form-margin_left
                      data      = <row>
                      with_data = abap_true ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD absolute_repeat.

    DATA lv_y      TYPE f.
    DATA lv_height TYPE f.
    DATA lv_bottom TYPE f.
    DATA ls_draw   TYPE ty_s_element.

    FIELD-SYMBOLS <rows> TYPE ANY TABLE.

    DATA(lt_block) = block_elements( form = form
                                     name = element-name ).

    " the height of one block: as drawn, or what its elements need
    lv_height = element-h.
    IF lv_height <= 0.
      LOOP AT lt_block ASSIGNING FIELD-SYMBOL(<element>).
        IF <element>-y + <element>-h > lv_height.
          lv_height = <element>-y + <element>-h.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF lv_height <= 0.
      lv_height = 1.
    ENDIF.

    lv_bottom = layout->page( )->height( ) - form-margin_bottom.
    end_page = page.
    lv_y = element-y.

    DATA(lr_rows) = rows_of( element = element
                             data    = data ).
    IF with_data = abap_false OR lr_rows IS NOT BOUND.
      " printed without its rows: the block once, with its placeholders
      LOOP AT lt_block ASSIGNING <element>.
        ls_draw = <element>.
        ls_draw-y = lv_y + <element>-y.
        fixed_element( pdf       = pdf
                       layout    = layout
                       page      = pdf->page( end_page )
                       element   = ls_draw
                       data      = data
                       with_data = abap_false ).
      ENDLOOP.
      end_y = lv_y + lv_height.
      RETURN.
    ENDIF.

    ASSIGN lr_rows->* TO <rows>.
    LOOP AT <rows> ASSIGNING FIELD-SYMBOL(<row>).
      " a block that does not fit above the bottom margin starts the next
      " page - unless it is the first one there
      IF lv_y + lv_height > lv_bottom AND lv_y > form-margin_top.
        IF end_page >= pdf->page_count( ).
          layout->page_break( ).
        ENDIF.
        end_page = end_page + 1.
        lv_y = form-margin_top.
      ENDIF.
      LOOP AT lt_block ASSIGNING <element>.
        IF is_shown( element   = <element>
                     data      = <row>
                     outer     = data
                     with_data = abap_true ) = abap_false.
          CONTINUE.
        ENDIF.
        ls_draw = block_element( element = <element>
                                 row     = <row>
                                 data    = data ).
        ls_draw-y = lv_y + <element>-y.
        fixed_element( pdf       = pdf
                       layout    = layout
                       page      = pdf->page( end_page )
                       element   = ls_draw
                       data      = <row>
                       with_data = abap_true ).
      ENDLOOP.
      lv_y = lv_y + lv_height.
    ENDLOOP.
    end_y = lv_y.

  ENDMETHOD.

  METHOD check_text.

    DATA lv_path   TYPE string.
    DATA lv_format TYPE string.
    DATA lv_option TYPE string.
    DATA lv_found  TYPE abap_bool.

    LOOP AT placeholders( text ) INTO DATA(lv_name).
      " the page numbers are the layout's, in the header and the footer
      IF ( part = cs_part-header OR part = cs_part-footer )
          AND ( to_lower( condense( lv_name ) ) = `page` OR to_lower( condense( lv_name ) ) = `pages` ).
        CONTINUE.
      ENDIF.
      CLEAR lv_path.
      CLEAR lv_format.
      CLEAR lv_option.
      SPLIT lv_name AT `:` INTO lv_path lv_format lv_option.
      component_text( EXPORTING path   = lv_path
                                data   = data
                                format = lv_format
                                option = lv_option
                      IMPORTING found  = lv_found ).
      IF lv_found = abap_false.
        component_text( EXPORTING path   = lv_path
                                  data   = outer
                                  format = lv_format
                                  option = lv_option
                        IMPORTING found  = lv_found ).
      ENDIF.
      IF lv_found = abap_true.
        CONTINUE.
      ENDIF.
      " a value that does not fit its format is no unknown name - a date
      " field that is empty in this print is one all the same
      lv_format = to_lower( condense( lv_format ) ).
      IF ( lv_format = `date` OR lv_format = `time` OR lv_format = `number`
          OR lv_format = `amount` OR lv_format = `quantity` )
          AND ( has_component( data = data
                               name = lv_path ) = abap_true OR has_component( data = outer
                                                                              name = lv_path ) = abap_true ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( name    = lv_name
                      part    = part
                      element = element
                      label   = label ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD has_component.

    result = xsdbool( component_ref( data = data
                                     path = name ) IS BOUND ).

  ENDMETHOD.

  METHOD resolve.

    DATA lv_rest  TYPE string.
    DATA lv_open  TYPE i.
    DATA lv_close TYPE i.
    DATA lv_name   TYPE string.
    DATA lv_path   TYPE string.
    DATA lv_format TYPE string.
    DATA lv_option TYPE string.
    DATA lv_text   TYPE string.
    DATA lv_found  TYPE abap_bool.

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
      CLEAR lv_path.
      CLEAR lv_format.
      CLEAR lv_option.
      SPLIT lv_name AT `:` INTO lv_path lv_format lv_option.
      component_text( EXPORTING path   = lv_path
                                data   = data
                                format = lv_format
                                option = lv_option
                      IMPORTING found  = lv_found
                                text   = lv_text ).
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

    DATA(lv_format) = to_lower( condense( format ) ).
    CASE lv_format.

      WHEN ``.
        text = |{ <node> }|.

      WHEN `date`.
        " a date in a text or a number field is a date all the same, if
        " it is one: eight digits
        DATA lv_date TYPE d.
        IF lv_kind <> `D` AND <node> IS NOT INITIAL AND is_digits( val = <node>
                                                                   len = 8 ) = abap_false.
          RETURN.
        ENDIF.
        lv_date = <node>.
        text = zabaputil_cl_util_context=>conv_get_text_by_user( lv_date ).

      WHEN `time`.
        DATA lv_time TYPE t.
        IF lv_kind <> `T` AND is_digits( val = <node>
                                         len = 6 ) = abap_false.
          RETURN.
        ENDIF.
        lv_time = <node>.
        text = zabaputil_cl_util_context=>conv_get_text_by_user( lv_time ).

      WHEN `number`.
        DATA lv_decimals TYPE i.
        lv_decimals = -1.
        IF option IS NOT INITIAL.
          TRY.
              lv_decimals = option.
            CATCH cx_root.
              RETURN.
          ENDTRY.
        ENDIF.
        text = zabaputil_cl_util_context=>conv_get_text_by_user( val      = <node>
                                                                 decimals = lv_decimals ).

      WHEN `amount`.
        DATA(lv_currency) = option_value( option = option
                                          data   = data ).
        text = zabaputil_cl_util_context=>conv_get_text_by_user( val      = <node>
                                                                 currency = lv_currency ).
        IF lv_currency IS NOT INITIAL AND text IS NOT INITIAL.
          text = |{ text } { lv_currency }|.
        ENDIF.

      WHEN `quantity`.
        DATA(lv_unit) = option_value( option = option
                                      data   = data ).
        text = zabaputil_cl_util_context=>conv_get_text_by_user( val  = <node>
                                                                 unit = lv_unit ).
        IF lv_unit IS NOT INITIAL AND text IS NOT INITIAL.
          text = |{ text } { lv_unit }|.
        ENDIF.

      WHEN OTHERS.
        RETURN.

    ENDCASE.
    found = abap_true.

  ENDMETHOD.

  METHOD layout_area.

    result-left = area-left.
    result-center = area-center.
    result-right = area-right.
    IF with_data = abap_true.
      result-left = resolve( val  = area-left
                             data = data ).
      result-center = resolve( val  = area-center
                               data = data ).
      result-right = resolve( val  = area-right
                              data = data ).
    ENDIF.
    result-size = area-size.
    result-image_align = area-image_align.
    result-image_w = area-image_w.
    result-image_h = area-image_h.
    IF area-image IS NOT INITIAL.
      result-image = pdf->image( area-image ).
    ENDIF.

  ENDMETHOD.

  METHOD is_digits.

    DATA(lv_text) = condense( |{ val }| ).
    result = xsdbool( strlen( lv_text ) = len AND lv_text CO `0123456789` ).

  ENDMETHOD.

  METHOD option_value.

    DATA lv_found TYPE abap_bool.

    IF option IS INITIAL.
      RETURN.
    ENDIF.
    component_text( EXPORTING path  = option
                              data  = data
                    IMPORTING found = lv_found
                              text  = result ).
    IF lv_found = abap_false.
      result = option.
    ENDIF.
    result = condense( result ).

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
    result->fill_sample( ).

  ENDMETHOD.

  METHOD element_font.

    DATA(lv_name) = element-font.
    IF lv_name IS INITIAL.
      lv_name = c_font.
    ENDIF.
    " PDF/A: no standard font may even be asked for
    IF go_pdfa_font IS BOUND.
      result = go_pdfa_font.
      RETURN.
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
    IF go_pdfa_font IS BOUND.
      result = go_pdfa_font.
      RETURN.
    ENDIF.
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
    DATA lv_extent  TYPE f.
    DATA lv_needed  TYPE f.
    DATA lv_lines   TYPE i.
    DATA lv_text    TYPE string.
    DATA lt_line    TYPE string_table.

    DATA(lo_first) = layout->page( ).
    lv_width = lo_first->width( ).
    lv_height = lo_first->height( ).

    " from the top of the canvas down: a table is printed before what
    " stands below it
    LOOP AT form-t_element INTO ls_element.
      IF ls_element-every_page = abap_true OR ls_element-block IS NOT INITIAL
          OR is_shown( element   = ls_element
                       data      = data
                       outer     = data
                       with_data = with_data ) = abap_false.
        CONTINUE.
      ENDIF.
      CASE ls_element-type.
        WHEN cs_type-text OR cs_type-table OR cs_type-line OR cs_type-rect OR cs_type-image OR cs_type-repeat
            OR cs_type-barcode.
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

      IF ls_element-type = cs_type-repeat.
        CLEAR ls_anchor.
        absolute_repeat( EXPORTING pdf       = pdf
                                   layout    = layout
                                   form      = form
                                   element   = ls_element
                                   page      = lv_page
                                   data      = data
                                   with_data = with_data
                         IMPORTING end_page  = ls_anchor-page
                                   end_y     = ls_anchor-y ).
        ls_anchor-bottom = lv_top + ls_element-h.
        INSERT ls_anchor INTO TABLE lt_anchor.
        CONTINUE.
      ENDIF.

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

      " a text with a height is as high as its lines make it, when they
      " need more than it is drawn: what stands below it moves, as below a
      " table
      lv_extent = ls_element-h.
      CLEAR lt_line.
      IF ls_element-type = cs_type-text AND ls_element-h > 0.
        lv_text = ls_element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        lt_line = text_lines( pdf     = pdf
                              layout  = layout
                              element = ls_element
                              text    = lv_text ).
        lv_lines = lines( lt_line ).
        lv_needed = lv_lines * line_height( pdf     = pdf
                                            element = ls_element ).
        IF lv_needed > lv_extent.
          lv_extent = lv_needed.
        ENDIF.
      ENDIF.

      " an element a table pushed over the bottom margin starts the next
      " page, and what stood below it on the canvas follows it there - a
      " text higher than a page goes on on the next one instead
      IF lv_shift > 0 AND ls_element-y + lv_extent > lv_height - form-margin_bottom
          AND lv_extent <= lv_height - form-margin_top - form-margin_bottom.
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

      IF lv_extent > ls_element-h AND lt_line IS NOT INITIAL.
        absolute_text( EXPORTING pdf      = pdf
                                 layout   = layout
                                 form     = form
                                 element  = ls_element
                                 lines    = lt_line
                                 page     = lv_page
                       IMPORTING end_page = ls_anchor-page
                                 end_y    = ls_anchor-y ).
        ls_anchor-bottom = lv_top + ls_element-h.
        INSERT ls_anchor INTO TABLE lt_anchor.
        CONTINUE.
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
        IF element-bookmark > 0 AND lv_text IS NOT INITIAL.
          " where its first line will be
          layout->ensure_space( element_size( element ) * 5 / 4 / pdf->scale( ) ).
          pdf->add_bookmark( title = lv_text
                             page  = pdf->page_count( )
                             y     = layout->y( )
                             level = element-bookmark ).
        ENDIF.
        IF element-markup = abap_true.
          layout->rich_text( val   = lv_text
                             align = element-align ).
        ELSE.
          layout->text( val   = lv_text
                        align = element-align ).
        ENDIF.

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
        lv_text = element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        IF lv_text IS INITIAL OR element-symbology IS INITIAL.
          RETURN.
        ENDIF.
        " a matrix code is as high as it is wide
        lv_height = element-h.
        IF lv_height <= 0.
          lv_height = element-w.
        ENDIF.
        layout->ensure_space( lv_height ).
        zabaputil_cl_pdf_barcode=>draw( page = layout->page( )
                                        type = element-symbology
                                        val  = lv_text
                                        x    = left
                                        y    = layout->y( )
                                        w    = element-w
                                        h    = lv_height ).
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
                                                      element = element )
                     options     = element-options ).
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
                                                         element = element )
                        options     = element-options ).

  ENDMETHOD.

  METHOD fixed_element.

    DATA lv_text  TYPE string.
    DATA lv_width TYPE f.

    " turned or see-through: in a graphics state of its own
    DATA(lv_state) = xsdbool( element-angle <> 0 OR ( element-opacity > 0 AND element-opacity < 1 ) ).
    IF lv_state = abap_true.
      page->save( ).
      IF element-opacity > 0 AND element-opacity < 1.
        page->set_alpha( element-opacity ).
      ENDIF.
      IF element-angle <> 0.
        page->rotate( angle = element-angle
                      x     = element-x
                      y     = element-y ).
      ENDIF.
    ENDIF.

    CASE element-type.

      WHEN cs_type-text.
        lv_text = element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        IF element-bookmark > 0 AND lv_text IS NOT INITIAL.
          DATA(lv_number) = 1.
          WHILE lv_number < pdf->page_count( ) AND pdf->page( lv_number ) <> page.
            lv_number = lv_number + 1.
          ENDWHILE.
          pdf->add_bookmark( title = lv_text
                             page  = lv_number
                             y     = element-y
                             level = element-bookmark ).
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
          page->rounded_rect( x = element-x
                              y = element-y
                              w = element-w
                              h = element-h
                              r = element-radius ).
        ELSE.
          page->set_fill_color( element-fill ).
          page->rounded_rect( x     = element-x
                              y     = element-y
                              w     = element-w
                              h     = element-h
                              r     = element-radius
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

      WHEN cs_type-barcode.
        lv_text = element-text.
        IF with_data = abap_true.
          lv_text = resolve( val  = lv_text
                             data = data ).
        ENDIF.
        IF lv_text IS NOT INITIAL AND element-symbology IS NOT INITIAL.
          zabaputil_cl_pdf_barcode=>draw( page = page
                                          type = element-symbology
                                          val  = lv_text
                                          x    = element-x
                                          y    = element-y
                                          w    = element-w
                                          h    = COND #( WHEN element-h > 0 THEN element-h ELSE element-w ) ).
        ENDIF.

    ENDCASE.

    IF lv_state = abap_true.
      page->restore( ).
    ENDIF.

  ENDMETHOD.

  METHOD fixed_text.

    text_draw( pdf     = pdf
               layout  = layout
               page    = page
               element = element
               lines   = text_lines( pdf     = pdf
                                     layout  = layout
                                     element = element
                                     text    = text )
               y       = element-y ).

  ENDMETHOD.

  METHOD text_lines.

    " the layout measures: a text with a width wraps in it
    layout->set_font( font = element_font( pdf     = pdf
                                           element = element )
                      size = element_size( element ) ).
    DATA(lv_line_feed) = |\n|.
    DATA(lv_text) = replace( val  = text
                             sub  = |\r\n|
                             with = lv_line_feed
                             occ  = 0 ).
    SPLIT lv_text AT lv_line_feed INTO TABLE DATA(lt_paragraph).
    LOOP AT lt_paragraph INTO DATA(lv_paragraph).
      " justified: the last line of a paragraph keeps a line feed at its
      " end, text_draw( ) writes it at the left
      IF to_upper( element-align ) = `J` AND element-w > 0 AND result IS NOT INITIAL.
        DATA(lv_last) = lines( result ).
        result[ lv_last ] = result[ lv_last ] && lv_line_feed.
      ENDIF.
      IF element-w > 0.
        IF element-markup = abap_true.
          DATA(lt_wrapped) = layout->wrap_markup( val   = lv_paragraph
                                                  width = element-w ).
        ELSE.
          lt_wrapped = layout->wrap( val   = lv_paragraph
                                     width = element-w ).
        ENDIF.
        IF lt_wrapped IS INITIAL.
          INSERT `` INTO TABLE result.
        ELSE.
          INSERT LINES OF lt_wrapped INTO TABLE result.
        ENDIF.
      ELSE.
        INSERT lv_paragraph INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF to_upper( element-align ) = `J` AND element-w > 0 AND result IS NOT INITIAL.
      lv_last = lines( result ).
      result[ lv_last ] = result[ lv_last ] && lv_line_feed.
    ENDIF.

  ENDMETHOD.

  METHOD line_height.

    result = element_size( element ) * 5 / 4 / pdf->scale( ).

  ENDMETHOD.

  METHOD text_draw.

    DATA lv_x TYPE f.
    DATA lv_y TYPE f.

    DATA(lo_font) = element_font( pdf     = pdf
                                  element = element ).
    DATA(lv_size) = element_size( element ).
    DATA(lv_align) = to_upper( element-align ).

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
    lv_y = y + lo_font->ascent( lv_size ) / pdf->scale( ).

    IF lv_align = `J`.
      " the lines stretched to the width, the last of a paragraph not
      layout->set_font( font = lo_font
                        size = lv_size ).
      layout->set_color( element_color( element ) ).
      LOOP AT lines INTO DATA(lv_justified).
        DATA(lv_length) = strlen( lv_justified ).
        IF lv_length > 0 AND substring( val = lv_justified
                                        off = lv_length - 1 ) = |\n|.
          lv_justified = substring( val = lv_justified
                                    len = lv_length - 1 ).
          IF element-markup = abap_true.
            layout->markup_line( page = page
                                 x    = lv_x
                                 y    = lv_y
                                 val  = lv_justified ).
          ELSE.
            page->set_font( font = lo_font
                            size = lv_size ).
            page->set_text_color( element_color( element ) ).
            page->text( x   = lv_x
                        y   = lv_y
                        val = lv_justified ).
          ENDIF.
        ELSE.
          layout->justified_line( page   = page
                                  x      = lv_x
                                  y      = lv_y
                                  val    = lv_justified
                                  width  = element-w
                                  markup = element-markup ).
        ENDIF.
        lv_y = lv_y + line_height( pdf     = pdf
                                   element = element ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF element-markup = abap_true.
      " the layout knows the cuts of the font
      layout->set_font( font = lo_font
                        size = lv_size ).
      layout->set_color( element_color( element ) ).
      LOOP AT lines INTO DATA(lv_rich).
        layout->markup_line( page  = page
                             x     = lv_x
                             y     = lv_y
                             val   = lv_rich
                             align = lv_align ).
        lv_y = lv_y + line_height( pdf     = pdf
                                   element = element ).
      ENDLOOP.
      RETURN.
    ENDIF.

    page->set_font( font = lo_font
                    size = lv_size ).
    page->set_text_color( element_color( element ) ).
    LOOP AT lines INTO DATA(lv_line).
      page->text( x     = lv_x
                  y     = lv_y
                  val   = lv_line
                  align = lv_align ).
      lv_y = lv_y + line_height( pdf     = pdf
                                 element = element ).
    ENDLOOP.

  ENDMETHOD.

  METHOD absolute_text.

    DATA lt_chunk  TYPE string_table.
    DATA lv_y      TYPE f.
    DATA lv_top    TYPE f.
    DATA lv_bottom TYPE f.

    DATA(lv_line_height) = line_height( pdf     = pdf
                                        element = element ).
    lv_bottom = layout->page( )->height( ) - form-margin_bottom.
    end_page = page.
    lv_top = element-y.
    lv_y = lv_top.

    LOOP AT lines INTO DATA(lv_line).
      " a line that does not fit above the bottom margin starts the next
      " page - unless it is the first one of the page
      IF lv_y + lv_line_height > lv_bottom AND lt_chunk IS NOT INITIAL.
        text_draw( pdf     = pdf
                   layout  = layout
                   page    = pdf->page( end_page )
                   element = element
                   lines   = lt_chunk
                   y       = lv_top ).
        CLEAR lt_chunk.
        IF end_page >= pdf->page_count( ).
          layout->page_break( ).
        ENDIF.
        end_page = end_page + 1.
        lv_top = form-margin_top.
        lv_y = lv_top.
      ENDIF.
      INSERT lv_line INTO TABLE lt_chunk.
      lv_y = lv_y + lv_line_height.
    ENDLOOP.

    text_draw( pdf     = pdf
               layout  = layout
               page    = pdf->page( end_page )
               element = element
               lines   = lt_chunk
               y       = lv_top ).
    end_y = lv_y.

  ENDMETHOD.

ENDCLASS.

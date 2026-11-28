CLASS zabaputil_cl_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " A PDF document, written in plain ABAP: no Adobe Document Services, no
    " spool, no kernel call beyond the byte conversion and the deflate of
    " zabaputil_cl_util_context. The same source runs on NW 7.02, Standard
    " ABAP and ABAP Cloud.
    "
    "   DATA(lo_pdf) = zabaputil_cl_pdf=>create( unit = `mm` ).
    "   DATA(lo_page) = lo_pdf->add_page( ).
    "   lo_page->set_font( font = lo_pdf->font( `Helvetica-Bold` ) size = 16 ).
    "   lo_page->text( x = 20 y = 30 val = `Hello` ).
    "   DATA(lv_file) = lo_pdf->render( ).
    "
    " The classes behind it: zabaputil_cl_pdf_page draws at fixed
    " positions, zabaputil_cl_pdf_layout flows text and tables over pages,
    " zabaputil_cl_pdf_font_std / _font_ttf and zabaputil_cl_pdf_image are
    " the resources, zabaputil_cl_pdf_writer writes the file.

    "! A new, empty document
    "! @parameter unit        | the unit of every coordinate: pt, mm, cm or in
    "! @parameter format      | the size of a new page: A3, A4, A5, LETTER or LEGAL
    "! @parameter orientation | P portrait, L landscape
    "! @parameter compress    | deflate the content of the file
    CLASS-METHODS create
      IMPORTING
        unit          TYPE clike DEFAULT `pt`
        format        TYPE clike DEFAULT `A4`
        orientation   TYPE clike DEFAULT `P`
        compress      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

    "! A new page at the end of the document. Without parameters it has the
    "! size the document was created with
    "! @parameter format      | A3, A4, A5, LETTER or LEGAL
    "! @parameter orientation | P portrait, L landscape
    "! @parameter width       | a size of its own, in the unit of the
    "!                          document; format and orientation are ignored then
    "! @parameter height      | the height that goes with width
    METHODS add_page
      IMPORTING
        format        TYPE clike OPTIONAL
        orientation   TYPE clike OPTIONAL
        width         TYPE numeric DEFAULT 0
        height        TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! A page by its number, the first one is 1
    METHODS page
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The number of pages
    METHODS page_count
      RETURNING
        VALUE(result) TYPE i.

    "! A standard font: nothing is embedded, Western European characters only
    "! @parameter name | Helvetica, Times-Roman or Courier, plain or with
    "!                   -Bold, -Oblique (Times: -Italic), -BoldOblique
    "!                   (Times: -BoldItalic)
    METHODS font
      IMPORTING
        name          TYPE clike DEFAULT `Helvetica`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_font.

    "! A TrueType font, embedded into the file: every script the font has
    "! @parameter content | the bytes of the .ttf file
    METHODS font_ttf
      IMPORTING
        content       TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_font.

    "! An image, to be placed with zabaputil_cl_pdf_page=&gt;image - as often
    "! as needed, the file holds it once
    "! @parameter content | the bytes of a .jpg or .png file
    METHODS image
      IMPORTING
        content       TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_image.

    "! The document properties a viewer shows
    METHODS set_info
      IMPORTING
        title         TYPE clike OPTIONAL
        author        TYPE clike OPTIONAL
        subject       TYPE clike OPTIONAL
        keywords      TYPE clike OPTIONAL
        creator       TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

    "! Points per unit of the document: 1 for pt, 2.835 for mm
    METHODS scale
      RETURNING
        VALUE(result) TYPE f.

    "! The file. A document without a page gets an empty one
    METHODS render
      RETURNING
        VALUE(result) TYPE xstring.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_font,
        name TYPE string,
        font TYPE REF TO zabaputil_if_pdf_font,
      END OF ty_s_font.
    TYPES:
      BEGIN OF ty_s_info,
        key  TYPE string,
        text TYPE string,
      END OF ty_s_info.

    DATA mv_scale    TYPE f.
    DATA mv_width    TYPE f.
    DATA mv_height   TYPE f.
    DATA mv_compress TYPE abap_bool.
    DATA mt_page     TYPE STANDARD TABLE OF REF TO zabaputil_cl_pdf_page WITH EMPTY KEY.
    DATA mt_font     TYPE STANDARD TABLE OF ty_s_font WITH EMPTY KEY.
    DATA mt_image    TYPE STANDARD TABLE OF REF TO zabaputil_cl_pdf_image WITH EMPTY KEY.
    DATA mt_info     TYPE STANDARD TABLE OF ty_s_info WITH EMPTY KEY.

    "! The size of a page format in points
    CLASS-METHODS format_size
      IMPORTING
        format      TYPE clike
        orientation TYPE clike
      EXPORTING
        width       TYPE f
        height      TYPE f.

    METHODS info_put
      IMPORTING
        key TYPE string
        val TYPE clike.

ENDCLASS.


CLASS zabaputil_cl_pdf IMPLEMENTATION.

  METHOD create.

    result = NEW #( ).
    result->mv_compress = compress.

    DATA(lv_unit) = to_lower( unit ).
    CASE lv_unit.
      WHEN `pt`.
        result->mv_scale = 1.
      WHEN `mm`.
        " an inch is 72 points and 25.4 mm
        result->mv_scale = 720.
        result->mv_scale = result->mv_scale / 254.
      WHEN `cm`.
        result->mv_scale = 7200.
        result->mv_scale = result->mv_scale / 254.
      WHEN `in`.
        result->mv_scale = 72.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: { unit } is not a unit (pt, mm, cm, in)|.
    ENDCASE.

    format_size( EXPORTING format      = format
                           orientation = orientation
                 IMPORTING width       = result->mv_width
                           height      = result->mv_height ).

  ENDMETHOD.

  METHOD format_size.

    DATA lv_swap TYPE f.

    DATA(lv_format) = to_upper( format ).
    CASE lv_format.
      WHEN `A3`.
        width = '841.89'.
        height = '1190.55'.
      WHEN `A4`.
        width = '595.28'.
        height = '841.89'.
      WHEN `A5`.
        width = '419.53'.
        height = '595.28'.
      WHEN `LETTER`.
        width = 612.
        height = 792.
      WHEN `LEGAL`.
        width = 612.
        height = 1008.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: { format } is not a page format (A3, A4, A5, LETTER, LEGAL)|.
    ENDCASE.

    DATA(lv_orientation) = to_upper( orientation ).
    IF lv_orientation IS NOT INITIAL AND lv_orientation(1) = `L`.
      lv_swap = width.
      width = height.
      height = lv_swap.
    ENDIF.

  ENDMETHOD.

  METHOD add_page.

    DATA lv_width  TYPE f.
    DATA lv_height TYPE f.

    lv_width = width.
    lv_height = height.

    IF lv_width > 0 AND lv_height > 0.
      lv_width = lv_width * mv_scale.
      lv_height = lv_height * mv_scale.
    ELSEIF format IS INITIAL AND orientation IS INITIAL.
      lv_width = mv_width.
      lv_height = mv_height.
    ELSEIF format IS INITIAL.
      " the document's format, turned when it lies the other way
      lv_width = mv_width.
      lv_height = mv_height.
      DATA(lv_orientation) = to_upper( orientation ).
      IF ( lv_orientation(1) = `L` AND lv_width < lv_height )
          OR ( lv_orientation(1) <> `L` AND lv_width > lv_height ).
        lv_width = mv_height.
        lv_height = mv_width.
      ENDIF.
    ELSE.
      format_size( EXPORTING format      = format
                             orientation = orientation
                   IMPORTING width       = lv_width
                             height      = lv_height ).
    ENDIF.

    result = NEW #( width  = lv_width
                    height = lv_height
                    scale  = mv_scale ).
    INSERT result INTO TABLE mt_page.

  ENDMETHOD.

  METHOD page.

    IF val < 1 OR val > lines( mt_page ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: there is no page { val }|.
    ENDIF.
    result = mt_page[ val ].

  ENDMETHOD.

  METHOD page_count.

    result = lines( mt_page ).

  ENDMETHOD.

  METHOD font.

    DATA ls_font TYPE ty_s_font.

    " one object per name: asked for twice, it is the same font
    ls_font-name = name.
    ASSIGN mt_font[ name = ls_font-name ] TO FIELD-SYMBOL(<font>).
    IF sy-subrc = 0.
      result = <font>-font.
      RETURN.
    ENDIF.

    ls_font-font = NEW zabaputil_cl_pdf_font_std( name     = ls_font-name
                                                  resource = |F{ lines( mt_font ) + 1 }| ).
    INSERT ls_font INTO TABLE mt_font.
    result = ls_font-font.

  ENDMETHOD.

  METHOD font_ttf.

    DATA ls_font TYPE ty_s_font.

    ls_font-font = NEW zabaputil_cl_pdf_font_ttf( content  = content
                                                  resource = |F{ lines( mt_font ) + 1 }| ).
    INSERT ls_font INTO TABLE mt_font.
    result = ls_font-font.

  ENDMETHOD.

  METHOD image.

    result = NEW #( content  = content
                    resource = |I{ lines( mt_image ) + 1 }| ).
    INSERT result INTO TABLE mt_image.

  ENDMETHOD.

  METHOD set_info.

    info_put( key = `Title`
              val = title ).
    info_put( key = `Author`
              val = author ).
    info_put( key = `Subject`
              val = subject ).
    info_put( key = `Keywords`
              val = keywords ).
    info_put( key = `Creator`
              val = creator ).
    result = me.

  ENDMETHOD.

  METHOD info_put.

    DATA ls_info TYPE ty_s_info.

    ls_info-key = key.
    ls_info-text = val.
    IF ls_info-text IS INITIAL.
      RETURN.
    ENDIF.
    DELETE mt_info WHERE key = ls_info-key.
    INSERT ls_info INTO TABLE mt_info.

  ENDMETHOD.

  METHOD scale.

    result = mv_scale.

  ENDMETHOD.

  METHOD render.

    DATA lv_fonts   TYPE string.
    DATA lv_images  TYPE string.
    DATA lv_kids    TYPE string.
    DATA lv_annots  TYPE string.
    DATA lv_info    TYPE string.
    DATA lv_info_id TYPE i.
    DATA lv_id      TYPE i.

    IF mt_page IS INITIAL.
      add_page( ).
    ENDIF.

    DATA(lo_writer) = NEW zabaputil_cl_pdf_writer( mv_compress ).

    " every page shares one resource dictionary with all fonts and images
    LOOP AT mt_font ASSIGNING FIELD-SYMBOL(<font>).
      lv_id = <font>-font->write( lo_writer ).
      lv_fonts = lv_fonts && | /{ <font>-font->resource( ) } { lv_id } 0 R|.
    ENDLOOP.
    LOOP AT mt_image INTO DATA(lo_image).
      lv_id = lo_image->write( lo_writer ).
      lv_images = lv_images && | /{ lo_image->resource( ) } { lv_id } 0 R|.
    ENDLOOP.
    DATA(lv_resources) = lo_writer->add_object(
        |<< /ProcSet [/PDF /Text /ImageB /ImageC /ImageI] /Font <<{ lv_fonts } >> /XObject <<{ lv_images } >> >>| ).

    DATA(lv_pages) = lo_writer->reserve( ).
    LOOP AT mt_page INTO DATA(lo_page).

      DATA(lv_content) = lo_writer->add_stream(
          zabaputil_cl_util_context=>conv_get_xstring_by_string( lo_page->content( ) ) ).

      CLEAR lv_annots.
      LOOP AT lo_page->annotations( ) INTO DATA(lv_annotation).
        lv_id = lo_writer->add_object( lv_annotation ).
        lv_annots = lv_annots && | { lv_id } 0 R|.
      ENDLOOP.
      IF lv_annots IS NOT INITIAL.
        lv_annots = | /Annots [{ lv_annots } ]|.
      ENDIF.

      lv_id = lo_writer->add_object(
          |<< /Type /Page /Parent { lv_pages } 0 R | &&
          |/MediaBox [0 0 { zabaputil_cl_pdf_writer=>num( lo_page->width_pt( ) ) } | &&
          |{ zabaputil_cl_pdf_writer=>num( lo_page->height_pt( ) ) }] | &&
          |/Resources { lv_resources } 0 R /Contents { lv_content } 0 R{ lv_annots } >>| ).
      lv_kids = lv_kids && | { lv_id } 0 R|.

    ENDLOOP.
    lo_writer->set_object( id  = lv_pages
                           val = |<< /Type /Pages /Kids [{ lv_kids } ] /Count { lines( mt_page ) } >>| ).

    DATA(lv_catalog) = lo_writer->add_object( |<< /Type /Catalog /Pages { lv_pages } 0 R >>| ).

    " text strings in UTF-16: a title is not limited to one code page
    LOOP AT mt_info ASSIGNING FIELD-SYMBOL(<info>).
      lv_info = lv_info && | /{ <info>-key } <{ zabaputil_cl_pdf_writer=>hex_utf16( <info>-text ) }>|.
    ENDLOOP.
    lv_info_id = lo_writer->add_object( |<<{ lv_info } /Producer (abap-util) >>| ).

    result = lo_writer->render( root = lv_catalog
                                info = lv_info_id ).

  ENDMETHOD.

ENDCLASS.

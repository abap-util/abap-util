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

    "! A TrueType or OpenType font, embedded into the file: every script
    "! the font has
    "! @parameter content | the bytes of the .ttf, .otf or .ttc file
    "! @parameter subset  | only the glyphs the document uses; abap_false
    "!                      embeds the whole file
    "! @parameter index   | of a collection (.ttc): the font, from 0
    METHODS font_ttf
      IMPORTING
        content       TYPE xstring
        subset        TYPE abap_bool DEFAULT abap_true
        index         TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_if_pdf_font.

    "! The font every standard font of the document writes the characters
    "! with that WinAnsiEncoding does not have - those of fonts asked for
    "! later, too
    "! @parameter val | an embedded font, from font_ttf( )
    METHODS set_fallback_font
      IMPORTING
        val           TYPE REF TO zabaputil_if_pdf_font
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

    "! The characters written so far that no font of the document had: a
    "! question mark for a standard font, the missing glyph of an embedded
    "! one. Each once - empty when every character could be written
    METHODS missing_chars
      RETURNING
        VALUE(result) TYPE string_table.

    "! An image, to be placed with zabaputil_cl_pdf_page=&gt;image - as often
    "! as needed, the file holds it once
    "! @parameter content | the bytes of a .jpg or .png file
    METHODS image
      IMPORTING
        content       TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_image.

    "! The document properties a viewer shows
    "! An entry of the outline of the document, the bookmarks a viewer
    "! shows beside the pages: in the order they are added, one level
    "! below the entry before when level is one more
    "! @parameter title | the text of the entry
    "! @parameter page  | the page it leads to; 0 for the last page so far
    "! @parameter y     | the place on that page, from its top
    "! @parameter level | 1 for the top level
    METHODS add_bookmark
      IMPORTING
        title         TYPE clike
        page          TYPE i DEFAULT 0
        y             TYPE numeric DEFAULT 0
        level         TYPE i DEFAULT 1
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

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
    DATA mo_fallback TYPE REF TO zabaputil_if_pdf_font.
    DATA mt_image    TYPE STANDARD TABLE OF REF TO zabaputil_cl_pdf_image WITH EMPTY KEY.
    DATA mt_info     TYPE STANDARD TABLE OF ty_s_info WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_bookmark,
        title TYPE string,
        page  TYPE i,
        y     TYPE f,
        level TYPE i,
      END OF ty_s_bookmark.
    DATA mt_bookmark TYPE STANDARD TABLE OF ty_s_bookmark WITH EMPTY KEY.

    "! The size of a page format in points
    CLASS-METHODS format_size
      IMPORTING
        format      TYPE clike
        orientation TYPE clike
      EXPORTING
        width       TYPE f
        height      TYPE f.

    "! A destination: the page object and the place on the page
    METHODS destination
      IMPORTING
        page          TYPE i
        y             TYPE f
        t_page_id     TYPE string_table
      RETURNING
        VALUE(result) TYPE string.

    "! The objects of the outline; its number, 0 without bookmarks
    METHODS write_outline
      IMPORTING
        writer        TYPE REF TO zabaputil_cl_pdf_writer
        t_page_id     TYPE string_table
      RETURNING
        VALUE(result) TYPE i.

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

    DATA(lo_std) = NEW zabaputil_cl_pdf_font_std( name     = ls_font-name
                                                  resource = |F{ lines( mt_font ) + 1 }| ).
    IF mo_fallback IS BOUND.
      lo_std->set_fallback( mo_fallback ).
    ENDIF.
    ls_font-font = lo_std.
    INSERT ls_font INTO TABLE mt_font.
    result = ls_font-font.

  ENDMETHOD.

  METHOD set_fallback_font.

    DATA lo_std TYPE REF TO zabaputil_cl_pdf_font_std.

    mo_fallback = val.
    LOOP AT mt_font ASSIGNING FIELD-SYMBOL(<font>).
      TRY.
          lo_std ?= <font>-font.
          lo_std->set_fallback( val ).
        CATCH cx_sy_move_cast_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
    result = me.

  ENDMETHOD.

  METHOD missing_chars.

    DATA lo_std TYPE REF TO zabaputil_cl_pdf_font_std.
    DATA lo_ttf TYPE REF TO zabaputil_cl_pdf_font_ttf.

    LOOP AT mt_font ASSIGNING FIELD-SYMBOL(<font>).
      TRY.
          lo_std ?= <font>-font.
          LOOP AT lo_std->missing( ) INTO DATA(lv_char).
            IF NOT line_exists( result[ table_line = lv_char ] ).
              INSERT lv_char INTO TABLE result.
            ENDIF.
          ENDLOOP.
        CATCH cx_sy_move_cast_error ##NO_HANDLER.
      ENDTRY.
      TRY.
          lo_ttf ?= <font>-font.
          LOOP AT lo_ttf->missing( ) INTO DATA(lv_cp).
            lv_char = zabaputil_cl_pdf_font_std=>char( lv_cp ).
            IF NOT line_exists( result[ table_line = lv_char ] ).
              INSERT lv_char INTO TABLE result.
            ENDIF.
          ENDLOOP.
        CATCH cx_sy_move_cast_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD font_ttf.

    DATA ls_font TYPE ty_s_font.

    ls_font-font = NEW zabaputil_cl_pdf_font_ttf( content  = content
                                                  resource = |F{ lines( mt_font ) + 1 }|
                                                  subset   = subset
                                                  index    = index ).
    INSERT ls_font INTO TABLE mt_font.
    result = ls_font-font.

  ENDMETHOD.

  METHOD image.

    result = NEW #( content  = content
                    resource = |I{ lines( mt_image ) + 1 }| ).
    INSERT result INTO TABLE mt_image.

  ENDMETHOD.

  METHOD add_bookmark.

    DATA ls_bookmark TYPE ty_s_bookmark.

    ls_bookmark-title = title.
    ls_bookmark-page = page.
    IF ls_bookmark-page <= 0.
      ls_bookmark-page = nmax( val1 = 1
                               val2 = lines( mt_page ) ).
    ENDIF.
    ls_bookmark-y = y.
    ls_bookmark-level = nmax( val1 = 1
                              val2 = level ).
    " a level deeper than one below the entry before is one below it
    IF mt_bookmark IS INITIAL.
      ls_bookmark-level = 1.
    ELSE.
      ls_bookmark-level = nmin( val1 = ls_bookmark-level
                                val2 = mt_bookmark[ lines( mt_bookmark ) ]-level + 1 ).
    ENDIF.
    INSERT ls_bookmark INTO TABLE mt_bookmark.
    result = me.

  ENDMETHOD.

  METHOD destination.

    DATA lv_top TYPE f.

    " the top of the page in points, the place below it
    DATA(lo_page) = mt_page[ page ].
    lv_top = lo_page->height_pt( ) - y * mv_scale.
    result = |[{ t_page_id[ page ] } 0 R /XYZ 0 { zabaputil_cl_pdf_writer=>num( lv_top ) } null]|.

  ENDMETHOD.

  METHOD write_outline.

    DATA lt_id     TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lt_parent TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lv_index  TYPE i.
    DATA lv_other  TYPE i.
    DATA lv_dict   TYPE string.

    IF mt_bookmark IS INITIAL.
      RETURN.
    ENDIF.

    " the outline and every entry an object; the parent of an entry is
    " the last entry before it one level higher
    result = writer->reserve( ).
    LOOP AT mt_bookmark ASSIGNING FIELD-SYMBOL(<bookmark>).
      lv_index = sy-tabix.
      INSERT writer->reserve( ) INTO TABLE lt_id.
      DATA(lv_parent) = 0.
      lv_other = lv_index - 1.
      WHILE lv_other >= 1.
        IF mt_bookmark[ lv_other ]-level < <bookmark>-level.
          lv_parent = lv_other.
          EXIT.
        ENDIF.
        lv_other = lv_other - 1.
      ENDWHILE.
      INSERT lv_parent INTO TABLE lt_parent.
    ENDLOOP.

    LOOP AT mt_bookmark ASSIGNING <bookmark>.
      lv_index = sy-tabix.
      DATA(lv_parent_index) = lt_parent[ lv_index ].
      lv_dict = |<< /Title <{ zabaputil_cl_pdf_writer=>hex_utf16( <bookmark>-title ) }> | &&
                |/Parent { COND i( WHEN lv_parent_index = 0 THEN result ELSE lt_id[ lv_parent_index ] ) } 0 R|.

      " the siblings: the entries of the same parent
      DATA(lv_prev) = 0.
      DATA(lv_next) = 0.
      DATA(lv_first) = 0.
      DATA(lv_last) = 0.
      DATA(lv_count) = 0.
      LOOP AT lt_parent INTO DATA(lv_of) .
        lv_other = sy-tabix.
        IF lv_of = lv_parent_index AND lv_other < lv_index.
          lv_prev = lv_other.
        ELSEIF lv_of = lv_parent_index AND lv_other > lv_index AND lv_next = 0.
          lv_next = lv_other.
        ENDIF.
        IF lv_of = lv_index.
          IF lv_first = 0.
            lv_first = lv_other.
          ENDIF.
          lv_last = lv_other.
        ENDIF.
      ENDLOOP.
      " every entry open: its count is all the entries below it
      " a DO with EXIT: the downport reads a table expression of a WHILE
      " condition once, in front of the loop
      lv_other = lv_index + 1.
      DO.
        IF lv_other > lines( mt_bookmark ).
          EXIT.
        ENDIF.
        IF mt_bookmark[ lv_other ]-level <= <bookmark>-level.
          EXIT.
        ENDIF.
        lv_count = lv_count + 1.
        lv_other = lv_other + 1.
      ENDDO.

      IF lv_prev > 0.
        lv_dict = |{ lv_dict } /Prev { lt_id[ lv_prev ] } 0 R|.
      ENDIF.
      IF lv_next > 0.
        lv_dict = |{ lv_dict } /Next { lt_id[ lv_next ] } 0 R|.
      ENDIF.
      IF lv_first > 0.
        lv_dict = |{ lv_dict } /First { lt_id[ lv_first ] } 0 R /Last { lt_id[ lv_last ] } 0 R /Count { lv_count }|.
      ENDIF.
      lv_dict = |{ lv_dict } /Dest { destination( page      = <bookmark>-page
                                                  y         = <bookmark>-y
                                                  t_page_id = t_page_id ) } >>|.
      writer->set_object( id  = lt_id[ lv_index ]
                          val = lv_dict ).
    ENDLOOP.

    " the outline: its first and last entry of the top level
    lv_first = 0.
    LOOP AT lt_parent INTO lv_of.
      IF lv_of = 0.
        IF lv_first = 0.
          lv_first = sy-tabix.
        ENDIF.
        lv_last = sy-tabix.
      ENDIF.
    ENDLOOP.
    writer->set_object( id  = result
                        val = |<< /Type /Outlines /First { lt_id[ lv_first ] } 0 R /Last { lt_id[ lv_last ] } 0 R | &&
                              |/Count { lines( mt_bookmark ) } >>| ).

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
    " the transparencies of all pages, a graphics state each
    DATA lt_alpha TYPE string_table.
    DATA lv_states TYPE string.
    LOOP AT mt_page INTO DATA(lo_alpha_page).
      LOOP AT lo_alpha_page->alphas( ) INTO DATA(lv_alpha).
        IF NOT line_exists( lt_alpha[ table_line = lv_alpha ] ).
          INSERT lv_alpha INTO TABLE lt_alpha.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    LOOP AT lt_alpha INTO lv_alpha.
      SPLIT lv_alpha AT ` ` INTO DATA(lv_state) DATA(lv_ca) DATA(lv_stroke_ca).
      lv_id = lo_writer->add_object( |<< /Type /ExtGState /ca { lv_ca } /CA { lv_stroke_ca } >>| ).
      lv_states = lv_states && | /{ lv_state } { lv_id } 0 R|.
    ENDLOOP.
    IF lv_states IS NOT INITIAL.
      lv_states = | /ExtGState <<{ lv_states } >>|.
    ENDIF.

    DATA(lv_resources) = lo_writer->add_object(
        |<< /ProcSet [/PDF /Text /ImageB /ImageC /ImageI] /Font <<{ lv_fonts } >> /XObject <<{ lv_images } >>{ lv_states } >>| ).

    DATA(lv_pages) = lo_writer->reserve( ).

    " links into the document and bookmarks need the numbers of the page
    " objects before the pages are written
    DATA lt_page_id TYPE string_table.
    DATA(lv_reserve) = xsdbool( mt_bookmark IS NOT INITIAL ).
    LOOP AT mt_page INTO DATA(lo_goto_page).
      IF lo_goto_page->gotos( ) IS NOT INITIAL.
        lv_reserve = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_reserve = abap_true.
      LOOP AT mt_page INTO lo_goto_page.
        INSERT |{ lo_writer->reserve( ) }| INTO TABLE lt_page_id.
      ENDLOOP.
    ENDIF.

    LOOP AT mt_page INTO DATA(lo_page).
      DATA(lv_page_index) = sy-tabix.

      DATA(lv_content) = lo_writer->add_stream(
          zabaputil_cl_util_context=>conv_get_xstring_by_string( lo_page->content( ) ) ).

      CLEAR lv_annots.
      LOOP AT lo_page->annotations( ) INTO DATA(lv_annotation).
        lv_id = lo_writer->add_object( lv_annotation ).
        lv_annots = lv_annots && | { lv_id } 0 R|.
      ENDLOOP.
      LOOP AT lo_page->gotos( ) INTO DATA(ls_goto).
        IF ls_goto-page < 1 OR ls_goto-page > lines( mt_page ).
          CONTINUE.
        ENDIF.
        lv_id = lo_writer->add_object( |<< /Type /Annot /Subtype /Link /Rect [{ ls_goto-rect }] /Border [0 0 0] | &&
                                       |/Dest { destination( page      = ls_goto-page
                                                             y         = ls_goto-y
                                                             t_page_id = lt_page_id ) } >>| ).
        lv_annots = lv_annots && | { lv_id } 0 R|.
      ENDLOOP.
      IF lv_annots IS NOT INITIAL.
        lv_annots = | /Annots [{ lv_annots } ]|.
      ENDIF.

      DATA(lv_page_object) = |<< /Type /Page /Parent { lv_pages } 0 R | &&
          |/MediaBox [0 0 { zabaputil_cl_pdf_writer=>num( lo_page->width_pt( ) ) } | &&
          |{ zabaputil_cl_pdf_writer=>num( lo_page->height_pt( ) ) }] | &&
          |/Resources { lv_resources } 0 R /Contents { lv_content } 0 R{ lv_annots } >>|.
      IF lv_reserve = abap_true.
        lv_id = lt_page_id[ lv_page_index ].
        lo_writer->set_object( id  = lv_id
                               val = lv_page_object ).
      ELSE.
        lv_id = lo_writer->add_object( lv_page_object ).
      ENDIF.
      lv_kids = lv_kids && | { lv_id } 0 R|.

    ENDLOOP.
    lo_writer->set_object( id  = lv_pages
                           val = |<< /Type /Pages /Kids [{ lv_kids } ] /Count { lines( mt_page ) } >>| ).

    DATA(lv_catalog_extra) = ``.
    DATA(lv_outline) = write_outline( writer    = lo_writer
                                      t_page_id = lt_page_id ).
    IF lv_outline > 0.
      lv_catalog_extra = | /Outlines { lv_outline } 0 R /PageMode /UseOutlines|.
    ENDIF.
    DATA(lv_catalog) = lo_writer->add_object( |<< /Type /Catalog /Pages { lv_pages } 0 R{ lv_catalog_extra } >>| ).

    " text strings in UTF-16: a title is not limited to one code page
    LOOP AT mt_info ASSIGNING FIELD-SYMBOL(<info>).
      lv_info = lv_info && | /{ <info>-key } <{ zabaputil_cl_pdf_writer=>hex_utf16( <info>-text ) }>|.
    ENDLOOP.
    lv_info_id = lo_writer->add_object( |<<{ lv_info } /Producer (abap-util) >>| ).

    result = lo_writer->render( root = lv_catalog
                                info = lv_info_id ).

  ENDMETHOD.

ENDCLASS.

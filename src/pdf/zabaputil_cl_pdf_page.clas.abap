CLASS zabaputil_cl_pdf_page DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " One page: every method draws at the position it is given and adds
    " the operators to the content stream of the page. Nothing flows and
    " nothing breaks here - zabaputil_cl_pdf_layout does that on top.
    "
    " Coordinates are in the unit of the document (points, mm, cm, inch),
    " measured from the top left corner of the page, y downwards. A font
    " size is always in points. Colours are `RRGGBB` in hex, with or
    " without a leading `#`.

    "! A page is created by zabaputil_cl_pdf=&gt;add_page
    "! @parameter width  | the width of the page in points
    "! @parameter height | the height of the page in points
    "! @parameter scale  | points per unit of the document
    METHODS constructor
      IMPORTING
        width  TYPE numeric
        height TYPE numeric
        scale  TYPE numeric.

    "! The width of the page, in the unit of the document
    METHODS width
      RETURNING
        VALUE(result) TYPE f.

    "! The height of the page, in the unit of the document
    METHODS height
      RETURNING
        VALUE(result) TYPE f.

    "! The font and size of the texts that follow
    "! @parameter font | from zabaputil_cl_pdf=&gt;font or =&gt;font_ttf
    "! @parameter size | the font size in points
    METHODS set_font
      IMPORTING
        font          TYPE REF TO zabaputil_if_pdf_font
        size          TYPE numeric DEFAULT 12
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The colour of the texts that follow
    METHODS set_text_color
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The colour shapes are filled with
    METHODS set_fill_color
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The colour of lines and outlines
    METHODS set_draw_color
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The width of lines and outlines, in points
    METHODS set_line_width
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! A dash pattern for lines and outlines, in points; without a
    "! parameter the lines are solid again
    "! @parameter dash | the length of a dash
    "! @parameter gap  | the length of a gap; 0 for as long as the dash
    METHODS set_dash
      IMPORTING
        dash          TYPE numeric DEFAULT 0
        gap           TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! One line of text. Nothing is wrapped
    "! @parameter x     | where the text starts (align L), is centred (C) or ends (R)
    "! @parameter y     | the baseline of the text
    "! @parameter val   | the text
    "! @parameter align | L, C or R
    METHODS text
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
        val           TYPE clike
        align         TYPE clike DEFAULT `L`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The width of a text in the current font, in the unit of the document
    METHODS text_width
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE f.

    "! A straight line
    METHODS line
      IMPORTING
        x1            TYPE numeric
        y1            TYPE numeric
        x2            TYPE numeric
        y2            TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! A rectangle
    "! @parameter x     | the left edge
    "! @parameter y     | the top edge
    "! @parameter w     | the width
    "! @parameter h     | the height
    "! @parameter style | D outline, F filled, DF both
    METHODS rect
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
        w             TYPE numeric
        h             TYPE numeric
        style         TYPE clike DEFAULT `D`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! An ellipse; a circle when both radii are the same
    "! @parameter x     | the centre
    "! @parameter y     | the centre
    "! @parameter rx    | the horizontal radius
    "! @parameter ry    | the vertical radius; 0 for the same as rx
    "! @parameter style | D outline, F filled, DF both
    METHODS ellipse
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
        rx            TYPE numeric
        ry            TYPE numeric DEFAULT 0
        style         TYPE clike DEFAULT `D`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! An image. With one of w and h the other follows from the
    "! proportions of the image, with none of them a pixel is one point
    "! @parameter image | from zabaputil_cl_pdf=&gt;image
    "! @parameter x     | the left edge
    "! @parameter y     | the top edge
    "! @parameter w     | the width, 0 to derive it
    "! @parameter h     | the height, 0 to derive it
    METHODS image
      IMPORTING
        image         TYPE REF TO zabaputil_cl_pdf_image
        x             TYPE numeric
        y             TYPE numeric
        w             TYPE numeric DEFAULT 0
        h             TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! An area that opens a web address when it is clicked. It draws
    "! nothing - put it over a text or an image
    "! @parameter x   | the left edge
    "! @parameter y   | the top edge
    "! @parameter w   | the width
    "! @parameter h   | the height
    "! @parameter url | the address, 7-bit ASCII (percent-encode the rest)
    METHODS link
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
        w             TYPE numeric
        h             TYPE numeric
        url           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! Operators of the PDF content stream, added as they are - for what
    "! this class has no method for
    METHODS raw
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The content stream written so far
    METHODS content
      RETURNING
        VALUE(result) TYPE string.

    "! The link annotations, one dictionary per row
    METHODS annotations
      RETURNING
        VALUE(result) TYPE string_table.

    "! The width of the page in points
    METHODS width_pt
      RETURNING
        VALUE(result) TYPE f.

    "! The height of the page in points
    METHODS height_pt
      RETURNING
        VALUE(result) TYPE f.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_width      TYPE f.
    DATA mv_height     TYPE f.
    DATA mv_scale      TYPE f.
    DATA mv_content    TYPE string.
    DATA mt_annotation TYPE string_table.
    DATA mo_font       TYPE REF TO zabaputil_if_pdf_font.
    DATA mv_font_size  TYPE f.
    " the colours as operands, `0 0 0`: text and fill share one operator in
    " PDF (rg), so a text in another colour than the fill is bracketed
    DATA mv_text_color TYPE string.
    DATA mv_fill_color TYPE string.

    METHODS emit
      IMPORTING
        val TYPE string.

    "! A horizontal position in points
    METHODS px
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    "! A vertical position in points, counted from the bottom as PDF does
    METHODS py
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    "! A length in points
    METHODS pl
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    "! `RRGGBB` as three operands between 0 and 1
    METHODS color
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The painting operator of a style: S stroke, f fill, B both
    METHODS paint
      IMPORTING
        style         TYPE clike
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_page IMPLEMENTATION.

  METHOD constructor.

    mv_width = width.
    mv_height = height.
    mv_scale = scale.
    IF mv_scale <= 0.
      mv_scale = 1.
    ENDIF.
    mv_font_size = 12.
    mv_text_color = `0 0 0`.
    mv_fill_color = `0 0 0`.

  ENDMETHOD.

  METHOD width.

    result = mv_width / mv_scale.

  ENDMETHOD.

  METHOD height.

    result = mv_height / mv_scale.

  ENDMETHOD.

  METHOD width_pt.

    result = mv_width.

  ENDMETHOD.

  METHOD height_pt.

    result = mv_height.

  ENDMETHOD.

  METHOD emit.

    IF mv_content IS INITIAL.
      mv_content = val.
    ELSE.
      mv_content = mv_content && |\n| && val.
    ENDIF.

  ENDMETHOD.

  METHOD px.

    DATA lv_val TYPE f.

    lv_val = val.
    result = zabaputil_cl_pdf_writer=>num( lv_val * mv_scale ).

  ENDMETHOD.

  METHOD py.

    DATA lv_val TYPE f.

    lv_val = val.
    result = zabaputil_cl_pdf_writer=>num( mv_height - lv_val * mv_scale ).

  ENDMETHOD.

  METHOD pl.

    DATA lv_val TYPE f.

    lv_val = val.
    result = zabaputil_cl_pdf_writer=>num( lv_val * mv_scale ).

  ENDMETHOD.

  METHOD color.

    DATA lv_hex   TYPE string.
    DATA lv_x     TYPE x LENGTH 3.
    DATA lv_part  TYPE i.
    DATA lv_share TYPE f.
    DATA lv_off   TYPE i.

    lv_hex = to_upper( val ).
    IF strlen( lv_hex ) = 7 AND lv_hex(1) = `#`.
      lv_hex = substring( val = lv_hex
                          off = 1 ).
    ENDIF.
    IF strlen( lv_hex ) <> 6 OR lv_hex CN `0123456789ABCDEF`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: { val } is not a colour (RRGGBB in hex)|.
    ENDIF.

    lv_x = lv_hex.
    DO 3 TIMES.
      lv_off = sy-index - 1.
      lv_part = lv_x+lv_off(1).
      lv_share = lv_part / 255.
      IF sy-index > 1.
        result = result && ` `.
      ENDIF.
      result = result && zabaputil_cl_pdf_writer=>num( lv_share ).
    ENDDO.

  ENDMETHOD.

  METHOD paint.

    DATA(lv_style) = to_upper( style ).
    CASE lv_style.
      WHEN `F`.
        result = `f`.
      WHEN `DF` OR `FD` OR `B`.
        result = `B`.
      WHEN OTHERS.
        result = `S`.
    ENDCASE.

  ENDMETHOD.

  METHOD set_font.

    mo_font = font.
    mv_font_size = size.
    result = me.

  ENDMETHOD.

  METHOD set_text_color.

    mv_text_color = color( val ).
    result = me.

  ENDMETHOD.

  METHOD set_fill_color.

    mv_fill_color = color( val ).
    emit( |{ mv_fill_color } rg| ).
    result = me.

  ENDMETHOD.

  METHOD set_draw_color.

    emit( |{ color( val ) } RG| ).
    result = me.

  ENDMETHOD.

  METHOD set_line_width.

    emit( |{ zabaputil_cl_pdf_writer=>num( val ) } w| ).
    result = me.

  ENDMETHOD.

  METHOD set_dash.

    DATA lv_dash TYPE f.
    DATA lv_gap  TYPE f.

    lv_dash = dash.
    lv_gap = gap.
    IF lv_dash <= 0.
      emit( `[] 0 d` ).
    ELSE.
      IF lv_gap <= 0.
        lv_gap = lv_dash.
      ENDIF.
      emit( |[{ zabaputil_cl_pdf_writer=>num( lv_dash ) } { zabaputil_cl_pdf_writer=>num( lv_gap ) }] 0 d| ).
    ENDIF.
    result = me.

  ENDMETHOD.

  METHOD text_width.

    IF mo_font IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: set a font before a text is measured or written (set_font)`.
    ENDIF.
    result = mo_font->width( val  = val
                             size = mv_font_size ) / mv_scale.

  ENDMETHOD.

  METHOD text.

    DATA lv_x    TYPE f.
    DATA lv_show TYPE string.

    result = me.
    IF mo_font IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: set a font before a text is measured or written (set_font)`.
    ENDIF.

    DATA(lv_text) = CONV string( val ).
    IF lv_text IS INITIAL.
      RETURN.
    ENDIF.

    lv_x = x.
    DATA(lv_align) = to_upper( align ).
    CASE lv_align.
      WHEN `R`.
        lv_x = lv_x - text_width( lv_text ).
      WHEN `C`.
        lv_x = lv_x - text_width( lv_text ) / 2.
    ENDCASE.

    lv_show = |BT /{ mo_font->resource( ) } { zabaputil_cl_pdf_writer=>num( mv_font_size ) } Tf | &&
              |{ px( lv_x ) } { py( y ) } Td { mo_font->encode( lv_text ) } Tj ET|.
    IF mv_text_color = mv_fill_color.
      emit( lv_show ).
    ELSE.
      emit( |q { mv_text_color } rg { lv_show } Q| ).
    ENDIF.

  ENDMETHOD.

  METHOD line.

    emit( |{ px( x1 ) } { py( y1 ) } m { px( x2 ) } { py( y2 ) } l S| ).
    result = me.

  ENDMETHOD.

  METHOD rect.

    DATA lv_y TYPE f.
    DATA lv_h TYPE f.

    " PDF wants the lower left corner
    lv_y = y.
    lv_h = h.
    emit( |{ px( x ) } { py( lv_y + lv_h ) } { pl( w ) } { pl( h ) } re { paint( style ) }| ).
    result = me.

  ENDMETHOD.

  METHOD ellipse.

    DATA lv_cx TYPE f.
    DATA lv_cy TYPE f.
    DATA lv_rx TYPE f.
    DATA lv_ry TYPE f.
    DATA lv_kx TYPE f.
    DATA lv_ky TYPE f.
    " the distance of the control points that comes closest to a circle
    CONSTANTS lc_kappa TYPE f VALUE '0.5523'.

    " everything in points, y from the bottom
    lv_cx = x.
    lv_cy = y.
    lv_rx = rx.
    lv_ry = ry.
    IF lv_ry <= 0.
      lv_ry = lv_rx.
    ENDIF.
    lv_cx = lv_cx * mv_scale.
    lv_cy = mv_height - lv_cy * mv_scale.
    lv_rx = lv_rx * mv_scale.
    lv_ry = lv_ry * mv_scale.

    " four cubic Bezier curves, one per quarter
    lv_kx = lv_rx * lc_kappa.
    lv_ky = lv_ry * lc_kappa.

    DATA(lv_left) = zabaputil_cl_pdf_writer=>num( lv_cx - lv_rx ).
    DATA(lv_right) = zabaputil_cl_pdf_writer=>num( lv_cx + lv_rx ).
    DATA(lv_top) = zabaputil_cl_pdf_writer=>num( lv_cy + lv_ry ).
    DATA(lv_bottom) = zabaputil_cl_pdf_writer=>num( lv_cy - lv_ry ).
    DATA(lv_mid_x) = zabaputil_cl_pdf_writer=>num( lv_cx ).
    DATA(lv_mid_y) = zabaputil_cl_pdf_writer=>num( lv_cy ).
    DATA(lv_x_minus) = zabaputil_cl_pdf_writer=>num( lv_cx - lv_kx ).
    DATA(lv_x_plus) = zabaputil_cl_pdf_writer=>num( lv_cx + lv_kx ).
    DATA(lv_y_minus) = zabaputil_cl_pdf_writer=>num( lv_cy - lv_ky ).
    DATA(lv_y_plus) = zabaputil_cl_pdf_writer=>num( lv_cy + lv_ky ).

    emit( |{ lv_right } { lv_mid_y } m | &&
          |{ lv_right } { lv_y_plus } { lv_x_plus } { lv_top } { lv_mid_x } { lv_top } c | &&
          |{ lv_x_minus } { lv_top } { lv_left } { lv_y_plus } { lv_left } { lv_mid_y } c | &&
          |{ lv_left } { lv_y_minus } { lv_x_minus } { lv_bottom } { lv_mid_x } { lv_bottom } c | &&
          |{ lv_x_plus } { lv_bottom } { lv_right } { lv_y_minus } { lv_right } { lv_mid_y } c | &&
          paint( style ) ).
    result = me.

  ENDMETHOD.

  METHOD image.

    DATA lv_w TYPE f.
    DATA lv_h TYPE f.
    DATA lv_y TYPE f.

    result = me.
    IF image IS NOT BOUND.
      RETURN.
    ENDIF.

    lv_w = w.
    lv_h = h.
    IF lv_w <= 0 AND lv_h <= 0.
      lv_w = image->width( ) / mv_scale.
      lv_h = image->height( ) / mv_scale.
    ELSEIF lv_h <= 0.
      lv_h = lv_w * image->height( ) / image->width( ).
    ELSEIF lv_w <= 0.
      lv_w = lv_h * image->width( ) / image->height( ).
    ENDIF.

    " an image is one unit square in its own space: scale it, move it
    lv_y = y.
    emit( |q { pl( lv_w ) } 0 0 { pl( lv_h ) } { px( x ) } { py( lv_y + lv_h ) } cm /{ image->resource( ) } Do Q| ).

  ENDMETHOD.

  METHOD link.

    DATA lv_y TYPE f.
    DATA lv_h TYPE f.
    DATA lv_x TYPE f.
    DATA lv_w TYPE f.

    lv_x = x.
    lv_y = y.
    lv_w = w.
    lv_h = h.
    " the address as a hex string: nothing in it has to be escaped
    INSERT |<< /Type /Annot /Subtype /Link /Rect [{ px( lv_x ) } { py( lv_y + lv_h ) } | &&
           |{ px( lv_x + lv_w ) } { py( lv_y ) }] /Border [0 0 0] | &&
           |/A << /S /URI /URI <{ zabaputil_cl_pdf_writer=>hex( url ) }> >> >>| INTO TABLE mt_annotation.
    result = me.

  ENDMETHOD.

  METHOD raw.

    emit( CONV string( val ) ).
    result = me.

  ENDMETHOD.

  METHOD content.

    result = mv_content.

  ENDMETHOD.

  METHOD annotations.

    result = mt_annotation.

  ENDMETHOD.

ENDCLASS.

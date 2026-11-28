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

    "! Keep the graphics state - colours, line width, rotation, clipping,
    "! transparency - to come back to it with restore( )
    METHODS save
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! Back to the graphics state of the last save( )
    METHODS restore
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! Everything that follows is turned around a point - until restore( )
    "! @parameter angle | in degrees, counter-clockwise
    "! @parameter x     | the point it turns around
    "! @parameter y     | the point it turns around
    METHODS rotate
      IMPORTING
        angle         TYPE numeric
        x             TYPE numeric
        y             TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! Everything that follows is drawn inside this rectangle only - until
    "! restore( )
    METHODS clip_rect
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
        w             TYPE numeric
        h             TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! How opaque what follows is: 1 opaque, 0 invisible - until restore( )
    "! or the next call
    "! @parameter fill   | of fillings and texts
    "! @parameter stroke | of lines and outlines; -1 for the same as fill
    METHODS set_alpha
      IMPORTING
        fill          TYPE numeric
        stroke        TYPE numeric DEFAULT -1
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! A rectangle with rounded corners
    "! @parameter r     | the radius of the corners
    "! @parameter style | D outline, F filled, DF both
    METHODS rounded_rect
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
        w             TYPE numeric
        h             TYPE numeric
        r             TYPE numeric
        style         TYPE clike DEFAULT `D`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! A free path: move_to( ) starts it (or a part of it), line_to( ) and
    "! curve_to( ) go on, close_path( ) goes back to the start of the part,
    "! draw_path( ) draws all of it
    METHODS move_to
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    METHODS line_to
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! A Bezier curve to x3, y3 with the control points x1, y1 and x2, y2
    METHODS curve_to
      IMPORTING
        x1            TYPE numeric
        y1            TYPE numeric
        x2            TYPE numeric
        y2            TYPE numeric
        x3            TYPE numeric
        y3            TYPE numeric
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    METHODS close_path
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! @parameter style | D outline, F filled, DF both
    METHODS draw_path
      IMPORTING
        style         TYPE clike DEFAULT `D`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! One line of text, turned around its start - a watermark, a label
    "! along an edge
    "! @parameter x     | where the text starts
    "! @parameter y     | the baseline
    "! @parameter angle | in degrees, counter-clockwise
    "! @parameter align | L, C or R: the text starts, is centred or ends at x, y
    METHODS text_rotated
      IMPORTING
        x             TYPE numeric
        y             TYPE numeric
        val           TYPE clike
        angle         TYPE numeric
        align         TYPE clike DEFAULT `L`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf_page.

    "! The transparencies the page uses, by the names of their graphics
    "! states - `GS500 0.5 0.5` - for the resources of the document
    METHODS alphas
      RETURNING
        VALUE(result) TYPE string_table.

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
    DATA mt_alpha      TYPE string_table.
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

    " a standard font with a fallback font: the characters it does not
    " have are written in the other one, piece by piece
    DATA lo_std TYPE REF TO zabaputil_cl_pdf_font_std.
    TRY.
        lo_std ?= mo_font.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
    ENDTRY.
    IF lo_std IS BOUND AND lo_std->fallback( ) IS BOUND.
      DATA(lt_part) = lo_std->parts( lv_text ).
      IF lines( lt_part ) > 1 OR lt_part[ 1 ]-fallback = abap_true.
        LOOP AT lt_part INTO DATA(ls_part).
          DATA(lo_font) = COND #( WHEN ls_part-fallback = abap_true THEN lo_std->fallback( ) ELSE mo_font ).
          lv_show = |BT /{ lo_font->resource( ) } { zabaputil_cl_pdf_writer=>num( mv_font_size ) } Tf | &&
                    |{ px( lv_x ) } { py( y ) } Td { lo_font->encode( ls_part-text ) } Tj ET|.
          IF mv_text_color = mv_fill_color.
            emit( lv_show ).
          ELSE.
            emit( |q { mv_text_color } rg { lv_show } Q| ).
          ENDIF.
          lv_x = lv_x + lo_font->width( val  = ls_part-text
                                        size = mv_font_size ) / mv_scale.
        ENDLOOP.
        RETURN.
      ENDIF.
    ENDIF.

    lv_show = |BT /{ mo_font->resource( ) } { zabaputil_cl_pdf_writer=>num( mv_font_size ) } Tf | &&
              |{ px( lv_x ) } { py( y ) } Td { mo_font->encode( lv_text ) } Tj ET|.
    IF mv_text_color = mv_fill_color.
      emit( lv_show ).
    ELSE.
      emit( |q { mv_text_color } rg { lv_show } Q| ).
    ENDIF.

  ENDMETHOD.

  METHOD save.

    emit( `q` ).
    result = me.

  ENDMETHOD.

  METHOD restore.

    emit( `Q` ).
    result = me.

  ENDMETHOD.

  METHOD rotate.

    DATA lv_rad TYPE f.
    DATA lv_cos TYPE f.
    DATA lv_sin TYPE f.
    DATA lv_x   TYPE f.
    DATA lv_y   TYPE f.

    " around the point, in the points of the page with y upwards
    lv_rad = angle.
    lv_rad = lv_rad * '3.14159265358979' / 180.
    lv_cos = cos( lv_rad ).
    lv_sin = sin( lv_rad ).
    lv_x = x.
    lv_x = lv_x * mv_scale.
    lv_y = y.
    lv_y = mv_height - lv_y * mv_scale.
    emit( |{ zabaputil_cl_pdf_writer=>num( lv_cos ) } { zabaputil_cl_pdf_writer=>num( lv_sin ) } | &&
          |{ zabaputil_cl_pdf_writer=>num( 0 - lv_sin ) } { zabaputil_cl_pdf_writer=>num( lv_cos ) } | &&
          |{ zabaputil_cl_pdf_writer=>num( lv_x - lv_cos * lv_x + lv_sin * lv_y ) } | &&
          |{ zabaputil_cl_pdf_writer=>num( lv_y - lv_sin * lv_x - lv_cos * lv_y ) } cm| ).
    result = me.

  ENDMETHOD.

  METHOD clip_rect.

    DATA lv_y TYPE f.
    DATA lv_h TYPE f.

    lv_y = y.
    lv_h = h.
    emit( |{ px( x ) } { py( lv_y + lv_h ) } { pl( w ) } { pl( h ) } re W n| ).
    result = me.

  ENDMETHOD.

  METHOD set_alpha.

    DATA lv_fill   TYPE f.
    DATA lv_stroke TYPE f.

    lv_fill = nmax( val1 = 0
                    val2 = nmin( val1 = 1
                                 val2 = fill ) ).
    lv_stroke = stroke.
    IF lv_stroke < 0.
      lv_stroke = lv_fill.
    ENDIF.
    lv_stroke = nmax( val1 = 0
                      val2 = nmin( val1 = 1
                                   val2 = lv_stroke ) ).

    " a graphics state per pair of values, named after them
    DATA(lv_fill_name) = round( val = lv_fill * 1000 dec = 0 ).
    DATA(lv_stroke_name) = round( val = lv_stroke * 1000 dec = 0 ).
    DATA(lv_name) = |GS{ CONV i( lv_fill_name ) }_{ CONV i( lv_stroke_name ) }|.
    DATA(lv_entry) = |{ lv_name } { zabaputil_cl_pdf_writer=>num( lv_fill ) } { zabaputil_cl_pdf_writer=>num( lv_stroke ) }|.
    IF NOT line_exists( mt_alpha[ table_line = lv_entry ] ).
      INSERT lv_entry INTO TABLE mt_alpha.
    ENDIF.
    emit( |/{ lv_name } gs| ).
    result = me.

  ENDMETHOD.

  METHOD alphas.

    result = mt_alpha.

  ENDMETHOD.

  METHOD rounded_rect.

    DATA lv_x  TYPE f.
    DATA lv_y  TYPE f.
    DATA lv_w  TYPE f.
    DATA lv_h  TYPE f.
    DATA lv_r  TYPE f.
    DATA lv_k  TYPE f.
    " the distance of the control points that comes closest to a circle
    CONSTANTS lc_kappa TYPE f VALUE '0.5523'.

    lv_x = x.
    lv_y = y.
    lv_w = w.
    lv_h = h.
    lv_r = r.
    lv_r = nmin( val1 = lv_r
                 val2 = nmin( val1 = lv_w / 2
                              val2 = lv_h / 2 ) ).
    IF lv_r <= 0.
      result = rect( x     = x
                     y     = y
                     w     = w
                     h     = h
                     style = style ).
      RETURN.
    ENDIF.
    lv_k = lv_r * lc_kappa.

    " clockwise on the page from the top edge, a quarter circle per corner
    move_to( x = lv_x + lv_r
             y = lv_y ).
    line_to( x = lv_x + lv_w - lv_r
             y = lv_y ).
    curve_to( x1 = lv_x + lv_w - lv_r + lv_k
              y1 = lv_y
              x2 = lv_x + lv_w
              y2 = lv_y + lv_r - lv_k
              x3 = lv_x + lv_w
              y3 = lv_y + lv_r ).
    line_to( x = lv_x + lv_w
             y = lv_y + lv_h - lv_r ).
    curve_to( x1 = lv_x + lv_w
              y1 = lv_y + lv_h - lv_r + lv_k
              x2 = lv_x + lv_w - lv_r + lv_k
              y2 = lv_y + lv_h
              x3 = lv_x + lv_w - lv_r
              y3 = lv_y + lv_h ).
    line_to( x = lv_x + lv_r
             y = lv_y + lv_h ).
    curve_to( x1 = lv_x + lv_r - lv_k
              y1 = lv_y + lv_h
              x2 = lv_x
              y2 = lv_y + lv_h - lv_r + lv_k
              x3 = lv_x
              y3 = lv_y + lv_h - lv_r ).
    line_to( x = lv_x
             y = lv_y + lv_r ).
    curve_to( x1 = lv_x
              y1 = lv_y + lv_r - lv_k
              x2 = lv_x + lv_r - lv_k
              y2 = lv_y
              x3 = lv_x + lv_r
              y3 = lv_y ).
    close_path( ).
    result = draw_path( style ).

  ENDMETHOD.

  METHOD move_to.

    emit( |{ px( x ) } { py( y ) } m| ).
    result = me.

  ENDMETHOD.

  METHOD line_to.

    emit( |{ px( x ) } { py( y ) } l| ).
    result = me.

  ENDMETHOD.

  METHOD curve_to.

    emit( |{ px( x1 ) } { py( y1 ) } { px( x2 ) } { py( y2 ) } { px( x3 ) } { py( y3 ) } c| ).
    result = me.

  ENDMETHOD.

  METHOD close_path.

    emit( `h` ).
    result = me.

  ENDMETHOD.

  METHOD draw_path.

    emit( paint( style ) ).
    result = me.

  ENDMETHOD.

  METHOD text_rotated.

    DATA lv_rad TYPE f.
    DATA lv_cos TYPE f.
    DATA lv_sin TYPE f.
    DATA lv_x   TYPE f.
    DATA lv_y   TYPE f.
    DATA lv_off TYPE f.

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

    lv_rad = angle.
    lv_rad = lv_rad * '3.14159265358979' / 180.
    lv_cos = cos( lv_rad ).
    lv_sin = sin( lv_rad ).

    " the start of the text, moved back along its direction to centre or end it
    DATA(lv_align) = to_upper( align ).
    CASE lv_align.
      WHEN `R`.
        lv_off = mo_font->width( val  = lv_text
                                 size = mv_font_size ).
      WHEN `C`.
        lv_off = mo_font->width( val  = lv_text
                                 size = mv_font_size ) / 2.
    ENDCASE.
    lv_x = x.
    lv_x = lv_x * mv_scale - lv_off * lv_cos.
    lv_y = y.
    lv_y = mv_height - lv_y * mv_scale - lv_off * lv_sin.

    DATA(lv_show) = |BT /{ mo_font->resource( ) } { zabaputil_cl_pdf_writer=>num( mv_font_size ) } Tf | &&
                    |{ zabaputil_cl_pdf_writer=>num( lv_cos ) } { zabaputil_cl_pdf_writer=>num( lv_sin ) } | &&
                    |{ zabaputil_cl_pdf_writer=>num( 0 - lv_sin ) } { zabaputil_cl_pdf_writer=>num( lv_cos ) } | &&
                    |{ zabaputil_cl_pdf_writer=>num( lv_x ) } { zabaputil_cl_pdf_writer=>num( lv_y ) } Tm | &&
                    |{ mo_font->encode( lv_text ) } Tj ET|.
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

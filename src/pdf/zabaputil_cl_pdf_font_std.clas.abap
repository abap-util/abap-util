CLASS zabaputil_cl_pdf_font_std DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " One of the standard fonts every PDF viewer brings along - nothing is
    " embedded, the file names the font. The text is written in
    " WinAnsiEncoding (Windows-1252): Western European scripts and the
    " euro sign; a character outside of it becomes a question mark. For
    " any other script embed a font file, zabaputil_cl_pdf_font_ttf.
    "
    " The widths are the ones of the Adobe font metrics (AFM) files of the
    " Core 14 fonts, which Adobe lets everybody use, copy and distribute:
    " Copyright (c) 1985, 1987, 1989, 1990, 1997 Adobe Systems Incorporated.
    " All Rights Reserved.

    INTERFACES zabaputil_if_pdf_font.

    "! @parameter name     | Helvetica, Times-Roman or Courier, plain or with
    "!                       -Bold, -Oblique (Times: -Italic), -BoldOblique
    "!                       (Times: -BoldItalic)
    "! @parameter resource | the name in the resource dictionary, `F1`
    METHODS constructor
      IMPORTING
        name     TYPE clike
        resource TYPE clike.

    "! The WinAnsiEncoding code of a Unicode code point, 0 for a character
    "! the encoding does not have
    CLASS-METHODS winansi
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_name     TYPE string.
    DATA mv_resource TYPE string.
    DATA mv_ascent   TYPE i.
    DATA mv_descent  TYPE i.
    " the widths of the codes 32 to 255 in thousandths of the font size;
    " empty for Courier, where every character is 600 wide
    DATA mt_width    TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    "! The codes of a text in WinAnsiEncoding, a question mark for what
    "! the encoding does not have
    METHODS codes
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_t_codepoint.

    "! The widths of the codes 32 to 255, blank-separated
    CLASS-METHODS widths
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_font_std IMPLEMENTATION.

  METHOD constructor.

    DATA lv_metrics TYPE string.
    DATA lv_width   TYPE i.

    mv_name = name.
    mv_resource = resource.

    " ascender and descender of the AFM files; the oblique and italic cuts
    " have the widths of the upright ones, except in Times
    CASE mv_name.
      WHEN `Helvetica` OR `Helvetica-Oblique`.
        lv_metrics = `Helvetica`.
        mv_ascent = 718.
        mv_descent = 207.
      WHEN `Helvetica-Bold` OR `Helvetica-BoldOblique`.
        lv_metrics = `Helvetica-Bold`.
        mv_ascent = 718.
        mv_descent = 207.
      WHEN `Times-Roman`.
        lv_metrics = mv_name.
        mv_ascent = 683.
        mv_descent = 217.
      WHEN `Times-Bold`.
        lv_metrics = mv_name.
        mv_ascent = 676.
        mv_descent = 205.
      WHEN `Times-Italic`.
        lv_metrics = mv_name.
        mv_ascent = 683.
        mv_descent = 205.
      WHEN `Times-BoldItalic`.
        lv_metrics = mv_name.
        mv_ascent = 699.
        mv_descent = 205.
      WHEN `Courier` OR `Courier-Oblique`.
        mv_ascent = 629.
        mv_descent = 157.
      WHEN `Courier-Bold` OR `Courier-BoldOblique`.
        mv_ascent = 626.
        mv_descent = 142.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: { mv_name } is not a standard font|.
    ENDCASE.

    IF lv_metrics IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_widths) = widths( lv_metrics ).
    SPLIT lv_widths AT ` ` INTO TABLE DATA(lt_width).
    LOOP AT lt_width INTO DATA(lv_text).
      lv_width = lv_text.
      INSERT lv_width INTO TABLE mt_width.
    ENDLOOP.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~resource.

    result = mv_resource.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~encode.

    DATA lv_code TYPE i.
    DATA lv_x    TYPE x LENGTH 1.
    DATA lv_hex  TYPE string.

    LOOP AT codes( val ) INTO lv_code.
      lv_x = lv_code.
      lv_hex = lv_x.
      result = result && lv_hex.
    ENDLOOP.
    result = |<{ result }>|.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~width.

    DATA lv_code  TYPE i.
    DATA lv_sum   TYPE i.
    DATA lv_size  TYPE f.
    DATA lv_index TYPE i.

    LOOP AT codes( val ) INTO lv_code.
      IF lv_code < 32.
        CONTINUE.
      ENDIF.
      IF mt_width IS INITIAL.
        lv_sum = lv_sum + 600.
      ELSE.
        lv_index = lv_code - 31.
        lv_sum = lv_sum + mt_width[ lv_index ].
      ENDIF.
    ENDLOOP.

    lv_size = size.
    result = lv_sum * lv_size / 1000.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~ascent.

    DATA lv_size TYPE f.

    lv_size = size.
    result = mv_ascent * lv_size / 1000.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~descent.

    DATA lv_size TYPE f.

    lv_size = size.
    result = mv_descent * lv_size / 1000.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~write.

    result = writer->add_object(
        |<< /Type /Font /Subtype /Type1 /BaseFont /{ mv_name } /Encoding /WinAnsiEncoding >>| ).

  ENDMETHOD.

  METHOD codes.

    DATA lv_cp   TYPE i.
    DATA lv_code TYPE i.

    LOOP AT zabaputil_cl_util_context=>c_get_codepoints( val ) INTO lv_cp.
      lv_code = winansi( lv_cp ).
      IF lv_code = 0.
        lv_code = 63.
      ENDIF.
      INSERT lv_code INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD winansi.

    " ASCII and the upper half of Latin-1 keep their code; the 27
    " characters Windows-1252 puts between 128 and 159 come from all over
    " Unicode
    IF ( val >= 32 AND val <= 126 ) OR ( val >= 160 AND val <= 255 ).
      result = val.
      RETURN.
    ENDIF.

    CASE val.
      WHEN 8364.
        result = 128.
      WHEN 8218.
        result = 130.
      WHEN 402.
        result = 131.
      WHEN 8222.
        result = 132.
      WHEN 8230.
        result = 133.
      WHEN 8224.
        result = 134.
      WHEN 8225.
        result = 135.
      WHEN 710.
        result = 136.
      WHEN 8240.
        result = 137.
      WHEN 352.
        result = 138.
      WHEN 8249.
        result = 139.
      WHEN 338.
        result = 140.
      WHEN 381.
        result = 142.
      WHEN 8216.
        result = 145.
      WHEN 8217.
        result = 146.
      WHEN 8220.
        result = 147.
      WHEN 8221.
        result = 148.
      WHEN 8226.
        result = 149.
      WHEN 8211.
        result = 150.
      WHEN 8212.
        result = 151.
      WHEN 732.
        result = 152.
      WHEN 8482.
        result = 153.
      WHEN 353.
        result = 154.
      WHEN 8250.
        result = 155.
      WHEN 339.
        result = 156.
      WHEN 382.
        result = 158.
      WHEN 376.
        result = 159.
      WHEN OTHERS.
        result = 0.
    ENDCASE.

  ENDMETHOD.

  METHOD widths.

    CASE name.
      WHEN `Helvetica`.
        result = `278 278 355 556 556 889 667 191 333 333 389 584 278 333 278 278 556 556 556 556 556 556 556 556 556 ` &&
                 `556 278 278 584 584 584 556 1015 667 667 722 722 667 611 778 722 278 500 667 556 833 722 778 667 778 ` &&
                 `722 667 611 722 667 944 667 667 611 278 278 278 469 556 333 556 556 500 556 556 278 556 556 222 222 ` &&
                 `500 222 833 556 556 556 556 333 500 278 556 500 722 500 500 500 334 260 334 584 350 556 0 222 556 ` &&
                 `333 1000 556 556 333 1000 667 333 1000 0 611 0 0 222 222 333 333 350 556 1000 333 1000 500 333 944 0 ` &&
                 `500 667 278 333 556 556 556 556 260 556 333 737 370 556 584 333 737 333 400 584 333 333 333 556 537 ` &&
                 `278 333 333 365 556 834 834 834 611 667 667 667 667 667 667 1000 722 667 667 667 667 278 278 278 278 ` &&
                 `722 722 778 778 778 778 778 584 778 722 722 722 722 667 667 611 556 556 556 556 556 556 889 500 556 ` &&
                 `556 556 556 278 278 278 278 556 556 556 556 556 556 556 584 611 556 556 556 556 500 556 500`.
      WHEN `Helvetica-Bold`.
        result = `278 333 474 556 556 889 722 238 333 333 389 584 278 333 278 278 556 556 556 556 556 556 556 556 556 ` &&
                 `556 333 333 584 584 584 611 975 722 722 722 722 667 611 778 722 278 556 722 611 833 722 778 667 778 ` &&
                 `722 667 611 722 667 944 667 667 611 333 278 333 584 556 333 556 611 556 611 556 333 611 611 278 278 ` &&
                 `556 278 889 611 611 611 611 389 556 333 611 556 778 556 556 500 389 280 389 584 350 556 0 278 556 ` &&
                 `500 1000 556 556 333 1000 667 333 1000 0 611 0 0 278 278 500 500 350 556 1000 333 1000 556 333 944 0 ` &&
                 `500 667 278 333 556 556 556 556 280 556 333 737 370 556 584 333 737 333 400 584 333 333 333 611 556 ` &&
                 `278 333 333 365 556 834 834 834 611 722 722 722 722 722 722 1000 722 667 667 667 667 278 278 278 278 ` &&
                 `722 722 778 778 778 778 778 584 778 722 722 722 722 667 667 611 556 556 556 556 556 556 889 556 556 ` &&
                 `556 556 556 278 278 278 278 611 611 611 611 611 611 611 584 611 611 611 611 611 556 611 556`.
      WHEN `Times-Roman`.
        result = `250 333 408 500 500 833 778 180 333 333 500 564 250 333 250 278 500 500 500 500 500 500 500 500 500 ` &&
                 `500 278 278 564 564 564 444 921 722 667 667 722 611 556 722 722 333 389 722 611 889 722 722 556 722 ` &&
                 `667 556 611 722 722 944 722 722 611 333 278 333 469 500 333 444 500 444 500 444 333 500 500 278 278 ` &&
                 `500 278 778 500 500 500 500 333 389 278 500 500 722 500 500 444 480 200 480 541 350 500 0 333 500 ` &&
                 `444 1000 500 500 333 1000 556 333 889 0 611 0 0 333 333 444 444 350 500 1000 333 980 389 333 722 0 ` &&
                 `444 722 250 333 500 500 500 500 200 500 333 760 276 500 564 333 760 333 400 564 300 300 333 500 453 ` &&
                 `250 333 300 310 500 750 750 750 444 722 722 722 722 722 722 889 667 611 611 611 611 333 333 333 333 ` &&
                 `722 722 722 722 722 722 722 564 722 722 722 722 722 722 556 500 444 444 444 444 444 444 667 444 444 ` &&
                 `444 444 444 278 278 278 278 500 500 500 500 500 500 500 564 500 500 500 500 500 500 500 500`.
      WHEN `Times-Bold`.
        result = `250 333 555 500 500 1000 833 278 333 333 500 570 250 333 250 278 500 500 500 500 500 500 500 500 500 ` &&
                 `500 333 333 570 570 570 500 930 722 667 722 722 667 611 778 778 389 500 778 667 944 722 778 611 778 ` &&
                 `722 556 667 722 722 1000 722 722 667 333 278 333 581 500 333 500 556 444 556 444 333 500 556 278 333 ` &&
                 `556 278 833 556 500 556 556 444 389 333 556 500 722 500 500 444 394 220 394 520 350 500 0 333 500 ` &&
                 `500 1000 500 500 333 1000 556 333 1000 0 667 0 0 333 333 500 500 350 500 1000 333 1000 389 333 722 0 ` &&
                 `444 722 250 333 500 500 500 500 220 500 333 747 300 500 570 333 747 333 400 570 300 300 333 556 540 ` &&
                 `250 333 300 330 500 750 750 750 500 722 722 722 722 722 722 1000 722 667 667 667 667 389 389 389 389 ` &&
                 `722 722 778 778 778 778 778 570 778 722 722 722 722 722 611 556 500 500 500 500 500 500 722 444 444 ` &&
                 `444 444 444 278 278 278 278 500 556 500 500 500 500 500 570 500 556 556 556 556 500 556 500`.
      WHEN `Times-Italic`.
        result = `250 333 420 500 500 833 778 214 333 333 500 675 250 333 250 278 500 500 500 500 500 500 500 500 500 ` &&
                 `500 333 333 675 675 675 500 920 611 611 667 722 611 611 722 722 333 444 667 556 833 667 722 611 722 ` &&
                 `611 500 556 722 611 833 611 556 556 389 278 389 422 500 333 500 500 444 500 444 278 500 500 278 278 ` &&
                 `444 278 722 500 500 500 500 389 389 278 500 444 667 444 444 389 400 275 400 541 350 500 0 333 500 ` &&
                 `556 889 500 500 333 1000 500 333 944 0 556 0 0 333 333 556 556 350 500 889 333 980 389 333 667 0 389 ` &&
                 `556 250 389 500 500 500 500 275 500 333 760 276 500 675 333 760 333 400 675 300 300 333 500 523 250 ` &&
                 `333 300 310 500 750 750 750 500 611 611 611 611 611 611 889 667 611 611 611 611 333 333 333 333 722 ` &&
                 `667 722 722 722 722 722 675 722 722 722 722 722 556 611 500 500 500 500 500 500 500 667 444 444 444 ` &&
                 `444 444 278 278 278 278 500 500 500 500 500 500 500 675 500 500 500 500 500 444 500 444`.
      WHEN `Times-BoldItalic`.
        result = `250 389 555 500 500 833 778 278 333 333 500 570 250 333 250 278 500 500 500 500 500 500 500 500 500 ` &&
                 `500 333 333 570 570 570 500 832 667 667 667 722 667 667 722 778 389 500 667 611 889 722 722 611 722 ` &&
                 `667 556 611 722 667 889 667 611 611 333 278 333 570 500 333 500 500 444 500 444 333 500 556 278 278 ` &&
                 `500 278 778 556 500 500 500 389 389 278 556 444 667 500 444 389 348 220 348 570 350 500 0 333 500 ` &&
                 `500 1000 500 500 333 1000 556 333 944 0 611 0 0 333 333 500 500 350 500 1000 333 1000 389 333 722 0 ` &&
                 `389 611 250 389 500 500 500 500 220 500 333 747 266 500 606 333 747 333 400 570 300 300 333 576 500 ` &&
                 `250 333 300 300 500 750 750 750 500 667 667 667 667 667 667 944 667 667 667 667 667 389 389 389 389 ` &&
                 `722 722 722 722 722 722 722 570 722 722 722 722 722 611 611 500 500 500 500 500 500 500 722 444 444 ` &&
                 `444 444 444 278 278 278 278 500 556 500 500 500 500 500 570 500 556 556 556 556 444 500 444`.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

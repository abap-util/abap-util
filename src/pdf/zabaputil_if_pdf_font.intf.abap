INTERFACE zabaputil_if_pdf_font
  PUBLIC.

  " What a page and the layout need from a font: how a text is written
  " into a content stream, and how wide it is. zabaputil_cl_pdf_font_std
  " (the standard fonts every viewer has) and zabaputil_cl_pdf_font_ttf
  " (an embedded TrueType file) implement it.

  "! The name of the font in the resource dictionary of the pages, `F1`
  METHODS resource
    RETURNING
      VALUE(result) TYPE string.

  "! A text as the string operand of the show operator: the character
  "! codes of this font in hex, with the angle brackets
  METHODS encode
    IMPORTING
      val           TYPE clike
    RETURNING
      VALUE(result) TYPE string.

  "! The width of a text in points
  "! @parameter val  | the text
  "! @parameter size | the font size in points
  METHODS width
    IMPORTING
      val           TYPE clike
      size          TYPE numeric
    RETURNING
      VALUE(result) TYPE f.

  "! How far the font reaches above the baseline, in points
  "! @parameter size | the font size in points
  METHODS ascent
    IMPORTING
      size          TYPE numeric
    RETURNING
      VALUE(result) TYPE f.

  "! How far the font reaches below the baseline, in points - a positive number
  "! @parameter size | the font size in points
  METHODS descent
    IMPORTING
      size          TYPE numeric
    RETURNING
      VALUE(result) TYPE f.

  "! Write the objects of the font
  "! @parameter writer | the file
  "! @parameter result | the object number of the font dictionary
  METHODS write
    IMPORTING
      writer        TYPE REF TO zabaputil_cl_pdf_writer
    RETURNING
      VALUE(result) TYPE i.

ENDINTERFACE.

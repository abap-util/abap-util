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

    "! A file embedded into the document - a viewer lists it as an
    "! attachment; an e-invoice carries its XML so
    "! @parameter name         | the file name, `factur-x.xml`
    "! @parameter content      | the bytes of the file
    "! @parameter mime         | its media type, `text/xml`
    "! @parameter description  | what it is, for the viewer
    "! @parameter relationship | how it belongs to the document (PDF/A-3):
    "!                           Source, Data, Alternative, Supplement or Unspecified
    METHODS attach
      IMPORTING
        name          TYPE clike
        content       TYPE xstring
        mime          TYPE clike DEFAULT `application/octet-stream`
        description   TYPE clike OPTIONAL
        relationship  TYPE clike DEFAULT `Unspecified`
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

    "! Write the document as PDF/A-3b, for archiving: XMP metadata, an
    "! output intent with an sRGB profile, an identifier. Every font has
    "! to be embedded - render( ) raises for a standard font
    METHODS set_pdfa
      IMPORTING
        val           TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

    "! A description of the XMP metadata of a PDF/A file of its own - an
    "! rdf:Description element, as ZUGFeRD / Factur-X asks for
    METHODS add_xmp
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_pdf.

    "! An e-invoice: the document becomes PDF/A-3b with the invoice XML
    "! (CII) attached, and the metadata say what it is - ZUGFeRD 2 /
    "! Factur-X 1.0. The XML is as the caller made it; nothing checks it
    "! @parameter xml       | the bytes of the invoice XML
    "! @parameter level     | MINIMUM, BASIC WL, BASIC, EN 16931, EXTENDED or XRECHNUNG
    "! @parameter file_name | factur-x.xml - xrechnung.xml for XRECHNUNG
    METHODS attach_invoice
      IMPORTING
        xml           TYPE xstring
        level         TYPE clike DEFAULT `EN 16931`
        file_name     TYPE clike OPTIONAL
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
    TYPES:
      BEGIN OF ty_s_attachment,
        name         TYPE string,
        content      TYPE xstring,
        mime         TYPE string,
        description  TYPE string,
        relationship TYPE string,
      END OF ty_s_attachment.
    DATA mt_attachment TYPE STANDARD TABLE OF ty_s_attachment WITH EMPTY KEY.
    DATA mv_pdfa       TYPE abap_bool.
    DATA mt_xmp        TYPE string_table.

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

    "! The embedded files: the entries of the catalog for them, empty
    "! without any
    METHODS write_attachments
      IMPORTING
        writer        TYPE REF TO zabaputil_cl_pdf_writer
      RETURNING
        VALUE(result) TYPE string.

    "! The XMP metadata of a PDF/A file
    "! @parameter date | the time of the document, YYYY-MM-DDThh:mm:ss
    METHODS xmp
      IMPORTING
        date          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! An ICC profile of sRGB, made for this class - no copyright
    CLASS-METHODS srgb_profile
      RETURNING
        VALUE(result) TYPE xstring.

    "! A text as a PDF name: what is no letter, digit, dot, plus or
    "! hyphen as #XX
    CLASS-METHODS pdf_name
      IMPORTING
        val           TYPE clike
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

  METHOD attach_invoice.

    DATA lv_relationship TYPE string.

    DATA(lv_level) = to_upper( condense( level ) ).
    IF lv_level <> `MINIMUM` AND lv_level <> `BASIC WL` AND lv_level <> `BASIC` AND lv_level <> `EN 16931`
        AND lv_level <> `EXTENDED` AND lv_level <> `XRECHNUNG`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: { level } is no level of Factur-X (MINIMUM, BASIC WL, BASIC, EN 16931, EXTENDED, XRECHNUNG)|.
    ENDIF.
    DATA(lv_name) = CONV string( file_name ).
    IF lv_name IS INITIAL.
      lv_name = COND #( WHEN lv_level = `XRECHNUNG` THEN `xrechnung.xml` ELSE `factur-x.xml` ).
    ENDIF.
    " MINIMUM and BASIC WL are no full invoice: the XML is data beside the
    " document, from BASIC on it is the same invoice once more
    lv_relationship = COND #( WHEN lv_level = `MINIMUM` OR lv_level = `BASIC WL` THEN `Data` ELSE `Alternative` ).

    set_pdfa( ).
    attach( name         = lv_name
            content      = xml
            mime         = `text/xml`
            description  = `Factur-X invoice`
            relationship = lv_relationship ).

    " the properties of Factur-X, and the PDF/A extension schema that
    " describes them
    DATA(lv_fx) = `urn:factur-x:pdfa:CrossIndustryDocument:invoice:1p0#`.
    add_xmp( |<rdf:Description rdf:about="" xmlns:fx="{ lv_fx }">| &&
             |<fx:DocumentType>INVOICE</fx:DocumentType>| &&
             |<fx:DocumentFileName>{ zabaputil_cl_util_context=>c_escape_xml( lv_name ) }</fx:DocumentFileName>| &&
             |<fx:Version>1.0</fx:Version>| &&
             |<fx:ConformanceLevel>{ lv_level }</fx:ConformanceLevel>| &&
             |</rdf:Description>| ).
    DATA(lv_properties) = ``.
    DATA(lt_property) = VALUE string_table( ( `DocumentFileName:The name of the embedded XML document` )
                                            ( `DocumentType:The type of the hybrid document in capital letters, e.g. INVOICE or ORDER` )
                                            ( `Version:The actual version of the standard applying to the embedded XML document` )
                                            ( `ConformanceLevel:The conformance level of the embedded XML document` ) ).
    LOOP AT lt_property INTO DATA(lv_property).
      SPLIT lv_property AT `:` INTO DATA(lv_property_name) DATA(lv_description).
      lv_properties = lv_properties &&
          |<rdf:li rdf:parseType="Resource"><pdfaProperty:name>{ lv_property_name }</pdfaProperty:name>| &&
          |<pdfaProperty:valueType>Text</pdfaProperty:valueType><pdfaProperty:category>external</pdfaProperty:category>| &&
          |<pdfaProperty:description>{ lv_description }</pdfaProperty:description></rdf:li>|.
    ENDLOOP.
    add_xmp( |<rdf:Description rdf:about="" xmlns:pdfaExtension="http://www.aiim.org/pdfa/ns/extension/" | &&
             |xmlns:pdfaSchema="http://www.aiim.org/pdfa/ns/schema#" xmlns:pdfaProperty="http://www.aiim.org/pdfa/ns/property#">| &&
             |<pdfaExtension:schemas><rdf:Bag><rdf:li rdf:parseType="Resource">| &&
             |<pdfaSchema:schema>Factur-X PDFA Extension Schema</pdfaSchema:schema>| &&
             |<pdfaSchema:namespaceURI>{ lv_fx }</pdfaSchema:namespaceURI>| &&
             |<pdfaSchema:prefix>fx</pdfaSchema:prefix>| &&
             |<pdfaSchema:property><rdf:Seq>{ lv_properties }</rdf:Seq></pdfaSchema:property>| &&
             |</rdf:li></rdf:Bag></pdfaExtension:schemas></rdf:Description>| ).
    result = me.

  ENDMETHOD.

  METHOD set_pdfa.

    mv_pdfa = val.
    result = me.

  ENDMETHOD.

  METHOD add_xmp.

    INSERT CONV string( val ) INTO TABLE mt_xmp.
    result = me.

  ENDMETHOD.

  METHOD xmp.

    DATA lv_dc  TYPE string.
    DATA lv_xmp TYPE string.
    DATA lv_pdf TYPE string.

    " the properties of the document, as the info dictionary has them
    LOOP AT mt_info ASSIGNING FIELD-SYMBOL(<info>).
      DATA(lv_text) = zabaputil_cl_util_context=>c_escape_xml( <info>-text ).
      CASE <info>-key.
        WHEN `Title`.
          lv_dc = |{ lv_dc }<dc:title><rdf:Alt><rdf:li xml:lang="x-default">{ lv_text }</rdf:li></rdf:Alt></dc:title>|.
        WHEN `Author`.
          lv_dc = |{ lv_dc }<dc:creator><rdf:Seq><rdf:li>{ lv_text }</rdf:li></rdf:Seq></dc:creator>|.
        WHEN `Subject`.
          lv_dc = |{ lv_dc }<dc:description><rdf:Alt><rdf:li xml:lang="x-default">{ lv_text }</rdf:li></rdf:Alt></dc:description>|.
        WHEN `Keywords`.
          lv_pdf = |{ lv_pdf }<pdf:Keywords>{ lv_text }</pdf:Keywords>|.
        WHEN `Creator`.
          lv_xmp = |{ lv_xmp }<xmp:CreatorTool>{ lv_text }</xmp:CreatorTool>|.
      ENDCASE.
    ENDLOOP.

    result = |<?xpacket begin="{ zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `EFBBBF` ) ) }" | &&
             |id="W5M0MpCehiHzreSzNTczkc9d"?>\n| &&
             |<x:xmpmeta xmlns:x="adobe:ns:meta/">\n| &&
             |<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n| &&
             |<rdf:Description rdf:about="" xmlns:pdfaid="http://www.aiim.org/pdfa/ns/id/">| &&
             |<pdfaid:part>3</pdfaid:part><pdfaid:conformance>B</pdfaid:conformance></rdf:Description>\n| &&
             |<rdf:Description rdf:about="" xmlns:dc="http://purl.org/dc/elements/1.1/">| &&
             |<dc:format>application/pdf</dc:format>{ lv_dc }</rdf:Description>\n| &&
             |<rdf:Description rdf:about="" xmlns:xmp="http://ns.adobe.com/xap/1.0/">| &&
             |<xmp:CreateDate>{ date }</xmp:CreateDate><xmp:ModifyDate>{ date }</xmp:ModifyDate>{ lv_xmp }</rdf:Description>\n| &&
             |<rdf:Description rdf:about="" xmlns:pdf="http://ns.adobe.com/pdf/1.3/">| &&
             |<pdf:Producer>abap-util</pdf:Producer>{ lv_pdf }</rdf:Description>\n|.
    LOOP AT mt_xmp INTO DATA(lv_description).
      result = |{ result }{ lv_description }\n|.
    ENDLOOP.
    result = |{ result }</rdf:RDF>\n</x:xmpmeta>\n<?xpacket end="w"?>|.

  ENDMETHOD.

  METHOD srgb_profile.

    " v2, a display: the sRGB primaries adapted to D50, the white point,
    " and the sRGB tone curve as 256 points
    DATA lv_hex TYPE string.
    lv_hex = `000003DC6E6F6E65021000006D6E74725247422058595A2007EA00010001000000000000616373704150504C00000000` &&
             `6E6F6E656E6F6E650000000000000000000000000000F6D6000100000000D32D6E6F6E65000000000000000000000000` &&
             `00000000000000000000000000000000000000000000000000000000000000000000000964657363000000F00000006C` &&
             `637072740000015C000000217774707400000180000000147258595A00000194000000146758595A000001A800000014` &&
             `6258595A000001BC0000001472545243000001D00000020C67545243000001D00000020C62545243000001D00000020C` &&
             `646573630000000000000012735247422049454336313936362D322E3100000000000000000000000000000000000000` &&
             `000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000` &&
             `00000000000000000000000074657874000000004E6F20636F707972696768742C2075736520667265656C7900000000` &&
             `58595A20000000000000F6D6000100000000D32D58595A200000000000006FA4000038F60000038F58595A2000000000` &&
             `000062960000B787000018DC58595A2000000000000024A200000F830000B6CF63757276000000000000010000000014` &&
             `0028003C005000630077008B009F00B300C700DB00F10108012001390154016F018C01AB01CA01EB020E02320257027D` &&
             `02A502CE02F903250353038203B303E50418044D048404BC04F60532056F05AD05ED062F067306B806FE0747079107DD` &&
             `082A087A08CA091D097209C80A200A790AD50B320B910BF20C550CBA0D200D880DF20E5E0ECC0F3C0FAE10211097110E` &&
             `11881203128013001381140414891510159A162516B2174117D3186618FB19931A2C1AC81B661C061CA71D4C1DF21E9A` &&
             `1F441FF120A02150220422B92370242A24E525A32664272627EB28B1297B2A462B142BE32CB62D8A2E612F3A301530F2` &&
             `31D232B433993480356936553742383339253A1A3B123C0B3D073E063F07400A4110421843234430453F46514765487C` &&
             `49954AB14BCF4CF04E134F395061518C52B953E9551B5650578758C159FE5B3D5C7E5DC25F096052619E62ED643E6591` &&
             `66E86840699C6AFA6C5B6DBE6F24708D71F8736674D7764A77C079397AB47C327DB37F3780BD824683D1855F86F08884` &&
             `8A1B8BB48D508EEF9090923593DC9586973298E29A949C499E019FBBA179A339A4FCA6C2A88BAA56AC25ADF6AFCAB1A1` &&
             `B37BB557B737B919BAFFBCE7BED2C0C0C2B1C4A5C69CC895CA92CC91CE94D099D2A1D4ADD6BBD8CCDAE0DCF7DF11E12E` &&
             `E34EE571E797E9C0EBECEE1BF04DF282F4BAF6F5F933FB74FDB8FFFF`.
    result = lv_hex.

  ENDMETHOD.

  METHOD attach.

    DATA ls_attachment TYPE ty_s_attachment.

    ls_attachment-name = name.
    ls_attachment-content = content.
    ls_attachment-mime = mime.
    ls_attachment-description = description.
    ls_attachment-relationship = relationship.
    IF ls_attachment-relationship IS INITIAL.
      ls_attachment-relationship = `Unspecified`.
    ENDIF.
    IF ls_attachment-name IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: an attachment needs a file name`.
    ENDIF.
    DELETE mt_attachment WHERE name = ls_attachment-name.
    INSERT ls_attachment INTO TABLE mt_attachment.
    result = me.

  ENDMETHOD.

  METHOD pdf_name.

    DATA lv_x TYPE x LENGTH 1.

    DATA(lv_bytes) = zabaputil_cl_util_context=>conv_get_xstring_by_string( CONV string( val ) ).
    DATA(lv_off) = 0.
    WHILE lv_off < xstrlen( lv_bytes ).
      lv_x = lv_bytes+lv_off(1).
      DATA(lv_code) = CONV i( lv_x ).
      IF ( lv_code >= 48 AND lv_code <= 57 ) OR ( lv_code >= 65 AND lv_code <= 90 )
          OR ( lv_code >= 97 AND lv_code <= 122 ) OR lv_code = 43 OR lv_code = 45 OR lv_code = 46.
        result = result && zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( lv_x ) ).
      ELSE.
        result = |{ result }#{ lv_x }|.
      ENDIF.
      lv_off = lv_off + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD write_attachments.

    DATA lv_names TYPE string.
    DATA lv_files TYPE string.
    DATA lv_date  TYPE string.

    IF mt_attachment IS INITIAL.
      RETURN.
    ENDIF.

    " the time of the document, in the form of a PDF date
    DATA(lv_stamp) = |{ zabaputil_cl_util_context=>time_get_timestamp( ) }|.
    lv_date = |D:{ substring( val = lv_stamp
                              len = 14 ) }Z|.

    " the name tree wants its keys in the order of their bytes
    DATA(lt_sorted) = mt_attachment.
    SORT lt_sorted BY name.
    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<attachment>).
      DATA(lv_file) = writer->add_stream(
          dict    = |/Type /EmbeddedFile /Subtype /{ pdf_name( <attachment>-mime ) } | &&
                    |/Params << /Size { xstrlen( <attachment>-content ) } /ModDate ({ lv_date }) >>|
          content = <attachment>-content ).
      DATA(lv_spec) = |<< /Type /Filespec /F <{ zabaputil_cl_pdf_writer=>hex( <attachment>-name ) }> | &&
                      |/UF <{ zabaputil_cl_pdf_writer=>hex_utf16( <attachment>-name ) }> | &&
                      |/AFRelationship /{ pdf_name( <attachment>-relationship ) }|.
      IF <attachment>-description IS NOT INITIAL.
        lv_spec = |{ lv_spec } /Desc <{ zabaputil_cl_pdf_writer=>hex_utf16( <attachment>-description ) }>|.
      ENDIF.
      DATA(lv_id) = writer->add_object( |{ lv_spec } /EF << /F { lv_file } 0 R /UF { lv_file } 0 R >> >>| ).
      lv_names = |{ lv_names } <{ zabaputil_cl_pdf_writer=>hex_utf16( <attachment>-name ) }> { lv_id } 0 R|.
      lv_files = |{ lv_files } { lv_id } 0 R|.
    ENDLOOP.

    result = | /Names << /EmbeddedFiles << /Names [{ lv_names } ] >> >> /AF [{ lv_files } ]|.

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

    " PDF/A embeds every font: a standard font is a reason to stop
    IF mv_pdfa = abap_true.
      DATA lo_std TYPE REF TO zabaputil_cl_pdf_font_std.
      LOOP AT mt_font ASSIGNING FIELD-SYMBOL(<std>).
        TRY.
            lo_std ?= <std>-font.
            RAISE EXCEPTION TYPE zabaputil_cx_util_error
              EXPORTING
                val = |PDF/A: the standard font { lo_std->name( ) } is not embedded - use font_ttf( )|.
          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.
      ENDLOOP.
      " nor does it show the missing glyph
      DATA(lt_missing) = missing_chars( ).
      IF lt_missing IS NOT INITIAL.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF/A: the fonts have no glyph for { concat_lines_of( table = lt_missing
                                                                         sep   = ` ` ) }|.
      ENDIF.
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
        " PDF/A: an annotation is printed
        IF mv_pdfa = abap_true.
          lv_annotation = replace( val  = lv_annotation
                                   sub  = `/Subtype /Link `
                                   with = `/Subtype /Link /F 4 ` ).
        ENDIF.
        lv_id = lo_writer->add_object( lv_annotation ).
        lv_annots = lv_annots && | { lv_id } 0 R|.
      ENDLOOP.
      LOOP AT lo_page->gotos( ) INTO DATA(ls_goto).
        IF ls_goto-page < 1 OR ls_goto-page > lines( mt_page ).
          CONTINUE.
        ENDIF.
        lv_id = lo_writer->add_object( |<< /Type /Annot /Subtype /Link{ COND #( WHEN mv_pdfa = abap_true THEN ` /F 4` ) } | &&
                                       |/Rect [{ ls_goto-rect }] /Border [0 0 0] | &&
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
    lv_catalog_extra = lv_catalog_extra && write_attachments( lo_writer ).

    " PDF/A: the metadata, the colours of the device as sRGB, the time
    DATA lv_date     TYPE string.
    DATA lv_pdf_date TYPE string.
    DATA lv_file_id  TYPE string.
    IF mv_pdfa = abap_true.
      DATA(lv_stamp) = |{ zabaputil_cl_util_context=>time_get_timestamp( ) }|.
      lv_date = |{ lv_stamp(4) }-{ lv_stamp+4(2) }-{ lv_stamp+6(2) }T{ lv_stamp+8(2) }:{ lv_stamp+10(2) }:{ lv_stamp+12(2) }+00:00|.
      lv_pdf_date = |D:{ lv_stamp(14) }+00'00'|.
      DATA(lv_metadata) = lo_writer->add_stream(
          dict     = `/Type /Metadata /Subtype /XML`
          content  = zabaputil_cl_util_context=>conv_get_xstring_by_string( xmp( lv_date ) )
          compress = abap_false ).
      DATA(lv_profile) = lo_writer->add_stream( dict    = `/N 3`
                                                content = srgb_profile( ) ).
      DATA(lv_intent) = lo_writer->add_object(
          |<< /Type /OutputIntent /S /GTS_PDFA1 /OutputConditionIdentifier (sRGB IEC61966-2.1) | &&
          |/Info (sRGB IEC61966-2.1) /DestOutputProfile { lv_profile } 0 R >>| ).
      lv_catalog_extra = |{ lv_catalog_extra } /Metadata { lv_metadata } 0 R /OutputIntents [{ lv_intent } 0 R]|.
      lv_file_id = substring( val = to_upper( zabaputil_cl_util_context=>hash_calculate(
                                                  |{ lv_stamp }{ lines( mt_page ) }{ lines( mt_font ) }{ lv_kids }| ) )
                              len = 32 ).
    ENDIF.
    DATA(lv_catalog) = lo_writer->add_object( |<< /Type /Catalog /Pages { lv_pages } 0 R{ lv_catalog_extra } >>| ).

    " text strings in UTF-16: a title is not limited to one code page
    LOOP AT mt_info ASSIGNING FIELD-SYMBOL(<info>).
      lv_info = lv_info && | /{ <info>-key } <{ zabaputil_cl_pdf_writer=>hex_utf16( <info>-text ) }>|.
    ENDLOOP.
    IF lv_pdf_date IS NOT INITIAL.
      lv_info = |{ lv_info } /CreationDate ({ lv_pdf_date }) /ModDate ({ lv_pdf_date })|.
    ENDIF.
    lv_info_id = lo_writer->add_object( |<<{ lv_info } /Producer (abap-util) >>| ).

    IF mv_pdfa = abap_true.
      result = lo_writer->render( root    = lv_catalog
                                  info    = lv_info_id
                                  version = `1.7`
                                  id      = lv_file_id ).
    ELSE.
      result = lo_writer->render( root = lv_catalog
                                  info = lv_info_id ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

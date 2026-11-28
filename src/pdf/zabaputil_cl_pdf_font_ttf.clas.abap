CLASS zabaputil_cl_pdf_font_ttf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " A TrueType font file, embedded into the PDF: any script the file has
    " glyphs for, no code page in between. The text is written as glyph
    " numbers (a composite font, Identity-H), and a ToUnicode map lets a
    " viewer copy and search it.
    "
    " Only the glyphs the text uses are embedded, and the parts of the
    " composite ones among them (subset = abap_false embeds the whole
    " file). The glyph numbers stay what they are: the glyphs that are not
    " used are empty, and tables a PDF viewer does not read (GSUB, GPOS,
    " kern, ...) are left out.
    "
    " An OpenType font with PostScript outlines (CFF, the usual .otf) is
    " embedded whole, as an OpenType font program (FontFile3) - it is not
    " subset. Of a font collection (.ttc) one font is embedded, the first
    " or the one of index, as a TrueType file of its own. Not supported:
    " everything a shaping engine does - ligatures, kerning, Arabic
    " joining, right-to-left.

    INTERFACES zabaputil_if_pdf_font.

    "! @parameter content  | the bytes of the .ttf file
    "! @parameter resource | the name in the resource dictionary, `F1`
    "! @parameter subset   | embed the glyphs in use only (TrueType outlines)
    "! @parameter index    | of a collection (.ttc): the font, from 0
    METHODS constructor
      IMPORTING
        content  TYPE xstring
        resource TYPE clike
        subset   TYPE abap_bool DEFAULT abap_true
        index    TYPE i DEFAULT 0.

    "! The font file as it is embedded: the glyphs used so far, or the
    "! whole file without subsetting
    METHODS file
      RETURNING
        VALUE(result) TYPE xstring.

    "! The glyph number of a Unicode code point, 0 (the missing glyph)
    "! for a character the font does not have
    METHODS glyph
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    "! The code points of the characters the font has no glyph for, that
    "! were written as its missing glyph so far
    METHODS missing
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_t_codepoint.

    "! The PostScript name of the font, as its name table gives it
    METHODS name
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_table,
        tag    TYPE string,
        offset TYPE i,
        length TYPE i,
      END OF ty_s_table.
    TYPES:
      BEGIN OF ty_s_glyph,
        codepoint TYPE i,
        glyph     TYPE i,
        width     TYPE i,
      END OF ty_s_glyph.

    DATA mv_content       TYPE xstring.
    DATA mv_subset        TYPE abap_bool.
    " where the font starts in the file: 0, or its offset in a collection
    DATA mv_base          TYPE i.
    DATA mv_index         TYPE i.
    DATA mv_collection    TYPE abap_bool.
    " PostScript outlines (CFF) instead of TrueType ones
    DATA mv_cff           TYPE abap_bool.
    DATA mv_resource      TYPE string.
    DATA mv_name          TYPE string.
    DATA mt_table         TYPE HASHED TABLE OF ty_s_table WITH UNIQUE KEY tag.
    DATA mv_units_per_em  TYPE i.
    DATA mv_ascent        TYPE i.
    DATA mv_descent       TYPE i.
    DATA mv_cap_height    TYPE i.
    DATA mv_bbox          TYPE string.
    DATA mv_italic_angle  TYPE i.
    DATA mv_fixed_pitch   TYPE abap_bool.
    DATA mv_num_glyphs    TYPE i.
    DATA mv_num_hmetrics  TYPE i.
    DATA mv_hmtx          TYPE i.
    " the character map in use: its offset in the file and its format (4 or 12)
    DATA mv_cmap          TYPE i.
    DATA mv_cmap_format   TYPE i.
    " every character asked for so far, with its glyph and the glyph's
    " width in thousandths of the font size
    DATA mt_glyph         TYPE HASHED TABLE OF ty_s_glyph WITH UNIQUE KEY codepoint.

    METHODS u8
      IMPORTING
        off           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS u16
      IMPORTING
        off           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS s16
      IMPORTING
        off           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS u32
      IMPORTING
        off           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    "! The offset of a table in the file; raises when the font has none
    METHODS table_offset
      IMPORTING
        tag           TYPE string
      RETURNING
        VALUE(result) TYPE i.

    "! A length in font units as thousandths of the font size
    METHODS scale
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS read_directory.
    METHODS read_metrics.
    METHODS read_name.
    METHODS read_cmap.

    METHODS glyph_format_4
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS glyph_format_12
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    "! Glyph and width of a character, looked up once and kept
    METHODS glyph_info
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE ty_s_glyph.

    "! A TrueType file of the glyphs in use only - or of all glyphs, for a
    "! font of a collection
    METHODS subset
      IMPORTING
        all           TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE xstring.

    "! Where a glyph is in the glyf table, and how long it is
    METHODS glyph_range
      IMPORTING
        glyph  TYPE i
      EXPORTING
        offset TYPE i
        length TYPE i.

    "! An unsigned 32 bit number as 8 hex digits
    CLASS-METHODS hex32
      IMPORTING
        val           TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    "! The checksum of a table: its 32 bit words added up, modulo 2^32
    CLASS-METHODS checksum
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE decfloat34.

    "! The bytes of a table, as a string of hex digits
    METHODS table_hex
      IMPORTING
        tag           TYPE string
      RETURNING
        VALUE(result) TYPE string.

    "! The map from glyph numbers back to Unicode, for copy and search
    METHODS to_unicode
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_font_ttf IMPLEMENTATION.

  METHOD constructor.

    mv_content = content.
    mv_resource = resource.
    mv_subset = subset.
    mv_index = index.

    read_directory( ).
    read_metrics( ).
    read_name( ).
    read_cmap( ).

  ENDMETHOD.

  METHOD u8.

    IF off < 0 OR off >= xstrlen( mv_content ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the font file is damaged (a table points behind its end)`.
    ENDIF.
    result = mv_content+off(1).

  ENDMETHOD.

  METHOD u16.

    result = u8( off ) * 256 + u8( off + 1 ).

  ENDMETHOD.

  METHOD s16.

    result = u16( off ).
    IF result >= 32768.
      result = result - 65536.
    ENDIF.

  ENDMETHOD.

  METHOD u32.

    " offsets and lengths: none of them reaches 2^31 in a file that fits
    " into memory, and a larger value would not fit into an integer
    DATA(lv_high) = u16( off ).
    IF lv_high >= 32768.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the font file is damaged (a length is out of range)`.
    ENDIF.
    result = lv_high * 65536 + u16( off + 2 ).

  ENDMETHOD.

  METHOD table_offset.

    ASSIGN mt_table[ tag = tag ] TO FIELD-SYMBOL(<table>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: the font file has no table { tag }|.
    ENDIF.
    result = <table>-offset.

  ENDMETHOD.

  METHOD scale.

    result = val * 1000 / mv_units_per_em.

  ENDMETHOD.

  METHOD read_directory.

    DATA lv_off     TYPE i.
    DATA lv_tag     TYPE xstring.
    DATA ls_table   TYPE ty_s_table.
    " the first four bytes in hex
    DATA lv_version TYPE string.

    IF xstrlen( mv_content ) < 12.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: not a TrueType font file`.
    ENDIF.

    " `ttcf` is a collection of several fonts: the offset of each of them
    " follows the number of fonts
    lv_version = mv_content(4).
    IF lv_version = `74746366`.
      mv_collection = abap_true.
      IF mv_index < 0 OR mv_index >= u32( 8 ).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: the font collection has no font { mv_index } - it has { u32( 8 ) }|.
      ENDIF.
      mv_base = u32( 12 + mv_index * 4 ).
      IF mv_base + 12 > xstrlen( mv_content ).
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: the font file is damaged (a font of the collection is behind its end)`.
      ENDIF.
      lv_version = mv_content+mv_base(4).
    ENDIF.

    " 00010000 or `true`: TrueType outlines; `OTTO`: PostScript outlines
    IF lv_version = `4F54544F`.
      mv_cff = abap_true.
      IF mv_collection = abap_true.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: a collection of fonts with PostScript outlines (.otc) cannot be embedded`.
      ENDIF.
    ELSEIF lv_version <> `00010000` AND lv_version <> `74727565`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: not a TrueType or OpenType font file (.ttf, .otf, .ttc)`.
    ENDIF.

    " the table offsets are from the start of the file, in a collection too
    DATA(lv_count) = u16( mv_base + 4 ).
    lv_off = mv_base + 12.
    DO lv_count TIMES.
      IF lv_off + 16 > xstrlen( mv_content ).
        EXIT.
      ENDIF.
      lv_tag = mv_content+lv_off(4).
      ls_table-tag = zabaputil_cl_util_context=>conv_get_string_by_xstring( lv_tag ).
      ls_table-offset = u32( lv_off + 8 ).
      ls_table-length = u32( lv_off + 12 ).
      INSERT ls_table INTO TABLE mt_table.
      lv_off = lv_off + 16.
    ENDDO.

  ENDMETHOD.

  METHOD read_metrics.

    DATA lv_off TYPE i.

    lv_off = table_offset( `head` ).
    mv_units_per_em = u16( lv_off + 18 ).
    IF mv_units_per_em = 0.
      mv_units_per_em = 1000.
    ENDIF.
    mv_bbox = |{ scale( s16( lv_off + 36 ) ) } { scale( s16( lv_off + 38 ) ) } | &&
              |{ scale( s16( lv_off + 40 ) ) } { scale( s16( lv_off + 42 ) ) }|.

    lv_off = table_offset( `hhea` ).
    mv_ascent = scale( s16( lv_off + 4 ) ).
    mv_descent = scale( s16( lv_off + 6 ) ) * -1.
    mv_num_hmetrics = u16( lv_off + 34 ).

    mv_num_glyphs = u16( table_offset( `maxp` ) + 4 ).
    mv_hmtx = table_offset( `hmtx` ).

    " the height of a capital letter: OS/2 has it from version 2 on
    mv_cap_height = mv_ascent.
    ASSIGN mt_table[ tag = `OS/2` ] TO FIELD-SYMBOL(<os2>).
    IF sy-subrc = 0 AND <os2>-length >= 90.
      IF u16( <os2>-offset ) >= 2.
        mv_cap_height = scale( s16( <os2>-offset + 88 ) ).
      ENDIF.
    ENDIF.

    " italicAngle is a 16.16 fixed point number; its integer part will do
    ASSIGN mt_table[ tag = `post` ] TO FIELD-SYMBOL(<post>).
    IF sy-subrc = 0 AND <post>-length >= 16.
      mv_italic_angle = s16( <post>-offset + 4 ).
      mv_fixed_pitch = xsdbool( u32( <post>-offset + 12 ) <> 0 ).
    ENDIF.

  ENDMETHOD.

  METHOD read_name.

    DATA lv_rec      TYPE i.
    DATA lv_platform TYPE i.
    DATA lv_len      TYPE i.
    DATA lv_pos      TYPE i.
    DATA lv_step     TYPE i.
    DATA lv_byte     TYPE i.
    DATA lv_x        TYPE x LENGTH 1.
    DATA lv_hex      TYPE string.

    mv_name = `EmbeddedFont`.

    ASSIGN mt_table[ tag = `name` ] TO FIELD-SYMBOL(<name>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_count) = u16( <name>-offset + 2 ).
    DATA(lv_strings) = <name>-offset + u16( <name>-offset + 4 ).

    " name id 6 is the PostScript name. Platform 3 (Windows) stores it in
    " UTF-16BE, platform 1 (Macintosh) in one byte per character; the name
    " is ASCII either way, so the low byte of every character is all of it
    lv_rec = <name>-offset + 6.
    DO lv_count TIMES.
      IF u16( lv_rec + 6 ) = 6.
        lv_platform = u16( lv_rec ).
        lv_len = u16( lv_rec + 8 ).
        lv_pos = lv_strings + u16( lv_rec + 10 ).
        IF lv_platform = 1.
          lv_step = 1.
        ELSE.
          lv_step = 2.
          lv_pos = lv_pos + 1.
        ENDIF.
        CLEAR lv_hex.
        DO lv_len DIV lv_step TIMES.
          lv_byte = u8( lv_pos ).
          lv_pos = lv_pos + lv_step.
          " letters, digits and the hyphen - what a PDF name takes unescaped
          IF ( lv_byte >= 48 AND lv_byte <= 57 ) OR ( lv_byte >= 65 AND lv_byte <= 90 )
              OR ( lv_byte >= 97 AND lv_byte <= 122 ) OR lv_byte = 45.
            lv_x = lv_byte.
            lv_hex = lv_hex && CONV string( lv_x ).
          ENDIF.
        ENDDO.
        IF lv_hex IS NOT INITIAL.
          mv_name = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( lv_hex ) ).
          RETURN.
        ENDIF.
      ENDIF.
      lv_rec = lv_rec + 12.
    ENDDO.

  ENDMETHOD.

  METHOD read_cmap.

    DATA lv_rec      TYPE i.
    DATA lv_platform TYPE i.
    DATA lv_encoding TYPE i.
    DATA lv_sub      TYPE i.
    DATA lv_format   TYPE i.
    DATA lv_rank     TYPE i.
    DATA lv_best     TYPE i.

    DATA(lv_cmap) = table_offset( `cmap` ).
    DATA(lv_count) = u16( lv_cmap + 2 ).

    " the Unicode subtables, the full repertoire (format 12) before the
    " Basic Multilingual Plane (format 4)
    lv_rec = lv_cmap + 4.
    DO lv_count TIMES.
      lv_platform = u16( lv_rec ).
      lv_encoding = u16( lv_rec + 2 ).
      lv_sub = lv_cmap + u32( lv_rec + 4 ).
      lv_format = u16( lv_sub ).
      lv_rank = 0.
      IF lv_platform = 3 AND lv_encoding = 10 AND lv_format = 12.
        lv_rank = 4.
      ELSEIF lv_platform = 0 AND lv_format = 12.
        lv_rank = 3.
      ELSEIF lv_platform = 3 AND lv_encoding = 1 AND lv_format = 4.
        lv_rank = 2.
      ELSEIF lv_platform = 0 AND lv_format = 4.
        lv_rank = 1.
      ENDIF.
      IF lv_rank > lv_best.
        lv_best = lv_rank.
        mv_cmap = lv_sub.
        mv_cmap_format = lv_format.
      ENDIF.
      lv_rec = lv_rec + 8.
    ENDDO.

    IF lv_best = 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the font file has no Unicode character map`.
    ENDIF.

  ENDMETHOD.

  METHOD glyph.

    DATA(ls_glyph) = glyph_info( val ).
    result = ls_glyph-glyph.

  ENDMETHOD.

  METHOD name.

    result = mv_name.

  ENDMETHOD.

  METHOD glyph_info.

    ASSIGN mt_glyph[ codepoint = val ] TO FIELD-SYMBOL(<glyph>).
    IF sy-subrc = 0.
      result = <glyph>.
      RETURN.
    ENDIF.

    result-codepoint = val.
    IF mv_cmap_format = 12.
      result-glyph = glyph_format_12( val ).
    ELSE.
      result-glyph = glyph_format_4( val ).
    ENDIF.
    IF result-glyph >= mv_num_glyphs.
      result-glyph = 0.
    ENDIF.

    " hmtx has one advance width per glyph up to numberOfHMetrics; the
    " glyphs after that share the last one
    IF mv_num_hmetrics > 0.
      IF result-glyph < mv_num_hmetrics.
        result-width = scale( u16( mv_hmtx + result-glyph * 4 ) ).
      ELSE.
        result-width = scale( u16( mv_hmtx + ( mv_num_hmetrics - 1 ) * 4 ) ).
      ENDIF.
    ENDIF.

    INSERT result INTO TABLE mt_glyph.

  ENDMETHOD.

  METHOD glyph_format_4.

    DATA lv_low   TYPE i.
    DATA lv_high  TYPE i.
    DATA lv_mid   TYPE i.
    DATA lv_start TYPE i.
    DATA lv_delta TYPE i.
    DATA lv_range TYPE i.
    DATA lv_pos   TYPE i.

    IF val > 65535.
      RETURN.
    ENDIF.

    " four parallel arrays of segCount entries: endCode, (a pad), startCode,
    " idDelta, idRangeOffset. Binary search for the first segment that ends
    " at or after the character
    DATA(lv_seg_x2) = u16( mv_cmap + 6 ).
    DATA(lv_end_codes) = mv_cmap + 14.
    DATA(lv_start_codes) = lv_end_codes + lv_seg_x2 + 2.
    DATA(lv_deltas) = lv_start_codes + lv_seg_x2.
    DATA(lv_ranges) = lv_deltas + lv_seg_x2.

    lv_low = 0.
    lv_high = lv_seg_x2 DIV 2 - 1.
    WHILE lv_low < lv_high.
      lv_mid = ( lv_low + lv_high ) DIV 2.
      IF u16( lv_end_codes + lv_mid * 2 ) < val.
        lv_low = lv_mid + 1.
      ELSE.
        lv_high = lv_mid.
      ENDIF.
    ENDWHILE.

    lv_start = u16( lv_start_codes + lv_low * 2 ).
    IF lv_start > val OR u16( lv_end_codes + lv_low * 2 ) < val.
      RETURN.
    ENDIF.

    lv_delta = u16( lv_deltas + lv_low * 2 ).
    lv_pos = lv_ranges + lv_low * 2.
    lv_range = u16( lv_pos ).
    IF lv_range = 0.
      result = ( val + lv_delta ) MOD 65536.
    ELSE.
      " the offset counts from the idRangeOffset entry itself into the
      " glyph id array behind the four arrays
      result = u16( lv_pos + lv_range + ( val - lv_start ) * 2 ).
      IF result <> 0.
        result = ( result + lv_delta ) MOD 65536.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD glyph_format_12.

    DATA lv_low   TYPE i.
    DATA lv_high  TYPE i.
    DATA lv_mid   TYPE i.
    DATA lv_group TYPE i.

    " groups of twelve bytes: first character, last character, glyph of
    " the first one
    DATA(lv_count) = u32( mv_cmap + 12 ).
    IF lv_count = 0.
      RETURN.
    ENDIF.

    lv_low = 0.
    lv_high = lv_count - 1.
    WHILE lv_low < lv_high.
      lv_mid = ( lv_low + lv_high ) DIV 2.
      IF u32( mv_cmap + 16 + lv_mid * 12 + 4 ) < val.
        lv_low = lv_mid + 1.
      ELSE.
        lv_high = lv_mid.
      ENDIF.
    ENDWHILE.

    lv_group = mv_cmap + 16 + lv_low * 12.
    IF u32( lv_group ) <= val AND u32( lv_group + 4 ) >= val.
      result = u32( lv_group + 8 ) + val - u32( lv_group ).
    ENDIF.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~resource.

    result = mv_resource.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~encode.

    DATA lv_cp    TYPE i.
    DATA ls_glyph TYPE ty_s_glyph.

    LOOP AT zabaputil_cl_util_context=>c_get_codepoints( val ) INTO lv_cp.
      ls_glyph = glyph_info( lv_cp ).
      result = result && zabaputil_cl_pdf_writer=>hex_word( ls_glyph-glyph ).
    ENDLOOP.
    result = |<{ result }>|.

  ENDMETHOD.

  METHOD zabaputil_if_pdf_font~width.

    DATA lv_cp    TYPE i.
    DATA lv_sum   TYPE i.
    DATA lv_size  TYPE f.
    DATA ls_glyph TYPE ty_s_glyph.

    LOOP AT zabaputil_cl_util_context=>c_get_codepoints( val ) INTO lv_cp.
      ls_glyph = glyph_info( lv_cp ).
      lv_sum = lv_sum + ls_glyph-width.
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

    DATA lv_widths TYPE string.
    DATA lv_flags  TYPE i VALUE 4.
    DATA lt_sorted TYPE SORTED TABLE OF ty_s_glyph WITH NON-UNIQUE KEY glyph.

    " the widths of the glyphs in use; two characters may share a glyph,
    " and a glyph is listed once
    lt_sorted = mt_glyph.
    DATA(lv_last) = -1.
    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<glyph>).
      IF <glyph>-glyph = lv_last.
        CONTINUE.
      ENDIF.
      lv_last = <glyph>-glyph.
      lv_widths = lv_widths && | { <glyph>-glyph } [{ <glyph>-width }]|.
    ENDLOOP.

    " flags: 4 symbolic (the glyphs are addressed by number, not through a
    " standard encoding), 1 fixed pitch, 64 italic
    IF mv_fixed_pitch = abap_true.
      lv_flags = lv_flags + 1.
    ENDIF.
    IF mv_italic_angle <> 0.
      lv_flags = lv_flags + 64.
    ENDIF.

    " a subset is named after its glyphs: six capitals and a plus sign
    DATA(lv_name) = mv_name.
    IF mv_subset = abap_true AND mv_cff = abap_false.
      DATA lv_hash TYPE i.
      DATA lv_tag TYPE string.
      LOOP AT lt_sorted ASSIGNING <glyph>.
        lv_hash = ( lv_hash * 31 + <glyph>-glyph ) MOD 308915776.
      ENDLOOP.
      DO 6 TIMES.
        lv_tag = lv_tag && substring( val = sy-abcde
                                      off = lv_hash MOD 26
                                      len = 1 ).
        lv_hash = lv_hash DIV 26.
      ENDDO.
      lv_name = |{ lv_tag }+{ mv_name }|.
    ENDIF.

    " TrueType outlines: FontFile2 and a CIDFontType2 with CID = glyph.
    " PostScript outlines: the OpenType file as FontFile3 and a
    " CIDFontType0 - a CFF that is not CID-keyed takes the CIDs as glyphs
    DATA(lv_content) = file( ).
    DATA lv_font_file TYPE string.
    DATA lv_subtype   TYPE string.
    DATA lv_gid_map   TYPE string.
    IF mv_cff = abap_true.
      DATA(lv_file) = writer->add_stream( dict    = `/Subtype /OpenType`
                                          content = lv_content ).
      lv_font_file = |/FontFile3 { lv_file } 0 R|.
      lv_subtype = `CIDFontType0`.
    ELSE.
      lv_file = writer->add_stream( dict    = |/Length1 { xstrlen( lv_content ) }|
                                    content = lv_content ).
      lv_font_file = |/FontFile2 { lv_file } 0 R|.
      lv_subtype = `CIDFontType2`.
      lv_gid_map = ` /CIDToGIDMap /Identity`.
    ENDIF.

    DATA(lv_descriptor) = writer->add_object(
        |<< /Type /FontDescriptor /FontName /{ lv_name } /Flags { lv_flags } /FontBBox [{ mv_bbox }] | &&
        |/ItalicAngle { mv_italic_angle } /Ascent { mv_ascent } /Descent { mv_descent * -1 } | &&
        |/CapHeight { mv_cap_height } /StemV 80 { lv_font_file } >>| ).

    DATA(lv_cid_font) = writer->add_object(
        |<< /Type /Font /Subtype /{ lv_subtype } /BaseFont /{ lv_name } | &&
        |/CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> | &&
        |/FontDescriptor { lv_descriptor } 0 R /DW 1000 /W [{ lv_widths } ]{ lv_gid_map } >>| ).

    DATA(lv_to_unicode) = writer->add_stream(
        zabaputil_cl_util_context=>conv_get_xstring_by_string( to_unicode( ) ) ).

    result = writer->add_object(
        |<< /Type /Font /Subtype /Type0 /BaseFont /{ lv_name } /Encoding /Identity-H | &&
        |/DescendantFonts [{ lv_cid_font } 0 R] /ToUnicode { lv_to_unicode } 0 R >>| ).

  ENDMETHOD.

  METHOD missing.

    LOOP AT mt_glyph ASSIGNING FIELD-SYMBOL(<glyph>) WHERE glyph = 0.
      INSERT <glyph>-codepoint INTO TABLE result.
    ENDLOOP.
    SORT result.

  ENDMETHOD.

  METHOD file.

    IF mv_cff = abap_true.
      result = mv_content.
    ELSEIF mv_subset = abap_true.
      result = subset( ).
    ELSEIF mv_collection = abap_true.
      result = subset( abap_true ).
    ELSE.
      result = mv_content.
    ENDIF.

  ENDMETHOD.

  METHOD glyph_range.

    DATA lv_next TYPE i.

    DATA(lv_loca) = table_offset( `loca` ).
    " indexToLocFormat: 0 for offsets / 2 in 16 bits, 1 for 32 bits
    IF s16( table_offset( `head` ) + 50 ) = 0.
      offset = u16( lv_loca + glyph * 2 ) * 2.
      lv_next = u16( lv_loca + glyph * 2 + 2 ) * 2.
    ELSE.
      offset = u32( lv_loca + glyph * 4 ).
      lv_next = u32( lv_loca + glyph * 4 + 4 ).
    ENDIF.
    length = lv_next - offset.
    IF length < 0.
      length = 0.
    ENDIF.

  ENDMETHOD.

  METHOD hex32.

    DATA lv_value TYPE decfloat34.
    DATA lv_x     TYPE x LENGTH 2.
    DATA lv_high  TYPE i.
    DATA lv_low   TYPE i.

    lv_value = val.
    lv_high = trunc( lv_value / 65536 ).
    lv_low = lv_value - lv_high * 65536.
    lv_x = lv_high.
    result = lv_x.
    lv_x = lv_low.
    result = result && lv_x.

  ENDMETHOD.

  METHOD checksum.

    DATA lv_word TYPE x LENGTH 2.
    DATA lv_off  TYPE i.
    DATA lv_high TYPE i.
    DATA lv_low  TYPE i.
    DATA lv_hi   TYPE decfloat34.
    DATA lv_lo   TYPE decfloat34.

    " the high and the low 16 bits of every word, added up apart; a
    " table that does not end on a word counts as padded with zeros
    DATA(lv_length) = xstrlen( val ).
    WHILE lv_off < lv_length.
      lv_high = 0.
      lv_low = 0.
      IF lv_off + 2 <= lv_length.
        lv_word = val+lv_off(2).
        lv_high = lv_word.
      ELSE.
        lv_word = val+lv_off(1).
        lv_high = lv_word.
        lv_high = lv_high * 256.
      ENDIF.
      lv_off = lv_off + 2.
      IF lv_off + 2 <= lv_length.
        lv_word = val+lv_off(2).
        lv_low = lv_word.
      ELSEIF lv_off < lv_length.
        lv_word = val+lv_off(1).
        lv_low = lv_word.
        lv_low = lv_low * 256.
      ENDIF.
      lv_off = lv_off + 2.
      lv_hi = lv_hi + lv_high.
      lv_lo = lv_lo + lv_low.
    ENDWHILE.
    result = ( lv_hi * 65536 + lv_lo ) MOD 4294967296.

  ENDMETHOD.

  METHOD table_hex.

    ASSIGN mt_table[ tag = tag ] TO FIELD-SYMBOL(<table>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF <table>-offset + <table>-length > xstrlen( mv_content ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: the font file is damaged (table { tag } reaches behind its end)|.
    ENDIF.
    DATA(lv_bytes) = mv_content+<table>-offset(<table>-length).
    result = lv_bytes.

  ENDMETHOD.

  METHOD subset.

    TYPES:
      BEGIN OF ty_s_out,
        tag TYPE string,
        hex TYPE string,
      END OF ty_s_out.

    DATA lt_used   TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
    DATA lt_queue  TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lv_glyph  TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.
    DATA lv_off    TYPE i.
    DATA lv_flags  TYPE i.
    DATA lt_glyf   TYPE string_table.
    DATA lt_loca   TYPE string_table.
    DATA lv_pos    TYPE i.
    DATA lv_hex    TYPE string.
    DATA lt_out    TYPE STANDARD TABLE OF ty_s_out WITH EMPTY KEY.
    DATA ls_out    TYPE ty_s_out.
    DATA lv_bytes  TYPE xstring.
    DATA lt_file   TYPE string_table.
    DATA lv_sum    TYPE decfloat34.
    DATA lv_power  TYPE i VALUE 1.
    DATA lv_log    TYPE i.
    DATA lv_x2     TYPE x LENGTH 2.
    DATA lv_head   TYPE i.

    " the flags of a component of a composite glyph
    CONSTANTS lc_words    TYPE i VALUE 1.
    CONSTANTS lc_scale    TYPE i VALUE 8.
    CONSTANTS lc_more     TYPE i VALUE 32.
    CONSTANTS lc_xy_scale TYPE i VALUE 64.
    CONSTANTS lc_two      TYPE i VALUE 128.

    DATA(lv_glyf) = table_offset( `glyf` ).

    " the missing glyph, the glyphs in use and the parts of the composite ones
    INSERT 0 INTO TABLE lt_queue.
    LOOP AT mt_glyph ASSIGNING FIELD-SYMBOL(<glyph>).
      INSERT <glyph>-glyph INTO TABLE lt_queue.
    ENDLOOP.
    IF all = abap_true.
      lv_glyph = 0.
      WHILE lv_glyph < mv_num_glyphs.
        INSERT lv_glyph INTO TABLE lt_used.
        lv_glyph = lv_glyph + 1.
      ENDWHILE.
      CLEAR lt_queue.
    ENDIF.
    WHILE lt_queue IS NOT INITIAL.
      lv_glyph = lt_queue[ 1 ].
      DELETE lt_queue INDEX 1.
      IF lv_glyph >= mv_num_glyphs OR line_exists( lt_used[ table_line = lv_glyph ] ).
        CONTINUE.
      ENDIF.
      INSERT lv_glyph INTO TABLE lt_used.
      glyph_range( EXPORTING glyph  = lv_glyph
                   IMPORTING offset = lv_offset
                             length = lv_length ).
      " fewer than zero contours: a composite glyph
      IF lv_length < 10.
        CONTINUE.
      ENDIF.
      IF s16( lv_glyf + lv_offset ) >= 0.
        CONTINUE.
      ENDIF.
      lv_off = lv_glyf + lv_offset + 10.
      DO.
        lv_flags = u16( lv_off ).
        INSERT u16( lv_off + 2 ) INTO TABLE lt_queue.
        lv_off = lv_off + 4.
        IF lv_flags MOD ( lc_words * 2 ) >= lc_words.
          lv_off = lv_off + 4.
        ELSE.
          lv_off = lv_off + 2.
        ENDIF.
        IF lv_flags MOD ( lc_scale * 2 ) >= lc_scale.
          lv_off = lv_off + 2.
        ELSEIF lv_flags MOD ( lc_xy_scale * 2 ) >= lc_xy_scale.
          lv_off = lv_off + 4.
        ELSEIF lv_flags MOD ( lc_two * 2 ) >= lc_two.
          lv_off = lv_off + 8.
        ENDIF.
        IF lv_flags MOD ( lc_more * 2 ) < lc_more.
          EXIT.
        ENDIF.
      ENDDO.
    ENDWHILE.

    " glyf: the glyphs in use, each on a four byte boundary; the others
    " are empty. loca: the long offsets into it
    lv_glyph = 0.
    WHILE lv_glyph < mv_num_glyphs.
      INSERT hex32( lv_pos ) INTO TABLE lt_loca.
      IF line_exists( lt_used[ table_line = lv_glyph ] ).
        glyph_range( EXPORTING glyph  = lv_glyph
                     IMPORTING offset = lv_offset
                               length = lv_length ).
        IF lv_length > 0.
          lv_off = lv_glyf + lv_offset.
          lv_bytes = mv_content+lv_off(lv_length).
          lv_hex = lv_bytes.
          WHILE lv_length MOD 4 <> 0.
            lv_hex = lv_hex && `00`.
            lv_length = lv_length + 1.
          ENDWHILE.
          INSERT lv_hex INTO TABLE lt_glyf.
          lv_pos = lv_pos + lv_length.
        ENDIF.
      ENDIF.
      lv_glyph = lv_glyph + 1.
    ENDWHILE.
    INSERT hex32( lv_pos ) INTO TABLE lt_loca.

    " the tables a viewer reads
    LOOP AT mt_table ASSIGNING FIELD-SYMBOL(<table>).
      CLEAR ls_out.
      ls_out-tag = <table>-tag.
      CASE <table>-tag.
        WHEN `head`.
          " checkSumAdjustment 0 for now, indexToLocFormat long
          lv_hex = table_hex( <table>-tag ).
          ls_out-hex = substring( val = lv_hex
                                  len = 16 ) && `00000000` &&
                       substring( val = lv_hex
                                  off = 24
                                  len = 76 ) && `0001` &&
                       substring( val = lv_hex
                                  off = 104 ).
        WHEN `glyf`.
          ls_out-hex = concat_lines_of( lt_glyf ).
        WHEN `loca`.
          ls_out-hex = concat_lines_of( lt_loca ).
        WHEN `hhea` OR `maxp` OR `hmtx` OR `cvt ` OR `fpgm` OR `prep` OR `OS/2` OR `name` OR `post` OR `cmap`.
          ls_out-hex = table_hex( <table>-tag ).
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
      INSERT ls_out INTO TABLE lt_out.
    ENDLOOP.
    " in the order of the bytes of their tags: capitals first
    SORT lt_out BY tag.

    " the offset table: version, number of tables and the search values
    DATA(lv_count) = lines( lt_out ).
    WHILE lv_power * 2 <= lv_count.
      lv_power = lv_power * 2.
      lv_log = lv_log + 1.
    ENDWHILE.
    lv_hex = `00010000`.
    lv_x2 = lv_count.
    lv_hex = lv_hex && lv_x2.
    lv_x2 = lv_power * 16.
    lv_hex = lv_hex && lv_x2.
    lv_x2 = lv_log.
    lv_hex = lv_hex && lv_x2.
    lv_x2 = lv_count * 16 - lv_power * 16.
    lv_hex = lv_hex && lv_x2.
    INSERT lv_hex INTO TABLE lt_file.

    " the directory, then the tables on four byte boundaries
    lv_pos = 12 + lv_count * 16.
    LOOP AT lt_out INTO ls_out.
      lv_length = strlen( ls_out-hex ) / 2.
      lv_bytes = ls_out-hex.
      IF ls_out-tag = `head`.
        lv_head = lv_pos.
      ENDIF.
      lv_hex = zabaputil_cl_util_context=>conv_get_xstring_by_string( ls_out-tag ).
      INSERT lv_hex && hex32( checksum( lv_bytes ) ) && hex32( lv_pos ) && hex32( lv_length ) INTO TABLE lt_file.
      WHILE lv_length MOD 4 <> 0.
        lv_length = lv_length + 1.
      ENDWHILE.
      lv_pos = lv_pos + lv_length.
    ENDLOOP.
    LOOP AT lt_out INTO ls_out.
      lv_hex = ls_out-hex.
      WHILE strlen( lv_hex ) MOD 8 <> 0.
        lv_hex = lv_hex && `00`.
      ENDWHILE.
      INSERT lv_hex INTO TABLE lt_file.
    ENDLOOP.
    lv_hex = concat_lines_of( lt_file ).
    lv_bytes = lv_hex.

    " checkSumAdjustment of head: B1B0AFBA minus the checksum of the file
    lv_sum = ( 2981146554 - checksum( lv_bytes ) ) MOD 4294967296.
    IF lv_sum < 0.
      lv_sum = lv_sum + 4294967296.
    ENDIF.
    lv_off = ( lv_head + 8 ) * 2.
    lv_hex = substring( val = lv_hex
                        len = lv_off ) && hex32( lv_sum ) && substring( val = lv_hex
                                                                        off = lv_off + 8 ).
    result = lv_hex.

  ENDMETHOD.

  METHOD to_unicode.

    DATA lt_sorted TYPE SORTED TABLE OF ty_s_glyph WITH NON-UNIQUE KEY glyph.
    DATA lt_line   TYPE string_table.
    DATA lv_cp     TYPE i.
    DATA lv_text   TYPE string.
    DATA lv_from   TYPE i.
    DATA lv_to     TYPE i.
    DATA lv_index  TYPE i.

    lt_sorted = mt_glyph.
    DATA(lv_last) = -1.
    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<glyph>).
      IF <glyph>-glyph = lv_last OR <glyph>-glyph = 0.
        CONTINUE.
      ENDIF.
      lv_last = <glyph>-glyph.
      lv_cp = <glyph>-codepoint.
      IF lv_cp < 65536.
        lv_text = zabaputil_cl_pdf_writer=>hex_word( lv_cp ).
      ELSE.
        lv_cp = lv_cp - 65536.
        lv_text = zabaputil_cl_pdf_writer=>hex_word( 55296 + lv_cp DIV 1024 ) &&
                  zabaputil_cl_pdf_writer=>hex_word( 56320 + lv_cp MOD 1024 ).
      ENDIF.
      INSERT |<{ zabaputil_cl_pdf_writer=>hex_word( <glyph>-glyph ) }> <{ lv_text }>| INTO TABLE lt_line.
    ENDLOOP.

    result = |/CIDInit /ProcSet findresource begin\n12 dict begin\nbegincmap\n| &&
             |/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def\n| &&
             |/CMapName /Adobe-Identity-UCS def\n/CMapType 2 def\n| &&
             |1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n|.

    " a section of the map takes at most 100 entries
    lv_from = 1.
    WHILE lv_from <= lines( lt_line ).
      lv_to = lv_from + 99.
      IF lv_to > lines( lt_line ).
        lv_to = lines( lt_line ).
      ENDIF.
      result = result && |{ lv_to - lv_from + 1 } beginbfchar\n|.
      lv_index = lv_from.
      WHILE lv_index <= lv_to.
        result = result && lt_line[ lv_index ] && |\n|.
        lv_index = lv_index + 1.
      ENDWHILE.
      result = result && |endbfchar\n|.
      lv_from = lv_to + 1.
    ENDWHILE.

    result = result && |endcmap\nCMapName currentdict /CMap defineresource pop\nend\nend\n|.

  ENDMETHOD.

ENDCLASS.

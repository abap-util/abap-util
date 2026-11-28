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
    " The file is embedded whole - no subsetting yet, so the PDF grows by
    " the size of the font (deflated). Not supported: OpenType fonts with
    " PostScript outlines (CFF, the usual .otf), font collections (.ttc),
    " and everything a shaping engine does - ligatures, kerning, Arabic
    " joining, right-to-left.

    INTERFACES zabaputil_if_pdf_font.

    "! @parameter content  | the bytes of the .ttf file
    "! @parameter resource | the name in the resource dictionary, `F1`
    METHODS constructor
      IMPORTING
        content  TYPE xstring
        resource TYPE clike.

    "! The glyph number of a Unicode code point, 0 (the missing glyph)
    "! for a character the font does not have
    METHODS glyph
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE i.

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
    DATA mv_resource      TYPE string.
    DATA mv_name          TYPE string.
    TYPES temp1_b0c6217ea4 TYPE HASHED TABLE OF ty_s_table WITH UNIQUE KEY tag.
DATA mt_table         TYPE temp1_b0c6217ea4.
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
    TYPES temp2_b0c6217ea4 TYPE HASHED TABLE OF ty_s_glyph WITH UNIQUE KEY codepoint.
DATA mt_glyph         TYPE temp2_b0c6217ea4.

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

    "! The map from glyph numbers back to Unicode, for copy and search
    METHODS to_unicode
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zabaputil_cl_pdf_font_ttf IMPLEMENTATION.

  METHOD constructor.

    mv_content = content.
    mv_resource = resource.

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
    DATA lv_high TYPE i.
    lv_high = u16( off ).
    IF lv_high >= 32768.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the font file is damaged (a length is out of range)`.
    ENDIF.
    result = lv_high * 65536 + u16( off + 2 ).

  ENDMETHOD.

  METHOD table_offset.

    FIELD-SYMBOLS <table> TYPE zabaputil_cl_pdf_font_ttf=>ty_s_table.
    READ TABLE mt_table WITH KEY tag = tag ASSIGNING <table>.
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
    DATA lv_count TYPE i.

    IF xstrlen( mv_content ) < 12.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: not a TrueType font file`.
    ENDIF.

    " 00010000 or `true`: TrueType outlines. `OTTO` has PostScript
    " outlines, `ttcf` is a collection of several fonts
    lv_version = mv_content(4).
    IF lv_version <> `00010000` AND lv_version <> `74727565`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: only TrueType fonts (.ttf) can be embedded, not OpenType/CFF (.otf) or collections (.ttc)`.
    ENDIF.

    
    lv_count = u16( 4 ).
    lv_off = 12.
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
    FIELD-SYMBOLS <os2> TYPE zabaputil_cl_pdf_font_ttf=>ty_s_table.
    FIELD-SYMBOLS <post> TYPE zabaputil_cl_pdf_font_ttf=>ty_s_table.
      DATA temp1 TYPE xsdboolean.

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
    
    READ TABLE mt_table WITH KEY tag = `OS/2` ASSIGNING <os2>.
    IF sy-subrc = 0 AND <os2>-length >= 90.
      IF u16( <os2>-offset ) >= 2.
        mv_cap_height = scale( s16( <os2>-offset + 88 ) ).
      ENDIF.
    ENDIF.

    " italicAngle is a 16.16 fixed point number; its integer part will do
    
    READ TABLE mt_table WITH KEY tag = `post` ASSIGNING <post>.
    IF sy-subrc = 0 AND <post>-length >= 16.
      mv_italic_angle = s16( <post>-offset + 4 ).
      
      temp1 = boolc( u32( <post>-offset + 12 ) <> 0 ).
      mv_fixed_pitch = temp1.
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
    FIELD-SYMBOLS <name> TYPE zabaputil_cl_pdf_font_ttf=>ty_s_table.
    DATA lv_count TYPE i.
    DATA lv_strings TYPE zabaputil_cl_pdf_font_ttf=>ty_s_table-offset.
            DATA temp7 TYPE string.
          DATA temp8 TYPE xstring.

    mv_name = `EmbeddedFont`.

    
    READ TABLE mt_table WITH KEY tag = `name` ASSIGNING <name>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    
    lv_count = u16( <name>-offset + 2 ).
    
    lv_strings = <name>-offset + u16( <name>-offset + 4 ).

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
            
            temp7 = lv_x.
            lv_hex = lv_hex && temp7.
          ENDIF.
        ENDDO.
        IF lv_hex IS NOT INITIAL.
          
          temp8 = lv_hex.
          mv_name = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp8 ).
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

    DATA lv_cmap TYPE i.
    DATA lv_count TYPE i.
    lv_cmap = table_offset( `cmap` ).
    
    lv_count = u16( lv_cmap + 2 ).

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

    DATA ls_glyph TYPE zabaputil_cl_pdf_font_ttf=>ty_s_glyph.
    ls_glyph = glyph_info( val ).
    result = ls_glyph-glyph.

  ENDMETHOD.

  METHOD name.

    result = mv_name.

  ENDMETHOD.

  METHOD glyph_info.

    FIELD-SYMBOLS <glyph> TYPE zabaputil_cl_pdf_font_ttf=>ty_s_glyph.
    READ TABLE mt_glyph WITH KEY codepoint = val ASSIGNING <glyph>.
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
    DATA lv_seg_x2 TYPE i.
    DATA lv_end_codes TYPE i.
    DATA lv_start_codes TYPE i.
    DATA lv_deltas TYPE i.
    DATA lv_ranges TYPE i.

    IF val > 65535.
      RETURN.
    ENDIF.

    " four parallel arrays of segCount entries: endCode, (a pad), startCode,
    " idDelta, idRangeOffset. Binary search for the first segment that ends
    " at or after the character
    
    lv_seg_x2 = u16( mv_cmap + 6 ).
    
    lv_end_codes = mv_cmap + 14.
    
    lv_start_codes = lv_end_codes + lv_seg_x2 + 2.
    
    lv_deltas = lv_start_codes + lv_seg_x2.
    
    lv_ranges = lv_deltas + lv_seg_x2.

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
    DATA lv_count TYPE i.
    lv_count = u32( mv_cmap + 12 ).
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

    DATA temp9 TYPE zabaputil_cl_util_context=>ty_t_codepoint.
    temp9 = zabaputil_cl_util_context=>c_get_codepoints( val ).
    LOOP AT temp9 INTO lv_cp.
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

    DATA temp10 TYPE zabaputil_cl_util_context=>ty_t_codepoint.
    temp10 = zabaputil_cl_util_context=>c_get_codepoints( val ).
    LOOP AT temp10 INTO lv_cp.
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
    TYPES temp3 TYPE SORTED TABLE OF ty_s_glyph WITH NON-UNIQUE KEY glyph.
DATA lt_sorted TYPE temp3.
    DATA lv_last TYPE i.
    FIELD-SYMBOLS <glyph> LIKE LINE OF lt_sorted.
    DATA lv_file TYPE i.
    DATA lv_descriptor TYPE i.
    DATA lv_cid_font TYPE i.
    DATA lv_to_unicode TYPE i.

    " the widths of the glyphs in use; two characters may share a glyph,
    " and a glyph is listed once
    lt_sorted = mt_glyph.
    
    lv_last = -1.
    
    LOOP AT lt_sorted ASSIGNING <glyph>.
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

    
    lv_file = writer->add_stream( dict    = |/Length1 { xstrlen( mv_content ) }|
                                        content = mv_content ).

    
    lv_descriptor = writer->add_object(
        |<< /Type /FontDescriptor /FontName /{ mv_name } /Flags { lv_flags } /FontBBox [{ mv_bbox }] | &&
        |/ItalicAngle { mv_italic_angle } /Ascent { mv_ascent } /Descent { mv_descent * -1 } | &&
        |/CapHeight { mv_cap_height } /StemV 80 /FontFile2 { lv_file } 0 R >>| ).

    
    lv_cid_font = writer->add_object(
        |<< /Type /Font /Subtype /CIDFontType2 /BaseFont /{ mv_name } | &&
        |/CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> | &&
        |/FontDescriptor { lv_descriptor } 0 R /DW 1000 /W [{ lv_widths } ] /CIDToGIDMap /Identity >>| ).

    
    lv_to_unicode = writer->add_stream(
        zabaputil_cl_util_context=>conv_get_xstring_by_string( to_unicode( ) ) ).

    result = writer->add_object(
        |<< /Type /Font /Subtype /Type0 /BaseFont /{ mv_name } /Encoding /Identity-H | &&
        |/DescendantFonts [{ lv_cid_font } 0 R] /ToUnicode { lv_to_unicode } 0 R >>| ).

  ENDMETHOD.

  METHOD to_unicode.

    TYPES temp4 TYPE SORTED TABLE OF ty_s_glyph WITH NON-UNIQUE KEY glyph.
DATA lt_sorted TYPE temp4.
    DATA lt_line   TYPE string_table.
    DATA lv_cp     TYPE i.
    DATA lv_text   TYPE string.
    DATA lv_from   TYPE i.
    DATA lv_to     TYPE i.
    DATA lv_index  TYPE i.
    DATA lv_last TYPE i.
    FIELD-SYMBOLS <glyph> LIKE LINE OF lt_sorted.
      DATA temp11 LIKE LINE OF lt_line.
        FIELD-SYMBOLS <temp12> LIKE LINE OF lt_line.
        DATA temp13 LIKE sy-tabix.

    lt_sorted = mt_glyph.
    
    lv_last = -1.
    
    LOOP AT lt_sorted ASSIGNING <glyph>.
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
      
      temp11 = |<{ zabaputil_cl_pdf_writer=>hex_word( <glyph>-glyph ) }> <{ lv_text }>|.
      INSERT temp11 INTO TABLE lt_line.
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
        
        
        temp13 = sy-tabix.
        READ TABLE lt_line INDEX lv_index ASSIGNING <temp12>.
        sy-tabix = temp13.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        result = result && <temp12> && |\n|.
        lv_index = lv_index + 1.
      ENDWHILE.
      result = result && |endbfchar\n|.
      lv_from = lv_to + 1.
    ENDWHILE.

    result = result && |endcmap\nCMapName currentdict /CMap defineresource pop\nend\nend\n|.

  ENDMETHOD.

ENDCLASS.

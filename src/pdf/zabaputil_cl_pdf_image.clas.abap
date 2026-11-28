CLASS zabaputil_cl_pdf_image DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " A JPEG or PNG file as an image of the PDF.
    "
    " A JPEG goes into the file as it is (DCTDecode). So does the pixel data
    " of a PNG without an alpha channel: it is deflated already, the PDF
    " names the PNG predictor. A PNG with an alpha channel is unpacked,
    " split into colour and transparency and packed again - plain ABAP over
    " every pixel, so keep such an image at the size it is printed in. So
    " is an interlaced PNG: its seven passes are put together into rows,
    " which are packed again.

    "! @parameter content  | the bytes of the .jpg or .png file
    "! @parameter resource | the name in the resource dictionary, `I1`
    METHODS constructor
      IMPORTING
        content  TYPE xstring
        resource TYPE clike.

    "! The name of the image in the resource dictionary of the pages, `I1`
    METHODS resource
      RETURNING
        VALUE(result) TYPE string.

    "! The width in pixels
    METHODS width
      RETURNING
        VALUE(result) TYPE i.

    "! The height in pixels
    METHODS height
      RETURNING
        VALUE(result) TYPE i.

    "! `JPEG` or `PNG`
    METHODS format
      RETURNING
        VALUE(result) TYPE string.

    "! Write the objects of the image
    "! @parameter writer | the file
    "! @parameter result | the object number of the image
    METHODS write
      IMPORTING
        writer        TYPE REF TO zabaputil_cl_pdf_writer
      RETURNING
        VALUE(result) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES ty_t_byte TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_row,
        t_byte TYPE ty_t_byte,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    DATA mv_content    TYPE xstring.
    DATA mv_resource   TYPE string.
    DATA mv_format     TYPE string.
    DATA mv_width      TYPE i.
    DATA mv_height     TYPE i.
    " JPEG: the number of components. PNG: the number of colour
    " components, without the alpha channel
    DATA mv_colors     TYPE i.
    DATA mv_depth      TYPE i.
    " PNG only: colour type (0 grey, 2 RGB, 3 palette, 4 grey with alpha,
    " 6 RGB with alpha), and the chunks PLTE, tRNS and IDAT in hex
    DATA mv_color_type TYPE i.
    DATA mv_palette    TYPE string.
    DATA mv_trns       TYPE string.
    DATA mv_idat       TYPE string.
    DATA mv_interlaced TYPE abap_bool.

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

    METHODS u32
      IMPORTING
        off           TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS read_jpeg.
    METHODS read_png.

    METHODS write_jpeg
      IMPORTING
        writer        TYPE REF TO zabaputil_cl_pdf_writer
      RETURNING
        VALUE(result) TYPE i.

    METHODS write_png
      IMPORTING
        writer        TYPE REF TO zabaputil_cl_pdf_writer
      RETURNING
        VALUE(result) TYPE i.

    "! The image data of an interlaced PNG as the rows of a plain one:
    "! the seven passes unfiltered, their pixels put where they belong,
    "! and every row with filter 0, packed again
    METHODS png_deinterlace
      RETURNING
        VALUE(result) TYPE string.

    "! The rows of a filtered image unfiltered: for every row its bytes
    CLASS-METHODS png_unfilter
      IMPORTING
        data          TYPE xstring
        offset        TYPE i
        rows          TYPE i
        row_bytes     TYPE i
        bpp           TYPE i
      EXPORTING
        next          TYPE i
        result        TYPE ty_t_row.

    "! The entry that makes a colour of a PNG without alpha channel
    "! transparent, from its tRNS chunk
    METHODS png_mask
      RETURNING
        VALUE(result) TYPE string.

    "! Split the unpacked rows of a PNG with alpha channel
    METHODS png_split_alpha
      EXPORTING
        color TYPE xstring
        alpha TYPE xstring.

ENDCLASS.


CLASS zabaputil_cl_pdf_image IMPLEMENTATION.

  METHOD constructor.

    DATA lv_magic TYPE string.

    mv_content = content.
    mv_resource = resource.

    IF xstrlen( mv_content ) >= 8.
      lv_magic = mv_content(8).
    ENDIF.
    IF lv_magic = `89504E470D0A1A0A`.
      mv_format = `PNG`.
      read_png( ).
    ELSEIF strlen( lv_magic ) >= 4 AND substring( val = lv_magic
                                                  len = 4 ) = `FFD8`.
      mv_format = `JPEG`.
      read_jpeg( ).
    ELSE.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the image is neither a JPEG nor a PNG file`.
    ENDIF.

    IF mv_width <= 0 OR mv_height <= 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the image file has no size (damaged header)`.
    ENDIF.

  ENDMETHOD.

  METHOD resource.

    result = mv_resource.

  ENDMETHOD.

  METHOD width.

    result = mv_width.

  ENDMETHOD.

  METHOD height.

    result = mv_height.

  ENDMETHOD.

  METHOD format.

    result = mv_format.

  ENDMETHOD.

  METHOD u8.

    IF off < 0 OR off >= xstrlen( mv_content ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the image file is damaged (it ends too early)`.
    ENDIF.
    result = mv_content+off(1).

  ENDMETHOD.

  METHOD u16.

    result = u8( off ) * 256 + u8( off + 1 ).

  ENDMETHOD.

  METHOD u32.

    " offsets and lengths: none of them reaches 2^31 in a file that fits
    " into memory, and a larger value would not fit into an integer
    DATA(lv_high) = u16( off ).
    IF lv_high >= 32768.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the image file is damaged (a length is out of range)`.
    ENDIF.
    result = lv_high * 65536 + u16( off + 2 ).

  ENDMETHOD.

  METHOD read_jpeg.

    DATA lv_off    TYPE i VALUE 2.
    DATA lv_marker TYPE i.

    " a chain of segments, each FF, a marker byte and (for most) a length.
    " The size is in the start-of-frame segment: C0 to CF, except C4
    " (Huffman tables), C8 and CC
    DATA(lv_len) = xstrlen( mv_content ).
    WHILE lv_off + 4 <= lv_len.
      IF u8( lv_off ) <> 255.
        lv_off = lv_off + 1.
        CONTINUE.
      ENDIF.
      lv_marker = u8( lv_off + 1 ).
      IF lv_marker = 255.
        " a fill byte
        lv_off = lv_off + 1.
        CONTINUE.
      ENDIF.
      IF lv_marker >= 192 AND lv_marker <= 207 AND lv_marker <> 196 AND lv_marker <> 200 AND lv_marker <> 204.
        mv_depth = u8( lv_off + 4 ).
        mv_height = u16( lv_off + 5 ).
        mv_width = u16( lv_off + 7 ).
        mv_colors = u8( lv_off + 9 ).
        RETURN.
      ENDIF.
      IF lv_marker = 218 OR lv_marker = 217.
        " start of scan or end of image: no frame header before the data
        RETURN.
      ENDIF.
      IF ( lv_marker >= 208 AND lv_marker <= 216 ) OR lv_marker = 1.
        " restart markers, start of image: no length
        lv_off = lv_off + 2.
      ELSE.
        lv_off = lv_off + 2 + u16( lv_off + 2 ).
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD read_png.

    DATA lv_off   TYPE i VALUE 8.
    DATA lv_size  TYPE i.
    DATA lv_type  TYPE string.
    DATA lv_chunk TYPE xstring.
    DATA lv_data  TYPE i.

    " chunks: four bytes length, four bytes type, the data, four bytes CRC
    DATA(lv_len) = xstrlen( mv_content ).
    WHILE lv_off + 12 <= lv_len.
      lv_size = u32( lv_off ).
      lv_data = lv_off + 8.
      IF lv_size < 0 OR lv_data + lv_size + 4 > lv_len.
        EXIT.
      ENDIF.
      lv_chunk = mv_content+lv_off(8).
      lv_type = lv_chunk.
      lv_type = substring( val = lv_type
                           off = 8 ).
      CLEAR lv_chunk.
      IF lv_size > 0.
        lv_chunk = mv_content+lv_data(lv_size).
      ENDIF.

      CASE lv_type.
        WHEN `49484452`.
          " IHDR
          mv_width = u32( lv_data ).
          mv_height = u32( lv_data + 4 ).
          mv_depth = u8( lv_data + 8 ).
          mv_color_type = u8( lv_data + 9 ).
          " Adam7: the pixels come in seven passes
          mv_interlaced = xsdbool( u8( lv_data + 12 ) <> 0 ).
        WHEN `504C5445`.
          " PLTE
          mv_palette = lv_chunk.
        WHEN `74524E53`.
          " tRNS
          mv_trns = lv_chunk.
        WHEN `49444154`.
          " IDAT - one zlib stream, cut into chunks
          mv_idat = mv_idat && CONV string( lv_chunk ).
        WHEN `49454E44`.
          " IEND
          EXIT.
      ENDCASE.

      lv_off = lv_data + lv_size + 4.
    ENDWHILE.

    CASE mv_color_type.
      WHEN 0 OR 3 OR 4.
        mv_colors = 1.
      WHEN 2 OR 6.
        mv_colors = 3.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: PNG colour type { mv_color_type } is not supported|.
    ENDCASE.

    IF mv_idat IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the PNG file has no image data`.
    ENDIF.
    IF mv_color_type = 3 AND mv_palette IS INITIAL.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the PNG file has no palette`.
    ENDIF.
    IF ( mv_color_type = 4 OR mv_color_type = 6 ) AND mv_depth <> 8 AND mv_depth <> 16.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the PNG file is damaged (bit depth of an image with alpha channel)`.
    ENDIF.

    IF mv_interlaced = abap_true.
      mv_idat = png_deinterlace( ).
    ENDIF.

  ENDMETHOD.

  METHOD write.

    IF mv_format = `PNG`.
      result = write_png( writer ).
    ELSE.
      result = write_jpeg( writer ).
    ENDIF.

  ENDMETHOD.

  METHOD write_jpeg.

    DATA lv_space TYPE string.

    " four components are CMYK, which Adobe applications store inverted
    CASE mv_colors.
      WHEN 1.
        lv_space = `/DeviceGray`.
      WHEN 4.
        lv_space = `/DeviceCMYK /Decode [1 0 1 0 1 0 1 0]`.
      WHEN OTHERS.
        lv_space = `/DeviceRGB`.
    ENDCASE.

    result = writer->add_stream(
        dict     = |/Type /XObject /Subtype /Image /Width { mv_width } /Height { mv_height } | &&
                   |/ColorSpace { lv_space } /BitsPerComponent { mv_depth } /Filter /DCTDecode|
        content  = mv_content
        compress = abap_false ).

  ENDMETHOD.

  METHOD write_png.

    DATA lv_space TYPE string.
    DATA lv_dict  TYPE string.
    DATA lv_color TYPE xstring.
    DATA lv_alpha TYPE xstring.
    DATA lv_parms TYPE string.

    CASE mv_color_type.
      WHEN 0 OR 4.
        lv_space = `/DeviceGray`.
      WHEN 3.
        " the palette is a string of RGB triples, written in hex
        lv_space = |[/Indexed /DeviceRGB { strlen( mv_palette ) DIV 6 - 1 } <{ mv_palette }>]|.
      WHEN OTHERS.
        lv_space = `/DeviceRGB`.
    ENDCASE.

    " the rows carry the PNG filter byte: predictor 15 tells the viewer
    lv_parms = |/DecodeParms << /Predictor 15 /Colors { mv_colors } /BitsPerComponent { mv_depth } | &&
               |/Columns { mv_width } >>|.
    lv_dict = |/Type /XObject /Subtype /Image /Width { mv_width } /Height { mv_height } | &&
              |/ColorSpace { lv_space } /BitsPerComponent { mv_depth }|.

    IF mv_color_type = 4 OR mv_color_type = 6.

      png_split_alpha( IMPORTING color = lv_color
                                 alpha = lv_alpha ).

      DATA(lv_mask) = writer->add_stream(
          dict     = |/Type /XObject /Subtype /Image /Width { mv_width } /Height { mv_height } | &&
                     |/ColorSpace /DeviceGray /BitsPerComponent { mv_depth } /Filter /FlateDecode | &&
                     |/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent { mv_depth } | &&
                     |/Columns { mv_width } >>|
          content  = zabaputil_cl_util_context=>zlib_compress( lv_alpha )
          compress = abap_false ).

      result = writer->add_stream(
          dict     = |{ lv_dict } /Filter /FlateDecode { lv_parms } /SMask { lv_mask } 0 R|
          content  = zabaputil_cl_util_context=>zlib_compress( lv_color )
          compress = abap_false ).

    ELSE.

      lv_color = mv_idat.
      result = writer->add_stream( dict     = |{ lv_dict } /Filter /FlateDecode { lv_parms }{ png_mask( ) }|
                                   content  = lv_color
                                   compress = abap_false ).

    ENDIF.

  ENDMETHOD.

  METHOD png_deinterlace.

    DATA lv_packed   TYPE xstring.
    DATA lv_raw      TYPE xstring.
    DATA lt_image    TYPE ty_t_row.
    DATA ls_row      TYPE ty_s_row.
    DATA lt_pass     TYPE ty_t_row.
    DATA lv_offset   TYPE i.
    DATA lv_x        TYPE i.
    DATA lv_y        TYPE i.
    DATA lv_bit      TYPE i.
    DATA lv_value    TYPE i.
    DATA lv_target   TYPE i.
    DATA lv_hex      TYPE x LENGTH 1.
    DATA lt_hex      TYPE string_table.
    DATA lv_line     TYPE string.
    DATA lv_b        TYPE i.
    DATA lv_k        TYPE i.

    " the seven passes: first column and row, and the steps between them
    TYPES:
      BEGIN OF ty_s_pass,
        x0 TYPE i,
        y0 TYPE i,
        dx TYPE i,
        dy TYPE i,
      END OF ty_s_pass.
    DATA lt_adam7 TYPE STANDARD TABLE OF ty_s_pass WITH EMPTY KEY.
    lt_adam7 = VALUE #( ( x0 = 0 y0 = 0 dx = 8 dy = 8 )
                        ( x0 = 4 y0 = 0 dx = 8 dy = 8 )
                        ( x0 = 0 y0 = 4 dx = 4 dy = 8 )
                        ( x0 = 2 y0 = 0 dx = 4 dy = 4 )
                        ( x0 = 0 y0 = 2 dx = 2 dy = 4 )
                        ( x0 = 1 y0 = 0 dx = 2 dy = 2 )
                        ( x0 = 0 y0 = 1 dx = 1 dy = 2 ) ).

    lv_packed = mv_idat.
    lv_raw = zabaputil_cl_util_context=>zlib_decompress( lv_packed ).

    " the samples of a pixel, alpha included, and their bits
    DATA(lv_channels) = mv_colors.
    IF mv_color_type = 4 OR mv_color_type = 6.
      lv_channels = lv_channels + 1.
    ENDIF.
    DATA(lv_bits) = lv_channels * mv_depth.
    DATA(lv_bpp) = nmax( val1 = 1
                         val2 = lv_bits DIV 8 ).
    DATA(lv_row_bytes) = ( mv_width * lv_bits + 7 ) DIV 8.

    " the image, every byte 0 to begin with
    DO lv_row_bytes TIMES.
      INSERT 0 INTO TABLE ls_row-t_byte.
    ENDDO.
    DO mv_height TIMES.
      INSERT ls_row INTO TABLE lt_image.
    ENDDO.

    LOOP AT lt_adam7 ASSIGNING FIELD-SYMBOL(<pass>).
      DATA(lv_width) = 0.
      DATA(lv_height) = 0.
      IF mv_width > <pass>-x0.
        lv_width = ( mv_width - <pass>-x0 + <pass>-dx - 1 ) DIV <pass>-dx.
      ENDIF.
      IF mv_height > <pass>-y0.
        lv_height = ( mv_height - <pass>-y0 + <pass>-dy - 1 ) DIV <pass>-dy.
      ENDIF.
      " a pass without pixels has no rows at all, not even a filter byte
      IF lv_width = 0 OR lv_height = 0.
        CONTINUE.
      ENDIF.

      png_unfilter( EXPORTING data      = lv_raw
                              offset    = lv_offset
                              rows      = lv_height
                              row_bytes = ( lv_width * lv_bits + 7 ) DIV 8
                              bpp       = lv_bpp
                    IMPORTING next      = lv_offset
                              result    = lt_pass ).

      " every pixel of the pass to its place in the image
      lv_y = 0.
      LOOP AT lt_pass ASSIGNING FIELD-SYMBOL(<pass_row>).
        ASSIGN lt_image[ <pass>-y0 + lv_y * <pass>-dy + 1 ] TO FIELD-SYMBOL(<image_row>).
        lv_x = 0.
        WHILE lv_x < lv_width.
          lv_target = <pass>-x0 + lv_x * <pass>-dx.
          IF lv_bits >= 8.
            " whole bytes
            lv_k = 0.
            WHILE lv_k < lv_bits DIV 8.
              <image_row>-t_byte[ lv_target * ( lv_bits DIV 8 ) + lv_k + 1 ] =
                  <pass_row>-t_byte[ lv_x * ( lv_bits DIV 8 ) + lv_k + 1 ].
              lv_k = lv_k + 1.
            ENDWHILE.
          ELSE.
            " 1, 2 or 4 bits of one byte, from the left
            lv_bit = lv_x * lv_bits.
            lv_b = <pass_row>-t_byte[ lv_bit DIV 8 + 1 ].
            lv_value = ( lv_b DIV ipow( base = 2
                                        exp  = 8 - lv_bit MOD 8 - lv_bits ) ) MOD ipow( base = 2
                                                                                        exp  = lv_bits ).
            lv_bit = lv_target * lv_bits.
            ASSIGN <image_row>-t_byte[ lv_bit DIV 8 + 1 ] TO FIELD-SYMBOL(<byte>).
            <byte> = <byte> + lv_value * ipow( base = 2
                                               exp  = 8 - lv_bit MOD 8 - lv_bits ).
          ENDIF.
          lv_x = lv_x + 1.
        ENDWHILE.
        lv_y = lv_y + 1.
      ENDLOOP.
    ENDLOOP.

    " the rows of a plain PNG: filter 0, then the bytes
    LOOP AT lt_image ASSIGNING <image_row>.
      INSERT `00` INTO TABLE lt_hex.
      CLEAR lv_line.
      LOOP AT <image_row>-t_byte INTO lv_b.
        lv_hex = lv_b.
        lv_line = lv_line && lv_hex.
      ENDLOOP.
      INSERT lv_line INTO TABLE lt_hex.
    ENDLOOP.
    lv_raw = concat_lines_of( lt_hex ).
    result = zabaputil_cl_util_context=>zlib_compress( lv_raw ).

  ENDMETHOD.

  METHOD png_unfilter.

    DATA ls_row    TYPE ty_s_row.
    DATA ls_prev   TYPE ty_s_row.
    DATA lv_filter TYPE i.
    DATA lv_x      TYPE x LENGTH 1.
    DATA lv_i      TYPE i.
    DATA lv_raw    TYPE i.
    DATA lv_a      TYPE i.
    DATA lv_b      TYPE i.
    DATA lv_c      TYPE i.
    DATA lv_p      TYPE i.
    DATA lv_pa     TYPE i.
    DATA lv_pb     TYPE i.
    DATA lv_pc     TYPE i.
    DATA lv_value  TYPE i.

    CLEAR result.
    next = offset.
    DATA(lv_length) = xstrlen( data ).

    " the row above the first one is zeros
    DO row_bytes TIMES.
      INSERT 0 INTO TABLE ls_prev-t_byte.
    ENDDO.

    DO rows TIMES.
      IF next + 1 + row_bytes > lv_length.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: the PNG file is damaged (image data too short)`.
      ENDIF.
      lv_x = data+next(1).
      lv_filter = lv_x.
      next = next + 1.
      CLEAR ls_row.
      lv_i = 0.
      WHILE lv_i < row_bytes.
        lv_x = data+next(1).
        lv_raw = lv_x.
        next = next + 1.
        lv_a = 0.
        lv_c = 0.
        IF lv_i >= bpp.
          lv_a = ls_row-t_byte[ lv_i - bpp + 1 ].
          lv_c = ls_prev-t_byte[ lv_i - bpp + 1 ].
        ENDIF.
        lv_b = ls_prev-t_byte[ lv_i + 1 ].
        CASE lv_filter.
          WHEN 1.
            lv_value = lv_raw + lv_a.
          WHEN 2.
            lv_value = lv_raw + lv_b.
          WHEN 3.
            lv_value = lv_raw + ( lv_a + lv_b ) DIV 2.
          WHEN 4.
            " Paeth: the neighbour closest to a + b - c
            lv_p = lv_a + lv_b - lv_c.
            lv_pa = abs( lv_p - lv_a ).
            lv_pb = abs( lv_p - lv_b ).
            lv_pc = abs( lv_p - lv_c ).
            IF lv_pa <= lv_pb AND lv_pa <= lv_pc.
              lv_value = lv_raw + lv_a.
            ELSEIF lv_pb <= lv_pc.
              lv_value = lv_raw + lv_b.
            ELSE.
              lv_value = lv_raw + lv_c.
            ENDIF.
          WHEN OTHERS.
            lv_value = lv_raw.
        ENDCASE.
        INSERT lv_value MOD 256 INTO TABLE ls_row-t_byte.
        lv_i = lv_i + 1.
      ENDWHILE.
      INSERT ls_row INTO TABLE result.
      ls_prev = ls_row.
    ENDDO.

  ENDMETHOD.

  METHOD png_mask.

    DATA lv_x     TYPE xstring.
    DATA lv_index TYPE i.
    DATA lv_byte  TYPE i.
    DATA lv_value TYPE i.

    IF mv_trns IS INITIAL.
      RETURN.
    ENDIF.
    lv_x = mv_trns.

    CASE mv_color_type.
      WHEN 3.
        " one alpha value per palette entry: the first entry that is fully
        " transparent becomes the masked colour, the others stay opaque
        WHILE lv_index < xstrlen( lv_x ).
          lv_byte = lv_x+lv_index(1).
          IF lv_byte = 0.
            result = | /Mask [{ lv_index } { lv_index }]|.
            RETURN.
          ENDIF.
          lv_index = lv_index + 1.
        ENDWHILE.
      WHEN 0.
        " one grey value of two bytes
        IF xstrlen( lv_x ) >= 2.
          lv_byte = lv_x(1).
          lv_value = lv_byte * 256.
          lv_byte = lv_x+1(1).
          lv_value = lv_value + lv_byte.
          result = | /Mask [{ lv_value } { lv_value }]|.
        ENDIF.
      WHEN 2.
        " red, green and blue, two bytes each
        IF xstrlen( lv_x ) >= 6.
          result = ` /Mask [`.
          DO 3 TIMES.
            lv_index = ( sy-index - 1 ) * 2.
            lv_byte = lv_x+lv_index(1).
            lv_value = lv_byte * 256.
            lv_index = lv_index + 1.
            lv_byte = lv_x+lv_index(1).
            lv_value = lv_value + lv_byte.
            result = |{ result }{ lv_value } { lv_value } |.
          ENDDO.
          result = result && `]`.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD png_split_alpha.

    DATA lv_packed  TYPE xstring.
    DATA lv_raw     TYPE string.
    DATA lv_color   TYPE string.
    DATA lv_alpha   TYPE string.
    DATA lv_filter  TYPE string.
    DATA lv_off     TYPE i.
    DATA lv_row     TYPE i.

    CLEAR color.
    CLEAR alpha.

    " everything in hex characters: two per byte
    lv_packed = mv_idat.
    lv_raw = zabaputil_cl_util_context=>zlib_decompress( lv_packed ).

    DATA(lv_sample) = mv_depth DIV 4.
    DATA(lv_color_len) = mv_colors * lv_sample.
    DATA(lv_pixel) = lv_color_len + lv_sample.
    DATA(lv_row_len) = 2 + mv_width * lv_pixel.

    IF strlen( lv_raw ) < lv_row_len * mv_height.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the PNG file is damaged (image data too short)`.
    ENDIF.

    " every row starts with its filter byte, and both halves keep it: the
    " PNG filters work on the bytes of one channel, so a row filtered as
    " RGBA is still a valid row as RGB and as A
    " a row at a time in a table, joined once: the halves are not copied
    " again for every pixel
    DATA lt_color TYPE string_table.
    DATA lt_alpha TYPE string_table.
    WHILE lv_row < mv_height.
      lv_off = lv_row * lv_row_len.
      lv_filter = substring( val = lv_raw
                             off = lv_off
                             len = 2 ).
      lv_color = lv_filter.
      lv_alpha = lv_filter.
      lv_off = lv_off + 2.
      DO mv_width TIMES.
        lv_color = lv_color && substring( val = lv_raw
                                          off = lv_off
                                          len = lv_color_len ).
        lv_alpha = lv_alpha && substring( val = lv_raw
                                          off = lv_off + lv_color_len
                                          len = lv_sample ).
        lv_off = lv_off + lv_pixel.
      ENDDO.
      INSERT lv_color INTO TABLE lt_color.
      INSERT lv_alpha INTO TABLE lt_alpha.
      lv_row = lv_row + 1.
    ENDWHILE.

    color = concat_lines_of( lt_color ).
    alpha = concat_lines_of( lt_alpha ).

  ENDMETHOD.

ENDCLASS.

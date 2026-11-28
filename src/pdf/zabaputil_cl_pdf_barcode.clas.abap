CLASS zabaputil_cl_pdf_barcode DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Barcodes as vector graphics: the modules of a symbol, worked out in
    " plain ABAP, and drawn as filled rectangles on a page - sharp at any
    " size, no image, no font.
    "
    "   zabaputil_cl_pdf_barcode=>draw( page = lo_page  type = `QR`
    "                                   val  = lv_url   x = 150 y = 20 w = 30 h = 30 ).
    "
    " Code 128 (code sets B and C - printable ASCII, digits in pairs),
    " EAN-13 (12 digits and the check digit, or 13 with a correct one),
    " QR (byte mode, UTF-8, versions 1 to 40, error correction L, M, Q,
    " H) and Data Matrix (ECC 200, square, ASCII encodation, Latin-1).
    " A linear code is its modules from left to right, `1` a bar; a matrix
    " code its rows from the top, `1` a dark module. The quiet zone is
    " not part of it: leave ten modules beside a linear code, four around
    " a QR code and one around a Data Matrix.

    CONSTANTS:
      BEGIN OF cs_type,
        code128    TYPE string VALUE `CODE128`,
        ean13      TYPE string VALUE `EAN13`,
        qr         TYPE string VALUE `QR`,
        datamatrix TYPE string VALUE `DATAMATRIX`,
      END OF cs_type.

    "! The modules of a Code 128 symbol, from the start to the stop code
    CLASS-METHODS code128
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The 95 modules of an EAN-13 symbol
    "! @parameter val | 12 digits, the check digit is added; or 13 with it
    CLASS-METHODS ean13
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The rows of a QR code, the smallest version the text fits into
    "! @parameter val  | the text, written as UTF-8 bytes
    "! @parameter ecc  | the error correction: L 7%, M 15%, Q 25%, H 30%
    "! @parameter mask | 0 to 7; -1 for the mask the rules of the standard choose
    CLASS-METHODS qr
      IMPORTING
        val           TYPE clike
        ecc           TYPE clike DEFAULT `M`
        mask          TYPE i DEFAULT -1
      RETURNING
        VALUE(result) TYPE string_table.

    "! The rows of a Data Matrix (ECC 200), the smallest square size the
    "! text fits into
    "! @parameter val | the text: characters up to U+00FF
    CLASS-METHODS datamatrix
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    "! The rows of a symbol: one row for a linear code
    "! @parameter type | one of cs_type
    CLASS-METHODS modules
      IMPORTING
        type          TYPE clike
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    "! A barcode on a page, in black, inside the box x, y, w, h. A linear
    "! code fills the box; the modules of a matrix code are square, as
    "! large as the box allows, at its top left corner
    "! @parameter type | one of cs_type
    CLASS-METHODS draw
      IMPORTING
        page TYPE REF TO zabaputil_cl_pdf_page
        type TYPE clike
        val  TYPE clike
        x    TYPE numeric
        y    TYPE numeric
        w    TYPE numeric
        h    TYPE numeric.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES ty_t_int TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    "! a matrix of modules: value and whether it is a function module
    TYPES:
      BEGIN OF ty_s_matrix,
        size     TYPE i,
        t_module TYPE ty_t_int,
        t_fixed  TYPE ty_t_int,
      END OF ty_s_matrix.

    CLASS-METHODS xor
      IMPORTING
        a             TYPE i
        b             TYPE i
      RETURNING
        VALUE(result) TYPE i.

    "! Exponents and logarithms in GF(256) of a primitive polynomial
    CLASS-METHODS gf_tables
      IMPORTING
        poly TYPE i
      EXPORTING
        exp  TYPE ty_t_int
        log  TYPE ty_t_int.

    "! The error correction codewords of a block (Reed-Solomon)
    "! @parameter first | the exponent of the first root of the generator
    CLASS-METHODS reed_solomon
      IMPORTING
        data          TYPE ty_t_int
        count         TYPE i
        poly          TYPE i
        first         TYPE i
      RETURNING
        VALUE(result) TYPE ty_t_int.

    CLASS-METHODS rows_of
      IMPORTING
        matrix        TYPE ty_s_matrix
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS set
      IMPORTING
        x      TYPE i
        y      TYPE i
        val    TYPE abap_bool
        fixed  TYPE abap_bool DEFAULT abap_true
      CHANGING
        matrix TYPE ty_s_matrix.

    CLASS-METHODS get
      IMPORTING
        x             TYPE i
        y             TYPE i
        matrix        TYPE ty_s_matrix
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS qr_blocks
      IMPORTING
        version TYPE i
        ecc     TYPE string
      EXPORTING
        t_count TYPE ty_t_int
        t_total TYPE ty_t_int
        t_data  TYPE ty_t_int.

    CLASS-METHODS qr_function
      IMPORTING
        version TYPE i
      CHANGING
        matrix  TYPE ty_s_matrix.

    CLASS-METHODS qr_format
      IMPORTING
        ecc    TYPE string
        mask   TYPE i
      CHANGING
        matrix TYPE ty_s_matrix.

    CLASS-METHODS qr_masked
      IMPORTING
        x             TYPE i
        y             TYPE i
        mask          TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS qr_penalty
      IMPORTING
        matrix        TYPE ty_s_matrix
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS dm_module
      IMPORTING
        row       TYPE i
        col       TYPE i
        codeword  TYPE i
        bit       TYPE i
        rows      TYPE i
        cols      TYPE i
        codewords TYPE ty_t_int
      CHANGING
        modules   TYPE ty_t_int.

    CLASS-METHODS dm_place
      IMPORTING
        rows          TYPE i
        cols          TYPE i
        codewords     TYPE ty_t_int
      RETURNING
        VALUE(result) TYPE ty_t_int.

ENDCLASS.


CLASS zabaputil_cl_pdf_barcode IMPLEMENTATION.

  METHOD xor.

    DATA lv_a TYPE x LENGTH 4.
    DATA lv_b TYPE x LENGTH 4.
    DATA lv_c TYPE x LENGTH 4.

    lv_a = a.
    lv_b = b.
    lv_c = lv_a BIT-XOR lv_b.
    result = lv_c.

  ENDMETHOD.

  METHOD code128.

    DATA lt_value TYPE ty_t_int.
    DATA lv_set   TYPE c LENGTH 1.
    DATA lv_pos   TYPE i.
    DATA lv_run   TYPE i.
    DATA lv_sum   TYPE i.

    DATA(lt_pattern) = VALUE string_table( ).
    SPLIT `11011001100 11001101100 11001100110 10010011000 10010001100 10001001100 10011001000 100110` &&
      `00100 10001100100 11001001000 11001000100 11000100100 10110011100 10011011100 10011001110 ` &&
      `10111001100 10011101100 10011100110 11001110010 11001011100 11001001110 11011100100 110011` &&
      `10100 11101101110 11101001100 11100101100 11100100110 11101100100 11100110100 11100110010 ` &&
      `11011011000 11011000110 11000110110 10100011000 10001011000 10001000110 10110001000 100011` &&
      `01000 10001100010 11010001000 11000101000 11000100010 10110111000 10110001110 10001101110 ` &&
      `10111011000 10111000110 10001110110 11101110110 11010001110 11000101110 11011101000 110111` &&
      `00010 11011101110 11101011000 11101000110 11100010110 11101101000 11101100010 11100011010 ` &&
      `11101111010 11001000010 11110001010 10100110000 10100001100 10010110000 10010000110 100001` &&
      `01100 10000100110 10110010000 10110000100 10011010000 10011000010 10000110100 10000110010 ` &&
      `11000010010 11001010000 11110111010 11000010100 10001111010 10100111100 10010111100 100100` &&
      `11110 10111100100 10011110100 10011110010 11110100100 11110010100 11110010010 11011011110 ` &&
      `11011110110 11110110110 10101111000 10100011110 10001011110 10111101000 10111100010 111101` &&
      `01000 11110100010 10111011110 10111101110 11101011110 11110101110 11010000100 11010010000 ` &&
      `11010011100 1100011101011` AT ` ` INTO TABLE lt_pattern.

    DATA(lv_text) = CONV string( val ).
    DATA(lv_length) = strlen( lv_text ).
    IF lv_length = 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: a Code 128 barcode needs a text`.
    ENDIF.
    DATA(lt_cp) = zabaputil_cl_util_context=>c_get_codepoints( lv_text ).
    IF lines( lt_cp ) <> lv_length.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: Code 128 takes the characters of ASCII from the blank to the tilde`.
    ENDIF.
    LOOP AT lt_cp INTO DATA(lv_cp).
      IF lv_cp < 32 OR lv_cp > 126.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: Code 128 takes the characters of ASCII from the blank to the tilde`.
      ENDIF.
    ENDLOOP.

    " code set C for digits in pairs where it saves room, B for the rest
    WHILE lv_pos < lv_length.
      lv_run = 0.
      WHILE lv_pos + lv_run < lv_length AND substring( val = lv_text
                                                       off = lv_pos + lv_run
                                                       len = 1 ) CO `0123456789`.
        lv_run = lv_run + 1.
      ENDWHILE.

      IF lv_set = `C` AND lv_run >= 2.
        INSERT CONV i( substring( val = lv_text
                                  off = lv_pos
                                  len = 2 ) ) INTO TABLE lt_value.
        lv_pos = lv_pos + 2.
        CONTINUE.
      ENDIF.

      IF lv_run >= 4 AND ( lv_set IS INITIAL OR lv_run >= 6 OR lv_pos + lv_run = lv_length ).
        " an odd run starts with a digit in code set B
        IF lv_run MOD 2 = 1.
          IF lv_set IS INITIAL.
            INSERT 104 INTO TABLE lt_value.
            lv_set = `B`.
          ENDIF.
          INSERT lt_cp[ lv_pos + 1 ] - 32 INTO TABLE lt_value.
          lv_pos = lv_pos + 1.
        ENDIF.
        IF lv_set IS INITIAL.
          INSERT 105 INTO TABLE lt_value.
        ELSE.
          INSERT 99 INTO TABLE lt_value.
        ENDIF.
        lv_set = `C`.
        CONTINUE.
      ENDIF.

      IF lv_set IS INITIAL AND lv_run = 2 AND lv_length = 2.
        INSERT 105 INTO TABLE lt_value.
        lv_set = `C`.
        CONTINUE.
      ENDIF.

      IF lv_set IS INITIAL.
        INSERT 104 INTO TABLE lt_value.
      ELSEIF lv_set = `C`.
        INSERT 100 INTO TABLE lt_value.
      ENDIF.
      lv_set = `B`.
      INSERT lt_cp[ lv_pos + 1 ] - 32 INTO TABLE lt_value.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    " the check value: the start code and every value times its position
    LOOP AT lt_value INTO DATA(lv_value).
      IF sy-tabix = 1.
        lv_sum = lv_value.
      ELSE.
        lv_sum = lv_sum + lv_value * ( sy-tabix - 1 ).
      ENDIF.
    ENDLOOP.
    INSERT lv_sum MOD 103 INTO TABLE lt_value.
    INSERT 106 INTO TABLE lt_value.

    LOOP AT lt_value INTO lv_value.
      result = result && lt_pattern[ lv_value + 1 ].
    ENDLOOP.

  ENDMETHOD.

  METHOD ean13.

    DATA lv_sum   TYPE i.
    DATA lv_digit TYPE i.

    DATA(lt_l) = VALUE string_table( ).
    DATA(lt_g) = VALUE string_table( ).
    DATA(lt_r) = VALUE string_table( ).
    DATA(lt_parity) = VALUE string_table( ).
    SPLIT `0001101 0011001 0010011 0111101 0100011 0110001 0101111 0111011 0110111 0001011` AT ` ` INTO TABLE lt_l.
    SPLIT `0100111 0110011 0011011 0100001 0011101 0111001 0000101 0010001 0001001 0010111` AT ` ` INTO TABLE lt_g.
    SPLIT `1110010 1100110 1101100 1000010 1011100 1001110 1010000 1000100 1001000 1110100` AT ` ` INTO TABLE lt_r.
    SPLIT `AAAAAA AABABB AABBAB AABBBA ABAABB ABBAAB ABBBAA ABABAB ABABBA ABBABA` AT ` ` INTO TABLE lt_parity.

    DATA(lv_text) = condense( val ).
    IF lv_text CN `0123456789` OR ( strlen( lv_text ) <> 12 AND strlen( lv_text ) <> 13 ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: { lv_text } is no EAN-13 - 12 or 13 digits|.
    ENDIF.

    " the check digit: weights 1 and 3 from the left
    DO 12 TIMES.
      lv_digit = substring( val = lv_text
                            off = sy-index - 1
                            len = 1 ).
      IF sy-index MOD 2 = 0.
        lv_sum = lv_sum + lv_digit * 3.
      ELSE.
        lv_sum = lv_sum + lv_digit.
      ENDIF.
    ENDDO.
    DATA(lv_check) = ( 10 - lv_sum MOD 10 ) MOD 10.
    IF strlen( lv_text ) = 13.
      IF substring( val = lv_text
                    off = 12
                    len = 1 ) <> |{ lv_check }|.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: the check digit of the EAN-13 { lv_text } is { lv_check }|.
      ENDIF.
    ELSE.
      lv_text = |{ lv_text }{ lv_check }|.
    ENDIF.

    " the first digit is in the parity of the next six
    lv_digit = lv_text(1).
    DATA(lv_parity) = lt_parity[ lv_digit + 1 ].
    result = `101`.
    DO 6 TIMES.
      lv_digit = substring( val = lv_text
                            off = sy-index
                            len = 1 ).
      IF substring( val = lv_parity
                    off = sy-index - 1
                    len = 1 ) = `A`.
        result = result && lt_l[ lv_digit + 1 ].
      ELSE.
        result = result && lt_g[ lv_digit + 1 ].
      ENDIF.
    ENDDO.
    result = result && `01010`.
    DO 6 TIMES.
      lv_digit = substring( val = lv_text
                            off = 6 + sy-index
                            len = 1 ).
      result = result && lt_r[ lv_digit + 1 ].
    ENDDO.
    result = result && `101`.

  ENDMETHOD.

  METHOD gf_tables.

    DATA lv_value TYPE i VALUE 1.

    CLEAR exp.
    CLEAR log.
    DO 256 TIMES.
      INSERT 0 INTO TABLE log.
    ENDDO.
    DO 255 TIMES.
      INSERT lv_value INTO TABLE exp.
      log[ lv_value + 1 ] = sy-index - 1.
      lv_value = lv_value * 2.
      IF lv_value >= 256.
        lv_value = xor( a = lv_value
                        b = poly ).
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD reed_solomon.

    DATA lt_exp       TYPE ty_t_int.
    DATA lt_log       TYPE ty_t_int.
    DATA lt_generator TYPE ty_t_int.
    DATA lt_next      TYPE ty_t_int.
    DATA lv_factor    TYPE i.
    DATA lv_term      TYPE i.
    DATA lv_index     TYPE i.

    gf_tables( EXPORTING poly = poly
               IMPORTING exp  = lt_exp
                         log  = lt_log ).

    " the generator: the product of (x - a^i), highest power first
    INSERT 1 INTO TABLE lt_generator.
    DO count TIMES.
      DATA(lv_root) = lt_exp[ ( first + sy-index - 1 ) MOD 255 + 1 ].
      CLEAR lt_next.
      LOOP AT lt_generator INTO DATA(lv_coefficient).
        INSERT lv_coefficient INTO TABLE lt_next.
      ENDLOOP.
      INSERT 0 INTO TABLE lt_next.
      LOOP AT lt_generator INTO lv_coefficient.
        lv_index = sy-tabix + 1.
        IF lv_coefficient <> 0.
          lv_term = lt_exp[ ( lt_log[ lv_coefficient + 1 ] + lt_log[ lv_root + 1 ] ) MOD 255 + 1 ].
          lt_next[ lv_index ] = xor( a = lt_next[ lv_index ]
                                     b = lv_term ).
        ENDIF.
      ENDLOOP.
      lt_generator = lt_next.
    ENDDO.

    " the remainder of the data times x^count, divided by the generator
    DO count TIMES.
      INSERT 0 INTO TABLE result.
    ENDDO.
    LOOP AT data INTO DATA(lv_byte).
      lv_factor = xor( a = lv_byte
                       b = result[ 1 ] ).
      DELETE result INDEX 1.
      INSERT 0 INTO TABLE result.
      IF lv_factor = 0.
        CONTINUE.
      ENDIF.
      lv_index = 0.
      WHILE lv_index < count.
        lv_coefficient = lt_generator[ lv_index + 2 ].
        IF lv_coefficient <> 0.
          lv_term = lt_exp[ ( lt_log[ lv_coefficient + 1 ] + lt_log[ lv_factor + 1 ] ) MOD 255 + 1 ].
          result[ lv_index + 1 ] = xor( a = result[ lv_index + 1 ]
                                        b = lv_term ).
        ENDIF.
        lv_index = lv_index + 1.
      ENDWHILE.
    ENDLOOP.

  ENDMETHOD.

  METHOD set.

    DATA(lv_index) = y * matrix-size + x + 1.
    matrix-t_module[ lv_index ] = COND #( WHEN val = abap_true THEN 1 ELSE 0 ).
    IF fixed = abap_true.
      matrix-t_fixed[ lv_index ] = 1.
    ENDIF.

  ENDMETHOD.

  METHOD get.

    result = matrix-t_module[ y * matrix-size + x + 1 ].

  ENDMETHOD.

  METHOD rows_of.

    DATA lv_row TYPE string.
    DATA lv_y   TYPE i.
    DATA lv_x   TYPE i.

    WHILE lv_y < matrix-size.
      CLEAR lv_row.
      lv_x = 0.
      WHILE lv_x < matrix-size.
        lv_row = lv_row && |{ matrix-t_module[ lv_y * matrix-size + lv_x + 1 ] }|.
        lv_x = lv_x + 1.
      ENDWHILE.
      INSERT lv_row INTO TABLE result.
      lv_y = lv_y + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD qr_blocks.

    DATA lv_table TYPE string.

    CLEAR t_count.
    CLEAR t_total.
    CLEAR t_data.
    lv_table = `1L:1,26,19 1M:1,26,16 1Q:1,26,13 1H:1,26,9 2L:1,44,34 2M:1,44,28 2Q:1,44,22 2H:1,44,16 3L:` &&
      `1,70,55 3M:1,70,44 3Q:2,35,17 3H:2,35,13 4L:1,100,80 4M:2,50,32 4Q:2,50,24 4H:4,25,9 5L:1,` &&
      `134,108 5M:2,67,43 5Q:2,33,15;2,34,16 5H:2,33,11;2,34,12 6L:2,86,68 6M:4,43,27 6Q:4,43,19 ` &&
      `6H:4,43,15 7L:2,98,78 7M:4,49,31 7Q:2,32,14;4,33,15 7H:4,39,13;1,40,14 8L:2,121,97 8M:2,60` &&
      `,38;2,61,39 8Q:4,40,18;2,41,19 8H:4,40,14;2,41,15 9L:2,146,116 9M:3,58,36;2,59,37 9Q:4,36,` &&
      `16;4,37,17 9H:4,36,12;4,37,13 10L:2,86,68;2,87,69 10M:4,69,43;1,70,44 10Q:6,43,19;2,44,20 ` &&
      `10H:6,43,15;2,44,16 11L:4,101,81 11M:1,80,50;4,81,51 11Q:4,50,22;4,51,23 11H:3,36,12;8,37,` &&
      `13 12L:2,116,92;2,117,93 12M:6,58,36;2,59,37 12Q:4,46,20;6,47,21 12H:7,42,14;4,43,15 13L:4` &&
      `,133,107 13M:8,59,37;1,60,38 13Q:8,44,20;4,45,21 13H:12,33,11;4,34,12 14L:3,145,115;1,146,` &&
      `116 14M:4,64,40;5,65,41 14Q:11,36,16;5,37,17 14H:11,36,12;5,37,13 15L:5,109,87;1,110,88 15` &&
      `M:5,65,41;5,66,42 15Q:5,54,24;7,55,25 15H:11,36,12;7,37,13 16L:5,122,98;1,123,99 16M:7,73,` &&
      `45;3,74,46 16Q:15,43,19;2,44,20 16H:3,45,15;13,46,16 17L:1,135,107;5,136,108 17M:10,74,46;` &&
      `1,75,47 17Q:1,50,22;15,51,23 17H:2,42,14;17,43,15 18L:5,150,120;1,151,121 18M:9,69,43;4,70` &&
      `,44 18Q:17,50,22;1,51,23 18H:2,42,14;19,43,15 19L:3,141,113;4,142,114 19M:3,70,44;11,71,45` &&
      ` 19Q:17,47,21;4,48,22 19H:9,39,13;16,40,14 20L:3,135,107;5,136,108 20M:3,67,41;13,68,42 20` &&
      `Q:15,54,24;5,55,25 20H:15,43,15;10,44,16 21L:4,144,116;4,145,117 21M:17,68,42 21Q:17,50,22` &&
      `;6,51,23 21H:19,46,16;6,47,17 22L:2,139,111;7,140,112 22M:17,74,46 22Q:7,54,24;16,55,25 22` &&
      `H:34,37,13 23L:4,151,121;5,152,122 23M:4,75,47;14,76,48 23Q:11,54,24;14,55,25 23H:16,45,15` &&
      `;14,46,16 24L:6,147,117;4,148,118 24M:6,73,45;14,74,46 24Q:11,54,24;16,55,25 24H:30,46,16;` &&
      `2,47,17 25L:8,132,106;4,133,107 25M:8,75,47;13,76,48 25Q:7,54,24;22,55,25 25H:22,45,15;13,` &&
      `46,16 26L:10,142,114;2,143,115 26M:19,74,46;4,75,47 26Q:28,50,22;6,51,23 26H:33,46,16;4,47` &&
      `,17 27L:8,152,122;4,153,123 27M:22,73,45;3,74,46 27Q:8,53,23;26,54,24 27H:12,45,15;28,46,1` &&
      `6 28L:3,147,117;10,148,118 28M:3,73,45;23,74,46 28Q:4,54,24;31,55,25 28H:11,45,15;31,46,16` &&
      ` 29L:7,146,116;7,147,117 29M:21,73,45;7,74,46 29Q:1,53,23;37,54,24 29H:19,45,15;26,46,16 3` &&
      `0L:5,145,115;10,146,116 30M:19,75,47;10,76,48 30Q:15,54,24;25,55,25 30H:23,45,15;25,46,16 ` &&
      `31L:13,145,115;3,146,116 31M:2,74,46;29,75,47 31Q:42,54,24;1,55,25 31H:23,45,15;28,46,16 3` &&
      `2L:17,145,115 32M:10,74,46;23,75,47 32Q:10,54,24;35,55,25 32H:19,45,15;35,46,16 33L:17,145` &&
      `,115;1,146,116 33M:14,74,46;21,75,47 33Q:29,54,24;19,55,25 33H:11,45,15;46,46,16 34L:13,14` &&
      `5,115;6,146,116 34M:14,74,46;23,75,47 34Q:44,54,24;7,55,25 34H:59,46,16;1,47,17 35L:12,151` &&
      `,121;7,152,122 35M:12,75,47;26,76,48 35Q:39,54,24;14,55,25 35H:22,45,15;41,46,16 36L:6,151` &&
      `,121;14,152,122 36M:6,75,47;34,76,48 36Q:46,54,24;10,55,25 36H:2,45,15;64,46,16 37L:17,152` &&
      `,122;4,153,123 37M:29,74,46;14,75,47 37Q:49,54,24;10,55,25 37H:24,45,15;46,46,16 38L:4,152` &&
      `,122;18,153,123 38M:13,74,46;32,75,47 38Q:48,54,24;14,55,25 38H:42,45,15;32,46,16 39L:20,1` &&
      `47,117;4,148,118 39M:40,75,47;7,76,48 39Q:43,54,24;22,55,25 39H:10,45,15;67,46,16 40L:19,1` &&
      `48,118;6,149,119 40M:18,75,47;31,76,48 40Q:34,54,24;34,55,25 40H:20,45,15;61,46,16`.
    SPLIT lv_table AT ` ` INTO TABLE DATA(lt_entry).
    DATA(lv_key) = |{ version }{ ecc }:|.
    LOOP AT lt_entry INTO DATA(lv_entry).
      IF strlen( lv_entry ) <= strlen( lv_key ) OR substring( val = lv_entry
                                                             len = strlen( lv_key ) ) <> lv_key.
        CONTINUE.
      ENDIF.
      SPLIT substring( val = lv_entry
                       off = strlen( lv_key ) ) AT `;` INTO TABLE DATA(lt_group).
      LOOP AT lt_group INTO DATA(lv_group).
        SPLIT lv_group AT `,` INTO DATA(lv_count) DATA(lv_total) DATA(lv_data).
        INSERT CONV i( lv_count ) INTO TABLE t_count.
        INSERT CONV i( lv_total ) INTO TABLE t_total.
        INSERT CONV i( lv_data ) INTO TABLE t_data.
      ENDLOOP.
      RETURN.
    ENDLOOP.

  ENDMETHOD.

  METHOD qr_function.

    DATA lv_i     TYPE i.
    DATA lv_dx    TYPE i.
    DATA lv_dy    TYPE i.
    DATA lv_dist  TYPE i.
    DATA lv_x     TYPE i.
    DATA lv_y     TYPE i.
    DATA lv_table TYPE string.
    DATA lv_rem   TYPE i.
    DATA lv_bits  TYPE i.

    DATA(lv_size) = matrix-size.

    " the timing patterns
    lv_i = 0.
    WHILE lv_i < lv_size.
      set( EXPORTING x = 6
                     y = lv_i
                     val = xsdbool( lv_i MOD 2 = 0 )
           CHANGING matrix = matrix ).
      set( EXPORTING x = lv_i
                     y = 6
                     val = xsdbool( lv_i MOD 2 = 0 )
           CHANGING matrix = matrix ).
      lv_i = lv_i + 1.
    ENDWHILE.

    " the three finder patterns with their separators
    DATA(lt_center) = VALUE ty_t_int( ( 3 ) ( 3 ) ( lv_size - 4 ) ( 3 ) ( 3 ) ( lv_size - 4 ) ).
    lv_i = 1.
    WHILE lv_i < 6.
      lv_dy = -4.
      WHILE lv_dy <= 4.
        lv_dx = -4.
        WHILE lv_dx <= 4.
          lv_dist = nmax( val1 = abs( lv_dx )
                          val2 = abs( lv_dy ) ).
          lv_x = lt_center[ lv_i ] + lv_dx.
          lv_y = lt_center[ lv_i + 1 ] + lv_dy.
          IF lv_x >= 0 AND lv_x < lv_size AND lv_y >= 0 AND lv_y < lv_size.
            set( EXPORTING x = lv_x
                           y = lv_y
                           val = xsdbool( lv_dist <> 2 AND lv_dist <> 4 )
                 CHANGING matrix = matrix ).
          ENDIF.
          lv_dx = lv_dx + 1.
        ENDWHILE.
        lv_dy = lv_dy + 1.
      ENDWHILE.
      lv_i = lv_i + 2.
    ENDWHILE.

    " the alignment patterns, not over the finder patterns
    IF version >= 2.
      lv_table = `6,18 6,22 6,26 6,30 6,34 6,22,38 6,24,42 6,26,46 6,28,50 6,30,54 6,32,58 6,34,62 6,26,46,6` &&
      `6 6,26,48,70 6,26,50,74 6,30,54,78 6,30,56,82 6,30,58,86 6,34,62,90 6,28,50,72,94 6,26,50,` &&
      `74,98 6,30,54,78,102 6,28,54,80,106 6,32,58,84,110 6,30,58,86,114 6,34,62,90,118 6,26,50,7` &&
      `4,98,122 6,30,54,78,102,126 6,26,52,78,104,130 6,30,56,82,108,134 6,34,60,86,112,138 6,30,` &&
      `58,86,114,142 6,34,62,90,118,146 6,30,54,78,102,126,150 6,24,50,76,102,128,154 6,28,54,80,` &&
      `106,132,158 6,32,58,84,110,136,162 6,26,54,82,110,138,166 6,30,58,86,114,142,170`.
      SPLIT lv_table AT ` ` INTO TABLE DATA(lt_version).
      SPLIT lt_version[ version - 1 ] AT `,` INTO TABLE DATA(lt_position).
      DATA(lv_count) = lines( lt_position ).
      LOOP AT lt_position INTO DATA(lv_row_text).
        DATA(lv_row_index) = sy-tabix.
        LOOP AT lt_position INTO DATA(lv_col_text).
          DATA(lv_col_index) = sy-tabix.
          IF ( lv_row_index = 1 AND lv_col_index = 1 ) OR ( lv_row_index = 1 AND lv_col_index = lv_count )
              OR ( lv_row_index = lv_count AND lv_col_index = 1 ).
            CONTINUE.
          ENDIF.
          lv_dy = -2.
          WHILE lv_dy <= 2.
            lv_dx = -2.
            WHILE lv_dx <= 2.
              set( EXPORTING x = CONV i( lv_col_text ) + lv_dx
                             y = CONV i( lv_row_text ) + lv_dy
                             val = xsdbool( nmax( val1 = abs( lv_dx )
                                                  val2 = abs( lv_dy ) ) <> 1 )
                   CHANGING matrix = matrix ).
              lv_dx = lv_dx + 1.
            ENDWHILE.
            lv_dy = lv_dy + 1.
          ENDWHILE.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    " the room of the format information, and the dark module
    qr_format( EXPORTING ecc    = `M`
                         mask   = 0
               CHANGING  matrix = matrix ).

    " the version information from version 7 on: 6 bits and 12 of BCH code
    IF version >= 7.
      lv_rem = version.
      DO 12 TIMES.
        lv_rem = xor( a = lv_rem * 2
                      b = ( lv_rem DIV 2048 ) * 7973 ).
      ENDDO.
      lv_bits = version * 4096 + lv_rem.
      lv_i = 0.
      WHILE lv_i < 18.
        DATA(lv_bit) = xsdbool( ( lv_bits DIV ipow( base = 2
                                                    exp  = lv_i ) ) MOD 2 = 1 ).
        set( EXPORTING x = lv_size - 11 + lv_i MOD 3
                       y = lv_i DIV 3
                       val = lv_bit
             CHANGING matrix = matrix ).
        set( EXPORTING x = lv_i DIV 3
                       y = lv_size - 11 + lv_i MOD 3
                       val = lv_bit
             CHANGING matrix = matrix ).
        lv_i = lv_i + 1.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.

  METHOD qr_format.

    DATA lv_rem  TYPE i.
    DATA lv_bits TYPE i.
    DATA lv_i    TYPE i.
    DATA lv_bit  TYPE abap_bool.

    DATA(lv_size) = matrix-size.

    " 2 bits of the error correction, 3 of the mask, 10 of BCH code
    DATA(lv_data) = COND i( WHEN ecc = `L` THEN 1 WHEN ecc = `M` THEN 0 WHEN ecc = `Q` THEN 3 ELSE 2 ).
    lv_data = lv_data * 8 + mask.
    lv_rem = lv_data.
    DO 10 TIMES.
      lv_rem = xor( a = lv_rem * 2
                    b = ( lv_rem DIV 512 ) * 1335 ).
    ENDDO.
    lv_bits = xor( a = lv_data * 1024 + lv_rem
                   b = 21522 ).

    WHILE lv_i < 15.
      lv_bit = xsdbool( ( lv_bits DIV ipow( base = 2
                                            exp  = lv_i ) ) MOD 2 = 1 ).
      " beside the finder pattern at the top left
      IF lv_i <= 5.
        set( EXPORTING x = 8 y = lv_i val = lv_bit CHANGING matrix = matrix ).
      ELSEIF lv_i = 6.
        set( EXPORTING x = 8 y = 7 val = lv_bit CHANGING matrix = matrix ).
      ELSEIF lv_i = 7.
        set( EXPORTING x = 8 y = 8 val = lv_bit CHANGING matrix = matrix ).
      ELSEIF lv_i = 8.
        set( EXPORTING x = 7 y = 8 val = lv_bit CHANGING matrix = matrix ).
      ELSE.
        set( EXPORTING x = 14 - lv_i y = 8 val = lv_bit CHANGING matrix = matrix ).
      ENDIF.
      " beside the two others
      IF lv_i < 8.
        set( EXPORTING x = lv_size - 1 - lv_i y = 8 val = lv_bit CHANGING matrix = matrix ).
      ELSE.
        set( EXPORTING x = 8 y = lv_size - 15 + lv_i val = lv_bit CHANGING matrix = matrix ).
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
    set( EXPORTING x = 8 y = lv_size - 8 val = abap_true CHANGING matrix = matrix ).

  ENDMETHOD.

  METHOD qr_masked.

    CASE mask.
      WHEN 0.
        result = xsdbool( ( x + y ) MOD 2 = 0 ).
      WHEN 1.
        result = xsdbool( y MOD 2 = 0 ).
      WHEN 2.
        result = xsdbool( x MOD 3 = 0 ).
      WHEN 3.
        result = xsdbool( ( x + y ) MOD 3 = 0 ).
      WHEN 4.
        result = xsdbool( ( x DIV 3 + y DIV 2 ) MOD 2 = 0 ).
      WHEN 5.
        result = xsdbool( ( x * y ) MOD 2 + ( x * y ) MOD 3 = 0 ).
      WHEN 6.
        result = xsdbool( ( ( x * y ) MOD 2 + ( x * y ) MOD 3 ) MOD 2 = 0 ).
      WHEN 7.
        result = xsdbool( ( ( x + y ) MOD 2 + ( x * y ) MOD 3 ) MOD 2 = 0 ).
    ENDCASE.

  ENDMETHOD.

  METHOD qr_penalty.

    DATA lv_size TYPE i.
    DATA lv_a    TYPE i.
    DATA lv_b    TYPE i.
    DATA lv_run  TYPE i.
    DATA lv_last TYPE i.
    DATA lv_dark TYPE i.
    DATA lv_line TYPE string.

    lv_size = matrix-size.

    " rule 1 and 3, along the rows (lv_dir 1) and the columns (2)
    DO 2 TIMES.
      DATA(lv_dir) = sy-index.
      lv_a = 0.
      WHILE lv_a < lv_size.
        lv_run = 0.
        lv_last = -1.
        CLEAR lv_line.
        lv_b = 0.
        WHILE lv_b < lv_size.
          DATA(lv_module) = COND i( WHEN lv_dir = 1 THEN matrix-t_module[ lv_a * lv_size + lv_b + 1 ]
                                    ELSE matrix-t_module[ lv_b * lv_size + lv_a + 1 ] ).
          lv_line = lv_line && |{ lv_module }|.
          IF lv_module = lv_last.
            lv_run = lv_run + 1.
            IF lv_run = 5.
              result = result + 3.
            ELSEIF lv_run > 5.
              result = result + 1.
            ENDIF.
          ELSE.
            lv_run = 1.
            lv_last = lv_module.
          ENDIF.
          lv_b = lv_b + 1.
        ENDWHILE.
        " a finder pattern like 1011101 with four light modules on a side,
        " the quiet zone counting as light
        lv_line = |0000{ lv_line }0000|.
        result = result + 40 * count( val = lv_line
                                      sub = `00001011101` ).
        result = result + 40 * count( val = lv_line
                                      sub = `10111010000` ).
        lv_a = lv_a + 1.
      ENDWHILE.
    ENDDO.

    " rule 2: blocks of 2 x 2 of one colour
    lv_a = 0.
    WHILE lv_a < lv_size - 1.
      lv_b = 0.
      WHILE lv_b < lv_size - 1.
        lv_module = matrix-t_module[ lv_a * lv_size + lv_b + 1 ].
        IF lv_module = matrix-t_module[ lv_a * lv_size + lv_b + 2 ]
            AND lv_module = matrix-t_module[ ( lv_a + 1 ) * lv_size + lv_b + 1 ]
            AND lv_module = matrix-t_module[ ( lv_a + 1 ) * lv_size + lv_b + 2 ].
          result = result + 3.
        ENDIF.
        lv_b = lv_b + 1.
      ENDWHILE.
      lv_a = lv_a + 1.
    ENDWHILE.

    " rule 4: the share of dark modules, away from half of them
    LOOP AT matrix-t_module INTO lv_module.
      lv_dark = lv_dark + lv_module.
    ENDLOOP.
    DATA(lv_total) = lv_size * lv_size.
    result = result + abs( lv_dark * 2 - lv_total ) * 10 DIV lv_total * 10.

  ENDMETHOD.

  METHOD qr.

    DATA lt_count   TYPE ty_t_int.
    DATA lt_total   TYPE ty_t_int.
    DATA lt_data    TYPE ty_t_int.
    DATA lv_version TYPE i.
    DATA lv_capacity TYPE i.
    DATA lv_bits    TYPE string.
    DATA lt_byte    TYPE ty_t_int.
    DATA lv_x1      TYPE x LENGTH 1.
    DATA lv_i       TYPE i.
    DATA lt_block   TYPE STANDARD TABLE OF ty_t_int WITH EMPTY KEY.
    DATA lt_ecc     TYPE STANDARD TABLE OF ty_t_int WITH EMPTY KEY.
    DATA lt_final   TYPE ty_t_int.
    DATA ls_matrix  TYPE ty_s_matrix.
    DATA ls_best    TYPE ty_s_matrix.
    DATA lv_best    TYPE i.

    DATA(lv_ecc) = to_upper( condense( ecc ) ).
    IF lv_ecc <> `L` AND lv_ecc <> `M` AND lv_ecc <> `Q` AND lv_ecc <> `H`.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: { ecc } is no error correction of a QR code (L, M, Q, H)|.
    ENDIF.

    " the text as bytes of UTF-8
    DATA(lv_bytes) = zabaputil_cl_util_context=>conv_get_xstring_by_string( CONV string( val ) ).
    DATA(lv_length) = xstrlen( lv_bytes ).

    " the smallest version the byte mode fits into
    lv_version = 1.
    DO.
      IF lv_version > 40.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |PDF: the text is too long for a QR code ({ lv_length } bytes)|.
      ENDIF.
      qr_blocks( EXPORTING version = lv_version
                           ecc     = lv_ecc
                 IMPORTING t_count = lt_count
                           t_total = lt_total
                           t_data  = lt_data ).
      lv_capacity = 0.
      LOOP AT lt_count INTO DATA(lv_count).
        lv_capacity = lv_capacity + lv_count * lt_data[ sy-tabix ].
      ENDLOOP.
      DATA(lv_count_bits) = COND i( WHEN lv_version <= 9 THEN 8 ELSE 16 ).
      IF 4 + lv_count_bits + lv_length * 8 <= lv_capacity * 8.
        EXIT.
      ENDIF.
      lv_version = lv_version + 1.
    ENDDO.

    " mode byte, the count, the bytes, at most four bits of terminator
    lv_bits = `0100`.
    DATA(lv_count_value) = lv_length.
    DATA lv_count_text TYPE string.
    DO lv_count_bits TIMES.
      lv_count_text = |{ lv_count_value MOD 2 }{ lv_count_text }|.
      lv_count_value = lv_count_value DIV 2.
    ENDDO.
    lv_bits = lv_bits && lv_count_text.
    lv_i = 0.
    WHILE lv_i < lv_length.
      lv_x1 = lv_bytes+lv_i(1).
      DATA(lv_byte) = CONV i( lv_x1 ).
      DATA lv_byte_text TYPE string.
      CLEAR lv_byte_text.
      DO 8 TIMES.
        lv_byte_text = |{ lv_byte MOD 2 }{ lv_byte_text }|.
        lv_byte = lv_byte DIV 2.
      ENDDO.
      lv_bits = lv_bits && lv_byte_text.
      lv_i = lv_i + 1.
    ENDWHILE.
    DO 4 TIMES.
      IF strlen( lv_bits ) < lv_capacity * 8.
        lv_bits = lv_bits && `0`.
      ENDIF.
    ENDDO.
    WHILE strlen( lv_bits ) MOD 8 <> 0.
      lv_bits = lv_bits && `0`.
    ENDWHILE.
    lv_i = 0.
    WHILE lv_i < strlen( lv_bits ).
      DATA(lv_value) = 0.
      DO 8 TIMES.
        lv_value = lv_value * 2 + COND i( WHEN substring( val = lv_bits
                                                          off = lv_i + sy-index - 1
                                                          len = 1 ) = `1` THEN 1 ELSE 0 ).
      ENDDO.
      INSERT lv_value INTO TABLE lt_byte.
      lv_i = lv_i + 8.
    ENDWHILE.
    " the rest of the room: EC and 11 by turns, EC first
    DATA(lv_pad) = 236.
    WHILE lines( lt_byte ) < lv_capacity.
      INSERT lv_pad INTO TABLE lt_byte.
      lv_pad = 253 - lv_pad.
    ENDWHILE.
    " the bytes in blocks, each with its error correction
    DATA(lv_offset) = 0.
    LOOP AT lt_count INTO lv_count.
      DATA(lv_group) = sy-tabix.
      DO lv_count TIMES.
        DATA lt_one TYPE ty_t_int.
        CLEAR lt_one.
        DO lt_data[ lv_group ] TIMES.
          INSERT lt_byte[ lv_offset + sy-index ] INTO TABLE lt_one.
        ENDDO.
        lv_offset = lv_offset + lt_data[ lv_group ].
        INSERT lt_one INTO TABLE lt_block.
        INSERT reed_solomon( data  = lt_one
                             count = lt_total[ lv_group ] - lt_data[ lv_group ]
                             poly  = 285
                             first = 0 ) INTO TABLE lt_ecc.
      ENDDO.
    ENDLOOP.

    " interleaved: the first byte of every block, then the second, ...
    DATA(lv_most) = 0.
    LOOP AT lt_block INTO lt_one.
      lv_most = nmax( val1 = lv_most
                      val2 = lines( lt_one ) ).
    ENDLOOP.
    lv_i = 1.
    WHILE lv_i <= lv_most.
      LOOP AT lt_block INTO lt_one.
        IF lv_i <= lines( lt_one ).
          INSERT lt_one[ lv_i ] INTO TABLE lt_final.
        ENDIF.
      ENDLOOP.
      lv_i = lv_i + 1.
    ENDWHILE.
    lv_i = 1.
    DATA(lv_ecc_count) = lines( lt_ecc[ 1 ] ).
    WHILE lv_i <= lv_ecc_count.
      LOOP AT lt_ecc INTO lt_one.
        INSERT lt_one[ lv_i ] INTO TABLE lt_final.
      ENDLOOP.
      lv_i = lv_i + 1.
    ENDWHILE.

    " the matrix: the function patterns, then the bits from the bottom
    " right, two columns at a time, up and down by turns
    ls_matrix-size = lv_version * 4 + 17.
    DO ls_matrix-size * ls_matrix-size TIMES.
      INSERT 0 INTO TABLE ls_matrix-t_module.
      INSERT 0 INTO TABLE ls_matrix-t_fixed.
    ENDDO.
    qr_function( EXPORTING version = lv_version
                 CHANGING  matrix  = ls_matrix ).

    DATA(lv_bit_count) = lines( lt_final ) * 8.
    DATA(lv_bit_index) = 0.
    DATA(lv_right) = ls_matrix-size - 1.
    WHILE lv_right >= 1.
      IF lv_right = 6.
        lv_right = 5.
      ENDIF.
      DATA(lv_vert) = 0.
      WHILE lv_vert < ls_matrix-size.
        DO 2 TIMES.
          DATA(lv_x) = lv_right - sy-index + 1.
          DATA(lv_y) = COND i( WHEN ( lv_right + 1 ) MOD 4 < 2 THEN ls_matrix-size - 1 - lv_vert ELSE lv_vert ).
          IF ls_matrix-t_fixed[ lv_y * ls_matrix-size + lv_x + 1 ] = 0 AND lv_bit_index < lv_bit_count.
            lv_value = lt_final[ lv_bit_index DIV 8 + 1 ].
            DATA(lv_dark) = xsdbool( ( lv_value DIV ipow( base = 2
                                                          exp  = 7 - lv_bit_index MOD 8 ) ) MOD 2 = 1 ).
            set( EXPORTING x      = lv_x
                           y      = lv_y
                           val    = lv_dark
                           fixed  = abap_false
                 CHANGING  matrix = ls_matrix ).
            lv_bit_index = lv_bit_index + 1.
          ENDIF.
        ENDDO.
        lv_vert = lv_vert + 1.
      ENDWHILE.
      lv_right = lv_right - 2.
    ENDWHILE.

    " the mask asked for, or the one with the least penalty
    lv_best = -1.
    DATA(lv_mask) = 0.
    WHILE lv_mask < 8.
      IF mask < 0 OR mask = lv_mask.
        DATA(ls_try) = ls_matrix.
        lv_y = 0.
        WHILE lv_y < ls_try-size.
          lv_x = 0.
          WHILE lv_x < ls_try-size.
            DATA(lv_index) = lv_y * ls_try-size + lv_x + 1.
            IF ls_try-t_fixed[ lv_index ] = 0 AND qr_masked( x    = lv_x
                                                             y    = lv_y
                                                             mask = lv_mask ) = abap_true.
              ls_try-t_module[ lv_index ] = 1 - ls_try-t_module[ lv_index ].
            ENDIF.
            lv_x = lv_x + 1.
          ENDWHILE.
          lv_y = lv_y + 1.
        ENDWHILE.
        qr_format( EXPORTING ecc    = lv_ecc
                             mask   = lv_mask
                   CHANGING  matrix = ls_try ).
        DATA(lv_penalty) = qr_penalty( ls_try ).
        IF lv_best < 0 OR lv_penalty < lv_best.
          lv_best = lv_penalty.
          ls_best = ls_try.
        ENDIF.
      ENDIF.
      lv_mask = lv_mask + 1.
    ENDWHILE.

    result = rows_of( ls_best ).

  ENDMETHOD.

  METHOD datamatrix.

    TYPES:
      BEGIN OF ty_s_size,
        size    TYPE i,
        data    TYPE i,
        ecc     TYPE i,
        regions TYPE i,
        blocks  TYPE i,
      END OF ty_s_size.

    DATA lt_size     TYPE STANDARD TABLE OF ty_s_size WITH EMPTY KEY.
    DATA ls_size     TYPE ty_s_size.
    DATA lt_word     TYPE ty_t_int.
    DATA lv_pos      TYPE i.
    DATA lt_final    TYPE ty_t_int.
    DATA lv_i        TYPE i.
    DATA lv_row      TYPE string.

    " square symbols: size, data and error correction codewords, data
    " regions along a side, interleaved blocks
    lt_size = VALUE #( ( size = 10 data = 3 ecc = 5 regions = 1 blocks = 1 )
                       ( size = 12 data = 5 ecc = 7 regions = 1 blocks = 1 )
                       ( size = 14 data = 8 ecc = 10 regions = 1 blocks = 1 )
                       ( size = 16 data = 12 ecc = 12 regions = 1 blocks = 1 )
                       ( size = 18 data = 18 ecc = 14 regions = 1 blocks = 1 )
                       ( size = 20 data = 22 ecc = 18 regions = 1 blocks = 1 )
                       ( size = 22 data = 30 ecc = 20 regions = 1 blocks = 1 )
                       ( size = 24 data = 36 ecc = 24 regions = 1 blocks = 1 )
                       ( size = 26 data = 44 ecc = 28 regions = 1 blocks = 1 )
                       ( size = 32 data = 62 ecc = 36 regions = 2 blocks = 1 )
                       ( size = 36 data = 86 ecc = 42 regions = 2 blocks = 1 )
                       ( size = 40 data = 114 ecc = 48 regions = 2 blocks = 1 )
                       ( size = 44 data = 144 ecc = 56 regions = 2 blocks = 1 )
                       ( size = 48 data = 174 ecc = 68 regions = 2 blocks = 1 )
                       ( size = 52 data = 204 ecc = 84 regions = 2 blocks = 2 )
                       ( size = 64 data = 280 ecc = 112 regions = 4 blocks = 2 )
                       ( size = 72 data = 368 ecc = 144 regions = 4 blocks = 4 )
                       ( size = 80 data = 456 ecc = 192 regions = 4 blocks = 4 )
                       ( size = 88 data = 576 ecc = 224 regions = 4 blocks = 4 )
                       ( size = 96 data = 696 ecc = 272 regions = 4 blocks = 4 )
                       ( size = 104 data = 816 ecc = 336 regions = 4 blocks = 6 )
                       ( size = 120 data = 1050 ecc = 408 regions = 6 blocks = 6 )
                       ( size = 132 data = 1304 ecc = 496 regions = 6 blocks = 8 )
                       ( size = 144 data = 1558 ecc = 620 regions = 6 blocks = 10 ) ).

    " ASCII encodation: two digits in one codeword, a character above 127
    " after an upper shift
    DATA(lt_cp) = zabaputil_cl_util_context=>c_get_codepoints( CONV string( val ) ).
    WHILE lv_pos < lines( lt_cp ).
      DATA(lv_cp) = lt_cp[ lv_pos + 1 ].
      IF lv_cp >= 48 AND lv_cp <= 57 AND lv_pos + 1 < lines( lt_cp ).
        DATA(lv_next) = lt_cp[ lv_pos + 2 ].
        IF lv_next >= 48 AND lv_next <= 57.
          INSERT 130 + ( lv_cp - 48 ) * 10 + lv_next - 48 INTO TABLE lt_word.
          lv_pos = lv_pos + 2.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF lv_cp > 255.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = `PDF: a Data Matrix takes characters up to U+00FF`.
      ELSEIF lv_cp > 127.
        INSERT 235 INTO TABLE lt_word.
        INSERT lv_cp - 127 INTO TABLE lt_word.
      ELSE.
        INSERT lv_cp + 1 INTO TABLE lt_word.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    " the smallest size it fits into
    LOOP AT lt_size INTO ls_size.
      IF ls_size-data >= lines( lt_word ).
        EXIT.
      ENDIF.
    ENDLOOP.
    IF ls_size-data < lines( lt_word ).
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `PDF: the text is too long for a Data Matrix`.
    ENDIF.

    " the padding: 129, then values that look random (253-state)
    IF lines( lt_word ) < ls_size-data.
      INSERT 129 INTO TABLE lt_word.
    ENDIF.
    WHILE lines( lt_word ) < ls_size-data.
      DATA(lv_pad) = 129 + ( 149 * ( lines( lt_word ) + 1 ) ) MOD 253 + 1.
      IF lv_pad > 254.
        lv_pad = lv_pad - 254.
      ENDIF.
      INSERT lv_pad INTO TABLE lt_word.
    ENDWHILE.

    " error correction per block; codeword i goes to block i mod blocks
    DATA lt_block TYPE STANDARD TABLE OF ty_t_int WITH EMPTY KEY.
    DATA lt_one   TYPE ty_t_int.
    DO ls_size-blocks TIMES.
      INSERT VALUE ty_t_int( ) INTO TABLE lt_block.
    ENDDO.
    LOOP AT lt_word INTO DATA(lv_word).
      ASSIGN lt_block[ ( sy-tabix - 1 ) MOD ls_size-blocks + 1 ] TO FIELD-SYMBOL(<block>).
      INSERT lv_word INTO TABLE <block>.
    ENDLOOP.
    lt_final = lt_word.
    DO ls_size-ecc TIMES.
      INSERT 0 INTO TABLE lt_final.
    ENDDO.
    DATA(lv_per_block) = ls_size-ecc DIV ls_size-blocks.
    LOOP AT lt_block INTO lt_one.
      DATA(lv_block) = sy-tabix.
      DATA(lt_ecc) = reed_solomon( data  = lt_one
                                   count = lv_per_block
                                   poly  = 301
                                   first = 1 ).
      LOOP AT lt_ecc INTO DATA(lv_ecc).
        lt_final[ ls_size-data + ( sy-tabix - 1 ) * ls_size-blocks + lv_block ] = lv_ecc.
      ENDLOOP.
    ENDLOOP.

    " the codewords placed in the data regions as one matrix, then the
    " regions with their finder patterns
    DATA(lv_region) = ( ls_size-size - 2 * ls_size-regions ) DIV ls_size-regions.
    DATA(lv_inner) = lv_region * ls_size-regions.
    DATA(lt_module) = dm_place( rows      = lv_inner
                                cols      = lv_inner
                                codewords = lt_final ).

    DATA(lv_y) = 0.
    WHILE lv_y < ls_size-size.
      CLEAR lv_row.
      DATA(lv_ry) = lv_y MOD ( lv_region + 2 ).
      DATA(lv_x) = 0.
      WHILE lv_x < ls_size-size.
        DATA(lv_rx) = lv_x MOD ( lv_region + 2 ).
        IF lv_ry = lv_region + 1.
          " the bottom edge of a region: solid
          lv_row = lv_row && `1`.
        ELSEIF lv_rx = 0.
          " the left edge: solid
          lv_row = lv_row && `1`.
        ELSEIF lv_ry = 0.
          " the top edge: dark and light by turns
          lv_row = lv_row && COND string( WHEN lv_rx MOD 2 = 0 THEN `1` ELSE `0` ).
        ELSEIF lv_rx = lv_region + 1.
          " the right edge: dark and light by turns
          lv_row = lv_row && COND string( WHEN lv_ry MOD 2 = 1 THEN `1` ELSE `0` ).
        ELSE.
          lv_i = ( ( lv_y DIV ( lv_region + 2 ) ) * lv_region + lv_ry - 1 ) * lv_inner
                 + ( lv_x DIV ( lv_region + 2 ) ) * lv_region + lv_rx - 1.
          lv_row = lv_row && |{ lt_module[ lv_i + 1 ] }|.
        ENDIF.
        lv_x = lv_x + 1.
      ENDWHILE.
      INSERT lv_row INTO TABLE result.
      lv_y = lv_y + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD dm_module.

    DATA lv_row TYPE i.
    DATA lv_col TYPE i.

    lv_row = row.
    lv_col = col.
    IF lv_row < 0.
      lv_row = lv_row + rows.
      lv_col = lv_col + 4 - ( rows + 4 ) MOD 8.
    ENDIF.
    IF lv_col < 0.
      lv_col = lv_col + cols.
      lv_row = lv_row + 4 - ( cols + 4 ) MOD 8.
    ENDIF.
    " 2 for a light module that is placed, 1 for a dark one
    DATA(lv_value) = 0.
    IF codeword <= lines( codewords ).
      lv_value = codewords[ codeword ].
    ENDIF.
    DATA(lv_dark) = ( lv_value DIV ipow( base = 2
                                         exp  = 8 - bit ) ) MOD 2.
    modules[ lv_row * cols + lv_col + 1 ] = COND #( WHEN lv_dark = 1 THEN 1 ELSE 2 ).

  ENDMETHOD.

  METHOD dm_place.

    DATA lv_pos TYPE i VALUE 1.
    DATA lv_row TYPE i VALUE 4.
    DATA lv_col TYPE i.

    DO rows * cols TIMES.
      INSERT 0 INTO TABLE result.
    ENDDO.

    DO.
      " the four corner cases
      IF lv_row = rows AND lv_col = 0.
        dm_module( EXPORTING row = rows - 1 col = 0 codeword = lv_pos bit = 1 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = rows - 1 col = 1 codeword = lv_pos bit = 2 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = rows - 1 col = 2 codeword = lv_pos bit = 3 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 2 codeword = lv_pos bit = 4 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 1 codeword = lv_pos bit = 5 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 1 col = cols - 1 codeword = lv_pos bit = 6 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 2 col = cols - 1 codeword = lv_pos bit = 7 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 3 col = cols - 1 codeword = lv_pos bit = 8 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        lv_pos = lv_pos + 1.
      ENDIF.
      IF lv_row = rows - 2 AND lv_col = 0 AND cols MOD 4 <> 0.
        dm_module( EXPORTING row = rows - 3 col = 0 codeword = lv_pos bit = 1 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = rows - 2 col = 0 codeword = lv_pos bit = 2 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = rows - 1 col = 0 codeword = lv_pos bit = 3 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 4 codeword = lv_pos bit = 4 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 3 codeword = lv_pos bit = 5 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 2 codeword = lv_pos bit = 6 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 1 codeword = lv_pos bit = 7 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 1 col = cols - 1 codeword = lv_pos bit = 8 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        lv_pos = lv_pos + 1.
      ENDIF.
      IF lv_row = rows - 2 AND lv_col = 0 AND cols MOD 8 = 4.
        dm_module( EXPORTING row = rows - 3 col = 0 codeword = lv_pos bit = 1 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = rows - 2 col = 0 codeword = lv_pos bit = 2 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = rows - 1 col = 0 codeword = lv_pos bit = 3 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 2 codeword = lv_pos bit = 4 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 1 codeword = lv_pos bit = 5 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 1 col = cols - 1 codeword = lv_pos bit = 6 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 2 col = cols - 1 codeword = lv_pos bit = 7 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 3 col = cols - 1 codeword = lv_pos bit = 8 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        lv_pos = lv_pos + 1.
      ENDIF.
      IF lv_row = rows + 4 AND lv_col = 2 AND cols MOD 8 = 0.
        dm_module( EXPORTING row = rows - 1 col = 0 codeword = lv_pos bit = 1 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = rows - 1 col = cols - 1 codeword = lv_pos bit = 2 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 3 codeword = lv_pos bit = 3 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 2 codeword = lv_pos bit = 4 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 0 col = cols - 1 codeword = lv_pos bit = 5 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 1 col = cols - 3 codeword = lv_pos bit = 6 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 1 col = cols - 2 codeword = lv_pos bit = 7 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        dm_module( EXPORTING row = 1 col = cols - 1 codeword = lv_pos bit = 8 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
        lv_pos = lv_pos + 1.
      ENDIF.

      " up and to the right
      DO.
        IF lv_row < rows AND lv_col >= 0 AND result[ lv_row * cols + lv_col + 1 ] = 0.
          dm_module( EXPORTING row = lv_row - 2 col = lv_col - 2 codeword = lv_pos bit = 1 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 2 col = lv_col - 1 codeword = lv_pos bit = 2 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 1 col = lv_col - 2 codeword = lv_pos bit = 3 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 1 col = lv_col - 1 codeword = lv_pos bit = 4 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 1 col = lv_col codeword = lv_pos bit = 5 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row col = lv_col - 2 codeword = lv_pos bit = 6 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row col = lv_col - 1 codeword = lv_pos bit = 7 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row col = lv_col codeword = lv_pos bit = 8 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          lv_pos = lv_pos + 1.
        ENDIF.
        lv_row = lv_row - 2.
        lv_col = lv_col + 2.
        IF lv_row < 0 OR lv_col >= cols.
          EXIT.
        ENDIF.
      ENDDO.
      lv_row = lv_row + 1.
      lv_col = lv_col + 3.

      " down and to the left
      DO.
        IF lv_row >= 0 AND lv_col < cols AND result[ lv_row * cols + lv_col + 1 ] = 0.
          dm_module( EXPORTING row = lv_row - 2 col = lv_col - 2 codeword = lv_pos bit = 1 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 2 col = lv_col - 1 codeword = lv_pos bit = 2 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 1 col = lv_col - 2 codeword = lv_pos bit = 3 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 1 col = lv_col - 1 codeword = lv_pos bit = 4 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row - 1 col = lv_col codeword = lv_pos bit = 5 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row col = lv_col - 2 codeword = lv_pos bit = 6 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row col = lv_col - 1 codeword = lv_pos bit = 7 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          dm_module( EXPORTING row = lv_row col = lv_col codeword = lv_pos bit = 8 rows = rows cols = cols codewords = codewords CHANGING modules = result ).
          lv_pos = lv_pos + 1.
        ENDIF.
        lv_row = lv_row + 2.
        lv_col = lv_col - 2.
        IF lv_row >= rows OR lv_col < 0.
          EXIT.
        ENDIF.
      ENDDO.
      lv_row = lv_row + 3.
      lv_col = lv_col + 1.

      IF lv_row >= rows AND lv_col >= cols.
        EXIT.
      ENDIF.
    ENDDO.

    " a corner that no codeword reached: a fixed pattern
    IF result[ rows * cols ] = 0.
      result[ rows * cols ] = 1.
      result[ ( rows - 2 ) * cols + cols - 1 ] = 1.
    ENDIF.

    " 1 dark, everything else light
    LOOP AT result ASSIGNING FIELD-SYMBOL(<module>).
      IF <module> <> 1.
        <module> = 0.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD modules.

    DATA(lv_type) = to_upper( condense( type ) ).
    IF lv_type = cs_type-code128.
      INSERT code128( val ) INTO TABLE result.
    ELSEIF lv_type = cs_type-ean13.
      INSERT ean13( val ) INTO TABLE result.
    ELSEIF lv_type = cs_type-qr.
      result = qr( val ).
    ELSEIF lv_type = cs_type-datamatrix.
      result = datamatrix( val ).
    ELSE.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = |PDF: { type } is no barcode type (CODE128, EAN13, QR, DATAMATRIX)|.
    ENDIF.

  ENDMETHOD.

  METHOD draw.

    DATA lv_module TYPE f.
    DATA lv_x      TYPE f.
    DATA lv_y      TYPE f.
    DATA lv_w      TYPE f.
    DATA lv_h      TYPE f.
    DATA lv_start  TYPE i.
    DATA lv_off    TYPE i.

    DATA(lt_row) = modules( type = type
                            val  = val ).
    IF lt_row IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_cols) = strlen( lt_row[ 1 ] ).
    DATA(lv_rows) = lines( lt_row ).

    lv_w = w.
    lv_h = h.
    page->save( ).
    page->set_fill_color( `000000` ).

    " a linear code fills the box; the modules of a matrix code are square
    lv_module = lv_w / lv_cols.
    DATA(lv_height) = lv_h.
    IF lv_rows > 1.
      lv_module = nmin( val1 = lv_w / lv_cols
                        val2 = lv_h / lv_rows ).
      lv_height = lv_module.
    ENDIF.

    " a run of dark modules in a row is one rectangle
    LOOP AT lt_row INTO DATA(lv_row).
      lv_y = y.
      lv_y = lv_y + ( sy-tabix - 1 ) * lv_height.
      lv_off = 0.
      WHILE lv_off < lv_cols.
        IF lv_row+lv_off(1) = `1`.
          lv_start = lv_off.
          WHILE lv_off < lv_cols AND lv_row+lv_off(1) = `1`.
            lv_off = lv_off + 1.
          ENDWHILE.
          lv_x = x.
          lv_x = lv_x + lv_start * lv_module.
          page->rect( x     = lv_x
                      y     = lv_y
                      w     = ( lv_off - lv_start ) * lv_module
                      h     = lv_height
                      style = `F` ).
        ELSE.
          lv_off = lv_off + 1.
        ENDIF.
      ENDWHILE.
    ENDLOOP.

    page->restore( ).

  ENDMETHOD.

ENDCLASS.

CLASS zabaputil_cl_pdf_barcode DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Code 128, the barcode of logistics: delivery notes, pick lists, storage
    " bin labels, the SSCC of a pallet. Every scanner reads it, and it takes
    " every printable ASCII character - digits at half the width.
    "
    " A text in the GS1 notation - application identifiers in parentheses,
    " (00)340123450000000018 or (02)04012345000009(37)24(10)LOT42 - is
    " written as GS1-128: FNC1 behind the start character, the parentheses
    " left out, and FNC1 behind every value of a variable length that is not
    " the last. The scanner hands over ]C1 0204012345000009 3724 <GS> 10LOT42.
    " Which identifiers there are and which have a fixed length is what
    " zabaputil_cl_util_context=>barcode_gs1_build( ) knows - the same that
    " barcode_gs1_parse( ) reads a scan with.
    "
    " The modules are drawn as bars, nothing else: the quiet zone of ten
    " modules left and right of the symbol is the caller's to leave free.

    "! The modules of the symbol: `1` a bar module, `0` a space module, from
    "! the start character to the end of the stop character. Code set C for
    "! runs of digits, B for the rest. Raises for a character that Code 128
    "! B does not have (outside of ASCII 32 to 126) and for an empty text
    "! @parameter val | the text, or GS1 data in parentheses
    CLASS-METHODS code128
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    "! The bars of the modules, filled in the fill colour the page has
    "! @parameter page    | the page
    "! @parameter modules | what code128( ) returns
    "! @parameter x       | the left edge
    "! @parameter y       | the top edge
    "! @parameter w       | the width of the symbol
    "! @parameter h       | the height of the bars
    CLASS-METHODS draw
      IMPORTING
        page    TYPE REF TO zabaputil_cl_pdf_page
        modules TYPE string
        x       TYPE numeric
        y       TYPE numeric
        w       TYPE numeric
        h       TYPE numeric.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_fnc1 TYPE string VALUE `FNC1`.
    "! the characters of Code 128 B, by their value: ASCII 32 to 126
    CONSTANTS c_ascii TYPE string VALUE ` !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_``abcdefghijklmnopqrstuvwxyz{|}~`.
    CONSTANTS:
      BEGIN OF cs_code,
        shift_c TYPE i VALUE 99,
        shift_b TYPE i VALUE 100,
        fnc1    TYPE i VALUE 102,
        start_b TYPE i VALUE 104,
        start_c TYPE i VALUE 105,
        stop    TYPE i VALUE 106,
      END OF cs_code.

    "! The widths of bar, space, bar ... of a symbol value
    CLASS-METHODS pattern
      IMPORTING
        val           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    "! The text as symbols: one character each, or FNC1
    CLASS-METHODS symbols
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    "! The data of GS1 notation as symbols
    CLASS-METHODS gs1_symbols
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE string_table.

    "! How many digits follow from a position on
    CLASS-METHODS digits
      IMPORTING
        symbols       TYPE string_table
        from          TYPE i
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.


CLASS zabaputil_cl_pdf_barcode IMPLEMENTATION.

  METHOD code128.

    DATA lt_value TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lv_set   TYPE c LENGTH 1.
    DATA lv_index TYPE i.
    DATA lv_run   TYPE i.
    DATA lv_value TYPE i.
    DATA lv_sum   TYPE i.
    DATA lv_first TYPE i.

    DATA(lt_symbol) = symbols( val ).
    DATA(lv_count) = lines( lt_symbol ).
    IF lv_count = 0.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING
          val = `Barcode: there is no text to encode`.
    ENDIF.

    " the start: C when the text begins with four digits - behind an FNC1,
    " where GS1 data begins - or is two digits alone
    lv_first = 1.
    IF lt_symbol[ 1 ] = c_fnc1.
      lv_first = 2.
    ENDIF.
    lv_run = digits( symbols = lt_symbol
                     from    = lv_first ).
    IF lv_run >= 4 OR ( lv_run = 2 AND lv_count = lv_first + 1 ).
      lv_set = `C`.
      INSERT cs_code-start_c INTO TABLE lt_value.
    ELSE.
      lv_set = `B`.
      INSERT cs_code-start_b INTO TABLE lt_value.
    ENDIF.

    lv_index = 1.
    WHILE lv_index <= lv_count.
      DATA(lv_symbol) = lt_symbol[ lv_index ].

      IF lv_symbol = c_fnc1.
        INSERT cs_code-fnc1 INTO TABLE lt_value.
        lv_index = lv_index + 1.
        CONTINUE.
      ENDIF.

      lv_run = digits( symbols = lt_symbol
                       from    = lv_index ).

      IF lv_set = `C`.
        IF lv_run >= 2.
          lv_value = lv_symbol && lt_symbol[ lv_index + 1 ].
          INSERT lv_value INTO TABLE lt_value.
          lv_index = lv_index + 2.
          CONTINUE.
        ENDIF.
        lv_set = `B`.
        INSERT cs_code-shift_b INTO TABLE lt_value.
        CONTINUE.
      ENDIF.

      " in B: a run of six digits, or of four that ends the text, is worth
      " the change to C - an odd one after its first digit
      IF lv_run >= 6 OR ( lv_run >= 4 AND lv_index + lv_run - 1 = lv_count ).
        IF lv_run MOD 2 = 1.
          lv_value = find( val = c_ascii
                           sub = lv_symbol ).
          INSERT lv_value INTO TABLE lt_value.
          lv_index = lv_index + 1.
        ENDIF.
        lv_set = `C`.
        INSERT cs_code-shift_c INTO TABLE lt_value.
        CONTINUE.
      ENDIF.

      lv_value = find( val = c_ascii
                       sub = lv_symbol ).
      INSERT lv_value INTO TABLE lt_value.
      lv_index = lv_index + 1.
    ENDWHILE.

    " the check character: the start, and every value by its position
    LOOP AT lt_value INTO lv_value.
      IF sy-tabix = 1.
        lv_sum = lv_value.
      ELSE.
        lv_sum = lv_sum + lv_value * ( sy-tabix - 1 ).
      ENDIF.
    ENDLOOP.
    lv_value = lv_sum MOD 103.
    INSERT lv_value INTO TABLE lt_value.
    INSERT cs_code-stop INTO TABLE lt_value.

    LOOP AT lt_value INTO lv_value.
      DATA(lv_pattern) = pattern( lv_value ).
      DATA(lv_bar) = abap_true.
      DO strlen( lv_pattern ) TIMES.
        DATA(lv_offset) = sy-index - 1.
        DATA(lv_width) = CONV i( lv_pattern+lv_offset(1) ).
        DO lv_width TIMES.
          IF lv_bar = abap_true.
            result = result && `1`.
          ELSE.
            result = result && `0`.
          ENDIF.
        ENDDO.
        lv_bar = xsdbool( lv_bar = abap_false ).
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD draw.

    DATA lv_module TYPE f.
    DATA lv_start  TYPE i.
    DATA lv_offset TYPE i.

    DATA(lv_count) = strlen( modules ).
    IF lv_count = 0.
      RETURN.
    ENDIF.
    lv_module = w.
    lv_module = lv_module / lv_count.

    " a run of bar modules is one bar
    lv_start = -1.
    DO lv_count + 1 TIMES.
      lv_offset = sy-index - 1.
      IF lv_offset < lv_count AND modules+lv_offset(1) = `1`.
        IF lv_start < 0.
          lv_start = lv_offset.
        ENDIF.
      ELSEIF lv_start >= 0.
        page->rect( x     = x + lv_start * lv_module
                    y     = y
                    w     = ( lv_offset - lv_start ) * lv_module
                    h     = h
                    style = `F` ).
        lv_start = -1.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD symbols.

    DATA lv_text TYPE string.

    lv_text = val.
    IF strlen( lv_text ) > 0 AND lv_text(1) = `(`.
      result = gs1_symbols( lv_text ).
      RETURN.
    ENDIF.

    " a blank is a character of its own: substring( ) keeps it, a field
    " of type c would lose it
    DO strlen( lv_text ) TIMES.
      DATA(lv_char) = substring( val = lv_text
                                 off = sy-index - 1
                                 len = 1 ).
      IF find( val = c_ascii
               sub = lv_char ) < 0.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: Code 128 has no character "{ lv_char }" (position { sy-index })|.
      ENDIF.
      INSERT lv_char INTO TABLE result.
    ENDDO.

  ENDMETHOD.

  METHOD gs1_symbols.

    DATA lt_gs1   TYPE zabaputil_cl_util_context=>ty_t_gs1.
    DATA ls_gs1   TYPE zabaputil_cl_util_context=>ty_s_gs1.
    DATA lv_rest  TYPE string.
    DATA lv_close TYPE i.
    DATA lv_next  TYPE i.

    " (ai)value(ai)value ... as the pairs of the GS1 element string
    lv_rest = val.
    WHILE lv_rest IS NOT INITIAL.
      lv_close = find( val = lv_rest
                       sub = `)` ).
      IF lv_rest(1) <> `(` OR lv_close < 2.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING
            val = |Barcode: { val } is no GS1 data - (AI)value(AI)value|.
      ENDIF.
      CLEAR ls_gs1.
      ls_gs1-ai = substring( val = lv_rest
                             off = 1
                             len = lv_close - 1 ).
      lv_rest = substring( val = lv_rest
                           off = lv_close + 1 ).
      lv_next = find( val = lv_rest
                      sub = `(` ).
      IF lv_next < 0.
        ls_gs1-value = lv_rest.
        CLEAR lv_rest.
      ELSE.
        ls_gs1-value = substring( val = lv_rest
                                  len = lv_next ).
        lv_rest = substring( val = lv_rest
                             off = lv_next ).
      ENDIF.
      INSERT ls_gs1 INTO TABLE lt_gs1.
    ENDWHILE.

    " the element string as the utilities of GS1 write it: a group separator
    " behind every value of a variable length that is followed by another -
    " where GS1-128 has FNC1. An identifier GS1 does not know raises there
    DATA(lv_separator) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `1D` ) ).
    DATA(lv_data) = zabaputil_cl_util_context=>barcode_gs1_build( lt_gs1 ).

    INSERT c_fnc1 INTO TABLE result.
    SPLIT lv_data AT lv_separator INTO TABLE DATA(lt_field).
    LOOP AT lt_field INTO DATA(lv_field).
      IF sy-tabix > 1.
        INSERT c_fnc1 INTO TABLE result.
      ENDIF.
      INSERT LINES OF symbols( lv_field ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD digits.

    DATA lv_index TYPE i.

    lv_index = from.
    WHILE lv_index <= lines( symbols ).
      DATA(lv_symbol) = symbols[ lv_index ].
      IF strlen( lv_symbol ) <> 1 OR lv_symbol CN `0123456789`.
        EXIT.
      ENDIF.
      result = result + 1.
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD pattern.

    " the widths of the symbols 0 to 105 of Code 128, six each, bar first;
    " the stop character is the one with seven
    CONSTANTS lc_stop TYPE string VALUE `2331112`.
    DATA lv_all TYPE string.

    IF val = cs_code-stop.
      result = lc_stop.
      RETURN.
    ENDIF.
    lv_all = `212222222122222221121223121322131222122213122312132212221213221312` &&
                `231212112232122132122231113222123122123221223211221132221231213212` &&
                `223112312131311222321122321221312212322112322211212123212321232121` &&
                `111323131123131321112313132113132311211313231113231311112133112331` &&
                `132131113123113321133121313121211331231131213113213311213131311123` &&
                `311321331121312113312311332111314111221411431111111224111422121124` &&
                `121421141122141221112214112412122114122411142112142211241211221114` &&
                `413111241112134111111242121142121241114212124112124211411212421112` &&
                `421211212141214121412121111143111341131141114113114311411113411311` &&
                `113141114131311141411131211412211214211232`.
    result = substring( val = lv_all
                        off = val * 6
                        len = 6 ).

  ENDMETHOD.

ENDCLASS.

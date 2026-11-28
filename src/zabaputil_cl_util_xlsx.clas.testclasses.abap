"! what the tests share: a part of a written file as text, and a workbook put
"! together by hand the way another program writes it
CLASS ltcl_file DEFINITION FOR TESTING.

  PUBLIC SECTION.

    CONSTANTS cv_main TYPE string VALUE `http://schemas.openxmlformats.org/spreadsheetml/2006/main`.
    CONSTANTS cv_rel  TYPE string VALUE `http://schemas.openxmlformats.org/officeDocument/2006/relationships`.
    CONSTANTS cv_pkg  TYPE string VALUE `http://schemas.openxmlformats.org/package/2006/relationships`.

    CLASS-METHODS part
      IMPORTING
        file          TYPE xstring
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS has_part
      IMPORTING
        file          TYPE xstring
        name          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! a workbook of one sheet `Data`: the rows of its sheetData, the si
    "! elements of its shared strings, the content of its styleSheet
    CLASS-METHODS pack
      IMPORTING
        rows          TYPE string
        shared        TYPE string OPTIONAL
        styles        TYPE string OPTIONAL
        workbook_pr   TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE xstring.

ENDCLASS.

CLASS ltcl_file IMPLEMENTATION.

  METHOD part.
    DATA(lt_file) = zabaputil_cl_util_context=>zip_unpack( file ).
    LOOP AT lt_file INTO DATA(ls_file).
      IF ls_file-name = name.
        result = zabaputil_cl_util_context=>conv_get_string_by_xstring( ls_file-content ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_part.
    DATA(lt_file) = zabaputil_cl_util_context=>zip_unpack( file ).
    result = xsdbool( line_exists( lt_file[ name = name ] ) ).
  ENDMETHOD.

  METHOD pack.
    DATA(lv_head) = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`.
    " as Excel writes it: indented, the document named from the root of the
    " package, the sheet under an id that is not its position
    DATA(lt_file) = VALUE zabaputil_cl_util_context=>ty_t_zip_file(
        ( name    = `[Content_Types].xml`
          content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                        |{ lv_head }<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">|
                     && `<Default Extension="xml" ContentType="application/xml"/></Types>` ) )
        ( name    = `_rels/.rels`
          content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                        |{ lv_head }<Relationships xmlns="{ cv_pkg }">|
                     && |<Relationship Id="rId1" Type="{ cv_rel }/officeDocument" Target="/xl/workbook.xml"/>|
                     && `</Relationships>` ) )
        ( name    = `xl/workbook.xml`
          content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                        |{ lv_head }\n<workbook xmlns="{ cv_main }" xmlns:r="{ cv_rel }">\n  { workbook_pr }|
                     && |\n  <sheets>\n    <sheet name="Data" sheetId="7" r:id="rId3"/>\n  </sheets>\n</workbook>| ) )
        ( name    = `xl/_rels/workbook.xml.rels`
          content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                        |{ lv_head }<Relationships xmlns="{ cv_pkg }">|
                     && |<Relationship Id="rId1" Type="{ cv_rel }/sharedStrings" Target="sharedStrings.xml"/>|
                     && |<Relationship Id="rId2" Type="{ cv_rel }/styles" Target="/xl/styles.xml"/>|
                     && |<Relationship Id="rId3" Type="{ cv_rel }/worksheet" Target="worksheets/sheet1.xml"/>|
                     && `</Relationships>` ) )
        ( name    = `xl/worksheets/sheet1.xml`
          content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                        |{ lv_head }\n<worksheet xmlns="{ cv_main }">\n  <dimension ref="A1"/>|
                     && |\n  <sheetData>\n    { rows }\n  </sheetData>\n  <pageMargins left="0.7" right="0.7"/>|
                     && |\n</worksheet>| ) ) ).
    IF shared IS NOT INITIAL.
      INSERT VALUE #( name    = `xl/sharedStrings.xml`
                      content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                                    |{ lv_head }<sst xmlns="{ cv_main }">{ shared }</sst>| ) ) INTO TABLE lt_file.
    ENDIF.
    IF styles IS NOT INITIAL.
      INSERT VALUE #( name    = `xl/styles.xml`
                      content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
                                    |{ lv_head }<styleSheet xmlns="{ cv_main }">{ styles }</styleSheet>| ) )
             INTO TABLE lt_file.
    ENDIF.
    result = zabaputil_cl_util_context=>zip_pack( lt_file ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! WRITING
"! ================================================================
CLASS ltcl_write DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_row,
        text   TYPE string,
        num    TYPE i,
        amount TYPE p LENGTH 8 DECIMALS 2,
        date   TYPE d,
        time   TYPE t,
        numc   TYPE n LENGTH 4,
        flag   TYPE abap_bool,
        sub    TYPE string_table,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    METHODS rows
      RETURNING
        VALUE(result) TYPE ty_t_row.

    METHODS parts_of_a_workbook  FOR TESTING RAISING cx_static_check.
    METHODS cells_by_type        FOR TESTING RAISING cx_static_check.
    METHODS header_row           FOR TESTING RAISING cx_static_check.
    METHODS dates_excel_has_none FOR TESTING RAISING cx_static_check.
    METHODS layout_columns       FOR TESTING RAISING cx_static_check.
    METHODS column_unknown       FOR TESTING RAISING cx_static_check.
    METHODS freeze_and_filter    FOR TESTING RAISING cx_static_check.
    METHODS excel_table          FOR TESTING RAISING cx_static_check.
    METHODS excel_table_options  FOR TESTING RAISING cx_static_check.
    METHODS start_cell           FOR TESTING RAISING cx_static_check.
    METHODS start_cell_invalid   FOR TESTING RAISING cx_static_check.
    METHODS totals_row           FOR TESTING RAISING cx_static_check.
    METHODS totals_row_of_table  FOR TESTING RAISING cx_static_check.
    METHODS formula_column       FOR TESTING RAISING cx_static_check.
    METHODS sheet_names          FOR TESTING RAISING cx_static_check.
    METHODS empty_workbook       FOR TESTING RAISING cx_static_check.
    METHODS no_columns           FOR TESTING RAISING cx_static_check.
    METHODS column_letters       FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_write IMPLEMENTATION.

  METHOD rows.
    result = VALUE #( ( text = `  a&b<c` num = -5 amount = `-12.5` date = `20240315` time = `103000`
                        numc = `0042` flag = abap_true )
                      ( text = |x{ cl_abap_char_utilities=>form_feed }y| ) ).
  ENDMETHOD.

  METHOD parts_of_a_workbook.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ).

    cl_abap_unit_assert=>assert_true( ltcl_file=>has_part( file = lv_file
                                                           name = `[Content_Types].xml` ) ).
    cl_abap_unit_assert=>assert_true( ltcl_file=>has_part( file = lv_file
                                                           name = `_rels/.rels` ) ).
    cl_abap_unit_assert=>assert_true( ltcl_file=>has_part( file = lv_file
                                                           name = `xl/workbook.xml` ) ).
    cl_abap_unit_assert=>assert_true( ltcl_file=>has_part( file = lv_file
                                                           name = `xl/_rels/workbook.xml.rels` ) ).
    cl_abap_unit_assert=>assert_true( ltcl_file=>has_part( file = lv_file
                                                           name = `xl/styles.xml` ) ).
    cl_abap_unit_assert=>assert_true( ltcl_file=>has_part( file = lv_file
                                                           name = `xl/worksheets/sheet1.xml` ) ).

    DATA(lv_book) = ltcl_file=>part( file = lv_file
                                     name = `xl/workbook.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_book CS `<sheet name="Sheet1" sheetId="1" r:id="rId1"/>` ) ).
    DATA(lv_types) = ltcl_file=>part( file = lv_file
                                      name = `[Content_Types].xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_types CS `PartName="/xl/worksheets/sheet1.xml"` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_types CS `PartName="/xl/styles.xml"` ) ).
  ENDMETHOD.

  METHOD cells_by_type.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ).
    DATA(lv_sheet) = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " a text with the blanks in front, & and < escaped
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<c r="A2" t="inlineStr"><is><t xml:space="preserve">  a&amp;b&lt;c</t></is></c>` ) ).
    " numbers as values, with a period and the sign in front
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="B2"><v>-5</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="C2"><v>-12.50</v></c>` ) ).
    " 2024-03-15 is day 45366, 10:30 is 0.4375 of a day - both with a format
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="D2" s="1"><v>45366</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="E2" s="2"><v>0.4375</v></c>` ) ).
    " a NUMC keeps its zeros as text, a flag is a boolean
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="F2" t="inlineStr"><is><t xml:space="preserve">0042</t>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="G2" t="b"><v>1</v></c>` ) ).
    " the table column is no column: seven of them
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<dimension ref="A1:G3"/>` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_sheet CS `r="H1"` ) ).

    " second row: the form feed XML forbids dropped, an initial date and a
    " flag that is not set no cell at all
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<t xml:space="preserve">xy</t>` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_sheet CS `r="D3"` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_sheet CS `r="G3"` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="B3"><v>0</v></c>` ) ).

    " the formats the cells name: the date and the time Excel has built in
    DATA(lv_styles) = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_styles CS `<cellXfs count="4">` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_styles CS `<xf numFmtId="14" fontId="0"` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_styles CS `<xf numFmtId="21" fontId="0"` ) ).
  ENDMETHOD.

  METHOD header_row.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ).
    DATA(lv_sheet) = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).
    " the component names, in the bold format
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<row r="1"><c r="A1" s="3" t="inlineStr"><is><t xml:space="preserve">TEXT</t></is></c>` ) ).
    DATA(lv_styles) = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_styles CS `<xf numFmtId="0" fontId="1"` ) ).

    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                    layout = VALUE #( no_header = abap_true ) ).
    lv_sheet = ltcl_file=>part( file = lv_file
                                name = `xl/worksheets/sheet1.xml` ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_sheet CS `>TEXT<` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="B1"><v>-5</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<dimension ref="A1:G2"/>` ) ).
  ENDMETHOD.

  METHOD dates_excel_has_none.
    TYPES:
      BEGIN OF ty_s_date,
        date TYPE d,
      END OF ty_s_date.
    DATA lt_date TYPE STANDARD TABLE OF ty_s_date WITH EMPTY KEY.

    lt_date = VALUE #( ( date = `18991231` )
                       ( date = `19000101` )
                       ( date = `19000228` )
                       ( date = `19000301` )
                       ( date = `99991231` ) ).
    DATA(lv_sheet) = ltcl_file=>part( file = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_date )
                                      name = `xl/worksheets/sheet1.xml` ).
    " before 1900 a text, then the days as Excel counts them - one ahead
    " up to the 1900-02-29 it believes in
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="A2" t="inlineStr"><is><t xml:space="preserve">1899-12-31</t>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="A3" s="1"><v>1</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="A4" s="1"><v>59</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="A5" s="1"><v>61</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="A6" s="1"><v>2958465</v></c>` ) ).
  ENDMETHOD.

  METHOD layout_columns.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = VALUE #( t_column = VALUE #( ( name = `amount` header = `Amount "net"` format = `#,##0.00` width = 20 )
                                              ( name = `TEXT` )
                                              ( name = `date` format = `dd.mm.yyyy` ) ) ) ).
    DATA(lv_sheet) = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " the columns the layout names, in its order, under its headers
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<t xml:space="preserve">Amount &quot;net&quot;</t>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="A2" s="1"><v>-12.50</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="C2" s="2"><v>45366</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<dimension ref="A1:C3"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<col min="1" max="1" width="20" customWidth="1"/>` ) ).

    DATA(lv_styles) = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_styles CS `<numFmt numFmtId="164" formatCode="#,##0.00"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_styles CS `<numFmt numFmtId="165" formatCode="dd.mm.yyyy"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_styles CS `<xf numFmtId="165" fontId="0"` ) ).
  ENDMETHOD.

  METHOD column_unknown.
    TRY.
        zabaputil_cl_util_xlsx=>xlsx_by_itab(
            val    = rows( )
            layout = VALUE #( t_column = VALUE #( ( name = `NOPE` ) ) ) ).
        cl_abap_unit_assert=>fail( `a column the row does not have has to raise` ).
      CATCH zabaputil_cx_util_error INTO DATA(x).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_COLUMN_UNKNOWN` ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD freeze_and_filter.
    DATA(lv_sheet) = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                     layout = VALUE #( freeze_header = abap_true
                                                                       auto_filter   = abap_true ) )
        name = `xl/worksheets/sheet1.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `</sheetData><autoFilter ref="A1:G3"/></worksheet>` ) ).
  ENDMETHOD.

  METHOD excel_table.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( rows( )
        )->add_sheet( val    = rows( )
                      layout = VALUE #( table_style = `TableStyleMedium2`
                                        t_column    = VALUE #( ( name = `TEXT` header = `Name` )
                                                               ( name = `NUM` header = `name` ) ) )
        )->get_xlsx( ).

    " the table of the second sheet is table 1 of the workbook
    DATA(lv_table) = ltcl_file=>part( file = lv_file
                                      name = `xl/tables/table1.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `id="1" name="Table1" displayName="Table1" ref="A1:B3"` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `<tableStyleInfo name="TableStyleMedium2"` ) ).
    " a column name once - a second `name` gets a number
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `<tableColumn id="1" name="Name"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `<tableColumn id="2" name="name 2"/>` ) ).

    DATA(lv_sheet) = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet2.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<tableParts count="1"><tablePart r:id="rId1"/></tableParts>` ) ).
    " the header as the table names its columns, and not bold: the table
    " formats it
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<c r="B1" t="inlineStr"><is><t xml:space="preserve">name 2</t></is></c>` ) ).

    DATA(lv_rels) = ltcl_file=>part( file = lv_file
                                     name = `xl/worksheets/_rels/sheet2.xml.rels` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_rels CS `Target="../tables/table1.xml"` ) ).
    DATA(lv_types) = ltcl_file=>part( file = lv_file
                                      name = `[Content_Types].xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_types CS `PartName="/xl/tables/table1.xml"` ) ).

    " no row, no table: Excel takes none without one
    DATA lt_none TYPE ty_t_row.
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = lt_none
                                                    layout = VALUE #( table_style = `TableStyleMedium2` ) ).
    cl_abap_unit_assert=>assert_false( ltcl_file=>has_part( file = lv_file
                                                            name = `xl/tables/table1.xml` ) ).
  ENDMETHOD.

  METHOD excel_table_options.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      layout = VALUE #( table_style    = `TableStyleLight9`
                                        table_name     = `My Orders 2024`
                                        no_row_stripes = abap_true
                                        column_stripes = abap_true
                                        no_filter      = abap_true )
        )->add_sheet( val    = rows( )
                      layout = VALUE #( table_style = `TableStyleLight9`
                                        table_name  = `myorders2024` )
        )->add_sheet( val    = rows( )
                      layout = VALUE #( table_style = `TableStyleLight9`
                                        table_name  = `A1` )
        )->get_xlsx( ).

    " the name without what Excel does not take in one, no filter, the
    " stripes as asked for
    DATA(lv_table) = ltcl_file=>part( file = lv_file
                                      name = `xl/tables/table1.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `name="MyOrders2024" displayName="MyOrders2024"` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_table CS `<autoFilter` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `showRowStripes="0" showColumnStripes="1"` ) ).

    " a name the workbook has, whatever its case, gets a number - and one
    " that reads as a cell an underscore
    lv_table = ltcl_file=>part( file = lv_file
                                name = `xl/tables/table2.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `name="myorders2024_2"` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `<autoFilter ref="A1:G3"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `showRowStripes="1" showColumnStripes="0"` ) ).
    lv_table = ltcl_file=>part( file = lv_file
                                name = `xl/tables/table3.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `name="_A1"` ) ).
  ENDMETHOD.

  METHOD start_cell.
    DATA(lv_sheet) = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                     layout = VALUE #( start_cell    = `b3`
                                                                       freeze_header = abap_true
                                                                       auto_filter   = abap_true ) )
        name = `xl/worksheets/sheet1.xml` ).

    " the header in B3, the rows under it, the columns from B on
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<dimension ref="B3:H5"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<row r="3"><c r="B3" s="3" t="inlineStr">` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="C4"><v>-5</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<col min="2" max="2"` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_sheet CS `<col min="1" max="1"` ) ).
    " what is frozen ends with the header row, and the filter is on it
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<pane ySplit="3" topLeftCell="A4"` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<autoFilter ref="B3:H5"/>` ) ).

    " an Excel table stands where the rows stand
    DATA(lv_table) = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                     layout = VALUE #( start_cell  = `$C$2`
                                                                       table_style = `TableStyleMedium2` ) )
        name = `xl/tables/table1.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS ` ref="C2:I4"` ) ).
  ENDMETHOD.

  METHOD start_cell_invalid.
    TRY.
        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                              layout = VALUE #( start_cell = `here` ) ).
        cl_abap_unit_assert=>fail( `a start cell that is none has to raise` ).
      CATCH zabaputil_cx_util_error INTO DATA(x).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_START_CELL` ) ).
    ENDTRY.
    TRY.
        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                              layout = VALUE #( start_cell = `B0` ) ).
        cl_abap_unit_assert=>fail( `row 0 has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD totals_row.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = VALUE #( auto_filter = abap_true
                          t_column    = VALUE #( ( name = `TEXT` total = `Total` )
                                                 ( name = `NUM` total = `SUM` )
                                                 ( name = `AMOUNT` total = `average` format = `0.00` )
                                                 ( name = `DATE` total = `count` )
                                                 ( name = `NUMC` total = `max` ) ) ) ).
    DATA(lv_sheet) = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " a row under the rows: the label bold, a formula over the cells of
    " the column with the value it gives - -5 and 0, -12.5 and 0
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<dimension ref="A1:E4"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<row r="4"><c r="A4" s="3" t="inlineStr"><is><t xml:space="preserve">Total</t></is></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="B4"><f>SUBTOTAL(109,B2:B3)</f><v>-5</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="C4" s="1"><f>SUBTOTAL(101,C2:C3)</f><v>-6.25</v></c>` ) ).
    " a count is a number, not a date: the cells that hold a value, one
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="D4"><f>SUBTOTAL(103,D2:D3)</f><v>1</v></c>` ) ).
    " a NUMC is text: nothing to add up here, the formula stands
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="E4"><f>SUBTOTAL(104,E2:E3)</f>` ) ).
    " the filter ends above the totals row
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<autoFilter ref="A1:E3"/>` ) ).

    " Excel is asked to work the formulas out when it opens the file
    cl_abap_unit_assert=>assert_true( xsdbool( ltcl_file=>part( file = lv_file
                                                                name = `xl/workbook.xml` )
                                               CS `</sheets><calcPr fullCalcOnLoad="1"/></workbook>` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( ltcl_file=>part( file = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) )
                                                                 name = `xl/workbook.xml` ) CS `calcPr` ) ).

    " read back, a totals cell holds the value that was written with it
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_cells( ).
    cl_abap_unit_assert=>assert_equals( exp = `-6.25`
                                        act = lt_cell[ row = 4 col = 3 ]-value ).

    " no row, no totals row
    DATA lt_none TYPE ty_t_row.
    lv_sheet = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
                   val    = lt_none
                   layout = VALUE #( t_column = VALUE #( ( name = `NUM` total = `sum` ) ) ) )
        name = `xl/worksheets/sheet1.xml` ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_sheet CS `SUBTOTAL` ) ).
  ENDMETHOD.

  METHOD totals_row_of_table.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = VALUE #( table_style = `TableStyleMedium2`
                          table_name  = `Orders`
                          t_column    = VALUE #( ( name = `TEXT` header = `Name` total = `Total` )
                                                 ( name = `NUM` header = `Qty [pcs] #1` total = `sum` )
                                                 ( name = `AMOUNT` total = `min` ) ) ) ).

    " the table reaches over the totals row, its filter does not
    DATA(lv_table) = ltcl_file=>part( file = lv_file
                                      name = `xl/tables/table1.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS ` ref="A1:C4" totalsRowCount="1"><autoFilter ref="A1:C3"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `<tableColumn id="1" name="Name" totalsRowLabel="Total"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `name="Qty [pcs] #1" totalsRowFunction="sum"/>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_table CS `name="AMOUNT" totalsRowFunction="min"/>` ) ).

    " the formula names the column of the table - with what a reference
    " reads as its own syntax marked - and the label is not bold: the
    " table formats its totals row
    DATA(lv_sheet) = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<c r="A4" t="inlineStr"><is><t xml:space="preserve">Total</t></is></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<c r="B4"><f>SUBTOTAL(109,Orders[Qty '[pcs'] '#1])</f><v>-5</v></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool(
        lv_sheet CS `<c r="C4"><f>SUBTOTAL(105,Orders[AMOUNT])</f><v>-12.5</v></c>` ) ).
  ENDMETHOD.

  METHOD formula_column.
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = VALUE #( t_column = VALUE #( ( name = `NUM` )
                                              ( name = `AMOUNT` )
                                              ( header = `Value` formula = `=A{row}*B{row}` format = `0.00` total = `sum` )
                                              ( formula = `IF(A{row}<0,"neg","")` ) ) ) ).
    DATA(lv_sheet) = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " every cell of the column the formula for its row, without a value
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `>Value</t>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="C2" s="1"><f>A2*B2</f></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="C3" s="1"><f>A3*B3</f></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="D2"><f>IF(A2&lt;0,&quot;neg&quot;,&quot;&quot;)</f></c>` ) ).
    " a column without a name and without a header is named by its place
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `>Column4</t>` ) ).
    " its total is a formula too, and nothing was added up for it here
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<c r="C4" s="1"><f>SUBTOTAL(109,C2:C3)</f></c>` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( ltcl_file=>part( file = lv_file
                                                                name = `xl/workbook.xml` ) CS `fullCalcOnLoad` ) ).

    " read back, a formula without a value is no cell - the columns with
    " values are read as before
    DATA lt_back TYPE ty_t_row.
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = -5
                                        act = lt_back[ 1 ]-num ).
  ENDMETHOD.

  METHOD sheet_names.
    DATA lt_none TYPE ty_t_row.

    DATA(lo_xlsx) = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val  = lt_none
                      name = `Q1/Q2: [plan]`
        )->add_sheet( val  = lt_none
                      name = `q1_q2_ _plan_`
        )->add_sheet( lt_none
        )->add_sheet( val  = lt_none
                      name = `a name that is longer than Excel takes it` ).

    DATA(lt_name) = lo_xlsx->get_sheet_names( ).
    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Q1_Q2_ _plan_`
                                        act = lt_name[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `q1_q2_ _plan_ (2)`
                                        act = lt_name[ 2 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `Sheet3`
                                        act = lt_name[ 3 ] ).
    cl_abap_unit_assert=>assert_equals( exp = `a name that is longer than Exce`
                                        act = lt_name[ 4 ] ).

    DATA(lv_book) = ltcl_file=>part( file = lo_xlsx->get_xlsx( )
                                     name = `xl/workbook.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_book CS `<sheet name="Sheet3" sheetId="3" r:id="rId3"/>` ) ).
  ENDMETHOD.

  METHOD empty_workbook.
    DATA(lo_xlsx) = zabaputil_cl_util_xlsx=>factory_by_xlsx( zabaputil_cl_util_xlsx=>factory( )->get_xlsx( ) ).
    DATA(lt_name) = lo_xlsx->get_sheet_names( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Sheet1`
                                        act = lt_name[ 1 ] ).
    cl_abap_unit_assert=>assert_initial( lo_xlsx->get_cells( ) ).
  ENDMETHOD.

  METHOD no_columns.
    " a row without an elementary component: a sheet without a cell
    DATA lt_deep TYPE STANDARD TABLE OF string_table WITH EMPTY KEY.
    INSERT VALUE #( ( `a` ) ) INTO TABLE lt_deep.
    DATA(lv_sheet) = ltcl_file=>part( file = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_deep )
                                      name = `xl/worksheets/sheet1.xml` ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_sheet CS `<dimension ref="A1"/>` ) ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_sheet CS `<cols>` ) ).
  ENDMETHOD.

  METHOD column_letters.
    cl_abap_unit_assert=>assert_equals( exp = `A`
                                        act = zabaputil_cl_util_xlsx=>column_to_letters( 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Z`
                                        act = zabaputil_cl_util_xlsx=>column_to_letters( 26 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `AA`
                                        act = zabaputil_cl_util_xlsx=>column_to_letters( 27 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `XFD`
                                        act = zabaputil_cl_util_xlsx=>column_to_letters( 16384 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = zabaputil_cl_util_xlsx=>column_to_number( `A` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 27
                                        act = zabaputil_cl_util_xlsx=>column_to_number( `aa` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 28
                                        act = zabaputil_cl_util_xlsx=>column_to_number( `$AB$12` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 16384
                                        act = zabaputil_cl_util_xlsx=>column_to_number( `XFD1048576` ) ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! WRITING AND READING BACK
"! ================================================================
CLASS ltcl_roundtrip DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_row,
        text   TYPE string,
        code   TYPE c LENGTH 5,
        num    TYPE i,
        amount TYPE p LENGTH 8 DECIMALS 2,
        ratio  TYPE f,
        date   TYPE d,
        time   TYPE t,
        numc   TYPE n LENGTH 4,
        flag   TYPE abap_bool,
        bytes  TYPE x LENGTH 2,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    METHODS rows
      RETURNING
        VALUE(result) TYPE ty_t_row.

    "! a text with umlauts, blanks around it and what XML escapes - built
    "! from the bytes, so that this source stays 7-bit ASCII
    METHODS text_with_umlauts
      RETURNING
        VALUE(result) TYPE string.

    METHODS typed_table        FOR TESTING RAISING cx_static_check.
    METHODS two_sheets         FOR TESTING RAISING cx_static_check.
    METHODS cells              FOR TESTING RAISING cx_static_check.
    METHODS table_of_strings   FOR TESTING RAISING cx_static_check.
    METHODS headers_of_caller  FOR TESTING RAISING cx_static_check.
    METHODS header_as_name     FOR TESTING RAISING cx_static_check.
    METHODS without_header     FOR TESTING RAISING cx_static_check.
    METHODS elementary_line    FOR TESTING RAISING cx_static_check.
    METHODS text_like_a_code   FOR TESTING RAISING cx_static_check.
    METHODS timestamp          FOR TESTING RAISING cx_static_check.
    METHODS start_cell         FOR TESTING RAISING cx_static_check.
    METHODS other_target_types FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_roundtrip IMPLEMENTATION.

  METHOD text_with_umlauts.
    DATA(lv_ue) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `C3BC` ) ).
    DATA(lv_oe) = zabaputil_cl_util_context=>conv_get_string_by_xstring( CONV xstring( `C3B6` ) ).
    result = |  M{ lv_ue }ller & S{ lv_oe }hne <GmbH>  |.
  ENDMETHOD.

  METHOD rows.
    result = VALUE #( ( text = text_with_umlauts( ) code = `AB` num = -5 amount = `-1234.5` ratio = `0.25`
                        date = `20240315` time = `103000` numc = `0042` flag = abap_true bytes = `00FF` )
                      ( )
                      ( text = |line 1{ cl_abap_char_utilities=>newline }line 2| num = 2147483647
                        amount = `99999999.99` date = `19000228` time = `235959` numc = `9999` ) ).
  ENDMETHOD.

  METHOD typed_table.
    DATA lt_back TYPE ty_t_row.

    DATA(lt_rows) = rows( ).
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_rows )
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_rows
                                        act = lt_back ).
  ENDMETHOD.

  METHOD two_sheets.
    DATA lt_back  TYPE ty_t_row.
    DATA lt_names TYPE string_table.
    DATA lt_text  TYPE string_table.

    lt_names = VALUE #( ( `one` ) ( `two` ) ).
    DATA(lo_xlsx) = zabaputil_cl_util_xlsx=>factory_by_xlsx(
        zabaputil_cl_util_xlsx=>factory(
            )->add_sheet( val  = rows( )
                          name = `Rows`
            )->add_sheet( val  = lt_names
                          name = `Names`
            )->get_xlsx( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE string_table( ( `Rows` ) ( `Names` ) )
                                        act = lo_xlsx->get_sheet_names( ) ).

    " by its name, whatever the case - and by its position
    lo_xlsx->get_table( EXPORTING sheet  = `names`
                        IMPORTING result = lt_text ).
    cl_abap_unit_assert=>assert_equals( exp = lt_names
                                        act = lt_text ).
    lo_xlsx->get_table( EXPORTING index  = 1
                        IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = rows( )
                                        act = lt_back ).

    TRY.
        lo_xlsx->get_cells( sheet = `Nope` ).
        cl_abap_unit_assert=>fail( `a sheet the workbook does not have has to raise` ).
      CATCH zabaputil_cx_util_error INTO DATA(x).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_SHEET_UNKNOWN` ) ).
    ENDTRY.
    TRY.
        lo_xlsx->get_cells( index = 3 ).
        cl_abap_unit_assert=>fail( `a position behind the last sheet has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD cells.
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                        zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ) )->get_cells( ).

    " the header, then every cell of the first row by what it is
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 1 col = 1 kind = `S` value = `TEXT` )
        act = lt_cell[ 1 ] ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 2 col = 1 kind = `S` value = text_with_umlauts( ) )
        act = lt_cell[ row = 2 col = 1 ] ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 2 col = 3 kind = `N` value = `-5` )
        act = lt_cell[ row = 2 col = 3 ] ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 2 col = 4 kind = `N` value = `-1234.5` )
        act = lt_cell[ row = 2 col = 4 ] ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 2 col = 6 kind = `D` value = `2024-03-15` )
        act = lt_cell[ row = 2 col = 6 ] ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 2 col = 7 kind = `T` value = `10:30:00` )
        act = lt_cell[ row = 2 col = 7 ] ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 2 col = 9 kind = `B` value = `X` )
        act = lt_cell[ row = 2 col = 9 ] ).
    " 1900-02-28, the last day before the one Excel made up
    cl_abap_unit_assert=>assert_equals( exp = `1900-02-28`
                                        act = lt_cell[ row = 4 col = 6 ]-value ).
    cl_abap_unit_assert=>assert_equals( exp = `23:59:59`
                                        act = lt_cell[ row = 4 col = 7 ]-value ).
    " a cell without a value is none
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( lt_cell[ row = 3 col = 1 ] ) ) ).
  ENDMETHOD.

  METHOD table_of_strings.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    DATA(lr_tab) = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                       zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ) )->get_table_ref( ).
    ASSIGN lr_tab->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( <tab> ) ).

    ASSIGN <tab>[ 1 ] TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT `AMOUNT` OF STRUCTURE <row> TO FIELD-SYMBOL(<amount>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = `-1234.5`
                                        act = <amount> ).
    ASSIGN COMPONENT `DATE` OF STRUCTURE <row> TO FIELD-SYMBOL(<date>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15`
                                        act = <date> ).
    " the second row holds its numbers and nothing else
    ASSIGN <tab>[ 2 ] TO <row>.
    ASSIGN COMPONENT `TEXT` OF STRUCTURE <row> TO FIELD-SYMBOL(<text>).
    cl_abap_unit_assert=>assert_initial( <text> ).
    ASSIGN COMPONENT `NUM` OF STRUCTURE <row> TO FIELD-SYMBOL(<num>).
    cl_abap_unit_assert=>assert_equals( exp = `0`
                                        act = <num> ).

    " no header row: the columns by their letters, the header a row
    lr_tab = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                 zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ) )->get_table_ref( header_row = 0 ).
    ASSIGN lr_tab->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( <tab> ) ).
    ASSIGN <tab>[ 1 ] TO <row>.
    ASSIGN COMPONENT `B` OF STRUCTURE <row> TO FIELD-SYMBOL(<b>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = `CODE`
                                        act = <b> ).
  ENDMETHOD.

  METHOD headers_of_caller.
    DATA lt_back TYPE ty_t_row.

    DATA(lt_column) = VALUE zabaputil_cl_util_xlsx=>ty_t_column( ( name = `NUM` header = `Anzahl` )
                                                                 ( name = `TEXT` header = `Bezeichnung` ) ).
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                          layout = VALUE #( t_column = lt_column ) ).

    " the headers that wrote the columns read them back
    zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_table( EXPORTING t_column = lt_column
                                                                   IMPORTING result   = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_back ) ).
    cl_abap_unit_assert=>assert_equals( exp = -5
                                        act = lt_back[ 1 ]-num ).
    cl_abap_unit_assert=>assert_equals( exp = text_with_umlauts( )
                                        act = lt_back[ 1 ]-text ).
    cl_abap_unit_assert=>assert_initial( lt_back[ 1 ]-amount ).

    " without them no column has a component: no row
    zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_table( IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_initial( lt_back ).
  ENDMETHOD.

  METHOD header_as_name.
    TYPES:
      BEGIN OF ty_s_order,
        order_id TYPE i,
        net_eur  TYPE p LENGTH 8 DECIMALS 2,
      END OF ty_s_order.
    DATA lt_order TYPE STANDARD TABLE OF ty_s_order WITH EMPTY KEY.
    DATA lt_back  TYPE STANDARD TABLE OF ty_s_order WITH EMPTY KEY.

    lt_order = VALUE #( ( order_id = 4711 net_eur = `10.5` ) ).
    " a header as a person writes it finds its component: ` Order Id `
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = lt_order
        layout = VALUE #( t_column = VALUE #( ( name = `ORDER_ID` header = ` Order Id ` )
                                              ( name = `NET_EUR` header = `net eur` ) ) ) ).
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_order
                                        act = lt_back ).
  ENDMETHOD.

  METHOD without_header.
    DATA lt_back TYPE ty_t_row.

    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                          layout = VALUE #( no_header = abap_true ) ).
    zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_table( EXPORTING header_row = 0
                                                                   IMPORTING result     = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = rows( )
                                        act = lt_back ).
  ENDMETHOD.

  METHOD elementary_line.
    DATA lt_num  TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lt_back TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    lt_num = VALUE #( ( 3 ) ( -1 ) ( 0 ) ).
    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_num ).
    cl_abap_unit_assert=>assert_true( xsdbool( ltcl_file=>part( file = lv_file
                                                                name = `xl/worksheets/sheet1.xml` ) CS `>TABLE_LINE<` ) ).
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_num
                                        act = lt_back ).
  ENDMETHOD.

  METHOD text_like_a_code.
    " `_x0041_` is how a file names the character A - a text that reads
    " like that has to come back as it went in
    DATA lt_text TYPE string_table.
    DATA lt_back TYPE string_table.

    lt_text = VALUE #( ( `a_x0041_b` ) ( `_x` ) ( `=SUM(A1)` ) ( `007` ) ).
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_text )
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_text
                                        act = lt_back ).
  ENDMETHOD.

  METHOD timestamp.
    TYPES:
      BEGIN OF ty_s_stamp,
        stamp TYPE timestamp,
      END OF ty_s_stamp.
    DATA lt_stamp TYPE STANDARD TABLE OF ty_s_stamp WITH EMPTY KEY.
    DATA lt_back  TYPE STANDARD TABLE OF ty_s_stamp WITH EMPTY KEY.

    lt_stamp = VALUE #( ( stamp = `20240315103000` ) ( ) ).
    " a timestamp is a packed number that says what it is by its name
    " only - a runtime that does not hand the name on writes the number
    IF zabaputil_cl_util_context=>rtti_get_type_name( lt_stamp[ 1 ]-stamp ) <> `TIMESTAMP`.
      RETURN.
    ENDIF.

    DATA(lv_file) = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_stamp ).
    " the day and the fraction of the day, in the date-and-time format
    cl_abap_unit_assert=>assert_true( xsdbool( ltcl_file=>part( file = lv_file
                                                                name = `xl/worksheets/sheet1.xml` )
                                               CS `<c r="A2" s="1"><v>45366.4375</v></c>` ) ).
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_cells( ).
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15T10:30:00`
                                        act = lt_cell[ row = 2 ]-value ).

    lt_stamp = VALUE #( ( stamp = `20240315103000` ) ).
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_stamp
                                        act = lt_back ).
  ENDMETHOD.

  METHOD start_cell.
    " rows that do not start in A1 are read by the row their header is in
    DATA lt_back TYPE ty_t_row.

    DATA(lo_xlsx) = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                              layout = VALUE #( start_cell = `C5` ) ) ).
    lo_xlsx->get_table( EXPORTING header_row = 5
                        IMPORTING result     = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = rows( )
                                        act = lt_back ).

    DATA(lt_cell) = lo_xlsx->get_cells( ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE zabaputil_cl_util_xlsx=>ty_s_cell( row = 5 col = 3 kind = `S` value = `TEXT` )
        act = lt_cell[ 1 ] ).
  ENDMETHOD.

  METHOD other_target_types.
    " what was written as one type read into another: every cell as text,
    " a date and a flag as numbers
    TYPES:
      BEGIN OF ty_s_text,
        text   TYPE string,
        num    TYPE string,
        amount TYPE c LENGTH 20,
        date   TYPE string,
        time   TYPE string,
        flag   TYPE i,
      END OF ty_s_text.
    DATA lt_back TYPE STANDARD TABLE OF ty_s_text WITH EMPTY KEY.

    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) )
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = `-5`
                                        act = lt_back[ 1 ]-num ).
    cl_abap_unit_assert=>assert_equals( exp = `-1234.5`
                                        act = lt_back[ 1 ]-amount ).
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15`
                                        act = lt_back[ 1 ]-date ).
    cl_abap_unit_assert=>assert_equals( exp = `10:30:00`
                                        act = lt_back[ 1 ]-time ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lt_back[ 1 ]-flag ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! READING WHAT ANOTHER PROGRAM WROTE
"! ================================================================
CLASS ltcl_read DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS cell
      IMPORTING
        t_cell        TYPE zabaputil_cl_util_xlsx=>ty_t_cell
        row           TYPE i
        col           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS shared_strings       FOR TESTING RAISING cx_static_check.
    METHODS dates_by_format      FOR TESTING RAISING cx_static_check.
    METHODS numbers_as_stored    FOR TESTING RAISING cx_static_check.
    METHODS cells_without_address FOR TESTING RAISING cx_static_check.
    METHODS other_cell_types     FOR TESTING RAISING cx_static_check.
    METHODS date_system_1904     FOR TESTING RAISING cx_static_check.
    METHODS typed_from_text      FOR TESTING RAISING cx_static_check.
    METHODS cell_that_does_not_fit FOR TESTING RAISING cx_static_check.
    METHODS no_workbook          FOR TESTING RAISING cx_static_check.
    METHODS written_is_not_read  FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_read IMPLEMENTATION.

  METHOD cell.
    " kind and value in one, `-` for a cell that is not there
    result = `-`.
    LOOP AT t_cell INTO DATA(ls_cell).
      IF ls_cell-row = row AND ls_cell-col = col.
        result = |{ ls_cell-kind }:{ ls_cell-value }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD shared_strings.
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
        shared = `<si><t>plain</t></si>`
              && `<si><r><rPr><b/></rPr><t>bold</t></r><r><t xml:space="preserve"> and not</t></r></si>`
              && `<si><t>kanji</t><rPh sb="0" eb="1"><t>reading</t></rPh><phoneticPr fontId="1"/></si>`
              && `<si><t>line 1_x000D__x000A_line 2 _x0041_ _x005F_x0041_ _xZZZZ_</t></si>`
              && `<si><t xml:space="preserve">  </t></si>`
        rows   = `<row r="1"><c r="A1" t="s"><v>0</v></c><c r="B1" t="s"><v>1</v></c>`
              && `<c r="C1" t="s"><v>2</v></c><c r="D1" t="s"><v>3</v></c><c r="E1" t="s"><v>4</v></c>`
              && `<c r="F1" t="s"><v>99</v></c></row>` ) )->get_cells( ).

    cl_abap_unit_assert=>assert_equals( exp = `S:plain`
                                        act = cell( t_cell = lt_cell row = 1 col = 1 ) ).
    " a text formatted in parts is one text
    cl_abap_unit_assert=>assert_equals( exp = `S:bold and not`
                                        act = cell( t_cell = lt_cell row = 1 col = 2 ) ).
    " without the reading aid
    cl_abap_unit_assert=>assert_equals( exp = `S:kanji`
                                        act = cell( t_cell = lt_cell row = 1 col = 3 ) ).
    " a character named by its number, an underscore named that way, and
    " what only looks like one
    cl_abap_unit_assert=>assert_equals(
        exp = |S:line 1{ cl_abap_char_utilities=>cr_lf }line 2 A _x0041_ _xZZZZ_|
        act = cell( t_cell = lt_cell row = 1 col = 4 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `S:  `
                                        act = cell( t_cell = lt_cell row = 1 col = 5 ) ).
    " an index the file has no text for
    cl_abap_unit_assert=>assert_equals( exp = `-`
                                        act = cell( t_cell = lt_cell row = 1 col = 6 ) ).
  ENDMETHOD.

  METHOD dates_by_format.
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
        styles = `<numFmts count="4">`
              && `<numFmt numFmtId="164" formatCode="dd/mm/yyyy\ hh:mm"/>`
              && `<numFmt numFmtId="165" formatCode="[$-407]d\.\ mmmm\ yyyy;@"/>`
              && `<numFmt numFmtId="166" formatCode="#,##0.00\ &quot;days&quot;;[Red]\-#,##0.00"/>`
              && `<numFmt numFmtId="167" formatCode="[h]:mm:ss"/></numFmts>`
              && `<cellStyleXfs count="1"><xf numFmtId="14"/></cellStyleXfs>`
              && `<cellXfs count="7"><xf numFmtId="0"/><xf numFmtId="14"/><xf numFmtId="164"/>`
              && `<xf numFmtId="165"/><xf numFmtId="166"/><xf numFmtId="20"/><xf numFmtId="167"/></cellXfs>`
        rows   = `<row r="1"><c r="A1"><v>45366</v></c><c r="B1" s="1"><v>45366</v></c>`
              && `<c r="C1" s="2"><v>45366.4375</v></c><c r="D1" s="3"><v>45366</v></c>`
              && `<c r="E1" s="4"><v>45366</v></c><c r="F1" s="5"><v>0.4375</v></c>`
              && `<c r="G1" s="6"><v>1.5</v></c><c r="H1" s="6"><v>0.5</v></c></row>`
              && `<row r="2"><c r="A2" s="2"><v>45366.999999</v></c><c r="B2" s="1"><v>60</v></c>`
              && `<c r="C2" s="1"><v>1</v></c><c r="D2" s="1"><v>0</v></c>`
              && `<c r="E2" s="1"><v>45366.75</v></c><c r="F2" s="5"><v>45366.75</v></c></row>` ) )->get_cells( ).

    " the same number is a number, a date or a date with its time - by
    " the format of its cell, a built-in one or one of the file
    cl_abap_unit_assert=>assert_equals( exp = `N:45366`
                                        act = cell( t_cell = lt_cell row = 1 col = 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `D:2024-03-15`
                                        act = cell( t_cell = lt_cell row = 1 col = 2 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `M:2024-03-15T10:30:00`
                                        act = cell( t_cell = lt_cell row = 1 col = 3 ) ).
    " a locale in brackets and escaped characters are no part of the format
    cl_abap_unit_assert=>assert_equals( exp = `D:2024-03-15`
                                        act = cell( t_cell = lt_cell row = 1 col = 4 ) ).
    " the d and the y of a quoted text and the Red of a colour make no date
    cl_abap_unit_assert=>assert_equals( exp = `N:45366`
                                        act = cell( t_cell = lt_cell row = 1 col = 5 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `T:10:30:00`
                                        act = cell( t_cell = lt_cell row = 1 col = 6 ) ).
    " a duration runs on past 24 hours
    cl_abap_unit_assert=>assert_equals( exp = `S:36:00:00`
                                        act = cell( t_cell = lt_cell row = 1 col = 7 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `T:12:00:00`
                                        act = cell( t_cell = lt_cell row = 1 col = 8 ) ).

    " a second before midnight that rounds up is the next day
    cl_abap_unit_assert=>assert_equals( exp = `M:2024-03-16T00:00:00`
                                        act = cell( t_cell = lt_cell row = 2 col = 1 ) ).
    " the 1900-02-29 of Excel is the first of March, day 1 the first of January
    cl_abap_unit_assert=>assert_equals( exp = `D:1900-03-01`
                                        act = cell( t_cell = lt_cell row = 2 col = 2 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `D:1900-01-01`
                                        act = cell( t_cell = lt_cell row = 2 col = 3 ) ).
    " day 0 is no date
    cl_abap_unit_assert=>assert_equals( exp = `N:0`
                                        act = cell( t_cell = lt_cell row = 2 col = 4 ) ).
    " a date format shows the day, a time format the time of a date with one
    cl_abap_unit_assert=>assert_equals( exp = `D:2024-03-15`
                                        act = cell( t_cell = lt_cell row = 2 col = 5 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `T:18:00:00`
                                        act = cell( t_cell = lt_cell row = 2 col = 6 ) ).
  ENDMETHOD.

  METHOD numbers_as_stored.
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
        rows = `<row r="3"><c r="A3"><v>1.5E-3</v></c><c r="B3"><v>0.30000000000000004</v></c>`
            && `<c r="C3"><v>-1.2345E+4</v></c><c r="D3"><v>12.50</v></c><c r="E3"><v>1E+20</v></c>`
            && `<c r="F3"><v>0.1</v><f>A3*2</f></c><c r="G3"><v>999999999999999.9</v></c>`
            && `<c r="H3"><v>0.0</v></c></row>` ) )->get_cells( ).

    " without exponent, and without the digits of the binary number
    cl_abap_unit_assert=>assert_equals( exp = `N:0.0015`
                                        act = cell( t_cell = lt_cell row = 3 col = 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:0.3`
                                        act = cell( t_cell = lt_cell row = 3 col = 2 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:-12345`
                                        act = cell( t_cell = lt_cell row = 3 col = 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:12.5`
                                        act = cell( t_cell = lt_cell row = 3 col = 4 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:100000000000000000000`
                                        act = cell( t_cell = lt_cell row = 3 col = 5 ) ).
    " a formula cell with the value it had last
    cl_abap_unit_assert=>assert_equals( exp = `N:0.1`
                                        act = cell( t_cell = lt_cell row = 3 col = 6 ) ).
    " the 16th digit rounds, and the carry runs through
    cl_abap_unit_assert=>assert_equals( exp = `N:1000000000000000`
                                        act = cell( t_cell = lt_cell row = 3 col = 7 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:0`
                                        act = cell( t_cell = lt_cell row = 3 col = 8 ) ).
  ENDMETHOD.

  METHOD cells_without_address.
    " r is optional: a cell follows the one before, a row the row before
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
        rows = `<row><c><v>1</v></c><c><v>2</v></c></row><row/>`
            && `<row r="5"><c r="C5"><v>3</v></c><c><v>4</v></c></row><row><c><v>5</v></c></row>` ) )->get_cells( ).

    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( lt_cell ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:2`
                                        act = cell( t_cell = lt_cell row = 1 col = 2 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:3`
                                        act = cell( t_cell = lt_cell row = 5 col = 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:4`
                                        act = cell( t_cell = lt_cell row = 5 col = 4 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:5`
                                        act = cell( t_cell = lt_cell row = 6 col = 1 ) ).
  ENDMETHOD.

  METHOD other_cell_types.
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
        rows = `<row r="1"><c r="A1" t="str"><f>A2&amp;"x"</f><v>result</v></c><c r="B1" t="b"><v>1</v></c>`
            && `<c r="C1" t="b"><v>0</v></c><c r="D1" t="e"><v>#DIV/0!</v></c>`
            && `<c r="E1" t="inlineStr"><is><r><t>in</t></r><r><t>line</t></r></is></c>`
            && `<c r="F1" t="d"><v>2024-03-15</v></c><c r="G1" t="d"><v>2024-03-15T10:30:00.000Z</v></c>`
            && `<c r="H1" s="3"/><c r="I1" t="n"><v>7</v></c></row>` ) )->get_cells( ).

    cl_abap_unit_assert=>assert_equals( exp = `S:result`
                                        act = cell( t_cell = lt_cell row = 1 col = 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `B:X`
                                        act = cell( t_cell = lt_cell row = 1 col = 2 ) ).
    " a FALSE is a cell, and empty
    cl_abap_unit_assert=>assert_equals( exp = `B:`
                                        act = cell( t_cell = lt_cell row = 1 col = 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `E:#DIV/0!`
                                        act = cell( t_cell = lt_cell row = 1 col = 4 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `S:inline`
                                        act = cell( t_cell = lt_cell row = 1 col = 5 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `D:2024-03-15`
                                        act = cell( t_cell = lt_cell row = 1 col = 6 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `M:2024-03-15T10:30:00`
                                        act = cell( t_cell = lt_cell row = 1 col = 7 ) ).
    " a cell that is formatted only is none
    cl_abap_unit_assert=>assert_equals( exp = `-`
                                        act = cell( t_cell = lt_cell row = 1 col = 8 ) ).
    cl_abap_unit_assert=>assert_equals( exp = `N:7`
                                        act = cell( t_cell = lt_cell row = 1 col = 9 ) ).
  ENDMETHOD.

  METHOD date_system_1904.
    " day 0 is 1904-01-01 - the same number is another date
    DATA(lt_cell) = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
        workbook_pr = `<workbookPr date1904="1"/>`
        styles      = `<cellXfs count="2"><xf numFmtId="0"/><xf numFmtId="14"/></cellXfs>`
        rows        = `<row r="1"><c r="A1" s="1"><v>43904</v></c></row>` ) )->get_cells( ).
    cl_abap_unit_assert=>assert_equals( exp = `D:2024-03-15`
                                        act = cell( t_cell = lt_cell row = 1 col = 1 ) ).
  ENDMETHOD.

  METHOD typed_from_text.
    " a sheet a person filled in: everything typed as text, in the
    " notations people use
    TYPES:
      BEGIN OF ty_s_row,
        day    TYPE d,
        day2   TYPE d,
        day3   TYPE d,
        at     TYPE t,
        at2    TYPE t,
        amount TYPE p LENGTH 8 DECIMALS 2,
        flag   TYPE abap_bool,
        numc   TYPE n LENGTH 6,
        serial TYPE d,
      END OF ty_s_row.
    DATA lt_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    zabaputil_cl_util_xlsx=>itab_by_xlsx(
      EXPORTING
        val    = ltcl_file=>pack(
                     rows = `<row r="1"><c r="A1" t="inlineStr"><is><t>DAY</t></is></c>`
                         && `<c r="B1" t="inlineStr"><is><t>day2</t></is></c>`
                         && `<c r="C1" t="inlineStr"><is><t>Day3</t></is></c>`
                         && `<c r="D1" t="inlineStr"><is><t>AT</t></is></c>`
                         && `<c r="E1" t="inlineStr"><is><t>AT2</t></is></c>`
                         && `<c r="F1" t="inlineStr"><is><t>AMOUNT</t></is></c>`
                         && `<c r="G1" t="inlineStr"><is><t>FLAG</t></is></c>`
                         && `<c r="H1" t="inlineStr"><is><t>NUMC</t></is></c>`
                         && `<c r="I1" t="inlineStr"><is><t>SERIAL</t></is></c>`
                         && `<c r="J1" t="inlineStr"><is><t>NO COMPONENT</t></is></c></row>`
                         && `<row r="2"><c r="A2" t="inlineStr"><is><t>2024-03-15</t></is></c>`
                         && `<c r="B2" t="inlineStr"><is><t>15.03.2024</t></is></c>`
                         && `<c r="C2"><v>20240315</v></c>`
                         && `<c r="D2" t="inlineStr"><is><t>10:30</t></is></c>`
                         && `<c r="E2"><v>0.4375</v></c>`
                         && `<c r="F2" t="inlineStr"><is><t>1.234,56</t></is></c>`
                         && `<c r="G2" t="inlineStr"><is><t>true</t></is></c>`
                         && `<c r="H2"><v>42</v></c>`
                         && `<c r="I2"><v>45366</v></c>`
                         && `<c r="J2"><v>1</v></c></row>` )
      IMPORTING
        result = lt_row ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_row ) ).
    DATA(ls_row) = lt_row[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = CONV d( `20240315` )
                                        act = ls_row-day ).
    cl_abap_unit_assert=>assert_equals( exp = CONV d( `20240315` )
                                        act = ls_row-day2 ).
    cl_abap_unit_assert=>assert_equals( exp = CONV d( `20240315` )
                                        act = ls_row-day3 ).
    cl_abap_unit_assert=>assert_equals( exp = CONV t( `103000` )
                                        act = ls_row-at ).
    cl_abap_unit_assert=>assert_equals( exp = CONV t( `103000` )
                                        act = ls_row-at2 ).
    cl_abap_unit_assert=>assert_equals( exp = CONV decfloat34( `1234.56` )
                                        act = CONV decfloat34( ls_row-amount ) ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = ls_row-flag ).
    cl_abap_unit_assert=>assert_equals( exp = `000042`
                                        act = CONV string( ls_row-numc ) ).
    " a number in a date component that is no YYYYMMDD is the day Excel counts
    cl_abap_unit_assert=>assert_equals( exp = CONV d( `20240315` )
                                        act = ls_row-serial ).
  ENDMETHOD.

  METHOD cell_that_does_not_fit.
    TYPES:
      BEGIN OF ty_s_row,
        num TYPE i,
        day TYPE d,
      END OF ty_s_row.
    DATA lt_row TYPE STANDARD TABLE OF ty_s_row WITH EMPTY KEY.

    " a text in a number: the address of the cell and what it holds
    TRY.
        zabaputil_cl_util_xlsx=>itab_by_xlsx(
          EXPORTING
            val    = ltcl_file=>pack(
                         rows = `<row r="1"><c r="A1" t="inlineStr"><is><t>NUM</t></is></c></row>`
                             && `<row r="2"><c r="A2"><v>1</v></c></row>`
                             && `<row r="3"><c r="A3" t="inlineStr"><is><t>total</t></is></c></row>` )
          IMPORTING
            result = lt_row ).
        cl_abap_unit_assert=>fail( `a text in a number component has to raise` ).
      CATCH zabaputil_cx_util_error INTO DATA(x).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_CELL_TYPE` ) ).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `cell A3` ) ).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `total` ) ).
    ENDTRY.

    TRY.
        zabaputil_cl_util_xlsx=>itab_by_xlsx(
          EXPORTING
            val    = ltcl_file=>pack(
                         rows = `<row r="1"><c r="A1" t="inlineStr"><is><t>DAY</t></is></c></row>`
                             && `<row r="2"><c r="A2" t="inlineStr"><is><t>soon</t></is></c></row>` )
          IMPORTING
            result = lt_row ).
        cl_abap_unit_assert=>fail( `a text that is no date has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `is no date` ) ).
    ENDTRY.

    " an error cell is no value for any component
    TRY.
        zabaputil_cl_util_xlsx=>itab_by_xlsx(
          EXPORTING
            val    = ltcl_file=>pack(
                         rows = `<row r="1"><c r="A1" t="inlineStr"><is><t>NUM</t></is></c></row>`
                             && `<row r="2"><c r="A2" t="e"><v>#N/A</v></c></row>` )
          IMPORTING
            result = lt_row ).
        cl_abap_unit_assert=>fail( `an error cell has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_CELL_ERROR` ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD no_workbook.
    " not a ZIP archive at all
    TRY.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( CONV xstring( `48656C6C6F2C20776F726C64` ) ).
        cl_abap_unit_assert=>fail( `a file that is no workbook has to raise` ).
      CATCH zabaputil_cx_util_error INTO DATA(x).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_NO_WORKBOOK` ) ).
    ENDTRY.

    " the container of an .xls file of Excel 97-2003
    TRY.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( CONV xstring( `D0CF11E0A1B11AE10000000000000000` ) ).
        cl_abap_unit_assert=>fail( `an .xls file has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_NO_WORKBOOK` ) ).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `.xls` ) ).
    ENDTRY.

    " a ZIP archive that is no workbook
    TRY.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( zabaputil_cl_util_context=>zip_pack(
            VALUE #( ( name = `readme.txt` content = CONV xstring( `414243` ) ) ) ) ).
        cl_abap_unit_assert=>fail( `an archive without a workbook has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_NO_WORKBOOK` ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD written_is_not_read.
    DATA lt_text TYPE string_table.

    TRY.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( lt_text )->get_cells( ).
        cl_abap_unit_assert=>fail( `a workbook that is written is not read` ).
      CATCH zabaputil_cx_util_error INTO DATA(x).
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_NOT_READ` ) ).
    ENDTRY.
    TRY.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_text )
            )->add_sheet( lt_text ).
        cl_abap_unit_assert=>fail( `a workbook that was read takes no sheet` ).
      CATCH zabaputil_cx_util_error INTO x.
        cl_abap_unit_assert=>assert_true( xsdbool( x->get_text( ) CS `XLSX_READ_ONLY` ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

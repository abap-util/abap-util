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
    DATA lt_file TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA ls_file LIKE LINE OF lt_file.
    lt_file = zabaputil_cl_util_context=>zip_unpack( file ).
    
    LOOP AT lt_file INTO ls_file.
      IF ls_file-name = name.
        result = zabaputil_cl_util_context=>conv_get_string_by_xstring( ls_file-content ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_part.
    DATA lt_file TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp1 LIKE sy-subrc.
    DATA temp2 TYPE xsdboolean.
    lt_file = zabaputil_cl_util_context=>zip_unpack( file ).
    
    READ TABLE lt_file WITH KEY name = name TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.
    
    temp2 = boolc( temp1 = 0 ).
    result = temp2.
  ENDMETHOD.

  METHOD pack.
    DATA lv_head TYPE string.
    DATA temp2 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
    DATA temp3 LIKE LINE OF temp2.
    DATA lt_file LIKE temp2.
      DATA temp4 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
      DATA temp5 TYPE zabaputil_cl_util_context=>ty_s_zip_file.
    lv_head = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`.
    " as Excel writes it: indented, the document named from the root of the
    " package, the sheet under an id that is not its position
    
    CLEAR temp2.
    
    temp3-name = `[Content_Types].xml`.
    temp3-content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
|{ lv_head }<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">|
&& `<Default Extension="xml" ContentType="application/xml"/></Types>` ).
    INSERT temp3 INTO TABLE temp2.
    temp3-name = `_rels/.rels`.
    temp3-content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
|{ lv_head }<Relationships xmlns="{ cv_pkg }">|
&& |<Relationship Id="rId1" Type="{ cv_rel }/officeDocument" Target="/xl/workbook.xml"/>|
&& `</Relationships>` ).
    INSERT temp3 INTO TABLE temp2.
    temp3-name = `xl/workbook.xml`.
    temp3-content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
|{ lv_head }\n<workbook xmlns="{ cv_main }" xmlns:r="{ cv_rel }">\n  { workbook_pr }|
&& |\n  <sheets>\n    <sheet name="Data" sheetId="7" r:id="rId3"/>\n  </sheets>\n</workbook>| ).
    INSERT temp3 INTO TABLE temp2.
    temp3-name = `xl/_rels/workbook.xml.rels`.
    temp3-content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
|{ lv_head }<Relationships xmlns="{ cv_pkg }">|
&& |<Relationship Id="rId1" Type="{ cv_rel }/sharedStrings" Target="sharedStrings.xml"/>|
&& |<Relationship Id="rId2" Type="{ cv_rel }/styles" Target="/xl/styles.xml"/>|
&& |<Relationship Id="rId3" Type="{ cv_rel }/worksheet" Target="worksheets/sheet1.xml"/>|
&& `</Relationships>` ).
    INSERT temp3 INTO TABLE temp2.
    temp3-name = `xl/worksheets/sheet1.xml`.
    temp3-content = zabaputil_cl_util_context=>conv_get_xstring_by_string(
|{ lv_head }\n<worksheet xmlns="{ cv_main }">\n  <dimension ref="A1"/>|
&& |\n  <sheetData>\n    { rows }\n  </sheetData>\n  <pageMargins left="0.7" right="0.7"/>|
&& |\n</worksheet>| ).
    INSERT temp3 INTO TABLE temp2.
    
    lt_file = temp2.
    IF shared IS NOT INITIAL.
      
      CLEAR temp4.
      temp4-name = `xl/sharedStrings.xml`.
      temp4-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( |{ lv_head }<sst xmlns="{ cv_main }">{ shared }</sst>| ).
      INSERT temp4 INTO TABLE lt_file.
    ENDIF.
    IF styles IS NOT INITIAL.
      
      CLEAR temp5.
      temp5-name = `xl/styles.xml`.
      temp5-content = zabaputil_cl_util_context=>conv_get_xstring_by_string( |{ lv_head }<styleSheet xmlns="{ cv_main }">{ styles }</styleSheet>| ).
      INSERT temp5
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
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

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
    DATA temp6 TYPE ltcl_write=>ty_t_row.
    DATA temp7 LIKE LINE OF temp6.
    CLEAR temp6.
    
    CLEAR temp7.
    temp7-text = `  a&b<c`.
    temp7-num = -5.
    temp7-amount = `-12.5`.
    temp7-date = `20240315`.
    temp7-time = `103000`.
    temp7-numc = `0042`.
    temp7-flag = abap_true.
    INSERT temp7 INTO TABLE temp6.
    CLEAR temp7.
    temp7-text = |x{ cl_abap_char_utilities=>form_feed }y|.
    INSERT temp7 INTO TABLE temp6.
    result = temp6.
  ENDMETHOD.

  METHOD parts_of_a_workbook.
    DATA lv_file TYPE xstring.
    DATA lv_book TYPE string.
    DATA temp3 TYPE xsdboolean.
    DATA lv_types TYPE string.
    DATA temp4 TYPE xsdboolean.
    DATA temp5 TYPE xsdboolean.
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ).

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

    
    lv_book = ltcl_file=>part( file = lv_file
                                     name = `xl/workbook.xml` ).
    
    temp3 = boolc( lv_book CS `<sheet name="Sheet1" sheetId="1" r:id="rId1"/>` ).
    cl_abap_unit_assert=>assert_true( temp3 ).
    
    lv_types = ltcl_file=>part( file = lv_file
                                      name = `[Content_Types].xml` ).
    
    temp4 = boolc( lv_types CS `PartName="/xl/worksheets/sheet1.xml"` ).
    cl_abap_unit_assert=>assert_true( temp4 ).
    
    temp5 = boolc( lv_types CS `PartName="/xl/styles.xml"` ).
    cl_abap_unit_assert=>assert_true( temp5 ).
  ENDMETHOD.

  METHOD cells_by_type.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp6 TYPE xsdboolean.
    DATA temp7 TYPE xsdboolean.
    DATA temp8 TYPE xsdboolean.
    DATA temp9 TYPE xsdboolean.
    DATA temp10 TYPE xsdboolean.
    DATA temp11 TYPE xsdboolean.
    DATA temp12 TYPE xsdboolean.
    DATA temp13 TYPE xsdboolean.
    DATA temp14 TYPE xsdboolean.
    DATA temp15 TYPE xsdboolean.
    DATA temp16 TYPE xsdboolean.
    DATA temp17 TYPE xsdboolean.
    DATA temp18 TYPE xsdboolean.
    DATA lv_styles TYPE string.
    DATA temp19 TYPE xsdboolean.
    DATA temp20 TYPE xsdboolean.
    DATA temp21 TYPE xsdboolean.
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ).
    
    lv_sheet = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " a text with the blanks in front, & and < escaped
    
    temp6 = boolc( lv_sheet CS `<c r="A2" t="inlineStr"><is><t xml:space="preserve">  a&amp;b&lt;c</t></is></c>` ).
    cl_abap_unit_assert=>assert_true( temp6 ).
    " numbers as values, with a period and the sign in front
    
    temp7 = boolc( lv_sheet CS `<c r="B2"><v>-5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp7 ).
    
    temp8 = boolc( lv_sheet CS `<c r="C2"><v>-12.50</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp8 ).
    " 2024-03-15 is day 45366, 10:30 is 0.4375 of a day - both with a format
    
    temp9 = boolc( lv_sheet CS `<c r="D2" s="1"><v>45366</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp9 ).
    
    temp10 = boolc( lv_sheet CS `<c r="E2" s="2"><v>0.4375</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp10 ).
    " a NUMC keeps its zeros as text, a flag is a boolean
    
    temp11 = boolc( lv_sheet CS `<c r="F2" t="inlineStr"><is><t xml:space="preserve">0042</t>` ).
    cl_abap_unit_assert=>assert_true( temp11 ).
    
    temp12 = boolc( lv_sheet CS `<c r="G2" t="b"><v>1</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp12 ).
    " the table column is no column: seven of them
    
    temp13 = boolc( lv_sheet CS `<dimension ref="A1:G3"/>` ).
    cl_abap_unit_assert=>assert_true( temp13 ).
    
    temp14 = boolc( lv_sheet CS `r="H1"` ).
    cl_abap_unit_assert=>assert_false( temp14 ).

    " second row: the form feed XML forbids dropped, an initial date and a
    " flag that is not set no cell at all
    
    temp15 = boolc( lv_sheet CS `<t xml:space="preserve">xy</t>` ).
    cl_abap_unit_assert=>assert_true( temp15 ).
    
    temp16 = boolc( lv_sheet CS `r="D3"` ).
    cl_abap_unit_assert=>assert_false( temp16 ).
    
    temp17 = boolc( lv_sheet CS `r="G3"` ).
    cl_abap_unit_assert=>assert_false( temp17 ).
    
    temp18 = boolc( lv_sheet CS `<c r="B3"><v>0</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp18 ).

    " the formats the cells name: the date and the time Excel has built in
    
    lv_styles = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    
    temp19 = boolc( lv_styles CS `<cellXfs count="4">` ).
    cl_abap_unit_assert=>assert_true( temp19 ).
    
    temp20 = boolc( lv_styles CS `<xf numFmtId="14" fontId="0"` ).
    cl_abap_unit_assert=>assert_true( temp20 ).
    
    temp21 = boolc( lv_styles CS `<xf numFmtId="21" fontId="0"` ).
    cl_abap_unit_assert=>assert_true( temp21 ).
  ENDMETHOD.

  METHOD header_row.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp22 TYPE xsdboolean.
    DATA lv_styles TYPE string.
    DATA temp23 TYPE xsdboolean.
    DATA temp8 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp24 TYPE xsdboolean.
    DATA temp25 TYPE xsdboolean.
    DATA temp26 TYPE xsdboolean.
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ).
    
    lv_sheet = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).
    " the component names, in the bold format
    
    temp22 = boolc( lv_sheet CS `<row r="1"><c r="A1" s="3" t="inlineStr"><is><t xml:space="preserve">TEXT</t></is></c>` ).
    cl_abap_unit_assert=>assert_true( temp22 ).
    
    lv_styles = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    
    temp23 = boolc( lv_styles CS `<xf numFmtId="0" fontId="1"` ).
    cl_abap_unit_assert=>assert_true( temp23 ).

    
    CLEAR temp8.
    temp8-no_header = abap_true.
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                    layout = temp8 ).
    lv_sheet = ltcl_file=>part( file = lv_file
                                name = `xl/worksheets/sheet1.xml` ).
    
    temp24 = boolc( lv_sheet CS `>TEXT<` ).
    cl_abap_unit_assert=>assert_false( temp24 ).
    
    temp25 = boolc( lv_sheet CS `<c r="B1"><v>-5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp25 ).
    
    temp26 = boolc( lv_sheet CS `<dimension ref="A1:G2"/>` ).
    cl_abap_unit_assert=>assert_true( temp26 ).
  ENDMETHOD.

  METHOD dates_excel_has_none.
    TYPES:
      BEGIN OF ty_s_date,
        date TYPE d,
      END OF ty_s_date.
    TYPES temp1 TYPE STANDARD TABLE OF ty_s_date WITH DEFAULT KEY.
DATA lt_date TYPE temp1.

    DATA temp9 LIKE lt_date.
    DATA temp10 LIKE LINE OF temp9.
    DATA lv_sheet TYPE string.
    DATA temp27 TYPE xsdboolean.
    DATA temp28 TYPE xsdboolean.
    DATA temp29 TYPE xsdboolean.
    DATA temp30 TYPE xsdboolean.
    DATA temp31 TYPE xsdboolean.
    CLEAR temp9.
    
    temp10-date = `18991231`.
    INSERT temp10 INTO TABLE temp9.
    temp10-date = `19000101`.
    INSERT temp10 INTO TABLE temp9.
    temp10-date = `19000228`.
    INSERT temp10 INTO TABLE temp9.
    temp10-date = `19000301`.
    INSERT temp10 INTO TABLE temp9.
    temp10-date = `99991231`.
    INSERT temp10 INTO TABLE temp9.
    lt_date = temp9.
    
    lv_sheet = ltcl_file=>part( file = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_date )
                                      name = `xl/worksheets/sheet1.xml` ).
    " before 1900 a text, then the days as Excel counts them - one ahead
    " up to the 1900-02-29 it believes in
    
    temp27 = boolc( lv_sheet CS `<c r="A2" t="inlineStr"><is><t xml:space="preserve">1899-12-31</t>` ).
    cl_abap_unit_assert=>assert_true( temp27 ).
    
    temp28 = boolc( lv_sheet CS `<c r="A3" s="1"><v>1</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp28 ).
    
    temp29 = boolc( lv_sheet CS `<c r="A4" s="1"><v>59</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp29 ).
    
    temp30 = boolc( lv_sheet CS `<c r="A5" s="1"><v>61</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp30 ).
    
    temp31 = boolc( lv_sheet CS `<c r="A6" s="1"><v>2958465</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp31 ).
  ENDMETHOD.

  METHOD layout_columns.
    DATA temp11 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp1 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp2 LIKE LINE OF temp1.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp32 TYPE xsdboolean.
    DATA temp33 TYPE xsdboolean.
    DATA temp34 TYPE xsdboolean.
    DATA temp35 TYPE xsdboolean.
    DATA temp36 TYPE xsdboolean.
    DATA lv_styles TYPE string.
    DATA temp37 TYPE xsdboolean.
    DATA temp38 TYPE xsdboolean.
    DATA temp39 TYPE xsdboolean.
    CLEAR temp11.
    
    CLEAR temp1.
    
    CLEAR temp2.
    temp2-name = `amount`.
    temp2-header = `Amount "net"`.
    temp2-format = `#,##0.00`.
    temp2-width = 20.
    INSERT temp2 INTO TABLE temp1.
    CLEAR temp2.
    temp2-name = `TEXT`.
    INSERT temp2 INTO TABLE temp1.
    CLEAR temp2.
    temp2-name = `date`.
    temp2-format = `dd.mm.yyyy`.
    INSERT temp2 INTO TABLE temp1.
    temp11-t_column = temp1.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = temp11 ).
    
    lv_sheet = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " the columns the layout names, in its order, under its headers
    
    temp32 = boolc( lv_sheet CS `<t xml:space="preserve">Amount &quot;net&quot;</t>` ).
    cl_abap_unit_assert=>assert_true( temp32 ).
    
    temp33 = boolc( lv_sheet CS `<c r="A2" s="1"><v>-12.50</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp33 ).
    
    temp34 = boolc( lv_sheet CS `<c r="C2" s="2"><v>45366</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp34 ).
    
    temp35 = boolc( lv_sheet CS `<dimension ref="A1:C3"/>` ).
    cl_abap_unit_assert=>assert_true( temp35 ).
    
    temp36 = boolc( lv_sheet CS `<col min="1" max="1" width="20" customWidth="1"/>` ).
    cl_abap_unit_assert=>assert_true( temp36 ).

    
    lv_styles = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    
    temp37 = boolc( lv_styles CS `<numFmt numFmtId="164" formatCode="#,##0.00"/>` ).
    cl_abap_unit_assert=>assert_true( temp37 ).
    
    temp38 = boolc( lv_styles CS `<numFmt numFmtId="165" formatCode="dd.mm.yyyy"/>` ).
    cl_abap_unit_assert=>assert_true( temp38 ).
    
    temp39 = boolc( lv_styles CS `<xf numFmtId="165" fontId="0"` ).
    cl_abap_unit_assert=>assert_true( temp39 ).
  ENDMETHOD.

  METHOD column_unknown.
        DATA temp12 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
        DATA temp3 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
        DATA temp4 LIKE LINE OF temp3.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp40 TYPE xsdboolean.
    TRY.
        
        CLEAR temp12.
        
        CLEAR temp3.
        
        temp4-name = `NOPE`.
        INSERT temp4 INTO TABLE temp3.
        temp12-t_column = temp3.
        zabaputil_cl_util_xlsx=>xlsx_by_itab(
            val    = rows( )
            layout = temp12 ).
        cl_abap_unit_assert=>fail( `a column the row does not have has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp40 = boolc( x->get_text( ) CS `XLSX_COLUMN_UNKNOWN` ).
        cl_abap_unit_assert=>assert_true( temp40 ).
    ENDTRY.
  ENDMETHOD.

  METHOD freeze_and_filter.
    DATA temp13 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_sheet TYPE string.
    DATA temp41 TYPE xsdboolean.
    DATA temp42 TYPE xsdboolean.
    CLEAR temp13.
    temp13-freeze_header = abap_true.
    temp13-auto_filter = abap_true.
    
    lv_sheet = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                     layout = temp13 )
        name = `xl/worksheets/sheet1.xml` ).
    
    temp41 = boolc( lv_sheet CS `<pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/>` ).
    cl_abap_unit_assert=>assert_true( temp41 ).
    
    temp42 = boolc( lv_sheet CS `</sheetData><autoFilter ref="A1:G3"/></worksheet>` ).
    cl_abap_unit_assert=>assert_true( temp42 ).
  ENDMETHOD.

  METHOD excel_table.
    DATA temp14 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp5 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp6 LIKE LINE OF temp5.
    DATA lv_file TYPE xstring.
    DATA lv_table TYPE string.
    DATA temp43 TYPE xsdboolean.
    DATA temp44 TYPE xsdboolean.
    DATA temp45 TYPE xsdboolean.
    DATA temp46 TYPE xsdboolean.
    DATA lv_sheet TYPE string.
    DATA temp47 TYPE xsdboolean.
    DATA temp48 TYPE xsdboolean.
    DATA lv_rels TYPE string.
    DATA temp49 TYPE xsdboolean.
    DATA lv_types TYPE string.
    DATA temp50 TYPE xsdboolean.
    DATA lt_none TYPE ty_t_row.
    DATA temp15 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    CLEAR temp14.
    temp14-table_style = `TableStyleMedium2`.
    
    CLEAR temp5.
    
    temp6-name = `TEXT`.
    temp6-header = `Name`.
    INSERT temp6 INTO TABLE temp5.
    temp6-name = `NUM`.
    temp6-header = `name`.
    INSERT temp6 INTO TABLE temp5.
    temp14-t_column = temp5.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( rows( )
        )->add_sheet( val    = rows( )
                      layout = temp14
        )->get_xlsx( ).

    " the table of the second sheet is table 1 of the workbook
    
    lv_table = ltcl_file=>part( file = lv_file
                                      name = `xl/tables/table1.xml` ).
    
    temp43 = boolc( lv_table CS `id="1" name="Table1" displayName="Table1" ref="A1:B3"` ).
    cl_abap_unit_assert=>assert_true( temp43 ).
    
    temp44 = boolc( lv_table CS `<tableStyleInfo name="TableStyleMedium2"` ).
    cl_abap_unit_assert=>assert_true( temp44 ).
    " a column name once - a second `name` gets a number
    
    temp45 = boolc( lv_table CS `<tableColumn id="1" name="Name"/>` ).
    cl_abap_unit_assert=>assert_true( temp45 ).
    
    temp46 = boolc( lv_table CS `<tableColumn id="2" name="name 2"/>` ).
    cl_abap_unit_assert=>assert_true( temp46 ).

    
    lv_sheet = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet2.xml` ).
    
    temp47 = boolc( lv_sheet CS `<tableParts count="1"><tablePart r:id="rId1"/></tableParts>` ).
    cl_abap_unit_assert=>assert_true( temp47 ).
    " the header as the table names its columns, and not bold: the table
    " formats it
    
    temp48 = boolc( lv_sheet CS `<c r="B1" t="inlineStr"><is><t xml:space="preserve">name 2</t></is></c>` ).
    cl_abap_unit_assert=>assert_true( temp48 ).

    
    lv_rels = ltcl_file=>part( file = lv_file
                                     name = `xl/worksheets/_rels/sheet2.xml.rels` ).
    
    temp49 = boolc( lv_rels CS `Target="../tables/table1.xml"` ).
    cl_abap_unit_assert=>assert_true( temp49 ).
    
    lv_types = ltcl_file=>part( file = lv_file
                                      name = `[Content_Types].xml` ).
    
    temp50 = boolc( lv_types CS `PartName="/xl/tables/table1.xml"` ).
    cl_abap_unit_assert=>assert_true( temp50 ).

    " no row, no table: Excel takes none without one
    
    
    CLEAR temp15.
    temp15-table_style = `TableStyleMedium2`.
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = lt_none
                                                    layout = temp15 ).
    cl_abap_unit_assert=>assert_false( ltcl_file=>has_part( file = lv_file
                                                            name = `xl/tables/table1.xml` ) ).
  ENDMETHOD.

  METHOD excel_table_options.
    DATA temp16 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp7 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp1 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_file TYPE xstring.
    DATA lv_table TYPE string.
    DATA temp51 TYPE xsdboolean.
    DATA temp52 TYPE xsdboolean.
    DATA temp53 TYPE xsdboolean.
    DATA temp54 TYPE xsdboolean.
    DATA temp55 TYPE xsdboolean.
    DATA temp56 TYPE xsdboolean.
    DATA temp57 TYPE xsdboolean.
    CLEAR temp16.
    temp16-table_style = `TableStyleLight9`.
    temp16-table_name = `My Orders 2024`.
    temp16-no_row_stripes = abap_true.
    temp16-column_stripes = abap_true.
    temp16-no_filter = abap_true.
    
    CLEAR temp7.
    temp7-table_style = `TableStyleLight9`.
    temp7-table_name = `myorders2024`.
    
    CLEAR temp1.
    temp1-table_style = `TableStyleLight9`.
    temp1-table_name = `A1`.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      layout = temp16
        )->add_sheet( val    = rows( )
                      layout = temp7
        )->add_sheet( val    = rows( )
                      layout = temp1
        )->get_xlsx( ).

    " the name without what Excel does not take in one, no filter, the
    " stripes as asked for
    
    lv_table = ltcl_file=>part( file = lv_file
                                      name = `xl/tables/table1.xml` ).
    
    temp51 = boolc( lv_table CS `name="MyOrders2024" displayName="MyOrders2024"` ).
    cl_abap_unit_assert=>assert_true( temp51 ).
    
    temp52 = boolc( lv_table CS `<autoFilter` ).
    cl_abap_unit_assert=>assert_false( temp52 ).
    
    temp53 = boolc( lv_table CS `showRowStripes="0" showColumnStripes="1"` ).
    cl_abap_unit_assert=>assert_true( temp53 ).

    " a name the workbook has, whatever its case, gets a number - and one
    " that reads as a cell an underscore
    lv_table = ltcl_file=>part( file = lv_file
                                name = `xl/tables/table2.xml` ).
    
    temp54 = boolc( lv_table CS `name="myorders2024_2"` ).
    cl_abap_unit_assert=>assert_true( temp54 ).
    
    temp55 = boolc( lv_table CS `<autoFilter ref="A1:G3"/>` ).
    cl_abap_unit_assert=>assert_true( temp55 ).
    
    temp56 = boolc( lv_table CS `showRowStripes="1" showColumnStripes="0"` ).
    cl_abap_unit_assert=>assert_true( temp56 ).
    lv_table = ltcl_file=>part( file = lv_file
                                name = `xl/tables/table3.xml` ).
    
    temp57 = boolc( lv_table CS `name="_A1"` ).
    cl_abap_unit_assert=>assert_true( temp57 ).
  ENDMETHOD.

  METHOD start_cell.
    DATA temp17 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_sheet TYPE string.
    DATA temp58 TYPE xsdboolean.
    DATA temp59 TYPE xsdboolean.
    DATA temp60 TYPE xsdboolean.
    DATA temp61 TYPE xsdboolean.
    DATA temp62 TYPE xsdboolean.
    DATA temp63 TYPE xsdboolean.
    DATA temp64 TYPE xsdboolean.
    DATA temp18 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_table TYPE string.
    DATA temp65 TYPE xsdboolean.
    CLEAR temp17.
    temp17-start_cell = `b3`.
    temp17-freeze_header = abap_true.
    temp17-auto_filter = abap_true.
    
    lv_sheet = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                     layout = temp17 )
        name = `xl/worksheets/sheet1.xml` ).

    " the header in B3, the rows under it, the columns from B on
    
    temp58 = boolc( lv_sheet CS `<dimension ref="B3:H5"/>` ).
    cl_abap_unit_assert=>assert_true( temp58 ).
    
    temp59 = boolc( lv_sheet CS `<row r="3"><c r="B3" s="3" t="inlineStr">` ).
    cl_abap_unit_assert=>assert_true( temp59 ).
    
    temp60 = boolc( lv_sheet CS `<c r="C4"><v>-5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp60 ).
    
    temp61 = boolc( lv_sheet CS `<col min="2" max="2"` ).
    cl_abap_unit_assert=>assert_true( temp61 ).
    
    temp62 = boolc( lv_sheet CS `<col min="1" max="1"` ).
    cl_abap_unit_assert=>assert_false( temp62 ).
    " what is frozen ends with the header row, and the filter is on it
    
    temp63 = boolc( lv_sheet CS `<pane ySplit="3" topLeftCell="A4"` ).
    cl_abap_unit_assert=>assert_true( temp63 ).
    
    temp64 = boolc( lv_sheet CS `<autoFilter ref="B3:H5"/>` ).
    cl_abap_unit_assert=>assert_true( temp64 ).

    " an Excel table stands where the rows stand
    
    CLEAR temp18.
    temp18-start_cell = `$C$2`.
    temp18-table_style = `TableStyleMedium2`.
    
    lv_table = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                     layout = temp18 )
        name = `xl/tables/table1.xml` ).
    
    temp65 = boolc( lv_table CS ` ref="C2:I4"` ).
    cl_abap_unit_assert=>assert_true( temp65 ).
  ENDMETHOD.

  METHOD start_cell_invalid.
        DATA temp19 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp66 TYPE xsdboolean.
        DATA temp20 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    TRY.
        
        CLEAR temp19.
        temp19-start_cell = `here`.
        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                              layout = temp19 ).
        cl_abap_unit_assert=>fail( `a start cell that is none has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp66 = boolc( x->get_text( ) CS `XLSX_START_CELL` ).
        cl_abap_unit_assert=>assert_true( temp66 ).
    ENDTRY.
    TRY.
        
        CLEAR temp20.
        temp20-start_cell = `B0`.
        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                              layout = temp20 ).
        cl_abap_unit_assert=>fail( `row 0 has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD totals_row.
    DATA temp21 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp8 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp9 LIKE LINE OF temp8.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp67 TYPE xsdboolean.
    DATA temp68 TYPE xsdboolean.
    DATA temp69 TYPE xsdboolean.
    DATA temp70 TYPE xsdboolean.
    DATA temp71 TYPE xsdboolean.
    DATA temp72 TYPE xsdboolean.
    DATA temp73 TYPE xsdboolean.
    DATA temp74 TYPE xsdboolean.
    DATA temp75 TYPE xsdboolean.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    FIELD-SYMBOLS <temp22> LIKE LINE OF lt_cell.
    DATA temp23 LIKE sy-tabix.
    DATA lt_none TYPE ty_t_row.
    DATA temp24 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp10 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp11 LIKE LINE OF temp10.
    DATA temp76 TYPE xsdboolean.
    CLEAR temp21.
    temp21-auto_filter = abap_true.
    
    CLEAR temp8.
    
    CLEAR temp9.
    temp9-name = `TEXT`.
    temp9-total = `Total`.
    INSERT temp9 INTO TABLE temp8.
    CLEAR temp9.
    temp9-name = `NUM`.
    temp9-total = `SUM`.
    INSERT temp9 INTO TABLE temp8.
    CLEAR temp9.
    temp9-name = `AMOUNT`.
    temp9-total = `average`.
    temp9-format = `0.00`.
    INSERT temp9 INTO TABLE temp8.
    CLEAR temp9.
    temp9-name = `DATE`.
    temp9-total = `count`.
    INSERT temp9 INTO TABLE temp8.
    CLEAR temp9.
    temp9-name = `NUMC`.
    temp9-total = `max`.
    INSERT temp9 INTO TABLE temp8.
    temp21-t_column = temp8.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = temp21 ).
    
    lv_sheet = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " a row under the rows: the label bold, a formula over the cells of
    " the column with the value it gives - -5 and 0, -12.5 and 0
    
    temp67 = boolc( lv_sheet CS `<dimension ref="A1:E4"/>` ).
    cl_abap_unit_assert=>assert_true( temp67 ).
    
    temp68 = boolc( lv_sheet CS `<row r="4"><c r="A4" s="3" t="inlineStr"><is><t xml:space="preserve">Total</t></is></c>` ).
    cl_abap_unit_assert=>assert_true( temp68 ).
    
    temp69 = boolc( lv_sheet CS `<c r="B4"><f>SUBTOTAL(109,B2:B3)</f><v>-5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp69 ).
    
    temp70 = boolc( lv_sheet CS `<c r="C4" s="1"><f>SUBTOTAL(101,C2:C3)</f><v>-6.25</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp70 ).
    " a count is a number, not a date: the cells that hold a value, one
    
    temp71 = boolc( lv_sheet CS `<c r="D4"><f>SUBTOTAL(103,D2:D3)</f><v>1</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp71 ).
    " a NUMC is text: nothing to add up here, the formula stands
    
    temp72 = boolc( lv_sheet CS `<c r="E4"><f>SUBTOTAL(104,E2:E3)</f>` ).
    cl_abap_unit_assert=>assert_true( temp72 ).
    " the filter ends above the totals row
    
    temp73 = boolc( lv_sheet CS `<autoFilter ref="A1:E3"/>` ).
    cl_abap_unit_assert=>assert_true( temp73 ).

    " Excel is asked to work the formulas out when it opens the file
    
    temp74 = boolc( ltcl_file=>part( file = lv_file name = `xl/workbook.xml` ) CS `</sheets><calcPr fullCalcOnLoad="1"/></workbook>` ).
    cl_abap_unit_assert=>assert_true( temp74 ).
    
    temp75 = boolc( ltcl_file=>part( file = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ) name = `xl/workbook.xml` ) CS `calcPr` ).
    cl_abap_unit_assert=>assert_false( temp75 ).

    " read back, a totals cell holds the value that was written with it
    
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_cells( ).
    
    
    temp23 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 4 col = 3 ASSIGNING <temp22>.
    sy-tabix = temp23.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `-6.25`
                                        act = <temp22>-value ).

    " no row, no totals row
    
    
    CLEAR temp24.
    
    CLEAR temp10.
    
    temp11-name = `NUM`.
    temp11-total = `sum`.
    INSERT temp11 INTO TABLE temp10.
    temp24-t_column = temp10.
    lv_sheet = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
                   val    = lt_none
                   layout = temp24 )
        name = `xl/worksheets/sheet1.xml` ).
    
    temp76 = boolc( lv_sheet CS `SUBTOTAL` ).
    cl_abap_unit_assert=>assert_false( temp76 ).
  ENDMETHOD.

  METHOD totals_row_of_table.
    DATA temp25 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp12 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp13 LIKE LINE OF temp12.
    DATA lv_file TYPE xstring.
    DATA lv_table TYPE string.
    DATA temp77 TYPE xsdboolean.
    DATA temp78 TYPE xsdboolean.
    DATA temp79 TYPE xsdboolean.
    DATA temp80 TYPE xsdboolean.
    DATA lv_sheet TYPE string.
    DATA temp81 TYPE xsdboolean.
    DATA temp82 TYPE xsdboolean.
    DATA temp83 TYPE xsdboolean.
    CLEAR temp25.
    temp25-table_style = `TableStyleMedium2`.
    temp25-table_name = `Orders`.
    
    CLEAR temp12.
    
    CLEAR temp13.
    temp13-name = `TEXT`.
    temp13-header = `Name`.
    temp13-total = `Total`.
    INSERT temp13 INTO TABLE temp12.
    CLEAR temp13.
    temp13-name = `NUM`.
    temp13-header = `Qty [pcs] #1`.
    temp13-total = `sum`.
    INSERT temp13 INTO TABLE temp12.
    CLEAR temp13.
    temp13-name = `AMOUNT`.
    temp13-total = `min`.
    INSERT temp13 INTO TABLE temp12.
    temp25-t_column = temp12.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = temp25 ).

    " the table reaches over the totals row, its filter does not
    
    lv_table = ltcl_file=>part( file = lv_file
                                      name = `xl/tables/table1.xml` ).
    
    temp77 = boolc( lv_table CS ` ref="A1:C4" totalsRowCount="1"><autoFilter ref="A1:C3"/>` ).
    cl_abap_unit_assert=>assert_true( temp77 ).
    
    temp78 = boolc( lv_table CS `<tableColumn id="1" name="Name" totalsRowLabel="Total"/>` ).
    cl_abap_unit_assert=>assert_true( temp78 ).
    
    temp79 = boolc( lv_table CS `name="Qty [pcs] #1" totalsRowFunction="sum"/>` ).
    cl_abap_unit_assert=>assert_true( temp79 ).
    
    temp80 = boolc( lv_table CS `name="AMOUNT" totalsRowFunction="min"/>` ).
    cl_abap_unit_assert=>assert_true( temp80 ).

    " the formula names the column of the table - with what a reference
    " reads as its own syntax marked - and the label is not bold: the
    " table formats its totals row
    
    lv_sheet = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).
    
    temp81 = boolc( lv_sheet CS `<c r="A4" t="inlineStr"><is><t xml:space="preserve">Total</t></is></c>` ).
    cl_abap_unit_assert=>assert_true( temp81 ).
    
    temp82 = boolc( lv_sheet CS `<c r="B4"><f>SUBTOTAL(109,Orders[Qty '[pcs'] '#1])</f><v>-5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp82 ).
    
    temp83 = boolc( lv_sheet CS `<c r="C4"><f>SUBTOTAL(105,Orders[AMOUNT])</f><v>-12.5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp83 ).
  ENDMETHOD.

  METHOD formula_column.
    DATA temp26 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp14 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp15 LIKE LINE OF temp14.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp84 TYPE xsdboolean.
    DATA temp85 TYPE xsdboolean.
    DATA temp86 TYPE xsdboolean.
    DATA temp87 TYPE xsdboolean.
    DATA temp88 TYPE xsdboolean.
    DATA temp89 TYPE xsdboolean.
    DATA temp90 TYPE xsdboolean.
    DATA lt_back TYPE ty_t_row.
    FIELD-SYMBOLS <temp27> LIKE LINE OF lt_back.
    DATA temp28 LIKE sy-tabix.
    CLEAR temp26.
    
    CLEAR temp14.
    
    CLEAR temp15.
    temp15-name = `NUM`.
    INSERT temp15 INTO TABLE temp14.
    CLEAR temp15.
    temp15-name = `AMOUNT`.
    INSERT temp15 INTO TABLE temp14.
    CLEAR temp15.
    temp15-header = `Value`.
    temp15-formula = `=A{row}*B{row}`.
    temp15-format = `0.00`.
    temp15-total = `sum`.
    INSERT temp15 INTO TABLE temp14.
    CLEAR temp15.
    temp15-formula = `IF(A{row}<0,"neg","")`.
    INSERT temp15 INTO TABLE temp14.
    temp26-t_column = temp14.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = temp26 ).
    
    lv_sheet = ltcl_file=>part( file = lv_file
                                      name = `xl/worksheets/sheet1.xml` ).

    " every cell of the column the formula for its row, without a value
    
    temp84 = boolc( lv_sheet CS `>Value</t>` ).
    cl_abap_unit_assert=>assert_true( temp84 ).
    
    temp85 = boolc( lv_sheet CS `<c r="C2" s="1"><f>A2*B2</f></c>` ).
    cl_abap_unit_assert=>assert_true( temp85 ).
    
    temp86 = boolc( lv_sheet CS `<c r="C3" s="1"><f>A3*B3</f></c>` ).
    cl_abap_unit_assert=>assert_true( temp86 ).
    
    temp87 = boolc( lv_sheet CS `<c r="D2"><f>IF(A2&lt;0,&quot;neg&quot;,&quot;&quot;)</f></c>` ).
    cl_abap_unit_assert=>assert_true( temp87 ).
    " a column without a name and without a header is named by its place
    
    temp88 = boolc( lv_sheet CS `>Column4</t>` ).
    cl_abap_unit_assert=>assert_true( temp88 ).
    " its total is a formula too, and nothing was added up for it here
    
    temp89 = boolc( lv_sheet CS `<c r="C4" s="1"><f>SUBTOTAL(109,C2:C3)</f></c>` ).
    cl_abap_unit_assert=>assert_true( temp89 ).
    
    temp90 = boolc( ltcl_file=>part( file = lv_file name = `xl/workbook.xml` ) CS `fullCalcOnLoad` ).
    cl_abap_unit_assert=>assert_true( temp90 ).

    " read back, a formula without a value is no cell - the columns with
    " values are read as before
    
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    
    
    temp28 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp27>.
    sy-tabix = temp28.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = -5
                                        act = <temp27>-num ).
  ENDMETHOD.

  METHOD sheet_names.
    DATA lt_none TYPE ty_t_row.

    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA lt_name TYPE string_table.
    FIELD-SYMBOLS <temp29> LIKE LINE OF lt_name.
    DATA temp30 LIKE sy-tabix.
    FIELD-SYMBOLS <temp31> LIKE LINE OF lt_name.
    DATA temp32 LIKE sy-tabix.
    FIELD-SYMBOLS <temp33> LIKE LINE OF lt_name.
    DATA temp34 LIKE sy-tabix.
    FIELD-SYMBOLS <temp35> LIKE LINE OF lt_name.
    DATA temp36 LIKE sy-tabix.
    DATA lv_book TYPE string.
    DATA temp91 TYPE xsdboolean.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val  = lt_none
                      name = `Q1/Q2: [plan]`
        )->add_sheet( val  = lt_none
                      name = `q1_q2_ _plan_`
        )->add_sheet( lt_none
        )->add_sheet( val  = lt_none
                      name = `a name that is longer than Excel takes it` ).

    
    lt_name = lo_xlsx->get_sheet_names( ).
    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_name ) ).
    
    
    temp30 = sy-tabix.
    READ TABLE lt_name INDEX 1 ASSIGNING <temp29>.
    sy-tabix = temp30.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Q1_Q2_ _plan_`
                                        act = <temp29> ).
    
    
    temp32 = sy-tabix.
    READ TABLE lt_name INDEX 2 ASSIGNING <temp31>.
    sy-tabix = temp32.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `q1_q2_ _plan_ (2)`
                                        act = <temp31> ).
    
    
    temp34 = sy-tabix.
    READ TABLE lt_name INDEX 3 ASSIGNING <temp33>.
    sy-tabix = temp34.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Sheet3`
                                        act = <temp33> ).
    
    
    temp36 = sy-tabix.
    READ TABLE lt_name INDEX 4 ASSIGNING <temp35>.
    sy-tabix = temp36.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a name that is longer than Exce`
                                        act = <temp35> ).

    
    lv_book = ltcl_file=>part( file = lo_xlsx->get_xlsx( )
                                     name = `xl/workbook.xml` ).
    
    temp91 = boolc( lv_book CS `<sheet name="Sheet3" sheetId="3" r:id="rId3"/>` ).
    cl_abap_unit_assert=>assert_true( temp91 ).
  ENDMETHOD.

  METHOD empty_workbook.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA lt_name TYPE string_table.
    FIELD-SYMBOLS <temp37> LIKE LINE OF lt_name.
    DATA temp38 LIKE sy-tabix.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx( zabaputil_cl_util_xlsx=>factory( )->get_xlsx( ) ).
    
    lt_name = lo_xlsx->get_sheet_names( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_name ) ).
    
    
    temp38 = sy-tabix.
    READ TABLE lt_name INDEX 1 ASSIGNING <temp37>.
    sy-tabix = temp38.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Sheet1`
                                        act = <temp37> ).
    cl_abap_unit_assert=>assert_initial( lo_xlsx->get_cells( ) ).
  ENDMETHOD.

  METHOD no_columns.
    " a row without an elementary component: a sheet without a cell
    TYPES temp2 TYPE STANDARD TABLE OF string_table WITH DEFAULT KEY.
DATA lt_deep TYPE temp2.
    DATA temp39 TYPE string_table.
    DATA lv_sheet TYPE string.
    DATA temp92 TYPE xsdboolean.
    DATA temp93 TYPE xsdboolean.
    CLEAR temp39.
    INSERT `a` INTO TABLE temp39.
    INSERT temp39 INTO TABLE lt_deep.
    
    lv_sheet = ltcl_file=>part( file = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_deep )
                                      name = `xl/worksheets/sheet1.xml` ).
    
    temp92 = boolc( lv_sheet CS `<dimension ref="A1"/>` ).
    cl_abap_unit_assert=>assert_true( temp92 ).
    
    temp93 = boolc( lv_sheet CS `<cols>` ).
    cl_abap_unit_assert=>assert_false( temp93 ).
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
"! WRITING A SHEET OF MORE THAN ONE TABLE: free cells, styles, links,
"! validations, names, printing, properties
"! ================================================================
CLASS ltcl_write_sheet DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_row,
        text TYPE string,
        num  TYPE i,
        url  TYPE string,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

    METHODS rows
      RETURNING
        VALUE(result) TYPE ty_t_row.

    METHODS sheet
      IMPORTING
        file          TYPE xstring
        index         TYPE i DEFAULT 1
      RETURNING
        VALUE(result) TYPE string.

    METHODS title_table_footer   FOR TESTING RAISING cx_static_check.
    METHODS two_tables           FOR TESTING RAISING cx_static_check.
    METHODS cell_taken           FOR TESTING RAISING cx_static_check.
    METHODS no_sheet             FOR TESTING RAISING cx_static_check.
    METHODS cell_by_type         FOR TESTING RAISING cx_static_check.
    METHODS rows_and_columns     FOR TESTING RAISING cx_static_check.
    METHODS merged_cells         FOR TESTING RAISING cx_static_check.
    METHODS styles               FOR TESTING RAISING cx_static_check.
    METHODS style_invalid        FOR TESTING RAISING cx_static_check.
    METHODS links                FOR TESTING RAISING cx_static_check.
    METHODS validations          FOR TESTING RAISING cx_static_check.
    METHODS names                FOR TESTING RAISING cx_static_check.
    METHODS printing             FOR TESTING RAISING cx_static_check.
    METHODS sheet_view           FOR TESTING RAISING cx_static_check.
    METHODS all_hidden           FOR TESTING RAISING cx_static_check.
    METHODS properties           FOR TESTING RAISING cx_static_check.
    METHODS calculated_column    FOR TESTING RAISING cx_static_check.
    METHODS read_under_a_title   FOR TESTING RAISING cx_static_check.
    METHODS rules                FOR TESTING RAISING cx_static_check.
    METHODS protection           FOR TESTING RAISING cx_static_check.
    METHODS comments             FOR TESTING RAISING cx_static_check.
    METHODS images               FOR TESTING RAISING cx_static_check.
    METHODS shared_strings       FOR TESTING RAISING cx_static_check.
    METHODS conversion_exit      FOR TESTING RAISING cx_static_check.
    METHODS currency_and_unit    FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_write_sheet IMPLEMENTATION.

  METHOD rows.
    DATA temp41 TYPE ltcl_write_sheet=>ty_t_row.
    DATA temp42 LIKE LINE OF temp41.
    CLEAR temp41.
    
    temp42-text = `a`.
    temp42-num = 1.
    temp42-url = `https://example.com/a?x=1&y=2`.
    INSERT temp42 INTO TABLE temp41.
    temp42-text = `b`.
    temp42-num = 2.
    temp42-url = `https://example.com/a?x=1&y=2`.
    INSERT temp42 INTO TABLE temp41.
    temp42-text = `c`.
    temp42-num = 3.
    temp42-url = `#Data!A1`.
    INSERT temp42 INTO TABLE temp41.
    result = temp41.
  ENDMETHOD.

  METHOD sheet.
    result = ltcl_file=>part( file = file
                              name = |xl/worksheets/sheet{ index }.xml| ).
  ENDMETHOD.

  METHOD title_table_footer.
    DATA temp43 TYPE zabaputil_cl_util_xlsx=>ty_s_style.
    DATA temp16 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp94 TYPE xsdboolean.
    DATA temp95 TYPE xsdboolean.
    DATA temp96 TYPE xsdboolean.
    DATA temp97 TYPE xsdboolean.
    DATA temp98 TYPE xsdboolean.
    DATA temp99 TYPE xsdboolean.
    DATA temp100 TYPE xsdboolean.
    DATA lv_styles TYPE string.
    DATA temp101 TYPE xsdboolean.
    CLEAR temp43.
    temp43-bold = abap_true.
    temp43-size = 14.
    
    CLEAR temp16.
    temp16-start_cell = `A3`.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( name = `Report`
        )->set_cell( row   = 1
                     col   = 1
                     val   = `Orders 2024`
                     style = temp43
        )->add_table( val    = rows( )
                      layout = temp16
        )->set_cell( row = 8
                     col = 1
                     val = `as of today`
        )->get_xlsx( ).
    
    lv_sheet = sheet( lv_file ).

    " the title in row 1, the table from row 3, the footer under it - each
    " row once and in its order
    
    temp94 = boolc( lv_sheet CS `<sheetData><row r="1"><c r="A1" s="` ).
    cl_abap_unit_assert=>assert_true( temp94 ).
    
    temp95 = boolc( lv_sheet CS `>Orders 2024</t>` ).
    cl_abap_unit_assert=>assert_true( temp95 ).
    
    temp96 = boolc( lv_sheet CS `</row><row r="3"><c r="A3"` ).
    cl_abap_unit_assert=>assert_true( temp96 ).
    
    temp97 = boolc( lv_sheet CS `<row r="6"><c r="A6"` ).
    cl_abap_unit_assert=>assert_true( temp97 ).
    
    temp98 = boolc( lv_sheet CS `</row><row r="8"><c r="A8" t="inlineStr">` ).
    cl_abap_unit_assert=>assert_true( temp98 ).
    
    temp99 = boolc( lv_sheet CS `<dimension ref="A1:C8"/>` ).
    cl_abap_unit_assert=>assert_true( temp99 ).
    " the title does not widen the column, the table does
    
    temp100 = boolc( lv_sheet CS `<col min="1" max="1" width="9"` ).
    cl_abap_unit_assert=>assert_true( temp100 ).

    
    lv_styles = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    
    temp101 = boolc( lv_styles CS `<font><b/><sz val="14"/>` ).
    cl_abap_unit_assert=>assert_true( temp101 ).
  ENDMETHOD.

  METHOD two_tables.
    DATA temp44 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp17 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp102 TYPE xsdboolean.
    DATA temp103 TYPE xsdboolean.
    DATA temp104 TYPE xsdboolean.
    DATA temp105 TYPE xsdboolean.
    DATA lv_rels TYPE string.
    DATA temp106 TYPE xsdboolean.
    DATA temp107 TYPE xsdboolean.
    DATA temp108 TYPE xsdboolean.
    CLEAR temp44.
    temp44-table_style = `TableStyleLight9`.
    
    CLEAR temp17.
    temp17-start_cell = `E1`.
    temp17-table_style = `TableStyleLight9`.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      layout = temp44
        )->add_table( val    = rows( )
                      layout = temp17
        )->get_xlsx( ).
    
    lv_sheet = sheet( lv_file ).

    " one row element, the cells of both tables in the order of their
    " columns
    
    temp102 = boolc( lv_sheet CS `<v>1</v></c><c r="C2" t="inlineStr">` ).
    cl_abap_unit_assert=>assert_true( temp102 ).
    
    temp103 = boolc( lv_sheet CS `</c><c r="E2" t="inlineStr">` ).
    cl_abap_unit_assert=>assert_true( temp103 ).
    
    temp104 = boolc( lv_sheet CS `<dimension ref="A1:G4"/>` ).
    cl_abap_unit_assert=>assert_true( temp104 ).
    
    temp105 = boolc( lv_sheet CS `<tableParts count="2"><tablePart r:id="rId1"/><tablePart r:id="rId2"/></tableParts>` ).
    cl_abap_unit_assert=>assert_true( temp105 ).
    
    lv_rels = ltcl_file=>part( file = lv_file
                                     name = `xl/worksheets/_rels/sheet1.xml.rels` ).
    
    temp106 = boolc( lv_rels CS `Id="rId2" Type=` ).
    cl_abap_unit_assert=>assert_true( temp106 ).
    
    temp107 = boolc( lv_rels CS `Target="../tables/table2.xml"` ).
    cl_abap_unit_assert=>assert_true( temp107 ).
    
    temp108 = boolc( ltcl_file=>part( file = lv_file name = `xl/tables/table2.xml` ) CS `name="Table2" displayName="Table2" ref="E1:G4"` ).
    cl_abap_unit_assert=>assert_true( temp108 ).
  ENDMETHOD.

  METHOD cell_taken.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp109 TYPE xsdboolean.
        DATA temp45 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
        DATA temp110 TYPE xsdboolean.
    DATA temp46 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
        DATA temp111 TYPE xsdboolean.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory( )->add_sheet( rows( ) ).
    TRY.
        lo_xlsx->set_cell( row = 2
                           col = 2
                           val = 5 ).
        cl_abap_unit_assert=>fail( `a cell of the table has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp109 = boolc( x->get_text( ) CS `XLSX_CELL_TAKEN` ).
        cl_abap_unit_assert=>assert_true( temp109 ).
    ENDTRY.
    TRY.
        
        CLEAR temp45.
        temp45-start_cell = `C4`.
        lo_xlsx->add_table( val    = rows( )
                            layout = temp45 ).
        cl_abap_unit_assert=>fail( `a table over a table has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        
        temp110 = boolc( x->get_text( ) CS `XLSX_CELL_TAKEN` ).
        cl_abap_unit_assert=>assert_true( temp110 ).
    ENDTRY.
    " right of it and under it is free
    lo_xlsx->set_cell( row = 2
                       col = 4
                       val = 5 ).
    
    CLEAR temp46.
    temp46-start_cell = `A5`.
    lo_xlsx->add_table( val    = rows( )
                        layout = temp46 ).
    TRY.
        lo_xlsx->set_cell( row = 0
                           col = 1
                           val = 5 ).
        cl_abap_unit_assert=>fail( `row 0 has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        
        temp111 = boolc( x->get_text( ) CS `XLSX_CELL` ).
        cl_abap_unit_assert=>assert_true( temp111 ).
    ENDTRY.
  ENDMETHOD.

  METHOD no_sheet.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp112 TYPE xsdboolean.
    TRY.
        zabaputil_cl_util_xlsx=>factory( )->set_cell( row = 1
                                                      col = 1
                                                      val = 1 ).
        cl_abap_unit_assert=>fail( `a cell without a sheet has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp112 = boolc( x->get_text( ) CS `XLSX_NO_SHEET` ).
        cl_abap_unit_assert=>assert_true( temp112 ).
    ENDTRY.
  ENDMETHOD.

  METHOD cell_by_type.
    DATA lv_date   TYPE d VALUE '20240315'.
    DATA lv_amount TYPE p LENGTH 8 DECIMALS 2 VALUE '-12.50'.

    DATA temp47 TYPE zabaputil_cl_util_xlsx=>ty_s_style.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp113 TYPE xsdboolean.
    DATA temp114 TYPE xsdboolean.
    DATA temp115 TYPE xsdboolean.
    DATA temp116 TYPE xsdboolean.
    DATA temp117 TYPE xsdboolean.
    DATA temp118 TYPE xsdboolean.
    DATA temp119 TYPE xsdboolean.
    CLEAR temp47.
    temp47-fill = `FFFF00`.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet(
        )->set_cell( row = 1
                     col = 1
                     val = 42
        )->set_cell( row    = 2
                     col    = 1
                     val    = lv_amount
                     format = `#,##0.00`
        )->set_cell( row = 3
                     col = 1
                     val = lv_date
        )->set_cell( row = 4
                     col = 1
                     val = abap_true
        )->set_cell( row     = 5
                     col     = 1
                     formula = `=SUM(A1:A2)`
        )->set_cell( row   = 5
                     col   = 2
                     style = temp47
        )->get_xlsx( ).
    
    lv_sheet = sheet( lv_file ).

    
    temp113 = boolc( lv_sheet CS `<c r="A1"><v>42</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp113 ).
    
    temp114 = boolc( lv_sheet CS `<c r="A2" s="1"><v>-12.50</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp114 ).
    
    temp115 = boolc( lv_sheet CS `<c r="A3" s="2"><v>45366</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp115 ).
    " abap_true is of type abap_bool: a boolean
    
    temp116 = boolc( lv_sheet CS `<c r="A4" t="b"><v>1</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp116 ).
    
    temp117 = boolc( lv_sheet CS `<c r="A5"><f>SUM(A1:A2)</f></c>` ).
    cl_abap_unit_assert=>assert_true( temp117 ).
    " a cell without a value, with a fill
    
    temp118 = boolc( lv_sheet CS `<c r="B5" s="3"/>` ).
    cl_abap_unit_assert=>assert_true( temp118 ).
    
    temp119 = boolc( ltcl_file=>part( file = lv_file name = `xl/workbook.xml` ) CS `fullCalcOnLoad` ).
    cl_abap_unit_assert=>assert_true( temp119 ).
  ENDMETHOD.

  METHOD rows_and_columns.
    DATA temp48 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp18 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp19 LIKE LINE OF temp18.
    DATA lv_sheet TYPE string.
    DATA temp120 TYPE xsdboolean.
    DATA temp121 TYPE xsdboolean.
    DATA temp122 TYPE xsdboolean.
    DATA temp123 TYPE xsdboolean.
    DATA temp124 TYPE xsdboolean.
    DATA temp125 TYPE xsdboolean.
    DATA temp126 TYPE xsdboolean.
    DATA temp127 TYPE xsdboolean.
    CLEAR temp48.
    temp48-row_height = 20.
    temp48-outline_level = 1.
    
    CLEAR temp18.
    
    CLEAR temp19.
    temp19-name = `TEXT`.
    INSERT temp19 INTO TABLE temp18.
    CLEAR temp19.
    temp19-name = `NUM`.
    temp19-hidden = abap_true.
    INSERT temp19 INTO TABLE temp18.
    CLEAR temp19.
    temp19-name = `URL`.
    temp19-outline_level = 2.
    temp19-width = 40.
    INSERT temp19 INTO TABLE temp18.
    temp48-t_column = temp18.
    
    lv_sheet = sheet( zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      layout = temp48
        )->set_row( row    = 10
                    height = 30
        )->set_row( row    = 2
                    hidden = abap_true
        )->set_column( col   = 6
                       width = 12
        )->get_xlsx( ) ).

    " the data rows high and one level down, the header not
    
    temp120 = boolc( lv_sheet CS `<row r="1"><c` ).
    cl_abap_unit_assert=>assert_true( temp120 ).
    
    temp121 = boolc( lv_sheet CS `<row r="3" ht="20" customHeight="1" outlineLevel="1">` ).
    cl_abap_unit_assert=>assert_true( temp121 ).
    " set_row( ) decides for the row it names
    
    temp122 = boolc( lv_sheet CS `<row r="2" hidden="1">` ).
    cl_abap_unit_assert=>assert_true( temp122 ).
    
    temp123 = boolc( lv_sheet CS `<row r="10" ht="30" customHeight="1"></row>` ).
    cl_abap_unit_assert=>assert_true( temp123 ).
    
    temp124 = boolc( lv_sheet CS `<sheetFormatPr defaultRowHeight="15" outlineLevelRow="1" outlineLevelCol="2"/>` ).
    cl_abap_unit_assert=>assert_true( temp124 ).
    
    temp125 = boolc( lv_sheet CS `<col min="2" max="2" width="8" customWidth="1" hidden="1"/>` ).
    cl_abap_unit_assert=>assert_true( temp125 ).
    
    temp126 = boolc( lv_sheet CS `<col min="3" max="3" width="40" customWidth="1" outlineLevel="2"/>` ).
    cl_abap_unit_assert=>assert_true( temp126 ).
    
    temp127 = boolc( lv_sheet CS `<col min="6" max="6" width="12" customWidth="1"/></cols>` ).
    cl_abap_unit_assert=>assert_true( temp127 ).
  ENDMETHOD.

  METHOD merged_cells.
    DATA temp49 TYPE zabaputil_cl_util_xlsx=>ty_s_style.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp128 TYPE xsdboolean.
        DATA temp129 TYPE xsdboolean.
    DATA lv_sheet TYPE string.
    DATA temp130 TYPE xsdboolean.
    CLEAR temp49.
    temp49-halign = `center`.
    
    lo_xlsx = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet(
        )->set_cell( row   = 1
                     col   = 1
                     val   = `Title`
                     style = temp49
        )->merge( `A1:D1`
        )->merge( `$F$2:F$5` ).
    TRY.
        lo_xlsx->merge( `C1:C3` ).
        cl_abap_unit_assert=>fail( `an overlap has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp128 = boolc( x->get_text( ) CS `XLSX_MERGE` ).
        cl_abap_unit_assert=>assert_true( temp128 ).
    ENDTRY.
    TRY.
        lo_xlsx->merge( `B7` ).
        cl_abap_unit_assert=>fail( `one cell has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        lo_xlsx->merge( `A1-D1` ).
        cl_abap_unit_assert=>fail( `no range has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        
        temp129 = boolc( x->get_text( ) CS `XLSX_RANGE` ).
        cl_abap_unit_assert=>assert_true( temp129 ).
    ENDTRY.

    
    lv_sheet = sheet( lo_xlsx->get_xlsx( ) ).
    
    temp130 = boolc( lv_sheet CS `</sheetData><mergeCells count="2"><mergeCell ref="A1:D1"/><mergeCell ref="F2:F5"/></mergeCells>` ).
    cl_abap_unit_assert=>assert_true( temp130 ).
  ENDMETHOD.

  METHOD styles.
    DATA temp50 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp20 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp21 LIKE LINE OF temp20.
    DATA lv_file TYPE xstring.
    DATA lv_styles TYPE string.
    DATA temp131 TYPE xsdboolean.
    DATA temp132 TYPE xsdboolean.
    DATA temp133 TYPE xsdboolean.
    DATA temp134 TYPE xsdboolean.
    DATA temp135 TYPE xsdboolean.
    DATA temp136 TYPE xsdboolean.
    DATA temp137 TYPE xsdboolean.
    DATA temp138 TYPE xsdboolean.
    DATA temp139 TYPE xsdboolean.
    DATA temp140 TYPE xsdboolean.
    DATA lv_sheet TYPE string.
    DATA temp141 TYPE xsdboolean.
    DATA temp142 TYPE xsdboolean.
    DATA temp143 TYPE xsdboolean.
    CLEAR temp50.
    CLEAR temp50-header_style.
    temp50-header_style-fill = `1F4E78`.
    temp50-header_style-color = `ffffff`.
    temp50-header_style-border_bottom = `medium`.
    
    CLEAR temp20.
    
    CLEAR temp21.
    temp21-name = `TEXT`.
    CLEAR temp21-style.
    temp21-style-wrap = abap_true.
    temp21-style-valign = `top`.
    INSERT temp21 INTO TABLE temp20.
    CLEAR temp21.
    temp21-name = `NUM`.
    temp21-format = `0.0`.
    CLEAR temp21-style.
    temp21-style-fill = `#C6EFCE`.
    temp21-style-italic = abap_true.
    temp21-style-border = `thin`.
    temp21-style-border_color = `808080`.
    INSERT temp21 INTO TABLE temp20.
    temp50-t_column = temp20.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      layout = temp50
        )->get_xlsx( ).
    
    lv_styles = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).

    " the header bold, white on blue, a line under it
    
    temp131 = boolc( lv_styles CS `<font><b/><sz val="11"/><color rgb="FFFFFFFF"/>` ).
    cl_abap_unit_assert=>assert_true( temp131 ).
    
    temp132 = boolc( lv_styles CS `<fgColor rgb="FF1F4E78"/>` ).
    cl_abap_unit_assert=>assert_true( temp132 ).
    
    temp133 = boolc( lv_styles CS `<bottom style="medium"></bottom>` ).
    cl_abap_unit_assert=>assert_true( temp133 ).
    " a column that wraps, one green, italic, framed, with its format
    
    temp134 = boolc( lv_styles CS `<alignment vertical="top" wrapText="1"/>` ).
    cl_abap_unit_assert=>assert_true( temp134 ).
    
    temp135 = boolc( lv_styles CS `<fgColor rgb="FFC6EFCE"/>` ).
    cl_abap_unit_assert=>assert_true( temp135 ).
    
    temp136 = boolc( lv_styles CS `<font><i/><sz val="11"/>` ).
    cl_abap_unit_assert=>assert_true( temp136 ).
    
    temp137 = boolc( lv_styles CS `<left style="thin"><color rgb="FF808080"/></left>` ).
    cl_abap_unit_assert=>assert_true( temp137 ).
    
    temp138 = boolc( lv_styles CS `<fills count="4">` ).
    cl_abap_unit_assert=>assert_true( temp138 ).
    
    temp139 = boolc( lv_styles CS `<borders count="3">` ).
    cl_abap_unit_assert=>assert_true( temp139 ).
    
    temp140 = boolc( lv_styles CS `fontId="2" fillId="2" borderId="1"` ).
    cl_abap_unit_assert=>assert_true( temp140 ).

    " equal styles share one format
    
    lv_sheet = sheet( lv_file ).
    
    temp141 = boolc( lv_sheet CS `<c r="A2" s="1" t="inlineStr">` ).
    cl_abap_unit_assert=>assert_true( temp141 ).
    
    temp142 = boolc( lv_sheet CS `<c r="A3" s="1" t="inlineStr">` ).
    cl_abap_unit_assert=>assert_true( temp142 ).
    
    temp143 = boolc( lv_sheet CS `<c r="B3" s="2"><v>2</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp143 ).
  ENDMETHOD.

  METHOD style_invalid.
        DATA temp51 TYPE zabaputil_cl_util_xlsx=>ty_s_style.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp144 TYPE xsdboolean.
        DATA temp52 TYPE zabaputil_cl_util_xlsx=>ty_s_style.
    TRY.
        
        CLEAR temp51.
        temp51-fill = `red`.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( )->set_cell( row   = 1
                                                                    col   = 1
                                                                    val   = 1
                                                                    style = temp51 ).
        cl_abap_unit_assert=>fail( `a colour by name has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp144 = boolc( x->get_text( ) CS `XLSX_STYLE` ).
        cl_abap_unit_assert=>assert_true( temp144 ).
    ENDTRY.
    TRY.
        
        CLEAR temp52.
        temp52-border = `bold`.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( )->set_cell( row   = 1
                                                                    col   = 1
                                                                    val   = 1
                                                                    style = temp52 ).
        cl_abap_unit_assert=>fail( `a border that is none has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD links.
    DATA temp53 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp22 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp23 LIKE LINE OF temp22.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp145 TYPE xsdboolean.
    DATA temp146 TYPE xsdboolean.
    DATA temp147 TYPE xsdboolean.
    DATA temp148 TYPE xsdboolean.
    DATA lv_rels TYPE string.
    DATA temp149 TYPE xsdboolean.
    DATA temp150 TYPE xsdboolean.
    DATA temp151 TYPE xsdboolean.
    DATA temp152 TYPE xsdboolean.
        DATA temp54 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
        DATA temp24 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
        DATA temp25 LIKE LINE OF temp24.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp153 TYPE xsdboolean.
    CLEAR temp53.
    
    CLEAR temp22.
    
    temp23-name = `TEXT`.
    temp23-link = `URL`.
    INSERT temp23 INTO TABLE temp22.
    temp23-name = `URL`.
    temp23-link = `url`.
    INSERT temp23 INTO TABLE temp22.
    temp53-t_column = temp22.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      name   = `Data`
                      layout = temp53
        )->set_cell( row  = 6
                     col  = 1
                     val  = `docs`
                     link = `https://abap-util.github.io/docs/`
        )->get_xlsx( ).
    
    lv_sheet = sheet( lv_file ).

    " a URL by a relationship - each one once -, a place in the workbook
    " by its location
    
    temp145 = boolc( lv_sheet CS `<hyperlinks><hyperlink ref="A2" r:id="rId1"/>` ).
    cl_abap_unit_assert=>assert_true( temp145 ).
    
    temp146 = boolc( lv_sheet CS `<hyperlink ref="A3" r:id="rId1"/>` ).
    cl_abap_unit_assert=>assert_true( temp146 ).
    
    temp147 = boolc( lv_sheet CS `<hyperlink ref="A4" location="Data!A1"/>` ).
    cl_abap_unit_assert=>assert_true( temp147 ).
    
    temp148 = boolc( lv_sheet CS `<hyperlink ref="A6" r:id="rId2"/></hyperlinks>` ).
    cl_abap_unit_assert=>assert_true( temp148 ).
    
    lv_rels = ltcl_file=>part( file = lv_file
                                     name = `xl/worksheets/_rels/sheet1.xml.rels` ).
    
    temp149 = boolc( lv_rels CS `Target="https://example.com/a?x=1&amp;y=2" TargetMode="External"/>` ).
    cl_abap_unit_assert=>assert_true( temp149 ).
    
    temp150 = boolc( lv_rels CS `Id="rId2"` ).
    cl_abap_unit_assert=>assert_true( temp150 ).
    
    temp151 = boolc( lv_rels CS `Id="rId3"` ).
    cl_abap_unit_assert=>assert_false( temp151 ).
    " blue and underlined
    
    temp152 = boolc( ltcl_file=>part( file = lv_file name = `xl/styles.xml` ) CS `<font><u/><sz val="11"/><color rgb="FF0563C1"/>` ).
    cl_abap_unit_assert=>assert_true( temp152 ).

    TRY.
        
        CLEAR temp54.
        
        CLEAR temp24.
        
        temp25-name = `TEXT`.
        temp25-link = `NOPE`.
        INSERT temp25 INTO TABLE temp24.
        temp54-t_column = temp24.
        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                              layout = temp54 ).
        cl_abap_unit_assert=>fail( `a link component the row does not have has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp153 = boolc( x->get_text( ) CS `XLSX_COLUMN_UNKNOWN` ).
        cl_abap_unit_assert=>assert_true( temp153 ).
    ENDTRY.
  ENDMETHOD.

  METHOD validations.
    DATA temp55 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp26 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp27 LIKE LINE OF temp26.
    DATA temp1 TYPE string_table.
    DATA temp28 TYPE zabaputil_cl_util_xlsx=>ty_s_validation.
    DATA lv_sheet TYPE string.
    DATA temp154 TYPE xsdboolean.
    DATA temp155 TYPE xsdboolean.
    DATA temp156 TYPE xsdboolean.
    DATA temp157 TYPE xsdboolean.
    DATA temp158 TYPE xsdboolean.
    DATA temp159 TYPE xsdboolean.
        DATA temp56 TYPE zabaputil_cl_util_xlsx=>ty_s_validation.
        DATA temp29 TYPE string_table.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp160 TYPE xsdboolean.
    CLEAR temp55.
    
    CLEAR temp26.
    
    CLEAR temp27.
    temp27-name = `TEXT`.
    CLEAR temp27-validation.
    temp27-validation-type = `list`.
    
    CLEAR temp1.
    INSERT `a` INTO TABLE temp1.
    INSERT `b` INTO TABLE temp1.
    INSERT `c & d` INTO TABLE temp1.
    temp27-validation-t_value = temp1.
    temp27-validation-error = `a, b or c & d`.
    INSERT temp27 INTO TABLE temp26.
    CLEAR temp27.
    temp27-name = `NUM`.
    INSERT temp27 INTO TABLE temp26.
    temp55-t_column = temp26.
    
    CLEAR temp28.
    temp28-type = `whole`.
    temp28-formula1 = `1`.
    temp28-formula2 = `10`.
    temp28-no_blank = abap_true.
    temp28-error_style = `warning`.
    temp28-prompt_title = `Quantity`.
    temp28-prompt = `1 to 10`.
    
    lv_sheet = sheet( zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      layout = temp55
        )->add_validation( range      = `B2:B4`
                           validation = temp28
        )->get_xlsx( ) ).

    
    temp154 = boolc( lv_sheet CS `<dataValidations count="2">` ).
    cl_abap_unit_assert=>assert_true( temp154 ).
    
    temp155 = boolc( lv_sheet CS `<dataValidation type="list" allowBlank="1" showErrorMessage="1" error="a, b or c &amp; d" sqref="A2:A4">` ).
    cl_abap_unit_assert=>assert_true( temp155 ).
    
    temp156 = boolc( lv_sheet CS `<formula1>&quot;a,b,c &amp; d&quot;</formula1>` ).
    cl_abap_unit_assert=>assert_true( temp156 ).
    
    temp157 = boolc( lv_sheet CS `<dataValidation type="whole" errorStyle="warning"` ).
    cl_abap_unit_assert=>assert_true( temp157 ).
    
    temp158 = boolc( lv_sheet CS `promptTitle="Quantity" prompt="1 to 10" sqref="B2:B4">` ).
    cl_abap_unit_assert=>assert_true( temp158 ).
    
    temp159 = boolc( lv_sheet CS `<formula1>1</formula1><formula2>10</formula2>` ).
    cl_abap_unit_assert=>assert_true( temp159 ).

    TRY.
        
        CLEAR temp56.
        temp56-type = `list`.
        
        CLEAR temp29.
        INSERT `a,b` INTO TABLE temp29.
        temp56-t_value = temp29.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( )->add_validation(
            range      = `A1`
            validation = temp56 )->get_xlsx( ).
        cl_abap_unit_assert=>fail( `a comma in a value of a list has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp160 = boolc( x->get_text( ) CS `XLSX_VALIDATION` ).
        cl_abap_unit_assert=>assert_true( temp160 ).
    ENDTRY.
  ENDMETHOD.

  METHOD names.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp161 TYPE xsdboolean.
    DATA lv_book TYPE string.
    DATA temp162 TYPE xsdboolean.
    DATA temp163 TYPE xsdboolean.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val  = rows( )
                      name = `It's data`
        )->add_name( name = `Texts`
                     ref  = `'It''s data'!$A$2:$A$4`
        )->add_name( name  = `Local`
                     ref   = `=$B$2`
                     local = abap_true ).
    TRY.
        lo_xlsx->add_name( name = `texts`
                           ref  = `$A$1` ).
        cl_abap_unit_assert=>fail( `a name the workbook has has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp161 = boolc( x->get_text( ) CS `XLSX_NAME` ).
        cl_abap_unit_assert=>assert_true( temp161 ).
    ENDTRY.
    TRY.
        lo_xlsx->add_name( name = `AB12`
                           ref  = `$A$1` ).
        cl_abap_unit_assert=>fail( `a name that reads as a cell has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
    TRY.
        lo_xlsx->add_name( name = `a b`
                           ref  = `$A$1` ).
        cl_abap_unit_assert=>fail( `a blank in a name has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.

    
    lv_book = ltcl_file=>part( file = lo_xlsx->get_xlsx( )
                                     name = `xl/workbook.xml` ).
    
    temp162 = boolc( lv_book CS `</sheets><definedNames><definedName name="Texts">'It''s data'!$A$2:$A$4</definedName>` ).
    cl_abap_unit_assert=>assert_true( temp162 ).
    
    temp163 = boolc( lv_book CS `<definedName name="Local" localSheetId="0">$B$2</definedName>` ).
    cl_abap_unit_assert=>assert_true( temp163 ).
  ENDMETHOD.

  METHOD printing.
    DATA temp57 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp164 TYPE xsdboolean.
    DATA temp165 TYPE xsdboolean.
    DATA temp166 TYPE xsdboolean.
    DATA temp167 TYPE xsdboolean.
    DATA lv_book TYPE string.
    DATA temp168 TYPE xsdboolean.
    DATA temp169 TYPE xsdboolean.
    CLEAR temp57.
    temp57-start_cell = `B2`.
    temp57-landscape = abap_true.
    temp57-paper_size = 9.
    temp57-fit_width = 1.
    temp57-margin = 10.
    temp57-print_area = `b2:d5`.
    temp57-repeat_header = abap_true.
    temp57-header_text = `Orders & more`.
    temp57-footer_text = `&LPage &P of &N`.
    
    lo_xlsx = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val  = rows( )
                      name = `First`
        )->add_sheet( val    = rows( )
                      name   = `Q1 2024`
                      layout = temp57 ).
    
    lv_file = lo_xlsx->get_xlsx( ).
    
    lv_sheet = sheet( file  = lv_file
                            index = 2 ).

    
    temp164 = boolc( lv_sheet CS `<sheetPr><pageSetUpPr fitToPage="1"/></sheetPr>` ).
    cl_abap_unit_assert=>assert_true( temp164 ).
    
    temp165 = boolc( lv_sheet CS `<pageMargins left="0.3937" right="0.3937" top="0.3937" bottom="0.3937" header="0.3" footer="0.3"/>` ).
    cl_abap_unit_assert=>assert_true( temp165 ).
    
    temp166 = boolc( lv_sheet CS `<pageSetup paperSize="9" orientation="landscape" fitToWidth="1" fitToHeight="0"/>` ).
    cl_abap_unit_assert=>assert_true( temp166 ).
    
    temp167 = boolc( lv_sheet CS `<headerFooter><oddHeader>&amp;COrders &amp; more</oddHeader><oddFooter>&amp;LPage &amp;P of &amp;N</oddFooter>` ).
    cl_abap_unit_assert=>assert_true( temp167 ).

    
    lv_book = ltcl_file=>part( file = lv_file
                                     name = `xl/workbook.xml` ).
    
    temp168 = boolc( lv_book CS `<definedName name="_xlnm.Print_Area" localSheetId="1">'Q1 2024'!$B$2:$D$5</definedName>` ).
    cl_abap_unit_assert=>assert_true( temp168 ).
    
    temp169 = boolc( lv_book CS `<definedName name="_xlnm.Print_Titles" localSheetId="1">'Q1 2024'!$2:$2</definedName>` ).
    cl_abap_unit_assert=>assert_true( temp169 ).
  ENDMETHOD.

  METHOD sheet_view.
    DATA temp58 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp31 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp2 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_file TYPE xstring.
    DATA lv_book TYPE string.
    DATA temp170 TYPE xsdboolean.
    DATA temp171 TYPE xsdboolean.
    DATA temp172 TYPE xsdboolean.
    DATA lv_sheet TYPE string.
    DATA temp173 TYPE xsdboolean.
    DATA temp174 TYPE xsdboolean.
    DATA temp175 TYPE xsdboolean.
    DATA temp176 TYPE xsdboolean.
    CLEAR temp58.
    temp58-hidden = abap_true.
    
    CLEAR temp31.
    temp31-tab_color = `00B050`.
    temp31-no_gridlines = abap_true.
    temp31-zoom = 85.
    temp31-freeze_cell = `B2`.
    
    CLEAR temp2.
    temp2-freeze_cell = `C1`.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      name   = `Hidden`
                      layout = temp58
        )->add_sheet( val    = rows( )
                      name   = `Shown`
                      layout = temp31
        )->add_sheet( val    = rows( )
                      layout = temp2
        )->get_xlsx( ).

    
    lv_book = ltcl_file=>part( file = lv_file
                                     name = `xl/workbook.xml` ).
    
    temp170 = boolc( lv_book CS `<workbookView activeTab="1"/>` ).
    cl_abap_unit_assert=>assert_true( temp170 ).
    
    temp171 = boolc( lv_book CS `<sheet name="Hidden" sheetId="1" state="hidden" r:id="rId1"/>` ).
    cl_abap_unit_assert=>assert_true( temp171 ).
    
    temp172 = boolc( sheet( lv_file ) CS `tabSelected` ).
    cl_abap_unit_assert=>assert_false( temp172 ).

    
    lv_sheet = sheet( file  = lv_file
                            index = 2 ).
    
    temp173 = boolc( lv_sheet CS `<sheetPr><tabColor rgb="FF00B050"/></sheetPr>` ).
    cl_abap_unit_assert=>assert_true( temp173 ).
    
    temp174 = boolc( lv_sheet CS `<sheetView workbookViewId="0" tabSelected="1" showGridLines="0" zoomScale="85" zoomScaleNormal="85">` ).
    cl_abap_unit_assert=>assert_true( temp174 ).
    
    temp175 = boolc( lv_sheet CS `<pane xSplit="1" ySplit="1" topLeftCell="B2" activePane="bottomRight" state="frozen"/>` ).
    cl_abap_unit_assert=>assert_true( temp175 ).
    lv_sheet = sheet( file  = lv_file
                      index = 3 ).
    
    temp176 = boolc( lv_sheet CS `<pane xSplit="2" topLeftCell="C1" activePane="topRight" state="frozen"/>` ).
    cl_abap_unit_assert=>assert_true( temp176 ).
  ENDMETHOD.

  METHOD all_hidden.
        DATA temp59 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp177 TYPE xsdboolean.
    TRY.
        
        CLEAR temp59.
        temp59-hidden = abap_true.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( val    = rows( )
                                                       layout = temp59 )->get_xlsx( ).
        cl_abap_unit_assert=>fail( `every sheet hidden has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp177 = boolc( x->get_text( ) CS `XLSX_ALL_HIDDEN` ).
        cl_abap_unit_assert=>assert_true( temp177 ).
    ENDTRY.
  ENDMETHOD.

  METHOD properties.
    DATA temp60 TYPE zabaputil_cl_util_xlsx=>ty_s_properties.
    DATA lv_file TYPE xstring.
    DATA lv_core TYPE string.
    DATA temp178 TYPE xsdboolean.
    DATA temp179 TYPE xsdboolean.
    DATA temp180 TYPE xsdboolean.
    DATA lv_app TYPE string.
    DATA temp181 TYPE xsdboolean.
    DATA temp182 TYPE xsdboolean.
    DATA temp183 TYPE xsdboolean.
    CLEAR temp60.
    temp60-title = `Orders <2024>`.
    temp60-creator = `Jane`.
    temp60-company = `ACME`.
    temp60-created = '20240315103000'.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->set_properties( temp60
        )->add_sheet( rows( )
        )->get_xlsx( ).

    
    lv_core = ltcl_file=>part( file = lv_file
                                     name = `docProps/core.xml` ).
    
    temp178 = boolc( lv_core CS `<dc:title>Orders &lt;2024&gt;</dc:title>` ).
    cl_abap_unit_assert=>assert_true( temp178 ).
    
    temp179 = boolc( lv_core CS `<dc:creator>Jane</dc:creator>` ).
    cl_abap_unit_assert=>assert_true( temp179 ).
    
    temp180 = boolc( lv_core CS `<dcterms:created xsi:type="dcterms:W3CDTF">2024-03-15T10:30:00Z</dcterms:created>` ).
    cl_abap_unit_assert=>assert_true( temp180 ).
    
    lv_app = ltcl_file=>part( file = lv_file
                                    name = `docProps/app.xml` ).
    
    temp181 = boolc( lv_app CS `<Application>abap-util</Application><Company>ACME</Company>` ).
    cl_abap_unit_assert=>assert_true( temp181 ).
    
    temp182 = boolc( ltcl_file=>part( file = lv_file name = `_rels/.rels` ) CS `Target="docProps/core.xml"` ).
    cl_abap_unit_assert=>assert_true( temp182 ).
    
    temp183 = boolc( ltcl_file=>part( file = lv_file name = `[Content_Types].xml` ) CS `PartName="/docProps/app.xml"` ).
    cl_abap_unit_assert=>assert_true( temp183 ).
    " without, no part
    cl_abap_unit_assert=>assert_false( ltcl_file=>has_part( file = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) )
                                                            name = `docProps/core.xml` ) ).
  ENDMETHOD.

  METHOD calculated_column.
    DATA temp61 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp32 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp33 LIKE LINE OF temp32.
    DATA lv_table TYPE string.
    DATA temp184 TYPE xsdboolean.
    CLEAR temp61.
    temp61-table_style = `TableStyleMedium2`.
    temp61-start_cell = `A3`.
    
    CLEAR temp32.
    
    CLEAR temp33.
    temp33-name = `NUM`.
    INSERT temp33 INTO TABLE temp32.
    CLEAR temp33.
    temp33-header = `Double`.
    temp33-formula = `A{row}*2`.
    INSERT temp33 INTO TABLE temp32.
    temp61-t_column = temp32.
    
    lv_table = ltcl_file=>part(
        file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
                   val    = rows( )
                   layout = temp61 )
        name = `xl/tables/table1.xml` ).
    
    temp184 = boolc( lv_table CS `<tableColumn id="2" name="Double"><calculatedColumnFormula>A4*2</calculatedColumnFormula></tableColumn>` ).
    cl_abap_unit_assert=>assert_true( temp184 ).
  ENDMETHOD.

  METHOD rules.
    DATA temp62 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp34 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp35 LIKE LINE OF temp34.
    DATA temp36 TYPE zabaputil_cl_util_xlsx=>ty_s_rule.
    DATA temp3 TYPE zabaputil_cl_util_xlsx=>ty_s_rule.
    DATA temp4 TYPE zabaputil_cl_util_xlsx=>ty_s_rule.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp185 TYPE xsdboolean.
    DATA temp186 TYPE xsdboolean.
    DATA temp187 TYPE xsdboolean.
    DATA temp188 TYPE xsdboolean.
    DATA temp189 TYPE xsdboolean.
    DATA lv_styles TYPE string.
    DATA temp190 TYPE xsdboolean.
        DATA temp63 TYPE zabaputil_cl_util_xlsx=>ty_s_rule.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp191 TYPE xsdboolean.
    CLEAR temp62.
    
    CLEAR temp34.
    
    CLEAR temp35.
    temp35-name = `TEXT`.
    INSERT temp35 INTO TABLE temp34.
    CLEAR temp35.
    temp35-name = `NUM`.
    CLEAR temp35-rule.
    temp35-rule-type = `cellIs`.
    temp35-rule-operator = `greaterThan`.
    temp35-rule-formula1 = `1`.
    CLEAR temp35-rule-style.
    temp35-rule-style-color = `9C0006`.
    temp35-rule-style-fill = `FFC7CE`.
    INSERT temp35 INTO TABLE temp34.
    temp62-t_column = temp34.
    
    CLEAR temp36.
    temp36-type = `containsText`.
    temp36-text = `b`.
    CLEAR temp36-style.
    temp36-style-bold = abap_true.
    
    CLEAR temp3.
    temp3-type = `colorScale`.
    temp3-color_min = `F8696B`.
    temp3-color_mid = `FFEB84`.
    temp3-color_max = `63BE7B`.
    
    CLEAR temp4.
    temp4-type = `iconSet`.
    temp4-icon_set = `3Arrows`.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( val    = rows( )
                      layout = temp62
        )->add_rule( range = `A2:A4`
                     rule  = temp36
        )->add_rule( range = `B2:B4`
                     rule  = temp3
        )->add_rule( range = `B2:B4`
                     rule  = temp4
        )->get_xlsx( ).
    
    lv_sheet = sheet( lv_file ).

    
    temp185 = boolc( lv_sheet CS `<conditionalFormatting sqref="A2:A4"><cfRule type="containsText" priority="2" dxfId="1" operator="containsText" text="b">` ).
    cl_abap_unit_assert=>assert_true( temp185 ).
    
    temp186 = boolc( lv_sheet CS `<formula>NOT(ISERROR(SEARCH(&quot;b&quot;,A2)))</formula>` ).
    cl_abap_unit_assert=>assert_true( temp186 ).
    
    temp187 = boolc( lv_sheet CS `<conditionalFormatting sqref="B2:B4"><cfRule type="cellIs" priority="1" dxfId="0" operator="greaterThan"><formula>1</formula>` ).
    cl_abap_unit_assert=>assert_true( temp187 ).
    
    temp188 = boolc( lv_sheet CS `<cfvo type="percentile" val="50"/><cfvo type="max"/><color rgb="FFF8696B"/><color rgb="FFFFEB84"/>` ).
    cl_abap_unit_assert=>assert_true( temp188 ).
    
    temp189 = boolc( lv_sheet CS `<iconSet iconSet="3Arrows"><cfvo type="percent" val="0"/><cfvo type="percent" val="33"/><cfvo type="percent" val="66"/>` ).
    cl_abap_unit_assert=>assert_true( temp189 ).
    
    lv_styles = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    
    temp190 = boolc( lv_styles CS `<dxfs count="2"><dxf><font><color rgb="FF9C0006"/></font><fill><patternFill><bgColor rgb="FFFFC7CE"/>` ).
    cl_abap_unit_assert=>assert_true( temp190 ).

    TRY.
        
        CLEAR temp63.
        temp63-type = `sparkle`.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( )->add_rule( range = `A1`
                                                                    rule  = temp63 ).
        cl_abap_unit_assert=>fail( `a rule that is none has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp191 = boolc( x->get_text( ) CS `XLSX_RULE` ).
        cl_abap_unit_assert=>assert_true( temp191 ).
    ENDTRY.
  ENDMETHOD.

  METHOD protection.
    DATA temp64 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp37 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp38 LIKE LINE OF temp37.
    DATA lv_file TYPE xstring.
    DATA temp192 TYPE xsdboolean.
    DATA temp193 TYPE xsdboolean.
    DATA temp194 TYPE xsdboolean.
    CLEAR temp64.
    temp64-protect = abap_true.
    temp64-password = `password`.
    temp64-auto_filter = abap_true.
    
    CLEAR temp37.
    
    temp38-name = `TEXT`.
    CLEAR temp38-style.
    temp38-style-unlocked = abap_true.
    INSERT temp38 INTO TABLE temp37.
    temp64-t_column = temp37.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->protect_workbook( `abc`
        )->add_sheet( val    = rows( )
                      layout = temp64
        )->get_xlsx( ).

    " the legacy hash, as openpyxl and Excel work it out
    
    temp192 = boolc( sheet( lv_file ) CS `</sheetData><sheetProtection sheet="1" objects="1" scenarios="1" password="83AF" autoFilter="0" sort="0"/>` ).
    cl_abap_unit_assert=>assert_true( temp192 ).
    
    temp193 = boolc( ltcl_file=>part( file = lv_file name = `xl/workbook.xml` ) CS `<workbookProtection lockStructure="1" workbookPassword="CC1A"/><bookViews>` ).
    cl_abap_unit_assert=>assert_true( temp193 ).
    
    temp194 = boolc( ltcl_file=>part( file = lv_file name = `xl/styles.xml` ) CS `applyProtection="1"><protection locked="0"/></xf>` ).
    cl_abap_unit_assert=>assert_true( temp194 ).
    TRY.
        zabaputil_cl_util_xlsx=>factory( )->protect_workbook( `a password longer than fifteen` ).
        cl_abap_unit_assert=>fail( `a long password has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD comments.
    DATA lv_file TYPE xstring.
    DATA lv_comments TYPE string.
    DATA temp195 TYPE xsdboolean.
    DATA temp196 TYPE xsdboolean.
    DATA temp197 TYPE xsdboolean.
    DATA lv_vml TYPE string.
    DATA temp198 TYPE xsdboolean.
    DATA temp199 TYPE xsdboolean.
    DATA lv_rels TYPE string.
    DATA temp200 TYPE xsdboolean.
    DATA lv_types TYPE string.
    DATA temp201 TYPE xsdboolean.
    DATA temp202 TYPE xsdboolean.
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( rows( )
        )->add_comment( row    = 2
                        col    = 2
                        text   = `checked <ok>`
                        author = `Jane`
        )->add_comment( row  = 3
                        col  = 1
                        text = `no author`
        )->get_xlsx( ).

    
    lv_comments = ltcl_file=>part( file = lv_file
                                         name = `xl/comments1.xml` ).
    
    temp195 = boolc( lv_comments CS `<authors><author>Jane</author><author></author></authors>` ).
    cl_abap_unit_assert=>assert_true( temp195 ).
    
    temp196 = boolc( lv_comments CS `<comment ref="B2" authorId="0"><text><r><t xml:space="preserve">checked &lt;ok&gt;</t>` ).
    cl_abap_unit_assert=>assert_true( temp196 ).
    
    temp197 = boolc( lv_comments CS `<comment ref="A3" authorId="1">` ).
    cl_abap_unit_assert=>assert_true( temp197 ).
    
    lv_vml = ltcl_file=>part( file = lv_file
                                    name = `xl/drawings/vmlDrawing1.vml` ).
    
    temp198 = boolc( lv_vml CS `<x:Row>1</x:Row><x:Column>1</x:Column>` ).
    cl_abap_unit_assert=>assert_true( temp198 ).
    
    temp199 = boolc( sheet( lv_file ) CS `<legacyDrawing r:id="rId2"/></worksheet>` ).
    cl_abap_unit_assert=>assert_true( temp199 ).
    
    lv_rels = ltcl_file=>part( file = lv_file
                                     name = `xl/worksheets/_rels/sheet1.xml.rels` ).
    
    temp200 = boolc( lv_rels CS `Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments"` ).
    cl_abap_unit_assert=>assert_true( temp200 ).
    
    lv_types = ltcl_file=>part( file = lv_file
                                      name = `[Content_Types].xml` ).
    
    temp201 = boolc( lv_types CS `<Default Extension="vml"` ).
    cl_abap_unit_assert=>assert_true( temp201 ).
    
    temp202 = boolc( lv_types CS `PartName="/xl/comments1.xml"` ).
    cl_abap_unit_assert=>assert_true( temp202 ).
  ENDMETHOD.

  METHOD images.
    " a PNG of 4 x 2 pixels
    DATA temp65 TYPE xstring.
    DATA lv_png LIKE temp65.
    DATA lv_file TYPE xstring.
    DATA lv_drawing TYPE string.
    DATA temp203 TYPE xsdboolean.
    DATA temp204 TYPE xsdboolean.
    DATA temp205 TYPE xsdboolean.
    DATA temp206 TYPE xsdboolean.
    DATA temp207 TYPE xsdboolean.
        DATA temp66 TYPE xstring.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp208 TYPE xsdboolean.
    temp65 = `89504E470D0A1A0A0000000D4948445200000004000000020802000000F0CAEA34` && `0000001049444154789C63F8CFC000470CC81C006FAA07F980DC00280000000049454E44AE426082`.
    
    lv_png = temp65.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( rows( )
        )->add_image( row   = 1
                      col   = 5
                      image = lv_png
                      width = 40
        )->get_xlsx( ).

    " the size by the width asked for, the proportions kept
    
    lv_drawing = ltcl_file=>part( file = lv_file
                                        name = `xl/drawings/drawing1.xml` ).
    
    temp203 = boolc( lv_drawing CS `<xdr:col>4</xdr:col>` ).
    cl_abap_unit_assert=>assert_true( temp203 ).
    
    temp204 = boolc( lv_drawing CS `<xdr:ext cx="381000" cy="190500"/>` ).
    cl_abap_unit_assert=>assert_true( temp204 ).
    cl_abap_unit_assert=>assert_true( ltcl_file=>has_part( file = lv_file
                                                           name = `xl/media/image1.png` ) ).
    
    temp205 = boolc( ltcl_file=>part( file = lv_file name = `xl/drawings/_rels/drawing1.xml.rels` ) CS `Target="../media/image1.png"` ).
    cl_abap_unit_assert=>assert_true( temp205 ).
    
    temp206 = boolc( sheet( lv_file ) CS `<drawing r:id="rId1"/></worksheet>` ).
    cl_abap_unit_assert=>assert_true( temp206 ).
    
    temp207 = boolc( ltcl_file=>part( file = lv_file name = `[Content_Types].xml` ) CS `<Default Extension="png" ContentType="image/png"/>` ).
    cl_abap_unit_assert=>assert_true( temp207 ).
    TRY.
        
        temp66 = `47494638`.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( )->add_image( row   = 1
                                                                     col   = 1
                                                                     image = temp66 ).
        cl_abap_unit_assert=>fail( `a GIF has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp208 = boolc( x->get_text( ) CS `XLSX_IMAGE` ).
        cl_abap_unit_assert=>assert_true( temp208 ).
    ENDTRY.
  ENDMETHOD.

  METHOD shared_strings.
    DATA lt_back TYPE ty_t_row.

    DATA temp67 TYPE ty_t_row.
    DATA temp68 LIKE LINE OF temp67.
    DATA lv_file TYPE xstring.
    DATA lv_sheet TYPE string.
    DATA temp209 TYPE xsdboolean.
    DATA temp210 TYPE xsdboolean.
    DATA temp211 TYPE xsdboolean.
    DATA lv_sst TYPE string.
    DATA temp212 TYPE xsdboolean.
    DATA temp213 TYPE xsdboolean.
    FIELD-SYMBOLS <temp69> LIKE LINE OF lt_back.
    DATA temp70 LIKE sy-tabix.
    CLEAR temp67.
    
    temp68-text = `same`.
    INSERT temp68 INTO TABLE temp67.
    temp68-text = `same`.
    INSERT temp68 INTO TABLE temp67.
    temp68-text = `a & b`.
    INSERT temp68 INTO TABLE temp67.
    
    lv_file = zabaputil_cl_util_xlsx=>factory( abap_true
        )->add_sheet( temp67
        )->get_xlsx( ).

    
    lv_sheet = sheet( lv_file ).
    " the header texts first, then each text once
    
    temp209 = boolc( lv_sheet CS `<c r="A1" s="1" t="s"><v>0</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp209 ).
    
    temp210 = boolc( lv_sheet CS `<c r="A2" t="s"><v>3</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp210 ).
    
    temp211 = boolc( lv_sheet CS `<c r="A3" t="s"><v>3</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp211 ).
    
    lv_sst = ltcl_file=>part( file = lv_file
                                    name = `xl/sharedStrings.xml` ).
    
    temp212 = boolc( lv_sst CS `count="5" uniqueCount="5"` ).
    cl_abap_unit_assert=>assert_true( temp212 ).
    
    temp213 = boolc( lv_sst CS `<si><t xml:space="preserve">a &amp; b</t></si>` ).
    cl_abap_unit_assert=>assert_true( temp213 ).

    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    
    
    temp70 = sy-tabix.
    READ TABLE lt_back INDEX 3 ASSIGNING <temp69>.
    sy-tabix = temp70.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a & b`
                                        act = <temp69>-text ).
  ENDMETHOD.

  METHOD currency_and_unit.
    TYPES:
      BEGIN OF ty_s_amount,
        amount   TYPE p LENGTH 8 DECIMALS 2,
        currency TYPE c LENGTH 5,
        qty      TYPE p LENGTH 8 DECIMALS 3,
        unit     TYPE c LENGTH 3,
      END OF ty_s_amount.
    TYPES temp3 TYPE STANDARD TABLE OF ty_s_amount WITH DEFAULT KEY.
DATA lt_amount TYPE temp3.

    DATA temp71 LIKE lt_amount.
    DATA temp72 LIKE LINE OF temp71.
    DATA temp73 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp39 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp40 LIKE LINE OF temp39.
    DATA lv_file TYPE xstring.
    DATA temp214 TYPE xsdboolean.
    DATA temp215 TYPE xsdboolean.
    DATA lv_styles TYPE string.
    DATA temp216 TYPE xsdboolean.
    DATA temp217 TYPE xsdboolean.
        DATA temp74 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
        DATA temp41 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
        DATA temp42 LIKE LINE OF temp41.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp218 TYPE xsdboolean.
    CLEAR temp71.
    
    temp72-amount = `12.50`.
    temp72-currency = `EUR`.
    temp72-qty = `1.5`.
    temp72-unit = `KG`.
    INSERT temp72 INTO TABLE temp71.
    lt_amount = temp71.
    
    CLEAR temp73.
    
    CLEAR temp39.
    
    CLEAR temp40.
    temp40-name = `AMOUNT`.
    temp40-currency = `currency`.
    INSERT temp40 INTO TABLE temp39.
    CLEAR temp40.
    temp40-name = `QTY`.
    temp40-unit = `UNIT`.
    INSERT temp40 INTO TABLE temp39.
    temp73-t_column = temp39.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = lt_amount
        layout = temp73 ).
    " a currency the system does not list has two decimals; a unit it does
    " not list the decimals of the field
    
    temp214 = boolc( sheet( lv_file ) CS `<c r="A2" s="2"><v>12.5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp214 ).
    
    temp215 = boolc( sheet( lv_file ) CS `<c r="B2" s="3"><v>1.5</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp215 ).
    
    lv_styles = ltcl_file=>part( file = lv_file
                                       name = `xl/styles.xml` ).
    
    temp216 = boolc( lv_styles CS `formatCode="#,##0.00"/>` ).
    cl_abap_unit_assert=>assert_true( temp216 ).
    
    temp217 = boolc( lv_styles CS `formatCode="#,##0.000"/>` ).
    cl_abap_unit_assert=>assert_true( temp217 ).

    TRY.
        
        CLEAR temp74.
        
        CLEAR temp41.
        
        temp42-name = `AMOUNT`.
        temp42-currency = `WAERS`.
        INSERT temp42 INTO TABLE temp41.
        temp74-t_column = temp41.
        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = lt_amount
                                              layout = temp74 ).
        cl_abap_unit_assert=>fail( `a currency component the row does not have has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp218 = boolc( x->get_text( ) CS `XLSX_COLUMN_UNKNOWN` ).
        cl_abap_unit_assert=>assert_true( temp218 ).
    ENDTRY.
  ENDMETHOD.

  METHOD conversion_exit.
    TYPES:
      BEGIN OF ty_s_doc,
        doc TYPE c LENGTH 10,
      END OF ty_s_doc.
    TYPES temp4 TYPE STANDARD TABLE OF ty_s_doc WITH DEFAULT KEY.
DATA lt_doc  TYPE temp4.
    TYPES temp5 TYPE STANDARD TABLE OF ty_s_doc WITH DEFAULT KEY.
DATA lt_back TYPE temp5.

    DATA temp75 LIKE lt_doc.
    DATA temp76 LIKE LINE OF temp75.
    DATA temp77 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp43 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp44 LIKE LINE OF temp43.
    DATA lv_file TYPE xstring.
    DATA temp219 TYPE xsdboolean.
    DATA temp78 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp79 LIKE LINE OF temp78.
    DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    FIELD-SYMBOLS <temp80> LIKE LINE OF lt_back.
    DATA temp81 LIKE sy-tabix.
    CLEAR temp75.
    
    temp76-doc = `0000004711`.
    INSERT temp76 INTO TABLE temp75.
    lt_doc = temp75.
    
    CLEAR temp77.
    
    CLEAR temp43.
    
    temp44-name = `DOC`.
    temp44-conv_exit = `ALPHA`.
    INSERT temp44 INTO TABLE temp43.
    temp77-t_column = temp43.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = lt_doc
        layout = temp77 ).
    " written without its zeros, read back with them
    
    temp219 = boolc( sheet( lv_file ) CS `<t xml:space="preserve">4711</t>` ).
    cl_abap_unit_assert=>assert_true( temp219 ).
    
    CLEAR temp78.
    
    temp79-name = `DOC`.
    temp79-conv_exit = `alpha`.
    INSERT temp79 INTO TABLE temp78.
    
    temp1 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp1->get_table(
        EXPORTING t_column = temp78
        IMPORTING result   = lt_back ).
    
    
    temp81 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp80>.
    sy-tabix = temp81.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `0000004711`
                                        act = <temp80>-doc ).
  ENDMETHOD.

  METHOD read_under_a_title.
    DATA lt_back TYPE ty_t_row.

    DATA temp82 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_file TYPE xstring.
    DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    FIELD-SYMBOLS <temp83> LIKE LINE OF lt_back.
    DATA temp84 LIKE sy-tabix.
    CLEAR temp82.
    temp82-start_cell = `A3`.
    
    lv_file = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet(
        )->set_cell( row = 1
                     col = 1
                     val = `Title`
        )->merge( `A1:C1`
        )->add_table( val    = rows( )
                      layout = temp82
        )->get_xlsx( ).
    
    temp1 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp1->get_table( EXPORTING header_row = 3
                                                                   IMPORTING result     = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_back ) ).
    
    
    temp84 = sy-tabix.
    READ TABLE lt_back INDEX 3 ASSIGNING <temp83>.
    sy-tabix = temp84.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `c`
                                        act = <temp83>-text ).
  ENDMETHOD.

ENDCLASS.


"! ================================================================
"! READING: formulas, ranges, row by row, errors, merged cells, links,
"! names and Excel tables, hidden rows and columns, text notations
"! ================================================================
CLASS ltcl_read_more DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_row,
        text TYPE string,
        num  TYPE i,
        date TYPE d,
      END OF ty_s_row.
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_amount,
        amount TYPE p LENGTH 10 DECIMALS 3,
        date   TYPE d,
      END OF ty_s_amount.
    TYPES ty_t_amount TYPE STANDARD TABLE OF ty_s_amount WITH DEFAULT KEY.

    METHODS rows
      RETURNING
        VALUE(result) TYPE ty_t_row.

    "! a workbook of three sheets: a title and a table, a hidden one, an
    "! Excel table - with names, links, merged and hidden cells
    METHODS book
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS formulas             FOR TESTING RAISING cx_static_check.
    METHODS range_and_max_rows   FOR TESTING RAISING cx_static_check.
    METHODS row_by_row           FOR TESTING RAISING cx_static_check.
    METHODS errors_collected     FOR TESTING RAISING cx_static_check.
    METHODS merged_and_links     FOR TESTING RAISING cx_static_check.
    METHODS hidden_parts         FOR TESTING RAISING cx_static_check.
    METHODS names_and_tables     FOR TESTING RAISING cx_static_check.
    METHODS table_by_name        FOR TESTING RAISING cx_static_check.
    METHODS text_notations       FOR TESTING RAISING cx_static_check.
    METHODS cell_formats         FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_read_more IMPLEMENTATION.

  METHOD rows.
    DATA temp85 TYPE ltcl_read_more=>ty_t_row.
    DATA temp86 LIKE LINE OF temp85.
    CLEAR temp85.
    
    temp86-text = `a`.
    temp86-num = 1.
    temp86-date = `20240315`.
    INSERT temp86 INTO TABLE temp85.
    temp86-text = `b`.
    temp86-num = 2.
    temp86-date = `20240316`.
    INSERT temp86 INTO TABLE temp85.
    temp86-text = `c`.
    temp86-num = 3.
    temp86-date = `20240317`.
    INSERT temp86 INTO TABLE temp85.
    result = temp85.
  ENDMETHOD.

  METHOD book.
    DATA temp87 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp45 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp4 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp5 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp6 LIKE LINE OF temp5.
    CLEAR temp87.
    temp87-start_cell = `A3`.
    
    CLEAR temp45.
    temp45-hidden = abap_true.
    
    CLEAR temp4.
    temp4-table_style = `TableStyleLight9`.
    temp4-table_name = `Orders`.
    temp4-start_cell = `B2`.
    
    CLEAR temp5.
    
    CLEAR temp6.
    temp6-name = `TEXT`.
    temp6-total = `Total`.
    INSERT temp6 INTO TABLE temp5.
    CLEAR temp6.
    temp6-name = `NUM`.
    temp6-total = `sum`.
    INSERT temp6 INTO TABLE temp5.
    CLEAR temp6.
    temp6-name = `DATE`.
    INSERT temp6 INTO TABLE temp5.
    temp4-t_column = temp5.
    result = zabaputil_cl_util_xlsx=>factory(
        )->add_sheet( name = `Report`
        )->set_cell( row  = 1
                     col  = 1
                     val  = `Title`
                     link = `https://example.com`
        )->merge( `A1:C1`
        )->add_table( val    = rows( )
                      layout = temp87
        )->set_row( row    = 5
                    hidden = abap_true
        )->set_column( col    = 2
                       hidden = abap_true
        )->add_name( name = `Block`
                     ref  = `Report!$A$3:$C$6`
        )->add_sheet( val    = rows( )
                      name   = `Secret`
                      layout = temp45
        )->add_sheet( val    = rows( )
                      name   = `Tab`
                      layout = temp4
        )->get_xlsx( ).
  ENDMETHOD.

  METHOD formulas.
    " a formula with its value, and a shared one: the cells after the
    " first move its references without a $
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    FIELD-SYMBOLS <temp88> LIKE LINE OF lt_cell.
    DATA temp89 LIKE sy-tabix.
    FIELD-SYMBOLS <temp90> LIKE LINE OF lt_cell.
    DATA temp91 LIKE sy-tabix.
    FIELD-SYMBOLS <temp92> LIKE LINE OF lt_cell.
    DATA temp93 LIKE sy-tabix.
    FIELD-SYMBOLS <temp94> LIKE LINE OF lt_cell.
    DATA temp95 LIKE sy-tabix.
    FIELD-SYMBOLS <temp96> LIKE LINE OF lt_cell.
    DATA temp97 LIKE sy-tabix.
    FIELD-SYMBOLS <temp98> LIKE LINE OF lt_cell.
    DATA temp99 LIKE sy-tabix.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
        `<row r="1"><c r="A1"><v>2</v></c><c r="B1"><f t="shared" ref="B1:B3" si="0">A1*$C$1+SUM(A$1:A1)</f><v>4</v></c>`
     && `<c r="C1"><v>1</v></c><c r="D1" t="str"><f>"A1"&amp;Sheet2!A1</f><v>x</v></c></row>`
     && `<row r="2"><c r="A2"><v>3</v></c><c r="B2"><f t="shared" si="0"/><v>8</v></c></row>`
     && `<row r="3"><c r="A3"><v>4</v></c><c r="B3"><f t="shared" si="0"/><v>13</v></c></row>` ) )->get_cells( ).

    
    
    temp89 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 1 col = 2 ASSIGNING <temp88>.
    sy-tabix = temp89.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A1*$C$1+SUM(A$1:A1)`
                                        act = <temp88>-formula ).
    
    
    temp91 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 col = 2 ASSIGNING <temp90>.
    sy-tabix = temp91.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A2*$C$1+SUM(A$1:A2)`
                                        act = <temp90>-formula ).
    
    
    temp93 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 3 col = 2 ASSIGNING <temp92>.
    sy-tabix = temp93.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A3*$C$1+SUM(A$1:A3)`
                                        act = <temp92>-formula ).
    
    
    temp95 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 3 col = 2 ASSIGNING <temp94>.
    sy-tabix = temp95.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `13`
                                        act = <temp94>-value ).
    
    
    temp97 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 1 col = 4 ASSIGNING <temp96>.
    sy-tabix = temp97.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `"A1"&Sheet2!A1`
                                        act = <temp96>-formula ).
    
    
    temp99 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 1 col = 1 ASSIGNING <temp98>.
    sy-tabix = temp99.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp98>-formula ).
  ENDMETHOD.

  METHOD range_and_max_rows.
    DATA lt_back TYPE ty_t_row.

    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    FIELD-SYMBOLS <temp100> LIKE LINE OF lt_cell.
    DATA temp101 LIKE sy-tabix.
    FIELD-SYMBOLS <temp102> LIKE LINE OF lt_cell.
    DATA temp103 LIKE sy-tabix.
    FIELD-SYMBOLS <temp104> LIKE LINE OF lt_back.
    DATA temp105 LIKE sy-tabix.
    DATA lr_any TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
    FIELD-SYMBOLS <num> TYPE any.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx( book( ) ).
    
    lt_cell = lo_xlsx->get_cells( range = `A4:A6` ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_cell ) ).
    
    
    temp101 = sy-tabix.
    READ TABLE lt_cell INDEX 1 ASSIGNING <temp100>.
    sy-tabix = temp101.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `a`
                                        act = <temp100>-value ).
    lt_cell = lo_xlsx->get_cells( max_rows = 2 ).
    
    
    temp103 = sy-tabix.
    READ TABLE lt_cell INDEX lines( lt_cell ) ASSIGNING <temp102>.
    sy-tabix = temp103.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = <temp102>-row ).

    " a range: the header in its first row
    lo_xlsx->get_table( EXPORTING range    = `A3:C6`
                                  max_rows = 2
                        IMPORTING result   = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_back ) ).
    
    
    temp105 = sy-tabix.
    READ TABLE lt_back INDEX 2 ASSIGNING <temp104>.
    sy-tabix = temp105.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `20240316`
                                        act = <temp104>-date ).

    
    lr_any = lo_xlsx->get_table_ref( range    = `B3:C6`
                                           max_rows = 1 ).
    
    ASSIGN lr_any->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( <tab> ) ).
    " a component of a table expression is assigned in two steps: the
    " 7.02 downport reads the whole row into the field symbol otherwise
    
    
    LOOP AT <tab> ASSIGNING <line>.
      EXIT.
    ENDLOOP.
    ASSIGN COMPONENT `NUM` OF STRUCTURE <line> TO <num>.
    cl_abap_unit_assert=>assert_equals( exp = `1`
                                        act = <num> ).
  ENDMETHOD.

  METHOD row_by_row.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    DATA lv_eof  TYPE abap_bool.
    DATA lv_rows TYPE i.

    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
        DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx( book( ) ).
    lo_xlsx->rows_open( sheet = `Tab` ).
    DO.
      lo_xlsx->rows_next( IMPORTING t_cell = lt_cell
                                    eof    = lv_eof ).
      IF lv_eof = abap_true.
        EXIT.
      ENDIF.
      lv_rows = lv_rows + 1.
    ENDDO.
    " the header, three rows, the totals row
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lv_rows ).
    TRY.
        
        temp1 = zabaputil_cl_util_xlsx=>factory_by_xlsx( book( ) ).
        temp1->rows_next( IMPORTING t_cell = lt_cell
                                                                                 eof    = lv_eof ).
        cl_abap_unit_assert=>fail( `rows_next( ) without rows_open( ) has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD errors_collected.
    DATA lt_back  TYPE ty_t_row.
    DATA lt_error TYPE zabaputil_cl_util_xlsx=>ty_t_error.

    DATA temp106 TYPE string_table.
    DATA lv_file TYPE xstring.
    DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    FIELD-SYMBOLS <temp108> LIKE LINE OF lt_error.
    DATA temp109 LIKE sy-tabix.
    FIELD-SYMBOLS <temp110> LIKE LINE OF lt_error.
    DATA temp111 LIKE sy-tabix.
    FIELD-SYMBOLS <temp112> LIKE LINE OF lt_error.
    DATA temp113 LIKE sy-tabix.
    FIELD-SYMBOLS <temp114> LIKE LINE OF lt_error.
    DATA temp115 LIKE sy-tabix.
    FIELD-SYMBOLS <temp116> LIKE LINE OF lt_error.
    DATA temp117 LIKE sy-tabix.
    DATA temp220 TYPE xsdboolean.
        DATA temp2 TYPE REF TO zabaputil_cl_util_xlsx.
    CLEAR temp106.
    INSERT `num` INTO TABLE temp106.
    INSERT `1` INTO TABLE temp106.
    INSERT `x` INTO TABLE temp106.
    INSERT `3` INTO TABLE temp106.
    INSERT `y` INTO TABLE temp106.
    
    lv_file = zabaputil_cl_util_xlsx=>factory( )->add_sheet(
        temp106 )->get_xlsx( ).
    
    temp1 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp1->get_table( EXPORTING header_row = 2
                                                                   IMPORTING result     = lt_back
                                                                             t_error    = lt_error ).
    " every row read, every cell that does not fit listed
    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_back ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_error ) ).
    
    
    temp109 = sy-tabix.
    READ TABLE lt_error INDEX 1 ASSIGNING <temp108>.
    sy-tabix = temp109.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A4`
                                        act = <temp108>-cell ).
    
    
    temp111 = sy-tabix.
    READ TABLE lt_error INDEX 1 ASSIGNING <temp110>.
    sy-tabix = temp111.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `x`
                                        act = <temp110>-value ).
    
    
    temp113 = sy-tabix.
    READ TABLE lt_error INDEX 1 ASSIGNING <temp112>.
    sy-tabix = temp113.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `NUM`
                                        act = <temp112>-component ).
    
    
    temp115 = sy-tabix.
    READ TABLE lt_error INDEX 1 ASSIGNING <temp114>.
    sy-tabix = temp115.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Sheet1`
                                        act = <temp114>-sheet ).
    
    
    temp117 = sy-tabix.
    READ TABLE lt_error INDEX 2 ASSIGNING <temp116>.
    sy-tabix = temp117.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    
    temp220 = boolc( <temp116>-message CS `XLSX_CELL_TYPE` ).
    cl_abap_unit_assert=>assert_true( temp220 ).

    " without t_error the first one raises
    TRY.
        
        temp2 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
        temp2->get_table( EXPORTING header_row = 2
                                                                       IMPORTING result     = lt_back ).
        cl_abap_unit_assert=>fail( `a cell that does not fit has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD merged_and_links.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA temp118 TYPE string_table.
    DATA lt_link TYPE zabaputil_cl_util_xlsx=>ty_t_hyperlink.
    FIELD-SYMBOLS <temp120> LIKE LINE OF lt_link.
    DATA temp121 LIKE sy-tabix.
    FIELD-SYMBOLS <temp122> LIKE LINE OF lt_link.
    DATA temp123 LIKE sy-tabix.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    FIELD-SYMBOLS <temp124> LIKE LINE OF lt_cell.
    DATA temp125 LIKE sy-tabix.
    FIELD-SYMBOLS <temp126> LIKE LINE OF lt_cell.
    DATA temp127 LIKE sy-tabix.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx( book( ) ).

    
    CLEAR temp118.
    INSERT `A1:C1` INTO TABLE temp118.
    cl_abap_unit_assert=>assert_equals( exp = temp118
                                        act = lo_xlsx->get_merged( ) ).
    
    lt_link = lo_xlsx->get_hyperlinks( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_link ) ).
    
    
    temp121 = sy-tabix.
    READ TABLE lt_link INDEX 1 ASSIGNING <temp120>.
    sy-tabix = temp121.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `A1`
                                        act = <temp120>-cell ).
    
    
    temp123 = sy-tabix.
    READ TABLE lt_link INDEX 1 ASSIGNING <temp122>.
    sy-tabix = temp123.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `https://example.com`
                                        act = <temp122>-target ).

    " the first cell of a merged range in every cell of it
    
    lt_cell = lo_xlsx->get_cells( range       = `A1:C1`
                                        fill_merged = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_cell ) ).
    
    
    temp125 = sy-tabix.
    READ TABLE lt_cell INDEX 3 ASSIGNING <temp124>.
    sy-tabix = temp125.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Title`
                                        act = <temp124>-value ).
    
    
    temp127 = sy-tabix.
    READ TABLE lt_cell INDEX 3 ASSIGNING <temp126>.
    sy-tabix = temp127.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = <temp126>-col ).
  ENDMETHOD.

  METHOD hidden_parts.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA temp128 TYPE string_table.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    DATA temp221 TYPE xsdboolean.
    DATA temp1 LIKE sy-subrc.
    DATA temp222 TYPE xsdboolean.
    DATA temp2 LIKE sy-subrc.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx( book( ) ).

    
    CLEAR temp128.
    INSERT `Report` INTO TABLE temp128.
    INSERT `Tab` INTO TABLE temp128.
    cl_abap_unit_assert=>assert_equals( exp = temp128
                                        act = lo_xlsx->get_sheet_names( no_hidden = abap_true ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lo_xlsx->get_sheet_names( ) ) ).

    " row 5 and column B are hidden
    
    lt_cell = lo_xlsx->get_cells( range       = `A3:C6`
                                        skip_hidden = abap_true ).
    
    
    READ TABLE lt_cell WITH KEY row = 5 TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.
    temp221 = boolc( temp1 = 0 ).
    cl_abap_unit_assert=>assert_false( temp221 ).
    
    
    READ TABLE lt_cell WITH KEY col = 2 TRANSPORTING NO FIELDS.
    temp2 = sy-subrc.
    temp222 = boolc( temp2 = 0 ).
    cl_abap_unit_assert=>assert_false( temp222 ).
    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = lines( lt_cell ) ).
  ENDMETHOD.

  METHOD names_and_tables.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA lt_name TYPE zabaputil_cl_util_xlsx=>ty_t_defined_name.
    FIELD-SYMBOLS <temp130> LIKE LINE OF lt_name.
    DATA temp131 LIKE sy-tabix.
    DATA lt_table TYPE zabaputil_cl_util_xlsx=>ty_t_excel_table.
    FIELD-SYMBOLS <temp132> LIKE LINE OF lt_table.
    DATA temp133 LIKE sy-tabix.
    FIELD-SYMBOLS <temp134> LIKE LINE OF lt_table.
    DATA temp135 LIKE sy-tabix.
    FIELD-SYMBOLS <temp136> LIKE LINE OF lt_table.
    DATA temp137 LIKE sy-tabix.
    FIELD-SYMBOLS <temp138> LIKE LINE OF lt_table.
    DATA temp139 LIKE sy-tabix.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx( book( ) ).

    
    lt_name = lo_xlsx->get_defined_names( ).
    
    
    temp131 = sy-tabix.
    READ TABLE lt_name WITH KEY name = `Block` ASSIGNING <temp130>.
    sy-tabix = temp131.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Report!$A$3:$C$6`
                                        act = <temp130>-ref ).
    
    lt_table = lo_xlsx->get_excel_tables( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_table ) ).
    
    
    temp133 = sy-tabix.
    READ TABLE lt_table INDEX 1 ASSIGNING <temp132>.
    sy-tabix = temp133.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Orders`
                                        act = <temp132>-name ).
    
    
    temp135 = sy-tabix.
    READ TABLE lt_table INDEX 1 ASSIGNING <temp134>.
    sy-tabix = temp135.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `Tab`
                                        act = <temp134>-sheet ).
    
    
    temp137 = sy-tabix.
    READ TABLE lt_table INDEX 1 ASSIGNING <temp136>.
    sy-tabix = temp137.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `B2:D6`
                                        act = <temp136>-ref ).
    
    
    temp139 = sy-tabix.
    READ TABLE lt_table INDEX 1 ASSIGNING <temp138>.
    sy-tabix = temp139.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = <temp138>-totals_rows ).
  ENDMETHOD.

  METHOD table_by_name.
    DATA lt_back TYPE ty_t_row.

    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    FIELD-SYMBOLS <temp140> LIKE LINE OF lt_back.
    DATA temp141 LIKE sy-tabix.
    FIELD-SYMBOLS <temp142> LIKE LINE OF lt_back.
    DATA temp143 LIKE sy-tabix.
    DATA lr_any TYPE REF TO data.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp223 TYPE xsdboolean.
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx( book( ) ).
    " an Excel table: its rows, without the totals row
    lo_xlsx->get_table( EXPORTING table_name = `orders`
                        IMPORTING result     = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_back ) ).
    
    
    temp141 = sy-tabix.
    READ TABLE lt_back INDEX 3 ASSIGNING <temp140>.
    sy-tabix = temp141.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = <temp140>-num ).
    " a defined name: its range, the header in its first row
    lo_xlsx->get_table( EXPORTING table_name = `Block`
                        IMPORTING result     = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_back ) ).
    
    
    temp143 = sy-tabix.
    READ TABLE lt_back INDEX 3 ASSIGNING <temp142>.
    sy-tabix = temp143.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `c`
                                        act = <temp142>-text ).
    
    lr_any = lo_xlsx->get_table_ref( table_name = `Orders` ).
    
    ASSIGN lr_any->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( <tab> ) ).
    TRY.
        lo_xlsx->get_table( EXPORTING table_name = `nope`
                            IMPORTING result     = lt_back ).
        cl_abap_unit_assert=>fail( `a name the workbook does not have has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp223 = boolc( x->get_text( ) CS `XLSX_NAME_UNKNOWN` ).
        cl_abap_unit_assert=>assert_true( temp223 ).
    ENDTRY.
  ENDMETHOD.

  METHOD cell_formats.
    DATA temp144 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp46 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp47 LIKE LINE OF temp46.
    DATA lv_file TYPE xstring.
    DATA lt_format TYPE zabaputil_cl_util_xlsx=>ty_t_cell_format.
    DATA ls_head LIKE LINE OF lt_format.
    FIELD-SYMBOLS <temp48> LIKE LINE OF lt_format.
    DATA temp49 LIKE sy-tabix.
    DATA ls_text LIKE LINE OF lt_format.
    FIELD-SYMBOLS <temp50> LIKE LINE OF lt_format.
    DATA temp51 LIKE sy-tabix.
    FIELD-SYMBOLS <temp145> LIKE LINE OF lt_format.
    DATA temp146 LIKE sy-tabix.
    CLEAR temp144.
    CLEAR temp144-header_style.
    temp144-header_style-fill = `1F4E78`.
    temp144-header_style-color = `FFFFFF`.
    
    CLEAR temp46.
    
    CLEAR temp47.
    temp47-name = `TEXT`.
    CLEAR temp47-style.
    temp47-style-wrap = abap_true.
    temp47-style-halign = `center`.
    INSERT temp47 INTO TABLE temp46.
    CLEAR temp47.
    temp47-name = `NUM`.
    temp47-format = `#,##0.00`.
    INSERT temp47 INTO TABLE temp46.
    temp144-t_column = temp46.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = rows( )
        layout = temp144 ).
    
    lt_format = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_cell_formats( ).

    
    
    
    temp49 = sy-tabix.
    READ TABLE lt_format WITH KEY row = 1 col = 1 ASSIGNING <temp48>.
    sy-tabix = temp49.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_head = <temp48>.
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = ls_head-bold ).
    cl_abap_unit_assert=>assert_equals( exp = `FFFFFF`
                                        act = ls_head-color ).
    cl_abap_unit_assert=>assert_equals( exp = `1F4E78`
                                        act = ls_head-fill ).
    
    
    
    temp51 = sy-tabix.
    READ TABLE lt_format WITH KEY row = 2 col = 1 ASSIGNING <temp50>.
    sy-tabix = temp51.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_text = <temp50>.
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = ls_text-wrap ).
    cl_abap_unit_assert=>assert_equals( exp = `center`
                                        act = ls_text-halign ).
    
    
    temp146 = sy-tabix.
    READ TABLE lt_format WITH KEY row = 3 col = 2 ASSIGNING <temp145>.
    sy-tabix = temp146.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `#,##0.00`
                                        act = <temp145>-format ).
    " the two header cells and the cells of the two columns of three rows
    cl_abap_unit_assert=>assert_equals( exp = 8
                                        act = lines( lt_format ) ).
  ENDMETHOD.

  METHOD text_notations.
    DATA lt_back TYPE ty_t_amount.

    DATA lv_file TYPE xstring.
    DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    FIELD-SYMBOLS <temp147> LIKE LINE OF lt_back.
    DATA temp148 LIKE sy-tabix.
    FIELD-SYMBOLS <temp149> LIKE LINE OF lt_back.
    DATA temp150 LIKE sy-tabix.
    FIELD-SYMBOLS <temp151> LIKE LINE OF lt_back.
    DATA temp152 LIKE sy-tabix.
    FIELD-SYMBOLS <temp153> LIKE LINE OF lt_back.
    DATA temp154 LIKE sy-tabix.
    FIELD-SYMBOLS <temp155> LIKE LINE OF lt_back.
    DATA temp156 LIKE sy-tabix.
    FIELD-SYMBOLS <temp157> LIKE LINE OF lt_back.
    DATA temp158 LIKE sy-tabix.
    DATA temp2 TYPE REF TO zabaputil_cl_util_xlsx.
    FIELD-SYMBOLS <temp159> LIKE LINE OF lt_back.
    DATA temp160 LIKE sy-tabix.
    lv_file = ltcl_file=>pack(
        `<row r="1"><c r="A1" t="inlineStr"><is><t>amount</t></is></c><c r="B1" t="inlineStr"><is><t>date</t></is></c></row>`
     && `<row r="2"><c r="A2" t="inlineStr"><is><t>1.234,56-</t></is></c><c r="B2" t="inlineStr"><is><t>03/15/2024</t></is></c></row>`
     && `<row r="3"><c r="A3" t="inlineStr"><is><t>EUR 12.50</t></is></c><c r="B3" t="inlineStr"><is><t>3/1/24</t></is></c></row>`
     && `<row r="4"><c r="A4" t="inlineStr"><is><t>12,5 %</t></is></c><c r="B4" t="inlineStr"><is><t>2024-12-01</t></is></c></row>` ).
    
    temp1 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp1->get_table( EXPORTING date_order = `MDY`
                                                                   IMPORTING result     = lt_back ).
    
    
    temp148 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp147>.
    sy-tabix = temp148.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `-1234.56`
                                        act = <temp147>-amount ).
    
    
    temp150 = sy-tabix.
    READ TABLE lt_back INDEX 2 ASSIGNING <temp149>.
    sy-tabix = temp150.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `12.5`
                                        act = <temp149>-amount ).
    
    
    temp152 = sy-tabix.
    READ TABLE lt_back INDEX 3 ASSIGNING <temp151>.
    sy-tabix = temp152.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `0.125`
                                        act = <temp151>-amount ).
    
    
    temp154 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp153>.
    sy-tabix = temp154.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `20240315`
                                        act = <temp153>-date ).
    
    
    temp156 = sy-tabix.
    READ TABLE lt_back INDEX 2 ASSIGNING <temp155>.
    sy-tabix = temp156.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `20240301`
                                        act = <temp155>-date ).
    
    
    temp158 = sy-tabix.
    READ TABLE lt_back INDEX 3 ASSIGNING <temp157>.
    sy-tabix = temp158.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `20241201`
                                        act = <temp157>-date ).
    " day first by default
    
    temp2 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp2->get_table( IMPORTING result = lt_back ).
    
    
    temp160 = sy-tabix.
    READ TABLE lt_back INDEX 2 ASSIGNING <temp159>.
    sy-tabix = temp160.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `20240103`
                                        act = <temp159>-date ).
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
    TYPES ty_t_row TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.

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
    DATA temp161 TYPE xstring.
    DATA lv_ue TYPE string.
    DATA temp162 TYPE xstring.
    DATA lv_oe TYPE string.
    temp161 = `C3BC`.
    
    lv_ue = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp161 ).
    
    temp162 = `C3B6`.
    
    lv_oe = zabaputil_cl_util_context=>conv_get_string_by_xstring( temp162 ).
    result = |  M{ lv_ue }ller & S{ lv_oe }hne <GmbH>  |.
  ENDMETHOD.

  METHOD rows.
    DATA temp163 TYPE ltcl_roundtrip=>ty_t_row.
    DATA temp164 LIKE LINE OF temp163.
    CLEAR temp163.
    
    CLEAR temp164.
    temp164-text = text_with_umlauts( ).
    temp164-code = `AB`.
    temp164-num = -5.
    temp164-amount = `-1234.5`.
    temp164-ratio = `0.25`.
    temp164-date = `20240315`.
    temp164-time = `103000`.
    temp164-numc = `0042`.
    temp164-flag = abap_true.
    temp164-bytes = `00FF`.
    INSERT temp164 INTO TABLE temp163.
    CLEAR temp164.
    INSERT temp164 INTO TABLE temp163.
    CLEAR temp164.
    temp164-text = |line 1{ cl_abap_char_utilities=>newline }line 2|.
    temp164-num = 2147483647.
    temp164-amount = `99999999.99`.
    temp164-date = `19000228`.
    temp164-time = `235959`.
    temp164-numc = `9999`.
    INSERT temp164 INTO TABLE temp163.
    result = temp163.
  ENDMETHOD.

  METHOD typed_table.
    DATA lt_back TYPE ty_t_row.

    DATA lt_rows TYPE ltcl_roundtrip=>ty_t_row.
    lt_rows = rows( ).
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_rows )
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_rows
                                        act = lt_back ).
  ENDMETHOD.

  METHOD two_sheets.
    DATA lt_back  TYPE ty_t_row.
    DATA lt_names TYPE string_table.
    DATA lt_text  TYPE string_table.

    DATA temp165 TYPE string_table.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA temp167 TYPE string_table.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp224 TYPE xsdboolean.
    CLEAR temp165.
    INSERT `one` INTO TABLE temp165.
    INSERT `two` INTO TABLE temp165.
    lt_names = temp165.
    
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx(
        zabaputil_cl_util_xlsx=>factory(
            )->add_sheet( val  = rows( )
                          name = `Rows`
            )->add_sheet( val  = lt_names
                          name = `Names`
            )->get_xlsx( ) ).

    
    CLEAR temp167.
    INSERT `Rows` INTO TABLE temp167.
    INSERT `Names` INTO TABLE temp167.
    cl_abap_unit_assert=>assert_equals( exp = temp167
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
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp224 = boolc( x->get_text( ) CS `XLSX_SHEET_UNKNOWN` ).
        cl_abap_unit_assert=>assert_true( temp224 ).
    ENDTRY.
    TRY.
        lo_xlsx->get_cells( index = 3 ).
        cl_abap_unit_assert=>fail( `a position behind the last sheet has to raise` ).
      CATCH zabaputil_cx_util_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD cells.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    DATA temp169 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp52> LIKE LINE OF lt_cell.
    DATA temp53 LIKE sy-tabix.
    DATA temp170 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp54> LIKE LINE OF lt_cell.
    DATA temp55 LIKE sy-tabix.
    DATA temp171 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp56> LIKE LINE OF lt_cell.
    DATA temp57 LIKE sy-tabix.
    DATA temp172 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp58> LIKE LINE OF lt_cell.
    DATA temp59 LIKE sy-tabix.
    DATA temp173 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp60> LIKE LINE OF lt_cell.
    DATA temp61 LIKE sy-tabix.
    DATA temp174 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp62> LIKE LINE OF lt_cell.
    DATA temp63 LIKE sy-tabix.
    DATA temp175 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp64> LIKE LINE OF lt_cell.
    DATA temp65 LIKE sy-tabix.
    FIELD-SYMBOLS <temp176> LIKE LINE OF lt_cell.
    DATA temp177 LIKE sy-tabix.
    FIELD-SYMBOLS <temp178> LIKE LINE OF lt_cell.
    DATA temp179 LIKE sy-tabix.
    DATA temp225 TYPE xsdboolean.
    DATA temp3 LIKE sy-subrc.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                        zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ) )->get_cells( ).

    " the header, then every cell of the first row by what it is
    
    CLEAR temp169.
    temp169-row = 1.
    temp169-col = 1.
    temp169-kind = `S`.
    temp169-value = `TEXT`.
    
    
    temp53 = sy-tabix.
    READ TABLE lt_cell INDEX 1 ASSIGNING <temp52>.
    sy-tabix = temp53.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp169
        act = <temp52> ).
    
    CLEAR temp170.
    temp170-row = 2.
    temp170-col = 1.
    temp170-kind = `S`.
    temp170-value = text_with_umlauts( ).
    
    
    temp55 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 col = 1 ASSIGNING <temp54>.
    sy-tabix = temp55.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp170
        act = <temp54> ).
    
    CLEAR temp171.
    temp171-row = 2.
    temp171-col = 3.
    temp171-kind = `N`.
    temp171-value = `-5`.
    
    
    temp57 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 col = 3 ASSIGNING <temp56>.
    sy-tabix = temp57.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp171
        act = <temp56> ).
    
    CLEAR temp172.
    temp172-row = 2.
    temp172-col = 4.
    temp172-kind = `N`.
    temp172-value = `-1234.5`.
    
    
    temp59 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 col = 4 ASSIGNING <temp58>.
    sy-tabix = temp59.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp172
        act = <temp58> ).
    
    CLEAR temp173.
    temp173-row = 2.
    temp173-col = 6.
    temp173-kind = `D`.
    temp173-value = `2024-03-15`.
    
    
    temp61 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 col = 6 ASSIGNING <temp60>.
    sy-tabix = temp61.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp173
        act = <temp60> ).
    
    CLEAR temp174.
    temp174-row = 2.
    temp174-col = 7.
    temp174-kind = `T`.
    temp174-value = `10:30:00`.
    
    
    temp63 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 col = 7 ASSIGNING <temp62>.
    sy-tabix = temp63.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp174
        act = <temp62> ).
    
    CLEAR temp175.
    temp175-row = 2.
    temp175-col = 9.
    temp175-kind = `B`.
    temp175-value = `X`.
    
    
    temp65 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 col = 9 ASSIGNING <temp64>.
    sy-tabix = temp65.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp175
        act = <temp64> ).
    " 1900-02-28, the last day before the one Excel made up
    
    
    temp177 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 4 col = 6 ASSIGNING <temp176>.
    sy-tabix = temp177.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `1900-02-28`
                                        act = <temp176>-value ).
    
    
    temp179 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 4 col = 7 ASSIGNING <temp178>.
    sy-tabix = temp179.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `23:59:59`
                                        act = <temp178>-value ).
    " a cell without a value is none
    
    
    READ TABLE lt_cell WITH KEY row = 3 col = 1 TRANSPORTING NO FIELDS.
    temp3 = sy-subrc.
    temp225 = boolc( temp3 = 0 ).
    cl_abap_unit_assert=>assert_false( temp225 ).
  ENDMETHOD.

  METHOD table_of_strings.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <amount> TYPE any.
    FIELD-SYMBOLS <date> TYPE any.
    FIELD-SYMBOLS <text> TYPE any.
    FIELD-SYMBOLS <num> TYPE any.
    FIELD-SYMBOLS <b> TYPE any.
    lr_tab = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                       zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ) )->get_table_ref( ).
    ASSIGN lr_tab->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( <tab> ) ).

    
    READ TABLE <tab> INDEX 1 ASSIGNING <row>.
    
    ASSIGN COMPONENT `AMOUNT` OF STRUCTURE <row> TO <amount>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = `-1234.5`
                                        act = <amount> ).
    
    ASSIGN COMPONENT `DATE` OF STRUCTURE <row> TO <date>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15`
                                        act = <date> ).
    " the second row holds its numbers and nothing else
    READ TABLE <tab> INDEX 2 ASSIGNING <row>.
    
    ASSIGN COMPONENT `TEXT` OF STRUCTURE <row> TO <text>.
    cl_abap_unit_assert=>assert_initial( <text> ).
    
    ASSIGN COMPONENT `NUM` OF STRUCTURE <row> TO <num>.
    cl_abap_unit_assert=>assert_equals( exp = `0`
                                        act = <num> ).

    " no header row: the columns by their letters, the header a row
    lr_tab = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                 zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) ) )->get_table_ref( header_row = 0 ).
    ASSIGN lr_tab->* TO <tab>.
    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( <tab> ) ).
    READ TABLE <tab> INDEX 1 ASSIGNING <row>.
    
    ASSIGN COMPONENT `B` OF STRUCTURE <row> TO <b>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = `CODE`
                                        act = <b> ).
  ENDMETHOD.

  METHOD headers_of_caller.
    DATA lt_back TYPE ty_t_row.

    DATA temp180 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp181 LIKE LINE OF temp180.
    DATA lt_column LIKE temp180.
    DATA temp182 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_file TYPE xstring.
    DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    FIELD-SYMBOLS <temp183> LIKE LINE OF lt_back.
    DATA temp184 LIKE sy-tabix.
    FIELD-SYMBOLS <temp185> LIKE LINE OF lt_back.
    DATA temp186 LIKE sy-tabix.
    FIELD-SYMBOLS <temp187> LIKE LINE OF lt_back.
    DATA temp188 LIKE sy-tabix.
    DATA temp2 TYPE REF TO zabaputil_cl_util_xlsx.
    CLEAR temp180.
    
    temp181-name = `NUM`.
    temp181-header = `Anzahl`.
    INSERT temp181 INTO TABLE temp180.
    temp181-name = `TEXT`.
    temp181-header = `Bezeichnung`.
    INSERT temp181 INTO TABLE temp180.
    
    lt_column = temp180.
    
    CLEAR temp182.
    temp182-t_column = lt_column.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                          layout = temp182 ).

    " the headers that wrote the columns read them back
    
    temp1 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp1->get_table( EXPORTING t_column = lt_column
                                                                   IMPORTING result   = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_back ) ).
    
    
    temp184 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp183>.
    sy-tabix = temp184.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = -5
                                        act = <temp183>-num ).
    
    
    temp186 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp185>.
    sy-tabix = temp186.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = text_with_umlauts( )
                                        act = <temp185>-text ).
    
    
    temp188 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp187>.
    sy-tabix = temp188.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp187>-amount ).

    " without them no column has a component: no row
    
    temp2 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp2->get_table( IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_initial( lt_back ).
  ENDMETHOD.

  METHOD header_as_name.
    TYPES:
      BEGIN OF ty_s_order,
        order_id TYPE i,
        net_eur  TYPE p LENGTH 8 DECIMALS 2,
      END OF ty_s_order.
    TYPES temp6 TYPE STANDARD TABLE OF ty_s_order WITH DEFAULT KEY.
DATA lt_order TYPE temp6.
    TYPES temp7 TYPE STANDARD TABLE OF ty_s_order WITH DEFAULT KEY.
DATA lt_back  TYPE temp7.

    DATA temp189 LIKE lt_order.
    DATA temp190 LIKE LINE OF temp189.
    DATA temp191 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA temp66 TYPE zabaputil_cl_util_xlsx=>ty_t_column.
    DATA temp67 LIKE LINE OF temp66.
    DATA lv_file TYPE xstring.
    CLEAR temp189.
    
    temp190-order_id = 4711.
    temp190-net_eur = `10.5`.
    INSERT temp190 INTO TABLE temp189.
    lt_order = temp189.
    " a header as a person writes it finds its component: ` Order Id `
    
    CLEAR temp191.
    
    CLEAR temp66.
    
    temp67-name = `ORDER_ID`.
    temp67-header = ` Order Id `.
    INSERT temp67 INTO TABLE temp66.
    temp67-name = `NET_EUR`.
    temp67-header = `net eur`.
    INSERT temp67 INTO TABLE temp66.
    temp191-t_column = temp66.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab(
        val    = lt_order
        layout = temp191 ).
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_order
                                        act = lt_back ).
  ENDMETHOD.

  METHOD without_header.
    DATA lt_back TYPE ty_t_row.

    DATA temp192 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lv_file TYPE xstring.
    DATA temp1 TYPE REF TO zabaputil_cl_util_xlsx.
    CLEAR temp192.
    temp192-no_header = abap_true.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                          layout = temp192 ).
    
    temp1 = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file ).
    temp1->get_table( EXPORTING header_row = 0
                                                                   IMPORTING result     = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = rows( )
                                        act = lt_back ).
  ENDMETHOD.

  METHOD elementary_line.
    TYPES temp8 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lt_num  TYPE temp8.
    TYPES temp9 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lt_back TYPE temp9.

    DATA temp193 LIKE lt_num.
    DATA lv_file TYPE xstring.
    DATA temp226 TYPE xsdboolean.
    CLEAR temp193.
    INSERT 3 INTO TABLE temp193.
    INSERT -1 INTO TABLE temp193.
    INSERT 0 INTO TABLE temp193.
    lt_num = temp193.
    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_num ).
    
    temp226 = boolc( ltcl_file=>part( file = lv_file name = `xl/worksheets/sheet1.xml` ) CS `>TABLE_LINE<` ).
    cl_abap_unit_assert=>assert_true( temp226 ).
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

    DATA temp195 TYPE string_table.
    CLEAR temp195.
    INSERT `a_x0041_b` INTO TABLE temp195.
    INSERT `_x` INTO TABLE temp195.
    INSERT `=SUM(A1)` INTO TABLE temp195.
    INSERT `007` INTO TABLE temp195.
    lt_text = temp195.
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
    TYPES temp10 TYPE STANDARD TABLE OF ty_s_stamp WITH DEFAULT KEY.
DATA lt_stamp TYPE temp10.
    TYPES temp11 TYPE STANDARD TABLE OF ty_s_stamp WITH DEFAULT KEY.
DATA lt_back  TYPE temp11.

    DATA temp197 LIKE lt_stamp.
    DATA temp198 LIKE LINE OF temp197.
    FIELD-SYMBOLS <temp199> LIKE LINE OF lt_stamp.
    DATA temp200 LIKE sy-tabix.
    DATA lv_file TYPE xstring.
    DATA temp227 TYPE xsdboolean.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    FIELD-SYMBOLS <temp201> LIKE LINE OF lt_cell.
    DATA temp202 LIKE sy-tabix.
    DATA temp203 LIKE lt_stamp.
    DATA temp204 LIKE LINE OF temp203.
    CLEAR temp197.
    
    CLEAR temp198.
    temp198-stamp = `20240315103000`.
    INSERT temp198 INTO TABLE temp197.
    CLEAR temp198.
    INSERT temp198 INTO TABLE temp197.
    lt_stamp = temp197.
    " a timestamp is a packed number that says what it is by its name
    " only - a runtime that does not hand the name on writes the number
    
    
    temp200 = sy-tabix.
    READ TABLE lt_stamp INDEX 1 ASSIGNING <temp199>.
    sy-tabix = temp200.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    IF zabaputil_cl_util_context=>rtti_get_type_name( <temp199>-stamp ) <> `TIMESTAMP`.
      RETURN.
    ENDIF.

    
    lv_file = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_stamp ).
    " the day and the fraction of the day, in the date-and-time format
    
    temp227 = boolc( ltcl_file=>part( file = lv_file name = `xl/worksheets/sheet1.xml` ) CS `<c r="A2" s="1"><v>45366.4375</v></c>` ).
    cl_abap_unit_assert=>assert_true( temp227 ).
    
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_file )->get_cells( ).
    
    
    temp202 = sy-tabix.
    READ TABLE lt_cell WITH KEY row = 2 ASSIGNING <temp201>.
    sy-tabix = temp202.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15T10:30:00`
                                        act = <temp201>-value ).

    
    CLEAR temp203.
    
    temp204-stamp = `20240315103000`.
    INSERT temp204 INTO TABLE temp203.
    lt_stamp = temp203.
    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = lv_file
                                          IMPORTING result = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = lt_stamp
                                        act = lt_back ).
  ENDMETHOD.

  METHOD start_cell.
    " rows that do not start in A1 are read by the row their header is in
    DATA lt_back TYPE ty_t_row.

    DATA temp205 TYPE zabaputil_cl_util_xlsx=>ty_s_layout.
    DATA lo_xlsx TYPE REF TO zabaputil_cl_util_xlsx.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    DATA temp206 TYPE zabaputil_cl_util_xlsx=>ty_s_cell.
    FIELD-SYMBOLS <temp68> LIKE LINE OF lt_cell.
    DATA temp69 LIKE sy-tabix.
    CLEAR temp205.
    temp205-start_cell = `C5`.
    
    lo_xlsx = zabaputil_cl_util_xlsx=>factory_by_xlsx(
                        zabaputil_cl_util_xlsx=>xlsx_by_itab( val    = rows( )
                                                              layout = temp205 ) ).
    lo_xlsx->get_table( EXPORTING header_row = 5
                        IMPORTING result     = lt_back ).
    cl_abap_unit_assert=>assert_equals( exp = rows( )
                                        act = lt_back ).

    
    lt_cell = lo_xlsx->get_cells( ).
    
    CLEAR temp206.
    temp206-row = 5.
    temp206-col = 3.
    temp206-kind = `S`.
    temp206-value = `TEXT`.
    
    
    temp69 = sy-tabix.
    READ TABLE lt_cell INDEX 1 ASSIGNING <temp68>.
    sy-tabix = temp69.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        exp = temp206
        act = <temp68> ).
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
    TYPES temp12 TYPE STANDARD TABLE OF ty_s_text WITH DEFAULT KEY.
DATA lt_back TYPE temp12.
    FIELD-SYMBOLS <temp207> LIKE LINE OF lt_back.
    DATA temp208 LIKE sy-tabix.
    FIELD-SYMBOLS <temp209> LIKE LINE OF lt_back.
    DATA temp210 LIKE sy-tabix.
    FIELD-SYMBOLS <temp211> LIKE LINE OF lt_back.
    DATA temp212 LIKE sy-tabix.
    FIELD-SYMBOLS <temp213> LIKE LINE OF lt_back.
    DATA temp214 LIKE sy-tabix.
    FIELD-SYMBOLS <temp215> LIKE LINE OF lt_back.
    DATA temp216 LIKE sy-tabix.

    zabaputil_cl_util_xlsx=>itab_by_xlsx( EXPORTING val    = zabaputil_cl_util_xlsx=>xlsx_by_itab( rows( ) )
                                          IMPORTING result = lt_back ).
    
    
    temp208 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp207>.
    sy-tabix = temp208.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `-5`
                                        act = <temp207>-num ).
    
    
    temp210 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp209>.
    sy-tabix = temp210.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `-1234.5`
                                        act = <temp209>-amount ).
    
    
    temp212 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp211>.
    sy-tabix = temp212.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `2024-03-15`
                                        act = <temp211>-date ).
    
    
    temp214 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp213>.
    sy-tabix = temp214.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `10:30:00`
                                        act = <temp213>-time ).
    
    
    temp216 = sy-tabix.
    READ TABLE lt_back INDEX 1 ASSIGNING <temp215>.
    sy-tabix = temp216.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = <temp215>-flag ).
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
    DATA ls_cell LIKE LINE OF t_cell.
    " kind and value in one, `-` for a cell that is not there
    result = `-`.
    
    LOOP AT t_cell INTO ls_cell.
      IF ls_cell-row = row AND ls_cell-col = col.
        result = |{ ls_cell-kind }:{ ls_cell-value }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD shared_strings.
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
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
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
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
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
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
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
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
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
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
    DATA lt_cell TYPE zabaputil_cl_util_xlsx=>ty_t_cell.
    lt_cell = zabaputil_cl_util_xlsx=>factory_by_xlsx( ltcl_file=>pack(
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
    TYPES temp13 TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.
DATA lt_row TYPE temp13.
    DATA ls_row LIKE LINE OF lt_row.
    FIELD-SYMBOLS <temp70> LIKE LINE OF lt_row.
    DATA temp71 LIKE sy-tabix.
    DATA temp217 TYPE d.
    DATA temp218 TYPE d.
    DATA temp219 TYPE d.
    DATA temp220 TYPE t.
    DATA temp221 TYPE t.
    DATA temp222 TYPE decfloat34.
    DATA temp72 TYPE decfloat34.
    DATA temp223 TYPE string.
    DATA temp224 TYPE d.

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
    
    
    
    temp71 = sy-tabix.
    READ TABLE lt_row INDEX 1 ASSIGNING <temp70>.
    sy-tabix = temp71.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_row = <temp70>.
    
    temp217 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp217
                                        act = ls_row-day ).
    
    temp218 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp218
                                        act = ls_row-day2 ).
    
    temp219 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp219
                                        act = ls_row-day3 ).
    
    temp220 = `103000`.
    cl_abap_unit_assert=>assert_equals( exp = temp220
                                        act = ls_row-at ).
    
    temp221 = `103000`.
    cl_abap_unit_assert=>assert_equals( exp = temp221
                                        act = ls_row-at2 ).
    
    temp222 = `1234.56`.
    
    temp72 = ls_row-amount.
    cl_abap_unit_assert=>assert_equals( exp = temp222
                                        act = temp72 ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = ls_row-flag ).
    
    temp223 = ls_row-numc.
    cl_abap_unit_assert=>assert_equals( exp = `000042`
                                        act = temp223 ).
    " a number in a date component that is no YYYYMMDD is the day Excel counts
    
    temp224 = `20240315`.
    cl_abap_unit_assert=>assert_equals( exp = temp224
                                        act = ls_row-serial ).
  ENDMETHOD.

  METHOD cell_that_does_not_fit.
    TYPES:
      BEGIN OF ty_s_row,
        num TYPE i,
        day TYPE d,
      END OF ty_s_row.
    TYPES temp14 TYPE STANDARD TABLE OF ty_s_row WITH DEFAULT KEY.
DATA lt_row TYPE temp14.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp228 TYPE xsdboolean.
        DATA temp229 TYPE xsdboolean.
        DATA temp230 TYPE xsdboolean.
        DATA temp231 TYPE xsdboolean.
        DATA temp232 TYPE xsdboolean.

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
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp228 = boolc( x->get_text( ) CS `XLSX_CELL_TYPE` ).
        cl_abap_unit_assert=>assert_true( temp228 ).
        
        temp229 = boolc( x->get_text( ) CS `cell A3` ).
        cl_abap_unit_assert=>assert_true( temp229 ).
        
        temp230 = boolc( x->get_text( ) CS `total` ).
        cl_abap_unit_assert=>assert_true( temp230 ).
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
        
        temp231 = boolc( x->get_text( ) CS `is no date` ).
        cl_abap_unit_assert=>assert_true( temp231 ).
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
        
        temp232 = boolc( x->get_text( ) CS `XLSX_CELL_ERROR` ).
        cl_abap_unit_assert=>assert_true( temp232 ).
    ENDTRY.
  ENDMETHOD.

  METHOD no_workbook.
        DATA temp225 TYPE xstring.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp233 TYPE xsdboolean.
        DATA temp226 TYPE xstring.
        DATA temp234 TYPE xsdboolean.
        DATA temp235 TYPE xsdboolean.
        DATA temp227 TYPE zabaputil_cl_util_context=>ty_t_zip_file.
        DATA temp228 LIKE LINE OF temp227.
        DATA temp73 TYPE xstring.
        DATA temp236 TYPE xsdboolean.
    " not a ZIP archive at all
    TRY.
        
        temp225 = `48656C6C6F2C20776F726C64`.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( temp225 ).
        cl_abap_unit_assert=>fail( `a file that is no workbook has to raise` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp233 = boolc( x->get_text( ) CS `XLSX_NO_WORKBOOK` ).
        cl_abap_unit_assert=>assert_true( temp233 ).
    ENDTRY.

    " the container of an .xls file of Excel 97-2003
    TRY.
        
        temp226 = `D0CF11E0A1B11AE10000000000000000`.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( temp226 ).
        cl_abap_unit_assert=>fail( `an .xls file has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        
        temp234 = boolc( x->get_text( ) CS `XLSX_NO_WORKBOOK` ).
        cl_abap_unit_assert=>assert_true( temp234 ).
        
        temp235 = boolc( x->get_text( ) CS `.xls` ).
        cl_abap_unit_assert=>assert_true( temp235 ).
    ENDTRY.

    " a ZIP archive that is no workbook
    TRY.
        
        CLEAR temp227.
        
        temp228-name = `readme.txt`.
        
        temp73 = `414243`.
        temp228-content = temp73.
        INSERT temp228 INTO TABLE temp227.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( zabaputil_cl_util_context=>zip_pack(
            temp227 ) ).
        cl_abap_unit_assert=>fail( `an archive without a workbook has to raise` ).
      CATCH zabaputil_cx_util_error INTO x.
        
        temp236 = boolc( x->get_text( ) CS `XLSX_NO_WORKBOOK` ).
        cl_abap_unit_assert=>assert_true( temp236 ).
    ENDTRY.
  ENDMETHOD.

  METHOD written_is_not_read.
    DATA lt_text TYPE string_table.
        DATA x TYPE REF TO zabaputil_cx_util_error.
        DATA temp237 TYPE xsdboolean.
        DATA temp238 TYPE xsdboolean.

    TRY.
        zabaputil_cl_util_xlsx=>factory( )->add_sheet( lt_text )->get_cells( ).
        cl_abap_unit_assert=>fail( `a workbook that is written is not read` ).
        
      CATCH zabaputil_cx_util_error INTO x.
        
        temp237 = boolc( x->get_text( ) CS `XLSX_NOT_READ` ).
        cl_abap_unit_assert=>assert_true( temp237 ).
    ENDTRY.
    TRY.
        zabaputil_cl_util_xlsx=>factory_by_xlsx( zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_text )
            )->add_sheet( lt_text ).
        cl_abap_unit_assert=>fail( `a workbook that was read takes no sheet` ).
      CATCH zabaputil_cx_util_error INTO x.
        
        temp238 = boolc( x->get_text( ) CS `XLSX_READ_ONLY` ).
        cl_abap_unit_assert=>assert_true( temp238 ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

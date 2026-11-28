[![ABAP_STANDARD](https://github.com/abap-util/abap-util/actions/workflows/ABAP_STANDARD.yaml/badge.svg)](https://github.com/abap-util/abap-util/actions/workflows/ABAP_STANDARD.yaml)
[![ABAP_CLOUD](https://github.com/abap-util/abap-util/actions/workflows/ABAP_CLOUD.yaml/badge.svg)](https://github.com/abap-util/abap-util/actions/workflows/ABAP_CLOUD.yaml)
[![ABAP_702](https://github.com/abap-util/abap-util/actions/workflows/ABAP_702.yaml/badge.svg)](https://github.com/abap-util/abap-util/actions/workflows/ABAP_702.yaml)
<br>
[![test_unit](https://github.com/abap-util/abap-util/actions/workflows/test_unit.yaml/badge.svg)](https://github.com/abap-util/abap-util/actions/workflows/test_unit.yaml)
[![test_rename](https://github.com/abap-util/abap-util/actions/workflows/test_rename.yaml/badge.svg)](https://github.com/abap-util/abap-util/actions/workflows/test_rename.yaml)
[![auto_downport](https://github.com/abap-util/abap-util/actions/workflows/auto_downport.yaml/badge.svg)](https://github.com/abap-util/abap-util/actions/workflows/auto_downport.yaml)

# abap-util
Utility functions for ABAP Cloud and Standard ABAP

📖 **[Documentation](https://abap-util.github.io/docs/)** — guides and full API reference

#### Key Features
* Simplified SAP APIs as class-based methods — one façade class, no function modules to remember
* One source for ABAP Cloud, Standard ABAP and NetWeaver 7.02 — release differences are hidden behind the methods
* Strings, JSON/XML/CSV/XLSX, PDF, RTTI, messages, logging, locks, calendar, transports, persistence, GS1 and custom barcodes, parallel processing, RFC, attachments and more
* Unit-tested without an SAP system — the tests run transpiled to JavaScript on every pull request

#### Compatibility
* S/4HANA Public Cloud and BTP ABAP Environment (ABAP Cloud)
* S/4HANA Private Cloud and on-premise (ABAP Cloud, Standard ABAP)
* NetWeaver AS ABAP 7.02 or higher (Standard ABAP)

Methods that need Standard ABAP (long texts, MIME repository, spool, forms, GOS, …) raise `zabaputil_cx_util_error` on ABAP Cloud.

#### Installation
Install via [abapGit](https://abapgit.org) - pick the branch that fits the system:

| Branch | For |
|---|---|
| `main` | NetWeaver 7.50 and higher, S/4HANA, ABAP Cloud |
| `702` | NetWeaver 7.02 to 7.4x - downported from `main` on every push |
| `rename_<name>` | an installation under another name, built on request (see [Renaming](#renaming)) |

#### Consumption Model — Copy, Don't Depend
abap-util is meant to be consumed **as a renamed copy, not as an installed dependency**. abapGit has no dependency management, so a hard dependency would force every user to install packages in the right order and keep versions in sync. Instead, this repository is the **master catalog with all utility classes and all methods**, and every project embeds a renamed copy of exactly the classes it needs. Only the context class is additionally **trimmed to the methods the project uses**, with the helpers they call:

| Project | Copies | Scope |
|---|---|---|
| [abap2UI5](https://github.com/abap2UI5/abap2UI5) | `z2ui5_cl_ui5_util_context`, `z2ui5_cl_ui5_util_http`, `z2ui5_cx_ui5_util_error` (`src/00/03/`) | context class trimmed to the methods of the core framework, the other two as-is |
| [popups](https://github.com/abap2UI5-addons/popups) | `z2ui5_cl_popup_context`, `z2ui5_cx_popup_error` (`src/00/`) | context class trimmed to the methods of the popup apps, the exception as-is |
| [business-api](https://github.com/abap-util/business-api) | `zabaputil_cl_bus_util_context`, `zabaputil_cx_bus_util_error` (`src/00/`) | context class trimmed to the `bapi_*` methods and `conv_alpha_in` with their helpers, the exception as-is |

Every consumer installs with zero dependencies and keeps its own namespace — several projects, even on different versions, coexist in one system. abap-util stays the place where all methods converge, are unit-tested and are kept compatible with every release:

* **Copy per class** — a project takes the classes it needs and renames them into its own namespace (business-api inserts `bus_`: `zabaputil_cl_util_context` becomes `zabaputil_cl_bus_util_context`).
* **Develop locally** — a method a project needs is written into its own copy first, no upstream detour.
* **Sync back** — what was added or fixed in a copy is merged back here regularly, so this catalog stays the superset and every other consumer can pick it up. Fix it here, then update the copies.

Not every class next to a copy has a master here: `z2ui5_cl_ui5_util_json_fl` is owned by abap2UI5, and abap2UI5's `src/99/01/` holds frozen copies from before this model that receive no fixes. Neither is a source for the sync.

#### Usage

###### Strings
```abap
DATA(lv_upper) = zabaputil_cl_util_context=>c_trim_upper( ` Hello World  ` ). " => 'HELLO WORLD'
DATA(lv_lower) = zabaputil_cl_util_context=>c_trim_lower( ` Hello World  ` ). " => 'hello world'
DATA(lv_trim)  = zabaputil_cl_util_context=>c_trim( ` Hello World  ` ).       " => 'Hello World'

" Predicates
DATA(lv_has)   = zabaputil_cl_util_context=>c_contains(    val = `Hello World`  sub    = `World` ). " => abap_true
DATA(lv_pre)   = zabaputil_cl_util_context=>c_starts_with( val = `Hello World`  prefix = `Hello` ). " => abap_true
DATA(lv_suf)   = zabaputil_cl_util_context=>c_ends_with(   val = `Hello World`  suffix = `World` ). " => abap_true

" Split & Join
DATA(lt_parts) = zabaputil_cl_util_context=>c_split( val = `a,b,c`  sep = `,` ). " => ('a' 'b' 'c')
DATA(lv_csv)   = zabaputil_cl_util_context=>c_join(  tab = lt_parts  sep = `;` ). " => 'a;b;c'

" Padding, truncating, replacing
DATA(lv_pad)   = zabaputil_cl_util_context=>c_pad_left(    val = `42`  len = 5 ).            " => '00042'
DATA(lv_cut)   = zabaputil_cl_util_context=>c_truncate(    val = `Hello World`  max = 8 ).   " => 'Hello...'
DATA(lv_rep)   = zabaputil_cl_util_context=>c_replace_all( val = `a-b-c`  sub = `-`  new_val = `_` ). " => 'a_b_c'
DATA(lv_sub)   = zabaputil_cl_util_context=>c_substring_safe( val = `abc`  off = 1  len = 99 ). " no dump => 'bc'
DATA(lv_blank) = zabaputil_cl_util_context=>c_is_blank( `   ` ). " => abap_true
```

###### Validation
```abap
DATA(lv_ok1) = zabaputil_cl_util_context=>check_is_email(          `mail@example.com` ). " => abap_true
DATA(lv_ok2) = zabaputil_cl_util_context=>check_is_numeric_string( `12.34` ).            " => abap_true
DATA(lv_ok3) = zabaputil_cl_util_context=>check_is_date_valid(     `2025-02-30` ).       " => abap_false
DATA(lv_ok4) = zabaputil_cl_util_context=>check_is_guid(           lv_uuid ).
DATA(lv_ok5) = zabaputil_cl_util_context=>check_max_length( val = lv_input  max = 40 ).
```

###### JSON
```abap
" Structure to JSON
DATA(lv_json) = zabaputil_cl_util_context=>json_stringify( ls_flight ).
" => '{"CARRID":"LH","CONNID":"0400","FLDATE":"2025-01-15"}'

" JSON to Structure
zabaputil_cl_util_context=>json_parse(
    val  = lv_json
    data = ls_flight ).
```

###### XML
```abap
" Serialize any ABAP data to XML
DATA(lv_xml) = zabaputil_cl_util_context=>xml_stringify( ls_data ).

" Deserialize XML back to ABAP data
zabaputil_cl_util_context=>xml_parse(
    xml = lv_xml
    any = ls_data ).
```

###### UUID
```abap
DATA(lv_uuid_32) = zabaputil_cl_util_context=>uuid_get_c32( ). " => '550E8400E29B41D4A716446655440000'
DATA(lv_uuid_22) = zabaputil_cl_util_context=>uuid_get_c22( ). " => 'VQ6EAOKbQdSnFkRmVU'
DATA(lv_uuid_36) = zabaputil_cl_util_context=>uuid_get_c36( ). " => '550E8400-E29B-41D4-A716-446655440000'

" Convert between formats
DATA(lv_c36) = zabaputil_cl_util_context=>uuid_conv_c32_to_c36( lv_uuid_32 ).
DATA(lv_c32) = zabaputil_cl_util_context=>uuid_conv_c36_to_c32( lv_c36 ).
DATA(lv_c22) = zabaputil_cl_util_context=>uuid_conv_c32_to_c22( lv_uuid_32 ).
```

###### Escaping & URL Encoding
```abap
DATA(lv_html) = zabaputil_cl_util_context=>c_escape_html( `Tom & <b>Jerry</b>` ). " => 'Tom &amp; &lt;b&gt;Jerry&lt;/b&gt;'
DATA(lv_json) = zabaputil_cl_util_context=>c_escape_json( `say "hi"` ).           " => 'say \"hi\"'

DATA(lv_enc) = zabaputil_cl_util_context=>url_encode( `a b&c=ä` ). " => 'a%20b%26c%3D%C3%A4'
DATA(lv_dec) = zabaputil_cl_util_context=>url_decode( lv_enc ).    " => 'a b&c=ä'
```

###### Regular Expressions
PCRE on ABAP Cloud, POSIX on Standard ABAP - same API everywhere.
```abap
DATA(lv_match) = zabaputil_cl_util_context=>regex_match(    val = `ID-4711`  regex = `^ID-\d+$` ). " => abap_true
DATA(lt_nums)  = zabaputil_cl_util_context=>regex_find_all( val = `a1 b22 c333`  regex = `\d+` ).  " => ('1' '22' '333')
DATA(lv_new)   = zabaputil_cl_util_context=>regex_replace_all(
    val     = `a1b2`
    regex   = `\d`
    new_val = `#` ). " => 'a#b#'
```

###### Hashing & HMAC
```abap
DATA(lv_hash) = zabaputil_cl_util_context=>hash_calculate( `Hello World` ).                   " SHA256 hex string
DATA(lv_sha1) = zabaputil_cl_util_context=>hash_calculate( val = `Hello`  algorithm = `SHA1` ).
DATA(lv_hmac) = zabaputil_cl_util_context=>hash_hmac( val = `payload`  key = `secret` ).      " HMAC-SHA256
```

###### Outbound HTTP Client
Unified API - `cl_web_http_client` on ABAP Cloud, `cl_http_client` on-premise.
```abap
DATA(ls_response) = zabaputil_cl_util_context=>http_get( `https://api.example.com/data` ).
" => ls_response-code = 200, ls_response-body = '...'

DATA(ls_result) = zabaputil_cl_util_context=>http_post(
    url          = `https://api.example.com/items`
    body         = lv_json
    content_type = `application/json`
    t_header     = VALUE #( ( n = `Authorization`  v = `Bearer ...` ) ) ).
```

###### Currency & Units
```abap
DATA(lv_decimals) = zabaputil_cl_util_context=>cur_get_decimals( `JPY` ). " => 0 (TCURX / I_Currency)

" Shift internal amounts to their display value and back
DATA(lv_display)  = zabaputil_cl_util_context=>cur_amount_to_external( val = lv_amount  currency = `JPY` ).
DATA(lv_internal) = zabaputil_cl_util_context=>cur_amount_to_internal( val = lv_display currency = `JPY` ).

" Unit conversion (UNIT_CONVERSION_SIMPLE / I_UnitOfMeasure)
DATA(lv_meters) = zabaputil_cl_util_context=>unit_convert( val = CONV decfloat34( 2 )  unit_from = `KM`  unit_to = `M` ).
```

###### Languages
```abap
DATA(lv_iso) = zabaputil_cl_util_context=>lang_sap_to_iso( `D` ).  " => 'de'
DATA(lv_sap) = zabaputil_cl_util_context=>lang_iso_to_sap( `en` ). " => 'E'
```

###### RTTI
```abap
" Check if a class exists
IF zabaputil_cl_util_context=>rtti_check_class_exists( 'ZCL_MY_CLASS' ) = abap_true.
  " ...
ENDIF.

" Get type name of any variable
DATA(lv_type) = zabaputil_cl_util_context=>rtti_get_type_name( ls_flight ). " => 'SFLIGHT'

" Create dynamic internal table by DDIC name
DATA(lr_table) = zabaputil_cl_util_context=>rtti_create_tab_by_name( 'SFLIGHT' ).

" Get component list of a structure
DATA(lt_components) = zabaputil_cl_util_context=>rtti_get_t_attri_by_any( ls_flight ).
```

###### URL Parameters
```abap
" Read a parameter from URL
DATA(lv_id) = zabaputil_cl_util_context=>url_param_get(
    val = 'ID'
    url = '/sap/bc/http/myapp?ID=12345&MODE=edit' ). " => '12345'

" Build a URL from parameters
DATA(lv_url) = zabaputil_cl_util_context=>url_param_create_url(
    t_params = VALUE #(
        ( n = 'ID'   v = '12345' )
        ( n = 'MODE' v = 'edit' ) ) ). " => 'ID=12345&MODE=edit'

" Add/update a parameter
DATA(lv_new_url) = zabaputil_cl_util_context=>url_param_set(
    url   = '/app?ID=1'
    name  = 'LANG'
    value = 'DE' ). " => '/app?ID=1&LANG=DE'
```

###### Base64 & Encoding
```abap
" String <-> xstring
DATA(lv_xstring) = zabaputil_cl_util_context=>conv_get_xstring_by_string( 'Hello' ).
DATA(lv_string)  = zabaputil_cl_util_context=>conv_get_string_by_xstring( lv_xstring ).

" Base64 encode/decode
DATA(lv_base64)  = zabaputil_cl_util_context=>conv_encode_x_base64( lv_xstring ).
DATA(lv_decoded) = zabaputil_cl_util_context=>conv_decode_x_base64( lv_base64 ).
```

###### CSV
```abap
" Internal table to CSV
DATA(lv_csv) = zabaputil_cl_util_context=>itab_get_csv_by_itab( lt_flights ).

" CSV to internal table (returns data reference)
DATA(lr_itab) = zabaputil_cl_util_context=>itab_get_itab_by_csv( lv_csv ).
```

###### XLSX / Excel
`zabaputil_cl_util_xlsx` writes and reads `.xlsx` workbooks in plain ABAP - no abap2xlsx, no XCO, the same code on ABAP Cloud, Standard ABAP and 7.02.
```abap
" Internal table to Excel and back, in one call each
DATA(lv_xlsx) = zabaputil_cl_util_xlsx=>xlsx_by_itab( lt_flights ).

zabaputil_cl_util_xlsx=>itab_by_xlsx(
    EXPORTING val    = lv_xlsx
    IMPORTING result = lt_flights ).   " by the header row, every value in the type of its component

" Several sheets, with a layout
DATA(lv_book) = zabaputil_cl_util_xlsx=>factory(
    )->add_sheet( val  = lt_flights
                  name = `Flights`
    )->add_sheet( val    = lt_bookings
                  name   = `Bookings`
                  layout = VALUE #( freeze_header = abap_true
                                    table_style   = `TableStyleMedium2`
                                    t_column      = VALUE #( ( name = `BOOKID` header = `Booking` )
                                                             ( name = `PRICE`  header = `Price` format = `#,##0.00` )
                                                             ( name = `FLDATE` header = `Date` width = 14 ) ) )
    )->get_xlsx( ).

" Reading: the sheets, a sheet as a typed table, as a table of strings, cell by cell
DATA(lo_book)  = zabaputil_cl_util_xlsx=>factory_by_xlsx( lv_book ).
DATA(lt_names) = lo_book->get_sheet_names( ).                 " => ( 'Flights' 'Bookings' )
lo_book->get_table( EXPORTING sheet  = `Flights`
                    IMPORTING result = lt_flights ).
DATA(lr_any)   = lo_book->get_table_ref( index = 2 ).         " columns named after the header row
DATA(lt_cells) = lo_book->get_cells( sheet = `Bookings` ).    " row, col, kind, value

" A block that starts in B3, as an Excel table with a name, a formula column and a totals row
DATA(lv_report) = zabaputil_cl_util_xlsx=>xlsx_by_itab(
    val    = lt_bookings
    layout = VALUE #( start_cell     = `B3`
                      table_style    = `TableStyleMedium2`
                      table_name     = `Bookings`
                      column_stripes = abap_true
                      t_column       = VALUE #( ( name = `BOOKID` header = `Booking` total = `Total` )
                                                ( name = `SEATS`  header = `Seats`   total = `sum` )
                                                ( name = `PRICE`  header = `Price`   format = `#,##0.00` )
                                                ( header = `Amount` formula = `D{row}*E{row}`
                                                  format = `#,##0.00` total = `sum` ) ) ) ).
```
An Excel table can also go without its row stripes (`no_row_stripes`) or its filter buttons (`no_filter`). A totals cell is `sum`, `average`, `count`, `countNums`, `min`, `max`, `stdDev`, `var` or a label; in a formula `{row}` stands for the number of the row.

A number is written as a number, a date as a date, a time as a time, a timestamp as a date with its time, a flag as a boolean; a NUMC keeps its leading zeros. Reading tells a date from a number by the format of its cell and hands it back as `YYYY-MM-DD`. Not covered: formatting cell by cell, formulas in single cells (a formula cell is read with its last value), merged cells, charts, images, `.xls` files, workbooks with a password.

`zabaputil_cl_util_context=>conv_get_xlsx_by_itab( lt_flights )` stays the one-sheet writer inside the context class, for a copy that does not carry the XLSX class.

###### Internal Tables
```abap
" Move corresponding rows between tables of different row types
zabaputil_cl_util_context=>itab_corresponding(
    EXPORTING val = lt_source
    CHANGING  tab = lt_target ).

" Filter table in place by a substring across all clike fields
zabaputil_cl_util_context=>itab_filter_by_val(
    EXPORTING val = `LH`
    CHANGING  tab = lt_flights ).

" Project a flat structure to a list of name/value pairs
DATA(lt_kv) = zabaputil_cl_util_context=>itab_get_by_struc( ls_flight ).
" => [ ( name = 'CARRID' value = 'LH' ) ( name = 'CONNID' value = '0400' ) ... ]

" Sort, slice & paginate dynamically
zabaputil_cl_util_context=>itab_sort_by(
    EXPORTING fieldname = `PRICE`  descending = abap_true
    CHANGING  tab       = lt_flights ).

DATA(lr_top10) = zabaputil_cl_util_context=>itab_slice( tab = lt_flights  from = 1  to = 10 ).

zabaputil_cl_util_context=>itab_paginate(
    EXPORTING tab         = lt_flights  page = 2  page_size = 20
    IMPORTING result      = lr_page
              total_count = lv_count
              total_pages = lv_pages ).

" Count rows grouped by a field
DATA(lt_counts) = zabaputil_cl_util_context=>itab_count_by( tab = lt_flights  fieldname = `CARRID` ).
" => [ ( n = 'LH' v = '42' ) ( n = 'AA' v = '17' ) ... ]

" Aggregations
DATA(lv_total)    = zabaputil_cl_util_context=>itab_sum_by(       tab = lt_flights  fieldname = `PRICE` ).
DATA(lt_carriers) = zabaputil_cl_util_context=>itab_distinct(     tab = lt_flights  fieldname = `CARRID` ).
DATA(lt_sums)     = zabaputil_cl_util_context=>itab_group_sum_by( tab = lt_flights  group_by = `CARRID`  sum_by = `PRICE` ).
```

###### Data Compare & Access
```abap
" Deep equality for any two values
DATA(lv_same) = zabaputil_cl_util_context=>data_equals( a = ls_before  b = ls_after ).

" Field-by-field diff of two structures
DATA(lt_diff) = zabaputil_cl_util_context=>data_diff( old = ls_before  new = ls_after ).
" => [ ( fieldname = 'PRICE' old_value = '100' new_value = '120' ) ... ]

" Read/write nested fields by path
DATA(lv_city) = zabaputil_cl_util_context=>data_get_by_path( data = ls_order  path = `CUSTOMER-ADDRESS-CITY` ).
zabaputil_cl_util_context=>data_set_by_path(
    EXPORTING path  = `CUSTOMER-ADDRESS-CITY`  value = `Berlin`
    CHANGING  data  = ls_order ).
```

###### Range & Token Filters
```abap
" Build select-options ranges programmatically
DATA(ls_eq) = zabaputil_cl_util_range=>eq( `LH` ).        " I EQ 'LH'
DATA(ls_bt) = zabaputil_cl_util_range=>bt( low = `100` high = `200` ).
DATA(ls_cp) = zabaputil_cl_util_range=>cp( `LH*` ).        " I CP 'LH*'
DATA(ls_ne) = zabaputil_cl_util_range=>ne( `XX` ).         " E EQ 'XX'

" Token strings (e.g. from a UI search field) <-> ranges
DATA(ls_range) = zabaputil_cl_util_context=>filter_get_range_by_token( `>=100` ).
DATA(lt_token) = zabaputil_cl_util_context=>filter_get_token_t_by_range_t( lt_range ).

" Apply a multi-field filter on an internal table in place
zabaputil_cl_util_context=>filter_itab(
    EXPORTING filter = lt_filter
    CHANGING  val    = lt_flights ).
```

###### Messages
```abap
" Resolve any message-like input (sy, exception, T100 record, ...) to a single text
DATA(lv_text) = zabaputil_cl_util_msg=>msg_get_text( sy ).

" Get the current sy-msg* as a typed message table (type, id, number, text, ...)
DATA(lt_msg)  = zabaputil_cl_util_msg=>msg_get_by_sy( ).

" Map an arbitrary value to a message text via a configurable mapping
DATA(lv_mapped) = zabaputil_cl_util_msg=>msg_map( name = `STATUS` ).

" Resolve a T100 message with placeholders
DATA(lv_t100) = zabaputil_cl_util_context=>text_get( msgid = `ZMY_MESSAGES`  msgno = `001`  v1 = `42` ).

" Domain fixed values as text (enum-style)
DATA(lv_text) = zabaputil_cl_util_context=>enum_to_text( domain = `ZSTATUS`  value = `A` ). " => 'Active'
DATA(lt_all)  = zabaputil_cl_util_context=>enum_get_all( domain = `ZSTATUS` ).
```

###### Timestamps
```abap
DATA(lv_now)  = zabaputil_cl_util_context=>time_get_timestampl( ).
DATA(lv_past) = zabaputil_cl_util_context=>time_subtract_seconds( time = lv_now  seconds = 3600 ).
DATA(lv_next) = zabaputil_cl_util_context=>time_add_seconds(      time = lv_now  seconds = 60 ).
DATA(lv_diff) = zabaputil_cl_util_context=>time_diff_seconds(     time_from = lv_past  time_to = lv_now ).
DATA(lv_date) = zabaputil_cl_util_context=>time_get_date_by_stampl( lv_now ).
DATA(lv_time) = zabaputil_cl_util_context=>time_get_time_by_stampl( lv_now ).

" Build a timestampl from date/time
DATA(lv_stmp) = zabaputil_cl_util_context=>time_get_stampl_by_date_time(
    date = sy-datum
    time = sy-uzeit ).

" Date <-> string with format pattern
DATA(lv_iso)  = zabaputil_cl_util_context=>conv_date_to_string(   val = sy-datum    format = `YYYY-MM-DD` ).
DATA(lv_dat)  = zabaputil_cl_util_context=>conv_string_to_date(   val = `2024-03-15` format = `YYYY-MM-DD` ).

" Stopwatch - measure runtime in milliseconds
DATA(lv_start) = zabaputil_cl_util_context=>time_measure_start( ).
" ... do work ...
DATA(lv_ms)    = zabaputil_cl_util_context=>time_measure_stop( lv_start ).

" Time zones
DATA(lv_tz) = zabaputil_cl_util_context=>time_get_user_timezone( ).
zabaputil_cl_util_context=>time_stampl_to_tz(
    EXPORTING val  = lv_now
              tz   = `CET`
    IMPORTING date = lv_date
              time = lv_time ).
DATA(lv_stamp) = zabaputil_cl_util_context=>time_stampl_from_tz( date = lv_date  time = lv_time  tz = `CET` ).
```

###### Calendar & Workdays
```abap
DATA(lv_wd)   = zabaputil_cl_util_context=>cal_get_weekday( sy-datum ).  " 1 = Monday ... 7 = Sunday
DATA(lv_we)   = zabaputil_cl_util_context=>cal_is_weekend(  sy-datum ).
DATA(lv_work) = zabaputil_cl_util_context=>cal_is_workday(  sy-datum ).  " optional: calendar_id for factory calendars

" Workday arithmetic
DATA(lv_due)  = zabaputil_cl_util_context=>cal_add_workdays(   date = sy-datum  days = 5 ).
DATA(lv_days) = zabaputil_cl_util_context=>cal_count_workdays( date_from = lv_start_date  date_to = lv_end_date ).
```

###### Barcodes (GS1)
```abap
" Parse a scanned GS1-128 / GS1 DataMatrix / GS1 QR code (raw with GS separators or human-readable)
DATA(lt_gs1) = zabaputil_cl_util_context=>barcode_gs1_parse( `(01)09506000134352(17)251231(10)ABC123` ).
" => ai = `01` title = `GTIN` value = `09506000134352`
"    ai = `17` title = `USE BY or EXPIRY` value = `251231` date = 20251231
"    ai = `10` title = `BATCH/LOT` value = `ABC123`

" Scanner sends `|` instead of GS? Pass it as separator
lt_gs1 = zabaputil_cl_util_context=>barcode_gs1_parse( val = lv_scan  separator = `|` ).

" Read single values - a three-digit AI also finds the measure AIs (3103 = kg with 3 decimals)
DATA(lv_batch)  = zabaputil_cl_util_context=>barcode_gs1_get_value( t_gs1 = lt_gs1  ai = `10` ).
DATA(lv_weight) = zabaputil_cl_util_context=>barcode_gs1_get_value( t_gs1 = lt_gs1  ai = `310` ).

" Build the element string for a label (GS after variable-length fields)
DATA(lv_code)  = zabaputil_cl_util_context=>barcode_gs1_build( lt_gs1 ).
DATA(lv_text)  = zabaputil_cl_util_context=>barcode_gs1_build( t_gs1 = lt_gs1  human_readable = abap_true ).

" Check digits of GTIN, GLN, SSCC
DATA(lv_digit) = zabaputil_cl_util_context=>barcode_gs1_calc_check_digit( `0950600013435` ). " => '2'
DATA(lv_ok)    = zabaputil_cl_util_context=>check_is_gs1_check_digit( `09506000134352` ).    " => abap_true
```

###### Barcodes (own formats)
```abap
" A label that is not GS1: describe its fields once, then split every scan with it.
" Fields with a separator between them - the n-th part is the n-th field
DATA(lt_value) = zabaputil_cl_util_context=>barcode_split_separator(
    val       = `4711|LOT42|12`
    separator = `|`
    t_field   = VALUE #( ( name = `MATERIAL` ) ( name = `BATCH` ) ( name = `QUANTITY` optional = abap_true ) ) ).
" => name = `MATERIAL` value = `4711`, name = `BATCH` value = `LOT42`, name = `QUANTITY` value = `12`

" Fields at fixed places - pos counts from 1, len 0 takes the rest, padding is dropped
lt_value = zabaputil_cl_util_context=>barcode_split_position(
    val     = `0004711LOT42A012`
    t_field = VALUE #( ( name = `MATERIAL` pos = 1 len = 7 ) ( name = `BATCH` pos = 8 len = 6 ) ( name = `QUANTITY` pos = 14 ) ) ).

" Fields that say what they are by a prefix - one per scan, or several with GS (or `separator`) between them
lt_value = zabaputil_cl_util_context=>barcode_split_prefix(
    val     = `Q12`
    t_field = VALUE #( ( name = `MATERIAL` prefix = `P` optional = abap_true ) ( name = `QUANTITY` prefix = `Q` optional = abap_true ) ) ).
" => name = `QUANTITY` value = `12`

" A missing field that is not optional and a value longer than len raise zabaputil_cx_util_error
```

###### Error Handling
```abap
" Raise exception conditionally
zabaputil_cl_util_context=>x_check_raise( when = xsdbool( sy-subrc <> 0 ) ).

" Raise exception directly
zabaputil_cl_util_context=>x_raise( 'Something went wrong' ).

" Get last T100 message from exception
TRY.
    " ...
  CATCH cx_root INTO DATA(lx_error).
    DATA(lv_msg) = zabaputil_cl_util_context=>x_get_last_t100( lx_error ).
ENDTRY.
```

###### Context
```abap
DATA(lv_user)  = zabaputil_cl_util_context=>context_get_user_tech( ). " => 'DEVELOPER'
DATA(lv_cloud) = zabaputil_cl_util_context=>check_abap_cloud( ). " => abap_true / abap_false
DATA(lt_stack) = zabaputil_cl_util_context=>context_get_callstack( ).

" User details - cl_abap_context_info on Cloud, USR01/USER_ADDR/ADR6 on-premise
DATA(ls_user) = zabaputil_cl_util_context=>context_get_user_info( ).
" => uname, name_formatted, email, langu, timezone, date_format, decimal_format
```

###### HTTP Handler
```abap
" On-Premise
DATA(lo_http) = zabaputil_cl_util_http=>factory( server = lo_server ).

" ABAP Cloud
DATA(lo_http) = zabaputil_cl_util_http=>factory_cloud( req = lo_req  res = lo_res ).

" Unified API - works the same in both environments
DATA(ls_req_info) = lo_http->get_req_info( ).  " method, body, path, params
DATA(lv_body)     = lo_http->get_cdata( ).
DATA(lv_method)   = lo_http->get_method( ).
lo_http->set_cdata( lv_json ).
lo_http->set_status( code = 200  reason = 'OK' ).
```

###### Logging
```abap
DATA(lo_log) = NEW zabaputil_cl_util_log( ).
lo_log->info(    `Step 1 completed` ).
lo_log->warning( `Cache miss, falling back` ).
lo_log->error(   `Step 3 failed` ).
lo_log->success( `Done` ).

" Add an exception directly
TRY.
    " ...
  CATCH cx_root INTO DATA(lx).
    lo_log->add( lx ).
ENDTRY.

IF lo_log->has_error( ) = abap_true.
  " ...
ENDIF.

DATA(lv_csv_log)  = lo_log->to_csv( ).   " Export as CSV
DATA(lv_xlsx_log) = lo_log->to_xlsx( ).  " Export as XLSX
DATA(lt_messages) = lo_log->to_msg( ).   " Get as message table
DATA(lv_text)     = lo_log->to_string( ).
```

###### Application Log (BAL)
```abap
" Search application logs
DATA(lt_logs) = zabaputil_cl_util_context=>bal_search(
    object    = `ZMY_APP`
    date_from = lv_start_date ).

" Read the latest message / messages of a specific type
DATA(ls_last)   = zabaputil_cl_util_context=>bal_read_latest(  object = `ZMY_APP`  subobject = `IMPORT`  id = `RUN1` ).
DATA(lt_errors) = zabaputil_cl_util_context=>bal_read_by_type( object = `ZMY_APP`  subobject = `IMPORT`  id = `RUN1`  msg_type = `E` ).
DATA(lv_count)  = zabaputil_cl_util_context=>bal_count(        object = `ZMY_APP`  subobject = `IMPORT`  id = `RUN1` ).

" Housekeeping: delete logs older than N days
zabaputil_cl_util_context=>bal_delete_before( object = `ZMY_APP`  days = 30 ).
```

###### Locks (Enqueue/Dequeue)
```abap
" Set / check / release a lock
DATA(lv_locked) = zabaputil_cl_util_context=>lock_set( `ZMY_LOCK_OBJECT` ).
DATA(lv_is)     = zabaputil_cl_util_context=>lock_is_locked( `ZMY_LOCK_OBJECT` ).
DATA(lv_owner)  = zabaputil_cl_util_context=>lock_get_owner( `ZMY_LOCK_OBJECT` ).
zabaputil_cl_util_context=>lock_delete( `ZMY_LOCK_OBJECT` ).

" Retry until the lock is available (5 retries, 500 ms delay by default)
DATA(lv_got_it) = zabaputil_cl_util_context=>lock_set_wait( val = `ZMY_LOCK_OBJECT`  retries = 10 ).

" Read current lock entries
DATA(lt_locks) = zabaputil_cl_util_context=>lock_read( lock_object = `ZMY_LOCK_OBJECT` ).
```

###### ZIP
```abap
" Pack files into a ZIP archive
DATA(lv_zip) = zabaputil_cl_util_context=>zip_pack( VALUE #(
    ( name = `data.json`  content = lv_json_x )
    ( name = `readme.txt` content = lv_text_x ) ) ).

" Unpack a ZIP archive
DATA(lt_files) = zabaputil_cl_util_context=>zip_unpack( lv_zip ). " => name + content per file
```

###### Compression (DEFLATE / zlib)
```abap
" Raw DEFLATE (RFC 1951), as CL_ABAP_GZIP writes it
DATA(lv_deflated) = zabaputil_cl_util_context=>gzip_compress( lv_bytes ).
DATA(lv_bytes2)   = zabaputil_cl_util_context=>gzip_decompress( lv_deflated ).

" A zlib stream (RFC 1950): header, DEFLATE data, Adler-32 - what PDF and PNG use
DATA(lv_zlib)   = zabaputil_cl_util_context=>zlib_compress( lv_bytes ).
DATA(lv_bytes3) = zabaputil_cl_util_context=>zlib_decompress( lv_zlib ).
DATA(lv_adler)  = zabaputil_cl_util_context=>hash_adler32( lv_bytes ). " => 4 bytes

" The Unicode code points of a text
DATA(lt_codepoints) = zabaputil_cl_util_context=>c_get_codepoints( `A` ). " => 65
```

###### PDF
PDF files written in plain ABAP (`src/pdf/`): no Adobe Document Services, no spool, no form. The only system calls are the byte conversion and the deflate of the context class, so the same source runs on NW 7.02, Standard ABAP and ABAP Cloud.

```abap
" Draw at fixed positions - coordinates in mm from the top left corner
DATA(lo_pdf)  = zabaputil_cl_pdf=>create( unit = `mm` ).
DATA(lo_page) = lo_pdf->add_page( ).
lo_page->set_font( font = lo_pdf->font( `Helvetica-Bold` ) size = 16 ).
lo_page->text( x = 20 y = 30 val = `Delivery note 4711` ).
lo_page->set_fill_color( `E8E8E8` )->rect( x = 20 y = 35 w = 170 h = 10 style = `DF` ).
lo_page->image( image = lo_pdf->image( lv_logo_png ) x = 150 y = 10 w = 40 ).
DATA(lv_file) = lo_pdf->render( ). " => xstring

" Flow text and tables over pages
DATA(lo_layout) = NEW zabaputil_cl_pdf_layout( lo_pdf ).
lo_layout->set_font( font = lo_pdf->font( ) size = 10 ).
lo_layout->text( `A paragraph that wraps at the right margin ...` ).
lo_layout->table( columns = VALUE #( ( name = `MATNR` title = `Material`    width = 40 align = `L` )
                                     ( name = `MAKTX` title = `Description` width = 0  align = `L` )
                                     ( name = `MENGE` title = `Quantity`    width = 25 align = `R` ) )
                  rows    = lt_items ). " the header row is repeated on every page
lo_layout->set_footer( `Page {page} of {pages}` ).
lo_layout->finish( ).
```

```abap
" A form: the document described as data, printed with the data of one order
DATA(ls_form) = zabaputil_cl_pdf_form=>new_form( ).
ls_form-t_element = VALUE #(
    ( type = zabaputil_cl_pdf_form=>cs_type-text  text = `Delivery note {VBELN}` size = 16 font = `Helvetica-Bold` )
    ( type = zabaputil_cl_pdf_form=>cs_type-table source = `T_ITEMS`
      t_column = VALUE #( ( name = `MATNR` title = `Material` width = 40 align = `L` )
                          ( name = `MENGE` title = `Quantity` width = 25 align = `R` ) ) ) ).
DATA(lv_pdf) = zabaputil_cl_pdf_form=>render( val = ls_form data = ls_delivery ).

" The data may be an object instead: its public attributes are what the form prints.
" A class that implements zabaputil_if_pdf_data is a whole form: the attributes are its
" data, and its method get( ) returns the form as text - what an editor shows to copy
DATA(lo_note) = NEW zabaputil_cl_pdf_data_demo( ).
lo_note->vbeln = `80004711`.
lv_pdf = zabaputil_cl_pdf_form=>print( lo_note ).

" What an editor works with: the fields of the class, the form as data and as text,
" and an instance with sample values in everything the class leaves initial
DATA(lt_fields) = zabaputil_cl_pdf_form=>fields( lo_note ).
ls_form = zabaputil_cl_pdf_form=>form_of( lo_note ).
DATA(lv_text) = zabaputil_cl_pdf_form=>stringify( ls_form ).   " <form version="1"><FORMAT>A4</FORMAT>...
ls_form = zabaputil_cl_pdf_form=>parse( lv_text ).
DATA(li_sample) = zabaputil_cl_pdf_form=>data_sample( `ZABAPUTIL_CL_PDF_DATA_DEMO` ).
```

| Class | Purpose |
|---|---|
| `zabaputil_cl_pdf` | the document: pages, fonts, images, properties, `render( )` |
| `zabaputil_cl_pdf_page` | one page: text, lines, rectangles, ellipses, images, links at fixed positions |
| `zabaputil_cl_pdf_layout` | flowing content: wrapped paragraphs, tables over several pages, header and footer |
| `zabaputil_cl_pdf_font_std` | the standard fonts (Helvetica, Times, Courier) with their metrics - Western European text, nothing embedded |
| `zabaputil_cl_pdf_font_ttf` | an embedded TrueType font (`lo_pdf->font_ttf( lv_ttf )`) - any script the font has glyphs for |
| `zabaputil_cl_pdf_image` | JPEG and PNG images, PNG with transparency |
| `zabaputil_cl_pdf_form` | a form as data - elements in the flow or at fixed positions, `{NAME}` placeholders for the components of the data - and its printout; an absolute form (`ty_s_form-absolute`) places every element at its x, y, w, h, as a designer with a canvas draws it, and moves what stands below a table to where the table ends |
| `zabaputil_if_pdf_data`, `zabaputil_cl_pdf_data_demo` | a form as one class - its public attributes are the data, `get( )` returns the form as text - and one by example |
| `zabaputil_cl_pdf_writer` | the file format: objects, streams, cross-reference table |

Not there yet: font subsetting (a TrueType font is embedded whole), OpenType/CFF fonts, text shaping (ligatures, right-to-left scripts), several fonts within one line, interlaced PNG files.

###### Transport Requests
```abap
DATA(lt_requests) = zabaputil_cl_util_context=>tr_get_user_requests( ).             " my open requests
DATA(lt_objects)  = zabaputil_cl_util_context=>tr_get_objects( `DEVK900123` ).
DATA(lv_descr)    = zabaputil_cl_util_context=>tr_get_description( `DEVK900123` ).
DATA(lv_released) = zabaputil_cl_util_context=>tr_is_released( `DEVK900123` ).

zabaputil_cl_util_context=>tr_add_object(
    trkorr   = `DEVK900123`
    object   = `CLAS`
    obj_name = `ZCL_MY_CLASS` ).
```

###### System Utilities
```abap
" Authorization check
IF zabaputil_cl_util_context=>auth_check( object = `S_TCODE`  field = `TCD`  value = `SE38` ) = abap_true.
  " ...
ENDIF.

" Next number from a number range
DATA(lv_number) = zabaputil_cl_util_context=>numrange_get_next( object = `ZMY_NR` ).

" Read change documents
DATA(lt_changes) = zabaputil_cl_util_context=>changdoc_read(
    objectclass = `ZMY_OBJECT`
    objectid    = lv_object_id ).

" Submit a report as background job
DATA(lv_job) = zabaputil_cl_util_context=>job_submit_report( report = `ZMY_REPORT`  variant = `DEFAULT` ).

" Send an e-mail
zabaputil_cl_util_context=>mail_send(
    to      = `mail@example.com`
    subject = `Import finished`
    body    = `<h1>Done!</h1>`
    html    = abap_true ).

" User parameter defaults (SPA/GPA, table USR05)
DATA(lv_plant) = zabaputil_cl_util_context=>param_get_user_default( `WRK` ).

" Background job status (table TBTCO)
DATA(lv_status) = zabaputil_cl_util_context=>job_get_status( jobname = `ZMY_JOB`  jobcount = lv_jobcount ).

" Generic DB access by table name
DATA(lv_exists) = zabaputil_cl_util_context=>db_check_table_exists( `T000` ).
DATA(lr_rows)   = zabaputil_cl_util_context=>db_select_by_name( tabname = `T000`  max_rows = 10 ).
DATA(lv_count)  = zabaputil_cl_util_context=>db_count_by_name( tabname = `T000` ).

" Misc helpers
DATA(lv_dist) = zabaputil_cl_util_context=>c_levenshtein( val1 = `kitten`  val2 = `sitting` ). " => 3
DATA(lv_mime) = zabaputil_cl_util_context=>file_get_mimetype( `report.xlsx` ).
DATA(lv_val)  = zabaputil_cl_util_context=>num_round( val = CONV decfloat34( '2.345' )  decimals = 2 ). " => 2.35
```

###### Parallel Processing
```abap
" every task implements IF_ABAP_PARALLEL (method DO) and carries its input and result in attributes
DATA(lt_result) = zabaputil_cl_util_context=>parallel_run( t_task = lt_tasks  tasks = 4 ).
LOOP AT lt_result INTO DATA(ls_result).
  DATA(lo_task) = CAST zcl_my_task( ls_result-task ).  " the copy that ran, with its results
ENDLOOP.

" debugging, or a release without CL_ABAP_PARALLEL: one after the other in this session
lt_result = zabaputil_cl_util_context=>parallel_run( t_task = lt_tasks  sequential = abap_true  method = `RUN` ).
```

###### Long Texts (SAPscript)
```abap
DATA(lv_text) = zabaputil_cl_util_context=>longtext_read( object = `VBBK`  id = `0001`  name = lv_vbeln ).
zabaputil_cl_util_context=>longtext_save(   object = `VBBK`  id = `0001`  name = lv_vbeln  val = lv_text ).
zabaputil_cl_util_context=>longtext_delete( object = `VBBK`  id = `0001`  name = lv_vbeln ).

" the line format conversion on its own (132 characters, `*` paragraph, `=` continuation)
DATA(lt_lines) = zabaputil_cl_util_context=>longtext_to_lines( lv_text ).
lv_text        = zabaputil_cl_util_context=>longtext_from_lines( lt_lines ).
```

###### RFC
```abap
" call any function module by name, parameters and result as JSON - locally or remote
DATA(lv_json) = zabaputil_cl_util_context=>rfc_call_dynamic(
    function    = `BAPI_USER_GET_DETAIL`
    params      = `{"USERNAME":"DEVELOPER"}`
    destination = `NONE` ).

" is the destination reachable?
IF zabaputil_cl_util_context=>rfc_check_destination( `ZMY_DEST` ) = abap_false.
  zabaputil_cl_util_context=>rfc_ping( `ZMY_DEST` ).  " raises with the communication error text
ENDIF.
```

###### BAPI Calls
```abap
" one message table and one LUW contract for every BAPI and function module.
" The message table is any table with the components of BAPIRET2 (TYPE, ID,
" NUMBER, MESSAGE, MESSAGE_V1..4) - BAPIRET2, BAPIRET1, BAPIRETURN1, ...
DATA lt_return   TYPE bapirettab.
DATA lv_is_error TYPE abap_bool.

zabaputil_cl_util_context=>bapi_set_material( EXPORTING iv_matnr = `MAT-1`     " MATERIAL_LONG on S/4HANA
                                              CHANGING  cs_data  = ls_item ).
TRY.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' ... TABLES return = lt_bapi_return.
    zabaputil_cl_util_context=>bapi_msg_add_table( EXPORTING it_table   = lt_bapi_return  " PROTT, MESG, BAPIRET1/2, ...
                                                   CHANGING  ct_bapiret = lt_return ).
  CATCH cx_root INTO DATA(lx).
    zabaputil_cl_util_context=>bapi_msg_add_exception( EXPORTING ix_error   = lx
                                                       CHANGING  ct_bapiret = lt_return ).
ENDTRY.

" error flag from E/A/X, BAPI_TRANSACTION_COMMIT on success, ROLLBACK on error
zabaputil_cl_util_context=>bapi_finish( EXPORTING iv_commit   = abap_true
                                        CHANGING  ct_bapiret  = lt_return
                                                  cv_is_error = lv_is_error ).

" further: bapi_msg_add_sy (after EXCEPTIONS error_message), bapi_msg_add_text,
" bapi_msg_add_bapiret2, bapi_check_is_error, bapi_refresh_buffers (delivery,
" HU, serial number buffers), bapi_call_function (module by name, e.g. EWM)
DATA(lv_vbeln) = zabaputil_cl_util_context=>conv_alpha_in( `80001234` ).   " 0080001234
```

###### MIME Repository, Spool & Forms
```abap
DATA(lv_logo) = zabaputil_cl_util_context=>mime_read( `/SAP/PUBLIC/ZMY_APP/logo.png` ).
zabaputil_cl_util_context=>mime_save( url = `/SAP/PUBLIC/ZMY_APP/logo.png`  content = lv_logo ).

" spool request (SAPscript, Smart Forms, ABAP list, Adobe form) as PDF
DATA(lv_pdf) = zabaputil_cl_util_context=>spool_get_pdf( lv_spool_id ).

" Adobe form as PDF - the components of data are the form interface parameters
lv_pdf = zabaputil_cl_util_context=>form_get_pdf( form_name = `ZMY_FORM`  data = ls_form_data ).

" binary tables to xstring (the inverse of conv_get_bintab_by_xstring)
DATA(lv_x) = zabaputil_cl_util_context=>conv_get_xstring_by_bintab( val = lt_solix  length = lv_size ).
```

###### Report Variants
```abap
DATA(lt_variants) = zabaputil_cl_util_context=>variant_list( `ZMY_REPORT` ).
DATA(lt_values)   = zabaputil_cl_util_context=>variant_read( report = `ZMY_REPORT`  variant = `DEFAULT` ).
```

###### ABAP Messaging Channels
```abap
zabaputil_cl_util_context=>amc_send( application_id = `ZMY_AMC`  channel_id = `/updates`  message = `refresh` ).
```

###### Attachments (GOS)
```abap
DATA(lv_doc_id) = zabaputil_cl_util_context=>gos_attach( object_type = `BUS2012`  object_key = lv_ebeln
                                                         file_name   = `offer.pdf`  content = lv_pdf ).
DATA(lt_files)  = zabaputil_cl_util_context=>gos_list( object_type = `BUS2012`  object_key = lv_ebeln ).
DATA(lv_file)   = zabaputil_cl_util_context=>gos_read( lt_files[ 1 ]-doc_id ).
```

The long text, RFC, MIME, spool, form, variant, AMC and GOS methods need Standard ABAP; on ABAP Cloud they raise `zabaputil_cx_util_error` (`rfc_ping` and `rfc_check_destination` take a BTP destination there).

###### XML Builder
Fluent builder for nested XML / UI5 view fragments.
`__` opens an element (must be closed with `n( )`), `_` writes a self-closing leaf,
`p` adds a single attribute, the `p =` parameter takes an attribute table.
```abap
DATA(lo_view) = zabaputil_cl_util_xml=>factory(
  )->__( n = `View` ns = `mvc`
         p = VALUE #( ( n = `xmlns`     v = `sap.m` )
                      ( n = `xmlns:mvc` v = `sap.ui.core.mvc` ) ) ).

DATA(lo_page) = lo_view->__( n = `Page`
  p = VALUE #( ( n = `title`         v = `Hello` )
               ( n = `showNavButton` v = `true` ) ) ).

lo_page->__( `content`
  )->_( n = `Button` p = VALUE #( ( n = `text` v = `Click me` ) )
  )->n( ).  " close <content>

DATA(lv_xml)        = lo_view->stringify( ).
DATA(lv_xml_pretty) = lo_view->stringify( indent = abap_true ).
```

###### Persistence
```abap
" Save any ABAP value under a composite handle (key/value store)
DATA(lv_id) = zabaputil_cl_util_db=>save(
    uname  = sy-uname
    handle = `FILTER`
    data   = ls_filter ).

" Load it back
zabaputil_cl_util_db=>load_by_handle(
    EXPORTING uname  = sy-uname
              handle = `FILTER`
    IMPORTING result = ls_filter ).

zabaputil_cl_util_db=>delete_by_handle( uname = sy-uname  handle = `FILTER` ).
```

#### Renaming
To install abap-util under another name, run the workflow [build_rename](https://github.com/abap-util/abap-util/actions/workflows/build_rename.yaml) with a customer name such as `ZMYUTIL` or a registered namespace such as `/MYNS/`. It renames every object (`zabaputil_cl_util_context` becomes `zmyutil_cl_util_context`) and pushes the result to the branch `rename_<name>`, ready for abapGit. Every object name stays at 29 characters or less, so even a 10-character name fits into the 30-character limit - `test_rename` checks that on every pull request.

#### Development
```bash
npm ci
npm run lint            # abaplint, Standard ABAP (v750)
npm run lint:cloud      # abaplint, ABAP Cloud
npm run downport        # rewrite src/ to 7.02 syntax - what auto_downport pushes to 702
npm run auto_transpile  # transpile to JavaScript
npm run unit            # run the unit tests in Node.js
npm run rename          # rename every object to the longest prefix allowed
npm test                # lint, lint:cloud, rename, auto_transpile and unit in one go
```
`node .github/scripts/sync-check.mjs --consumer <dir>` compares a consumer's renamed copies with their masters here - which methods changed, which exist only in the copy.
The rules every change keeps - compatibility with all three targets, the master-catalog contract, the sync-back - are in [AGENTS.md](AGENTS.md).

#### Documentation
The full API reference with all methods, parameters and further examples lives at **[abap-util.github.io/docs](https://abap-util.github.io/docs/)**.

#### Credits
* [ajson](https://github.com/sbcgua/ajson)
* [S-RTTI](https://github.com/sandraros/S-RTTI)
* [steampunkification](https://github.com/heliconialabs/steampunkification)

#### Contribution & Support
Pull requests are welcome - bug fixes, new methods and better documentation alike. If you run into a problem, open an issue.

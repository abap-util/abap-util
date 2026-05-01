[![ABAP_STANDARD](https://github.com/oblomov-dev/abap-util/actions/workflows/ABAP_STANDARD.yaml/badge.svg)](https://github.com/oblomov-dev/abap-util/actions/workflows/ABAP_STANDARD.yaml)
[![ABAP_CLOUD](https://github.com/oblomov-dev/abap-util/actions/workflows/ABAP_CLOUD.yaml/badge.svg)](https://github.com/oblomov-dev/abap-util/actions/workflows/ABAP_CLOUD.yaml)
<br>
[![auto_build](https://github.com/oblomov-dev/abap-util/actions/workflows/auto_build.yml/badge.svg)](https://github.com/oblomov-dev/abap-util/actions/workflows/auto_build.yml)
[![auto_downport](https://github.com/oblomov-dev/abap-util/actions/workflows/auto_downport.yaml/badge.svg)](https://github.com/oblomov-dev/abap-util/actions/workflows/auto_downport.yaml)
<br>
[![test_unit](https://github.com/oblomov-dev/abap-util/actions/workflows/test_unit.yaml/badge.svg)](https://github.com/oblomov-dev/abap-util/actions/workflows/test_unit.yaml)
[![test_rename](https://github.com/oblomov-dev/abap-util/actions/workflows/test_rename.yaml/badge.svg)](https://github.com/oblomov-dev/abap-util/actions/workflows/test_rename.yaml)

# abap-util
Utility Functions for ABAP Cloud & Standard ABAP 

#### Key Features
* Simplified SAP APIs to class-based methods
* Hides language version differences between ABAP Cloud and Standard ABAP
* Function Scope: GUIDs, RTTI, Messages, Persistence etc.

#### Compatibility
* S/4 Public Cloud and BTP ABAP Environment (ABAP Cloud)
* S/4 Private Cloud and On-Premise (ABAP Cloud, Standard ABAP)
* R/3 NetWeaver AS ABAP 7.02 or higher (Standard ABAP)

#### Credits
* [ajson](https://github.com/sbcgua/ajson)
* [S-RTTI](https://github.com/sandraros/S-RTTI)
* [steampunkification](https://github.com/heliconialabs/steampunkification)
  
#### Installation
Install via [abapGit](https://abapgit.org) - clone this repository into your SAP system.

#### Usage

###### Strings
```abap
DATA(lv_upper) = zabaputil_cl_util=>c_trim_upper( ` Hello World  ` ). " => 'HELLO WORLD'
DATA(lv_lower) = zabaputil_cl_util=>c_trim_lower( ` Hello World  ` ). " => 'hello world'
DATA(lv_trim)  = zabaputil_cl_util=>c_trim( ` Hello World  ` ).       " => 'Hello World'

" Predicates
DATA(lv_has)   = zabaputil_cl_util=>c_contains(    val = `Hello World`  sub    = `World` ). " => abap_true
DATA(lv_pre)   = zabaputil_cl_util=>c_starts_with( val = `Hello World`  prefix = `Hello` ). " => abap_true
DATA(lv_suf)   = zabaputil_cl_util=>c_ends_with(   val = `Hello World`  suffix = `World` ). " => abap_true

" Split & Join
DATA(lt_parts) = zabaputil_cl_util=>c_split( val = `a,b,c`  sep = `,` ). " => ('a' 'b' 'c')
DATA(lv_csv)   = zabaputil_cl_util=>c_join(  tab = lt_parts  sep = `;` ). " => 'a;b;c'
```

###### JSON
```abap
" Structure to JSON
DATA(lv_json) = zabaputil_cl_util=>json_stringify( ls_flight ).
" => '{"CARRID":"LH","CONNID":"0400","FLDATE":"2025-01-15"}'

" JSON to Structure
zabaputil_cl_util=>json_parse(
    val  = lv_json
    data = ls_flight ).
```

###### XML
```abap
" Serialize any ABAP data to XML
DATA(lv_xml) = zabaputil_cl_util=>xml_stringify( ls_data ).

" Deserialize XML back to ABAP data
zabaputil_cl_util=>xml_parse(
    xml = lv_xml
    any = ls_data ).
```

###### UUID
```abap
DATA(lv_uuid_32) = zabaputil_cl_util=>uuid_get_c32( ). " => '550E8400E29B41D4A716446655440000'
DATA(lv_uuid_22) = zabaputil_cl_util=>uuid_get_c22( ). " => 'VQ6EAOKbQdSnFkRmVU'
```

###### RTTI
```abap
" Check if a class exists
IF zabaputil_cl_util=>rtti_check_class_exists( 'ZCL_MY_CLASS' ) = abap_true.
  " ...
ENDIF.

" Get type name of any variable
DATA(lv_type) = zabaputil_cl_util=>rtti_get_type_name( ls_flight ). " => 'SFLIGHT'

" Create dynamic internal table by DDIC name
DATA(lr_table) = zabaputil_cl_util=>rtti_create_tab_by_name( 'SFLIGHT' ).

" Get component list of a structure
DATA(lt_components) = zabaputil_cl_util=>rtti_get_t_attri_by_any( ls_flight ).
```

###### URL Parameters
```abap
" Read a parameter from URL
DATA(lv_id) = zabaputil_cl_util=>url_param_get(
    val = 'ID'
    url = '/sap/bc/http/myapp?ID=12345&MODE=edit' ). " => '12345'

" Build a URL from parameters
DATA(lv_url) = zabaputil_cl_util=>url_param_create_url(
    t_params = VALUE #(
        ( n = 'ID'   v = '12345' )
        ( n = 'MODE' v = 'edit' ) ) ). " => 'ID=12345&MODE=edit'

" Add/update a parameter
DATA(lv_new_url) = zabaputil_cl_util=>url_param_set(
    url   = '/app?ID=1'
    name  = 'LANG'
    value = 'DE' ). " => '/app?ID=1&LANG=DE'
```

###### Base64 & Encoding
```abap
" String <-> xstring
DATA(lv_xstring) = zabaputil_cl_util=>conv_get_xstring_by_string( 'Hello' ).
DATA(lv_string)  = zabaputil_cl_util=>conv_get_string_by_xstring( lv_xstring ).

" Base64 encode/decode
DATA(lv_base64)  = zabaputil_cl_util=>conv_encode_x_base64( lv_xstring ).
DATA(lv_decoded) = zabaputil_cl_util=>conv_decode_x_base64( lv_base64 ).
```

###### CSV
```abap
" Internal table to CSV
DATA(lv_csv) = zabaputil_cl_util=>itab_get_csv_by_itab( lt_flights ).

" CSV to internal table (returns data reference)
DATA(lr_itab) = zabaputil_cl_util=>itab_get_itab_by_csv( lv_csv ).
```

###### XLSX / Excel
```abap
" Internal table to Excel
DATA(lv_xlsx) = zabaputil_cl_util=>conv_get_xlsx_by_itab( lt_flights ).

" Excel to internal table
zabaputil_cl_util=>conv_get_itab_by_xlsx(
    EXPORTING val    = lv_xlsx
    IMPORTING result = lr_data ).
```

###### Internal Tables
```abap
" Move corresponding rows between tables of different row types
zabaputil_cl_util=>itab_corresponding(
    EXPORTING val = lt_source
    CHANGING  tab = lt_target ).

" Filter table in place by a substring across all clike fields
zabaputil_cl_util=>itab_filter_by_val(
    EXPORTING val = `LH`
    CHANGING  tab = lt_flights ).

" Project a flat structure to a list of name/value pairs
DATA(lt_kv) = zabaputil_cl_util=>itab_get_by_struc( ls_flight ).
" => [ ( name = 'CARRID' value = 'LH' ) ( name = 'CONNID' value = '0400' ) ... ]
```

###### Range & Token Filters
```abap
" Build select-options ranges programmatically
DATA(ls_eq) = zabaputil_cl_util_range=>eq( `LH` ).        " I EQ 'LH'
DATA(ls_bt) = zabaputil_cl_util_range=>bt( low = `100` high = `200` ).
DATA(ls_cp) = zabaputil_cl_util_range=>cp( `LH*` ).        " I CP 'LH*'
DATA(ls_ne) = zabaputil_cl_util_range=>ne( `XX` ).         " E EQ 'XX'

" Token strings (e.g. from a UI search field) <-> ranges
DATA(ls_range) = zabaputil_cl_util=>filter_get_range_by_token( `>=100` ).
DATA(lt_token) = zabaputil_cl_util=>filter_get_token_t_by_range_t( lt_range ).

" Apply a multi-field filter on an internal table in place
zabaputil_cl_util=>filter_itab(
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
```

###### Timestamps
```abap
DATA(lv_now)  = zabaputil_cl_util=>time_get_timestampl( ).
DATA(lv_past) = zabaputil_cl_util=>time_subtract_seconds( time = lv_now  seconds = 3600 ).
DATA(lv_next) = zabaputil_cl_util=>time_add_seconds(      time = lv_now  seconds = 60 ).
DATA(lv_diff) = zabaputil_cl_util=>time_diff_seconds(     val1 = lv_now  val2    = lv_past ).
DATA(lv_date) = zabaputil_cl_util=>time_get_date_by_stampl( lv_now ).
DATA(lv_time) = zabaputil_cl_util=>time_get_time_by_stampl( lv_now ).

" Build a timestampl from date/time
DATA(lv_stmp) = zabaputil_cl_util=>time_get_stampl_by_date_time(
    date = sy-datum
    time = sy-uzeit ).

" Date <-> string with format pattern
DATA(lv_iso)  = zabaputil_cl_util=>conv_date_to_string(   val = sy-datum    format = `YYYY-MM-DD` ).
DATA(lv_dat)  = zabaputil_cl_util=>conv_string_to_date(   val = `2024-03-15` format = `YYYY-MM-DD` ).
```

###### Error Handling
```abap
" Raise exception conditionally
zabaputil_cl_util=>x_check_raise( when = xsdbool( sy-subrc <> 0 ) ).

" Raise exception directly
zabaputil_cl_util=>x_raise( 'Something went wrong' ).

" Get last T100 message from exception
TRY.
    " ...
  CATCH cx_root INTO DATA(lx_error).
    DATA(lv_msg) = zabaputil_cl_util=>x_get_last_t100( lx_error ).
ENDTRY.
```

###### Context
```abap
DATA(lv_user)  = zabaputil_cl_util=>context_get_user_tech( ). " => 'DEVELOPER'
DATA(lv_cloud) = zabaputil_cl_util=>context_check_abap_cloud( ). " => abap_true / abap_false
DATA(lt_stack) = zabaputil_cl_util=>context_get_callstack( ).
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

#### Contribution & Support
Pull requests are welcome! Whether you're fixing bugs, adding new functionality, or improving documentation, your contributions are highly appreciated. If you encounter any problems, feel free to open an issue.

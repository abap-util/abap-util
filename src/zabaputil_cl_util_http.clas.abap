CLASS zabaputil_cl_util_http DEFINITION PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_http_req,
        method   TYPE string,
        body     TYPE string,
        path     TYPE string,
        t_params TYPE zabaputil_cl_util_context=>ty_t_name_value,
        " the raw query string t_params was parsed from, see get_query
        query    TYPE string,
      END OF ty_s_http_req.

    TYPES:
      BEGIN OF ty_s_http_res,
        body          TYPE string,
        status_code   TYPE i,
        status_reason TYPE string,
      END OF ty_s_http_res.

    CLASS-METHODS client_create
      IMPORTING
        !destination  TYPE clike OPTIONAL
        url           TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO object.

    CLASS-METHODS client_call
      IMPORTING
        !method       TYPE clike
        body          TYPE clike OPTIONAL
        !destination  TYPE clike OPTIONAL
        url           TYPE clike OPTIONAL
        !query        TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_http_res.

    " One outbound HTTP request with header fields and a timeout - the general
    " form of client_call, which builds this structure and calls it. url is
    " the full URL, or with destination the path below it; t_header carries
    " the request header fields (content-type among them); timeout in
    " seconds, 0 for the client's default. The platform decides the client
    " (zabaputil_cl_util_context=>check_abap_cloud): CL_HTTP_CLIENT with an
    " SM59 destination or a URL on Standard, CL_WEB_HTTP_CLIENT_MANAGER on
    " ABAP Cloud with a URL, a communication arrangement
    " (SCENARIO/SERVICE_ID[/COMM_SYSTEM]) or a destination of the BTP
    " destination service (any other name). Both named dynamically, so the
    " class activates on every release
    TYPES:
      BEGIN OF ty_s_client_req,
        method      TYPE string,
        url         TYPE string,
        destination TYPE string,
        query       TYPE string,
        body        TYPE string,
        t_header    TYPE zabaputil_cl_util_context=>ty_t_name_value,
        timeout     TYPE i,
      END OF ty_s_client_req.

    CLASS-METHODS client_request
      IMPORTING
        is_req        TYPE ty_s_client_req
      RETURNING
        VALUE(result) TYPE ty_s_http_res.

    CLASS-METHODS factory
      IMPORTING
        server        TYPE REF TO object
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_http.

    CLASS-METHODS factory_cloud
      IMPORTING
        req           TYPE REF TO object
        res           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE REF TO zabaputil_cl_util_http.

    METHODS get_req_info
      RETURNING
        VALUE(result) TYPE ty_s_http_req.

    METHODS get_header_field
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_cdata
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_method
      RETURNING
        VALUE(result) TYPE string.

    METHODS set_cdata
      IMPORTING
        val TYPE clike.

    METHODS set_status
      IMPORTING
        !code  TYPE i
        reason TYPE clike.

    METHODS set_session_stateful
      IMPORTING
        val TYPE i.

    METHODS get_response_cookie
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    METHODS delete_response_cookie
      IMPORTING
        val TYPE clike.

    METHODS set_header_field
      IMPORTING
        !n TYPE clike
        v  TYPE clike.

    DATA mo_server_onprem  TYPE REF TO object.
    DATA mo_request_cloud  TYPE REF TO object.
    DATA mo_response_cloud TYPE REF TO object.

    " the query string of the request, raw - everything after the FIRST `?`
    " of its URI, not decoded. What a proxy forwards: parsing it into
    " t_params and encoding it again is not guaranteed to give the same
    " string back (sap-startup-params, see url_param_get_tab)
    METHODS get_query
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_client_target,
        url         TYPE string,
        request_uri TYPE string,
      END OF ty_s_client_target.

    " the shape of IF_WEB_HTTP_RESPONSE=>HTTP_STATUS, declared here because
    " the interface is not on every release - a dynamic RECEIVING needs a
    " structurally identical target
    TYPES:
      BEGIN OF ty_s_http_status_cloud,
        code   TYPE i,
        reason TYPE string,
      END OF ty_s_http_status_cloud.

    CLASS-METHODS client_request_std
      IMPORTING
        is_req        TYPE ty_s_client_req
      RETURNING
        VALUE(result) TYPE ty_s_http_res.

    CLASS-METHODS client_send_std
      IMPORTING
        io_client   TYPE REF TO object
        is_req      TYPE ty_s_client_req
        request_uri TYPE string.

    CLASS-METHODS client_request_cloud
      IMPORTING
        is_req        TYPE ty_s_client_req
      RETURNING
        VALUE(result) TYPE ty_s_http_res.

    CLASS-METHODS client_destination_cloud
      IMPORTING
        is_req        TYPE ty_s_client_req
      RETURNING
        VALUE(result) TYPE REF TO object.

    DATA mo_request_onprem  TYPE REF TO object.
    DATA mo_response_onprem TYPE REF TO object.

    METHODS get_request_onprem
      RETURNING
        VALUE(result) TYPE REF TO object.

    METHODS get_response_onprem
      RETURNING
        VALUE(result) TYPE REF TO object.

    " where client_call puts its query - see the method
    CLASS-METHODS client_target
      IMPORTING
        !destination  TYPE clike
        url           TYPE clike
        !query        TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_client_target.

ENDCLASS.


CLASS zabaputil_cl_util_http IMPLEMENTATION.

  METHOD client_create.

    DATA lv_classname TYPE c LENGTH 14.
    DATA lv_destination TYPE c LENGTH 32.
    DATA temp18 TYPE string.
    DATA lv_url LIKE temp18.
        DATA x TYPE REF TO cx_root.
    lv_classname = `CL_HTTP_CLIENT`.

    
    lv_destination = destination.
    
    temp18 = url.
    
    lv_url = temp18.

    TRY.

        IF lv_destination IS NOT INITIAL AND lv_destination <> `NONE`.

          CALL METHOD (lv_classname)=>create_by_destination
            EXPORTING
              destination              = lv_destination
            IMPORTING
              client                   = result
            EXCEPTIONS
              argument_not_found       = 1
              destination_not_found    = 2
              destination_no_authority = 3
              plugin_not_active        = 4
              internal_error           = 5
              OTHERS                   = 6.

        ELSE.

          CALL METHOD (lv_classname)=>create_by_url
            EXPORTING
              url                = lv_url
            IMPORTING
              client             = result
            EXCEPTIONS
              argument_not_found = 1
              plugin_not_active  = 2
              internal_error     = 3
              OTHERS             = 4.

        ENDIF.

        IF sy-subrc <> 0.
          CLEAR result.
        ENDIF.

        
      CATCH cx_root INTO x.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = x.
    ENDTRY.

    IF result IS NOT BOUND.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING val = `HTTP_CLIENT_CREATE_ERROR - check the destination/url configuration`.
    ENDIF.

  ENDMETHOD.

  METHOD client_call.

    DATA ls_req TYPE ty_s_client_req.

    ls_req-method      = method.
    ls_req-body        = body.
    ls_req-destination = destination.
    ls_req-query       = query.
    " client_call has always ignored the url next to a destination, where
    " client_request reads it as the path below it - kept as it was
    IF ls_req-destination IS INITIAL OR ls_req-destination = `NONE`.
      ls_req-url = url.
    ENDIF.

    result = client_request( ls_req ).

  ENDMETHOD.

  METHOD client_request.

    IF zabaputil_cl_util_context=>check_abap_cloud( ) = abap_true.
      result = client_request_cloud( is_req ).
    ELSE.
      result = client_request_std( is_req ).
    ENDIF.

  ENDMETHOD.

  METHOD client_request_std.

    DATA lo_response TYPE REF TO object.
    FIELD-SYMBOLS <any> TYPE any.

    DATA ls_target TYPE zabaputil_cl_util_http=>ty_s_client_target.
    DATA lo_client TYPE REF TO object.
        DATA x TYPE REF TO cx_root.
    ls_target = client_target( destination = is_req-destination
                                     url         = is_req-url
                                     query       = is_req-query ).

    
    lo_client = client_create( destination = is_req-destination
                                     url         = ls_target-url ).

    TRY.

        client_send_std( io_client   = lo_client
                         is_req      = is_req
                         request_uri = ls_target-request_uri ).

        " IS ASSIGNED, not sy-subrc, and a RAISE rather than an ASSERT - see
        " get_response_onprem for both halves. Here the RAISE also buys the
        " CLOSE below: the CATCH that closes the ICM connection on the failure
        " path cannot see an ASSERTION_FAILED
        UNASSIGN <any>.
        ASSIGN lo_client->(`RESPONSE`) TO <any>.
        IF <any> IS NOT ASSIGNED.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING val = `HTTP_CLIENT_RESPONSE_NOT_FOUND - the http client has no RESPONSE attribute`.
        ENDIF.
        lo_response = <any>.

        CALL METHOD lo_response->(`GET_CDATA`)
          RECEIVING
            data = result-body.

        CALL METHOD lo_response->(`GET_STATUS`)
          IMPORTING
            code   = result-status_code
            reason = result-status_reason.

        CALL METHOD lo_client->(`CLOSE`)
          EXCEPTIONS
            OTHERS = 1.

        
      CATCH cx_root INTO x.
        " CLOSE on the failure path too: every throw between client_create
        " and the success-path CLOSE above (a failing dynamic GET_CDATA /
        " GET_STATUS, the RESPONSE assign, a failed SEND) used to leave the
        " ICM connection open for the lifetime of the work process
        IF lo_client IS BOUND.
          TRY.
              CALL METHOD lo_client->(`CLOSE`)
                EXCEPTIONS
                  OTHERS = 1.
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = x.
    ENDTRY.

  ENDMETHOD.

  METHOD client_send_std.

    DATA lo_request TYPE REF TO object.
    DATA lv_message TYPE string.
    FIELD-SYMBOLS <any> TYPE any.
    DATA ls_header LIKE LINE OF is_req-t_header.
    DATA lv_method LIKE is_req-method.
    DATA lv_body LIKE is_req-body.
      DATA lv_timeout LIKE is_req-timeout.

    " IS ASSIGNED, not sy-subrc, and a RAISE rather than an ASSERT - see
    " get_response_onprem for both halves. An HTTP client whose REQUEST
    " could not be reached leaked its connection before the caller's CATCH
    " closed it
    UNASSIGN <any>.
    ASSIGN io_client->(`REQUEST`) TO <any>.
    IF <any> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING val = `HTTP_CLIENT_REQUEST_NOT_FOUND - the http client has no REQUEST attribute`.
    ENDIF.
    lo_request = <any>.

    IF request_uri IS NOT INITIAL.
      CALL METHOD lo_request->(`SET_HEADER_FIELD`)
        EXPORTING
          name  = `~request_uri`
          value = request_uri.
    ENDIF.

    " every header field as it came - name and value are strings already,
    " which is what the dynamic call needs (see set_cdata on the CONV)
    
    LOOP AT is_req-t_header INTO ls_header.
      CALL METHOD lo_request->(`SET_HEADER_FIELD`)
        EXPORTING
          name  = ls_header-n
          value = ls_header-v.
    ENDLOOP.

    
    lv_method = is_req-method.
    CALL METHOD lo_request->(`SET_METHOD`)
      EXPORTING
        method = lv_method.

    
    lv_body = is_req-body.
    CALL METHOD lo_request->(`SET_CDATA`)
      EXPORTING
        data = lv_body.

    IF is_req-timeout > 0.
      
      lv_timeout = is_req-timeout.
      CALL METHOD io_client->(`SEND`)
        EXPORTING
          timeout                    = lv_timeout
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5.
    ELSE.
      CALL METHOD io_client->(`SEND`)
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5.
    ENDIF.

    IF sy-subrc = 0.
      CALL METHOD io_client->(`RECEIVE`)
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
    ENDIF.

    IF sy-subrc <> 0.
      CALL METHOD io_client->(`GET_LAST_ERROR`)
        IMPORTING
          message = lv_message.
      RAISE EXCEPTION TYPE zabaputil_cx_util_error
        EXPORTING val = |HTTP_COMMUNICATION_ERROR - { lv_message }|.
    ENDIF.

  ENDMETHOD.

  METHOD client_destination_cloud.

    " CL_HTTP_DESTINATION_PROVIDER, by name: the class is not on Standard
    " releases and a static reference would leave this class inactive
    " there. Three ways to a destination: a URL; a communication
    " arrangement SCENARIO/SERVICE_ID[/COMM_SYSTEM] (the slash tells it
    " apart); any other name is a destination of the BTP destination
    " service, used as the system itself (service_specific), not as the
    " user in front of the screen - an API key belongs to the system
    DATA lv_provider TYPE c LENGTH 28.
    DATA lv_service TYPE c LENGTH 17.
    DATA lv_scenario TYPE c LENGTH 30.
    DATA lv_service_id TYPE c LENGTH 40.
    DATA lv_system_id TYPE c LENGTH 60.
    DATA lt_part TYPE string_table.
    FIELD-SYMBOLS <authn> TYPE any.
      DATA lv_url LIKE is_req-url.
      DATA temp19 LIKE lv_scenario.
      DATA temp20 TYPE string.
      DATA temp21 LIKE lv_service_id.
      DATA temp22 TYPE string.
      DATA temp23 LIKE lv_system_id.
      DATA temp24 TYPE string.
      DATA lv_name LIKE is_req-destination.

    lv_provider = `CL_HTTP_DESTINATION_PROVIDER`.
    lv_service  = `IF_A4C_CP_SERVICE`.

    IF is_req-destination IS INITIAL OR is_req-destination = `NONE`.
      
      lv_url = is_req-url.
      CALL METHOD (lv_provider)=>create_by_url
        EXPORTING
          i_url              = lv_url
        RECEIVING
          r_http_destination = result.
    ELSEIF is_req-destination CS `/`.
      SPLIT is_req-destination AT `/` INTO TABLE lt_part.
      
      CLEAR temp19.
      
      READ TABLE lt_part INTO temp20 INDEX 1.
      IF sy-subrc = 0.
        temp19 = temp20.
      ENDIF.
      lv_scenario   = temp19.
      
      CLEAR temp21.
      
      READ TABLE lt_part INTO temp22 INDEX 2.
      IF sy-subrc = 0.
        temp21 = temp22.
      ENDIF.
      lv_service_id = temp21.
      
      CLEAR temp23.
      
      READ TABLE lt_part INTO temp24 INDEX 3.
      IF sy-subrc = 0.
        temp23 = temp24.
      ENDIF.
      lv_system_id  = temp23.
      CALL METHOD (lv_provider)=>create_by_comm_arrangement
        EXPORTING
          comm_scenario      = lv_scenario
          service_id         = lv_service_id
          comm_system_id     = lv_system_id
        RECEIVING
          r_http_destination = result.
    ELSE.
      
      lv_name = is_req-destination.
      ASSIGN (lv_service)=>(`SERVICE_SPECIFIC`) TO <authn>.
      IF <authn> IS NOT ASSIGNED.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = `HTTP_CLIENT_CREATE_ERROR - IF_A4C_CP_SERVICE=>SERVICE_SPECIFIC not found`.
      ENDIF.
      CALL METHOD (lv_provider)=>create_by_cloud_destination
        EXPORTING
          i_name             = lv_name
          i_authn_mode       = <authn>
        RECEIVING
          r_http_destination = result.
    ENDIF.

  ENDMETHOD.

  METHOD client_request_cloud.

    " CL_WEB_HTTP_CLIENT_MANAGER, by name, for the same reason as the
    " destination provider above. The request method is the constant
    " IF_WEB_HTTP_CLIENT=>POST (GET, ...), read dynamically - its type cannot
    " be named on a release that does not have the interface
    DATA lv_manager TYPE c LENGTH 26.
    DATA lv_client_intf TYPE c LENGTH 18.
    DATA lo_client TYPE REF TO object.
    DATA lo_request TYPE REF TO object.
    DATA lo_response TYPE REF TO object.
    DATA ls_status TYPE ty_s_http_status_cloud.
    FIELD-SYMBOLS <method> TYPE any.
    DATA ls_target TYPE zabaputil_cl_util_http=>ty_s_client_target.
    DATA ls_req LIKE is_req.
        DATA lo_destination TYPE REF TO object.
          DATA lv_path LIKE ls_target-request_uri.
        DATA ls_header LIKE LINE OF is_req-t_header.
        DATA lv_body LIKE is_req-body.
        DATA lv_method TYPE string.
          DATA lv_timeout LIKE is_req-timeout.
        DATA x TYPE REF TO cx_root.

    lv_manager     = `CL_WEB_HTTP_CLIENT_MANAGER`.
    lv_client_intf = `IF_WEB_HTTP_CLIENT`.

    " the query goes where it goes on Standard - into the URL, or after the
    " path below a destination
    
    ls_target = client_target( destination = is_req-destination
                                     url         = is_req-url
                                     query       = is_req-query ).
    
    ls_req = is_req.
    ls_req-url = ls_target-url.

    TRY.
        
        lo_destination = client_destination_cloud( ls_req ).

        CALL METHOD (lv_manager)=>create_by_http_destination
          EXPORTING
            i_destination = lo_destination
          RECEIVING
            r_client      = lo_client.

        CALL METHOD lo_client->(`GET_HTTP_REQUEST`)
          RECEIVING
            r_http_request = lo_request.

        " with a destination the url is the path below it, with a URL the
        " path is already part of it
        IF ls_target-request_uri IS NOT INITIAL.
          
          lv_path = ls_target-request_uri.
          CALL METHOD lo_request->(`SET_URI_PATH`)
            EXPORTING
              i_uri_path = lv_path.
        ENDIF.

        
        LOOP AT is_req-t_header INTO ls_header.
          CALL METHOD lo_request->(`SET_HEADER_FIELD`)
            EXPORTING
              i_name  = ls_header-n
              i_value = ls_header-v.
        ENDLOOP.

        
        lv_body = is_req-body.
        CALL METHOD lo_request->(`SET_TEXT`)
          EXPORTING
            i_text = lv_body.

        
        lv_method = to_upper( is_req-method ).
        ASSIGN (lv_client_intf)=>(lv_method) TO <method>.
        IF <method> IS NOT ASSIGNED.
          RAISE EXCEPTION TYPE zabaputil_cx_util_error
            EXPORTING val = |HTTP_CLIENT_METHOD_NOT_FOUND - { is_req-method } is no IF_WEB_HTTP_CLIENT method|.
        ENDIF.

        " the timeout only when one is asked for - the parameter's own
        " default is the client's
        IF is_req-timeout > 0.
          
          lv_timeout = is_req-timeout.
          CALL METHOD lo_client->(`EXECUTE`)
            EXPORTING
              i_method   = <method>
              i_timeout  = lv_timeout
            RECEIVING
              r_response = lo_response.
        ELSE.
          CALL METHOD lo_client->(`EXECUTE`)
            EXPORTING
              i_method   = <method>
            RECEIVING
              r_response = lo_response.
        ENDIF.

        CALL METHOD lo_response->(`GET_STATUS`)
          RECEIVING
            r_value = ls_status.
        result-status_code   = ls_status-code.
        result-status_reason = ls_status-reason.

        CALL METHOD lo_response->(`GET_TEXT`)
          RECEIVING
            r_value = result-body.

        CALL METHOD lo_client->(`CLOSE`).

        
      CATCH cx_root INTO x.
        IF lo_client IS BOUND.
          TRY.
              CALL METHOD lo_client->(`CLOSE`).
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = x.
    ENDTRY.

  ENDMETHOD.


  METHOD delete_response_cookie.

    DATA temp25 TYPE string.
    DATA lv_val LIKE temp25.
      DATA object TYPE REF TO object.
    temp25 = val.
    
    lv_val = temp25.

    IF mo_server_onprem IS BOUND.

      
      object = get_response_onprem( ).

      CALL METHOD object->(`DELETE_COOKIE`)
        EXPORTING
          name = lv_val.

    ENDIF.

  ENDMETHOD.

  METHOD get_response_cookie.

    DATA temp26 TYPE string.
    DATA lv_val LIKE temp26.
      DATA object TYPE REF TO object.
    temp26 = val.
    
    lv_val = temp26.

    IF mo_server_onprem IS BOUND.

      
      object = get_response_onprem( ).

      CALL METHOD object->(`GET_COOKIE`)
        EXPORTING
          name  = lv_val
        IMPORTING
          value = result.

      " reading a response cookie has no released counterpart in ABAP
      " Cloud - there is nothing to answer with there
    ENDIF.

  ENDMETHOD.

  METHOD get_header_field.

    DATA temp27 TYPE string.
    DATA lv_val LIKE temp27.
      DATA object TYPE REF TO object.
    temp27 = val.
    
    lv_val = temp27.

    IF mo_server_onprem IS BOUND.

      
      object = get_request_onprem( ).

      CALL METHOD object->(`GET_HEADER_FIELD`)
        EXPORTING
          name  = lv_val
        RECEIVING
          value = result.

    ELSE.

      CALL METHOD mo_request_cloud->(`IF_WEB_HTTP_REQUEST~GET_HEADER_FIELD`)
        EXPORTING
          i_name  = lv_val
        RECEIVING
          r_value = result.

    ENDIF.

  ENDMETHOD.

  METHOD set_header_field.

    DATA temp28 TYPE string.
    DATA lv_n LIKE temp28.
    DATA temp29 TYPE string.
    DATA lv_v LIKE temp29.
      DATA temp30 TYPE string.
      DATA lv_cr LIKE temp30.
      DATA temp31 TYPE string.
      DATA lv_lf LIKE temp31.
      DATA object TYPE REF TO object.
    temp28 = n.
    
    lv_n = temp28.
    
    temp29 = v.
    
    lv_v = temp29.

    " strip CR/LF from both halves before they reach the stack: a header
    " name or value that carries a line break splits the response into two
    " (response splitting), and the halves of a header are routinely
    " derived from request data by the caller. The stack may reject
    " embedded CRLF itself, but nothing here should depend on it
    IF lv_n CA zabaputil_cl_util_context=>cv_char_util_cr_lf
        OR lv_v CA zabaputil_cl_util_context=>cv_char_util_cr_lf.
      
      temp30 = zabaputil_cl_util_context=>cv_char_util_cr_lf(1).
      
      lv_cr = temp30.
      
      temp31 = zabaputil_cl_util_context=>cv_char_util_cr_lf+1(1).
      
      lv_lf = temp31.
      REPLACE ALL OCCURRENCES OF lv_cr IN lv_n WITH ``.
      REPLACE ALL OCCURRENCES OF lv_lf IN lv_n WITH ``.
      REPLACE ALL OCCURRENCES OF lv_cr IN lv_v WITH ``.
      REPLACE ALL OCCURRENCES OF lv_lf IN lv_v WITH ``.
    ENDIF.

    IF mo_server_onprem IS BOUND.

      
      object = get_response_onprem( ).

      CALL METHOD object->(`SET_HEADER_FIELD`)
        EXPORTING
          name  = lv_n
          value = lv_v.

    ELSE.

      CALL METHOD mo_response_cloud->(`IF_WEB_HTTP_RESPONSE~SET_HEADER_FIELD`)
        EXPORTING
          i_name  = lv_n
          i_value = lv_v.

    ENDIF.

  ENDMETHOD.

  METHOD factory.

    CREATE OBJECT result.
    result->mo_server_onprem = server.

  ENDMETHOD.

  METHOD factory_cloud.

    CREATE OBJECT result.
    result->mo_request_cloud  = req.
    result->mo_response_cloud = res.

  ENDMETHOD.

  METHOD get_cdata.
      DATA object TYPE REF TO object.

    IF mo_server_onprem IS BOUND.

      
      object = get_request_onprem( ).

      CALL METHOD object->(`GET_CDATA`)
        RECEIVING
          data = result.

    ELSE.

      CALL METHOD mo_request_cloud->(`IF_WEB_HTTP_REQUEST~GET_TEXT`)
        RECEIVING
          r_value = result.

    ENDIF.

  ENDMETHOD.

  METHOD get_method.
      DATA object TYPE REF TO object.

    IF mo_server_onprem IS BOUND.

      
      object = get_request_onprem( ).

      CALL METHOD object->(`IF_HTTP_REQUEST~GET_METHOD`)
        RECEIVING
          method = result.

    ELSE.

      CALL METHOD mo_request_cloud->(`IF_WEB_HTTP_REQUEST~GET_METHOD`)
        RECEIVING
          r_value = result.

    ENDIF.

  ENDMETHOD.

  METHOD set_cdata.

    " a string, whatever clike the caller passed: IF_HTTP_RESPONSE~SET_CDATA
    " and IF_WEB_HTTP_RESPONSE~SET_TEXT both take a STRING, and a dynamic
    " call with a C actual for it is CX_SY_DYN_CALL_ILLEGAL_TYPE - the typed
    " call would have converted. Converted ONCE, above the branch: the cloud
    " half used to pass the generic value through, so a CHAR caller worked
    " on-prem and failed on cloud only
    DATA temp32 TYPE string.
    DATA lv_data LIKE temp32.
      DATA object TYPE REF TO object.
    temp32 = val.
    
    lv_data = temp32.

    IF mo_server_onprem IS BOUND.

      
      object = get_response_onprem( ).

      CALL METHOD object->(`SET_CDATA`)
        EXPORTING
          data = lv_data.

    ELSE.

      CALL METHOD mo_response_cloud->(`IF_WEB_HTTP_RESPONSE~SET_TEXT`)
        EXPORTING
          i_text = lv_data.

    ENDIF.

  ENDMETHOD.

  METHOD set_status.

    DATA temp33 TYPE string.
    DATA lv_reason LIKE temp33.
      DATA object TYPE REF TO object.
    temp33 = reason.
    
    lv_reason = temp33.

    IF mo_server_onprem IS BOUND.

      
      object = get_response_onprem( ).

      CALL METHOD object->(`IF_HTTP_RESPONSE~SET_STATUS`)
        EXPORTING
          code   = code
          reason = lv_reason.

    ELSE.

      CALL METHOD mo_response_cloud->(`IF_WEB_HTTP_RESPONSE~SET_STATUS`)
        EXPORTING
          i_code   = code
          i_reason = lv_reason.

    ENDIF.

  ENDMETHOD.

  METHOD set_session_stateful.

    IF mo_server_onprem IS BOUND.

      CALL METHOD mo_server_onprem->(`SET_SESSION_STATEFUL`)
        EXPORTING
          stateful = val.

      " stateful sessions are not released in ABAP Cloud - no-op there
    ENDIF.

  ENDMETHOD.

  METHOD get_req_info.

    result-body     = get_cdata( ).
    result-method   = get_method( ).
    result-path     = get_header_field( `~path` ).
    result-t_params = zabaputil_cl_util_context=>url_param_get_tab( get_header_field( `~request_uri` ) ).
    result-query    = get_query( ).

  ENDMETHOD.

  METHOD get_request_onprem.
      FIELD-SYMBOLS <any> TYPE any.

    IF mo_request_onprem IS NOT BOUND.
      
      " IS ASSIGNED, not sy-subrc, and a RAISE rather than an ASSERT - see
      " the note at get_response_onprem below, which both share
      ASSIGN mo_server_onprem->(`REQUEST`) TO <any>.
      IF <any> IS NOT ASSIGNED.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = `HTTP_SERVER_REQUEST_NOT_FOUND - the server object has no REQUEST attribute`.
      ENDIF.
      mo_request_onprem = <any>.
    ENDIF.

    result = mo_request_onprem.

  ENDMETHOD.

  METHOD get_response_onprem.
      FIELD-SYMBOLS <any> TYPE any.

    IF mo_response_onprem IS NOT BOUND.
      
      " Both halves of this guard changed together, and both matter on the
      " path every single request takes (get_cdata / get_method /
      " get_header_field all come through here):
      "
      " IS ASSIGNED, not sy-subrc - a successful dynamic ASSIGN does not
      " reset sy-subrc on every release (#1937,
      " zabaputil_cl_util_context=>unassign_data), so a stale 4 left by an
      " earlier statement read as "the server has no RESPONSE" on the one
      " statement of the handler that cannot fail.
      "
      " ...and a RAISE rather than an ASSERT, because of what the failure
      " then costs: ASSERTION_FAILED is a short dump no TRY can take (the
      " reasoning is written out at z2ui5_cl_ui5_view_builder=>a), so it
      " bypassed the outer TRY of z2ui5_cl_ui5_http_handler=>main and the
      " request was answered with no body, no status code and none of the
      " security headers - the exact state that TRY was added to prevent.
      " The framework exception travels into it and comes back as a 500
      " naming the cause
      ASSIGN mo_server_onprem->(`RESPONSE`) TO <any>.
      IF <any> IS NOT ASSIGNED.
        RAISE EXCEPTION TYPE zabaputil_cx_util_error
          EXPORTING val = `HTTP_SERVER_RESPONSE_NOT_FOUND - the server object has no RESPONSE attribute`.
      ENDIF.
      mo_response_onprem = <any>.
    ENDIF.

    result = mo_response_onprem.

  ENDMETHOD.

  METHOD client_target.

    " The query travels as it came, never parsed and encoded again (see
    " get_query). By URL it becomes part of the URL. By destination it is the
    " request URI, which the ICF client appends to the destination's path
    " prefix - so the path stays the one maintained in SM59. The destination
    " test is client_create's, which decides between the two
    DATA lv_destination TYPE c LENGTH 32.
    DATA temp34 TYPE string.
    DATA lv_query LIKE temp34.
    lv_destination = destination.

    result-url = url.
    
    temp34 = query.
    
    lv_query = temp34.
    SHIFT lv_query LEFT DELETING LEADING `?`.

    " by destination the url is the path below it (client_request; client_call
    " passes none), and the query follows that path
    IF lv_destination IS NOT INITIAL AND lv_destination <> `NONE`.
      result-request_uri = url.
      IF lv_query IS NOT INITIAL.
        result-request_uri = |{ result-request_uri }?{ lv_query }|.
      ENDIF.
      RETURN.
    ENDIF.

    IF lv_query IS INITIAL.
      RETURN.
    ELSEIF result-url CS `?`.
      result-url = |{ result-url }&{ lv_query }|.
    ELSE.
      result-url = |{ result-url }?{ lv_query }|.
    ENDIF.

  ENDMETHOD.

  METHOD get_query.

    DATA lv_uri TYPE string.
    lv_uri = get_header_field( `~request_uri` ).
    IF lv_uri CS `?`.
      result = substring_after( val = lv_uri
                                sub = `?` ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_http_con DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES: BEGIN OF ty_s_file,
             file     TYPE xstring,
             filename TYPE string,
           END OF ty_s_file.

    TYPES: BEGIN  OF ty_s_multipart,
             content_type TYPE string,
             header_name  TYPE string,
             header_value TYPE string,
             form_name    TYPE string,
             form_value   TYPE string,
             file_data    TYPE ty_s_file,
             is_file      TYPE abap_bool,
           END OF ty_s_multipart.

    TYPES: BEGIN OF ty_s_response,
             response TYPE string,
             request  TYPE string,
             status   TYPE char3,
             message  TYPE string,
             file_    TYPE xstring,
           END OF ty_s_response.


    METHODS constructor
      IMPORTING
                !iv_hostname TYPE string OPTIONAL
      RAISING   zcx_rest_exception.

    METHODS set_context_type
      IMPORTING
        !iv_context_type TYPE string .

    METHODS set_host_name
      IMPORTING
        iv_host_name TYPE string
      RAISING
        zcx_rest_exception .

    METHODS set_path
      IMPORTING
        iv_host_path TYPE string
      RAISING
        zcx_rest_exception .

    METHODS set_url
      IMPORTING
                iv_url TYPE string
      RAISING   zcx_rest_exception.

    METHODS append_body_form
      IMPORTING
        !iv_body_name  TYPE string
        !iv_body_value TYPE string .

    METHODS set_body
      IMPORTING
        iv_body         TYPE string
        iv_content_type TYPE string DEFAULT 'application/json'
        iv_method_type  TYPE string DEFAULT 'GET'.

    METHODS set_multipart_data
      IMPORTING
        iv_content_type TYPE string OPTIONAL
        iv_header_value TYPE string OPTIONAL
        iv_header_name  TYPE string OPTIONAL
        !iv_form_name   TYPE string
        !iv_form_value  TYPE string OPTIONAL
        !is_file        TYPE ty_s_file OPTIONAL .

    METHODS set_method_type
      IMPORTING
        !iv_method_type TYPE string .


    METHODS set_header_fields
      IMPORTING
        !iv_name  TYPE string
        !iv_value TYPE string .

    METHODS execute
      IMPORTING
        !iv_timeout        TYPE i DEFAULT 60
      EXPORTING
        !eo_request        TYPE REF TO if_http_request
        !ev_errortext      TYPE string
        !ev_subrc          TYPE sysubrc
      RETURNING
        VALUE(ro_response) TYPE REF TO if_http_response
      RAISING
        zcx_rest_exception.

    METHODS set_proxy
      IMPORTING
        !iv_proxy_host    TYPE string
        !iv_proxy_service TYPE string .


    METHODS set_free .


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_body_form TYPE tihttpnvp .
    DATA mt_header TYPE tihttpnvp .
    DATA mv_body_x TYPE xstring .
    DATA mv_method_type TYPE string .
    DATA mv_context_type TYPE string .
    DATA mv_host_name TYPE string .
    DATA mv_host_path TYPE string .
    DATA mv_url       TYPE string.
    DATA mt_multipart_data TYPE STANDARD TABLE OF ty_s_multipart .

    " Proxy data declaration
    DATA mv_proxy_host TYPE string .
    DATA mv_proxy_service TYPE string .

    METHODS assign_multipart_data
      IMPORTING
        !io_http_client TYPE REF TO if_http_client .


ENDCLASS.



CLASS zcl_http_con IMPLEMENTATION.


  METHOD append_body_form.
    DATA: ls_form_body  TYPE ihttpnvp.

    ls_form_body-name = iv_body_name.
    ls_form_body-value = iv_body_value.

    APPEND ls_form_body TO me->mt_body_form.
  ENDMETHOD.


  METHOD constructor.
    " Set the Host Name
    me->set_host_name( iv_hostname ).

  ENDMETHOD.


  METHOD execute.

    DATA: lo_http_client TYPE REF TO if_http_client.
    DATA: lv_full_url_path TYPE string.

    TRY.
        IF me->mv_url IS INITIAL.
          CONCATENATE mv_host_name mv_host_path INTO lv_full_url_path.
        ELSE.
          lv_full_url_path = me->mv_url.
        ENDIF.

        cl_http_client=>create_by_url(
          EXPORTING
            url                = lv_full_url_path
            proxy_host         = me->mv_proxy_host
            proxy_service      = me->mv_proxy_service
          IMPORTING
            client             = lo_http_client
          EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
            OTHERS             = 4
        ).

        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              MESSAGE e001(00) WITH 'Argument Not Found !' INTO zcx_rest_exception=>mv_msg_text.
            WHEN 2.
              MESSAGE e001(00) WITH 'Plugin not Active !'  INTO zcx_rest_exception=>mv_msg_text.
            WHEN 3.
              MESSAGE e001(00) WITH 'Internal Error !'     INTO zcx_rest_exception=>mv_msg_text.
          ENDCASE.

          zcx_rest_exception=>s_raise( ).
        ENDIF.

        " set header fields
        lo_http_client->request->set_header_fields( fields = me->mt_header ).

        " disable the pop-up for authentication
        lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.

        " set method type
        lo_http_client->request->set_method( me->mv_method_type ).

        IF me->mv_context_type IS NOT INITIAL.
          lo_http_client->request->set_content_type( me->mv_context_type ).
        ENDIF.

        IF me->mt_multipart_data IS NOT INITIAL.
          " prepare file for sending
          me->assign_multipart_data( lo_http_client ).
        ENDIF.

        IF me->mv_body_x IS NOT INITIAL.
          " assign the xbody
          lo_http_client->request->set_data( me->mv_body_x ).
        ENDIF.

        "assign form fields if there are any in sap
        IF me->mt_body_form IS NOT INITIAL.
          lo_http_client->request->set_form_fields(
            EXPORTING
              fields     = me->mt_body_form    " Form fields
          ).
        ENDIF.

        lo_http_client->send(
          EXPORTING
            timeout                    = iv_timeout    " Timeout of Answer Waiting Time
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5
        ).


        IF sy-subrc = 0.

          CASE sy-subrc.
            WHEN 1.
              MESSAGE e001(00) WITH 'HTTP Communication Failure !' INTO zcx_rest_exception=>mv_msg_text.
            WHEN 2.
              MESSAGE e001(00) WITH 'HTTP Invalid State !'         INTO zcx_rest_exception=>mv_msg_text.
            WHEN 3.
              MESSAGE e001(00) WITH 'HTTP Processing Failed !'     INTO zcx_rest_exception=>mv_msg_text.
            WHEN 4.
              MESSAGE e001(00) WITH 'HTTP Invalid Timeout!'        INTO zcx_rest_exception=>mv_msg_text.
          ENDCASE.

          zcx_rest_exception=>s_raise( ).
        ENDIF.

        lo_http_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4
        ).

        IF sy-subrc = 0.
          CASE sy-subrc.
            WHEN 1.
              MESSAGE e001(00) WITH 'HTTP Communication Failure !' INTO zcx_rest_exception=>mv_msg_text.
            WHEN 2.
              MESSAGE e001(00) WITH 'HTTP Invalid State !'         INTO zcx_rest_exception=>mv_msg_text.
            WHEN 3.
              MESSAGE e001(00) WITH 'HTTP Processing Failed !'     INTO zcx_rest_exception=>mv_msg_text.
          ENDCASE.

          zcx_rest_exception=>s_raise( ).
        ENDIF.

        " return the structure with the information
        ro_response = lo_http_client->response.
        eo_request = lo_http_client->request.

        " close http connection
        lo_http_client->close( ).

      CATCH zcx_rest_exception INTO DATA(lo_x_catch).
        " if the response error
        IF lo_http_client IS BOUND.
          lo_http_client->get_last_error( IMPORTING code = ev_subrc message = ev_errortext ).
        ENDIF.

        RAISE EXCEPTION lo_x_catch.

    ENDTRY.

  ENDMETHOD.


  METHOD set_body.
    " Define Body Parameters
    DATA: lv_payload_x TYPE xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_body
      IMPORTING
        buffer = lv_payload_x.

    me->mv_body_x = lv_payload_x.

    IF me->mv_context_type IS INITIAL.
      me->mv_context_type = iv_content_type .
    ENDIF.

    IF me->mv_method_type IS INITIAL.
      me->mv_method_type = iv_method_type.
    ENDIF.
  ENDMETHOD.


  METHOD set_context_type.
    " Define the Context Type
    me->mv_context_type = iv_context_type.

  ENDMETHOD.


  METHOD assign_multipart_data.

    DATA: lo_m_part TYPE REF TO if_http_entity.
    DATA: ls_file TYPE ty_s_file.

    FIELD-SYMBOLS: <ls_multipart_data> TYPE ty_s_multipart.

    LOOP AT me->mt_multipart_data ASSIGNING <ls_multipart_data>.
      CLEAR: ls_file.

      " Create multipart
      lo_m_part = io_http_client->request->add_multipart( ).

      CHECK lo_m_part IS BOUND.

      lo_m_part->set_content_type( content_type = <ls_multipart_data>-content_type ). "
      IF <ls_multipart_data>-header_name IS NOT INITIAL AND <ls_multipart_data>-header_value IS NOT INITIAL.
        lo_m_part->set_header_field( name = <ls_multipart_data>-header_name value = <ls_multipart_data>-header_value ).
      ENDIF.

      IF <ls_multipart_data>-form_name IS NOT INITIAL.
        CASE <ls_multipart_data>-is_file.
          WHEN abap_true. "file
            " Assign the header and the data for the file
            ls_file = <ls_multipart_data>-file_data.
            lo_m_part->set_header_field( name = if_http_header_fields=>content_disposition value = |form-data; name="{ <ls_multipart_data>-form_name }"; filename="{ ls_file-filename }"| ).
            lo_m_part->set_data( data = ls_file-file ).

          WHEN OTHERS.
            " Assign header field
            lo_m_part->set_header_field( name = if_http_header_fields=>content_disposition value = |form-data;name="{ <ls_multipart_data>-form_name }"| ).
            lo_m_part->set_cdata( data = <ls_multipart_data>-form_value ).

        ENDCASE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_free.
    " Clear instance
    CLEAR:   mv_host_path,
             mv_context_type,
             mv_method_type,
             mv_body_x,
             mt_header[],
             mt_header ,
             mt_body_form,
             mt_body_form[],
             mt_multipart_data,
             mt_multipart_data[].
  ENDMETHOD.


  METHOD set_header_fields.
    " Define the header fields
    DATA: ls_header  TYPE ihttpnvp.

    ls_header-name = iv_name.
    ls_header-value = iv_value.

    APPEND ls_header TO me->mt_header.
  ENDMETHOD.


  METHOD set_host_name.
    IF me->mv_url IS NOT INITIAL.
      MESSAGE e001(00) WITH 'URL is already assigned !' INTO zcx_rest_exception=>mv_msg_text.
      zcx_rest_exception=>s_raise( ).
    ENDIF.

    " Define the HostName
    me->mv_host_name = iv_host_name.
  ENDMETHOD.


  METHOD set_url.
    IF me->mv_host_name IS NOT INITIAL OR me->mv_host_path IS NOT INITIAL.
      MESSAGE e001(00) WITH 'Hostname & Path are already assigned !' INTO zcx_rest_exception=>mv_msg_text.
      zcx_rest_exception=>s_raise( ).
    ENDIF.

    me->mv_url = iv_url.
  ENDMETHOD.

  METHOD set_method_type.
    " set the method type
    me->mv_method_type = iv_method_type.
    " Set the Method Type to Uppercase letters
    TRANSLATE me->mv_method_type TO UPPER CASE.
  ENDMETHOD.


  METHOD set_multipart_data.
    " Add Multipart data
    DATA: ls_multipart_data TYPE ty_s_multipart.

    ls_multipart_data-content_type = iv_content_type.
    ls_multipart_data-header_name = iv_header_name.
    ls_multipart_data-header_value = iv_header_value.
    ls_multipart_data-form_name = iv_form_name.
    ls_multipart_data-form_value = iv_form_value.

    IF is_file IS NOT INITIAL.
      ls_multipart_data-file_data = is_file.
      ls_multipart_data-is_file = abap_true.
    ENDIF.

    " append the structure info with the multipart data
    APPEND ls_multipart_data TO me->mt_multipart_data.
  ENDMETHOD.


  METHOD set_path.
    IF me->mv_url IS NOT INITIAL.
      MESSAGE e001(00) WITH 'URL is already assigned !' INTO zcx_rest_exception=>mv_msg_text.
      zcx_rest_exception=>s_raise( ).
    ENDIF.

    " Provide the host path
    me->mv_host_path = iv_host_path.
  ENDMETHOD.


  METHOD set_proxy.
    me->mv_proxy_host = iv_proxy_host.
    me->mv_proxy_service = iv_proxy_service.
  ENDMETHOD.

ENDCLASS.

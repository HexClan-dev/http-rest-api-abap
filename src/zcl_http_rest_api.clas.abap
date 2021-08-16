CLASS zcl_http_rest_api DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_status_create TYPE string VALUE '201' ##NO_TEXT.
    CONSTANTS gc_status_update TYPE string VALUE '200' ##NO_TEXT.
    CONSTANTS gc_status_list TYPE string VALUE '200' ##NO_TEXT.
    CONSTANTS gc_status_delete TYPE string VALUE '204' ##NO_TEXT.
    CONSTANTS gc_token_jwt TYPE char10 VALUE 'JWT' ##NO_TEXT.
    CONSTANTS gc_token_bearer TYPE char10 VALUE 'Bearer' ##NO_TEXT.
    CONSTANTS gc_auth_basic TYPE char10 VALUE 'Basic' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !iv_hostname TYPE string
      RAISING
        zcx_rest_exception.

    METHODS authentication
      IMPORTING
        !iv_username   TYPE char200 OPTIONAL
        !iv_password   TYPE char200 OPTIONAL
        !iv_token      TYPE string OPTIONAL
        !iv_token_type TYPE char20 OPTIONAL
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_http_rest_api .

    METHODS add_header
      IMPORTING
        !iv_name       TYPE string
        !iv_value      TYPE string
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_http_rest_api .


    METHODS set_method_type
      IMPORTING
        !iv_body         TYPE string OPTIONAL
        !iv_content_type TYPE string OPTIONAL
        !iv_method_type  TYPE string
      RETURNING
        VALUE(ro_self)   TYPE REF TO zcl_http_rest_api .

    METHODS execute
      IMPORTING
        !iv_timeout         TYPE i
      RETURNING
        VALUE(rs_http_data) TYPE zcl_http_con=>ty_s_response
      RAISING
        zcx_rest_exception .


    METHODS set_path
      IMPORTING
        iv_url       TYPE string OPTIONAL
        iv_host_path TYPE string OPTIONAL
        iv_host_name TYPE string OPTIONAL
          PREFERRED PARAMETER iv_url
      RAISING
        zcx_rest_exception.


  PROTECTED SECTION.
    DATA: mo_http_con TYPE REF TO zcl_http_con.
    DATA: mv_token TYPE string.

    DATA: mv_username TYPE char200,
          mv_password TYPE char200.

    DATA: mv_url      TYPE string,
          mv_hostname TYPE string,
          mv_path     TYPE string.


    METHODS convert2base64credentials
      RETURNING VALUE(rv_base64_val) TYPE string.

    METHODS clear.


  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_http_rest_api IMPLEMENTATION.

  METHOD add_header.
    me->mo_http_con->set_header_fields( iv_name = iv_name iv_value = iv_value ).
    ro_self = me.
  ENDMETHOD.


  METHOD authentication.

    DATA: lv_header_value TYPE string.

    IF iv_token IS SUPPLIED.
      me->mv_token = iv_token.

      lv_header_value = |{ iv_token_type } { iv_token }|.
    ELSEIF iv_username IS SUPPLIED AND iv_password IS SUPPLIED.
      me->mv_username = iv_username.
      me->mv_password = iv_password.

      lv_header_value = |{ gc_auth_basic } { me->convert2base64credentials( ) }|.
    ELSE.
      RETURN.
    ENDIF.

    " Provide Credentials on the Header
    mo_http_con->set_header_fields( iv_name = 'Authorization' iv_value = lv_header_value ).

    ro_self = me.
  ENDMETHOD.

  METHOD constructor.
    " Create the HTTP Communication Instance
    me->mo_http_con = NEW zcl_http_con( iv_hostname ).
  ENDMETHOD.


  METHOD convert2base64credentials.
    " Encode Username & Password for Authentication
    rv_base64_val = cl_http_utility=>encode_base64( unencoded = |{ me->mv_username }:{ me->mv_password }| ).

  ENDMETHOD.


  METHOD execute.

    DATA: lo_http_req TYPE REF TO if_http_request.

    " TODO -> add a Structure Type for returning HTTP
    DATA(lo_http_response) = me->mo_http_con->execute(
      EXPORTING
        iv_timeout   = iv_timeout
      IMPORTING
        eo_request   = lo_http_req
        ev_errortext = rs_http_data-message
    ).

    " return the Response with the information
    rs_http_data-status = lo_http_response->get_header_field( name = '~status_code' ).
    rs_http_data-response = lo_http_response->get_cdata(  ).
    rs_http_data-request = lo_http_response->get_cdata( ).
    rs_http_data-file_ = lo_http_response->get_data( ).

    me->mo_http_con->set_free( ).

  ENDMETHOD.


  METHOD set_method_type.

    " Set Method Type
    me->mo_http_con->set_body(
      EXPORTING
        iv_body         = iv_body
        iv_content_type = iv_content_type
        iv_method_type  = iv_method_type
    ).

    ro_self = me.
  ENDMETHOD.


  METHOD set_path.

    IF iv_url IS SUPPLIED.
      me->mo_http_con->set_url( iv_url ).
    ELSEIF iv_host_name IS SUPPLIED AND iv_host_path IS SUPPLIED.
      " Insert HostName and the Path
      me->mo_http_con->set_host_name( iv_host_name = iv_host_name ).
      me->mo_http_con->set_path( iv_host_path = iv_host_path ).
    ELSE.
      MESSAGE e001(00) WITH 'Please define URL ' 'OR HostName and Path ' INTO zcx_rest_exception=>mv_msg_text.
      zcx_rest_exception=>s_raise(  ).
    ENDIF.

  ENDMETHOD.

  METHOD clear.
    CLEAR:
        me->mv_hostname,
        me->mv_username,
        me->mv_password,
        me->mv_path,
        me->mv_token,
        me->mv_url.

    " Clear the http object part
    me->mo_http_con->set_free( ).
  ENDMETHOD.


ENDCLASS.

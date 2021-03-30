CLASS zcl_http_rest_api DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.


    CONSTANTS gc_status_create TYPE string VALUE '201'.
    CONSTANTS gc_status_update TYPE string VALUE '200'.
    CONSTANTS gc_status_list   TYPE string VALUE '200'.
    CONSTANTS gc_status_delete TYPE string VALUE '204'.

    CONSTANTS gc_token_jwt        TYPE char10 VALUE 'JWT'.
    CONSTANTS gc_token_bearer     TYPE char10 VALUE 'Bearer'.
    CONSTANTS gc_auth_basic       TYPE char10 VALUE 'Basic'.

    METHODS:
      constructor
        IMPORTING
          iv_hostname TYPE char200.

    METHODS:

      authenticate
        IMPORTING
                  iv_username    TYPE char200 OPTIONAL
                  iv_password    TYPE char200 OPTIONAL
                  iv_token       TYPE string  OPTIONAL
                  iv_token_type  TYPE char20  OPTIONAL
        RETURNING VALUE(ro_self) TYPE REF TO zcl_http_rest_api,

      add_header
        IMPORTING
                  !iv_name       TYPE string
                  !iv_value      TYPE string
        RETURNING VALUE(ro_self) TYPE REF TO zcl_http_rest_api,

      post
        IMPORTING
                  iv_body         TYPE string OPTIONAL
                  iv_content_type TYPE string OPTIONAL
        RETURNING VALUE(ro_self)  TYPE REF TO zcl_http_rest_api,

      get
        IMPORTING
                  iv_body         TYPE string OPTIONAL
                  iv_content_type TYPE string OPTIONAL
        RETURNING VALUE(ro_self)  TYPE REF TO zcl_http_rest_api,

      put
        IMPORTING
                  iv_body         TYPE string OPTIONAL
                  iv_content_type TYPE string OPTIONAL
        RETURNING VALUE(ro_self)  TYPE REF TO zcl_http_rest_api,

      delete
        IMPORTING
                  iv_body         TYPE string OPTIONAL
                  iv_content_type TYPE string OPTIONAL
        RETURNING VALUE(ro_self)  TYPE REF TO zcl_http_rest_api,

      set_method_type
        IMPORTING
                  iv_body         TYPE string OPTIONAL
                  iv_content_type TYPE string OPTIONAL
                  !iv_method_type TYPE string
        RETURNING VALUE(ro_self)  TYPE REF TO zcl_http_rest_api,

      set_file
        IMPORTING
          is_file TYPE zcl_http_con=>ty_s_file,

      execute
        IMPORTING
          !iv_timeout   TYPE i
        EXPORTING
          !eo_request   TYPE REF TO if_http_request
          !ev_errortext TYPE string
        RAISING
          zcx_rest_exception
        .


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_http_con TYPE REF TO zcl_http_con.

    DATA: mv_token TYPE string.
    DATA: mv_hostname TYPE char200.

    DATA: mv_username TYPE char200,
          mv_password TYPE char200.


    METHODS convert2base64credentials
      RETURNING VALUE(rv_base64_val) TYPE string.


ENDCLASS.



CLASS zcl_http_rest_api IMPLEMENTATION.


  METHOD constructor.
    " Create the HTTP Communication Instance
    me->mo_http_con = NEW zcl_http_con( ).
  ENDMETHOD.


  METHOD convert2base64credentials.

    " Encode Username & Password for Authentication
    rv_base64_val = cl_http_utility=>encode_base64( unencoded = |{ me->mv_username }:{ me->mv_password }| ).

  ENDMETHOD.


  METHOD authenticate.

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

  ENDMETHOD.



  METHOD add_header.
    me->mo_http_con->set_header_fields( iv_name = iv_name iv_value = iv_value ).
    ro_self = me.
  ENDMETHOD.


  METHOD post.
    " Call Post Method
    me->set_method_type(
        iv_method_type  = 'POST'
        iv_body         = iv_body
        iv_content_type = iv_content_type
    ).

    ro_self = me.
  ENDMETHOD.

  METHOD get.
    " Call Get Method
    me->set_method_type(
        iv_method_type  = 'GET'
        iv_body         = iv_body
        iv_content_type = iv_content_type
    ).

    ro_self = me.
  ENDMETHOD.

  METHOD put.
    " Call Put Method
    me->set_method_type(
        iv_method_type  = 'PUT'
        iv_body         = iv_body
        iv_content_type = iv_content_type
    ).

    ro_self = me.
  ENDMETHOD.

  METHOD delete.
    " Set Method Type
    me->set_method_type(
        iv_method_type  = 'DELETE'
        iv_body         = iv_body
        iv_content_type = iv_content_type
    ).


    ro_self = me.
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

  METHOD execute.


    " TODO -> add a Structure Type for returning HTTP
    me->mo_http_con->execute(
      EXPORTING
        iv_timeout   = iv_timeout
*      IMPORTING
*        eo_request   =
*        ev_errortext =
*      RECEIVING
*        ro_response  =
    ).

  ENDMETHOD.

  METHOD set_file.

    " TODO -> implement sending File

  ENDMETHOD.

ENDCLASS.

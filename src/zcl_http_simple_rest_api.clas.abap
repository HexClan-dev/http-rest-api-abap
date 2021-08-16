CLASS zcl_http_simple_rest_api DEFINITION
  INHERITING FROM zcl_http_rest_api
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS post
      IMPORTING
        !iv_body         TYPE string OPTIONAL
        !iv_content_type TYPE string OPTIONAL
      RETURNING
        VALUE(ro_self)   TYPE REF TO zcl_http_rest_api .

    METHODS get
      IMPORTING
        !iv_body         TYPE string OPTIONAL
        !iv_content_type TYPE string OPTIONAL
      RETURNING
        VALUE(ro_self)   TYPE REF TO zcl_http_rest_api .

    METHODS put
      IMPORTING
        !iv_body         TYPE string OPTIONAL
        !iv_content_type TYPE string OPTIONAL
      RETURNING
        VALUE(ro_self)   TYPE REF TO zcl_http_rest_api .

    METHODS delete
      IMPORTING
        !iv_body         TYPE string OPTIONAL
        !iv_content_type TYPE string OPTIONAL
      RETURNING
        VALUE(ro_self)   TYPE REF TO zcl_http_rest_api .

    METHODS set_method_type REDEFINITION.

    METHODS authentication REDEFINITION.

    METHODS add_header REDEFINITION.

    METHODS execute REDEFINITION.

    METHODS: set_json_body
      IMPORTING
                ir_any_data   TYPE any
      RETURNING VALUE(ro_ref) TYPE REF TO zcl_http_rest_api.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_http_simple_rest_api IMPLEMENTATION.



  METHOD authentication.

    super->authentication(
        iv_password = iv_password
        iv_token = iv_token
        iv_token_type = iv_token_type
        iv_username = iv_username
    ).

    ro_self = me.
  ENDMETHOD.


  METHOD add_header.
    " Add a header value
    super->add_header(
      EXPORTING
        iv_name  = iv_name
        iv_value = iv_value
    ).

    ro_self = me.
  ENDMETHOD.


  METHOD execute.


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


  METHOD get.
    " Call Get Method
    me->set_method_type(
        iv_method_type  = 'GET'
        iv_body         = iv_body
        iv_content_type = iv_content_type
    ).

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


  METHOD set_json_body.

  ENDMETHOD.

ENDCLASS.

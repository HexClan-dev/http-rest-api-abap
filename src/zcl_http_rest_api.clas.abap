CLASS zcl_http_rest_api DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iv_hostname TYPE char200.

    METHODS:

      authenticate
        IMPORTING
                  iv_username    TYPE char200 OPTIONAL
                  iv_password    TYPE char200 OPTIONAL
                  iv_token       TYPE string OPTIONAL
                    PREFERRED PARAMETER iv_token
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
        RETURNING VALUE(ro_self) TYPE REF TO zcl_http_rest_api,

      put
        RETURNING VALUE(ro_self) TYPE REF TO zcl_http_rest_api,

      delete
        RETURNING VALUE(ro_self) TYPE REF TO zcl_http_rest_api,

      set_method_type
        IMPORTING
                  !iv_method_type TYPE string
        RETURNING VALUE(ro_self)  TYPE REF TO zcl_http_rest_api,

      execute
        IMPORTING
          !iv_timeout   TYPE i
        EXPORTING
          !eo_request   TYPE REF TO if_http_request
          !ev_errortext TYPE string
        .


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_http_con TYPE REF TO zcl_http_con.

    DATA: mv_token TYPE string.
    DATA: mv_hostname TYPE char200.

    DATA: mv_username TYPE char200,
          mv_password TYPE char200.


ENDCLASS.



CLASS zcl_http_rest_api IMPLEMENTATION.


  METHOD constructor.
    " Create the HTTP Communication Instance
    me->mo_http_con = NEW zcl_http_con(  ).
  ENDMETHOD.


  METHOD authenticate.

    IF iv_token IS SUPPLIED.
      me->mv_token = iv_token.
    ELSEIF iv_username IS SUPPLIED AND iv_password IS SUPPLIED.
      me->mv_username = iv_username.
      me->mv_password = iv_password.
    ELSE.
      " TODO -> Raise Exception to provide credentials
    ENDIF.

    ro_self = me.
  ENDMETHOD.



  METHOD add_header.
    me->mo_http_con->set_header_fields( iv_name = iv_name iv_value = iv_value ).
    ro_self = me.
  ENDMETHOD.


  METHOD post.
    " Set Method Type
    me->mo_http_con->set_body(
          EXPORTING
            iv_body         = iv_body
            iv_content_type = iv_content_type
            iv_method_type  = 'POST'
        ).


    ro_self = me.
  ENDMETHOD.

  METHOD get.
    " Set Method Type
    me->mo_http_con->set_method_type( iv_method_type = 'GET' ).

    ro_self = me.
  ENDMETHOD.

  METHOD put.
    " Set Method Type
    me->mo_http_con->set_method_type( iv_method_type = 'PUT' ).

    ro_self = me.
  ENDMETHOD.

  METHOD delete.
    " Set Method Type
    me->mo_http_con->set_method_type( iv_method_type = 'DELETE' ).

    ro_self = me.
  ENDMETHOD.


  METHOD set_method_type.
    " Set Method Type
    me->mo_http_con->set_method_type( iv_method_type = iv_method_type ).

    ro_self = me.
  ENDMETHOD.

  METHOD execute.


    me->mo_http_con->execute(
*      EXPORTING
*        iv_timeout   = 60
*      IMPORTING
*        eo_request   =
*        ev_errortext =
*      RECEIVING
*        ro_response  =
    ).
  ENDMETHOD.

ENDCLASS.

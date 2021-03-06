CLASS zcl_http_multiform_rest_api DEFINITION
  INHERITING FROM zcl_http_rest_api
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add_form_field
      IMPORTING
        !iv_form_name  TYPE string
        !iv_form_value TYPE string.

    METHODS add_file
      IMPORTING
        iv_content_type TYPE string OPTIONAL
        !iv_form_name   TYPE string
        !is_file        TYPE zcl_http_con=>ty_s_file .


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_http_multiform_rest_api IMPLEMENTATION.


  METHOD add_form_field.
    " Add form value
    me->mo_http_con->set_multipart_data(
         iv_form_name =  iv_form_name
         iv_form_value = iv_form_value
     ).

  ENDMETHOD.


  METHOD add_file.
    "Sending File
    me->mo_http_con->set_multipart_data(
        iv_content_type = iv_content_type
        iv_form_name = iv_form_name
        is_file = is_file
    ).

  ENDMETHOD.

ENDCLASS.

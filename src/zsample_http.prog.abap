*&---------------------------------------------------------------------*
*& Report zsample_http
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsample_http.



START-OF-SELECTION.



  DATA(lo_simple_f) = NEW zcl_http_simple_rest_api( 'https://evw.test.com/' ).

  lo_simple_f->authentication(
      iv_username   = ''
      iv_password   = ''
  ).

  lo_simple_f->set_body(
    EXPORTING
*        iv_body         =
*        iv_content_type =
      iv_method_type  = ''
  ).




  DATA(lo_multi_f) = NEW zcl_http_multiform_rest_api( 'https://evw.test.com/' ).

  lo_multi_f->authentication(
      iv_username   = ''
      iv_password   = ''
  ).

  lo_multi_f->add_file(
      iv_form_name    = ''
      is_file         = VALUE #(  )
  ).

  lo_multi_f->add_form_field(
      iv_form_name  = 'username'
      iv_form_value = 'sdfasfdadsf'
  ).

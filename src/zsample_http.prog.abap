*&---------------------------------------------------------------------*
*& Report zsample_http
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsample_http.



START-OF-SELECTION.


  TRY.

      DATA(lo_simple_f) = NEW zcl_http_simple_rest_api( 'https://api.github.com/user' )."

      lo_simple_f->basic_authentication(
          iv_username   = 'nailspahija'
          iv_password   = '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'
      ).

*      lo_simple_f->set_body( iv_method_type  = 'GET' ).

      lo_simple_f->set_method_type( iv_method_type = zif_http_method_type=>gc_get_method ).

      DATA(ls_response) = lo_simple_f->execute( ).

      WRITE: ls_response-response.

      "------------------------------
*
*      DATA(lo_multi_f) = NEW zcl_http_multiform_rest_api( 'https://api.github.com/user' ).
*
*      lo_multi_f->basic_authentication(
*          iv_username   = 'nailspahija'
*          iv_password   = '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'
*      ).
*
*      lo_multi_f->add_file(
*          iv_form_name    = ''
*          is_file         = VALUE #(  )
*      ).
*
*      lo_multi_f->add_file(
*          iv_form_name    = ''
*          is_file         = VALUE #(  )
*      ).
*
*      lo_multi_f->add_form_field(
*          iv_form_name  = 'username'
*          iv_form_value = 'sdfasfdadsf'
*      ).
*
*      lo_multi_f->execute( ).


    CATCH  zcx_rest_exception INTO DATA(lx_error).

      WRITE: 'ERROR: ', lx_error->mv_msg_text.
      BREAK-POINT.
  ENDTRY.









**  CATCH zcx_rest_exception. " Exception Class

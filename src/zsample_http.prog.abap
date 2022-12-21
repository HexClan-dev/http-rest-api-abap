*&---------------------------------------------------------------------*
*& Report zsample_http
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsample_http.



START-OF-SELECTION.


  TRY.

      DATA(lo_simple_f) = NEW zcl_http_simple_rest_api( 'https://api.github.com/user' )."

      lo_simple_f->authentication(
          iv_username   = 'nailspahija'
          iv_password   = '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'
      ).

*      lo_simple_f->authentication(
*        EXPORTING
**          iv_username   =
**          iv_password   =
*          iv_token      = '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'
*          iv_token_type = zcl_http_rest_api=>token_type-token_bearer
*      ).

      lo_simple_f->set_body( iv_method_type  = 'GET' ).

      DATA(ls_response) = lo_simple_f->execute( ).

      WRITE: ls_response-response.

    CATCH  zcx_rest_exception INTO DATA(lx_error).

      WRITE: 'ERROR: ', lx_error->mv_msg_text.
      BREAK-POINT.
  ENDTRY.








*  DATA(lo_multi_f) = NEW zcl_http_multiform_rest_api( 'https://evw.test.com/' ).
*
*  lo_multi_f->authentication(
*      iv_username   = ''
*      iv_password   = ''
*  ).
*
*  lo_multi_f->add_file(
*      iv_form_name    = ''
*      is_file         = VALUE #(  )
*  ).
*
*  lo_multi_f->add_form_field(
*      iv_form_name  = 'username'
*      iv_form_value = 'sdfasfdadsf'
*  ).
*
*  lo_multi_f->execute(
**      iv_timeout = 1000
*  ).
**  CATCH zcx_rest_exception. " Exception Class

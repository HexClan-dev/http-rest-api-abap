*"* use this source file for your ABAP unit test classes



CLASS tst_cl_http_rest_api DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.


  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: setup.
    METHODS: teardown.

    METHODS: test_authentication FOR TESTING.

ENDCLASS.


CLASS tst_cl_http_rest_api IMPLEMENTATION.


  METHOD setup.
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD test_authentication.

    TRY.
        DATA: lo_rest_api TYPE REF TO zcl_http_simple_rest_api.

        lo_rest_api = NEW #( iv_hostname = 'https://www.test.login.com' ).

        lo_rest_api->basic_authentication(
          EXPORTING
            iv_username   = 'nspahija'
            iv_password   = 'test123123'
        ).


*        ->post(
*          EXPORTING
*            iv_body       =  '{}'
*        ).

      CATCH zcx_rest_exception INTO DATA(lo_err).
        cl_abap_unit_assert=>fail( ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

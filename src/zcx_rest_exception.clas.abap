CLASS zcx_rest_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CLASS-METHODS:
      s_raise
        RAISING
          zcx_rest_exception .

    METHODS:
      get_bapireturn
        RETURNING VALUE(rs_return) TYPE bapiret2.

    CLASS-DATA: mv_msg_text TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCX_REST_EXCEPTION IMPLEMENTATION.


  METHOD get_bapireturn.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = if_t100_dyn_msg~msgty
        cl     = if_t100_message~t100key-msgid
        number = if_t100_message~t100key-msgno
        par1   = if_t100_dyn_msg~msgv1
        par2   = if_t100_dyn_msg~msgv2
        par3   = if_t100_dyn_msg~msgv3
        par4   = if_t100_dyn_msg~msgv4
      IMPORTING
        return = rs_return.

  ENDMETHOD.


  METHOD s_raise.

    DATA(lo_exception) = NEW zcx_rest_exception(  ).
    lo_exception->if_t100_message~t100key-msgid = sy-msgid.
    lo_exception->if_t100_message~t100key-msgno = sy-msgno.
    lo_exception->if_t100_dyn_msg~msgty         = sy-msgty.
    lo_exception->if_t100_dyn_msg~msgv1         = sy-msgv1.
    lo_exception->if_t100_dyn_msg~msgv2         = sy-msgv2.
    lo_exception->if_t100_dyn_msg~msgv3         = sy-msgv3.
    lo_exception->if_t100_dyn_msg~msgv4         = sy-msgv4.
    RAISE EXCEPTION lo_exception.


  ENDMETHOD.
ENDCLASS.

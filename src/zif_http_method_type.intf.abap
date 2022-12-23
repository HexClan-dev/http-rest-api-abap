INTERFACE zif_http_method_type
  PUBLIC .

  TYPES: ty_method_type TYPE char10.

  CONSTANTS: gc_get_method    TYPE ty_method_type VALUE 'GET',
             gc_post_method   TYPE ty_method_type VALUE 'POST',
             gc_put_method    TYPE ty_method_type VALUE 'PUT',
             gc_update_method TYPE ty_method_type VALUE 'UPDATE',
             gc_delete_method TYPE ty_method_type VALUE 'DELETE'.

ENDINTERFACE.

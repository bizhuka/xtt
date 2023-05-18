INTERFACE zif_xtt_file
  PUBLIC.

  CONSTANTS: BEGIN OF ms_name,
               tech TYPE i VALUE 0,
               verb TYPE i VALUE 1,
             END OF ms_name.

  METHODS get_name IMPORTING iv_mode        TYPE i DEFAULT ms_name-tech
                   RETURNING VALUE(rv_name) TYPE string.

  METHODS get_content EXPORTING
                        ev_as_string  TYPE string
                        ev_as_xstring TYPE xstring.
ENDINTERFACE.

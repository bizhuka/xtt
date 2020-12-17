*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_11.
  " another templates
  me->example_10( EXPORTING io_xtt   = io_xtt
                            iv_raw   = abap_true
                  IMPORTING ev_break = ev_break ).
ENDMETHOD.

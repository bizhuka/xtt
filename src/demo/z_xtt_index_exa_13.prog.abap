*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_13.
  " another templates
  me->example_02( EXPORTING io_xtt   = io_xtt
                  IMPORTING ev_break = ev_break ).
ENDMETHOD.

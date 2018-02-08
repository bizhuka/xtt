FUNCTION z_show_xtt_screen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_XTT) TYPE REF TO  ZCL_XTT
*"----------------------------------------------------------------------
  lcl_main=>sender = io_xtt.

  " Call
  CALL SCREEN 100.
ENDFUNCTION.

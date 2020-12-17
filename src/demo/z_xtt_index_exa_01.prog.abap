*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_01.
  TYPES:
    " Document structure
    BEGIN OF ts_root,
      title  TYPE char15,
      text   TYPE string,
      int    TYPE i,
      bottom TYPE string, " Any field could be REF TO, STRUCTURE or TABLE
    END OF ts_root.

  " Init document structure
  DATA ls_root TYPE ts_root.
  ls_root-title   = 'Document title'.                       "#EC NOTEXT
  ls_root-text    = 'Just string'.                          "#EC NOTEXT
  ls_root-int     = 3.
  ls_root-bottom  = 'bottom'.                               "#EC NOTEXT

  " Show data structure only
  IF p_stru = abap_true.
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK mo_injection IS NOT INITIAL.
    mo_injection->send_merge( ls_root ).
  ENDIF.

  " R is a marker in the IV_TEMPLATE
  io_xtt->merge( is_block      = ls_root
                 iv_block_name = 'R' ).
ENDMETHOD.

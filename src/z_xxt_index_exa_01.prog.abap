*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_01.
  TYPES:
    " Structure of document
    BEGIN OF ts_root,
      title  TYPE char15,
      string TYPE string,
      int    TYPE i,
      bottom TYPE string, " Could be REF TO
    END OF ts_root.

  DATA:
    lo_file TYPE REF TO zif_xtt_file,
    ls_root TYPE ts_root.

  " No need to fill for empty template
  IF p_temp <> abap_true.
    ls_root-title     = 'Document title'.
    ls_root-string    = 'Just string'.
    ls_root-int       = 3.
    ls_root-bottom    = 'bottom'.
  ENDIF.

  " Show data structure only
  IF p_stru = abap_true.
    check_break_point_id( ).
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--
    RETURN.
  ENDIF.

  " Info about template & the main class itself
  CREATE OBJECT:
   lo_file TYPE zcl_xtt_file_smw0 EXPORTING
     iv_objid = iv_template,

   ro_xtt TYPE (iv_class_name) EXPORTING
    io_file = lo_file.

  " Paste data
  IF p_temp <> abap_true.
    ro_xtt->merge( is_block = ls_root iv_block_name = 'R' ).
  ENDIF.
ENDMETHOD.

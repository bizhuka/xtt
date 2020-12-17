*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_04.
  TYPES:
    " Structure of document
    BEGIN OF ts_root,
      date     TYPE d,
      time     TYPE t,
      bool     TYPE abap_bool,
      int      TYPE i,
      sum      TYPE p LENGTH 13 DECIMALS 2,
      datetime TYPE char14, " date(8) + time(6)
      u        TYPE STANDARD TABLE OF t006a WITH DEFAULT KEY,
      c        TYPE STANDARD TABLE OF t005t WITH DEFAULT KEY,
      w        TYPE STANDARD TABLE OF t005t WITH DEFAULT KEY, " Only for Xml Spreadsheet 2003 (*.xml)
    END OF ts_root.
  DATA ls_root TYPE ts_root.

  " Document structure
  ls_root-date   = sy-datum.
  ls_root-time   = sy-uzeit.
  ls_root-bool   = abap_true.
  ls_root-int    = 5.
  ls_root-sum    = '777.77'.
  " obligatory only for datetime   (;type=datetime)
  CONCATENATE sy-datum sy-uzeit INTO ls_root-datetime.

  " Assign Internal to Language-Dependent Unit
  SELECT msehi msehl INTO CORRESPONDING FIELDS OF TABLE ls_root-u ##too_many_itab_fields
  FROM t006a
  WHERE spras = sy-langu.

  " Country Names
  SELECT land1 landx INTO CORRESPONDING FIELDS OF TABLE ls_root-c ##too_many_itab_fields
  FROM t005t
  WHERE spras = sy-langu.

  " Only for ZCL_XTT_EXCEL_XML
  ls_root-w[] = ls_root-c[].

  " Show data structure only
  IF p_stru = abap_true.
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK mo_injection IS NOT INITIAL.
    mo_injection->send_merge( ls_root ).
  ENDIF.

  " Paste data
  io_xtt->merge( is_block      = ls_root
                 iv_block_name = 'R' ).
ENDMETHOD.

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
    END OF ts_root.

  DATA:
    lo_file TYPE REF TO zif_xtt_file,
    ls_root TYPE ts_root.

  " No need to fill for empty template
  IF p_temp <> abap_true.
    ls_root-date   = sy-datum.
    ls_root-time   = sy-uzeit.
    ls_root-bool   = abap_true.
    ls_root-int    = 5.
    ls_root-sum    = '777.77'.
    " obligatory only for datetime   (;type=datetime)
    CONCATENATE sy-datum sy-uzeit INTO ls_root-datetime.

    " Assign Internal to Language-Dependent Unit
    SELECT msehi msehl INTO CORRESPONDING FIELDS OF TABLE ls_root-u
    FROM t006a
    WHERE spras = sy-langu.
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

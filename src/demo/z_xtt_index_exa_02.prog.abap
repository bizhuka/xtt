*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_02.
  TYPES:
    " Document structure
    BEGIN OF ts_root,
      footer   TYPE string,
      header   TYPE string,
      t        TYPE tt_rand_data, " internal flat table ( In template {R-T} )
      date     TYPE d,            " 8
      time     TYPE t,            " 6
      datetime TYPE char14,       " date(8) + time(6)
    END OF ts_root.

  DATA:
    lo_file TYPE REF TO zif_xtt_file,
    ls_root TYPE ts_root.

  " No need to fill for empty template
  IF p_temp <> abap_true.
    " {R-T} in a temaplte. @see get_random_table description
    cl_main=>get_random_table(
     IMPORTING
       et_table = ls_root-t ).

    " For printing
    ls_root-footer = 'Footer'.
    ls_root-header = 'Header'.

    " Date and time in header and footer
    ls_root-date   = sy-datum.
    ls_root-time   = sy-uzeit.
    " obligatory only for datetime   (;type=datetime)
    CONCATENATE sy-datum sy-uzeit INTO ls_root-datetime.
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

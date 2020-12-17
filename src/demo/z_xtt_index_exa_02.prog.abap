*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_02.
  TYPES:
    " Document structure
    BEGIN OF ts_root,
      title    TYPE string,
      t        TYPE tt_rand_data, " internal flat table ( In template {R-T} )
      date     TYPE d,            " 8
      time     TYPE t,            " 6
      datetime TYPE char14,       " date(8) + time(6)
    END OF ts_root.
  DATA ls_root TYPE ts_root.

  " {R-T} in a temaplte. @see get_random_table description
  get_random_table(
   IMPORTING
     et_table = ls_root-t ).

  " Document structure
  ls_root-title = 'Title'(tit).

  " Date and time in header and footer
  ls_root-date   = sy-datum.
  ls_root-time   = sy-uzeit.
  " obligatory only for datetime   (;type=datetime)
  CONCATENATE sy-datum sy-uzeit INTO ls_root-datetime.

  " Show data structure only
  IF p_stru = abap_true.
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK mo_injection IS NOT INITIAL.
    mo_injection->send_merge( ls_root ).
  ENDIF.

  " Paste data
  io_xtt->merge( is_block      = ls_root " IN DDIC is faster ? -> CORRESPONDING zsxtt_example_02( )
                 iv_block_name = 'R' ).
ENDMETHOD.

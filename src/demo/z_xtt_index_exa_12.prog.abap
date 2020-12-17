*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_12.
  DATA lo_root TYPE REF TO lcl_root_12.

  CREATE OBJECT lo_root.

  " {R-T} in a temaplte. @see get_random_table description
  get_random_table( IMPORTING et_table = lo_root->t ).
  " {R-A}
  FIELD-SYMBOLS <ls_rand_data> TYPE ts_rand_data.
  LOOP AT lo_root->t ASSIGNING <ls_rand_data>.
    DATA lo_attr TYPE REF TO lcl_root_12_attr.
    CREATE OBJECT lo_attr
      EXPORTING
        is_rand_data = <ls_rand_data>.

    APPEND lo_attr TO lo_root->a[].
  ENDLOOP.

  " Document structure
  lo_root->title = 'Title'(tit).

  " Date and time in header and footer
  lo_root->date   = sy-datum.
  lo_root->time   = sy-uzeit.
  " obligatory only for datetime   (;type=datetime)
  CONCATENATE sy-datum sy-uzeit INTO lo_root->datetime.

  " Show data structure only
  IF p_stru = abap_true.
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK mo_injection IS NOT INITIAL.
    mo_injection->send_merge( lo_root ).
  ENDIF.

  " Paste data
  io_xtt->merge( " iv_block_name = 'R' default name
                 lo_root ).
ENDMETHOD.

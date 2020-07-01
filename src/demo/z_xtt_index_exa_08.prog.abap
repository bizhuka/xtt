*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_08.
  TYPES:
    " Document structure
    BEGIN OF ts_root,
      a TYPE REF TO data, " Tree
      t TYPE tt_rand_data, " internal flat table ( In template {R-T} )
    END OF ts_root.

  DATA:
    lo_file TYPE REF TO zif_xtt_file,
    ls_root TYPE ts_root,
*    lr_table TYPE REF TO data,
    ls_item TYPE REF TO ts_rand_data,
    lt_rows TYPE REF TO tt_tree_05,
    ls_row  TYPE REF TO ts_tree_05.

  " Document structure
  " {R-T} in a temaplte. @see get_random_table description
  cl_main=>get_random_table(
   IMPORTING
     et_table = ls_root-t ).
  CREATE DATA lt_rows.
  LOOP AT ls_root-t REFERENCE INTO ls_item.
    APPEND INITIAL LINE TO lt_rows->* REFERENCE INTO ls_row.
    MOVE-CORRESPONDING ls_item->* TO ls_row->*.
  ENDLOOP.

  SET HANDLER on_prepare_tree_05 ACTIVATION abap_true.

  " Old way in ABAP code
  ls_root-a = zcl_xtt_replace_block=>tree_create(
   it_table      = lt_rows
   iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;

  " Show data structure only
  IF p_stru = abap_true.
    check_break_point_id( ).
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK jekyll_add_json( ls_root ) = abap_true.
  ENDIF.

  " Info about template & the main class itself
  CREATE OBJECT:
   lo_file TYPE zcl_xtt_file_smw0 EXPORTING
     iv_objid = iv_template,

   ro_xtt TYPE (iv_class_name) EXPORTING
    io_file = lo_file.

  " Paste data
  ro_xtt->merge( is_block = ls_root iv_block_name = 'R' ).

  " Switch off
  SET HANDLER on_prepare_tree_05 ACTIVATION abap_false.
ENDMETHOD.

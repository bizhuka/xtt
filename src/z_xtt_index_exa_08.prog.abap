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
    lo_file  TYPE REF TO zif_xtt_file,
    ls_root  TYPE ts_root,
    lr_table TYPE REF TO data,
    ls_item  TYPE REF TO ts_rand_data,
    lt_rows  TYPE tt_tree_05,
    ls_row   TYPE REF TO ts_tree_05.

  " No need to fill for empty template
  IF p_temp <> abap_true.
    " {R-T} in a temaplte. @see get_random_table description
    ls_root-t = cl_main=>get_random_table( ).

    LOOP AT ls_root-t REFERENCE INTO ls_item.
      APPEND INITIAL LINE TO lt_rows REFERENCE INTO ls_row.
      MOVE-CORRESPONDING ls_item->* TO ls_row->*.
    ENDLOOP.

    SET HANDLER on_prepare_tree_05. " ACTIVATION abap_true.

    GET REFERENCE OF lt_rows INTO lr_table.
    ls_root-a = zcl_xtt_replace_block=>tree_create(
     it_table      = lr_table       " from 7.5 REF #(lt_rows)
     iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;

    "  Will call later in MERGE
    " SET HANDLER on_prepare_tree_05 ACTIVATION abap_false.
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

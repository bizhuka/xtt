*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_05.
  TYPES:
    " Document structure
    BEGIN OF ts_root,
      title TYPE string,
      t     TYPE  REF TO data, " Tree better use general type than zcl_xtt_replace_block=>ts_tree
    END OF ts_root.

  DATA:
    lo_file  TYPE REF TO zif_xtt_file,
    ls_root  TYPE ts_root,
    lt_items TYPE tt_rand_data,
    ls_item  TYPE REF TO ts_rand_data,
    lt_rows  TYPE tt_tree_05,
    ls_row   TYPE REF TO ts_tree_05,
    lr_table TYPE REF TO DATA.

  " No need to fill for empty template
  IF p_temp <> abap_true.
    ls_root-title = `Title`.

    " @see get_random_table description
    lt_items = cl_main=>get_random_table( ).
    LOOP AT lt_items REFERENCE INTO ls_item.
      APPEND INITIAL LINE TO lt_rows REFERENCE INTO ls_row.
      MOVE-CORRESPONDING ls_item->* TO ls_row->*.
    ENDLOOP.

    SET HANDLER on_prepare_tree_05 ACTIVATION abap_true.

    GET REFERENCE OF lt_rows INTO lr_table.
    ls_root-t = zcl_xtt_replace_block=>tree_create(
     it_table      = lr_table       " from 7.5 REF #(lt_rows)
     iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;

    " No need for future invokes
    SET HANDLER on_prepare_tree_05 ACTIVATION abap_false.
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

METHOD on_prepare_tree_05.
  FIELD-SYMBOLS:
    <ls_data>     TYPE ts_tree_05,
    <lt_sub_data> TYPE ANY TABLE,
    <ls_sub_data> TYPE ts_tree_05.

  CHECK ir_sub_data IS NOT INITIAL AND
        ir_tree->level < 2. " Just for demonstration (both conditions are same)

  " Cast to specefic data
  ASSIGN:
   ir_data->*        TO <ls_data>,
   ir_sub_data->*    TO <lt_sub_data>.

  " Virtual field
  <ls_data>-ch_count = lines( <lt_sub_data> ).
  "<ls_data>-level    = ir_tree->level.

  " And calc sums
  LOOP AT <lt_sub_data> ASSIGNING <ls_sub_data>.
    <ls_data>-sum1  = <ls_data>-sum1 + <ls_sub_data>-sum1.
    <ls_data>-sum2  = <ls_data>-sum2 + <ls_sub_data>-sum2.

    " text description (1 time)
    CHECK <ls_data>-group IS INITIAL.
    <ls_data>-group = <ls_sub_data>-group.
  ENDLOOP.
ENDMETHOD.

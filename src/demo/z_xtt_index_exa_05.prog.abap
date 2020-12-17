*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_05.
  TYPES:
    " Document structure
    BEGIN OF ts_root,
      title TYPE string,

      " If not tree (just table) could be -> TYPE tt_rand_data
      t     TYPE REF TO data, " better to use general type than zcl_xtt_replace_block=>ts_tree
    END OF ts_root.

  DATA:
    ls_root  TYPE ts_root,
    lt_items TYPE REF TO tt_rand_data,
    ls_item  TYPE REF TO ts_rand_data,
    lt_rows  TYPE tt_tree_05,
    ls_row   TYPE REF TO ts_tree_05.

  " Document structure
  ls_root-title  = 'Title'(tit).

  " @see get_random_table description
  CREATE DATA lt_items.
  get_random_table(
   IMPORTING
     et_table = lt_items->* ).
  LOOP AT lt_items->* REFERENCE INTO ls_item.
    APPEND INITIAL LINE TO lt_rows REFERENCE INTO ls_row.
    MOVE-CORRESPONDING ls_item->* TO ls_row->*.
  ENDLOOP.

  " New way use declarations in a template
  ls_root-t = lt_items.

  " Old way in code
*    ls_root-t = zcl_xtt_replace_block=>tree_create(
*     it_table      = REF #( lt_items )
*     iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;

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

METHOD on_prepare_tree_05.
  FIELD-SYMBOLS:
    <ls_data>     TYPE ts_tree_05,
    <lt_sub_data> TYPE tt_tree_05,
    <ls_sub_data> TYPE ts_tree_05.

  CHECK ir_sub_data IS NOT INITIAL AND
        ir_tree->level < 2. " Just for demonstration (both conditions are same)

  " Cast to specefic data
  ASSIGN:
   ir_data->*        TO <ls_data>,
   ir_sub_data->*    TO <lt_sub_data> CASTING.

  " Virtual field
  <ls_data>-ch_count = lines( <lt_sub_data> ).                 " -----> ;func=COUNT
  "<ls_data>-level    = ir_tree->level.

  " And calc sums
  LOOP AT <lt_sub_data> ASSIGNING <ls_sub_data>.
    <ls_data>-sum1  = <ls_data>-sum1 + <ls_sub_data>-sum1.     " -----> ;func=SUM
    <ls_data>-sum2  = <ls_data>-sum2 + <ls_sub_data>-sum2.     " -----> ;func=SUM

    " text description (1 time)
    CHECK <ls_data>-group IS INITIAL.
    <ls_data>-group = <ls_sub_data>-group.
  ENDLOOP.
ENDMETHOD.

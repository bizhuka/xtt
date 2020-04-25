*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_tree_handler IMPLEMENTATION.
  METHOD constructor.
    mo_owner      = io_owner.
    mv_block_name = iv_block_name.
    mt_text_match = it_text_match.

    " If there are dynamic levels
    zcl_xtt_replace_block=>tree_initialize(
     EXPORTING
       ir_tree      = ir_tree
     IMPORTING
       ev_program   = mv_check_prog
     CHANGING
       ct_row_match = mt_text_match ).
  ENDMETHOD.

  METHOD add_tree_data.
    DATA:
      lo_replace_block TYPE REF TO zcl_xtt_replace_block,
      lr_found_match   TYPE REF TO zcl_xtt_xml_base=>ts_text_match,
      lv_top           TYPE abap_bool,
      lv_text_top      TYPE string,
      lv_text_bottom   TYPE string,
      lr_tree_attr     TYPE REF TO zcl_xtt_replace_block=>ts_tree_attr,
      lr_tree          TYPE REF TO zcl_xtt_replace_block=>ts_tree.
    FIELD-SYMBOLS:
      <ls_data> TYPE any,
      <lv_text> TYPE string.
    ASSIGN ir_tree->data->* TO <ls_data>.

    " Create merge description
    CREATE OBJECT lo_replace_block
      EXPORTING
        is_block      = <ls_data>
        iv_block_name = mv_block_name.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN lv_text_top TO <lv_text>.
          lv_top = abap_true.
        WHEN 2.
          ASSIGN lv_text_bottom TO <lv_text>.
          lv_top = abap_false.
        WHEN 3.
          " 3-d try
          CHECK lv_text_top IS INITIAL AND lv_text_bottom IS INITIAL.

          ASSIGN lv_text_top TO <lv_text>.
          lv_top = abap_undefined.
      ENDCASE.

      " Find match
      lr_found_match ?= zcl_xtt_replace_block=>tree_find_match(
         ir_tree        = ir_tree
         iv_block_name  = mv_block_name
         iv_top         = lv_top
         iv_check_prog  = mv_check_prog
         it_row_match   = mt_text_match ).
      CHECK lr_found_match IS NOT INITIAL.

      " Merge with data
      <lv_text> = lr_found_match->text.
      mo_owner->do_merge(
       EXPORTING
        io_replace_block = lo_replace_block
       CHANGING
        cv_content       = <lv_text> ).
    ENDDO.

    " text before
    CONCATENATE cv_text lv_text_top INTO cv_text RESPECTING BLANKS.

    " children texts
    LOOP AT ir_tree->sub_nodes REFERENCE INTO lr_tree_attr.
      lr_tree ?= lr_tree_attr->attr.
      add_tree_data(
       EXPORTING
        ir_tree = lr_tree
       CHANGING
        cv_text = cv_text ).
    ENDLOOP.

    " text after
    CONCATENATE cv_text lv_text_bottom INTO cv_text RESPECTING BLANKS.
  ENDMETHOD.
ENDCLASS.

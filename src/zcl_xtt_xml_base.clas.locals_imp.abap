*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_tree_handler IMPLEMENTATION.
  METHOD constructor.
    mo_owner      = io_owner.
    mv_block_name = iv_block_name.
    mt_text_match = it_text_match.
  ENDMETHOD.

  METHOD add_tree_data.
    DATA:
      lo_replace_block TYPE REF TO zcl_xtt_replace_block,
      ls_text_match    TYPE REF TO zcl_xtt_xml_base=>ts_text_match,
      lv_top           TYPE abap_bool,
      lv_index         TYPE sytabix,
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

      IF lv_top <> abap_undefined.
        READ TABLE mt_text_match REFERENCE INTO ls_text_match
         WITH TABLE KEY level = ir_tree->level top = lv_top.
      ELSE.
        lv_index = lines( mt_text_match ).
        READ TABLE mt_text_match REFERENCE INTO ls_text_match INDEX lv_index.
      ENDIF.
      CHECK sy-subrc = 0.

      " Merge with data
      <lv_text> = ls_text_match->text.
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

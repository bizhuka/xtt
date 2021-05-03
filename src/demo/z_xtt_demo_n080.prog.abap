*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_080 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
  PROTECTED SECTION.
    TYPES:
      " Document structure
      BEGIN OF ts_root,
        a TYPE REF TO data,  " Tree old way
        t TYPE tt_rand_data, " internal flat table ( In template {R-T} )
      END OF ts_root.

    METHODS:
      _merge          REDEFINITION,

      on_prepare_tree_08 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
          ir_tree      " Type Ref To ZCL_XTT_REPLACE_BLOCK=>TS_TREE
          ir_data      " Type Ref To DATA
          ir_sub_data. " Type Ref To DATA

*    TYPES:
*    " Tree structure
*    BEGIN OF ts_row.
*            INCLUDE TYPE ts_rand_data. " random data
*    TYPES:
*      " Fill in callback. Functions in Excel like `;func=COUNT` are preferable
*      ch_count TYPE i, " New field for trees
*    END OF ts_row,
*    tt_row TYPE STANDARD TABLE OF ts_row WITH DEFAULT KEY.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_080 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'direction=column'(080).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/output-direction/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = abap_true.
  ENDMETHOD.

  METHOD set_merge_info.
    DATA ls_root TYPE ts_root.

    " Document structure
    " {R-T} in a temaplte. @see get_random_table description
    io_report->get_random_table( IMPORTING et_table = ls_root-t ).

    " Cannot show TREE in alv
    ls_root-a  = _make_string_message( 'Tree - REF TO DATA (Old way)'(tre) ).

    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD _merge.
    " Make copy
    DATA lt_merge LIKE it_merge.
    lt_merge = it_merge.

    " Change R-C
    FIELD-SYMBOLS <ls_merge> LIKE LINE OF lt_merge.
    READ TABLE lt_merge ASSIGNING <ls_merge>
     WITH TABLE KEY key = 'R'.

    DATA lr_root TYPE REF TO ts_root.
    lr_root ?= <ls_merge>-val.

*    " For CH_COUNT field
*    DATA ls_item TYPE REF TO ts_rand_data.
*    DATA lt_rows TYPE REF TO tt_row.
*    DATA ls_row  TYPE REF TO ts_row.
*    CREATE DATA lt_rows.
*    LOOP AT lr_root->t REFERENCE INTO ls_item.
*      APPEND INITIAL LINE TO lt_rows->* REFERENCE INTO ls_row.
*      MOVE-CORRESPONDING ls_item->* TO ls_row->*.
*    ENDLOOP.

    DATA lr_table TYPE REF TO DATA.
    GET REFERENCE OF lr_root->t INTO lr_table.

    " Old way in code (new way in template)
    lr_root->a = zcl_xtt_replace_block=>tree_create(
      it_table      = lr_table     " lt_rows
      iv_fields     = 'GROUP'   ). " Name of the fields delimited by ;

    " Fill some fields in ON_PREPARE_TREE_08( )
    SET HANDLER on_prepare_tree_08 ACTIVATION abap_true.

    " Pass copy
    super->_merge( io_xtt   = io_xtt
                   it_merge = lt_merge[] ).

    SET HANDLER on_prepare_tree_08 ACTIVATION abap_false.
  ENDMETHOD.


  METHOD on_prepare_tree_08.
    FIELD-SYMBOLS:
      <ls_data>     TYPE ts_rand_data, "ts_row,
      <lt_sub_data> TYPE tt_rand_data, "tt_row,
      <ls_sub_data> TYPE ts_rand_data. "ts_row.

    CHECK ir_sub_data IS NOT INITIAL AND
          ir_tree->level < 2. " Just for demonstration (both conditions are same)

    " Cast to specefic data
    ASSIGN:
     ir_data->*        TO <ls_data>,
     ir_sub_data->*    TO <lt_sub_data> CASTING.

    " Virtual field
    "<ls_data>-ch_count = lines( <lt_sub_data> ).                 " -----> ;func=COUNT
    "<ls_data>-level    = ir_tree->level.

    " And calc sums (old way)
    " ';func=SUM' in template is preferable
    LOOP AT <lt_sub_data> ASSIGNING <ls_sub_data>.
      <ls_data>-sum1  = <ls_data>-sum1 + <ls_sub_data>-sum1.     " -----> ;func=SUM
      <ls_data>-sum2  = <ls_data>-sum2 + <ls_sub_data>-sum2.     " -----> ;func=SUM

      " text description (1 time)
      CHECK <ls_data>-group IS INITIAL.
      <ls_data>-group = <ls_sub_data>-group.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_080-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

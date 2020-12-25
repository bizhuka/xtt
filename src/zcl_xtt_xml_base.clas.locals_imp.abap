*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_tree_handler IMPLEMENTATION.
  METHOD fill_text_match.
    detect_row_offset_01(
       ir_field      = ir_field
       iv_middle     = iv_middle
       iv_beg        = is_bounds-pos_beg
       it_match      = is_bounds-t_match ).

    check_overlaps_02( ir_field ).

    fill_text_match_03(
     iv_tr_id   = iv_tr_id
     it_match   = is_bounds-t_match
     iv_beg     = is_bounds-pos_beg
     iv_middle  = iv_middle ).

    CHECK mt_row_offset IS NOT INITIAL.
    rv_has_text = abap_true.

    DATA lr_tree TYPE REF TO zcl_xtt_replace_block=>ts_tree.
    lr_tree ?= ir_field->dref.
    generate_prog( lr_tree ). " iv_cond_id = ir_field->name ).
  ENDMETHOD.

  METHOD detect_row_offset_01.
    DATA:
      lv_tabix   TYPE sytabix,
      lv_off     TYPE i,
      lv_text    TYPE string,
      ls_row_off TYPE zcl_xtt_tree_function=>ts_row_offset.
    FIELD-SYMBOLS:
      <ls_match>    LIKE LINE OF it_match,
      <ls_submatch> LIKE LINE OF <ls_match>-submatches.

    LOOP AT it_match ASSIGNING <ls_match>.
      lv_tabix = sy-tabix.
      READ TABLE <ls_match>-submatches ASSIGNING <ls_submatch> INDEX 3.
      CHECK sy-subrc = 0
        AND <ls_match>-offset = <ls_submatch>-offset
        AND <ls_match>-length = <ls_submatch>-length.

      " Get whole match
      lv_off  = <ls_match>-offset - iv_beg - 1.
      lv_text = iv_middle+lv_off(<ls_match>-length).

      " Read from texts
      detect_options( EXPORTING iv_text       = lv_text
                                iv_pos        = lv_tabix
                                iv_par_fld    = ir_field->name
                      CHANGING  cs_row_offset = ls_row_off ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_overlaps_02.
    DATA lv_from TYPE sytabix.
    FIELD-SYMBOLS <ls_row_off>  LIKE LINE OF mt_row_offset.
    FIELD-SYMBOLS <ls_row_off2> LIKE LINE OF mt_row_offset.

    LOOP AT mt_row_offset ASSIGNING <ls_row_off>.
      lv_from = sy-tabix + 1.
      LOOP AT mt_row_offset ASSIGNING <ls_row_off2> FROM lv_from WHERE
         ( first <= <ls_row_off>-last AND first >= <ls_row_off>-first ) OR
         ( last  <= <ls_row_off>-last AND last  >= <ls_row_off>-first ).
        MESSAGE e011(zsy_xtt) WITH ir_field->name INTO sy-msgli.
        zcx_eui_no_check=>raise_sys_error( ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_text_match_03.
    DATA:
      lv_minus TYPE i,
      lv_beg   TYPE i,
      lv_end   TYPE i.
    FIELD-SYMBOLS <ls_row_off> LIKE LINE OF mt_row_offset.
    FIELD-SYMBOLS <ls_match>   LIKE LINE OF it_match.

    " Offset
    lv_minus = iv_beg.
    LOOP AT mt_row_offset ASSIGNING <ls_row_off>.
      DATA lr_cache TYPE REF TO ts_match_cache.
      create_tr_cache lr_cache.

      lv_beg = <ls_row_off>-first - 1.
      READ TABLE it_match ASSIGNING <ls_match> INDEX lv_beg.
      lv_beg = <ls_match>-offset - lv_minus.

      lv_end = <ls_row_off>-last + 1.
      READ TABLE it_match ASSIGNING <ls_match> INDEX lv_end.
      lv_end = <ls_match>-offset + <ls_match>-length - lv_beg - lv_minus.

      " And add
      lr_cache->tr_text  = iv_middle+lv_beg(lv_end).
      <ls_row_off>-_data = lr_cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_tree_data.
    DATA lv_text_top      TYPE string.
    DATA lv_text_bottom   TYPE string.
    DATA lr_tree_attr     TYPE REF TO zcl_xtt_replace_block=>ts_tree_attr.
    DATA lr_tree          TYPE REF TO zcl_xtt_replace_block=>ts_tree.

    add_tree_data_own( EXPORTING io_owner       = io_owner
                                 ir_tree        = ir_tree
                                 iv_tabix       = iv_tabix
                       IMPORTING ev_text_top    = lv_text_top
                                 ev_text_bottom = lv_text_bottom ).
    " text before
    CONCATENATE cv_text lv_text_top INTO cv_text RESPECTING BLANKS.

    " Index for hashed table
    DATA lv_tabix TYPE sytabix.
    lv_tabix = 0.

    " children texts
    LOOP AT ir_tree->sub_nodes REFERENCE INTO lr_tree_attr.
      lv_tabix = lv_tabix + 1.
      lr_tree ?= lr_tree_attr->attr.

      add_tree_data(
       EXPORTING
        io_owner = io_owner
        ir_tree  = lr_tree
        iv_tabix = lv_tabix
       CHANGING
        cv_text  = cv_text ).
    ENDLOOP.

    " text after
    CONCATENATE cv_text lv_text_bottom INTO cv_text RESPECTING BLANKS.
  ENDMETHOD.

  METHOD add_tree_data_own.
    DATA lo_replace_block TYPE REF TO zcl_xtt_replace_block.
    DATA lv_top           TYPE abap_bool.

    " result
    CLEAR: ev_text_top,
           ev_text_bottom.

    " Main data
    FIELD-SYMBOLS <ls_data> TYPE any.
    ASSIGN ir_tree->data->* TO <ls_data>.

    " Create merge description
    CREATE OBJECT lo_replace_block
      EXPORTING
        io_xtt        = mo_xtt
        is_block      = <ls_data>
        iv_block_name = mv_block_name.

    FIELD-SYMBOLS <lv_text> TYPE string.
    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN ev_text_top TO <lv_text>.
          lv_top = abap_true.
        WHEN 2.
          ASSIGN ev_text_bottom TO <lv_text>.
          lv_top = abap_false.
        WHEN 3.
          " 3-d try
          CHECK ev_text_top IS INITIAL AND ev_text_bottom IS INITIAL.

          ASSIGN ev_text_top TO <lv_text>.
          lv_top = abap_undefined.
      ENDCASE.

      " Find match
      DATA lr_cache TYPE REF TO ts_match_cache.
      lr_cache ?= find_match( ir_tree = ir_tree
                              iv_top  = lv_top ).
      CHECK lr_cache IS NOT INITIAL.

      " Unique ID
      lo_replace_block->set_id( lr_cache->tr_id ).

      " Merge with data
      <lv_text> = lr_cache->tr_text.
      io_owner->do_merge( EXPORTING io_block   = lo_replace_block
                                    iv_tabix   = iv_tabix
                          CHANGING  cv_content = <lv_text> ).
    ENDDO.
  ENDMETHOD.
ENDCLASS.

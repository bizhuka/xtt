**********************************************************************
**********************************************************************
CLASS lcl_ex_sheet IMPLEMENTATION.
*--------------------------------------------------------------------*
  METHOD constructor.
    " Set owner
    me->mo_xlsx = io_xlsx.

    " Path to the sheet
    int_to_text iv_ind.
    CONCATENATE `xl/worksheets/sheet` iv_ind_txt `.xml` INTO mv_full_path. "#EC NOTEXT

    " Content as an object
    zcl_eui_conv=>xml_from_zip(
     EXPORTING
       io_zip     = io_xlsx->mo_zip
       iv_name    = mv_full_path
     IMPORTING
       eo_xmldoc  = mo_dom ).
***************************************
    " Loop through excels rows
    io_xlsx->row_read_xml( io_sheet          = me
                           it_shared_strings = io_xlsx->mt_shared_strings ).
    " No need already sorted
    "SORT mt_cells BY table_line->c_row table_line->c_col_ind.
***************************************
    " Loop through excels columns
    io_xlsx->column_read_xml( me ).
***************************************
    merged_cells_read( ).
***************************************
    " Data validation read
    io_xlsx->data_validation_read_xml( io_sheet    = me
                                       io_workbook = mo_dom ).
***************************************
    " Defined name contains areas
    mv_name = io_node->get_attribute( `name` ).             "#EC NOTEXT
    defined_names_read( ).
***************************************
    " Find related list objects
    CONCATENATE `xl/worksheets/_rels/sheet` iv_ind_txt `.xml.rels` INTO mv_rel_path. "#EC NOTEXT
    io_xlsx->list_object_read_xml( io_zip   = io_xlsx->mo_zip
                                   io_sheet = me ).
***************************************
    " later in merge write_cells_offset( ).
***************************************
    mr_drawing = zcl_xtt_excel_xlsx=>drawing_read_xml( io_sheet = me
                                                       io_zip   = io_xlsx->mo_zip ).
  ENDMETHOD. "constructor
*--------------------------------------------------------------------*
  METHOD merged_cells_read.
    DATA lo_merge_cell TYPE REF TO if_ixml_element.
    lo_merge_cell ?= mo_dom->find_from_name( 'mergeCell' ). "#EC NOTEXT
    WHILE lo_merge_cell IS BOUND.
      " Create new area
      DATA ls_area TYPE REF TO ts_ex_area.
      DATA l_val   TYPE string.

      l_val = lo_merge_cell->get_attribute( 'ref' ).        "#EC NOTEXT
      CREATE DATA ls_area.
      zcl_xtt_excel_xlsx=>area_read_xml(
         iv_value = l_val
         is_area  = ls_area ).

      " Always 2 cells
      DATA ls_cell_beg TYPE REF TO ts_ex_cell.
      DATA ls_cell_end TYPE REF TO ts_ex_cell.
      READ TABLE ls_area->a_cells REFERENCE INTO ls_cell_beg INDEX 1.
      READ TABLE ls_area->a_cells REFERENCE INTO ls_cell_end INDEX 2.

      " Get ref to existing or ls_cell_beg cell
      DATA ls_cell TYPE REF TO ts_ex_cell.
      ls_cell = find_cell( ls_cell_beg->* ).

      " Fill dx
      ls_cell->c_merge_row_dx = ls_cell_end->c_row     - ls_cell_beg->c_row.
      ls_cell->c_merge_col_dx = ls_cell_end->c_col_ind - ls_cell_beg->c_col_ind.

      " Next
      lo_merge_cell ?= lo_merge_cell->get_next( ).
    ENDWHILE.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD defined_names_read.
    DATA ls_defined_name TYPE REF TO ts_ex_defined_name.
    DATA ls_area         TYPE REF TO ts_ex_area.
    DATA ls_cell         TYPE REF TO ts_ex_cell.

    LOOP AT mo_xlsx->mt_defined_names REFERENCE INTO ls_defined_name.
      LOOP AT ls_defined_name->d_areas REFERENCE INTO ls_area WHERE a_sheet_name = mv_name.
        " Reference to reference
        LOOP AT ls_area->a_cells REFERENCE INTO ls_cell.
          " Get existing cell Or insert new one
          find_cell(
           ir_cell     = ls_cell->*
           iv_def_name = ls_defined_name->d_name ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD write_cells_offset.
    " 1 time only
    CHECK mv_has_offset <> abap_true.
    mv_has_offset = abap_true.

    DATA ls_cell      TYPE REF TO ts_ex_cell.
    DATA l_prev_row   TYPE i VALUE 0.
    DATA l_prev_col   TYPE i VALUE 0.

    LOOP AT mt_cells REFERENCE INTO ls_cell.
      " If new row ls_cell->c_row_dx > 0
      ls_cell->c_row_dx = ls_cell->c_row     - l_prev_row.

      " If new row then column also from 0
      IF ls_cell->c_row_dx <> 0.
        l_prev_col = 0.
      ENDIF.
      ls_cell->c_col_dx = ls_cell->c_col_ind - l_prev_col.

      l_prev_row = ls_cell->c_row.
      l_prev_col = ls_cell->c_col_ind.
    ENDLOOP.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD find_cell.
    DATA ls_row TYPE ts_ex_row.

    READ TABLE mt_cells BINARY SEARCH REFERENCE INTO rr_ex_cell WITH KEY " with table key
      c_row = ir_cell-c_row c_col_ind = ir_cell-c_col_ind.
    IF sy-subrc <> 0 AND iv_add = abap_true.
*      ev_new_tabix = sy-tabix.
      INSERT ir_cell INTO mt_cells INDEX sy-tabix REFERENCE INTO rr_ex_cell.

      READ TABLE mt_rows TRANSPORTING NO FIELDS
       WITH TABLE KEY r = ir_cell-c_row.
      IF sy-subrc <> 0.
        ls_row-r = sy-tabix.
        INSERT ls_row INTO TABLE mt_rows.
      ENDIF.
    ENDIF.

    CHECK rr_ex_cell IS NOT INITIAL.
    rr_ex_cell->c_def_name = iv_def_name.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD fill_shared_strings.
    DATA ls_cell TYPE REF TO ts_ex_cell.

    " Add one by one
    LOOP AT mt_cells REFERENCE INTO ls_cell. "WHERE c_value IS NOT INITIAL.
      CHECK zcl_xtt_excel_xlsx=>cell_is_string( ls_cell ) = abap_true.
      APPEND ls_cell->c_value TO ct_shared_strings.
    ENDLOOP.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD save.
    " Late initilization (without merge)
    write_cells_offset( ).
    mv_has_offset = abap_false.

    " Blank sheet
    CHECK mt_cells IS NOT INITIAL.
***************************************
    " Find old -> new match
    DATA lt_cell_ref TYPE tt_cell_ref.
    lt_cell_ref = cells_create_refs( ).
***************************************
    DATA ls_transmit TYPE ts_transmit.
    ls_transmit = _cells_write_xml( ).
***************************************
    " Data validation (Use dom)
    mo_xlsx->data_validation_save_xml( io_workbook = mo_dom
                                       io_sheet    = me
                                       it_cell_ref = lt_cell_ref ).
***************************************
    " Replace existing text. Work with dom
    _replace_by_transmit( ls_transmit ).
***************************************
    defined_name_save( lt_cell_ref ).
***************************************
    " List object
    mo_xlsx->list_object_save_xml( io_zip      = mo_xlsx->mo_zip
                                   io_sheet    = me
                                   it_cell_ref = lt_cell_ref ).
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD cells_create_refs.
    DATA ls_cell TYPE REF TO ts_ex_cell.
    DATA ls_cell_ref TYPE ts_cell_ref.
    DATA lr_cell_ref TYPE REF TO ts_cell_ref.

    LOOP AT mt_cells REFERENCE INTO ls_cell.
      READ TABLE rt_cell_ref REFERENCE INTO lr_cell_ref
       WITH TABLE KEY r = ls_cell->c_row
                      c = ls_cell->c_col_ind.

      IF sy-subrc <> 0.
        ls_cell_ref-r   = ls_cell->c_row.
        ls_cell_ref-c   = ls_cell->c_col_ind.
        ls_cell_ref-beg = ls_cell.
        INSERT ls_cell_ref INTO TABLE rt_cell_ref REFERENCE INTO lr_cell_ref.
      ENDIF.

      lr_cell_ref->end = ls_cell.
      APPEND ls_cell TO lr_cell_ref->all.
    ENDLOOP.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD _cells_write_xml.
    DATA l_new_row_ind  TYPE i VALUE 0.
    DATA l_new_col_ind  TYPE i VALUE 0.
    DATA lt_columns     LIKE mt_columns.

    " Write cells data one by one
    DATA ls_cell TYPE REF TO ts_ex_cell.
    LOOP AT mt_cells REFERENCE INTO ls_cell.
      mo_xlsx->cell_write_new_row( EXPORTING is_cell        = ls_cell
                                             io_sheet       = me
                                   CHANGING  cv_sheet_data  = rs_transmit-new_txt_rows
                                             cv_current_row = l_new_row_ind
                                             cv_current_col = l_new_col_ind ).

      mo_xlsx->cell_write_new_col( EXPORTING is_cell        = ls_cell
                                             io_sheet       = me
                                   CHANGING  ct_columns     = lt_columns
                                             cv_current_col = l_new_col_ind ).
      " Complex saving
      mo_xlsx->drawing_add( ir_me      = mr_drawing
                            ir_cell    = ls_cell
                            io_zip     = mo_xlsx->mo_zip
                            iv_new_row = l_new_row_ind
                            iv_new_col = l_new_col_ind ).
      " Append cell info
      mo_xlsx->cell_write_xml(
       EXPORTING
        is_cell           = ls_cell
        iv_new_row        = l_new_row_ind
        iv_new_col_ind    = l_new_col_ind
        it_shared_strings	= mo_xlsx->mt_shared_strings
       CHANGING
        cs_transmit       = rs_transmit ).
    ENDLOOP.

    " Rows & cells Closing tag
    CONCATENATE rs_transmit-new_txt_rows `</row>` INTO rs_transmit-new_txt_rows.

    " Columns
    rs_transmit-new_txt_cols = mo_xlsx->column_write_xml( lt_columns ).
    FIELD-SYMBOLS <ls_column> LIKE LINE OF lt_columns.
    LOOP AT lt_columns ASSIGNING <ls_column>.
      DELETE mt_columns WHERE min = <ls_column>-min.
      INSERT <ls_column> INTO TABLE mt_columns.
    ENDLOOP.

    " Merged cells
    IF rs_transmit-_merge_cnt IS NOT INITIAL.
      int_2_text rs_transmit-_merge_cnt rs_transmit-merge_cnt_txt.
    ENDIF.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD _replace_by_transmit.
    DATA lo_mc TYPE REF TO if_ixml_element.

    xml_replace_node(         iv_tag_name  = 'sheetData'    "#EC NOTEXT
                              iv_repl_text = '_SHEET_DATA_' ).
    xml_replace_node(         iv_tag_name  = 'cols'         "#EC NOTEXT
                              iv_repl_text = '_NEW_COLUMNS_' ).
    lo_mc = xml_replace_node( iv_tag_name  = 'mergeCells'   "#EC NOTEXT
                              iv_repl_text = '_MERGE_CELLS_' ).

    " Change count
    IF lo_mc IS NOT INITIAL AND is_transmit-merge_cnt_txt IS NOT INITIAL.
      lo_mc->set_attribute( name = 'count' value = is_transmit-merge_cnt_txt ). "#EC NOTEXT
    ENDIF.

    " Transform to string
    DATA l_dom_str TYPE string.
    zcl_eui_conv=>xml_to_str( EXPORTING io_doc = mo_dom
                              IMPORTING ev_str = l_dom_str ).
    " Do replcement
    REPLACE FIRST OCCURRENCE OF '_SHEET_DATA_'  IN l_dom_str WITH is_transmit-new_txt_rows. "#EC NOTEXT
    " New columns
    REPLACE FIRST OCCURRENCE OF '_NEW_COLUMNS_' IN l_dom_str WITH is_transmit-new_txt_cols. "#EC NOTEXT

    IF lo_mc IS NOT INITIAL AND is_transmit-merge_cnt_txt IS NOT INITIAL.
      REPLACE FIRST OCCURRENCE OF '_MERGE_CELLS_' IN l_dom_str WITH is_transmit-merge_cells. "#EC NOTEXT
    ENDIF.

***************************************
    " Add ref to images
    mo_xlsx->drawing_save_xml( EXPORTING ir_me        = mr_drawing
                                         io_sheet     = me
                                         io_zip       = mo_xlsx->mo_zip
                               CHANGING  cv_sheet_xml = l_dom_str ).
***************************************
    " Replace XML file
    zcl_eui_conv=>xml_to_zip( io_zip  = mo_xlsx->mo_zip
                              iv_name = mv_full_path
                              iv_sdoc = l_dom_str ).
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD defined_name_save.
    DATA lt_defined_name TYPE tt_ex_defined_name.
    DATA ls_defined_name TYPE REF TO ts_ex_defined_name.
    DATA lv_delete_name  TYPE abap_bool.
    DATA lv_tabix        TYPE sytabix.
    DATA ls_area         TYPE REF TO ts_ex_area.

    LOOP AT mo_xlsx->mt_defined_names REFERENCE INTO ls_defined_name.
      " Save postion
      lv_tabix = sy-tabix.
      CLEAR lv_delete_name.

      LOOP AT ls_defined_name->d_areas REFERENCE INTO ls_area WHERE a_sheet_name = mv_name.
        replace_with_new(
         EXPORTING
           ir_area         = ls_area
           it_cell_ref     = it_cell_ref
           is_defined_name = ls_defined_name->*
         IMPORTING
           ev_delete_name  = lv_delete_name
         CHANGING
           ct_defined_name = lt_defined_name ).
      ENDLOOP.

      " Delete old range name
      CHECK lv_delete_name = abap_true.
      DELETE mo_xlsx->mt_defined_names INDEX lv_tabix.
    ENDLOOP.
    " Add new names
    INSERT LINES OF lt_defined_name INTO TABLE mo_xlsx->mt_defined_names.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD replace_with_new.
    DATA:
      lr_cell         TYPE REF TO ts_ex_cell,
      lr_cell_ref     TYPE REF TO ts_cell_ref,
      lv_tabix        TYPE sytabix,
      lv_cell_cnt     TYPE i,
      lv_area_cnt     TYPE i,
      ls_defined_name LIKE is_defined_name,
      ls_area         TYPE ts_ex_area,
      ls_ex_cell      TYPE REF TO ts_ex_cell.

    lv_cell_cnt = lines( ir_area->a_cells ).
    lv_area_cnt = lines( is_defined_name-d_areas ).
    CLEAR ev_delete_name.

    LOOP AT ir_area->a_cells REFERENCE INTO lr_cell.
      lv_tabix = sy-tabix.

      " OLD -> NEW
      READ TABLE it_cell_ref REFERENCE INTO lr_cell_ref
       WITH TABLE KEY r = lr_cell->c_row
                      c = lr_cell->c_col_ind.
      CHECK sy-subrc = 0.

      " Add new names
      IF    is_defined_name-d_name CP lcl_ex_sheet=>mc_dyn_def_name
        AND lv_cell_cnt = 1
        AND lv_area_cnt = 1
        AND ct_defined_name IS REQUESTED.

        LOOP AT lr_cell_ref->all INTO ls_ex_cell.
          " Define name
          CLEAR ls_defined_name.
          ls_defined_name-d_name = ls_ex_cell->c_def_name.

          " Add cell
          ls_area = ir_area->*.
          CLEAR ls_area-a_cells.
          INSERT ls_ex_cell->* INTO TABLE ls_area-a_cells.
          INSERT ls_area INTO TABLE ls_defined_name-d_areas.

          " Add new range name
          INSERT ls_defined_name INTO TABLE ct_defined_name.
        ENDLOOP.

        ev_delete_name = abap_true.
        RETURN.
      ENDIF.

      CASE lv_tabix.
          " First occurrence
        WHEN 1.
          lr_cell->* = lr_cell_ref->beg->*.

          " Last occurrence
        WHEN 2.
          lr_cell->* = lr_cell_ref->end->*.
      ENDCASE.
    ENDLOOP.

    " Add additional cell to the end
    CHECK lv_tabix = 1 AND lr_cell_ref IS NOT INITIAL.
    APPEND lr_cell_ref->end->* TO ir_area->a_cells.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD merge.
    DATA lo_scope TYPE REF TO zcl_xtt_scope.
    read_scopes( EXPORTING io_block = io_block
                           iv_tabix = iv_tabix
                           iv_force = iv_force
                 IMPORTING eo_scope = lo_scope
                 CHANGING  ct_cells = ct_cells ).
    write_cells_offset( ).

    " Already found scopes
    FIELD-SYMBOLS <ls_scope> LIKE LINE OF lo_scope->mt_scope.
    LOOP AT lo_scope->mt_scope ASSIGNING <ls_scope>.
      " Current -> ms_cell
      READ TABLE ct_cells REFERENCE INTO ms_cell INDEX <ls_scope>-index.

      IF sy-subrc <> 0.
        MESSAGE e019(zsy_xtt) WITH <ls_scope>-field INTO sy-msgli.
        mo_xlsx->add_log_message( iv_syst = abap_true ).
        CONTINUE.
      ENDIF.

      " @see match_found
      io_block->find_match( EXPORTING io_xtt     = mo_xlsx
                                      is_scope   = <ls_scope>
                            CHANGING  cv_content = ms_cell->c_value ).
    ENDLOOP.

    DATA lr_field TYPE REF TO zcl_xtt_replace_block=>ts_field.
    LOOP AT io_block->mt_fields REFERENCE INTO lr_field.
      CASE lr_field->typ.
          " merge-2 Structures and objects
        WHEN zcl_xtt_replace_block=>mc_type-struct OR zcl_xtt_replace_block=>mc_type-object.
          merge_sub_structures( EXPORTING ir_field = lr_field
                                CHANGING  ct_cells = ct_cells ).
          " merge-3,4 Array types and trees
        WHEN zcl_xtt_replace_block=>mc_type-table OR zcl_xtt_replace_block=>mc_type-tree.
          merge_tables_trees( EXPORTING ir_field = lr_field
                                        io_scope = lo_scope
                              CHANGING  ct_cells = ct_cells ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD read_scopes.
    " Position holder
    DATA lv_new   TYPE abap_bool.
    zcl_xtt_scope=>get_instance( EXPORTING io_block    = io_block
                                           iv_force    = iv_force
                                 IMPORTING eo_instance = eo_scope
                                           ev_new      = lv_new ).
    IF lv_new = abap_true.
      DATA lr_cell TYPE REF TO ts_ex_cell.
      LOOP AT ct_cells REFERENCE INTO lr_cell WHERE c_formula IS INITIAL.
        DATA lv_tabix TYPE sytabix.
        lv_tabix = sy-tabix.

        DATA lv_inline_tree TYPE abap_bool.
        eo_scope->get_scopes( EXPORTING iv_index       = lv_tabix
                                        io_xtt         = mo_xlsx
                              IMPORTING ev_inline_tree = lv_inline_tree
                              CHANGING  cv_content     = lr_cell->c_value ).
        " No need the cell value
        CHECK lv_inline_tree = abap_true.
        CLEAR lr_cell->c_value.

        " Add blank cell
        CHECK lr_cell->c_col_ind > 1.

        DATA ls_blank TYPE ts_ex_cell.
        ls_blank-c_row     = lr_cell->c_row.
        ls_blank-c_col_ind = lr_cell->c_col_ind - 1.
        find_cell( ls_blank ).

        " Calc dx again
        mv_has_offset = abap_false.
      ENDLOOP.
    ENDIF.

    eo_scope->calc_cond_matches( io_xtt   = mo_xlsx
                                 io_block = io_block
                                 iv_tabix = iv_tabix
                                 iv_init  = lv_new ).
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD merge_sub_structures.
    " No data ?
    IF ir_field->typ = zcl_xtt_replace_block=>mc_type-struct.
      CHECK ir_field->dref IS NOT INITIAL.
    ENDIF.
    IF ir_field->typ = zcl_xtt_replace_block=>mc_type-object.
      CHECK ir_field->oref IS NOT INITIAL.
    ENDIF.

    " Based on nested structure
    DATA lo_new_replace_block TYPE REF TO zcl_xtt_replace_block.
    CREATE OBJECT lo_new_replace_block
      EXPORTING
        is_field = ir_field.

    " Recursion
    me->merge( EXPORTING io_block = lo_new_replace_block
               CHANGING  ct_cells = ct_cells ).
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD merge_tables_trees.
    DATA:
      lv_by_column    TYPE abap_bool,
      lt_cells_end    LIKE ct_cells,
      lt_cells_mid    LIKE ct_cells,
      lo_tree_handler TYPE REF TO lcl_tree_handler.

    " Tree or Table Find for replication
    split_2_content( EXPORTING ir_field        = ir_field
                               io_scope        = io_scope
                     IMPORTING ev_by_column    = lv_by_column
                               et_cells_mid    = lt_cells_mid
                               et_cells_end    = lt_cells_end
                               eo_tree_handler = lo_tree_handler
                     CHANGING  ct_cells        = ct_cells ).

**********************************************************************
    CASE ir_field->typ.
      WHEN zcl_xtt_replace_block=>mc_type-tree.
        lo_tree_handler->init( EXPORTING io_owner = me
                                         ir_field = ir_field
                               CHANGING  ct_cells = ct_cells ).

      WHEN zcl_xtt_replace_block=>mc_type-table.
        merge_table_ext( EXPORTING ir_field      = ir_field
                                   it_cells_mid  = lt_cells_mid
                         CHANGING  ct_cells      = ct_cells ).
    ENDCASE.
**********************************************************************

    " Rest of the cells
    APPEND LINES OF lt_cells_end TO ct_cells.

    " Set the order back
    IF lv_by_column = abap_true.
      SORT ct_cells STABLE BY c_row.
    ENDIF.

    " Recalc
    CHECK lv_by_column = abap_true.
    save( ). " _cells_write_xml( ).
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD merge_table_ext.
    CHECK it_cells_mid IS NOT INITIAL.

    " Replicate middle
    FIELD-SYMBOLS <lt_items> TYPE ANY TABLE.
    ASSIGN ir_field->dref->* TO <lt_items>.

    " Use copy
    DATA lr_field2 TYPE REF TO zcl_xtt_replace_block=>ts_field.
    CREATE DATA lr_field2.
    lr_field2->* = ir_field->*.

    DATA lt_copy LIKE ct_cells.
    LOOP AT <lt_items> REFERENCE INTO lr_field2->dref.
      DATA lv_tabix TYPE sytabix.
      lv_tabix = sy-tabix.

      lt_copy[] = it_cells_mid[].

      DATA lv_first TYPE abap_bool.
      IF lv_tabix = 1.
        lv_first = abap_true.
      ELSE.
        lv_first = abap_false.
      ENDIF.

      " Create merge description, Create copy first
      DATA lo_new_replace_block TYPE REF TO zcl_xtt_replace_block.
      IF lo_new_replace_block IS INITIAL OR lo_new_replace_block->reuse_check( lr_field2 ) <> abap_true.
        CREATE OBJECT lo_new_replace_block
          EXPORTING
            is_field = lr_field2.
      ENDIF.

      " Recursion
      me->merge( EXPORTING io_block = lo_new_replace_block
                           iv_tabix = lv_tabix
                           iv_force = lv_first
                 CHANGING  ct_cells = lt_copy ).
      APPEND LINES OF lt_copy TO ct_cells.
    ENDLOOP.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD xml_replace_node.
    DATA lo_node  TYPE REF TO if_ixml_node.
    DATA lo_elem  TYPE REF TO if_ixml_element.

    ro_elem = mo_dom->find_from_name( iv_tag_name ).
    CHECK ro_elem IS BOUND.

    " Delete child elements
    DO.
      TRY.
          lo_node = ro_elem->get_last_child( ).  " replace_child( ) instead ?
          lo_elem ?= lo_node.
        CATCH cx_sy_move_cast_error.
          EXIT.
      ENDTRY.
      IF lo_elem IS INITIAL.
        EXIT.
      ENDIF.

      ro_elem->remove_child( lo_elem ).
      CLEAR lo_elem.
    ENDDO.

    " Replace with text
    ro_elem->set_value( iv_repl_text ).
  ENDMETHOD.                    "xml_replace_node
*--------------------------------------------------------------------*
  METHOD split_2_content.
    DATA:
      lv_find_str  TYPE string,
      ls_cell      TYPE REF TO ts_ex_cell,
      lt_row_begs  TYPE tt_pair,
      lt_row_ends  TYPE tt_pair,
      lv_row_first TYPE i,
      lv_row_last  TYPE i,
      ls_cur_pair  TYPE ts_pair,
      ls_pair_ref  TYPE REF TO ts_pair,
      ls_row_off   TYPE zcl_xtt_tree_function=>ts_row_offset.

    " All positions
    CLEAR: et_cells_end,
           et_cells_mid,
           eo_tree_handler.

    ev_by_column = io_scope->is_by_column( ir_field->name ).
    IF ev_by_column = abap_true.
      SORT ct_cells STABLE BY c_col_ind c_row.
    ENDIF.

    " CONCATENATE zcl_xtt_replace_block=>mc_char_block_begin iv_fld_name INTO lv_find_str.
    CONCATENATE `\{` ir_field->name `\b[^}]*\}` INTO lv_find_str.

    IF ir_field->typ = zcl_xtt_replace_block=>mc_type-tree.
      CREATE OBJECT eo_tree_handler
        EXPORTING
          io_xtt        = mo_xlsx
          iv_block_name = ir_field->name.
    ENDIF.

    " Find matches
    LOOP AT ct_cells REFERENCE INTO ls_cell.
      " Current values
      ls_cur_pair-array_ind  = sy-tabix.

      " What field to use
      IF ev_by_column = abap_true.
        ls_cur_pair-position  = ls_cell->c_col_ind. " ls_cur_pair-position + ls_cell->c_col_dx.
      ELSE.
        ls_cur_pair-position  = ls_cell->c_row.  " ls_cur_pair-position + ls_cell->c_row_dx.
      ENDIF.

      " Find row range
      READ TABLE lt_row_begs WITH TABLE KEY
       position = ls_cur_pair-position TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT ls_cur_pair INTO TABLE lt_row_begs.
      ENDIF.

      " Make range bigger
      READ TABLE lt_row_ends REFERENCE INTO ls_pair_ref WITH TABLE KEY
       position = ls_cur_pair-position.
      IF sy-subrc = 0.
        ls_pair_ref->array_ind = ls_cur_pair-array_ind.
      ELSE.
        INSERT ls_cur_pair INTO TABLE lt_row_ends.
      ENDIF.

      " Rows range
      DATA lv_offset    TYPE i.
      DATA lv_length    TYPE i.
      FIND FIRST OCCURRENCE OF REGEX lv_find_str IN ls_cell->c_value
       MATCH OFFSET lv_offset
       MATCH LENGTH lv_length.
      CHECK sy-subrc = 0.

      lv_row_last = ls_cur_pair-position.
      " Set 1 time only
      IF lv_row_first IS INITIAL.
        lv_row_first = lv_row_last.
      ENDIF.

***************************
      " TREE begin or is_field->typ = zcl_xtt_replace_block=>mc_type-tree.
      CHECK eo_tree_handler IS NOT INITIAL.

      " Delete surrounding {}
      lv_offset = lv_offset + 1.
      lv_length = lv_length - 2.

      DATA lv_text TYPE string.
      lv_text = ls_cell->c_value+lv_offset(lv_length).

      " Read from texts
      eo_tree_handler->detect_options( EXPORTING iv_text       = lv_text
                                                 iv_pos        = ls_cur_pair-position
                                                 iv_par_fld    = ir_field->name
                                       CHANGING  cs_row_offset = ls_row_off ).
      " TREE end
***************************
    ENDLOOP.

    CHECK is_row_off_ok( is_field     = ir_field
                         iv_row_first = lv_row_first
                         iv_row_last  = lv_row_last ) = abap_true.

    IF eo_tree_handler IS NOT INITIAL.
      eo_tree_handler->fill_cell_match(
       iv_tr_id      = ir_field->name
       it_row_begs   = lt_row_begs
       it_row_ends   = lt_row_ends
       it_cells      = ct_cells ).
    ENDIF.

    move_cell_content( EXPORTING it_row_begs  = lt_row_begs
                                 it_row_ends  = lt_row_ends
                                 iv_row_first = lv_row_first
                                 iv_row_last  = lv_row_last
                       CHANGING  ct_cells     = ct_cells
                                 ct_cells_mid = et_cells_mid
                                 ct_cells_end = et_cells_end ).
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD is_row_off_ok.
    " Skip
    IF iv_row_first IS INITIAL AND iv_row_last IS INITIAL.
      RETURN.
    ENDIF.

    " Oops not found
    IF iv_row_first IS INITIAL OR iv_row_last IS INITIAL.
      MESSAGE e009(zsy_xtt) WITH is_field->name INTO sy-msgli.
      zcx_eui_no_check=>raise_sys_error( ).
    ENDIF.

    rv_ok = abap_true.
  ENDMETHOD.
  METHOD move_cell_content.
    DATA ls_pair_ref  TYPE REF TO ts_pair.
    DATA lv_ind_beg   TYPE i.
    DATA lv_ind_end   TYPE i.
    DATA lv_ind_end_1 TYPE i.

    " Detect middle
    READ TABLE it_row_begs REFERENCE INTO ls_pair_ref
     WITH TABLE KEY position = iv_row_first.
    lv_ind_beg = ls_pair_ref->array_ind.

    READ TABLE it_row_ends REFERENCE INTO ls_pair_ref
     WITH TABLE KEY position = iv_row_last.
    lv_ind_end = ls_pair_ref->array_ind.

    " End
    lv_ind_end_1 = lv_ind_end + 1.
    APPEND LINES OF ct_cells FROM lv_ind_end_1 TO ct_cells_end.
    " Middle
    APPEND LINES OF ct_cells FROM lv_ind_beg TO lv_ind_end TO ct_cells_mid.
    " Begin!
    DELETE ct_cells FROM lv_ind_beg.
  ENDMETHOD.
*--------------------------------------------------------------------*
ENDCLASS.                    "lcl_ex_sheet IMPLEMENTATION

**********************************************************************
**********************************************************************

CLASS lcl_tree_handler IMPLEMENTATION.
  METHOD fill_cell_match.
    " TREE begin Check overlaps
    FIELD-SYMBOLS <ls_row_off>  TYPE zcl_xtt_tree_function=>ts_row_offset.
    FIELD-SYMBOLS <ls_row_off2> TYPE zcl_xtt_tree_function=>ts_row_offset.
    DATA          lv_from       TYPE sytabix.

    LOOP AT mt_row_offset ASSIGNING <ls_row_off>.
      lv_from = sy-tabix + 1.
      LOOP AT mt_row_offset ASSIGNING <ls_row_off2> FROM lv_from WHERE
         ( first <= <ls_row_off>-last AND first >= <ls_row_off>-first ) OR
         ( last  <= <ls_row_off>-last AND last  >= <ls_row_off>-first ).
        MESSAGE e011(zsy_xtt) WITH me->mv_block_name INTO sy-msgli.
        zcx_eui_no_check=>raise_sys_error( ).
      ENDLOOP.
    ENDLOOP.

    LOOP AT mt_row_offset ASSIGNING <ls_row_off>.
      DATA lr_cache TYPE REF TO ts_match_cache.
      create_tr_cache lr_cache.

      DATA ls_pair_beg TYPE REF TO ts_pair.
      READ TABLE it_row_begs REFERENCE INTO ls_pair_beg
       WITH TABLE KEY position = <ls_row_off>-first.

      DATA ls_pair_end TYPE REF TO ts_pair.
      READ TABLE it_row_ends REFERENCE INTO ls_pair_end
       WITH TABLE KEY position = <ls_row_off>-last.

      " And add
      APPEND LINES OF it_cells FROM ls_pair_beg->array_ind TO ls_pair_end->array_ind TO lr_cache->tr_cells.
      <ls_row_off>-_data = lr_cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD init.
    mo_owner = io_owner.

    DATA lr_tree TYPE REF TO zcl_xtt_replace_block=>ts_tree.
    lr_tree ?= ir_field->dref.

    " If there are dynamic levels
    generate_prog( lr_tree ). " iv_cond_id = ir_field->name ).

    add_tree_data( EXPORTING ir_tree  = lr_tree
                             iv_tabix = 0
                   CHANGING  ct_cells = ct_cells ).
  ENDMETHOD.

  METHOD add_tree_data.
    DATA lt_row_top       TYPE tt_ex_cell.
    DATA lt_row_bottom    TYPE tt_ex_cell.
    DATA lr_tree_attr     TYPE REF TO zcl_xtt_replace_block=>ts_tree_attr.
    DATA lr_tree          TYPE REF TO zcl_xtt_replace_block=>ts_tree.

    add_tree_data_own( EXPORTING ir_tree         = ir_tree
                                 iv_tabix        = iv_tabix
                       IMPORTING et_row_top      = lt_row_top
                                 et_row_bottom   = lt_row_bottom
                       CHANGING  ct_dyn_def_name = ct_dyn_def_name ).

    " row before
    DATA lt_cells_ref TYPE tt_ex_cell_ref.
    append_to( EXPORTING it_cells     = lt_row_top
               CHANGING  ct_cells     = ct_cells
                         ct_cells_ref = lt_cells_ref ).

    " children rows
    DATA lt_dyn_def_name  LIKE ct_dyn_def_name.

    " Index for hashed table
    DATA lv_tabix TYPE sytabix.
    lv_tabix = 0.
    LOOP AT ir_tree->sub_nodes REFERENCE INTO lr_tree_attr.
      lv_tabix = lv_tabix + 1.
      lr_tree ?= lr_tree_attr->attr.

      add_tree_data(
       EXPORTING
        ir_tree         = lr_tree
        iv_tabix        = lv_tabix
       CHANGING
        ct_cells        = ct_cells
        ct_dyn_def_name = lt_dyn_def_name ).
    ENDLOOP.

    " row after
    append_to( EXPORTING it_cells     = lt_row_bottom
               CHANGING  ct_cells     = ct_cells
                         ct_cells_ref = lt_cells_ref ).

    update_cells_ref( EXPORTING it_dyn_def_name = lt_dyn_def_name
                      CHANGING  ct_cells_ref    = lt_cells_ref ).
  ENDMETHOD.

  METHOD add_tree_data_own.
    DATA lo_replace_block TYPE REF TO zcl_xtt_replace_block.
    DATA lv_top           TYPE abap_bool.

    " result
    CLEAR: et_row_top,
           et_row_bottom.

    " Main data
    FIELD-SYMBOLS <ls_data> TYPE any.
    ASSIGN ir_tree->data->* TO <ls_data>.

    " Create merge description
    CREATE OBJECT lo_replace_block
      EXPORTING
        is_block      = <ls_data>
        iv_block_name = mv_block_name.

    " Check amount of level's templates
    DATA lv_templ_lev_cnt TYPE i.
    lv_templ_lev_cnt = lines( mt_row_offset ).

    FIELD-SYMBOLS <lt_cell> TYPE tt_ex_cell.
    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN et_row_top TO <lt_cell>.
          lv_top = abap_true.
        WHEN 2.
          ASSIGN et_row_bottom TO <lt_cell>.
          lv_top = abap_false.
        WHEN 3.
          " 3-d try
          CHECK et_row_top IS INITIAL AND et_row_bottom IS INITIAL.

          ASSIGN et_row_top TO <lt_cell>.
          lv_top = abap_undefined.
      ENDCASE.

      " Find match
      DATA lr_cache TYPE REF TO ts_match_cache.
      lr_cache ?= find_match( ir_tree = ir_tree
                              iv_top  = lv_top ).
      CHECK lr_cache IS NOT INITIAL.

      " Merge with data
      <lt_cell>[] = lr_cache->tr_cells[].
      " Unique ID
      lo_replace_block->set_id( lr_cache->tr_id ).

      " For new rows & columns only (+ for groups)
      IF lv_templ_lev_cnt = 1.
        set_outline( EXPORTING iv_level        = ir_tree->level
                     CHANGING  ct_block_cells  = <lt_cell> ).
      ENDIF.

      create_new_names( CHANGING ct_block_cells  = <lt_cell>
                                 ct_dyn_def_name = ct_dyn_def_name ).

      mo_owner->merge( EXPORTING io_block = lo_replace_block
                                 iv_tabix = iv_tabix
                       CHANGING  ct_cells = <lt_cell> ).
    ENDDO.
  ENDMETHOD.

  METHOD set_outline.
    DATA ls_cell          TYPE REF TO ts_ex_cell.
    DATA ls_row           TYPE REF TO ts_ex_row.
    DATA ls_column        TYPE REF TO ts_ex_column.

    LOOP AT ct_block_cells REFERENCE INTO ls_cell.
      " Rows
      DO 1 TIMES. " WHERE c_row_dx IS NOT INITIAL. ?
        " Have outline ot not
        READ TABLE mo_owner->mt_rows WITH TABLE KEY r = ls_cell->c_row REFERENCE INTO ls_row.
        CHECK sy-subrc = 0 AND ls_row->outlinelevel IS NOT INITIAL.

        ls_cell->c_row_outline = iv_level.
        ls_row->outline_skip   = abap_true.
      ENDDO.

      " Columns
      DO 1 TIMES.
        " Have outline ot not
        READ TABLE mo_owner->mt_columns WITH TABLE KEY min = ls_cell->c_col_ind REFERENCE INTO ls_column.
        CHECK sy-subrc = 0 AND ls_column->outlinelevel IS NOT INITIAL.

        ls_cell->c_column_outline = iv_level.
        ls_column->outline_skip   = abap_true.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_new_names.
    DATA ls_dyn_def_name  LIKE LINE OF ct_dyn_def_name.
    DATA lr_cell          TYPE REF TO ts_ex_cell.

    LOOP AT ct_block_cells REFERENCE INTO lr_cell WHERE
             c_def_name CP lcl_ex_sheet=>mc_dyn_def_name.

      " Find in parent of parent
      FIELD-SYMBOLS <ls_def_name> TYPE ts_ex_defined_name.
      READ TABLE mo_owner->mo_xlsx->mt_defined_names ASSIGNING <ls_def_name>
       WITH TABLE KEY d_name = lr_cell->c_def_name.
      CHECK sy-subrc = 0.

      " Add new group
      FIELD-SYMBOLS <ls_dyn_def_name> LIKE LINE OF ct_dyn_def_name.
      READ TABLE ct_dyn_def_name ASSIGNING <ls_dyn_def_name>
       WITH TABLE KEY name = lr_cell->c_def_name.
      IF sy-subrc <> 0.
        ls_dyn_def_name-name = lr_cell->c_def_name.
        CONCATENATE `*(` ls_dyn_def_name-name `)*` INTO ls_dyn_def_name-mask.
        INSERT ls_dyn_def_name INTO TABLE ct_dyn_def_name ASSIGNING <ls_dyn_def_name>.
      ENDIF.

      " Create new name
      ADD 1 TO <ls_def_name>-d_count.
      lr_cell->c_def_name = <ls_def_name>-d_count.
      CONDENSE lr_cell->c_def_name.
      CONCATENATE <ls_def_name>-d_name lr_cell->c_def_name INTO lr_cell->c_def_name.

      " And add to result
      INSERT lr_cell->c_def_name INTO TABLE <ls_dyn_def_name>-t_all_def.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_cells_ref.
    DATA lv_new_formula   TYPE string.
    DATA lv_part          TYPE string.
    DATA lr_cell          TYPE REF TO ts_ex_cell.
    DATA lv_cnt           TYPE i.
    DATA lv_len           TYPE i.
    FIELD-SYMBOLS <ls_dyn_def_name> LIKE LINE OF it_dyn_def_name.

    CHECK it_dyn_def_name IS NOT INITIAL.
    LOOP AT it_dyn_def_name ASSIGNING <ls_dyn_def_name>.
      CLEAR lv_new_formula.

      " Number of arguments
      lv_cnt = lines( <ls_dyn_def_name>-t_all_def ).

      " New formula separeted by space
      LOOP AT <ls_dyn_def_name>-t_all_def INTO lv_part.
        IF lv_new_formula IS INITIAL.
          lv_new_formula = lv_part.
          CONTINUE.
        ENDIF.

        CONCATENATE lv_new_formula `,` lv_part INTO lv_new_formula.
      ENDLOOP.

      LOOP AT ct_cells_ref INTO lr_cell WHERE table_line->c_formula CP <ls_dyn_def_name>-mask.
        REPLACE ALL OCCURRENCES OF <ls_dyn_def_name>-name IN lr_cell->c_formula WITH lv_new_formula.

        " Formula length
        lv_len = strlen( lr_cell->c_formula ).
        IF lv_len > 8192 OR lv_cnt > 255.
          MESSAGE s006(zsy_xtt) WITH lr_cell->c_col lr_cell->c_row INTO sy-msgli.
          zcx_xtt_exception=>raise_dump( iv_message = sy-msgli ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_to.
    DATA lv_from TYPE i.
    DATA lr_cell TYPE REF TO ts_ex_cell.

    lv_from = lines( ct_cells ) + 1.
    APPEND LINES OF it_cells TO ct_cells.

    " Refs to new cells
    LOOP AT ct_cells REFERENCE INTO lr_cell FROM lv_from.
      APPEND lr_cell TO ct_cells_ref.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS cl_ex_sheet IMPLEMENTATION.
*--------------------------------------------------------------------*
  METHOD constructor.
    DATA:
      l_sheet_ind     TYPE string,
      l_sheet_name    TYPE string,
      lo_row          TYPE REF TO if_ixml_element,
      lo_cell         TYPE REF TO if_ixml_element,
      ls_cell         TYPE REF TO ts_ex_cell,
      ls_row          TYPE ts_ex_row,
      " Write here then to mt_cells
      "lt_cells_hash     TYPE SORTED TABLE OF ts_ex_cell WITH UNIQUE KEY c_row c_col,
      lo_merge_cell   TYPE REF TO if_ixml_element,
      l_val           TYPE string,
      ls_area         TYPE REF TO ts_ex_area,
      ls_cell_beg     TYPE REF TO ts_ex_cell,
      ls_cell_end     TYPE REF TO ts_ex_cell,
      ls_defined_name TYPE REF TO ts_ex_defined_name,
      ls_list_object  TYPE REF TO ts_ex_list_object,
      l_prev_row      TYPE i.
    FIELD-SYMBOLS:
     <ls_cell>         TYPE REF TO ts_ex_cell. " Double reference
***************************************
    " Get existing cell Or insert new one
    DEFINE check_in_hash.
      READ TABLE mt_cells BINARY SEARCH INTO &2 WITH KEY " with table key
        table_line->c_row = &1->c_row table_line->c_col_ind = &1->c_col_ind.
      IF sy-subrc <> 0.
        INSERT &1 INTO mt_cells INDEX sy-tabix.
        &2 = &1.

        READ TABLE mt_rows TRANSPORTING NO FIELDS
         WITH TABLE KEY r = &1->c_row.
        IF sy-subrc <> 0.
          ls_row-r = sy-tabix.
          INSERT ls_row INTO TABLE mt_rows.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

***************************************
    " Path to the sheet
    int_2_text iv_ind l_sheet_ind.
    CONCATENATE `xl/worksheets/sheet` l_sheet_ind `.xml` INTO mv_full_path.

    " Content as an object
    zcl_xtt_util=>xml_from_zip(
     EXPORTING
       io_zip     = io_xlsx->mo_zip
       iv_name    = mv_full_path
     IMPORTING
       eo_xmldoc  = mo_dom ).

***************************************
    " Loop through excels rows
    lo_row ?= mo_dom->find_from_name( 'row' ).
    WHILE lo_row IS BOUND.
      " Add new row
      zcl_xtt_excel_xlsx=>row_read_xml(
       EXPORTING
        io_node = lo_row
       CHANGING
        ct_rows = mt_rows ).

      " Loop through excels cells
      lo_cell = lo_row->find_from_name( 'c' ).
      WHILE lo_cell IS BOUND.
        io_xlsx->cell_read_xml(
         EXPORTING
          io_node  = lo_cell
         CHANGING
          ct_cells = mt_cells ).

        " Add by key
        "INSERT ls_cell INTO TABLE lt_cells_hash.
        lo_cell ?= lo_cell->get_next( ).
      ENDWHILE.

      lo_row ?= lo_row->get_next( ).
    ENDWHILE.

    " No need already sorted
    "SORT mt_cells BY table_line->c_row table_line->c_col_ind.

***************************************
    lo_merge_cell ?= mo_dom->find_from_name( 'mergeCell' ).
    WHILE lo_merge_cell IS BOUND.
      " Create new area
      l_val = lo_merge_cell->get_attribute( 'ref' ).
      CREATE DATA ls_area.
      zcl_xtt_excel_xlsx=>area_read_xml(
         iv_value = l_val
         is_area  = ls_area ).

      " Always 2 cells
      READ TABLE ls_area->a_cells INTO ls_cell_beg INDEX 1.
      READ TABLE ls_area->a_cells INTO ls_cell_end INDEX 2.

      " Get ref to existing or ls_cell_beg cell
      check_in_hash ls_cell_beg ls_cell.

      " Fill dx
      ls_cell->c_merge_dx  = ls_cell_end->c_row - ls_cell_beg->c_row.
      ls_cell->c_merge_col = ls_cell_end->c_col.

      " Next
      lo_merge_cell ?= lo_merge_cell->get_next( ).
    ENDWHILE.

***************************************
    " Defined name contains areas
    l_sheet_name = io_node->get_attribute( `name` ).
    LOOP AT io_xlsx->mt_defined_names REFERENCE INTO ls_defined_name.
      LOOP AT ls_defined_name->d_areas REFERENCE INTO ls_area WHERE a_sheet_name = l_sheet_name.
        " Reference to reference
        LOOP AT ls_area->a_cells ASSIGNING <ls_cell>.
          " Get existing cell Or insert new one
          check_in_hash <ls_cell> ls_cell.

          " Change ref in ls_area
          <ls_cell> = ls_cell.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

***************************************
    " Find related list objects
    CONCATENATE `xl/worksheets/_rels/sheet` l_sheet_ind `.xml.rels` INTO l_val.
    io_xlsx->list_object_read_xml(
     EXPORTING
      iv_path        = l_val
     CHANGING
      ct_list_objects = mt_list_objects ).

    LOOP AT mt_list_objects REFERENCE INTO ls_list_object.
      LOOP AT ls_list_object->area-a_cells ASSIGNING <ls_cell>.
        " Get existing cell Or insert new one
        check_in_hash <ls_cell> ls_cell.

        " Change ref in ls_area
        <ls_cell> = ls_cell.
      ENDLOOP.
    ENDLOOP.

***************************************
    " Write offest
    l_prev_row = 0.
    LOOP AT mt_cells INTO ls_cell.
      " If new row ls_cell->c_row_dx > 0
      ls_cell->c_row_dx = ls_cell->c_row - l_prev_row.
      l_prev_row = ls_cell->c_row.
    ENDLOOP.
  ENDMETHOD.                    "constructor
*--------------------------------------------------------------------*
  METHOD fill_shared_strings.
    DATA:
     ls_cell TYPE REF TO ts_ex_cell.

    " Add one by one
    LOOP AT mt_cells INTO ls_cell. "WHERE c_value IS NOT INITIAL.
      CHECK zcl_xtt_excel_xlsx=>cell_is_string( ls_cell ) = abap_true.
      APPEND ls_cell->c_value TO ct_shared_strings.
    ENDLOOP.
  ENDMETHOD.                    "fill_shared_strings
*--------------------------------------------------------------------*
  METHOD save.
    DATA:
      l_sheet_data   TYPE string,
      l_merge_cells  TYPE string,
      l_merge_cnt    TYPE i,
      ls_cell        TYPE REF TO ts_ex_cell,
      ls_row         TYPE REF TO ts_ex_row,
      l_new_row_ind  TYPE i,
      l_str          TYPE string,
      lo_mc          TYPE REF TO if_ixml_element,
      ls_list_object TYPE REF TO ts_ex_list_object.

    " Blank sheet
    CHECK mt_cells IS NOT INITIAL.

    " Write cells data one by one
    LOOP AT mt_cells INTO ls_cell.
      " New row index as a string
      l_new_row_ind = l_new_row_ind + ls_cell->c_row_dx.

      " New row
      IF ls_cell->c_row_dx > 0.
        " Closing tag
        IF l_sheet_data IS NOT INITIAL.
          CONCATENATE l_sheet_data `</row>` INTO l_sheet_data.
        ENDIF.

        " Read by previous key
        READ TABLE mt_rows WITH TABLE KEY r = ls_cell->c_row REFERENCE INTO ls_row.
        IF sy-subrc = 0.
          zcl_xtt_excel_xlsx=>row_write_xml(
           EXPORTING
            is_row           = ls_row
            iv_new_row       = l_new_row_ind
            iv_outline_level = ls_cell->c_outline
           CHANGING
            cv_sheet_data    = l_sheet_data ).
        ENDIF.
      ENDIF.

      " Append cell info
      io_xlsx->cell_write_xml(
       EXPORTING
        is_cell         = ls_cell
        iv_new_row      = l_new_row_ind
       CHANGING
        cv_sheet_data   = l_sheet_data
        cv_merge_cnt    = l_merge_cnt
        cv_merge_cells  = l_merge_cells ).
    ENDLOOP.

    " Closing tag
    CONCATENATE l_sheet_data `</row>` INTO l_sheet_data.

***************************************
    " Replace existing text
    xml_repleace_node(
     iv_tag_name  = 'sheetData'
     iv_repl_text = '_SHEET_DATA_' ).

    lo_mc = xml_repleace_node(
     iv_tag_name  = 'mergeCells'
     iv_repl_text = '_MERGE_CELLS_' ).

    " Change count
    IF lo_mc IS NOT INITIAL AND l_merge_cnt > 0.
      int_2_text l_merge_cnt l_str.
      lo_mc->set_attribute( name = 'count' value = l_str ).
    ENDIF.

    " Transform to string
    zcl_xtt_util=>xml_to_str(
     EXPORTING
       io_doc    = mo_dom
     IMPORTING
       ev_str    = l_str ).

    " Do replcement
    REPLACE FIRST OCCURRENCE OF '_SHEET_DATA_' IN l_str WITH l_sheet_data.
    IF lo_mc IS NOT INITIAL AND l_merge_cnt > 0.
      REPLACE FIRST OCCURRENCE OF '_MERGE_CELLS_' IN l_str WITH l_merge_cells.
    ENDIF.

    " Replace XML file
    zcl_xtt_util=>xml_to_zip(
     io_zip  = io_xlsx->mo_zip
     iv_name = mv_full_path
     iv_sdoc = l_str ).

***************************************
    " List object
    LOOP AT mt_list_objects REFERENCE INTO ls_list_object.
      io_xlsx->list_object_write_xml( ls_list_object ).
    ENDLOOP.
  ENDMETHOD.                    "save
*--------------------------------------------------------------------*
  METHOD merge.
    DATA:
      lo_new_replace_block TYPE REF TO zcl_xtt_replace_block,
      ls_field             TYPE REF TO zcl_xtt_replace_block=>ts_field,
      lt_cells_end         LIKE ct_cells,
      lt_cells_mid         LIKE ct_cells,
      lt_copy              LIKE ct_cells,
      ls_cell              TYPE REF TO ts_ex_cell,
      ls_cell_copy         LIKE ls_cell,
      l_cnt                TYPE i,
      lt_cell_match        TYPE tt_cell_match,
      lo_tree_handler      TYPE REF TO lcl_tree_handler,
      lr_tree              TYPE REF TO zcl_xtt_replace_block=>ts_tree.
    FIELD-SYMBOLS:
      <lt_items> TYPE ANY TABLE,
      <ls_item>  TYPE any.
***************************************
    " merge-№1 @see ME->MATCH_FOUND
    SET HANDLER match_found FOR io_replace_block ACTIVATION abap_true.

    " Current cell
    LOOP AT ct_cells INTO ms_cell WHERE table_line->c_formula IS INITIAL.
      " @see match_found
      io_replace_block->find_match(
       CHANGING
        cv_content = ms_cell->c_value ).
    ENDLOOP.

    " Turn off event handler
    SET HANDLER match_found FOR io_replace_block ACTIVATION abap_false.

***************************************
    " merge-№2 Structures and objects
    LOOP AT io_replace_block->mt_fields REFERENCE INTO ls_field WHERE
     typ = zcl_xtt_replace_block=>mc_type_struct OR typ = zcl_xtt_replace_block=>mc_type_object.

      " Based on nested structure
      CREATE OBJECT lo_new_replace_block
        EXPORTING
          is_field = ls_field.

      " Recursion if type is the same
      CHECK ls_field->typ = zcl_xtt_replace_block=>mc_type_struct OR
            ls_field->typ = zcl_xtt_replace_block=>mc_type_object.

      " Recursion
      me->merge(
       EXPORTING
        io_replace_block = lo_new_replace_block
       CHANGING
          ct_cells       = ct_cells ).
    ENDLOOP.

***************************************
    " merge-№3 Array types
    LOOP AT io_replace_block->mt_fields REFERENCE INTO ls_field WHERE typ = zcl_xtt_replace_block=>mc_type_table
                                                                   OR typ = zcl_xtt_replace_block=>mc_type_tree.
      " Find for replication
      cl_ex_sheet=>split_2_content(
       EXPORTING
        is_field      = ls_field
       CHANGING
        ct_cells      = ct_cells        " Begin!
        ct_cells_mid  = lt_cells_mid
        ct_cells_end  = lt_cells_end
        ct_cell_match = lt_cell_match ).

      CASE ls_field->typ.
**********************************************************************
        WHEN zcl_xtt_replace_block=>mc_type_tree.

          CREATE OBJECT lo_tree_handler
            EXPORTING
              io_owner      = me
              iv_block_name = ls_field->name
              it_row_match  = lt_cell_match.

          lr_tree ?= ls_field->dref.

          lo_tree_handler->add_tree_data(
           EXPORTING
             ir_tree = lr_tree
           CHANGING
             ct_cells = ct_cells ).

**********************************************************************
        WHEN zcl_xtt_replace_block=>mc_type_table.

          " Replicate middle
          ASSIGN ls_field->dref->* TO <lt_items>.
          l_cnt = lines( <lt_items> ).
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            IF sy-tabix = l_cnt.
              lt_copy[] = lt_cells_mid[].
            ELSE.
              " Create copy
              CLEAR lt_copy.
              LOOP AT lt_cells_mid INTO ls_cell.
                CREATE DATA ls_cell_copy.
                ls_cell_copy->* = ls_cell->*.

                APPEND ls_cell_copy TO lt_copy.
              ENDLOOP.
            ENDIF.

*            " Create descriptor
*            CREATE OBJECT lo_new_replace_block
*              EXPORTING
*                is_block      = <ls_item>
*                iv_block_name = ls_field->name.

            " Create merge description
            GET REFERENCE OF <ls_item> INTO ls_field->dref.
            CREATE OBJECT lo_new_replace_block
              EXPORTING
                is_field = ls_field.

            " Recursion
            me->merge(
             EXPORTING
              io_replace_block = lo_new_replace_block
             CHANGING
                ct_cells       = lt_copy ).
            APPEND LINES OF lt_copy TO ct_cells.
          ENDLOOP.
      ENDCASE.

      " Rest of the cells
      APPEND LINES OF lt_cells_end TO ct_cells.
    ENDLOOP.
  ENDMETHOD.                    "merge
*--------------------------------------------------------------------*
  METHOD match_found.
    CONSTANTS:
      c_date_start TYPE d VALUE '18991230',
      c_time_start TYPE t VALUE '000000'.
    DATA:
      l_len   TYPE i,
      l_value TYPE string,
      l_date  TYPE float.
    FIELD-SYMBOLS:
      <l_string> TYPE csequence,
      <l_date>   TYPE d,
      <l_time>   TYPE t.

    " Just skip
    CHECK is_field->typ <> zcl_xtt_replace_block=>mc_type_tree.

    " If the exactly in one cell and number (ms_cell->c_typ can be 's' string)
    l_len      = strlen( ms_cell->c_value ).
    iv_pos_end = iv_pos_end + 1.
    IF iv_pos_beg = 0 AND l_len = iv_pos_end.
      CASE is_field->typ.
          " integer and double(float)
        WHEN zcl_xtt_replace_block=>mc_type_integer OR zcl_xtt_replace_block=>mc_type_double.
          CLEAR ms_cell->c_type.

          " Datetime Whole as a string like  d + t
        WHEN zcl_xtt_replace_block=>mc_type_datetime.
          ASSIGN is_field->dref->* TO <l_string>.

          " Both parts
          ASSIGN <l_string>(8)     TO <l_date> CASTING.
          ASSIGN <l_string>+8(6)   TO <l_time> CASTING.
          CLEAR ms_cell->c_type.

          " Date
        WHEN zcl_xtt_replace_block=>mc_type_date.
          ASSIGN is_field->dref->* TO <l_date>.
          CLEAR ms_cell->c_type.

          " Time
        WHEN zcl_xtt_replace_block=>mc_type_time.
          ASSIGN is_field->dref->* TO <l_time>.
          CLEAR ms_cell->c_type.

        WHEN zcl_xtt_replace_block=>mc_type_boolean.
          ms_cell->c_type = 'b'.
      ENDCASE.

      DO 1 TIMES.
        " Transform date to excel format as float
        CHECK <l_date> IS ASSIGNED OR <l_time> IS ASSIGNED.

        " Date
        IF <l_date> IS ASSIGNED AND <l_date> IS NOT INITIAL.
          " Number of days since
          l_date = <l_date> - c_date_start.
        ENDIF.

        " Time
        IF <l_time> IS ASSIGNED AND <l_time> IS NOT INITIAL.
          " 0.5 half of a day
          l_date = l_date + ( <l_time> - c_time_start ) / ( 60 * 60 * 24 ).
        ENDIF.

        " Empty string
        IF     l_date IS INITIAL.
          " Empty string
          CREATE DATA is_field->dref TYPE string.
          is_field->typ = zcl_xtt_replace_block=>mc_type_string.
        ELSEIF l_date < 0.
          " Use WRITE ... TO
        ELSE.
          GET REFERENCE OF l_date INTO is_field->dref.
          is_field->typ = zcl_xtt_replace_block=>mc_type_double.
        ENDIF.
      ENDDO.
    ENDIF.

    " Try to get value as a string
    IF l_value IS INITIAL.
      l_value = zcl_xtt_replace_block=>get_as_string( is_field = is_field ).

      " Use WRITE ... TO
      IF l_date < 0.
        ms_cell->c_style = ''.
        ms_cell->c_type  = 's'.
        is_field->typ        = zcl_xtt_replace_block=>mc_type_string.
      ENDIF.
    ENDIF.

    " Create new value
    CONCATENATE
     ms_cell->c_value(iv_pos_beg)
     l_value
     ms_cell->c_value+iv_pos_end INTO ms_cell->c_value RESPECTING BLANKS.
  ENDMETHOD.                    "match_found
*--------------------------------------------------------------------*
  METHOD xml_repleace_node.
    DATA:
      lo_elem       TYPE REF TO if_ixml_element.

    ro_elem = mo_dom->find_from_name( iv_tag_name ).
    CHECK ro_elem IS BOUND.

    " Delete child elements
    DO.
      lo_elem ?= ro_elem->get_last_child( ).  " replace_child( ) instead ? IF_IXML_NODE
      IF lo_elem IS NOT BOUND.
        EXIT.
      ENDIF.

      ro_elem->remove_child( lo_elem ).
    ENDDO.

    " Replace with text
    ro_elem->set_value( iv_repl_text ).
  ENDMETHOD.                    "xml_repleace_node
*--------------------------------------------------------------------*
  METHOD split_2_content.
    TYPES:
      BEGIN OF ts_pair,
        excel_row TYPE i,       " Excel row
        array_ind TYPE sytabix, " Index in ct_cells[]
      END OF ts_pair,
      tt_pair TYPE SORTED TABLE OF ts_pair WITH UNIQUE KEY excel_row,

      BEGIN OF ts_row_off,
        level TYPE i,
        top   TYPE abap_bool,
        first TYPE i,
        last  TYPE i,
      END OF ts_row_off ,
      tt_row_off TYPE SORTED TABLE OF ts_row_off WITH UNIQUE KEY level top.

    DATA:
      lv_ind_beg   TYPE i,
      lv_ind_end   TYPE i,
      lv_find_str  TYPE string,
      ls_cell      TYPE REF TO ts_ex_cell,
      lt_row_begs  TYPE tt_pair,
      lt_row_ends  TYPE tt_pair,
      lv_row_first TYPE i,
      lv_row_last  TYPE i,
      ls_cur_pair  TYPE ts_pair,
      ls_pair_ref  TYPE REF TO ts_pair,
      lv_offset    TYPE i,
      lv_length    TYPE i,
      lv_text      TYPE string,
      lt_text      TYPE stringtab,
      lv_value     TYPE string,
      ls_row_off   TYPE ts_row_off,
      lt_row_off   TYPE tt_row_off,
      lv_from      TYPE sytabix,
      ls_row_match TYPE ts_cell_match.
    FIELD-SYMBOLS:
      <ls_row_off>  TYPE ts_row_off,
      <ls_row_off2> TYPE ts_row_off.

    " All positions
    CLEAR:
     ct_cell_match,
     ct_cells_end,
     ct_cells_mid.

    " CONCATENATE zcl_xtt_replace_block=>mc_char_block_begin iv_fld_name INTO lv_find_str.
    CONCATENATE `\{` is_field->name `\b[^}]*\}` INTO lv_find_str.

    " Find matches
    LOOP AT ct_cells INTO ls_cell.
      " Current values
      ls_cur_pair-array_ind  = sy-tabix.
      ls_cur_pair-excel_row  = ls_cur_pair-excel_row + ls_cell->c_row_dx.

      " Find row range
      READ TABLE lt_row_begs WITH TABLE KEY
       excel_row = ls_cur_pair-excel_row TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT ls_cur_pair INTO TABLE lt_row_begs.
      ENDIF.

      " Make range bigger
      READ TABLE lt_row_ends REFERENCE INTO ls_pair_ref WITH TABLE KEY
       excel_row = ls_cur_pair-excel_row.
      IF sy-subrc = 0.
        ls_pair_ref->array_ind = ls_cur_pair-array_ind.
      ELSE.
        INSERT ls_cur_pair INTO TABLE lt_row_ends.
      ENDIF.

      " Rows range
      FIND FIRST OCCURRENCE OF REGEX lv_find_str IN ls_cell->c_value
       MATCH OFFSET lv_offset
       MATCH LENGTH lv_length.
      CHECK sy-subrc = 0.

      lv_row_last = ls_cur_pair-excel_row.
      " Set 1 time only
      IF lv_row_first IS INITIAL.
        lv_row_first = lv_row_last.
      ENDIF.

***************************
      " TREE begin
      CHECK is_field->typ = zcl_xtt_replace_block=>mc_type_tree.
      lv_text = ls_cell->c_value+lv_offset(lv_length).

      " Read from texts
      SPLIT lv_text AT ';' INTO TABLE lt_text.
      LOOP AT lt_text INTO lv_text.
        SPLIT lv_text AT '=' INTO lv_text lv_value.

        CASE lv_text.
          WHEN 'level'.
            ls_row_off-level = lv_value.
          WHEN 'top'.
            IF lv_value CP 'X*'.
              ls_row_off-top = abap_true.
            ELSE.
              ls_row_off-top = abap_false.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      " Exist or new
      READ TABLE lt_row_off ASSIGNING <ls_row_off>
       FROM ls_row_off.
      IF sy-subrc <> 0.
        CLEAR:
         ls_row_off-first,
         ls_row_off-last.
        INSERT ls_row_off INTO TABLE lt_row_off ASSIGNING <ls_row_off>.
      ENDIF.

      <ls_row_off>-last = ls_cur_pair-excel_row.
      IF <ls_row_off>-first IS INITIAL.
        <ls_row_off>-first = <ls_row_off>-last.
      ENDIF.
***************************
      " TREE end
    ENDLOOP.

    " Oops not found
    IF lv_row_first IS INITIAL OR lv_row_last IS INITIAL.
      MESSAGE x001(zsy_xtt).
    ENDIF.


***************************
    " TREE begin Check overlaps
    LOOP AT lt_row_off ASSIGNING <ls_row_off>.
      lv_from = sy-tabix + 1.
      LOOP AT lt_row_off ASSIGNING <ls_row_off2> FROM lv_from WHERE
         ( first <= <ls_row_off>-last AND first >= <ls_row_off>-first ) OR
         ( last  <= <ls_row_off>-last AND last  >= <ls_row_off>-first ).
        MESSAGE x001(zsy_xtt).
      ENDLOOP.
    ENDLOOP.

    " And add
    LOOP AT lt_row_off ASSIGNING <ls_row_off>.
      CLEAR ls_row_match.

      READ TABLE lt_row_begs REFERENCE INTO ls_pair_ref
       WITH TABLE KEY excel_row = <ls_row_off>-first.
      lv_ind_beg = ls_pair_ref->array_ind.

      READ TABLE lt_row_ends REFERENCE INTO ls_pair_ref
       WITH TABLE KEY excel_row = <ls_row_off>-last.
      lv_ind_end = ls_pair_ref->array_ind.

      " And add
      ls_row_match-level = <ls_row_off>-level.
      ls_row_match-top   = <ls_row_off>-top.
      APPEND LINES OF ct_cells FROM lv_ind_beg TO lv_ind_end TO ls_row_match-cells.
      INSERT ls_row_match INTO TABLE ct_cell_match.
    ENDLOOP.
***************************
    " TREE end

    " Detect middle
    READ TABLE lt_row_begs REFERENCE INTO ls_pair_ref
     WITH TABLE KEY excel_row = lv_row_first.
    lv_ind_beg = ls_pair_ref->array_ind.

    READ TABLE lt_row_ends REFERENCE INTO ls_pair_ref
     WITH TABLE KEY excel_row = lv_row_last.
    lv_ind_end = ls_pair_ref->array_ind.

    " End
    APPEND LINES OF ct_cells FROM lv_ind_end + 1 TO ct_cells_end.
    " Middle
    APPEND LINES OF ct_cells FROM lv_ind_beg TO lv_ind_end TO ct_cells_mid.
    " Begin!
    DELETE ct_cells FROM lv_ind_beg.
  ENDMETHOD.                    "split_2_content
*--------------------------------------------------------------------*
  METHOD convert_column2int.
    DATA: lv_uccpi  TYPE i,
          lv_factor TYPE i,
          lv_offset TYPE i,
          lv_char   TYPE c,
          lv_column TYPE char3.

*   Upper case
    lv_column = iv_column.
    TRANSLATE lv_column TO UPPER CASE.
    CONDENSE lv_column NO-GAPS.

*   Get string lenght and align to right
    lv_offset = 3 - strlen( lv_column ).

    SHIFT lv_column RIGHT BY lv_offset PLACES.

*   Claculate column position
    DO 3 TIMES.
      lv_offset = sy-index - 1.
      lv_char = lv_column+lv_offset(1).
      IF lv_char IS INITIAL.
        CONTINUE.
      ENDIF.
      lv_uccpi   = cl_abap_conv_out_ce=>uccpi( lv_char ).
      lv_factor  = 26 ** ( 3 - sy-index ).
      rv_column  = rv_column + ( lv_uccpi MOD 64 ) * lv_factor.
    ENDDO.
  ENDMETHOD.                    "convert_column2int
*--------------------------------------------------------------------*
  METHOD get_cells_copy.
    DATA:
      ls_cell     TYPE REF TO ts_ex_cell,
      ls_cell_new TYPE REF TO ts_ex_cell,
      ls_row      TYPE REF TO ts_ex_row.

    LOOP AT it_cells INTO ls_cell.
      CREATE DATA ls_cell_new.
      ls_cell_new->* = ls_cell->*.

      DO 1 TIMES.
        " For new rows only
        CHECK ls_cell_new->c_row_dx IS NOT INITIAL.

        " Have outline ot not
        READ TABLE mt_rows WITH TABLE KEY r = ls_cell->c_row REFERENCE INTO ls_row.
        CHECK sy-subrc = 0 AND ls_row->outlinelevel IS NOT INITIAL.

        ls_cell_new->c_outline = ir_tree->level.
        ls_row->outline_skip   = abap_true.
      ENDDO.

      APPEND ls_cell_new TO rt_cells.
    ENDLOOP.
  ENDMETHOD.
*--------------------------------------------------------------------*
ENDCLASS.                    "cl_ex_sheet IMPLEMENTATION

**********************************************************************
**********************************************************************

CLASS lcl_tree_handler IMPLEMENTATION.
  METHOD constructor.
    mo_owner      = io_owner.
    mv_block_name = iv_block_name.
    mt_row_match  = it_row_match.
  ENDMETHOD.

  METHOD add_tree_data.
    DATA:
      lo_replace_block TYPE REF TO zcl_xtt_replace_block,
      ls_row_match     TYPE REF TO ts_cell_match,
      lv_top           TYPE abap_bool,
      lv_index         TYPE sytabix,
      lt_row_top       TYPE tt_ex_cell,
      lt_row_bottom    TYPE tt_ex_cell,
      lr_tree_attr     TYPE REF TO zcl_xtt_replace_block=>ts_tree_attr,
      lr_tree          TYPE REF TO zcl_xtt_replace_block=>ts_tree.
    FIELD-SYMBOLS:
      <ls_data> TYPE any,
      <lt_row>  TYPE tt_ex_cell.
    ASSIGN ir_tree->data->* TO <ls_data>.

    " Create merge description
    CREATE OBJECT lo_replace_block
      EXPORTING
        is_block      = <ls_data>
        iv_block_name = mv_block_name.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN lt_row_top TO <lt_row>.
          lv_top = abap_true.
        WHEN 2.
          ASSIGN lt_row_bottom TO <lt_row>.
          lv_top = abap_false.
        WHEN 3.
          " 3-d try
          CHECK lt_row_top IS INITIAL AND lt_row_bottom IS INITIAL.

          ASSIGN lt_row_top TO <lt_row>.
          lv_top = abap_undefined.
      ENDCASE.

      IF lv_top <> abap_undefined.
        READ TABLE mt_row_match REFERENCE INTO ls_row_match
         WITH TABLE KEY level = ir_tree->level top = lv_top.
      ELSE.
        lv_index = lines( mt_row_match ).
        READ TABLE mt_row_match REFERENCE INTO ls_row_match INDEX lv_index.
      ENDIF.
      CHECK sy-subrc = 0.

      " Merge with data
      <lt_row>[] = mo_owner->get_cells_copy(
       ir_tree  = ir_tree
       it_cells = ls_row_match->cells[] ).

      mo_owner->merge(
       EXPORTING
        io_replace_block = lo_replace_block
       CHANGING
        ct_cells         = <lt_row> ).
    ENDDO.

    " row before
    APPEND LINES OF lt_row_top TO ct_cells.

    " children rows
    LOOP AT ir_tree->sub_nodes REFERENCE INTO lr_tree_attr.
      lr_tree ?= lr_tree_attr->attr.
      add_tree_data(
       EXPORTING
        ir_tree  = lr_tree
       CHANGING
        ct_cells = ct_cells ).
    ENDLOOP.

    " row after
    APPEND LINES OF lt_row_bottom TO ct_cells.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

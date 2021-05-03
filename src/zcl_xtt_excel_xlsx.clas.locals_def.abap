*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPE-POOLS:
 abap.

CLASS zcl_xtt_replace_block DEFINITION LOAD.
CLASS lcl_ex_sheet          DEFINITION DEFERRED.
CLASS lcl_tree_handler      DEFINITION DEFERRED.

TYPES:
  lcl_ex_sheet_tab TYPE STANDARD TABLE OF REF TO lcl_ex_sheet,

  " Cell of Excel
  BEGIN OF ts_ex_cell,
    c_row            TYPE i,
    c_col            TYPE char3,
    c_col_ind        TYPE i,

    " Main data
    c_value          TYPE string,
    c_type           TYPE string,
    c_style          TYPE string,

    " FM
    c_formula        TYPE string,
    c_formula_is_arr TYPE abap_bool,

    " Range name
    c_def_name       TYPE string,

    " If > 0 this cell is the beginning of a new row
    c_row_dx         TYPE i,
    c_row_outline    TYPE i,

    " Same for columns
    c_col_dx         TYPE i,
    c_column_outline TYPE i,

    " Merged data
    c_merge_row_dx   TYPE i,
    c_merge_col_dx   TYPE i,
    " if equal then merge cells
    c_merge_me       TYPE abap_bool,
    c_merge_tabix    TYPE sytabix,
    c_merge_level    TYPE i,

    " Image, Shape, ...
    c_image          TYPE REF TO zcl_xtt_image,
  END OF ts_ex_cell,
  " Standard CELLS
  tt_ex_cell     TYPE STANDARD TABLE OF ts_ex_cell WITH DEFAULT KEY,
  " REFs to CELLS
  tt_ex_cell_ref TYPE STANDARD TABLE OF REF TO ts_ex_cell WITH DEFAULT KEY,

  " tree cache
  BEGIN OF ts_match_cache,
    tr_id    TYPE string,
    tr_cells TYPE tt_ex_cell,
  END OF ts_match_cache,

  " Row of Excel
  BEGIN OF ts_ex_row,
    r            TYPE i,      " Just key. Doesn't use value
    customheight TYPE string, " i
    ht           TYPE string, " Double
    hidden       TYPE string, " i
    outlinelevel TYPE string, " i
    outline_skip TYPE abap_bool,
  END OF ts_ex_row,
  tt_ex_row TYPE SORTED TABLE OF ts_ex_row WITH UNIQUE KEY r,

  BEGIN OF ts_ex_column,
    min          TYPE i,
    max          TYPE i,
    collapsed    TYPE string,
    customwidth  TYPE string,
    hidden       TYPE string,
    outlinelevel TYPE string,
    phonetic     TYPE string,
    style        TYPE string,
    width        TYPE string,
    outline_skip TYPE abap_bool,
  END OF ts_ex_column,
  tt_ex_column TYPE SORTED TABLE OF ts_ex_column WITH UNIQUE KEY min,

  " Area of Excel
  BEGIN OF ts_ex_area,
    a_sheet_name     TYPE string,      " Sheet name
    a_cells          TYPE tt_ex_cell,  " Table of ref to!
    a_original_value TYPE string,
  END OF ts_ex_area,
  tt_ex_area TYPE STANDARD TABLE OF ts_ex_area WITH DEFAULT KEY,

  " Range's name in VBA term
  BEGIN OF ts_ex_defined_name,
    d_name      TYPE string,      " Name in the top left combo
    d_local_sid TYPE string,
    d_areas     TYPE tt_ex_area,
    d_count     TYPE i,
  END OF ts_ex_defined_name,
  tt_ex_defined_name TYPE SORTED TABLE OF ts_ex_defined_name WITH UNIQUE KEY d_name d_local_sid,

  " Table or list object in VBA terms
  BEGIN OF ts_ex_list_object,
    dom             TYPE REF TO if_ixml_document,
    area            TYPE ts_ex_area,
    arc_path        TYPE string,
    _lo_id          TYPE string,
    _lo_name        TYPE string,
    _lo_displayname TYPE string,

    name_fm_mask    TYPE string,
  END OF ts_ex_list_object,
  tt_ex_list_object TYPE STANDARD TABLE OF ts_ex_list_object WITH DEFAULT KEY,

  BEGIN OF ts_cell_ref,
    r   TYPE i,
    c   TYPE i,
    beg TYPE REF TO ts_ex_cell,
    end TYPE REF TO ts_ex_cell,
    all TYPE STANDARD TABLE OF REF TO ts_ex_cell WITH DEFAULT KEY,
  END OF ts_cell_ref,
  tt_cell_ref TYPE SORTED TABLE OF ts_cell_ref WITH UNIQUE KEY r c,

  BEGIN OF ts_dyn_def_name,
    name      TYPE string,
    mask      TYPE string,
    t_all_def TYPE stringtab,
  END OF ts_dyn_def_name,
  tt_dyn_def_name TYPE SORTED TABLE OF ts_dyn_def_name WITH UNIQUE KEY name,

  " Drawn templates
  BEGIN OF ts_img_template,
    row    TYPE i,
    row_dx TYPE i,
    col    TYPE i,
    col_dx TYPE i,
    tag    TYPE string,
  END OF ts_img_template,
  tt_img_template TYPE SORTED TABLE OF ts_img_template WITH UNIQUE KEY row col,

  " Drawing (Instead of lcl class)
  BEGIN OF ts_drawing,
    _dr_xml_xl_drawings      TYPE REF TO zcl_xtt_xml_updater, " Images in cell 'xl/drawings/drawing1.xml'
    _dr_xml_xl_drawings_rels TYPE REF TO zcl_xtt_xml_updater, " Ref to files 'xl/drawings/_rels/drawing1.xml.rels'
    dr_t_required            TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line, " Insert to '[Content_Types].xml'

    " Images templates
    dr_t_img_template        TYPE tt_img_template,
  END OF ts_drawing,

  " split_2_content
  BEGIN OF ts_pair,
    position  TYPE i,       " Excel row Or column
    array_ind TYPE sytabix, " Index in ct_cells[]
  END OF ts_pair,
  tt_pair TYPE SORTED TABLE OF ts_pair WITH UNIQUE KEY position,

  BEGIN OF ts_shared_fm,
    sf_index TYPE string,
    sf_cell  TYPE REF TO ts_ex_cell,
  END OF ts_shared_fm,
  tt_shared_fm TYPE SORTED TABLE OF ts_shared_fm WITH UNIQUE KEY sf_index, " sf_row sf_col.

  BEGIN OF ts_transmit,
    rows_cells  TYPE stringtab, " sheetData tag
    cols        TYPE stringtab,
    merge_cells TYPE stringtab,
    iv_sid      TYPE i,
  END OF ts_transmit.
**********************************************************************
**********************************************************************

CLASS lcl_ex_sheet DEFINITION FINAL.
  PUBLIC SECTION.
    " INTERFACES: if_serializable_object.

    CONSTANTS:
      mc_dyn_def_name TYPE string VALUE '*_',

      BEGIN OF mc_path,
        full_path TYPE string VALUE `xl/worksheets/sheet{1}.xml`, "#EC NOTEXT
        rel_path  TYPE string VALUE `xl/worksheets/_rels/sheet{1}.xml.rels`, "#EC NOTEXT
        rel_tag   TYPE string VALUE `<Relationship Target="worksheets/sheet{1}.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Id="rId{1}"/>`, "#EC NOTEXT
      END OF mc_path.

    DATA:
      " Nested blocks / Templates based on a sheet
      _index            TYPE string,
      _index_original   TYPE string,

      _name             TYPE string,
      _state            TYPE string,  " `` || `hidden` || `veryHidden`

      mo_xlsx           TYPE REF TO zcl_xtt_excel_xlsx,
      " Dom updater
      _xml_xl_worksheet TYPE REF TO zcl_xtt_xml_updater,
      mo_document       TYPE REF TO if_ixml_document, " As an object

      mt_cells          TYPE tt_ex_cell,
      mt_rows           TYPE tt_ex_row,
      mt_columns        TYPE tt_ex_column,
      mt_list_objects   TYPE tt_ex_list_object,
      mt_data_valid     TYPE tt_ex_area,
      _t_defined_names  TYPE tt_ex_defined_name,

      " Current cell. For event handler
      " DELETE @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ms_cell           TYPE REF TO ts_ex_cell,
      mv_has_offset     TYPE abap_bool,
      _t_merge_me       TYPE tt_ex_cell_ref,

      " Images & shapes?
      mr_drawing        TYPE REF TO ts_drawing,

      " Shared formula
      mt_shared_fm      TYPE tt_shared_fm,

      " New offsets
      _t_cell_ref       TYPE tt_cell_ref.

    METHODS:
      constructor
        IMPORTING
          iv_index TYPE string
          io_xlsx  TYPE REF TO zcl_xtt_excel_xlsx,
      merged_cells_read,
      write_cells_offset,
      defined_names_read
        IMPORTING
          io_node TYPE REF TO if_ixml_element OPTIONAL
          io_src  TYPE REF TO lcl_ex_sheet    OPTIONAL,

      get_indexed
        IMPORTING
                  iv_mask        TYPE string
                  iv_original    TYPE abap_bool OPTIONAL
                  iv_2           TYPE string    OPTIONAL
        RETURNING VALUE(rv_path) TYPE string,
      _read_dom,

      find_cell
        IMPORTING
                  ir_cell           TYPE ts_ex_cell
                  iv_def_name       TYPE csequence OPTIONAL
                  iv_add            TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rr_ex_cell) TYPE REF TO ts_ex_cell,

      replace_with_new
        IMPORTING
          ir_area         TYPE REF TO ts_ex_area
          is_defined_name TYPE ts_ex_defined_name OPTIONAL
        EXPORTING
          ev_skip_name    TYPE abap_bool
        CHANGING
          ct_defined_name TYPE tt_ex_defined_name OPTIONAL,

      fill_shared_strings
        CHANGING
          ct_shared_strings TYPE stringtab,

      save
        IMPORTING
          iv_sid           TYPE i "zero based index
        CHANGING
          ct_defined_names TYPE tt_ex_defined_name OPTIONAL,
      cells_create_refs,
      _cells_write_xml
        RETURNING VALUE(rs_transmit) TYPE ts_transmit,
      _replace_by_transmit
        IMPORTING
          is_transmit TYPE ts_transmit,
      defined_name_save
        IMPORTING
          iv_sid           TYPE i
        CHANGING
          ct_defined_names TYPE tt_ex_defined_name,

      get_tag
        RETURNING VALUE(rv_tag) TYPE string,
      get_rel_tag
        RETURNING VALUE(rv_tag) TYPE string,
      change_xml_doc
        IMPORTING
                  iv_sid  TYPE i
                  iv_mask TYPE csequence
        CHANGING  cv_doc  TYPE string,
      get_new_id
        IMPORTING
                  iv_id        TYPE string
        RETURNING VALUE(rv_id) TYPE string,
      get_new_name
        IMPORTING
                  iv_name        TYPE string
        RETURNING VALUE(rv_name) TYPE string,

      merge
        IMPORTING
          io_block TYPE REF TO zcl_xtt_replace_block
          iv_tabix TYPE sytabix   OPTIONAL
          iv_force TYPE abap_bool OPTIONAL
        CHANGING
          ct_cells TYPE tt_ex_cell,
      read_scopes
        IMPORTING
          io_block TYPE REF TO zcl_xtt_replace_block
          iv_tabix TYPE sytabix
          iv_force TYPE abap_bool
        EXPORTING
          eo_scope TYPE REF TO zcl_xtt_scope
        CHANGING
          ct_cells TYPE tt_ex_cell,
      merge_sub_structures
        IMPORTING
          ir_field TYPE REF TO zcl_xtt_replace_block=>ts_field
        CHANGING
          ct_cells TYPE tt_ex_cell,
      merge_tables_trees
        IMPORTING
          ir_field TYPE REF TO zcl_xtt_replace_block=>ts_field
          io_scope TYPE REF TO zcl_xtt_scope
        CHANGING
          ct_cells TYPE tt_ex_cell,
      merge_table_ext
        IMPORTING
          ir_field     TYPE REF TO zcl_xtt_replace_block=>ts_field
          it_cells_mid TYPE tt_ex_cell
        CHANGING
          ct_cells     TYPE tt_ex_cell,

      split_2_content
        IMPORTING
          ir_field        TYPE REF TO zcl_xtt_replace_block=>ts_field
          io_scope        TYPE REF TO zcl_xtt_scope
        EXPORTING
          ev_by_column    TYPE abap_bool
          et_cells_end    TYPE tt_ex_cell
          et_cells_mid    TYPE tt_ex_cell
          eo_tree_handler TYPE REF TO lcl_tree_handler
        CHANGING
          ct_cells        TYPE tt_ex_cell,

      is_row_off_ok
        IMPORTING
                  is_field     TYPE REF TO zcl_xtt_replace_block=>ts_field
                  iv_row_first TYPE i
                  iv_row_last  TYPE i
        RETURNING VALUE(rv_ok) TYPE abap_bool,

      move_cell_content
        IMPORTING
          it_row_begs  TYPE tt_pair
          it_row_ends  TYPE tt_pair
          iv_row_first TYPE i
          iv_row_last  TYPE i
        CHANGING
          ct_cells_end TYPE tt_ex_cell
          ct_cells_mid TYPE tt_ex_cell
          ct_cells     TYPE tt_ex_cell,

      clone
        IMPORTING is_block       TYPE any
                  iv_block_name  TYPE csequence
                  iv_new_index   TYPE i
        RETURNING VALUE(ro_copy) TYPE REF TO lcl_ex_sheet,
      _is_new
        RETURNING VALUE(rv_new) TYPE abap_bool,

      merge_me
        IMPORTING
          iv_by_column TYPE abap_bool,
      _do_merge_me
        IMPORTING
          iv_by_column TYPE abap_bool
          ir_cell      TYPE REF TO ts_ex_cell
          iv_count     TYPE i.
ENDCLASS. "cl_ex_sheet DEFINITION

CLASS lcl_tree_handler DEFINITION INHERITING FROM zcl_xtt_tree_function FINAL.
  PUBLIC SECTION.
    DATA:
      mo_owner     TYPE REF TO lcl_ex_sheet.

    METHODS:
      fill_cell_match
        IMPORTING
          iv_tr_id    TYPE string
          it_row_begs TYPE tt_pair
          it_row_ends TYPE tt_pair
          it_cells    TYPE tt_ex_cell,

      init
        IMPORTING
          io_owner TYPE REF TO lcl_ex_sheet
          ir_field TYPE REF TO zcl_xtt_replace_block=>ts_field
        CHANGING
          ct_cells TYPE tt_ex_cell,

      add_tree_data
        IMPORTING
          ir_tree         TYPE REF TO zcl_xtt_replace_block=>ts_tree
          iv_tabix        TYPE sytabix
        CHANGING
          ct_dyn_def_name TYPE tt_dyn_def_name OPTIONAL
          ct_cells        TYPE tt_ex_cell,

      add_tree_data_own
        IMPORTING
          ir_tree         TYPE REF TO zcl_xtt_replace_block=>ts_tree
          iv_tabix        TYPE sytabix
        EXPORTING
          et_row_top      TYPE tt_ex_cell
          et_row_bottom   TYPE tt_ex_cell
        CHANGING
          ct_dyn_def_name TYPE tt_dyn_def_name,

      set_outline
        IMPORTING
          iv_level       TYPE i
        CHANGING
          ct_block_cells TYPE tt_ex_cell,

      create_new_names
        CHANGING
          ct_block_cells  TYPE tt_ex_cell
          ct_dyn_def_name TYPE tt_dyn_def_name,

      update_cells_ref
        IMPORTING
          it_dyn_def_name TYPE tt_dyn_def_name
        CHANGING
          ct_cells_ref    TYPE tt_ex_cell_ref,

      append_to
        IMPORTING
          it_cells     TYPE tt_ex_cell
        CHANGING
          ct_cells     TYPE tt_ex_cell
          ct_cells_ref TYPE tt_ex_cell_ref.
ENDCLASS.

**********************************************************************
**********************************************************************

* Make close friends :)
CLASS zcl_xtt_excel_xlsx DEFINITION LOCAL FRIENDS lcl_ex_sheet.
CLASS zcl_xtt_excel_xlsx DEFINITION LOCAL FRIENDS lcl_tree_handler.

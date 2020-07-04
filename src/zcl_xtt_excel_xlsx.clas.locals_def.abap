*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPE-POOLS:
 abap.

CLASS zcl_xtt_replace_block DEFINITION LOAD.

TYPES:
  " Cell of Excel
  BEGIN OF ts_ex_cell,
    c_row            TYPE i,
    c_col            TYPE char3,
    c_col_ind        TYPE i,

    " Main data
    c_value          TYPE string,
    c_type           TYPE string,
    c_style          TYPE string,
    c_formula        TYPE string,

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
  END OF ts_ex_cell,
  " Standard CELLS
  tt_ex_cell     TYPE STANDARD TABLE OF ts_ex_cell WITH DEFAULT KEY,
  " REFs to CELLS
  tt_ex_cell_ref TYPE STANDARD TABLE OF REF TO ts_ex_cell WITH DEFAULT KEY,

  BEGIN OF ts_cell_match.
    INCLUDE TYPE zcl_xtt_replace_block=>ts_tree_group.
TYPES:
  cells TYPE tt_ex_cell,
  END OF ts_cell_match,
  tt_cell_match TYPE SORTED TABLE OF ts_cell_match WITH UNIQUE KEY level top if_where,

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
    d_name  TYPE string,      " Name in the top left combo
    d_areas TYPE tt_ex_area,
    d_count TYPE i,
  END OF ts_ex_defined_name,
  tt_ex_defined_name TYPE SORTED TABLE OF ts_ex_defined_name WITH UNIQUE KEY d_name,

  " Table or list object in VBA terms
  BEGIN OF ts_ex_list_object,
    dom      TYPE REF TO if_ixml_document,
    area     TYPE ts_ex_area,
    arc_path TYPE string,
  END OF ts_ex_list_object,
  tt_ex_list_object TYPE STANDARD TABLE OF ts_ex_list_object WITH DEFAULT KEY,

  BEGIN OF ts_cell_ref,
    r   TYPE i,
    c   TYPE i,
    beg TYPE REF TO ts_ex_cell,
    end TYPE REF TO ts_ex_cell,
    all TYPE STANDARD TABLE OF REF TO ts_ex_cell WITH DEFAULT KEY,
  END OF ts_cell_ref,

  BEGIN OF ts_dyn_def_name,
    name      TYPE string,
    mask      TYPE string,
    t_all_def TYPE stringtab,
  END OF ts_dyn_def_name,
  tt_dyn_def_name TYPE SORTED TABLE OF ts_dyn_def_name WITH UNIQUE KEY name.

**********************************************************************
**********************************************************************

CLASS cl_ex_sheet DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      mc_dyn_def_name TYPE string VALUE '*_'.

    DATA:
      mo_xlsx          TYPE REF TO zcl_xtt_excel_xlsx,
      mv_full_path     TYPE string,                  " Path in zip(.xlsx,.xlsm) archive
      mv_name          TYPE string,
      mo_dom           TYPE REF TO if_ixml_document, " As an object
      mt_cells         TYPE tt_ex_cell,
      mt_rows          TYPE tt_ex_row,
      mt_columns       TYPE tt_ex_column,
      mt_list_objects  TYPE tt_ex_list_object,
      mt_data_valid    TYPE tt_ex_area,

      " Current cell. For event handler
      ms_cell          TYPE REF TO ts_ex_cell,
      mt_extra_tab_opt TYPE zcl_xtt_replace_block=>tt_extra_tab_opt,
      mt_cell_ref      TYPE SORTED TABLE OF ts_cell_ref WITH UNIQUE KEY r c.

    METHODS:
      constructor
        IMPORTING
          VALUE(iv_ind) TYPE i
          io_node       TYPE REF TO if_ixml_element
          io_xlsx       TYPE REF TO zcl_xtt_excel_xlsx,

      find_cell
        IMPORTING
                  ir_cell           TYPE ts_ex_cell
                  iv_def_name       TYPE csequence OPTIONAL
        RETURNING VALUE(rr_ex_cell) TYPE REF TO ts_ex_cell,

      replace_with_new
        IMPORTING
          ir_area         TYPE REF TO ts_ex_area
          is_defined_name TYPE ts_ex_defined_name OPTIONAL
        EXPORTING
          ev_delete_name  TYPE abap_bool
        CHANGING
          ct_defined_name TYPE tt_ex_defined_name OPTIONAL,

      fill_shared_strings
        CHANGING
          ct_shared_strings TYPE stringtab,

      save
        IMPORTING
          io_xlsx TYPE REF TO zcl_xtt_excel_xlsx,

      merge
        IMPORTING
          io_replace_block TYPE REF TO zcl_xtt_replace_block
        CHANGING
          ct_cells         TYPE tt_ex_cell,

      xml_repleace_node
        IMPORTING
                  iv_tag_name    TYPE string
                  iv_repl_text   TYPE string
        RETURNING VALUE(ro_elem) TYPE REF TO if_ixml_element,

      " Call back
      match_found FOR EVENT match_found OF zcl_xtt_replace_block
        IMPORTING is_field iv_pos_beg iv_pos_end. " iv_content

    CLASS-METHODS:
      split_2_content
        IMPORTING
          is_field      TYPE REF TO zcl_xtt_replace_block=>ts_field
          iv_by_column  TYPE abap_bool
        CHANGING
          ct_cells      TYPE tt_ex_cell
          ct_cells_end  TYPE tt_ex_cell
          ct_cells_mid  TYPE tt_ex_cell
          ct_cell_match TYPE tt_cell_match.
ENDCLASS.                    "cl_ex_sheet DEFINITION


CLASS lcl_tree_handler DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      mt_row_match  TYPE tt_cell_match,
      mo_owner      TYPE REF TO cl_ex_sheet,
      mv_block_name TYPE string,
      mv_check_prog TYPE string.

    METHODS:
      constructor
        IMPORTING
          io_owner      TYPE REF TO cl_ex_sheet
          ir_tree       TYPE REF TO zcl_xtt_replace_block=>ts_tree
          iv_block_name TYPE string
          it_row_match  TYPE tt_cell_match,

      add_tree_data
        IMPORTING
          ir_tree         TYPE REF TO zcl_xtt_replace_block=>ts_tree
        CHANGING
          ct_dyn_def_name TYPE tt_dyn_def_name OPTIONAL
          ct_cells        TYPE tt_ex_cell,

      append_to
        IMPORTING
          it_cells     TYPE tt_ex_cell
        CHANGING
          ct_cells     TYPE tt_ex_cell
          ct_cells_ref TYPE tt_ex_cell_ref.
ENDCLASS.

* Make close friends :)
CLASS zcl_xtt_excel_xlsx DEFINITION LOCAL FRIENDS cl_ex_sheet.
CLASS zcl_xtt_excel_xlsx DEFINITION LOCAL FRIENDS lcl_tree_handler.

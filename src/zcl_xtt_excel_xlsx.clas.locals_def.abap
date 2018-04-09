*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPE-POOLS:
 abap.

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
  " Only reference to cells !
  tt_ex_cell TYPE STANDARD TABLE OF ts_ex_cell WITH DEFAULT KEY,

  BEGIN OF ts_cell_match,
    level TYPE i,
    top   TYPE abap_bool,
    cells TYPE tt_ex_cell,
  END OF ts_cell_match,
  tt_cell_match TYPE SORTED TABLE OF ts_cell_match WITH UNIQUE KEY level top,

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
  END OF ts_ex_defined_name,
  tt_ex_defined_name TYPE STANDARD TABLE OF ts_ex_defined_name WITH DEFAULT KEY,

  " Table or list object in VBA terms
  BEGIN OF ts_ex_list_object,
    dom      TYPE REF TO if_ixml_document,
    area     TYPE ts_ex_area,
    arc_path TYPE string,
  END OF ts_ex_list_object,
  tt_ex_list_object TYPE STANDARD TABLE OF ts_ex_list_object WITH DEFAULT KEY,

  " For speed
  BEGIN OF ts_col_ind,
    col TYPE char3,
    ind TYPE i,
  END OF ts_col_ind,

  BEGIN OF ts_cell_ref,
    r   TYPE i,
    c   TYPE i,
    beg TYPE REF TO ts_ex_cell,
    end TYPE REF TO ts_ex_cell,
  END OF ts_cell_ref.

**********************************************************************
**********************************************************************

CLASS cl_ex_sheet DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      mv_full_path    TYPE string,                  " Path in zip(.xlsx,.xlsm) archive
      mv_name         TYPE string,
      mo_dom          TYPE REF TO if_ixml_document, " As an object
      mt_cells        TYPE tt_ex_cell,
      mt_rows         TYPE tt_ex_row,
      mt_columns      TYPE tt_ex_column,
      mt_list_objects TYPE tt_ex_list_object,
      mt_data_valid   TYPE tt_ex_area,

      " Current cell. For event handler
      ms_cell         TYPE REF TO ts_ex_cell,
      mt_column_dir   TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line,
      mt_cell_ref     TYPE SORTED TABLE OF ts_cell_ref WITH UNIQUE KEY r c.

    CLASS-DATA:
      " Backward compatibility (could be 1)
      mt_col_ind TYPE SORTED TABLE OF ts_col_ind WITH UNIQUE KEY col,
      mt_ind_col TYPE SORTED TABLE OF ts_col_ind WITH UNIQUE KEY ind.

    METHODS:
      constructor
        IMPORTING
          VALUE(iv_ind) TYPE i
          io_node       TYPE REF TO if_ixml_element
          io_xlsx       TYPE REF TO zcl_xtt_excel_xlsx,

      find_cell
        IMPORTING
                  ir_cell           TYPE ts_ex_cell
        RETURNING VALUE(rr_ex_cell) TYPE REF TO ts_ex_cell,

      replace_with_new
        IMPORTING
          ir_area TYPE REF TO ts_ex_area,

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
          ct_cell_match TYPE tt_cell_match,

      convert_column2int
        IMPORTING VALUE(iv_column) TYPE char3
        RETURNING VALUE(rv_column) TYPE i,

      convert_column2alpha
        IMPORTING VALUE(iv_column) TYPE i
        RETURNING VALUE(rv_column) TYPE char3.
ENDCLASS.                    "cl_ex_sheet DEFINITION


CLASS lcl_tree_handler DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      mt_row_match  TYPE tt_cell_match,
      mo_owner      TYPE REF TO cl_ex_sheet,
      mv_block_name TYPE string.

    METHODS:
      constructor
        IMPORTING
          io_owner      TYPE REF TO cl_ex_sheet
          iv_block_name TYPE string
          it_row_match  TYPE tt_cell_match,

      add_tree_data
        IMPORTING
          ir_tree  TYPE REF TO zcl_xtt_replace_block=>ts_tree
        CHANGING
          ct_cells TYPE tt_ex_cell.
ENDCLASS.

* Make close friends :)
CLASS zcl_xtt_excel_xlsx DEFINITION LOCAL FRIENDS cl_ex_sheet.

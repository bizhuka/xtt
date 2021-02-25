*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_tree_handler DEFINITION INHERITING FROM zcl_xtt_tree_function FINAL.
  PUBLIC SECTION.
    TYPES:
      " tree cache
      BEGIN OF ts_match_cache,
        tr_id   TYPE string,
        tr_text TYPE string,
      END OF ts_match_cache.

    METHODS:
      " only have to be intance
      detect_row_offset_01
        IMPORTING
          ir_field  TYPE REF TO zcl_xtt_replace_block=>ts_field
          iv_middle TYPE csequence
          it_match  TYPE match_result_tab
          iv_beg    TYPE i,
      check_overlaps_02
        IMPORTING
          ir_field TYPE REF TO zcl_xtt_replace_block=>ts_field,
      fill_text_match_03
        IMPORTING
          iv_tr_id  TYPE string
          it_match  TYPE match_result_tab
          iv_beg    TYPE i
          iv_middle TYPE csequence.

    METHODS:
      fill_text_match
        IMPORTING
                  iv_tr_id           TYPE string
                  iv_middle          TYPE csequence
                  is_bounds          TYPE zss_xtt_bounds
                  ir_field           TYPE REF TO zcl_xtt_replace_block=>ts_field
        RETURNING VALUE(rv_has_text) TYPE abap_bool,

      add_tree_data
        IMPORTING
          io_owner TYPE REF TO zcl_xtt_xml_base
          ir_tree  TYPE REF TO zcl_xtt_replace_block=>ts_tree
          iv_tabix TYPE sytabix
        CHANGING
          cv_text  TYPE string,

      add_tree_data_own
        IMPORTING
          io_owner       TYPE REF TO zcl_xtt_xml_base
          ir_tree        TYPE REF TO zcl_xtt_replace_block=>ts_tree
          iv_tabix       TYPE sytabix
        EXPORTING
          ev_text_top    TYPE string
          ev_text_bottom TYPE string.
*        CHANGING
    " cv_text  TYPE string.

ENDCLASS.

" Access to private data
CLASS zcl_xtt_xml_base DEFINITION LOCAL FRIENDS lcl_tree_handler.

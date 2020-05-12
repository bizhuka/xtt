*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_tree_handler DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      mt_text_match TYPE zcl_xtt_xml_base=>tt_text_match,
      mo_owner      TYPE REF TO zcl_xtt_xml_base,
      mv_block_name TYPE string,
      mv_check_prog TYPE string.

    CLASS-METHODS:
      find_extra
        CHANGING
          ct_extra_tab_opt TYPE zcl_xtt_replace_block=>tt_extra_tab_opt
          cv_content       TYPE string.

    METHODS:
      constructor
        IMPORTING
          io_owner      TYPE REF TO zcl_xtt_xml_base
          ir_tree       TYPE REF TO zcl_xtt_replace_block=>ts_tree
          iv_block_name TYPE string
          it_text_match TYPE zcl_xtt_xml_base=>tt_text_match,

      add_tree_data
        IMPORTING
          ir_tree TYPE REF TO zcl_xtt_replace_block=>ts_tree
        CHANGING
          cv_text TYPE string.
ENDCLASS.

" Access to private data
CLASS zcl_xtt_xml_base DEFINITION LOCAL FRIENDS lcl_tree_handler.

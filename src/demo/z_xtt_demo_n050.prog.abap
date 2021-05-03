*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_050 DEFINITION INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_050 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Tree group by fields'(050).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/tree-group-by-fields/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = abap_true.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      " Document structure
      BEGIN OF ts_root,
        title TYPE string,
        t     TYPE tt_rand_data,
      END OF ts_root.
    DATA ls_root  TYPE ts_root.

    " Document structure
    ls_root-title  = 'Title'(tit).

    " @see get_random_table description
    io_report->get_random_table( IMPORTING et_table = ls_root-t ).

    " Old way in code.  'R-T' ref to data
*    ls_root-t = zcl_xtt_replace_block=>tree_create(
*     it_table      = lt_items
*     iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;

    " Paste data
    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_050-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

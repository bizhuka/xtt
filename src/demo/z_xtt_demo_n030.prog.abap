*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_030 DEFINITION FINAL INHERITING FROM lcl_demo.
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
CLASS lcl_demo_030 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Nested blocks'(030).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/nested-blocks/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = rs_opt-block_count = abap_true.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      " Second merge
      BEGIN OF ts_doc,
        f_title TYPE string,
        l_title TYPE string,
      END OF ts_doc,

      " Structure of document
      BEGIN OF ts_root,
        title  TYPE string,
        bottom TYPE string,
        t      TYPE tt_rand_data, " Table within another table (lt_root)
      END OF ts_root,
      tt_root TYPE STANDARD TABLE OF ts_root WITH DEFAULT KEY.

    DATA:
      lt_root TYPE tt_root,
      ls_root TYPE REF TO ts_root,
      lv_num  TYPE char4,
      ls_doc  TYPE ts_doc,
      lv_rem  TYPE i.

    " Document structure
    DO p_b_cnt TIMES.
      " For detecting blocks
      WRITE sy-index TO lv_num LEFT-JUSTIFIED.
      lv_rem = sy-index MOD 4.

      APPEND INITIAL LINE TO lt_root REFERENCE INTO ls_root.
      CONCATENATE `Title ` lv_num INTO ls_root->title.      "#EC NOTEXT
      CONCATENATE `Bottom ` lv_num INTO ls_root->bottom.    "#EC NOTEXT

      " @see get_random_table description
      io_report->get_random_table( IMPORTING et_table = ls_root->t ).
      " Next time generate largee table
      " p_r_cnt = p_r_cnt + 1.

      CASE lv_rem.
        WHEN 1.
          SORT ls_root->t BY caption.
        WHEN 2.
          SORT ls_root->t BY group.
        WHEN 3.
          SORT ls_root->t BY sum1.
        WHEN 4.
          SORT ls_root->t BY sum2.
      ENDCASE.
    ENDDO.

    " second merge
    ls_doc-f_title = `First title`.                         "#EC NOTEXT
    ls_doc-l_title = `Last title`.                          "#EC NOTEXT

    " Faster if the begining
    io_report->merge_add_one(
      is_root    = ls_doc
      iv_root_id = 'DOC' ).

    " Each 'R' is sheet (Excel) or Page (Pdf) or Endire documnet (Word)
    io_report->merge_add_one(
      is_root    = lt_root
      iv_root_id = 'R' ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_030_A-XLSX'    TO rt_templates.
    APPEND 'ZXXT_DEMO_030_B-XLSX'    TO rt_templates.
    APPEND 'ZXXT_DEMO_030_C-XLSX'    TO rt_templates.
    APPEND 'ZXXT_DEMO_030-DOCX'      TO rt_templates.
    APPEND 'ZXXT_DEMO_030-XDP'       TO rt_templates.
    APPEND 'ZXXT_DEMO_030_EXCEL-XML' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

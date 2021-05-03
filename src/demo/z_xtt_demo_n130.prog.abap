*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_130 DEFINITION FINAL INHERITING FROM lcl_demo_020.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_130 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'COND #( ) operator'(130).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/cond/'.
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_130_A-XLSX'      TO rt_templates.
    APPEND 'ZXXT_DEMO_130_B-XLSX'      TO rt_templates.
    APPEND 'ZXXT_DEMO_130_EXCEL_A-XML' TO rt_templates.
    APPEND 'ZXXT_DEMO_130_EXCEL_B-XML' TO rt_templates.
    APPEND 'ZXXT_DEMO_130-DOCX'        TO rt_templates.
    APPEND 'ZXXT_DEMO_130-XDP'         TO rt_templates.
  ENDMETHOD.
ENDCLASS.

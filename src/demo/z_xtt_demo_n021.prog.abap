*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_021 DEFINITION FINAL INHERITING FROM lcl_demo_020.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_021 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Different formulas'(021).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/excel-formula/'.
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_021-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

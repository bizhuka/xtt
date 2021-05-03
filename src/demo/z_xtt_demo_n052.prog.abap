*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_052 DEFINITION FINAL INHERITING FROM lcl_demo_050.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_052 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Aggregation functions'(052).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/tree-aggregation-functions/'.
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_052-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

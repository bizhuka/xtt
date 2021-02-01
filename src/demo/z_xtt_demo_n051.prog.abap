*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_demo_051 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Output level by condition'(051).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/tree-output-level-by-condition/'.
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_051-XLSX'      TO rt_templates.
    APPEND 'ZXXT_DEMO_051_EXCEL-XML' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

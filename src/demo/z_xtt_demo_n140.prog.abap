*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_demo_140 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = ';type=block'(140).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/block/'.
  ENDMETHOD.

  METHOD get_templates.
    APPEND `ZXXT_DEMO_140-XLSX` TO rt_templates.
    APPEND `ZXXT_DEMO_140-DOCX` TO rt_templates.
    APPEND `ZXXT_DEMO_140-XDP`  TO rt_templates.
  ENDMETHOD.
ENDCLASS.

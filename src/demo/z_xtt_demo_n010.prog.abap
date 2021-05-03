*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_010 DEFINITION INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_010 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Simple structure'(010).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/simple-structure/'.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      " Document structure
      BEGIN OF ts_root,
        title  TYPE char15,
        text   TYPE string,
        int    TYPE i,
        bottom TYPE string, " Any field could be REF TO, STRUCTURE or TABLE
      END OF ts_root.

    " Init document structure
    DATA ls_root TYPE ts_root.
    ls_root-title   = 'Document title'.                     "#EC NOTEXT
    ls_root-text    = 'Just string'.                        "#EC NOTEXT
    ls_root-int     = 3.
    ls_root-bottom  = 'bottom'.                             "#EC NOTEXT

    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND `ZXXT_DEMO_010-XLSX` TO rt_templates.
    APPEND `ZXXT_DEMO_010-HTML` TO rt_templates.
    APPEND `ZXXT_DEMO_010-DOCX` TO rt_templates.
    APPEND `ZXXT_DEMO_010-XDP`  TO rt_templates.
  ENDMETHOD.
ENDCLASS.

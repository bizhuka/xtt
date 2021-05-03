*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_110 DEFINITION FINAL INHERITING FROM lcl_demo_100.
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
CLASS lcl_demo_110 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Image template declration'(110).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/images-no-code/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = abap_true. " Clear rs_opt-img_size.
  ENDMETHOD.

  METHOD set_merge_info.
    " General info
    DATA ls_root TYPE ts_root.

    " No direct call of ZCL_XTT_IMAGE=>CREATE_IMAGE( )
    ls_root = _get_root( iv_raw = abap_true ).

    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_110-XLSX' TO rt_templates.
    APPEND 'ZXXT_DEMO_110-DOCX' TO rt_templates.
    APPEND 'ZXXT_DEMO_110-XDP'  TO rt_templates.
  ENDMETHOD.
ENDCLASS.

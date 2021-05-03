*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_091 DEFINITION FINAL INHERITING FROM lcl_demo.
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
CLASS lcl_demo_091 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = '3D example (sheets, columns & rows)'(091).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = ''.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = rs_opt-block_count = rs_opt-colum_count = abap_true.
  ENDMETHOD.

  METHOD set_merge_info.
    DATA lt_merge TYPE lcl_demo_090=>tt_merge.

    DO p_b_cnt TIMES.
      DATA lv_index TYPE STRING.
      lv_index = sy-index.
      CONDENSE lv_index.

      DATA ls_merge LIKE LINE OF lt_merge.
      ls_merge = lcl_demo_090=>get_one_merge( io_report ).

      CONCATENATE `Sheet ` lv_index INTO ls_merge-title.

      APPEND ls_merge TO lt_merge.
    ENDDO.

    io_report->merge_add_one( lt_merge[] ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_091-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

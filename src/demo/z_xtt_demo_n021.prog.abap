*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_021 DEFINITION FINAL INHERITING FROM lcl_demo_020.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
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

  METHOD set_merge_info.
    rv_exit = super->set_merge_info( io_report ).

    " Additional info above main table
    TYPES:
      BEGIN OF ts_add,
        info TYPE string,
      END OF ts_add.
    DATA lt_add TYPE STANDARD TABLE OF ts_add WITH DEFAULT KEY.

    DATA ls_add TYPE ts_add.
    DO 3 TIMES.
      ls_add-info = sy-index.
      CONCATENATE 'String'(str) ls_add-info INTO ls_add-info SEPARATED BY ` `.
      APPEND ls_add TO lt_add.
    ENDDO.

    io_report->merge_add_one( is_root    = lt_add
                              iv_root_id = 'A' ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_021-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

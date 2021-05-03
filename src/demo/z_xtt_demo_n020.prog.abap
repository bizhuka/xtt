*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_020 DEFINITION INHERITING FROM lcl_demo.
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
CLASS lcl_demo_020 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Basic table example'(020).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/basic-tables/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = abap_true.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      " Document structure
      BEGIN OF ts_root,
        title    TYPE string,
        t        TYPE tt_rand_data,      " internal flat table ( In template {R-T} )
        date     TYPE d,                 " 8
        time     TYPE t,                 " 6
        datetime TYPE cpet_reftimestamp, " 14 = date(8) + time(6)
      END OF ts_root.
    DATA ls_root TYPE ts_root.

    " {R-T} in a temaplte. @see get_random_table description
    io_report->get_random_table( IMPORTING et_table = ls_root-t ).

    " Document structure
    ls_root-title = 'Title'(tit).

    " Date and time in header and footer
    ls_root-date   = sy-datum.
    ls_root-time   = sy-uzeit.
    " obligatory only for datetime   (;type=datetime)
    CONCATENATE sy-datum sy-uzeit INTO ls_root-datetime.

    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_020_A-XLSX'    TO rt_templates.
    APPEND 'ZXXT_DEMO_020_B-XLSX'    TO rt_templates.
    APPEND 'ZXXT_DEMO_020-DOCX'      TO rt_templates.
    APPEND 'ZXXT_DEMO_020_EXCEL-XML' TO rt_templates.
    APPEND 'ZXXT_DEMO_020_WORD-XML'  TO rt_templates.
    APPEND 'ZXXT_DEMO_020-XDP'       TO rt_templates.
  ENDMETHOD.
ENDCLASS.

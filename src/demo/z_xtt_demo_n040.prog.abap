*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_040 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION,

      _add_delimiter
        IMPORTING
          iv_delimiter TYPE csequence
          iv_field     TYPE csequence
        CHANGING
          ct_table     TYPE STANDARD TABLE.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_040 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Data types'(040).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/data-types/'.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      " Structure of document
      BEGIN OF ts_root,
        date     TYPE d,
        time     TYPE t,
        bool     TYPE abap_bool,
        int      TYPE i,
        sum      TYPE p LENGTH 13 DECIMALS 2,
        datetime TYPE cpet_reftimestamp, " 14 = date(8) + time(6)
        u        TYPE STANDARD TABLE OF t006a WITH DEFAULT KEY,
        c        TYPE STANDARD TABLE OF t005t WITH DEFAULT KEY,
        w        TYPE STANDARD TABLE OF t005t WITH DEFAULT KEY, " Only for Xml Spreadsheet 2003 (*.xml)
      END OF ts_root.
    DATA ls_root TYPE ts_root.

    " Document structure
    ls_root-date   = sy-datum.
    ls_root-time   = sy-uzeit.
    ls_root-bool   = abap_true.
    ls_root-int    = 5.
    ls_root-sum    = '777.77'.
    " obligatory only for datetime   (;type=datetime)
    CONCATENATE sy-datum sy-uzeit INTO ls_root-datetime.

    " Assign Internal to Language-Dependent Unit
    SELECT msehi msehl INTO CORRESPONDING FIELDS OF TABLE ls_root-u "#EC TOO_MANY_ITAB_FIELDS
    FROM t006a
    WHERE spras = sy-langu.

    " Country Names
    SELECT land1 landx50 INTO CORRESPONDING FIELDS OF TABLE ls_root-c "#EC TOO_MANY_ITAB_FIELDS
    FROM t005t
    WHERE spras = sy-langu.
    ls_root-w[] = ls_root-c[].

    " № 1 - Test new line delimiters -> *.xslx
    _add_delimiter( EXPORTING iv_delimiter = cl_abap_char_utilities=>cr_lf
                              iv_field     = 'MSEHL'
                    CHANGING  ct_table     = ls_root-u[] ).
    " № 2 - Test new line delimiters -> *.xslx
    _add_delimiter( EXPORTING iv_delimiter = cl_abap_char_utilities=>cr_lf
                              iv_field     = 'LANDX50'
                    CHANGING  ct_table     = ls_root-c[] ).

    " Only for ZCL_XTT_EXCEL_XML
    _add_delimiter( EXPORTING iv_delimiter = `&#10;` " ;type=as_is
                              iv_field     = 'LANDX50'
                    CHANGING  ct_table     = ls_root-w[] ).

    " Paste data
    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD _add_delimiter.
    FIELD-SYMBOLS <ls_row>   TYPE any.
    FIELD-SYMBOLS <lv_field> TYPE csequence.

    LOOP AT ct_table ASSIGNING <ls_row>.
      ASSIGN COMPONENT iv_field OF STRUCTURE <ls_row> TO <lv_field>.
      CONCATENATE <lv_field>
                  iv_delimiter
                  <lv_field> INTO <lv_field>.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_templates.
    APPEND `ZXXT_DEMO_040-XLSX`      TO rt_templates.
    APPEND `ZXXT_DEMO_040_EXCEL-XML` TO rt_templates.
  ENDMETHOD.
ENDCLASS.

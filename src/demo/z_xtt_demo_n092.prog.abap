*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_092 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_column,
        mon       TYPE numc2,
        mon_name  TYPE string,
        row_field TYPE string, " <-- NAME like R-T-T_SUMS[ 1 ]-SUM
      END OF ts_column,
      tt_column TYPE STANDARD TABLE OF ts_column WITH DEFAULT KEY,

      BEGIN OF ts_row.
        INCLUDE TYPE ts_no_sum AS _static.
      TYPES:
        t_sums TYPE tt_sums_alv,
      END OF ts_row,
      tt_row TYPE STANDARD TABLE OF ts_row WITH DEFAULT KEY.

    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.

    CLASS-METHODS:
      get_columns
        RETURNING VALUE(rt_column) TYPE tt_column.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_092 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Dynamic table (New syntax)'(092).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/dynamic-table-new-syntax/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = rs_opt-colum_count = abap_true.
    p_c_cnt  = 12.
  ENDMETHOD.

  METHOD get_columns.
    DATA lt_month_name TYPE wdr_date_nav_month_name_tab.
    CALL FUNCTION 'MONTH_NAMES_GET'
      TABLES
        month_names = lt_month_name[]
      EXCEPTIONS
        OTHERS      = 0.
    SORT lt_month_name BY mnr.

    DO p_c_cnt TIMES.
      " Index without leading space
      DATA lv_index  TYPE string.
      lv_index = sy-index.
      CONDENSE lv_index.

      " Index of month
      DATA ls_column TYPE ts_column.
      ls_column-mon = sy-index MOD 12.
      IF ls_column-mon = 0.
        ls_column-mon = 12.
      ENDIF.

      FIELD-SYMBOLS <ls_month_name> LIKE LINE OF lt_month_name.
      READ TABLE lt_month_name ASSIGNING <ls_month_name> BINARY SEARCH
       WITH KEY mnr = ls_column-mon.
      IF sy-subrc = 0.
        ls_column-mon_name = <ls_month_name>-ltx.
      ENDIF.

      " Add with dynamic column name
      CONCATENATE `{R-T:v-T_SUMS[ ` lv_index ` ]-SUM;type=double}` INTO ls_column-row_field.
      APPEND ls_column TO rt_column.
    ENDDO.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      " Document structure
      BEGIN OF ts_merge,
        c TYPE tt_column, " 1st. In template {R-C}
        t TYPE tt_row,    " 2nd.
      END OF ts_merge.

    " Columns first
    DATA ls_merge TYPE ts_merge.
    ls_merge-c = get_columns( ).

    io_report->get_random_table( EXPORTING iv_column_cnt = p_c_cnt
                                 IMPORTING et_table      = ls_merge-t ).
    io_report->merge_add_one( ls_merge ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_092-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

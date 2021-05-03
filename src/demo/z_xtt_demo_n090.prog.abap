*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_090 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_column,
        mon      TYPE numc2,
        mon_name TYPE string,
        col_name TYPE string, " <-- NAME like {R-T-SUM*}
      END OF ts_column,
      tt_column TYPE STANDARD TABLE OF ts_column WITH DEFAULT KEY,

      BEGIN OF ts_merge,
        title TYPE string,      " for 091
        c     TYPE tt_column,   " Month columns
        t     TYPE REF TO data, " Table part
      END OF ts_merge,
      tt_merge TYPE STANDARD TABLE OF ts_merge WITH DEFAULT KEY.

    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.

    CLASS-METHODS:
      get_one_merge
        IMPORTING
                  io_report       TYPE REF TO lcl_report
        RETURNING VALUE(rs_merge) TYPE ts_merge.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_090 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Dynamic table (tree)'(090).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/dynamic-table/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = rs_opt-colum_count = abap_true.
  ENDMETHOD.

  METHOD get_one_merge.
    " Structure without sums
    DATA lo_struc  TYPE REF TO cl_abap_structdescr.
    DATA ls_no_sum TYPE ts_no_sum.
    lo_struc ?= cl_abap_typedescr=>describe_by_data( ls_no_sum ).

    DATA lt_comp   TYPE abap_component_tab.
    lt_comp = lo_struc->get_components( ).

    DATA ls_comp   TYPE abap_componentdescr.
    ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'BF_RBETR' ).

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
      CONCATENATE `{R-T-SUM` lv_index `;func=SUM}` INTO ls_column-col_name.
      APPEND ls_column TO rs_merge-c.

      " Add new column
      CONCATENATE `SUM` lv_index INTO ls_comp-name.
      INSERT ls_comp INTO TABLE lt_comp.
    ENDDO.

    " Create new structure with SUM* fields
    lo_struc = cl_abap_structdescr=>create( p_components = lt_comp ).

    " Create standard table based on new structure
    DATA lo_table  TYPE REF TO cl_abap_tabledescr.
    lo_table = cl_abap_tabledescr=>create( p_line_type = lo_struc ).

    " Table part
    " {R-T} in a temaplte. @see get_random_table description
    CREATE DATA rs_merge-t TYPE HANDLE lo_table.

    " And fill table part
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN rs_merge-t->* TO <lt_table>.
    io_report->get_random_table( EXPORTING iv_column_cnt = p_c_cnt
                                 IMPORTING et_table      = <lt_table> ).
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      " Document structure
      BEGIN OF ts_merge0,
        a TYPE tt_column, " Tree 1 In template {C-A}
      END OF ts_merge0,

      BEGIN OF ts_merge1,
        t TYPE REF TO data, " Tree 2 In template {R-T}
      END OF ts_merge1.

**********************************
    DATA ls_data TYPE ts_merge.
    ls_data = get_one_merge( io_report ).

**********************************
    " Columns - old way
*  ls_merge0-a = zcl_xtt_replace_block=>tree_create(
*   it_table      = lt_column
*   iv_fields     = ''   ).   " <-- No fields! Tree with level=0

    " New way (declaration in template)
    DATA ls_merge0 TYPE ts_merge0.
    ls_merge0-a = ls_data-c[].

**********************************
    " Rows - old way
*    ls_merge1-t = zcl_xtt_replace_block=>tree_create(
*     it_table      = lr_table
*     iv_fields     = ''   ).   " <-- No fields! Tree with level=0

    " New way (declaration in template)
    DATA ls_merge1 TYPE ts_merge1.
    ls_merge1-t = ls_data-t.

**********************************
    io_report->merge_add_one( is_root    = ls_merge0
                              iv_root_id = 'C' ).
    io_report->merge_add_one( is_root    = ls_merge1
                              iv_root_id = 'R' ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_090-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

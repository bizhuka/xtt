*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_09.
  TYPES:
    " Document structure
    BEGIN OF ts_merge0,
      a TYPE REF TO data, " Tree 1 In template {C-A}
    END OF ts_merge0,

    BEGIN OF ts_merge1,
      t TYPE REF TO data, " Tree 2 In template {R-T}
    END OF ts_merge1,

    BEGIN OF ts_column,
      mon      TYPE numc2,
      col_name TYPE string, " <-- NAME like {R-T-SUM*}
    END OF ts_column,
    tt_column TYPE STANDARD TABLE OF ts_column WITH DEFAULT KEY.

  DATA lo_file   TYPE REF TO zif_xtt_file.
  DATA ls_merge0 TYPE ts_merge0.
  DATA ls_merge1 TYPE ts_merge1.
  DATA lt_column TYPE REF TO tt_column.
  DATA ls_column TYPE ts_column.
  DATA ls_no_sum TYPE ts_no_sum.
  DATA lo_struc  TYPE REF TO cl_abap_structdescr.
  DATA lt_comp   TYPE abap_component_tab.
  DATA ls_comp   TYPE abap_componentdescr.
  DATA lo_table  TYPE REF TO cl_abap_tabledescr.
  DATA lr_table  TYPE REF TO data.
  DATA lv_index  TYPE string.
  FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

  " Structure without sums
  lo_struc ?= cl_abap_typedescr=>describe_by_data( ls_no_sum ).
  lt_comp = lo_struc->get_components( ).
  ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'BF_RBETR' ).

  CREATE DATA lt_column.
  DO p_c_cnt TIMES.
    " Index without leading space
    lv_index = sy-index.
    CONDENSE lv_index.

    " Index of month
    ls_column-mon = sy-index MOD 12.
    IF ls_column-mon = 0.
      ls_column-mon = 12.
    ENDIF.

    " Add with dynamic column name
    CONCATENATE `{R-T-SUM` lv_index `;func=SUM}` INTO ls_column-col_name.
    APPEND ls_column TO lt_column->*.

    " Add new column
    CONCATENATE `SUM` lv_index INTO ls_comp-name.
    INSERT ls_comp INTO TABLE lt_comp.
  ENDDO.

  " Create new structure with SUM* fields
  lo_struc = cl_abap_structdescr=>create( p_components = lt_comp ).

  " Create standard table based on new structure
  lo_table = cl_abap_tabledescr=>create( p_line_type = lo_struc ).
  CREATE DATA lr_table TYPE HANDLE lo_table.
  ASSIGN lr_table->* TO <lt_table>.

  " No need to fill for empty template
  IF p_temp <> abap_true.
    " Columns
    ls_merge0-a = zcl_xtt_replace_block=>tree_create(
     it_table      = lt_column
     iv_fields     = ''   ).   " <-- No fields! Tree with level=0

    " {R-T} in a temaplte. @see get_random_table description
    cl_main=>get_random_table(
     EXPORTING
      iv_column_cnt = p_c_cnt
     IMPORTING
      et_table      = <lt_table> ).

    " Rows
    ls_merge1-t = zcl_xtt_replace_block=>tree_create(
     it_table      = lr_table
     iv_fields     = ''   ).   " <-- No fields! Tree with level=0
  ENDIF.

  " Show data structure only
  IF p_stru = abap_true.
    check_break_point_id( ).
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--
    RETURN.
  ENDIF.

  " Info about template & the main class itself
  CREATE OBJECT:
   lo_file TYPE zcl_xtt_file_smw0 EXPORTING
     iv_objid = iv_template,

   ro_xtt TYPE (iv_class_name) EXPORTING
    io_file = lo_file.

  " Paste data
  IF p_temp <> abap_true.
    " Columns
    ro_xtt->merge( is_block = ls_merge0 iv_block_name = 'C' ).

    " Rows
    ro_xtt->merge( ls_merge1 ).
  ENDIF.
ENDMETHOD.

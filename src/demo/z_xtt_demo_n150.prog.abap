*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_150 DEFINITION FINAL INHERITING FROM lcl_demo_022.
  PUBLIC SECTION.
    METHODS:
      get_desc_text     REDEFINITION,
      get_url_base      REDEFINITION,
      set_merge_info    REDEFINITION,
      get_templates     REDEFINITION,
      get_from_template REDEFINITION,
      on_user_command   REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      c_sum_fields TYPE string VALUE 'PRICE,SEATSMAX,SEATSOCC'.

    DATA:
      mo_alv          TYPE REF TO zcl_eui_alv,

      " Unit test mode
      mt_test_catalog TYPE lvc_t_fcat,
      mt_test_sort    TYPE lvc_t_sort.

    METHODS:
      _get_grid_params
        RETURNING VALUE(rs_gp) TYPE ts_grid_params,

      _group_by
        IMPORTING
                  iv_fields       TYPE csequence
        RETURNING VALUE(rt_group) TYPE lvc_t_sort,

      _get_modify_catalog
        RETURNING VALUE(rt_catalog) TYPE lvc_t_fcat,

      _get_test_catalog
        IMPORTING
                  ir_table          TYPE REF TO data
        RETURNING VALUE(rt_catalog) TYPE lvc_t_fcat,

      _make_toolbar
        RETURNING VALUE(rt_toolbar) TYPE ttb_button.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_150 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'No template. Grid-based'(150).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/without-grid-template/'.
  ENDMETHOD.

  METHOD _get_grid_params.
    DATA lr_flight_info TYPE REF TO tt_flight_info.

    " Result table
    CREATE DATA lr_flight_info.
    lr_flight_info->* = _get_flight_info( ).

    " ALV grid data
    rs_gp-r_table = lr_flight_info.

    " Layout
    CONCATENATE me->v_desc ` - №` p_exa INTO rs_gp-s_layout-grid_title.

    " What fields to change in field catalog
    rs_gp-t_catalog = _get_modify_catalog( ).

    " Sub groups
    rs_gp-t_sort = _group_by( '_GROUP1' ).

    rs_gp-t_toolbar = _make_toolbar( ).
  ENDMETHOD.

  METHOD _group_by.
    DATA lt_field TYPE STANDARD TABLE OF lvc_s_fcat-fieldname.
    SPLIT iv_fields AT ',' INTO TABLE lt_field.

    DATA ls_group TYPE lvc_s_sort.
    LOOP AT lt_field INTO ls_group-fieldname.
      ls_group-subtot = 'X'.
      IF sy-tabix > 1.
        ls_group-expa = 'X'.
      ENDIF.

      APPEND ls_group TO rt_group.
    ENDLOOP.
  ENDMETHOD.

  METHOD _make_toolbar.
    FIELD-SYMBOLS <ls_button> LIKE LINE OF rt_toolbar.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-butn_type = cntb_btype_sep.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = 'NO_GROUP'.
    <ls_button>-icon     = icon_wd_table.
    <ls_button>-text     = 'No group'(ngr).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = 'NO_SUM'.
    <ls_button>-icon     = icon_wd_table.
    <ls_button>-text     = 'No sum'(ngr).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function  = '_GROUP_1_FIELD'.
    <ls_button>-icon      = icon_wd_tree.
    <ls_button>-quickinfo = <ls_button>-text = 'Group by _GROUP1'.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function  = '_GROUP_2_FIELD'.
    <ls_button>-icon      = icon_wd_tree.
    <ls_button>-quickinfo = <ls_button>-text = 'Group by LandTo & Airline'.
  ENDMETHOD.

  METHOD on_user_command.
    DATA lt_sort    TYPE lvc_t_sort.

    DATA lv_do_sum TYPE abap_bool VALUE abap_true.
    CASE e_ucomm.
      WHEN 'NO_GROUP'.
        CLEAR lt_sort.

      WHEN 'NO_SUM'.
        lv_do_sum = abap_false.

      WHEN '_GROUP_1_FIELD'.
        lt_sort = _group_by( '_GROUP1' ).

      WHEN '_GROUP_2_FIELD'.
        lt_sort = _group_by( 'LANDXTO,CARRNAME' ).
    ENDCASE.
    sender->set_sort_criteria( lt_sort ).

    DATA lt_catalog TYPE lvc_t_fcat.
    DATA lr_catalog TYPE REF TO lvc_s_fcat.

    sender->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_catalog ).
    LOOP AT lt_catalog REFERENCE INTO lr_catalog.
      CHECK c_sum_fields CS lr_catalog->fieldname.
      lr_catalog->do_sum = lv_do_sum.
    ENDLOOP.
    sender->set_frontend_fieldcatalog( lt_catalog ).

    DATA ls_stable TYPE lvc_s_stbl.
    ls_stable-col = ls_stable-row = 'X'.
    sender->refresh_table_display( is_stable = ls_stable ).
  ENDMETHOD.

  METHOD _get_modify_catalog.
    DATA lr_field TYPE REF TO lvc_s_fcat.
    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_field.
    lr_field->fieldname = '_GROUP1'.
    lr_field->coltext   = 'Group flights by Departure - Arrival'.

    " Check hide fields
    APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_field.
    lr_field->fieldname = 'CURRENCY'.
    lr_field->no_out    = 'X'.

    DATA lt_sum_field TYPE STANDARD TABLE OF lvc_s_fcat-fieldname.
    SPLIT c_sum_fields AT ',' INTO TABLE lt_sum_field.

    DATA lv_field_name TYPE lvc_s_fcat-fieldname.
    LOOP AT lt_sum_field INTO lv_field_name.
      APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_field.
      lr_field->fieldname = lv_field_name.
      lr_field->do_sum    = 'X'.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_test_catalog.
    rt_catalog = zcl_eui_type=>get_catalog( ir_table = ir_table ).

    DATA lt_mod LIKE rt_catalog.
    DATA ls_mod LIKE LINE OF lt_mod.
    lt_mod = _get_modify_catalog( ).

    LOOP AT lt_mod INTO ls_mod.
      FIELD-SYMBOLS <ls_catalog> LIKE LINE OF rt_catalog.
      READ TABLE rt_catalog ASSIGNING <ls_catalog>
       WITH KEY fieldname = ls_mod-fieldname.
      CHECK sy-subrc = 0.

      zcl_eui_conv=>move_corresponding(
       EXPORTING
         is_source         = ls_mod
         iv_except_initial = abap_true    " <--- Move-corresponding except initial
       CHANGING
         cs_destination    = <ls_catalog> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_merge_info.
    DATA ls_grid_params TYPE ts_grid_params.
    ls_grid_params = _get_grid_params( ).

    " No root just table
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN ls_grid_params-r_table->* TO <lt_table>.

    " Add Info about merge.
    io_report->merge_add_one( <lt_table> ).

    " Do not show merge
    rv_exit = abap_true.

    " For test mode
    IF io_report->mo_injection IS NOT INITIAL.
      mt_test_catalog = _get_test_catalog( ls_grid_params-r_table ).
      mt_test_sort    = _group_by( '_GROUP1' ).
      RETURN.
    ENDIF.

    " Show Flight info table instead
    io_report->show_alv( EXPORTING is_grid_params = ls_grid_params
                         CHANGING  co_alv         = mo_alv  ).
  ENDMETHOD.

  METHOD get_templates.
    " No SMW0 template! Just name
    APPEND 'ZXXT_DEMO_150-XLSX' TO rt_templates.
  ENDMETHOD.

  METHOD get_from_template.
    CLEAR: eo_file, eo_xtt, ev_type.

    ev_type  = 'Excel'.                                     "#EC NOTEXT

    " Based directly on CL_GUI_ALV_GRID
    CHECK iv_template IS NOT INITIAL
      AND ( eo_file IS REQUESTED OR eo_xtt IS REQUESTED ).

    " Normal visible mode
    IF mo_alv IS NOT INITIAL.
      DATA lo_grid TYPE REF TO cl_gui_alv_grid.
      lo_grid = mo_alv->get_grid( ).
    ENDIF.

    CREATE OBJECT eo_file TYPE zcl_xtt_file_grid
      EXPORTING
        io_grid    = lo_grid
        " For test mode only
        it_catalog = mt_test_catalog
        it_sort    = mt_test_sort.

    CHECK eo_xtt IS REQUESTED.
    CREATE OBJECT eo_xtt TYPE zcl_xtt_excel_xlsx
      EXPORTING
        io_file = eo_file.
  ENDMETHOD.
ENDCLASS.

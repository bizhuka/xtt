*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

**********************************************************************
**********************************************************************

CLASS lcl_report DEFINITION FINAL INHERITING FROM zcl_xtt_report FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_cmd,
        template TYPE syucomm VALUE 'TEMPLATE',
        send     TYPE syucomm VALUE 'SEND',
        show     TYPE syucomm VALUE 'SHOW',
        download TYPE syucomm VALUE 'DOWNLOAD',
      END OF c_cmd.

    TYPES:
      BEGIN OF ts_merge_alv,
        root_id TYPE string,
        ui_type TYPE string,
        field   TYPE string,
        value   TYPE string, " For simple value
        t_color TYPE lvc_t_scol,
      END OF ts_merge_alv,
      tt_merge_alv TYPE STANDARD TABLE OF ts_merge_alv WITH DEFAULT KEY.

    " Test mode
    TYPES:
      BEGIN OF ts_file,
        kind     TYPE string,
        template TYPE string,
        report   TYPE string,
      END OF ts_file,
      BEGIN OF ts_test_demo,
        id    TYPE numc3,
        label TYPE string,
        files TYPE STANDARD TABLE OF ts_file WITH DEFAULT KEY,
        merge TYPE zcl_xtt_demo=>tt_merge,
      END OF ts_test_demo,
      tt_test_demo TYPE STANDARD TABLE OF ts_test_demo WITH DEFAULT KEY.
    DATA mt_test_demo  TYPE tt_test_demo READ-ONLY.

    METHODS:
      constructor,

      merge_add_one REDEFINITION,

      pbo,

      init REDEFINITION,


      f4_full_path
        IMPORTING
          iv_title    TYPE csequence
        CHANGING
          cv_fullpath TYPE csequence,

      f4_dir_browse
        IMPORTING
          iv_title TYPE csequence
        CHANGING
          cv_path  TYPE csequence,

      show_alv
        IMPORTING is_grid_params TYPE zcl_xtt_demo=>ts_grid_params
        CHANGING  co_alv         TYPE REF TO zcl_eui_alv OPTIONAL,

      download
        IMPORTING
          it_merge    TYPE zcl_xtt_demo=>tt_merge
          " test all templates
          iv_template TYPE csequence OPTIONAL,

      create_new_test_demo RETURNING VALUE(rr_test_demo) TYPE REF TO ts_test_demo,
      fill_file_info
        IMPORTING iv_objid       TYPE csequence
        RETURNING VALUE(rr_file) TYPE REF TO ts_file.

  PRIVATE SECTION.
    DATA t_merge_alv  TYPE tt_merge_alv.
    DATA mo_menu_docu TYPE REF TO zcl_eui_menu.

    " mv_test_mode = abap_true
    DATA mr_test_demo TYPE REF TO ts_test_demo.

    METHODS:
      show
        IMPORTING
          it_merge    TYPE zcl_xtt_demo=>tt_merge
          iv_template TYPE csequence OPTIONAL,

      send
        IMPORTING
          it_merge    TYPE zcl_xtt_demo=>tt_merge
          iv_template TYPE csequence OPTIONAL,

      _send_email
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt,

      _show_screen
        IMPORTING
                  iv_dynnr           TYPE sydynnr
                  iv_lb_id           TYPE vrm_id
                  iv_title           TYPE csequence
        RETURNING VALUE(rv_template) TYPE string,
      _is_break_point_active,

      _online_docu_button,
      _on_function_selected FOR EVENT function_selected OF cl_gui_toolbar "#EC CALLED
        IMPORTING
          fcode,
      _hide_online_docu,

      _get_docu_url
        IMPORTING
                  iv_append          TYPE csequence
        RETURNING VALUE(rv_full_url) TYPE string,

      _update_demo_listbox,

      _get_sub_fields
        IMPORTING
                  is_root             TYPE any
                  iv_root_id          TYPE string
        RETURNING VALUE(rt_sub_field) TYPE zcl_eui_type=>tt_field_desc,

      _get_dref
        IMPORTING
                  is_root        TYPE any
                  iv_field       TYPE csequence
        RETURNING VALUE(rr_data) TYPE REF TO data,

      _merge_add_sub_fields_to_alv
        IMPORTING
          is_root      TYPE any
          iv_root_id   TYPE string
          it_sub_field TYPE zcl_eui_type=>tt_field_desc,

      _get_table_text
        IMPORTING
                  ir_value       TYPE REF TO data
                  ir_alv         TYPE REF TO ts_merge_alv
        RETURNING VALUE(rv_text) TYPE string,

      _get_grid_params
        RETURNING VALUE(rs_gp) TYPE zcl_xtt_demo=>ts_grid_params,

      _on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid "#EC CALLED
        IMPORTING "sender
          e_row_id,

      _on_user_command FOR EVENT user_command OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          sender
          e_ucomm,

      _on_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid "#EC CALLED
        IMPORTING
          e_dyndoc_id,

      _get_from_root_id
        IMPORTING
                  iv_root_id     TYPE csequence
        RETURNING VALUE(rr_root) TYPE REF TO data,

      _make_std_table_copy
        IMPORTING
                  it_table        TYPE ANY TABLE
        RETURNING VALUE(rr_table) TYPE REF TO data,

      _make_tech_catalog
        IMPORTING
                  ir_table          TYPE REF TO data
        RETURNING VALUE(rt_catalog) TYPE lvc_t_fcat,

      _make_toolbar
        RETURNING VALUE(rt_toolbar) TYPE ttb_button,

      _check_has_sub_tables IMPORTING io_alv   TYPE REF TO zcl_eui_alv
                                      it_table TYPE ANY TABLE.
ENDCLASS.
**********************************************************************

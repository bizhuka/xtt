*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_report DEFINITION DEFERRED.

TYPES:
  BEGIN OF ts_merge,
    key TYPE string,
    val TYPE REF TO data,
  END OF ts_merge,
  tt_merge TYPE STANDARD TABLE OF ts_merge WITH DEFAULT KEY, " Same order HASHED  UNIQUE KEY key,

  " Random table data
  BEGIN OF ts_rand_data,
    group   TYPE string,
    caption TYPE string,
    date    TYPE d,
    sum1    TYPE bf_rbetr, " P with sign
    sum2    TYPE bf_rbetr, " P with sign
  END OF ts_rand_data,
  tt_rand_data TYPE STANDARD TABLE OF ts_rand_data WITH DEFAULT KEY,

  BEGIN OF ts_no_sum,
    group   TYPE string,
    caption TYPE string,
    date    TYPE d,
  END OF ts_no_sum,

  BEGIN OF ts_grid_params,
    r_table   TYPE REF TO data,
    s_layout  TYPE lvc_s_layo,
    t_catalog TYPE lvc_t_fcat,
    t_sort    TYPE lvc_t_sort,
    t_toolbar TYPE ttb_button,
  END OF ts_grid_params.

**********************************************************************
**********************************************************************
INTERFACE lif_injection.
  METHODS:
    send_merge
      IMPORTING
        is_merge TYPE ts_merge,

    prepare
      IMPORTING
        io_xtt TYPE REF TO zcl_xtt.
ENDINTERFACE.

**********************************************************************
**********************************************************************
CLASS lcl_demo DEFINITION ABSTRACT.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_screen_opt,
        row_count   TYPE abap_bool,
        colum_count TYPE abap_bool,
        block_count TYPE abap_bool,
        zip         TYPE abap_bool,
        img_size    TYPE abap_bool,
      END OF ts_screen_opt,

      BEGIN OF ts_template,
        objid TYPE wwwdata-objid,
      END OF ts_template,
      tt_template TYPE STANDARD TABLE OF ts_template WITH DEFAULT KEY.

    DATA:
      v_desc   TYPE string.

    METHODS:
      get_desc_text
        RETURNING VALUE(rv_desc_text) TYPE string,

      get_url_base
        RETURNING VALUE(rv_url_base) TYPE string,

      get_screen_opt
        RETURNING VALUE(rs_opt) TYPE ts_screen_opt,

      template FINAL,

      show FINAL
        IMPORTING
          it_merge TYPE tt_merge,

      download FINAL
        IMPORTING
          it_merge     TYPE tt_merge

          " test all templates
          io_injection TYPE REF TO lif_injection OPTIONAL
          iv_template  TYPE csequence            OPTIONAL,

      send FINAL
        IMPORTING
          it_merge TYPE tt_merge,

      set_merge_info ABSTRACT
        IMPORTING
                  io_report      TYPE REF TO lcl_report
        RETURNING VALUE(rv_exit) TYPE abap_bool,

      get_templates ABSTRACT
        RETURNING VALUE(rt_templates) TYPE tt_template,

      download_template
        IMPORTING
          io_file      TYPE REF TO zif_xtt_file
          iv_file_name TYPE csequence OPTIONAL,

      get_from_template
        IMPORTING
          iv_template TYPE csequence
        EXPORTING
          ev_type     TYPE string
          eo_xtt      TYPE REF TO zcl_xtt
          eo_file     TYPE REF TO zif_xtt_file,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          sender
          e_ucomm.

  PROTECTED SECTION.
    METHODS:
      _merge
        IMPORTING
          io_xtt   TYPE REF TO zcl_xtt
          it_merge TYPE tt_merge,

      _do_download
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt,

      _make_string_message
        IMPORTING
                  iv_info           TYPE csequence
        RETURNING VALUE(rr_message) TYPE REF TO string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_vrm_value,
        key  TYPE wwwdata-objid,
        text TYPE text80,
      END OF ts_vrm_value,
      tt_vrm_value TYPE STANDARD TABLE OF ts_vrm_value WITH DEFAULT KEY.

    METHODS:
      _show_screen
        IMPORTING
                  iv_dynnr           TYPE sydynnr
                  iv_lb_id           TYPE vrm_id
        RETURNING VALUE(rv_template) TYPE string,

      _get_template_lisbox
        RETURNING VALUE(rt_listbox) TYPE tt_vrm_value,

      _get_template_by_f4
        RETURNING VALUE(rv_template) TYPE string,

      _send_email
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt,

      _is_break_point_active.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_report DEFINITION FINAL FRIENDS zcl_eui_event_caller.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_cmd,
        template TYPE syucomm VALUE 'TEMPLATE',
        send     TYPE syucomm VALUE 'SEND',
        show     TYPE syucomm VALUE 'SHOW',
        download TYPE syucomm VALUE 'DOWNLOAD',
      END OF c_cmd.

    TYPES:
      BEGIN OF ts_demo,
        ind  TYPE numc3,
        inst TYPE REF TO lcl_demo,
      END OF ts_demo,
      tt_demo TYPE SORTED TABLE OF ts_demo WITH UNIQUE KEY ind,

      BEGIN OF ts_merge_alv,
        root_id TYPE string,
        ui_type TYPE string,
        field   TYPE string,
        value   TYPE string, " For simple value
        t_color TYPE lvc_t_scol,
      END OF ts_merge_alv,
      tt_merge_alv TYPE STANDARD TABLE OF ts_merge_alv WITH DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING
          io_injection TYPE REF TO lif_injection OPTIONAL,

      pbo,

      start_of_selection,

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

      merge_add_one
        IMPORTING
          is_root    TYPE any
          iv_root_id TYPE string DEFAULT 'R',

      " Random data for tables
      get_random_table
        IMPORTING
          iv_column_cnt TYPE numc2 DEFAULT 2
        EXPORTING
          et_table      TYPE STANDARD TABLE,

      init_random_generator,

      show_alv
        IMPORTING is_grid_params TYPE ts_grid_params
        CHANGING  co_alv         TYPE REF TO zcl_eui_alv OPTIONAL.

    CLASS-METHODS:
      class_constructor.

    " Current example
    DATA o_demo        TYPE REF TO lcl_demo READ-ONLY.
    DATA t_merge       TYPE tt_merge        READ-ONLY.

    " Random numbers
    DATA mo_rand_i     TYPE REF TO cl_abap_random_int    READ-ONLY.
    DATA mo_rand_p     TYPE REF TO cl_abap_random_packed READ-ONLY.

    " Test mode
    DATA mo_injection  TYPE REF TO lif_injection READ-ONLY.

  PRIVATE SECTION.
    CLASS-DATA:
      t_demo TYPE tt_demo.

    DATA t_merge_alv   TYPE tt_merge_alv.

    METHODS:
      _update_demo_listbox,

      _merge_get_sub_fields
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
        RETURNING VALUE(rs_gp) TYPE ts_grid_params,

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
        RETURNING VALUE(rt_toolbar) TYPE ttb_button.
ENDCLASS.
**********************************************************************

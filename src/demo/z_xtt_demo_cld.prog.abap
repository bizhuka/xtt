*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

TYPE-POOLS:
 abap,
 vrm,
 cntb.

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
CLASS lcl_demo_010 DEFINITION INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_020 DEFINITION INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_021 DEFINITION FINAL INHERITING FROM lcl_demo_020.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_022 DEFINITION INHERITING FROM lcl_demo_020.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ts_flight_info,
        cityfrom  TYPE spfli-cityfrom,
        landxfrom TYPE t005t-landx,
        cityto    TYPE spfli-cityto,
        landxto   TYPE t005t-landx,
        carrname  TYPE scarr-carrname,
        connid    TYPE spfli-connid,
        fldate    TYPE sflight-fldate,
        deptime   TYPE spfli-deptime,
        arrtime   TYPE spfli-arrtime,
        price     TYPE sflight-price,
        currency  TYPE sflight-currency,
        seatsmax  TYPE sflight-seatsmax,
        seatsocc  TYPE sflight-seatsocc,
        _group1   TYPE string,
      END OF ts_flight_info,
      tt_flight_info TYPE STANDARD TABLE OF ts_flight_info WITH DEFAULT KEY.

    METHODS:
      _get_flight_info
        RETURNING VALUE(rt_flight_info) TYPE tt_flight_info.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_030 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_040 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_050 DEFINITION INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_051 DEFINITION FINAL INHERITING FROM lcl_demo_050.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_052 DEFINITION FINAL INHERITING FROM lcl_demo_050.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_060 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_tree_06,
        " Folders hierarchy
        dir          TYPE string,
        par_dir      TYPE string,

        " Empty field. Filled in on_prepare_tree_06
        level        TYPE i,

        sum          TYPE bf_rbetr,
        has_children TYPE abap_bool,
      END OF ts_tree_06,
      tt_tree_06 TYPE STANDARD TABLE OF ts_tree_06 WITH DEFAULT KEY,

      " Document structure
      BEGIN OF ts_root,
        title TYPE string,

        " Or just TYPE tt_tree_06
        t     TYPE REF TO data, " <-- Table of trees (better to use general REF TO)

        " Old way
        c     TYPE REF TO data,
      END OF ts_root.

    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base    REDEFINITION,
      set_merge_info  REDEFINITION,
      get_templates   REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      _merge          REDEFINITION,
      on_prepare_tree_06 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
          ir_tree
          ir_data,

      _fill_with_folders
        IMPORTING
          iv_dir    TYPE csequence
          iv_sep    TYPE char1
        CHANGING
          ct_folder TYPE tt_tree_06.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_070 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION,

      on_pbo_07 FOR EVENT pbo_event OF zif_eui_manager      "#EC CALLED
        IMPORTING
          sender
          io_container.

  PROTECTED SECTION.
    METHODS:
      _merge          REDEFINITION,
      _do_download    REDEFINITION,

      on_prepare_raw_07 FOR EVENT prepare_raw OF zcl_xtt
        IMPORTING "sender
          iv_path
          ir_content, " Type Ref To XSTRING

      check_ole_07
        IMPORTING
          io_ole_app TYPE ole2_object.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_080 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
  PROTECTED SECTION.
    TYPES:
      " Document structure
      BEGIN OF ts_root,
        a TYPE REF TO data,  " Tree old way
        t TYPE tt_rand_data, " internal flat table ( In template {R-T} )
      END OF ts_root.

    METHODS:
      _merge          REDEFINITION,

      on_prepare_tree_08 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
          ir_tree      " Type Ref To ZCL_XTT_REPLACE_BLOCK=>TS_TREE
          ir_data      " Type Ref To DATA
          ir_sub_data. " Type Ref To DATA

*    TYPES:
*    " Tree structure
*    BEGIN OF ts_row.
*            INCLUDE TYPE ts_rand_data. " random data
*    TYPES:
*      " Fill in callback. Functions in Excel like `;func=COUNT` are preferable
*      ch_count TYPE i, " New field for trees
*    END OF ts_row,
*    tt_row TYPE STANDARD TABLE OF ts_row WITH DEFAULT KEY.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_090 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_100 DEFINITION INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ts_icon,
        id   TYPE icon-id,
        name TYPE bapibds01-objkey,
        " use code declaration instead
        img  TYPE REF TO object, " zcl_xtt_image
        raw  TYPE xstring,
      END OF ts_icon,

      " Document structure
      BEGIN OF ts_root,
        title TYPE string,
        t     TYPE STANDARD TABLE OF ts_icon WITH DEFAULT KEY, " internal flat table ( In template {R-T} )
      END OF ts_root.

    METHODS:
      _get_root
        IMPORTING
                  iv_raw         TYPE abap_bool
        RETURNING VALUE(rs_root) TYPE ts_root.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_110 DEFINITION FINAL INHERITING FROM lcl_demo_100.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_120_attr DEFINITION FINAL
     FRIENDS zcl_xtt_replace_block. " <--- for private fields
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_rand_data TYPE ts_rand_data.

    " All fields are private!
  PRIVATE SECTION.
    DATA:
      group   TYPE string,                                  "#EC NEEDED
      caption TYPE string,                                  "#EC NEEDED
      date    TYPE d,                                       "#EC NEEDED
      sum1    TYPE bf_rbetr,                                "#EC NEEDED
      sum2    TYPE bf_rbetr.                                "#EC NEEDED
ENDCLASS.

CLASS lcl_demo_120 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    " INTERFACES if_serializable_object

    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.

    TYPES:
     tt_demo_120_attr TYPE STANDARD TABLE OF REF TO lcl_demo_120_attr WITH DEFAULT KEY.

    " Public fields are always accessible
    DATA title TYPE string.                                 "#EC NEEDED
    DATA t TYPE tt_rand_data. "#EC NEEDED internal flat table ( In template {R-T} )
    DATA a TYPE REF TO data. "#EC NEEDED tt_demo_120_attr, Cannot show in ALV
    DATA date TYPE d.                                     "#EC NEEDED 8
    DATA time TYPE t.                                     "#EC NEEDED 6
    DATA datetime TYPE cpet_reftimestamp. "#EC NEEDED 14 = date(8) + time(6)

  PROTECTED SECTION.
    METHODS:
      _make_a_obj_table.

ENDCLASS.

**********************************************************************
CLASS lcl_demo_130 DEFINITION FINAL INHERITING FROM lcl_demo_020.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_140 DEFINITION FINAL INHERITING FROM lcl_demo_010.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_templates  REDEFINITION.
ENDCLASS.

**********************************************************************
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

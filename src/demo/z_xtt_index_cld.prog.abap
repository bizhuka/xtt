*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

TYPE-POOLS:
 abap,
 vrm.

INTERFACE lif_injection.
  METHODS:
    send_merge
      IMPORTING
        i_name  TYPE csequence DEFAULT 'R'
        i_value TYPE any,

    prepare
      IMPORTING
        iv_class_name TYPE csequence
        io_xtt        TYPE REF TO zcl_xtt.
ENDINTERFACE.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_no_sum,
        group   TYPE string,
        caption TYPE string,
        date    TYPE d,
      END OF ts_no_sum,

      " Random table data
      BEGIN OF ts_rand_data.
        INCLUDE TYPE ts_no_sum.
      TYPES:
        sum1 TYPE bf_rbetr, " P with sign
        sum2 TYPE bf_rbetr, " P with sign
      END OF ts_rand_data,
      tt_rand_data TYPE STANDARD TABLE OF ts_rand_data WITH DEFAULT KEY,

      " 1 example
      BEGIN OF ts_screen_opt,
        key              TYPE char5,
        show_row_count   TYPE abap_bool,
        show_colum_count TYPE abap_bool,
        show_block_count TYPE abap_bool,
        show_zip         TYPE abap_bool,
        show_size        TYPE abap_bool,
        class_name       TYPE string,
        template         TYPE string,
      END OF ts_screen_opt,
      tt_screen_opt TYPE SORTED TABLE OF ts_screen_opt WITH UNIQUE KEY key,

      " Structure of document
      BEGIN OF ts_root_03,
        title  TYPE string,
        bottom TYPE string,
        t      TYPE tt_rand_data, " Table within another table (lt_root)
      END OF ts_root_03,
      tt_root_03 TYPE STANDARD TABLE OF ts_root_03 WITH DEFAULT KEY,

      " Tree structure
      BEGIN OF ts_tree_05.
        INCLUDE TYPE ts_rand_data. " random data
      TYPES:
        ch_count TYPE i, " New field for trees
      END OF ts_tree_05,
      tt_tree_05 TYPE STANDARD TABLE OF ts_tree_05 WITH DEFAULT KEY,

      BEGIN OF ts_tree_06,
        " Folders hierarchy
        dir          TYPE string,
        par_dir      TYPE string,

        " Empty field. Filled in on_prepare_tree_06
        level        TYPE i,

        sum          TYPE bf_rbetr,
        has_children TYPE abap_bool,
      END OF ts_tree_06,
      tt_tree_06 TYPE STANDARD TABLE OF ts_tree_06 WITH DEFAULT KEY.

    METHODS:
      constructor
       IMPORTING
         io_injection  TYPE REF TO lif_injection OPTIONAL,

      pbo,

      pai
        IMPORTING
          cv_cmd TYPE syucomm,

      start_of_selection
        IMPORTING
          iv_file_name TYPE csequence OPTIONAL,

      call_example
        IMPORTING
         is_screen_opt TYPE REF TO ts_screen_opt
        RETURNING VALUE(ro_xtt) TYPE REF TO zcl_xtt,

      send_email
       IMPORTING
         io_xtt TYPE REF TO zcl_xtt EXPORTING ev_break TYPE abap_bool,

      export_template
        IMPORTING
         iv_file_name  TYPE csequence
         is_screen_opt TYPE REF TO ts_screen_opt,

      is_break_point_active,

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

      " Random data for tables
      get_random_table
        IMPORTING
          iv_column_cnt TYPE numc2 DEFAULT 2
        EXPORTING
          et_table      TYPE STANDARD TABLE,

      " Basic example
      example_01                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING ev_break TYPE abap_bool,

      example_02                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      example_03                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,
      _2nd_time_for_excel
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
          it_root TYPE tt_root_03,

      example_04                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      example_05                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      on_prepare_tree_05 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
            ir_tree      " Type Ref To ZCL_XTT_REPLACE_BLOCK=>TS_TREE
            ir_data      " Type Ref To DATA
            ir_sub_data, " Type Ref To DATA

      example_06                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      on_prepare_tree_06 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
            ir_tree
            ir_data,

      fill_with_folders
        IMPORTING
          iv_dir    TYPE csequence
          iv_sep    TYPE char1
        CHANGING
          ct_folder TYPE tt_tree_06,

      example_07                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      on_prepare_raw_07 FOR EVENT prepare_raw OF zcl_xtt
        IMPORTING
            sender
            iv_path
            ir_content, " Type Ref To XSTRING

      on_pbo_07 FOR EVENT pbo_event OF zif_eui_manager      "#EC CALLED
        IMPORTING
            sender
            io_container,

      check_ole_07
        IMPORTING
          io_ole_app TYPE ole2_object,

      example_08                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      example_09                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      example_10                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
          iv_raw TYPE abap_bool OPTIONAL
        EXPORTING
          ev_break TYPE abap_bool,

      example_11                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      example_12                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      example_13                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      example_14                                            "#EC CALLED
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt
        EXPORTING
          ev_break TYPE abap_bool,

      _init_random_numbers.

    DATA mt_screen_opt TYPE tt_screen_opt.
    DATA mo_injection  TYPE REF TO lif_injection.

    " Random numbers
    DATA mo_rand_i     TYPE REF TO cl_abap_random_int.
    DATA mo_rand_p     TYPE REF TO cl_abap_random_packed.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_test DEFINITION FINAL FOR TESTING  "#AU Risk_Level Harmless
                                      .      "#AU Duration Long
  PUBLIC SECTION.

  PRIVATE SECTION.
    DATA:
      cut         TYPE REF TO lcl_report.

    METHODS:
      setup,

      export_all FOR TESTING,

      _set_sy.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT cut
      EXPORTING
        iv_test_mode = abap_true.

    " Folders for report
    p_r_path = 'C:\Users\modekz\Desktop\arc\xtt-image\'.
  ENDMETHOD.

  METHOD export_all. " For live demo in https://bizhuka.github.io/xtt/
    " Get listbox
    DATA lt_list TYPE vrm_values.
    CALL FUNCTION 'VRM_GET_VALUES'
      EXPORTING
        id     = 'P_EXA'
      IMPORTING
        values = lt_list.

    " DELETE lt_list WHERE key ''.
    CHECK lt_list IS NOT INITIAL.


*DELETE lt_list WHERE key <> '092'.
***********************************        p_r_cnt = 15.
***********************************        p_c_cnt = 36.
***********************************        p_b_cnt = 3.
    p_open  = abap_false.
    " All demo
    DATA: ls_list TYPE REF TO vrm_value, lo_demo TYPE REF TO zcl_xtt_demo.
    LOOP AT lt_list REFERENCE INTO ls_list.
      DATA lr_test_demo TYPE REF TO lcl_report=>ts_test_demo.
      lr_test_demo = cut->create_new_test_demo( ).

      " Set current example
      p_exa = lr_test_demo->id = ls_list->key.

      " Always the same random data
      _set_sy( ).

      " Launch current example
      cut->start_of_selection(
        iv_r_cnt = 15 " p_r_cnt
        iv_c_cnt = 36 " p_c_cnt
        iv_b_cnt = 3 " p_b_cnt
      ).
      lo_demo = cut->o_demo.
      lr_test_demo->label = lo_demo->v_desc.

      DATA lt_template TYPE zcl_xtt_demo=>tt_template.
      lt_template = lo_demo->get_templates( ).

      DATA lr_template TYPE REF TO zcl_xtt_demo=>ts_template.
      LOOP AT lt_template REFERENCE INTO lr_template.
        DATA lr_file TYPE REF TO lcl_report=>ts_file.
        lr_file = cut->fill_file_info( lr_template->objid ).
        _set_sy( ).

        " Download report
        cut->download( it_merge    = cut->t_merge
                       iv_template = lr_template->objid ).

        " Download template
        DATA lo_xtt_file TYPE REF TO zif_xtt_file.
        lo_demo->get_from_template( EXPORTING iv_template = lr_template->objid
                                    IMPORTING eo_file     = lo_xtt_file ).
        lo_demo->download_template( io_file      = lo_xtt_file
                                    iv_file_name = lr_file->template ).
      ENDLOOP.
    ENDLOOP.

    " Export file
    DATA lv_file   TYPE string.
    DATA lo_file   TYPE REF TO zcl_eui_file.
    DATA lo_error  TYPE REF TO zcx_eui_exception.

    TRY.
        lv_file = zcl_eui_conv=>to_json( im_data = cut->mt_test_demo
                                         iv_pure = abap_true ).

        CREATE OBJECT lo_file.
        lo_file->import_from_string( lv_file ).
        lo_file->download( iv_full_path = `xtt_demo.json` ). "#EC NOTEXT
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _set_sy.
    sy-uzeit = '121212'.                                  "#EC WRITE_OK
    sy-datum = '20201021'.                                "#EC WRITE_OK
  ENDMETHOD.
ENDCLASS.

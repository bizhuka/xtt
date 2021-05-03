*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_test DEFINITION FINAL FOR TESTING  "#AU Risk_Level Harmless
                                      .      "#AU Duration Long
  PUBLIC SECTION.
    INTERFACES lif_injection.

  PRIVATE SECTION.
    TYPES:
*      1 demo
      BEGIN OF ts_file,
        kind     TYPE string,
        template TYPE string,
        report   TYPE string,
      END OF ts_file,

      BEGIN OF ts_demo,
        id    TYPE numc3,
        label TYPE string,
        files TYPE STANDARD TABLE OF ts_file WITH DEFAULT KEY,
        merge TYPE tt_merge,
      END OF ts_demo,
      tt_demo TYPE STANDARD TABLE OF ts_demo WITH DEFAULT KEY.

    DATA:
      cut         TYPE REF TO lcl_report,
      ms_cur_demo TYPE REF TO ts_demo,
      _raw_folder TYPE string.

    METHODS:
      setup,

      export_all FOR TESTING,

      _fill_file_info
        IMPORTING
                  iv_objid       TYPE csequence
        RETURNING VALUE(rr_file) TYPE REF TO ts_file,

      on_prepare_raw FOR EVENT prepare_raw OF zcl_xtt
        IMPORTING "sender
          iv_path
          ir_content, " Type Ref To XSTRING,

      _set_sy.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT cut
      EXPORTING
        io_injection = me.

    " Folders for report
    p_r_path = 'C:\Users\moldab\Desktop\arc\xtt-image\'.

    p_r_cnt = 15.
    p_c_cnt = 36.
    p_b_cnt = 3.
    p_open  = abap_false.
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

    " Result in JSON
    DATA lt_demo  TYPE tt_demo.

    " All demo
    DATA: ls_list TYPE REF TO vrm_value, lo_demo TYPE REF TO lcl_demo.
    LOOP AT lt_list REFERENCE INTO ls_list.
      " Info about example group
      APPEND INITIAL LINE TO lt_demo REFERENCE INTO ms_cur_demo.

      " Set current example
      p_exa = ms_cur_demo->id = ls_list->key.

      " Always the same random data
      _set_sy( ).

      " Launch current example
      cut->start_of_selection( ).
      lo_demo = cut->o_demo.
      ms_cur_demo->label = lo_demo->v_desc.

      DATA lt_template TYPE lcl_demo=>tt_template.
      lt_template = lo_demo->get_templates( ).

      DATA lr_template TYPE REF TO lcl_demo=>ts_template.
      LOOP AT lt_template REFERENCE INTO lr_template.
        DATA lr_file TYPE REF TO ts_file.
        lr_file = _fill_file_info( lr_template->objid ).

        " Download report
        lo_demo->download( it_merge     = cut->t_merge
                           io_injection = me
                           iv_template  = lr_template->objid ).

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
        lv_file = zcl_eui_conv=>to_json( im_data = lt_demo
                                         iv_pure = abap_true ).

        CREATE OBJECT lo_file.
        lo_file->import_from_string( lv_file ).
        lo_file->download( iv_full_path = `xtt_demo.json` ). "#EC NOTEXT
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _fill_file_info.
    APPEND INITIAL LINE TO ms_cur_demo->files REFERENCE INTO rr_file.

    cut->o_demo->get_from_template( EXPORTING iv_template = iv_objid
                                    IMPORTING ev_type     = rr_file->kind ).

    " 2 unique names
    rr_file->template = rr_file->report  = iv_objid.
    REPLACE FIRST OCCURRENCE OF '-' IN: rr_file->template WITH `_T.`,
                                        rr_file->report   WITH `_R.`.
    REPLACE FIRST OCCURRENCE OF '.XDP' IN rr_file->report WITH '.PDF'.

    " Always the same random data
    _set_sy( ).

    " Download preparations
    p_path = rr_file->report.

    CONCATENATE p_exa `_` iv_objid  INTO _raw_folder.
  ENDMETHOD.

  METHOD _set_sy.
    sy-uzeit = '121212'.                                  "#EC WRITE_OK
    sy-datum = '20201021'.                                "#EC WRITE_OK
  ENDMETHOD.

  METHOD lif_injection~send_merge.
    INSERT is_merge INTO TABLE ms_cur_demo->merge.
  ENDMETHOD.

  METHOD lif_injection~prepare.
    SET HANDLER on_prepare_raw FOR io_xtt.

    DATA lo_class TYPE REF TO cl_abap_classdescr.
    lo_class ?= cl_abap_classdescr=>describe_by_object_ref( io_xtt ).

    " For data exporting
    CASE lo_class->absolute_name.
      WHEN '\CLASS=ZCL_XTT_WORD_DOCX'.
        " io_xtt->add_raw_event( 'word/document.xml' ).
        io_xtt->add_raw_event( 'word/header1.xml' ).
        io_xtt->add_raw_event( 'word/footer1.xml' ).

      WHEN '\CLASS=ZCL_XTT_EXCEL_XLSX'.
        io_xtt->add_raw_event( 'xl/workbook.xml' ).

        " Max number of sheets
        DO 12 TIMES.
          " Path to file
          DATA lv_path TYPE string.

          lv_path = sy-index.
          CONDENSE lv_path NO-GAPS.

          CONCATENATE `xl/worksheets/sheet` lv_path `.xml` INTO lv_path.

          io_xtt->add_raw_event( lv_path ).
        ENDDO.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD on_prepare_raw.
    " No need to export entire file
    CHECK iv_path IS NOT INITIAL.

    " Work with copy
    DATA lv_content TYPE xstring.
    lv_content = ir_content->*.

    DO 1 TIMES.
      DATA lo_dom TYPE REF TO if_ixml_document.
      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          xml      = lv_content
        IMPORTING
          document = lo_dom
        EXCEPTIONS
          OTHERS   = 1.
      CHECK sy-subrc = 0.

      CALL FUNCTION 'SDIXML_DOM_TO_XML'
        EXPORTING
          document      = lo_dom
          pretty_print  = 'X'
        IMPORTING
          xml_as_string = lv_content
        EXCEPTIONS
          OTHERS        = 1.
      CHECK sy-subrc = 0.
    ENDDO.

    DATA lv_path TYPE string.
    "TODO check path
    CONCATENATE `C:\Users\moldab\AppData\Local\SAP\SAP GUI\tmp\` _raw_folder `\` iv_path INTO lv_path.
    REPLACE ALL OCCURRENCES OF `/` IN lv_path WITH `\`.

    " Export file
    DATA lo_file   TYPE REF TO zcl_eui_file.
    DATA lo_error  TYPE REF TO zcx_eui_exception.

    TRY.
        CREATE OBJECT lo_file.
        lo_file->import_from_xstring( lv_content ).
        lo_file->download( iv_full_path = lv_path ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

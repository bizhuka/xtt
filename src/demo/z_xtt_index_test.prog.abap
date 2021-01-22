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

      BEGIN OF ts_merge_param,
        key TYPE string,
        val TYPE REF TO data,
      END OF ts_merge_param,

      BEGIN OF ts_demo,
        id    TYPE num2,
        label TYPE string,
        files TYPE STANDARD TABLE OF ts_file WITH DEFAULT KEY,
        merge TYPE HASHED TABLE OF ts_merge_param WITH UNIQUE KEY key,
      END OF ts_demo,
      tt_demo TYPE STANDARD TABLE OF ts_demo WITH DEFAULT KEY.

    DATA:
      cut         TYPE REF TO lcl_main,
      ms_cur_demo TYPE REF TO ts_demo.

    METHODS:
      setup,

      export_01 FOR TESTING,

      on_prepare_raw FOR EVENT prepare_raw OF zcl_xtt
        IMPORTING
          sender
          iv_path
          ir_content. " Type Ref To XSTRING.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT cut
      EXPORTING
        io_injection = me.

    " Folders for report
    p_r_path = 'C:\Users\MoldaB\Desktop\arc\xtt-image\'.

    " Export data & template & report
    SPLIT `X-X-X-X- ` AT `-` INTO p_stru
                                  p_temp
                                  p_repo
                                  p_dwnl
                                  p_open.  " p_zip, p_size
    p_r_cnt = 15.
    p_c_cnt = 36.
    p_b_cnt = 3.
  ENDMETHOD.

  METHOD export_01. " For live demo in https://bizhuka.github.io/xtt/
    " Get listbox
    DATA lt_list TYPE vrm_values.
    CALL FUNCTION 'VRM_GET_VALUES'
      EXPORTING
        id     = 'P_EXA'
      IMPORTING
        values = lt_list.
    CHECK lt_list IS NOT INITIAL.

    " if no ADS
*    DELETE lt_list WHERE text CP '*PDF*'.
*    DELETE lt_list WHERE key NP '*11*'.

    " Result in JSON
    DATA lt_demo  TYPE tt_demo.

    " All demo
    DATA ls_list TYPE REF TO vrm_value.
    LOOP AT lt_list REFERENCE INTO ls_list WHERE key NP '*-00'.
      " Extract file info
      DATA lv_text TYPE string.
      DATA ls_file TYPE ts_file.
      SPLIT ls_list->text AT ` - ` INTO lv_text ls_file-kind.

      " Name & extension
      DATA lv_ext TYPE string.
      SPLIT lv_text AT ` (` INTO lv_text lv_ext.
      REPLACE FIRST OCCURRENCE OF `)` IN lv_ext WITH ``.

*      2 unique names
      CONCATENATE ls_list->key `_T.` lv_ext INTO ls_file-template.
      CONCATENATE ls_list->key `_R.` lv_ext INTO ls_file-report.

      " Original format
      REPLACE FIRST OCCURRENCE OF `.pdf` IN ls_file-template WITH `.xdp`. "#EC NOTEXT

      " New demo index
      DATA lv_num  TYPE num2.
      DATA lv_prev TYPE num2.

      lv_num = ls_list->key(2).
      IF lv_num <> lv_prev.
        lv_prev = lv_num.
        APPEND INITIAL LINE TO lt_demo REFERENCE INTO ms_cur_demo.
        ms_cur_demo->id    = lv_num.
        ms_cur_demo->label = lv_text.
      ENDIF.

      " Add file
      APPEND ls_file TO ms_cur_demo->files.

      " Set current example
      p_exa = ls_list->key.
      p_path = ls_file-report.
      cut->start_of_selection( iv_file_name = ls_file-template ).
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

  METHOD lif_injection~send_merge.
    DATA          ls_merge  TYPE ts_merge_param.
    FIELD-SYMBOLS <l_value> TYPE any.

    " № 1 - merge IV_BLOCK_NAME parameter
    ls_merge-key = i_name.

    " № 2 - merge IS_BLOCK parameter
    CREATE DATA ls_merge-val LIKE i_value.
    ASSIGN ls_merge-val->* TO <l_value>.
    <l_value> = i_value.

    INSERT ls_merge INTO TABLE ms_cur_demo->merge.
  ENDMETHOD.

  METHOD lif_injection~prepare.
    SET HANDLER on_prepare_raw FOR io_xtt.

    " For data exporting
    CASE iv_class_name.
      WHEN 'ZCL_XTT_WORD_DOCX'.
        " io_xtt->add_raw_event( 'word/document.xml' ).
        io_xtt->add_raw_event( 'word/header1.xml' ).
        io_xtt->add_raw_event( 'word/footer1.xml' ).

      WHEN 'ZCL_XTT_EXCEL_XLSX'.
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

    " Always the same random data
    sy-uzeit = '121212'.    "#EC WRITE_OK
    sy-datum = '20201021'.  "#EC WRITE_OK
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
    CONCATENATE `C:\Users\MoldaB\AppData\Local\SAP\SAP GUI\tmp\` p_exa `\` iv_path INTO lv_path.
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

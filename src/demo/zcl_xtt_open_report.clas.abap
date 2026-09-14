CLASS zcl_xtt_open_report DEFINITION INHERITING FROM zcl_xtt_report PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_example,
             ind  TYPE char3,
             desc TYPE string,
           END OF ts_example,
           tt_examples TYPE STANDARD TABLE OF ts_example WITH DEFAULT KEY.

    METHODS make_all.

    METHODS get_all_examples RETURNING VALUE(rt_examples) TYPE tt_examples.

    METHODS web_get_example_meta
      IMPORTING iv_ind       TYPE char3
      EXPORTING es_opt       TYPE zcl_xtt_demo=>ts_screen_opt
                et_templates TYPE zcl_xtt_demo=>tt_template
                ev_error     TYPE string.

    METHODS web_generate
      IMPORTING iv_ind      TYPE char3
                iv_template TYPE string
                iv_r_cnt    TYPE int4
                iv_c_cnt    TYPE numc2
                iv_b_cnt    TYPE int4
      EXPORTING ev_raw      TYPE xstring
                ev_filename TYPE string
                ev_mimetype TYPE string
                ev_error    TYPE string.

  PRIVATE SECTION.
    DATA mv_file_no_ext TYPE string.

ENDCLASS.


CLASS zcl_xtt_open_report IMPLEMENTATION.

  METHOD make_all.
    DATA: lt_templates TYPE zcl_xtt_demo=>tt_template,
          lv_meta_err  TYPE string,
          lo_xtt       TYPE REF TO zcl_xtt,
          lv_filename  TYPE string,
          lv_filepath  TYPE string.

    FIELD-SYMBOLS: <lfs_demo>    LIKE LINE OF t_demo,
                   <ls_template> LIKE LINE OF lt_templates.

    WRITE / |{ sy-datum DATE = ENVIRONMENT } { sy-uzeit TIME = USER }|.

    DELETE t_demo WHERE ind <> '090'.
    LOOP AT t_demo ASSIGNING <lfs_demo>.
      WRITE / |Running { <lfs_demo>-ind }|.

      web_get_example_meta( EXPORTING iv_ind       = <lfs_demo>-ind
                            IMPORTING et_templates = lt_templates
                                      ev_error     = lv_meta_err ).
      IF lv_meta_err IS NOT INITIAL.
        WRITE / lv_meta_err.
        CONTINUE.
      ENDIF.

      init( iv_ind       = <lfs_demo>-ind
            iv_test_mode = abap_true
            iv_r_cnt     = 15
            iv_c_cnt     = 36
            iv_b_cnt     = 3 ).
      o_demo->set_merge_info( ).

      LOOP AT lt_templates ASSIGNING <ls_template>.
        mv_raw_folder = |{ <lfs_demo>-ind }_{ <ls_template>-objid }|.

        " Paste data
        lo_xtt = o_demo->merge( iv_template = <ls_template>-objid
                                it_merge    = t_merge[] ).

        lv_filename = <ls_template>-objid.
        TRANSLATE lv_filename TO LOWER CASE.
        REPLACE FIRST OCCURRENCE OF '-' IN lv_filename WITH '.'.
        lv_filepath = |./output/result/{ lv_filename }|.

        lo_xtt->download( EXPORTING iv_open     = abap_false
                          CHANGING  cv_fullpath = lv_filepath ).
        WRITE / |Saved { lv_filepath }|.
      ENDLOOP.
    ENDLOOP.

    CLEAR mv_raw_folder.
  ENDMETHOD.

  METHOD get_all_examples.
    DATA ls_example LIKE LINE OF rt_examples.
    FIELD-SYMBOLS <lfs_demo> LIKE LINE OF t_demo.

    CLEAR rt_examples.
    LOOP AT t_demo ASSIGNING <lfs_demo>.
      ls_example-ind  = <lfs_demo>-ind.
      ls_example-desc = <lfs_demo>-inst->v_desc.
      APPEND ls_example TO rt_examples.
    ENDLOOP.
  ENDMETHOD.

  METHOD web_get_example_meta.
    FIELD-SYMBOLS <ls_demo> LIKE LINE OF t_demo.

    CLEAR: es_opt,
           et_templates,
           ev_error.

    READ TABLE t_demo ASSIGNING <ls_demo> WITH KEY ind = iv_ind.
    IF sy-subrc <> 0.
      ev_error = |Cannot read example { iv_ind }|.
      RETURN.
    ENDIF.

    es_opt = <ls_demo>-inst->get_screen_opt( ).
    et_templates = <ls_demo>-inst->get_templates( ).
    DELETE et_templates WHERE objid CP '*-PDF' OR objid CP '*-XDP'.
  ENDMETHOD.

  METHOD web_generate.
    DATA: lo_xtt    TYPE REF TO zcl_xtt,
          lv_suffix TYPE string,
          lv_ext    TYPE string.

    CLEAR: ev_raw,
           ev_filename,
           ev_mimetype,
           ev_error.

    IF iv_r_cnt < 1 OR iv_r_cnt > 25.
      ev_error = 'mv_r_cnt must be between 1 and 25'.
      RETURN.
    ENDIF.
    IF iv_c_cnt < 1 OR iv_c_cnt > 3.
      ev_error = 'mv_c_cnt must be between 1 and 3'.
      RETURN.
    ENDIF.
    IF iv_b_cnt < 1 OR iv_b_cnt > 3.
      ev_error = 'mv_b_cnt must be between 1 and 3'.
      RETURN.
    ENDIF.

    init( iv_ind       = iv_ind
          iv_test_mode = abap_true
          iv_r_cnt     = iv_r_cnt
          iv_c_cnt     = iv_c_cnt
          iv_b_cnt     = iv_b_cnt ).
    o_demo->set_merge_info( ).

    " Paste data
    lo_xtt = o_demo->merge( iv_template = iv_template
                            it_merge    = t_merge[] ).

    " Take binary file
    ev_raw = lo_xtt->get_raw( ).

    SPLIT iv_template AT '-' INTO mv_file_no_ext lv_suffix.

    CONDENSE lv_suffix NO-GAPS.
    TRANSLATE lv_suffix TO LOWER CASE.
    lv_ext = |.{ lv_suffix }|.

    TRANSLATE mv_file_no_ext TO LOWER CASE.
    ev_filename = |{ mv_file_no_ext }{ lv_ext }|.

    CASE lv_ext.
      WHEN '.xlsx'.
        ev_mimetype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
      WHEN '.xlsm'.
        ev_mimetype = 'application/vnd.ms-excel.sheet.macroEnabled.12'.
      WHEN '.docx'.
        ev_mimetype = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'.
      WHEN '.docm'.
        ev_mimetype = 'application/vnd.ms-word.document.macroEnabled.12'.
      WHEN '.html' OR '.htm'.
        ev_mimetype = 'text/html'.
      WHEN '.xml'.
        ev_mimetype = 'application/xml'.
      WHEN OTHERS.
        ev_mimetype = 'application/octet-stream'.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

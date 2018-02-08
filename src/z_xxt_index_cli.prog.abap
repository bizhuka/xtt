*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*


CLASS cl_main IMPLEMENTATION.
  METHOD constructor.
    DATA:
      lt_obj_name   TYPE STANDARD TABLE OF tadir-obj_name,
      lv_grp        TYPE num2,
      lv_prev_grp   TYPE num2,
      lv_number     TYPE num2,
      ls_screen_opt LIKE LINE OF mt_screen_opt,
      lt_list       TYPE vrm_values,
      ls_list       TYPE REF TO vrm_value,
      lv_desc       TYPE string.
    FIELD-SYMBOLS:
      <lv_obj_name>   LIKE LINE OF lt_obj_name.

    DEFINE add_to_list.
      APPEND INITIAL LINE TO lt_list REFERENCE INTO ls_list.
      CONCATENATE lv_grp '-' lv_number INTO ls_list->key.
      lv_number = lv_number + 1.

      ls_list->text = &1.
      IF lv_desc IS NOT INITIAL.
       CONCATENATE ls_list->text ` - ` lv_desc INTO ls_list->text.
      ENDIF.
    END-OF-DEFINITION.

    " Parameter settings for Web Reporting
    SELECT obj_name INTO TABLE lt_obj_name
    FROM tadir
    WHERE pgmid    = 'R3TR'
      AND object   = 'W3MI'
      AND devclass IN ( SELECT DISTINCT devclass
                        FROM tadir
                        WHERE pgmid    = 'R3TR'
                          AND object   = 'PROG'
                          AND obj_name = 'Z_XXT_INDEX' )
    ORDER BY obj_name.

    " Fill examples
    LOOP AT lt_obj_name ASSIGNING <lv_obj_name>.
      CLEAR:
       lv_desc,
       ls_screen_opt.
      lv_grp = <lv_obj_name>+5(2).
      ls_screen_opt-template = <lv_obj_name>.

      " New example group
      IF lv_grp <> lv_prev_grp.
        lv_prev_grp = lv_grp.
        CLEAR lv_number.
        add_to_list '---------------------------------------'.
      ENDIF.

      IF <lv_obj_name> CP '*.DOC*'.
        ls_screen_opt-class_name = 'ZCL_XTT_WORD_DOCX'.
        lv_desc                  = 'Word'.

      ELSEIF <lv_obj_name> CP '*.XLS*'.
        ls_screen_opt-class_name = 'ZCL_XTT_EXCEL_XLSX'.
        lv_desc                  = 'Excel'.

      ELSEIF <lv_obj_name> CP '*WORD*.XML'.
        ls_screen_opt-class_name = 'ZCL_XTT_WORD_XML'.
        lv_desc                  = 'Word XML'.
        ls_screen_opt-show_zip = abap_true.

      ELSEIF <lv_obj_name> CP '*EXCEL*.XML'.
        ls_screen_opt-class_name = 'ZCL_XTT_EXCEL_XML'.
        lv_desc                  = 'Excel XML'.
        ls_screen_opt-show_zip = abap_true.

      ELSEIF <lv_obj_name> CP '*.PDF' OR <lv_obj_name> CP '*.XDP'.
        ls_screen_opt-class_name = 'ZCL_XTT_PDF'.
        lv_desc                  = 'Adobe PDF'.
      ENDIF.

      CASE lv_grp.
        WHEN 01.
          add_to_list 'Simple structure'(t01).

        WHEN 02.
          add_to_list 'Basic table example'(t02).
          ls_screen_opt-show_row_count = abap_true.

        WHEN 03.
          add_to_list 'Nested block'(t03).
          ls_screen_opt-show_row_count   = abap_true.
          ls_screen_opt-show_block_count = abap_true.

        WHEN 04.
          add_to_list 'Data types'(t04).

        WHEN 05.
          add_to_list 'Tree (group by fields)'(t05).
          ls_screen_opt-show_row_count   = abap_true.

        WHEN 06.
          add_to_list 'Tree (group by field relations)'(t06).

        WHEN 07.
          add_to_list 'Macro call (do not recommended)'(t07).

      ENDCASE.

      " And add
      ls_screen_opt-key = ls_list->key.
      INSERT ls_screen_opt INTO TABLE mt_screen_opt.
    ENDLOOP.

    " Update listbox
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'P_EXA'
        values = lt_list.

    " Send to current user
    SELECT SINGLE adr6~smtp_addr INTO p_email
    FROM adr6
    INNER JOIN usr21 ON usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
    WHERE usr21~bname = sy-uname.

    " First item
    p_exa = '01-00'.

    " 1010
    cl_gui_frontend_services=>get_desktop_directory(
     CHANGING
       desktop_directory = p_r_path ).
  ENDMETHOD.

  METHOD check_break_point_id.
    DATA:
      lv_exp_tstamp   TYPE aab_id_act-exp_tstamp,
      lv_date         TYPE d,
      lv_time         TYPE t,
      lv_datetime_db  TYPE char14,
      lv_datetime_now TYPE char14.

    " Activatable IDs for Breakpoints and Assertions: Activation
    SELECT SINGLE exp_tstamp INTO lv_exp_tstamp
    FROM aab_id_act
    WHERE name       = 'ZXTT_BREAK_POINT'
      AND username   = sy-uname
      AND server     = space
      AND is_program = space
      AND actdefault = space.

    " Convert to SAP
    CONVERT TIME STAMP lv_exp_tstamp TIME ZONE sy-zonlo
      INTO DATE lv_date TIME lv_time.

    " For comaparison
    CONCATENATE lv_date  lv_time  INTO lv_datetime_db.
    CONCATENATE sy-datum sy-uzeit INTO lv_datetime_now.

    CHECK lv_datetime_now > lv_datetime_db.
    MESSAGE 'Activate ZXTT_BREAK_POINT in tr. SAAB'(tbr) TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD pbo.
    DATA:
      ls_screen_opt TYPE ty_screen_opt,
      l_show        TYPE abap_bool,
      lv_proxy_app  TYPE string,
      lv_ext        TYPE string.

    " Show or hide controls
    READ TABLE mt_screen_opt INTO ls_screen_opt
     WITH TABLE KEY key = p_exa.

    LOOP AT SCREEN.
      IF ls_screen_opt IS INITIAL.
        CHECK screen-group1 <> 'EXA' AND screen-group1 IS NOT INITIAL.
        l_show = abap_false.
      ELSE.
        l_show = abap_true.

        CASE screen-group1.
          WHEN 'DWN'. " Additional parameters for `download` method
            IF p_stru = abap_true OR p_dwnl <> abap_true.
              l_show = abap_false.
            ENDIF.

          WHEN 'SND'. " Additional parameters for `send` method
            IF p_stru = abap_true OR p_send <> abap_true.
              l_show = abap_false.
            ENDIF.

          WHEN 'RES'. " Last index is the documentation
*          IF p_exa = .
*            l_show = p_temp = p_stru = abap_false.
*          ENDIF.

          WHEN 'MET'. " Dont'show data, just data structure
            IF p_stru = abap_true.
              l_show = abap_false.
            ELSEIF screen-name CP '*P_SHOW*'.
              zcl_xtt_util=>get_ole_info(
               EXPORTING
                 iv_class_name = ls_screen_opt-class_name
               CHANGING
                 cv_proxy_app  = lv_proxy_app
                 cv_file_ext   = lv_ext ).

              l_show = boolc( lv_proxy_app IS NOT INITIAL OR lv_ext IS NOT INITIAL ).
              IF l_show <> abap_true.
                p_show = abap_false.
              ENDIF.
            ENDIF.

          WHEN OTHERS.
            IF screen-name CP '*P_R_CNT*'.
              l_show = ls_screen_opt-show_row_count.
            ELSEIF screen-name CP '*P_B_CNT*'.
              l_show = ls_screen_opt-show_block_count.
            ELSEIF screen-name CP '*P_ZIP*'.
              l_show = ls_screen_opt-show_zip.
            ELSE.
              CONTINUE.
            ENDIF.
        ENDCASE.
      ENDIF.

      " Show or hide paramater
      IF l_show = abap_true.
        screen-invisible = '0'.
        screen-input     = '1'.
      ELSE.
        screen-invisible = '1'.
        screen-input     = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai.

  ENDMETHOD.

  METHOD start_of_selection.
    DATA:
      ls_screen_opt TYPE REF TO ty_screen_opt,
      lv_meth       TYPE string,
      lt_recipient  TYPE rmps_recipient_bcs,
      lo_err        TYPE REF TO cx_address_bcs,
      lv_text       TYPE string,
      lo_xtt        TYPE REF TO zcl_xtt.

    " Current example
    READ TABLE mt_screen_opt REFERENCE INTO ls_screen_opt
     WITH TABLE KEY key = p_exa.

    IF sy-subrc <> 0.
      MESSAGE 'Please select example'(pse) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Get parameters for merge method
    CONCATENATE 'EXAMPLE_' ls_screen_opt->key(2) INTO lv_meth.
    CALL METHOD me->(lv_meth)
      EXPORTING
        iv_class_name = ls_screen_opt->class_name
        iv_template   = ls_screen_opt->template
      RECEIVING
        ro_xtt        = lo_xtt.

    " Call respective method
    CHECK lo_xtt IS NOT INITIAL.
    CASE 'X'.
      WHEN p_dwnl.
        lo_xtt->download(        " All parameters are optional
         EXPORTING
          iv_open     = p_open
          iv_zip      = p_zip
         CHANGING
          cv_fullpath = p_path ).

      WHEN p_show.
        lo_xtt->show( ).

      WHEN p_send.
        " Add recipients
        TRY.
            IF p_user IS NOT INITIAL.
              APPEND cl_sapuser_bcs=>create( p_user ) TO lt_recipient.
            ENDIF.

            IF p_email IS NOT INITIAL.
              APPEND cl_cam_address_bcs=>create_internet_address( p_email ) TO lt_recipient.
            ENDIF.
          CATCH cx_address_bcs INTO lo_err.
            lv_text = lo_err->if_message~get_text( ).
            MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.
        CHECK lt_recipient IS NOT INITIAL.

        lo_xtt->send(
         it_recipients = lt_recipient
         iv_subject    = p_title
         iv_body       = p_text ).
    ENDCASE.
  ENDMETHOD.

  METHOD f4_full_path.
    DATA:
      lv_fullpath TYPE string,
      lv_filename TYPE string,
      lv_path     TYPE string,
      lv_result   TYPE i.

    lv_fullpath = cv_fullpath.
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title    = iv_title
      CHANGING
        filename    = lv_filename
        path        = lv_path
        fullpath    = lv_fullpath
        user_action = lv_result
      EXCEPTIONS
        OTHERS      = 1 ).
    CHECK sy-subrc = 0 AND lv_result = cl_gui_frontend_services=>action_ok.
    cv_fullpath = lv_fullpath.
  ENDMETHOD.

  METHOD f4_dir_browse.
    DATA:
      lv_path        TYPE string,
      lv_init_folder TYPE string.

    lv_init_folder = cv_path.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title   = iv_title
        initial_folder = lv_init_folder
      CHANGING
        selected_folder = lv_path
      EXCEPTIONS
        OTHERS          = 1 ).
    CHECK sy-subrc = 0 AND lv_path IS NOT INITIAL.
    cv_path = lv_path.
  ENDMETHOD.

  METHOD get_random_table.
    DATA:
      lo_rand_i TYPE REF TO cl_abap_random_int,
      lo_rand_p TYPE REF TO cl_abap_random_packed,
      lv_int    TYPE i.
    FIELD-SYMBOLS:
     <ls_item> LIKE LINE OF rt_table.

    " A,B,C chars
    lo_rand_i = cl_abap_random_int=>create( min = 0 max = 2 ).
    " SUMS
    lo_rand_p = cl_abap_random_packed=>create( min = 0 max = 1000000 ).
    DO p_r_cnt TIMES.
      APPEND INITIAL LINE TO rt_table ASSIGNING <ls_item>.

      " Special XML symbols <>
      <ls_item>-caption = sy-index.
      CONCATENATE `<Caption ` <ls_item>-caption `/>` INTO <ls_item>-caption.

      " Date
      lv_int = lo_rand_i->get_next( ).
      <ls_item>-date = sy-datum - lv_int.

      " 3 different groups
      lv_int = lv_int + 65.
      <ls_item>-group = cl_abap_conv_in_ce=>uccpi( lv_int ).
      CONCATENATE `GRP ` <ls_item>-group INTO <ls_item>-group.

      " And finally sums
**********************************************************************
      " in Word and pdf (except Excel formats), 'P' type always has dot as a delimiter
      " If 'N' type has conversion exit it will transformed to mask type
      " Use ;type=mask addition in template for using WRITE ... TO
**********************************************************************
      <ls_item>-sum1 = lo_rand_p->get_next( ). " / 100
      <ls_item>-sum1 = <ls_item>-sum1 / 100.

      <ls_item>-sum2 = lo_rand_p->get_next( ). " / 100
      <ls_item>-sum2 = <ls_item>-sum2 / 100.
    ENDDO.
  ENDMETHOD.

  INCLUDE z_xxt_index_exa_01.
  INCLUDE z_xxt_index_exa_02.
  INCLUDE z_xxt_index_exa_03.
  INCLUDE z_xxt_index_exa_04.
  INCLUDE z_xxt_index_exa_05.
  INCLUDE z_xxt_index_exa_06.
  INCLUDE z_xxt_index_exa_07.

ENDCLASS.

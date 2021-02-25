*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_demo IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = ''.
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = ''.
  ENDMETHOD.

  METHOD get_screen_opt.
    " Hide all by default
    CLEAR rs_opt.
  ENDMETHOD.

  METHOD _merge.
    FIELD-SYMBOLS: <ls_merge> LIKE LINE OF it_merge,
                   <ls_root>  TYPE any.

    " Double click here --> it_merge[] <--
    IF p_stop = abap_true.
      BREAK-POINT ID zxtt_break_point.
    ENDIF.

    LOOP AT it_merge ASSIGNING <ls_merge>.
      ASSIGN <ls_merge>-val->* TO <ls_root>.

      io_xtt->merge( is_block      = <ls_root>
                     iv_block_name = <ls_merge>-key " <--- 'R' by defualt
                    ).
    ENDLOOP.
  ENDMETHOD.

  METHOD template.
    DATA: lv_template TYPE string, lo_file TYPE REF TO zif_xtt_file.
    lv_template = _get_template_by_f4( ).

    get_from_template( EXPORTING iv_template = lv_template
                       IMPORTING eo_file     = lo_file ).
    download_template( lo_file ).
  ENDMETHOD.

  METHOD show.
    DATA lv_template TYPE string.
    lv_template = _get_template_by_f4( ).

    DATA: lo_xtt TYPE REF TO zcl_xtt.
    get_from_template( EXPORTING iv_template = lv_template
                       IMPORTING eo_xtt      = lo_xtt ).
    CHECK lo_xtt IS NOT INITIAL.

    " Paste data
    _merge( io_xtt   = lo_xtt
            it_merge = it_merge[] ).

    " For PAI & PBO events
    lo_xtt->show( io_handler = me ).
  ENDMETHOD.

  METHOD download.
    DATA: lv_template TYPE string, lo_xtt TYPE REF TO zcl_xtt.

    IF iv_template IS NOT INITIAL.
      lv_template = iv_template.
    ELSE.
      " Prepare screen '1010'
      CLEAR p_path.
      lv_template = _show_screen( iv_dynnr = '1010'
                                  iv_lb_id = 'P_DTEMPL' ).
    ENDIF.

    get_from_template( EXPORTING iv_template = lv_template
                       IMPORTING eo_xtt      = lo_xtt ).
    CHECK lo_xtt IS NOT INITIAL.

    " Stop befor merge
    IF p_stop = abap_true.
      _is_break_point_active( ).
    ENDIF.

    IF io_injection IS NOT INITIAL.
      io_injection->prepare( lo_xtt ).
    ENDIF.

    " Paste data
    _merge( io_xtt   = lo_xtt
            it_merge = it_merge[] ).

    _do_download( lo_xtt ).
  ENDMETHOD.

  METHOD _do_download.
    IF p_open = abap_true.
      " All parameters are optional
      io_xtt->download( EXPORTING iv_zip      = p_zip
                        CHANGING  cv_fullpath = p_path ).
      RETURN.
    ENDIF.

    io_xtt->download( EXPORTING iv_open     = p_open " Could be ZCL_XTT=>MC_BY-OLE.  @see N070!
                                iv_zip      = p_zip
                      CHANGING  cv_fullpath = p_path ).
  ENDMETHOD.

  METHOD send.
    DATA: lv_template TYPE string, lo_xtt TYPE REF TO zcl_xtt.

    " Prepare screen '1020'. Send to current user
    SELECT SINGLE adr6~smtp_addr INTO p_email
    FROM adr6
    INNER JOIN usr21 ON usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
    WHERE usr21~bname = sy-uname.                       "#EC CI_NOORDER

    lv_template = _show_screen( iv_dynnr = '1020'
                                iv_lb_id = 'P_STEMPL' ).
    get_from_template( EXPORTING iv_template = lv_template
                       IMPORTING eo_xtt      = lo_xtt ).
    CHECK lo_xtt IS NOT INITIAL.

    " Paste data
    _merge( io_xtt   = lo_xtt
            it_merge = it_merge[] ).

    _send_email( lo_xtt ).
  ENDMETHOD.

  METHOD _send_email.
    DATA lo_recipient  TYPE REF TO if_recipient_bcs.
    DATA lt_recipient  TYPE rmps_recipient_bcs.
    DATA lo_err        TYPE REF TO cx_address_bcs.

    " Add recipients
    TRY.
        IF p_user IS NOT INITIAL.
          lo_recipient = cl_sapuser_bcs=>create( p_user ).
          APPEND lo_recipient TO lt_recipient.
        ENDIF.

        IF p_email IS NOT INITIAL.
          lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ).
          APPEND lo_recipient TO lt_recipient.
        ENDIF.
      CATCH cx_address_bcs INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    CHECK lt_recipient IS NOT INITIAL.
    io_xtt->send(
     it_recipients = lt_recipient
     iv_subject    = p_title
     iv_body       = p_text ).
  ENDMETHOD.

  METHOD _show_screen.
    DATA lo_screen  TYPE REF TO zcl_eui_screen.
    DATA lo_error   TYPE REF TO zcx_eui_exception.
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr = iv_dynnr.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Prepare scrren
    DATA lt_listbox TYPE tt_vrm_value.
    lt_listbox = _get_template_lisbox( ).
    lo_screen->customize( name       = iv_lb_id
                          it_listbox = lt_listbox
                          required   = '1' ).

    " Listbox parameter
    FIELD-SYMBOLS: <lv_template> TYPE vrm_value-key, <ls_template> LIKE LINE OF lt_listbox.
    ASSIGN (iv_lb_id) TO <lv_template>.

    " Set 1st by default
    READ TABLE lt_listbox ASSIGNING <ls_template> INDEX 1.
    IF sy-subrc = 0.
      <lv_template> = <ls_template>-key.
    ENDIF.

    " Show popup
    lo_screen->popup( iv_col_end = 116 ).
    CHECK lo_screen->show( ) = 'OK'.

    " Return template from screen
    rv_template = <lv_template>.
  ENDMETHOD.

  METHOD _get_template_lisbox.
    DATA lt_template TYPE tt_template.
    lt_template = get_templates( ).
    CHECK lt_template IS NOT INITIAL.

    " Set KEY
    DATA lr_template TYPE REF TO ts_template.
    LOOP AT lt_template REFERENCE INTO lr_template.
      DATA lr_listbox TYPE REF TO ts_vrm_value.
      APPEND INITIAL LINE TO rt_listbox REFERENCE INTO lr_listbox.

      lr_listbox->key = lr_template->objid.
    ENDLOOP.

    DATA lt_wwwdata TYPE STANDARD TABLE OF wwwdata WITH DEFAULT KEY.
    SELECT DISTINCT objid text INTO CORRESPONDING FIELDS OF TABLE lt_wwwdata "#EC TOO_MANY_ITAB_FIELDS
    FROM wwwdata
    FOR ALL ENTRIES IN lt_template
    WHERE relid = 'MI'
      AND objid = lt_template-objid.

    " Set text
    LOOP AT rt_listbox REFERENCE INTO lr_listbox.
      DATA lr_wwwdata TYPE REF TO wwwdata.
      READ TABLE lt_wwwdata REFERENCE INTO lr_wwwdata
       WITH KEY objid = lr_listbox->key.
      CHECK sy-subrc = 0.

      lr_listbox->text = lr_wwwdata->text.

      " Add info from template extension
      DATA lv_type TYPE string.
      get_from_template( EXPORTING iv_template = lr_listbox->key
                         IMPORTING ev_type     = lv_type ).
      CONCATENATE lr_listbox->text ` - ` lv_type INTO lr_listbox->text.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_from_template.
    CLEAR: eo_file, eo_xtt, ev_type.
    CHECK iv_template IS NOT INITIAL.

    DATA lv_class TYPE string.
    IF iv_template CP '*-DOC*'.
      lv_class = 'ZCL_XTT_WORD_DOCX'.
      ev_type  = 'Word'.                                    "#EC NOTEXT
    ELSEIF iv_template CP '*-XLS*'.
      lv_class = 'ZCL_XTT_EXCEL_XLSX'.
      ev_type  = 'Excel'.                                   "#EC NOTEXT
    ELSEIF iv_template CP '*WORD*-XML'.
      lv_class = 'ZCL_XTT_WORD_XML'.
      ev_type  = 'Word XML'.                                "#EC NOTEXT
    ELSEIF iv_template CP '*EXCEL*-XML'.
      lv_class = 'ZCL_XTT_EXCEL_XML'.
      ev_type  = 'Excel XML'.                               "#EC NOTEXT
    ELSEIF iv_template CP '*-PDF' OR iv_template CP '*-XDP'.
      lv_class = 'ZCL_XTT_PDF'.
      ev_type  = 'Adobe PDF'.                               "#EC NOTEXT
    ELSEIF iv_template CP '*-HTM*'.
      lv_class = 'ZCL_XTT_HTML'.
      ev_type  = 'Html'.                                    "#EC NOTEXT
    ENDIF.

    " SMW0 reader
    CHECK eo_file IS REQUESTED OR eo_xtt IS REQUESTED.
    CREATE OBJECT eo_file TYPE zcl_xtt_file_smw0
      EXPORTING
        iv_objid = iv_template.

    CHECK eo_xtt IS REQUESTED.
    CREATE OBJECT eo_xtt TYPE (lv_class)
      EXPORTING
        io_file = eo_file.
  ENDMETHOD.

  METHOD on_user_command.
  ENDMETHOD.

  METHOD _get_template_by_f4.
    DATA lt_template TYPE tt_template.
    lt_template = get_templates( ).

    " No exmples ?
    CHECK lt_template IS NOT INITIAL.

    " No need to show SH
    IF lines( lt_template ) = 1.
      READ TABLE lt_template INTO rv_template INDEX 1.
      RETURN.
    ENDIF.

    DATA lt_template_txt TYPE tt_vrm_value.
    lt_template_txt = _get_template_lisbox( ).

    " Show dialog
    DATA lt_return    TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.
    DATA lr_return    TYPE REF TO ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'KEY'
        value_org  = 'S'
      TABLES
        value_tab  = lt_template_txt
        return_tab = lt_return
      EXCEPTIONS
        OTHERS     = 3.
    CHECK sy-subrc = 0.

    READ TABLE lt_return REFERENCE INTO lr_return INDEX 1.
    CHECK sy-subrc = 0.

    rv_template = lr_return->fieldval.
  ENDMETHOD.

  METHOD download_template.
    CHECK io_file IS NOT INITIAL.

    " Get file content
    DATA lv_content TYPE xstring.
    io_file->get_content( IMPORTING ev_as_xstring = lv_content ).

    DATA lv_file_name TYPE string.
    lv_file_name = iv_file_name.
    IF lv_file_name IS INITIAL.
      lv_file_name = io_file->get_name( ).
    ENDIF.

    " Initilize
    DATA lo_file TYPE REF TO zcl_eui_file.
    CREATE OBJECT lo_file
      EXPORTING
        iv_xstring = lv_content.

    DATA lo_error TYPE REF TO zcx_eui_exception.
    TRY.
        lo_file->download( iv_full_path = lv_file_name ).

        " Open template
        CHECK iv_file_name IS INITIAL.
        lo_file->open( ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _make_string_message.
    CREATE DATA rr_message.
    rr_message->* = iv_info.
  ENDMETHOD.

  METHOD _is_break_point_active.
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
    MESSAGE 'Activate "ZXTT_BREAK_POINT" in tr. SAAB'(tbr) TYPE 'I'. " DISPLAY LIKE 'E'.
  ENDMETHOD.
ENDCLASS.

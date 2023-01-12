class ZCL_XTT definition
  public
  abstract
  create protected

  global friends ZCL_XTT_REPLACE_BLOCK
                 ZCL_XTT_SCOPE .

public section.
*"* public components of class ZCL_XTT
*"* do not include other source files here!!!
  type-pools ABAP .
  type-pools OLE2 .
  class ZCL_XTT_COND definition load .

  interfaces ZIF_XTT .

  aliases MC_BY
    for ZIF_XTT~MC_BY .
  aliases DOWNLOAD
    for ZIF_XTT~DOWNLOAD .
  aliases GET_RAW
    for ZIF_XTT~GET_RAW .
  aliases MERGE
    for ZIF_XTT~MERGE .
  aliases SEND
    for ZIF_XTT~SEND .
  aliases SHOW
    for ZIF_XTT~SHOW .
  aliases TS_RECIPIENT_BCS
    for ZIF_XTT~TS_RECIPIENT_BCS .
  aliases TT_RECIPIENTS_BCS
    for ZIF_XTT~TT_RECIPIENTS_BCS .

  types TS_BOUNDS type ZSS_XTT_BOUNDS .

  events PREPARE_RAW
    exporting
      value(IV_PATH) type STRING
      value(IR_CONTENT) type ref to XSTRING .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_OLE_EXT type STRING optional .
  methods ADD_RAW_EVENT
    importing
      !IV_PATH type CSEQUENCE .
  methods ADD_LOG_MESSAGE
    importing
      !IV_SYST type ABAP_BOOL optional
      !IV_TEXT type CSEQUENCE optional
      !IO_EXCEPTION type ref to CX_ROOT optional
      !IV_MSGTY type SYMSGTY optional .
  methods GET_CALLER_INFO
    changing
      !CS_MATCH type ZCL_XTT_COND=>TS_MATCH .
protected section.

  types:
    BEGIN OF to_scope,
      sc_id TYPE string,
      scope TYPE REF TO zcl_xtt_scope,
    END OF to_scope .
  types:
    tt_scopes TYPE SORTED TABLE OF to_scope WITH UNIQUE KEY sc_id .
  types:
    BEGIN OF ts_log_xml,
      rows    TYPE STRING,
      title   TYPE STRING,
      before  TYPE STRING,
      after   TYPE STRING,
      has_axe TYPE abap_bool,
    END OF ts_log_xml .

  data MO_FILE type ref to ZIF_XTT_FILE .
  data MV_FILE_NAME type STRING .
  data MV_OLE_EXT type STRING .
  data MV_SKIP_TAGS type ABAP_BOOL .
  data MS_LOG_PROFILE type BAL_S_PROF .
  data _LOGGER type ref to ZCL_EUI_LOGGER .
  data _SCOPES type TT_SCOPES .

  methods BOUNDS_SPLIT
    importing
      !IV_NAME type CSEQUENCE
    changing
      !CS_BOUNDS type TS_BOUNDS
      !CV_MIDDLE type CSEQUENCE .
  methods RAISE_RAW_EVENTS
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP .
  methods ON_MATCH_FOUND
  abstract
    importing
      !IS_FIELD type ref to ZSS_XTT_FIELD
      value(IV_POS_BEG) type I
      value(IV_POS_END) type I
    changing
      !CV_CONTENT type STRING .
  methods _GET_SCOPE
    importing
      !IO_BLOCK type ref to ZCL_XTT_REPLACE_BLOCK
      !IV_FORCE type ABAP_BOOL
    exporting
      !EV_NEW type ABAP_BOOL
      !EO_SCOPE type ref to ZCL_XTT_SCOPE .
  methods _LOGGER_AS_XML
    importing
      !IV_ROW type STRING optional
    returning
      value(RS_LOG_XML) type TS_LOG_XML .
private section.

*"* private components of class ZCL_XTT
*"* do not include other source files here!!!
  data MT_RAW_EVENT type STRINGTAB .
  data MO_HELPER type ref to OBJECT .
  data MO_DEBUG type ref to ZCL_XTT_DEBUG .
  data MV_IS_PROD type ABAP_BOOL .
  data MV_CAN_SHOW_MENU type ABAP_BOOL .

  methods _LOGGER_INIT .
ENDCLASS.



CLASS ZCL_XTT IMPLEMENTATION.


METHOD add_log_message.
  IF iv_syst = abap_true.
    _logger->add( iv_msgty = iv_msgty ).
    RETURN.
  ELSEIF iv_text IS NOT INITIAL.
    _logger->add_text( iv_text  = iv_text
                       iv_msgty = iv_msgty ).
    RETURN.
  ENDIF.

  CHECK io_exception IS NOT INITIAL.
  TRY.
      DATA lo_no_check TYPE REF TO zcx_eui_no_check.
      lo_no_check ?= io_exception.

      " Based on message class ?
      IF lo_no_check->ms_msg IS NOT INITIAL.
        _logger->add( is_msg   = lo_no_check->ms_msg
                      iv_msgty = iv_msgty ).
        RETURN.
      ENDIF.
    CATCH cx_sy_move_cast_error.                        "#EC NO_HANDLER
  ENDTRY.

  " Ordinary exception
  " in 7.00 cannot read texts
*  _logger->add_exception( io_exception = io_exception
*                          iv_msgty     = iv_msgty ).
  DATA lv_msg TYPE string.
  lv_msg = io_exception->get_text( ).
  _logger->add_text( iv_text  = lv_msg
                     iv_msgty = 'E' ).
ENDMETHOD.


METHOD add_raw_event.
  APPEND iv_path TO mt_raw_event.
ENDMETHOD.


METHOD bounds_split.
  CLEAR: cs_bounds-pos_beg,
         cs_bounds-pos_end,
         cs_bounds-pos_cnt,
         " Resulting fields
         cs_bounds-text_before,
         cs_bounds-text_after.

  " 1 - Text before body
  FIELD-SYMBOLS <ls_match>   LIKE LINE OF cs_bounds-t_match.
  READ TABLE cs_bounds-t_match ASSIGNING <ls_match> INDEX cs_bounds-first_match.

  " TODO silent mode ?
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF cs_bounds-first_match >= cs_bounds-last_match.
    MESSAGE e009(zsy_xtt) WITH iv_name INTO sy-msgli.
    add_log_message( iv_syst = abap_true ).
    RETURN.
  ENDIF.

  " Does need an open tag?
  IF cs_bounds-with_tag = abap_true.
    cs_bounds-pos_beg = <ls_match>-offset.
  ELSE.
    cs_bounds-pos_beg = <ls_match>-offset + <ls_match>-length.
  ENDIF.
  cs_bounds-text_before = cv_middle(cs_bounds-pos_beg).

*************
  " 2 - Text after body
  READ TABLE cs_bounds-t_match ASSIGNING <ls_match> INDEX cs_bounds-last_match.

  IF cs_bounds-with_tag = abap_true.
    cs_bounds-pos_end = <ls_match>-offset + <ls_match>-length.
  ELSE.
    cs_bounds-pos_end = <ls_match>-offset.
  ENDIF.
  cs_bounds-text_after = cv_middle+cs_bounds-pos_end.

*************
  " 3 - Body
  cs_bounds-pos_cnt = cs_bounds-pos_end - cs_bounds-pos_beg.
  cv_middle = cv_middle+cs_bounds-pos_beg(cs_bounds-pos_cnt).
ENDMETHOD.


METHOD constructor.
  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  _logger_init( ).

  mo_file    = io_file.
  mv_ole_ext = iv_ole_ext.
  TRY.
      mv_file_name = mo_file->get_name( ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      add_log_message( io_exception = lo_no_check ).
  ENDTRY.

  " Export MERGE ?
  DATA lv_debug TYPE abap_bool VALUE abap_false.
  DATA lv_cprog TYPE sycprog.

  lv_cprog = sy-cprog.
  PERFORM in_debug IN PROGRAM ('Z_XTT_DEBUG') IF FOUND USING    lv_cprog
                                                       CHANGING lv_debug.
  CHECK lv_debug = abap_true.
  CREATE OBJECT mo_debug.
  mo_debug->save_template( io_file ).
ENDMETHOD.


METHOD get_caller_info.
  cs_match-caller = mo_helper.

  IF cs_match-caller IS INITIAL.
    DATA lv_message TYPE string.
    add_log_message( iv_text  = `Please pass IO_HELPER to MERGE( ) method`
                     iv_msgty = 'E' ).
    RETURN.
  ENDIF.

**********************************************************************
* method( ) in string
**********************************************************************
  DATA: lv_beg TYPE i, lv_end TYPE i.
  IF cs_match-cond CS '('.
    lv_beg = sy-fdpos.
  ENDIF.
  IF cs_match-cond CP '*)'.
    lv_end = sy-fdpos.
  ENDIF.

  IF lv_beg IS INITIAL OR lv_end IS INITIAL.
    CONCATENATE `No method call in "` cs_match-cond `"` INTO lv_message.
    add_log_message( iv_text  = lv_message
                     iv_msgty = 'E' ).
    RETURN.
  ENDIF.

**********************************************************************
* serach method in IO_HELPER
**********************************************************************
  DATA lo_object TYPE REF TO cl_abap_objectdescr.
  lo_object ?= cl_abap_objectdescr=>describe_by_object_ref( cs_match-caller ).

  DATA lv_method TYPE string.
  lv_method = cs_match-cond(lv_beg).
  TRANSLATE lv_method TO UPPER CASE.

  FIELD-SYMBOLS <ls_method> LIKE LINE OF lo_object->methods.
  READ TABLE lo_object->methods ASSIGNING <ls_method>
   WITH KEY name = lv_method.
  IF sy-subrc <> 0.
    CONCATENATE `No method "` lv_method `" in IO_HELPER` INTO lv_message.
    add_log_message( iv_text  = lv_message
                     iv_msgty = 'E' ).
    RETURN.
  ENDIF.

**********************************************************************
* Returning parameter
**********************************************************************
  FIELD-SYMBOLS <ls_param> LIKE LINE OF <ls_method>-parameters.
  READ TABLE <ls_method>-parameters ASSIGNING <ls_param>
   WITH KEY parm_kind = lo_object->returning.
  IF sy-subrc <> 0.
    CONCATENATE `No returning parameter in method "` lv_method `"` INTO lv_message.
    add_log_message( iv_text  = lv_message
                     iv_msgty = 'E' ).
    RETURN.
  ENDIF.
  cs_match-type = zcl_xtt_replace_block=>get_simple_type( <ls_param>-type_kind ).

  DATA lv_code_line TYPE string.
  CONCATENATE ` RECEIVING ` <ls_param>-name ` = result` INTO lv_code_line.
  CONCATENATE cs_match-cond(lv_end) lv_code_line INTO cs_match-cond.

**********************************************************************
* Pass optional IS_ROOT parameter
**********************************************************************
  CONCATENATE `CALL METHOD _caller->('` lv_method `') EXPORTING` INTO lv_code_line.
  READ TABLE <ls_method>-parameters TRANSPORTING NO FIELDS
   WITH KEY name      = `IS_ROOT`
            parm_kind = lo_object->importing.
  IF sy-subrc = 0.
    CONCATENATE lv_code_line ` IS_ROOT = root ` INTO lv_code_line.
  ENDIF.

  CONCATENATE lv_method `(` INTO lv_method.
  REPLACE FIRST OCCURRENCE OF lv_method IN cs_match-cond WITH lv_code_line IGNORING CASE.
ENDMETHOD.


METHOD raise_raw_events.
  DATA lv_path    TYPE string.
  DATA lv_content TYPE xstring.
  DATA lr_content TYPE REF TO xstring.

  LOOP AT mt_raw_event INTO lv_path.
    io_zip->get( EXPORTING name    = lv_path
                 IMPORTING content = lv_content
                 EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc = 0.

    GET REFERENCE OF lv_content INTO lr_content.
    RAISE EVENT prepare_raw
     EXPORTING
       iv_path    = lv_path
       ir_content = lr_content.

    " Replace XML file
    zcl_eui_conv=>xml_to_zip(
     io_zip  = io_zip
     iv_name = lv_path
     iv_xdoc = lv_content ).
  ENDLOOP.
ENDMETHOD.


METHOD zif_xtt~download.
  DATA lv_filename  TYPE string.
  DATA lv_no_ext    TYPE string.
  DATA lv_ext       TYPE string.
  DATA lv_path      TYPE string.
  DATA lo_zip       TYPE REF TO cl_abap_zip.
  " New functionality
  DATA lo_eui_file  TYPE REF TO zcl_eui_file.
  DATA lo_eui_error TYPE REF TO zcx_eui_exception.
  DATA lv_visible   TYPE abap_bool.

  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  TRY.
      get_no_warning mv_can_show_menu.

      " As a xstring (implemented in subclasses)
      DATA lv_content   TYPE xstring.
      lv_content = get_raw( iv_no_warning = lv_no_warning ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      add_log_message( io_exception = lo_no_check ).
  ENDTRY.

  " Use existing path
  DATA lv_fullpath TYPE string.
  lv_fullpath = cv_fullpath.
  " Just name without path
  IF lv_fullpath IS INITIAL.
    cv_fullpath = lv_fullpath = mv_file_name.
  ENDIF.

  " Could be file name
  zcl_eui_file=>split_file_path(
    EXPORTING
      iv_fullpath       = lv_fullpath
    IMPORTING
      ev_filename       = lv_filename
      ev_file_noext     = lv_no_ext
      ev_extension      = lv_ext
      ev_path           = lv_path ).

  " For plain XMLs only (meaningless for docx & xlsx)
  IF iv_zip = abap_true AND lv_content IS NOT INITIAL.
    lv_ext = 'zip'.
    CREATE OBJECT lo_zip.

    zcl_eui_conv=>xml_to_zip(
     io_zip  = lo_zip
     iv_name = lv_filename
     iv_xdoc = lv_content ).

    " And compress
    lv_content = lo_zip->save( ).
  ENDIF.

  " Create fullpath again
  CONCATENATE lv_path lv_no_ext `.` lv_ext INTO lv_fullpath.
  cv_fullpath = lv_fullpath.

  IF mv_ole_ext IS NOT INITIAL.
    lv_ext = mv_ole_ext.
  ENDIF.

  " delegate to new class
  DO 1 TIMES.
    CHECK lv_content IS NOT INITIAL.
    TRY.
        CREATE OBJECT lo_eui_file
          EXPORTING
            iv_xstring   = lv_content
            iv_file_name = lv_ext.

        " Download to app server
        IF iv_open = mc_by-app_server.
          lo_eui_file->to_app_server( iv_full_path = lv_fullpath ).
          EXIT.
        ENDIF.

        lo_eui_file->download(
         iv_full_path   = lv_fullpath
         iv_save_dialog = abap_false ).

        " Where saved
        cv_fullpath = lv_fullpath = lo_eui_file->get_full_path( ).

        " And open file
        CHECK iv_open IS NOT INITIAL AND
              zcl_xtt_util=>is_common_gui( ) = abap_true.

        " Open ?
        CASE iv_open.
          WHEN zcl_xtt=>mc_by-ole.
            lv_visible = abap_true.

          WHEN zcl_xtt=>mc_by-ole_hide.
            lv_visible = abap_false.

          WHEN OTHERS.
            lv_visible = abap_undefined.
        ENDCASE.

        " Just open
        IF  lv_visible = abap_undefined OR
          ( iv_open IS NOT INITIAL AND iv_zip = abap_true ). " Open as zip
          lo_eui_file->open( ).
          EXIT.
        ENDIF.

        " Meaningless
        CHECK iv_zip <> abap_true.

        lo_eui_file->open_by_ole(
         EXPORTING
           iv_visible = lv_visible
         CHANGING
           cv_ole_app = cv_ole_app
           cv_ole_doc = cv_ole_doc ).
      CATCH zcx_eui_exception INTO lo_eui_error.
        add_log_message( io_exception = lo_eui_error ).
        MESSAGE lo_eui_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDDO.

  " And finally show logs
  _logger->show_as_button( iv_write_message = 'Please read logs for finding issues'(prl)
                           is_profile       = ms_log_profile ).
  CHECK _logger->has_messages( iv_msg_types = zcl_eui_logger=>mc_msg_types-error ) = abap_true.
  _logger->show( iv_profile = zcl_eui_logger=>mc_profile-popup
                 is_profile = ms_log_profile ).
ENDMETHOD.


METHOD zif_xtt~get_raw.
  zcx_xtt_exception=>raise_dump( iv_message = 'Implementation is missing' ).
ENDMETHOD.


METHOD zif_xtt~merge.
  " For chain calls
  ro_xtt = me.

  " For each merge
  CLEAR _scopes.

  mo_helper = io_helper.

  " Delegate to another class
  CHECK mo_debug IS NOT INITIAL.
  mo_debug->save_merge( is_block      = is_block
                        iv_block_name = iv_block_name ).
ENDMETHOD.


METHOD zif_xtt~send.
  DATA:
    lo_mail      TYPE REF TO cl_bcs,
    lo_doc       TYPE REF TO cl_document_bcs,
    lo_recipient TYPE REF TO if_recipient_bcs,
    lt_header    TYPE soli_tab,
    ls_header    TYPE REF TO soli,
    lv_subject   TYPE sood-objdes,
    lv_ext       TYPE soodk-objtp,
    lv_file_size TYPE i,
    lt_data      TYPE solix_tab,
    lo_error     TYPE REF TO cx_bcs,
    lv_value     TYPE xstring,
    lv_filename  TYPE string.
  FIELD-SYMBOLS:
    <ls_recipient> LIKE LINE OF it_recipients_bcs.

  TRY.
      " Request
      lo_mail = cl_bcs=>create_persistent( ).

      " Body
      DATA: lv_body TYPE string,
            lt_body TYPE soli_tab, " SOLIX_TAB
            lv_len  TYPE i.
      lv_body = iv_body.
      zcl_eui_conv=>string_to_text_table( EXPORTING iv_string = lv_body
                                          IMPORTING et_text   = lt_body
                                                    ev_length = lv_len ).
      DATA lv_size TYPE sood-objlen.
      lv_size = lv_len.
      lo_doc  = cl_document_bcs=>create_document(
       i_type    = 'HTM'
       i_text    = lt_body  " I_HEX
       i_length  = lv_size
       i_subject = iv_subject ).

      " № 1 - Add recipients
      LOOP AT it_recipients INTO lo_recipient.
        lo_mail->add_recipient( i_recipient = lo_recipient
                                i_express   = iv_express ).
      ENDLOOP.

      " № 2 - Add recipients
      LOOP AT it_recipients_bcs ASSIGNING <ls_recipient>.
        lo_mail->add_recipient(
          i_recipient  = <ls_recipient>-recipient
          i_express    = <ls_recipient>-express
          i_copy       = <ls_recipient>-copy
          i_blind_copy = <ls_recipient>-blind_copy
          i_no_forward = <ls_recipient>-no_forward ).
      ENDLOOP.

      " Set to null in children to avoid sending attachment
      IF mv_file_name IS NOT INITIAL.
        " File name
        APPEND INITIAL LINE TO lt_header REFERENCE INTO ls_header.
        lv_filename = mv_file_name.
        CONCATENATE '&SO_FILENAME=' lv_filename INTO ls_header->line.

        " Show icon
        zcl_eui_file=>split_file_path(
         EXPORTING
          iv_fullpath       = lv_filename
         IMPORTING
          ev_extension      = lv_ext ). " First 3 chars
        TRANSLATE lv_ext TO UPPER CASE.

        " Convert to table
        get_no_warning '-'.
        lv_value = get_raw( iv_no_warning = lv_no_warning ).
        zcl_eui_conv=>xstring_to_binary( EXPORTING iv_xstring = lv_value
                                         IMPORTING ev_length  = lv_file_size
                                                   et_table   = lt_data ).
        lv_size = lv_file_size.

        " Convert to proper type
        lv_subject = lv_filename.

        " Add attachment
        lo_doc->add_attachment(
         i_attachment_type    = lv_ext
         i_attachment_subject = lv_subject
         i_attachment_size    = lv_size
         i_att_content_hex    = lt_data
         i_attachment_header  = lt_header ).
      ENDIF.

      " Change sender if necessary
      IF io_sender IS NOT INITIAL.
        lo_mail->set_sender( io_sender ).
      ENDIF.

      " And send
      lo_mail->set_document( lo_doc ).
      lo_mail->set_send_immediately( abap_true ).
      lo_mail->send( ).

      " Sometimes force send email
      IF iv_commit = abap_true.
        COMMIT WORK AND WAIT.
      ENDIF.
    CATCH cx_bcs INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMETHOD.


METHOD zif_xtt~show.
  DATA lv_file_name TYPE string.

  " Get from file name
  IF mv_ole_ext IS NOT INITIAL.
    " Call save as
    me->download(
     EXPORTING
       iv_open     = mc_by-ole_hide
     CHANGING
       cv_fullpath = lv_file_name ).
  ELSE.
    TRY.
        get_no_warning mv_can_show_menu.

        " As a xstring (implemented in subclasses)
        DATA lv_content   TYPE xstring.
        lv_content = get_raw( iv_no_warning = lv_no_warning ).

        DATA lo_no_check TYPE REF TO zcx_eui_no_check.
      CATCH zcx_eui_no_check INTO lo_no_check.
        add_log_message( io_exception = lo_no_check ).
    ENDTRY.

    " If has logs show them
    _logger->show_as_button( is_profile = ms_log_profile ).
    IF _logger->has_messages( iv_msg_types = zcl_eui_logger=>mc_msg_types-error ) = abap_true.
      _logger->show( iv_profile = zcl_eui_logger=>mc_profile-popup
                     is_profile = ms_log_profile ).
    ENDIF.

    lv_file_name = mv_file_name.
  ENDIF.

  DATA lo_eui_file  TYPE REF TO zcl_eui_file.
  CREATE OBJECT lo_eui_file
    EXPORTING
      iv_xstring   = lv_content
      iv_file_name = lv_file_name.

  TRY.
      lo_eui_file->show( io_handler = io_handler ).
      DATA lo_error     TYPE REF TO zcx_eui_exception.
    CATCH zcx_eui_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMETHOD.


METHOD _get_scope.
  CLEAR: ev_new,
         eo_scope.

  DATA ls_scope TYPE to_scope.
  DATA lr_scope TYPE REF TO to_scope.

  READ TABLE _scopes REFERENCE INTO lr_scope
   WITH TABLE KEY sc_id = io_block->ms_ext-rb_id.
  IF sy-subrc = 0.
    " Recreate
    IF iv_force = abap_true.
      DELETE _scopes INDEX sy-tabix.
    ELSE.
      eo_scope = lr_scope->scope.
      eo_scope->set_block( io_block ).
      RETURN.
    ENDIF.
  ENDIF.

  " Not initilized
  ev_new = abap_true.
  CREATE OBJECT eo_scope.
  eo_scope->set_block( io_block ).

  ls_scope-sc_id = io_block->ms_ext-rb_id.
  ls_scope-scope = eo_scope.
  INSERT ls_scope INTO TABLE _scopes.
ENDMETHOD.


METHOD _logger_as_xml.
  " do not change document in productive system
  IF mv_is_prod = abap_true OR iv_row IS INITIAL.
    RETURN.
  ENDIF.

  DATA lt_message TYPE sfb_t_bal_s_msg.
  lt_message = _logger->get_messages( ).
  CHECK lt_message IS NOT INITIAL.

  FIELD-SYMBOLS <ls_message>  LIKE LINE OF lt_message.
  LOOP AT lt_message ASSIGNING <ls_message>.
    DATA lv_msgli TYPE string.
    MESSAGE ID <ls_message>-msgid TYPE <ls_message>-msgty NUMBER <ls_message>-msgno WITH <ls_message>-msgv1 <ls_message>-msgv2 <ls_message>-msgv3 <ls_message>-msgv4
     INTO lv_msgli.
    lv_msgli = cl_http_utility=>escape_html( lv_msgli ).

    DATA lv_row_copy TYPE string.
    lv_row_copy = iv_row.

    REPLACE FIRST OCCURRENCE OF: `{MSGTY}` IN lv_row_copy WITH <ls_message>-msgty,
                                 `{MSGID}` IN lv_row_copy WITH <ls_message>-msgid,
                                 `{MSGNO}` IN lv_row_copy WITH <ls_message>-msgno,
                                 `{MSGLI}` IN lv_row_copy WITH lv_msgli.
    CONCATENATE rs_log_xml-rows
                lv_row_copy INTO rs_log_xml-rows RESPECTING BLANKS.

    CHECK <ls_message>-msgty CA 'AXE'.
    rs_log_xml-has_axe = abap_true.
  ENDLOOP.

  CONCATENATE `DEV Warnings ` sy-datum(4) `.` sy-datum+4(2) `.` sy-datum+6(2) ` - ` sy-uzeit(2) `.` sy-uzeit+2(2) ` Please fix them` INTO rs_log_xml-title.
ENDMETHOD.


METHOD _logger_init.
  DATA lv_is_prod TYPE symandt.
  SELECT SINGLE mandt INTO lv_is_prod
  FROM t000
  WHERE mandt = sy-mandt AND cccategory = 'P'.

  " Show all info & warnings
  DATA lv_msg_types TYPE string VALUE zcl_eui_logger=>mc_msg_types-all.
  " Only errors
  IF lv_is_prod IS NOT INITIAL.
    mv_is_prod   = abap_true.
    lv_msg_types = zcl_eui_logger=>mc_msg_types-error.
  ENDIF.
  mv_can_show_menu = zcl_eui_menu=>can_show( ).

  CREATE OBJECT _logger
    EXPORTING
      iv_msg_types = lv_msg_types
      iv_unique    = abap_true.

  ms_log_profile-use_grid = abap_true.
  ms_log_profile-start_col = ms_log_profile-start_row = 3.

* change pf-status
  ms_log_profile-clbk_pbo-userexitp  = 'Z_XTT_TEMPLATE_013_ERR_REPAIR'.
  ms_log_profile-clbk_pbo-userexitf  = 'LOGGER_CALLBACK_PBO'.
ENDMETHOD.
ENDCLASS.

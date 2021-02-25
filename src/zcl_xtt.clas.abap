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

  types:
    BEGIN OF TS_RECIPIENT_BCS,
      RECIPIENT  TYPE REF TO IF_RECIPIENT_BCS,
      EXPRESS    TYPE OS_BOOLEAN,
      COPY       TYPE OS_BOOLEAN,
      BLIND_COPY TYPE OS_BOOLEAN,
      NO_FORWARD TYPE OS_BOOLEAN,
   END OF TS_RECIPIENT_BCS .
  types:
    TT_RECIPIENTS_BCS type STANDARD TABLE OF TS_RECIPIENT_BCS WITH DEFAULT KEY .

  constants:
    BEGIN OF MC_BY,
     ole        type STRING value 'OLE',
     ole_hide   type STRING value 'OLE_HIDE',
     app_server type STRING value 'APP_SERVER',
   END OF MC_BY .

  events PREPARE_RAW
    exporting
      value(IV_PATH) type STRING
      value(IR_CONTENT) type ref to XSTRING .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_OLE_EXT type STRING optional .
  methods MERGE
    importing
      !IS_BLOCK type ANY
      !IV_BLOCK_NAME type CSEQUENCE default `R`
    returning
      value(RO_XTT) type ref to ZCL_XTT .
  methods GET_RAW
  abstract
    returning
      value(RV_CONTENT) type XSTRING .
  methods DOWNLOAD
    importing
      !IV_OPEN type CSEQUENCE default ABAP_TRUE
      !IV_ZIP type ABAP_BOOL optional
    changing
      !CV_OLE_APP type OLE2_OBJECT optional
      !CV_OLE_DOC type OLE2_OBJECT optional
      !CV_FULLPATH type CSEQUENCE optional .
  methods SHOW
  final
    importing
      !IO_HANDLER type ref to OBJECT optional .
  methods SEND
    importing
      !IT_RECIPIENTS type RMPS_RECIPIENT_BCS optional
      !IT_RECIPIENTS_BCS type TT_RECIPIENTS_BCS optional
      !IV_SUBJECT type SO_OBJ_DES
      !IV_BODY type CSEQUENCE
      !IV_EXPRESS type ABAP_BOOL default ABAP_TRUE
      !IO_SENDER type ref to IF_SENDER_BCS optional
      !IV_COMMIT type ABAP_BOOL default ABAP_FALSE .
  methods ADD_RAW_EVENT
    importing
      !IV_PATH type CSEQUENCE .
  methods ADD_LOG_MESSAGE
    importing
      !IV_SYST type ABAP_BOOL optional
      !IV_TEXT type CSEQUENCE optional
      !IO_EXCEPTION type ref to CX_ROOT optional
      !IV_MSGTY type SYMSGTY optional .
protected section.

  types:
    BEGIN OF to_scope,
      sc_id TYPE string,
      scope TYPE REF TO zcl_xtt_scope,
    END OF to_scope .
  types:
    tt_scopes TYPE SORTED TABLE OF to_scope WITH UNIQUE KEY sc_id .

  data MV_FILE_NAME type STRING .
  data MV_OLE_EXT type STRING .
  data MV_SKIP_TAGS type ABAP_BOOL .
  data _LOGGER type ref to ZCL_EUI_LOGGER .
  data _SCOPES type TT_SCOPES .

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
private section.

*"* private components of class ZCL_XTT
*"* do not include other source files here!!!
  data MT_RAW_EVENT type STRINGTAB .
  data MO_DEBUG type ref to ZCL_XTT_DEBUG .

  methods _INIT_LOGGER .
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


METHOD constructor.
  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  _init_logger( ).

  mv_ole_ext = iv_ole_ext.
  TRY.
      mv_file_name = io_file->get_name( ).
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


METHOD download.
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
      " As a xstring (implemented in subclasses)
      DATA lv_content   TYPE xstring.
      lv_content = get_raw( ).
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
  _logger->show_as_button( iv_write_message = 'Please read logs for finding issues'(prl) ).
ENDMETHOD.


METHOD merge.
  " For chain calls
  ro_xtt = me.

  " For each merge
  CLEAR _scopes.

  " Delegate to another class
  CHECK mo_debug IS NOT INITIAL.
  mo_debug->save_merge( is_block      = is_block
                        iv_block_name = iv_block_name ).
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


METHOD send.
  DATA:
    lo_mail      TYPE REF TO cl_bcs,
    lo_doc       TYPE REF TO cl_document_bcs,
    lo_recipient TYPE REF TO if_recipient_bcs,
    lt_header    TYPE soli_tab,
    ls_header    TYPE REF TO soli,
    lv_subject   TYPE sood-objdes,
    lv_ext       TYPE soodk-objtp,
    lv_size      TYPE sood-objlen,
    lv_file_size TYPE i,
    lt_data      TYPE solix_tab,
    lo_error     TYPE REF TO cx_bcs,
    lt_body      TYPE solix_tab,
    lv_value     TYPE xstring,
    lv_filename  TYPE string,
    lv_body      TYPE string,
    lx_body      TYPE xstring,
    lv_len       TYPE i.
  FIELD-SYMBOLS:
    <ls_recipient> LIKE LINE OF it_recipients_bcs.

  TRY.
      " Request
      lo_mail = cl_bcs=>create_persistent( ).

      " Body
      lv_body = iv_body.
      " i_text = cl_document_bcs=>string_to_soli( lv_body ).
      lx_body = zcl_eui_conv=>string_to_xstring( lv_body ).
      zcl_eui_conv=>xstring_to_binary( EXPORTING iv_xstring = lx_body
                                       IMPORTING et_table   = lt_body
                                                 ev_length  = lv_len ).
      lv_size = lv_len.
      lo_doc  = cl_document_bcs=>create_document(
       i_type    = 'HTM'
       i_hex     = lt_body
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
        lv_value = get_raw( ).
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


METHOD show.
  DATA lv_content   TYPE xstring.
  DATA lv_file_name TYPE string.
  DATA lo_eui_file  TYPE REF TO zcl_eui_file.
  DATA lo_error     TYPE REF TO zcx_eui_exception.

  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  TRY.
      " As a xstring (implemented in subclasses)
      lv_content = get_raw( ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      add_log_message( io_exception = lo_no_check ).
  ENDTRY.
  " If has los show them
  _logger->show_as_button( ).

  " Get from file name
  IF mv_ole_ext IS NOT INITIAL.
    " Call save as
    me->download(
     EXPORTING
       iv_open     = mc_by-ole_hide
     CHANGING
       cv_fullpath = lv_file_name ).
  ELSE.
    lv_file_name = mv_file_name.
  ENDIF.

  CREATE OBJECT lo_eui_file
    EXPORTING
      iv_xstring   = lv_content
      iv_file_name = lv_file_name.

  TRY.
      lo_eui_file->show( io_handler = io_handler ).
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
      RETURN.
    ENDIF.
  ENDIF.

  " Not initilized
  ev_new = abap_true.
  CREATE OBJECT eo_scope
    EXPORTING
      io_block = io_block.

  ls_scope-sc_id = io_block->ms_ext-rb_id.
  ls_scope-scope = eo_scope.
  INSERT ls_scope INTO TABLE _scopes.
ENDMETHOD.


METHOD _init_logger.
  DATA lv_has_dev TYPE symandt.
  SELECT SINGLE mandt INTO lv_has_dev
  FROM t000
  WHERE mandt = sy-mandt AND cccategory <> 'P'.

  DATA lv_msg_types TYPE string.

  IF lv_has_dev IS INITIAL.
    lv_msg_types = zcl_eui_logger=>mc_msg_types-error.
  ELSE.
    " Add to log also all info & warnings
    lv_msg_types = zcl_eui_logger=>mc_msg_types-all.
  ENDIF.

  CREATE OBJECT _logger
    EXPORTING
      iv_msg_types = lv_msg_types
      iv_unique    = abap_true.
ENDMETHOD.
ENDCLASS.

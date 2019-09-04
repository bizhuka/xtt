class ZCL_XTT definition
  public
  abstract
  create protected .

public section.
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

  constants MC_BY_OLE type STRING value 'BY_OLE' ##NO_TEXT.

  class-events PBO
    exporting
      value(SENDER) type ref to ZCL_XTT
      value(IO_APP_OBJ) type OLE2_OBJECT .
  class-events PAI
    exporting
      value(SENDER) type ref to ZCL_XTT
      value(IV_CMD) type SYUCOMM .
  events PREPARE_RAW
    exporting
      value(IR_CONTENT) type ref to XSTRING .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE .
  methods MERGE
  abstract
    importing
      !IS_BLOCK type ANY
      !IV_BLOCK_NAME type CSEQUENCE default `R` .
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
      !CV_FULLPATH type STRING optional .
  methods SHOW
  final .
  methods SEND
    importing
      !IT_RECIPIENTS type RMPS_RECIPIENT_BCS optional
      !IT_RECIPIENTS_BCS type TT_RECIPIENTS_BCS optional
      !IV_SUBJECT type SO_OBJ_DES
      !IV_BODY type CSEQUENCE
      !IV_EXPRESS type ABAP_BOOL default ABAP_TRUE
      !IO_SENDER type ref to IF_SENDER_BCS optional
      !IV_COMMIT type ABAP_BOOL default ABAP_FALSE .
protected section.
  data MO_FILE type ref to ZIF_XTT_FILE .
private section.

  methods IS_WEB_DYNPRO
    importing
      !IV_INPLACE type ABAP_BOOL
    returning
      value(RV_WD) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_XTT IMPLEMENTATION.


METHOD constructor.
  mo_file = io_file.
ENDMETHOD.


METHOD download.
  DATA:
*    cv_fullpath  TYPE string,
    lv_content   TYPE xstring,
    lt_data      TYPE solix_tab,
    lv_file_size TYPE i,
    lv_filename  TYPE string,
    lv_no_ext    TYPE string,
    lv_ext       TYPE string,
    lv_path      TYPE string,
    lv_sep       TYPE char1,
    lv_len       TYPE i,
    lv_dest      TYPE rfcdes-rfcdest,
    lv_cpath     TYPE text255,
    lo_zip       TYPE REF TO cl_abap_zip,
    lv_ole_app   TYPE text255,
    lv_ole_doc   TYPE text255,
    lo_docs      TYPE ole2_object.

  " Just attach file
  CHECK is_web_dynpro( iv_inplace = abap_false ) IS INITIAL.

  " As a xstring (implemented in subclasses)
  lv_content = get_raw( ).

  " Use existing path
  IF cv_fullpath IS INITIAL.
    " Just name without path
    cv_fullpath = mo_file->get_name( ).
  ENDIF.

  " New file name
  zcl_xtt_util=>split_file_path(
    EXPORTING
      iv_fullpath       = cv_fullpath
    IMPORTING
      ev_filename       = lv_filename
      ev_filename_noext = lv_no_ext
      ev_extension      = lv_ext
      ev_path           = lv_path ).

  IF lv_path IS INITIAL.
    " No need to clean files (cl_gui_frontend_services=>file_delete). SAP gui cleans 'SAP GUI\tmp\' automatically
    cl_gui_frontend_services=>get_temp_directory( CHANGING temp_dir = lv_path EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc = 0.

    " Add file separator
    cl_gui_frontend_services=>get_file_separator(
     CHANGING
       file_separator = lv_sep ).
    cl_gui_cfw=>flush( ).

    lv_len = strlen( lv_path ) - 1.
    IF lv_path+lv_len(1) <> lv_sep.
      CONCATENATE lv_path lv_sep INTO lv_path.
    ENDIF.
  ENDIF.

  " For OLE
  TRANSLATE lv_ext TO LOWER CASE.
  zcl_xtt_util=>get_ole_info(
   EXPORTING
     io_xtt      = me
   CHANGING
     cv_ole_app  = lv_ole_app
     cv_ole_doc  = lv_ole_doc
     cv_file_ext = lv_ext ).

  " For plain XMLs only (meaningless for docx & xlsx)
  IF iv_zip = abap_true.
    lv_ext = 'zip'.
    CREATE OBJECT lo_zip.

    zcl_xtt_util=>xml_to_zip(
     io_zip  = lo_zip
     iv_name = lv_filename
     iv_xdoc = lv_content ).

    " And compress
    lv_content = lo_zip->save( ).
  ENDIF.

  " Create fullpath again
  CONCATENATE lv_path lv_no_ext `.` lv_ext INTO cv_fullpath.

  " Already exist. Add date and time
  IF cl_gui_frontend_services=>file_exist( cv_fullpath ) = abap_true.
    CONCATENATE lv_path lv_no_ext ` ` sy-datum ` ` sy-uzeit `.` lv_ext INTO cv_fullpath.
  ENDIF.

  " Convert to table
  zcl_xtt_util=>xstring_to_binary(
   EXPORTING
     iv_xstring = lv_content
   IMPORTING
     ev_length  = lv_file_size
     et_table   = lt_data ).

  IF lv_file_size < 10000000.
    " For small files
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename     = cv_fullpath
        filetype     = 'BIN'
        bin_filesize = lv_file_size
      TABLES
        data_tab     = lt_data
      EXCEPTIONS
        OTHERS       = 1.
    CHECK sy-subrc = 0.
  ELSE.
    " For big files
    CALL FUNCTION 'SCMS_FE_START_REG_SERVER'
      EXPORTING
        destname    = 'SAPFTP'
      IMPORTING
        destination = lv_dest
      EXCEPTIONS
        OTHERS      = 1.
    CHECK sy-subrc = 0.

    " create_folders( cv_fullpath ).
    lv_cpath = cv_fullpath.
    CALL FUNCTION 'FTP_R3_TO_CLIENT'
      EXPORTING
        fname           = lv_cpath
        rfc_destination = lv_dest
        blob_length     = lv_file_size
      TABLES
        blob            = lt_data
      EXCEPTIONS
        OTHERS          = 3.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'SCMS_FE_STOP_REG_SERVER'
      CHANGING
        destination = lv_dest.
  ENDIF.

  " And open file
  CHECK iv_open IS NOT INITIAL AND
        zcl_xtt_util=>is_common_gui( ) = abap_true.

  " Just open
  IF  iv_open <> zcl_xtt=>mc_by_ole OR
    ( iv_open IS NOT INITIAL AND iv_zip = abap_true ). " Open as zip
    cl_gui_cfw=>flush( ).
    cl_gui_frontend_services=>execute(
     EXPORTING
      document               = cv_fullpath
      operation              = 'OPEN'
     EXCEPTIONS
      OTHERS                 = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
    RETURN.
  ENDIF.

  " Meaningless
  CHECK iv_zip <> abap_true.

  " Open with OLE for call a macro. Only for .docx, .docm, .xlsx, .xlsm
  CREATE OBJECT cv_ole_app lv_ole_app.

  " Cannot use variables!
  CASE lv_ole_doc.
    WHEN 'Workbooks'.
      GET PROPERTY OF cv_ole_app 'Workbooks' = lo_docs.
    WHEN 'Documents'.
      GET PROPERTY OF cv_ole_app 'Documents' = lo_docs.
    WHEN OTHERS.
      MESSAGE x002(zsy_xtt) WITH lv_ole_doc.
  ENDCASE.

  CALL METHOD OF lo_docs 'Open' = cv_ole_doc
    EXPORTING
      #1 = cv_fullpath.

  SET PROPERTY OF cv_ole_app 'Visible' = 1.
ENDMETHOD.


METHOD is_web_dynpro.
  DATA:
    lv_content  TYPE xstring,
    lv_filename TYPE string.

  " Check Web Dynpro
  CHECK wdr_task=>application IS NOT INITIAL.

  " As a xstring (implemented in subclasses)
  lv_content = get_raw( ).

  " File name
  lv_filename = mo_file->get_name( ).

  " Add as an attachment
  cl_wd_runtime_services=>attach_file_to_response(
    EXPORTING
      i_filename      = lv_filename  " File name with extension
      i_content       = lv_content
      i_mime_type     = 'RAW'        " No need to specify
      i_in_new_window = abap_false
      i_inplace       = iv_inplace ).

  " Is web dynpro
  rv_wd = abap_true.
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
    lo_exp       TYPE REF TO cx_bcs,
    lv_text      TYPE string,
    lt_body      TYPE soli_tab,
    lv_value     TYPE xstring,
    lv_filename  TYPE string,
    lv_body      TYPE string.
  FIELD-SYMBOLS:
    <ls_recipient> LIKE LINE OF it_recipients_bcs.

  TRY.
      " Request
      lo_mail = cl_bcs=>create_persistent( ).

      " Body
      lv_body = iv_body.
      lt_body = cl_document_bcs=>string_to_soli( lv_body ).
      lo_doc = cl_document_bcs=>create_document(
       i_type    = 'HTM'
       i_text    = lt_body
       i_subject = iv_subject ).

      " № 1 - Add recipients
      LOOP AT it_recipients INTO lo_recipient.
        lo_mail->add_recipient( i_recipient = lo_recipient i_express = iv_express ).
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
      IF mo_file IS NOT INITIAL.
        " File name
        APPEND INITIAL LINE TO lt_header REFERENCE INTO ls_header.
        lv_filename = mo_file->get_name( ).
        CONCATENATE '&SO_FILENAME=' lv_filename INTO ls_header->line.

        " Show icon
        zcl_xtt_util=>split_file_path(
         EXPORTING
          iv_fullpath       = lv_filename
         IMPORTING
          ev_extension      = lv_ext ). " First 3 chars
        TRANSLATE lv_ext TO UPPER CASE.

        " Convert to table
        lv_value = get_raw( ).
        zcl_xtt_util=>xstring_to_binary(
         EXPORTING
           iv_xstring = lv_value
         IMPORTING
           ev_length  = lv_file_size
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
    CATCH cx_bcs INTO lo_exp.
      lv_text = lo_exp->if_message~get_text( ).
      MESSAGE lv_text TYPE 'E'.
  ENDTRY.
ENDMETHOD.


METHOD show.
  " Try to open inplace in browser
  CHECK is_web_dynpro( iv_inplace = abap_true ) IS INITIAL.

  CALL FUNCTION 'Z_SHOW_XTT_SCREEN'
    EXPORTING
      io_xtt = me.
ENDMETHOD.
ENDCLASS.

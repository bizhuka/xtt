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

  events PREPARE_RAW
    exporting
      value(IR_CONTENT) type ref to XSTRING .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_OLE_EXT type STRING optional .
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
  class-methods IS_COMMON_GUI
    returning
      value(RV_OK) type ABAP_BOOL .
protected section.

  constants MC_BY_OLE_HIDE type STRING value 'BY_OLE_HIDE' ##NO_TEXT.
  data MV_FILE_NAME type STRING .
  data MV_OLE_EXT type STRING .
private section.
ENDCLASS.



CLASS ZCL_XTT IMPLEMENTATION.


METHOD constructor.
  mv_file_name = io_file->get_name( ).
  mv_ole_ext   = iv_ole_ext.
ENDMETHOD.


METHOD download.
  DATA lv_content   TYPE xstring.
  DATA lv_filename  TYPE string.
  DATA lv_no_ext    TYPE string.
  DATA lv_ext       TYPE string.
  DATA lv_path      TYPE string.
  DATA lo_zip       TYPE REF TO cl_abap_zip.
  " New functionality
  DATA lo_eui_file  TYPE REF TO zcl_eui_file.
  DATA lo_error     TYPE REF TO zcx_eui_exception.
  DATA lv_visible   TYPE abap_bool.

  " As a xstring (implemented in subclasses)
  lv_content = get_raw( ).

  " Use existing path
  IF cv_fullpath IS INITIAL.
    " Just name without path
    cv_fullpath = mv_file_name.
  ENDIF.

  " Could be file name
  zcl_eui_file=>split_file_path(
    EXPORTING
      iv_fullpath       = cv_fullpath
    IMPORTING
      ev_filename       = lv_filename
      ev_file_noext     = lv_no_ext
      ev_extension      = lv_ext
      ev_path           = lv_path ).

  " For plain XMLs only (meaningless for docx & xlsx)
  IF iv_zip = abap_true.
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
  CONCATENATE lv_path lv_no_ext `.` lv_ext INTO cv_fullpath.

  " delegate to new class
  TRY.
      IF mv_ole_ext IS NOT INITIAL.
        lv_ext = mv_ole_ext.
      ENDIF.

      CREATE OBJECT lo_eui_file
        EXPORTING
          iv_xstring   = lv_content
          iv_file_name = lv_ext.

      lo_eui_file->download(
       iv_full_path   = cv_fullpath
       iv_save_dialog = abap_false ).

      " Where saved
      cv_fullpath = lo_eui_file->get_full_path( ).

      " And open file
      CHECK iv_open IS NOT INITIAL AND
            is_common_gui( ) = abap_true.

      " Open ?
      CASE iv_open.
        WHEN zcl_xtt=>mc_by_ole.
          lv_visible = abap_true.

        WHEN zcl_xtt=>mc_by_ole_hide.
          lv_visible = abap_false.

        WHEN OTHERS.
          lv_visible = abap_undefined.
      ENDCASE.

      " Just open
      IF  lv_visible = abap_undefined OR
        ( iv_open IS NOT INITIAL AND iv_zip = abap_true ). " Open as zip
        lo_eui_file->open( ).
        RETURN.
      ENDIF.

      " Meaningless
      CHECK iv_zip <> abap_true.

      lo_eui_file->open_by_ole(
       EXPORTING
         iv_visible = lv_visible
       CHANGING
         cv_ole_app = cv_ole_app
         cv_ole_doc = cv_ole_doc ).
    CATCH zcx_eui_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMETHOD.


METHOD IS_COMMON_GUI.
  " Background process
  CHECK sy-batch <> abap_true.

  " Web dynpro  (TODO SAPUI5)
  CHECK wdr_task=>application IS INITIAL.

  " Is Ok
  rv_ok = abap_true.
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
        zcl_eui_conv=>xstring_to_binary(
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
    CATCH cx_bcs INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMETHOD.


METHOD show.
  DATA lv_content   TYPE xstring.
  DATA lv_file_name TYPE string.
  DATA lo_eui_file  TYPE REF TO zcl_eui_file.
  DATA lv_name      TYPE string.
  DATA lo_error     TYPE REF TO zcx_eui_exception.

  lv_content = get_raw( ).

  " Get from file name
  IF mv_ole_ext IS NOT INITIAL.
    " Call save as
    me->download(
     EXPORTING
       iv_open     = mc_by_ole_hide
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
ENDCLASS.

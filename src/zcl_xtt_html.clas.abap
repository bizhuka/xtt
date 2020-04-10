class ZCL_XTT_HTML definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_AS_EMAIL_BODY type ABAP_BOOL optional .

  methods SEND
    redefinition .
protected section.
private section.

  data MV_AS_EMAIL_BODY type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_XTT_HTML IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file        = io_file
   iv_body_tag    = 'body'
   iv_row_tag     = 'tr' ).

  " Template as email body
  mv_as_email_body = iv_as_email_body.
ENDMETHOD.


METHOD send.
  DATA:
    lv_body      TYPE string,
    lv_raw       TYPE xstring,
    lv_file_name LIKE mv_file_name.

  " Default body
  lv_body = iv_body.

  " Only if no body
  IF lv_body IS INITIAL AND mv_as_email_body = abap_true.
    " Get as HTML text
    lv_raw = get_raw( ).
    lv_body = zcl_eui_conv=>xstring_to_string( lv_raw ).

    " Do not have any attachments
    lv_file_name = mv_file_name.
    CLEAR mv_file_name.
  ENDIF.

  super->send(
    it_recipients     = it_recipients
    it_recipients_bcs = it_recipients_bcs
    iv_subject        = iv_subject
    iv_body           = lv_body
    iv_express        = iv_express
    io_sender         = io_sender
    iv_commit         = iv_commit ).

  " Set it back
  IF lv_file_name IS NOT INITIAL.
    mv_file_name = lv_file_name.
  ENDIF.
ENDMETHOD.
ENDCLASS.

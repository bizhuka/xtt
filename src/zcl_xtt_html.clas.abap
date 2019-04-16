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
    lv_body TYPE string,
    lv_raw  TYPE xstring,
    lo_file LIKE mo_file.

  " Default body
  lv_body = iv_body.

  " Only if no body
  IF lv_body IS INITIAL AND mv_as_email_body = abap_true.
    " Get as HTML text
    lv_raw = get_raw( ).
    lv_body = zcl_xtt_util=>xstring_to_string( lv_raw ).

    " Do not have any attachments
    lo_file = mo_file.
    CLEAR mo_file.
  ENDIF.

  super->send(
    it_recipients = it_recipients
    iv_subject    = iv_subject
    iv_body       = lv_body ).

  " Set it back
  IF lo_file IS NOT INITIAL.
    mo_file = lo_file.
  ENDIF.
ENDMETHOD.
ENDCLASS.

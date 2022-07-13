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
  class-methods FORMAT
    importing
      !IV_TEMPLATE type CSEQUENCE
      !IT_FORMAT_FIELD type ZCL_XTT_UTIL=>TT_FORMAT_FIELD optional
      !IS_ROOT type ANY optional
      !IV_ROOT_NAME type CSEQUENCE default 'R'
    returning
      value(RV_TEXT) type STRING .

  methods ZIF_XTT~SEND
    redefinition .
protected section.

  methods _LOGGER_AS_XML
    redefinition .
private section.

  data MV_AS_EMAIL_BODY type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_XTT_HTML IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file        = io_file
   iv_body_tag    = 'body'
   iv_row_tag     = 'tr'
   iv_line_break  = '<br/>' ).

  " Template as email body
  mv_as_email_body = iv_as_email_body.

  " Skip MESSAGE w008(zsy_xtt).
  _logger->skip( iv_msgid = 'ZSY_XTT'
                 iv_msgno = 008
                 iv_msgty = 'W' ).
ENDMETHOD.


METHOD format.
  " Check input
  IF is_root IS NOT SUPPLIED AND it_format_field IS NOT SUPPLIED.
    zcx_eui_no_check=>raise_sys_error( iv_message = `Please pass 'IS_ROOT' or 'IT_FORMAT_FIELD'` ).
  ENDIF.

  "  NEW zcl_xtt_html( NEW zcl_xtt_file_raw( iv_name = 'dummy' iv_string = iv_template )

  DATA lo_file TYPE REF TO zif_xtt_file.
  CREATE OBJECT lo_file TYPE zcl_xtt_file_raw
    EXPORTING
      iv_name   = 'dummy'
      iv_string = iv_template.

  DATA lo_html TYPE REF TO zcl_xtt_html.
  CREATE OBJECT lo_html
    EXPORTING
      io_file = lo_file.

  FIELD-SYMBOLS <ls_root> TYPE any.
  IF is_root IS SUPPLIED.
    ASSIGN is_root TO <ls_root>.
  ELSE.
    " Based on fields
    DATA lr_root TYPE REF TO data.
    lr_root = zcl_xtt_util=>create_root( it_format_field ).
    CHECK lr_root IS NOT INITIAL.
    ASSIGN lr_root->* TO <ls_root>.
  ENDIF.

  " Pass data to the tamplate
  lo_html->merge( is_block      = <ls_root>
                  iv_block_name = iv_root_name
                ).

  " DATA lv_text TYPE xstring. lv_text =
  lo_html->get_raw( iv_no_warning = abap_true ).

  " Convert
  rv_text = lo_html->mv_file_content. " zcl_eui_conv=>xstring_to_string( lv_text ).
ENDMETHOD.


METHOD zif_xtt~send.
  DATA:
    lv_body      TYPE string,
    lv_file_name LIKE mv_file_name.

  " Default body
  lv_body = iv_body.

  " Only if no body
  IF lv_body IS INITIAL AND mv_as_email_body = abap_true.
    " Get as HTML text. lv_raw =
    " Raise events to change content
    get_raw( iv_no_warning = abap_true ).

    " None UTF-8 system?
    " lv_body = zcl_eui_conv=>xstring_to_string( lv_raw ).
    lv_body = mv_file_content.

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


METHOD _logger_as_xml.
  DATA lv_row TYPE string.
  CONCATENATE
      `<tr>`
        `<td>{MSGTY}</td>`
        `<td>{MSGID}</td>`
        `<td>{MSGNO}</td>`
        `<td>{MSGLI}</td>`
      `</tr>` INTO lv_row.
  rs_log_xml = super->_logger_as_xml( lv_row ).
  CHECK rs_log_xml IS NOT INITIAL.

  DATA lv_color TYPE string.
  IF rs_log_xml-has_axe = abap_true.
    lv_color = ` color: #ff0000`.
  ENDIF.

  CONCATENATE:
    `<hr/><p style="text-align:center;` lv_color `; font-size:34px;">`
      rs_log_xml-title
    `</p>`
    `<table border="1"`
       ` cellspacing="0"`
       ` cellpadding="0">`
      `<tr>`
        `<td style="text-align:center"><b>Type</b></td>`
        `<td style="text-align:center"><b>Class</b></td>`
        `<td style="text-align:center"><b>Number</b></td>`
        `<td style="text-align:center"><b>Message</b></td>`
      `</tr>`        INTO rs_log_xml-before,
     `</table>`  ``  INTO rs_log_xml-after.
ENDMETHOD.
ENDCLASS.

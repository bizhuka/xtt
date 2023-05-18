class ZCL_XTT_PDF definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE .

  methods ZIF_XTT~GET_RAW
    redefinition .
protected section.

  methods BOUNDS_FROM_BODY
    redefinition .
  methods ON_MATCH_FOUND
    redefinition .
  methods _LOGGER_AS_XML
    redefinition .
  methods _GET_FILE_NAME
    redefinition .
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_XTT_PDF IMPLEMENTATION.


METHOD bounds_from_body.
  IF iv_root_is_table = abap_true OR iv_block_name IS INITIAL.
    mv_body_tag = 'subform'.                                "#EC NOTEXT
  ENDIF.

  rs_bounds = super->bounds_from_body( iv_context       = iv_context
                                       iv_root_is_table = iv_root_is_table
                                       iv_block_name    = iv_block_name ).

  DATA:
    lv_pattern TYPE string,
    lv_text    TYPE string,
    lv_tabix   TYPE sytabix.
  FIELD-SYMBOLS:
   <ls_match>  LIKE LINE OF rs_bounds-t_match.

  rs_bounds-with_tag = iv_root_is_table.
  CHECK iv_root_is_table = abap_true.

  " Detect bounds
  CONCATENATE `<subform*name="` iv_block_name `"*` INTO lv_pattern.
  LOOP AT rs_bounds-t_match ASSIGNING <ls_match>.
    lv_tabix = sy-tabix.

    " First with pattern
    lv_text = iv_context+<ls_match>-offset(<ls_match>-length).
    CHECK lv_text CP lv_pattern.

    rs_bounds-first_match = lv_tabix.
    rs_bounds-last_match  = rs_bounds-last_match - rs_bounds-first_match + 1.
    EXIT.
  ENDLOOP.

  mv_body_tag = 'template'.                                 "#EC NOTEXT
ENDMETHOD.


METHOD constructor.
  super->constructor(
   io_file       = io_file
   iv_body_tag   = 'template'                               "#EC NOTEXT
   iv_row_tag    = 'subform'                                "#EC NOTEXT
   "iv_line_break = '' ?
   iv_page_break = '<breakAfter targetType="pageArea"/>'    "#EC NOTEXT
  ).
ENDMETHOD.


METHOD on_match_found.
  DO 1 TIMES.
    CHECK is_field->typ = zcl_xtt_replace_block=>mc_type-image
      AND is_field->oref IS NOT INITIAL.

    " Transform ref
    DATA lo_image TYPE REF TO zcl_xtt_image.
    lo_image ?= is_field->oref.

    " Get upper <draw> tag
    DATA lt_match     TYPE match_result_tab.
    DATA ls_match_end TYPE REF TO match_result.
    DATA ls_match_beg TYPE REF TO match_result.

    " All tags
    lt_match = zcl_xtt_util=>get_tag_matches( iv_context = cv_content
                                              iv_tag     = 'draw' "#EC NOTEXT
                                             ).

    " Get upper nearest value. index of the entry before which it would be inserted
    READ TABLE lt_match TRANSPORTING NO FIELDS BINARY SEARCH
     WITH KEY offset = iv_pos_beg.
    CHECK sy-subrc = 4.

    " Read bounds
    READ TABLE lt_match REFERENCE INTO ls_match_end INDEX sy-tabix.
    sy-tabix = sy-tabix - 1.
    READ TABLE lt_match REFERENCE INTO ls_match_beg INDEX sy-tabix.
    CHECK ls_match_end IS NOT INITIAL
      AND ls_match_beg IS NOT INITIAL.

    " yes its upper tag
    CHECK cv_content+ls_match_beg->offset(5) = '<draw'
      AND cv_content+ls_match_end->offset(7) = '</draw>'.
    iv_pos_beg = ls_match_beg->offset.
    iv_pos_end = ls_match_end->offset + ls_match_end->length + 1. " plus 1 ?

    " new value fo image
    DATA lv_image_val TYPE string.
    lv_image_val = zcl_eui_conv=>xstring_to_base64( lo_image->mv_image ).

    " Get original text
    DATA lv_len TYPE i.
    lv_len   = iv_pos_end - iv_pos_beg - 1.
    mv_value = cv_content+iv_pos_beg(lv_len).

    " Text -> Image ?
    REPLACE FIRST OCCURRENCE OF `<textEdit/>` IN mv_value WITH `<imageEdit/>`.

    " Yes convert Text->Image
    IF sy-subrc = 0.
      " What to insert 1) 2)aspect="none" 3)aspect="actual">
      DATA lv_aspect TYPE string.
      IF lo_image->mv_width IS INITIAL AND lo_image->mv_height IS INITIAL.
        lv_aspect = ` aspect="actual"`.                     "#EC NOTEXT
      ELSE.
        DATA lv_width  TYPE p DECIMALS 3.
        DATA lv_height TYPE p DECIMALS 3.

        " 1 mm = 36000 EMU
        lv_width  = lo_image->mv_width  / 36000.
        lv_height = lo_image->mv_height / 36000.
        int_to_text lv_width.
        int_to_text lv_height.

        CONCATENATE ` w="` lv_width_txt  `mm" w_old="` INTO lv_width_txt. "#EC NOTEXT
        CONCATENATE ` h="` lv_height_txt `mm" h_old="` INTO lv_height_txt. "#EC NOTEXT

        " Should be in template!
        REPLACE FIRST OCCURRENCE OF:
         ` w="` IN mv_value WITH lv_width_txt,
         ` h="` IN mv_value WITH lv_height_txt.
      ENDIF.


      CONCATENATE
        " With Extension
        `<value><image contentType="image/` lo_image->mv_ext+1 `"` lv_aspect `>`
        lv_image_val
        `</image></value></draw>` INTO lv_image_val.
      " `<draw w="4.233mm" h="4.233mm">`

      " Comment old value
      REPLACE FIRST OCCURRENCE OF:
      `<value>`     IN mv_value WITH `<!--`,
      `</value>`    IN mv_value WITH `-->`,
      " Insert new data
      `</draw>`     IN mv_value WITH lv_image_val.
    ELSE. " No. It's already an Image field
      DATA lt_replace TYPE zcl_xtt_util=>tt_replace.
      FIELD-SYMBOLS <ls_replace> LIKE LINE OF lt_replace.

      " Rename some attributes
      APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
      <ls_replace>-from = ` contentType=`.                  "#EC NOTEXT
      <ls_replace>-to   = ` contentType2=`.                 "#EC NOTEXT

      APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
      <ls_replace>-from = ` href="`.                        "#EC NOTEXT
      <ls_replace>-to   = ` href2="`.                       "#EC NOTEXT

      " What to insert
      CONCATENATE ` contentType="image/` lo_image->mv_ext+1 `">` lv_image_val `</image>` INTO lv_image_val RESPECTING BLANKS.
      APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
      <ls_replace>-from = `/>`.
      <ls_replace>-to   = lv_image_val.

      " Find an image
      DATA lv_fdpos  TYPE syfdpos.
      IF mv_value CS `<image `.
        lv_fdpos = sy-fdpos.
        zcl_xtt_util=>replace_1st_from( EXPORTING it_replace = lt_replace
                                                  iv_from    = lv_fdpos
                                        CHANGING  cv_xml     = mv_value ).
      ENDIF.
    ENDIF.
  ENDDO.

  super->on_match_found(
   EXPORTING
    is_field   = is_field
    iv_pos_beg = iv_pos_beg
    iv_pos_end = iv_pos_end
   CHANGING
    cv_content = cv_content ).
ENDMETHOD.


METHOD zif_xtt~get_raw.
  DATA:
    lo_fp     TYPE REF TO if_fp,
    lo_pdfobj TYPE REF TO if_fp_pdf_object,
    lo_err    TYPE REF TO cx_fp_exception.
  " Get ready XML file
  rv_content = super->get_raw( iv_no_warning  = iv_no_warning ).

  " Create an instance
  lo_fp = cl_fp=>get_reference( ).
  TRY.
      " Create an object
      lo_pdfobj = lo_fp->create_pdf_object( ).

      " Set template
      lo_pdfobj->set_template( xftdata = rv_content ).

      "Data in XML format ->set_data( formdata = ). But data already set in XDP file itself

      " New task
      lo_pdfobj->set_task_renderpdf( )." changesrestricted = 'F' printable = abap_false

      " Execute of action
      lo_pdfobj->execute( ).
    CATCH cx_fp_exception INTO lo_err.
      zcx_eui_no_check=>raise_sys_error( io_error = lo_err ).
  ENDTRY.

  " Return as xstring
  CLEAR rv_content.
  lo_pdfobj->get_pdf( IMPORTING pdfdata = rv_content ).
ENDMETHOD.


METHOD _get_file_name.
  rv_file_name = super->_get_file_name( iv_mode ).
  " Change extension
  REPLACE ALL OCCURRENCES OF `.xdp` IN rv_file_name WITH `.pdf` IGNORING CASE. "#EC NOTEXT
ENDMETHOD.


METHOD _logger_as_xml.
  DATA lv_row TYPE string.
  CONCATENATE
  `<subform layout="row">`
    `<draw h="10mm">`
      `<border><edge/><corner thickness="0.1778mm"/></border>`
      `<ui><textEdit/></ui>`
      `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="62FA1503-5B7C-4CC4-9FFE-28102AFB29CA">{MSGTY}</text></value>`
      `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="center"/>`
    `</draw>`

    `<draw h="10mm">`
      `<border><edge/><corner thickness="0.1778mm"/></border>`
      `<ui><textEdit/></ui>`
      `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="EF88A2A4-760E-427F-8DF4-BB49261C3ADB">{MSGID}</text></value>`
      `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="center"/>`
    `</draw>`

    `<draw h="10mm">`
      `<border><edge/><corner thickness="0.1778mm"/></border>`
      `<ui><textEdit/></ui>`
      `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="F2638213-9E81-48F7-A61A-7623A79FFEC3">{MSGNO}</text></value>`
      `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="center"/>`
    `</draw>`

    `<draw h="10mm">`
      `<border><edge/><corner thickness="0.1778mm"/></border>`
      `<ui><textEdit/></ui>`
      `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="C3D5510B-76A8-42F8-9865-361895C88D77">{MSGLI}</text></value>`
      `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="right"/>`
    `</draw>`

    `<border><edge presence="hidden"/></border><?templateDesigner expand 1?>`
  `</subform>`
      INTO lv_row.

  rs_log_xml = super->_logger_as_xml( lv_row ).
  CHECK rs_log_xml IS NOT INITIAL.

  DATA lv_color TYPE string.
  IF rs_log_xml-has_axe = abap_true.
    lv_color = `<fill><color value="255,0,0"/></fill>`.
  ENDIF.

  CONCATENATE:
      `<draw hAlign="center" minH="13mm" w="201mm">`
         `<ui><textEdit/></ui><value><text>` rs_log_xml-title `</text></value>`
         `<font size="34pt" typeface="Arial" baselineShift="0pt">` lv_color `</font>`
         `<margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/>`
         `<para hAlign="center" spaceAbove="0pt" spaceBelow="0pt" textIndent="0pt" marginLeft="0pt" marginRight="0pt"/>`
      `</draw>`

      `<subform xmlns="http://www.xfa.org/schema/xfa-template/3.3/" layout="table" columnWidths="40.6402mm 40.6402mm 40.6402mm 80.657mm" x="0in" y="0in">`
        `<border><edge/></border>`
        `<subform layout="row" name="HeaderRow" id="HeaderRow_ID">`

          `<draw h="10mm">`
            `<border><edge/><corner thickness="0.1778mm"/></border>`
            `<ui><textEdit/></ui>`
            `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="83483D43-7C10-463E-B94B-4ADC352A4DB9">Type</text></value>`
            `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="center"/>`
          `</draw>`

          `<draw h="10mm">`
            `<border><edge/><corner thickness="0.1778mm"/></border>`
            `<ui><textEdit/></ui>`
            `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="ADF01250-2066-40E1-BDD6-CD5CF0A62EA6">Class</text></value>`
            `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="center"/>`
          `</draw>`

          `<draw h="10mm">`
            `<border><edge/><corner thickness="0.1778mm"/></border>`
            `<ui><textEdit/></ui>`
            `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="E305E5C2-14E7-48B3-92C6-C4CB94B1C1C5">Number</text></value>`
            `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="center"/>`
          `</draw>`

          `<draw h="10mm">`
            `<border><edge/><corner thickness="0.1778mm"/></border>`
            `<ui><textEdit/></ui>`
            `<value><text xmlns:xliff="urn:oasis:names:tc:xliff:document:1.1" xliff:rid="8B31CA66-6574-45A9-975D-C26701AD9A2C">Message</text></value>`
            `<font typeface="Arial"/><margin topInset="0.5mm" bottomInset="0.5mm" leftInset="0.5mm" rightInset="0.5mm"/><para vAlign="middle" hAlign="center"/>`
          `</draw>`

          `<border><edge presence="hidden"/></border><bind match="none"/><?templateDesigner expand 1?>`
        `</subform>`
      INTO rs_log_xml-before,
           `<?templateDesigner rowpattern first:1, next:1, firstcolor:f0f0f0, nextcolor:ffffff, apply:0?><overflow leader="HeaderRow"/><?templateDesigner expand 1?>`
         `</subform>`
      INTO rs_log_xml-after.
ENDMETHOD.
ENDCLASS.

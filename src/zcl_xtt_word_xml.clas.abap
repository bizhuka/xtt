class ZCL_XTT_WORD_XML definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_OLE_EXT type STRING default ZCL_EUI_FILE=>MC_EXTENSION-DOCX
      !IV_OLE_EXT_FORMAT type I default 12 .
protected section.

  methods ON_MATCH_FOUND
    redefinition .
private section.

  class-methods DETECT_IMAGE_ANNOTATION
    exporting
      !EV_IS_ANNOTATION type ABAP_BOOL
    changing
      !CV_XML type STRING
      !CV_POS_BEG type I
      !CV_POS_END type I .
  class-methods REPLACE_BINARY_DATA
    importing
      !IO_IMAGE type ref to ZCL_XTT_IMAGE
      !IV_BIN_AFTER type ABAP_BOOL
    changing
      !CV_XML type STRING
      !CV_VALUE type STRING
      !CV_POS_BEG type I
      !CV_POS_END type I .
ENDCLASS.



CLASS ZCL_XTT_WORD_XML IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'w:body' "#EC NOTEXT
   iv_row_tag          = 'w:tr'   "#EC NOTEXT
   iv_ole_ext          = iv_ole_ext
   iv_ole_ext_format   = iv_ole_ext_format
   iv_skip_tags        = abap_true
   iv_table_page_break = zcl_xtt_word_docx=>mc_table_page_break ).

  " REPLACE ALL OCCURRENCES OF `.xml` IN mv_file_name WITH `.doc` IGNORING CASE.
ENDMETHOD.


METHOD detect_image_annotation.
  CLEAR ev_is_annotation.

  " Has near and </aml:annotation>
  DATA lv_plus_50 TYPE i.
  lv_plus_50 = cv_pos_end + 50.
  CHECK strlen( cv_xml ) >= lv_plus_50.
  CHECK " cv_xml+cv_pos_end(50) = `}</w:t></w:r></w:p></aml:content></aml:annotation>`.
        cv_xml+cv_pos_end(50) CS '</aml:annotation>'.

  " Get all <aml:annotation> tags
  DATA lt_match     TYPE match_result_tab.
  DATA lr_match     TYPE REF TO match_result.
  FIELD-SYMBOLS: " From last to first
    <ls_m_txt_end> TYPE match_result,
    <ls_m_txt_beg> TYPE match_result,
    <ls_m_end>     TYPE match_result,
    <ls_m_start>   TYPE match_result.

  lt_match = zcl_xtt_util=>get_tag_matches( iv_context = cv_xml
                                            iv_tag     = `aml:annotation` "#EC NOTEXT
                                           ).
  " Get index after {R-T-IMG}
  DATA lv_tabix TYPE i.
  READ TABLE lt_match TRANSPORTING NO FIELDS BINARY SEARCH
   WITH KEY offset = cv_pos_beg.
  CHECK sy-subrc = 4.
  lv_tabix = sy-tabix.

  " Should be at least 4 items
  DATA lv_index TYPE i.
  DO 4 TIMES.
    lv_index = sy-index.

    READ TABLE lt_match REFERENCE INTO lr_match INDEX lv_tabix.
    IF sy-subrc = 0.
      lv_tabix = lv_tabix - 1.
    ELSE.
      RETURN.
    ENDIF.

    " Set match
    CASE lv_index.
      WHEN 1.
        ASSIGN lr_match->* TO <ls_m_txt_end>.
      WHEN 2.
        ASSIGN lr_match->* TO <ls_m_txt_beg>.
      WHEN 3.
        ASSIGN lr_match->* TO <ls_m_end>.
      WHEN 4.
        ASSIGN lr_match->* TO <ls_m_start>.
    ENDCASE.
  ENDDO.

  " Where to start search of ID and binary data
  cv_pos_beg = <ls_m_start>-offset.

  " Delete 2,3,4 `<aml:annotation aml:id="0" w:type="Word.Comment.End"/>` ... `</aml:annotation>`
  DATA: lv_len    TYPE i, lv_blanks TYPE string.

  lv_len    = <ls_m_txt_end>-offset + <ls_m_txt_end>-length - <ls_m_end>-offset + strlen( `</w:r>` ).
  lv_blanks = zcl_xtt_util=>repeat( val = ` `
                                    occ = lv_len ).
  REPLACE SECTION OFFSET <ls_m_end>-offset LENGTH lv_len OF cv_xml WITH lv_blanks.

  " Delete 1-st `<aml:annotation aml:id="0" w:type="Word.Comment.Start"/>`
  lv_blanks = zcl_xtt_util=>repeat( val = ` `
                                    occ = <ls_m_start>-length ).
  REPLACE SECTION OFFSET <ls_m_start>-offset LENGTH <ls_m_start>-length OF cv_xml WITH lv_blanks.

  " Yes is within annotation
  ev_is_annotation = abap_true.
ENDMETHOD.


METHOD on_match_found.
  DO 1 TIMES.
    CHECK is_field->typ = zcl_xtt_replace_block=>mc_type-image
      AND is_field->oref IS NOT INITIAL.

    " Transform ref
    DATA lo_image TYPE REF TO zcl_xtt_image.
    lo_image ?= is_field->oref.

    DATA lv_is_annotation TYPE abap_bool.
    " №1 Is image based ?
    detect_image_annotation( IMPORTING ev_is_annotation = lv_is_annotation
                             CHANGING  cv_xml           = cv_content
                                       cv_pos_beg       = iv_pos_beg
                                       cv_pos_end       = iv_pos_end ).
    " №2 Is image based ?
    DATA lv_id_in_alter_text TYPE abap_bool.
    " Previous 5 CHARs
    DATA lv_alt_pos TYPE i.
    lv_alt_pos = iv_pos_beg - 5.

    " Yes image id {R-T-IMG} in alternative text
    IF iv_pos_beg > 0 AND strlen( cv_content ) > iv_pos_beg AND cv_content+lv_alt_pos(5) = `alt="`.
      lv_id_in_alter_text = abap_true.
    ENDIF.

    " Is image template based
    IF lv_is_annotation = abap_true OR lv_id_in_alter_text = abap_true.
      replace_binary_data( EXPORTING io_image     = lo_image
                                     iv_bin_after = lv_is_annotation
                           CHANGING  cv_xml       = cv_content
                                     cv_value     = mv_value
                                     cv_pos_beg   = iv_pos_beg
                                     cv_pos_end   = iv_pos_end ).
    ELSE.
      " Create whole tag from scratch (just insert as new mv_value)
      zcl_xtt_word_docx=>get_image_tag( EXPORTING io_image  = lo_image
                                                  iv_inline = abap_true
                                        IMPORTING ev_tag    = mv_value ).
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


METHOD replace_binary_data.
  DATA lv_value LIKE cv_value.
  DATA lv_name  TYPE string.

  " Get binary data, not whole tag
  zcl_xtt_word_docx=>get_image_tag( EXPORTING io_image   = io_image
                                              iv_inline  = abap_true
                                    IMPORTING ev_bindata = lv_value
                                              ev_name    = lv_name ).
  DATA lv_to TYPE string.
  CONCATENATE `<v:imagedata src="` lv_name `"` INTO lv_to.
  zcl_xtt_util=>replace_1st( EXPORTING iv_from     = `<v:imagedata src="\b[^"]*"`
                                       iv_to       = lv_to
                                       iv_pos      = cv_pos_beg
                                       iv_keep_len = abap_true
                             CHANGING  cv_xml      = cv_xml ).

  " Get upper <draw> tag
  DATA lt_match     TYPE match_result_tab.
  DATA ls_match_end TYPE REF TO match_result.
  DATA ls_match_beg TYPE REF TO match_result.

  " All tags
  lt_match = zcl_xtt_util=>get_tag_matches( iv_context = cv_xml
                                            iv_tag     = 'w:binData' ).
  " TODO always 2 last 'w:binData' tags ?
  sy-tabix = lines( lt_match ) + 1.

  " Get upper nearest value. index of the entry before which it would be inserted
*  READ TABLE lt_match TRANSPORTING NO FIELDS BINARY SEARCH WITH KEY offset = cv_pos_beg.
*  CHECK sy-subrc = 4.

*  " Read bounds
*  IF iv_bin_after = abap_true.
*    READ TABLE lt_match REFERENCE INTO ls_match_beg INDEX sy-tabix.
*    sy-tabix = sy-tabix + 1.
*    READ TABLE lt_match REFERENCE INTO ls_match_end INDEX sy-tabix.
*  ELSE.
  sy-tabix = sy-tabix - 1.
  READ TABLE lt_match REFERENCE INTO ls_match_end INDEX sy-tabix.
  sy-tabix = sy-tabix - 1.
  READ TABLE lt_match REFERENCE INTO ls_match_beg INDEX sy-tabix.
*  ENDIF.

  CHECK ls_match_end IS NOT INITIAL
    AND ls_match_beg IS NOT INITIAL.

  " Just delete data
  CHECK cv_xml+ls_match_beg->offset(10) = '<w:binData'
    AND cv_xml+ls_match_end->offset(12) = '</w:binData>'.

  cv_pos_beg = ls_match_beg->offset.
  cv_pos_end = ls_match_end->offset + ls_match_end->length - 1. " plus 1 ?
  cv_value   = lv_value.
ENDMETHOD.
ENDCLASS.

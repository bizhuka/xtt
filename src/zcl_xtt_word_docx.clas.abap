class ZCL_XTT_WORD_DOCX definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  constants MC_TABLE_PAGE_BREAK type STRING value '<w:br w:type="page"/>' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE .
  class-methods GET_IMAGE_TAG
    importing
      !IO_IMAGE type ref to ZCL_XTT_IMAGE
      !IV_INLINE type ABAP_BOOL optional
    exporting
      !EV_BINDATA type STRING
      !EV_TAG type STRING
      !EV_NAME type STRING .

  methods GET_RAW
    redefinition .
  methods MERGE
    redefinition .
protected section.

  data MO_ZIP type ref to CL_ABAP_ZIP .

  methods ON_MATCH_FOUND
    redefinition .
private section.

  types:
    tt_required TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .

  data _XML_DOCUMENT_RELS type ref to ZCL_XTT_XML_UPDATER .
  data MT_REQUIRED type TT_REQUIRED .

  methods _CHANGE_HEADER_FOOTER
    importing
      !IS_BLOCK type ANY
      !IV_BLOCK_NAME type CSEQUENCE .
  methods _FIND_IMAGE_TEMPLATES .
  methods _CHECK_DRAWING_REL .
ENDCLASS.



CLASS ZCL_XTT_WORD_DOCX IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'w:body'                           "#EC NOTEXT
   iv_row_tag          = 'w:tr'                             "#EC NOTEXT
   iv_path             = 'word/document.xml'                "#EC NOTEXT
   iv_skip_tags        = abap_true
   iv_table_page_break = mc_table_page_break ).

  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  TRY.
      DATA lv_value TYPE xstring.
      io_file->get_content( IMPORTING ev_as_xstring = lv_value ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      add_log_message( io_exception = lo_no_check ).
      RETURN.
  ENDTRY.

  " Load zip archive from XSTRING
  CREATE OBJECT mo_zip.
  mo_zip->load( lv_value ).

  " Get content as a string from file
  zcl_eui_conv=>xml_from_zip( EXPORTING io_zip   = mo_zip
                                        iv_name  = mv_path
                              IMPORTING ev_sdoc  = mv_file_content ).

  _find_image_templates( ).
ENDMETHOD.


METHOD get_image_tag.
  CLEAR ev_bindata.
  CLEAR ev_tag.
  CLEAR ev_name.

  " Calculate new width & height IN -> lv_style
  DATA lv_style TYPE string.
  DATA lv_width  TYPE p.
  DATA lv_height TYPE p.

  " To points
  IF io_image->mv_width IS NOT INITIAL AND io_image->mv_height IS NOT INITIAL.
    lv_width  = io_image->mv_width  / 12700.
    lv_height = io_image->mv_height / 12700.

    int_to_text lv_width.
    int_to_text lv_height.

    CONCATENATE ` style="width:` lv_width_txt `pt;height:` lv_height_txt `pt"` INTO lv_style. "#EC NOTEXT
  ENDIF.

  " Insert IMAGE tags
  CONCATENATE
  `<w:pict>`
  `<v:shapetype id="_x0000_t75" coordsize="21600,21600" o:spt="75" o:preferrelative="t" path="m@4@5l@4@11@9@11@9@5xe" filled="f" stroked="f">`
  `<v:stroke joinstyle="miter"/>`
  `<v:formulas>`
  `<v:f eqn="if lineDrawn pixelLineWidth 0"/><v:f eqn="sum @0 1 0"/><v:f eqn="sum 0 0 @1"/><v:f eqn="prod @2 1 2"/><v:f eqn="prod @3 21600 pixelWidth"/><v:f eqn="prod @3 21600 pixelHeight"/>`
  `<v:f eqn="sum @0 0 1"/><v:f eqn="prod @6 1 2"/><v:f eqn="prod @7 21600 pixelWidth"/><v:f eqn="sum @8 21600 0"/><v:f eqn="prod @7 21600 pixelHeight"/><v:f eqn="sum @10 21600 0"/>`
  `</v:formulas>`
  `<v:path o:extrusionok="f" gradientshapeok="t" o:connecttype="rect"/><o:lock v:ext="edit" aspectratio="t"/>`
  `</v:shapetype>` INTO ev_tag.

  DATA lv_value2 TYPE string.

  IF iv_inline = abap_true.
    CONCATENATE `wordml://777`                              "#EC NOTEXT
     io_image->mv_index_txt
     io_image->mv_ext INTO ev_name.

    ev_bindata = zcl_eui_conv=>xstring_to_base64( io_image->mv_image ).
    CONCATENATE `<w:binData w:name="` ev_name `" xml:space="preserve">`
     ev_bindata `</w:binData>` INTO ev_bindata.

    CONCATENATE `<v:imagedata src="` ev_name `"/>` INTO lv_value2.
  ELSE.
    CONCATENATE `<v:imagedata r:id="_P` io_image->mv_index_txt `"/>` INTO lv_value2.
  ENDIF.

  " Final merge
  CONCATENATE
    ev_tag
    ev_bindata " For inline only
    `<v:shape id="_x0000_i1027" type="#_x0000_t75"` lv_style `>`
    lv_value2
    `</v:shape>`
    `</w:pict>` INTO ev_tag.
ENDMETHOD.


METHOD get_raw.
  " 1-st process additional files
  raise_raw_events( mo_zip ).

  " Raise from parent method
  super->get_raw( ). " rv_content

**********************************************************************
  " Raise evant with whole archive

  " for images
  _check_drawing_rel( ).

  " Replace XML file
  zcl_eui_conv=>xml_to_zip(
   io_zip  = mo_zip
   iv_name = mv_path
   iv_sdoc = mv_file_content ).

  " ZIP archive as xstring
  rv_content = mo_zip->save( ).

  " Change content in special cases
  DATA lr_content TYPE REF TO xstring.
  GET REFERENCE OF rv_content INTO lr_content.
  RAISE EVENT prepare_raw
   EXPORTING
     iv_path    = '' " <--- whole archive
     ir_content = lr_content.
ENDMETHOD.


METHOD merge.
  ro_xtt = super->merge( is_block      = is_block
                         iv_block_name = iv_block_name ).

  _change_header_footer( is_block      = is_block
                         iv_block_name = iv_block_name ).
ENDMETHOD.


METHOD on_match_found.
  DATA lv_skip TYPE abap_bool.

  DO 1 TIMES.
    CHECK is_field->typ = zcl_xtt_replace_block=>mc_type-image
      AND is_field->oref IS NOT INITIAL.

    " Transform ref
    DATA lo_image TYPE REF TO zcl_xtt_image.
    lo_image ?= is_field->oref.

**********************************************************************

    " Is image based ?
    DATA lv_id_in_alter_text TYPE abap_bool.
    " Previous 5 CHARs
    DATA lv_alt_pos TYPE i.
    lv_alt_pos = iv_pos_beg - 5.

    " Yes image id {R-T-IMG} in alternative text
    IF iv_pos_beg > 0 AND strlen( cv_content ) > iv_pos_beg AND cv_content+lv_alt_pos(5) = `alt="`.
      lv_id_in_alter_text = abap_true.
    ENDIF.

    " Create tag (just insert as new value)
    IF lv_id_in_alter_text <> abap_true.
      get_image_tag( EXPORTING io_image = lo_image
                     IMPORTING ev_tag       = mv_value ).
    ELSE.
      " Change image ID only
      DATA lv_to TYPE string.
      CONCATENATE `<v:imagedata r:id="_P` lo_image->mv_index_txt `"` INTO lv_to.
      zcl_xtt_util=>replace_1st( EXPORTING iv_from     = `<v:imagedata r:id="\b[^"]*"`
                                           iv_to       = lv_to
                                           iv_pos      = iv_pos_beg
                                           iv_keep_len = abap_true
                                 CHANGING  cv_xml      = cv_content ).

      " Do not change IV_CONTENT any more
      lv_skip = abap_true.
    ENDIF.

**********************************************************************
    DATA lv_file_name TYPE string.
    DATA lv_mime_text TYPE string.
    DATA lv_rewrite   TYPE abap_bool.
    lo_image->save_in_archive( EXPORTING io_zip       = mo_zip
                                             iv_prefix    = 'word/media/' "#EC NOTEXT
                                   IMPORTING ev_file_name = lv_file_name
                                             ev_mime_text = lv_mime_text
                                             ev_rewrite   = lv_rewrite ).
    INSERT lv_mime_text INTO TABLE mt_required.

    " Already have in releations
    CHECK lv_rewrite <> abap_true.

    " Initilize refs to files
    IF _xml_document_rels IS INITIAL.
      DATA lv_xml_document_rels TYPE string.
      CONCATENATE `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`
                  `</Relationships>` INTO lv_xml_document_rels.
      CREATE OBJECT _xml_document_rels
        EXPORTING
          io_zip     = mo_zip
          iv_path    = 'word/_rels/document.xml.rels' "#EC NOTEXT
          iv_str_tag = 'Relationships'
          iv_str_doc = lv_xml_document_rels.
    ENDIF.

    DATA lv_value TYPE string.
    CONCATENATE
      `<Relationship Id="_P`
      lo_image->mv_index_txt
      `" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/`
      lv_file_name `"/>` INTO lv_value.

    " Bew file to relations
    _xml_document_rels->str_add( lv_value ).
  ENDDO.

  " Just do not modify IV_CONTENT
  CHECK lv_skip <> abap_true.

  super->on_match_found(
   EXPORTING
    is_field   = is_field
    iv_pos_beg = iv_pos_beg
    iv_pos_end = iv_pos_end
   CHANGING
    cv_content = cv_content ).
ENDMETHOD.


METHOD _change_header_footer.
  DATA lv_prefix TYPE string.
  CONCATENATE `{` iv_block_name INTO lv_prefix.

  FIELD-SYMBOLS <ls_file> LIKE LINE OF mo_zip->files.
  LOOP AT mo_zip->files ASSIGNING <ls_file> WHERE name CP 'word/header*.xml'
                                               OR name CP 'word/footer*.xml'.
    DATA lv_path TYPE string.
    lv_path = <ls_file>-name.

    " Get content as a string from file
    DATA lv_content TYPE string.
    zcl_eui_conv=>xml_from_zip( EXPORTING io_zip   = mo_zip
                                          iv_name  = lv_path
                                IMPORTING ev_sdoc  = lv_content ).
    CHECK lv_content CS lv_prefix.

    " Kind of ZCL_XTT_HTML=>FORMAT( )
    DATA lo_file TYPE REF TO zif_xtt_file.
    CREATE OBJECT lo_file TYPE zcl_xtt_file_raw
      EXPORTING
        iv_name   = 'dummy'  "#EC NOTEXT
        iv_string = lv_content.

    DATA lo_html TYPE REF TO zcl_xtt.
    CREATE OBJECT lo_html TYPE zcl_xtt_html
      EXPORTING
        io_file = lo_file.

    " No need to show
    lo_html->_logger->skip( iv_msgid = 'ZSY_XTT' iv_msgno = 010 iv_msgty = 'W' ).
    lo_html->_logger->skip( iv_msgid = 'ZSY_XTT' iv_msgno = 012 iv_msgty = 'W' ).

    " Work as DOCX
    lo_html->mv_skip_tags = me->mv_skip_tags.

    " Pass data
    lo_html->merge( is_block      = is_block
                    iv_block_name = iv_block_name ).

    DATA lx_content TYPE xstring.
    lx_content = lo_html->get_raw( ).

    " Write back
    zcl_eui_conv=>xml_to_zip( io_zip  = mo_zip
                              iv_name = lv_path
                              iv_xdoc = lx_content ).

    " Add all logs
    DATA lt_msg TYPE sfb_t_bal_s_msg.
    lt_msg = lo_html->_logger->get_messages( ).
    _logger->add_batch( lt_msg ).
  ENDLOOP.
ENDMETHOD.


METHOD _check_drawing_rel.
  CHECK _xml_document_rels IS NOT INITIAL.
  _xml_document_rels->save( ).

  " Change files info
  DATA lo_xml_content_types TYPE REF TO zcl_xtt_xml_updater.
  CREATE OBJECT lo_xml_content_types
    EXPORTING
      io_zip     = mo_zip
      iv_path    = '[Content_Types].xml' "#EC NOTEXT
      " Append mode for file
      iv_str_tag = `Types`.

  " Add 1 by one
  DATA lv_req_text TYPE string.
  LOOP AT mt_required INTO lv_req_text.
    lo_xml_content_types->str_add( lv_req_text ).
  ENDLOOP.

  lo_xml_content_types->save( ).
ENDMETHOD.


METHOD _find_image_templates.
  " Get comments
  DATA lv_comments TYPE string.
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip   = mo_zip
    iv_name  = 'word/comments.xml'                          "#EC NOTEXT
   IMPORTING
    ev_sdoc  = lv_comments ).
  CHECK lv_comments IS NOT INITIAL.

  " All matches of `<w:comment></w:comment>`
  DATA lt_xml_match TYPE zcl_xtt_util=>tt_xml_match.
  FIELD-SYMBOLS <ls_xml_match> LIKE LINE OF lt_xml_match.

  lt_xml_match = zcl_xtt_util=>get_xml_matches( iv_context   = lv_comments
                                                iv_tag       = 'w:comment' "#EC NOTEXT
                                               ).
  " Safe delete from the end
  LOOP AT lt_xml_match ASSIGNING <ls_xml_match>.
    " Find XTT anchor
    DATA lv_xtt_name TYPE string.
    zcl_xtt_util=>get_from_tag(
      EXPORTING
        iv_beg_part = `<w:t>{`                              "#EC NOTEXT
        iv_end_part = `}</w:t>`                             "#EC NOTEXT
      IMPORTING
        ev_value0   = lv_xtt_name
      CHANGING
        cv_xml      = <ls_xml_match>-tag ).
    CHECK lv_xtt_name IS NOT INITIAL.

    DATA lv_word_id TYPE string.
    zcl_xtt_util=>get_from_tag(
      EXPORTING
        iv_beg_part = `w:id="`                              "#EC NOTEXT
        iv_end_part = `"`                                   "#EC NOTEXT
      IMPORTING
        ev_value0   = lv_word_id
      CHANGING
        cv_xml      = <ls_xml_match>-tag ).
    CHECK lv_word_id IS NOT INITIAL.

    " Change main document
    DATA lv_xml_part TYPE string.
    CONCATENATE `<w:commentRangeStart w:id="` lv_word_id `"` INTO lv_xml_part. "#EC NOTEXT
    CHECK mv_file_content CS lv_xml_part.

    " Where to start searach
    DATA lv_pos_beg TYPE i.
    lv_pos_beg = sy-fdpos.

    " What to change
    DATA lt_replace TYPE zcl_xtt_util=>tt_replace.
    CLEAR lt_replace.
    FIELD-SYMBOLS <ls_replace> LIKE LINE OF lt_replace.

    " Commnet Start
    APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
    <ls_replace>-from = lv_xml_part.
    <ls_replace>-to   = `<!--`.
    APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
    <ls_replace>-from = `/>`.
    <ls_replace>-to   = `-->`.

    " Comment End
    CONCATENATE `<w:commentRangeEnd w:id="` lv_word_id `"` INTO lv_xml_part.
    APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
    <ls_replace>-from = lv_xml_part.
    <ls_replace>-to   = `<!--`.

    CONCATENATE `<w:commentReference w:id="` lv_word_id `"/></w:r>` INTO lv_xml_part.
    APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
    <ls_replace>-from = lv_xml_part.
    <ls_replace>-to   = `-->`.

    " Paste to any id `<v:shape id="` or any other
    APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
    <ls_replace>-from = ` id="`.                            "#EC NOTEXT
    CONCATENATE         ` anchor_xtt_alt="{` lv_xtt_name `}" id="` INTO <ls_replace>-to. "#EC NOTEXT

    zcl_xtt_util=>replace_1st_from( EXPORTING it_replace = lt_replace
                                              iv_from    = lv_pos_beg
                                    CHANGING  cv_xml     = mv_file_content ).
  ENDLOOP.
ENDMETHOD.
ENDCLASS.

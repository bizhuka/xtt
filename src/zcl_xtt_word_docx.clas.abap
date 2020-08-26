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
      !IO_COMP_CELL type ref to ZCL_XTT_COMP_CELL
      !IV_INLINE type ABAP_BOOL optional
    exporting
      !EV_BINDATA type STRING
      !EV_TAG type STRING
      !EV_NAME type STRING .

  methods GET_RAW
    redefinition .
protected section.

  methods ON_MATCH_FOUND
    redefinition .
private section.

  types:
    tt_required TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .

  data MV_DRAWING_REL type STRING .
  data MT_REQUIRED type TT_REQUIRED .
ENDCLASS.



CLASS ZCL_XTT_WORD_DOCX IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'w:body'                           "#EC NOTEXT
   iv_row_tag          = 'w:tr'                             "#EC NOTEXT
   iv_path_in_arc      = 'word/document.xml'                "#EC NOTEXT
   iv_skip_tags        = abap_true
   iv_table_page_break = mc_table_page_break ).

**********************************************************************
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
  DATA lt_xml_match TYPE tt_xml_match.
  FIELD-SYMBOLS <ls_xml_match> LIKE LINE OF lt_xml_match.

  zcl_xtt_xml_base=>get_tag_matches( EXPORTING iv_context   = lv_comments
                                               iv_tag       = 'w:comment' "#EC NOTEXT
                                     IMPORTING et_xml_match = lt_xml_match ).
  " Safe delete from the end
  LOOP AT lt_xml_match ASSIGNING <ls_xml_match>.
    " Find XTT anchor
    DATA lv_xtt_name TYPE string.
    zcl_xtt_xml_base=>get_from_tag(
      EXPORTING
        iv_beg_part = `<w:t>{`                              "#EC NOTEXT
        iv_end_part = `}</w:t>`                             "#EC NOTEXT
      IMPORTING
        ev_value0   = lv_xtt_name
      CHANGING
        cv_xml      = <ls_xml_match>-tag ).
    CHECK lv_xtt_name IS NOT INITIAL.

    DATA lv_word_id TYPE string.
    zcl_xtt_xml_base=>get_from_tag(
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
    DATA lt_replace TYPE tt_replace.
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

    replace_1st_from( EXPORTING it_replace = lt_replace
                                iv_from    = lv_pos_beg
                      CHANGING  cv_xml     = mv_file_content ).
  ENDLOOP.
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
  IF io_comp_cell->mv_width IS NOT INITIAL AND io_comp_cell->mv_height IS NOT INITIAL.
    lv_width  = io_comp_cell->mv_width  / 12700.
    lv_height = io_comp_cell->mv_height / 12700.

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
     io_comp_cell->mv_index_txt
     io_comp_cell->mv_ext INTO ev_name.

    ev_bindata = cl_http_utility=>encode_x_base64( io_comp_cell->mv_image ).
    CONCATENATE `<w:binData w:name="` ev_name `" xml:space="preserve">`
     ev_bindata `</w:binData>` INTO ev_bindata.

    CONCATENATE `<v:imagedata src="` ev_name `"/>` INTO lv_value2.
  ELSE.
    CONCATENATE `<v:imagedata r:id="_rImgId` io_comp_cell->mv_index_txt `"/>` INTO lv_value2.
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
  DO 1 TIMES.
    CHECK mv_drawing_rel IS NOT INITIAL.

    zcl_eui_conv=>xml_to_zip(
     io_zip     = mo_zip
     iv_name    = 'word/_rels/document.xml.rels'            "#EC NOTEXT
     iv_sdoc    = mv_drawing_rel ).

    " Change files info
    DATA lv_content_xml TYPE string.
    zcl_eui_conv=>xml_from_zip(
     EXPORTING io_zip     = mo_zip
               iv_name    = '[Content_Types].xml'           "#EC NOTEXT
     IMPORTING ev_sdoc    = lv_content_xml ).

    " Add 1 by one
    DATA lv_req_text TYPE string.
    DATA lv_len      TYPE i.
    LOOP AT mt_required INTO lv_req_text.
      CHECK lv_content_xml NS lv_req_text.

      lv_len = strlen( lv_content_xml ) - strlen( `</Types>` ).
      CONCATENATE lv_content_xml(lv_len) lv_req_text `</Types>` INTO lv_content_xml.
    ENDLOOP.

    " And add
    zcl_eui_conv=>xml_to_zip(
     io_zip     = mo_zip
     iv_name    = '[Content_Types].xml'                     "#EC NOTEXT
     iv_sdoc    = lv_content_xml ).

  ENDDO.

  " And parent method
  rv_content = super->get_raw( ).
ENDMETHOD.


METHOD on_match_found.
  DATA lv_skip TYPE abap_bool.

  DO 1 TIMES.
    CHECK is_field->typ = zcl_xtt_replace_block=>mc_type_comp_cell
      AND is_field->oref IS NOT INITIAL.

    " Transform ref
    DATA lo_comp_cell TYPE REF TO zcl_xtt_comp_cell.
    lo_comp_cell ?= is_field->oref.

**********************************************************************
    " As ordinary string
    FIELD-SYMBOLS <lv_content> TYPE string.
    ASSIGN iv_content->* TO <lv_content>.

    " Is image based ?
    DATA lv_id_in_alter_text TYPE abap_bool.
    " Previous 5 CHARs
    DATA lv_alt_pos TYPE i.
    lv_alt_pos = iv_pos_beg - 5.

    " Yes image id {R-T-IMG} in alternative text
    IF iv_pos_beg > 0 AND strlen( <lv_content> ) > iv_pos_beg AND <lv_content>+lv_alt_pos(5) = `alt="`.
      lv_id_in_alter_text = abap_true.
    ENDIF.

    " Create tag (just insert as new value)
    IF lv_id_in_alter_text <> abap_true.
      get_image_tag( EXPORTING io_comp_cell = lo_comp_cell
                     IMPORTING ev_tag       = mv_value ).
    ELSE.
      " Change image ID only
      DATA lt_replace TYPE tt_replace.
      FIELD-SYMBOLS <ls_replace> LIKE LINE OF lt_replace.

      APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
      <ls_replace>-from = `<v:imagedata r:id="`.
      CONCATENATE         `<v:imagedata r:id="_rImgId` lo_comp_cell->mv_index_txt
                          `" old_id="` INTO <ls_replace>-to.

      replace_1st_from( EXPORTING it_replace = lt_replace
                                  iv_from    = iv_pos_beg
                        CHANGING  cv_xml     = <lv_content> ).

      " Do not change IV_CONTENT any more
      lv_skip = abap_true.
    ENDIF.

**********************************************************************
    DATA lv_file_name TYPE string.
    DATA lv_mime_text TYPE string.
    DATA lv_rewrite   TYPE abap_bool.
    lo_comp_cell->save_in_archive( EXPORTING io_zip       = mo_zip
                                             iv_prefix    = 'word/media/' "#EC NOTEXT
                                   IMPORTING ev_file_name = lv_file_name
                                             ev_mime_text = lv_mime_text
                                             ev_rewrite   = lv_rewrite ).
    INSERT lv_mime_text INTO TABLE mt_required.

    " Already have in releations
    CHECK lv_rewrite <> abap_true.

    " Initilize refs to files
    IF mv_drawing_rel IS INITIAL.
      zcl_eui_conv=>xml_from_zip(
       EXPORTING io_zip     = mo_zip
                 iv_name    = 'word/_rels/document.xml.rels' "#EC NOTEXT
       IMPORTING ev_sdoc    = mv_drawing_rel ).
    ENDIF.
    IF mv_drawing_rel IS INITIAL.
      CONCATENATE
        `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
        `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`
        `</Relationships>` INTO mv_drawing_rel.
    ENDIF.

    " Position of new string
    DATA lv_from_rel TYPE i.
    lv_from_rel = strlen( mv_drawing_rel ) - strlen( `</Relationships>` ).

    DATA lv_value TYPE string.
    CONCATENATE
      `<Relationship Id="_rImgId`
      lo_comp_cell->mv_index_txt
      `" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/`
      lv_file_name `"/>` INTO lv_value.
    CONCATENATE mv_drawing_rel(lv_from_rel) lv_value `</Relationships>` INTO mv_drawing_rel.
  ENDDO.

  " Just do not modify IV_CONTENT
  CHECK lv_skip <> abap_true.

  super->on_match_found(
    iv_content = iv_content
    is_field   = is_field
    iv_pos_beg = iv_pos_beg
    iv_pos_end = iv_pos_end ).
ENDMETHOD.
ENDCLASS.

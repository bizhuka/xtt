class ZCL_XTT_PDF definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE .

  methods GET_RAW
    redefinition .
protected section.

  methods FIND_BOUNDS
    redefinition .
  methods ON_MATCH_FOUND
    redefinition .
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_XTT_PDF IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'template'                         "#EC NOTEXT
   iv_row_tag          = 'subform'                          "#EC NOTEXT
   iv_table_page_break = '<breakAfter targetType="pageArea"/>' "#EC NOTEXT
  ).

  " Change extension
  REPLACE ALL OCCURRENCES OF `.xdp` IN mv_file_name WITH `.pdf` IGNORING CASE. "#EC NOTEXT
ENDMETHOD.


METHOD find_bounds.
  DATA:
    lv_tag     TYPE string,
    lv_pattern TYPE string,
    lv_text    TYPE string,
    lv_tabix   TYPE sytabix.
  FIELD-SYMBOLS:
   <ls_match>  LIKE LINE OF et_match.

  lv_tag = iv_tag.
  IF iv_first_level_is_table = abap_true.
    mv_body_tag = lv_tag = 'subform'. "#EC NOTEXT
  ENDIF.

  super->find_bounds(
   EXPORTING
    iv_context              = iv_context
    iv_tag                  = lv_tag
    iv_tag_add              = iv_tag_add
    iv_first_level_is_table = iv_first_level_is_table
    iv_block_name           = iv_block_name
   IMPORTING
    et_match                = et_match
    ev_with_tag             = ev_with_tag
    ev_first_match          = ev_first_match
    ev_last_match           = ev_last_match ).

  CASE iv_tag.
    WHEN mv_body_tag.
      ev_with_tag = iv_first_level_is_table.
      CHECK iv_first_level_is_table = abap_true.

      " Detect bounds
      CONCATENATE `<subform*name="` iv_block_name `"*` INTO lv_pattern.
      LOOP AT et_match ASSIGNING <ls_match>.
        lv_tabix = sy-tabix.

        " First with pattern
        lv_text = iv_context+<ls_match>-offset(<ls_match>-length).
        CHECK lv_text CP lv_pattern.

        ev_first_match = lv_tabix.
        ev_last_match  = ev_last_match - ev_first_match + 1.
        EXIT.
      ENDLOOP.

      mv_body_tag = 'template'. "#EC NOTEXT
      RETURN.
  ENDCASE.
ENDMETHOD.


METHOD get_raw.
  DATA:
    lo_fp     TYPE REF TO if_fp,
    lo_pdfobj TYPE REF TO if_fp_pdf_object,
    lo_err    TYPE REF TO cx_fp_exception.
  " Get ready XML file
  rv_content = super->get_raw( ).

*  IF 1 = 2.
*    DATA lo_file TYPE REF TO zcl_eui_file.
*    TRY.
*        CREATE OBJECT lo_file
*          EXPORTING
*            iv_xstring = rv_content.
*        lo_file->download( iv_default_filename = 'Ok4.xdp' ).
*      CATCH zcx_eui_exception.
*        CLEAR lo_file.
*    ENDTRY.
*  ENDIF.

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
      MESSAGE lo_err TYPE 'X'.
  ENDTRY.

  " Return as xstring
  CLEAR rv_content.
  lo_pdfobj->get_pdf( IMPORTING pdfdata = rv_content ).
ENDMETHOD.


METHOD on_match_found.
  DO 1 TIMES.
    CHECK is_field->typ = zcl_xtt_replace_block=>mc_type_comp_cell
      AND is_field->oref IS NOT INITIAL.

    " Transform ref
    DATA lo_comp_cell TYPE REF TO zcl_xtt_comp_cell.
    lo_comp_cell ?= is_field->oref.

    " Get upper <draw> tag
    DATA lt_match     TYPE match_result_tab.
    DATA ls_match_end TYPE REF TO match_result.
    DATA ls_match_beg TYPE REF TO match_result.

    " Text of current row
    FIELD-SYMBOLS <lv_content> TYPE string.
    ASSIGN iv_content->* TO <lv_content>.

    " All tags
    get_tag_matches( EXPORTING iv_context = <lv_content>
                               iv_tag     = 'draw' "#EC NOTEXT
                     IMPORTING et_match   = lt_match ).

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
    CHECK <lv_content>+ls_match_beg->offset(5) = '<draw'
      AND <lv_content>+ls_match_end->offset(7) = '</draw>'.
    iv_pos_beg = ls_match_beg->offset.
    iv_pos_end = ls_match_end->offset + ls_match_end->length + 1. " plus 1 ?

    " new value fo image
    DATA lv_image_val TYPE string.
    lv_image_val = cl_http_utility=>encode_x_base64( lo_comp_cell->mv_image ).

    " Get original text
    DATA lv_len TYPE i.
    lv_len   = iv_pos_end - iv_pos_beg - 1.
    mv_value = <lv_content>+iv_pos_beg(lv_len).

    " Text -> Image ?
    REPLACE FIRST OCCURRENCE OF `<textEdit/>` IN mv_value WITH `<imageEdit/>`.

    " Yes convert Text->Image
    IF sy-subrc = 0.
      " What to insert 1) 2)aspect="none" 3)aspect="actual">
      DATA lv_aspect TYPE string.
      IF lo_comp_cell->mv_width IS INITIAL AND lo_comp_cell->mv_height IS INITIAL.
        lv_aspect = ` aspect="actual"`. "#EC NOTEXT
      ELSE.
        DATA lv_width  TYPE p DECIMALS 3.
        DATA lv_height TYPE p DECIMALS 3.

        " 1 mm = 36000 EMU
        lv_width  = lo_comp_cell->mv_width  / 36000.
        lv_height = lo_comp_cell->mv_height / 36000.
        int_to_text lv_width.
        int_to_text lv_height.

        CONCATENATE ` w="` lv_width_txt  `mm" w_old="` INTO lv_width_txt.  "#EC NOTEXT
        CONCATENATE ` h="` lv_height_txt `mm" h_old="` INTO lv_height_txt. "#EC NOTEXT

        " Should be in template!
        REPLACE FIRST OCCURRENCE OF:
         ` w="` IN mv_value WITH lv_width_txt,
         ` h="` IN mv_value WITH lv_height_txt.
      ENDIF.


      CONCATENATE
        " With Extension
        `<value><image contentType="image/` lo_comp_cell->mv_ext+1 `"` lv_aspect `>`
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
      DATA lt_replace TYPE tt_replace.
      FIELD-SYMBOLS <ls_replace> LIKE LINE OF lt_replace.

      " Rename some attributes
      APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
      <ls_replace>-from = ` contentType=`.  "#EC NOTEXT
      <ls_replace>-to   = ` contentType2=`. "#EC NOTEXT

      APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
      <ls_replace>-from = ` href="`.  "#EC NOTEXT
      <ls_replace>-to   = ` href2="`. "#EC NOTEXT

      " What to insert
      CONCATENATE ` contentType="image/` lo_comp_cell->mv_ext+1 `">` lv_image_val `</image>` INTO lv_image_val RESPECTING BLANKS.
      APPEND INITIAL LINE TO lt_replace ASSIGNING <ls_replace>.
      <ls_replace>-from = `/>`.
      <ls_replace>-to   = lv_image_val.

      " Find an image
      DATA lv_fdpos  TYPE syfdpos.
      IF mv_value CS `<image `.
        lv_fdpos = sy-fdpos.
        replace_1st_from( EXPORTING it_replace = lt_replace
                                    iv_from    = lv_fdpos
                          CHANGING  cv_xml     = mv_value ).
      ENDIF.
    ENDIF.
  ENDDO.

  super->on_match_found(
    iv_content = iv_content
    is_field   = is_field
    iv_pos_beg = iv_pos_beg
    iv_pos_end = iv_pos_end ).
ENDMETHOD.
ENDCLASS.

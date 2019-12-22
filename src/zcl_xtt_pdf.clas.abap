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
private section.
ENDCLASS.



CLASS ZCL_XTT_PDF IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'template'
   iv_row_tag          = 'subform'
   iv_table_page_break = '<breakAfter targetType="pageArea"/>' ).
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
    mv_body_tag = lv_tag = 'subform'.
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

      mv_body_tag = 'template'.
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
ENDCLASS.

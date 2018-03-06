class ZCL_XTT_UTIL definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  constants MC_UTF8 type ABAP_ENCODING value '4110' ##NO_TEXT.

  class-methods SPLIT_FILE_PATH
    importing
      !IV_FULLPATH type CSEQUENCE
    exporting
      !EV_PATH type CSEQUENCE
      !EV_FILENAME type CSEQUENCE
      !EV_FILENAME_NOEXT type CSEQUENCE
      !EV_EXTENSION type CSEQUENCE .
  class-methods IS_COMMON_GUI
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods XML_FROM_ZIP
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IV_NAME type CSEQUENCE
    exporting
      !EO_XMLDOC type ref to IF_IXML_DOCUMENT
      !EV_SDOC type STRING .
  class-methods XML_TO_ZIP
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IV_NAME type STRING
      !IV_XDOC type XSTRING optional
      !IV_SDOC type STRING optional
      !IO_XMLDOC type ref to IF_IXML_DOCUMENT optional .
  class-methods STR_TO_XML
    importing
      !IV_STR type STRING optional
      !IV_XSTR type XSTRING optional
    returning
      value(RO_DOC) type ref to IF_IXML_DOCUMENT .
  class-methods XML_TO_STR
    importing
      !IO_DOC type ref to IF_IXML_DOCUMENT
    exporting
      !EV_STR type STRING
      !EV_XSTR type XSTRING .
  class-methods GET_OLE_INFO
    importing
      !IO_XTT type ref to ZCL_XTT optional
      !IV_CLASS_NAME type CSEQUENCE optional
    changing
      !CV_OLE_APP type CSEQUENCE optional
      !CV_OLE_DOC type CSEQUENCE optional
      !CV_PROXY_APP type CSEQUENCE optional
      !CV_FILE_EXT type CSEQUENCE optional .
  class-methods STRING_TO_XSTRING
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods BINARY_TO_XSTRING
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods BINARY_TO_STRING
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
    returning
      value(RV_STRING) type STRING .
  class-methods XSTRING_TO_BINARY
    importing
      !IV_XSTRING type XSTRING
    exporting
      !EV_LENGTH type I
      !ET_TABLE type SOLIX_TAB .
  class-methods XSTRING_TO_STRING
    importing
      !IV_XSTRING type XSTRING
    returning
      value(RV_STRING) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_XTT_UTIL IMPLEMENTATION.


METHOD binary_to_string.
  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = iv_length
      encoding     = mc_utf8
    IMPORTING
      text_buffer  = rv_string
    TABLES
      binary_tab   = it_table.
ENDMETHOD.


METHOD binary_to_xstring.
  " cl_bcs_convert=>solix_to_xstring( )
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = iv_length
    IMPORTING
      buffer       = rv_xstring
    TABLES
      binary_tab   = it_table.
ENDMETHOD.


METHOD get_ole_info.
  DATA:
    lv_class_name TYPE string,
    lo_desc       TYPE REF TO cl_abap_classdescr.

  " Get class name
  lv_class_name = iv_class_name.
  IF io_xtt IS NOT INITIAL.
    lo_desc ?= cl_abap_classdescr=>describe_by_object_ref( io_xtt ).
    lv_class_name = lo_desc->get_relative_name( ).
  ENDIF.

  CLEAR:
   cv_ole_app,
   cv_ole_doc,
   cv_proxy_app.

  CASE lv_class_name.
    WHEN 'ZCL_XTT_EXCEL_XLSX' OR 'ZCL_XTT_EXCEL_XML'.
      cv_ole_app = 'Excel.Application'.
      cv_ole_doc = 'Workbooks'.

*      IF lv_class_name = 'ZCL_XTT_EXCEL_XLSX'. Sometimes don't work for XML
        cv_proxy_app = 'Excel.Sheet'.
*      ENDIF.

    WHEN 'ZCL_XTT_WORD_DOCX' OR 'ZCL_XTT_WORD_XML'.
      cv_ole_app = 'Word.Application'.
      cv_ole_doc = 'Documents'.

*      IF lv_class_name = 'ZCL_XTT_WORD_DOCX'. Sometimes don't work for XML
        cv_proxy_app = 'Word.Document'.
*      ENDIF.

    WHEN 'ZCL_XTT_PDF'.
      cv_file_ext = 'pdf'.

    WHEN 'ZCL_XTT_HTML'.
      cv_file_ext = 'html'.
  ENDCASE.
ENDMETHOD.


method IS_COMMON_GUI.
  DATA:
   lv_web TYPE char1.

  " Background process
  CHECK sy-batch <> abap_true.

  " ITS/WebGUI
  CALL FUNCTION 'GUI_IS_ITS'
    IMPORTING
      return = lv_web.
  CHECK lv_web IS INITIAL.

  " Is Ok
  rv_ok = abap_true.
endmethod.


METHOD SPLIT_FILE_PATH.
  DATA:
    lv_ind     TYPE i,
    lv_cnt     TYPE i,
    lv_dot_pos TYPE i.

********************
  DEFINE set_if_requested.
    IF &1 IS REQUESTED.
      &1 = &2.
    ENDIF.
  END-OF-DEFINITION.
********************

  " What ?
  CHECK iv_fullpath IS NOT INITIAL.

  " Prepare vars
  "  set_if_requested ev_ev_tension ''.
  lv_ind = strlen( iv_fullpath ) - 1.
  TRY.
      WHILE lv_ind > 0 AND iv_fullpath+lv_ind(1) <> '\' AND iv_fullpath+lv_ind(1) <> '/'.
        IF iv_fullpath+lv_ind(1) = '.' AND lv_dot_pos IS INITIAL. " Only 1 time
          lv_dot_pos = lv_ind + 1.
          set_if_requested ev_extension iv_fullpath+lv_dot_pos.
        ENDIF.
        lv_ind = lv_ind - 1.
        lv_cnt = sy-index.
      ENDWHILE.
    CATCH cx_sy_range_out_of_bounds.
      EXIT.
  ENDTRY.

  " Fill ev_path, ev_filename, ev_filename_noext, ev_ev_tension
  IF lv_ind = 0.
    set_if_requested ev_filename iv_fullpath.
    set_if_requested ev_path     ''.

    IF lv_dot_pos IS INITIAL.
      set_if_requested ev_filename_noext iv_fullpath.
    ELSE.
      lv_cnt = lv_dot_pos - 1.
      set_if_requested ev_filename_noext iv_fullpath(lv_cnt).
    ENDIF.
  ELSE.
    lv_ind = lv_ind + 1.
    set_if_requested ev_filename iv_fullpath+lv_ind(lv_cnt).
    set_if_requested ev_path     iv_fullpath(lv_ind).

    IF lv_dot_pos IS INITIAL.
      set_if_requested ev_filename_noext iv_fullpath+lv_ind(lv_cnt).
    ELSE.
      lv_cnt = lv_dot_pos - lv_ind - 1.
      set_if_requested ev_filename_noext iv_fullpath+lv_ind(lv_cnt).
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD string_to_xstring.
  " rv_xstring = cl_bcs_convert=>string_to_xstring( iv_string = iv_string iv_codepage = mc_utf8 ).
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text     = iv_string
      encoding = mc_utf8
    IMPORTING
      buffer   = rv_xstring.
ENDMETHOD.


METHOD STR_TO_XML.
  DATA:
    lo_xml     TYPE REF TO if_ixml,
    lo_factory TYPE REF TO if_ixml_stream_factory,
    lo_stream  TYPE REF TO if_ixml_istream,
    lo_parser  TYPE REF TO if_ixml_parser.

  lo_xml      = cl_ixml=>create( ).
  lo_factory  = lo_xml->create_stream_factory( ).

  " From xstring
  IF iv_xstr IS SUPPLIED.
    lo_stream   = lo_factory->create_istream_xstring( iv_xstr ).
  ELSEIF iv_str IS SUPPLIED. " From string
    lo_stream   = lo_factory->create_istream_cstring( iv_str ).
  ELSE.
    RETURN.
  ENDIF.

  " Parse a document and return re_doc
  ro_doc = lo_xml->create_document( ).
  lo_parser = lo_xml->create_parser(
    stream_factory  = lo_factory
    istream         = lo_stream
    document        = ro_doc ).
  lo_parser->set_normalizing( ).
  lo_parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
  lo_parser->parse( ).
ENDMETHOD.


METHOD xml_from_zip.
  DATA:
    lv_value TYPE xstring.

  " As a string
  IF ev_sdoc IS REQUESTED.
    CLEAR ev_sdoc.
  ENDIF.

  " As an object
  IF eo_xmldoc IS REQUESTED.
    CLEAR eo_xmldoc.
  ENDIF.

  " Try to read the document from archive
  io_zip->get(
   EXPORTING
    name                    = iv_name
   IMPORTING
    content                 = lv_value
   EXCEPTIONS
    OTHERS                  = 1 ).
  CHECK sy-subrc = 0.

  " As a string
  IF ev_sdoc IS REQUESTED.
    ev_sdoc = xstring_to_string( lv_value ).
  ENDIF.

  " As an object
  IF eo_xmldoc IS REQUESTED.
    eo_xmldoc = zcl_xtt_util=>str_to_xml( iv_xstr = lv_value ).
  ENDIF.
ENDMETHOD.


METHOD XML_TO_STR.
  DATA:
    lo_xml     TYPE REF TO if_ixml,
    lo_encode  TYPE REF TO if_ixml_encoding,
    lo_factory TYPE REF TO if_ixml_stream_factory,
    lo_stream  TYPE REF TO if_ixml_ostream,
    lo_rendr   TYPE REF TO if_ixml_renderer.

  lo_xml    = cl_ixml=>create( ).

*    In a different encoding
*    IF im_charset IS NOT INITIAL.
  lo_encode = lo_xml->create_encoding( byte_order    = if_ixml_encoding=>co_platform_endian
                                       character_set = 'UTF-8' ).
  io_doc->set_encoding( lo_encode ).
*    ENDIF.

  lo_factory = lo_xml->create_stream_factory( ).

  " Return a xtring
  IF ev_xstr IS REQUESTED.
    CLEAR ev_xstr.
    lo_stream = lo_factory->create_ostream_xstring( string = ev_xstr ).
  ELSEIF ev_str IS REQUESTED. " Return a string
    CLEAR ev_str.
    lo_stream = lo_factory->create_ostream_cstring( string = ev_str ).
  ELSE.
    RETURN.
  ENDIF.

  lo_rendr = lo_xml->create_renderer( ostream  = lo_stream document = io_doc ).
  lo_rendr->render( ).

  " Change encoding. still utf-16 in header
  IF ev_str IS REQUESTED.
    REPLACE FIRST OCCURRENCE OF 'utf-16' IN ev_str WITH 'UTF-8'.
  ENDIF.
ENDMETHOD.


METHOD xml_to_zip.
  DATA:
   lv_value TYPE xstring.

  " From string
  IF iv_xdoc IS SUPPLIED.
    lv_value = iv_xdoc.
  ELSEIF iv_sdoc IS SUPPLIED.
    " Transform string to xString
    lv_value = string_to_xstring( iv_sdoc ).
  ELSEIF io_xmldoc IS SUPPLIED. " From object
    " Transform document to xString
    zcl_xtt_util=>xml_to_str(
     EXPORTING
       io_doc     = io_xmldoc
     IMPORTING
       ev_xstr    = lv_value ).
  ELSE.
    RETURN.
  ENDIF.

  " Delete from ZIP
  io_zip->delete( EXPORTING name = iv_name EXCEPTIONS OTHERS = 1 ).

  " Add to ZIP
  io_zip->add( name = iv_name content = lv_value ).
ENDMETHOD.


METHOD xstring_to_binary.
  " et_table = cl_bcs_convert=>xstring_to_solix( iv_xstring ).
  " ev_length = xstrlen( iv_xstring ).
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = iv_xstring
    IMPORTING
      output_length = ev_length
    TABLES
      binary_tab    = et_table.
ENDMETHOD.


METHOD xstring_to_string.
  DATA:
    lo_conv  TYPE REF TO cl_abap_conv_in_ce.

  lo_conv = cl_abap_conv_in_ce=>create(
   encoding = mc_utf8
   input    = iv_xstring ).

  lo_conv->read(
   IMPORTING
    data =  rv_string ).
ENDMETHOD.
ENDCLASS.

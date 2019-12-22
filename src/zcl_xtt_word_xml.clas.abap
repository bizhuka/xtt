class ZCL_XTT_WORD_XML definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  constants MC_WD_FORMAT_XML_DOCUMENT type I value 12 ##NO_TEXT.
  constants MC_WD_FORMAT_XML_DOCUMENT_EXT type STRING value '.docx' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_FILE_FORMAT type I default MC_WD_FORMAT_XML_DOCUMENT
      !IV_FILE_FORMAT_EXT type STRING default MC_WD_FORMAT_XML_DOCUMENT_EXT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_XTT_WORD_XML IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'w:body'
   iv_row_tag          = 'w:tr'
   iv_file_format      = iv_file_format
   iv_file_format_ext  = iv_file_format_ext
   iv_skip_tags        = abap_true
   iv_table_page_break = zcl_xtt_word_docx=>mc_table_page_break ).
ENDMETHOD.
ENDCLASS.

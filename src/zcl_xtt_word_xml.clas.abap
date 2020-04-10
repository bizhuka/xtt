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
private section.
ENDCLASS.



CLASS ZCL_XTT_WORD_XML IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'w:body'
   iv_row_tag          = 'w:tr'
   iv_ole_ext          = iv_ole_ext
   iv_ole_ext_format   = iv_ole_ext_format
   iv_skip_tags        = abap_true
   iv_table_page_break = zcl_xtt_word_docx=>mc_table_page_break ).

  " REPLACE ALL OCCURRENCES OF `.xml` IN mv_file_name WITH `.doc` IGNORING CASE.
ENDMETHOD.
ENDCLASS.

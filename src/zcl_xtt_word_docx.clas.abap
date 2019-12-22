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
protected section.
private section.
ENDCLASS.



CLASS ZCL_XTT_WORD_DOCX IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file             = io_file
   iv_body_tag         = 'w:body'
   iv_row_tag          = 'w:tr'
   iv_path_in_arc      = 'word/document.xml'
   iv_skip_tags        = abap_true
   iv_table_page_break = mc_table_page_break ).
ENDMETHOD.
ENDCLASS.

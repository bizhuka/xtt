class ZCL_XTT_WORD_DOCX definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_XTT_WORD_DOCX IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file        = io_file
   iv_body_tag    = 'w:body'
   iv_row_tag     = 'w:tr'
   iv_path_in_arc = 'word/document.xml'
   iv_skip_tags   = abap_true ).
ENDMETHOD.
ENDCLASS.

class ZCL_XTT_HTML definition
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



CLASS ZCL_XTT_HTML IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   io_file        = io_file
   iv_body_tag    = 'body'
   iv_row_tag     = 'tr' ).
ENDMETHOD.
ENDCLASS.

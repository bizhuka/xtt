class ZCL_XTT_FILE_SFP definition
  public
  final
  create public .

public section.

  interfaces ZIF_XTT_FILE .

  methods CONSTRUCTOR
    importing
      !IV_FORM_NAME type FPWBFORMNAME .
protected section.
private section.

  data MV_FILE_INFO type FPWBFORMNAME .
ENDCLASS.



CLASS ZCL_XTT_FILE_SFP IMPLEMENTATION.


method CONSTRUCTOR.
  ME->mv_file_info = iv_form_name.
endmethod.


METHOD zif_xtt_file~get_content.
ENDMETHOD.


METHOD zif_xtt_file~get_name.
  SELECT SINGLE text INTO rv_name
  FROM fpcontextt
  WHERE name     = me->mv_file_info
    AND state    = 'A'
    AND language = sy-langu
    AND id       = space.
ENDMETHOD.
ENDCLASS.

class ZCL_XTT_FILE_RAW definition
  public
  final
  create public .

public section.

  interfaces ZIF_XTT_FILE .

  methods CONSTRUCTOR
    importing
      !IV_NAME type CSEQUENCE
      !IV_STRING type STRING optional
      !IV_XSTRING type XSTRING optional .
protected section.
private section.

  data MV_NAME type STRING .
  data MV_XSTRING type XSTRING .
ENDCLASS.



CLASS ZCL_XTT_FILE_RAW IMPLEMENTATION.


METHOD constructor.
  mv_name = iv_name.

  IF iv_xstring IS SUPPLIED.
    mv_xstring = iv_xstring.
  ELSEIF iv_string IS SUPPLIED.
    mv_xstring = zcl_eui_conv=>string_to_xstring( iv_string ).
  ENDIF.
ENDMETHOD.


METHOD zif_xtt_file~get_content.
  CLEAR ev_as_xstring.
  CLEAR ev_as_string.

  IF ev_as_xstring IS REQUESTED.
    ev_as_xstring = mv_xstring.
  ENDIF.

  IF ev_as_string IS REQUESTED.
    ev_as_string = zcl_eui_conv=>xstring_to_string( mv_xstring ).
  ENDIF.
ENDMETHOD.


METHOD zif_xtt_file~get_name.
  rv_name = mv_name.
ENDMETHOD.
ENDCLASS.

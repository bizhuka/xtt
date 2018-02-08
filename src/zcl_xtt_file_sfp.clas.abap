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


method ZIF_XTT_FILE~GET_CONTENT.
*  " Detect file size
*  l_file_size = me->get_param( 'filesize' ).
*
*  " Result as a xstring
*  IF ex_as_xstring IS REQUESTED.
*    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*      EXPORTING
*        input_length = l_file_size
*      IMPORTING
*        buffer       = ex_as_xstring
*      TABLES
*        binary_tab   = <lt_table>.
*    RETURN.
*  ENDIF.
*
*  " Result as a string. if EX_AS_STRING Is Requested
*  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
*    EXPORTING
*      input_length = l_file_size
*      encoding     = zcl_xtt_util=>c_utf8
*    IMPORTING
*      text_buffer  = ex_as_string
*    TABLES
*      binary_tab   = <lt_table>.
endmethod.


METHOD zif_xtt_file~get_name.
  SELECT SINGLE text INTO rv_name
  FROM fpcontextt
  WHERE name     = me->mv_file_info
    AND state    = 'A'
    AND language = sy-langu
    AND id       = space.
ENDMETHOD.
ENDCLASS.

class ZCL_XTT_FILE_SMW0 definition
  public
  final
  create public .

public section.

  interfaces ZIF_XTT_FILE .

  methods CONSTRUCTOR
    importing
      !IV_OBJID type CSEQUENCE default SY-CPROG .
protected section.
private section.

  types:
    tyt_wwwparams TYPE SORTED TABLE OF wwwparams WITH UNIQUE KEY name .

  data MS_KEY type WWWDATATAB .
  data MT_WWWPARAMS type tyt_wwwparams .

  methods GET_PARAM
    importing
      !IV_NAME type WWWPARAMS-NAME
    returning
      value(RV_VALUE) type STRING .
ENDCLASS.



CLASS ZCL_XTT_FILE_SMW0 IMPLEMENTATION.


method CONSTRUCTOR.
  " Save key into
  ms_key-relid = 'MI'.
  ms_key-objid = iv_objid.

  " Tech info
  SELECT * INTO TABLE mt_wwwparams
  FROM wwwparams
  WHERE relid = ms_key-relid
    AND objid = ms_key-objid.
endmethod.


method GET_PARAM.
  FIELD-SYMBOLS:
   <ls_param>   LIKE LINE OF mt_wwwparams.
  " Just for optimisation
  READ TABLE mt_wwwparams ASSIGNING <ls_param>
   WITH TABLE KEY name = iv_name.
  " @see wwwparams db table
  IF sy-subrc = 0.
    rv_value = <ls_param>-value.
  ENDIF.
endmethod.


method ZIF_XTT_FILE~GET_CONTENT.
  DATA:
   lt_text      TYPE w3htmltabtype,
   lt_bin       TYPE w3mimetabtype,
   lv_file_size TYPE i.
  FIELD-SYMBOLS:
   <lt_table>   TYPE STANDARD TABLE.

  " Read by key
  CALL FUNCTION 'WWWDATA_IMPORT'
    EXPORTING
      key    = ms_key
    TABLES
      html   = lt_text
      mime   = lt_bin
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Could not load file' TYPE 'X'.
  ENDIF.

  " Text or binary
  IF ms_key-relid = zcl_smw0_file_info=>c_text.
    ASSIGN lt_text TO <lt_table>.
  ELSE.
    ASSIGN lt_bin  TO <lt_table>.
  ENDIF.

  " Detect file size
  lv_file_size = me->get_param( 'filesize' ).

  " Result as a xstring
  IF ev_as_xstring IS REQUESTED.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_file_size
      IMPORTING
        buffer       = ev_as_xstring
      TABLES
        binary_tab   = <lt_table>.
    RETURN.
  ENDIF.

  " Result as a string. if ev_as_STRING Is Requested
  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = lv_file_size
      encoding     = zcl_aok_util=>c_utf8
    IMPORTING
      text_buffer  = ev_as_string
    TABLES
      binary_tab   = <lt_table>.
endmethod.


METHOD zif_xtt_file~get_name.
  DATA:
   lv_ext TYPE string.

  " Read from parameters
  rv_name = me->get_param( 'filename' ).
  lv_ext  = me->get_param( 'fileextension' ).

  " Return only file name
  zcl_aok_util=>split_name(
    EXPORTING
     im_fullpath       = rv_name
    IMPORTING
     ex_filename_noext = rv_name ).

  " Sometimes path too long for extension
  CONCATENATE rv_name lv_ext INTO rv_name.
ENDMETHOD.
ENDCLASS.

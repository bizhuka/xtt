CLASS zcl_xtt_file_smw0 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_xtt_file .

    METHODS constructor
      IMPORTING
        !iv_objid TYPE csequence DEFAULT sy-cprog .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tyt_wwwparams TYPE SORTED TABLE OF wwwparams WITH UNIQUE KEY name .

    DATA ms_key TYPE wwwdatatab .
    DATA mt_wwwparams TYPE tyt_wwwparams .

    METHODS get_param
      IMPORTING
        !iv_name        TYPE wwwparams-name
      RETURNING
        VALUE(rv_value) TYPE string .
ENDCLASS.



CLASS ZCL_XTT_FILE_SMW0 IMPLEMENTATION.


  METHOD constructor.
    " Save key into
    ms_key-relid = 'MI'.
    ms_key-objid = iv_objid.

    " Tech info
    SELECT * INTO TABLE mt_wwwparams
    FROM wwwparams
    WHERE relid = ms_key-relid
      AND objid = ms_key-objid.
  ENDMETHOD.


  METHOD get_param.
    FIELD-SYMBOLS:
     <ls_param>   LIKE LINE OF mt_wwwparams.
    " Just for optimisation
    READ TABLE mt_wwwparams ASSIGNING <ls_param>
     WITH TABLE KEY name = iv_name.
    " @see wwwparams db table
    IF sy-subrc = 0.
      rv_value = <ls_param>-value.
    ENDIF.
  ENDMETHOD.


  METHOD zif_xtt_file~get_content.
    DATA:
      lt_text      TYPE w3htmltabtype,
      lt_bin       TYPE w3mimetabtype,
      lv_file_size TYPE i.
    FIELD-SYMBOLS:
     <lt_table>   TYPE STANDARD TABLE.

    CLEAR:
     ev_as_string,
     ev_as_xstring.

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
      MESSAGE e007(zsy_xtt) WITH ms_key-objid INTO sy-msgli.
      zcx_eui_no_check=>raise_sys_error( ).
    ENDIF.

    " Text or binary
    IF ms_key-relid = 'HT'.
      ASSIGN lt_text TO <lt_table>.
    ELSE.
      ASSIGN lt_bin  TO <lt_table>.
    ENDIF.

    " Detect file size
    lv_file_size = me->get_param( 'filesize' ).

    " Result as a xstring
    IF ev_as_xstring IS REQUESTED.
      ev_as_xstring = zcl_eui_conv=>binary_to_xstring(
       it_table  = <lt_table>
       iv_length = lv_file_size ).
    ENDIF.

    " Result as a string. if ev_as_STRING Is Requested
    IF ev_as_string IS REQUESTED.
      ev_as_string = zcl_eui_conv=>binary_to_string(
       it_table  = <lt_table>
       iv_length = lv_file_size ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_xtt_file~get_name.
    IF iv_mode = zif_xtt_file=>ms_name-verb.
      DATA lv_text TYPE wwwdata-text.
      SELECT SINGLE text INTO lv_text
      FROM wwwdata
      WHERE relid = ms_key-relid
        AND objid = ms_key-objid
        AND srtf2 = 0.
      rv_name = lv_text.
    ENDIF.

    IF iv_mode = zif_xtt_file=>ms_name-tech OR rv_name IS INITIAL.
      " Read from parameters
      rv_name = me->get_param( 'filename' ).

      " Return only file name
      zcl_eui_file=>split_file_path(
        EXPORTING iv_fullpath   = rv_name
        IMPORTING ev_file_noext = rv_name ).
    ENDIF.

    DATA lv_ext TYPE string.
    lv_ext = me->get_param( 'fileextension' ).

    " Sometimes path too long for extension
    CONCATENATE rv_name lv_ext INTO rv_name.
  ENDMETHOD.
ENDCLASS.

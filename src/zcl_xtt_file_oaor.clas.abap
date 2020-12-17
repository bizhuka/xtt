class ZCL_XTT_FILE_OAOR definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools SBDST .

  interfaces ZIF_XTT_FILE .

  methods CONSTRUCTOR
    importing
      !IV_CLASSNAME type SBDST_CLASSNAME
      !IV_CLASSTYPE type SBDST_CLASSTYPE default 'OT'
      !IV_OBJECT_KEY type SBDST_OBJECT_KEY
      !IV_FILENAME type CSEQUENCE .
protected section.
private section.

  types:
    BEGIN OF ts_signature,
      objid            TYPE bapicompo2-objid,
      file_name        TYPE bapicompo2-file_name,
      doc_ver_no       TYPE bapisignat-doc_ver_no,
      doc_id           TYPE bapisignat-doc_id,
      class            TYPE sdok_class,
*    bds_documenttype  TYPE string,
*    bds_documentclass TYPE string,
*    created_at        TYPE string,
*    created_by        TYPE string,
      description      TYPE string,
      language         TYPE string,
*    last_changed_at   TYPE string,
*    last_changed_by   TYPE string,
*    state             TYPE string,
      storage_category TYPE text128,
    END OF ts_signature .
  types:
    BEGIN OF ts_key,
      classname  TYPE sbdst_classname,
      classtype  TYPE sbdst_classtype,
      object_key TYPE sbdst_object_key,
      filename   TYPE string,
    END OF ts_key .

  data MS_KEY type TS_KEY .

  methods _READ_SIGNATURE
    returning
      value(RS_SIGNATURE) type TS_SIGNATURE .
ENDCLASS.



CLASS ZCL_XTT_FILE_OAOR IMPLEMENTATION.


METHOD constructor.
  ms_key-classname  = iv_classname.
  ms_key-classtype  = iv_classtype.
  ms_key-object_key = iv_object_key.
  ms_key-filename   = iv_filename.
ENDMETHOD.


METHOD zif_xtt_file~get_content.
  DATA:
    ls_object_id TYPE sdokobject,
    lt_info      TYPE STANDARD TABLE OF sdokfilaci,
    ls_info      TYPE REF TO sdokfilaci,
    lt_text      TYPE STANDARD TABLE OF sdokcntasc,
    lt_bin       TYPE STANDARD TABLE OF sdokcntbin,
    lv_file_size TYPE i.
  FIELD-SYMBOLS:
   <lt_table>   TYPE STANDARD TABLE.

  CLEAR ev_as_string.
  CLEAR ev_as_xstring.

  DATA ls_signature TYPE ts_signature.
  ls_signature = _read_signature( ).

  ls_object_id-class = ls_signature-class.
  ls_object_id-objid = ls_signature-objid.

  CALL FUNCTION 'SDOK_PHIO_LOAD_CONTENT'
    EXPORTING
      object_id           = ls_object_id
      text_as_stream      = abap_true
    TABLES
      file_access_info    = lt_info
      file_content_ascii  = lt_text
      file_content_binary = lt_bin
    EXCEPTIONS
      OTHERS              = 5.
  IF sy-subrc <> 0.
    MESSAGE e007(zsy_xtt) WITH ms_key-filename INTO sy-msgli.
    zcx_eui_no_check=>raise_sys_error( ).
  ENDIF.

  READ TABLE lt_info REFERENCE INTO ls_info INDEX 1.
  CHECK sy-subrc = 0.

  " Text or binary
  IF lt_bin[] IS NOT INITIAL.
    ASSIGN lt_bin  TO <lt_table>.
  ELSE.
    ASSIGN lt_text TO <lt_table>.
  ENDIF.

  " Detect file size
  lv_file_size = ls_info->file_size.

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
  " rv_name = ms_key-file_name.
  DATA ls_signature TYPE ts_signature.
  ls_signature = _read_signature( ).
  rv_name = ls_signature-file_name.
ENDMETHOD.


METHOD _read_signature.
  DATA:
    lt_signature         TYPE STANDARD TABLE OF ts_signature,
    ls_signature         TYPE REF TO ts_signature,
    lt_sbdst_signature   TYPE sbdst_signature,
    ls_sbdst_signature   TYPE REF TO bapisignat,
    lt_sbdst_components2 TYPE sbdst_components2,
    ls_sbdst_components2 TYPE REF TO bapicompo2.
  FIELD-SYMBOLS:
   <l_val>               TYPE csequence.

**********************************************************************
* To edit files in OAOR  -> SM30 -> V_TOADD
*   DOC application/msword  WORD.DOCUMENT.8
*   XLS application/vnd.ms-excel  EXCEL.SHEET.8
**********************************************************************

  " Finding the right documents
  cl_bds_document_set=>get_info(
   EXPORTING
    classname           = ms_key-classname
    classtype           = ms_key-classtype
    object_key          = ms_key-object_key
   IMPORTING
    extended_components = lt_sbdst_components2
    "connections         = lt_connect
   CHANGING
    signature           = lt_sbdst_signature
   EXCEPTIONS
    OTHERS              = 7 ).
  IF sy-subrc <> 0.
    MESSAGE e007(zsy_xtt) WITH ms_key-filename INTO sy-msgli.
    zcx_eui_no_check=>raise_sys_error( ).
  ENDIF.

  " lt_sbdst_signature structure is complex
  LOOP AT lt_sbdst_signature REFERENCE INTO ls_sbdst_signature.

    AT NEW doc_count.
      " Create new item
      APPEND INITIAL LINE TO lt_signature REFERENCE INTO ls_signature.
      ls_signature->doc_id     = ls_sbdst_signature->doc_id.
      ls_signature->doc_ver_no = ls_sbdst_signature->doc_ver_no.

      " Concatenate 2 tables
      READ TABLE lt_sbdst_components2 REFERENCE INTO ls_sbdst_components2
       INDEX ls_sbdst_signature->doc_count.
      ls_signature->objid     = ls_sbdst_components2->objid.
      ls_signature->file_name = ls_sbdst_components2->file_name.
      ls_signature->class     = ls_sbdst_components2->class.
    ENDAT.

    " Find field by name
    UNASSIGN <l_val>.
    ASSIGN COMPONENT ls_sbdst_signature->prop_name OF STRUCTURE ls_signature->* TO <l_val>.
    CHECK <l_val> IS ASSIGNED.

    <l_val> = ls_sbdst_signature->prop_value.
  ENDLOOP.

  " Apply filter
  DELETE lt_signature WHERE file_name <> ms_key-filename.

  " Max versions first
  SORT lt_signature BY doc_id doc_ver_no DESCENDING.

  " Read the last version
  READ TABLE lt_signature INTO rs_signature INDEX 1.

  CHECK sy-subrc <> 0.
  MESSAGE e007(zsy_xtt) WITH ms_key-filename INTO sy-msgli.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.
ENDCLASS.

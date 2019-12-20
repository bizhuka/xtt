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
PRIVATE SECTION.

  TYPES:
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
    END OF ts_signature.

  DATA ms_signature TYPE ts_signature.
ENDCLASS.



CLASS ZCL_XTT_FILE_OAOR IMPLEMENTATION.


METHOD constructor.
  DATA:
    lt_signature         TYPE STANDARD TABLE OF ts_signature,
    ls_signature         TYPE REF TO ts_signature,

    lt_sbdst_signature   TYPE sbdst_signature,
    ls_sbdst_signature   TYPE REF TO bapisignat,
    lt_sbdst_components2 TYPE sbdst_components2,
    ls_sbdst_components2 TYPE REF TO bapicompo2,
*    lt_connect           TYPE srm_bdsconn_t,
*    ls_connect           TYPE REF TO bapiconnec,
*    lv_maxver            LIKE iv_version,
*    lv_type              TYPE toav0-sap_object,
*    lv_objid             TYPE toav0-object_id,
*    lt_conn              TYPE STANDARD TABLE OF toav0,
*    ls_conn              TYPE REF TO toav0,
    lv_cnt               TYPE i.
  FIELD-SYMBOLS:
   <l_val>               TYPE csequence.

*  " Special case for archive link or GOS(Generic Object Services)
*  IF iv_archiv_link = abap_true.
*    lv_type  = iv_classname.
*    lv_objid = iv_object_key.
*    CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
*      EXPORTING
*        objecttype    = lv_type
*        object_id     = lv_objid
*      TABLES
*        connections   = lt_conn
*      EXCEPTIONS
*        OTHERS        = 2.
*    CHECK sy-subrc = 0.
*
*    " Get last one
*    SORT lt_conn BY ar_date.
*    lv_cnt = lines( lt_conn ).
*    READ TABLE lt_conn REFERENCE INTO ls_conn INDEX lv_cnt.
*
*    " Last version by date
*    IF sy-subrc = 0.
*      ms_signature-storage_category = ls_conn->archiv_id.
*      ms_signature-objid            = ls_conn->arc_doc_id.
*
*      " TODO only from importing parameter
*      ms_signature-file_name        = iv_filename.
*    ENDIF.
*
*    RETURN.
*  ENDIF.

**********************************************************************
* To edit files in OAOR  -> SM30 -> V_TOADD
*   DOC application/msword  WORD.DOCUMENT.8
*   XLS application/vnd.ms-excel  EXCEL.SHEET.8
**********************************************************************

  " Finding the right documents
  cl_bds_document_set=>get_info(
   EXPORTING
    classname           = iv_classname
    classtype           = iv_classtype
    object_key          = iv_object_key
   IMPORTING
    extended_components = lt_sbdst_components2
    "connections         = lt_connect
   CHANGING
    signature           = lt_sbdst_signature
   EXCEPTIONS
    OTHERS              = 7 ).
  CHECK sy-subrc = 0.

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
  DELETE lt_signature WHERE file_name <> iv_filename.

  " Read the last version
  lv_cnt = lines( lt_signature ).
  READ TABLE lt_signature INTO ms_signature INDEX lv_cnt.

  CHECK sy-subrc <> 0.
  MESSAGE 'Unable to detect the file'(udf) TYPE 'X'.
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

  ls_object_id-class = ms_signature-class.
  ls_object_id-objid = ms_signature-objid.

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
  CHECK sy-subrc = 0.

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
    ev_as_xstring = zcl_xtt_util=>binary_to_xstring(
     it_table  = <lt_table>
     iv_length = lv_file_size ).
    RETURN.
  ENDIF.

  " Result as a string. if ev_as_STRING Is Requested
  ev_as_string = zcl_xtt_util=>binary_to_string(
   it_table  = <lt_table>
   iv_length = lv_file_size ).
ENDMETHOD.


method ZIF_XTT_FILE~GET_NAME.
  rv_name = ms_signature-file_name.
endmethod.
ENDCLASS.

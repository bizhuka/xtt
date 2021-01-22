class ZCL_XTT_XML_UPDATER definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF C_STATUS,
    initial        type i value 0,
    empty_template type i value 1,
    empty          type i value 2,
    changed        type i value 3, " Need to save
    saved          type i value 4,
   END OF C_STATUS .
  data STR_STATUS type I read-only .

  methods CONSTRUCTOR
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IV_PATH type CSEQUENCE
      !IV_STR_TAG type STRING optional
      !IV_STR_DOC type STRING optional .
  methods REPLACE
    importing
      !IV_TAG type STRING
      !IT_TAGS type STRINGTAB optional
      !IV_PART_ATTRIBUTE type CSEQUENCE optional
      !IV_PART_VALUE type CSEQUENCE optional
    returning
      value(RR_TAG) type ref to IF_IXML_ELEMENT .
  methods STR_ADD
    importing
      !IV_TAG type STRING .
  methods STR_GET_DOCUMENT
    returning
      value(RV_DOC) type STRING .
  methods STR_DELETE_PART
    importing
      !IV_BEG type I
      !IV_END type I .
  methods SAVE .
  methods GET_RAW_XML
    returning
      value(RV_XML) type STRING .
  methods GET_DOCUMENT
    returning
      value(RO_DOC) type ref to IF_IXML_DOCUMENT .
protected section.
private section.

  types:
    BEGIN OF TS_NEW_TAG,
     _from TYPE STRING,
     _to   TYPE STRING,
   END OF TS_NEW_TAG .
  types:
    TT_NEW_TAG TYPE STANDARD TABLE OF TS_NEW_TAG WITH DEFAULT KEY .

  data MO_ZIP type ref to CL_ABAP_ZIP .
  data MV_PATH type STRING .
  data _DOC type ref to IF_IXML_DOCUMENT .
  data T_NEW_TAG type TT_NEW_TAG .
  data STR_TAG type STRING .
  data STR_DOC type STRING .

  methods _GET_ANCHOR
    importing
      !IV_TAG type CSEQUENCE
    returning
      value(RV_ANCHOR) type STRING .
ENDCLASS.



CLASS ZCL_XTT_XML_UPDATER IMPLEMENTATION.


METHOD constructor.
  mo_zip  = io_zip.
  mv_path = iv_path.

  " Do not replace any XML, just add
  str_tag  = iv_str_tag.
  str_doc  = iv_str_doc.

  " Check template from the archive
  CHECK str_doc IS NOT INITIAL.
  str_get_document( ).
ENDMETHOD.


METHOD get_document.
  ro_doc = _doc.
  CHECK ro_doc IS INITIAL.

  " Late initialization
  zcl_eui_conv=>xml_from_zip( EXPORTING io_zip     = mo_zip
                                        iv_name    = mv_path
                              IMPORTING eo_xmldoc  = _doc ).
  ro_doc = _doc.
ENDMETHOD.


METHOD get_raw_xml.
  DATA lo_xml LIKE _doc.
  lo_xml = get_document( ).

  " Transform to string
  zcl_eui_conv=>xml_to_str( EXPORTING io_doc = lo_xml
                            IMPORTING ev_str = rv_xml ).

  FIELD-SYMBOLS <ls_new_tag> LIKE LINE OF t_new_tag.
  LOOP AT t_new_tag ASSIGNING <ls_new_tag>.
    REPLACE FIRST OCCURRENCE OF <ls_new_tag>-_from IN rv_xml
                           WITH <ls_new_tag>-_to.
  ENDLOOP.

  " Ready for new actions
  CLEAR: t_new_tag,
         _doc.
ENDMETHOD.


METHOD replace.
  DATA lo_xml LIKE _doc.
  lo_xml = get_document( ).

  " parent node
  rr_tag = lo_xml->find_from_name( iv_tag ).
  CHECK rr_tag IS NOT INITIAL.

  " new replacement
  FIELD-SYMBOLS <ls_new_tag> LIKE LINE OF t_new_tag.

  " From text
  APPEND INITIAL LINE TO t_new_tag ASSIGNING <ls_new_tag>.
  <ls_new_tag>-_from = _get_anchor( iv_tag ).

  " To text
  CONCATENATE LINES OF it_tags INTO <ls_new_tag>-_to.

  " Replace all children
  IF iv_part_attribute IS INITIAL.
    rr_tag->set_value( <ls_new_tag>-_from ).
    RETURN.
  ENDIF.

**********************************************************************
  " Replace part
  DATA lo_text TYPE REF TO if_ixml_text.
  lo_text = lo_xml->create_text( <ls_new_tag>-_from ).

  IF iv_part_attribute <> abap_undefined.
    " Delete children elemts by condition
    DATA lo_children TYPE REF TO if_ixml_node_list.
    lo_children = rr_tag->get_children( ).

    DATA lv_count TYPE i.
    lv_count = lo_children->get_length( ).
    " Delete from the end
    WHILE lv_count >= 1.
      lv_count = lv_count - 1.
      DATA lo_elem  TYPE REF TO if_ixml_element.
      lo_elem ?= lo_children->get_item( lv_count ).

      DATA lv_attribute TYPE string.
      lv_attribute = lo_elem->get_attribute( iv_part_attribute ).

      " Yes delete child element
      CHECK lv_attribute CP iv_part_value.

      rr_tag->remove_child( lo_elem ).
    ENDWHILE.
  ENDIF.

  " To the end
  rr_tag->append_child( lo_text ).
ENDMETHOD.


METHOD save.
  DATA lv_xml TYPE string.

  " Object mode with replace
  IF str_tag IS INITIAL.
    lv_xml = get_raw_xml( ).
  ELSE.
    " Text mode with append
    lv_xml = str_doc.
    " Just skip saving
    CHECK str_status = c_status-changed.
  ENDIF.

  " Paste data to ZIP file
  zcl_eui_conv=>xml_to_zip( io_zip    = mo_zip
                            iv_name   = mv_path
                            iv_sdoc   = lv_xml ).

  " Init again
  CHECK str_tag IS NOT INITIAL.
  str_status = c_status-empty.
ENDMETHOD.


METHOD str_add.
  IF str_status = c_status-initial. " str_doc IS INITIAL.
    str_doc = str_get_document( ).
  ENDIF.

  " Do not insert if already exist, 1 time only
  CHECK str_doc NS iv_tag.
  str_status = c_status-changed.

  DATA lv_len TYPE i.
  lv_len = strlen( str_doc ) - strlen( `</>` ) - strlen( str_tag ).
  CONCATENATE str_doc(lv_len) iv_tag `</` str_tag `>` INTO str_doc.
ENDMETHOD.


METHOD str_delete_part.
  CONCATENATE str_doc(iv_beg)
              str_doc+iv_end INTO str_doc.
*  str_status = c_status-changed.
ENDMETHOD.


METHOD str_get_document.
  " Read 1 time only
  IF str_status <> c_status-initial.
    rv_doc = str_doc.
    RETURN.
  ENDIF.

  " Yes initilized
  str_status = c_status-empty.

  " Late initialization
  DATA lv_xml LIKE str_doc.
  zcl_eui_conv=>xml_from_zip( EXPORTING io_zip  = mo_zip
                                        iv_name = mv_path
                              IMPORTING ev_sdoc = lv_xml ).

  " No file in archive
  IF lv_xml IS INITIAL.
    str_status = c_status-empty_template.
    rv_doc = str_doc.
    RETURN.
  ENDIF.

  " from zip
  rv_doc = str_doc = lv_xml.
ENDMETHOD.


METHOD _get_anchor.
  CONCATENATE `__@@` iv_tag `@@__` INTO rv_anchor.
ENDMETHOD.
ENDCLASS.

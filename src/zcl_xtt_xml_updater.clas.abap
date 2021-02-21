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
  methods OBJ_GET_DOCUMENT
    returning
      value(RO_DOC) type ref to IF_IXML_DOCUMENT .
  methods OBJ_REPLACE
    importing
      !IV_TAG type STRING
      !IT_TAGS type STRINGTAB
      !IV_TAG_RELAT type STRING optional
      !IV_PART_ATTRIBUTE type CSEQUENCE optional
      !IV_PART_VALUE type CSEQUENCE optional
    returning
      value(RR_TAG) type ref to IF_IXML_ELEMENT .
  methods OBJ_REPLACE_TEXT
    importing
      !IV_FROM type STRING
      !IV_TO type STRING .
  methods SAVE
    importing
      !IV_PATH type STRING optional .
  methods STR_ADD
    importing
      !IV_TAG type STRING .
  methods STR_DELETE_PART
    importing
      !IV_BEG type I
      !IV_END type I .
  methods STR_GET_DOCUMENT
    returning
      value(RV_DOC) type STRING .
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

  methods _OBJ_GET_ANCHOR
    importing
      !IV_TAG type CSEQUENCE
    returning
      value(RV_ANCHOR) type STRING .
  methods _OBJ_GET_DOCUMENT_STR
    returning
      value(RV_XML) type STRING .
  methods _OBJ_ADD_RELAT
    importing
      !IV_TAG type STRING
      !IV_TAG_RELAT type STRING
    returning
      value(RR_TAG) type ref to IF_IXML_ELEMENT .
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


METHOD obj_get_document.
  ro_doc = _doc.
  CHECK ro_doc IS INITIAL.

  " Late initialization
  zcl_eui_conv=>xml_from_zip( EXPORTING io_zip     = mo_zip
                                        iv_name    = mv_path
                              IMPORTING eo_xmldoc  = _doc ).
  ro_doc = _doc.
ENDMETHOD.


METHOD obj_replace.
  DATA lo_xml LIKE _doc.
  lo_xml = obj_get_document( ).

  " Serach in parent node
  rr_tag = lo_xml->find_from_name( iv_tag ).
  IF rr_tag IS INITIAL AND it_tags[] IS NOT INITIAL.
    rr_tag = _obj_add_relat( iv_tag       = iv_tag
                             iv_tag_relat = iv_tag_relat ).
  ENDIF.
  CHECK rr_tag IS NOT INITIAL.

  " new replacement
  FIELD-SYMBOLS <ls_new_tag> LIKE LINE OF t_new_tag.
  APPEND INITIAL LINE TO t_new_tag ASSIGNING <ls_new_tag>.

  " From - To
  <ls_new_tag>-_from = _obj_get_anchor( iv_tag ).
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


METHOD obj_replace_text.
  " new replacement
  FIELD-SYMBOLS <ls_new_tag> LIKE LINE OF t_new_tag.
  APPEND INITIAL LINE TO t_new_tag ASSIGNING <ls_new_tag>.

  " From - To
  <ls_new_tag>-_from = iv_from.
  <ls_new_tag>-_to   = iv_to.
ENDMETHOD.


METHOD save.
  DATA lv_xml TYPE string.

  " Object mode with replace
  IF str_tag IS INITIAL.
    lv_xml = _obj_get_document_str( ).
  ELSE.
    " Text mode with append
    lv_xml = str_doc.
    " Just skip saving
    CHECK str_status = c_status-changed.
  ENDIF.

  " Paste data to ZIP file
  DATA lv_path TYPE string.
  IF iv_path IS NOT INITIAL.
    lv_path = iv_path.
  ELSE.
    lv_path = mv_path.
  ENDIF.
  zcl_eui_conv=>xml_to_zip( io_zip    = mo_zip
                            iv_name   = lv_path
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


METHOD _obj_add_relat.
  " Where to insert ?
  CHECK iv_tag_relat IS NOT INITIAL.

  DATA: lv_tag_relat LIKE iv_tag_relat, lv_oper TYPE char1.
  lv_tag_relat = iv_tag_relat+1.
  lv_oper      = iv_tag_relat(1).

  DATA lo_xml LIKE _doc.
  lo_xml = obj_get_document( ).

  " Insert before
  DATA lr_relat TYPE REF TO if_ixml_element.
  lr_relat = lo_xml->find_from_name( lv_tag_relat ).
  CHECK lr_relat IS NOT INITIAL.

  " Insert after ?
  IF lv_oper = '+'.
    lr_relat ?= lr_relat->get_next( ).
  ENDIF.
  CHECK lr_relat IS NOT INITIAL.

  " Result tag
  rr_tag = lo_xml->create_element( iv_tag ).

  " Use XML root
  DATA lo_root TYPE REF TO if_ixml_node.
  lo_root = lo_xml->get_first_child( ).
  lo_root->insert_child( new_child = rr_tag
                         ref_child = lr_relat ).
ENDMETHOD.


METHOD _obj_get_anchor.
  CONCATENATE `__@@` iv_tag `@@__` INTO rv_anchor.
ENDMETHOD.


METHOD _obj_get_document_str.
  DATA lo_xml LIKE _doc.
  lo_xml = obj_get_document( ).

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
ENDCLASS.

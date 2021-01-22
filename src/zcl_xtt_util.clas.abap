class ZCL_XTT_UTIL definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_xml_match,
      tag TYPE string,
      beg TYPE i,
      end TYPE i,
      cnt TYPE i,
    END OF ts_xml_match .
  types:
    tt_xml_match TYPE STANDARD TABLE OF ts_xml_match WITH DEFAULT KEY .
  types:
    BEGIN OF ts_replace,
      from TYPE string,
      to   TYPE string,
    END OF ts_replace .
  types:
    tt_replace TYPE STANDARD TABLE OF ts_replace WITH DEFAULT KEY .
  types:
    BEGIN OF TS_FORMAT_FIELD,
     name TYPE STRING,
     ref  TYPE REF TO DATA,
   END OF TS_FORMAT_FIELD .
  types:
    TT_FORMAT_FIELD TYPE SORTED TABLE OF TS_FORMAT_FIELD WITH UNIQUE KEY name .

  class-methods GET_FROM_TAG
    importing
      !IV_BEG_PART type CSEQUENCE
      !IV_END_PART type CSEQUENCE
      !IV_NEW_NAME type CSEQUENCE optional
    exporting
      !EV_VALUE0 type ANY
      !EV_VALUE1 type ANY
      !EV_VALUE2 type ANY
    changing
      !CV_XML type STRING .
  class-methods GET_TAG_MATCHES
    importing
      !IV_CONTEXT type STRING
      !IV_TAG type CSEQUENCE
      !IV_TAG_ADD type CSEQUENCE optional
    returning
      value(RT_MATCH) type MATCH_RESULT_TAB .
  class-methods GET_XML_MATCHES
    importing
      !IV_CONTEXT type STRING
      !IV_TAG type CSEQUENCE
    returning
      value(RT_XML_MATCH) type TT_XML_MATCH .
*"* public components of class ZCL_XTT_UTIL
*"* do not include other source files here!!!
  class-methods HIDE_EXCEL_WARNING .
  class-methods IS_COMMON_GUI
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods REPLACE_1ST_FROM
    importing
      !IT_REPLACE type TT_REPLACE
      !IV_FROM type I
    changing
      !CV_XML type STRING .
  class-methods REPLACE_1ST
    importing
      !IV_FROM type CSEQUENCE
      !IV_TO type CSEQUENCE
      !IV_POS type I
      !IV_KEEP_LEN type ABAP_BOOL optional
    changing
      !CV_XML type STRING .
  class-methods CHECK_LOG_MESSAGE
    importing
      !IO_LOGGER type ref to ZCL_EUI_LOGGER
      !IV_MESSAGES type STRING .
  class-methods CREATE_ROOT
    importing
      !IT_FORMAT_FIELD type TT_FORMAT_FIELD
    returning
      value(RR_ROOT) type ref to DATA .
  class-methods REPEAT
    importing
      !VAL type CSEQUENCE
      !OCC type I
    returning
      value(RESULT) type STRING .
  class-methods IS_TABLE
    importing
      !IS_ROOT type ANY
    returning
      value(RV_IS_TABLE) type ABAP_BOOL .
protected section.
*"* protected components of class ZCL_XTT_UTIL
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_XTT_UTIL IMPLEMENTATION.


METHOD check_log_message.
  DATA lt_message TYPE sfb_t_bal_s_msg.
  lt_message = io_logger->get_messages( ).

  IF lt_message IS NOT INITIAL AND iv_messages IS INITIAL.
    zcl_eui_conv=>assert_equals( act = ' '
                                 exp = 'X'
                                 msg = `Should be no errors & warnings` ).
  ENDIF.

  " Parse messages
  DATA lt_pair TYPE stringtab.
  SPLIT iv_messages AT ';' INTO TABLE lt_pair.

  DATA lv_exp_cnt TYPE i.
  DATA lv_act_cnt TYPE i.
  lv_exp_cnt = lines( lt_pair ).
  lv_act_cnt = lines( lt_message ).
  zcl_eui_conv=>assert_equals( act = lv_act_cnt
                               exp = lv_exp_cnt
                               msg = `Number of logs differs` ).

  DATA lv_pair TYPE string.
  LOOP AT lt_pair INTO lv_pair.
    SPLIT lv_pair AT '-' INTO sy-msgid sy-msgno.
    READ TABLE lt_message TRANSPORTING NO FIELDS
     WITH KEY msgid = sy-msgid
              msgno = sy-msgno.
    CHECK sy-subrc <> 0.

    DATA lv_message TYPE string.
    CONCATENATE `No message ` sy-msgno ` found in log` INTO lv_message.
    zcl_eui_conv=>assert_equals( act = ' '
                                 exp = 'X'
                                 msg = lv_message ).
  ENDLOOP.
ENDMETHOD.


METHOD create_root.
  DATA lt_comp TYPE cl_abap_structdescr=>component_table.
  DATA ls_comp LIKE LINE OF lt_comp.

  " No need
  CHECK it_format_field IS NOT INITIAL.

  FIELD-SYMBOLS <ls_field> LIKE LINE OF it_format_field.
  LOOP AT it_format_field ASSIGNING <ls_field>.
    ls_comp-name = <ls_field>-name.

    ls_comp-type ?= cl_abap_typedescr=>describe_by_data_ref( <ls_field>-ref ).
    INSERT ls_comp INTO TABLE lt_comp.
  ENDLOOP.

  " New strcuture
  DATA lr_handle TYPE REF TO cl_abap_structdescr.
  lr_handle = cl_abap_structdescr=>create( lt_comp ).
  CREATE DATA rr_root TYPE HANDLE lr_handle.

  " As structure
  FIELD-SYMBOLS <ls_root> TYPE any.
  ASSIGN rr_root->* TO <ls_root>.

  " Copy data to root
  FIELD-SYMBOLS <lv_dest> TYPE any.
  FIELD-SYMBOLS <lv_src>  TYPE any.
  LOOP AT it_format_field ASSIGNING <ls_field>.
    ASSIGN COMPONENT <ls_field>-name OF STRUCTURE <ls_root> TO <lv_dest>.
    ASSIGN <ls_field>-ref->* TO <lv_src>.

    <lv_dest> = <lv_src>.
  ENDLOOP.
ENDMETHOD.


METHOD get_from_tag.
  DATA lv_offset TYPE i.
  DATA lv_index  TYPE num1.
  DATA lv_cnt    TYPE i.
  DATA lv_name   TYPE string.
  FIELD-SYMBOLS <lv_value> TYPE any.

  CLEAR ev_value0.
  CLEAR ev_value1.
  CLEAR ev_value2.

  WHILE cv_xml+lv_offset CS iv_beg_part.
    lv_offset = lv_offset + sy-fdpos + strlen( iv_beg_part ).

    " Oops
    IF cv_xml+lv_offset NS iv_end_part.
      zcx_xtt_exception=>raise_dump( iv_message = `No ending tag` ).
    ENDIF.
    lv_cnt = sy-fdpos.

    " Get value
    DATA lv_value TYPE string.
    lv_value  = cv_xml+lv_offset(lv_cnt).

    IF iv_new_name IS NOT INITIAL.
      CONCATENATE iv_new_name lv_index INTO lv_name.
      REPLACE SECTION OFFSET lv_offset LENGTH lv_cnt OF cv_xml WITH lv_name.
    ENDIF.

    CONCATENATE 'EV_VALUE' lv_index INTO lv_name.
    ASSIGN (lv_name) TO <lv_value>.
    IF sy-subrc <> 0.
      zcx_xtt_exception=>raise_dump( iv_message = `Error in logic` ).
    ENDIF.

    <lv_value> = lv_value.
    lv_offset  = lv_offset + lv_cnt.

    " Go on ?
    lv_index   = lv_index  + 1.
    CASE lv_index.
      WHEN 1. IF ev_value1 IS NOT REQUESTED. RETURN. ENDIF.
      WHEN 2. IF ev_value2 IS NOT REQUESTED. RETURN. ENDIF.
    ENDCASE.
  ENDWHILE.
ENDMETHOD.


METHOD get_tag_matches.
  " Divide text by body tag. Usualy 2 matches. <body> and </body>
  DATA lv_pattern TYPE string.
  CONCATENATE `(<` iv_tag `\b[^>]*>)|(<\/` iv_tag `>)` INTO lv_pattern.

  " Also find this part
  IF iv_tag_add IS NOT INITIAL.
    CONCATENATE lv_pattern `|(` iv_tag_add `)` INTO lv_pattern.
  ENDIF.

  FIND ALL OCCURRENCES OF REGEX lv_pattern IN iv_context RESULTS rt_match RESPECTING CASE.
ENDMETHOD.


METHOD get_xml_matches.
  DATA lt_match TYPE match_result_tab.

  " Return ready XML
  lt_match = get_tag_matches( iv_context = iv_context
                              iv_tag     = iv_tag ).

  DATA ls_xml_match LIKE LINE OF rt_xml_match.
  FIELD-SYMBOLS <ls_match_beg> LIKE LINE OF lt_match.
  FIELD-SYMBOLS <ls_match_end> LIKE LINE OF lt_match.

  " Safe delete from the end
  DATA lv_index TYPE i.
  lv_index = lines( lt_match ).

  WHILE lv_index > 0.
    READ TABLE lt_match ASSIGNING <ls_match_end> INDEX lv_index.
    lv_index = lv_index - 1.

    READ TABLE lt_match ASSIGNING <ls_match_beg> INDEX lv_index.
    lv_index = lv_index - 1.

    " Get tag from match
    ls_xml_match-beg = <ls_match_beg>-offset.
    ls_xml_match-end = <ls_match_end>-offset + <ls_match_end>-length.
    ls_xml_match-cnt = ls_xml_match-end - ls_xml_match-beg.

    " And whole string
    ls_xml_match-tag = iv_context+ls_xml_match-beg(ls_xml_match-cnt).

    " Ready line
    APPEND ls_xml_match TO rt_xml_match.
  ENDWHILE.
ENDMETHOD.


METHOD hide_excel_warning.
  " Hide the warning for Excel (For Word is ok to change file ext from .xml to .doc)
  " The file you are trying to open, '[filename]', is in a different format than specified by the file extension
  DATA:
    lv_value TYPE string,
    lv_len   TYPE i.                                        "#EC NEEDED

  " Can change registry
  CHECK is_common_gui( ) = abap_true.

  " Get current excel version
  cl_gui_frontend_services=>registry_get_value(
   EXPORTING
    root                 = cl_gui_frontend_services=>hkey_classes_root
    key                  = 'Excel.Application\CurVer'
   IMPORTING
    reg_value            = lv_value
   EXCEPTIONS
    OTHERS               = 1 ).
  CHECK sy-subrc = 0 AND lv_value IS NOT INITIAL.

  " two last digits are excel current version
  lv_len   = strlen( lv_value ).                            " - 2
  lv_len   = lv_len - 2.
  lv_value = lv_value+lv_len.

  " Write value
  CONCATENATE `Software\Microsoft\Office\` lv_value `.0\Excel\Security` INTO lv_value.
  cl_gui_frontend_services=>registry_set_dword_value(
   EXPORTING
    root                 = cl_gui_frontend_services=>hkey_current_user
    key                  = lv_value
    value                = 'ExtensionHardening'
    dword_value          = 0
   IMPORTING
    rc                   = lv_len
   EXCEPTIONS
    OTHERS               = 1 ).

  CHECK sy-subrc = 0.
  cl_gui_cfw=>flush( ).
ENDMETHOD.


METHOD is_common_gui.
  " Background process
  CHECK sy-batch <> abap_true.

  " Web dynpro  (TODO SAPUI5)
  CHECK wdr_task=>application IS INITIAL.

  " Is Ok
  rv_ok = abap_true.
ENDMETHOD.


METHOD is_table.
  DATA lv_type_kind TYPE abap_typekind.
  DESCRIBE FIELD is_root TYPE lv_type_kind.

  CHECK lv_type_kind = cl_abap_typedescr=>typekind_table.
  rv_is_table = abap_true.
ENDMETHOD.


METHOD repeat.
  DO occ TIMES.
    CONCATENATE result val INTO result RESPECTING BLANKS.
  ENDDO.
ENDMETHOD.


METHOD replace_1st.
  DATA: lv_len TYPE i, lv_off TYPE i.
  lv_len = strlen( cv_xml ).
  CHECK lv_len > iv_pos.

  FIND FIRST OCCURRENCE OF REGEX iv_from IN cv_xml+iv_pos MATCH LENGTH lv_len
                                                          MATCH OFFSET lv_off.
  CHECK sy-subrc = 0.

  " Fill with blanks ?
  DATA lv_to TYPE string.
  lv_to = iv_to.

  IF iv_keep_len = abap_true.
    DATA lv_cnt TYPE i.
    lv_cnt = lv_len - strlen( lv_to ).

    DATA lv_rem TYPE string.
    lv_rem = repeat( val = ` `
                     occ = lv_cnt ).
    CONCATENATE lv_to lv_rem INTO lv_to.
  ENDIF.

  lv_off = lv_off + iv_pos.
  REPLACE SECTION OFFSET lv_off LENGTH lv_len OF cv_xml WITH lv_to.
ENDMETHOD.


METHOD replace_1st_from.
  DATA lv_offset TYPE i.
  DATA lv_len    TYPE i.
  FIELD-SYMBOLS <ls_replace> LIKE LINE OF it_replace.

  LOOP AT it_replace ASSIGNING <ls_replace>.
    FIND FIRST OCCURRENCE OF <ls_replace>-from IN SECTION OFFSET iv_from OF cv_xml
                                                    MATCH OFFSET lv_offset.
    CHECK sy-subrc = 0.

    " Replcae with value
    lv_len = strlen( <ls_replace>-from ).
    REPLACE SECTION OFFSET lv_offset LENGTH lv_len OF cv_xml WITH <ls_replace>-to.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.

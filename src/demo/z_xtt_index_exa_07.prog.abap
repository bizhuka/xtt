*&---------------------------------------------------------------------*
* Most of the tasks could be performed without macro.
* Macro cannot run in background and executed only in MS Office at Windows.
* This drawbacks and security issues narrow the scope of VBA  macro.
* That's why I highly do not recommend to use them at all.
*&---------------------------------------------------------------------*

METHOD example_07.
  DATA:
    lo_file    TYPE REF TO zif_xtt_file,
    lv_ole_app TYPE ole2_object.

  " Show data structure only
  IF p_stru = abap_true.
    MESSAGE 'No data at all'(nda) TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Info about template & the main class itself
  CREATE OBJECT:
   lo_file TYPE zcl_xtt_file_smw0 EXPORTING
     iv_objid = iv_template,

   ro_xtt TYPE (iv_class_name) EXPORTING
    io_file = lo_file.

  SET HANDLER:
   on_prepare_raw_07 FOR ro_xtt.

  CASE 'X'.
    WHEN p_dwnl.
      ro_xtt->download(        " All parameters are optional
       EXPORTING
        iv_open     = zcl_xtt=>mc_by_ole " Open with ole
       CHANGING
        cv_fullpath = p_path
        cv_ole_app  = lv_ole_app ).      " Get ole2_object back

      " Call macro Ok
      check_ole_07( io_ole_app = lv_ole_app ).

    WHEN p_show.
      ro_xtt->show( io_handler = me ).
  ENDCASE.

  " Skip further execution
  CLEAR ro_xtt.
ENDMETHOD.

" Macro always will execute for download method (regardless VBA security level)
" Show method depends on security options (that's why do not call macro)
METHOD on_pbo_07.
  " Just for test. @SHOW( )
  APPEND 'EXIT' TO sender->ms_status-exclude.
  APPEND 'CANC' TO sender->ms_status-exclude.

  " 1 time only
  CHECK io_container IS NOT INITIAL.

  DATA lo_eui_file TYPE REF TO zcl_eui_file.
  DATA ls_ole_info TYPE zcl_eui_file=>ts_ole_info.

  lo_eui_file ?= sender.
  ls_ole_info = lo_eui_file->get_ole_info( ).

  " Call macro Error
  check_ole_07( io_ole_app = ls_ole_info-app ).
ENDMETHOD.

METHOD check_ole_07.
  DATA lv_charts TYPE ole2_object.

  " Better to use XML than macro
  CHECK io_ole_app IS NOT INITIAL.

  " @see on_prepare_raw_07
  CALL METHOD OF io_ole_app 'Run'
    EXPORTING
      #1 = 'MAIN.start'
      #2 = 'From SAP'. "#EC NOTEXT

  " OR Call OLE like that
  SET PROPERTY OF io_ole_app 'StatusBar' = 'OLE Call'.      "#EC NOTEXT

  GET PROPERTY OF io_ole_app 'Charts' = lv_charts.
  CALL METHOD OF lv_charts 'Add'.
ENDMETHOD.

" Use for XML parsing (In the end of GET_RAW)
METHOD on_prepare_raw_07.
  DATA:
    lo_zip         TYPE REF TO cl_abap_zip,
    lo_desc        TYPE REF TO cl_abap_classdescr,
    lv_class_name  TYPE string,
    lv_path_in_arc TYPE string,
    lo_xml         TYPE REF TO if_ixml_document,
    lo_col         TYPE REF TO if_ixml_element,
    lv_mod         TYPE i.
  FIELD-SYMBOLS:
    <lv_content> TYPE xstring.

  " As xstring
  ASSIGN ir_content->* TO <lv_content>.

  " Load zip archive from XSTRING
  CREATE OBJECT lo_zip.
  lo_zip->load( <lv_content> ).

  " Get class name
  lo_desc ?= cl_abap_classdescr=>describe_by_object_ref( sender ).
  lv_class_name = lo_desc->get_relative_name( ).

  " For XML is just the documnet itself
  " For new MS Office Open XML formats is a zip archive
  CASE lv_class_name.
    WHEN 'ZCL_XTT_EXCEL_XLSX'. " OR 'ZCL_XTT_EXCEL_XML'.
      lv_path_in_arc = 'xl/worksheets/sheet1.xml'.          "#EC NOTEXT

    WHEN 'ZCL_XTT_WORD_DOCX'. " OR 'ZCL_XTT_WORD_XML'.
      lv_path_in_arc = 'word/document.xml'.                 "#EC NOTEXT
  ENDCASE.

  " Get content as a string from file
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip    = lo_zip
    iv_name   = lv_path_in_arc
   IMPORTING
    eo_xmldoc = lo_xml    " As REF TO if_ixml_document
*    ev_sdoc   = lv_xml " As STRING
   ).

  " do some action with lv_xml and write data back
  " Usually with REGEX
  " ....

  " For xlsx only
  IF lv_class_name = 'ZCL_XTT_EXCEL_XLSX'.
    lo_col ?= lo_xml->find_from_name( 'col' ).

    " Update columns
    WHILE lo_col IS BOUND.
      lv_mod = sy-index MOD 2.
      IF lv_mod = 0.
        lo_col->set_attribute(
          name = 'hidden'                                   "#EC NOTEXT
          value = '1' ).
      ENDIF.
      lo_col ?= lo_col->get_next( ).
    ENDWHILE.
  ENDIF.

  " Write data back
  zcl_eui_conv=>xml_to_zip(
   io_zip  = lo_zip
   iv_name = lv_path_in_arc
   io_xmldoc = lo_xml ). " Or use --> iv_sdoc = lv_xml

  " ZIP archive as xstring
  <lv_content> = lo_zip->save( ).
ENDMETHOD.

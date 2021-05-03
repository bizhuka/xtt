*&---------------------------------------------------------------------*
* Most of the tasks could be performed without macro.
* Macro cannot run in background and executed only in MS Office at Windows.
* This drawbacks and security issues narrow the scope of VBA  macro.
* That's why I highly do not recommend to use them at all.

* on other hand ON_PREPARE_RAW event always works
*&---------------------------------------------------------------------*

CLASS lcl_demo_070 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION,

      on_pbo_07 FOR EVENT pbo_event OF zif_eui_manager      "#EC CALLED
        IMPORTING
          sender
          io_container.

  PROTECTED SECTION.
    METHODS:
      _merge          REDEFINITION,
      _do_download    REDEFINITION,

      on_prepare_raw_07 FOR EVENT prepare_raw OF zcl_xtt
        IMPORTING "sender
          iv_path
          ir_content, " Type Ref To XSTRING

      check_ole_07
        IMPORTING
          io_ole_app TYPE ole2_object.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_070 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Macro call & on_prepare_raw'(070).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/macro/'.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
     BEGIN OF ts_root,
       message TYPE string,
     END OF ts_root.
    DATA ls_root TYPE ts_root.

    ls_root-message = 'No data at all'(nda).
    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD _merge.
    SET HANDLER on_prepare_raw_07 FOR io_xtt.
    " do not call io_xtt->merge( ), no data for the example

    " For Excel only
    io_xtt->add_raw_event( 'xl/worksheets/sheet1.xml' ). "#EC NOTEXT
  ENDMETHOD.

  " No macro. Always works, even in background mode
  " Use for XML parsing (Called in the end of GET_RAW)
  METHOD on_prepare_raw_07.
    " if 'IV_PATH' is initial then intire XLSX or DOCX archive
    CHECK iv_path = 'xl/worksheets/sheet1.xml'. "#EC NOTEXT

*    " For REGEX
*    DATA lv_string TYPE string.
*    lv_string = zcl_eui_conv=>xstring_to_string( ir_content->* ).

    " Use XML classes
    DATA lo_doc TYPE REF TO if_ixml_document.
    lo_doc = zcl_eui_conv=>str_to_xml( iv_xstr = ir_content->* ).

    " Find Excel columns
    DATA lo_col TYPE REF TO if_ixml_element.
    lo_col ?= lo_doc->find_from_name( 'col' ).

    " Update columns
    WHILE lo_col IS BOUND.
      DATA lv_mod TYPE i.
      lv_mod = sy-index MOD 2.

      " Hide every 2nd column
      IF lv_mod = 0.
        lo_col->set_attribute(
          name = 'hidden'                                   "#EC NOTEXT
          value = '1' ).
      ENDIF.
      lo_col ?= lo_col->get_next( ).
    ENDWHILE.

    " Write data back
    zcl_eui_conv=>xml_to_str( EXPORTING io_doc  = lo_doc
                              IMPORTING ev_xstr = ir_content->* ).
  ENDMETHOD.

  METHOD _do_download.
    DATA lv_ole_app TYPE ole2_object.
    io_xtt->download(        " All parameters are optional
     EXPORTING
      iv_open     = zcl_xtt=>mc_by-ole " Open with ole
     CHANGING
      cv_fullpath = p_path
      cv_ole_app  = lv_ole_app ).      " Get ole2_object back

    " Call macro. Works good regardless VBA politics
    check_ole_07( io_ole_app = lv_ole_app ).
  ENDMETHOD.

  " Macro always will execute for download method (regardless VBA security level)
  " Show method depends on security options, usually doesn't work (that's why do not call macro in SHOW )
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

  " Ok for 'xtt->DOWNLOAD( )', but not ok for 'xtt->SHOW( )'
  METHOD check_ole_07.
    DATA lv_charts TYPE ole2_object.

    " Better to use XML than macro
    CHECK io_ole_app IS NOT INITIAL.

    " @see on_prepare_raw_07
    CALL METHOD OF
        io_ole_app
        'Run'

      EXPORTING
        #1         = 'MAIN.start'
        #2         = 'From SAP'.                            "#EC NOTEXT

    " OR Call OLE like that
    SET PROPERTY OF io_ole_app 'StatusBar' = 'OLE Call'.    "#EC NOTEXT

    GET PROPERTY OF io_ole_app 'Charts' = lv_charts.
    CALL METHOD OF
        lv_charts
        'Add'.
  ENDMETHOD.

  METHOD get_templates.
    " xls(M) & doc(M)
    APPEND `ZXXT_DEMO_070-XLSM` TO rt_templates.
    APPEND `ZXXT_DEMO_070-DOCM` TO rt_templates.
  ENDMETHOD.
ENDCLASS.

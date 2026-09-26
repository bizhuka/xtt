*"* use this source file for your ABAP unit test classes

CLASS lcl_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      _download
        IMPORTING
          io_file     TYPE REF TO zif_xtt_file
          iv_messages TYPE string,

      smw0_no_file FOR TESTING,
      log_sheet_rows FOR TESTING,
      oaor_no_file FOR TESTING.
ENDCLASS.
CLASS zcl_xtt DEFINITION LOCAL FRIENDS lcl_test.

**********************************************************************
**********************************************************************
CLASS lcl_test IMPLEMENTATION.
  METHOD log_sheet_rows.
    DATA lo_file TYPE REF TO zif_xtt_file.
    DATA cut TYPE REF TO zcl_xtt.
    DATA lo_zip TYPE REF TO cl_abap_zip.
    DATA lv_raw TYPE xstring.
    DATA lv_xml TYPE string.
    DATA lv_count TYPE i.
    DATA lv_ref TYPE string.
    CREATE OBJECT lo_file TYPE zcl_xtt_file_smw0
      EXPORTING iv_objid = 'ZXXT_DEMO_140-XLSX'.
    CREATE OBJECT cut TYPE zcl_xtt_excel_xlsx EXPORTING io_file = lo_file.
    cut->mv_is_prod = abap_false.
    cut->add_log_message( iv_text = 'First log entry' iv_msgty = 'W' ).
    cut->add_log_message( iv_text = 'Second log entry' iv_msgty = 'E' ).
    cut->add_log_message( iv_text = 'Third log entry' iv_msgty = 'E' ).
    lv_raw = cut->get_raw( ).
    CREATE OBJECT lo_zip.
    lo_zip->load( lv_raw ).
    lo_zip->get( EXPORTING name = 'xl/worksheets/sheet999.xml' IMPORTING content = lv_raw ).
    lv_xml = zcl_eui_conv=>xstring_to_string( lv_raw ).
    FIND ALL OCCURRENCES OF '<row ' IN lv_xml MATCH COUNT lv_count.
    zcl_eui_conv=>assert_equals( exp = 4 act = lv_count ).
    DO 4 TIMES.
      lv_ref = |r="A{ sy-index }"|.
      FIND ALL OCCURRENCES OF lv_ref IN lv_xml MATCH COUNT lv_count.
      zcl_eui_conv=>assert_equals( exp = 1 act = lv_count ).
      lv_ref = |r="D{ sy-index }"|.
      FIND ALL OCCURRENCES OF lv_ref IN lv_xml MATCH COUNT lv_count.
      zcl_eui_conv=>assert_equals( exp = 1 act = lv_count ).
    ENDDO.
    FIND ALL OCCURRENCES OF 'ref="A1:D4"' IN lv_xml MATCH COUNT lv_count.
    zcl_eui_conv=>assert_equals( exp = 1 act = lv_count ).
  ENDMETHOD.

  METHOD _download.
    DATA cut TYPE REF TO zcl_xtt.

    CREATE OBJECT cut TYPE zcl_xtt_excel_xml
      EXPORTING
        io_file = io_file.

    cut->download( iv_open = abap_false ).

    zcl_xtt_util=>check_log_message( io_logger   = cut->_logger
                                     iv_messages = iv_messages ).
  ENDMETHOD.

  METHOD smw0_no_file.
    DATA lo_file TYPE REF TO zif_xtt_file.
    CREATE OBJECT lo_file TYPE zcl_xtt_file_smw0
      EXPORTING
        iv_objid = `no such file.xml`.

    _download( io_file     = lo_file
               iv_messages = 'ZSY_XTT-007' ). ";ZSY_XTT-008
  ENDMETHOD.

  METHOD oaor_no_file.
    DATA lo_file TYPE REF TO zif_xtt_file.
    CREATE OBJECT lo_file TYPE zcl_xtt_file_oaor
      EXPORTING
        iv_classname  = `~!@#$`
        iv_object_key = `~!@#$`
        iv_filename   = `no such file.xml`.

    _download( io_file     = lo_file
               iv_messages = 'ZSY_XTT-007' ). ";ZSY_XTT-008
  ENDMETHOD.
ENDCLASS.

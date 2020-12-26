*"* use this source file for your ABAP unit test classes

CLASS lcl_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      download
        IMPORTING
          io_file     TYPE REF TO zif_xtt_file
          iv_messages TYPE string,

      smw0_no_file FOR TESTING,
      oaor_no_file FOR TESTING.
ENDCLASS.
CLASS zcl_xtt DEFINITION LOCAL FRIENDS lcl_test.

**********************************************************************
**********************************************************************
CLASS lcl_test IMPLEMENTATION.
  METHOD download.
    DATA cut TYPE REF TO zcl_xtt.

    CREATE OBJECT cut TYPE zcl_xtt_excel_xml
      EXPORTING
        io_file = io_file.

    cut->download( ).

    zcl_xtt_util=>check_log_message( io_logger   = cut->_logger
                                     iv_messages = iv_messages ).
  ENDMETHOD.

  METHOD smw0_no_file.
    DATA lo_file TYPE REF TO zif_xtt_file.
    CREATE OBJECT lo_file TYPE zcl_xtt_file_smw0
      EXPORTING
        iv_objid = `no such file.xml`.

    download( io_file     = lo_file
              iv_messages = 'ZSY_XTT-007' ).
  ENDMETHOD.

  METHOD oaor_no_file.
    DATA lo_file TYPE REF TO zif_xtt_file.
    CREATE OBJECT lo_file TYPE zcl_xtt_file_oaor
      EXPORTING
        iv_classname  = `~!@#$`
        iv_object_key = `~!@#$`
        iv_filename   = `no such file.xml`.

    download( io_file     = lo_file
              iv_messages = 'ZSY_XTT-007' ).
  ENDMETHOD.
ENDCLASS.

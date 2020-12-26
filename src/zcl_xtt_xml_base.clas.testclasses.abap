*"* use this source file for your ABAP unit test classes

CLASS lcl_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .          "#AU Duration Short
  PUBLIC SECTION.
    DATA:
      tab TYPE STANDARD TABLE OF t000.
    METHODS:
      body_tag
        IMPORTING
          iv_class    TYPE string
          iv_template TYPE string
          iv_messages TYPE string,

      excel_xml FOR TESTING,
      html      FOR TESTING.
ENDCLASS.
CLASS zcl_xtt_xml_base DEFINITION LOCAL FRIENDS lcl_test.

**********************************************************************
**********************************************************************
CLASS lcl_test IMPLEMENTATION.
  METHOD body_tag.
    DATA lo_file TYPE REF TO zif_xtt_file.
    DATA cut TYPE REF TO zcl_xtt_xml_base.

    CREATE OBJECT lo_file TYPE zcl_xtt_file_raw
      EXPORTING
        iv_name   = 'Ok.xml'
        iv_string = iv_template.
    CREATE OBJECT cut TYPE (iv_class)
      EXPORTING
        io_file = lo_file.

    cut->merge( is_block      = me
                iv_block_name = 'R' ).

    zcl_xtt_util=>check_log_message( io_logger   = cut->_logger
                                     iv_messages = iv_messages ).
  ENDMETHOD.

  METHOD excel_xml.
    body_tag( iv_class    = 'ZCL_XTT_EXCEL_XML'
              iv_template = '<Worksheet>Ok <Row>{R-TAB}</Row><Worksheet>'
              iv_messages = '' ).

    body_tag( iv_class    = 'ZCL_XTT_EXCEL_XML'
              iv_template = '<Worksheet>Ok {R-TAB}<Worksheet>'
              iv_messages = 'ZSY_XTT-012' ).

    body_tag( iv_class    = 'ZCL_XTT_EXCEL_XML'
              iv_template = '<Worksh Not Ok rksheet>'
              iv_messages = 'ZSY_XTT-008;ZSY_XTT-010;ZSY_XTT-012' ).

    body_tag( iv_class    = 'ZCL_XTT_EXCEL_XML'
              iv_template = '<Worksheet>Ok ?'
              iv_messages = 'ZSY_XTT-009;ZSY_XTT-010;ZSY_XTT-012' ).
  ENDMETHOD.

  METHOD html.
    body_tag( iv_class    = 'ZCL_XTT_HTML'
              iv_template = '<body>Ok <tr> {R-TAB} </tr> <body>'
              iv_messages = '' ).

    body_tag( iv_class    = 'ZCL_XTT_HTML'
              iv_template = '<Worksh Not Ok rksheet>'
              iv_messages = 'ZSY_XTT-010;ZSY_XTT-012' ). " Should be no ZSY_XTT-008

    body_tag( iv_class    = 'ZCL_XTT_HTML'
              iv_template = '<body>Ok ?'
              iv_messages = 'ZSY_XTT-009;ZSY_XTT-010;ZSY_XTT-012' ).
  ENDMETHOD.
ENDCLASS.

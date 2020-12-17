*"* use this source file for your ABAP unit test classes

CLASS lcl_test  DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      generate    FOR TESTING,
      _702_cond   FOR TESTING,
      _702_concat FOR TESTING.
ENDCLASS.

CLASS zcl_xtt_cond DEFINITION LOCAL FRIENDS lcl_test.

**********************************************************************
**********************************************************************
CLASS lcl_test IMPLEMENTATION.
  METHOD generate.
*    DATA cut TYPE REF TO zcl_xtt_cond.
*    zcl_xtt_cond=>get_instance( EXPORTING iv_id       = 'R'
*                                IMPORTING eo_instance = cut ).
*    TYPES:
*      BEGIN OF ts_nested,
*        i TYPE i,
*        p TYPE bf_rbetr,
*        n TYPE n LENGTH 16,
*      END OF ts_nested,
*
*      BEGIN OF ts_root,
*        " Simple data
*        group  TYPE string,
*        date   TYPE d,
*        time   TYPE t,
*        sum1   TYPE bf_rbetr,
*        " Nested stucrure IN SE11
*        sy     TYPE syst,
*        nested TYPE ts_nested,
*        " 7.40 tab0   TYPE STANDARD TABLE OF ts_nested WITH EMPTY KEY,
*        tab1   TYPE STANDARD TABLE OF ts_nested WITH DEFAULT KEY,
*        tab2   TYPE SORTED   TABLE OF ts_nested WITH NON-UNIQUE KEY i n,
*        tab3   TYPE HASHED   TABLE OF ts_nested WITH UNIQUE KEY i n,
*      END OF ts_root.
*    DATA ls_root TYPE ts_root.
*
*    cut->get_type( EXPORTING is_data = ls_root
*                   IMPORTING ev_type = cut->mv_root_type ).
*
*    cut->_make_cond_forms( ). " zcx_eui_no_check
  ENDMETHOD.

  METHOD _702_cond.
*    DATA cut TYPE REF TO zcl_xtt_cond.
*    zcl_xtt_cond=>get_instance( EXPORTING iv_id       = 'R'
*                                IMPORTING eo_instance = cut ).
*
*    DATA lv_code TYPE STRING.
*    lv_code = cut->_702_cond(        `WHEN sy-datum(4) < '2020' THEN 28284 WHEN sy-datum(4) = '2020' THEN 42500 WHEN sy-datum(4) > '2020' THEN 42500 * '1.1' ELSE 0` ).
*    zcl_eui_conv=>assert_equals( exp = `IF sy-datum(4) < '2020' . result =  28284 .ELSEIF sy-datum(4) = '2020' . result =  42500 .ELSEIF sy-datum(4) > '2020' . result =  42500 * '1.1' . ELSE. result =  0.ENDIF.`
*                                 act = lv_code ).
  ENDMETHOD.

  METHOD _702_concat.
*    DATA cut TYPE REF TO zcl_xtt_cond.
*    zcl_xtt_cond=>get_instance( EXPORTING iv_id       = 'R'
*                                IMPORTING eo_instance = cut ).
*
*    DATA lv_code TYPE STRING.
*    lv_code = cut->_702_concat( `'Test' && 'Text'` ).
*
*    zcl_eui_conv=>assert_equals( act = lv_code
*                                 exp = `CONCATENATE 'Test'  'Text' INTO result.` ).
  ENDMETHOD.
ENDCLASS.

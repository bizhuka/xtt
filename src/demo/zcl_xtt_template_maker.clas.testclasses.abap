*"* use this source file for your ABAP unit test classes

CLASS lcl_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      get_structure      FOR TESTING,
      get_one_line_table FOR TESTING.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD get_structure.
    TYPES: BEGIN OF ts_src,
             d TYPE d,
             t TYPE t,
             i TYPE i,
           END OF ts_src,

           BEGIN OF ts_dest,
             d TYPE string,
             t TYPE string,
             i TYPE string,
           END OF ts_dest.

    DATA ls_src  TYPE ts_src.
    DATA lr_dest TYPE REF TO data.
    lr_dest = zcl_xtt_template_maker=>get_structure( is_root = ls_src ).

    FIELD-SYMBOLS <ls_dest> TYPE ts_dest.
    ASSIGN lr_dest->* TO <ls_dest>  CASTING.

    zcl_eui_conv=>assert_equals(  act   = <ls_dest>-d
                                  exp   = '{R-D}'
                                  msg   = 'Date anchor is wrong' ).
    zcl_eui_conv=>assert_equals(  act   = <ls_dest>-t
                                  exp   = '{R-T}'
                                  msg   = 'Time anchor is wrong' ).
    zcl_eui_conv=>assert_equals(  act   = <ls_dest>-i
                                  exp   = '{R-I}'
                                  msg   = 'INTEGER anchor is wrong' ).

  ENDMETHOD.

  METHOD get_one_line_table.
    TYPES: BEGIN OF ts_src,
             d TYPE d,
             t TYPE t,
             i TYPE i,
           END OF ts_src,

           BEGIN OF ts_dest,
             d TYPE string,
             t TYPE string,
             i TYPE string,
           END OF ts_dest,
           tt_dest TYPE STANDARD TABLE OF ts_dest WITH DEFAULT KEY.

    DATA lt_src  TYPE STANDARD TABLE OF ts_src.
    DATA lr_dest TYPE REF TO data.
    lr_dest = zcl_xtt_template_maker=>get_one_line_table( it_table = lt_src ).

    FIELD-SYMBOLS <lt_dest> TYPE tt_dest.
    FIELD-SYMBOLS <ls_dest> TYPE ts_dest.
    ASSIGN lr_dest->* TO <lt_dest>  CASTING.

    READ TABLE <lt_dest> ASSIGNING <ls_dest> INDEX 1.
    zcl_eui_conv=>assert_equals(  act   = <ls_dest>-d
                                  exp   = '{R-T-D}'
                                  msg   = 'Date anchor is wrong' ).
    zcl_eui_conv=>assert_equals(  act   = <ls_dest>-t
                                  exp   = '{R-T-T}'
                                  msg   = 'Time anchor is wrong' ).
    zcl_eui_conv=>assert_equals(  act   = <ls_dest>-i
                                  exp   = '{R-T-I}'
                                  msg   = 'INTEGER anchor is wrong' ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_100 DEFINITION INHERITING FROM zcl_xtt_demo_100.
  PUBLIC SECTION.


  PROTECTED SECTION.


    METHODS:
      _get_root REDEFINITION.
ENDCLASS.

CLASS lcl_demo_100 IMPLEMENTATION.
  METHOD _get_root.
    mv_img_size = img_size.
    rs_root = super->_get_root( iv_raw ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

CLASS lcl_main DEFINITION
  INHERITING FROM zcl_xtt.

  PUBLIC SECTION.
    CLASS-DATA:
      sender     TYPE REF TO zcl_xtt,

      mo_cont    TYPE REF TO cl_gui_custom_container,

      " For OLE
      mv_app_obj TYPE ole2_object.

    CLASS-METHODS:
      pbo_0100,

      pai_0100.

    METHODS:
      merge REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      get_raw REDEFINITION.
ENDCLASS.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  lcl_main=>pbo_0100( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  lcl_main=>pai_0100( ).
ENDMODULE.                    "pai_0100 INPUT

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

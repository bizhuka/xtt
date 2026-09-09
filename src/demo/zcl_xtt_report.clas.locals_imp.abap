**********************************************************************
**********************************************************************
CLASS lcl_helper DEFINITION DEFERRED.
CLASS zcl_xtt_report DEFINITION LOCAL FRIENDS lcl_helper.

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_all_demos IMPORTING io_report      TYPE REF TO zcl_xtt_report
                    RETURNING VALUE(rt_demo) TYPE zcl_xtt_report=>tt_demo.
ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.
  METHOD get_all_demos.
    DATA:
      lt_class_names TYPE STANDARD TABLE OF string,
      ls_demo        TYPE zcl_xtt_report=>ts_demo.
    FIELD-SYMBOLS:
      <lv_class_name> TYPE string.

    IF sy-saprl = 'OPEN'.
      WRITE '@KERNEL for (const className of Object.keys(globalThis.abap.Classes)) {'.
      WRITE '@KERNEL   if (className.startsWith("ZCL_XTT_DEMO_")) {'.
      WRITE '@KERNEL     let abapName = new abap.types.String();'.
      WRITE '@KERNEL     abapName.set(className);'.
      WRITE '@KERNEL     lt_class_names.append(abapName);'.
      WRITE '@KERNEL   }'.
      WRITE '@KERNEL }'.
    ELSE.
      DATA:
        lt_include   TYPE STANDARD TABLE OF char30.
      FIELD-SYMBOLS
        <lv_include> TYPE char30.

      SELECT name INTO TABLE lt_include
      FROM trdirt
      WHERE name LIKE 'Z_XTT_DEMO_N%'
        AND sprsl = 'E'.

      LOOP AT lt_include ASSIGNING <lv_include>.
        APPEND INITIAL LINE TO lt_class_names ASSIGNING <lv_class_name>.

        CONCATENATE `\PROGRAM=Z_XTT_DEMO\CLASS=LCL_DEMO_` <lv_include>+12(3) INTO <lv_class_name>.
      ENDLOOP.
    ENDIF.

    " Create all instances
    LOOP AT lt_class_names ASSIGNING <lv_class_name>.
      DATA lv_off TYPE i.
      lv_off = strlen( <lv_class_name> ) - 3.
      ls_demo-ind = <lv_class_name>+lv_off(3).

      CREATE OBJECT ls_demo-inst TYPE (<lv_class_name>).
      ls_demo-inst->set_report( io_report ).

      INSERT ls_demo INTO TABLE rt_demo[].
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

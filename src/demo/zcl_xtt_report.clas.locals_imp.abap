**********************************************************************
**********************************************************************
CLASS lcl_helper DEFINITION DEFERRED.
CLASS zcl_xtt_report DEFINITION LOCAL FRIENDS lcl_helper.

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_all_demos IMPORTING io_report      TYPE REF TO zcl_xtt_report
                    RETURNING VALUE(rt_demo) TYPE zcl_xtt_report=>tt_demo,

      pretty_print IMPORTING iv_xml         TYPE xstring
                             iv_indent_size TYPE i DEFAULT 2
                   RETURNING VALUE(rv_xml)  TYPE xstring.
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

  METHOD pretty_print.
    rv_xml = iv_xml.

    IF sy-saprl = 'OPEN'.
      DATA lv_xml TYPE string.
      lv_xml = zcl_eui_conv=>xstring_to_string( iv_xml ).

      WRITE '@KERNEL let xml = lv_xml.get();'.
      WRITE '@KERNEL let indentSize = iv_indent_size.get();'.
      WRITE '@KERNEL let indent = " ".repeat(indentSize);'.
      WRITE '@KERNEL let depth = 0;'.

      WRITE '@KERNEL let tokens = xml'.
      WRITE '@KERNEL   .replace(/>\s+</g, "><")'.
      WRITE '@KERNEL   .trim()'.
      WRITE '@KERNEL   .match(/(<\[CDATA\[.*?\]\]>|<!--.*?-->|<[^>]+>|[^<]+)/gs) || [];'.

      WRITE '@KERNEL let formatted = tokens.map(token => {'.
      WRITE '@KERNEL   if (!token.trim()) return "";'.
      WRITE '@KERNEL   if (token.startsWith("<!--") || token.startsWith("<![CDATA[")) {'.
      WRITE '@KERNEL     return indent.repeat(depth) + token.trim();'.
      WRITE '@KERNEL   }'.
      WRITE '@KERNEL   if (token.match(/^<[^>]+?\/>$/) || token.startsWith("<?")) {'.
      WRITE '@KERNEL     return indent.repeat(depth) + token;'.
      WRITE '@KERNEL   }'.
      WRITE '@KERNEL   if (token.startsWith("</")) {'.
      WRITE '@KERNEL     depth = Math.max(0, depth - 1);'.
      WRITE '@KERNEL     return indent.repeat(depth) + token;'.
      WRITE '@KERNEL   }'.
      WRITE '@KERNEL   if (token.startsWith("<")) {'.
      WRITE '@KERNEL     let line = indent.repeat(depth) + token;'.
      WRITE '@KERNEL     depth++;'.
      WRITE '@KERNEL     return line;'.
      WRITE '@KERNEL   }'.
      WRITE '@KERNEL   return indent.repeat(depth) + token.trim();'.
      WRITE '@KERNEL }).filter(line => line.length > 0).join("\n");'.
      WRITE '@KERNEL lv_xml.set(formatted);'.

      rv_xml = zcl_eui_conv=>string_to_xstring( lv_xml ).
      RETURN.
    ENDIF.

    DATA lo_dom TYPE REF TO if_ixml_document.
    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml      = iv_xml
      IMPORTING
        document = lo_dom
      EXCEPTIONS
        OTHERS   = 1.
    CHECK sy-subrc = 0.

    DATA lv_pretty_xml TYPE xstring.
    CALL FUNCTION 'SDIXML_DOM_TO_XML'
      EXPORTING
        document       = lo_dom
        pretty_print   = 'X'
      IMPORTING
        xml_as_string  = lv_pretty_xml
      EXCEPTIONS
        OTHERS         = 1.
    CHECK sy-subrc = 0.

    rv_xml = lv_pretty_xml.
  ENDMETHOD.
ENDCLASS.

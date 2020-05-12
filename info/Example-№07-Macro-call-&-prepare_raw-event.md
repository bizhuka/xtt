The simplest way I can describe macro calls is `Do not use them at all!`. There are lot of issues, restrictions with VBA macro and in general the approach is error prone. That's why in my humble opinion you should avoid it. Instead of that there is special event `PREPARE_RAW` which can give you the abilty to change XML content directly. But the posibility to call a macro still exist in the library.

### VBA macro (do not recommended)
All macro calls work only in SAP gui, but not in web-dynpro nor in background task.

* DOWNLOAD
```abap
      ro_xtt->download(        " All parameters are optional
       EXPORTING
        iv_open     = zcl_xtt=>mc_by_ole " Open with ole
       CHANGING
        cv_ole_app  = lv_ole_app ).      " Get ole2_object back
```

after that you can call a macro by name
```abap
  CALL METHOD OF lv_ole_app 'Run'
    EXPORTING
      #1 = 'MAIN.start'
      #2 = 'From SAP'.
```
This way of calling a macro works regardless of VBA security level. 

* SHOW\
If you decided to call a macro within SAP gui you should use special pbo event handler.

```abap
      on_pbo_07 FOR EVENT pbo OF zcl_xtt
        IMPORTING
            sender
            io_app_obj.
```
here you can call the same macro
```abap
METHOD on_pbo_07.
  CALL METHOD OF io_app_obj 'Run'
    EXPORTING
      #1 = 'MAIN.start'
      #2 = 'From SAP'.

  " OR Call OLE like that
  SET PROPERTY OF io_app_obj 'StatusBar' = 'OLE Call'.

  GET PROPERTY OF io_app_obj 'Charts' = lv_charts.
  CALL METHOD OF lv_charts 'Add'.
ENDMETHOD.
```
The bad news this approach depends on VBA security level. And in general do not work.
Because of that more stable method is to handle with raw XML. It's not so difficult as it sounds.

## PREPARE_RAW event (recommended)
For 3 ms office formats (Xml Spreadsheet 2003, Word XML Document, Word XML 2003 Document) and Adobe xdp format everything is straightforward.

```abap
      on_prepare_raw_07 FOR EVENT prepare_raw OF zcl_xtt
        IMPORTING
            sender
            ir_content. " Type Ref To XSTRING
```
ir_content contains reference to document itself. And you can change it by REPLACE or REGEX statements.

For new MS Office Open XML formats
1. Document (*.docx), Macro-Enabled Document (*.docm)
1. Workbook (*.xlsx), Macro-Enabled Workbook (*.xlsm)
the method is a little bit difficult.

First of all since all previously mentioned file formats are actually zip archives, you have to extract desired file.
```abap
  CASE lv_class_name.
    WHEN 'ZCL_XTT_EXCEL_XLSX'. " OR 'ZCL_XTT_EXCEL_XML'.
      lv_path_in_arc = 'xl/worksheets/sheet1.xml'.

    WHEN 'ZCL_XTT_WORD_DOCX'. " OR 'ZCL_XTT_WORD_XML'.
      lv_path_in_arc = 'word/document.xml'.
  ENDCASE.

  " Get content as a string from file
  zcl_xtt_util=>xml_from_zip(
   EXPORTING
    io_zip    = lo_zip
    iv_name   = lv_path_in_arc
   IMPORTING
    eo_xmldoc = lo_xml    " As REF TO if_ixml_document
    ev_sdoc   = lv_xml ). " As STRING
```

After all manipulations with STRING content or IF_IXML_DOCUMENT(more convenient way) just write data back to the zip
```abap
  " Write data back
  zcl_xtt_util=>xml_to_zip(
   io_zip  = lo_zip
   iv_name = lv_path_in_arc
   iv_sdoc = lv_xml ). " Or use --> io_xmldoc = lo_xml

  " ZIP archive as xstring
  <lv_content> = lo_zip->save( ).
```
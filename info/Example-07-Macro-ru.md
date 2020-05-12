Самый простой способ которым я могу охарактеризовать вызовы макроса из кода - «Никогда так не делайте!». Есть много сложностей, ограничений и в целом данный подход склонен к ошибкам. Вот почему, по моему скромному мнению, вы должны избегать их. Вместо этого, в данной разработке, существует специальное событие `PREPARE_RAW`, которое дает возможность напрямую изменять содержимое XML. Но возможность вызова макроса все же существует.

### Вызов VBA макроса (не рекомендуется)
Все вызовы макросов работают только в стандартном SAP gui, в web-dynpro или при фоновом выполнении они не работают.

* DOWNLOAD
```abap
      ro_xtt->download(        " All parameters are optional
       EXPORTING
        iv_open     = zcl_xtt=>mc_by_ole " Open with ole
       CHANGING
        cv_ole_app  = lv_ole_app ).      " Get ole2_object back
```

после этого вы можете вызвать макрос по имени
```abap
  CALL METHOD OF lv_ole_app 'Run'
    EXPORTING
      #1 = 'MAIN.start'
      #2 = 'From SAP'.
```
Этот способ вызова макроса работает независимо от уровня безопасности VBA.

* SHOW<br/>
Если вы решили вызвать макрос напрямую в самом окне SAP gui, вы должны использовать специальный обработчик события.

```abap
      on_pbo_07 FOR EVENT pbo OF zcl_xtt
        IMPORTING
            sender
            io_app_obj.
```
здесь вы можете вызвать тот же макрос
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
Плохая новость - такой вызов (используя метод SHOW) зависит от уровня безопасности VBA (и в целом не работает).
Из-за этого более стабильным решением является обработка самого XML документа. Это не так сложно, как кажется.

## Событие PREPARE_RAW (рекомендуемое)
Для 3-х офисных форматов (Xml Spreadsheet 2003, Word XML Document, Word XML 2003 Document) и формата Adobe xdp все просто.

```abap
      on_prepare_raw_07 FOR EVENT prepare_raw OF zcl_xtt
        IMPORTING
            sender
            ir_content. " Type Ref To XSTRING
```
ir_content содержит ссылку на сам документ. И вы можете изменить его с помощью операторов REPLACE или REGEX.

Для новых форматов MS Office Open XML
1. Документ Word (.docx), Документ Word с поддержкой макросов (.docm)
1. Книга Excel (.xlsx), Книга Excel с поддержкой макросов (.xlsm)<br/>
все немного сложнее.

Прежде всего, поскольку все ранее упомянутые форматы файлов представляют собой zip-архивы, вам нужно сначала извлечь желаемый файл.
```abap
  CASE lv_class_name.
    WHEN 'ZCL_XTT_EXCEL_XLSX'. " OR 'ZCL_XTT_EXCEL_XML'.
      lv_path_in_arc = 'xl/worksheets/sheet1.xml'.

    WHEN 'ZCL_XTT_WORD_DOCX'. " OR 'ZCL_XTT_WORD_XML'.
      lv_path_in_arc = 'word/document.xml'.
  ENDCASE.

  " Получить контент в виде строки
  zcl_xtt_util=>xml_from_zip(
   EXPORTING
    io_zip    = lo_zip
    iv_name   = lv_path_in_arc
   IMPORTING
    eo_xmldoc = lo_xml    " As REF TO if_ixml_document
    ev_sdoc   = lv_xml ). " As STRING
```

После всех манипуляций с содержимым STRING или IF_IXML_DOCUMENT (более удобный способ) просто запишите данные обратно в zip
```abap
  " Write data back
  zcl_xtt_util=>xml_to_zip(
   io_zip  = lo_zip
   iv_name = lv_path_in_arc
   iv_sdoc = lv_xml ). " Or use --> io_xmldoc = lo_xml

  " ZIP archive as xstring
  <lv_content> = lo_zip->save( ).
```
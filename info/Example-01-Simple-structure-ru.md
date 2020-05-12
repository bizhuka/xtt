Самый простой способ отправить простые данные в отчет - передать их в структуре
```abap
    " Document structure
    BEGIN OF ts_root,
      title  TYPE char15,
      text   TYPE string,
      int    TYPE i,
      bottom TYPE string, " Any field could be REF TO, STRUCTURE or TABLE
    END OF ts_root.
```

Данные могут выглядеть так:
```abap
    ls_root-title   = 'Document title'.
    ls_root-text    = 'Just string'.
    ls_root-int     = 3.
    ls_root-bottom  = 'bottom'.
```

Шаблон в Excel, Word или pdf может выглядеть так:

***
Basic example

Title: **_{R-TITLE}_**

Just put markers where you want

Just string    **{R-TEXT}**

Integer        _{R-INT}_

Bottom: ~~{R-BOTTOM}~~

***

Код XTT для инициализации будет одинаковым:
```abap
  " Info about template (Use ZCL_XTT_FILE_OAOR for tr. OAOR)
  CREATE OBJECT:
   lo_file TYPE ZCL_XTT_FILE_SMW0 EXPORTING
     iv_objid = iv_template, " SMW0 binary file

  " The main class (Use ZCL_XTT_WORD_DOCX, ZCL_XTT_PDF for word and pdf respectively)
   lo_xtt TYPE ZCL_XTT_EXCEL_XLSX EXPORTING
    io_file = lo_file.

...

  " R is a marker in the IV_TEMPLATE
  lo_xtt->merge( is_block = ls_root iv_block_name = 'R' ).
```

После вызова методов lo_xtt **DOWNLOAD** или **SHOW** конечный результат будет таким:

***
Basic example

Title: **_Document title_**

Just put markers where you want

Just string    **Just string**

Integer        _3_

Bottom: ~~bottom~~
***

Все форматирование текста внутри **{}** остается неизменным.

**В word зачастую, блок который выглядит одинаково может состоять из нескольких с одинаковым форматированием.**

~~Для исключение подобного случая нужно скопировать блок с {} в notepad, скопировать в нем и вставить обратно~~

Теперь если в Word будет подобное фоматирование (Классы **ZCL_XTT_WORD_DOCX** и **ZCL_XTT_WORD_XML**)

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/01_word_part_text.png)

форматирование будет аналогично 1-й части текста

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/01_word_part_text_f.png)
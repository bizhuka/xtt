При обработке таблицы необходимо определить границы «выходной области».
Вот почему вы должны очень тщательно определять вложенные блоки (таблицу таблиц итп).

```abap

    " Structure of document
    BEGIN OF ts_root,
      title  TYPE string,
      bottom TYPE string,
      t      TYPE tt_rand_data, " Table within another table (lt_root)
    END OF ts_root.
  DATA:
    lt_root TYPE STANDARD TABLE OF ts_root.

...
ro_xtt->merge( is_block = lt_root iv_block_name = 'R' ).
```

Если вы передадите саму таблицу в качестве параметра метода `merge`, границы для классов
`ZCL_XTT_WORD_DOCX`, `ZCL_XTT_EXCEL_XML` и `ZCL_XTT_PDF` будет весь документ.
Для `ZCL_XTT_EXCEL_XML` границы будут представлять собой 1 лист.

---

Для MS Word границами области {R} будет сам документ с завершающим «Разрывом страницы»<br/>
А границами {R-T} будет 1 строка без заголовка

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_word_templ.png)

---

Что касается формата «XML-таблица 2003», то границу {R} будет представлять средний лист
![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_2003_templ.png)

И после репликации данных каждая строка ts_root будет отдельным листом
![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_2003_res.png)

Хочу подчеркнуть что **TITLE** не является ключевым словом. Это всего лишь поле структуры, не более

---

Для PDF вы должны назвать реплицируемую страницу как параметр IV_BLOCK_NAME метода MERGE.
Также если вам нужны разрывы страниц в документе, установите атрибут первого потомка, как здесь ... 

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_pdf_templ.png)
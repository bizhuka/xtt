### Xml template toolkit

XTT helps you to automate your reporting routine in SAP

- Get existing report from your customers
- Use preferable editor (MS Excel, Word or Adobe LiveCycle Designer) and replace some parts of it with markers inclosed in [curly brackets](info/mustache-en.md)
- Link all formulas, charts and pivot tables to this marksers
- In abap call **`merge()`** method to pass all data including nested strucures, tables and trees
- Use **`download( ) | send( ) | show( )`** method for appropreate action
- That's all!

---

###  Template
![image](https://user-images.githubusercontent.com/36256417/80579411-6b7c0600-8a23-11ea-8166-d48e63b7d085.png)

### Code

```abap
" Template storage class (tr OAOR -> zcl_xtt_file_oaor, external sources -> ZCL_XTT_FILE_RAW)
DATA(lo_file) = NEW zcl_xtt_file_smw0( 'Z_TEMPLATE_ID.XLSX' ).

" Excel (Use ZCL_XTT_WORD_DOCX, ZCL_XTT_PDF for word and pdf respectively)
DATA(lo_xtt) = NEW zcl_xtt_excel_xlsx( io_file = lo_file ).

" R is a marker in the IV_TEMPLATE
lo_xtt->merge( iv_block_name = 'R'
               is_block = VALUE ts_root(
                begda = pn-begda
                endda = pn-endda
                
                " for hierarchies use ZCL_XTT_REPLACE_BLOCK=>TREE_CREATE* methods
                t     = lt_alv    
               ) ).

" Download to sap_tmp. You could specify path. Show SaveAs dialogue
" Call SHOW( ) for inplace mode
lo_xtt->download( ).
```

---

| English| Русский |
|-------------|-------------|
|[Mustachioed template engine](info/mustache-en.md)| [Усатый шаблонизатор](info/mustache-ru.md)|
|[Installation guide](info/Installation-guide-en.md)| [Инструкция по установке](info/Installation-guide-ru.md)|
|[Main features](info/Main-features-en.md)| [Ключевые особенности](info/Main-features-ru.md)  |
| tr. Z_XTT_INDEX | тр. Z_XTT_INDEX |
| [Example №01 Simple structure](info/Example-01-Simple-structure-en.md)| [Пример №01 Вывод простой структуры](info/Example-01-Simple-structure-ru.md)|
| [Example №02 Basic tables](info/Example-02-Basic-tables-en.md)| [Пример №02 Вывод простой таблицы](info/Example-02-Basic-tables-ru.md)|
| [Example №03 Nested blocks](info/Example-03-Nested-blocks-en.md)| [Пример №03 Вложенные блоки](info/Example-03-Nested-blocks-ru.md)|
| [Example №04 Data types & Excel post processing](info/Example-04-Data-types-en.md)| [Пример №04 Типы данных и пост обработка Excel](info/Example-04-Data-types-ru.md)|
| [Example №05 Tree (group by fields)](info/Example-05-01-Tree-en.md)| [Пример №05 Деревья (группировка по полям)](info/Example-05-01-Tree-ru.md)|
| [Example №05_co Output level by condition](info/Example-05-02-Tree-en.md)|[Пример №05_co Вывод уровня по условию](info/Example-05-02-Tree-ru.md)|
| [Example №05_fm Aggregation functions](info/Example-05-03-Tree-en.md)|[Пример №05_fm Функции агрегации](info/Example-05-03-Tree-ru.md)|
| [Example №06 Tree (group by field relations)](info/Example-06-Tree-en.md)| [Пример №06 Деревья (на основе связи родитель дочерний)](info/Example-06-Tree-ru.md)|
| [Example №07 Macro call & prepare_raw event](info/Example-07-Macro-en.md)| [Пример №07 Вызов макроса и событие prepare_raw](info/Example-07-Macro-ru.md)|
| [Example №08 ;direction=column addition](info/Example-08-direction-en.md)| [Пример №08 Дополнение ;direction=column](info/Example-08-direction-ru.md)|
| [Example №09 Dynamic table (tree)](info/Example-09-Dynamic-table-en.md)| [Пример №09 Динамическая таблица (дерево)](info/Example-09-Dynamic-table-ru.md)|

---

### From 2020
You have to install [EUI](https://github.com/bizhuka/eui) library first 

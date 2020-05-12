### Xml template toolkit

XTT helps you to automate your reporting routine in SAP

- Get existing report from your customers
- Use preferable editor (MS Excel, Word or Adobe LiveCycle Designer) and replace some parts of it with markers inclosed in _**{curly brackets}**_
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
|[Installation guide](info/Installation-guide.md)| [Инструкция по установке](info/Инструкция-по-установке.md)|
|[Main features](info/Main-features.md)| [Ключевые особенности](info/Ключевые-особенности)  |
| tr. Z_XTT_INDEX | тр. Z_XTT_INDEX |
| [Example №01 Simple structure](info/Example-%E2%84%9601-Simple-structure.md)| [Пример №01 Вывод простой структуры](info/Пример-№01-Вывод-простой-структуры.md)|
| [Example №02 Basic tables](info/Example-%E2%84%9602-Basic-tables.md)| [Пример №02 Вывод простой таблицы](info/Пример-№02-Вывод-простой-таблицы.md)|
| [Example №03 Nested blocks](info/Example-%E2%84%9603-Nested-blocks.md)| [Пример №03 Вложенные блоки](info/Пример-№03-Вложенные-блоки.md)|
| [Example №04 Data types & Excel post processing](info/Example-%E2%84%9604-Data-types-&-Excel-post-processing.md)| [Пример №04 Типы данных и пост обработка Excel](info/Пример-№04-Типы-данных-и-пост-обработка-Excel.md)|
| [Example №05 Tree (group by fields)](info/Example-%E2%84%9605-Tree-(group-by-fields).md)| [Пример №05 Деревья (группировка по полям)](info/Пример-№05-Деревья-(группировка-по-полям).md)|
| [Example №05_co Output level by condition](info/Example-№05_co-Output-level-by-condition.md)|[Пример №05_co Вывод уровня по условию](info/Пример-№05_co-Вывод-уровня-по-условию.md)|
| [Example №05_fm Aggregation functions](info/Example-№05_fm-Aggregation-functions.md)|[Пример №05_fm Функции агрегации](info/Пример-№05_fm-Функции-агрегации.md)|
| [Example №06 Tree (group by field relations)](info/Example-%E2%84%9606-Tree-(group-by-field-relations).md)| [Пример №06 Деревья (на основе связи родитель дочерний)](info/Пример-№06-Деревья-(на-основе-связи-родитель-дочерний).md)|
| [Example №07 Macro call & prepare_raw event](info/Example-%E2%84%9607-Macro-call-&-prepare_raw-event.md)| [Пример №07 Вызов макроса и событие prepare_raw](info/Пример-№07-Вызов-макроса-и-событие-prepare_raw.md)|
| [Example №08 ;direction=column addition](info/Example-%E2%84%9608-;direction=column-addition.md)| [Пример №08 Дополнение ;direction=column](info/Пример-№08-Дополнение-;direction=column.md)|
| [Example №09 Dynamic table (tree)](info/Example-%E2%84%9609-Dynamic-table.md)| [Пример №09 Динамическая таблица (дерево)](info/Пример-№09-Динамическая-таблица.md)|

---

### From 2020
You have to install [EUI](https://github.com/bizhuka/eui) library first 

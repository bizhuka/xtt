### Xml template toolkit

XTT helps you to automate your reporting routine in SAP

- Get existing report from your customers
- Use preferable editor (MS Excel, Word or Adobe LiveCycle Designer) and replace some parts of it with markers inclosed in _**{curly brackets}**_
- Link all formulas, charts and pivot tables to this marksers
- In abap call **`merge()`** method to pass all data including nested strucures, tables and trees
- Use **`download()|send()|show()`** method for appropreate action
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

For more information please read [wiki](https://github.com/bizhuka/xtt/wiki)

| English| Русский |
|-------------|-------------|
|[Installation guide](https://github.com/bizhuka/aqo/wiki/Installation-guide)| [Инструкция по установке](https://github.com/bizhuka/aqo/wiki/Инструкция-по-установке) |
|[Main features](https://github.com/bizhuka/aqo/wiki/Main-features)| [Ключевые особенности](https://github.com/bizhuka/aqo/wiki/Ключевые-особенности)  |
| tr. Z_XTT_INDEX | тр. Z_XTT_INDEX |
| [Example №01 Simple structure](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9601-Simple-structure) | [Пример №01 Вывод простой структуры](https://github.com/bizhuka/aqo/wiki/Пример-№01-Вывод-простой-структуры) |
| [Example №02 Basic tables](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9602-Basic-tables) | [Пример №02 Вывод простой таблицы](https://github.com/bizhuka/aqo/wiki/Пример-№02-Вывод-простой-таблицы) |
| [Example №03 Nested blocks](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9603-Nested-blocks) | [Пример №03 Вложенные блоки](https://github.com/bizhuka/aqo/wiki/Пример-№03-Вложенные-блоки) |
| [Example №04 Data types & Excel post processing](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9604-Data-types-&-Excel-post-processing) | [Пример №04 Типы данных и пост обработка Excel](https://github.com/bizhuka/aqo/wiki/Пример-№04-Типы-данных-и-пост-обработка-Excel) |
| [Example №05 Tree (group by fields)](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9605-Tree-(group-by-fields)) | [Пример №05 Деревья (группировка по полям)](https://github.com/bizhuka/aqo/wiki/Пример-№05-Деревья-(группировка-по-полям)) |
| [Example №05_co Output level by condition](https://github.com/bizhuka/aqo/wiki/Example-№05_co-Output-level-by-condition) |[Пример №05_co Вывод уровня по условию](https://github.com/bizhuka/aqo/wiki/Пример-№05_co-Вывод-уровня-по-условию) |
| [Example №05_fm Aggregation functions](https://github.com/bizhuka/aqo/wiki/Example-№05_fm-Aggregation-functions) |[Пример №05_fm Функции агрегации](https://github.com/bizhuka/aqo/wiki/Пример-№05_fm-Функции-агрегации) |
| [Example №06 Tree (group by field relations)](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9606-Tree-(group-by-field-relations)) | [Пример №06 Деревья (на основе связи родитель дочерний)](https://github.com/bizhuka/aqo/wiki/Пример-№06-Деревья-(на-основе-связи-родитель-дочерний)) |
| [Example №07 Macro call & prepare_raw event](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9607-Macro-call-&-prepare_raw-event) | [Пример №07 Вызов макроса и событие prepare_raw](https://github.com/bizhuka/aqo/wiki/Пример-№07-Вызов-макроса-и-событие-prepare_raw) |
| [Example №08 ;direction=column addition](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9608-;direction=column-addition) | [Пример №08 Дополнение ;direction=column](https://github.com/bizhuka/aqo/wiki/Пример-№08-Дополнение-;direction=column) |
| [Example №09 Dynamic table (tree)](https://github.com/bizhuka/aqo/wiki/Example-%E2%84%9609-Dynamic-table) | [Пример №09 Динамическая таблица (дерево)](https://github.com/bizhuka/aqo/wiki/Пример-№09-Динамическая-таблица) |

---

### From 2020
You have to install [EUI](https://github.com/bizhuka/eui) library first 

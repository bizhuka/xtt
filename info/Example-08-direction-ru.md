Все таблицы и деревья в вашем шаблоне могут быть реплицированы как по строкам так и столбцам.<br/>
Данная возможность работает только для класса `ZCL_XTT_EXCEL_XLSX` и 2 типов файлов (xlsx и xlsm).<br/>
Для этого в любом месте вашего шаблона просто добавьте специальный маркер {;direction=column} для таблицы или дерева.

Код ABAP аналогичен [Пример №05](Пример-№05-Деревья-(группировка-по-полям))
```abap
    SET HANDLER on_prepare_tree_05 ACTIVATION abap_true.

    GET REFERENCE OF lt_rows INTO lr_table.
    ls_root-a = zcl_xtt_replace_block=>tree_create(
     it_table      = lr_table       " from 7.5 REF #(lt_rows)
     iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;

    SET HANDLER on_prepare_tree_05 ACTIVATION abap_false.
```
Основное отличие в самом шаблоне.

* Таблица

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_02.png)

* Дерево

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_01.png)

Результат будет выглядеть следующим образом:
* Таблица

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_03.png)

* Дерево

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_04.png)
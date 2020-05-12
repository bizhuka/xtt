Если у вас есть обычная таблица с простыми итогами, вы можете использовать внутренние таблицы abap.<br/>
Но если вам также нужно отображать промежуточные итоги и другие иерархические структуры, лучше использовать `деревья`.<br/>
Существует два метода создания деревьев. Сейчас речь пойдет о первом из них `TREE_CREATE`. <br/>
О `TREE_CREATE_RELAT` вы можете прочитать на [следующей странице](Example-05-02-Tree-ru.md).

### TREE_CREATE
```abap
    " Document structure
    BEGIN OF ts_root,
      title TYPE string,
      t     TYPE  REF TO data, " better to use general type than zcl_xtt_replace_block=>ts_tree
    END OF ts_root.

    ls_root-t = zcl_xtt_replace_block=>tree_create(
     it_table      = lr_table       " from 7.5 REF #(lt_rows)
     iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;
```
Самый простой способ описать данный метод - представить ALV grid с промежуточными итогами.
```abap
 iv_fields     = 'BUKRS'        " Подитоги по 1 полю
 iv_fields     = 'BUKRS;WERKS'  " Подитоги по 2 полям
```

для вычисления сумм (или средних значений) вы должны определить обработчик для обработки дерева и нижестоящих элементов
```abap
      on_prepare_tree_05 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
            ir_tree      " Type Ref To ZCL_XTT_REPLACE_BLOCK=>TS_TREE
            ir_data      " Type Ref To DATA
            ir_sub_data, " Type Ref To DATA
```
в первую очередь в обработчике вы должны преобразовать данные в исходные типы данных
```abap
  FIELD-SYMBOLS:
    <ls_data>     TYPE ts_tree_05,
    <ls_sub_data> TYPE ts_tree_05.

  " Cast to specefic data
  ASSIGN:
   ir_data->*        TO <ls_data>,
   ir_sub_data->*    TO <lt_sub_data>.
```
и в цикле вы можете рассчитать все, что вам нужно
```abap
  " And calc sums
  LOOP AT <lt_sub_data> ASSIGNING <ls_sub_data>.
    <ls_data>-sum1  = <ls_data>-sum1 + <ls_sub_data>-sum1.
    <ls_data>-sum2  = <ls_data>-sum2 + <ls_sub_data>-sum2.
  ENDLOOP.
```

***
В шаблоне, если у вас есть разные форматирование строк для каждого уровня, вы можете указать для них дополнительные параметры `;level=` и `;top=`.
Если нижестоящие элементы идут после родительского элемента, просто добавьте `;top=X` для данного уровня, иначе `;top=` или просто ничего не указывайте.

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/tree_01.png)
Также, если вы укажете уровень группировке в шаблоне, он будет реплицирован в окончательном отчете для соответствующих элементов.

Результат выглядит как в ALV GRID
![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/tree_02.png)

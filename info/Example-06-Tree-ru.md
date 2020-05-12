Возможно, деревья основанные на [подитогах](Пример-№05-Деревья-(группировка-по-полям)) более распространены, но в SAP есть некоторые другие иерархические данные, такие как СПП-элементы или организационные единицы HR.<br/>
Для такого рода данных, когда количество подуровней неизвестно заранее и существует только отношение «родительский - дочерний», вы можете возпользоваться методом «TREE_CREATE_RELAT».

### Иерархия папок
```abap
      BEGIN OF ts_tree_06,
        " Folders hierarchy
        dir     TYPE string,
        par_dir TYPE string,
        
        " Empty field. Filled in on_prepare_tree_06
        level   TYPE i,
      END OF ts_tree_06,
      tt_tree_06 TYPE STANDARD TABLE OF ts_tree_06 WITH DEFAULT KEY,

    " Document structure
    BEGIN OF ts_root,
      title TYPE string,
      t     TYPE REF TO data, " <-- Table of trees (better to use general REF TO)
    END OF ts_root.
```

После заполнения таблицы lt_folders (dir & par_dir) просто передаем имена полей методу `TREE_CREATE_RELAT`.
```abap
    GET REFERENCE OF lt_folders INTO lr_table.
    ls_root-t = zcl_xtt_replace_block=>tree_create_relat(
      it_table      = lr_table        " from 7.5 REF #(lt_folders)
      iv_node_key   = 'DIR'
      iv_relat_key  = 'PAR_DIR' ).
```

Все промежуточные итоги также заполняются обработчиком `prepare_tree`. Для демонстрационной цели в примере заполняется только поле `LEVEL`.
```abap
METHOD on_prepare_tree_06.
  FIELD-SYMBOLS:
    <ls_data>     TYPE ts_tree_06.

  " Cast to specefic data
  ASSIGN ir_data->* TO <ls_data>.
  <ls_data>-level = ir_tree->level.
ENDMETHOD.
```

Уровень группировки также будет скопирован в нижестоящие элементы
![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/tree_03.png)
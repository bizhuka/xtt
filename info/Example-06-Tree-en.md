Maybe trees based on [subtotals](Example-%E2%84%9605-Tree-(group-by-fields)) are more common, but in sap there are some other hierarchical data like WBS elements or HR organizational units.<br/>
For this kind of data when number of sublevels are unknown and there is only relation `parent - child` you can call the method `TREE_CREATE_RELAT`.

### Folders hierarchy
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

After filling table lt_folders (dir & par_dir) just pass fields' names to `TREE_CREATE_RELAT` method.
```abap
    GET REFERENCE OF lt_folders INTO lr_table.
    ls_root-t = zcl_xtt_replace_block=>tree_create_relat(
      it_table      = lr_table        " from 7.5 REF #(lt_folders)
      iv_node_key   = 'DIR'
      iv_relat_key  = 'PAR_DIR' ).
```

All subtotals also filled in `prepare_tree` handler. For demonstration purpose in the example only filled `LEVEL` field.
```abap
METHOD on_prepare_tree_06.
  FIELD-SYMBOLS:
    <ls_data>     TYPE ts_tree_06.

  " Cast to specefic data
  ASSIGN ir_data->* TO <ls_data>.
  <ls_data>-level = ir_tree->level.
ENDMETHOD.
```

Outline level also would copy to subitems
![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/tree_03.png)
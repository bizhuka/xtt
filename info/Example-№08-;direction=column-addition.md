All tables and trees in you template could be populated with rows and columns as well.\
But the feature works only for the `ZCL_XTT_EXCEL_XLSX` class and 2 file types (xlsx & xlsm).\
To do this just add special marker {;direction=column} for your table or tree wherever you want.

ABAP code is similar to the [Example №05](Example-%E2%84%9605-Tree-(group-by-fields))
```abap
    SET HANDLER on_prepare_tree_05 ACTIVATION abap_true.

    GET REFERENCE OF lt_rows INTO lr_table.
    ls_root-a = zcl_xtt_replace_block=>tree_create(
     it_table      = lr_table       " from 7.5 REF #(lt_rows)
     iv_fields     = 'GROUP'   ).   " Name of the fields delimited by ;

    SET HANDLER on_prepare_tree_05 ACTIVATION abap_false.
```
The main difference in the template itself.

* Table

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_02.png)
* Tree

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_01.png)

The result will appear like that
* Table

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_03.png)
* Tree

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_04.png)
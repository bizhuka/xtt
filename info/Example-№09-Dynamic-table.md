All you have to do is to **merge** your columns first\
And then just insert main table with the second **merge**

template
![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/09_templ.png)


Declaration
```abap
    " Document structure
    BEGIN OF ts_merge0,
      a TYPE REF TO data, " Tree 1 In template {C-A}
    END OF ts_merge0,

    BEGIN OF ts_merge1,
      t TYPE REF TO data, " Tree 2 In template {R-T}
    END OF ts_merge1.
```

Insert columns as __table__ or __tree__ and then insert main data
```abap
    " Columns
    ro_xtt->merge( is_block = ls_merge0 iv_block_name = 'C' ).

    " Rows
    ro_xtt->merge( ls_merge1 ).
```
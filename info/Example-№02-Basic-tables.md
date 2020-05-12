To represent table in a template you have to declare it as a internal table
```abap
    " Document structure
    BEGIN OF ts_root,
      footer   TYPE string,
      header   TYPE string,
      t        TYPE tt_rand_data, " internal flat table ( In template {R-T} )
      date     TYPE d,            " 8
      time     TYPE t,            " 6
      datetime TYPE char14,       " date(8) + time(6)
    END OF ts_root.
```
Where **tt_rand_data** is a standard table
```abap
      " Random table data
      BEGIN OF ts_rand_data,
        group   TYPE string,
        caption TYPE string,
        date    TYPE d,
        sum1    TYPE p LENGTH 13 DECIMALS 2,
        sum2    TYPE p LENGTH 13 DECIMALS 2,
      END OF ts_rand_data,
      tt_rand_data TYPE STANDARD TABLE OF ts_rand_data WITH DEFAULT KEY,
```
The template

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/basic_table_templ.png)

The pattern for rows can take several rows (From row with first **{R-T}** to the last one)\
\
Data filling
```abap
    " {R-T} in a temaplte. @see get_random_table description
    ls_root-t      = cl_main=>get_random_table( ).

    " For printing
    ls_root-footer = 'Footer'.
    ls_root-header = 'Header'.
```
\
\
The final result for MS Word

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/basic_table_word.png)

All list objects in Excel with corresponding formulas would be properly zoomed.
![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/basic_table_01.png)
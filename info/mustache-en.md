### Mustachioed template engine

&nbsp;&nbsp;&nbsp;Most modern template engines in the web (and also in ABAP) work according to one simple principle. <br/>
**Data + Template = Ready document** <br/>
So how is **XTT** different from its peers?

![image](https://user-images.githubusercontent.com/36256417/81717603-31374d80-9494-11ea-9b6f-7e20d85f7752.png)

<br/>
<br/>
<br/>

***

&nbsp;&nbsp;&nbsp;Without diminishing the merits of more popular report generation libraries, let's compare XTT with the main ones:<br/>
* [XLSX Workbench](https://sites.google.com/site/sapxlwb/home/rus) uses a special **editor** to customize the template

![image](https://user-images.githubusercontent.com/36256417/81719472-8e340300-9496-11ea-816e-90e04bb0c7a0.png)

<br/>
<br/>
<br/>

* [abap2xlsx](https://github.com/sapmentors/abap2xlsx) uses **ZCL_EXCEL_READER_2007** class to read a template

![image](https://user-images.githubusercontent.com/36256417/81720186-7dd05800-9497-11ea-9911-899d3e6ab904.png)

<br/>
<br/>
<br/>


* [ZWWW](https://sapboard.ru/forum/viewtopic.php?f=13&t=4880&sid=12bc45858aa39ac048f08509efa5d5eb) uses the familiar **WYSIWYG** MS Excel or Word editor. <br/>
Bookmarks are also needed in Word (2 previous ones specialize in Excel format) 

![image](https://user-images.githubusercontent.com/36256417/81720960-8aa17b80-9498-11ea-80dc-4a4df507d4d7.png)

<br/>
<br/>
<br/>

***

&nbsp;&nbsp;&nbsp;This opus is more like the latest development and maximizes the capabilities of the WYSIWYG editor itself.<br/>
For data transfer, as in the XLSX Workbench, the concept of context is used.

&nbsp;&nbsp;&nbsp;All you need to do to combine the data with the template is to call the **MERGE** method<br/>
Context **IS_BLOCK** (usually an ABAP structure) matches the structure of the document.<br/>
The root label (usually '**R**') is an optional parameter and is most likely needed if the data is combined with the template several times. 

![image](https://user-images.githubusercontent.com/36256417/81728962-af9beb80-94a4-11ea-88b8-5aa6966c4e82.png)

<br/>
<br/>
<br/>

***

&nbsp;&nbsp;&nbsp;Since the MERGE signature is as simple as possible, the main difficulty can arise with the template itself  

![image](https://user-images.githubusercontent.com/36256417/81731141-fb03c900-94a7-11ea-9528-01423c219b3e.png)

<br/>
<br/>
<br/>

I hope, due to its obviousness, in most cases the study of **{mustachioed syntax}** is not required at all.<br/>
The structure of the document will look approximately the same everywhere.

```abap
    " Document structure
    BEGIN OF ts_root,
      header   TYPE string,
      t        TYPE tt_rand_data, " internal flat table ( In template {R-T} )
      date     TYPE d,            " 8
      datetime TYPE char14,       " date(8) + time(6)
    END OF ts_root.
```

#### MS Excel

![image](https://user-images.githubusercontent.com/36256417/81732894-985ffc80-94aa-11ea-8a8d-7206a7562d44.png)

#### MS Word

![image](https://user-images.githubusercontent.com/36256417/81733163-fdb3ed80-94aa-11ea-93a9-c8cc8a629736.png)

#### Adobe LiveCycle

![image](https://user-images.githubusercontent.com/36256417/81733595-b4b06900-94ab-11ea-9c92-688414b59c56.png)

<br/>
<br/>
<br/>

***

&nbsp;&nbsp;&nbsp;All special features (directives) are always inside **{curly braces}**<br/>
They, quite conditionally, can be divided into 3 main groups:
* Related to one value
* Clarification for the entire table or tree
* And tree output directives

Let's look at each of them individually:

***

<br>

### 1) Directives for one value

&nbsp;&nbsp;&nbsp;The first part of them relates to the clarification of [data type {;type=}](Example-04-Data-types-en.md)<br/>
In general, it is advisable to leave the type as is without converting to others.<br/>
* If the report contains numbers, then they do not need to be converted to strings. If you need to output an empty cell instead of 0, it is easier to use a change in the format itself.<br/>
**Ctrl+1** - > Custom -> **0;-0;;@**

* For dates, conversion to strings is also not advisable. If the report displays a number like **43964** instead of the date, just change the format of the cell itself to the date

But there are exceptions:
* **{;type=boolean}** & **{;type=datetime}** since they are not in the ABAP language itself

* **{;type=mask}** if you need an analog of `WRITE TO` (for example WBS element)

* **{;type=integer}** to convert from type **CHAR** <br/>
If you are 100% sure that only numbers will be in the characters (but even in this case it is advisable to declare the type as **NUMC** for the report)  

With (BTRTL) and without(WERKS) type specification

![image](https://user-images.githubusercontent.com/36256417/81779925-1229d180-950f-11ea-80e9-ea63550de2ec.png)

Result

![image](https://user-images.githubusercontent.com/36256417/81778016-72b70f80-950b-11ea-912b-56e26769d6ba.png)

<br/>
<br/>
<br/>

&nbsp;&nbsp;&nbsp;The second part is related to aggregation functions [;func= SUM | AVG | COUNT | FIRST](Example-05-03-Tree-en.md)
which work in conjunction with **3) tree output directives**. <br/>
For more complex cases there is a special event in ABAP

For simple totals in Excel, you can use standard tools

![image](https://user-images.githubusercontent.com/36256417/81732894-985ffc80-94aa-11ea-8a8d-7206a7562d44.png)

But if you need a universal method that works in Word and Pdf, you can use these functions

![image](https://user-images.githubusercontent.com/36256417/81783258-8450e500-9514-11ea-86ab-1e12cc05cc36.png)

 
***

<br/>

### 2) Clarification for an entire table or tree

&nbsp;&nbsp;&nbsp; Unlike previous directives that were directly in the cell itself,
this kind of directives can be placed anywhere in the template

#### {;direction=column}
[{;direction=column}](Example-08-direction-en.md) works only for in the `ZCL_XTT_EXCEL_XLSX` class and specifies how to display the table.
By default, tables and trees are displayed row by row.

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/dir_column_01.png)

&nbsp;&nbsp;&nbsp;If you need a dynamic table that expands row by row,
and column by column please look at [this example](Example-09-Dynamic-table-en.md)
 
#### {;group=}
&nbsp;Allows you to group tabular data in a hierarchical structure similar to a **tree**. <br/>
&nbsp;To convert a table to a tree, you can use ABAP methods
zcl_xtt_replace_block=>[tree_create](Example-05-01-Tree-en.md) or zcl_xtt_replace_block=>[tree_create_relat](Example-06-Tree-en.md). <br/>
&nbsp;But making such a group declaration in the template itself allows you to give a clear picture of the fact that the table has been transformed into a tree.<br/>

* Instead tree_create => {;group=_Comma-separated table fields_}.<br/>
For subtotals with BUKRS **{R-T;group=BUKRS}**
 
* Instead tree_create_relat => {;group=_Fields Parent-subsidiary through dash_}<br/>
For tree org units by IT 1001 **{R-T;group=OBJID-SOBID}** 

&nbsp;In the simplest case, you need to create simple subtotals,<br/>
**level=0** (overall totals) in which there is **level=1** with the data of the table itself

![image](https://user-images.githubusercontent.com/36256417/81783258-8450e500-9514-11ea-86ab-1e12cc05cc36.png)

<br/>

&nbsp;If we need subtotal on BUKRS **{;group=BUKRS}** we get 3 levels (0,1,2)<br/>
**level=0** Total for all BUKRSs<br/>
**level=1** Total for 1 BUKRS<br/>
**level=2** Table data

In the `ZCL_XTT_EXCEL_XLSX` class, the declaration of a tree at the sheet level makes it possible to display the same table on different sheets with different groupings 

[In this](Example-05-01-Tree-en.md) example, the table {R-T} is displayed differently depending on the declaration on a particular sheet

![image](https://user-images.githubusercontent.com/36256417/81787378-60909d80-951a-11ea-9e94-36ae3666bd75.png)

<br/>

***

<br/>

### 3) Tree output directives
&nbsp;After the table has been converted to a tree, you can set the boundaries and output conditions of each level.<br/>
* **;level=** Specifies for which level this row (column) is intended
* **;top=X** It says that the withdrawal will be carried out **before** a lower level (empty or ;top= that **after** a lower level)
* **;show_if=** & **;hide_if=** allow you to specify the output condition of a particular level <br/>
The condition is written in ABAP and has a special reserved name **row** to refer to the current row in the tree

This example is grouped by 1 field

![image](https://user-images.githubusercontent.com/36256417/81786911-b87ad480-9519-11ea-8e2f-2c30964b3631.png)

Result

![image](https://user-images.githubusercontent.com/36256417/81808568-529e4500-9539-11ea-9523-e273681d3974.png)

&nbsp;More about directives [level, top, show_if, hide_if](Example-05-02-Tree-en.md)
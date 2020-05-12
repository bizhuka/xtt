What if [in the previous example](Example-05-01-Tree-en.md) at the level **{R-T;level=1}** it would not be enough to divide the tree by:
* **{R-T;level=1;top=X}** upper &
* **{R-T;level=1;top=}** bottom<br/>
blocks ?

Sometimes it is necessary to show special HEADER for a certain BUKRS. In Excel, you can use [conditional formatting.](Https://spreadsheeto.com/conditional-formatting/)<br/>
But what if you need to merge several cells in the header or hide the header completely or do it in Word or Pdf?

### ABAP approach
Previously, this could be achieved using the special method ZCL_XTT_REPLACE_BLOCK=<br/>>ON_TREE_CHANGE_LEVEL
in which it was possible to replace level 1 with another one.

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_co_code1.png)

You could just create levels 55, 66 or 77 in the template (Any fairly large level that will certainly not be displayed)

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_co_temp1.png)


and in the event handler change **IV_LEVEL_INDEX->*** with desired level. If you assign a value that is not in the pattern (-1), then the level would simply disappear.

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_co_code2.png)

But this approach has a significant drawback. In order to understand how the final template will look like, you need to go to the ABAP code editor and see these conditions.

### Declarative approach
For the readability of the template 2 additions:
* **show_if** &
* **hide_if**<br/>
was introduced.

They work like the **CASE** statement.<br/>
If all conditions are false, the default block **WHEN OTHERS** will work for the given level. Which could be omitted and would not be displayed in the final report (But explicitly stated **hide_if** is preferable)<br/>
In the example below, line 6 works like _WHEN OTHERS_

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_co_temp2.png)

To indicate the current level the **row** keyword has been introduced(Case insensitive, like any ABAP code). You can write any ABAP condition. For example, except for mathematical conditions like =, <>,> =, <= their analogues EQ, NE, GT, LT can be used. (CP, NP, CO, CN, CA, NA, CS, NS are also acceptable)

As a result, each level **{R-T;level=1}** has its own title:
* **C** - Yellow
* **B** - Consisting of 2 vertically merged cells
* **A** - Default
* **D** - Hidden

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_co_temp3.png)

#### Combining level, top, show_if, hide_if
Any combination is permissible. Conditions can be set or omitted at any level.<br/>
<br/>
If you use only **level** (without top, show_if, hide_if) the subtotals will be displayed below each level (top=abap_false and no conditions for output).<br/>
<br/>
**show_if** and **hide_if** additions can be used without any **level** or **top**. You can call zcl_xtt_replace_block=>tree_create(iv_fields = ''). The table is converted to a tree with levels=0, and for each level you can set your conditional output
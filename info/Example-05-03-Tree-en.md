### Calculation in ABAP
There is a special event for [calculating subtotals](Example-%E2%84%9605-Tree-(group-by-fields)) of the parent level based on the children

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_fm_code_1.png)

where you can write a simple cycle and calculate what you need in a particular case

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_fm_code_2.png)

Casting to a specific type through ASSIGN could cause some difficulty. But in general there is nothing complicated in code.
But this approach is also not entirely informative.

### Aggregation Functions
For that purpose the special addition **;func=** with functions **SUM | AVG | COUNT | FIRST** was introduced.

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_fm_templ_1.png)

_The function can be specified once for the whole level. It is not necessary to write it in each cell like {R-T_CH_COUNT;func=COUNT} for level=1_

Note that in the example for level=0 there is **AVG** function, and for level=1 there is **SUM** function

***FIRST** takes the value from the first child
***COUNT** returns their number

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/05_fm_templ_2.png)

---
Combining programmatic and declarative is possible. But in my opinion in the template it looks a little clearer
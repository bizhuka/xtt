For MS Excel data types could be crucial, since most of the formulas depends on cell's type.
In `ZCL_XTT_REPLACE_BLOCK` attributes you can find the following declaretions.

Basic data types

|Type      | Description      |
|-------------|-------------|
|integer| Template format (int4, int8, b, s) |
|double| Template format (p, f, decfloat16, decfloat34) |
|date| Template format (d) |
|time| Template format (t) |
|string| Template format (string) |
|`datetime`| Virtual type (DATE + TIME) |
|`boolean`|For Excel logical functions (TRUE|FALSE)|
|`mask`|Use WRITE TO|
|`as_is`|Do not escape XML symbols (can contain special chars <>&'..)|

The first 4 data types detected implicitly. But for the boolean and datetime you have to specify type explicitly.
![data_types_01.png](img/data_types_01.png)

As you could see `;type=mask` addition can be used for material numbers to delete leading zeros and for the WBS elements use custom pattern for proper visualisation.

***

There are also defined several constants which detects what kind of data you pass to `merge` method.
* 'struct'
* 'object'
* 'table'
* 'tree'

As you can notice so far we passed only structures and tables to `merge` method.
But also we can pass objets (they work exactly as structures) & and trees [(for hierarchical output)](Example-%E2%84%9605-Tree-(group-by-fields)).

***
If you have in Excel
* [List object](Example-%E2%84%9602-Basic-tables) & pivot tables based on them
* Merged cells
* Defined names
* Formulas (with preceded $ sign)

They would be 'streched' after data pasting in `ZCL_XTT_EXCEL_XLSX` class.

In `ZCL_XTT_EXCEL_XML` only formulas & merged cells would be proccessed correctly\
Since relative references are used in the internal XML (unlike xlsx), there is no need in $ sign.
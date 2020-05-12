While processing a table it is necessary to determine the bounds of the "output area".
That's why you have to  define nested blocks (table of tables etc) very carefully.

```abap

    " Structure of document
    BEGIN OF ts_root,
      title  TYPE string,
      bottom TYPE string,
      t      TYPE tt_rand_data, " Table within another table (lt_root)
    END OF ts_root.
  DATA:
    lt_root TYPE STANDARD TABLE OF ts_root.

...
ro_xtt->merge( is_block = lt_root iv_block_name = 'R' ).
```

If you pass the whole table as a parameter to `merge` method the bounds for
`ZCL_XTT_WORD_DOCX`, `ZCL_XTT_EXCEL_XML` and `ZCL_XTT_PDF` will be the entire document. For the `ZCL_XTT_EXCEL_XML` the bounds will be a spreadsheet.

---

For MS Word the {R} bounds will be the document itself with trailing 'Page Break'
And {R-T} bounds will be 1 row without header

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_word_templ.png)

---

As for 'Xml Spreadsheet 2003' format {R} bounds will be middle sheet

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_2003_templ.png)

And after data replication each ts_root row will be a single sheet

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_2003_res.png)

I want to emphasize that **TITLE** is not a key word. This is just a structure field, not more than

---

For pdf class you have to name replicated page as IV_BLOCK_NAME parameter of MERGE method.
And if you want page breaks within the document set the attribute of the first child as here ... 

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/nested_bl_pdf_templ.png)
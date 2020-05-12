The easiest way to send elementary data to a report is to pass them by a structure
```abap
    " Document structure
    BEGIN OF ts_root,
      title  TYPE char15,
      text   TYPE string,
      int    TYPE i,
      bottom TYPE string, " Any field could be REF TO, STRUCTURE or TABLE
    END OF ts_root.
```

The data could look like
```abap
    ls_root-title   = 'Document title'.
    ls_root-text    = 'Just string'.
    ls_root-int     = 3.
    ls_root-bottom  = 'bottom'.
```
\
The template in Excel, Word or pdf could be like:\
\
Basic example\
Title: **_{R-TITLE}_**\
Just put markers where you want\
Just string    **_{R-TEXT}_**\
Integer        **_{R-INT}_**\
Bottom: **_{R-BOTTOM}_**
***
\
 XTT code for initialization will be the same all the time:
```abap
  " Info about template (Use ZCL_XTT_FILE_OAOR for tr. OAOR)
  CREATE OBJECT:
   lo_file TYPE ZCL_XTT_FILE_SMW0 EXPORTING
     iv_objid = iv_template, " SMW0 binary file

  " The main class (Use ZCL_XTT_WORD_DOCX, ZCL_XTT_PDF for word and pdf respectively)
   lo_xtt TYPE ZCL_XTT_EXCEL_XLSX EXPORTING
    io_file = lo_file.

...

  " R is a marker in the IV_TEMPLATE
  lo_xtt->merge( is_block = ls_root iv_block_name = 'R' ).
```

After calling `download` or `show` methods of **lo_xtt** the final result would be like that:\
\
Basic example\
Title: **_Document title_**\
Just put markers where you want\
Just string    **_Just string_**\
Integer        **_3_**\
Bottom: **_bottom_**\
\
All text formatting within **{}** remains the same.

**In a word, a block that looks the same can consist of several with the same formatting.**\
\
~~To exclude such a case, you need to copy the block from {} to notepad, copy it and paste it back~~

From new version **ZCL_XTT_WORD_DOCX** & **ZCL_XTT_WORD_XML** classes in such cases

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/01_word_part_text.png)

would use a style of the first part 

![](https://raw.githubusercontent.com/wiki/bizhuka/xtt/img/01_word_part_text_f.png)
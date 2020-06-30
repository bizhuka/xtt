Now full documentation available here https://bizhuka.github.io/xtt

### Xml template toolkit

XTT helps you to automate your reporting routine in SAP

- Get existing report from your customers
- Use preferable editor (MS Excel, Word or Adobe LiveCycle Designer) and replace some parts of it with markers inclosed in curly brackets
- Link all formulas, charts and pivot tables to this marksers
- In abap call **`merge()`** method to pass all data including nested strucures, tables and trees
- Use **`download( ) | send( ) | show( )`** method for appropreate action
- That's all!

---

###  Template
![image](https://user-images.githubusercontent.com/36256417/80579411-6b7c0600-8a23-11ea-8166-d48e63b7d085.png)

### Code

```abap
" Template storage class (tr OAOR -> zcl_xtt_file_oaor, external sources -> ZCL_XTT_FILE_RAW)
DATA(lo_file) = NEW zcl_xtt_file_smw0( 'Z_TEMPLATE_ID.XLSX' ).

" Excel (Use ZCL_XTT_WORD_DOCX, ZCL_XTT_PDF for word and pdf respectively)
DATA(lo_xtt) = NEW zcl_xtt_excel_xlsx( io_file = lo_file ).

" R is a marker in the IV_TEMPLATE
lo_xtt->merge( iv_block_name = 'R'
               is_block = VALUE ts_root(
                begda = pn-begda
                endda = pn-endda               
                t     = lt_alv    
               ) ).

" Download to sap_tmp. You could specify path. Show SaveAs dialogue
" Call SHOW( ) for inplace mode
lo_xtt->download( ).
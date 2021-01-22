*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_03.
  TYPES:
    " Second merge
    BEGIN OF ts_doc,
      f_title TYPE string,
      l_title TYPE string,
    END OF ts_doc.

  DATA:
    lt_root TYPE tt_root_03,
    ls_root TYPE REF TO ts_root_03,
    lv_num  TYPE char4,
    ls_doc  TYPE ts_doc,
    lv_rem  TYPE i.

  " Document structure
  DO p_b_cnt TIMES.
    " For detecting blocks
    WRITE sy-index TO lv_num LEFT-JUSTIFIED.
    lv_rem = sy-index MOD 4.

    APPEND INITIAL LINE TO lt_root REFERENCE INTO ls_root.
    CONCATENATE `Title ` lv_num INTO ls_root->title.        "#EC NOTEXT
    CONCATENATE `Bottom ` lv_num INTO ls_root->bottom.      "#EC NOTEXT

    " @see get_random_table description
    get_random_table(
     IMPORTING
       et_table = ls_root->t ).

    CASE lv_rem.
      WHEN 1.
        SORT ls_root->t BY caption.
      WHEN 2.
        SORT ls_root->t BY group.
      WHEN 3.
        SORT ls_root->t BY sum1.
      WHEN 4.
        SORT ls_root->t BY sum2.
    ENDCASE.
  ENDDO.

  " second merge
  ls_doc-f_title = `First title`.                           "#EC NOTEXT
  ls_doc-l_title = `Last title`.                            "#EC NOTEXT

  " Show data structure only
  IF p_stru = abap_true.
    BREAK-POINT ID zxtt_break_point. " Double click here --> lt_root , ls_doc <--

    " For internal use
    CHECK mo_injection IS NOT INITIAL.
    mo_injection->send_merge( i_name = 'R'   i_value = lt_root ).
    mo_injection->send_merge( i_name = 'DOC' i_value = ls_doc ).
  ENDIF.

  " Paste data
  io_xtt->merge( is_block = ls_doc  iv_block_name = 'DOC' ). " Faster if the begining

  " Each 'R' is sheet (Excel) or Page (Pdf) or Endire documnet (Word)
  io_xtt->merge( is_block      = lt_root
                 iv_block_name = 'R' ).

  _2nd_time_for_excel( io_xtt  = io_xtt
                       it_root = lt_root ).
ENDMETHOD.

METHOD _2nd_time_for_excel.
  DATA lo_obj TYPE REF TO cl_abap_objectdescr.
  lo_obj ?= cl_abap_classdescr=>describe_by_object_ref( io_xtt ).

  " No meaning for WORD
  CHECK lo_obj->absolute_name = '\CLASS=ZCL_XTT_EXCEL_XLSX'
   " TODO change examples
*     OR lo_obj->absolute_name = '\CLASS=ZCL_XTT_EXCEL_XML'
*     OR lo_obj->absolute_name = '\CLASS=ZCL_XTT_PDF'
    .

  " Use 'D' anchor for other sheet
  io_xtt->merge( is_block      = it_root
                 iv_block_name = 'D' ).
ENDMETHOD.

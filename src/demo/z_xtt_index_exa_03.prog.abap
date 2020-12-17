*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_03.
  TYPES:
    " Structure of document
    BEGIN OF ts_root,
      title  TYPE string,
      bottom TYPE string,
      t      TYPE tt_rand_data, " Table within another table (lt_root)
    END OF ts_root,

    " Second merge
    BEGIN OF ts_doc,
      f_title TYPE string,
      l_title TYPE string,
    END OF ts_doc.

  DATA:
    lt_root TYPE STANDARD TABLE OF ts_root,
    ls_root TYPE REF TO ts_root,
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
  io_xtt->merge( is_block = lt_root iv_block_name = 'R' ).
  io_xtt->merge( is_block = ls_doc  iv_block_name = 'DOC' ).
ENDMETHOD.

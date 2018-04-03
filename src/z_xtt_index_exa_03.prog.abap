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
    lo_file TYPE REF TO zif_xtt_file,
    lt_root TYPE STANDARD TABLE OF ts_root,
    ls_root TYPE REF TO ts_root,
    lv_num  TYPE char4,
    ls_doc  TYPE ts_doc,
    lv_rem  TYPE i.

  " No need to fill for empty template
  IF p_temp <> abap_true.
    DO p_b_cnt TIMES.
      " For detecting blocks
      WRITE sy-index TO lv_num LEFT-JUSTIFIED.
      lv_rem = sy-index MOD 4.

      APPEND INITIAL LINE TO lt_root REFERENCE INTO ls_root.
      CONCATENATE `Title ` lv_num INTO ls_root->title.
      CONCATENATE `Bottom ` lv_num INTO ls_root->bottom.

      " @see get_random_table description
      ls_root->t = cl_main=>get_random_table( ).

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
    ls_doc-f_title = `First title`.
    ls_doc-l_title = `Last title`.
  ENDIF.

  " Show data structure only
  IF p_stru = abap_true.
    check_break_point_id( ).
    BREAK-POINT ID zxtt_break_point. " Double click here --> lt_root , ls_doc <--
    RETURN.
  ENDIF.

  " Info about template & the main class itself
  CREATE OBJECT:
   lo_file TYPE zcl_xtt_file_smw0 EXPORTING
     iv_objid = iv_template,

   ro_xtt TYPE (iv_class_name) EXPORTING
    io_file = lo_file.

  " Paste data
  IF p_temp <> abap_true.
    ro_xtt->merge( is_block = lt_root iv_block_name = 'R' ).
    ro_xtt->merge( is_block = ls_doc iv_block_name = 'DOC' ).
  ENDIF.
ENDMETHOD.

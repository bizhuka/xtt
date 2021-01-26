class ZCL_XTT_TREE_FUNCTION definition
  public
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_row_offset,
        " INCLUDE TYPE ts_tree_group.
        level    TYPE i,
        top      TYPE abap_bool,
        if_where TYPE string,
        if_show  TYPE abap_bool, " Show or hide
        if_form  TYPE string,    " Name of preform
        " funcs    TYPE SORTED TABLE OF ts_func WITH UNIQUE KEY name,

        first TYPE i,
        last  TYPE i,

        _data TYPE REF TO data,
      END OF ts_row_offset .
  types:
    tt_row_offset TYPE SORTED TABLE OF ts_row_offset WITH UNIQUE KEY level top if_where .

  class-events TREE_CHANGE_LEVEL
    exporting
      value(IR_TREE) type ref to ZCL_XTT_REPLACE_BLOCK=>TS_TREE
      value(IV_BLOCK_NAME) type CSEQUENCE
      value(IV_TOP) type ABAP_BOOL
      value(IV_LEVEL_INDEX) type ref to I .

  methods CONSTRUCTOR
    importing
      !IO_XTT type ref to ZCL_XTT
      !IV_BLOCK_NAME type STRING .
  methods DETECT_OPTIONS
    importing
      !IV_TEXT type CSEQUENCE
      !IV_POS type I
      !IV_PAR_FLD type STRING
    changing
      !CS_ROW_OFFSET type TS_ROW_OFFSET .
protected section.

  data MV_BLOCK_NAME type STRING .
  data MT_ROW_OFFSET type TT_ROW_OFFSET .
  data MO_XTT type ref to ZCL_XTT .

  methods GENERATE_PROG
    importing
      !IR_TREE type ref to ZCL_XTT_REPLACE_BLOCK=>TS_TREE .
  methods FIND_MATCH
    importing
      !IR_TREE type ref to ZCL_XTT_REPLACE_BLOCK=>TS_TREE
      !IV_TOP type ABAP_BOOL
    returning
      value(RR_DATA) type ref to DATA .
private section.

  types:
    BEGIN OF ts_func,
      level   TYPE i,
      name    TYPE string,
      _field  TYPE string,

      path    TYPE stringtab,
      par_fld TYPE string,
      " fields TYPE SORTED TABLE OF string WITH UNIQUE KEY TABLE_LINE,
    END OF ts_func .
  types:
    tt_func TYPE HASHED TABLE OF ts_func WITH UNIQUE KEY level name _field .

  data MT_FUNC type TT_FUNC .
  data MV_CHECK_PROG type PROGRAMM .

  methods CATCH_PREPARE_TREE
    for event PREPARE_TREE of ZCL_XTT_REPLACE_BLOCK
    importing
      !IR_TREE
      !IR_DATA
      !IR_SUB_DATA .
  methods _GET_FIELD_VALUE
    importing
      !IR_ROW type ref to DATA
      !IS_FUNC type TS_FUNC
    returning
      value(RR_REF) type ref to DATA .
ENDCLASS.



CLASS ZCL_XTT_TREE_FUNCTION IMPLEMENTATION.


METHOD catch_prepare_tree.
  DATA:
    lr_ref         TYPE REF TO  data.
  FIELD-SYMBOLS:
    <ls_func>      LIKE LINE OF mt_func,
    <lt_sub_item>  TYPE ANY TABLE, " STANDARD ?
    <lv_field>     TYPE any,
    <lv_sub_field> TYPE any.

  CHECK mt_func IS NOT INITIAL AND ir_sub_data IS NOT INITIAL.

  " Cast to specefic data
  ASSIGN:
   ir_sub_data->*     TO <lt_sub_item>.

  DO 2 TIMES.
    " If no match was found & all formulas in 0 level then 2 times
    CASE sy-index.
      WHEN 1.
        DATA lv_level TYPE i.
        DATA lv_all_0 TYPE abap_bool VALUE abap_true.
        DATA lv_found TYPE abap_bool VALUE abap_false.
        lv_level = ir_tree->level.

      WHEN 2.
        CHECK lv_all_0 = abap_true AND lv_found <> abap_true.
        lv_level = 0.
    ENDCASE.

    LOOP AT mt_func ASSIGNING <ls_func>.
      " Is all formulas for level = 0 ?
      IF <ls_func>-level <> 0.
        lv_all_0 = abap_false.
      ENDIF.

      " Only for certain levele
      CHECK <ls_func>-level = lv_level.

      " Yes match found
      lv_found = abap_true.

      lr_ref = _get_field_value( ir_row  = ir_data
                                 is_func = <ls_func> ).
      CHECK lr_ref IS NOT INITIAL.
      ASSIGN lr_ref->* TO <lv_field>.

      CASE <ls_func>-name.
        WHEN 'COUNT'.
          <lv_field> = lines( <lt_sub_item> ).

        WHEN 'FIRST'. " 'LAST'.
          DATA lr_sub_data TYPE REF TO data.
          LOOP AT <lt_sub_item> REFERENCE INTO lr_sub_data.
            lr_ref = _get_field_value( ir_row  = lr_sub_data
                                       is_func = <ls_func> ).
            CHECK lr_ref IS NOT INITIAL.
            ASSIGN lr_ref->* TO <lv_sub_field>.

            <lv_field> = <lv_sub_field>.
            EXIT.
          ENDLOOP.

        WHEN 'SUM' OR 'AVG'.
          " Calculate sum
          <lv_field> = 0.
          LOOP AT <lt_sub_item> REFERENCE INTO lr_sub_data.
            lr_ref = _get_field_value( ir_row  = lr_sub_data
                                       is_func = <ls_func> ).
            CHECK lr_ref IS NOT INITIAL.
            ASSIGN lr_ref->* TO <lv_sub_field>.

            <lv_field> = <lv_field> + <lv_sub_field>.
          ENDLOOP.

          IF <ls_func>-name = 'AVG'.
            <lv_field> = <lv_field> / lines( <lt_sub_item> ).
          ENDIF.

        WHEN OTHERS.
          MESSAGE e015(zsy_xtt) WITH <ls_func>-name INTO sy-msgli.
          zcx_eui_no_check=>raise_sys_error( ).
      ENDCASE.
    ENDLOOP.
  ENDDO.
ENDMETHOD.


METHOD constructor.
  mo_xtt        = io_xtt.
  mv_block_name = iv_block_name.
ENDMETHOD.


METHOD detect_options.
  DATA:
    lt_text  TYPE stringtab,
    lv_text  TYPE string,
    lv_value TYPE string,
    ls_func  TYPE ts_func.
  FIELD-SYMBOLS:
    <ls_func>    LIKE ls_func,
    <ls_row_off> TYPE ts_row_offset.

  " Read from texts
  lv_text = zcl_xtt_cond=>unescape( iv_text ).

  SPLIT lv_text AT ';' INTO TABLE lt_text.
  LOOP AT lt_text INTO lv_text.
    CLEAR lv_value.
    SPLIT lv_text AT '=' INTO lv_text lv_value.

    CASE lv_text.
      WHEN 'level'.
        CLEAR cs_row_offset.
        cs_row_offset-level = lv_value.

      WHEN 'top'.
        cs_row_offset-top = lv_value.

      WHEN 'show_if'.
        cs_row_offset-if_where = lv_value.
        cs_row_offset-if_show  = abap_true.

      WHEN 'hide_if'.
        cs_row_offset-if_where = lv_value.
        cs_row_offset-if_show  = abap_false.

      WHEN 'func'.
        ls_func-name = lv_value.
        READ TABLE lt_text INTO ls_func-_field INDEX 1.

    ENDCASE.
  ENDLOOP.

  " Exist or new
  READ TABLE mt_row_offset ASSIGNING <ls_row_off>
   FROM cs_row_offset.
  IF sy-subrc <> 0.
    CLEAR:
     cs_row_offset-first,
     cs_row_offset-last. " Clear only here !!!
    INSERT cs_row_offset INTO TABLE mt_row_offset ASSIGNING <ls_row_off>.
  ENDIF.

  <ls_row_off>-last = iv_pos.
  IF <ls_row_off>-first IS INITIAL.
    <ls_row_off>-first = <ls_row_off>-last.
  ENDIF.

***************************
  " Add function
  CHECK ls_func-name IS NOT INITIAL.

  " Current level
  ls_func-level   = cs_row_offset-level.
  ls_func-par_fld = iv_par_fld.

  TRANSLATE:
   ls_func-name  TO UPPER CASE,
   ls_func-_field TO UPPER CASE.

  " has FM ?
  DATA lv_prefix TYPE string.
  CONCATENATE iv_par_fld '-' INTO lv_prefix.
  CHECK ls_func-_field CS lv_prefix.

  " Path in strucure
  DATA lv_off TYPE i.
  lv_off = sy-fdpos + strlen( lv_prefix ).
  lv_text = ls_func-_field+lv_off.
  SPLIT lv_text AT '-' INTO TABLE ls_func-path.

  " And add
  INSERT ls_func INTO TABLE mt_func.
ENDMETHOD.


METHOD find_match.
  DATA:
    lv_level_index TYPE REF TO i,
    lv_last_top    TYPE abap_bool,
    lv_last_index  TYPE i,
    lv_ok          TYPE abap_bool.
  FIELD-SYMBOLS:
    <ls_data>    TYPE any,
    <ls_row_off> LIKE LINE OF mt_row_offset.

  " Data to ANY
  ASSIGN ir_tree->data->* TO <ls_data>.

  " What level to use
  CREATE DATA lv_level_index.
  lv_level_index->* = -1.
  lv_last_top       = iv_top.

  IF iv_top <> abap_undefined.
    lv_level_index->* = ir_tree->level.
  ELSE.
    lv_last_index = lines( mt_row_offset ).
    READ TABLE mt_row_offset ASSIGNING <ls_row_off> INDEX lv_last_index.
    IF sy-subrc = 0.
      lv_level_index->* = <ls_row_off>-level.
      lv_last_top       = <ls_row_off>-top.
    ENDIF.
  ENDIF.

  " Change level by external condition
  RAISE EVENT tree_change_level EXPORTING
    ir_tree        = ir_tree
    iv_block_name  = mv_block_name
    iv_top         = iv_top
    iv_level_index = lv_level_index.

  " Check all conditions
  LOOP AT mt_row_offset ASSIGNING <ls_row_off>.
    " Slow match by level and top
    CHECK <ls_row_off>-level = lv_level_index->*
      AND <ls_row_off>-top   = lv_last_top.

    " Just get default row group
    IF <ls_row_off>-if_form IS INITIAL.
      rr_data = <ls_row_off>-_data.
    ENDIF.

    CHECK <ls_row_off>-if_form IS NOT INITIAL AND
          mv_check_prog IS NOT INITIAL.

    CLEAR lv_ok.
    PERFORM (<ls_row_off>-if_form) IN PROGRAM (mv_check_prog) IF FOUND
      USING
            <ls_data>
      CHANGING
            lv_ok.

    CHECK lv_ok = abap_true.

    " Hide ?
    IF <ls_row_off>-if_show = abap_true.
      rr_data = <ls_row_off>-_data.
    ELSE.
      CLEAR rr_data.
    ENDIF.

    EXIT.
  ENDLOOP.
ENDMETHOD.


METHOD generate_prog.
  " Raise event
  SET HANDLER catch_prepare_tree.
  zcl_xtt_replace_block=>tree_raise_prepare(
   ir_tree  = ir_tree
   iv_level = 0 ).
  CLEAR mt_func.

  " Yes create new program on fly
  DATA lv_ok TYPE abap_bool.
  LOOP AT mt_row_offset TRANSPORTING NO FIELDS WHERE if_where IS NOT INITIAL. "#EC CI_SORTSEQ
    lv_ok = abap_true.
    EXIT.
  ENDLOOP.

  " No need in program
  CHECK lv_ok = abap_true.

  " Code holder
  DATA lo_cond TYPE REF TO zcl_xtt_cond.
  CREATE OBJECT lo_cond
    EXPORTING
      io_xtt = mo_xtt.

  " add TYPE TS_ROW
  lo_cond->get_type( EXPORTING is_data = ir_tree->data
                     IMPORTING ev_type = lo_cond->mv_root_type ).

  lo_cond->make_tree_forms( IMPORTING ev_prog       = mv_check_prog
                            CHANGING  ct_row_offset = mt_row_offset ).
ENDMETHOD.


METHOD _get_field_value.
  CHECK is_func-path IS NOT INITIAL.

  FIELD-SYMBOLS <ls_data> TYPE any.
  ASSIGN ir_row->* TO <ls_data>.

  DATA lv_path TYPE string.
  LOOP AT is_func-path INTO lv_path.
    ASSIGN COMPONENT lv_path OF STRUCTURE <ls_data> TO <ls_data>.

    IF sy-subrc <> 0.
      MESSAGE w022(zsy_xtt) WITH lv_path is_func-par_fld INTO sy-msgli.
      mo_xtt->add_log_message( iv_syst = abap_true ).
      RETURN.
    ENDIF.
  ENDLOOP.

  GET REFERENCE OF <ls_data> INTO rr_ref.
ENDMETHOD.
ENDCLASS.

CLASS zcl_xtt_report DEFINITION PUBLIC CREATE PUBLIC .

  PUBLIC SECTION.
    DATA mv_test_mode  TYPE abap_bool    READ-ONLY.

    DATA:
      mv_r_cnt TYPE int4       READ-ONLY,
      mv_c_cnt TYPE numc2      READ-ONLY,
      mv_b_cnt TYPE int4       READ-ONLY.

    " Random numbers
    DATA mo_rand_i     TYPE REF TO cl_abap_random_int.
    DATA mo_rand_p     TYPE REF TO cl_abap_random_packed.

    " Current example
    DATA o_demo        TYPE REF TO zcl_xtt_demo     READ-ONLY.
    DATA t_merge       TYPE zcl_xtt_demo=>tt_merge  READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          iv_test_mode TYPE abap_bool OPTIONAL,

      merge_add_one
        IMPORTING
                  is_root         TYPE any
                  iv_root_id      TYPE string DEFAULT 'R'
                  io_helper       TYPE REF TO object OPTIONAL
        RETURNING VALUE(rs_merge) TYPE zcl_xtt_demo=>ts_merge,

      " Random data for tables
      get_random_table
        IMPORTING
          iv_column_cnt TYPE numc2 DEFAULT 2
        EXPORTING
          et_table      TYPE STANDARD TABLE,

      init_random_generator,

      get_template_by_f4
        RETURNING VALUE(rv_template) TYPE string.

  PROTECTED SECTION.

ENDCLASS.



CLASS zcl_xtt_report IMPLEMENTATION.


  METHOD constructor.
    mv_test_mode = iv_test_mode.
  ENDMETHOD.

  METHOD merge_add_one.
    FIELD-SYMBOLS <l_value> TYPE any.

    " № 1 - merge IV_BLOCK_NAME parameter
    rs_merge-key = iv_root_id.

    " № 2 - merge IS_BLOCK parameter (make copy)
    CREATE DATA rs_merge-val LIKE is_root.
    ASSIGN rs_merge-val->* TO <l_value>.
    <l_value> = is_root.

    " № 3 - for 160 only
    rs_merge-obj = io_helper.

    INSERT rs_merge INTO TABLE t_merge.
  ENDMETHOD.


  METHOD get_random_table.
    DATA:
      ls_no_sum TYPE zcl_xtt_demo=>ts_no_sum,
      lv_int    TYPE i,
      lv_column TYPE string.
    FIELD-SYMBOLS:
      <ls_item> TYPE any,
      <lv_sum>  TYPE bf_rbetr. " P with sign
    init_random_generator( ).

    CLEAR et_table.
    DO mv_r_cnt TIMES.
      " Fill without sums
      CLEAR ls_no_sum.

      " Special XML symbols <>
      ls_no_sum-caption = sy-index.
      CONDENSE ls_no_sum-caption.
      CONCATENATE `<Caption ` ls_no_sum-caption ` />` INTO ls_no_sum-caption.

      " Date
      lv_int = mo_rand_i->get_next( ).
      ls_no_sum-date = sy-datum - lv_int.

      " 3 different groups
      lv_int = lv_int + 65.
      ls_no_sum-group = cl_abap_conv_in_ce=>uccpi( lv_int ).
      CONCATENATE `GRP ` ls_no_sum-group INTO ls_no_sum-group.

      " And finally sums
**********************************************************************
      " in Word and pdf (except Excel formats), 'P' type always has dot as a delimiter
      " If 'N' type has conversion exit it will transformed to mask type
      " Use ;type=mask addition in template for using WRITE ... TO
**********************************************************************

      " Write without sums
      APPEND INITIAL LINE TO et_table ASSIGNING <ls_item>.
      MOVE-CORRESPONDING ls_no_sum TO <ls_item>.

      " For 092 example
      FIELD-SYMBOLS <lt_sums> TYPE zcl_xtt_demo=>tt_sums_alv.
      FIELD-SYMBOLS <ls_sums> TYPE zcl_xtt_demo=>ts_sum_alv.
      ASSIGN COMPONENT 'T_SUMS' OF STRUCTURE <ls_item> TO <lt_sums>.

      " Fill R-T-SUM*
      DO iv_column_cnt TIMES.
        " Get column name
        lv_column = sy-index.
        CONDENSE lv_column.

        IF <lt_sums> IS ASSIGNED.
          APPEND INITIAL LINE TO <lt_sums> ASSIGNING <ls_sums>.
          ASSIGN <ls_sums>-sum TO <lv_sum>.
        ELSE.
          " Fields like SUM1, SUM2 ...
          CONCATENATE `SUM` lv_column INTO lv_column.

          " Exist ?
          ASSIGN COMPONENT lv_column OF STRUCTURE <ls_item> TO <lv_sum>.
          IF sy-subrc <> 0.
            zcx_xtt_exception=>raise_dump( iv_message = 'Check data structure'(cds) ).
          ENDIF.
        ENDIF.

        " Show with decimals
        <lv_sum> = mo_rand_p->get_next( ).
        <lv_sum> = <lv_sum> / 100.
      ENDDO.
    ENDDO.
  ENDMETHOD.


  METHOD init_random_generator.
    CHECK mo_rand_i IS INITIAL OR mv_test_mode = abap_true.

    " Always the same random data
    DATA lv_seed TYPE i.
    IF mv_test_mode = abap_true.
      lv_seed  = 777.
    ENDIF.
    " A,B,C,D chars
    mo_rand_i = cl_abap_random_int=>create( seed = lv_seed
                                            min  = 0
                                            max  = 3 ).
    " SUMS
    mo_rand_p = cl_abap_random_packed=>create( seed = lv_seed
                                               min  = 0
                                               max  = 1000000 ).
  ENDMETHOD.


  METHOD get_template_by_f4.
    DATA lt_template TYPE zcl_xtt_demo=>tt_template.
    lt_template = o_demo->get_templates( ).

    " No examples ?
    CHECK lt_template IS NOT INITIAL.

    " No need to show SH
    IF lines( lt_template ) = 1.
      READ TABLE lt_template INTO rv_template INDEX 1.
      RETURN.
    ENDIF.

    DATA lt_template_txt TYPE zcl_xtt_demo=>tt_vrm_value.
    lt_template_txt = o_demo->get_template_lisbox( ).

    " Show dialog
    DATA lt_return    TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.
    DATA lr_return    TYPE REF TO ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'KEY'
        value_org  = 'S'
      TABLES
        value_tab  = lt_template_txt
        return_tab = lt_return
      EXCEPTIONS
        OTHERS     = 3.
    CHECK sy-subrc = 0.

    READ TABLE lt_return REFERENCE INTO lr_return INDEX 1.
    CHECK sy-subrc = 0.

    rv_template = lr_return->fieldval.
  ENDMETHOD.
ENDCLASS.

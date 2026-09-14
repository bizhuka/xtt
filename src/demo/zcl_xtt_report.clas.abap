CLASS zcl_xtt_report DEFINITION PUBLIC CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      mv_test_mode TYPE abap_bool  READ-ONLY,
      mv_r_cnt     TYPE int4       READ-ONLY,
      mv_c_cnt     TYPE numc2      READ-ONLY,
      mv_b_cnt     TYPE int4       READ-ONLY.

    " Random numbers
    DATA mo_rand_i     TYPE REF TO cl_abap_random_int.
    DATA mo_rand_p     TYPE REF TO cl_abap_random_packed.

    " Current example
    DATA o_demo        TYPE REF TO zcl_xtt_demo     READ-ONLY.
    DATA t_merge       TYPE zcl_xtt_demo=>tt_merge  READ-ONLY.

    METHODS:
      constructor,

      init
        IMPORTING
          iv_ind       TYPE char3
          iv_test_mode TYPE abap_bool
          iv_r_cnt     TYPE int4
          iv_c_cnt     TYPE numc2
          iv_b_cnt     TYPE int4,

      prepare
        IMPORTING
          io_xtt TYPE REF TO zcl_xtt,

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
    TYPES:
      BEGIN OF ts_demo,
        ind  TYPE char3,
        inst TYPE REF TO zcl_xtt_demo,
      END OF ts_demo,
      tt_demo TYPE SORTED TABLE OF ts_demo WITH UNIQUE KEY ind.

    DATA:
      t_demo       TYPE tt_demo,
      mv_raw_folder TYPE string.

  PRIVATE SECTION.
    METHODS on_prepare_raw FOR EVENT prepare_raw OF zcl_xtt
      IMPORTING
        iv_path
        ir_content.
ENDCLASS.



CLASS zcl_xtt_report IMPLEMENTATION.
  METHOD constructor.
    t_demo[] = lcl_helper=>get_all_demos( me ).
  ENDMETHOD.

  METHOD init.
    DATA lr_demo TYPE REF TO ts_demo.

    mv_test_mode = iv_test_mode.
    mv_r_cnt     = iv_r_cnt.
    mv_c_cnt     = iv_c_cnt.
    mv_b_cnt     = iv_b_cnt.

    " Data for report & ALV items
    CLEAR: o_demo, t_merge.

    READ TABLE t_demo REFERENCE INTO lr_demo WITH TABLE KEY ind = iv_ind.
    IF sy-subrc = 0.
      o_demo = lr_demo->inst.
    ENDIF.
  ENDMETHOD.

  METHOD prepare.
    CHECK mv_test_mode = abap_true
      AND mv_raw_folder IS NOT INITIAL.

    SET HANDLER on_prepare_raw FOR io_xtt.

    DATA lo_class TYPE REF TO cl_abap_classdescr.
    lo_class ?= cl_abap_classdescr=>describe_by_object_ref( io_xtt ).

    CASE lo_class->absolute_name.
      WHEN '\CLASS=ZCL_XTT_WORD_DOCX'.
        " io_xtt->add_raw_event( 'word/document.xml' ).
        io_xtt->add_raw_event( 'word/header1.xml' ).
        io_xtt->add_raw_event( 'word/footer1.xml' ).

      WHEN '\CLASS=ZCL_XTT_EXCEL_XLSX'.
        io_xtt->add_raw_event( 'xl/workbook.xml' ).
        io_xtt->add_raw_event( 'xl/_rels/workbook.xml.rels' ).

        DO 12 TIMES.
          DATA lv_path TYPE string.
          lv_path = sy-index.
          CONDENSE lv_path NO-GAPS.
          CONCATENATE `xl/worksheets/sheet` lv_path `.xml` INTO lv_path.
          io_xtt->add_raw_event( lv_path ).
        ENDDO.
        io_xtt->add_raw_event( `xl/worksheets/sheet999.xml` ).
        io_xtt->add_raw_event( `xl/sharedStrings.xml` ).
    ENDCASE.
  ENDMETHOD.

  METHOD on_prepare_raw.
    CHECK iv_path IS NOT INITIAL.

    DATA lv_content TYPE xstring.
    lv_content = lcl_helper=>pretty_print( ir_content->* ).

    DATA lv_path TYPE string.
    IF sy-saprl = 'OPEN'.
      CONCATENATE `./output/result/raw/` mv_raw_folder `/` iv_path INTO lv_path.
    ELSE.
      CONCATENATE `C:\Users\modekz\AppData\Local\SAP\SAP GUI\tmp\`
                  mv_raw_folder `\` iv_path INTO lv_path.
      REPLACE ALL OCCURRENCES OF `/` IN lv_path WITH `\`.
    ENDIF.

    DATA lo_file  TYPE REF TO zcl_eui_file.
    DATA lo_error TYPE REF TO zcx_eui_exception.

    TRY.
        CREATE OBJECT lo_file.
        lo_file->import_from_xstring( lv_content ).
        lo_file->download( iv_full_path = lv_path ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
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

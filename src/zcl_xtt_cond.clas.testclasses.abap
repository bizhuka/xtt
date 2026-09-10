"*" use this source file for the definition and implementation of
"*" local helper classes, interface definitions and type declarations

CLASS lcl_test  DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      generate    FOR TESTING RAISING zcx_xtt_exception,
      _702_cond   FOR TESTING RAISING zcx_xtt_exception,
      _702_concat FOR TESTING RAISING zcx_xtt_exception.
ENDCLASS.

CLASS lcl_expression_text_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                                .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      system_fields         FOR TESTING RAISING zcx_xtt_exception,
      pipe_template         FOR TESTING RAISING zcx_xtt_exception,
      arithmetic            FOR TESTING RAISING zcx_xtt_exception,
      multiple_spaces       FOR TESTING RAISING zcx_xtt_exception,
      negative_arithmetic   FOR TESTING RAISING zcx_xtt_exception,
      table_expression      FOR TESTING RAISING zcx_xtt_exception,
      table_condition_expr  FOR TESTING RAISING zcx_xtt_exception,
      conditional_then_else FOR TESTING RAISING zcx_xtt_exception,
      switch_expression     FOR TESTING RAISING zcx_xtt_exception,
      to_lower_function     FOR TESTING RAISING zcx_xtt_exception,
      to_upper_function     FOR TESTING RAISING zcx_xtt_exception,
      to_mixed_function     FOR TESTING RAISING zcx_xtt_exception,
      substring_offset_len  FOR TESTING RAISING zcx_xtt_exception.
ENDCLASS.

CLASS lcl_expression_boolean_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                                   .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      compound_condition FOR TESTING RAISING zcx_xtt_exception,
      equality_condition FOR TESTING RAISING zcx_xtt_exception,
      raw_dynamic_form   FOR TESTING RAISING zcx_xtt_exception,
      nested_parentheses    FOR TESTING RAISING zcx_xtt_exception, " NEW
      numeric_comparisons   FOR TESTING RAISING zcx_xtt_exception, " NEW
      is_initial_test       FOR TESTING RAISING zcx_xtt_exception, " NEW
      string_contains_cs_ns FOR TESTING RAISING zcx_xtt_exception. " NEW
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_expression_text_test IMPLEMENTATION.
  METHOD system_fields.
    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA lv_result TYPE string.

    CREATE OBJECT lo_calc.
    TRY.
        lo_calc->compile( `|ABC { sy-datum } { sy-uzeit }|` ).
      CATCH zcx_xtt_exception.
    ENDTRY.

    lv_result = lo_calc->evaluate( sy ).
    IF lv_result <> |ABC { sy-datum } { sy-uzeit }|.
      zcx_xtt_exception=>raise_sys_error( iv_message = |system_fields: { lv_result }| ).
    ENDIF.
  ENDMETHOD.

  METHOD pipe_template.
    TYPES:
      BEGIN OF ts_sample_row,
        sum1 TYPE i,
        sum2 TYPE p LENGTH 8 DECIMALS 2,
        sum3 TYPE p LENGTH 8 DECIMALS 2,
      END OF ts_sample_row.

    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA ls_value  TYPE ts_sample_row.
    DATA lv_total  TYPE i.
    DATA lv_result TYPE string.

    ls_value-sum1 = 4.
    ls_value-sum2 = '4.50'.
    ls_value-sum3 = '5.50'.

    CREATE OBJECT lo_calc.
    TRY.
        lo_calc->compile( `|Total: { value-SUM1 * (value-SUM2 + value-SUM3) } USD (Date: { sy-datum })|` ).
      CATCH zcx_xtt_exception.
    ENDTRY.

    lv_result = lo_calc->evaluate( ls_value ).
    lv_total  = ls_value-sum1 * ( ls_value-sum2 + ls_value-sum3 ).
    IF lv_result <> |Total: { lv_total } USD (Date: { sy-datum })|.
      zcx_xtt_exception=>raise_sys_error( iv_message = |pipe_template: { lv_result }| ).
    ENDIF.
  ENDMETHOD.

  METHOD arithmetic.
    TYPES:
      BEGIN OF ts_sample_row,
        sum1 TYPE i,
        sum2 TYPE p LENGTH 8 DECIMALS 2,
        sum3 TYPE i,
      END OF ts_sample_row.

    DATA ls_data   TYPE ts_sample_row.
    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA lv_result TYPE string.
    DATA lv_number TYPE decfloat34.

    ls_data-sum1 = 4.
    ls_data-sum2 = '2.50'.
    ls_data-sum3 = 3.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `value-SUM1 * (value-SUM2 + value-SUM3)` ).
    lv_result = lo_calc->evaluate( ls_data ).
    lv_number = lv_result.

    IF lv_number <> 22.
      zcx_xtt_exception=>raise_sys_error( iv_message = |arithmetic: expected 22 but got '{ lv_result }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD multiple_spaces.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_res  TYPE string.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `|Hello    World|` ).
    lv_res = lo_calc->evaluate( sy ).
    IF lv_res <> `Hello    World`.
      zcx_xtt_exception=>raise_sys_error( iv_message = |multiple_spaces failed: '{ lv_res }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD negative_arithmetic.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_res  TYPE string.
    DATA lv_num  TYPE decfloat34.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `-10 + 4 * -2` ).
    lv_res = lo_calc->evaluate( sy ).
    lv_num = lv_res.
    IF lv_num <> -18.
      zcx_xtt_exception=>raise_sys_error( iv_message = |negative_arithmetic: expected -18 got '{ lv_res }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD table_expression.
    TYPES:
      BEGIN OF ts_sum_item,
        sum TYPE p LENGTH 8 DECIMALS 2,
      END OF ts_sum_item,
      tt_sum_item TYPE STANDARD TABLE OF ts_sum_item WITH DEFAULT KEY,

      BEGIN OF ts_sample_row,
        t_sums TYPE tt_sum_item,
      END OF ts_sample_row.

    DATA ls_data TYPE ts_sample_row.
    DATA ls_sum  TYPE ts_sum_item.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_res1 TYPE string.
    DATA lv_res2 TYPE string.
    DATA lv_res3 TYPE string.
    DATA lv_num  TYPE decfloat34.

    ls_sum-sum = '10.50'.
    APPEND ls_sum TO ls_data-t_sums.
    ls_sum-sum = '20.50'.
    APPEND ls_sum TO ls_data-t_sums.

    CREATE OBJECT lo_calc.

    " 1. Direct field access with spaces [ 1 ]
    lo_calc->compile( `value-T_SUMS[ 1 ]-SUM` ).
    lv_res1 = lo_calc->evaluate( ls_data ).
    IF lv_res1 <> '10.50'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |table_expression 1 failed: expected 10.50 got '{ lv_res1 }'| ).
    ENDIF.

    " 2. Inside arithmetic
    lo_calc->compile( `value-T_SUMS[ 1 ]-SUM + value-T_SUMS[ 2 ]-SUM` ).
    lv_res2 = lo_calc->evaluate( ls_data ).
    lv_num  = lv_res2.
    IF lv_num <> 31.
      zcx_xtt_exception=>raise_sys_error( iv_message = |table_expression 2 failed: expected 31 got '{ lv_res2 }'| ).
    ENDIF.

    " 3. Inside string template
    lo_calc->compile( `|Sum1: { value-T_SUMS[ 1 ]-SUM } USD, Sum2: { value-T_SUMS[ 2 ]-SUM } USD|` ).
    lv_res3 = lo_calc->evaluate( ls_data ).
    IF lv_res3 <> `Sum1: 10.50 USD, Sum2: 20.50 USD`.
      zcx_xtt_exception=>raise_sys_error( iv_message = |table_expression 3 failed: '{ lv_res3 }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD table_condition_expr.
    TYPES:
      BEGIN OF ts_row_item,
        group   TYPE string,
        caption TYPE string,
        amount  TYPE i,
      END OF ts_row_item,
      tt_row_item TYPE STANDARD TABLE OF ts_row_item WITH DEFAULT KEY,

      BEGIN OF ts_root,
        t TYPE tt_row_item,
      END OF ts_root.

    DATA ls_data TYPE ts_root.
    DATA ls_item TYPE ts_row_item.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_res1 TYPE string.
    DATA lv_res2 TYPE string.
    DATA lv_res3 TYPE string.

    ls_item-group   = 'GRP B'.
    ls_item-caption = 'Cap B'.
    ls_item-amount  = 20.
    APPEND ls_item TO ls_data-t.

    ls_item-group   = 'GRP A'.
    ls_item-caption = 'Cap A'.
    ls_item-amount  = 50.
    APPEND ls_item TO ls_data-t.

    ls_item-group   = 'GRP C'.
    ls_item-caption = 'Cap C'.
    ls_item-amount  = 99.
    APPEND ls_item TO ls_data-t.

    CREATE OBJECT lo_calc.

    " 1. Condition inside string template
    lo_calc->compile( `|First caption in group 'A' { value-t[ group = 'GRP A' ]-caption }|` ).
    lv_res1 = lo_calc->evaluate( ls_data ).
    IF lv_res1 <> `First caption in group 'A' Cap A`.
      zcx_xtt_exception=>raise_sys_error( iv_message = |table_condition 1 failed: '{ lv_res1 }'| ).
    ENDIF.

    " 2. Compound condition with AND
    lo_calc->compile( `value-t[ group = 'GRP A' AND amount = 50 ]-caption` ).
    lv_res2 = lo_calc->evaluate( ls_data ).
    IF lv_res2 <> 'Cap A'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |table_condition 2 failed: '{ lv_res2 }'| ).
    ENDIF.

    " 3. Numeric index
    lo_calc->compile( `value-t[ 1 ]-caption` ).
    lv_res3 = lo_calc->evaluate( ls_data ).
    IF lv_res3 <> 'Cap B'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |table_condition 3 failed: '{ lv_res3 }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD conditional_then_else.
    TYPES:
      BEGIN OF ts_row,
        group TYPE string,
        sum1  TYPE i,
        sum2  TYPE i,
      END OF ts_row.

    DATA ls_row_a   TYPE ts_row.
    DATA ls_row_b   TYPE ts_row.
    DATA lo_calc    TYPE REF TO lcl_expression.
    DATA lv_res_a   TYPE string.
    DATA lv_res_b   TYPE string.
    DATA lv_res_tmpl TYPE string.

    ls_row_a-group = 'GRP A'.
    ls_row_a-sum1  = 10.
    ls_row_a-sum2  = 5.

    ls_row_b-group = 'GRP B'.
    ls_row_b-sum1  = 10.
    ls_row_b-sum2  = 5.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `WHEN value-GROUP cp '*A*' THEN value-SUM1 + value-SUM2 ELSE value-SUM1 - value-SUM2` ).

    " 1. True branch: 10 + 5 = 15
    lv_res_a = lo_calc->evaluate( ls_row_a ).
    IF lv_res_a <> '15'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |conditional_then_else A failed: expected 15 got '{ lv_res_a }'| ).
    ENDIF.

    " 2. False branch: 10 - 5 = 5
    lv_res_b = lo_calc->evaluate( ls_row_b ).
    IF lv_res_b <> '5'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |conditional_then_else B failed: expected 5 got '{ lv_res_b }'| ).
    ENDIF.

    " 3. Inside string template with COND #( ... )
    lo_calc->compile( `|Result: { COND #( WHEN value-GROUP cp '*A*' THEN value-SUM1 + value-SUM2 ELSE value-SUM1 - value-SUM2 ) }|` ).
    lv_res_tmpl = lo_calc->evaluate( ls_row_a ).
    IF lv_res_tmpl <> `Result: 15`.
      zcx_xtt_exception=>raise_sys_error( iv_message = |conditional_then_else template failed: '{ lv_res_tmpl }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD switch_expression.
    TYPES:
      BEGIN OF ts_person,
        gesch TYPE c LENGTH 1,
      END OF ts_person.

    DATA ls_person TYPE ts_person.
    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA lv_res    TYPE string.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `SWITCH #( value-GESCH WHEN '1' THEN 'M' WHEN '2' THEN 'F' ELSE 'U' )` ).

    " 1. Test WHEN '1' -> 'M'
    ls_person-gesch = '1'.
    lv_res = lo_calc->evaluate( ls_person ).
    IF lv_res <> 'M'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |switch WHEN 1 failed: expected M got '{ lv_res }'| ).
    ENDIF.

    " 2. Test WHEN '2' -> 'F'
    ls_person-gesch = '2'.
    lv_res = lo_calc->evaluate( ls_person ).
    IF lv_res <> 'F'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |switch WHEN 2 failed: expected F got '{ lv_res }'| ).
    ENDIF.

    " 3. Test ELSE -> 'U'
    ls_person-gesch = '9'.
    lv_res = lo_calc->evaluate( ls_person ).
    IF lv_res <> 'U'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |switch ELSE failed: expected U got '{ lv_res }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD to_lower_function.
    TYPES:
      BEGIN OF ts_person,
        nachn TYPE string,
        vorna TYPE string,
        midnm TYPE string,
      END OF ts_person.

    DATA ls_person   TYPE ts_person.
    DATA lo_calc     TYPE REF TO lcl_expression.
    DATA lv_res      TYPE string.
    DATA lv_expected TYPE string.

    ls_person-nachn = 'DOE'.
    ls_person-vorna = 'JOHN'.
    ls_person-midnm = 'FITZGERALD'.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `to_lower( |{ value-NACHN } { value-VORNA } { value-MIDNM }| )` ).

    lv_res = lo_calc->evaluate( ls_person ).

    lv_expected = |{ ls_person-nachn } { ls_person-vorna } { ls_person-midnm }|.
    TRANSLATE lv_expected TO LOWER CASE.

    IF lv_res <> lv_expected.
      zcx_xtt_exception=>raise_sys_error( iv_message = |to_lower failed: expected '{ lv_expected }' got '{ lv_res }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD to_upper_function.
    TYPES:
      BEGIN OF ts_person,
        nachn TYPE string,
        vorna TYPE string,
        midnm TYPE string,
      END OF ts_person.

    DATA ls_person   TYPE ts_person.
    DATA lo_calc     TYPE REF TO lcl_expression.
    DATA lv_res      TYPE string.
    DATA lv_expected TYPE string.

    ls_person-nachn = 'doe'.
    ls_person-vorna = 'john'.
    ls_person-midnm = 'fitzgerald'.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `to_upper( |{ value-NACHN } { value-VORNA } { value-MIDNM }| )` ).

    lv_res = lo_calc->evaluate( ls_person ).

    lv_expected = |{ ls_person-nachn } { ls_person-vorna } { ls_person-midnm }|.
    TRANSLATE lv_expected TO UPPER CASE.

    IF lv_res <> lv_expected.
      zcx_xtt_exception=>raise_sys_error( iv_message = |to_upper failed: expected '{ lv_expected }' got '{ lv_res }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD to_mixed_function.
    TYPES:
      BEGIN OF ts_person,
        nachn TYPE string,
        vorna TYPE string,
        midnm TYPE string,
      END OF ts_person.

    DATA ls_person   TYPE ts_person.
    DATA lo_calc     TYPE REF TO lcl_expression.
    DATA lv_res      TYPE string.
    DATA lv_expected TYPE string.

    ls_person-nachn = 'DOE'.
    ls_person-vorna = 'JOHN'.
    ls_person-midnm = 'FITZGERALD'.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `to_mixed( |{ value-NACHN }_{ value-VORNA }_{ value-MIDNM }| )` ).

    lv_res = lo_calc->evaluate( ls_person ).

    lv_expected = 'DoeJohnFitzgerald'.

    IF lv_res <> lv_expected.
      zcx_xtt_exception=>raise_sys_error( iv_message = |to_mixed failed: expected '{ lv_expected }' got '{ lv_res }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD substring_offset_len.
    TYPES:
      BEGIN OF ts_person,
        midnm TYPE string,
      END OF ts_person.

    DATA ls_person TYPE ts_person.
    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA lv_res    TYPE string.

    ls_person-midnm = '0123456789ABCDEF'.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `value-MIDNM+10(3)` ).

    lv_res = lo_calc->evaluate( ls_person ).
    IF lv_res <> 'ABC'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |substring offset+len failed: expected ABC got '{ lv_res }'| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_expression_boolean_test IMPLEMENTATION.
  METHOD compound_condition.
    TYPES:
      BEGIN OF ts_rand_data,
        group TYPE string,
        val   TYPE i,
      END OF ts_rand_data.

    DATA ls_row    TYPE ts_rand_data.
    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA lv_result TYPE abap_bool.

    ls_row-group = 'C'.
    ls_row-val   = 10.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `( row-GROUP = 'C' OR ROW-group cp '*C' ) AND ROW-GROUP <> 'Z'` ).
    lv_result = lo_calc->evaluate_bool( ls_row ).

    IF lv_result <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = |compound_condition: expected X but got '{ lv_result }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD equality_condition.
    TYPES:
      BEGIN OF ts_rand_data,
        group TYPE string,
        val   TYPE i,
      END OF ts_rand_data.

    DATA ls_row    TYPE ts_rand_data.
    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA lv_result TYPE abap_bool.

    ls_row-group = 'C'.
    ls_row-val   = 10.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `ROW-GROUP eq 'GRP B'` ).
    lv_result = lo_calc->evaluate_bool( ls_row ).

    IF lv_result <> abap_false.
      zcx_xtt_exception=>raise_sys_error( iv_message = |equality_condition: expected blank but got '{ lv_result }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD raw_dynamic_form.
    TYPES:
      BEGIN OF ts_rand_data,
        group TYPE string,
        val   TYPE i,
      END OF ts_rand_data.

    DATA ls_row    TYPE ts_rand_data.
    DATA lo_calc   TYPE REF TO lcl_expression.
    DATA lv_result TYPE abap_bool.

    ls_row-group = 'C'.
    ls_row-val   = 10.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `( row-GROUP = 'C' OR ROW-group cp '*C' ) AND ROW-GROUP <> 'Z'` ).
    lv_result = lo_calc->evaluate_bool( ls_row ).

    IF lv_result <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = |raw_dynamic_form: expected X but got '{ lv_result }'| ).
    ENDIF.
  ENDMETHOD.

  METHOD nested_parentheses.
    TYPES:
      BEGIN OF ts_row,
        a TYPE i,
        b TYPE i,
      END OF ts_row.

    DATA ls_row  TYPE ts_row.
    DATA lo_calc TYPE REF TO lcl_expression.

    ls_row-a = 1.
    ls_row-b = 2.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `( ( row-a = 1 ) AND ( row-b = 2 ) )` ).
    IF lo_calc->evaluate_bool( ls_row ) <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'nested_parentheses failed for true' ).
    ENDIF.

    lo_calc->compile( `( ( row-a = 2 ) OR ( row-b = 99 ) )` ).
    IF lo_calc->evaluate_bool( ls_row ) <> abap_false.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'nested_parentheses failed for false' ).
    ENDIF.
  ENDMETHOD.

  METHOD numeric_comparisons.
    TYPES:
      BEGIN OF ts_row,
        val TYPE p LENGTH 8 DECIMALS 2,
      END OF ts_row.

    DATA ls_row  TYPE ts_row.
    DATA lo_calc TYPE REF TO lcl_expression.

    ls_row-val = '15.50'.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `row-val > 10 AND row-val <= 20` ).
    IF lo_calc->evaluate_bool( ls_row ) <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'numeric_comparisons failed' ).
    ENDIF.

    lo_calc->compile( `row-val >= 20` ).
    IF lo_calc->evaluate_bool( ls_row ) <> abap_false.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'numeric_comparisons >= failed' ).
    ENDIF.
  ENDMETHOD.

  METHOD is_initial_test.
    TYPES:
      BEGIN OF ts_row,
        text TYPE string,
        num  TYPE i,
      END OF ts_row.

    DATA ls_row  TYPE ts_row.
    DATA lo_calc TYPE REF TO lcl_expression.

    ls_row-text = ''.
    ls_row-num  = 5.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `row-text IS INITIAL AND row-num IS NOT INITIAL` ).
    IF lo_calc->evaluate_bool( ls_row ) <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'is_initial_test failed' ).
    ENDIF.
  ENDMETHOD.

  METHOD string_contains_cs_ns.
    TYPES:
      BEGIN OF ts_row,
        name TYPE string,
      END OF ts_row.

    DATA ls_row  TYPE ts_row.
    DATA lo_calc TYPE REF TO lcl_expression.

    ls_row-name = 'Quick Brown Fox'.

    CREATE OBJECT lo_calc.
    lo_calc->compile( `row-name CS 'brown' AND row-name NS 'cat'` ).
    IF lo_calc->evaluate_bool( ls_row ) <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'string_contains_cs_ns failed' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS zcl_xtt_cond DEFINITION LOCAL FRIENDS lcl_test.
CLASS lcl_test IMPLEMENTATION.
  METHOD generate.
*    DATA cut TYPE REF TO zcl_xtt_cond.
*    zcl_xtt_cond=>get_instance( EXPORTING iv_id       = 'R'
*                                IMPORTING eo_instance = cut ).
*    TYPES:
*      BEGIN OF ts_nested,
*        i TYPE i,
*        p TYPE bf_rbetr,
*        n TYPE n LENGTH 16,
*      END OF ts_nested,
*
*      BEGIN OF ts_root,
*        " Simple data
*        group  TYPE string,
*        date   TYPE d,
*        time   TYPE t,
*        sum1   TYPE bf_rbetr,
*        " Nested stucrure IN SE11
*        sy     TYPE syst,
*        nested TYPE ts_nested,
*        " 7.40 tab0   TYPE STANDARD TABLE OF ts_nested WITH EMPTY KEY,
*        tab1   TYPE STANDARD TABLE OF ts_nested WITH DEFAULT KEY,
*        tab2   TYPE SORTED   TABLE OF ts_nested WITH NON-UNIQUE KEY i n,
*        tab3   TYPE HASHED   TABLE OF ts_nested WITH UNIQUE KEY i n,
*      END OF ts_root.
*    DATA ls_root TYPE ts_root.
*
*    cut->get_type( EXPORTING is_data = ls_root
*                   IMPORTING ev_type = cut->mv_root_type ).
*
*    cut->_make_cond_forms( ). " zcx_xtt_exception
  ENDMETHOD.

  METHOD _702_cond.
*    DATA cut TYPE REF TO zcl_xtt_cond.
*    zcl_xtt_cond=>get_instance( EXPORTING iv_id       = 'R'
*                                IMPORTING eo_instance = cut ).
*
*    DATA lv_code TYPE STRING.
*    lv_code = cut->_702_cond(        `WHEN sy-datum(4) < '2020' THEN 28284 WHEN sy-datum(4) = '2020' THEN 42500 WHEN sy-datum(4) > '2020' THEN 42500 * '1.1' ELSE 0` ).
*    zcl_eui_conv=>assert_equals( exp = `IF sy-datum(4) < '2020' . result =  28284 .ELSEIF sy-datum(4) = '2020' . result =  42500 .ELSEIF sy-datum(4) > '2020' . result =  42500 * '1.1' . ELSE. result =  0.ENDIF.`
*                                 act = lv_code ).
  ENDMETHOD.

  METHOD _702_concat.
*    DATA cut TYPE REF TO zcl_xtt_cond.
*    zcl_xtt_cond=>get_instance( EXPORTING iv_id       = 'R'
*                                IMPORTING eo_instance = cut ).
*
*    DATA lv_code TYPE STRING.
*    lv_code = cut->_702_concat( `'Test' && 'Text'` ).
*
*    zcl_eui_conv=>assert_equals( act = lv_code
*                                 exp = `CONCATENATE 'Test'  'Text' INTO result.` ).
  ENDMETHOD.
ENDCLASS.

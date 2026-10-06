"*" use this source file for the definition and implementation of
"*" local helper classes, interface definitions and type declarations

CLASS lcl_test  DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      generate    FOR TESTING RAISING zcx_xtt_exception,
      block_result FOR TESTING RAISING zcx_xtt_exception,
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
      concat_values         FOR TESTING RAISING zcx_xtt_exception,
      country_date_values   FOR TESTING RAISING zcx_xtt_exception,
      country_rejects_text  FOR TESTING RAISING zcx_xtt_exception,
      country_unknown       FOR TESTING RAISING zcx_xtt_exception,
      failed_recompile      FOR TESTING RAISING zcx_xtt_exception,
      strlen_function       FOR TESTING RAISING zcx_xtt_exception,
      demo_condense         FOR TESTING RAISING zcx_xtt_exception,
      demo_line_exists      FOR TESTING RAISING zcx_xtt_exception,
      demo_reduce           FOR TESTING RAISING zcx_xtt_exception,
      demo_user_formats     FOR TESTING RAISING zcx_xtt_exception,
      demo_invalid_operands FOR TESTING RAISING zcx_xtt_exception,
      date_difference       FOR TESTING RAISING zcx_xtt_exception,
      substring_offset_len  FOR TESTING RAISING zcx_xtt_exception.
ENDCLASS.

CLASS lcl_expression_boolean_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                                   .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      compound_condition FOR TESTING RAISING zcx_xtt_exception,
      boolean_results    FOR TESTING RAISING zcx_xtt_exception,
      equality_condition FOR TESTING RAISING zcx_xtt_exception,
      raw_dynamic_form   FOR TESTING RAISING zcx_xtt_exception,
      nested_parentheses    FOR TESTING RAISING zcx_xtt_exception, " NEW
      numeric_comparisons   FOR TESTING RAISING zcx_xtt_exception, " NEW
      is_initial_test       FOR TESTING RAISING zcx_xtt_exception, " NEW
      string_contains_cs_ns FOR TESTING RAISING zcx_xtt_exception, " NEW
      system_field_len_only FOR TESTING RAISING zcx_xtt_exception.
ENDCLASS.

CLASS lcl_call_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                    .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      fullname              FOR TESTING RAISING zcx_xtt_exception,
      date_default_language FOR TESTING RAISING zcx_xtt_exception,
      date_explicit_language FOR TESTING RAISING zcx_xtt_exception,
      cond_fullname         FOR TESTING RAISING zcx_xtt_exception,
      actual_month_names    FOR TESTING RAISING zcx_xtt_exception,
      month_names_layouts   FOR TESTING.

  PRIVATE SECTION.
    METHODS _assert_call
      IMPORTING
        iv_call       TYPE string
        iv_expected   TYPE string
        iv_rows       TYPE abap_bool DEFAULT abap_false
        iv_lang       TYPE sylangu DEFAULT sy-langu
        iv_real_months TYPE abap_bool DEFAULT abap_false
        iv_month_text TYPE string DEFAULT 'March'
      RAISING zcx_xtt_exception.
    METHODS _prepare
      IMPORTING
        iv_lang       TYPE sylangu DEFAULT sy-langu
        iv_real_months TYPE abap_bool DEFAULT abap_false
        iv_month_text TYPE string DEFAULT 'March'
      EXPORTING
        es_root       TYPE zcl_xtt_demo_160=>ts_root
        eo_caller     TYPE REF TO zcl_xtt_demo_160.
ENDCLASS.

CLASS zcl_xtt_cond DEFINITION LOCAL FRIENDS lcl_test.

**********************************************************************
**********************************************************************

CLASS lcl_call_test IMPLEMENTATION.
  METHOD cond_fullname.
    DATA ls_root TYPE zcl_xtt_demo_160=>ts_root.
    DATA lo_expression TYPE REF TO lcl_expression.
    _prepare( IMPORTING es_root = ls_root ).
    CREATE OBJECT lo_expression.
    lo_expression->compile( 'to_upper( value-FIRST_NAME && ` ` && value-LAST_NAME && ` ` && value-MIDDLE_NAME )' ).
    zcl_eui_conv=>assert_equals(
      exp = 'FIRSTNAME LASTNAME MIDDLENAME'
      act = lo_expression->evaluate( ls_root ) ).
  ENDMETHOD.

  METHOD actual_month_names.
    _assert_call(
      iv_real_months = abap_true
      iv_rows = abap_true
      iv_call = `date_text( iv_date = value-FLDATE iv_lang = 'E' )`
      iv_expected = `2026-March-08|2026-March-09` ).
    _assert_call(
      iv_real_months = abap_true
      iv_rows = abap_true
      iv_call = `date_text( iv_date = value-FLDATE iv_lang = 'D' )`
      iv_expected = `2026-März-08|2026-März-09` ).
  ENDMETHOD.

  METHOD month_names_layouts.
    DATA lt_names TYPE wdr_date_nav_month_name_tab.
    DATA ls_name LIKE LINE OF lt_names.
    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING language = 'E'
      TABLES month_names = lt_names.
    zcl_eui_conv=>assert_equals( exp = 12 act = lines( lt_names ) ).
    READ TABLE lt_names INTO ls_name INDEX 3.
    zcl_eui_conv=>assert_equals( exp = '03' act = ls_name-mnr ).
    zcl_eui_conv=>assert_equals( exp = 'March' act = ls_name-ltx ).

    DATA lt_sap_names TYPE STANDARD TABLE OF t247.
    DATA ls_sap_name TYPE t247.
    CALL FUNCTION 'MONTH_NAMES_GET'
      TABLES month_names = lt_sap_names.
    READ TABLE lt_sap_names INTO ls_sap_name INDEX 3.
    zcl_eui_conv=>assert_equals( exp = sy-langu act = ls_sap_name-spras ).
    zcl_eui_conv=>assert_equals( exp = '03' act = ls_sap_name-mnr ).
  ENDMETHOD.

  METHOD fullname.
    " IS_ROOT is passed implicitly; RV_TEXT becomes the replacement value.
    _assert_call(
      iv_call = `get_fullname( )`
      iv_expected = `FIRSTNAME LASTNAME MIDDLENAME` ).
    _assert_call(
      iv_call = `get_fullname()`
      iv_expected = `FIRSTNAME LASTNAME MIDDLENAME` ).
  ENDMETHOD.

  METHOD date_default_language.
    " Resolve value-FLDATE for each row and leave IV_LANG at its method default.
    _assert_call(
      iv_rows = abap_true
      iv_call = `date_text( iv_date = value-FLDATE )`
      iv_expected = `2026-March-08|2026-March-09` ).
  ENDMETHOD.

  METHOD date_explicit_language.
    " Pass both a row field and a literal to a method without IS_ROOT.
    _assert_call(
      iv_rows       = abap_true
      iv_call       = `date_text( iv_date = value-FLDATE iv_lang = 'D' )`
      iv_expected   = `2026-Maerz-08|2026-Maerz-09`
      iv_lang       = 'D'
      iv_month_text = 'Maerz' ).
  ENDMETHOD.

  METHOD _prepare.
    CLEAR es_root.
    es_root-first_name  = 'FirstName'.
    es_root-last_name   = 'LastName'.
    es_root-middle_name = 'MiddleName'.

    DATA ls_flight LIKE LINE OF es_root-t.
    ls_flight-fldate = '20260308'.
    APPEND ls_flight TO es_root-t.
    ls_flight-fldate = '20260309'.
    APPEND ls_flight TO es_root-t.

    CREATE OBJECT eo_caller.

    " Seed the caller's month names without depending on database contents.
    DATA ls_month TYPE t247.
    ls_month-spras = iv_lang.
    ls_month-mnr   = '03'.
    ls_month-ltx   = iv_month_text.
    APPEND ls_month TO eo_caller->mt_month_name.
    IF iv_real_months = abap_true.
      CLEAR eo_caller->mt_month_name.
      DATA lt_months LIKE eo_caller->mt_month_name.
      CALL FUNCTION 'MONTH_NAMES_GET'
        EXPORTING language = 'E'
        TABLES month_names = eo_caller->mt_month_name.
      CALL FUNCTION 'MONTH_NAMES_GET'
        EXPORTING language = 'D'
        TABLES month_names = lt_months.
      APPEND LINES OF lt_months TO eo_caller->mt_month_name.
      SORT eo_caller->mt_month_name BY spras mnr.
    ENDIF.
  ENDMETHOD.

  METHOD _assert_call.
    DATA ls_root TYPE zcl_xtt_demo_160=>ts_root.
    DATA lo_caller TYPE REF TO zcl_xtt_demo_160.
    _prepare( EXPORTING iv_lang = iv_lang iv_real_months = iv_real_months iv_month_text = iv_month_text
              IMPORTING es_root = ls_root eo_caller = lo_caller ).
    DATA lo_expression TYPE REF TO lcl_expression.
    CREATE OBJECT lo_expression.
    lo_expression->compile_call( iv_call = iv_call io_caller = lo_caller ).

    DATA lv_result TYPE string.
    DATA lv_value TYPE string.
    DATA ls_flight LIKE LINE OF ls_root-t.
    IF iv_rows = abap_true.
      " Reuse the compiled call with a different row context.
      LOOP AT ls_root-t INTO ls_flight.
        lv_value = lo_expression->evaluate( ls_flight ).
        IF lv_result IS NOT INITIAL.
          CONCATENATE lv_result '|' INTO lv_result.
        ENDIF.
        CONCATENATE lv_result lv_value INTO lv_result.
      ENDLOOP.
    ELSE.
      lv_result = lo_expression->evaluate( ls_root ).
    ENDIF.
    zcl_eui_conv=>assert_equals( exp = iv_expected act = lv_result ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_expression_text_test IMPLEMENTATION.
  METHOD demo_condense.
    DATA: BEGIN OF ls_root,
            caption TYPE string VALUE '  First   caption  ',
          END OF ls_root.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    lo_calc->compile( 'condense( value-CAPTION )' ).
    zcl_eui_conv=>assert_equals( exp = 'First caption' act = lo_calc->evaluate( ls_root ) ).
    CLEAR ls_root-caption.
    zcl_eui_conv=>assert_equals( exp = '' act = lo_calc->evaluate( ls_root ) ).
  ENDMETHOD.

  METHOD demo_line_exists.
    TYPES: BEGIN OF ts_line,
             group TYPE string,
             caption TYPE string,
           END OF ts_line.
    DATA: BEGIN OF ls_root,
            t TYPE STANDARD TABLE OF ts_line WITH DEFAULT KEY,
          END OF ls_root.
    DATA ls_line TYPE ts_line.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    ls_line-group = 'GRP A'.
    ls_line-caption = 'First'.
    APPEND ls_line TO ls_root-t.
    lo_calc->compile( `WHEN line_exists( value-t[ group = 'GRP A' ] ) THEN |First caption in group 'A' { value-t[ group = 'GRP A' ]-caption }|` ).
    zcl_eui_conv=>assert_equals( exp = `First caption in group 'A' First` act = lo_calc->evaluate( ls_root ) ).
    CLEAR ls_root-t.
    zcl_eui_conv=>assert_equals( exp = '' act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( 'line_exists( value-t[ 1 ] )' ).
    zcl_eui_conv=>assert_equals( exp = abap_false act = lo_calc->evaluate( ls_root ) ).
    APPEND ls_line TO ls_root-t.
    zcl_eui_conv=>assert_equals( exp = abap_true act = lo_calc->evaluate( ls_root ) ).
  ENDMETHOD.

  METHOD demo_reduce.
    TYPES: BEGIN OF ts_line,
             sum1 TYPE decfloat34,
             sum2 TYPE decfloat34,
           END OF ts_line.
    DATA: BEGIN OF ls_root,
            t TYPE STANDARD TABLE OF ts_line WITH DEFAULT KEY,
          END OF ls_root.
    DATA ls_line TYPE ts_line.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    ls_line-sum1 = '10.5'.
    ls_line-sum2 = '-2'.
    APPEND ls_line TO ls_root-t.
    ls_line-sum1 = '-3'.
    ls_line-sum2 = '8.25'.
    APPEND ls_line TO ls_root-t.
    lo_calc->compile( 'REDUCE decfloat34( INIT s TYPE decfloat34 FOR ls_line IN value-t[] NEXT s = s + ls_line-SUM1 )' ).
    zcl_eui_conv=>assert_equals( exp = '7.5' act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( 'REDUCE decfloat34( INIT s TYPE decfloat34 FOR ls_line IN value-t[] NEXT s = s + ls_line-SUM2 )' ).
    zcl_eui_conv=>assert_equals( exp = '6.25' act = lo_calc->evaluate( ls_root ) ).
    CLEAR ls_root-t.
    zcl_eui_conv=>assert_equals( exp = '0' act = lo_calc->evaluate( ls_root ) ).
  ENDMETHOD.

  METHOD demo_user_formats.
    DATA: BEGIN OF ls_root,
            gbdat TYPE d VALUE '20260301',
            sum1 TYPE decfloat34 VALUE '42500',
            sum2 TYPE decfloat34 VALUE '1.25',
          END OF ls_root.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_expected TYPE string.
    DATA lv_number TYPE decfloat34.
    CREATE OBJECT lo_calc.
    lo_calc->compile( '|{ value-GBDAT DATE = USER }|' ).
    lv_expected = |{ ls_root-gbdat DATE = USER }|.
    zcl_eui_conv=>assert_equals( exp = lv_expected act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( '|{ 42500 NUMBER = USER }|' ).
    lv_number = 42500.
    lv_expected = |{ lv_number NUMBER = USER }|.
    zcl_eui_conv=>assert_equals( exp = lv_expected act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( '|{ value-SUM1 - value-SUM2 NUMBER = USER }|' ).
    lv_number = ls_root-sum1 - ls_root-sum2.
    lv_expected = |{ lv_number NUMBER = USER }|.
    zcl_eui_conv=>assert_equals( exp = lv_expected act = lo_calc->evaluate( ls_root ) ).
  ENDMETHOD.

  METHOD date_difference.
    DATA: BEGIN OF ls_root,
            date1 TYPE d VALUE '20260301',
            date2 TYPE d VALUE '20260228',
          END OF ls_root.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    lo_calc->compile( 'value-DATE1 - value-DATE2' ).
    zcl_eui_conv=>assert_equals( exp = '1' act = lo_calc->evaluate( ls_root ) ).
    ls_root-date1 = '20240228'.
    ls_root-date2 = '20240301'.
    zcl_eui_conv=>assert_equals( exp = '-2' act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( '20260301 - 20260228' ).
    zcl_eui_conv=>assert_equals( exp = '73' act = lo_calc->evaluate( ls_root ) ).
  ENDMETHOD.

  METHOD demo_invalid_operands.
    DATA: BEGIN OF ls_root,
            text TYPE string VALUE '20260301',
          END OF ls_root.
    DATA lt_expr TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_expr TYPE string.
    DATA lv_failed TYPE abap_bool.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    APPEND 'line_exists( value-missing[ 1 ] )' TO lt_expr.
    APPEND '|{ value-text DATE = USER }|' TO lt_expr.
    APPEND '|{ value-text NUMBER = USER }|' TO lt_expr.
    APPEND 'REDUCE string( INIT s TYPE string FOR row IN value-t[] NEXT s = s )' TO lt_expr.
    LOOP AT lt_expr INTO lv_expr.
      CLEAR lv_failed.
      TRY.
          lo_calc->compile( lv_expr ).
          lo_calc->evaluate( ls_root ).
        CATCH zcx_xtt_exception.
          lv_failed = abap_true.
      ENDTRY.
      zcl_eui_conv=>assert_equals( exp = abap_true act = lv_failed ).
    ENDLOOP.
  ENDMETHOD.

  METHOD strlen_function.
    DATA: BEGIN OF ls_root,
            title TYPE string VALUE 'Document title',
          END OF ls_root.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    lo_calc->compile( 'strlen( value-TITLE ) eq -1' ).
    zcl_eui_conv=>assert_equals( exp = abap_false act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( 'strlen( value-TITLE ) gt 0' ).
    zcl_eui_conv=>assert_equals( exp = abap_true act = lo_calc->evaluate( ls_root ) ).
    CLEAR ls_root-title.
    zcl_eui_conv=>assert_equals( exp = abap_false act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( 'strlen( `a  ` ) + strlen( `` )' ).
    zcl_eui_conv=>assert_equals( exp = '3' act = lo_calc->evaluate( ls_root ) ).
    lo_calc->compile( 'strlen( `1234567890` ) > strlen( `ab` )' ).
    zcl_eui_conv=>assert_equals( exp = abap_true act = lo_calc->evaluate( ls_root ) ).
  ENDMETHOD.

  METHOD failed_recompile.
    DATA lo_expression TYPE REF TO lcl_expression.
    DATA lo_caller TYPE REF TO zcl_xtt_demo_160.
    DATA lv_compile_failed TYPE abap_bool.
    DATA lv_evaluate_failed TYPE abap_bool.
    CREATE OBJECT lo_expression.
    CREATE OBJECT lo_caller.
    DO 2 TIMES.
      lo_expression->compile( '`old`' ).
      CLEAR: lv_compile_failed, lv_evaluate_failed.
      TRY.
          IF sy-index = 1.
            lo_expression->compile( '1 +' ).
          ELSE.
            lo_expression->compile_call( iv_call = 'get_fullname(' io_caller = lo_caller ).
          ENDIF.
        CATCH zcx_xtt_exception.
          lv_compile_failed = abap_true.
      ENDTRY.
      TRY.
          lo_expression->evaluate( sy ).
        CATCH zcx_xtt_exception.
          lv_evaluate_failed = abap_true.
      ENDTRY.
      zcl_eui_conv=>assert_equals( exp = abap_true act = lv_compile_failed ).
      zcl_eui_conv=>assert_equals( exp = abap_true act = lv_evaluate_failed ).
    ENDDO.
    lo_expression->compile( '`new`' ).
    zcl_eui_conv=>assert_equals( exp = 'new' act = lo_expression->evaluate( sy ) ).
  ENDMETHOD.

  METHOD concat_values.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    lo_calc->compile( 'to_upper( `a` && ` ` && ( `b` && `c` ) )' ).
    zcl_eui_conv=>assert_equals( exp = 'A BC' act = lo_calc->evaluate( sy ) ).
    lo_calc->compile( '`sum=` && 1 + 2 * 3' ).
    zcl_eui_conv=>assert_equals( exp = 'sum=7' act = lo_calc->evaluate( sy ) ).
    lo_calc->compile( '`a` && `b` = `ab`' ).
    zcl_eui_conv=>assert_equals( exp = abap_true act = lo_calc->evaluate( sy ) ).
  ENDMETHOD.

  METHOD country_date_values.
    DATA: BEGIN OF ls_row,
            fldate TYPE d,
            country TYPE land1,
          END OF ls_row.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    ls_row-fldate = '20260322'.
    ls_row-country = 'US'.
    lo_calc->compile( `|Date: { value-FLDATE COUNTRY = value-COUNTRY }|` ).
    zcl_eui_conv=>assert_equals( exp = 'Date: 03/22/2026' act = lo_calc->evaluate( ls_row ) ).
    ls_row-country = 'RU'.
    zcl_eui_conv=>assert_equals( exp = 'Date: 22.03.2026' act = lo_calc->evaluate( ls_row ) ).
    ls_row-country = 'DE'.
    ls_row-fldate = '20240229'.
    zcl_eui_conv=>assert_equals( exp = 'Date: 29.02.2024' act = lo_calc->evaluate( ls_row ) ).
    CLEAR ls_row-fldate.
    zcl_eui_conv=>assert_equals( exp = 'Date: 00.00.0000' act = lo_calc->evaluate( ls_row ) ).
  ENDMETHOD.

  METHOD country_rejects_text.
    DATA: BEGIN OF ls_row,
            fldate TYPE string VALUE '20260322',
          END OF ls_row.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_rejected TYPE abap_bool.
    CREATE OBJECT lo_calc.
    lo_calc->compile( `|{ value-FLDATE COUNTRY = 'US ' }|` ).
    TRY.
        lo_calc->evaluate( ls_row ).
      CATCH zcx_xtt_exception.
        lv_rejected = abap_true.
    ENDTRY.
    zcl_eui_conv=>assert_equals( exp = abap_true act = lv_rejected ).
  ENDMETHOD.

  METHOD country_unknown.
    DATA: BEGIN OF ls_row,
            fldate TYPE d VALUE '20260322',
          END OF ls_row.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_rejected TYPE abap_bool.
    CREATE OBJECT lo_calc.
    lo_calc->compile( `|{ value-FLDATE COUNTRY = 'ZZZ' }|` ).
    TRY.
        lo_calc->evaluate( ls_row ).
      CATCH zcx_xtt_exception.
        lv_rejected = abap_true.
    ENDTRY.
    zcl_eui_conv=>assert_equals( exp = abap_true act = lv_rejected ).
  ENDMETHOD.

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
  METHOD boolean_results.
    DATA lt_false TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_expr TYPE string.
    DATA lv_boolean TYPE abap_bool.
    DATA lo_calc TYPE REF TO lcl_expression.
    CREATE OBJECT lo_calc.
    APPEND 'abap_false' TO lt_false.
    APPEND '``' TO lt_false.
    APPEND '` `' TO lt_false.
    APPEND '`  `' TO lt_false.
    APPEND '`0`' TO lt_false.
    APPEND '`XX`' TO lt_false.
    LOOP AT lt_false INTO lv_expr.
      lo_calc->compile( lv_expr ).
      lv_boolean = abap_false.
      IF lo_calc->evaluate( sy ) = abap_true.
        lv_boolean = abap_true.
      ENDIF.
      zcl_eui_conv=>assert_equals( exp = abap_false
        act = lv_boolean ).
      lo_calc->compile( |{ lv_expr } AND abap_true| ).
      zcl_eui_conv=>assert_equals( exp = abap_false
        act = lo_calc->evaluate( sy ) ).
      lo_calc->compile( |abap_true AND { lv_expr }| ).
      zcl_eui_conv=>assert_equals( exp = abap_false
        act = lo_calc->evaluate( sy ) ).
      lo_calc->compile( |abap_false OR { lv_expr }| ).
      zcl_eui_conv=>assert_equals( exp = abap_false
        act = lo_calc->evaluate( sy ) ).
      lo_calc->compile( |{ lv_expr } OR abap_true| ).
      zcl_eui_conv=>assert_equals( exp = abap_true
        act = lo_calc->evaluate( sy ) ).
      lo_calc->compile( |NOT { lv_expr }| ).
      zcl_eui_conv=>assert_equals( exp = abap_true
        act = lo_calc->evaluate( sy ) ).
    ENDLOOP.
    lo_calc->compile( 'abap_true' ).
    zcl_eui_conv=>assert_equals( exp = abap_true
      act = lo_calc->evaluate( sy ) ).
  ENDMETHOD.

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
    lv_result = lo_calc->evaluate( ls_row ).

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
    lv_result = lo_calc->evaluate( ls_row ).

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
    lv_result = lo_calc->evaluate( ls_row ).

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
    IF lo_calc->evaluate( ls_row ) <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'nested_parentheses failed for true' ).
    ENDIF.

    lo_calc->compile( `( ( row-a = 2 ) OR ( row-b = 99 ) )` ).
    IF lo_calc->evaluate( ls_row ) = abap_true.
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
    IF lo_calc->evaluate( ls_row ) <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'numeric_comparisons failed' ).
    ENDIF.

    lo_calc->compile( `row-val >= 20` ).
    IF lo_calc->evaluate( ls_row ) = abap_true.
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
    IF lo_calc->evaluate( ls_row ) <> abap_true.
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
    IF lo_calc->evaluate( ls_row ) <> abap_true.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'string_contains_cs_ns failed' ).
    ENDIF.
  ENDMETHOD.

  METHOD system_field_len_only.
    DATA lo_calc TYPE REF TO lcl_expression.
    DATA lv_current_year TYPE string.
    CREATE OBJECT lo_calc.

    lv_current_year = sy-datum(4).

    " Test ABAP shorthand length without offset: sy-datum(4)
    lo_calc->compile( |sy-datum(4) eq '{ lv_current_year }'| ).
    zcl_eui_conv=>assert_equals(
      exp = abap_true
      act = lo_calc->evaluate( sy ) ).

    lo_calc->compile( 'sy-datum+4(2)' ).
    zcl_eui_conv=>assert_equals( exp = sy-datum+4(2) act = lo_calc->evaluate( sy ) ).

    " Test with field from structure
    TYPES: BEGIN OF ts_test,
             datum TYPE d,
           END OF ts_test.
    DATA ls_test TYPE ts_test.
    ls_test-datum = '20191231'.

    lo_calc->compile( `value-datum(4) eq '2019'` ).
    zcl_eui_conv=>assert_equals(
      exp = abap_true
      act = lo_calc->evaluate( ls_test ) ).

    " Numeric function arguments are not substring lengths.
    lo_calc->compile( `to_upper(123)` ).
    zcl_eui_conv=>assert_equals( exp = '123' act = lo_calc->evaluate( ls_test ) ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_test IMPLEMENTATION.
  METHOD block_result.
    DATA lo_xtt TYPE REF TO zcl_xtt.
    DATA lo_cond TYPE REF TO zcl_xtt_cond.
    DATA lo_block TYPE REF TO zcl_xtt_replace_block.
    DATA lo_expression TYPE REF TO lcl_expression.
    DATA ls_match TYPE zcl_xtt_cond=>ts_match.
    DATA ls_field TYPE zcl_xtt_replace_block=>ts_field.
    DATA lv_root TYPE string VALUE 'test'.
    FIELD-SYMBOLS <lt_result> TYPE STANDARD TABLE.
    CREATE OBJECT lo_cond EXPORTING io_xtt = lo_xtt.
    CREATE OBJECT lo_block EXPORTING io_xtt = lo_xtt is_block = lv_root iv_block_name = 'R'.
    CREATE OBJECT lo_expression.
    lo_expression->compile( 'abap_true' ).
    ls_match-cid = 'VISIBLE'.
    ls_match-type = zcl_xtt_replace_block=>mc_type-block.
    ls_match-o_expr = lo_expression.
    INSERT ls_match INTO TABLE lo_cond->mt_match.
    CREATE OBJECT lo_expression.
    lo_expression->compile( 'abap_false' ).
    ls_match-cid = 'HIDDEN'.
    ls_match-o_expr = lo_expression.
    INSERT ls_match INTO TABLE lo_cond->mt_match.

    lo_cond->calc_matches( io_xtt = lo_xtt iv_tabix = 1 io_block = lo_block ).
    READ TABLE lo_block->mt_fields INTO ls_field WITH TABLE KEY name = 'VISIBLE'.
    zcl_eui_conv=>assert_equals( exp = 0 act = sy-subrc ).
    ASSIGN ls_field-dref->* TO <lt_result>.
    zcl_eui_conv=>assert_equals( exp = 1 act = lines( <lt_result> ) ).
    READ TABLE lo_block->mt_fields INTO ls_field WITH TABLE KEY name = 'HIDDEN'.
    zcl_eui_conv=>assert_equals( exp = 0 act = sy-subrc ).
    ASSIGN ls_field-dref->* TO <lt_result>.
    zcl_eui_conv=>assert_equals( exp = 0 act = lines( <lt_result> ) ).
  ENDMETHOD.

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

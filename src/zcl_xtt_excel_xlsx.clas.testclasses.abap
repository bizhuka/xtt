*"* use this source file for your ABAP unit test classes

CLASS lcl_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      basic_no_shift FOR TESTING,
      shift_right_down FOR TESTING,
      shift_left_up FOR TESTING,
      absolute_reference FOR TESTING,
      mixed_absolute_range FOR TESTING,
      range_name FOR TESTING,
      string_literal FOR TESTING,
      external_sheet FOR TESTING,
      preserve_blanks FOR TESTING,
      structured_reference FOR TESTING,
      xml_encoded_concat FOR TESTING,
      underscore_names FOR TESTING,
      indirect_r1c1_xml FOR TESTING,
      nested_functions FOR TESTING,
      function_no_args FOR TESTING,
      quoted_cell FOR TESTING,
      empty_formula FOR TESTING,
      column_underflow FOR TESTING,
      row_underflow FOR TESTING,
      row_column_underflow FOR TESTING,
      sheet_reference FOR TESTING,
      numbered_sheet FOR TESTING,
      quoted_sheet FOR TESTING,
      structured_subtotal FOR TESTING,
      names_in_sum FOR TESTING,
      indirect_r1c1 FOR TESTING,
      quoted_cell_sheet FOR TESTING,
      table_this_row FOR TESTING,
      table_and_cell FOR TESTING,
      whole_column_shift FOR TESTING,
      whole_column_row_shift FOR TESTING,
      absolute_whole_column FOR TESTING,
      whole_column_range FOR TESTING,
      whole_row_shift FOR TESTING,
      whole_row_column_shift FOR TESTING,
      whole_column_sumif FOR TESTING,
      sheet_whole_column FOR TESTING,
      range_name_and_cell FOR TESTING.

  PRIVATE SECTION.
    METHODS _assert_formula_shift
      IMPORTING
        iv_reference_formula TYPE string
        iv_shift_cols        TYPE i
        iv_shift_rows        TYPE i
        iv_expected          TYPE string.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_test IMPLEMENTATION.
  METHOD _assert_formula_shift.
    DATA lv_resulting_formula TYPE string.
    DATA lv_message TYPE string.
    DATA lv_exception TYPE string.

    TRY.
        lv_resulting_formula = zcl_xtt_excel_xlsx=>formula_shift(
          iv_reference_formula = iv_reference_formula
          iv_shift_cols        = iv_shift_cols
          iv_shift_rows        = iv_shift_rows ).
      CATCH cx_root INTO DATA(lo_error).
        lv_exception = lo_error->get_text( ).
    ENDTRY.

    IF lv_exception IS INITIAL.
      lv_message = |Formula shift failed: { iv_reference_formula }| &&
                   cl_abap_char_utilities=>newline &&
                   |expected: { iv_expected }| &&
                   cl_abap_char_utilities=>newline &&
                   |actual: { lv_resulting_formula }|.
    ELSE.
      lv_message = |Formula shift failed: { iv_reference_formula }| &&
                   cl_abap_char_utilities=>newline &&
                   |expected: { iv_expected }| &&
                   cl_abap_char_utilities=>newline &&
                   |actual: <no value returned>| &&
                   cl_abap_char_utilities=>newline &&
                   |exception: { lv_exception }|.
    ENDIF.
    zcl_eui_conv=>assert_equals( act   = lv_resulting_formula
                                 exp   = iv_expected
                                 msg   = lv_message
                                 quit  = 0  " continue tests
                                 level = if_aunit_constants=>critical ).
  ENDMETHOD.

  METHOD basic_no_shift.
    _assert_formula_shift( iv_reference_formula = 'C17'
                          iv_shift_cols = 0 iv_shift_rows = 0 iv_expected = 'C17' ).
  ENDMETHOD.

  METHOD shift_right_down.
    _assert_formula_shift( iv_reference_formula = 'C17'
                          iv_shift_cols = 2 iv_shift_rows = 3 iv_expected = 'E20' ).
  ENDMETHOD.

  METHOD shift_left_up.
    _assert_formula_shift( iv_reference_formula = 'C17'
                          iv_shift_cols = -2 iv_shift_rows = -3 iv_expected = 'A14' ).
  ENDMETHOD.

  METHOD absolute_reference.
    _assert_formula_shift( iv_reference_formula = '$C$17'
                          iv_shift_cols = 1 iv_shift_rows = 1 iv_expected = '$C$17' ).
  ENDMETHOD.

  METHOD mixed_absolute_range.
    _assert_formula_shift( iv_reference_formula = 'SUM($C17:C$23)+C30'
                          iv_shift_cols = 1 iv_shift_rows = 11
                          iv_expected = 'SUM($C28:D$23)+D41' ).
  ENDMETHOD.

  METHOD range_name.
    _assert_formula_shift( iv_reference_formula = 'RNGNAME1+C7'
                          iv_shift_cols = -1 iv_shift_rows = -4
                          iv_expected = 'RNGNAME1+B3' ).
  ENDMETHOD.

  METHOD string_literal.
    _assert_formula_shift( iv_reference_formula = '"Date:"&TEXT(B2)'
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = '"Date:"&TEXT(C3)' ).
  ENDMETHOD.

  METHOD external_sheet.
    _assert_formula_shift( iv_reference_formula = '[TEST6.XLSX]SHEET1!A1'
                          iv_shift_cols = 1 iv_shift_rows = 11
                          iv_expected = '[TEST6.XLSX]SHEET1!B12' ).
  ENDMETHOD.

  METHOD preserve_blanks.
    " Respecting blanks
*    _assert_formula_shift( iv_reference_formula = `X(B13, "KK" )  `
*                          iv_shift_cols = 1 iv_shift_rows = 1
*                          iv_expected = `X(C14,"KK")` ).
    _assert_formula_shift( iv_reference_formula = `X(B13, "KK" )  `
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = `X(C14, "KK" )  ` ).
  ENDMETHOD.

  METHOD structured_reference.
    _assert_formula_shift( iv_reference_formula = `SUBTOTAL(109,Table1[SUM 1])`
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = `SUBTOTAL(109,Table1[SUM 1])` ).
  ENDMETHOD.

  METHOD xml_encoded_concat.
    _assert_formula_shift( iv_reference_formula = `B4 &amp; C4`
                          iv_shift_cols = 0 iv_shift_rows = 1
                          iv_expected = `B5 &amp; C5` ).
  ENDMETHOD.

  METHOD underscore_names.
    " F & RC are not columns
    _assert_formula_shift( iv_reference_formula = `SUM(F_1,F_2)`
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = `SUM(F_1,F_2)` ).
  ENDMETHOD.

  METHOD indirect_r1c1_xml.
    _assert_formula_shift( iv_reference_formula = `INDIRECT(&quot;RC[4]&quot;,FALSE)`
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = `INDIRECT(&quot;RC[4]&quot;,FALSE)` ).
  ENDMETHOD.

  METHOD nested_functions.
*    _assert_formula_shift( iv_reference_formula = `SIN((((((B2))))))`
*                          iv_shift_cols = 1 iv_shift_rows = 1
*                          iv_expected = `SIN((((((C3))))))` ).
*    _assert_formula_shift( iv_reference_formula = `SIN(SIN(SIN(SIN(E22))))`
*                          iv_shift_cols = 0 iv_shift_rows = 1
*                          iv_expected = `SIN(SIN(SIN(SIN(E23))))` ).
    _assert_formula_shift( iv_reference_formula = `SIN(SIN(SIN(SIN(E22))))`
                          iv_shift_cols = 0 iv_shift_rows = 1
                          iv_expected = 'SIN(SIN(SIN(SIN(E23))))' ).
  ENDMETHOD.

  METHOD function_no_args.
    _assert_formula_shift( iv_reference_formula = 'HEUTE()'
                          iv_shift_cols = 2 iv_shift_rows = 5 iv_expected = 'HEUTE()' ).
  ENDMETHOD.

  METHOD quoted_cell.
    _assert_formula_shift( iv_reference_formula = '"B2"'
                          iv_shift_cols = 2 iv_shift_rows = 5 iv_expected = '"B2"' ).
  ENDMETHOD.

  METHOD empty_formula.
    _assert_formula_shift( iv_reference_formula = ''
                          iv_shift_cols = 2 iv_shift_rows = 5 iv_expected = '' ).
  ENDMETHOD.

  METHOD column_underflow.
    _assert_formula_shift( iv_reference_formula = 'A1+$A1+A$1+$A$1+B2'
                          iv_shift_cols = -1 iv_shift_rows = 0
                          iv_expected = '#REF!+$A1+#REF!+$A$1+A2' ).
  ENDMETHOD.

  METHOD row_underflow.
    _assert_formula_shift( iv_reference_formula = 'A1+$A1+A$1+$A$1+B2'
                          iv_shift_cols = 0 iv_shift_rows = -1
                          iv_expected = '#REF!+#REF!+A$1+$A$1+B1' ).
  ENDMETHOD.

  METHOD row_column_underflow.
    _assert_formula_shift( iv_reference_formula = 'A1+$A1+A$1+$A$1+B2'
                          iv_shift_cols = -1 iv_shift_rows = -1
                          iv_expected = '#REF!+#REF!+#REF!+$A$1+A1' ).
  ENDMETHOD.

  METHOD sheet_reference.
    _assert_formula_shift( iv_reference_formula = 'Sheet!A1'
                          iv_shift_cols = 1 iv_shift_rows = 1 iv_expected = 'Sheet!B2' ).
  ENDMETHOD.

  METHOD numbered_sheet.
    _assert_formula_shift( iv_reference_formula = 'Sheet2!A1'
                          iv_shift_cols = 1 iv_shift_rows = 1 iv_expected = 'Sheet2!B2' ).
  ENDMETHOD.

  METHOD quoted_sheet.
    _assert_formula_shift( iv_reference_formula = `'Sheet name'!A1`
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = `'Sheet name'!B2` ).
  ENDMETHOD.

  METHOD structured_subtotal.
    _assert_formula_shift( iv_reference_formula = 'SUBTOTAL(109,Table1[SUM 1])'
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = 'SUBTOTAL(109,Table1[SUM 1])' ).
  ENDMETHOD.

  METHOD names_in_sum.
    _assert_formula_shift( iv_reference_formula = 'SUM(F_1,F_2)'
                          iv_shift_cols = 1 iv_shift_rows = 1 iv_expected = 'SUM(F_1,F_2)' ).
  ENDMETHOD.

  METHOD indirect_r1c1.
    _assert_formula_shift( iv_reference_formula = 'INDIRECT("RC[4]",FALSE)'
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = 'INDIRECT("RC[4]",FALSE)' ).
  ENDMETHOD.

  METHOD quoted_cell_sheet.
    _assert_formula_shift( iv_reference_formula = `'A1'!$A$1`
                          iv_shift_cols = 1 iv_shift_rows = 1 iv_expected = `'A1'!$A$1` ).
  ENDMETHOD.

  METHOD table_this_row.
    _assert_formula_shift( iv_reference_formula = 'Tbl[[#This Row],[Air fare]]'
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = 'Tbl[[#This Row],[Air fare]]' ).
  ENDMETHOD.

  METHOD table_and_cell.
    _assert_formula_shift( iv_reference_formula = 'Tbl[[#This Row],[Air]]+A1'
                          iv_shift_cols = 1 iv_shift_rows = 1
                          iv_expected = 'Tbl[[#This Row],[Air]]+B2' ).
  ENDMETHOD.

  METHOD whole_column_shift.
    _assert_formula_shift( iv_reference_formula = 'SUM(A:A)'
                          iv_shift_cols = 1 iv_shift_rows = 0 iv_expected = 'SUM(B:B)' ).
  ENDMETHOD.

  METHOD whole_column_row_shift.
    _assert_formula_shift( iv_reference_formula = 'SUM(A:A)'
                          iv_shift_cols = 0 iv_shift_rows = 5 iv_expected = 'SUM(A:A)' ).
  ENDMETHOD.

  METHOD absolute_whole_column.
    _assert_formula_shift( iv_reference_formula = 'SUM($A:$A)'
                          iv_shift_cols = 1 iv_shift_rows = 0 iv_expected = 'SUM($A:$A)' ).
  ENDMETHOD.

  METHOD whole_column_range.
    _assert_formula_shift( iv_reference_formula = 'SUM(A:C)'
                          iv_shift_cols = 2 iv_shift_rows = 0 iv_expected = 'SUM(C:E)' ).
  ENDMETHOD.

  METHOD whole_row_shift.
    _assert_formula_shift( iv_reference_formula = 'SUM(1:1)'
                          iv_shift_cols = 0 iv_shift_rows = 1 iv_expected = 'SUM(2:2)' ).
  ENDMETHOD.

  METHOD whole_row_column_shift.
    _assert_formula_shift( iv_reference_formula = 'SUM(1:1)'
                          iv_shift_cols = 3 iv_shift_rows = 0 iv_expected = 'SUM(1:1)' ).
  ENDMETHOD.

  METHOD whole_column_sumif.
    _assert_formula_shift( iv_reference_formula = 'SUMIF($AC:$AC,"<>",Q:Q)'
                          iv_shift_cols = 1 iv_shift_rows = 0
                          iv_expected = 'SUMIF($AC:$AC,"<>",R:R)' ).
  ENDMETHOD.

  METHOD sheet_whole_column.
    _assert_formula_shift( iv_reference_formula = 'Sheet2!A:A'
                          iv_shift_cols = 1 iv_shift_rows = 0 iv_expected = 'Sheet2!B:B' ).
  ENDMETHOD.

  METHOD range_name_and_cell.
    " A colon is what tells a range from a name: no colon, no shift
    _assert_formula_shift( iv_reference_formula = 'RNGNAME1+A1'
                          iv_shift_cols = 1 iv_shift_rows = 0
                          iv_expected = 'RNGNAME1+B1' ).
  ENDMETHOD.
ENDCLASS.

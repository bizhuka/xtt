*"* use this source file for your ABAP unit test classes

CLASS lcl_test  DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                 .           "#AU Duration Short
  PUBLIC SECTION.
    METHODS:
      check_formula_shift FOR TESTING.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_test IMPLEMENTATION.
  METHOD check_formula_shift.
    DATA: lv_resulting_formula TYPE string,
          lv_message           TYPE string,
          lv_counter           TYPE num8.

    DEFINE macro_shift_formula.
      ADD 1 TO lv_counter.
      CLEAR lv_resulting_formula.
      TRY.
          lv_resulting_formula = zcl_xtt_excel_xlsx=>formula_shift( iv_reference_formula = &1
                                                                    iv_shift_cols        = &2
                                                                    iv_shift_rows        = &3 ).
          CONCATENATE 'Wrong result in test'
                      lv_counter
                      'shifting formula '
                      &1
               INTO lv_message SEPARATED BY space.
          zcl_eui_conv=>assert_equals( act   = lv_resulting_formula
                                       exp   = &4
                                       msg   = lv_message
                                       quit  = 0  " continue tests
                                       level = if_aunit_constants=>critical ).
        CATCH zcx_xtt_exception.
          CONCATENATE 'Unexpected exception occurred in test'
                      lv_counter
                      'shifting formula '
                      &1
               INTO lv_message SEPARATED BY space.
          zcl_eui_conv=>assert_equals(  act   = lv_resulting_formula
                                        exp   = &4
                                        msg   = lv_message
                                        quit  = 0  " continue tests
                                        level = if_aunit_constants=>critical ).
 ENDTRY.
    END-OF-DEFINITION.

* Test shifts that should result in a valid output
    macro_shift_formula:
          'C17'                                  0   0       'C17',                       " Very basic check
          'C17'                                  2   3       'E20',                       " Check shift right and down
          'C17'                                 -2  -3       'A14',                       " Check shift left and up
          '$C$17'                                1   1       '$C$17',                     " Fixed columns/rows
          'SUM($C17:C$23)+C30'                   1  11       'SUM($C28:D$23)+D41',        " Operators and Ranges, mixed fixed rows or columns
          'RNGNAME1+C7'                         -1  -4       'RNGNAME1+B3',               " Operators and Rangename
          '"Date:"&TEXT(B2)'                     1   1       '"Date:"&TEXT(C3)',          " String literals and string concatenation
          '[TEST6.XLSX]SHEET1!A1'                1  11       '[TEST6.XLSX]SHEET1!B12',    " External sheet reference

          " Respecting blanks
*          `X(B13, "KK" )  `                      1   1       `X(C14,"KK")`,               " superflous blanks, multi-argument functions, literals in function, unknown functions
          `X(B13, "KK" )  `                      1   1       `X(C14, "KK" )  `,
          `SUBTOTAL(109,Table1[SUM 1])`          1   1       `SUBTOTAL(109,Table1[SUM 1])`,
          `B4 &amp; C4`                          0   1       `B5 &amp; C5`,

          " F & RC are not columns
          `SUM(F_1,F_2)`                         1   1       `SUM(F_1,F_2)`,
          `INDIRECT(&quot;RC[4]&quot;,FALSE)`    1   1       `INDIRECT(&quot;RC[4]&quot;,FALSE)`,

*          `SIN((((((B2))))))`                    1   1       `SIN((((((C3))))))`,        " Deep nesting
*          `SIN(SIN(SIN(SIN(E22))))`              0   1       `SIN(SIN(SIN(SIN(E23))))`,   " Different type of deep nesting
          `SIN(SIN(SIN(SIN(E22))))`              0   1       'SIN(SIN(SIN(SIN(E23))))',   " same as above - but with string input instead of Char-input
          'HEUTE()'                              2   5       'HEUTE()',                   " Functions w/o arguments, No cellreferences
          '"B2"'                                 2   5       '"B2"',                      " No cellreferences
          ''                                     2   5       '',                          " Empty
          'A1+$A1+A$1+$A$1+B2'                  -1   0       '#REF!+$A1+#REF!+$A$1+A2',   " Referencing error , column only    , underflow
          'A1+$A1+A$1+$A$1+B2'                   0  -1       '#REF!+#REF!+A$1+$A$1+B1',   " Referencing error , row only       , underflow
          'A1+$A1+A$1+$A$1+B2'                  -1  -1       '#REF!+#REF!+#REF!+$A$1+A1'. " Referencing error , row and column , underflow
  ENDMETHOD.
ENDCLASS.

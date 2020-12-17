*"* use this source file for your ABAP unit test classes

CLASS lcl_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                .           "#AU Duration Short
  PUBLIC SECTION.
    DATA:
      date TYPE d,
      time TYPE t.
    METHODS:
      format_by_field FOR TESTING,
      format_by_root  FOR TESTING.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD format_by_field.
    DATA lt_field TYPE zcl_xtt_util=>tt_format_field.
    DATA ls_field TYPE zcl_xtt_util=>ts_format_field.

    DATA lr_date TYPE REF TO d.
    CREATE DATA lr_date.
    lr_date->* = '20201106'.

    DATA lr_time TYPE REF TO t.
    CREATE DATA lr_time.
    lr_time->* = '152635'.

    ls_field-name = 'DATE'.
    ls_field-ref  = lr_date.
    INSERT ls_field INTO TABLE lt_field.

    ls_field-name = 'TIME'.
    ls_field-ref  = lr_time.
    INSERT ls_field INTO TABLE lt_field.

    DATA lv_text TYPE string.
    lv_text = zcl_xtt_html=>format( iv_template     = `AA {R-DATE} BB {R-TIME}`
                                    " New syntax -> VALUE #( ( name = 'DATE' ref = NEW d( '20201106' ) ) ( name = 'TIME' ref = NEW t( '152635' ) ) )
                                    it_format_field = lt_field
                                   ).
    zcl_eui_conv=>assert_equals(  act   = lv_text
                                  exp   = `AA 06.11.2020 BB 15:26:35`
                                  quit  = 0  ).
  ENDMETHOD.

  METHOD format_by_root.
    me->date = '20201106'.
    me->time = '152635'.
    DATA lv_text TYPE string.
    lv_text = zcl_xtt_html=>format( iv_template = `AA {R-DATE} BB {R-TIME}`
                                    " object or structure
                                    is_root     = me ).
    zcl_eui_conv=>assert_equals(  act   = lv_text
                                  exp   = `AA 06.11.2020 BB 15:26:35`
                                  quit  = 0  ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
* Instead of writing pretty long expression like this
* {R;cond=value-VORNA+2(1)}
* You can write in a short form
* {R:v-VORNA+2(1)}
*&---------------------------------------------------------------------*
CLASS lcl_demo_131 DEFINITION INHERITING FROM zcl_xtt_demo_131.
  PUBLIC SECTION.
    METHODS:
      _get_screen_context REDEFINITION.
ENDCLASS.

CLASS lcl_demo_131 IMPLEMENTATION.
  METHOD _get_screen_context.
    DATA: lr_screen_context TYPE REF TO ts_screen_context.

    GET REFERENCE OF rs_screen_context INTO lr_screen_context.
    lr_screen_context->p_max_count = 5.

    TRY.
        DATA lo_screen TYPE REF TO zcl_eui_screen.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic
            ir_context = lr_screen_context.

        DATA lo_error TYPE REF TO zcx_eui_exception.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    lo_screen->customize( name     = 'P_MAX_COUNT'
                          required = '1'
                          iv_label = 'Max. Personnel Number'(mpn) ).
    DATA lv_col_end TYPE i.
    lo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
    lo_screen->popup( iv_col_end = lv_col_end ). " 114 IF lo_screen->set_status( VALUE #( title = '' ) ).
    CHECK lo_screen->show( ) <> 'OK'.
    " Cancel is pressed
    CLEAR rs_screen_context.
  ENDMETHOD.
ENDCLASS.

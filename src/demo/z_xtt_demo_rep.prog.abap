*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_report IMPLEMENTATION.
  METHOD class_constructor.
    DATA lt_include TYPE STANDARD TABLE OF trdirt.
    SELECT name text INTO CORRESPONDING FIELDS OF TABLE lt_include "#EC TOO_MANY_ITAB_FIELDS "#EC "#EC CI_GENBUFF or "#EC "#EC CI_SGLSELECT
    FROM trdirt
    WHERE name LIKE 'Z_XTT_DEMO_N%'
      AND sprsl = 'E'.

    DATA lr_include TYPE REF TO trdirt.
    LOOP AT lt_include REFERENCE INTO lr_include.
      DATA ls_demo TYPE ts_demo.

      " Index of demo
      ls_demo-ind = lr_include->name+12.

      " Dynamic class name
      DATA lv_cl_name TYPE string.
      CONCATENATE `LCL_DEMO_` ls_demo-ind INTO lv_cl_name.
      CREATE OBJECT ls_demo-inst TYPE (lv_cl_name).

      " Description
      ls_demo-inst->v_desc = ls_demo-inst->get_desc_text( ).
      IF ls_demo-inst->v_desc IS INITIAL.
        ls_demo-inst->v_desc = lr_include->text.
      ENDIF.

      " All instances
      INSERT ls_demo INTO TABLE t_demo.
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    mo_injection = io_injection.
    _update_demo_listbox( ).
  ENDMETHOD.

  METHOD _update_demo_listbox.
    DATA lt_list TYPE vrm_values.

    DATA lr_demo TYPE REF TO ts_demo.
    LOOP AT t_demo REFERENCE INTO lr_demo.
      DATA lr_list TYPE REF TO vrm_value.
      APPEND INITIAL LINE TO lt_list REFERENCE INTO lr_list.

      lr_list->key  = lr_demo->ind.
      lr_list->text = lr_demo->inst->v_desc.
    ENDLOOP.

    " Update listbox
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'P_EXA'
        values = lt_list.
  ENDMETHOD.

  METHOD pbo.
    DATA lo_screen     TYPE REF TO zcl_eui_screen.
    DATA lo_error      TYPE REF TO zcx_eui_exception.
    DATA lt_customize  TYPE zcl_eui_screen=>tt_customize.
    DATA ls_customize  TYPE REF TO zcl_eui_screen=>ts_customize.

    " Prepare screens
    IF sy-dynnr <> '1000'.
      zcl_eui_screen=>top_pbo( ).
      RETURN.
    ENDIF.

    " Only for selection screen
    CHECK p_exa IS NOT INITIAL.

    " Show or hide controls
    DATA lr_demo TYPE REF TO ts_demo.
    READ TABLE t_demo REFERENCE INTO lr_demo WITH TABLE KEY ind = p_exa.
    CHECK sy-subrc = 0.

    " What PARAMETRS to show
    DATA ls_screen_opt TYPE lcl_demo=>ts_screen_opt.
    ls_screen_opt = lr_demo->inst->get_screen_opt( ).

    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr = sy-dynnr.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " By default is visible
    DEFINE _visible.
      APPEND INITIAL LINE TO lt_customize REFERENCE INTO ls_customize.
      ls_customize->input     = '1'.
      ls_customize->invisible = '0' .

      " screen-name or screen-group1
      IF &1 CS '*'.
        ls_customize->name    = &1.
      ELSE.
        ls_customize->group1 = &1.
      ENDIF.
    END-OF-DEFINITION.

    " Change current row
    DEFINE _hide.
      ls_customize->input     = '0'.
      ls_customize->invisible = '1' .
    END-OF-DEFINITION.

    _visible '*P_R_CNT*'.
    IF ls_screen_opt-row_count <> abap_true.
      _hide.
    ENDIF.

    _visible '*P_C_CNT*'.
    IF ls_screen_opt-colum_count <> abap_true.
      _hide.
    ENDIF.

    _visible '*P_B_CNT*'.
    IF ls_screen_opt-block_count <> abap_true.
      _hide.
    ENDIF.

    _visible '*P_ZIP*'.
    IF ls_screen_opt-zip <> abap_true.
      _hide.
    ENDIF.

    _visible '*IMG_SIZE*'.
    IF ls_screen_opt-img_size <> abap_true.
      _hide.
    ENDIF.

    " Show or hide paramaters
    lo_screen->customize( it_ = lt_customize ).
    lo_screen->pbo( ).
  ENDMETHOD.

  METHOD start_of_selection.
    " Data for report & ALV items
    CLEAR: o_demo, t_merge, t_merge_alv.

    DATA lr_demo TYPE REF TO ts_demo.
    READ TABLE t_demo REFERENCE INTO lr_demo WITH TABLE KEY ind = p_exa.
    CHECK sy-subrc = 0.

    " Current demo
    o_demo = lr_demo->inst.
    DATA lv_exit TYPE abap_bool.
    lv_exit = o_demo->set_merge_info( me ).

    CHECK mo_injection IS INITIAL
      AND lv_exit <> abap_true.

    DATA ls_grid_params TYPE ts_grid_params.
    ls_grid_params = _get_grid_params( ).
    show_alv( ls_grid_params ).
  ENDMETHOD.

  METHOD f4_full_path.
    DATA:
      lv_fullpath TYPE string,
      lv_filename TYPE string,
      lv_path     TYPE string,
      lv_result   TYPE i.

    lv_fullpath = cv_fullpath.
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title    = iv_title
      CHANGING
        filename    = lv_filename
        path        = lv_path
        fullpath    = lv_fullpath
        user_action = lv_result
      EXCEPTIONS
        OTHERS      = 1 ).
    CHECK sy-subrc = 0 AND lv_result = cl_gui_frontend_services=>action_ok.
    cv_fullpath = lv_fullpath.
  ENDMETHOD.

  METHOD f4_dir_browse.
    DATA:
      lv_path        TYPE string,
      lv_init_folder TYPE string.

    lv_init_folder = cv_path.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title   = iv_title
        initial_folder = lv_init_folder
      CHANGING
        selected_folder = lv_path
      EXCEPTIONS
        OTHERS          = 1 ).

    CHECK sy-subrc = 0 AND lv_path IS NOT INITIAL.
    cv_path = lv_path.
  ENDMETHOD.

  METHOD merge_add_one.
    DATA          ls_merge  TYPE ts_merge.
    FIELD-SYMBOLS <l_value> TYPE any.

    " № 1 - merge IV_BLOCK_NAME parameter
    ls_merge-key = iv_root_id.

    " № 2 - merge IS_BLOCK parameter (make copy)
    CREATE DATA ls_merge-val LIKE is_root.
    ASSIGN ls_merge-val->* TO <l_value>.
    <l_value> = is_root.

    INSERT ls_merge INTO TABLE t_merge.

    " Skip for test
    IF mo_injection IS NOT INITIAL.
      mo_injection->send_merge( ls_merge ).
      RETURN.
    ENDIF.

    DATA lt_sub_field TYPE zcl_eui_type=>tt_field_desc.
    lt_sub_field = _merge_get_sub_fields( is_root    = is_root
                                          iv_root_id = iv_root_id ).
    _merge_add_sub_fields_to_alv( is_root      = is_root
                                  iv_root_id   = iv_root_id
                                  it_sub_field = lt_sub_field ).
  ENDMETHOD.

  METHOD _merge_get_sub_fields.
    DATA lo_error TYPE REF TO zcx_eui_exception.
    TRY.
        DATA ls_field_desc TYPE zcl_eui_type=>ts_field_desc.
        ls_field_desc = zcl_eui_type=>get_field_desc( iv_field_name = iv_root_id
                                                      iv_data       = is_root
                                                      iv_tech       = abap_true ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    rt_sub_field = zcl_eui_type=>get_sub_field_desc( ls_field_desc ).
  ENDMETHOD.

  METHOD _get_dref.
    " Structure or Table or Object ?
    DATA lv_type TYPE abap_typekind.
    DESCRIBE FIELD is_root TYPE lv_type.

    FIELD-SYMBOLS <lv_value> TYPE any.
    CASE lv_type.
      WHEN cl_abap_typedescr=>typekind_struct2.
        ASSIGN COMPONENT iv_field OF STRUCTURE is_root TO <lv_value>.

      WHEN cl_abap_typedescr=>typekind_oref.
        DATA lv_name TYPE string.
        CONCATENATE 'IS_ROOT->' iv_field INTO lv_name.
        ASSIGN (lv_name) TO <lv_value>.

      WHEN OTHERS.
        zcx_eui_no_check=>raise_sys_error( iv_message = 'Cannot detect IS_ROOT type'(cdr) ).
    ENDCASE.

    " Already ref
    DESCRIBE FIELD <lv_value> TYPE lv_type.
    IF lv_type = cl_abap_typedescr=>typekind_dref.
      rr_data = <lv_value>.
      RETURN.
    ENDIF.

    GET REFERENCE OF <lv_value> INTO rr_data.
  ENDMETHOD.

  METHOD _merge_add_sub_fields_to_alv.
    " Structure or Table or Object ?
    DATA lv_type TYPE abap_typekind.
    DESCRIBE FIELD is_root TYPE lv_type.

    " Add each line of table
    IF lv_type = cl_abap_typedescr=>typekind_table.
      FIELD-SYMBOLS: <lt_root> TYPE ANY TABLE, <ls_root> TYPE any.
      ASSIGN is_root TO <lt_root>.
      LOOP AT <lt_root> ASSIGNING <ls_root>.
        DATA lv_root_id TYPE string.
        lv_root_id = sy-tabix.
        CONDENSE lv_root_id.

        " New ID
        CONCATENATE iv_root_id `[` lv_root_id `]` INTO lv_root_id.
        " Recursion
        _merge_add_sub_fields_to_alv( is_root      = <ls_root>
                                      iv_root_id   = lv_root_id
                                      it_sub_field = it_sub_field ).
      ENDLOOP.
      RETURN.
    ENDIF.

    DATA lr_sub_field TYPE REF TO zcl_eui_type=>ts_field_desc.
    LOOP AT it_sub_field REFERENCE INTO lr_sub_field.
      DATA lr_alv TYPE REF TO ts_merge_alv.
      APPEND INITIAL LINE TO t_merge_alv REFERENCE INTO lr_alv.

      lr_alv->root_id = iv_root_id.
      lr_alv->ui_type = lr_sub_field->ui_type.
      lr_alv->field   = lr_sub_field->name.

      DATA: lr_value TYPE REF TO data, lv_value TYPE text255.
      lr_value = _get_dref( is_root  = is_root
                            iv_field = lr_sub_field->name ).
      CASE lr_alv->ui_type.
        WHEN zcl_eui_type=>mc_ui_type-table.
          lv_value = _get_table_text( ir_value = lr_value
                                      ir_alv   = lr_alv ).
        WHEN OTHERS.
          FIELD-SYMBOLS <lv_value> TYPE any.
          ASSIGN lr_value->* TO <lv_value>.
          WRITE <lv_value> TO lv_value LEFT-JUSTIFIED.
      ENDCASE.
      lr_alv->value = lv_value.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_table_text.
    FIELD-SYMBOLS <lt_value> TYPE ANY TABLE.
    ASSIGN ir_value->* TO <lt_value>.

    rv_text = lines( <lt_value> ).
    CONDENSE rv_text.
    CONCATENATE 'Lines count ='(lcn) rv_text INTO rv_text SEPARATED BY space.

    " Get more attention
    CONCATENATE ir_alv->field ` [] ---> ` INTO ir_alv->field.

    " Change color of cell
    FIELD-SYMBOLS <ls_color> LIKE LINE OF ir_alv->t_color.
    APPEND INITIAL LINE TO ir_alv->t_color ASSIGNING <ls_color>.
    <ls_color>-fname = 'VALUE'.
    <ls_color>-color-col = <ls_color>-color-int = '1'.
  ENDMETHOD.

  METHOD _get_grid_params.
    GET REFERENCE OF t_merge_alv INTO rs_gp-r_table.

    rs_gp-t_catalog = _make_tech_catalog( rs_gp-r_table ).
    DATA lr_field TYPE REF TO lvc_s_fcat.
    READ TABLE rs_gp-t_catalog REFERENCE INTO lr_field WITH KEY fieldname = 'VALUE'.
    lr_field->hotspot = 'X'.

    rs_gp-s_layout-ctab_fname = 'T_COLOR'.
    CONCATENATE o_demo->v_desc ` - №` p_exa INTO rs_gp-s_layout-grid_title.
  ENDMETHOD.

  METHOD show_alv.
    DATA lt_toolbar TYPE ttb_button.
    lt_toolbar = _make_toolbar( ).

    " Merge both toolbars
    APPEND LINES OF is_grid_params-t_toolbar TO lt_toolbar.

    CREATE OBJECT co_alv
      EXPORTING
        ir_table       = is_grid_params-r_table
        is_layout      = is_grid_params-s_layout
        it_mod_catalog = is_grid_params-t_catalog
        it_sort        = is_grid_params-t_sort
        it_toolbar     = lt_toolbar.

    co_alv->set_top_of_page_height( ).
    co_alv->show( io_handler      = me
                  iv_handlers_map = '_ON_HOTSPOT_CLICK;_ON_TOP_OF_PAGE;_ON_USER_COMMAND' ).
  ENDMETHOD.

  METHOD _get_from_root_id.
    DATA: lv_root_id TYPE string, lv_index TYPE string.
    IF iv_root_id CS `[`.
      lv_root_id = iv_root_id(sy-fdpos).
      lv_index   = iv_root_id+sy-fdpos.
      REPLACE FIRST OCCURRENCE OF: `[` IN lv_index WITH ``,
                                   `]` IN lv_index WITH ``.
    ELSE.
      lv_root_id = iv_root_id.
    ENDIF.

    FIELD-SYMBOLS <ls_merge> LIKE LINE OF t_merge.
    READ TABLE t_merge ASSIGNING <ls_merge>
     WITH KEY key = lv_root_id.
    CHECK sy-subrc = 0.

    rr_root = <ls_merge>-val.
    CHECK lv_index IS NOT INITIAL.

    FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.
    ASSIGN rr_root->* TO <lt_table>.
    READ TABLE <lt_table> REFERENCE INTO rr_root INDEX lv_index.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    FIELD-SYMBOLS <ls_alv> LIKE LINE OF t_merge_alv.
    READ TABLE t_merge_alv ASSIGNING <ls_alv> INDEX e_row_id-index.
    CHECK sy-subrc = 0
      AND <ls_alv>-ui_type = zcl_eui_type=>mc_ui_type-table.

    DATA          lr_root   TYPE REF TO data.
    FIELD-SYMBOLS <ls_root> TYPE any.
    lr_root = _get_from_root_id( <ls_alv>-root_id ).
    ASSIGN lr_root->* TO <ls_root>.

    " Could be special symbols
    DATA: lv_field TYPE string, lv_rem TYPE string.         "#EC NEEDED
    SPLIT <ls_alv>-field AT space INTO lv_field lv_rem.

    DATA lr_table TYPE REF TO data.
    lr_table = _get_dref( is_root  = <ls_root>
                          iv_field = lv_field ).

    " Could be HASED or indexed
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
    ASSIGN lr_table->* TO <lt_table>.
    lr_table = _make_std_table_copy( <lt_table> ).

    " Show in popup
    DATA lt_catalog TYPE lvc_t_fcat.
    lt_catalog = _make_tech_catalog( lr_table ).

    DATA lo_alv TYPE REF TO zcl_eui_alv.
    CREATE OBJECT lo_alv
      EXPORTING
        ir_table       = lr_table
        it_mod_catalog = lt_catalog.

    lo_alv->popup( ).
    lo_alv->show( ).
  ENDMETHOD.

  METHOD _on_top_of_page.
    DATA: lv_full_url TYPE string, lv_text TYPE text255.
    lv_full_url = o_demo->get_url_base( ).
    CHECK lv_full_url IS NOT INITIAL.

    DATA lv_prefix TYPE string VALUE 'https://bizhuka.github.io'. "#EC NOTEXT
    IF sy-langu = 'R'.
      CONCATENATE lv_prefix '/ru' INTO lv_prefix.
    ENDIF.

    CONCATENATE lv_prefix lv_full_url INTO lv_full_url.
    CONCATENATE 'Documentation №'(dcn)
                p_exa
                o_demo->v_desc INTO lv_text SEPARATED BY space.

    e_dyndoc_id->add_link( text =  lv_text
                           url  =  lv_full_url ).
  ENDMETHOD.

  METHOD _on_user_command.
    CASE e_ucomm.
      WHEN c_cmd-template.
        o_demo->template( ).

      WHEN c_cmd-send.
        o_demo->send( t_merge[] ).

      WHEN c_cmd-show.
        o_demo->show( t_merge[] ).

      WHEN c_cmd-download.
        o_demo->download( t_merge[] ).

      WHEN OTHERS.
        o_demo->on_user_command( sender  = sender
                                 e_ucomm = e_ucomm ).
    ENDCASE.
  ENDMETHOD.

  METHOD _make_std_table_copy.
    DATA lr_row TYPE REF TO data.
    CREATE DATA lr_row LIKE LINE OF it_table.

    FIELD-SYMBOLS <ls_row> TYPE any.
    ASSIGN lr_row->* TO <ls_row>.
    CREATE DATA rr_table LIKE STANDARD TABLE OF <ls_row>.

    " Fill STANDARD copy
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN rr_table->* TO <lt_table>.
    <lt_table>[] = it_table[].
  ENDMETHOD.

  METHOD _make_tech_catalog.
    DATA lt_catalog LIKE rt_catalog.
    lt_catalog = zcl_eui_type=>get_catalog( ir_table = ir_table ).

    DATA: lr_src  TYPE REF TO lvc_s_fcat, lr_dest LIKE lr_src.
    LOOP AT lt_catalog REFERENCE INTO lr_src.
      APPEND INITIAL LINE TO rt_catalog REFERENCE INTO lr_dest.
      lr_dest->coltext = lr_dest->fieldname = lr_src->fieldname.
    ENDLOOP.
  ENDMETHOD.

  METHOD _make_toolbar.
    FIELD-SYMBOLS <ls_button> LIKE LINE OF rt_toolbar.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-butn_type = cntb_btype_sep.

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = c_cmd-template.
    <ls_button>-icon     = icon_xls.
    <ls_button>-text     = 'Template'(tem).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = c_cmd-send.
    <ls_button>-icon     = icon_eml.
    <ls_button>-text     = 'Send'(eml).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = c_cmd-show.
    <ls_button>-icon     = icon_display.
    <ls_button>-text     = 'Show'(shw).

    APPEND INITIAL LINE TO rt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = c_cmd-download.
    <ls_button>-icon     = icon_execute_object.
    <ls_button>-text     = 'Download'(dwn).
  ENDMETHOD.

  METHOD init_random_generator.
    CHECK mo_rand_i IS INITIAL OR mo_injection IS NOT INITIAL.

    " Always the same random data
    DATA lv_seed TYPE i.
    IF mo_injection IS NOT INITIAL.
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

  METHOD get_random_table.
    DATA:
      ls_no_sum TYPE ts_no_sum,
      lv_int    TYPE i,
      lv_column TYPE string.
    FIELD-SYMBOLS:
      <ls_item> TYPE any,
      <lv_sum>  TYPE bf_rbetr. " P with sign
    init_random_generator( ).

    CLEAR et_table.
    DO p_r_cnt TIMES.
      " Fill without sums
      CLEAR ls_no_sum.

      " Special XML symbols <>
      ls_no_sum-caption = sy-index.
      CONCATENATE `<Caption ` ls_no_sum-caption `/>` INTO ls_no_sum-caption.

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

      " Fill R-T-SUM*
      DO iv_column_cnt TIMES.
        " Get column name
        lv_column = sy-index.
        CONDENSE lv_column.
        CONCATENATE `SUM` lv_column INTO lv_column.

        " Exist ?
        ASSIGN COMPONENT lv_column OF STRUCTURE <ls_item> TO <lv_sum>.
        IF sy-subrc <> 0.
          zcx_xtt_exception=>raise_dump( iv_message = `Check data structure` ). "#EC NOTEXT
        ENDIF.

        " Show with decimals
        <lv_sum> = mo_rand_p->get_next( ).                  " / 100
        <lv_sum> = <lv_sum> / 100.
      ENDDO.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

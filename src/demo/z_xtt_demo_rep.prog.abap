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
    FIELD-SYMBOLS: <ls_demo> TYPE ts_demo.
    super->constructor( iv_test_mode ).

    LOOP AT t_demo ASSIGNING <ls_demo>.
      <ls_demo>-inst->set_report( me ).
    ENDLOOP.

    _online_docu_button( ).
    _update_demo_listbox( ).
  ENDMETHOD.

  method merge_add_one.
    rs_merge = super->merge_add_one(
       is_root    = is_root
       iv_root_id = iv_root_id
       io_helper  = io_helper
    ).

    " Skip for test
    IF mv_test_mode = abap_true.
      INSERT rs_merge INTO TABLE mr_test_demo->merge.
      RETURN.
    ENDIF.

    DATA lt_sub_field TYPE zcl_eui_type=>tt_field_desc.
    lt_sub_field = _get_sub_fields( is_root    = is_root
                                    iv_root_id = iv_root_id ).
    _merge_add_sub_fields_to_alv( is_root      = is_root
                                  iv_root_id   = iv_root_id
                                  it_sub_field = lt_sub_field ).
  ENDMETHOD.


  METHOD _get_sub_fields.
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

  METHOD _online_docu_button.
    CREATE OBJECT mo_menu_docu
      EXPORTING
        io_handler = me.

    DATA lt_menu TYPE zcl_eui_menu=>tt_menu.
    DATA lr_menu TYPE REF TO zcl_eui_menu=>ts_menu.

    APPEND INITIAL LINE TO lt_menu REFERENCE INTO lr_menu.
    lr_menu->function = 'ONLINE_DOCU'.
    lr_menu->text     = 'Online documentation'(odo).
    lr_menu->icon     = icon_message_information_small.

    mo_menu_docu->create_toolbar( it_menu  = lt_menu
                                  iv_width = 200 ).
  ENDMETHOD.

  METHOD _on_function_selected.
    CHECK fcode = 'ONLINE_DOCU'.

    DATA lv_url TYPE text255.
    lv_url = _get_docu_url( '/xtt' ).

    " Show online documentation in browser
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url    = lv_url
      EXCEPTIONS
        OTHERS = 0.
  ENDMETHOD.

  METHOD _hide_online_docu.
    DATA lo_container TYPE REF TO cl_gui_container.
    lo_container = mo_menu_docu->get_container( ).
    IF lo_container IS NOT INITIAL.
      lo_container->set_visible( abap_false ).
    ENDIF.
  ENDMETHOD.

  METHOD _get_docu_url.
    DATA lv_prefix TYPE string VALUE 'https://bizhuka.github.io'. "#EC NOTEXT
    IF sy-langu = 'R'.
      CONCATENATE lv_prefix '/ru' INTO lv_prefix.
    ENDIF.

    CONCATENATE lv_prefix iv_append INTO rv_full_url.
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
    DATA ls_screen_opt TYPE ZCL_XTT_DEMO=>ts_screen_opt.
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
    mv_r_cnt = iv_r_cnt.
    mv_c_cnt = iv_c_cnt.
    mv_b_cnt = iv_b_cnt.
    _hide_online_docu( ).

    " Data for report & ALV items
    CLEAR: o_demo, t_merge, t_merge_alv.

    DATA lr_demo TYPE REF TO ts_demo.
    READ TABLE t_demo REFERENCE INTO lr_demo WITH TABLE KEY ind = p_exa.
    CHECK sy-subrc = 0.

    " Current demo
    o_demo = lr_demo->inst.
    DATA lv_exit TYPE abap_bool.
    lv_exit = o_demo->set_merge_info( ).

    CHECK mv_test_mode <> abap_true
      AND lv_exit      <> abap_true.

    IF p_action IS NOT INITIAL AND p_templ IS NOT INITIAL.
      CASE p_action.
        WHEN 'D'.
          download( it_merge    = t_merge[]
                    iv_template = p_templ ).
        WHEN 'S'.
          show( it_merge    = t_merge[]
                iv_template = p_templ ).
        WHEN 'E'.
          send( it_merge    = t_merge[]
                iv_template = p_templ ).
      ENDCASE.

      IF zcl_xtt_util=>is_common_gui( ) = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.
      RETURN.
    ENDIF.

    DATA ls_grid_params TYPE ZCL_XTT_DEMO=>ts_grid_params.
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
        window_title = iv_title
      CHANGING
        filename     = lv_filename
        path         = lv_path
        fullpath     = lv_fullpath
        user_action  = lv_result
      EXCEPTIONS
        OTHERS       = 1 ).
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
        window_title    = iv_title
        initial_folder  = lv_init_folder
      CHANGING
        selected_folder = lv_path
      EXCEPTIONS
        OTHERS          = 1 ).

    CHECK sy-subrc = 0 AND lv_path IS NOT INITIAL.
    cv_path = lv_path.
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

    _check_has_sub_tables( io_alv   = lo_alv
                           it_table = <lt_table> ).
    lo_alv->popup( ).
    lo_alv->show( ).
  ENDMETHOD.

  METHOD _check_has_sub_tables.
    DATA lt_sub_field TYPE zcl_eui_type=>tt_field_desc.
    lt_sub_field = _get_sub_fields( is_root    = it_table
                                    iv_root_id = '' ).

    FIELD-SYMBOLS <ls_sub_field> LIKE LINE OF lt_sub_field.
    DATA lv_has_sub_table TYPE abap_bool.
    LOOP AT lt_sub_field ASSIGNING <ls_sub_field>.
      <ls_sub_field>-label = <ls_sub_field>-name.

      CHECK <ls_sub_field>-ui_type = zcl_eui_type=>mc_ui_type-table.
      lv_has_sub_table = abap_true.

      DATA lt_sub2 LIKE lt_sub_field.
      zcl_eui_conv=>from_json( EXPORTING iv_json = <ls_sub_field>-sub_fdesc
                               IMPORTING ex_data = lt_sub2 ).

      FIELD-SYMBOLS <ls_sub2> LIKE LINE OF lt_sub2.
      LOOP AT lt_sub2 ASSIGNING <ls_sub2>.
        <ls_sub2>-label = <ls_sub2>-name.
      ENDLOOP.
      <ls_sub_field>-sub_fdesc = zcl_eui_conv=>to_json( im_data = lt_sub2 ).
    ENDLOOP.

    CHECK lv_has_sub_table = abap_true.
    DATA ls_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc.
    CREATE DATA ls_field_desc.
    ls_field_desc->sub_fdesc = zcl_eui_conv=>to_json( im_data = lt_sub_field ).
    io_alv->set_field_desc( ls_field_desc ).
  ENDMETHOD.

  METHOD _on_top_of_page.
    DATA: lv_full_url TYPE string, lv_text TYPE text255.

    lv_full_url = o_demo->get_url_base( ).
    CHECK lv_full_url IS NOT INITIAL.
    lv_full_url = _get_docu_url( lv_full_url ).

    CONCATENATE 'Documentation №'(dcn)
                p_exa
                o_demo->v_desc INTO lv_text SEPARATED BY space.

    e_dyndoc_id->add_link( text = lv_text
                           url  = lv_full_url ).
  ENDMETHOD.

  METHOD _on_user_command.
    CASE e_ucomm.
      WHEN c_cmd-template.
        o_demo->template( ).

      WHEN c_cmd-send.
        send( t_merge[] ).

      WHEN c_cmd-show.
        show( t_merge[] ).

      WHEN c_cmd-download.
        download( it_merge = t_merge[] ).

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


  METHOD show.
    DATA lv_template TYPE string.
    lv_template = iv_template.
    IF lv_template IS INITIAL.
      lv_template = get_template_by_f4( ).
    ENDIF.

    DATA: lo_xtt TYPE REF TO zcl_xtt.
    o_demo->get_from_template( EXPORTING iv_template = lv_template
                               IMPORTING eo_xtt      = lo_xtt ).
    CHECK lo_xtt IS NOT INITIAL.

    " Paste data
    o_demo->merge( io_xtt   = lo_xtt
                   it_merge = it_merge[] ).

    " For PAI & PBO events
    lo_xtt->show( io_handler = me ).
  ENDMETHOD.

  METHOD download.
    DATA: lv_template TYPE string, lo_xtt TYPE REF TO zcl_xtt.

    IF iv_template IS NOT INITIAL.
      lv_template = iv_template.
    ELSE.
      " Prepare screen '1010'
      CLEAR p_path.
      lv_template = _show_screen( iv_dynnr = '1010'
                                  iv_lb_id = 'P_DTEMPL'
                                  iv_title = 'Download'(dwn) ).
    ENDIF.

    o_demo->get_from_template( EXPORTING iv_template = lv_template
                               IMPORTING eo_xtt      = lo_xtt ).
    CHECK lo_xtt IS NOT INITIAL.

    " Stop befor merge
    IF p_stop = abap_true.
      _is_break_point_active( ).
      BREAK-POINT ID zxtt_break_point.    " Double click here --> it_merge[] <--
    ENDIF.

    IF mv_test_mode = abap_true.
      prepare( lo_xtt ).
    ENDIF.

    " Paste data
    o_demo->merge( io_xtt   = lo_xtt
                   it_merge = it_merge[] ).

    o_demo->do_download( EXPORTING io_xtt      = lo_xtt
                                   iv_open     = p_open
                                   iv_zip      = p_zip
                         CHANGING  cv_fullpath = p_path ).
  ENDMETHOD.

  METHOD send.
    DATA: lv_template TYPE string, lo_xtt TYPE REF TO zcl_xtt.

    " Prepare screen '1020'. Send to current user
    SELECT SINGLE adr6~smtp_addr INTO p_email
    FROM adr6
    INNER JOIN usr21 ON usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
    WHERE usr21~bname = sy-uname.                       "#EC CI_NOORDER

    lv_template = iv_template.
    IF lv_template IS INITIAL.
      lv_template = _show_screen( iv_dynnr = '1020'
                                  iv_lb_id = 'P_STEMPL'
                                  iv_title = 'Send options'(snd) ).
    ENDIF.

    o_demo->get_from_template( EXPORTING iv_template = lv_template
                               IMPORTING eo_xtt      = lo_xtt ).
    CHECK lo_xtt IS NOT INITIAL.

    " Paste data
    o_demo->merge( io_xtt   = lo_xtt
                   it_merge = it_merge[] ).

    _send_email( lo_xtt ).
  ENDMETHOD.

  METHOD _send_email.
    DATA lo_recipient  TYPE REF TO if_recipient_bcs.
    DATA lt_recipient  TYPE rmps_recipient_bcs.
    DATA lo_err        TYPE REF TO cx_address_bcs.

    " Add recipients
    TRY.
        IF p_user IS NOT INITIAL.
          lo_recipient = cl_sapuser_bcs=>create( p_user ).
          APPEND lo_recipient TO lt_recipient.
        ENDIF.

        IF p_email IS NOT INITIAL.
          lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ).
          APPEND lo_recipient TO lt_recipient.
        ENDIF.
      CATCH cx_address_bcs INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    CHECK lt_recipient IS NOT INITIAL.
    io_xtt->send(
      it_recipients = lt_recipient
      iv_subject    = p_title
      iv_body       = p_text ).
  ENDMETHOD.

  METHOD _is_break_point_active.
    DATA:
      lv_exp_tstamp   TYPE aab_id_act-exp_tstamp,
      lv_date         TYPE d,
      lv_time         TYPE t,
      lv_datetime_db  TYPE char14,
      lv_datetime_now TYPE char14.

    " Activatable IDs for Breakpoints and Assertions: Activation
    SELECT SINGLE exp_tstamp INTO lv_exp_tstamp
    FROM aab_id_act
    WHERE name       = 'ZXTT_BREAK_POINT'
      AND username   = sy-uname
      AND server     = space
      AND is_program = space
      AND actdefault = space.

    " Convert to SAP
    CONVERT TIME STAMP lv_exp_tstamp TIME ZONE sy-zonlo
      INTO DATE lv_date TIME lv_time.

    " For comaparison
    CONCATENATE lv_date  lv_time  INTO lv_datetime_db.
    CONCATENATE sy-datum sy-uzeit INTO lv_datetime_now.

    CHECK lv_datetime_now > lv_datetime_db.
    MESSAGE 'Activate "ZXTT_BREAK_POINT" in tr. SAAB'(tbr) TYPE 'I'. " DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD _show_screen.
    DATA lo_screen  TYPE REF TO zcl_eui_screen.
    DATA lo_error   TYPE REF TO zcx_eui_exception.
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr = iv_dynnr.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Prepare scrren
    DATA lt_listbox TYPE vrm_values.
    lt_listbox = o_demo->get_template_lisbox( ).
    lo_screen->customize( name       = iv_lb_id
                          it_listbox = lt_listbox
                          required   = '1' ).

    " Listbox parameter
    FIELD-SYMBOLS: <lv_template> TYPE vrm_value-key, <ls_template> LIKE LINE OF lt_listbox.
    ASSIGN (iv_lb_id) TO <lv_template>.

    " Set 1st by default
    READ TABLE lt_listbox ASSIGNING <ls_template> INDEX 1.
    IF sy-subrc = 0.
      <lv_template> = <ls_template>-key.
    ENDIF.

    " Instead of 'WITH FRAME TITLE'
    DATA ls_status TYPE zif_eui_manager=>ts_status.
    ls_status-title = iv_title.
    lo_screen->set_status( ls_status ).

    " Show popup
    DATA lv_col_end TYPE i.
    lo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
    lo_screen->popup( iv_col_end = lv_col_end ).
    CHECK lo_screen->show( ) = 'OK'.

    " Return template from screen
    rv_template = <lv_template>.
  ENDMETHOD.

  METHOD prepare.
    SET HANDLER on_prepare_raw FOR io_xtt.

    DATA lo_class TYPE REF TO cl_abap_classdescr.
    lo_class ?= cl_abap_classdescr=>describe_by_object_ref( io_xtt ).

    " For data exporting
    CASE lo_class->absolute_name.
      WHEN '\CLASS=ZCL_XTT_WORD_DOCX'.
        " io_xtt->add_raw_event( 'word/document.xml' ).
        io_xtt->add_raw_event( 'word/header1.xml' ).
        io_xtt->add_raw_event( 'word/footer1.xml' ).

      WHEN '\CLASS=ZCL_XTT_EXCEL_XLSX'.
        io_xtt->add_raw_event( 'xl/workbook.xml' ).
        io_xtt->add_raw_event( 'xl/_rels/workbook.xml.rels' ).

        " Max number of sheets
        DO 12 TIMES.
          " Path to file
          DATA lv_path TYPE string.

          lv_path = sy-index.
          CONDENSE lv_path NO-GAPS.

          CONCATENATE `xl/worksheets/sheet` lv_path `.xml` INTO lv_path.

          io_xtt->add_raw_event( lv_path ).
        ENDDO.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD on_prepare_raw.
    " No need to export entire file
    CHECK iv_path IS NOT INITIAL.

    " Work with copy
    DATA lv_content TYPE xstring.
    lv_content = ir_content->*.

    DO 1 TIMES.
      DATA lo_dom TYPE REF TO if_ixml_document.
      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          xml      = lv_content
        IMPORTING
          document = lo_dom
        EXCEPTIONS
          OTHERS   = 1.
      CHECK sy-subrc = 0.

      CALL FUNCTION 'SDIXML_DOM_TO_XML'
        EXPORTING
          document      = lo_dom
          pretty_print  = 'X'
        IMPORTING
          xml_as_string = lv_content
        EXCEPTIONS
          OTHERS        = 1.
      CHECK sy-subrc = 0.
    ENDDO.

    DATA lv_path TYPE string.
    "TODO check path
    CONCATENATE `C:\Users\modekz\AppData\Local\SAP\SAP GUI\tmp\` _raw_folder `\` iv_path INTO lv_path.
    REPLACE ALL OCCURRENCES OF `/` IN lv_path WITH `\`.

    " Export file
    DATA lo_file   TYPE REF TO zcl_eui_file.
    DATA lo_error  TYPE REF TO zcx_eui_exception.

    TRY.
        CREATE OBJECT lo_file.
        lo_file->import_from_xstring( lv_content ).
        lo_file->download( iv_full_path = lv_path ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD create_new_test_demo.
    APPEND INITIAL LINE TO mt_test_demo REFERENCE INTO rr_test_demo.
    mr_test_demo = rr_test_demo.
  ENDMETHOD.

  METHOD fill_file_info.
    APPEND INITIAL LINE TO mr_test_demo->files REFERENCE INTO rr_file.

    o_demo->get_from_template( EXPORTING iv_template = iv_objid
                               IMPORTING ev_type     = rr_file->kind ).

    " 2 unique names
    rr_file->template = rr_file->report  = iv_objid.
    REPLACE FIRST OCCURRENCE OF '-' IN: rr_file->template WITH `_T.`,
                                        rr_file->report   WITH `_R.`.
    REPLACE FIRST OCCURRENCE OF '.XDP' IN rr_file->report WITH '.PDF'.


    " Download preparations
    p_path = rr_file->report.

    CONCATENATE p_exa `_` iv_objid  INTO _raw_folder.
  ENDMETHOD.
ENDCLASS.

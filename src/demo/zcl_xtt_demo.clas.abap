CLASS zcl_xtt_demo DEFINITION PUBLIC ABSTRACT CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_merge,
        key TYPE string,
        val TYPE REF TO data,
        obj TYPE REF TO object, " For 160 only
      END OF ts_merge,
      tt_merge TYPE STANDARD TABLE OF ts_merge WITH DEFAULT KEY, " Same order HASHED  UNIQUE KEY key,

      " Random table data
      BEGIN OF ts_rand_data,
        group   TYPE text100, "string,
        caption TYPE text100, "string,
        date    TYPE d,
        sum1    TYPE bf_rbetr, " P with sign
        sum2    TYPE bf_rbetr, " P with sign
      END OF ts_rand_data,
      tt_rand_data TYPE STANDARD TABLE OF ts_rand_data WITH DEFAULT KEY,

      BEGIN OF ts_no_sum,
        group   TYPE text100, "string,
        caption TYPE text100, "string,
        date    TYPE d,
      END OF ts_no_sum,

      " Dynamic columns new syntax
*  tt_sums TYPE STANDARD TABLE OF bf_rbetr WITH DEFAULT KEY,
      BEGIN OF ts_sum_alv,
        sum TYPE bf_rbetr, " Just 1 field for alv only
      END OF ts_sum_alv,
      tt_sums_alv TYPE STANDARD TABLE OF ts_sum_alv WITH DEFAULT KEY,

      BEGIN OF ts_grid_params,
        r_table   TYPE REF TO data,
        s_layout  TYPE lvc_s_layo,
        t_catalog TYPE lvc_t_fcat,
        t_sort    TYPE lvc_t_sort,
        t_toolbar TYPE ttb_button,
      END OF ts_grid_params,

      BEGIN OF ts_screen_opt,
        row_count   TYPE abap_bool,
        colum_count TYPE abap_bool,
        block_count TYPE abap_bool,
        zip         TYPE abap_bool,
        img_size    TYPE abap_bool,
      END OF ts_screen_opt,

      BEGIN OF ts_template,
        objid TYPE wwwdata-objid,
      END OF ts_template,
      tt_template TYPE STANDARD TABLE OF ts_template WITH DEFAULT KEY,

      BEGIN OF ts_vrm_value,
        key  TYPE wwwdata-objid,
        text TYPE text80,
      END OF ts_vrm_value,
      tt_vrm_value TYPE STANDARD TABLE OF ts_vrm_value WITH DEFAULT KEY.

    DATA:
      v_desc    TYPE string.

    METHODS:
      set_report IMPORTING io_report TYPE REF TO zcl_xtt_report,

      get_desc_text
        RETURNING VALUE(rv_desc_text) TYPE string,

      get_url_base
        RETURNING VALUE(rv_url_base) TYPE string,

      get_screen_opt
        RETURNING VALUE(rs_opt) TYPE ts_screen_opt,

      template FINAL,

      merge
        IMPORTING
          io_xtt   TYPE REF TO zcl_xtt
          it_merge TYPE tt_merge,

      get_raw FINAL
        IMPORTING
          it_merge    TYPE tt_merge
          iv_template TYPE csequence,

      set_merge_info ABSTRACT
        RETURNING VALUE(rv_exit) TYPE abap_bool,

      get_templates ABSTRACT
        RETURNING VALUE(rt_templates) TYPE tt_template,

      download_template
        IMPORTING
          io_file      TYPE REF TO zif_xtt_file
          iv_file_name TYPE csequence OPTIONAL,

      get_from_template
        IMPORTING
          iv_template TYPE csequence
        EXPORTING
          ev_class    TYPE string
          ev_type     TYPE string
          eo_xtt      TYPE REF TO zcl_xtt
          eo_file     TYPE REF TO zif_xtt_file,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          sender
          e_ucomm,

      do_download
        IMPORTING
          io_xtt      TYPE REF TO zcl_xtt
          iv_open     TYPE csequence
          iv_zip      TYPE abap_bool
        CHANGING
          cv_fullpath TYPE csequence,

      get_template_lisbox
        RETURNING VALUE(rt_listbox) TYPE tt_vrm_value.
  PROTECTED SECTION.
    DATA mo_report TYPE REF TO zcl_xtt_report.

    METHODS:
      _make_string_message
        IMPORTING
                  iv_info           TYPE csequence
        RETURNING VALUE(rr_message) TYPE REF TO string.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_xtt_demo IMPLEMENTATION.
  METHOD set_report.
    mo_report = io_report.
  ENDMETHOD.

  METHOD get_desc_text.
    rv_desc_text = ''.
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = ''.
  ENDMETHOD.

  METHOD get_screen_opt.
    " Hide all by default
    CLEAR rs_opt.
  ENDMETHOD.

  METHOD merge.
    FIELD-SYMBOLS: <ls_merge> LIKE LINE OF it_merge,
                   <ls_root>  TYPE any.

    LOOP AT it_merge ASSIGNING <ls_merge>.
      ASSIGN <ls_merge>-val->* TO <ls_root>.

      io_xtt->merge( is_block      = <ls_root>
                     iv_block_name = <ls_merge>-key " <--- 'R' by defualt
                     io_helper     = <ls_merge>-obj " For 160 example only
                     ).
    ENDLOOP.
  ENDMETHOD.

  METHOD do_download.
    IF iv_open = abap_true.
      " All parameters are optional
      io_xtt->download( EXPORTING iv_zip      = iv_zip
                        CHANGING  cv_fullpath = cv_fullpath ).
      RETURN.
    ENDIF.

    io_xtt->download( EXPORTING iv_open     = iv_open " Could be ZCL_XTT=>MC_BY-OLE.  @see N070!
                                iv_zip      = iv_zip
                      CHANGING  cv_fullpath = cv_fullpath ).
  ENDMETHOD.

  METHOD template.
    DATA: lv_template TYPE string, lo_file TYPE REF TO zif_xtt_file.
    lv_template = mo_report->get_template_by_f4( ).

    get_from_template( EXPORTING iv_template = lv_template
                       IMPORTING eo_file     = lo_file ).
    download_template( lo_file ).
  ENDMETHOD.

  METHOD get_raw.
    DATA lo_xtt TYPE REF TO zcl_xtt.
    get_from_template( EXPORTING iv_template = iv_template
                       IMPORTING eo_xtt      = lo_xtt ).
    CHECK lo_xtt IS NOT INITIAL.

    " Paste data
    merge( io_xtt   = lo_xtt
           it_merge = it_merge[] ).

    " Take binary file
    DATA lv_file TYPE xstring.
    lv_file = lo_xtt->get_raw( ).
  ENDMETHOD.

  METHOD get_template_lisbox.
    DATA lt_template TYPE tt_template.
    lt_template = get_templates( ).
    CHECK lt_template IS NOT INITIAL.

    " Set KEY
    DATA lr_template TYPE REF TO ts_template.
    LOOP AT lt_template REFERENCE INTO lr_template.
      DATA lr_listbox TYPE REF TO ts_vrm_value.
      APPEND INITIAL LINE TO rt_listbox REFERENCE INTO lr_listbox.

      lr_listbox->key = lr_template->objid.
    ENDLOOP.

    DATA lt_wwwdata TYPE STANDARD TABLE OF wwwdata WITH DEFAULT KEY.
    SELECT DISTINCT objid text INTO CORRESPONDING FIELDS OF TABLE lt_wwwdata "#EC TOO_MANY_ITAB_FIELDS
    FROM wwwdata
    FOR ALL ENTRIES IN lt_template
    WHERE relid = 'MI'
      AND objid = lt_template-objid.

    " Set text
    LOOP AT rt_listbox REFERENCE INTO lr_listbox.
      DATA lr_wwwdata TYPE REF TO wwwdata.
      READ TABLE lt_wwwdata REFERENCE INTO lr_wwwdata
       WITH KEY objid = lr_listbox->key.
      CHECK sy-subrc = 0.

      lr_listbox->text = lr_wwwdata->text.

      " Add info from template extension
      DATA lv_type TYPE string.
      get_from_template( EXPORTING iv_template = lr_listbox->key
                         IMPORTING ev_type     = lv_type ).
      CONCATENATE lr_listbox->text ` - ` lv_type INTO lr_listbox->text.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_from_template.
    CLEAR: eo_file, eo_xtt, ev_type, ev_class.
    CHECK iv_template IS NOT INITIAL.

    IF iv_template CP '*-DOC*'.
      ev_class = 'ZCL_XTT_WORD_DOCX'.
      ev_type  = 'Word'.                                    "#EC NOTEXT
    ELSEIF iv_template CP '*-XLS*'.
      ev_class = 'ZCL_XTT_EXCEL_XLSX'.
      ev_type  = 'Excel'.                                   "#EC NOTEXT
    ELSEIF iv_template CP '*WORD*-XML'.
      ev_class = 'ZCL_XTT_WORD_XML'.
      ev_type  = 'Word XML'.                                "#EC NOTEXT
    ELSEIF iv_template CP '*EXCEL*-XML'.
      ev_class = 'ZCL_XTT_EXCEL_XML'.
      ev_type  = 'Excel XML'.                               "#EC NOTEXT
    ELSEIF iv_template CP '*-PDF' OR iv_template CP '*-XDP'.
      ev_class = 'ZCL_XTT_PDF'.
      ev_type  = 'Adobe PDF'.                               "#EC NOTEXT
    ELSEIF iv_template CP '*-HTM*'.
      ev_class = 'ZCL_XTT_HTML'.
      ev_type  = 'Html'.                                    "#EC NOTEXT
    ENDIF.

    " SMW0 reader
    CHECK eo_file IS REQUESTED OR eo_xtt IS REQUESTED.
    CREATE OBJECT eo_file TYPE zcl_xtt_file_smw0
      EXPORTING
        iv_objid = iv_template.

    CHECK eo_xtt IS REQUESTED.

    DATA lo_xtt TYPE REF TO object.
    CREATE OBJECT lo_xtt TYPE (ev_class)
      EXPORTING
        io_file = eo_file.
    eo_xtt ?= lo_xtt.
  ENDMETHOD.

  METHOD on_user_command.
  ENDMETHOD.

  METHOD download_template.
    CHECK io_file IS NOT INITIAL.

    " Get file content
    DATA lv_content TYPE xstring.
    io_file->get_content( IMPORTING ev_as_xstring = lv_content ).

    DATA lv_file_name TYPE string.
    lv_file_name = iv_file_name.
    IF lv_file_name IS INITIAL.
      lv_file_name = io_file->get_name( ).
    ENDIF.

    " Initialize
    DATA lo_file TYPE REF TO zcl_eui_file.
    CREATE OBJECT lo_file
      EXPORTING
        iv_xstring = lv_content.

    DATA lo_error TYPE REF TO zcx_eui_exception.
    TRY.
        lo_file->download( iv_full_path = lv_file_name ).

        " Open template
        CHECK iv_file_name IS INITIAL.
        lo_file->open( ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _make_string_message.
    CREATE DATA rr_message.
    rr_message->* = iv_info.
  ENDMETHOD.
ENDCLASS.

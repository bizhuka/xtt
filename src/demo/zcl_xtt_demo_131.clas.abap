CLASS zcl_xtt_demo_131 DEFINITION INHERITING FROM zcl_xtt_demo PUBLIC.
  PUBLIC SECTION.
    TYPES:
      " dynamic screen
      BEGIN OF ts_screen_context,
        s_pernr     TYPE RANGE OF pernr_d, " ANLAV-PERNR "for SH PREM
        p_max_count TYPE sytabix,
      END OF ts_screen_context,

      " report structure
      BEGIN OF ts_root,
        pernr   TYPE pernr_d,
        nachn   TYPE c LENGTH 40, "P0002-NACHN,
        vorna   TYPE c LENGTH 40, "P0002-VORNA,
        midnm   TYPE c LENGTH 40, "P0002-MIDNM,
        gesch   TYPE c LENGTH 1,  "P0002-GESCH,
        gbdat   TYPE d,           "P0002-GBDAT,
        perid   TYPE c LENGTH 20, "P0002-PERID,
        photo   TYPE xstring,
        s_photo TYPE string,
      END OF ts_root,
      tt_root TYPE STANDARD TABLE OF ts_root WITH DEFAULT KEY.

    METHODS:
      constructor,
*      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION,

      _get_screen_context
        RETURNING VALUE(rs_screen_context) TYPE ts_screen_context,

      _get_root
        IMPORTING
                  is_screen_context TYPE ts_screen_context
        RETURNING VALUE(rt_root)    TYPE tt_root,

      _get_photo
        IMPORTING
                  iv_pernr        TYPE pernr_d
        RETURNING VALUE(rv_photo) TYPE xstring.
ENDCLASS.


CLASS zcl_xtt_demo_131 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    v_desc = 'Shorthand for COND #( )'(131).

*  METHOD get_url_base. rv_url_base = '/xtt/cond/shorthand/'.
  ENDMETHOD.

  METHOD _get_screen_context.

  ENDMETHOD.

  METHOD _get_root.
    DATA lv_datum TYPE d.
    lv_datum = sy-datum.

    SELECT DISTINCT * " pernr nachn vorna midnm gesch gbdat perid
      INTO CORRESPONDING FIELDS OF TABLE rt_root
    FROM ('PA0002') " No HR in sandbox
    UP TO is_screen_context-p_max_count ROWS
    WHERE pernr IN is_screen_context-s_pernr[]
      AND endda >= lv_datum
      AND begda <= lv_datum
      AND sprps <> 'X'
    ORDER BY pernr.

    DATA lr_root TYPE REF TO ts_root.
    LOOP AT rt_root REFERENCE INTO lr_root.
      " All to_upper
      TRANSLATE: lr_root->nachn TO UPPER CASE,
                 lr_root->vorna TO UPPER CASE,
                 lr_root->midnm TO UPPER CASE.
      IF mo_report->mv_test_mode = abap_true.
        lr_root->photo = cl_http_utility=>decode_x_base64( lr_root->s_photo ).
      ELSE.
        lr_root->photo = _get_photo( lr_root->pernr ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_photo.
    DATA lv_objkey TYPE swotobjid-objkey.
    CONCATENATE iv_pernr '%' INTO lv_objkey.

    DATA lt_connection TYPE STANDARD TABLE OF bdn_con.
    CALL FUNCTION 'BDS_ALL_CONNECTIONS_GET'
      EXPORTING
        classname       = 'PREL'
        classtype       = 'CL'
        objkey          = lv_objkey
      TABLES
        all_connections = lt_connection
      EXCEPTIONS
        OTHERS          = 5.

    DATA lr_connection TYPE REF TO bdn_con.
    LOOP AT lt_connection REFERENCE INTO lr_connection WHERE doc_type EQ 'HRICOLFOTO' OR doc_type EQ 'HRIEMPFOTO'.
      DATA: lt_info TYPE TABLE OF scms_acinf, lr_info TYPE REF TO scms_acinf,
            lt_bin  TYPE TABLE OF sdokcntbin.
      CLEAR: lt_info, lt_bin.
      CALL FUNCTION 'SCMS_DOC_READ'
        EXPORTING
          stor_cat    = space
          crep_id     = lr_connection->contrep
          doc_id      = lr_connection->bds_docid
        TABLES
          access_info = lt_info
          content_bin = lt_bin
        EXCEPTIONS
          OTHERS      = 15.
      CHECK sy-subrc = 0 AND lt_info[] IS NOT INITIAL AND lt_bin IS NOT INITIAL.

      READ TABLE lt_info REFERENCE INTO lr_info INDEX 1.
      rv_photo = zcl_eui_conv=>binary_to_xstring( it_table  = lt_bin
                                                  iv_length = lr_info->comp_size ).
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_merge_info.
    DATA ls_screen_context TYPE ts_screen_context.
    " Test mode?
    IF mo_report->mv_test_mode = abap_true.
      ls_screen_context-p_max_count = mo_report->mv_r_cnt.
    ELSE.
      ls_screen_context = _get_screen_context( ).
      CHECK ls_screen_context IS NOT INITIAL.
    ENDIF.
    DATA lt_root TYPE tt_root.
    lt_root = _get_root( ls_screen_context ).

    mo_report->merge_add_one( lt_root ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND `ZXXT_DEMO_131-XLSX` TO rt_templates.
    APPEND `ZXXT_DEMO_131-DOCX` TO rt_templates.
  ENDMETHOD.
ENDCLASS.

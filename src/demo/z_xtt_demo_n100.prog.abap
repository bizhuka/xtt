*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_demo_100 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Images'(100).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/images/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = rs_opt-img_size = abap_true.
  ENDMETHOD.

  METHOD _get_root.
    rs_root-title = 'Test icons'(tic).

    " Get icon IDs
    SELECT id name INTO CORRESPONDING FIELDS OF TABLE rs_root-t ##too_many_itab_fields
    FROM icon UP TO p_r_cnt ROWS
    WHERE oleng = 2.

    " From business data storage
    DATA lo_bds TYPE REF TO cl_bds_document_set.
    CREATE OBJECT lo_bds.

    " Create images
    FIELD-SYMBOLS <ls_icon> TYPE ts_icon.
    LOOP AT rs_root-t ASSIGNING <ls_icon>.
      " File info
      DATA  lt_content        TYPE sbdst_content.
      DATA  lt_components     TYPE sbdst_components.
      DATA  lr_component      TYPE REF TO bapicompon.
      DATA  lv_size           TYPE i.
      DATA  lv_image          TYPE xstring.

      CLEAR lt_content.
      CLEAR lt_components.

      " Read 1 icon
      lo_bds->get_with_table(
        EXPORTING
          classname   = 'SAP_ICONS'
          classtype   = 'OT'
          object_key  = <ls_icon>-name
        CHANGING
          content     = lt_content
          components  = lt_components
        EXCEPTIONS
          OTHERS      = 1 ) .
      CHECK sy-subrc = 0.

      " Get file size
      READ TABLE lt_components REFERENCE INTO lr_component INDEX 1.
      CHECK sy-subrc = 0.
      lv_size = lr_component->comp_size.

      " As xString
      lv_image = zcl_eui_conv=>binary_to_xstring( it_table  = lt_content
                                                  iv_length = lv_size ).

      " @see code declaration (preferable way)
      IF iv_raw = abap_true.
        <ls_icon>-raw = lv_image.
        CONTINUE.
      ENDIF.

      " old approach (direct call)
      DATA lv_width  TYPE i.
      DATA lv_height TYPE i.
      IF img_size = abap_true.
        lv_width = lv_height = 200000.
      ENDIF.

      " Create new instance (for internal use only)
      <ls_icon>-img = zcl_xtt_image=>create_image( iv_image  = lv_image
                                                   iv_ext    = '.gif' "#EC NOTEXT
                                                   iv_width  = lv_width
                                                   iv_height = lv_height ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_merge_info.
    " General info
    DATA ls_root TYPE ts_root.
    ls_root = _get_root( iv_raw = abap_false ).

    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_100-XLSX'     TO rt_templates.
    APPEND 'ZXXT_DEMO_100-DOCX'     TO rt_templates.
    APPEND 'ZXXT_DEMO_100_WORD-XML' TO rt_templates.
    APPEND 'ZXXT_DEMO_100-XDP'      TO rt_templates.
  ENDMETHOD.
ENDCLASS.

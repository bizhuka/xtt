*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_xtt_styles IMPLEMENTATION.
  METHOD read_all.
    DATA lv_xml TYPE string.
    zcl_eui_conv=>xml_from_zip( EXPORTING io_zip  = io_zip
                                          iv_name = 'xl/styles.xml' "#EC NOTEXT
                                IMPORTING ev_sdoc = lv_xml ).

    t_num_fmts = _read_collection( iv_xml  = lv_xml iv_name = `numFmt` ). "#EC NOTEXT
    t_fonts    = _read_collection( iv_xml  = lv_xml iv_name = `font` ). "#EC NOTEXT
    t_fills    = _read_collection( iv_xml  = lv_xml iv_name = `fill` ). "#EC NOTEXT
    t_borders  = _read_collection( iv_xml  = lv_xml iv_name = `border` ). "#EC NOTEXT

    " Parent styles
    DATA lt_xml_match TYPE zcl_xtt_util=>tt_xml_match.
    lt_xml_match = zcl_xtt_util=>get_xml_matches( iv_context = lv_xml
                                                  iv_tag     = `cellXfs` ).
    IF lines( lt_xml_match ) <> 1.
      zcx_xtt_exception=>raise_dump( iv_message = `'styles.xml' has no styles root tag!` ). "#EC NOTEXT
    ENDIF.

    " Always 1 element
    DATA lr_xml_match TYPE REF TO zcl_xtt_util=>ts_xml_match.
    READ TABLE lt_xml_match INDEX 1 REFERENCE INTO lr_xml_match.

    t_styles = _read_collection( iv_xml  = lr_xml_match->tag iv_name = `xf` ).
  ENDMETHOD.

  METHOD save_all.
    DATA lo_xml TYPE REF TO zcl_xtt_xml_updater.
    CREATE OBJECT lo_xml
      EXPORTING
        io_zip  = io_zip
        iv_path = 'xl/styles.xml'. "#EC NOTEXT

    _paste( io_xml = lo_xml it_collection = t_num_fmts iv_root_tag   = `numFmts` iv_relat_tag  = `-fonts` ). "#EC NOTEXT
    _paste( io_xml = lo_xml it_collection = t_fonts    iv_root_tag   = `fonts` ). "#EC NOTEXT
    _paste( io_xml = lo_xml it_collection = t_fills    iv_root_tag   = `fills` ). "#EC NOTEXT
    _paste( io_xml = lo_xml it_collection = t_borders  iv_root_tag   = `borders` ). "#EC NOTEXT
    _paste( io_xml = lo_xml it_collection = t_styles   iv_root_tag   = `cellXfs` ). "#EC NOTEXT

    lo_xml->save( ).
  ENDMETHOD.

  METHOD _paste.
    DATA lo_root TYPE REF TO if_ixml_element.
    lo_root = io_xml->obj_replace( iv_tag       = iv_root_tag
                                   it_tags      = it_collection
                                   iv_tag_relat = iv_relat_tag ).
    DATA lv_count TYPE i.
    lv_count = lines( it_collection ).

    int_to_text lv_count.
    lo_root->set_attribute( name  = `count`                 "#EC NOTEXT
                            value = lv_count_txt ).
  ENDMETHOD.

  METHOD _read_collection.
    DATA lt_xml_match TYPE zcl_xtt_util=>tt_xml_match.
    lt_xml_match = zcl_xtt_util=>get_xml_matches( iv_context = iv_xml
                                                  iv_tag     = iv_name ).

    DATA lr_xml_match TYPE REF TO zcl_xtt_util=>ts_xml_match.
    LOOP AT lt_xml_match REFERENCE INTO lr_xml_match.
      INSERT lr_xml_match->tag INTO rt_collection INDEX 1. " reverse order !
    ENDLOOP.
  ENDMETHOD.

  METHOD get_style_index.
    DATA: lt_align TYPE stringtab, lv_align TYPE string.
    IF iv_align_v IS NOT INITIAL.
      CONCATENATE ` vertical="` iv_align_v `"` INTO lv_align. "#EC NOTEXT
      APPEND lv_align TO lt_align.
    ENDIF.

    IF iv_align_h IS NOT INITIAL.
      CONCATENATE ` horizontal="` iv_align_h `"` INTO lv_align. "#EC NOTEXT
      APPEND lv_align TO lt_align.
    ENDIF.

    IF lt_align IS NOT INITIAL.
      INSERT `<alignment` INTO lt_align INDEX 1.
      APPEND `/>` TO lt_align.
      CONCATENATE LINES OF lt_align INTO lv_align.
    ENDIF.

    DATA lv_style_tag TYPE string.
    CONCATENATE
        `<xf numFmtId="` iv_num_fmt_id
        `" fontId="`     iv_font_id
        `" fillId="`     iv_fill_id
        `" borderId="`   iv_border_id
        `" xfId="0" applyFill="1">`
        lv_align `</xf>` INTO lv_style_tag.

    _index_of( EXPORTING iv_tag   = lv_style_tag
               IMPORTING ev_index = rv_index
               CHANGING  ct_table = t_styles ).
  ENDMETHOD.

  METHOD get_num_format_index.
    rv_index = '0'.

    CASE abap_true.
      WHEN iv_number.
        rv_index = '4'.
        RETURN.

      WHEN iv_time.
        rv_index = '46'.
        RETURN.
    ENDCASE.

    CHECK iv_date = abap_true.

    DATA lv_num_format_tag TYPE string.
    lv_num_format_tag = _get_date_format_tag_end( ).

    " Partial search
    DATA lv_index TYPE i.
    FIND FIRST OCCURRENCE OF lv_num_format_tag IN TABLE t_num_fmts MATCH LINE lv_index.
    IF sy-subrc = 0.
      lv_index = 163 + lv_index.
    ELSE.
      lv_index = 163 + lines( t_num_fmts ) + 1.

      int_to_text lv_index.
      CONCATENATE `<numFmt numFmtId="` lv_index_txt `" ` lv_num_format_tag INTO lv_num_format_tag.
      INSERT lv_num_format_tag INTO TABLE t_num_fmts.
    ENDIF.

    int_2_text lv_index rv_index.
  ENDMETHOD.

  METHOD _get_date_format_tag_end.
    DATA lv_mask TYPE text10.
    SELECT SINGLE ddtext INTO lv_mask                   "#EC CI_NOORDER
    FROM usr01 AS u INNER JOIN dd07t AS d ON u~datfm = d~domvalue_l "#EC CI_BUFFJOIN
    WHERE u~bname      = sy-uname
      AND d~domname    = 'XUDATFM'
      AND d~ddlanguage = 'E'
      AND d~as4local   = 'A'.

    rv_tag_end = lv_mask.

    TRANSLATE rv_tag_end TO LOWER CASE.
    REPLACE ALL OCCURRENCES OF: `.` IN rv_tag_end WITH `\.`,
                                `-` IN rv_tag_end WITH `\-`.
    CONCATENATE `formatCode="` rv_tag_end `;@"/>` INTO rv_tag_end.
  ENDMETHOD.

  METHOD get_font_index.
    int_to_text iv_size.

    DATA lv_font_tag TYPE string.
    CONCATENATE
      `<font>`
      iv_opt  " Combination of <b/> <i/> <u/>
      `<sz val="` iv_size_txt `"/>`
      `<color theme="` iv_color `"/>`
      `<name val="` iv_name `"/>`
      `<family val="2"/>`
      `</font>` INTO lv_font_tag.

    _index_of( EXPORTING iv_tag   = lv_font_tag
               IMPORTING ev_index = rv_index
               CHANGING  ct_table = t_fonts ).
  ENDMETHOD.

  METHOD get_fill_index.
    DATA lv_tint_txt TYPE c LENGTH 12.
    WRITE iv_percent NO-SIGN LEFT-JUSTIFIED TO lv_tint_txt.
    IF iv_percent IS INITIAL.
      CLEAR lv_tint_txt.
    ELSEIF iv_percent < 0.
      CONCATENATE `tint="-0.` lv_tint_txt `"` INTO lv_tint_txt. "#EC NOTEXT
    ELSE.
      CONCATENATE `tint="0.` lv_tint_txt `"`  INTO lv_tint_txt. "#EC NOTEXT
    ENDIF.

    DATA lv_fill_tag TYPE string.
    CONCATENATE
      `<fill><patternFill patternType="solid">`
      `<fgColor ` lv_tint_txt ` theme="` iv_color `"/>`     "#EC NOTEXT
      `<bgColor indexed="64"/>`
      `</patternFill></fill>` INTO lv_fill_tag.

    _index_of( EXPORTING iv_tag   = lv_fill_tag
               IMPORTING ev_index = rv_index
               CHANGING  ct_table = t_fills ).
  ENDMETHOD.

  METHOD get_border_index.
    DATA lv_border_tag LIKE iv_kind.
    lv_border_tag = iv_kind.

    DATA lv_style TYPE string.
    CONCATENATE ` style="` iv_style `"` INTO lv_style.      "#EC NOTEXT

    REPLACE ALL OCCURRENCES OF `_s_` IN lv_border_tag WITH lv_style.
    _index_of( EXPORTING iv_tag   = lv_border_tag
               IMPORTING ev_index = rv_index
               CHANGING  ct_table = t_borders ).
  ENDMETHOD.

  METHOD _index_of.
    READ TABLE ct_table TRANSPORTING NO FIELDS
     WITH KEY table_line = iv_tag.
    IF sy-subrc = 0.
      ev_index = sy-tabix - 1.
    ELSE.
      ev_index = lines( ct_table ).
      INSERT iv_tag INTO TABLE ct_table.
    ENDIF.

    CONDENSE ev_index.
  ENDMETHOD.
ENDCLASS.

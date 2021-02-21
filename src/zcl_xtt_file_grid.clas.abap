class ZCL_XTT_FILE_GRID definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  interfaces ZIF_XTT_FILE .

  methods CONSTRUCTOR
    importing
      !IO_GRID type ref to CL_GUI_ALV_GRID optional
      !IT_CATALOG type LVC_T_FCAT optional
      !IT_SORT type LVC_T_SORT optional
      !IV_NAME type CSEQUENCE optional .
protected section.
private section.

  data MV_NAME type STRING .
  data MT_CATALOG type LVC_T_FCAT .
  data MT_SORT type LVC_T_SORT .
  data MO_ZIP type ref to CL_ABAP_ZIP .

  methods _GET_TEMPLATE_TABLE
    importing
      !IV_IS_TREE type ABAP_BOOL
    exporting
      !ER_TABLE type ref to DATA .
  methods _ADD_ROW
    importing
      !IR_TABLE type ref to DATA
      !IV_LEVEL type I
      !IV_SUM_ONLY type ABAP_BOOL optional
      !IT_GR_FIELD type STRINGTAB optional .
  methods _NEW_CATALOG
    importing
      !IR_TABLE type ref to DATA
    returning
      value(RT_CATALOG) type LVC_T_FCAT .
  methods _FIX_CURLY_BRACES .
  methods _ADD_STYLES_ROW
    importing
      !IO_STYLES type ref to LCL_XTT_STYLES
      !IV_IS_TREE type ABAP_BOOL
    returning
      value(RO_SHEET) type ref to IF_IXML_DOCUMENT .
  methods _ADD_GROUP_BY_CELL
    importing
      !IO_SHEET type ref to IF_IXML_DOCUMENT
      !IV_IS_TREE type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_XTT_FILE_GRID IMPLEMENTATION.


METHOD constructor.
  mv_name = iv_name.
  IF mv_name IS INITIAL.
    CONCATENATE sy-cprog '.xlsx' INTO mv_name. "#EC NOTEXT
  ENDIF.

  " If has no grid
  mt_catalog = it_catalog.
  mt_sort    = it_sort.

  " Usual way of initialization
  CHECK io_grid IS NOT INITIAL.
  io_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_catalog[] ).
  io_grid->get_sort_criteria(         IMPORTING et_sort         = mt_sort[] ).
ENDMETHOD.


METHOD zif_xtt_file~get_content.
  DELETE mt_catalog WHERE tech = 'X' OR no_out = 'X'.

  DELETE mt_sort WHERE subtot <> 'X'.
  SORT mt_sort BY spos DESCENDING.

  DATA lv_is_tree TYPE abap_bool.
  READ TABLE mt_catalog TRANSPORTING NO FIELDS
   WITH KEY do_sum = 'X'.
  IF sy-subrc = 0 OR mt_sort[] IS NOT INITIAL.
    lv_is_tree = abap_true.
  ENDIF.

  DATA lr_table TYPE REF TO data.
  _get_template_table( EXPORTING iv_is_tree = lv_is_tree
                       IMPORTING er_table   = lr_table ).

  DATA lt_string_catalog LIKE mt_catalog.
  lt_string_catalog = _new_catalog( lr_table ).

  DATA lo_salv_ex_res TYPE REF TO cl_salv_ex_result_data_table.
  CALL METHOD ('CL_SALV_EX_UTIL')=>('FACTORY_RESULT_DATA_TABLE')
    EXPORTING
      r_data              = lr_table
      t_fieldcatalog      = lt_string_catalog
    RECEIVING
      r_result_data_table = lo_salv_ex_res.

  " 7.02
  CALL METHOD ('CL_SALV_BS_LEX')=>('EXPORT_FROM_RESULT_DATA_TABLE')
    EXPORTING
      is_format            = 'xlsx' "#EC NOTEXT if_salv_bs_lex_format=>mc_format_xlsx
      ir_result_data_table = lo_salv_ex_res
    IMPORTING
      er_result_file       = ev_as_xstring. " Only as binary. No need EV_AS_STRING

**********************************************************************
  " Load zip archive from XSTRING
  CREATE OBJECT mo_zip.
  mo_zip->load( ev_as_xstring ).

  _fix_curly_braces( ).

**********************************************************************
  DATA lo_styles TYPE REF TO lcl_xtt_styles.
  CREATE OBJECT lo_styles.

  lo_styles->read_all( mo_zip ).

  " Change sheet row & cells
  DATA lo_sheet TYPE REF TO if_ixml_document.
  lo_sheet = _add_styles_row( io_styles  = lo_styles
                              iv_is_tree = lv_is_tree ).
  _add_group_by_cell( io_sheet   = lo_sheet
                      iv_is_tree = lv_is_tree ).

  lo_styles->save_all( mo_zip ).
**********************************************************************

  " write back
  ev_as_xstring = mo_zip->save( ).
ENDMETHOD.


METHOD zif_xtt_file~get_name.
  rv_name = mv_name.
ENDMETHOD.


METHOD _add_group_by_cell.
  " Save without tree option text
  IF iv_is_tree <> abap_true.
    zcl_eui_conv=>xml_to_zip( io_zip    = mo_zip
                              iv_name   = 'xl/worksheets/sheet1.xml' "#EC NOTEXT
                              io_xmldoc = io_sheet ).
    RETURN.
  ENDIF.

  DATA lv_xml TYPE string.
  zcl_eui_conv=>xml_to_str( EXPORTING io_doc = io_sheet
                            IMPORTING ev_str = lv_xml ).

**********************************************************************
  DATA lv_column_ind TYPE i.
  lv_column_ind = lines( mt_catalog ) + 3.

  DATA lv_cell_coor TYPE string.
  TRY.
      lv_cell_coor = zcl_eui_file_io=>int_2_column( lv_column_ind ).
      CONCATENATE lv_cell_coor `1` INTO lv_cell_coor.
    CATCH zcx_eui_exception.
      CLEAR lv_cell_coor.
  ENDTRY.

  " Group by fields
  DATA lt_field TYPE stringtab.
  DATA lr_sort TYPE REF TO lvc_s_sort.
  LOOP AT mt_sort REFERENCE INTO lr_sort.
    INSERT lr_sort->fieldname INTO lt_field INDEX 1. " Revers order again
  ENDLOOP.

  DATA lv_group_opt TYPE string.
  CONCATENATE LINES OF lt_field INTO lv_group_opt SEPARATED BY `,`.
  CONCATENATE `<c r="` lv_cell_coor `" t="inlineStr"><is><t>{R;group=` lv_group_opt `}</t></is></c></row>` INTO lv_group_opt.
  REPLACE FIRST OCCURRENCE OF `</row>` IN  lv_xml WITH lv_group_opt.

**********************************************************************

  " Write back
  zcl_eui_conv=>xml_to_zip( io_zip  = mo_zip
                            iv_name = 'xl/worksheets/sheet1.xml' "#EC NOTEXT
                            iv_sdoc = lv_xml ).
ENDMETHOD.


METHOD _add_row.
  FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <ls_row>   TYPE any.
  FIELD-SYMBOLS <lv_value> TYPE string.
  FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_catalog.

  ASSIGN ir_table->* TO <lt_table>.
  APPEND INITIAL LINE TO <lt_table> ASSIGNING <ls_row>.

  LOOP AT mt_catalog ASSIGNING <ls_field>. " TODO no_out,tech ?
    DATA lv_prefix TYPE string.
    CLEAR lv_prefix.

    DO 1 TIMES.
      " Level of group
      CHECK sy-tabix = 1.
      int_2_text iv_level lv_prefix.
      CONCATENATE `{R;level=` lv_prefix `}` INTO lv_prefix.

      CHECK iv_sum_only = abap_true.
      IF it_gr_field IS INITIAL.
        CONCATENATE lv_prefix 'Grand totals'(tot) INTO lv_prefix.
        CONTINUE.
      ENDIF.

      DATA lv_gr_field LIKE LINE OF it_gr_field.
      READ TABLE it_gr_field INTO lv_gr_field INDEX 1.
      CONCATENATE lv_prefix 'Totals by'(tby) ` {R-` lv_gr_field `}` INTO lv_prefix.
    ENDDO.

    ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE <ls_row> TO <lv_value>.

    DATA lv_opt TYPE string.
    CLEAR lv_opt.
    IF <ls_field>-do_sum = 'X'.
      lv_opt = ';func=SUM'. " TODO AVG COUNT?
    ENDIF.

    READ TABLE it_gr_field TRANSPORTING NO FIELDS
     WITH KEY table_line = <ls_field>-fieldname.
    IF sy-subrc = 0.
      lv_opt = ';func=FIRST'.
    ENDIF.

    DATA lv_value TYPE string.
    CONCATENATE `{R-` <ls_field>-fieldname lv_opt `}` INTO lv_value.

    " Show only totals.
    IF iv_sum_only = abap_true AND lv_opt IS INITIAL.
      CLEAR lv_value.
    ENDIF.

    CONCATENATE lv_prefix lv_value INTO <lv_value>.
  ENDLOOP.
ENDMETHOD.


METHOD _add_styles_row.
  zcl_eui_conv=>xml_from_zip( EXPORTING io_zip    = mo_zip
                                        iv_name   = 'xl/worksheets/sheet1.xml' "#EC NOTEXT
                              IMPORTING eo_xmldoc = ro_sheet ).
  DATA lv_last_cell_index TYPE i.
  lv_last_cell_index = lines( mt_catalog ).

  DATA lo_row TYPE REF TO if_ixml_element.
  lo_row ?= ro_sheet->find_from_name( 'row' ).              "#EC NOTEXT

  DATA: lv_collapse_level TYPE i.
  LOOP AT mt_sort TRANSPORTING NO FIELDS WHERE expa = 'X'.
    lv_collapse_level = sy-tabix + 2.
  ENDLOOP.

  DATA lv_percent TYPE i VALUE 100.
  WHILE lo_row IS BOUND.
    DATA: lv_row_index     TYPE i, lv_is_header TYPE abap_bool, lv_is_data_row TYPE abap_bool, lv_outline_level TYPE i.

    " Row index dependent
    lv_row_index = sy-index.
    lv_is_header = lv_is_data_row = abap_false.

    CASE lv_row_index.
      WHEN 1.
        lv_is_header   = abap_true.
      WHEN 2.
        lv_is_data_row = abap_true.
    ENDCASE.

    " '+' buttons
    lv_outline_level = lines( mt_sort ) - lv_row_index + 3.
    DO 1 TIMES.
      CHECK iv_is_tree = abap_true
        AND lv_is_header <> abap_true
        AND lv_outline_level > 0.

      int_to_text lv_outline_level.
      lo_row->set_attribute( name  = 'outlineLevel' "#EC NOTEXT
                             value = lv_outline_level_txt ).

      CHECK lv_collapse_level > lv_row_index.
      lo_row->set_attribute( name  = 'hidden'  "#EC NOTEXT
                             value = '1' ).
    ENDDO.

    DATA: lv_num_fmt_id TYPE string, lv_style_id  TYPE string, lv_font_id TYPE string, lv_border_id TYPE string, lv_fill_id TYPE string.
    lv_num_fmt_id = lv_style_id = lv_font_id = lv_border_id = lv_fill_id = '0'.

    IF lv_is_header = abap_true.
      lv_fill_id   = io_styles->get_fill_index( iv_color = io_styles->c_theme-gray ).
    ELSE.
      lv_percent   = lv_percent - 20.
      lv_fill_id   = io_styles->get_fill_index( iv_color   = io_styles->c_theme-blue
                                                iv_percent = lv_percent ).
    ENDIF.

    " Loop through excels cells
    DATA lo_cell TYPE REF TO if_ixml_element.
    lo_cell = lo_row->find_from_name( 'c' ).                "#EC NOTEXT
    WHILE lo_cell IS BOUND.
      DATA: lv_cell_index   TYPE i, lv_border_style TYPE string, lv_border_kind TYPE string.

      " Cell index dependent
      lv_cell_index = sy-index.
      lv_num_fmt_id = lv_font_id = '0'.

      CASE lv_cell_index.
        WHEN 1.
          lv_border_kind = io_styles->c_border-beg.
        WHEN lv_last_cell_index.
          lv_border_kind = io_styles->c_border-end.
        WHEN OTHERS.
          IF lv_is_data_row = abap_true.
            lv_border_kind = io_styles->c_border-all.
          ELSE.
            lv_border_kind = io_styles->c_border-mid.
          ENDIF.
      ENDCASE.

      IF lv_is_data_row <> abap_true.
        lv_font_id = io_styles->get_font_index( iv_opt = `<b/>` ).
      ENDIF.

      IF lv_is_header = abap_true.
        lv_border_style = `medium`.                         "#EC NOTEXT
      ELSE.
        lv_border_style = `thin`.                           "#EC NOTEXT

        FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_catalog.
        READ TABLE mt_catalog ASSIGNING <ls_field> INDEX lv_cell_index.
        IF <ls_field>-do_sum = 'X'.
          lv_num_fmt_id = io_styles->get_num_format_index( iv_number = abap_true ).
        ENDIF.

        CASE <ls_field>-inttype.
          WHEN cl_abap_typedescr=>typekind_date.
            lv_num_fmt_id = io_styles->get_num_format_index( iv_date = abap_true ).
          WHEN cl_abap_typedescr=>typekind_time.
            lv_num_fmt_id = io_styles->get_num_format_index( iv_time = abap_true ).
        ENDCASE.
      ENDIF.

      lv_border_id = io_styles->get_border_index( iv_kind  = lv_border_kind
                                                  iv_style = lv_border_style ).

      lv_style_id = io_styles->get_style_index( iv_num_fmt_id = lv_num_fmt_id
                                                iv_font_id    = lv_font_id
                                                iv_fill_id    = lv_fill_id
                                                iv_border_id  = lv_border_id ).
      lo_cell->set_attribute( name  = `s`
                              value = lv_style_id ).
      " Next cell
      lo_cell ?= lo_cell->get_next( ).
    ENDWHILE.

    " Next row
    lo_row ?= lo_row->get_next( ).
  ENDWHILE.
ENDMETHOD.


METHOD _fix_curly_braces.
  DATA lv_xml TYPE string.
  zcl_eui_conv=>xml_from_zip( EXPORTING io_zip  = mo_zip
                                        iv_name = 'xl/sharedStrings.xml' "#EC NOTEXT
                              IMPORTING ev_sdoc = lv_xml ).

  REPLACE ALL OCCURRENCES OF: `&#x7b;` IN lv_xml WITH `{`,
                              `&#x7d;` IN lv_xml WITH `}`,
                              `&#x3b;` IN lv_xml WITH `;`,
                              `&#x3d;` IN lv_xml WITH `=`.

  zcl_eui_conv=>xml_to_zip( io_zip    = mo_zip
                            iv_name   = 'xl/sharedStrings.xml' "#EC NOTEXT
                            iv_sdoc   = lv_xml ).
ENDMETHOD.


METHOD _get_template_table.
  " Fields of new structure
  DATA lt_comp TYPE cl_abap_structdescr=>component_table.

  FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_catalog.
  LOOP AT mt_catalog ASSIGNING <ls_field>.
    DATA ls_comp TYPE abap_componentdescr.
    ls_comp-name = <ls_field>-fieldname.
    ls_comp-type = cl_abap_elemdescr=>get_string( ).

    INSERT ls_comp INTO TABLE lt_comp.
  ENDLOOP.

  " New structure
  DATA lo_struc TYPE REF TO cl_abap_structdescr.
  lo_struc = cl_abap_structdescr=>create( lt_comp ).

  " New table
  DATA lo_table TYPE REF TO cl_abap_tabledescr.
  lo_table = cl_abap_tabledescr=>create( p_line_type = lo_struc ).

  " Result
  CREATE DATA er_table TYPE HANDLE lo_table.

  " Fill result
  DATA lv_level TYPE i.
  lv_level = lines( mt_sort ) + 1.

  _add_row( ir_table   = er_table
            iv_level   = lv_level ).

  DATA lr_sort TYPE REF TO lvc_s_sort. "#EC NEEDED  " from the current item
  DATA lr_sort_grp TYPE REF TO lvc_s_sort.

  LOOP AT mt_sort REFERENCE INTO lr_sort.
    lv_level = lv_level - 1.

    DATA lt_groups TYPE stringtab.
    CLEAR lt_groups.
    LOOP AT mt_sort REFERENCE INTO lr_sort_grp FROM sy-tabix.
      APPEND lr_sort_grp->fieldname TO lt_groups.
    ENDLOOP.

    _add_row( ir_table    = er_table
              iv_level    = lv_level
              iv_sum_only = abap_true
              it_gr_field = lt_groups ).
  ENDLOOP.

  " Totals
  CHECK iv_is_tree = abap_true.
  _add_row( ir_table    = er_table
            iv_level    = 0
            iv_sum_only = abap_true ).
ENDMETHOD.


METHOD _new_catalog.
  rt_catalog = zcl_eui_type=>get_catalog( ir_table = ir_table ).

  FIELD-SYMBOLS <ls_src>  LIKE LINE OF mt_catalog.
  FIELD-SYMBOLS <ls_dest> LIKE LINE OF rt_catalog.
  LOOP AT rt_catalog ASSIGNING <ls_dest>.
    READ TABLE mt_catalog ASSIGNING <ls_src> WITH KEY fieldname = <ls_dest>-fieldname.
    CHECK sy-subrc = 0.

    <ls_dest>-coltext = <ls_src>-coltext.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.

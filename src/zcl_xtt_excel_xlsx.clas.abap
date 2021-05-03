class ZCL_XTT_EXCEL_XLSX definition
  public
  inheriting from ZCL_XTT
  final
  create public .

public section.
  type-pools ABAP .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE .
  class-methods FORMULA_SHIFT
    importing
      !IV_REFERENCE_FORMULA type CLIKE
      !IV_SHIFT_COLS type I optional
      !IV_SHIFT_ROWS type I optional
    returning
      value(RV_RESULTING_FORMULA) type STRING
    raising
      ZCX_XTT_EXCEPTION .

  methods GET_RAW
    redefinition .
  methods MERGE
    redefinition .
protected section.

  methods ON_MATCH_FOUND
    redefinition .
private section.

  data MT_SHEETS type LCL_EX_SHEET_TAB .
  data MO_SHEET type ref to LCL_EX_SHEET .
  data MO_ZIP type ref to CL_ABAP_ZIP .
  data MT_SHARED_STRINGS type STRINGTAB .
  data _XML_XL_WORKBOOK type ref to ZCL_XTT_XML_UPDATER .
  data _XML_CONTENT_TYPES type ref to ZCL_XTT_XML_UPDATER .
  data _SHEETS_COUNTER type I value 555 ##NO_TEXT.

  methods _WORKBOOK_WRITE_XML
    importing
      !IT_TAGS_SHEETS type STRINGTAB
      !IT_TAGS_DEF_NAMES type STRINGTAB .
  methods _SHEET_MERGE
    importing
      !IV_BLOCK_NAME type CSEQUENCE
      !IT_BLOCK type ANY TABLE
    returning
      value(RV_MERGED) type ABAP_BOOL .
  methods _SHEET_SAVE
    exporting
      !ET_DEFINED_NAMES type TT_EX_DEFINED_NAME
      !ET_NEW_TAGS type STRINGTAB .
  methods SHARED_STRINGS_READ .
  methods SHARED_STRINGS_SAVE .
  class-methods COLUMN_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET .
  class-methods COLUMN_WRITE_XML
    importing
      !IT_COLUMNS type TT_EX_COLUMN
    returning
      value(RT_COLUMNS) type STRINGTAB .
  class-methods CELL_INIT
    importing
      !IS_CELL type ref to TS_EX_CELL
      !IV_COORDINATE type STRING
    exceptions
      WRONG_FORMAT .
  class-methods CELL_IS_STRING
    importing
      !IS_CELL type ref to TS_EX_CELL
    returning
      value(RV_IS_STRING) type ABAP_BOOL .
  class-methods CELL_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
      !IO_NODE type ref to IF_IXML_ELEMENT
      !IT_SHARED_STRINGS type STRINGTAB
      !IV_ROW type I .
  class-methods CELL_WRITE_NEW_ROW
    importing
      !IS_CELL type ref to TS_EX_CELL
      !IO_SHEET type ref to LCL_EX_SHEET
    changing
      !CS_TRANSMIT type TS_TRANSMIT
      !CV_CURRENT_ROW type I
      !CV_CURRENT_COL type I .
  class-methods CELL_WRITE_NEW_COL
    importing
      !IS_CELL type ref to TS_EX_CELL
      !IO_SHEET type ref to LCL_EX_SHEET
    changing
      !CT_COLUMNS type TT_EX_COLUMN
      !CV_CURRENT_COL type I .
  methods CELL_WRITE_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
      !IS_CELL type ref to TS_EX_CELL
      !IV_NEW_ROW type I
      !IV_NEW_COL_IND type I
    changing
      !CS_TRANSMIT type TS_TRANSMIT .
  class-methods ROW_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
      !IT_SHARED_STRINGS type STRINGTAB .
  class-methods ROW_WRITE_XML
    importing
      !IS_ROW type ref to TS_EX_ROW
      !IV_NEW_ROW type I
      !IV_OUTLINE_LEVEL type I
    changing
      !CS_TRANSMIT type TS_TRANSMIT .
  class-methods AREA_READ_XML
    importing
      !IV_VALUE type STRING
      !IS_AREA type ref to TS_EX_AREA optional
    changing
      !CT_AREAS type TT_EX_AREA optional .
  class-methods AREA_GET_ADDRESS
    importing
      !IS_AREA type ref to TS_EX_AREA
      !IV_NO_BUCKS type ABAP_BOOL
    returning
      value(RV_ADDRESS) type STRING .
  methods DEFINED_NAME_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
    returning
      value(RT_DEFINED_NAMES) type TT_EX_DEFINED_NAME .
  class-methods DEFINED_NAME_SAVE_XML
    importing
      !IT_DEFINED_NAMES type TT_EX_DEFINED_NAME
    returning
      value(RT_NEW_NAME) type STRINGTAB .
  methods LIST_OBJECT_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET .
  methods LIST_OBJECT_SAVE_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
      !IV_SID type I .
  class-methods DATA_VALIDATION_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET .
  class-methods DATA_VALIDATION_SAVE_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET .
  class-methods GET_WITHOUT_T
    importing
      !IV_TEXT type STRING
    returning
      value(RV_TEXT) type STRING .
  methods DRAWING_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
    returning
      value(RR_ME) type ref to TS_DRAWING .
  methods DRAWING_ADD
    importing
      !IR_ME type ref to TS_DRAWING
      !IR_CELL type ref to TS_EX_CELL
      value(IV_NEW_ROW) type I
      value(IV_NEW_COL) type I .
  class-methods DRAWING_GET_IMAGE_TEMPLATE
    importing
      !IR_ME type ref to TS_DRAWING
      !IR_CELL type ref to TS_EX_CELL
      value(IV_NEW_ROW) type I
      value(IV_NEW_COL) type I
    returning
      value(RV_COMP_VALUE) type STRING .
  class-methods DRAWING_GET_IMAGE_BASIC
    importing
      !IR_CELL type ref to TS_EX_CELL
      value(IV_NEW_ROW) type I
      value(IV_NEW_COL) type I
    returning
      value(RV_COMP_VALUE) type STRING .
  methods DRAWING_SAVE_XML
    importing
      !IR_ME type ref to TS_DRAWING
      !IV_PATH type STRING
      !IV_PATH_REL type STRING
      !IO_XML_XL_WORKSHEET type ref to ZCL_XTT_XML_UPDATER
      !IO_XML_SHEET_RELS type ref to ZCL_XTT_XML_UPDATER .
ENDCLASS.



CLASS ZCL_XTT_EXCEL_XLSX IMPLEMENTATION.


METHOD area_get_address.
  DATA:
    ls_cell TYPE REF TO ts_ex_cell,
    l_row   TYPE char10,
    l_part  TYPE string.

  " Oops
  IF is_area->a_cells IS INITIAL.
    rv_address = is_area->a_original_value.
    RETURN.
  ENDIF.

  CLEAR rv_address.
  LOOP AT is_area->a_cells REFERENCE INTO ls_cell.
    " As string
    int_2_text ls_cell->c_row l_row.

    " $ sign
    IF iv_no_bucks = abap_true.
      CONCATENATE     ls_cell->c_col     l_row INTO l_part.
    ELSE.
      CONCATENATE `$` ls_cell->c_col `$` l_row INTO l_part.
    ENDIF.

    " Concatenate 2 cells
    IF rv_address IS INITIAL.
      rv_address = l_part.
    ELSE.
      CONCATENATE rv_address `:` l_part INTO rv_address.
    ENDIF.
  ENDLOOP.

  " Add sheet name
  CHECK is_area->a_sheet_name IS NOT INITIAL.

  DATA lv_name TYPE string.
  lv_name = is_area->a_sheet_name.

  " Duplicate '
  REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

  " Surround with '
  CONCATENATE `'` lv_name `'!` rv_address INTO rv_address.
ENDMETHOD.                    "area_get_address


METHOD area_read_xml.
  DATA:
    ls_area TYPE REF TO ts_ex_area,
    l_len   TYPE i,
    lt_ref  TYPE stringtab,
    l_name  TYPE string,
    l_ref   TYPE string,
    ls_cell TYPE REF TO ts_ex_cell.

  " Change existing
  IF is_area IS SUPPLIED.
    ls_area = is_area.
  ELSE. " Add new one
    APPEND INITIAL LINE TO ct_areas REFERENCE INTO ls_area.
  ENDIF.

  " Save previous value
  ls_area->a_original_value = iv_value.

  " Sheet name and address
  SPLIT iv_value AT '!' INTO l_name l_ref.

  " No sheet name
  IF l_ref IS INITIAL.
    l_ref = iv_value.
  ELSE.
    ls_area->a_sheet_name = l_name.

    " Delete ''
    REPLACE ALL OCCURRENCES OF `''` IN ls_area->a_sheet_name WITH `'`.

    " Delete '
    IF ls_area->a_sheet_name(1) = `'`.
      l_len = strlen( ls_area->a_sheet_name ) - 2.
      ls_area->a_sheet_name = ls_area->a_sheet_name+1(l_len).
    ENDIF.
  ENDIF.

  " Broken link
  CHECK l_ref(1) <> '#'.

  " Remove all the dollars :)
  REPLACE ALL OCCURRENCES OF `$` IN l_ref WITH ``.

  " 1 or 2 cells
  SPLIT l_ref AT ':' INTO TABLE lt_ref.
  LOOP AT lt_ref INTO l_ref.
    " Save references !
    CREATE DATA ls_cell.

    " Fill with values
    cell_init(
     EXPORTING
       iv_coordinate = l_ref
       is_cell       = ls_cell
     EXCEPTIONS
       wrong_format  = 1 ).

    " Add if ok
    IF sy-subrc = 0.
      APPEND ls_cell->* TO ls_area->a_cells.
    ENDIF.
  ENDLOOP.
ENDMETHOD.                    "area_read_xml


METHOD cell_init.
  DATA:
    l_len  TYPE i,
    l_off  TYPE i,
    l_char TYPE char1.

  l_len = strlen( iv_coordinate ).
  DO l_len TIMES.
    l_off  = l_len - sy-index.
    l_char = iv_coordinate+l_off(1).

    " Find last pos
    IF l_char < '0' OR l_char > '9'.
      l_off = l_off + 1.
      EXIT.
    ENDIF.
  ENDDO.

  " Oops
  IF l_off = 0 OR l_off = l_len.
    RAISE wrong_format.
  ENDIF.

  " Row
  l_len          = l_len - l_off.
  is_cell->c_row = iv_coordinate+l_off(l_len).

  " Column
  is_cell->c_col     = iv_coordinate(l_off).
  is_cell->c_col_ind = zcl_eui_file_io=>column_2_int( is_cell->c_col ).
ENDMETHOD.                    "cell_init


METHOD cell_is_string.
  " Different behavior for string
  IF is_cell->c_value IS NOT INITIAL AND is_cell->c_formula IS INITIAL AND
     is_cell->c_type  IS NOT INITIAL AND is_cell->c_type <> 'b'.
    rv_is_string = abap_true.
  ELSE.
    rv_is_string = abap_false.
  ENDIF.
ENDMETHOD.                    "cell_is_string


METHOD cell_read_xml.
  DATA: ls_cell TYPE REF TO ts_ex_cell,
        lo_elem TYPE REF TO if_ixml_element,
        l_ind   TYPE i.
  DATA  lo_xtt_error TYPE REF TO zcx_xtt_exception.

  " Transform coordinates
  DATA l_cell_ref TYPE string.
  l_cell_ref = io_node->get_attribute( 'r' ).
  IF l_cell_ref IS INITIAL.
    MESSAGE w026(zsy_xtt) WITH iv_row INTO sy-msgli.
    io_sheet->mo_xlsx->add_log_message( iv_syst = abap_true ).
    RETURN.
  ENDIF.

  " Insert new one
  APPEND INITIAL LINE TO io_sheet->mt_cells REFERENCE INTO ls_cell.
  cell_init(
   iv_coordinate = l_cell_ref
   is_cell       = ls_cell ).

  ls_cell->c_type  = io_node->get_attribute( 't' ).
  ls_cell->c_style = io_node->get_attribute( 's' ).

  " General formula
  lo_elem = io_node->find_from_name( name = 'f' ).
  IF lo_elem IS BOUND.
    " Unescape fm
    DATA l_val TYPE string.
    l_val = lo_elem->get_value( ).
    l_val = cl_http_utility=>escape_html( l_val ).

    " Shared formula
    DO 1 TIMES.
      DATA ls_shared_fm TYPE ts_shared_fm.
      ls_shared_fm-sf_index = lo_elem->get_attribute( 'si' ).
      CHECK ls_shared_fm-sf_index IS NOT INITIAL.

      " Save 1-st cell with formula
      IF l_val IS NOT INITIAL.
        ls_shared_fm-sf_cell = ls_cell.
        INSERT ls_shared_fm INTO TABLE io_sheet->mt_shared_fm.
        CONTINUE.
      ENDIF.

      " Read 1-st shared formula
      READ TABLE io_sheet->mt_shared_fm INTO ls_shared_fm
       WITH TABLE KEY sf_index = ls_shared_fm-sf_index.
      CHECK sy-subrc = 0.

      " Calc offsets
      DATA lv_r_dx TYPE i.
      DATA lv_c_dx TYPE i.
      lv_r_dx = ls_cell->c_row     - ls_shared_fm-sf_cell->c_row.
      lv_c_dx = ls_cell->c_col_ind - ls_shared_fm-sf_cell->c_col_ind.

      TRY.
          l_val = formula_shift(
            iv_reference_formula = ls_shared_fm-sf_cell->c_formula
            iv_shift_rows        = lv_r_dx
            iv_shift_cols        = lv_c_dx ).
        CATCH zcx_xtt_exception INTO lo_xtt_error.
          zcx_eui_exception=>raise_dump( io_error = lo_xtt_error ).
      ENDTRY.
    ENDDO.

    " Set fm
    ls_cell->c_formula = l_val.

    " Array FM ?
    DO 1 TIMES.
      l_val = lo_elem->get_attribute( 't' ).
      CHECK l_val = 'array'.

      l_val = lo_elem->get_attribute( 'ref' ).
      CHECK l_val = l_cell_ref.

      ls_cell->c_formula_is_arr = abap_true.
    ENDDO.
    RETURN.
  ENDIF.

  " Default value
  lo_elem = io_node->find_from_name( name = 'v' ).
  IF lo_elem IS BOUND.
    l_val = lo_elem->get_value( ).
  ELSE.
    l_val = io_node->get_value( ).  " TODO 'inlineStr' !!! with tags!
  ENDIF.

  CASE ls_cell->c_type.
    WHEN 's'.
      l_ind = l_val + 1.
      READ TABLE it_shared_strings INTO ls_cell->c_value INDEX l_ind.

    WHEN 'inlineStr'.
      ls_cell->c_value = l_val. " <si> tag get_without_t(  ).
      ls_cell->c_type  = 's'.

    WHEN OTHERS.
      ls_cell->c_value = l_val.

  ENDCASE.
ENDMETHOD.                    "cell_read_xml


METHOD cell_write_new_col.
  " New column
  cv_current_col = cv_current_col + is_cell->c_col_dx.

  " Existing old column
  DATA ls_column TYPE ts_ex_column.
  READ TABLE io_sheet->mt_columns INTO ls_column
   WITH TABLE KEY min = is_cell->c_col_ind.

  " Prog error
  CHECK sy-subrc = 0.

  IF ls_column-outline_skip = abap_true.
    CLEAR ls_column-outlinelevel.
  ENDIF.

  " New outline
  IF is_cell->c_column_outline IS NOT INITIAL.
    int_2_text is_cell->c_column_outline ls_column-outlinelevel.
  ENDIF.

  IF cv_current_col <> is_cell->c_col_ind.
    ls_column-min = ls_column-max = cv_current_col.
  ENDIF.
  INSERT ls_column INTO TABLE ct_columns.
ENDMETHOD.


METHOD cell_write_new_row.
  " New row index as a string
  cv_current_row = cv_current_row + is_cell->c_row_dx.

  " New row
  CHECK is_cell->c_row_dx > 0.

  " New column!
  cv_current_col = 0.

  " Closing tag
  IF cs_transmit-rows_cells IS NOT INITIAL.
    APPEND `</row>` TO cs_transmit-rows_cells.
  ENDIF.

  " Read by previous key
  DATA ls_row TYPE REF TO ts_ex_row.
  READ TABLE io_sheet->mt_rows WITH TABLE KEY r = is_cell->c_row REFERENCE INTO ls_row.
  IF sy-subrc = 0.
    zcl_xtt_excel_xlsx=>row_write_xml(
     EXPORTING
      is_row           = ls_row
      iv_new_row       = cv_current_row
      iv_outline_level = is_cell->c_row_outline
     CHANGING
      cs_transmit      = cs_transmit ).
  ELSE.
    " New row ref
    DATA ls_blank_row TYPE REF TO ts_ex_row.
    CREATE DATA ls_blank_row.

    ls_blank_row->r = is_cell->c_row.
    zcl_xtt_excel_xlsx=>row_write_xml(
     EXPORTING
      is_row           = ls_blank_row
      iv_new_row       = cv_current_row
      iv_outline_level = is_cell->c_row_outline
     CHANGING
      cs_transmit      = cs_transmit ).
  ENDIF.
ENDMETHOD.


METHOD cell_write_xml.
  DATA:
    lv_new_col   TYPE char3,
    lv_number    TYPE char10,
    lv_data      TYPE string,
    lv_index     TYPE i,
    lv_merge_col TYPE char3,
    lv_from      TYPE string,
    lv_to        TYPE string,
    lo_error     TYPE REF TO zcx_eui_exception.

  " Update formula. Only for backward compatibility?
  DEFINE replace_all_with_buck.
    CONCATENATE:
     `$` &1 INTO lv_from,
     `$` &2 INTO lv_to.

    REPLACE ALL OCCURRENCES OF lv_from IN is_cell->c_formula WITH lv_to.
  END-OF-DEFINITION.

  " To text
  int_to_text iv_new_row.
  TRY.
      lv_new_col = zcl_eui_file_io=>int_2_column( iv_new_col_ind ).
    CATCH zcx_eui_exception INTO lo_error.
      zcx_xtt_exception=>raise_dump( io_error = lo_error ).
  ENDTRY.

  " Formula
  IF is_cell->c_formula IS NOT INITIAL.
    " Replace to new row index. Only for backward compatibility
    FIND FIRST OCCURRENCE OF '$' IN is_cell->c_formula.
    IF sy-subrc = 0.
      " Old row index -> New row index
      int_2_text is_cell->c_row lv_number.

      " Row
      replace_all_with_buck lv_number        iv_new_row_txt.
      " Or Column with $
      IF sy-subrc <> 0.
        replace_all_with_buck is_cell->c_col lv_new_col.
      ENDIF.
    ENDIF.

    " Formula in ListObjects
    IF io_sheet->_is_new( ) = abap_true AND cs_transmit-iv_sid >= 0.
      FIELD-SYMBOLS <ls_list_object> LIKE LINE OF io_sheet->mt_list_objects.
      LOOP AT io_sheet->mt_list_objects ASSIGNING <ls_list_object>.
        CHECK is_cell->c_formula CS <ls_list_object>-name_fm_mask. "  -> _lo_name && '['

        DATA lv_tab_name TYPE string.
        lv_tab_name = io_sheet->get_new_name( <ls_list_object>-_lo_name ).
        CONCATENATE lv_tab_name '[' INTO lv_tab_name.

        REPLACE ALL OCCURRENCES OF <ls_list_object>-name_fm_mask IN is_cell->c_formula
                                   WITH lv_tab_name.
      ENDLOOP.
    ENDIF.

    " TODO test formula_shift

    " Write formula
    lv_data = `<f`.

    IF is_cell->c_formula_is_arr = abap_true.
      CONCATENATE lv_data ` t="array"` " is_cell->c_formula_type
                          ` ref="` lv_new_col iv_new_row_txt `"`
             INTO lv_data.
    ENDIF.

    CONCATENATE lv_data `>` is_cell->c_formula `</f>` INTO lv_data.
  ELSEIF is_cell->c_value IS NOT INITIAL.
    " String
    IF cell_is_string( is_cell ) = abap_true.
      READ TABLE mt_shared_strings TRANSPORTING NO FIELDS BINARY SEARCH
       WITH KEY table_line = is_cell->c_value.
      " Write index of strings
      lv_index = sy-tabix - 1.
      int_2_text lv_index lv_number.
      CONCATENATE `<v>` lv_number        `</v>` INTO lv_data.
    ELSE. " Numbers
      CONCATENATE `<v>` is_cell->c_value `</v>` INTO lv_data.
    ENDIF.
  ENDIF.

  " No need is empty
  CHECK lv_data IS NOT INITIAL
     OR is_cell->c_style IS NOT INITIAL
     OR is_cell->c_type  IS NOT INITIAL
     OR is_cell->c_merge_row_dx IS NOT INITIAL.

  " Change current row
  is_cell->c_row     = iv_new_row.
  is_cell->c_col     = lv_new_col.
  is_cell->c_col_ind = iv_new_col_ind.

  " Address
  DATA lv_cell_tag TYPE string.
  CONCATENATE `<c r="` is_cell->c_col iv_new_row_txt `"` INTO lv_cell_tag.

  " Style and type
  IF is_cell->c_style IS NOT INITIAL.
    CONCATENATE lv_cell_tag ` s="` is_cell->c_style `"` INTO lv_cell_tag.
  ENDIF.
  IF is_cell->c_type IS NOT INITIAL.
    CONCATENATE lv_cell_tag ` t="` is_cell->c_type  `"` INTO lv_cell_tag.
  ENDIF.

  " Data
  CONCATENATE lv_cell_tag `>` lv_data `</c>` INTO lv_cell_tag.
  APPEND lv_cell_tag TO cs_transmit-rows_cells.

  " Merged cell
  IF is_cell->c_merge_row_dx IS NOT INITIAL OR
     is_cell->c_merge_col_dx IS NOT INITIAL.

    " Calculate new val
    lv_index = is_cell->c_row     + is_cell->c_merge_row_dx.
    int_2_text lv_index lv_number.

    lv_index = is_cell->c_col_ind + is_cell->c_merge_col_dx.
    TRY.
        lv_merge_col = zcl_eui_file_io=>int_2_column( lv_index  ).
      CATCH zcx_eui_exception INTO lo_error.
        zcx_xtt_exception=>raise_dump( io_error = lo_error ).
    ENDTRY.

    DATA lv_merge_cell TYPE string.
    CONCATENATE `<mergeCell ref="`
      is_cell->c_col  iv_new_row_txt `:`
      lv_merge_col    lv_number `"/> ` INTO lv_merge_cell.
    APPEND lv_merge_cell TO cs_transmit-merge_cells.
  ENDIF.
ENDMETHOD.


METHOD column_read_xml.
  DATA lo_col TYPE REF TO if_ixml_element.
  DATA ls_col LIKE LINE OF io_sheet->mt_columns.
  DATA lv_cnt TYPE i.

  lo_col ?= io_sheet->mo_document->find_from_name( 'col' ).             "#EC NOTEXT
  WHILE lo_col IS BOUND.
    " read new col
    ls_col-collapsed    = lo_col->get_attribute( 'collapsed' ).    "#EC NOTEXT
    ls_col-customwidth  = lo_col->get_attribute( 'customWidth' ).  "#EC NOTEXT
    ls_col-hidden       = lo_col->get_attribute( 'hidden' ).       "#EC NOTEXT
    ls_col-max          = lo_col->get_attribute( 'max' ).          "#EC NOTEXT
    ls_col-min          = lo_col->get_attribute( 'min' ).          "#EC NOTEXT
    ls_col-outlinelevel = lo_col->get_attribute( 'outlineLevel' ). "#EC NOTEXT
    ls_col-phonetic     = lo_col->get_attribute( 'phonetic' ).     "#EC NOTEXT
    ls_col-style        = lo_col->get_attribute( 'style' ).        "#EC NOTEXT
    ls_col-width        = lo_col->get_attribute( 'width' ).        "#EC NOTEXT

    " From = To
    IF ls_col-min = ls_col-max.
      INSERT ls_col INTO TABLE io_sheet->mt_columns.
    ELSE.
      " Create several columns
      lv_cnt = ls_col-max - ls_col-min + 1.

      " Each column separately
      DO lv_cnt TIMES.
        ls_col-max = ls_col-min.
        INSERT ls_col INTO TABLE io_sheet->mt_columns.

        " Next line
        ls_col-min = ls_col-min + 1.
      ENDDO.
    ENDIF.

    lo_col ?= lo_col->get_next( ).
  ENDWHILE.
ENDMETHOD.


METHOD column_write_xml.
  " All columns as new tags
  FIELD-SYMBOLS <ls_item> LIKE LINE OF it_columns.
  LOOP AT it_columns ASSIGNING <ls_item>.
    " Result tag
    DATA: lv_column TYPE string, lv_text TYPE string.
    lv_column = `<col`.

    add_attr lv_column <ls_item>-collapsed    'collapsed'.    "#EC NOTEXT
    add_attr lv_column <ls_item>-customwidth  'customWidth'.  "#EC NOTEXT
    add_attr lv_column <ls_item>-hidden       'hidden'.       "#EC NOTEXT
    add_attr lv_column <ls_item>-max          'max'.          "#EC NOTEXT
    add_attr lv_column <ls_item>-min          'min'.          "#EC NOTEXT
    add_attr lv_column <ls_item>-outlinelevel 'outlineLevel'. "#EC NOTEXT
    add_attr lv_column <ls_item>-phonetic     'phonetic'.     "#EC NOTEXT
    add_attr lv_column <ls_item>-style        'style'.        "#EC NOTEXT
    add_attr lv_column <ls_item>-width        'width'.        "#EC NOTEXT
    CONCATENATE lv_column `></col>` INTO lv_column RESPECTING BLANKS. "#EC NOTEXT

    APPEND lv_column TO rt_columns.
  ENDLOOP.
ENDMETHOD.


METHOD constructor.
  super->constructor( io_file = io_file ).
  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  TRY.
      DATA l_x_value TYPE xstring.
      io_file->get_content( IMPORTING ev_as_xstring = l_x_value ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      add_log_message( io_exception = lo_no_check ).
      RETURN.
  ENDTRY.

  " Load zip archive from XSTRING
  CREATE OBJECT mo_zip.
  mo_zip->load( l_x_value ).

***************************************
  shared_strings_read( ).

***************************************
  CREATE OBJECT:
   _xml_xl_workbook   EXPORTING io_zip  = mo_zip iv_path = 'xl/workbook.xml', "#EC NOTEXT

   _xml_content_types EXPORTING io_zip  = mo_zip iv_path = '[Content_Types].xml' "#EC NOTEXT
     " Append mode for file
      iv_str_tag = `Types`.                                  "#EC NOTEXT

  DATA lo_workbook TYPE REF TO if_ixml_document.
  lo_workbook = _xml_xl_workbook->obj_get_document( ).

  IF lo_workbook IS INITIAL.
    add_log_message( iv_text  = `The xlsx archive is broken` "#EC NOTEXT
                     iv_msgty = 'E' ).
    CLEAR mo_zip.
    RETURN.
  ENDIF.

***************************************
  " Read sheets
  DATA lo_sheet TYPE REF TO lcl_ex_sheet.
  DATA lo_node  TYPE REF TO if_ixml_element.

  lo_node = lo_workbook->find_from_name( 'sheet' ).         "#EC NOTEXT
  WHILE lo_node IS BOUND.
    DATA lv_index TYPE string.
    int_2_text sy-index lv_index.

    " Prepare and add
    CREATE OBJECT lo_sheet
      EXPORTING
        iv_index = lv_index
        io_xlsx  = me.
    lo_sheet->defined_names_read( io_node = lo_node ).
    APPEND lo_sheet TO mt_sheets.

    " Next
    lo_node ?= lo_node->get_next( ).
  ENDWHILE.
ENDMETHOD.                    "constructor


METHOD data_validation_read_xml.
  DATA lo_data_valid   TYPE REF TO if_ixml_element.
  DATA ls_area         TYPE REF TO ts_ex_area.
  DATA ls_cell         TYPE REF TO ts_ex_cell.
  DATA l_val           TYPE string.

  lo_data_valid ?= io_sheet->mo_document->find_from_name( 'dataValidation' ). "#EC NOTEXT
  WHILE lo_data_valid IS BOUND.
    APPEND INITIAL LINE TO io_sheet->mt_data_valid REFERENCE INTO ls_area.

    " Create new area
    l_val = lo_data_valid->get_attribute( 'sqref' ).        "#EC NOTEXT
    area_read_xml(
       iv_value = l_val
       is_area  = ls_area ).

    LOOP AT ls_area->a_cells REFERENCE INTO ls_cell.
      " Get existing cell Or insert new one
      io_sheet->find_cell( ls_cell->* ).
    ENDLOOP.

    " Next
    lo_data_valid ?= lo_data_valid->get_next( ).
  ENDWHILE.
ENDMETHOD.


METHOD data_validation_save_xml.
  DATA lo_data_valid   TYPE REF TO if_ixml_element.
  DATA ls_area         TYPE REF TO ts_ex_area.
  DATA lv_address      TYPE string.

  CHECK io_sheet->mt_data_valid IS NOT INITIAL.

  lo_data_valid ?= io_sheet->mo_document->find_from_name( 'dataValidation' ). "#EC NOTEXT
  LOOP AT io_sheet->mt_data_valid REFERENCE INTO ls_area.
    CHECK lo_data_valid IS NOT INITIAL.

    " Change
    io_sheet->replace_with_new( ir_area = ls_area ).

    lv_address = zcl_xtt_excel_xlsx=>area_get_address(
      is_area     = ls_area
      iv_no_bucks = abap_true ).

    lo_data_valid->set_attribute( name = 'sqref' value = lv_address ). "#EC NOTEXT

    " Next
    lo_data_valid ?= lo_data_valid->get_next( ).
  ENDLOOP.
ENDMETHOD.


METHOD defined_name_read_xml.
  DATA lo_node         TYPE REF TO if_ixml_element.
  DATA l_value         TYPE string.
  DATA ls_defined_name TYPE ts_ex_defined_name.
  DATA lt_value        TYPE stringtab.

  DATA lo_workbook TYPE REF TO if_ixml_document.
  lo_workbook = _xml_xl_workbook->obj_get_document( ).
  lo_node ?= lo_workbook->find_from_name( 'definedName' ).

  " Name & value
  WHILE lo_node IS BOUND.
    CLEAR ls_defined_name.

    " Regardless CHECK l_value IS NOT INITIAL AND l_value(1) <> '#'.
    ls_defined_name-d_name      = lo_node->get_attribute( 'name' ). "#EC NOTEXT
    ls_defined_name-d_local_sid = lo_node->get_attribute( 'localSheetId' ). "#EC NOTEXT
    l_value = lo_node->get_value( ).

    " Several areas
    CLEAR lt_value.
    SPLIT l_value AT ',' INTO TABLE lt_value.
    LOOP AT lt_value INTO l_value.
      area_read_xml( EXPORTING iv_value = l_value
                     CHANGING  ct_areas = ls_defined_name-d_areas ).
    ENDLOOP.

    " Related to sheet ?
    DELETE ls_defined_name-d_areas WHERE a_sheet_name <> io_sheet->_name.

    " Create new defined name
    IF ls_defined_name-d_areas IS NOT INITIAL.
      INSERT ls_defined_name INTO TABLE rt_defined_names.
    ENDIF.

    lo_node ?= lo_node->get_next( ).
  ENDWHILE.
ENDMETHOD.


METHOD defined_name_save_xml.
  " New tag
  DATA lv_name         TYPE string.
  DATA l_ref           TYPE string.
  DATA ls_area         TYPE REF TO ts_ex_area.
  DATA l_part          TYPE string.
  DATA lv_text         TYPE string.
  FIELD-SYMBOLS:
   <ls_item> LIKE LINE OF IT_DEFINED_NAMES.

  LOOP AT it_defined_names ASSIGNING <ls_item>.

    " Modify existing
    CLEAR l_ref.
    LOOP AT <ls_item>-d_areas REFERENCE INTO ls_area.
      l_part = area_get_address( is_area     = ls_area
                                 iv_no_bucks = abap_false ).
      CHECK l_part IS NOT INITIAL.

      " Concatenate 2 cells
      IF l_ref IS INITIAL.
        l_ref = l_part.
      ELSE.
        CONCATENATE l_ref `,` l_part INTO l_ref.
      ENDIF.
    ENDLOOP.

    lv_name = `<definedName`.
    add_attr lv_name <ls_item>-d_name      `name`.            "#EC NOTEXT
    add_attr lv_name <ls_item>-d_local_sid `localSheetId`.    "#EC NOTEXT

    CONCATENATE lv_name  `>` l_ref `</definedName>` INTO lv_name RESPECTING BLANKS.
    APPEND lv_name TO rt_new_name.
  ENDLOOP.
ENDMETHOD.


METHOD drawing_add.
  " No need
  DATA lo_image LIKE ir_cell->c_image.
  lo_image = ir_cell->c_image.
  CHECK lo_image IS NOT INITIAL.

  " Obligatory text
  INSERT
   `<Override PartName="/xl/drawings/drawing1.xml" ContentType="application/vnd.openxmlformats-officedocument.drawing+xml"/>`
  INTO TABLE ir_me->dr_t_required.

  DATA lv_file_name TYPE string.
  DATA lv_mime_text TYPE string.
  DATA lv_rewrite   TYPE abap_bool.

  lo_image->save_in_archive(
   EXPORTING io_zip         = mo_zip
             iv_prefix      = 'xl/media/'                   "#EC NOTEXT
   IMPORTING ev_file_name   = lv_file_name
             ev_mime_text   = lv_mime_text
             ev_rewrite     = lv_rewrite ).

  " Read mime type ?
  INSERT lv_mime_text INTO TABLE ir_me->dr_t_required.


  " Is there an image template ?
  DATA lv_value TYPE string.
  lv_value = drawing_get_image_template( ir_me      = ir_me
                                         ir_cell    = ir_cell
                                         iv_new_row = iv_new_row
                                         iv_new_col = iv_new_col ).
  " Create from scratch
  IF lv_value IS INITIAL.
    lv_value = drawing_get_image_basic( ir_cell    = ir_cell
                                        iv_new_row = iv_new_row
                                        iv_new_col = iv_new_col ).
  ENDIF.

  " №1 - Add Image
  ir_me->_dr_xml_xl_drawings->str_add( lv_value ).

  " №2 - Add Image Relation
  CHECK lv_rewrite <> abap_true.
  CONCATENATE
    `<Relationship Id="_P`
    lo_image->mv_index_txt
    `" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/`
    lv_file_name `"/>` INTO lv_value.
  ir_me->_dr_xml_xl_drawings_rels->str_add( lv_value ).
ENDMETHOD.


METHOD drawing_get_image_basic.
  " Get comp ref
  DATA lo_image LIKE ir_cell->c_image.
  lo_image = ir_cell->c_image.

  " 1 cell
  DATA lv_row_dx TYPE i VALUE 1.
  DATA lv_col_dx TYPE i VALUE 1.

  " Calc end row
  ADD -1 TO iv_new_row.
  lv_row_dx = iv_new_row + lv_row_dx.

  " Calc end column
  ADD -1 TO iv_new_col.
  lv_col_dx = iv_new_col + lv_col_dx.

  DATA lv_width_txt  TYPE string.
  DATA lv_height_txt TYPE string.

  " Create string
  int_to_text iv_new_row.
  int_to_text iv_new_col.
  int_to_text lv_row_dx.
  int_to_text lv_col_dx.
  int_2_text lo_image->mv_width  lv_width_txt.
  int_2_text lo_image->mv_height lv_height_txt.

**********************************************************************
  " twoCellAnchor if IMAGE matches the cell
  DATA lv_tag   TYPE string.

  IF lo_image->mv_width IS INITIAL AND lo_image->mv_height IS INITIAL.
    lv_tag = `twoCellAnchor`.
    CONCATENATE
        `<xdr:to><xdr:col>` lv_col_dx_txt `</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>` lv_row_dx_txt `</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>`
     INTO rv_comp_value.
  ELSE.
    " Specify Width & Height
    lv_tag = `oneCellAnchor`.
    CONCATENATE `<xdr:ext cx="` lv_width_txt `" cy="` lv_height_txt `"/>`
     INTO rv_comp_value.
  ENDIF.

  " Image without any template
  CONCATENATE
    `<xdr:` lv_tag `>`
    `<xdr:from><xdr:col>` iv_new_col_txt `</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>`  iv_new_row_txt `</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>`
    rv_comp_value
    `<xdr:pic>`
    `<xdr:nvPicPr><xdr:cNvPr id="` lo_image->mv_index_txt `" name="Object_` lo_image->mv_index_txt `"/><xdr:cNvPicPr><a:picLocks noChangeAspect="1"/></xdr:cNvPicPr></xdr:nvPicPr>`
    `<xdr:blipFill><a:blip xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" r:embed="_P` lo_image->mv_index_txt `"/><a:stretch><a:fillRect/></a:stretch></xdr:blipFill>`
    `<xdr:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></xdr:spPr>`
    `</xdr:pic>`
    `<xdr:clientData/>`
    `</xdr:` lv_tag `>` INTO rv_comp_value.
ENDMETHOD.


METHOD drawing_get_image_template.
  DATA lr_img_template TYPE REF TO ts_img_template.
  READ TABLE ir_me->dr_t_img_template REFERENCE INTO lr_img_template
    WITH TABLE KEY row = ir_cell->c_row
                   col = ir_cell->c_col_ind.
  CHECK sy-subrc = 0.

  " Copy entire tag
  rv_comp_value = lr_img_template->tag.

  " Get comp ref
  DATA lo_image LIKE ir_cell->c_image.
  lo_image = ir_cell->c_image.

  " Change id
  DATA lv_new_txt TYPE string.
  DATA lv_row_1 TYPE i.
  DATA lv_col_1 TYPE i.

  " Name is optional field
  CONCATENATE `NAME_ID_` lo_image->mv_index_txt INTO lv_new_txt.
  REPLACE FIRST OCCURRENCE OF `{NAME_ID_0}` IN rv_comp_value WITH lv_new_txt.

  " Ref to another files
  CONCATENATE ` r:embed="_P` lo_image->mv_index_txt `" old_id="` INTO lv_new_txt.
  REPLACE FIRST OCCURRENCE OF ` r:embed="` IN rv_comp_value WITH lv_new_txt. "#EC NOTEXT

  " Row
  SUBTRACT 1 FROM iv_new_row.
  int_to_text iv_new_row.
  CONCATENATE `<xdr:row>` iv_new_row_txt `</xdr:row>` INTO lv_new_txt.
  REPLACE FIRST OCCURRENCE OF `<xdr:row>ROW_0</xdr:row>` IN rv_comp_value WITH lv_new_txt.

  " Column
  SUBTRACT 1 FROM iv_new_col.
  int_to_text iv_new_col.
  CONCATENATE `<xdr:col>` iv_new_col_txt `</xdr:col>` INTO lv_new_txt.
  REPLACE FIRST OCCURRENCE OF `<xdr:col>COL_0</xdr:col>` IN rv_comp_value WITH lv_new_txt.

  " Row DX
  lv_row_1 = iv_new_row + lr_img_template->row_dx.
  int_to_text lv_row_1.
  CONCATENATE `<xdr:row>` lv_row_1_txt `</xdr:row>` INTO lv_new_txt.
  REPLACE FIRST OCCURRENCE OF `<xdr:row>ROW_1</xdr:row>` IN rv_comp_value WITH lv_new_txt.

  " Column DX
  lv_col_1 = iv_new_col + lr_img_template->col_dx.
  int_to_text lv_col_1.
  CONCATENATE `<xdr:col>` lv_col_1_txt `</xdr:col>` INTO lv_new_txt.
  REPLACE FIRST OCCURRENCE OF `<xdr:col>COL_1</xdr:col>` IN rv_comp_value WITH lv_new_txt.
ENDMETHOD.


METHOD drawing_read_xml.
  " Instead of constructor
  CREATE DATA rr_me.

**********************************************************************
**********************************************************************
  DATA lv_xml_drawings TYPE string.
  CONCATENATE `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
              `<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">`
              `</xdr:wsDr>` INTO lv_xml_drawings.
  CREATE OBJECT rr_me->_dr_xml_xl_drawings
    EXPORTING
      io_zip     = mo_zip
      iv_path    = 'xl/drawings/drawing1.xml'        "#EC NOTEXT
      iv_str_tag = 'xdr:wsDr'                        "#EC NOTEXT
      iv_str_doc = lv_xml_drawings.
  lv_xml_drawings = rr_me->_dr_xml_xl_drawings->str_get_document( ).

**********************************************************************
**********************************************************************
  DATA lv_xml_drawings_rels TYPE string.
  CONCATENATE
    `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
    `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`
    `</Relationships>` INTO lv_xml_drawings_rels.
  CREATE OBJECT rr_me->_dr_xml_xl_drawings_rels
    EXPORTING
      io_zip     = mo_zip
      iv_path    = 'xl/drawings/_rels/drawing1.xml.rels'  "#EC NOTEXT
      iv_str_tag = 'Relationships'                        "#EC NOTEXT
      iv_str_doc = lv_xml_drawings_rels.
**********************************************************************
**********************************************************************
  " Get all tags
  DATA lt_xml_match TYPE zcl_xtt_util=>tt_xml_match.
  DATA ls_img_template TYPE ts_img_template.
  FIELD-SYMBOLS <ls_xml_match> LIKE LINE OF lt_xml_match.

  " All images from the last
  lt_xml_match = zcl_xtt_util=>get_xml_matches( iv_context   = lv_xml_drawings
                                                iv_tag       = `xdr:twoCellAnchor`
                                               ).

  LOOP AT lt_xml_match ASSIGNING <ls_xml_match>.
    " Entire `xdr:twoCellAnchor`
    ls_img_template-tag = <ls_xml_match>-tag.

    " Get name & ref to cell with {R-T-IMG}
    DATA lv_xtt_name TYPE string.
    zcl_xtt_util=>get_from_tag(
      EXPORTING
        iv_beg_part = `name="{`                             "#EC NOTEXT
        iv_end_part = `}`
        iv_new_name = `NAME_ID_`
      IMPORTING
        ev_value0   = lv_xtt_name
      CHANGING
        cv_xml      = ls_img_template-tag ).
    " Or from title (Alternative text)
    IF lv_xtt_name IS INITIAL.
      zcl_xtt_util=>get_from_tag(
        EXPORTING
          iv_beg_part = `title="{`                          "#EC NOTEXT
          iv_end_part = `}`
        IMPORTING
          ev_value0   = lv_xtt_name
        CHANGING
          cv_xml      = ls_img_template-tag ).
    ENDIF.

    zcl_xtt_util=>get_from_tag(
      EXPORTING
        iv_beg_part = `<xdr:row>`
        iv_end_part = `</xdr:row>`
        iv_new_name = `ROW_`
      IMPORTING
        ev_value0   = ls_img_template-row
        ev_value1   = ls_img_template-row_dx
      CHANGING
        cv_xml      = ls_img_template-tag ).
    ls_img_template-row_dx = ls_img_template-row_dx - ls_img_template-row.

    zcl_xtt_util=>get_from_tag(
      EXPORTING
        iv_beg_part = `<xdr:col>`
        iv_end_part = `</xdr:col>`
        iv_new_name = `COL_`
      IMPORTING
        ev_value0   = ls_img_template-col
        ev_value1   = ls_img_template-col_dx
      CHANGING
        cv_xml      = ls_img_template-tag ).
    ls_img_template-col_dx = ls_img_template-col_dx - ls_img_template-col.

    " Is offset
    ADD 1 TO: ls_img_template-row,
              ls_img_template-col.
    INSERT ls_img_template INTO TABLE rr_me->dr_t_img_template.

    " New empty cell
    DATA lr_cell TYPE REF TO ts_ex_cell.
    CREATE DATA lr_cell.
    lr_cell->c_row     = ls_img_template-row.
    lr_cell->c_col_ind = ls_img_template-col.

    " Find or create new one
    DATA lv_add TYPE abap_bool.
    CLEAR lv_add.
    IF lv_xtt_name IS NOT INITIAL.
      lv_add = abap_true.
    ENDIF.

    lr_cell = io_sheet->find_cell( ir_cell = lr_cell->*
                                   iv_add  = lv_add ).
    CHECK lr_cell IS NOT INITIAL.

    " Set name
    IF lv_xtt_name IS NOT INITIAL.
      CONCATENATE `{` lv_xtt_name `}` INTO lr_cell->c_value.
    ENDIF.

    " Delete original image from a cell
    IF lr_cell->c_value CP `{*}`.
      rr_me->_dr_xml_xl_drawings->str_delete_part( iv_beg = <ls_xml_match>-beg
                                                   iv_end = <ls_xml_match>-end ).
*      lv_has_template = abap_true.
*    ELSEIF lv_new_tabix IS NOT INITIAL.
*      DELETE io_sheet->mt_cells INDEX lv_new_tabix.
    ENDIF.
  ENDLOOP.

*  CHECK lv_has_template <> abap_true.
*  CLEAR rr_me->dr_v_drawing.
*  CLEAR rr_me->dr_v_drawing_rel.
ENDMETHOD.


METHOD drawing_save_xml.
  CHECK ir_me->_dr_xml_xl_drawings->str_status      = zcl_xtt_xml_updater=>c_status-changed
     OR ir_me->_dr_xml_xl_drawings_rels->str_status = zcl_xtt_xml_updater=>c_status-changed.

  ir_me->_dr_xml_xl_drawings->save( iv_path ).
  ir_me->_dr_xml_xl_drawings_rels->save( iv_path_rel ).

  " Add 1 by one
  DATA lv_req_text TYPE string.
  LOOP AT ir_me->dr_t_required INTO lv_req_text.
    _xml_content_types->str_add( lv_req_text ).
  ENDLOOP.

  " Has ref to file ?
  DATA lv_sheet_rel_xml TYPE string.
  lv_sheet_rel_xml = io_xml_sheet_rels->str_get_document( ).
  CHECK lv_sheet_rel_xml NS `Target="../drawings/drawing1.xml"/>`.

  " Add ref
  IF io_xml_sheet_rels->str_status = zcl_xtt_xml_updater=>c_status-empty_template.
    io_xml_xl_worksheet->obj_replace_text( iv_from = `</worksheet>`
                                           iv_to   = `<drawing r:id="_rDrawId"/></worksheet>` ).
  ELSE.
    " TODO before any ID in 'sheet1.xml.rels'
    io_xml_xl_worksheet->obj_replace_text( iv_from = `<tableParts `
                                           iv_to   = `<drawing r:id="_rDrawId"/><tableParts ` ).
  ENDIF.

  " Add relation
  io_xml_sheet_rels->str_add( `<Relationship Id="_rDrawId" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing" Target="../drawings/drawing1.xml"/>` ).
ENDMETHOD.


METHOD formula_shift.

  CONSTANTS:  lcv_operators                   TYPE string VALUE '+-/*^%=<>&, !',
              lcv_letters                     TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ$',
              lcv_digits                      TYPE string VALUE '0123456789',
              lcv_cell_reference_error        TYPE string VALUE '#REF!'.

  DATA:       lv_tcnt                         TYPE i,         " Counter variable
              lv_tlen                         TYPE i,         " Temp variable length
              lv_cnt                          TYPE i,         " Counter variable
              lv_cnt2                         TYPE i,         " Counter variable
              lv_offset1                      TYPE i,         " Character offset
              lv_numchars                     TYPE i,         " Number of characters counter
              lv_tchar(1)                     TYPE c,         " Temp character
              lv_tchar2(1)                    TYPE c,         " Temp character
              lv_cur_form                     TYPE string, " (2000)c, " Formula for current cell
              lv_ref_cell_addr                TYPE string,    " Reference cell address
              lv_tcol1                        TYPE string,    " Temp column letter
              lv_tcol2                        TYPE string,    " Temp column letter
              lv_tcoln                        TYPE i,         " Temp column number
              lv_trow1                        TYPE string,    " Temp row number
              lv_trow2                        TYPE string,    " Temp row number
              lv_flen                         TYPE i,         " Length of reference formula
              lv_tlen2                        TYPE i,         " Temp variable length
              lv_substr1                      TYPE string,    " Substring variable
              lv_abscol                       TYPE string,    " Absolute column symbol
              lv_absrow                       TYPE string.    " Absolute row symbol

*              lv_errormessage                 TYPE string.

*--------------------------------------------------------------------*
* When copying a cell in EXCEL to another cell any inherent formulas
* are copied as well.  Cell-references in the formula are being adjusted
* by the distance of the new cell to the original one
*--------------------------------------------------------------------*
* §1 Parse reference formula character by character
* §2 Identify Cell-references
* §3 Shift cell-reference
* §4 Build resulting formula
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* No distance --> Reference = resulting cell/formula
*--------------------------------------------------------------------*
  IF    iv_shift_cols = 0
    AND iv_shift_rows = 0.
    rv_resulting_formula = iv_reference_formula.
    EXIT. " done
  ENDIF.


  lv_flen     = strlen( iv_reference_formula ).
  lv_numchars = 1.

*--------------------------------------------------------------------*
* §1 Parse reference formula character by character
*--------------------------------------------------------------------*
  DO lv_flen TIMES.

    CLEAR: lv_tchar,
           lv_substr1,
           lv_ref_cell_addr.
    lv_cnt2 = lv_cnt + 1.
    IF lv_cnt2 > lv_flen.
      EXIT. " Done
    ENDIF.

*--------------------------------------------------------------------*
* Here we have the current character in the formula
*--------------------------------------------------------------------*
    lv_tchar = iv_reference_formula+lv_cnt(1).

*--------------------------------------------------------------------*
* Operators or opening parenthesis will separate possible cellreferences
*--------------------------------------------------------------------*
    IF    (    lv_tchar CA lcv_operators
            OR lv_tchar CA '(' )
      AND lv_cnt2 = 1.
      lv_substr1  = iv_reference_formula+lv_offset1(1).
      CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
      lv_cnt      = lv_cnt + 1.
      lv_offset1  = lv_cnt.
      lv_numchars = 1.
      CONTINUE.       " --> next character in formula can be analyzed
    ENDIF.

*--------------------------------------------------------------------*
* Quoted literal text holds no cell reference --> advance to end of text
*--------------------------------------------------------------------*
    IF lv_tchar EQ '"'.
      lv_cnt      = lv_cnt + 1.
      lv_numchars = lv_numchars + 1.
      lv_tchar     = iv_reference_formula+lv_cnt(1).
      WHILE lv_tchar NE '"'.

        lv_cnt      = lv_cnt + 1.
        lv_numchars = lv_numchars + 1.
        lv_tchar    = iv_reference_formula+lv_cnt(1).

      ENDWHILE.
      lv_cnt2    = lv_cnt + 1.
      lv_substr1 = iv_reference_formula+lv_offset1(lv_numchars).
      CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
      lv_cnt     = lv_cnt + 1.
      IF lv_cnt = lv_flen.
        EXIT.
      ENDIF.
      lv_offset1  = lv_cnt.
      lv_numchars = 1.
      lv_tchar    = iv_reference_formula+lv_cnt(1).
      lv_cnt2     = lv_cnt + 1.
      CONTINUE.       " --> next character in formula can be analyzed
    ENDIF.


*--------------------------------------------------------------------*
* Operators or parenthesis or last character in formula will separate possible cellreferences
*--------------------------------------------------------------------*
    IF   lv_tchar CA lcv_operators
      OR lv_tchar CA '():'
      OR lv_cnt2  =  lv_flen.
      IF lv_cnt > 0.
        lv_substr1 = iv_reference_formula+lv_offset1(lv_numchars).
*--------------------------------------------------------------------*
* Check for text concatenation and functions
*--------------------------------------------------------------------*
        IF ( lv_tchar CA lcv_operators AND lv_tchar EQ lv_substr1 ) OR lv_tchar EQ '('.
          CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
          lv_cnt = lv_cnt + 1.
          lv_offset1 = lv_cnt.
          lv_cnt2 = lv_cnt + 1.
          lv_numchars = 1.
          CONTINUE.       " --> next character in formula can be analyzed
        ENDIF.

        lv_tlen = lv_cnt2 - lv_offset1.
*--------------------------------------------------------------------*
* Exclude mathematical operators and closing parentheses
*--------------------------------------------------------------------*
        IF   lv_tchar CA lcv_operators
          OR lv_tchar CA ':)'.
          IF    lv_cnt2     = lv_flen
            AND lv_numchars = 1.
            CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
            lv_cnt      = lv_cnt + 1.
            lv_offset1  = lv_cnt.
            lv_cnt2     = lv_cnt + 1.
            lv_numchars = 1.
            CONTINUE.       " --> next character in formula can be analyzed
          ELSEIF lv_tchar <> space.
            lv_tlen = lv_tlen - 1.
          ENDIF.
        ENDIF.
*--------------------------------------------------------------------*
* Capture reference cell address
*--------------------------------------------------------------------*
        TRY.
            MOVE: iv_reference_formula+lv_offset1(lv_tlen) TO lv_ref_cell_addr. "Ref cell address
          CATCH cx_root.
            zcx_xtt_exception=>raise_sys_error( iv_message =
              'Internal error in Class ZCL_XTT_EXCEL_XLSX Method FORMULA_SHIFT Spot 1' "#EC NOTEXT
           ).  " Change to messageclass if possible
        ENDTRY.

*--------------------------------------------------------------------*
* Split cell address into characters and numbers
*--------------------------------------------------------------------*
        CLEAR: lv_tlen,
               lv_tcnt,
               lv_tcol1,
               lv_trow1.
        lv_tlen = strlen( lv_ref_cell_addr ).
        IF lv_tlen <> 0.
          CLEAR: lv_tcnt.
          DO lv_tlen TIMES.
            CLEAR: lv_tchar2.
            lv_tchar2 = lv_ref_cell_addr+lv_tcnt(1).
            IF lv_tchar2 CA lcv_letters.
              CONCATENATE lv_tcol1 lv_tchar2 INTO lv_tcol1.
            ELSEIF lv_tchar2 CA lcv_digits.
              CONCATENATE lv_trow1 lv_tchar2 INTO lv_trow1.
            ENDIF.
            lv_tcnt = lv_tcnt + 1.
          ENDDO.
        ENDIF.

        " Is valid column & row ?
        DO 1 TIMES.
          CHECK lv_tcol1 IS NOT INITIAL AND lv_trow1 IS NOT INITIAL.
          DATA lv_compare_1 TYPE string.
          DATA lv_compare_2 TYPE string.

          " COLUMN + ROW
          CONCATENATE lv_tcol1 lv_trow1 INTO lv_compare_1.

          " Original condensed string
          lv_compare_2 = lv_ref_cell_addr.
          CONDENSE lv_compare_2.

          CHECK lv_compare_1 <> lv_compare_2.
          CLEAR: lv_trow1, lv_tchar2.
        ENDDO.

*--------------------------------------------------------------------*
* Check for invalid cell address
*--------------------------------------------------------------------*
        IF lv_tcol1 IS INITIAL OR lv_trow1 IS INITIAL.
          CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
          lv_cnt = lv_cnt + 1.
          lv_offset1 = lv_cnt.
          lv_cnt2 = lv_cnt + 1.
          lv_numchars = 1.
          CONTINUE.
        ENDIF.
*--------------------------------------------------------------------*
* Check for range names
*--------------------------------------------------------------------*
        CLEAR: lv_tlen.
        lv_tlen = strlen( lv_tcol1 ).
        IF lv_tlen GT 3.
          CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
          lv_cnt = lv_cnt + 1.
          lv_offset1 = lv_cnt.
          lv_cnt2 = lv_cnt + 1.
          lv_numchars = 1.
          CONTINUE.
        ENDIF.
*--------------------------------------------------------------------*
* Check for valid row
*--------------------------------------------------------------------*
        IF lv_trow1 GT 1048576.
          CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
          lv_cnt = lv_cnt + 1.
          lv_offset1 = lv_cnt.
          lv_cnt2 = lv_cnt + 1.
          lv_numchars = 1.
          CONTINUE.
        ENDIF.
*--------------------------------------------------------------------*
* Check for absolute column or row reference
*--------------------------------------------------------------------*
        CLEAR: lv_tcol2,
               lv_trow2,
               lv_abscol,
               lv_absrow.
        lv_tlen2 = strlen( lv_tcol1 ) - 1.
        IF lv_tcol1 IS NOT INITIAL.
          lv_abscol = lv_tcol1(1).
        ENDIF.
        IF lv_tlen2 GE 0.
          lv_absrow = lv_tcol1+lv_tlen2(1).
        ENDIF.
        IF lv_abscol EQ '$' AND lv_absrow EQ '$'.
          lv_tlen2 = lv_tlen2 - 1.
          IF lv_tlen2 > 0.
            lv_tcol1 = lv_tcol1+1(lv_tlen2).
          ENDIF.
          lv_tlen2 = lv_tlen2 + 1.
        ELSEIF lv_abscol EQ '$'.
          lv_tcol1 = lv_tcol1+1(lv_tlen2).
        ELSEIF lv_absrow EQ '$'.
          lv_tcol1 = lv_tcol1(lv_tlen2).
        ENDIF.
*--------------------------------------------------------------------*
* Check for valid column
*--------------------------------------------------------------------*
        TRY.
            lv_tcoln = zcl_eui_file_io=>column_2_int( lv_tcol1 ) + iv_shift_cols.
          CATCH zcx_eui_exception.
            CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
            lv_cnt = lv_cnt + 1.
            lv_offset1 = lv_cnt.
            lv_cnt2 = lv_cnt + 1.
            lv_numchars = 1.
            CONTINUE.
        ENDTRY.
*--------------------------------------------------------------------*
* Check whether there is a referencing problem
*--------------------------------------------------------------------*
        lv_trow2 = lv_trow1 + iv_shift_rows.
        " Now could contain spaces
        CONDENSE lv_trow2.
        IF   ( lv_tcoln < 1 AND lv_abscol <> '$' )   " Maybe we should add here max-column and max row-tests as well.
          OR ( lv_trow2 < 1 AND lv_absrow <> '$' ).  " Check how EXCEL behaves in this case
*--------------------------------------------------------------------*
* Referencing problem encountered --> set error
*--------------------------------------------------------------------*
          CONCATENATE lv_cur_form lcv_cell_reference_error INTO lv_cur_form.
        ELSE.
*--------------------------------------------------------------------*
* No referencing problems --> adjust row and column
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Adjust column
*--------------------------------------------------------------------*
          IF lv_abscol EQ '$'.
            CONCATENATE lv_cur_form lv_abscol lv_tcol1 INTO lv_cur_form.
          ELSEIF iv_shift_cols EQ 0.
            CONCATENATE lv_cur_form lv_tcol1 INTO lv_cur_form.
          ELSE.
            TRY.
                lv_tcol2 = zcl_eui_file_io=>int_2_column( lv_tcoln ).
                CONCATENATE lv_cur_form lv_tcol2 INTO lv_cur_form.
              CATCH zcx_eui_exception.
                CONCATENATE lv_cur_form lv_substr1 INTO lv_cur_form.
                lv_cnt = lv_cnt + 1.
                lv_offset1 = lv_cnt.
                lv_cnt2 = lv_cnt + 1.
                lv_numchars = 1.
                CONTINUE.
            ENDTRY.
          ENDIF.
*--------------------------------------------------------------------*
* Adjust row
*--------------------------------------------------------------------*
          IF lv_absrow EQ '$'.
            CONCATENATE lv_cur_form lv_absrow lv_trow1 INTO lv_cur_form.
          ELSEIF iv_shift_rows = 0.
            CONCATENATE lv_cur_form lv_trow1 INTO lv_cur_form.
*        elseif lv_trow2 < 1.
*          CONCATENATE lv_cur_form lc_cell_reference_error INTO lv_cur_form.
          ELSE.
            CONCATENATE lv_cur_form lv_trow2 INTO lv_cur_form.
          ENDIF.
        ENDIF.

        lv_numchars = 0.
        IF   lv_tchar CA lcv_operators
          OR lv_tchar CA ':)'.
          CONCATENATE lv_cur_form lv_tchar INTO lv_cur_form RESPECTING BLANKS.
        ENDIF.
        lv_offset1 = lv_cnt2.
      ENDIF.
    ENDIF.
    lv_numchars = lv_numchars + 1.
    lv_cnt   = lv_cnt   + 1.
    lv_cnt2  = lv_cnt   + 1.

  ENDDO.



*--------------------------------------------------------------------*
* Return resulting formula
*--------------------------------------------------------------------*
  IF lv_cur_form IS NOT INITIAL.
    MOVE lv_cur_form TO rv_resulting_formula.
  ENDIF.

ENDMETHOD.


METHOD get_raw.
  CHECK mo_zip IS NOT INITIAL.

***************************************
  shared_strings_save( ).

***************************************
  " Save every sheet
  DATA lt_sheets        TYPE stringtab.
  DATA lt_defined_names TYPE tt_ex_defined_name.
  _sheet_save( IMPORTING et_defined_names = lt_defined_names
                         et_new_tags      = lt_sheets ).

***************************************
  " Save cell movements
  DATA lt_def_names TYPE stringtab.
  lt_def_names = defined_name_save_xml( lt_defined_names ).

***************************************
  _workbook_write_xml( it_tags_sheets    = lt_sheets
                       it_tags_def_names = lt_def_names ).

***************************************
  " Remove from ZIP
  mo_zip->delete( EXPORTING name    = 'xl/calcChain.xml'    "#EC NOTEXT
                  EXCEPTIONS OTHERS = 0 ).

  " 1-st process additional files
  raise_raw_events( mo_zip ).

  " ZIP archive as xstring
  rv_content = mo_zip->save( ).

  " Change content in special cases
  DATA lr_content TYPE REF TO xstring.
  GET REFERENCE OF rv_content INTO lr_content.
  RAISE EVENT prepare_raw
   EXPORTING
     iv_path    = '' " Entire file
     ir_content = lr_content.
ENDMETHOD.


METHOD get_without_t.
  DATA:
   l_len TYPE i.
  " Remove <t>...</t>
  IF iv_text(3) = '<t>'.
    l_len = strlen( iv_text ) - 7.
    rv_text = iv_text+3(l_len).
  ELSE.
    rv_text = iv_text.
  ENDIF.
ENDMETHOD.                    "get_without_t


METHOD list_object_read_xml.
  DATA:
    lv_path        TYPE string,
    lo_rels        TYPE REF TO if_ixml_document,
    lo_rel         TYPE REF TO if_ixml_element,
    l_val          TYPE string,
    ls_list_object TYPE ts_ex_list_object,
    lo_area        TYPE REF TO if_ixml_element,
    ls_area_ref    TYPE REF TO ts_ex_area.

  " Read relations
  lv_path = io_sheet->get_indexed( io_sheet->mc_path-rel_path ).
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip    = mo_zip
    iv_name   = lv_path
   IMPORTING
    eo_xmldoc = lo_rels ).

  " No relations
  CHECK lo_rels IS NOT INITIAL.

  lo_rel ?= lo_rels->find_from_name( `Relationship` ).      "#EC NOTEXT
  WHILE lo_rel IS BOUND.
    l_val = lo_rel->get_attribute( `Target` ).              "#EC NOTEXT
    lo_rel ?= lo_rel->get_next( ).
    CHECK l_val CP `../tables/*`.                           "#EC NOTEXT

    " Path to table
    CLEAR ls_list_object.
    CONCATENATE `xl/tables/` l_val+10 INTO ls_list_object-arc_path. "#EC NOTEXT

    " Dom
    zcl_eui_conv=>xml_from_zip(
     EXPORTING
      io_zip    = mo_zip
      iv_name   = ls_list_object-arc_path
     IMPORTING
      eo_xmldoc = ls_list_object-dom ).

    " Get address
    lo_area ?= ls_list_object-dom->get_first_child( ).
    l_val = lo_area->get_attribute( 'ref' ).                "#EC NOTEXT

    " Edit area
    GET REFERENCE OF ls_list_object-area INTO ls_area_ref.
    area_read_xml(
     iv_value = l_val
     is_area  = ls_area_ref ).

    " Add to list if Ok
    CHECK ls_area_ref->a_cells IS NOT INITIAL.

    ls_list_object-_lo_id          = lo_area->get_attribute( 'id' ). "#EC NOTEXT
    ls_list_object-_lo_name        = lo_area->get_attribute( 'name' ). "#EC NOTEXT
    ls_list_object-_lo_displayname = lo_area->get_attribute( 'displayName' ). "#EC NOTEXT
    " For speed
    CONCATENATE ls_list_object-_lo_name `[` INTO ls_list_object-name_fm_mask.

    APPEND ls_list_object TO io_sheet->mt_list_objects.
  ENDWHILE.

  " Set CELL positions
  DATA lr_list_object TYPE REF TO ts_ex_list_object.
  DATA ls_cell        TYPE REF TO ts_ex_cell.

  LOOP AT io_sheet->mt_list_objects REFERENCE INTO lr_list_object.
    LOOP AT lr_list_object->area-a_cells REFERENCE INTO ls_cell.
      " Get existing cell Or insert new one
      io_sheet->find_cell( ls_cell->* ).
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.                    "list_object_read_xml


METHOD list_object_save_xml.
  DATA ls_list_object  TYPE REF TO ts_ex_list_object.
  DATA lo_table        TYPE REF TO if_ixml_element.
  DATA ls_area         TYPE REF TO ts_ex_area.
  DATA lv_address      TYPE string.

  LOOP AT io_sheet->mt_list_objects REFERENCE INTO ls_list_object.
    " Get address
    GET REFERENCE OF ls_list_object->area INTO ls_area.

    " Change cells
    io_sheet->replace_with_new( ir_area = ls_area ).

    lv_address = area_get_address(
     is_area     = ls_area
     iv_no_bucks = abap_true ).
    CHECK lv_address IS NOT INITIAL.

    " Change area
    lo_table = ls_list_object->dom->get_root_element( ).
    lo_table->set_attribute( name  = 'ref'                  "#EC NOTEXT
                             value = lv_address ).

    DATA lv_path TYPE string.
    lv_path = ls_list_object->arc_path.

    IF io_sheet->_is_new( ) = abap_true AND iv_sid >= 0.
      io_sheet->change_xml_doc( EXPORTING iv_sid  = iv_sid
                                          iv_mask = `tables/table`
                                CHANGING  cv_doc  = lv_path ).
      DATA lv_content_type TYPE string.
      CONCATENATE `<Override ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.table+xml" PartName="/` "#EC NOTEXT
                  lv_path `"/>` INTO lv_content_type.
      io_sheet->mo_xlsx->_xml_content_types->str_add( lv_content_type ).

      DATA lv_attr TYPE string.
      lv_attr = io_sheet->get_new_id( ls_list_object->_lo_id ).
      lo_table->set_attribute( name  = `id`                 "#EC NOTEXT
                               value = lv_attr ).

      lv_attr = io_sheet->get_new_name( ls_list_object->_lo_name ).
      lo_table->set_attribute( name  = `name`               "#EC NOTEXT
                               value = lv_attr ).

      lv_attr = io_sheet->get_new_name( ls_list_object->_lo_displayname ).
      lo_table->set_attribute( name  = `displayName`        "#EC NOTEXT
                               value = lv_attr ).
    ENDIF.

    " Replace in zip
    zcl_eui_conv=>xml_to_zip(
     io_zip     = mo_zip
     iv_name    = lv_path
     io_xmldoc  = ls_list_object->dom ).
  ENDLOOP.
ENDMETHOD.


METHOD merge.
  " For chain calls
  ro_xtt = super->merge( is_block      = is_block
                         iv_block_name = iv_block_name ).
  CHECK mo_zip IS NOT INITIAL.

  " Special case
  DATA lv_root_is_table TYPE abap_bool.
  lv_root_is_table = zcl_xtt_util=>is_table( is_block ).
  IF lv_root_is_table = abap_true.
    CHECK _sheet_merge( iv_block_name = iv_block_name
                        it_block      = is_block ) <> abap_true.
  ENDIF.

  " Update each sheet
  DATA lo_replace_block TYPE REF TO zcl_xtt_replace_block.
  LOOP AT mt_sheets INTO mo_sheet.
    DATA lo_no_check TYPE REF TO zcx_eui_no_check.
    TRY.
        " Prepare for replacement
        CREATE OBJECT lo_replace_block
          EXPORTING
            io_xtt        = me
            is_block      = is_block
            iv_block_name = iv_block_name.

        " Find trees in file
        mo_sheet->merge( EXPORTING io_block = lo_replace_block
                         CHANGING  ct_cells = mo_sheet->mt_cells ).
      CATCH zcx_eui_no_check INTO lo_no_check.
        add_log_message( io_exception = lo_no_check ).
    ENDTRY.

    CLEAR _scopes.
  ENDLOOP.
ENDMETHOD.


METHOD on_match_found.
  CONSTANTS:
    c_date_start TYPE d VALUE '18991230',
    c_time_start TYPE t VALUE '000000'.
  DATA:
    l_len       TYPE i,
    l_value     TYPE string,
    l_date      TYPE float,
    lv_prev_typ TYPE string.
  FIELD-SYMBOLS:
    <l_string> TYPE csequence,
    <l_date>   TYPE d,
    <l_time>   TYPE t.

  " Just skip
  CHECK is_field->typ <> zcl_xtt_replace_block=>mc_type-tree.

  " If the exactly in one cell and number (ms_cell->c_typ can be 's' string)
  l_len      = strlen( mo_sheet->ms_cell->c_value ).
  iv_pos_end = iv_pos_end + 1.
  IF iv_pos_beg = 0 AND l_len = iv_pos_end.
    CASE is_field->typ.
        " integer and double(float)
      WHEN zcl_xtt_replace_block=>mc_type-integer OR zcl_xtt_replace_block=>mc_type-double.
        CLEAR mo_sheet->ms_cell->c_type.

        " Datetime Whole as a string like  d + t
      WHEN zcl_xtt_replace_block=>mc_type-datetime.
        ASSIGN is_field->dref->* TO <l_string>.

        " Both parts
        ASSIGN <l_string>(8)     TO <l_date> CASTING.
        ASSIGN <l_string>+8(6)   TO <l_time> CASTING.
        CLEAR mo_sheet->ms_cell->c_type.

        " Date
      WHEN zcl_xtt_replace_block=>mc_type-date.
        ASSIGN is_field->dref->* TO <l_date> CASTING. " allow accept char(8) as date
        CLEAR mo_sheet->ms_cell->c_type.

        " Time
      WHEN zcl_xtt_replace_block=>mc_type-time.
        ASSIGN is_field->dref->* TO <l_time> CASTING. " allow accept char(6) as time
        CLEAR mo_sheet->ms_cell->c_type.

      WHEN zcl_xtt_replace_block=>mc_type-boolean.
        mo_sheet->ms_cell->c_type = 'b'.

      WHEN zcl_xtt_replace_block=>mc_type-image.
        mo_sheet->ms_cell->c_image ?= is_field->oref.
        CLEAR mo_sheet->ms_cell->c_value.
        CLEAR mo_sheet->ms_cell->c_type.
        RETURN.
    ENDCASE.

    DO 1 TIMES.
      " Transform date to excel format as float
      CHECK <l_date> IS ASSIGNED OR <l_time> IS ASSIGNED.

      " Date
      IF <l_date> IS ASSIGNED AND <l_date> IS NOT INITIAL.
        " Number of days since
        l_date = <l_date> - c_date_start.
      ENDIF.

      " Time
      IF <l_time> IS ASSIGNED AND <l_time> IS NOT INITIAL.
        " 0.5 half of a day
        l_date = l_date + ( <l_time> - c_time_start ) / ( 60 * 60 * 24 ).
      ENDIF.

      " Save previous type for dates only
      lv_prev_typ = is_field->typ.

      " Empty string
      IF     l_date IS INITIAL.
        " Empty string
        CREATE DATA is_field->dref TYPE string.
        is_field->typ = zcl_xtt_replace_block=>mc_type-string.
      ELSEIF l_date < 0.
        " Use WRITE ... TO
      ELSE.
        GET REFERENCE OF l_date INTO is_field->dref.
        is_field->typ = zcl_xtt_replace_block=>mc_type-double.
      ENDIF.
    ENDDO.
  ENDIF.

  " Try to get value as a string
  IF l_value IS INITIAL.
    l_value = zcl_xtt_replace_block=>get_as_string( is_field = is_field ).

    " Use WRITE ... TO
    IF l_date < 0.
      mo_sheet->ms_cell->c_style = ''.
      mo_sheet->ms_cell->c_type  = 's'.
      " is_field->typ = zcl_xtt_replace_block=>mc_type-string.
    ENDIF.
  ENDIF.

  IF lv_prev_typ IS NOT INITIAL.
    is_field->typ = lv_prev_typ.
  ENDIF.

  " Create new value
  CONCATENATE
   mo_sheet->ms_cell->c_value(iv_pos_beg)
   l_value
   mo_sheet->ms_cell->c_value+iv_pos_end INTO mo_sheet->ms_cell->c_value RESPECTING BLANKS.
ENDMETHOD.


METHOD row_read_xml.
  DATA lo_row   TYPE REF TO if_ixml_element.
  DATA ls_row   LIKE LINE OF io_sheet->mt_rows.
  DATA lo_cell  TYPE REF TO if_ixml_element.

  lo_row ?= io_sheet->mo_document->find_from_name( 'row' ).      "#EC NOTEXT

  " Add new rows
  WHILE lo_row IS BOUND.
    CLEAR ls_row.
    ls_row-r             = lo_row->get_attribute( 'r' ).    "#EC NOTEXT
    ls_row-customheight  = lo_row->get_attribute( 'customHeight' ). "#EC NOTEXT
    ls_row-ht            = lo_row->get_attribute( 'ht' ).   "#EC NOTEXT
    ls_row-hidden        = lo_row->get_attribute( 'hidden' ). "#EC NOTEXT
    ls_row-outlinelevel  = lo_row->get_attribute( 'outlineLevel' ). "#EC NOTEXT

    " And add by key
    INSERT ls_row INTO TABLE io_sheet->mt_rows.

    " Loop through excels cells
    lo_cell = lo_row->find_from_name( 'c' ).                "#EC NOTEXT
    WHILE lo_cell IS BOUND.
      cell_read_xml( io_sheet           = io_sheet
                     io_node            = lo_cell
                     it_shared_strings  = it_shared_strings
                     iv_row             = ls_row-r ).
      " Next cell
      lo_cell ?= lo_cell->get_next( ).
    ENDWHILE.

    " Next row
    lo_row ?= lo_row->get_next( ).
  ENDWHILE.
ENDMETHOD.


METHOD row_write_xml.
  DATA lv_row_tag TYPE string.

  " To text
  int_to_text iv_new_row.

  " Write attributes
  CONCATENATE lv_row_tag `<row`
   ` r="`            iv_new_row_txt            `"` INTO lv_row_tag. "#EC NOTEXT

  " if im_row->customheight = 1 then height = im_row->ht
  IF is_row->ht IS NOT INITIAL AND is_row->customheight IS NOT INITIAL.
    CONCATENATE lv_row_tag
    ` customHeight="` is_row->customheight `"`
    ` ht="`           is_row->ht           `"` INTO lv_row_tag. "#EC NOTEXT
  ENDIF.

  " ht = 0
  IF is_row->hidden IS NOT INITIAL.
    CONCATENATE lv_row_tag
    ` hidden="`       is_row->hidden       `"` INTO lv_row_tag. "#EC NOTEXT
  ENDIF.

  " + sign
  DATA lv_outline TYPE string.
  IF iv_outline_level IS NOT INITIAL.
    lv_outline = iv_outline_level.
    CONDENSE lv_outline.
  ELSEIF is_row->outline_skip <> abap_true.
    lv_outline = is_row->outlinelevel.
  ENDIF.

  IF lv_outline IS NOT INITIAL.
    CONCATENATE lv_row_tag
    ` outlineLevel="` lv_outline `"` INTO lv_row_tag.       "#EC NOTEXT
  ENDIF.

  " Closing >
  CONCATENATE lv_row_tag `>` INTO lv_row_tag.
  APPEND lv_row_tag TO cs_transmit-rows_cells.
ENDMETHOD.                    "row_write_xml


METHOD shared_strings_read.
  " Mainly <t> tags
  DATA l_shared_strings TYPE string.
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip     = mo_zip
    iv_name    = 'xl/sharedStrings.xml'                     "#EC NOTEXT
   IMPORTING
    ev_sdoc    = l_shared_strings ).

  " Find all pairs
  DATA lt_match TYPE match_result_tab.
  lt_match = zcl_xtt_util=>get_tag_matches( iv_context = l_shared_strings
                                            iv_tag     = 'si' ).
  " From the end
  DATA l_cnt TYPE i.
  l_cnt = lines( lt_match ).

  DATA l_ind TYPE i.
  l_ind = 0.
  WHILE l_cnt > l_ind.
    " Start tag
    l_ind = l_ind + 1.
    DATA ls_match_beg TYPE REF TO match_result.
    READ TABLE lt_match REFERENCE INTO ls_match_beg INDEX l_ind.

    " End tag
    l_ind = l_ind + 1.
    DATA ls_match_end TYPE REF TO match_result.
    READ TABLE lt_match REFERENCE INTO ls_match_end INDEX l_ind.

    " Calc offeset
    DATA l_off TYPE i.
    DATA l_len TYPE i.
    l_off = ls_match_beg->offset + 4. " strlen( <si> )
    l_len = ls_match_end->offset - ls_match_beg->offset - 4.

    " Get value
    DATA l_val TYPE string.
    l_val = l_shared_strings+l_off(l_len).
    l_val = get_without_t( l_val ).

    " Add to table
    APPEND l_val TO mt_shared_strings.
  ENDWHILE.
ENDMETHOD.


METHOD shared_strings_save.
  DATA lo_sheet TYPE REF TO lcl_ex_sheet.

  " Fill shared strings
  CLEAR mt_shared_strings.
  LOOP AT mt_sheets INTO lo_sheet.
    lo_sheet->fill_shared_strings(
     CHANGING
      ct_shared_strings = mt_shared_strings ).
  ENDLOOP.

  " Unique lines
  SORT mt_shared_strings BY table_line.
  DELETE ADJACENT DUPLICATES FROM mt_shared_strings.

  " Number of lines
  DATA lv_val TYPE i.
  lv_val = lines( mt_shared_strings ).
  int_to_text lv_val.

  " Header
  CONCATENATE `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
              `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="`
                 lv_val_txt `" uniqueCount="` lv_val_txt `">` INTO lv_val_txt.

  " Main body
  DATA l_str TYPE string.
  LOOP AT mt_shared_strings INTO l_str.
    DATA l_off TYPE i.
    l_off = strlen( l_str ) - 1.

    " Is XML ?
    IF l_str(1) = '<' AND l_str+l_off(1) = '>'.
      CONCATENATE lv_val_txt `<si>` l_str `</si>` INTO lv_val_txt.
    ELSE.
      CONCATENATE lv_val_txt `<si><t>` l_str `</t></si>` INTO lv_val_txt.
    ENDIF.
  ENDLOOP.
  CONCATENATE lv_val_txt `</sst>` INTO lv_val_txt.

  zcl_eui_conv=>xml_to_zip(
   io_zip   = mo_zip
   iv_name  = `xl/sharedStrings.xml`
   iv_sdoc  = lv_val_txt ).
ENDMETHOD.


METHOD _sheet_merge.
  DATA lv_name_mask TYPE string.
  CONCATENATE `*{` iv_block_name `-*}*` INTO lv_name_mask.

  " Has template ?
  DATA lo_sheet LIKE mo_sheet.
  LOOP AT mt_sheets INTO lo_sheet.
    CHECK lo_sheet->_name CP lv_name_mask.

    " Insert from the next sheet
    DATA lv_position TYPE i.
    lv_position  = sy-tabix + 1.

    " Use several times
    lo_sheet->_state = `veryHidden`.
    EXIT.
  ENDLOOP.
  CHECK lv_position IS NOT INITIAL.

  " The pattern was found
  rv_merged = abap_true.

  FIELD-SYMBOLS <ls_block> TYPE any.
  LOOP AT it_block ASSIGNING <ls_block>.
    " New sheet
    _sheets_counter = _sheets_counter + 1.
    mo_sheet = lo_sheet->clone( is_block      = <ls_block>
                                iv_block_name = iv_block_name
                                iv_new_index  = _sheets_counter ).
    INSERT mo_sheet INTO mt_sheets INDEX lv_position.
    lv_position = lv_position + 1.

    DATA lo_replace_block TYPE REF TO zcl_xtt_replace_block.
    DATA lo_no_check      TYPE REF TO zcx_eui_no_check.
    TRY.
        " Prepare for replacement
        CREATE OBJECT lo_replace_block
          EXPORTING
            io_xtt        = me
            is_block      = <ls_block>
            iv_block_name = iv_block_name.

        " Find trees in file
        mo_sheet->merge( EXPORTING io_block = lo_replace_block
                         CHANGING  ct_cells = mo_sheet->mt_cells ).
      CATCH zcx_eui_no_check INTO lo_no_check.
        add_log_message( io_exception = lo_no_check ).
    ENDTRY.

    " TODO ?
    CLEAR _scopes.
  ENDLOOP.
ENDMETHOD.


METHOD _sheet_save.
  " Results
  CLEAR et_defined_names.
  CLEAR et_new_tags.

  DATA lo_sheet TYPE REF TO lcl_ex_sheet.
  LOOP AT mt_sheets INTO lo_sheet.
    DATA: lv_sid   TYPE i.
    lv_sid   = sy-tabix - 1.

    lo_sheet->save( EXPORTING iv_sid           = lv_sid
                    CHANGING  ct_defined_names = et_defined_names ).

    DATA lv_tag TYPE string.
    lv_tag = lo_sheet->get_tag( ).
    APPEND lv_tag TO et_new_tags.
  ENDLOOP.
ENDMETHOD.


METHOD _workbook_write_xml.
  _xml_content_types->save( ).

  DATA lv_new_wb TYPE abap_bool.
  IF it_tags_def_names IS NOT INITIAL.
    lv_new_wb = abap_true. " Source 1
    _xml_xl_workbook->obj_replace( iv_tag  = `definedNames`
                                   it_tags = it_tags_def_names ).
  ENDIF.

  IF _sheets_counter > 555.
    lv_new_wb = abap_true. " Source 2
    _xml_xl_workbook->obj_replace( iv_tag  = `sheets`              "#EC NOTEXT
                                   it_tags = it_tags_sheets ).
  ENDIF.

  " Yes write back
  CHECK lv_new_wb = abap_true.
  _xml_xl_workbook->save( ).

**********************************************************************
**********************************************************************
  " Writeback rel file
  CHECK _sheets_counter > 555.

  "№ 1 - New sheets' tags
  DATA lt_rel_tag TYPE stringtab.
  DATA lv_rel_tag TYPE string.
  DATA lo_sheet TYPE REF TO lcl_ex_sheet.
  LOOP AT mt_sheets INTO lo_sheet.
    lv_rel_tag = lo_sheet->get_rel_tag( ).
    APPEND lv_rel_tag TO lt_rel_tag.
  ENDLOOP.
  "№ 2 - What file to update?
  DATA lo_rel_updater TYPE REF TO zcl_xtt_xml_updater.
  CREATE OBJECT lo_rel_updater
    EXPORTING
      io_zip  = mo_zip
      iv_path = 'xl/_rels/workbook.xml.rels'. "#EC NOTEXT
  "№ 3 - delete previous sheets
  lo_rel_updater->obj_replace( iv_tag            = `Relationships` "#EC NOTEXT
                               it_tags           = lt_rel_tag
                               iv_part_attribute = `Target`        "#EC NOTEXT
                               iv_part_value     = `worksheets/*`  "#EC NOTEXT
  ).
  "№ 4 - write back to zip
  lo_rel_updater->save( ).
ENDMETHOD.
ENDCLASS.

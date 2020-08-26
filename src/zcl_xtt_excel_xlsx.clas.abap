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
  methods GET_RAW
    redefinition .
  methods MERGE
    redefinition .
protected section.

private section.

  data MT_SHEETS type LCL_EX_SHEET_TAB .
  data MO_ZIP type ref to CL_ABAP_ZIP .
  data MT_SHARED_STRINGS type STRINGTAB .
  data MT_DEFINED_NAMES type TT_EX_DEFINED_NAME .

  class-methods COLUMN_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET .
  class-methods COLUMN_WRITE_XML
    importing
      !IT_COLUMNS type TT_EX_COLUMN
    returning
      value(RV_COLUMN_TEXT) type STRING .
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
      !IO_NODE type ref to IF_IXML_ELEMENT
      !IT_SHARED_STRINGS type STRINGTAB
    changing
      !CT_CELLS type TT_EX_CELL .
  class-methods CELL_WRITE_XML
    importing
      !IS_CELL type ref to TS_EX_CELL
      !IV_NEW_ROW type I
      !IV_NEW_COL_IND type I
      !IT_SHARED_STRINGS type STRINGTAB
    changing
      !CV_SHEET_DATA type STRING
      !CV_MERGE_CNT type I
      !CV_MERGE_CELLS type STRING .
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
      !CV_SHEET_DATA type STRING .
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
  class-methods DEFINED_NAME_READ_XML
    importing
      !IO_WORKBOOK type ref to IF_IXML_DOCUMENT
    changing
      !CT_DEFINED_NAMES type TT_EX_DEFINED_NAME .
  class-methods DEFINED_NAME_SAVE_XML
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IT_DEFINED_NAMES type TT_EX_DEFINED_NAME .
  class-methods LIST_OBJECT_READ_XML
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IO_SHEET type ref to LCL_EX_SHEET .
  class-methods LIST_OBJECT_SAVE_XML
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IO_SHEET type ref to LCL_EX_SHEET
      !IT_CELL_REF type TT_CELL_REF .
  class-methods DATA_VALIDATION_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
      !IO_WORKBOOK type ref to IF_IXML_DOCUMENT .
  class-methods DATA_VALIDATION_SAVE_XML
    importing
      !IO_WORKBOOK type ref to IF_IXML_DOCUMENT
      !IO_SHEET type ref to LCL_EX_SHEET
      !IT_CELL_REF type TT_CELL_REF .
  class-methods GET_WITHOUT_T
    importing
      !IV_TEXT type STRING
    returning
      value(RV_TEXT) type STRING .
  class-methods DRAWING_READ_XML
    importing
      !IO_SHEET type ref to LCL_EX_SHEET
      !IO_ZIP type ref to CL_ABAP_ZIP
    returning
      value(RR_ME) type ref to TS_DRAWING .
  class-methods DRAWING_ADD
    importing
      !IR_ME type ref to TS_DRAWING
      !IR_CELL type ref to TS_EX_CELL
      !IO_ZIP type ref to CL_ABAP_ZIP
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
  class-methods DRAWING_SAVE_XML
    importing
      !IR_ME type ref to TS_DRAWING
      !IO_SHEET type ref to LCL_EX_SHEET
      !IO_ZIP type ref to CL_ABAP_ZIP
    changing
      !CV_SHEET_XML type STRING .
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
  IF is_area->a_sheet_name IS NOT INITIAL.
    CONCATENATE `'` is_area->a_sheet_name `'!` rv_address INTO rv_address.
  ENDIF.
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
    " Delete '
    ls_area->a_sheet_name = l_name.
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
  DATA:
    ls_cell TYPE REF TO ts_ex_cell,
    lo_elem TYPE REF TO if_ixml_element,
    l_val   TYPE string,
    l_ind   TYPE i.

  " Insert new one
  APPEND INITIAL LINE TO ct_cells REFERENCE INTO ls_cell.

  " Transform coordinates
  l_val = io_node->get_attribute( 'r' ).
  cell_init(
   iv_coordinate = l_val
   is_cell       = ls_cell ).

  ls_cell->c_type  = io_node->get_attribute( 't' ).
  ls_cell->c_style = io_node->get_attribute( 's' ).

  " Just formula
  lo_elem = io_node->find_from_name( name = 'f' ).
  IF lo_elem IS BOUND.
    " TODO raw xml       shared formula!
    l_val = lo_elem->get_value( ).
    l_val = cl_http_utility=>escape_html( l_val ).

    ls_cell->c_formula = l_val.
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

  " Update formula
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
      zcx_eui_exception=>raise_dump( io_error = lo_error ).
  ENDTRY.

  " Formula
  IF is_cell->c_formula IS NOT INITIAL.
    " Replace to new row index
    FIND FIRST OCCURRENCE OF '$' IN is_cell->c_formula.
    IF sy-subrc = 0.
      " Old row index -> New row index
      int_2_text is_cell->c_row lv_number.

      replace_all_with_buck lv_number iv_new_row_txt.
      IF sy-subrc <> 0.
        replace_all_with_buck is_cell->c_col lv_new_col.
      ENDIF.
    ENDIF.
    " Write formula
    CONCATENATE `<f>` is_cell->c_formula `</f>` INTO lv_data.
  ELSEIF is_cell->c_value IS NOT INITIAL.
    " String
    IF cell_is_string( is_cell ) = abap_true.
      READ TABLE it_shared_strings TRANSPORTING NO FIELDS BINARY SEARCH
       WITH KEY table_line = is_cell->c_value.
      " Write index of strings
      lv_index = sy-tabix - 1.
      int_2_text lv_index lv_number.
      CONCATENATE `<v>` lv_number        `</v>` INTO lv_data.
    ELSE. " Numbers
      CONCATENATE `<v>` is_cell->c_value `</v>` INTO lv_data.
    ENDIF.
  ENDIF.

  " Change current row
  is_cell->c_row     = iv_new_row.
  is_cell->c_col     = lv_new_col.
  is_cell->c_col_ind = iv_new_col_ind.

  " Address
  CONCATENATE cv_sheet_data `<c r="` is_cell->c_col iv_new_row_txt `"` INTO cv_sheet_data.

  " Style and type
  IF is_cell->c_style IS NOT INITIAL.
    CONCATENATE cv_sheet_data ` s="` is_cell->c_style `"` INTO cv_sheet_data.
  ENDIF.
  IF is_cell->c_type IS NOT INITIAL.
    CONCATENATE cv_sheet_data ` t="` is_cell->c_type  `"` INTO cv_sheet_data.
  ENDIF.

  " Data
  CONCATENATE cv_sheet_data `>` lv_data `</c>` INTO cv_sheet_data.

  " Merged cell
  IF is_cell->c_merge_row_dx IS NOT INITIAL OR
     is_cell->c_merge_col_dx IS NOT INITIAL.
    cv_merge_cnt = cv_merge_cnt + 1.

    " Calculate new val
    lv_index = is_cell->c_row     + is_cell->c_merge_row_dx.
    int_2_text lv_index lv_number.

    lv_index = is_cell->c_col_ind + is_cell->c_merge_col_dx.
    TRY.
        lv_merge_col = zcl_eui_file_io=>int_2_column( lv_index  ).
      CATCH zcx_eui_exception INTO lo_error.
        zcx_eui_exception=>raise_dump( io_error = lo_error ).
    ENDTRY.

    CONCATENATE cv_merge_cells `<mergeCell ref="`
      is_cell->c_col  iv_new_row_txt `:`
      lv_merge_col    lv_number `"/> ` INTO cv_merge_cells.
  ENDIF.
ENDMETHOD.


METHOD column_read_xml.
  DATA lo_col TYPE REF TO if_ixml_element.
  DATA ls_col LIKE LINE OF io_sheet->mt_columns.
  DATA lv_cnt TYPE i.

  lo_col ?= io_sheet->mo_dom->find_from_name( 'col' ).             "#EC NOTEXT
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
  DATA lv_text TYPE string.
  FIELD-SYMBOLS:
   <ls_item> LIKE LINE OF it_columns.

  " All columns as 1 string
  LOOP AT it_columns ASSIGNING <ls_item>.
    CONCATENATE rv_column_text `<col` INTO rv_column_text RESPECTING BLANKS.
    add_attr rv_column_text collapsed    'collapsed'.                      "#EC NOTEXT
    add_attr rv_column_text customwidth  'customWidth'.                    "#EC NOTEXT
    add_attr rv_column_text hidden       'hidden'.                         "#EC NOTEXT
    add_attr rv_column_text max          'max'.                            "#EC NOTEXT
    add_attr rv_column_text min          'min'.                            "#EC NOTEXT
    add_attr rv_column_text outlinelevel 'outlineLevel'.                   "#EC NOTEXT
    add_attr rv_column_text phonetic     'phonetic'.                       "#EC NOTEXT
    add_attr rv_column_text style        'style'.                          "#EC NOTEXT
    add_attr rv_column_text width        'width'.                          "#EC NOTEXT
    CONCATENATE rv_column_text `></col>` INTO rv_column_text RESPECTING BLANKS. "#EC NOTEXT
  ENDLOOP.
ENDMETHOD.


METHOD constructor.
  DATA:
    l_x_value        TYPE xstring,
    l_shared_strings TYPE string,
    lt_match         TYPE match_result_tab,
    l_cnt            TYPE i,
    l_ind            TYPE i,
    ls_match_beg     TYPE REF TO match_result,
    ls_match_end     TYPE REF TO match_result,
    l_len            TYPE i,
    l_off            TYPE i,
    l_val            TYPE string,
    lo_node          TYPE REF TO if_ixml_element,
    lo_sheet         TYPE REF TO lcl_ex_sheet,
    lo_workbook      TYPE REF TO if_ixml_document.

  super->constructor( io_file = io_file ).

  " Load zip archive from XSTRING
  CREATE OBJECT mo_zip.
  io_file->get_content(
   IMPORTING
    ev_as_xstring = l_x_value ).
  mo_zip->load( l_x_value ).

***************************************
  " Main work book
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip     = mo_zip
    iv_name    = 'xl/workbook.xml'                          "#EC NOTEXT
   IMPORTING
    eo_xmldoc  = lo_workbook ).

***************************************
  " Shared strings. Mainly <t> tags
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip     = mo_zip
    iv_name    = 'xl/sharedStrings.xml'                     "#EC NOTEXT
   IMPORTING
    ev_sdoc    = l_shared_strings ).

  " Find all pairs
  FIND ALL OCCURRENCES OF REGEX '(<si>)|(<\/si>)' IN l_shared_strings RESULTS lt_match. "#EC NOTEXT
  l_cnt = lines( lt_match ).

  l_ind = 0.
  WHILE l_cnt > l_ind.
    " Start tag
    l_ind = l_ind + 1.
    READ TABLE lt_match REFERENCE INTO ls_match_beg INDEX l_ind.

    " End tag
    l_ind = l_ind + 1.
    READ TABLE lt_match REFERENCE INTO ls_match_end INDEX l_ind.

    " Calc offeset
    l_off = ls_match_beg->offset + 4. " strlen( <si> )
    l_len = ls_match_end->offset - ls_match_beg->offset - 4.

    " Get value
    l_val = l_shared_strings+l_off(l_len).
    l_val = get_without_t( l_val ).

    " Add to table
    APPEND l_val TO mt_shared_strings.
  ENDWHILE.

***************************************
  " Defined names for moving the second cell of areas
  defined_name_read_xml(
   EXPORTING
    io_workbook      = lo_workbook
   CHANGING
    ct_defined_names = mt_defined_names ).

***************************************
  " Sheets
  lo_node = lo_workbook->find_from_name( 'sheet' ).         "#EC NOTEXT
  WHILE lo_node IS BOUND.
    " Prepare and add
    CREATE OBJECT lo_sheet
      EXPORTING
        iv_ind  = sy-index
        io_node = lo_node
        io_xlsx = me.
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

  lo_data_valid ?= io_workbook->find_from_name( 'dataValidation' ). "#EC NOTEXT
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

  lo_data_valid ?= io_workbook->find_from_name( 'dataValidation' ). "#EC NOTEXT
  LOOP AT io_sheet->mt_data_valid REFERENCE INTO ls_area.
    CHECK lo_data_valid IS NOT INITIAL.

    " Change
    io_sheet->replace_with_new( ir_area     = ls_area
                                it_cell_ref = it_cell_ref ).

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

  lo_node ?= io_workbook->find_from_name( 'definedName' ).

  " Name & value
  WHILE lo_node IS BOUND.
    CLEAR ls_defined_name.
    CLEAR lt_value.

    " Regardless CHECK l_value IS NOT INITIAL AND l_value(1) <> '#'.
    "  APPEND INITIAL LINE TO ct_defined_names REFERENCE INTO ls_defined_name.
    ls_defined_name-d_name      = lo_node->get_attribute( 'name' ). "#EC NOTEXT
    ls_defined_name-d_local_sid = lo_node->get_attribute( 'localSheetId' ). "#EC NOTEXT
    l_value  = lo_node->get_value( ).

    " Several areas
    SPLIT l_value AT ',' INTO TABLE lt_value.
    LOOP AT lt_value INTO l_value.
      area_read_xml(
       EXPORTING
        iv_value = l_value
       CHANGING
        ct_areas = ls_defined_name-d_areas ).
    ENDLOOP.

    " Create new defined name
    INSERT ls_defined_name INTO TABLE ct_defined_names.

    lo_node ?= lo_node->get_next( ).
  ENDWHILE.
ENDMETHOD.


METHOD defined_name_save_xml.
  CHECK it_defined_names IS NOT INITIAL.

  DATA lv_workbook TYPE string.
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip     = io_zip
    iv_name    = 'xl/workbook.xml'                          "#EC NOTEXT
   IMPORTING
    ev_sdoc    = lv_workbook ).

  " New tag
  DATA lv_names        TYPE string.
  DATA l_ref           TYPE string.
  DATA ls_area         TYPE REF TO ts_ex_area.
  DATA l_part          TYPE string.
  DATA lv_text         TYPE string.
  FIELD-SYMBOLS:
   <ls_item> LIKE LINE OF IT_DEFINED_NAMES.

  lv_names = `<definedNames>`.
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

    CONCATENATE lv_names `<definedName` INTO lv_names RESPECTING BLANKS.
    add_attr lv_names d_name      `name`.            "#EC NOTEXT
    add_attr lv_names d_local_sid `localSheetId`.    "#EC NOTEXT

    CONCATENATE lv_names  `>` l_ref `</definedName>` INTO lv_names RESPECTING BLANKS.
  ENDLOOP.

  " Only 1 replace
  CONCATENATE lv_names `</definedNames>` INTO lv_names.
  REPLACE FIRST OCCURRENCE OF REGEX
    `(<definedNames>).*(</definedNames>)` IN lv_workbook WITH
    `_DEFINED_NAMES_`.

  " Insert from second attempt
  REPLACE FIRST OCCURRENCE OF `_DEFINED_NAMES_` IN lv_workbook WITH lv_names.

  " Write XML to zip
  zcl_eui_conv=>xml_to_zip( io_zip  = io_zip
                            iv_name = 'xl/workbook.xml'     "#EC NOTEXT
                            iv_sdoc = lv_workbook ).
ENDMETHOD.


METHOD drawing_add.
  " No need
  DATA lo_comp_cell LIKE ir_cell->c_comp_cell.
  lo_comp_cell = ir_cell->c_comp_cell.
  CHECK lo_comp_cell IS NOT INITIAL.

  " Obligatory text
  INSERT
   `<Override PartName="/xl/drawings/drawing1.xml" ContentType="application/vnd.openxmlformats-officedocument.drawing+xml"/>`
  INTO TABLE ir_me->dr_t_required.

  DATA lv_file_name TYPE string.
  DATA lv_mime_text TYPE string.
  DATA lv_rewrite   TYPE abap_bool.

  lo_comp_cell->save_in_archive(
   EXPORTING io_zip         = io_zip
             iv_prefix      = 'xl/media/'                   "#EC NOTEXT
   IMPORTING ev_file_name   = lv_file_name
             ev_mime_text   = lv_mime_text
             ev_rewrite     = lv_rewrite ).

  " Read mime type ?
  INSERT lv_mime_text INTO TABLE ir_me->dr_t_required.

  " where insert new data (faster than CALL TRANSFORMATION)
  DATA lv_from     TYPE i.
  DATA lv_from_rel TYPE i.

  " Initialize
  " №1 - Image
  IF ir_me->dr_v_drawing IS INITIAL.
    CONCATENATE
      `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
      `<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">`
      `</xdr:wsDr>` INTO ir_me->dr_v_drawing.
  ENDIF.
  lv_from = strlen( ir_me->dr_v_drawing ) - strlen( `</xdr:wsDr>`).

  " №2 - Image Relation
  IF ir_me->dr_v_drawing_rel IS INITIAL.
    CONCATENATE
      `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
      `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`
      `</Relationships>` INTO ir_me->dr_v_drawing_rel.
  ENDIF.
  lv_from_rel = strlen( ir_me->dr_v_drawing_rel ) - strlen( `</Relationships>` ).

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
  CONCATENATE ir_me->dr_v_drawing(lv_from) lv_value `</xdr:wsDr>` INTO ir_me->dr_v_drawing.

  " №2 - Add Image Relation
  CHECK lv_rewrite <> abap_true.
  CONCATENATE
    `<Relationship Id="_rImgId`
    lo_comp_cell->mv_index_txt
    `" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/`
    lv_file_name `"/>` INTO lv_value.
  CONCATENATE ir_me->dr_v_drawing_rel(lv_from_rel) lv_value `</Relationships>` INTO ir_me->dr_v_drawing_rel.
ENDMETHOD.


METHOD drawing_get_image_basic.
  " Get comp ref
  DATA lo_comp_cell LIKE ir_cell->c_comp_cell.
  lo_comp_cell = ir_cell->c_comp_cell.

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
  int_2_text lo_comp_cell->mv_width  lv_width_txt.
  int_2_text lo_comp_cell->mv_height lv_height_txt.

**********************************************************************
  " twoCellAnchor if IMAGE matches the cell
  DATA lv_tag   TYPE string.

  IF lo_comp_cell->mv_width IS INITIAL AND lo_comp_cell->mv_height IS INITIAL.
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
    `<xdr:nvPicPr><xdr:cNvPr id="` lo_comp_cell->mv_index_txt `" name="Object_` lo_comp_cell->mv_index_txt `"/><xdr:cNvPicPr><a:picLocks noChangeAspect="1"/></xdr:cNvPicPr></xdr:nvPicPr>`
    `<xdr:blipFill><a:blip xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" r:embed="_rImgId` lo_comp_cell->mv_index_txt `"/><a:stretch><a:fillRect/></a:stretch></xdr:blipFill>`
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
  DATA lo_comp_cell LIKE ir_cell->c_comp_cell.
  lo_comp_cell = ir_cell->c_comp_cell.

  " Change id
  DATA lv_new_txt TYPE string.
  DATA lv_row_1 TYPE i.
  DATA lv_col_1 TYPE i.

  " Name is optional field
  CONCATENATE `NAME_ID_` lo_comp_cell->mv_index_txt INTO lv_new_txt.
  REPLACE FIRST OCCURRENCE OF `{NAME_ID_0}` IN rv_comp_value WITH lv_new_txt.

  " Ref to another files
  CONCATENATE ` r:embed="_rImgId` lo_comp_cell->mv_index_txt `" old_id="` INTO lv_new_txt.
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

  zcl_eui_conv=>xml_from_zip(
   EXPORTING io_zip     = io_zip
             iv_name    = 'xl/drawings/drawing1.xml'        "#EC NOTEXT
   IMPORTING ev_sdoc    = rr_me->dr_v_drawing ).

  zcl_eui_conv=>xml_from_zip(
   EXPORTING io_zip     = io_zip
             iv_name    = 'xl/drawings/_rels/drawing1.xml.rels' "#EC NOTEXT
   IMPORTING ev_sdoc    = rr_me->dr_v_drawing_rel ).

  " Get all tags
  DATA lt_xml_match TYPE zcl_xtt_xml_base=>tt_xml_match.
  DATA ls_img_template TYPE ts_img_template.
  FIELD-SYMBOLS <ls_xml_match> LIKE LINE OF lt_xml_match.

  " All images from the last
  zcl_xtt_xml_base=>get_tag_matches( EXPORTING iv_context   = rr_me->dr_v_drawing
                                               iv_tag       = `xdr:twoCellAnchor`
                                     IMPORTING et_xml_match = lt_xml_match ).
  LOOP AT lt_xml_match ASSIGNING <ls_xml_match>.
    " Entire `xdr:twoCellAnchor`
    ls_img_template-tag = <ls_xml_match>-tag.

    " Get name & ref to cell with {R-T-IMG}
    DATA lv_xtt_name TYPE string.
    zcl_xtt_xml_base=>get_from_tag(
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
      zcl_xtt_xml_base=>get_from_tag(
        EXPORTING
          iv_beg_part = `title="{`                          "#EC NOTEXT
          iv_end_part = `}`
        IMPORTING
          ev_value0   = lv_xtt_name
        CHANGING
          cv_xml      = ls_img_template-tag ).
    ENDIF.

    zcl_xtt_xml_base=>get_from_tag(
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

    zcl_xtt_xml_base=>get_from_tag(
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
    lr_cell = io_sheet->find_cell( lr_cell->* ).
    IF lv_xtt_name IS NOT INITIAL.
      CONCATENATE `{` lv_xtt_name `}` INTO lr_cell->c_value.
    ENDIF.

    " Delete original image from a cell
    CHECK lr_cell->c_value CP `{*}`.
    CONCATENATE rr_me->dr_v_drawing(<ls_xml_match>-beg)
                rr_me->dr_v_drawing+<ls_xml_match>-end INTO rr_me->dr_v_drawing.
  ENDLOOP.
ENDMETHOD.


METHOD drawing_save_xml.
  CHECK ir_me->dr_v_drawing IS NOT INITIAL
    AND ir_me->dr_t_required IS NOT INITIAL.

  zcl_eui_conv=>xml_to_zip(
   io_zip     = io_zip
   iv_name    = 'xl/drawings/drawing1.xml'                  "#EC NOTEXT
   iv_sdoc    = ir_me->dr_v_drawing ).

  zcl_eui_conv=>xml_to_zip(
   io_zip     = io_zip
   iv_name    = 'xl/drawings/_rels/drawing1.xml.rels'       "#EC NOTEXT
   iv_sdoc    = ir_me->dr_v_drawing_rel ).

  " Change files info
  DATA lv_content_xml TYPE string.
  zcl_eui_conv=>xml_from_zip(
   EXPORTING io_zip     = io_zip
             iv_name    = '[Content_Types].xml'             "#EC NOTEXT
   IMPORTING ev_sdoc    = lv_content_xml ).

  " Add 1 by one
  DATA lv_req_text TYPE string.
  DATA lv_len      TYPE i.
  LOOP AT ir_me->dr_t_required INTO lv_req_text.
    " Do not insert if already exist
    CHECK lv_content_xml NS lv_req_text.

    lv_len = strlen( lv_content_xml ) - strlen( `</Types>` ).
    CONCATENATE lv_content_xml(lv_len) lv_req_text `</Types>` INTO lv_content_xml.
  ENDLOOP.

  " And add
  zcl_eui_conv=>xml_to_zip(
   io_zip     = io_zip
   iv_name    = '[Content_Types].xml'                       "#EC NOTEXT
   iv_sdoc    = lv_content_xml ).

  " In sheet Read relations
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip    = io_zip
    iv_name   = io_sheet->mv_rel_path " <--- sheet index
   IMPORTING
    ev_sdoc   = lv_content_xml ).

  " Has ref to file ?
  CHECK lv_content_xml NS `Target="../drawings/drawing1.xml"/>`.

  " New empty file
  DATA lv_empty TYPE abap_bool.
  IF lv_content_xml IS INITIAL.
    lv_empty = abap_true.
    lv_content_xml =
    `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"></Relationships>`.
  ENDIF.

  " Add ref
  lv_len = strlen( lv_content_xml ) - strlen( `</Relationships>` ).
  CONCATENATE
   lv_content_xml(lv_len)
   `<Relationship Id="_rDrawId" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing" Target="../drawings/drawing1.xml"/>`
   `</Relationships>` INTO lv_content_xml.

  " And add relation
  zcl_eui_conv=>xml_to_zip(
   io_zip     = io_zip
   iv_name    = io_sheet->mv_rel_path
   iv_sdoc    = lv_content_xml ).

  " Add ref
  IF lv_empty = abap_true.
    REPLACE FIRST OCCURRENCE OF `</worksheet>` IN cv_sheet_xml WITH `<drawing r:id="_rDrawId"/></worksheet>`.
  ELSE.
    " TODO before any ID in 'sheet1.xml.rels'
    REPLACE FIRST OCCURRENCE OF `<tableParts ` IN cv_sheet_xml WITH `<drawing r:id="_rDrawId"/><tableParts `.
  ENDIF.
ENDMETHOD.


METHOD get_raw.
  DATA:
    lo_sheet   TYPE REF TO lcl_ex_sheet,
    lv_val     TYPE i,
    l_off      TYPE i,
    l_str      TYPE string,
    lr_content TYPE REF TO xstring.

***************************************
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
  lv_val = lines( mt_shared_strings ).
  int_to_text lv_val.

  " Header
  CONCATENATE `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
              `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="`
                 lv_val_txt `" uniqueCount="` lv_val_txt `">` INTO lv_val_txt.

  " Body
  LOOP AT mt_shared_strings INTO l_str.
    l_off = strlen( l_str ) - 1.
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

***************************************
  " Save every sheet
  LOOP AT mt_sheets INTO lo_sheet.
    lo_sheet->save( me ).
  ENDLOOP.

***************************************
  " Save cell movements
  defined_name_save_xml( io_zip           = mo_zip
                         it_defined_names = mt_defined_names ).

***************************************
  " Remove from ZIP
  mo_zip->delete( EXPORTING name    = 'xl/calcChain.xml'    "#EC NOTEXT
                  EXCEPTIONS OTHERS = 0 ).
  " ZIP archive as xstring
  rv_content = mo_zip->save( ).

  " Change content in special cases
  GET REFERENCE OF rv_content INTO lr_content.
  RAISE EVENT prepare_raw
   EXPORTING
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
    lo_rels        TYPE REF TO if_ixml_document,
    lo_rel         TYPE REF TO if_ixml_element,
    l_val          TYPE string,
    ls_list_object TYPE ts_ex_list_object,
    lo_area        TYPE REF TO if_ixml_element,
    ls_area_ref    TYPE REF TO ts_ex_area.

  " Read relations
  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip    = io_zip
    iv_name   = io_sheet->mv_rel_path
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
      io_zip    = io_zip
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
    io_sheet->replace_with_new( ir_area     = ls_area
                                it_cell_ref = it_cell_ref ).

    lv_address = area_get_address(
     is_area     = ls_area
     iv_no_bucks = abap_true ).
    CHECK lv_address IS NOT INITIAL.

    " Change area
    lo_table = ls_list_object->dom->get_root_element( ).
    lo_table->set_attribute( name  = 'ref'                  "#EC NOTEXT
                             value = lv_address ).

    " Replace in zip
    zcl_eui_conv=>xml_to_zip(
     io_zip     = io_zip
     iv_name    = ls_list_object->arc_path
     io_xmldoc  = ls_list_object->dom ).
  ENDLOOP.
ENDMETHOD.


METHOD merge.
  DATA lo_replace_block TYPE REF TO zcl_xtt_replace_block.
  DATA lo_sheet         TYPE REF TO lcl_ex_sheet.

  " Update each sheet
  LOOP AT mt_sheets INTO lo_sheet.
    " Prepare for replacement
    CREATE OBJECT lo_replace_block
      EXPORTING
        is_block      = is_block
        iv_block_name = iv_block_name.

    " Find trees in file
    lo_replace_block->extra_create_tree( lo_sheet->mt_extra_tab_opt ).

    lo_sheet->merge(
     EXPORTING
      io_replace_block = lo_replace_block
     CHANGING
      ct_cells         = lo_sheet->mt_cells ).
  ENDLOOP.

  " For chain calls
  ro_xtt = me.
ENDMETHOD.


METHOD row_read_xml.
  DATA lo_row   TYPE REF TO if_ixml_element.
  DATA ls_row   LIKE LINE OF io_sheet->mt_rows.
  DATA lo_cell  TYPE REF TO if_ixml_element.

  lo_row ?= io_sheet->mo_dom->find_from_name( 'row' ).      "#EC NOTEXT

  " Add new rows
  WHILE lo_row IS BOUND.
    CLEAR ls_row.
    ls_row-r             = lo_row->get_attribute( 'r' ).            "#EC NOTEXT
    ls_row-customheight  = lo_row->get_attribute( 'customHeight' ). "#EC NOTEXT
    ls_row-ht            = lo_row->get_attribute( 'ht' ).           "#EC NOTEXT
    ls_row-hidden        = lo_row->get_attribute( 'hidden' ).       "#EC NOTEXT
    ls_row-outlinelevel  = lo_row->get_attribute( 'outlineLevel' ). "#EC NOTEXT

    " And add by key
    INSERT ls_row INTO TABLE io_sheet->mt_rows.

    " Loop through excels cells
    lo_cell = lo_row->find_from_name( 'c' ).                "#EC NOTEXT
    WHILE lo_cell IS BOUND.
      cell_read_xml(
       EXPORTING
        io_node           = lo_cell
        it_shared_strings	= it_shared_strings
       CHANGING
        ct_cells          = io_sheet->mt_cells ).

      " Add by key
      "INSERT ls_cell INTO TABLE lt_cells_hash.
      lo_cell ?= lo_cell->get_next( ).
    ENDWHILE.

    lo_row ?= lo_row->get_next( ).
  ENDWHILE.
ENDMETHOD.


METHOD row_write_xml.
  DATA:
    lv_outline TYPE string.

  " To text
  int_to_text iv_new_row.

  " Write attributes
  CONCATENATE cv_sheet_data `<row`
   ` r="`            iv_new_row_txt            `"` INTO cv_sheet_data. "#EC NOTEXT

  " if im_row->customheight = 1 then height = im_row->ht
  IF is_row->ht IS NOT INITIAL AND is_row->customheight IS NOT INITIAL.
    CONCATENATE cv_sheet_data
    ` customHeight="` is_row->customheight `"`
    ` ht="`           is_row->ht           `"` INTO cv_sheet_data. "#EC NOTEXT
  ENDIF.

  " ht = 0
  IF is_row->hidden IS NOT INITIAL.
    CONCATENATE cv_sheet_data
    ` hidden="`       is_row->hidden       `"` INTO cv_sheet_data. "#EC NOTEXT
  ENDIF.

  " + sign
  IF iv_outline_level IS NOT INITIAL.
    lv_outline = iv_outline_level.
    CONDENSE lv_outline.
  ELSEIF is_row->outline_skip <> abap_true.
    lv_outline = is_row->outlinelevel.
  ENDIF.

  IF lv_outline IS NOT INITIAL.
    CONCATENATE cv_sheet_data
    ` outlineLevel="` lv_outline `"` INTO cv_sheet_data. "#EC NOTEXT
  ENDIF.

  " Closing >
  CONCATENATE cv_sheet_data `>`                INTO cv_sheet_data.
ENDMETHOD.                    "row_write_xml
ENDCLASS.

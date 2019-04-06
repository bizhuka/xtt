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

  data:
    mt_sheets   TYPE STANDARD TABLE OF REF TO cl_ex_sheet .
  data MO_ZIP type ref to CL_ABAP_ZIP .
  data MO_WORKBOOK type ref to IF_IXML_DOCUMENT .
  data MT_SHARED_STRINGS type STRINGTAB .
  data MT_DEFINED_NAMES type TT_EX_DEFINED_NAME .

  class-methods COLUMN_READ_XML
    importing
      !IO_NODE type ref to IF_IXML_ELEMENT
    changing
      !CT_COLUMNS type TT_EX_COLUMN .
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
  methods CELL_READ_XML
    importing
      !IO_NODE type ref to IF_IXML_ELEMENT
    changing
      !CT_CELLS type TT_EX_CELL .
  methods CELL_WRITE_XML
    importing
      !IS_CELL type ref to TS_EX_CELL
      !IV_NEW_ROW type I
      !IV_NEW_COL_IND type I
    changing
      !CV_SHEET_DATA type STRING
      !CV_MERGE_CNT type I
      !CV_MERGE_CELLS type STRING .
  class-methods ROW_READ_XML
    importing
      !IO_NODE type ref to IF_IXML_ELEMENT
    changing
      !CT_ROWS type TT_EX_ROW .
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
      !IO_NODE type ref to IF_IXML_ELEMENT
    changing
      !CT_DEFINED_NAMES type TT_EX_DEFINED_NAME .
  methods LIST_OBJECT_READ_XML
    importing
      !IV_PATH type STRING
    changing
      !CT_LIST_OBJECTS type TT_EX_LIST_OBJECT .
  class-methods GET_WITHOUT_T
    importing
      !IV_TEXT type STRING
    returning
      value(RV_TEXT) type STRING .
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
    zcl_xtt_excel_xlsx=>cell_init(
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


METHOD CELL_INIT.
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
    is_cell->c_col_ind = cl_ex_sheet=>convert_column2int( is_cell->c_col ).
  ENDMETHOD.                    "cell_init


METHOD CELL_IS_STRING.
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
  zcl_xtt_excel_xlsx=>cell_init(
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
      READ TABLE mt_shared_strings INTO ls_cell->c_value INDEX l_ind.

    WHEN 'inlineStr'.
      ls_cell->c_value = l_val. " <si> tag zcl_xtt_excel_xlsx=>get_without_t(  ).
      ls_cell->c_type  = 's'.

    WHEN OTHERS.
      ls_cell->c_value = l_val.

  ENDCASE.
ENDMETHOD.                    "cell_read_xml


METHOD cell_write_xml.
  DATA:
    lv_new_row   TYPE char10,
    lv_new_col   TYPE char3,
    lv_number    TYPE char10,
    lv_data      TYPE string,
    lv_index     TYPE i,
    lv_merge_col TYPE char3,
    lv_from      TYPE string,
    lv_to        TYPE string.

  " Update formula
  DEFINE replace_all_with_buck.
    CONCATENATE:
     `$` &1 INTO lv_from,
     `$` &2 INTO lv_to.

    REPLACE ALL OCCURRENCES OF lv_from IN is_cell->c_formula WITH lv_to.
  END-OF-DEFINITION.

  " To text
  int_2_text iv_new_row lv_new_row.
  lv_new_col = cl_ex_sheet=>convert_column2alpha( iv_new_col_ind ).

  " Formula
  IF is_cell->c_formula IS NOT INITIAL.
    " Replace to new row index
    FIND FIRST OCCURRENCE OF '$' IN is_cell->c_formula.
    IF sy-subrc = 0.
      " Old row index -> New row index
      int_2_text is_cell->c_row lv_number.

      replace_all_with_buck lv_number lv_new_row.
      IF sy-subrc <> 0.
        replace_all_with_buck is_cell->c_col lv_new_col.
      ENDIF.
    ENDIF.
    " Write formula
    CONCATENATE `<f>` is_cell->c_formula `</f>` INTO lv_data.
  ELSEIF is_cell->c_value IS NOT INITIAL.
    " String
    IF zcl_xtt_excel_xlsx=>cell_is_string( is_cell ) = abap_true.
      READ TABLE mt_shared_strings TRANSPORTING NO FIELDS BINARY SEARCH
       WITH KEY table_line = is_cell->c_value.
      " Write index of strings
      lv_index = sy-tabix - 1.
      int_2_text lv_index lv_number.
      CONCATENATE `<v>` lv_number         `</v>` INTO lv_data.
    ELSE. " Numbers
      CONCATENATE `<v>` is_cell->c_value `</v>` INTO lv_data.
    ENDIF.
  ENDIF.

  " Change current row
  is_cell->c_row     = iv_new_row.
  is_cell->c_col     = lv_new_col.
  is_cell->c_col_ind = iv_new_col_ind.

  " Address
  CONCATENATE cv_sheet_data `<c r="` is_cell->c_col lv_new_row `"` INTO cv_sheet_data.

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
    lv_merge_col = cl_ex_sheet=>convert_column2alpha( lv_index  ).

    CONCATENATE cv_merge_cells `<mergeCell ref="`
      is_cell->c_col  lv_new_row `:`
      lv_merge_col    lv_number `"/> ` INTO cv_merge_cells.
  ENDIF.
ENDMETHOD.


METHOD column_read_xml.
  DATA:
   ls_col LIKE LINE OF ct_columns,
   lv_cnt TYPE i.

  ls_col-collapsed    = io_node->get_attribute( 'collapsed' ).    "#EC NOTEXT
  ls_col-customwidth  = io_node->get_attribute( 'customWidth' ).  "#EC NOTEXT
  ls_col-hidden       = io_node->get_attribute( 'hidden' ).       "#EC NOTEXT
  ls_col-max          = io_node->get_attribute( 'max' ).          "#EC NOTEXT
  ls_col-min          = io_node->get_attribute( 'min' ).          "#EC NOTEXT
  ls_col-outlinelevel = io_node->get_attribute( 'outlineLevel' ). "#EC NOTEXT
  ls_col-phonetic     = io_node->get_attribute( 'phonetic' ).     "#EC NOTEXT
  ls_col-style        = io_node->get_attribute( 'style' ).        "#EC NOTEXT
  ls_col-width        = io_node->get_attribute( 'width' ).        "#EC NOTEXT

  IF ls_col-min = ls_col-max.
    INSERT ls_col INTO TABLE ct_columns.
    RETURN.
  ENDIF.

  " Separate to columns
  lv_cnt = ls_col-max - ls_col-min + 1.
  DO lv_cnt TIMES.
    ls_col-max = ls_col-min.
    INSERT ls_col INTO TABLE ct_columns.

    " Next line
    ls_col-min = ls_col-min + 1.
  ENDDO.
ENDMETHOD.


METHOD column_write_xml.
  DATA:
   lv_text     TYPE string.
  FIELD-SYMBOLS:
   <ls_column> LIKE LINE OF it_columns.

  DEFINE add_text.
    lv_text = <ls_column>-&1.
    IF lv_text IS NOT INITIAL.
      CONDENSE lv_text.
      CONCATENATE rv_column_text ` ` &2 `="` lv_text `"` INTO rv_column_text RESPECTING BLANKS.
    ENDIF.
  END-OF-DEFINITION.

  LOOP AT it_columns ASSIGNING <ls_column>.
    CONCATENATE rv_column_text `<col` INTO rv_column_text RESPECTING BLANKS.
    add_text collapsed    'collapsed'.    "#EC NOTEXT
    add_text customwidth  'customWidth'.  "#EC NOTEXT
    add_text hidden       'hidden'.       "#EC NOTEXT
    add_text max          'max'.          "#EC NOTEXT
    add_text min          'min'.          "#EC NOTEXT
    add_text outlinelevel 'outlineLevel'. "#EC NOTEXT
    add_text phonetic     'phonetic'.     "#EC NOTEXT
    add_text style        'style'.        "#EC NOTEXT
    add_text width        'width'.        "#EC NOTEXT
    CONCATENATE rv_column_text `></col>` INTO rv_column_text RESPECTING BLANKS. "#EC NOTEXT
  ENDLOOP.
ENDMETHOD.


METHOD CONSTRUCTOR.
  DATA:
   l_x_value           TYPE xstring,
   l_shared_strings    TYPE string,
   lt_match            TYPE match_result_tab,
   l_cnt               TYPE i,
   l_ind               TYPE i,
   ls_match_beg        TYPE REF TO match_result,
   ls_match_end        TYPE REF TO match_result,
   l_len               TYPE i,
   l_off               TYPE i,
   l_val               TYPE string,
   lo_node             TYPE REF TO if_ixml_element,
   lo_sheet            TYPE REF TO cl_ex_sheet.

  super->constructor( io_file = io_file ).

  " Load zip archive from XSTRING
  CREATE OBJECT mo_zip.
  io_file->get_content(
   IMPORTING
    ev_as_xstring = l_x_value ).
  mo_zip->load( l_x_value ).

***************************************
  " Main work book
  zcl_xtt_util=>xml_from_zip(
   EXPORTING
    io_zip     = mo_zip
    iv_name    = 'xl/workbook.xml'      "#EC NOTEXT
   IMPORTING
    eo_xmldoc  = mo_workbook ).

***************************************
  " Shared strings. Mainly <t> tags
  zcl_xtt_util=>xml_from_zip(
   EXPORTING
    io_zip     = mo_zip
    iv_name    = 'xl/sharedStrings.xml' "#EC NOTEXT
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
    l_val = zcl_xtt_excel_xlsx=>get_without_t( l_val ).

    " Add to table
    APPEND l_val TO mt_shared_strings.
  ENDWHILE.

***************************************
  " Defined names for moving the second cell of areas
  lo_node ?= mo_workbook->find_from_name( 'definedName' ).
  WHILE lo_node IS BOUND.
    " Create new defined name
    defined_name_read_xml(
     EXPORTING
      io_node          = lo_node
     CHANGING
      ct_defined_names = mt_defined_names ).

    lo_node ?= lo_node->get_next( ).
  ENDWHILE.

***************************************
  " Sheets
  lo_node = mo_workbook->find_from_name( 'sheet' ). "#EC NOTEXT
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


METHOD defined_name_read_xml.
  DATA:
    l_value         TYPE string,
    ls_defined_name TYPE REF TO ts_ex_defined_name,
    lt_value        TYPE stringtab.

  " Name & value
  " Regardless CHECK l_value IS NOT INITIAL AND l_value(1) <> '#'.
  APPEND INITIAL LINE TO ct_defined_names REFERENCE INTO ls_defined_name.
  ls_defined_name->d_name = io_node->get_attribute( 'name' ). "#EC NOTEXT
  l_value  = io_node->get_value( ).

  " Several areas
  SPLIT l_value AT ',' INTO TABLE lt_value.
  LOOP AT lt_value INTO l_value.
    area_read_xml(
     EXPORTING
      iv_value = l_value
     CHANGING
      ct_areas = ls_defined_name->d_areas ).
  ENDLOOP.
ENDMETHOD.


METHOD get_raw.
  DATA:
    lo_sheet        TYPE REF TO cl_ex_sheet,
    l_cnt           TYPE i,
    l_off           TYPE i,
    l_val           TYPE string,
    l_str           TYPE string,
    ls_defined_name TYPE REF TO ts_ex_defined_name,
    lo_node         TYPE REF TO if_ixml_element,
    ls_area         TYPE REF TO ts_ex_area,
    l_ref           TYPE string,
    l_part          TYPE string,
    lr_content      TYPE REF TO xstring.

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
  l_cnt  = lines( mt_shared_strings ).
  int_2_text l_cnt l_val.

  " Header
  CONCATENATE `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
              `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="`
                 l_val `" uniqueCount="` l_val `">` INTO l_val.

  " Body
  LOOP AT mt_shared_strings INTO l_str.
    l_off = strlen( l_str ) - 1.
    IF l_str(1) = '<' AND l_str+l_off(1) = '>'.
      CONCATENATE l_val `<si>` l_str `</si>` INTO l_val.
    ELSE.
      CONCATENATE l_val `<si><t>` l_str `</t></si>` INTO l_val.
    ENDIF.
  ENDLOOP.
  CONCATENATE l_val `</sst>` INTO l_val.

  zcl_xtt_util=>xml_to_zip(
   io_zip   = mo_zip
   iv_name  = `xl/sharedStrings.xml`
   iv_sdoc  = l_val ).

***************************************
  " Save every sheet
  LOOP AT mt_sheets INTO lo_sheet.
    lo_sheet->save( me ).
  ENDLOOP.

***************************************
  " Save cell movements
  lo_node ?= mo_workbook->find_from_name( 'definedName' ).
  WHILE lo_node IS BOUND.
    " Do not create new tags
    READ TABLE mt_defined_names REFERENCE INTO ls_defined_name INDEX sy-index.

    " Modify existing
    CLEAR l_ref.
    LOOP AT ls_defined_name->d_areas REFERENCE INTO ls_area.
      l_part = area_get_address( is_area = ls_area iv_no_bucks = abap_false ).
      CHECK l_part IS NOT INITIAL.

      " Concatenate 2 cells
      IF l_ref IS INITIAL.
        l_ref = l_part.
      ELSE.
        CONCATENATE l_ref `,` l_part INTO l_ref.
      ENDIF.
    ENDLOOP.

    IF l_ref IS NOT INITIAL.
      lo_node->set_value( l_ref ).
    ENDIF.

    lo_node ?= lo_node->get_next( ).
  ENDWHILE.

  " Write XML to zip
  IF mt_defined_names IS NOT INITIAL.
    zcl_xtt_util=>xml_to_zip(
     io_zip    = mo_zip
     iv_name   = 'xl/workbook.xml' "#EC NOTEXT
     io_xmldoc = mo_workbook ).
  ENDIF.

***************************************
  " Remove from ZIP
  mo_zip->delete(
   EXPORTING
    name     = 'xl/calcChain.xml'
   EXCEPTIONS
    OTHERS   = 1 ). "#SUBRC_OK delete if exist

  " ZIP archive as xstring
  rv_content = mo_zip->save( ).

  " Change content in special cases
  GET REFERENCE OF rv_content INTO lr_content.
  RAISE EVENT prepare_raw
   EXPORTING
     ir_content = lr_content.
ENDMETHOD.


METHOD GET_WITHOUT_T.
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


METHOD LIST_OBJECT_READ_XML.
    DATA:
     lo_rels          TYPE REF TO if_ixml_document,
     lo_rel           TYPE REF TO if_ixml_element,
     l_val            TYPE string,
     ls_list_object   TYPE ts_ex_list_object,
     lo_area          TYPE REF TO if_ixml_element,
     ls_area_ref      TYPE REF TO ts_ex_area.

    " Read relations
    zcl_xtt_util=>xml_from_zip(
     EXPORTING
      io_zip    = mo_zip
      iv_name   = iv_path
     IMPORTING
      eo_xmldoc = lo_rels ).

    " No relations
    CHECK lo_rels IS NOT INITIAL.

    lo_rel ?= lo_rels->find_from_name( `Relationship` ). "#EC NOTEXT
    WHILE lo_rel IS BOUND.
      l_val = lo_rel->get_attribute( `Target` ). "#EC NOTEXT
      lo_rel ?= lo_rel->get_next( ).
      CHECK l_val CP `../tables/*`. "#EC NOTEXT

      " Path to table
      CLEAR ls_list_object.
      CONCATENATE `xl/tables/` l_val+10 INTO ls_list_object-arc_path. "#EC NOTEXT

      " Dom
      zcl_xtt_util=>xml_from_zip(
       EXPORTING
        io_zip    = mo_zip
        iv_name   = ls_list_object-arc_path
       IMPORTING
        eo_xmldoc = ls_list_object-dom ).

      " Get address
      lo_area ?= ls_list_object-dom->get_first_child( ).
      l_val = lo_area->get_attribute( 'ref' ). "#EC NOTEXT

      " Edit area
      GET REFERENCE OF ls_list_object-area INTO ls_area_ref.
      zcl_xtt_excel_xlsx=>area_read_xml(
       iv_value = l_val
       is_area  = ls_area_ref ).

      " Add to list if Ok
      CHECK ls_area_ref->a_cells IS NOT INITIAL.
      APPEND ls_list_object TO ct_list_objects.
    ENDWHILE.
  ENDMETHOD.                    "list_object_read_xml


METHOD MERGE.
  DATA:
    lo_replace_block TYPE REF TO zcl_xtt_replace_block,
    lo_sheet         TYPE REF TO cl_ex_sheet.

  " Prepare for replacement
  CREATE OBJECT lo_replace_block
    EXPORTING
      is_block      = is_block
      iv_block_name = iv_block_name.

  " Update each sheet
  LOOP AT mt_sheets INTO lo_sheet.
    lo_sheet->merge(
     EXPORTING
      io_replace_block = lo_replace_block
     CHANGING
      ct_cells         = lo_sheet->mt_cells ).
  ENDLOOP.
ENDMETHOD.


METHOD row_read_xml.
  DATA:
   ls_row LIKE LINE OF ct_rows.

  ls_row-r             = io_node->get_attribute( 'r' ).             "#EC NOTEXT
  ls_row-customheight  = io_node->get_attribute( 'customHeight' ).  "#EC NOTEXT
  ls_row-ht            = io_node->get_attribute( 'ht' ).            "#EC NOTEXT
  ls_row-hidden        = io_node->get_attribute( 'hidden' ).        "#EC NOTEXT
  ls_row-outlinelevel  = io_node->get_attribute( 'outlineLevel' ).  "#EC NOTEXT

  " And add by key
  INSERT ls_row INTO TABLE ct_rows.
ENDMETHOD.


METHOD row_write_xml.
  DATA:
    l_new_row  TYPE char10,
    lv_outline TYPE string.

  " To text
  int_2_text iv_new_row l_new_row.

  " Write attributes
  CONCATENATE cv_sheet_data `<row`
   ` r="`            l_new_row            `"` INTO cv_sheet_data. "#EC NOTEXT

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

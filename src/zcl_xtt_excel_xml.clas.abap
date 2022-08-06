class ZCL_XTT_EXCEL_XML definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

*"* public components of class ZCL_XTT_EXCEL_XML
*"* do not include other source files here!!!
  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_OLE_EXT type STRING default ZCL_EUI_FILE=>MC_EXTENSION-XLSX
      !IV_OLE_EXT_FORMAT type I default 51 .
protected section.

  methods BOUNDS_FROM_BODY
    redefinition .
  methods ON_MATCH_FOUND
    redefinition .
  methods _LOGGER_AS_XML
    redefinition .
private section.
*"* private components of class ZCL_XTT_EXCEL_XML
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_XTT_EXCEL_XML IMPLEMENTATION.


METHOD bounds_from_body.
  rs_bounds = super->bounds_from_body( iv_context       = iv_context
                                       iv_root_is_table = iv_root_is_table
                                       iv_block_name    = iv_block_name ).

  rs_bounds-with_tag       = iv_root_is_table.
  CHECK rs_bounds-with_tag = abap_true.

  DATA:
    lv_pattern TYPE string,
    lv_text    TYPE string,
    lv_tabix   TYPE sytabix.
  FIELD-SYMBOLS:
   <ls_match>  LIKE LINE OF rs_bounds-t_match.

  " Detect bounds
  CONCATENATE `<Worksheet*ss:Name="` zcl_xtt_replace_block=>mc_block-open iv_block_name `*` INTO lv_pattern.
  LOOP AT rs_bounds-t_match ASSIGNING <ls_match>.
    lv_tabix = sy-tabix.

    " Skip previous `</Worksheet>`
    CHECK <ls_match>-length <> 12.

    " First with pattern
    lv_text = iv_context+<ls_match>-offset(<ls_match>-length).
    CHECK lv_text CP lv_pattern.

    rs_bounds-first_match = lv_tabix.
    rs_bounds-last_match  = rs_bounds-first_match + 1.
    EXIT.
  ENDLOOP.
ENDMETHOD.


METHOD constructor.
  super->constructor(
   io_file            = io_file
   iv_body_tag        = 'Worksheet'                         "#EC NOTEXT
   iv_row_tag         = 'Row'                               "#EC NOTEXT
   iv_ole_ext         = iv_ole_ext
   iv_ole_ext_format  = iv_ole_ext_format
   iv_line_break      = '&#10;'                             "#EC NOTEXT
   ).

  " REPLACE ALL OCCURRENCES OF `.xml` IN mv_file_name WITH `.xls` IGNORING CASE.

  " Delete row count in sheet and row index (Delete blank rows)
  REPLACE ALL OCCURRENCES OF REGEX:
   `(<Table [^>]+)\bss:ExpandedRowCount="[^"]*"` IN mv_file_content WITH `$1`, "#EC NOTEXT
   `(<Row [^>]+)\bss:Index="[^"]*"`              IN mv_file_content WITH `$1`. "#EC NOTEXT
ENDMETHOD.


METHOD on_match_found.
  CONSTANTS:
    c_date_start      TYPE d VALUE '18991231'.
  DATA:
    l_start           TYPE i,
    lv_cell_block_end TYPE string,
    lv_date           TYPE d,
    lv_time           TYPE t.

  " If the exactly in one cell and need number
  l_start = iv_pos_beg - 8.
  DO 1 TIMES.
    " original type is string
    CHECK cv_content+l_start(6)    = 'String'.             "#EC NO_TEXT

    " ends with template delimiter (Value takes exactly whole cell)
    CONCATENATE zcl_xtt_replace_block=>mc_block-close  '</' INTO lv_cell_block_end.
    CHECK cv_content+iv_pos_end(3) = lv_cell_block_end.

    " Change offset
    iv_pos_beg = iv_pos_beg - 8.

    " Change type
    CASE is_field->typ.
        " Datetime
      WHEN zcl_xtt_replace_block=>mc_type-datetime.
        FIELD-SYMBOLS <lv_string> TYPE csequence.
        ASSIGN is_field->dref->* TO <lv_string>.

        FIELD-SYMBOLS <lv_date>   TYPE d.
        FIELD-SYMBOLS <lv_time>   TYPE t.
        ASSIGN <lv_string>(8)     TO <lv_date> CASTING.
        ASSIGN <lv_string>+8(6)   TO <lv_time> CASTING.

        " Both parts
        lv_date = <lv_date>.
        lv_time = <lv_time>.
        me->mv_prefix = 'DateTime">'.                      "#EC NO_TEXT

        " Date
      WHEN zcl_xtt_replace_block=>mc_type-date.
        ASSIGN is_field->dref->* TO <lv_date>.
        lv_date = <lv_date>.
        me->mv_prefix = 'DateTime">'.                      "#EC NO_TEXT

        " Time
      WHEN zcl_xtt_replace_block=>mc_type-time.
        ASSIGN is_field->dref->* TO <lv_time>.
        lv_time = <lv_time>.
        me->mv_prefix = 'DateTime">'.                       "#EC NOTEXT

        " Boolean
      WHEN zcl_xtt_replace_block=>mc_type-boolean.
        me->mv_prefix = 'Boolean">'.                        "#EC NOTEXT

        " Number
      WHEN zcl_xtt_replace_block=>mc_type-integer OR zcl_xtt_replace_block=>mc_type-double.
        me->mv_prefix = 'Number">'.                         "#EC NOTEXT

        " Just string
      WHEN OTHERS.
        iv_pos_beg = iv_pos_beg + 8.
    ENDCASE.

    " For date and time
    CHECK me->mv_prefix = 'DateTime">'.                     "#EC NOTEXT

    IF <lv_time> IS ASSIGNED AND lv_date IS INITIAL.
      lv_date = c_date_start.
    ENDIF.

    IF lv_date <= c_date_start AND lv_date IS NOT INITIAL  AND
       is_field->typ <> zcl_xtt_replace_block=>mc_type-time." Date & datetime
      CLEAR me->mv_value. " treat as a string
      me->mv_prefix = `String">`.                           "#EC NOTEXT
    ELSE.
      CONCATENATE lv_date(4) `-` lv_date+4(2) `-` lv_date+6(2) `T`
                  lv_time(2) `:` lv_time+2(2) `:` lv_time+4(2) INTO me->mv_value.
      IF `1899-12-31T00:00:00` = me->mv_value OR `0000-00-00T00:00:00` = me->mv_value.
        me->mv_value  = ` `. " Do not reaplace MV_VALUE
        me->mv_prefix = `String">`.
      ENDIF.
    ENDIF.
  ENDDO.

  super->on_match_found(
   EXPORTING
     is_field   = is_field
     iv_pos_beg = iv_pos_beg
     iv_pos_end = iv_pos_end
   CHANGING
     cv_content = cv_content ).
ENDMETHOD.


METHOD _logger_as_xml.
  DATA lv_row TYPE string.
  CONCATENATE
      `<Row>`
        `<Cell><Data ss:Type="String">{MSGTY}</Data></Cell>`
        `<Cell><Data ss:Type="String">{MSGID}</Data></Cell>`
        `<Cell><Data ss:Type="Number">{MSGNO}</Data></Cell>`
        `<Cell><Data ss:Type="String">{MSGLI}</Data></Cell>`
      `</Row>` INTO lv_row.
  rs_log_xml = super->_logger_as_xml( lv_row ).
  CHECK rs_log_xml IS NOT INITIAL.

  DATA lv_color TYPE string.
  IF rs_log_xml-has_axe = abap_true.
    lv_color = `<TabColorIndex>10</TabColorIndex>`.
  ENDIF.

  CONCATENATE:
    `</Worksheet><Worksheet ss:Name="` rs_log_xml-title(31) `">`
      `<Table ss:ExpandedColumnCount="4" x:FullColumns="1" x:FullRows="1" ss:DefaultRowHeight="15">`
       `<Column ss:AutoFitWidth="0" ss:Width="52.5" ss:Span="2"/>`
       `<Column ss:Index="4" ss:AutoFitWidth="0" ss:Width="577.5"/>`
       `<Row>`
        `<Cell><Data ss:Type="String">Type</Data></Cell>`
        `<Cell><Data ss:Type="String">Class</Data></Cell>`
        `<Cell><Data ss:Type="String">Number</Data></Cell>`
        `<Cell><Data ss:Type="String">Message</Data></Cell>`
       `</Row>`      INTO rs_log_xml-before,

     `</Table>`
        `<WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">`
        lv_color
       `</WorksheetOptions>`
       INTO rs_log_xml-after.
ENDMETHOD.
ENDCLASS.

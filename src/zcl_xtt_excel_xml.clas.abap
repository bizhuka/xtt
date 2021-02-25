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
   iv_body_tag        = 'Worksheet' "#EC NOTEXT
   iv_row_tag         = 'Row'       "#EC NOTEXT
   iv_ole_ext         = iv_ole_ext
   iv_ole_ext_format  = iv_ole_ext_format ).

  " REPLACE ALL OCCURRENCES OF `.xml` IN mv_file_name WITH `.xls` IGNORING CASE.

  " Delete row count in sheet and row index (Delete blank rows)
  REPLACE ALL OCCURRENCES OF REGEX:
   `(<Table [^>]+)\bss:ExpandedRowCount="[^"]*"` IN mv_file_content WITH `$1`, "#EC NOTEXT
   `(<Row [^>]+)\bss:Index="[^"]*"`              IN mv_file_content WITH `$1`. "#EC NOTEXT
ENDMETHOD.


METHOD on_match_found.
  DATA:
    l_start           TYPE i,
    lv_cell_block_end TYPE string.
  FIELD-SYMBOLS:
    <l_string> TYPE csequence,
    <l_date>   TYPE d,
    <l_time>   TYPE t.

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
        ASSIGN is_field->dref->* TO <l_string>.

        " Both parts
        ASSIGN <l_string>(8)     TO <l_date> CASTING.
        ASSIGN <l_string>+8(6)   TO <l_time> CASTING.
        me->mv_prefix = 'DateTime">'.                      "#EC NO_TEXT

        " Date
      WHEN zcl_xtt_replace_block=>mc_type-date.
        ASSIGN is_field->dref->* TO <l_date>.
        me->mv_prefix = 'DateTime">'.                      "#EC NO_TEXT

        " Time
      WHEN zcl_xtt_replace_block=>mc_type-time.
        ASSIGN is_field->dref->* TO <l_time>.
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

    " Date only
    IF <l_date> IS ASSIGNED.
      CONCATENATE <l_date>(4) `-` <l_date>+4(2) `-` <l_date>+6(2) `T00:00:00` INTO me->mv_value.
      REPLACE FIRST OCCURRENCE OF `0000-00-00T` IN me->mv_value WITH `1899-12-31T`.
    ENDIF.

    " Concatenate time
    IF <l_time> IS ASSIGNED.
      " Datetime ?
      IF <l_date> IS ASSIGNED.  " Both parts
        CONCATENATE me->mv_value(11)  <l_time>(2) `:` <l_time>+2(2) `:` <l_time>+4(2) INTO me->mv_value.
      ELSE.                     " Just time
        CONCATENATE `1899-12-31T` <l_time>(2) `:` <l_time>+2(2) `:` <l_time>+4(2) INTO me->mv_value.
      ENDIF.
    ENDIF.

    IF     `1899-12-31T00:00:00` = me->mv_value.
      me->mv_value  = ` `.
      me->mv_prefix = `String">`.                           "#EC NOTEXT
      " Use WRITE ... TO
    ELSEIF `1899-12-31T`         > me->mv_value.
      CLEAR me->mv_value.
      me->mv_prefix = `String">`.                           "#EC NOTEXT
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
ENDCLASS.

class ZCL_XTT_EXCEL_XML definition
  public
  inheriting from ZCL_XTT_XML_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_OLE_EXT type STRING default ZCL_EUI_FILE=>MC_EXTENSION-XLSX
      !IV_OLE_EXT_FORMAT type I default 51 .
  class-methods HIDE_EXCEL_WARNING .
protected section.

  methods FIND_BOUNDS
    redefinition .
  methods ON_MATCH_FOUND
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_XTT_EXCEL_XML IMPLEMENTATION.


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


METHOD find_bounds.
  DATA:
    lv_pattern TYPE string,
    lv_text    TYPE string,
    lv_tabix   TYPE sytabix.
  FIELD-SYMBOLS:
   <ls_match>  LIKE LINE OF et_match.

  super->find_bounds(
   EXPORTING
    iv_context              = iv_context
    iv_tag                  = iv_tag
    iv_tag_add              = iv_tag_add
    iv_first_level_is_table = iv_first_level_is_table
    iv_block_name           = iv_block_name
   IMPORTING
    et_match                = et_match
    ev_with_tag             = ev_with_tag
    ev_first_match          = ev_first_match
    ev_last_match           = ev_last_match ).

  CASE iv_tag.
    WHEN mv_body_tag.
      ev_with_tag    = iv_first_level_is_table.
      CHECK ev_with_tag = abap_true.

      " Detect bounds
      CONCATENATE `<Worksheet*ss:Name="` zcl_xtt_replace_block=>mc_char_block_begin iv_block_name `*` INTO lv_pattern.
      LOOP AT et_match ASSIGNING <ls_match>.
        lv_tabix = sy-tabix.

        " Skip previous `</Worksheet>`
        CHECK <ls_match>-length <> 12.

        " First with pattern
        lv_text = iv_context+<ls_match>-offset(<ls_match>-length).
        CHECK lv_text CP lv_pattern.

        ev_first_match = lv_tabix.
        ev_last_match  = ev_first_match + 1.
        EXIT.
      ENDLOOP.

    WHEN mv_row_tag.
      ev_with_tag    = abap_true.

  ENDCASE.
ENDMETHOD.


METHOD hide_excel_warning.
  " Hide the warning for Excel (For Word is ok to change file ext from .xml to .doc)
  " The file you are trying to open, '[filename]', is in a different format than specified by the file extension
  DATA:
    lv_value TYPE string,
    lv_len   TYPE i.                                        "#EC NEEDED

  " Can change registry
  CHECK is_common_gui( ) = abap_true.

  " Get current excel version
  cl_gui_frontend_services=>registry_get_value(
   EXPORTING
    root                 = cl_gui_frontend_services=>hkey_classes_root
    key                  = 'Excel.Application\CurVer'
   IMPORTING
    reg_value            = lv_value
   EXCEPTIONS
    OTHERS               = 1 ).
  CHECK sy-subrc = 0 AND lv_value IS NOT INITIAL.

  " two last digits are excel current version
  lv_len   = strlen( lv_value ). " - 2
  lv_len   = lv_len - 2.
  lv_value = lv_value+lv_len.

  " Write value
  CONCATENATE `Software\Microsoft\Office\` lv_value `.0\Excel\Security` INTO lv_value.
  cl_gui_frontend_services=>registry_set_dword_value(
   EXPORTING
    root                 = cl_gui_frontend_services=>hkey_current_user
    key                  = lv_value
    value                = 'ExtensionHardening'
    dword_value          = 0
   IMPORTING
    rc                   = lv_len
   EXCEPTIONS
    OTHERS               = 1 ).

  CHECK sy-subrc = 0.
  cl_gui_cfw=>flush( ).
ENDMETHOD.


METHOD on_match_found.
  DATA:
    l_start           TYPE i,
    lv_cell_block_end TYPE string.
  FIELD-SYMBOLS:
    <lv_content> TYPE string,
    <l_string>   TYPE csequence,
    <l_date>     TYPE d,
    <l_time>     TYPE t.

  " Current document
  ASSIGN iv_content->* TO <lv_content>.

  " If the exactly in one cell and need number
  l_start = iv_pos_beg - 8.
  DO 1 TIMES.
    " original type is string
    CHECK <lv_content>+l_start(6)    = 'String'. "#EC NO_TEXT

    " ends with template delimiter (Value takes exactly whole cell)
    CONCATENATE zcl_xtt_replace_block=>mc_char_block_end  '</' INTO lv_cell_block_end.
    CHECK <lv_content>+iv_pos_end(3) = lv_cell_block_end.

    " Change offset
    iv_pos_beg = iv_pos_beg - 8.

    " Change type
    CASE is_field->typ.
        " Datetime
      WHEN zcl_xtt_replace_block=>mc_type_datetime.
        ASSIGN is_field->dref->* TO <l_string>.

        " Both parts
        ASSIGN <l_string>(8)     TO <l_date> CASTING.
        ASSIGN <l_string>+8(6)   TO <l_time> CASTING.
        me->mv_prefix = 'DateTime">'. "#EC NO_TEXT

        " Date
      WHEN zcl_xtt_replace_block=>mc_type_date.
        ASSIGN is_field->dref->* TO <l_date>.
        me->mv_prefix = 'DateTime">'. "#EC NO_TEXT

        " Time
      WHEN zcl_xtt_replace_block=>mc_type_time.
        ASSIGN is_field->dref->* TO <l_time>.
        me->mv_prefix = 'DateTime">'. "#EC NOTEXT

        " Boolean
      WHEN zcl_xtt_replace_block=>mc_type_boolean.
        me->mv_prefix = 'Boolean">'. "#EC NOTEXT

        " Number
      WHEN zcl_xtt_replace_block=>mc_type_integer OR zcl_xtt_replace_block=>mc_type_double.
        me->mv_prefix = 'Number">'. "#EC NOTEXT

        " Just string
      WHEN OTHERS.
        iv_pos_beg = iv_pos_beg + 8.
    ENDCASE.

    " For date and time
    CHECK me->mv_prefix = 'DateTime">'. "#EC NOTEXT

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
      me->mv_prefix = `String">`. "#EC NOTEXT
      " Use WRITE ... TO
    ELSEIF `1899-12-31T`         > me->mv_value.
      CLEAR me->mv_value.
      me->mv_prefix = `String">`. "#EC NOTEXT
    ENDIF.
  ENDDO.

  super->on_match_found(
   iv_content = iv_content
   is_field   = is_field
   iv_pos_beg = iv_pos_beg
   iv_pos_end = iv_pos_end ).
ENDMETHOD.
ENDCLASS.

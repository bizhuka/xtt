class ZCL_XTT_COND definition
  public
  final
  create public .

public section.

  data MV_ROOT_TYPE type STRING .

  methods CONSTRUCTOR
    importing
      !IO_XTT type ref to ZCL_XTT .
  class-methods UNESCAPE
    importing
      !IV_TEXT type STRING
    returning
      value(RV_TEXT) type STRING .
  methods CALC_MATCHES
    importing
      !IO_XTT type ref to ZCL_XTT
      !IV_TABIX type SYTABIX
      !IO_BLOCK type ref to ZCL_XTT_REPLACE_BLOCK
      !IT_SCOPE type ZCL_XTT_SCOPE=>TT_SCOPE optional .
  methods GET_TYPE
    importing
      !IS_DATA type ANY
      !IV_SUFFIX type CSEQUENCE default 'ROW'
      !IV_IS_TOP type ABAP_BOOL default ABAP_TRUE
    exporting
      !EV_TYPE type CSEQUENCE .
  methods MAKE_TREE_FORMS
    exporting
      !EV_PROG type PROGRAMM
    changing
      !CT_ROW_OFFSET type ZCL_XTT_TREE_FUNCTION=>TT_ROW_OFFSET .
protected section.
private section.

  types:
    BEGIN OF ts_match,
      cid  TYPE string,
      cond TYPE string,
      type TYPE string,
      form TYPE string,
    END OF ts_match .
  types:
    tt_match TYPE SORTED TABLE OF ts_match WITH UNIQUE KEY cid .

  data MO_XTT type ref to ZCL_XTT .
  data MT_ABAP_CODE type STRINGTAB .
  data MT_MATCH type TT_MATCH .
  data MV_PROG type PROGRAMM .
  data MV_TOP_IS_DDIC_TYPE type ABAP_BOOL .

  methods _MAKE_COND_FORMS .
  methods _READ_SCOPES
    importing
      !IT_SCOPE type ZCL_XTT_SCOPE=>TT_SCOPE .
  methods _GENERATE .
  methods _702_COND
    importing
      !IV_VALUE type STRING
    returning
      value(RT_CODE) type STRINGTAB .
  methods _702_CONCAT
    importing
      !IV_VALUE type STRING
    returning
      value(RT_CODE) type STRINGTAB .
  methods _702_BLOCK
    importing
      !IV_VALUE type STRING
    returning
      value(RT_CODE) type STRINGTAB .
ENDCLASS.



CLASS ZCL_XTT_COND IMPLEMENTATION.


METHOD calc_matches.
  IF it_scope IS NOT INITIAL.
    _read_scopes( it_scope ).
    get_type( EXPORTING is_data = io_block->ms_ext-dref
              IMPORTING ev_type = mv_root_type ).
    _make_cond_forms( ).
  ENDIF.

  " No dynamic fields
  CHECK mt_match[] IS NOT INITIAL.

  " CREATE DATA types
  TYPES integer   TYPE int4.
  TYPES double    TYPE p LENGTH 16 DECIMALS 5.
  TYPES date      TYPE d.
  TYPES time      TYPE t.
  TYPES boolean   TYPE abap_bool.
  TYPES datetime  TYPE c LENGTH 14.
*  TYPES string    TYPE string.
  TYPES: BEGIN OF ts_block,
           ok TYPE i,
         END OF ts_block.
  TYPES block TYPE STANDARD TABLE OF ts_block WITH DEFAULT KEY. " just any table

  FIELD-SYMBOLS <ls_root>   TYPE any.
  FIELD-SYMBOLS <lv_result> TYPE any.
  ASSIGN io_block->ms_ext-dref->* TO <ls_root>.

  FIELD-SYMBOLS <ls_match> LIKE LINE OF mt_match.
  LOOP AT mt_match ASSIGNING <ls_match>.
    DATA ls_field LIKE LINE OF io_block->mt_fields.
    ls_field-name = <ls_match>-cid.
    ls_field-typ  = <ls_match>-type.

    " Just make it table
    IF ls_field-typ = zcl_xtt_replace_block=>mc_type-block.
      ls_field-typ = zcl_xtt_replace_block=>mc_type-table.
    ENDIF.

    " Use pseudo types
    CREATE DATA ls_field-dref TYPE (<ls_match>-type).
    ASSIGN ls_field-dref->* TO <lv_result>.

    DATA lo_error TYPE REF TO zcx_eui_no_check.
    TRY.
        sy-tabix = iv_tabix.
        PERFORM (<ls_match>-form) IN PROGRAM (mv_prog) IF FOUND
          USING
                <ls_root>
          CHANGING
                <lv_result>.
      CATCH zcx_eui_no_check INTO lo_error.
        MESSAGE w025(zsy_xtt) WITH <ls_match>-cond INTO sy-msgli.
        io_xtt->add_log_message( iv_syst = abap_true ).
        " tech info
        io_xtt->add_log_message( io_exception = lo_error
                                 iv_msgty     = 'W' ).
    ENDTRY.

    INSERT ls_field INTO TABLE io_block->mt_fields.
    IF sy-subrc <> 0.
      MESSAGE e021(zsy_xtt) WITH ls_field-name INTO sy-msgli.
      zcx_eui_no_check=>raise_sys_error( ).
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD constructor.
  mo_xtt = io_xtt.
ENDMETHOD.


METHOD get_type.
  DATA ls_field_ext TYPE zcl_xtt_replace_block=>ts_field_ext.
  ls_field_ext =  zcl_xtt_replace_block=>_get_field_ext( io_xtt        = mo_xtt
                                                         is_block      = is_data
                                                         iv_block_name = iv_suffix ).

  DATA lo_type LIKE ls_field_ext-desc.
  lo_type = ls_field_ext-desc.

  " Simple case declared in SE11
  CLEAR ev_type.
  IF lo_type->is_ddic_type( ) = abap_true.
    ev_type = lo_type->get_relative_name( ).
    IF iv_is_top = abap_true.
      mv_top_is_ddic_type = abap_true.
    ENDIF.
  ELSE.
    DATA lv_len TYPE string.
    DATA lv_dec TYPE string.

    lv_len = lo_type->length.
    lv_dec = lo_type->decimals.

    CASE lo_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
        ev_type = 'INT4'.

      WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
        lv_len = lv_len / 2.
        CONCATENATE `n LENGTH ` lv_len INTO ev_type.

      WHEN cl_abap_typedescr=>typekind_char OR cl_abap_typedescr=>typekind_clike.
        CONCATENATE `c LENGTH ` lv_len INTO ev_type.

      WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
           '/' OR 'a' OR 'e'. " typekind_decfloat OR typekind_decfloat16 OR typekind_decfloat34.
        ev_type = `p LENGTH 16 DECIMALS 5`.

      WHEN cl_abap_typedescr=>typekind_date.
        ev_type = 'd'.

      WHEN cl_abap_typedescr=>typekind_time.
        ev_type = 't'.

        " No trunsformation for STRING
      WHEN cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_w OR
           " Binary data in template? Dump ?
           cl_abap_typedescr=>typekind_hex OR cl_abap_typedescr=>typekind_xsequence OR cl_abap_typedescr=>typekind_xstring.
        ev_type = 'STRING'.

        " Special case for objects
      WHEN cl_abap_typedescr=>typekind_intf OR cl_abap_typedescr=>typekind_class OR cl_abap_typedescr=>typekind_oref.
        " Local classes ?
        RETURN.

        " Structures
      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
        " Declare in code
        DATA lt_code LIKE mt_abap_code.
        CONCATENATE `TS_` iv_suffix INTO ev_type.

        " Line by line
        DATA lv_line TYPE string.
        CONCATENATE `TYPES: BEGIN OF TS_` iv_suffix `,` INTO lv_line.
        APPEND lv_line TO lt_code.

        DATA lo_sdesc TYPE REF TO cl_abap_structdescr.
        lo_sdesc ?= lo_type.

        " Get structure data
        FIELD-SYMBOLS <ls_struc> TYPE any.
        ASSIGN ls_field_ext-dref->* TO <ls_struc>.

        " Add every field
        FIELD-SYMBOLS <ls_comp>  TYPE abap_compdescr.
        " TODO ref ???
        LOOP AT lo_sdesc->components ASSIGNING <ls_comp> WHERE type_kind <> cl_abap_typedescr=>typekind_dref.
          " data
          FIELD-SYMBOLS <ls_sub_fld> TYPE any.
          ASSIGN COMPONENT sy-tabix OF STRUCTURE <ls_struc> TO <ls_sub_fld>.

          DATA lv_suffix TYPE string.
          CONCATENATE iv_suffix `_` <ls_comp>-name INTO lv_suffix.

          " Add sub field
          DATA lv_sub_type TYPE string.
          get_type( EXPORTING is_data   = <ls_sub_fld>
                              iv_suffix = lv_suffix
                              iv_is_top = abap_false
                    IMPORTING ev_type   = lv_sub_type ).

          CONCATENATE <ls_comp>-name ` TYPE ` lv_sub_type `,` INTO lv_line. "#EC NOTEXT
          APPEND lv_line TO lt_code.
        ENDLOOP.

        CONCATENATE `END OF TS_` iv_suffix `.` INTO lv_line. " #EC NOTEXT
        APPEND lv_line TO lt_code.
        APPEND '' TO lt_code.

        " Tables
      WHEN cl_abap_typedescr=>typekind_table.
        DATA lo_tdesc TYPE REF TO cl_abap_tabledescr.
        lo_tdesc ?= lo_type.

        " Only stuctures ?
        DATA lo_line TYPE REF TO cl_abap_datadescr.
        lo_line = lo_tdesc->get_table_line_type( ).

        " Create empy line
        DATA lr_line TYPE REF TO data.
        CREATE DATA lr_line TYPE HANDLE lo_line.

        CONCATENATE iv_suffix `_LINE` INTO lv_suffix.
        get_type( EXPORTING is_data   = lr_line
                            iv_suffix = lv_suffix
                            iv_is_top = abap_false
                  IMPORTING ev_type   = lv_sub_type ).

        CASE lo_tdesc->table_kind.
          WHEN lo_tdesc->tablekind_std.
            ev_type = 'STANDARD'.
          WHEN lo_tdesc->tablekind_hashed.
            ev_type = 'HASHED'.
          WHEN lo_tdesc->tablekind_sorted.
            ev_type = 'SORTED'.
          WHEN OTHERS.
        ENDCASE.

        DATA lv_kind TYPE string.
        CASE lo_tdesc->key_defkind.
          WHEN lo_tdesc->keydefkind_default.
            lv_kind = `DEFAULT`.
          WHEN 'E'. " lo_tdesc->keydefkind_empty. " only with kernel >= 7.40
            lv_kind = `EMPTY`.
          WHEN lo_tdesc->keydefkind_user OR lo_tdesc->keydefkind_tableline.
            IF lo_tdesc->has_unique_key = abap_true.
              lv_kind = `UNIQUE`.
            ELSE.
              lv_kind = `NON-UNIQUE`.
            ENDIF.
            DATA lv_key TYPE string.
            CONCATENATE LINES OF lo_tdesc->key INTO lv_key SEPARATED BY space.
          WHEN OTHERS.
        ENDCASE.

        CONCATENATE ev_type ` TABLE OF ` lv_sub_type ` WITH ` lv_kind  ` KEY ` lv_key INTO ev_type.

*TYPEKIND_IREF, TYPEKIND_BREF TYPEKIND_DATA, TYPEKIND_SIMPLE, TYPEKIND_ANY
      WHEN OTHERS.
        MESSAGE e002(zsy_xtt) WITH lo_type->type_kind INTO sy-msgli.
        zcx_eui_no_check=>raise_sys_error( ).
    ENDCASE.
  ENDIF.

  " And add all
  CHECK lt_code IS NOT INITIAL.
  APPEND LINES OF lt_code TO mt_abap_code.
ENDMETHOD.


METHOD make_tree_forms.
  " Global structure
  DATA lv_line TYPE string.
  CONCATENATE `DATA: ROW TYPE ` mv_root_type `.` INTO lv_line.
  APPEND lv_line TO mt_abap_code.                                "#EC NOTEXT
  APPEND '' TO mt_abap_code.

  " Generate FORMs
  FIELD-SYMBOLS <ls_row_off> LIKE LINE OF ct_row_offset.
  LOOP AT ct_row_offset ASSIGNING <ls_row_off> WHERE if_where IS NOT INITIAL. "#EC CI_SORTSEQ

    " Name of FORM
    <ls_row_off>-if_form = sy-tabix.
    CONDENSE <ls_row_off>-if_form.
    CONCATENATE `F_` <ls_row_off>-if_form INTO <ls_row_off>-if_form.

    " FORM declaration
    CONCATENATE `FORM ` <ls_row_off>-if_form ` USING is_row TYPE ANY CHANGING cv_ok TYPE abap_bool.` INTO lv_line. "#EC NOTEXT
    APPEND lv_line TO mt_abap_code.

    APPEND 'MOVE-CORRESPONDING is_row TO row.' TO mt_abap_code.  "#EC NOTEXT

    CONCATENATE `IF ` <ls_row_off>-if_where `. cv_ok = 'X'. ENDIF.` INTO lv_line. "#EC NOTEXT
    APPEND lv_line TO mt_abap_code.

    APPEND 'ENDFORM.' TO mt_abap_code.                           "#EC NOTEXT
    APPEND '' TO mt_abap_code.
  ENDLOOP.

  _generate( ).
  ev_prog = mv_prog.
ENDMETHOD.


METHOD unescape.
  rv_text = iv_text.

  " TODO unescape all? Only for <>
  REPLACE ALL OCCURRENCES OF:
   '&lt;'   IN rv_text WITH '<',
   '&gt;'   IN rv_text WITH '>',
   '&amp;'  IN rv_text WITH '&',
   '&quot;' IN rv_text WITH '"',
   '&apos;' IN rv_text WITH `'`.
ENDMETHOD.


METHOD _702_block.
  DATA lv_code LIKE LINE OF rt_code.

  CONCATENATE `IF ` iv_value `.` INTO lv_code.
  APPEND lv_code TO rt_code.

  APPEND `APPEND INITIAL LINE TO result.` TO rt_code.

  APPEND `ENDIF.` TO rt_code.
ENDMETHOD.


METHOD _702_concat.
  DATA lv_code LIKE LINE OF rt_code.

  IF iv_value CS '&&'.
    CONCATENATE `CONCATENATE ` iv_value ` INTO result.` INTO lv_code. "#EC NOTEXT
    REPLACE ALL OCCURRENCES OF `&&` IN lv_code WITH ``.
  ELSE.
    " Just use =
    CONCATENATE `result = ` iv_value `.` INTO lv_code.      "#EC NOTEXT
  ENDIF.

  APPEND lv_code TO rt_code.
ENDMETHOD.


METHOD _702_cond.
  DATA lv_code LIKE LINE OF rt_code.
*  CONCATENATE `result = COND STRING( ` iv_value ` ).` INTO lv_code. "#EC NOTEXT
*  RETURN.

  " COND -> IF .. ELSEIF
  CONCATENATE iv_value `.ENDIF.` INTO lv_code.
  REPLACE ALL OCCURRENCES OF `THEN`  IN lv_code WITH `. result = `.
  REPLACE FIRST OCCURRENCE OF `ELSE` IN lv_code WITH `. ELSE. result = `.

  REPLACE FIRST OCCURRENCE OF `WHEN` IN lv_code WITH `IF`.
  REPLACE ALL  OCCURRENCES OF `WHEN` IN lv_code WITH `.ELSEIF`.

  APPEND lv_code TO rt_code.
ENDMETHOD.


METHOD _generate.
  " Basic header of app
  INSERT `REPORT DYNAMIC_IF.` INTO mt_abap_code INDEX 1.
  INSERT `TYPE-POOLS ABAP.`   INTO mt_abap_code INDEX 2. " Required for ABAP 7.01
  INSERT ``                   INTO mt_abap_code INDEX 3.

  DATA lo_prog TYPE REF TO zcl_eui_prog.
  CREATE OBJECT lo_prog.
  mv_prog = lo_prog->generate( it_code  = mt_abap_code
                               iv_cprog = `` ).
ENDMETHOD.


METHOD _make_cond_forms.
  " No dynamic fields
  CHECK mt_match[] IS NOT INITIAL.

  " Global structure For move-corresponding
  DATA lv_line      TYPE string.
  DATA lv_form_decl TYPE string.

  IF mv_top_is_ddic_type = abap_true.
    CONCATENATE `value TYPE ` mv_root_type INTO lv_form_decl.
  ELSE.
    lv_form_decl = `root TYPE ANY`.
    CONCATENATE `DATA: VALUE TYPE ` mv_root_type `.` INTO lv_line.
    APPEND lv_line TO mt_abap_code.                         "#EC NOTEXT
  ENDIF.
  APPEND '' TO mt_abap_code.

  FIELD-SYMBOLS <ls_match> LIKE LINE OF mt_match.
  LOOP AT mt_match ASSIGNING <ls_match>.
    " Name of FORM
    <ls_match>-form = sy-tabix.
    CONDENSE <ls_match>-form.
    CONCATENATE `F_` <ls_match>-form INTO <ls_match>-form.

    " FORM declaration
    IF <ls_match>-type = zcl_xtt_replace_block=>mc_type-block.
      lv_line = `STANDARD TABLE`.
    ELSE.
      lv_line = `ANY`.
    ENDIF.
    CONCATENATE `FORM ` <ls_match>-form ` USING ` lv_form_decl ` CHANGING result TYPE ` lv_line `.` INTO lv_line. "#EC NOTEXT
    APPEND lv_line TO mt_abap_code.

    " Use slow MOVE-CORRESPONDING
    IF mv_top_is_ddic_type <> abap_true.
      APPEND 'MOVE-CORRESPONDING root TO value.' TO mt_abap_code. "#EC NOTEXT
    ENDIF.

    DATA lt_line LIKE mt_abap_code.
    IF <ls_match>-type = zcl_xtt_replace_block=>mc_type-block.
      lt_line = _702_block(   <ls_match>-cond ).
    ELSEIF <ls_match>-cond CP 'when*'.
      lt_line = _702_cond(   <ls_match>-cond ).
    ELSE.
      lt_line = _702_concat( <ls_match>-cond ).
    ENDIF.

    APPEND `DATA lo_error TYPE REF TO cx_root.` TO mt_abap_code.
    APPEND `TRY.`                               TO mt_abap_code.
    LOOP AT lt_line INTO lv_line.
      APPEND lv_line                            TO mt_abap_code.
    ENDLOOP.
    APPEND `CATCH cx_root INTO lo_error.`       TO mt_abap_code.
    APPEND ` zcx_eui_no_check=>raise_sys_error( io_error = lo_error ).` TO mt_abap_code.
    APPEND `ENDTRY.`                            TO mt_abap_code.

    APPEND 'ENDFORM.' TO mt_abap_code.                      "#EC NOTEXT
    APPEND '' TO mt_abap_code.
  ENDLOOP.

  _generate( ).
ENDMETHOD.


METHOD _read_scopes.
  FIELD-SYMBOLS <ls_scope> LIKE LINE OF it_scope.
  LOOP AT it_scope ASSIGNING <ls_scope>.
    DATA ls_match LIKE LINE OF mt_match.
    CLEAR ls_match.

    " Defaults
    ls_match-type = zcl_xtt_replace_block=>mc_type-string.
    ls_match-cid  = <ls_scope>-field.

    " Get params
    FIELD-SYMBOLS <ls_pair> LIKE LINE OF <ls_scope>-t_pair.
    LOOP AT <ls_scope>-t_pair ASSIGNING <ls_pair>.
      CASE <ls_pair>-key.
        WHEN 'cond'.
          ls_match-cond = <ls_pair>-val.
        WHEN 'type'.
          ls_match-type = <ls_pair>-val.
        WHEN OTHERS.
          MESSAGE e017(zsy_xtt) WITH <ls_pair>-key INTO sy-msgli.
          zcx_eui_no_check=>raise_sys_error( ).
      ENDCASE.
    ENDLOOP.

    INSERT ls_match INTO TABLE mt_match.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.

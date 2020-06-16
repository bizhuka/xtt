class ZCL_XTT_REPLACE_BLOCK definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_field,
        name TYPE string,        " Name in template
        typ  TYPE string,        " Type of data
        dref TYPE REF TO data,   " Value for replacement
        oref TYPE REF TO object, " Value for replacement
      END OF ts_field .
  types:
    tt_field TYPE SORTED TABLE OF ts_field WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_tree_attr,
        name TYPE string,
        attr TYPE REF TO data, " OR REF TO
      END OF ts_tree_attr .
  types:
    tt_tree_attr TYPE HASHED TABLE OF ts_tree_attr WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_tree,
        level      TYPE i,             " From 0
        sub_nodes  TYPE tt_tree_attr,
        data       TYPE REF TO DATA,
      END OF ts_tree .
  types:
    tt_tree TYPE STANDARD TABLE OF REF TO ts_tree WITH DEFAULT KEY .
  types:
    BEGIN OF ts_func,
      level  TYPE i,
      name   TYPE string,
      field  TYPE string,
      " fields TYPE SORTED TABLE OF string WITH UNIQUE KEY TABLE_LINE,
    END OF ts_func .
  types:
    tt_func TYPE SORTED TABLE OF ts_func WITH UNIQUE KEY TABLE_LINE .
  types:
    BEGIN OF ts_tree_group,
      level    TYPE i,
      top      TYPE abap_bool,
      if_where TYPE string,
      if_show  TYPE abap_bool, " Show or hide
      if_form  TYPE string,    " Name of preform
      " funcs    TYPE SORTED TABLE OF ts_func WITH UNIQUE KEY name,
    end OF ts_tree_group .
  types:
    BEGIN OF ts_extra_tab_opt,
      name        TYPE string, " Name of table 'R-T'
      direction   TYPE string, " ;direction=column ?
      group       TYPE string, " ;group=BUKRS;WERKS or ;group=FILED-FILED_PAR
    END OF ts_extra_tab_opt .
  types:
    TT_EXTRA_TAB_OPT TYPE SORTED TABLE OF ts_extra_tab_opt WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_row_offset.
      INCLUDE TYPE ts_tree_group.
  types:
      first    TYPE i,
      last     TYPE i,
    END OF ts_row_offset .
  types:
    tt_row_offset TYPE SORTED TABLE OF ts_row_offset WITH UNIQUE KEY level top if_where .
  types:
    tt_std_ref_data TYPE STANDARD TABLE OF REF TO DATA .

  constants MC_CHAR_BLOCK_BEGIN type CHAR1 value '{' ##NO_TEXT.
  constants MC_CHAR_BLOCK_END type CHAR1 value '}' ##NO_TEXT.
  constants MC_CHAR_NAME_DELIMITER type CHAR1 value '-' ##NO_TEXT.
  constants MC_CHAR_OPTION_DELIMITER type CHAR1 value ';' ##NO_TEXT.
  constants MC_TYPE_STRUCT type STRING value 'struct' ##NO_TEXT.
  constants MC_TYPE_OBJECT type STRING value 'object' ##NO_TEXT.
  constants MC_TYPE_TABLE type STRING value 'table' ##NO_TEXT.
  constants MC_TYPE_TREE type STRING value 'tree' ##NO_TEXT.
  constants MC_TYPE_INTEGER type STRING value 'integer' ##NO_TEXT.
  constants MC_TYPE_DOUBLE type STRING value 'double' ##NO_TEXT.
  constants MC_TYPE_DATE type STRING value 'date' ##NO_TEXT.
  constants MC_TYPE_TIME type STRING value 'time' ##NO_TEXT.
  constants MC_TYPE_BOOLEAN type STRING value 'boolean' ##NO_TEXT.
  constants MC_TYPE_DATETIME type STRING value 'datetime' ##NO_TEXT.
  constants MC_TYPE_STRING type STRING value 'string' ##NO_TEXT.
  constants MC_TYPE_MASK type STRING value 'mask' ##NO_TEXT.
  constants MC_TYPE_AS_IS type STRING value 'as_is' ##NO_TEXT.
  data MT_FIELDS type TT_FIELD .

  events MATCH_FOUND
    exporting
      value(IV_CONTENT) type ref to STRING
      value(IS_FIELD) type ref to TS_FIELD
      value(IV_POS_BEG) type I
      value(IV_POS_END) type I .
  class-events PREPARE_TREE
    exporting
      value(IR_TREE) type ref to TS_TREE
      value(IR_DATA) type ref to DATA
      value(IR_SUB_DATA) type ref to DATA optional
      value(IT_SUB_DATA_REF) type TT_STD_REF_DATA optional .
  class-events ON_TREE_CHANGE_LEVEL
    exporting
      value(IR_TREE) type ref to ZCL_XTT_REPLACE_BLOCK=>TS_TREE
      value(IV_BLOCK_NAME) type CSEQUENCE
      value(IV_TOP) type ABAP_BOOL
      value(IV_LEVEL_INDEX) type ref to I .

  methods CONSTRUCTOR
    importing
      !IS_BLOCK type ANY optional
      !IV_BLOCK_NAME type STRING optional
      !IS_FIELD type ref to ZCL_XTT_REPLACE_BLOCK=>TS_FIELD optional .
  methods EXTRA_CREATE_TREE
    importing
      !IT_EXTRA_TAB_OPT type TT_EXTRA_TAB_OPT .
  class-methods EXTRA_ADD_TAB_OPT
    importing
      !IV_TEXT type STRING
    changing
      !CT_EXTRA_TAB_OPT type TT_EXTRA_TAB_OPT .
  methods FIND_MATCH
    importing
      !IV_SKIP_TAGS type ABAP_BOOL optional
    changing
      !CV_CONTENT type STRING .
  class-methods GET_AS_STRING
    importing
      !IS_FIELD type ref to TS_FIELD
    returning
      value(RV_RESULT) type STRING .
  class-methods TREE_CREATE
    importing
      !IT_TABLE type ref to DATA
      !IV_FIELDS type CSEQUENCE
    returning
      value(RR_ROOT) type ref to ZCL_XTT_REPLACE_BLOCK=>TS_TREE .
  class-methods TREE_CREATE_RELAT
    importing
      !IT_TABLE type ref to DATA
      !IV_NODE_KEY type CSEQUENCE
      !IV_RELAT_KEY type CSEQUENCE
    returning
      value(RR_ROOT) type ref to DATA
    exceptions
      EX_LOOP_REF
      EX_KEY_DUPL .
  class-methods TREE_RAISE_PREPARE
    importing
      !IR_TREE type ref to TS_TREE
      !IV_LEVEL type I .
  class-methods TREE_FIND_MATCH
    importing
      !IR_TREE type ref to ZCL_XTT_REPLACE_BLOCK=>TS_TREE
      !IV_BLOCK_NAME type CSEQUENCE
      !IV_TOP type ABAP_BOOL
      !IV_CHECK_PROG type STRING
      !IT_ROW_MATCH type SORTED TABLE
    returning
      value(RR_FOUND_MATCH) type ref to DATA .
  class-methods TREE_DETECT_OPTIONS
    importing
      !IV_TEXT type CSEQUENCE
      !IV_POS type I
    changing
      !CS_ROW_OFFSET type TS_ROW_OFFSET
      !CT_ROW_OFFSET type TT_ROW_OFFSET .
  class-methods TREE_INITIALIZE
    importing
      !IR_TREE type ref to TS_TREE
    exporting
      !EV_PROGRAM type STRING
    changing
      !CT_ROW_MATCH type SORTED TABLE .
  PROTECTED SECTION.
private section.

  data MV_BLOCK_BEGIN type STRING .
  class-data MT_FUNC type TT_FUNC .

  methods ADD_2_FIELDS
    importing
      !IV_NAME type CSEQUENCE
      !IV_TYPE type CSEQUENCE optional
      !IR_VALUE type ref to DATA .
  class-methods CATCH_PREPARE_TREE
    for event PREPARE_TREE of ZCL_XTT_REPLACE_BLOCK
    importing
      !IR_TREE
      !IR_DATA
      !IR_SUB_DATA .
ENDCLASS.



CLASS ZCL_XTT_REPLACE_BLOCK IMPLEMENTATION.


METHOD add_2_fields.
  DATA:
    ls_field   TYPE ts_field,
    l_typekind TYPE abap_typekind,
    l_mask     TYPE string.
  FIELD-SYMBOLS:
    <lv_value> TYPE any,
    <fs_data>  TYPE any.

  " Data could be changed
  ASSIGN ir_value->* TO <lv_value>.

  " 1 Detect C_TYPE_* (References)
  ls_field-name = iv_name.
  DESCRIBE FIELD:
   <lv_value> TYPE      l_typekind,
   <lv_value> EDIT MASK l_mask.

  CASE l_typekind.
      " Special case for objects
    WHEN cl_abap_typedescr=>typekind_intf OR cl_abap_typedescr=>typekind_class OR cl_abap_typedescr=>typekind_oref.
      ls_field-oref = <lv_value>.
      ls_field-typ = zcl_xtt_replace_block=>mc_type_object.
      INSERT ls_field INTO TABLE mt_fields.
      RETURN. " <-- That's all

      " Try to detect data
    WHEN cl_abap_typedescr=>typekind_dref.
      ls_field-dref = <lv_value>.
      ASSIGN ls_field-dref->* TO <fs_data>.  " <-- Usally use dref
      DESCRIBE FIELD <fs_data> TYPE l_typekind.

    WHEN OTHERS.
      ls_field-dref = ir_value.
  ENDCASE.

  IF iv_type IS NOT INITIAL.
    ls_field-typ = iv_type.
  ELSE.
    " 2 Detect C_TYPE_*
    CASE l_typekind.
        " Structures
      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
        ls_field-typ = zcl_xtt_replace_block=>mc_type_struct.

        " Tables
      WHEN cl_abap_typedescr=>typekind_table.
        ls_field-typ = zcl_xtt_replace_block=>mc_type_table.

        " Integer, byte, short
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
        ls_field-typ = zcl_xtt_replace_block=>mc_type_integer.

        " Use mask and don't delete dots in ZCL_XTT_REPLACE_BLOCK=>get_as_string
      WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
        IF l_mask IS INITIAL.
          ls_field-typ = zcl_xtt_replace_block=>mc_type_integer.
        ELSE. " For safety
          ls_field-typ = zcl_xtt_replace_block=>mc_type_mask.
        ENDIF.

        " Double
      WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
           '/' OR 'a' OR 'e'. " cl_abap_typedescr=>typekind_decfloat  OR cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
        ls_field-typ = zcl_xtt_replace_block=>mc_type_double.

        " Date
      WHEN cl_abap_typedescr=>typekind_date.
        ls_field-typ = zcl_xtt_replace_block=>mc_type_date.

        " Time
      WHEN cl_abap_typedescr=>typekind_time.
        ls_field-typ = zcl_xtt_replace_block=>mc_type_time.

        " No trunsformation for STRING
      WHEN cl_abap_typedescr=>typekind_char OR cl_abap_typedescr=>typekind_clike OR
           cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_w OR
           " Binary data in template? Dump ?
           cl_abap_typedescr=>typekind_hex OR cl_abap_typedescr=>typekind_xsequence OR cl_abap_typedescr=>typekind_xstring.
        ls_field-typ = zcl_xtt_replace_block=>mc_type_string.

*TYPEKIND_IREF, TYPEKIND_BREF
*TYPEKIND_DATA, TYPEKIND_SIMPLE, TYPEKIND_ANY
      WHEN OTHERS.
        MESSAGE x002(zsy_xtt) WITH l_typekind.
    ENDCASE.
  ENDIF.

  " And add
  INSERT ls_field INTO TABLE mt_fields.
ENDMETHOD.


METHOD catch_prepare_tree.
  DATA lv_message TYPE string.
  FIELD-SYMBOLS:
    <ls_func>      LIKE LINE OF mt_func,
    <ls_data>      TYPE any,
    <lt_sub_data>  TYPE ANY TABLE, " STANDARD ?
    <ls_sub_data>  TYPE any,
    <lv_field>     TYPE any,
    <lv_sub_field> TYPE any.

  CHECK mt_func IS NOT INITIAL AND ir_sub_data IS NOT INITIAL.

  " Cast to specefic data
  ASSIGN:
   ir_data->*        TO <ls_data>,
   ir_sub_data->*    TO <lt_sub_data>.

  LOOP AT mt_func ASSIGNING <ls_func> WHERE level = ir_tree->level.
    ASSIGN COMPONENT <ls_func>-field OF STRUCTURE <ls_data> TO <lv_field>.
    CHECK sy-subrc = 0.

    CASE <ls_func>-name.
      WHEN 'COUNT'.
        <lv_field> = lines( <lt_sub_data> ).

      WHEN 'FIRST'. " 'LAST'.
        LOOP AT <lt_sub_data> ASSIGNING <ls_sub_data>.
          ASSIGN COMPONENT <ls_func>-field OF STRUCTURE <ls_sub_data> TO <lv_sub_field>.
          <lv_field> = <lv_sub_field>.
          EXIT.
        ENDLOOP.

      WHEN 'SUM' OR 'AVG'.
        " Calculate sum
        <lv_field> = 0.
        LOOP AT <lt_sub_data> ASSIGNING <ls_sub_data>.
          ASSIGN COMPONENT <ls_func>-field OF STRUCTURE <ls_sub_data> TO <lv_sub_field>.
          <lv_field> = <lv_field> + <lv_sub_field>.
        ENDLOOP.

        IF <ls_func>-name = 'AVG'.
          <lv_field> = <lv_field> / lines( <lt_sub_data> ).
        ENDIF.

      WHEN OTHERS.
        CONCATENATE `Unknown function ` <ls_func>-name INTO lv_message. "#EC NOTEXT
        zcx_xtt_exception=>raise_dump( iv_message = lv_message ).
    ENDCASE.
  ENDLOOP.
ENDMETHOD.


METHOD constructor.
  DATA:
    lv_block_name   LIKE iv_block_name,
    lo_desc         TYPE REF TO cl_abap_typedescr,
    lo_sdesc        TYPE REF TO cl_abap_structdescr,
    lo_odesc        TYPE REF TO cl_abap_objectdescr,
    lr_data         TYPE REF TO data,
    lr_ref          TYPE REF TO data,
    lo_name         TYPE REF TO cl_abap_typedescr,
    lo_block        TYPE REF TO object,
    l_field_name    TYPE string,
    l_absolute_name TYPE string,
    lv_name         TYPE string.
  FIELD-SYMBOLS:
    <fs_block> TYPE any,
    <fs_any>   TYPE any,
    <ls_comp>  TYPE abap_compdescr,
    <ls_attr>  TYPE abap_attrdescr.

  " For nested structures
  lv_block_name = iv_block_name.
  IF is_field IS NOT SUPPLIED.
    " Work by field symbol
    ASSIGN is_block TO <fs_block>.
  ELSE.
    " ROOT-FIELD1...
    lv_block_name = is_field->name.

    " Objects
    IF is_field->oref IS NOT INITIAL.
      ASSIGN is_field->oref TO <fs_block>.
    ELSE. " Other types
      ASSIGN is_field->dref->* TO <fs_block>.
    ENDIF.
  ENDIF.

  " What will search in template. At first '{ROOT-'
  CONCATENATE zcl_xtt_replace_block=>mc_char_block_begin lv_block_name INTO mv_block_begin.

  " 1 Is data (The most common)
  TRY.
      lo_desc = cl_abap_typedescr=>describe_by_data( <fs_block> ).
    CATCH cx_dynamic_check.
      MESSAGE x003(zsy_xtt) WITH lv_block_name.
  ENDTRY.

  " Description of description :)
  lo_name = cl_abap_typedescr=>describe_by_object_ref( lo_desc ).
  WHILE lo_name->absolute_name = '\CLASS=CL_ABAP_REFDESCR'.
    " Reference to nowhere
    CLEAR lo_desc.
    IF <fs_block> IS INITIAL.
      EXIT.
    ENDIF.

    " 2 Is data ref ?
    TRY.
        lo_desc = cl_abap_typedescr=>describe_by_data_ref( <fs_block> ).
        lr_ref ?= <fs_block>.
        ASSIGN lr_ref->* TO <fs_block>.
      CATCH cx_dynamic_check.
        CLEAR lo_desc.
    ENDTRY.

    " 3 Is object ref?
    IF lo_desc IS INITIAL.
      TRY.
          lo_desc = cl_abap_typedescr=>describe_by_object_ref( <fs_block> ).
          lo_block ?= <fs_block>.
        CATCH cx_dynamic_check.
          EXIT.
      ENDTRY.
    ENDIF.

    " Goes deeper ->*  ->*
    lo_name = cl_abap_typedescr=>describe_by_object_ref( lo_desc ).
  ENDWHILE.

  " Skip ?
  IF lo_desc IS INITIAL.
    lv_name = lv_block_name.
    IF lv_name IS INITIAL AND is_field IS NOT INITIAL.
      lv_name = is_field->name.
    ENDIF.
    MESSAGE x004(zsy_xtt) WITH lv_name.
  ENDIF.

*lo_desc is instance of:
*CL_ABAP_TYPEDESCR
*-CL_ABAP_DATADESCR
*--CL_ABAP_COMPLEXDESCR
*---CL_ABAP_STRUCTDESCR  +
*---CL_ABAP_TABLEDESCR   + WHEN OTHERS
*
*--CL_ABAP_ELEMDESCR     + WHEN OTHERS
*--CL_ABAP_REFDESCR      - Is not possible (DESCRIBE_BY_DATA_REF DESCRIBE_BY_OBJECT_REF)
*
*-CL_ABAP_OBJECTDESCR    +
*--CL_ABAP_CLASSDESCR    +
*--CL_ABAP_INTFDESCR     +

  " Detect by name what to do
  CASE lo_name->absolute_name.
      " A structure is the most popular case
    WHEN '\CLASS=CL_ABAP_STRUCTDESCR'.
      lo_sdesc ?= lo_desc.
      l_absolute_name = lo_sdesc->absolute_name.

**********************************************************************
**********************************************************************

      " TREE (Dynamic objects)
      IF l_absolute_name = '\CLASS=ZCL_XTT_REPLACE_BLOCK\TYPE=TS_TREE'.
        is_field->typ = zcl_xtt_replace_block=>mc_type_tree.

        GET REFERENCE OF <fs_block> INTO lr_data.
        me->add_2_fields(
           iv_name     = lv_block_name
           iv_type     = is_field->typ
           ir_value    = lr_data ).
        RETURN.
      ENDIF.
**********************************************************************
**********************************************************************
      " Add every field
      LOOP AT lo_sdesc->components ASSIGNING <ls_comp>.
        " Name and data
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <fs_block> TO <fs_any>.
        CONCATENATE lv_block_name zcl_xtt_replace_block=>mc_char_name_delimiter <ls_comp>-name INTO l_field_name.

        " Insert field to me->fields
        GET REFERENCE OF <fs_any> INTO lr_data.
        me->add_2_fields(
          iv_name     = l_field_name
          ir_value    = lr_data ).
      ENDLOOP.

      " Object processed as a structure ↑↑↑
    WHEN `\CLASS=CL_ABAP_CLASSDESCR` OR `\CLASS=CL_ABAP_INTFDESCR` OR `\CLASS=CL_ABAP_OBJECTDESCR`.
      lo_odesc ?= lo_desc.
      lo_block ?= <fs_block>.
      " Add every field
      LOOP AT lo_odesc->attributes ASSIGNING <ls_attr> WHERE visibility = cl_abap_objectdescr=>public.
        " Name and data
        ASSIGN lo_block->(<ls_attr>-name) TO <fs_any>.
        CONCATENATE lv_block_name zcl_xtt_replace_block=>mc_char_name_delimiter <ls_attr>-name INTO l_field_name.

        " Insert field to me->fields
        GET REFERENCE OF <fs_any> INTO lr_data.
        me->add_2_fields(
           iv_name     = l_field_name
           ir_value    = lr_data ).
      ENDLOOP.

    WHEN OTHERS.
      " CL_ABAP_TABLEDESCR, CL_ABAP_ELEMDESCR
      GET REFERENCE OF <fs_block> INTO lr_data.
      me->add_2_fields(
         iv_name     = lv_block_name
         ir_value    = lr_data ).
  ENDCASE.
ENDMETHOD.


METHOD extra_add_tab_opt.
  DATA ls_extra_tab_opt LIKE LINE OF ct_extra_tab_opt.
  DATA lv_ind           TYPE i.
  DATA lv_field         TYPE string.
  DATA lv_key           TYPE string.
  DATA lv_value         TYPE string.
  DATA lt_param         TYPE stringtab.

  " Get without {}
  lv_ind   = strlen( iv_text ).
  lv_ind   = lv_ind - 2.
  lv_field = iv_text+1(lv_ind).

  " First part is a name
  SPLIT lv_field AT `;` INTO TABLE lt_param.
  READ TABLE lt_param INTO ls_extra_tab_opt-name INDEX 1.

  " Find a match by name
  " TRANSLATE ls_extra_tab_opt-name TO UPPER CASE. Better make case sensetive
  READ TABLE ct_extra_tab_opt TRANSPORTING NO FIELDS
   WITH TABLE KEY name = ls_extra_tab_opt-name.
  IF sy-subrc = 0.
    CONCATENATE 'Filed' ls_extra_tab_opt-name 'declared several times'  INTO lv_field SEPARATED BY space.
    zcx_xtt_exception=>raise_dump( iv_message = lv_field ).
  ENDIF.

  " Set additional options
  ls_extra_tab_opt-group = abap_undefined.
  LOOP AT lt_param INTO lv_field FROM 2.  "<-- First item is field name
    SPLIT lv_field AT '=' INTO lv_key lv_value.
    CASE lv_key.
      WHEN 'direction'.
        ls_extra_tab_opt-direction = lv_value.
      WHEN 'group'.
        ls_extra_tab_opt-group     = lv_value.
      WHEN OTHERS.
        CONCATENATE 'Key: ' lv_key 'is unknown'  INTO lv_field SEPARATED BY space.
        zcx_xtt_exception=>raise_dump( iv_message = lv_field ).
    ENDCASE.
  ENDLOOP.

  " And add
  INSERT ls_extra_tab_opt INTO TABLE ct_extra_tab_opt.
ENDMETHOD.


METHOD extra_create_tree.
  " Check extra paramaters
  CHECK it_extra_tab_opt IS NOT INITIAL.

  DATA lv_group1 TYPE string.
  DATA lv_group2 TYPE string.
  FIELD-SYMBOLS <ls_extra_tab_opt> LIKE LINE OF it_extra_tab_opt.
  FIELD-SYMBOLS <ls_field>         LIKE LINE OF mt_fields.

  " Find matching
  LOOP AT it_extra_tab_opt ASSIGNING <ls_extra_tab_opt> "#EC CI_SORTSEQ
       WHERE group <> abap_undefined. " group could be empty
    " 1 field
    READ TABLE mt_fields ASSIGNING <ls_field>
     WITH TABLE KEY name = <ls_extra_tab_opt>-name.

    " Just skip ?
    CHECK sy-subrc = 0.

    " Table ?
    IF <ls_field>-typ <> mc_type_table.
      CONCATENATE 'Field' <ls_field>-name 'is not table!' INTO lv_group1 SEPARATED BY space.
      zcx_xtt_exception=>raise_dump( iv_message = lv_group1 ).
    ENDIF.

    CLEAR lv_group2.
    SPLIT <ls_extra_tab_opt>-group AT '-' INTO lv_group1 lv_group2.

    " Have both parts
    IF lv_group2 IS NOT INITIAL.
      <ls_field>-dref = tree_create_relat(
        it_table      = <ls_field>-dref
        iv_node_key   = lv_group1
        iv_relat_key  = lv_group2 ).
      CONTINUE.
    ENDIF.

    " Now is tree
    <ls_field>-typ = zcl_xtt_replace_block=>mc_type_tree.

    " Fields separeted by ;
    <ls_field>-dref = tree_create(
     it_table      = <ls_field>-dref
     iv_fields     = lv_group1 ).
  ENDLOOP.
ENDMETHOD.


METHOD find_match.
  DATA:
    lr_content    TYPE REF TO string,
    lt_find_res   TYPE match_result_tab,
    l_find_pos    TYPE i,
    l_off         TYPE i,
    l_beg         TYPE i,
    l_cnt         TYPE i,
    l_whole_field TYPE string,
    lt_params     TYPE stringtab,
    ls_param      TYPE string,
    l_key         TYPE string,
    l_val         TYPE string,
    l_fld_name    TYPE string,
    ls_field      TYPE REF TO ts_field.
  FIELD-SYMBOLS:
   <ls_find_res> LIKE LINE OF lt_find_res.

  " To change text in event handler
  GET REFERENCE OF cv_content INTO lr_content.

  " Look for '{ROOT-'
  FIND ALL OCCURRENCES OF mv_block_begin IN cv_content RESULTS lt_find_res.

  " Search from the last position. That's why 1 run only
  l_find_pos = lines( lt_find_res ).
  WHILE l_find_pos > 0.
    " Read the next match
    READ TABLE lt_find_res ASSIGNING <ls_find_res> INDEX l_find_pos.
    l_find_pos = l_find_pos - 1.

    " End of tech name
    FIND FIRST OCCURRENCE OF zcl_xtt_replace_block=>mc_char_block_end IN SECTION OFFSET <ls_find_res>-offset OF cv_content MATCH OFFSET l_off.
    CHECK sy-subrc = 0.

    " Read tech name
    l_beg = <ls_find_res>-offset + 1.
    l_cnt = l_off - <ls_find_res>-offset - 1.
    l_whole_field = cv_content+l_beg(l_cnt).

    " Delete all rubbish between
    IF iv_skip_tags = abap_true.
      REPLACE ALL OCCURRENCES OF REGEX '<[^\>]+>' IN l_whole_field WITH ''.
    ENDIF.

    " First part is a name
    SPLIT l_whole_field AT zcl_xtt_replace_block=>mc_char_option_delimiter INTO TABLE lt_params.
    READ TABLE lt_params INTO l_fld_name INDEX 1.
    CHECK sy-subrc = 0.

    " Find a match by name
    " TRANSLATE l_fld_name TO UPPER CASE. Better make case sensetive
    READ TABLE mt_fields REFERENCE INTO ls_field
     WITH TABLE KEY name = l_fld_name.

    " Cannot find
    IF sy-subrc <> 0 OR
      " Or complex type
       ls_field->typ = zcl_xtt_replace_block=>mc_type_struct OR
       ls_field->typ = zcl_xtt_replace_block=>mc_type_object OR
       " ls_field->typ = zcl_xtt_replace_block=>mc_type_tree   OR " TODO check!
       ls_field->typ = zcl_xtt_replace_block=>mc_type_table.

      " Markers for block's range (tables only). Can specify block's end(start) explicitly
      CONCATENATE zcl_xtt_replace_block=>mc_char_block_begin l_fld_name INTO l_fld_name.
      IF mv_block_begin = l_fld_name.
        l_off = l_off + 1.
        CONCATENATE cv_content(<ls_find_res>-offset) cv_content+l_off INTO cv_content RESPECTING BLANKS.
      ENDIF.
      CONTINUE.
    ENDIF.

    " Set additional options
    LOOP AT lt_params INTO ls_param FROM 2.  "<-- First item is field name
      SPLIT ls_param AT '=' INTO l_key l_val.
      CASE l_key.
          " Change type
        WHEN 'type'.
          ls_field->typ = l_val.
      ENDCASE.
    ENDLOOP.

    " Replace value in event handler
    RAISE EVENT match_found
     EXPORTING
      iv_content = lr_content
      is_field   = ls_field
      iv_pos_beg = <ls_find_res>-offset
      iv_pos_end = l_off.
  ENDWHILE.
ENDMETHOD.


METHOD get_as_string.
  DATA:
    l_text TYPE text255, " For long values use 'string' OR 'as_is', but do not use 'mask'
    l_len  TYPE i.
  FIELD-SYMBOLS:
    <l_string> TYPE csequence,
    <l_any>    TYPE any, "numeric, + n
    <l_date>   TYPE d,
    <l_time>   TYPE t.

  " Just skip
  CHECK is_field->dref IS NOT INITIAL OR is_field->oref IS NOT INITIAL.

  " Convert field to a string
  CASE is_field->typ.
      " String
    WHEN zcl_xtt_replace_block=>mc_type_string.
      ASSIGN is_field->dref->* TO <l_string>.

      " If too long
      l_len = strlen( <l_string> ).
      IF l_len > 32767.
        rv_result = <l_string>(32767).
      ELSE.
        rv_result = <l_string>.
      ENDIF.

      " Delete symbols 0-31
      REPLACE ALL OCCURRENCES OF REGEX '[^[:print:]]' IN rv_result WITH ''.

      " Replace special chars
      rv_result = cl_http_utility=>escape_html( rv_result ).

      " Numbers
    WHEN zcl_xtt_replace_block=>mc_type_integer OR zcl_xtt_replace_block=>mc_type_double.
      ASSIGN is_field->dref->* TO <l_any>.

      " Old method faster cl_abap_format=>o_simple
      WRITE <l_any> TO l_text EXPONENT 0 NO-SIGN NO-GROUPING LEFT-JUSTIFIED.

      IF <l_any> >= 0.
        rv_result = l_text.
      ELSE.
        CONCATENATE `-` l_text INTO rv_result.
      ENDIF.
      " Even with replace is faster than using mask like RRV_.__
      REPLACE FIRST OCCURRENCE OF ',' IN rv_result WITH '.'.

      " As is using an edit mask
    WHEN zcl_xtt_replace_block=>mc_type_mask.
      ASSIGN is_field->dref->* TO <l_any>.
      WRITE <l_any> TO l_text LEFT-JUSTIFIED.
      rv_result = l_text.

      " Do not transform at all
    WHEN zcl_xtt_replace_block=>mc_type_as_is.
      ASSIGN is_field->dref->* TO <l_any>.
      rv_result = <l_any>.

      " Boolean
    WHEN zcl_xtt_replace_block=>mc_type_boolean.
      ASSIGN is_field->dref->* TO <l_string>.
      IF <l_string> IS NOT INITIAL. " = abap_true.
        rv_result = '1'.
      ELSE.
        rv_result = '0'.
      ENDIF.

      " Whole as a string like  d + t
    WHEN zcl_xtt_replace_block=>mc_type_datetime.
      ASSIGN is_field->dref->* TO <l_string>.

      " Both parts
      ASSIGN <l_string>(8)     TO <l_date> CASTING.
      ASSIGN <l_string>+8(6)   TO <l_time> CASTING.

      " Date
    WHEN zcl_xtt_replace_block=>mc_type_date.
      ASSIGN is_field->dref->* TO <l_date>.

      " Time
    WHEN zcl_xtt_replace_block=>mc_type_time.
      ASSIGN is_field->dref->* TO <l_time>.

    WHEN OTHERS.
      MESSAGE s002(zsy_xtt) WITH is_field->typ INTO sy-msgli.
      zcx_xtt_exception=>raise_dump( iv_message = sy-msgli ).
  ENDCASE.

  " Never will happen in MS Excel template (MS Word and pdf only)
  " Excel uses its own formats for date and time
  " Format depends on country
  IF <l_date> IS ASSIGNED AND <l_date> IS NOT INITIAL.
    WRITE <l_date> TO l_text.
    rv_result = l_text.
  ENDIF.

  IF <l_time> IS ASSIGNED.
    WRITE <l_time> TO l_text.

    " Datetime ?
    IF <l_date> IS ASSIGNED.  " Both parts
      CONCATENATE rv_result ` ` l_text INTO rv_result.
    ELSE.                     " Just time
      rv_result = l_text.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD tree_create.
  DATA:
    lo_tdesc    TYPE REF TO cl_abap_tabledescr,
    lo_sdesc    TYPE REF TO cl_abap_structdescr,
    ls_curtree  TYPE REF TO zcl_xtt_replace_block=>ts_tree,
    ls_subtree  TYPE REF TO zcl_xtt_replace_block=>ts_tree,
    lt_fields   TYPE stringtab,
    ls_field    TYPE string,
    l_field_val TYPE string,
    ls_attr     TYPE ts_tree_attr,
    ls_attr_ref TYPE REF TO ts_tree_attr.
*    l_level     TYPE i.
  FIELD-SYMBOLS:
    <lt_table> TYPE ANY TABLE,
    <ls_item>  TYPE any,
    <fs_any>   TYPE any.

  DEFINE create_tree.
    CREATE DATA &1.
*    &1->level = &2.
    CREATE DATA &1->data TYPE HANDLE lo_sdesc.
  END-OF-DEFINITION.

  ASSIGN it_table->* TO <lt_table>.
  lo_tdesc ?= cl_abap_tabledescr=>describe_by_data( <lt_table> ).
  lo_sdesc ?= lo_tdesc->get_table_line_type( ).

  " Level 0
  create_tree rr_root. " 0.

  " More convenient than pass table
  IF iv_fields CS ','.
    SPLIT iv_fields AT ',' INTO TABLE lt_fields.
  ELSE.
    SPLIT iv_fields AT ';' INTO TABLE lt_fields.
  ENDIF.

  LOOP AT <lt_table> ASSIGNING <ls_item>.
    ls_curtree = rr_root.
*    l_level    = 0.

    LOOP AT lt_fields INTO ls_field.
      " Next level
*      l_level = sy-tabix.

      " Value of the field
      ASSIGN COMPONENT ls_field OF STRUCTURE <ls_item> TO <fs_any>.
      l_field_val = <fs_any>.

      " Find existing
      READ TABLE ls_curtree->sub_nodes REFERENCE INTO ls_attr_ref
       WITH TABLE KEY name = l_field_val.

      " Insert as a new subTREE
      IF sy-subrc <> 0.
        " New TREE
        create_tree ls_subtree. "l_level.

        " Add
        ls_attr-name  = l_field_val.
        ls_attr-attr  = ls_subtree.

        INSERT ls_attr INTO TABLE ls_curtree->sub_nodes REFERENCE INTO ls_attr_ref.
      ENDIF.

      " Make current
      ls_curtree ?= ls_attr_ref->attr.
    ENDLOOP.

    " Last level
*    l_level = l_level + 1.
    create_tree ls_subtree. " l_level.
    GET REFERENCE OF <ls_item> INTO ls_subtree->data.

    " Add
    ls_attr-name  = lines( ls_curtree->sub_nodes ).
    ls_attr-attr  = ls_subtree.
    INSERT ls_attr INTO TABLE ls_curtree->sub_nodes.
  ENDLOOP.

*  " Raise event
*  tree_raise_prepare(
*   ir_tree  = rr_root
*   iv_level = 0 ).
ENDMETHOD.


METHOD tree_create_relat.
  TYPES:
    BEGIN OF ts_relat,
      node_key        TYPE string,
      relat_key       TYPE string,
*      relat_not_empty TYPE abap_bool,
      tree            TYPE REF TO ts_tree,
    END OF ts_relat,
    tt_relat TYPE HASHED TABLE OF ts_relat WITH UNIQUE KEY node_key.

  DATA:
    ls_tree      TYPE REF TO ts_tree,
    lt_tree      TYPE REF TO tt_tree,
    ls_relat     TYPE ts_relat,
    lt_relat     TYPE tt_relat,
    ls_tree_attr TYPE ts_tree_attr.

  FIELD-SYMBOLS:
    <lt_table>     TYPE ANY TABLE,
    <ls_row>       TYPE any,
    <lv_node_key>  TYPE any,
    <lv_relat_key> TYPE any,
    <ls_relat>     TYPE ts_relat,
    <ls_relat_par> TYPE ts_relat,
    <lt_tree>      TYPE tt_tree.

  ASSIGN it_table->* TO <lt_table>.
  " Add to mediator
  LOOP AT <lt_table> ASSIGNING <ls_row>.
    ASSIGN COMPONENT iv_node_key  OF STRUCTURE <ls_row> TO <lv_node_key>.
    ASSIGN COMPONENT iv_relat_key OF STRUCTURE <ls_row> TO <lv_relat_key>.

    IF <lv_node_key> = <lv_relat_key>.
      RAISE ex_loop_ref.
    ENDIF.

    " Is level 0 OR error ?
*    IF <lv_relat_key> IS NOT INITIAL.
*      ls_relat-relat_not_empty = abap_true.
*    ENDIF.

    ls_relat-node_key  = <lv_node_key>.
    ls_relat-relat_key = <lv_relat_key>.
    CREATE DATA ls_relat-tree.
    GET REFERENCE OF <ls_row> INTO ls_relat-tree->data.

    " Insert new item
    INSERT ls_relat INTO TABLE lt_relat.
    IF sy-subrc <> 0.
      RAISE ex_key_dupl.
    ENDIF.

    CLEAR ls_relat.
  ENDLOOP.

  " Where write result
  CREATE DATA lt_tree.
  ASSIGN lt_tree->* TO <lt_tree>.

  LOOP AT lt_relat ASSIGNING <ls_relat>.
    " Try to find
    READ TABLE lt_relat ASSIGNING <ls_relat_par>
     WITH TABLE KEY node_key = <ls_relat>-relat_key.

    " To level
    IF sy-subrc <> 0.
*      IF <ls_relat>-relat_not_empty = abap_true.
*        RAISE ex_ref_to_nowhere.
*      ENDIF.

      "<ls_relat>-tree->level = 0.
      INSERT <ls_relat>-tree INTO TABLE <lt_tree>.
    ELSE.
      " Add as child
      ls_tree_attr-name      = <ls_relat>-node_key. " lines( <lt_tree_attr> ).
      ls_tree_attr-attr      = <ls_relat>-tree.
      "<ls_relat>-tree->level = <ls_relat_par>-tree->level + 1.
      INSERT ls_tree_attr INTO TABLE <ls_relat_par>-tree->sub_nodes.
    ENDIF.
  ENDLOOP.

  rr_root = lt_tree.
*  " Raise events
*  LOOP AT <lt_tree> INTO ls_tree.
*    tree_raise_prepare(
*     ir_tree  = ls_tree
*     iv_level = 0 ).
*  ENDLOOP.
ENDMETHOD.


METHOD tree_detect_options.
  DATA:
    lt_text  TYPE stringtab,
    lv_text  TYPE string,
    lv_value TYPE string,
    ls_func  TYPE ts_func,
    lv_ind   TYPE i.
  FIELD-SYMBOLS:
    <ls_func>    LIKE ls_func,
    <ls_row_off> TYPE ts_row_offset.

  " Read from texts
  lv_text = iv_text.

  " TODO unescape all? Only for <>
  REPLACE ALL OCCURRENCES OF:
   '&lt;'   IN lv_text WITH '<',
   '&gt;'   IN lv_text WITH '>',
   '&amp;'  IN lv_text WITH '&',
   '&quot;' IN lv_text WITH '"',
   '&apos;' IN lv_text WITH `'`.

  SPLIT lv_text AT ';' INTO TABLE lt_text.
  LOOP AT lt_text INTO lv_text.
    CLEAR lv_value.
    SPLIT lv_text AT '=' INTO lv_text lv_value.

    CASE lv_text.
      WHEN 'level'.
        CLEAR cs_row_offset.
        cs_row_offset-level = lv_value.

      WHEN 'top'.
        cs_row_offset-top = lv_value.

      WHEN 'show_if'.
        cs_row_offset-if_where = lv_value.
        cs_row_offset-if_show  = abap_true.

      WHEN 'hide_if'.
        cs_row_offset-if_where = lv_value.
        cs_row_offset-if_show  = abap_false.

      WHEN 'func'.
        ls_func-name = lv_value.
        READ TABLE lt_text INTO ls_func-field INDEX 1.

    ENDCASE.
  ENDLOOP.

  " Exist or new
  READ TABLE ct_row_offset ASSIGNING <ls_row_off>
   FROM cs_row_offset.
  IF sy-subrc <> 0.
    CLEAR:
     cs_row_offset-first,
     cs_row_offset-last. " Clear only here !!!
    INSERT cs_row_offset INTO TABLE ct_row_offset ASSIGNING <ls_row_off>.
  ENDIF.

  <ls_row_off>-last = iv_pos.
  IF <ls_row_off>-first IS INITIAL.
    <ls_row_off>-first = <ls_row_off>-last.
  ENDIF.

***************************
  " Add function
  CHECK ls_func-name IS NOT INITIAL.

  " Current level
  ls_func-level = cs_row_offset-level.

  TRANSLATE:
   ls_func-name  TO UPPER CASE,
   ls_func-field TO UPPER CASE.

  SPLIT ls_func-field AT '-' INTO TABLE lt_text.
  lv_ind = lines( lt_text ).
  READ TABLE lt_text INTO ls_func-field INDEX lv_ind.

  " And add
  INSERT ls_func INTO TABLE mt_func.
ENDMETHOD.


METHOD tree_find_match.
  DATA:
    lv_level_index TYPE REF TO i,
    lv_last_top    TYPE abap_bool,
    lv_last_index  TYPE i,
    ls_tree_group  TYPE ts_tree_group,
    lv_ok          TYPE abap_bool,
    lr_row_match   TYPE REF TO data.
  FIELD-SYMBOLS:
    <ls_data>      TYPE any,
    <ls_row_match> TYPE any.

  " Data to ANY
  ASSIGN ir_tree->data->* TO <ls_data>.

  " What level to use
  CREATE DATA lv_level_index.
  lv_level_index->* = -1.
  lv_last_top       = iv_top.

  IF iv_top <> abap_undefined.
    lv_level_index->* = ir_tree->level.
  ELSE.
    lv_last_index = lines( it_row_match ).
    READ TABLE it_row_match ASSIGNING <ls_row_match> INDEX lv_last_index.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <ls_row_match> TO ls_tree_group.
      lv_level_index->* = ls_tree_group-level.
      lv_last_top       = ls_tree_group-top.
    ENDIF.
  ENDIF.

  " Change level by external condition
  RAISE EVENT on_tree_change_level EXPORTING
    ir_tree        = ir_tree
    iv_block_name  = iv_block_name
    iv_top         = iv_top
    iv_level_index = lv_level_index.

  " Check all conditions
  CLEAR rr_found_match.
  LOOP AT it_row_match ASSIGNING <ls_row_match>.
    " Slow match by level and top
    MOVE-CORRESPONDING <ls_row_match> TO ls_tree_group.
    CHECK ls_tree_group-level = lv_level_index->* AND ls_tree_group-top = lv_last_top.

    " As refernce
    GET REFERENCE OF <ls_row_match> INTO lr_row_match.

    " Just get default row group
    IF ls_tree_group-if_form IS INITIAL.
      rr_found_match = lr_row_match.
    ENDIF.

    CHECK ls_tree_group-if_form IS NOT INITIAL AND
          iv_check_prog IS NOT INITIAL.

    CLEAR lv_ok.
    PERFORM (ls_tree_group-if_form) IN PROGRAM (iv_check_prog) IF FOUND
      USING
            <ls_data>
      CHANGING
            lv_ok.

    IF lv_ok = abap_true.
      " Hide
      IF ls_tree_group-if_show = abap_true.
        rr_found_match = lr_row_match.
      ELSE.
        CLEAR rr_found_match.
      ENDIF.

      EXIT.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD tree_initialize.
  DATA:
    ls_tree_group TYPE ts_tree_group,
    lv_ok         TYPE abap_bool,
    lo_struc      TYPE REF TO cl_abap_structdescr, " TODO class attributes
    ls_comp       TYPE REF TO abap_compdescr,
    lv_len        TYPE string,
    lv_dec        TYPE string,
    lv_type       TYPE string,
    lt_code       TYPE stringtab,
    lv_line       TYPE string,
    lv_message    TYPE string,
    lv_pos        TYPE i.                                   "#EC NEEDED
  FIELD-SYMBOLS:
    <ls_row_match> TYPE any,
    <lv_any>       TYPE any.

  " The same key and base fields
  LOOP AT ct_row_match ASSIGNING <ls_row_match>.
    MOVE-CORRESPONDING <ls_row_match> TO ls_tree_group.

    " Yes create new program on fly
    IF ls_tree_group-if_where IS NOT INITIAL.
      lv_ok = abap_true.
    ENDIF.
  ENDLOOP.

  " Raise event
  SET HANDLER catch_prepare_tree.
  tree_raise_prepare(
   ir_tree  = ir_tree
   iv_level = 0 ).
  CLEAR mt_func.

  " No need in program
  CHECK lv_ok = abap_true.

  ASSIGN ir_tree->data->* TO <lv_any>.
  lo_struc ?= cl_abap_typedescr=>describe_by_data( <lv_any> ).

  " Structure declaration
  APPEND `REPORT DYNAMIC_IF.` TO lt_code.
  APPEND `TYPE-POOLS ABAP.` TO lt_code. " Required for ABAP 7.01
  APPEND `TYPES: BEGIN OF TS_ROW, ` TO lt_code.

  LOOP AT lo_struc->components REFERENCE INTO ls_comp.
    " Convert to strings
    lv_len = ls_comp->length.
    lv_dec = ls_comp->decimals.

    CASE ls_comp->type_kind.
        " Convert to string
      WHEN cl_abap_typedescr=>typekind_char OR cl_abap_typedescr=>typekind_clike OR
           cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_string.
        lv_type = 'STRING'.

        " Integer, byte, short
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
        lv_type = 'INT4'.

        " Use mask and don't delete dots in ZCL_XTT_REPLACE_BLOCK=>get_as_string
      WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
        CONCATENATE ` n LENGTH ` lv_len INTO lv_type. "#EC NOTEXT   thanks to Zhukov, Evgenii  No need ` DECIMALS ` lv_dec

        " Double
      WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
           '/' OR 'a' OR 'e'. " cl_abap_typedescr=>typekind_decfloat  OR cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
        lv_type = ` p LENGTH 16 DECIMALS 5`. "#EC NOTEXT No need to be spcific

        " Date
      WHEN cl_abap_typedescr=>typekind_date.
        lv_type = 'SYDATUM'.

        " Time
      WHEN cl_abap_typedescr=>typekind_time.
        lv_type = 'SYUZEIT'.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    CONCATENATE ls_comp->name ` TYPE ` lv_type `,` INTO lv_type. "#EC NOTEXT
    APPEND lv_type TO lt_code.
  ENDLOOP.

  APPEND 'END OF TS_ROW.' TO lt_code.                       "#EC NOTEXT

  " Global structure
  APPEND '' TO lt_code.
  APPEND 'DATA: ROW TYPE TS_ROW.' TO lt_code.               "#EC NOTEXT
  APPEND '' TO lt_code.

  " Generate FORMs
  LOOP AT ct_row_match ASSIGNING <ls_row_match>.
    MOVE-CORRESPONDING <ls_row_match> TO ls_tree_group.
    CHECK ls_tree_group-if_where IS NOT INITIAL.

    " Name of FORM
    ls_tree_group-if_form = sy-tabix.
    CONDENSE ls_tree_group-if_form.
    CONCATENATE `F_` ls_tree_group-if_form INTO ls_tree_group-if_form.

    " Change form name
    ASSIGN COMPONENT 'IF_FORM' OF STRUCTURE <ls_row_match> TO <lv_any>.
    <lv_any> = ls_tree_group-if_form.

    " FORM declaration
    CONCATENATE `FORM ` ls_tree_group-if_form ` USING is_row TYPE ANY CHANGING cv_ok TYPE abap_bool.` INTO lv_line. "#EC NOTEXT
    APPEND lv_line TO lt_code.

    APPEND 'MOVE-CORRESPONDING is_row TO row.' TO lt_code.  "#EC NOTEXT

    CONCATENATE `IF ` ls_tree_group-if_where `. cv_ok = 'X'. ENDIF.` INTO lv_line. "#EC NOTEXT
    APPEND lv_line TO lt_code.

    APPEND 'ENDFORM.' TO lt_code.                           "#EC NOTEXT
    APPEND '' TO lt_code.
  ENDLOOP.

  GENERATE SUBROUTINE POOL lt_code NAME ev_program
    MESSAGE lv_message
    LINE lv_pos.                    "#EC CI_GENERATE. <--- in lt_code[]

  " Ooops! wrong syntax in if_show of if_hide!
  CHECK sy-subrc <> 0.
  zcx_xtt_exception=>raise_dump( iv_message = lv_message ).
ENDMETHOD.


METHOD tree_raise_prepare.
  DATA:
    ls_tree      TYPE REF TO zcl_xtt_replace_block=>ts_tree,
    lr_table     TYPE REF TO data,
    lr_table_ref TYPE zcl_xtt_replace_block=>tt_std_ref_data, " For data changing
    lv_level     TYPE i.
  FIELD-SYMBOLS:
    <ls_tree_attr> TYPE zcl_xtt_replace_block=>ts_tree_attr,
    <ls_sub_tree>  TYPE zcl_xtt_replace_block=>ts_tree,
    <ls_row>       TYPE any,
    <lt_std_table> TYPE STANDARD TABLE.

  ir_tree->level = iv_level.

  " Sub levels first
  lv_level = iv_level + 1.
  LOOP AT ir_tree->sub_nodes ASSIGNING <ls_tree_attr>.
    ls_tree ?= <ls_tree_attr>-attr.
    tree_raise_prepare(
     ir_tree  = ls_tree
     iv_level = lv_level ).
  ENDLOOP.

**********************************************************************

  " Set sub_data --> fill lr_table
  " Standard table of subnodes
  IF ir_tree->sub_nodes IS NOT INITIAL.
    ASSIGN ir_tree->data->* TO <ls_row>.

    " Create sub levels
    CREATE DATA lr_table LIKE STANDARD TABLE OF <ls_row>.
    ASSIGN lr_table->* TO <lt_std_table>.
    LOOP AT ir_tree->sub_nodes ASSIGNING <ls_tree_attr>.
      ASSIGN <ls_tree_attr>-attr->* TO <ls_sub_tree>.
      ASSIGN <ls_sub_tree>-data->* TO <ls_row>.

      " 2 kinds
      APPEND:
       <ls_row>           TO <lt_std_table>, " Copy of data (convenient for handler to process)
       <ls_sub_tree>-data TO lr_table_ref.   " Original refs to data (to change it in handler)
    ENDLOOP.
  ENDIF.

  " Own call
  RAISE EVENT prepare_tree
   EXPORTING
     ir_tree         = ir_tree
     ir_data         = ir_tree->data
     ir_sub_data     = lr_table
     it_sub_data_ref = lr_table_ref.
ENDMETHOD.
ENDCLASS.

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
      value(IR_SUB_DATA) type ref to DATA optional .

  methods CONSTRUCTOR
    importing
      !IS_BLOCK type ANY optional
      value(IV_BLOCK_NAME) type STRING optional
      !IS_FIELD type ref to TS_FIELD optional .
  methods ADD_2_FIELDS
    importing
      !IV_NAME type CSEQUENCE
      !IV_TYPE type CSEQUENCE optional
      !IS_VALUE type ANY .
  methods FIND_MATCH
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
      value(RR_ROOT) type ref to DATA .
  class-methods TREE_RAISE_PREPARE
    importing
      !IR_TREE type ref to TS_TREE
      !IV_LEVEL type I .
  PROTECTED SECTION.
private section.

  data MV_BLOCK_BEGIN type STRING .
ENDCLASS.



CLASS ZCL_XTT_REPLACE_BLOCK IMPLEMENTATION.


METHOD add_2_fields.
  DATA:
    ls_field   TYPE TS_field,
    l_typekind TYPE abap_typekind,
    l_mask     TYPE string.
  FIELD-SYMBOLS:
   <fs_data>   TYPE any.

  " 1 Detect C_TYPE_* (References)
  ls_field-name = iv_name.
  DESCRIBE FIELD:
   is_value TYPE      l_typekind,
   is_value EDIT MASK l_mask.

  CASE l_typekind.
      " Special case for objects
    WHEN cl_abap_typedescr=>typekind_intf OR cl_abap_typedescr=>typekind_class OR cl_abap_typedescr=>typekind_oref.
      ls_field-oref = is_value.
      ls_field-typ = zcl_xtt_replace_block=>mc_type_object.
      INSERT ls_field INTO TABLE mt_fields.
      RETURN. " <-- That's all

      " Try to detect data
    WHEN cl_abap_typedescr=>typekind_dref.
      ls_field-dref = is_value.
      ASSIGN ls_field-dref->* TO <fs_data>.  " <-- Usally use dref
      DESCRIBE FIELD <fs_data> TYPE l_typekind.

    WHEN OTHERS.
      GET REFERENCE OF is_value INTO ls_field-dref.
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


METHOD constructor.
  DATA:
    lo_desc         TYPE REF TO cl_abap_typedescr,
    lo_sdesc        TYPE REF TO cl_abap_structdescr,
    lo_odesc        TYPE REF TO cl_abap_objectdescr,
    lo_ref          TYPE REF TO data,
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
  IF is_field IS NOT SUPPLIED.
    " Work by field symbol
    ASSIGN is_block TO <fs_block>.
  ELSE.
    " ROOT-FIELD1...
    iv_block_name = is_field->name.

    " Objects
    IF is_field->oref IS NOT INITIAL.
      ASSIGN is_field->oref TO <fs_block>.
    ELSE. " Other types
      ASSIGN is_field->dref->* TO <fs_block>.
    ENDIF.
  ENDIF.

  " What will search in template. At first '{ROOT-'
  CONCATENATE zcl_xtt_replace_block=>mc_char_block_begin iv_block_name INTO mv_block_begin.

  " 1 Is data (The most common)
  TRY.
      lo_desc = cl_abap_typedescr=>describe_by_data( <fs_block> ).
    CATCH cx_dynamic_check.
      MESSAGE x003(zsy_xtt) WITH iv_block_name.
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
        lo_ref ?= <fs_block>.
        ASSIGN lo_ref->* TO <fs_block>.
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
    lv_name = iv_block_name.
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

        me->add_2_fields(
         iv_name     = iv_block_name
         iv_type     = is_field->typ
         is_value    = <fs_block> ).
        RETURN.
      ENDIF.
**********************************************************************
**********************************************************************
      " Add every field
      LOOP AT lo_sdesc->components ASSIGNING <ls_comp>.
        " Name and data
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <fs_block> TO <fs_any>.
        CONCATENATE iv_block_name zcl_xtt_replace_block=>mc_char_name_delimiter <ls_comp>-name INTO l_field_name.

        " Insert field to me->fields
        me->add_2_fields(
         iv_name     = l_field_name
         is_value    = <fs_any> ).
      ENDLOOP.

      " Object processed as a structure ↑↑↑
    WHEN `\CLASS=CL_ABAP_CLASSDESCR` OR `\CLASS=CL_ABAP_INTFDESCR` OR `\CLASS=CL_ABAP_OBJECTDESCR`.
      lo_odesc ?= lo_desc.
      lo_block ?= <fs_block>.
      " Add every field
      LOOP AT lo_odesc->attributes ASSIGNING <ls_attr> WHERE visibility = cl_abap_objectdescr=>public.
        " Name and data
        ASSIGN lo_block->(<ls_attr>-name) TO <fs_any>.
        CONCATENATE iv_block_name zcl_xtt_replace_block=>mc_char_name_delimiter <ls_attr>-name INTO l_field_name.

        " Insert field to me->fields
        me->add_2_fields(
         iv_name     = l_field_name
         is_value    = <fs_any> ).
      ENDLOOP.

    WHEN OTHERS.
      " CL_ABAP_TABLEDESCR, CL_ABAP_ELEMDESCR
      me->add_2_fields(
       iv_name     = iv_block_name
       is_value    = <fs_block> ).
  ENDCASE.
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
      MESSAGE x002(zsy_xtt) WITH is_field->typ.
  ENDCASE.

  " Never will happen in MS Excel template (MS Word and pdf only)
  " Excel uses its own formats for date and time
  " Format depends on country
  IF <l_date> IS ASSIGNED.
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
  SPLIT iv_fields AT ';' INTO TABLE lt_fields.

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

  " Raise event
  tree_raise_prepare(
   ir_tree  = rr_root
   iv_level = 0 ).
ENDMETHOD.


METHOD tree_create_relat.
  TYPES:
    BEGIN OF ts_relat,
      node_key  TYPE string,
      relat_key TYPE string,
      tree      TYPE REF TO ts_tree,
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

    ls_relat-node_key  = <lv_node_key>.
    ls_relat-relat_key = <lv_relat_key>.
    CREATE DATA ls_relat-tree.
    GET REFERENCE OF <ls_row> INTO ls_relat-tree->data.

    INSERT ls_relat INTO TABLE lt_relat.
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
  " Raise events
  LOOP AT <lt_tree> INTO ls_tree.
    tree_raise_prepare(
     ir_tree  = ls_tree
     iv_level = 0 ).
  ENDLOOP.
ENDMETHOD.


METHOD tree_raise_prepare.
  DATA:
    ls_tree  TYPE REF TO zcl_xtt_replace_block=>ts_tree,
    lr_table TYPE REF TO data,
    lv_level TYPE i.
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

      APPEND <ls_row> TO <lt_std_table>.
    ENDLOOP.
  ENDIF.

  " Own call
  RAISE EVENT prepare_tree
   EXPORTING
     ir_tree     = ir_tree
     ir_data     = ir_tree->data
     ir_sub_data = lr_table.
ENDMETHOD.
ENDCLASS.

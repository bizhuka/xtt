class ZCL_XTT_REPLACE_BLOCK definition
  public
  final
  create public

  global friends ZCL_XTT_COND .

public section.
  type-pools ABAP .

  types:
    ts_field TYPE zss_xtt_field .
  types:
    BEGIN OF ts_field_ext.
        INCLUDE TYPE ts_field AS fld.
      TYPES:
        rb_level TYPE i,
        rb_id    TYPE string,
        desc     TYPE REF TO cl_abap_typedescr,
      END OF ts_field_ext .
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
        level     TYPE i,             " From 0
        sub_nodes TYPE tt_tree_attr,
        data      TYPE REF TO data,
      END OF ts_tree .
  types:
    tt_tree TYPE STANDARD TABLE OF REF TO ts_tree WITH DEFAULT KEY .
  types:
    tt_std_ref_data TYPE STANDARD TABLE OF REF TO data .

  constants:
    BEGIN OF mc_block,
        open       TYPE char1 VALUE '{',
        close      TYPE char1 VALUE '}',
        name_delim TYPE char1 VALUE '-',
        opt_delim  TYPE char1 VALUE ';',
      END OF mc_block .
  constants:
    BEGIN OF mc_type,
        struct   TYPE string VALUE 'struct',
        object   TYPE string VALUE 'object',
        table    TYPE string VALUE 'table',
        tree     TYPE string VALUE 'tree',
        integer  TYPE string VALUE 'integer',
        double   TYPE string VALUE 'double',
        date     TYPE string VALUE 'date',
        time     TYPE string VALUE 'time',
        boolean  TYPE string VALUE 'boolean',  " Excel (TRUE|FALSE)
        datetime TYPE string VALUE 'datetime', " Virtual type (DATE + TIME)
        string   TYPE string VALUE 'string',
        mask     TYPE string VALUE 'mask',     " Use WRITE TO
        as_is    TYPE string VALUE 'as_is',    " Do not convert to any format
        image    TYPE string VALUE 'image',
        block    TYPE string VALUE 'block',
      END OF mc_type .
  data MT_FIELDS type TT_FIELD .
  data MS_EXT type TS_FIELD_EXT read-only .

  class-events PREPARE_TREE
    exporting
      value(IR_TREE) type ref to TS_TREE
      value(IR_DATA) type ref to DATA
      value(IR_SUB_DATA) type ref to DATA optional
      value(IT_SUB_DATA_REF) type TT_STD_REF_DATA optional .

  methods CONSTRUCTOR
    importing
      !IO_XTT type ref to ZCL_XTT
      !IS_BLOCK type ANY optional
      !IV_BLOCK_NAME type CSEQUENCE optional
      !IS_FIELD type ref to TS_FIELD optional .
  methods REUSE_CHECK
    importing
      !IR_FIELD type ref to TS_FIELD
    returning
      value(RV_OK) type ABAP_BOOL .
  methods FIND_MATCH
    importing
      !IO_XTT type ref to ZCL_XTT
      !IS_SCOPE type ZSS_XTT_SCOPE
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
      value(RR_ROOT) type ref to TS_TREE .
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
  methods SET_ID
    importing
      !IV_ID type STRING .
  PROTECTED SECTION.
private section.

  data MV_SUB_OFFSET type I .

  class-methods _GET_FIELD_EXT
    importing
      !IO_XTT type ref to ZCL_XTT
      !IS_BLOCK type ANY
      !IV_BLOCK_NAME type CSEQUENCE
      !IS_FIELD type ref to TS_FIELD optional
    returning
      value(RS_FIELD_EXT) type TS_FIELD_EXT .
  class-methods _CHECK_OFFEST
    importing
      !IO_XTT type ref to ZCL_XTT
      !IS_SCOPE type ZSS_XTT_SCOPE
      !IV_CONTENT type STRING
    returning
      value(RV_OFFSET) type I .
ENDCLASS.



CLASS ZCL_XTT_REPLACE_BLOCK IMPLEMENTATION.


METHOD constructor.
  ms_ext = _get_field_ext( io_xtt        = io_xtt
                           is_block      = is_block
                           iv_block_name = iv_block_name
                           is_field      = is_field ).

  " Structure or object sub field
  DATA ls_sub_field          TYPE ts_field_ext.
  FIELD-SYMBOLS <fs_sub_fld> TYPE any.

  " Detect by name what to do
  CASE ms_ext-typ.
      " A structure is the most popular case
    WHEN mc_type-struct.
      DATA lo_sdesc TYPE REF TO cl_abap_structdescr.
      lo_sdesc ?= ms_ext-desc.

      " Get structure data
      FIELD-SYMBOLS <ls_struc> TYPE any.
      ASSIGN ms_ext-dref->* TO <ls_struc>.

      " Add every field
      FIELD-SYMBOLS <ls_comp>  TYPE abap_compdescr.
      LOOP AT lo_sdesc->components ASSIGNING <ls_comp>.
        " Name and data
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <ls_struc> TO <fs_sub_fld>.
        CONCATENATE ms_ext-name mc_block-name_delim <ls_comp>-name INTO ls_sub_field-name.

        " Add sub field
        ls_sub_field = _get_field_ext( io_xtt        = io_xtt
                                       is_block      = <fs_sub_fld>
                                       iv_block_name = ls_sub_field-name ).
        CHECK ls_sub_field-desc IS NOT INITIAL.
        ls_sub_field-fl_stat = abap_true.
        INSERT ls_sub_field-fld INTO TABLE mt_fields.
      ENDLOOP.

      " If it is plain structure
      IF is_field IS NOT INITIAL AND is_field->dref = ms_ext-dref.
        mv_sub_offset = strlen( is_field->name ) + 1. " strlen( mc_block-name_delim )
      ENDIF.

      " Object processed as a structure ↑↑↑
    WHEN mc_type-object.
      DATA lo_odesc TYPE REF TO cl_abap_objectdescr.
      DATA lo_cdesc TYPE REF TO cl_abap_classdescr.

      " Can read private & protected ?
      DATA lv_public_only TYPE abap_bool.
      lv_public_only = abap_true.

      TRY.
          " Is class ?
          lo_cdesc ?= ms_ext-desc.
          lo_odesc = lo_cdesc.

          DATA lt_friends TYPE abap_frndtypes_tab.
          lt_friends = lo_cdesc->get_friend_types( ).
          READ TABLE lt_friends TRANSPORTING NO FIELDS
           WITH KEY table_line->absolute_name = '\CLASS=ZCL_XTT_REPLACE_BLOCK'.
          IF sy-subrc = 0.
            CLEAR lv_public_only.
          ENDIF.
        CATCH cx_sy_move_cast_error.
          " Is interface ?
          lo_odesc ?= ms_ext-desc.
      ENDTRY.

      " Add every field
      FIELD-SYMBOLS <ls_attr>  TYPE abap_attrdescr.
      LOOP AT lo_odesc->attributes ASSIGNING <ls_attr>.
        IF lv_public_only = abap_true.
          CHECK <ls_attr>-visibility = cl_abap_objectdescr=>public.
        ENDIF.

        " Name and data
        ASSIGN ms_ext-oref->(<ls_attr>-name) TO <fs_sub_fld>.
        CONCATENATE ms_ext-name mc_block-name_delim <ls_attr>-name INTO ls_sub_field-name.

        " Add sub field
        ls_sub_field = _get_field_ext( io_xtt        = io_xtt
                                       is_block      = <fs_sub_fld>
                                       iv_block_name = ls_sub_field-name ).
        CHECK ls_sub_field-desc IS NOT INITIAL.
        ls_sub_field-fl_stat = abap_true.
        INSERT ls_sub_field-fld INTO TABLE mt_fields.
      ENDLOOP.

      " If it is the same class
      DO 1 TIMES.
        CHECK is_field IS NOT INITIAL
          AND is_field->dref IS NOT INITIAL
          AND ms_ext-oref    IS NOT INITIAL.

        FIELD-SYMBOLS <lo_obj> TYPE any.
        ASSIGN is_field->dref->* TO <lo_obj>.
        " TODO Could be dump ?
        CHECK <lo_obj> = ms_ext-oref.

        mv_sub_offset = strlen( is_field->name ) + 1. " strlen( mc_block-name_delim )
        mv_sub_offset = -1 * mv_sub_offset.
      ENDDO.

    WHEN OTHERS.
      " CL_ABAP_TABLEDESCR, CL_ABAP_ELEMDESCR
      INSERT ms_ext-fld INTO TABLE mt_fields.
  ENDCASE.
ENDMETHOD.


METHOD find_match.
  DATA lv_off TYPE i.
  lv_off = _check_offest( io_xtt     = io_xtt
                          is_scope   = is_scope
                          iv_content = cv_content ).
  CHECK lv_off > 0.

  " Replace by fields
  DATA lr_field TYPE REF TO ts_field.
  READ TABLE mt_fields REFERENCE INTO lr_field
   WITH TABLE KEY name = is_scope-field.

  " Cannot find
  IF sy-subrc <> 0 OR
    " Or complex type
     lr_field->typ = mc_type-struct OR
     lr_field->typ = mc_type-object OR
     " lr_field->typ = mc_type-tree   OR " TODO check!
     lr_field->typ = mc_type-table.

    " Markers for block's range (tables only). Can specify block's end(start) explicitly
    DO 1 TIMES.
      CHECK ms_ext-rb_level >= is_scope-sc_level
        AND ms_ext-typ <> mc_type-table
        AND ms_ext-typ <> mc_type-tree. " is_scope-field = ms_ext-name.

      CONCATENATE cv_content(is_scope-beg) cv_content+lv_off INTO cv_content RESPECTING BLANKS.
    ENDDO.

    " Go on
    RETURN.
  ENDIF.

  DATA lv_type_warn TYPE abap_bool.
  lv_type_warn = abap_true.

  " Set additional options
  FIELD-SYMBOLS <ls_pair> LIKE LINE OF is_scope-t_pair.
  LOOP AT is_scope-t_pair ASSIGNING <ls_pair>.  "<-- First item is field name
    CASE <ls_pair>-key.
        " Change type
      WHEN 'type'.
        lr_field->typ = <ls_pair>-val.
        CLEAR lv_type_warn.
    ENDCASE.
  ENDLOOP.

  CASE lr_field->typ.
    WHEN mc_type-mask.
      IF lv_type_warn = abap_true.
        MESSAGE w014(zsy_xtt) WITH lr_field->name INTO sy-msgli.
        io_xtt->add_log_message( iv_syst = abap_true ).
      ENDIF.

    WHEN mc_type-image.
      zcl_xtt_image=>create_image_decl( ir_field = lr_field
                                        it_pair  = is_scope-t_pair ).

*    WHEN mc_type-block.
*      " Just proccess later
*      RETURN.
  ENDCASE.

  " Replace value in event handler
  io_xtt->on_match_found( EXPORTING is_field   = lr_field
                                    iv_pos_beg = is_scope-beg
                                    iv_pos_end = is_scope-end
                          CHANGING  cv_content = cv_content ).
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
      WHEN mc_type-string.
        ASSIGN is_field->dref->* TO <l_string>.

        " If too long
        l_len = strlen( <l_string> ).
        IF l_len > 32767.
          rv_result = <l_string>(32767).
        ELSE.
          rv_result = <l_string>.
        ENDIF.

        " Delete symbols 0-31
        REPLACE ALL OCCURRENCES OF REGEX '[^[:print:]\r\n]' IN rv_result WITH ''.

        " Replace special chars
        rv_result = cl_http_utility=>escape_html( rv_result ).

        " Numbers
      WHEN mc_type-integer OR mc_type-double.
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
      WHEN mc_type-mask.
        ASSIGN is_field->dref->* TO <l_any>.
        WRITE <l_any> TO l_text LEFT-JUSTIFIED.
        rv_result = l_text.

        " Do not transform at all
      WHEN mc_type-as_is.
        ASSIGN is_field->dref->* TO <l_any>.
        rv_result = <l_any>.

        " Boolean
      WHEN mc_type-boolean.
        ASSIGN is_field->dref->* TO <l_string>.
        IF <l_string> IS NOT INITIAL. " = abap_true.
          rv_result = '1'.
        ELSE.
          rv_result = '0'.
        ENDIF.

        " Whole as a string like  d + t
      WHEN mc_type-datetime.
        ASSIGN is_field->dref->* TO <l_string>.

        " Both parts
        ASSIGN <l_string>(8)     TO <l_date> CASTING.
        ASSIGN <l_string>+8(6)   TO <l_time> CASTING.

        " Date
      WHEN mc_type-date.
        ASSIGN is_field->dref->* TO <l_date>.

        " Time
      WHEN mc_type-time.
        ASSIGN is_field->dref->* TO <l_time>.

      WHEN mc_type-image.
        rv_result = is_field->name.
        RETURN.

      WHEN OTHERS.
        MESSAGE e002(zsy_xtt) WITH is_field->typ is_field->name INTO sy-msgli.
        zcx_eui_no_check=>raise_sys_error( ).
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


METHOD reuse_check.
  CHECK mv_sub_offset IS NOT INITIAL.

  DATA lv_off    LIKE mv_sub_offset.
  DATA lv_is_obj TYPE abap_bool.
  IF mv_sub_offset > 0.
    lv_off = mv_sub_offset.
  ELSE.
    lv_off = -1 * mv_sub_offset.
    lv_is_obj = abap_true.
  ENDIF.

  " rv_ok = abap_false
  DEFINE check_subrc.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  END-OF-DEFINITION.

  " New source
  FIELD-SYMBOLS <ls_field_src> TYPE any.
  ASSIGN ir_field->dref->* TO <ls_field_src>.
  check_subrc.

  " No dynamic fields
  DELETE mt_fields WHERE fl_stat <> abap_true.          "#EC CI_SORTSEQ

  " Check by existing items
  DATA lr_field TYPE REF TO ts_field.
  LOOP AT mt_fields REFERENCE INTO lr_field.
    DATA lv_tabix TYPE sytabix.
    lv_tabix = sy-tabix.

    DATA lv_name TYPE string.
    lv_name = lr_field->name+lv_off.

    " New source field
    FIELD-SYMBOLS <lv_src> TYPE any.
    IF lv_is_obj = abap_true.
      CONCATENATE `<ls_field_src>->` lv_name INTO lv_name.
      ASSIGN (lv_name) TO <lv_src>.
    ELSE.
      ASSIGN COMPONENT lv_name OF STRUCTURE <ls_field_src> TO <lv_src>.
    ENDIF.
    check_subrc.

    IF lr_field->oref IS INITIAL.
      " Is ref to ref ->*->* ?
      DO.
        DATA lv_kind TYPE abap_typekind.
        DESCRIBE FIELD <lv_src> TYPE lv_kind.
        IF lv_kind <> cl_abap_typedescr=>typekind_dref OR <lv_src> IS INITIAL.
          EXIT.
        ENDIF.
        ASSIGN <lv_src>->* TO <lv_src>.
      ENDDO.

      " Update with new value
      GET REFERENCE OF <lv_src> INTO lr_field->dref.
      CONTINUE.
    ENDIF.

    TRY.
        CALL METHOD lr_field->oref->('CLONE')
          EXPORTING
            source = <lv_src>
          RECEIVING
            result = lr_field->oref.

        IF lr_field->oref IS INITIAL.
          lr_field->oref = <lv_src>.
        ENDIF.

        " No clone method ?
      CATCH cx_sy_dyn_call_illegal_method.
        lr_field->oref = <lv_src>.
    ENDTRY.
  ENDLOOP.

  " Context for dynamic fields
  ms_ext-dref = ir_field->dref.
  ms_ext-oref = ir_field->oref.

  rv_ok = abap_true.
ENDMETHOD.


METHOD set_id.
  " Replace in child
  FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.
  LOOP AT mt_fields ASSIGNING <ls_field>.
    <ls_field>-fl_id = <ls_field>-name.

    REPLACE FIRST OCCURRENCE OF ms_ext-rb_id IN <ls_field>-fl_id
                                WITH iv_id.
  ENDLOOP.

  " Own replace
  ms_ext-rb_id = iv_id.
ENDMETHOD.


  METHOD tree_create.
    DATA:
      lo_tdesc    TYPE REF TO cl_abap_tabledescr,
      lo_sdesc    TYPE REF TO cl_abap_structdescr,
      ls_curtree  TYPE REF TO ts_tree,
      ls_subtree  TYPE REF TO ts_tree,
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
        node_key  TYPE string,
        relat_key TYPE string,
*      relat_not_empty TYPE abap_bool,
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


  METHOD tree_raise_prepare.
    DATA:
      ls_tree      TYPE REF TO ts_tree,
      lr_table     TYPE REF TO data,
      lt_table_ref TYPE tt_std_ref_data, " For data changing
      lv_level     TYPE i.
    FIELD-SYMBOLS:
      <ls_tree_attr> TYPE ts_tree_attr,
      <ls_sub_tree>  TYPE ts_tree,
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
         <ls_sub_tree>-data TO lt_table_ref.   " Original refs to data (to change it in handler)
      ENDLOOP.
    ENDIF.

    " Own call
    RAISE EVENT prepare_tree
     EXPORTING
       ir_tree         = ir_tree
       ir_data         = ir_tree->data
       ir_sub_data     = lr_table
       it_sub_data_ref = lt_table_ref.
  ENDMETHOD.


METHOD _check_offest.
  DATA: lv_len TYPE i, lv_off LIKE rv_offset.
  lv_len = strlen( iv_content ).
  lv_off = is_scope-end + 1.

  IF lv_off > lv_len.
    MESSAGE e020(zsy_xtt) WITH lv_off is_scope-field INTO sy-msgli.
    io_xtt->add_log_message( iv_syst = abap_true ).
    RETURN.
  ENDIF.

  IF iv_content+is_scope-beg(1) <> '{' OR
     iv_content+is_scope-end(1) <> '}'.
    MESSAGE e023(zsy_xtt) WITH is_scope-field INTO sy-msgli.
    io_xtt->add_log_message( iv_syst = abap_true ).
    RETURN.
  ENDIF.

  " Result
  rv_offset = lv_off.
ENDMETHOD.


METHOD _get_field_ext.
  " Result block
  FIELD-SYMBOLS <fs_block> TYPE any.

  IF is_field IS INITIAL.
    rs_field_ext-name = iv_block_name.
    ASSIGN is_block TO <fs_block>.
  ELSE." ROOT-FIELD1...
    rs_field_ext-name  = is_field->name.
    rs_field_ext-rb_id = is_field->fl_id.
    IF is_field->dref IS NOT INITIAL.
      ASSIGN is_field->dref->* TO <fs_block>.
    ELSEIF is_field->oref IS NOT INITIAL. " Objects
      ASSIGN is_field->oref    TO <fs_block>.
    ENDIF.
    CHECK <fs_block> IS ASSIGNED.
  ENDIF.
  " Same with name by default
  IF rs_field_ext-rb_id IS INITIAL.
    rs_field_ext-rb_id = rs_field_ext-name.
  ENDIF.

  " Ingone option of grandchildren {R-T-FIELD}
  FIND ALL OCCURRENCES OF mc_block-name_delim IN rs_field_ext-name MATCH COUNT rs_field_ext-rb_level.

  " TODO join with ADD_2_FIELDS
  rs_field_ext-desc = cl_abap_typedescr=>describe_by_data( <fs_block> ).
  DO.
    IF rs_field_ext-desc->type_kind = cl_abap_typedescr=>typekind_dref.
      DATA lv_ref TYPE REF TO data.
      lv_ref ?= <fs_block>.

      " Oops
      IF lv_ref IS INITIAL.
        MESSAGE w004(zsy_xtt) WITH rs_field_ext-name INTO sy-msgli.
        io_xtt->add_log_message( iv_syst = abap_true ).
        RETURN.
      ENDIF.

      rs_field_ext-desc = cl_abap_typedescr=>describe_by_data_ref( lv_ref ).
      ASSIGN lv_ref->* TO <fs_block>.
      rs_field_ext-dref = lv_ref.
      CONTINUE.
    ENDIF.

    CASE rs_field_ext-desc->type_kind.
        " Structures
      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
        rs_field_ext-typ = mc_type-struct.

        IF rs_field_ext-desc->absolute_name = '\CLASS=ZCL_XTT_REPLACE_BLOCK\TYPE=TS_TREE'.
          rs_field_ext-typ = mc_type-tree.
        ENDIF.

        " Special case for objects
      WHEN cl_abap_typedescr=>typekind_intf OR cl_abap_typedescr=>typekind_class OR cl_abap_typedescr=>typekind_oref.
        " Only for objects
        IF <fs_block> IS NOT INITIAL.
          rs_field_ext-typ  = mc_type-object.
          rs_field_ext-oref = <fs_block>.
          rs_field_ext-desc = cl_abap_typedescr=>describe_by_object_ref( rs_field_ext-oref ).

          IF rs_field_ext-desc->absolute_name = '\CLASS=ZCL_XTT_IMAGE'.
            rs_field_ext-typ = mc_type-image.
          ENDIF.
        ENDIF.

        RETURN.

        " Tables
      WHEN cl_abap_typedescr=>typekind_table.
        rs_field_ext-typ = mc_type-table.

        " Integer, byte, short
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
        rs_field_ext-typ = mc_type-integer.

        " Use mask and don't delete dots in ZCL_XTT_REPLACE_BLOCK=>get_as_string
      WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
        DATA l_mask TYPE string.
        DESCRIBE FIELD <fs_block> EDIT MASK l_mask.
        IF l_mask IS INITIAL.
          rs_field_ext-typ = mc_type-integer.
        ELSE. " For safety
          rs_field_ext-typ = mc_type-mask.
        ENDIF.

        " Double
      WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
           '/' OR 'a' OR 'e'. " cl_abap_typedescr=>typekind_decfloat  OR cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
        rs_field_ext-typ = mc_type-double.

        " Date
      WHEN cl_abap_typedescr=>typekind_date.
        rs_field_ext-typ = mc_type-date.

        " Time
      WHEN cl_abap_typedescr=>typekind_time.
        rs_field_ext-typ = mc_type-time.

        " No trunsformation for STRING
      WHEN cl_abap_typedescr=>typekind_char OR cl_abap_typedescr=>typekind_clike OR
           cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_w OR
           " Binary data in template? Dump ?
           cl_abap_typedescr=>typekind_hex OR cl_abap_typedescr=>typekind_xsequence OR cl_abap_typedescr=>typekind_xstring.
        rs_field_ext-typ = mc_type-string.

*TYPEKIND_IREF, TYPEKIND_BREF
*TYPEKIND_DATA, TYPEKIND_SIMPLE, TYPEKIND_ANY
      WHEN OTHERS.
        MESSAGE e002(zsy_xtt) WITH rs_field_ext-desc->type_kind INTO sy-msgli.
        zcx_eui_no_check=>raise_sys_error( ).
    ENDCASE.

    " 1 time only
    EXIT.
  ENDDO.

  " Result
  GET REFERENCE OF <fs_block> INTO rs_field_ext-dref.

  " Oops
  CHECK rs_field_ext-desc IS INITIAL.
  IF rs_field_ext-name IS INITIAL AND is_field IS NOT INITIAL.
    rs_field_ext-name = is_field->name.
  ENDIF.
  MESSAGE w004(zsy_xtt) WITH rs_field_ext-name INTO sy-msgli.
  io_xtt->add_log_message( iv_syst = abap_true ).
ENDMETHOD.
ENDCLASS.

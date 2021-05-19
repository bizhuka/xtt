class ZCL_XTT_XML_BASE definition
  public
  inheriting from ZCL_XTT
  create public .

public section.
  type-pools ABAP .
  class ZCL_XTT_REPLACE_BLOCK definition load .

  types TS_BOUNDS type ZSS_XTT_BOUNDS .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZIF_XTT_FILE
      !IV_BODY_TAG type CSEQUENCE
      !IV_ROW_TAG type CSEQUENCE
      !IV_PATH type CSEQUENCE default 'document.xml'
      !IV_OLE_EXT type STRING optional
      !IV_OLE_EXT_FORMAT type I optional
      !IV_SKIP_TAGS type ABAP_BOOL optional
      !IV_TABLE_PAGE_BREAK type STRING optional .

  methods DOWNLOAD
    redefinition .
  methods GET_RAW
    redefinition .
  methods MERGE
    redefinition .
protected section.

  data MV_BODY_TAG type STRING .
  data MV_ROW_TAG type STRING .
  data MV_OLE_EXT_FORMAT type I .
  data MV_FILE_CONTENT type STRING .
  data MV_VALUE type STRING .
  data MV_PREFIX type STRING .
  data MV_PATH type STRING .

  methods BOUNDS_FROM_BODY
    importing
      !IV_CONTEXT type CSEQUENCE
      !IV_ROOT_IS_TABLE type ABAP_BOOL
      !IV_BLOCK_NAME type CSEQUENCE
    returning
      value(RS_BOUNDS) type TS_BOUNDS .

  methods ON_MATCH_FOUND
    redefinition .
private section.

  data MV_IF_TABLE_PAGE_BREAK type STRING .

  methods BOUNDS_FROM_ROW
    importing
      !IV_CONTEXT type CSEQUENCE
      !IS_FIELD type ZCL_XTT_REPLACE_BLOCK=>TS_FIELD
    returning
      value(RS_BOUNDS) type TS_BOUNDS .
  methods BOUNDS_SPLIT
    importing
      !IV_NAME type CSEQUENCE
    changing
      !CS_BOUNDS type TS_BOUNDS
      !CV_MIDDLE type CSEQUENCE .
  methods DO_MERGE
    importing
      !IV_ROOT_IS_TABLE type ABAP_BOOL default ABAP_FALSE
      !IV_TABIX type SYTABIX optional
      !IO_BLOCK type ref to ZCL_XTT_REPLACE_BLOCK
      !IV_FORCE type ABAP_BOOL optional
    changing
      !CV_CONTENT type STRING .
  methods MERGE_SUB_STRUCTURES
    importing
      !IR_FIELD type ref to ZCL_XTT_REPLACE_BLOCK=>TS_FIELD
    changing
      !CV_CONTENT type STRING .
  methods MERGE_TABLES
    importing
      !IV_ROOT_IS_TABLE type ABAP_BOOL
      !IR_FIELD type ref to ZCL_XTT_REPLACE_BLOCK=>TS_FIELD
    changing
      !CV_CONTENT type STRING .
  methods MERGE_TREES
    importing
      !IV_ROOT_IS_TABLE type ABAP_BOOL
      !IR_FIELD type ref to ZCL_XTT_REPLACE_BLOCK=>TS_FIELD
    changing
      !CV_CONTENT type STRING .
  methods DELETE_TAGS_BEFORE_SEARCH
    importing
      !IV_BLOCK_NAME type CSEQUENCE .
  methods DELETE_LAST_PAGE_BREAK .
  methods READ_SCOPES
    importing
      !IO_BLOCK type ref to ZCL_XTT_REPLACE_BLOCK
      !IV_TABIX type SYTABIX
      !IV_FORCE type ABAP_BOOL
    exporting
      !EO_SCOPE type ref to ZCL_XTT_SCOPE
    changing
      !CV_CONTENT type STRING .
ENDCLASS.



CLASS ZCL_XTT_XML_BASE IMPLEMENTATION.


METHOD bounds_from_body.
  CLEAR rs_bounds-with_tag.

  rs_bounds-t_match = zcl_xtt_util=>get_tag_matches( iv_context = iv_context
                                                     iv_tag     = mv_body_tag ).
  " Wrong template?
  IF rs_bounds-t_match[] IS INITIAL.
    MESSAGE w008(zsy_xtt) WITH mv_body_tag INTO sy-msgli.
    add_log_message( iv_syst = abap_true ).

    " Go on
    RETURN.
  ENDIF.

  " First and last matches
  rs_bounds-first_match = 1.
  rs_bounds-last_match  = lines( rs_bounds-t_match ).
ENDMETHOD.


METHOD bounds_from_row.
  rs_bounds-with_tag = abap_true.

  DATA lv_find_str TYPE string.
  CONCATENATE `\{` is_field-name `\b[^}]*\}`  INTO lv_find_str.

  rs_bounds-t_match = zcl_xtt_util=>get_tag_matches( iv_context = iv_context
                                                     iv_tag     = mv_row_tag
                                                     iv_tag_add = lv_find_str ).
  " Wrong template?
  IF rs_bounds-t_match[] IS INITIAL.
    MESSAGE w010(zsy_xtt) WITH mv_row_tag is_field-name INTO sy-msgli.
    add_log_message( iv_syst = abap_true ).
    RETURN.
  ENDIF.

  DATA lv_tabix   TYPE sytabix.
  FIELD-SYMBOLS:
    <ls_match>    LIKE LINE OF rs_bounds-t_match,
    <ls_submatch> LIKE LINE OF <ls_match>-submatches.

  " {iv_block_name*  -  CONCATENATE '{' iv_block_name '*' INTO lv_pattern.
  LOOP AT rs_bounds-t_match ASSIGNING <ls_match>.
    lv_tabix = sy-tabix.

*    lv_text = iv_context+<ls_match>-offset(<ls_match>-length).
*    CHECK lv_text CP lv_pattern.

    " Only
    READ TABLE <ls_match>-submatches ASSIGNING <ls_submatch> INDEX 3.
    CHECK sy-subrc = 0
      AND <ls_match>-offset = <ls_submatch>-offset
      AND <ls_match>-length = <ls_submatch>-length.

    rs_bounds-last_match = lv_tabix.
    IF rs_bounds-first_match IS INITIAL.
      rs_bounds-first_match = lv_tabix.
    ENDIF.
  ENDLOOP.

  " Extend bounds for iv_tag
  rs_bounds-first_match = rs_bounds-first_match - 1.
  rs_bounds-last_match  = rs_bounds-last_match + 1.
ENDMETHOD.


METHOD bounds_split.
  CLEAR: cs_bounds-pos_beg,
         cs_bounds-pos_end,
         cs_bounds-pos_cnt,
         " Resulting fields
         cs_bounds-text_before,
         cs_bounds-text_after.

  " 1 - Text before body
  FIELD-SYMBOLS <ls_match>   LIKE LINE OF cs_bounds-t_match.
  READ TABLE cs_bounds-t_match ASSIGNING <ls_match> INDEX cs_bounds-first_match.

  " TODO silent mode ?
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF cs_bounds-first_match >= cs_bounds-last_match.
    MESSAGE e009(zsy_xtt) WITH iv_name INTO sy-msgli.
    add_log_message( iv_syst = abap_true ).
    RETURN.
  ENDIF.

  " Does need an open tag?
  IF cs_bounds-with_tag = abap_true.
    cs_bounds-pos_beg = <ls_match>-offset.
  ELSE.
    cs_bounds-pos_beg = <ls_match>-offset + <ls_match>-length.
  ENDIF.
  cs_bounds-text_before = cv_middle(cs_bounds-pos_beg).

*************
  " 2 - Text after body
  READ TABLE cs_bounds-t_match ASSIGNING <ls_match> INDEX cs_bounds-last_match.

  IF cs_bounds-with_tag = abap_true.
    cs_bounds-pos_end = <ls_match>-offset + <ls_match>-length.
  ELSE.
    cs_bounds-pos_end = <ls_match>-offset.
  ENDIF.
  cs_bounds-text_after = cv_middle+cs_bounds-pos_end.

*************
  " 3 - Body
  cs_bounds-pos_cnt = cs_bounds-pos_end - cs_bounds-pos_beg.
  cv_middle = cv_middle+cs_bounds-pos_beg(cs_bounds-pos_cnt).
ENDMETHOD.


METHOD constructor.
  super->constructor(
   io_file    = io_file
   iv_ole_ext = iv_ole_ext  ).

  " For regex
  mv_body_tag            = iv_body_tag.
  mv_row_tag             = iv_row_tag.
  mv_path                = iv_path.
  mv_skip_tags           = iv_skip_tags.
  mv_if_table_page_break = iv_table_page_break.

  " For download & show methods
  mv_ole_ext_format = iv_ole_ext_format.

  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  TRY.
      " Is archive ?
      CHECK mv_path NS '/'.

      io_file->get_content( IMPORTING ev_as_string = mv_file_content ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      add_log_message( io_exception = lo_no_check ).
  ENDTRY.
ENDMETHOD.


METHOD delete_last_page_break.
  CHECK mv_if_table_page_break IS NOT INITIAL.

  " Delete last page break
  DATA lt_match TYPE match_result_tab.
  FIND ALL OCCURRENCES OF mv_if_table_page_break IN mv_file_content RESULTS lt_match.
  CHECK sy-subrc = 0.

  " Get last OCCURRENCES OF page-break
  DATA lv_len   TYPE i.
  DATA ls_match TYPE REF TO match_result.

  lv_len = lines( lt_match ).
  READ TABLE lt_match REFERENCE INTO ls_match INDEX lv_len.
  CHECK sy-subrc = 0.

  " Delete last page break
  lv_len = ls_match->offset + ls_match->length.
  CONCATENATE
   mv_file_content(ls_match->offset)
   mv_file_content+lv_len INTO mv_file_content.
ENDMETHOD.


METHOD delete_tags_before_search.
  CHECK mv_skip_tags = abap_true.

  DATA: lv_cnt  TYPE i, lv_from TYPE string, lv_to TYPE string.
  lv_cnt = strlen( mv_file_content ).

  CONCATENATE `\{`
              `(<[^\>]+>)*`  " <--- delete tags
              iv_block_name INTO lv_from.
  CONCATENATE `{`
              iv_block_name INTO lv_to.

  REPLACE ALL OCCURRENCES OF REGEX lv_from IN mv_file_content WITH lv_to RESPECTING CASE.
  CHECK sy-subrc = 0.

  lv_cnt = lv_cnt - strlen( mv_file_content ).
  MESSAGE e013(zsy_xtt) WITH lv_cnt lv_to INTO sy-msgli.
  add_log_message( iv_syst = abap_true ).
ENDMETHOD.


METHOD download.
  " If need saveAs
  DATA lv_open TYPE string.
  lv_open = iv_open.
  IF iv_open IS NOT SUPPLIED AND mv_ole_ext IS NOT INITIAL AND mv_ole_ext_format IS NOT INITIAL.
    lv_open = mc_by-ole.
  ENDIF.

  super->download(
   EXPORTING
     iv_open     = lv_open
     iv_zip      = iv_zip
   CHANGING
     cv_ole_app  = cv_ole_app
     cv_ole_doc  = cv_ole_doc
     cv_fullpath = cv_fullpath ).

  " If opened as OLE
  CHECK lv_open CP 'OLE*'.

  " New file name
  DATA lv_path   TYPE string.
  DATA lv_no_ext TYPE string.
  zcl_eui_file=>split_file_path(
    EXPORTING
      iv_fullpath   = cv_fullpath
    IMPORTING
      ev_file_noext = lv_no_ext
      ev_path       = lv_path ).
  CONCATENATE lv_path lv_no_ext `.` mv_ole_ext INTO cv_fullpath.

  " Already exist. Add date and time
  IF zcl_eui_file=>file_exist( cv_fullpath ) = abap_true.
    CONCATENATE lv_path lv_no_ext ` ` sy-datum ` ` sy-uzeit `.` mv_ole_ext INTO cv_fullpath.
  ENDIF.

  CALL METHOD OF cv_ole_doc 'SaveAs'
    EXPORTING
      #1 = cv_fullpath
      #2 = mv_ole_ext_format.

  IF lv_open = mc_by-ole_hide.
    CALL METHOD OF cv_ole_app 'QUIT'.
    FREE OBJECT: cv_ole_doc, cv_ole_app.
  ENDIF.
ENDMETHOD.


METHOD do_merge.
  IF iv_root_is_table <> abap_true.
    DATA lo_scope TYPE REF TO zcl_xtt_scope.
    read_scopes( EXPORTING io_block = io_block
                           iv_tabix = iv_tabix
                           iv_force = iv_force
                 IMPORTING eo_scope = lo_scope
                 CHANGING  cv_content = cv_content ).

    " What will search in template. At first '{ROOT-'
    " Already found scopes
    FIELD-SYMBOLS <ls_scope> LIKE LINE OF lo_scope->mt_scope.
    LOOP AT lo_scope->mt_scope ASSIGNING <ls_scope>.
      io_block->find_match( EXPORTING io_xtt     = me
                                      is_scope   = <ls_scope>
                            CHANGING  cv_content = cv_content ).
    ENDLOOP.
  ENDIF.

  DATA lr_field TYPE REF TO zcl_xtt_replace_block=>ts_field.
  LOOP AT io_block->mt_fields REFERENCE INTO lr_field.
    CASE lr_field->typ.

        " merge-2 Structures and objects
      WHEN zcl_xtt_replace_block=>mc_type-struct OR zcl_xtt_replace_block=>mc_type-object.
        merge_sub_structures( EXPORTING ir_field   = lr_field
                              CHANGING  cv_content = cv_content ).
        " merge-3 Array types
      WHEN zcl_xtt_replace_block=>mc_type-table.
        merge_tables( EXPORTING ir_field         = lr_field
                                iv_root_is_table = iv_root_is_table
                      CHANGING  cv_content       = cv_content ).
        " merge-4 And trees
      WHEN zcl_xtt_replace_block=>mc_type-tree.
        merge_trees( EXPORTING ir_field         = lr_field
                               iv_root_is_table = iv_root_is_table
                     CHANGING  cv_content       = cv_content ).
    ENDCASE.
  ENDLOOP.
ENDMETHOD.


METHOD get_raw.
  " Can convert XML or HTML result to pdf or attach to email for example
  rv_content = zcl_eui_conv=>string_to_xstring( mv_file_content ).

  " Change content in special cases
  DATA lr_content TYPE REF TO xstring.
  GET REFERENCE OF rv_content INTO lr_content.
  RAISE EVENT prepare_raw
   EXPORTING
     iv_path    = mv_path
     ir_content = lr_content.
ENDMETHOD.


METHOD merge.
  " For chain calls
  ro_xtt = super->merge( is_block      = is_block
                         iv_block_name = iv_block_name ).

  delete_tags_before_search( iv_block_name ).

  " Special case
  DATA lv_root_is_table TYPE abap_bool.
  lv_root_is_table = zcl_xtt_util=>is_table( is_block ).

  DATA ls_bounds TYPE ts_bounds.
  ls_bounds = bounds_from_body( iv_context       = mv_file_content
                                iv_root_is_table = lv_root_is_table
                                iv_block_name    = iv_block_name ).

  " Divide to 3 parts
  bounds_split( EXPORTING iv_name   = iv_block_name
                CHANGING  cs_bounds = ls_bounds
                          cv_middle = mv_file_content ).

  " Update middle part (Body)
  DATA lo_no_check TYPE REF TO zcx_eui_no_check.
  TRY.
      " Prepare for replacement
      DATA lo_replace_block TYPE REF TO zcl_xtt_replace_block.
      CREATE OBJECT lo_replace_block
        EXPORTING
          io_xtt        = me
          is_block      = is_block
          iv_block_name = iv_block_name.

      do_merge(
       EXPORTING
        iv_root_is_table = lv_root_is_table
        io_block         = lo_replace_block
       CHANGING
        cv_content       = mv_file_content ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      add_log_message( io_exception = lo_no_check ).
  ENDTRY.

  " And just concatenate
  CONCATENATE ls_bounds-text_before
              mv_file_content
              ls_bounds-text_after INTO mv_file_content RESPECTING BLANKS.

  " Move from get_raw
  IF lv_root_is_table = abap_true.
    delete_last_page_break( ).
  ENDIF.
ENDMETHOD.


METHOD merge_sub_structures.
  " No data ?
  CASE ir_field->typ.
    WHEN zcl_xtt_replace_block=>mc_type-struct.
      CHECK ir_field->dref IS NOT INITIAL.

    WHEN zcl_xtt_replace_block=>mc_type-object.
      CHECK ir_field->oref IS NOT INITIAL.
  ENDCASE.

  " Based on nested structure
  DATA lo_new_replace_block TYPE REF TO zcl_xtt_replace_block.
  CREATE OBJECT lo_new_replace_block
    EXPORTING
      io_xtt   = me
      is_field = ir_field.

  do_merge( EXPORTING io_block   = lo_new_replace_block
            CHANGING  cv_content = cv_content ).
ENDMETHOD.


METHOD merge_tables.                                     .
  DATA ls_bounds TYPE ts_bounds.
  CLEAR ls_bounds.

  " Detect bounds
  IF iv_root_is_table <> abap_true.
    ls_bounds = bounds_from_row( iv_context = cv_content
                                 is_field   = ir_field->* ).
    " Divide to 3 parts
    bounds_split( EXPORTING iv_name   = ir_field->name
                  CHANGING  cs_bounds = ls_bounds
                            cv_middle = cv_content " TODO always empty ???
                 ).
    " No proper bounds found
    IF ls_bounds-text_before IS INITIAL AND ls_bounds-text_after IS INITIAL
       " AND iv_root_is_table IS SUPPLIED.
       .
      MESSAGE w012(zsy_xtt) WITH 'table' ir_field->name INTO sy-msgli.
      add_log_message( iv_syst = abap_true ).
      RETURN.
    ENDIF.
  ENDIF.

  " CHECK cv_content IS NOT INITIAL.
  DATA lv_middle TYPE string.
  lv_middle = cv_content.

  " Replicate middle
  FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
  ASSIGN ir_field->dref->* TO <lt_table>.

  " Index for hashed table
  DATA lv_tabix TYPE sytabix.
  lv_tabix = 0.
  LOOP AT <lt_table> REFERENCE INTO ir_field->dref.
    lv_tabix = lv_tabix + 1.

    DATA lv_first TYPE abap_bool.
    IF lv_tabix = 1.
      lv_first = abap_true.
    ELSE.
      lv_first = abap_false.
    ENDIF.

    " Create merge description
    DATA lo_new_replace_block TYPE REF TO zcl_xtt_replace_block.
    IF lo_new_replace_block IS INITIAL OR lo_new_replace_block->reuse_check( ir_field ) <> abap_true.
      CREATE OBJECT lo_new_replace_block
        EXPORTING
          io_xtt   = me
          is_field = ir_field.
    ENDIF.

    " Recursion
    DATA lv_clone TYPE string.
    lv_clone = lv_middle.

    do_merge( EXPORTING io_block   = lo_new_replace_block
                        iv_tabix   = lv_tabix
                        iv_force   = lv_first
              CHANGING  cv_content = lv_clone ).

    " Add
    CONCATENATE ls_bounds-text_before lv_clone INTO ls_bounds-text_before RESPECTING BLANKS.

    " 1 merge cache " TODO TEST speed
    CHECK iv_root_is_table = abap_true.
    CLEAR _scopes[]. "clear_all( ).
  ENDLOOP.

  " End
  CONCATENATE ls_bounds-text_before ls_bounds-text_after INTO cv_content RESPECTING BLANKS.
ENDMETHOD.


METHOD merge_trees.                                                              .
  DATA ls_bounds TYPE ts_bounds.
  CLEAR ls_bounds.

  " Detect bounds
  IF iv_root_is_table <> abap_true.
    ls_bounds = bounds_from_row( iv_context = cv_content
                                 is_field   = ir_field->* ).
    " Divide to 3 parts
    bounds_split( EXPORTING iv_name   = ir_field->name
                  CHANGING  cs_bounds = ls_bounds
                            cv_middle = cv_content " TODO always empty ???
                ).
    " For trees
    DATA lo_tree_handler TYPE REF TO lcl_tree_handler.
    CREATE OBJECT lo_tree_handler
      EXPORTING
        io_xtt        = me
        iv_block_name = ir_field->name.

    " Has match in a template
    DATA lv_has_text TYPE abap_bool.
    lv_has_text = lo_tree_handler->fill_text_match( iv_tr_id  = ir_field->name
                                                    iv_middle = cv_content
                                                    is_bounds = ls_bounds
                                                    ir_field  = ir_field ).
    " No proper bounds found
    IF ls_bounds-text_before IS INITIAL AND ls_bounds-text_after IS INITIAL
       " AND iv_root_is_table IS SUPPLIED.
       AND lv_has_text <> abap_true. " lt_text_match[] IS INITIAL.
      MESSAGE w012(zsy_xtt) WITH 'tree' ir_field->name INTO sy-msgli.
      add_log_message( iv_syst = abap_true ).
      RETURN.
    ENDIF.
  ENDIF.

  DATA lr_tree TYPE REF TO zcl_xtt_replace_block=>ts_tree.
  lr_tree ?= ir_field->dref.

  lo_tree_handler->add_tree_data(
   EXPORTING
     io_owner = me
     ir_tree  = lr_tree
     iv_tabix = 0
   CHANGING
     cv_text  = ls_bounds-text_before ).

  " And set result
  CONCATENATE ls_bounds-text_before
              ls_bounds-text_after INTO cv_content RESPECTING BLANKS.
ENDMETHOD.


METHOD on_match_found.
  " Just skip
  CHECK is_field->typ <> zcl_xtt_replace_block=>mc_type-tree.

  " Try to get value as a string
  IF mv_value IS INITIAL.
    mv_value = zcl_xtt_replace_block=>get_as_string( is_field = is_field ).
  ENDIF.

  " Write new value
  iv_pos_end = iv_pos_end + 1.
  CONCATENATE
    cv_content(iv_pos_beg)
       mv_prefix mv_value
    cv_content+iv_pos_end INTO cv_content RESPECTING BLANKS.

  " Used in sub classes
  CLEAR:
   mv_prefix,
   mv_value.
ENDMETHOD.


METHOD read_scopes.
  " Position holder
  DATA lv_new TYPE abap_bool.
  _get_scope( EXPORTING io_block = io_block
                        iv_force = iv_force
              IMPORTING eo_scope = eo_scope
                        ev_new   = lv_new ).
  IF lv_new = abap_true.
    eo_scope->get_scopes( EXPORTING io_xtt     = me
                          CHANGING  cv_content = cv_content ).
  ENDIF.

  eo_scope->calc_cond_matches( io_xtt   = me
                               io_block = io_block
                               iv_tabix = iv_tabix
                               iv_init  = lv_new ).
ENDMETHOD.
ENDCLASS.

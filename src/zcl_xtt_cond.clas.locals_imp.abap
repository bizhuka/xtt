CLASS lcl_ast_node IMPLEMENTATION.
  METHOD _to_number.
    DATA lv_val TYPE string.
    lv_val = iv_value.
    REPLACE FIRST OCCURRENCE OF `,` IN lv_val WITH `.`.
    TRY.
        rv_number = lv_val.
      CATCH cx_sy_conversion_error.
        zcx_xtt_exception=>raise_sys_error( iv_message = |Operand '{ iv_value }' cannot be converted to number| ).
    ENDTRY.
  ENDMETHOD.

  METHOD is_numeric.
    rv_num = abap_false.
  ENDMETHOD.

  METHOD _is_number.
    DATA lv_type        TYPE c LENGTH 1.
    DATA lv_simple_type TYPE string.

    DESCRIBE FIELD iv_value TYPE lv_type.
    lv_simple_type = zcl_xtt_replace_block=>get_simple_type( lv_type ).
    IF lv_simple_type = zcl_xtt_replace_block=>mc_type-integer OR
       lv_simple_type = zcl_xtt_replace_block=>mc_type-double.
      rv_is_number = abap_true.
    ELSE.
      rv_is_number = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Literal node
" ---------------------------------------------------------------------
CLASS lcl_node_value IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_value  = iv_val.
    mv_is_num = iv_is_num.
  ENDMETHOD.

  METHOD eval.
    rv_val = mv_value.
  ENDMETHOD.

  METHOD is_numeric.
    rv_num = mv_is_num.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Variable node (ROW-*, VALUE-*, ROOT-*, SY-*, etc.)
" ---------------------------------------------------------------------
CLASS lcl_node_var IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_path = iv_path.
  ENDMETHOD.

  METHOD is_numeric.
    rv_num = mv_is_num.
  ENDMETHOD.

  METHOD eval.
    DATA lv_sy_fld      TYPE string.
    DATA lt_parts       TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_part        TYPE string.
    DATA lv_len         TYPE i.
    DATA lv_pos         TYPE i.
    DATA lv_in_brk      TYPE abap_bool.
    DATA lv_char_str    TYPE string.
    DATA lv_in_quote    TYPE char1.
    DATA lv_start       TYPE i.
    DATA lv_len_part    TYPE i.
    DATA lv_upper_path  TYPE string.
    DATA lv_upper_part  TYPE string.
    DATA lo_type        TYPE REF TO cl_abap_typedescr.
    DATA lv_tab_name    TYPE string.
    DATA lv_cond_str    TYPE string.
    DATA lv_idx         TYPE i.
    DATA lv_dummy       TYPE string.
    DATA lv_upper_tab   TYPE string.
    DATA lo_cond_expr   TYPE REF TO lcl_expression.
    DATA lv_found       TYPE abap_bool.
    DATA lv_fld_name    TYPE string.
    DATA lv_off_str     TYPE string.
    DATA lv_len_str     TYPE string.
    DATA lv_offset      TYPE i.
    DATA lv_length      TYPE i.
    DATA lv_has_off     TYPE abap_bool.
    DATA lv_upper_fld   TYPE string.
    DATA lv_raw_str     TYPE string.
    DATA lv_avail_len   TYPE i.

    FIELD-SYMBOLS <sy_val> TYPE any.
    FIELD-SYMBOLS <curr>   TYPE any.
    FIELD-SYMBOLS <next>   TYPE any.
    FIELD-SYMBOLS <deref>  TYPE any.
    FIELD-SYMBOLS <lt_tab> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_row> TYPE any.

    mv_is_num = abap_false.
    ASSIGN is_context TO <curr>.

    " 1. System variables (e.g. SY-DATUM, SY-DATUM+0(4), SY-TABIX)
    lv_upper_path = mv_path.
    TRANSLATE lv_upper_path TO UPPER CASE.
    IF lv_upper_path CP 'SY-*'.
      lv_sy_fld = lv_upper_path+3.
      lv_has_off = abap_false.

      IF lv_sy_fld CS '+' AND lv_sy_fld CS '(' AND lv_sy_fld CS ')'.
        SPLIT lv_sy_fld AT '+' INTO lv_sy_fld lv_off_str.
        SPLIT lv_off_str AT '(' INTO lv_off_str lv_len_str.
        SPLIT lv_len_str AT ')' INTO lv_len_str lv_dummy.
        SHIFT lv_sy_fld LEFT DELETING LEADING space.
        SHIFT lv_sy_fld RIGHT DELETING TRAILING space.
        SHIFT lv_off_str LEFT DELETING LEADING space.
        SHIFT lv_off_str RIGHT DELETING TRAILING space.
        SHIFT lv_len_str LEFT DELETING LEADING space.
        SHIFT lv_len_str RIGHT DELETING TRAILING space.
        IF lv_off_str CO '0123456789 ' AND lv_len_str CO '0123456789 '.
          lv_offset  = lv_off_str.
          lv_length  = lv_len_str.
          lv_has_off = abap_true.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT lv_sy_fld OF STRUCTURE sy TO <sy_val>.
      ASSERT sy-subrc = 0.

      IF lv_has_off = abap_true.
        lv_raw_str = |{ <sy_val> }|.
        IF strlen( lv_raw_str ) >= lv_offset + lv_length.
          rv_val = lv_raw_str+lv_offset(lv_length).
        ELSEIF strlen( lv_raw_str ) > lv_offset.
          lv_avail_len = strlen( lv_raw_str ) - lv_offset.
          rv_val = lv_raw_str+lv_offset(lv_avail_len).
        ELSE.
          CLEAR rv_val.
        ENDIF.
      ELSE.
        rv_val = <sy_val>.
      ENDIF.
      mv_is_num = _is_number( <sy_val> ).
      RETURN.
    ENDIF.

    " 2. Dynamic path split at '-' (ignoring '-' inside [...])
    lv_len      = strlen( mv_path ).
    lv_pos      = 0.
    lv_start    = 0.
    lv_in_brk   = abap_false.
    CLEAR lv_in_quote.
    CLEAR lt_parts.

    WHILE lv_pos < lv_len.
      lv_char_str = mv_path+lv_pos(1).

      IF ( lv_char_str = `'` OR lv_char_str = '`' ) AND lv_in_brk = abap_true.
        IF lv_in_quote IS INITIAL.
          lv_in_quote = lv_char_str.
        ELSEIF lv_in_quote = lv_char_str.
          CLEAR lv_in_quote.
        ENDIF.
      ELSEIF lv_char_str = '[' AND lv_in_quote IS INITIAL.
        lv_in_brk = abap_true.
      ELSEIF lv_char_str = ']' AND lv_in_quote IS INITIAL.
        lv_in_brk = abap_false.
      ENDIF.

      IF lv_char_str = '-' AND lv_in_brk = abap_false.
        lv_len_part = lv_pos - lv_start.
        APPEND mv_path+lv_start(lv_len_part) TO lt_parts.
        lv_start = lv_pos + 1.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    IF lv_start < lv_len.
      APPEND mv_path+lv_start TO lt_parts.
    ENDIF.

    " Dereference if context is a data reference
    lo_type = cl_abap_typedescr=>describe_by_data( <curr> ).
    IF lo_type->type_kind = cl_abap_typedescr=>typekind_dref.
      ASSIGN <curr>->* TO <deref>.
      IF sy-subrc = 0.
        ASSIGN <deref> TO <curr>.
      ENDIF.
    ENDIF.

    LOOP AT lt_parts INTO lv_part.
      lv_upper_part = lv_part.
      TRANSLATE lv_upper_part TO UPPER CASE.
      IF sy-tabix = 1 AND ( lv_upper_part = 'ROW' OR lv_upper_part = 'VALUE' OR lv_upper_part = 'ROOT' ).
        CONTINUE.
      ENDIF.

      " --- Table expression: [ index ] or [ condition ] ---
      IF lv_part CS '[' AND lv_part CS ']'.
        SPLIT lv_part AT '[' INTO lv_tab_name lv_cond_str.
        SPLIT lv_cond_str AT ']' INTO lv_cond_str lv_dummy.

        SHIFT lv_tab_name LEFT DELETING LEADING space.
        SHIFT lv_tab_name RIGHT DELETING TRAILING space.
        SHIFT lv_cond_str LEFT DELETING LEADING space.
        SHIFT lv_cond_str RIGHT DELETING TRAILING space.

        lv_upper_tab = lv_tab_name.
        TRANSLATE lv_upper_tab TO UPPER CASE.
        ASSIGN COMPONENT lv_upper_tab OF STRUCTURE <curr> TO <next>.
        IF sy-subrc <> 0.
          ASSIGN COMPONENT lv_tab_name OF STRUCTURE <curr> TO <next>.
        ENDIF.
        IF sy-subrc <> 0.
          zcx_xtt_exception=>raise_sys_error( iv_message = |Table '{ lv_tab_name }' in path '{ mv_path }' not found.| ).
        ENDIF.

        ASSIGN <next> TO <lt_tab>.
        IF sy-subrc <> 0.
          zcx_xtt_exception=>raise_sys_error( iv_message = |Field '{ lv_tab_name }' is not an internal table.| ).
        ENDIF.

        " Case A: Numeric index [ 1 ]
        IF lv_cond_str CO '0123456789 '.
          lv_idx = lv_cond_str.
          READ TABLE <lt_tab> INDEX lv_idx ASSIGNING <ls_row>.
          IF sy-subrc <> 0.
            zcx_xtt_exception=>raise_sys_error( iv_message = |Index { lv_idx } out of bounds for table '{ lv_tab_name }'.| ).
          ENDIF.
          ASSIGN <ls_row> TO <curr>.
        ELSE.
          " Case B: Key condition [ group = 'GRP A' ]
          lv_found = abap_false.
          CREATE OBJECT lo_cond_expr.
          lo_cond_expr->compile( lv_cond_str ).

          LOOP AT <lt_tab> ASSIGNING <ls_row>.
            IF lo_cond_expr->evaluate_bool( <ls_row> ) = abap_true.
              ASSIGN <ls_row> TO <curr>.
              lv_found = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF lv_found = abap_false.
            zcx_xtt_exception=>raise_sys_error(
              iv_message = |Line with condition '{ lv_cond_str }' not found in table '{ lv_tab_name }'.| ).
          ENDIF.
        ENDIF.

        CONTINUE.
      ENDIF.

      " --- Substring offset + length: FIELD+10(3) ---
      lv_has_off = abap_false.
      IF lv_part CS '+' AND lv_part CS '(' AND lv_part CS ')'.
        SPLIT lv_part AT '+' INTO lv_fld_name lv_off_str.
        SPLIT lv_off_str AT '(' INTO lv_off_str lv_len_str.
        SPLIT lv_len_str AT ')' INTO lv_len_str lv_dummy.
        SHIFT lv_fld_name LEFT DELETING LEADING space.
        SHIFT lv_fld_name RIGHT DELETING TRAILING space.
        SHIFT lv_off_str LEFT DELETING LEADING space.
        SHIFT lv_off_str RIGHT DELETING TRAILING space.
        SHIFT lv_len_str LEFT DELETING LEADING space.
        SHIFT lv_len_str RIGHT DELETING TRAILING space.
        IF lv_off_str CO '0123456789 ' AND lv_len_str CO '0123456789 '.
          lv_offset  = lv_off_str.
          lv_length  = lv_len_str.
          lv_has_off = abap_true.
        ENDIF.
      ELSE.
        lv_fld_name = lv_part.
      ENDIF.

      lv_upper_fld = lv_fld_name.
      TRANSLATE lv_upper_fld TO UPPER CASE.
      ASSIGN COMPONENT lv_upper_fld OF STRUCTURE <curr> TO <next>.
      IF sy-subrc <> 0.
        ASSIGN COMPONENT lv_fld_name OF STRUCTURE <curr> TO <next>.
      ENDIF.
      IF sy-subrc <> 0.
        zcx_xtt_exception=>raise_sys_error( iv_message = |Field '{ lv_fld_name }' in path '{ mv_path }' not found.| ).
      ENDIF.

      IF lv_has_off = abap_true.
        lv_raw_str = |{ <next> }|.
        IF strlen( lv_raw_str ) >= lv_offset + lv_length.
          rv_val = lv_raw_str+lv_offset(lv_length).
        ELSEIF strlen( lv_raw_str ) > lv_offset.
          lv_avail_len = strlen( lv_raw_str ) - lv_offset.
          rv_val = lv_raw_str+lv_offset(lv_avail_len).
        ELSE.
          CLEAR rv_val.
        ENDIF.
        ASSIGN rv_val TO <curr>.
        CONTINUE.
      ENDIF.

      ASSIGN <next> TO <curr>.
    ENDLOOP.

    rv_val = |{ <curr> }|.
    mv_is_num = _is_number( <curr> ).
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Arithmetic node
" ---------------------------------------------------------------------
CLASS lcl_node_arith IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_op    = iv_op.
    mo_left  = io_left.
    mo_right = io_right.
  ENDMETHOD.

  METHOD is_numeric.
    rv_num = abap_true.
  ENDMETHOD.

  METHOD eval.
    DATA lv_l   TYPE decfloat34.
    DATA lv_r   TYPE decfloat34.
    DATA lv_res TYPE decfloat34.

    lv_l = _to_number( mo_left->eval( is_context ) ).
    lv_r = _to_number( mo_right->eval( is_context ) ).

    CASE mv_op.
      WHEN '+'. lv_res = lv_l + lv_r.
      WHEN '-'. lv_res = lv_l - lv_r.
      WHEN '*'. lv_res = lv_l * lv_r.
      WHEN '/'.
        IF lv_r = 0.
          zcx_xtt_exception=>raise_sys_error( iv_message = 'Division by zero' ).
        ENDIF.
        lv_res = lv_l / lv_r.
    ENDCASE.

    rv_val = |{ lv_res }|.
    REPLACE FIRST OCCURRENCE OF `,` IN rv_val WITH `.`.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Binary comparison node
" ---------------------------------------------------------------------
CLASS lcl_node_compare IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_op    = iv_op.
    TRANSLATE mv_op TO UPPER CASE.
    mo_left  = io_left.
    mo_right = io_right.
  ENDMETHOD.

  METHOD eval.
    DATA lv_l      TYPE string.
    DATA lv_r      TYPE string.
    DATA lv_num_l  TYPE decfloat34.
    DATA lv_num_r  TYPE decfloat34.
    DATA lv_is_num TYPE abap_bool.

    lv_l = mo_left->eval( is_context ).
    lv_r = mo_right->eval( is_context ).

    IF mo_left->is_numeric( ) = abap_true OR mo_right->is_numeric( ) = abap_true.
      TRY.
          lv_num_l  = _to_number( lv_l ).
          lv_num_r  = _to_number( lv_r ).
          lv_is_num = abap_true.
        CATCH zcx_xtt_exception.
          lv_is_num = abap_false.
      ENDTRY.
    ENDIF.

    rv_val = abap_false.

    IF lv_is_num = abap_true.
      CASE mv_op.
        WHEN '=' OR 'EQ'.
          IF lv_num_l = lv_num_r. rv_val = abap_true. ENDIF.
        WHEN '<>' OR 'NE' OR '><'.
          IF lv_num_l <> lv_num_r. rv_val = abap_true. ENDIF.
        WHEN '<' OR 'LT'.
          IF lv_num_l < lv_num_r. rv_val = abap_true. ENDIF.
        WHEN '<=' OR 'LE'.
          IF lv_num_l <= lv_num_r. rv_val = abap_true. ENDIF.
        WHEN '>' OR 'GT'.
          IF lv_num_l > lv_num_r. rv_val = abap_true. ENDIF.
        WHEN '>=' OR 'GE'.
          IF lv_num_l >= lv_num_r. rv_val = abap_true. ENDIF.
        WHEN OTHERS.
          lv_is_num = abap_false.
      ENDCASE.
    ENDIF.

    IF lv_is_num = abap_false.
      CASE mv_op.
        WHEN '=' OR 'EQ'.
          IF lv_l = lv_r. rv_val = abap_true. ENDIF.
        WHEN '<>' OR 'NE' OR '><'.
          IF lv_l <> lv_r. rv_val = abap_true. ENDIF.
        WHEN '<' OR 'LT'.
          IF lv_l < lv_r. rv_val = abap_true. ENDIF.
        WHEN '<=' OR 'LE'.
          IF lv_l <= lv_r. rv_val = abap_true. ENDIF.
        WHEN '>' OR 'GT'.
          IF lv_l > lv_r. rv_val = abap_true. ENDIF.
        WHEN '>=' OR 'GE'.
          IF lv_l >= lv_r. rv_val = abap_true. ENDIF.
        WHEN 'CP'.
          IF lv_l CP lv_r. rv_val = abap_true. ENDIF.
        WHEN 'NP'.
          IF lv_l NP lv_r. rv_val = abap_true. ENDIF.
        WHEN 'CS'.
          IF lv_l CS lv_r. rv_val = abap_true. ENDIF.
        WHEN 'NS'.
          IF lv_l NS lv_r. rv_val = abap_true. ENDIF.
        WHEN 'CA'.
          IF lv_l CA lv_r. rv_val = abap_true. ENDIF.
        WHEN 'NA'.
          IF lv_l NA lv_r. rv_val = abap_true. ENDIF.
        WHEN 'CO'.
          IF lv_l CO lv_r. rv_val = abap_true. ENDIF.
        WHEN 'CN'.
          IF lv_l CN lv_r. rv_val = abap_true. ENDIF.
        WHEN OTHERS.
          zcx_xtt_exception=>raise_sys_error( iv_message = |Unsupported operator: { mv_op }| ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" IS [NOT] INITIAL node
" ---------------------------------------------------------------------
CLASS lcl_node_is_initial IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_child = io_child.
    mv_not   = iv_not.
  ENDMETHOD.

  METHOD eval.
    DATA lv_val TYPE string.
    lv_val = mo_child->eval( is_context ).

    IF lv_val IS INITIAL OR ( mo_child->is_numeric( ) = abap_true AND lv_val = '0' ).
      rv_val = abap_true.
    ELSE.
      rv_val = abap_false.
    ENDIF.

    IF mv_not = abap_true.
      IF rv_val = abap_true.
        rv_val = abap_false.
      ELSE.
        rv_val = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Logical operator node (AND, OR)
" ---------------------------------------------------------------------
CLASS lcl_node_logical IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_op    = iv_op.
    TRANSLATE mv_op TO UPPER CASE.
    mo_left  = io_left.
    mo_right = io_right.
  ENDMETHOD.

  METHOD eval.
    DATA lv_l TYPE abap_bool.
    lv_l = mo_left->eval( is_context ).

    IF mv_op = 'AND'.
      IF lv_l = abap_false.
        rv_val = abap_false.
        RETURN.
      ENDIF.
      rv_val = mo_right->eval( is_context ).
    ELSEIF mv_op = 'OR'.
      IF lv_l = abap_true.
        rv_val = abap_true.
        RETURN.
      ENDIF.
      rv_val = mo_right->eval( is_context ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Logical NOT node
" ---------------------------------------------------------------------
CLASS lcl_node_not IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_child = io_child.
  ENDMETHOD.

  METHOD eval.
    IF mo_child->eval( is_context ) = abap_true.
      rv_val = abap_false.
    ELSE.
      rv_val = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Truthy test node
" ---------------------------------------------------------------------
CLASS lcl_node_truthy IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_child = io_child.
  ENDMETHOD.

  METHOD eval.
    DATA lv_s TYPE string.
    lv_s = mo_child->eval( is_context ).
    IF lv_s = abap_true OR ( lv_s IS NOT INITIAL AND lv_s <> '0' ).
      rv_val = abap_true.
    ELSE.
      rv_val = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Template Node: handles "text { expr1 } text { expr2 }"
" ---------------------------------------------------------------------
CLASS lcl_node_template IMPLEMENTATION.
  METHOD add_child.
    APPEND io_child TO mt_children.
  ENDMETHOD.

  METHOD eval.
    DATA lo_child TYPE REF TO lcl_ast_node.
    CLEAR rv_val.
    LOOP AT mt_children INTO lo_child.
      rv_val = |{ rv_val }{ lo_child->eval( is_context ) }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Conditional branch node
" ---------------------------------------------------------------------
CLASS lcl_node_cond IMPLEMENTATION.
  METHOD eval.
    DATA ls_branch LIKE LINE OF mt_branches.
    LOOP AT mt_branches INTO ls_branch.
      IF ls_branch-cond->eval( is_context ) = abap_true.
        rv_val = ls_branch-val->eval( is_context ).
        RETURN.
      ENDIF.
    ENDLOOP.

    IF mo_else IS BOUND.
      rv_val = mo_else->eval( is_context ).
    ELSE.
      CLEAR rv_val.
    ENDIF.
  ENDMETHOD.

  METHOD is_numeric.
    DATA ls_b LIKE LINE OF mt_branches.
    READ TABLE mt_branches INTO ls_b INDEX 1.
    IF sy-subrc = 0.
      rv_num = ls_b-val->is_numeric( ).
    ELSEIF mo_else IS BOUND.
      rv_num = mo_else->is_numeric( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_node_switch IMPLEMENTATION.
  METHOD eval.
    DATA lv_switch_val TYPE string.
    DATA lv_branch_val TYPE string.
    DATA ls_branch     LIKE LINE OF mt_branches.
    DATA lv_num_s      TYPE decfloat34.
    DATA lv_num_b      TYPE decfloat34.
    DATA lv_is_num     TYPE abap_bool.

    lv_switch_val = mo_switch_expr->eval( is_context ).

    LOOP AT mt_branches INTO ls_branch.
      lv_branch_val = ls_branch-val_from->eval( is_context ).
      lv_is_num     = abap_false.

      IF mo_switch_expr->is_numeric( ) = abap_true OR ls_branch-val_from->is_numeric( ) = abap_true.
        TRY.
            lv_num_s  = _to_number( lv_switch_val ).
            lv_num_b  = _to_number( lv_branch_val ).
            lv_is_num = abap_true.
          CATCH zcx_xtt_exception.
            lv_is_num = abap_false.
        ENDTRY.
      ENDIF.

      IF ( lv_is_num = abap_true AND lv_num_s = lv_num_b ) OR
         ( lv_is_num = abap_false AND lv_switch_val = lv_branch_val ).
        rv_val = ls_branch-val_to->eval( is_context ).
        RETURN.
      ENDIF.
    ENDLOOP.

    IF mo_else IS BOUND.
      rv_val = mo_else->eval( is_context ).
    ELSE.
      CLEAR rv_val.
    ENDIF.
  ENDMETHOD.

  METHOD is_numeric.
    DATA ls_b LIKE LINE OF mt_branches.
    READ TABLE mt_branches INTO ls_b INDEX 1.
    IF sy-subrc = 0.
      rv_num = ls_b-val_to->is_numeric( ).
    ELSEIF mo_else IS BOUND.
      rv_num = mo_else->is_numeric( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_node_func IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_func_name = iv_name.
    mo_arg       = io_arg.
  ENDMETHOD.

  METHOD eval.
    rv_val = mo_arg->eval( is_context ).
    CASE mv_func_name.
      WHEN 'TO_LOWER'.
        TRANSLATE rv_val TO LOWER CASE.
      WHEN 'TO_UPPER'.
        TRANSLATE rv_val TO UPPER CASE.
      WHEN 'TO_MIXED'.
        DATA lt_parts TYPE STANDARD TABLE OF string.
        DATA lv_part  TYPE string.
        DATA lv_char  TYPE c LENGTH 1.

        SPLIT rv_val AT '_' INTO TABLE lt_parts.
        CLEAR rv_val.
        LOOP AT lt_parts INTO lv_part.
          CHECK lv_part IS NOT INITIAL.
          TRANSLATE lv_part TO LOWER CASE.
          lv_char = lv_part(1).
          TRANSLATE lv_char TO UPPER CASE.
          SHIFT lv_part BY 1 PLACES.
          CONCATENATE rv_val lv_char lv_part INTO rv_val.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

  METHOD is_numeric.
    rv_num = mo_arg->is_numeric( ).
  ENDMETHOD.
ENDCLASS.
" ====================================================================
" Tokenizer Implementation
" ====================================================================
CLASS lcl_tokenizer IMPLEMENTATION.
  METHOD tokenize.
    DATA lv_len      TYPE i.
    DATA lv_pos      TYPE i.
    DATA lv_char     TYPE string.
    DATA ls_token    TYPE ts_token.
    DATA lv_str      TYPE string.
    DATA lv_ident    TYPE string.
    DATA lv_quote    TYPE string.
    DATA c           TYPE string.
    DATA lv_next_pos TYPE i.
    DATA lv_two      TYPE string.
    DATA lv_kw       TYPE string.
    DATA lv_next     TYPE i.
    DATA lv_abcde    TYPE c LENGTH 26.

    lv_len = strlen( iv_text ).
    lv_pos = 0.
    lv_abcde = sy-abcde.
    TRANSLATE lv_abcde TO LOWER CASE.

    WHILE lv_pos < lv_len.
      lv_char = iv_text+lv_pos(1).

      IF lv_char = '#'.
        CLEAR ls_token.
        ls_token-type  = 'HASH'.
        ls_token-value = '#'.
        APPEND ls_token TO rt_tokens.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      " 1. Skip whitespaces
      IF lv_char = ` ` OR lv_char = cl_abap_char_utilities=>horizontal_tab OR
         lv_char = cl_abap_char_utilities=>newline OR lv_char = cl_abap_char_utilities=>cr_lf.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      " 2. Parentheses
      IF lv_char = '('.
        CLEAR ls_token. ls_token-type = 'LPAREN'. ls_token-value = '('.
        APPEND ls_token TO rt_tokens. lv_pos = lv_pos + 1. CONTINUE.
      ENDIF.
      IF lv_char = ')'.
        CLEAR ls_token. ls_token-type = 'RPAREN'. ls_token-value = ')'.
        APPEND ls_token TO rt_tokens. lv_pos = lv_pos + 1. CONTINUE.
      ENDIF.

      " 3. Quoted strings ('...' or `...`)
      IF lv_char = `'` OR lv_char = '`'.
        lv_quote = lv_char.
        lv_pos   = lv_pos + 1.
        CLEAR lv_str.
        WHILE lv_pos < lv_len.
          c = iv_text+lv_pos(1).
          IF c = lv_quote.
            lv_next_pos = lv_pos + 1.
            IF lv_next_pos < lv_len AND iv_text+lv_next_pos(1) = lv_quote.
              lv_str = |{ lv_str }{ lv_quote }|.
              lv_pos = lv_pos + 2.
              CONTINUE.
            ELSE.
              lv_pos = lv_pos + 1.
              EXIT.
            ENDIF.
          ELSE.
            lv_str = |{ lv_str }{ c }|.
            lv_pos = lv_pos + 1.
          ENDIF.
        ENDWHILE.
        CLEAR ls_token.
        ls_token-type  = 'STR'.
        ls_token-value = lv_str.
        APPEND ls_token TO rt_tokens.
        CONTINUE.
      ENDIF.

      " 4. Comparison operator symbols: <>, ><, <=, >=, =, <, >
      IF lv_pos + 1 < lv_len.
        lv_two = iv_text+lv_pos(2).
        IF lv_two = '<>' OR lv_two = '><' OR lv_two = '<=' OR lv_two = '>='.
          CLEAR ls_token.
          ls_token-type  = 'COMP'.
          ls_token-value = lv_two.
          APPEND ls_token TO rt_tokens.
          lv_pos = lv_pos + 2.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF lv_char = '=' OR lv_char = '<' OR lv_char = '>'.
        CLEAR ls_token.
        ls_token-type  = 'COMP'.
        ls_token-value = lv_char.
        APPEND ls_token TO rt_tokens.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      " 5. Arithmetic operators (+, *, /)
      IF lv_char = '+' OR lv_char = '*' OR lv_char = '/'.
        CLEAR ls_token.
        ls_token-type  = 'ARITH'.
        ls_token-value = lv_char.
        APPEND ls_token TO rt_tokens.
        lv_pos = lv_pos + 1.
        CONTINUE.
      ENDIF.

      " 6. Standalone minus
      IF lv_char = '-'.
        lv_next_pos = lv_pos + 1.
        IF lv_next_pos >= lv_len OR iv_text+lv_next_pos(1) = ` ` OR iv_text+lv_next_pos(1) CA '0123456789'.
          CLEAR ls_token.
          ls_token-type  = 'ARITH'.
          ls_token-value = '-'.
          APPEND ls_token TO rt_tokens.
          lv_pos = lv_pos + 1.
          CONTINUE.
        ENDIF.
      ENDIF.

      " 7. Number literals
      IF lv_char CA '0123456789'.
        CLEAR lv_str.
        WHILE lv_pos < lv_len AND iv_text+lv_pos(1) CA '0123456789.'.
          lv_str = |{ lv_str }{ iv_text+lv_pos(1) }|.
          lv_pos = lv_pos + 1.
        ENDWHILE.
        CLEAR ls_token.
        ls_token-type  = 'NUM'.
        ls_token-value = lv_str.
        APPEND ls_token TO rt_tokens.
        CONTINUE.
      ENDIF.

      " 8. Words, Keywords, and Identifiers
      IF lv_char CA sy-abcde OR lv_char CA lv_abcde OR lv_char = '_'.
        CLEAR lv_ident.
        WHILE lv_pos < lv_len.
          c = iv_text+lv_pos(1).

          " Array bracket
          IF c = '['.
            WHILE lv_pos < lv_len.
              c = iv_text+lv_pos(1).
              lv_ident = |{ lv_ident }{ c }|.
              lv_pos   = lv_pos + 1.
              IF c = ']'.
                EXIT.
              ENDIF.
            ENDWHILE.
            CONTINUE.
          ENDIF.

          " Substring offset+length: +10(3)
          IF c = '+'.
            DATA lv_rem         TYPE string.
            DATA lv_close_paren TYPE i.
            DATA lv_sub_spec    TYPE string.
            lv_rem = iv_text+lv_pos.
            lv_close_paren = find( val = lv_rem sub = ')' ).
            IF lv_close_paren > 1.
              lv_sub_spec = substring( val = lv_rem off = 1 len = lv_close_paren ).
              IF lv_sub_spec CA '(' AND lv_sub_spec CA ')'.
                lv_next = lv_close_paren + 1.
                lv_ident = |{ lv_ident }{ iv_text+lv_pos(lv_next) }|.
                lv_pos = lv_pos + lv_close_paren + 1.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF c = '-'.
            lv_next_pos = lv_pos + 1.
            IF lv_next_pos < lv_len AND ( iv_text+lv_next_pos(1) CA sy-abcde OR
                                          iv_text+lv_next_pos(1) CA lv_abcde OR
                                          iv_text+lv_next_pos(1) = '_' ).
              lv_ident = |{ lv_ident }{ c }|.
              lv_pos   = lv_pos + 1.
              CONTINUE.
            ELSE.
              EXIT.
            ENDIF.
          ENDIF.

          IF c CA sy-abcde OR c CA lv_abcde OR c CA '0123456789_'.
            lv_ident = |{ lv_ident }{ c }|.
            lv_pos   = lv_pos + 1.
          ELSE.
            EXIT.
          ENDIF.
        ENDWHILE.

        lv_kw = lv_ident.
        TRANSLATE lv_kw TO UPPER CASE.

        CLEAR ls_token.
        CASE lv_kw.
          WHEN 'AND'.
            ls_token-type = 'AND'.
          WHEN 'OR'.
            ls_token-type = 'OR'.
          WHEN 'NOT'.
            ls_token-type = 'NOT'.
          WHEN 'EQ' OR 'NE' OR 'LT' OR 'LE' OR 'GT' OR 'GE' OR
               'CP' OR 'NP' OR 'CS' OR 'NS' OR 'CA' OR 'NA' OR 'CO' OR 'CN'.
            ls_token-type  = 'COMP'.
            ls_token-value = lv_kw.
          WHEN 'THEN'.
            ls_token-type = 'THEN'.
          WHEN 'ELSE'.
            ls_token-type = 'ELSE'.
          WHEN 'COND'.
            ls_token-type = 'COND'.
          WHEN 'WHEN'.
            ls_token-type = 'WHEN'.
          WHEN 'SWITCH'.
            ls_token-type = 'SWITCH'.
          WHEN 'TO_LOWER' OR 'TO_UPPER' OR 'TO_MIXED'.
            ls_token-type  = 'FUNC'.
            ls_token-value = lv_kw.
          WHEN 'IS'.
            ls_token-type = 'IS'.
          WHEN 'INITIAL'.
            ls_token-type = 'INITIAL'.
          WHEN 'SPACE'.
            ls_token-type  = 'STR'.
            ls_token-value = ''.
          WHEN 'ABAP_TRUE'.
            ls_token-type  = 'STR'.
            ls_token-value = abap_true.
          WHEN 'ABAP_FALSE'.
            ls_token-type  = 'STR'.
            ls_token-value = abap_false.
          WHEN OTHERS.
            ls_token-type  = 'IDENT'.
            ls_token-value = lv_ident.
        ENDCASE.
        APPEND ls_token TO rt_tokens.
        CONTINUE.
      ENDIF.

      " String template |...|
      IF lv_char = '|'.
        DATA lv_tmpl_str  TYPE string.
        DATA lv_in_braces TYPE i.
        CLEAR: lv_tmpl_str, lv_in_braces.
        lv_pos = lv_pos + 1.
        WHILE lv_pos < lv_len.
          c = iv_text+lv_pos(1).
          IF c = '\' AND lv_pos + 1 < lv_len.
            lv_tmpl_str = |{ lv_tmpl_str }{ iv_text+lv_pos(2) }|.
            lv_pos = lv_pos + 2.
            CONTINUE.
          ENDIF.
          IF c = '{'.
            lv_in_braces = lv_in_braces + 1.
          ELSEIF c = '}'.
            lv_in_braces = lv_in_braces - 1.
          ELSEIF c = '|' AND lv_in_braces = 0.
            lv_pos = lv_pos + 1.
            EXIT.
          ENDIF.
          lv_tmpl_str = |{ lv_tmpl_str }{ c }|.
          lv_pos = lv_pos + 1.
        ENDWHILE.
        CLEAR ls_token.
        ls_token-type  = 'TMPL'.
        ls_token-value = lv_tmpl_str.
        APPEND ls_token TO rt_tokens.
        CONTINUE.
      ENDIF.

      zcx_xtt_exception=>raise_sys_error( iv_message = |Unexpected character: { lv_char }| ).
    ENDWHILE.

    CLEAR ls_token.
    ls_token-type  = 'EOF'.
    ls_token-value = ''.
    APPEND ls_token TO rt_tokens.
  ENDMETHOD.
ENDCLASS.

" ====================================================================
" Parser Implementation
" ====================================================================
CLASS lcl_parser IMPLEMENTATION.
  METHOD constructor.
    mt_tokens = it_tokens.
    mv_idx    = 1.
  ENDMETHOD.

  METHOD current.
    IF mv_idx <= lines( mt_tokens ).
      READ TABLE mt_tokens INDEX mv_idx INTO rs_tok.
    ELSE.
      CLEAR rs_tok.
      rs_tok-type = 'EOF'.
    ENDIF.
  ENDMETHOD.

  METHOD consume.
    IF iv_expected IS NOT INITIAL AND current( )-type <> iv_expected.
      zcx_xtt_exception=>raise_sys_error( iv_message = |Expected token '{ iv_expected }' but found '{ current( )-value }'| ).
    ENDIF.
    mv_idx = mv_idx + 1.
  ENDMETHOD.

  METHOD parse.
    ro_root = parse_cond( ).
    IF current( )-type <> 'EOF'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |Unexpected token at end: { current( )-value }| ).
    ENDIF.
  ENDMETHOD.

  METHOD parse_or.
    DATA lo_right TYPE REF TO lcl_ast_node.
    DATA lo_log   TYPE REF TO lcl_node_logical.

    ro_node = parse_and( ).
    WHILE current( )-type = 'OR'.
      consume( 'OR' ).
      lo_right = parse_and( ).
      CREATE OBJECT lo_log
        EXPORTING
          iv_op    = 'OR'
          io_left  = ro_node
          io_right = lo_right.
      ro_node = lo_log.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_and.
    DATA lo_right TYPE REF TO lcl_ast_node.
    DATA lo_log   TYPE REF TO lcl_node_logical.

    ro_node = parse_not( ).
    WHILE current( )-type = 'AND'.
      consume( 'AND' ).
      lo_right = parse_not( ).
      CREATE OBJECT lo_log
        EXPORTING
          iv_op    = 'AND'
          io_left  = ro_node
          io_right = lo_right.
      ro_node = lo_log.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_cond.
    DATA lo_cond_node TYPE REF TO lcl_node_cond.
    DATA lo_cond      TYPE REF TO lcl_ast_node.
    DATA lo_val       TYPE REF TO lcl_ast_node.
    DATA ls_branch    LIKE LINE OF lo_cond_node->mt_branches.

    " Syntax: SWITCH #( expr WHEN val THEN res ... ELSE default )
    IF current( )-type = 'SWITCH'.
      consume( 'SWITCH' ).
      IF current( )-type = 'HASH' OR current( )-type = 'IDENT'.
        consume( ).
      ENDIF.
      consume( 'LPAREN' ).
      ro_node = parse_switch( ).
      consume( 'RPAREN' ).
      RETURN.
    ENDIF.

    " Syntax COND #( ... ) or COND type( ... )
    IF current( )-type = 'COND'.
      consume( 'COND' ).
      IF current( )-type = 'HASH' OR current( )-type = 'IDENT'.
        consume( ).
      ENDIF.
      consume( 'LPAREN' ).
      ro_node = parse_cond( ).
      consume( 'RPAREN' ).
      RETURN.
    ENDIF.

    " Syntax: WHEN cond THEN val [WHEN cond THEN val ...] [ELSE else_val]
    IF current( )-type = 'WHEN'.
      CREATE OBJECT lo_cond_node.
      WHILE current( )-type = 'WHEN'.
        consume( 'WHEN' ).
        lo_cond = parse_or( ).
        consume( 'THEN' ).
        lo_val = parse_or( ).
        CLEAR ls_branch.
        ls_branch-cond = lo_cond.
        ls_branch-val  = lo_val.
        APPEND ls_branch TO lo_cond_node->mt_branches.
      ENDWHILE.
      IF current( )-type = 'ELSE'.
        consume( 'ELSE' ).
        lo_cond_node->mo_else = parse_cond( ).
      ENDIF.
      ro_node = lo_cond_node.
      RETURN.
    ENDIF.

    " Syntax: cond THEN val_true ELSE val_false
    ro_node = parse_or( ).
    IF current( )-type = 'THEN'.
      CREATE OBJECT lo_cond_node.
      lo_cond = ro_node.
      consume( 'THEN' ).
      lo_val = parse_or( ).
      CLEAR ls_branch.
      ls_branch-cond = lo_cond.
      ls_branch-val  = lo_val.
      APPEND ls_branch TO lo_cond_node->mt_branches.

      IF current( )-type = 'ELSE'.
        consume( 'ELSE' ).
        lo_cond_node->mo_else = parse_cond( ).
      ENDIF.
      ro_node = lo_cond_node.
    ENDIF.
  ENDMETHOD.

  METHOD parse_switch.
    DATA lo_switch TYPE REF TO lcl_node_switch.
    DATA ls_branch LIKE LINE OF lo_switch->mt_branches.

    CREATE OBJECT lo_switch.
    lo_switch->mo_switch_expr = parse_or( ).

    WHILE current( )-type = 'WHEN'.
      consume( 'WHEN' ).
      CLEAR ls_branch.
      ls_branch-val_from = parse_or( ).
      consume( 'THEN' ).
      ls_branch-val_to   = parse_or( ).
      APPEND ls_branch TO lo_switch->mt_branches.
    ENDWHILE.

    IF current( )-type = 'ELSE'.
      consume( 'ELSE' ).
      lo_switch->mo_else = parse_cond( ).
    ENDIF.

    ro_node = lo_switch.
  ENDMETHOD.

  METHOD parse_not.
    DATA lo_child TYPE REF TO lcl_ast_node.

    IF current( )-type = 'NOT'.
      consume( 'NOT' ).
      lo_child = parse_not( ).
      CREATE OBJECT ro_node TYPE lcl_node_not
        EXPORTING
          io_child = lo_child.
      RETURN.
    ENDIF.

    ro_node = parse_predicate( ).
  ENDMETHOD.

  METHOD _is_cond_paren.
    DATA lv_depth TYPE i.
    DATA lv_i     TYPE i.
    DATA ls_tok   TYPE lcl_tokenizer=>ts_token.

    rv_is_cond = abap_false.
    lv_depth   = 0.
    lv_i       = mv_idx.

    WHILE lv_i <= lines( mt_tokens ).
      READ TABLE mt_tokens INDEX lv_i INTO ls_tok.
      IF ls_tok-type = 'LPAREN'.
        lv_depth = lv_depth + 1.
      ELSEIF ls_tok-type = 'RPAREN'.
        lv_depth = lv_depth - 1.
        IF lv_depth = 0.
          EXIT.
        ENDIF.
      ELSEIF lv_depth >= 1.
        IF ls_tok-type = 'COMP' OR ls_tok-type = 'OR' OR ls_tok-type = 'AND' OR
           ls_tok-type = 'NOT'  OR ls_tok-type = 'IS'.
          rv_is_cond = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_predicate.
    DATA lo_left  TYPE REF TO lcl_ast_node.
    DATA lv_not   TYPE abap_bool.
    DATA lv_op    TYPE string.
    DATA lo_right TYPE REF TO lcl_ast_node.

    IF current( )-type = 'LPAREN' AND _is_cond_paren( ) = abap_true.
      consume( 'LPAREN' ).
      ro_node = parse_or( ).
      consume( 'RPAREN' ).
      RETURN.
    ENDIF.

    lo_left = parse_operand( ).

    " IS [NOT] INITIAL
    IF current( )-type = 'IS'.
      consume( 'IS' ).
      lv_not = abap_false.
      IF current( )-type = 'NOT'.
        consume( 'NOT' ).
        lv_not = abap_true.
      ENDIF.
      consume( 'INITIAL' ).
      CREATE OBJECT ro_node TYPE lcl_node_is_initial
        EXPORTING
          io_child = lo_left
          iv_not   = lv_not.
      RETURN.
    ENDIF.

    " Binary comparison: = , <>, CP, etc.
    IF current( )-type = 'COMP'.
      lv_op = current( )-value.
      consume( 'COMP' ).
      lo_right = parse_operand( ).
      CREATE OBJECT ro_node TYPE lcl_node_compare
        EXPORTING
          iv_op    = lv_op
          io_left  = lo_left
          io_right = lo_right.
      RETURN.
    ENDIF.

    ro_node = lo_left.
  ENDMETHOD.

  METHOD parse_operand.
    DATA lv_op    TYPE string.
    DATA lo_right TYPE REF TO lcl_ast_node.
    DATA lo_arith TYPE REF TO lcl_node_arith.

    ro_node = parse_arith_term( ).
    WHILE current( )-type = 'ARITH' AND ( current( )-value = '+' OR current( )-value = '-' ).
      lv_op = current( )-value.
      consume( 'ARITH' ).
      lo_right = parse_arith_term( ).
      CREATE OBJECT lo_arith
        EXPORTING
          iv_op    = lv_op
          io_left  = ro_node
          io_right = lo_right.
      ro_node = lo_arith.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_arith_term.
    DATA lv_op    TYPE string.
    DATA lo_right TYPE REF TO lcl_ast_node.
    DATA lo_arith TYPE REF TO lcl_node_arith.

    ro_node = parse_arith_factor( ).
    WHILE current( )-type = 'ARITH' AND ( current( )-value = '*' OR current( )-value = '/' ).
      lv_op = current( )-value.
      consume( 'ARITH' ).
      lo_right = parse_arith_factor( ).
      CREATE OBJECT lo_arith
        EXPORTING
          iv_op    = lv_op
          io_left  = ro_node
          io_right = lo_right.
      ro_node = lo_arith.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_arith_factor.
    DATA ls_tok       TYPE lcl_tokenizer=>ts_token.
    DATA lo_sub       TYPE REF TO lcl_ast_node.
    DATA lo_minus_one TYPE REF TO lcl_node_value.
    DATA lo_val       TYPE REF TO lcl_node_value.
    DATA lo_var       TYPE REF TO lcl_node_var.

    ls_tok = current( ).

    " Allow COND #( ... ) as an operand
    IF ls_tok-type = 'COND'.
      ro_node = parse_cond( ).
      RETURN.
    ENDIF.

    " Unary +/-
    IF ls_tok-type = 'ARITH' AND ( ls_tok-value = '-' OR ls_tok-value = '+' ).
      consume( 'ARITH' ).
      lo_sub = parse_arith_factor( ).
      IF ls_tok-value = '-'.
        CREATE OBJECT lo_minus_one
          EXPORTING
            iv_val    = '-1'
            iv_is_num = abap_true.
        CREATE OBJECT ro_node TYPE lcl_node_arith
          EXPORTING
            iv_op    = '*'
            io_left  = lo_minus_one
            io_right = lo_sub.
      ELSE.
        ro_node = lo_sub.
      ENDIF.
      RETURN.
    ENDIF.

    CASE ls_tok-type.
      WHEN 'NUM'.
        consume( 'NUM' ).
        CREATE OBJECT lo_val
          EXPORTING
            iv_val    = ls_tok-value
            iv_is_num = abap_true.
        ro_node = lo_val.
      WHEN 'STR'.
        consume( 'STR' ).
        CREATE OBJECT lo_val
          EXPORTING
            iv_val    = ls_tok-value
            iv_is_num = abap_false.
        ro_node = lo_val.
      WHEN 'IDENT'.
        consume( 'IDENT' ).
        CREATE OBJECT lo_var
          EXPORTING
            iv_path = ls_tok-value.
        ro_node = lo_var.
      WHEN 'LPAREN'.
        consume( 'LPAREN' ).
        ro_node = parse_operand( ).
        consume( 'RPAREN' ).

      WHEN 'SWITCH'.
        ro_node = parse_cond( ).

        " String template |...|
      WHEN 'TMPL'.
        consume( 'TMPL' ).
        ro_node = lcl_expression=>_compile_template( ls_tok-value ).

        " Functions: to_lower( ... ), to_upper( ... ), to_mixed( ... )
      WHEN 'FUNC'.
        DATA lv_fname TYPE string.
        DATA lo_arg   TYPE REF TO lcl_ast_node.
        DATA lo_func  TYPE REF TO lcl_node_func.

        lv_fname = ls_tok-value.
        consume( 'FUNC' ).
        consume( 'LPAREN' ).
        lo_arg = parse_cond( ).
        consume( 'RPAREN' ).

        CREATE OBJECT lo_func
          EXPORTING
            iv_name = lv_fname
            io_arg  = lo_arg.
        ro_node = lo_func.

      WHEN OTHERS.
        zcx_xtt_exception=>raise_sys_error( iv_message = |Unexpected operand: { ls_tok-value }| ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

" ====================================================================
" Facade Implementation
" ====================================================================
CLASS lcl_expression IMPLEMENTATION.
  METHOD compile.
    DATA lv_expr   TYPE string.
    DATA lv_len    TYPE i.
    DATA lv_len_m1 TYPE i.
    DATA lv_len_m2 TYPE i.

    lv_expr = iv_expr.
    SHIFT lv_expr LEFT DELETING LEADING space.
    SHIFT lv_expr RIGHT DELETING TRAILING space.

    " Strip outer pipes |...| only if the entire expression is enclosed in pipes
    lv_len = strlen( lv_expr ).
    IF lv_len >= 2 AND lv_expr(1) = '|'.
      lv_len_m1 = lv_len - 1.
      IF lv_expr+lv_len_m1(1) = '|'.
        lv_len_m2 = lv_len - 2.
        lv_expr = lv_expr+1(lv_len_m2).
        mo_ast = _compile_template( lv_expr ).
        RETURN.
      ENDIF.
    ENDIF.

    " Legacy unquoted templates without pipes (e.g. ABC { sy-datum })
    IF lv_expr CS '{' AND lv_expr CS '}' AND lv_expr NS '|'.  " <--- ADDED: AND lv_expr NS '|'
      mo_ast = _compile_template( lv_expr ).
      RETURN.
    ENDIF.

    " Arithmetic / functions / conditional expressions
    mo_ast = _compile_sub_expr( lv_expr ).
  ENDMETHOD.

  METHOD _compile_sub_expr.
    DATA lv_sub    TYPE string.
    DATA lt_tokens TYPE lcl_tokenizer=>tt_token.
    DATA lo_parser TYPE REF TO lcl_parser.

    lv_sub = iv_sub_expr.
    SHIFT lv_sub LEFT DELETING LEADING space.
    SHIFT lv_sub RIGHT DELETING TRAILING space.

    lt_tokens = lcl_tokenizer=>tokenize( lv_sub ).
    CREATE OBJECT lo_parser
      EXPORTING
        it_tokens = lt_tokens.
    ro_node = lo_parser->parse( ).
  ENDMETHOD.

  METHOD _compile_template.
    DATA lo_tmpl        TYPE REF TO lcl_node_template.
    DATA lv_len         TYPE i.
    DATA lv_pos         TYPE i.
    DATA lv_chunk       TYPE string.
    DATA lv_char        TYPE string.
    DATA lv_open_pos    TYPE i.
    DATA lv_close_pos   TYPE i.
    DATA lv_sub_expr    TYPE string.
    DATA lv_sub_len     TYPE i.
    DATA lo_sub_node    TYPE REF TO lcl_ast_node.
    DATA lo_node_val    TYPE REF TO lcl_node_value.

    CREATE OBJECT lo_tmpl.
    lv_len = strlen( iv_template ).
    lv_pos = 0.
    CLEAR lv_chunk.

    WHILE lv_pos < lv_len.
      lv_char = iv_template+lv_pos(1).

      IF lv_char = '{'.
        " Flush preceding static text
        IF lv_chunk IS NOT INITIAL.
          CREATE OBJECT lo_node_val EXPORTING iv_val = lv_chunk.
          lo_tmpl->add_child( lo_node_val ).
          CLEAR lv_chunk.
        ENDIF.

        lv_open_pos  = lv_pos + 1.
        lv_close_pos = find( val = iv_template sub = '}' off = lv_open_pos ).
        IF lv_close_pos < 0.
          zcx_xtt_exception=>raise_sys_error( iv_message = 'Unclosed "{" in string template' ).
        ENDIF.

        lv_sub_len  = lv_close_pos - lv_open_pos.
        lv_sub_expr = substring( val = iv_template off = lv_open_pos len = lv_sub_len ).

        lo_sub_node = _compile_sub_expr( lv_sub_expr ).
        lo_tmpl->add_child( lo_sub_node ).

        lv_pos = lv_close_pos + 1.
      ELSE.
        lv_chunk = |{ lv_chunk }{ lv_char }|.
        lv_pos   = lv_pos + 1.
      ENDIF.
    ENDWHILE.

    " Flush trailing static text
    IF lv_chunk IS NOT INITIAL.
      CREATE OBJECT lo_node_val EXPORTING iv_val = lv_chunk.
      lo_tmpl->add_child( lo_node_val ).
    ENDIF.

    ro_node = lo_tmpl.
  ENDMETHOD.

  METHOD evaluate.
    IF mo_ast IS NOT BOUND.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'Expression not compiled.' ).
    ENDIF.
    rv_result = mo_ast->eval( is_context ).
  ENDMETHOD.

  METHOD evaluate_bool.
    IF mo_ast IS NOT BOUND.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'Expression not compiled.' ).
    ENDIF.
    IF mo_ast->eval( is_context ) = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

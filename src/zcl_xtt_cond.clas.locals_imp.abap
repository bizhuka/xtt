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

  METHOD is_variable.
    rv_variable = abap_false.
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

  METHOD is_variable.
    rv_variable = abap_true.
  ENDMETHOD.

  METHOD eval.
    DATA lr_value TYPE REF TO data.
    DATA lo_type TYPE REF TO cl_abap_typedescr.
    FIELD-SYMBOLS <value> TYPE any.
    lr_value = resolve( is_context ).
    ASSIGN lr_value->* TO <value>.
    rv_val = |{ <value> }|.
    mv_is_num = _is_number( <value> ).
    lo_type = cl_abap_typedescr=>describe_by_data( <value> ).
    mv_kind = lo_type->type_kind.
  ENDMETHOD.

  METHOD resolve.
    DATA rv_val TYPE string.
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
    CLEAR mv_kind.
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
      ELSEIF lv_sy_fld CS '(' AND lv_sy_fld CS ')'.
        SPLIT lv_sy_fld AT '(' INTO lv_sy_fld lv_len_str.
        SPLIT lv_len_str AT ')' INTO lv_len_str lv_dummy.
        SHIFT lv_sy_fld LEFT DELETING LEADING space.
        SHIFT lv_sy_fld RIGHT DELETING TRAILING space.
        SHIFT lv_len_str LEFT DELETING LEADING space.
        SHIFT lv_len_str RIGHT DELETING TRAILING space.
        IF lv_len_str CO '0123456789 '.
          lv_offset  = 0.
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
      IF lv_has_off = abap_true.
        GET REFERENCE OF rv_val INTO rr_value.
      ELSE.
        GET REFERENCE OF <sy_val> INTO rr_value.
      ENDIF.
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
            IF iv_optional = abap_true.
              RETURN.
            ENDIF.
            zcx_xtt_exception=>raise_sys_error( iv_message = |Index { lv_idx } out of bounds for table '{ lv_tab_name }'.| ).
          ENDIF.
          ASSIGN <ls_row> TO <curr>.
        ELSE.
          " Case B: Key condition [ group = 'GRP A' ]
          lv_found = abap_false.
          CREATE OBJECT lo_cond_expr.
          lo_cond_expr->compile( lv_cond_str ).

          LOOP AT <lt_tab> ASSIGNING <ls_row>.
            IF lo_cond_expr->evaluate( <ls_row> ) = abap_true.
              ASSIGN <ls_row> TO <curr>.
              lv_found = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF lv_found = abap_false.
            IF iv_optional = abap_true.
              RETURN.
            ENDIF.
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
      ELSEIF lv_part CS '(' AND lv_part CS ')'.
        SPLIT lv_part AT '(' INTO lv_fld_name lv_len_str.
        SPLIT lv_len_str AT ')' INTO lv_len_str lv_dummy.
        SHIFT lv_fld_name LEFT DELETING LEADING space.
        SHIFT lv_fld_name RIGHT DELETING TRAILING space.
        SHIFT lv_len_str LEFT DELETING LEADING space.
        SHIFT lv_len_str RIGHT DELETING TRAILING space.
        IF lv_len_str CO '0123456789 '.
          lv_offset  = 0.
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

    GET REFERENCE OF <curr> INTO rr_value.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_node_user_format IMPLEMENTATION.
  METHOD eval.
    DATA lv_text TYPE string.
    DATA lv_date TYPE d.
    DATA lv_number TYPE decfloat34.
    DATA lo_var TYPE REF TO lcl_node_var.
    DATA lr_value TYPE REF TO data.
    FIELD-SYMBOLS <value> TYPE any.
    lv_text = mo_value->eval( is_context ).
    CASE mv_option.
      WHEN 'DATE'.
        TRY.
          lo_var ?= mo_value.
        CATCH cx_sy_move_cast_error.
          zcx_xtt_exception=>raise_sys_error( iv_message = 'DATE requires a date field' ).
        ENDTRY.
        IF lo_var->mv_kind <> cl_abap_typedescr=>typekind_date.
          zcx_xtt_exception=>raise_sys_error( iv_message = 'DATE requires a date field' ).
        ENDIF.
        lv_date = lv_text.
        rv_val = |{ lv_date DATE = USER }|.
      WHEN 'NUMBER'.
        IF mo_value->is_numeric( ) = abap_false.
          zcx_xtt_exception=>raise_sys_error( iv_message = 'NUMBER requires a numeric operand' ).
        ENDIF.
        " Preserve a field's declared decimals when it is formatted directly.
        IF mo_value->is_variable( ) = abap_true.
          lo_var ?= mo_value.
          lr_value = lo_var->resolve( is_context ).
          ASSIGN lr_value->* TO <value>.
          lv_number = <value>.
          rv_val = |{ lv_number NUMBER = USER }|.
          RETURN.
        ENDIF.
        lv_number = _to_number( lv_text ).
        rv_val = |{ lv_number NUMBER = USER }|.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_node_reduce IMPLEMENTATION.
  METHOD is_numeric.
    rv_num = abap_true.
  ENDMETHOD.

  METHOD eval.
    DATA lr_table TYPE REF TO data.
    DATA lr_context TYPE REF TO data.
    DATA lo_table TYPE REF TO cl_abap_tabledescr.
    DATA lo_context TYPE REF TO cl_abap_structdescr.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA ls_component LIKE LINE OF lt_components.
    DATA lv_accumulator TYPE decfloat34.
    FIELD-SYMBOLS <table> TYPE ANY TABLE.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <context> TYPE any.
    FIELD-SYMBOLS <accumulator> TYPE any.
    FIELD-SYMBOLS <iterator> TYPE any.
    lr_table = mo_table->resolve( is_context ).
    ASSIGN lr_table->* TO <table>.
    IF sy-subrc <> 0.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'REDUCE requires an internal table' ).
    ENDIF.
    lo_table ?= cl_abap_typedescr=>describe_by_data( <table> ).
    ls_component-name = mv_accumulator.
    ls_component-type ?= cl_abap_typedescr=>describe_by_data( lv_accumulator ).
    APPEND ls_component TO lt_components.
    ls_component-name = mv_iterator.
    ls_component-type = lo_table->get_table_line_type( ).
    APPEND ls_component TO lt_components.
    lo_context = cl_abap_structdescr=>create( lt_components ).
    CREATE DATA lr_context TYPE HANDLE lo_context.
    ASSIGN lr_context->* TO <context>.
    ASSIGN COMPONENT mv_accumulator OF STRUCTURE <context> TO <accumulator>.
    ASSIGN COMPONENT mv_iterator OF STRUCTURE <context> TO <iterator>.
    LOOP AT <table> ASSIGNING <row>.
      <iterator> = <row>.
      <accumulator> = _to_number( mo_next->eval( <context> ) ).
    ENDLOOP.
    rv_val = |{ <accumulator> }|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_node_country_date IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_value = io_value.
    mo_country = io_country.
  ENDMETHOD.

  METHOD eval.
    DATA lv_date TYPE d.
    DATA lv_country TYPE t005x-land.
    DATA lv_format TYPE t005x-datfm.

    rv_val = mo_value->eval( is_context ).
    IF mo_value->mv_kind <> cl_abap_typedescr=>typekind_date.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'COUNTRY currently supports date fields only' ).
    ENDIF.
    lv_date = rv_val.
    lv_country = mo_country->eval( is_context ).
    IF lv_country IS INITIAL.
      rv_val = |{ lv_date DATE = USER }|.
      RETURN.
    ENDIF.

    SELECT SINGLE datfm INTO lv_format FROM t005x WHERE land = lv_country.
    IF sy-subrc <> 0.
      zcx_xtt_exception=>raise_sys_error( iv_message = |Unknown COUNTRY: { lv_country }| ).
    ENDIF.
    " Gregorian SAP date formats. Do not change the session's SET COUNTRY.
    CASE lv_format.
      WHEN '1'. rv_val = |{ lv_date+6(2) }.{ lv_date+4(2) }.{ lv_date(4) }|.
      WHEN '2'. rv_val = |{ lv_date+4(2) }/{ lv_date+6(2) }/{ lv_date(4) }|.
      WHEN '3'. rv_val = |{ lv_date+4(2) }-{ lv_date+6(2) }-{ lv_date(4) }|.
      WHEN '4'. rv_val = |{ lv_date(4) }.{ lv_date+4(2) }.{ lv_date+6(2) }|.
      WHEN '5'. rv_val = |{ lv_date(4) }/{ lv_date+4(2) }/{ lv_date+6(2) }|.
      WHEN '6'. rv_val = |{ lv_date(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|.
      WHEN OTHERS.
        zcx_xtt_exception=>raise_sys_error( iv_message = |Unsupported COUNTRY date format: { lv_format }| ).
    ENDCASE.
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

    DATA lv_left TYPE string.
    DATA lv_right TYPE string.
    DATA lo_left TYPE REF TO lcl_node_var.
    DATA lo_right TYPE REF TO lcl_node_var.
    DATA lv_date_left TYPE d.
    DATA lv_date_right TYPE d.
    lv_left = mo_left->eval( is_context ).
    lv_right = mo_right->eval( is_context ).
    " Date-to-date subtraction counts days, not YYYYMMDD numbers.
    IF mv_op = '-'.
      TRY.
        lo_left ?= mo_left.
        lo_right ?= mo_right.
        IF lo_left->mv_kind = cl_abap_typedescr=>typekind_date AND
           lo_right->mv_kind = cl_abap_typedescr=>typekind_date.
          lv_date_left = lv_left.
          lv_date_right = lv_right.
          lv_res = lv_date_left - lv_date_right.
          rv_val = |{ lv_res }|.
          RETURN.
        ENDIF.
      CATCH cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.
    lv_l = _to_number( lv_left ).
    lv_r = _to_number( lv_right ).

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
    DATA lv_l TYPE string.
    lv_l = mo_left->eval( is_context ).

    IF mv_op = 'AND'.
      IF lv_l <> abap_true.
        CLEAR rv_val.
        RETURN.
      ENDIF.
      IF mo_right->eval( is_context ) = abap_true.
        rv_val = abap_true.
      ENDIF.
    ELSEIF mv_op = 'OR'.
      IF lv_l = abap_true.
        rv_val = abap_true.
        RETURN.
      ENDIF.
      IF mo_right->eval( is_context ) = abap_true.
        rv_val = abap_true.
      ENDIF.
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
    IF mv_func_name = 'LINE_EXISTS'.
      DATA lo_var TYPE REF TO lcl_node_var.
      DATA lr_value TYPE REF TO data.
      TRY.
        lo_var ?= mo_arg.
      CATCH cx_sy_move_cast_error.
        zcx_xtt_exception=>raise_sys_error( iv_message = 'LINE_EXISTS requires a table expression' ).
      ENDTRY.
      IF lo_var->mv_path NS '[' OR lo_var->mv_path CS '[]'.
        zcx_xtt_exception=>raise_sys_error( iv_message = 'LINE_EXISTS requires a table expression' ).
      ENDIF.
      lr_value = lo_var->resolve( is_context = is_context iv_optional = abap_true ).
      IF lr_value IS BOUND.
        rv_val = abap_true.
      ENDIF.
      RETURN.
    ENDIF.
    rv_val = mo_arg->eval( is_context ).
    CASE mv_func_name.
      WHEN 'STRLEN'.
        rv_val = |{ strlen( rv_val ) }|.
      WHEN 'CONDENSE'.
        CONDENSE rv_val.
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
    IF mv_func_name = 'STRLEN'.
      rv_num = abap_true.
    ELSE.
      rv_num = mo_arg->is_numeric( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
" ====================================================================
" Method Call Implementation
" ====================================================================
CLASS lcl_node_call IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    IF io_caller IS NOT BOUND.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'Please pass IO_HELPER to MERGE( ) method' ).
    ENDIF.
    mo_caller = io_caller.
    mo_descr ?= cl_abap_objectdescr=>describe_by_object_ref( mo_caller ).

    DATA lv_method TYPE string.
    lv_method = iv_method.
    TRANSLATE lv_method TO UPPER CASE.
    READ TABLE mo_descr->methods INTO ms_method WITH KEY name = lv_method.
    IF sy-subrc <> 0 OR ms_method-visibility <> cl_abap_objectdescr=>public.
      zcx_xtt_exception=>raise_sys_error( iv_message = |No public method "{ lv_method }" in IO_HELPER| ).
    ENDIF.

    DATA ls_return TYPE abap_parmdescr.
    READ TABLE ms_method-parameters INTO ls_return WITH KEY parm_kind = cl_abap_objectdescr=>returning.
    IF sy-subrc <> 0.
      zcx_xtt_exception=>raise_sys_error( iv_message = |No returning parameter in method "{ lv_method }"| ).
    ENDIF.
    mv_return_name = ls_return-name.
    mo_return_type = mo_descr->get_method_parameter_type(
      p_method_name = ms_method-name p_parameter_name = mv_return_name ).
  ENDMETHOD.

  METHOD add_argument.
    DATA ls_argument TYPE ts_argument.
    ls_argument-name = iv_name.
    TRANSLATE ls_argument-name TO UPPER CASE.
    READ TABLE ms_method-parameters TRANSPORTING NO FIELDS
      WITH KEY name = ls_argument-name parm_kind = cl_abap_objectdescr=>importing.
    IF sy-subrc <> 0.
      zcx_xtt_exception=>raise_sys_error( iv_message = |Unknown input parameter "{ iv_name }" in { ms_method-name }| ).
    ENDIF.
    ls_argument-expression = io_value.
    ls_argument-datatype = mo_descr->get_method_parameter_type(
      p_method_name = ms_method-name p_parameter_name = ls_argument-name ).
    INSERT ls_argument INTO TABLE mt_arguments.
    IF sy-subrc <> 0.
      zcx_xtt_exception=>raise_sys_error( iv_message = |Duplicate parameter "{ iv_name }" in { ms_method-name }| ).
    ENDIF.
  ENDMETHOD.

  METHOD eval.
    DATA lt_parameters TYPE abap_parmbind_tab.
    DATA ls_parameter TYPE abap_parmbind.
    DATA ls_argument TYPE ts_argument.
    DATA ls_formal TYPE abap_parmdescr.
    DATA lo_error TYPE REF TO cx_root.
    FIELD-SYMBOLS <value> TYPE any.
    FIELD-SYMBOLS <result> TYPE any.

    TRY.
        LOOP AT mt_arguments INTO ls_argument.
          CLEAR ls_parameter.
          ls_parameter-name = ls_argument-name.
          ls_parameter-kind = cl_abap_objectdescr=>exporting.
          CREATE DATA ls_parameter-value TYPE HANDLE ls_argument-datatype.
          ASSIGN ls_parameter-value->* TO <value>.
          <value> = ls_argument-expression->eval( is_context ).
          INSERT ls_parameter INTO TABLE lt_parameters.
        ENDLOOP.

        " Supply the current root/row only when the helper declares IS_ROOT.
        LOOP AT ms_method-parameters INTO ls_formal WHERE parm_kind = cl_abap_objectdescr=>importing.
          READ TABLE lt_parameters TRANSPORTING NO FIELDS WITH TABLE KEY name = ls_formal-name.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
          IF ls_formal-name = 'IS_ROOT'.
            CLEAR ls_parameter.
            ls_parameter-name = ls_formal-name.
            ls_parameter-kind = cl_abap_objectdescr=>exporting.
            GET REFERENCE OF is_context INTO ls_parameter-value.
            INSERT ls_parameter INTO TABLE lt_parameters.
          ENDIF.
        ENDLOOP.

        " Allocate the declared return type, including fixed length and numeric types.
        CLEAR ls_parameter.
        ls_parameter-name = mv_return_name.
        ls_parameter-kind = cl_abap_objectdescr=>receiving.
        CREATE DATA ls_parameter-value TYPE HANDLE mo_return_type.
        ASSIGN ls_parameter-value->* TO <result>.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD mo_caller->(ms_method-name) PARAMETER-TABLE lt_parameters.
        rv_val = <result>.
      CATCH cx_root INTO lo_error.
        zcx_xtt_exception=>raise_sys_error( io_error = lo_error ).
    ENDTRY.
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
        IF lv_two = '&&'.
          CLEAR ls_token.
          ls_token-type = 'CONCAT'.
          ls_token-value = lv_two.
          APPEND ls_token TO rt_tokens.
          lv_pos = lv_pos + 2.
          CONTINUE.
        ENDIF.
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

          " Substring length shorthand: (4)
          IF c = '('.
            lv_kw = lv_ident.
            TRANSLATE lv_kw TO UPPER CASE.
            " Built-in functions keep their argument parentheses as tokens.
            IF lv_kw = 'TO_LOWER' OR lv_kw = 'TO_UPPER' OR lv_kw = 'TO_MIXED' OR lv_kw = 'STRLEN'
              OR lv_kw = 'CONDENSE' OR lv_kw = 'LINE_EXISTS'.
              EXIT.
            ENDIF.
            DATA lv_rem_len     TYPE string.
            DATA lv_cp_len      TYPE i.
            DATA lv_len_spec    TYPE string.
            lv_rem_len = iv_text+lv_pos.
            lv_cp_len = find( val = lv_rem_len sub = ')' ).
            IF lv_cp_len > 1.
              lv_len_spec = substring( val = lv_rem_len off = 1 len = lv_cp_len - 1 ).
              " An empty argument list, e.g. get_fullname( ), is not a length.
              IF lv_len_spec CO '0123456789 ' AND lv_len_spec CA '0123456789'.
                lv_next = lv_cp_len + 1.
                lv_ident = |{ lv_ident }{ iv_text+lv_pos(lv_next) }|.
                lv_pos = lv_pos + lv_next.
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
          WHEN 'REDUCE'.
            ls_token-type = 'REDUCE'.
          WHEN 'TO_LOWER' OR 'TO_UPPER' OR 'TO_MIXED' OR 'STRLEN' OR 'CONDENSE' OR 'LINE_EXISTS'.
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
    DATA lv_option TYPE string.
    DATA lo_value TYPE REF TO lcl_node_var.
    DATA lo_country TYPE REF TO lcl_ast_node.
    ro_root = parse_cond( ).
    lv_option = current( )-value.
    TRANSLATE lv_option TO UPPER CASE.
    IF iv_template = abap_true AND current( )-type = 'IDENT' AND lv_option = 'COUNTRY'.
      consume( 'IDENT' ).
      IF current( )-value <> '='.
        zcx_xtt_exception=>raise_sys_error( iv_message = 'Expected = after COUNTRY' ).
      ENDIF.
      consume( 'COMP' ).
      lo_country = parse_concat( ).
      TRY.
          lo_value ?= ro_root.
        CATCH cx_sy_move_cast_error.
          zcx_xtt_exception=>raise_sys_error( iv_message = 'COUNTRY currently supports date fields only' ).
      ENDTRY.
      CREATE OBJECT ro_root TYPE lcl_node_country_date
        EXPORTING io_value = lo_value io_country = lo_country.
    ELSEIF iv_template = abap_true AND current( )-type = 'IDENT' AND
      ( lv_option = 'DATE' OR lv_option = 'NUMBER' ).
      DATA lo_format TYPE REF TO lcl_node_user_format.
      consume( 'IDENT' ).
      IF current( )-value <> '='.
        zcx_xtt_exception=>raise_sys_error( iv_message = |Expected = after { lv_option }| ).
      ENDIF.
      consume( 'COMP' ).
      consume_word( 'USER' ).
      CREATE OBJECT lo_format.
      lo_format->mv_option = lv_option.
      lo_format->mo_value = ro_root.
      ro_root = lo_format.
    ENDIF.
    IF current( )-type <> 'EOF'.
      zcx_xtt_exception=>raise_sys_error( iv_message = |Unexpected token at end: { current( )-type }-{ current( )-value }| ).
    ENDIF.
  ENDMETHOD.

  METHOD parse_call.
    DATA lv_method TYPE string.
    DATA lv_name TYPE string.
    DATA lo_call TYPE REF TO lcl_node_call.
    DATA lo_value TYPE REF TO lcl_ast_node.

    lv_method = current( )-value.
    consume( 'IDENT' ).
    CREATE OBJECT lo_call EXPORTING io_caller = io_caller iv_method = lv_method.
    consume( 'LPAREN' ).
    WHILE current( )-type <> 'RPAREN'.
      lv_name = current( )-value.
      consume( 'IDENT' ).
      IF current( )-value <> '='.
        zcx_xtt_exception=>raise_sys_error( iv_message = |Expected '=' after parameter { lv_name }| ).
      ENDIF.
      consume( 'COMP' ).
      lo_value = parse_cond( ).
      lo_call->add_argument( iv_name = lv_name io_value = lo_value ).
    ENDWHILE.
    consume( 'RPAREN' ).
    consume( 'EOF' ).
    ro_root = lo_call.
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

    IF current( )-type = 'REDUCE'.
      ro_node = parse_reduce( ).
      RETURN.
    ENDIF.

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

  METHOD consume_word.
    DATA lv_word TYPE string.
    lv_word = current( )-value.
    TRANSLATE lv_word TO UPPER CASE.
    IF current( )-type <> 'IDENT' OR lv_word <> iv_word.
      zcx_xtt_exception=>raise_sys_error( iv_message = |Expected { iv_word }, found { current( )-value }| ).
    ENDIF.
    consume( 'IDENT' ).
  ENDMETHOD.

  METHOD parse_reduce.
    DATA lo_reduce TYPE REF TO lcl_node_reduce.
    DATA lv_path TYPE string.
    DATA lv_name TYPE string.
    DATA lv_length TYPE i.
    CREATE OBJECT lo_reduce.
    consume( 'REDUCE' ).
    consume_word( 'DECFLOAT34' ).
    consume( 'LPAREN' ).
    consume_word( 'INIT' ).
    lo_reduce->mv_accumulator = current( )-value.
    TRANSLATE lo_reduce->mv_accumulator TO UPPER CASE.
    consume( 'IDENT' ).
    consume_word( 'TYPE' ).
    consume_word( 'DECFLOAT34' ).
    consume_word( 'FOR' ).
    lo_reduce->mv_iterator = current( )-value.
    TRANSLATE lo_reduce->mv_iterator TO UPPER CASE.
    IF lo_reduce->mv_iterator = lo_reduce->mv_accumulator.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'REDUCE iterator and accumulator must differ' ).
    ENDIF.
    consume( 'IDENT' ).
    consume_word( 'IN' ).
    lv_path = current( )-value.
    consume( 'IDENT' ).
    lv_length = strlen( lv_path ) - 2.
    IF lv_length > 0 AND lv_path+lv_length = '[]'.
      lv_path = lv_path(lv_length).
    ENDIF.
    CREATE OBJECT lo_reduce->mo_table EXPORTING iv_path = lv_path.
    consume_word( 'NEXT' ).
    lv_name = current( )-value.
    TRANSLATE lv_name TO UPPER CASE.
    IF lv_name <> lo_reduce->mv_accumulator.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'REDUCE NEXT must assign the accumulator' ).
    ENDIF.
    consume( 'IDENT' ).
    IF current( )-value <> '='.
      zcx_xtt_exception=>raise_sys_error( iv_message = 'Expected = after REDUCE accumulator' ).
    ENDIF.
    consume( 'COMP' ).
    lo_reduce->mo_next = parse_concat( ).
    consume( 'RPAREN' ).
    ro_node = lo_reduce.
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

    lo_left = parse_concat( ).

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
      lo_right = parse_concat( ).
      CREATE OBJECT ro_node TYPE lcl_node_compare
        EXPORTING
          iv_op    = lv_op
          io_left  = lo_left
          io_right = lo_right.
      RETURN.
    ENDIF.

    ro_node = lo_left.
  ENDMETHOD.

  METHOD parse_concat.
    DATA lo_concat TYPE REF TO lcl_node_template.
    DATA lo_right TYPE REF TO lcl_ast_node.
    ro_node = parse_operand( ).
    IF current( )-type <> 'CONCAT'.
      RETURN.
    ENDIF.
    CREATE OBJECT lo_concat.
    lo_concat->add_child( ro_node ).
    WHILE current( )-type = 'CONCAT'.
      consume( 'CONCAT' ).
      lo_right = parse_operand( ).
      lo_concat->add_child( lo_right ).
    ENDWHILE.
    ro_node = lo_concat.
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
    IF ls_tok-type = 'COND' OR ls_tok-type = 'REDUCE'.
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
        ro_node = parse_concat( ).
        consume( 'RPAREN' ).

      WHEN 'SWITCH'.
        ro_node = parse_cond( ).

        " String template |...|
      WHEN 'TMPL'.
        consume( 'TMPL' ).
        ro_node = lcl_expression=>_compile_template( ls_tok-value ).

        " Functions: to_lower( ... ), to_upper( ... ), to_mixed( ... ), strlen( ... )
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
  METHOD compile_call.
    DATA lt_tokens TYPE lcl_tokenizer=>tt_token.
    DATA lo_parser TYPE REF TO lcl_parser.
    CLEAR mo_ast.
    lt_tokens = lcl_tokenizer=>tokenize( iv_call ).
    CREATE OBJECT lo_parser EXPORTING it_tokens = lt_tokens.
    mo_ast = lo_parser->parse_call( io_caller ).
  ENDMETHOD.

  METHOD compile.
    DATA lv_expr   TYPE string.
    DATA lv_len    TYPE i.
    DATA lv_len_m1 TYPE i.
    DATA lv_len_m2 TYPE i.

    CLEAR mo_ast.
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
    ro_node = lo_parser->parse( iv_template = iv_template ).
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

        lo_sub_node = _compile_sub_expr( iv_sub_expr = lv_sub_expr iv_template = abap_true ).
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

ENDCLASS.

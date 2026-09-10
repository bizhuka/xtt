" ====================================================================
" 1. AST Node Interface
" ====================================================================

CLASS lcl_ast_node DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES if_serializable_object.

    METHODS eval ABSTRACT
      IMPORTING is_context    TYPE any
      RETURNING VALUE(rv_val) TYPE string
      RAISING   zcx_xtt_exception.

    METHODS is_numeric
      RETURNING VALUE(rv_num) TYPE abap_bool.

    CLASS-METHODS _is_number IMPORTING iv_value            TYPE any
                             RETURNING VALUE(rv_is_number) TYPE abap_bool.
    CLASS-METHODS _to_number
      IMPORTING iv_value         TYPE string
      RETURNING VALUE(rv_number) TYPE decfloat34
      RAISING   zcx_xtt_exception.
ENDCLASS.

" Literal value (quoted string or numeric constant)
CLASS lcl_node_value DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mv_value  TYPE string.
    DATA mv_is_num TYPE abap_bool.

    METHODS constructor IMPORTING iv_val TYPE string iv_is_num TYPE abap_bool DEFAULT abap_false.
    METHODS eval REDEFINITION.
    METHODS is_numeric REDEFINITION.
ENDCLASS.

" Variable resolution (supports ROW-*, VALUE-*, ROOT-*, SY-*, and field paths)
CLASS lcl_node_var DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mv_path   TYPE string.
    DATA mv_is_num TYPE abap_bool.

    METHODS constructor IMPORTING iv_path TYPE string.
    METHODS eval REDEFINITION.
    METHODS is_numeric REDEFINITION.
ENDCLASS.

" Arithmetic sub-expression (+, -, *, /)
CLASS lcl_node_arith DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mv_op    TYPE string.
    DATA mo_left  TYPE REF TO lcl_ast_node.
    DATA mo_right TYPE REF TO lcl_ast_node.

    METHODS constructor IMPORTING iv_op TYPE string io_left TYPE REF TO lcl_ast_node io_right TYPE REF TO lcl_ast_node.
    METHODS eval REDEFINITION.
    METHODS is_numeric REDEFINITION.
ENDCLASS.

" Binary comparison (=, <>, <, <=, >, >=, CP, NP, CS, NS, etc.)
CLASS lcl_node_compare DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mv_op    TYPE string.
    DATA mo_left  TYPE REF TO lcl_ast_node.
    DATA mo_right TYPE REF TO lcl_ast_node.

    METHODS constructor IMPORTING iv_op TYPE string io_left TYPE REF TO lcl_ast_node io_right TYPE REF TO lcl_ast_node.
    METHODS eval REDEFINITION.
ENDCLASS.

" Predicate: IS INITIAL / IS NOT INITIAL
CLASS lcl_node_is_initial DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mo_child TYPE REF TO lcl_ast_node.
    DATA mv_not   TYPE abap_bool.

    METHODS constructor IMPORTING io_child TYPE REF TO lcl_ast_node iv_not TYPE abap_bool DEFAULT abap_false.
    METHODS eval REDEFINITION.
ENDCLASS.

" Logical operators (AND, OR)
CLASS lcl_node_logical DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mv_op    TYPE string.
    DATA mo_left  TYPE REF TO lcl_ast_node.
    DATA mo_right TYPE REF TO lcl_ast_node.

    METHODS constructor IMPORTING iv_op TYPE string io_left TYPE REF TO lcl_ast_node io_right TYPE REF TO lcl_ast_node.
    METHODS eval REDEFINITION.
ENDCLASS.

" Logical NOT
CLASS lcl_node_not DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mo_child TYPE REF TO lcl_ast_node.

    METHODS constructor IMPORTING io_child TYPE REF TO lcl_ast_node.
    METHODS eval REDEFINITION.
ENDCLASS.

" Truthy test for bare boolean variables
CLASS lcl_node_truthy DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mo_child TYPE REF TO lcl_ast_node.

    METHODS constructor IMPORTING io_child TYPE REF TO lcl_ast_node.
    METHODS eval REDEFINITION.
ENDCLASS.

" Template Node: handles "text { expr1 } text { expr2 }"
CLASS lcl_node_template DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mt_children TYPE STANDARD TABLE OF REF TO lcl_ast_node WITH DEFAULT KEY.

    METHODS eval REDEFINITION.
    METHODS add_child IMPORTING io_child TYPE REF TO lcl_ast_node.
ENDCLASS.


" Conditional branch node: cond THEN val [WHEN cond THEN val] ELSE val
CLASS lcl_node_cond DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_branch,
        cond TYPE REF TO lcl_ast_node,
        val  TYPE REF TO lcl_ast_node,
      END OF ts_branch,
      tt_branch TYPE STANDARD TABLE OF ts_branch WITH DEFAULT KEY.

    DATA mt_branches TYPE tt_branch.
    DATA mo_else     TYPE REF TO lcl_ast_node.

    METHODS eval REDEFINITION.
    METHODS is_numeric REDEFINITION.
ENDCLASS.

" SWITCH #( expr WHEN val THEN res ... ELSE default )
CLASS lcl_node_switch DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_branch,
        val_from TYPE REF TO lcl_ast_node,
        val_to   TYPE REF TO lcl_ast_node,
      END OF ts_branch,
      tt_branch TYPE STANDARD TABLE OF ts_branch WITH DEFAULT KEY.

    DATA mo_switch_expr TYPE REF TO lcl_ast_node.
    DATA mt_branches    TYPE tt_branch.
    DATA mo_else        TYPE REF TO lcl_ast_node.

    METHODS eval REDEFINITION.
    METHODS is_numeric REDEFINITION.
ENDCLASS.

" Built-in functions: to_lower( ... ), to_upper( ... )
CLASS lcl_node_func DEFINITION INHERITING FROM lcl_ast_node.
  PUBLIC SECTION.
    DATA mv_func_name TYPE string.
    DATA mo_arg       TYPE REF TO lcl_ast_node.

    METHODS constructor IMPORTING iv_name TYPE string io_arg TYPE REF TO lcl_ast_node.
    METHODS eval REDEFINITION.
    METHODS is_numeric REDEFINITION.
ENDCLASS.

" ====================================================================
" 2. Tokenizer / Lexer
" ====================================================================
CLASS lcl_tokenizer DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_token,
        type  TYPE string,
        value TYPE string,
      END OF ts_token,
      tt_token TYPE STANDARD TABLE OF ts_token WITH DEFAULT KEY.

    CLASS-METHODS tokenize
      IMPORTING iv_text          TYPE string
      RETURNING VALUE(rt_tokens) TYPE tt_token
      RAISING   zcx_xtt_exception.
ENDCLASS.

" ====================================================================
" 3. Recursive-Descent Condition Parser
" ====================================================================
CLASS lcl_parser DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING it_tokens TYPE lcl_tokenizer=>tt_token.
    METHODS parse RETURNING VALUE(ro_root) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.

  PRIVATE SECTION.
    DATA mt_tokens TYPE lcl_tokenizer=>tt_token.
    DATA mv_idx    TYPE i.

    METHODS current RETURNING VALUE(rs_tok) TYPE lcl_tokenizer=>ts_token.
    METHODS consume IMPORTING iv_expected TYPE string OPTIONAL RAISING zcx_xtt_exception.
    METHODS parse_or RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_cond RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_switch RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_and RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_not RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_predicate RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_operand RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_arith_term RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS parse_arith_factor RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node RAISING zcx_xtt_exception.
    METHODS _is_cond_paren RETURNING VALUE(rv_is_cond) TYPE abap_bool.
ENDCLASS.

" ====================================================================
" 4. Expression Evaluator Facade
" ====================================================================
CLASS lcl_expression DEFINITION.
  PUBLIC SECTION.
    METHODS compile
      IMPORTING iv_expr TYPE string
      RAISING   zcx_xtt_exception.

    METHODS evaluate
      IMPORTING is_context       TYPE any
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xtt_exception.

    METHODS evaluate_bool
      IMPORTING is_context       TYPE any
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xtt_exception.

    CLASS-METHODS _compile_template
      IMPORTING iv_template    TYPE string
      RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node
      RAISING   zcx_xtt_exception.

  PRIVATE SECTION.
    DATA mo_ast TYPE REF TO lcl_ast_node.

    CLASS-METHODS _compile_sub_expr
      IMPORTING iv_sub_expr    TYPE string
      RETURNING VALUE(ro_node) TYPE REF TO lcl_ast_node
      RAISING   zcx_xtt_exception.
ENDCLASS.

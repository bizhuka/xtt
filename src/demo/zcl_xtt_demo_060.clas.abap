CLASS zcl_xtt_demo_060 DEFINITION PUBLIC INHERITING FROM zcl_xtt_demo CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_tree_06,
        " Folders hierarchy
        dir          TYPE string,
        par_dir      TYPE string,

        " Empty field. Filled in on_prepare_tree_06
        level        TYPE i,

        sum          TYPE bf_rbetr,
        has_children TYPE abap_bool,
      END OF ts_tree_06,
      tt_tree_06 TYPE STANDARD TABLE OF ts_tree_06 WITH DEFAULT KEY,

      " Document structure
      BEGIN OF ts_root,
        title TYPE string,

        " Or just TYPE tt_tree_06
        t     TYPE REF TO data, " <-- Table of trees (better to use general REF TO)

        " Old way
        c     TYPE REF TO data,
      END OF ts_root.

    METHODS:
      constructor,
      get_url_base    REDEFINITION,
      set_merge_info  REDEFINITION,
      get_templates   REDEFINITION,

      merge           REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      on_prepare_tree_06 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
          ir_tree
          ir_data,

      _get_folders RETURNING VALUE(rt_folder) TYPE tt_tree_06.
ENDCLASS.


CLASS zcl_xtt_demo_060 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    v_desc = 'Tree (group by field relations)'(060).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/tree-group-by-field-relations/'.
  ENDMETHOD.

  METHOD set_merge_info.
    " Document structure
    DATA ls_root    TYPE ts_root.
    DATA lt_folders TYPE REF TO tt_tree_06.
    DATA ls_folder  TYPE REF TO ts_tree_06.

    ls_root-title = 'Title'(tit).
    CREATE DATA lt_folders.
    lt_folders->* = _get_folders( ).

    " Add sums to last elements with no children
    mo_report->init_random_generator( ).
    LOOP AT lt_folders->* REFERENCE INTO ls_folder.

      CHECK ls_folder->has_children <> abap_true.
      ls_folder->sum = mo_report->mo_rand_p->get_next( ).
    ENDLOOP.

    " New way use declarations in a template
    ls_root-t = lt_folders.

    " Old way in code
*    ls_root-c = zcl_xtt_replace_block=>tree_create_relat(
*      it_table      = ls_root-t " REF #( lt_folders )
*      iv_node_key   = 'DIR'
*      iv_relat_key  = 'PAR_DIR' ).

    " Cannot show TREE in alv
    ls_root-c  = _make_string_message( 'Tree - REF TO DATA (Old way)'(tre) ).

    " Paste data
    mo_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD merge.
    " Make copy
    DATA lt_merge LIKE it_merge.
    lt_merge = it_merge.

    " Change R-C
    FIELD-SYMBOLS <ls_merge> LIKE LINE OF lt_merge.
    READ TABLE lt_merge ASSIGNING <ls_merge>
     WITH TABLE KEY key = 'R'.

    DATA lr_root TYPE REF TO ts_root.
    lr_root ?= <ls_merge>-val.

    " Old way in code (new way in template)
    lr_root->c = zcl_xtt_replace_block=>tree_create_relat(
      it_table      = lr_root->t " REF #( lt_folders )
      iv_node_key   = 'DIR'
      iv_relat_key  = 'PAR_DIR' ).

    " Fill some fields in ON_PREPARE_TREE_06( )
    SET HANDLER on_prepare_tree_06 ACTIVATION abap_true.

    " Pass copy
    ro_xtt = super->merge( iv_template = iv_template
                           io_file     = io_file
                           it_merge    = lt_merge[] ).

    SET HANDLER on_prepare_tree_06 ACTIVATION abap_false.
  ENDMETHOD.

  METHOD on_prepare_tree_06.
    FIELD-SYMBOLS <ls_data> TYPE ts_tree_06.

    " Cast to specific data
    ASSIGN ir_data->* TO <ls_data>.

    " Can change value since 'R-T' is REF TO DATA
    <ls_data>-level = ir_tree->level.
  ENDMETHOD.

  METHOD get_templates.
    APPEND `ZXXT_DEMO_060-XLSX`      TO rt_templates.
    APPEND `ZXXT_DEMO_060_FM-XLSX`   TO rt_templates.
    APPEND `ZXXT_DEMO_060_EXCEL-XML` TO rt_templates.
  ENDMETHOD.

  METHOD _get_folders.
    FIELD-SYMBOLS <ls_folder> LIKE LINE OF rt_folder.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:'.
    <ls_folder>-par_dir      = ''.
    <ls_folder>-has_children = abap_true.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\docProps'.
    <ls_folder>-par_dir      = 'R:'.
    <ls_folder>-has_children = abap_false.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\xl'.
    <ls_folder>-par_dir      = 'R:'.
    <ls_folder>-has_children = abap_true.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\xl\printerSettings'.
    <ls_folder>-par_dir      = 'R:\xl'.
    <ls_folder>-has_children = abap_false.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\xl\theme'.
    <ls_folder>-par_dir      = 'R:\xl'.
    <ls_folder>-has_children = abap_false.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\xl\worksheets'.
    <ls_folder>-par_dir      = 'R:\xl'.
    <ls_folder>-has_children = abap_true.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\xl\worksheets\_rels'.
    <ls_folder>-par_dir      = 'R:\xl\worksheets'.
    <ls_folder>-has_children = abap_false.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\xl\_rels'.
    <ls_folder>-par_dir      = 'R:\xl'.
    <ls_folder>-has_children = abap_false.

    APPEND INITIAL LINE TO rt_folder ASSIGNING <ls_folder>.
    <ls_folder>-dir          = 'R:\_rels'.
    <ls_folder>-par_dir      = 'R:'.
    <ls_folder>-has_children = abap_false.
  ENDMETHOD.
ENDCLASS.

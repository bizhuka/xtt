*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_060 DEFINITION FINAL INHERITING FROM lcl_demo.
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
      get_desc_text  REDEFINITION,
      get_url_base    REDEFINITION,
      set_merge_info  REDEFINITION,
      get_templates   REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      _merge          REDEFINITION,
      on_prepare_tree_06 FOR EVENT prepare_tree OF zcl_xtt_replace_block
        IMPORTING
          ir_tree
          ir_data,

      _fill_with_folders
        IMPORTING
          iv_dir    TYPE csequence
          iv_sep    TYPE char1
        CHANGING
          ct_folder TYPE tt_tree_06.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_060 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Tree (group by field relations)'(060).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/tree-group-by-field-relations/'.
  ENDMETHOD.

  METHOD set_merge_info.
    " Show directory.
    IF p_r_path IS INITIAL.
      TRY.
          DATA lo_screen TYPE REF TO zcl_eui_screen.
          DATA lo_error  TYPE REF TO zcx_eui_exception.

          CREATE OBJECT lo_screen
            EXPORTING
              iv_dynnr = '2010'.
        CATCH zcx_eui_exception INTO lo_error.
          MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.

      " Choose folder
      lo_screen->popup( iv_col_end = 87 ).
      CHECK lo_screen->show( ) = 'OK'.
    ENDIF.

    " Ready path
    DATA lv_path LIKE p_r_path.
    lv_path = p_r_path.

    " Document structure
    DATA ls_root    TYPE ts_root.
    DATA lt_folders TYPE REF TO tt_tree_06.

    ls_root-title = `Title`.                                "#EC NOTEXT
    CREATE DATA lt_folders.

    " Delete file separator
    DATA lv_sep TYPE char1.
    cl_gui_frontend_services=>get_file_separator(
     CHANGING
       file_separator = lv_sep ).
    cl_gui_cfw=>flush( EXCEPTIONS OTHERS = 0 ).

    DATA lv_len TYPE i.
    lv_len = strlen( lv_path ) - 1.
    IF lv_path+lv_len(1) = lv_sep.
      lv_path = lv_path(lv_len).
    ENDIF.

    " Add first level or not
    IF p_r_many <> abap_true.
      DATA ls_folder  TYPE REF TO ts_tree_06.
      APPEND INITIAL LINE TO lt_folders->* REFERENCE INTO ls_folder.
      ls_folder->has_children = abap_true.
      ls_folder->dir          = lv_path.
    ENDIF.

    _fill_with_folders(
     EXPORTING
       iv_dir    = lv_path
       iv_sep    = lv_sep
     CHANGING
       ct_folder = lt_folders->* ).

    " Add sums to last elements with no children
    io_report->init_random_generator( ).
    LOOP AT lt_folders->* REFERENCE INTO ls_folder.
      REPLACE FIRST OCCURRENCE OF lv_path: IN ls_folder->par_dir WITH 'R:',
                                           IN ls_folder->dir     WITH 'R:'.
      CHECK ls_folder->has_children <> abap_true.
      ls_folder->sum = io_report->mo_rand_p->get_next( ).
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
    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD _fill_with_folders.
    DATA:
      lt_folder TYPE STANDARD TABLE OF text1000,
      lv_folder TYPE REF TO text1000,
      ls_folder TYPE REF TO ts_tree_06,
      lv_cnt    TYPE i,
      lv_prev   TYPE i.

    cl_gui_frontend_services=>directory_list_files(
     EXPORTING
       directory        = iv_dir
       directories_only = abap_true
     CHANGING
       file_table       = lt_folder
       count            = lv_cnt
     EXCEPTIONS
       OTHERS           = 1 ).
    CHECK sy-subrc = 0 AND lv_cnt > 0.

    " Add one by one
    LOOP AT lt_folder REFERENCE INTO lv_folder.
      " New item
      APPEND INITIAL LINE TO ct_folder REFERENCE INTO ls_folder.
      CONCATENATE iv_dir iv_sep lv_folder->* INTO ls_folder->dir.
      ls_folder->par_dir = iv_dir.

      " Next level
      lv_prev = lines( ct_folder ).
      _fill_with_folders(
       EXPORTING
         iv_dir    = ls_folder->dir
         iv_sep    = iv_sep
       CHANGING
         ct_folder = ct_folder ).

      IF lines( ct_folder ) > lv_prev.
        ls_folder->has_children = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD _merge.
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
    super->_merge( io_xtt   = io_xtt
                   it_merge = lt_merge[] ).

    SET HANDLER on_prepare_tree_06 ACTIVATION abap_false.
  ENDMETHOD.

  METHOD on_prepare_tree_06.
    FIELD-SYMBOLS <ls_data> TYPE ts_tree_06.

    " Cast to specefic data
    ASSIGN ir_data->* TO <ls_data>.

    " Can change value since 'R-T' is REF TO DATA
    <ls_data>-level = ir_tree->level.
  ENDMETHOD.

  METHOD get_templates.
    APPEND `ZXXT_DEMO_060-XLSX`      TO rt_templates.
    APPEND `ZXXT_DEMO_060_FM-XLSX`   TO rt_templates.
    APPEND `ZXXT_DEMO_060_EXCEL-XML` TO rt_templates.
  ENDMETHOD.
ENDCLASS.

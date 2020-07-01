*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_06.
  TYPES:
    " Document structure
    BEGIN OF ts_root,
      title TYPE string,

      " If not tree (just table) could be -> TYPE tt_tree_06
      t     TYPE REF TO data, " <-- Table of trees (better to use general REF TO)

      " Old way
      c     TYPE REF TO data,
    END OF ts_root.

  DATA:
    lo_screen  TYPE REF TO zcl_eui_screen,
    lo_error   TYPE REF TO zcx_eui_exception,
    lo_file    TYPE REF TO zif_xtt_file,
    ls_root    TYPE ts_root,
    lt_folders TYPE REF TO tt_tree_06,
    ls_folder  TYPE REF TO ts_tree_06,
    lv_sep     TYPE char1,
    lv_len     TYPE i.

  " Show directory.
  TRY.
      CREATE OBJECT lo_screen
        EXPORTING
          iv_dynnr = '1010'.
    CATCH zcx_eui_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Choose folder
  lo_screen->popup( iv_col_end = 87 ).
  CHECK lo_screen->show( ) = 'OK'.

  " Document structure
  ls_root-title = `Title`.                                  "#EC NOTEXT
  CREATE DATA lt_folders.

  " Delete file separator
  cl_gui_frontend_services=>get_file_separator(
   CHANGING
     file_separator = lv_sep ).
  cl_gui_cfw=>flush( EXCEPTIONS OTHERS = 0 ).

  lv_len = strlen( p_r_path ) - 1.
  IF p_r_path+lv_len(1) = lv_sep.
    p_r_path = p_r_path(lv_len).
  ENDIF.

  " Add first level or not
  IF p_r_many <> abap_true.
    APPEND INITIAL LINE TO lt_folders->* REFERENCE INTO ls_folder.
    ls_folder->dir = p_r_path.
  ENDIF.

  fill_with_folders(
   EXPORTING
     iv_dir    = p_r_path
     iv_sep    = lv_sep
   CHANGING
     ct_folder = lt_folders->* ).

  " Create tree
  SET HANDLER on_prepare_tree_06 ACTIVATION abap_true.

  " New way use declarations in a template
  ls_root-t = lt_folders.

  " Old way in code
  ls_root-c = zcl_xtt_replace_block=>tree_create_relat(
    it_table      = ls_root-t " REF #( lt_folders )
    iv_node_key   = 'DIR'
    iv_relat_key  = 'PAR_DIR' ).

  " Show data structure only
  IF p_stru = abap_true.
    check_break_point_id( ).
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK jekyll_add_json( ls_root ) = abap_true.
  ENDIF.

  " Info about template & the main class itself
  CREATE OBJECT:
   lo_file TYPE zcl_xtt_file_smw0 EXPORTING
     iv_objid = iv_template,

   ro_xtt TYPE (iv_class_name) EXPORTING
    io_file = lo_file.

  " Paste data
  ro_xtt->merge( is_block = ls_root iv_block_name = 'R' ).

  " Switch off
  SET HANDLER on_prepare_tree_06 ACTIVATION abap_false.
ENDMETHOD.

METHOD fill_with_folders.
  DATA:
    lt_folder TYPE STANDARD TABLE OF text1000,
    lv_folder TYPE REF TO text1000,
    ls_folder TYPE REF TO ts_tree_06,
    lv_cnt    TYPE i.

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
    fill_with_folders(
     EXPORTING
       iv_dir    = ls_folder->dir
       iv_sep    = iv_sep
     CHANGING
       ct_folder = ct_folder ).
  ENDLOOP.
ENDMETHOD.

METHOD on_prepare_tree_06.
  FIELD-SYMBOLS:
    <ls_data>     TYPE ts_tree_06.

  " Cast to specefic data
  ASSIGN ir_data->* TO <ls_data>.
  <ls_data>-level = ir_tree->level.
ENDMETHOD.

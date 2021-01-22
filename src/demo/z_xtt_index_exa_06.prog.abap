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

  " Show directory.
  IF p_r_path IS INITIAL.
    TRY.
        DATA lo_screen TYPE REF TO zcl_eui_screen.
        DATA lo_error  TYPE REF TO zcx_eui_exception.

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
  ENDIF.

  " Ready path
  DATA lv_path LIKE p_r_path.
  lv_path = p_r_path.

  " Document structure
  DATA ls_root    TYPE ts_root.
  DATA lt_folders TYPE REF TO tt_tree_06.

  ls_root-title = `Title`.                                  "#EC NOTEXT
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

  fill_with_folders(
   EXPORTING
     iv_dir    = lv_path
     iv_sep    = lv_sep
   CHANGING
     ct_folder = lt_folders->* ).

  " Add sums to last elements with no children
  _init_random_numbers( ).
  LOOP AT lt_folders->* REFERENCE INTO ls_folder.
    REPLACE FIRST OCCURRENCE OF lv_path: IN ls_folder->par_dir WITH 'R:',
                                         IN ls_folder->dir     WITH 'R:'.
    CHECK ls_folder->has_children <> abap_true.
    ls_folder->sum     = mo_rand_p->get_next( ).
  ENDLOOP.

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
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK mo_injection IS NOT INITIAL.
    mo_injection->send_merge( ls_root ).
  ENDIF.

  " Paste data
  io_xtt->merge( is_block = ls_root iv_block_name = 'R' ).

  " Switch off
  SET HANDLER on_prepare_tree_06 ACTIVATION abap_false.
ENDMETHOD.

METHOD fill_with_folders.
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
    fill_with_folders(
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

METHOD on_prepare_tree_06.
  FIELD-SYMBOLS <ls_data> TYPE ts_tree_06.

  " Cast to specefic data
  ASSIGN ir_data->* TO <ls_data>.
  <ls_data>-level = ir_tree->level.
ENDMETHOD.

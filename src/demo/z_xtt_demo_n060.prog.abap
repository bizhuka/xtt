*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_060 DEFINITION FINAL INHERITING FROM zcl_xtt_demo_060.
  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS:
      _get_folders REDEFINITION,

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

  METHOD _get_folders.
    DATA lo_screen  TYPE REF TO zcl_eui_screen.
    DATA ls_folder  TYPE REF TO ts_tree_06.

    rt_folder[] = super->_get_folders( ).
    " Show directory.
    IF p_r_path IS INITIAL.
      TRY.
          DATA lo_error  TYPE REF TO zcx_eui_exception.

          CREATE OBJECT lo_screen
            EXPORTING
              iv_dynnr = '2010'.
        CATCH zcx_eui_exception INTO lo_error.
          MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.

      " Make obligatory
      lo_screen->customize( name     = 'P_R_PATH'
                            required = '1' ).

      " Choose folder
      DATA lv_col_end TYPE i.
      lo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
      lo_screen->popup( iv_col_end = lv_col_end ).
      CHECK lo_screen->show( ) = 'OK'.

      CLEAR rt_folder[].
    ENDIF.

    " Ready path
    DATA lv_path LIKE p_r_path.
    lv_path = p_r_path.

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
      APPEND INITIAL LINE TO rt_folder REFERENCE INTO ls_folder.
      ls_folder->has_children = abap_true.
      ls_folder->dir          = lv_path.
    ENDIF.

    _fill_with_folders(
     EXPORTING
       iv_dir    = lv_path
       iv_sep    = lv_sep
     CHANGING
       ct_folder = rt_folder ).

    " Add sums to last elements with no children
    CHECK lo_screen IS NOT INITIAL.
    LOOP AT rt_folder REFERENCE INTO ls_folder.
      REPLACE FIRST OCCURRENCE OF lv_path: IN ls_folder->par_dir WITH 'R:',
                                           IN ls_folder->dir     WITH 'R:'.
    ENDLOOP.
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
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT z_xtt_template_013_err_repair.

CLASS lcl_repair DEFINITION INHERITING FROM zcl_xtt_xml_base FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      init IMPORTING io_xtt  TYPE any
                     io_file TYPE REF TO zif_xtt_file,
      repair.

  PRIVATE SECTION.
    CLASS-METHODS:
      _skip_013           CHANGING cv_document TYPE string,
      _replace_all_in_zip,
      _replace_1_in_zip   IMPORTING iv_path TYPE string.

    CLASS-DATA:
      __xtt     TYPE REF TO zcl_xtt_xml_base,
      __file    TYPE REF TO zif_xtt_file,

      __content TYPE xstring,
      __zip     TYPE REF TO cl_abap_zip.
ENDCLASS.

CLASS lcl_repair IMPLEMENTATION.
  METHOD init.
    __file = io_file.
    __xtt ?= io_xtt.
  ENDMETHOD.

  METHOD repair.
    " lv_xtt_class = cl_abap_classdescr=>get_class_name( io_xtt ).
    DATA lo_class TYPE REF TO cl_abap_classdescr.
    lo_class ?= cl_abap_classdescr=>describe_by_object_ref( __xtt ).

    DATA lv_xtt_class TYPE string.
    lv_xtt_class = lo_class->absolute_name+7.

***********************************
    CASE lv_xtt_class.
      WHEN 'ZCL_XTT_WORD_XML'.
        DATA lv_document TYPE string.
        __file->get_content( IMPORTING ev_as_string = lv_document ).
        _skip_013( CHANGING cv_document = lv_document ).
        __content = zcl_eui_conv=>string_to_xstring( lv_document ).

      WHEN 'ZCL_XTT_WORD_DOCX'.
        __file->get_content( IMPORTING ev_as_xstring = __content ).
        _replace_all_in_zip( ).
    ENDCASE.

***********************************
    DATA lo_file  TYPE REF TO zcl_eui_file.
    DATA lo_error TYPE REF TO zcx_eui_exception.
    TRY.
        CREATE OBJECT lo_file.
        lo_file->import_from_xstring( __content ).

        DATA lv_file_name TYPE string.
        lv_file_name = __file->get_name( ).
        lo_file->download( iv_full_path   = lv_file_name
                           iv_save_dialog = abap_true ).
        lo_file->open( ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _skip_013.
    DATA lv_name TYPE string.
    LOOP AT __xtt->mt_names INTO lv_name.
      DATA lv_regexed_name TYPE string.

      CLEAR lv_regexed_name.
      CONCATENATE '{' lv_name '}' INTO lv_name.

      DATA lv_len TYPE i.
      lv_len = strlen( lv_name ).
      DO lv_len TIMES.
        DATA lv_index TYPE i.
        lv_index = sy-index - 1.

        DATA lv_char TYPE string.
        lv_char = lv_name+lv_index(1).

        CASE sy-index.
          WHEN 1.
            lv_regexed_name = '\{'.
            CONTINUE.
          WHEN lv_len.
            lv_char = '\}'.
        ENDCASE.

        CONCATENATE lv_regexed_name '(<[^\>]+>)*' lv_char INTO lv_regexed_name.
      ENDDO.

      REPLACE ALL OCCURRENCES OF REGEX lv_regexed_name IN cv_document WITH lv_name.
    ENDLOOP.
  ENDMETHOD.

  METHOD _replace_all_in_zip.
    CREATE OBJECT __zip.
    __zip->load( __content ).

    _replace_1_in_zip( 'word/document.xml' ).

    FIELD-SYMBOLS <ls_file> LIKE LINE OF __zip->files.
    LOOP AT __zip->files ASSIGNING <ls_file> WHERE name CP 'word/header*.xml'
                                                OR name CP 'word/footer*.xml'.
      _replace_1_in_zip( <ls_file>-name ).
    ENDLOOP.

    __content = __zip->save( ).
  ENDMETHOD.

  METHOD _replace_1_in_zip.
    " Get content as a string from file
    DATA lv_content TYPE string.
    zcl_eui_conv=>xml_from_zip( EXPORTING io_zip   = __zip
                                          iv_name  = iv_path
                                IMPORTING ev_sdoc  = lv_content ).
    _skip_013( CHANGING cv_document = lv_content ).

    zcl_eui_conv=>xml_to_zip( io_zip  = __zip
                              iv_name = iv_path
                              iv_sdoc = lv_content ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM set_template USING
      io_xtt  TYPE any "REF TO zcl_xtt
      io_file TYPE REF TO zif_xtt_file.
  lcl_repair=>init( io_xtt  = io_xtt
                    io_file = io_file ).
ENDFORM.

FORM logger_callback_pbo                                    "#EC CALLED
       USING
         i_t_extab TYPE slis_t_extab.
  APPEND '&ONT'      TO i_t_extab.
  APPEND '%LONGTEXT' TO i_t_extab.
  APPEND '%TECHDET'  TO i_t_extab.
  APPEND '%DETAIL'   TO i_t_extab.
  APPEND '%HELP_BAL' TO i_t_extab.
ENDFORM.

FORM logger_callback_ucomm                                  "#EC CALLED
       CHANGING
         cs_user_command TYPE bal_s_cbuc.

  CHECK cs_user_command-ucomm = '%EXT_PUSH1'.
  lcl_repair=>repair( ).
ENDFORM.

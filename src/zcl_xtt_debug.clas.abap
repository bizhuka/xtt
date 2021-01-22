class ZCL_XTT_DEBUG definition
  public
  final
  create public .

public section.

  methods SAVE_TEMPLATE
    importing
      !IO_FILE type ref to ZIF_XTT_FILE .
  methods SAVE_MERGE
    importing
      !IS_BLOCK type ANY
      !IV_BLOCK_NAME type CSEQUENCE .
  methods LOAD_ALL
    importing
      !IV_FULLPATH type STRING
      !IV_CLASS type CSEQUENCE
    returning
      value(RO_XTT) type ref to ZCL_XTT .
protected section.
private section.

  data MV_DEFAULT_DIR type STRING .
  data MV_MERGE_INDEX type I .

  methods _SAVE
    importing
      !IV_XCONTENT type XSTRING optional
      !IV_CONTENT type STRING optional
      !IV_NAME type STRING .
ENDCLASS.



CLASS ZCL_XTT_DEBUG IMPLEMENTATION.


METHOD load_all.
  DATA lv_file_name TYPE string.
  DATA lv_path      TYPE string.
  zcl_eui_file=>split_file_path( EXPORTING iv_fullpath = iv_fullpath
                                 IMPORTING ev_filename = lv_file_name
                                           ev_path     = lv_path ).
  DATA lo_file TYPE REF TO zcl_eui_file.
  CREATE OBJECT lo_file.

  DATA lo_error TYPE REF TO zcx_eui_exception.
  TRY.
      lo_file->import_from_file( iv_full_path = iv_fullpath ).
    CATCH zcx_eui_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  DATA lo_raw TYPE REF TO zcl_xtt_file_raw.
  CREATE OBJECT lo_raw
    EXPORTING
      iv_name    = lv_file_name
      iv_xstring = lo_file->mv_xstring.

  CREATE OBJECT ro_xtt TYPE (iv_class)
    EXPORTING
      io_file = lo_raw.

  DATA lv_index        TYPE i.
  DATA lv_merge_index  TYPE string.
  DATA lv_content      TYPE string.
  DATA lv_ok           TYPE abap_bool.
  DATA lo_type         TYPE REF TO cl_abap_datadescr.
  DATA lr_data         TYPE REF TO data.
  FIELD-SYMBOLS <ls_data> TYPE any.
  DO.
    " Next name
    ADD 1 TO lv_index.
    lv_merge_index = lv_index.
    CONDENSE lv_merge_index.

    TRY.
        CONCATENATE lv_path `MERGE_TYPE_` lv_merge_index INTO lv_file_name.
        lo_file->import_from_file( iv_full_path = lv_file_name ).
        lv_content = zcl_eui_conv=>xstring_to_string( lo_file->mv_xstring ).

        DATA ls_field_desc TYPE zcl_eui_type=>ts_field_desc.
        zcl_eui_conv=>from_json( EXPORTING iv_json = lv_content
                                 IMPORTING ev_ok   = lv_ok
                                           ex_data = ls_field_desc ).

        " Create by description
        lo_type = zcl_eui_type=>create_type_descr( is_field_desc = ls_field_desc ).
        CREATE DATA lr_data TYPE HANDLE lo_type.
        ASSIGN lr_data->* TO <ls_data>.

        " Load data
        CONCATENATE lv_path `MERGE_DATA_` lv_merge_index INTO lv_file_name.
        lo_file->import_from_file( iv_full_path = lv_file_name ).
        lv_content = zcl_eui_conv=>xstring_to_string( lo_file->mv_xstring ).

        zcl_eui_conv=>from_json( EXPORTING iv_json = lv_content
                                 IMPORTING ev_ok   = lv_ok
                                           ex_data = <ls_data> ).
        ro_xtt->merge( is_block      = <ls_data>
                       iv_block_name = ls_field_desc-name ).
      CATCH zcx_eui_exception INTO lo_error.
        DATA lv_error TYPE string.
        lv_error = lo_error->get_text( ).
*        DATA: lv_prog TYPE syrepid, lv_incl TYPE syrepid, lv_line TYPE i.
*        lo_error->get_source_position( IMPORTING program_name = lv_prog
*                                                 include_name = lv_incl
*                                                 source_line  = lv_line ).
        MESSAGE lv_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDDO.
ENDMETHOD.


METHOD save_merge.
  DATA lv_merge_index  TYPE string.
  DATA lv_file_name    TYPE string.

  ADD 1 TO mv_merge_index.
  lv_merge_index = mv_merge_index.
  CONDENSE lv_merge_index.

  DATA lo_error TYPE REF TO zcx_eui_exception.
  TRY.
      DATA ls_field_desc TYPE zcl_eui_type=>ts_field_desc.
      ls_field_desc = zcl_eui_type=>get_field_desc( iv_field_name = iv_block_name
                                                    iv_data       = is_block
                                                    iv_tech       = abap_true ).
    CATCH zcx_eui_exception INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  DATA lv_content TYPE string.
  lv_content = zcl_eui_conv=>to_json( ls_field_desc ).
  CONCATENATE `MERGE_TYPE_` lv_merge_index INTO lv_file_name.
  _save( iv_content = lv_content
         iv_name    = lv_file_name ).

  lv_content = zcl_eui_conv=>to_json( is_block ).
  CONCATENATE `MERGE_DATA_` lv_merge_index INTO lv_file_name.
  _save( iv_content = lv_content
         iv_name    = lv_file_name ).
ENDMETHOD.


METHOD save_template.
  DATA lv_name      TYPE string.
  DATA lv_xcontent  TYPE xstring.
  DATA lo_no_check  TYPE REF TO zcx_eui_no_check.

  TRY.
      lv_name = io_file->get_name( ).
      io_file->get_content( IMPORTING ev_as_xstring = lv_xcontent ).
    CATCH zcx_eui_no_check INTO lo_no_check.
      MESSAGE lo_no_check TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Path
  DATA lv_sep TYPE char1.
  cl_gui_frontend_services=>get_file_separator(
   CHANGING   file_separator = lv_sep
   EXCEPTIONS OTHERS         = 3 ).
  CHECK sy-subrc  = 0.

  " Save to Desktop
  cl_gui_frontend_services=>get_desktop_directory(
   CHANGING   desktop_directory = mv_default_dir
   EXCEPTIONS OTHERS            = 2 ).
  CHECK sy-subrc = 0.
  cl_gui_cfw=>flush( ).

  " Full path
  DATA lv_guid TYPE string.
  lv_guid = zcl_eui_conv=>guid_create( ).
  CONCATENATE mv_default_dir lv_sep sy-cprog ` ` lv_guid lv_sep INTO mv_default_dir.

  " Save template
  _save( iv_xcontent = lv_xcontent
         iv_name     = lv_name ).
ENDMETHOD.


METHOD _save.
  DATA lv_xcontent LIKE iv_xcontent.
  IF iv_xcontent IS SUPPLIED.
    lv_xcontent = iv_xcontent.
  ELSE.
    lv_xcontent = zcl_eui_conv=>string_to_xstring( iv_content ).
  ENDIF.

  " Pass file content
  DATA lo_file TYPE REF TO zcl_eui_file.
  CREATE OBJECT lo_file
    EXPORTING
      iv_xstring = lv_xcontent.

  " Fill path to file
  DATA lv_fullpath TYPE string.
  CONCATENATE mv_default_dir iv_name INTO lv_fullpath.

  DATA lo_exception TYPE REF TO zcx_eui_exception.
  TRY.
      lo_file->download( iv_full_path = lv_fullpath ).
    CATCH zcx_eui_exception INTO lo_exception.
      MESSAGE lo_exception TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
ENDMETHOD.
ENDCLASS.

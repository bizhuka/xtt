class ZCL_XTT_COMP_CELL definition
  public
  final
  create private .

public section.

  data MV_IMAGE type XSTRING read-only .
  data MV_EXT type STRING read-only .
  data MV_WIDTH type I read-only .
  data MV_HEIGHT type I read-only .
  data MV_INDEX_TXT type STRING read-only .

  class-methods CREATE_IMAGE
    importing
      !IV_IMAGE type XSTRING
      !IV_EXT type CSEQUENCE default '.jpeg'
      !IV_WIDTH type I optional
      !IV_HEIGHT type I optional
    returning
      value(RO_COMP_CELL) type ref to ZCL_XTT_COMP_CELL .
  class-methods CREATE_SHAPE
    returning
      value(RO_COMP_CELL) type ref to ZCL_XTT_COMP_CELL .
  methods SAVE_IN_ARCHIVE
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IV_PREFIX type CSEQUENCE
    exporting
      !EV_FILE_NAME type STRING
      !EV_MIME_TEXT type STRING
      !EV_REWRITE type ABAP_BOOL .
protected section.
private section.

  types:
    TT_COMP_CELL type STANDARD TABLE OF REF TO ZCL_XTT_COMP_CELL .

  class-data MT_COMP_CELL type TT_COMP_CELL .
ENDCLASS.



CLASS ZCL_XTT_COMP_CELL IMPLEMENTATION.


METHOD CREATE_IMAGE.
  " Factory method?
  CREATE OBJECT ro_comp_cell.

  ro_comp_cell->mv_image  = iv_image.
  ro_comp_cell->mv_ext    = iv_ext.

  " PDF WORD ?
  ro_comp_cell->mv_width  = iv_width.
  ro_comp_cell->mv_height = iv_height.

  " All comp cells
  APPEND ro_comp_cell TO mt_comp_cell.

  DATA lv_cnt TYPE i.
  lv_cnt = lines( mt_comp_cell ).
  int_2_text lv_cnt ro_comp_cell->mv_index_txt.
ENDMETHOD.


  method CREATE_SHAPE.
  endmethod.


METHOD save_in_archive.
  CLEAR ev_file_name.
  CLEAR ev_mime_text.
  CLEAR ev_rewrite.

  " File name
  CONCATENATE `_img` mv_index_txt mv_ext INTO ev_file_name.

  CASE mv_ext.
    WHEN `.png`.
      ev_mime_text = `<Default Extension="png" ContentType="image/png"/>`.
    WHEN `.gif`.
      ev_mime_text = `<Default Extension="gif" ContentType="image/gif"/>`.
    WHEN `.jpg`.
      ev_mime_text = `<Default Extension="jpg" ContentType="image/jpeg"/>`.
    WHEN `.jpeg`.
      ev_mime_text = `<Default Extension="jpeg" ContentType="image/jpeg"/>`.
    WHEN OTHERS.
      zcx_xtt_exception=>raise_dump( iv_message = `Unknown image type` ).
  ENDCASE.

  " Path to image
  DATA lv_full_path TYPE string.
  CONCATENATE iv_prefix ev_file_name INTO lv_full_path.

  " Already saved?
  io_zip->get( EXPORTING  name   = lv_full_path
               EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc = 0.
    ev_rewrite = abap_true.
    RETURN.
  ENDIF.

  io_zip->add( name    = lv_full_path
               content = mv_image ).
ENDMETHOD.
ENDCLASS.

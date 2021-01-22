class ZCL_XTT_IMAGE definition
  public
  final
  create private .

public section.
  type-pools ABAP .

  data MV_IMAGE type XSTRING read-only .
  data MV_EXT type STRING read-only .
  data MV_WIDTH type I read-only .
  data MV_HEIGHT type I read-only .
  data MV_INDEX_TXT type STRING read-only .

  class-methods CREATE_IMAGE
    importing
      !IV_IMAGE type XSTRING
      !IV_EXT type CSEQUENCE optional
      !IV_WIDTH type I optional
      !IV_HEIGHT type I optional
      !IV_BY_DECL type ABAP_BOOL optional
    returning
      value(RO_IMAGE) type ref to ZCL_XTT_IMAGE .
  class-methods CREATE_IMAGE_DECL
    importing
      !IR_FIELD type ref to ZCL_XTT_REPLACE_BLOCK=>TS_FIELD
      !IT_PAIR type ZCL_XTT_SCOPE=>TT_PAIR .
  methods SAVE_IN_ARCHIVE
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IV_PREFIX type CSEQUENCE
    exporting
      !EV_FILE_NAME type STRING
      !EV_MIME_TEXT type STRING
      !EV_REWRITE type ABAP_BOOL .
  methods CLONE
    importing
      !SOURCE type ANY
    returning
      value(RESULT) type ref to ZCL_XTT_IMAGE .
protected section.
private section.

  types:
    BEGIN OF ts_cache,
      value    TYPE xstring,
      width    TYPE i,
      height   TYPE i,
      instance TYPE REF TO zcl_xtt_image,
    END OF ts_cache .
  types:
    tt_cache TYPE SORTED TABLE OF ts_cache WITH UNIQUE KEY value width height .

  class-data MT_CACHE type TT_CACHE .
  data MV_BY_DECL type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_XTT_IMAGE IMPLEMENTATION.


METHOD clone.
  " Is raw xString?
  FIELD-SYMBOLS <lv_xstring> TYPE xstring.
  TRY.
      ASSIGN source TO <lv_xstring> CASTING.
      result = zcl_xtt_image=>create_image( iv_image   = <lv_xstring>
                                            iv_ext     = me->mv_ext
                                            iv_width   = me->mv_width
                                            iv_height  = me->mv_height
                                            iv_by_decl = me->mv_by_decl
                                           ).
    CATCH cx_sy_assign_cast_illegal_cast.
      CLEAR result.
  ENDTRY.
ENDMETHOD.


METHOD create_image.
  DATA          ls_cache   LIKE LINE OF mt_cache.
  FIELD-SYMBOLS <ls_cache> LIKE LINE OF mt_cache.

  READ TABLE mt_cache ASSIGNING <ls_cache>
   WITH TABLE KEY value  = iv_image
                  width  = iv_width
                  height = iv_height.

  IF sy-subrc = 0.
    ro_image = <ls_cache>-instance.
  ELSE.
    CREATE OBJECT ro_image.
    ls_cache-instance = ro_image.
    ls_cache-value    = iv_image.

    " Constructor ?
    ro_image->mv_width   = ls_cache-width  = iv_width.
    ro_image->mv_height  = ls_cache-height = iv_height.
    ro_image->mv_by_decl = iv_by_decl.

    INSERT ls_cache INTO TABLE mt_cache.

    " Set 1 time only
    ro_image->mv_image  = iv_image.
    IF iv_ext IS INITIAL.
      ro_image->mv_ext = '.jpeg'.
    ELSE.
      ro_image->mv_ext = iv_ext.
    ENDIF.

    DATA lv_cnt TYPE i.
    lv_cnt = lines( mt_cache ).
    int_2_text lv_cnt ro_image->mv_index_txt.
  ENDIF.

  " Set custom size
***  ro_image->mv_width  = iv_width.
***  ro_image->mv_height = iv_height.
ENDMETHOD.


METHOD create_image_decl.
  TYPES: BEGIN OF ts_option,
           width  TYPE i,
           height TYPE i,
           ext    TYPE string,
         END OF ts_option.
  DATA ls_option TYPE ts_option.
  FIELD-SYMBOLS <lv_xstring> TYPE xstring.

  IF ir_field->oref IS NOT INITIAL.
    DATA lo_source TYPE REF TO zcl_xtt_image.
    lo_source ?= ir_field->oref.
    ASSIGN lo_source->mv_image TO <lv_xstring>.

    ls_option-ext    = lo_source->mv_ext.
    IF lo_source->mv_by_decl <> abap_true.
      ls_option-height = lo_source->mv_height.
      ls_option-width  = lo_source->mv_width.
    ENDIF.
  ENDIF.

  IF ir_field->dref IS NOT INITIAL.
    ASSIGN ir_field->dref->* TO <lv_xstring>.
  ENDIF.

  FIELD-SYMBOLS <ls_pair> LIKE LINE OF it_pair.
  LOOP AT it_pair ASSIGNING <ls_pair>.

    CASE <ls_pair>-key.
      WHEN 'type'.

      WHEN 'width'.
        ls_option-width  = <ls_pair>-val.
      WHEN 'height'.
        ls_option-height = <ls_pair>-val.
      WHEN 'ext'.
        CONCATENATE '.' <ls_pair>-val INTO ls_option-ext.
      WHEN OTHERS.
        MESSAGE e017(zsy_xtt) WITH <ls_pair>-key INTO sy-msgli.
        zcx_eui_no_check=>raise_sys_error( ).
    ENDCASE.
  ENDLOOP.

  CLEAR ir_field->dref.

*  IF ir_field->oref IS NOT INITIAL.
*    lo_source->mv_height = ls_option-height.
*    lo_source->mv_width  = ls_option-width.
*    RETURN.
*  ENDIF.

  ir_field->oref = create_image( iv_image   = <lv_xstring>
                                 iv_ext     = ls_option-ext
                                 iv_width   = ls_option-width
                                 iv_height  = ls_option-height
                                 iv_by_decl = abap_true ).
ENDMETHOD.


METHOD SAVE_IN_ARCHIVE.
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

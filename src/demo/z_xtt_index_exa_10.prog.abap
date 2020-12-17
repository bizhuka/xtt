*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

METHOD example_10.
  TYPES:
    BEGIN OF ts_icon,
      id   TYPE icon-id,
      name TYPE bapibds01-objkey,
      " use code declaration instead
      img  TYPE REF TO object, " zcl_xtt_image
      raw  TYPE xstring,
    END OF ts_icon,

    " Document structure
    BEGIN OF ts_root,
      title TYPE string,
      t     TYPE STANDARD TABLE OF ts_icon WITH DEFAULT KEY, " internal flat table ( In template {R-T} )
    END OF ts_root.

  " General info
  DATA ls_root TYPE ts_root.
  ls_root-title = 'Test icons'(tic).

  " Get icon IDs
  SELECT id name INTO CORRESPONDING FIELDS OF TABLE ls_root-t ##too_many_itab_fields
  FROM icon UP TO p_r_cnt ROWS
  WHERE oleng = 2.

  " From business data storage
  DATA lo_bds TYPE REF TO cl_bds_document_set.
  CREATE OBJECT lo_bds.

  " Create images
  FIELD-SYMBOLS <ls_icon> TYPE ts_icon.
  LOOP AT ls_root-t ASSIGNING <ls_icon>.
    " File info
    DATA  lt_content        TYPE sbdst_content.
    DATA  lt_components     TYPE sbdst_components.
    DATA  lr_component      TYPE REF TO bapicompon.
    DATA  lv_size           TYPE i.
    DATA  lv_image          TYPE xstring.

    CLEAR lt_content.
    CLEAR lt_components.

    " Read 1 icon
    lo_bds->get_with_table(
      EXPORTING
        classname   = 'SAP_ICONS'
        classtype   = 'OT'
        object_key  = <ls_icon>-name
      CHANGING
        content     = lt_content
        components  = lt_components
      EXCEPTIONS
        OTHERS      = 1 ) .
    CHECK sy-subrc = 0.

    " Get file size
    READ TABLE lt_components REFERENCE INTO lr_component INDEX 1.
    CHECK sy-subrc = 0.
    lv_size = lr_component->comp_size.

    " As xString
    lv_image = zcl_eui_conv=>binary_to_xstring( it_table  = lt_content
                                                iv_length = lv_size ).

    " @see code declaration (preferable way)
    IF iv_raw = abap_true.
      <ls_icon>-raw = lv_image.
      CONTINUE.
    ENDIF.

    " old approach (direct call)
    DATA lv_width  TYPE i.
    DATA lv_height TYPE i.
    IF p_size = abap_true.
      lv_width = lv_height = 200000.
    ENDIF.

    " Create new instance (for internal use only)
    <ls_icon>-img = zcl_xtt_image=>create_image( iv_image  = lv_image
                                                 iv_ext    = '.gif' "#EC NOTEXT
                                                 iv_width  = lv_width
                                                 iv_height = lv_height ).
  ENDLOOP.

  " Show data structure only
  IF p_stru = abap_true.
    BREAK-POINT ID zxtt_break_point. " Double click here --> ls_root <--

    " For internal use
    CHECK mo_injection IS NOT INITIAL.
    mo_injection->send_merge( ls_root ).
  ENDIF.

  " Paste data
  io_xtt->merge( is_block      = ls_root
                 iv_block_name = 'R' ).
ENDMETHOD.

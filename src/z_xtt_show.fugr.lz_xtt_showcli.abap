*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_main IMPLEMENTATION.
  METHOD pbo_0100.
    DATA:
      lv_data        TYPE xstring,
      lv_file_size   TYPE i,
      lt_data        TYPE solix_tab,
      lo_control     TYPE REF TO i_oi_container_control,
      lo_document    TYPE REF TO i_oi_document_proxy,
      lo_html_viewer TYPE REF TO cl_gui_html_viewer,
      lv_proxy_app   TYPE text255,
      lv_ext         TYPE char4,
      lv_url         TYPE text255,
      ls_handle      TYPE cntl_handle.

    DO 1 TIMES.
      " Set default status. thanks to monty79
      SET PF-STATUS 'STATUS_100'.

      " Create 1 time only
      CHECK mo_cont IS INITIAL.
      CREATE OBJECT mo_cont
        EXPORTING
          container_name = 'EMPTY_AREA'.

      " Convert to table
      lv_data = sender->get_raw( ).
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_data
        IMPORTING
          output_length = lv_file_size
        TABLES
          binary_tab    = lt_data
        EXCEPTIONS
          OTHERS        = 1.
      CHECK sy-subrc = 0.

      zcl_xtt_util=>get_ole_info(
       EXPORTING
         io_xtt       = sender
       CHANGING
         cv_proxy_app = lv_proxy_app
         cv_file_ext  = lv_ext ).

      IF lv_proxy_app IS NOT INITIAL.
        c_oi_container_control_creator=>get_container_control( IMPORTING control = lo_control ).
        lo_control->init_control( EXPORTING  inplace_enabled     = abap_true
                                             r3_application_name = sy-tcode
                                             parent              = mo_cont ).


        lo_control->get_document_proxy( EXPORTING document_type  = lv_proxy_app
                                        IMPORTING document_proxy = lo_document ).

        " Show documnet
        lo_document->open_document_from_table( EXPORTING document_size    = lv_file_size
                                                         document_table   = lt_data
                                                         open_inplace     = abap_true ).

        " For OLE
        lo_document->get_document_handle(
           IMPORTING
            handle = ls_handle ).

        " Get Application object (the same for Word & Excel)
        GET PROPERTY OF ls_handle-obj 'Application' = mv_app_obj.

      ELSEIF lv_ext IS NOT INITIAL.
        CREATE OBJECT lo_html_viewer
          EXPORTING
            parent = mo_cont.

        " Load
        lo_html_viewer->load_data(
         EXPORTING
           type                 = 'application'
           subtype              = lv_ext
         IMPORTING
           assigned_url         = lv_url
         CHANGING
           data_table           = lt_data
         EXCEPTIONS
           OTHERS               = 1 ).

        " Show it
        IF sy-subrc = 0.
          lo_html_viewer->show_url(
            url      = lv_url
            in_place = abap_true ).
        ENDIF.
      ENDIF.
    ENDDO.

    RAISE EVENT pbo
     EXPORTING
       sender     = sender
       io_app_obj = mv_app_obj.
  ENDMETHOD.

  METHOD pai_0100.
    RAISE EVENT pai
     EXPORTING
       sender = sender
       iv_cmd = gv_cmd.
  ENDMETHOD.

  METHOD get_raw.
  ENDMETHOD.

  METHOD merge.
  ENDMETHOD.
ENDCLASS.

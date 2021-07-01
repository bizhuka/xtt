*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_160 DEFINITION INHERITING FROM lcl_demo_022.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_root,
        first_name  TYPE string,
        last_name   TYPE string,
        middle_name TYPE string,
        t           TYPE tt_flight_info,
      END OF ts_root.

    DATA:
      mt_month_name TYPE STANDARD TABLE OF t247 WITH DEFAULT KEY.

    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION,

      _get_month_texts
        RETURNING VALUE(rt_month_name) LIKE mt_month_name,

      get_fullname                                          "#EC CALLED
        IMPORTING
                  is_root        TYPE ts_root " <--- is passed implicitly
        RETURNING VALUE(rv_text) TYPE string,

      date_text                                             "#EC CALLED
        IMPORTING
                  " is_root TYPE ts_flight_info <-- no need. Pass v-FLDATE explicitly
                  iv_date        TYPE d
                  iv_lang        TYPE sylangu DEFAULT sy-langu
        RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_160 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = ';call'(160).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/call/'.
  ENDMETHOD.

  METHOD set_merge_info.
    " Init document structure
    DATA ls_root TYPE ts_root.
    ls_root-first_name   = 'FirstName'.                     "#EC NOTEXT
    ls_root-last_name    = 'LastName'.                      "#EC NOTEXT
    ls_root-middle_name  = 'MiddleName'.                    "#EC NOTEXT
    ls_root-t            = _get_flight_info( ).

    io_report->merge_add_one( is_root   = ls_root
                              io_helper = me ).

    mt_month_name = _get_month_texts( ).
  ENDMETHOD.

  METHOD _get_month_texts.
    DO 2 TIMES.
      DATA lv_langu TYPE sylangu.
      CASE sy-index.
        WHEN 1.
          lv_langu = sy-langu.
        WHEN 2.
          lv_langu = 'D'.
      ENDCASE.

      DATA lt_month_name LIKE rt_month_name.
      CLEAR lt_month_name.
      CALL FUNCTION 'MONTH_NAMES_GET'
        EXPORTING
          language    = lv_langu
        TABLES
          month_names = lt_month_name[]
        EXCEPTIONS
          OTHERS      = 1.
      CHECK sy-subrc = 0.
      APPEND LINES OF lt_month_name TO rt_month_name.
    ENDDO.
    SORT rt_month_name BY spras mnr.
  ENDMETHOD.

  " @see in the template ---> Fullname -> {R@get_fullname( )}
  METHOD get_fullname.
    CONCATENATE is_root-first_name is_root-last_name is_root-middle_name INTO rv_text SEPARATED BY space.
    TRANSLATE rv_text TO UPPER CASE.
  ENDMETHOD.

  " @see in the template ---> {R-T@date_text( iv_date = v-FLDATE )}
  METHOD date_text.
    FIELD-SYMBOLS <ls_month_name> LIKE LINE OF mt_month_name.

    DATA lv_month TYPE t247-mnr.
    lv_month = iv_date+4(2).

    READ TABLE mt_month_name ASSIGNING <ls_month_name> BINARY SEARCH
     WITH KEY spras = iv_lang
              mnr   = lv_month.
    CHECK sy-subrc = 0.

    CONCATENATE iv_date(4) <ls_month_name>-ltx iv_date+6(2) INTO rv_text SEPARATED BY '-'.
  ENDMETHOD.

  METHOD get_templates.
    APPEND `ZXXT_DEMO_160-XLSX` TO rt_templates.
  ENDMETHOD.
ENDCLASS.

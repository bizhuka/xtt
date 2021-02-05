*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_demo_022 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Flight Model'(022).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = ''.
  ENDMETHOD.

  METHOD set_merge_info.
    TYPES:
      BEGIN OF ts_row,
        cityfrom  TYPE spfli-cityfrom,
        landxfrom TYPE t005t-landx,
        cityto    TYPE spfli-cityto,
        landxto   TYPE t005t-landx,
        carrname  TYPE scarr-carrname,
        connid    TYPE spfli-connid,
        fldate    TYPE sflight-fldate,
        price     TYPE sflight-price,
        currency  TYPE sflight-currency,
        seatsmax  TYPE sflight-seatsmax,
        seatsocc  TYPE sflight-seatsocc,
        _group1   TYPE string,
      END OF ts_row,

      " Document structure
      BEGIN OF ts_root,
        t TYPE STANDARD TABLE OF ts_row WITH DEFAULT KEY,
      END OF ts_root.
    DATA ls_root TYPE ts_root.

    SELECT pf~cityfrom
           cf~landx       AS landxfrom
           pf~cityto
           ct~landx       AS landxto
           cr~carrname
           pf~connid
           fl~fldate
           fl~price
           fl~currency
           fl~seatsmax
           fl~seatsocc
        INTO CORRESPONDING FIELDS OF TABLE ls_root-t
        FROM spfli          AS pf
       INNER JOIN sflight   AS fl
          ON pf~carrid      EQ fl~carrid
         AND pf~connid      EQ fl~connid
       INNER JOIN scarr     AS cr
          ON pf~carrid      EQ cr~carrid
       INNER JOIN t005t     AS cf
          ON pf~countryfr   EQ cf~land1
         AND cf~spras       EQ sy-langu
       INNER JOIN t005t     AS ct         "#EC "#EC CI_BUFFJOIN
          ON pf~countryto   EQ ct~land1
         AND ct~spras       EQ sy-langu
       UP TO p_r_cnt ROWS
       ORDER
          BY cf~landx
             pf~cityfrom
             ct~landx
             pf~cityto
             cr~carrname
             pf~connid
             fl~fldate.

    DATA lr_row TYPE REF TO ts_row.
    LOOP AT ls_root-t REFERENCE INTO lr_row.
      CONCATENATE lr_row->cityfrom
                  lr_row->landxfrom
                  lr_row->cityto
                  lr_row->landxto   INTO lr_row->_group1 SEPARATED BY '-'.

*      CONCATENATE lr_row->cityfrom
*                  lr_row->landxfrom INTO lr_row->_group2 SEPARATED BY '-'.
    ENDLOOP.

    io_report->merge_add_one( ls_root ).
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_022-XLSX' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

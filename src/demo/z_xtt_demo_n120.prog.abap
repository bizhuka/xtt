*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_120_attr DEFINITION FINAL
     FRIENDS zcl_xtt_replace_block. " <--- for private fields
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_rand_data TYPE ts_rand_data.

    " All fields are private!
  PRIVATE SECTION.
    DATA:
      group   TYPE string,                                  "#EC NEEDED
      caption TYPE string,                                  "#EC NEEDED
      date    TYPE d,                                       "#EC NEEDED
      sum1    TYPE bf_rbetr,                                "#EC NEEDED
      sum2    TYPE bf_rbetr.                                "#EC NEEDED
ENDCLASS.
**********************************************************************
CLASS lcl_demo_120 DEFINITION FINAL INHERITING FROM lcl_demo.
  PUBLIC SECTION.
    " INTERFACES if_serializable_object

    METHODS:
      get_desc_text  REDEFINITION,
      get_url_base   REDEFINITION,
      get_screen_opt REDEFINITION,
      set_merge_info REDEFINITION,
      get_templates  REDEFINITION.

    TYPES:
     tt_demo_120_attr TYPE STANDARD TABLE OF REF TO lcl_demo_120_attr WITH DEFAULT KEY.

    " Public fields are always accessible
    DATA title TYPE string.                                 "#EC NEEDED
    DATA t TYPE tt_rand_data. "#EC NEEDED internal flat table ( In template {R-T} )
    DATA a TYPE REF TO data. "#EC NEEDED tt_demo_120_attr, Cannot show in ALV
    DATA date TYPE d.                                     "#EC NEEDED 8
    DATA time TYPE t.                                     "#EC NEEDED 6
    DATA datetime TYPE cpet_reftimestamp. "#EC NEEDED 14 = date(8) + time(6)

  PROTECTED SECTION.
    METHODS:
      _make_a_obj_table.
ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
CLASS lcl_demo_120 IMPLEMENTATION.
  METHOD get_desc_text.
    rv_desc_text = 'Class attributes'(120).
  ENDMETHOD.

  METHOD get_url_base.
    rv_url_base = '/xtt/class-attributes/'.
  ENDMETHOD.

  METHOD get_screen_opt.
    rs_opt-row_count = abap_true.
  ENDMETHOD.

  METHOD set_merge_info.
    " {R-T} in a temaplte. @see get_random_table description
    io_report->get_random_table( IMPORTING et_table = me->t ).

    " Fill {R-A} later. Cannot show TABLE OF OBJECTS
    me->a = _make_string_message( 'Table of objects'(too) ).

    " Document structure
    me->title = 'Title'(tit).

    " Date and time in header and footer
    me->date   = sy-datum.
    me->time   = sy-uzeit.
    " obligatory only for datetime   (;type=datetime)
    CONCATENATE sy-datum sy-uzeit INTO me->datetime.

    io_report->merge_add_one( me ).

    " Make table of objects
    _make_a_obj_table( ).
  ENDMETHOD.

  METHOD _make_a_obj_table.
    " Fill {R-A}
    DATA lt_demo_120_attr TYPE REF TO tt_demo_120_attr.
    CREATE DATA lt_demo_120_attr.
    me->a = lt_demo_120_attr.

    " Convert 'T' table to 'A' table
    FIELD-SYMBOLS <ls_rand_data> TYPE ts_rand_data.
    LOOP AT me->t ASSIGNING <ls_rand_data>.
      DATA lo_attr TYPE REF TO lcl_demo_120_attr.
      CREATE OBJECT lo_attr
        EXPORTING
          is_rand_data = <ls_rand_data>.

      APPEND lo_attr TO lt_demo_120_attr->*.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_templates.
    APPEND 'ZXXT_DEMO_120-XLSX'      TO rt_templates.
    APPEND 'ZXXT_DEMO_120_EXCEL-XML' TO rt_templates.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
CLASS lcl_demo_120_attr IMPLEMENTATION.
  METHOD constructor.
    me->group   = is_rand_data-group.
    me->caption = is_rand_data-caption.
    " Change a little bit from 'T' table
    me->date    = is_rand_data-date + 10.
    " Swap
    me->sum1    = is_rand_data-sum2.
    me->sum2    = is_rand_data-sum1.
  ENDMETHOD.
ENDCLASS.

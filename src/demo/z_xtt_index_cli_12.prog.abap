*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_root_12_attr DEFINITION FINAL
     FRIENDS zcl_xtt_replace_block. " <--- for private fields
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_rand_data TYPE lcl_main=>ts_rand_data.

    " All fields are private!
  PRIVATE SECTION.
    DATA:
      group   TYPE string,
      caption TYPE string,
      date    TYPE d,
      sum1    TYPE bf_rbetr,
      sum2    TYPE bf_rbetr.
ENDCLASS.

CLASS lcl_root_12 DEFINITION FINAL.
  PUBLIC SECTION.
    " INTERFACES if_serializable_object.

    " Public fields are always accessible
    DATA:
      title    TYPE string,
      t        TYPE lcl_main=>tt_rand_data, " internal flat table ( In template {R-T} )
      a        TYPE STANDARD TABLE OF REF TO lcl_root_12_attr,
      date     TYPE d,            " 8
      time     TYPE t,            " 6
      datetime TYPE char14.       " date(8) + time(6)
ENDCLASS.

" Just data
CLASS lcl_root_12 IMPLEMENTATION.
ENDCLASS.

CLASS lcl_root_12_attr IMPLEMENTATION.
  METHOD constructor.
    me->group   = is_rand_data-group.
    me->caption = is_rand_data-caption.
    me->date    = is_rand_data-date + 10.
    " Swap
    me->sum1    = is_rand_data-sum2.
    me->sum2    = is_rand_data-sum1.
  ENDMETHOD.
ENDCLASS.

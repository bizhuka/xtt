*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

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

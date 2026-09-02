*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_demo_120_attr DEFINITION FINAL FRIENDS zcl_xtt_replace_block. " <--- for private fields
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_rand_data TYPE zcl_xtt_demo=>ts_rand_data.

    " All fields are private!
  PRIVATE SECTION.
    DATA:
      group   TYPE string,                                  "#EC NEEDED
      caption TYPE string,                                  "#EC NEEDED
      date    TYPE d,                                       "#EC NEEDED
      sum1    TYPE bf_rbetr,                                "#EC NEEDED
      sum2    TYPE bf_rbetr.                                "#EC NEEDED
ENDCLASS.

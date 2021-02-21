*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

" TODO global
CLASS lcl_xtt_styles DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      tt_collection TYPE stringtab.

    CONSTANTS:
      BEGIN OF c_theme,
        black TYPE char1 VALUE '1',
        blue  TYPE char1 VALUE '4',
        gray  TYPE char1 VALUE '6',
      END OF c_theme,

      BEGIN OF c_border,
        none TYPE string VALUE `<border><left/><right/><top/><bottom/><diagonal/></border>`,
        all  TYPE string VALUE `<border><left_s_><color indexed="64"/></left><right_s_><color indexed="64"/></right><top_s_><color indexed="64"/></top><bottom_s_><color indexed="64"/></bottom><diagonal/></border>`,
        beg  TYPE string VALUE `<border><left_s_><color indexed="64"/></left><right/><top_s_><color indexed="64"/></top><bottom_s_><color indexed="64"/></bottom><diagonal/></border>`,
        mid  TYPE string VALUE `<border><left/><right/><top_s_><color indexed="64"/></top><bottom_s_><color indexed="64"/></bottom><diagonal/></border>`,
        end  TYPE string VALUE `<border><left/><right_s_><color indexed="64"/></right><top_s_><color indexed="64"/></top><bottom_s_><color indexed="64"/></bottom><diagonal/></border>`,
      END OF c_border.

    DATA:
      t_styles   TYPE tt_collection,

      t_num_fmts TYPE tt_collection,
      t_fonts    TYPE tt_collection,
      t_fills    TYPE tt_collection,
      t_borders  TYPE tt_collection.

    METHODS:
      read_all
        IMPORTING
          io_zip TYPE REF TO cl_abap_zip,

      save_all
        IMPORTING
          io_zip TYPE REF TO cl_abap_zip,

      _read_collection
        IMPORTING
                  iv_xml               TYPE string
                  iv_name              TYPE csequence
        RETURNING VALUE(rt_collection) TYPE tt_collection,

      _paste
        IMPORTING
          io_xml        TYPE REF TO zcl_xtt_xml_updater
          it_collection TYPE tt_collection
          iv_root_tag   TYPE string
          iv_relat_tag  TYPE string OPTIONAL,

      _index_of
        IMPORTING
          iv_tag   TYPE string
        EXPORTING
          ev_index TYPE string
        CHANGING
          ct_table TYPE tt_collection,

      get_style_index
        IMPORTING
                  iv_num_fmt_id   TYPE string DEFAULT `0`
                  iv_font_id      TYPE string DEFAULT `0`
                  iv_fill_id      TYPE string DEFAULT `0`
                  iv_border_id    TYPE string DEFAULT `0`
                  iv_align_v      TYPE string OPTIONAL
                  iv_align_h      TYPE string OPTIONAL
        RETURNING VALUE(rv_index) TYPE string,

      get_num_format_index
        IMPORTING
                  iv_number       TYPE abap_bool OPTIONAL
                  iv_time         TYPE abap_bool OPTIONAL
                  iv_date         TYPE abap_bool OPTIONAL
        RETURNING VALUE(rv_index) TYPE string,

      _get_date_format_tag_end
        RETURNING VALUE(rv_tag_end) TYPE string,

      get_font_index
        IMPORTING
                  iv_size         TYPE i         DEFAULT 11
                  iv_name         TYPE csequence DEFAULT `Calibri` "#EC NOTEXT
                  iv_color        TYPE csequence DEFAULT c_theme-black
                  iv_opt          TYPE csequence OPTIONAL
        RETURNING VALUE(rv_index) TYPE string,

      get_fill_index
        IMPORTING
                  iv_percent      TYPE i         DEFAULT 0
                  iv_color        TYPE csequence DEFAULT c_theme-blue
        RETURNING VALUE(rv_index) TYPE string,

      get_border_index
        IMPORTING
                  iv_kind         TYPE string    DEFAULT c_border-all
                  iv_style        TYPE string    DEFAULT `thin` "#EC NOTEXT
        RETURNING VALUE(rv_index) TYPE string.
ENDCLASS.

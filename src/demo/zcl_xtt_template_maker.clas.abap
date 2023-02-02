class ZCL_XTT_TEMPLATE_MAKER definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  class-methods GET_STRUCTURE
    importing
      !IS_ROOT type ANY
      !IV_AS_CHAR type ABAP_BOOL default ABAP_TRUE
      !IV_PREFIX type STRING optional
    returning
      value(RR_ROOT) type ref to DATA .
  class-methods GET_ONE_LINE_TABLE
    importing
      !IT_TABLE type ANY TABLE
      !IV_AS_CHAR type ABAP_BOOL default ABAP_TRUE
      !IV_NAME type STRING default 'T'
    returning
      value(RR_TABLE) type ref to DATA .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_XTT_TEMPLATE_MAKER IMPLEMENTATION.


  METHOD get_one_line_table.
    DATA lr_src_struct TYPE REF TO data.
    CREATE DATA lr_src_struct LIKE LINE OF it_table.

    FIELD-SYMBOLS <ls_src_struct> TYPE any.
    ASSIGN lr_src_struct->* TO <ls_src_struct>.

    DATA lv_prefix TYPE string.
    CONCATENATE iv_name `-` INTO lv_prefix.

    DATA lr_dest_struct TYPE REF TO data.
    lr_dest_struct = get_structure( is_root    = <ls_src_struct>
                                    iv_as_char = iv_as_char
                                    iv_prefix  = lv_prefix ).

    FIELD-SYMBOLS <ls_dest_struct> TYPE any.
    ASSIGN lr_dest_struct->* TO <ls_dest_struct>.

    " Add created line
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    CREATE DATA rr_table LIKE STANDARD TABLE OF <ls_dest_struct>.
    ASSIGN rr_table->* TO <lt_table>.
    APPEND <ls_dest_struct> TO <lt_table>.
  ENDMETHOD.


  METHOD get_structure.
    DATA lo_struct TYPE REF TO cl_abap_structdescr.
    lo_struct ?= cl_abap_structdescr=>describe_by_data( is_root ).

    DATA lt_comp TYPE abap_compdescr_tab.
    DATA lr_comp TYPE REF TO abap_compdescr.
    lt_comp = lo_struct->components[].

    DATA lt_res TYPE abap_component_tab.
    DATA ls_res TYPE abap_componentdescr.
    LOOP AT lt_comp REFERENCE INTO lr_comp.
      ls_res-name = lr_comp->name.
      IF iv_as_char = abap_true.
        ls_res-type = cl_abap_elemdescr=>get_c( p_length = 100 ).
      ELSE.
        ls_res-type = cl_abap_elemdescr=>get_string( ).
      ENDIF.
      INSERT ls_res INTO TABLE lt_res[].
    ENDLOOP.

    lo_struct = cl_abap_structdescr=>create( lt_res ).
    CREATE DATA rr_root TYPE HANDLE lo_struct.

    FIELD-SYMBOLS <ls_root> TYPE any.
    ASSIGN rr_root->* TO <ls_root>.

    LOOP AT lt_comp REFERENCE INTO lr_comp.
      FIELD-SYMBOLS <lv_field> TYPE any.
      ASSIGN COMPONENT lr_comp->name OF STRUCTURE <ls_root> TO <lv_field>.
      CONCATENATE `{R-` iv_prefix lr_comp->name `}` INTO <lv_field>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

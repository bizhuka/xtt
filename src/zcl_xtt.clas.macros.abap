*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE get_no_warning.
  DATA lv_no_warning TYPE abap_bool.
  IF mv_is_prod = abap_true OR &1 = abap_true.
     lv_no_warning = abap_true.
  ENDIF.
END-OF-DEFINITION.

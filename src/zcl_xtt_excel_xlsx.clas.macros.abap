" Positive int only!. The fastest way. The same for chars and string
DEFINE int_to_text.
  DATA &1_txt TYPE string.
   &1_txt = &1.
  CONDENSE &1_txt.
END-OF-DEFINITION.

DEFINE int_2_text.
  &2 = &1.
  condense &2.
END-OF-DEFINITION.

DEFINE add_attr.
  lv_text = <ls_item>-&2.
  IF lv_text IS NOT INITIAL.
    CONDENSE lv_text.
    CONCATENATE &1 ` ` &3 `="` lv_text `"` INTO &1 RESPECTING BLANKS.
  ENDIF.
END-OF-DEFINITION.

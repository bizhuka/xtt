*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE int_2_text.
  &2 = &1.
  condense &2.
END-OF-DEFINITION.

" with new name
DEFINE create_tr_cache.
  DATA lv_tabix TYPE sytabix.
  lv_tabix = sy-tabix.

  CREATE DATA &1.
  int_2_text lv_tabix &1->tr_id.
  CONCATENATE iv_tr_id `#` &1->tr_id `#` INTO &1->tr_id.
END-OF-DEFINITION.

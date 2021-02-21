*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE int_to_text.
  DATA &1_txt TYPE string.
   &1_txt = &1.
  CONDENSE &1_txt.
END-OF-DEFINITION.

DEFINE int_2_text.
  &2 = &1.
  condense &2.
END-OF-DEFINITION.

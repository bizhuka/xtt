*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

DATA go_report TYPE REF TO lcl_report.

INITIALIZATION.
  CREATE OBJECT go_report.

AT SELECTION-SCREEN OUTPUT.
  go_report->pbo( ).

START-OF-SELECTION.
  go_report->start_of_selection( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  go_report->f4_full_path(
   EXPORTING
    iv_title    = 'Select fullpath'(f4f)
   CHANGING
    cv_fullpath = p_path ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_r_path.
  go_report->f4_dir_browse(
   EXPORTING
    iv_title = 'Select root directory'(f4p)
   CHANGING
    cv_path  = p_r_path ).

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM test USING i_exa    LIKE p_exa
                i_action LIKE p_action
                i_templ  LIKE p_templ.
  p_exa    = i_exa.
  p_action = i_action.
  p_templ  = i_templ.

  p_r_cnt  = 25.
  p_c_cnt  = 10.
  p_b_cnt  = 3.
  p_open   = abap_true.
  p_user   = sy-uname.

  CREATE OBJECT go_report.
  go_report->start_of_selection( ).
ENDFORM.

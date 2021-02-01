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

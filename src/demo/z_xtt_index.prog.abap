**********************************************************************
*  XTT - Xml template toolkit examples
*  @see https://github.com/bizhuka/xtt
*  @version: 1.0
**********************************************************************
REPORT z_xtt_index.

INCLUDE z_xtt_index_scr.
INCLUDE z_xtt_index_cld.
INCLUDE z_xtt_index_cli.


**********************************************************************
DATA:
 go_main TYPE REF TO cl_main. "#EC NEEDED

INITIALIZATION.
  CREATE OBJECT go_main.

AT SELECTION-SCREEN OUTPUT.
  go_main->pbo( ).

AT SELECTION-SCREEN.
  go_main->pai( sy-ucomm ).

START-OF-SELECTION.
  go_main->start_of_selection( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  go_main->f4_full_path(
   EXPORTING
    iv_title    = 'Select fullpath'(f4f)
   CHANGING
    cv_fullpath = p_path ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_r_path.
  go_main->f4_dir_browse(
   EXPORTING
    iv_title = 'Select root directory'(f4p)
   CHANGING
    cv_path  = p_r_path ).

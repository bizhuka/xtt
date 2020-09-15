*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

" Example option
SELECTION-SCREEN BEGIN OF BLOCK bl_example WITH FRAME TITLE TEXT-exa.
PARAMETERS:
  " What example to launch
  p_exa   TYPE char5 AS LISTBOX VISIBLE LENGTH 80 OBLIGATORY MODIF ID exa USER-COMMAND update_scr
   DEFAULT '06-03'
  ,

  " Row count
  p_r_cnt TYPE int4 DEFAULT 15 MODIF ID oth,

  " Column count
  p_c_cnt TYPE numc2 DEFAULT 36 MODIF ID oth,

  " Block count
  p_b_cnt TYPE int4 DEFAULT 3  MODIF ID oth,

  " Compress to zip file
  p_zip   AS CHECKBOX MODIF ID oth,

  p_size  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF   BLOCK bl_example.

" Template or data stucture or report itself
SELECTION-SCREEN BEGIN OF BLOCK bl_action WITH FRAME TITLE TEXT-res.
PARAMETERS:
  p_repo RADIOBUTTON GROUP res MODIF ID res USER-COMMAND update_scr,
  p_temp RADIOBUTTON GROUP res MODIF ID res,
  p_stru RADIOBUTTON GROUP res MODIF ID res.
SELECTION-SCREEN END OF   BLOCK bl_action.


SELECTION-SCREEN BEGIN OF BLOCK bl_method WITH FRAME TITLE TEXT-met.
* download method
PARAMETERS:
  p_dwnl RADIOBUTTON GROUP meth MODIF ID met USER-COMMAND update_scr DEFAULT 'X',
  p_path TYPE string  MODIF ID dwn,
  p_open AS CHECKBOX  MODIF ID dwn DEFAULT 'X'.
SELECTION-SCREEN SKIP 1.

* show method
PARAMETERS:
  p_show  RADIOBUTTON GROUP meth MODIF ID met.
SELECTION-SCREEN SKIP 1.

* send method
PARAMETERS:
  p_send  RADIOBUTTON GROUP meth   MODIF ID met,
  p_email TYPE adr6-smtp_addr      MODIF ID snd,
  p_user  TYPE syuname             MODIF ID snd DEFAULT sy-uname,
  p_title TYPE so_obj_des          MODIF ID snd DEFAULT 'Letter subject', "#EC NOTEXT
  p_text  TYPE text255             MODIF ID snd DEFAULT 'The body of the letter'. "#EC NOTEXT
SELECTION-SCREEN END OF   BLOCK bl_method.

**********************************************************************
" Show sub folders of
**********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 1010 AS SUBSCREEN.
PARAMETERS:
  p_r_path TYPE stringval,
  p_r_many AS CHECKBOX. " DEFAULT 'X'.
SELECTION-SCREEN END OF SCREEN 1010.

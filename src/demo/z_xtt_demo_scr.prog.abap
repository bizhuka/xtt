*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

TYPE-POOLS:
 abap,
 vrm,
 cntb.

" Example option
SELECTION-SCREEN BEGIN OF BLOCK bl_example WITH FRAME TITLE text-exa.
PARAMETERS:
  " What example to launch
  p_exa   TYPE char3 AS LISTBOX VISIBLE LENGTH 80 OBLIGATORY MODIF ID exa USER-COMMAND update_scr
   DEFAULT '010',

  " Row count
  p_r_cnt TYPE int4 DEFAULT 255 MODIF ID oth,

  " Column count
  p_c_cnt TYPE numc2 DEFAULT 36 MODIF ID oth,

  " Block count
  p_b_cnt TYPE int4 DEFAULT 3  MODIF ID oth,

  " Compress to zip file
  p_zip   AS CHECKBOX MODIF ID oth DEFAULT ' ',

  " Set image size
  img_size  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF   BLOCK bl_example.


**********************************************************************
**********************************************************************

* download method
SELECTION-SCREEN BEGIN OF: SCREEN 1010 AS SUBSCREEN,
                           BLOCK bl_dwn WITH FRAME TITLE text-dwn.
PARAMETERS:
  p_dtempl TYPE vrm_value-key AS LISTBOX VISIBLE LENGTH 80,
  p_path   TYPE text255,
  p_open   AS CHECKBOX DEFAULT 'X',
  p_stop   AS CHECKBOX DEFAULT ' ',
  " download to app server
  p_appser NO-DISPLAY. " AS CHECKBOX .

SELECTION-SCREEN END OF: BLOCK bl_dwn,
                         SCREEN 1010.

**********************************************************************
**********************************************************************


* send method
SELECTION-SCREEN BEGIN OF: SCREEN 1020 AS SUBSCREEN,
                           BLOCK bl_snd WITH FRAME TITLE text-snd.
PARAMETERS:
  p_stempl TYPE vrm_value-key AS LISTBOX VISIBLE LENGTH 80,
  p_email TYPE adr6-smtp_addr,
  p_user  TYPE syuname             DEFAULT sy-uname,
  p_title TYPE so_obj_des          DEFAULT 'Letter subject', "#EC NOTEXT
  p_text  TYPE text255             DEFAULT 'The body of the letter'. "#EC NOTEXT
SELECTION-SCREEN END OF: BLOCK bl_snd,
                         SCREEN 1020.

**********************************************************************
**********************************************************************


* example 060 - Show sub folders of
SELECTION-SCREEN BEGIN OF SCREEN 2010 AS SUBSCREEN.
PARAMETERS:
  p_r_path TYPE stringval,
  p_r_many AS CHECKBOX. " DEFAULT 'X'.
SELECTION-SCREEN END OF SCREEN 2010.

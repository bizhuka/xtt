*&---------------------------------------------------------------------*
*  XTT demo reports
*  @see https://bizhuka.github.io/xtt/
*  @version: 2.0
*&---------------------------------------------------------------------*
REPORT  z_xtt_demo.

INCLUDE z_xtt_demo_scr.
INCLUDE z_xtt_demo_cld.
INCLUDE z_xtt_demo_rep.
INCLUDE z_xtt_demo_def.

" usually you could create a report in a one piece of line
" NEW zcl_xtt_excel_xlsx( NEW zcl_xtt_file_smw0( 'ZZZ.XLSX' ) )->merge( ls_root )->download( ).
" examples below fill 'LS_ROOT' in the SET_MERGE_INFO( ) method

INCLUDE z_xtt_demo_n010.

INCLUDE z_xtt_demo_n020.
INCLUDE z_xtt_demo_n021.
INCLUDE z_xtt_demo_n022.

INCLUDE z_xtt_demo_n030.
INCLUDE z_xtt_demo_n040.

INCLUDE z_xtt_demo_n050.
INCLUDE z_xtt_demo_n051.
INCLUDE z_xtt_demo_n052.

INCLUDE z_xtt_demo_n060.
INCLUDE z_xtt_demo_n070.
INCLUDE z_xtt_demo_n080.
INCLUDE z_xtt_demo_n090.
INCLUDE z_xtt_demo_n091.

INCLUDE z_xtt_demo_n100.
INCLUDE z_xtt_demo_n110.

INCLUDE z_xtt_demo_n120.
INCLUDE z_xtt_demo_n130.
INCLUDE z_xtt_demo_n131.
INCLUDE z_xtt_demo_n140.
INCLUDE z_xtt_demo_n150.

INCLUDE z_xtt_demo_test.
INCLUDE z_xtt_demo_evt.

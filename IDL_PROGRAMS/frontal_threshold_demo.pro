; $ID:	FRONTAL_THRESHOLD_DEMO.PRO,	2023-07-07-16,	USER-KJWH	$
  PRO FRONTAL_THRESHOLD_DEMO

;+
; NAME:
;   FRONTAL_THRESHOLD_DEMO
;
; PURPOSE:
;   $PURPOSE$
;
; PROJECT:
;   FRONTAL_METRICS
;
; CALLING SEQUENCE:
;   Result = FRONTAL_THRESHOLD_DEMO($Parameter1$, $Parameter2$, $Keyword=Keyword$, ...)
;
; REQUIRED INPUTS:
;   Parm1.......... Describe the positional input parameters here. 
;
; OPTIONAL INPUTS:
;   Parm2.......... Describe optional inputs here. If none, delete this section.
;
; KEYWORD PARAMETERS:
;   KEY1........... Document keyword parameters like this. Note that the keyword is shown in ALL CAPS!
;
; OUTPUTS:
;   OUTPUT.......... Decribe the output of this program or function
;
; OPTIONAL OUTPUTS:
;   None
;
; COMMON BLOCKS: 
;   None
;
; SIDE EFFECTS:  
;   None
;
; RESTRICTIONS:  
;   None
;
; EXAMPLE:
; 
;
; NOTES:
;   $Citations or any other useful notes$
;   
; COPYRIGHT: 
; Copyright (C) 2023, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on July 07, 2023 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Jul 07, 2023 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'FRONTAL_THRESHOLD_DEMO'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  F = GET_FILES('ACSPO', PRODS='GRAD_SST-BOA')
  STACKED_2PNGS, F[-1], PRODS='GRAD_SST', MAP_OUT='NES'
  STOP
  
  TEST_DIR = !S.FRONTAL_METRICS + 'IDL_TEST' + SL & DIR_TEST, TEST_DIR
  
  TESTDATA = !S.FRONTAL_METRICS + 'csv_files/simulated_grad_mag.csv'
  TDAT = CSV_READ(TESTDATA)
  TDAT = REFORM(TDAT.X,10,10)
  PLUN, [], 'Input grad mag values', 1
  PRINT, REFORM(ROUNDS(TDAT,2),10,10)
  TPNG = TEST_DIR + 'simulated_grad_mag_input_data.png'
  IF FILE_MAKE(TESTDATA,TPNG) THEN IMGR,TDAT,PNG=TPNG, DELAY=1
  
  THOLD = FRONTS_THRESHOLD(TDAT)
  PLUN, [], 'Threshold values', 1
  PRINT, REFORM(ROUNDS(THOLD,2),10,10)
  HPNG = TEST_DIR + 'simulated_grad_mag_threshold.png'
  IF FILE_MAKE(TESTDATA,HPNG) THEN IMGR,THOLD,PNG=HPNG, DELAY=1
  
  TFRONT = TDAT < THOLD
  
  STOP


END ; ***************** End of FRONTAL_THRESHOLD_DEMO *****************

; $ID:	FRONTAL_THRESHOLD_DEMO.PRO,	2023-09-19-09,	USER-KJWH	$
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
;   OUTPUT.......... Describe the output of this program or function
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
  
  TEST_DIR = !S.FRONTAL_METRICS + 'IDL_TEST' + SL & DIR_TEST, TEST_DIR
  SST_SUBSET = !S.FRONTAL_METRICS + 'csv_files/sst_test_data.csv'
  GRAD_SST_SUBSET = !S.FRONTAL_METRICS + 'csv_files/grad_sst_test_data.csv'
  SIMDATA = !S.FRONTAL_METRICS + 'csv_files/simulated_grad_mag.csv'
  SAMPLE_GRAD_SST = !S.FRONTAL_METRICS + 'csv_files/sample_grad_sst.sav'

  
  MAKE_SUBSET_DATA = 0
  IF KEYWORD_SET(MAKE_SUBSET_DATA) THEN BEGIN
    F = GET_FILES('ACSPO', PRODS='GRAD_SST-BOA')
    ;STACKED_2PNGS, F[-1], PRODS='GRAD_SST', MAP_OUT='NES'
    D = STACKED_READ(F[-1])
    ms= maps_remap(D.grad_sst[*,*,38],bins=d.bins, map_in='L3B2',map_out='NES')
    subs = ms[400:499,400:499]
    write_csv,  grad_sst_subset, subs
    imgr, subs, prod='grad_sst', delay=5, PNG=TEST_DIR + 'grad_sst_test_data.png'
    ms= maps_remap(D.sst[*,*,38],bins=d.bins, map_in='L3B2',map_out='NES')
    sstsubs = ms[400:499,400:499]
    write_csv,  sst_subset, sstsubs
    imgr, sstsubs, prod='sst_10_25', delay=5, PNG=TEST_DIR + 'sst_test_data.png'
  ENDIF
  
  
  
  D = FINDGEN(2067,1261)
  TIC
  THOLD = FRONTS_THRESHOLD(d)
  TOC
  STOP
  
  
  
  ; Run the frontal indicators using SAMPLE_GRAD_SST
  ; 
  
  D = STRUCT_READ(SAMPLE_GRAD_SST,STRUCT=STR)
  FRT = FRONT_INDICATORS_MILLER(GRAD_MAG=D.GRAD_SST, GRAD_X=D.GRADSST_X, GRAD_Y=D.GRADSST_Y, TRANSFORM=0, THRESHOLD_BOX=9, PERSISTENCE=0.3, CPERS_ORG=CPERS_ORG)
  
  stop


  
  PROD = 'GRAD_SST_0_1'
  
  TDAT = READ_ASCII(GRAD_SST_SUBSET,delimiter=',')
  TDAT = TDAT.(0)
  PLUN, [], 'Input grad mag values', 1
;  PRINT, REFORM(ROUNDS(TDAT,2),100,100)
  TPNG = TEST_DIR + 'actual_grad_mag_input_data.png'
  IF FILE_MAKE(TESTDATA,TPNG) THEN IMGR,TDAT,PNG=TPNG, PROD=PROD, DELAY=1

  THOLD = FRONTS_THRESHOLD(TDAT)
  PLUN, [], 'Threshold values', 1
;  PRINT, REFORM(ROUNDS(THOLD,2),100,100)
  HPNG = TEST_DIR + 'actual_grad_mag_midrange_threshold.png'
  IF FILE_MAKE(TESTDATA,HPNG) THEN IMGR,THOLD,PNG=HPNG,PROD=PROD, DELAY=1
  
  FRONTS = FLTARR(100,100) ;& FRONTS[*] = MISSINGS(0.0)
  OK = WHERE(THOLD GT TDAT,COUNT)
  IF COUNT GT 0 THEN FRONTS[OK] = 1
  OK = WHERE(THOLD EQ TDAT,COUNT)
  IF COUNT GT 0 THEN FRONTS[OK] = 0.5
  FPNG = TEST_DIR + 'actual_grad_mag_midrange_fronts.png'
  IF FILE_MAKE(TESTDATA,FPNG,OVERWRITE=OVERWRITE) THEN IMGR,FRONTS,PNG=FPNG, DELAY=1

  OK = WHERE(TDAT LT THOLD)
  TCPY = TDAT
  TCPY[OK] = MISSINGS(0.0)
  CPNG = TEST_DIR + 'actual_grad_mag_midrange.png'
  IF FILE_MAKE(TESTDATA,CPNG,OVERWRITE=OVERWRITE) THEN IMGR,TCPY,PROD=PROD,PNG=CPNG, DELAY=1


overwrite=1    
  TMED = MEDIAN(TDAT,25)
  MPNG = TEST_DIR + 'actual_grad_mag_median_threshold.png'
  IF FILE_MAKE(TESTDATA,MPNG) THEN IMGR,TMED,PNG=MPNG, PROD=PROD, DELAY=1

  FRONTS = FLTARR(100,100) ;& FRONTS[*] = MISSINGS(0.0)
  OK = WHERE(TMED GT TDAT,COUNT)
  IF COUNT GT 1 THEN FRONTS[OK] = 1
  OK = WHERE(THOLD EQ TDAT,COUNT)
  IF COUNT GT 0 THEN FRONTS[OK] = 0.5
  MPNG = TEST_DIR + 'actual_grad_mag_fronts_median_w25.png'
  IF FILE_MAKE(TESTDATA,MPNG) THEN IMGR,FRONTS,PNG=MPNG, DELAY=1

  OK = WHERE(TDAT LT TMED)
  TCPY = TDAT
  TCPY[OK] = MISSINGS(0.0)
  CPNG = TEST_DIR + 'actual_grad_mag_median_w25.png'
  IF FILE_MAKE(TESTDATA,CPNG,OVERWRITE=OVERWRITE) THEN IMGR,TCPY,PROD=PROD,PNG=CPNG, DELAY=1


  
  stop
  
  TDAT = CSV_READ(SIMDATA)
  TDAT = REFORM(TDAT.X,10,10)
  PLUN, [], 'Input grad mag values', 1
  PRINT, REFORM(ROUNDS(TDAT,2),10,10)
  TPNG = TEST_DIR + 'simulated_grad_mag_input_data.png'
  IF FILE_MAKE(TESTDATA,TPNG) THEN IMGR,TDAT,PNG=TPNG, DELAY=1
  
  THOLD = FRONTS_THRESHOLD(TDAT)
  PLUN, [], 'Threshold values', 1
  PRINT, REFORM(ROUNDS(THOLD,2),10,10)
  HPNG = TEST_DIR + 'simulated_grad_mag_threshold.png'
  IF FILE_MAKE(TESTDATA,HPNG) THEN IMGR,THOLD,PNG=HPNG, DELAY=1, PROD='NUM_0_1'
  
  TFRONT = TDAT < THOLD
  
  STOP


END ; ***************** End of FRONTAL_THRESHOLD_DEMO *****************

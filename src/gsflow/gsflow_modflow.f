!***********************************************************************
!     GSFLOW module that replaces mf2005.f
!***********************************************************************
      MODULE GSFMODFLOW
!   Local Variables
      INTEGER, PARAMETER :: ITDIM = 80
      INTEGER, SAVE :: Convfail_cnt, Steady_state
      INTEGER, SAVE :: IGRID, KKPER, ICNVG, IBDT(8), NSOL, IOUTS
      INTEGER, SAVE :: KSTP, KKSTP, IERR, Max_iters, Nszchanging
      INTEGER, SAVE :: Mfiter_cnt(ITDIM), Iterations
      INTEGER, SAVE :: Szcheck, Sziters, Logunt, KPER, NCVGERR
      INTEGER, SAVE :: Have_lakes, Max_sziters, Maxgziter
      INTEGER, SAVE, ALLOCATABLE :: Iter_cnt(:)
      REAL, SAVE :: Delt_save
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Stress_dates(:)
      CHARACTER(LEN=68), SAVE :: Versn
      CHARACTER(LEN=14), PARAMETER :: MODNAME = 'gsflow_modflow'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'GSFLOW Integration'
!   Declared Variables
      INTEGER, SAVE :: KKITER
!   Declared Variables from other modules, prms2mf
      INTEGER, SAVE :: Stopcount
!   Control Parameters
      CHARACTER(LEN=256), SAVE :: Modflow_name
      END MODULE GSFMODFLOW

C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- MODFLOW-NWT
!rgn------REVISION NUMBER CHANGED TO BE CONSISTENT WITH NWT RELEASE
!rgn------NEW VERSION NUMBER 1.0.7:  January 15, 2013
C     ******************************************************************
C
      INTEGER FUNCTION gsflow_modflow()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL:: gsfdecl, gsfinit, gsfrun, gsfclean
!***********************************************************************
      gsflow_modflow = 0

      IF ( Process_flag==0 ) THEN
        gsflow_modflow = gsfrun()
      ELSEIF ( Process_flag==1 ) THEN
        gsflow_modflow = gsfdecl()
      ELSEIF ( Process_flag==2 ) THEN
        gsflow_modflow = gsfinit()
      ELSEIF ( Process_flag==3 ) THEN
        gsflow_modflow = gsfclean()
      ENDIF

      END FUNCTION gsflow_modflow

!***********************************************************************
!     gsfdecl - set up parameters for GSFLOW computations
!***********************************************************************
      INTEGER FUNCTION gsfdecl()
!     ------------------------------------------------------------------
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GSFMODFLOW
      ! Model (0=integrated; 1=PRMS-only; 2=MODFLOW-only)
      USE PRMS_MODULE, ONLY: Version_gsflow_modflow, Gsflow_modflow_nc
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: n
!***********************************************************************
      gsfdecl = 1

      Version_gsflow_modflow =
     &'$Id: gsflow_modflow.f 5498 2013-03-14 21:49:27Z rsregan $'
      Versn = Version_gsflow_modflow
      Gsflow_modflow_nc = INDEX( Version_gsflow_modflow, 'Z' )
      n = INDEX( Version_gsflow_modflow, '.f' ) + 1
      IF ( declmodule(Version_gsflow_modflow(6:n), PROCNAME,
     +     Version_gsflow_modflow(n+2:Gsflow_modflow_nc))/=0 ) STOP

! Declared Variables
      IF ( declvar(MODNAME, 'KKITER', 'one', 1, 'integer',
     &     'Current iteration in GSFLOW simulation', 'none',
     &     KKITER)/=0 ) CALL read_error(3, 'KKITER')

      Have_lakes = 0

      gsfdecl = 0
      END FUNCTION gsfdecl

!***********************************************************************
!     gsfinit - Initialize MODFOW module - get parameter values
!***********************************************************************
      INTEGER FUNCTION gsfinit()
!     ------------------------------------------------------------------
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GSFMODFLOW
      USE PRMS_MODULE, ONLY: Model, Versn_gsfprms, Version_soilzone,
     +    Print_debug
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFUZFMODULE, ONLY: Version_uzf, IGSFLOW
      USE GWFSFRMODULE, ONLY: Version_sfr
      USE GWFLAKMODULE, ONLY: Version_lak
      USE PRMS_BASIN, ONLY: Starttime, Endtime, Timestep
      USE PCGN
      IMPLICIT NONE
      INTEGER :: I
      INCLUDE 'openspec.inc'
! Functions
      INTEGER, EXTERNAL :: gsfrun, GET_FUNIT, getparam, control_string
!     INTEGER, EXTERNAL :: GET_KPER
      EXTERNAL READ_STRESS, SET_STRESS_DATES, read_error
      INTRINSIC INDEX, CHAR
! Local Variables
      INTEGER :: INUNIT, MAXUNIT, NC
      LOGICAL :: exists
!     INTEGER :: kkper_new
C
C-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION,VERSION2
!gsf  CHARACTER*40 VERSION,VERSION2,VERSION3
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='1.9.01 5/01/2012')
      PARAMETER (VERSION2='1.0.7 01/15/2013')
!gsf  PARAMETER (VERSION3='0.56.0 05/18/2012')
      PARAMETER (MFVNAM='-NWT')
C
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 FNAME
C
      CHARACTER*4 CUNIT(NIUNIT)
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', 'gfd ', 'GHB ',  !  7
     &           'RCH ', 'SIP ', 'DE4 ', '    ', 'OC  ', 'PCG ', 'lmg ',  ! 14
     &           'gwt ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     &           'LAK ', 'LPF ', 'DIS ', '    ', 'PVAL', '    ', 'HOB ',  ! 28
     &           '    ', '    ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     &           'STOB', 'HUF2', 'CHOB', 'ETS ', 'DRT ', '    ', 'GMG ',  ! 42
     &           'HYD ', 'SFR ', '    ', 'GAGE', 'LVDA', '    ', 'LMT6',  ! 49
     &           'MNW2', 'MNWI', 'MNW1', 'KDEP', 'SUB ', 'UZF ', 'gwm ',  ! 56
     &           'SWT ', 'cfp ', 'PCGN', '    ', '    ', 'UPW ', 'NWT ',  ! 63
     &           87*'    '/
C     ------------------------------------------------------------------
!rsr ?? what should IOUTS be
      IOUTS = 432
      IGRID=1
      NSOL=1
      gsfinit = 1
C
C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      WRITE (*, 3)
    3 FORMAT(///, 30X,
     &       'U.S. Geological Survey', /, 12X,
     &       'Coupled Groundwater and Surface-water FLOW model',
     &       ' (GSFLOW)', /,
     &       29X, 'Version 1.1.6 03/20/2013', //, 8X,
     &       'An integration of the Precipitation-Runoff Modeling',
     &       ' System (PRMS)', /, 8X,
     &       'and the Modular Groundwater Model (MODFLOW-NWT',
     &       ' and MODFLOW-2005)')
      WRITE (*,1) MFVNAM(:4), VERSION2(:16), VERSION(:16)
!gsf  WRITE (*,1) MFVNAM,VERSION2,VERSION3
    1 FORMAT (/,34X,'MODFLOW',A,/,
     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &' GROUNDWATER-FLOW MODEL',/,29X,'WITH NEWTON FORMULATION',
     &  /,29X,'Version ',A/,20X,'BASED ON MODFLOW-2005 Version ',A/)
!gsf &  /,20X,'SWR1 Version ',A/)
      INUNIT = 99
      NCVGERR=0
      IF ( Model.NE.1 ) WRITE (*, 8)
      IF ( Model/=2 ) THEN
        WRITE ( *, 5 ) Versn_gsfprms(20:34)
        WRITE ( *, 9 )
      ENDIF
    5 FORMAT (39X, 'PRMS', /, 11X,
     &    'U.S. GEOLOGICAL SURVEY PRECIPITATION-RUNOFF MODELING SYSTEM',
     &    /, 24X, 'Version 3.0.5 Tag: ', A, /)
      INQUIRE (FILE='modflow.bf',EXIST=exists)
      IF ( exists ) THEN
        OPEN (UNIT=377, FILE='modflow.bf', STATUS='OLD')
        PRINT *, 'Using modflow.bf'
        READ (377, '(A)') FNAME
        IF ( FNAME(:1).EQ.' ' ) STOP 'INVALID modflow.bf'
        CLOSE (377)
        NC = 0
      ELSE
        IF ( control_string(Modflow_name, 'modflow_name').NE.0 ) RETURN
        FNAME = Modflow_name(:200)
        IF ( FNAME(:1).EQ.' ' ) FNAME = 'modflow.nam'
        NC = INDEX(FNAME,CHAR(0))
        IF ( NC.GT.1 ) FNAME(NC:200) = ' '
C
C3------GET THE NAME OF THE NAME FILE
!gsf  CALL GETNAMFIL(FNAME)
      ENDIF
      MAXUNIT= INUNIT
C
C4------OPEN NAME FILE.
      INQUIRE (FILE=FNAME,EXIST=exists)
      IF ( .NOT.exists ) THEN
        PRINT '(A, A, /)', 'MODFLOW Name File does not exist: ', FNAME
        RETURN
      ENDIF
      OPEN (UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
      NC=INDEX(FNAME,' ')
      IF ( NC.GT.1 ) THEN
      WRITE(*,490)' Using NAME file: ',FNAME(1:NC)
      ELSE
        WRITE (*, 490) ' Using NAME file: ', FNAME
      ENDIF
  490 FORMAT(A,A)
C
C5------Get current date and time, assign to IBDT, and write to screen
      CALL DATE_AND_TIME(VALUES=IBDT)
      IF ( Print_debug>-1 )
     &     WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2,/)
C
C6------ALLOCATE AND READ (AR) PROCEDURE
      IGRID=1
      NSOL=1
      CALL GWF2BAS7AR(INUNIT,CUNIT,VERSION,VERSION2,24,31,32,MAXUNIT,
     1                IGRID,12,HEADNG,26,MFVNAM)
      IF(IUNIT(50).GT.0 .AND. IUNIT(52).GT.0) THEN
        WRITE(IOUT,'(1X,/,1X,A)')
     1  'MNW1 and MNW2 cannot both be active in the same simulation'
        CALL USTOP(' ')
      END IF
      IF ( IUNIT(22).GT.0 ) Have_lakes = 1
      IF(IUNIT(1).GT.0) CALL GWF2BCF7AR(IUNIT(1),IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7AR(IUNIT(23),IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7AR(IUNIT(37),IUNIT(47),
     1                                     IUNIT(53),IGRID)
! Allocate arrays for Newton Solver
      IF(IUNIT(63).GT.0) CALL GWF2NWT1AR(IUNIT(63),MXITER, 
     1                                   IUNIT(22),IGRID)
      IF(IUNIT(62).GT.0) CALL GWF2UPW1AR(IUNIT(62),IGRID)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7AR(IUNIT(2),IUNIT(63),IGRID)
!gsf  IF(IUNIT(3).GT.0) CALL GWF2DRN7AR(IUNIT(3),IGRID)
!gsf  IF(IUNIT(4).GT.0) CALL GWF2RIV7AR(IUNIT(4),IGRID)
!gsf  IF(IUNIT(5).GT.0) CALL GWF2EVT7AR(IUNIT(5),IGRID)
      IF(IUNIT(7).GT.0) CALL GWF2GHB7AR(IUNIT(7),IGRID)
!gsf  IF(IUNIT(8).GT.0) CALL GWF2RCH7AR(IUNIT(8),IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7AR(IUNIT(16),IGRID)
!gsf  IF(IUNIT(17).GT.0) CALL GWF2RES7AR(IUNIT(17),IGRID)
!gsf  IF(IUNIT(18).GT.0) CALL GWF2STR7AR(IUNIT(18),IGRID)
!gsf  IF(IUNIT(19).GT.0) CALL GWF2IBS7AR(IUNIT(19),IUNIT(54),IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7AR(IUNIT(20),IGRID)
      IF(IUNIT(21).GT.0) CALL GWF2HFB7AR(IUNIT(21),IGRID)
! Modify conductance for HFB when using UPW.
      IF ( IUNIT(62).GT.0 ) THEN
        IF(IUNIT(21).GT.0) CALL GWF2HFB7UPW(IGRID)
      END IF
      IF(IUNIT(44).GT.0) CALL GWF2SFR7AR(IUNIT(44),IUNIT(1),IUNIT(23),
     1                           IUNIT(37),IUNIT(15),NSOL,IOUTS,
     2                           IUNIT(62),IUNIT(63),IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1),
     1                                   IUNIT(23),IUNIT(37),
     2                                   IUNIT(63),IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0) CALL GWF2LAK7AR(
     1             IUNIT(22),IUNIT(44),IUNIT(15),IUNIT(55),NSOL,IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7AR(IUNIT(46),IUNIT(44),
     1                                     IUNIT(22),IGRID)
!gsf  IF(IUNIT(39).GT.0) CALL GWF2ETS7AR(IUNIT(39),IGRID)
!gsf  IF(IUNIT(40).GT.0) CALL GWF2DRT7AR(IUNIT(40),IGRID)
!gsf  IF(IUNIT(54).GT.0) CALL GWF2SUB7AR(IUNIT(54),IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7AR(IUNIT(9),MXITER,IGRID)
      IF(IUNIT(10).GT.0) CALL DE47AR(IUNIT(10),MXITER,IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7AR(IUNIT(13),MXITER,IGRID)
c      IF(IUNIT(14).GT.0) CALL LMG7AR(IUNIT(14),MXITER,IGRID)
      IF(IUNIT(42).GT.0) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(59).GT.0) CALL PCGN2AR(IUNIT(59),IFREFM,MXITER,IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27AR(IUNIT(50),IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7AR(IUNIT(51),IUNIT(50),IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17AR(IUNIT(52),IUNIT(9),
     1                     IUNIT(10),IUNIT(63),0,IUNIT(13),
     2                     0,IUNIT(42),FNAME,IGRID)
!gsf  IF(IUNIT(57).GT.0) CALL GWF2SWT7AR(IUNIT(57),IGRID)
!gsf  IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7AR(IUNIT(43),IGRID)
!gsf  IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0)
!gsf 1                   CALL GWF2HYD7IBS7AR(IUNIT(43),IGRID)
!gsf  IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0)
!gsf 1                   CALL GWF2HYD7SUB7AR(IUNIT(43),IGRID)
!gsf  IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
!gsf 1                   CALL GWF2HYD7STR7AR(IUNIT(43),IGRID)
!gsf  IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
!gsf 1                   CALL GWF2HYD7SFR7AR(IUNIT(43),IGRID)
!gsf  IF(IUNIT(49).GT.0) CALL LMT7BAS7AR(INUNIT,CUNIT,IGRID)
C
C  Observation allocate and read
      CALL OBS2BAS7AR(IUNIT(28),IGRID)
!gsf  IF(IUNIT(33).GT.0) CALL OBS2DRN7AR(IUNIT(33),IUNIT(3),IGRID)
!gsf  IF(IUNIT(34).GT.0) CALL OBS2RIV7AR(IUNIT(34),IUNIT(4),IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7AR(IUNIT(35),IUNIT(7),IGRID)
!gsf  IF(IUNIT(36).GT.0) CALL OBS2STR7AR(IUNIT(36),IUNIT(18),IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7AR(IUNIT(38),IGRID)
      Logunt = GET_FUNIT()
      OPEN ( UNIT=Logunt, FILE='gsflow.log' )
      WRITE (Logunt, 3)
      WRITE ( Logunt, 1 ) MFVNAM(:4), VERSION2(:17), VERSION(:16)
      IF ( Model/=1 ) WRITE ( Logunt, 8 )
    8 FORMAT (30X, 'Processes: GWF and OBS', /, 7X,
     &        'Packages: BAS, BCF, CHD, DE4, FHB, GAG, GHB,',
     &        ' GMG, HFB, HUF LAK', /, 17X, 'LPF, MNW1, MNW2, NWT,',
     &        ' PCG, PCGN, SFR, SIP, UPW, UZF, WEL', //)
    9 FORMAT (7X, 'Modules: ',
     &        'basin, basin_sum, cascade, ccsolrad, climate_hru',
     &        /, 16X,
     &        'ddsolrad, gwflow, intcp, map_results, obs',
     &        /, 16X,
     &        'potet_hamon, potet_jh, potet_pan, precip_1sta',
     &        /, 16X,
     &        'precip_dist2, precip_laps, snowcomp, soilzone, soltab',
     &        /, 16X,
     &        'srunoff_carea, srunoff_smidx, strmflow, subbasin',
     &        /, 16X,
     &      'temp_1sta, temp_dist2, temp_laps, transp_tindex, xyz_dist',
     &        //)
      IF ( Model/=2 ) THEN
        WRITE (Logunt, 5) Versn_gsfprms(20:34)
        WRITE (Logunt, 9)
      ENDIF
      WRITE (Logunt, 490) ' Using NAME file: ', FNAME(:NC)
      WRITE (Logunt, 15) Version_uzf, Version_sfr, Version_lak
      WRITE (*, 15) Version_uzf, Version_sfr, Version_lak
      IF ( Model/=2 ) THEN
        WRITE ( Logunt, 16 ) Versn_gsfprms(20:34), Version_soilzone
        WRITE ( *, 16 ) Version_soilzone(:72)
      ELSE
        PRINT *, ' '
      ENDIF
   15 FORMAT (/, 2(A,/), A)
   16 FORMAT (2(A,/))
      WRITE (Logunt, 2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)

      IF ( Model.NE.2 ) THEN
        Sziters = 0
        IF ( Timestep==0 ) KPER = 0
        IF ( .NOT.ALLOCATED(Stress_dates)) 
     &       ALLOCATE (Stress_dates(NPER+1))
        CALL SET_STRESS_DATES(Starttime)
      ENDIF
C
C7------SIMULATE EACH STRESS PERIOD.
!gsf  DO 100 KPER = 1, NPER
      KPER = 1
        KKPER = KPER
      CALL READ_STRESS()
      KSTP = 0

! DANGER markstro set this because
      Delt_save = DELT
      IF ( ISSFLG(1).EQ.1 ) DELT = 1.0

      Nszchanging = 0
      Convfail_cnt = 0
      Steady_state = 0
      IF ( ISSFLG(1).EQ.1 ) THEN
        Steady_state = 1
        IF ( gsfrun().NE.0 ) RETURN
        Steady_state = 0
        IF ( IUNIT(55)/=0 ) IGSFLOW = 1
        IF ( ICNVG.EQ.0 ) THEN
          PRINT *, 'Steady state simulation did not converge', KKITER
          WRITE (Logunt, *) 'Steady state simulation did not converge',
     &                      KKITER
        ELSE
          PRINT *, 'Steady state simulation successful, used:', KKITER,
     &             'iterations'
          WRITE (Logunt, *) 'Steady state simulation successful, used:',
     &                      KKITER, 'iterations'
        ENDIF
      ENDIF
      Max_iters = 0
      Max_sziters = 0
      Iterations = 0
      Mfiter_cnt = 0
      IF ( Model.NE.2 ) THEN
        WRITE (*, 4) ' Simulation time period:', Starttime, ' -',
     &               Endtime
        WRITE (Logunt, 4) ' Simulation time period:', Starttime, ' -',
     &                    Endtime
      ENDIF
    4 FORMAT (/, 2(A, I5, 2('/',I2.2), I3.2, 2(':',I2.2)), /)

      gsfinit = 0

      END FUNCTION gsfinit

!***********************************************************************
!     gsfrun - Run GSFLOW
!***********************************************************************
      INTEGER FUNCTION gsfrun()
!     ------------------------------------------------------------------
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GSFMODFLOW
      USE PRMS_MODULE, ONLY: Model, Kper_mfo, Print_debug
      USE PRMS_BASIN, ONLY: Timestep
      USE PRMS_OBS, ONLY: Nowtime
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
!gsf  USE GWFEVTMODULE, ONLY:NEVTOP
!gsf  USE GWFRCHMODULE, ONLY:NRCHOP
      USE GWFSFRMODULE, ONLY:NUMTAB
      USE PCGMODULE
c      USE LMGMODULE
      USE SIPMODULE
      USE DE4MODULE
      USE GMGMODULE
      USE PCGN
      USE GWFNWTMODULE, ONLY:Itreal
      IMPLICIT NONE
      INTEGER I
      INCLUDE 'openspec.inc'
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: soilzone, GET_KPER
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms
      EXTERNAL READ_STRESS
      INTRINSIC MIN
! Local Variables
      INTEGER :: retval, II, kkper_new, KITER, IBDRET, iss
      INTEGER :: IC1, IC2, IR1, IR2, IL1, IL2, IDIR, day, gsflag
      REAL :: BUDPERC
!***********************************************************************
      gsfrun = 1
      
      IF ( Steady_state.EQ.1 ) THEN
        kkper_new = 1
        Kper_mfo = 2
      ELSEIF ( Model.NE.2 ) THEN
        kkper_new = GET_KPER()
      ELSE
        kkper_new = Kper_mfo
      ENDIF

      IF ( kkper_new.NE.KKPER ) THEN
        KPER = kkper_new
        KKPER = kkper_new
        CALL READ_STRESS()
        IF ( ISSFLG(KKPER).EQ.1 ) STOP
     &       'Error, cannot run steady state after first stress period.'
        IF ( ISSFLG(1).EQ.1 ) Delt_save = DELT
        IF ( DELT.NE.Delt_save ) STOP 'Error, cannot change DELT'
        KSTP = 0
      ENDIF
      iss = ISSFLG(KKPER)
      gsflag = 0
      IF ( Model.EQ.0 .AND. iss==0 ) gsflag = 1
C
C7C-----SIMULATE EACH TIME STEP.
!gsf    DO 90 KSTP = 1, NSTP(KPER)
          KSTP = KSTP + 1
          KKSTP = KSTP
          IF ( IUNIT(63).GT.0 )itreal = 0
          day = Nowtime(3)
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
          IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(1,IGRID)
          CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
          IF(IUNIT(62).GT.0) CALL GWF2UPW1AD(IGRID)
          IF(IUNIT(20).GT.0) CALL GWF2CHD7AD(KKPER,IGRID)
          IF(IUNIT(1).GT.0) CALL GWF2BCF7AD(KKPER,IGRID)
!gsf      IF(IUNIT(17).GT.0) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
          IF(IUNIT(23).GT.0) CALL GWF2LPF7AD(KKPER,IGRID)
          IF(IUNIT(37).GT.0) CALL GWF2HUF7AD(KKPER,IGRID)
          IF(IUNIT(16).GT.0) CALL GWF2FHB7AD(IGRID)
          IF(IUNIT(22).GT.0) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(15),
     1                                           IGRID)
          IF( IUNIT(44).GT.0 .AND. NUMTAB.GT.0 ) 
     2                             CALL GWF2SFR7AD(IUNIT(22))
          IF(IUNIT(50).GT.0) THEN
            IF (IUNIT(1).GT.0) THEN
              CALL GWF2MNW27BCF(KPER,IGRID)
            ELSE IF (IUNIT(23).GT.0) THEN
              CALL GWF2MNW27LPF(KPER,IGRID)
            ELSE IF(IUNIT(37).GT.0) THEN
              CALL GWF2MNW27HUF(KPER,IGRID)
            ELSE IF(IUNIT(62).GT.0) THEN
              CALL GWF2MNW27UPW(KPER,IGRID)
            ELSE
              WRITE(IOUT,1000)
 1000         FORMAT(/1X,
     &      '***ERROR: MNW2 PACKAGE DOES NOT SUPPORT',/,
     &      ' SELECTED FLOW PACKAGE',/,
     &      ' (MNW2 DOES FULLY SUPPORT BCF, LPF, AND HUF PACKAGES)',/,
     &      ' -- STOP EXECUTION')
              CALL USTOP('MNW2 error-flow package')
            END IF
            CALL GWF2MNW27AD(KKSTP,KKPER,IUNIT(62),IGRID)
          END IF
          IF(IUNIT(52).GT.0) CALL GWF2MNW17AD(IUNIT(1),IUNIT(23),
     1                                       IUNIT(37),IUNIT(62),IGRID)
          IF ( Model.EQ.2 ) THEN
C
C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
          CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
            WRITE (Logunt, 25) KPER, KSTP
          IF ( Print_debug>-1 ) WRITE(*,25)KPER,KSTP
   25     FORMAT(' Solving:  Stress period: ',i5,4x,
     &       'Time step: ',i5,4x,'Ground-Water Flow Eqn.')
          ENDIF
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
          Szcheck = 0
          IF ( gsflag==1 ) Szcheck = 1
          DO 30 KITER = 1, MXITER
            KKITER = KITER
            IF(IUNIT(62).GT.0) CALL GWF2UPWUPDATE(2,IGRID)
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
            CALL GWF2BAS7FM(IGRID)
            IF(IUNIT(1).GT.0) CALL GWF2BCF7FM(KKITER,KKSTP,
     1                               KKPER,IGRID)
            IF(IUNIT(62).GT.0) CALL GWF2UPWFMS(KKITER,KKSTP,KKPER,IGRID)
            IF(IUNIT(23).GT.0) CALL GWF2LPF7FM(KKITER,
     1                             KKSTP,KKPER,IGRID)
            IF(IUNIT(37).GT.0) CALL GWF2HUF7FM(KKITER,
     1                             KKSTP,KKPER,IUNIT(47),IGRID)
            IF ( IUNIT(62).EQ.0 ) THEN
              IF(IUNIT(21).GT.0) CALL GWF2HFB7FM(IGRID)
            END IF
            IF(IUNIT(2).GT.0) CALL GWF2WEL7FM(IUNIT(63),IGRID)
!gsf        IF(IUNIT(3).GT.0) CALL GWF2DRN7FM(IGRID)
!gsf        IF(IUNIT(4).GT.0) CALL GWF2RIV7FM(IGRID)
!gsf        IF(IUNIT(5).GT.0) THEN
!gsf          IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                    0,IGRID)
!gsf          CALL GWF2EVT7FM(IGRID)
!gsf          IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                    1,IGRID)
!gsf        END IF
            IF(IUNIT(7).GT.0) CALL GWF2GHB7FM(IGRID)
!gsf        IF(IUNIT(8).GT.0) THEN
!gsf           IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
!gsf  1                                                   0,IGRID)
!gsf           CALL GWF2RCH7FM(IGRID)
!gsf           IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
!gsf  1                                                   1,IGRID)
!gsf        END IF
            IF(IUNIT(16).GT.0) CALL GWF2FHB7FM(IGRID)
!gsf        IF(IUNIT(17).GT.0) CALL GWF2RES7FM(IGRID)
!gsf        IF(IUNIT(18).GT.0) CALL GWF2STR7FM(IGRID)
!gsf        IF(IUNIT(19).GT.0) CALL GWF2IBS7FM(KKPER,IGRID)
!gsf        IF(IUNIT(39).GT.0) CALL GWF2ETS7FM(IUNIT(63),IGRID)
!gsf        IF(IUNIT(40).GT.0) CALL GWF2DRT7FM(IGRID)

!  Call the PRMS modules that need to be inside the iteration loop
            IF ( Szcheck>0 ) THEN
              retval = soilzone()
              IF ( retval.NE.0 ) THEN
                PRINT 9001, 'soilzone', retval
                RETURN
              ENDIF
              retval = gsflow_prms2mf()
              IF ( retval.NE.0 ) THEN
                PRINT 9001, 'gsflow_prms2mf', retval
                RETURN
              ENDIF
              Sziters = Sziters + 1
              Maxgziter = KKITER
            ENDIF
            IF(IUNIT(55).GT.0) CALL GWF2UZF1FM(KKPER,KKSTP,KKITER,
     1                           IUNIT(44),IUNIT(22),IUNIT(63),0,
     2                           IGRID)
            IF(IUNIT(44).GT.0) CALL GWF2SFR7FM(KKITER,KKPER,KKSTP,
     1                              IUNIT(22),IUNIT(63),0,IUNIT(55),
     2                              IGRID)
            IF(IUNIT(22).GT.0) CALL GWF2LAK7FM(KKITER,KKPER,KKSTP,
     1                                     IUNIT(44),IUNIT(55),
     2                                     IGRID)
            IF(IUNIT(50).GT.0) THEN
              IF (IUNIT(1).GT.0) THEN
                CALL GWF2MNW27BCF(KPER,IGRID)
              ELSE IF (IUNIT(23).GT.0) THEN
                CALL GWF2MNW27LPF(KPER,IGRID)
              ELSE IF(IUNIT(37).GT.0) THEN
                CALL GWF2MNW27HUF(KPER,IGRID)
              ELSE IF(IUNIT(62).GT.0) THEN
                CALL GWF2MNW27UPW(KPER,IGRID)
              END IF
              CALL GWF2MNW27FM(KKITER,KKSTP,KKPER,IUNIT(62),IGRID)
            END IF
            IF(IUNIT(52).GT.0) CALL GWF2MNW17FM(KKITER,IUNIT(1),
     1                               IUNIT(23),IUNIT(37),IUNIT(62),
     2                               IGRID)
!gsf        IF(IUNIT(54).GT.0) CALL GWF2SUB7FM(KKPER,KKITER,IUNIT(9),
!gsf 1                                         IGRID)
!gsf        IF(IUNIT(57).GT.0) CALL GWF2SWT7FM(KKPER,IGRID)
C
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
            IERR=0
            IF (IUNIT(9).GT.0) THEN
                   CALL SIP7PNT(IGRID)
                   CALL SIP7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,EL,FL,GL,
     1               V,W,HDCG,LRCH,NPARM,KKITER,HCLOSE,ACCL,ICNVG,
     2               KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP(KKPER),
     3               NCOL,NROW,NLAY,NODES,IOUT,0,IERR)
            END IF
            IF (IUNIT(10).GT.0) THEN
                   CALL DE47PNT(IGRID)
                   CALL DE47AP(HNEW,IBOUND,AU,AL,IUPPNT,IEQPNT,D4B,MXUP,
     1               MXLOW,MXEQ,MXBW,CR,CC,CV,HCOF,RHS,ACCLDE4,KITER,
     2               ITMX,MXITER,NITERDE4,HCLOSEDE4,IPRD4,ICNVG,NCOL,
     3               NROW,NLAY,IOUT,LRCHDE4,HDCGDE4,IFREQ,KKSTP,KKPER,
     4               DELT,NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,
     5               DELTL,NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW,IERR)
            END IF
            IF (IUNIT(13).GT.0) THEN
!rgn increase damping factor for transient solution.
                   IF ( iss.EQ.0 ) DAMPPCG = DAMPPCGT
                   CALL PCG7PNT(IGRID)
                   CALL PCG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,VPCG,SS,
     1               P,CD,HCHG,LHCH,RCHG,LRCHPCG,KKITER,NITER,
     2               HCLOSEPCG,RCLOSEPCG,ICNVG,KKSTP,KKPER,IPRPCG,
     3               MXITER,ITER1,NPCOND,NBPOL,NSTP(KKPER),NCOL,NROW,
     4               NLAY,NODES,RELAXPCG,IOUT,MUTPCG,IT1,DAMPPCG,BUFF,
     5               HCSV,IERR,HPCG,DAMPPCGT,ISSFLG(KKPER),HDRY,
     6               IHCOFADD)
            END IF
c            IF (IUNIT(14).GT.0) THEN
c              CALL LMG7PNT(IGRID)
c              CALL LMG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U1,
c     1           FRHS,IG,ISIZ1,ISIZ2,ISIZ3,ISIZ4,KKITER,BCLOSE,DAMPLMG,
c     2           ICNVG,KKSTP,KKPER,MXITER,MXCYC,NCOL,NROW,NLAY,NODES,
c     3           HNOFLO,IOUT,IOUTAMG,ICG,IADAMPLMG,DUPLMG,DLOWLMG)
c            END IF
            IF (IUNIT(42).GT.0) THEN
                   CALL GMG7PNT(IGRID)
                   CALL GMG7AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,
     1                         IITER,MXITER,RCLOSEGMG,HCLOSEGMG,
     2                         KKITER,KKSTP,KKPER,NCOL,NROW,NLAY,ICNVG,
     3                         SITER,TSITER,DAMPGMG,IADAMPGMG,IOUTGMG,
     4                         IOUT,GMGID,
     5                         IUNITMHC,DUP,DLOW,CHGLIMIT,
     6                         BIGHEADCHG,HNEWLAST)
            ENDIF
            IF (IUNIT(59).GT.0) CALL PCGN2AP(HNEW,RHS,CR,CC,CV,HCOF,
     1                                       IBOUND,KKITER,KKSTP,KKPER,
     2                                       ICNVG,HNOFLO,IGRID)
! Calculate new heads using Newton solver
            IF(IUNIT(63).GT.0 ) 
     1          CALL GWF2NWT1FM(KKITER,ICNVG,KSTP,KPER,MXITER,
     2                          IUNIT(22),IGRID)
            IF(IERR.EQ.1) CALL USTOP(' ')
C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
            IF (ICNVG.EQ.1) GOTO 33
            IF ( Szcheck>0 ) THEN
              retval = gsflow_mf2prms()
              IF ( retval.NE.0 ) THEN
                PRINT 9001, 'gsflow_mf2prms', retval
                RETURN
              ENDIF
            ENDIF
  30      CONTINUE
          KITER = MXITER
C
  33      CONTINUE
          IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(2,IGRID)
C
C7C3----DETERMINE WHICH OUTPUT IS NEEDED.
          CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,IUNIT(12),IGRID)
C
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
          MSUM = 1
          IF (IUNIT(1).GT.0) THEN
            CALL GWF2BCF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2BCF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 37 IDIR = 1, 3
              CALL GWF2BCF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     1                          IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
   37       CONTINUE
          ENDIF
! ADDED CALLS FOR FLOW BUDGETS TO UPW PACKAGE.
          IF(IUNIT(62).GT.0) THEN
            CALL GWF2UPWBDS(KKSTP,KKPER,IGRID)
           CALL GWF2UPWBDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 158 IDIR=1,3
              CALL GWF2UPWBDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
158         CONTINUE
          ENDIF
          IF(IUNIT(23).GT.0) THEN
            CALL GWF2LPF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2LPF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 157 IDIR=1,3
              CALL GWF2LPF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
157         CONTINUE
          ENDIF
          IF(IUNIT(37).GT.0) THEN
            CALL GWF2HUF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2HUF7BDCH(KKSTP,KKPER,IUNIT(47),IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 159 IDIR=1,3
              CALL GWF2HUF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
159         CONTINUE
          ENDIF
          IF(IUNIT(2).GT.0) CALL GWF2WEL7BD(KKSTP,KKPER,IUNIT(63),IGRID)
!gsf      IF(IUNIT(3).GT.0) CALL GWF2DRN7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(4).GT.0) CALL GWF2RIV7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(5).GT.0) THEN
!gsf         IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                     0,IGRID)
!gsf         CALL GWF2EVT7BD(KKSTP,KKPER,IGRID)
!gsf         IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                     1,IGRID)
!gsf      END IF
          IF(IUNIT(7).GT.0) CALL GWF2GHB7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(8).GT.0) THEN
!gsf        IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                    0,IGRID)
!gsf        CALL GWF2RCH7BD(KKSTP,KKPER,IUNIT(44),IGRID)
!gsf        CALL GWF2RCH7BD(KKSTP,KKPER,IGRID)
!gsf        IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                    1,IGRID)
!gsf      END IF
          IF(IUNIT(16).GT.0) CALL GWF2FHB7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(17).GT.0) CALL GWF2RES7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(18).GT.0) CALL GWF2STR7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(19).GT.0) CALL GWF2IBS7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(39).GT.0) CALL GWF2ETS7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(40).GT.0) CALL GWF2DRT7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(55).GT.0) CALL GWF2UZF1BD(KKSTP,KKPER,IUNIT(22),
     1                             IUNIT(44),IGRID)
          IF(IUNIT(44).GT.0) CALL GWF2SFR7BD(KKSTP,KKPER,IUNIT(15),
     1                        IUNIT(22),IUNIT(46),IUNIT(55),NSOL,
     2                        IUNIT(8),IGRID)
          IF(IUNIT(22).GT.0) CALL GWF2LAK7BD(KKSTP,KKPER,IUNIT(15),
     1                       IUNIT(46),IUNIT(44),IUNIT(55),NSOL,IGRID)
          IF(IUNIT(50).GT.0) CALL GWF2MNW27BD(KKSTP,KKPER,IUNIT(62),
     1                      IGRID)
          IF(IUNIT(52).GT.0) CALL GWF2MNW17BD(NSTP(KPER),KKSTP,KKPER,
     1                      IGRID)
!gsf      IF(IUNIT(54).GT.0) CALL GWF2SUB7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(57).GT.0) CALL GWF2SWT7BD(KKSTP,KKPER,IGRID)
CLMT
CLMT----CALL LINK-MT3DMS SUBROUTINES TO SAVE FLOW-TRANSPORT LINK FILE
CLMT----FOR USE BY MT3DMS FOR TRANSPORT SIMULATION
CLMT
!gsf      IF(IUNIT(49).GT.0) CALL LMT7BD(KKSTP,KKPER,IGRID)
CLMT                              
C
C
!  Set NWT heads to Hdry when head is below bottom.
          IF(IUNIT(63).GT.0) CALL GWF2NWT1BD()
C  Observation and hydrograph simulated equivalents
          CALL OBS2BAS7SE(IUNIT(28),IGRID)
!gsf      IF(IUNIT(33).GT.0) CALL OBS2DRN7SE(IGRID)
!gsf      IF(IUNIT(34).GT.0) CALL OBS2RIV7SE(IGRID)
          IF(IUNIT(35).GT.0) CALL OBS2GHB7SE(IGRID)
!gsf      IF(IUNIT(36).GT.0) CALL OBS2STR7SE(IGRID)
          IF(IUNIT(38).GT.0) CALL OBS2CHD7SE(KKPER,IGRID)
!gsf      IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7SE(1,IGRID)
!gsf      IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0)
!gsf 1                              CALL GWF2HYD7IBS7SE(1,IGRID)
!gsf      IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0)
!gsf 1                              CALL GWF2HYD7SUB7SE(1,IGRID)
!gsf      IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
!gsf 1                              CALL GWF2HYD7STR7SE(1,IGRID)
!gsf      IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
!gsf 1                              CALL GWF2HYD7SFR7SE(1,IGRID)
C
C7C5---PRINT AND/OR SAVE DATA.
          CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID,BUDPERC)
!gsf      IF(IUNIT(19).GT.0) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),
!gsf 1                                       IGRID)
          IF(IUNIT(37).GT.0)THEN
            IF(IOHUFHDS .NE.0 .OR.IOHUFFLWS .NE.0)
     1         CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
          ENDIF
          IF(IUNIT(51).NE.0) CALL GWF2MNW2I7OT(NSTP(KKPER),KKSTP,
     1                       KKPER,IGRID)
!gsf      IF(IUNIT(54).GT.0) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54),
!gsf 1                                       IGRID)
!gsf      IF(IUNIT(57).GT.0) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7OT(KKSTP,KKPER,IGRID)
C
C7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
          IF(ICNVG.EQ.0) THEN
            NCVGERR=NCVGERR+1
            WRITE(IOUT,87) BUDPERC
   87       FORMAT(1X,'FAILURE TO MEET SOLVER CONVERGENCE CRITERIA',/
     1       1X,'BUDGET PERCENT DISCREPANCY IS',F10.4)
!gsf        IF(ABS(BUDPERC).GT.STOPER) THEN
!gsf          WRITE(IOUT,*) 'STOPPING SIMULATION'
!gsf          GO TO 110
!gsf        ELSE
!gsf          WRITE(IOUT,*) 'CONTINUING EXECUTION'
!gsf        END IF
            Convfail_cnt = Convfail_cnt + 1
            IF ( Print_debug>-1 )
     &           PRINT 9004, Nowtime(1), Nowtime(2), day, Convfail_cnt
            WRITE (Logunt, 9004) Nowtime(1), Nowtime(2), day,
     &                           Convfail_cnt
          ENDIF
C
C-----END OF TIME STEP (KSTP) AND STRESS PERIOD (KPER) LOOPS
!gsf 90   CONTINUE
!gsf100 CONTINUE
C
C
      IF(IUNIT(52).NE.0) CALL GWF2MNW17OT(IGRID)

      IF ( gsflag==1 ) THEN
        IF ( Szcheck==-1 .OR. Szcheck==1 ) Nszchanging = Nszchanging + 1
        Iter_cnt(Maxgziter) = Iter_cnt(Maxgziter) + 1
        IF ( Maxgziter.GT.Max_sziters ) Max_sziters = Maxgziter
        II = MIN(ITDIM, KKITER)
        Mfiter_cnt(II) = Mfiter_cnt(II) + 1
        Iterations = Iterations + KKITER
        IF ( KKITER.GT.Max_iters ) Max_iters = KKITER
        IF ( Print_debug>-1 ) THEN
          IF ( day.EQ.1 ) THEN
            PRINT 9002, Nowtime(1), Nowtime(2), day, KKPER, KKSTP,
     &                  Timestep, KKITER, Maxgziter
          ELSEIF ( KKITER.GT.75 ) THEN
            PRINT 9002, Nowtime(1), Nowtime(2), day, KKPER, KKSTP,
     &                  Timestep, KKITER, Maxgziter
          ENDIF
        ENDIF
        WRITE (Logunt, 9002) Nowtime(1), Nowtime(2), day, KKPER,
     &                       KKSTP, Timestep, KKITER, Maxgziter
      ENDIF

 9001 FORMAT ('Error in ', A, ' module, arg = run.',
     &        ' Called from gsfrun.', /, 'Return val =', I2)
 9002 FORMAT('Date:', I5, 2('/',I2.2), '; Stress:', I3, '; Step:', I5,
     &       '; Simulation step:', I5, /, 18X, 'MF iterations:', I4,
     &       '; SZ computations:', I4, /)
 9004 FORMAT('***TIME STEP FAILED TO CONVERGE - Date:', I5, 2('/',I2.2),
     &       ' number:', I5, /)

      gsfrun = 0
      END FUNCTION gsfrun
!
!***********************************************************************
!     gsfclean - After GSFFLOW is done
!***********************************************************************
      INTEGER FUNCTION gsfclean()
!     ------------------------------------------------------------------
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GSFMODFLOW
      USE PRMS_MODULE, ONLY: Model
      USE PRMS_BASIN, ONLY: Timestep
      USE PRMS_OBS, ONLY: Nowtime
      USE GLOBAL, ONLY: IOUT, IUNIT, NIUNIT
      USE PCGN
      USE GWFNWTMODULE, ONLY:LINMETH
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getvar
      INTEGER istep
!***********************************************************************
      gsfclean = 1
C
C8------END OF SIMULATION
C-------SAVE RESTART RECORDS FOR SUB PACKAGE
!gsf  110 IF(IUNIT(54).GT.0) CALL GWF2SUB7SV(IGRID)
C
C  Observation output
      IF(IUNIT(28).GT.0) CALL OBS2BAS7OT(IUNIT(28),IGRID)
!gsf  IF(IUNIT(33).GT.0) CALL OBS2DRN7OT(IGRID)
!gsf  IF(IUNIT(34).GT.0) CALL OBS2RIV7OT(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7OT(IGRID)
!gsf  IF(IUNIT(36).GT.0) CALL OBS2STR7OT(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7OT(IGRID)
      CALL GLO1BAS6ET(IOUT,IBDT,1)
C
C9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7DA MUST BE CALLED
C9------LAST BECAUSE IT DEALLOCATES IUNIT.
      CALL SGWF2BAS7PNT(IGRID)
      IF(IUNIT(1).GT.0) CALL GWF2BCF7DA(IGRID)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7DA(IGRID)
!gsf  IF(IUNIT(3).GT.0) CALL GWF2DRN7DA(IGRID)
!gsf  IF(IUNIT(4).GT.0) CALL GWF2RIV7DA(IGRID)
!gsf  IF(IUNIT(5).GT.0) CALL GWF2EVT7DA(IGRID)
      IF(IUNIT(7).GT.0) CALL GWF2GHB7DA(IGRID)
!gsf  IF(IUNIT(8).GT.0) CALL GWF2RCH7DA(IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7DA(IGRID)
      IF(IUNIT(10).GT.0) CALL DE47DA(IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7DA(IGRID)
c      IF(IUNIT(14).GT.0) CALL LMG7DA(IGRID)
      IF(IUNIT(63).GT.0) THEN
        IF(LINMETH.EQ.1) THEN
          CALL GMRES7DA(IGRID)
        ELSEIF(LINMETH.EQ.2) THEN
          CALL XMD7DA(IGRID)
!        ELSEIF(LINMETH.EQ.3) THEN
!          CALL SAMG7DA(IGRID)
        END IF
        CALL GWF2NWT1DA(IGRID)
      END IF
      IF(IUNIT(62).GT.0) CALL GWF2UPW1DA(IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7DA(IGRID)
!gsf  IF(IUNIT(17).GT.0) CALL GWF2RES7DA(IGRID)
!gsf  IF(IUNIT(18).GT.0) CALL GWF2STR7DA(IGRID)
!gsf  IF(IUNIT(19).GT.0) CALL GWF2IBS7DA(IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7DA(IGRID)
      IF(IUNIT(21).GT.0) CALL GWF2HFB7DA(IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0)CALL GWF2LAK7DA(IUNIT(22),
     1                                              IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7DA(IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7DA(IGRID)
!gsf  IF(IUNIT(39).GT.0) CALL GWF2ETS7DA(IGRID)
!gsf  IF(IUNIT(40).GT.0) CALL GWF2DRT7DA(IGRID)
      IF(IUNIT(42).GT.0) CALL GMG7DA(IGRID)
      IF(IUNIT(59).GT.0) CALL PCGN2DA(IGRID)
      IF(IUNIT(44).GT.0) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27DA(IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7DA(IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17DA(IGRID)
!gsf  IF(IUNIT(54).GT.0) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1DA(IGRID)
!gsf  IF(IUNIT(57).GT.0) CALL GWF2SWT7DA(IGRID)
      CALL OBS2BAS7DA(IUNIT(28),IGRID)
!gsf  IF(IUNIT(33).GT.0) CALL OBS2DRN7DA(IGRID)
!gsf  IF(IUNIT(34).GT.0) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7DA(IGRID)
!gsf  IF(IUNIT(36).GT.0) CALL OBS2STR7DA(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7DA(IGRID)
!gsf  IF(IUNIT(43).GT.0) CALL GWF2HYD7DA(IGRID)
!gsf  IF(IUNIT(49).GT.0) CALL LMT7DA(IGRID)
      CALL GWF2BAS7DA(IGRID)
C
      IF ( Model.NE.2 ) THEN
        istep = Timestep
        PRINT 9001, Iterations, FLOAT(Iterations)/FLOAT(istep),Max_iters
        WRITE (Logunt, 9001) Iterations, FLOAT(Iterations)/FLOAT(istep),
     &                       Max_iters
        PRINT 9002, Convfail_cnt, istep
        WRITE (Logunt, 9002) Convfail_cnt, istep
        WRITE (Logunt, 9003) 'MF iteration distribution:', Mfiter_cnt
        WRITE (Logunt, '(/)')
        PRINT 9004, Sziters, FLOAT(Sziters)/FLOAT(istep), Max_sziters
        WRITE (Logunt, 9004) Sziters, FLOAT(Sziters)/FLOAT(istep),
     &                       Max_sziters
        IF ( getvar('prms2mf', 'stopcount', 1, 'integer', Stopcount)
     +     .NE.0 ) RETURN
        PRINT 9005, Nszchanging, Stopcount
        WRITE (Logunt, 9005) Nszchanging, Stopcount
        WRITE (Logunt, 9007) 'SZ computation distribution:', Iter_cnt
        WRITE (Logunt, '(/)')
        PRINT 9006, ' Simulation end time: ', Nowtime
        WRITE (Logunt, 9006) ' Simulation end time: ', Nowtime
        DEALLOCATE (Stress_dates)
      ENDIF

C10-----END OF PROGRAM.
      IF(NCVGERR.GT.0) THEN
        WRITE(*,*) 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA ',
     1          NCVGERR,' TIME(S)'
        WRITE (Logunt, *) 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA ',
     1          NCVGERR,' TIME(S)'
      ELSE
        WRITE(*,*) ' Normal termination of simulation'
        WRITE (Logunt, *) ' Normal termination of simulation'
      END IF
      CLOSE (Logunt)

!gsf  CALL USTOP(' ')
C
      IF ( Model.EQ.2 ) CALL USTOP(' ')

      gsfclean = 0

 9001 FORMAT ('Iterations:', I6, '; Average iterations:', F6.2,
     &        '; Maximum iterations:', I4)
 9002 FORMAT ('Number of non-convergence:', I5,
     &        '; Number of time steps:', I5, /)
 9003 FORMAT (A, 2X, 10I5, /, 10(28X, 10I5, /))
 9004 FORMAT ('SZ computations:', I6, '; Average SZ computations:',F5.2,
     &        '; Maximum SZ computations:', I4)
 9005 FORMAT ('Times SZ changing when MF converged:', I6,
     &        '; mxsziter reached:', I4, /)
 9006 FORMAT (A, I4, 2('/', I2.2), I3.2, 2(':', I2.2), //)
 9007 FORMAT (A, 10I5, /, 10(28X, 10I5, /))

      END FUNCTION gsfclean
!
      SUBROUTINE GETNAMFIL(FNAME)
C     ******************************************************************
C     GET THE NAME OF THE NAME FILE
C     ******************************************************************
C        SPECIFICATIONS:
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      CHARACTER*(*) FNAME
      CHARACTER*200 COMLIN
      LOGICAL EXISTS
      INTEGER ICOL, ISTART, ISTOP, N, NC
      REAL R
C     ------------------------------------------------------------------
C
C Get name file from command line or user interaction.
        FNAME=' '
        COMLIN=' '
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** Modflow-2000 to read the name of a Name file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
        CALL GETARG(1,COMLIN)
C        CALL GETCL(COMLIN)
        ICOL = 1
        IF(COMLIN.NE.' ') THEN
          FNAME=COMLIN
        ELSE
   15     WRITE (*,*) ' Enter the name of the NAME FILE: '
          READ (*,'(A)') FNAME
          CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FNAME=FNAME(ISTART:ISTOP)
          IF (FNAME.EQ.' ') GOTO 15
        ENDIF
        INQUIRE (FILE=FNAME,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          NC=INDEX(FNAME,' ')
          FNAME(NC:NC+3)='.nam'
          INQUIRE (FILE=FNAME,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            WRITE (*,480) FNAME(1:NC-1),FNAME(1:NC+3)
  480       FORMAT(1X,'Can''t find name file ',A,' or ',A)
            CALL USTOP(' ')
          ENDIF
        ENDIF
C
      RETURN
      END
      SUBROUTINE GLO1BAS6ET(IOUT,IBDT,IPRTIM)
C     ******************************************************************
C     Get end time and calculate elapsed time
C     ******************************************************************
      USE GSFMODFLOW, ONLY: Logunt
      USE PRMS_MODULE, ONLY: Print_debug
C
C        SPECIFICATIONS:
      IMPLICIT NONE
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: IOUT, IBDT(8), IPRTIM
      INTEGER IBD, IED, MB, ME, MC, NM, M, NSPD, I, NDAYS, LEAP
      INTEGER NHOURS, NMINS, NSECS, MSECS, NRSECS
      INTEGER IEDT(8), IDPM(12)
      REAL ELSEC, RSECS
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IEDT, and write.
      CALL DATE_AND_TIME(VALUES=IEDT)
      IF ( Print_debug>-1 ) WRITE(*,1000)(IEDT(I),I=1,3),(IEDT(I),I=5,7)
      WRITE (Logunt, '(//)')
      WRITE (Logunt, 1000) (IEDT(I),I=1,3), (IEDT(I),I=5,7)
 1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
      IF(IPRTIM.GT.0) THEN
        WRITE(IOUT,'(1X)')
        WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
      END IF
C
C     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
C     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
C       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
C       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
C
C     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
C
C     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
      NDAYS = ELSEC/NSPD
      RSECS = MOD(ELSEC,86400.0)
      NHOURS = RSECS/3600.0
      RSECS = MOD(RSECS,3600.0)
      NMINS = RSECS/60.0
      RSECS = MOD(RSECS,60.0)
      NSECS = RSECS
      RSECS = MOD(RSECS,1.0)
      MSECS = NINT(RSECS*1000.0)
      NRSECS = NSECS
      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
C
C     Write elapsed time to screen
        IF (NDAYS.GT.0) THEN
          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
          WRITE (Logunt, 1010) NDAYS, NHOURS, NMINS, NRSECS
 1010     FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(*,1020) NHOURS,NMINS,NRSECS
          WRITE (Logunt, 1020) NHOURS, NMINS, NRSECS
 1020     FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NMINS.GT.0) THEN
          WRITE(*,1030) NMINS,NSECS,MSECS
          WRITE (Logunt, 1030) NMINS, NSECS, MSECS
 1030     FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',
     &      I2,'.',I3.3,' Seconds',/)
        ELSE
          WRITE(*,1040) NSECS,MSECS
          WRITE (Logunt, 1040) NSECS, MSECS
 1040     FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
        ENDIF
C
C     Write times to file if requested
      IF(IPRTIM.GT.0) THEN
        IF (NDAYS.GT.0) THEN
          WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
        ELSEIF (NMINS.GT.0) THEN
          WRITE(IOUT,1030) NMINS,NSECS,MSECS
        ELSE
          WRITE(IOUT,1040) NSECS,MSECS
        ENDIF
      ENDIF
C
      RETURN
      END

!***********************************************************************
!     READ AND PREPARE INFORMATION FOR STRESS PERIOD.
!***********************************************************************
      SUBROUTINE READ_STRESS()
      USE GSFMODFLOW, ONLY: IGRID, KKPER, KPER, NSOL, IOUTS, KKSTP
      USE GLOBAL, ONLY: IUNIT, ISSFLG
      USE PRMS_MODULE, ONLY: Model
      IMPLICIT NONE
!***********************************************************************
C7------SIMULATE EACH STRESS PERIOD.
        KKPER = KPER
        IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(1,IGRID)
        CALL GWF2BAS7ST(KKPER,IGRID)
!gsf    IF(IUNIT(19).GT.0) CALL GWF2IBS7ST(KKPER,IGRID)
!gsf    IF(IUNIT(54).GT.0) CALL GWF2SUB7ST(KKPER,IGRID)
!gsf    IF(IUNIT(57).GT.0) CALL GWF2SWT7ST(KKPER,IGRID)
C
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C----------READ USING PACKAGE READ AND PREPARE MODULES.
        IF(IUNIT(2).GT.0) CALL GWF2WEL7RP(IUNIT(2),IGRID)
!gsf    IF(IUNIT(3).GT.0) CALL GWF2DRN7RP(IUNIT(3),IGRID)
!gsf    IF(IUNIT(4).GT.0) CALL GWF2RIV7RP(IUNIT(4),IGRID)
!gsf    IF(IUNIT(5).GT.0) CALL GWF2EVT7RP(IUNIT(5),IGRID)
        IF(IUNIT(7).GT.0) CALL GWF2GHB7RP(IUNIT(7),IGRID)
!        IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IUNIT(44),IGRID)
!gsf    IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IGRID)
!gsf    IF(IUNIT(17).GT.0) CALL GWF2RES7RP(IUNIT(17),IGRID)
!gsf    IF(IUNIT(18).GT.0) CALL GWF2STR7RP(IUNIT(18),IGRID)
!gsf    IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
!gsf 1                     CALL GWF2HYD7STR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(20).GT.0) CALL GWF2CHD7RP(IUNIT(20),IGRID)
        IF(IUNIT(44).GT.0) CALL GWF2SFR7RP(IUNIT(44),IUNIT(15),
     1                                     IUNIT(22),KKPER,KKSTP,NSOL,
     2                                     IOUTS,IUNIT(1),IUNIT(23),
     3                                     IUNIT(37), IUNIT(62),
     4                                     IUNIT(55), IGRID)
!gsf    IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
!gsf 1                     CALL GWF2HYD7SFR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(55).GT.0) CALL GWF2UZF1RP(IUNIT(55),KKPER,IUNIT(44),
     1                                     IGRID)
        IF(IUNIT(22).GT.0) CALL GWF2LAK7RP(IUNIT(22),IUNIT(1),
     1               IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),
     2               IUNIT(62),KKPER,NSOL,IOUTS,IGRID)
        IF(IUNIT(46).GT.0.AND.KKPER.EQ.1) CALL GWF2GAG7RP(IUNIT(15),
     1             IUNIT(22),IUNIT(55),NSOL,IGRID)
!gsf    IF(IUNIT(39).GT.0) CALL GWF2ETS7RP(IUNIT(39),IGRID)
!gsf    IF(IUNIT(40).GT.0) CALL GWF2DRT7RP(IUNIT(40),IGRID)
        IF(IUNIT(50).GT.0) CALL GWF2MNW27RP(IUNIT(50),kper,IUNIT(9),
     +                                      IUNIT(10),0,IUNIT(13),
     +                                      IUNIT(15),IUNIT(63),IGRID)
        IF(IUNIT(51).GT.0.AND.KKPER.EQ.1) CALL GWF2MNW2I7RP(IUNIT(51),
     1                     0,IGRID)
        IF(IUNIT(52).GT.0) CALL GWF2MNW17RP(IUNIT(52),IUNIT(1),
     1                            IUNIT(23),IUNIT(37),IUNIT(62),KKPER,
     2                            IGRID)
        IF ( Model.EQ.0 .AND. ISSFLG(KPER).EQ.0 )
     1                   CALL ZERO_SPECIFIED_FLOWS(IUNIT(22),IUNIT(44))

      END SUBROUTINE READ_STRESS

!     ******************************************************************
!     DETERMINE THE STRESS PERIOD FOR THE CURRENT TIMESTEP
!     ******************************************************************
      INTEGER FUNCTION GET_KPER()
      USE GLOBAL, ONLY: NPER
      USE GSFMODFLOW, ONLY: Stress_dates, KPER
      USE PRMS_BASIN, ONLY: Starttime
      IMPLICIT NONE
      INTRINSIC DBLE
      DOUBLE PRECISION, EXTERNAL :: nowjt, getjulday
! Local Variables
      DOUBLE PRECISION :: now, seconds
!     ------------------------------------------------------------------
      GET_KPER = -1
      now = nowjt()
!
!     If called from init, then "now" isn't set yet.
!     Set "now" to model start date.
      IF ( now.LE.1.0D0 ) THEN
        seconds = DBLE(Starttime(6))
        now = getjulday(Starttime(2), Starttime(3), Starttime(1),
     &                  Starttime(4), Starttime(5), seconds)
      ENDIF
      IF ( now.LT.Stress_dates(1) ) RETURN
      IF ( now.GT.Stress_dates(NPER) ) THEN
        GET_KPER = NPER
      ELSEIF ( now.LT.Stress_dates(KPER+1) ) THEN
        GET_KPER = KPER
      ELSE
        GET_KPER = KPER + 1
      ENDIF

      END FUNCTION GET_KPER

!***********************************************************************
!     READ AND PREPARE INFORMATION FOR STRESS PERIOD.
!***********************************************************************
      SUBROUTINE SET_STRESS_DATES(Stresstime)
      USE GLOBAL, ONLY: NPER, PERLEN, ISSFLG
      USE GSFMODFLOW, ONLY: Stress_dates
      IMPLICIT NONE
      DOUBLE PRECISION, EXTERNAL :: getjulday
! Arguments
      INTEGER, INTENT(IN) :: Stresstime(6)
! Local Variables
      INTEGER :: i
      DOUBLE PRECISION :: seconds
      REAL :: plen
!***********************************************************************
      seconds = Stresstime(6)
      Stress_dates(1) = getjulday(Stresstime(2), Stresstime(3),
     &                            Stresstime(1), Stresstime(4),
     &                            Stresstime(5), seconds)
      DO i = 1, NPER
        IF ( ISSFLG(i).EQ.1 ) THEN
          plen = 1.0
          Stress_dates(i) = Stress_dates(i) - 1
        ELSE
          plen = PERLEN(i)
        ENDIF
        Stress_dates(i+1) = Stress_dates(i) + plen
      ENDDO

      END SUBROUTINE SET_STRESS_DATES

!***********************************************************************
!     READ AND PREPARE INFORMATION FOR STRESS PERIOD.
!***********************************************************************
      INTEGER FUNCTION GET_FUNIT()
! Local Variables
      LOGICAL :: opend
      INTEGER :: iunt
!***********************************************************************
      opend = .TRUE.
      iunt = 300
      DO WHILE ( opend )
        iunt = iunt + 1
        INQUIRE (UNIT=iunt, OPENED=opend)
      ENDDO
      GET_FUNIT = iunt
      
      END FUNCTION GET_FUNIT
!
!***********************************************************************
!     Zero SFR and LAK arrays for specified precipitation, ET and runoff.
!     For GSFLOW simulations only.
!***********************************************************************
      SUBROUTINE ZERO_SPECIFIED_FLOWS(Iunitlak,Iunitsfr)
      USE GWFSFRMODULE, ONLY: NSTRM, STRM
      USE GWFLAKMODULE, ONLY: PRCPLK,EVAPLK,RNF,WTHDRW,NLAKES
      USE GSFMODFLOW, ONLY: Logunt
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunitlak, Iunitsfr
! Local Variables
      INTEGER :: i, j 
      REAL :: ZERO, TESTSFR, TESTLAK
!***********************************************************************
! Zero SFR flows (RUNOFF, ETSW, and PPTSW)
      ZERO = 0.0
      TESTSFR = 0.0
      TESTLAK = 0.0
      IF ( Iunitsfr.GT.0 ) THEN
        DO i =1, NSTRM
          DO j=12,14
            TESTSFR = TESTSFR + ABS(STRM(j,i))
            STRM(j,i) = ZERO
          END DO
        END DO
        IF ( TESTSFR.GT.1.0E-5 ) THEN
          WRITE (Logunt, *)
          WRITE (Logunt, *)'***WARNING***'
          WRITE (Logunt, 10)
          WRITE (Logunt, *)
          WRITE (*, *)
          WRITE (*, *)'***WARNING***'
          WRITE (*, 10)
          WRITE (Logunt, *)
        END IF
      END IF
! Zero LAK flows (PPT, EVAP, RUNOFF, SP.WITHDRAWL).
      IF ( Iunitlak.GT.0 ) THEN
        DO i = 1, NLAKES
          TESTLAK = TESTLAK + ABS(PRCPLK(i)) + ABS(EVAPLK(i)) + 
     +              ABS(RNF(i)) + ABS(WTHDRW(i))
          PRCPLK(i) = ZERO
          EVAPLK(i) = ZERO
          RNF(i) = ZERO
          WTHDRW(i) = ZERO
        END DO
        IF ( TESTLAK.GT.1.0E-5 ) THEN
          WRITE (Logunt, *)
          WRITE (Logunt, *)'***WARNING***'
          WRITE (Logunt, 11)
          WRITE (Logunt, *)
          WRITE (*, *)
          WRITE (*, *)'***WARNING***'
          WRITE (*, 11)
          WRITE (*, *)
        END IF
      END IF
   10  FORMAT('Non-zero values were specified for precipitation,',/,
     +       'streamflow, and ET for streams in MODFLOW input files.',/,
     +       'These values are set to zero for GSFLOW ',
     +       'simulations')    
   11 FORMAT('Non-zero values were specified for precipitation,',/,
     +       'streamflow, ET, and Sp.Flow for lakes in MODFLOW',/,
     +       'input files. These values are set to zero',/,
     +       'for GSFLOW simulations.')
      RETURN
      END SUBROUTINE ZERO_SPECIFIED_FLOWS

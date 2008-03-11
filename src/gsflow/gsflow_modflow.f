!***********************************************************************
!     GSFLOW module that replaces mf2005.f
!***********************************************************************
      MODULE GSFMODFLOW
!   Local Variables
      INTEGER, PARAMETER :: ITDIM = 50
      INTEGER :: Convfail_cnt, Nowtime(6), Steady_state, Iterations
      INTEGER :: IGRID, MXITER, KKPER, ICNVG, IBDT(8), NSOL, IOUTS
      INTEGER :: KSTP, KKSTP, IERR, Max_iters
      INTEGER :: Mfiter_cnt(ITDIM)
      INTEGER :: Nszchanging
      REAL :: Delt_save, Hdryhuf
      DOUBLE PRECISION, ALLOCATABLE :: Stress_dates(:)
      CHARACTER(LEN=68) :: Versn
!   Declared Variables
      INTEGER :: KKITER, KPER, Logunt
!   Declared Variable from gsflow_prms
      INTEGER :: Model
!   Declared Parameters
      INTEGER :: Mxsziter
!   Control Parameters
      CHARACTER(LEN=256) :: Modflow_name, Model_mode
      END MODULE GSFMODFLOW

C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- MODFLOW-2005
C     ******************************************************************
C
      INTEGER FUNCTION gsflow_modflow(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL:: gsfdecl, gsfinit, gsfrun, gsfclean
!***********************************************************************
      gsflow_modflow = 0

      IF ( Arg.EQ.'run' ) THEN
        gsflow_modflow = gsfrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        gsflow_modflow = gsfdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        gsflow_modflow = gsfinit()
      ELSEIF ( Arg.EQ.'cleanup' ) THEN
        gsflow_modflow = gsfclean()
      ENDIF

      END FUNCTION gsflow_modflow

!***********************************************************************
!     gsfdecl - set up parameters for GSFLOW computations
!   Declared Parameters
!     mxsziter
!***********************************************************************
      INTEGER FUNCTION gsfdecl()
!     ------------------------------------------------------------------
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GSFMODFLOW
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      gsfdecl = 1

      Versn =
     &'$Id: gsflow_modflow.f 3927 2008-03-05 20:51:12Z rsregan $'
      IF ( declmodule(Versn(:62)).NE.0 ) RETURN

! Declared Variables
      IF ( declvar('gsflow_modflow', 'KKITER', 'one', 1, 'integer',
     &     'Current iteration in GSFLOW simulation', 'none',
     &     KKITER).NE.0 ) RETURN

      IF ( declvar('gsflow_modflow', 'KPER', 'one', 1, 'integer',
     &     'Stress period number', 'none ',
     &     KPER).NE.0 ) RETURN

      IF ( declvar('gsflow_modflow', 'logunt', 'one', 1, 'integer',
     &     'Fortran unit number for GSFLOW Log File', 'none',
     &     Logunt).NE.0 ) RETURN

      IF ( control_string(Model_mode, 'model_mode').NE.0 ) RETURN
! Declared Parameters
      IF ( Model_mode(:7).NE.'MODFLOW' ) THEN
        IF ( declparam('gsflow_modflow', 'mxsziter', 'one', 'integer',
     &      '15', '2', '200',
     &      'Maximum number of iterations soilzone states are computed',
     &      'Maximum number of iterations soilzone states are computed',
     &      'none').NE.0 ) RETURN
      ENDIF

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
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFUZFMODULE, ONLY: Version_uzf, IGSFLOW
      USE GWFSFRMODULE, ONLY: Version_sfr
      USE GWFLAKMODULE, ONLY: Version_lak
      USE PRMS_BASIN, ONLY: Versn_prms
      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
      IMPLICIT NONE
      INTEGER :: I
      INCLUDE 'openspec.inc'
      INCLUDE 'fmodules.inc'
! Functions
      INTEGER, EXTERNAL :: gsfrun, GET_FUNIT
!     INTEGER, EXTERNAL :: GET_KPER
      EXTERNAL READ_STRESS, SET_STRESS_DATES
      INTRINSIC INDEX, CHAR
! Local Variables
      INTEGER :: INUNIT, MAXUNIT, NC, endtime(6)
      LOGICAL :: exists
!     INTEGER :: kkper_new
C
C-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='1.4.00 11/2/2007')
      PARAMETER (MFVNAM='-2005')
C
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 FNAME
C
      CHARACTER*4 CUNIT(NIUNIT)
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', '    ', 'GHB ',  !  7
     &           'RCH ', 'SIP ', 'DE4 ', '    ', 'OC  ', 'PCG ', 'lmg ',  ! 14
     &           'gwt ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     &           'LAK ', 'LPF ', 'DIS ', '    ', 'PVAL', '    ', 'HOB ',  ! 28
     &           '    ', '    ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     &           '    ', 'HUF2', 'CHOB', 'ETS ', 'DRT ', '    ', 'GMG ',  ! 42
     &           'hyd ', 'SFR ', '    ', 'GAGE', 'LVDA', '    ', 'lmt6',  ! 49
     &           'MNW1', '    ', '    ', 'KDEP', 'SUB ', 'UZF ', '    ',  ! 56
     &           'SWT ', '    ', '    ', '    ', '    ', '    ', '    ',  ! 63
     &           87*'    '/
C     ------------------------------------------------------------------
      gsfinit = 1

      IF ( getvar('gsflow_prms', 'model', 1, 'integer', Model)
     &     .NE.0 ) RETURN

      IF ( Model.NE.3 ) THEN
        IF ( getparam('gsflow_modflow', 'mxsziter', 1, 'integer',
     &       Mxsziter).NE.0 ) RETURN
        IF ( getstep().EQ.0 ) KPER = 0
      ENDIF

C
C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
!     WRITE (*, 3) Versn(23:37)
      WRITE (*, 3)
    3 FORMAT(///, 30X,
     &       'U.S. Geological Survey', /, 11X,
     &       'Coupled Ground-water and Surface-water FLOW model',
     &       ' (GSFLOW)', /, 29X, 'Version 1.0.00 03/05/2008', //, 7X,
     &       'An integration of the Precipitation-Runoff Modeling',
     &       ' System (PRMS)', /, 15X,
     &       'and the Modular Ground-Water Model (MODFLOW-2005)')
      WRITE (*,1) MFVNAM,VERSION
    1 FORMAT (/,34X,'MODFLOW',A,/,
     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &' GROUND-WATER FLOW MODEL',/,29X,'Version ',A/)
      WRITE (*, 5) Versn_prms(19:33)
    5 FORMAT (38X, 'PRMS', /, 10X,
     &    'U.S. GEOLOGICAL SURVEY PRECIPITATION-RUNOFF MODELING SYSTEM',
     &    /, 29X, 'Version 2.', A, //)
      INUNIT = 99
!rsr ?? what should IERR be
      IERR = 6
      IOUTS = 96

      INQUIRE (FILE='modflow.bf',EXIST=exists)
      IF ( exists ) THEN
        OPEN (UNIT=377, FILE='modflow.bf', STATUS='OLD')
        PRINT *, 'Using modflow.bf'
        READ (377, '(A)') FNAME
        IF ( FNAME(:1).EQ.' ' ) STOP 'INVALID modflow.bf'
        CLOSE (377)
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
      WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2,/)
C
C6------ALLOCATE AND READ (AR) PROCEDURE
      IGRID=1
      NSOL=1
      CALL GWF2BAS7AR(INUNIT,CUNIT,VERSION,24,31,32,MAXUNIT,IGRID,12,
     1                HEADNG,26,MFVNAM)
      IF(IUNIT(1).GT.0) CALL GWF2BCF7AR(IUNIT(1),IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7AR(IUNIT(23),IGRID)
      Hdryhuf = 0.0
      IF(IUNIT(37).GT.0) CALL GWF2HUF7AR(IUNIT(37),IUNIT(47),
     1                                 IUNIT(53),Hdryhuf,IGRID)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7AR(IUNIT(2),IGRID)
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
      IF(IUNIT(44).GT.0) CALL GWF2SFR7AR(IUNIT(44),IUNIT(1),IUNIT(23),
     1                           IUNIT(37),IUNIT(15),NSOL,IOUTS,IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1),
     1                                   IUNIT(23),IUNIT(37),IGRID)
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
      IF(IUNIT(42).GT.0) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW7AR(IUNIT(50),IUNIT(9),
     1                     IUNIT(10),0,IUNIT(13),
     2                     0,IUNIT(42),FNAME,IGRID)
!gsf  IF(IUNIT(57).GT.0) CALL GWF2SWT7AR(IUNIT(57),IGRID)
C
C  Observation allocate and read
      CALL OBS2BAS7AR(IUNIT(28),IGRID)
!gsf  IF(IUNIT(33).GT.0) CALL OBS2DRN7AR(IUNIT(33),IUNIT(3),IGRID)
!gsf  IF(IUNIT(34).GT.0) CALL OBS2RIV7AR(IUNIT(34),IUNIT(4),IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7AR(IUNIT(35),IUNIT(7),IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7AR(IUNIT(38),IGRID)

      Logunt = GET_FUNIT()
      OPEN (UNIT=Logunt, FILE='gsflow.log')
!     WRITE (Logunt, 3) Versn(23:37)
      WRITE (Logunt, 3)
      WRITE (Logunt, 1) MFVNAM,VERSION
      WRITE (Logunt, 8)
    8 FORMAT (29X, 'Processes: GWF and OBS', /, 15X,
     &        'Packages: BAS, BCF, CHD, DE4, FHB, GAG, GHB, GMG, HFB,',
     &        /, 25X, 'HUF, LAK, LPF, MNW, PCG, SFR, SIP, UZF, WEL', //)
      WRITE (Logunt, 5) Versn_prms(19:33)
      WRITE (Logunt, 9)
    9 FORMAT ('  Modules: basin_prms, basin_sum_prms,',
     &        ' cascade_prms, ccsolrad_hru_prms,', /, 11X,
     &        'ddsolrad_hru_prms gwflow_casc_prms, hru_sum_prms,',
     &        ' intcp_prms,', /, 11X, 'obs_prms, potet_hamon_hru_prms,',
     &        ' potet_jh_prms, potet_pan_prms,', /, 11X,
     &        'precip_dist2_prms, precip_laps_prms, precip_prms,',
     &        ' soilzone_gsflow,', /, 11X, 'soltab_hru_prms,'
     &        ' srunoff_carea_casc, srunoff_smidx_casc,', /, 11X,
     &        'strmflow_prms, temp_1sta_prms, temp_laps_prms,',
     &        ' temp_dist2_prms,', /, 11X, 'xyz_dist', //)
      WRITE (Logunt, 490) ' Using NAME file: ', FNAME(:NC)
      WRITE (Logunt, 15) Version_uzf, Version_sfr, Version_lak
   15 FORMAT (/, 4(A,/))
      WRITE (Logunt, 2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)

      IF ( Model.NE.3 ) THEN
        IF ( .NOT.ALLOCATED(Stress_dates)) 
     &       ALLOCATE (Stress_dates(NPER+1))
        CALL dattim('start', Nowtime)
        CALL SET_STRESS_DATES(Nowtime)
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
        IGSFLOW = 1
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
      Iterations = 0
      Mfiter_cnt = 0
      IF ( Model.NE.3 ) THEN
        CALL dattim('start', Nowtime)
        CALL dattim('end', endtime)
        WRITE (*, 4) ' Simulation time period:', Nowtime, ' -', endtime
        WRITE (Logunt, 4) ' Simulation time period:', Nowtime, ' -',
     &                    endtime
      ENDIF
    4 FORMAT (/, 2(A, I5, 2('/',I2.2), I3.2, 2(':',I2.2)), /)
      
! rsr, need to skip stress periods if < start time
!      kkper_new = GET_KPER()
!      DO i = 2, kkper_new
!        KPER = i
!        KKPER = i
!        CALL READ_STRESS()
!      ENDDO

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
      USE GSFPRMS2MF, ONLY:Szcheck, Iter_cnt, Maxgziter
      USE GSFPRMS_MODULE, ONLY:Kper_mfo
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
!gsf  USE GWFEVTMODULE, ONLY:NEVTOP
!gsf  USE GWFRCHMODULE, ONLY:NRCHOP
      USE GWFLAKMODULE, ONLY:NLAKESAR,THETA,STGOLD,STGNEW,VOL
      USE GWFUZFMODULE, ONLY:IUZFBND, FINF, VKS
! Added BCF and HDRY for SFR2 11/9/07
      USE GWFBCFMODULE, ONLY: HDRY
      USE PCGMODULE
      USE SIPMODULE
      USE DE4MODULE
      USE GMGMODULE
      IMPLICIT NONE
      INTEGER I
      INCLUDE 'openspec.inc'
      INCLUDE 'fmodules.inc'
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: soilzone_gsflow, GET_KPER
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms, gsflow_budget
      EXTERNAL READ_STRESS
      INTRINSIC MIN
! Local Variables
!     CHARACTER(LEN=19) :: ptext
      INTEGER :: retval, II, kkper_new, KITER, IBDRET, iss, mf2prmsflg
      INTEGER :: IC1, IC2, IR1, IR2, IL1, IL2, IDIR, day
! Added hdrybcf for SFR2 11/19/07
      REAL :: hdrybcf
!***********************************************************************
      gsfrun = 1
      
      IF ( Steady_state.EQ.1 ) THEN
        kkper_new = 1
        Kper_mfo = 2
      ELSEIF ( Model.NE.3 ) THEN
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
C
C7C-----SIMULATE EACH TIME STEP.
!gsf    DO 90 KSTP = 1, NSTP(KPER)
          KSTP = KSTP + 1
          KKSTP = KSTP
          CALL dattim('now', Nowtime)
          day = Nowtime(3)
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
          CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
          IF(IUNIT(20).GT.0) CALL GWF2CHD7AD(KKPER,IGRID)
          IF(IUNIT(1).GT.0) THEN
            CALL GWF2BCF7AD(KKPER,IGRID)
            hdrybcf = HDRY
          ELSE
            hdrybcf = 0.0
          END IF
!gsf      IF(IUNIT(17).GT.0) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
          IF(IUNIT(23).GT.0) CALL GWF2LPF7AD(KKPER,IGRID)
          IF(IUNIT(37).GT.0) CALL GWF2HUF7AD(KKPER,IGRID)
          IF(IUNIT(16).GT.0) CALL GWF2FHB7AD(IGRID)
          IF(IUNIT(22).GT.0) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(15),
     1                                           IGRID)
          IF(IUNIT(50).GT.0) CALL GWF2MNW7AD(IUNIT(1),IUNIT(23),
     1                                       IUNIT(37),IGRID)
C
C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
          CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)

          IF ( Model.EQ.3 ) THEN
            WRITE (Logunt, 25) KPER, KSTP
          WRITE(*,25)KPER,KSTP
   25     FORMAT(' Solving:  Stress period: ',i5,4x,
     &       'Time step: ',i5,4x,'Ground-Water Flow Eqn.')
          ENDIF
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
          Szcheck = 1
          DO 30 KITER = 1, MXITER
            KKITER = KITER
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
            CALL GWF2BAS7FM(IGRID)
            IF(IUNIT(1).GT.0) CALL GWF2BCF7FM(KKITER,KKSTP,
     1                               KKPER,IGRID)
            IF(IUNIT(23).GT.0) CALL GWF2LPF7FM(KKITER,
     1                             KKSTP,KKPER,IGRID)
            IF(IUNIT(37).GT.0) CALL GWF2HUF7FM(KKITER,
     1                             KKSTP,KKPER,IUNIT(47),IGRID)
            IF(IUNIT(21).GT.0) CALL GWF2HFB7FM(IGRID)
            IF(IUNIT(2).GT.0) CALL GWF2WEL7FM(IGRID)
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
!gsf 1                                                    0,IGRID)
!gsf           CALL GWF2RCH7FM(IGRID)
!gsf           IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                    1,IGRID)
!gsf        END IF
            IF(IUNIT(16).GT.0) CALL GWF2FHB7FM(IGRID)
!gsf        IF(IUNIT(17).GT.0) CALL GWF2RES7FM(IGRID)
!gsf        IF(IUNIT(18).GT.0) CALL GWF2STR7FM(IGRID)
!gsf        IF(IUNIT(19).GT.0) CALL GWF2IBS7FM(KKPER,IGRID)
!gsf        IF(IUNIT(39).GT.0) CALL GWF2ETS7FM(IGRID)
!gsf        IF(IUNIT(40).GT.0) CALL GWF2DRT7FM(IGRID)
!
!  Call the PRMS modules that need to be inside the iteration loop
            mf2prmsflg = 0
            IF ( Szcheck.EQ.1 .AND. Model.EQ.1 .AND. iss.EQ.0 ) THEN
              mf2prmsflg = 1
              retval = soilzone_gsflow("run")
              IF ( retval.NE.0 ) THEN
                PRINT 9001, 'soilzone_gsflow', retval
                RETURN
              ENDIF
              retval = gsflow_prms2mf("run")
              IF ( retval.NE.0 ) THEN
                PRINT 9001, 'gsflow_prms2mf', retval
                RETURN
              ENDIF

            ENDIF
!gsf        IF(IUNIT(56).GT.0 .AND. iss.EQ.0) 
!RGN added iunitlpf and iunitbcf, Hdryhuf, Iunithuf, and hdrybcf 1/24/08
            IF(IUNIT(55).GT.0) CALL GWF2UZF1FM(KKPER,KKSTP,KKITER,
     1                              IUNIT(44),IUNIT(22),IUNIT(1),
     2                              IUNIT(23),IUNIT(37),Hdrybcf,
     3                              Hdryhuf,IGRID)

            IF ( mf2prmsflg.EQ.1 .AND. Model.EQ.1 .AND. iss.EQ.0 ) THEN
              retval = gsflow_mf2prms("run")
              IF ( retval.NE.0 ) THEN
                PRINT 9001, 'gsflow_mf2prms', retval
                RETURN
              ENDIF
            ENDIF
!
! Added Iunitbcf, hdrybcf, and Iunitlpf to argument list 11/19/07 and 2/25/08
            IF(IUNIT(44).GT.0) CALL GWF2SFR7FM(IUNIT(1),IUNIT(23),
     1                           IUNIT(37),KKITER,KKPER,KKSTP,IUNIT(22),
     2                           NLAKESAR,THETA,STGOLD,STGNEW,VOL,
     3                           hdrybcf,Hdryhuf,IGRID)

            IF(IUNIT(22).GT.0) CALL GWF2LAK7FM(KKITER,KKPER,
     1                                     IUNIT(44),IUNIT(55),
     2                                     IUZFBND,FINF,VKS,IGRID)
            IF(IUNIT(50).GT.0) CALL GWF2MNW7FM(KKITER,IUNIT(1),
     1                               IUNIT(23),IUNIT(37),IGRID)
!gsf        IF(IUNIT(54).GT.0) CALL GWF2SUB7FM(KKPER,KKITER,IUNIT(9),
!gsf 1                                         IGRID)
!gsf        IF(IUNIT(57).GT.0) CALL GWF2SWT7FM(KKPER,IGRID)
C
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
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
     5               HCSV,IERR,HPCG)
            END IF
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
C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
            IF (ICNVG.EQ.1) GOTO 33
  30      CONTINUE
          KITER = MXITER
C
   33     CONTINUE
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
          IF(IUNIT(2).GT.0) CALL GWF2WEL7BD(KKSTP,KKPER,IGRID)
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
!gsf         IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                     0,IGRID)
!gsf         CALL GWF2RCH7BD(KKSTP,KKPER,IGRID)
!gsf         IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
!gsf 1                                                     1,IGRID)
!gsf      END IF
          IF(IUNIT(16).GT.0) CALL GWF2FHB7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(17).GT.0) CALL GWF2RES7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(18).GT.0) CALL GWF2STR7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(19).GT.0) CALL GWF2IBS7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(39).GT.0) CALL GWF2ETS7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(40).GT.0) CALL GWF2DRT7BD(KKSTP,KKPER,IGRID)
! RGN added Iunitbcf and Iunitlpf, and Hdrybcf 1/24/08 and 2/25/08
          IF(IUNIT(55).GT.0) CALL GWF2UZF1BD(KKSTP,
     1                         KKPER,IUNIT(22),IUNIT(1),IUNIT(23),
     2                         IUNIT(37),Hdrybcf, Hdryhuf, IGRID)
! Added Iunitbcf, hdrybcf, and Iunitlpf to argument list 11/19/07 and 2/25/08
          IF(IUNIT(44).GT.0) CALL GWF2SFR7BD(IUNIT(1),IUNIT(23),
     1                               IUNIT(37),KKSTP,KKPER,IUNIT(15),
     2                        IUNIT(22),IUNIT(46),IUNIT(55),NLAKESAR,
     3                        VOL,NSOL,hdrybcf,Hdryhuf,IGRID)
          IF(IUNIT(22).GT.0) CALL GWF2LAK7BD(KKSTP,KKPER,IUNIT(15),
     1                       IUNIT(46),IUNIT(44),IUNIT(55),NSOL,
     2                                         IUZFBND,FINF,VKS,IGRID)
          IF(IUNIT(50).GT.0) CALL GWF2MNW7BD(NSTP(KPER),KKSTP,KKPER,
     1                      IGRID)
!gsf      IF(IUNIT(54).GT.0) CALL GWF2SUB7BD(KKSTP,KKPER,IGRID)
!gsf      IF(IUNIT(57).GT.0) CALL GWF2SWT7BD(KKSTP,KKPER,IGRID)
C
C  Observation simulated equivalents
          CALL OBS2BAS7SE(IUNIT(28),IGRID)
!gsf      IF(IUNIT(33).GT.0) CALL OBS2DRN7SE(IGRID)
!gsf      IF(IUNIT(34).GT.0) CALL OBS2RIV7SE(IGRID)
          IF(IUNIT(35).GT.0) CALL OBS2GHB7SE(IGRID)
          IF(IUNIT(38).GT.0) CALL OBS2CHD7SE(KKPER,IGRID)
C
C7C5---PRINT AND/OR SAVE DATA.
          CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID)
!gsf      IF(IUNIT(19).GT.0) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),
!gsf 1                                       IGRID)
          IF(IUNIT(37).GT.0)THEN
            IF(IOHUFHDS .NE.0 .OR.IOHUFFLWS .NE.0)
     1         CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
          ENDIF
!gsf      IF(IUNIT(54).GT.0) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54),
!gsf 1                                       IGRID)
!gsf      IF(IUNIT(57).GT.0) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
C
C7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
!gsf      IF(ICNVG.EQ.0) GO TO 110
          IF ( ICNVG.EQ.0 ) THEN
            Convfail_cnt = Convfail_cnt + 1
            PRINT *, "******TIME STEP FAILED TO CONVERGE*****",
     &               Convfail_cnt
            WRITE (Logunt, *) "******TIME STEP FAILED TO CONVERGE*****",
     &                        Convfail_cnt
          ENDIF

          IF ( Model.NE.3 .AND. iss.EQ.0 ) THEN
            IF ( Szcheck.EQ.1 .AND. Model.EQ.1 ) THEN
              Nszchanging = Nszchanging + 1
              II = MIN(Mxsziter, KKITER)
              Iter_cnt(II) = Iter_cnt(II) + 1
!             ptext = '; SZ still changing'
            ELSE
!             ptext = ' '
            ENDIF
            IF ( day.EQ.1 ) THEN
              PRINT 9002, Nowtime(1), Nowtime(2), day, KKPER, KKSTP,
     &                    getstep(), KKITER, Maxgziter, ' '
            ELSEIF ( KKITER.GT.40 ) THEN
              PRINT 9002, Nowtime(1), Nowtime(2), day, KKPER, KKSTP,
     &                    getstep(), KKITER, Maxgziter, ' '
            ENDIF
            WRITE (Logunt, 9002) Nowtime(1), Nowtime(2), day, KKPER,
     &                           KKSTP, getstep(), KKITER, Maxgziter
     &                           , ' '
!    &                           , ptext
          ENDIF
          IF ( KKITER.GT.Max_iters ) Max_iters = KKITER
          Iterations = Iterations + KKITER
          II = MIN(ITDIM, KKITER)
          Mfiter_cnt(II) = Mfiter_cnt(II) + 1
C
C-----END OF TIME STEP (KSTP) AND STRESS PERIOD (KPER) LOOPS
!gsf 90   CONTINUE

!gsf100 CONTINUE
C
C
      IF ( Model.EQ.1 .AND. iss.EQ.0 ) THEN
        retval = gsflow_budget("run")
        IF ( retval.NE.0 ) PRINT 9001, 'gsflow_budget', retval
        IF ( retval.NE.0 ) RETURN
      ENDIF
 9001 FORMAT ('Error in ', A, ' module, arg = run.',
     &        ' Called from gsfrun.', /, 'Return val =', I2)
 9002 FORMAT('Date:', I5, 2('/',I2.2), '; Stress:', I3, '; Step:', I5,
     &       '; Simulation step:', I5, /, 18X, 'MF iterations:', I4,
     &       '; SZ iterations:', I4, A, /)

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
      USE GLOBAL,     ONLY:IOUT, IUNIT, NIUNIT
      USE GSFPRMS2MF, ONLY:Stopcount, Sziters, Iter_cnt
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      LOGICAL lop
      INTEGER i, istep
!***********************************************************************
      gsfclean = 1
C
C
      IF(IUNIT(50).NE.0) CALL GWF2MNW7OT(IGRID)
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
      IF(IUNIT(44).GT.0) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW7DA(IGRID)
!gsf  IF(IUNIT(54).GT.0) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1DA(IGRID)
!gsf  IF(IUNIT(57).GT.0) CALL GWF2SWT7DA(IGRID)
      CALL OBS2BAS7DA(IUNIT(28),IGRID)
!gsf  IF(IUNIT(33).GT.0) CALL OBS2DRN7DA(IGRID)
!gsf  IF(IUNIT(34).GT.0) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7DA(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7DA(IGRID)
      CALL GWF2BAS7DA(IGRID)
C
      IF ( Model.NE.3 ) THEN
        istep = getstep()
        PRINT 9001, Iterations, FLOAT(Iterations)/FLOAT(istep),Max_iters
        WRITE (Logunt, 9001) Iterations, FLOAT(Iterations)/FLOAT(istep),
     &                       Max_iters
        PRINT 9002, Convfail_cnt, istep
        WRITE (Logunt, 9002) Convfail_cnt, istep
        WRITE (Logunt, 9003) 'MF iteration distribution:', Mfiter_cnt
        WRITE (Logunt, '(/)')
        PRINT 9004, Sziters, FLOAT(Sziters)/FLOAT(istep), Stopcount
        WRITE (Logunt, 9004) Sziters, FLOAT(Sziters)/FLOAT(istep),
     &                       Stopcount
        PRINT 9005, Nszchanging
        WRITE (Logunt, 9005) Nszchanging
        WRITE (Logunt, 9003) 'SZ iteration distribution:', Iter_cnt
        WRITE (Logunt, '(/)')
        CALL dattim('now', Nowtime)
        PRINT 9006, ' Simulation end time:', Nowtime
        WRITE (Logunt, 9006) ' Simulation end time:', Nowtime
        DEALLOCATE (Stress_dates)
      ENDIF

C10-----END OF PROGRAM.
      IF(ICNVG.EQ.0) THEN
        WRITE(*,*) ' Failure to converge'
        WRITE (Logunt, *) ' Failure to converge'
      ELSE
        WRITE(*,*) ' Normal termination of simulation'
        WRITE (Logunt, *) ' Normal termination of simulation'
      END IF
      CLOSE (Logunt)

      DO i = 8, NIUNIT
        INQUIRE (UNIT=i, OPENED=lop)
        IF (lop) CLOSE (UNIT=i)
      ENDDO

!gsf  CALL USTOP(' ')
      IF ( Model.EQ.3 ) CALL USTOP(' ')

      gsfclean = 0

 9001 FORMAT ('Iterations:', I6, '; Average iterations:', F6.2,
     &        '; Maximum iterations:', I4)
 9002 FORMAT ('Number of non-convergence:', I5,
     &        '; Number of time steps:', I5, /)
 9003 FORMAT (A, 10I5, /, 6(26X, 10I5, /))
 9004 FORMAT ('SZ iterations:', I6, '; Average SZ iterations:', F6.2,
     &        '; mxsziter reached:', I4)
 9005 FORMAT ('Number of times SZ still changing when MF converged:',
     &        I8, /)
 9006 FORMAT (A, I4, 2('/', I2.2), I3.2, 2(':', I2.2), //)

      END FUNCTION gsfclean
!
      SUBROUTINE GETNAMFIL(FNAME)
C     ******************************************************************
C     GET THE NAME OF THE NAME FILE
C     ******************************************************************
C        SPECIFICATIONS:
C
C     ------------------------------------------------------------------
      CHARACTER*(*) FNAME
      CHARACTER*200 COMLIN
      LOGICAL EXISTS
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
      USE GSFMODFLOW, ONLY:Logunt
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IBDT(8), IEDT(8), IDPM(12)
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IEDT, and write.
      CALL DATE_AND_TIME(VALUES=IEDT)
      WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
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
      USE GSFMODFLOW,    ONLY:IGRID, KKPER, KPER, NSOL, IOUTS
      USE GLOBAL,       ONLY:IUNIT
      IMPLICIT NONE
!***********************************************************************
C7------SIMULATE EACH STRESS PERIOD.
        KKPER = KPER
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
!gsf    IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IGRID)
!gsf    IF(IUNIT(17).GT.0) CALL GWF2RES7RP(IUNIT(17),IGRID)
!gsf    IF(IUNIT(18).GT.0) CALL GWF2STR7RP(IUNIT(18),IGRID)
        IF(IUNIT(20).GT.0) CALL GWF2CHD7RP(IUNIT(20),IGRID)
        IF(IUNIT(44).GT.0) CALL GWF2SFR7RP(IUNIT(44),IUNIT(15),
     1                                     IUNIT(22),KKPER,NSOL,
     2                                     IOUTS,IGRID)
        IF(IUNIT(55).GT.0) CALL GWF2UZF1RP(IUNIT(55),KKPER,IGRID)
        IF(IUNIT(22).GT.0) CALL GWF2LAK7RP(IUNIT(22),IUNIT(1),
     1               IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),
     2               KKPER,NSOL,IOUTS,IGRID)
        IF(IUNIT(46).GT.0.AND.KKPER.EQ.1) CALL GWF2GAG7RP(IUNIT(15),
     1             IUNIT(22),IUNIT(55),NSOL,IGRID)
!gsf    IF(IUNIT(39).GT.0) CALL GWF2ETS7RP(IUNIT(39),IGRID)
!gsf    IF(IUNIT(40).GT.0) CALL GWF2DRT7RP(IUNIT(40),IGRID)
        IF(IUNIT(50).GT.0) CALL GWF2MNW7RP(IUNIT(50),IUNIT(1),
     1                            IUNIT(23),IUNIT(37),KKPER,IGRID)

      END SUBROUTINE READ_STRESS

!     ******************************************************************
!     DETERMINE THE STRESS PERIOD FOR THE CURRENT TIMESTEP
!     ******************************************************************
      INTEGER FUNCTION GET_KPER()
      USE GLOBAL,    ONLY:NPER
      USE GSFMODFLOW, ONLY:Stress_dates, KPER, Nowtime
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
        CALL dattim('start', Nowtime)
        seconds = DBLE(Nowtime(6))
        now = getjulday(Nowtime(2), Nowtime(3), Nowtime(1), Nowtime(4),
     &                  Nowtime(5), seconds)
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
      USE GLOBAL,    ONLY:NPER, PERLEN, ISSFLG
      USE GSFMODFLOW, ONLY:Stress_dates
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


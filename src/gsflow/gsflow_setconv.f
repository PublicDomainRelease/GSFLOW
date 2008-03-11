!***********************************************************************
!     Set conversion factors between PRMS and MODFLOW for GSFLOW
!***********************************************************************
      MODULE GSFCONVERT
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: CLOSEZERO = 1.E-12, ACRE_2_SQFT = 43560.0
      INTEGER :: Nhrucell, Ngwcell
      INTEGER, ALLOCATABLE :: Gwc_col(:), Gwc_row(:)
!   Module Variables
      REAL :: Mfl3t_to_cfs, Mfl3_to_ft3, Acre_inches_to_cfs, Cfs2inches
      REAL :: Acre_inches_to_mfl3, Inch_to_mfl_t, Mfl_to_inch, Sfr_conv
      REAL, ALLOCATABLE :: Cellarea(:), Gvr2cell_conv(:)
      REAL, ALLOCATABLE :: Mfq2inch_conv(:), Mfvol2inch_conv(:)
!   Declared Variables from other modules - basin
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Gvr_cell_id(:)
      REAL, ALLOCATABLE :: Gvr_cell_pct(:)
      END MODULE GSFCONVERT

!     ******************************************************************
!     Set conversion variables for PRMS & MODFLOW states in GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_setconv(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: gsfconvdecl, gsfconvinit
!***********************************************************************
      gsflow_setconv = 0

      IF ( Arg.EQ.'declare' ) THEN
        gsflow_setconv = gsfconvdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        gsflow_setconv = gsfconvinit()
      ENDIF

      END FUNCTION gsflow_setconv

!***********************************************************************
!     gsfconvdecl - set up parameters
!   Declared Parameters
!     gvr_cell_id, gvr_cell_pct
!***********************************************************************
      INTEGER FUNCTION gsfconvdecl()
      USE GSFCONVERT
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      gsfconvdecl = 1

      IF ( declmodule(
     &'$Id: gsflow_setconv.f 3779 2008-01-30 23:16:23Z rsregan $'
     &).NE.0 ) RETURN

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell.EQ.-1 ) RETURN

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell.EQ.-1 ) RETURN

      ALLOCATE (Gvr_cell_id(Nhrucell))
      IF ( declparam('gsfconv', 'gvr_cell_id', 'nhrucell', 'integer',
     &     '0', 'bounded', 'ngwcell',
     &     'Corresponding MODFLOW cell id of each GVR',
     &     'Index of the MODFLOW cell associated with each gravity'//
     &     ' reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_cell_pct(Nhrucell))
      IF ( declparam('gsfconv', 'gvr_cell_pct', 'nhrucell', 'real',
     &     '0.0', '0.0', '1.0',
     &     'Proportion of the MODFLOW cell associated with each GVR',
     &     'Proportion of the MODFLOW cell area associated with each'//
     &     ' gravity reservoir',
     &     'decimal fraction').NE.0 ) RETURN      

! Allocate local module variables
      ALLOCATE (Gvr2cell_conv(Nhrucell), Cellarea(Ngwcell))
      ALLOCATE (Mfq2inch_conv(Nhrucell), Mfvol2inch_conv(Nhrucell))

      gsfconvdecl = 0

      END FUNCTION gsfconvdecl

!***********************************************************************
!     gsfconvinit - Initialize GSFCONVERT module - get parameter values
!***********************************************************************
      INTEGER FUNCTION gsfconvinit()
      USE GSFCONVERT
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Functions
      EXTERNAL SETCONVFACTORS, INITCELLS
!***********************************************************************
      gsfconvinit = 1

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     &     .NE.0 ) RETURN

      IF ( getparam('gsfmap', 'gvr_cell_id', Nhrucell, 'integer',
     &     Gvr_cell_id).NE.0 ) RETURN

      IF ( getparam('gsfmap', 'gvr_cell_pct', Nhrucell, 'real',
     &     Gvr_cell_pct).NE.0 ) RETURN

      CALL SETCONVFACTORS()

      CALL INITCELLS()

      gsfconvinit = 0

      END FUNCTION gsfconvinit

!***********************************************************************
! Set conversion factors to go to and from PRMS and MF units
!***********************************************************************
      SUBROUTINE SETCONVFACTORS()
      USE GSFCONVERT
      USE GLOBAL, ONLY:ITMUNI, LENUNI, IOUT, ISSFLG
      USE GWFBASMODULE, ONLY:DELT
      IMPLICIT NONE
      INTRINSIC SNGL, ABS
      INCLUDE 'fmodules.inc'
! Local Variables
      REAL :: inch_to_mfl, mft_to_sec, prms_dtsec
!     REAL :: inch3_to_mfl3
!***********************************************************************
      IF ( LENUNI.LT.1 .OR. ITMUNI.LT.1 .OR. LENUNI.GT.3 .OR.
     &     ITMUNI.GT.6 ) THEN
        WRITE (IOUT, 9001) LENUNI, ITMUNI
        PRINT *, LENUNI, ITMUNI
        STOP
      ENDIF

      IF ( LENUNI.EQ.1 ) THEN
! Modflow in feet
        inch_to_mfl = 1.0/12.0
!       mfl2_to_acre = 1.0
        Mfl3_to_ft3 = 1.0

      ELSEIF ( LENUNI.EQ.2 ) THEN
! Modflow in meters
        inch_to_mfl = 0.0254
!       mfl2_to_acre = 3.28084*3.28084
        Mfl3_to_ft3 = 3.28084**3.0

      ELSEIF ( LENUNI.EQ.3 ) THEN
! Modflow in centimeters
        inch_to_mfl = 2.54
!       mfl2_to_acre = 328.084*328.084
        Mfl3_to_ft3 = 328.084**3.0
      ELSE
        STOP '***Error, invalid MF Length unit'
      ENDIF
!     inch3_to_mfl3 = inch_to_mfl**3.0
      Mfl_to_inch = 1.0/inch_to_mfl
!     mfl3_to_inch3 = 1.0/inch3_to_mfl3
!     mfl2_to_acre = mfl2_to_acre/ACRE_2_SQFT
      Inch_to_mfl_t = inch_to_mfl/DELT

      IF ( ITMUNI.EQ.1 ) THEN
! Modflow in seconds
        mft_to_sec = 1.0
      ELSEIF ( ITMUNI.EQ.2 ) THEN
! Modflow in minutes
        mft_to_sec = 60.0
      ELSEIF ( ITMUNI.EQ.3 ) THEN
! Modflow in hours
        mft_to_sec = 3600.0
      ELSEIF ( ITMUNI.EQ.4 ) THEN
! Modflow in days
        mft_to_sec = 86400.0
      ELSEIF ( ITMUNI.EQ.5 ) THEN
! Modflow in years
!DANGER, not all years have 365 days
        mft_to_sec = 86400.0*365.0
      ELSE
        STOP '***Error, invalid MF Time Unit'
      ENDIF
      Sfr_conv = mft_to_sec/Mfl3_to_ft3
      Mfl3t_to_cfs = Mfl3_to_ft3/mft_to_sec
! inch over basin (acres) conversion to modflow length cubed
      Acre_inches_to_mfl3 = ACRE_2_SQFT/(Mfl3_to_ft3*12.)

      prms_dtsec = SNGL(deltim()*3600.D0)
      IF ( ISSFLG(1).EQ.0 ) THEN
        IF ( ABS(prms_dtsec-DELT*mft_to_sec).GT.CLOSEZERO ) THEN
          WRITE (IOUT, 9002) prms_dtsec, DELT, mft_to_sec
          PRINT 9002, prms_dtsec, DELT, mft_to_sec
          STOP
        ENDIF
      ENDIF
      ! need to move if < daily time step is used
      Acre_inches_to_cfs = ACRE_2_SQFT/12.0/prms_dtsec

!fix Cfs2inches to compute each time step for variable time steps      
      Cfs2inches = prms_dtsec*12.0*Basin_area_inv/ACRE_2_SQFT

 9001 FORMAT (' Units are undefined. LENUNI and ITMUNI must be > 0:',
     &        'Lenuni =', I4, 'Itmuni =', I4)
 9002 FORMAT (' Time steps must be equal: PRMS dtsec =', F8.1,
     &        ' MODFLOW delt =', F8.1, 'Mft_to_sec =', F13.5)

      END SUBROUTINE SETCONVFACTORS

!***********************************************************************
!***********************************************************************
      SUBROUTINE INITCELLS()
      USE GSFCONVERT
      USE GLOBAL, ONLY:DELR, DELC, NROW, NCOL
      USE GWFBASMODULE, ONLY:DELT
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, irow, icol, icell, j, k, l
!***********************************************************************
      IF ( NROW*NCOL.NE.Ngwcell ) THEN
        PRINT *, 'Dimension error Ngwcell.ne.NROW*NCOL', Ngwcell,
     &           NROW, NCOL
        STOP 'ERROR in gsflow_setconv'
      ENDIF
      ALLOCATE (Gwc_row(Ngwcell), Gwc_col(Ngwcell))
      l = 1
      DO i = 1, Ngwcell, NCOL
        k = i
        DO j = 1, NCOL
          Gwc_col(k) = j
          Gwc_row(k) = l
          k = k + 1
        ENDDO
        l = l + 1
      ENDDO

      DO i = 1, Nhrucell
        icell = Gvr_cell_id(i)
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)
        Cellarea(icell) = DELR(icol)*DELC(irow)
        IF ( Cellarea(icell).LT.CLOSEZERO ) PRINT *,
     &       'Cellarea = 0.0, irow, icol', irow, icol
! PRMS inches in a gravity-flow reservoir to MF rate
        Gvr2cell_conv(i) = Gvr_cell_pct(i)*Inch_to_mfl_t
! MF volume to PRMS inches
        Mfvol2inch_conv(i) = Mfl_to_inch/Cellarea(icell)
! MF discharge to PRMS inches
! rsr, note DELT cannot change during simulation
        Mfq2inch_conv(i) = Mfvol2inch_conv(i)*DELT
      ENDDO

      END SUBROUTINE INITCELLS

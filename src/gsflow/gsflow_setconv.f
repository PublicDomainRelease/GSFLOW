!***********************************************************************
!     Set conversion factors between PRMS and MODFLOW for GSFLOW
!***********************************************************************
      MODULE GSFCONVERT
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: ACRE_2_SQFT = 43560.0
      DOUBLE PRECISION, PARAMETER :: PCT_CHK = 0.00000005D0
      INTEGER, SAVE :: Nhrucell, Ngwcell
      INTEGER, SAVE, ALLOCATABLE :: Gwc_col(:), Gwc_row(:)
!   Module Variables
      REAL, SAVE :: Mfl3t_to_cfs, Mfl3_to_ft3, Acre_inches_to_cfs
      REAL, SAVE :: Acre_inches_to_mfl3, Inch_to_mfl_t, Mfl_to_inch
      REAL, SAVE :: Cfs2inches, Sfr_conv, Mfl2_to_acre
      REAL, SAVE, ALLOCATABLE :: Cellarea(:), Gvr2cell_conv(:)
      REAL, SAVE, ALLOCATABLE :: Mfq2inch_conv(:), Mfvol2inch_conv(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
      END MODULE GSFCONVERT

!     ******************************************************************
!     Set conversion variables for PRMS & MODFLOW states in GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_setconv()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gsfconvdecl, gsfconvinit
!***********************************************************************
      gsflow_setconv = 0

      IF ( Process_flag==1 ) THEN
        gsflow_setconv = gsfconvdecl()
      ELSEIF ( Process_flag==2 ) THEN
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
      INTEGER, EXTERNAL :: declmodule, declparam, getdim
!***********************************************************************
      gsfconvdecl = 1

      IF ( declmodule(
     &'$Id: gsflow_setconv.f 3116 2011-05-17 16:20:01Z rsregan $'
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
      INTEGER, EXTERNAL :: getparam
! Functions
      EXTERNAL SETCONVFACTORS, INITCELLS
!***********************************************************************
      gsfconvinit = 1

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
      USE PRMS_BASIN, ONLY: Basin_area_inv, NEARZERO
      USE PRMS_OBS, ONLY: Timestep_seconds
      IMPLICIT NONE
      INTRINSIC SNGL, ABS
! Local Variables
      REAL :: inch_to_mfl, mft_to_sec
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
        Mfl2_to_acre = 1.0
        Mfl3_to_ft3 = 1.0

      ELSEIF ( LENUNI.EQ.2 ) THEN
! Modflow in meters
        inch_to_mfl = 0.0254
        Mfl2_to_acre = 3.280839895*3.280839895
        Mfl3_to_ft3 = 3.280839895**3.0

      ELSEIF ( LENUNI.EQ.3 ) THEN
! Modflow in centimeters
        inch_to_mfl = 2.54
        Mfl2_to_acre = 328.0839895*328.0839895
        Mfl3_to_ft3 = 328.0839895**3.0
      ELSE
        STOP '***Error, invalid MF Length unit'
      ENDIF
!     inch3_to_mfl3 = inch_to_mfl**3.0
      Mfl_to_inch = 1.0/inch_to_mfl
!     mfl3_to_inch3 = 1.0/inch3_to_mfl3
      Mfl2_to_acre = Mfl2_to_acre/ACRE_2_SQFT
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

      IF ( ISSFLG(1).EQ.0 ) THEN
        IF ( ABS(Timestep_seconds-DELT*mft_to_sec).GT.NEARZERO ) THEN
          WRITE (IOUT, 9002) Timestep_seconds, DELT, mft_to_sec
          PRINT 9002, Timestep_seconds, DELT, mft_to_sec
          STOP
        ENDIF
      ENDIF
      ! need to move if < daily time step is used
      Acre_inches_to_cfs = ACRE_2_SQFT/12.0/Timestep_seconds

!fix Cfs2inches to compute each time step for variable time steps      
      Cfs2inches = Timestep_seconds*12.0*Basin_area_inv/ACRE_2_SQFT

 9001 FORMAT (' Units are undefined. LENUNI and ITMUNI must be > 0:',
     &        'Lenuni =', I4, 'Itmuni =', I4)
 9002 FORMAT (' Time steps must be equal: PRMS dtsec =', F12.4,
     &        ' MODFLOW delt =', F12.4, ' Mft_to_sec =', F12.4)

      END SUBROUTINE SETCONVFACTORS

!***********************************************************************
!***********************************************************************
      SUBROUTINE INITCELLS()
      USE GSFCONVERT
      USE GLOBAL, ONLY: DELR, DELC, NROW, NCOL
      USE GWFBASMODULE, ONLY: DELT
      USE GSFMODFLOW, ONLY: Logunt
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, irow, icol, j, k, l, ic, ii
      DOUBLE PRECISION :: totalarea, pctdiff
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: cell_pct, newpct
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: temp_pct
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

      ALLOCATE (cell_pct(Ngwcell), newpct(Ngwcell), temp_pct(Nhrucell))
      DO i = 1, Ngwcell
        cell_pct(i) = 0.0D0
        newpct(i) = 0.0D0
      ENDDO
      DO i = 1, Nhrucell
        ic = Gvr_cell_id(i)
        IF ( ic==0 ) THEN
          PRINT *, 'gvr_cell_id = 0 for gvr:', i
          PRINT *, 'Be sure gvr_cell_id is in the Parameter File'
          STOP
        ENDIF
        temp_pct(i) = DBLE(Gvr_cell_pct(i))
        cell_pct(ic) = cell_pct(ic) + temp_pct(i)
      ENDDO

! way to adjust Gvr_cell_pct, rsr
!     WRITE (841,*) '####'
!     WRITE (841,*) 'gvr_cell_pct 12'
!     WRITE (841,*) '1'
!     WRITE (841,*) 'nhrucell'
!     WRITE (841,*) Nhrucell
!     WRITE (841,*) '2'
      DO i = 1, Nhrucell
        ic = Gvr_cell_id(i)
        temp_pct(i) = temp_pct(i) +
     &                temp_pct(i)*(1.0D0-cell_pct(ic))/cell_pct(ic)
        Gvr_cell_pct(i) = SNGL(temp_pct(i))
        newpct(ic) = newpct(ic) + temp_pct(i)
!       WRITE (841,'(f15.13)') temp_pct(i)
      ENDDO
!     WRITE (841,'******, old gvr_cell_pct')
!     WRITE (841,'(f14.11)') cell_pct
!     STOP

      ii = 0
      totalarea = 0.0
      DO i = 1, Nhrucell
        ic = Gvr_cell_id(i)
        IF ( Gvr_cell_pct(i)>0.0 ) THEN
          pctdiff = newpct(ic) - 1.0D0
          IF ( pctdiff>PCT_CHK ) THEN
            PRINT *, 'Will make some water in MF cell:', ic,
     &               newpct(ic), cell_pct(ic)
          ELSEIF ( pctdiff<-PCT_CHK ) THEN
            PRINT *, 'Will lose some water in cell:', ic,
     &               newpct(ic), cell_pct(ic)
          ENDIF
          totalarea = totalarea + pctdiff
          ii = ii + 1
        ENDIF
        irow = Gwc_row(ic)
        icol = Gwc_col(ic)
        Cellarea(ic) = DELR(icol)*DELC(irow)
        IF ( Cellarea(ic).LT.NEARZERO ) PRINT *,
     &       'Cellarea = 0.0, irow, icol', irow, icol
! PRMS inches in a gravity-flow reservoir to MF rate
        Gvr2cell_conv(i) = Gvr_cell_pct(i)*Inch_to_mfl_t
! MF volume to PRMS inches
        Mfvol2inch_conv(i) = Mfl_to_inch/Cellarea(ic)
! MF discharge to PRMS inches
! rsr, note DELT cannot change during simulation
        Mfq2inch_conv(i) = Mfvol2inch_conv(i)*DELT
      ENDDO
      WRITE (Logunt, *) 'Percentage difference between cell mapping:',
     &                  totalarea/FLOAT(ii)*100.0D0

      DEALLOCATE (cell_pct, newpct)

      END SUBROUTINE INITCELLS

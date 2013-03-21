!***********************************************************************
!     Set conversion factors between PRMS and MODFLOW for GSFLOW
!***********************************************************************
      MODULE GSFCONVERT
      IMPLICIT NONE
!   Local Variables
      DOUBLE PRECISION, PARAMETER :: ACRE_2_SQFT = 43560.0D0
      REAL, PARAMETER :: PCT_CHK = 0.000001
      INTEGER, SAVE :: Nhrucell, Ngwcell
      INTEGER, SAVE, ALLOCATABLE :: Gwc_col(:), Gwc_row(:)
      CHARACTER(LEN=14), PARAMETER :: MODNAME = 'gsflow_setconv'
      CHARACTER(LEN=26), PARAMETER::PROCNAME='GSFLOW Conversion Factors'
!   Module Variables
      REAL, SAVE :: Mfl_to_inch, Inch_to_mfl_t
      DOUBLE PRECISION, SAVE :: Mfl3t_to_cfs, Mfl3_to_ft3
!      DOUBLE PRECISION, SAVE :: Acre_inches_to_cfs
      DOUBLE PRECISION, SAVE :: Acre_inches_to_mfl3
      DOUBLE PRECISION, SAVE :: Mfl2_to_acre, Sfr_conv
      DOUBLE PRECISION, SAVE :: Cfs2inches
      REAL, SAVE, ALLOCATABLE :: Cellarea(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gvr2cell_conv(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Mfq2inch_conv(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Mfvol2inch_conv(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
      END MODULE GSFCONVERT

!     ******************************************************************
!     Set conversion variables for PRMS & MODFLOW states in GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_setconv()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gsfconvdecl, gsfconvinit
!***********************************************************************
      gsflow_setconv = 0

      IF ( Process(:4)=='decl' ) THEN
        gsflow_setconv = gsfconvdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
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
      USE PRMS_MODULE, ONLY: Version_gsflow_setconv, Gsflow_setconv_nc
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, getdim
      EXTERNAL read_error
! Local Variables
      INTEGER :: n
!***********************************************************************
      gsfconvdecl = 1

      Version_gsflow_setconv =
     &'$Id: gsflow_setconv.f 4263 2012-03-08 18:29:32Z rsregan $'
      Gsflow_setconv_nc = INDEX( Version_gsflow_setconv, 'Z' )
      n = INDEX( Version_gsflow_setconv, '.f' ) + 1
      IF ( declmodule(Version_gsflow_setconv(6:n), PROCNAME,
     +     Version_gsflow_setconv(n+2:Gsflow_setconv_nc))/=0 ) STOP

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell==-1 ) RETURN

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell==-1 ) RETURN

      ALLOCATE ( Gvr_cell_id(Nhrucell) )
      IF ( declparam(MODNAME, 'gvr_cell_id', 'nhrucell', 'integer',
     &     '0', 'bounded', 'ngwcell',
     &     'Corresponding MODFLOW cell id of each GVR',
     &     'Index of the MODFLOW cell associated with each gravity'//
     &     ' reservoir',
     &     'none')/=0 ) CALL read_error(1, 'gvr_cell_id')

      ALLOCATE ( Gvr_cell_pct(Nhrucell) )
      IF ( declparam(MODNAME, 'gvr_cell_pct', 'nhrucell', 'real',
     &     '0.0', '0.0', '1.0',
     &     'Proportion of the MODFLOW cell associated with each GVR',
     &     'Proportion of the MODFLOW cell area associated with each'//
     &     ' gravity reservoir',
     &     'decimal fraction')/=0 ) CALL read_error(1, 'gvr_cell_id') 

! Allocate local module variables
      ALLOCATE ( Gvr2cell_conv(Nhrucell), Cellarea(Ngwcell) )
      ALLOCATE ( Mfq2inch_conv(Nhrucell), Mfvol2inch_conv(Nhrucell) )

      gsfconvdecl = 0
      END FUNCTION gsfconvdecl

!***********************************************************************
!     gsfconvinit - Initialize GSFCONVERT module - get parameter values
!***********************************************************************
      INTEGER FUNCTION gsfconvinit()
      USE GSFCONVERT
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Functions
      EXTERNAL SETCONVFACTORS, INITCELLS, read_error
! Local Variables
      INTEGER :: i
!***********************************************************************
      gsfconvinit = 1

      IF ( getparam(MODNAME, 'gvr_cell_id', Nhrucell, 'integer',
     &     Gvr_cell_id)/=0 ) CALL read_error(2, 'gvr_cell_id')
      IF ( Nhru==Nhrucell ) THEN
        DO i = 1, Nhru
          Gvr_cell_pct(i) = 1.0
        ENDDO
      ELSE
        IF ( getparam(MODNAME, 'gvr_cell_pct', Nhrucell, 'real',
     &       Gvr_cell_pct)/=0 ) CALL read_error(2, 'gvr_cell_id')
      ENDIF

      CALL SETCONVFACTORS()

      CALL INITCELLS()

      gsfconvinit = 0
      END FUNCTION gsfconvinit

!***********************************************************************
! Set conversion factors to go to and from PRMS and MF units
!***********************************************************************
      SUBROUTINE SETCONVFACTORS()
      USE GSFCONVERT
      USE GLOBAL, ONLY: ITMUNI, LENUNI, IOUT, ISSFLG
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_BASIN, ONLY: Basin_area_inv, NEARZERO
      USE PRMS_OBS, ONLY: Timestep_seconds
      IMPLICIT NONE
      INTRINSIC SNGL, ABS
! Local Variables
      DOUBLE PRECISION :: inch_to_mfl, mft_to_sec
!***********************************************************************
      IF ( LENUNI<1 .OR. ITMUNI<1 .OR. LENUNI>3 .OR. ITMUNI>6 ) THEN
        WRITE ( IOUT, 9001 ) LENUNI, ITMUNI
        PRINT 9001, LENUNI, ITMUNI
        STOP
      ENDIF

      IF ( LENUNI==1 ) THEN
! Modflow in feet
        inch_to_mfl = 1.0D0/12.0D0
        Mfl2_to_acre = 1.0D0
        Mfl3_to_ft3 = 1.0D0

      ELSEIF ( LENUNI==2 ) THEN
! Modflow in meters
        inch_to_mfl = 0.0254D0
        Mfl2_to_acre = 3.280839895D0*3.280839895D0
        Mfl3_to_ft3 = 3.280839895D0**3.0D0

      ELSEIF ( LENUNI==3 ) THEN
! Modflow in centimeters
        inch_to_mfl = 2.54D0
        Mfl2_to_acre = 328.0839895D0*328.0839895D0
        Mfl3_to_ft3 = 328.0839895D0**3.0D0
      ELSE
        STOP '***ERROR, invalid MODFLOW Length unit'
      ENDIF
      Mfl_to_inch = 1.0D0/inch_to_mfl
      Mfl2_to_acre = Mfl2_to_acre/ACRE_2_SQFT
      Inch_to_mfl_t = inch_to_mfl/DELT

      IF ( ITMUNI==1 ) THEN
! Modflow in seconds
        mft_to_sec = 1.0D0
      ELSEIF ( ITMUNI==2 ) THEN
! Modflow in minutes
        mft_to_sec = 60.0D0
      ELSEIF ( ITMUNI==3 ) THEN
! Modflow in hours
        mft_to_sec = 3600.0D0
      ELSEIF ( ITMUNI==4 ) THEN
! Modflow in days
        mft_to_sec = 86400.0D0
      ELSEIF ( ITMUNI==5 ) THEN
! Modflow in years
!DANGER, not all years have 365 days
        mft_to_sec = 86400.0D0*365.0D0
      ELSE
        STOP '***ERROR, invalid MODFLOW Time Unit'
      ENDIF
      Sfr_conv = mft_to_sec/Mfl3_to_ft3
      Mfl3t_to_cfs = Mfl3_to_ft3/mft_to_sec
! inch over basin (acres) conversion to modflow length cubed
      Acre_inches_to_mfl3 = ACRE_2_SQFT/(Mfl3_to_ft3*12.0D0)

      IF ( ISSFLG(1)==0 ) THEN
        IF ( ABS(Timestep_seconds-DELT*mft_to_sec)>NEARZERO ) THEN
          WRITE (IOUT, 9002) Timestep_seconds, DELT, mft_to_sec
          PRINT 9002, Timestep_seconds, DELT, mft_to_sec
          STOP
        ENDIF
      ENDIF
      ! need to move if < daily time step is used
!      Acre_inches_to_cfs = ACRE_2_SQFT/12.0D0/Timestep_seconds

!fix Cfs2inches to compute each time step for variable time steps      
      Cfs2inches = Timestep_seconds*12.0D0*Basin_area_inv/ACRE_2_SQFT

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
      USE GWFUZFMODULE, ONLY: IUZFBND
      USE GSFMODFLOW, ONLY: Logunt
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, irow, icol, j, k, l, ic, ii, ierr
      REAL :: pctdiff
      REAL, ALLOCATABLE, DIMENSION(:) :: cell_pct, newpct, temp_pct
      DOUBLE PRECISION :: totalarea
!***********************************************************************
      IF ( NROW*NCOL/=Ngwcell ) THEN
        PRINT *, 'ERROR, dimension Ngwcell not equal to NROW*NCOL',
     &           Ngwcell, NROW, NCOL
        PRINT *, '       Check for use of correct parameter file'
        STOP
      ENDIF

      ALLOCATE ( Gwc_row(Ngwcell), Gwc_col(Ngwcell) )
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

      ALLOCATE ( cell_pct(Ngwcell), newpct(Ngwcell), temp_pct(Nhrucell))
      cell_pct = 0.0
      ierr = 0
      DO i = 1, Nhrucell
        ic = Gvr_cell_id(i)
        IF ( ic==0 ) THEN
          PRINT *, 'ERROR, gvr_cell_id = 0 for gvr:', i
          PRINT *, 'Be sure gvr_cell_id is in the Parameter File'
          ierr = 1
        ENDIF
        temp_pct(i) = Gvr_cell_pct(i)
        cell_pct(ic) = cell_pct(ic) + Gvr_cell_pct(i)
      ENDDO
      IF ( ierr==1 ) STOP

      ierr = 0
      DO i = 1, Ngwcell
        irow = Gwc_row(i)
        icol = Gwc_col(i)
        IF ( IUZFBND(icol, irow)==0 ) CYCLE
        IF ( cell_pct(i)<0.99 ) THEN
          WRITE ( Logunt, * ) 'Portion of cell not included in',
     &                    ' gvr_cell_pct mapping, cell:', i, cell_pct(i)
          ierr = 1
        ENDIF
        IF ( cell_pct(i)>1.00001 ) THEN
          WRITE ( Logunt, * ) 'Extra portion of cell included in',
     &                    ' gvr_cell_pct mapping, cell:', i, cell_pct(i)
          ierr = 1
        ENDIF
      ENDDO
!      IF ( ierr==1 ) STOP 'ERROR, check gsflow.log for messages'

! way to adjust Gvr_cell_pct, rsr
!     WRITE (841,*) '####'
!     WRITE (841,*) 'gvr_cell_pct 12'
!     WRITE (841,*) '1'
!     WRITE (841,*) 'nhrucell'
!     WRITE (841,*) Nhrucell
!     WRITE (841,*) '2'
      newpct = 0.0
      DO i = 1, Nhrucell
        ic = Gvr_cell_id(i)
        temp_pct(i) = temp_pct(i) +
     &                temp_pct(i)*(1.0-cell_pct(ic))/cell_pct(ic)
        Gvr_cell_pct(i) = temp_pct(i)
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
          pctdiff = newpct(ic) - 1.0
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
        IF ( Cellarea(ic)<NEARZERO ) PRINT *,
     &       'Cellarea = 0.0, irow, icol', irow, icol
! PRMS inches in a gravity-flow reservoir to MF rate
        Gvr2cell_conv(i) = Gvr_cell_pct(i)*Inch_to_mfl_t
! MF volume to PRMS inches
        Mfvol2inch_conv(i) = Mfl_to_inch/Cellarea(ic)
! MF discharge to PRMS inches
! rsr, note DELT cannot change during simulation
        Mfq2inch_conv(i) = Mfvol2inch_conv(i)*DELT
      ENDDO

      WRITE ( Logunt, * ) 'Percentage difference between cell mapping:',
     &                    totalarea/FLOAT(ii)*100.0D0

      DEALLOCATE ( cell_pct, newpct, temp_pct, Gvr_cell_pct )

      END SUBROUTINE INITCELLS

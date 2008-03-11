!***********************************************************************
! Determine whether transpiration is occurring and compute the potential
! evapotranspiration for each HRU using pan evaporation data and no
! temperature information
!***********************************************************************
      MODULE PRMS_POTET_PAN
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Nevap, Lday, Datetime(6)
      REAL, ALLOCATABLE :: Daily_potet(:)
!   Declared Variables
      INTEGER :: Basin_transp_on
      INTEGER, ALLOCATABLE :: Transp_on(:)
      REAL :: Basin_potet
      REAL, ALLOCATABLE :: Potet(:)
!   Declared Variables from other modules - obs
      REAL, ALLOCATABLE :: Pan_evap(:)
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Transp_beg(:), Transp_end(:)
      INTEGER, ALLOCATABLE :: Hru_pansta(:)
      REAL, ALLOCATABLE :: Hru_area(:), Epan_coef(:)
      END MODULE PRMS_POTET_PAN

!***********************************************************************
!     Main potet_pan_prms routine
!***********************************************************************
      INTEGER FUNCTION potet_pan_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: petdecl_pan, petinit_pan, petrun_pan
!***********************************************************************
      potet_pan_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        potet_pan_prms = petrun_pan()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        potet_pan_prms = petdecl_pan()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        potet_pan_prms = petinit_pan()
      ENDIF

      END FUNCTION potet_pan_prms

!***********************************************************************
!     petdecl - set up parameters for potential et computations
!   Declared Parameters
!     transp_beg, transp_end, hru_area, hru_pansta
!***********************************************************************
      INTEGER FUNCTION petdecl_pan()
      USE PRMS_POTET_PAN
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      petdecl_pan = 1

      IF ( declmodule(
     +'$Id: potet_pan_prms.f 3584 2007-10-04 17:51:42Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nevap = getdim('nevap')
      IF ( Nevap.EQ.-1 ) RETURN
      IF ( Nevap.EQ.0 ) Nevap = 1

      ALLOCATE (Daily_potet(Nhru))
      IF ( declpri('potetpan_lday', 1, 'integer', Lday).NE.0 ) RETURN
      IF ( declpri('potetpan_daily_potet', Nhru, 'real',
     +     Daily_potet).NE.0 ) RETURN

      ALLOCATE (Transp_on(Nhru))
      IF ( declvar('potet', 'transp_on', 'nhru', Nhru, 'integer',
     +     'Switch indicating whether transpiration is occurring'//
     +     ' (0=no; 1=yes)',
     +     'none',
     +     Transp_on).NE.0 ) RETURN

      IF ( declvar('potet', 'basin_transp_on', 'one', 1, 'integer',
     +     'Switch indicating whether transpiration is occurring'//
     +     ' anywhere in the basin (0=no; 1=yes)',
     +     'none',
     +     Basin_transp_on).NE.0 ) RETURN

      ALLOCATE (Potet(Nhru))
      IF ( declvar('potet', 'potet', 'nhru', Nhru, 'real',
     +     'Potential evapotranspiration on an HRU',
     +     'inches',
     +     Potet).NE.0 ) RETURN

      IF ( declvar('potet', 'basin_potet', 'one', 1, 'real',
     +     'Basin area-weighted average of potential et',
     +     'inches',
     +     Basin_potet).NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('potet', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      ALLOCATE (Transp_beg(Nhru))
      IF ( declparam('potet', 'transp_beg', 'nhru', 'integer',
     +     '4', '1', '12',
     +     'Month to begin transpiration period',
     +     'Month to begin transpiration period',
     +     'month').NE.0 ) RETURN

      ALLOCATE (Transp_end(Nhru))
      IF ( declparam('potet', 'transp_end', 'nhru', 'integer',
     +     '10', '1', '12',
     +     'Month to stop transpiration period',
     +     'Month to stop transpiration computations;'//
     +     ' Transpiration is computed thru end of previous month',
     +     'month').NE.0 ) RETURN

      ALLOCATE (Epan_coef(MAXMO))
      IF ( declparam('potet', 'epan_coef', 'nmonths', 'real',
     +     '1.0', '0.2', '3.0',
     +     'Evaportaion pan coefficient', 'Evaporation pan coefficient',
     +     'none').NE.0 ) RETURN      

      ALLOCATE (Hru_pansta(Nhru))
      IF ( declparam('potet', 'hru_pansta', 'nevap', 'integer',
     +     '1', 'bounded', 'nevap',
     +     'Index of pan evaporation station for each HRU',
     +     'Index of pan evaporation station used to compute'//
     +     ' HRU potential evapotranspiration',
     +     'none').NE.0 ) RETURN      

! Allocate arrays for variables from other modules
      ALLOCATE (Pan_evap(Nevap), Hru_route_order(Nhru))

      petdecl_pan = 0
      END FUNCTION petdecl_pan

!***********************************************************************
!     petinit - Initialize potet module - get parameter values,
!                set initial transp_on switch
!***********************************************************************
      INTEGER FUNCTION petinit_pan()
      USE PRMS_POTET_PAN
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: mo, i, j
!***********************************************************************
      petinit_pan = 1

      IF ( getparam('potet', 'transp_beg', Nhru, 'integer', Transp_beg)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'transp_end', Nhru, 'integer', Transp_end)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'epan_coef', MAXMO, 'real', Epan_coef)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( getparam('potet', 'hru_pansta', Nhru, 'integer', Hru_pansta)
     +     .NE.0 ) RETURN

      CALL dattim('start', Datetime)
      mo = Datetime(2)

      IF ( getstep().EQ.0 ) THEN
        Potet = 0.0
        Basin_potet = 0.0
        Daily_potet = 0.0
        Lday = 0
      ENDIF

      Basin_transp_on = 0
      Transp_on = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( (Transp_end(i)-Transp_beg(i)).GT.0 ) THEN
          IF ( mo.GE.Transp_beg(i) .AND. mo.LT.Transp_end(i) )
     +         Transp_on(i) = 1

        ELSE
          IF ( (mo.GE.Transp_beg(i) .AND. mo.LT.13) .OR.
     +         (mo.GT.0 .AND. mo.LT.Transp_end(i)) ) Transp_on(i) = 1

        ENDIF
        IF ( Transp_on(i).EQ.1 ) Basin_transp_on = 1
        IF ( Hru_pansta(i).LT.1 ) Hru_pansta = 1
      ENDDO

      petinit_pan = 0
      END FUNCTION petinit_pan

!***********************************************************************
!      petrun - Keeps track of transpiration on or off and computes
!               potential et for each HRU each day
!***********************************************************************
      INTEGER FUNCTION petrun_pan()
      USE PRMS_POTET_PAN
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC SNGL
! Local Variables
      INTEGER :: mo, i, day, k, j
      REAL :: factor
      DOUBLE PRECISION :: dt
!***********************************************************************
      petrun_pan = 1

      dt = deltim()
      CALL dattim('now', Datetime)
      mo = Datetime(2)
      day = Datetime(3)
      Basin_potet = 0.0

!******Set switch for active transpiration period

      IF ( Lday.NE.day ) THEN

        Lday = day

        Basin_transp_on = 0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

          IF ( day.EQ.1 ) THEN

!******If transpiration switch on, check for end of period

            IF ( Transp_on(i).EQ.1 ) THEN
              IF ( mo.EQ.Transp_end(i) ) Transp_on(i) = 0
            ELSE
              IF ( mo.EQ.Transp_beg(i) ) Transp_on(i) = 1
            ENDIF
          ENDIF
          IF ( Transp_on(i).EQ.1 ) Basin_transp_on = 1
        ENDDO
      ENDIF

!****Compute potential et for each HRU using pan evaporation formulation

      IF ( getvar('obs', 'pan_evap', Nevap, 'real', Pan_evap)
     +       .NE.0 ) RETURN

      IF ( dt.LT.23.999D0 ) THEN
        factor = SNGL(dt/24.D0)
      ELSE
        factor = 1.
      ENDIF
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        k = Hru_pansta(i)
        Daily_potet(i) = Pan_evap(k)*Epan_coef(mo)
        Potet(i) = factor*Daily_potet(i)
        Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
      ENDDO

      Basin_potet = Basin_potet*Basin_area_inv

      petrun_pan = 0

      END FUNCTION petrun_pan

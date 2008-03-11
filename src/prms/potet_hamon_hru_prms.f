!***********************************************************************
! Determine whether transpiration is occurring and compute the potential
! evapotranspiration for each HRU
!***********************************************************************
      MODULE PRMS_POTET_HAMON_HRU
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Lday, Datetime(6)
      INTEGER, ALLOCATABLE :: Transp_check(:)
      REAL, ALLOCATABLE :: Daily_potet(:), Tmax_sum(:)
!   Declared Variables
      INTEGER :: Basin_transp_on
      INTEGER, ALLOCATABLE :: Transp_on(:)
      REAL :: Basin_potet
      REAL, ALLOCATABLE :: Potet(:)
!   Declared Variables from other modules - temp
      REAL, ALLOCATABLE :: Tmaxf(:), Tmaxc(:), Tavgc(:)
!   Declared Variables from other modules - soltab
      REAL, ALLOCATABLE :: Soltab_sunhrs(:, :)
!   Declared Variables from other modules - basin
!dbg  INTEGER :: Prt_debug
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Temp_units
      INTEGER, ALLOCATABLE :: Transp_beg(:), Transp_end(:)
      REAL, ALLOCATABLE :: Transp_tmax(:), Hru_area(:), Hamon_coef(:)
      END MODULE PRMS_POTET_HAMON_HRU

!***********************************************************************
!     Main potet_hamon routine
!***********************************************************************
      INTEGER FUNCTION potet_hamon_hru_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: pethdecl, pethinit, pethrun
!***********************************************************************
      potet_hamon_hru_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        potet_hamon_hru_prms = pethrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        potet_hamon_hru_prms = pethdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        potet_hamon_hru_prms = pethinit()
      ENDIF

      END FUNCTION potet_hamon_hru_prms

!***********************************************************************
!     pethdecl - set up parameters for potential et computations
!   Declared Parameters
!     transp_beg, transp_end, transp_tmax, hamon_coef
!     hru_area, temp_units
!***********************************************************************
      INTEGER FUNCTION pethdecl()
      USE PRMS_POTET_HAMON_HRU
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      pethdecl = 1

      IF ( declmodule(
     +'$Id: potet_hamon_hru_prms.f 3584 2007-10-04 17:51:42Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      ALLOCATE (Daily_potet(Nhru), Tmax_sum(Nhru), Transp_check(Nhru))
      IF ( declpri('potetham_lday', 1, 'integer', Lday).NE.0 ) RETURN
      IF ( declpri('potetham_daily_potet', Nhru, 'real',
     +     Daily_potet).NE.0 ) RETURN
      IF ( declpri('potetham_tmax_sum', Nhru, 'real', Tmax_sum)
     +     .NE.0 ) RETURN
      IF ( declpri('potetham_transp_check', Nhru, 'integer',
     +     Transp_check).NE.0 ) RETURN

      ALLOCATE (Transp_on(Nhru))
      IF ( declvar('potet', 'transp_on', 'nhru', Nhru, 'integer',
     +     'Switch indicating whether transpiration is occurring'//
     +     ' (0=no; 1=yes)',
     +     'none',
     +     Transp_on).NE.0 ) RETURN

      IF ( declvar('potet', 'basin_transp_on', 'one', 1,
     +     'integer',
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
     +     'Month to begin testing for transpiration',
     +     'Month to begin summing tmaxf for each HRU; when sum is'//
     +     ' >= to transp_tmax, transpiration begins',
     +     'month').NE.0 ) RETURN

      ALLOCATE (Transp_end(Nhru))
      IF ( declparam('potet', 'transp_end', 'nhru', 'integer',
     +     '10', '1', '12',
     +     'Month to stop transpiration period',
     +     'Month to stop transpiration computations;'//
     +     ' Transpiration is computed thru end of previous month',
     +     'month').NE.0 ) RETURN

      ALLOCATE (Transp_tmax(Nhru))
      IF ( declparam('potet', 'transp_tmax', 'nhru', 'real',
     +     '500.', '0.', '1000.',
     +     'Tmax index to determine start of transpiration',
     +     'Temperature index to determine the specific date of the'//
     +     ' start of the transpiration period.  Subroutine sums tmax'//
     +     ' for each HRU starting with the first day of month'//
     +     ' transp_beg.  When the sum exceeds this index,'//
     +     ' transpiration begins',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Hamon_coef(MAXMO))
      IF ( declparam('potet', 'hamon_coef', 'nmonths', 'real',
     +     '.0055', '.004', '.008',
     +     'Monthly air temp coefficient - Hamon',
     +     'Monthly air temperature coefficient used in Hamon'//
     +     ' potential evapotranspiration computations, see'//
     +     ' PRMS manual',
     +     '????').NE.0 ) RETURN

      IF ( declparam('potet', 'temp_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for observed temperature',
     +     'Units for observed temperature (0=Fahrenheit; 1=Celsius)',
     +     'none').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Tmaxf(Nhru), Tmaxc(Nhru), Tavgc(Nhru))
      ALLOCATE (Soltab_sunhrs(MAXDAY, Nhru), Hru_route_order(Nhru))

      pethdecl = 0
      END FUNCTION pethdecl

!***********************************************************************
!     pethinit - Initialize potet module - get parameter values,
!                set initial transp_on switch
!***********************************************************************
      INTEGER FUNCTION pethinit()
      USE PRMS_POTET_HAMON_HRU
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: mo, day, i, j
!***********************************************************************
      pethinit = 1

      IF ( getparam('potet', 'transp_beg', Nhru, 'integer', Transp_beg)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'transp_end', Nhru, 'integer', Transp_end)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'transp_tmax', Nhru, 'real', Transp_tmax)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'hamon_coef', MAXMO, 'real', Hamon_coef)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

!dbg  IF ( get var('basin', 'prt_debug', 1, 'integer', Prt_debug)
!dbg +     .NE.0 ) RETURN

      IF ( getparam('potet', 'temp_units', 1, 'integer', Temp_units)
     +     .NE.0 ) RETURN

      IF ( getvar('soltab', 'soltab_sunhrs', MAXDAY*Nhru, 'real',
     +     Soltab_sunhrs).NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      CALL dattim('start', Datetime)
      mo = Datetime(2)
      day = Datetime(3)

      IF ( getstep().EQ.0 ) THEN
        Tmax_sum = 0.0
        Potet = 0.0
        Basin_potet = 0.0
        Daily_potet = 0.0
        Lday = 0
      ENDIF

      Basin_transp_on = 0
      Transp_on = 0
      Transp_check = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( mo.EQ.Transp_beg(i) ) THEN
          IF ( day.GT.10 ) THEN
            Transp_on(i) = 1
          ELSE
            Transp_check(i) = 1
          ENDIF

        ELSEIF ( (Transp_end(i)-Transp_beg(i)).GT.0 ) THEN
          IF ( mo.GT.Transp_beg(i) .AND. mo.LT.Transp_end(i) )
     +         Transp_on(i) = 1

        ELSE
          IF ( (mo.GT.Transp_beg(i) .AND. mo.LT.13) .OR.
     +         (mo.GT.0 .AND. mo.LT.Transp_end(i)) ) Transp_on(i) = 1

        ENDIF
        IF ( Transp_on(i).EQ.1 ) Basin_transp_on = 1
      ENDDO

      pethinit = 0
      END FUNCTION pethinit

!***********************************************************************
!      pethrun - Keeps track of transpiration on or off and computes
!                potential et for each HRU each day
!***********************************************************************
      INTEGER FUNCTION pethrun()
      USE PRMS_POTET_HAMON_HRU
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC SNGL, EXP
! Local Variables
      INTEGER :: mo, i, day, j, jday
      REAL :: dyl, vpsat, vdsat, factor
      DOUBLE PRECISION :: dt
!***********************************************************************
      pethrun = 1

      dt = deltim()
      CALL dattim('now', Datetime)
      mo = Datetime(2)
      day = Datetime(3)
      jday = julian('now', 'calendar')
      Basin_potet = 0.0

!******Set switch for active transpiration period

      IF ( Lday.NE.day ) THEN

        Lday = day

        IF ( getvar('temp', 'tmaxf', Nhru, 'real', Tmaxf)
     +       .NE.0 ) RETURN

        IF ( getvar('temp', 'tmaxc', Nhru, 'real', Tmaxc)
     +       .NE.0 ) RETURN

        Basin_transp_on = 0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

!******If in checking period, then for each day
!******sum max temp until greater than temperature index parameter,
!******at which time, turn transpiration switch on, check switch off

          IF ( Transp_check(i).EQ.1 ) THEN
            IF ( Temp_units.EQ.0 ) THEN
              IF ( Tmaxf(i).GT.32. ) Tmax_sum(i) = Tmax_sum(i)
     +             + Tmaxf(i)
            ELSE
              IF ( Tmaxc(i).GT.0. ) Tmax_sum(i) = Tmax_sum(i) + Tmaxc(i)
            ENDIF

            IF ( Tmax_sum(i).GT.Transp_tmax(i) ) THEN
              Transp_on(i) = 1
              Transp_check(i) = 0
              Tmax_sum(i) = 0.
            ENDIF

!******Otherwise, check for month to turn check switch on or
!******transpiration switch off

          ELSEIF ( day.EQ.1 ) THEN
            IF ( mo.EQ.Transp_beg(i) ) THEN
              Transp_check(i) = 1
              IF ( Temp_units.EQ.0 ) THEN
                IF ( Tmaxf(i).GT.32. ) Tmax_sum(i) = Tmax_sum(i)
     +               + Tmaxf(i)
              ELSE
                IF ( Tmaxc(i).GT.0. ) Tmax_sum(i) = Tmax_sum(i)
     +               + Tmaxc(i)
              ENDIF

!******If transpiration switch on, check for end of period

            ELSEIF ( Transp_on(i).EQ.1 ) THEN
              IF ( mo.EQ.Transp_end(i) ) Transp_on(i) = 0
            ENDIF
          ENDIF
          IF ( Transp_on(i).EQ.1 ) Basin_transp_on = 1
        ENDDO

!******Compute potential et for each HRU using Hamon formulation

        IF ( getvar('temp', 'tavgc', Nhru, 'real', Tavgc)
     +       .NE.0 ) RETURN

        DO j = 1, Active_hrus
          i = Hru_route_order(j)
! Convert daylength from hours to 12 hour multiple (equal day and night period)
          dyl = Soltab_sunhrs(jday, i)/12.0
          vpsat = 6.108*EXP(17.26939*Tavgc(i)/(Tavgc(i)+237.3))
          vdsat = 216.7*vpsat/(Tavgc(i)+273.3)
          Daily_potet(i) = Hamon_coef(mo)*dyl*dyl*vdsat
          IF ( Daily_potet(i).LT.0.0 ) Daily_potet(i) = 0.0
!dbg      IF ( Prt_debug.EQ.6 ) WRITE (87, 9001) mo, day, i, vpsat,
!dbg +         vdsat, dyl, Daily_potet(i)
        ENDDO

      ENDIF

      IF ( dt.LT.23.999D0 ) THEN
        factor = SNGL(dt/24.D0)
      ELSE
        factor = 1.
      ENDIF
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        Potet(i) = factor*Daily_potet(i)
        Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
      ENDDO

      Basin_potet = Basin_potet*Basin_area_inv

!dbg  IF ( Prt_debug.EQ.6 ) THEN
!dbg    PRINT *, 'basin_potet = ', Basin_potet
!dbg    PRINT 9002, Potet
!dbg  ENDIF

      pethrun = 0

!dbg 9001 FORMAT (3I4, 5F8.4)
!dbg 9002 FORMAT (' potet: ', 8F10.4)

      END FUNCTION pethrun

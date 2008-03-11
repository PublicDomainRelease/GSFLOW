!***********************************************************************
! Determine whether transpiration is occurring and compute the potential
! evapotranspiration for each HRU
!***********************************************************************
      MODULE PRMS_POTET_JH
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
      REAL, ALLOCATABLE :: Tmaxf(:), Tmaxc(:), Tavgc(:), Tavgf(:)
!   Declared Variables from other modules - solrad
      REAL, ALLOCATABLE :: Swrad(:)
!   Declared Variables from other modules - basin
!dbg  INTEGER :: Prt_debug
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Temp_units
      INTEGER, ALLOCATABLE :: Transp_beg(:), Transp_end(:)
      REAL, ALLOCATABLE :: Transp_tmax(:), Jh_coef_hru(:)
      REAL, ALLOCATABLE :: Hru_area(:), Jh_coef(:)
      END MODULE PRMS_POTET_JH

!***********************************************************************
!     Main potet_jh routine
!***********************************************************************
      INTEGER FUNCTION potet_jh_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: petdecl_jh, petinit_jh, petrun_jh
!***********************************************************************
      potet_jh_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        potet_jh_prms = petrun_jh()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        potet_jh_prms = petdecl_jh()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        potet_jh_prms = petinit_jh()
      ENDIF

      END FUNCTION potet_jh_prms

!***********************************************************************
!     petdecl - set up parameters for potential et computations
!   Declared Parameters
!     transp_beg, transp_end, transp_tmax, jh_coef, jh_coef_hru,
!     hru_area, temp_units
!***********************************************************************
      INTEGER FUNCTION petdecl_jh()
      USE PRMS_POTET_JH
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      petdecl_jh = 1

      IF ( declmodule(
     +'$Id: potet_jh_prms.f 3584 2007-10-04 17:51:42Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      ALLOCATE (Daily_potet(Nhru), Tmax_sum(Nhru), Transp_check(Nhru))
      IF ( declpri('potetjh_lday', 1, 'integer', Lday).NE.0 ) RETURN
      IF ( declpri('potetjh_daily_potet', Nhru, 'real',
     +     Daily_potet).NE.0 ) RETURN
      IF ( declpri('potetjh_tmax_sum', Nhru, 'real', Tmax_sum)
     +     .NE.0 ) RETURN
      IF ( declpri('potetjh_transp_check', Nhru, 'integer',
     +     Transp_check).NE.0 ) RETURN

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

      ALLOCATE (Jh_coef(MAXMO))
      IF ( declparam('potet', 'jh_coef', 'nmonths', 'real',
     +     '.014', '.005', '.060',
     +     'Monthly air temp coefficient - Jensen-Haise',
     +     'Monthly air temperature coefficient used in Jensen'//
     +     '-Haise potential evapotranspiration computations, see'//
     +     ' PRMS manual for calculation method',
     +     'per degrees').NE.0 ) RETURN

      ALLOCATE (Jh_coef_hru(Nhru))
      IF ( declparam('potet', 'jh_coef_hru', 'nhru', 'real',
     +     '13.0', '5.0', '20.0',
     +     'HRU air temp coefficient - Jensen-Haise',
     +    'Air temperature coefficient used in Jensen-Haise potential'//
     +     ' evapotranspiration computations for each HRU.  See PRMS'//
     +     ' manual for calculation method',
     +     'per degrees').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('potet', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      IF ( declparam('potet', 'temp_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for observed temperature',
     +     'Units for observed temperature (0=Fahrenheit; 1=Celsius)',
     +     'none').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Tmaxf(Nhru), Tmaxc(Nhru), Tavgc(Nhru), Tavgf(Nhru))
      ALLOCATE (Swrad(Nhru), Hru_route_order(Nhru))

      petdecl_jh = 0
      END FUNCTION petdecl_jh

!***********************************************************************
!     petinit - Initialize potet module - get parameter values,
!                set initial transp_on switch
!***********************************************************************
      INTEGER FUNCTION petinit_jh()
      USE PRMS_POTET_JH
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: mo, day, i, j
!***********************************************************************
      petinit_jh = 1

      IF ( getparam('potet', 'transp_beg', Nhru, 'integer', Transp_beg)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'transp_end', Nhru, 'integer', Transp_end)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'transp_tmax', Nhru, 'real', Transp_tmax)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'jh_coef', MAXMO, 'real', Jh_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'jh_coef_hru', Nhru, 'real', Jh_coef_hru)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

!dbg  IF ( get var('basin', 'prt_debug', 1, 'integer', Prt_debug)
!dbg +     .NE.0 ) RETURN

      IF ( getparam('potet', 'temp_units', 1, 'integer', Temp_units)
     +     .NE.0 ) RETURN

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

      petinit_jh = 0
      END FUNCTION petinit_jh

!***********************************************************************
!      petrun - Keeps track of transpiration on or off and computes
!               potential et for each HRU each day
!***********************************************************************
      INTEGER FUNCTION petrun_jh()
      USE PRMS_POTET_JH
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC SNGL
! Local Variables
      INTEGER :: mo, i, day, j
      REAL :: elh, rin, factor
      DOUBLE PRECISION :: dt
!***********************************************************************
      petrun_jh = 1

      dt = deltim()
      CALL dattim('now', Datetime)
      mo = Datetime(2)
      day = Datetime(3)
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

!******Compute potential et for each HRU using Jensen-Haise formulation

        IF ( getvar('temp', 'tavgf', Nhru, 'real', Tavgf)
     +       .NE.0 ) RETURN

        IF ( getvar('temp', 'tavgc', Nhru, 'real', Tavgc)
     +       .NE.0 ) RETURN

        IF ( getvar('solrad', 'swrad', Nhru, 'real', Swrad)
     +       .NE.0 ) RETURN

! 597.3 cal/gm at 0 C is the energy required to change the state of 
! water to vapor
! elh is the latent heat of vaporization (not including the *2.54
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          elh = (597.3-(.5653*Tavgc(i)))*2.54
          rin = Swrad(i)/elh
          Daily_potet(i) = Jh_coef(mo)*(Tavgf(i)-Jh_coef_hru(i))*rin
          IF ( Daily_potet(i).LT.0. ) Daily_potet(i) = 0.
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
!dbg    PRINT 9001, Potet
!dbg  ENDIF

      petrun_jh = 0

!dbg 9001 FORMAT (' potet: ', 8F10.4)

      END FUNCTION petrun_jh

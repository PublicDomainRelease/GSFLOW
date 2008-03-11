!***********************************************************************
! Distribute temperatures to HRU's using 1 station and monthly lapse
! rate parameters
! Variables needed from DATA FILE: tmax, tmin
! Variables needed from DATA FILE: tstemp (commented out)
!***********************************************************************
      MODULE PRMS_TEMP
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Ntemp, Nowtime(6)
      INTEGER, ALLOCATABLE :: Itempflg(:)
      REAL, ALLOCATABLE :: Tcrn(:), Tcrx(:), Elfac(:), Tcr(:)
!   Declared Variables
      REAL :: Basin_temp, Basin_tmax, Basin_tmin
      REAL :: Solrad_tmax, Solrad_tmin
      REAL, ALLOCATABLE :: Tmaxf(:), Tminf(:), Tavgf(:)
      REAL, ALLOCATABLE :: Tmaxc(:), Tminc(:), Tavgc(:)
      REAL, ALLOCATABLE :: Tempf(:), Tempc(:)
!   Declared Variables from other modules - obs
      REAL, ALLOCATABLE :: Obs_tmax(:), Obs_tmin(:)
      REAL, ALLOCATABLE :: Obs_temp(:)
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Temp_units, Basin_tsta
!     INTEGER :: Elev_units
      INTEGER, ALLOCATABLE :: Hru_tsta(:)
      REAL, ALLOCATABLE :: Tsta_elev(:), Tmax_adj(:), Tmin_adj(:)
      REAL, ALLOCATABLE :: Hru_elev(:), Hru_area(:)
      REAL, ALLOCATABLE :: Tmax_lapse(:), Tmin_lapse(:)
      END MODULE PRMS_TEMP

!***********************************************************************
!     Main temp_1sta routine
!***********************************************************************
      INTEGER FUNCTION temp_1sta_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: t1decl, t1init, t1run
!***********************************************************************
      temp_1sta_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        temp_1sta_prms = t1run()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        temp_1sta_prms = t1decl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        temp_1sta_prms = t1init()
      ENDIF

      END FUNCTION temp_1sta_prms

!***********************************************************************
!     t1decl - set up parameters for temperature computations
!   Declared Parameters
!     tmax_lapse, tmin_lapse, tsta_elev, tmax_adj, tmin_adj
!     hru_tsta, hru_elev, hru_area, temp_units, basin_tsta
!***********************************************************************
      INTEGER FUNCTION t1decl()
      USE PRMS_TEMP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      t1decl = 1

      IF ( declmodule(
     +'$Id: temp_1sta_prms.f 3618 2007-11-15 20:09:22Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN
      ALLOCATE (Obs_tmin(Ntemp), Obs_tmax(Ntemp))
      ALLOCATE (Obs_temp(Ntemp))
      ALLOCATE (Itempflg(Ntemp))

      ALLOCATE (Tcrn(Nhru), Tcrx(Nhru), Tcr(Nhru), Elfac(Nhru))
      IF ( declpri('temp_tcrn', Nhru, 'real', Tcrn).NE.0 ) RETURN
      IF ( declpri('temp_tcrx', Nhru, 'real', Tcrx).NE.0 ) RETURN
      IF ( declpri('temp_tcr', Nhru, 'real', Tcr).NE.0 ) RETURN

      ALLOCATE (Tmaxf(Nhru))
      IF ( declvar('temp', 'tmaxf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily maximum temperature',
     +     'degrees F',
     +     Tmaxf).NE.0 ) RETURN

      ALLOCATE (Tminf(Nhru))
      IF ( declvar('temp', 'tminf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily minimum temperature',
     +     'degrees F',
     +     Tminf).NE.0 ) RETURN

      ALLOCATE (Tavgf(Nhru))
      IF ( declvar('temp', 'tavgf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily average temperature',
     +     'degrees F',
     +     Tavgf).NE.0 ) RETURN

      ALLOCATE (Tempf(Nhru))
      IF ( declvar('temp', 'tempf', 'nhru', Nhru, 'real',
     +     'HRU adjusted temperature for timestep < 24',
     +     'degrees F',
     +     Tempf).NE.0 ) RETURN

      ALLOCATE (Tmaxc(Nhru))
      IF ( declvar('temp', 'tmaxc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily maximum temperature',
     +     'degrees Celsius',
     +     Tmaxc).NE.0 ) RETURN

      ALLOCATE (Tminc(Nhru))
      IF ( declvar('temp', 'tminc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily minimum temperature',
     +     'degrees Celsius',
     +     Tminc).NE.0 ) RETURN

      ALLOCATE (Tavgc(Nhru))
      IF ( declvar('temp', 'tavgc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily average temperature',
     +     'degrees Celsius',
     +     Tavgc).NE.0 ) RETURN

      ALLOCATE (Tempc(Nhru))
      IF ( declvar('temp', 'tempc', 'nhru', Nhru, 'real',
     +     'HRU adjusted temperature for timestep < 24',
     +     'degrees Celsius',
     +     Tempc).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_tmax', 'one', 1, 'real',
     +     'Basin area-weighted daily maximum temperature',
     +     'degrees',
     +     Basin_tmax).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_tmin', 'one', 1, 'real',
     +     'Basin area-weighted daily minimum temperature',
     +     'degrees',
     +     Basin_tmin).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_temp', 'one', 1, 'real',
     +     'Basin area-weighted temperature for timestep < 24',
     +     'degrees',
     +     Basin_temp).NE.0 ) RETURN

      IF ( declvar('temp', 'solrad_tmax', 'one', 1, 'real',
     +     'Basin daily maximum temperature for use with solrad'//
     +     ' radiation',
     +     'degrees',
     +     Solrad_tmax).NE.0 ) RETURN

      IF ( declvar('temp', 'solrad_tmin', 'one', 1, 'real',
     +     'Basin daily minimum temperature for use with solrad'//
     +     ' radiation',
     +     'degrees',
     +     Solrad_tmin).NE.0 ) RETURN

      ALLOCATE (Tmax_lapse(MAXMO))
      IF ( declparam('temp', 'tmax_lapse', 'nmonths', 'real',
     +     '3.0', '-10.0', '10.0',
     +     'Monthly maximum temperature lapse rate',
     +     'Array of twelve values representing the change in'//
     +     ' maximum temperature per 1000 elev_units of'//
     +     ' elevation change for each month, January to December',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tmin_lapse(MAXMO))
      IF ( declparam('temp', 'tmin_lapse', 'nmonths', 'real',
     +     '3.0', '-10.0', '10.0',
     +     'Monthly minimum temperature lapse rate',
     +     'Array of twelve values representing the change in'//
     +     ' minimum temperture per 1000 elev_units of'//
     +     ' elevation change for each month, January to December',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tsta_elev(Ntemp))
      IF ( declparam('temp', 'tsta_elev', 'ntemp', 'real',
     +     '0', '-300.', '30000.',
     +     'Temperature station elevation',
     +     'Elevation of each temperature measurement station',
     +     'elev_units').NE.0 ) RETURN

      ALLOCATE (Tmax_adj(Nhru))
      IF ( declparam('temp', 'tmax_adj', 'nhru', 'real',
     +     '0.0', '-10.', '10.0',
     +     'HRU maximum temperature adjustment',
     +     'Adjustment to maximum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tmin_adj(Nhru))
      IF ( declparam('temp', 'tmin_adj', 'nhru', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'HRU minimum temperature adjustment',
     +     'Adjustment to minimum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Hru_tsta(Nhru))
      IF ( declparam('temp', 'hru_tsta', 'nhru', 'integer',
     +     '1', 'bounded', 'ntemp',
     +     'Index of base temperature station for HRU',
     +     'Index of the base temperature station used for lapse'//
     +     ' rate calculations',
     +     'none').NE.0 ) RETURN

      IF ( declparam('temp', 'basin_tsta', 'one', 'integer',
     +     '1', 'bounded', 'ntemp',
     +     'Index of main temperature station',
     +     'Index of temperature station used to compute basin'//
     +     ' temperature values',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('temp', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_elev(Nhru))
      IF ( declparam('temp', 'hru_elev', 'nhru', 'real',
     +     '0.', '-300.', '30000',
     +     'HRU mean elevation', 'Mean elevation for each HRU',
     +     'elev_units').NE.0 ) RETURN

      IF ( declparam('temp', 'temp_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for observed temperature',
     +     'Units for observed temperature (0=Fahrenheit; 1=Celsius)',
     +     'none').NE.0 ) RETURN

!     IF ( decl param('temp', 'elev_units', 'one', 'integer',
!    +     '0', '0', '1',
!    +     'Elevation units flag',
!    +     'Flag to indicate the units of the elevation values'//
!    +     ' (0=feet; 1=meters)',
!    +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_route_order(Nhru))

      t1decl = 0
      END FUNCTION t1decl

!***********************************************************************
!     t1init - Initialize temp_1sta module - get parameter values,
!              compute elfac
!***********************************************************************
      INTEGER FUNCTION t1init()
      USE PRMS_TEMP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: j, k, jj
      REAL :: tmaxlaps, tminlaps
!***********************************************************************
      t1init = 1

      IF ( getparam('temp', 'tmin_lapse', MAXMO, 'real', Tmin_lapse)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tmax_lapse', MAXMO, 'real', Tmax_lapse)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tsta_elev', Ntemp, 'real', Tsta_elev)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_elev', Nhru, 'integer', Hru_elev)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tmax_adj', Nhru, 'real', Tmax_adj)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tmin_adj', Nhru, 'real', Tmin_adj)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_tsta', Nhru, 'integer', Hru_tsta)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'temp_units', 1, 'integer', Temp_units)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'basin_tsta', 1, 'integer', Basin_tsta)
     +     .NE.0 ) RETURN
      IF ( Basin_tsta.LT.1 ) Basin_tsta = 1

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( getstep().EQ.0 ) THEN
        Tmaxf = 0.0
        Tminf = 0.0
        Tavgf = 0.0
        Tmaxc = 0.0
        Tminc = 0.0
        Tavgc = 0.0
        Tempf = 0.0
        Tempc = 0.0
        Solrad_tmax = 0.0
        Solrad_tmin = 0.0
        Basin_temp = 0.0
        Basin_tmax = 0.0
        Basin_tmin = 0.0
      ENDIF

      CALL dattim('start', Nowtime)
      tmaxlaps = Tmax_lapse(Nowtime(2))
      tminlaps = Tmin_lapse(Nowtime(2))
      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        IF ( Hru_tsta(j).LT.1 ) Hru_tsta(j) = 1
        k = Hru_tsta(j)
        Elfac(j) = (Hru_elev(j)-Tsta_elev(k))/1000.
        Tcrx(j) = tmaxlaps*Elfac(j) - Tmax_adj(j)
        Tcrn(j) = tminlaps*Elfac(j) - Tmin_adj(j)
        Tcr(j) = (Tcrx(j)+Tcrn(j))*0.5
      ENDDO

      t1init = 0
      END FUNCTION t1init

!***********************************************************************
!     t1run - Computes maximum, minumum and average temperature
!             for each HRU based on monthly lapse rate
!***********************************************************************
      INTEGER FUNCTION t1run()
      USE PRMS_TEMP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      REAL, EXTERNAL :: c_to_f, f_to_c
! Local Variables
      INTEGER :: j, k, storm, day, mo, jj
      REAL :: tmx, tmn, tmaxlaps, tminlaps, ts_temp
!***********************************************************************
      t1run = 1

      CALL dattim('now', Nowtime)
      day = Nowtime(3)
      mo = Nowtime(2)

      IF ( getvar('obs', 'tmax', Ntemp, 'real', Obs_tmax).NE.0 ) RETURN
      IF ( getvar('obs', 'tmin', Ntemp, 'real', Obs_tmin).NE.0 ) RETURN

      IF ( deltim().LT.23.999D0 ) THEN
        storm = 1
        IF ( getvar('obs', 'tstemp', Ntemp, 'real', Obs_temp)
     +       .NE.0 ) RETURN
      ELSE
        storm = 0
      ENDIF

      Basin_tmax = 0.
      Basin_tmin = 0.
      Basin_temp = 0.

      IF ( day.EQ.1 ) THEN
        tmaxlaps = Tmax_lapse(mo)
        tminlaps = Tmin_lapse(mo)
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          Tcrx(j) = tmaxlaps*Elfac(j) - Tmax_adj(j)
          Tcrn(j) = tminlaps*Elfac(j) - Tmin_adj(j)
          IF ( storm.EQ.1 ) Tcr(j) = (Tcrx(j)+Tcrn(j))*0.5
        ENDDO
      ENDIF

      Itempflg = 0
      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        k = Hru_tsta(j)
        IF ( Itempflg(k).EQ.0 ) THEN
          IF ( Obs_tmax(k).LT.-50.0 .OR. Obs_tmax(k).GT.150.0 )
     +         PRINT 9001, 'tmax', Obs_tmax(k), k, Nowtime
          IF ( Obs_tmin(k).LT.-50.0 .OR. Obs_tmin(k).GT.150.0 )
     +         PRINT 9001, 'tmin', Obs_tmin(k), k, Nowtime
          Itempflg(k) = 1
        ENDIF
        tmx = Obs_tmax(k) - Tcrx(j)
        tmn = Obs_tmin(k) - Tcrn(j)
        IF ( storm.EQ.1 ) THEN
          ts_temp = Obs_temp(k) - Tcr(j)
          Basin_temp = Basin_temp + ts_temp*Hru_area(j)
        ENDIF
        IF ( Temp_units.EQ.0 ) THEN
!         degrees F
          Tmaxf(j) = tmx
          Tminf(j) = tmn
          Tavgf(j) = (tmx+tmn)*0.5
          Tmaxc(j) = f_to_c(tmx)
          Tminc(j) = f_to_c(tmn)
          Tavgc(j) = f_to_c(Tavgf(j))
          IF ( storm.EQ.1 ) THEN
            Tempf(j) = ts_temp
            Tempc(j) = f_to_c(ts_temp)
          ENDIF
        ELSE
!         degrees C
          Tmaxc(j) = tmx
          Tminc(j) = tmn
          Tavgc(j) = (tmx+tmn)*0.5
          Tmaxf(j) = c_to_f(tmx)
          Tminf(j) = c_to_f(tmn)
          Tavgf(j) = c_to_f(Tavgc(j))
          IF ( storm.EQ.1 ) THEN
            Tempc(j) = ts_temp
            Tempf(j) = c_to_f(ts_temp)
          ENDIF
        ENDIF

        Basin_tmax = Basin_tmax + tmx*Hru_area(j)
        Basin_tmin = Basin_tmin + tmn*Hru_area(j)
      ENDDO

      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv
      IF ( storm.EQ.1 ) Basin_temp = Basin_temp*Basin_area_inv

      Solrad_tmax = Obs_tmax(Basin_tsta)
      Solrad_tmin = Obs_tmin(Basin_tsta)

      t1run = 0

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3,
     +        '; temperature staion:', I3, ' Time:', I5, 2('/', I2.2),
     +        I3, 2(':', I2.2))
      END FUNCTION t1run

!***********************************************************************
! Convert Fahrenheit to Celsius
!***********************************************************************
      REAL FUNCTION f_to_c(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: FIVE_NINTHS = 5.0/9.0
!***********************************************************************
      f_to_c = (Temp-32.0)*FIVE_NINTHS
      END FUNCTION f_to_c

!***********************************************************************
! Convert Celsius to Fahrenheit
!***********************************************************************
      REAL FUNCTION c_to_f(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: NINE_FIFTHS = 9.0/5.0
!***********************************************************************
      c_to_f = Temp*NINE_FIFTHS + 32.0
      END FUNCTION c_to_f

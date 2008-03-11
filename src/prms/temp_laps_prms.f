!***********************************************************************
! Distribute temperatures to HRU's using 2 station daily lapse rate
! computations
! Variables needed from DATA FILE: tmax, tmin
! Variables needed from DATA FILE: tstemp (commented out)
!***********************************************************************
      MODULE PRMS_TEMP_LAPS
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Ntemp, Nowtime(6)
      INTEGER, ALLOCATABLE :: Itempflg(:)
      REAL, ALLOCATABLE :: Elfac(:)
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
      INTEGER, ALLOCATABLE :: Hru_tsta(:), Hru_tlaps(:)
      REAL, ALLOCATABLE :: Tsta_elev(:), Tmax_adj(:), Tmin_adj(:)
      REAL, ALLOCATABLE :: Hru_elev(:), Hru_area(:)
      END MODULE PRMS_TEMP_LAPS

!***********************************************************************
!     Main temp_laps routine
!***********************************************************************
      INTEGER FUNCTION temp_laps_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: tlapsdecl, tlapsinit, tlapsrun
!***********************************************************************
      temp_laps_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        temp_laps_prms = tlapsrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        temp_laps_prms = tlapsdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        temp_laps_prms = tlapsinit()
      ENDIF

      END FUNCTION temp_laps_prms

!***********************************************************************
!     tlapsdecl - set up parameters for temperature computations
!   Declared Parameters
!     tsta_elev, tmax_adj, tmin_adj
!     hru_tsta, hru_tlaps, hru_elev, hru_area, temp_units, basin_tsta
!***********************************************************************
      INTEGER FUNCTION tlapsdecl()
      USE PRMS_TEMP_LAPS
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      tlapsdecl = 1

      IF ( declmodule(
     +'$Id: temp_laps_prms.f 3618 2007-11-15 20:09:22Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN
      ALLOCATE (Obs_tmin(Ntemp), Obs_tmax(Ntemp))
      ALLOCATE (Obs_temp(Ntemp))
      ALLOCATE (Itempflg(Ntemp))
      ALLOCATE (Elfac(Nhru))

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

      ALLOCATE (Hru_tlaps(Nhru))
      IF ( declparam('temp', 'hru_tlaps', 'nhru', 'integer',
     +     '1', 'bounded', 'ntemp',
     +     'Index of lapse temperature station for HRU',
     +     'Index of the lapse temperature station used for lapse'//
     +     ' rate calculateions',
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

      tlapsdecl = 0
      END FUNCTION tlapsdecl

!***********************************************************************
!     tlapsinit - Initialize temp_laps module - get parameter values,
!              compute elfac
!***********************************************************************
      INTEGER FUNCTION tlapsinit()
      USE PRMS_TEMP_LAPS
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: j, k, l, jj
!***********************************************************************
      tlapsinit = 1

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

      IF ( getparam('temp', 'hru_tlaps', Nhru, 'integer', Hru_tlaps)
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

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        IF ( Hru_tsta(j).LT.1 ) Hru_tsta(j) = 1
        IF ( Hru_tlaps(j).LT.1 ) Hru_tlaps(j) = 1
        k = Hru_tsta(j)
        l = Hru_tlaps(j)
        Elfac(j) = (Hru_elev(j)-Tsta_elev(k))/
     +             (Tsta_elev(l)-Tsta_elev(k))
      ENDDO

      tlapsinit = 0
      END FUNCTION tlapsinit

!***********************************************************************
!     tlapsrun - Computes maximum, minumum and average temperature
!                for each HRU based on 2-station lapse rate
!***********************************************************************
      INTEGER FUNCTION tlapsrun()
      USE PRMS_TEMP_LAPS
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      REAL, EXTERNAL :: c_to_f_tl, f_to_c_tl
! Local Variables
      INTEGER :: j, k, storm, l, jj
      REAL :: tmx, tmn, tcrx, tcrn, tmxsta, tmnsta
      REAL :: tmxtsta, tmntsta, tmxlaps, tmnlaps, ts_temp, tcr
!***********************************************************************
      tlapsrun = 1

      CALL dattim('now', Nowtime)

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

      Itempflg = 0
      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        k = Hru_tsta(j)
        l = Hru_tlaps(j)
        tmxlaps = Obs_tmax(l)
        tmxtsta = Obs_tmax(k)
        tmnlaps = Obs_tmin(l)
        tmntsta = Obs_tmin(k)

        IF ( Itempflg(k).EQ.0 ) THEN
          IF ( tmxtsta.LT.-50.0 .OR. tmxtsta.GT.150.0 )
     +         PRINT 9001, 'tmax', tmxtsta, k, Nowtime
          IF ( tmntsta.LT.-50.0 .OR. tmntsta.GT.150.0 )
     +         PRINT 9001, 'tmax', tmntsta, k, Nowtime
          Itempflg(k) = 1
        ENDIF
        IF ( Itempflg(l).EQ.0 ) THEN
          IF ( tmxlaps.LT.-50.0 .OR. tmxlaps.GT.150.0 )
     +         PRINT 9001, 'tmax', tmxlaps, k, Nowtime
          IF ( tmnlaps.LT.-50.0 .OR. tmnlaps.GT.150.0 )
     +         PRINT 9001, 'tmax', tmnlaps, k, Nowtime
          Itempflg(l) = 1
        ENDIF
                
        tmxsta = tmxlaps - tmxtsta
        tmnsta = tmnlaps - tmntsta

        tcrx = tmxsta*Elfac(j) + Tmax_adj(j)
        tcrn = tmnsta*Elfac(j) + Tmin_adj(j)
        tmx = tmxtsta + tcrx
        tmn = tmntsta + tcrn
        IF ( storm.EQ.1 ) THEN
          tcr = (tcrx+tcrn)*0.5
          ts_temp = Obs_temp(k) + tcr
        ENDIF
        IF ( Temp_units.EQ.0 ) THEN
!         degrees F
          Tmaxf(j) = tmx
          Tminf(j) = tmn
          Tavgf(j) = (tmx+tmn)*0.5
          Tmaxc(j) = f_to_c_tl(tmx)
          Tminc(j) = f_to_c_tl(tmn)
          Tavgc(j) = f_to_c_tl(Tavgf(j))
          IF ( storm.EQ.1 ) THEN
            Tempf(j) = ts_temp
            Tempc(j) = f_to_c_tl(ts_temp)
          ENDIF
        ELSE
!         degrees C
          Tmaxc(j) = tmx
          Tminc(j) = tmn
          Tavgc(j) = (tmx+tmn)*0.5
          Tmaxf(j) = c_to_f_tl(tmx)
          Tminf(j) = c_to_f_tl(tmn)
          Tavgf(j) = c_to_f_tl(Tavgc(j))
          IF ( storm.EQ.1 ) THEN
            Tempc(j) = ts_temp
            Tempf(j) = c_to_f_tl(ts_temp)
          ENDIF
        ENDIF

        Basin_tmax = Basin_tmax + tmx*Hru_area(j)
        Basin_tmin = Basin_tmin + tmn*Hru_area(j)
        IF ( storm.EQ.1 ) Basin_temp = Basin_temp + ts_temp*Hru_area(j)
      ENDDO

      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv
      IF ( storm.EQ.1 ) Basin_temp = Basin_temp*Basin_area_inv

      Solrad_tmax = Obs_tmax(Basin_tsta)
      Solrad_tmin = Obs_tmin(Basin_tsta)

      tlapsrun = 0

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3,
     +        '; temperature staion:', I3, ' Time:', I5, 2('/', I2.2),
     +        I3, 2(':', I2.2), I5)
      END FUNCTION tlapsrun

!***********************************************************************
! Convert Fahrenheit to Celsius
!***********************************************************************
      REAL FUNCTION f_to_c_tl(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: FIVE_NINTHS = 5.0/9.0
!***********************************************************************
      f_to_c_tl = (Temp-32.0)*FIVE_NINTHS
      END FUNCTION f_to_c_tl

!***********************************************************************
! Convert Celsius to Fahrenheit
!***********************************************************************
      REAL FUNCTION c_to_f_tl(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: NINE_FIFTHS = 9.0/5.0
!***********************************************************************
      c_to_f_tl = Temp*NINE_FIFTHS + 32.0
      END FUNCTION c_to_f_tl

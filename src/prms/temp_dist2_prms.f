!***********************************************************************
! temp_dist2_prms.f
! Distribute temperatures to HRU's using all station's daily lapse rate
! computations
!
!     Revised 5/8/98 by Mark Mastin, J Vaccaro
!         --Declared variables basin_lapse_max and basin_lapse_min
!           They are computed in function t2dist2run
!       calculations now use all the stations and distance weight for
!       interpolating values, evens out distribution of temp and can
!       smooth out if bad data, and still accounts for local effects
!
! Variables needed from DATA FILE: tmax, tmin
! Variables needed from DATA FILE: tstemp (commented out)
!***********************************************************************
      MODULE PRMS_TEMP_DIST2
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Ntemp, Nowtime(6)
      REAL, ALLOCATABLE :: Elfac(:, :), Delv(:, :), Dist(:, :)
!   Declared Variables
      REAL :: Basin_temp, Basin_tmax, Basin_tmin
      REAL :: Basin_lapse_max, Basin_lapse_min
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
      REAL, ALLOCATABLE :: Tsta_elev(:), Tmax_adj(:, :), Tmin_adj(:, :)
      REAL, ALLOCATABLE :: Hru_elev(:), Hru_area(:)
      REAL, ALLOCATABLE :: Monmin(:), Monmax(:)
      REAL, ALLOCATABLE :: Lapsemin_min(:), Lapsemin_max(:)
      REAL, ALLOCATABLE :: Lapsemax_min(:), Lapsemax_max(:)
      REAL, ALLOCATABLE :: Tsta_xlong(:), Tsta_ylat(:)
      REAL, ALLOCATABLE :: Hru_xlong(:), Hru_ylat(:)
      END MODULE PRMS_TEMP_DIST2

!***********************************************************************
!     Main temp_dist2 routine
!***********************************************************************
      INTEGER FUNCTION temp_dist2_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: t2dist2decl, t2dist2init, t2dist2run
!***********************************************************************
      temp_dist2_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        temp_dist2_prms = t2dist2run()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        temp_dist2_prms = t2dist2decl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        temp_dist2_prms = t2dist2init()
      ENDIF

      END FUNCTION temp_dist2_prms

!***********************************************************************
!     t2dist2decl - set up parameters for temperature computations
!   Declared Parameters
!     tsta_elev, tmax_adj, tmin_adj
!     hru_elev, hru_area, temp_units, basin_tsta
!     monmin, monmax, lapsemin_min, lapsemin_max, lapsemax_min
!     lapsemax_max, tsta_xlong, tsta_ylat, hru_ylat, hru_xlong
!***********************************************************************
      INTEGER FUNCTION t2dist2decl()
      USE PRMS_TEMP_DIST2
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      t2dist2decl = 1

      IF ( declmodule(
     +'$Id: temp_dist2_prms.f 3618 2007-11-15 20:09:22Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN
      IF ( Ntemp.LT.2 ) THEN
        PRINT *, 'temp_dist2 requires at least 2 temperature stations'
        RETURN
      ENDIF

      ALLOCATE (Obs_tmin(Ntemp), Obs_tmax(Ntemp))
      ALLOCATE (Obs_temp(Ntemp))
      ALLOCATE (Elfac(Nhru,Ntemp), Delv(Ntemp,Ntemp), Dist(Nhru,Ntemp))
      !rsr, Elfac and Dist are computed, not parameters
!     IF ( decl param('temp', 'elfac', 'nhru,ntemp', 'real',
!    +     '0.0', '0.0', '10.0',
!    +     'Elevation difference mru to each ntemp temp. sites',
!    +     'Elevation/1000. for lapse: temperature-gages to'//
!    +     ' each hru-- accounts for differences in elevation'//
!    +     ' although /1000., meters or feet as elev are OK',
!    +     'feet/feet').NE.0 ) RETURN
!     IF ( decl param('temp', 'delv', 'ntemp,ntemp', 'real',
!    +     '0.0', '0.0', '10.0',
!    +     'Elevation difference between each ntemp temp. sites',
!    +     'Elevation/1000 for daily lapse using temp-gage to'//
!    +     ' each other gage-- accounts for differences in elevation'//
!    +     ' although /1000., meters or feet as elev are OK',
!    +     'feet/feet').NE.0 ) RETURN
!     IF ( decl param('temp', 'dist', 'nhru,ntemp', 'real',
!    +     '0.0', '0.0', '50.0',
!    +     '1/Distance**2 from HRU to each of the ntemp sites',
!    +     'Distance**2 to weight tmperature-gages to'//
!    +     ' each HRU to account for closeness of gages'//
!    +     ' miles, meters or feet as x,y are OK',
!    +     'sqrdist').NE.0 ) RETURN

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

! added by Mastin 5/8/98

      IF ( declvar('temp', 'basin_lapse_max', 'one', 1, 'real',
     +     'Basin average maximum temperature lapse rate per 1000 feet',
     +     'degrees',
     +     Basin_lapse_max).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_lapse_min', 'one', 1, 'real',
     +     'Basin average minimum temperature lapse rate per 1000 feet',
     +     'degrees',
     +     Basin_lapse_min).NE.0 ) RETURN

! added THE FOLLOWING NEW PARAMETERS by J Vaccaro 7.98,
!       various parmaeters to interpolate
!       and constrain lapse rates for temperature

      ALLOCATE (Monmin(MAXMO))
      IF ( declparam('temp', 'monmin', 'nmonths', 'real',
     +     '-20.0', '-35.0', '45.0',
     +     'Daily minimum temperature',
     +     'Mimimum temperature in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily minimum data for bad'//
     +     ' values based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Monmax(MAXMO))
      IF ( declparam('temp', 'monmax', 'nmonths', 'real',
     +     '100.0', '45.0', '115.0',
     +     'Daily maximum temperature',
     +     'Maximum temperature in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily maximum data for bad'//
     +     ' values based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemin_min(MAXMO))
      IF ( declparam('temp', 'lapsemin_min', 'nmonths', 'real',
     +     '-4.0', '-7.0', '-3.0',
     +     'Monthly minimum lapse rate for minimum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily minimum lapse rate'//
     +     ' based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemin_max(MAXMO))
      IF ( declparam('temp', 'lapsemin_max', 'nmonths', 'real',
     +     '3.0', '-2.0', '4.0',
     +     'Monthly maximum lapse rate for minimum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain highest daily minimum lapse rate'//
     +     ' based on historical daily data for all sites',
     +   'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemax_min(MAXMO))
      IF ( declparam('temp', 'lapsemax_min', 'nmonths', 'real',
     +     '-6.5', '-7.0', '-3.0',
     +     'Monthly minimum lapse rate for maximum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily maximum lapse rate'//
     +     ' based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemax_max(MAXMO))
      IF ( declparam('temp', 'lapsemax_max', 'nmonths', 'real',
     +     '2.0', '-3.0', '3.0',
     +     'Monthly maximum lapse rate for maximum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily maximum lapse rate'//
     +     ' based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tsta_xlong(Ntemp))
      IF ( declparam('temp', 'tsta_xlong', 'ntemp', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'Temperature station longitude, state plane',
     +     'Longitude of each temperature measurement station',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Tsta_ylat(Ntemp))
      IF ( declparam('temp', 'tsta_ylat', 'ntemp', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'Temperature station latitude, state plane',
     +     'Latitude of each temperature measurement station',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Hru_ylat(Nhru))
      IF ( declparam('temp', 'hru_ylat', 'nhru', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'HRU latitude of centroid, state plane',
     +     'Latitude of each HRU for the centroid',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Hru_xlong(Nhru))
      IF ( declparam('temp', 'hru_xlong', 'nhru', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'HRU longitude of centroid, state plane',
     +     'Longitude of each HRU for the centroid',
     +     'feet').NE.0 ) RETURN

! END NEW PARAMETERS

      ALLOCATE (Tsta_elev(Ntemp))
      IF ( declparam('temp', 'tsta_elev', 'ntemp', 'real',
     +     '0', '-300.', '30000.',
     +     'Temperature station elevation',
     +     'Elevation of each temperature measurement station',
     +     'elev_units').NE.0 ) RETURN

      ALLOCATE (Tmax_adj(Nhru,MAXMO))
      IF ( declparam('temp', 'tmax_adj', 'nhru,nmonths', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'HRU monthly maximum temperature adjustment',
     +     'Adjustment to maximum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tmin_adj(Nhru,MAXMO))
      IF ( declparam('temp', 'tmin_adj', 'nhru,nmonths', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'HRU monthly minimum temperature adjustment',
     +     'Adjustment to minimum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

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

      t2dist2decl = 0
      END FUNCTION t2dist2decl

!***********************************************************************
!     t2dist2init - Initialize temp_dist2 module
!                 - get parameter values, compute elfac, dist
!***********************************************************************
      INTEGER FUNCTION t2dist2init()
      USE PRMS_TEMP_DIST2
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC SQRT
! Local Variables
      INTEGER :: i, j, k, jj
      REAL :: distx, disty, dis
!***********************************************************************
      t2dist2init = 1

      IF ( getparam('temp', 'monmin', MAXMO, 'real', Monmin)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'monmax', MAXMO, 'real', Monmax)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemin_min', MAXMO, 'real', Lapsemin_min)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemin_max', MAXMO, 'real', Lapsemin_max)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemax_min', MAXMO, 'real', Lapsemax_min)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemax_max', MAXMO, 'real', Lapsemax_max)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tsta_elev', Ntemp, 'real', Tsta_elev)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tsta_xlong', Ntemp, 'real', Tsta_xlong)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tsta_ylat', Ntemp, 'real', Tsta_ylat)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_xlong', Nhru, 'real', Hru_xlong)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_ylat', Nhru, 'real', Hru_ylat)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_elev', Nhru, 'integer', Hru_elev)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tmax_adj', Nhru*MAXMO, 'real', Tmax_adj)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tmin_adj', Nhru*MAXMO, 'real', Tmin_adj)
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
        Basin_lapse_max = 0.0
        Basin_lapse_min = 0.0
      ENDIF

! CALCULATE:  DISTANCE FROM EACH MRU TO EACH NTEMP GAGE
!          :  ELEVATION FACTOR FOR EACH MRU TO EACH NTEMP GAGE
      DO jj = 1, Active_hrus
        i = Hru_route_order(jj)
        DO k = 1, Ntemp
          Elfac(i, k) = (Hru_elev(i)-Tsta_elev(k))/1000.
          distx = (Hru_xlong(i)-Tsta_xlong(k))**2
          disty = (Hru_ylat(i)-Tsta_ylat(k))**2
          dis = 1./SQRT(distx+disty)
          Dist(i, k) = dis*dis
        ENDDO
      ENDDO

      DO j = 1, Ntemp - 1
        DO k = j + 1, Ntemp
          Delv(j, k) = (Tsta_elev(j)-Tsta_elev(k))/1000.
        ENDDO
      ENDDO

      t2dist2init = 0
      END FUNCTION t2dist2init

!***********************************************************************
!     t2dist2run - Computes maximum, minumum and average temperature
!                  for each HRU based on average lapse rate for all
!                  stations. Average is constrained by maximum and
!                  minimum lapse rates for minimum and maximum
!                  temperatures (each has an upper and lower limit).
!                  Limits can be calculated using all data for the
!                  available period of record
!***********************************************************************
      INTEGER FUNCTION t2dist2run()
      USE PRMS_TEMP_DIST2
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      REAL, EXTERNAL :: c_to_f_dis, f_to_c_dis
      INTRINSIC FLOAT
! Local Variables
      INTEGER :: j, k, storm, ntotx, ntotn, mon, jj
      REAL :: tmx, tmn, tcrx, tcrn, ts_temp, tcr
      REAL :: sumdist, sumtx, sumtn, diffn, diffx, mx, mn
      REAL :: lapsemaxmax, lapsemaxmin, lapseminmax, lapseminmin
!***********************************************************************
      t2dist2run = 1

      CALL dattim('now', Nowtime)
      mon = Nowtime(2)
      mn = Monmin(mon)
      mx = Monmax(mon)

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

! Calculate basin-average lapse rate using all NTEMP stations

      sumtx = 0.0
      sumtn = 0.0
      ntotx = 0
      ntotn = 0
      lapsemaxmax = Lapsemax_max(mon)
      lapsemaxmin = Lapsemax_min(mon)
      lapseminmax = Lapsemin_max(mon)
      lapseminmin = Lapsemin_min(mon)

      DO j = 1, Ntemp - 1

! check for missing or bad temps based on min and max daily values
! observed for each month. 

! the value of  -9999 = missing in HDB, and rdb

        IF ( Obs_tmax(j).LT.mn .OR. Obs_tmin(j).LT.mn .OR.
     +       Obs_tmax(j).GT.mx .OR. Obs_tmin(j).GT.mx ) CYCLE

        DO k = j + 1, Ntemp

          IF ( Obs_tmax(k).LT.mn .OR. Obs_tmin(k).LT.mn .OR.
     +         Obs_tmax(k).GT.mx .OR. Obs_tmin(k).GT.mx ) CYCLE

          diffx = (Obs_tmax(j)-Obs_tmax(k))/Delv(j, k)
          diffn = (Obs_tmin(j)-Obs_tmin(k))/Delv(j, k)
          IF ( diffx.GT.lapsemaxmax ) diffx = lapsemaxmax
          IF ( diffx.LT.lapsemaxmin ) diffx = lapsemaxmin
          IF ( diffn.GT.lapseminmax ) diffn = lapseminmax
          IF ( diffn.LT.lapseminmin ) diffn = lapseminmin
          sumtx = sumtx + diffx
          ntotx = ntotx + 1
          sumtn = sumtn + diffn
          ntotn = ntotn + 1
        ENDDO
      ENDDO

      IF ( ntotx.GT.0 ) THEN
        Basin_lapse_max = sumtx/FLOAT(ntotx)
      ELSE
        Basin_lapse_max = (lapsemaxmax+lapsemaxmin)*0.5
      ENDIF
      IF ( ntotn.GT.0 ) THEN
        Basin_lapse_min = sumtn/FLOAT(ntotn)
      ELSE
        Basin_lapse_min = (lapseminmax+lapseminmin)*0.5
      ENDIF

! NHRU loop (10) for this day or timestep

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)

        tmx = 0.0
        tmn = 0.0
        ts_temp = 0.0
        sumdist = 0.0

        DO k = 1, Ntemp

! check for missing or bad temps

          IF ( Obs_tmax(k).LT.mn .OR. Obs_tmin(k).LT.mn .OR.
     +         Obs_tmax(k).GT.mx .OR. Obs_tmin(k).GT.mx ) CYCLE

          sumdist = sumdist + Dist(j, k)
          tcrx = Basin_lapse_max*Elfac(j, k) + Tmax_adj(j, mon)
          tcrn = Basin_lapse_min*Elfac(j, k) + Tmin_adj(j, mon)
          tmx = tmx + (Obs_tmax(k)+tcrx)*Dist(j, k)
          tmn = tmn + (Obs_tmin(k)+tcrn)*Dist(j, k)
          IF ( storm.EQ.1 ) THEN
            tcr = (tcrx+tcrn)*0.5
            ts_temp = ts_temp + (Obs_temp(k)+tcr)*Dist(j, k)
          ENDIF
        ENDDO

        IF ( sumdist.GT.0.0 ) THEN
          tmn = tmn/sumdist
          tmx = tmx/sumdist
          IF ( storm.EQ.1 ) THEN
            ts_temp = ts_temp/sumdist
            Basin_temp = Basin_temp + ts_temp*Hru_area(j)
          ENDIF
        ELSE
          tmn = (mn+mx)*0.5
          tmx = tmn
          IF ( storm.EQ.1 ) THEN
            ts_temp = tmn
            Basin_temp = Basin_temp + ts_temp*Hru_area(j)
          ENDIF
        ENDIF

        IF ( tmx.LE.tmn ) tmx = tmn + 0.01

        IF ( Temp_units.EQ.0 ) THEN
!         degrees F
          Tmaxf(j) = tmx
          Tminf(j) = tmn
          Tavgf(j) = (tmx+tmn)*0.5
          Tmaxc(j) = f_to_c_dis(tmx)
          Tminc(j) = f_to_c_dis(tmn)
          Tavgc(j) = f_to_c_dis(Tavgf(j))
          IF ( storm.EQ.1 ) THEN
            Tempf(j) = ts_temp
            Tempc(j) = f_to_c_dis(ts_temp)
          ENDIF
        ELSE
!         degrees C
          Tmaxc(j) = tmx
          Tminc(j) = tmn
          Tavgc(j) = (tmx+tmn)*0.5
          Tmaxf(j) = c_to_f_dis(tmx)
          Tminf(j) = c_to_f_dis(tmn)
          Tavgf(j) = c_to_f_dis(Tavgc(j))
          IF ( storm.EQ.1 ) THEN
            Tempc(j) = ts_temp
            Tempf(j) = c_to_f_dis(ts_temp)
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

      t2dist2run = 0

      END FUNCTION t2dist2run

!***********************************************************************
! Convert Fahrenheit to Celsius
!***********************************************************************
      REAL FUNCTION f_to_c_dis(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: FIVE_NINTHS = 5.0/9.0
!***********************************************************************
      f_to_c_dis = (Temp-32.0)*FIVE_NINTHS
      END FUNCTION f_to_c_dis

!***********************************************************************
! Convert Celsius to Fahrenheit
!***********************************************************************
      REAL FUNCTION c_to_f_dis(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: NINE_FIFTHS = 9.0/5.0
!***********************************************************************
      c_to_f_dis = Temp*NINE_FIFTHS + 32.0
      END FUNCTION c_to_f_dis

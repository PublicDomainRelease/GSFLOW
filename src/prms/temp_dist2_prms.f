!***********************************************************************
! Distributes maximum and minimum temperatures to each HRU using a
! basin wide lapse rate applied to the temperature data, adjusted for
! distance, measured at each station
!
!     Revised 5/8/98 by Mark Mastin, J Vaccaro
!         --Declared variables basin_lapse_max and basin_lapse_min
!           They are computed in function t2dist2run
!       calculations now use all the stations and distance weight for
!       interpolating values, evens out distribution of temp and can
!       smooth out if bad data, and still accounts for local effects
!
! Variables needed from DATA FILE: tmax, tmin
!***********************************************************************
      MODULE PRMS_TEMP_DIST2
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: N_tsta(:), Nuse_tsta(:, :)
      REAL, SAVE, ALLOCATABLE :: Elfac(:, :), Delv(:, :), Dist(:, :)
!   Declared Variables
      REAL, SAVE :: Basin_lapse_max, Basin_lapse_min
!   Declared Parameters
      INTEGER, SAVE :: Max_tsta
      REAL, SAVE :: Dist_max
      REAL, SAVE, ALLOCATABLE :: Tmax_mo_adj(:, :), Tmin_mo_adj(:, :)
      REAL, SAVE, ALLOCATABLE :: Monmin(:), Monmax(:)
      REAL, SAVE, ALLOCATABLE :: Lapsemin_min(:), Lapsemin_max(:)
      REAL, SAVE, ALLOCATABLE :: Lapsemax_min(:), Lapsemax_max(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_xlong(:), Tsta_ylat(:)
      REAL, SAVE, ALLOCATABLE :: Hru_xlong(:), Hru_ylat(:)
      END MODULE PRMS_TEMP_DIST2

!***********************************************************************
!     Main temp_dist2 routine
!***********************************************************************
      INTEGER FUNCTION temp_dist2_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: t2dist2decl, t2dist2init, t2dist2run
!***********************************************************************
      temp_dist2_prms = 0

      IF ( Process_flag==0 ) THEN
        temp_dist2_prms = t2dist2run()
      ELSEIF ( Process_flag==1 ) THEN
        temp_dist2_prms = t2dist2decl()
      ELSEIF ( Process_flag==2 ) THEN
        temp_dist2_prms = t2dist2init()
      ENDIF

      END FUNCTION temp_dist2_prms

!***********************************************************************
!     t2dist2decl - set up parameters for temperature computations
!   Declared Parameters
!     tsta_elev, tmax_mo_adj, tmin_mo_adj
!     hru_elev, hru_area, temp_units, basin_tsta, max_tsta
!     monmin, monmax, lapsemin_min, lapsemin_max, lapsemax_min
!     lapsemax_max, tsta_xlong, tsta_ylat, hru_ylat, hru_xlong, dist_max
!***********************************************************************
      INTEGER FUNCTION t2dist2decl()
      USE PRMS_TEMP_DIST2
      USE PRMS_MODULE, ONLY: Model
      USE PRMS_BASIN, ONLY: Nhru
      USE PRMS_CLIMATEVARS, ONLY: Ntemp
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      t2dist2decl = 1

      IF ( declmodule(
     +'$Id: temp_dist2_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

      IF ( Ntemp.LT.2 .AND. Model/=99 ) THEN
        PRINT *, 'ERROR, temp_dist2 requires at least 2 temperature',
     +           ' stations'
        STOP
      ENDIF

! added by Mastin 5/8/98

      IF ( declvar('temp', 'basin_lapse_max', 'one', 1, 'real',
     +     'Basin average maximum temperature lapse rate per 1000 feet',
     +     'degrees',
     +     Basin_lapse_max).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_lapse_min', 'one', 1, 'real',
     +     'Basin average minimum temperature lapse rate per 1000 feet',
     +     'degrees',
     +     Basin_lapse_min).NE.0 ) RETURN

      IF ( declparam('temp', 'dist_max', 'one', 'real',
     +     '1E+09', '0.0', '1E+09',
     +     'Maximum distance from HRU to include a climate station',
     +     'Maximum distance from HRU to include a climate station',
     +     'feet').NE.0 ) RETURN

      IF ( declparam('temp', 'max_tsta', 'one', 'integer',
     +     '50', '2', '50',
     +     'Maximum number of temp stations to distribute to an HRU',
     +     'Maximum number of temp stations to distribute to an HRU',
     +     'none').NE.0 ) RETURN

! added THE FOLLOWING NEW PARAMETERS by J Vaccaro 7.98,
!       various parmaeters to interpolate
!       and constrain lapse rates for temperature

      ALLOCATE (Monmin(12))
      IF ( declparam('temp', 'monmin', 'nmonths', 'real',
     +     '-60.0', '-60.0', '65.0',
     +     'Daily minimum temperature',
     +     'Minimum temperature in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily minimum data for bad'//
     +     ' values based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Monmax(12))
      IF ( declparam('temp', 'monmax', 'nmonths', 'real',
     +     '100.0', '0.0', '115.0',
     +     'Daily maximum temperature',
     +     'Maximum temperature in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily maximum data for bad'//
     +     ' values based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemin_min(12))
      IF ( declparam('temp', 'lapsemin_min', 'nmonths', 'real',
     +     '-4.0', '-7.0', '-3.0',
     +     'Monthly minimum lapse rate for minimum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily minimum lapse rate'//
     +     ' based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemin_max(12))
      IF ( declparam('temp', 'lapsemin_max', 'nmonths', 'real',
     +     '3.0', '-2.0', '4.0',
     +     'Monthly maximum lapse rate for minimum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain highest daily minimum lapse rate'//
     +     ' based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemax_min(12))
      IF ( declparam('temp', 'lapsemax_min', 'nmonths', 'real',
     +     '-6.5', '-7.0', '-3.0',
     +     'Monthly minimum lapse rate for maximum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily maximum lapse rate'//
     +     ' based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Lapsemax_max(12))
      IF ( declparam('temp', 'lapsemax_max', 'nmonths', 'real',
     +     '2.0', '-3.0', '3.0',
     +     'Monthly maximum lapse rate for maximum temperature',
     +     'Lapse rate, in deg F or C depending on units of'//
     +     ' data, to constrain lowest daily maximum lapse rate'//
     +     ' based on historical daily data for all sites',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tsta_xlong(Ntemp))
      IF ( declparam('temp', 'tsta_xlong', 'ntemp', 'real',
     +     '0.', '-1E+09', '1E+09',
     +     'Temperature station longitude, state plane',
     +     'Longitude of each temperature measurement station',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Tsta_ylat(Ntemp))
      IF ( declparam('temp', 'tsta_ylat', 'ntemp', 'real',
     +     '0.', '-1E+09', '1E+09',
     +     'Temperature station latitude, state plane',
     +     'Latitude of each temperature measurement station',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Hru_ylat(Nhru))
      IF ( declparam('temp', 'hru_ylat', 'nhru', 'real',
     +     '0.', '-1E+09', '1E+09',
     +     'HRU latitude of centroid, state plane',
     +     'Latitude of each HRU for the centroid',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Hru_xlong(Nhru))
      IF ( declparam('temp', 'hru_xlong', 'nhru', 'real',
     +     '0.', '-1E+09', '1E+09',
     +     'HRU longitude of centroid, state plane',
     +     'Longitude of each HRU for the centroid',
     +     'feet').NE.0 ) RETURN

! END NEW PARAMETERS

      ALLOCATE (Tmax_mo_adj(Nhru,12))
      IF ( declparam('temp', 'tmax_mo_adj', 'nhru,nmonths', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'HRU monthly maximum temperature adjustment',
     +     'Adjustment to maximum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tmin_mo_adj(Nhru,12))
      IF ( declparam('temp', 'tmin_mo_adj', 'nhru,nmonths', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'HRU monthly minimum temperature adjustment',
     +     'Adjustment to minimum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

      t2dist2decl = 0
      END FUNCTION t2dist2decl

!***********************************************************************
!     t2dist2init - Initialize temp_dist2 module
!                 - get parameter values, compute elfac, dist
!***********************************************************************
      INTEGER FUNCTION t2dist2init()
      USE PRMS_TEMP_DIST2
      USE PRMS_BASIN, ONLY: Timestep, Nhru, Hru_elev, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Tsta_elev, Ntemp
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      INTRINSIC SQRT
! Local Variables
      INTEGER :: i, j, k, n, kk, kkbig
      REAL :: distx, disty, distance, big_dist, dist2, diff
      REAL, ALLOCATABLE :: nuse_tsta_dist(:, :)
!***********************************************************************
      t2dist2init = 1

      IF ( getparam('temp', 'dist_max', 1, 'real', Dist_max)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'max_tsta', 1, 'real', Max_tsta)
     +     .NE.0 ) RETURN
      IF ( Max_tsta==50 ) Max_tsta = Ntemp

      IF ( getparam('temp', 'monmin', 12, 'real', Monmin)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'monmax', 12, 'real', Monmax)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemin_min', 12, 'real', Lapsemin_min)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemin_max', 12, 'real', Lapsemin_max)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemax_min', 12, 'real', Lapsemax_min)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'lapsemax_max', 12, 'real', Lapsemax_max)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tsta_xlong', Ntemp, 'real', Tsta_xlong)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tsta_ylat', Ntemp, 'real', Tsta_ylat)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_xlong', Nhru, 'real', Hru_xlong)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'hru_ylat', Nhru, 'real', Hru_ylat)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tmax_mo_adj', Nhru*12, 'real',
     +     Tmax_mo_adj).NE.0 ) RETURN

      IF ( getparam('temp', 'tmin_mo_adj', Nhru*12, 'real',
     +     Tmin_mo_adj).NE.0 ) RETURN

      IF ( Timestep==0 ) THEN
        Basin_lapse_max = 0.0
        Basin_lapse_min = 0.0
      ENDIF

! CALCULATE:  DISTANCE FROM EACH MRU TO EACH NTEMP GAGE
!          :  ELEVATION FACTOR FOR EACH MRU TO EACH NTEMP GAGE
      ALLOCATE (Elfac(Nhru,Ntemp), Delv(Ntemp,Ntemp), Dist(Nhru,Ntemp))
      ALLOCATE (N_tsta(Nhru))
      ALLOCATE (Nuse_tsta(Max_tsta,Nhru), nuse_tsta_dist(Max_tsta,Nhru))
      N_tsta = 0
      Nuse_tsta = 0
      nuse_tsta_dist = 0.0
      DO i = 1, Nhru
        DO k = 1, Ntemp
          diff = Hru_elev(i) - Tsta_elev(k)
          IF ( ABS(diff)<NEARZERO ) diff = 1.0
          Elfac(i, k) = diff/1000.0
          distx = (Hru_xlong(i)-Tsta_xlong(k))**2
          disty = (Hru_ylat(i)-Tsta_ylat(k))**2
          distance = SQRT(distx+disty)
          dist2 = 1.0 / (distance/5280.0)
          Dist(i, k) = dist2*dist2
          IF ( distance<Dist_max ) THEN
            n = N_tsta(i)
            IF ( n<Max_tsta ) THEN
              n = n + 1
              Nuse_tsta(n, i) = k
              nuse_tsta_dist(n, i) = distance
              N_tsta(i) = n
            ELSE ! have max_tsta, don't use biggest distance
              big_dist = 0.0
              kkbig = 1
              DO kk = 1, Max_tsta
                IF ( big_dist<nuse_tsta_dist(kk,i) ) THEN
                  big_dist = nuse_tsta_dist(kk, i)
                  kkbig = kk
                ENDIF
              ENDDO
              IF ( distance<big_dist ) THEN ! if equal use first one
                Nuse_tsta(kkbig, i) = k
                nuse_tsta_dist(kkbig, i) = distance
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      DO j = 1, Ntemp - 1
        DO k = j + 1, Ntemp
          Delv(j, k) = (Tsta_elev(j)-Tsta_elev(k))/1000.
          IF ( ABS(Delv(j,k))<NEARZERO ) Delv(j, k) = 1.0
        ENDDO
      ENDDO

      DEALLOCATE (nuse_tsta_dist)

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
      USE PRMS_BASIN, ONLY: Nhru, Active_hrus, Hru_route_order,
     +    Basin_area_inv, Hru_area
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp,
     +    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf,
     +    Tavgc, Temp_units, Basin_tsta, Ntemp
      USE PRMS_OBS, ONLY: Nowtime, Tmax, Tmin
      IMPLICIT NONE
      REAL, EXTERNAL :: c_to_f, f_to_c
      INTRINSIC FLOAT
! Local Variables
      INTEGER :: j, k, ntotx, ntotn, mon, jj, kk, allmissing
      REAL :: tmx, tmn, tcrx, tcrn
      REAL :: sumdist, sumtx, sumtn, diffn, diffx, mx, mn
      REAL :: lapsemaxmax, lapsemaxmin, lapseminmax, lapseminmin
!***********************************************************************
      mon = Nowtime(2)
      mn = Monmin(mon)
      mx = Monmax(mon)

      Basin_tmax = 0.
      Basin_tmin = 0.
      Basin_temp = 0.

! Calculate basin-average lapse rate using all NTEMP stations

      lapsemaxmax = Lapsemax_max(mon)
      lapsemaxmin = Lapsemax_min(mon)
      lapseminmax = Lapsemin_max(mon)
      lapseminmin = Lapsemin_min(mon)

      sumtx = 0.0
      sumtn = 0.0
      ntotx = 0
      ntotn = 0
      allmissing = 0
      DO j = 1, Ntemp - 1

! check for missing or bad temps based on min and max daily values
! observed for each month. 

! the value of  -9999 = missing in HDB, and rdb

        IF ( Tmax(j).LT.mn ) CYCLE
        IF ( Tmin(j).LT.mn ) CYCLE
        IF ( Tmax(j).GT.mx ) CYCLE
        IF ( Tmin(j).GT.mx ) CYCLE

        DO k = j + 1, Ntemp

          IF ( Tmax(k).LT.mn ) CYCLE
          IF ( Tmin(k).LT.mn ) CYCLE
          IF ( Tmax(k).GT.mx ) CYCLE
          IF ( Tmin(k).GT.mx ) CYCLE
          allmissing = 1

          diffx = (Tmax(j)-Tmax(k))/Delv(j, k)
          diffn = (Tmin(j)-Tmin(k))/Delv(j, k)
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
      IF ( allmissing==0 ) THEN
        PRINT *,'Error, all temperature stations have missing data',
     +          Nowtime
        STOP
      ENDIF

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
        sumdist = 0.0

        DO kk = 1, N_tsta(j)
          k = Nuse_tsta(kk, j)

! check for missing or bad temps
          IF ( Tmax(k).LT.mn ) CYCLE
          IF ( Tmin(k).LT.mn ) CYCLE
          IF ( Tmax(k).GT.mx ) CYCLE
          IF ( Tmin(k).GT.mx ) CYCLE

          sumdist = sumdist + Dist(j, k)
          tcrx = Basin_lapse_max*Elfac(j, k)
          tcrn = Basin_lapse_min*Elfac(j, k)
          tmx = tmx + (Tmax(k)+tcrx)*Dist(j, k)
          tmn = tmn + (Tmin(k)+tcrn)*Dist(j, k)
        ENDDO

        IF ( sumdist.GT.0.0 ) THEN
          tmn = tmn/sumdist - Tmin_mo_adj(j, mon)
          tmx = tmx/sumdist - Tmax_mo_adj(j, mon)
        ELSE
          PRINT *, 'warning, HRU:', j, Nowtime,
     +             ' no climate stations used to set temperature'
          tmn = (mn+mx)*0.5
          tmx = tmn
        ENDIF

        IF ( tmx.LE.tmn ) tmx = tmn + 0.01

        IF ( Temp_units.EQ.0 ) THEN
!         degrees F
          Tmaxf(j) = tmx
          Tminf(j) = tmn
          Tavgf(j) = (tmx+tmn)*0.5
          Tmaxc(j) = f_to_c(tmx)
          Tminc(j) = f_to_c(tmn)
          Tavgc(j) = f_to_c(Tavgf(j))
          Basin_temp = Basin_temp + Tavgf(j)*Hru_area(j)
        ELSE
!         degrees C
          Tmaxc(j) = tmx
          Tminc(j) = tmn
          Tavgc(j) = (tmx+tmn)*0.5
          Tmaxf(j) = c_to_f(tmx)
          Tminf(j) = c_to_f(tmn)
          Tavgf(j) = c_to_f(Tavgc(j))
          Basin_temp = Basin_temp + Tavgc(j)*Hru_area(j)
        ENDIF

        Basin_tmax = Basin_tmax + tmx*Hru_area(j)
        Basin_tmin = Basin_tmin + tmn*Hru_area(j)
      ENDDO

      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv
      Basin_temp = Basin_temp*Basin_area_inv

      Solrad_tmax = Tmax(Basin_tsta)
      Solrad_tmin = Tmin(Basin_tsta)

      t2dist2run = 0
      END FUNCTION t2dist2run

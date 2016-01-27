!***********************************************************************
! Compute potential solar radiation and sunlight hours for each HRU for
! each day of year; modification of soltab_prms
!
! References -- you *will* need these to figure out what is going on:
!   Swift, L.W., Jr., 1976, Algorithm for solar radiation on mountain
!   slopes: Water Resources Research, v. 12, no. 1, p. 108-112.
!
!   Lee, R., 1963, Evaluation of solar beam irradiation as a climatic parameter
!   of mountain watersheds, Colorado State University Hydrology Papers, 2,
!   50 pp. 
!***********************************************************************
      MODULE PRMS_SOLTAB
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: PI=3.1415926535898, ECCENTRICY = 0.01671, DAYSYR = 365.242
      REAL, PARAMETER :: RADIANS=PI/180.0, TWOPI=2.0*PI, PI_12=12.0/PI
      REAL, PARAMETER :: DEGDAY = 360.0/DAYSYR
      REAL, PARAMETER :: DEGDAYRAD = DEGDAY*RADIANS
! TWOPI ~ 6.2831853071786
! RADIANS ~ 0.017453292519943
! PI_12 ~ 3.8197186342055
      CHARACTER(LEN=6), SAVE :: MODNAME
      REAL, SAVE :: Solar_declination(366), Soltab_basinpotsw(366)
      REAL, SAVE, ALLOCATABLE :: Hru_cossl(:), Soltab_sunhrs(:, :)
      REAL, SAVE, ALLOCATABLE :: Soltab_potsw(:, :), Soltab_horad_potsw(:, :)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Hru_aspect(:), Hru_slope(:)
      END MODULE PRMS_SOLTAB

!***********************************************************************
!     Main soltab routine
!***********************************************************************
      INTEGER FUNCTION soltab()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: sthdecl, sthinit
!***********************************************************************
      soltab = 0

      IF ( Process(:4)=='decl' ) THEN
        soltab = sthdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        soltab = sthinit()
      ENDIF

      END FUNCTION soltab

!***********************************************************************
!     sthdecl - set up parameters for solar radiation computations
!   Declared Parameters
!     hru_aspect, hru_lat, hru_slope
!***********************************************************************
      INTEGER FUNCTION sthdecl()
      USE PRMS_SOLTAB
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declparam
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_soltab
!***********************************************************************
      sthdecl = 0

      Version_soltab = '$Id: soltab.f90 7563 2015-08-04 20:04:22Z rsregan $'
      CALL print_module(Version_soltab, 'Potential Solar Radiation   ', 90)
      MODNAME = 'soltab'

      ALLOCATE ( Soltab_potsw(366, Nhru), Soltab_sunhrs(366, Nhru) )
      ALLOCATE ( Hru_cossl(Nhru), Soltab_horad_potsw(366, Nhru) )

!   Declared Parameters
      ALLOCATE ( Hru_slope(Nhru) )
      IF ( declparam(MODNAME, 'hru_slope', 'nhru', 'real', &
     &     '0.0', '0.0', '10.0', &
     &     'HRU slope', &
     &     'Slope of each HRU, specified as change in vertical length divided by change in horizontal length', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'hru_slope')

      ALLOCATE ( Hru_aspect(Nhru) )
      IF ( declparam(MODNAME, 'hru_aspect', 'nhru', 'real', &
     &     '0.0', '0.0', '360.0', &
     &     'HRU aspect', 'Aspect of each HRU', &
     &     'angular degrees')/=0 ) CALL read_error(1, 'hru_aspect')

      END FUNCTION sthdecl

!***********************************************************************
!     sthinit - Initialize soltab module - get parameter values,
!               compute soltab_potsw (potential shortwave radiation)
!               and soltab_sunhrs (hours between sunrise and sunset)
!               for each HRU for each day of the year.
!***********************************************************************
      INTEGER FUNCTION sthinit()
      USE PRMS_SOLTAB
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Inputerror_flag, Parameter_check_flag
      USE PRMS_BASIN, ONLY: Hru_type, Active_hrus, Hru_route_order, Basin_lat, Hru_lat
      IMPLICIT NONE
! Functions
      INTRINSIC SIN, COS, FLOAT
!     INTRINSIC ASIN
      INTEGER, EXTERNAL :: getparam
      EXTERNAL compute_soltab, read_error, PRMS_open_module_file, check_param_limits
! Local Variables
      CHARACTER(LEN=12) :: output_path
      INTEGER :: jd, j, n, file_unit, ierr, nn, ierr2
      REAL :: basin_sunhrs(366), obliquity(366), basin_cossl, y, y2, y3, jdreal, lat
!***********************************************************************
      sthinit = 0

      IF ( getparam(MODNAME, 'hru_slope', Nhru, 'real', Hru_slope)/=0 ) CALL read_error(2, 'hru_slope')
      IF ( getparam(MODNAME, 'hru_aspect', Nhru, 'real', Hru_aspect)/=0 ) CALL read_error(2, 'hru_aspect')

      DO jd = 1, 366
        jdreal = FLOAT( jd )
!rsr .0172 = 2PI/365 = RADIAN_YEAR = DEGDAYRAD
!rsr01/2006 commented out equations from Llowd W. Swift paper 2/1976
!       obliquity(jd) = 1.0 - (0.0167*COS((jd-3)*0.0172))
        obliquity(jd) = 1.0 - (ECCENTRICY*COS((jdreal-3.0)*DEGDAYRAD))
!       Solar_declination(jd) = 0.007 - (0.4067*COS((jd+10)*0.0172))
!       Solar_declination(jd) = ASIN(0.39785 * SIN( (278.9709+DEGDAY*jd)*RADIANS + 1.9163*RADIANS * SIN((356.6153+DEGDAY*jd)*RADIANS )) )
        ! hour = 12.0
!       y = DEGDAYRAD*(jdreal-1.0 +(hour-12.0)/24.0)
        y = DEGDAYRAD*(jdreal-1.0) ! assume noon
        y2 = 2.0*y
        y3 = 3.0*y
        Solar_declination(jd) = 0.006918 - 0.399912*COS(y) + 0.070257*SIN(y) &
     &                          - 0.006758*COS(y2) + 0.000907*SIN(y2) &
     &                          - 0.002697*COS(y3) + 0.00148*SIN(y3)
      ENDDO

!   Module Variables
      Soltab_sunhrs = 0.0
      Soltab_potsw = 0.0
      Soltab_horad_potsw = 0.0
      DO nn = 1, Active_hrus
        n = Hru_route_order(nn)
        ierr = 0
        IF ( Hru_aspect(n)<0.0 ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT *, 'ERROR, hru_aspect<0.0 for HRU:', n, ', hru_aspect:', Hru_aspect(n), ', hru_slope:', Hru_slope(n)
            ierr = 1
          ELSE
            PRINT *, 'WARNING, hru_aspect<0.0', Hru_aspect(n), ' HRU:', n, ', hru_slope', Hru_slope(n)
            PRINT *, 'hru_aspect and hru_slope set to 0.0'
            Hru_aspect(n) = 0.0
            Hru_slope(n) = 0.0
          ENDIF
        ENDIF
        ierr2 = 0
        CALL check_param_limits(n, 'hru_slope', Hru_slope(n), 0.0, 89.99, ierr2)
        IF ( ierr==1 .OR. ierr2==1 ) THEN
          Inputerror_flag = 1
          CYCLE
        ENDIF

        CALL compute_soltab(obliquity, Solar_declination, 0.0, 0.0, Hru_lat(n), &
     &                      Hru_cossl(n), Soltab_horad_potsw(1, n), &
     &                      Soltab_sunhrs(1, n), Hru_type(n), n)
        CALL compute_soltab(obliquity, Solar_declination, Hru_slope(n), Hru_aspect(n), &
     &                      Hru_lat(n), Hru_cossl(n), Soltab_potsw(1, n), &
     &                      Soltab_sunhrs(1, n), Hru_type(n), n)
      ENDDO

      lat = SNGL( Basin_lat )
      CALL compute_soltab(obliquity, Solar_declination, 0.0, 0.0, lat, basin_cossl, &
     &                    Soltab_basinpotsw, basin_sunhrs, 0, 0)

      IF ( Print_debug==5 ) THEN
        output_path = 'soltab_debug'
        PRINT *, ''
        PRINT *, 'soltab debug data written to: ', output_path
        CALL PRMS_open_module_file(file_unit, output_path)
        DO n = 1, Nhru
          WRITE ( file_unit, * ) 'HRU:', n
          WRITE ( file_unit, * ) '***Soltab_sunhrs***'
          WRITE ( file_unit, '(13F8.3)' ) (Soltab_sunhrs(j,n), j=1,366)
          WRITE ( file_unit, * ) '***Soltab_potsw***'
          WRITE ( file_unit, '(13F8.3)' ) (Soltab_potsw(j,n), j=1,366)
        ENDDO
!       WRITE ( file_unit, * ) obliquity, Solar_declination
        WRITE ( file_unit, * ) 2.0/(obliquity(356)*obliquity(356)), 2.0/(obliquity(10)*obliquity(10)), &
     &                         2.0/(obliquity(23)*obliquity(23)), 2.0/(obliquity(38)*obliquity(38)), &
     &                         2.0/(obliquity(51)*obliquity(51)), 2.0/(obliquity(66)*obliquity(66)), &
     &                         2.0/(obliquity(80)*obliquity(80)), 2.0/(obliquity(94)*obliquity(94)), &
     &                         2.0/(obliquity(109)*obliquity(109)), 2.0/(obliquity(123)*obliquity(123)), &
     &                         2.0/(obliquity(138)*obliquity(138)), 2.0/(obliquity(152)*obliquity(152)), &
     &                         2.0/(obliquity(173)*obliquity(173))
        WRITE ( file_unit, * ) Solar_declination(356), Solar_declination(10), Solar_declination(23), &
     &                         Solar_declination(38), Solar_declination(51), Solar_declination(66), &
     &                         Solar_declination(80), Solar_declination(94), Solar_declination(109), &
     &                         Solar_declination(123), Solar_declination(138), Solar_declination(152), &
     &                         Solar_declination(173)
        CLOSE ( file_unit) 
! from original soltab
!     data obliquity/2.06699,2.06317,2.05582,2.04520,2.03243,2.01706,2.00080,
!    +1.98553,1.96990,1.95714,1.94689,1.94005,1.93616/

!     data Solar_declination/-.410152,-.383391,-.337430,-.27198,-.190532,-.09832,0.,
!    +.09832,.190532,.27198,.33743,.383391,.410152/

!     data jday/356,10,23,38,51,66,80,94,109,123,138,152,173/
      ENDIF

      DEALLOCATE ( Hru_slope, Hru_aspect )

      END FUNCTION sthinit

!***********************************************************************
!  compute soltab_potsw (potential shortwave radiation)
!  and soltab_sunhrs (hours between sunrise and sunset)
!  for each HRU for each day of the year.
!***********************************************************************
      SUBROUTINE compute_soltab(Obliquity, Solar_declination, Slope, Aspect, &
     &                          Latitude, Cossl, Soltab_potetsw, Sunhrs, Hru_type, Id)
      USE PRMS_SOLTAB, ONLY: PI, TWOPI, RADIANS, PI_12
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      EXTERNAL compute_t
!     Functions
      REAL, EXTERNAL :: func3
      INTRINSIC ASIN, SIN, COS, ATAN, ABS
!     Arguments
      INTEGER, INTENT(IN) :: Hru_type, Id
      REAL, INTENT(IN) :: Obliquity(366), Solar_declination(366), Slope, Aspect, Latitude
      REAL, INTENT(OUT) :: Cossl, Soltab_potetsw(366), Sunhrs(366)
!     Local Variables
      INTEGER :: jd
      REAL :: a, x0, x1, x2, r0, r1, d1, t, sunh, solt, t0, t1, t2, t3, t6, t7, d, sl
!***********************************************************************
! from SWIFT (1976)
! x0, x1, x2 = l0, l1, l2
! sl = i

      sl = ATAN(Slope)
      Cossl = COS(sl)
      a = Aspect*RADIANS

! x0 latitude of HRU
      x0 = Latitude*RADIANS

! x1 latitude of equivalent slope
! This is equation 13 from Lee, 1963
      x1 = ASIN(Cossl*SIN(x0)+SIN(sl)*COS(x0)*COS(a))

! d1 is the denominator of equation 12, Lee, 1963
      d1 = Cossl*COS(x0) - SIN(sl)*SIN(x0)*COS(a)
      IF ( ABS(d1)<NEARZERO ) d1 = NEARZERO

! x2 is the difference in longitude between the location of
! the HRU and the equivalent horizontal surface expressed in angle hour
! This is equation 12 from Lee, 1963
      x2 = ATAN(SIN(sl)*SIN(a)/d1)
      IF ( d1<0.0 ) x2 = x2 + PI

! r0 is the minute solar constant cal/cm2/min
      r0 = 2.0
! r0 could be 1.95 (Drummond, et al 1968)
      DO jd = 1, 366
        d = Solar_declination(jd)

! This is adjusted to express the variability of available insolation as
! a function of the earth-sun distance.  Lee, 1963, p 16.
! r1 is the hour solar constant cal/cm2/hour
! r0 is the minute solar constant cal/cm2/min
! 60.0D0 is minutes in an hour
! Obliquity is the obliquity of the ellipse of the earth's orbit around the sun. E
! is also called the radius vector of the sun (or earth) and is the ratio of
! the earth-sun distance on a day to the mean earth-sun distance.
! obliquity = ~23.439 (obliquity of sun)
        r1 = 60.0*r0/(Obliquity(jd)*Obliquity(jd))

!  compute_t is the sunrise equation.
!  t7 is the hour angle of sunset on the equivalent slope
!  t6 is the hour angle of sunrise on the equivalent slope
        CALL compute_t(x1, d, t)
        t7 = t - x2
        t6 = -t - x2

!  compute_t is the sunrise equation.
!  t1 is the hour angle of sunset on a hroizontal surface at the HRU
!  t0 is the hour angle of sunrise on a hroizontal surface at the HRU
        CALL compute_t(x0, d, t)
        t1 = t
        t0 = -t

! For HRUs that have an east or west direction component to their aspect, the
! longitude adjustment (moving the effective slope east or west) will cause either:
! (1) sunrise to be earlier than at the horizontal plane at the HRU
! (2) sunset to be later than at the horizontal plane at the HRU
! This is not possible. The if statements below check for this and adjust the
! sunrise/sunset angle hours on the equivalent slopes as necessary.
!
! t3 is the hour angle of sunrise on the slope at the HRU
! t2 is the hour angle of sunset on the slope at the HRU
        IF ( t7>t1 ) THEN
          t3 = t1
        ELSE
          t3 = t7
        ENDIF
        IF ( t6<t0 ) THEN
          t2 = t0
        ELSE
          t2 = t6
        ENDIF

        IF ( ABS(sl)<NEARZERO ) THEN
!  solt is Swift's R4 (potential solar radiation on a sloping surface cal/cm2/day)
!  Swift, 1976, equation 6
          solt = func3(0.0, x0, t1, t0, r1, d)
!  sunh is the number of hours of direct sunlight (sunset minus sunrise) converted
!  from angle hours in radians to hours (24 hours in a day divided by 2 pi radians
!  in a day).
          sunh = (t1-t0)*PI_12
        ELSE
          IF ( t3<t2 ) THEN
            t2 = 0.0
            t3 = 0.0
          ENDIF
          t6 = t6 + TWOPI
          IF ( t6<t1 ) THEN
            solt = func3(x2, x1, t3, t2, r1, d) + func3(x2, x1, t1, t6, r1, d)
            sunh = (t3-t2+t1-t6)*PI_12
          ELSE
            t7 = t7 - TWOPI
            IF ( t7>t0 ) THEN
              solt = func3(x2, x1, t3, t2, r1, d) + func3(x2, x1, t7, t0, r1, d)
              sunh = (t3-t2+t7-t0)*PI_12
            ELSE
              solt = func3(x2, x1, t3, t2, r1, d)
              sunh = (t3-t2)*PI_12
            ENDIF
          ENDIF
        ENDIF
        IF ( solt<0.0 ) THEN
          PRINT *, 'WARNING: solar table value for day:', jd, &
     &             ' computed as:', solt, ' set to', 0.0, &
     &             ' for HRU:', Id, ' hru_type:', Hru_type
          PRINT *, 'slope, aspect, latitude, cossl', Slope, Aspect, Latitude, Cossl
          solt = 0.0
          PRINT *, Slope, Aspect, Latitude, Cossl, sunh
          PRINT *, t0, t1, t2, t3, t6, t7, d
        ENDIF
        IF ( sunh<NEARZERO ) sunh = 0.0
        Sunhrs(jd) = sunh
        Soltab_potetsw(jd) = solt

      ENDDO

      END SUBROUTINE compute_soltab

!***********************************************************************
!***********************************************************************
      SUBROUTINE compute_t(Lat, Solar_declination, T)
      USE PRMS_SOLTAB, ONLY: PI
      IMPLICIT NONE
      INTRINSIC TAN, ACOS
! Arguments
      REAL, INTENT(IN) :: Lat, Solar_declination
      REAL, INTENT(OUT) :: T
! Local Variables
      REAL :: tx
!***********************************************************************

!  This is the sunrise equation
!  Lat is the latitude
!  Solar_declination is the declination of the sun on a day
!  T is the angle hour from the local meridian (local solar noon) to the 
!  sunrise (negative) or sunset (positive).  The Earth rotates at the angular
!  speed of 15�/hour (2 pi / 24 hour in radians) and, therefore, T/15� (T*24/pi
!  in radians) gives the time of sunrise as the number of hours before the local
!  noon, or the time of sunset as the number of hours after the local noon.
!  Here the term local noon indicates the local time when the sun is exactly to
!  the south or north or exactly overhead.
      tx = -TAN(Lat)*TAN(Solar_declination)
      IF ( tx<-1.0 ) THEN
        T = PI
!rsr bug fix, old code would set t=acos(0.0) for tx>1 12/05
      ELSEIF ( tx>1.0 ) THEN
        T = 0.0
      ELSE
        T = ACOS(tx)
      ENDIF

      END SUBROUTINE compute_t

!***********************************************************************
!***********************************************************************
      REAL FUNCTION func3(V, W, X, Y, R1, Solar_declination)
      USE PRMS_SOLTAB, ONLY: PI_12
      IMPLICIT NONE
      INTRINSIC SIN, COS
! Arguments
      REAL, INTENT(IN) :: V, W, X, Y, R1, Solar_declination
!***********************************************************************
!  This is the radian angle version of FUNC3 (eqn 6) from Swift, 1976
!  or Lee, 1963 equation 5.
!  func3 (R4) is potential solar radiation on the surface cal/cm2/day
!  V (L2) latitude angle hour offset between actual and equivalent slope
!  W (L1) latitude of the equivalent slope
!  X (T3) hour angle of sunset on equivalent slope
!  Y (T2) hour angle of sunrise on equivalent slope
!  R1 solar constant for 60 minutes
!  Solar_declination declination of sun
      func3 = R1*PI_12*(SIN(Solar_declination)*SIN(W)*(X-Y) + COS(Solar_declination)*COS(W)*(SIN(X+V)-SIN(Y+V)))

      END FUNCTION func3

!***********************************************************************
! Compute potential solar radiation and sunlight hours for each HRU
! for each day of year
!
! References -- you *will* need these to figure out what is going on:
!   Swift, L.W., Jr., 1976, Algorithm for solar radiation on mountain
!   slopes: Water Resources Research, v. 12, no. 1, p. 108-112.
!
!   Lee, R., 1963, Evaluation of solar beam irradiation as a climatic parameter
!   of mountain watersheds, Colorado State University Hydrology Papers, 2,
!   50 pp.
!
!
!***********************************************************************
      MODULE PRMS_SOLTAB
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru
      DOUBLE PRECISION, PARAMETER :: CLOSEZERO=1.0D-15
      DOUBLE PRECISION, PARAMETER :: PI=3.1415926535898D0
      DOUBLE PRECISION, PARAMETER :: RADIANS=PI/180.0D0, TWOPI=2.0D0*PI
      DOUBLE PRECISION, PARAMETER :: DAYSYR=365.242D0
      DOUBLE PRECISION, PARAMETER :: PI_12=12.0D0/PI
      DOUBLE PRECISION, PARAMETER :: ECCENTRICY=0.01671D0
      DOUBLE PRECISION, PARAMETER :: DEGDAY=360.0D0/DAYSYR
      DOUBLE PRECISION, PARAMETER :: DEGDAYRAD=DEGDAY*RADIANS
! TWOPI = 6.2831853071786
! RADIANS = 0.017453292519943
! PI_12 ~ 3.8197186342055
! DEGDAY = 360 degrees/days in year
! obliquity = 23.439 (obliquity of sun)
!   Declared Variables
      REAL, ALLOCATABLE :: Hru_cossl(:), Soltab_basinpotsw(:)
      REAL, ALLOCATABLE :: Soltab_potsw(:, :), Soltab_sunhrs(:, :)
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug, Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
!   Declared Parameters
      REAL :: Basin_lat
      REAL, ALLOCATABLE :: Hru_aspect(:), Hru_lat(:), Hru_slope(:)
      END MODULE PRMS_SOLTAB

!***********************************************************************
!     Main soltab routine
!***********************************************************************
      INTEGER FUNCTION soltab_hru_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: sthdecl, sthinit
!***********************************************************************
      soltab_hru_prms = 0

      IF ( Arg.EQ.'declare' ) THEN
        soltab_hru_prms = sthdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        soltab_hru_prms = sthinit()
      ENDIF

      END FUNCTION soltab_hru_prms

!***********************************************************************
!     sthdecl - set up parameters for solar radiation computations
!   Declared Parameters
!     hru_aspect, hru_lat, hru_slope, basin_lat
!***********************************************************************
      INTEGER FUNCTION sthdecl()
      USE PRMS_SOLTAB
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      sthdecl = 1

      IF ( declmodule(
     +'$Id: soltab_hru_prms.f 3637 2007-12-05 20:35:19Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

!   Declared Variables
      ALLOCATE (Soltab_potsw(MAXDAY, Nhru))
      IF ( declvar('soltab', 'soltab_potsw', 'ndays,nhru', MAXDAY*Nhru,
     +     'real', 'Potential daily shortwave radiation for each HRU',
     +     'langleys', 
     +     Soltab_potsw).NE.0 ) RETURN

      ALLOCATE (Soltab_sunhrs(MAXDAY, Nhru))
      IF ( declvar('soltab', 'soltab_sunhrs', 'ndays,nhru', MAXDAY*Nhru,
     +     'real', 'Hours between sunrise and sunset for each HRU',
     +     'hours', 
     +     Soltab_sunhrs).NE.0 ) RETURN

      ALLOCATE (Hru_cossl(Nhru))
      IF ( declvar('soltab', 'hru_cossl', 'nhru', Nhru, 'real',
     +     'Cosine of each HRU slope',
     +     'none',
     +     Hru_cossl).NE.0 ) RETURN

      ALLOCATE (Soltab_basinpotsw(MAXDAY))
      IF ( declvar('soltab', 'soltab_basinpotsw', 'ndays', MAXDAY,
     +     'real',
     +     'Potential daily shortwave radiation for basin centroid on'//
     +     ' a horizontal surface',
     +     'langleys',
     +     Soltab_basinpotsw).NE.0 ) RETURN

!   Declared Parameters
      ALLOCATE (Hru_slope(Nhru))
      IF ( declparam('basin', 'hru_slope', 'nhru', 'real',
     +     '0.0', '0.0', '10.0',
     +     'HRU slope',
     +     'Slope of each HRU, specified as change in vertical length'//
     +     ' divided by change in horizontal length',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_aspect(Nhru))
      IF ( declparam('soltab', 'hru_aspect', 'nhru', 'real',
     +     '0.0', '0.0', '360.0',
     +     'HRU aspect', 'Aspect of each HRU',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Hru_lat(Nhru))
      IF ( declparam('soltab', 'hru_lat', 'nhru', 'real',
     +     '40.0', '-90.0', '90.0',
     +     'HRU latitude', 'Latitude of each HRU',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('soltab', 'basin_lat', 'one', 'real',
     +     '40.0', '-90.0', '90.0',
     +     'Latitude of basin centroid', 'Latitude of basin centroid',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Hru_route_order(Nhru))

      sthdecl = 0

      END FUNCTION sthdecl

!***********************************************************************
!     sthinit - Initialize soltab module - get parameter values,
!               compute soltab_potsw (potential shortwave radiation)
!               and soltab_sunhrs (hours between sunrise and sunset)
!               for each HRU for each day of the year.
!***********************************************************************
      INTEGER FUNCTION sthinit()
      USE PRMS_SOLTAB
      IMPLICIT NONE
      INTRINSIC SIN, COS, DBLE
!     INTRINSIC ASIN
      INCLUDE 'fmodules.inc'
      EXTERNAL compute_soltab
! Local Variables
      CHARACTER(LEN=12) :: output_path
      LOGICAL :: filflg
      INTEGER :: jd, j, n, file_unit, nn
      REAL :: basin_cossl
      REAL, ALLOCATABLE :: basin_sunhrs(:)
      DOUBLE PRECISION, ALLOCATABLE :: e(:), dm(:)
      DOUBLE PRECISION :: y, y2, y3, jddbl
!***********************************************************************
      sthinit = 1

      ALLOCATE (basin_sunhrs(MAXDAY), e(MAXDAY), dm(MAXDAY))

      IF ( getparam('soltab', 'hru_slope', Nhru, 'real', Hru_slope)
     +     .NE.0 ) RETURN

      IF ( getparam('soltab', 'hru_aspect', Nhru, 'real', Hru_aspect)
     +     .NE.0 ) RETURN

      IF ( getparam('soltab', 'hru_lat', Nhru, 'real', Hru_lat)
     +     .NE.0 ) RETURN

      IF ( getparam('soltab', 'basin_lat', 1, 'real', Basin_lat)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      DO jd = 1, MAXDAY
        jddbl = DBLE(jd)
!rsr .0172 = 2PI/365 = RADIAN_YEAR = DEGDAYRAD
!rsr01/2006 commented out equations from Llowd W. Swift paper 2/1976
!       e(jd) = 1.0 - (.0167*COS((jd-3)*.0172))
        e(jd) = 1.0D0 - (ECCENTRICY*COS((jddbl-3.0D0)*DEGDAYRAD))
!       dm(jd) = .007 - (.4067*COS((jd+10)*.0172))
!       dm(jd) = ASIN(0.39785 * SIN( (278.9709+DEGDAY*jd)*RADIANS +
!    +           1.9163*RADIANS * SIN((356.6153+DEGDAY*jd)*RADIANS )) )
!       y = DEGDAYRAD*(jddbl-1.0D0 +(hour-12.0D0)/24.0D0)
        y = DEGDAYRAD*(jddbl-1.0D0)
        y2 = 2.0D0*y
        y3 = 3.0D0*y
! dm = solar declination
        dm(jd) = 0.006918D0 - 0.399912D0*COS(y) + 0.070257D0*SIN(y)
     +           - 0.006758D0*COS(y2) + 0.000907D0*SIN(y2)
     +           - 0.002697D0*COS(y3) + 0.00148D0*SIN(y3)
      ENDDO

      CALL compute_soltab(e, dm, 0.0, 0.0, Basin_lat, basin_cossl,
     +                    Soltab_basinpotsw, basin_sunhrs)
      DO nn = 1, Active_hrus
        n = Hru_route_order(nn)
        CALL compute_soltab(e, dm, Hru_slope(n), Hru_aspect(n),
     +                      Hru_lat(n), Hru_cossl(n), Soltab_potsw(1, n)
     +                      , Soltab_sunhrs(1, n))
      ENDDO

      IF ( Prt_debug.EQ.5 ) THEN
        output_path = 'soltab_debug'
        file_unit = 91
        INQUIRE (FILE=output_path, EXIST=filflg)
        IF ( filflg ) THEN
          OPEN (UNIT=file_unit, FILE=output_path, STATUS='old')
          CLOSE (UNIT=file_unit, STATUS='delete')
        ENDIF
        PRINT *, ''
        PRINT *, 'soltab debug data written to: ', output_path
        OPEN (UNIT=file_unit, FILE=output_path, ACCESS='sequential',
     +        FORM='formatted', STATUS='new')
        DO nn = 1, Active_hrus
          n = Hru_route_order(nn)
          WRITE (file_unit, *) 'HRU:', n
          WRITE (file_unit, *) '***Soltab_sunhrs***'
          WRITE (file_unit, '(13F8.3)') (Soltab_sunhrs(j,n), j=1,MAXDAY)
          WRITE (file_unit, *) '***Soltab_potsw***'
          WRITE (file_unit, '(13F8.3)') (Soltab_potsw(j,n), j=1,MAXDAY)
        ENDDO
!       WRITE (file_unit, *) e, dm
        WRITE (file_unit, *) 2./(e(356)*e(356)), 2./(e(10)*e(10)),
     +                       2./(e(23)*e(23)), 2./(e(38)*e(38)),
     +                       2./(e(51)*e(51)), 2./(e(66)*e(66)),
     +                       2./(e(80)*e(80)), 2./(e(94)*e(94)),
     +                       2./(e(109)*e(109)), 2./(e(123)*e(123)),
     +                       2./(e(138)*e(138)), 2./(e(152)*e(152)),
     +                       2./(e(173)*e(173))
        WRITE (file_unit, *) dm(356), dm(10), dm(23), dm(38), dm(51),
     +                       dm(66), dm(80), dm(94), dm(109), dm(123),
     +                       dm(138), dm(152), dm(173)
        CLOSE (file_unit)
! from original soltab
!     data e/2.06699,2.06317,2.05582,2.04520,2.03243,2.01706,2.00080,
!    +1.98553,1.96990,1.95714,1.94689,1.94005,1.93616/

!     data dm/-.410152,-.383391,-.337430,-.27198,-.190532,-.09832,0.,
!    +.09832,.190532,.27198,.33743,.383391,.410152/

!     data jday/356,10,23,38,51,66,80,94,109,123,138,152,173/
      ENDIF

      DEALLOCATE (basin_sunhrs, e, dm)
      sthinit = 0
      END FUNCTION sthinit

!***********************************************************************
!  compute soltab_potsw (potential shortwave radiation)
!  and soltab_sunhrs (hours between sunrise and sunset)
!  for each HRU for each day of the year.
!***********************************************************************
      SUBROUTINE compute_soltab(E, Dm, Slope, Aspect, Latitude, Cossl,
     +                          Soltab, Sunhrs)
      USE PRMS_SOLTAB, ONLY:PI, TWOPI, RADIANS, CLOSEZERO, PI_12
      IMPLICIT NONE
      EXTERNAL compute_t
      INCLUDE 'fmodules.inc'
!     Functions
      DOUBLE PRECISION, EXTERNAL :: func3
      INTRINSIC ASIN, SIN, COS, ATAN, ABS, SNGL
!     Arguments
      DOUBLE PRECISION, INTENT(IN), DIMENSION(MAXDAY) :: E, Dm
      REAL, INTENT(IN) :: Slope, Aspect, Latitude
      REAL, INTENT(OUT) :: Cossl
      REAL, INTENT(OUT), DIMENSION(MAXDAY) :: Sunhrs, Soltab
!     Local Variables
      INTEGER :: jd
      DOUBLE PRECISION :: a, x0, x1, x2, r0, r1, d1, t, sunh, solt
      DOUBLE PRECISION :: t0, t1, t2, t3, t6, t7, d, sl
!***********************************************************************
! from SWIFT (1976)
! x0, x1, x2 = l0, l1, l2
! sl = i

      sl = ATAN(Slope)
      a = Aspect*RADIANS

! x0 latitude of HRU
      x0 = Latitude*RADIANS
      Cossl = SNGL(COS(sl))

! x1 latitude of equivalent slope
! This is equation 13 from Lee, 1963
      x1 = ASIN(Cossl*SIN(x0)+SIN(sl)*COS(x0)*COS(a))

! d1 is the denominator of equation 12, Lee, 1963
      d1 = Cossl*COS(x0) - SIN(sl)*SIN(x0)*COS(a)
      IF ( ABS(d1).LT.CLOSEZERO ) d1 = .0000000001D0

! x2 is the difference in longitude between the location of
! the HRU and the equivalent horizontal surface expressed in angle hour
! This is equation 12 from Lee, 1963
      x2 = ATAN(SIN(sl)*SIN(a)/d1)
      IF ( d1.LT.0.0D0 ) x2 = x2 + PI

! r0 is the minute solar constant cal/cm2/min
      r0 = 2.0D0
! r0 could be 1.95 (Drummond, et al 1968)
      DO jd = 1, MAXDAY
        d = Dm(jd)

! This is adjusted to express the variability of available insolation as
! a function of the earth-sun distance.  Lee, 1963, p 16.
! r1 is the hour solar constant cal/cm2/hour
! r0 is the minute solar constant cal/cm2/min
! E is the obliquity of the ellipse of the earth's orbit around the sun. E
! is also called the radius vector of the sun (or earth) and is the ratio of
! the earth-sun distance on a day to the mean earth-sun distance.
        r1 = 60.0D0*r0/(E(jd)*E(jd))

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
! t3 is the hour angle of sunset on the slope at the HRU
! t2 is the hour angle of sunset on the slope at the HRU
        IF ( t7.GT.t1 ) THEN
          t3 = t1
        ELSE
          t3 = t7
        ENDIF
        IF ( t6.LT.t0 ) THEN
          t2 = t0
        ELSE
          t2 = t6
        ENDIF

        IF ( ABS(sl).LT.CLOSEZERO ) THEN
!  solt is Swift's R4 (potential solar radiation on a sloping surface cal/cm2/day)
!  Swift, 1976, equation 6
          solt = func3(0.0D0, x0, t1, t0, r1, d)
!  sunh is the number of hours of direct sunlight (sunset minus sunrise) converted
!  from angle hours in radians to hours (24 hours in a day divided by 2 pi radians
!  in a day).
          sunh = (t1-t0)*PI_12
        ELSE
          IF ( t3.LT.t2 ) THEN
            t2 = 0.0D0
            t3 = 0.0D0
          ENDIF
          t6 = t6 + TWOPI
          IF ( t6.LT.t1 ) THEN
            solt = func3(x2, x1, t3, t2, r1, d)
     +             + func3(x2, x1, t1, t6, r1, d)
            sunh = (t3-t2+t1-t6)*PI_12
          ELSE
            t7 = t7 - TWOPI
            IF ( t7.GT.t0 ) THEN
              solt = func3(x2, x1, t3, t2, r1, d)
     +               + func3(x2, x1, t7, t0, r1, d)
              sunh = (t3-t2+t7-t0)*PI_12
            ELSE
              solt = func3(x2, x1, t3, t2, r1, d)
              sunh = (t3-t2)*PI_12
            ENDIF
          ENDIF
        ENDIF
        Sunhrs(jd) = SNGL(sunh)
        Soltab(jd) = SNGL(solt)

      ENDDO

      END SUBROUTINE compute_soltab

!***********************************************************************
!***********************************************************************
      SUBROUTINE compute_t(Lat, D, T)
      USE PRMS_SOLTAB, ONLY:PI
      IMPLICIT NONE
      INTRINSIC TAN, ACOS
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: Lat, D
      DOUBLE PRECISION, INTENT(OUT) :: T
! Local Variables
      DOUBLE PRECISION :: tx
!***********************************************************************

!  This is the sunrise equation
!  Lat is the latitude
!  D is the declination of the sun on a day
!  T is the angle hour from the local meridian (local solar noon) to the 
!  sunrise (negative) or sunset (positive).  The Earth rotates at the angular
!  speed of 15°/hour (2 pi / 24 hour in radians) and, therefore, T/15° (T*24/pi
!  in radians) gives the time of sunrise as the number of hours before the local
!  noon, or the time of sunset as the number of hours after the local noon.
!  Here the term local noon indicates the local time when the sun is exactly to
!  the south or north or exactly overhead.
      tx = -TAN(Lat)*TAN(D)
      IF ( tx.LT.-1.0D0 ) THEN
        T = PI
!rsr bug fix, old code would set t=acos(0.0) for tx>1 12/05
      ELSEIF ( tx.GT.1.0D0 ) THEN
        T = 0.0D0
      ELSE
        T = ACOS(tx)
      ENDIF

      END SUBROUTINE compute_t

!***********************************************************************
!***********************************************************************
      DOUBLE PRECISION FUNCTION func3(V, W, X, Y, R1, D)
      USE PRMS_SOLTAB, ONLY:PI_12
      IMPLICIT NONE
      INTRINSIC SIN, COS
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: V, W, X, Y, R1, D
!***********************************************************************
!  This is the radian angle version of FUNC3 (eqn 6) from Swift, 1976
!  or Lee, 1963 equation 5.
!  func3 (R4) is potential solar radiation on the surface cal/cm2/day
!  V (L2) latitude angle hour offset between actual and equivalent slope
!  W (L1) latitude of the equivalent slope
!  X (T3) hour angle of sunset on equivalent slope
!  Y (T2) hour angle of sunrise on equivalent slope
!  R1 solar constant for 60 minutes
!  D declination of sun
      func3 = R1*PI_12*(SIN(D)*SIN(W)*(X-Y) + COS(D)*COS(W)
     +        *(SIN(X+V)-SIN(Y+V)))

      END FUNCTION func3

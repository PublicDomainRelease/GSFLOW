!***********************************************************************
! precip_dist2_prms.f
! Determine form of precipitation (rain, snow, mix) and distribute
! precipitation to HRU's.
! Needs variable "precip" in the DATA FILE
! Needs observed variable tmax read in the temperature module
!
! MODIFIED 7/1998 ,JJ VACCARO, INTERPOLATES PRECIP BASED ON DISTANCE
!   SQUARED USING ALL STATIONS IDENTIFIED FOR BASIN, MAY GIVE MORE
! PRECIP IN LOWLANDS, BUT SMOOTHS OUT DISTRIBUTION, USES MONTHLY MEANS
! FOR 61-90 NORMAL PERIOD OF HRUS AND STATIONS FOR ADJUSTMENT
! The storm adjustment to precipitation during routing of less than
! daily time step has been eliminated under assumption improved
! adjustment is obtained by using more than one weather site.
!***********************************************************************
      MODULE PRMS_DIST2_PRECIP
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: INCH2MM = 25.4, NEARZERO = 1.0E-15
      INTEGER :: Nhru, Nrain, Nform
!     INTEGER, ALLOCATABLE :: Istack(:)
      REAL, ALLOCATABLE :: Tmax(:), Tmin(:), Dist2(:, :)
!   Declared Variables
      INTEGER, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      REAL :: Basin_ppt, Basin_obs_ppt, Basin_rain, Basin_snow
      REAL, ALLOCATABLE :: Hru_ppt(:), Hru_rain(:), Hru_snow(:), Prmx(:)
!   Declared Variables from other modules - obs
      INTEGER, ALLOCATABLE :: Form_data(:)
      REAL, ALLOCATABLE :: Precip(:)
!   Declared Variables from other modules - temp
      REAL :: Solrad_tmax
!   Declared Variables from other modules - basin
!dbg  INTEGER :: Prt_debug
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Precip_units, Temp_units
      REAL :: Tmax_allsnow
      REAL, ALLOCATABLE :: Hru_area(:), Tmax_allrain(:), Adjmix_rain(:)
      REAL, ALLOCATABLE :: Rain_mon(:, :), Snow_mon(:, :)
      REAL, ALLOCATABLE :: Psta_mon(:, :)
      REAL, ALLOCATABLE :: Maxmon_prec(:), Psta_xlong(:), Psta_ylat(:)
      REAL, ALLOCATABLE :: Hru_ylat(:), Hru_xlong(:)
      END MODULE PRMS_DIST2_PRECIP

!***********************************************************************
!     Main precip routine
!***********************************************************************
      INTEGER FUNCTION precip_dist2_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: pptdist2decl, pptdist2init, pptdist2run
!***********************************************************************
      precip_dist2_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        precip_dist2_prms = pptdist2run()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        precip_dist2_prms = pptdist2decl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        precip_dist2_prms = pptdist2init()
      ENDIF

      END FUNCTION precip_dist2_prms

!***********************************************************************
!     pptdist2decl - set up parameters for precipitation computations
!   Declared Parameters
!     tmax_allrain, tmax_allsnow, adjmix_rain
!     rain_mon, snow_mon, precip_units
!     hru_area, temp_units, maxmon_prec, dist2, psta_xlong, psta_ylong
!     hru_ylat, hru_xlong
!***********************************************************************
      INTEGER FUNCTION pptdist2decl()
      USE PRMS_DIST2_PRECIP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      pptdist2decl = 1

      IF ( declmodule(
     +'$Id: precip_dist2_prms.f 3773 2008-01-29 20:28:34Z rsregan $'
     +).NE.0 ) RETURN

      Nrain = getdim('nrain')
      IF ( Nrain.EQ.-1 ) RETURN
      IF ( Nrain.LT.2 ) THEN
        PRINT *, 'precip_dist2 requires at least precipitation stations'
        RETURN
      ENDIF

      ALLOCATE (Precip(Nrain))
!     ALLOCATE (Istack(Nrain))

      Nform = getdim('nform')
      IF ( Nform.EQ.-1 ) RETURN
      IF ( Nform.GT.0 ) ALLOCATE (Form_data(Nform))

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN
      !rsr, Dist2 is computed, not a parameter
      ALLOCATE (Dist2(Nhru, Nrain))
!     IF ( decl param('precip', 'dist2', 'nhru,nrain', 'real',
!    +     '1.0', '0.0', '50.0',
!    +     '1/Distance**2 from HRU to each of the nrain precip sites',
!    +     'Distance**2 to weight precipitation gages to'//
!    +     ' each HRU to account for closeness of gages'//
!    +     ' although /5280., meters or feet as x,y are OK',
!    +     'miles').NE.0 ) RETURN

      IF ( declvar('precip', 'basin_rain', 'one', 1, 'real',
     +     'Area weighted adjusted average rain for basin',
     +     'inches',
     +     Basin_rain).NE.0 ) RETURN

      IF ( declvar('precip', 'basin_snow', 'one', 1, 'real',
     +     'Area weighted adjusted average snow for basin',
     +     'inches',
     +     Basin_snow).NE.0 ) RETURN

      IF ( declvar('precip', 'basin_ppt', 'one', 1, 'real',
     +     'Area weighted adjusted average precip for basin',
     +     'inches',
     +     Basin_ppt).NE.0 ) RETURN

      IF ( declvar('precip', 'basin_obs_ppt', 'one', 1, 'real',
     +     'Area weighted measured average precip for basin',
     +     'inches',
     +     Basin_obs_ppt).NE.0 ) RETURN

      ALLOCATE (Hru_ppt(Nhru))
      IF ( declvar('precip', 'hru_ppt', 'nhru', Nhru, 'real',
     +     'Adjusted precipitation on each HRU',
     +     'inches',
     +     Hru_ppt).NE.0 ) RETURN

      ALLOCATE (Hru_rain(Nhru))
      IF ( declvar('precip', 'hru_rain', 'nhru', Nhru, 'real',
     +     'Computed rain on each HRU',
     +     'inches',
     +     Hru_rain).NE.0 ) RETURN

      ALLOCATE (Hru_snow(Nhru))
      IF ( declvar('precip', 'hru_snow', 'nhru', Nhru, 'real',
     +     'Computed snow on each HRU',
     +     'inches',
     +     Hru_snow).NE.0 ) RETURN

      ALLOCATE (Prmx(Nhru))
      IF ( declvar('precip', 'prmx', 'nhru', Nhru, 'real',
     +     'Proportion of rain in a mixed event',
     +     'decimal fraction',
     +     Prmx).NE.0 ) RETURN

      ALLOCATE (Pptmix(Nhru))
      IF ( declvar('precip', 'pptmix', 'nhru', Nhru, 'integer',
     +     'Precipitation mixture (0=no; 1=yes)',
     +     'none',
     +     Pptmix).NE.0 ) RETURN

      ALLOCATE (Newsnow(Nhru))
      IF ( declvar('precip', 'newsnow', 'nhru', Nhru, 'integer',
     +     'New snow on HRU (0=no; 1=yes)',
     +     'none',
     +     Newsnow).NE.0 ) RETURN

! declare parameters
      ALLOCATE (Tmax_allrain(MAXMO))
      IF ( declparam('precip', 'tmax_allrain', 'nmonths', 'real',
     +     '40.', '0.', '90.',
     +     'Precip all rain if HRU max temperature above this value',
     +     'If maximum temperature of an HRU is greater than or equal'//
     +     ' to this value (for each month, January to December),'//
     +     ' precipitation is assumed to be rain,'//
     +     ' in deg C or F, depending on units of data',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('precip', 'tmax_allsnow', 'one', 'real',
     +     '32.', '-10.', '40.',
     +     'Precip all snow if HRU max temperature below this value',
     +     'If HRU maximum temperature is less than or equal to this'//
     +     ' value, precipitation is assumed to be snow,'//
     +     ' in deg C or F, depending on units of data',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Maxmon_prec(MAXMO))
      IF ( declparam('precip', 'maxmon_prec', 'nmonths', 'real',
     +     '5.', '0.', '15.',
     +     'Maximum monthly precipitation for any weather site',
     +     'If observed monthly precipitation is > maxmon_prec value,'//
     +     ' precipitation is assumed to be in error',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Adjmix_rain(MAXMO))
      IF ( declparam('precip', 'adjmix_rain', 'nmonths', 'real',
     +     '1.', '0.', '3.',
     +     'Adjustment factor for rain in a rain/snow mix',
     +     'Monthly factor to adjust rain proportion in a mixed'//
     +     ' rain/snow event',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Rain_mon(Nhru, MAXMO))
      IF ( declparam('precip', 'rain_mon', 'nhru,nmonths', 'real',
     +     '1.0', '0.0', '50.0',
     +     'Rain adjustment factor, by month for each HRU',
     +     'Monthly precipitation to adjust measured precipitation on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Snow_mon(Nhru, MAXMO))
      IF ( declparam('precip', 'snow_mon', 'nhru,nmonths', 'real',
     +     '1.0', '0.0', '50.0',
     +     'Snow adjustment factor, by month for each HRU',
     +     'Monthly precipitation to adjust snow on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Psta_mon(Nrain, MAXMO))
      IF ( declparam('precip', 'psta_mon', 'nrain,nmonths', 'real',
     +     '1.0', '0.0', '50.0',
     +     'Monthly precipitation for each of the nrain precip sites',
     +     ' Monthly precipitation to adjust HRU precipitation to'//
     +     ' each HRU to account for differences in elevation, etc.',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Psta_xlong(Nrain))
      IF ( declparam('precip', 'psta_xlong', 'nrain', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'Precipitation station longitude, state plane',
     +     'Longitude of each precipitation measurement station',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Psta_ylat(Nrain))
      IF ( declparam('precip', 'psta_ylat', 'nrain', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'Precipitation station latitude, state plane',
     +     'Latitude of each precipitation measurement station',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Hru_ylat(Nhru))
      IF ( declparam('precip', 'hru_ylat', 'nhru', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'HRU latitude of centroid, state plane',
     +     'Latitude of each HRU for the centroid',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Hru_xlong(Nhru))
      IF ( declparam('precip', 'hru_xlong', 'nhru', 'real',
     +     '0.', '-1e+09', '1e+09',
     +     'HRU longitude of centroid, state plane',
     +     'Longitude of each HRU for the centroid',
     +     'feet').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('precip', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      IF ( declparam('precip', 'precip_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for observed precipitation',
     +     'Units for observed precipitation (0=inches; 1=mm)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('precip', 'temp_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for observed temperature',
     +     'Units for observed temperature (0=Fahrenheit; 1=Celsius)',
     +     'none').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Tmax(Nhru), Tmin(Nhru), Hru_route_order(Nhru))

      pptdist2decl = 0
      END FUNCTION pptdist2decl

!***********************************************************************
!     pptdist2init - Initialize precip module - get parameter values
!***********************************************************************
      INTEGER FUNCTION pptdist2init()
      USE PRMS_DIST2_PRECIP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC SQRT
! Local Variables
      INTEGER :: i, k, j, jj
      REAL :: distx, disty, dist
!***********************************************************************
      pptdist2init = 1

      IF ( getstep().EQ.0 ) THEN
        Basin_obs_ppt = 0.0
        Basin_ppt = 0.0
        Basin_rain = 0.0
        Basin_snow = 0.0
        Hru_ppt = 0.0
        Hru_rain = 0.0
        Hru_snow = 0.0
        Prmx = 0.0
        Pptmix = 0
        Newsnow = 0
        Tmax = 0.0
        Tmin = 0.0
      ENDIF

! NEW PARAMETERS

      IF ( getparam('precip', 'maxmon_prec', MAXMO, 'real', Maxmon_prec)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'tmax_allrain', MAXMO, 'real',
     +     Tmax_allrain).NE.0 ) RETURN

      IF ( getparam('precip', 'tmax_allsnow', 1, 'real', Tmax_allsnow)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'adjmix_rain', MAXMO, 'real', Adjmix_rain)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'rain_mon', Nhru*MAXMO, 'real', Rain_mon)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'snow_mon', Nhru*MAXMO, 'real', Snow_mon)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'psta_mon', Nrain*MAXMO, 'real', Psta_mon)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'psta_xlong', Nrain, 'real', Psta_xlong)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'psta_ylat', Nrain, 'real', Psta_ylat)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'hru_xlong', Nhru, 'real', Hru_xlong)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'hru_ylat', Nhru, 'real', Hru_ylat)
     +     .NE.0 ) RETURN

! END NEW

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'temp_units', 1, 'integer', Temp_units)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'precip_units', 1, 'integer',
     +     Precip_units).NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

! CALCULATE DISTANCE FROM EACH HRU TO EACH NRAIN GAGE,
! AS AN INVERSE FUNCTION, THEN SQUARE IT

      DO  jj = 1, Active_hrus
        i = Hru_route_order(jj)
        DO k = 1, Nrain
          distx = (Hru_xlong(i)-Psta_xlong(k))**2
          disty = (Hru_ylat(i )-Psta_ylat(k))**2 
          dist = 1./SQRT(distx+disty)
          Dist2(i, k) = dist*dist
          DO j = 1, MAXMO
            IF ( Psta_mon(k,j).LT.0.01 ) Psta_mon(k, j) = 0.01
          ENDDO
        ENDDO
      ENDDO

!dbg  IF ( get var('basin', 'prt_debug', 1, 'integer', Prt_debug)
!dbg +     .NE.0 ) RETURN

!dbg  IF ( Prt_debug.EQ.1 ) WRITE (94, 9001)

      pptdist2init = 0

!dbg 9001 FORMAT ('    Date     Water Bal   Precip    Rain     Snow')

      END FUNCTION pptdist2init

!***********************************************************************
!     pptdist2run - Computes precipitation form (rain, snow or mix) and
!                   depth for each HRU, and basin weighted avg. precip
!***********************************************************************
      INTEGER FUNCTION pptdist2run()
      USE PRMS_DIST2_PRECIP
      IMPLICIT NONE
      INTRINSIC ABS
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, mo, iform, nowtime(6), k, j
      REAL :: sum_obs, ppt, pcor, sumdist, sump, prec
!dbg  REAL :: ppt_bal
!***********************************************************************
      pptdist2run = 1

      IF ( getvar('obs', 'precip', Nrain, 'real', Precip).NE.0 ) RETURN

      IF ( Precip_units.EQ.1 ) THEN
        DO i = 1, Nrain
          Precip(i) = Precip(i)/INCH2MM
        ENDDO
      ENDIF

      IF ( Nform.GT.0 ) THEN
        IF ( getvar('obs', 'form_data', Nform, 'integer', Form_data)
     +       .NE.0 ) RETURN
        iform = Form_data(1)
      ELSE
        iform = 0
      ENDIF

      IF ( getvar('temp', 'solrad_tmax', 1, 'real', Solrad_tmax)
     +     .NE.0 ) RETURN

      CALL dattim('now', nowtime)

      IF ( Solrad_tmax.LT.-50.00 ) THEN
        PRINT *, 'Bad temperature data, using previous time step values'
     +           , Solrad_tmax, nowtime
! load Tmax and Tmin with appropriate observed values
      ELSEIF ( Temp_units.EQ.0 ) THEN
        IF ( getvar('temp', 'tmaxf', Nhru, 'real', Tmax).NE.0 ) RETURN
        IF ( getvar('temp', 'tminf', Nhru, 'real', Tmin).NE.0 ) RETURN
      ELSE
        IF ( getvar('temp', 'tmaxc', Nhru, 'real', Tmax).NE.0 ) RETURN
        IF ( getvar('temp', 'tminc', Nhru, 'real', Tmin).NE.0 ) RETURN
      ENDIF

      mo = nowtime(2)
      Basin_ppt = 0.0
      Basin_rain = 0.0
      Basin_snow = 0.0

      !rsr, zero precip arrays
!     Istack = 0
      Pptmix = 0
      Hru_ppt = 0.0
      Hru_rain = 0.0
      Hru_snow = 0.0
      Newsnow = 0
      Prmx = 0.0

      sum_obs = 0.0

      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        ppt = 0.0
        sumdist = 0.0
        sump = 0.0

! Determine if any precip on HRU=i, if not start next HRU

        DO k = 1, Nrain

!   Make sure stations precip is not negative or missing

!???rsr, pcor should only be used for portion of precip that is rain
          IF ( Precip(k).GT.0.0 .AND.
     +         Precip(k).LT.Maxmon_prec(mo) ) THEN
            pcor = Rain_mon(i, mo)/Psta_mon(k, mo)
            sumdist = sumdist + Dist2(i,k)
            prec = Precip(k)*pcor
            sump = sump + prec
            ppt = ppt + prec*Dist2(i, k)
!         ELSEIF ( Precip(k).LT.0.0 ) THEN
!           IF ( Istack(k).EQ.0 ) THEN
!             PRINT 9002, Precip(k), k, nowtime
!             Istack(k) = 1
!           ENDIF
          ENDIF
        ENDDO

        IF ( sump.LT.0.01 ) THEN
          ppt = 0.0
        ELSE
          ppt = ppt/sumdist
        ENDIF

!******Zero precipitation on HRU

        IF ( ppt.LT.NEARZERO ) CYCLE

        sum_obs = sum_obs + ppt*Hru_area(i)

!******If observed temperature data are not available or if observed
!******form data are available and rain is explicitly specified then
!******precipitation is all rain.
! MODIFIED BELOW (10/99, JJV SO THAT ASSUMING ALWAYS TEMP DATA FOR A DAY
! FOR AT LEAST ONE SITE

        IF ( Solrad_tmax.LT.-50.0 .OR. Solrad_tmax.GT.150.0 .OR.
     +           iform.EQ.2 ) THEN
          IF ( (Solrad_tmax.GT.-998.AND.Solrad_tmax.LT.-50.0) .OR.
     +          Solrad_tmax.GT.150.0 ) PRINT *,
     +          'Warning, bad solrad_tmax', Solrad_tmax, nowtime
          Hru_ppt(i) = ppt
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******If form data are available and snow is explicitly specified or if
!******maximum temperature is below or equal to the base temperature for
!******snow then precipitation is all snow

        ELSEIF ( iform.EQ.1 .OR. Tmax(i).LE.Tmax_allsnow ) THEN
          ppt = 0.0
          sumdist = 0.0
          sump = 0.0
          DO k = 1, Nrain

!   Make sure stations precip is not negative or missing or bad
!   bad is defined as any daily value > maxmon_prec (max monthly precip)

            IF ( Precip(k).GT.-NEARZERO .AND. 
     +           Precip(k).LT.Maxmon_prec(mo) ) THEN

              pcor = Snow_mon(i, mo)/Psta_mon(k, mo)
              sumdist = sumdist + Dist2(i, k)
              prec = Precip(k)*pcor
              sump = sump + prec
              ppt = ppt + prec*Dist2(i, k)
            ENDIF
          ENDDO

          IF ( sump.LT.0.01 ) THEN
            ppt = 0.0
          ELSE
            ppt = ppt/sumdist
          ENDIF

          Hru_ppt(i) = ppt
          Hru_snow(i) = Hru_ppt(i)
          Newsnow(i) = 1

!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain

        ELSEIF ( Tmin(i).GT.Tmax_allsnow .OR.
     +           Tmax(i).GE.Tmax_allrain(mo) ) THEN
          Hru_ppt(i) = ppt
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******Otherwise precipitation is a mixture of rain and snow

        ELSE
          Prmx(i) = ((Tmax(i)-Tmax_allsnow)/(Tmax(i)-Tmin(i)))*
     +              Adjmix_rain(mo)

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain

          IF ( Prmx(i).GE.1.0 ) THEN  !rsr changed > to GE 1/8/2006
            Hru_ppt(i) = ppt
            Hru_rain(i) = Hru_ppt(i)

!******If not, it is a rain/snow mixture

          ELSE
            Pptmix(i) = 1
            Hru_ppt(i) = ppt
            Hru_rain(i) = Prmx(i)*Hru_ppt(i)
            Hru_snow(i) = Hru_ppt(i) - Hru_rain(i)
            Newsnow(i) = 1
          ENDIF
        ENDIF

        Basin_ppt = Basin_ppt + Hru_ppt(i)*Hru_area(i)
        Basin_rain = Basin_rain + Hru_rain(i)*Hru_area(i)
        Basin_snow = Basin_snow + Hru_snow(i)*Hru_area(i)

      ENDDO
      Basin_ppt = Basin_ppt*Basin_area_inv
      Basin_obs_ppt = sum_obs*Basin_area_inv

      Basin_rain = Basin_rain*Basin_area_inv
      Basin_snow = Basin_snow*Basin_area_inv

!dbg  IF ( Prt_debug.EQ.1 ) THEN
!dbg    ppt_bal = Basin_ppt - Basin_rain - Basin_snow
!dbg    IF ( ABS(ppt_bal).GT.1.0E-5 ) THEN
!dbg      WRITE (94, *) 'possible water balance error'
!dbg    ELSEIF ( ABS(ppt_bal).GT.5.0E-7 ) THEN
!dbg      WRITE (94, *) 'precip rounding issue', ppt_bal, nowtime
!dbg    ENDIF
!dbg    WRITE (94, 9001) nowtime(1), mo, nowtime(3), ppt_bal, Basin_ppt,
!dbg +                   Basin_rain, Basin_snow
!dbg  ENDIF

      pptdist2run = 0

!dbg 9001 FORMAT (I5, 2('/', I2), F11.5, 3F9.5)
!9002 FORMAT ('Warning, bad precipitation value:', F10.3,
!    +        '; precip station:', I3, '; Time:', I5, 2('/', I2.2), I3,
!    +        2(':', I2.2), '; value set to 0.0')

      END FUNCTION pptdist2run

!***********************************************************************
! Determines the form of precipitation and distributes it to each HRU
! using an inverse distance weighting scheme
!
! Needs variable "precip" in the DATA FILE
! Needs observed variable tmax read in the temperature module
!
! MODIFIED 7/1998 ,JJ VACCARO, INTERPOLATES PRECIP BASED ON DISTANCE
!   SQUARED USING ALL STATIONS IDENTIFIED FOR BASIN, MAY GIVE MORE
! PRECIP IN LOWLANDS, BUT SMOOTHS OUT DISTRIBUTION, USES MONTHLY MEANS
! FOR 61-90 NORMAL PERIOD OF HRUS AND STATIONS FOR ADJUSTMENT
! RSR, added adjust to rain and snow, rather than using rain adjust to
!      adjust total precip
!***********************************************************************
      MODULE PRMS_DIST2_PRECIP
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: N_psta(:), Nuse_psta(:, :)
      REAL, SAVE, ALLOCATABLE :: Tmax_hru(:), Tmin_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dist2(:, :)
!   Declared Parameters
      INTEGER, SAVE :: Max_psta
      REAL, SAVE :: Dist_max, Maxday_prec
      REAL, SAVE, ALLOCATABLE :: Rain_mon(:, :), Snow_mon(:, :)
      REAL, SAVE, ALLOCATABLE :: Psta_mon(:, :)
      REAL, SAVE, ALLOCATABLE :: Psta_xlong(:), Psta_ylat(:)
      REAL, SAVE, ALLOCATABLE :: Hru_ylat(:), Hru_xlong(:)
!      REAL, SAVE, ALLOCATABLE :: Maxmon_prec(:)
      END MODULE PRMS_DIST2_PRECIP

!***********************************************************************
!     Main precip routine
!***********************************************************************
      INTEGER FUNCTION precip_dist2_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: pptdist2decl, pptdist2init, pptdist2run
!***********************************************************************
      precip_dist2_prms = 0

      IF ( Process_flag==0 ) THEN
        precip_dist2_prms = pptdist2run()
      ELSEIF ( Process_flag==1 ) THEN
        precip_dist2_prms = pptdist2decl()
      ELSEIF ( Process_flag==2 ) THEN
        precip_dist2_prms = pptdist2init()
      ENDIF

      END FUNCTION precip_dist2_prms

!***********************************************************************
!     pptdist2decl - set up parameters for precipitation computations
!   Declared Parameters
!     tmax_allrain, tmax_allsnow, adjmix_rain
!     rain_mon, snow_mon, precip_units
!     hru_area, temp_units, maxmon_prec, dist2, psta_xlong, psta_ylong
!     hru_ylat, hru_xlong, max_psta, dist_max, maxday_prec
!***********************************************************************
      INTEGER FUNCTION pptdist2decl()
      USE PRMS_DIST2_PRECIP
      USE PRMS_MODULE, ONLY: Model
      USE PRMS_BASIN, ONLY: Nhru
      USE PRMS_CLIMATEVARS, ONLY: Nrain
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      pptdist2decl = 1

      IF ( declmodule(
     +'$Id: precip_dist2_prms.f 2439 2011-02-11 20:36:52Z rsregan $'
     +).NE.0 ) RETURN

      IF ( Nrain.LT.2 .AND. Model/=99 ) THEN
        PRINT *, 'precip_dist2 requires at least 2 precip stations'
        RETURN
      ENDIF

! declare parameters
      IF ( declparam('precip', 'dist_max', 'one', 'real',
     +     '1e+09', '0.0', '1e+09',
     +     'Maximum distance from HRU to include a climate station',
     +     'Maximum distance from HRU to include a climate station',
     +     'elev_units').NE.0 ) RETURN

      IF ( declparam('precip', 'max_psta', 'one', 'integer',
     +     '50', '2', '50',
     +     'Maximum number of precip stations to distribute to an HRU',
     +     'Maximum number of precip stations to distribute to an HRU',
     +     'none').NE.0 ) RETURN

      IF ( declparam('precip', 'maxday_prec', 'one', 'real',
     +     '15.', '0.', '20.',
     +     'Maximum daily precipitation for any weather site',
     +     'If measured monthly precipitation is > maxday_prec value,'//
     +     ' precipitation is assumed to be in error',
     +     'inches').NE.0 ) RETURN

!      ALLOCATE (Maxmon_prec(12))
!      IF ( decl param('precip', 'maxmon_prec', 'nmonths', 'real',
!     +     '5.', '0.', '15.',
!     +     'Maximum monthly precipitation for any weather site',
!     +     'If measured monthly precipitation is > maxmon_prec value,'//
!     +     ' precipitation is assumed to be in error',
!     +     'inches').NE.0 ) RETURN

      ALLOCATE (Rain_mon(Nhru, 12))
      IF ( declparam('precip', 'rain_mon', 'nhru,nmonths', 'real',
     +     '1.0', '0.0', '50.0',
     +     'Rain adjustment factor, by month for each HRU',
     +     'Monthly precipitation to adjust measured precipitation on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Snow_mon(Nhru, 12))
      IF ( declparam('precip', 'snow_mon', 'nhru,nmonths', 'real',
     +     '1.0', '0.0', '50.0',
     +     'Snow adjustment factor, by month for each HRU',
     +     'Monthly precipitation to adjust snow on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Psta_mon(Nrain, 12))
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

      pptdist2decl = 0
      END FUNCTION pptdist2decl

!***********************************************************************
!     pptdist2init - Initialize precip module - get parameter values
!***********************************************************************
      INTEGER FUNCTION pptdist2init()
      USE PRMS_DIST2_PRECIP
      USE PRMS_BASIN, ONLY: Nhru
!dbg  USE PRMS_BASIN, ONLY: Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Nrain
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      INTRINSIC SQRT
! Local Variables
      INTEGER :: i, k, j, n, kk, kkbig
      REAL :: distx, disty, distance, big_dist, dist
      REAL, ALLOCATABLE :: nuse_psta_dist(:, :)
!***********************************************************************
      pptdist2init = 1

      ALLOCATE ( Tmax_hru(Nhru), Tmin_hru(Nhru) )

! NEW PARAMETERS
      IF ( getparam('precip', 'maxday_prec', 1, 'real', Maxday_prec)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'dist_max', 1, 'real', Dist_max)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'max_psta', 1, 'real', Max_psta)
     +     .NE.0 ) RETURN
      IF ( Max_psta==50 ) Max_psta = Nrain

!      IF ( get param('precip', 'maxmon_prec', 12, 'real', Maxmon_prec)
!     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'rain_mon', Nhru*12, 'real', Rain_mon)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'snow_mon', Nhru*12, 'real', Snow_mon)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'psta_mon', Nrain*12, 'real', Psta_mon)
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

! CALCULATE DISTANCE FROM EACH HRU TO EACH NRAIN GAGE,
! AS AN INVERSE FUNCTION, THEN SQUARE IT

      ALLOCATE ( N_psta(Nhru), Dist2(Nhru, Nrain) )
      N_psta = 0
      ALLOCATE (Nuse_psta(Max_psta,Nhru), nuse_psta_dist(Max_psta,Nhru))
      Nuse_psta = 0
      nuse_psta_dist = 0.0

      DO  i = 1, Nhru
        DO k = 1, Nrain
          distx = (Hru_xlong(i)-Psta_xlong(k))**2
          disty = (Hru_ylat(i )-Psta_ylat(k))**2 
          distance = SQRT(distx+disty)
          dist = 1.0/(distance/5280.0)
          Dist2(i, k) = dist*dist
          IF ( distance<Dist_max ) THEN
            n = N_psta(i)
            IF ( n<Max_psta ) THEN
              n = n + 1
              Nuse_psta(n, i) = k
              nuse_psta_dist(n, i) = distance
              N_psta(i) = n
            ELSE ! have max_psta, don't use biggest distance
              big_dist = 0.0
              kkbig = 1
              DO kk = 1, Max_psta
                IF ( big_dist<nuse_psta_dist(kk,i) ) THEN
                  big_dist = nuse_psta_dist(kk, i)
                  kkbig = kk
                ENDIF
              ENDDO
              IF ( distance<big_dist ) THEN ! if equal use first one
                Nuse_psta(kkbig, i) = k
                nuse_psta_dist(kkbig, i) = distance
              ENDIF
            ENDIF
          ENDIF

          DO j = 1, 12
            IF ( Psta_mon(k,j).LT.0.01 ) Psta_mon(k, j) = 0.01
          ENDDO
        ENDDO
      ENDDO

!dbg  IF ( Print_debug.EQ.1 ) WRITE (94, 9001)

      DEALLOCATE (nuse_psta_dist)

      pptdist2init = 0

!dbg 9001 FORMAT ('    Date     Water Bal   Precip    Rain     Snow')

      END FUNCTION pptdist2init

!***********************************************************************
!     pptdist2run - Computes precipitation form (rain, snow or mix) and
!                   depth for each HRU, and basin weighted avg. precip
!***********************************************************************
      INTEGER FUNCTION pptdist2run()
      USE PRMS_DIST2_PRECIP
      USE PRMS_BASIN, ONLY: Nhru, Active_hrus, Hru_route_order,
     +    Hru_area, Basin_area_inv, NEARZERO, INCH2MM
!dbg  USE PRMS_BASIN, ONLY: Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Prmx, Basin_ppt,
     +    Basin_rain, Basin_snow, Hru_ppt, Hru_rain, Hru_snow,
     +    Basin_obs_ppt, Temp_units, Tmaxf, Tminf, Tmaxc, Nrain,
     +    Tminc, Precip_units, Tmax_allsnow, Tmax_allrain, Adjmix_rain
      USE PRMS_OBS, ONLY: Precip, Nowtime, Nowmonth
      IMPLICIT NONE
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, iform, k, j, kk, allmissing
      REAL :: sum_obs, ppt, pcor, sumdist, sump, prec, tdiff
!dbg  REAL :: ppt_bal
!***********************************************************************
      IF ( Precip_units.EQ.1 ) THEN
        DO i = 1, Nrain
          Precip(i) = Precip(i)/INCH2MM
        ENDDO
      ENDIF

! load Tmax and Tmin with appropriate measured values
      IF ( Temp_units.EQ.0 ) THEN
        Tmax_hru = Tmaxf
        Tmin_hru = Tminf
      ELSE
        Tmax_hru = Tmaxc
        Tmin_hru = Tminc
      ENDIF

      Basin_ppt = 0.0
      Basin_rain = 0.0
      Basin_snow = 0.0
      sum_obs = 0.0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        !rsr, zero precip arrays
        Pptmix(i) = 0
        Hru_ppt(i) = 0.0
        Hru_rain(i) = 0.0
        Hru_snow(i) = 0.0
        Newsnow(i) = 0
        Prmx(i) = 0.0

        ! rsr, determine form of precip

!******IF maximum temperature is below or equal to the base temperature
!******for snow then precipitation is all snow
        IF ( Tmax_hru(i)<=Tmax_allsnow ) THEN
          !rsr, precip is all snow
          iform = 1

!******If measured temperature data are not available then
!******precipitation is all rain.
!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain
! MODIFIED BELOW (10/99, JJV SO THAT ASSUMING ALWAYS TEMP DATA FOR A DAY
! FOR AT LEAST ONE SITE
        ELSEIF ( Tmin_hru(i)>Tmax_allsnow .OR.
     +           Tmax_hru(i)>=Tmax_allrain(Nowmonth) ) THEN
          iform = 2
!******Otherwise precipitation is a mixture of rain and snow
        ELSE
          iform = 0
        ENDIF

! Determine if any precip on HRU=i, if not start next HRU

        ppt = 0.0
        sumdist = 0.0
        sump = 0.0
        allmissing = 0
        DO kk = 1, N_psta(i)
          k = Nuse_psta(kk, i)

!   Make sure stations precip is not negative or missing

!???rsr, pcor should only be used for portion of precip that is rain
          IF ( Precip(k)>-NEARZERO .AND. Precip(k)<Maxday_prec ) THEN
!     +         Precip(k).LT.Maxmon_prec(Nowmonth) ) THEN
            allmissing = 1
            !rsr, if all rain use rain adjustment
            IF ( iform==2 ) THEN
              pcor = Rain_mon(i, Nowmonth)/Psta_mon(k, Nowmonth)
            !rsr, if all snow or mixed use snow adjustment
!           ELSEIF ( iform==1 .OR. iform==0 ) THEN
            ELSE
              pcor = Snow_mon(i, Nowmonth)/Psta_mon(k, Nowmonth)
            ENDIF
            sumdist = sumdist + Dist2(i,k)
            prec = Precip(k)*pcor
            sump = sump + prec
            ppt = ppt + prec*Dist2(i, k)
!          ELSE
!            PRINT *, 'bad precip value, HRU:', k, Precip(k), Nowtime
          ENDIF
        ENDDO
        IF ( allmissing==0 ) THEN
          PRINT *,'Error, all precipitation stations have missing data',
     +            Nowtime
          STOP
        ENDIF

        IF ( sump.LT.NEARZERO ) CYCLE

        IF ( sumdist>0.0 ) ppt = ppt/sumdist

        Hru_ppt(i) = ppt
        sum_obs = sum_obs + ppt*Hru_area(i)

        IF ( iform==2 ) THEN
          Hru_rain(i) = ppt
          Prmx(i) = 1.0

        ELSEIF ( iform==1 ) THEN
          Hru_snow(i) = ppt
          Newsnow(i) = 1

       !rsr, precip is a mixture of rain and snow
        ELSE
          tdiff = Tmax_hru(i) - Tmin_hru(i)
          IF ( ABS(tdiff)<NEARZERO ) tdiff = NEARZERO
          Prmx(i) = ((Tmax_hru(i)-Tmax_allsnow)
     +              /tdiff)*Adjmix_rain(Nowmonth)

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain

          IF ( Prmx(i).GE.1.0 ) THEN  !rsr changed > to GE 1/8/2006
            Hru_rain(i) = ppt
            Prmx(i) = 1.0

!******If not, it is a rain/snow mixture

          ELSE
            !sanity check
            IF ( Prmx(i)<0.0 )
     +           PRINT *, 'mixed precip problem', i, Prmx(i), Nowtime
            Pptmix(i) = 1
            Hru_rain(i) = Prmx(i)*ppt
            Hru_snow(i) = ppt - Hru_rain(i)
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

!dbg  IF ( Print_debug.EQ.1 ) THEN
!dbg    ppt_bal = Basin_ppt - Basin_rain - Basin_snow
!dbg    IF ( ABS(ppt_bal).GT.1.0E-5 ) THEN
!dbg      WRITE (94, *) 'possible water balance error'
!dbg    ELSEIF ( ABS(ppt_bal).GT.5.0E-7 ) THEN
!dbg      WRITE (94, *) 'precip rounding issue', ppt_bal, Nowtime
!dbg    ENDIF
!dbg    WRITE (94, 9001) Nowtime(1), Nowmonth, Nowtime(3), ppt_bal, Basin_ppt,
!dbg +                   Basin_rain, Basin_snow
!dbg  ENDIF

      pptdist2run = 0

!dbg 9001 FORMAT (I5, 2('/', I2), F11.5, 3F9.5)

      END FUNCTION pptdist2run

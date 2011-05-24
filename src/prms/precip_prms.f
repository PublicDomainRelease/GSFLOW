!***********************************************************************
! Determines the form of precipitation and distributes it from one or
! more stations to each HRU using monthly correction factors to account
! for differences in altitude, spatial variation, topography, and
! measurement gage efficiency
!
! Needs variable "precip" in the DATA FILE
! Needs measured variable tmax read in the temperature module
!***********************************************************************
      MODULE PRMS_PRECIP
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: Istack(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_psta(:)
      REAL, SAVE, ALLOCATABLE :: Rain_adj(:, :), Snow_adj(:, :)
      END MODULE PRMS_PRECIP

!***********************************************************************
!     Main precip routine
!***********************************************************************
      INTEGER FUNCTION precip_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: pptdecl, pptinit, pptrun
!***********************************************************************
      precip_prms = 0

      IF ( Process_flag==0 ) THEN
        precip_prms = pptrun()
      ELSEIF ( Process_flag==1 ) THEN
        precip_prms = pptdecl()
      ELSEIF ( Process_flag==2 ) THEN
        precip_prms = pptinit()
      ENDIF

      END FUNCTION precip_prms

!***********************************************************************
!     pptdecl - set up parameters for precipitation computations
!   Declared Parameters
!     tmax_allrain, tmax_allsnow, hru_psta, adjmix_rain
!     rain_adj, snow_adj, precip_units, hru_area
!***********************************************************************
      INTEGER FUNCTION pptdecl()
      USE PRMS_PRECIP
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      pptdecl = 1

      IF ( declmodule(
     +'$Id: precip_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

! declare parameters
      ALLOCATE (Hru_psta(Nhru))
      IF ( declparam('precip', 'hru_psta', 'nhru', 'integer',
     +     '1', 'bounded', 'nrain',
     +     'Index of base precipitation station for HRU',
     +     'Index of the base precipitation station used for lapse'//
     +     ' rate calculations for each HRU.',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Rain_adj(Nhru, 12))
      IF ( declparam('precip', 'rain_adj', 'nhru,nmonths', 'real',
     +     '1.0', '0.2', '5.0',
     +     'Rain adjustment factor, by month for each HRU',
     +     'Monthly factor to adjust measured precipitation on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Snow_adj(Nhru, 12))
      IF ( declparam('precip', 'snow_adj', 'nhru,nmonths', 'real',
     +     '1.0', '0.2', '5.0',
     +     'Snow adjustment factor, by month for each HRU',
     +     'Monthly factor to adjust measured precipitation on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'decimal fraction').NE.0 ) RETURN

      pptdecl = 0
      END FUNCTION pptdecl

!***********************************************************************
!     pptinit - Initialize precip module - get parameter values
!***********************************************************************
      INTEGER FUNCTION pptinit()
      USE PRMS_PRECIP
      USE PRMS_BASIN, ONLY: Nhru
!dbg  USE PRMS_BASIN, ONLY: Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Nrain
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i
!***********************************************************************
      pptinit = 1

      ALLOCATE ( Istack(Nrain) )

      IF ( getparam('precip', 'hru_psta', Nhru, 'integer', Hru_psta)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'rain_adj', Nhru*12, 'real', Rain_adj)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'snow_adj', Nhru*12, 'real', Snow_adj)
     +     .NE.0 ) RETURN

!dbg  IF ( Print_debug.EQ.1 ) WRITE (94, 9001)

      DO i = 1, Nhru
        IF ( Hru_psta(i)<1 .OR. Hru_psta(i)>Nrain ) Hru_psta(i) = 1
      ENDDO

      pptinit = 0

!dbg 9001 FORMAT ('    Date     Water Bal   Precip    Rain     Snow')

      END FUNCTION pptinit

!***********************************************************************
!     pptrun - Computes precipitation form (rain, snow or mix) and
!              depth for each HRU, and basin weighted avg. precip
!***********************************************************************
      INTEGER FUNCTION pptrun()
      USE PRMS_PRECIP
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_area, Hru_route_order,
     +    Basin_area_inv, NEARZERO, INCH2MM
!dbg  USE PRMS_BASIN, ONLY: Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Precip_units,
     +    Solrad_tmax, Tmaxf, Tminf, Tmax_allsnow_f, Basin_ppt,
     +    Basin_rain, Basin_snow, Hru_ppt, Hru_rain, Hru_snow,
     +    Tmax_allrain_f, Basin_obs_ppt, Prmx, Adjmix_rain, Nrain
      USE PRMS_OBS, ONLY: Form_data, Precip, Nform, Nowtime, Nowmonth
      IMPLICIT NONE
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, ip, ii, iform
      REAL :: sum_obs, ppt, pcor, tdiff
!dbg  REAL :: ppt_bal
!***********************************************************************
      IF ( Precip_units.EQ.1 ) THEN
        DO i = 1, Nrain
          Precip(i) = Precip(i)/INCH2MM
        ENDDO
      ENDIF

      iform = 0
      IF ( Nform>0 ) iform = Form_data(1)

      IF ( Solrad_tmax.LT.-50.00 ) PRINT *,
     +     'Bad temperature data, using previous time step values',
     +     Solrad_tmax, Nowtime

      Basin_ppt = 0.0
      Basin_rain = 0.0
      Basin_snow = 0.0

      Istack = 0

      sum_obs = 0.0

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)

!******Zero precipitation on HRU

        Pptmix(i) = 0
        Hru_ppt(i) = 0.0
        Hru_rain(i) = 0.0
        Hru_snow(i) = 0.0
        Newsnow(i) = 0
        Prmx(i) = 0.0
        ip = Hru_psta(i)
        ppt = Precip(ip)
        IF ( ppt.LT.0.0 ) THEN
          IF ( Istack(ip).EQ.0 ) THEN
            PRINT 9002, ppt, ip, Nowtime
            Istack(ip) = 1
          ENDIF
          CYCLE
        ENDIF

        ! ignore very small amounts of precipitation
        IF ( ppt.LT.NEARZERO ) CYCLE

        sum_obs = sum_obs + ppt*Hru_area(i)

!******If observed temperature data are not available or if observed
!******form data are available and rain is explicitly specified then
!******precipitation is all rain.

        IF ( Solrad_tmax.LT.-50.0 .OR. Solrad_tmax.GT.150.0 .OR.
     +       iform.EQ.2 ) THEN
          IF ( (Solrad_tmax.GT.-998.AND.Solrad_tmax.LT.-50.0) .OR.
     +          Solrad_tmax.GT.150.0 ) PRINT *,
     +          'Warning, bad solrad_tmax', Solrad_tmax, Nowtime
          pcor = Rain_adj(i, Nowmonth)
          Hru_ppt(i) = ppt*pcor
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******If form data are available and snow is explicitly specified or if
!******maximum temperature is below or equal to the base temperature for
!******snow then precipitation is all snow

        ELSEIF ( iform.EQ.1 .OR. Tmaxf(i).LE.Tmax_allsnow_f ) THEN
          pcor = Snow_adj(i, Nowmonth)
          Hru_ppt(i) = ppt*pcor
          Hru_snow(i) = Hru_ppt(i)
          Newsnow(i) = 1

!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain

        ELSEIF ( Tminf(i).GT.Tmax_allsnow_f .OR.
     +           Tmaxf(i).GE.Tmax_allrain_f(Nowmonth) ) THEN
          pcor = Rain_adj(i, Nowmonth)
          Hru_ppt(i) = ppt*pcor
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******Otherwise precipitation is a mixture of rain and snow

        ELSE
          tdiff = Tmaxf(i) - Tminf(i)
          IF ( ABS(tdiff)<NEARZERO ) tdiff = 0.01
          Prmx(i) = ((Tmaxf(i)-Tmax_allsnow_f)
     +              /tdiff)*Adjmix_rain(Nowmonth)

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain

          IF ( Prmx(i).GE.1.0 ) THEN  !rsr changed > to GE 1/8/2006
            pcor = Rain_adj(i, Nowmonth)
            Hru_ppt(i) = ppt*pcor
            Hru_rain(i) = Hru_ppt(i)
            Prmx(i) = 1.0

!******If not, it is a rain/snow mixture

          ELSE
            pcor = Snow_adj(i, Nowmonth)
            Pptmix(i) = 1
            Hru_ppt(i) = ppt*pcor
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

      pptrun = 0

!dbg 9001 FORMAT (I5, 2('/', I2), F11.5, 3F9.5)
 9002 FORMAT ('Warning, bad precipitation value:', F10.3,
     +        '; precip station:', I3, '; Time:', I5, 2('/', I2.2), I3,
     +        2(':', I2.2), '; value set to 0.0')

      END FUNCTION pptrun

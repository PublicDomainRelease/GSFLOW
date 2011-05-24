!***********************************************************************
! Determines the form of precipitation and distributes it to each HRU
! using monthly lapse rates
!
! Needs variable "precip" in the DATA FILE
! Needs measured variable tmax read in the temperature module
!***********************************************************************
      MODULE PRMS_LAPS_PRECIP
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: Istack(:)
      REAL, SAVE, ALLOCATABLE :: Rain_adj_lapse(:, :)
      REAL, SAVE, ALLOCATABLE :: Snow_adj_lapse(:, :)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_psta(:), Hru_plaps(:)
      REAL, SAVE, ALLOCATABLE :: Padj_rn(:, :), Padj_sn(:, :)
      REAL, SAVE, ALLOCATABLE :: Pmn_mo(:, :)
      END MODULE PRMS_LAPS_PRECIP

!***********************************************************************
!     Main precip routine
!***********************************************************************
      INTEGER FUNCTION precip_laps_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: pptlapsdecl, pptlapsinit, pptlapsrun
!***********************************************************************
      precip_laps_prms = 0

      IF ( Process_flag==0 ) THEN
        precip_laps_prms = pptlapsrun()
      ELSEIF ( Process_flag==1 ) THEN
        precip_laps_prms = pptlapsdecl()
      ELSEIF ( Process_flag==2 ) THEN
        precip_laps_prms = pptlapsinit()
      ENDIF

      END FUNCTION precip_laps_prms

!***********************************************************************
!     pptlapsdecl - set up parameters for precipitation computations
!   Declared Parameters
!     tmax_allrain, tmax_allsnow, hru_psta, adjmix_rain
!     padj_rn, padj_sn, precip_units, hru_area
!     hru_plaps, psta_elev, pmn_mo, hru_elev
!***********************************************************************
      INTEGER FUNCTION pptlapsdecl()
      USE PRMS_LAPS_PRECIP
      USE PRMS_MODULE, ONLY: Model
      USE PRMS_BASIN, ONLY: Nhru
      USE PRMS_CLIMATEVARS, ONLY: Nrain
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      pptlapsdecl = 1

      IF ( declmodule(
     +'$Id: precip_laps_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

! declare parameters
      ALLOCATE (Hru_psta(Nhru))
      IF ( declparam('precip', 'hru_psta', 'nhru', 'integer',
     +     '1', 'bounded', 'nrain',
     +     'Index of base precipitation station for HRU',
     +     'Index of the base precipitation station used for lapse'//
     +     ' rate calculations for each HRU.',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Padj_rn(Nrain, 12))
      IF ( declparam('precip', 'padj_rn', 'nrain,nmonths', 'real',
     +     '1.0', '-2.0', '10.0',
     +     'Rain adjustment factor, by month for each precip station',
     +     'Monthly factor to adjust precipitation lapse rate'//
     +     ' computed between station psta and station plaps.'//
     +     ' Positive factors are mutiplied times the lapse rate and'//
     +     ' negative factors are made positive and substituted for'//
     +     ' the computed lapse rate.',
     +     'inches/day').NE.0 ) RETURN

      ALLOCATE (Padj_sn(Nrain, 12))
      IF ( declparam('precip', 'padj_sn', 'nrain,nmonths', 'real',
     +     '1.0', '-2.0', '10.0',
     +     'Snow adjustment factor, by month for each precip station',
     +     'Monthly factor to adjust precipitation lapse rate '//
     +     ' computed between station psta and station plaps.'//
     +     ' Positive factors are mutiplied times the lapse rate and'//
     +     ' negative factors are made positive and substituted for'//
     +     ' the computed lapse rate.',
     +     'inches/day').NE.0 ) RETURN

      ALLOCATE (Pmn_mo(Nrain, 12))
      IF ( declparam('precip', 'pmn_mo', 'nrain,nmonths', 'real',
     +     '1.', '0.', '100.0',
     +     'Mean monthly precipitation for each lapse precip station',
     +     'Mean monthly precipitation for each lapse precip station',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_plaps(Nhru))
      IF ( declparam('precip', 'hru_plaps', 'nhru', 'integer',
     +     '1', 'bounded', 'nrain',
     +     'Index of precipitation station to lapse against hru_psta',
     +     'Index of the lapse precipitation station used for lapse'//
     +     ' rate calculations for each HRU using hru_psta.',
     +     'none').NE.0 ) RETURN

      pptlapsdecl = 0
      END FUNCTION pptlapsdecl

!***********************************************************************
!     pptlapsinit - Initialize precip module - get parameter values
!***********************************************************************
      INTEGER FUNCTION pptlapsinit()
      USE PRMS_LAPS_PRECIP
      USE PRMS_BASIN, ONLY: Nhru, Hru_elev, NEARZERO
!dbg  USE PRMS_BASIN, ONLY: Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Nrain, Psta_elev
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, j, np1, np2
      REAL :: elp_diff, elh_diff, pmo_diff, pmo_rate, adj_p
!***********************************************************************
      pptlapsinit = 1

      ALLOCATE ( Istack(Nrain) )
      ALLOCATE ( Snow_adj_lapse(Nhru, 12), Rain_adj_lapse(Nhru, 12))

      IF ( getparam('precip', 'hru_psta', Nhru, 'integer', Hru_psta)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'padj_rn', Nrain*12, 'real', Padj_rn)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'padj_sn', Nrain*12, 'real', Padj_sn)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'hru_plaps', Nhru, 'integer',
     +     Hru_plaps).NE.0 ) RETURN

      IF ( getparam('precip', 'pmn_mo', Nrain*12, 'real', Pmn_mo)
     +     .NE.0 ) RETURN

      DO i = 1, Nrain
        DO j = 1, 12
          IF ( Pmn_mo(i,j)<NEARZERO ) THEN
            PRINT *, 'Mean monthly precipitation for station:', i,
     +               ' set to 0, needs to be positive, set to 0.00001'
            Pmn_mo(i, j) = 0.00001
          ENDIF
        ENDDO
      ENDDO
            
      DO i = 1, Nhru
        IF ( Hru_psta(i).LT.1 ) Hru_psta(i) = 1
        IF ( Hru_plaps(i).LT.1 ) Hru_plaps(i) = 1
        np1 = Hru_psta(i)
        np2 = Hru_plaps(i)
        elp_diff = Psta_elev(np2) - Psta_elev(np1)
        IF ( ABS(elp_diff)<NEARZERO ) elp_diff = 1.0
        elh_diff = Hru_elev(i) - Psta_elev(np1)
        DO j = 1, 12
          pmo_diff = Pmn_mo(np2,j) - Pmn_mo(np1,j)
          pmo_rate = pmo_diff / elp_diff
          adj_p = (pmo_rate*elh_diff)/Pmn_mo(np1,j)
          IF ( Padj_sn(np1,j).GE.0.0 ) THEN
            Snow_adj_lapse(i,j) = 1.0 + Padj_sn(np1,j)*adj_p
          ELSE 
            Snow_adj_lapse(i,j) = -Padj_sn(np1,j)
          ENDIF

          IF ( Padj_rn(np1,j).GE.0.0 ) THEN
            Rain_adj_lapse(i,j) = 1.0 + Padj_rn(np1,j)*adj_p
          ELSE 
            Rain_adj_lapse(i,j) = -Padj_rn(np1,j)
          ENDIF
        ENDDO
      ENDDO

!dbg  IF ( Print_debug.EQ.1 ) WRITE (94, 9001)

      pptlapsinit = 0

!dbg 9001 FORMAT ('    Date     Water Bal   Precip    Rain     Snow')

      END FUNCTION pptlapsinit

!***********************************************************************
!     pptlapsrun - Computes precipitation form (rain, snow or mix) and
!                  depth for each HRU, and basin weighted avg. precip
!***********************************************************************
      INTEGER FUNCTION pptlapsrun()
      USE PRMS_LAPS_PRECIP
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv, NEARZERO, INCH2MM
!dbg  USE PRMS_BASIN, ONLY: Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Precip_units,
     +    Solrad_tmax, Tmaxf, Tminf, Basin_ppt, Prmx, Basin_rain,
     +    Basin_snow, Hru_ppt, Hru_rain, Hru_snow, Nrain, Adjmix_rain,
     +    Basin_obs_ppt, Tmax_allsnow_f, Tmax_allrain_f
      USE PRMS_OBS, ONLY: Precip, Form_data, Nform, Nowtime, Nowmonth
      IMPLICIT NONE
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, ip, iform, ii
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
          pcor = Rain_adj_lapse(i, Nowmonth)
          Hru_ppt(i) = ppt*pcor
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******If form data are available and snow is explicitly specified or if
!******maximum temperature is below or equal to the base temperature for
!******snow then precipitation is all snow

        ELSEIF ( iform.EQ.1 .OR. Tmaxf(i).LE.Tmax_allsnow_f ) THEN
          pcor = Snow_adj_lapse(i, Nowmonth)
          Hru_ppt(i) = ppt*pcor
          Hru_snow(i) = Hru_ppt(i)
          Newsnow(i) = 1

!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain

        ELSEIF ( Tminf(i).GT.Tmax_allsnow_f .OR.
     +           Tmaxf(i).GE.Tmax_allrain_f(Nowmonth) ) THEN
          pcor = Rain_adj_lapse(i, Nowmonth)
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
            pcor = Rain_adj_lapse(i, Nowmonth)
            Hru_ppt(i) = ppt*pcor
            Hru_rain(i) = Hru_ppt(i)

!******If not, it is a rain/snow mixture

          ELSE
            pcor = Snow_adj_lapse(i, Nowmonth)
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

      pptlapsrun = 0

!dbg 9001 FORMAT (I5, 2('/', I2), F11.5, 3F9.5)
 9002 FORMAT ('Warning, bad precipitation value:', F10.3,
     +        '; precip station:', I3, '; Time:', I5, 2('/', I2.2), I3,
     +        2(':', I2.2), '; value set to 0.0')

      END FUNCTION pptlapsrun

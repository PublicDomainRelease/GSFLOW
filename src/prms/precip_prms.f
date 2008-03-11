!***********************************************************************
! Determine form of precipitation (rain, snow, mix) and distribute
! precipitation to HRU's.
! Needs variable "precip" in the DATA FILE
! Needs observed variable tmax read in the temperature module
!***********************************************************************
      MODULE PRMS_PRECIP
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: INCH2MM = 25.4, NEARZERO = 1.0E-15
      INTEGER :: Nhru, Nrain, Nform
      INTEGER, ALLOCATABLE :: Istack(:)
      REAL, ALLOCATABLE :: Tmax(:), Tmin(:)
!   Declared Variables
      INTEGER, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      REAL :: Basin_ppt, Basin_obs_ppt, Basin_rain, Basin_snow
      REAL, ALLOCATABLE :: Hru_ppt(:), Hru_rain(:), Hru_snow(:), Prmx(:)
!   Declared Variables from other modules - obs
      INTEGER :: Route_on
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
      INTEGER, ALLOCATABLE :: Hru_psta(:)
      REAL :: Tmax_allsnow
      REAL, ALLOCATABLE :: Hru_area(:), Tmax_allrain(:), Adjmix_rain(:)
      REAL, ALLOCATABLE :: Rain_adj(:, :), Snow_adj(:, :)
!     REAL, ALLOCATABLE :: Strain_adj(:, :)
      END MODULE PRMS_PRECIP

!***********************************************************************
!     Main precip routine
!***********************************************************************
      INTEGER FUNCTION precip_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: pptdecl, pptinit, pptrun
!***********************************************************************
      precip_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        precip_prms = pptrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        precip_prms = pptdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        precip_prms = pptinit()
      ENDIF

      END FUNCTION precip_prms

!***********************************************************************
!     pptdecl - set up parameters for precipitation computations
!   Declared Parameters
!     tmax_allrain, tmax_allsnow, hru_psta, adjmix_rain
!     rain_adj, snow_adj, strain_adj, precip_units
!     hru_area, temp_units
!***********************************************************************
      INTEGER FUNCTION pptdecl()
      USE PRMS_PRECIP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      pptdecl = 1

      IF ( declmodule(
     +'$Id: precip_prms.f 3573 2007-09-25 23:07:59Z rsregan $'
     +).NE.0 ) RETURN

      Nrain = getdim('nrain')
      IF ( Nrain.EQ.-1 ) RETURN
      ALLOCATE (Precip(Nrain), Istack(Nrain))

      Nform = getdim('nform')
      IF ( Nform.EQ.-1 ) RETURN
      IF ( Nform.GT.0 ) ALLOCATE (Form_data(Nform))

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

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

      ALLOCATE (Hru_psta(Nhru))
      IF ( declparam('precip', 'hru_psta', 'nhru', 'integer',
     +     '1', 'bounded', 'nrain',
     +     'Index of base precipitation station for HRU',
     +     'Index of the base precipitation station used for lapse'//
     +     ' rate calculattions for each HRU.',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Adjmix_rain(MAXMO))
      IF ( declparam('precip', 'adjmix_rain', 'nmonths', 'real',
     +     '1.', '0.', '3.',
     +     'Adjustment factor for rain in a rain/snow mix',
     +     'Monthly factor to adjust rain proportion in a mixed'//
     +     ' rain/snow event',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Rain_adj(Nhru, MAXMO))
      IF ( declparam('precip', 'rain_adj', 'nhru,nmonths', 'real',
     +     '1.0', '0.2', '5.0',
     +     'Rain adjustment factor, by month for each HRU',
     +     'Monthly factor to adjust measured precipitation on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Snow_adj(Nhru, MAXMO))
      IF ( declparam('precip', 'snow_adj', 'nhru,nmonths', 'real',
     +     '1.0', '0.2', '5.0',
     +     'Snow adjustment factor, by month for each HRU',
     +     'Monthly factor to adjust measured precipitation on'//
     +     ' each HRU to account for differences in elevation, etc',
     +     'decimal fraction').NE.0 ) RETURN

!     ALLOCATE (Strain_adj(Nhru, MAXMO))
!     IF ( declparam('precip', 'strain_adj', 'nhru,nmonths', 'real',
!    +     '1.0', '0.2', '5.0',
!    +     'Storm rain adjustment factor, by month for each HRU',
!    +     'Monthly factor to adjust measured precipitation to'//
!    +     ' each HRU to account for differences in elevation, etc.'//
!    +     ' This factor is for the rain gage used for kinematic or'//
!    +     ' storm routing',
!    +     'decimal fraction').NE.0 ) RETURN

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

      pptdecl = 0
      END FUNCTION pptdecl

!***********************************************************************
!     pptinit - Initialize precip module - get parameter values
!***********************************************************************
      INTEGER FUNCTION pptinit()
      USE PRMS_PRECIP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      pptinit = 1

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

      IF ( getparam('precip', 'tmax_allrain', MAXMO, 'real',
     +     Tmax_allrain).NE.0 ) RETURN

      IF ( getparam('precip', 'tmax_allsnow', 1, 'real', Tmax_allsnow)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'hru_psta', Nhru, 'integer', Hru_psta)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'adjmix_rain', MAXMO, 'real', Adjmix_rain)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'rain_adj', Nhru*MAXMO, 'real', Rain_adj)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'snow_adj', Nhru*MAXMO, 'real', Snow_adj)
     +     .NE.0 ) RETURN

!     IF ( getparam('precip', 'strain_adj', Nhru*MAXMO, 'real',
!    +     Strain_adj).NE.0 ) RETURN

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

!dbg  IF ( get var('basin', 'prt_debug', 1, 'integer', Prt_debug)
!dbg +     .NE.0 ) RETURN

!dbg  IF ( Prt_debug.EQ.1 ) WRITE (94, 9001)

      pptinit = 0

!dbg 9001 FORMAT ('    Date     Water Bal   Precip    Rain     Snow')

      END FUNCTION pptinit

!***********************************************************************
!     pptrun - Computes precipitation form (rain, snow or mix) and
!              depth for each HRU, and basin weighted avg. precip
!***********************************************************************
      INTEGER FUNCTION pptrun()
      USE PRMS_PRECIP
      IMPLICIT NONE
      INTRINSIC ABS
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, ip, mo, iform, nowtime(6), storm, ii
      REAL :: sum_obs, ppt, pcor
!dbg  REAL :: ppt_bal
!***********************************************************************
      pptrun = 1

      IF ( deltim().LT.23.999D0 ) THEN
        storm = 1
      ELSE
        storm = 0
      ENDIF

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

      IF ( getvar('obs', 'route_on', 1, 'integer', Route_on)
     +     .NE.0 ) RETURN

      IF ( getvar('temp', 'solrad_tmax', 1, 'real', Solrad_tmax)
     +     .NE.0 ) RETURN

      CALL dattim('now', nowtime)

      IF ( Solrad_tmax.LT.-50.00 ) THEN
        PRINT *, 'Bad temperature data, using previous time step values'
     +           , Solrad_tmax, nowtime
! load Tmax and Tmin with appropriate observed values
      ELSEIF ( Temp_units.EQ.0 ) THEN
        IF ( storm.EQ.1 ) THEN
          !rsr, warning, tempf needs to be set in temperature module
          IF ( getvar('temp', 'tempf', Nhru, 'real', Tmax)
     +         .NE.0 ) RETURN
          Tmin = Tmax
        ELSE
          IF ( getvar('temp', 'tmaxf', Nhru, 'real', Tmax)
     +         .NE.0 ) RETURN
          IF ( getvar('temp', 'tminf', Nhru, 'real', Tmin)
     +         .NE.0 ) RETURN
        ENDIF
      ELSEIF ( storm.EQ.1 ) THEN
        !rsr, warning, tempc needs to be set in temperature module
        IF ( getvar('temp', 'tempc', Nhru, 'real', Tmax).NE.0 ) RETURN
        Tmin = Tmax
      ELSE
        IF ( getvar('temp', 'tmaxc', Nhru, 'real', Tmax).NE.0 ) RETURN
        IF ( getvar('temp', 'tminc', Nhru, 'real', Tmin).NE.0 ) RETURN
      ENDIF

      mo = nowtime(2)
      Basin_ppt = 0.0
      Basin_rain = 0.0
      Basin_snow = 0.0

      !rsr, zero precip arrays
      Istack = 0
      Pptmix = 0
      Hru_ppt = 0.0
      Hru_rain = 0.0
      Hru_snow = 0.0
      Newsnow = 0
      Prmx = 0.0

      sum_obs = 0.0

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Hru_psta(i).LT.1 ) Hru_psta(i) = 1
        ip = Hru_psta(i)
        ppt = Precip(ip)
        IF ( ppt.LT.0.0 ) THEN
          IF ( Istack(ip).EQ.0 ) THEN
            PRINT 9002, ppt, ip, nowtime
            Istack(ip) = 1
          ENDIF
          ppt = 0.0
        ENDIF

!******Zero precipitation on HRU

        IF ( ppt.LT.NEARZERO ) CYCLE

        sum_obs = sum_obs + ppt*Hru_area(i)

!******If within storm period for kinematic routing, adjust precip
!******by storm adjustment factor

        IF ( Route_on.EQ.1 ) THEN
!         pcor = Strain_adj(i, mo)
          pcor = 1.0
          Hru_ppt(i) = ppt*pcor
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******If observed temperature data are not available or if observed
!******form data are available and rain is explicitly specified then
!******precipitation is all rain.

        ELSEIF ( Solrad_tmax.LT.-50.0 .OR. Solrad_tmax.GT.150.0 .OR.
     +           iform.EQ.2 ) THEN
          IF ( (Solrad_tmax.GT.-998.AND.Solrad_tmax.LT.-50.0) .OR.
     +          Solrad_tmax.GT.150.0 ) PRINT *,
     +          'Warning, bad solrad_tmax', Solrad_tmax, nowtime
          pcor = Rain_adj(i, mo)
          Hru_ppt(i) = ppt*pcor
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******If form data are available and snow is explicitly specified or if
!******maximum temperature is below or equal to the base temperature for
!******snow then precipitation is all snow

        ELSEIF ( iform.EQ.1 .OR. Tmax(i).LE.Tmax_allsnow ) THEN
          pcor = Snow_adj(i, mo)
          Hru_ppt(i) = ppt*pcor
          Hru_snow(i) = Hru_ppt(i)
          Newsnow(i) = 1

!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain

        ELSEIF ( Tmin(i).GT.Tmax_allsnow .OR.
     +           Tmax(i).GE.Tmax_allrain(mo) ) THEN
          pcor = Rain_adj(i, mo)
          Hru_ppt(i) = ppt*pcor
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******Otherwise precipitation is a mixture of rain and snow

        ELSE
          Prmx(i) = ((Tmax(i)-Tmax_allsnow)/(Tmax(i)-Tmin(i)))*
     +              Adjmix_rain(mo)

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain

          IF ( Prmx(i).GE.1.0 ) THEN  !rsr changed > to GE 1/8/2006
            pcor = Rain_adj(i, mo)
            Hru_ppt(i) = ppt*pcor
            Hru_rain(i) = Hru_ppt(i)

!******If not, it is a rain/snow mixture

          ELSE
            pcor = Snow_adj(i, mo)
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

      pptrun = 0

!dbg 9001 FORMAT (I5, 2('/', I2), F11.5, 3F9.5)
 9002 FORMAT ('Warning, bad precipitation value:', F10.3,
     +        '; precip station:', I3, '; Time:', I5, 2('/', I2.2), I3,
     +        2(':', I2.2), '; value set to 0.0')

      END FUNCTION pptrun

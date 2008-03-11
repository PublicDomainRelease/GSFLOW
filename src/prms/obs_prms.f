!***********************************************************************
! Module to read and make available measured data from PRMS data file
!***********************************************************************
      MODULE PRMS_OBS
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nobs, Nrain, Ntemp, Nsol, Nevap, Nform
      INTEGER :: Nwind, Nhumid, Ngateht
      REAL, ALLOCATABLE :: Tmax_prev(:), Tmin_prev(:)
!   Declared Variables
      INTEGER :: Route_on, Rain_day
      REAL :: Dt_data
      INTEGER, ALLOCATABLE :: Form_data(:)
      REAL, ALLOCATABLE :: Pan_evap(:), Runoff(:), Precip(:), Solrad(:)
      REAL, ALLOCATABLE :: Tmax(:), Tmin(:), Tstemp(:)
      REAL, ALLOCATABLE :: Humid(:), Wind(:), Gate_ht(:)
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Rain_code(:)
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun
!***********************************************************************
      obs_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        obs_prms = obsrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        obs_prms = obsdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        obs_prms = obsinit()
      ENDIF

      END FUNCTION obs_prms

!***********************************************************************
!     obsdecl - makes public variable declarations for the
!                     obs module
!   Declared Parameters
!     rain_code
!***********************************************************************
      INTEGER FUNCTION obsdecl()
      USE PRMS_OBS
      IMPLICIT NONE
      INTRINSIC MAX
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: n
!***********************************************************************
      obsdecl = 1

      IF ( declmodule(
     +'$Id: obs_prms.f 3618 2007-11-15 20:09:22Z rsregan $'
     +).NE.0 ) RETURN

      Nobs = getdim('nobs')
      IF ( Nobs.EQ.-1 ) RETURN

      Nrain = getdim('nrain')
      IF ( Nrain.EQ.-1 ) RETURN

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN

      Nsol = getdim('nsol')
      IF ( Nsol.EQ.-1 ) RETURN

      Nform = getdim('nform')
      IF ( Nform.EQ.-1 ) RETURN

!comment next line and uncomment following two for wind data
      Nwind = 0
!     Nwind = get dim('nwind')
!     IF ( Nwind.EQ.-1 ) RETURN

!comment next line and uncomment following two for wind data
      Nhumid = 0
!     Nhumid = get dim('nhumid')
!     IF ( Nhumid.EQ.-1 ) RETURN

!comment next line and uncomment following two for gate data
      Ngateht = 0
!     Ngateht = get dim('ngateht')
!     IF ( Ngateht.EQ.-1 ) RETURN

      Nevap = getdim('nevap')
      IF ( Nevap.EQ.-1 ) RETURN

!   Declared Variables
      IF ( declvar ('obs', 'rain_day', 'one', 1, 'integer',
     +     'Flag to force rain day',
     +     'none',
     +     Rain_day).NE.0 ) RETURN

      n = MAX(Nobs, 1)
      ALLOCATE (Runoff(n))
      IF ( declvar('obs', 'runoff', 'nobs', n, 'real',
     +     'Observed runoff for each stream gage',
     +     'cfs',
     +     Runoff).NE.0 ) RETURN

      n = MAX(Nrain, 1)
      ALLOCATE (Precip(n))
      IF ( declvar('obs', 'precip', 'nrain', n, 'real',
     +     'Observed precipitation at each rain gage',
     +     'inches',
     +     Precip).NE.0 ) RETURN

      n = MAX(Ntemp, 1)
      ALLOCATE (Tmin(n), Tmax(n))
      ALLOCATE (Tmin_prev(n), Tmax_prev(n))
      IF ( declvar('obs', 'tmin', 'ntemp', n, 'real',
     +     'Observed daily minimum temperature at each measurement'//
     +     'station',
     +     'degrees',
     +     Tmin).NE.0 ) RETURN

      IF ( declvar('obs', 'tmax', 'ntemp', n, 'real',
     +     'Observed daily maximum temperature at each measurement'//
     +     'station',
     +     'degrees',
     +     Tmax).NE.0 ) RETURN

      ALLOCATE (Tstemp(n))
      IF ( declvar('obs', 'tstemp', 'ntemp', n, 'real',
     +     'Observed temperature for timestep',
     +     'degrees',
     +     Tstemp).NE.0 ) RETURN

      IF ( declvar('obs', 'dt_data', 'one', 1, 'integer',
     +     'Time step of model',
     +     'hours',
     +     Dt_data).NE.0 ) RETURN

      n = MAX(Nsol, 1)
      ALLOCATE (Solrad(n))
      IF ( declvar('obs', 'solrad', 'nsol', n, 'real',
     +     'Observed solar radiation at each measurement station',
     +     'langleys',
     +     Solrad).NE.0 ) RETURN

      n = MAX(Nevap, 1)
      ALLOCATE (Pan_evap(n))
      IF ( declvar('obs', 'pan_evap', 'nevap', n, 'real',
     +     'Observed pan evaporation at each measurement station',
     +     'inches',
     +     Pan_evap).NE.0 ) RETURN

      n = MAX(Nform, 1)
      ALLOCATE (Form_data(n))
      IF ( declvar('obs', 'form_data', 'nform', n, 'integer',
     +     'Form of precipitation  (0=not known; 1=snow; 2=rain)',
     +     'none',
     +     Form_data).NE.0 ) RETURN

      IF ( declvar('obs', 'route_on', 'one', 1, 'integer',
     +     'Kinematic routing switch (0=daily; 1=storm period)',
     +     'none',
     +     Route_on).NE.0 ) RETURN

!     IF ( Nhumid.GT.0 ) THEN
!       ALLOCATE (Humid(Nhumid))
!       IF ( decl var('obs', 'humid', 'nhumid', Nhumid, 'real',
!    +       'Observed relative humidity',
!    +       'precent',
!    +       Humid).NE.0) RETURN
!     ENDIF

!     IF ( Nwind.GT.0 ) THEN
!       ALLOCATE (Wind(Nwind))
!       IF ( decl var('obs', 'wind', 'nwind', Nwind, 'real',
!    +       'Observed wind speed',
!    +       'm/s',
!    +       Wind).NE.0 ) RETURN
!     ENDIF

!     IF ( Ngateht.GT.0 ) THEN
!       ALLOCATE (Gate_ht(Ngateht))
!       IF ( decl var('obs', 'gate_ht', 'ngate', Ngateht, 'real',
!    +       'Height of the gate opening at the dam.',
!    +       'inches',
!    +       Gate_ht).NE.0 ) RETURN
!     ENDIF

!   Declared Parameters
      ALLOCATE ( Rain_code(MAXMO))
      IF ( declparam('obs', 'rain_code', 'nmonths', 'integer',
     +     '2', '1', '5',
     +     'Code indicating rule for precip station use',
     +     'Code indicating rule for precip station use'//
     +     ' (1=only precip if the regression stations have precip;'//
     +     ' 2=only precip if any station in the basin has precip;'//
     +     ' 3=precip if xyz says so;'//
     +     ' 4=only precip if rain_day variable is set to 1;'//
     +     ' 5=only precip if psta_freq_nuse stations see precip)',
     +     'none').NE.0 ) RETURN

      obsdecl = 0

      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_OBS
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      obsinit = 1

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getparam('obs', 'rain_code', MAXMO, 'integer',
     +     Rain_code).NE.0 ) RETURN

      IF ( getstep().EQ.0 ) THEN
        Dt_data = 0
        IF ( Nhumid.GT.0 ) Humid = -100.0
        IF ( Nwind.GT.0 ) Wind = -100.0
        IF ( Ngateht.GT.0 ) Gate_ht = -100.0
        Runoff = -1.0
        Precip = -1.0
        Tmax = -100.0
        Tmin = -100.0
        Tmax_prev = -100.0
        Tmin_prev = -100.0
        Tstemp = -100.0
        Solrad = -1.0
        Pan_evap = -1.0
        Form_data = 0
        Route_on = 0
      ENDIF

      obsinit = 0
      END FUNCTION obsinit

! **********************************************************************
!     obsrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsrun()
      USE PRMS_OBS
      IMPLICIT NONE
      INTRINSIC SNGL
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, nstep, julcalen, julsolar, julwater
      INTEGER :: datetime(6), storm
      DOUBLE PRECISION :: dt
! **********************************************************************
      obsrun = 1

      dt = deltim()
      Dt_data = SNGL(dt)
      IF ( dt.LT.23.999D0 .OR. dt.GT.24.0001D0 ) THEN
        storm = 1
        PRINT *, 'Error, GSFLOW can only run a daily time step'
        RETURN
      ELSE
        storm = 0
      ENDIF

      CALL dattim('now', datetime)

      IF ( Prt_debug.EQ.3 ) THEN
        nstep = getstep()
        PRINT *, 'obsrun dt =', dt
        PRINT *, 'obsrun nstep = ', nstep
!     print out nstep, deltim, time and julian dates
        julcalen = julian('now', 'calendar')
        julsolar = julian('now', 'solar')
        julwater = julian('now', 'water')
 
        CALL dpint4('Nstep        :', nstep, 1, 2)
        CALL dpdble('Delta t      :', dt, 1, 2)
        CALL dpint4('Date/Time    :', datetime, 6, 2)
        CALL dpint4('Jul Calen.   :', julcalen, 1, 2)
        CALL dpint4('Jul Solar    :', julsolar, 1, 2)
        CALL dpint4('Jul Water    :', julwater, 1, 2)
      ENDIF

      IF ( Nobs.GT.0 ) THEN
        IF ( readvar('obs', 'runoff').NE.0 ) RETURN
      ENDIF

      IF ( Nrain.GT.0 ) THEN
        IF ( readvar('obs', 'precip').NE.0 ) RETURN
      ENDIF

      IF ( Ntemp.GT.0 ) THEN
        IF ( readvar('obs', 'tmax').NE.0 ) RETURN
        IF ( readvar('obs', 'tmin').NE.0 ) RETURN
        DO i = 1, Ntemp
          IF ( Tmax(i).LT.-50.0 .OR. Tmax(i).GT.150.0 ) THEN
            Tmax(i) = Tmax_prev(i)
!           PRINT 9001, 'tmax', Tmax(i), i, datetime
          ELSE
            Tmax_prev(i) = Tmax(i)
          ENDIF
          IF ( Tmin(i).LT.-50.0 .OR. Tmin(i).GT.150.0 ) THEN
            Tmin(i) = Tmin_prev(i)
!           PRINT 9001, 'tmin', Tmin(i), i, datetime
          ELSE
            Tmin_prev(i) = Tmin(i)
          ENDIF
          Tstemp(i) = (Tmax(i)+Tmin(i))*0.5
        ENDDO

!       IF ( storm.EQ.1 ) THEN
!         IF ( read var('obs', 'tstemp').NE.0 ) RETURN
!       ENDIF

      ENDIF

      IF ( Nsol.GT.0 ) THEN
        IF ( readvar('obs', 'solrad').NE.0 ) RETURN
      ENDIF

      IF ( Nevap.GT.0 ) THEN
        IF ( readvar('obs', 'pan_evap').NE.0 ) RETURN
      ENDIF

      IF ( Nform.GT.0 ) THEN
        IF ( readvar('obs', 'form_data').NE.0 ) RETURN
      ENDIF

      Route_on = 0
      IF ( storm.EQ.1 ) THEN
        IF ( readvar('obs', 'route_on').NE.0 ) RETURN
      ENDIF

      IF ( Nhumid.GT.0 ) THEN
        IF ( readvar('obs', 'humid').NE.0 ) RETURN
      ENDIF
      IF ( Nwind.GT.0 ) THEN
        IF ( readvar('obs', 'wind').NE.0 ) RETURN
      ENDIF
      IF ( Ngateht.GT.0 ) THEN
        IF ( readvar('obs', 'gate_ht').NE.0 ) RETURN
      ENDIF

      Rain_day = 1
      IF ( Prt_debug.EQ.3 ) THEN
        PRINT *, 'rain_code = ', Rain_code(datetime(2))
        PRINT *, Rain_code
        PRINT *, 'datetime = ', datetime(2)
      ENDIF
      IF ( Rain_code(datetime(2)).EQ.4 ) THEN
        IF ( Prt_debug.EQ.3 ) PRINT *, '   reading rain_day '
        IF ( readvar('obs', 'rain_day').NE.0 ) RETURN
      ENDIF

      obsrun = 0

!9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3,
!    +        '; temperature staion:', I3, ' Time:', I5, 2('/', I2.2),
!    +        I3, 2(':', I2.2))

      END FUNCTION obsrun

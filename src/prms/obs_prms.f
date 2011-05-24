!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
      MODULE PRMS_OBS
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Nobs, Nevap, Nform, Modays(12), Yrdays
      INTEGER, SAVE :: Nowtime(6), Xyz_flg, Jday, Jsol, Julwater
      INTEGER, SAVE :: Nowday, Nowmonth, Nowyear
      REAL, SAVE :: Cfs_conv, Timestep_seconds, Timestep_days
!   Declared Variables
      INTEGER, SAVE :: Rain_day
      INTEGER, SAVE, ALLOCATABLE :: Form_data(:)
      REAL, SAVE, ALLOCATABLE :: Pan_evap(:), Runoff(:), Precip(:)
      REAL, SAVE, ALLOCATABLE :: Tmax(:), Tmin(:), Solrad(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Rain_code(:)
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun
!***********************************************************************
      obs_prms = 0

      IF ( Process_flag==0 ) THEN
        obs_prms = obsrun()
      ELSEIF ( Process_flag==1 ) THEN
        obs_prms = obsdecl()
      ELSEIF ( Process_flag==2 ) THEN
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
      USE PRMS_MODULE, ONLY: Precip_flag, Model
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Nrain, Nsol
      IMPLICIT NONE
      INTRINSIC MAX
      INTEGER, EXTERNAL :: declmodule, declvar, getdim, declparam
! Local Variables
      INTEGER :: n
!***********************************************************************
      obsdecl = 1

      IF ( declmodule(
     +'$Id: obs_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

      Nobs = getdim('nobs')
      IF ( Nobs.EQ.-1 ) RETURN
      IF ( Nrain<1 ) THEN
        PRINT *, 'ERROR, must have at least one precip station'
        RETURN
      ENDIF      

      IF ( Ntemp<1 ) THEN
        PRINT *, 'ERROR, must have at least one temperature station'
        RETURN
      ENDIF      

!   Declared Variables
      n = MAX(Nobs, 1)
      ALLOCATE (Runoff(n))
      IF ( declvar('obs', 'runoff', 'nobs', n, 'real',
     +     'Measured runoff for each stream gage',
     +     'runoff_units',
     +     Runoff).NE.0 ) RETURN

      ALLOCATE (Precip(Nrain))
      IF ( declvar('obs', 'precip', 'nrain', Nrain, 'real',
     +     'Measured precipitation at each rain gage',
     +     'precip_units',
     +     Precip).NE.0 ) RETURN

      ALLOCATE (Tmin(Ntemp), Tmax(Ntemp))
      IF ( declvar('obs', 'tmin', 'ntemp', Ntemp, 'real',
     +     'Measured daily minimum temperature at each measurement'//
     +     ' station',
     +     'temp_units',
     +     Tmin).NE.0 ) RETURN

      IF ( declvar('obs', 'tmax', 'ntemp', Ntemp, 'real',
     +     'Measured daily maximum temperature at each measurement'//
     +     ' station',
     +     'temp_units',
     +     Tmax).NE.0 ) RETURN

      n = MAX(Nsol, 1)
      ALLOCATE (Solrad(n))
      IF ( declvar('obs', 'solrad', 'nsol', n, 'real',
     +     'Measured solar radiation at each measurement station',
     +     'langleys',
     +     Solrad).NE.0 ) RETURN

      Nform = getdim('nform')
      IF ( Nform.EQ.-1 ) RETURN
      n = MAX(Nform, 1)
      ALLOCATE (Form_data(n))
      IF ( declvar('obs', 'form_data', 'nform', n, 'integer',
     +     'Form of precipitation  (0=not known; 1=snow; 2=rain)',
     +     'none',
     +     Form_data).NE.0 ) RETURN

      Nevap = getdim('nevap')
      IF ( Nevap.EQ.-1 ) RETURN
      n = MAX(Nevap, 1)
      ALLOCATE (Pan_evap(n))
      IF ( declvar('obs', 'pan_evap', 'nevap', n, 'real',
     +     'Measured pan evaporation at each measurement station',
     +     'inches',
     +     Pan_evap).NE.0 ) RETURN

!   Declared Parameters
      IF ( Precip_flag==6 .OR. Model==99 ) THEN
        IF ( declvar ('obs', 'rain_day', 'one', 1, 'integer',
     +       'Flag to force rain day',
     +       'none',
     +       Rain_day).NE.0 ) RETURN

        ALLOCATE ( Rain_code(12))
        IF ( declparam('obs', 'rain_code', 'nmonths', 'integer',
     +       '2', '1', '5',
     +       'Code indicating rule for precip station use',
     +       'Code indicating rule for precip station use'//
     +       ' (1=only precip if the regression stations have precip;'//
     +       ' 2=only precip if any station in the basin has precip;'//
     +       ' 3=precip if xyz says so;'//
     +       ' 4=only precip if rain_day variable is set to 1;'//
     +       ' 5=only precip if psta_freq_nuse stations see precip)',
     +       'none').NE.0 ) RETURN
        Xyz_flg = 1
      ELSE
        Xyz_flg = 0
      ENDIF

      obsdecl = 0
      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_OBS
      USE PRMS_BASIN, ONLY: Timestep, Starttime
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam, isleap, julian
      DOUBLE PRECISION, EXTERNAL :: deltim
      DOUBLE PRECISION :: dts
!***********************************************************************
      obsinit = 1

      IF ( Xyz_flg==1 ) THEN
        IF ( getparam('obs', 'rain_code', 12, 'integer',
     +       Rain_code).NE.0 ) RETURN
      ENDIF

      IF ( Timestep==0 ) THEN
        Runoff = -1.0
        Precip = -1.0
        Tmax = -100.0
        Tmin = -100.0
        Solrad = -1.0
        Pan_evap = -1.0
        Form_data = 0
      ENDIF

      Modays(1) = 31
      Modays(3) = 31
      Modays(4) = 30
      Modays(5) = 31
      Modays(6) = 30
      Modays(7) = 31
      Modays(8) = 31
      Modays(9) = 30
      Modays(10) = 31
      Modays(11) = 30
      Modays(12) = 31
      IF ( isleap(Starttime(1)).EQ.1 ) THEN
        Yrdays = 366
        Modays(2) = 29
      ELSE
        Yrdays = 365
        Modays(2) = 28
      ENDIF

      Jsol = julian('start', 'solar')
      Julwater = julian('start', 'water')

      dts = deltim()*3600.0D0
      Timestep_seconds = SNGL(dts)
      Timestep_days = SNGL(deltim()/24.0D0)
      Cfs_conv = SNGL(43560.0D0/12.0D0/dts)

      obsinit = 0
      END FUNCTION obsinit

! **********************************************************************
!     obsrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsrun()
      USE PRMS_OBS
      USE PRMS_BASIN, ONLY: Timestep
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Nrain, Nsol
      IMPLICIT NONE
      INTEGER, EXTERNAL :: julian, isleap, readvar, getstep
      DOUBLE PRECISION, EXTERNAL :: deltim
      EXTERNAL :: dattim
      INTRINSIC SNGL
! Local Variables
      INTEGER :: i
      DOUBLE PRECISION :: dt, dthr
! **********************************************************************
      obsrun = 1

      Timestep = getstep()

      CALL dattim('now', Nowtime)
      Nowyear = Nowtime(1)
      Nowmonth = Nowtime(2)
      Nowday = Nowtime(3)

      Jday = julian('now', 'calendar')
      Jsol = julian('now', 'solar')
      Julwater = julian('now', 'water')

      dthr = deltim() 
      dt = dthr*3600.0D0
!   Check to see if daily time step
      IF ( dthr>24.0001D0 ) THEN
        PRINT *, 'ERROR, timestep > daily, fix Data File', dthr
        STOP
      ENDIF
      IF ( dthr.LT.23.999D0 ) THEN
        PRINT *, 'ERROR, timestep < daily, fix Data File', dthr
        STOP
      ENDIF
      Timestep_seconds = SNGL(dt)
      Cfs_conv = SNGL(43560.0D0/12.0D0/dt)
      Timestep_days = SNGL(dthr/24.0D0)

      IF ( isleap(Nowyear)==1 ) THEN
        Yrdays = 366
        Modays(2) = 29
      ELSE
        Yrdays = 365
        Modays(2) = 28
      ENDIF

      IF ( Nobs>0 ) THEN
        IF ( readvar('obs', 'runoff').NE.0 ) RETURN
        DO i = 1, Nobs
          IF ( Runoff(i)<0.0 ) Runoff(i) = -11.0
        ENDDO
      ENDIF

      IF ( readvar('obs', 'precip').NE.0 ) RETURN

      IF ( readvar('obs', 'tmax').NE.0 ) RETURN
      IF ( readvar('obs', 'tmin').NE.0 ) RETURN

      IF ( Nsol>0 ) THEN
        IF ( readvar('obs', 'solrad').NE.0 ) RETURN
      ENDIF

      IF ( Nform>0 ) THEN
        IF ( readvar('obs', 'form_data').NE.0 ) RETURN
      ENDIF

      IF ( Nevap>0 ) THEN
        IF ( readvar('obs', 'pan_evap').NE.0 ) RETURN
      ENDIF

      IF ( Xyz_flg==1 ) THEN
        IF ( Rain_code(Nowmonth).EQ.4 ) THEN
          IF ( readvar('obs', 'rain_day').NE.0 ) RETURN
        ENDIF
      ENDIF

      obsrun = 0
      END FUNCTION obsrun

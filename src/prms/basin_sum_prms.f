!***********************************************************************
! Computes daily, monthly, yearly, and total flow summaries of volumes
! and flows for all HRUs
!***********************************************************************

      MODULE PRMS_BASINSUM
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 199
      INTEGER, SAVE :: Header_prt, Endjday, Endyr
      CHARACTER(LEN=32) :: Buffer32
      CHARACTER(LEN=40) :: Buffer40
      CHARACTER(LEN=48) :: Buffer48
      CHARACTER(LEN=80) :: Buffer80
      CHARACTER(LEN=120) :: Buffer120
      CHARACTER(LEN=160) :: Buffer160
      CHARACTER(LEN=140), PARAMETER :: DASHS =
     +' -----------------------------------------------------'//
     +'--------------------------------------------------------------'//
     +'------------------------'
      CHARACTER(LEN=140), PARAMETER :: STARS =
     +' *****************************************************'//
     +'**************************************************************'//
     +'************************'
      CHARACTER(LEN=140), PARAMETER :: EQULS =
     +' ====================================================='//
     +'=============================================================='//
     +'========================'
!   Declared Private Variables
      INTEGER, SAVE :: Totdays
      LOGICAL, SAVE :: Dprt, Mprt, Yprt, Tprt
      REAL, SAVE :: Obs_runoff_mo, Obs_runoff_yr, Obs_runoff_tot
      REAL, SAVE :: Basin_cfs_mo, Basin_cfs_yr, Basin_cfs_tot
      REAL, SAVE :: Basin_net_ppt_yr, Basin_net_ppt_tot, Watbal_sum
      REAL, SAVE :: Basin_max_temp_yr, Basin_max_temp_tot
      REAL, SAVE :: Basin_min_temp_yr, Basin_min_temp_tot
      REAL, SAVE :: Basin_potet_yr, Basin_potet_tot
      REAL, SAVE :: Basin_actet_yr, Basin_actet_tot
      REAL, SAVE :: Basin_et_yr, Basin_et_tot
      REAL, SAVE :: Basin_snowmelt_yr, Basin_snowmelt_tot
      REAL, SAVE :: Basin_gwflow_yr, Basin_gwflow_tot
      REAL, SAVE :: Basin_ssflow_yr, Basin_ssflow_tot
      REAL, SAVE :: Basin_sroff_yr, Basin_sroff_tot
      REAL, SAVE :: Basin_stflow_yr, Basin_stflow_tot
      REAL, SAVE :: Basin_ppt_yr, Basin_ppt_tot, Last_basin_stor
      REAL, SAVE :: Basin_intcp_evap_yr, Basin_intcp_evap_tot
      REAL, SAVE :: Obsq_inches_yr, Obsq_inches_tot
!   Declared Variables
      REAL, SAVE :: Basin_net_ppt_mo
      REAL, SAVE :: Basin_max_temp_mo, Basin_min_temp_mo, Basin_potet_mo
      REAL, SAVE :: Basin_actet_mo, Basin_et_mo, Basin_ppt_mo
      REAL, SAVE :: Basin_snowmelt_mo, Basin_gwflow_mo, Basin_ssflow_mo
      REAL, SAVE :: Basin_sroff_mo, Basin_stflow_mo, Obsq_inches_mo
      REAL, SAVE :: Basin_intcp_evap_mo, Basin_storage, Basin_et
      REAL, SAVE, ALLOCATABLE :: Hru_et_cum(:), Obsq_cms(:), Obsq_cfs(:)
      REAL, SAVE :: Basin_storvol, Obsq_inches
!   Declared Parameters
      INTEGER, SAVE :: Print_type, Print_freq, Outlet_sta, Runoff_units
      END MODULE PRMS_BASINSUM

!***********************************************************************
!     Main basin_sum_prms routine
!***********************************************************************
      INTEGER FUNCTION basin_sum_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: sumbdecl, sumbinit, sumbrun
!***********************************************************************
      basin_sum_prms = 0

      IF ( Process_flag==0 ) THEN
        basin_sum_prms = sumbrun()
      ELSEIF ( Process_flag==1 ) THEN
        basin_sum_prms = sumbdecl()
      ELSEIF ( Process_flag==2 ) THEN
        basin_sum_prms = sumbinit()
      ENDIF

      END FUNCTION basin_sum_prms

!***********************************************************************
!     sumbdecl - set up basin summary parameters
!   Declared Parameters
!     print_type, print_freq, runoff_units, outlet_sta
!***********************************************************************
      INTEGER FUNCTION sumbdecl()
      USE PRMS_BASINSUM
      USE PRMS_MODULE, ONLY: Model
      USE PRMS_BASIN, ONLY: Nhru
      USE PRMS_OBS, ONLY: Nobs
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declpri, declparam, declvar
!***********************************************************************
      sumbdecl = 1

      IF ( declmodule(
     +'$Id: basin_sum_prms.f 2182 2010-11-26 19:23:44Z rsregan $'
     +).NE.0 ) RETURN

      IF ( declpri('sumb_last_basin_stor', 1, 'real', Last_basin_stor)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_watbal_sum', 1, 'real', Watbal_sum)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obs_runoff_mo', 1, 'real', Obs_runoff_mo)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_cfs_mo', 1, 'real', Basin_cfs_mo)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obs_runoff_yr', 1, 'real', Obs_runoff_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_cfs_yr', 1, 'real', Basin_cfs_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_net_ppt_yr', 1, 'real', Basin_net_ppt_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_max_temp_yr', 1, 'real',
     +     Basin_max_temp_yr).NE.0 ) RETURN
      IF ( declpri('sumb_basin_min_temp_yr', 1, 'real',
     +     Basin_min_temp_yr).NE.0 ) RETURN
      IF ( declpri('sumb_basin_potet_yr', 1, 'real', Basin_potet_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_actet_yr', 1, 'real', Basin_actet_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_snowmelt_yr', 1, 'real',
     +     Basin_snowmelt_yr).NE.0 ) RETURN
      IF ( declpri('sumb_basin_gwflow_yr', 1, 'real', Basin_gwflow_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ssflow_yr', 1, 'real', Basin_ssflow_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_sroff_yr', 1, 'real', Basin_sroff_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ppt_yr', 1, 'real', Basin_ppt_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_et_yr', 1, 'real', Basin_et_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_stflow_yr', 1, 'real', Basin_stflow_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obsq_inches_yr', 1, 'real', Obsq_inches_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_intcp_evap_yr', 1, 'real',
     +     Basin_intcp_evap_yr).NE.0 ) RETURN
      IF ( declpri('sumb_obs_runoff_tot', 1, 'real', Obs_runoff_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_cfs_tot', 1, 'real', Basin_cfs_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ppt_tot', 1, 'real', Basin_ppt_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_net_ppt_tot', 1, 'real',
     +     Basin_net_ppt_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_max_temp_tot', 1, 'real',
     +     Basin_max_temp_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_min_temp_tot', 1, 'real',
     +     Basin_min_temp_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_potet_tot', 1, 'real', Basin_potet_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_actet_tot', 1, 'real', Basin_actet_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_et_tot', 1, 'real', Basin_et_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_snowmelt_tot', 1, 'real',
     +     Basin_snowmelt_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_gwflow_tot', 1, 'real', Basin_gwflow_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ssflow_tot', 1, 'real', Basin_ssflow_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_sroff_tot', 1, 'real', Basin_sroff_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_stflow_tot', 1, 'real', Basin_stflow_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obsq_inches_tot', 1, 'real', Obsq_inches_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_intcp_evap_tot', 1, 'real',
     +     Basin_intcp_evap_tot).NE.0 ) RETURN
      IF ( declpri('sumb_totdays', 1, 'integer', Totdays).NE.0 ) RETURN

! declare parameters
      IF ( Nobs>0 .OR. Model==99 ) THEN
        IF ( declparam('sumb', 'outlet_sta', 'one', 'integer',
     +       '1', 'bounded', 'nobs',
     +       'Index of measurement station to use for basin outlet',
     +       'Index of measurement station to use for basin outlet',
     +       'none').NE.0 ) RETURN

        IF ( declparam('sumb', 'runoff_units', 'one', 'integer',
     +       '0', '0', '1',
     +       'Measured runoff units',
     +       'Measured runoff units (0=cfs; 1=cms)',
     +       'none').NE.0 ) RETURN
      ENDIF

      IF ( declparam('sumb', 'print_type', 'one', 'integer',
     +     '1', '0', '2',
     +     'Type of output data file',
     +     'Output data file (0=measured and simulated flow only;'//
     +     ' 1=water balance table; 2=detailed output)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('sumb', 'print_freq', 'one', 'integer',
     +     '1', '0', '15',
     +     'Frequency for output data file',
     +     'Output data file (0=none; 1=run totals; 2=yearly;'//
     +     ' 4=monthly; 8=daily; or additive combinations)'//
     +     ' For combinations, add index numbers, e.g., daily'//
     +     ' plus yearly = 10; yearly plus total = 3',
     +     'none').NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_intcp_evap_mo', 'one', 1, 'real',
     +     'Total monthly basin interception evaporation',
     +     'inches',
     +     Basin_intcp_evap_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_storage', 'one', 1, 'real',
     +     'Storage in basin including groundwater, subsurface'//
     +     ' storage, soil moisture, snowpack, and interception',
     +     'inches',
     +     Basin_storage).NE.0 ) RETURN

!******************basin_storage volume:
      IF ( declvar('sumb', 'basin_storvol', 'one', 1, 'real',
     +     'Storage in basin including groundwater, subsurface'//
     +     ' storage, soil moisture, snowpack, and interception',
     +     'acre-inches',
     +     Basin_storvol).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_et', 'one', 1, 'real',
     +     'Evapotranspiration on basin including et, snow evap'//
     +     ' and interception evap for timestep',
     +     'inches',
     +     Basin_et).NE.0 ) RETURN

      IF ( declvar('sumb', 'obsq_inches', 'one', 1, 'real',
     +     'Measured streamflow',
     +     'inches',
     +     Obsq_inches).NE.0 ) RETURN

      IF ( Nobs>0 .OR. Model==99 ) THEN
        ALLOCATE (Obsq_cms(Nobs))
        IF ( declvar('sumb', 'obsq_cms', 'nobs', Nobs, 'real',
     +       'Measured streamflow for each streamflow station',
     +       'm3/s',
     +       Obsq_cms).NE.0 ) RETURN

        ALLOCATE (Obsq_cfs(Nobs))
        IF ( declvar('sumb', 'obsq_cfs', 'nobs', Nobs, 'real',
     +       'Measured streamflow for each streamflow station',
     +       'cfs',
     +       Obsq_cfs).NE.0 ) RETURN
      ENDIF

      IF ( declvar('sumb', 'basin_ppt_mo', 'one', 1, 'real',
     +     'Total monthly basin precip',
     +     'inches',
     +     Basin_ppt_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_net_ppt_mo', 'one', 1, 'real',
     +     'Total monthly basin net precip',
     +     'inches',
     +     Basin_net_ppt_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_max_temp_mo', 'one', 1, 'real',
     +     'Monthly average basin maximum temperature',
     +     'degrees',
     +     Basin_max_temp_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_min_temp_mo', 'one', 1, 'real',
     +     'Monthly average basin minimum temperature',
     +     'degrees',
     +     Basin_min_temp_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_potet_mo', 'one', 1, 'real',
     +     'Total monthly basin potential evapotranspiration',
     +     'inches',
     +     Basin_potet_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_actet_mo', 'one', 1, 'real',
     +     'Total monthly basin computed evapotranspiration',
     +     'inches',
     +     Basin_actet_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_et_mo', 'one', 1, 'real',
     +     'Total monthly basin_et',
     +     'inches',
     +     Basin_et_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_snowmelt_mo', 'one', 1, 'real',
     +     'Total monthly basin snowmelt',
     +     'inches',
     +     Basin_snowmelt_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_gwflow_mo', 'one', 1, 'real',
     +     'Total monthly basin groundwater flow',
     +     'inches',
     +     Basin_gwflow_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_ssflow_mo', 'one', 1, 'real',
     +     'Total monthly basin subsurface flow',
     +     'inches',
     +     Basin_ssflow_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_sroff_mo', 'one', 1, 'real',
     +     'Total monthly basin surface runoff',
     +     'inches',
     +     Basin_sroff_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'basin_stflow_mo', 'one', 1, 'real',
     +     'Total monthly basin simulated streamflow',
     +     'inches',
     +     Basin_stflow_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'obsq_inches_mo', 'one', 1, 'real',
     +     'Total monthly basin measured streamflow',
     +     'inches',
     +     Obsq_inches_mo).NE.0 ) RETURN

      ALLOCATE (Hru_et_cum(Nhru))
      IF ( declvar('sumb', 'hru_et_cum', 'nhru', Nhru, 'real',
     +     'Cumulative computed et for each hru for the year',
     +     'inches',
     +     Hru_et_cum).NE.0 ) RETURN

      sumbdecl = 0
      END FUNCTION sumbdecl

!***********************************************************************
!     sumbinit - Initialize basinsum module - get parameter values
!                set to zero
!***********************************************************************
      INTEGER FUNCTION sumbinit()
      USE PRMS_BASINSUM
      USE PRMS_BASIN, ONLY: Timestep, Print_debug, Endtime
      USE PRMS_FLOWVARS, ONLY: Basin_imperv_stor
      USE PRMS_OBS, ONLY: Nobs
      USE PRMS_INTCP, ONLY: Basin_intcp_stor
      USE PRMS_SNOW, ONLY: Basin_pweqv
      USE PRMS_SOILZONE, ONLY: Basin_soil_moist, Basin_ssstor
      USE PRMS_GWFLOW_CASC, ONLY: Basin_gwstor
      IMPLICIT NONE
      INTRINSIC MAX
      INTEGER, EXTERNAL :: getparam, getvar, julian
      EXTERNAL :: header_print
! Local Variables
      INTEGER :: pftemp
!***********************************************************************
      sumbinit = 1

      IF ( Nobs>0 ) THEN
        IF ( getparam('sumb', 'outlet_sta', 1, 'integer', Outlet_sta)
     +       .NE.0 ) RETURN
        IF ( Outlet_sta==0 ) Outlet_sta = Nobs
      ENDIF

      IF ( getparam('sumb', 'print_type', 1, 'integer', Print_type)
     +     .NE.0 ) RETURN

      IF ( getparam('sumb', 'print_freq', 1, 'integer', Print_freq)
     +     .NE.0 ) RETURN

!****************************************************************
      Last_basin_stor = Basin_soil_moist + Basin_intcp_stor +
     +                  Basin_gwstor + Basin_ssstor + Basin_pweqv +
     +                  Basin_imperv_stor

!******Set daily print switch
      pftemp = Print_freq

      IF ( pftemp.GE.8 ) THEN
        Dprt = .TRUE.
        pftemp = pftemp - 8
      ELSE
        Dprt = .FALSE.
      ENDIF

!******Set monthly print switch
      IF ( pftemp.GE.4 ) THEN
        Mprt = .TRUE.
        pftemp = pftemp - 4
      ELSE
        Mprt = .FALSE.
      ENDIF

!******Set yearly print switch
      IF ( pftemp.GE.2 ) THEN
        Yprt = .TRUE.
        pftemp = pftemp - 2
      ELSE
        Yprt = .FALSE.
      ENDIF

!******Set total print switch
      IF ( pftemp.EQ.1 ) THEN
        Tprt = .TRUE.
      ELSE
        Tprt = .FALSE.
      ENDIF

!  Zero stuff out when Timestep = 0
      IF ( Timestep==0 ) THEN
        Watbal_sum = 0.

!******Zero all mo yr to that aren't variables
        Obs_runoff_mo = 0.
        Basin_cfs_mo = 0.
        Basin_ppt_mo = 0.
        Basin_net_ppt_mo = 0.
        Basin_max_temp_mo = 0.
        Basin_min_temp_mo = 0.
        Basin_intcp_evap_mo = 0.
        Basin_potet_mo = 0.
        Basin_actet_mo = 0.
        Basin_et_mo = 0.
        Basin_snowmelt_mo = 0.
        Basin_gwflow_mo = 0.
        Basin_ssflow_mo = 0.
        Basin_sroff_mo = 0.
        Basin_stflow_mo = 0.
        Obsq_inches_mo = 0.

        Obs_runoff_yr = 0.
        Basin_cfs_yr = 0.
        Basin_ppt_yr = 0.
        Basin_net_ppt_yr = 0.
        Basin_max_temp_yr = 0.
        Basin_min_temp_yr = 0.
        Basin_intcp_evap_yr = 0.
        Basin_potet_yr = 0.
        Basin_actet_yr = 0.
        Basin_et_yr = 0.
        Basin_snowmelt_yr = 0.
        Basin_gwflow_yr = 0.
        Basin_ssflow_yr = 0.
        Basin_sroff_yr = 0.
        Basin_stflow_yr = 0.
        Obsq_inches_yr = 0.

        Obs_runoff_tot = 0.
        Basin_cfs_tot = 0.
        Basin_ppt_tot = 0.
        Basin_net_ppt_tot = 0.
        Basin_max_temp_tot = 0.
        Basin_min_temp_tot = 0.
        Basin_intcp_evap_tot = 0.
        Basin_potet_tot = 0.
        Basin_actet_tot = 0.
        Basin_et_tot = 0.
        Basin_snowmelt_tot = 0.
        Basin_gwflow_tot = 0.
        Basin_ssflow_tot = 0.
        Basin_sroff_tot = 0.
        Basin_stflow_tot = 0.
        Obsq_inches_tot = 0.

        Hru_et_cum = 0.

        Totdays = 0
        Obsq_inches = 0.

        Basin_storage = 0.
        Basin_storvol = 0.
        Basin_et = 0.
      ENDIF

      IF ( Nobs>0 ) THEN
        Obsq_cms = -1.
        Obsq_cfs = -1.
        IF ( getparam('sumb', 'runoff_units', 1, 'integer',
     +       Runoff_units).NE.0 ) RETURN
      ENDIF

!******Set header print switch (1 prints a new header after every month
!****** summary, 2 prints a new header after every year summary
      Header_prt = 0
      IF ( Print_freq.EQ.6 .OR. Print_freq.EQ.7 .OR. Print_freq.EQ.10
     +     .OR. Print_freq.EQ.11 ) Header_prt = 1
      IF ( Print_freq.GE.12 ) Header_prt = 2
!$$$ Commented out since this prevented detailed report at yearly
!$$$ frequency. Intent unclear. RMTW
!$$$      
      IF ( Print_freq.EQ.2 .OR. Print_freq.EQ.3 ) Print_type = 3
      IF ( Print_freq==0 .AND. Print_type==0 ) Print_type = 4

!  Put a header on the output file (regardless of Timestep)
!  when the model starts.
      CALL header_print(Print_type)

! rmtw - Print span dashes and initial storage
!
!
      IF ( Print_type.EQ.1 ) THEN
        WRITE (Buffer48, "(35X,F9.4)") Last_basin_stor
        CALL opstr(Buffer48(:44))

      ELSEIF ( Print_type.EQ.2 ) THEN
        WRITE (Buffer120, 9001) Basin_intcp_stor, 
     +                          Basin_soil_moist, Basin_pweqv,
     +                          Basin_gwstor, Basin_ssstor
        CALL opstr(Buffer120(:94))
      ENDIF

      Endjday = julian('end', 'calendar')
      Endyr = Endtime(1)

      IF ( Print_debug.EQ.4 ) OPEN (BALUNT, FILE='basin_sum_prms.wbal')

      sumbinit = 0

 9001 FORMAT (39X, F6.2, 18X, 2F6.2, F13.2, F6.2)

      END FUNCTION sumbinit

!***********************************************************************
!     sumbrun - Computes summary values
!***********************************************************************
      INTEGER FUNCTION sumbrun()
      USE PRMS_BASINSUM
      USE PRMS_MODULE, ONLY: Ncascade
      USE PRMS_BASIN, ONLY: Cfs2inches, CFS2CMS_CONV, Basin_area_inv,
     +    Active_hrus, Hru_route_order, Print_debug, Basin_cfs,
     +    Basin_stflow, Nhru
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_lakeevap,
     +    Basin_actet, Basin_perv_et, Basin_swale_et, Hru_actet,
     +    Basin_sroff, Strm_farfield, Basin_imperv_evap,
     +    Basin_imperv_stor
      USE PRMS_CLIMATEVARS, ONLY: Orad, Basin_ppt, Basin_potet,
     +    Solrad_tmax, Solrad_tmin
      USE PRMS_OBS, ONLY: Jday, Runoff, Nowtime, Nobs, Modays, Yrdays
      USE PRMS_GWFLOW_CASC, ONLY: Basin_gwflow, Basin_gwstor,
     +    Basin_gwsink
      USE PRMS_INTCP, ONLY: Basin_intcp_evap, Basin_intcp_stor,
     +    Basin_net_ppt
      USE PRMS_SNOW, ONLY: Basin_snowmelt, Basin_pweqv, Basin_snowevap
      USE PRMS_SOILZONE, ONLY: Basin_ssstor, Basin_soil_moist
      IMPLICIT NONE
      INTRINSIC SNGL, ABS, ALOG
      INTEGER, EXTERNAL :: getvar, julian
      EXTERNAL :: header_print
! Local variables
      INTEGER :: i, j, wyday, year, mo, day, endrun
      REAL :: ryear, rmo, rday, wat_bal, obsrunoff, tmax, tmin
      REAL :: strmfarflow_inches
!***********************************************************************
      sumbrun = 1

      wyday = julian('now', 'water')
      year = Nowtime(1)
      ryear = year
      mo = Nowtime(2)
      rmo = mo
      day = Nowtime(3)
      rday = day

      IF ( year.EQ.Endyr .AND. Jday.EQ.Endjday ) THEN
        endrun = 1
      ELSE
        endrun = 0
      ENDIF

      IF ( Nobs.GT.0 ) THEN
        IF ( Runoff_units.EQ.1 ) THEN
          DO j = 1, Nobs
            Obsq_cms(j) = Runoff(j)
            Obsq_cfs(j) = Runoff(j)/CFS2CMS_CONV
          ENDDO
        ELSE
          DO j = 1, Nobs
            Obsq_cms(j) = Runoff(j)*CFS2CMS_CONV
            Obsq_cfs(j) = Runoff(j)
          ENDDO
        ENDIF
      ENDIF

      IF ( Ncascade>0 ) THEN
        Basin_cfs = Basin_cfs - Strm_farfield
        strmfarflow_inches = Strm_farfield*Cfs2inches
      ELSE
        strmfarflow_inches = 0.0
      ENDIF

!*****Compute aggregated values

      Last_basin_stor = Last_basin_stor
      Basin_storage = Basin_soil_moist + Basin_intcp_stor +
     +                Basin_gwstor + Basin_ssstor + Basin_pweqv +
     +                Basin_imperv_stor

! volume calculation for storage
      Basin_storvol = Basin_storage/Basin_area_inv

      Basin_et = Basin_perv_et + Basin_imperv_evap +
     +           Basin_intcp_evap + Basin_snowevap + Basin_lakeevap
     +           + Basin_swale_et
       !rsr, this should be the same as basin_actet, a sanity check
       !     note it does not include depression storage ET

      obsrunoff = 0.0
      IF ( Nobs>0 ) obsrunoff = Obsq_cfs(Outlet_sta)
      Obsq_inches = obsrunoff*Cfs2inches

      wat_bal = Last_basin_stor - Basin_storage + Basin_ppt
     +          - Basin_et - Basin_stflow - Basin_gwsink

      IF ( Print_debug.EQ.4 ) THEN
        WRITE (BALUNT,"(A,2I4,7F8.4)") ' bsto-sm-in-gw-ss-sn-iv ', mo,
     +         day, Basin_storage, Basin_soil_moist, Basin_intcp_stor,
     +         Basin_gwstor, Basin_ssstor, Basin_pweqv,
     +         Basin_imperv_stor
 
        WRITE (BALUNT,"(A,I6,5F8.4)") ' bet-pv-iv-in-sn', day, Basin_et,
     +         Basin_perv_et, Basin_imperv_evap, Basin_intcp_evap,
     +         Basin_snowevap

        WRITE (BALUNT,"(A,I6,7F8.4,/)") ' bal-pp-et-st-ls-bs-gs ', day,
     +         wat_bal, Basin_ppt, Basin_et, Basin_stflow,
     +         Last_basin_stor, Basin_storage, Basin_gwsink
      ENDIF

      Watbal_sum = Watbal_sum + wat_bal

      Last_basin_stor = Basin_storage

      tmax = Solrad_tmax
      tmin = Solrad_tmin

!******Check for daily print

      IF ( Dprt ) THEN
        IF ( Print_type.EQ.0 ) THEN
          WRITE (Buffer40, "(F7.0,2F5.0,F11.2,F12.2)") ryear, rmo, rday,
     +           obsrunoff, Basin_cfs
          CALL opstr(Buffer40)

        ELSEIF ( Print_type.EQ.1 ) THEN
          WRITE (Buffer120,"(F7.0,2F5.0,2F9.5,6F9.4)") ryear,
     +           rmo, rday, Basin_ppt, Basin_et, Basin_storage,
     +           Basin_stflow-strmfarflow_inches, Obsq_inches, wat_bal,
     +           Watbal_sum, strmfarflow_inches
          CALL opstr(Buffer120)

        ELSEIF ( Print_type.EQ.2 ) THEN
          WRITE (Buffer160, 9001) ryear, rmo, rday, Orad, tmax, tmin,
     +          Basin_ppt, Basin_net_ppt, Basin_intcp_stor,
     +          Basin_intcp_evap, Basin_potet, Basin_actet,
     +          Basin_soil_moist, Basin_pweqv, Basin_snowmelt,
     +          Basin_gwstor, Basin_ssstor, Basin_gwflow, Basin_ssflow,
     +          Basin_sroff, Basin_stflow-strmfarflow_inches, Basin_cfs,
     +          obsrunoff, Basin_lakeevap
          CALL opstr(Buffer160(:154))

        ENDIF
      ENDIF
      IF ( Print_debug.EQ.4 ) WRITE (BALUNT, *) 'wat_bal =', wat_bal,
     +                               ' watbal_sum=', Watbal_sum

!******Compute monthly values
      IF ( day.EQ.1 ) THEN
        Obs_runoff_mo = 0.
        Basin_cfs_mo = 0.
        Basin_ppt_mo = 0.
        Basin_net_ppt_mo = 0.
        Basin_max_temp_mo = 0.
        Basin_min_temp_mo = 0.
        Basin_intcp_evap_mo = 0.
        Basin_potet_mo = 0.
        Basin_actet_mo = 0.
        Basin_et_mo = 0.
        Basin_snowmelt_mo = 0.
        Basin_gwflow_mo = 0.
        Basin_ssflow_mo = 0.
        Basin_sroff_mo = 0.
        Basin_stflow_mo = 0.
        Obsq_inches_mo = 0.
      ENDIF

      Obs_runoff_mo = Obs_runoff_mo + obsrunoff
      Obsq_inches_mo = Obsq_inches_mo + obsrunoff*Cfs2inches
      Basin_cfs_mo = Basin_cfs_mo + Basin_cfs
      Basin_ppt_mo = Basin_ppt_mo + Basin_ppt
      Basin_net_ppt_mo = Basin_net_ppt_mo + Basin_net_ppt
      Basin_max_temp_mo = Basin_max_temp_mo + tmax
      Basin_min_temp_mo = Basin_min_temp_mo + tmin
      Basin_intcp_evap_mo = Basin_intcp_evap_mo + Basin_intcp_evap
      Basin_potet_mo = Basin_potet_mo + Basin_potet
      Basin_actet_mo = Basin_actet_mo + Basin_actet
      Basin_et_mo = Basin_et_mo + Basin_et
      Basin_snowmelt_mo = Basin_snowmelt_mo + Basin_snowmelt
      Basin_gwflow_mo = Basin_gwflow_mo + Basin_gwflow
      Basin_ssflow_mo = Basin_ssflow_mo + Basin_ssflow
      Basin_sroff_mo = Basin_sroff_mo + Basin_sroff
      Basin_stflow_mo = Basin_stflow_mo + Basin_stflow

      IF ( day.EQ.Modays(mo) ) THEN
        Basin_max_temp_mo = Basin_max_temp_mo/Modays(mo)
        Basin_min_temp_mo = Basin_min_temp_mo/Modays(mo)
        Obs_runoff_mo = Obs_runoff_mo/Modays(mo)
        Basin_cfs_mo = Basin_cfs_mo/Modays(mo)

        IF ( Mprt ) THEN
          IF ( Print_type.EQ.0 ) THEN
            IF ( Dprt ) CALL opstr(DASHS(:40))
            WRITE (Buffer40, "(F7.0,F5.0,F16.2,F12.2)") ryear, rmo,
     +             Obs_runoff_mo, Basin_cfs_mo
            CALL opstr(Buffer40)

          ELSEIF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            IF ( Dprt ) CALL opstr(DASHS(:62))
            WRITE (Buffer80, "(F7.0,F5.0,5X,5F9.3)") ryear, rmo,
     +             Basin_ppt_mo, Basin_et_mo, Basin_storage,
     +             Basin_stflow_mo, Obsq_inches_mo
            CALL opstr(Buffer80(:62))

          ELSEIF ( Print_type.EQ.2 ) THEN
            IF ( Dprt ) CALL opstr(DASHS)

            WRITE (Buffer160, 9006) ryear, rmo, Basin_max_temp_mo,
     +            Basin_min_temp_mo, Basin_ppt_mo, Basin_net_ppt_mo,
     +            Basin_intcp_evap_mo, Basin_potet_mo, Basin_actet_mo,
     +            Basin_soil_moist, Basin_pweqv, Basin_snowmelt_mo,
     +            Basin_gwstor, Basin_ssstor, Basin_gwflow_mo,
     +            Basin_ssflow_mo, Basin_sroff_mo, Basin_stflow_mo,
     +            Basin_cfs_mo, Obs_runoff_mo
            CALL opstr(Buffer160(:131))
          ENDIF

        ENDIF
      ENDIF

!******Check for year print

      IF ( Yprt ) THEN
        Obs_runoff_yr = Obs_runoff_yr + obsrunoff
        Obsq_inches_yr = Obsq_inches_yr + obsrunoff*Cfs2inches
        Basin_cfs_yr = Basin_cfs_yr + Basin_cfs
        Basin_ppt_yr = Basin_ppt_yr + Basin_ppt
        Basin_net_ppt_yr = Basin_net_ppt_yr + Basin_net_ppt
        Basin_max_temp_yr = Basin_max_temp_yr + tmax
        Basin_min_temp_yr = Basin_min_temp_yr + tmin
        Basin_intcp_evap_yr = Basin_intcp_evap_yr + Basin_intcp_evap
        Basin_potet_yr = Basin_potet_yr + Basin_potet
        Basin_actet_yr = Basin_actet_yr + Basin_actet
        Basin_et_yr = Basin_et_yr + Basin_et
        Basin_snowmelt_yr = Basin_snowmelt_yr + Basin_snowmelt
        Basin_gwflow_yr = Basin_gwflow_yr + Basin_gwflow
        Basin_ssflow_yr = Basin_ssflow_yr + Basin_ssflow
        Basin_sroff_yr = Basin_sroff_yr + Basin_sroff
        Basin_stflow_yr = Basin_stflow_yr + Basin_stflow
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Hru_et_cum(i) = Hru_et_cum(i) + Hru_actet(i)
        ENDDO

        IF ( wyday.EQ.Yrdays ) THEN
          IF ( Print_type.EQ.0 ) THEN

            Obs_runoff_yr = Obs_runoff_yr/Yrdays
            Basin_cfs_yr = Basin_cfs_yr/Yrdays
            IF ( Mprt .OR. Dprt ) CALL opstr(EQULS(:40))
            WRITE (Buffer40, "(F7.0,F21.2,F12.2)") ryear, Obs_runoff_yr,
     +             Basin_cfs_yr
            CALL opstr(Buffer40)

! ****annual summary here
          ELSEIF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            IF ( Mprt .OR. Dprt ) CALL opstr(EQULS(:62))
            WRITE (Buffer80, "(F7.0,10X,5F9.3)") ryear, Basin_ppt_yr,
     +             Basin_et_yr, Basin_storage, Basin_stflow_yr,
     +             Obsq_inches_yr
            CALL opstr(Buffer80(:62))

          ELSEIF ( Print_type.EQ.2 ) THEN
            Basin_max_temp_yr = Basin_max_temp_yr/Yrdays
            Basin_min_temp_yr = Basin_min_temp_yr/Yrdays
            Obs_runoff_yr = Obs_runoff_yr/Yrdays
            Basin_cfs_yr = Basin_cfs_yr/Yrdays
            IF ( Mprt .OR. Dprt ) CALL opstr(EQULS)
            WRITE (Buffer160, 9007) ryear, Basin_max_temp_yr,
     +            Basin_min_temp_yr, Basin_ppt_yr, Basin_net_ppt_yr,
     +            Basin_intcp_evap_yr, Basin_potet_yr, Basin_actet_yr,
     +            Basin_soil_moist, Basin_pweqv, Basin_snowmelt_yr,
     +            Basin_gwstor, Basin_ssstor, Basin_gwflow_yr,
     +            Basin_ssflow_yr, Basin_sroff_yr, Basin_stflow_yr,
     +            Basin_cfs_yr, Obs_runoff_yr
            CALL opstr(Buffer160(:131))
          ENDIF

          Obs_runoff_yr = 0.
          Basin_cfs_yr = 0.
          Basin_ppt_yr = 0.
          Basin_net_ppt_yr = 0.
          Basin_max_temp_yr = 0.
          Basin_min_temp_yr = 0.
          Basin_intcp_evap_yr = 0.
          Basin_potet_yr = 0.
          Basin_actet_yr = 0.
          Basin_et_yr = 0.
          Basin_snowmelt_yr = 0.
          Basin_gwflow_yr = 0.
          Basin_ssflow_yr = 0.
          Basin_sroff_yr = 0.
          Basin_stflow_yr = 0.
          Obsq_inches_yr = 0.
          Hru_et_cum = 0.

        ENDIF
      ENDIF

!******Print heading if needed
      IF ( endrun.EQ.0 ) THEN
        IF ( (Header_prt.EQ.2 .AND. day.EQ.Modays(mo)) .OR.
     +       (Header_prt.EQ.1 .AND. wyday.EQ.Yrdays) ) THEN
          Buffer32 = ' '
          CALL opstr(Buffer32(:1))
          CALL header_print(Print_type)
        ENDIF
      ENDIF

!******Check for total print

      IF ( Tprt ) THEN
        Totdays = Totdays + 1
        Obs_runoff_tot = Obs_runoff_tot + obsrunoff
        Obsq_inches_tot = Obsq_inches_tot + obsrunoff*Cfs2inches
        Basin_cfs_tot = Basin_cfs_tot + Basin_cfs
        Basin_ppt_tot = Basin_ppt_tot + Basin_ppt
        Basin_net_ppt_tot = Basin_net_ppt_tot + Basin_net_ppt
        Basin_max_temp_tot = Basin_max_temp_tot + tmax
        Basin_min_temp_tot = Basin_min_temp_tot + tmin
        Basin_intcp_evap_tot = Basin_intcp_evap_tot + Basin_intcp_evap
        Basin_potet_tot = Basin_potet_tot + Basin_potet
        Basin_actet_tot = Basin_actet_tot + Basin_actet
        Basin_et_tot = Basin_et_tot + Basin_et
        Basin_snowmelt_tot = Basin_snowmelt_tot + Basin_snowmelt
        Basin_gwflow_tot = Basin_gwflow_tot + Basin_gwflow
        Basin_ssflow_tot = Basin_ssflow_tot + Basin_ssflow
        Basin_sroff_tot = Basin_sroff_tot + Basin_sroff
        Basin_stflow_tot = Basin_stflow_tot + Basin_stflow

        IF ( endrun.EQ.1 ) THEN

          IF ( Print_type.EQ.0 ) THEN
            Obs_runoff_tot = Obs_runoff_tot/Totdays
            Basin_cfs_tot = Basin_cfs_tot/Totdays
            CALL opstr(STARS(:40))
            WRITE (Buffer48, "(A,3X,2F12.2)") ' Total for run',
     +             Obs_runoff_tot, Basin_cfs_tot
            CALL opstr(Buffer48(:41))
          ENDIF

          IF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            CALL opstr(STARS(:62))
            WRITE (Buffer80, 9005) ' Total for run', Basin_ppt_tot,
     +                            Basin_et_tot, Basin_storage,
     +                            Basin_stflow_tot, Obsq_inches_tot
            CALL opstr(Buffer80(:62))
          ENDIF

          IF ( Print_type.EQ.2 ) THEN
            Obs_runoff_tot = Obs_runoff_tot/Totdays
            Basin_cfs_tot = Basin_cfs_tot/Totdays
            CALL opstr(STARS)
            WRITE (Buffer160, 9004) ' Total for run', Basin_ppt_tot,
     +            Basin_net_ppt_tot, Basin_intcp_evap_tot,
     +            Basin_potet_tot, Basin_actet_tot, Basin_soil_moist,
     +            Basin_pweqv, Basin_snowmelt_tot, Basin_gwstor,
     +            Basin_ssstor, Basin_gwflow_tot, Basin_ssflow_tot,
     +            Basin_sroff_tot, Basin_stflow_tot, Basin_cfs_tot,
     +            Obs_runoff_tot
            CALL opstr(Buffer160(:140))
          ENDIF
        ENDIF
      ENDIF

      sumbrun = 0

 9001 FORMAT (F6.0, 2F3.0, 3F5.0, 8F6.2, F7.2, 2F6.2, 4F7.4, 2F9.2,
     +        2F7.4)
 9004 FORMAT (A, 13X, 2F6.2, F11.1, F7.2, 3F6.2, F7.2, 2F6.2, 4F7.2,
     +        2F9.2)
 9005 FORMAT (A, 3X, 6F9.3)
 9006 FORMAT (F6.0, F3.0, 8X, 2F5.0, 2F6.2, F11.1, F7.2, 3F6.2,
     +        F7.2, 2F6.2, 4F7.2, 2F9.2)
 9007 FORMAT (F6.0, 11X, 2F5.0, 2F6.2, F11.1, F7.2, 3F6.2, F7.2,
     +        2F6.2, 4F7.2, 2F9.2)

      END FUNCTION sumbrun

!***********************************************************************
! Print headers for tables
! This writes the measured and simulated table header.
!***********************************************************************
      SUBROUTINE header_print(Print_type)
      USE PRMS_BASINSUM, ONLY: DASHS, Buffer80
      IMPLICIT NONE
      EXTERNAL opstr
! Arguments
      INTEGER, INTENT(IN) :: Print_type
!***********************************************************************
      IF ( Print_type.EQ.0 ) THEN
        CALL opstr('1  Year Month Day   Measured   Simulated')
        CALL opstr('                      (cfs)      (cfs)')
        CALL opstr(DASHS(:40))

!  This writes the water balance table header.
      ELSEIF ( Print_type.EQ.1 ) THEN
        CALL opstr(
     +'1  Year Month Day   Precip     ET    Storage S-Runoff M-Runoff'//
     +'   Watbal  WBalSum')
        WRITE (Buffer80, 9001)
        CALL opstr(Buffer80)
        CALL opstr(DASHS(:80))

!  This writes the detailed table header.
      ELSEIF ( Print_type.EQ.2 ) THEN
        CALL opstr(
     +    '1Year mo day srad  tmx  tmn  ppt  n-ppt  ints  intl potet'//
     +    ' actet  smav pweqv   melt gwsto sssto gwflow ssflow  sroff'//
     +    ' tot-fl      sim    meas')
        CALL opstr(
     +    '             (ly)  (F)  (F) (in.) (in.) (in.) (in.) (in.)'//
     +    ' (in.) (in.) (in.)  (in.) (in.) (in.)  (in.)  (in.)  (in.)'//
     +    '  (in.)    (cfs)    (cfs)')
        CALL opstr(DASHS)

!  This writes the water balance table header.
      ELSEIF ( Print_type.EQ.3 ) THEN
        CALL opstr(
     + '1  Year Month Day   Precip     ET    Storage S-Runoff M-Runoff')
        WRITE (Buffer80, 9002)
        CALL opstr(Buffer80(:62))
        CALL opstr(DASHS(:62))

      ENDIF

 9001 FORMAT (17X, 7(' (inches)'))
 9002 FORMAT (17X, 5(' (inches)'))

      END SUBROUTINE header_print

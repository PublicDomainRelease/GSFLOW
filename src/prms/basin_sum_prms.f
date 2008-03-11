!***********************************************************************
! Sums values for daily, monthly, yearly and total flow for daily mode
!***********************************************************************

      MODULE PRMS_BASINSUM
!   Local Variables
      INTEGER :: Nhru, Nobs, Ntemp, Header_prt, Objfuncq, Endjday
      INTEGER :: Endyr, Nowtime(6)
      REAL, PARAMETER :: CFS2CMS_CONV = 0.028316847
      REAL :: Cfs2inches
!   Declared Private Variables
      INTEGER :: Totdays
      LOGICAL :: Dprt, Mprt, Yprt, Tprt
      REAL :: Obs_runoff_mo, Obs_runoff_yr, Obs_runoff_tot
      REAL :: Basin_cfs_mo, Basin_cfs_yr, Basin_cfs_tot
      REAL :: Basin_net_ppt_yr, Basin_net_ppt_tot, Watbal_sum
      REAL :: Basin_max_temp_yr, Basin_max_temp_tot
      REAL :: Basin_min_temp_yr, Basin_min_temp_tot
      REAL :: Basin_potet_yr, Basin_potet_tot
      REAL :: Basin_actet_yr, Basin_actet_tot
      REAL :: Basin_et_yr, Basin_et_tot
      REAL :: Basin_snowmelt_yr, Basin_snowmelt_tot
      REAL :: Basin_gwflow_yr, Basin_gwflow_tot
      REAL :: Basin_ssflow_yr, Basin_ssflow_tot
      REAL :: Basin_sroff_yr, Basin_sroff_tot
      REAL :: Basin_stflow_yr, Basin_stflow_tot
      REAL :: Basin_ppt_yr, Basin_ppt_tot, Last_basin_stor
      REAL :: Basin_intcp_evap_yr, Basin_intcp_evap_tot
      REAL :: Obsq_inches_yr, Obsq_inches_tot
      REAL :: Sum_obj_func_yr(5), Sum_obj_func_mo(5)
!   Declared Variables
      REAL :: Basin_net_ppt_mo
      REAL :: Basin_max_temp_mo, Basin_min_temp_mo, Basin_potet_mo
      REAL :: Basin_actet_mo, Basin_et_mo, Basin_ppt_mo
      REAL :: Basin_snowmelt_mo, Basin_gwflow_mo, Basin_ssflow_mo
      REAL :: Basin_sroff_mo, Basin_stflow_mo, Obsq_inches_mo
      REAL :: Basin_intcp_evap_mo, Basin_storage, Basin_et, Obsq_inches
      REAL, ALLOCATABLE :: Hru_et_cum(:), Obsq_cms(:), Obsq_cfs(:)
!   Declared Variables from other modules - precip
      REAL :: Basin_ppt
!   Declared Variables from other modules - smbal or soilzone
      REAL :: Basin_perv_et, Basin_actet, Basin_soil_moist
      REAL, ALLOCATABLE :: Hru_actet(:)
!   Declared Variables from other modules - snowmelt
      REAL :: Basin_snowmelt, Basin_pweqv, Basin_snowevap
!   Declared Variables from other modules - srunoff
      REAL :: Basin_imperv_stor, Basin_imperv_evap, Basin_sroff
!   Declared Variables from other modules - gwflow
      REAL :: Basin_gwflow, Basin_gwsink, Basin_gwstor
!   Declared Variables from other modules - ssflow or soilzone
      REAL :: Basin_ssstor, Basin_ssflow
!   Declared Variables from other modules - intcp
      REAL :: Basin_intcp_evap, Basin_intcp_stor, Basin_net_ppt
!   Declared Variables from other modules - solrad
      REAL :: Orad
!   Declared Variables from other modules - potet
      REAL :: Basin_potet
!   Declared Variables from other modules - temp
      REAL, ALLOCATABLE :: Tmaxf(:), Tminf(:)
!   Declared Variables from other modules - obs
      REAL, ALLOCATABLE :: Obs_runoff(:), Obs_tmax(:), Obs_tmin(:)
!   Declared Variables from other modules - strmflow
      REAL :: Basin_stflow, Basin_cfs
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug, Xyz_flg, Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Print_type, Print_freq, Print_objfunc
      INTEGER :: Objfunc_q, Runoff_units, Basin_tsta, Basin_tsta_hru
      END MODULE PRMS_BASINSUM

!***********************************************************************
!     Main basin_sum routine
!***********************************************************************
      INTEGER FUNCTION basin_sum_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: sumbdecl, sumbinit, sumbrun
!***********************************************************************
      basin_sum_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        basin_sum_prms = sumbrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        basin_sum_prms = sumbdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        basin_sum_prms = sumbinit()
      ENDIF

      END FUNCTION basin_sum_prms

!***********************************************************************
!     sumbdecl - set up basin summary parameters
!   Declared Parameters
!     print_type, print_freq, print_objfunc, objfunc_q
!     runoff_units, basin_tsta, basin_tsta_hru
!***********************************************************************
      INTEGER FUNCTION sumbdecl()
      USE PRMS_BASINSUM
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC MAX
! Local Variables
      INTEGER :: ndim
!***********************************************************************
      sumbdecl = 1

      IF ( declmodule(
     +'$Id: basin_sum_prms.f 3907 2008-02-26 16:57:37Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nobs = getdim('nobs')
      IF ( Nobs.EQ.-1 ) RETURN

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN

      IF ( declpri('sumb_last_basin_stor', 1, 'real', Last_basin_stor)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_watbal_sum', 1, 'real', Watbal_sum)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_sum_obj_func_yr', 5, 'real', Sum_obj_func_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_sum_obj_func_mo', 5, 'real', Sum_obj_func_mo)
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
      IF ( declparam('sumb', 'print_type', 'one', 'integer',
     +     '1', '0', '2',
     +     'Type of output data file',
     +     'Output data file (0=observed and predicted flow only;'//
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

      IF ( declparam('sumb', 'print_objfunc', 'one', 'integer',
     +     '0', '0', '1',
     +     'Switch to turn objective function printing off and on',
     +     'Print objective functions (0=no; 1=yes)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('sumb', 'objfunc_q', 'one', 'integer',
     +     '0', 'bounded', 'nobs',
     +     'Index of runoff station used in objective function calc.',
     +     'Index of the runoff station used as the observed'//
     +     ' runoff variable in the objective function calculation',
     +     'none').NE.0 ) RETURN

      IF ( declparam('sumb', 'basin_tsta_hru', 'one', 'integer',
     +     '0', 'bounded', 'nhru',
     +     'Index of HRU to use for basin temperature',
     +     'Index of HRU to use for basin temperature',
     +     'none').NE.0 ) RETURN

      IF ( declparam('sumb', 'basin_tsta', 'one', 'integer',
     +     '1', 'bounded', 'ntemp',
     +     'Index of main temperature station',
     +     'Index of temperature station used to compute basin'//
     +     ' temperature values',
     +     'none').NE.0 ) RETURN

      IF ( declparam('sumb', 'runoff_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Observed runoff units',
     +     'Observed runoff units (0=cfs; 1=cms)',
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

      IF ( declvar('sumb', 'basin_et', 'one', 1, 'real',
     +     'Evapotranspiration on basin including et, snow evap'//
     +     ' and interception evap for timestep',
     +     'inches',
     +     Basin_et).NE.0 ) RETURN

      IF ( declvar('sumb', 'obsq_inches', 'one', 1, 'real',
     +     'Observed streamflow',
     +     'inches',
     +     Obsq_inches).NE.0 ) RETURN

      ndim = MAX(Nobs, 1)
      ALLOCATE (Obsq_cms(ndim))
      IF ( declvar('sumb', 'obsq_cms', 'nobs', ndim, 'real',
     +     'Observed streamflow for each streamflow station',
     +     'm3/s',
     +     Obsq_cms).NE.0 ) RETURN

      ALLOCATE (Obsq_cfs(ndim))
      IF ( declvar('sumb', 'obsq_cfs', 'nobs', ndim, 'real',
     +     'Observed streamflow for each streamflow station',
     +     'cfs',
     +     Obsq_cfs).NE.0 ) RETURN

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
     +     'Total monthly basin predicted streamflow',
     +     'inches',
     +     Basin_stflow_mo).NE.0 ) RETURN

      IF ( declvar('sumb', 'obsq_inches_mo', 'one', 1, 'real',
     +     'Total monthly basin observed streamflow',
     +     'inches',
     +     Obsq_inches_mo).NE.0 ) RETURN

      ALLOCATE (Hru_et_cum(Nhru))
      IF ( declvar('sumb', 'hru_et_cum', 'nhru', Nhru, 'real',
     +     'Cumulative computed et for each hru for the year',
     +     'inches',
     +     Hru_et_cum).NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Hru_actet(Nhru), Obs_runoff(ndim))
      ALLOCATE (Tmaxf(Nhru), Tminf(Nhru), Hru_route_order(Nhru))
      ALLOCATE (Obs_tmax(Ntemp), Obs_tmin(Ntemp))

      sumbdecl = 0
      END FUNCTION sumbdecl

!***********************************************************************
!     sumbinit - Initialize basinsum module - get parameter values
!                set to zero
!***********************************************************************
      INTEGER FUNCTION sumbinit()
      USE PRMS_BASINSUM
      IMPLICIT NONE
      INTRINSIC MAX
      INCLUDE 'fmodules.inc'
      EXTERNAL :: header_print
! Local Variables
      INTEGER :: pftemp, nstep
!***********************************************************************
      sumbinit = 1

      nstep = getstep()

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN
      Cfs2inches = Basin_area_inv*12.0*86400.0/43560.0

      IF ( getparam('sumb', 'print_type', 1, 'integer', Print_type)
     +     .NE.0 ) RETURN

      IF ( getparam('sumb', 'print_freq', 1, 'integer', Print_freq)
     +     .NE.0 ) RETURN

      IF ( getparam('sumb', 'print_objfunc', 1, 'integer',
     +     Print_objfunc).NE.0 ) RETURN

      IF ( getparam('sumb', 'objfunc_q', 1, 'integer', Objfunc_q)
     +     .NE.0 ) RETURN
      Objfuncq = MAX(1, Objfunc_q)

      IF ( getparam('sumb', 'basin_tsta', 1, 'integer', Basin_tsta)
     +     .NE.0 ) RETURN
      IF ( Basin_tsta.LT.1 ) Basin_tsta = 1

      IF ( getparam('sumb', 'basin_tsta_hru', 1, 'integer',
     +     Basin_tsta_hru).NE.0 ) RETURN
      IF ( Basin_tsta_hru.LT.1 ) Basin_tsta_hru = 1

      IF ( getparam('sumb', 'runoff_units', 1, 'integer', Runoff_units)
     +     .NE.0 ) RETURN

      IF ( getvar('smbal', 'basin_soil_moist', 1, 'real',
     +     Basin_soil_moist).NE.0 ) RETURN

      IF ( getvar('gwflow', 'basin_gwstor', 1, 'real',
     +     Basin_gwstor).NE.0 ) RETURN

      IF ( getvar('ssflow', 'basin_ssstor', 1, 'real',
     +     Basin_ssstor).NE.0 ) RETURN

      IF ( getvar('intcp', 'basin_intcp_stor', 1, 'real',
     +     Basin_intcp_stor).NE.0 ) RETURN

      IF ( getvar('srunoff', 'basin_imperv_stor', 1, 'real',
     +     Basin_imperv_stor).NE.0 ) RETURN

      IF ( getvar('snow', 'basin_pweqv', 1, 'real', Basin_pweqv)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'xyz_flg', 1, 'integer', Xyz_flg)
     +     .NE.0 ) RETURN

      Last_basin_stor = Basin_soil_moist + Basin_intcp_stor +
     +                  Basin_gwstor + Basin_ssstor + Basin_pweqv +
     +                  Basin_imperv_stor

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

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

!  Zero stuff out when nstep = 0
      IF ( nstep.EQ.0 ) THEN
        Watbal_sum = 0.
        Sum_obj_func_yr = 0.
        Sum_obj_func_mo = 0.

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
        Obs_runoff = -1.
        Obsq_cms = -1.
        Obsq_cfs = -1.

        Basin_storage = 0.
        Basin_et = 0.
      ENDIF

!******Set header print switch
      Header_prt = 0
      IF ( Print_freq.EQ.6 .OR. Print_freq.EQ.7 .OR. Print_freq.EQ.10
     +     .OR. Print_freq.EQ.11 ) Header_prt = 1
      IF ( Print_freq.GE.12 ) Header_prt = 2
      IF ( Print_freq.EQ.2 .OR. Print_freq.EQ.3 ) Print_type = 3

!  Put a header on the output file (regardless of nstep)
!  when the model starts.
      CALL header_print(Print_type)

      Endjday = julian('end', 'calendar')
      CALL dattim('end', Nowtime)
      Endyr = Nowtime(1)

      IF ( Prt_debug.EQ.4 ) OPEN (199, FILE='basin_sum_prms.wbal')

      sumbinit = 0
      END FUNCTION sumbinit

!***********************************************************************
!     sumbrun - Computes summary values
!***********************************************************************
      INTEGER FUNCTION sumbrun()
      USE PRMS_BASINSUM
      IMPLICIT NONE
      INTRINSIC SNGL, ABS, ALOG
      INCLUDE 'fmodules.inc'
      EXTERNAL :: header_print
! Local variables
      CHARACTER(LEN=45) :: buffer
      CHARACTER(LEN=75) :: bufferw
      CHARACTER(LEN=150) :: bufferd
      CHARACTER(LEN=80) :: buffer80
      CHARACTER(LEN=140) :: stars, dashs, equls
      INTEGER :: i, j, jj, wyday, year, mo, day, jday, endrun, yrdays
      REAL :: ryear, rmo, rday, wat_bal, diffop, oflgo, oflgp, obsq
      REAL :: simq, obsrunoff, fac, diflg, obj_func(5), tmax, tmin
!     INTEGER hr
! Local static variables
      INTEGER, SAVE :: modays(12)
      DATA modays/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!***********************************************************************
      sumbrun = 1

      stars = ' *****************************************************'//
     +'**************************************************************'//
     +'************************'
      dashs = ' -----------------------------------------------------'//
     +'--------------------------------------------------------------'//
     +'------------------------'
      equls = ' ====================================================='//
     +'=============================================================='//
     +'========================'

      CALL dattim('now', Nowtime)
      wyday = julian('now', 'water')
      jday = julian('now', 'calendar')
      year = Nowtime(1)
      ryear = year
      mo = Nowtime(2)
      rmo = mo
      day = Nowtime(3)
      rday = day
!     hr = Nowtime(4)

      IF ( isleap(year).EQ.1 ) THEN
        yrdays = 366
        modays(2) = 29
      ELSE
        yrdays = 365
        modays(2) = 28
      ENDIF

      IF ( year.EQ.Endyr .AND. jday.EQ.Endjday ) THEN
        endrun = 1
      ELSE
        endrun = 0
      ENDIF

      IF ( Nobs.GT.0 ) THEN
        IF ( getvar('obs', 'runoff', Nobs, 'real', Obs_runoff)
     +       .NE.0 ) RETURN
        IF ( Runoff_units.EQ.1 ) THEN
          DO j = 1, Nobs
            Obsq_cms(j) = Obs_runoff(j)
            Obsq_cfs(j) = Obs_runoff(j)/CFS2CMS_CONV
          ENDDO
        ELSE
          DO j = 1, Nobs
            Obsq_cms(j) = Obs_runoff(j)*CFS2CMS_CONV
            Obsq_cfs(j) = Obs_runoff(j)
          ENDDO
        ENDIF
      ENDIF

      IF ( getvar('precip', 'basin_ppt', 1, 'real', Basin_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('smbal', 'basin_actet', 1, 'real', Basin_actet)
     +     .NE.0 ) RETURN

      IF ( getvar('smbal', 'basin_perv_et', 1, 'real',
     +     Basin_perv_et).NE.0 ) RETURN

      IF ( getvar('intcp', 'basin_intcp_evap', 1, 'real',
     +     Basin_intcp_evap).NE.0 ) RETURN

      IF ( getvar('snow', 'basin_snowmelt', 1, 'real',
     +     Basin_snowmelt).NE.0 ) RETURN

      IF ( getvar('smbal', 'basin_soil_moist', 1, 'real',
     +     Basin_soil_moist).NE.0 ) RETURN

      IF ( getvar('intcp', 'basin_intcp_stor', 1, 'real',
     +     Basin_intcp_stor).NE.0 ) RETURN

      IF ( getvar('srunoff', 'basin_imperv_stor', 1, 'real',
     +     Basin_imperv_stor).NE.0 ) RETURN

      IF ( getvar('srunoff', 'basin_imperv_evap', 1, 'real',
     +     Basin_imperv_evap).NE.0 ) RETURN

      IF ( getvar('gwflow', 'basin_gwstor', 1, 'real',
     +     Basin_gwstor).NE.0 ) RETURN

      IF ( getvar('gwflow', 'basin_gwsink', 1, 'real',
     +     Basin_gwsink).NE.0 ) RETURN

      IF ( getvar('ssflow', 'basin_ssstor', 1, 'real',
     +     Basin_ssstor).NE.0 ) RETURN

      IF ( getvar('srunoff', 'basin_sroff', 1, 'real',
     +     Basin_sroff).NE.0 ) RETURN

      IF ( getvar('gwflow', 'basin_gwflow', 1, 'real',
     +     Basin_gwflow).NE.0 ) RETURN

      IF ( getvar('ssflow', 'basin_ssflow', 1, 'real',
     +     Basin_ssflow).NE.0 ) RETURN

      IF ( getvar('solrad', 'orad', 1, 'real',
     +     Orad).NE.0 ) RETURN

      IF ( getvar('intcp', 'basin_net_ppt', 1, 'real', Basin_net_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('potet', 'basin_potet', 1, 'real', Basin_potet)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'basin_pweqv', 1, 'real', Basin_pweqv)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'basin_snowevap', 1, 'real',
     +     Basin_snowevap).NE.0 ) RETURN

      IF ( getvar('smbal', 'hru_actet', Nhru, 'real', Hru_actet)
     +     .NE.0 ) RETURN

      IF ( getvar('strmflow', 'basin_stflow', 1, 'real',
     +     Basin_stflow).NE.0 ) RETURN

      IF ( getvar('strmflow', 'basin_cfs', 1, 'real',
     +     Basin_cfs).NE.0 ) RETURN

!*****Compute aggregated values

      Basin_storage = Basin_soil_moist + Basin_intcp_stor +
     +                Basin_gwstor + Basin_ssstor + Basin_pweqv +
     +                Basin_imperv_stor

      Basin_et = Basin_perv_et + Basin_imperv_evap +
     +           Basin_intcp_evap + Basin_snowevap

      obsrunoff = Obsq_cfs(Objfuncq)
!rsr, original version used .04208754 instead of Cfs2inches
!          should have been .04201389
!     obs_inches = obs_runoff(objfunc_q) / (basin_area * .04208754)
      Obsq_inches = obsrunoff*Cfs2inches

      wat_bal = Basin_ppt - Basin_et - Basin_stflow +
     +          Last_basin_stor - Basin_storage - Basin_gwsink

      IF ( Prt_debug.EQ.4 ) THEN
        WRITE (199,"(A,2I4,7F8.4)") ' bsto-sm-in-gw-ss-sn-iv ', mo, day,
     +                              Basin_storage, Basin_soil_moist,
     +                              Basin_intcp_stor, Basin_gwstor,
     +                              Basin_ssstor, Basin_pweqv,
     +                              Basin_imperv_stor
 
        WRITE (199,"(A,I6,5F8.4)") ' bet-pv-iv-in-sn', day, Basin_et,
     +                             Basin_perv_et, Basin_imperv_evap,
     +                             Basin_intcp_evap, Basin_snowevap

        WRITE (199,"(A,I6,7F8.4,/)") ' bal-pp-et-st-ls-bs-gs ', day,
     +                               wat_bal, Basin_ppt, Basin_et,
     +                               Basin_stflow, Last_basin_stor,
     +                               Basin_storage, Basin_gwsink
      ENDIF

      Watbal_sum = Watbal_sum + wat_bal

      Last_basin_stor = Basin_storage

      fac = 1.

!******Compute Objective Function

      obsq = obsrunoff
      simq = Basin_cfs

!rsr??? should obsq be multiplied by fac?, it is below
!rsr  obsq = obsrunoff*fac
      IF ( Objfunc_q.GT.0 ) THEN
        diffop = obsq - simq
        oflgo = ALOG(obsq+1.0)
        oflgp = ALOG(simq+1.0)
      ELSE
        diffop = -1.0
        oflgo = 1.0
        oflgp = 1.0
      ENDIF
      obj_func(1) = ABS(diffop)
      obj_func(2) = diffop*diffop
      diflg = oflgo - oflgp
      obj_func(3) = ABS(diflg)
      obj_func(4) = diflg*diflg
      obj_func(5) = diffop

      DO j = 1, 5
        Sum_obj_func_yr(j) = Sum_obj_func_yr(j) + obj_func(j)
        Sum_obj_func_mo(j) = Sum_obj_func_mo(j) + obj_func(j)
      ENDDO

      IF ( Xyz_flg.EQ.0 ) THEN
        IF ( getvar('obs', 'tmax', Ntemp, 'real', Obs_tmax).NE.0 )RETURN
        IF ( getvar('obs', 'tmin', Ntemp, 'real', Obs_tmin).NE.0 )RETURN
        tmax = Obs_tmax(Basin_tsta)
        tmin = Obs_tmin(Basin_tsta)
      ELSE
        IF ( getvar('temp', 'tmaxf', Nhru, 'real', Tmaxf).NE.0 ) RETURN
        IF ( getvar('temp', 'tminf', Nhru, 'real', Tminf).NE.0 ) RETURN
        tmax = Tmaxf(Basin_tsta_hru)
        tmin = Tminf(Basin_tsta_hru)
      ENDIF

!******Check for daily print

!rsr??? what if dprt is true and using less than daily timestep
!rsr??? should it be average daily or at midnight?
!rsr  IF ( Dprt .AND. hr.EQ.24 ) THEN
!rsr  IF ( Dprt .AND. (hr.EQ.24 .OR. hr.EQ.0 .AND. mn.EQ.0 ) THEN
      IF ( Dprt ) THEN
        IF ( Print_type.EQ.0 ) THEN
          WRITE (buffer, "(F7.0,2F5.0,F11.2,F12.2)") ryear, rmo, rday,
     +           obsrunoff, Basin_cfs
          CALL opstr(buffer)

        ELSEIF ( Print_type.EQ.1 ) THEN
          WRITE (buffer80, "(F7.0,2F5.0,5F9.3,2F9.5)") ryear, rmo, rday,
     +           Basin_ppt, Basin_et, Basin_storage, Basin_stflow,
     +           Obsq_inches, wat_bal, Watbal_sum
          CALL opstr(buffer80)

        ELSEIF ( Print_type.EQ.2 ) THEN
          WRITE (bufferd, 9001) ryear, rmo, rday, Orad, tmax, tmin,
     +                          Basin_ppt, Basin_net_ppt,
     +                          Basin_intcp_stor, Basin_intcp_evap,
     +                          Basin_potet, Basin_actet,
     +                          Basin_soil_moist, Basin_pweqv,
     +                          Basin_snowmelt, Basin_gwstor,
     +                          Basin_ssstor, Basin_gwflow,
     +                          Basin_ssflow, Basin_sroff, Basin_stflow,
     +                          Basin_cfs, obsrunoff
          CALL opstr(bufferd)

        ENDIF
      ENDIF
      IF ( Prt_debug.EQ.4 ) WRITE (199, *) 'wat_bal =', wat_bal,
     +                                     ' watbal_sum=', Watbal_sum

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
        DO i = 1, 5
          Sum_obj_func_mo(i) = obj_func(i)
        ENDDO
      ENDIF

!rsr??? problems if timestep is not 24
      obsq = obsrunoff*fac
      Obs_runoff_mo = Obs_runoff_mo + obsq
!rsr, original version used .04208754 instead of Cfs2inches
!          should have been .04201389
      Obsq_inches_mo = Obsq_inches_mo + obsq*Cfs2inches
      Basin_cfs_mo = Basin_cfs_mo + (Basin_cfs*fac)
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

      IF ( day.EQ.modays(mo) ) THEN
        Basin_max_temp_mo = Basin_max_temp_mo/modays(mo)
        Basin_min_temp_mo = Basin_min_temp_mo/modays(mo)
        Obs_runoff_mo = Obs_runoff_mo/modays(mo)
        Basin_cfs_mo = Basin_cfs_mo/modays(mo)

!rsr    IF ( Mprt .AND. (hr.EQ.24 .OR. hr.EQ.0 .AND. mn.EQ.0 ) THEN
        IF ( Mprt ) THEN
          IF ( Print_type.EQ.0 ) THEN
            IF ( Dprt ) CALL opstr(dashs(:40))
            WRITE (buffer, "(F7.0,F5.0,F16.2,F12.2)") ryear, rmo,
     +             Obs_runoff_mo, Basin_cfs_mo
            CALL opstr(buffer)

          ELSEIF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            IF ( Dprt ) CALL opstr(dashs(:62))
            WRITE (bufferw, "(F7.0,F5.0,5X,5F9.3)") ryear, rmo,
     +             Basin_ppt_mo, Basin_et_mo, Basin_storage,
     +             Basin_stflow_mo, Obsq_inches_mo
            CALL opstr(bufferw(:62))

          ELSEIF ( Print_type.EQ.2 ) THEN
            IF ( Dprt ) CALL opstr(dashs(:140))

            WRITE (bufferd, 9006) ryear, rmo, Basin_max_temp_mo,
     +                            Basin_min_temp_mo, Basin_ppt_mo,
     +                            Basin_net_ppt_mo, Basin_intcp_evap_mo,
     +                            Basin_potet_mo, Basin_actet_mo,
     +                            Basin_soil_moist, Basin_pweqv,
     +                            Basin_snowmelt_mo, Basin_gwstor,
     +                            Basin_ssstor, Basin_gwflow_mo,
     +                            Basin_ssflow_mo, Basin_sroff_mo,
     +                            Basin_stflow_mo, Basin_cfs_mo,
     +                            Obs_runoff_mo
            CALL opstr(bufferd)
          ENDIF

          IF ( Print_objfunc.EQ.1 ) THEN
            CALL opstr('Monthly Objective Functions')
            WRITE (bufferd, 9002) (Sum_obj_func_mo(jj), jj=1, 5)
            CALL opstr(bufferd)
          ENDIF

        ENDIF
      ENDIF

!******Check for year print

!rsr  IF ( Yprt .AND. (hr.EQ.24 .OR. hr.EQ.0 .AND. mn.EQ.0 ) THEN
      IF ( Yprt ) THEN
        Obs_runoff_yr = Obs_runoff_yr + obsq
!rsr, original version used .04208754 instead of Cfs2inches
!          should have been .04201389
        Obsq_inches_yr = Obsq_inches_yr + obsq*Cfs2inches
        Basin_cfs_yr = Basin_cfs_yr + (Basin_cfs*fac)
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

        IF ( wyday.EQ.yrdays ) THEN
          IF ( Print_type.EQ.0 ) THEN

            Obs_runoff_yr = Obs_runoff_yr/yrdays
            Basin_cfs_yr = Basin_cfs_yr/yrdays
            IF ( Mprt .OR. Dprt ) CALL opstr(equls(:40))
            WRITE (buffer, "(F7.0,F21.2,F12.2)") ryear, Obs_runoff_yr,
     +             Basin_cfs_yr
            CALL opstr(buffer)

! ****annual summary here
          ELSEIF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            IF ( Mprt .OR. Dprt ) CALL opstr(equls(:62))
            WRITE (bufferw, "(F7.0,10X,5F9.3)") ryear, Basin_ppt_yr,
     +             Basin_et_yr, Basin_storage, Basin_stflow_yr,
     +             Obsq_inches_yr
            CALL opstr(bufferw(:62))

          ELSEIF ( Print_type.EQ.2 ) THEN
            Basin_max_temp_yr = Basin_max_temp_yr/yrdays
            Basin_min_temp_yr = Basin_min_temp_yr/yrdays
            Obs_runoff_yr = Obs_runoff_yr/yrdays
            Basin_cfs_yr = Basin_cfs_yr/yrdays
            IF ( Mprt .OR. Dprt ) CALL opstr(equls(:140))
            WRITE (bufferd, 9007) ryear, Basin_max_temp_yr,
     +                            Basin_min_temp_yr, Basin_ppt_yr,
     +                            Basin_net_ppt_yr, Basin_intcp_evap_yr,
     +                            Basin_potet_yr, Basin_actet_yr,
     +                            Basin_soil_moist, Basin_pweqv,
     +                            Basin_snowmelt_yr, Basin_gwstor,
     +                            Basin_ssstor, Basin_gwflow_yr,
     +                            Basin_ssflow_yr, Basin_sroff_yr,
     +                            Basin_stflow_yr, Basin_cfs_yr,
     +                            Obs_runoff_yr
            CALL opstr(bufferd)
          ENDIF

          IF ( Print_objfunc.EQ.1 ) THEN
            CALL opstr('Yearly Objective Functions')
            WRITE (bufferd, 9003) (Sum_obj_func_yr(i), i=1, 5)
            CALL opstr(bufferd)
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
          Sum_obj_func_yr = 0.
          Hru_et_cum = 0.

        ENDIF
      ENDIF

!******Print heading if needed
      IF ( endrun.EQ.0 ) THEN
        IF ( (Header_prt.EQ.2 .AND. day.EQ.modays(mo)) .OR.
     +       (Header_prt.EQ.1 .AND. wyday.EQ.yrdays) ) THEN
          buffer = ' '
          CALL opstr(buffer)
          CALL header_print(Print_type)
        ENDIF
      ENDIF

!******Check for total print

!rsr??? what if some timesteps are < 24
!rsr  IF ( Tprt .AND. (hr.EQ.24 .OR. hr.EQ.0 .AND. mn.EQ.0 ) THEN
      IF ( Tprt ) THEN
        Totdays = Totdays + 1
        Obs_runoff_tot = Obs_runoff_tot + obsq
!rsr, original version used .04208754 instead of Cfs2inches
!          should have been .04201389
        Obsq_inches_tot = Obsq_inches_tot + obsq*Cfs2inches
        Basin_cfs_tot = Basin_cfs_tot + (Basin_cfs*fac)
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
            CALL opstr(stars(:40))
            WRITE (buffer, "(A,3X,2F12.2)") ' Total for run',
     +             Obs_runoff_tot, Basin_cfs_tot
            CALL opstr(buffer(:40))
          ENDIF

          IF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            CALL opstr(stars(:62))
            WRITE (bufferw, 9005) ' Total for run', Basin_ppt_tot,
     +                            Basin_et_tot, Basin_storage,
     +                            Basin_stflow_tot, Obsq_inches_tot
            CALL opstr(bufferw(:62))
          ENDIF

          IF ( Print_type.EQ.2 ) THEN
            Obs_runoff_tot = Obs_runoff_tot/Totdays
            Basin_cfs_tot = Basin_cfs_tot/Totdays
            CALL opstr(stars(:140))
            WRITE (bufferd, 9004) ' Total for run', Basin_ppt_tot,
     +                            Basin_net_ppt_tot,
     +                            Basin_intcp_evap_tot, Basin_potet_tot,
     +                            Basin_actet_tot, Basin_soil_moist,
     +                            Basin_pweqv, Basin_snowmelt_tot,
     +                            Basin_gwstor, Basin_ssstor,
     +                            Basin_gwflow_tot, Basin_ssflow_tot,
     +                            Basin_sroff_tot, Basin_stflow_tot,
     +                            Basin_cfs_tot, Obs_runoff_tot
            CALL opstr(bufferd)
          ENDIF
        ENDIF
      ENDIF

      sumbrun = 0

 9001 FORMAT (F6.0, 2F3.0, 3F5.0, 8F6.2, F7.2, 2F6.2, 4F7.4, 2F9.2,
     +        F7.4)
 9002 FORMAT (' Abs Dif= ', F12.1, ' Dif Sq= ', F12.1, ' Abs Diflg= ',
     +        F12.1, ' Diflg Sq= ', F12.1, ' Dif MoSum= ', F12.1)
 9003 FORMAT (' Abs Dif= ', F12.1, ' Dif Sq= ', F12.1, ' Abs Diflg= ',
     +        F12.1, ' Diflg Sq= ', F12.1, ' Dif YrSum= ', F12.1)
 9004 FORMAT (A, 13X, 3F6.2, 6X, 4F6.2, F7.2, 2F6.2, 4F7.2, 2F9.2)
 9005 FORMAT (A, 3X, 6F9.3)
 9006 FORMAT (F6.0, F3.0, 8X, 2F5.0, 3F6.2, 6X, 4F6.2, F7.2, 2F6.2,
     +        4F7.2,2F9.2)
 9007 FORMAT (F6.0, 11X, 2F5.0, 3F6.2, 6X, 4F6.2, F7.2, 2F6.2, 4F7.2,
     +        2F9.2)

      END FUNCTION sumbrun

!***********************************************************************
! Print headers for tables
!***********************************************************************
      SUBROUTINE header_print(Print_type)
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Arguments
      INTEGER, INTENT(IN) :: Print_type
! Local Variables
      CHARACTER(LEN=80) :: bufferw
!***********************************************************************
!  This writes the observed and predicted table header.
      IF ( Print_type.EQ.0 ) THEN
        CALL opstr('1  Year Month Day   Observed   Predicted')
        CALL opstr('                      (cfs)      (cfs)')

!  This writes the water balance table header.
      ELSEIF ( Print_type.EQ.1 ) THEN
        CALL opstr(
     +'1  Year Month Day   Precip     ET    Storage P-Runoff O-Runoff'//
     +'   Watbal  WBalSum')
        WRITE (bufferw, 9001)
        CALL opstr(bufferw)

!  This writes the detailed table header.
      ELSEIF ( Print_type.EQ.2 ) THEN
        CALL opstr(
     +    '1Year mo day srad  tmx  tmn  ppt  n-ppt  ints  intl potet'//
     +    ' actet  smav pweqv   melt gwsto sssto gwflow ssflow  sroff'//
     +    ' tot-fl     pred     obs')
        CALL opstr(
     +    '             (ly)  (F)  (F) (in.) (in.) (in.) (in.) (in.)'//
     +    ' (in.) (in.) (in.)  (in.) (in.) (in.)  (in.)  (in.)  (in.)'//
     +    '  (in.)    (cfs)    (cfs)')

!  This writes the water balance table header.
      ELSEIF ( Print_type.EQ.3 ) THEN
        CALL opstr(
     + '1  Year Month Day   Precip     ET    Storage P-Runoff O-Runoff')
        WRITE (bufferw, 9002)
        CALL opstr(bufferw(:62))

      ENDIF

 9001 FORMAT (17X, 7(' (inches)'))
 9002 FORMAT (17X, 5(' (inches)'))

      END SUBROUTINE header_print

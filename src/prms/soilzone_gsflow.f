!***********************************************************************
! Daily accounting for soil zone;
!    adds infiltration
!    computes et
!    computes recharge of soil zone
!    computes interflow to stream or cascade
!    adjusts storage in soil zone
!    sends dunnian runoff to stream or cascade by adding to sroff
!    computes drainage to groundwater
!***********************************************************************
      MODULE SOILZONE_GSF
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 195
      REAL, PARAMETER :: NEARZERO = 1.0E-15, SMALL = 0.000001
      REAL, PARAMETER :: ONETHIRD = 1.0/3.0, TWOTHIRDS = 2.0/3.0
      INTEGER :: Nhru, Nssr, Ncascade, Nsegment, Nhrucell
      INTEGER :: Nowtime(6), Day, Month, Daily, Nsegmentp1
      REAL :: Last_soil_moist, Last_ssstor, Td, Ts, Cfs_conv
      REAL, ALLOCATABLE :: Replenish_pct(:), Gvr2pfr(:)
      REAL, ALLOCATABLE :: It0_soil_rechr(:), It0_soil_moist(:)
      REAL, ALLOCATABLE :: It0_pref_flow_stor(:), It0_ssres_stor(:)
      REAL, ALLOCATABLE :: It0_gravity_stor_res(:), It0_sroff(:)
      REAL, ALLOCATABLE :: It0_slow_stor(:), It0_strm_seg_in(:)
      REAL :: It0_strm_farfield
!   Declared Variables
      REAL :: Basin_actet, Basin_soil_moist, Basin_sz2gw
      REAL :: Basin_soil_rechr, Basin_perv_et, Basin_soil_to_gw
      REAL :: Basin_ssflow, Basin_ssstor, Basin_ssin, Basin_pref_stor
      REAL :: Basin_sm2gvr, Basin_dnflow, Basin_gvr2sm, Basin_slstor
      REAL :: Basin_infil_tot, Basin_pref_flow_in, Basin_dunnian
      REAL :: Basin_gvr2pfr, Basin_slowflow, Basin_prefflow
      REAL :: Basin_lakeevap, Basin_lakeinsz, Basin_lakeprecip
      REAL :: Basin_szfarflow
      REAL, ALLOCATABLE :: Sm2gw_grav(:), Sm2gw_grav_old(:)
      REAL, ALLOCATABLE :: Gravity_stor_res(:), Gvr2sm(:)
      REAL, ALLOCATABLE :: Ssr_to_gw(:), Ssres_stor(:), Ssres_in(:)
      REAL, ALLOCATABLE :: Ssres_flow(:)
      REAL, ALLOCATABLE :: Soil_moist_tot(:), Soil_moist_pct(:)
      REAL, ALLOCATABLE :: Soil_rechr(:), Soil_moist(:), Perv_actet(:)
      REAL, ALLOCATABLE :: Soil_to_gw(:), Soil_to_ssr(:), Hru_actet(:)
      REAL, ALLOCATABLE :: Upslope_interflow(:), Dunnian_flow(:)
      REAL, ALLOCATABLE :: Upslope_dunnianflow(:), Infil_tot(:)
      REAL, ALLOCATABLE :: Pref_flow_stor(:), Pref_flow(:)
      REAL, ALLOCATABLE :: Pref_flow_thrsh(:), Pref_flow_in(:)
      REAL, ALLOCATABLE :: Slow_flow(:), Slow_stor(:), Pref_flow_max(:)
      REAL, ALLOCATABLE :: Lakein_sz(:), Soil_zone_max(:)
!   Declared Variables from other modules - gsflow_modflow
      INTEGER :: Kkiter
!   Declared Variables from other modules - gsflow_prms
      INTEGER :: Model
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug, Active_hrus
      REAL :: Basin_area_inv
      INTEGER, ALLOCATABLE :: Ncascade_hru(:), Hru_route_order(:)
      REAL, ALLOCATABLE :: Hru_perv(:), Hru_percent_perv(:)
!   Declared Variables from other modules - srunoff
! WARNING!!! Sroff, Basin_sroff, and Strm_seg_in can be updated
      REAL :: Basin_sroff, Strm_farfield
      REAL, ALLOCATABLE :: Strm_seg_in(:), Sroff(:)
      REAL, ALLOCATABLE :: Hru_impervevap(:), Infil(:)
!   Declared Variables from other modules - snow
      REAL, ALLOCATABLE :: Snowcov_area(:), Snow_evap(:)
!   Declared Variables from other modules - potet
      INTEGER, ALLOCATABLE :: Transp_on(:)
      REAL, ALLOCATABLE :: Potet(:)
!   Declared Variables from other modules - intcp
      REAL, ALLOCATABLE :: Hru_intcpevap(:)
!   Declared Variables from other modules - precip
      REAL, ALLOCATABLE :: Hru_ppt(:)
!   Declared Variables from other modules - mf2prms
      REAL, ALLOCATABLE :: Gw2sm_grav(:)
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Soil_type(:), Cov_type(:), Hru_type(:)
      INTEGER, ALLOCATABLE :: Gvr_hru_id(:)
      REAL, ALLOCATABLE :: Gvr_hru_pct(:)
      REAL, ALLOCATABLE :: Soil_moist_max(:), Soil_rechr_max(:)
      REAL, ALLOCATABLE :: Soil_moist_init(:), Ssstor_init(:)
      REAL, ALLOCATABLE :: Sat_threshold(:), Pref_flow_den(:)
      REAL, ALLOCATABLE :: Fastcoef_lin(:), Fastcoef_sq(:)
      REAL, ALLOCATABLE :: Slowcoef_lin(:), Slowcoef_sq(:)
      REAL, ALLOCATABLE :: Ssr2gw_rate(:), Ssrmax_coef(:), Ssr2gw_exp(:)
      REAL, ALLOCATABLE :: Soil_rechr_init(:), Soil2gw_max(:)
      REAL, ALLOCATABLE :: Hru_area(:)
      END MODULE SOILZONE_GSF

!***********************************************************************
!     Main soilzone routine
!***********************************************************************
      INTEGER FUNCTION soilzone_gsflow(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: szdecl, szinit, szrun
!***********************************************************************
      soilzone_gsflow = 0

      IF ( Arg.EQ.'run' ) THEN
        soilzone_gsflow = szrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        soilzone_gsflow = szdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        soilzone_gsflow = szinit()
      ENDIF

      END FUNCTION soilzone_gsflow

!***********************************************************************
!     szdecl - set up parameters for soil zone computations
!   Declared Parameters
!     sat_threshold, ssstor_init fastcoef_lin, fastcoef_sq
!     ssr2gw_rate, ssrmax_coef, ssr2gw_exp, soil2gw_max, soil_type
!     soil_rechr_max, soil_rechr_init, soil_moist_max, soil_moist_init
!     pref_flow_den, slowcoef_lin, cov_type
!     hru_area, slowcoef_sq, hru_type, gvr_hru_id, gvr_hru_pct
!***********************************************************************
      INTEGER FUNCTION szdecl()
      USE SOILZONE_GSF
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      szdecl = 1

      IF ( declmodule(
     +'$Id: soilzone_gsflow.f 3928 2008-03-06 17:03:55Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ncascade = getdim('ncascade')
      IF ( Ncascade.EQ.-1 ) RETURN

      Nsegment = getdim('nsegment')
      IF ( Nsegment.EQ.-1 ) RETURN
      Nsegmentp1 = Nsegment + 1

      Nssr = getdim('nssr')
      IF ( Nssr.EQ.-1 ) RETURN
      IF ( Nhru.NE.Nssr ) THEN
        PRINT *, 'error, nhru must equal nssr', Nhru, Nssr
        RETURN
      ENDIF

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell.EQ.-1 ) RETURN

! Declare Variables
      ALLOCATE (Gravity_stor_res(Nhrucell))
      IF ( declvar('soilzone', 'gravity_stor_res', 'nhrucell', Nhrucell,
     +     'real', 'Storage in each gravity-flow reservoir',
     +     'inches',
     +     Gravity_stor_res).NE.0 ) RETURN

      ALLOCATE (Sm2gw_grav(Nhrucell))
      IF ( declvar('soilzone', 'sm2gw_grav', 'nhrucell', Nhrucell,
     +     'real',
     +     'Drainage from each gravity reservoir to each MODFLOW cell',
     +     'inches',
     +     Sm2gw_grav).NE.0 ) RETURN

      ALLOCATE (Sm2gw_grav_old(Nhrucell))
      IF ( declvar('soilzone', 'sm2gw_grav_old', 'nhrucell', Nhrucell,
     +     'real',
     +     'Drainage from each gravity reservoir to each MODFLOW cell'//
     +     ' from the previous iteration',
     +     'inches',
     +     Sm2gw_grav_old).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_infil_tot', 'one', 1, 'real',
     +     'Basin average infiltration total into capillary reservoirs',
     +     'inches',
     +     Basin_infil_tot).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_pref_flow_in', 'one', 1, 'real',
     +     'Basin average infiltration to preferential-flow reservoirs',
     +     'inches',
     +     Basin_pref_flow_in).NE.0 ) RETURN

      ALLOCATE (Infil_tot(Nhru))
      IF ( declvar('soilzone', 'infil_tot', 'nhru', Nhru, 'real',
     +     'Soil infiltration total (precip, melt, cascade)',
     +     'inches',
     +     Infil_tot).NE.0 ) RETURN

      ALLOCATE (Soil_moist_tot(Nhru))
      IF ( declvar('soilzone', 'soil_moist_tot', 'nhru', Nhru, 'real',
     +     'Total soil moisture (soil_moist + ssres_stor)',
     +     'inches',
     +     Soil_moist_tot).NE.0 ) RETURN

      ALLOCATE (Soil_moist_pct(Nhru))
      IF ( declvar('soilzone', 'soil_moist_pct', 'nhru', Nhru, 'real',
     +     'Percent soil moisture'//
     +     ' (soil_moist + ssres_stor)/soil_zone_max',
     +     'decimal fraction',
     +     Soil_moist_pct).NE.0 ) RETURN

      ALLOCATE (Ssr_to_gw(Nssr))
      IF ( declvar('soilzone', 'ssr_to_gw', 'nssr', Nssr, 'real',
     +     'HRU average drainage from each gravity reservoir'//
     +     ' storage to its associated ground-water reservoir',
     +     'inches',
     +     Ssr_to_gw).NE.0 ) RETURN

      ALLOCATE (Ssres_stor(Nssr))
      IF ( declvar('soilzone', 'ssres_stor', 'nssr', Nssr, 'real',
     +     'Storage in each subsurface reservoir',
     +     'inches',
     +     Ssres_stor).NE.0 ) RETURN

      ALLOCATE (Ssres_in(Nssr))
      IF ( declvar('soilzone', 'ssres_in', 'nssr', Nssr, 'real',
     +     'Sum of inflow to subsurface reservoir from all'//
     +     ' associated HRUs',
     +     'inches',
     +     Ssres_in).NE.0 ) RETURN

      ALLOCATE (Ssres_flow(Nssr))
      IF ( declvar('soilzone', 'ssres_flow', 'nssr', Nssr, 'real',
     +     'Outflow from each subsurface reservoir',
     +     'inches',
     +     Ssres_flow).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_sm2gvr', 'one', 1, 'real',
     +     'Basin weighted average for soil_to_ssr',
     +     'inches',
     +     Basin_sm2gvr).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_dnflow', 'one', 1, 'real',
     +     'Basin weighted average for cascading down slope flow',
     +     'inches',
     +     Basin_dnflow).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_gvr2sm', 'one', 1, 'real',
     +     'Basin weighted average for gravity flow to soil moist',
     +     'inches',
     +     Basin_gvr2sm).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_gvr2pfr', 'one', 1, 'real',
     +     'Basin weighted average for gravity flow to preferential'//
     +     ' flow reserviors',
     +     'inches',
     +     Basin_gvr2pfr).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_slowflow', 'one', 1, 'real',
     +     'Basin weighted average for slow interflow to streams',
     +     'inches',
     +     Basin_slowflow).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_prefflow', 'one', 1, 'real',
     +     'Basin weighted average for fast interflow to streams',
     +     'inches',
     +     Basin_prefflow).NE.0 ) RETURN

      ALLOCATE (Gvr2sm(Nhru))
      IF ( declvar('soilzone', 'gvr2sm', 'nhru', Nhru, 'real',
     +     'gravity flow to soil moist replenishment for each HRU',
     +     'inches',
     +     Gvr2sm).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_ssflow', 'one', 1, 'real',
     +     'Basin weighted average for subsurface reservoir outflow',
     +     'inches',
     +     Basin_ssflow).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_ssstor', 'one', 1, 'real',
     +     'Basin weighted average for subsurface reservoir storage',
     +     'inches',
     +     Basin_ssstor).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_ssin', 'one', 1, 'real',
     +     'Basin weighted average for inflow to subsurface reservoirs',
     +     'inches',
     +     Basin_ssin).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_slstor', 'one', 1, 'real',
     +     'Basin weighted average for subsurface reservoir'//
     +     ' storage for slow interflow',
     +     'inches',
     +     Basin_slstor).NE.0 ) RETURN

      ALLOCATE (Dunnian_flow(Nhru))
      IF ( declvar('soilzone', 'dunnian_flow', 'nhru', Nhru, 'real',
     +     'Dunnian runoff added to surface runoff',
     +     'inches',
     +     Dunnian_flow).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_dunnian', 'one', 1, 'real',
     +     'Basin weighted average dunnian runoff',
     +     'inches',
     +     Basin_dunnian).NE.0 ) RETURN

      ALLOCATE (Soil_rechr(Nhru))
      IF ( declvar('soilzone', 'soil_rechr', 'nhru', Nhru, 'real',
     +     'Current moisture content of soil recharge zone, ie, the'//
     +     ' portion of the soil profile from which evaporation can'//
     +     ' take place',
     +     'inches',
     +     Soil_rechr).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_soil_rechr', 'one', 1, 'real',
     +     'Basin area weighted average for soil_rechr',
     +     'inches',
     +     Basin_soil_rechr).NE.0 ) RETURN

      ALLOCATE (Soil_moist(Nhru))
      IF ( declvar('soilzone', 'soil_moist', 'nhru', Nhru, 'real',
     +     'Current moisture content of soil profile to the depth'//
     +     ' of the rooting zone of the major vegetation type on the'//
     +     ' HRU',
     +     'inches',
     +     Soil_moist).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_soil_moist', 'one', 1, 'real',
     +     'Basin area weighted average for soil_moist',
     +     'inches',
     +     Basin_soil_moist).NE.0 ) RETURN

      ALLOCATE (Perv_actet(Nhru))
      IF ( declvar('soilzone', 'perv_actet', 'nhru', Nhru, 'real',
     +     'Actual evapotranspiration from pervious areas of HRU',
     +     'inches',
     +     Perv_actet).NE.0 ) RETURN

      ALLOCATE (Hru_actet(Nhru))
      IF ( declvar('soilzone', 'hru_actet', 'nhru', Nhru, 'real',
     +     'Actual evapotranspiration on HRU, pervious + impervious',
     +     'inches',
     +     Hru_actet).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_actet', 'one', 1, 'real',
     +     'Basin area weighted average of hru_actet',
     +     'inches',
     +     Basin_actet).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_perv_et', 'one', 1, 'real',
     +     'Basin area weighted average of pervious area ET',
     +     'inches',
     +     Basin_perv_et).NE.0 ) RETURN

      ALLOCATE (Soil_to_gw(Nhru))
      IF ( declvar('soilzone', 'soil_to_gw', 'nhru', Nhru, 'real',
     +     'Portion of excess soil water from an HRU that flows to'//
     +     ' its associated groundwater reservoir',
     +     'inches',
     +     Soil_to_gw).NE.0 ) RETURN

      ALLOCATE (Soil_to_ssr(Nhru))
      IF ( declvar('soilzone', 'soil_to_ssr', 'nhru', Nhru, 'real',
     +     'Portion of excess soil water from an HRU that flows to'//
     +     ' its associated subsurface reservoir',
     +     'inches',
     +     Soil_to_ssr).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_soil_to_gw', 'one', 1, 'real',
     +     'Basin average excess soil water that flows directly to'//
     +     ' groundwater reservoirs',
     +     'inches',
     +     Basin_soil_to_gw).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_sz2gw', 'one', 1, 'real',
     +     'Basin average drainage from soil added to groundwater',
     +     'inches',
     +     Basin_sz2gw).NE.0 ) RETURN

      ALLOCATE (Upslope_interflow(Nhru))
      IF ( declvar('soilzone', 'upslope_interflow', 'nhru', Nhru,
     +     'real',
     +     'Interflow received from HRUs up slope',
     +     'inches',
     +     Upslope_interflow).NE.0 ) RETURN

      ALLOCATE (Upslope_dunnianflow(Nhru))
      IF ( declvar('soilzone', 'upslope_dunnianflow', 'nhru', Nhru,
     +     'real', 'Dunnian runoff received from HRUs up slope',
     +     'inches',
     +     Upslope_dunnianflow).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_pref_stor', 'one', 1, 'real',
     +     'Basin average storage in the preferential-flow reservoir',
     +     'inches',
     +     Basin_pref_stor).NE.0 ) RETURN
 
      ALLOCATE (Pref_flow_in(Nhru))
      IF ( declvar('soilzone', 'pref_flow_in', 'nhru', Nhru, 'real',
     +     'Infiltration as a result of preferential-flow pore space',
     +     'inches',
     +     Pref_flow_in).NE.0 ) RETURN

      ALLOCATE (Slow_flow(Nhru))
      IF ( declvar('soilzone', 'slow_flow', 'nhru', Nhru, 'real',
     +     'Slow interflow',
     +     'inches',
     +     Slow_flow).NE.0 ) RETURN

      ALLOCATE (Slow_stor(Nhru))
      IF ( declvar('soilzone', 'slow_stor', 'nhru', Nhru, 'real',
     +     'Gravity flow reservoir storage for slow interflow',
     +     'inches',
     +     Slow_stor).NE.0 ) RETURN

      ALLOCATE (Pref_flow_stor(Nhru))
      IF ( declvar('soilzone', 'pref_flow_stor', 'nhru', Nhru, 'real',
     +     'Soil storage based on preferential-flow pore density',
     +     'inches',
     +     Pref_flow_stor).NE.0 ) RETURN

      ALLOCATE (Pref_flow(Nhru))
      IF ( declvar('soilzone', 'pref_flow', 'nhru', Nhru, 'real',
     +     'Interflow as a result of preferential-flow pore space',
     +     'inches',
     +     Pref_flow).NE.0 ) RETURN

      ALLOCATE (Pref_flow_thrsh(Nhru))
      IF ( declvar('soilzone', 'pref_flow_thrsh', 'nhru', Nhru, 'real',
     +     'Soil storage threshold above which quick interflow occurs',
     +     'inches',
     +     Pref_flow_thrsh).NE.0 ) RETURN

      ALLOCATE (Pref_flow_max(Nhru))
      IF ( declvar('soilzone', 'pref_flow_max', 'nhru', Nhru, 'real',
     +     'Depth of preferential-flow reservoir (upper portion of'//
     +     ' gravity flow reservoir where quick interflow occurs)',
     +     'inches',
     +     Pref_flow_max).NE.0 ) RETURN

      ALLOCATE (Soil_zone_max(Nhru))
      IF ( declvar('soilzone', 'soil_zone_max', 'nhru', Nhru, 'real',
     +     'Maximum water depth of soil zone',
     +     'inches',
     +     Soil_zone_max).NE.0 ) RETURN

      ALLOCATE (Lakein_sz(Nhru))
      IF ( declvar('soilzone', 'lakein_sz', 'nhru', Nhru, 'real',
     +     'Interflow & Dunnian runoff lakes receive from HRUs upslope',
     +     'inches',
     +     Lakein_sz).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_lakeinsz', 'one', 1, 'real',
     +     'Basin area weighted average of lake inflow from soil zone',
     +     'inches',
     +     Basin_lakeinsz).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_lakeevap', 'one', 1, 'real',
     +     'Basin area weighted average of lake evaporation',
     +     'inches',
     +     Basin_lakeevap).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_lakeprecip', 'one', 1, 'real',
     +     'Basin area weighted average of precipitation on lakes',
     +     'inches',
     +     Basin_lakeprecip).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_szfarflow', 'one', 1, 'real',
     +     'Basin area weighted average of far-field flow from soil',
     +     'inches',
     +     Basin_szfarflow).NE.0 ) RETURN

! Declare Parameters
      ALLOCATE (Gvr_hru_id(Nhrucell))
      IF ( declparam('soilzone', 'gvr_hru_id', 'nhrucell', 'integer',
     +     '1', 'bounded', 'nhru',
     +     'Corresponding HRU id of each GVR',
     +     'Index of the HRU assocated with each gravity reservoir',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_pct(Nhrucell))
      IF ( declparam('soilzone', 'gvr_hru_pct', 'nhrucell', 'real',
     +     '0.0', '0.0', '1.0',
     +     'Proportion of the HRU associated with each GVR',
     +     'Proportion of the HRU area associated with each gravity'//
     +     ' reservoir',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Slowcoef_lin(Nhru))
      IF ( declparam('soilzone', 'slowcoef_lin', 'nhru', 'real',
     +     '0.015', '0.0', '1.0',
     +     'Linear gravity-flow reservoir routing coefficient',
     +     'Coefficient to route gravity-flow storage down slope'//
     +     ' using the following equation: '//
     +     ' ssres_flow = slowcoef_lin * ssres_stor +'//
     +     ' slowcoef_sq * ssres_stor**2',
     +     '1/day').NE.0 ) RETURN

      ALLOCATE (Slowcoef_sq(Nhru))
      IF ( declparam('soilzone', 'slowcoef_sq', 'nhru', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Non-linear gravity-flow reservoir routing coefficient',
     +     'Coefficient to route gravity-flow storage down slope'//
     +     ' using the following equation: '//
     +     ' ssres_flow = slowcoef_lin * ssres_stor +'//
     +     ' slowcoef_sq * ssres_stor**2',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Pref_flow_den(Nhru))
      IF ( declparam('soilzone', 'pref_flow_den', 'nhru', 'real',
     +     '0.2', '0.0', '1.0',
     +     'Preferential-flow pore density',
     +     'Preferential-flow pore density',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Soil_rechr_max(Nhru))
      IF ( declparam('soilzone', 'soil_rechr_max', 'nhru', 'real',
     +     '2.', '0.', '10.',
     +     'Maximum value for soil recharge zone',
     +     'Maximum value for soil recharge zone (upper portion'//
     +     ' of soil_moist where losses occur as both evaporation'//
     +     ' and transpiration).  Must be less than or equal to'//
     +     ' soil_moist',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Soil_rechr_init(Nhru))
      IF ( declparam('soilzone', 'soil_rechr_init', 'nhru', 'real',
     +     '1.', '0.', '10.',
     +     'Initial value of water for soil recharge zone',
     +     'Initial value for soil recharge zone (upper part of'//
     +   ' soil_moist).  Must be less than or equal to soil_moist_init',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Soil_moist_max(Nhru))
      IF ( declparam('soilzone', 'soil_moist_max', 'nhru', 'real',
     +     '6.', '0.', '20.',
     +     'Maximum value of water for soil zone',
     +     'Maximum available water holding capacity of soil profile.'//
     +     ' Soil profile is surface to bottom of rooting zone',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Soil_moist_init(Nhru))
      IF ( declparam('soilzone', 'soil_moist_init', 'nhru', 'real',
     +     '3.', '0.', '20.',
     +     'Initial values of water for soil zone',
     +     'Initial value of available water in soil profile',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Soil2gw_max(Nhru))
      IF ( declparam('soilzone', 'soil2gw_max', 'nhru', 'real',
     +     '0.', '0.', '5.0',
     +     'Maximum value for soil water excess to groundwater',
     +     'The maximum amount of the soil water excess for an HRU'//
     +     ' that is routed directly to the associated groundwater'//
     +     ' reservoir each day',
     +     ' inches').NE.0 ) RETURN

      ALLOCATE (Soil_type(Nhru))
      IF ( declparam('soilzone', 'soil_type', 'nhru', 'integer',
     +     '2', '1', '3',
     +     'HRU soil type', 'HRU soil type (1=sand; 2=loam; 3=clay)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Sat_threshold(Nhru))
      IF ( declparam('soilzone', 'sat_threshold', 'nhru', 'real',
     +     '999.0', '1.0', '999.0',
     +     'Soil saturation threshold, above field-capacity threshold',
     +     'Soil saturation threshold, above field-capacity threshold',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Ssstor_init(Nssr))
      IF ( declparam('soilzone', 'ssstor_init', 'nssr', 'real',
     +     '0.', '0.', '20.',
     +     'Initial storage in each subsurface reservoir',
     +     'Initial storage in each subsurface reservoir;'//
     +     ' estimated based on observed flow',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Fastcoef_lin(Nhru))
      IF ( declparam('soilzone', 'fastcoef_lin', 'nhru', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Linear preferential-flow routing coefficient',
     +     'Coefficient to route preferential-flow storage down slope'//
     +     ' using the following equation: '//
     +     ' pref_flow = fastcoef_lin * pref_flow_stor +'//
     +     ' fastcoef_sq * pref_flow_stor**2',
     +     '1/day').NE.0 ) RETURN

      ALLOCATE (Fastcoef_sq(Nhru))
      IF ( declparam('soilzone', 'fastcoef_sq', 'nhru', 'real',
     +     '0.8', '0.0', '1.0',
     +     'Non-linear preferential-flow routing coefficient',
     +     'Coefficient to route preferential-flow storage down slope'//
     +     ' using the following equation: '//
     +     ' pref_flow = fastcoef_lin * pref_flow_stor +'//
     +     ' fastcoef_sq * pref_flow_stor**2',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Ssr2gw_rate(Nssr))
      IF ( declparam('soilzone', 'ssr2gw_rate', 'nssr', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Coefficient in equation used to route water from the'//
     +     ' subsurface reservoirs to the groundwater reservoirs: '//
     +     ' ssr_to_gw = ssr2gw_rate *'//
     +     ' ((ssres_stor / ssrmax_coef)**ssr2gw_exp)',
     +     '1/day').NE.0 ) RETURN

      ALLOCATE (Ssrmax_coef(Nssr))
      IF ( declparam('soilzone', 'ssrmax_coef', 'nssr', 'real',
     +     '1.0', '1.0', '20.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Coefficient in equation used to route water from the'//
     +     ' subsurface reservoirs to the groundwater reservoirs: '//
     +     ' ssr_to_gw = ssr2gw_rate *'//
     +     ' ((ssres_stor / ssrmax_coef)**ssr2gw_exp);'//
     +     ' recommended value is 1.0',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Ssr2gw_exp(Nssr))
      IF ( declparam('soilzone', 'ssr2gw_exp', 'nssr', 'real',
     +     '1.0', '0.0', '3.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Coefficient in equation used to route water from the'//
     +     ' subsurface reservoirs to the groundwater reservoirs: '//
     +     ' ssr_to_gw = ssr2gw_rate * '//
     +     ' ((ssres_stor / ssrmax_coef)**ssr2gw_exp);'//
     +     ' recommended value is 1.0',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Cov_type(Nhru))
      IF ( declparam('soilzone', 'cov_type', 'nhru', 'integer',
     +     '3', '0', '3',
     +     'Cover type designation for HRU',
     +     'Vegetation cover type designation for HRU'//
     +     ' (0=bare soil; 1=grasses; 2=shrubs; 3=trees)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('soilzone', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('soilzone', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '2',
     +     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     +     'none').NE.0 ) RETURN

! Allocate arrays for local and variables from other modules
      ALLOCATE (Hru_perv(Nhru))
      ALLOCATE (Ncascade_hru(Nhru), Hru_route_order(Nhru))
      ALLOCATE (Snowcov_area(Nhru), Snow_evap(Nhru), Hru_ppt(Nhru))
      ALLOCATE (Transp_on(Nhru), Potet(Nhru), Hru_intcpevap(Nhru))
      ALLOCATE (Infil(Nhru), Hru_impervevap(Nhru), Sroff(Nhru))
      ALLOCATE (Replenish_pct(Nhru), Gvr2pfr(Nhru))
      ALLOCATE (Hru_percent_perv(Nhru))
      ALLOCATE (It0_pref_flow_stor(Nhru), It0_ssres_stor(Nhru))
      ALLOCATE (It0_soil_rechr(Nhru), It0_soil_moist(Nhru))
      ALLOCATE (It0_gravity_stor_res(Nhrucell), It0_sroff(Nhru))
      ALLOCATE (It0_slow_stor(Nhru), Gw2sm_grav(Nhrucell))
      ALLOCATE (Strm_seg_in(Nsegment), It0_strm_seg_in(Nsegment))

      szdecl = 0
      END FUNCTION szdecl

!***********************************************************************
!     szinit - Initialize soilzone module - get parameter values,
!              set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION szinit()
      USE SOILZONE_GSF
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC MIN
! Local Variables
      INTEGER :: i, ii
!***********************************************************************
      szinit = 1

      IF ( getparam('soilzone', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('soilzone', 'slowcoef_lin', Nhru, 'real',
     +     Slowcoef_lin).NE.0 ) RETURN

      IF ( getparam('soilzone', 'slowcoef_sq', Nhru, 'real',
     +     Slowcoef_sq).NE.0 ) RETURN

      IF ( getparam('soilzone', 'pref_flow_den', Nhru, 'real',
     +     Pref_flow_den).NE.0 ) RETURN

      IF ( getparam('soilzone', 'ssstor_init', Nssr, 'real',
     +     Ssstor_init).NE.0 ) RETURN

      IF ( getparam('soilzone', 'fastcoef_lin', Nhru, 'real',
     +     Fastcoef_lin).NE.0 ) RETURN

      IF ( getparam('soilzone', 'fastcoef_sq', Nhru, 'real',
     +     Fastcoef_sq).NE.0 ) RETURN

      IF ( getparam('soilzone', 'ssr2gw_rate', Nssr, 'real',
     +     Ssr2gw_rate).NE.0 ) RETURN

      IF ( getparam('soilzone', 'ssr2gw_exp', Nssr, 'real', Ssr2gw_exp)
     +     .NE.0 ) RETURN

      IF ( getparam('soilzone', 'ssrmax_coef', Nssr, 'real',
     +     Ssrmax_coef).NE.0 ) RETURN

      IF ( getparam('soilzone', 'sat_threshold', Nhru, 'real',
     +     Sat_threshold).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_moist_max', Nhru, 'real',
     +     Soil_moist_max).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_moist_init', Nhru, 'real',
     +     Soil_moist_init).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_rechr_max', Nhru, 'real',
     +     Soil_rechr_max).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_rechr_init', Nhru, 'real',
     +     Soil_rechr_init).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil2gw_max', Nhru, 'real',
     +     Soil2gw_max).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_type', Nhru, 'integer', Soil_type)
     +     .NE.0 ) RETURN

      IF ( getparam('soilzone', 'cov_type', Nhru, 'integer', Cov_type)
     +     .NE.0 ) RETURN

      IF ( getparam('soilzone', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_perv', Nhru, 'real', Hru_perv)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'ncascade_hru', Nhru, 'integer',
     +     Ncascade_hru).NE.0 ) RETURN

      IF ( getvar('basin', 'hru_percent_perv', Nhru, 'real',
     +     Hru_percent_perv).NE.0 ) RETURN

      IF ( getparam('soilzone', 'gvr_hru_id', Nhrucell, 'integer',
     +     Gvr_hru_id).NE.0 ) RETURN

      IF ( getparam('soilzone', 'gvr_hru_pct', Nhrucell, 'real',
     +     Gvr_hru_pct).NE.0 ) RETURN

      IF ( getvar('gsflow_prms', 'model', 1, 'integer', Model)
     +     .NE.0 ) RETURN

      IF ( getstep().EQ.0 ) THEN
! initialize scalers
        Basin_ssflow = 0.0
        Basin_sm2gvr = 0.0
        Basin_dnflow = 0.0
        Basin_gvr2sm = 0.0
        Basin_ssin = 0.0
        Basin_ssstor = 0.0
        Basin_slstor = 0.0
        Basin_sz2gw = 0.0
        Basin_soil_to_gw = 0.0
        Basin_dunnian = 0.0
        Basin_soil_rechr = 0.0
        Basin_perv_et = 0.0
        Basin_actet = 0.0
        Basin_pref_stor = 0.0
        Basin_infil_tot = 0.0
        Basin_pref_flow_in = 0.0
        Basin_gvr2pfr = 0.0
        Basin_slowflow = 0.0
        Basin_prefflow = 0.0
        Basin_lakeevap = 0.0
        Basin_lakeprecip = 0.0
        Basin_lakeinsz = 0.0
        Basin_szfarflow = 0.0
! initialize arrays (dimensioned Nhru)
        Ssr_to_gw = 0.0
        Ssres_in = 0.0
        Ssres_flow = 0.0
        Dunnian_flow = 0.0
        Soil_moist_tot = 0.0
        Soil_moist_pct = 0.0
        Soil_to_gw = 0.0
        Soil_to_ssr = 0.0
        Hru_actet = 0.0
        Perv_actet = 0.0
        Upslope_interflow = 0.0
        Upslope_dunnianflow = 0.0
        Infil_tot = 0.0
        Pref_flow_in = 0.0
        Slow_flow = 0.0
        Pref_flow = 0.0
        Gvr2sm = 0.0
        Lakein_sz = 0.0
! do only once so restart uses saved values
        Ssres_stor = Ssstor_init
        Soil_rechr = Soil_rechr_init
        Soil_moist = Soil_moist_init
! initialize arrays (dimensioned Nhrucell)
        IF ( Model.EQ.1 ) THEN
          Sm2gw_grav = 0.0
          Sm2gw_grav_old = 0.0
          Gw2sm_grav = 0.0
          DO i = 1, Nhrucell
            Gravity_stor_res(i) = Ssstor_init(Gvr_hru_id(i))
          ENDDO
        ENDIF
      ENDIF

      Basin_soil_moist = 0.0

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        !sanity checks
        IF ( Soil_rechr(i).GT.Soil_moist(i) ) THEN
          PRINT *, 'HRU:', i, Soil_rechr(i), Soil_moist(i),
     +          ' soil_rechr > soil_moist, soil_rechr set to soil_moist'
          Soil_rechr(i) = Soil_moist(i)
        ENDIF
        IF ( Soil_rechr_max(i).GT.Soil_moist_max(i) ) THEN
          PRINT *, 'HRU:', i, Soil_rechr_max(i), Soil_moist_max(i),
     +          ' soil_rechr_max > soil_moist_max,',
     +          ' soil_rechr_max set to soil_moist_max'
          Soil_rechr_max(i) = Soil_moist_max(i)
        ENDIF
        IF ( Soil_rechr(i).GT.Soil_rechr_max(i) ) THEN
          PRINT *, 'HRU:', i, Soil_rechr(i), Soil_rechr_max(i),
     +          ' soil_rechr > soil_rechr_max, soil_rechr set to max'
          Soil_rechr(i) = Soil_rechr_max(i)
        ENDIF
        IF ( Soil_moist(i).GT.Soil_moist_max(i) ) THEN
          PRINT *, 'HRU:', i, Soil_moist(i), Soil_moist_max(i),
     +          ' soil_moist > soil_moist_max, soil_moist set to max'
          Soil_moist(i) = Soil_moist_max(i)
        ENDIF
        IF ( Ssres_stor(i).GT.Sat_threshold(i) ) THEN
          PRINT *, 'HRU:', i, Ssres_stor(i), Sat_threshold(i),
     +          ' ssres_stor > sat_threshold, ssres_stor set to max'
          Ssres_stor(i) = Sat_threshold(i)
        ENDIF
        IF ( Hru_type(i).EQ.2 .OR. Hru_perv(i).LT.NEARZERO ) THEN
          Soil_rechr(i) = 0.0
          Soil_moist(i) = 0.0
          Replenish_pct(i) = 0.0
          Ssres_stor(i) = 0.0
          IF ( Pref_flow_den(i).GT.0.0 ) THEN
            Pref_flow_den(i) = 0.0
            PRINT *, 'HRU:', i, 'Pref_flow_den must be 0.0 for lakes'
          ENDIF
        ELSE
          Replenish_pct(i) = Soil_rechr_max(i)/Soil_moist_max(i)
        ENDIF
        Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*Hru_perv(i)
        Pref_flow_thrsh(i) = Sat_threshold(i)*(1.0-Pref_flow_den(i))
        Pref_flow_max(i) = Sat_threshold(i) - Pref_flow_thrsh(i)
        Soil_zone_max(i) = Sat_threshold(i) + Soil_moist_max(i)
        Slow_stor(i) = MIN(Ssres_stor(i), Pref_flow_thrsh(i))
        Pref_flow_stor(i) = Ssres_stor(i) - Slow_stor(i)
        Basin_slstor = Basin_slstor + Slow_stor(i)*Hru_area(i)
        Basin_ssstor = Basin_ssstor + Ssres_stor(i)*Hru_area(i)
      ENDDO
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Last_ssstor = Basin_ssstor
      Last_soil_moist = Basin_soil_moist

      Kkiter = 1 ! set for PRMS-only mode

      IF ( Prt_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='soilzone_gsflow.wbal')
        WRITE (BALUNT, 9001)
      ENDIF

      szinit = 0

 9001 FORMAT ('    Date     Water Bal     bsmbal    last SM  soilmoist',
     +        '  last stor    SS stor    perv ET      sz2gw  interflow',
     +        '    soil2gw    Dunnian    soil in   lakeinsz   downflow',
     +        '     gvr2sm    farflow  iteration')

      END FUNCTION szinit

!***********************************************************************
!     szrun - Does soil water balance for each HRU, adds in infiltration
!             then computes actual et and apportions remainder between
!             recharge of soil moisture, soil storage available for
!             interflow, excess routed to stream,
!             and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION szrun()
      USE SOILZONE_GSF
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC SNGL, ABS, MAX, MIN
      EXTERNAL compute_soilmoist, compute_szactet, compute_cascades
      EXTERNAL compute_interflow, comp_inter_gwflow
      EXTERNAL compute_gravflow
! Local Variables
      INTEGER :: i, k
      REAL :: dunnianflw, ets, last_ss, cap_waterin, waterin
      REAL :: etr, avail_potet, soil_lower, perv_area, harea, soil_in
      REAL :: basin_bal, bsmbal, soilbal, last_sm, capacity, interflow
      REAL :: gvrbal, dnslowflow, dnpreflow, dndunn, gwin
      REAL :: farflow_slow, farflow_pref, farflow_dunn, err
      REAL :: perv_pct, pref_flow_infil
      DOUBLE PRECISION :: tstep
!***********************************************************************
      szrun = 1

      IF ( Model.EQ.1 ) THEN
        IF ( getvar('gsflow', 'KKITER', 1, 'integer', Kkiter)
     +       .NE.0 ) RETURN
      ENDIF

      IF ( Kkiter.EQ.1 ) THEN
        CALL dattim('now', Nowtime)
        Month = Nowtime(2)
        Day = Nowtime(3)

!*****Ts= timesteps in a day, Td = timestep in days
        tstep = deltim()
        Ts = SNGL(24.0D0/deltim())
        Td = SNGL(deltim()/24.0D0)

! convert timestep in hours to seconds
! Cfs_conv converts acre-inches per timestep to cfs
        Cfs_conv = SNGL(43560.0D0/12.0D0/(tstep*3600.0D0))


        IF ( getvar('snow', 'snowcov_area', Nhru, 'real', Snowcov_area)
     +       .NE.0 ) RETURN

        IF ( getvar('potet', 'transp_on', Nhru, 'integer', Transp_on)
     +       .NE.0 ) RETURN

        IF ( getvar('intcp', 'hru_intcpevap', Nhru, 'real',
     +       Hru_intcpevap).NE.0 ) RETURN

        IF ( getvar('snow', 'snow_evap', Nhru, 'real', Snow_evap)
     +       .NE.0 ) RETURN

        IF ( getvar('potet', 'potet', Nhru, 'real', Potet)
     +       .NE.0 ) RETURN

        IF ( getvar('srunoff', 'infil', Nhru, 'real', Infil)
     +       .NE.0 ) RETURN

        IF ( getvar('srunoff', 'hru_impervevap', Nhru, 'real',
     +       Hru_impervevap).NE.0 ) RETURN

        IF ( getvar('srunoff', 'sroff', Nhru, 'real', Sroff)
     +       .NE.0 ) RETURN

        IF ( getvar('srunoff', 'strm_seg_in', Nsegment, 'real',
     +       Strm_seg_in).NE.0 ) RETURN

        IF ( getvar('srunoff', 'strm_farfield', 1, 'real',
     +       Strm_farfield).NE.0 ) RETURN

!rsr, get these in case states have been updated
        IF ( getvar('basin', 'hru_perv', Nhru, 'real', Hru_perv)
     +       .NE.0 ) RETURN

        IF ( getvar('precip', 'hru_ppt', Nhru, 'real', Hru_ppt)
     +       .NE.0 ) RETURN

        IF ( tstep.LT.23.999D0 ) THEN
          Daily = 0
        ELSE
          Daily = 1
        ENDIF

        Last_soil_moist = Basin_soil_moist
        Last_ssstor = Basin_ssstor

! It0 variables used with MODFLOW integration to save iteration states.
        IF ( Model.EQ.1 ) THEN
          It0_soil_rechr = Soil_rechr
          It0_soil_moist = Soil_moist
          It0_pref_flow_stor = Pref_flow_stor
          It0_slow_stor = Slow_stor
          It0_ssres_stor = Ssres_stor
          It0_gravity_stor_res = Gravity_stor_res
          It0_sroff = Sroff
          It0_strm_seg_in = Strm_seg_in
          It0_strm_farfield = Strm_farfield
!rsr, use value from last iteration for each time step instead of 0.0
          IF ( getvar('mf2prms', 'gw2sm_grav', Nhrucell, 'real',
     +         Gw2sm_grav).NE.0 ) RETURN
        ENDIF
      ELSE
        Soil_rechr = It0_soil_rechr
        Soil_moist = It0_soil_moist
        Ssres_stor = It0_ssres_stor
        Pref_flow_stor = It0_pref_flow_stor
        Slow_stor = It0_slow_stor
        Gravity_stor_res = It0_gravity_stor_res
        Sroff = It0_sroff
        Strm_seg_in = It0_strm_seg_in
        Strm_farfield = It0_strm_farfield
!rsr, use value from last iteration for each time step instead of 0.0
        IF ( getvar('mf2prms', 'gw2sm_grav', Nhrucell, 'real',
     +       Gw2sm_grav).NE.0 ) RETURN
      ENDIF

      IF ( Ncascade.GT.0 ) THEN
        Upslope_interflow = 0.0
        Upslope_dunnianflow = 0.0
        Lakein_sz = 0.0
      ENDIF

      Basin_actet = 0.0
      Basin_soil_moist = 0.0
      Basin_soil_rechr = 0.0
      Basin_perv_et = 0.0
      Basin_lakeevap = 0.0
      Basin_lakeprecip = 0.0
      Basin_lakeinsz = 0.0
      Basin_sz2gw = 0.0
      Basin_soil_to_gw = 0.0
      Basin_ssin = 0.0
      Basin_ssflow = 0.0
      Basin_sm2gvr = 0.0
      Basin_pref_stor = 0.0
      Basin_ssstor = 0.0
      Basin_sroff = 0.0
      Basin_dunnian = 0.0
      basin_bal = 0.0
      soil_in = 0.0
      gwin = 0.0
      Basin_infil_tot = 0.0
      Basin_pref_flow_in = 0.0
      Basin_gvr2sm = 0.0
      Basin_dnflow = 0.0
      Basin_gvr2pfr = 0.0
      Basin_slowflow = 0.0
      Basin_prefflow = 0.0
      Basin_szfarflow = 0.0
      
      IF ( Model.EQ.1 ) THEN
        IF ( Kkiter.GT.1 ) Sm2gw_grav_old = Sm2gw_grav
        Sm2gw_grav = 0.0
      ENDIF

      Soil_to_gw = 0.0
      Soil_to_ssr = 0.0
      Ssr_to_gw = 0.0

      DO k = 1, Active_hrus
        i = Hru_route_order(k)

        harea = Hru_area(i)
        IF ( Hru_type(i).EQ.1 ) THEN
          perv_area = Hru_perv(i)
          perv_pct = Hru_percent_perv(i)
          last_sm = Soil_moist(i)
          last_ss = Ssres_stor(i)
          dnslowflow = 0.0
          dnpreflow = 0.0
          dndunn = 0.0
          farflow_slow = 0.0
          farflow_pref = 0.0
          farflow_dunn = 0.0

!******if cascading flow available from upslope cascades
!****** add soil excess (Dunnian flow) to infiltration
          !infil_tot is the depth in whole HRU
          !infil is for pervious area
          !capillary reservoir for pervious area
          !preferential flow reservoir for whole HRU
          !gravity reservoir for whole HRU
          !upslope flow for whole HRU
          IF ( perv_pct.GT.0.0 ) THEN
            cap_waterin = (Upslope_dunnianflow(i)+Upslope_interflow(i))
     +                    /perv_pct
          ELSE
            cap_waterin = 0.0
          ENDIF
          pref_flow_infil = Infil(i)*Pref_flow_den(i)*perv_pct
          cap_waterin = cap_waterin + Infil(i) -
     +                  Infil(i)*Pref_flow_den(i)
          Infil_tot(i) = cap_waterin*perv_pct
          Basin_infil_tot = Basin_infil_tot + Infil_tot(i)*harea

!******Add infiltration to soil and compute excess

!rsr soil moisture assumed only in perv_area
!rsr soil interflow assumed in perv_area (change from original)

          CALL compute_soilmoist(Td, perv_area, cap_waterin,
     +                           Soil_moist_max(i), Soil_rechr_max(i),
     +                           Soil2gw_max(i), Soil_to_ssr(i),
     +                           Soil_moist(i), Soil_rechr(i),
     +                           soil_lower, Soil_to_gw(i))

          Ssres_in(i) = Soil_to_ssr(i)*perv_pct

! compute interflow and ssr_to_gw
          Slow_flow(i) = 0.0
          Pref_flow(i) = 0.0
          Gvr2sm(i) = 0.0
          Gvr2pfr(i) = 0.0
          capacity = Soil_moist_max(i) - Soil_moist(i)
          IF ( Model.EQ.1 ) THEN
            CALL compute_gravflow(i, capacity, Slowcoef_lin(i),
     +                           Slowcoef_sq(i), Ssr2gw_rate(i),
     +                           Ssr2gw_exp(i), Ssrmax_coef(i),
     +                           Pref_flow_thrsh(i), Td, Soil_to_ssr(i),
     +                           Gvr2pfr(i), Ssr_to_gw(i),
     +                           Slow_flow(i), Slow_stor(i), Gvr2sm(i),
     +                           Soil_to_gw(i), gwin, perv_pct)
            Ssres_in(i) = Ssres_in(i) + gwin
          ELSE
            CALL comp_inter_gwflow(capacity, Slowcoef_lin(i),
     +                            Slowcoef_sq(i), Ssr2gw_rate(i),
     +                            Ssr2gw_exp(i), Ssrmax_coef(i),
     +                            Pref_flow_thrsh(i), Td, Ssres_stor(i),
     +                            Soil_to_ssr(i), Gvr2pfr(i),
     +                            Ssr_to_gw(i), Slow_flow(i),
     +                            Slow_stor(i), Gvr2sm(i), perv_pct)
          ENDIF

! adjust soil moisture and compute ET
          IF ( perv_pct.GT.NEARZERO ) THEN
            Soil_moist(i) = Soil_moist(i) + Gvr2sm(i)/perv_pct
!           IF ( Soil_moist(i).GT.Soil_moist_max(i) )
!    +           PRINT *, 'sm>max', Soil_moist(i), Soil_moist_max(i), i
            Soil_rechr(i) = Soil_rechr(i) +
     +                      Gvr2sm(i)/perv_pct*Replenish_pct(i)
          ENDIF
          Soil_rechr(i) = MIN(Soil_rechr_max(i), Soil_rechr(i))

!******Compute actual evapotranspiration

          avail_potet = Potet(i) - Hru_intcpevap(i) - Snow_evap(i)
     +                  - Hru_impervevap(i)
          IF ( Daily.EQ.0 ) THEN
            IF ( Hru_ppt(i).GT.NEARZERO ) avail_potet = 0.0
          ENDIF
          CALL compute_szactet(perv_area, Soil_moist_max(i),
     +                         Soil_rechr_max(i), Snowcov_area(i),
     +                         Transp_on(i), Cov_type(i), Soil_type(i),
     +                         Soil_moist(i), Soil_rechr(i),
     +                         Perv_actet(i), ets, etr, avail_potet)
          Hru_actet(i) = Perv_actet(i)*perv_pct + Hru_impervevap(i) +
     +                   Hru_intcpevap(i) + Snow_evap(i)

! compute contribution to Dunnian flow, if any
          Pref_flow_in(i) = pref_flow_infil + Gvr2pfr(i)
          Basin_pref_flow_in = Basin_pref_flow_in +
     +                         Pref_flow_in(i)*harea
          Pref_flow_stor(i) = Pref_flow_stor(i) + Pref_flow_in(i)
          Ssres_in(i) = Ssres_in(i) + pref_flow_infil
          dunnianflw = MAX(0.0, Pref_flow_stor(i)-Pref_flow_max(i))
          Pref_flow_stor(i) = Pref_flow_stor(i) - dunnianflw
! compute preferential flow, if any
          IF ( Pref_flow_stor(i).GT.0.0 ) CALL compute_interflow
     +         (Fastcoef_lin(i), Fastcoef_sq(i), 0.0, Pref_flow_stor(i),
     +         Pref_flow(i))
          Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)

! if hru cascades,
! compute interflow and excess flow to each hru or stream
          interflow = Slow_flow(i) + Pref_flow(i)
          Dunnian_flow(i) = dunnianflw
          IF ( Ncascade_hru(i).GT.0 .AND. (interflow.GT.0.0.OR.
     +         dunnianflw.GT.0.0) ) CALL compute_cascades(i,
     +         Ncascade_hru(i), Slow_flow(i), Pref_flow(i),
     +         Dunnian_flow(i), dnslowflow, dnpreflow, dndunn,
     +         farflow_slow, farflow_pref, farflow_dunn)
! treat pref_flow as interflow
          Ssres_flow(i) = Slow_flow(i) + Pref_flow(i)

          Basin_dnflow = Basin_dnflow +
     +                   (dnslowflow+dnpreflow+dndunn)*harea
          Basin_szfarflow = Basin_szfarflow +
     +                    (farflow_slow+farflow_pref+farflow_dunn)*harea

! treat dunnianflw as surface runoff to streams
          Sroff(i) = Sroff(i) + Dunnian_flow(i)

          Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*perv_pct
          Soil_moist_pct(i) = Soil_moist_tot(i)/Soil_zone_max(i)

!rsr 9/21/05 ??? what should be multiplied by harea and perv_area
          Basin_ssstor = Basin_ssstor + Ssres_stor(i)*harea
          Basin_slstor = Basin_slstor + Slow_stor(i)*harea
          Basin_ssin = Basin_ssin + Ssres_in(i)*harea
          Basin_sroff = Basin_sroff + Sroff(i)*harea
          Basin_pref_stor = Basin_pref_stor + Pref_flow_stor(i)*harea
          Basin_ssflow = Basin_ssflow + Ssres_flow(i)*harea
          Basin_dunnian = Basin_dunnian + Dunnian_flow(i)*harea
! ghl1299
! soil_moist & soil_rechr multiplied by perv_area instead of harea
          Basin_soil_to_gw = Basin_soil_to_gw + Soil_to_gw(i)*perv_area
          Basin_soil_rechr = Basin_soil_rechr + Soil_rechr(i)*perv_area
          Basin_perv_et = Basin_perv_et + Perv_actet(i)*perv_area
          Basin_sz2gw = Basin_sz2gw + Ssr_to_gw(i)*harea
          Basin_sm2gvr = Basin_sm2gvr + Soil_to_ssr(i)*perv_area
          Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*perv_area
          Basin_gvr2sm = Basin_gvr2sm + Gvr2sm(i)*harea
          Basin_gvr2pfr = Basin_gvr2pfr + Gvr2pfr(i)*harea
          Basin_slowflow = Basin_slowflow + Slow_flow(i)*harea
          Basin_prefflow = Basin_prefflow + Pref_flow(i)*harea
          Basin_actet = Basin_actet + Hru_actet(i)*harea

        ELSE ! else it is a lake or reservoir
          avail_potet = 0.0
          Hru_actet(i) = Potet(i)
          Basin_lakeevap = Basin_lakeevap + Hru_actet(i)*harea
          Basin_lakeprecip = Basin_lakeprecip + Hru_ppt(i)*harea
          Lakein_sz(i) = Upslope_interflow(i) + Upslope_dunnianflow(i)
          Basin_lakeinsz = Basin_lakeinsz + Lakein_sz(i)*harea
        ENDIF

        IF ( Prt_debug.EQ.7 ) THEN
          soil_lower = Soil_moist(i) - Soil_rechr(i)
          WRITE (90, 9003) Month, Day, i, Soil_rechr(i), Soil_moist(i),
     +                     soil_lower, Infil_tot(i), ets, etr, Potet(i),
     +                     avail_potet, Perv_actet(i), Ssr_to_gw(i)
        ELSEIF ( Prt_debug.EQ.1 .AND. Hru_type(i).EQ.1 ) THEN
          soilbal = Infil_tot(i) + Gvr2sm(i) + (last_sm-Soil_moist(i)-
     +              Perv_actet(i)-Soil_to_ssr(i)-Soil_to_gw(i))*perv_pct
          IF ( ABS(soilbal).GT.1.0E-4 ) WRITE (BALUNT, *) soilbal,
     +         Infil_tot(i), Gvr2sm(i), last_sm, Soil_moist(i),
     +         Perv_actet(i), Soil_to_ssr(i), Soil_to_gw(i), i,
     +         Infil(i), Pref_flow_in(i), Upslope_interflow(i),
     +         Upslope_dunnianflow(i), perv_pct
          gvrbal = last_ss - Ssres_stor(i) + Ssres_in(i) - Ssres_flow(i)
     +             - Dunnian_flow(i) - Gvr2sm(i) - Ssr_to_gw(i)
     +             - dnslowflow - dnpreflow - dndunn - farflow_slow
     +             - farflow_pref - farflow_dunn
          IF ( ABS(gvrbal).GT.1.0E-4 ) WRITE (BALUNT,*) gvrbal, last_ss,
     +         Ssres_stor(i), Ssres_in(i), Ssres_flow(i),
     +         Dunnian_flow(i), Gvr2sm(i), Ssr_to_gw(i), dnslowflow,
     +         dnpreflow, dndunn, dunnianflw, Soil_to_ssr(i), gwin,
     +         Pref_flow_in(i), farflow_slow, farflow_pref, interflow,
     +         farflow_dunn, pref_flow_infil, i, Gvr2pfr(i), 'bad gvr'
          soil_in = soil_in + Infil(i)*perv_area + gwin*harea
          waterin = Infil(i)*perv_pct + Upslope_dunnianflow(i)
     +              + Upslope_interflow(i) + gwin
          ! have to figure out which are perv area and hru_area
          soilbal = waterin + last_ss - Ssres_stor(i) +
     +              (last_sm-Soil_moist(i)-Perv_actet(i))*perv_pct -
     +              Ssr_to_gw(i) - interflow - dunnianflw -
     +              Soil_to_gw(i)*perv_pct
          basin_bal = basin_bal + soilbal*harea
          err = ABS(soilbal)/perv_area
          IF ( err.GT.1.0E-6 ) THEN
            IF ( err.GT.1.0E-4 ) THEN
              WRITE (BALUNT, *) 'HRU water balance ***ERROR***'
            ELSEIF ( err.GT.1.0E-5 ) THEN
              WRITE (BALUNT, *) 'possible HRU water balance error'
            ELSE
              WRITE (BALUNT, *)
     +              'possible HRU water balance rounding issue'
            ENDIF
            WRITE (BALUNT,9001) Nowtime(1), Month, Day, i, Kkiter,
     +            soilbal, Infil(i), gwin, Upslope_dunnianflow(i),
     +            Upslope_interflow(i), last_sm, last_ss, Soil_moist(i),
     +            Ssres_stor(i), Perv_actet(i), Ssr_to_gw(i), interflow,
     +            Slow_flow(i), Pref_flow(i), dunnianflw, Soil_to_gw(i),
     +            Pref_flow_in(i), Pref_flow_stor(i), Slow_stor(i),
     +            Soil_rechr(i), soil_lower, Soil_to_ssr(i),
     +            Ssres_flow(i), waterin
          ENDIF
        ENDIF

      ENDDO

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      Basin_pref_stor = Basin_pref_stor*Basin_area_inv
      Basin_ssflow = Basin_ssflow*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_sz2gw = Basin_sz2gw*Basin_area_inv
      Basin_ssin = Basin_ssin*Basin_area_inv
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_dunnian = Basin_dunnian*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_sm2gvr = Basin_sm2gvr*Basin_area_inv
      Basin_infil_tot = Basin_infil_tot*Basin_area_inv
      Basin_pref_flow_in = Basin_pref_flow_in*Basin_area_inv
      Basin_dnflow = Basin_dnflow*Basin_area_inv
      Basin_szfarflow = Basin_szfarflow*Basin_area_inv
      Basin_gvr2sm = Basin_gvr2sm*Basin_area_inv
      Basin_gvr2pfr = Basin_gvr2pfr*Basin_area_inv
      Basin_slowflow = Basin_slowflow*Basin_area_inv
      Basin_prefflow = Basin_prefflow*Basin_area_inv
      Basin_lakeevap = Basin_lakeevap*Basin_area_inv
      Basin_lakeprecip = Basin_lakeprecip*Basin_area_inv
      Basin_lakeinsz = Basin_lakeinsz*Basin_area_inv

      IF ( Prt_debug.EQ.1 ) THEN
        gvrbal = Last_ssstor - Basin_ssstor + Basin_ssin -
     +           Basin_ssflow - Basin_dunnian - Basin_gvr2sm -
     +           Basin_sz2gw - Basin_dnflow - Basin_szfarflow
        IF ( ABS(gvrbal).GT.5.0E-4 ) WRITE (BALUNT, *) 'basinbal',
     +      gvrbal, Last_ssstor, Basin_ssstor, Basin_ssin, Basin_ssflow,
     +      Basin_dunnian, Basin_gvr2sm, Basin_sz2gw, Basin_sm2gvr,
     +      Basin_pref_flow_in, Basin_dnflow, Basin_pref_stor,
     +      Basin_szfarflow, Kkiter

        soilbal = Last_soil_moist - Basin_soil_moist - Basin_perv_et -
     +            Basin_soil_to_gw - Basin_sm2gvr + Basin_gvr2sm +
     +            Basin_infil_tot
        IF ( ABS(soilbal).GT.5.0E-4 ) WRITE(BALUNT,*) 'soilbal', soilbal

        soilbal = Last_ssstor - Basin_ssstor + Basin_ssin -
     +            Basin_ssflow - Basin_dunnian - Basin_gvr2sm -
     +            Basin_sz2gw - Basin_dnflow - Basin_szfarflow +
     +            Last_soil_moist - Basin_soil_moist - Basin_perv_et -
     +            Basin_soil_to_gw - Basin_sm2gvr + Basin_gvr2sm +
     +            Basin_infil_tot
        IF ( ABS(soilbal).GT.5.0E-4 ) WRITE (BALUNT, *) 'basin bal 2',
     +                                                  soilbal

        soil_in = soil_in*Basin_area_inv
        basin_bal = basin_bal*Basin_area_inv
        bsmbal = Last_soil_moist - Basin_soil_moist + Last_ssstor -
     +           Basin_ssstor - Basin_perv_et - Basin_sz2gw + soil_in -
     +           Basin_ssflow - Basin_soil_to_gw - Basin_dunnian -
     +           Basin_lakeinsz - Basin_szfarflow
        WRITE (BALUNT, 9002) Nowtime(1), Month, Day, basin_bal, bsmbal,
     +        Last_soil_moist, Basin_soil_moist, Last_ssstor,
     +        Basin_ssstor, Basin_perv_et, Basin_sz2gw, Basin_ssflow,
     +        Basin_soil_to_gw, Basin_dunnian, soil_in, Basin_lakeinsz,
     +        Basin_dnflow, Basin_gvr2sm, Basin_szfarflow, Kkiter
        IF ( ABS(bsmbal).GT.0.05 .OR. ABS(basin_bal).GT.0.001 ) THEN
          WRITE (BALUNT, *) '*ERROR, basin water balance', bsmbal,
     +                      basin_bal
          WRITE (BALUNT, *) Basin_pref_stor, Basin_slstor
        ELSEIF ( ABS(bsmbal).GT.0.005 .OR. ABS(basin_bal).GT.0.0001 )
     +           THEN
          WRITE (BALUNT, *) 'Possible water balance error', bsmbal,
     +                      basin_bal
        ELSEIF ( ABS(bsmbal).GT.0.0005 .OR. ABS(basin_bal).GT.0.00001 )
     +           THEN
          WRITE (BALUNT, '(A,2F12.7)') 'Basin rounding issue', bsmbal,
     +                                 basin_bal
        ENDIF
      ENDIF

      IF ( putvar('srunoff', 'basin_sroff', 1, 'real', Basin_sroff)
     +     .NE.0 ) RETURN
      IF ( putvar('srunoff', 'sroff', Nhru, 'real', Sroff).NE.0 ) RETURN
      IF ( putvar('srunoff', 'strm_seg_in', Nsegment, 'real',
     +     Strm_seg_in).NE.0 ) RETURN
      IF ( putvar('srunoff', 'strm_farfield', 1, 'real', Strm_farfield)
     +     .NE.0 ) RETURN

      szrun = 0

 9001 FORMAT (I5, 2('/', I2.2), 2I5, 24F11.7)
 9002 FORMAT (I5, 2('/', I2.2), 2F11.7, 14F11.6, I7)
 9003 FORMAT (' smbal: ', 3I4, 10F10.6)

      END FUNCTION szrun

!***********************************************************************
!     Add infiltration to soil and compute excess
!***********************************************************************
      SUBROUTINE compute_soilmoist(Td, Perv_area, Infil, Soil_moist_max,
     +                             Soil_rechr_max, Soil2gw_max,
     +                             Soil_to_ssr, Soil_moist, Soil_rechr,
     +                             Soil_lower, Soil_to_gw)
      USE SOILZONE_GSF, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Infil, Perv_area, Td
      REAL, INTENT(IN) :: Soil_moist_max, Soil_rechr_max, Soil2gw_max
      REAL, INTENT(INOUT) :: Soil_moist, Soil_rechr
      REAL, INTENT(OUT) :: Soil_to_ssr, Soil_lower, Soil_to_gw
! Local Variables
      REAL :: excs, s2gwmax
!***********************************************************************
!******Add infiltration to soil and compute excess

      Soil_lower = Soil_moist - Soil_rechr
      IF ( Perv_area.GT.0.0 ) THEN
        Soil_rechr = Soil_rechr + Infil
        IF ( Soil_rechr.GT.Soil_rechr_max ) THEN
          excs = Soil_rechr - Soil_rechr_max
          Soil_rechr = Soil_rechr_max
          Soil_lower = Soil_lower + excs
        ENDIF
        Soil_moist = Soil_lower + Soil_rechr
        IF ( Soil_moist.GT.Soil_moist_max ) THEN
          excs = Soil_moist - Soil_moist_max
          Soil_moist = Soil_moist_max
        ELSE
          excs = 0.
        ENDIF

        IF ( Soil2gw_max.GT.NEARZERO ) THEN
          s2gwmax = Soil2gw_max*Td
          IF ( excs.GT.s2gwmax ) THEN
            Soil_to_gw = s2gwmax
          ELSE
            Soil_to_gw = excs
          ENDIF
        ENDIF
        Soil_to_ssr = excs - Soil_to_gw
      ELSE
        Soil_moist = 0.
        Soil_rechr = 0.
      ENDIF

      END SUBROUTINE compute_soilmoist

!***********************************************************************
!     Compute actual evapotranspiration
!***********************************************************************
      SUBROUTINE compute_szactet(Perv_area, Soil_moist_max,
     +                           Soil_rechr_max, Snowcov_area,
     +                           Transp_on, Cov_type, Soil_type,
     +                           Soil_moist, Soil_rechr, Perv_actet,
     +                           Ets, Etr, Avail_potet)
      USE SOILZONE_GSF, ONLY: ONETHIRD, TWOTHIRDS, NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Transp_on, Cov_type, Soil_type
      REAL, INTENT(IN) :: Soil_moist_max, Soil_rechr_max, Perv_area
      REAL, INTENT(IN) :: Snowcov_area
      REAL, INTENT(INOUT) :: Soil_moist, Soil_rechr, Avail_potet
      REAL, INTENT(OUT) :: Perv_actet, Ets, Etr
! Local Variables
      INTEGER :: et_type
      REAL :: et, open_ground, pcts, pctr
!***********************************************************************
      Ets = 0.0
      Etr = 0.0

      IF ( Perv_area.GT.0.0 ) THEN
        et = 0.0
        open_ground = 1.0 - Snowcov_area

!******Determine if evaporation(et_type = 2) or transpiration plus
!******evaporation(et_type = 3) are active.  if not, et_type = 1

        IF ( Avail_potet.LT.NEARZERO ) THEN
          et_type = 1
          Avail_potet = 0.0
        ELSEIF ( Transp_on.EQ.0 ) THEN
          IF ( open_ground.LT.0.01 ) THEN
            et_type = 1
          ELSE
            et_type = 2
          ENDIF
        ELSEIF ( Cov_type.GT.0 ) THEN
          et_type = 3
        ELSEIF ( open_ground.LT.0.01 ) THEN
          et_type = 1
        ELSE
          et_type = 2
        ENDIF

        IF ( et_type.GT.1 ) THEN
          pcts = Soil_moist/Soil_moist_max
! allow for inaccurate gvr_hru_pct, average error, rsr commented out
!         IF ( pcts.GT.0.999 ) pcts = 1.0
          pctr = Soil_rechr/Soil_rechr_max
          Ets = Avail_potet
          Etr = Avail_potet

!******sandy soil
          IF ( Soil_type.EQ.1 ) THEN
            IF ( pcts.LT.0.25 ) Ets = 0.5*pcts*Avail_potet
            IF ( pctr.LT.0.25 ) Etr = 0.5*pctr*Avail_potet
!******loam soil
          ELSEIF ( Soil_type.EQ.2 ) THEN
            IF ( pcts.LT.0.5 ) Ets = pcts*Avail_potet
            IF ( pctr.LT.0.5 ) Etr = pctr*Avail_potet
!******clay soil
          ELSEIF ( Soil_type.EQ.3 ) THEN
            IF ( pcts.LT.TWOTHIRDS .AND. pcts.GT.ONETHIRD ) THEN
              Ets = pcts*Avail_potet
            ELSEIF ( pcts.LE.ONETHIRD ) THEN
              Ets = 0.5*pcts*Avail_potet
            ENDIF
            IF ( pctr.LT.TWOTHIRDS .AND. pctr.GT.ONETHIRD ) THEN
              Etr = pctr*Avail_potet
            ELSEIF ( pctr.LE.ONETHIRD ) THEN
              Etr = 0.5*pctr*Avail_potet
            ENDIF
          ENDIF

!******Soil moisture accounting
          IF ( et_type.EQ.2 ) Etr = Etr*open_ground
          IF ( Etr.GT.Soil_rechr ) THEN
            Etr = Soil_rechr
            Soil_rechr = 0.0
          ELSE
            Soil_rechr = Soil_rechr - Etr
          ENDIF
          IF ( et_type.EQ.2 .OR. Etr.GE.Ets ) THEN
            IF ( Etr.GT.Soil_moist ) THEN
              Etr = Soil_moist
              Soil_moist = 0.0
            ELSE
              Soil_moist = Soil_moist - Etr
            ENDIF
            et = Etr
          ELSEIF ( Ets.GE.Soil_moist ) THEN
            et = Soil_moist
            Soil_moist = 0.0
            Soil_rechr = 0.0
          ELSE
            Soil_moist = Soil_moist - Ets
            et = Ets
          ENDIF
          IF ( Soil_rechr.GT.Soil_moist ) Soil_rechr = Soil_moist

        ELSE
          et = 0.0
        ENDIF
        Perv_actet = et
      ELSE
        Perv_actet = 0.0
      ENDIF

      END SUBROUTINE compute_szactet

!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE comp_inter_gwflow(Capacity, Slowcoef_lin, Slowcoef_sq,
     +                             Ssr2gw_rate, Ssr2gw_exp, Ssrmax_coef,
     +                             Pref_flow_thrsh, Td, Ssres_stor,
     +                             Soil_to_ssr, Gvr2pfr, Ssr_to_gw,
     +                             Slow_flow, Slow_stor, Gvr2sm,
     +                             Perv_pct)
      IMPLICIT NONE
      EXTERNAL check_soilmoist, compute_interflow
! Arguments
      REAL, INTENT(IN) :: Capacity, Slowcoef_lin, Slowcoef_sq, Perv_pct
      REAL, INTENT(IN) :: Ssr2gw_rate, Ssr2gw_exp, Ssrmax_coef
      REAL, INTENT(IN) :: Pref_flow_thrsh, Td, Ssres_stor, Soil_to_ssr
      REAL, INTENT(INOUT) :: Slow_stor, Gvr2sm
      REAL, INTENT(OUT) :: Ssr_to_gw, Slow_flow, Gvr2pfr
! Local Variables
      REAL :: input
!***********************************************************************
      Ssr_to_gw = 0.0
      IF ( Ssres_stor.GT.0.0 .OR. Soil_to_ssr.GT.0.0 ) THEN
        input = Soil_to_ssr*Perv_pct

        CALL check_soilmoist(Capacity, Pref_flow_thrsh, Slow_stor,
     +                       Gvr2pfr, input, Gvr2sm, Perv_pct)

! compute slow contribution to interflow, if any
        CALL compute_interflow(Slowcoef_lin, Slowcoef_sq, input,
     +                         Slow_stor, Slow_flow)

!******compute flow to groundwater
        IF ( Slow_stor.GT.0.0 .AND. Ssr2gw_rate.GT.0.0 ) THEN
          Ssr_to_gw = Ssr2gw_rate*Td*
     +                ((Slow_stor/Ssrmax_coef)**Ssr2gw_exp)
          IF ( Ssr_to_gw.GT.Slow_stor ) Ssr_to_gw = Slow_stor
          IF ( Ssr_to_gw.LT.0.0 ) Ssr_to_gw = 0.0
!remove this print after debugging
!         IF ( Ssr_to_gw.GT.Slow_stor ) THEN
!           PRINT *, '***Issue in soilzone computing drainage',
!    +               Ssr_to_gw, Slow_stor
!           Ssr_to_gw = Slow_stor
!         ENDIF
          Slow_stor = Slow_stor - Ssr_to_gw
        ENDIF
      ELSE
        Slow_flow = 0.0
      ENDIF

      END SUBROUTINE comp_inter_gwflow

!***********************************************************************
!     Compute subsurface lateral flow
!***********************************************************************
      SUBROUTINE compute_interflow(Coef_lin, Coef_sq, Input, Storage,
     +                             Inter_flow)
!     USE SOILZONE_GSF, ONLY:Nowtime
      USE SOILZONE_GSF, ONLY:Td, Ts, NEARZERO, SMALL
      IMPLICIT NONE
      INTRINSIC EXP, SQRT
! Arguments
      REAL, INTENT(IN) :: Coef_lin, Coef_sq, Input
      REAL, INTENT(INOUT) :: Storage
      REAL, INTENT(OUT) :: Inter_flow
! Local Variables
      REAL :: c1, c2, c3, sos, availh2o
!     REAL :: tmp
!***********************************************************************
! remove after debugging
!     IF ( Storage.LT.0.0 .OR. Input.LT.0.0 ) THEN
!       PRINT *, "Sanity check in compute_interflow:"
!       PRINT *, "   Storage = ", Storage, " Input = ", Input, Nowtime
!       STOP
!     ENDIF

      Inter_flow = 0.0
      availh2o = Storage + Input
! Inter_flow is in inches for the timestep
      IF ( availh2o.GT.0.0 ) THEN

!******compute interflow
        IF ( Coef_lin.LT.NEARZERO .AND. Input.LE.0.0 ) THEN
          c1 = Coef_sq*Storage
          Inter_flow = Storage*(c1/(1.0+c1))
        ELSEIF ( Coef_sq.LT.NEARZERO ) THEN
          c2 = 1.0 - EXP(-Coef_lin*Td)
          Inter_flow = Input*(1.0-c2/Coef_lin*Td) + Storage*c2
        ELSE
          c3 = SQRT(Coef_lin**2.0+4.0*Coef_sq*Input*Ts)
          sos = Storage - ((c3-Coef_lin)/(2.0*Coef_sq))
          c1 = Coef_sq*sos/c3
          c2 = 1.0 - EXP(-c3*Td)
          Inter_flow = Input + (sos*(1.0+c1)*c2)/(1.0+c1*c2)
        ENDIF

! sanity check
!       IF ( Inter_flow.LT.-1.0E-10 ) PRINT *, 'interflow<0', Inter_flow
        IF ( Inter_flow.LT.0.0 ) THEN
          Inter_flow = 0.0
        ELSEIF ( Inter_flow.GT.availh2o ) THEN
          Inter_flow = availh2o
        ENDIF
        Storage = availh2o - Inter_flow
! if small amount of storage left, route it out as interflow
        IF ( Storage.LT.SMALL ) THEN
!         PRINT *, 'small', Storage
          Inter_flow = Inter_flow + Storage
          Storage = 0.0
        ENDIF
! the following makes no difference, values too small
!       tmp = availh2o - Inter_flow - Storage
!       IF ( ABS(tmp).GT.0.0 ) THEN
!         PRINT *, 'comp_in', availh2o, Inter_flow, Storage, tmp
!         IF ( Storage.GT.ABS(tmp) ) THEN
!           Storage = Storage + tmp
!         ELSE
!           Inter_flow = Inter_flow + tmp
!         ENDIF
!         tmp = availh2o - Inter_flow - Storage
!         PRINT *, 'comp_in 2', availh2o, Inter_flow, Storage, tmp
!       ENDIF
      ENDIF

      END SUBROUTINE compute_interflow

!***********************************************************************
!     adjust soil moist based on being below field capacity (capacity)
!     and preferential-flow threshold (Pref_flow_thrsh)
!***********************************************************************
      SUBROUTINE check_soilmoist(Capacity, Pref_flow_thrsh, Depth,
     +                           Gvr2pfr, Input, Gvr2sm, Perv_pct)
      USE SOILZONE_GSF, ONLY:Model
      IMPLICIT NONE
      INTRINSIC MAX
! Arguments
      REAL, INTENT(IN) :: Capacity, Pref_flow_thrsh, Perv_pct
      REAL, INTENT(INOUT) :: Depth, Input
      REAL, INTENT(OUT) :: Gvr2sm, Gvr2pfr
! Local Variables
      REAL :: availh2o, to_sm
!***********************************************************************
      availh2o = Depth + Input

! check to see if soil is below capacity, if so add up to field capacity
! do not replenish if in PRMS only mode
! Capacity is in pervious portion
! to_sm is for whole HRU
      IF ( Model.EQ.1 .AND. Capacity.GT.0.0 ) THEN
        IF ( Capacity.GE.availh2o/Perv_pct ) THEN
          to_sm = availh2o
        ELSE
          to_sm = Capacity*Perv_pct
        ENDIF
! compute adjustment to soil moist to field capacity
        Gvr2sm = to_sm
      ELSE
        to_sm = 0.0
      ENDIF
      availh2o = availh2o - to_sm

! compute contribution to preferential-flow reservoir storage, if any
      Gvr2pfr = MAX(0.0, availh2o-Pref_flow_thrsh)

! adjust depth and input, take from input first
      Input = Input - (Gvr2pfr+to_sm)
      IF ( Input.LT.0.0 ) THEN
!rsr??? how should this be done
!       PRINT *, 'warning, input<0 in check_soilmoist', Depth,
!    +           Gvr2pfr, to_sm, Input, Capacity
        Depth = Depth + Input
        Input = 0.0
        IF ( Depth.LT.0.0 ) Depth = 0.0
!       IF ( Depth.LT.0.0 ) THEN
!         IF ( Depth.GT.1.0E-7 )
!    +         PRINT *, 'warning in check_soilmoist', Input, Depth
!         Depth = 0.0
!       ENDIF
      ENDIF

      END SUBROUTINE check_soilmoist

!***********************************************************************
!     Compute cascading interflow and excess flow
!***********************************************************************
      SUBROUTINE compute_cascades(Ihru, Ncascade_hru, Slowflow, Preflow,
     +                            Dunnian, Dnslowflow, Dnpreflow,
     +                            Dndunnflow, Farflow_slow,
     +                            Farflow_pref, Farflow_dunn)
      USE SOILZONE_GSF, ONLY:Upslope_dunnianflow, Upslope_interflow,
     +    Strm_seg_in, Cfs_conv, Strm_farfield, Nsegmentp1
      USE PRMS_CASCADE, ONLY:Hru_down, Hru_down_pct, Hru_down_pctwt,
     +    Cascade_area
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Ncascade_hru
      REAL, INTENT(INOUT) :: Dunnian, Slowflow, Preflow
      REAL, INTENT(INOUT) :: Dnslowflow, Dnpreflow, Dndunnflow
      REAL, INTENT(INOUT) :: Farflow_slow, Farflow_pref, Farflow_dunn
! Local Variables
      INTEGER :: j, k
      REAL :: pct, pctwt
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
        pct = Hru_down_pct(k, Ihru)
        pctwt = Hru_down_pctwt(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j.GT.0 ) THEN
          Upslope_interflow(j) = Upslope_interflow(j) +
     +                           (Slowflow+Preflow)*pctwt
          Upslope_dunnianflow(j) = Upslope_dunnianflow(j)
     +                             + Dunnian*pctwt
          Dnslowflow = Dnslowflow + Slowflow*pct
          Dnpreflow = Dnpreflow + Preflow*pct
          Dndunnflow = Dndunnflow + Dunnian*pct
! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j.LT.0 ) THEN
          j = IABS(j)
          IF ( j.NE.Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) + (Slowflow+Preflow+Dunnian)
     +                       *Cascade_area(k, Ihru)*Cfs_conv
          ELSE
            Strm_farfield = Strm_farfield + (Slowflow+Preflow+Dunnian)
     +                      *Cascade_area(k, Ihru)*Cfs_conv
            Farflow_slow = Farflow_slow + Slowflow*pct
            Farflow_pref = Farflow_pref + Preflow*pct
            Farflow_dunn = Farflow_dunn + Dunnian*pct
          ENDIF
        ENDIF
      ENDDO

! reset Slowflow, Preflow, and Dunnian_flow as they accumulate flow to streams
      Slowflow = Slowflow - Dnslowflow - Farflow_slow
      Preflow = Preflow - Dnpreflow - Farflow_pref
      Dunnian = Dunnian - Dndunnflow - Farflow_dunn
      !sanity check, remove later
!     IF ( Inter_flow.NE.0.0 )
!    +   PRINT *, 'interflw problem 1', Inter_flow, pct, Ncascade_hru, j
!     IF ( pct.GT.0.999 .AND. Inter_flow.GT.0.0)
!    +   PRINT *, 'interflw problem', Inter_flow, pct, Ncascade_hru, j
!     IF ( Dunnian.NE.0.0 )
!    +   PRINT *, 'dunnian problem', Dunnian, pct, Ncascade_hru, j

      END SUBROUTINE compute_cascades

!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE compute_gravflow(Ihru, Capacity, Slowcoef_lin,
     +                            Slowcoef_sq, Ssr2gw_rate, Ssr2gw_exp,
     +                            Ssrmax_coef, Pref_flow_thrsh, Td,
     +                            Soil_to_ssr, Gvr2pfr, Ssr_to_gw,
     +                            Slow_flow, Slow_stor, Gvr2sm,
     +                            Soil_to_gw, Gwin, Perv_pct)
      USE SOILZONE_GSF, ONLY:Gvr_hru_id, Gvr_hru_pct, Gw2sm_grav,
     +    Gravity_stor_res, Sm2gw_grav, Kkiter, Nhrucell, Sm2gw_grav_old
      IMPLICIT NONE
      EXTERNAL check_gvr_sm, compute_interflow
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(IN) :: Capacity, Slowcoef_lin, Slowcoef_sq, Perv_pct
      REAL, INTENT(IN) :: Ssr2gw_rate, Ssr2gw_exp, Ssrmax_coef
      REAL, INTENT(IN) :: Pref_flow_thrsh, Td, Soil_to_ssr, Soil_to_gw
      REAL, INTENT(INOUT) :: Gvr2sm
      REAL, INTENT(OUT) :: Ssr_to_gw, Slow_flow, Slow_stor, Gwin
      REAL, INTENT(OUT) :: Gvr2pfr
! Local Variables
      INTEGER :: i
      REAL :: input, depth, pct, slowflow, perc
!***********************************************************************
      !Capacity, Soil_to_ssr, Soil_to_gw are for pervious area
      !TO DO
! use VKS as a function of slope (vector analysis) instead of coef_lin
! coef_lin for pref_flow needs to be VKS lateral times a factor
! change slow to interflow
! in init, set an array dimensioned by nhrucell to vks*mfl_to_inch

      Slow_flow = 0.0
      Slow_stor = 0.0
      Ssr_to_gw = 0.0
      Gwin = 0.0
      DO i = 1, Nhrucell
        IF ( Gvr_hru_id(i).NE.Ihru ) CYCLE
        pct = Gvr_hru_pct(i)
! Gravity_stor_res is reset to It0 for each iteration
        depth = Gravity_stor_res(i)
        input = Soil_to_ssr*Perv_pct + Gw2sm_grav(i)

        IF ( depth.GT.0.0 .OR. input.GT.0.0 ) THEN
          Gwin = Gwin + Gw2sm_grav(i)*pct

          CALL check_gvr_sm(Capacity, Pref_flow_thrsh, depth, Gvr2pfr,
     +                      input, pct, Gvr2sm, Perv_pct)

! compute contribution to interflow, if any
          CALL compute_interflow(Slowcoef_lin, Slowcoef_sq, input,
     +                           depth, slowflow)
          Slow_flow = Slow_flow + slowflow*pct

! compute flow to groundwater, if any
          IF ( depth.GT.0.0 ) THEN
! use VKS instead of rate  ???????????????
!           perc = Ssr2gw_rate*Td*((depth/Sat_threshold)**Ssr2gw_exp)
            perc = Ssr2gw_rate*Td*((depth/Ssrmax_coef)**Ssr2gw_exp)
! assume best guess is halfway between last iteration and this iteration
            IF ( Kkiter.GT.2 ) perc = perc - (perc-Sm2gw_grav_old(i))*.5
            IF ( perc.GT.depth ) THEN
!remove this print after debugging
!             PRINT *, 'Issue in soilzone computing perc', perc,
!    +                depth, Sm2gw_grav_old(i), i, Gw2sm_grav(i), Kkiter
              perc = depth
            ENDIF
            depth = depth - perc
            Sm2gw_grav(i) = perc
            Ssr_to_gw = Ssr_to_gw + perc*pct
          ENDIF

!         IF ( depth.LT.0.0 ) PRINT *, 'depth<0', depth
          Gravity_stor_res(i) = depth
          Slow_stor = Slow_stor + depth*pct
        ENDIF

! add any direct recharge from soil infiltration
        Sm2gw_grav(i) = Sm2gw_grav(i) + Soil_to_gw*Perv_pct

      ENDDO ! end Nhrucell loop

      END SUBROUTINE compute_gravflow

!***********************************************************************
!     adjust soil moist based on being below field capacity (capacity)
!     and preferential-flow threshold (Pref_flow_thrsh)
!***********************************************************************
      SUBROUTINE check_gvr_sm(Capacity, Pref_flow_thrsh, Depth, Gvr2pfr,
     +                        Input, Pct, Gvr2sm, Perv_pct)
      IMPLICIT NONE
      INTRINSIC MAX, ABS
! Arguments
      REAL, INTENT(IN) :: Capacity, Pref_flow_thrsh, Pct, Perv_pct
      REAL, INTENT(INOUT) :: Depth, Gvr2pfr, Input, Gvr2sm
! Local Variables
      REAL :: availh2o, to_sm, pfr_h2o
!***********************************************************************
      availh2o = Depth + Input

! check to see if soil is below capacity, if so add up to field capacity
! Capacity is for pervious area
! to_sm is for whole HRU
      IF ( Capacity.GT.0.0 ) THEN
        IF ( Capacity.GE.availh2o/Perv_pct ) THEN
          to_sm = availh2o
        ELSE
          to_sm = Capacity*Perv_pct
        ENDIF
! compute adjusmtent to soil moist to get to field capacity
        !Gvr2sm is over whole HRU
        Gvr2sm = Gvr2sm + to_sm*Pct
      ELSE
        to_sm = 0.0
      ENDIF
      availh2o = availh2o - to_sm

! compute contribution to preferential-flow reservoir storage, if any
! Note, Gvr2pfr could have a value based on other GVR's
      pfr_h2o = MAX(0.0, availh2o-Pref_flow_thrsh)
      Gvr2pfr = Gvr2pfr + pfr_h2o*Pct

! adjust depth and input, take from input first
      Input = Input - (pfr_h2o+to_sm)
      IF ( Input.LT.0.0 ) THEN
!rsr??? how should this be done
!       PRINT *, 'warning, input<0 in check_gvr_sm', Depth,
!    +           pfr_h2o, to_sm, Input, Capacity
        Depth = Depth + Input
        Input = 0.0
        IF ( Depth.LT.0.0 ) Depth = 0.0
!       IF ( Depth.LT.0.0 ) THEN
!         IF ( ABS(Depth).GT.1.0E-7 )
!    +         PRINT *, 'warning in check_gvr_sm', Input, Depth
!         Depth = 0.0
!       ENDIF
      ENDIF

      END SUBROUTINE check_gvr_sm

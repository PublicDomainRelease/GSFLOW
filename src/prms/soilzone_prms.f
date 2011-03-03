!***********************************************************************
! Computes inflows to and outflows from soil zone of each HRU and
! includes inflows from infiltration, ground-water, and upslope HRUs,
! and outflows to gravity drainage, interflow, and surface runoff to
! down-slope HRUs; merge of smbal_prms and ssflow_prms with enhancements
!
! Daily accounting for soil zone;
!    adds infiltration
!    computes et
!    computes recharge of soil zone
!    computes interflow to stream or cascade
!    adjusts storage in soil zone
!    sends dunnian runoff to stream or cascade by adding to sroff
!    computes drainage to groundwater
!***********************************************************************
      MODULE PRMS_SOILZONE
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 195
      REAL, PARAMETER :: TOOSMALL=8.0E-5
      INTEGER, SAVE :: Nhrucell, First_run
      INTEGER, SAVE, ALLOCATABLE :: Hru_gvr_count(:), Hru_gvr_index(:,:)
      REAL, SAVE :: Last_soil_moist, Last_ssstor
      INTEGER, SAVE, ALLOCATABLE :: Soil2gw(:)
      REAL, SAVE, ALLOCATABLE :: Replenish_pct(:), Gvr2pfr(:)
      REAL, SAVE, ALLOCATABLE :: It0_soil_rechr(:), It0_soil_moist(:)
      REAL, SAVE, ALLOCATABLE:: It0_pref_flow_stor(:), It0_ssres_stor(:)
      REAL, SAVE, ALLOCATABLE :: It0_gravity_stor_res(:), It0_sroff(:)
      REAL, SAVE, ALLOCATABLE :: It0_slow_stor(:), It0_strm_seg_in(:)
      CHARACTER(LEN=68), SAVE :: Versn_soilzone
      REAL, SAVE, ALLOCATABLE :: Swale_limit(:)
      REAL, SAVE :: It0_strm_farfield
!   Declared Variables
      REAL, SAVE :: Basin_soil_moist, Basin_sz2gw
      REAL, SAVE :: Basin_soil_rechr, Basin_ssstor
      REAL, SAVE :: Basin_ssr2gw, Basin_ssin
      REAL, SAVE :: Basin_sm2gvr, Basin_dnflow, Basin_gvr2sm
      REAL, SAVE :: Basin_infil_tot, Basin_pref_flow_in, Basin_dunnian
      REAL, SAVE :: Basin_gvr2pfr, Basin_slowflow, Basin_prefflow
      REAL, SAVE :: Basin_lakeinsz, Basin_lakeprecip
      REAL, SAVE :: Basin_szfarflow, Basin_pref_stor, Basin_slstor
      REAL, SAVE, ALLOCATABLE :: Sm2gw_grav(:), Sm2gw_grav_old(:)
      REAL, SAVE, ALLOCATABLE :: Gravity_stor_res(:), Gvr2sm(:)
      REAL, SAVE, ALLOCATABLE :: Ssres_stor(:), Ssres_in(:)
      REAL, SAVE, ALLOCATABLE :: Perv_actet(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_tot(:), Soil_moist_pct(:)
      REAL, SAVE, ALLOCATABLE :: Soil_rechr(:), Soil_moist(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_interflow(:), Dunnian_flow(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_dunnianflow(:), Infil_tot(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_stor(:), Pref_flow(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_thrsh(:), Pref_flow_in(:)
      REAL, SAVE, ALLOCATABLE :: Slow_flow(:), Slow_stor(:)
      REAL, SAVE, ALLOCATABLE :: Lakein_sz(:), Soil_zone_max(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sz_cascadeflow(:), Swale_actet(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_max(:)
!   Declared Variables from other modules - gsflow_modflow
      INTEGER, SAVE :: Kkiter
!   Declared Variables from other modules - mf2prms
      REAL, ALLOCATABLE :: Gw2sm_grav(:)
!   Declared Variables from other modules - prms2mf
      REAL, SAVE, ALLOCATABLE :: Gvr_hru_pct_adjusted(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Soil_type(:), Gvr_hru_id(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_init(:), Ssstor_init(:)
      REAL, SAVE, ALLOCATABLE :: Sat_threshold(:), Pref_flow_den(:)
      REAL, SAVE, ALLOCATABLE :: Fastcoef_lin(:), Fastcoef_sq(:)
      REAL, SAVE, ALLOCATABLE :: Slowcoef_lin(:), Slowcoef_sq(:)
      REAL, SAVE, ALLOCATABLE :: Ssr2gw_rate(:), Ssr2gw_exp(:)
      REAL, SAVE, ALLOCATABLE :: Soil_rechr_init(:), Soil2gw_max(:)
      END MODULE PRMS_SOILZONE

!***********************************************************************
!     Main soilzone routine
!***********************************************************************
      INTEGER FUNCTION soilzone_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: szdecl, szinit, szrun
!***********************************************************************
      soilzone_prms = 0

      IF ( Process_flag==0 ) THEN
        soilzone_prms = szrun()
      ELSEIF ( Process_flag==1 ) THEN
        soilzone_prms = szdecl()
      ELSEIF ( Process_flag==2 ) THEN
        soilzone_prms = szinit()
      ENDIF

      END FUNCTION soilzone_prms

!***********************************************************************
!     szdecl - set up parameters for soil zone computations
!   Declared Parameters
!     sat_threshold, ssstor_init fastcoef_lin, fastcoef_sq
!     ssr2gw_rate, ssr2gw_exp, soil2gw_max, soil_type
!     soil_rechr_max, soil_rechr_init, soil_moist_max, soil_moist_init
!     pref_flow_den, slowcoef_lin, cov_type
!     hru_area, slowcoef_sq, gvr_hru_id
!***********************************************************************
      INTEGER FUNCTION szdecl()
      USE PRMS_SOILZONE
      USE PRMS_MODULE, ONLY: Model, Ncascade
      USE PRMS_BASIN, ONLY: Nhru, Nssr, Nsegment
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar, getdim
!***********************************************************************
      szdecl = 1

      IF ( declmodule(
     +'$Id: soilzone_prms.f 2276 2010-12-15 16:24:36Z rsregan $'
     +).NE.0 ) RETURN
      Versn_soilzone =
     +'$Id: soilzone_prms.f 2276 2010-12-15 16:24:36Z rsregan $'

      IF ( Nhru.NE.Nssr .AND. Model/=99 ) THEN
        PRINT *, 'Error, nhru must equal nssr to use soilzone module',
     +           Nhru, Nssr
        RETURN
      ENDIF

! Declare Variables
      ALLOCATE (Gvr2sm(Nhru))
      IF ( Model==0 .OR. Model==99 ) THEN
        Nhrucell = getdim('nhrucell')
        IF ( Nhrucell.EQ.-1 ) RETURN

        ALLOCATE (Gravity_stor_res(Nhrucell))
        IF ( declvar('soilzone', 'gravity_stor_res', 'nhrucell',
     +       Nhrucell, 'real', 'Storage in each gravity-flow reservoir',
     +       'inches',
     +       Gravity_stor_res).NE.0 ) RETURN

        ALLOCATE (Sm2gw_grav(Nhrucell))
        IF ( declvar('soilzone', 'sm2gw_grav', 'nhrucell', Nhrucell,
     +       'real',
     +      'Drainage from each gravity reservoir to each MODFLOW cell',
     +       'inches',
     +       Sm2gw_grav).NE.0 ) RETURN

        ALLOCATE (Sm2gw_grav_old(Nhrucell))
        IF ( declvar('soilzone', 'sm2gw_grav_old', 'nhrucell', Nhrucell,
     +       'real',
     +       'Drainage from each gravity reservoir to each MODFLOW'//
     +       ' cell from the previous iteration',
     +       'inches',
     +       Sm2gw_grav_old).NE.0 ) RETURN

        IF ( declvar('soilzone', 'basin_gvr2sm', 'one', 1, 'real',
     +       'Basin weighted average for gravity flow to soil moist',
     +       'inches',
     +       Basin_gvr2sm).NE.0 ) RETURN

        IF ( declvar('soilzone', 'gvr2sm', 'nhru', Nhru, 'real',
     +       'gravity flow to soil moist replenishment for each HRU',
     +       'inches',
     +       Gvr2sm).NE.0 ) RETURN

      ENDIF

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

      ALLOCATE (Ssres_stor(Nssr))
      IF ( declvar('soilzone', 'ssres_stor', 'nssr', Nssr, 'real',
     +     'Storage in each subsurface reservoir',
     +     'inches',
     +     Ssres_stor).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_sm2gvr', 'one', 1, 'real',
     +     'Basin weighted average for soil_to_ssr',
     +     'inches',
     +     Basin_sm2gvr).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_dnflow', 'one', 1, 'real',
     +     'Basin weighted average for cascading down slope flow',
     +     'inches',
     +     Basin_dnflow).NE.0 ) RETURN

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

      IF ( declvar('soilzone', 'basin_ssstor', 'one', 1, 'real',
     +     'Basin weighted average for subsurface reservoir storage',
     +     'inches',
     +     Basin_ssstor).NE.0 ) RETURN

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

      IF ( declvar('soilzone', 'basin_sz2gw', 'one', 1, 'real',
     +     'Basin average drainage from soil added to groundwater',
     +     'inches',
     +     Basin_sz2gw).NE.0 ) RETURN

      ALLOCATE (Ssres_in(Nssr))
      IF ( declvar('soilzone', 'ssres_in', 'nssr', Nssr, 'real',
     +     'Sum of inflow to subsurface reservoir from all'//
     +     ' associated HRUs',
     +     'inches',
     +     Ssres_in).NE.0 ) RETURN

      ALLOCATE (Perv_actet(Nhru))
      IF ( declvar('soilzone', 'perv_actet', 'nhru', Nhru, 'real',
     +     'Actual evapotranspiration from pervious areas of HRU',
     +     'inches',
     +     Perv_actet).NE.0 ) RETURN

      !rsr, added to be compatible with ssflow_prms
      IF ( declvar('soilzone', 'basin_ssin', 'one', 1, 'real',
     +     'Basin weighted average for inflow to subsurface reservoirs',
     +     'inches',
     +     Basin_ssin).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_ssr2gw', 'one', 1, 'real',
     +     'Basin average drainage from soil added to groundwater',
     +     'inches',
     +     Basin_ssr2gw).NE.0 ) RETURN

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
     +    'Basin area weighted average of lake inflow from soil zone',
     +     'inches',
     +     Basin_lakeinsz).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_lakeprecip', 'one', 1, 'real',
     +     'Basin area weighted average of precipitation on lakes',
     +     'inches',
     +     Basin_lakeprecip).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_szfarflow', 'one', 1, 'real',
     +     'Basin area weighted average of far-field flow from soil',
     +     'inches',
     +     Basin_szfarflow).NE.0 ) RETURN

      ALLOCATE (Hru_sz_cascadeflow(Nhru))
      IF ( declvar('soilzone', 'hru_sz_cascadeflow', 'nhru', Nhru,
     +     'real',
     +     'Cascading interflow and Dunnian surface runoff leaving'//
     +     ' each HRU',
     +     'inches',
     +     Hru_sz_cascadeflow).NE.0 ) RETURN

      ALLOCATE (Swale_actet(Nhru))
      IF ( declvar('soilzone', 'swale_actet', 'nhru', Nhru, 'real',
     +     'Actual evapotranspiration from gravity reservoir when'//
     +     ' greater than sat_threshold',
     +     'inches',
     +     Swale_actet).NE.0 ) RETURN

! Declare Parameters
      IF ( Model==0 .OR. Model==99 ) THEN
        ALLOCATE (Gvr_hru_id(Nhrucell))
        IF ( declparam('soilzone', 'gvr_hru_id', 'nhrucell', 'integer',
     +       '1', 'bounded', 'nhru',
     +       'Corresponding HRU id of each GVR',
     +       'Index of the HRU assocated with each gravity reservoir',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Gvr_hru_pct_adjusted(Nhrucell))
        ALLOCATE (It0_pref_flow_stor(Nhru), It0_ssres_stor(Nhru))
        ALLOCATE (It0_soil_rechr(Nhru), It0_soil_moist(Nhru))
        ALLOCATE (It0_gravity_stor_res(Nhrucell), It0_sroff(Nhru))
        ALLOCATE (It0_slow_stor(Nhru), Gw2sm_grav(Nhrucell))
        ALLOCATE (It0_strm_seg_in(Nsegment))
      ENDIF

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
     +     '0.0', '0.0', '1.0',
     +     'Preferential-flow pore density',
     +     'Preferential-flow pore density',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Soil_rechr_init(Nhru))
      IF ( declparam('soilzone', 'soil_rechr_init', 'nhru', 'real',
     +     '1.', '0.', '10.',
     +     'Initial value of water for soil recharge zone',
     +     'Initial value for soil recharge zone (upper part of'//
     +   ' soil_moist).  Must be less than or equal to soil_moist_init',
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
     +     ' estimated based on measured flow',
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
     +     ' ssr_to_gw = ssr2gw_rate * (ssres_stor**ssr2gw_exp)',
     +     '1/day').NE.0 ) RETURN

      ALLOCATE (Ssr2gw_exp(Nssr))
      IF ( declparam('soilzone', 'ssr2gw_exp', 'nssr', 'real',
     +     '1.0', '0.0', '3.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Coefficient in equation used to route water from the'//
     +     ' subsurface reservoirs to the groundwater reservoirs: '//
     +     ' ssr_to_gw = ssr2gw_rate * (ssres_stor**ssr2gw_exp)'//
     +     ' recommended value is 1.0',
     +     'none').NE.0 ) RETURN

! Allocate arrays for local and variables from other modules
      ALLOCATE ( Soil2gw(Nhru), Gvr2pfr(Nhru) )
      ALLOCATE ( Swale_limit(Nhru) )
      ALLOCATE ( Replenish_pct(Nhru) )

      szdecl = 0
      END FUNCTION szdecl

!***********************************************************************
!     szinit - Initialize soilzone module - get parameter values,
!              set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION szinit()
      USE PRMS_SOILZONE
      USE PRMS_MODULE, ONLY: Model, Ncascade
      USE PRMS_BASIN, ONLY: Hru_type, Hru_perv, Nhru, Nssr, Print_debug,
     +    Print_debug, Basin_area_inv, Hru_area, Timestep, NEARZERO,
     +    Hru_percent_perv, Numlakes, Active_hrus, Hru_route_order
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_rechr_max, DBGUNT
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      INTRINSIC MIN
! Local Variables
      INTEGER :: i, ii, max_gvrs, icnt
!***********************************************************************
      szinit = 1

      IF ( getparam('soilzone', 'slowcoef_lin', Nhru, 'real',
     +     Slowcoef_lin).NE.0 ) RETURN

      IF ( getparam('soilzone', 'slowcoef_sq', Nhru, 'real',
     +     Slowcoef_sq).NE.0 ) RETURN

      IF ( getparam('soilzone', 'pref_flow_den', Nhru, 'real',
     +     Pref_flow_den).NE.0 ) RETURN

      IF ( getparam('soilzone', 'fastcoef_lin', Nhru, 'real',
     +     Fastcoef_lin).NE.0 ) RETURN

      IF ( getparam('soilzone', 'fastcoef_sq', Nhru, 'real',
     +     Fastcoef_sq).NE.0 ) RETURN

      IF ( getparam('soilzone', 'sat_threshold', Nhru, 'real',
     +     Sat_threshold).NE.0 ) RETURN

      IF ( getparam('soilzone', 'ssstor_init', Nssr, 'real',
     +     Ssstor_init).NE.0 ) RETURN

      IF ( getparam('soilzone', 'ssr2gw_rate', Nssr, 'real',
     +     Ssr2gw_rate).NE.0 ) RETURN

      IF ( getparam('soilzone', 'ssr2gw_exp', Nssr, 'real', Ssr2gw_exp)
     +     .NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_moist_init', Nhru, 'real',
     +     Soil_moist_init).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_type', Nhru, 'integer', Soil_type)
     +     .NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_rechr_init', Nhru, 'real',
     +     Soil_rechr_init).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil2gw_max', Nhru, 'real',
     +     Soil2gw_max).NE.0 ) RETURN

      IF ( Model==0 .AND. Nhrucell>0 ) THEN
        IF ( getparam('soilzone', 'gvr_hru_id', Nhrucell, 'integer',
     +       Gvr_hru_id).NE.0 ) RETURN
      ENDIF

      First_run = 1

      Swale_limit = 0.0
      Soil2gw = 0
      ! Sanity checks for parameters
      DO i = 1, Nhru
        IF ( Soil_moist_init(i)<0.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, ' soil_moist_init specified <0, set to 0',
     +         Soil_moist_init(i)
          Soil_moist_init(i) = 0.0
        ENDIF
        IF ( Soil_rechr_init(i)<0.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, ' soil_rechr_init specified <0, set to 0',
     +         Soil_rechr_init(i)
          Soil_rechr_init(i) = 0.0
        ENDIF
        IF ( Ssstor_init(i)<0.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, ' ssstor_init specified <0, set to 0',
     +         Ssstor_init(i)
          Ssstor_init(i) = 0.0
        ENDIF
        IF ( Sat_threshold(i)<0.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, ' sat_threshold specified < 0, set to 0',
     +         Sat_threshold(i)
          Sat_threshold(i) = 0.0
        ENDIF
        IF ( Slowcoef_lin(i).GT.1.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, Slowcoef_lin(i),
     +         ' slowcoef_lin > 1.0, it is set to 1.0'
          Slowcoef_lin(i) = 1.0
        ELSEIF ( Slowcoef_lin(i)<NEARZERO ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, Slowcoef_lin(i),
     +         ' slowcoef_lin <= 0.0, it is set to last HRU value'
          IF ( i/=1 ) THEN
            Slowcoef_lin(i) = Slowcoef_lin(i-1)
          ELSE
            Slowcoef_lin(i) = 0.015
          ENDIF
        ENDIF
        IF ( Fastcoef_lin(i).GT.1.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, Fastcoef_lin(i),
     +         ' fastcoef_lin > 1.0, it is set to 1.0'
          Fastcoef_lin(i) = 1.0
        ELSEIF ( Fastcoef_lin(i)<NEARZERO ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, Fastcoef_lin(i),
     +         ' fastcoef_lin <= 0.0, it is set to last HRU value'
          IF ( i>1 ) THEN
            Fastcoef_lin(i) = Fastcoef_lin(i-1)
          ELSE
            Fastcoef_lin(i) = 0.1
          ENDIF
        ENDIF
        IF ( Hru_type(i).EQ.2 ) THEN
          IF ( Pref_flow_den(i).GT.0.0 ) THEN
            IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +           'Warning, pref_flow_den must be 0.0 for lakes',
     +           ' or impervious HRUs, reset from:', Pref_flow_den(i),
     +           ' to 0.0 for HRU:', i
            Pref_flow_den(i) = 0.0
          ENDIF
          Soil_rechr(i) = 0.0
          Soil_moist(i) = 0.0
          Replenish_pct(i) = 0.0
          Ssres_stor(i) = 0.0
        ELSE
          IF ( Soil2gw_max(i)>NEARZERO ) Soil2gw(i) = 1
          IF ( Soil_moist_max(i)>NEARZERO ) THEN
            Replenish_pct(i) = Soil_rechr_max(i)/Soil_moist_max(i)
          ELSE
            Replenish_pct(i) = 0.0
          ENDIF
          IF (Hru_type(i)==3 ) THEN ! swales
            IF ( Pref_flow_den(i)>0.0 ) THEN
              IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +             'Warning, pref_flow_den must be 0.0 for swale',
     +             ' HRUs, reset from:', Pref_flow_den(i),
     +             ' to 0.0 for HRU:', i
            ENDIF
            Pref_flow_den(i) = 0.0
            Swale_limit(i) = 3.0*Sat_threshold(i)
          ENDIF
        ENDIF
        Pref_flow_thrsh(i) = Sat_threshold(i)*(1.0-Pref_flow_den(i))
        Pref_flow_max(i) = Sat_threshold(i) - Pref_flow_thrsh(i)
        Soil_zone_max(i) = Sat_threshold(i) + Soil_moist_max(i)
      ENDDO

      IF ( Timestep==0 ) THEN
! initialize arrays (dimensioned Nhru)
        Dunnian_flow = 0.0
        Upslope_interflow = 0.0
        Upslope_dunnianflow = 0.0
        Infil_tot = 0.0
        Pref_flow_in = 0.0
        Slow_flow = 0.0
        Pref_flow = 0.0
        Gvr2sm = 0.0
        Gvr2pfr = 0.0
        Swale_actet = 0.0
        Soil_moist_tot = 0.0
        Soil_moist_pct = 0.0
        Hru_sz_cascadeflow = 0.0
        IF ( Numlakes>0 ) THEN
          Lakein_sz = 0.0
        ELSE
          DEALLOCATE ( Lakein_sz )
        ENDIF
        Ssres_in = 0.0
        Perv_actet = 0.0

! initialize arrays (dimensioned Nhrucell)
        IF ( Model==0 ) THEN
          Gw2sm_grav = 0.0
          Sm2gw_grav = 0.0
          Sm2gw_grav_old = 0.0
        ENDIF

! initialize scalers, if not a warm-start simulation
        Basin_ssin = 0.0
        Basin_ssr2gw = 0.0
        Basin_sm2gvr = 0.0
        Basin_dnflow = 0.0
        Basin_gvr2sm = 0.0
        Basin_sz2gw = 0.0
        Basin_dunnian = 0.0
        Basin_infil_tot = 0.0
        Basin_pref_flow_in = 0.0
        Basin_gvr2pfr = 0.0
        Basin_slowflow = 0.0
        Basin_prefflow = 0.0
        Basin_lakeprecip = 0.0
        Basin_lakeinsz = 0.0
        Basin_szfarflow = 0.0
! do only once so restart uses saved values
        Soil_rechr = Soil_rechr_init
        Soil_moist = Soil_moist_init
        Ssres_stor = Ssstor_init
      ENDIF

! initialize arrays (dimensioned Nhrucell)
      IF ( Model==0 ) THEN
        ALLOCATE ( Hru_gvr_count(Nhru) )
        Hru_gvr_count = 0
        max_gvrs = 0
        DO i = 1, Nhrucell
          ii = Gvr_hru_id(i)
          IF ( ii>Nhru) THEN
            PRINT *, 'Error, gvr_hru_id greater than number of HRUs'
            PRINT *, ' gvr_hru_id:', ii, ' nhru=', Nhru
            PRINT *, ' gvr number:', i
            RETURN
          ENDIF
          ! set only for cold start simulations
          IF ( Timestep==0 ) Gravity_stor_res(i) = Ssstor_init(ii)
          Hru_gvr_count(ii) = Hru_gvr_count(ii) + 1
          IF ( Hru_gvr_count(ii)>max_gvrs )
     +         max_gvrs = Hru_gvr_count(ii)
        ENDDO
        ALLOCATE ( Hru_gvr_index(max_gvrs, Nhru))
        DO i = 1, Nhru
          icnt = 0
          DO ii = 1, Nhrucell
            IF ( Gvr_hru_id(ii)==i ) THEN
              icnt = icnt + 1
              Hru_gvr_index(icnt, i) = ii
              IF ( icnt==Hru_gvr_count(i) ) EXIT
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      Basin_soil_moist = 0.0
      Basin_slstor = 0.0
      Basin_ssstor = 0.0
      Basin_pref_stor = 0.0
      Basin_soil_rechr = 0.0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Soil_rechr(i)>Soil_rechr_max(i) ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT,9003) i, Soil_rechr(i),
     +         Soil_rechr_max(i)
          Soil_rechr(i) = Soil_rechr_max(i)
        ENDIF
        IF ( Soil_moist(i).GT.Soil_moist_max(i) ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT,9004) i, Soil_moist(i),
     +         Soil_moist_max(i)
          Soil_moist(i) = Soil_moist_max(i)
        ENDIF
        IF ( Soil_rechr(i).GT.Soil_moist(i)) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT,9005) i, Soil_rechr(i),
     +         Soil_moist(i)
          Soil_rechr(i) = Soil_moist(i)
        ENDIF
        IF ( Ssres_stor(i).GT.Sat_threshold(i) ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *) 'HRU:', i,
     +         Ssres_stor(i), Sat_threshold(i),
     +         ' ssres_stor > sat_threshold, ssres_stor set to max'
          Ssres_stor(i) = Sat_threshold(i)
        ENDIF
        Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*Hru_perv(i)
        Slow_stor(i) = MIN(Ssres_stor(i), Pref_flow_thrsh(i))
        Pref_flow_stor(i) = Ssres_stor(i) - Slow_stor(i)
        Soil_moist_tot(i) = Ssres_stor(i)
     +                        + Soil_moist(i)*Hru_percent_perv(i)
        Basin_slstor = Basin_slstor + Slow_stor(i)*Hru_area(i)
        Basin_ssstor = Basin_ssstor + Ssres_stor(i)*Hru_area(i)
        Basin_pref_stor = Basin_pref_stor
     +                    + Pref_flow_stor(i)*Hru_area(i)
        Basin_soil_rechr = Basin_soil_rechr
     +                     + Soil_rechr(i)*Hru_perv(i)
      ENDDO
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_pref_stor = Basin_pref_stor*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
 
      Kkiter = 1 ! set for PRMS-only mode

      IF ( Print_debug==1 ) THEN
        OPEN (BALUNT, FILE='soilzone_prms.wbal')
        WRITE (BALUNT, 9001)
      ENDIF

      DEALLOCATE ( Ssstor_init, Soil_moist_init, Soil_rechr_init )

      szinit = 0

 9001 FORMAT ('    Date     Water Bal     bsmbal    last SM  soilmoist',
     +        '  last stor    SS stor    perv ET      sz2gw  interflow',
     +        '    soil2gw    Dunnian    soil in   lakeinsz   downflow',
     +        '     gvr2sm    farflow   swale ET  pref flow  iteration')
 9003 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_rechr > soil_rechr_max,', /, 29X,
     +        'soil_rechr set to soil_rechr_max')
 9004 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_moist > soil_moist_max,', /, 29X,
     +        'soil_moist set to soil_moist_max')
 9005 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_rechr > soil_moist,', /, 29X,
     +        'soil_rechr set to soil_moist')

      END FUNCTION szinit

!***********************************************************************
!     szrun - Does soil water balance for each HRU, adds in infiltration
!             then computes actual et and apportions remainder between
!             recharge of soil moisture, soil storage available for
!             interflow, excess routed to stream,
!             and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION szrun()
      USE PRMS_SOILZONE
      USE PRMS_MODULE, ONLY: Model, Ncascade
      USE PRMS_BASIN, ONLY: Hru_type, Hru_perv, Hru_percent_perv,
     +    Hru_route_order, Active_hrus, Print_debug, Basin_area_inv,
     +    Hru_area, Nhru, Nssr, NEARZERO, Numlakes
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Transp_on, Potet
! WARNING!!! Sroff, Basin_sroff, and Strm_seg_in can be updated
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_actet, Hru_actet,
     +    Ssres_flow, Soil_to_gw, Basin_soil_to_gw, Ssr_to_gw,
     +    Soil_to_ssr, Basin_lakeevap, Basin_perv_et, Basin_swale_et,
     +    Hru_impervevap, Sroff, Basin_sroff, Soil_moist_max, Infil,
     +    Soil_rechr_max, DBGUNT, Strm_seg_in, Strm_farfield
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_INTCP, ONLY: Hru_intcpevap, Cov_type
      USE PRMS_OBS, ONLY: Nowtime
      USE PRMS_SNOW, ONLY: Snowcov_area, Snow_evap
      IMPLICIT NONE
      INTRINSIC MIN, ABS, MAX
      INTEGER, EXTERNAL :: getvar
      EXTERNAL compute_soilmoist, compute_szactet, compute_cascades
      EXTERNAL compute_interflow, comp_inter_gwflow
      EXTERNAL compute_gravflow
! Local Variables
      INTEGER :: i, k, ncasc, month, day
      REAL :: avail_potet, soil_lower
      REAL :: perv_area, harea, soilin, bsmbal
!     REAL :: tmp
      REAL :: dunnianflw, last_ss, cap_waterin, waterin
      REAL :: basin_bal, soilbal, last_sm, interflow
      REAL :: gvrbal, dnslowflow, dnpreflow, dndunn, gwin
      REAL :: farflow_slow, farflow_pref, farflow_dunn
      REAL :: perv_pct, pref_flow_infil, soil_in
      REAL :: capacity, availh2o, test
      SAVE month, day
!***********************************************************************
      szrun = 1

      IF ( Model==0 ) THEN
        IF ( getvar('gsflow', 'KKITER', 1, 'integer', Kkiter)
     +       .NE.0 ) RETURN
       !rsr, get gvr pct now, as prms2mf init called after soilzone init
        IF ( First_run==1 ) THEN
          IF ( getvar('prms2mf', 'gvr_hru_pct_adjusted',Nhrucell,'real',
     +         Gvr_hru_pct_adjusted).NE.0 ) RETURN
          First_run = 0
        ENDIF
      ENDIF

      IF ( Kkiter.EQ.1 ) THEN
        month = Nowtime(2)
        day = Nowtime(3)

        Last_soil_moist = Basin_soil_moist
        Last_ssstor = Basin_ssstor

! It0 variables used with MODFLOW integration to save iteration states.
        IF ( Model==0 ) THEN
          DO k = 1, Active_hrus
            i = Hru_route_order(k)
            It0_soil_rechr(i) = Soil_rechr(i)
            It0_soil_moist(i) = Soil_moist(i)
            It0_pref_flow_stor(i) = Pref_flow_stor(i)
            It0_slow_stor(i) = Slow_stor(i)
            It0_ssres_stor(i) = Ssres_stor(i)
            It0_sroff(i) = Sroff(i)
          ENDDO
          It0_gravity_stor_res = Gravity_stor_res
          Sm2gw_grav = 0.0
          Gw2sm_grav = 0.0
          It0_strm_seg_in = Strm_seg_in
          It0_strm_farfield = Strm_farfield
!rsr, use value from last iteration for each time step instead of 0.0
!          IF ( getvar('mf2prms', 'gw2sm_grav', Nhrucell, 'real',
!     +         Gw2sm_grav).NE.0 ) RETURN
        ENDIF
      ELSE
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Soil_rechr(i) = It0_soil_rechr(i)
          Soil_moist(i) = It0_soil_moist(i)
          Ssres_stor(i) = It0_ssres_stor(i)
          Pref_flow_stor(i) = It0_pref_flow_stor(i)
          Slow_stor(i) = It0_slow_stor(i)
          Sroff(i) = It0_sroff(i)
        ENDDO
        Gravity_stor_res = It0_gravity_stor_res
        Sm2gw_grav_old = Sm2gw_grav
        Sm2gw_grav = 0.0
        Strm_seg_in = It0_strm_seg_in
        Strm_farfield = It0_strm_farfield
!rsr, use value from last iteration for each time step instead of 0.0
        IF ( getvar('mf2prms', 'gw2sm_grav', Nhrucell, 'real',
     +       Gw2sm_grav).NE.0 ) RETURN
      ENDIF

      IF ( Ncascade.GT.0 ) THEN
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Upslope_interflow(i) = 0.0
          Upslope_dunnianflow(i) = 0.0
        ENDDO
        IF ( Numlakes>0 ) THEN
          Lakein_sz = 0.0
          Basin_lakeinsz = 0.0
        ENDIF
      ENDIF

      Basin_actet = 0.0
      Basin_soil_moist = 0.0
      Basin_soil_rechr = 0.0
      Basin_perv_et = 0.0
      Basin_swale_et = 0.0
      Basin_lakeevap = 0.0
      Basin_soil_to_gw = 0.0
      Basin_lakeprecip = 0.0
      Basin_sz2gw = 0.0
      Basin_ssflow = 0.0
      Basin_ssstor = 0.0
      Basin_ssin = 0.0
      Basin_ssr2gw = 0.0
      Basin_sm2gvr = 0.0
      Basin_pref_stor = 0.0
      Basin_sroff = 0.0
      Basin_dunnian = 0.0
      basin_bal = 0.0
      soil_in = 0.0
      gwin = 0.0
      Basin_infil_tot = 0.0
      Basin_pref_flow_in = 0.0
      Basin_dnflow = 0.0
      Basin_gvr2pfr = 0.0
      Basin_slowflow = 0.0
      Basin_prefflow = 0.0
      Basin_szfarflow = 0.0
      Basin_gvr2sm = 0.0

      ncasc = 0
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        harea = Hru_area(i)
        perv_area = Hru_perv(i)

        ! Soil_to_gw for whole HRU
        ! Soil_to_ssr for whole HRU
        Soil_to_gw(i) = 0.0
        Soil_to_ssr(i) = 0.0
        Perv_actet(i) = 0.0
        Hru_actet(i) = Hru_impervevap(i) + Hru_intcpevap(i) +
     +                 Snow_evap(i)
        avail_potet = Potet(i) - Hru_actet(i)

        !Hru_type can be 1 (land) or 3 (swale)
        IF ( Hru_type(i).NE.2 ) THEN
          Slow_flow(i) = 0.0
          Ssr_to_gw(i) = 0.0
          Gvr2pfr(i) = 0.0

!******Add infiltration to soil and compute excess
          !rsr, note perv_area has to be > 0.0
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

          !rsr, perv_pct has to be above 0.0
          !rsr divide by perv_pct as it is a depth over the whole HRU
          cap_waterin = (Upslope_dunnianflow(i)+Upslope_interflow(i))
     +                  /perv_pct
          !infil for pervious portion of HRU
          soilin = Infil(i)
          IF ( Hru_type(i)==1 ) THEN
            ! times perv_pct because infil for pervious,
            ! but pref_flow for whole HRU
            pref_flow_infil = soilin*Pref_flow_den(i)*perv_pct
            cap_waterin = cap_waterin + soilin -
     +                    pref_flow_infil/perv_pct
            Pref_flow(i) = 0.0
          ELSE
            ! for swales
            cap_waterin = cap_waterin + soilin
            dunnianflw = 0.0
            interflow = 0.0
            pref_flow_infil = 0.0
            !rsr, remove later
!           if(ABS(Pref_flow(i))>NEARZERO) print*,'swale pref problem'
          ENDIF
          Infil_tot(i) = cap_waterin*perv_pct
          Basin_infil_tot = Basin_infil_tot + Infil_tot(i)*harea

!******Add infiltration to soil and compute excess
          !rsr, note perv_area has to be > 0.0

!rsr soil moisture assumed only in perv_area

          CALL compute_soilmoist(cap_waterin, Soil_moist_max(i),
     +         Soil_rechr_max(i), Soil2gw_max(i), Soil_to_ssr(i),
     +         Soil_moist(i), Soil_rechr(i), Soil_to_gw(i), Soil2gw(i),
     +         perv_pct)
          Ssres_in(i) = Soil_to_ssr(i)

! compute interflow and ssr_to_gw
          IF ( Model==0 ) THEN
            capacity = Soil_moist_max(i) - Soil_moist(i)
            Gvr2sm(i) = 0.0
            CALL compute_gravflow(i, capacity, Slowcoef_lin(i),
     +           Slowcoef_sq(i), Ssr2gw_rate(i), Ssr2gw_exp(i),
     +           Pref_flow_thrsh(i), Ssres_in(i), Gvr2pfr(i),
     +           Ssr_to_gw(i), Slow_flow(i), Slow_stor(i), Gvr2sm(i),
     +           Soil_to_gw(i), gwin, perv_pct, Hru_type(i))
            ! adjust soil moisture with replenish amount
            !rsr, perv_pct has to be above zero now
            IF ( Gvr2sm(i)>0.0 ) THEN
              Soil_moist(i) = Soil_moist(i) + Gvr2sm(i)/perv_pct
!             IF ( Soil_moist(i).GT.Soil_moist_max(i) )
!    +             PRINT *, 'sm>max', Soil_moist(i), Soil_moist_max(i),i
              Soil_rechr(i) = Soil_rechr(i) +
     +                        Gvr2sm(i)/perv_pct*Replenish_pct(i)
              Soil_rechr(i) = MIN(Soil_rechr_max(i), Soil_rechr(i))
              Basin_gvr2sm = Basin_gvr2sm + Gvr2sm(i)*harea
            ENDIF
            Ssres_in(i) = Ssres_in(i) + gwin
          ELSE
            IF ( Ssres_in(i)>0.0 .OR. Slow_stor(i)>0.0 )
     +           CALL comp_inter_gwflow(Slowcoef_lin(i), Slowcoef_sq(i),
     +                Ssr2gw_rate(i), Ssr2gw_exp(i), Pref_flow_thrsh(i),
     +                Soil_to_ssr(i), Gvr2pfr(i), Ssr_to_gw(i),
     +                Slow_flow(i), Slow_stor(i), Hru_type(i))
          ENDIF

!******Compute actual evapotranspiration

          IF ( Soil_moist(i)>0.0 ) THEN
            CALL compute_szactet(Soil_moist_max(i), Soil_rechr_max(i),
     +           Snowcov_area(i), Transp_on(i), Cov_type(i),
     +           Soil_type(i), Soil_moist(i), Soil_rechr(i),
     +           Perv_actet(i), avail_potet)
          ENDIF
          IF ( Soil_moist(i)<0.0 ) THEN
            IF ( Perv_actet(i)>=ABS(Soil_moist(i)) ) THEN
              Perv_actet(i) = Perv_actet(i) + Soil_moist(i)
              Soil_moist(i) = 0.0
            ENDIF
            IF ( Soil_moist(i)<-NEARZERO ) THEN
              IF ( Print_debug==7 ) WRITE (DBGUNT, *) 'HRU:', i,
     +             ' soil_moist<0.0', Soil_moist(i)
            ENDIF
            Soil_moist(i) = 0.0
          ENDIF
          Hru_actet(i) = Hru_actet(i) + Perv_actet(i)*perv_pct
!         tmp = Potet(i) - Hru_actet(i)
!         IF ( tmp<0.0 ) THEN
!           PRINT *, 'hru_actet>potet', i, month, day, Hru_actet(i),
!    +               Potet(i)
!           Hru_actet(i) = Potet(i)
!         ENDIF

! compute contribution to Dunnian flow, if any
          IF ( Hru_type(i)==1 ) THEN
            Pref_flow_in(i) = pref_flow_infil + Gvr2pfr(i)
            Basin_pref_flow_in = Basin_pref_flow_in +
     +                           Pref_flow_in(i)*harea
            Pref_flow_stor(i) = Pref_flow_stor(i) + Pref_flow_in(i)
            Ssres_in(i) = Ssres_in(i) + pref_flow_infil
            dunnianflw = MAX(0.0, Pref_flow_stor(i)-Pref_flow_max(i))
            Pref_flow_stor(i) = Pref_flow_stor(i) - dunnianflw
            availh2o = Pref_flow_stor(i)
! compute preferential flow, if any
            IF ( availh2o.GT.0.0 ) CALL compute_interflow
     +           (Fastcoef_lin(i), Fastcoef_sq(i), 0.0,
     +           Pref_flow_stor(i), Pref_flow(i), availh2o)

! if hru cascades,
! compute interflow and excess flow to each hru or stream
            interflow = Slow_flow(i) + Pref_flow(i)
            Dunnian_flow(i) = dunnianflw
            IF ( Ncascade>0 ) THEN
              ncasc = Ncascade_hru(i)
              IF ( ncasc>0 ) THEN
                IF ( interflow.GT.0.0 .OR. dunnianflw.GT.0.0 )
     +               CALL compute_cascades(i, ncasc, Slow_flow(i),
     +                    Pref_flow(i), Dunnian_flow(i), dnslowflow,
     +                    dnpreflow, dndunn, farflow_slow, farflow_pref,
     +                    farflow_dunn)
                Hru_sz_cascadeflow(i) = dnslowflow + dnpreflow + dndunn
     +                      + farflow_slow + farflow_pref + farflow_dunn
              ENDIF
            ENDIF
! treat pref_flow as interflow
! note ssres_flow does not include farfield flow as it leaves the stream network
!      and model domain, thus not included in basin_cfs
            Ssres_flow(i) = Slow_flow(i) + Pref_flow(i)

            Basin_dnflow = Basin_dnflow +
     +                     (dnslowflow+dnpreflow+dndunn)*harea
            Basin_szfarflow = Basin_szfarflow +
     +                    (farflow_slow+farflow_pref+farflow_dunn)*harea

! treat dunnianflw as surface runoff to streams
! note sroff does not include farfield flow as it leaves the stream network
!      and model domain, thus not included in basin_cfs
            Sroff(i) = Sroff(i) + Dunnian_flow(i)
            Basin_sroff = Basin_sroff + Sroff(i)*harea
            Basin_pref_stor = Basin_pref_stor + Pref_flow_stor(i)*harea
            Basin_ssflow = Basin_ssflow + Ssres_flow(i)*harea
            Basin_dunnian = Basin_dunnian + Dunnian_flow(i)*harea
            Basin_slowflow = Basin_slowflow + Slow_flow(i)*harea
            Basin_prefflow = Basin_prefflow + Pref_flow(i)*harea
            Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)
!            if(ssres_stor(i)>100.)print*,slow_stor(i),pref_flow_stor(i)
          ELSE
            ! for swales
            availh2o = Slow_stor(i) - Sat_threshold(i)
            Swale_actet(i) = 0.0
            IF ( availh2o>0.0 ) THEN
              Swale_actet(i) = Potet(i) - Hru_actet(i)
              IF ( Swale_actet(i)>0.0 ) THEN
                IF ( Swale_actet(i)>availh2o ) Swale_actet(i) = availh2o
                Hru_actet(i) = Hru_actet(i) + Swale_actet(i)
                Slow_stor(i) = Slow_stor(i) - Swale_actet(i)
              ENDIF
              IF ( Print_debug==7 ) THEN
                IF ( Slow_stor(i)>Swale_limit(i) ) THEN
                  WRITE (DBGUNT, *) 'Swale ponding, HRU:', i,
     +                          ' gravity reservoir is 3*sat_threshold',
     +                          Slow_stor(i), Sat_threshold(i), Nowtime
                ENDIF
              ENDIF
            ENDIF
            Ssres_stor(i) = Slow_stor(i)
          ENDIF

          Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*perv_pct
          Soil_moist_pct(i) = Soil_moist_tot(i)/Soil_zone_max(i)

          Basin_ssstor = Basin_ssstor + Ssres_stor(i)*harea
          Basin_slstor = Basin_slstor + Slow_stor(i)*harea
          Basin_ssin = Basin_ssin + Ssres_in(i)*harea
          Basin_sm2gvr = Basin_sm2gvr + Soil_to_ssr(i)*harea
          Basin_sz2gw = Basin_sz2gw + Ssr_to_gw(i)*harea
! ghl1299
! soil_moist & soil_rechr multiplied by perv_area instead of harea
          Basin_soil_to_gw = Basin_soil_to_gw + Soil_to_gw(i)*harea
          Basin_soil_rechr = Basin_soil_rechr + Soil_rechr(i)*perv_area
          Basin_perv_et = Basin_perv_et + Perv_actet(i)*perv_area
          Basin_swale_et = Basin_swale_et + Swale_actet(i)*harea
          Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*perv_area
          Basin_gvr2pfr = Basin_gvr2pfr + Gvr2pfr(i)*harea
          IF ( Print_debug==1 ) THEN
            soilbal = Infil_tot(i) + (last_sm-Soil_moist(i)-
     +              Perv_actet(i))*perv_pct - Soil_to_ssr(i)
     +              - Soil_to_gw(i) + Gvr2sm(i)
            IF ( ABS(soilbal).GT.TOOSMALL ) WRITE (BALUNT, *) soilbal,
     +           Infil_tot(i), Gvr2sm(i), last_sm, Soil_moist(i),
     +           Perv_actet(i), Soil_to_ssr(i), Soil_to_gw(i), i,
     +           Infil(i), Pref_flow_in(i), Upslope_interflow(i),
     +           Upslope_dunnianflow(i), perv_pct, 'soilbal problem'
            gvrbal = last_ss - Ssres_stor(i) + Ssres_in(i)
     +               - Dunnian_flow(i) - Gvr2sm(i) - Ssr_to_gw(i)
     +               - dnslowflow - dnpreflow - dndunn - farflow_slow
     +               - farflow_pref - farflow_dunn - Ssres_flow(i)
     +               - Swale_actet(i)
            test = ABS(gvrbal)
            IF ( test>TOOSMALL ) THEN
              IF ( test>Ssres_stor(i)*TOOSMALL ) WRITE(BALUNT,*) gvrbal,
     +           last_ss, Ssres_stor(i), Ssres_in(i), Ssres_flow(i),
     +           Dunnian_flow(i), Gvr2sm(i), Ssr_to_gw(i), dnslowflow,
     +           dnpreflow, dndunn, dunnianflw, Soil_to_ssr(i), gwin,
     +           Pref_flow_in(i), farflow_slow, farflow_pref, interflow,
     +           farflow_dunn, pref_flow_infil, i, Gvr2pfr(i), 'bad gvr'
     +           , Hru_type(i), Swale_actet(i), perv_pct, perv_actet(i),
     +           Hru_sz_cascadeflow(i)
            ENDIF
            soil_in = soil_in + Infil(i)*perv_area +
     +                gwin*harea
!    +                - pref_flow_infil*harea
            waterin = Infil(i)*perv_pct + gwin +
     +                Upslope_dunnianflow(i) + Upslope_interflow(i)
            ! have to figure out which are perv area and hru_area
            soilbal = waterin + last_ss - Ssres_stor(i) +
     +                (last_sm-Soil_moist(i)-Perv_actet(i))*perv_pct -
     +                Ssr_to_gw(i) - interflow - dunnianflw -
     +                Soil_to_gw(i) - Swale_actet(i)
            basin_bal = basin_bal + soilbal*harea
            test = ABS(soilbal)
            IF ( test>TOOSMALL ) THEN
              IF ( test>Ssres_stor(i)*TOOSMALL ) THEN
                WRITE (BALUNT, *) 'Hru_type', Hru_type(i)
                IF ( test>5.0E-3 ) THEN
                  WRITE (BALUNT, *) 'HRU water balance ***ERROR***'
                ELSEIF ( test>5.0E-4 ) THEN
                  WRITE (BALUNT, *) 'possible HRU water balance error'
                ELSE
                  WRITE (BALUNT, *)
     +                  'possible HRU water balance rounding issue'
                ENDIF
                soil_lower = Soil_moist(i) - Soil_rechr(i)
                WRITE (BALUNT,9001) Nowtime(1), month, day, i, Kkiter,
     +                soilbal, Infil(i), gwin, Upslope_dunnianflow(i),
     +                Upslope_interflow(i), last_sm, last_ss,
     +                Soil_moist(i), Ssres_stor(i), Perv_actet(i),
     +                Ssr_to_gw(i), interflow,Slow_flow(i),Pref_flow(i),
     +                dunnianflw, Soil_to_gw(i), Pref_flow_in(i),
     +                Pref_flow_stor(i), Slow_stor(i), Soil_rechr(i),
     +                soil_lower, Soil_to_ssr(i), Ssres_flow(i),waterin,
     +                Gvr2sm(i), Swale_actet(i)
                WRITE (BALUNT,*) perv_area, perv_pct, ncasc,
     +                           Pref_flow_den(i)
              ENDIF
            ENDIF
          ENDIF

        ELSE ! else it is a lake or reservoir
          Hru_actet(i) = Hru_actet(i) + avail_potet
          Basin_lakeevap = Basin_lakeevap + Hru_actet(i)*harea
          Basin_lakeprecip = Basin_lakeprecip + Hru_ppt(i)*harea
          IF ( Ncascade>0 ) THEN
                  ! if lake HRU doesn't cascade, should we limit ET to
                  !  water entering the HRU to this point (no gwflow
                  !  yet)
            Lakein_sz(i) = Upslope_interflow(i) + Upslope_dunnianflow(i)
            Basin_lakeinsz = Basin_lakeinsz + Lakein_sz(i)*harea
          ENDIF
        ENDIF

        Basin_actet = Basin_actet + Hru_actet(i)*harea

!        IF ( Hru_type(i)==3 ) THEN
!          IF ( Sroff(i)>0.0 ) PRINT *, 'swale runoff problem', i,
!     +                                 Sroff(i), Hru_type(i)
!          IF ( Gvr2pfr(i)>0.0 ) PRINT *, 'swale gvr2pfr problem', i,
!     +                                   Gvr2pfr(i), Hru_type(i)
!        ENDIF
      ENDDO

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_swale_et = Basin_swale_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_lakeevap = Basin_lakeevap*Basin_area_inv
      Basin_pref_stor = Basin_pref_stor*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_ssflow = Basin_ssflow*Basin_area_inv
      Basin_sz2gw = Basin_sz2gw*Basin_area_inv
      Basin_ssr2gw = Basin_sz2gw
      Basin_ssin = Basin_ssin*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_dunnian = Basin_dunnian*Basin_area_inv
      Basin_sm2gvr = Basin_sm2gvr*Basin_area_inv
      Basin_infil_tot = Basin_infil_tot*Basin_area_inv
      Basin_pref_flow_in = Basin_pref_flow_in*Basin_area_inv
      Basin_dnflow = Basin_dnflow*Basin_area_inv
      Basin_szfarflow = Basin_szfarflow*Basin_area_inv
      Basin_gvr2sm = Basin_gvr2sm*Basin_area_inv
      Basin_gvr2pfr = Basin_gvr2pfr*Basin_area_inv
      Basin_slowflow = Basin_slowflow*Basin_area_inv
      Basin_prefflow = Basin_prefflow*Basin_area_inv
      Basin_lakeprecip = Basin_lakeprecip*Basin_area_inv
      Basin_lakeinsz = Basin_lakeinsz*Basin_area_inv

      IF ( Print_debug.EQ.1 ) THEN
        gvrbal = Last_ssstor - Basin_ssstor + Basin_ssin -
     +           Basin_ssflow - Basin_dunnian - Basin_gvr2sm -
     +           Basin_sz2gw - Basin_dnflow - Basin_szfarflow -
     +           Basin_swale_et
        IF ( ABS(gvrbal).GT.5.0E-4 ) WRITE (BALUNT, *) 'basinbal',
     +      gvrbal, Last_ssstor, Basin_ssstor, Basin_ssin, Basin_ssflow,
     +      Basin_dunnian, Basin_gvr2sm, Basin_sz2gw, Basin_sm2gvr,
     +      Basin_pref_flow_in, Basin_dnflow, Basin_pref_stor,
     +      Basin_szfarflow, Kkiter

        soilbal = Last_soil_moist - Basin_soil_moist - Basin_perv_et -
     +            Basin_soil_to_gw - Basin_sm2gvr + Basin_gvr2sm +
     +            Basin_infil_tot
        IF ( ABS(soilbal).GT.TOOSMALL ) WRITE(BALUNT,*)
     +       'possible basin capillary balance issue', soilbal

        soilbal = Last_ssstor - Basin_ssstor + Basin_ssin -
     +            Basin_ssflow - Basin_dunnian -
     +            Basin_sz2gw - Basin_dnflow - Basin_szfarflow +
     +            Last_soil_moist - Basin_soil_moist - Basin_perv_et -
     +            Basin_soil_to_gw - Basin_sm2gvr +
     +            Basin_infil_tot - Basin_swale_et
        IF ( ABS(soilbal).GT.TOOSMALL ) WRITE (BALUNT, *)
     +       'possible basin soil zone rounding issue', soilbal

        soil_in = soil_in*Basin_area_inv
        basin_bal = basin_bal*Basin_area_inv
        bsmbal = Last_soil_moist - Basin_soil_moist + Last_ssstor -
     +           Basin_ssstor - Basin_perv_et - Basin_sz2gw + soil_in -
     +           Basin_ssflow - Basin_soil_to_gw - Basin_dunnian -
     +           Basin_lakeinsz - Basin_szfarflow - Basin_swale_et

        WRITE (BALUNT, 9002) Nowtime(1), month, day, basin_bal, bsmbal,
     +        Last_soil_moist, Basin_soil_moist, Last_ssstor,
     +        Basin_ssstor, Basin_perv_et, Basin_sz2gw, Basin_ssflow,
     +        Basin_soil_to_gw, Basin_dunnian, soil_in, Basin_lakeinsz,
     +        Basin_dnflow, Basin_gvr2sm, Basin_szfarflow,
     +        Basin_swale_et, Basin_prefflow, Kkiter
        IF ( ABS(bsmbal).GT.0.05 .OR. ABS(basin_bal).GT.0.001 ) THEN
          WRITE (BALUNT, *) '*ERROR, basin water balance', bsmbal,
     +                      basin_bal
          WRITE (BALUNT, *) Basin_pref_stor, Basin_slstor
        ELSEIF ( ABS(bsmbal).GT.0.005 .OR. ABS(basin_bal).GT.0.0001 )
     +           THEN
          WRITE (BALUNT, *) 'Possible water balance error', bsmbal,
     +                      basin_bal
        ELSEIF ( ABS(bsmbal).GT.0.0005 .OR. ABS(basin_bal).GT.TOOSMALL )
     +           THEN
          WRITE (BALUNT, '(A,2F12.7)') 'Basin rounding issue', bsmbal,
     +                                 basin_bal
          WRITE (BALUNT, *) soilbal, Basin_ssin, Basin_dnflow,
     +                      Basin_sm2gvr, Basin_infil_tot, soil_in,
     +                      Basin_gvr2sm, Basin_gvr2sm
        ENDIF
      ENDIF

      szrun = 0

 9001 FORMAT (I5, 2('/', I2.2), 2I5, 26F11.5)
 9002 FORMAT (I5, 2('/', I2.2), 18F11.5, I7)

      END FUNCTION szrun

!***********************************************************************
!     Add infiltration to soil and compute excess
!     Soil_to_gw for whole HRU
!     Soil_to_ssr for whole HRU
!***********************************************************************
      SUBROUTINE compute_soilmoist(Infil, Soil_moist_max,
     +           Soil_rechr_max, Soil2gw_max, Soil_to_ssr, Soil_moist,
     +           Soil_rechr, Soil_to_gw, Soil2gw, Perv_pct)
      USE PRMS_OBS, ONLY: Timestep_days
      IMPLICIT NONE
      INTRINSIC MIN
! Arguments
      INTEGER, INTENT(IN) :: Soil2gw
      REAL, INTENT(IN) :: Infil, Perv_pct
      REAL, INTENT(IN) :: Soil_moist_max, Soil_rechr_max, Soil2gw_max
      REAL, INTENT(INOUT) :: Soil_moist, Soil_rechr
      REAL, INTENT(INOUT) :: Soil_to_ssr, Soil_to_gw
! Local Variables
      REAL :: excs
!***********************************************************************
      Soil_rechr = MIN((Soil_rechr+Infil), Soil_rechr_max)
      ! may need to check soil_moist as gwin, could have set it above
      ! soil_moist_max from previous time step
      excs = Soil_moist + Infil
      Soil_moist = MIN(excs, Soil_moist_max)
      excs = (excs - Soil_moist_max)*Perv_pct
      IF ( excs>0.0 ) THEN
        IF ( Soil2gw==1 )
     +       Soil_to_gw = MIN(Soil2gw_max*Timestep_days, excs)
        ! warning, Soil_to_gw set to zero in init, doesn't change if
        ! soil2gw isn't dynamic, if so need to set to zero
        Soil_to_ssr = excs - Soil_to_gw
      ENDIF

      END SUBROUTINE compute_soilmoist

!***********************************************************************
!     Compute actual evapotranspiration
!***********************************************************************
      SUBROUTINE compute_szactet(Soil_moist_max, Soil_rechr_max,
     +           Snowcov_area, Transp_on, Cov_type, Soil_type,
     +           Soil_moist, Soil_rechr, Perv_actet, Avail_potet)
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Transp_on, Cov_type, Soil_type
      REAL, INTENT(IN) :: Soil_moist_max, Soil_rechr_max
      REAL, INTENT(IN) :: Snowcov_area
      REAL, INTENT(INOUT) :: Soil_moist, Soil_rechr, Avail_potet
      REAL, INTENT(OUT) :: Perv_actet
! Local Variables
      REAL, PARAMETER :: ONETHIRD = 1.0/3.0, TWOTHIRDS = 2.0/3.0
      INTEGER :: et_type
      REAL :: et, open_ground, pcts, pctr, ets, etr
!***********************************************************************
      ets = 0.0
      etr = 0.0

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
        pctr = Soil_rechr/Soil_rechr_max
        ets = Avail_potet
        etr = Avail_potet

!******sandy soil
        IF ( Soil_type.EQ.1 ) THEN
          IF ( pcts.LT.0.25 ) ets = 0.5*pcts*Avail_potet
          IF ( pctr.LT.0.25 ) etr = 0.5*pctr*Avail_potet
!******loam soil
        ELSEIF ( Soil_type.EQ.2 ) THEN
          IF ( pcts.LT.0.5 ) ets = pcts*Avail_potet
          IF ( pctr.LT.0.5 ) etr = pctr*Avail_potet
!******clay soil
        ELSEIF ( Soil_type.EQ.3 ) THEN
          IF ( pcts.LT.TWOTHIRDS .AND. pcts.GT.ONETHIRD ) THEN
            ets = pcts*Avail_potet
          ELSEIF ( pcts.LE.ONETHIRD ) THEN
            ets = 0.5*pcts*Avail_potet
          ENDIF
          IF ( pctr.LT.TWOTHIRDS .AND. pctr.GT.ONETHIRD ) THEN
            etr = pctr*Avail_potet
          ELSEIF ( pctr.LE.ONETHIRD ) THEN
            etr = 0.5*pctr*Avail_potet
          ENDIF
        ENDIF

!******Soil moisture accounting
        IF ( et_type.EQ.2 ) etr = etr*open_ground
        IF ( etr.GT.Soil_rechr ) THEN
          etr = Soil_rechr
          Soil_rechr = 0.0
        ELSE
          Soil_rechr = Soil_rechr - etr
        ENDIF
        IF ( et_type.EQ.2 .OR. etr.GE.ets ) THEN
          IF ( etr.GT.Soil_moist ) THEN
            etr = Soil_moist
            Soil_moist = 0.0
          ELSE
            Soil_moist = Soil_moist - etr
          ENDIF
          et = etr
        ELSEIF ( ets.GT.Soil_moist ) THEN
          et = Soil_moist
          Soil_moist = 0.0
        ELSE
          Soil_moist = Soil_moist - ets
          et = ets
        ENDIF
        IF ( Soil_rechr.GT.Soil_moist ) Soil_rechr = Soil_moist
      ELSE
        et = 0.0
      ENDIF
      Perv_actet = et
      !rsr, sanity check
      IF ( Perv_actet>Avail_potet ) THEN
        Soil_moist = Soil_moist + Perv_actet - Avail_potet
        Perv_actet = Avail_potet
        PRINT *, 'perv_et problem', perv_actet, avail_potet
      ENDIF

      END SUBROUTINE compute_szactet

!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE comp_inter_gwflow(Slowcoef_lin, Slowcoef_sq,
     +           Ssr2gw_rate, Ssr2gw_exp, Pref_flow_thrsh, Ssres_in,
     +           Gvr2pfr, Ssr_to_gw, Slow_flow, Slow_stor, Hru_type)
      USE PRMS_BASIN, ONLY: NEARZERO
      USE PRMS_OBS, ONLY: Timestep_days
      IMPLICIT NONE
      EXTERNAL check_gravity, compute_interflow
! Arguments
      !rsr, capacity, perv_pct, and gvr2sm removed as capillary
      !     reservoir not replenished in PRMS-only mode
      INTEGER, INTENT(IN) :: Hru_type
!     REAL, INTENT(IN) :: Capacity, Perv_pct
      REAL, INTENT(IN) :: Slowcoef_lin, Slowcoef_sq
      REAL, INTENT(IN) :: Ssr2gw_rate, Ssr2gw_exp
      REAL, INTENT(IN) :: Pref_flow_thrsh, Ssres_in
      REAL, INTENT(INOUT) :: Slow_stor
!     REAL, INTENT(INOUT) :: Gvr2sm
      REAL, INTENT(INOUT) :: Ssr_to_gw, Slow_flow, Gvr2pfr
! Local Variables
      REAL :: input, availh2o
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        input = Ssres_in
        availh2o = Ssres_in + Slow_stor
        CALL check_gravity(Pref_flow_thrsh, Slow_stor, Gvr2pfr, input,
     +       availh2o)

! compute slow contribution to interflow, if any
        IF ( availh2o>0.0 ) CALL compute_interflow(Slowcoef_lin,
     +       Slowcoef_sq, input, Slow_stor, Slow_flow, availh2o)
      ELSE ! Hru_type = 3
        Slow_stor = Slow_stor + Ssres_in
      ENDIF

!******compute flow to groundwater
      IF ( Slow_stor.GT.0.0 ) THEN
        IF ( Ssr2gw_rate.GT.NEARZERO ) THEN
          Ssr_to_gw = Ssr2gw_rate*Timestep_days*(Slow_stor**Ssr2gw_exp)
          IF ( Ssr_to_gw>Slow_stor ) THEN
!remove this print after debugging
!         PRINT *, '***Issue in soilzone computing drainage',
!    +             Ssr_to_gw, Slow_stor
            Ssr_to_gw = Slow_stor
          ENDIF
          IF ( Ssr_to_gw.LT.0.0 ) Ssr_to_gw = 0.0
          Slow_stor = Slow_stor - Ssr_to_gw
        ENDIF
      ENDIF

      END SUBROUTINE comp_inter_gwflow

!***********************************************************************
!     Compute subsurface lateral flow
!***********************************************************************
      SUBROUTINE compute_interflow(Coef_lin, Coef_sq, Input, Storage,
     +           Inter_flow, Availh2o)
!     USE PRMS_OBS, ONLY: Nowtime
      USE PRMS_FLOWVARS, ONLY: DBGUNT
      USE PRMS_BASIN, ONLY: Print_debug, NEARZERO
      USE PRMS_OBS, ONLY: Timestep_days
      IMPLICIT NONE
      INTRINSIC EXP, SQRT
! Arguments
      REAL, INTENT(IN) :: Coef_lin, Coef_sq, Input
      REAL, INTENT(INOUT) :: Storage, Availh2o
      REAL, INTENT(OUT) :: Inter_flow
! Local Variables
      REAL :: c1, c2, c3, sos
!     REAL :: tmp
!***********************************************************************
! remove after debugging
!     IF ( Storage.LT.0.0 .OR. Input.LT.0.0 ) THEN
!       PRINT *, "Sanity check in compute_interflow:"
!       PRINT *, "   Storage = ", Storage, " Input = ", Input, Nowtime
!       STOP
!     ENDIF

      Inter_flow = 0.0
! Inter_flow is in inches for the timestep
!******compute interflow
      IF ( Coef_lin.LT.NEARZERO .AND. Input.LE.0.0 ) THEN
        c1 = Coef_sq*Storage
        Inter_flow = Storage*(c1/(1.0+c1))
      ELSEIF ( Coef_sq.LT.NEARZERO ) THEN
        c2 = 1.0 - EXP(-Coef_lin*Timestep_days)
        Inter_flow = Input*(1.0-c2/Coef_lin*Timestep_days) + Storage*c2
      ELSE
        c3 = SQRT(Coef_lin**2.0+4.0*Coef_sq*Input/Timestep_days)
        sos = Storage - ((c3-Coef_lin)/(2.0*Coef_sq))
        c1 = Coef_sq*sos/c3
        c2 = 1.0 - EXP(-c3*Timestep_days)
        Inter_flow = Input + (sos*(1.0+c1)*c2)/(1.0+c1*c2)
      ENDIF

! sanity check
!     IF ( Inter_flow.LT.-1.0E-10 ) PRINT *, 'interflow<0', Inter_flow
      IF ( Inter_flow.LT.0.0 ) THEN
        Inter_flow = 0.0
      ELSEIF ( Inter_flow.GT.Availh2o ) THEN
        Inter_flow = Availh2o
      ENDIF
      Storage = Availh2o - Inter_flow
      IF ( Storage.LT.0.0 ) THEN
        IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +       'Sanity check, ssres_stor<0.0', Storage
        Storage = 0.0
! rsr, if very small storage, add it to interflow
      ELSEIF ( Storage.LT.NEARZERO ) THEN
!       PRINT *, 'small', Storage
        Inter_flow = Inter_flow + Storage
        Storage = 0.0
      ENDIF
! the following makes no difference, values too small
!     tmp = Availh2o - Inter_flow - Storage
!     IF ( ABS(tmp).GT.0.0 ) THEN
!       PRINT *, 'comp_in', Availh2o, Inter_flow, Storage, tmp
!       IF ( Storage.GT.ABS(tmp) ) THEN
!         Storage = Storage + tmp
!       ELSE
!         Inter_flow = Inter_flow + tmp
!       ENDIF
!       tmp = Availh2o - Inter_flow - Storage
!       PRINT *, 'comp_in 2', Availh2o, Inter_flow, Storage, tmp
!     ENDIF

      Availh2o = Storage + Input

      END SUBROUTINE compute_interflow

!***********************************************************************
!     adjust soil moist based on being below field capacity (capacity)
!     and preferential-flow threshold (Pref_flow_thrsh)
!***********************************************************************
      SUBROUTINE check_gravity(Pref_flow_thrsh, Depth, Gvr2pfr, Input,
     +           Availh2o)
      !rsr, note removed capacity, perv_pct, and gvr2sm from argument
      !     list as not used in PRMS-only mode
      IMPLICIT NONE
      INTRINSIC MAX
! Arguments
      REAL, INTENT(IN) :: Pref_flow_thrsh
!     REAL, INTENT(IN) :: Perv_pct, Capacity
      REAL, INTENT(INOUT) :: Depth, Input, Availh2o
      REAL, INTENT(OUT) :: Gvr2pfr
!     REAL, INTENT(OUT) :: Gvr2sm
! Local Variables
!     REAL :: to_sm
!***********************************************************************
! check to see if soil is below capacity, if so add up to field capacity
! do not replenish capillary reservoir
! Capacity is in pervious portion
! to_sm and Gvr2sm are for whole HRU
!     IF ( Capacity>0.0 ) THEN
!       IF ( Capacity*Perv_pct<Availh2o ) THEN
          !rsr, fill up capillary with part of gravity water
!         to_sm = Capacity*Perv_pct
!       ELSE
          !rsr, take all gravity water and put in capillary
!         to_sm = Availh2o
!       ENDIF
! compute adjustment to soil moist to field capacity
!       Availh2o = Availh2o - to_sm
!       Gvr2sm = to_sm
!     ELSE
!       to_sm = 0.0
!     ENDIF

! compute contribution to preferential-flow reservoir storage, if any
! adjust depth and input, take from input first
      Gvr2pfr = MAX(0.0, Availh2o-Pref_flow_thrsh)
      Input = Input - Gvr2pfr

      IF ( Input.LT.0.0 ) THEN
!rsr??? how should this be done
!        IF ( Input<-1.0E-05 ) PRINT *,
!     +       'warning, input<0 in check_gravity', Depth, Gvr2pfr, Input
!     +       , to_sm, Capacity
        Depth = Depth + Input
        IF ( Depth.LT.0.0 ) Depth = 0.0
!        IF ( Depth.LT.0.0 ) THEN
!          IF ( Depth<-1.0E-5 )
!     +         PRINT *, 'warning in check_gravity', Input, Depth
!          Depth = 0.0
!        ENDIF
        Input = 0.0
      ENDIF

      Availh2o = Input + Depth

      END SUBROUTINE check_gravity

!***********************************************************************
!     Compute cascading interflow and excess flow
!***********************************************************************
      SUBROUTINE compute_cascades(Ihru, Ncascade_hru, Slowflow, Preflow,
     +           Dunnian, Dnslowflow, Dnpreflow, Dndunnflow,
     +           Farflow_slow, Farflow_pref, Farflow_dunn)
      USE PRMS_BASIN, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_SOILZONE, ONLY: Upslope_dunnianflow, Upslope_interflow
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_pct, Hru_down_pctwt,
     +    Cascade_area
      USE PRMS_OBS, ONLY: Cfs_conv
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
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j.GT.0 ) THEN
          pctwt = Hru_down_pctwt(k, Ihru)
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

      END SUBROUTINE compute_cascades

!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE compute_gravflow(Ihru, Capacity, Slowcoef_lin,
     +           Slowcoef_sq, Ssr2gw_rate, Ssr2gw_exp,
     +           Pref_flow_thrsh, Ssres_in, Gvr2pfr, Ssr_to_gw,
     +           Slow_flow, Slow_stor, Gvr2sm, Soil_to_gw, Gwin,
     +           Perv_pct, Hru_type)
      USE PRMS_SOILZONE, ONLY:Gvr_hru_id, Gvr_hru_pct_adjusted,
     +    Gravity_stor_res, Sm2gw_grav, Kkiter, Sm2gw_grav_old,
     +    Hru_gvr_count, Hru_gvr_index, Gw2sm_grav
      USE PRMS_BASIN, ONLY: NEARZERO
      USE PRMS_OBS, ONLY: Timestep_days
      IMPLICIT NONE
      EXTERNAL check_gvr_sm, compute_interflow
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Hru_type
      REAL, INTENT(IN) :: Capacity, Slowcoef_lin, Slowcoef_sq, Perv_pct
      REAL, INTENT(IN) :: Ssr2gw_rate, Ssr2gw_exp
      REAL, INTENT(IN) :: Pref_flow_thrsh, Ssres_in, Soil_to_gw
      REAL, INTENT(INOUT) :: Gvr2pfr, Gvr2sm, Ssr_to_gw, Slow_flow
      REAL, INTENT(OUT) :: Slow_stor, Gwin
! Local Variables
      INTEGER :: j, igvr
      REAL :: input, depth, pct, slowflow, perc, availh2o
!***********************************************************************
      !Capacity is for pervious area
      !Soil_to_gw is for whole HRU
      !TO DO
! use VKS as a function of slope (vector analysis) instead of coef_lin
! coef_lin for pref_flow needs to be VKS lateral times a factor
! change slow to interflow
! in init, set an array dimensioned by nhrucell to vks*mfl_to_inch

      Slow_stor = 0.0
      Gwin = 0.0
      DO j = 1, Hru_gvr_count(Ihru)
        igvr = Hru_gvr_index(j, Ihru)
! Gravity_stor_res is reset to It0 for each iteration
        depth = Gravity_stor_res(igvr)
        pct = Gvr_hru_pct_adjusted(igvr)
        Gwin = Gwin + Gw2sm_grav(igvr)*pct
        input = Ssres_in + Gw2sm_grav(igvr)

        availh2o = depth + input
        IF ( availh2o>0.0 ) THEN
          !sanity check, remove later
!         if (depth<0.0.or.input<0.0) print *,'gvr problem',depth,input
          CALL check_gvr_sm(Capacity, availh2o, Pref_flow_thrsh, depth,
     +         Gvr2pfr, input, pct, Gvr2sm, Perv_pct, Hru_type)

! compute contribution to interflow, if any
          IF ( availh2o>0.0 ) THEN
            IF ( Hru_type==1 ) THEN
              CALL compute_interflow(Slowcoef_lin, Slowcoef_sq, input,
     +             depth, slowflow, availh2o)
              Slow_flow = Slow_flow + slowflow*pct
            ELSE ! Hru_type = 3
              depth = availh2o
            ENDIF
          ENDIF

! compute flow to groundwater, if any
          IF ( depth>0.0 ) THEN
            IF ( Ssr2gw_rate>NEARZERO ) THEN
! use VKS instead of rate  ???????????????
              perc = Ssr2gw_rate*Timestep_days*(depth**Ssr2gw_exp)
              IF ( perc<0.0 ) perc = 0.0
! assume best guess is halfway between last iteration and this iteration
              IF ( Kkiter>2 ) perc = perc
     +                                  - (perc-Sm2gw_grav_old(igvr))*.5
              IF ( perc<0.0 ) THEN
                perc = 0.0
              ELSEIF ( perc>depth ) THEN
                perc = depth
!                depth = 0.0
!              ELSE
!                depth = depth - perc
              ENDIF
              depth = depth - perc
              Sm2gw_grav(igvr) = perc
              Ssr_to_gw = Ssr_to_gw + perc*pct
            ENDIF
          ENDIF

!         IF ( depth.LT.0.0 ) PRINT *, 'depth<0', depth
          Gravity_stor_res(igvr) = depth
          Slow_stor = Slow_stor + depth*pct
!       ELSEIF ( availh2o<0.0 ) THEN
!         !sanity check, remove later
!         PRINT *, 'availh2o problem', input, depth
!         input = 0.0
!         depth = 0.0
        ENDIF

! add any direct recharge from soil infiltration
        Sm2gw_grav(igvr) = Sm2gw_grav(igvr) + Soil_to_gw

      ENDDO ! end loop of GVRs in the HRU

      END SUBROUTINE compute_gravflow

!***********************************************************************
!     adjust soil moist based on being below field capacity (capacity)
!     and preferential-flow threshold (Pref_flow_thrsh)
!***********************************************************************
      SUBROUTINE check_gvr_sm(Capacity, Availh2o, Pref_flow_thrsh,
     +           Depth, Gvr2pfr, Input, Pct, Gvr2sm, Perv_pct, Hru_type)
      IMPLICIT NONE
      INTRINSIC MAX, ABS
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Capacity, Pref_flow_thrsh, Pct, Perv_pct
      REAL, INTENT(INOUT) :: Depth, Gvr2pfr, Input, Gvr2sm, Availh2o
! Local Variables
      REAL :: to_sm, pfr_h2o
!***********************************************************************
! check to see if soil is below capacity, if so add up to field capacity
! Capacity is for pervious area
! to_sm and Gvr2sm are for whole HRU
      IF ( Capacity>0.0 ) THEN
        IF ( Capacity*Perv_pct<Availh2o ) THEN
          !rsr, fill up capillary with part of gravity water
          to_sm = Capacity*Perv_pct
        ELSE
          !rsr, take all gravity water and put in capillary
          to_sm = Availh2o
        ENDIF
! compute adjusmtent to soil moist to get to field capacity
        Gvr2sm = Gvr2sm + to_sm*Pct
!        Gvr2sm = Gvr2sm + to_sm
      ELSE
        to_sm = 0.0
      ENDIF

! compute contribution to preferential-flow reservoir storage, if any
! adjust depth and input, take from input first
! Note, Gvr2pfr could have a value based on other GVR's
      IF ( Hru_type==1 ) THEN
        Availh2o = Availh2o - to_sm
        pfr_h2o = MAX(0.0, Availh2o-Pref_flow_thrsh)
        Gvr2pfr = Gvr2pfr + pfr_h2o*Pct
        Input = Input - pfr_h2o - to_sm
      ELSE
        Input = Input - to_sm
        pfr_h2o = 0.0
      ENDIF

      IF ( Input.LT.0.0 ) THEN
!rsr??? how should this be done
!        PRINT *, 'warning, input<0 in check_gvr_sm', Depth,
!     +           pfr_h2o, to_sm, Input, Capacity
        Depth = Depth + Input
!        IF ( Depth.LT.0.0 ) Depth = 0.0
       IF ( Depth.LT.0.0 ) THEN
         !sanity check, remove later
!         IF ( ABS(Depth)>1.0E-7 )
!     +         PRINT *, 'warning in check_gvr_sm', Input, Depth
         Depth = 0.0
       ENDIF
        Input = 0.0
      ENDIF

      Availh2o = Depth + Input

      END SUBROUTINE check_gvr_sm

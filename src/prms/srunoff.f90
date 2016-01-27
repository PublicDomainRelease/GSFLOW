!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a
! non-linear (smidx) and linear (carea) variable-source-area method
! Combinded smidx and carea modules 3/12/2013
!
!     version: 2.2 added cascading flow for infiltration and runoff
! rsr, 10/30/2008 added depression storage code
! rsr, 04/11/2011 changed so dprst_area to be a parameter (does not change)
! rsr, 07/1/2013 combined smidx and carea into one module
!***********************************************************************
      MODULE PRMS_SRUNOFF
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=13), SAVE :: MODNAME
      INTEGER, SAVE :: Ihru
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_thres_open(:), Dprst_in(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open_max(:), Dprst_vol_clos_max(:)
      REAL, SAVE, ALLOCATABLE :: Carea_dif(:), Imperv_stor_ante(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_stor_ante(:)
      REAL, SAVE :: Srp, Sri, Perv_frac, Imperv_frac, Hruarea_imperv
      DOUBLE PRECISION, SAVE :: Hruarea
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sroff_down, Basin_sroff_upslope
      DOUBLE PRECISION, SAVE :: Basin_sroffi, Basin_sroffp
      DOUBLE PRECISION, SAVE :: Basin_imperv_stor, Basin_imperv_evap, Basin_sroff, Basin_infil
      DOUBLE PRECISION, SAVE :: Basin_hortonian, Basin_hortonian_lakes, Basin_contrib_fraction
      REAL, SAVE, ALLOCATABLE :: Contrib_fraction(:)
      REAL, SAVE, ALLOCATABLE :: Imperv_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sroffp(:), Hru_sroffi(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Upslope_hortonian(:)
      REAL, SAVE, ALLOCATABLE :: Hortonian_flow(:)
      REAL, SAVE, ALLOCATABLE :: Hru_impervevap(:), Hru_impervstor(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Strm_seg_in(:), Hortonian_lakes(:), Hru_hortn_cascflow(:)
      DOUBLE PRECISION, SAVE :: Basin_sroff_farflow, Strm_farfield
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Smidx_coef(:), Smidx_exp(:)
      REAL, SAVE, ALLOCATABLE :: Carea_min(:), Carea_max(:)
!   Declared Parameters for Depression Storage
      REAL, SAVE, ALLOCATABLE :: Op_flow_thres(:), Sro_to_dprst(:)
      REAL, SAVE, ALLOCATABLE :: Va_clos_exp(:), Va_open_exp(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_flow_coef(:), Dprst_frac_init(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_open(:), Dprst_seep_rate_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_depth_avg(:), Sro_to_dprst_imperv(:), Dprst_et_coef(:)
!   Declared Variables for Depression Storage
      DOUBLE PRECISION, SAVE :: Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep
      DOUBLE PRECISION, SAVE :: Basin_dprst_volop, Basin_dprst_volcl
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_stor_hru(:), Dprst_sroff_hru(:), Dprst_seep_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open(:), Dprst_area_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_insroff_hru(:), Dprst_evap_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_vol_frac(:), Dprst_vol_open_frac(:), Dprst_vol_clos_frac(:)
      END MODULE PRMS_SRUNOFF

!***********************************************************************
!     Main srunoff routine
!***********************************************************************
      INTEGER FUNCTION srunoff()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srunoffdecl, srunoffinit, srunoffrun
      EXTERNAL :: srunoff_restart
!***********************************************************************
      srunoff = 0

      IF ( Process(:3)=='run' ) THEN
        srunoff = srunoffrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        srunoff = srunoffdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL srunoff_restart(1)
        srunoff = srunoffinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL srunoff_restart(0)
      ENDIF

      END FUNCTION srunoff

!***********************************************************************
!     srunoffdecl - set up parameters for surface runoff computations
!   Declared Parameters
!     smidx_coef, smidx_exp, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max, soil_rechr_max, carea_min
!***********************************************************************
      INTEGER FUNCTION srunoffdecl()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Model, Dprst_flag, Nhru, Nsegment, Print_debug, &
     &    Cascade_flag, Sroff_flag, Nlake, Init_vars_from_file, Call_cascade
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, declparam
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_srunoff
!***********************************************************************
      srunoffdecl = 0

      Version_srunoff = '$Id: srunoff.f90 7588 2015-08-18 22:58:42Z rsregan $'
      IF ( Sroff_flag==1 ) THEN
        Version_srunoff = Version_srunoff(:12)//'_smidx'//Version_srunoff(13:80)
        MODNAME = 'srunoff_smidx'
      ELSE
        Version_srunoff = Version_srunoff(:12)//'_carea'//Version_srunoff(13:80)
        MODNAME = 'srunoff_carea'
      ENDIF
      CALL print_module(Version_srunoff, 'Surface Runoff              ', 90)

      IF ( declvar(MODNAME, 'basin_imperv_evap', 'one', 1, 'double', &
     &     'Basin area-weighted average evaporation from impervious area', &
     &     'inches', Basin_imperv_evap)/=0 ) CALL read_error(3, 'basin_imperv_evap')

      IF ( declvar(MODNAME, 'basin_imperv_stor', 'one', 1, 'double', &
     &     'Basin area-weighted average storage on impervious area', &
     &     'inches', Basin_imperv_stor)/=0 ) CALL read_error(3, 'basin_imperv_stor')

      IF ( declvar(MODNAME, 'basin_infil', 'one', 1, 'double', &
     &     'Basin area-weighted average infiltration to the capillary reservoirs', &
     &     'inches', Basin_infil)/=0 ) CALL read_error(3, 'basin_infil')

      IF ( declvar(MODNAME, 'basin_sroff', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff to the stream network', &
     &     'inches', Basin_sroff)/=0 ) CALL read_error(3, 'basin_sroff')

      IF ( declvar(MODNAME, 'basin_hortonian', 'one', 1, 'double', &
     &     'Basin area-weighted average Hortonian runoff', &
     &     'inches', Basin_hortonian)/=0 ) CALL read_error(3, 'basin_hortonian')

      IF ( declvar(MODNAME, 'basin_contrib_fraction', 'one', 1, 'double', &
     &     'Basin area-weighted average contributing area of the pervious area of each HRU', &
     &     'decimal fraction', Basin_contrib_fraction)/=0 ) CALL read_error(3, 'basin_contrib_fraction')

      ALLOCATE ( Contrib_fraction(Nhru) )
      IF ( declvar(MODNAME, 'contrib_fraction', 'nhru', nhru, 'real', &
     &     'Contributing area of each HRU pervious area', &
     &     'inches', Contrib_fraction)/=0 ) CALL read_error(3, 'contrib_fraction')

      ALLOCATE ( Hru_impervevap(Nhru) )
      IF ( declvar(MODNAME, 'hru_impervevap', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average evaporation from impervious area for each HRU', &
     &     'inches', Hru_impervevap)/=0 ) CALL read_error(3, 'hru_impervevap')

      ALLOCATE ( Hru_impervstor(Nhru) )
      IF ( declvar(MODNAME, 'hru_impervstor', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average storage on impervious area for each HRU', &
     &     'inches', Hru_impervstor)/=0 ) CALL read_error(3, 'hru_impervstor')

      ALLOCATE ( Imperv_evap(Nhru) )
      IF ( declvar(MODNAME, 'imperv_evap', 'nhru', Nhru, 'real', &
     &     'Evaporation from impervious area for each HRU', &
     &     'inches', Imperv_evap)/=0 ) CALL read_error(3, 'imperv_evap')

      IF ( declvar(MODNAME, 'basin_sroffi', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff from impervious areas', &
     &     'inches', Basin_sroffi)/=0 ) CALL read_error(3, 'basin_sroffi')

      IF ( declvar(MODNAME, 'basin_sroffp', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff from pervious areas', &
     &     'inches', Basin_sroffp)/=0 ) CALL read_error(3, 'basin_sroffp')

      ALLOCATE ( Hru_sroffp(Nhru) )
      IF ( declvar(MODNAME, 'hru_sroffp', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average surface runoff from pervious areas for each HRU', &
     &     'inches', Hru_sroffp)/=0 ) CALL read_error(3, 'hru_sroffp')

      ALLOCATE ( Hru_sroffi(Nhru) )
      IF ( declvar(MODNAME, 'hru_sroffi', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average surface runoff from impervious areas for each HRU', &
     &     'inches', Hru_sroffi)/=0 ) CALL read_error(3, 'hru_sroffi')

! Depression storage variables
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'basin_dprst_sroff', 'one', 1, 'double', &
     &       'Basin area-weighted average surface runoff from open surface-depression storage', &
     &       'inches', Basin_dprst_sroff)/=0 ) CALL read_error(3, 'basin_dprst_sroff')

        IF ( declvar(MODNAME, 'basin_dprst_evap', 'one', 1, 'double', &
     &       'Basin area-weighted average evaporation from surface-depression storage', &
     &       'inches', Basin_dprst_evap)/=0 ) CALL read_error(3, 'basin_dprst_evap')

        IF ( declvar(MODNAME, 'basin_dprst_seep', 'one', 1, 'double', &
     &       'Basin area-weighted average seepage from surface-depression storage', &
     &       'inches', Basin_dprst_seep)/=0 ) CALL read_error(3, 'basin_dprst_seep')

        IF ( declvar(MODNAME, 'basin_dprst_volop', 'one', 1, 'double', &
     &       'Basin area-weighted average storage volume in open surface depressions', &
     &       'inches', Basin_dprst_volop)/=0 ) CALL read_error(3, 'basin_dprst_volop')

        IF ( declvar(MODNAME, 'basin_dprst_volcl', 'one', 1, 'double', &
     &       'Basin area-weighted average storage volume in closed surface depressions', &
     &       'inches', Basin_dprst_volcl)/=0 ) CALL read_error(3, 'basin_dprst_volcl')

        ALLOCATE ( Dprst_sroff_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_sroff_hru', 'nhru', Nhru, 'double', &
     &       'Surface runoff from open surface-depression storage for each HRU', &
     &       'inches', Dprst_sroff_hru)/=0 ) CALL read_error(3, 'dprst_sroff_hru')

        ALLOCATE ( Dprst_insroff_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_insroff_hru', 'nhru', Nhru, 'real', &
     &       'Surface runoff from pervious and impervious portions into open and closed surface-depression storage for each HRU', &
     &       'inches', Dprst_insroff_hru)/=0 ) CALL read_error(3, 'dprst_insroff_hru')

        ALLOCATE ( Dprst_area_open(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_open', 'nhru', Nhru, 'real', &
     &       'Surface area of open surface depressions based on storage volume for each HRU', &
     &       'acres', Dprst_area_open)/=0 ) CALL read_error(3, 'dprst_area_open')

        ALLOCATE ( Dprst_area_clos(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_clos', 'nhru', Nhru, 'real', &
     &       'Surface area of closed surface depressions based on storage volume for each HRU', &
     &       'acres', Dprst_area_clos)/=0 ) CALL read_error(3, 'dprst_area_clos')

        ALLOCATE ( Dprst_stor_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_stor_hru', 'nhru', Nhru, 'double', &
     &       'Surface-depression storage for each HRU', &
     &       'inches', Dprst_stor_hru)/=0 ) CALL read_error(3, 'dprst_stor_hru')

        ALLOCATE ( Dprst_seep_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_seep_hru', 'nhru', Nhru, 'double', &
     &       'Seepage from surface-depression storage to associated GWR for each HRU', &
     &       'inches', Dprst_seep_hru)/=0 ) CALL read_error(3, 'dprst_seep_hru')

        ALLOCATE ( Dprst_evap_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_evap_hru', 'nhru', Nhru, 'real', &
     &       'Evaporation from surface-depression storage for each HRU', &
     &       'inches', Dprst_evap_hru)/=0 ) CALL read_error(3, 'dprst_evap_hru')

        ALLOCATE ( Dprst_vol_open_frac(Nhru) )
        IF ( declvar(MODNAME, 'dprst_vol_open_frac', 'nhru', Nhru, 'real', &
     &      'Fraction of open surface-depression storage of the maximum storage for each HRU', &
     &      'decimal fraction', Dprst_vol_open_frac)/=0 ) CALL read_error(3, 'dprst_vol_open_frac')

        ALLOCATE ( Dprst_vol_clos_frac(Nhru) )
        IF ( declvar(MODNAME, 'dprst_vol_clos_frac', 'nhru', Nhru, 'real', &
     &      'Fraction of closed surface-depression storage of the maximum storage for each HRU', &
     &      'decimal fraction', Dprst_vol_clos_frac)/=0 ) CALL read_error(3, 'dprst_vol_clos_frac')

        ALLOCATE ( Dprst_vol_frac(Nhru) )
        IF ( declvar(MODNAME, 'dprst_vol_frac', 'nhru', Nhru, 'real', &
     &      'Fraction of surface-depression storage of the maximum storage for each HRU', &
     &      'decimal fraction', Dprst_vol_frac)/=0 ) CALL read_error(3, 'dprst_vol_frac')

        ALLOCATE ( Dprst_vol_open_max(Nhru), Dprst_vol_clos_max(Nhru), Dprst_vol_thres_open(Nhru), Dprst_in(Nhru) )
      ENDIF

      ALLOCATE ( Hortonian_flow(Nhru) )
      IF ( declvar(MODNAME, 'hortonian_flow', 'nhru', Nhru, 'real', &
     &     'Hortonian surface runoff reaching stream network for each HRU', &
     &     'inches', Hortonian_flow)/=0 ) CALL read_error(3, 'hortonian_flow')

! cascading variables and parameters
      IF ( Cascade_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Upslope_hortonian(Nhru) )
        IF ( declvar(MODNAME, 'upslope_hortonian', 'nhru', Nhru, 'double', &
     &       'Hortonian surface runoff received from upslope HRUs', &
     &       'inches', Upslope_hortonian)/=0 ) CALL read_error(3, 'upslope_hortonian')

        IF ( declvar(MODNAME, 'basin_sroff_down', 'one', 1, 'double', &
     &       'Basin area-weighted average of cascading surface runoff', &
     &       'inches', Basin_sroff_down)/=0 ) CALL read_error(3, 'basin_sroff_down')

        IF ( declvar(MODNAME, 'basin_sroff_upslope', 'one', 1, 'double', &
     &       'Basin area-weighted average of cascading surface runoff received from upslope HRUs', &
     &       'inches', Basin_sroff_upslope)/=0 ) CALL read_error(3, 'basin_sroff_upslope')

        ALLOCATE ( Hru_hortn_cascflow(Nhru) )
        IF ( declvar(MODNAME, 'hru_hortn_cascflow', 'nhru', Nhru, 'double', &
     &       'Cascading Hortonian surface runoff leaving each HRU', &
     &       'inches', Hru_hortn_cascflow)/=0 ) CALL read_error(3, 'hru_hortn_cascflow')

        IF ( Nlake>0 ) THEN
          IF ( declvar(MODNAME, 'basin_hortonian_lakes', 'one', 1, 'double', &
     &         'Basin area-weighted average Hortonian surface runoff to lakes', &
     &         'inches', Basin_hortonian_lakes)/=0 ) CALL read_error(3, 'basin_hortonian_lakes')

          ALLOCATE ( Hortonian_lakes(Nhru) )
          IF ( declvar(MODNAME, 'hortonian_lakes', 'nhru', Nhru, 'double', &
     &         'Surface runoff to lakes for each HRU', &
     &         'inches', Hortonian_lakes)/=0 ) CALL read_error(3, 'hortonian_lakes')
        ENDIF
      ENDIF

      IF ( Call_cascade==1 .OR. Model==99 ) THEN
        ALLOCATE ( Strm_seg_in(Nsegment) )
        IF ( declvar(MODNAME, 'strm_seg_in', 'nsegment', Nsegment, 'double', &
     &       'Flow in stream segments as a result of cascading flow', &
     &       'cfs', Strm_seg_in)/=0 ) CALL read_error(3,'strm_seg_in')
        IF ( declvar(MODNAME, 'strm_farfield', 'one', 1, 'double', &
     &       'Flow out of basin as far-field flow', &
     &       'cfs', Strm_farfield)/=0 ) CALL read_error(3, 'strm_farfield')
        IF ( declvar(MODNAME, 'basin_sroff_farflow', 'one', 1, 'double', &
     &       'Basin area-weighted average cascading surface runoff to farfield', &
     &       'inches', Basin_sroff_farflow)/=0 ) CALL read_error(3, 'basin_sroff_farflow')
      ENDIF

! Declare parameters
      IF ( Sroff_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Smidx_coef(Nhru) )
        IF ( declparam(MODNAME, 'smidx_coef', 'nhru', 'real', &
     &       '0.005', '0.001', '0.06', &
     &       'Coefficient in contributing area computations', &
     &       'Coefficient in non-linear contributing area algorithm for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'smidx_coef')
        ALLOCATE ( Smidx_exp(Nhru) )
        IF ( declparam(MODNAME, 'smidx_exp', 'nhru', 'real', &
     &       '0.3', '0.1', '0.5', &
     &       'Exponent in contributing area computations', &
     &       'Exponent in non-linear contributing area algorithm for each HRU', &
     &       '1.0/inch')/=0 ) CALL read_error(1, 'smidx_exp')
      ENDIF

      IF ( Sroff_flag==2 .OR. Model==99 ) THEN
        ALLOCATE ( Carea_min(Nhru), Carea_dif(Nhru) )
        IF ( declparam(MODNAME, 'carea_min', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Minimum contributing area', &
     &       'Minimum possible area contributing to surface runoff expressed as a portion of the area for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'carea_min')
      ENDIF

      ALLOCATE ( Carea_max(Nhru) )
      IF ( declparam(MODNAME, 'carea_max', 'nhru', 'real', &
     &     '0.6', '0.0', '1.0', &
     &     'Maximum contributing area', &
     &     'Maximum possible area contributing to surface runoff expressed as a portion of the HRU area', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'carea_max')

! Depression Storage parameters:
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Dprst_depth_avg(Nhru) )
        IF ( declparam(MODNAME, 'dprst_depth_avg', 'nhru', 'real', &
     &       '132.0', '0.0', '500.0', &
     &       'Average depth of surface depressions at maximum storage capacity', &
     &       'Average depth of surface depressions at maximum storage capacity', &
     &       'inches')/=0 ) CALL read_error(1, 'dprst_depth_avg')

        ALLOCATE ( Dprst_flow_coef(Nhru) )
        IF ( declparam(MODNAME, 'dprst_flow_coef', 'nhru', 'real', &
     &       '0.05', '0.0001', '0.3', &
     &       'Coefficient in linear flow routing equation for open surface depressions', &
     &       'Coefficient in linear flow routing equation for open surface depressions for each HRU', &
     &       'fraction/day')/=0 ) CALL read_error(1, 'dprst_flow_coef')

        ALLOCATE ( Dprst_seep_rate_open(Nhru) )
        IF ( declparam(MODNAME, 'dprst_seep_rate_open', 'nhru', 'real', &
     &       '0.02', '0.0001', '0.1', &
     &       'Coefficient used in linear seepage flow equation for open surface depressions', &
     &       'Coefficient used in linear seepage flow equation for'// &
     &       ' open surface depressions for each HRU', &
     &       'fraction/day')/=0 ) CALL read_error(1, 'dprst_seep_rate_open')

        ALLOCATE ( Dprst_seep_rate_clos(Nhru) )
        IF ( declparam(MODNAME, 'dprst_seep_rate_clos', 'nhru', 'real', &
     &       '0.02', '0.0001', '0.1', &
     &       'Coefficient used in linear seepage flow equation for closed surface depressions', &
     &       'Coefficient used in linear seepage flow equation for'// &
     &       ' closed surface depressions for each HRU', &
     &       'fraction/day')/=0 ) CALL read_error(1, 'dprst_seep_rate_clos')

        ALLOCATE ( Op_flow_thres(Nhru) )
        IF ( declparam(MODNAME, 'op_flow_thres', 'nhru', 'real', &
     &       '1.0', '0.75', '1.0', &
     &       'Fraction of open depression storage above which surface runoff occurs for each timestep', &
     &       'Fraction of open depression storage above'// &
     &       ' which surface runoff occurs; any water above'// &
     &       ' maximum open storage capacity spills as surface runoff', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'op_flow_thres')

        ALLOCATE ( Sro_to_dprst(Nhru) )
        IF ( declparam(MODNAME, 'sro_to_dprst', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Fraction of pervious surface runoff that flows into surface-depression storage', &
     &       'Fraction of pervious surface runoff that'// &
     &       ' flows into surface-depression storage; the remainder'// &
     &       ' flows to a stream network for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst')

        ALLOCATE ( Sro_to_dprst_imperv(Nhru) )
        IF ( declparam(MODNAME, 'sro_to_dprst_imperv', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Fraction of impervious surface runoff that flows into surface-depression storage', &
     &       'Fraction of impervious surface runoff that'// &
     &       ' flows into surface-depression storage; the remainder'// &
     &       ' flows to a stream network for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst_imperv')

        ALLOCATE ( Dprst_et_coef(Nhru) )
        IF ( declparam(MODNAME, 'dprst_et_coef', 'nhru', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_et_coef')

        IF ( Init_vars_from_file==0 ) THEN
          ALLOCATE ( Dprst_frac_init(Nhru) )
          IF ( declparam(MODNAME, 'dprst_frac_init', 'nhru', 'real', &
     &         '0.5', '0.0', '1.0', &
     &         'Fraction of maximum storage that contains water at the start of a simulation', &
     &         'Fraction of maximum surface-depression storage that'// &
     &         ' contains water at the start of a simulation', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_init')
        ENDIF

        ALLOCATE ( Va_open_exp(Nhru) )
        IF ( declparam(MODNAME, 'va_open_exp', 'nhru', 'real', &
     &       '1.0', '0.001', '10.0', &
     &       'Coefficient in the exponential equation to compute'// &
     &       ' current surface area of open surface-depression storage', &
     &       'Coefficient in the exponential equation relating'// &
     &       ' maximum surface area to the fraction that open'// &
     &       ' depressions are full to compute current surface area for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'va_open_exp')

        ALLOCATE ( Va_clos_exp(Nhru) )
        IF ( declparam(MODNAME, 'va_clos_exp', 'nhru', 'real', &
     &       '1.0', '0.001', '10.0', &
     &       'Coefficient in the exponential equation to compute'// &
     &       ' current surface area of closed surface-depression storage', &
     &       'Coefficient in the exponential equation relating'// &
     &       ' maximum surface area to the fraction that closed'// &
     &       ' depressions are full to compute current surface area for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'va_clos_exp')
      ENDIF

      IF ( Print_debug==1 ) THEN
        ALLOCATE ( Imperv_stor_ante(Nhru) )
        IF ( Dprst_flag==1 ) ALLOCATE ( Dprst_stor_ante(Nhru) )
      ENDIF

      END FUNCTION srunoffdecl

!***********************************************************************
!     srunoffinit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srunoffinit()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Dprst_flag, Nhru, Nlake, Cascade_flag, Sroff_flag, &
     &    Inputerror_flag, Parameter_check_flag, Print_debug, Init_vars_from_file, Call_cascade
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error, check_param_value
! Local Variables
      INTEGER :: i, j, k, num_hrus
      REAL :: frac
!***********************************************************************
      srunoffinit = 0

      IF ( Init_vars_from_file==0 ) THEN
        Imperv_evap = 0.0
        Hortonian_flow = 0.0
        Hru_sroffi = 0.0
        Hru_sroffp = 0.0
        Basin_sroffi = 0.0D0
        Basin_sroffp = 0.0D0
        Basin_infil = 0.0D0
        Basin_sroff = 0.0D0
        Basin_imperv_evap = 0.0D0
        Basin_imperv_stor = 0.0D0
        Basin_hortonian = 0.0D0
        Basin_dprst_sroff = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
        Basin_sroff_upslope = 0.0D0
        Basin_sroff_down = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Basin_contrib_fraction = 0.0D0
        Basin_sroff_farflow = 0.0D0
        Strm_farfield = 0.0D0
        Contrib_fraction = 0.0
        Hru_impervevap = 0.0
        Hru_impervstor = 0.0
        Srp = 0.0
        Sri = 0.0

        IF ( Call_cascade==1 ) Strm_seg_in = 0.0D0
        IF ( Cascade_flag==1 ) THEN
          Upslope_hortonian = 0.0D0
          Hru_hortn_cascflow = 0.0D0
          IF ( Nlake>0 ) Hortonian_lakes = 0.0D0
        ENDIF
      ENDIF

      IF ( getparam(MODNAME, 'carea_max', Nhru, 'real', Carea_max)/=0 ) CALL read_error(2, 'carea_max')

      IF ( Sroff_flag==1 ) THEN
! Smidx parameters
        IF ( getparam(MODNAME, 'smidx_coef', Nhru, 'real', Smidx_coef)/=0 ) CALL read_error(2, 'smidx_coef')
        IF ( getparam(MODNAME, 'smidx_exp', Nhru, 'real', Smidx_exp)/=0 ) CALL read_error(2, 'smidx_exp')
      ELSE !IF ( Sroff_flag==2 ) THEN
! Carea parameters
        IF ( getparam(MODNAME, 'carea_min', Nhru, 'real', Carea_min)/=0 ) CALL read_error(2, 'carea_min')
        Carea_dif = 0.0
      ENDIF

      num_hrus = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        CALL check_param_value(i, 'carea_max', Carea_max(i), Inputerror_flag)
        IF ( Sroff_flag==2 ) THEN
          CALL check_param_value(i, 'carea_min', Carea_min(i), Inputerror_flag)
          Carea_dif(i) = Carea_max(i) - Carea_min(i)
        ELSEIF ( Parameter_check_flag>0 ) THEN
          CALL check_param_value(i, 'smidx_coef', Smidx_coef(i), Inputerror_flag)
          CALL check_param_value(i, 'smidx_exp', Smidx_exp(i), Inputerror_flag)
          frac = Smidx_coef(i)*10**(Soil_moist_max(i)*Smidx_exp(i))
          k = 0
          IF ( frac>2.0 ) k = 1
          IF ( frac>Carea_max(i)*2.0 ) k = k + 2
          IF ( k>0 ) THEN
            num_hrus = num_hrus + 1
            !IF ( Print_debug>-1 ) THEN
            !  PRINT *, ' '
            !  PRINT *, 'WARNING'
            !  PRINT *, 'Contributing area based on smidx parameters and soil_moist_max:', frac
            !  IF ( k==1 .OR. k==3 ) PRINT *, 'Maximum contributing area > 200%'
            !  IF ( k>1 ) PRINT *, 'Maximum contributing area > carea_max:', Carea_max(i)
            !  PRINT *, 'HRU:', i, '; soil_moist_max:', Soil_moist_max(i)
            !  PRINT *, 'smidx_coef:', Smidx_coef(i), '; smidx_exp:', Smidx_exp(i)
            !  PRINT *, 'This can make smidx parameters insensitive and carea_max very sensitive'
            !ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF ( num_hrus>0 .AND. Print_debug>-1 ) THEN
        WRITE (*, '(/,A,/,9X,A,/,9X,A,I7,/,9X,A,/,9X,A,/)') &
     &         'WARNING, maximum contributing area based on smidx coefficents and', &
     &         'soil_moist_max are > 200% of the HRU area and/or > 2*carea_max', &
     &         'number of HRUs for which this condition exists:', num_hrus, &
     &         'This means the smidx parameters are insensitive and', &
     &         'carea_max very sensitive for those HRUs'
      ENDIF

! Depression Storage parameters and variables:
      IF ( Dprst_flag==1 ) CALL dprst_init()

      IF ( Print_debug==1 ) THEN
        Imperv_stor_ante = Hru_impervstor
        IF ( Dprst_flag==1 ) Dprst_stor_ante = Dprst_stor_hru
      ENDIF

      END FUNCTION srunoffinit

!***********************************************************************
!     srunoffrun - Computes surface runoff using contributing area
!                  computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srunoffrun()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Dprst_flag, Cascade_flag, Call_cascade, Print_debug
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, &
     &    Hru_perv, Hru_imperv, Hru_frac_imperv, Hru_frac_perv, &
     &    Dprst_area_max, Hru_area_dble, Hru_type, Basin_area_inv, &
     &    Dprst_area_clos_max, Dprst_area_open_max, DNEARZERO, CLOSEZERO
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Sroff, Infil, Imperv_stor, Pkwater_equiv, Dprst_vol_open, Dprst_vol_clos, &
     &    Imperv_stor_max, Snowinfil_max
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_INTCP, ONLY: Net_rain, Net_snow, Net_ppt, Hru_intcpevap
      USE PRMS_SNOW, ONLY: Snow_evap, Snowcov_area, Snowmelt
      IMPLICIT NONE
      INTRINSIC SNGL, DBLE
      EXTERNAL imperv_et, compute_infil, run_cascade_sroff, dprst_comp, perv_comp
! Local Variables
      INTEGER :: i, k, dprst_chk
      REAL :: srunoff, avail_et, hperv
      DOUBLE PRECISION :: hru_sroff_down, runoff, farflow
!***********************************************************************
      srunoffrun = 0

      IF ( Print_debug==1 ) THEN
        Imperv_stor_ante = Hru_impervstor
        IF ( Dprst_flag==1 ) Dprst_stor_ante = Dprst_stor_hru
      ENDIF
      Basin_sroffi = 0.0D0
      Basin_sroffp = 0.0D0
      Basin_sroff = 0.0D0
      Basin_infil = 0.0D0
      Basin_imperv_evap = 0.0D0
      Basin_imperv_stor = 0.0D0
      Basin_hortonian = 0.0D0
      Basin_contrib_fraction = 0.0D0

      IF ( Call_cascade==1 ) Strm_seg_in = 0.0D0
      IF ( Cascade_flag==1 ) THEN
        Basin_sroff_down = 0.0D0
        Basin_sroff_upslope = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Upslope_hortonian = 0.0D0
        Strm_farfield = 0.0D0
        Basin_sroff_farflow = 0.0D0
      ENDIF

      IF ( Dprst_flag==1 ) THEN
        Basin_dprst_sroff = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
      ENDIF

      dprst_chk = 0
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        Hruarea = Hru_area_dble(i)
        Ihru = i
        runoff = 0.0D0

        IF ( Hru_type(i)==2 ) THEN
! HRU is a lake
!     eventually add code for lake area less than hru_area
!     that includes soil_moist for fraction of hru_area that is dry bank
          ! Sanity check
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i)>0.0 ) &
     &         PRINT *, 'srunoff lake ERROR', Infil(i), Sroff(i), Imperv_stor(i), Imperv_evap(i), i
          IF ( Cascade_flag==1 ) THEN
            Hortonian_lakes(i) = Upslope_hortonian(i)
            Basin_hortonian_lakes = Basin_hortonian_lakes + Hortonian_lakes(i)*Hruarea
          ENDIF
          CYCLE
        ENDIF

        Infil(i) = 0.0
        hperv = Hru_perv(i)
        Perv_frac = Hru_frac_perv(i)
        Srp = 0.0
        Sri = 0.0
        Hru_sroffp(i) = 0.0
        Contrib_fraction(i) = 0.0
        Hruarea_imperv = Hru_imperv(i)
        IF ( Hruarea_imperv>0.0 ) THEN
          Imperv_frac = Hru_frac_imperv(i)
          Hru_sroffi(i) = 0.0
          Imperv_evap(i) = 0.0
          Hru_impervevap(i) = 0.0
        ENDIF

        avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)

!******Compute runoff for pervious, impervious, and depression storage area
        CALL compute_infil(Net_rain(i), Net_ppt(i), Imperv_stor(i), Imperv_stor_max(i), Snowmelt(i), &
     &                     Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i), Infil(i), Hru_type(i))

        IF ( Dprst_flag==1 ) THEN
          Dprst_in(i) = 0.0D0
          dprst_chk = 0
          IF ( Dprst_area_max(i)>0.0 ) THEN
            dprst_chk = 1
!           ******Compute the depression storage component
!           only call if total depression surface area for each HRU is > 0.0
            CALL dprst_comp(Dprst_vol_clos(i), Dprst_area_clos_max(i), Dprst_area_clos(i), &
     &                      Dprst_vol_open_max(i), Dprst_vol_open(i), Dprst_area_open_max(i), Dprst_area_open(i), &
     &                      Dprst_sroff_hru(i), Dprst_seep_hru(i), Sro_to_dprst(i), Sro_to_dprst_imperv(i), &
     &                      Dprst_evap_hru(i), avail_et, Net_rain(i), Dprst_in(i))
            runoff = runoff + Dprst_sroff_hru(i)*Hruarea
          ENDIF
        ENDIF
!         **********************************************************

        srunoff = 0.0
        IF ( Hru_type(i)==1 ) THEN
!******Compute runoff for pervious and impervious area, and depression storage area
          runoff = runoff + Srp*hperv + Sri*Hruarea_imperv
          srunoff = SNGL( runoff/Hruarea )

!******Compute HRU weighted average (to units of inches/dt)
          IF ( Cascade_flag==1 ) THEN
            farflow = 0.0D0
            hru_sroff_down = 0.0D0
            IF ( srunoff>0.0 ) THEN
              IF ( Ncascade_hru(i)>0 ) CALL run_cascade_sroff(Ncascade_hru(i), srunoff, hru_sroff_down, farflow)
              Hru_hortn_cascflow(i) = hru_sroff_down + farflow
              !IF ( Hru_hortn_cascflow(i)<0.0D0 ) Hru_hortn_cascflow(i) = 0.0D0
              !IF ( Upslope_hortonian(i)<0.0D0 ) Upslope_hortonian(i) = 0.0D0
              Basin_sroff_upslope = Basin_sroff_upslope + Upslope_hortonian(i)*Hruarea
              Basin_sroff_down = Basin_sroff_down + hru_sroff_down*Hruarea
              Basin_sroff_farflow = Basin_sroff_farflow + farflow*Hruarea
            ELSE
              Hru_hortn_cascflow(i) = 0.0D0
            ENDIF
          ENDIF
          Hru_sroffp(i) = Srp*Perv_frac
          Basin_sroffp = Basin_sroffp + Srp*hperv
        ENDIF

        Basin_infil = Basin_infil + DBLE( Infil(i)*hperv )
        Basin_contrib_fraction = Basin_contrib_fraction + DBLE( Contrib_fraction(i)*hperv )

!******Compute evaporation from impervious area
        IF ( Hruarea_imperv>0.0 ) THEN
          IF ( Imperv_stor(i)>0.0 ) THEN
            CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i), Snowcov_area(i), avail_et)
            Hru_impervevap(i) = Imperv_evap(i)*Imperv_frac
            !IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
            avail_et = avail_et - Hru_impervevap(i)
            IF ( avail_et<0.0 ) THEN
               ! sanity check
!              IF ( avail_et<-NEARZERO ) PRINT*, 'avail_et<0 in srunoff imperv', i, Nowmonth, Nowday, avail_et
              Hru_impervevap(i) = Hru_impervevap(i) + avail_et
              IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
              Imperv_evap(i) = Hru_impervevap(i)/Imperv_frac
              Imperv_stor(i) = Imperv_stor(i) - avail_et/Imperv_frac
            ENDIF
            Basin_imperv_evap = Basin_imperv_evap + DBLE(Hru_impervevap(i))*Hruarea
            Hru_impervstor(i) = Imperv_stor(i)*Imperv_frac
            Basin_imperv_stor = Basin_imperv_stor + DBLE(Imperv_stor(i)*Hruarea_imperv )
          ENDIF
          Hru_sroffi(i) = Sri*Imperv_frac
          Basin_sroffi = Basin_sroffi + Sri*Hruarea_imperv
        ENDIF

        IF ( dprst_chk==1 ) Dprst_stor_hru(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/Hruarea

        Sroff(i) = srunoff
        Hortonian_flow(i) = srunoff
        Basin_hortonian = Basin_hortonian + srunoff*Hruarea
        Basin_sroff = Basin_sroff + srunoff*Hruarea
      ENDDO

!******Compute basin weighted averages (to units of inches/dt)
      !rsr, should be land_area???
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_imperv_evap = Basin_imperv_evap*Basin_area_inv
      Basin_imperv_stor = Basin_imperv_stor*Basin_area_inv
      Basin_infil = Basin_infil*Basin_area_inv
      Basin_sroffp = Basin_sroffp*Basin_area_inv
      Basin_sroffi = Basin_sroffi*Basin_area_inv
      Basin_hortonian = Basin_hortonian*Basin_area_inv
      Basin_contrib_fraction = Basin_contrib_fraction*Basin_area_inv
      IF ( Cascade_flag==1 ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_farflow = Basin_sroff_farflow*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
      ENDIF

      IF ( Dprst_flag==1 ) THEN
        Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
        Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
        Basin_dprst_evap = Basin_dprst_evap*Basin_area_inv
        Basin_dprst_seep = Basin_dprst_seep*Basin_area_inv
        Basin_dprst_sroff = Basin_dprst_sroff*Basin_area_inv
      ENDIF

      END FUNCTION srunoffrun

!***********************************************************************
!      Subroutine to compute evaporation from impervious area at
!      potential ET rate up to available ET
!***********************************************************************
      SUBROUTINE imperv_et(Imperv_stor, Potet, Imperv_evap, Sca, Avail_et)
      USE PRMS_SRUNOFF, ONLY: Imperv_frac
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Potet, Sca, Avail_et
      REAL, INTENT(INOUT) :: Imperv_stor, Imperv_evap
!***********************************************************************
      IF ( Sca<1.0 ) THEN
        IF ( Potet<Imperv_stor ) THEN
          Imperv_evap = Potet*(1.0-Sca)
        ELSE
          Imperv_evap = Imperv_stor*(1.0-Sca)
        ENDIF
        IF ( Imperv_evap*Imperv_frac>Avail_et ) Imperv_evap = Avail_et/Imperv_frac
        Imperv_stor = Imperv_stor - Imperv_evap
      ENDIF
      !rsr, sanity check
!      IF ( Imperv_stor<0.0 ) THEN
!        PRINT *, 'imperv_stor<0', Imperv_stor
!        Imperv_stor = 0.0
!      ENDIF

      END SUBROUTINE imperv_et

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil(Net_rain, Net_ppt, Imperv_stor, Imperv_stor_max, Snowmelt, &
     &                         Snowinfil_max, Net_snow, Pkwater_equiv, Infil, Hru_type)
      USE PRMS_SRUNOFF, ONLY: Sri, Hruarea_imperv, Upslope_hortonian, Ihru
      USE PRMS_SNOW, ONLY: Pptmix_nopack
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      USE PRMS_MODULE, ONLY: Cascade_flag
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Net_rain, Net_ppt, Imperv_stor_max
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow
      DOUBLE PRECISION, INTENT(IN) :: Pkwater_equiv
      REAL, INTENT(INOUT) :: Imperv_stor, Infil
! Functions
      INTRINSIC SNGL
      EXTERNAL perv_comp, check_capacity
! Local Variables
      REAL :: avail_water
!***********************************************************************
! compute runoff from cascading Hortonian flow
      IF ( Cascade_flag==1 ) THEN
        avail_water = SNGL( Upslope_hortonian(Ihru) )
        IF ( avail_water>0.0 ) THEN
          Infil = avail_water
          IF ( Hru_type==1 ) CALL perv_comp(avail_water, avail_water, Infil)
        ENDIF
      ELSE
        avail_water = 0.0
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack(Ihru)==1 ) THEN
        avail_water = avail_water + Net_rain
        Infil = Infil + Net_rain
        IF ( Hru_type==1 ) CALL perv_comp(Net_rain, Net_rain, Infil)
      ENDIF

!******If precipitation on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN
        avail_water = avail_water + Snowmelt
        Infil = Infil + Snowmelt
        IF ( Hru_type==1 ) THEN
          IF ( Pkwater_equiv>0.0D0 .OR. Net_ppt-Net_snow<NEARZERO ) THEN
!******Pervious area computations
            CALL check_capacity(Snowinfil_max, Infil)
!******Snowmelt occurred and depleted the snowpack
          ELSE
            CALL perv_comp(Snowmelt, Net_ppt, Infil)
          ENDIF
        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( Pkwater_equiv<DNEARZERO ) THEN

!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.

        IF ( Net_snow<NEARZERO .AND. Net_rain>0.0 ) THEN
! no snow, some rain
          avail_water = avail_water + Net_rain
          Infil = Infil + Net_rain
          IF ( Hru_type==1 ) CALL perv_comp(Net_rain, Net_rain, Infil)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.

      ELSEIF ( Infil>0.0 ) THEN
        IF ( Hru_type==1 ) CALL check_capacity(Snowinfil_max, Infil)
      ENDIF

!******Impervious area computations
      IF ( Hruarea_imperv>0.0 ) THEN
        Imperv_stor = Imperv_stor + avail_water
        IF ( Hru_type==1 ) THEN
          IF ( Imperv_stor>Imperv_stor_max ) THEN
            Sri = Imperv_stor - Imperv_stor_max
            Imperv_stor = Imperv_stor_max
          ENDIF
        ENDIF
      ENDIF

      END SUBROUTINE compute_infil

!***********************************************************************
      SUBROUTINE perv_comp(Pptp, Ptc, Infil)
      USE PRMS_SRUNOFF, ONLY: Srp, Ihru, Smidx_coef, Smidx_exp, &
     &    Carea_max, Carea_min, Carea_dif, Contrib_fraction
      USE PRMS_MODULE, ONLY: Sroff_flag
      USE PRMS_BASIN, ONLY: CLOSEZERO
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Soil_rechr, Soil_rechr_max
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Pptp, Ptc
      REAL, INTENT(INOUT) :: Infil
! Local Variables
      REAL :: smidx, srpp, ca_fraction
!***********************************************************************
!******Pervious area computations
      IF ( Sroff_flag==1 ) THEN
        ! antecedent soil_moist
        smidx = Soil_moist(Ihru) + (0.5*Ptc)
        ca_fraction = Smidx_coef(Ihru)*10.0**(Smidx_exp(Ihru)*smidx)
      ELSE
        ! antecedent soil_rechr
        ca_fraction = Carea_min(Ihru) + Carea_dif(Ihru)*(Soil_rechr(Ihru)/Soil_rechr_max(Ihru))
      ENDIF
      IF ( ca_fraction>Carea_max(Ihru) ) ca_fraction = Carea_max(Ihru)
      srpp = ca_fraction*Pptp
      Contrib_fraction(Ihru) = ca_fraction
!      IF ( srpp<0.0 ) THEN
!        PRINT *, 'negative srp', srpp
!        srpp = 0.0
!      ENDIF
      Infil = Infil - srpp
      Srp = Srp + srpp
      !IF ( Srp<CLOSEZERO ) Srp = 0.0

      END SUBROUTINE perv_comp

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_sroff(Ncascade_hru, Runoff, Hru_sroff_down, Farflow)
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Ihru, Upslope_hortonian, Strm_seg_in, Strm_farfield
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt, Cascade_area, Nsegmentp1
      IMPLICIT NONE
! Functions
      INTRINSIC IABS, ABS, DBLE, SNGL
! Arguments
      INTEGER, INTENT(IN) :: Ncascade_hru
      REAL, INTENT(INOUT) :: Runoff
      DOUBLE PRECISION, INTENT(INOUT) :: Hru_sroff_down, Farflow
! Local Variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j>0 ) THEN
          Upslope_hortonian(j) = Upslope_hortonian(j) + DBLE( Runoff*Hru_down_fracwt(k, Ihru) )
          Hru_sroff_down = Hru_sroff_down + DBLE( Runoff*Hru_down_frac(k,Ihru) )

! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          IF ( j/=Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) + DBLE(Runoff*Cascade_area(k, Ihru))*Cfs_conv
          ELSE
            Strm_farfield = Strm_farfield + DBLE(Runoff*Cascade_area(k, Ihru))*Cfs_conv
            Farflow = Farflow + DBLE( Runoff*Hru_down_frac(k, Ihru) )
          ENDIF
        ENDIF
      ENDDO

! reset Sroff as it accumulates flow to streams
      Runoff = Runoff - Hru_sroff_down - SNGL( Farflow )
!      IF ( ABS(Runoff)<CLOSEZERO ) Runoff = 0.0
      IF ( Runoff<0.0 ) THEN
        IF ( Runoff<-NEARZERO ) PRINT *, 'runoff < NEARZERO', Runoff
        IF ( Hru_sroff_down>ABS(Runoff) ) THEN
          Hru_sroff_down = Hru_sroff_down - Runoff
        ELSE
          DO k = 1, Ncascade_hru
            j = Hru_down(k, Ihru)
            IF ( Strm_seg_in(j)>ABS(Runoff) ) THEN
              Strm_seg_in(j) = Strm_seg_in(j) - Runoff
              EXIT
            ENDIF
          ENDDO
        ENDIF
        Runoff = 0.0
      ENDIF

      END SUBROUTINE run_cascade_sroff

!***********************************************************************
! fill soil to soil_moist_max, if more than capacity restrict
! infiltration by snowinfil_max, with excess added to runoff
!***********************************************************************
      SUBROUTINE check_capacity(Snowinfil_max, Infil)
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_moist
      USE PRMS_SRUNOFF, ONLY: Ihru, Srp
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Snowinfil_max
      REAL, INTENT(INOUT) :: Infil
! Local Variables
      REAL :: capacity, excess
!***********************************************************************
      capacity = Soil_moist_max(Ihru) - Soil_moist(Ihru)
      excess = Infil - capacity
      IF ( excess>Snowinfil_max ) THEN
        Srp = Srp + excess - Snowinfil_max
        Infil = Snowinfil_max + capacity
      ENDIF

      END SUBROUTINE check_capacity

!***********************************************************************
! Initialize depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_init()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Inputerror_flag, Init_vars_from_file, Nhru
      USE PRMS_BASIN, ONLY: Dprst_clos_flag, NEARZERO, Dprst_area_max, &
     &    Dprst_area_clos_max, Dprst_area_open_max, Basin_area_inv, &
     &    Hru_area_dble, Active_hrus, Hru_route_order, DNEARZERO
      USE PRMS_FLOWVARS, ONLY: Dprst_vol_open, Dprst_vol_clos
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, LOG, DBLE, SNGL
      INTEGER, EXTERNAL :: getparam
      EXTERNAL check_param_value
! Local Variables
      INTEGER :: i, j, ierr
      REAL :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r
!***********************************************************************
      IF ( Init_vars_from_file==0 ) THEN
        Dprst_evap_hru = 0.0
        Dprst_seep_hru = 0.0D0
        Dprst_sroff_hru = 0.0D0
        Dprst_insroff_hru = 0.0
        IF ( getparam(MODNAME, 'dprst_frac_init', Nhru, 'real', Dprst_frac_init)/=0 ) CALL read_error(2, 'dprst_frac_init')
      ENDIF
      IF ( getparam(MODNAME, 'dprst_flow_coef', Nhru, 'real', Dprst_flow_coef)/=0 ) CALL read_error(2, 'dprst_flow_coef')
      IF ( getparam(MODNAME, 'dprst_seep_rate_open', Nhru, 'real', Dprst_seep_rate_open)/=0 )  &
     &     CALL read_error(2, 'dprst_seep_rate_open')
      IF ( getparam(MODNAME, 'va_open_exp', Nhru, 'real', Va_open_exp)/=0 ) CALL read_error(2, 'va_open_exp')
      IF ( getparam(MODNAME, 'sro_to_dprst', Nhru, 'real', Sro_to_dprst)/=0 ) CALL read_error(2, 'sro_to_dprst')
      IF ( getparam(MODNAME, 'sro_to_dprst_imperv', Nhru, 'real', Sro_to_dprst_imperv)/=0 ) &
     &     CALL read_error(2, 'sro_to_dprst_imperv')
      IF ( getparam(MODNAME, 'dprst_depth_avg', Nhru, 'real', Dprst_depth_avg)/=0 ) CALL read_error(2, 'dprst_depth_avg')
      IF ( getparam(MODNAME, 'op_flow_thres', Nhru, 'real', Op_flow_thres)/=0 ) CALL read_error(2, 'op_flow_thres')
      IF ( getparam(MODNAME, 'dprst_et_coef', Nhru, 'real', Dprst_et_coef)/=0 ) CALL read_error(2, 'dprst_et_coef')
      IF ( Dprst_clos_flag==1 ) THEN
        IF ( getparam(MODNAME, 'dprst_seep_rate_clos', Nhru, 'real', Dprst_seep_rate_clos)/=0 ) &
     &       CALL read_error(2, 'dprst_seep_rate_clos')
        IF ( getparam(MODNAME, 'va_clos_exp', Nhru, 'real', Va_clos_exp)/=0 ) CALL read_error(2, 'va_clos_exp')
      ELSE
        Dprst_seep_rate_clos = 0.0
        Va_clos_exp = 0.0
      ENDIF
      Dprst_in = 0.0D0
      Dprst_area_open = 0.0
      Dprst_area_clos = 0.0
      Dprst_stor_hru = 0.0D0
      Dprst_vol_thres_open = 0.0D0
      Dprst_vol_open_max = 0.0D0
      Dprst_vol_clos_max = 0.0D0
      Dprst_vol_frac = 0.0
      Dprst_vol_open_frac = 0.0
      Dprst_vol_clos_frac = 0.0
      Basin_dprst_volop = 0.0D0
      Basin_dprst_volcl = 0.0D0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        ierr = 0
        CALL check_param_value(i, 'dprst_et_coef', Dprst_et_coef(i), ierr)
        CALL check_param_value(i, 'op_flow_thres', Op_flow_thres(i), ierr)
        CALL check_param_value(i, 'sro_to_dprst', Sro_to_dprst(i), ierr)
        CALL check_param_value(i, 'sro_to_dprst_imperv', Sro_to_dprst_imperv(i), ierr)
        CALL check_param_value(i, 'dprst_flow_coef', Dprst_flow_coef(i), ierr)
        CALL check_param_value(i, 'dprst_seep_rate_open', Dprst_seep_rate_open(i), ierr)
        IF ( Dprst_clos_flag==1 ) CALL check_param_value(i, 'dprst_seep_rate_clos', Dprst_seep_rate_clos(i), ierr)
        IF ( ierr==1 ) THEN
          Inputerror_flag = 1
          CYCLE
        ENDIF

        IF ( Dprst_area_max(i)>NEARZERO ) THEN
!         calculate open and closed volumes (acre-inches) of depression storage by HRU
!         Dprst_area_open_max is the maximum open depression area (acres) that can generate surface runoff:
          IF ( Dprst_clos_flag==1 ) Dprst_vol_clos_max(i) = DBLE( Dprst_area_clos_max(i)*Dprst_depth_avg(i) )
          Dprst_vol_open_max(i) = DBLE( Dprst_area_open_max(i)*Dprst_depth_avg(i) )

!         calculate the intial open and closed depression storage volume:
          IF ( Init_vars_from_file==0 ) THEN
            CALL check_param_value(i, 'dprst_frac_init', Dprst_frac_init(i), ierr)
            Dprst_vol_open(i) = DBLE(Dprst_frac_init(i))*Dprst_vol_open_max(i)
            IF ( Dprst_clos_flag==1 ) Dprst_vol_clos(i) = DBLE(Dprst_frac_init(i))*Dprst_vol_clos_max(i)
          ENDIF

!         threshold volume is calculated as the % of maximum open
!         depression storage above which flow occurs *  total open depression storage volume
          Dprst_vol_thres_open(i) = DBLE(Op_flow_thres(i))*Dprst_vol_open_max(i)

!         initial open and closed storage volume as fraction of total open and closed storage volume

!         Open depression surface area for each HRU:
          IF ( Dprst_vol_open(i)>DNEARZERO ) THEN
            open_vol_r = SNGL( Dprst_vol_open(i)/Dprst_vol_open_max(i) )
            IF ( open_vol_r<NEARZERO ) THEN
              frac_op_ar = 0.0
            ELSEIF ( open_vol_r>1.0 ) THEN
              frac_op_ar = 1.0
            ELSE
              frac_op_ar = EXP(Va_open_exp(i)*LOG(open_vol_r))
            ENDIF
            Dprst_area_open(i) = Dprst_area_open_max(i)*frac_op_ar
            IF ( Dprst_area_open(i)>Dprst_area_open_max(i) ) Dprst_area_open(i) = Dprst_area_open_max(i)
            IF ( Dprst_area_open(i)<NEARZERO ) Dprst_area_open(i) = 0.0
          ENDIF

!         Closed depression surface area for each HRU:
          IF ( Dprst_vol_clos(i)>DNEARZERO ) THEN
            clos_vol_r = SNGL( Dprst_vol_clos(i)/Dprst_vol_clos_max(i) )
            IF ( clos_vol_r<NEARZERO ) THEN
              frac_cl_ar = 0.0
            ELSEIF ( clos_vol_r>1.0 ) THEN
              frac_cl_ar = 1.0
            ELSE
              frac_cl_ar = EXP(Va_clos_exp(i)*LOG(clos_vol_r))
            ENDIF
            Dprst_area_clos(i) = Dprst_area_clos_max(i)*frac_cl_ar
            IF ( Dprst_area_clos(i)>Dprst_area_clos_max(i) ) Dprst_area_clos(i) = Dprst_area_clos_max(i)
            IF ( Dprst_area_clos(i)<NEARZERO ) Dprst_area_clos(i) = 0.0
          ENDIF

!         calculate the basin open and closed depression storage volumes
          Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open(i)
          Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos(i)
          Dprst_stor_hru(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/Hru_area_dble(i)
          IF ( Dprst_vol_open_max(i)>0.0 ) Dprst_vol_open_frac(i) = Dprst_vol_open(i)/Dprst_vol_open_max(i)
          IF ( Dprst_vol_clos_max(i)>0.0 ) Dprst_vol_clos_frac(i) = Dprst_vol_clos(i)/Dprst_vol_clos_max(i)
          Dprst_vol_frac(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/(Dprst_vol_open_max(i)+Dprst_vol_clos_max(i))
        ENDIF
      ENDDO
      Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
      Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
      IF ( Init_vars_from_file==0 ) DEALLOCATE ( Dprst_frac_init )

      END SUBROUTINE dprst_init

!***********************************************************************
!     Compute depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_comp(Dprst_vol_clos, Dprst_area_clos_max, Dprst_area_clos, &
     &           Dprst_vol_open_max, Dprst_vol_open, Dprst_area_open_max, Dprst_area_open, &
     &           Dprst_sroff_hru, Dprst_seep_hru, Sro_to_dprst, Sro_to_dprst_imperv, Dprst_evap_hru, &
     &           Avail_et, Net_rain, Dprst_in)
      USE PRMS_SRUNOFF, ONLY: Srp, Sri, Ihru, Perv_frac, Imperv_frac, Hruarea, Dprst_et_coef, &
     &    Dprst_seep_rate_open, Dprst_seep_rate_clos, Va_clos_exp, Va_open_exp, Dprst_flow_coef, &
     &    Dprst_vol_thres_open, Dprst_vol_clos_max, Dprst_insroff_hru, Upslope_hortonian, &
     &    Basin_dprst_volop, Basin_dprst_volcl, Basin_dprst_evap, Basin_dprst_seep, Basin_dprst_sroff, &
     &    Dprst_vol_open_frac, Dprst_vol_clos_frac, Dprst_vol_frac, Dprst_stor_hru
      USE PRMS_MODULE, ONLY: Cascade_flag !, Print_debug
      USE PRMS_BASIN, ONLY: NEARZERO, CLOSEZERO, DNEARZERO, Dprst_clos_flag, Dprst_frac_open, Dprst_frac_clos
      USE PRMS_INTCP, ONLY: Net_snow
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Pkwater_equiv
      USE PRMS_SNOW, ONLY: Snowmelt, Pptmix_nopack, Snowcov_area
      IMPLICIT NONE
      INTRINSIC EXP, LOG, MAX, DBLE, SNGL
! Arguments
      REAL, INTENT(IN) :: Dprst_area_open_max, Dprst_area_clos_max, Net_rain
      REAL, INTENT(IN) :: Sro_to_dprst, Sro_to_dprst_imperv
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_open_max
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_vol_open, Dprst_vol_clos, Dprst_in
      REAL, INTENT(INOUT) :: Avail_et
      REAL, INTENT(OUT) :: Dprst_area_open, Dprst_area_clos, Dprst_evap_hru
      DOUBLE PRECISION, INTENT(OUT) :: Dprst_sroff_hru, Dprst_seep_hru
! Local Variables
      REAL :: inflow, dprst_avail_et
      DOUBLE PRECISION :: dprst_srp, dprst_sri
      DOUBLE PRECISION :: dprst_srp_open, dprst_srp_clos, dprst_sri_open, dprst_sri_clos
      REAL :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r, unsatisfied_et
      DOUBLE PRECISION :: tmp, dprst_evap_open, dprst_evap_clos, seep_open, seep_clos
!***********************************************************************
!     add the hortonian flow to the depression storage volumes:
      IF ( Cascade_flag==1 ) THEN
        inflow = SNGL( Upslope_hortonian(Ihru) )
      ELSE
        inflow = 0.0
      ENDIF

      IF ( Pptmix_nopack(Ihru)==1 ) inflow = inflow + Net_rain

!******If precipitation on snowpack all water available to the surface is considered to be snowmelt
!******If there is no snowpack and no precip,then check for melt from last of snowpack.
!******If rain/snow mix with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt(Ihru)>0.0 ) THEN
        inflow = inflow + Snowmelt(Ihru)

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.
      ELSEIF ( Pkwater_equiv(Ihru)<DNEARZERO ) THEN

!      If no snowmelt and no snowpack but there was net snow then
!      snowpack was small and was lost to sublimation.
        IF ( Net_snow(Ihru)<NEARZERO .AND. Net_rain>0.0 ) THEN
          inflow = inflow + Net_rain
        ENDIF
      ENDIF

      Dprst_in = DBLE( inflow*Dprst_area_open_max ) ! inch-acres
      Dprst_vol_open = Dprst_vol_open + Dprst_in
      IF ( Dprst_clos_flag==1 ) THEN
        tmp = DBLE( inflow*Dprst_area_clos_max )
        Dprst_vol_clos = Dprst_vol_clos + tmp
        Dprst_in = Dprst_in + tmp
      ENDIF
      Dprst_in = Dprst_in/Hruarea ! inches over HRU

      ! add any pervious surface runoff fraction to depressions
      dprst_srp = 0.0D0
      dprst_sri = 0.0D0
      IF ( Srp>0.0D0 ) THEN
        tmp = Srp*DBLE(Perv_frac*Sro_to_dprst)*Hruarea
        dprst_srp_open = tmp*Dprst_frac_open(Ihru) ! acre-inches
        dprst_srp = dprst_srp_open/Hruarea
        Dprst_vol_open = Dprst_vol_open + dprst_srp_open
        IF ( Dprst_clos_flag==1 ) THEN
          dprst_srp_clos = tmp*Dprst_frac_clos(Ihru)
          dprst_srp = dprst_srp + dprst_srp_clos/Hruarea
          Dprst_vol_clos = Dprst_vol_clos + dprst_srp_clos
        ENDIF
        Srp = Srp - Srp*DBLE(Sro_to_dprst)
        IF ( Srp<0.0D0 ) THEN
          IF ( Srp<-DNEARZERO ) PRINT *, 'dprst srp<0.0', Srp, dprst_srp
          ! may need to adjust dprst_srp and volumes
          Srp = 0.0D0
        ENDIF
      ENDIF

      IF ( Sri>0.0D0 ) THEN
        tmp = Sri*DBLE(Imperv_frac*Sro_to_dprst_imperv)*Hruarea
        dprst_sri_open = tmp*Dprst_frac_open(Ihru)
        dprst_sri = dprst_sri_open/Hruarea
        Dprst_vol_open = Dprst_vol_open + dprst_sri_open
        IF ( Dprst_clos_flag==1 ) THEN
          dprst_sri_clos = tmp*Dprst_frac_clos(Ihru)
          dprst_sri = dprst_sri + dprst_sri_clos/Hruarea
          Dprst_vol_clos = Dprst_vol_clos + dprst_sri_clos
        ENDIF
        Sri = Sri - Sri*DBLE(Sro_to_dprst_imperv)
        IF ( Sri<0.0D0 ) THEN
          IF ( Sri<-DNEARZERO ) PRINT *, 'dprst sri<0.0', Sri, dprst_sri
          ! may need to adjust dprst_sri and volumes
          Sri = 0.0D0
        ENDIF
      ENDIF

      Dprst_insroff_hru(Ihru) = SNGL( dprst_srp + dprst_sri )

!     Compute spill from open surface depressions
      Dprst_sroff_hru = 0.0D0
      IF ( Dprst_vol_open>0.0D0 ) THEN
       ! compute open surface runoff
        Dprst_sroff_hru = MAX( 0.0D0, Dprst_vol_open-Dprst_vol_open_max )
        Dprst_sroff_hru = Dprst_sroff_hru + &
     &                    MAX( 0.0D0, (Dprst_vol_open-Dprst_sroff_hru-Dprst_vol_thres_open(Ihru))*DBLE(Dprst_flow_coef(Ihru)) )
        Dprst_vol_open = Dprst_vol_open - Dprst_sroff_hru
        Dprst_sroff_hru = Dprst_sroff_hru/Hruarea
        ! sanity checks
        IF ( Dprst_vol_open<0.0D0 ) THEN
          IF ( Dprst_vol_open<-DNEARZERO ) PRINT *, 'issue, dprst_vol_open<0.0', Dprst_vol_open
          Dprst_vol_open = 0.0D0
        ENDIF
      ENDIF

      ! compute seepage
      Dprst_seep_hru = 0.0D0
      IF ( Dprst_vol_open>0.0D0 ) THEN
        seep_open = Dprst_vol_open*DBLE(Dprst_seep_rate_open(Ihru))
        Dprst_vol_open = Dprst_vol_open - seep_open
        IF ( Dprst_vol_open<-DNEARZERO ) THEN
          seep_open = seep_open + Dprst_vol_open
          Dprst_vol_open = 0.0D0
        ENDIF
        Dprst_seep_hru = seep_open/Hruarea
      ENDIF

      IF ( Dprst_clos_flag==1 ) THEN
        IF ( Dprst_area_clos>NEARZERO ) THEN
          seep_clos = Dprst_vol_clos*DBLE(Dprst_seep_rate_clos(Ihru))
          Dprst_vol_clos = Dprst_vol_clos - seep_clos
          IF ( Dprst_vol_clos<-DNEARZERO ) THEN
            seep_clos = seep_clos + Dprst_vol_clos
            Dprst_vol_clos = 0.0D0
          ENDIF
          Dprst_seep_hru = Dprst_seep_hru + seep_clos/Hruarea
        ENDIF
        IF ( Dprst_vol_clos<0.0D0 ) THEN
          IF ( Dprst_vol_clos<-DNEARZERO ) PRINT *, 'issue, dprst_vol_clos<0.0', Dprst_vol_clos
          Dprst_vol_clos = 0.0D0
        ENDIF
      ENDIF

!     Open depression surface area for each HRU:
      Dprst_area_open = 0.0
      IF ( Dprst_vol_open>0.0D0 ) THEN
        open_vol_r = SNGL( Dprst_vol_open/Dprst_vol_open_max )
        IF ( open_vol_r<NEARZERO ) THEN
          frac_op_ar = 0.0
        ELSEIF ( open_vol_r>1.0 ) THEN
          frac_op_ar = 1.0
        ELSE
          frac_op_ar = EXP(Va_open_exp(Ihru)*LOG(open_vol_r))
        ENDIF
        Dprst_area_open = Dprst_area_open_max*frac_op_ar
        IF ( Dprst_area_open>Dprst_area_open_max ) Dprst_area_open = Dprst_area_open_max
        IF ( Dprst_area_open<NEARZERO ) Dprst_area_open = 0.0
      ENDIF

!     Closed depression surface area for each HRU:
      IF ( Dprst_clos_flag==1 ) THEN
        Dprst_area_clos = 0.0
        IF ( Dprst_vol_clos>0.0D0 ) THEN
          clos_vol_r = SNGL( Dprst_vol_clos/Dprst_vol_clos_max(Ihru) )
          IF ( clos_vol_r<NEARZERO ) THEN
            frac_cl_ar = 0.0
          ELSEIF ( clos_vol_r>1.0 ) THEN
            frac_cl_ar = 1.0
          ELSE
            frac_cl_ar = EXP(Va_clos_exp(Ihru)*LOG(clos_vol_r))
          ENDIF
          Dprst_area_clos = Dprst_area_clos_max*frac_cl_ar
          IF ( Dprst_area_clos>Dprst_area_clos_max ) Dprst_area_clos = Dprst_area_clos_max
          IF ( Dprst_area_clos<NEARZERO ) Dprst_area_clos = 0.0
        ENDIF
      ENDIF

      ! evaporate water from depressions based on snowcov_area
      ! dprst_evap_open & dprst_evap_clos = inches-acres on the HRU
      unsatisfied_et = Avail_et
      dprst_avail_et = (Potet(Ihru)*(1.0-Snowcov_area(Ihru)))*Dprst_et_coef(Ihru)
      Dprst_evap_hru = 0.0
      IF ( dprst_avail_et>0.0 ) THEN
        dprst_evap_open = 0.0D0
        dprst_evap_clos = 0.0D0
        IF ( Dprst_area_open>0.0 ) THEN
          dprst_evap_open = MIN(Dprst_area_open*dprst_avail_et, SNGL(Dprst_vol_open))
          IF ( SNGL(dprst_evap_open/Hruarea)>unsatisfied_et ) THEN
            !IF ( Print_debug>-1 ) THEN
            !  PRINT *, 'Warning, open dprst evaporation > available ET, HRU:, ', Ihru, unsatisfied_et, dprst_evap_open*DBLE(Dprst_frac_open(Ihru))
            !  PRINT *, 'Set to available ET, perhaps dprst_et_coef specified too large'
            !  PRINT *, 'Set print_debug to -1 to turn off message'
            !ENDIF
            dprst_evap_open = DBLE(unsatisfied_et)*Hruarea
          ENDIF
          !IF ( dprst_evap_open>Dprst_vol_open ) print *, '>', dprst_evap_open, dprst_vol_open
          IF ( dprst_evap_open>Dprst_vol_open ) dprst_evap_open = Dprst_vol_open
          unsatisfied_et = unsatisfied_et - SNGL( dprst_evap_open/Hruarea )
          Dprst_vol_open = Dprst_vol_open - dprst_evap_open
        ENDIF
        IF ( Dprst_area_clos>0.0 ) THEN
          dprst_evap_clos = MIN(Dprst_area_clos*dprst_avail_et, SNGL(Dprst_vol_clos))
          IF ( SNGL(dprst_evap_clos/Hruarea)>unsatisfied_et ) THEN
            !IF ( Print_debug>-1 ) THEN
            !  PRINT *, 'Warning, closed dprst evaporation > available ET, HRU:, ', Ihru, unsatisfied_et, dprst_evap_clos*DBLE(Dprst_clos_frac(Ihru))
            !  PRINT *, 'Set to available ET, perhaps dprst_et_coef specified too large'
            !  PRINT *, 'Set print_debug to -1 to turn off message'
            !ENDIF
            dprst_evap_clos = DBLE(unsatisfied_et)*Hruarea
          ENDIF
          IF ( dprst_evap_clos>Dprst_vol_clos ) dprst_evap_clos = Dprst_vol_clos
          Dprst_vol_clos = Dprst_vol_clos - dprst_evap_clos
        ENDIF
        Dprst_evap_hru = SNGL( (dprst_evap_open + dprst_evap_clos)/Hruarea )
      ENDIF

      Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open
      Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos
      Basin_dprst_evap = Basin_dprst_evap + DBLE(Dprst_evap_hru)*Hruarea
      Basin_dprst_seep = Basin_dprst_seep + DBLE(Dprst_seep_hru)*Hruarea
      Basin_dprst_sroff = Basin_dprst_sroff + Dprst_sroff_hru*Hruarea
      Avail_et = Avail_et - Dprst_evap_hru
      IF ( Dprst_vol_open_max>0.0 ) Dprst_vol_open_frac(Ihru) = Dprst_vol_open/Dprst_vol_open_max
      IF ( Dprst_vol_clos_max(Ihru)>0.0 ) Dprst_vol_clos_frac(Ihru) = Dprst_vol_clos/Dprst_vol_clos_max(Ihru)
      Dprst_vol_frac(Ihru) = (Dprst_vol_open+Dprst_vol_clos)/(Dprst_vol_open_max+Dprst_vol_clos_max(Ihru))
      Dprst_stor_hru(Ihru) = (Dprst_vol_open+Dprst_vol_clos)/Hruarea

      END SUBROUTINE dprst_comp

!***********************************************************************
!     srunoff_restart - write or read srunoff restart file
!***********************************************************************
      SUBROUTINE srunoff_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Nlake, Dprst_flag, Cascade_flag, Call_cascade
      USE PRMS_SRUNOFF
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_sroff_down, Basin_sroff_upslope, Basin_sroffi, Basin_sroffp, &
     &                            Basin_imperv_stor, Basin_imperv_evap, Basin_sroff, Basin_infil, Basin_hortonian, &
     &                            Sri, Srp, Basin_hortonian_lakes
        WRITE ( Restart_outunit ) Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep, &
     &                            Basin_dprst_volop, Basin_dprst_volcl, Basin_contrib_fraction, Basin_sroff_farflow, Strm_farfield
        WRITE ( Restart_outunit ) Hru_sroffp
        WRITE ( Restart_outunit ) Imperv_evap
        WRITE ( Restart_outunit ) Hru_impervevap
        WRITE ( Restart_outunit ) Hru_impervstor
        WRITE ( Restart_outunit ) Hru_sroffi
        WRITE ( Restart_outunit ) Hortonian_flow
        WRITE ( Restart_outunit ) Contrib_fraction
        IF ( Call_cascade==1 ) WRITE ( Restart_outunit ) Strm_seg_in
        IF ( Cascade_flag==1 ) THEN
          WRITE ( Restart_outunit ) Upslope_hortonian
          WRITE ( Restart_outunit ) Hru_hortn_cascflow
          IF ( Nlake>0 ) WRITE ( Restart_outunit ) Hortonian_lakes
        ENDIF
        IF ( Dprst_flag==1 ) THEN
          WRITE ( Restart_outunit ) Dprst_evap_hru
          WRITE ( Restart_outunit ) Dprst_seep_hru
          WRITE ( Restart_outunit ) Dprst_area_open
          WRITE ( Restart_outunit ) Dprst_sroff_hru
          WRITE ( Restart_outunit ) Dprst_insroff_hru
          WRITE ( Restart_outunit ) Dprst_area_clos
          WRITE ( Restart_outunit ) Dprst_stor_hru
          WRITE ( Restart_outunit ) Dprst_vol_thres_open
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_sroff_down, Basin_sroff_upslope, Basin_sroffi, Basin_sroffp, &
     &                          Basin_imperv_stor, Basin_imperv_evap, Basin_sroff, Basin_infil, Basin_hortonian, &
     &                          Sri, Srp, Basin_hortonian_lakes
        READ ( Restart_inunit ) Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep, &
     &                          Basin_dprst_volop, Basin_dprst_volcl, Basin_contrib_fraction, Basin_sroff_farflow, Strm_farfield
        READ ( Restart_inunit ) Hru_sroffp
        READ ( Restart_inunit ) Imperv_evap
        READ ( Restart_inunit ) Hru_impervevap
        READ ( Restart_inunit ) Hru_impervstor
        READ ( Restart_inunit ) Hru_sroffi
        READ ( Restart_inunit ) Hortonian_flow
        READ ( Restart_inunit ) Contrib_fraction
        IF ( Call_cascade==1 ) READ ( Restart_inunit ) Strm_seg_in
        IF ( Cascade_flag==1 ) THEN
          READ ( Restart_inunit ) Upslope_hortonian
          READ ( Restart_inunit ) Hru_hortn_cascflow
          IF ( Nlake>0 ) READ ( Restart_inunit ) Hortonian_lakes
        ENDIF
        IF ( Dprst_flag==1 ) THEN
          READ ( Restart_inunit ) Dprst_evap_hru
          READ ( Restart_inunit ) Dprst_seep_hru
          READ ( Restart_inunit ) Dprst_area_open
          READ ( Restart_inunit ) Dprst_sroff_hru
          READ ( Restart_inunit ) Dprst_insroff_hru
          READ ( Restart_inunit ) Dprst_area_clos
          READ ( Restart_inunit ) Dprst_stor_hru
          READ ( Restart_inunit ) Dprst_vol_thres_open
        ENDIF
      ENDIF
      END SUBROUTINE srunoff_restart

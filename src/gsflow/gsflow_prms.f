!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAXFILE_LENGTH = 128
      INTEGER, SAVE :: PRMS_flag
      INTEGER, SAVE :: Model, Process_flag
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Nlake
      INTEGER, SAVE :: Nsegment, Nsegmentp1, Nsfres, Ndepl, Nmodules
      INTEGER, SAVE :: Nhrucell, Nratetbl
      INTEGER, SAVE :: Dprst_flag, Transp_flag
      INTEGER, SAVE :: Sroff_flag, Solrad_flag, Et_flag, Subbasin_flag
      INTEGER, SAVE :: Strmflow_flag, Temp_flag, Precip_flag
      INTEGER, SAVE :: Et_climate_flag, Solrad_climate_flag
      INTEGER, SAVE :: Precip_climate_flag, Transp_climate_flag
      INTEGER, SAVE :: Call_cascade
      CHARACTER(LEN=16), SAVE :: Process
      CHARACTER(LEN=20), SAVE :: Precip_module, Temp_module, Et_module
      CHARACTER(LEN=20), SAVE :: Srunoff_module, Solrad_module
      CHARACTER(LEN=20), SAVE :: Strmflow_module, Transp_module
      CHARACTER(LEN=20), SAVE :: Soilzone_module, Gsfmf_module
      CHARACTER(LEN=80), SAVE :: Versn_gsfprms
      CHARACTER(LEN=80), SAVE :: Version_basin, Version_basin_sum
      CHARACTER(LEN=80), SAVE :: Version_cascade, Version_ccsolrad
      INTEGER, SAVE :: Basin_nc, Basin_sum_nc, Cascade_nc, Ccsolrad_nc
      CHARACTER(LEN=80), SAVE :: Version_climateflow, Version_ddsolrad
      INTEGER, SAVE :: Climateflow_nc, Ddsolrad_nc, Call_modules_nc
      CHARACTER(LEN=80), SAVE :: Version_climate_hru
      INTEGER, SAVE :: Climate_hru_nc, Gwflow_nc
      CHARACTER(LEN=80), SAVE :: Version_gwflow
      CHARACTER(LEN=80), SAVE :: Version_intcp
      CHARACTER(LEN=80), SAVE :: Version_map_results
      INTEGER, SAVE :: Intcp_nc, Map_results_nc
      CHARACTER(LEN=80), SAVE :: Version_obs, Version_potet_hamon
      CHARACTER(LEN=80), SAVE :: Version_potet_jh, Version_potet_pan
      INTEGER, SAVE :: Obs_nc, Potet_hamon_nc, Potet_jh_nc
      CHARACTER(LEN=80), SAVE :: Version_precip_1sta, Version_snowcomp
      CHARACTER(LEN=80), SAVE :: Version_precip_dist2, Version_soilzone
      INTEGER, SAVE :: Potet_pan_nc, Precip_1sta_nc, Snowcomp_nc
      CHARACTER(LEN=80), SAVE :: Version_precip_laps, Version_soltab
      INTEGER, SAVE :: Precip_dist2_nc, Soilzone_nc, Precip_laps_nc
      CHARACTER(LEN=80), SAVE :: Version_srunoff_carea, Version_strmflow
      CHARACTER(LEN=80), SAVE :: Version_srunoff_smidx, Version_subbasin
      INTEGER, SAVE :: Soltab_nc, Srunoff_carea_nc, Srunoff_smidx_nc
      CHARACTER(LEN=80), SAVE :: Version_temp_1sta, Version_temp_dist2
      INTEGER, SAVE :: Strmflow_nc, Subbasin_nc, Temp_1sta_nc
      CHARACTER(LEN=80), SAVE :: Version_temp_laps
      CHARACTER(LEN=80), SAVE :: Version_transp_tindex, Version_xyz_dist
      INTEGER, SAVE :: Temp_dist2_nc, Temp_laps_nc
      INTEGER, SAVE :: Xyz_dist_nc, Transp_tindex_nc
      CHARACTER(LEN=80), SAVE :: Version_gsflow_budget
      CHARACTER(LEN=80), SAVE :: Version_gsflow_sum
      CHARACTER(LEN=80), SAVE :: Version_gsflow_prms2mf
      INTEGER, SAVE :: Gsflow_budget_nc, Gsflow_sum_nc, Gsflow_prms_nc
      CHARACTER(LEN=80), SAVE :: Version_gsflow_mf2prms
      CHARACTER(LEN=80), SAVE :: Version_gsflow_prms
      INTEGER, SAVE :: Gsflow_mf2prms_nc, Gsflow_prms2mf_nc
      CHARACTER(LEN=80), SAVE :: Version_gsflow_modflow
      CHARACTER(LEN=80), SAVE :: Version_gsflow_setconv
      INTEGER, SAVE :: Gsflow_modflow_nc, Gsflow_setconv_nc
      CHARACTER(LEN=80), SAVE :: Version_write_climate_hru
      INTEGER, SAVE :: Write_climate_hru_nc
      CHARACTER(LEN=80), SAVE, ALLOCATABLE :: Module_versions(:)
      CHARACTER(LEN=12), PARAMETER :: MODNAME = 'call_modules'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME =
     +                                'Internal GSFLOW Caller    '
      INTEGER, SAVE :: Kper_mfo, Kkstp_mfo
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2;
!              6=xyz_dist; 7=climate_hru
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2;
!            6=xyz_dist; 7=climate_hru
! Control parameters
      INTEGER, SAVE :: Print_debug, MapOutON_OFF
      CHARACTER(LEN=MAXFILE_LENGTH) :: Tmin_day, Tmax_day, Precip_day
      CHARACTER(LEN=MAXFILE_LENGTH) :: Potet_day, Swrad_day, Transp_day
      CHARACTER(LEN=16), SAVE :: Model_mode
      END MODULE PRMS_MODULE

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION call_modules(Arg)
      USE PRMS_MODULE
      USE GLOBAL, ONLY: NSTP, NPER
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: check_dims, basin, climateflow
      INTEGER, EXTERNAL :: cascade, obs, soltab
      INTEGER, EXTERNAL :: transp_tindex
      INTEGER, EXTERNAL :: temp_1sta, temp_dist2, temp_laps
      INTEGER, EXTERNAL :: precip_1sta, precip_laps, climate_hru
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow
      INTEGER, EXTERNAL :: srunoff_smidx, srunoff_carea, soilzone
      INTEGER, EXTERNAL :: strmflow
      INTEGER, EXTERNAL :: subbasin, basin_sum, map_results
      INTEGER, EXTERNAL :: write_climate_hru
      INTEGER, EXTERNAL :: gsflow_modflow, gsflow_setconv
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms
      INTEGER, EXTERNAL :: gsflow_budget, gsflow_sum
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL module_error, version_check
      EXTERNAL module_version_check, module_doc
! Local Variables
      INTEGER :: n, i
!***********************************************************************
      call_modules = 1

      Process = Arg
      IF ( Process(:3)=='run' ) THEN
        Process_flag = 0 !(0=run, 1=declare, 2=init, 3=clean, 4=setdims)

      ELSEIF ( Process(:4)=='decl' ) THEN
        Process_flag = 1

        Versn_gsfprms =
     +'$Id: gsflow_prms.f 5498 2013-03-14 21:49:27Z rsregan $'
        Gsfmf_module = 'gsflow_modflow      '

        IF ( check_dims()/=0 ) STOP

        IF ( Model==2 ) THEN
! for MODFLOW simulations
          Kper_mfo = 1
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 )
     +         CALL module_error(Gsfmf_module, Arg, call_modules)
          Process_flag = 2
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 )
     +         CALL module_error(Gsfmf_module, Arg, call_modules)
          PRINT *, ' '
          Process_flag = 0
          DO WHILE ( Kper_mfo.LE.NPER )
            DO i = 1, NSTP(Kper_mfo)
              call_modules = gsflow_modflow()
              IF ( call_modules/=0 )
     +             CALL module_error(Gsfmf_module, Arg,call_modules)
            ENDDO
            Kper_mfo = Kper_mfo + 1
          ENDDO
          Process_flag = 3
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 )
     +         CALL module_error(Gsfmf_module, Arg, call_modules)
          STOP
        ENDIF

        IF ( Print_debug>-1 .AND. Model==1 ) THEN
          WRITE (*, 10) Versn_gsfprms(20:34)
          PRINT 15
          PRINT 16
        ENDIF
  10  FORMAT (///, 5X, 'Surface Water and Energy Budgets Simulated by:',
     +        /, 7X, 'PRMS Version 3.0.5 Tag: ', A, /)
  15  FORMAT (/, 'The following modules are available in this version:'
     +        , //, 5X, 'Process',  19X, 'Modules', /, 78('-'), /,
     +        'Basin Definition: basin', /,
     +        '  Cascading Flow: cascade', /,
     +        'Time Series Data: obs', /,
     +        '     Solar Table: soltab', /, 
     +        ' HRU Temperature: temp_1sta, temp_laps, temp_dist2,', /,
     +        '                  climate_hru', /,
     +       '      HRU Precip: precip_1sta, precip_laps, precip_dist2,'
     +   , /, '                  climate_hru', /,
     +        '     HRU Climate: xyz_dist', /,
     +        '   HRU Solar Rad: ccsolrad, ddsolrad, climate_hru', /,
     +        '   Transpiration: transp_tindex, climate_hru', /,
     +        '    Potential ET: potet_hamon, potet_jh, potet_pan,', /,
     +        '                  climate_hru', /,
     +        '    Interception: intcp', /,
     +        '       Snow Melt: snowcomp', /,
     +        '  Surface Runoff: srunoff_smidx, srunoff_carea', /,
     +        '       Soil Zone: soilzone', /,
     +        '     Groundwater: gwflow', /,
     +        '  Stream Routing: strmflow', /,
     +        '  Output Summary: basin_sum, subbasin, map_results', /,
     +        '   Preprocessing: frost_date, write_climate_hru')
  16  FORMAT (//, 'Selected modules listed in the order in which',
     +        ' they are called:', //, 10X, 'Process', 25X,
     +        'Module (source code version)', /, 78('-'))

        Call_modules_nc = INDEX( Versn_gsfprms, 'Z' ) + 1
        n = INDEX( Versn_gsfprms, '.f' ) + 1
        call_modules = declmodule(Versn_gsfprms(6:n),
     +                 'Internal GSFLOW Caller    ',
     +                 Versn_gsfprms(n+2:Call_modules_nc))

        IF ( Nmodules>0 ) CALL module_doc()

      ELSEIF ( Process(:4)=='init' ) THEN
        Process_flag = 2

        IF ( Nmodules>0 ) CALL module_version_check()

      ELSEIF ( Process(:7)=='setdims' ) THEN
        Process_flag = 4

      ELSE  !IF ( Process(:5)=='clean' ) THEN
        Process_flag = 3
      ENDIF

! All modules must be called for setdims, declare, initialize, and cleanup
      IF ( Process_flag/=0 ) THEN
        call_modules = basin()
        IF ( call_modules/=0 )
     +       CALL module_error('basin', Arg, call_modules)

        IF ( Call_cascade==1 .OR. Process_flag==1 ) THEN
          call_modules = cascade()
          IF ( call_modules/=0 )
     +         CALL module_error('cascade', Arg, call_modules)
        ENDIF

        call_modules = climateflow()
        IF ( call_modules/=0 )
     +       CALL module_error('climateflow', Arg, call_modules)

        call_modules = soltab()
        IF ( call_modules/=0 )
     +       CALL module_error('soltab', Arg, call_modules)
      ENDIF

      call_modules = obs()
      IF ( call_modules/=0 )
     +     CALL module_error('obs', Arg, call_modules)

      IF ( Temp_flag==6 ) THEN
        call_modules = xyz_dist()
        IF ( call_modules/=0 )
     +       CALL module_error(Precip_module, Arg, call_modules)
      ELSE
        IF ( Temp_flag==7 ) THEN
          call_modules = climate_hru()
        ELSEIF ( Temp_flag==1 ) THEN
          call_modules = temp_1sta()
        ELSEIF ( Temp_flag==2 ) THEN
          call_modules = temp_laps()
        ELSEIF ( Temp_flag==3 ) THEN
          call_modules = temp_dist2()
        ENDIF
        IF ( call_modules/=0 )
     +       CALL module_error(Temp_module, Arg, call_modules)

        IF ( Precip_climate_flag==1 ) THEN
          call_modules = climate_hru()
        ELSEIF ( Precip_flag==1 ) THEN
          call_modules = precip_1sta()
        ELSEIF ( Precip_flag==2 ) THEN
          call_modules = precip_laps()
        ELSEIF ( Precip_flag==3 ) THEN
          call_modules = precip_dist2()
        ENDIF
        IF ( call_modules/=0 )
     +       CALL module_error(Precip_module, Arg, call_modules)
      ENDIF

      IF ( Solrad_flag==1 ) THEN
        call_modules = ddsolrad()
      ELSEIF ( Solrad_climate_flag==1 ) THEN
        call_modules = climate_hru()
      ELSEIF ( Solrad_flag==2 ) THEN
        call_modules = ccsolrad()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Solrad_module, Arg, call_modules)

      IF ( Transp_flag==1 ) THEN
        call_modules = transp_tindex()
      ELSEIF ( Transp_climate_flag==1 ) THEN
        call_modules = climate_hru()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Transp_module, Arg, call_modules)

      IF ( Et_flag==1 ) THEN
        call_modules = potet_jh()
      ELSEIF ( Et_climate_flag==1 ) THEN
        call_modules = climate_hru()
      ELSEIF ( Et_flag==2 ) THEN
        call_modules = potet_hamon()
      ELSEIF ( Et_flag==4 ) THEN
        call_modules = potet_pan()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Et_module, Arg, call_modules)

      IF ( Model==4 ) THEN
        call_modules = write_climate_hru()
        IF ( call_modules/=0 )
     +       CALL module_error('write_climate_hru', Arg, call_modules)
        IF ( Process_flag==0 ) RETURN
      ENDIF

      call_modules = intcp()
      IF ( call_modules/=0 )
     +     CALL module_error('intcp', Arg, call_modules)

      call_modules = snowcomp()
      IF ( call_modules/=0 )
     +     CALL module_error('snowcomp', Arg, call_modules)

      IF ( Sroff_flag==1 ) THEN
        call_modules = srunoff_smidx()
      ELSE !IF ( Sroff_flag==2 ) THEN
        call_modules = srunoff_carea()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Srunoff_module, Arg, call_modules)

! for PRMS-only simulations
      IF ( PRMS_flag==1 ) THEN
        call_modules = soilzone()
        IF ( call_modules/=0 )
     +       CALL module_error(Soilzone_module, Arg, call_modules)

        call_modules = gwflow()
        IF ( call_modules/=0 )
     +       CALL module_error('gwflow', Arg, call_modules)

        call_modules = strmflow()
        IF ( call_modules/=0 )
     +       CALL module_error(Strmflow_module, Arg, call_modules)

        call_modules = basin_sum()
        IF ( call_modules/=0 )
     +       CALL module_error('basin_sum', Arg, call_modules)

        IF ( MapOutON_OFF>0 ) THEN
          call_modules = map_results()
          IF ( call_modules/=0 )
     +         CALL module_error('map_results', Arg, call_modules)
        ENDIF

! for GSFLOW simulations
      ELSEIF ( Model==0 ) THEN
!      ELSE

        call_modules = gsflow_modflow()
        IF ( call_modules/=0 )
     +       CALL module_error(Gsfmf_module, Arg, call_modules)

! The following modules are in the MODFLOW iteration loop
! (contained in gsflow_modflow.f).
! They still need to be called for declare, initialize and cleanup
        IF ( Process_flag/=0 ) THEN
          call_modules = gsflow_setconv()
          IF ( call_modules/=0 )
     +         CALL module_error('gsflow_setconv', Arg, call_modules)

! SOILZONE for GSFLOW is in the MODFLOW iteration loop,
! only call for declare, initialize, and cleanup.
          call_modules = soilzone()
          IF ( call_modules/=0 )
     +         CALL module_error(Soilzone_module, Arg, call_modules)

          call_modules = gsflow_prms2mf()
          IF ( call_modules/=0 )
     +         CALL module_error('gsflow_prms2mf', Arg, call_modules)

          call_modules = gsflow_mf2prms()
          IF ( call_modules/=0 )
     +         CALL module_error('gsflow_mf2prms', Arg, call_modules)
        ENDIF

        call_modules = gsflow_budget()
        IF ( call_modules/=0 )
     +       CALL module_error('gsflow_budget', Arg, call_modules)

        call_modules = gsflow_sum()
        IF ( call_modules/=0 )
     +       CALL module_error('gsflow_sum', Arg, call_modules)
      ENDIF

      IF ( Subbasin_flag==1 ) THEN
        call_modules = subbasin()
        IF ( call_modules/=0 )
     +       CALL module_error('subbasin', Arg, call_modules)
      ENDIF

      IF ( Process_flag==3 ) THEN
        IF ( MODEL==1 ) PRINT 9001
      ENDIF

      IF ( Process(:4)=='decl' ) PRINT 9002

 9001 FORMAT (/, 38('='), /, 'INFORMATION: Normal completion of PRMS',
     +        /, 38('='))
 9002 FORMAT (///, 'Messages from your model.', //,
     +        'Please give careful consideration to fixing all',
     +        ' ERRORS and WARNINGS.', /, 78('-'))

      call_modules = 0
      END FUNCTION call_modules

!***********************************************************************
!     declare the dimensions
!***********************************************************************
      INTEGER FUNCTION setdims()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix, call_modules
      INTEGER, EXTERNAL :: control_string, control_integer
      EXTERNAL read_error
      ! Maximum values are no longer limits
! Local Variables
      INTEGER, PARAMETER :: MAXDIM = 500
      INTEGER :: idim
!***********************************************************************
      setdims = 1

      ! debug print flag:
      ! -1=quiet - reduced screen output
      ! 0=none; 1=water balances; 2=basin;
      ! 4=basin_sum; 5=soltab; 7=soil zone;
      ! 9=snowcomp; 13=cascade; 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug')/=0 )
     +     CALL read_error(5, 'print_debug')

      IF ( control_string(Model_mode, 'model_mode')/=0 )
     +     CALL read_error(5, 'model_mode')
      PRMS_flag = 1
!       Model (0=GSFLOW; 1=PRMS; 2=MODFLOW)
      IF ( Model_mode(:6)=='GSFLOW' .OR. Model_mode(:4)=='    ') THEN
        Model = 0
        PRMS_flag = 0
      ELSEIF ( Model_mode(:4)=='PRMS' .OR. Model_mode(:5)=='DAILY' )THEN
        Model = 1
      ELSEIF ( Model_mode(:7)=='MODFLOW' ) THEN
        Model = 2
        PRMS_flag = 0
      ELSEIF ( Model_mode(:13)=='WRITE_CLIMATE' ) THEN
        Model = 4
      ELSE
        Model = -1
        PRINT 9002, Model_mode
        STOP
      ENDIF

      IF ( control_string(Temp_module, 'temp_module')/=0 )
     +     CALL read_error(5, 'temp_module')
      IF ( control_string(Precip_module, 'precip_module')
     +     /=0 ) CALL read_error(5, 'precip_module')
      Precip_flag = 1 ! precip_prms or precip_1sta
      IF ( Precip_module(:11)=='precip_laps' ) THEN
        Precip_flag = 2
      ELSEIF ( Precip_module(:12)=='precip_dist2' ) THEN
        Precip_flag = 3
      ELSEIF ( Precip_module(:11)=='climate_hru' ) THEN
        Precip_flag = 7
      ELSEIF ( Precip_module(:8)=='xyz_dist' ) THEN
        Precip_flag = 6
        IF ( Temp_module(:8)/='xyz_dist' ) THEN
          PRINT *, 'if xyz_dist is specified for precipitation module,'
          PRINT *, 'it also must be specified for temperature module.'
          PRINT *, 'using xyz_dist'
          Temp_module = 'xyz_dist            '
          Temp_flag = 6
        ENDIF
      ELSEIF ( Precip_module(:11)/='precip_1sta' .AND. 
     +         Precip_module(:11)/='precip_prms' ) THEN
        PRINT *, 'WARNING: Invalid precip_module value, reset to:',
     +           ' precip_1sta, value was: ', Precip_module
        Precip_module = 'precip_1sta         '
      ENDIF

      Temp_flag = 1 ! temp_1sta
      IF ( Temp_module(:9)=='temp_laps' ) THEN
        Temp_flag = 2
      ELSEIF ( Temp_module(:10)=='temp_dist2' ) THEN
        Temp_flag = 3
      ELSEIF ( Temp_module(:11)=='climate_hru' ) THEN
        Temp_flag = 7
      ELSEIF ( Temp_module(:8)=='xyz_dist' ) THEN
        Temp_flag = 6
        IF ( Precip_module(:8)/='xyz_dist' ) THEN
          PRINT *, 'if xyz_dist is specified for temperature module,'
          PRINT *, 'it also must be specified for precipitation module.'
          PRINT *, 'using xyz_dist'
          Precip_module = 'xyz_dist            '
          Precip_flag = 6
        ENDIF
      ELSEIF ( Temp_module(:9)/='temp_1sta' ) THEN
        PRINT *, 'WARNING: Invalid temp_module value, reset to:',
     +           ' temp_1sta, value was: ', Temp_module
        Temp_module = 'temp_1sta           '
      ENDIF

      IF ( control_string(Transp_module, 'transp_module')
     +     /=0 ) CALL read_error(5, 'transp_module')
      Transp_flag = 1 ! transp_tindex
      IF ( Transp_module(:11)=='climate_hru' ) THEN
        Transp_flag = 3
      ELSEIF ( Transp_module(:13)/='transp_tindex' ) THEN
        PRINT *, 'WARNING: Invalid transp_module value, reset to:',
     +           ' transp_tindex, value was: ', Transp_module
        Transp_module = 'transp_tindex       '
      ENDIF

      IF ( control_string(Et_module, 'et_module')/=0 )
     +     CALL read_error(5, 'et_module')
      Et_flag = 1 ! potet_jh
      IF ( Et_module(:11)=='potet_hamon' ) THEN
        Et_flag = 2
      ELSEIF ( Et_module(:11)=='climate_hru' ) THEN
        Et_flag = 7
      ELSEIF ( Et_module(:9)=='potet_pan' ) THEN
        Et_flag = 4
      ELSEIF ( Et_module(:8)/='potet_jh' ) THEN
        PRINT *, 'WARNING: Invalid et_module value, reset to:',
     +           ' potet_jh, value was: ', Et_module
        Et_module = 'potet_jh            '
      ENDIF

      IF ( control_string(Srunoff_module, 'srunoff_module')
     +     /=0 ) CALL read_error(5, 'srunoff_module')
      Sroff_flag = 1 ! srunoff_smidx
      IF ( Srunoff_module(:13)=='srunoff_carea' ) THEN
        Sroff_flag = 2
      ELSEIF ( Srunoff_module(:13)/='srunoff_smidx' ) THEN
        PRINT *, 'WARNING: Invalid srunoff_module value, reset to:',
     +           ' srunoff_smidx, value was: ', Srunoff_module
        Srunoff_module = 'srunoff_smidx       '
      ENDIF

      Soilzone_module = 'soilzone            '
      IF ( control_string(Solrad_module, 'solrad_module')
     +     /=0 ) CALL read_error(5, 'solrad_module')
      Solrad_flag = 1 ! ddsolrad
      IF ( Solrad_module(:11)=='climate_hru' ) THEN
        Solrad_flag = 7
      ELSEIF ( Solrad_module(:8)=='ccsolrad' ) THEN
        Solrad_flag = 2
      ELSEIF ( Solrad_module(:8)/='ddsolrad' ) THEN
        PRINT *, 'WARNING: Invalid solrad_module value, reset to:',
     +           ' ddsolrad, value was: ', Solrad_module
        Solrad_module = 'ddsolrad            '
      ENDIF

      Precip_climate_flag = 0
      IF ( Precip_flag==7 .AND. Temp_flag/=7 ) THEN
           Precip_climate_flag = 1
      ENDIF

      Solrad_climate_flag = 0
      IF ( Solrad_flag==7 .AND. Temp_flag/=7 .AND. Precip_flag/=7 ) THEN
        Solrad_climate_flag = 1
      ENDIF

      Transp_climate_flag = 0
      IF ( Transp_flag==3 .AND. Temp_flag/=7 .AND. Precip_flag/=7 .AND.
     +     Solrad_flag/=7 ) THEN
        Transp_climate_flag = 1
      ENDIF

      Et_climate_flag = 0
      IF ( Et_flag==7 .AND. Temp_flag/=7 .AND. Precip_flag/=7 .AND.
     +     Solrad_flag/=7 .AND. Transp_flag/=3 ) THEN
        Et_climate_flag = 1
      ENDIF

      Strmflow_module = 'strmflow            '
      Strmflow_flag = 1 ! strmflow

! cascade dimensions
      IF ( decldim('ncascade', 0, MAXDIM,
     +     'Number of HRU links for cascading flow')/=0 )
     +      CALL read_error(7, 'ncascade')
      IF ( decldim('ncascdgw', 0, MAXDIM,
     +     'Number of GWR links for cascading flow')/=0 )
     +      CALL read_error(7, 'ncascdgw')

! nsegment dimension
      IF ( decldim('nsegment', 1, MAXDIM,
     +     'Number of stream-channel segments')/=0 )
     +     CALL read_error(7, 'nsegment')

! subbasin dimensions
      IF ( control_integer(Subbasin_flag, 'subbasin_flag')/=0 )
     +     Subbasin_flag = 1
      IF ( decldim('nsub', 0, MAXDIM, 'Number of internal subbasins')
     +     /=0 ) CALL read_error(7, 'nsub')

      IF ( control_integer(Dprst_flag, 'dprst_flag')/=0 )
     +     Dprst_flag = 0

! map results dimensions
      IF ( control_integer(MapOutON_OFF, 'mapOutON_OFF')
     +     /=0 ) MapOutON_OFF = 0
      idim = 0
      IF ( Model==0 .OR. MapOutON_OFF==1 ) idim = 1
      IF ( decldim('nhrucell', idim, MAXDIM,
     +     'Number of unique intersections between HRUs and mapped'//
     +     ' spatial units of a target map')
     +     /=0 ) CALL read_error(7, 'nhrucell')
      IF ( decldim('ngwcell', idim, MAXDIM,
     +     'Number of spatial units in the target grid for mapped'//
     +     ' results')/=0 ) CALL read_error(7, 'ngwcell')
      IF ( decldim('nreach', idim, MAXDIM,
     +     'Number of reaches on all stream segments')/=0 )
     +      CALL read_error(7, 'nreach')

! spatial units
      IF ( decldim('ngw', 1, MAXDIM, 'Number of GWRs')
     +     /=0 ) CALL read_error(7, 'ngw')
      IF ( decldim('nhru', 1, MAXDIM,
     +     'Number of HRUs')/=0 )
     +     CALL read_error(7, 'nhru')
      IF ( decldim('nssr', 1, MAXDIM, 'Number of subsurface reservoirs')
     +     /=0 ) CALL read_error(7, 'nssr')

      idim = 0
      IF ( Strmflow_flag==2 ) idim = 1
      IF ( decldim('nsfres', idim, MAXDIM,
     +     'Number of storage-detention reservoirs and lakes'//
     +     ' connected to the stream network')/=0 )
     +     CALL read_error(7, 'nsfres')
      IF ( decldim('nlake', idim, MAXDIM,
     +     'Number of lake HRUs')/=0 ) CALL read_error(7, 'nlake')

! Time-series data stations, need to know if in Data File
      IF ( decldim('nrain', 0, MAXDIM, 'Number of precipitation'//
     +     ' measurement stations')/=0 ) CALL read_error(7, 'nrain')
      IF ( decldim('nsol', 0, MAXDIM,
     +     'Number of solar-radiation measurement stations')/=0 )
     +       CALL read_error(7, 'nsol')
      IF ( decldim('ntemp', 0, MAXDIM,
     +     'Number of air-temperature measurement stations')
     +     /=0 ) CALL read_error(7, 'ntemp')
      IF ( decldim('nratetbl', 0, MAXDIM,
     +     'Number of rating-table data sets for lake elevations')
     +     /=0 ) CALL read_error(7, 'nratetbl')

! depletion curves
      IF ( decldim('ndepl', 1, MAXDIM,
     +     'Number of snow-depletion curves')/=0 )
     +      CALL read_error(7, 'ndelp')
      IF ( decldim('ndeplval', 11, MAXDIM, 'Number of values in'//
     +     ' all snow-depletion curves (set to ndepl*11)')
     +     /=0 ) CALL read_error(7, 'ndelplval')

      IF ( decldim('nmodules', 0, MAXDIM,
     +     'Number of declared modules in a simulation')/=0 )
     +      CALL read_error(7, 'nmodules')

! fixed dimensions
      IF ( declfix('ndays', 366, 366,
     +     'Maximum number of days in a year ')/=0 )
     +     CALL read_error(7, 'ndays')
      IF ( declfix('nmonths', 12, 12, 'Number of months in a year')
     +     /=0 ) CALL read_error(7, 'nmonths')

      IF ( call_modules('setdims')/=0 ) STOP 'ERROR, in setdims'

 9002 FORMAT ('ERROR in setdims: model_mode: ', A, ' not implemented',/)

      setdims = 0
      END FUNCTION setdims

!***********************************************************************
!     Get and check consistency of dimensions with flags
!***********************************************************************
      INTEGER FUNCTION check_dims()
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Nsub, Nmodules, Nsfres,
     +    Nlake, Nsegment, Nsegmentp1, Ndepl,
     +    Strmflow_flag, Subbasin_flag, Nhrucell
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getdim
!***********************************************************************
      check_dims = 1

      Nhru = getdim('nhru')
      IF ( Nhru==-1 ) CALL read_error(7, 'nhru')

      Nssr = getdim('nssr')
      IF ( Nssr==-1 ) CALL read_error(7, 'nssr')

      Ngw = getdim('ngw')
      IF ( Ngw==-1 ) CALL read_error(7, 'ngw')

      IF ( Nhru==0 .OR. Nssr==0 .OR. Ngw==0 ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must be > 0: nhru=', Nhru,
     +           ' nssr=', Nssr, ' ngw=', Ngw
        STOP
      ENDIF

      IF ( Nssr/=Nhru .OR. Ngw/=Nhru ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must be equal in GSFLOW',
     +           ' nhru=', Nhru, ' nssr=', Nssr, ' ngw=', Ngw
        STOP
      ENDIF

      Nmodules = getdim('nmodules')
      IF ( Nmodules==-1 ) CALL read_error(7, 'nmodules')

      Nsfres = getdim('nsfres')
      IF ( Nsfres==-1 ) CALL read_error(7, 'nsfres')
      Nlake = getdim('nlake')
      IF ( Nlake==-1 ) CALL read_error(7, 'nlake')
      IF ( Nsfres>0 .AND. Nlake==0 ) THEN
        PRINT *, 'ERROR, dimension nsfres has been changed to nlake.'
        PRINT *, '       All references to nsfres must be changed to',
     +           ' nlake in your Parameter File'
        PRINT *, 'The following parameter names have been changed'
        PRINT *, '   old name        new name'
        PRINT *, 'sfres_type       lake_type'
        PRINT *, 'sfres_out2       lake_out2'
        PRINT *, 'sfres_out2_a     lake_out2_a'
        PRINT *, 'sfres_out2_b     lake_out2_b'
        PRINT *, 'ratetbl_sfres    ratetbl_lake'
        PRINT *, 'sfres_qro        lake_qro'
        PRINT *, 'sfres_din1       lake_din1'
        PRINT *, 'sfres_init       lake_init'
        PRINT *, 'sfres_coef       lake_coef'
        PRINT *, 'sfres_vol_init   lake_vol_init'
        PRINT *, 'elevsurf_init    elevlake_init'
        PRINT *, 'sfres_hru        lake_hru'
        PRINT *, 'seg_res_id       seg_lake_id'
        PRINT *, 'sfres_seep_elev  lake_seep_elev'
        PRINT *, 'Data File: sfres_elev changed to lake_elev'
        PRINT *, ' '
        STOP
      ENDIF

      IF ( Strmflow_flag==2 ) THEN
        IF ( Nlake==0 ) THEN
          PRINT *, 
     +       'ERROR, nlake must be at least 1 when using strmflow_lake'
          STOP
        ENDIF
      ENDIF

      Ndepl = getdim('ndepl')
      IF ( Ndepl==-1 ) CALL read_error(7, 'ndepl')

      Nsub = getdim('nsub')
      IF ( Nsub==-1 ) CALL read_error(7, 'nsub')
      IF ( Subbasin_flag==1 .AND. Nsub==0 ) Subbasin_flag = 0

      Nsegment = getdim('nsegment')
      IF ( Nsegment==-1 ) CALL read_error(7, 'nsegment')
      Nsegmentp1 = Nsegment + 1

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell==-1 ) CALL read_error(6, 'nhrucell')

      check_dims = 0
      END FUNCTION check_dims

!**********************************************************************
!     Module documentation
!**********************************************************************
      SUBROUTINE module_doc()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam
      EXTERNAL read_error
!**********************************************************************
      ALLOCATE ( Module_versions(Nmodules) )
      IF ( declparam('call_modules', 'module_versions', 'nmodules',
     +     'string',
     +     '#', '#', 'z',
     +     'Version identifiers associated with the Parameter File',
     +     'Version identifiers associated with the Parameter File',
     +     'string')/=0 ) CALL read_error(1, 'module_versions')

      Version_basin = ' '
      Version_basin_sum = ' '
      Version_obs = ' '
      Version_snowcomp = ' '
      Version_gwflow = ' '
      Version_intcp = ' '
      Version_climateflow = ' '
      Version_cascade = ' '
      Version_ccsolrad = ' '
      Version_ddsolrad = ' '
      Version_climate_hru = ' '
      Version_map_results = ' '
      Version_potet_hamon = ' '
      Version_potet_jh = ' '
      Version_potet_pan = ' '
      Version_precip_1sta = ' '
      Version_soilzone = ' '
      Version_precip_dist2 = ' '
      Version_precip_laps = ' '
      Version_soltab = ' '
      Version_srunoff_carea = ' '
      Version_strmflow = ' '
      Version_srunoff_smidx = ' '
      Version_subbasin = ' '
      Version_temp_1sta = ' '
      Version_temp_dist2 = ' '
      Version_temp_laps = ' '
      Version_transp_tindex = ' '
      Version_write_climate_hru = ' '
      Version_gsflow_budget = ' '
      Version_gsflow_sum = ' '
      Version_gsflow_prms2mf = ' '
      Version_gsflow_mf2prms = ' '
      Version_gsflow_prms = ' '
      Version_gsflow_modflow = ' '
      Version_gsflow_setconv = ' '

      END SUBROUTINE module_doc

!**********************************************************************
!     Module version check strings
!**********************************************************************
      SUBROUTINE module_version_check()
      USE PRMS_MODULE
! Functions
      INTEGER, EXTERNAL :: getparamstring
      EXTERNAL read_error, version_check
! Local Variable
      INTEGER :: i
!**********************************************************************
! compare module versions
      DO i = 1, Nmodules
        IF ( getparamstring('call_modules', 'module_versions', Nmodules,
     +       'string', i-1, Module_versions(i))/=0 )
     +       CALL read_error(2, 'module_versions')
        IF ( Module_versions(i)(:1)=='#' ) CYCLE
        IF ( Module_versions(i)(18:29)=='call_modules' ) THEN
          CALL version_check(Module_versions(i), Call_modules_nc,
     +                       Versn_gsfprms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='basin_sum' ) THEN
          CALL version_check(Module_versions(i), Basin_sum_nc,
     +                       Version_basin_sum)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:22)=='basin' ) THEN
          CALL version_check(Module_versions(i), Basin_nc,
     +                       Version_basin)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='climateflow' ) THEN
          CALL version_check(Module_versions(i), Climateflow_nc,
     +                       Version_climateflow)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:22)=='intcp' ) THEN
          CALL version_check(Module_versions(i), Intcp_nc,
     +                       Version_intcp)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:23)=='gwflow' ) THEN
          CALL version_check(Module_versions(i), Gwflow_nc,
     +                       Version_gwflow)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:20)=='obs' ) THEN
          CALL version_check(Module_versions(i), Obs_nc,
     +                       Version_obs)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:24)=='cascade' ) THEN
          CALL version_check(Module_versions(i), Cascade_nc,
     +                       Version_cascade)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:23)=='soltab' ) THEN
          CALL version_check(Module_versions(i), Soltab_nc,
     +                       Version_soltab)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='ccsolrad' ) THEN
          CALL version_check(Module_versions(i), Ccsolrad_nc,
     +                       Version_ccsolrad)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='ddsolrad' ) THEN
          CALL version_check(Module_versions(i), Ddsolrad_nc,
     +                       Version_ddsolrad)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='potet_hamon' ) THEN
          CALL version_check(Module_versions(i), Potet_hamon_nc,
     +                       Version_potet_hamon)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='climate_hru' ) THEN
          CALL version_check(Module_versions(i), Climate_hru_nc,
     +                       Version_climate_hru)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='xyz_dist' ) THEN
          CALL version_check(Module_versions(i), Xyz_dist_nc,
     +                       Version_xyz_dist)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='map_results' ) THEN
          CALL version_check(Module_versions(i), Map_results_nc,
     +                       Version_map_results)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='potet_jh' ) THEN
          CALL version_check(Module_versions(i), Potet_jh_nc,
     +                       Version_potet_jh)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='potet_pan' ) THEN
          CALL version_check(Module_versions(i), Potet_pan_nc,
     +                       Version_potet_pan)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='precip_1sta' ) THEN
          CALL version_check(Module_versions(i), Precip_1sta_nc,
     +                       Version_precip_1sta)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='precip_laps' ) THEN
          CALL version_check(Module_versions(i), Precip_laps_nc,
     +                       Version_precip_laps)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:29)=='precip_dist2' ) THEN
          CALL version_check(Module_versions(i), Precip_dist2_nc,
     +                       Version_precip_dist2)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='soilzone' ) THEN
          CALL version_check(Module_versions(i), Soilzone_nc,
     +                       Version_soilzone)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='snowcomp' ) THEN
          CALL version_check(Module_versions(i), Snowcomp_nc,
     +                       Version_snowcomp)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='srunoff_carea' ) THEN
          CALL version_check(Module_versions(i), Srunoff_carea_nc,
     +                       Version_srunoff_carea)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='srunoff_smidx' ) THEN
          CALL version_check(Module_versions(i), Srunoff_smidx_nc,
     +                       Version_srunoff_smidx)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:27)=='temp_dist2' ) THEN
          CALL version_check(Module_versions(i), Temp_dist2_nc,
     +                       Version_temp_dist2)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='temp_laps' ) THEN
          CALL version_check(Module_versions(i), Temp_laps_nc,
     +                       Version_temp_laps)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='temp_1sta' ) THEN
          CALL version_check(Module_versions(i), Temp_1sta_nc,
     +                       Version_temp_1sta)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='strmflow' ) THEN
          CALL version_check(Module_versions(i), Strmflow_nc,
     +                       Version_strmflow)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='subbasin' ) THEN
          CALL version_check(Module_versions(i), Subbasin_nc,
     +                       Version_subbasin)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='transp_tindex' ) THEN
          CALL version_check(Module_versions(i), Transp_tindex_nc,
     +                       Version_transp_tindex)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:34)=='write_climate_hru' ) THEN
          CALL version_check(Module_versions(i), Write_climate_hru_nc,
     +                       Version_write_climate_hru)
        ENDIF
        IF ( Module_versions(i)(18:31)=='gsflow_modflow' ) THEN
          CALL version_check(Module_versions(i), Gsflow_modflow_nc,
     +                       Version_gsflow_modflow)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='gsflow_prms' ) THEN
          CALL version_check(Module_versions(i), Gsflow_prms_nc,
     +                       Version_gsflow_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:31)=='gsflow_prms2mf' ) THEN
          CALL version_check(Module_versions(i), Gsflow_prms2mf_nc,
     +                       Version_gsflow_prms2mf)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:31)=='gsflow_mf2prms' ) THEN
          CALL version_check(Module_versions(i), Gsflow_mf2prms_nc,
     +                       Version_gsflow_mf2prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='gsflow_budget' ) THEN
          CALL version_check(Module_versions(i), Gsflow_budget_nc,
     +                       Version_gsflow_budget)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:27)=='gsflow_sum' ) THEN
          CALL version_check(Module_versions(i), Gsflow_sum_nc,
     +                       Version_gsflow_sum)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:31)=='gsflow_setconv' ) THEN
          CALL version_check(Module_versions(i), Gsflow_setconv_nc,
     +                       Version_gsflow_setconv)
          CYCLE
        ENDIF
      ENDDO

      END SUBROUTINE module_version_check

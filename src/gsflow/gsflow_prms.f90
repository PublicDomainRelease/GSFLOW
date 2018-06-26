!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAXFILE_LENGTH = 256, MAXCONTROL_LENGTH = 32
      INTEGER, PARAMETER :: MAXDIM = 500
      CHARACTER(LEN=68), PARAMETER :: &
     &  EQULS = '===================================================================='
      CHARACTER(LEN=11), PARAMETER :: MODNAME = 'gsflow_prms'
      CHARACTER(LEN=24), PARAMETER :: PRMS_VERSION = 'Version 4.0.3 06/01/2017'
      CHARACTER(LEN=8), SAVE :: Process
      CHARACTER(LEN=80), SAVE :: PRMS_versn
      INTEGER, SAVE :: Model, Process_flag, Call_cascade, Ncascade, Ncascdgw
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Nhrucell, Nlake, Ngwcell, Nlake_hrus
      INTEGER, SAVE :: Ntemp, Nrain, Nsol, Nsegment, Ndepl, Nobs, Nevap, Ndeplval
      INTEGER, SAVE :: Starttime(6), Endtime(6)
      INTEGER, SAVE :: Start_year, Start_month, Start_day, End_year, End_month, End_day
      INTEGER, SAVE :: Transp_flag, Sroff_flag, Solrad_flag, Et_flag
      INTEGER, SAVE :: Climate_temp_flag, Climate_precip_flag, Climate_potet_flag, Climate_transp_flag
      INTEGER, SAVE :: Lake_route_flag, Nratetbl, Strmflow_flag, Stream_order_flag
      INTEGER, SAVE :: Temp_flag, Precip_flag, Climate_hru_flag, Climate_swrad_flag
      INTEGER, SAVE :: Precip_combined_flag, Temp_combined_flag
      INTEGER, SAVE :: Inputerror_flag, Timestep
      INTEGER, SAVE :: Humidity_cbh_flag, Windspeed_cbh_flag
      INTEGER, SAVE :: Grid_flag, Logunt, First_timestep
      INTEGER, SAVE :: Kper_mfo, Kkstp_mfo, PRMS_flag
      INTEGER, SAVE :: PRMS_output_unit, Restart_inunit, Restart_outunit
      INTEGER, SAVE :: Elapsed_time_start(8), Elapsed_time_end(8), Elapsed_time_minutes
      REAL, SAVE :: Execution_time_start, Execution_time_end, Elapsed_time
      INTEGER, SAVE :: Kkiter
!   Declared Parameters
      INTEGER, SAVE :: Mxsziter
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru
! Control parameters
      INTEGER, SAVE :: Print_debug, MapOutON_OFF, CsvON_OFF, Dprst_flag, Subbasin_flag, Parameter_check_flag
      INTEGER, SAVE :: Init_vars_from_file, Save_vars_to_file, Orad_flag, Cascade_flag, Cascadegw_flag
      INTEGER, SAVE :: NhruOutON_OFF, Gwr_swale_flag, NsubOutON_OFF, BasinOutON_OFF, NsegmentOutON_OFF
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Model_output_file, Var_init_file, Var_save_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Csv_output_file, Model_control_file, Param_file
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Temp_module, Srunoff_module, Et_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Strmflow_module, Transp_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Model_mode, Precip_module, Solrad_module
      CHARACTER(LEN=8), SAVE :: Soilzone_module
      END MODULE PRMS_MODULE

!***********************************************************************
      INTEGER FUNCTION call_modules(Arg)
      USE PRMS_MODULE
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTRINSIC :: DATE_AND_TIME, INT
      INTEGER, EXTERNAL :: check_dims, basin, climateflow, prms_time
      INTEGER, EXTERNAL :: cascade, obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: transp_frost, frost_date, routing
      INTEGER, EXTERNAL :: temp_1sta_laps, temp_dist2
      INTEGER, EXTERNAL :: precip_1sta_laps, climate_hru
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon, potet_hs, potet_pt, potet_pm
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow
      INTEGER, EXTERNAL :: srunoff, soilzone
      INTEGER, EXTERNAL :: strmflow, subbasin, basin_sum, map_results, write_climate_hru
      INTEGER, EXTERNAL :: strmflow_in_out, muskingum, potet_pm_sta
      EXTERNAL :: module_doc, nsub_summary, nsegment_summary, basin_summary
      INTEGER, EXTERNAL :: gsflow_modflow, gsflow_prms2mf, gsflow_mf2prms, gsflow_budget, gsflow_sum
      INTEGER, EXTERNAL :: declvar, declparam, getparam, numchars
      EXTERNAL :: module_error, read_error, print_module, PRMS_open_output_file
      EXTERNAL :: call_modules_restart, check_nhru_params, water_balance, nhru_summary
! Local Variables
      INTEGER :: i, iret, nc
!***********************************************************************
      call_modules = 1

      Process = Arg

      IF ( Process(:3)=='run' ) THEN
        Process_flag = 0 !(0=run, 1=declare, 2=init, 3=clean, 4=setdims)

      ELSEIF ( Process(:4)=='decl' ) THEN
        IF ( Model==1 ) THEN
          CALL DATE_AND_TIME(VALUES=Elapsed_time_start)
          Execution_time_start = Elapsed_time_start(5)*3600 + Elapsed_time_start(6)*60 + &
     &                           Elapsed_time_start(7) + Elapsed_time_start(8)*0.001
        ENDIF
        Process_flag = 1

        PRMS_versn = 'gsflow_prms.f90 2017-11-14 15:20:00Z'

        IF ( check_dims()/=0 ) STOP

        IF ( Print_debug>-1 ) PRINT 10, PRMS_VERSION
        WRITE ( Logunt, 10 ) PRMS_VERSION
        WRITE ( PRMS_output_unit, 10 ) PRMS_VERSION
  10  FORMAT (/, 15X, 'Precipitation-Runoff Modeling System (PRMS)', /, 23X, A)
  15  FORMAT (/, 8X, 'Process',  12X, 'Available Modules', /, 68('-'), /, &
     &        '  Basin Definition: basin', /, &
     &        '    Cascading Flow: cascade', /, &
     &        '  Time Series Data: obs', /, &
     &        '   Potet Solar Rad: soltab', /, &
     &        '  Temperature Dist: temp_1sta, temp_laps, temp_dist2, climate_hru', /, &
     &        '       Precip Dist: precip_1sta, precip_laps, precip_dist2,', /, &
     &        '                    climate_hru', /, &
     &        'Temp & Precip Dist: xyz_dist, ide_dist', /, &
     &        '    Solar Rad Dist: ccsolrad, ddsolrad, climate_hru', /, &
     &        'Transpiration Dist: transp_tindex, climate_hru, transp_frost', /, &
     &        '      Potential ET: potet_hamon, potet_jh, potet_pan, climate_hru,', /, &
     &        '                    potet_hs, potet_pt, potet_pm, potet_pm_sta', /, &
     &        '      Interception: intcp', /, &
     &        '     Snow Dynamics: snowcomp', /, &
     &        '    Surface Runoff: srunoff_smidx, srunoff_carea', /, &
     &        '         Soil Zone: soilzone', /, &
     &        '       Groundwater: gwflow', /, &
     &        'Streamflow Routing: strmflow, strmflow_in_out, muskingum', /, &
     &        '    Output Summary: basin_sum, subbasin, map_results, nhru_summary', /, &
     &        '                    nsub_summary, basin_summary, water_balance', /, &
     &        '     Preprocessing: write_climate_hru, frost_date', /, 68('-'))
  16  FORMAT (//, 4X, 'Active modules listed in the order in which they are called', //, 8X, 'Process', 19X, &
     &        'Module', 16X, 'Version Date', /, A)

        IF ( Model==0 ) THEN
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
        ENDIF

        IF ( Print_debug>-1 ) THEN
          PRINT 15
          PRINT 9002
          PRINT 16, EQULS
        ENDIF
        WRITE ( Logunt, 15 )
        WRITE ( Logunt, 16 ) EQULS
        WRITE ( PRMS_output_unit, 15 )
        WRITE ( PRMS_output_unit, 16 ) EQULS
        CALL print_module(PRMS_versn, 'GSFLOW Computation Order    ', 90)

        IF ( Model==0 .OR. Model==99 ) THEN
          IF ( declvar(MODNAME, 'KKITER', 'one', 1, 'integer', &
     &         'Current iteration in GSFLOW simulation', 'none', KKITER)/=0 ) CALL read_error(3, 'KKITER')
          IF ( declparam(MODNAME, 'mxsziter', 'one', 'integer', &
     &         '0', '0', '5000', &
     &         'Maximum number of iterations soilzone states are computed', &
     &         'Maximum number of iterations soilzone states are computed', &
     &         'none')/=0 ) CALL read_error(1, 'mxsziter')
          ALLOCATE ( Gvr_cell_pct(Nhrucell) )
          IF ( Nhru/=Nhrucell ) THEN
            IF ( declparam(MODNAME, 'gvr_cell_pct', 'nhrucell', 'real', &
     &           '0.0', '0.0', '1.0', &
     &           'Proportion of the grid cell associated with each GVR', &
     &           'Proportion of the grid cell area associated with each gravity reservoir', &
     &           'decimal fraction')/=0 ) CALL read_error(1, 'gvr_cell_pct')
          ENDIF
        ENDIF
        IF ( MapOutON_OFF>0 .OR. Model==0 .OR. Model==99 ) THEN
          ALLOCATE ( Gvr_cell_id(Nhrucell) )
          IF ( declparam(MODNAME, 'gvr_cell_id', 'nhrucell', 'integer', &
     &         '-1', '-1', '999999999', &
     &         'Corresponding grid cell id associated with each GVR', &
     &         'Index of the grid cell associated with each gravity reservoir', &
     &         'none')/=0 ) CALL read_error(1, 'gvr_cell_id')
        ENDIF

        Kkiter = 1 ! set for PRMS-only mode

        IF ( Init_vars_from_file==0 ) THEN
          Timestep = 0
        ELSE
          CALL call_modules_restart(1)
        ENDIF
        First_timestep = Timestep

      ELSEIF ( Process(:4)=='init' ) THEN
        Process_flag = 2

        Grid_flag = 0
        IF ( Nhru==Nhrucell ) Grid_flag = 1
        IF ( Model==0 ) THEN
          IF ( Nhru==Nhrucell ) THEN
            Gvr_cell_pct = 1.0
          ELSE
            IF ( getparam(MODNAME, 'gvr_cell_pct', Nhrucell, 'real', &
     &           Gvr_cell_pct)/=0 ) CALL read_error(2, 'gvr_cell_pct')
          ENDIF
          IF ( getparam(MODNAME, 'mxsziter', 1, 'integer', Mxsziter)/=0 ) CALL read_error(2, 'mxsziter')
        ENDIF
        IF ( MapOutON_OFF>0 .OR. Model==0 ) THEN
          IF ( getparam(MODNAME, 'gvr_cell_id', Nhrucell, 'integer', &
     &         Gvr_cell_id)/=0 ) CALL read_error(2, 'gvr_cell_id')
          IF ( Gvr_cell_id(1)==-1 ) THEN
            IF ( Nhru==Nhrucell ) THEN
              IF ( Print_debug>-1 ) THEN
                PRINT *, 'WARNING, HRUs are assumed to be numbered from upper left corner'
                PRINT *, '         gvr_cell_id values are set to 1 through nhru'
              ENDIF
              DO i = 1, Nhrucell
                Gvr_cell_id(i) = i
              ENDDO
            ELSE
              STOP 'ERROR, gvr_cell_id must be specified'
            ENDIF
          ENDIF
        ENDIF
        IF ( Model==0 ) THEN
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
        ENDIF

        nc = numchars(Model_control_file)
        IF ( Print_debug>-1 ) PRINT 9004, 'Using Control File: ', Model_control_file(:nc)
        WRITE ( PRMS_output_unit, 9004 ) 'Using Control File: ', Model_control_file(:nc)
        WRITE ( Logunt, 9004 ) 'Using Control File: ', Model_control_file(:nc)

        nc = numchars(Param_file)
        IF ( Print_debug>-1 ) PRINT 9004, 'Using Parameter File: ', Param_file(:nc)
        WRITE ( PRMS_output_unit, 9004 ) 'Using Parameter File: ', Param_file(:nc)
        WRITE ( Logunt, 9004 ) 'Using Parameter File: ', Param_file(:nc)

        IF ( Init_vars_from_file==1 ) THEN
          nc = numchars(Var_init_file)
          IF ( Print_debug>-1 ) PRINT 9004, 'Using var_init_file: ', Var_init_file(:nc)
          WRITE ( Logunt, 9004 ) 'Writing var_init_file: ', Var_init_file(:nc)
        ENDIF
        IF ( Save_vars_to_file==1 ) THEN
          nc = numchars(Var_save_file)
          IF ( Print_debug>-1 ) PRINT 9004, 'Using var_save_file: ', Var_save_file(:nc)
          WRITE ( Logunt, 9004 ) 'Writing var_save_file: ', Var_save_file(:nc)
        ENDIF

        nc = numchars(Model_output_file)
        IF ( Print_debug>-1 ) PRINT 9004, 'Writing PRMS Water Budget File: ', Model_output_file(:nc)
        WRITE ( Logunt, 9004 ) 'Writing PRMS Water Budget File: ', Model_output_file(:nc)

      ELSEIF ( Process(:7)=='setdims' ) THEN
        Process_flag = 4

      ELSE  !IF ( Process(:5)=='clean' ) THEN
        Process_flag = 3
        IF ( Init_vars_from_file==1 ) CLOSE ( Restart_inunit )
        IF ( Save_vars_to_file==1 ) THEN
          nc = numchars(Var_save_file)
          CALL PRMS_open_output_file(Restart_outunit, Var_save_file(:nc), 'var_save_file', 1, iret)
          IF ( iret/=0 ) STOP
          CALL call_modules_restart(0)
        ENDIF
        IF ( Model==0 ) THEN
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
        ENDIF
      ENDIF

      IF ( Model==99 ) THEN
        IF ( Process_flag==4 .OR. Process_flag<2 ) THEN
          Init_vars_from_file = 0 ! make sure this is set so all variables and parameters are declared
          CALL module_doc()
          call_modules = 0
          RETURN
        ELSE
          STOP
        ENDIF
      ENDIF

! All modules must be called for setdims, declare, initialize, and cleanup
      IF ( Process_flag/=0 ) THEN
        call_modules = basin()
        IF ( call_modules/=0 ) CALL module_error('basin', Arg, call_modules)

        IF ( Call_cascade==1 ) THEN
          call_modules = cascade()
          IF ( call_modules/=0 ) CALL module_error('cascade', Arg, call_modules)
        ENDIF

        call_modules = climateflow()
        IF ( call_modules/=0 ) CALL module_error('climateflow', Arg, call_modules)

        call_modules = soltab()
        IF ( call_modules/=0 ) CALL module_error('soltab', Arg, call_modules)
      ENDIF

      call_modules = prms_time()
      IF ( call_modules/=0 ) CALL module_error('prms_time', Arg, call_modules)

      call_modules = obs()
      IF ( call_modules/=0 ) CALL module_error('obs', Arg, call_modules)

      IF ( Climate_hru_flag==1 ) THEN
        call_modules = climate_hru()
        IF ( call_modules/=0 ) CALL module_error('climate_hru', Arg, call_modules)
      ENDIF

      IF ( Climate_temp_flag==0 ) THEN
        IF ( Temp_combined_flag==1 ) THEN
          call_modules = temp_1sta_laps()
        ELSEIF ( Temp_flag==6 ) THEN
          call_modules = xyz_dist()
        ELSEIF ( Temp_flag==3 ) THEN
          call_modules = temp_dist2()
        ELSE !IF ( Temp_flag==5 ) THEN
          call_modules = ide_dist()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Temp_module, Arg, call_modules)
      ENDIF

      IF ( Climate_precip_flag==0 ) THEN
        IF ( Precip_combined_flag==1 ) THEN
          call_modules = precip_1sta_laps()
        ELSEIF ( Precip_flag==3 ) THEN
          call_modules = precip_dist2()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Precip_module, Arg, call_modules)
      ENDIF

! frost_date is a pre-process module
      IF ( Model==9 ) THEN
        call_modules = frost_date()
        IF ( call_modules/=0 ) CALL module_error('frost_date', Arg, call_modules)
        IF ( Process_flag==0 ) RETURN
        IF ( Process_flag==3 ) STOP
      ENDIF

      IF ( Climate_swrad_flag==0 ) THEN
        IF ( Solrad_flag==1 ) THEN
          call_modules = ddsolrad()
        ELSE !IF ( Solrad_flag==2 ) THEN
          call_modules = ccsolrad()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Solrad_module, Arg, call_modules)
      ENDIF

      IF ( Transp_flag==1 ) THEN
        call_modules = transp_tindex()
      ELSEIF ( Transp_flag==2 ) THEN
        call_modules = transp_frost()
      ENDIF
      IF ( call_modules/=0 ) CALL module_error(Transp_module, Arg, call_modules)

      IF ( Climate_potet_flag==0 ) THEN
        IF ( Et_flag==1 ) THEN
          call_modules = potet_jh()
        ELSEIF ( Et_flag==2 ) THEN
          call_modules = potet_hamon()
        ELSEIF ( Et_flag==4 ) THEN
          call_modules = potet_pan()
        ELSEIF ( Et_flag==5 ) THEN
          call_modules = potet_pt()
        ELSEIF ( Et_flag==6 ) THEN
          call_modules = potet_pm_sta()
        ELSEIF ( Et_flag==11 ) THEN
          call_modules = potet_pm()
        ELSE !IF ( Et_flag==10 ) THEN
          call_modules = potet_hs()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Et_module, Arg, call_modules)
      ENDIF

      IF ( Model==4 ) THEN
        call_modules = write_climate_hru()
        IF ( call_modules/=0 ) CALL module_error('write_climate_hru', Arg, call_modules)
        IF ( Process_flag==0 ) RETURN
      ENDIF

      call_modules = intcp()
      IF ( call_modules/=0 ) CALL module_error('intcp', Arg, call_modules)

      call_modules = snowcomp()
      IF ( call_modules/=0 ) CALL module_error('snowcomp', Arg, call_modules)

      call_modules = srunoff()
      IF ( call_modules/=0 ) CALL module_error(Srunoff_module, Arg, call_modules)

! for PRMS-only simulations
      IF ( PRMS_flag==1 ) THEN
        call_modules = soilzone()
        IF ( call_modules/=0 ) CALL module_error(Soilzone_module, Arg, call_modules)

        call_modules = gwflow()
        IF ( call_modules/=0 ) CALL module_error('gwflow', Arg, call_modules)

        IF ( Stream_order_flag==1 ) THEN
          call_modules = routing()
          IF ( call_modules/=0 ) CALL module_error('routing', Arg, call_modules)
        ENDIF

        IF ( Strmflow_flag==1 ) THEN
          call_modules = strmflow()
        ELSEIF ( Strmflow_flag==4 ) THEN
          call_modules = muskingum()
        ELSE !IF ( Strmflow_flag==5 ) THEN
          call_modules = strmflow_in_out()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Strmflow_module, Arg, call_modules)

        call_modules = basin_sum()
        IF ( call_modules/=0 ) CALL module_error('basin_sum', Arg, call_modules)

        IF ( Print_debug==1 ) CALL water_balance()

! for GSFLOW simulations
      ELSEIF ( Model==0 ) THEN

        IF ( Process_flag==0 ) THEN
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)

! The following modules are in the MODFLOW iteration loop
! (contained in gsflow_modflow.f).
! They still need to be called for declare, initialize and cleanup
        ELSE !IF ( Process_flag/=0 ) THEN

! SOILZONE for GSFLOW is in the MODFLOW iteration loop,
! only call for declare, initialize, and cleanup.
          call_modules = soilzone()
          IF ( call_modules/=0 ) CALL module_error(Soilzone_module, Arg, call_modules)

          call_modules = gsflow_prms2mf()
          IF ( call_modules/=0 ) CALL module_error('gsflow_prms2mf', Arg, call_modules)

          call_modules = gsflow_mf2prms()
          IF ( call_modules/=0 ) CALL module_error('gsflow_mf2prms', Arg, call_modules)
        ENDIF

        call_modules = gsflow_budget()
        IF ( call_modules/=0 ) CALL module_error('gsflow_budget', Arg, call_modules)

        call_modules = gsflow_sum()
        IF ( call_modules/=0 ) CALL module_error('gsflow_sum', Arg, call_modules)
      ENDIF

      IF ( MapOutON_OFF>0 ) THEN
        call_modules = map_results()
        IF ( call_modules/=0 ) CALL module_error('map_results', Arg, call_modules)
      ENDIF

      IF ( NhruOutON_OFF>0 ) CALL nhru_summary()

      IF ( NsubOutON_OFF==1 ) CALL nsub_summary()

      IF ( BasinOutON_OFF==1 ) CALL basin_summary()

      IF ( NsegmentOutON_OFF==1 ) CALL nsegment_summary()

      IF ( Subbasin_flag==1 ) THEN
        call_modules = subbasin()
        IF ( call_modules/=0 ) CALL module_error('subbasin', Arg, call_modules)
      ENDIF

      IF ( Process_flag==3 ) THEN
        IF ( Model==1 ) THEN
          CALL DATE_AND_TIME(VALUES=Elapsed_time_end)
          PRINT 9001
          PRINT 9003, 'start', (Elapsed_time_start(i),i=1,3), (Elapsed_time_start(i),i=5,7)
          PRINT 9003, 'end  ', (Elapsed_time_end(i),i=1,3), (Elapsed_time_end(i),i=5,7)
          Execution_time_end = Elapsed_time_end(5)*3600 + Elapsed_time_end(6)*60 + &
     &                         Elapsed_time_end(7) + Elapsed_time_end(8)*0.001
          Elapsed_time = Execution_time_end - Execution_time_start
          Elapsed_time_minutes = INT(Elapsed_time/60.0)
          PRINT '(A,I5,A,F6.2,A,/)', 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                               Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
          WRITE ( PRMS_output_unit,'(A,I5,A,F6.2,A,/)') 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                                                  Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
          WRITE ( Logunt,'(A,I5,A,F6.2,A,/)') 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                                        Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
        ENDIF
        CLOSE ( Logunt )
      ELSEIF ( Process_flag==1 ) THEN
        IF ( Model==1 ) THEN
          PRINT '(A)', EQULS
          WRITE ( PRMS_output_unit, '(A)' ) EQULS
          WRITE ( Logunt, '(A)') EQULS
        ENDIF
      ELSEIF ( Process_flag==2 ) THEN
        IF ( Parameter_check_flag>0 ) CALL check_nhru_params()
        IF ( Inputerror_flag==1 ) THEN
          PRINT '(//,A,//,A,/,A,/,A)', '**Fix input errors in your Parameter File to continue**', &
     &          '  Set control parameter parameter_check_flag to 0 after', &
     &          '  all parameter values are valid.'
          PRINT '(/,A,/,A,/,A,/,A,/,A,/)', &
     &          'If input errors are related to paramters used for automated', &
     &          'calibration processes, with CAUTION, set control parameter', &
     &          'parameter_check_flag to 0. After calibration set the', &
     &          'parameter_check_flag to 1 to verify that those calibration', &
     &          'parameters have valid and compatible values.'
          STOP
        ENDIF
        IF ( Parameter_check_flag==2 ) STOP
        PRINT 4, 'Simulation time period:', Start_year, Start_month, Start_day, ' -', End_year, End_month, End_day, EQULS
        WRITE ( Logunt, 4 ) 'Simulation time period:', Start_year, Start_month, Start_day, ' -', End_year, End_month, End_day, EQULS
      ENDIF
    4 FORMAT (/, 2(A, I5, 2('/',I2.2)), //, A, /)
 9001 FORMAT (/, 26X, 27('='), /, 26X, 'Normal completion of GSFLOW', /, 26X, 27('='), /)
 9002 FORMAT (//, 74('='), /, 'Please give careful consideration to fixing all ERROR and WARNING messages', /, 74('='))
 9003 FORMAT ('Execution ', A, ' date and time (yyyy/mm/dd hh:mm:ss)', I5, 2('/',I2.2), I3, 2(':',I2.2), /)
 9004 FORMAT (/, 2A)

      END FUNCTION call_modules

!***********************************************************************
!     declare the dimensions
!***********************************************************************
      INTEGER FUNCTION setdims()
      USE PRMS_MODULE
      USE GLOBAL, ONLY: NSTP, NPER
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix, call_modules, control_integer_array, control_file_name
      INTEGER, EXTERNAL :: control_string, control_integer, gsflow_modflow
      EXTERNAL :: read_error, PRMS_open_output_file, PRMS_open_input_file, check_module_names, module_error
! Local Variables
      ! Maximum values are no longer limits
! Local Variables
      INTEGER :: idim, iret, j
      INTEGER :: test, mf_timestep
!***********************************************************************
      setdims = 1

      Inputerror_flag = 0

      CALL PRMS_open_output_file(Logunt, 'gsflow.log', 'gsflow.log', 0, iret)
      IF ( iret/=0 ) STOP

      PRINT 3
      WRITE ( Logunt, 3 )
    3 FORMAT (//, 26X, 'U.S. Geological Survey', /, 8X, &
     &        'Coupled Groundwater and Surface-water FLOW model (GSFLOW)', /, &
     &        25X, 'Version 1.2.2 02/23/2018', //, &
     &        '    An integration of the Precipitation-Runoff Modeling System (PRMS)', /, &
     &        '    and the Modular Groundwater Model (MODFLOW-NWT and MODFLOW-2005)', /)

      ! debug print flag:
      ! -1=quiet - reduced screen output
      ! 0=none; 1=water balances; 2=basin;
      ! 4=basin_sum; 5=soltab; 7=soil zone;
      ! 9=snowcomp; 13=cascade; 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug')/=0 ) Print_debug = 0

      IF ( control_integer(Parameter_check_flag, 'parameter_check_flag')/=0 ) Parameter_check_flag = 1

      IF ( control_string(Model_mode, 'model_mode')/=0 ) CALL read_error(5, 'model_mode')
      PRMS_flag = 1
!     Model (0=GSFLOW; 1=PRMS; 2=MODFLOW)
      IF ( Model_mode(:6)=='GSFLOW' .OR. Model_mode(:4)=='    ') THEN
        Model = 0
        PRMS_flag = 0
      ELSEIF ( Model_mode(:4)=='PRMS' .OR. Model_mode(:5)=='DAILY' )THEN
        Model = 1
      ELSEIF ( Model_mode(:7)=='MODFLOW' ) THEN
        Model = 2
        PRMS_flag = 0
      ELSEIF ( Model_mode(:5)=='FROST' ) THEN
        Model = 9
      ELSEIF ( Model_mode(:13)=='WRITE_CLIMATE' ) THEN
        Model = 4
      ELSEIF ( Model_mode(:13)=='DOCUMENTATION' ) THEN
        Model = 99
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid model_mode value: ', Model_mode
        STOP
      ENDIF

      ! get simulation start_time and end_time
      Starttime = -1
      DO j = 1, 6
        IF ( control_integer_array(Starttime(j), j, 'start_time')/=0 ) THEN
          PRINT *, 'ERROR, start_time, index:', j, 'value: ', Starttime(j)
          STOP
        ENDIF
      ENDDO
      Start_year = Starttime(1)
      IF ( Start_year<0 ) STOP 'ERROR, control parameter start_time must be specified'
      Start_month = Starttime(2)
      Start_day = Starttime(3)
      Endtime = -1
      DO j = 1, 6
        IF ( control_integer_array(Endtime(j), j, 'end_time')/=0 ) THEN
          PRINT *, 'ERROR, end_time, index:', j, 'value: ', Endtime(j)
          STOP
        ENDIF
      ENDDO
      End_year = Endtime(1)
      IF ( End_year<0 ) STOP 'ERROR, control parameter start_time must be specified'
      End_month = Endtime(2)
      End_day = Endtime(3)

      IF ( control_integer(Init_vars_from_file, 'init_vars_from_file')/=0 ) Init_vars_from_file = 0
      IF ( control_integer(Save_vars_to_file, 'save_vars_to_file')/=0 ) Save_vars_to_file = 0

      IF ( Model==2 ) THEN
! for MODFLOW-only simulations
        Kper_mfo = 1
        mf_timestep = 1
        Process_flag = 1
        test = gsflow_modflow()
        IF ( test/=0 ) CALL module_error(MODNAME, 'declare', test)
        Process_flag = 2
        test = gsflow_modflow()
        IF ( test/=0 ) CALL module_error(MODNAME, 'initialize', test)
        PRINT *, ' '
        WRITE (Logunt, '(1X)')
        Process_flag = 0
        DO WHILE ( Kper_mfo<=Nper )
          test = gsflow_modflow()
          IF ( test/=0 ) CALL module_error(MODNAME, 'run', test)
          IF ( mf_timestep==NSTP(Kper_mfo) ) THEN
              Kper_mfo = Kper_mfo + 1
              mf_timestep = 0
          END IF
          mf_timestep = mf_timestep + 1
        ENDDO
        Process_flag = 3
        test = gsflow_modflow()
        IF ( test/=0 ) CALL module_error(MODNAME, 'clean', test)
        STOP
      ENDIF

      ! Open PRMS module output file
      IF ( control_string(Model_output_file, 'model_output_file')/=0 ) CALL read_error(5, 'model_output_file')
      CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
      IF ( iret/=0 ) STOP
      IF ( control_file_name(Model_control_file)/=0 ) CALL read_error(5, 'control_file_name')
      IF ( control_string(Param_file, 'param_file')/=0 ) CALL read_error(5, 'param_file')

      ! Check for restart files
      IF ( Init_vars_from_file==1 ) THEN
        IF ( control_string(Var_init_file, 'var_init_file')/=0 ) CALL read_error(5, 'var_init_file')
        CALL PRMS_open_input_file(Restart_inunit, Var_init_file, 'var_init_file', 1, iret)
        IF ( iret/=0 ) STOP
      ENDIF
      IF ( Save_vars_to_file==1 ) THEN
        IF ( control_string(Var_save_file, 'var_save_file')/=0 ) CALL read_error(5, 'var_save_file')
      ENDIF

      Temp_module = ' '
      IF ( control_string(Temp_module, 'temp_module')/=0 ) CALL read_error(5, 'temp_module')
      Precip_module = ' '
      IF ( control_string(Precip_module, 'precip_module')/=0 ) CALL read_error(5, 'precip_module')
      Transp_module = ' '
      IF ( control_string(Transp_module, 'transp_module')/=0 ) CALL read_error(5, 'transp_module')
      Et_module = ' '
      IF ( control_string(Et_module, 'et_module')/=0 ) CALL read_error(5, 'et_module')
      Srunoff_module = ' '
      IF ( control_string(Srunoff_module, 'srunoff_module')/=0 ) CALL read_error(5, 'srunoff_module')
      Solrad_module = ' '
      IF ( control_string(Solrad_module, 'solrad_module')/=0 ) CALL read_error(5, 'solrad_module')
      Strmflow_module = 'strmflow'
      IF ( control_string(Strmflow_module, 'strmflow_module')/=0 ) CALL read_error(5, 'strmflow_module')

      IF ( Parameter_check_flag>0 ) CALL check_module_names()

      Climate_precip_flag = 0
      Climate_temp_flag = 0
      Climate_transp_flag = 0
      Climate_potet_flag = 0
      Climate_swrad_flag = 0

      IF ( Precip_module(:11)=='precip_1sta' .OR. Precip_module(:11)=='precip_prms') THEN
        Precip_flag = 1
      ELSEIF ( Precip_module(:11)=='precip_laps' ) THEN
        Precip_flag = 2
      ELSEIF ( Precip_module(:12)=='precip_dist2' ) THEN
        Precip_flag = 3
      ELSEIF ( Precip_module(:8)=='ide_dist' ) THEN
        Precip_flag = 5
      ELSEIF ( Precip_module(:11)=='climate_hru' ) THEN
        Precip_flag = 7
        Climate_precip_flag = 1
      ELSEIF ( Precip_module(:8)=='xyz_dist' ) THEN
        Precip_flag = 6
      ELSE
        PRINT '(/,2A)', 'ERROR: invalid precip_module value: ', Precip_module
        Inputerror_flag = 1
      ENDIF
      Precip_combined_flag = 0
      IF ( Precip_flag==1 .OR. Precip_flag==2 ) Precip_combined_flag = 1

      IF ( Temp_module(:9)=='temp_1sta' ) THEN
        Temp_flag = 1
      ELSEIF ( Temp_module(:9)=='temp_laps' ) THEN
        Temp_flag = 2
      ELSEIF ( Temp_module(:10)=='temp_dist2' ) THEN
        Temp_flag = 3
      ELSEIF ( Temp_module(:8)=='ide_dist' ) THEN
        Temp_flag = 5
      ELSEIF ( Temp_module(:11)=='climate_hru' ) THEN
        Temp_flag = 7
        Climate_temp_flag = 1
      ELSEIF ( Temp_module(:8)=='xyz_dist' ) THEN
        Temp_flag = 6
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid temp_module value: ', Temp_module
        Inputerror_flag = 1
      ENDIF
      Temp_combined_flag = 0
      IF ( Temp_flag==1 .OR. Temp_flag==2 ) Temp_combined_flag = 1

      IF ( Transp_module(:13)=='transp_tindex' ) THEN
        Transp_flag = 1
      ELSEIF ( Transp_module(:12)=='transp_frost' ) THEN
        Transp_flag = 2
      ELSEIF ( Transp_module(:11)=='climate_hru' ) THEN
        Transp_flag = 3
        Climate_transp_flag = 1
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid transp_module value: ', Transp_module
        Inputerror_flag = 1
      ENDIF

      IF ( Et_module(:8)=='potet_jh' ) THEN
        Et_flag = 1
      ELSEIF ( Et_module(:11)=='potet_hamon' ) THEN
        Et_flag = 2
      ELSEIF ( Et_module(:11)=='climate_hru' ) THEN
        Et_flag = 7
        Climate_potet_flag = 1
      ELSEIF ( Et_module(:8)=='potet_hs' ) THEN
        Et_flag = 10
      ELSEIF ( Et_module(:12)=='potet_pm_sta' ) THEN
        Et_flag = 6
      ELSEIF ( Et_module(:8)=='potet_pm' ) THEN
        Et_flag = 11
      ELSEIF ( Et_module(:8)=='potet_pt' ) THEN
        Et_flag = 5
      ELSEIF ( Et_module(:9)=='potet_pan' ) THEN
        Et_flag = 4
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid et_module value: ', Et_module
        Inputerror_flag = 1
      ENDIF
      Humidity_cbh_flag = 0
      Windspeed_cbh_flag = 0
      IF ( Et_flag==11 .OR. Et_flag==5 ) Humidity_cbh_flag = 1
      IF ( Et_flag==11 ) Windspeed_cbh_flag = 1

      IF ( Srunoff_module(:13)=='srunoff_smidx' ) THEN
        Sroff_flag = 1
      ELSEIF ( Srunoff_module(:13)=='srunoff_carea' ) THEN
        Sroff_flag = 2
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid srunoff_module value: ', Srunoff_module
        Inputerror_flag = 1
      ENDIF

      Soilzone_module = 'soilzone'

      IF ( control_integer(Orad_flag, 'orad_flag')/=0 ) Orad_flag = 0
      IF ( Solrad_module(:8)=='ddsolrad' ) THEN
        Solrad_flag = 1
      ELSEIF ( Solrad_module(:11)=='climate_hru' ) THEN
        Solrad_flag = 7
        Climate_swrad_flag = 1
      ELSEIF ( Solrad_module(:8)=='ccsolrad' ) THEN
        Solrad_flag = 2
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid solrad_module value: ', Solrad_module
        Inputerror_flag = 1
      ENDIF

      Climate_hru_flag = 0
      IF ( Climate_temp_flag==1 .OR. Climate_precip_flag==1 .OR. Climate_potet_flag==1 .OR. &
     &     Climate_swrad_flag==1 .OR. Climate_transp_flag==1 .OR. &
     &     Humidity_cbh_flag==1 .OR. Windspeed_cbh_flag==1 ) Climate_hru_flag = 1

      IF ( Strmflow_module(:15)=='strmflow_in_out' ) THEN
        Strmflow_flag = 5
      ELSEIF ( Strmflow_module(:13)=='strmflow_lake' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid strmflow_module value; lakes are not simulated: ', Strmflow_module
        Inputerror_flag = 1
      ELSEIF ( Strmflow_module(:8)=='strmflow' ) THEN
        Strmflow_flag = 1
      ELSEIF ( Strmflow_module(:9)=='muskingum' ) THEN
        Strmflow_flag = 4
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid strmflow_module value: ', Strmflow_module
        Inputerror_flag = 1
      ENDIF

! cascade dimensions
      IF ( decldim('ncascade', 0, MAXDIM, &
     &     'Number of HRU links for cascading flow')/=0 ) CALL read_error(7, 'ncascade')
      IF ( decldim('ncascdgw', 0, MAXDIM, &
     &     'Number of GWR links for cascading flow')/=0 ) CALL read_error(7, 'ncascdgw')

! nsegment dimension
      IF ( decldim('nsegment', 0, MAXDIM, 'Number of stream-channel segments')/=0 ) CALL read_error(7, 'nsegment')

! subbasin dimensions
      IF ( control_integer(Subbasin_flag, 'subbasin_flag')/=0 ) Subbasin_flag = 1
      IF ( decldim('nsub', 0, MAXDIM, 'Number of internal subbasins')/=0 ) CALL read_error(7, 'nsub')

      IF ( control_integer(Dprst_flag, 'dprst_flag')/=0 ) Dprst_flag = 0
      IF ( control_integer(CsvON_OFF, 'csvON_OFF')/=0 ) CsvON_OFF = 0

! map results dimensions
      IF ( control_integer(MapOutON_OFF, 'mapOutON_OFF')/=0 ) MapOutON_OFF = 0
      idim = 0
      IF ( Model==0 .OR. MapOutON_OFF==1 ) idim = 1
      IF ( decldim('nhrucell', idim, MAXDIM, &
     &     'Number of unique intersections between HRUs and spatial units of a target map for mapped results')/=0 ) &
     &     CALL read_error(7, 'nhrucell')
      IF ( decldim('ngwcell', 0, MAXDIM, &
     &     'Number of spatial units in the target map for mapped results')/=0 ) CALL read_error(7, 'ngwcell')
      IF ( decldim('nreach', idim, MAXDIM, 'Number of reaches on all stream segments')/=0 ) CALL read_error(7, 'nreach')

      IF ( control_integer(Gwr_swale_flag, 'gwr_swale_flag')/=0 ) Gwr_swale_flag = 0

! nhru_summary
      IF ( control_integer(NhruOutON_OFF, 'nhruOutON_OFF')/=0 ) NhruOutON_OFF = 0

! nsub_summary
      IF ( control_integer(NsubOutON_OFF, 'nsubOutON_OFF')/=0 ) NsubOutON_OFF = 0

! basin_summary
      IF ( control_integer(BasinOutON_OFF, 'basinOutON_OFF')/=0 ) BasinOutON_OFF = 0
! nsegment_summary
      IF ( control_integer(NsegmentOutON_OFF, 'nsegmentOutON_OFF')/=0 ) NsegmentOutON_OFF = 0

! cascade
      IF ( control_integer(Cascade_flag, 'cascade_flag')/=0 ) Cascade_flag = 1
      ! if cascadegw_flag = 2, use same cascades as HRUs
      IF ( control_integer(Cascadegw_flag, 'cascadegw_flag')/=0 ) Cascadegw_flag = 1

! spatial units
      IF ( decldim('ngw', 1, MAXDIM, 'Number of GWRs')/=0 ) CALL read_error(7, 'ngw')
      IF ( decldim('nhru', 1, MAXDIM, 'Number of HRUs')/=0 ) CALL read_error(7, 'nhru')
      IF ( decldim('nssr', 1, MAXDIM, 'Number of subsurface reservoirs')/=0 ) CALL read_error(7, 'nssr')
      IF ( decldim('nlake', 0, MAXDIM, 'Number of lakes')/=0 ) CALL read_error(7, 'nlake')
      ! nlake_hrus to be added in 5.0.1
!      IF ( decldim('nlake_hrus', 0, MAXDIM, 'Number of lake HRUs')/=0 ) CALL read_error(7, 'nlake_hrus')

! Time-series data stations, need to know if in Data File
      IF ( decldim('nrain', 0, MAXDIM, 'Number of precipitation-measurement stations')/=0 ) CALL read_error(7, 'nrain')
      IF ( decldim('nsol', 0, MAXDIM, 'Number of solar-radiation measurement stations')/=0 ) CALL read_error(7, 'nsol')
      IF ( decldim('ntemp', 0, MAXDIM, 'Number of air-temperature-measurement stations')/=0 ) CALL read_error(7, 'ntemp')
      IF ( decldim('nobs', 0, MAXDIM, 'Number of streamflow-measurement stations')/=0 ) CALL read_error(7, 'nobs')
      IF ( decldim('nevap', 0, MAXDIM, 'Number of pan-evaporation data sets')/=0 ) CALL read_error(7, 'nevap')
!      IF ( decldim('nratetbl', 0, MAXDIM, 'Number of rating-table data sets for lake elevations') &
!     &     /=0 ) CALL read_error(7, 'nratetbl')

! depletion curves
      IF ( decldim('ndepl', 1, MAXDIM, 'Number of snow-depletion curves')/=0 ) CALL read_error(7, 'ndelp')
      IF ( decldim('ndeplval', 11, MAXDIM, 'Number of values in all snow-depletion curves (set to ndepl*11)')/=0 ) &
     &     CALL read_error(7, 'ndelplval')

! fixed dimensions
      IF ( declfix('ndays', 366, 366, 'Maximum number of days in a year ')/=0 ) CALL read_error(7, 'ndays')
      IF ( declfix('nmonths', 12, 12, 'Number of months in a year')/=0 ) CALL read_error(7, 'nmonths')
      IF ( declfix('one', 1, 1, 'Number of values for scaler array')/=0 ) CALL read_error(7, 'one')

      IF ( call_modules('setdims')/=0 ) STOP 'ERROR, in setdims'

      IF ( Inputerror_flag==1 ) THEN
        PRINT '(//,A,/,A)', '**FIX input errors in your Control File to continue**', &
     &        'NOTE: some errors may be due to use of defalut values'
        STOP
      ENDIF

      setdims = 0
      END FUNCTION setdims

!***********************************************************************
!     Get and check consistency of dimensions with flags
!***********************************************************************
      INTEGER FUNCTION check_dims()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getdim
      EXTERNAL :: check_dimens
!***********************************************************************

      Nhru = getdim('nhru')
      IF ( Nhru==-1 ) CALL read_error(7, 'nhru')

      Nssr = getdim('nssr')
      IF ( Nssr==-1 ) CALL read_error(7, 'nssr')

      Ngw = getdim('ngw')
      IF ( Ngw==-1 ) CALL read_error(7, 'ngw')

      Ntemp = getdim('ntemp')
      IF ( Ntemp==-1 ) CALL read_error(6, 'ntemp')

      Nrain = getdim('nrain')
      IF ( Nrain==-1 ) CALL read_error(6, 'nrain')

      Nsol = getdim('nsol')
      IF ( Nsol==-1 ) CALL read_error(6, 'nsol')

      Nobs = getdim('nobs')
      IF ( Nobs==-1 ) CALL read_error(6, 'nobs')

      Nevap = getdim('nevap')
      IF ( Nevap==-1 ) CALL read_error(6, 'nevap')

      Ncascade = getdim('ncascade')
      IF ( Ncascade==-1 ) CALL read_error(7, 'ncascade')
      Ncascdgw = getdim('ncascdgw')
      IF ( Ncascdgw==-1 ) CALL read_error(7, 'ncascdgw')
      IF ( Cascadegw_flag==2 ) Ncascdgw = Ncascade
      IF ( Ncascade==0 ) Cascade_flag = 0
      IF ( Ncascdgw==0 .OR. Model==0 .OR. Model==2 ) Cascadegw_flag = 0
      IF ( Cascade_flag==1 .OR. Cascadegw_flag>0 ) THEN
        Call_cascade = 1
      ELSE
        Call_cascade = 0
      ENDIF

      Nlake = getdim('nlake')
      IF ( Nlake==-1 ) CALL read_error(7, 'nlake')

      ! Nlake_hrus will be added in version 5.0.1
!      Nlake_hrus = getdim('nlake_hrus')
!      IF ( Nlake_hrus==-1 ) CALL read_error(7, 'nlake_hrus')
!      IF ( Nlake>0 .AND. Nlake_hrus==0 ) Nlake_hrus = Nlake
      Nlake_hrus = Nlake

      Ndepl = getdim('ndepl')
      IF ( Ndepl==-1 ) CALL read_error(7, 'ndepl')

      Ndeplval = getdim('ndeplval')
      IF ( Ndeplval==-1 ) CALL read_error(7, 'ndeplval')

      Nsub = getdim('nsub')
      IF ( Nsub==-1 ) CALL read_error(7, 'nsub')
      ! default = 1, turn off if no subbasins
      IF ( Subbasin_flag==1 .AND. Nsub==0 ) Subbasin_flag = 0

      Nsegment = getdim('nsegment')
      IF ( Nsegment==-1 ) CALL read_error(7, 'nsegment')

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell==-1 ) CALL read_error(6, 'nhrucell')

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell==-1 ) CALL read_error(6, 'ngwcell')

!      Nratetbl = getdim('nratetbl')
!      IF ( Nratetbl==-1 ) CALL read_error(6, 'nratetbl')
      Nratetbl = 0

      Stream_order_flag = 0
      IF ( Nsegment>0 .AND. Strmflow_flag>1 .AND. Model/=0 ) THEN
        Stream_order_flag = 1 ! strmflow_in_out or muskingum
      ENDIF

      IF ( Nsegment<1 .AND. Model/=99 ) THEN
        IF ( Stream_order_flag==1 .OR. Call_cascade==1 ) THEN
          PRINT *, 'ERROR, streamflow and cascade routing require nsegment > 0, specified as:', Nsegment
          STOP
        ENDIF
      ENDIF

      Lake_route_flag = 0
      IF ( Nlake>0 .AND. Strmflow_flag==3 .AND. Model/=0 ) Lake_route_flag = 1 ! muskingum_lake

      IF ( NsubOutON_OFF==1 .AND. Nsub==0 ) THEN
        NsubOutON_OFF = 0
        PRINT *, 'nsubOutON_OFF = 1 and nsub = 0, thus nsub_summary not used'
      ENDIF

      IF ( Model==99 .OR. Parameter_check_flag>0 ) CALL check_dimens()

      check_dims = Inputerror_flag
      END FUNCTION check_dims

!***********************************************************************
!     Check consistency of dimensions with flags
!***********************************************************************
      SUBROUTINE check_dimens()
      USE PRMS_MODULE
      IMPLICIT NONE
! Local Variables
      INTEGER :: ierr
!***********************************************************************
      ierr = 0
      IF ( Nhru==0 .OR. Nssr==0 .OR. Ngw==0 ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must be > 0: nhru=', Nhru, ', nssr=', Nssr, ', ngw=', Ngw
        ierr = 1
      ELSEIF ( Nssr/=Nhru .OR. Ngw/=Nhru ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must equal: nhru=', Nhru, ', nssr=', Nssr, ', ngw=', Ngw
        ierr = 1
      ENDIF
      IF ( Ndepl==0 ) THEN
        PRINT *, 'ERROR, ndepl must be > 0: ndepl=', Ndepl
        ierr = 1
      ENDIF
      IF ( Ndeplval/=Ndepl*11 ) THEN
        PRINT *, 'ERROR, ndeplval must be = ndepl*11: ndeplval:', Ndeplval, ', ndepl=', Ndepl
        ierr = 1
      ENDIF

      IF ( ierr==1 ) STOP

      IF ( Model==99 ) THEN
        IF ( Ntemp==0 ) Ntemp = 1
        IF ( Nrain==0 ) Nrain = 1
        IF ( Nlake==0 ) Nlake = 1
        IF ( Nlake_hrus==0 ) Nlake_hrus = 1
        IF ( Nsol==0 ) Nsol = 1
        IF ( Nobs==0 ) Nobs = 1
        IF ( Ncascade==0 ) Ncascade = 1
        IF ( Ncascdgw==0 ) Ncascdgw = 1
        IF ( Nsub==0 ) Nsub = 1
        IF ( Nevap==0 ) Nevap = 1
        IF ( Nhrucell==0 ) Nhrucell = 1
        IF ( Ngwcell==0 ) Ngwcell = 1
        IF ( Nsegment==0 ) Nsegment = 1
!        IF ( Nratetbl==0 ) Nratetbl = 4
        Subbasin_flag = 1
        Cascade_flag = 1
        Cascadegw_flag = 1
        Call_cascade = 1
        Stream_order_flag = 1
        Climate_hru_flag = 1
        Lake_route_flag = 1
      ENDIF

      END SUBROUTINE check_dimens

!**********************************************************************
!     Module documentation
!**********************************************************************
      SUBROUTINE module_doc()
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basin, climateflow, prms_time
      INTEGER, EXTERNAL :: cascade, obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: transp_frost, frost_date, routing
      INTEGER, EXTERNAL :: temp_1sta_laps, temp_dist2
      INTEGER, EXTERNAL :: precip_1sta_laps, climate_hru
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon, potet_hs, potet_pt, potet_pm
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow, srunoff, soilzone
      INTEGER, EXTERNAL :: strmflow, subbasin, basin_sum, map_results, strmflow_in_out
      INTEGER, EXTERNAL :: write_climate_hru, muskingum, potet_pm_sta
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms, gsflow_budget, gsflow_sum
      EXTERNAL :: nhru_summary, water_balance, nsub_summary, basin_summary, nsegment_summary
! Local variable
      INTEGER :: test
!**********************************************************************
      test = basin()
      test = cascade()
      test = climateflow()
      test = soltab()
      test = prms_time()
      test = obs()
      test = temp_1sta_laps()
      test = temp_dist2()
      test = xyz_dist()
      test = ide_dist()
      test = climate_hru()
      test = precip_1sta_laps()
      test = precip_dist2()
      test = ddsolrad()
      test = ccsolrad()
      test = transp_tindex()
      test = frost_date()
      test = transp_frost()
      test = potet_jh()
      test = potet_hamon()
      test = potet_pan()
      test = potet_hs()
      test = potet_pt()
      test = potet_pm()
      test = potet_pm_sta()
      test = write_climate_hru()
      test = intcp()
      test = snowcomp()
      test = srunoff()
      test = soilzone()
      test = gsflow_prms2mf()
      test = gsflow_mf2prms()
      test = gsflow_budget()
      test = gsflow_sum()
      test = gwflow()
      test = routing()
      test = strmflow()
      test = strmflow_in_out()
      test = muskingum()
      test = basin_sum()
      test = map_results()
      CALL nhru_summary()
      CALL nsub_summary()
      CALL basin_summary()
      CALL nsegment_summary()
      CALL water_balance()
      test = subbasin()

      PRINT 9001
 9001 FORMAT (//, ' All available modules have been called.', /, &
     &        ' All parameters have been declared.', /, &
     &        ' Note, no simulation was computed.', /)

      END SUBROUTINE module_doc

!***********************************************************************
!     check module names
!***********************************************************************
      SUBROUTINE check_module_names()
      USE PRMS_MODULE, ONLY: Temp_module, Precip_module, Et_module, Solrad_module, &
     &    Transp_module, Srunoff_module, Strmflow_module
      IMPLICIT NONE
! Local Variables
      INTEGER :: ierr
!***********************************************************************
      ierr = 0
      IF ( Temp_module(:14)=='temp_1sta_prms' ) THEN
        PRINT *, 'WARNING, deprecated temp_module value, change temp_1sta_prms to temp_1sta'
        Temp_module = 'temp_1sta'
      ELSEIF ( Temp_module(:14)=='temp_laps_prms' ) THEN
        PRINT *, 'WARNING, deprecated temp_module value, change temp_laps_prms to temp_laps'
        Temp_module = 'temp_laps'
      ELSEIF ( Temp_module(:15)=='temp_dist2_prms' ) THEN
        PRINT *, 'WARNING, deprecated temp_module value, change temp_dist2_prms to temp_dist2'
        Temp_module = 'temp_dist2'
      ELSEIF ( Temp_module(:9)=='temp_2sta' ) THEN
        PRINT *, 'ERROR, module temp_2sta_prms not available, use a different temp_module'
        ierr = 1
      ENDIF

      IF ( Precip_module(:11)=='precip_prms' ) THEN
        PRINT *, 'WARNING, deprecated precip_module value, change precip_prms to precip_1sta'
        Precip_module = 'precip_1sta'
      ELSEIF ( Precip_module(:16)=='precip_laps_prms' ) THEN
        PRINT *, 'WARNING, deprecated precip_module value, change precip_laps_prms to precip_laps'
        Precip_module = 'precip_laps'
      ELSEIF ( Precip_module(:17)=='precip_dist2_prms' ) THEN
        PRINT *, 'WARNING, deprecated precip_module value, change precip_dist2_prms to precip_dist2'
        Precip_module = 'precip_dist2'
      ENDIF

      IF ( Temp_module(:8)=='ide_dist' .AND. Precip_module(:8)/='ide_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if ide_dist is specified for temp_module,', &
     &        'it also must be specified for precip_module: ', Precip_module
        ierr = 1
      ELSEIF ( Precip_module(:8)=='ide_dist' .AND. Temp_module(:8)/='ide_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if ide_dist is specified for precip_module,', &
     &        'it also must be specified for temp_module: ', Temp_module
        ierr = 1
      ELSEIF ( Temp_module(:8)=='xyz_dist' .AND. Precip_module(:8)/='xyz_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if xyz_dist is specified for temp_module,', &
     &        'it also must be specified for precip_module: ', Precip_module
        ierr = 1
      ELSEIF ( Precip_module(:8)=='xyz_dist' .AND. Temp_module(:8)/='xyz_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if xyz_dist is specified for precip_module,', &
     &        'it also must be specified for temp_module: ', Temp_module
        ierr = 1
      ENDIF

      IF ( Transp_module(:18)=='transp_tindex_prms' ) THEN
        PRINT *, 'WARNING, deprecated transp_module value, change transp_tindex_prms to transp_tindex'
        Transp_module = 'transp_tindex'
      ENDIF

      IF ( Et_module(:13)=='potet_jh_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_jh_prms to potet_jh'
        Et_module = 'potet_jh'
      ELSEIF ( Et_module(:14)=='potet_pan_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_pan_prms to potet_pan'
        Et_module = 'potet_pan'
      ELSEIF ( Et_module(:15)=='potet_epan_prms' ) THEN
        PRINT *, 'ERROR, deprecated et_module value, change potet_epan_prms to potet_pan'
        ierr = 1
      ELSEIF ( Et_module(:20)=='potet_hamon_hru_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_hamon_hru_prms to potet_hamon_hru'
        Et_module = 'potet_hamon'
      ELSEIF ( Et_module(:16)=='potet_hamon_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_hamon_prms to potet_hamon'
        Et_module = 'potet_hamon'
      ENDIF

      IF ( Solrad_module(:17)=='ddsolrad_hru_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ddsolrad_hru_prms to ddsolrad'
        solrad_module = 'ddsolrad'
      ELSEIF ( Solrad_module(:17)=='ccsolrad_hru_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ccsolrad_hru_prms to ccsolrad'
        solrad_module = 'ccsolrad'
      ELSEIF ( Solrad_module(:13)=='ddsolrad_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ddsolrad_prms to ddsolrad'
        solrad_module = 'ddsolrad'
      ELSEIF ( Solrad_module(:13)=='ccsolrad_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ccsolrad_prms to ccsolrad'
        solrad_module = 'ccsolrad'
      ENDIF

      IF ( Srunoff_module(:18)=='srunoff_carea_prms' ) THEN
        PRINT *, 'WARNING, deprecated srunoff_module value, change srunoff_carea_prms to srunoff_carea'
        srunoff_module = 'srunoff_carea'
      ELSEIF ( Srunoff_module(:18)=='srunoff_smidx_prms' ) THEN
        PRINT *, 'WARNING, deprecated srunoff_module value, change srunoff_smidx_prms to srunoff_smidx'
        srunoff_module = 'srunoff_smidx'
      ENDIF

      IF ( Strmflow_module(:13)=='strmflow_prms' ) THEN
        PRINT *, 'WARNING, deprecated strmflow_module value, change strmflow_prms to strmflow'
        strmflow_module = 'strmflow'
      ELSEIF ( Strmflow_module(:13)=='strmflow_lake' ) THEN
        PRINT *, 'ERROR, module strmflow_lake not available, use a different strmflow_module'
        ierr = 1
      ENDIF
      IF ( ierr==1 ) STOP
      END SUBROUTINE check_module_names

!***********************************************************************
!     call_modules_restart - write or read restart file
!***********************************************************************
      SUBROUTINE call_modules_restart(In_out)
      USE PRMS_MODULE
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart, check_restart_dimen
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: nhru_test, dprst_test, nsegment_test, temp_test, et_test, ierr
      INTEGER :: cascade_test, cascdgw_test, nhrucell_test, nlake_test
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: model_test
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Timestep, Nhru, Dprst_flag, Nsegment, Temp_flag, Et_flag, &
     &          Cascade_flag, Cascadegw_flag, Nhrucell, Nlake, Model_mode
      ELSE
        ierr = 0
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Timestep, nhru_test, dprst_test, nsegment_test, temp_test, et_test, &
     &         cascade_test, cascdgw_test, nhrucell_test, nlake_test, model_test
        IF ( TRIM(Model_mode)/=TRIM(model_test) ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model_mode=', model_test
          PRINT *, '       Current model has model_mode=', Model_mode, ' they must be equal'
          ierr = 1
        ENDIF
        CALL check_restart_dimen('nhru', nhru_test, Nhru, ierr)
        CALL check_restart_dimen('nhrucell', nhrucell_test, Nhrucell, ierr)
        CALL check_restart_dimen('nlake', nlake_test, Nlake, ierr)
        IF ( Dprst_flag/=dprst_test ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model with dprst_flag=', dprst_test
          PRINT *, '       Current model has dprst_flag=', Dprst_flag, ' they must be equal'
          ierr = 1
        ENDIF
        IF ( Cascade_flag/=cascade_test ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model with cascade_flag=', cascade_test
          PRINT *, '       Current model has cascade_flag=', Cascade_flag, ' they must be equal'
          ierr = 1
        ENDIF
        IF ( Cascadegw_flag/=cascdgw_test ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model with cascadegw_flag=', cascdgw_test
          PRINT *, '       Current model has cascadegw_flag=', Cascadegw_flag, ' they must be equal'
          ierr = 1
        ENDIF
        CALL check_restart_dimen('nsegment', nsegment_test, Nsegment, ierr)
        IF ( Temp_flag/=temp_test ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model with different temperature'
          PRINT *, '       module than current model, they must use the same module'
          ierr = 1
        ENDIF
        IF ( Et_flag/=et_test ) THEN
          IF ( Et_flag==4 ) THEN
            PRINT *, 'ERROR, Initial Conditions File saved for model using potet_pan module'
            PRINT *, '       current model also must use potet_pan'
            ierr = 1
          ELSEIF ( Et_flag==6 ) THEN
            PRINT *, 'ERROR, Initial Conditions File saved for model using potet_pm_sta module'
            PRINT *, '       current model also must use potet_pm_sta'
            ierr = 1
          ELSEIF ( Et_flag==5 ) THEN
            PRINT *, 'ERROR, Initial Conditions File saved for model using potet_pt module'
            PRINT *, '       current model also must use potet_pt'
            ierr = 1
          ELSEIF ( Et_flag==11 ) THEN
            PRINT *, 'ERROR, Initial Conditions File saved for model using potet_pm module'
            PRINT *, '       current model also must use potet_pm'
            ierr = 1
          ENDIF
        ENDIF
        IF ( ierr==1 ) STOP
      ENDIF
      END SUBROUTINE call_modules_restart

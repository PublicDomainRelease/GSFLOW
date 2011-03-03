!***********************************************************************
!     GSFLOW module that replaces the PRMS module call_modules.f
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      CHARACTER(LEN=32), SAVE :: Precip_module, Temp_module, Et_module
      CHARACTER(LEN=32), SAVE :: Srunoff_module, Solrad_module
      CHARACTER(LEN=68), SAVE :: Versn_gsfprms
      INTEGER, SAVE :: Kper_mfo, Kkstp_mfo
      INTEGER, SAVE :: Model, Cascade, Process_flag
      INTEGER, SAVE :: Nsub, Ncascade, Ncascdgw, Nsegment
      INTEGER, SAVE :: Sroff_flag, Solrad_flag, Et_flag
      INTEGER, SAVE :: Temp_flag, Precip_flag, grid_reportON_OFF
!   Control Parameters
      CHARACTER(LEN=16), SAVE :: Model_mode
      END MODULE PRMS_MODULE

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION call_modules(Arg)
      USE PRMS_MODULE
      USE GLOBAL, ONLY:NSTP, NPER
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, getdim, control_string
      INTEGER, EXTERNAL :: control_integer
      DOUBLE PRECISION, EXTERNAL :: deltim
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions (define in calling order)
      INTEGER, EXTERNAL :: basin_prms
      INTEGER, EXTERNAL :: climate_vars_prms
      INTEGER, EXTERNAL :: flow_vars_prms
      INTEGER, EXTERNAL :: cascade_prms
      INTEGER, EXTERNAL :: obs_prms
      INTEGER, EXTERNAL :: obs_adjust_prms
      INTEGER, EXTERNAL :: soltab_hru_prms
      INTEGER, EXTERNAL :: transp_tindex_prms
      INTEGER, EXTERNAL :: temp_1sta_prms
      INTEGER, EXTERNAL :: temp_dist2_prms
      INTEGER, EXTERNAL :: temp_laps_prms
      INTEGER, EXTERNAL :: precip_prms
      INTEGER, EXTERNAL :: precip_laps_prms
      INTEGER, EXTERNAL :: precip_dist2_prms
      INTEGER, EXTERNAL :: xyz_dist
      INTEGER, EXTERNAL :: ddsolrad_hru_prms
      INTEGER, EXTERNAL :: ccsolrad_hru_prms
      INTEGER, EXTERNAL :: potet_pan_prms
      INTEGER, EXTERNAL :: potet_jh_prms
      INTEGER, EXTERNAL :: potet_hamon_hru_prms
      INTEGER, EXTERNAL :: intcp_prms
      INTEGER, EXTERNAL :: snowcomp_prms
      INTEGER, EXTERNAL :: srunoff_smidx_casc
      INTEGER, EXTERNAL :: srunoff_carea_casc
      INTEGER, EXTERNAL :: gsflow_modflow
      INTEGER, EXTERNAL :: soilzone_prms
      INTEGER, EXTERNAL :: gsflow_setconv
      INTEGER, EXTERNAL :: gsflow_prms2mf
      INTEGER, EXTERNAL :: gsflow_mf2prms
      INTEGER, EXTERNAL :: gsflow_budget
      INTEGER, EXTERNAL :: gsflow_sum
      INTEGER, EXTERNAL :: gwflow_casc_prms
      INTEGER, EXTERNAL :: strmflow_prms
      INTEGER, EXTERNAL :: subbasin_prms
      INTEGER, EXTERNAL :: hru_sum_prms
      INTEGER, EXTERNAL :: basin_sum_prms
      INTEGER, EXTERNAL :: grid_report
! Local Variables
      DOUBLE PRECISION :: dt
      INTEGER :: i
!***********************************************************************
      call_modules = 1

      ! Process_flag (0=run, 1=declare, 2=init, 3=clean)
      IF ( Arg=='run' ) THEN
        Process_flag = 0
      ELSEIF ( Arg=='declare' ) THEN
        Process_flag = 1
      ELSEIF ( Arg=='initialize' ) THEN
        Process_flag = 2
      ELSE  !IF ( Arg=='cleanup' ) THEN
        Process_flag = 3
      ENDIF

      IF ( Process_flag==1 ) THEN
        Versn_gsfprms =
     +'$Id: gsflow_prms.f 2506 2011-02-25 17:41:17Z rsregan $'
        call_modules = declmodule(Versn_gsfprms)

        IF ( control_string(Model_mode, 'model_mode').NE.0 ) RETURN
!       Model (0=GSFLOW; 1=PRMS; 2=MODFLOW)

        IF ( Model_mode(:6).EQ.'GSFLOW' .OR. Model_mode.EQ.'        ')
     +       THEN
          Model = 0
        ELSEIF ( Model_mode(:4).EQ.'PRMS' ) THEN
          Model = 1
        ELSEIF ( Model_mode(:5).EQ.'DAILY' ) THEN
          Model = 1
        ELSEIF ( Model_mode(:7).EQ.'MODFLOW' ) THEN
          Model = 2
! for MODFLOW simulations
          Kper_mfo = 1
          call_modules = gsflow_modflow()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_modflow', 'declare', call_modules
            RETURN
          ENDIF
          Process_flag = 2
          call_modules = gsflow_modflow()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_modflow', 'initialize', call_modules
            RETURN
          ENDIF
          Process_flag = 0
          DO WHILE ( Kper_mfo.LE.NPER )
            DO i = 1, NSTP(Kper_mfo)
              call_modules = gsflow_modflow()
              IF ( call_modules.NE.0 ) THEN
                PRINT 9001, 'gsflow_modflow', 'run', call_modules
                RETURN
              ENDIF
            ENDDO
            Kper_mfo = Kper_mfo + 1
          ENDDO
          Process_flag = 3
          call_modules = gsflow_modflow()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_modflow', 'cleanup', call_modules
            RETURN
          ENDIF
        ELSE
          Model = -1
          PRINT 9002, Model_mode, Arg
          RETURN
        ENDIF

        Nsub = getdim('nsub')
        IF ( Nsub==-1 ) RETURN

        Ncascade = getdim('ncascade')
        IF ( Ncascade==-1 ) RETURN

        Ncascdgw = getdim('ncascdgw')
        IF ( Ncascdgw==-1 ) RETURN
        IF ( Ncascade>1 .OR. Ncascdgw>1 ) THEN
          Cascade = 1
        ELSE
          Cascade = 0
        ENDIF

        Nsegment = getdim('nsegment')
        IF ( Nsegment==-1 ) RETURN

        WRITE (*, 10) Versn_gsfprms(20:34)
  10    FORMAT (/, 5X, 'Surface Water and Energy Budgets Simulated by:',
     +          /, 14X, 'PRMS Version 2.', A, /)
      PRINT*,'    GSFLOW includes the following PRMS modules:'
      PRINT*,'basin_prms, basin_sum_prms, cascade_prms'
      PRINT*,'ccsolrad_hru_prms, ddsolrad_hru_prms, grid_report'
      PRINT*,'gwflow_casc_prms, hru_sum_prms, intcp_prms'
      PRINT*,'obs_adjust_prms, obs_prms, potet_jh_prms'
      PRINT*,'potet_hamon_hru_prms, potet_pan_prms, precip_prms'
      PRINT*,'precip_dist2_prms, precip_laps_prms, snowcomp_prms'
      PRINT*,'soilzone_prms, soltab_hru_prms, srunoff_carea_casc'
      PRINT*,'srunoff_smidx_casc, strmflow_prms, temp_1sta_prms'
      PRINT*,'temp_dist2_prms, temp_laps_prms, transp_tindex_prms'
      PRINT*,'xyz_dist'

        IF ( control_string(Precip_module, 'precip_module')
     +       .NE.0 ) RETURN
        IF ( Precip_module(:11).NE.'precip_prms' .AND.
     +       Precip_module(:16).NE.'precip_laps_prms' .AND.
     +       Precip_module(:17).NE.'precip_dist2_prms' .AND.
     +       Precip_module(:8).NE.'xyz_dist' ) THEN
          PRINT *, 'WARNING: Invalid precip_module value, reset to:',
     +             ' precip_prms, value was: ', Precip_module
          Precip_module = 'precip_prms'
        ENDIF

        IF ( control_string(Temp_module, 'temp_module').NE.0 ) RETURN
        IF ( Temp_module(:14).NE.'temp_1sta_prms' .AND.
     +       Temp_module(:14).NE.'temp_laps_prms' .AND.
     +       Temp_module(:15).NE.'temp_dist2_prms' .AND.
     +       Temp_module(:8).NE.'xyz_dist' ) THEN
          PRINT *, 'WARNING: Invalid temp_module value, reset to:',
     +             ' temp_1sta_prms, value was: ', Temp_module
          Temp_module = 'temp_1sta_prms'
        ENDIF

        IF ( Precip_module(:11)=='precip_prms' ) THEN
          Precip_flag = 1
        ELSEIF ( Precip_module(:16)=='precip_laps_prms' ) THEN
          Precip_flag = 2
        ELSEIF ( Precip_module(:17)=='precip_dist2_prms' ) THEN
          Precip_flag = 3
        ELSE ! xyz_dist
          Precip_flag = 6
          IF ( Temp_module(:8)/='xyz_dist' ) THEN
            PRINT *,'if xyz_dist is specified for precip module,'
            PRINT *, 'it also must be specified for temp module.'
            PRINT *, 'using xyz_dist'
            Temp_module = Precip_module
            Temp_flag = 6
          ENDIF
        ENDIF

        IF ( Temp_module(:14)=='temp_1sta_prms' ) THEN
          Temp_flag = 1
        ELSEIF ( Temp_module(:14)=='temp_laps_prms' ) THEN
          Temp_flag = 3
        ELSEIF ( Temp_module(:15)=='temp_dist2_prms' ) THEN
          Temp_flag = 4
        ELSE ! xyz_dist
          Temp_flag = 6
          IF ( Precip_module(:8)/='xyz_dist' ) THEN
            PRINT *,'if xyz_dist is specified for temperature module,'
            PRINT *, 'it also must be specified for precip module.'
            PRINT *, 'using xyz_dist'
            Precip_module = Temp_module
            Precip_flag = 6
          ENDIF
        ENDIF

        IF ( control_string(Et_module, 'et_module').NE.0 ) RETURN
        IF ( Et_module(:13).NE.'potet_jh_prms' .AND.
     +       Et_module(:14).NE.'potet_pan_prms' .AND.
     +       Et_module(:20).NE.'potet_hamon_hru_prms' ) THEN
          PRINT *, 'WARNING: Invalid et_module value, reset to:',
     +             ' potet_jh_prms, value was: ', Et_module
          Et_module = 'potet_jh_prms'
        ENDIF
        IF ( Et_module(:13)=='potet_jh_prms' ) THEN
          Et_flag = 1
        ELSEIF ( Et_module(:20)=='potet_hamon_hru_prms' ) THEN
          Et_flag = 2
        ELSE ! pan
          Et_flag = 4
        ENDIF

        IF ( control_string(Srunoff_module, 'srunoff_module')
     +       .NE.0 ) RETURN
        IF ( Srunoff_module(:18).NE.'srunoff_smidx_casc' .AND.
     +       Srunoff_module(:18).NE.'srunoff_carea_casc' ) THEN
          PRINT *, 'WARNING: Invalid srunoff_module value, reset to:',
     +             ' srunoff_smidx_casc, value was: ', Srunoff_module
          Srunoff_module = 'srunoff_smidx_casc'
        ENDIF
        IF ( Srunoff_module(:18)=='srunoff_smidx_casc' ) THEN
          Sroff_flag = 1
        ELSE ! carea
          Sroff_flag = 2
        ENDIF

        IF ( control_integer(grid_reportON_OFF, 'grid_reportON_OFF')
     +       /=0 ) RETURN

        IF ( control_string(Solrad_module, 'solrad_module')
     +       .NE.0 ) RETURN
        IF ( Solrad_module(:17).NE.'ddsolrad_hru_prms' .AND.
     +       Solrad_module(:17).NE.'ccsolrad_hru_prms' ) THEN
          PRINT *, 'WARNING: Invalid solrad_module value, reset to:',
     +             ' ddsolrad_hru_prms, value was: ', Solrad_module
          Solrad_module = 'ddsolrad_hru_prms'
        ENDIF
        IF ( Solrad_module(:17)=='ddsolrad_hru_prms' ) THEN
          Solrad_flag = 1
        ELSE ! ccsolrad_hru_prms
          Solrad_flag = 2
        ENDIF
      ENDIF

      IF ( Process_flag==0 ) THEN ! run process
!   Check to see if daily time step
        dt = deltim()
        IF ( dt>24.0001D0 ) THEN
          PRINT *, 'ERROR, timestep > daily, fix Data File', dt
          RETURN
        ENDIF
        IF ( dt.LT.23.999D0 ) THEN
           PRINT *, 'ERROR, timestep < daily, fix Data File', dt
           RETURN
        ENDIF

! All modules must be called during declare for model modes 0 & 1
      ELSE
        call_modules = basin_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'basin_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = climate_vars_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'climate_vars_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = flow_vars_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'flow_vars_prms', Arg, call_modules
          RETURN
        ENDIF

        IF ( Cascade==1 ) THEN
          call_modules = cascade_prms()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'cascade_prms', Arg, call_modules
            RETURN
          ENDIF
        ENDIF

        call_modules = soltab_hru_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'soltab_hru_prms', Arg, call_modules
          RETURN
        ENDIF
      ENDIF

      call_modules = obs_prms()
      IF ( call_modules.NE.0 ) THEN
        PRINT 9001, 'obs_prms', Arg, call_modules
        RETURN
      ENDIF

      IF ( Temp_flag<4 ) THEN
        call_modules = obs_adjust_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'obs_adjust_prms', Arg, call_modules
          RETURN
        ENDIF
      ENDIF

      IF ( Temp_flag==6 ) THEN
        call_modules = xyz_dist()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'xyz_dist', Arg, call_modules
          RETURN
        ENDIF
      ELSE
        IF ( Temp_flag==1 ) THEN
          call_modules = temp_1sta_prms()
        ELSEIF ( Temp_flag==3 ) THEN
          call_modules = temp_laps_prms()
        ELSE ! Temp_flag=4
          call_modules = temp_dist2_prms()
        ENDIF
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, Temp_module, Arg, call_modules
          RETURN
        ENDIF

        IF ( Precip_flag==1 ) THEN
          call_modules = precip_prms()
        ELSEIF ( Precip_flag==2 ) THEN
          call_modules = precip_laps_prms()
        ELSE
          call_modules = precip_dist2_prms()
        ENDIF
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, Precip_module, Arg, call_modules
          RETURN
        ENDIF
      ENDIF

      IF ( Solrad_flag==1 ) THEN
        call_modules = ddsolrad_hru_prms()
      ELSE
        call_modules = ccsolrad_hru_prms()
      ENDIF
      IF ( call_modules.NE.0 ) THEN
        PRINT 9001, Solrad_module, Arg, call_modules
        RETURN
      ENDIF

      call_modules = transp_tindex_prms()
      IF ( call_modules.NE.0 ) THEN
        PRINT 9001, 'transp_tindex_prms', Arg, call_modules
        RETURN
      ENDIF

      IF ( Et_flag==1 ) THEN
        call_modules = potet_jh_prms()
      ELSEIF ( Et_flag==2 ) THEN
        call_modules = potet_hamon_hru_prms()
      ELSE
        call_modules = potet_pan_prms()
      ENDIF
      IF ( call_modules.NE.0 ) THEN
        PRINT 9001, Et_module, Arg, call_modules
        RETURN
      ENDIF

      call_modules = intcp_prms()
      IF ( call_modules.NE.0 ) THEN
        PRINT 9001, 'intcp_prms', Arg, call_modules
        RETURN
      ENDIF

      call_modules = snowcomp_prms()
      IF ( call_modules.NE.0 ) THEN
        PRINT 9001, 'snowcomp_prms', Arg, call_modules
        RETURN
      ENDIF

      IF ( Sroff_flag==1 ) THEN
        call_modules = srunoff_smidx_casc()
      ELSE
        call_modules = srunoff_carea_casc()
      ENDIF
      IF ( call_modules.NE.0 ) THEN
        PRINT 9001, Srunoff_module, Arg, call_modules
        RETURN
      ENDIF

! for PRMS-only simulations
      IF ( Model.EQ.1 ) THEN
        call_modules = soilzone_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'soilzone_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = gwflow_casc_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'gwflow_casc_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = strmflow_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'strmflow_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = hru_sum_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'hru_sum_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = basin_sum_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'basin_sum_prms', Arg, call_modules
          RETURN
        ENDIF

        IF ( grid_reportON_OFF==1 ) THEN
          call_modules = grid_report()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'grid_report', Arg, call_modules
            RETURN
          ENDIF
        ENDIF

! for GSFLOW simulations
      ! ELSEIF ( Model.EQ.0 ) THEN
      ELSE

        call_modules = gsflow_modflow()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'gsflow_modflow', Arg, call_modules
          RETURN
        ENDIF

! The following modules are in the MODFLOW iteration loop
! (contained in gsflow_modflow.f).
! They still need to be called for declare, initialize and cleanup
        IF ( Process_flag.NE.0 ) THEN
          call_modules = gsflow_setconv()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_setconv', Arg, call_modules
            RETURN
          ENDIF

! SOILZONE for GSFLOW is in the MODFLOW iteration loop,
! only call for declare, initialize, and cleanup.
          call_modules = soilzone_prms()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'soilzone_prms', Arg, call_modules
            RETURN
          ENDIF

          call_modules = gsflow_prms2mf()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_prms2mf', Arg, call_modules
            RETURN
          ENDIF

          call_modules = gsflow_mf2prms()
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_mf2prms', Arg, call_modules
            RETURN
          ENDIF
        ENDIF

        call_modules = gsflow_budget()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'gsflow_budget', Arg, call_modules
          RETURN
        ENDIF

        call_modules = gsflow_sum()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'gsflow_sum', Arg, call_modules
          RETURN
        ENDIF
      ENDIF

      IF ( Nsub>0 ) THEN
        call_modules = subbasin_prms()
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'subbasin_prms', Arg, call_modules
          RETURN
        ENDIF
      ENDIF

      call_modules = 0

 9001 FORMAT ('Error in ', A, ' module, arg = ', A, /, 'Return val =',
     +        I4)
 9002 FORMAT ('Error in gsflow_prms: model_mode', A,
     +        ' not implemented', /, A)

      END FUNCTION call_modules

!***********************************************************************
!     declare the dimensions
!***********************************************************************

      INTEGER FUNCTION setdims()
      IMPLICIT NONE
      INTEGER, EXTERNAL :: decldim, declfix
! Local Variables
      INTEGER :: ndepl
      ! Maximum values are no longer limits
      INTEGER, PARAMETER :: MAXDIM = 500
!***********************************************************************
      setdims = 1

! spatial units
      IF ( decldim('ngw', 1, MAXDIM, 'Number of groundwater reservoirs')
     +     .NE.0 ) RETURN

      IF ( decldim('nhru', 1, MAXDIM,
     +     'Number of hydrologic response units').NE.0 ) RETURN

      IF ( decldim('nsegment', 0, MAXDIM,
     +     'Number of stream channel segments').NE.0 ) RETURN

      IF ( decldim('nsfres', 0, MAXDIM,
     +     'Number of on-channel storage reservoirs').NE.0 ) RETURN

      IF ( decldim('nssr', 1, MAXDIM, 'Number of subsurface reservoirs')
     +     .NE.0 ) RETURN

      IF ( decldim('nsub', 0, MAXDIM, 'Number of internal subbasins')
     +     .NE.0 ) RETURN

! Time-series data stations
      IF ( decldim('nevap', 0, MAXDIM,
     +     'Number of pan evaporation data sets').NE.0 ) RETURN

      IF ( decldim('nform', 0, MAXDIM,
     +     'Number of form_data input values. This has a max of 1 but'//
     +     ' is an array to satisy a readvar requirement. Set to 0 if'//
     +     ' no form_data are included in the input file.')
     +     .NE.0 ) RETURN

      IF ( decldim('nobs', 0, MAXDIM,
     +     'Number of stream measurement stations').NE.0 ) RETURN

      IF ( decldim('nrain', 1, MAXDIM, 'Number of precipitation gages')
     +     .NE.0 ) RETURN

!     IF ( decl dim('nsnow', 0, MAXDIM,
!    +     'Number of snow measurement stations').NE.0 ) RETURN

      IF ( decldim('nsol', 0, MAXDIM,
     +     'Number of solar radiation stations').NE.0 ) RETURN

      IF ( decldim('ntemp', 1, MAXDIM,
     +     'Number of air-temperature measurement stations')
     +     .NE.0 ) RETURN

! computation dimensions
      IF ( decldim('ncascade', 0, MAXDIM,
     +     'Number of subareas of all HRUs for cascading flow')
     +     .NE.0 ) RETURN

      IF ( decldim('ncascdgw', 0, MAXDIM,
     +     'Number of subareas of all GWRs for cascading flow')
     +     .NE.0 ) RETURN

! depletion curves
      ndepl = 1
      IF ( decldim('ndepl', ndepl, MAXDIM,
     +     'Number of snow depletion curves').NE.0 ) RETURN

      IF ( decldim('ndeplval', ndepl*11, MAXDIM, 'ndepl * 11')
     +     .NE.0 ) RETURN

! gsflow dimensions
      IF ( decldim('nhrucell', 0, MAXDIM,
     +     'Number of unique intersections between SSRs and GW cells')
     +     .NE.0 ) RETURN

      IF ( decldim('nreach', 0, MAXDIM,
     +     'Number of reaches on all stream segments').NE.0 ) RETURN

      IF ( decldim('ngwcell', 0, MAXDIM,
     +     'Number of cells in the GW model').NE.0 ) RETURN

! fixed dimensions
      IF ( declfix('ndays', 366, MAXDIM, 'Number of days in a year')
     +     .NE.0 ) RETURN

      IF ( declfix('nlapse', 3, MAXDIM,
     +     'Number of lapse rates in X, Y, and Z directions')
     +     .NE.0 ) RETURN

      IF ( declfix('nmonths', 12, MAXDIM, 'Number of months in a year')
     +     .NE.0 ) RETURN

      setdims = 0
      END FUNCTION setdims

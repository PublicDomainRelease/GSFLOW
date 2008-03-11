!***********************************************************************
!     GSFLOW module that replaces call_modules.c and setdims.f
!***********************************************************************
      MODULE GSFPRMS_MODULE
      IMPLICIT NONE
      CHARACTER(LEN=32) :: Precip_module, Temp_module, Et_module
      CHARACTER(LEN=32) :: Srunoff_module, Solrad_module
      INTEGER :: Kper_mfo, Kkstp_mfo
!   Declared Variables
      INTEGER :: Model
!   Control Parameters
      CHARACTER(LEN=8) :: Model_mode
      END MODULE GSFPRMS_MODULE

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION call_modules(Arg)
      USE GSFPRMS_MODULE
      USE GLOBAL, ONLY:NSTP, NPER
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions (define in calling order)
      INTEGER, EXTERNAL :: basin_prms
      INTEGER, EXTERNAL :: cascade_prms
      INTEGER, EXTERNAL :: obs_prms
      INTEGER, EXTERNAL :: soltab_hru_prms
      INTEGER, EXTERNAL :: temp_1sta_prms
      INTEGER, EXTERNAL :: temp_laps_prms
      INTEGER, EXTERNAL :: temp_dist2_prms
      INTEGER, EXTERNAL :: precip_prms
      INTEGER, EXTERNAL :: precip_laps_prms
      INTEGER, EXTERNAL :: precip_dist2_prms
      INTEGER, EXTERNAL :: xyz_dist
      INTEGER, EXTERNAL :: ddsolrad_hru_prms
      INTEGER, EXTERNAL :: ccsolrad_hru_prms
      INTEGER, EXTERNAL :: potet_jh_prms
      INTEGER, EXTERNAL :: potet_hamon_hru_prms
      INTEGER, EXTERNAL :: potet_pan_prms
      INTEGER, EXTERNAL :: intcp_prms
      INTEGER, EXTERNAL :: snowcomp_prms
      INTEGER, EXTERNAL :: srunoff_smidx_casc
      INTEGER, EXTERNAL :: srunoff_carea_casc
      INTEGER, EXTERNAL :: gsflow_modflow
      INTEGER, EXTERNAL :: soilzone_gsflow
      INTEGER, EXTERNAL :: gsflow_setconv
      INTEGER, EXTERNAL :: gsflow_prms2mf
      INTEGER, EXTERNAL :: gsflow_mf2prms
      INTEGER, EXTERNAL :: gsflow_budget
      INTEGER, EXTERNAL :: gsflow_sum
      INTEGER, EXTERNAL :: gwflow_casc_prms
      INTEGER, EXTERNAL :: strmflow_prms
      INTEGER, EXTERNAL :: hru_sum_prms
      INTEGER, EXTERNAL :: basin_sum_prms
! Local Variables
      INTEGER :: i
!***********************************************************************
      call_modules = 1

      IF ( Arg.EQ.'declare' ) THEN
        call_modules = declmodule(
     &'$Id: gsflow_prms.f 3815 2008-02-08 17:50:01Z rsregan $')

        IF ( declvar('gsflow_prms', 'model', 'one', 1, 'integer',
     &       'Simulation model (GSFLOW, PRMS, MODFLOW)', 'none',
     &       Model).NE.0 ) RETURN

        IF ( control_string(Model_mode, 'model_mode').NE.0 ) RETURN

        IF ( Model_mode(:6).EQ.'GSFLOW' .OR. Model_mode.EQ.'        ')
     &       THEN
          Model = 1
        ELSEIF ( Model_mode(:4).EQ.'PRMS' ) THEN
          Model = 2
        ELSEIF ( Model_mode(:7).EQ.'MODFLOW' ) THEN
          Model = 3
        ELSE
          Model = -1
          PRINT 9002, Model_mode, Arg
          RETURN
        ENDIF

        IF ( Model.NE.3 ) THEN
          IF ( control_string(Precip_module, 'precip_module')
     &         .NE.0 ) RETURN
          IF ( Precip_module(:11).NE.'precip_prms' .AND.
     &         Precip_module(:16).NE.'precip_laps_prms' .AND.
     &         Precip_module(:17).NE.'precip_dist2_prms' .AND.
     &         Precip_module(:8).NE.'xyz_dist' ) THEN
            PRINT *, 'WARNING: Invalid precip_module value, reset to:',
     &             ' precip_prms, value was: ', Precip_module
            Precip_module = 'precip_prms'
          ENDIF

          IF ( control_string(Temp_module, 'temp_module').NE.0 ) RETURN
          IF ( Temp_module(:14).NE.'temp_1sta_prms' .AND.
     &         Temp_module(:14).NE.'temp_laps_prms' .AND.
     &         Temp_module(:15).NE.'temp_dist2_prms' .AND.
     &         Temp_module(:8).NE.'xyz_dist' ) THEN
            PRINT *, 'WARNING: Invalid temp_module value, reset to:',
     &               ' temp_1sta_prms, value was: ', Temp_module
            Temp_module = 'temp_1sta_prms'
          ENDIF

          IF ( control_string(Et_module, 'et_module').NE.0 ) RETURN
          IF ( Et_module(:13).NE.'potet_jh_prms' .AND.
     &         Et_module(:14).NE.'potet_pan_prms' .AND.
     &         Et_module(:20).NE.'potet_hamon_hru_prms' ) THEN
            PRINT *, 'WARNING: Invalid et_module value, reset to:',
     &               ' potet_jh_prms, value was: ', Et_module
            Et_module = 'potet_jh_prms'
          ENDIF

          IF ( control_string(Srunoff_module, 'srunoff_module')
     &         .NE.0 ) RETURN
          IF ( Srunoff_module(:18).NE.'srunoff_smidx_casc' .AND.
     &         Srunoff_module(:18).NE.'srunoff_carea_casc' ) THEN
            PRINT *, 'WARNING: Invalid srunoff_module value, reset to:',
     &               ' srunoff_smidx_casc, value was: ', Srunoff_module
            Srunoff_module = 'srunoff_smidx_casc'
          ENDIF

          IF ( control_string(Solrad_module, 'solrad_module')
     &         .NE.0 ) RETURN
          IF ( Solrad_module(:17).NE.'ddsolrad_hru_prms' .AND.
     &         Solrad_module(:17).NE.'ccsolrad_hru_prms' ) THEN
            PRINT *, 'WARNING: Invalid solrad_module value, reset to:',
     &               ' ddsolrad_hru_prms, value was: ', Solrad_module
            Solrad_module = 'ddsolrad_hru_prms'
          ENDIF

! if xyz is specified for one, it must be specified for the other
          IF ( Temp_module(:8).EQ.'xyz_dist' ) THEN
            Precip_module = Temp_module
          ELSEIF ( Precip_module(:8).EQ.'xyz_dist' ) THEN
            Temp_module = Precip_module
          ENDIF

        ENDIF
      ENDIF

      IF ( Model.NE.3 ) THEN
! All modules must be called during declare for model modes 1 & 2
        IF ( Arg.NE.'run' ) THEN
          call_modules = basin_prms(Arg)
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'basin_prms', Arg, call_modules
            RETURN
          ENDIF

          call_modules = cascade_prms(Arg)
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'cascade_prms', Arg, call_modules
            RETURN
          ENDIF
        ENDIF

        call_modules = obs_prms(Arg)
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'obs_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = soltab_hru_prms(Arg)
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'soltab_hru_prms', Arg, call_modules
          RETURN
        ENDIF

        IF ( Temp_module(:14).EQ.'temp_1sta_prms' ) THEN
          call_modules = temp_1sta_prms(Arg)
        ELSEIF ( Temp_module(:14).EQ.'temp_laps_prms' ) THEN
          call_modules = temp_laps_prms(Arg)
        ELSEIF ( Temp_module(:15).EQ.'temp_dist2_prms' ) THEN
          call_modules = temp_dist2_prms(Arg)
        ELSEIF ( Temp_module(:8).EQ.'xyz_dist' ) THEN
          call_modules = xyz_dist(Arg)
        ENDIF
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, Temp_module, Arg, call_modules
          RETURN
        ENDIF

        IF ( Precip_module(:11).EQ.'precip_prms' ) THEN
          call_modules = precip_prms(Arg)
        ELSEIF ( Precip_module(:16).EQ.'precip_laps_prms' ) THEN
          call_modules = precip_laps_prms(Arg)
        ELSEIF ( Precip_module(:17).EQ.'precip_dist2_prms' ) THEN
          call_modules = precip_dist2_prms(Arg)
        ENDIF
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, Precip_module, Arg, call_modules
          RETURN
        ENDIF

        IF ( Solrad_module(:17).EQ.'ddsolrad_hru_prms' ) THEN
          call_modules = ddsolrad_hru_prms(Arg)
        ELSEIF ( Solrad_module(:17).EQ.'ccsolrad_hru_prms' ) THEN
           call_modules = ccsolrad_hru_prms(Arg)
        ENDIF
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, Solrad_module, Arg, call_modules
          RETURN
        ENDIF

        IF ( Et_module(:13).EQ.'potet_jh_prms' ) THEN
          call_modules = potet_jh_prms(Arg)
        ELSEIF ( Et_module(:20).EQ.'potet_hamon_hru_prms' ) THEN
          call_modules = potet_hamon_hru_prms(Arg)
        ELSEIF ( Et_module(:14).EQ.'potet_pan_prms' ) THEN
          call_modules = potet_pan_prms(Arg)
        ENDIF
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, Et_module, Arg, call_modules
          RETURN
        ENDIF

        call_modules = intcp_prms(Arg)
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'intcp_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = snowcomp_prms(Arg)
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'snowcomp_prms', Arg, call_modules
          RETURN
        ENDIF

        IF ( Srunoff_module(:18).EQ.'srunoff_smidx_casc' ) THEN
          call_modules = srunoff_smidx_casc(Arg)
        ELSEIF ( Srunoff_module(:18).EQ.'srunoff_carea_casc' ) THEN
          call_modules = srunoff_carea_casc(Arg)
        ENDIF
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, Srunoff_module, Arg, call_modules
          RETURN
        ENDIF

! SOILZONE for GSFLOW is in the MODFLOW iteration loop,
! only call for declare, initialize, and cleanup.
        IF ( Arg.NE.'run' .OR. Model.EQ.2 ) THEN
          call_modules = soilzone_gsflow(Arg)
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'soilzone_gsflow', Arg, call_modules
            RETURN
          ENDIF
        ENDIF

        IF ( Arg.EQ.'declare' .OR. Model.EQ.1 ) THEN
          call_modules = gsflow_modflow(Arg)
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_modflow', Arg, call_modules
            RETURN
          ENDIF

! The following modules are in the MODFLOW iteration loop
! (contained in gsflow_modflow.f).
! They still need to be called for declare, initialize and cleanup
          IF ( Arg.NE.'run' ) THEN
            call_modules = gsflow_setconv(Arg)
            IF ( call_modules.NE.0 ) THEN
              PRINT 9001, 'gsflow_setconv', Arg, call_modules
              RETURN
            ENDIF

            call_modules = gsflow_prms2mf(Arg)
            IF ( call_modules.NE.0 ) THEN
              PRINT 9001, 'gsflow_prms2mf', Arg, call_modules
              RETURN
            ENDIF

            call_modules = gsflow_mf2prms(Arg)
            IF ( call_modules.NE.0 ) THEN
              PRINT 9001, 'gsflow_mf2prms', Arg, call_modules
              RETURN
            ENDIF

            call_modules = gsflow_budget(Arg)
            IF ( call_modules.NE.0 ) THEN
              PRINT 9001, 'gsflow_budget', Arg, call_modules
              RETURN
            ENDIF
          ENDIF

          call_modules = gsflow_sum(Arg)
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gsflow_sum', Arg, call_modules
            RETURN
          ENDIF
        ENDIF

! gwflow and strmflow for PRMS-only simulations
        IF ( Arg.EQ.'declare' .OR. Model.EQ.2 ) THEN
          call_modules = gwflow_casc_prms(Arg)
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'gwflow_casc_prms', Arg, call_modules
            RETURN
          ENDIF

          call_modules = strmflow_prms(Arg)
          IF ( call_modules.NE.0 ) THEN
            PRINT 9001, 'strmflow_prms', Arg, call_modules
            RETURN
          ENDIF
        ENDIF

        call_modules = hru_sum_prms(Arg)
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'hru_sum_prms', Arg, call_modules
          RETURN
        ENDIF

        call_modules = basin_sum_prms(Arg)
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'basin_sum_prms', Arg, call_modules
          RETURN
        ENDIF

!     ELSEIF ( Model.EQ.3 ) THEN
      ELSE
! MODFLOW only mode
        Kper_mfo = 1
        call_modules = gsflow_modflow('declare')
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'gsflow_modflow', 'declare', call_modules
          RETURN
        ENDIF
        call_modules = gsflow_modflow('initialize')
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'gsflow_modflow', 'initialize', call_modules
          RETURN
        ENDIF
        DO WHILE ( Kper_mfo.LE.NPER )
           DO i = 1, NSTP(Kper_mfo)
            call_modules = gsflow_modflow('run')
            IF ( call_modules.NE.0 ) THEN
              PRINT 9001, 'gsflow_modflow', 'run', call_modules
              RETURN
            ENDIF
          ENDDO
          Kper_mfo = Kper_mfo + 1
        ENDDO
        call_modules = gsflow_modflow('cleanup')
        IF ( call_modules.NE.0 ) THEN
          PRINT 9001, 'gsflow_modflow', 'cleanup', call_modules
          RETURN
        ENDIF

      ENDIF

      call_modules = 0

 9001 FORMAT ('Error in ', A, ' module, arg = ', A, /, 'Return val =',
     &        I4)
 9002 FORMAT ('Error in gsflow_prms: model_mode', A, ' not implemented',
     &        /, A)

      END FUNCTION call_modules

!***********************************************************************
!     declare the dimensions
!***********************************************************************

      INTEGER FUNCTION setdims()
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: ndepl
!***********************************************************************
      setdims = 1

! spatial units
      IF ( decldim('nhru', 1, MAXHRU,
     &     'Number of hydrologic response units').NE.0 ) RETURN

      IF ( decldim('ngw', 1, MAXGWR, 'Number of groundwater reservoirs')
     &     .NE.0 ) RETURN

      IF ( decldim('nssr', 1, MAXSSR, 'Number of subsurface reservoirs')
     &     .NE.0 ) RETURN

      IF ( decldim('nsub', 0, MAXSUB, 'Number of internal subbasins')
     &     .NE.0 ) RETURN

      IF ( decldim('ngroup', 0, MAXGROUP,
     &     'Number of HRU groupings').NE.0 ) RETURN

! data stations
      IF ( decldim('nrain', 1, MAXRAIN, 'Number of precipitation gages')
     &     .NE.0 ) RETURN

      IF ( decldim('ntemp', 1, MAXTEMP,
     &     'Number of air-temperature measurement stations')
     &     .NE.0 ) RETURN

      IF ( decldim('nobs', 0, MAXOBS,
     &     'Number of stream measurement stations').NE.0 ) RETURN

      IF ( decldim('nform', 0, MAXFORM,
     &     'Number of form_data input values. This has a max of 1 but'//
     &     ' is an array to satisy a readvar requirement. Set to 0 if'//
     &     ' no form_data are included in the input file.')
     &     .NE.0 ) RETURN

      IF ( decldim('nsol', 0, MAXSOL,
     &     'Number of solar radiation stations').NE.0 ) RETURN

      IF ( decldim('nevap', 0, MAXEVAP,
     &     'Number of pan evaporation data sets').NE.0 ) RETURN

! depletion curves
      ndepl = 1
      IF ( decldim('ndepl', ndepl, MAXSNODPL,
     &     'Number of snow depletion curves').NE.0 ) RETURN

      IF ( decldim('ndeplval', ndepl*11, MAXDEPLVAL, 'ndepl * 11')
     &     .NE.0 ) RETURN

! cascading dimensions
      IF ( decldim('ncascade', 0, MAXCASCADE,
     &     'Number of subareas of all HRUs for cascading flow')
     &     .NE.0 ) RETURN

      IF ( decldim('ncascdgw', 0, MAXCASCADE,
     &     'Number of subareas of all GWRs for cascading flow')
     &     .NE.0 ) RETURN

!     IF ( decldim('ndown', 0, MAXDOWN,
!    &     'Number of downstream HRU or flow planes for cascading flow')
!    &     .NE.0 ) RETURN

! gsflow dimensions
      IF ( decldim('nhrucell', 0, MAXHRUCELL,
     &     'Number of unique intersections between SSRs and GW cells')
     &     .NE.0 ) RETURN

      IF ( decldim('nsegment', 0, MAXSEGMENT,
     &     'Number of stream segments').NE.0 ) RETURN

      IF ( decldim('nreach', 0, MAXREACH,
     &     'Number of reaches on all stream segments').NE.0 ) RETURN

      IF ( decldim('ngwcell', 0, MAXGWCELL,
     &     'Number of cells in the GW model').NE.0 ) RETURN

! reservoirs/lakes
      IF ( decldim('nsfres', 0, MAXSFR,
     &     'Number of on-channel storage reservoirs').NE.0 ) RETURN

      IF ( decldim('mxnsos', 0, MAXNSOS,
     &     'Maximum number of storage/outflow table values for'//
     &     ' channel storage reservoir using Puls routing')
     &     .NE.0 ) RETURN

! fixed dimensions
      IF ( declfix('nmonths', 12, MAXMO, 'Number of months in a year')
     &     .NE.0 ) RETURN

      IF ( declfix('ndays', 366, MAXDAY, 'Number of days in a year')
     &     .NE.0 ) RETURN

      IF ( declfix('nlapse', 3, MAXLAPSE,
     &     'Number of lapse rates in X, Y, and Z directions')
     &     .NE.0 ) RETURN

      setdims = 0
      END FUNCTION setdims

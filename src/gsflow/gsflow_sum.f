!**********************************************************************
!     Sums values for daily, monthly, yearly and total flow
!     for daily mode
!***********************************************************************

      MODULE GSFSUM
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 188
      DOUBLE PRECISION, PARAMETER :: ERRCHK = 0.0001D0
      INTEGER, SAVE :: Balance_unt, Vbnm_index(13), Gsf_unt, Rpt_count
      INTEGER, SAVE :: Have_wells
      DOUBLE PRECISION, SAVE :: Cumvol_precip, Cumvol_strmin
      DOUBLE PRECISION, SAVE :: Rate_precip, Cumvol_gwbndin
      DOUBLE PRECISION, SAVE :: Rate_gwbndin, Cumvol_wellin
      DOUBLE PRECISION, SAVE :: Cumvol_et, Rate_et, Cumvol_strmot
      DOUBLE PRECISION, SAVE :: Rate_strmot, Cumvol_wellot
      DOUBLE PRECISION, SAVE :: Cumvol_gwbndot, Rate_gwbndot
      DOUBLE PRECISION, SAVE :: Cum_surfstor, Basin_convert
      DOUBLE PRECISION, SAVE :: Cum_delstore, Rate_delstore
      DOUBLE PRECISION, SAVE :: Rate_farout, Cumvol_farout
      DOUBLE PRECISION, SAVE :: Last_basin_soil_moist, Last_basin_ssstor
      DOUBLE PRECISION, SAVE :: Rate_strmin, Rate_wellin, Rate_wellot
      DOUBLE PRECISION, SAVE :: Rate_surfstor, Last_basingravstor
      DOUBLE PRECISION, SAVE :: Last_basinintcpstor, Basinswaleet
      DOUBLE PRECISION, SAVE :: Last_basinimpervstor
      DOUBLE PRECISION, SAVE :: Last_basinpweqv, Last_basinsoilmoist
      DOUBLE PRECISION, SAVE :: Basin_gsfstor, Last_basinprefstor
      CHARACTER(LEN=10), PARAMETER :: MODNAME = 'gsflow_sum'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'GSFLOW Summary'
!      DOUBLE PRECISION, SAVE :: Cumvol_lakeppt, Cumvol_lakeevap, Cumvol_uzfet
! Added lake variables
      DOUBLE PRECISION, SAVE :: Rate_lakin, Rate_lakot, Cumvol_lakin
      DOUBLE PRECISION, SAVE :: Rate_lakestor, Cum_lakestor,Cumvol_lakot
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Cum_soilstor, Rate_soilstor
      DOUBLE PRECISION, SAVE :: Cum_uzstor, Rate_uzstor, Basingwstor
      DOUBLE PRECISION, SAVE :: Cum_satstor, Rate_satstor, Basingvr2sm
      DOUBLE PRECISION, SAVE :: Cum_pweqv, Rate_pweqv, Lake_change_stor
      DOUBLE PRECISION, SAVE :: Basinpweqv, Ave_uzf_infil, Basininfil
      DOUBLE PRECISION, SAVE :: Basinsoilstor, Basinsoilmoist
      DOUBLE PRECISION, SAVE :: Basinsoiltogw, Basinstrmflow
      DOUBLE PRECISION, SAVE :: Strm_stor, Lake_stor, Gwflow2strms
      DOUBLE PRECISION, SAVE :: Basinppt, Basinpervet, Basinimpervevap
      DOUBLE PRECISION, SAVE :: Basinsz2gw, Basingw2sz, Lakebed_loss
      DOUBLE PRECISION, SAVE :: Basinintcpstor, Basinimpervstor
      DOUBLE PRECISION, SAVE :: Basininterflow, Basinsroff
      DOUBLE PRECISION, SAVE :: Obs_strmflow, Basinszreject
      DOUBLE PRECISION, SAVE :: Unsat_et, Sat_et, Uzf_et, Uzf_recharge
      DOUBLE PRECISION, SAVE :: Basinseepout, Uzf_infil, Uzf_del_stor
      DOUBLE PRECISION, SAVE :: Basinrain, Basinsnow, Basinslowflow
      DOUBLE PRECISION, SAVE :: Basingvr2pfr, Basinsnowevap
      DOUBLE PRECISION, SAVE :: Basinhortonian, Basinhortonianlakes
      DOUBLE PRECISION, SAVE :: Basinlakeinsz, Basinlakeevap
      DOUBLE PRECISION, SAVE :: Basinlakeprecip, Basingravstor
      DOUBLE PRECISION, SAVE :: Basinprefflow, Basinprefstor
      DOUBLE PRECISION, SAVE :: Streambed_loss, Basinsm2gvr
      DOUBLE PRECISION, SAVE :: Sfruz_change_stor, Sfruz_tot_stor
      DOUBLE PRECISION, SAVE :: Gwflow2lakes, Basindunnian
      DOUBLE PRECISION, SAVE :: Basininfil_tot, Basininfil2pref
      DOUBLE PRECISION, SAVE :: Basinactet, Basinsnowmelt
      DOUBLE PRECISION, SAVE :: Basinfarfieldflow, Basinszfarflow
      DOUBLE PRECISION, SAVE :: Basinsrofffarflow, Basinintcpevap
      DOUBLE PRECISION, SAVE :: Basindnflow, Basinnetgwwel
!   Declared Parameters
      INTEGER, SAVE :: Id_obsrunoff, Runoff_units
!   Control Parameters
      INTEGER, SAVE :: Rpt_days, Gsf_rpt
      CHARACTER(LEN=256), SAVE :: Csv_output_file, Gsflow_output_file
      CHARACTER(LEN=256), SAVE :: Model_output_file
      END MODULE GSFSUM

!***********************************************************************
!     Main gsflow_sum routine
!***********************************************************************
      INTEGER FUNCTION gsflow_sum()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gsfsumdecl, gsfsuminit, gsfsumrun
      INTEGER, EXTERNAL :: gsfsumclean
!***********************************************************************
      gsflow_sum = 0

      IF ( Process_flag==0 ) THEN
        gsflow_sum = gsfsumrun()
      ELSEIF ( Process_flag==1 ) THEN
        gsflow_sum = gsfsumdecl()
      ELSEIF ( Process_flag==2 ) THEN
        gsflow_sum = gsfsuminit()
      ELSEIF ( Process_flag==3 ) THEN
        gsflow_sum = gsfsumclean()
      ENDIF

      END FUNCTION gsflow_sum

!***********************************************************************
!     gsfsumdecl - set up basin summary parameters
!   Declared Parameters
!     id_obsrunoff, runoff_units
!   Declared Control Parameters
!     rpt_days, csv_output_file, gsflow_output_file, model_output_file
!***********************************************************************
      INTEGER FUNCTION gsfsumdecl()
      USE GSFSUM
      USE PRMS_MODULE, ONLY: Version_gsflow_sum, Gsflow_sum_nc
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
! Local Variables
      INTEGER :: n
!***********************************************************************
      gsfsumdecl = 1

      Version_gsflow_sum =
     &'$Id: gsflow_sum.f 4217 2012-02-24 22:45:55Z rsregan $'
      Gsflow_sum_nc = INDEX( Version_gsflow_sum, 'Z' )
      n = INDEX( Version_gsflow_sum, '.f' ) + 1
      IF ( declmodule(Version_gsflow_sum(6:n), PROCNAME,
     +     Version_gsflow_sum(n+2:Gsflow_sum_nc))/=0 ) STOP

      IF ( declvar(MODNAME, 'basinsrofffarflow', 'one', 1, 'double',
     &     'Volumetric flow rate of PRMS surface runoff'//
     &     ' leaving land surface as far-field flow',
     &     'L3/T', Basinsrofffarflow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinszfarflow', 'one', 1, 'double',
     &     'Volumetric flow rate of PRMS interflow and surface runoff'//
     &     ' leaving soilzone modeled region as far-field flow',
     &     'L3/T', Basinszfarflow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinfarfieldflow', 'one', 1, 'double',
     &     'Volumetric flow rate of PRMS interflow and surface runoff'//
     &     ' leaving modeled region as far-field flow',
     &     'L3/T', Basinfarfieldflow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsoiltogw', 'one', 1, 'double',
     &     'Volumetric flow rate of direct gravity drainage from'//
     &     ' excess capillary water to the unsaturated zone',
     &     'L3/T', Basinsoiltogw)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinppt', 'one', 1, 'double',
     &     'Volumetric flow rate of precipitation on modeled region',
     &     'L3/T', Basinppt)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsnow', 'one', 1, 'double',
     &     'Volumetric flow rate of snow on modeled region',
     &     'L3/T', Basinsnow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinrain', 'one', 1, 'double',
     &     'Volumetric flow rate of rain on modeled region',
     &     'L3/T', Basinrain)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinpervet', 'one', 1, 'double',
     &     'Volumetric flow rate of evapotranspiration from pervious'//
     &     ' areas', 'L3/T',
     &     Basinpervet)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinimpervevap', 'one', 1, 'double',
     &     'Volumetric flow rate of evaporation from impervious areas',
     &     'L3/T', Basinimpervevap)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinintcpevap', 'one', 1, 'double',
     &     'Volumetric flow rate of evaporation of intercepted'//
     &     ' precipitation', 'L3/T',
     &     Basinintcpevap)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsnowevap', 'one', 1, 'double',
     &     'Volumetric flow rate of snowpack sublimation', 'L3/T',
     &     Basinsnowevap)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinlakeevap', 'one', 1, 'double',
     &     'Volumetric flow rate of evaporation from lakes',
     &     'L3/T', Basinlakeevap)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinlakeprecip', 'one', 1, 'double',
     &     'Volumetric flow rate of precipitation on lakes',
     &     'L3/T', Basinlakeprecip)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinstrmflow', 'one', 1, 'double',
     &     'Volumetric flow rate of streamflow leaving modeled region',
     &     'L3/T', Basinstrmflow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsz2gw', 'one', 1, 'double',
     &     'Potential volumetric flow rate of gravity drainage from'//
     &     ' the soil zone to the unsaturated zone (before conditions'//
     &     ' of the unsaturated and saturated zones are applied)',
     &     'L3/T', Basinsz2gw)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basingw2sz', 'one', 1, 'double',
     &     'Volumetric flow rate of ground-water discharge from the'//
     &     ' saturated zone to the soil zone', 'L3/T',
     &     Basingw2sz)/=0 ) RETURN

      IF ( declvar(MODNAME, 'uzf_recharge', 'one', 1, 'double',
     &     'Volumetric flow rate of recharge from the unsaturated'//
     &     ' zone to the saturated zone', 'L3/T',
     &     Uzf_recharge)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinseepout', 'one', 1, 'double',
     &     'Volumetric flow rate of ground-water discharge from the'//
     &     ' saturated zone to the soil zone', 'L3/T',
     &     Basinseepout)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsoilmoist', 'one', 1, 'double',
     &     'Volume of water in capillary reservoirs of the soil zone',
     &     'L3', Basinsoilmoist)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basingravstor', 'one', 1, 'double',
     &     'Volume of water in gravity reservoirs of the soil zone',
     &     'L3', Basingravstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinintcpstor', 'one', 1, 'double',
     &     'Volume of intercepted percipitation in plant-canopy'//
     &     ' reservoirs', 'L3',
     &     Basinintcpstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinimpervstor', 'one', 1, 'double',
     &     'Volume of water in impervious reservoirs', 'L3',
     &     Basinimpervstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basininterflow', 'one', 1, 'double',
     &     'Volumetric flow rate of slow interflow to streams', 'L3/T',
     &     Basininterflow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsroff', 'one', 1, 'double',
     &     'Volumetric flow rate of surface runoff to streams', 'L3/T',
     &     Basinsroff)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinhortonianlakes', 'one', 1, 'double',
     &     'Volumetric flow rate of Hortonian surface runoff to lakes',
     &     'L3/T', Basinhortonianlakes)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinlakeinsz', 'one', 1, 'double',
     &     'Volumetric flow rate of interflow and Dunnian surface'//
     &     ' runoff to lakes',
     &     'L3/T', Basinlakeinsz)/=0 ) RETURN

      IF ( declvar(MODNAME, 'strm_stor', 'one', 1, 'double',
     &     'Volume of water in streams', 'L3',
     &     Strm_stor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'lake_stor', 'one', 1, 'double',
     &     'Volume of water in lakes', 'L3',
     &     Lake_stor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'obs_strmflow', 'one', 1, 'double',
     &     'Volumetric flow rate of streamflow measured at a gaging'//
     &     ' station', 'L3/T',
     &     Obs_strmflow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinszreject', 'one', 1, 'double',
     &     'Volumetric flow rate of gravity drainage from the soil'//
     &     ' zone not accepted due to conditions in the unsaturated'//
     &     ' and saturated zones', 'L3/T',
     &     Basinszreject)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinprefstor', 'one', 1, 'double',
     &     'Volume of water stored in preferential-flow reservoirs of'//
     &     ' the soil zone', 'L3',
     &     Basinprefstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'uzf_et', 'one', 1, 'double',
     &     'Volumetric flow rate of evapotranspiration from the'//
     &     ' unsaturated and saturated zones', 'L3/T',
     &     Uzf_et)/=0 ) RETURN

      IF ( declvar(MODNAME, 'unsat_et', 'one', 1, 'double',
     &     'Volumetric flow rate of evapotranspiration from the'//
     &     ' unsaturated zone', 'L3/T',
     &     Unsat_et)/=0 ) RETURN

      IF ( declvar(MODNAME, 'sat_et', 'one', 1, 'double',
     &     'Volumetric flow rate of evapotranspiration from the'//
     &     ' saturated zone', 'L3/T',
     &     Sat_et)/=0 ) RETURN

      IF ( declvar(MODNAME, 'uzf_del_stor', 'one', 1, 'double',
     &     'Change in unsaturated-zone storage', 'L3',
     &     Uzf_del_stor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'uzf_infil', 'one', 1, 'double',
     &     'Volumetric flow rate of gravity drainage to the'//
     &     ' unsaturated and saturated zones', 'L3/T',
     &     Uzf_infil)/=0 ) RETURN

      IF ( declvar(MODNAME, 'streambed_loss', 'one', 1, 'double',
     &     'Volumetric flow rate of stream leakage to the'//
     &     ' unsaturated and saturated zones', 'L3/T',
     &     Streambed_loss)/=0 ) RETURN

      IF ( declvar(MODNAME, 'sfruz_change_stor', 'one', 1, 'double',
     &     'Change in unsaturated-zone storage under streams', 'L3',
     &     Sfruz_change_stor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'gwflow2strms', 'one', 1, 'double',
     &     'Volumetric flow rate of ground-water discharge to streams',
     &     'L3/T', Gwflow2strms)/=0 ) RETURN
     
      IF ( declvar(MODNAME, 'sfruz_tot_stor', 'one', 1, 'double',
     &     'Volume of water in the unsaturated zone beneath streams',
     &     'L3', Sfruz_tot_stor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'lakebed_loss', 'one', 1, 'double',
     &     'Volumetric flow rate of lake leakage to the unsaturated'//
     &     ' and saturated zones', 'L3/T',
     &     Lakebed_loss)/=0 ) RETURN

      IF ( declvar(MODNAME, 'lake_change_stor', 'one', 1, 'double',
     &     'Change in lake storage', 'L3',
     &     Lake_change_stor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'gwflow2lakes', 'one', 1, 'double',
     &     'Volumetric flow rate of ground-water discharge to lakes',
     &     'L3/T', Gwflow2lakes)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basininfil', 'one', 1, 'double',
     &     'Volumetric flow rate of soil infiltration including'//
     &     ' precipitation, snowmelt, and cascading Hortonian flow',
     &     'L3/T', Basininfil)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basindunnian', 'one', 1, 'double',
     &     'Volumetric flow rate of Dunnian runoff to streams', 'L3/T',
     &     Basindunnian)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsm2gvr', 'one', 1, 'double',
     &     'Volumetric flow rate of flow from capillary reservoirs to'//
     &     ' gravity reservoirs', 'L3/T',
     &     Basinsm2gvr)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basingvr2sm', 'one', 1, 'double',
     &     'Volumetric flow rate of flow from gravity reservoirs to'//
     &     ' capillary reservoirs', 'L3/T',
     &     Basingvr2sm)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basininfil_tot', 'one', 1, 'double',
     &     'Volumetric flow rate of soil infiltration into capillary'//
     &     ' reservoirs including precipitation, snowmelt, and'//
     &     ' cascading Hortonian and Dunnian runoff and interflow'//
     &     ' minus infiltration to preferential-flow reservoirs',
     &     'L3/T', Basininfil_tot)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basininfil2pref', 'one', 1, 'double',
     &     'Volumetric flow rate of soil infiltration into'//
     &     ' preferential-flow reservoirs including precipitation,'//
     &     ' snowmelt, and cascading surface runoff', 'L3/T',
     &     Basininfil2pref)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basindnflow', 'one', 1, 'double',
     &     'Volumetric flow rate of cascading Dunnian runoff and'//
     &     ' interflow to HRUs', 'L3/T',
     &     Basindnflow)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinactet', 'one', 1, 'double',
     &     'Volumetric flow rate of actual evaporation from HRUS',
     &     'L3/T', Basinactet)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsnowmelt', 'one', 1, 'double',
     &     'Volumetric flow rate of snowmelt', 'L3/T',
     &     Basinsnowmelt)/=0 ) RETURN

      IF ( declvar(MODNAME, 'ave_uzf_infil', 'one', 1, 'double',
     &     'Running average infiltration to UZF cell', 'L3',
     &     Ave_uzf_infil)/=0 ) RETURN

      IF ( declvar(MODNAME, 'cum_pweqv', 'one', 1, 'double',
     &     'Cumulative change in snowpack storage in MODFLOW units',
     &     'L3', Cum_pweqv)/=0 ) RETURN

      IF ( declvar(MODNAME, 'cum_soilstor', 'one', 1, 'double',
     &     'Cumulative change in soil storage in MODFLOW units', 'L3',
     &     Cum_soilstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'cum_uzstor', 'one', 1, 'double',
     &     'Cumulative change in unsaturated storage', 'L3',
     &     Cum_uzstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'cum_satstor', 'one', 1, 'double',
     &     'Cumulative change in saturated storage', 'L3',
     &     Cum_satstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'rate_pweqv', 'one', 1, 'double',
     &     'Change in snow pack storage in MODFLOW units', 'L3',
     &     Rate_pweqv)/=0 ) RETURN

      IF ( declvar(MODNAME, 'rate_soilstor', 'one', 1, 'double',
     &     'Change in soil storage in MODFLOW units', 'L3',
     &     Rate_soilstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'rate_uzstor', 'one', 1, 'double',
     &     'Change in unsaturated storage', 'L3',
     &     Rate_uzstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'rate_satstor', 'one', 1, 'double',
     &     'Change in saturated storage', 'L3',
     &     Rate_satstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinpweqv', 'one', 1, 'double',
     &     'Volume of water in snowpack storage', 'L3',
     &     Basinpweqv)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinsoilstor', 'one', 1, 'double',
     &     'Soil moisture storage in volume of MODFLOW units', 'L3',
     &     Basinsoilstor)/=0 ) RETURN

      IF ( declvar(MODNAME, 'basinnetgwwel', 'one', 1, 'double',
     &     'Net groundwater pumping in volume of MODFLOW units', 'L3',
     &     Basinnetgwwel)/=0 ) RETURN

      IF ( declparam(MODNAME, 'id_obsrunoff', 'one', 'integer',
     &     '0', 'bounded', 'nobs',
     &     'Index of basin outlet observed runoff station',
     &     'Index of basin outlet observed runoff station',
     &     'none')/=0 ) RETURN

      IF ( declparam(MODNAME, 'runoff_units', 'one', 'integer',
     &     '0', '0', '1',
     &     'Measured runoff units',
     &     'Measured runoff units (0=cfs; 1=cms)',
     &     'none')/=0 ) RETURN

      gsfsumdecl = 0

      END FUNCTION gsfsumdecl

!***********************************************************************
!     gsfsuminit - Initialize basinsum module - get parameter values
!                set to zero
!***********************************************************************
      INTEGER FUNCTION gsfsuminit()
      USE GSFSUM
      USE GSFCONVERT, ONLY: Acre_inches_to_mfl3
      USE GWFLAKMODULE, ONLY: TOTSTOR_LAK
      USE GWFSFRMODULE, ONLY: IRTFLG
      USE GLOBAL, ONLY: IUNIT
      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_BASIN, ONLY: Basin_area_inv, Timestep
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Basin_ssstor
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL GSF_PRINT
!***********************************************************************
      gsfsuminit = 1

      IF ( getparam(MODNAME, 'id_obsrunoff', 1, 'integer',
     &     Id_obsrunoff)/=0 ) RETURN
      IF ( Id_obsrunoff.EQ.0 ) Id_obsrunoff = 1

      IF ( getparam(MODNAME, 'runoff_units', 1, 'integer',
     &     Runoff_units)/=0 ) RETURN

      IF ( Print_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='gsflow_sum.wbal')
        WRITE (BALUNT, 9001)
      ENDIF

      Basin_convert = Acre_inches_to_mfl3/Basin_area_inv

      Have_wells = 0
      IF ( IUNIT(2)>0 .OR. IUNIT(50)>0 .OR. IUNIT(51)>0 .OR.
     &     IUNIT(52)>0 ) Have_wells = 1

!  Set the volume budget indicies to -1 anytime "init" is called.
!  This will make "run" figure out the vbnm order.
      Vbnm_index = -1

!  Put a header on the output file when the model starts.
      CALL GSF_PRINT()

!  Initialize cumulative GSF report variables to 0.0
      IF ( Timestep==0 ) THEN
      Cumvol_precip = 0.0D0
      Cumvol_strmin = 0.0D0
      Cumvol_gwbndin = 0.0D0
      Cumvol_wellin = 0.0D0
      Cumvol_et = 0.0D0
      Cumvol_strmot = 0.0D0
      Cumvol_gwbndot = 0.0D0
      Cumvol_wellot = 0.0D0
      Cumvol_farout = 0.0D0
      Cum_delstore = 0.0D0
      Cum_surfstor = 0.0D0
      Cum_soilstor = 0.0D0
      Cum_uzstor = 0.0D0
      Cum_satstor = 0.0D0
      Cum_pweqv = 0.0D0
      Rate_soilstor = 0.0D0
      Rate_uzstor = 0.0D0
      Rate_satstor = 0.0D0
      Rate_pweqv = 0.0D0
      Rate_farout = 0.0D0
      Uzf_infil = 0.0D0
      Ave_uzf_infil = 0.0D0
      Lakebed_loss = 0.0D0
      Lake_change_stor = 0.0D0
      Basinpweqv = 0.0D0
      Basinppt = 0.0D0
      Basinpervet = 0.0D0
      Basinimpervevap = 0.0D0
      Basinimpervstor = 0.0D0
      Basinintcpevap = 0.0D0
      Basinintcpstor = 0.0D0
      Basininterflow = 0.0D0
      Basinsnowevap = 0.0D0
      Basinlakeevap = 0.0D0
      Basinlakeprecip = 0.0D0
      Basinlakeinsz = 0.0D0
      Basinstrmflow = 0.0D0
      Basinsz2gw = 0.0D0
      Basingw2sz = 0.0D0
      Uzf_recharge = 0.0D0
      Basinseepout = 0.0D0
!     Basingwstor not computed, was the PRMS GWR storage which
!     is not available
      Basingwstor = 0.0D0
      Basinsroff = 0.0D0
      Basinhortonianlakes = 0.0D0
      Basinhortonian = 0.0D0
      Lake_stor = 0.0D0
      IF ( IUNIT(22).GT.0 ) Lake_stor = TOTSTOR_LAK
      Obs_strmflow = 0.0D0
      Basinnetgwwel = 0.0D0
      Basinprefflow = 0.0D0
      Basinprefstor = 0.0D0
      Basinszreject = 0.0D0
      Basingravstor = 0.0D0
      Basindnflow = 0.0D0
      Basinswaleet = 0.0D0
      Uzf_et = 0.0D0
      Unsat_et = 0.0D0
      Sat_et = 0.0D0
      Uzf_del_stor = 0.0D0
      Streambed_loss = 0.0D0
      Sfruz_change_stor = 0.0D0
      Gwflow2strms = 0.0D0
      Sfruz_tot_stor = 0.0D0
      Gwflow2lakes = 0.0D0
      Lakebed_loss = 0.0D0
      Basinslowflow = 0.0D0
      Basininfil = 0.0D0
      Basinrain = 0.0D0
      Basinsnow = 0.0D0
      Basinsnowmelt = 0.0D0
      Basindunnian = 0.0D0
      Basinsm2gvr = 0.0D0
      Basingvr2sm = 0.0D0
      Basingvr2pfr = 0.0D0
      Basininfil_tot = 0.0D0
      Basininfil2pref = 0.0D0
      Basinfarfieldflow = 0.0D0
      Basinszfarflow = 0.0D0
      Basinsoilstor = 0.0D0
      Basinsoilmoist = 0.0D0
      Basinsoiltogw = 0.0D0
      Basinactet = 0.0D0
      Strm_stor = 0.0D0
      Basinsrofffarflow = 0.0D0
      Last_basinintcpstor = 0.0D0
      Last_basinimpervstor = 0.0D0
      Last_basinpweqv = 0.0D0
! Added lake variables
      Rate_lakin = 0.0D0
      Rate_lakot = 0.0D0
      Cumvol_lakin = 0.0D0
      Cumvol_lakot = 0.0D0
      Rate_lakestor = 0.0D0
      Cum_lakestor = 0.0D0
      Basinnetgwwel = 0.0D0
!      Cumvol_lakeppt = 0.0D0
!      Cumvol_lakeevap = 0.0D0
!      Cumvol_uzfet = 0.0D0
      ENDIF

      IF ( IRTFLG.GT.0 ) CALL MODFLOW_SFR_GET_STORAGE

      CALL BASIN_GET_STORAGE
      Last_basinsoilmoist = Basinsoilmoist
      Last_basingravstor = Basingravstor
      Last_basinprefstor = Basinprefstor
      Last_basin_soil_moist = Basin_soil_moist
      Last_basin_ssstor = Basin_ssstor

      Rpt_count = 0

      gsfsuminit = 0

 9001 FORMAT('    Date         SZ Bal    lakeinsz     Dunnian',
     &       '    Slowflow    prefflow      pervet       infil',
     &       '   soilmoist Last_soilmoist gravstor Last_gravstor',
     &       '     gw2sz       sz2gw    szreject    soiltogw',
     &       '     farflow  farflowtot')
      END FUNCTION gsfsuminit

!***********************************************************************
!     gsfsumrun - Computes summary values
!***********************************************************************
      INTEGER FUNCTION gsfsumrun()
      USE GSFSUM
      USE GSFCONVERT, ONLY: Mfl3t_to_cfs, Mfl3_to_ft3
      USE GSFPRMS2MF, ONLY: Net_sz2gw
      USE GSFBUDGET, ONLY: Gw_inout, Gw_bnd_in, Gw_bnd_out, Well_in,
     &    Well_out, Stream_inflow, Basin_gw2sm, Sat_change_stor,
     &    Stream_leakage, Basin_szreject, Unsat_store, Sat_store
!      USE GSFPRMS2MF, ONLY: Basin_reach_latflow
      USE GSFBUDGET, ONLY: Basin_actetgw
      USE GWFUZFMODULE, ONLY: UZTSRAT
      USE GWFSFRMODULE, ONLY: SFRUZBD, STRMDELSTOR_RATE, SFRRATIN, 
     &    SFRRATOUT, IRTFLG
      USE GWFLAKMODULE, ONLY: TOTGWIN_LAK, TOTGWOT_LAK, TOTDELSTOR_LAK,
     &    TOTSTOR_LAK, TOTWTHDRW_LAK, TOTRUNF_LAK, TOTSURFIN_LAK,
     &    TOTSURFOT_LAK, TOTEVAP_LAK, TOTPPT_LAK
      USE GSFMODFLOW, ONLY: KKSTP, KKPER, KKITER, Have_lakes, Maxgziter
      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_OBS, ONLY: Runoff, Nowtime, Nobs
      USE PRMS_CASCADE, ONLY: Outflow_flg
      USE PRMS_BASIN, ONLY: Basin_cfs, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Basin_ppt, Basin_rain, Basin_snow
      USE PRMS_FLOWVARS, ONLY: Basin_perv_et, Basin_swale_et,
     &    Basin_lakeevap, Basin_soil_to_gw, Basin_ssflow, Basin_actet,
     &    Basin_soil_moist, Basin_ssstor, Basin_imperv_evap,
     &    Basin_sroff, Basin_hortonian, Basin_hortonian_lakes,
     &    Basin_infil, Strm_farfield, Basin_sroff_farflow
      USE PRMS_SNOW, ONLY: Basin_snowevap, Basin_snowmelt
      USE PRMS_INTCP, ONLY: Basin_intcp_evap
      USE PRMS_SOILZONE, ONLY: Basin_lakeprecip, Basin_dunnian,
     &    Basin_slowflow, Basin_prefflow, Basin_lakeinsz,
     &    Basin_capwaterin, Basin_pref_flow_in, Basin_sz2gw,
     &    Basin_szfarflow, Basin_sm2gvr, Basin_gvr2sm,
     &    Basin_gvr2pfr, Basin_dunnian
      IMPLICIT NONE
! Local variables
      INTEGER :: year, mo, day
      REAL :: obsq_cfs, obsq_cms
!     REAL :: gw_out, basinreachlatflowm3
      DOUBLE PRECISION :: sz_bal, et, rnf, gvf, szin, szout, szdstor
!***********************************************************************
      gsfsumrun = 1

!*****Evapotranspiration
      ! basin_actet includes land HRU and lake ET

      Uzf_et = UZTSRAT(2) + UZTSRAT(7)
      Unsat_et = UZTSRAT(2)
      Sat_et = UZTSRAT(7)
!      Cumvol_uzfet = Cumvol_uzfet + Uzf_et
!      et = Basin_actetgw*Basin_convert
!      IF (abs(uzf_et-et)>0.0001 )
!    +  print *, 'uzfet',uzf_et-et,uzf_et,unsat_et,sat_et,
!    + et,basin_actetgw,basin_actet,basin_perv_et

! convert PRMS variables acre-inches over the basin area (depth)
! to modflow length cubed (total volume)
      Basinpervet = Basin_perv_et*Basin_convert
      Basinswaleet = Basin_swale_et*Basin_convert
      Basinimpervevap = Basin_imperv_evap*Basin_convert
      Basinintcpevap = Basin_intcp_evap*Basin_convert
      Basinsnowevap = Basin_snowevap*Basin_convert
      Basinlakeevap = Basin_lakeevap*Basin_convert
!      IF ( Have_lakes==1 ) Basinlakeevap = TOTEVAP_LAK
      ! sanity check
!      IF ( Have_lakes==1 ) THEN
!        IF ( ABS(Basinlakeevap+TOTEVAP_LAK)>0.0001 ) PRINT *, 
!     &       'LAKE EVAP PROBLEM, MF lake evap not equal PRMS lake evap',
!     &       Basinlakeevap+TOTEVAP_LAK, Basinlakeevap, TOTEVAP_LAK
!      ENDIF
!      Basinactet = Basin_actet*Basin_convert + Uzf_et + Basinlakeevap
!rsr, 3/16/2010 basinactet includes PRMS lake evaporation = TOTEVAP_LAK
!      Basinactet = Basin_actet*Basin_convert + Uzf_et
      Basinactet = Basin_actet*Basin_convert

! STREAMFLOW

! convert basin_cfs from cfs over to modflow l3/t (volume per time step)
      Basinstrmflow = Basin_cfs/Mfl3t_to_cfs

      IF ( Nobs.LT.1 ) THEN
        obsq_cfs = -1.0
      ELSE
        IF ( Runoff_units.EQ.1 ) THEN
          obsq_cms = Runoff(Id_obsrunoff)
          obsq_cfs = obsq_cms*Mfl3_to_ft3
        ELSE
          obsq_cfs = Runoff(Id_obsrunoff)
        ENDIF
      ENDIF
      Obs_strmflow = obsq_cfs/Mfl3t_to_cfs

!rsr  basinreachlatflowm3 = Basin_reach_latflow/Mfl3t_to_cfs

! PRECIPITATION
      !Basinppt includes precipitation on lakes
      Basinppt = Basin_ppt*Basin_convert
!rsr, 3/16/2010 do not subtract lake evaporation, not included in lake in below
      Basinrain = Basin_rain*Basin_convert
      Basinsnow = Basin_snow*Basin_convert
      Basinlakeprecip = Basin_lakeprecip*Basin_convert

! SOIL/RUNOFF TOTALS

      !flows to streams
      Basinsroff = Basin_sroff*Basin_convert  !Hortonian and Dunnian
      Basinhortonian = Basin_hortonian*Basin_convert
      Basindunnian = Basin_dunnian*Basin_convert
      Basininterflow = Basin_ssflow*Basin_convert !slow + pref
      Basinslowflow = Basin_slowflow*Basin_convert
      Basinprefflow = Basin_prefflow*Basin_convert
      !flows to lakes
      Basinhortonianlakes = Basin_hortonian_lakes*Basin_convert
      !interflow + Dunnian soilzone
      Basinlakeinsz = Basin_lakeinsz*Basin_convert

! SOIL/GW TOTALS
      !flows to soilzone
      !to capillary and preferential
      Basininfil = Basin_infil*Basin_convert
      Basingw2sz = Basin_gw2sm*Basin_convert !to gravity
      !infil plus cascading flow to capillary
      Basininfil_tot = Basin_capwaterin*Basin_convert
      !portion of infil to preferential
      Basininfil2pref = Basin_pref_flow_in*Basin_convert
      Basinsnowmelt = Basin_snowmelt*Basin_convert

      !flows from soilzone
      Basinsoiltogw = Basin_soil_to_gw*Basin_convert
      Basinsz2gw = Basin_sz2gw*Basin_convert
      Basinszreject = Basin_szreject*Basin_convert
      Basinszfarflow = Basin_szfarflow*Basin_convert

      !internal soilzone flows
      Basinsm2gvr = Basin_sm2gvr*Basin_convert !> field capacity
      Basingvr2sm = Basin_gvr2sm*Basin_convert !replenish soil moist
      Basingvr2pfr = Basin_gvr2pfr*Basin_convert !>pref_flow threshold
      !cascading slow, pref, and Dunnian
      Basindnflow = Basin_dunnian*Basin_convert

      !flows from PRMS that go outside of basin and not to MODFLOW
      IF ( Outflow_flg==1 ) THEN
        Basinfarfieldflow = Strm_farfield/Mfl3t_to_cfs
        !cascading surface runoff
        Basinsrofffarflow = Basin_sroff_farflow*Basin_convert
      ENDIF
      
!  Stuff from MODFLOW
      IF ( Vbnm_index(1).EQ.-1 ) CALL MODFLOW_VB_DECODE(Vbnm_index)

      Uzf_recharge = UZTSRAT(3)
      !?? doesn't match basin_gw2sm from budget
      Basinseepout = UZTSRAT(5)
      Uzf_infil = UZTSRAT(1)
      Uzf_del_stor = UZTSRAT(4)

      Streambed_loss = SFRRATIN
      Sfruz_change_stor = SFRUZBD(5)
      Gwflow2strms = SFRRATOUT
      Sfruz_tot_stor = SFRUZBD(10)

      IF ( Timestep>1 ) THEN
        Ave_uzf_infil = (Ave_uzf_infil*(Timestep-1)) + Basin_sz2gw -
     &                  Basin_szreject + Basin_soil_to_gw
      ELSE
        Ave_uzf_infil = Basin_sz2gw - Basin_szreject + Basin_soil_to_gw
      ENDIF
      Ave_uzf_infil = Ave_uzf_infil/Timestep

      IF ( Have_lakes==1 ) THEN
        Lake_stor = TOTSTOR_LAK
        Gwflow2lakes = TOTGWIN_LAK 
        Lakebed_loss = TOTGWOT_LAK
        Lake_change_stor = TOTDELSTOR_LAK
      ENDIF

      IF ( IRTFLG.GT.0 ) CALL MODFLOW_SFR_GET_STORAGE

      CALL BASIN_GET_STORAGE

! Moved Well calculations up here for printing to CSV file
      Cumvol_wellin = Cumvol_wellin + Well_in
      Rate_wellin = Well_in
      Cumvol_wellot = Cumvol_wellot + Well_out
      Rate_wellot = Well_out
      Basinnetgwwel = Cumvol_wellin - Cumvol_wellot

      year = Nowtime(1)
      mo = Nowtime(2)
      day = Nowtime(3)

      IF ( Print_debug==1 ) THEN
        et = Basin_perv_et + Basin_snowevap + Basin_imperv_evap +
     &       Basin_intcp_evap + Basin_lakeevap + Basin_swale_et +
     &       Basin_actetgw
        IF ( ABS(Basin_actet-et)>ERRCHK ) THEN
          WRITE (BALUNT, *) 'ET', Basin_actet - et, Basin_actet, et
          WRITE (BALUNT, *) 'ET', Basin_perv_et, Basin_snowevap,
     &                      Basin_imperv_evap, Basin_intcp_evap,
     &                      Basin_lakeevap, Unsat_et, Sat_et, 
     &                      Basin_swale_et, Basin_actetgw
        ENDIF

        rnf = Basin_hortonian + Basin_dunnian - Basin_sroff
        IF ( ABS(rnf)>ERRCHK ) WRITE (BALUNT, *) 'runoff', rnf,
     &       Basin_hortonian, Basin_dunnian, Basin_sroff
        gvf = Basin_slowflow + Basin_prefflow - Basin_ssflow
        IF ( ABS(gvf)>ERRCHK ) WRITE (BALUNT, *) 'gravflow', gvf,
     &       Basin_slowflow, Basin_prefflow, Basin_ssflow

        szin = Basin_infil + Basin_gw2sm + Basin_szreject
        szdstor = Last_basin_soil_moist + Last_basin_ssstor
     &            - Basin_soil_moist - Basin_ssstor
        szout = Basin_sz2gw + Basin_ssflow + Basin_lakeinsz +
     &          Basin_dunnian + Basin_perv_et + Basin_szfarflow
     &          + Basin_soil_to_gw + Basin_swale_et
        IF ( Basin_soil_moist>0.0D0 ) THEN
          IF ( ABS(szin-szout+szdstor)>ERRCHK ) THEN
            WRITE (BALUNT, 9002) year, mo, day
            WRITE (BALUNT, *) 'SZ flow', szin-szout+szdstor, szin,
     &                        szout, szdstor
            WRITE (BALUNT, *) 'SZ flow', Basin_infil, Basin_gw2sm,
     &                        Basin_szreject, Last_basin_soil_moist,
     &                        Last_basin_ssstor, Basin_soil_moist,
     &                        Basin_ssstor, Basin_sz2gw, Basin_ssflow,
     &                        Basin_lakeinsz, Basin_dunnian,
     &                        Basin_perv_et, Basin_szfarflow,
     &                        Basin_soil_to_gw, Strm_farfield,
     &                        Basin_sroff_farflow, Basin_swale_et
            WRITE (BALUNT, *) KKITER, Maxgziter
          ENDIF
        ENDIF
        Last_basin_soil_moist = Basin_soil_moist
        Last_basin_ssstor = Basin_ssstor

        sz_bal = Basinlakeinsz + Basindunnian + Basinslowflow +
     &           Basinprefflow + Basinpervet - Basininfil +
     &           Basinsoilmoist - Last_basinsoilmoist +
     &           Basingravstor - Last_basingravstor - Basingw2sz -
     &           Basinszreject + Basinsz2gw + Basinsoiltogw +
     &           Basinszfarflow + Basinswaleet
        IF ( ABS(sz_bal)/Basinsoilmoist>ERRCHK )
     &       WRITE (BALUNT, *) 'Possible water balance problem'
        WRITE (BALUNT, 9002) year, mo, day, sz_bal, Basinlakeinsz,
     &         Basindunnian, Basinslowflow, Basinprefflow,
     &         Basinpervet, Basininfil, Basinsoilmoist,
     &         Last_basinsoilmoist, Basingravstor,
     &         Last_basingravstor, Basingw2sz, Basinsz2gw,
     &         Basinszreject, Basinsoiltogw,
     &         Basinszfarflow, Basinfarfieldflow, Basinswaleet
      ENDIF

      IF ( Gsf_rpt.EQ.1 ) THEN
        WRITE (Balance_unt, 9001) mo, day, year, Basinppt, Basinpervet,
     &         Basinimpervevap, Basinintcpevap, Basinsnowevap,
     &         Basinstrmflow, Basinsz2gw, Basingw2sz, Gw_inout,
     &         Stream_leakage, Uzf_recharge, Basinseepout, Sat_store,
     &         Unsat_store, Basinsoilmoist, Basingravstor, Basingwstor,
     &         Basinintcpstor, Basinimpervstor, Basinpweqv,
     &         Basininterflow, Basinsroff, Strm_stor, Lake_stor,
     &         Obs_strmflow, Basinszreject, Basinprefstor, Uzf_et,
     &         Uzf_infil, Uzf_del_stor, Net_sz2gw, Sat_change_stor,
     &         Streambed_loss, Sfruz_change_stor, Gwflow2strms,
     &         Sfruz_tot_stor, Lakebed_loss, Lake_change_stor,
     &         Gwflow2lakes, Basininfil, Basindunnian, Basinhortonian,
     &         Basinsm2gvr, Basingvr2sm, Basininfil_tot,
     &         Basininfil2pref, Basindnflow, Basinactet, Basinsnowmelt,
     &         Basinhortonianlakes, Basinlakeinsz, Basinlakeevap,
     &         Basinlakeprecip, Basinfarfieldflow, Basinsoiltogw,
     &         Basinszfarflow, Basinsrofffarflow, Basinswaleet,
     &         Unsat_et, Sat_et, Basinnetgwwel, KKITER
!     &        basinreachlatflowm3, Basinrain,
!     &        Basinsnow, Basingvr2pfr, Basinslowflow, Basinprefflow
      ENDIF

!     DANGER strmin set to zero
!  RGN I think I fixed strmin
      Cumvol_precip = Cumvol_precip + Basinppt
      Rate_precip = Basinppt
! RGN change Cumvol_strmin to include specified inflows
      Rate_strmin = Stream_inflow
      Cumvol_strmin = Cumvol_strmin + Rate_strmin
      Cumvol_gwbndin = Cumvol_gwbndin + Gw_bnd_in
      Rate_gwbndin = Gw_bnd_in
      Rate_et = Basinactet
      Cumvol_et = Cumvol_et + Rate_et
      Rate_strmot = Basinstrmflow
      Cumvol_strmot = Cumvol_strmot + Rate_strmot
      Cumvol_gwbndot = Cumvol_gwbndot + Gw_bnd_out
      Rate_gwbndot = Gw_bnd_out
      IF ( Outflow_flg.EQ.1 ) THEN
        Rate_farout = Basinfarfieldflow
        Cumvol_farout = Cumvol_farout + Rate_farout
      ENDIF
 ! RGN added specified lake inflow/outflow and storage change
      IF ( Have_lakes==1 ) THEN
!        IF ( TOTWTHDRW_LAK.GT.0.0 ) THEN
!          Rate_lakin = 0.0D0
!          Rate_lakot = TOTWTHDRW_LAK
!        ELSE
!          Rate_lakot = 0.0D0
!          Rate_lakin = TOTWTHDRW_LAK
!        END IF
       !rsr, 3/16/2010 TOTEVAP_LAK included in basinactet
        Rate_lakot = -TOTSURFOT_LAK - TOTGWOT_LAK
     +               - TOTEVAP_LAK - TOTWTHDRW_LAK
!        Cumvol_lakeevap = Cumvol_lakeevap + TOTEVAP_LAK
        Rate_lakin = TOTRUNF_LAK + TOTSURFIN_LAK
     +               + TOTGWIN_LAK + TOTPPT_LAK
!        Cumvol_lakeppt = Cumvol_lakeppt + TOTPPT_LAK
        Cumvol_lakot = Cumvol_lakot + Rate_lakot
        Cumvol_lakin = Cumvol_lakin + Rate_lakin
        Rate_lakestor = Rate_lakin - Rate_lakot
        Cum_lakestor = Cum_lakestor + Rate_lakestor
!        Cum_lakestor = TOTSTOR_LAK
!        print *, totsurfot_lak, totevap_lak, totgwot_lak, totrunf_lak
!        print *, totsurfin_lak, totgwin_lak
      END IF
      Rate_pweqv = Basinpweqv - Last_basinpweqv
      Cum_pweqv = Cum_pweqv + Rate_pweqv

      Rate_surfstor = Basinintcpstor - Last_basinintcpstor +
     &                Basinimpervstor - Last_basinimpervstor +
     &                Rate_pweqv
      Cum_surfstor = Cum_surfstor + Rate_surfstor

      Rate_soilstor = Basinsoilmoist - Last_basinsoilmoist +
     &                Basingravstor - Last_basingravstor  !grav + pref
      Cum_soilstor = Cum_soilstor + Rate_soilstor

      Rate_uzstor = Uzf_del_stor + Sfruz_change_stor
      Cum_uzstor = Cum_uzstor + Rate_uzstor

      Rate_satstor = Sat_change_stor
      Cum_satstor = Cum_satstor + Rate_satstor

      Rate_delstore = Rate_surfstor + Rate_soilstor + Rate_satstor +
     &                Rate_uzstor + Rate_lakestor + STRMDELSTOR_RATE
      Cum_delstore = Cum_delstore + Rate_delstore

      Rpt_count = Rpt_count + 1
      IF ( Rpt_count.EQ.Rpt_days ) THEN  !rpt_days default = 7
        CALL GSFSUMREPORT(year, mo, day, Timestep, KKSTP, KKPER)
        Rpt_count = 0
      ENDIF

!  Save old values before computation of new ones
      Last_basinintcpstor = Basinintcpstor
      Last_basinimpervstor = Basinimpervstor
      Last_basinpweqv = Basinpweqv
      Last_basinsoilmoist = Basinsoilmoist
      Last_basingravstor = Basingravstor
      Last_basinprefstor = Basinprefstor

      gsfsumrun = 0

 9001 FORMAT (2(I2.2, '/'), I4, 61(',', E15.7), ',', I5)
 9002 FORMAT (I5, 2('/', I2.2), F12.3, 17(F12.0))
      END FUNCTION gsfsumrun

!***********************************************************************
!     gsfsumclean - Computes summary values
!***********************************************************************
      INTEGER FUNCTION gsfsumclean()
      USE GSFSUM, ONLY: Balance_unt, Gsf_unt, Gsf_rpt, BALUNT
      USE PRMS_MODULE, ONLY: Print_debug
      IMPLICIT NONE
!***********************************************************************
      gsfsumclean = 1
      IF ( Print_debug==1 ) CLOSE (BALUNT)
      IF ( Gsf_rpt==1 ) CLOSE (Balance_unt)
      CLOSE (Gsf_unt)
      gsfsumclean = 0
      END FUNCTION gsfsumclean

!***********************************************************************
! Print headers for tables
!***********************************************************************
      SUBROUTINE GSF_PRINT()
      USE GSFSUM, ONLY: Balance_unt, Gsf_unt, Csv_output_file, Rpt_days,
     &    Gsflow_output_file, Model_output_file, Gsf_rpt
      USE GSFMODFLOW, ONLY: Logunt, Have_lakes
      USE PRMS_MODULE, ONLY: Print_debug
      IMPLICIT NONE
      INTEGER, EXTERNAL :: control_integer, control_string
      EXTERNAL GSF_HEADERS
      INTRINSIC CHAR, INDEX
! Local Variables
      LOGICAL :: opend
      INTEGER :: nc
!***********************************************************************
      IF ( control_integer(Gsf_rpt, 'gsf_rpt')/=0 ) RETURN
      IF ( Gsf_rpt.EQ.1 ) THEN  !gsf_rpt default = 1
        opend = .TRUE.
        Balance_unt = 300
        DO WHILE ( opend )
          Balance_unt = Balance_unt + 1
          INQUIRE (UNIT=Balance_unt, OPENED=opend)
        ENDDO

        IF ( control_string(Csv_output_file, 'csv_output_file')/=0 )
     &       RETURN
        IF ( Csv_output_file(:1).EQ.' ' .OR.
     &       Csv_output_file(:1).EQ.CHAR(0) )
     &       Csv_output_file = 'gsflow.csv'

        OPEN (UNIT=Balance_unt, FILE=Csv_output_file)
      ENDIF
 
! Open the GSF volumetric balance report file
      opend = .TRUE.
      Gsf_unt = Balance_unt
      DO WHILE ( opend )
        Gsf_unt = Gsf_unt + 1
        INQUIRE (UNIT=Gsf_unt, OPENED=opend)
      ENDDO

      IF ( control_integer(Rpt_days, 'rpt_days')/=0 ) RETURN
      IF ( Print_debug>-1 )
     &     PRINT *, 'Water Budget print frequency is:', Rpt_days
      WRITE (Logunt, *) 'Water Budget print frequency is:', Rpt_days
      IF ( control_string(Gsflow_output_file, 'gsflow_output_file')
     &     /=0 ) RETURN
      IF ( Gsflow_output_file(:1).EQ.' ' .OR.
     &     Gsflow_output_file(:1).EQ.CHAR(0) )
     &     Gsflow_output_file = 'gsflow.out'

      OPEN (UNIT=Gsf_unt, FILE=Gsflow_output_file)
      nc = INDEX(Gsflow_output_file,CHAR(0)) - 1
      IF ( nc.EQ.0 ) nc = 256
      PRINT *, 'Writing GSFLOW Water Budget File: ',
     &         Gsflow_output_file(:nc)
      WRITE (Logunt, *) 'Writing GSFLOW Water Budget File: ',
     &                  Gsflow_output_file(:nc)
      IF ( Gsf_rpt.EQ.1 ) THEN
        nc = INDEX(Csv_output_file,CHAR(0)) - 1
        IF ( nc.EQ.0 ) nc = 256
        PRINT *, 'Writing GSFLOW CSV File: ', Csv_output_file(:nc)
        WRITE (Logunt, *) 'Writing GSFLOW CSV File: ',
     &                    Csv_output_file(:nc)
        CALL GSF_HEADERS()
      ENDIF

      IF ( control_string(Model_output_file, 'model_output_file')/=0 )
     &     RETURN
      nc = INDEX(Model_output_file,CHAR(0)) - 1
      IF ( nc.EQ.0 ) nc = 256
      PRINT 9001, ' Writing PRMS Water Budget File: ',
     &            Model_output_file(:nc)
      WRITE (Logunt, 9001) ' Writing PRMS Water Budget File: ',
     &                     Model_output_file(:nc)

 9001 FORMAT (A, A, /)
      END SUBROUTINE GSF_PRINT

!***********************************************************************
! Print headers for reports
!***********************************************************************
      SUBROUTINE GSF_HEADERS()
      USE GSFSUM, ONLY: Balance_unt
      IMPLICIT NONE
!***********************************************************************
      ! uzf_tot_stor = unsat_store, modflow_tot_stor = sat_store
      WRITE (Balance_unt, 9001)
 9001 FORMAT ('Date,basinppt,basinpervet,basinimpervevap',
     &        ',basinintcpevap,basinsnowevap,basinstrmflow',
     &        ',basinsz2gw,basingw2sz,gw_inout,stream_leakage',
     &        ',uzf_recharge,basinseepout,sat_stor,unsat_stor',
     &        ',basinsoilmoist,basingravstor,basingwstor',
     &        ',basinintcpstor,basinimpervstor,basinpweqv',
     &        ',basininterflow,basinsroff,strm_stor,lake_stor',
     &        ',obs_strmflow,basinszreject,basinprefstor',
     &        ',uzf_et,uzf_infil,uzf_del_stor,net_sz2gw',
     &        ',sat_change_stor,streambed_loss,sfruz_change_stor',
     &        ',gwflow2strms,sfruz_tot_stor,lakebed_loss',
     &        ',lake_change_stor,gwflow2lakes,basininfil,basindunnian',
     &        ',basinhortonian,basinsm2gvr,basingvr2sm,basininfil_tot',
     &        ',basininfil2pref,basindnflow,basinactet,basinsnowmelt',
     &        ',basinhortonianlakes,basinlakeinsz,basinlakeevap',
     &        ',basinlakeprecip,basinfarfieldflow,basinsoiltogw',
     &        ',basinszfarflow,basinsrofffarflow,basinswaleet',
     &        ',unsat_et,sat_et,basinnetgwwel,kkiter')

      END SUBROUTINE GSF_HEADERS

!***********************************************************************
! Figure out the total basin_gsfstor
!***********************************************************************
      SUBROUTINE BASIN_GET_STORAGE()
      USE GSFSUM
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Basin_ssstor,
     &    Basin_imperv_stor
      USE PRMS_INTCP, ONLY: Basin_intcp_stor
      USE PRMS_SNOW, ONLY: Basin_pweqv
      USE PRMS_SOILZONE, ONLY: Basin_pref_stor
      USE GSFBUDGET, ONLY: Sat_store, Unsat_store
      IMPLICIT NONE
!***********************************************************************
! LAND SURFACE STORAGE
      Basinintcpstor = Basin_intcp_stor*Basin_convert
      Basinimpervstor = Basin_imperv_stor*Basin_convert
      Basinpweqv = Basin_pweqv*Basin_convert

! SOIL STORAGE
      Basinsoilmoist = Basin_soil_moist*Basin_convert
      Basingravstor = Basin_ssstor*Basin_convert
      Basinsoilstor = Basingravstor + Basinsoilmoist
      Basinprefstor = Basin_pref_stor*Basin_convert

! MODFLOW STORAGE
!rsr, 4/18/2010 Basingravstor includes gravity and preferential
      Basin_gsfstor = Sat_store + Unsat_store + Basinsoilmoist +
     &                Basingravstor + Basinintcpstor +
     &                Basinpweqv + Basinimpervstor + Lake_stor +
     &                Strm_stor

      END SUBROUTINE BASIN_GET_STORAGE

!-------SUBROUTINE GSFSUMREPORT
      SUBROUTINE GSFSUMREPORT(Year, Mo, Day, Nstep, Kkstp, Kkper)
!***********************************************************************
!     PRINTS VOLUMETRIC BUDGET FOR ENTIRE GSFLOW MODEL
!***********************************************************************
      USE GSFSUM
      USE GWFSFRMODULE, ONLY: STRMDELSTOR_RATE, STRMDELSTOR_CUM, IRTFLG
      USE GSFMODFLOW, ONLY: KKITER, Have_lakes
      USE PRMS_CASCADE, ONLY: Outflow_flg
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL GSFFMTNUM
! Arguments
      INTEGER :: Kkper, Kkstp, Year, Mo, Day, Nstep
! Local Variables
      DOUBLE PRECISION :: cumvol_in, cumvol_out, cumdiff, rate_in
      DOUBLE PRECISION :: ratediff, cum_error, rate_error, cum_percent
      DOUBLE PRECISION :: rate_out, rate_percent, temp
      CHARACTER(LEN=18) :: text1, text2, text3, text4, text5, text6
      CHARACTER(LEN=18) :: text7, text8, text9, text10, text11, text12
      CHARACTER(LEN=18) :: val1, val2
!***********************************************************************
      text1 = '     PRECIPITATION'
      text2 = '        STREAMFLOW'
      text3 = '  GW BOUNDARY FLOW'
      text4 = '             WELLS'
      text5 = 'EVAPOTRANSPIRATION'
      text6 = '      LAND SURFACE'
      text7 = '         SOIL ZONE'
      text8 = '  UNSATURATED ZONE'
      text9 = '    SATURATED ZONE'
      text10 ='             LAKES'
      text11 ='           STREAMS'
      text12 =' FAR-FIELD OUTFLOW'
      WRITE (Gsf_unt, 9001) Mo, Day, Year, Nstep, Kkper, Kkstp, KKITER
!
!1------PRINT CUMULATIVE VOLUMES AND RATES FOR INFLOW.
      WRITE (Gsf_unt, 9002)
!
!1A-----PRECIPITATION.
      CALL GSFFMTNUM(Cumvol_precip, val1)
      CALL GSFFMTNUM(Rate_precip, val2)
      WRITE (Gsf_unt, 9003) text1, val1, text1, val2
!1B-----STREAMFLOW.
      CALL GSFFMTNUM(Cumvol_strmin, val1)
      CALL GSFFMTNUM(Rate_strmin, val2)
      WRITE (Gsf_unt, 9003) text2, val1, text2, val2
!1C-----GROUND WATER FLOW.
      CALL GSFFMTNUM(Cumvol_gwbndin, val1)
      CALL GSFFMTNUM(Rate_gwbndin, val2)
      WRITE (Gsf_unt, 9003) text3, val1, text3, val2
!1D-----ALL WELLS.
      IF ( Have_wells==1 ) THEN
        CALL GSFFMTNUM(Cumvol_wellin, val1)
        CALL GSFFMTNUM(Rate_wellin, val2)
        WRITE (Gsf_unt, 9003) text4, val1, text4, val2
      ENDIF
!1E-----LAKES.
      IF ( Have_lakes==1 ) THEN
        CALL GSFFMTNUM(Cumvol_lakin, val1)
        CALL GSFFMTNUM(Rate_lakin, val2)
        WRITE (Gsf_unt, 9003) text10, val1, text10, val2
      END IF
!
!2------PRINT CUMULATIVE VOLUMES AND RATES FOR OUTFLOW.
      WRITE (Gsf_unt, 9004)
!
!2A-----ALL ET.
      CALL GSFFMTNUM(Cumvol_et, val1)
      CALL GSFFMTNUM(Rate_et, val2)
      WRITE (Gsf_unt, 9003) text5, val1, text5, val2
!2B-----STREAMFLOW.
      CALL GSFFMTNUM(Cumvol_strmot, val1)
      CALL GSFFMTNUM(Rate_strmot, val2)
      WRITE (Gsf_unt, 9003) text2, val1, text2, val2
!2C-----GROUND WATER FLOW.
      CALL GSFFMTNUM(Cumvol_gwbndot, val1)
      CALL GSFFMTNUM(Rate_gwbndot, val2)
      WRITE (Gsf_unt, 9003) text3, val1, text3, val2
!2D-----ALL WELLS.
      IF ( Have_wells==1 ) THEN
        CALL GSFFMTNUM(Cumvol_wellot, val1)
        CALL GSFFMTNUM(Rate_wellot, val2)
        WRITE (Gsf_unt, 9003) text4, val1, text4, val2
      ENDIF
!2E-----LAKES.
      IF ( Have_lakes==1 ) THEN
        CALL GSFFMTNUM(Cumvol_lakot, val1)
        CALL GSFFMTNUM(Rate_lakot, val2)
        WRITE (Gsf_unt, 9003) text10, val1, text10, val2
      END IF
!2F-----FAR FIELD
      IF ( Outflow_flg.EQ.1 ) THEN
        CALL GSFFMTNUM(Cumvol_farout, val1)
        CALL GSFFMTNUM(Rate_farout, val2)
        WRITE (Gsf_unt, 9003) text12, val1, text12, val2
      END IF
!
!3------CUMULATIVE INFLOW MINUS CUMULATIVE OUTFLOW.
      cumvol_in = Cumvol_precip + Cumvol_strmin + Cumvol_gwbndin +
     &            Cumvol_wellin
      ! rsr, need lake pipeline in???
!     &            + Cumvol_lakin - Cumvol_lakeppt
      cumvol_out = Cumvol_et + Cumvol_strmot + Cumvol_gwbndot +
     &             Cumvol_wellot + Cumvol_farout
      ! rsr, need lake pipeline out???
!     &             + Cumvol_lakot - Cumvol_lakeevap - Cumvol_uzfet
      cumdiff = cumvol_in - cumvol_out
!
!4------INFLOW RATE MINUS OUTFLOW RATE.
!      rate_in = Rate_precip + Rate_strmin + Rate_gwbndin + Rate_wellin +
!     &          Rate_lakin
!      rate_out = Rate_et + Rate_strmot + Rate_gwbndot + Rate_wellot +
!     &           Rate_lakot + Rate_farout

      rate_in = Rate_precip + Rate_strmin + Rate_gwbndin + Rate_wellin
      rate_out = Rate_et + Rate_strmot + Rate_gwbndot + Rate_wellot +
     &           Rate_farout
      ratediff = rate_in - rate_out
!
!5------PRINT CUMULATIVE AND RATE DIFFERENCES.
      CALL GSFFMTNUM(cumdiff, val1)
      CALL GSFFMTNUM(ratediff, val2)
      WRITE (Gsf_unt, 9005) val1, val2
!
!6-----TOTAL STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_delstore, val1)
      CALL GSFFMTNUM(Rate_delstore, val2)
      WRITE (Gsf_unt, 9006) val1, val2
!
!6A----SURFACE STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_surfstor, val1)
      CALL GSFFMTNUM(Rate_surfstor, val2)
      WRITE (Gsf_unt, 9003) text6, val1, text6, val2
!
!6B----SOIL STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_soilstor, val1)
      CALL GSFFMTNUM(Rate_soilstor, val2)
      WRITE (Gsf_unt, 9003) text7, val1, text7, val2
!
!6C----UNSATURATED ZONE STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_uzstor, val1)
      CALL GSFFMTNUM(Rate_uzstor, val2)
      WRITE (Gsf_unt, 9003) text8, val1, text8, val2
!
!6D----SATURATED ZONE STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_satstor, val1)
      CALL GSFFMTNUM(Rate_satstor, val2)
      WRITE (Gsf_unt, 9003) text9, val1, text9, val2
!
!6E----LAKE STORAGE CHANGE.
      IF ( Have_lakes==1 ) THEN
        CALL GSFFMTNUM(Cum_lakestor, val1)
        CALL GSFFMTNUM(Rate_lakestor, val2)
        WRITE (Gsf_unt, 9003) text10, val1, text10, val2
      ENDIF
!
!6F----STREAM STORAGE CHANGE.
      IF ( IRTFLG.GT.0 ) THEN
        temp = DBLE(STRMDELSTOR_CUM)
        CALL GSFFMTNUM(temp, val1)
        temp = DBLE(STRMDELSTOR_RATE)
        CALL GSFFMTNUM(temp, val2)
        WRITE (Gsf_unt, 9003) text11, val1, text11, val2
      END IF
!
!7------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN IN MINUS
!       OUT AND STORAGE CHANGE.
      cum_error = cumdiff - Cum_delstore
      rate_error = ratediff - Rate_delstore
      CALL GSFFMTNUM(cum_error, val1)
      CALL GSFFMTNUM(rate_error, val2)
      WRITE (Gsf_unt, 9007) val1, val2
      cum_percent = 100.0D0*(cum_error/
     &              ((cumvol_in+cumvol_out+ABS(Cum_delstore))*0.5D0))
      rate_percent = 100.0D0*(rate_error/
     &               ((rate_in+rate_out+ABS(Rate_delstore))*0.5D0))
      IF ( ABS(cum_percent).GT.5.0D0 ) WRITE (Gsf_unt, *)
     &     ' ***WARNING, CUMULATIVE VOLUME OFF > 5%'
      IF ( ABS(rate_percent).GT.3.0D0 ) THEN
        IF ( ABS(rate_percent).GT.10.0D0 ) THEN
          WRITE (Gsf_unt, *) ' ***CAUTION, FLUX RATES OFF > 10%'
        ELSE
          WRITE (Gsf_unt, *) ' ***WARNING, FLUX RATES OFF > 3%'
        ENDIF
      ENDIF
      WRITE (Gsf_unt, 9008) cum_percent, rate_percent

 9001 FORMAT ('1', /, ' SUMMARY VOLUMETRIC BUDGET FOR GSFLOW ', /,
     &        ' DATE:', 2(I3.2), I5.4, 14X, 'CUMULATIVE TIME STEP:', I8,
     &        /, ' MODFLOW STRESS PERIOD', I7, 5X, 'CURRENT TIME STEP:',
     &        I8, 5X, 'ITERATIONS:', I8, //, 1X, 83('-'))
 9002 FORMAT (/, '   CUMULATIVE VOLUMES', 15X, 'L**3', 3X,
     &        'RATES FOR THIS TIME STEP', 11X, 'L**3/T', /, 3X, 18('-'),
     &        22X, 24('-'), //, 37X, 'IN', 41X, 'IN', /, 37X, '--', 41X,
     &        '--')
 9003 FORMAT (3X, A18, ' =', A18, 5X, A18, ' =', A18)
 9004 FORMAT (//, 36X, 'OUT', 40X, 'OUT', /, 36X, '---', 40X, '---') 
 9005 FORMAT (/, 3X, 'INFLOWS - OUTFLOWS =', A18, 5X,
     &        'INFLOWS - OUTFLOWS =', A18, /, 13X, 8('-'), 35X, 8('-'))
 9006 FORMAT (/, ' TOTAL STORAGE CHANGE =', A18, 9X, 'STORAGE CHANGE =',
     &        A18, /, 7X, 14('-'), 29X, 14('-'))
 9007 FORMAT (/, ' OVERALL BUDGET ERROR =', A18, 11X,
     &        'BUDGET ERROR =', A18, /)
 9008 FORMAT (/, '  PERCENT DISCREPANCY =', F18.2, 3X,
     &        ' PERCENT DISCREPANCY =', F18.2, ///)
!
!8------RETURN.
      END SUBROUTINE GSFSUMREPORT

!-------SUBROUTINE GSFFMTNUM
      SUBROUTINE GSFFMTNUM(Val, Strng)
!     ******************************************************************
!     FORMAT VALUE BASED ON VALUE SIZE
!     ******************************************************************
      USE PRMS_BASIN, ONLY: DNEARZERO
      IMPLICIT NONE
      INTRINSIC ABS, INT
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: Val
      CHARACTER(LEN=*), INTENT(OUT) :: Strng
! Local Variables
      DOUBLE PRECISION, PARAMETER :: BIG = 1.0D07, SMALL = 0.01D0
      DOUBLE PRECISION :: absval
!***********************************************************************
      absval = ABS(Val)
      IF ( absval.LT.DNEARZERO ) THEN
!       WRITE (Strng, '(I18)') INT(Val)
        Strng = ' '
      ELSEIF ( absval.GT.BIG .OR. absval.LT.SMALL ) THEN
        WRITE (Strng, '(1PD18.4)') Val
      ELSE
        WRITE (Strng, '(F18.2)') Val
      ENDIF
      END SUBROUTINE GSFFMTNUM

!***********************************************************************
! Figure out the total storage of the streams
!***********************************************************************
      SUBROUTINE MODFLOW_SFR_GET_STORAGE
      USE GSFSUM, ONLY: Strm_stor
      USE GWFSFRMODULE, ONLY: STRM, NSTRM
      IMPLICIT NONE
! Local Variables
      INTEGER :: l
!      REAL :: depth, width, strlen
!***********************************************************************
      Strm_stor = 0.0D0

      DO l = 1, NSTRM
!        depth = STRM(7, l)
!        width = STRM(5, l)
!        strlen = STRM(1, l)
        Strm_stor = Strm_stor + (STRM(7, l)*STRM(5, l)*STRM(1, l))
      ENDDO

      END SUBROUTINE MODFLOW_SFR_GET_STORAGE

!***********************************************************************
! Sums inflow to and outflow from PRMS ground-water reservoirs; outflow
! can be routed to downslope ground-water reservoirs and stream
! segments
!
! Can be used for depression storage
!***********************************************************************
! Modified 7/1997 J. Vaccaro to set a minimum value for groundwater flow
! by reading in a minimum ground-water storage value for each groundwater
! reservoir, if this value is set=0, then standard PRMS routine module.
! A minimum may represent an injection well, intrabasin transfer,
! contribution from larger regional gw system, or past residual storage
! modified 10/1/2008 rsregan to include Vaccaro code
!***********************************************************************
      MODULE PRMS_GWFLOW
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 191
      REAL, ALLOCATABLE :: Elevsurf(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwstor_minarea(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seepage_gwr(:)
      DOUBLE PRECISION, SAVE :: Basin_gw_upslope, Basin_farflow
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwin_dprst(:)
      INTEGER, SAVE :: Gwminarea_flag
      DOUBLE PRECISION, SAVE :: Basin_dnflow, Basin_gwstor_minarea_wb
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_gwstor, Basin_gwflow, Basin_gwsink
      DOUBLE PRECISION, SAVE :: Basin_sfres_seep, Basin_gwin
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwres_stor(:)
      REAL, SAVE, ALLOCATABLE :: Gwres_flow(:), Gwres_sink(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gw_upslope(:), Gwres_in(:)
      REAL, SAVE, ALLOCATABLE :: Hru_gw_cascadeflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gw_in_soil(:), Gw_in_ssr(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sfres_seepage(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gw_seep_lakein(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwstor_minarea_wb(:)
!   Declared Variables from other modules - srunoff
      REAL, ALLOCATABLE :: Dprst_seep_hru(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Ssr_gwres(:)
      REAL, SAVE, ALLOCATABLE :: Gwflow_coef(:), Gwsink_coef(:)
      REAL, SAVE, ALLOCATABLE :: Gwstor_init(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_seep_elev(:), Elevsurf_init(:)
      REAL, SAVE, ALLOCATABLE :: Gw_seep_coef(:), Gwstor_min(:)
      END MODULE PRMS_GWFLOW

!***********************************************************************
!     Main gwflow routine
!***********************************************************************
      INTEGER FUNCTION gwflow()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gwflowdecl, gwflowinit, gwflowrun
!***********************************************************************
      gwflow = 0

      IF ( Process(:3)=='run' ) THEN
        gwflow = gwflowrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        gwflow = gwflowdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        gwflow = gwflowinit()
      ENDIF

      END FUNCTION gwflow

!***********************************************************************
!     gwflowdecl - set up parameters for groundwater computations
!   Declared Parameters
!     ssr_gwres, hru_gwres, gwstor_init, gwflow_coef, gwsink_coef
!     sfres_seep_elev, elevsurf_init, gw_seep_coef
!***********************************************************************
      INTEGER FUNCTION gwflowdecl()
      USE PRMS_GWFLOW
      USE PRMS_MODULE, ONLY: Nhru, Ngw, Nssr, Nsfres, Cascadegw_flag, &
          Strmflow_flag, Print_debug, Model, Version_gwflow, Gwflow_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
!***********************************************************************
      gwflowdecl = 1

      Version_gwflow = '$Id: gwflow.f90 4183 2012-02-15 21:36:07Z rsregan $'
      Gwflow_nc = INDEX( Version_gwflow, ' $' ) + 1
      IF ( Print_debug>-1 ) THEN
        IF ( declmodule(Version_gwflow(:Gwflow_nc))/=0 ) STOP
      ENDIF

! cascading variables and parameters
      IF ( Cascadegw_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Gw_upslope(Ngw) )
        IF ( declvar('gwflow', 'gw_upslope', 'ngw', Ngw, 'double', &
             'Groundwater flow received from upslope GWRs for each GWR', &
             'acre-inches', Gw_upslope)/=0 ) CALL read_error(3, 'gw_upslope')

        ALLOCATE ( Hru_gw_cascadeflow(Ngw) )
        IF ( declvar('gwflow', 'hru_gw_cascadeflow', 'ngw', Ngw, 'real', &
             'Cascading groundwater flow from each GWR', &
             'inches', Hru_gw_cascadeflow)/=0 ) CALL read_error(3, 'hru_gw_cascadeflow')
      ENDIF

      ALLOCATE ( Gwres_stor(Ngw) )
      IF ( declvar('gwflow', 'gwres_stor', 'ngw', Ngw, 'double', &
           'Storage in each GWR', &
           'inches', Gwres_stor)/=0 ) CALL read_error(3, 'gwres_stor')

      ALLOCATE ( Gwres_flow(Ngw) )
      IF ( declvar('gwflow', 'gwres_flow', 'ngw', Ngw, 'real', &
           'Groundwater discharge from each GWR to the stream network', &
           'inches', Gwres_flow)/=0 ) CALL read_error(3, 'gwres_flow')

      ALLOCATE ( Gwres_in(Ngw) )
      IF ( declvar('gwflow', 'gwres_in', 'ngw', Ngw, 'double', &
           'Total inflow to each GWR from associated capillary and gravity reservoirs', &
           'acre-inches', Gwres_in)/=0 ) CALL read_error(3, 'gwres_in')

      ALLOCATE ( Gwres_sink(Ngw) )
      IF ( declvar('gwflow', 'gwres_sink', 'ngw', Ngw, 'real', &
           'Outflow from GWRs to the groundwater sink; water is'// &
           ' considered underflow or flow to deep aquifers and does'// &
           ' not flow to the stream network', &
           'inches', Gwres_sink)/=0 ) CALL read_error(3, 'gwres_sink')

      ALLOCATE ( Gw_in_soil(Ngw) )
      IF ( declvar('gwflow', 'gw_in_soil', 'ngw', Ngw, 'double', &
           'Drainage from capillary reservoir excess water for each GWR', &
           'acre-inches', Gw_in_soil)/=0 ) CALL read_error(3, 'gw_in_soil')

      ALLOCATE ( Gw_in_ssr(Ngw) )
      IF ( declvar('gwflow', 'gw_in_ssr', 'ngw', Ngw, 'double', &
           'Drainage from gravity reservoir excess water for each GWR', &
           'acre-inches', Gw_in_ssr)/=0 ) CALL read_error(3, 'gw_in_ssr')

      IF ( declvar('gwflow', 'basin_gwstor', 'one', 1, 'double', &
           'Basin area-weighted average of storage in GWRs', &
           'inches', Basin_gwstor)/=0 ) CALL read_error(3, 'basin_gwstor')

      IF ( declvar('gwflow', 'basin_gwin', 'one', 1, 'double', &
           'Basin area-weighted average of inflow to GWRs', &
           'inches', Basin_gwin)/=0 ) CALL read_error(3, 'basin_gwin')

      IF ( declvar('gwflow', 'basin_gwflow', 'one', 1, 'double', &
           'Basin area-weighted average of groundwater flow to the stream network', &
           'inches', Basin_gwflow)/=0 ) CALL read_error(3, 'basin_gwflow')

      IF ( declvar('gwflow', 'basin_gwsink', 'one', 1, 'double', &
           'Basin area-weighted average of GWR outflow to the groundwater sink', &
           'inches', Basin_gwsink)/=0 ) CALL read_error(3, 'basin_gwsink')

      ALLOCATE ( Ssr_gwres(Nssr) )
      IF ( Nssr/=Ngw .OR. Model==99 ) THEN
        IF ( declparam('gwflow', 'ssr_gwres', 'nssr', 'integer', &
             '1', 'bounded', 'ngw', &
             'Index of GWR to receive flow from associated gravity reservoirs', &
             'Index of the GWR that receives flow from each'// &
             ' associated subsurface or gravity reservoir (deprecated)', &
             'none')/=0 ) CALL read_error(1, 'ssr_gwres')
      ENDIF

      ALLOCATE ( Gwstor_init(Ngw) )
      IF ( declparam('gwflow', 'gwstor_init', 'ngw', 'real', &
           '0.1', '0.0', '20.0', &
           'Initial storage in each GWR', &
           'Storage in each GWR at the beginning of a simulation', &
           'inches')/=0 ) CALL read_error(1, 'gwstor_init')

      ALLOCATE ( Gwflow_coef(Ngw) )
      IF ( declparam('gwflow', 'gwflow_coef', 'ngw', 'real', &
           '0.015', '0.0', '1.0', &
           'Groundwater routing coefficient', &
           'Linear coefficient in the equation to compute groundwater'// &
           ' discharge for each GWR', &
           '1.0/day')/=0 ) CALL read_error(1, 'gwflow_coef')

      ALLOCATE ( Gwsink_coef(Ngw) )
      IF ( declparam('gwflow', 'gwsink_coef', 'ngw', 'real', &
           '0.0', '0.0', '1.0', &
           'Groundwater sink coefficient', &
           'Linear coefficient in the equation to compute outflow'// &
           ' to the groundwater sink for each GWR', &
           '1.0/day')/=0 ) CALL read_error(1, 'gwsink_coef')

      IF ( Strmflow_flag==2 .OR. Model==99 ) THEN
        IF ( declvar('gwflow', 'basin_sfres_seep', 'one', 1, 'double', &
             'Basin area-weighted average of lake-bed seepage to GWRs', &
             'inches', Basin_sfres_seep)/=0 ) CALL read_error(3, 'basin_sfres_seep')

        ALLOCATE ( Sfres_seepage(Nsfres) )
        IF ( declvar('gwflow', 'sfres_seepage', 'nsfres', Nsfres, 'double', &
             'Lake-bed seepage from each lake HRU to the associated GWR', &
             'inches', Sfres_seepage)/=0 ) CALL read_error(3, 'sfres_seepage')

        ALLOCATE ( Gw_seep_lakein(Nsfres) )
        IF ( declvar('gwflow', 'gw_seep_lakein', 'nsfres', Nsfres, 'double', &
             'Groundwater discharge to any associated lake HRU for each GWR', &
             'inches', Gw_seep_lakein)/=0 ) CALL read_error(3, 'gw_seep_lakein')

        ALLOCATE ( Elevsurf(Nsfres), Sfres_seep_elev(Nsfres) )
        IF ( declparam('gwflow', 'sfres_seep_elev', 'nsfres', 'real', &
             '1.0', '0.0', '1000.0', &
             'Elevation over which lakebed seepage to the GWR occurs', &
             'Elevation over which lakebed seepage to the GWR occurs for lake HRUs', &
             'feet')/=0 ) CALL read_error(1, 'sfres_seep_elev')

        ALLOCATE ( Elevsurf_init(Nsfres) )
        IF ( declparam('gwflow', 'elevsurf_init', 'nsfres', 'real', &
             '100.0', '0.0', '10000.0', &
             'Initial lake surface elevation', 'Initial lake surface elevation for lake HRUs', &
             'feet')/=0 ) CALL read_error(1, 'elevsurf_init')

        ALLOCATE ( Gw_seep_coef(Nsfres) )
        IF ( declparam('gwflow', 'gw_seep_coef', 'nsfres', 'real', &
             '0.015', '0.0', '1.0', &
             'Linear coefficient to compute seepage and groundwater'// &
             ' discharge to and from associated lake HRUs', &
             'Linear coefficient in equation to compute lakebed'// &
             ' seepage to the GWR and groundwater discharge to lake HRUs', &
             '1.0/day')/=0 ) CALL read_error(1, 'gw_seep_coef')
      ENDIF

      ALLOCATE ( Gwstor_min(Ngw) )
      IF ( declparam('gwflow', 'gwstor_min', 'ngw', 'real', &
           '0.0', '0.0', '5.0', &
           'Minimum storage in each GWR', &
           'Minimum storage in each GWR to ensure storage is greater'// &
           ' than specified value to account for inflow from deep'// &
           ' aquifers or injection wells with the water source'// &
           ' outside the basin', &
           'inches')/=0 ) CALL read_error(1, 'gwstor_min')

      ALLOCATE ( Gwstor_minarea_wb(Ngw) )
      IF ( declvar('gwflow', 'gwstor_minarea_wb', 'ngw', Ngw, 'double', &
           'Storage added to each GWR when storage is less than gwstor_min', &
           'inches', Gwstor_minarea_wb)/=0 ) CALL read_error(3, 'gwstor_minarea_wb')

      gwflowdecl = 0
      END FUNCTION gwflowdecl

!***********************************************************************
!     gwflowinit - Initialize gwflow module - get parameter values,
!               compute initial values.
!***********************************************************************
      INTEGER FUNCTION gwflowinit()
      USE PRMS_GWFLOW
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Cascadegw_flag, Nsfres, &
          Dprst_flag, Strmflow_flag, Print_debug
      USE PRMS_BASIN, ONLY: Timestep, Gwres_area, Gwr_type, Lake_hru_id, &
          Basin_area_inv, Active_gwrs, Gwr_route_order
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
      INTRINSIC ABS, DBLE
! Local Variables
      INTEGER :: i, j, jj, jjj
      DOUBLE PRECISION :: seepage
!***********************************************************************
      gwflowinit = 1

      IF ( Nssr/=Ngw ) THEN
        IF ( getparam('gwflow', 'ssr_gwres', Nssr, 'integer', Ssr_gwres)/=0 ) CALL read_error(2, 'ssr_gwres')
      ELSE
        DO i = 1, Nssr
          Ssr_gwres(i) = i
        ENDDO
      ENDIF

      IF ( getparam('gwflow', 'gwflow_coef', Ngw, 'real', Gwflow_coef)/=0 ) CALL read_error(2, 'gwflow_coef')
      IF ( getparam('gwflow', 'gwsink_coef', Ngw, 'real', Gwsink_coef)/=0 ) CALL read_error(2, 'gwsink_coef')
      IF ( getparam('gwflow', 'gwstor_min', Ngw, 'real', Gwstor_min)/=0 ) CALL read_error(2, 'gwstor_min')

      Gwminarea_flag = 0
      ALLOCATE ( Gwstor_minarea(Ngw) )
      DO i = 1, Ngw
        IF ( Gwstor_min(i)>0.0 ) Gwminarea_flag = 1
        Gwstor_minarea(i) = DBLE( Gwstor_min(i)*Gwres_area(i) )
        Gwstor_minarea_wb(i) = 0.0D0
        IF ( Gwflow_coef(i)>1.0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, gwflow_coef > 1.0 for GWR:', i, ' set to 1.0', Gwflow_coef(i)
          Gwflow_coef(i) = 1.0
        ENDIF
        IF ( Gwflow_coef(i)<0.0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, gwflow_coef < 0.0 for GWR:', i, ' set to 0.0', Gwflow_coef(i)
          Gwflow_coef(i) = 0.0
        ENDIF
        ! GWR's cannot be swales or lakes in GSFLOW
        IF ( Gwr_type(i)==3 .OR. Gwr_type(i)==2 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, gwr_type > 1 for GWR:', i, ' set to 1', Gwr_type(i)
          Gwr_type(i) = 1
        ENDIF
      ENDDO
      IF ( Gwminarea_flag==0 ) DEALLOCATE ( Gwstor_min )

      IF ( Strmflow_flag==2 ) THEN
        ALLOCATE ( Seepage_gwr(Ngw) )
        Seepage_gwr = 0.0D0
        IF ( getparam('gwflow', 'gw_seep_coef', Nsfres, 'real', Gw_seep_coef)/=0 ) CALL read_error(2, 'gw_seep_coef')
        IF ( getparam('gwflow', 'sfres_seep_elev', Nsfres, 'real', Sfres_seep_elev)/=0 ) CALL read_error(2, 'sfres_seep_elev')
        Sfres_seepage = 0.0D0
        Gw_seep_lakein = 0.0D0
      ENDIF

      IF ( Dprst_flag==1 ) THEN
        ALLOCATE ( Dprst_seep_hru(Nhru), Gwin_dprst(Ngw) )
        Gwin_dprst = 0.0D0
      ENDIF

! do only once, so restart uses saved values
      IF ( Timestep==0 ) THEN
        IF ( Cascadegw_flag==1 ) THEN
          Gw_upslope = 0.0D0
          Hru_gw_cascadeflow = 0.0
        ENDIF
        Gwres_flow = 0.0
        Gwres_in = 0.0D0
        Gwres_sink = 0.0
        Gw_in_ssr = 0.0D0
        Gw_in_soil = 0.0D0
        IF ( getparam('gwflow', 'gwstor_init', Ngw, 'real', Gwstor_init)/=0 ) CALL read_error(2, 'gwstor_init')
        DO i = 1, Ngw
          Gwres_stor(i)= DBLE( Gwstor_init(i) )
        ENDDO
        DEALLOCATE ( Gwstor_init )
        Basin_gwflow = 0.0D0
        Basin_gwsink = 0.0D0
        Basin_gwin = 0.0D0
        Basin_farflow = 0.0D0
        Basin_sfres_seep = 0.0D0
        IF ( Strmflow_flag==2 ) THEN
          IF ( getparam('gwflow', 'elevsurf_init', Nsfres, 'real', Elevsurf_init)/=0 ) CALL read_error(2, 'elevsurf_init')
          Elevsurf = Elevsurf_init
          DEALLOCATE ( Elevsurf_init )
        ENDIF
        Basin_gw_upslope = 0.0D0
        Basin_dnflow = 0.0D0
        Basin_gwstor_minarea_wb = 0.0D0
      ENDIF

      IF ( Strmflow_flag==2 ) THEN
        Basin_sfres_seep = 0.0D0
        DO i = 1, Active_gwrs
          j = Gwr_route_order(i)
          IF ( Gwr_type(j)==2 ) THEN
            jjj = Lake_hru_id(j)
            IF ( jjj==0 ) THEN
              WRITE (*, '(/,A,I10,2A,/)') 'ERROR, GWR', j, &
                     'specified as a lake but the GWR is not a lake'
              STOP
            ENDIF
            seepage = (Elevsurf(jjj)-Sfres_seep_elev(jjj))*12.0*Gw_seep_coef(jjj)
            IF ( seepage<0.0D0 ) THEN
              IF ( ABS(seepage)>Gwres_stor(j) ) seepage = -Gwres_stor(j)
              Gw_seep_lakein(jjj) = -seepage
            ELSE
              Sfres_seepage(jjj) = seepage
            ENDIF
            Basin_sfres_seep = Basin_sfres_seep + seepage*Gwres_area(j)
            Gwres_stor(j) = Gwres_stor(j) + seepage
            Seepage_gwr(j) = seepage
          ENDIF
        ENDDO
        Basin_sfres_seep = Basin_sfres_seep*Basin_area_inv
      ENDIF

      Basin_gwstor = 0.0D0
      DO jj = 1, Active_gwrs
        j = Gwr_route_order(jj)
        Basin_gwstor = Basin_gwstor + Gwres_stor(j)*Gwres_area(j)
      ENDDO
      Basin_gwstor = Basin_gwstor*Basin_area_inv

      IF ( Print_debug==1 ) THEN
        OPEN ( BALUNT, FILE='gwflow.wbal' )
        WRITE ( BALUNT, 9001 )
      ENDIF

 9001 FORMAT ('    Date     Water Bal last store  GWR store', &
              '   GW input    GW flow    GW sink    farflow', &
              ' GW upslope minarea_in   downflow')

      gwflowinit = 0
      END FUNCTION gwflowinit

!***********************************************************************
!     gwflowrun - Computes groundwater flow to streamflow and to
!              groundwater sink
!***********************************************************************
      INTEGER FUNCTION gwflowrun()
      USE PRMS_GWFLOW
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Strmflow_flag, Dprst_flag, &
          Cascadegw_flag, Nsfres, Print_debug
      USE PRMS_BASIN, ONLY: Active_gwrs, Gwr_route_order, Gwr_type, &
          Gwres_area, Basin_area_inv, Active_hrus, Hru_route_order, &
          Hru_area, Ssres_area, Hru_gwres, Lake_hru_id
      USE PRMS_FLOWVARS, ONLY: Soil_to_gw, Ssr_to_gw
      USE PRMS_CASCADE, ONLY: Ncascade_gwr
      USE PRMS_OBS, ONLY: Nowtime
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getvar
      EXTERNAL rungw_cascade, read_error
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, j, ii, jj, jjj
      REAL :: gwarea, dnflow, far_gwflow
      DOUBLE PRECISION :: gwin, gwstor, gwsink, seepage, gwflow
      DOUBLE PRECISION :: gwbal, gwstor_last
      DOUBLE PRECISION :: last_basin_gwstor, last_gwstor, gwup
!***********************************************************************
      gwflowrun = 1

      IF ( Dprst_flag==1 ) THEN
        IF ( getvar('srunoff', 'dprst_seep_hru', Nhru, 'real', Dprst_seep_hru)/=0 ) CALL read_error(4, 'dprst_seep_hru')
      ENDIF

      IF ( Cascadegw_flag==1 ) THEN
        Gw_upslope = 0.0D0
        Basin_dnflow = 0.0D0
        Basin_gw_upslope = 0.0D0
      ENDIF

      DO jj = 1, Active_gwrs
        j = Gwr_route_order(jj)
        Gw_in_soil(j) = 0.0D0
        Gwres_stor(j) = Gwres_stor(j)*Gwres_area(j)
      ENDDO

      IF ( Strmflow_flag==2 ) THEN
        ! elevsurf from last timestep
        IF ( getvar('strmlake', 'elevsurf', Nsfres, 'real', Elevsurf)/=0 ) CALL read_error(4, 'elevsurf')
        Sfres_seepage = 0.0D0
        Gw_seep_lakein = 0.0D0
        Basin_sfres_seep = 0.0D0
        DO jj = 1, Active_gwrs
          j = Gwr_route_order(jj)
          IF ( Gwr_type(j)==2 ) THEN
            gwarea = Gwres_area(j)
            ! seepage added to GWR
            jjj = Lake_hru_id(j)
            IF ( jjj>0 ) THEN
              !rsr, need seepage variable for WB
              seepage = (Elevsurf(jjj)-Sfres_seep_elev(jjj))*12.0*Gw_seep_coef(jjj)*gwarea
              IF ( seepage<0.0D0 ) THEN
                IF ( ABS(seepage)>Gwres_stor(j) ) seepage = -Gwres_stor(j)
                Gw_seep_lakein(jjj) = -seepage/gwarea
              ELSE
                Sfres_seepage(jjj) = seepage/gwarea
              ENDIF
              Basin_sfres_seep = Basin_sfres_seep + seepage
              Gwres_stor(j) = Gwres_stor(j) + seepage
              Seepage_gwr(j) = seepage
            ENDIF
          ENDIF
        ENDDO
        Basin_sfres_seep = Basin_sfres_seep*Basin_area_inv
      ENDIF

!******Sum the inflows to each GWR to units of acre-inches
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        j = Hru_gwres(i)
        IF ( Gwr_type(j)==2 ) CYCLE
        !rsr, soil_to_gw is for whole HRU, not just perv
        Gw_in_soil(j) = Gw_in_soil(j) + Soil_to_gw(i)*Hru_area(i)
        IF ( Dprst_flag>0 ) Gwin_dprst(j) = Dprst_seep_hru(i)*Hru_area(i)
      ENDDO

      IF ( Ngw/=Nhru ) THEN
        Gw_in_ssr = 0.0D0
        DO i = 1, Nssr
          j = Ssr_gwres(i)
          Gw_in_ssr(j) = Gw_in_ssr(j) + Ssr_to_gw(i)*Ssres_area(i)
        ENDDO
      ELSE
        DO ii = 1, Active_gwrs
          i = Gwr_route_order(ii)
          IF ( Gwr_type(i)==2 ) CYCLE
          Gw_in_ssr(i) = Ssr_to_gw(i)*Ssres_area(i)
        ENDDO
      ENDIF

      Basin_gwstor_minarea_wb = 0.0D0
      Basin_gwflow = 0.0D0
      last_basin_gwstor = Basin_gwstor
      Basin_gwstor = 0.0D0
      Basin_gwsink = 0.0D0
      Basin_gwin = 0.0D0
      Basin_farflow = 0.0D0
      DO j = 1, Active_gwrs
        i = Gwr_route_order(j)
        IF ( Gwr_type(i)==2 .OR. Gwr_type(i)==0 ) CYCLE
        gwarea = Gwres_area(i)
        last_gwstor = Gwres_stor(i)
        gwstor = last_gwstor
        gwin = Gw_in_soil(i) + Gw_in_ssr(i)
        IF ( Strmflow_flag==2 ) gwin = gwin + Seepage_gwr(i)
        IF ( Cascadegw_flag==1 ) THEN
          gwin = gwin + Gw_upslope(i)
          Basin_gw_upslope = Basin_gw_upslope + Gw_upslope(i)
        ENDIF
        IF ( Dprst_flag==1 ) THEN
          !rsr, need basin variable for WB
          Gwin_dprst(i) = Dprst_seep_hru(i)*gwarea
          gwin = gwin + Gwin_dprst(i)
        ENDIF
        IF ( Gwminarea_flag==1 ) THEN
          ! check to be sure gwres_stor >= gwstor_minarea before computing outflows
          IF ( gwstor<Gwstor_minarea(i) ) THEN
            gwstor_last = gwstor
            gwstor = Gwstor_minarea(i)
            !rsr, keep track of change in storage for WB
            Gwstor_minarea_wb(i) = gwstor - gwstor_last
            gwin = gwin + Gwstor_minarea_wb(i)
!            PRINT *, 'Added gwstor based on gwstor_min', Gwstor_minarea_wb(i)/gwarea
            Basin_gwstor_minarea_wb = Basin_gwstor_minarea_wb + Gwstor_minarea_wb(i)
          ELSE
            Gwstor_minarea_wb(i) = 0.0D0
          ENDIF
        ENDIF
        gwstor = gwstor + gwin
        Basin_gwin = Basin_gwin + gwin

! Compute groundwater discharge
        gwflow = gwstor*Gwflow_coef(i)
! Reduce storage by outflow
        gwstor = gwstor - gwflow

        gwsink = 0.0D0
        IF ( Gwsink_coef(i)>0.0 ) THEN
          gwsink = gwstor*Gwsink_coef(i)
          gwstor = gwstor - gwsink
          IF ( gwstor<0.0D0 ) THEN
            gwsink = gwsink + gwstor
            gwstor = 0.0D0
          ENDIF
          Gwres_sink(i) = gwsink/gwarea
          Basin_gwsink = Basin_gwsink + gwsink
        ENDIF
        Basin_gwstor = Basin_gwstor + gwstor

        dnflow = 0.0
        Gwres_flow(i) = gwflow/gwarea
        far_gwflow = 0.0
        IF ( Cascadegw_flag==1 ) THEN
          IF ( Ncascade_gwr(i)>0 ) THEN
            CALL rungw_cascade(i, Ncascade_gwr(i), Gwres_flow(i), dnflow, far_gwflow)
            Hru_gw_cascadeflow(i) = dnflow + far_gwflow
            Basin_dnflow = Basin_dnflow + dnflow*gwarea
            Basin_farflow = Basin_farflow + far_gwflow*gwarea
          ENDIF
        ENDIF
        Basin_gwflow = Basin_gwflow + Gwres_flow(i)*gwarea

        IF ( Print_debug==1 ) THEN
          gwbal = (last_gwstor + gwin - gwstor - gwsink)/gwarea &
                  - Gwres_flow(i) - dnflow - far_gwflow
          gwup = 0.0D0
          IF ( Cascadegw_flag==1 ) gwup = Gw_upslope(i)
          IF ( ABS(gwbal)>5.0D-4 ) THEN
            WRITE ( BALUNT, * ) 'GWR possible water balance issue', &
                    i, gwbal, last_gwstor, gwin, gwstor, Gwres_flow(i), &
                    gwsink, dnflow, Gw_in_soil(i), Gw_in_ssr(i), &
                    gwup, far_gwflow, gwarea
            IF ( Dprst_flag==1 ) WRITE ( BALUNT, * ) 'Gwin_dprst', Gwin_dprst(i)
          ENDIF
        ENDIF

        ! leave gwin in inch-acres
        Gwres_in(i) = gwin
        Gwres_stor(i) = gwstor/gwarea
        Gwstor_minarea_wb(i) = Gwstor_minarea_wb(i)/gwarea
      ENDDO

      Basin_gwflow = Basin_gwflow*Basin_area_inv
      Basin_gwstor = Basin_gwstor*Basin_area_inv
      Basin_gwsink = Basin_gwsink*Basin_area_inv
      Basin_gwin = Basin_gwin*Basin_area_inv
      Basin_farflow = Basin_farflow*Basin_area_inv
      Basin_gw_upslope = Basin_gw_upslope*Basin_area_inv
      Basin_gwstor_minarea_wb = Basin_gwstor_minarea_wb*Basin_area_inv
      IF ( Strmflow_flag==2 ) Basin_sfres_seep = Basin_sfres_seep*Basin_area_inv

      ! not going to balance because gwstor under lakes is computed each time step, maybe
      IF ( Print_debug==1 ) THEN
        Basin_dnflow = Basin_dnflow*Basin_area_inv
        ! gwin includes upslope flow, farflow, gwin_dprst, gw_in_loss, gw_in_soil, gw_in_ssr
        gwbal = last_basin_gwstor - Basin_gwstor - Basin_gwsink &
                + Basin_gwin - Basin_gwflow - Basin_dnflow - Basin_farflow
        IF ( ABS(gwbal)>5.0D-4 ) WRITE ( BALUNT, * ) 'Possible GWR basin water balance issue'
        WRITE ( BALUNT, 9001 ) Nowtime(1), Nowtime(2), Nowtime(3), &
                gwbal, last_basin_gwstor, Basin_gwstor, Basin_gwin, &
                Basin_gwflow, Basin_gwsink, Basin_farflow, &
                Basin_gw_upslope, Basin_gwstor_minarea_wb, Basin_dnflow
 9001   FORMAT (I5, 2('/', I2.2), 11F11.4)
      ENDIF

      gwflowrun = 0
      END FUNCTION gwflowrun

!***********************************************************************
!     Compute cascading GW flow
!***********************************************************************
      SUBROUTINE rungw_cascade(Igwr, Ncascade_gwr, Gwres_flow, Dnflow, Far_gwflow)
      USE PRMS_MODULE, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_GWFLOW, ONLY: Gw_upslope
      USE PRMS_CASCADE, ONLY: Gwr_down, Gwr_down_frac, Cascade_gwr_area
! Cfs_conv converts acre-inches per timestep to cfs
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Igwr, Ncascade_gwr
      REAL, INTENT(INOUT) :: Gwres_flow, Dnflow, Far_gwflow
! Local variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_gwr
        j = Gwr_down(k, Igwr)
        ! Gwres_flow is in inches
! if gwr_down(k, Igwr) > 0, cascade contributes to a downslope GWR
        IF ( j>0 ) THEN
          Gw_upslope(j) = Gw_upslope(j) + Gwres_flow*Cascade_gwr_area(k, Igwr)
          Dnflow = Dnflow + Gwres_flow*Gwr_down_frac(k, Igwr)
! if gwr_down(k, Igwr) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          IF ( j/=Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) + Gwres_flow*Cascade_gwr_area(k, Igwr)*Cfs_conv
          ELSE
            Strm_farfield = Strm_farfield + Gwres_flow*Cascade_gwr_area(k, Igwr)*Cfs_conv
            Far_gwflow = Far_gwflow + Gwres_flow*Gwr_down_frac(k, Igwr)
          ENDIF
        ENDIF
      ENDDO

      ! gwres_flow reduced by cascading flow to HRUs or farfield
      Gwres_flow = Gwres_flow - Dnflow - Far_gwflow
      IF ( Gwres_flow<0.0 ) Gwres_flow = 0.0

      END SUBROUTINE rungw_cascade

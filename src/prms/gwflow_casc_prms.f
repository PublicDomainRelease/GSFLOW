!***********************************************************************
! Sums inflow to and outflow from PRMS ground-water reservoirs; outflow
! can be routed to downslope ground-water reservoirs and stream
! segments; modification of gwflow_prms
!
!06/15/2009, Gw_upslope should not go to stream segments in lakes      
!            need to fix/check for??
!***********************************************************************
      MODULE PRMS_GWFLOW_CASC
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 191
      REAL, SAVE :: Basin_gw_upslope, Basin_farflow
!   Declared Variables
      REAL, SAVE :: Basin_gwflow, Basin_gwstor, Basin_gwsink, Basin_gwin
      REAL, SAVE, ALLOCATABLE :: Gwres_stor(:), Gwres_flow(:)
      REAL, SAVE, ALLOCATABLE :: Gwres_sink(:), Gw_in_ssr(:)
      REAL, SAVE, ALLOCATABLE :: Gw_upslope(:), Gwres_in(:)
      REAL, SAVE, ALLOCATABLE :: Hru_gw_cascadeflow(:), Gw_in_soil(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Ssr_gwres(:)
      REAL, SAVE, ALLOCATABLE :: Gwflow_coef(:), Gwsink_coef(:)
      REAL, SAVE, ALLOCATABLE :: Gwstor_init(:)
      END MODULE PRMS_GWFLOW_CASC

!***********************************************************************
!     Main gwflow routine
!***********************************************************************
      INTEGER FUNCTION gwflow_casc_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gwcdecl, gwcinit, gwcrun
!***********************************************************************
      gwflow_casc_prms = 0

      IF ( Process_flag==0 ) THEN
        gwflow_casc_prms = gwcrun()
      ELSEIF ( Process_flag==1 ) THEN
        gwflow_casc_prms = gwcdecl()
      ELSEIF ( Process_flag==2 ) THEN
        gwflow_casc_prms = gwcinit()
      ENDIF

      END FUNCTION gwflow_casc_prms
!***********************************************************************
!     gwcdecl - set up parameters for groundwater computations
!   Declared Parameters
!     ssr_gwres, hru_gwres, gwstor_init, gwflow_coef, gwsink_coef
!***********************************************************************

      INTEGER FUNCTION gwcdecl()
      USE PRMS_GWFLOW_CASC
      USE PRMS_MODULE, ONLY: Model, Ncascdgw
      USE PRMS_BASIN, ONLY: Nhru, Ngw, Nssr
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      gwcdecl = 1

      IF ( declmodule(
     +'$Id: gwflow_casc_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

      IF ( Ncascdgw>0 .AND. Nhru.NE.Nssr .AND. Model/=99 ) THEN
        PRINT *, 'Error, using gwflow_casc and nhru not equal to nssr',
     +           Nhru, Nssr
        RETURN
      ENDIF

      IF ( Ncascdgw>0 .AND. Nhru.NE.Ngw .AND. Model/=99 ) THEN
        PRINT *, 'Error, using gwflow_casc and nhru not equal to ngw',
     +           Nhru, Ngw
        RETURN
      ENDIF

! cascading variables and parameters
      IF ( Ncascdgw>0 .OR. Model==99 ) THEN
        ALLOCATE (Gw_upslope(Ngw))
        IF ( declvar('gwflow', 'gw_upslope', 'ngw', Ngw, 'real',
     +       'Ground-water flow received from upslope ground-water'//
     +       ' reservoirs',
     +       'acre-inches',
     +       Gw_upslope).NE.0 ) RETURN

        ALLOCATE (Hru_gw_cascadeflow(Ngw))
        IF ( declvar('gwflow', 'hru_gw_cascadeflow', 'ngw', Ngw,
     +       'real',
     +       'Cascading ground-water flow from each HRU when'//
     +       ' nhru=ngw and cascades',
     +       'inches',
     +       Hru_gw_cascadeflow).NE.0 ) RETURN
      ENDIF

      ALLOCATE (Gwres_stor(Ngw))
      IF ( declvar('gwflow', 'gwres_stor', 'ngw', Ngw, 'real',
     +     'Storage in each groundwater reservoir',
     +     'inches',
     +     Gwres_stor).NE.0 ) RETURN

      ALLOCATE (Gwres_flow(Ngw))
      IF ( declvar('gwflow', 'gwres_flow', 'ngw', Ngw, 'real',
     +     'Outflow from each groundwater reservoir to streams',
     +     'inches',
     +     Gwres_flow).NE.0 ) RETURN

      ALLOCATE (Gwres_in(Ngw))
      IF ( declvar('gwflow', 'gwres_in', 'ngw', Ngw, 'real',
     +     'Sum of inflows to each groundwater reservoir from all'//
     +     ' associated soil-zone reservoirs',
     +     'acre-inches',
     +     Gwres_in).NE.0 ) RETURN

      ALLOCATE (Gwres_sink(Ngw))
      IF ( declvar('gwflow', 'gwres_sink', 'ngw', Ngw, 'real',
     +     'Amount of water transferred from groundwater'//
     +     ' reservoirs to the groundwater sink.  This water is'//
     +     ' effectively routed out of the basin and will not'//
     +     ' be included in streamflow',
     +     'inches',
     +     Gwres_sink).NE.0 ) RETURN

      ALLOCATE (Gw_in_soil(Ngw))
      IF ( declvar('gwflow', 'gw_in_soil', 'ngw', Ngw, 'real',
     +     'Sum of inflows to each groundwater reservoir from the'//
     +     ' soil-water excess of associated HRUs',
     +     'acre-inches',
     +     Gw_in_soil).NE.0 ) RETURN

      ALLOCATE (Gw_in_ssr(Ngw))
      IF ( declvar('gwflow', 'gw_in_ssr', 'ngw', Ngw, 'real',
     +     'Sum of inflows to each groundwater reservoir from'//
     +     ' associated subsurface or gravity reservoirs',
     +     'acre-inches',
     +     Gw_in_ssr).NE.0 ) RETURN

      IF ( declvar('gwflow', 'basin_gwstor', 'one', 1, 'real',
     +     'Basin area weighted average of groundwater storage',
     +     'inches',
     +     Basin_gwstor).NE.0 ) RETURN

      IF ( declvar('gwflow', 'basin_gwflow', 'one', 1, 'real',
     +     'Basin area weighted average of groundwater flow',
     +     'inches',
     +     Basin_gwflow).NE.0 ) RETURN

      IF ( declvar('gwflow', 'basin_gwsink', 'one', 1, 'real',
     +     'Basin area weighted average of groundwater'//
     +     ' reservoir storage to the groundwater sink',
     +     'inches',
     +     Basin_gwsink).NE.0 ) RETURN

      IF ( declvar('gwflow', 'basin_gwin', 'one', 1, 'real',
     +     'Basin area weighted average of inflow to'//
     +     ' groundwater reservoirs',
     +     'inches',
     +     Basin_gwin).NE.0 ) RETURN
      ALLOCATE (Ssr_gwres(Nssr))
      IF ( Nssr.NE.Ngw .OR. Model==99 ) THEN
        IF ( declparam('gwflow', 'ssr_gwres', 'nssr', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'Index of gw reservoir to receive flow from ss reservoir',
     +       'Index of the groundwater reservoir that will receive'//
     +       ' flow from each subsurface or gravity reservoir',
     +       'none').NE.0 ) RETURN
      ENDIF

      ALLOCATE (Gwstor_init(Ngw))
      IF ( declparam('gwflow', 'gwstor_init', 'ngw', 'real',
     +     '.1', '0.', '20.',
     +     'Initial storage in each gw reservoir',
     +     'Storage in each groundwater reservoir at the'//
     +     ' beginning of a simulation',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Gwflow_coef(Ngw))
      IF ( declparam('gwflow', 'gwflow_coef', 'ngw', 'real',
     +     '.015', '.0', '1.0',
     +     'Groundwater routing coefficient',
     +     'Groundwater routing coefficient - is multiplied by the'//
     +     ' storage in the groundwater reservoir to compute'//
     +     ' groundwater flow contribution to down-slope flow',
     +     '1/day').NE.0 ) RETURN

      ALLOCATE (Gwsink_coef(Ngw))
      IF ( declparam('gwflow', 'gwsink_coef', 'ngw', 'real',
     +     '0.', '0.', '1.0',
     +     'Groundwater sink coefficient',
     +     'Groundwater sink coefficient - is multiplied by the'//
     +     ' storage in the groundwater reservoir to compute the'//
     +     ' seepage from each reservoir to the groundwater sink',
     +     '1/day').NE.0 ) RETURN

      gwcdecl = 0
      END FUNCTION gwcdecl

!***********************************************************************
!     gwcinit - Initialize gwflow module - get parameter values,
!               compute initial values.
!***********************************************************************
      INTEGER FUNCTION gwcinit()
      USE PRMS_GWFLOW_CASC
      USE PRMS_MODULE, ONLY: Ncascdgw
      USE PRMS_BASIN, ONLY: Timestep, Nhru, Nssr, Ngw, Print_debug,
     +    Hru_elev, Gwres_area, Basin_area_inv, Active_gwrs,
     +    Gwr_route_order
      USE PRMS_BASIN, ONLY: Gwr_type
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, j, jj
!***********************************************************************
      gwcinit = 1

      IF ( Nssr.NE.Ngw ) THEN
        IF ( getparam('gwflow', 'ssr_gwres', Nssr, 'integer',
     +       Ssr_gwres).NE.0 ) RETURN
      ELSE
        DO i = 1, Nssr
          Ssr_gwres(i) = i
        ENDDO
      ENDIF

      IF ( getparam('gwflow', 'gwflow_coef', Ngw, 'real',
     +     Gwflow_coef).NE.0 ) RETURN

      IF ( getparam('gwflow', 'gwsink_coef', Ngw, 'real',
     +     Gwsink_coef).NE.0 ) RETURN

! do only once, so restart uses saved values
      IF ( Timestep==0 ) THEN
        IF ( Ncascdgw>0 ) THEN
          Gw_upslope = 0.0
          Hru_gw_cascadeflow = 0.0
        ENDIF
        Gwres_flow = 0.0
        Gwres_in = 0.0
        Gwres_sink = 0.0
        Gw_in_ssr = 0.0
        Gw_in_soil = 0.0
        IF ( getparam('gwflow', 'gwstor_init', Ngw, 'real',
     +       Gwstor_init).NE.0 ) RETURN
        Gwres_stor = Gwstor_init
        DEALLOCATE ( Gwstor_init )
        Basin_gwflow = 0.0
        Basin_gwsink = 0.0
        Basin_gwin = 0.0
        Basin_farflow = 0.0
      ENDIF

      Basin_gwstor = 0.0
      DO jj = 1, Active_gwrs
        j = Gwr_route_order(jj)
        Basin_gwstor = Basin_gwstor + Gwres_stor(j)*Gwres_area(j)
      ENDDO
      Basin_gwstor = Basin_gwstor*Basin_area_inv

      IF ( Print_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='gwflow_casc_prms.wbal')
        WRITE (BALUNT, 9001)
      ENDIF

 9001 FORMAT ('    Date     Water Bal last store  GWR store',
     +        '   GW input    GW flow    GW sink    farflow',
     +        ' GW upslope   downflow')

      gwcinit = 0
      END FUNCTION gwcinit

!***********************************************************************
!     gwcrun - Computes groundwater flow to streamflow and to
!              groundwater sink
!***********************************************************************
      INTEGER FUNCTION gwcrun()
      USE PRMS_GWFLOW_CASC
      USE PRMS_MODULE, ONLY: Ncascdgw
      USE PRMS_BASIN, ONLY: Nhru, Nssr, Active_gwrs, Gwr_route_order,
     +    Gwres_area, Basin_area_inv, Active_hrus, Hru_route_order,
     +    Print_debug, Hru_area, Ngw, Ssres_area, Hru_gwres, Gwr_type
      USE PRMS_FLOWVARS, ONLY: Soil_to_gw, Ssr_to_gw
      USE PRMS_CASCADE, ONLY: Ncascade_gwr
      USE PRMS_OBS, ONLY: Nowtime, Timestep_days
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getvar
      EXTERNAL rungw_cascade
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, j, ii, jj
      REAL :: gwarea, gwin, gwstor, gwsink, gwflow
      REAL :: gwbal, dnflow, far_gwflow, gwup, last_gwstor
      REAL :: basin_dnflow, last_basin_gwstor
!***********************************************************************
      gwcrun = 1
      IF ( Ncascdgw.GT.0 ) Gw_upslope = 0.0

      Basin_gwstor = 0.0
      DO jj = 1, Active_gwrs
        j = Gwr_route_order(jj)
        Gw_in_soil(j) = 0.0
        Gwres_stor(j) = Gwres_stor(j)*Gwres_area(j)
        Basin_gwstor = Basin_gwstor + Gwres_stor(j)
      ENDDO
      Basin_gwstor = Basin_gwstor*Basin_area_inv

!******Sum the inflows to each groundwater reservoir

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        j = Hru_gwres(i)
        IF ( Gwr_type(j)==2 ) CYCLE
        !rsr, soil_to_gw is for whole HRU, not just perv
        Gw_in_soil(j) = Gw_in_soil(j) + Soil_to_gw(i)*Hru_area(i)
      ENDDO

      IF ( Ngw.NE.Nhru ) THEN
        Gw_in_ssr = 0.0
        DO i = 1, Nssr
          j = Ssr_gwres(i)
          Gw_in_ssr(j) = Gw_in_ssr(j) + (Ssr_to_gw(i)*Ssres_area(i))
        ENDDO
      ELSE
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          IF ( Gwr_type(i)==2 ) CYCLE
          Gw_in_ssr(i) = Ssr_to_gw(i)*Ssres_area(i)
        ENDDO
      ENDIF

      last_basin_gwstor = Basin_gwstor
      Basin_gwflow = 0.
      Basin_gwstor = 0.
      Basin_gwsink = 0.
      Basin_gwin = 0.
      Basin_farflow = 0.
      Basin_gw_upslope = 0.
      basin_dnflow = 0.0

      DO j = 1, Active_gwrs
        i = Gwr_route_order(j)
        gwarea = Gwres_area(i)
        last_gwstor = Gwres_stor(i)
        gwin = Gw_in_soil(i) + Gw_in_ssr(i)
        IF ( Ncascdgw>0 ) THEN
          gwin = gwin + Gw_upslope(i)
          Basin_gw_upslope = Basin_gw_upslope + Gw_upslope(i)
        ENDIF
        gwstor = last_gwstor + gwin
        Basin_gwin = Basin_gwin + gwin

        gwflow = (gwstor*Gwflow_coef(i))*Timestep_days
        gwstor = gwstor - gwflow
        gwsink = 0.0
        IF ( Gwsink_coef(i).GT.0.0 ) THEN
          gwsink = (gwstor*Gwsink_coef(i))*Timestep_days
          gwstor = gwstor - gwsink
          IF ( gwstor.LT.0.0 ) THEN
            gwsink = gwsink + gwstor
            gwstor = 0.0
          ENDIF
        ENDIF
        Basin_gwstor = Basin_gwstor + gwstor
        Basin_gwsink = Basin_gwsink + gwsink
        dnflow = 0.0
        Gwres_flow(i) = gwflow/gwarea
        far_gwflow = 0.0
        IF ( Ncascdgw>0 ) THEN
          IF ( Ncascade_gwr(i).GT.0 ) THEN
            CALL rungw_cascade(i, Ncascade_gwr(i), Gwres_flow(i),
     +                         dnflow, far_gwflow)
            Hru_gw_cascadeflow(i) = dnflow + far_gwflow
            basin_dnflow = basin_dnflow + dnflow*gwarea
          ENDIF
        ENDIF

        Basin_gwflow = Basin_gwflow + Gwres_flow(i)*gwarea
        Basin_farflow = Basin_farflow + far_gwflow*gwarea
        IF ( Print_debug.EQ.1 ) THEN
          gwbal = (last_gwstor - gwstor - gwsink + gwin)/gwarea
     +             - dnflow - Gwres_flow(i)
          !rsr, far_gwflow included in gwres_flow
          gwup = 0.0
          IF ( Ncascdgw>0 ) gwup = Gw_upslope(i)
          IF ( ABS(gwbal).GT.5.0E-4 ) WRITE (BALUNT, *)
     +         'GWR possible water balance issue', i, gwbal,
     +         last_gwstor, gwin, gwstor, Gwres_flow(i),
     +         gwsink, dnflow, Gw_in_soil(i), Gw_in_ssr(i),
     +         gwup, far_gwflow, gwarea
        ENDIF
        Gwres_in(i) = gwin
        Gwres_sink(i) = gwsink/gwarea
        Gwres_stor(i) = gwstor/gwarea
      ENDDO

      Basin_gwflow = Basin_gwflow*Basin_area_inv
      Basin_gwstor = Basin_gwstor*Basin_area_inv
      Basin_gwsink = Basin_gwsink*Basin_area_inv
      Basin_gwin = Basin_gwin*Basin_area_inv
      Basin_farflow = Basin_farflow*Basin_area_inv
      Basin_gw_upslope = Basin_gw_upslope*Basin_area_inv

      !???rsr, not going to balance because gwstor under lakes
      !        is computed each time step, maybe
      IF ( Print_debug.EQ.1 ) THEN
        basin_dnflow = basin_dnflow*Basin_area_inv
        !rsr, gwin includes upslope flow, farflow in gwflow
        gwbal = last_basin_gwstor - Basin_gwstor - Basin_gwsink +
     +          Basin_gwin - Basin_gwflow - basin_dnflow
        IF ( ABS(gwbal).GT.5.0E-4 )
     +       WRITE (BALUNT, *) 'Possible GWR basin water balance issue'
        WRITE (BALUNT, 9001) Nowtime(1), Nowtime(2), Nowtime(3), gwbal,
     +                       last_basin_gwstor, Basin_gwstor, Basin_gwin
     +                       , Basin_gwflow, Basin_gwsink, Basin_farflow
     +                       , Basin_gw_upslope, basin_dnflow
 9001   FORMAT (I5, 2('/', I2.2), 9F11.7)
      ENDIF

      gwcrun = 0
      END FUNCTION gwcrun

!***********************************************************************
!     Compute cascading GW flow
!***********************************************************************
      SUBROUTINE rungw_cascade(Igwr, Ncascade_gwr, Gwres_flow, Dnflow,
     +                         Far_gwflow)
      USE PRMS_BASIN, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_GWFLOW_CASC, ONLY: Gw_upslope
      USE PRMS_CASCADE, ONLY: Gwr_down, Gwr_down_pct, Cascade_gwr_area
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
        IF ( j.GT.0 ) THEN
          Gw_upslope(j) = Gw_upslope(j)
     +                    + Gwres_flow*Cascade_gwr_area(k, Igwr)
          Dnflow = Dnflow + Gwres_flow*Gwr_down_pct(k, Igwr)
! if gwr_down(k, Igwr) < 0, cascade contributes to a stream
        ELSEIF ( j.LT.0 ) THEN
          j = IABS(j)
          IF ( j.NE.Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) + Gwres_flow
     +                       *Cascade_gwr_area(k, Igwr)*Cfs_conv
          ELSE
            Strm_farfield = Strm_farfield + Gwres_flow
     +                       *Cascade_gwr_area(k, Igwr)*Cfs_conv
            Far_gwflow = Far_gwflow + Gwres_flow*Gwr_down_pct(k, Igwr)
          ENDIF
        ENDIF
      ENDDO

      ! gwres_flow only reduced by cascading flow to HRUs
      Gwres_flow = Gwres_flow - Dnflow
      IF ( Gwres_flow.LT.0.0 ) Gwres_flow = 0.0

      END SUBROUTINE rungw_cascade


!***********************************************************************
! Sums inflow to groundwater reservoirs and computes outflow to
! streamflow and to a sink if specified, includes cascading flow
!***********************************************************************

      MODULE PRMS_GWFLOW_CASC
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Nssr, Ngw, Ncascdgw, Nsegment, Nsegmentp1
      INTEGER :: Iputflg
      REAL :: Cfs_conv
!   Declared Variables
      REAL :: Basin_gwflow, Basin_gwstor, Basin_gwsink, Basin_gwin
      REAL, ALLOCATABLE :: Gwres_stor(:), Gwres_flow(:), Gwres_in(:)
      REAL, ALLOCATABLE :: Gwres_sink(:), Gw_in_ssr(:), Gw_in_soil(:)
      REAL, ALLOCATABLE :: Gw_upslope(:)
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug, Active_hrus, Active_gwrs
      REAL :: Basin_area_inv
      INTEGER, ALLOCATABLE :: Gwr_route_order(:), Hru_route_order(:)
      INTEGER, ALLOCATABLE :: Ncascade_gwr(:)
      REAL, ALLOCATABLE :: Hru_perv(:), Ssres_area(:), Gwres_area(:)
!   Declared Variables from other modules - ssflow or soilzone
      REAL, ALLOCATABLE :: Soil_to_gw(:), Ssr_to_gw(:)
!   Declared Variables from other modules - srunoff
! Strm_seg_in can be updated
      REAL, ALLOCATABLE :: Strm_seg_in(:)
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Hru_gwres(:)
      REAL, ALLOCATABLE :: Gwflow_coef(:), Gwsink_coef(:)
      REAL, ALLOCATABLE :: Gwstor_init(:), Hru_area(:)
      END MODULE PRMS_GWFLOW_CASC

!***********************************************************************
!     Main gwflow routine
!***********************************************************************
      INTEGER FUNCTION gwflow_casc_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: gwcdecl, gwcinit, gwcrun
!***********************************************************************
      gwflow_casc_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        gwflow_casc_prms = gwcrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        gwflow_casc_prms = gwcdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        gwflow_casc_prms = gwcinit()
      ENDIF

      END FUNCTION gwflow_casc_prms
!***********************************************************************
!     gwcdecl - set up parameters for groundwater computations
!   Declared Parameters
!     hru_gwres, gwstor_init, gwflow_coef, gwsink_coef, hru_area
!***********************************************************************

      INTEGER FUNCTION gwcdecl()
      USE PRMS_GWFLOW_CASC
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      gwcdecl = 1

      IF ( declmodule(
     +'$Id: gwflow_casc_prms.f 3868 2008-02-13 20:55:36Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nssr = getdim('nssr')
      IF ( Nssr.EQ.-1 ) RETURN
      IF ( Nhru.NE.Nssr ) THEN
        PRINT *, 'Error, using gwflow_casc and nhru /= nssr', Nhru, Nssr
        RETURN
      ENDIF

      Ngw = getdim('ngw')
      IF ( Ngw.EQ.-1 ) RETURN

      Ncascdgw = getdim('ncascdgw')
      IF ( Ncascdgw.EQ.-1 ) RETURN

      Nsegment = getdim('nsegment')
      IF ( Nsegment.EQ.-1 ) RETURN
      Nsegmentp1 = Nsegment + 1

! cascading variables and parameters
      ALLOCATE (Gw_upslope(Ngw))
      IF ( declvar('gwflow', 'gw_upslope', 'ngw', Ngw, 'real',
     +     'Ground-water flow received from upslope ground-water'//
     +     ' reservoirs',
     +     'acre-inches',
     +     Gw_upslope).NE.0 ) RETURN

      ALLOCATE (Gwres_stor(Ngw))
      IF ( declvar('gwflow', 'gwres_stor', 'ngw', Ngw, 'real',
     +     'Storage in each groundwater reservoir',
     +     'inches',
     +     Gwres_stor).NE.0 ) RETURN

      ALLOCATE (Gwres_flow(Ngw))
      IF ( declvar('gwflow', 'gwres_flow', 'ngw', Ngw, 'real',
     +     'Outflow from each groundwater reservoir',
     +     'inches',
     +     Gwres_flow).NE.0 ) RETURN
      Gwres_flow = 0.0

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
      Basin_gwflow = 0.0

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

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('gwflow', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_gwres(Nhru))
      IF ( Nhru.NE.Ngw ) THEN
        IF ( declparam('gwflow', 'hru_gwres', 'nhru', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'Index of groundwater reservoir assigned to HRU',
     +       'Index of groundwater reservoir receiving excess soil'//
     +       ' water from each HRU',
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
     +     'Groundwater sink coefficeint',
     +     'Groundwater sink coefficient - is multiplied by the'//
     +     ' storage in the groundwater reservoir to compute the'//
     +     ' seepage from each reservoir to the groundwater sink.',
     +     '1/day').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Gwr_route_order(Ngw), Hru_route_order(Nhru))
      ALLOCATE (Ncascade_gwr(Ngw), Gwres_area(Ngw))
      ALLOCATE (Hru_perv(Nhru), Ssres_area(Nssr))
      ALLOCATE (Soil_to_gw(Nhru), Ssr_to_gw(Nssr))
      ALLOCATE (Strm_seg_in(Nsegmentp1))

      gwcdecl = 0
      END FUNCTION gwcdecl

!***********************************************************************
!     gwcinit - Initialize gwflow module - get parameter values,
!               compute initial values.
!***********************************************************************
      INTEGER FUNCTION gwcinit()
      USE PRMS_GWFLOW_CASC
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, j, jj
!***********************************************************************
      gwcinit = 1

      IF ( getparam('gwflow', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getparam('gwflow', 'gwstor_init', Ngw, 'real',
     +     Gwstor_init).NE.0 ) RETURN

      IF ( getparam('gwflow', 'gwflow_coef', Ngw, 'real',
     +     Gwflow_coef).NE.0 ) RETURN

      IF ( getparam('gwflow', 'gwsink_coef', Ngw, 'real',
     +     Gwsink_coef).NE.0 ) RETURN

      IF ( Nhru.NE.Ngw ) THEN
        IF ( getparam('gwflow', 'hru_gwres', Nhru, 'integer',
     +       Hru_gwres).NE.0 ) RETURN
      ELSE
        DO i = 1, Nhru
          Hru_gwres(i) = i
        ENDDO
      ENDIF

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'ssres_area', Nssr, 'real', Ssres_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'gwr_route_order', Ngw, 'integer',
     +     Gwr_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'ncascade_gwr', Ngw, 'integer', Ncascade_gwr)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_gwrs', 1, 'integer', Active_gwrs)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'gwres_area', Ngw, 'real', Gwres_area)
     +     .NE.0 ) RETURN

      IF ( getstep().EQ.0 ) THEN
        Gw_upslope = 0.0
        Gwres_flow = 0.0
        Gwres_in = 0.0
        Gwres_sink = 0.0
        Gw_in_ssr = 0.0
        Gw_in_soil = 0.0
        Basin_gwflow = 0.0
        Basin_gwsink = 0.0
        Basin_gwin = 0.0
! do only once, so restart uses saved values
        Gwres_stor = Gwstor_init
      ENDIF

      Basin_gwstor = 0.0
      DO jj = 1, Active_gwrs
        j = Gwr_route_order(jj)
        Basin_gwstor = Basin_gwstor + Gwres_stor(j)*Gwres_area(j)
      ENDDO
      Basin_gwstor = Basin_gwstor*Basin_area_inv

      IF ( Prt_debug.EQ.1 ) OPEN (191, FILE='gwflow_casc_prms.wbal')

      gwcinit = 0
      END FUNCTION gwcinit

!***********************************************************************
!     gwcrun - Computes groundwater flow to streamflow and to
!              groundwater sink
!***********************************************************************
      INTEGER FUNCTION gwcrun()
      USE PRMS_GWFLOW_CASC
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      EXTERNAL rungw_cascade
      INTRINSIC SNGL, ABS
! Local Variables
      INTEGER :: i, j, ii, nowtime(6)
      REAL :: td, gwarea, gwin, gwstor, gwsink, gwflow
      REAL :: last_gwstor, gwbal, dnflow, basin_dnflow
!***********************************************************************
      gwcrun = 1
      Iputflg = 0

!*****ts= timesteps in a day, td = timestep in days
!     ts = SNGL(24.D0/deltim())
      td = SNGL(deltim()/24.D0)

      IF ( Ncascdgw.GT.0 ) THEN
        IF ( getvar('srunoff', 'strm_seg_in', Nsegmentp1, 'real',
     +       Strm_seg_in).NE.0 ) RETURN
! convert timestep in hours to seconds
! Cfs_conv converts acre-inches per timestep to cfs
        Cfs_conv = SNGL(43560.0D0/12.0D0/(deltim()*3600.0D0))
      ENDIF

      IF ( getvar('smbal', 'soil_to_gw', Nhru, 'real', Soil_to_gw)
     +     .NE.0 ) RETURN

      IF ( getvar('ssflow', 'ssr_to_gw', Nssr, 'real', Ssr_to_gw)
     +     .NE.0 ) RETURN

!rsr, get in case state has been updated
      IF ( getvar('basin', 'hru_perv', Nhru, 'real', Hru_perv)
     +     .NE.0 ) RETURN

      DO ii = 1, Active_gwrs
        i = Gwr_route_order(ii)
        Gwres_stor(i) = Gwres_stor(i)*Gwres_area(i)
      ENDDO

!******Sum the inflows to each groundwater reservoir

      Gw_in_soil = 0.0
      Gw_in_ssr = 0.0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        j = Hru_gwres(i)
        Gw_in_soil(j) = Gw_in_soil(j) + (Soil_to_gw(i)*Hru_perv(i))
        Gw_in_ssr(i) = Gw_in_ssr(i) + (Ssr_to_gw(i)*Ssres_area(i))
      ENDDO

      last_gwstor = Basin_gwstor
      basin_dnflow = 0.
      Basin_gwflow = 0.
      Basin_gwstor = 0.
      Basin_gwsink = 0.
      Basin_gwin = 0.
      Gw_upslope = 0.

      DO j = 1, Active_gwrs
        i = Gwr_route_order(j)
        gwarea = Gwres_area(i)
        gwin = Gw_in_soil(i) + Gw_in_ssr(i) + Gw_upslope(i)
        gwstor = Gwres_stor(i) + gwin
        gwflow = (gwstor*Gwflow_coef(i))*td
        gwstor = gwstor - gwflow
        IF ( Gwsink_coef(i).GT.0.0 ) THEN
          gwsink = (gwstor*Gwsink_coef(i))*td
          gwstor = gwstor - gwsink
          IF ( gwstor.LT.0.0 ) gwstor = 0.0
          Gwres_sink(i) = gwsink/gwarea
        ELSE
          gwsink = 0.
          Gwres_sink(i) = 0.
        ENDIF
        dnflow = 0.
        IF ( Ncascade_gwr(i).GT.0 )
     +       CALL rungw_cascade(i, Ncascade_gwr(i), gwflow, dnflow)
        Basin_gwflow = Basin_gwflow + gwflow
        Basin_gwstor = Basin_gwstor + gwstor
        Basin_gwsink = Basin_gwsink + gwsink
        Basin_gwin = Basin_gwin + gwin
        IF ( Prt_debug.EQ.1 ) THEN
          basin_dnflow = basin_dnflow + dnflow
          gwbal = (Gwres_stor(i) - gwstor + gwin - gwflow - gwsink
     +            - dnflow)/gwarea
          IF ( ABS(gwbal).GT.5.E-5 ) WRITE (191, *)
     +         'GWR water balance issue', i, gwbal, Gwres_stor(i), gwin,
     +         gwstor, gwflow, gwsink, dnflow, Gw_in_soil(i),
     +         Gw_in_ssr(i), Gw_upslope(i)
        ENDIF
        Gwres_flow(i) = gwflow/gwarea
        Gwres_stor(i) = gwstor/gwarea
        Gwres_in(i) = gwin/gwarea
        Gw_in_ssr(i) = Gw_in_ssr(i)/gwarea
        Gw_in_soil(i) = Gw_in_soil(i)/gwarea
      ENDDO

      Basin_gwflow = Basin_gwflow*Basin_area_inv
      Basin_gwstor = Basin_gwstor*Basin_area_inv
      Basin_gwsink = Basin_gwsink*Basin_area_inv
      Basin_gwin = Basin_gwin*Basin_area_inv

      IF ( Prt_debug.EQ.1 ) THEN
        basin_dnflow = basin_dnflow*Basin_area_inv
        gwbal = last_gwstor - Basin_gwstor + Basin_gwin - Basin_gwflow
     +          - Basin_gwsink - basin_dnflow
        IF ( ABS(gwbal).GT.1.0E-4 ) THEN
          CALL dattim('now', nowtime)
          WRITE (191, *) nowtime
          WRITE (191, *) 'GWR basin water balance issue', gwbal,
     +                  last_gwstor, Basin_gwstor, Basin_gwin,
     +                  Basin_gwflow, Basin_gwsink, basin_dnflow
        ENDIF
      ENDIF

      gwcrun = 0

      IF ( Iputflg.EQ.1 ) gwcrun = putvar('srunoff', 'strm_seg_in',
     +     Nsegmentp1, 'real', Strm_seg_in)

      END FUNCTION gwcrun

!***********************************************************************
!     Compute cascading GW flow
!***********************************************************************
      SUBROUTINE rungw_cascade(Igwr, Ncascade_gwr, Gwres_flow, Dnflow)
      USE PRMS_GWFLOW_CASC, ONLY:Gw_upslope, Strm_seg_in, Iputflg,
     +                           Cfs_conv
      USE PRMS_CASCADE, ONLY: Gwr_down, Gwr_down_pct
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Igwr, Ncascade_gwr
      REAL, INTENT(INOUT) :: Gwres_flow, Dnflow
! Local variables
      INTEGER :: j, k
      REAL :: pctflow
!***********************************************************************
      DO k = 1, Ncascade_gwr
        j = Gwr_down(k, Igwr)
        ! Gwres_flow is in acre-inches
        pctflow = Gwres_flow*Gwr_down_pct(k, Igwr)
! if gwr_down(k, Igwr) > 0, cascade contributes to a downslope GWR
        IF ( j.GT.0 ) THEN
          Gw_upslope(j) = Gw_upslope(j) + pctflow
          Dnflow = Dnflow + pctflow
! if gwr_down(k, Igwr) < 0, cascade contributes to a stream
        ELSEIF ( j.LT.0 ) THEN
          j = IABS(j)
          Strm_seg_in(j) = Strm_seg_in(j) + pctflow*Cfs_conv
          Iputflg = 1
        ENDIF
      ENDDO

! reset Ssres_flow and Excess_flow as they accumulate flow to streams
      Gwres_flow = Gwres_flow - Dnflow
      IF ( Gwres_flow.LT.0.0 ) Gwres_flow = 0.

      END SUBROUTINE rungw_cascade

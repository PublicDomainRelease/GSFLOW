!***********************************************************************
! Computes streamflow and other variables in subbasins
!
!     version: 3.1 (rsregan, April 2008 ) changed code to assume
!                  nhru=nssr=ngw
!     version: 3.0 (rsregan, April 2008 ) removed all but subbasin code
!     version: 2.1 (rsregan, June 2007)
!     version: 2 (lhay, May 2007)
!     version: 1.1 (rsregan, March 2007)
!     version: 1 (lhay, March 2007)
!***********************************************************************

      MODULE PRMS_SUBBASIN
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=8), SAVE :: MODNAME
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Qsub(:), Sub_area(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Subincstor(:), Laststor(:)
      INTEGER, SAVE, ALLOCATABLE :: Tree(:, :)
!   Declared Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sub_inq(:), Sub_cfs(:), Sub_cms(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sub_interflow(:), Sub_gwflow(:), Sub_sroff(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Subinc_interflow(:), Subinc_gwflow(:), Subinc_sroff(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Subinc_precip(:), Subinc_snowmelt(:), Subinc_pkweqv(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Subinc_actet(:), Subinc_potet(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Subinc_swrad(:), Subinc_snowcov(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Subinc_tmaxc(:), Subinc_tminc(:), Subinc_tavgc(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Subinc_deltastor(:), Subinc_wb(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Subbasin_down(:), Hru_subbasin(:)
      END MODULE PRMS_SUBBASIN

!***********************************************************************
!     Main daily stream flow routine
!***********************************************************************
      INTEGER FUNCTION subbasin()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: subdecl, subinit, subrun
!***********************************************************************
      subbasin = 0

      IF ( Process(:3)=='run' ) THEN
        subbasin = subrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        subbasin = subdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        subbasin = subinit()
      ENDIF

      END FUNCTION subbasin

!***********************************************************************
!     subdecl - set up parameters for streamflow, lake computations,
!               and subbasin flow
!   Declared Parameters
!     hru_area, subbasin_down, hru_subbasin
!***********************************************************************
      INTEGER FUNCTION subdecl()
      USE PRMS_SUBBASIN
      USE PRMS_MODULE, ONLY: Model, Nsub, Nhru
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_subbasin
!***********************************************************************
      subdecl = 0

      Version_subbasin = '$Id: subbasin.f90 7432 2015-06-10 21:50:54Z rsregan $'
      CALL print_module(Version_subbasin, 'Output Summary              ', 90)
      MODNAME = 'subbasin'

      IF ( Nsub==0 ) THEN
        IF ( Model/=99 ) STOP 'ERROR, nsub=0 when subbasin module called'
        Nsub = 1
      ENDIF

! Declared Variables
      ALLOCATE ( Sub_interflow(Nsub) )
      IF ( declvar(MODNAME, 'sub_interflow', 'nsub', Nsub, 'double', &
     &     'Area-weighted average interflow to each subbasin from'// &
     &     ' associated HRUs and from upstream subbasins', &
     &     'cfs', Sub_interflow)/=0 ) CALL read_error(3, 'sub_interflow')

      ALLOCATE ( Sub_gwflow(Nsub) )
      IF ( declvar(MODNAME, 'sub_gwflow', 'nsub', Nsub, 'double', &
     &     'Area-weighted average groundwater discharge from'// &
     &     ' associated GWRs to each subbasin and from upstream subbasins', &
     &     'cfs', Sub_gwflow)/=0 ) CALL read_error(3, 'sub_gwflow')

      ALLOCATE ( Sub_sroff(Nsub) )
      IF ( declvar(MODNAME, 'sub_sroff', 'nsub', Nsub, 'double', &
     &     'Area-weighted average surface runoff from associated HRUs'// &
     &     ' to each subbasin and from upstream subbasins', &
     &     'cfs', Sub_sroff)/=0 ) CALL read_error(3, 'sub_sroff')

      ALLOCATE ( Subinc_snowcov(Nsub) )
      IF ( declvar(MODNAME, 'subinc_snowcov', 'nsub', Nsub, 'double', &
     &     'Area-weighted average snow-covered area for associated HRUs for each subbasin', &
     &     'decimal fraction', Subinc_snowcov)/=0 ) CALL read_error(3, 'subinc_snowcov')

      ALLOCATE ( Subinc_interflow(Nsub) )
      IF ( declvar(MODNAME, 'subinc_interflow', 'nsub', Nsub, 'double', &
     &     'Area-weighted average interflow from associated HRUs to each subbasin', &
     &     'cfs', Subinc_interflow)/=0 ) CALL read_error(3, 'subinc_interflow')

      ALLOCATE ( Subinc_gwflow(Nsub) )
      IF ( declvar(MODNAME, 'subinc_gwflow', 'nsub', Nsub, 'double', &
     &     'Area-weighted average groundwater discharge from associated  GWRs to each subbasin', &
     &     'cfs', Subinc_gwflow)/=0 ) CALL read_error(3, 'subinc_gwflow')

      ALLOCATE ( Subinc_sroff(Nsub) )
      IF ( declvar(MODNAME, 'subinc_sroff', 'nsub', Nsub, 'double', &
     &    'Area-weighted average surface runoff from associated HRUs to each subbasin', &
     &    'cfs', Subinc_sroff)/=0 ) CALL read_error(3, 'subinc_sroff')

      ALLOCATE ( Subinc_precip(Nsub) )
      IF ( declvar(MODNAME, 'subinc_precip', 'nsub', Nsub, 'double', &
     &     'Area-weighted average precipitation from associated HRUs to each subbasin', &
     &     'inches', Subinc_precip)/=0 ) CALL read_error(3, 'subinc_precip')

      ALLOCATE ( Subinc_actet(Nsub) )
      IF ( declvar(MODNAME, 'subinc_actet', 'nsub', Nsub, 'double', &
     &     'Area-weighted average actual ET from associated HRUs to each subbasin', &
     &     'inches', Subinc_actet)/=0 ) CALL read_error(3, 'subinc_actet')

      ALLOCATE ( Subinc_potet(Nsub) )
      IF ( declvar(MODNAME, 'subinc_potet', 'nsub', Nsub, 'double', &
     &     'Area-weighted average potential ET from associated HRUs to each subbasin', &
     &     'inches', Subinc_potet)/=0 ) CALL read_error(3, 'subinc_potet')

      ALLOCATE ( Subinc_swrad(Nsub) )
      IF ( declvar(MODNAME, 'subinc_swrad', 'nsub', Nsub, 'double', &
     &     'Area-weighted average shortwave radiation distributed'// &
     &     ' to associated HRUs of each subbasin', &
     &     'Langleys', Subinc_swrad)/=0 ) CALL read_error(3, 'subinc_swrad')

      ALLOCATE ( Subinc_tminc(Nsub) )
      IF ( declvar(MODNAME, 'subinc_tminc', 'nsub', Nsub, 'double', &
     &     'Area-weighted average minimum air temperature for'// &
     &     ' associated HRUs to each subbasin', &
     &     'degrees Celsius', Subinc_tminc)/=0 ) CALL read_error(3, 'subinc_tminc')

      ALLOCATE ( Subinc_tmaxc(Nsub) )
      IF ( declvar(MODNAME, 'subinc_tmaxc', 'nsub', Nsub, 'double', &
     &     'Area-weighted average maximum air temperature for'// &
     &     ' associated HRUs to each subbasin', &
     &     'degrees Celsius', Subinc_tmaxc)/=0 ) CALL read_error(3, 'subinc_tmaxc')

      ALLOCATE ( Subinc_tavgc(Nsub) )
      IF ( declvar(MODNAME, 'subinc_tavgc', 'nsub', Nsub, 'double', &
     &     'Area-weighted average air temperature for associated HRUs to each subbasin', &
     &     'degrees Celsius', Subinc_tavgc)/=0 ) CALL read_error(3, 'subinc_tavgc')

      ALLOCATE ( Subinc_wb(Nsub) )
      IF ( declvar(MODNAME, 'subinc_wb', 'nsub', Nsub, 'double', &
     &     'Water balance for each subbasin', &
     &     'cfs', Subinc_wb)/=0 ) CALL read_error(3, 'subinc_wb')

      ALLOCATE ( Subinc_deltastor(Nsub) )
      IF ( declvar(MODNAME, 'subinc_deltastor', 'nsub', Nsub, 'double', &
     &     'Change in storage for each subbasin', &
     &     'cfs', Subinc_deltastor)/=0 ) CALL read_error(3, 'subinc_deltastor')

      ALLOCATE ( Subinc_snowmelt(Nsub) )
      IF ( declvar(MODNAME, 'subinc_snowmelt', 'nsub', Nsub, 'double', &
     &     'Area-weighted average snowmelt from associated HRUs of each subbasin', &
     &     'inches', Subinc_snowmelt)/=0 ) CALL read_error(3, 'subinc_snowmelt')

      ALLOCATE ( Subinc_pkweqv(Nsub) )
      IF ( declvar(MODNAME, 'subinc_pkweqv', 'nsub', Nsub, 'double', &
     &     'Area-weighted average snowpack water equivalent from'// &
     &     ' associated HRUs of each subbasin', &
     &     'inches', Subinc_pkweqv)/=0 ) CALL read_error(3, 'subinc_pkweqv')

      ALLOCATE ( Qsub(Nsub), Tree(Nsub, Nsub), Sub_inq(Nsub) )
      IF ( declvar(MODNAME, 'sub_inq', 'nsub', Nsub, 'double', &
     &     'Sum of streamflow from upstream subbasins to each subbasin', &
     &     'cfs', Sub_inq)/=0 ) CALL read_error(3, 'sub_inq')

      ALLOCATE ( Sub_cfs(Nsub) )
      IF ( declvar(MODNAME, 'sub_cfs', 'nsub', Nsub, 'double', &
     &     'Total streamflow leaving each subbasin', &
     &     'cfs', Sub_cfs)/=0 ) CALL read_error(3, 'sub_cfs')

      ALLOCATE ( Sub_cms(Nsub) )
      IF ( declvar(MODNAME, 'sub_cms', 'nsub', Nsub, 'double', &
     &     'Total streamflow leaving each subbasin', &
     &     'cms', Sub_cms)/=0 ) CALL read_error(3, 'sub_cms')

      ALLOCATE ( Subbasin_down(Nsub) )
      IF ( declparam(MODNAME, 'subbasin_down', 'nsub', 'integer', &
     &     '0', 'bounded', 'nsub', &
     &     'Downstream subbasin for each subbasin', &
     &     'Index number for the downstream subbasin whose inflow is outflow from this subbasin', &
     &     'none')/=0 ) CALL read_error(1, 'subbasin_down')

      ALLOCATE ( Hru_subbasin(Nhru) )
      IF ( declparam(MODNAME, 'hru_subbasin', 'nhru', 'integer', &
     &     '0', 'bounded', 'nsub', &
     &     'Index of subbasin assigned to each HRU', &
     &     'Index of subbasin assigned to each HRU', &
     &     'none')/=0 ) CALL read_error(1, 'hru_subbasin')

! Allocate arrays for variables
      ALLOCATE ( Sub_area(Nsub), Subincstor(Nsub), Laststor(Nsub) )

      END FUNCTION subdecl

!***********************************************************************
!     subinit - Initialize subbasin module - get parameter values,
!               compute initial values
!***********************************************************************
      INTEGER FUNCTION subinit()
      USE PRMS_SUBBASIN
      USE PRMS_MODULE, ONLY: Model, Nsub, Nhru, Print_debug, Inputerror_flag
      USE PRMS_BASIN, ONLY: Hru_area_dble, Active_hrus, Hru_route_order, &
     &    Hru_type, Hru_frac_perv, DNEARZERO
      USE PRMS_FLOWVARS, ONLY: Ssres_stor, Soil_moist, Pkwater_equiv, Gwres_stor
      USE PRMS_INTCP, ONLY: Hru_intcpstor
      USE PRMS_SRUNOFF, ONLY: Hru_impervstor
      IMPLICIT NONE
      INTRINSIC :: DBLE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error, PRMS_open_module_file
! Local Variables
      INTEGER :: i, j, k, kk, TREEUNIT
      DOUBLE PRECISION :: harea, gwstor, soilstor, snowstor, landstor
!***********************************************************************
      subinit = 0

      IF ( getparam(MODNAME, 'hru_subbasin', Nhru, 'integer', Hru_subbasin)/=0 ) CALL read_error(2, 'hru_subbasin')
      IF ( getparam(MODNAME, 'subbasin_down', Nsub, 'integer', Subbasin_down)/=0 ) CALL read_error(2, 'subbasin_down')

! Determine the tree structure for the internal nodes
      Tree = 0

      DO j = 1, Nsub
        kk = Subbasin_down(j)
        IF ( kk/=0 ) Tree(kk, j) = 1
      ENDDO

      Sub_cfs = 0.0D0
      Sub_cms = 0.0D0
      Sub_inq = 0.0D0
      Subinc_interflow = 0.0D0
      Subinc_gwflow = 0.0D0
      Subinc_sroff = 0.0D0
      Subinc_precip = 0.0D0
      Subinc_snowmelt = 0.0D0
      Subinc_pkweqv = 0.0D0
      Subinc_actet = 0.0D0
      Subinc_snowcov = 0.0D0
      Subinc_swrad = 0.0D0
      Subinc_tminc = 0.0D0
      Subinc_tmaxc = 0.0D0
      Subinc_tavgc = 0.0D0
      Subinc_potet = 0.0D0
      Subinc_wb = 0.0D0
      Subinc_deltastor = 0.0D0

      IF ( Print_debug==14 ) THEN
        CALL PRMS_open_module_file(TREEUNIT, 'tree_structure')
        WRITE ( TREEUNIT, 9001 ) (j, j=1, Nsub)
        DO j = 1, Nsub
          WRITE ( TREEUNIT, 9002 ) j, (Tree(j,k), k=1, Nsub)
        ENDDO
      ENDIF

      DO j = 1, Nsub
        DO k = 1, Nsub
          IF ( Tree(j,k)>0 ) THEN
            DO i = 1, Nsub
              Tree(j, i) = Tree(j, i) + Tree(k, i)
              IF ( Tree(j,i)>1 ) THEN
                Tree(j, i) = 1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      DO j = Nsub, 1, -1
        DO k = 1, Nsub
          IF ( Tree(j,k)>0 ) THEN
            DO i = 1, Nsub
              Tree(j, i) = Tree(j, i) + Tree(k, i)
              IF ( Tree(j,i)>1 ) Tree(j,i)=1
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      IF ( Print_debug==14 ) THEN
        WRITE ( TREEUNIT, 9003 ) (j, j=1, Nsub)
        DO j = 1, Nsub
          WRITE ( TREEUNIT, 9002 ) j, (Tree(j,k), k=1, Nsub)
        ENDDO
        CLOSE ( TREEUNIT )
      ENDIF

      Subincstor = 0.0D0
      Sub_area = 0.0D0
      gwstor = 0.0D0
      DO i = 1, Active_hrus
        j = Hru_route_order(i)
        ! k indicates which HRU is in which subbasin
        k = Hru_subbasin(j)
        IF ( k>0 ) THEN
          harea = Hru_area_dble(j)
          IF ( Hru_type(j)/=2 ) THEN
            IF ( Model/=0 ) gwstor = Gwres_stor(j)*harea
            soilstor = DBLE(Soil_moist(j)*Hru_frac_perv(j) + Ssres_stor(j))*harea
            snowstor = Pkwater_equiv(j)*harea
            landstor = DBLE(Hru_intcpstor(j)+Hru_impervstor(j))*harea
          ELSE
            gwstor = 0.0D0
            soilstor = 0.0D0
            snowstor = 0.0D0
            landstor = 0.0D0
          ENDIF
          Subincstor(k) = Subincstor(k) + soilstor + gwstor + snowstor + landstor
          Sub_area(k) = Sub_area(k) + harea
        ENDIF
      ENDDO
      DO i = 1, Nsub
        IF ( Sub_area(i)<DNEARZERO ) THEN
          PRINT *, 'ERROR, subbasin:', i, ' does not include any HRUs'
          Inputerror_flag = 1
        ELSE
          Subincstor(i) = Subincstor(i)/Sub_area(i)
        ENDIF
      ENDDO

 9001 FORMAT ('Initial Tree Structure for Internal Subbasins', /, &
     &        ' 1 indicates a node that flows directly into subbasin', /, 5X, 500I3)
 9002 FORMAT (I3, 2X, 500I3)
 9003 FORMAT (/, 'Tree Structure for Internal Subbasins', /, &
     &        ' 1 indicates a node that eventually flows into subbasin', /, 5X, 500I3)

      END FUNCTION subinit

!***********************************************************************
!     subrun - Computes basin streamflow and on-channel reservoir
!                  storage and outflows
!***********************************************************************
      INTEGER FUNCTION subrun()
      USE PRMS_SUBBASIN
      USE PRMS_MODULE, ONLY: Model, Nsub, Cascade_flag
      USE PRMS_BASIN, ONLY: Hru_area_dble, Active_hrus, Hru_route_order, &
     &    Hru_type, CFS2CMS_CONV, Hru_frac_perv
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SNOW, ONLY: Snowcov_area, Snowmelt
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Swrad, Potet, Tminc, Tmaxc, Tavgc
      USE PRMS_FLOWVARS, ONLY: Hru_actet, Ssres_flow, Sroff, &
     &    Ssres_stor, Soil_moist, Pkwater_equiv, Gwres_stor
      USE PRMS_INTCP, ONLY: Hru_intcpstor
      USE PRMS_SRUNOFF, ONLY: Hru_impervstor, Hortonian_lakes
      USE PRMS_SOILZONE, ONLY: Lakein_sz
      USE PRMS_GWFLOW, ONLY: Gwres_flow
      IMPLICIT NONE
! Local Variables
      INTEGER :: j, jj, k
      DOUBLE PRECISION :: harea, srq, ssq, gwq, dmy, dmy1, subarea
      DOUBLE PRECISION :: soilstor, gwstor, snowstor, landstor, dmy2
!     DOUBLE PRECISION :: conv
!***********************************************************************
      subrun = 0

!   Daily time step.
!   Compute reservoir routing and basin outflow

      !rsr, not getting groundwater storage and flow for GSFLOW mode

      DO j = 1, Nsub
        Sub_cfs(j) = 0.0D0
        Qsub(j) = 0.0D0
        Sub_inq(j) = 0.0D0
        Subinc_interflow(j) = 0.0D0
        Subinc_gwflow(j) = 0.0D0
        Subinc_sroff(j) = 0.0D0
        Subinc_precip(j) = 0.0D0
        Subinc_snowmelt(j) = 0.0D0
        Subinc_pkweqv(j) = 0.0D0
        Subinc_actet(j) = 0.0D0
        Subinc_snowcov(j) = 0.0D0
        Subinc_swrad(j) = 0.0D0
        Subinc_tminc(j) = 0.0D0
        Subinc_tmaxc(j) = 0.0D0
        Subinc_tavgc(j) = 0.0D0
        Subinc_potet(j) = 0.0D0
        Subinc_wb(j) = 0.0D0
      ENDDO

      Laststor = Subincstor
      Subincstor = 0.0D0

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        ! k indicates which HRU is in which subbasin
        k = Hru_subbasin(j)
        IF ( k/=0 ) THEN
          harea = Hru_area_dble(j)
          srq = 0.0D0
          ssq = 0.0D0
          gwq = 0.0D0
          gwstor = 0.0D0
          soilstor = 0.0D0
          snowstor = 0.0D0
          landstor = 0.0D0
          IF ( Hru_type(j)/=2 ) THEN
            srq = DBLE(Sroff(j))*harea
            ssq = DBLE(Ssres_flow(j))*harea
            IF ( Model/=0 ) THEN
              gwq = DBLE(Gwres_flow(j))*harea
              gwstor = Gwres_stor(j)*harea
            ENDIF
            soilstor = DBLE(Soil_moist(j)*Hru_frac_perv(j) + Ssres_stor(j))*harea
            snowstor = Pkwater_equiv(j)*harea
            landstor = DBLE(Hru_intcpstor(j)+Hru_impervstor(j))*harea
          !rsr???, this is probably wrong
          ELSEIF ( Cascade_flag==1 ) THEN
            srq = Hortonian_lakes(j)*harea
            ssq = Lakein_sz(j)*harea
          ENDIF
          Qsub(k) = Qsub(k) + srq + ssq + gwq
          Subinc_interflow(k) = Subinc_interflow(k) + ssq
          IF ( Model/=0 ) Subinc_gwflow(k) = Subinc_gwflow(k) + gwq
          Subinc_sroff(k) = Subinc_sroff(k) + srq
          Subinc_precip(k) = Subinc_precip(k) + DBLE(Hru_ppt(j))*harea
          Subinc_actet(k) = Subinc_actet(k) + DBLE(Hru_actet(j))*harea
          Subinc_snowmelt(k) = Subinc_snowmelt(k) + DBLE(Snowmelt(j))*harea
          Subinc_pkweqv(k) = Subinc_pkweqv(k) + Pkwater_equiv(j)*harea
          Subinc_snowcov(k) = Subinc_snowcov(k) + DBLE(Snowcov_area(j))*harea
          Subinc_potet(k) = Subinc_potet(k) + DBLE(Potet(j))*harea
          Subinc_swrad(k) = Subinc_swrad(k) + DBLE(Swrad(j))*harea
          Subinc_tminc(k) = Subinc_tminc(k) + DBLE(Tminc(j))*harea
          Subinc_tmaxc(k) = Subinc_tmaxc(k) + DBLE(Tmaxc(j))*harea
          Subinc_tavgc(k) = Subinc_tavgc(k) + DBLE(Tavgc(j))*harea
          Subincstor(k) = Subincstor(k) + soilstor + gwstor + snowstor + landstor
        ENDIF
      ENDDO

      !convert first as subbasins don't have to be in order
      DO j = 1, Nsub
        subarea = Sub_area(j)
        Sub_inq(j) = Qsub(j)*Cfs_conv
        dmy = Subinc_interflow(j)/subarea
        dmy1 = Subinc_gwflow(j)/subarea
        dmy2 = Subinc_sroff(j)/subarea
        Subinc_interflow(j) = Subinc_interflow(j)*Cfs_conv
        Subinc_gwflow(j) = Subinc_gwflow(j)*Cfs_conv
        Subinc_sroff(j) = Subinc_sroff(j)*Cfs_conv
        Subinc_precip(j) = Subinc_precip(j)/subarea
        Subinc_actet(j) = Subinc_actet(j)/subarea
        Subinc_snowmelt(j) = Subinc_snowmelt(j)/subarea
        Subinc_pkweqv(j) = Subinc_pkweqv(j)/subarea
        Subinc_snowcov(j) = Subinc_snowcov(j)/subarea
        Subinc_potet(j) = Subinc_potet(j)/subarea
        Subinc_swrad(j) = Subinc_swrad(j)/subarea
        Subinc_tminc(j) = Subinc_tminc(j)/subarea
        Subinc_tmaxc(j) = Subinc_tmaxc(j)/subarea
        Subinc_tavgc(j) = Subinc_tavgc(j)/subarea
        Subincstor(j) = Subincstor(j)/subarea
        Subinc_deltastor(j) = Laststor(j) - Subincstor(j)
        Subinc_wb(j) = Subinc_precip(j) - Subinc_actet(j) - &
     &                 dmy - dmy1 - dmy2 + Subinc_deltastor(j)
      ENDDO

      !get cummulative subbasin flows
      DO j = 1, Nsub
        Sub_gwflow(j) = Subinc_gwflow(j)
        Sub_sroff(j) = Subinc_sroff(j)
        Sub_interflow(j) = Subinc_interflow(j)
        DO k = 1, Nsub
          IF ( Tree(j,k)/=0 ) THEN
            Sub_gwflow(j) = Sub_gwflow(j) + Subinc_gwflow(k)
            Sub_sroff(j) = Sub_sroff(j) + Subinc_sroff(k)
            Sub_interflow(j) = Sub_interflow(j) + Subinc_interflow(k)
          ENDIF
        ENDDO
      ENDDO

      DO j = 1, Nsub
        Sub_cfs(j) = Sub_inq(j)
        DO k = 1, Nsub
          IF ( Tree(j,k)/=0 ) Sub_cfs(j) = Sub_cfs(j) + Sub_inq(k)
        ENDDO
        Sub_cms(j) = Sub_cfs(j)*CFS2CMS_CONV
      ENDDO

      END FUNCTION subrun

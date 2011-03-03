!***********************************************************************
! Computes streamflow at internal basin nodes
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
      INTEGER, PARAMETER :: TREEUNIT = 445
      INTEGER, SAVE :: Num
      REAL, SAVE, ALLOCATABLE :: Qsub(:), Sub_area(:)
      REAL, SAVE, ALLOCATABLE :: Subincstor(:), Laststor(:)
      INTEGER, SAVE, ALLOCATABLE :: Tree(:, :)
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Sub_inq(:), Sub_cfs(:), Sub_cms(:)
      REAL, SAVE, ALLOCATABLE :: Sub_interflow(:), Sub_gwflow(:)
      REAL, SAVE, ALLOCATABLE :: Subinc_interflow(:), Subinc_gwflow(:)
      REAL, SAVE, ALLOCATABLE :: Subinc_sroff(:), Subinc_precip(:)
      REAL, SAVE, ALLOCATABLE :: Subinc_snowmelt(:), Subinc_pkweqv(:)
      REAL, SAVE, ALLOCATABLE :: Subinc_actet(:)
      REAL, SAVE, ALLOCATABLE :: Sub_sroff(:)
      REAL, SAVE, ALLOCATABLE :: Subinc_snowcov(:), Subinc_wb(:)
      REAL, SAVE, ALLOCATABLE :: Subinc_deltastor(:)
!   Declared Variables from other modules - ssflow
      REAL, ALLOCATABLE :: Ssres_stor(:)
!   Declared Variables from other modules - smbal or soilzone
      REAL, ALLOCATABLE :: Soil_moist(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Subbasin_down(:), Hru_subbasin(:)
      END MODULE PRMS_SUBBASIN

!***********************************************************************
!     Main daily stream flow routine
!***********************************************************************
      INTEGER FUNCTION subbasin_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: subdecl, subinit, subrun
!***********************************************************************
      subbasin_prms = 0

      IF ( Process_flag==0 ) THEN
        subbasin_prms = subrun()
      ELSEIF ( Process_flag==1 ) THEN
        subbasin_prms = subdecl()
      ELSEIF ( Process_flag==2 ) THEN
        subbasin_prms = subinit()
      ENDIF

      END FUNCTION subbasin_prms

!***********************************************************************
!     subdecl - set up parameters for streamflow, surface reservoir
!               flow computations, and subbasin flow
!   Declared Parameters
!     hru_area, subbasin_down, hru_subbasin
!***********************************************************************
      INTEGER FUNCTION subdecl()
      USE PRMS_SUBBASIN
      USE PRMS_MODULE, ONLY: Model, Nsub
      USE PRMS_BASIN, ONLY: Nhru, Nssr, Ngw
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      subdecl = 1

      IF ( declmodule(
     +'$Id: subbasin_prms.f 2505 2011-02-25 17:40:32Z rsregan $'
     +).NE.0 ) RETURN

      ! make sure nhru=nssr=ngw for GSFLOW mode
      IF ( Model==0 ) THEN
        IF ( Nssr/=Nhru ) PRINT *, 'Subbasin_prms:',
     +       ' nssr not equal nhru in GSFLOW model, nssr set to nhru'
        IF ( Ngw/=Nhru ) PRINT *, 'Subbasin_prms:',
     +       ' ngw not equal nhru in GSFLOW model, ngw set to nhru'
        Nssr = Nhru
        Ngw = Nhru
      ENDIF

      IF ( Model/=99 ) THEN
        IF ( Nhru.NE.Nssr ) THEN
          PRINT *, 'ERROR, subbasin module requires nssr=nhru'
          RETURN
        ENDIF
        IF ( Nhru.NE.Ngw ) THEN
          PRINT *, 'ERROR, subbasin module requires nssr=ngw'
          RETURN
        ENDIF
      ELSE
        Nsub = 1
      ENDIF

! Declared Variables
      ALLOCATE (Sub_interflow(Nsub))
      IF ( declvar('subbasin', 'sub_interflow', 'nsub', Nsub, 'real',
     +     'Sum of interflow to each subbasin from associated HRUs',
     +     'cfs',
     +     Sub_interflow).NE.0 ) RETURN
 
      ALLOCATE (Sub_gwflow(Nsub))
      IF ( declvar('subbasin', 'sub_gwflow', 'nsub', Nsub, 'real',
     +     'Sum of gwflow to each subbasin from associated HRUs',
     +     'cfs',
     +     Sub_gwflow).NE.0 ) RETURN
 
      ALLOCATE (Sub_sroff(Nsub))
      IF ( declvar('subbasin', 'sub_sroff', 'nsub', Nsub, 'real',
     +     'Sum of sroff to each subbasin from associated HRUs',
     +     'cfs',
     +     Sub_sroff).NE.0 ) RETURN
 
      ALLOCATE (Subinc_snowcov(Nsub))
      IF ( declvar('subbasin', 'subinc_snowcov', 'nsub', Nsub, 'real',
     +     'Sum of subbasin snowcov_area from subbasin HRUs',
     +     'decimal fraction',
     +     Subinc_snowcov).NE.0 ) RETURN
 
      ALLOCATE (Subinc_interflow(Nsub))
      IF ( declvar('subbasin', 'subinc_interflow', 'nsub', Nsub, 'real',
     +     'Sum of subbasin interflow from subbasin HRUs',
     +     'cfs',
     +     Subinc_interflow).NE.0 ) RETURN

      ALLOCATE (Subinc_gwflow(Nsub))
      IF ( declvar('subbasin', 'subinc_gwflow', 'nsub', Nsub, 'real',
     +     'Sum of subbasin gwflow from subbasin HRUs',
     +     'cfs',
     +     Subinc_gwflow).NE.0 ) RETURN
 
      ALLOCATE (Subinc_sroff(Nsub))
      IF ( declvar('subbasin', 'subinc_sroff', 'nsub', Nsub, 'real',
     +     'Sum of subbasin sroff from subbasin HRUs',
     +     'cfs',
     +     Subinc_sroff).NE.0 ) RETURN
 
      ALLOCATE (Subinc_precip(Nsub))
      IF ( declvar('subbasin', 'subinc_precip', 'nsub', Nsub, 'real',
     +     'Sum of subbasin hru_ppt from subbasin HRUs',
     +     'inches',
     +     Subinc_precip).NE.0 ) RETURN
 
      ALLOCATE (Subinc_actet(Nsub))
      IF ( declvar('subbasin', 'subinc_actet', 'nsub', Nsub, 'real',
     +     'Sum of subbasin hru_actet from subbasin HRUs',
     +     'inches',
     +     Subinc_actet).NE.0 ) RETURN

      ALLOCATE (Subinc_wb(Nsub))
      IF ( declvar('subbasin', 'subinc_wb', 'nsub', Nsub, 'real',
     +     'Water balance for each subbasin',
     +     'cfs',
     +     Subinc_wb).NE.0 ) RETURN

      ALLOCATE (Subinc_deltastor(Nsub))
      IF ( declvar('subbasin', 'subinc_deltastor', 'nsub', Nsub, 'real',
     +     'Change in storage for each subbasin',
     +     'cfs',
     +     Subinc_deltastor).NE.0 ) RETURN

      ALLOCATE (Subinc_snowmelt(Nsub))
      IF ( declvar('subbasin', 'subinc_snowmelt', 'nsub', Nsub, 'real',
     +     'Sum of subbasin snowmelt from subbasin HRUs',
     +     'inches',
     +     Subinc_snowmelt).NE.0 ) RETURN
 
      ALLOCATE (Subinc_pkweqv(Nsub))
      IF ( declvar('subbasin', 'subinc_pkweqv', 'nsub', Nsub, 'real',
     +     'Sum of subbasin pkwater_equiv from subbasin HRUs',
     +     'inches',
     +     Subinc_pkweqv).NE.0 ) RETURN
 
      ALLOCATE (Qsub(Nsub), Tree(Nsub, Nsub))
      ALLOCATE (Sub_inq(Nsub))
      IF ( declvar('subbasin', 'sub_inq', 'nsub', Nsub, 'real',
     +     'Sum of inflows to each subbasin from associated HRUs',
     +     'cfs',
     +     Sub_inq).NE.0 ) RETURN

      ALLOCATE (Sub_cfs(Nsub))
      IF ( declvar('subbasin', 'sub_cfs', 'nsub', Nsub, 'real',
     +     'Total outflow from each subbasin',
     +     'cfs',
     +     Sub_cfs).NE.0 ) RETURN

      ALLOCATE (Sub_cms(Nsub))
      IF ( declvar('subbasin', 'sub_cms', 'nsub', Nsub, 'real',
     +     'Total outflow from each subbasin',
     +     'cms',
     +     Sub_cms).NE.0 ) RETURN

      ALLOCATE (Subbasin_down(Nsub))
      IF ( declparam('subbasin', 'subbasin_down', 'nsub', 'integer',
     +     '0', 'bounded', 'nsub',
     +     'Downstream subbasin for each subbasin',
     +     'Index number for the downstream subbasin whose'//
     +     ' inflow is outflow from this subbasin.',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_subbasin(Nhru))
      IF ( declparam('subbasin', 'hru_subbasin', 'nhru', 'integer',
     +     '0', 'bounded', 'nsub',
     +     'Index of subbasin assigned to each HRU',
     +     'Index of subbasin assigned to each HRU',
     +     'none').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE ( Soil_moist(Nhru), Ssres_stor(Nhru) )
      ALLOCATE ( Sub_area(Nsub), Subincstor(Nsub), Laststor(Nsub) )

      subdecl = 0
      END FUNCTION subdecl

!***********************************************************************
!     subinit - Initialize subbasin module - get parameter values,
!                   compute initial values
!***********************************************************************
      INTEGER FUNCTION subinit()
      USE PRMS_SUBBASIN
      USE PRMS_MODULE, ONLY: Model, Nsub
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order,
     +    Print_debug, Hru_type, Hru_percent_perv, Nhru, Timestep,
     +    NEARZERO
      USE PRMS_FLOWVARS, ONLY: Hru_impervstor
      USE PRMS_OBS, ONLY: Cfs_conv
      USE PRMS_SNOW, ONLY: Pkwater_equiv
      USE PRMS_INTCP, ONLY: Hru_intcpstor
      USE PRMS_GWFLOW_CASC, ONLY: Gwres_stor
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam, getvar
! Local Variables
      INTEGER :: i, j, k, kk
      REAL :: gwstor, soilstor, snowstor, landstor, harea
!***********************************************************************
      subinit = 1

      IF ( Timestep==0 ) THEN
        Sub_cfs = 0.0
        Sub_cms = 0.0
        Sub_inq = 0.0
        Subinc_interflow = 0.0
        Subinc_gwflow = 0.0
        Subinc_sroff = 0.0
        Subinc_precip = 0.0
        Subinc_snowmelt = 0.0
        Subinc_pkweqv = 0.0
        Subinc_actet = 0.0
        Subinc_snowcov = 0.0
      ENDIF

      IF ( getparam('subbasin', 'hru_subbasin', Nhru, 'integer',
     +     Hru_subbasin).NE.0 ) RETURN

      IF ( getparam('subbasin', 'subbasin_down', Nsub, 'integer',
     +     Subbasin_down).NE.0 ) RETURN

! Determine the tree structure for the internal nodes
      Tree = 0
      DO j = 1, Nsub
        kk = Subbasin_down(j)
        IF ( kk.NE.0 ) Tree(kk, j) = 1
      ENDDO

      IF ( Print_debug==14 ) THEN
        OPEN (TREEUNIT, FILE='tree_structure', STATUS='replace')
        WRITE (TREEUNIT, 9001) (j, j=1, Nsub)
        DO j = 1, Nsub
          WRITE (TREEUNIT, 9002) j, (Tree(j,k), k=1, Nsub)
        ENDDO
      ENDIF

      DO j = 1, Nsub
        DO k = 1, Nsub
          IF ( Tree(j,k).GT.0 ) THEN
            DO i = 1, Nsub
              Tree(j, i) = Tree(j, i) + Tree(k, i)
              IF ( Tree(j,i).GT.1 ) THEN
                Tree(j, i) = 1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      DO j = Nsub, 1, -1
        DO k = 1, Nsub
          IF ( Tree(j,k).GT.0 ) THEN
            DO i = 1, Nsub
              Tree(j, i) = Tree(j, i) + Tree(k, i)
              IF ( Tree(j,i).GT.1 ) Tree(j,i)=1
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      IF ( Print_debug==14 ) THEN
        WRITE (TREEUNIT, 9003) (j, j=1, Nsub)
        DO j = 1, Nsub
          WRITE (TREEUNIT,9002) j, (Tree(j,k), k=1, Nsub)
        ENDDO
        CLOSE (TREEUNIT)
      ENDIF

      IF ( getvar('smbal', 'soil_moist', Nhru, 'real',
     +     Soil_moist).NE.0 ) RETURN

      IF ( getvar('ssflow', 'ssres_stor', Nhru, 'real', Ssres_stor)
     +     .NE.0 ) RETURN

      Qsub = 0.0
      Subincstor = 0.0
      Sub_area = 0.0
      gwstor = 0.0
      DO i = 1, Active_hrus
        j = Hru_route_order(i)
        ! k indicates which HRU is in which subbasin
        k = Hru_subbasin(j)
        IF ( k>0 ) THEN
          harea = Hru_area(j)
          IF ( Hru_type(j)/=2 ) THEN
            IF ( Model/=0 ) gwstor = Gwres_stor(j)*harea
            soilstor = (Soil_moist(j)*Hru_percent_perv(j)
     +                 + Ssres_stor(j))*harea
            snowstor = Pkwater_equiv(j)*harea
            landstor = (Hru_intcpstor(j)+Hru_impervstor(j))*harea
          ELSE
            gwstor = 0.0
            soilstor = 0.0
            snowstor = 0.0
            landstor = 0.0
          ENDIF
          Subincstor(k) = Subincstor(k) + soilstor + gwstor
     +                    + snowstor + landstor
          Sub_area(k) = Sub_area(k) + harea
        ENDIF
      ENDDO
      DO i = 1, Nsub
        IF ( Sub_area(i)<NEARZERO ) THEN
          PRINT *, 'Warning, subbasin:', i, ' does not include any HRUs'
          Sub_area(i) = 1.0
        ENDIF
      ENDDO

      !convert first as subbasins don't have to be in order
      Subincstor = Subincstor*Cfs_conv
      Subinc_wb = 0.0
      Subinc_deltastor = 0.0

      subinit = 0

 9001 FORMAT ('Initial Tree Structure for Internal Subbasins', /
     +        ' 1 indicates a node that flows directly into subbasin', /
     +        , 5X, 500I3)
 9002 FORMAT (I3, 2X, 500I3)
 9003 FORMAT (/, 'Tree Structure for Internal Subbasins', /
     +        ' 1 indicates a node that eventually flows into subbasin',
     +        /, 5X, 500I3)

      END FUNCTION subinit

!***********************************************************************
!     subrun - Computes basin streamflow and on-channel reservoir
!                  storage and outflows
!***********************************************************************
      INTEGER FUNCTION subrun()
      USE PRMS_SUBBASIN
      USE PRMS_MODULE, ONLY: Model, Nsub, Ncascade
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order,
     +    Print_debug, Hru_type, CFS2CMS_CONV, Hru_percent_perv, Nhru,
     +    Ngw
      USE PRMS_SNOW, ONLY: Snowcov_area, Snowmelt, Pkwater_equiv
      USE PRMS_OBS, ONLY: Runoff, Cfs_conv
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Hru_actet, Ssres_flow, Sroff,
     +    Hru_impervstor, Hortonian_lakes
      USE PRMS_INTCP, ONLY: Hru_intcpstor
      USE PRMS_SOILZONE, ONLY: Lakein_sz
      USE PRMS_GWFLOW_CASC, ONLY: Gwres_flow, Gwres_stor
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getvar
! Local Variables
      INTEGER :: j, jj, k
      REAL :: srq, ssq, gwq, harea, dmy, dmy1
      REAL :: soilstor, gwstor, snowstor, landstor
!     REAL :: conv
!***********************************************************************
      subrun = 1

!   Daily time step.
!   Compute reservoir routing and basin outflow

      !rsr, not getting groundwater storage and flow for GSFLOW mode

      IF ( getvar('smbal', 'soil_moist', Nhru, 'real',
     +     Soil_moist).NE.0 ) RETURN

      IF ( getvar('ssflow', 'ssres_stor', Nhru, 'real', Ssres_stor)
     +     .NE.0 ) RETURN

      DO j = 1, Nsub
        Sub_cfs(j) = 0.0
        Qsub(j) = 0.0
        Sub_inq(j) = 0.0
        Subinc_interflow(j) = 0.0
        Subinc_gwflow(j) = 0.0
        Subinc_sroff(j) = 0.0
        Subinc_precip(j) = 0.0
        Subinc_snowmelt(j) = 0.0
        Subinc_pkweqv(j) = 0.0
        Subinc_actet(j) = 0.0
        Subinc_snowcov(j) = 0.0
        Subinc_wb(j) = 0.0
      ENDDO

      Laststor = Subincstor
      Subincstor = 0.0

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        ! k indicates which HRU is in which subbasin
        k = Hru_subbasin(j)
        IF ( k.NE.0 ) THEN
          harea = Hru_area(j)
          gwstor = 0.0
          soilstor = 0.0
          gwq = 0.0
          snowstor = 0.0
          landstor = 0.0
          IF ( Hru_type(j)/=2 ) THEN
            srq = Sroff(j)*harea
            ssq = Ssres_flow(j)*harea
            IF ( Model/=0 ) THEN
              gwq = Gwres_flow(j)*harea
              gwstor = Gwres_stor(j)*harea
            ENDIF
            soilstor = (Soil_moist(j)*Hru_percent_perv(j)
     +                 + Ssres_stor(j))*harea
            snowstor = Pkwater_equiv(j)*harea
            landstor = (Hru_intcpstor(j)+Hru_impervstor(j))*harea
          ELSEIF ( Ncascade>0 ) THEN
            srq = Hortonian_lakes(j)*harea
            ssq = Lakein_sz(j)*harea
          ELSE
            srq = 0.0
            ssq = 0.0
          ENDIF
          Qsub(k) = Qsub(k) + srq + ssq + gwq
          Subinc_interflow(k) = Subinc_interflow(k) + ssq
          Subinc_gwflow(k) = Subinc_gwflow(k) + gwq
          Subinc_sroff(k) = Subinc_sroff(k) + srq
          Subinc_precip(k) = Subinc_precip(k) + Hru_ppt(j)*harea
          Subinc_actet(k) = Subinc_actet(k) + Hru_actet(j)*harea
          Subinc_snowmelt(k) = Subinc_snowmelt(k) + Snowmelt(j)*harea
          Subinc_pkweqv(k) = Subinc_pkweqv(k) + Pkwater_equiv(j)*harea
          Subinc_snowcov(k) = Subinc_snowcov(k) + Snowcov_area(j)*harea
          Subincstor(k) = Subincstor(k) + soilstor + gwstor
     +                    + snowstor + landstor
        ENDIF
      ENDDO

      !convert first as subbasins don't have to be in order
      DO j = 1, Nsub
        IF ( Model/=0 ) Sub_inq(j) = Qsub(j)*Cfs_conv
        Subinc_interflow(j) = Subinc_interflow(j)*Cfs_conv
        Subinc_gwflow(j) = Subinc_gwflow(j)*Cfs_conv
        Subinc_sroff(j) = Subinc_sroff(j)*Cfs_conv
        dmy = Subinc_precip(j)*Cfs_conv
        Subinc_precip(j) = Subinc_precip(j)/Sub_area(j)
        dmy1 = Subinc_actet(j)*Cfs_conv
        Subinc_actet(j) = Subinc_actet(j)/Sub_area(j)
        Subinc_snowmelt(j) = Subinc_snowmelt(j)/Sub_area(j)
        Subinc_pkweqv(j) = Subinc_pkweqv(j)/Sub_area(j)
        Subinc_snowcov(j) = Subinc_snowcov(j)/Sub_area(j)
        Subincstor(j) = Subincstor(j)*Cfs_conv
        Subinc_deltastor(j) = Laststor(j) - Subincstor(j)
        Subinc_wb(j) = dmy - dmy1 - Subinc_sroff(j)
     +                 - Subinc_gwflow(j) - Subinc_interflow(j)
     +                 + Subinc_deltastor(j)
      ENDDO

      !get cummulative subbasin flows
      DO j = 1, Nsub
        Sub_gwflow(j) = Subinc_gwflow(j)
        Sub_sroff(j) = Subinc_sroff(j)
        Sub_interflow(j) = Subinc_interflow(j)
        DO k = 1, Nsub
          IF ( Tree(j,k).NE.0 ) THEN
            Sub_gwflow(j) = Sub_gwflow(j) + Subinc_gwflow(k)
            Sub_sroff(j) = Sub_sroff(j) + Subinc_sroff(k)
            Sub_interflow(j) = Sub_interflow(j) + Subinc_interflow(k)
          ENDIF
        ENDDO
      ENDDO

!     DO k = 1, Nsub
!       conv = Sub_area(k)*Cfs_conv
!     ENDDO

      IF ( Model/=0 ) THEN
        DO j = 1, Nsub
          Sub_cfs(j) = Sub_inq(j)
          DO k = 1, Nsub
            IF ( Tree(j,k).NE.0 )
     +           Sub_cfs(j) = Sub_cfs(j) + Sub_inq(k)
          ENDDO
          Sub_cfs(j) = Sub_cfs(j) - Subinc_wb(j)
          Sub_cms(j) = Sub_cfs(j)*CFS2CMS_CONV
        ENDDO
      ENDIF

      subrun = 0

      END FUNCTION subrun

!***********************************************************************
! Computes basin streamflow & on-channel reservoir storage and outflows
!***********************************************************************

      MODULE PRMS_STRMFLOW
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: CFS2CMS_CONV = 0.028316844
      INTEGER :: Nhru, Nsfres, Nsf1, Nsf, Num, Nssr, Ngw
      REAL, ALLOCATABLE :: C24(:, :), S24(:, :), Wvd(:, :), Qrai(:)
!   Declared Variables
      REAL :: Basin_stflow, Basin_sroff_cfs, Basin_ssflow_cfs
      REAL :: Basin_gwflow_cfs, Basin_cfs, Basin_cms
      REAL, ALLOCATABLE :: Sfres_sto(:), Sfres_inq(:), Din1(:)
      REAL, ALLOCATABLE :: Sfres_outq(:), Sfres_area(:), Sfres_outcms(:)
!   Declared Variables from other modules - srunoff
!Warning modifies another modules variables: Basin_sroff
      REAL :: Basin_sroff
      REAL, ALLOCATABLE :: Sroff(:)
!   Declared Variables from other modules - gwflow
      REAL Basin_gwflow
      REAL, ALLOCATABLE :: Gwres_flow(:)
!   Declared Variables from other modules - ssflow
      REAL :: Basin_ssflow
      REAL, ALLOCATABLE :: Ssres_flow(:)
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Sfres_type(:), Hru_sfres(:), Nsos(:)
      INTEGER, ALLOCATABLE :: Upst_res1(:), Upst_res2(:), Upst_res3(:)
      INTEGER, ALLOCATABLE :: Hru_ssres(:), Hru_gwres(:)
      REAL :: Basin_cfs_init
      REAL, ALLOCATABLE :: Sfres_qro(:), Sfres_din1(:), Sfres_init(:)
      REAL, ALLOCATABLE :: Sfres_coef(:), Hru_area(:)
      REAL, ALLOCATABLE :: O2(:,:), S2(:,:)
      END MODULE PRMS_STRMFLOW

!***********************************************************************
!     Main daily and storm stream flow routine
!***********************************************************************
      INTEGER FUNCTION strmflow_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: strmdecl, strminit, strmrun
!***********************************************************************
      strmflow_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        strmflow_prms = strmrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        strmflow_prms = strmdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        strmflow_prms = strminit()
      ENDIF

      END FUNCTION strmflow_prms

!***********************************************************************
!     strmdecl - set up parameters for streamflow and surface reservoir
!                flow computations
!   Declared Parameters
!     sfres_type, hru_sfres, sfres_init, sfres_qro, sfres_din1
!     sfres_coef, o2, s2, nsos, upst_res1, upst_res2, upst_res3
!     hru_area, hru_ssres, hru_gwres, basin_cfs_init
!***********************************************************************
      INTEGER FUNCTION strmdecl()
      USE PRMS_STRMFLOW
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTEGER mxnsos, n
!***********************************************************************
      strmdecl = 1

      IF ( declmodule(
     +'$Id: strmflow_prms.f 3604 2007-11-01 22:10:19Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nssr = getdim('nssr')
      IF ( Nssr.EQ.-1 ) RETURN

      Ngw = getdim('ngw')
      IF ( Ngw.EQ.-1 ) RETURN

      Nsfres = getdim('nsfres')
      IF ( Nsfres.EQ.-1 ) RETURN
      Nsf = 1
      IF ( Nsfres.GT.1 ) Nsf = Nsfres
      Nsf1 = Nsf + 1

! Declared Variables
      IF ( declvar('strmflow', 'basin_stflow', 'one', 1, 'real',
     +     'Sum of basin_sroff, basin_ssflow and basin_gwflow for'//
     +     ' timestep',
     +     'inches',
     +     Basin_stflow).NE.0 ) RETURN

      IF ( declvar('strmflow', 'basin_cfs', 'one', 1, 'real',
     +     'Streamflow from basin',
     +     'cfs',
     +     Basin_cfs).NE.0 ) RETURN

      IF ( declvar('strmflow', 'basin_cms', 'one', 1, 'real',
     +     'Streamflow from basin',
     +     'cms',
     +     Basin_cms).NE.0 ) RETURN

      IF ( declvar('strmflow', 'basin_sroff_cfs', 'one', 1, 'real',
     +     'Basin surface runoff for timestep ',
     +     'cfs',
     +     Basin_sroff_cfs).NE.0 ) RETURN

      IF ( declvar('strmflow', 'basin_ssflow_cfs', 'one', 1, 'real',
     +     'Basin subsurface flow for timestep',
     +     'cfs',
     +     Basin_ssflow_cfs).NE.0 ) RETURN

      IF ( declvar('strmflow', 'basin_gwflow_cfs', 'one', 1, 'real',
     +     'Basin ground-water flow for timestep',
     +     'cfs',
     +     Basin_gwflow_cfs).NE.0 ) RETURN

      IF ( Nsfres.GT.0 ) THEN
        ALLOCATE (Qrai(Nsf1))
        mxnsos = getdim('mxnsos')
        IF ( mxnsos.EQ.-1 ) RETURN
        n = 1
        IF ( mxnsos.GT.1 ) n = mxnsos
        Num = n*Nsf
        IF ( n.GT.MAXNSOS ) THEN
          PRINT *, 'Error, MAXNSOS < mxnsos', MAXNSOS, mxnsos
          RETURN
        ENDIF
        IF ( Nsf.GT.MAXSFR ) THEN
          PRINT *, 'Error, MAXSFR < nsf', MAXSFR, Nsf
          RETURN
        ENDIF
        ALLOCATE (Wvd(n, Nsf), S24(n, Nsf), C24(n, Nsf))
        IF ( declpri('strmflow_wvd', Num, 'real', Wvd).NE.0 ) RETURN
        IF ( declpri('strmflow_s24', Num, 'real', S24).NE.0 ) RETURN
        IF ( declpri('strmflow_c24', Num, 'real', C24).NE.0 ) RETURN

        ALLOCATE (Sfres_sto(Nsf))
        IF ( declvar('strmflow', 'sfres_sto', 'nsfres', Nsf, 'real',
     +       'Storage in each surface reservoir',
     +       'cfs-days',
     +       Sfres_sto).NE.0 ) RETURN

        ALLOCATE (Sfres_inq(Nsf1))
        IF ( declvar('strmflow', 'sfres_inq', 'nsfres', Nsf1, 'real',
     +       'Sum of inflows to surface reservoir from all'//
     +       ' associated HRUs',
     +       'cfs',
     +       Sfres_inq).NE.0 ) RETURN

        ALLOCATE (Sfres_outq(Nsf1))
        IF ( declvar('strmflow', 'sfres_outq', 'nsfres', Nsf1, 'real',
     +       'Outflow from each surface reservoir',
     +       'cfs',
     +       Sfres_outq).NE.0 ) RETURN

        ALLOCATE (Sfres_outcms(Nsf1))
        IF ( declvar('strmflow', 'sfres_outcms', 'nsfres', Nsf1, 'real',
     +       'Outflow from each surface reservoir',
     +       'cms',
     +       Sfres_outcms).NE.0) RETURN

        ALLOCATE (Sfres_area(Nsf1))
        IF ( declvar('strmflow', 'sfres_area', 'nsfres', Nsf1, 'real',
     +       'Sum of HRU areas contributing to this surface reservoir',
     +       'acres',
     +       Sfres_area).NE.0 ) RETURN

        ALLOCATE (Din1(Nsf))
        IF ( declvar('strmflow', 'din1', 'nsfres', Nsf, 'real',
     +       'Storage reservoir inflow from the previous time step.',
     +       'cfs',
     +       Din1).NE.0 ) RETURN

! declare parameters
        ALLOCATE (Sfres_type(Nsf))
        IF ( declparam('strmflow', 'sfres_type', 'nsfres', 'integer',
     +       '8', '8', '9',
     +       'Type of surface reservoir',
     +       'Type of surface reservoir (8=Puls routing;'//
     +       ' 9=Linear routing)',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Hru_sfres(Nhru))
        IF ( declparam('strmflow', 'hru_sfres', 'nhru', 'integer',
     +       '0', 'bounded', 'nsfres',
     +       'Index of surface reservoir assigned to HRU',
     +       'Index of surface reservoir receiving excess water'//
     +       ' from HRU. If HRU does not feed a reservoir, then = 0',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Sfres_init(Nsf))
        IF ( declparam('strmflow', 'sfres_init', 'nsfres', 'real',
     +       '0.', '0.', '2000000.',
     +       'Initial storage in each surface reservoir',
     +       'Initial storage in each surface reservoir',
     +       'cfs-days').NE.0 ) RETURN

        ALLOCATE (Sfres_qro(Nsf))
        IF ( declparam('strmflow', 'sfres_qro', 'nsfres', 'real',
     +       '0.1', '0.0', '1.0',
     +       'Initial daily mean outflow from each storage reservoir',
     +       'Initial daily mean outflow from each storage reservoir',
     +       'cfs').NE.0 ) RETURN

        ALLOCATE (Sfres_din1(Nsf))
        IF ( declparam('strmflow', 'sfres_din1', 'nsfres', 'real',
     +       '0.1', '0.0', '1.0',
     +       'Surface reservoir inflow from the previous time step',
     +       'Surface reservoir inflow from the previous time step',
     +       'cfs').NE.0 ) RETURN

        ALLOCATE (Sfres_coef(Nsf))
        IF ( declparam('strmflow', 'sfres_coef', 'nsfres', 'real',
     +       '0.1', '0.0', '1.0',
     +       'Linear reservoir routing coefficient',
     +       'Coefficient to route reservoir storage to streamflow'//
     +       ' using the equation: res_flow = sfres_coef * res_stor',
     +       '1/day').NE.0 ) RETURN

        ALLOCATE (O2(n,Nsf))
        IF ( declparam('strmflow', 'o2', 'mxnsos,nsfres', 'real',
     +       '0.0', '0.0', '100000.',
     +      'Outflow values in outflow/storage table for Puls routing.',
     +      'Outflow values in outflow/storage table for Puls routing.',
     +       'cfs').NE.0 ) RETURN

        ALLOCATE (S2(n,Nsf))
        IF ( declparam('strmflow', 's2', 'mxnsos,nsfres', 'real',
     +       '0.0', '0.0', '100000.',
     +      'Storage values in outflow/storage table for Puls routing.',
     +      'Storage values in outflow/storage table for Puls routing.',
     +       'cfs-days').NE.0 ) RETURN

        ALLOCATE (Nsos(Nsf))
        IF ( declparam('strmflow', 'nsos', 'nsfres', 'integer',
     +       '0', '0', '10',
     +    'Number of storage/outflow values in table for Puls routing.',
     +    'Number of storage/outflow values in table for Puls routing.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Upst_res1(Nsf))
        IF ( declparam('strmflow', 'upst_res1', 'nsfres', 'integer',
     +       '0', 'bounded', 'nsfres',
     +       'Upstream reservoir index #1',
     +       'Index number for the first upstream reservoir whose'//
     +       ' outflow is inflow to this reservoir.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Upst_res2(Nsf))
        IF ( declparam('strmflow', 'upst_res2', 'nsfres', 'integer',
     +       '0', 'bounded', 'nsfres',
     +       'Upstream reservoir index #2',
     +       'Index number for the second upstream reservoir whose'//
     +       ' outflow is inflow to this reservoir.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Upst_res3(Nsf))
        IF ( declparam('strmflow', 'upst_res3', 'nsfres', 'integer',
     +       '0', 'bounded', 'nsfres',
     +       'Upstream reservoir index #3',
     +       'Index number for the third upstream reservoir whose'//
     +       ' outflow is inflow to this reservoir',
     +       'none').NE.0 ) RETURN

      ENDIF

! Declared Parameters
      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('strmflow', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      IF ( declparam('strmflow', 'basin_cfs_init', 'one', 'real',
     +     '0.0', '0.0', '1e+09',
     +     'Intial basin streamflow',
     +     'Initial basin streamflow, required if the first timestep'//
     +     ' is a storm period',
     +     'cfs').NE.0 ) RETURN

      ALLOCATE (Hru_ssres(Nhru))
      IF ( Nhru.NE.Nssr ) THEN
        IF ( declparam('strmflow', 'hru_ssres', 'nhru', 'integer',
     +       '1', 'bounded', 'nssr',
     +       'Index of subsurface reservoir assigned to HRU',
     +       'Index of subsurface reservoir receiving excess water'//
     +       ' from HRU soil zone',
     +       'none').NE.0 ) RETURN
      ENDIF

      ALLOCATE (Hru_gwres(Nhru))
      IF ( Nhru.NE.Ngw ) THEN
        IF ( declparam('strmflow', 'hru_gwres', 'nhru', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'Index of groundwater reservoir assigned to HRU',
     +       'Index of groundwater reservoir receiving excess soil'//
     +       ' water from each HRU',
     +       'none').NE.0 ) RETURN
      ENDIF

! Allocate arrays for variables from other modules
      ALLOCATE (Ssres_flow(Nssr), Gwres_flow(Ngw), Sroff(Nhru))
      ALLOCATE (Hru_route_order(Nhru))

      strmdecl = 0
      END FUNCTION strmdecl

!***********************************************************************
!     strminit - Initialize strmflow module - get parameter values,
!                compute initial values
!***********************************************************************
      INTEGER FUNCTION strminit()
      USE PRMS_STRMFLOW
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: j, jj, k, kk, nstep
!***********************************************************************
      strminit = 1

      nstep = getstep()

      IF ( getparam('strmflow', 'basin_cfs_init', 1, 'real',
     +     Basin_cfs_init).NE.0 ) RETURN

      Basin_cfs = Basin_cfs_init
      Basin_cms = Basin_cfs_init*CFS2CMS_CONV

      IF ( getparam('strmflow', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( Nsfres.GT.0 ) THEN

        IF ( Nhru.NE.Nssr ) THEN
          IF ( getparam('strmflow', 'hru_ssres', Nhru, 'integer',
     +         Hru_ssres).NE.0 ) RETURN
        ELSE
          DO j = 1, Nhru
            Hru_ssres(j) = j
          ENDDO
        ENDIF

        IF ( Nhru.NE.Ngw ) THEN
          IF ( getparam('strmflow', 'hru_gwres', Nhru, 'integer',
     +         Hru_gwres).NE.0 ) RETURN
        ELSE
          DO j = 1, Ngw
            Hru_gwres(j) = j
          ENDDO
        ENDIF

        IF ( getparam('strmflow', 'hru_sfres', Nhru, 'integer',
     +       Hru_sfres).NE.0 ) RETURN

        IF ( getparam('strmflow', 'sfres_type', Nsf, 'integer',
     +       Sfres_type).NE.0 ) RETURN

        IF ( getparam('strmflow', 'nsos', Nsf, 'integer', Nsos)
     +       .NE.0 ) RETURN

        IF ( getparam('strmflow', 'upst_res1', Nsf, 'integer',
     +       Upst_res1).NE.0 ) RETURN

        IF ( getparam('strmflow', 'upst_res2', Nsf, 'integer',
     +       Upst_res2).NE.0 ) RETURN

        IF ( getparam('strmflow', 'upst_res3', Nsf, 'integer',
     +       Upst_res3).NE.0 ) RETURN

        IF ( getparam('strmflow', 'sfres_coef', Nsf, 'real',
     +       Sfres_coef).NE.0 ) RETURN

        IF ( getparam('strmflow', 'sfres_init', Nsf, 'real',
     +       Sfres_init).NE.0 ) RETURN

        IF ( getparam('strmflow', 'sfres_qro', Nsf, 'real',
     +       Sfres_qro).NE.0 ) RETURN

        IF ( getparam('strmflow', 'sfres_din1', Nsf, 'real',
     +       Sfres_din1).NE.0 ) RETURN

        IF ( getparam('strmflow', 'o2', Num, 'real', O2)
     +       .NE.0 ) RETURN

        IF ( getparam('strmflow', 's2', Num, 'real', S2)
     +       .NE.0 ) RETURN

        IF ( nstep.EQ.0 ) Sfres_inq = 0.0

        Sfres_area = 0.0
        DO j = 1, Nsfres
!rsr stoin, a local variable was removed, unused
!         Stoin(j) = (Sfres_init(j)*23.76)*Basin_area_inv
          Sfres_sto(j) = Sfres_init(j)
          Sfres_outq(j) = Sfres_qro(j)
          Sfres_outcms(j) = Sfres_qro(j)*CFS2CMS_CONV
          Din1(j) = Sfres_din1(j)
          IF ( Sfres_type(j).EQ.8 ) THEN
            kk = Nsos(j)
            DO k = 1, kk
              Wvd(k, j) = S2(k, j) + (O2(k, j)*.5)
            ENDDO
            DO k = 2, kk
              S24(k, j) = (O2(k, j)-O2(k-1, j))/(Wvd(k, j)-Wvd(k-1, j))
              C24(k, j) = O2(k, j) - (S24(k, j)*Wvd(k, j))
            ENDDO
          ENDIF
        ENDDO

        Sfres_area(Nsf1) = 0.
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_sfres(j)
          IF ( k.EQ.0 ) k = Nsf1
          Sfres_area(k) = Sfres_area(k) + Hru_area(j)
        ENDDO
      ENDIF

      IF ( nstep.EQ.0 ) THEN
        Basin_stflow = 0.0
        Basin_sroff_cfs = 0.0
        Basin_ssflow_cfs = 0.0
        Basin_gwflow_cfs = 0.0
      ENDIF

      strminit = 0
      END FUNCTION strminit

!***********************************************************************
!     strmrun - Computes basin streamflow and on-channel reservoir
!               storage and outflows
!***********************************************************************
      INTEGER FUNCTION strmrun()
      USE PRMS_STRMFLOW
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC SNGL, EXP
! Local Variables
      INTEGER :: j, jj, k, kr, kg, n
      REAL :: dts, cfs_conv, area_fac
      REAL :: srq, ssq, gwq, qr, din2, avin, s2o2, q2, xkt, c2
!***********************************************************************
      strmrun = 1

      IF ( getvar('srunoff', 'basin_sroff', 1, 'real',
     +     Basin_sroff).NE.0 ) RETURN

      IF ( getvar('gwflow', 'basin_gwflow', 1, 'real',
     +     Basin_gwflow).NE.0 ) RETURN

      IF ( getvar('ssflow', 'basin_ssflow', 1, 'real',
     +     Basin_ssflow).NE.0 ) RETURN

      dts = SNGL(deltim()*3600.D0)
      cfs_conv = 43560.0/12.0/dts
      area_fac = cfs_conv/Basin_area_inv

!   Daily time step.
      IF ( Nsfres.GT.0 ) THEN


!   Daily time step and basin has storage reservoirs.
!   Compute reservoir routing and basin outflow

        IF ( getvar('ssflow', 'ssres_flow', Nssr, 'real', Ssres_flow)
     +       .NE.0 ) RETURN

        IF ( getvar('gwflow', 'gwres_flow', Ngw, 'real', Gwres_flow)
     +       .NE.0 ) RETURN

        IF ( getvar('srunoff', 'sroff', Nhru, 'real', Sroff)
     +       .NE.0 ) RETURN

        Qrai = 0.
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_sfres(j)
          IF ( k.EQ.0 ) k = Nsf1
          kr = Hru_ssres(j)
          kg = Hru_gwres(j)
          srq = Sroff(j)*Hru_area(j)
          ssq = Ssres_flow(kr)*Hru_area(j)
          gwq = Gwres_flow(kg)*Hru_area(j)
          Qrai(k) = Qrai(k) + srq + ssq + gwq
        ENDDO

!rsr, original code used .04208754 instead of cfs_conv
        DO j = 1, Nsf1
          Sfres_inq(j) = Qrai(j)*cfs_conv
        ENDDO

        DO j = 1, Nsfres
          qr = 0.
          k = Upst_res1(j)
          IF ( k.NE.0 ) THEN
            qr = Sfres_outq(k)
            k = Upst_res2(j)
            IF ( k.NE.0 ) THEN
              qr = qr + Sfres_outq(k)
              k = Upst_res3(j)
              IF ( k.NE.0 ) qr = qr + Sfres_outq(k)
            ENDIF
          ENDIF

          din2 = qr + Sfres_inq(j)
          avin = (din2+Din1(j))*.5

!   Compute outflow using Puls routing method
          IF ( Sfres_type(j).EQ.8 ) THEN
            s2o2 = Sfres_sto(j) - (Sfres_outq(j)*.5)
            IF ( s2o2.LT.0. ) s2o2 = 0.
            s2o2 = s2o2 + avin
            n = Nsos(j)
            DO jj = 2, Nsos(j)
              IF ( s2o2.LT.Wvd(jj, j) ) THEN
                n = jj
                EXIT
              ENDIF
            ENDDO
            q2 = S24(n, j)*s2o2 + C24(n, j)

            IF ( q2.LT.0.0 ) q2 = 0.
            Sfres_sto(j) = s2o2 - (q2*.5)
            IF ( Sfres_sto(j).LT.0.0 ) THEN
              q2 = s2o2 + (Sfres_outq(j)*.5)
              Sfres_sto(j) = 0.
            ENDIF

!   Compute outflow using linear reservoir method
          ELSE
            xkt = Sfres_coef(j)
            c2 = 1. - EXP(-xkt)
            q2 = (avin*(1.-(c2/xkt))) + (Sfres_sto(j)*c2)
            IF ( q2.LT.0.0 ) q2 = 0.
            Sfres_sto(j) = Sfres_sto(j) + avin - q2
          ENDIF

          Sfres_outq(j) = q2
          Sfres_outcms(j) = q2*CFS2CMS_CONV
          Din1(j) = din2
        ENDDO

        Sfres_outq(Nsf1) = Sfres_outq(Nsfres) + Sfres_inq(Nsf1)
        Sfres_outcms(Nsf1) = Sfres_outq(Nsf1)*CFS2CMS_CONV

        Basin_cfs = Sfres_outq(Nsf1) ! Sfres_outq has to be basin outlet

        Basin_stflow = Basin_sroff + Basin_gwflow + Basin_ssflow

!   Daily time step and basin has NO storage reservoirs.
!   Compute daily flow.
      ELSE
        Basin_stflow = Basin_sroff + Basin_gwflow + Basin_ssflow
!rsr, original code used .04208754 instead of cfs_conv
!       should have been .04201389
        Basin_cfs = Basin_stflow*area_fac
      ENDIF

      Basin_cms = Basin_cfs*CFS2CMS_CONV

      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac

      strmrun = 0
      END FUNCTION strmrun

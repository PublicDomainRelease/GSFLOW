!***********************************************************************
! $Id: gsflow_mf2prms.f 3793 2008-02-04 21:55:25Z rsregan $
!***********************************************************************
      MODULE GSFMF2PRMS
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Nhrucell
!   Declared Variables
      REAL :: Basin_gw2sm
      REAL, ALLOCATABLE :: Gw2sm(:), Gw2sm_grav(:), Gw2sm_last_grav(:)
      REAL, ALLOCATABLE :: Actet_gw(:), Actet_tot_gwsz(:)
!   Declared Variables from other modules - soilzone
!     Warning module, modifies Basin_lakeevap, and Hru_actet
      REAL :: Basin_lakeevap
      REAL, ALLOCATABLE :: Hru_actet(:)
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Gvr_hru_id(:), Gvr_cell_id(:)
      INTEGER, ALLOCATABLE :: Lake_hru_id(:), Hru_type(:)
      REAL, ALLOCATABLE :: Gvr_hru_pct(:), Hru_area(:)
      END MODULE GSFMF2PRMS

!     ******************************************************************
!     Mapping module to convert MODFLOW to PRMS states for use by GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_mf2prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: mf2prmsdecl, mf2prmsinit, mf2prmsrun
!***********************************************************************
      gsflow_mf2prms = 0

      IF ( Arg.EQ.'run' ) THEN
        gsflow_mf2prms = mf2prmsrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        gsflow_mf2prms = mf2prmsdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        gsflow_mf2prms = mf2prmsinit()
      ENDIF

      END FUNCTION gsflow_mf2prms

!***********************************************************************
!     mf2prmsdecl - set up parameters
!   Declared Parameters
!     hru_area, gvr_hru_id, gvr_cell_id, gvr_hru_pct
!***********************************************************************
      INTEGER FUNCTION mf2prmsdecl()
      USE GSFMF2PRMS
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      mf2prmsdecl = 1

      IF ( declmodule(
     &'$Id: gsflow_mf2prms.f 3793 2008-02-04 21:55:25Z rsregan $')
     &     .NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell.EQ.-1 ) RETURN

! Declared Variables
      IF ( declvar('mf2prms', 'basin_gw2sm', 'one', 1, 'real',
     &     'Basin average water exfiltrated from UZF and added to SZ',
     &     'inches', Basin_gw2sm).NE.0) RETURN

      ALLOCATE (Gw2sm_grav(Nhrucell))
      IF ( declvar('mf2prms', 'gw2sm_grav', 'nhrucell', Nhrucell,
     &     'real', 'Ground-water discharge to gravity-flow reservoirs',
     &     'inches', Gw2sm_grav).NE.0 ) RETURN

      ALLOCATE (Gw2sm_last_grav(Nhrucell))
      IF ( declvar('mf2prms', 'gw2sm_last_grav', 'nhrucell', Nhrucell,
     &     'real', 'Ground-water discharge to gravity-flow reservoirs'//
     &     ' from last iteration',
     &     'inches', Gw2sm_last_grav).NE.0 ) RETURN

      ALLOCATE (Gw2sm(Nhru))
      IF ( declvar('mf2prms', 'gw2sm', 'nhru', Nhru, 'real',
     &     'HRU average water exfiltrated from groundwater model'//
     &     ' and added back to SM', 'inches',
     &     Gw2sm).NE.0 ) RETURN

      ALLOCATE (Actet_gw(Nhru))
      IF ( declvar('mf2prms', 'actet_gw', 'nhru', Nhru, 'real',
     &     'Actual ET from each GW cell', 'inches',
     &     Actet_gw).NE.0 ) RETURN

      ALLOCATE (Actet_tot_gwsz(Nhru))
      IF ( declvar('mf2prms', 'actet_tot_gwsz', 'nhru', Nhru, 'real',
     &     'Total actual ET from each GW cell and PRMS soil zone',
     &     'inches', Actet_tot_gwsz).NE.0 ) RETURN

! Declared Parameters
      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('mf2prms', 'hru_type', 'nhru', 'integer',
     &     '1', '0', '2',
     &     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     &     'none').NE.0 ) RETURN
 
      ALLOCATE (Lake_hru_id(Nhru))
      IF ( declparam('mf2prms', 'lake_hru_id', 'nhru', 'integer',
     &     '0', 'bounded', 'nhru',
     &     'MODFLOW lake associated with each HRU',
     &     'Index of the MODFLOW lake associated with each HRU',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_id(Nhrucell))
      IF ( declparam('mf2prms', 'gvr_hru_id', 'nhrucell', 'integer',
     &     '1', 'bounded', 'nhru',
     &     'Corresponding HRU id of each GVR',
     &     'Index of the HRU assocated with each gravity reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_cell_id(Nhrucell))
      IF ( declparam('mf2prms', 'gvr_cell_id', 'nhrucell', 'integer',
     &     '0', 'bounded', 'ngwcell',
     &     'Corresponding MODFLOW cell id of each GVR',
     &     'Index of the MODFLOW cell associated with each gravity'//
     &     ' reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_pct(Nhrucell))
      IF ( declparam('mf2prms', 'gvr_hru_pct', 'nhrucell', 'real',
     &     '0.0', '0.0', '1.0',
     &     'Proportion of the HRU associated with each GVR',
     &     'Proportion of the HRU area associated with each gravity'//
     &     ' reservoir',
     &     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('mf2prms', 'hru_area', 'nhru', 'real',
     &     '1.0', '0.01', '1e+09',
     &     'HRU area', 'Area of each HRU',
     &     'acres').NE.0 ) RETURN

! Allocate variables from other modules
      ALLOCATE (Hru_actet(Nhru), Hru_route_order(Nhru))

      mf2prmsdecl = 0

      END FUNCTION mf2prmsdecl

!***********************************************************************
!     mf2prmsinit - Initialize module - get parameter values
!***********************************************************************
      INTEGER FUNCTION mf2prmsinit()
      USE GSFMF2PRMS
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: nstep
!***********************************************************************
      mf2prmsinit = 1

      IF ( getparam('mf2prms', 'hru_area', Nhru, 'real', Hru_area)
     &     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     &     .NE.0 ) RETURN

      IF ( getparam('mf2prms', 'gvr_hru_id', Nhrucell, 'integer',
     &     Gvr_hru_id).NE.0 ) RETURN

      IF ( getparam('mf2prms', 'gvr_cell_id', Nhrucell, 'integer',
     &     Gvr_cell_id).NE.0 ) RETURN

      IF ( getparam('mf2prms', 'gvr_hru_pct', Nhrucell, 'real',
     &     Gvr_hru_pct).NE.0 ) RETURN

      IF ( getparam('mf2prms', 'lake_hru_id', Nhru, 'integer',
     &     Lake_hru_id).NE.0 ) RETURN

      IF ( getparam('mf2prms', 'hru_type', Nhru, 'integer', Hru_type)
     &     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     &     Hru_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     &     .NE.0 ) RETURN

      nstep = getstep()
      IF ( nstep.EQ.0 ) THEN
        Actet_gw = 0.0
        Actet_tot_gwsz = 0.0
        Gw2sm = 0.0
        Gw2sm_grav = 0.0
        Gw2sm_last_grav = 0.0
        Basin_gw2sm = 0.0
      ENDIF

      mf2prmsinit = 0

      END FUNCTION mf2prmsinit

!***********************************************************************
! mf2prmsrun: Maps MODFLOW results to PRMS HRU & gravity-flow cells
!***********************************************************************
      INTEGER FUNCTION mf2prmsrun()
      USE GSFMF2PRMS
      USE GSFCONVERT, ONLY:Mfq2inch_conv, Mfvol2inch_conv,
     &    Acre_inches_to_mfl3, Gwc_col, Gwc_row
      USE GWFUZFMODULE, ONLY:SEEPOUT, UZFETOUT
      USE GWFLAKMODULE, ONLY:EVAP
      IMPLICIT NONE
      INTRINSIC SNGL
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, irow, icol, icell, ihru, ii
      REAL :: harea
!***********************************************************************
      IF ( getvar('soilzone', 'hru_actet', Nhru, 'real', Hru_actet)
     &     .NE.0 ) RETURN

      Gw2sm_last_grav = Gw2sm_grav
      Gw2sm = 0.0
      Gw2sm_grav = 0.0
      Actet_gw = 0.0
      DO i = 1, Nhrucell
        ihru = Gvr_hru_id(i)
        IF ( Hru_type(ihru).NE.1 ) CYCLE
        icell = Gvr_cell_id(i)
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)
! SEEPOUT is a discharge L3/T
        IF ( SEEPOUT(icol, irow).GT.0.0 ) THEN
          Gw2sm_grav(i) = SEEPOUT(icol, irow)*Mfq2inch_conv(i)
          Gw2sm(ihru) = Gw2sm(ihru) + Gw2sm_grav(i)*Gvr_hru_pct(i)
        ENDIF
        Actet_gw(ihru) = Actet_gw(ihru) + !??? assumes cell all in HRU
     &                   UZFETOUT(icol, irow)*Mfvol2inch_conv(i)*
     &                   Gvr_hru_pct(i)
      ENDDO
      DO i = 1, Nhru
        IF ( Hru_type(i).EQ.1 )
     &       Actet_tot_gwsz(i) = Hru_actet(i) + Actet_gw(i)
      ENDDO

      Basin_gw2sm = 0.0
      Basin_lakeevap = 0.0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        harea = Hru_area(i)
        IF ( Hru_type(i).EQ.1 ) THEN
          Basin_gw2sm = Basin_gw2sm + Gw2sm(i)*harea
!-----------------------------------------------------------------------
! Get actual et from lakes
!-----------------------------------------------------------------------
        ELSEIF ( Hru_type(i).EQ.2 ) THEN
          Hru_actet(i) = SNGL(EVAP(Lake_hru_id(i)))/Acre_inches_to_mfl3/
     &                   harea
          Actet_tot_gwsz(i) = Hru_actet(i)
          Basin_lakeevap = Basin_lakeevap + Hru_actet(i)*harea
        ENDIF
      ENDDO
      Basin_gw2sm = Basin_gw2sm*Basin_area_inv
      Basin_lakeevap = Basin_lakeevap*Basin_area_inv

      IF ( putvar('soilzone', 'hru_actet', Nhru, 'real', Hru_actet)
     &     .NE.0 ) RETURN
      IF ( putvar('soilzone', 'basin_lakeevap', 1, 'real',
     &     Basin_lakeevap).NE.0 ) RETURN

      mf2prmsrun = 0

      END FUNCTION mf2prmsrun

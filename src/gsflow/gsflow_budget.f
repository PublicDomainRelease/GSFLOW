!***********************************************************************
!     Perform the MODFLOW budget procedure for PRMS soil zone
!***********************************************************************
      MODULE GSFBUDGET
!   Local Variables
      REAL, PARAMETER :: CFS2CMS = 0.028316844
      INTEGER :: Ngwcell, Nreach, Nhrucell, Nhru
      INTEGER :: Vbnm_index(13)
      REAL :: Gw_bnd_in, Gw_bnd_out, Well_in, Well_out, Basin_actetgw
      REAL, ALLOCATABLE :: Hru_area_sqft(:)
!rsr  REAL :: Basin_flux
!   Declared Variables
      REAL :: Total_pump, Total_pump_cfs, Sat_change_stor, Gw_inout
      REAL :: Stream_leakage, Sat_store, Unsat_store, Basin_szreject
      REAL :: Stream_inflow
      REAL, ALLOCATABLE :: Gwc_head(:), Reach_cfs(:), Reach_wse(:)
      REAL, ALLOCATABLE :: Gw_rejected(:)
!   Declared Variables from other modules - strmflow
!Warning, modifies Basin_gwflow_cfs, Basin_cfs, Basin_cms
      REAL :: Basin_gwflow_cfs, Basin_cfs, Basin_cms, Basin_stflow
!   Declared Variables from other modules - soilzone
!Warning, modifies Basin_soil_moist, Basin_ssstor, Basin_gvr2sm,
!     Soil_moist, Ssres_stor, Slow_stor, Gvr2sm, Gravity_stor_res,
!     Hru_actet
      REAL :: Basin_soil_moist, Basin_ssstor, Basin_gvr2sm
      REAL :: Basin_lakeevap
      REAL, ALLOCATABLE :: Soil_moist(:), Ssres_stor(:), Hru_actet(:)
      REAL, ALLOCATABLE :: Slow_stor(:), Pref_flow_stor(:), Gvr2sm(:)
      REAL, ALLOCATABLE :: Gravity_stor_res(:), Pref_flow_thrsh(:)
!   Declared Variables from other modules - mf2prms
!Warning, modifies Basin_gw2sm, Gw2sm, Gw2sm_grav, Actet_gw, and
!     Actet_tot_gwsz
      REAL :: Basin_gw2sm
      REAL, ALLOCATABLE :: Gw2sm(:), Gw2sm_grav(:), Actet_gw(:)
      REAL, ALLOCATABLE :: Gw2sm_last_grav(:)
      REAL, ALLOCATABLE :: Actet_tot_gwsz(:)
!   Declared Variables from other modules - prms2mf
!Warning, modifies Gw_rejected_grav
      REAL, ALLOCATABLE :: Gw_rejected_grav(:)
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
      REAL, ALLOCATABLE :: Hru_perv(:), Hru_percent_perv(:)
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Gvr_hru_id(:), Gvr_cell_id(:)
      INTEGER, ALLOCATABLE :: Lake_hru_id(:), Hru_type(:)
      REAL :: Basin_cfs_init
      REAL, ALLOCATABLE :: Hru_area(:), Gvr_hru_pct(:)
      END MODULE GSFBUDGET

!     ******************************************************************
!     Budget module to convert PRMS & MODFLOW states for use by GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_budget(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: gsfbuddecl, gsfbudinit, gsfbudrun
!***********************************************************************
      gsflow_budget = 0

      IF ( Arg.EQ.'run' ) THEN
        gsflow_budget = gsfbudrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        gsflow_budget = gsfbuddecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        gsflow_budget = gsfbudinit()
      ENDIF

      END FUNCTION gsflow_budget

!***********************************************************************
!     gsfbuddecl - set up parameters
!   Declared Parameters
!     hru_area, basin_cfs_init
!     gvr_hru_pct, gvr_hru_id, gvr_cell_id, hru_type, lake_hru_id
!***********************************************************************
      INTEGER FUNCTION gsfbuddecl()
      USE GSFBUDGET
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      gsfbuddecl = 1

      IF ( declmodule(
     &'$Id: gsflow_budget.f 3897 2008-02-25 20:47:16Z rsregan $')
     &     .NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nreach = getdim('nreach')
      IF ( Nreach.EQ.-1 ) RETURN

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell.EQ.-1 ) RETURN

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell.EQ.-1 ) RETURN

! Declared Variables
      IF ( declvar('gsfbud', 'gw_inout', 'one', 1, 'real',
     &     'Volumetric flow rate to saturated zone along external'//
     &     ' boundary (negative value is flow out of modeled region)',
     &     'L3',
     &     Gw_inout).NE.0 ) RETURN

      IF ( declvar('gsfbud', 'basin_szreject', 'one', 1, 'real',
     &     'Basin average recharge from SZ and rejected by UZF',
     &     'inches',
     &     Basin_szreject).NE.0) RETURN

      ALLOCATE (Gw_rejected(Nhru))
      IF ( declvar('gsfbud', 'gw_rejected', 'nhru', Nhru, 'real',
     &     'HRU average recharge rejected by UZF', 'inches',
     &     Gw_rejected).NE.0 ) RETURN

      IF ( declvar('gsfbud', 'stream_leakage', 'one', 1, 'real',
     &     'Volumetric flow rate of stream leakage to the unsaturated'//
     &     ' and saturated zones', 'L3',
     &     Stream_leakage).NE.0 ) RETURN

      IF ( declvar('gsfbud', 'stream_inflow', 'one', 1, 'real',
     &     'Specified volumetric stream inflow rate into model ',
     &     'L3',
     &     Stream_inflow).NE.0 ) RETURN

      IF ( declvar('gsfbud', 'unsat_store', 'one', 1, 'real',
     &     'Volume of water in the unsaturated zone', 'L3',
     &     Unsat_store).NE.0 ) RETURN
     
      IF ( declvar('gsfbud', 'sat_store', 'one', 1, 'real',
     &     'Volume of water in the saturated zone', 'L3',
     &     Sat_store).NE.0 ) RETURN
     
      IF ( declvar('gsfbud', 'sat_change_stor', 'one', 1, 'real',
     &     'Change in saturated-zone storage', 'L3',
     &     Sat_change_stor).NE.0 ) RETURN
     
      IF ( declvar('gsfbud', 'total_pump', 'one', 1, 'real',
     &     'Total pumpage from all cells in MODFLOW units', 'none ',
     &     Total_pump).NE.0 ) RETURN
     
      IF ( declvar('gsfbud', 'total_pump_cfs', 'one', 1, 'real',
     &     'Total pumpage from all cells', 'cfs ',
     &     Total_pump_cfs).NE.0 ) RETURN
     
      ALLOCATE (Gwc_head(Ngwcell))
      IF ( declvar('gsfbud', 'gwc_head', 'ngwcell', Ngwcell, 'real',
     &     'Head at each GW cell', 'length',
     &     Gwc_head).NE.0 ) RETURN

      ALLOCATE (Reach_cfs(Nreach))
      IF ( declvar('gsfbud', 'reach_cfs', 'nreach', Nreach, 'real',
     &     'Stream flow leaving each stream reach', 'cfs',
     &     Reach_cfs).NE.0 ) RETURN

      ALLOCATE (Reach_wse(Nreach))
      IF ( declvar('gsfbud', 'reach_wse', 'nreach', Nreach, 'real',
     &     'Water surface elevation in each stream reach', 'length',
     &     Reach_wse).NE.0 ) RETURN

! Declared Parameters
      IF ( declparam('gsfbud', 'basin_cfs_init', 'one', 'real',
     &     '0.0', '0.0', '1e+09',
     &     'Intial basin streamflow',
     &     'Initial basin streamflow, required if the first timestep'//
     &     ' is a storm period timestep',
     &     'cfs').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_id(Nhrucell))
      IF ( declparam('gsfbud', 'gvr_hru_id', 'nhrucell', 'integer',
     &     '1', 'bounded', 'nhru',
     &     'Corresponding HRU id of each GVR',
     &     'Index of the HRU assocated with each gravity reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_cell_id(Nhrucell))
      IF ( declparam('gsfbud', 'gvr_cell_id', 'nhrucell', 'integer',
     &     '0', 'bounded', 'ngwcell',
     &     'Corresponding MODFLOW cell id of each GVR',
     &     'Index of the MODFLOW cell associated with each gravity'//
     &     ' reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_pct(Nhrucell))
      IF ( declparam('gsfbud', 'gvr_hru_pct', 'nhrucell', 'real',
     &     '0.0', '0.0', '1.0',
     &     'Proportion of the HRU associated with each GVR',
     &     'Proportion of the HRU area associated with each gravity'//
     &     ' reservoir',
     &     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('gsfbud', 'hru_area', 'nhru', 'real',
     &     '1.0', '0.01', '1e+09',
     &     'HRU area', 'Area of each HRU',
     &     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('gsfbud', 'hru_type', 'nhru', 'integer',
     &     '1', '0', '2',
     &     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Lake_hru_id(Nhru))
      IF ( declparam('gsfbud', 'lake_hru_id', 'nhru', 'integer',
     &     '0', 'bounded', 'nhru',
     &     'MODFLOW lake associated with each HRU',
     &     'Index of the MODFLOW lake associated with each HRU',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Hru_area_sqft(Nhru), Slow_stor(Nhru))
      ALLOCATE (Soil_moist(Nhru), Ssres_stor(Nhru), Hru_actet(Nhru))
      ALLOCATE (Pref_flow_stor(Nhru), Pref_flow_thrsh(Nhru))
      ALLOCATE (Gvr2sm(Nhru), Gw2sm_grav(Nhrucell))
      ALLOCATE (Gw2sm_last_grav(Nhrucell), Hru_percent_perv(Nhru))
      ALLOCATE (Gw2sm(Nhru), Actet_gw(Nhru), Actet_tot_gwsz(Nhru))
      ALLOCATE (Gw_rejected_grav(Nhrucell), Gravity_stor_res(Nhrucell))
      ALLOCATE (Hru_route_order(Nhru), Hru_perv(Nhru))

      gsfbuddecl = 0

      END FUNCTION gsfbuddecl

!***********************************************************************
!     gsfbudinit - Initialize GSFBUDGET module - get parameter values
!***********************************************************************
      INTEGER FUNCTION gsfbudinit()
      USE GSFBUDGET
!fix Cfs2inches to compute each time step when time step intervals vary
      USE GSFCONVERT, ONLY: Cfs2inches
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: nstep
!***********************************************************************
      gsfbudinit = 1

      IF ( getparam('gsfbud', 'hru_area', Nhru, 'real', Hru_area)
     &     .NE.0 ) RETURN

      IF ( getparam('gsfbud', 'hru_type', Nhru, 'integer', Hru_type)
     &     .NE.0 ) RETURN

      IF ( getparam('gsfbud', 'lake_hru_id', Nhru, 'integer',
     &     Lake_hru_id).NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     &     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     &     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     &     Hru_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'hru_perv', Nhru, 'real', Hru_perv)
     &     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_percent_perv', Nhru, 'real',
     &     Hru_percent_perv).NE.0 ) RETURN

      IF ( getparam('gsfbud', 'gvr_hru_id', Nhrucell, 'integer',
     &     Gvr_hru_id).NE.0 ) RETURN

      IF ( getparam('gsfbud', 'gvr_cell_id', Nhrucell, 'integer',
     &     Gvr_cell_id).NE.0 ) RETURN

      IF ( getparam('gsfbud', 'gvr_hru_pct', Nhrucell, 'real',
     &     Gvr_hru_pct).NE.0 ) RETURN

      IF ( getparam('gsfbud', 'basin_cfs_init', 1, 'real',
     &     Basin_cfs_init).NE.0 ) RETURN

      Basin_cfs = Basin_cfs_init
      Basin_cms = Basin_cfs_init*CFS2CMS
      Basin_stflow = Basin_cfs*Cfs2inches
      IF ( putvar('strmflow', 'basin_cfs', 1, 'real', Basin_cfs)
     &     .NE.0 ) RETURN
      IF ( putvar('strmflow', 'basin_cms', 1, 'real', Basin_cms)
     &     .NE.0 ) RETURN
      IF ( putvar('strmflow', 'basin_stflow', 1, 'real', Basin_stflow)
     &     .NE.0 ) RETURN

      IF ( getvar('soilzone', 'pref_flow_thrsh', Nhru, 'real',
     &     Pref_flow_thrsh).NE.0 ) RETURN

      nstep = getstep()
      IF ( nstep.EQ.0 ) THEN
        Gwc_head = 0.0
        Reach_cfs = 0.0
        Reach_wse = 0.0
        Total_pump = 0.0
        Total_pump_cfs = 0.0
        Unsat_store = 0.0
        Sat_store = 0.0
        Sat_change_stor = 0.0
        Stream_leakage = 0.0
        Stream_inflow = 0.0
        Basin_szreject = 0.0
        Gw_rejected = 0.0
      ENDIF

!  Set the volume budget indicies to -1 anytime "init" is called.
!  This will make "run" figure out the vbnm order.
      Vbnm_index = -1

      gsfbudinit = 0

      END FUNCTION gsfbudinit

!***********************************************************************
! Compute basin budget for GSFLOW
! adjust gravity flow storage with last gw2sm and gw_rejected
!***********************************************************************
      INTEGER FUNCTION gsfbudrun()
      USE GSFBUDGET
      USE GSFCONVERT, ONLY:Acre_inches_to_mfl3, Mfq2inch_conv,
     &    Mfvol2inch_conv, Mfl3t_to_cfs, Mfl_to_inch, Gwc_col, Gwc_row
      USE GSFPRMS2MF, ONLY:Excess
      USE GLOBAL, ONLY:IUNIT
      USE GWFBASMODULE, ONLY:VBVL
      USE GWFUZFMODULE, ONLY:SEEPOUT, UZFETOUT, UZTSRAT, REJ_INF
      USE GWFLAKMODULE, ONLY:EVAP
      IMPLICIT NONE
      INTRINSIC SNGL
      INCLUDE 'fmodules.inc'
      EXTERNAL MODFLOW_GET_STORAGE_BCF, MODFLOW_GET_STORAGE_LPF
      EXTERNAL MODFLOW_VB_DECODE, getStreamFlow, getHeads, getPump
! Local Variables
      INTEGER :: iret, i, ihru, icell, irow, icol, ii
      REAL :: modflow_in, modflow_out, gwdisch, deficit, flux_change
      REAL :: pct, harea
!***********************************************************************
      gsfbudrun = 1

! adjust gravity flow storage using last gw discharge and rejected

      IF ( getvar('mf2prms', 'gw2sm_grav', Nhrucell, 'real', Gw2sm_grav)
     &     .NE.0 ) RETURN
      IF ( getvar('mf2prms', 'gw2sm_last_grav', Nhrucell, 'real',
     &     Gw2sm_last_grav).NE.0 ) RETURN
      IF ( getvar('prms2mf', 'gw_rejected_grav', Nhrucell, 'real',
     &     Gw_rejected_grav).NE.0 ) RETURN
      IF ( getvar('soilzone', 'soil_moist', Nhru, 'real', Soil_moist)
     &     .NE.0 ) RETURN
      IF ( getvar('soilzone', 'hru_actet', Nhru, 'real', Hru_actet)
     &     .NE.0 ) RETURN
      IF ( getvar('soilzone', 'pref_flow_stor', Nhru, 'real',
     &     Pref_flow_stor).NE.0 ) RETURN
      IF ( getvar('soilzone', 'gvr2sm', Nhru, 'real', Gvr2sm)
     &     .NE.0 ) RETURN
      IF ( getvar('soilzone', 'gravity_stor_res', Nhrucell, 'real',
     &     Gravity_stor_res).NE.0 ) RETURN

      Gw2sm = 0.0
      Gw_rejected = 0.0
!rsr  Basin_flux = 0.0
      Actet_gw = 0.0
      Slow_stor = 0.0
      Gw2sm_grav = 0.0
      DO i = 1, Nhrucell
        ihru = Gvr_hru_id(i)
        IF ( Hru_type(ihru).NE.1 ) CYCLE
        icell = Gvr_cell_id(i)
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)
        pct = Gvr_hru_pct(i)
!-----------------------------------------------------------------------
! Add any excess infiltration to Gw_rejected array
! rejected can be added now as water would not have been used for
! any other purpose, it was water that could have been sent to MODFLOW
! if MODFLOW could accept it, includes (1) binning (Excess) and rejected
! infiltration due to (2) inactive cells and too many waves
! (Gw_rejected_grav(i)), (3) exceeding K and high gw head (gw
! discharging into GVR) (REJ_INF).
!-----------------------------------------------------------------------
        Gw_rejected_grav(i) = Gw_rejected_grav(i)
     &                        + Excess(icell)*Mfl_to_inch
     &                        + REJ_INF(icol, irow)*Mfq2inch_conv(i)
 
        Gw_rejected(ihru) = Gw_rejected(ihru) + Gw_rejected_grav(i)*pct

        gwdisch = SEEPOUT(icol, irow)
        IF ( gwdisch.GT.0.0 ) THEN
          gwdisch = gwdisch*Mfq2inch_conv(i)
          Gw2sm_grav(i) = gwdisch
          Gw2sm(ihru) = Gw2sm(ihru) + gwdisch*pct
        ELSEIF ( gwdisch.LT.0.0 ) THEN
          print *, 'seepout problem', gwdisch
        ENDIF

! flux equals current minus last GW discharge plus rejected infiltration
        flux_change = gwdisch - Gw2sm_last_grav(i) + Gw_rejected_grav(i)
        Gravity_stor_res(i) = Gravity_stor_res(i) + flux_change
!rsr    Basin_flux = Basin_flux + (flux_change * pct * Hru_area(ihru))

        IF ( Gravity_stor_res(i).LT.0.0 ) THEN
          deficit = Gravity_stor_res(i)*pct
          Gvr2sm(ihru) = Gvr2sm(ihru) + deficit
          IF ( Hru_percent_perv(ihru).GT.0.0 ) Soil_moist(ihru) =
     &         Soil_moist(ihru) + deficit/Hru_percent_perv(ihru)
          IF ( Soil_moist(ihru).LT.0.0 ) THEN
! water probably ET'd out, set gravity_stor_res(i)=0, should be small
! this happens, value is small (<1.0e-6) so ignore
            IF ( deficit.LT.-1.0E-4 ) PRINT *,
     &           'negative GW flux > soil_moist', Soil_moist(ihru),
     &           Gravity_stor_res(i), deficit, ihru, i, flux_change,
     &           gwdisch, Gw2sm_last_grav(i), Gw_rejected_grav(i), icell
            Soil_moist(ihru) = 0.0
          ENDIF
          Gravity_stor_res(i) = 0.0
        ENDIF
        Slow_stor(ihru) = Slow_stor(ihru) + Gravity_stor_res(i)*pct

        !sanity check, see if overfilling storage, remove later
!       IF ( Gravity_stor_res(i).GT.Pref_flow_thrsh(ihru) ) PRINT *,
!    &       'overfilling GVR storage', ihru, i, Gravity_stor_res(i),
!    &       Pref_flow_thrsh(ihru), Gw2sm_grav(i), Gw_rejected_grav(i),
!    &       gwdisch, Gravity_stor_res(i) - Pref_flow_thrsh(ihru)
        Actet_gw(ihru) = Actet_gw(ihru) +
     &                   UZFETOUT(icol, irow)*Mfvol2inch_conv(i)*pct
      ENDDO
      Basin_actetgw = 0.0
      DO i = 1, Nhru
        IF ( Hru_type(i).EQ.1 )
     &       Actet_tot_gwsz(i) = Hru_actet(i) + Actet_gw(i)
        Basin_actetgw = Basin_actetgw + Actet_gw(i)*Hru_area(i)
      ENDDO
      Basin_actetgw = Basin_actetgw*Basin_area_inv


! adjust basin_soil_moist and Basin_ssstor as they may have been updated
      Basin_soil_moist = 0.0
      Basin_ssstor = 0.0
      Basin_gw2sm = 0.0
      Basin_gvr2sm = 0.0
      Basin_szreject = 0.0
      Basin_lakeevap = 0.0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        harea = Hru_area(i)
        IF ( Hru_type(i).EQ.1 ) THEN
          Basin_soil_moist = Basin_soil_moist +
     &                       Soil_moist(i)*Hru_perv(i)
          Basin_gw2sm = Basin_gw2sm + Gw2sm(i)*harea
!         IF ( Gvr2sm(i).LT.-1.0E-05 ) PRINT *, 'screw up', Gvr2sm(i), i
          IF ( Gvr2sm(i).LT.0.0 ) Gvr2sm(i) = 0.0
          Basin_gvr2sm = Basin_gvr2sm + Gvr2sm(i)*harea
          Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)
          Basin_ssstor = Basin_ssstor + Ssres_stor(i)*harea
          Basin_szreject = Basin_szreject + Gw_rejected(i)*harea
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
      
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_gw2sm = Basin_gw2sm*Basin_area_inv
      Basin_gvr2sm = Basin_gvr2sm*Basin_area_inv
      Basin_szreject = Basin_szreject*Basin_area_inv
      Basin_lakeevap = Basin_lakeevap*Basin_area_inv
!rsr  Basin_flux = Basin_flux*Acre_inches_to_mfl3

      IF ( putvar('mf2prms', 'basin_gw2sm', 1, 'real', Basin_gw2sm)
     &     .NE.0 ) RETURN
      IF ( putvar('mf2prms', 'gw2sm', Nhru, 'real', Gw2sm).NE.0 ) RETURN
      IF ( putvar('mf2prms', 'gw2sm_grav', Nhrucell, 'real', Gw2sm_grav)
     &     .NE.0 ) RETURN
      IF ( putvar('mf2prms', 'actet_gw', Nhru, 'real', Actet_gw)
     &     .NE.0 ) RETURN
      IF ( putvar('mf2prms', 'actet_tot_gwsz', Nhru, 'real',
     &     Actet_tot_gwsz).NE.0 ) RETURN

      IF ( putvar('prms2mf', 'gw_rejected_grav', Nhrucell, 'real',
     &     Gw_rejected_grav).NE.0 ) RETURN

      IF ( putvar('soilzone', 'basin_soil_moist', 1, 'real',
     &     Basin_soil_moist).NE.0 ) RETURN
      IF ( putvar('soilzone', 'soil_moist', Nhru, 'real', Soil_moist)
     &     .NE.0 ) RETURN
      IF ( putvar('ssflow', 'basin_ssstor', 1, 'real', Basin_ssstor)
     &     .NE.0 ) RETURN
      IF ( putvar('ssflow', 'ssres_stor', Nhru, 'real', Ssres_stor)
     &     .NE.0 ) RETURN
      IF ( putvar('soilzone', 'basin_gvr2sm', 1, 'real', Basin_gvr2sm)
     &     .NE.0 ) RETURN
      IF ( putvar('soilzone', 'gvr2sm', Nhru, 'real', Gvr2sm)
     &     .NE.0 ) RETURN
      IF ( putvar('soilzone', 'slow_stor', Nhru, 'real', Slow_stor)
     &     .NE.0 ) RETURN
      IF ( putvar('soilzone', 'gravity_stor_res', Nhrucell, 'real',
     &     Gravity_stor_res).NE.0 ) RETURN
      IF ( putvar('soilzone', 'hru_actet', Nhru, 'real', Hru_actet)
     &     .NE.0 ) RETURN
      IF ( putvar('soilzone', 'basin_lakeevap', 1, 'real',
     &     Basin_lakeevap).NE.0 ) RETURN

      IF ( IUNIT(1).GT.0 ) CALL MODFLOW_GET_STORAGE_BCF()
      IF ( IUNIT(23).GT.0 ) CALL MODFLOW_GET_STORAGE_LPF()

      IF ( Vbnm_index(1).EQ.-1 ) CALL MODFLOW_VB_DECODE(Vbnm_index)
      Sat_change_stor = VBVL(4,Vbnm_index(12)) - VBVL(3,Vbnm_index(12))

      Unsat_store = SNGL(UZTSRAT(6))

!  Stuff from MODFLOW

      modflow_in = 0.0
      Gw_bnd_in = 0.0
      Well_in = 0.0
      IF ( Vbnm_index(1).NE.-1 ) THEN ! constant heads
        modflow_in = modflow_in + VBVL(3, Vbnm_index(1))
        Gw_bnd_in = Gw_bnd_in + VBVL(3, Vbnm_index(1))
      ENDIF

      IF ( Vbnm_index(3).NE.-1 ) THEN ! head dep bounds
        modflow_in = modflow_in + VBVL(3, Vbnm_index(3))
        Gw_bnd_in = Gw_bnd_in + VBVL(3, Vbnm_index(3))
      ENDIF

      IF ( Vbnm_index(4).NE.-1 ) THEN ! specified heads
        modflow_in = modflow_in + VBVL(3, Vbnm_index(4))
        Gw_bnd_in = Gw_bnd_in + VBVL(3, Vbnm_index(4))
      ENDIF

      IF ( Vbnm_index(5).NE.-1 ) THEN ! wells
        modflow_in = modflow_in + VBVL(3, Vbnm_index(5))
        Well_in = Well_in + VBVL(3, Vbnm_index(5))
      ENDIF

      IF ( Vbnm_index(6).NE.-1 ) THEN ! multi node wells
        modflow_in = modflow_in + VBVL(3, Vbnm_index(6))
        Well_in = Well_in + VBVL(3, Vbnm_index(6))
      ENDIF

      modflow_out = 0.0
      Gw_bnd_out = 0.0
      Well_out = 0.0
      IF ( Vbnm_index(1).NE.-1 ) THEN ! constant heads
        modflow_out = modflow_out + VBVL(4, Vbnm_index(1))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(1))
      ENDIF

      IF ( Vbnm_index(2).NE.-1 ) THEN ! drains
        modflow_out = modflow_out + VBVL(4, Vbnm_index(2))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(2))
      ENDIF

      IF ( Vbnm_index(3).NE.-1 ) THEN ! head dep bounds
        modflow_out = modflow_out + VBVL(4, Vbnm_index(3))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(3))
      ENDIF

      IF ( Vbnm_index(4).NE.-1 ) THEN ! specified heads
        modflow_out = modflow_out + VBVL(4, Vbnm_index(4))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(4))
      ENDIF

      IF ( Vbnm_index(5).NE.-1 ) THEN ! wells
        modflow_out = modflow_out + VBVL(4, Vbnm_index(5))
        Well_out = Well_out + VBVL(4, Vbnm_index(5))
      ENDIF

      IF ( Vbnm_index(6).NE.-1 ) THEN ! multi node wells
        modflow_out = modflow_out + VBVL(4, Vbnm_index(6))
        Well_out = Well_out + VBVL(4, Vbnm_index(6))
      ENDIF

      Gw_inout = modflow_in - modflow_out

      CALL getStreamFlow(iret)
      IF ( iret.NE.0 ) RETURN

      Basin_gwflow_cfs = Stream_leakage*Mfl3t_to_cfs
      IF ( putvar('strmflow', 'basin_gwflow_cfs', 1, 'real',
     &     Basin_gwflow_cfs).NE.0 ) RETURN

      CALL getHeads()

      IF ( IUNIT(2).GT.0 ) CALL getPump()

      gsfbudrun = 0

      END FUNCTION gsfbudrun

!***********************************************************************
! Figure out the total storage of the cells in MODFLOW
! written by markstro but hijacked from MODFLOW subroutine SGWF1BCF6S
! Use Sat_store for display purposes only, don't use in budget.
!***********************************************************************
      SUBROUTINE MODFLOW_GET_STORAGE_BCF()
      USE GSFBUDGET, ONLY:Sat_store
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IBOUND, BOTM, HNEW, LBOTM
      USE GWFBASMODULE, ONLY:DELT
      USE GWFBCFMODULE, ONLY:LAYCON, SC1, SC2
      IMPLICIT NONE
      INTRINSIC SNGL
! Local Variables
      INTEGER :: i, j, k, kt, lc
      REAL :: tled, top, bot, rho, head, storage
!***********************************************************************
      tled = 1.0/DELT
      Sat_store = 0.0

!5------LOOP THROUGH EVERY CELL IN THE GRID.
      kt = 0
      DO k = 1, NLAY
        lc = LAYCON(k)
        IF ( lc.EQ.3 .OR. lc.EQ.2 ) kt = kt + 1
        DO i = 1, NROW
          DO j = 1, NCOL

!6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
            IF ( IBOUND(j, i, k).GT.0 ) THEN
              head = SNGL(HNEW(j, i, k))
              top = BOTM(j, i, LBOTM(k)-1)
              bot = BOTM(j, i, LBOTM(k))

!7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
              IF ( lc.EQ.3 .OR. lc.EQ.2 ) THEN
                rho = SC2(j, i, kt)
!7A----TWO STORAGE CAPACITIES.
!                IF ( head.GT.top ) THEN
!                  rho = SC1(j, i, k)
!                ELSE
!                  rho = SC2(j, i, kt)
!                ENDIF
              ELSE
!7B----ONE STORAGE CAPACITY.
                rho = SC1(j, i, k)
              ENDIF
              IF ( head.GE.top ) THEN
                storage = rho*(top-bot)*tled
              ELSE
                storage = rho*(head-bot)*tled
              ENDIF
              Sat_store = Sat_store + storage
            ENDIF

          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE MODFLOW_GET_STORAGE_BCF

!***********************************************************************
! Figure out the total storage of the cells in MODFLOW
! written by markstro but hijacked from MODFLOW subroutine SGWF1LPF1S
! Use Sat_store for display purposes only, don't use in budget.
!***********************************************************************
      SUBROUTINE MODFLOW_GET_STORAGE_LPF()
      USE GSFBUDGET, ONLY:Sat_store
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IBOUND, BOTM, HNEW, LBOTM
      USE GWFBASMODULE, ONLY:DELT
      USE GWFLPFMODULE, ONLY:LAYTYP, SC1, SC2
      IMPLICIT NONE
      INTRINSIC SNGL
! Local Variables
      INTEGER :: i, j, k, kt, lc
      REAL :: tled, top, bot, rho, storage, head
!***********************************************************************
      tled = 1.0/DELT
      Sat_store = 0.0

!5------LOOP THROUGH EVERY CELL IN THE GRID.
      kt = 0
      DO k = 1, NLAY
        lc = LAYTYP(k)
        IF ( lc.NE.0 ) kt = kt + 1
        DO i = 1, NROW
          DO j = 1, NCOL

!6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
            IF ( IBOUND(j, i, k).GT.0 ) THEN
              head = SNGL(HNEW(j, i, k))
              top = BOTM(j, i, LBOTM(k)-1)
              bot = BOTM(j, i, LBOTM(k))

!7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
              IF ( lc.NE.0 ) THEN
!7A----TWO STORAGE CAPACITIES.
!  markstro - always use specific yield
                rho = SC2(j, i, kt)
!               IF ( head.GT.top ) THEN
!                 rho = SC1(j, i, k)
!               ELSE
!                 rho = SC2(j, i, kt)
!               ENDIF
              ELSE
!7A----ONE STORAGE CAPACITY.
                rho = SC1(j, i, k)
              ENDIF
              IF ( head.GE.top ) THEN
                storage = rho*(top-bot)*tled
              ELSE
                storage = rho*(head-bot)*tled
              ENDIF
              Sat_store = Sat_store + storage
            ENDIF

          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE MODFLOW_GET_STORAGE_LPF
 
!***********************************************************************
! Decode the MODFLOW VBNM array
!***********************************************************************
      SUBROUTINE MODFLOW_VB_DECODE(Vbnm_index)
      USE GWFBASMODULE, ONLY:VBNM, MSUM
      IMPLICIT NONE
! Arguments
      INTEGER :: Vbnm_index(13)
! Local Variables
      INTEGER :: i
!***********************************************************************
!  Stuff from MODFLOW
      DO i = 1, MSUM - 1
        IF ( VBNM(i).EQ.'   CONSTANT HEAD' ) Vbnm_index(1) = i
        IF ( VBNM(i).EQ.'          DRAINS' ) Vbnm_index(2) = i
        IF ( VBNM(i).EQ.' HEAD DEP BOUNDS' ) Vbnm_index(3) = i
        IF ( VBNM(i).EQ.' SPECIFIED FLOWS' ) Vbnm_index(4) = i
        IF ( VBNM(i).EQ.'           WELLS' ) Vbnm_index(5) = i
        IF ( VBNM(i).EQ.'             MNW' ) Vbnm_index(6) = i
        IF ( VBNM(i).EQ.'    UZF RECHARGE' ) Vbnm_index(7) = i
        IF ( VBNM(i).EQ.'           GW ET' ) Vbnm_index(8) = i
        IF ( VBNM(i).EQ.' SURFACE LEAKAGE' ) Vbnm_index(9) = i
        IF ( VBNM(i).EQ.'  STREAM LEAKAGE' ) Vbnm_index(10) = i
        IF ( VBNM(i).EQ.'   LAKE  SEEPAGE' ) Vbnm_index(11) = i
        IF ( VBNM(i).EQ.'         STORAGE' ) Vbnm_index(12) = i
        IF ( VBNM(i).EQ.'INTERBED STORAGE' ) Vbnm_index(13) = i
      ENDDO

      END SUBROUTINE MODFLOW_VB_DECODE

!***********************************************************************
!***********************************************************************
      SUBROUTINE getStreamFlow(Iret)
      USE GSFBUDGET, ONLY:Nreach, Reach_cfs, Reach_wse, Basin_cfs,
     &    Basin_stflow, Basin_cms, Stream_leakage, CFS2CMS,
     &    Stream_inflow
      USE GSFCONVERT, ONLY:Mfl3t_to_cfs, Cfs2inches
      USE GWFSFRMODULE, ONLY:STRM, IOTSG, NSS, SGOTFLW, SEG
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Arguments
      INTEGER :: Iret
! Local Variables
      INTEGER :: i
!***********************************************************************
      Stream_leakage = 0.0
      DO i = 1, Nreach
! Reachcfs is not used to calculate a value. Remove?
        Reach_cfs(i) = STRM(9, i)*Mfl3t_to_cfs
        Reach_wse(i) = STRM(15, i)
        Stream_leakage = Stream_leakage + STRM(11, i)
      ENDDO

! Total streamflow out of basin for all streams leaving model area.
! Total specified streamflow into model area.
      Basin_cfs = 0.0
      Stream_inflow = 0.0
      DO i = 1, NSS
        IF ( IOTSG(i).EQ.0 ) THEN   
          Basin_cfs = Basin_cfs + SGOTFLW(i)
        END IF
        IF ( SEG(2, i).LT.0.0 ) THEN
          Basin_cfs = Basin_cfs + SEG(2, i)
        ELSE
          Stream_inflow = Stream_inflow + SEG(2, i)
        END IF
      ENDDO 
      Basin_cfs = Basin_cfs*Mfl3t_to_cfs
      Basin_cms = Basin_cfs*CFS2CMS
      Basin_stflow = Basin_cfs*Cfs2inches
      Iret = 1
      IF ( putvar('strmflow', 'basin_cfs', 1, 'real', Basin_cfs)
     &     .NE.0 ) RETURN
      IF ( putvar('strmflow', 'basin_stflow', 1, 'real', Basin_stflow)
     &     .NE.0 ) RETURN
      Iret = putvar('strmflow', 'basin_cms', 1, 'real', Basin_cms)

      END SUBROUTINE getStreamFlow

!***********************************************************************
!***********************************************************************
      SUBROUTINE getHeads()
      USE GSFBUDGET, ONLY:Ngwcell, Gwc_head
      USE GLOBAL, ONLY:NCOL, HNEW
      IMPLICIT NONE
      INTRINSIC SNGL
! Local Variables
      INTEGER :: l, r, c, n, i
!***********************************************************************
      l = 1
      r = 1
      c = 1
      n = NCOL + 1
      DO i = 1, Ngwcell
        Gwc_head(i) = SNGL(HNEW(c, r, l))
        c = c + 1
        IF ( c.EQ.n ) THEN
          c = 1
          r = r + 1
        ENDIF
      ENDDO

      END SUBROUTINE getHeads

!***********************************************************************
!     READ AND PREPARE INFORMATION FOR STRESS PERIOD.
!***********************************************************************
      SUBROUTINE getPump()
      USE GSFBUDGET, ONLY:Total_pump, Total_pump_cfs, Vbnm_index
      USE GSFCONVERT, ONLY:Mfl3t_to_cfs
      USE GWFBASMODULE, ONLY:VBVL
      IMPLICIT NONE

!***********************************************************************
      Total_pump = 0.0
      
      ! wells
      IF ( Vbnm_index(5).NE.-1 ) Total_pump = Total_pump -
     &                                        VBVL(4, Vbnm_index(5))
     
      ! multi node wells
      IF ( Vbnm_index(6).NE.-1 ) Total_pump = Total_pump -
     &                                        VBVL(4, Vbnm_index(6))
     
      ! wells
      IF ( Vbnm_index(5).NE.-1 ) Total_pump = Total_pump +
     &                                        VBVL(3, Vbnm_index(5))
     
      ! multi node wells
      IF ( Vbnm_index(6).NE.-1 )Total_pump = Total_pump +
     &                                       VBVL(3, Vbnm_index(6))
     
      Total_pump_cfs = Total_pump * Mfl3t_to_cfs

      END SUBROUTINE getPump

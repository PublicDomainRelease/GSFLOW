!***********************************************************************
!     Perform the MODFLOW budget procedure for PRMS soil zone
!***********************************************************************
      MODULE GSFBUDGET
!   Local Variables
      INTEGER, SAVE :: Nreach
      INTEGER, SAVE :: Vbnm_index(13)
      REAL, SAVE :: Gw_bnd_in, Gw_bnd_out, Well_in, Well_out
      REAL, SAVE :: Basin_actetgw
!   Declared Variables
      REAL, SAVE :: Total_pump, Total_pump_cfs, Sat_change_stor
      REAL, SAVE :: Stream_leakage, Sat_store, Unsat_store
      REAL, SAVE :: Stream_inflow, Basin_gw2sm, Gw_inout, Basin_szreject
      REAL, SAVE, ALLOCATABLE :: Reach_cfs(:), Reach_wse(:)
      REAL, SAVE, ALLOCATABLE :: Gw_rejected(:), Gw2sm(:), Actet_gw(:)
      REAL, SAVE, ALLOCATABLE :: Actet_tot_gwsz(:)
!   Declared Variables from other modules - srunoff
      REAL :: Basin_sroff
      END MODULE GSFBUDGET

!     ******************************************************************
!     Budget module to convert PRMS & MODFLOW states for use by GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_budget()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gsfbuddecl, gsfbudinit, gsfbudrun
!***********************************************************************
      gsflow_budget = 0

      IF ( Process_flag==0 ) THEN
        gsflow_budget = gsfbudrun()
      ELSEIF ( Process_flag==1 ) THEN
        gsflow_budget = gsfbuddecl()
      ELSEIF ( Process_flag==2 ) THEN
        gsflow_budget = gsfbudinit()
      ENDIF

      END FUNCTION gsflow_budget

!***********************************************************************
!     gsfbuddecl - set up parameters
!   Declared Parameters
!     hru_area, gvr_hru_id, gvr_cell_id, lake_hru_id
!***********************************************************************
      INTEGER FUNCTION gsfbuddecl()
      USE GSFBUDGET
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declvar, getdim
!***********************************************************************
      gsfbuddecl = 1

      IF ( declmodule(
     &'$Id: gsflow_budget.f 3116 2011-05-17 16:20:01Z rsregan $')
     &     .NE.0 ) RETURN

      Nreach = getdim('nreach')
      IF ( Nreach.EQ.-1 ) RETURN

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

      ALLOCATE (Reach_cfs(Nreach))
      IF ( declvar('gsfbud', 'reach_cfs', 'nreach', Nreach, 'real',
     &     'Stream flow leaving each stream reach', 'cfs',
     &     Reach_cfs).NE.0 ) RETURN

      ALLOCATE (Reach_wse(Nreach))
      IF ( declvar('gsfbud', 'reach_wse', 'nreach', Nreach, 'real',
     &     'Water surface elevation in each stream reach', 'length',
     &     Reach_wse).NE.0 ) RETURN

      IF ( declvar('gsfbud', 'basin_gw2sm', 'one', 1, 'real',
     &     'Basin average water exfiltrated from UZF and added to SZ',
     &     'inches',
     &     Basin_gw2sm).NE.0) RETURN

      ALLOCATE (Gw2sm(Nhru))
      IF ( declvar('gsfbud', 'gw2sm', 'nhru', Nhru, 'real',
     &     'HRU average water exfiltrated from groundwater model'//
     &     ' and added back to SM', 'inches',
     &     Gw2sm).NE.0 ) RETURN

      ALLOCATE (Actet_gw(Nhru))
      IF ( declvar('gsfbud', 'actet_gw', 'nhru', Nhru, 'real',
     &     'Actual ET from each GW cell', 'inches',
     &     Actet_gw).NE.0 ) RETURN

      ALLOCATE (Actet_tot_gwsz(Nhru))
      IF ( declvar('gsfbud', 'actet_tot_gwsz', 'nhru', Nhru, 'real',
     &     'Total actual ET from each GW cell and PRMS soil zone',
     &     'inches', Actet_tot_gwsz).NE.0 ) RETURN

      gsfbuddecl = 0

      END FUNCTION gsfbuddecl

!***********************************************************************
!     gsfbudinit - Initialize GSFBUDGET module - get parameter values
!***********************************************************************
      INTEGER FUNCTION gsfbudinit()
      USE GSFBUDGET
      USE PRMS_BASIN, ONLY: Timestep
      USE GWFSFRMODULE, ONLY: NSTRM
      IMPLICIT NONE
!***********************************************************************
      gsfbudinit = 1

      IF ( Nreach/=NSTRM ) THEN
        PRINT *, 'Error, nreach must equal to NSTRM', Nreach, NSTRM
        RETURN
      ENDIF

      IF ( Timestep==0 ) THEN
        Reach_cfs = 0.0 ! dimension NSTRM
        Reach_wse = 0.0 ! dimension NSTRM
        Total_pump = 0.0
        Total_pump_cfs = 0.0
        Unsat_store = 0.0
        Sat_store = 0.0
        Sat_change_stor = 0.0
        Stream_leakage = 0.0
        Stream_inflow = 0.0
        Basin_szreject = 0.0
        Basin_gw2sm = 0.0
        Gw_rejected = 0.0 ! dimension nhru
        Gw2sm = 0.0 ! dimension nhru
        Actet_gw = 0.0 ! dimension nhru
        Actet_tot_gwsz = 0.0 ! dimension nhru
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
      USE GSFCONVERT, ONLY: Mfq2inch_conv, Nhrucell, Mfl2_to_acre,
     &    Mfvol2inch_conv, Mfl3t_to_cfs, Mfl_to_inch, Gwc_col, Gwc_row,
     &    Gvr_cell_id
!Warning, modifies Gw_rejected_grav, Gw_rejected_grav
      USE GSFPRMS2MF, ONLY: Excess, Gvr_hru_pct_adjusted, Hrucheck,
     &    Gvr_hru_id, Gw_rejected_grav, Lake_hru_id, Lake_area
!Warning, modifies Gw2sm_grav
      USE GSFMF2PRMS, ONLY: Gw2sm_grav
      USE GLOBAL, ONLY: IUNIT
      USE GWFBASMODULE, ONLY: VBVL
      USE GWFUZFMODULE, ONLY: SEEPOUT, UZFETOUT, UZTSRAT, REJ_INF, GWET
      USE GWFLAKMODULE, ONLY: EVAP, SURFA
!      USE GSFMODFLOW, ONLY: KKITER
      USE GSFMODFLOW, ONLY: Have_lakes
!Warning, modifies Basin_gwflow_cfs, Basin_cfs, Basin_cms, Basin_stflow,
!                  Basin_ssflow_cfs, Basin_sroff_cfs
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type,
     &    Hru_perv, Hru_percent_perv, Basin_area_inv, Basin_gwflow_cfs,
     &    Basin_ssflow_cfs, Basin_sroff_cfs, Hru_area
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_lakeevap, Hru_actet,
     &    Basin_actet
      USE PRMS_OBS, ONLY: Cfs_conv
!Warning, modifies Basin_soil_moist, Basin_ssstor, Basin_gvr2sm,
      USE PRMS_SOILZONE, ONLY: Pref_flow_stor, Gvr2sm, Basin_gvr2sm,
     &    Slow_stor, Gravity_stor_res, Soil_moist, Ssres_stor,
     &    Basin_ssstor, Basin_soil_moist
!      USE PRMS_SOILZONE, ONLY: Pref_flow_thrsh
      IMPLICIT NONE
      INTRINSIC SNGL
      INTEGER, EXTERNAL :: getvar
      EXTERNAL MODFLOW_GET_STORAGE_BCF, MODFLOW_GET_STORAGE_LPF
      EXTERNAL MODFLOW_VB_DECODE, getStreamFlow, getPump
!     EXTERNAL getHeads
! Local Variables
      INTEGER :: i, ihru, icell, irow, icol, ii, lake
      REAL :: modflow_in, modflow_out, gwdisch, deficit, flux_change
      REAL :: pct, harea, area_fac, temp
      DOUBLE PRECISION :: inches_on_lake
!***********************************************************************
      gsfbudrun = 1

! adjust gravity flow storage using last gw discharge and rejected

      IF ( getvar('srunoff', 'basin_sroff', 1, 'real', Basin_sroff)
     &     .NE.0 ) RETURN
      area_fac = Cfs_conv/Basin_area_inv
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_sroff_cfs = Basin_sroff*area_fac

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        Gw2sm(i) = 0.0
        Gw_rejected(i) = 0.0
        Actet_gw(i) = 0.0
        Slow_stor(i) = 0.0
      ENDDO

      DO i = 1, Nhrucell
        ihru = Gvr_hru_id(i)
        IF ( Hrucheck(ihru)/=1 ) CYCLE
        icell = Gvr_cell_id(i)
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)
        pct = Gvr_hru_pct_adjusted(i)
!-----------------------------------------------------------------------
! Add any excess infiltration to Gw_rejected array
! rejected can be added now as water would not have been used for
! any other purpose, it was water that could have been sent to MODFLOW
! if MODFLOW could accept it, includes (1) binning (Excess) and rejected
! infiltration due to (2) inactive cells and too many waves
! (Gw_rejected_grav(i)), (3) exceeding K and high gw head (gw
! discharging into GVR) (REJ_INF).
!-----------------------------------------------------------------------
        temp = Excess(icell)*Mfl_to_inch
     &         + REJ_INF(icol, irow)*Mfq2inch_conv(i)
        !Gw_rejcted_grav includes rejected soil_to_gw 
        Gw_rejected_grav(i) = Gw_rejected_grav(i) + temp
        Gw_rejected(ihru) = Gw_rejected(ihru) + Gw_rejected_grav(i)*pct

        gwdisch = SEEPOUT(icol, irow)*Mfq2inch_conv(i)
! flux equals current minus last GW discharge
        flux_change = gwdisch - Gw2sm_grav(i)
        !sanity check remove later
!        IF ( gwdisch.LT.0.0 ) print *, 'seepout problem', gwdisch
        Gw2sm_grav(i) = gwdisch
        Gw2sm(ihru) = Gw2sm(ihru) + gwdisch*pct
        Gravity_stor_res(i) = Gravity_stor_res(i) + flux_change
     &                        + Gw_rejected_grav(i)

        IF ( Gravity_stor_res(i)<0.0 ) THEN
          deficit = -Gravity_stor_res(i)*pct
          IF ( Gvr2sm(ihru)>deficit ) THEN
            Gvr2sm(ihru) = Gvr2sm(ihru) - deficit
          ELSE
            Gvr2sm(ihru) = 0.0
          ENDIF
          Soil_moist(ihru) = Soil_moist(ihru)
     &                       - deficit/Hru_percent_perv(ihru)
          Gravity_stor_res(i) = 0.0
        ENDIF

        Slow_stor(ihru) = Slow_stor(ihru) + Gravity_stor_res(i)*pct

        !sanity check, see if overfilling storage, remove later
!       IF ( Gravity_stor_res(i).GT.Pref_flow_thrsh(ihru) ) PRINT *,
!    &       'overfilling GVR storage', ihru, i, Gravity_stor_res(i),
!    &       Pref_flow_thrsh(ihru), Gw2sm_grav(i), Gw_rejected_grav(i),
!    &       gwdisch, Gravity_stor_res(i) - Pref_flow_thrsh(ihru)
        Actet_gw(ihru) = Actet_gw(ihru) + (GWET(icol,irow) +
     &                   UZFETOUT(icol, irow))*Mfvol2inch_conv(i)*pct
      ENDDO

! adjust basin_soil_moist and Basin_ssstor as they may have been updated
      Basin_soil_moist = 0.0
      Basin_ssstor = 0.0
      Basin_gw2sm = 0.0
      Basin_gvr2sm = 0.0
      Basin_szreject = 0.0
      Basin_lakeevap = 0.0
      Basin_actetgw = 0.0
      Basin_actet = 0.0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        harea = Hru_area(i)
        IF ( Have_lakes==1 ) THEN
!-----------------------------------------------------------------------
! Get actual et from lakes
!-----------------------------------------------------------------------
          IF ( Hru_type(i)==2 ) THEN
            lake = Lake_hru_id(i)
            !EVAP in mfl3/dt   SURFA in MFL2/dt
            inches_on_lake = EVAP(lake)/SURFA(lake)*Mfl_to_inch
            Hru_actet(i) = SNGL(inches_on_lake)*SURFA(lake)*Mfl2_to_acre
     &                     /Lake_area(lake)
            ! does not include any ET from UZF, i.e., dry areas in lake
            Actet_tot_gwsz(i) = Hru_actet(i)
            Basin_lakeevap = Basin_lakeevap + Hru_actet(i)*harea
            Basin_actet = Basin_actet + Hru_actet(i)*harea
            CYCLE
          ENDIF
        ENDIF
        !rsr, where does deficit go in water budget
        IF ( Soil_moist(i)<0.0 ) THEN
! water probably ET'd out, set gravity_stor_res(i)=0, should be small
! if this happens, value is small (<1.0e-4) but still add to actet to
! maintain water balance
!          IF ( deficit.LT.-1.0E-4 ) PRINT *,
!     &         'negative GW flux > soil_moist', Soil_moist(ihru),
!     &         Gravity_stor_res(i), deficit, ihru, i, flux_change,
!     &         gwdisch, Gw2sm_last_grav(i), Gw_rejected_grav(i), icell
!     &         , KKITER, Gvr2sm(ihru)
          Hru_actet(i) =Hru_actet(i) - Soil_moist(i)/Hru_percent_perv(i)
          Soil_moist(i) = 0.0
        ENDIF
        Actet_tot_gwsz(i) = Hru_actet(i) + Actet_gw(i)
        !rsr, need to adjust hru_actet for UZF
        Hru_actet(i) = Actet_tot_gwsz(i)
        Basin_actet = Basin_actet + Hru_actet(i)*harea
        Basin_actetgw = Basin_actetgw + Actet_gw(i)*harea
        Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*Hru_perv(i)
        Basin_gw2sm = Basin_gw2sm + Gw2sm(i)*harea
        Basin_gvr2sm = Basin_gvr2sm + Gvr2sm(i)*harea
        Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)
        Basin_ssstor = Basin_ssstor + Ssres_stor(i)*harea
        Basin_szreject = Basin_szreject + Gw_rejected(i)*harea
      ENDDO

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_actetgw = Basin_actetgw*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_gw2sm = Basin_gw2sm*Basin_area_inv
      Basin_gvr2sm = Basin_gvr2sm*Basin_area_inv
      Basin_szreject = Basin_szreject*Basin_area_inv
      Basin_lakeevap = Basin_lakeevap*Basin_area_inv

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

      CALL getStreamFlow()

      Basin_gwflow_cfs = Stream_leakage*Mfl3t_to_cfs

!     CALL getHeads()

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
      SUBROUTINE getStreamFlow()
      USE GSFBUDGET, ONLY:Reach_cfs, Reach_wse, Stream_leakage,
     &    Stream_inflow
      USE GSFCONVERT, ONLY:Mfl3t_to_cfs, Cfs2inches
      USE GWFSFRMODULE, ONLY:STRM, IOTSG, NSS, SGOTFLW, SFRRATOUT,
     &    TOTSPFLOW, NSTRM, SFRRATIN
      USE PRMS_BASIN, ONLY: Basin_cfs, Basin_cms, Basin_stflow,
     &    CFS2CMS_CONV
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
!***********************************************************************
      DO i = 1, NSTRM
! Reach_cfs and reach_wse are not used except to be available for output
        Reach_cfs(i) = STRM(9, i)*Mfl3t_to_cfs
        Reach_wse(i) = STRM(15, i)
      ENDDO

! Total streamflow out of basin for all streams leaving model area.
! Total specified streamflow into model area.
      Basin_cfs = 0.0
      Stream_inflow = 0.0
      DO i = 1, NSS
        IF ( IOTSG(i).EQ.0 ) THEN   
          Basin_cfs = Basin_cfs + SGOTFLW(i)
        END IF
      ENDDO 
      IF ( TOTSPFLOW.LT.0.0 ) THEN
        Basin_cfs = Basin_cfs + TOTSPFLOW
      ELSE
! RGN added specified inflows and outflows from SFR. 
        Stream_inflow = Stream_inflow + TOTSPFLOW
      END IF
! RGN added next line.
      Stream_leakage = SFRRATIN - SFRRATOUT 
      Basin_cfs = Basin_cfs*Mfl3t_to_cfs
      Basin_cms = Basin_cfs*CFS2CMS_CONV
      Basin_stflow = Basin_cfs*Cfs2inches

      END SUBROUTINE getStreamFlow

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

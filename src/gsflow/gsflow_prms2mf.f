!***********************************************************************
!     Route PRMS gravity flow to MODFLOW cells
!***********************************************************************
      MODULE GSFPRMS2MF
      IMPLICIT NONE
!   Module Variables
      REAL, PARAMETER :: PCT_CHK = 0.000005
      INTEGER :: Nhru, Nhrucell, Ngwcell, Nsegment, Nreach
      INTEGER :: NTRAIL_CHK, Sziters, Szcheck
      INTEGER :: Stopcount
      INTEGER, ALLOCATABLE :: Iter_cnt(:)
      REAL, ALLOCATABLE :: Excess(:), Sm2gw_grav_older(:), Hru2cfs(:)
!   Declared Variables
      INTEGER :: Maxgziter
      REAL :: Basin_reach_latflow, Net_sz2gw
      INTEGER, ALLOCATABLE :: Reach_id(:,:)
      REAL, ALLOCATABLE :: Reach_latflow(:), Gw_rejected_grav(:)
      REAL, ALLOCATABLE :: Cell_drain_rate(:), Unused_potet(:)
!   Declared Variables from other modules - gsflow_modflow
      INTEGER :: KKITER, Logunt
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus, Prt_debug
      INTEGER, ALLOCATABLE :: Ncascade_hru(:), Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Variables from other modules - srunoff
      REAL, ALLOCATABLE :: Sroff(:), Strm_seg_in(:)
      REAL, ALLOCATABLE :: Hortonian_lakes(:)
!   Declared Variables from other modules - potet
      REAL, ALLOCATABLE :: Potet(:)
!   Declared Variables from other modules - soilzone
      REAL, ALLOCATABLE :: Lakein_sz(:), Sm2gw_grav(:)
      REAL, ALLOCATABLE :: Ssres_flow(:), Hru_actet(:)
      REAL, ALLOCATABLE :: Sm2gw_grav_old(:)
!   Declared Variables from other modules - precip
      REAL, ALLOCATABLE :: Hru_ppt(:)
!   Declared Parameters
      INTEGER :: Mxsziter
      REAL :: Szconverge
      INTEGER, ALLOCATABLE :: Gvr_hru_id(:), Gvr_cell_id(:)
      INTEGER, ALLOCATABLE :: Local_reachid(:), Hru_segment(:)
      INTEGER, ALLOCATABLE :: Reach_segment(:), Numreach_segment(:)
      INTEGER, ALLOCATABLE :: Lake_hru_id(:), Hru_type(:)
      REAL, ALLOCATABLE :: Gvr_hru_pct(:), Gvr_cell_pct(:), Hru_area(:)
      REAL, ALLOCATABLE :: Segment_pct_area(:)
      END MODULE GSFPRMS2MF

!     ******************************************************************
!     Mapping module to convert PRMS & MODFLOW states for use by GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_prms2mf(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: prms2mfdecl, prms2mfinit, prms2mfrun
!***********************************************************************
      gsflow_prms2mf = 0

      IF ( Arg.EQ.'run' ) THEN
        gsflow_prms2mf = prms2mfrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        gsflow_prms2mf = prms2mfdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        gsflow_prms2mf = prms2mfinit()
      ENDIF

      END FUNCTION gsflow_prms2mf

!***********************************************************************
!     prms2mfdecl - set up parameters
!   Declared Parameters
!     numreach_segment
!     local_reachid, reach_segment, hru_segment, segment_pct_area
!     gvr_hru_id, gvr_cell_id, gvr_hru_pct, gvr_cell_pct
!     hru_area, mxsziter, szconverge
!***********************************************************************
      INTEGER FUNCTION prms2mfdecl()
      USE GSFPRMS2MF
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      prms2mfdecl = 1

      IF ( declmodule(
     &'$Id: gsflow_prms2mf.f 3927 2008-03-05 20:51:12Z rsregan $')
     &     .NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nsegment = getdim('nsegment')
      IF ( Nsegment.EQ.-1 ) RETURN

      Nreach = getdim('nreach')
      IF ( Nreach.EQ.-1 ) RETURN

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell.EQ.-1 ) RETURN

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell.EQ.-1 ) RETURN

! Declared Variables
      IF ( declvar('prms2mf', 'net_sz2gw', 'one', 1, 'real',
     &     'Net volumetric flow rate of gravity drainage from the'//
     &     ' soil zone to the unsaturated and saturated zones', 'L3/T',
     &     Net_sz2gw).NE.0 ) RETURN

      IF ( declvar('prms2mf', 'MAXGZITER', 'one', 1, 'integer',
     &     'Maximum iterations infiltration is allowed to vary',
     &     'none ',
     &     Maxgziter).NE.0 ) RETURN

      ALLOCATE (Unused_potet(Nhru))
      IF ( declvar('prms2mf', 'unused_potet', 'nhru', Nhru, 'real',
     &     'Unsatisfied potential ET for UZF and MODFLOW', 'inches',
     &     Unused_potet).NE.0 ) RETURN
     
      ALLOCATE (Reach_latflow(Nreach))
      IF ( declvar('prms2mf', 'reach_latflow', 'nreach', Nreach, 'real',
     &     'Lateral flow (surface runoff and interflow) into each'//
     &     'stream reach', 'cfs',
     &     Reach_latflow).NE.0 ) RETURN

      ALLOCATE (Reach_id(Nsegment, Nreach))
      IF ( declvar('prms2mf', 'reach_id', 'nsegment,nreach',
     &     Nsegment*Nreach, 'real',
     &     'Mapping of reach id by segment id', 'none',
     &     Reach_id).NE.0 ) RETURN

      ALLOCATE (Cell_drain_rate(Ngwcell))
      IF ( declvar('prms2mf', 'cell_drain_rate', 'ngwcell', Ngwcell,
     &     'real', 'Recharge rate for each cell', 'MF L/T',
     &     Cell_drain_rate).NE.0 ) RETURN

      IF ( declvar('prms2mf', 'basin_reach_latflow', 'one', 1, 'real',
     &     'Lateral flow into all reaches in basin', 'cfs',
     &     Basin_reach_latflow).NE.0 ) RETURN

      ALLOCATE (Gw_rejected_grav(Nhrucell))
      IF ( declvar('prms2mf', 'gw_rejected_grav', 'nhrucell', Nhrucell,
     &     'real',
     &     'Recharge rejected by UZF for each gravity-flow reservoir',
     &     'inches',
     &     Gw_rejected_grav).NE.0 ) RETURN

! Declared Parameters
      IF ( declparam('prms2mf', 'szconverge', 'one', 'real',
     &     '1.0E-8', '1.0E-15', '1.0E-1',
     &     'Significant difference for checking soilzone states',
     &     'Significant difference for checking soilzone states',
     &     'inches').NE.0 ) RETURN

      IF ( declparam('prms2mf', 'mxsziter', 'one', 'integer',
     &     '15', '2', '200',
     &     'Maximum number of iterations soilzone states are computed',
     &     'Maximum number of iterations soilzone states are computed',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_id(Nhrucell))
      IF ( declparam('prms2mf', 'gvr_hru_id', 'nhrucell', 'integer',
     &     '1', 'bounded', 'nhru',
     &     'Corresponding HRU id of each GVR',
     &     'Index of the HRU assocated with each gravity reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_cell_id(Nhrucell))
      IF ( declparam('prms2mf', 'gvr_cell_id', 'nhrucell', 'integer',
     &     '0', 'bounded', 'ngwcell',
     &     'Corresponding MODFLOW cell id of each GVR',
     &     'Index of the MODFLOW cell associated with each gravity'//
     &     ' reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_pct(Nhrucell))
      IF ( declparam('prms2mf', 'gvr_hru_pct', 'nhrucell', 'real',
     &     '0.0', '0.0', '1.0',
     &     'Proportion of the HRU associated with each GVR',
     &     'Proportion of the HRU area associated with each gravity'//
     &     ' reservoir',
     &     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Gvr_cell_pct(Nhrucell))
      IF ( declparam('prms2mf', 'gvr_cell_pct', 'nhrucell', 'real',
     &     '0.0', '0.0', '1.0',
     &     'Proportion of the MODFLOW cell associated with each GVR',
     &     'Proportion of the MODFLOW cell area associated with each'//
     &     ' gravity reservoir',
     &     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Local_reachid(Nreach))
      IF ( declparam('prms2mf', 'local_reachid', 'nreach', 'integer',
     &     '0', 'bounded', 'nreach',
     &     'Map of the global reach ids to reach ids of each segment',
     &     'Index of stream reach within a stream segment for each'//
     &     ' stream reach',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Reach_segment(Nreach))
      IF ( declparam('prms2mf', 'reach_segment', 'nreach', 'integer',
     &     '0', 'bounded', 'nsegment',
     &     'Map of the stream reaches to the stream segments',
     &     'Index of stream segment associate with each stream reach',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Hru_segment(Nhru))
      IF ( declparam('prms2mf', 'hru_segment', 'nhru', 'integer',
     &     '0', 'bounded', 'nsegment',
     &     'Map of HRUs to stream segments',
     &     'Index of HRU associated with each stream segments',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Segment_pct_area(Nreach))
      IF ( declparam('prms2mf', 'segment_pct_area', 'nreach', 'real',
     &     '0.0', '0.0', '1.0',
     &     'HRU contributing area percent mapped to each reach',
     &     'Proportion of the HRU area that contributes flow to a'//
     &     ' stream reach',
     &     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Numreach_segment(Nsegment))
      IF ( declparam('prms2mf', 'numreach_segment', 'nsegment',
     &     'integer', '0', 'bounded', 'nreach',
     &     'Number of reaches in each segment',
     &     'Number of stream reaches in each stream segment',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('prms2mf', 'hru_area', 'nhru', 'real',
     &     '1.0', '0.01', '1e+09',
     &     'HRU area', 'Area of each HRU',
     &     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('prms2mf', 'hru_type', 'nhru', 'integer',
     &     '1', '0', '2',
     &     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Lake_hru_id(Nhru))
      IF ( declparam('prms2mf', 'lake_hru_id', 'nhru', 'integer',
     &     '0', 'bounded', 'nhru',
     &     'MODFLOW lake associated with each HRU',
     &     'Index of the MODFLOW lake associated with each HRU',
     &     'none').NE.0 ) RETURN

! Allocate arrays from other modules and local arrays
      ALLOCATE (Potet(Nhru), Lakein_sz(Nhru))
      ALLOCATE (Ssres_flow(Nhru), Hru_actet(Nhru), Hru2cfs(Nhru))
      ALLOCATE (Sm2gw_grav_older(Nhrucell), Sm2gw_grav(Nhrucell))
      ALLOCATE (Excess(Ngwcell), Sm2gw_grav_old(Nhrucell), Sroff(Nhru))
      ALLOCATE (Ncascade_hru(Nhru), Strm_seg_in(Nsegment))
      ALLOCATE (Hru_ppt(Nhru), Hortonian_lakes(Nhru))
      ALLOCATE (Hru_route_order(Nhru))

      prms2mfdecl = 0

      END FUNCTION prms2mfdecl

!***********************************************************************
!     prms2mfinit - Initialize PRMS2MF module - get parameter values
!***********************************************************************
      INTEGER FUNCTION prms2mfinit()
      USE GSFPRMS2MF
      USE GSFCONVERT, ONLY:Acre_inches_to_cfs
      USE GLOBAL, ONLY:ISSFLG
      USE GWFUZFMODULE, ONLY:NTRAIL, NWAV
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC ABS, FLOAT
! Local Variables
      INTEGER :: nstep, ic, is, i, iseg, max_seg, irch, ii
      REAL :: totalarea, pctdiff
      INTEGER, ALLOCATABLE, DIMENSION(:) :: nseg_rch
      REAL, ALLOCATABLE, DIMENSION(:) :: hru_pct, cell_pct, seg_area
!***********************************************************************
      prms2mfinit = 1

      IF ( getvar('gsflow_modflow', 'logunt', 1, 'integer', Logunt)
     &     .NE.0 ) RETURN
 
      IF ( getparam('prms2mf', 'hru_area', Nhru, 'real', Hru_area)
     &     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     &     .NE.0 ) RETURN

      IF ( getparam('prms2mf', 'szconverge', 1, 'real', Szconverge)
     &     .NE.0 ) RETURN

      IF ( getparam('prms2mf', 'mxsziter', 1, 'integer', Mxsziter)
     &     .NE.0 ) RETURN
      WRITE (Logunt, *) 'Szconverge =', Szconverge, 'Mxsziter =',
     &                  Mxsziter
      WRITE (Logunt, *) 'Tolerance check for Gvr_hru_pct:', PCT_CHK
      ALLOCATE (Iter_cnt(Mxsziter))

      IF ( getparam('prms2mf', 'gvr_hru_id', Nhrucell, 'integer',
     &     Gvr_hru_id).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'gvr_cell_id', Nhrucell, 'integer',
     &     Gvr_cell_id).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'gvr_hru_pct', Nhrucell, 'real',
     &     Gvr_hru_pct).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'gvr_cell_pct', Nhrucell, 'real',
     &     Gvr_cell_pct).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'reach_segment', Nreach, 'integer',
     &     Reach_segment).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'hru_segment', Nhru, 'integer',
     &     Hru_segment).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'segment_pct_area', Nreach, 'real',
     &     Segment_pct_area).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'numreach_segment', Nsegment, 'integer',
     &     Numreach_segment).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'local_reachid', Nreach, 'integer',
     &     Local_reachid).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'lake_hru_id', Nhru, 'integer',
     &     Lake_hru_id).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'hru_type', Nhru, 'integer', Hru_type)
     &     .NE.0 ) RETURN

      IF ( getvar('basin', 'ncascade_hru', Nhru, 'integer',
     &     Ncascade_hru).NE.0 ) RETURN
     
      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     &     Hru_route_order).NE.0 ) RETURN
     
      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     &     .NE.0 ) RETURN
     
      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     &     .NE.0 ) RETURN
     
!  DANGER markstro - overriding the parameter Segment_pct_area to test
!                    precision issues
      DO i = 1, Nreach
        iseg = Reach_segment(i)
        Segment_pct_area(i) = 1.0 / real(Numreach_segment(iseg))
      ENDDO
     
      nstep = getstep()
      IF ( nstep.EQ.0 ) THEN
        ALLOCATE (nseg_rch(Nsegment), seg_area(Nsegment))
        DO i = 1, Nsegment
          nseg_rch(i) = 0
          seg_area(i) = 0.0
        ENDDO
        max_seg = 0
        Reach_id = 0
        DO i = 1, Nreach
          iseg = Reach_segment(i)
          IF ( iseg.GT.max_seg ) max_seg = iseg
          IF ( iseg.GT.Nsegment ) PRINT *,
     &         'Problem with segment number', i, Nsegment, iseg
          irch = Local_reachid(i)
          seg_area(iseg) = seg_area(iseg) + Segment_pct_area(i)
          IF ( irch.GT.Numreach_segment(iseg) ) PRINT *,
     &         'Problem with segment reach id', i, irch,
     &         Numreach_segment(iseg)
          Reach_id(iseg, irch) = i
          nseg_rch(iseg) = nseg_rch(iseg) + 1
        ENDDO
        IF ( max_seg.NE.Nsegment ) PRINT *,
     &       'Problem with number of segments', Nsegment, max_seg
        
        DO i = 1, Nsegment
          IF ( nseg_rch(i).NE.Numreach_segment(i) ) PRINT *,
     &         'Problem with number of reaches in a segment', i,
     &         nseg_rch(i), Numreach_segment(i)
          IF ( ABS(seg_area(i)-1.0).GT.PCT_CHK ) WRITE (Logunt, *)
     &         'Possible issue with segment area percentages', i,
     &         seg_area(i)
        ENDDO
        
! way to adjust segment_pct_area, rsr
!       DO i = 1, Nreach
!         iseg = Reach_segment(i)
!         Segment_pct_area(i) = Segment_pct_area(i) +
!    &            Segment_pct_area(i)*(1.-seg_area(iseg))/seg_area(iseg)
!         WRITE (39,'(f15.13)') Segment_pct_area(i)
!       ENDDO
!       seg_area = 0.0
!       DO i = 1, Nreach
!         iseg = Reach_segment(i)
!         seg_area(iseg) = seg_area(iseg) + Segment_pct_area(i)
!       ENDDO
!       WRITE (39,'(f15.13)') seg_area
!       STOP

        ALLOCATE (cell_pct(Ngwcell))
        cell_pct = 0.0
        ALLOCATE (hru_pct(Nhru))
        hru_pct = 0.0

        DO i = 1, Nhrucell
          ic = Gvr_cell_id(i)
          is = Gvr_hru_id(i)
          cell_pct(ic) = cell_pct(ic) + Gvr_cell_pct(i)
          hru_pct(is) = hru_pct(is) + Gvr_hru_pct(i)
        ENDDO

! way to adjust gvr_hru_pct, rsr
!       DO i = 1, Nhrucell
!         is = Gvr_hru_id(i)
!         Gvr_hru_pct(i) = Gvr_hru_pct(i) +
!    &                     Gvr_hru_pct(i)*(1.-hru_pct(is))/hru_pct(is)
!         WRITE (39,'(f15.13)') Gvr_hru_pct(i)
!       ENDDO
!       hru_pct = 0.0
!       DO i = 1, Nhrucell
!         is = Gvr_hru_id(i)
!         hru_pct(is) = hru_pct(is) + Gvr_hru_pct(i)
!       ENDDO
!       WRITE (39,'(f15.13)') hru_pct
!       STOP

        totalarea = 0.0
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          Hru2cfs(i) = Hru_area(i)*Acre_inches_to_cfs
          IF ( ABS(hru_pct(i)-1.0).GT.PCT_CHK ) WRITE (Logunt, *)
     &         'Possible issue with GVR to HRU precentage, HRU:', i,
     &         hru_pct(i)
          totalarea = totalarea + hru_pct(i)*Hru_area(i)
          IF ( Ncascade_hru(i).GT.0 .AND. Hru_segment(i).GT.0 ) THEN
!            WRITE (Logunt, *) 'Warning, hru_segment set to 0 for HRU:',
!     &                         i, ' as the HRU has cascades' 
            Hru_segment(i) = 0
          ENDIF
        ENDDO
        totalarea = totalarea*Basin_area_inv
        WRITE (Logunt, *)
     &     'Percentage difference between GVR mapping and basin area:',
     &     (totalarea-1.0)*100.0

        ii = 0
        totalarea = 0.0
        DO i = 1, Ngwcell
          IF ( cell_pct(i).GT.0.0 ) THEN
            pctdiff = cell_pct(i) - 1.0
            IF ( pctdiff>PCT_CHK ) THEN
              PRINT *, 'Will make some water in cell:', i, cell_pct(i)
            ELSEIF ( pctdiff<-PCT_CHK ) THEN
              PRINT *, 'Will lose some water in cell:', i, cell_pct(i)
            ENDIF
            totalarea = totalarea + pctdiff
            ii = ii + 1
          ENDIF
        ENDDO
        WRITE (Logunt, *) 'Percentage difference between cell mapping:',
     &                    totalarea/FLOAT(ii)*100.0

! way to adjust Gvr_cell_pct, rsr
!       DO i = 1, Nhrucell
!         ic = Gvr_cell_id(i)
!         Gvr_cell_pct(i) = Gvr_cell_pct(i) +
!    &                    Gvr_cell_pct(i)*(1.-cell_pct(ic))/cell_pct(ic)
!         WRITE (39,'(f15.13)') Gvr_cell_pct(i)
!       ENDDO
!       cell_pct = 0.0
!       DO i = 1, Nhrucell
!         ic = Gvr_cell_id(i)
!         cell_pct(ic) = cell_pct(ic) + Gvr_cell_pct(i)
!       ENDDO
!       WRITE (39,'(f14.11)') cell_pct
!       STOP

        DEALLOCATE (nseg_rch, seg_area, hru_pct, cell_pct)

        Maxgziter = 0
        Cell_drain_rate = 0.0
        Reach_latflow = 0.0
        Unused_potet = 0.0
        Basin_reach_latflow = 0.0
        Gw_rejected_grav = 0.0
        Net_sz2gw = 0.0
      ENDIF

      Stopcount = 0
      Sziters = 0
      Iter_cnt = 0

      NTRAIL_CHK = NWAV - 3*NTRAIL + 1

      WRITE (Logunt, '(1X)')

      prms2mfinit = 0

      END FUNCTION prms2mfinit

!***********************************************************************
!     prms2mfrun - Maps the PRMS results to MODFLOW cells
! NOTE:  This module assumes that the Sm2gw_grav variable is in inches.
!        It produces cell_drain in MODFLOW units.
!***********************************************************************
      INTEGER FUNCTION prms2mfrun()
      USE GSFPRMS2MF
      USE GSFCONVERT, ONLY:Gvr2cell_conv, Acre_inches_to_mfl3,
     &    Inch_to_mfl_t, Gwc_row, Gwc_col
      USE GLOBAL, ONLY:NLAY, IOUT, IBOUND
      USE GWFUZFMODULE, ONLY:IUZFBND, NWAVST, PETRATE, IGSFLOW
      USE GWFLAKMODULE, ONLY:RNF, EVAPLK, PRCPLK
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INCLUDE 'fmodules.inc'
      INTEGER, EXTERNAL :: toStream_ET
      EXTERNAL Bin_percolation
! Local Variables
      INTEGER :: irow, icol, ik, jk, ibndcheck, nsignif, ii
      INTEGER :: j, icell, ihru, icheck, maxdiff_cell, maxdiff_cell2
      REAL :: diff, seep, diff2, maxdiff, maxdiff2
!***********************************************************************
      prms2mfrun = 1

! gsflow sets to current iteration
      IF ( getvar('gsflow', 'KKITER', 1, 'integer', KKITER)
     &     .NE.0 ) RETURN
      IF ( KKITER.LT.4 ) THEN
        icheck = 1
      ELSE
        icheck = 0
      ENDIF

!-----------------------------------------------------------------------
! Computed unused potential ET and add runoff to stream reaches
!-----------------------------------------------------------------------
      IF ( toStream_ET().NE.0 ) RETURN

!-----------------------------------------------------------------------
! Add runoff and precip to lakes
! Pass in potet for the lake
!-----------------------------------------------------------------------
      IF ( getvar('soilzone', 'lakein_sz', Nhru, 'real', Lakein_sz)
     &     .NE.0 ) RETURN
      IF ( getvar('precip', 'hru_ppt', Nhru, 'real', Hru_ppt)
     &     .NE.0 ) RETURN
      IF ( getvar('srunoff', 'hortonian_lakes', Nhru, 'real',
     &     Hortonian_lakes)
     &     .NE.0 ) RETURN

      DO ii = 1, Active_hrus
        j = Hru_route_order(ii)
        IF ( Hru_type(j).EQ.2 ) THEN
          RNF(Lake_hru_id(j)) = (Lakein_sz(j)+Hortonian_lakes(j))
     &                          *Hru_area(j)*Acre_inches_to_mfl3
!         if (rnf(lake_hru_id(j))>0.) print *, rnf(lake_hru_id(j))
          PRCPLK(Lake_hru_id(j)) = Hru_ppt(j)*Inch_to_mfl_t
          EVAPLK(Lake_hru_id(j)) = Potet(j)*Inch_to_mfl_t
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
      IF ( getvar('soilzone', 'sm2gw_grav', Nhrucell, 'real',
     &     Sm2gw_grav).NE.0 ) RETURN
      IF ( getvar('soilzone', 'sm2gw_grav_old', Nhrucell, 'real',
     &     Sm2gw_grav_old).NE.0 ) RETURN

      PETRATE = 0.0
      Cell_drain_rate = 0.0
      maxdiff = 0.0
      maxdiff_cell = 0
      maxdiff2 = 0.0
      maxdiff_cell2 = 0
      nsignif = 0
      Gw_rejected_grav = 0.0
      DO j = 1, Nhrucell
        ihru = Gvr_hru_id(j)
        IF ( Hru_type(ihru).NE.1 ) CYCLE
        icell = Gvr_cell_id(j)
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)

        jk = 0
        ik = 1
        DO WHILE ( jk.EQ.0 .AND. ik.LE.NLAY )
          IF ( IBOUND(icol, irow, ik).GT.0 ) jk = 1
          ik = ik + 1
        ENDDO
        ibndcheck = 1
        IF ( jk.EQ.0 .OR. IUZFBND(icol, irow).EQ.0 ) ibndcheck = 0
!-----------------------------------------------------------------------
! If UZF cell is inactive OR if too many waves then dump water back into
! the soilzone
!-----------------------------------------------------------------------
        seep = Sm2gw_grav(j)
        IF ( seep.GT.0.0 ) THEN
          IF ( ibndcheck.EQ.0 ) THEN
            Gw_rejected_grav(j) = seep
!            PRINT *, 'inactive uzf cell', icol, irow, seep, j, ihru
          ELSEIF ( NWAVST(icol, irow).GE.NTRAIL_CHK ) THEN
            WRITE (IOUT, *) '--WARNING-- Too many waves in UZF cell'
            WRITE (IOUT, *) ' col =', icol, ' row =', irow, 'numwaves=',
     &                       NTRAIL_CHK
            PRINT *, '--WARNING-- Too many waves in UZF cell: col =',
     &            icol, 'row =', irow, 'cell=', icell, 'numwaves=',
     &            NTRAIL_CHK
            Gw_rejected_grav(j) = seep
          ELSE
!-----------------------------------------------------------------------
! Convert drainage from inches to MF Length/Time
!-----------------------------------------------------------------------
            IF ( icheck.EQ.0 ) THEN
!rsr, check to see if current infiltration is within a tolerance of
!     the last iteration, if so, stop recomputing soil zone states
              diff = ABS(seep-Sm2gw_grav_old(j))
              IF ( diff.GT.Szconverge ) THEN
!rsr, check to see if current infiltration is equal to (within a
!     tolerance) of the iteration before last (i.e, solution is likely
!     oscillating), if so, stop recomputing soil zone states
                IF ( KKITER.GT.2 ) THEN
                  diff2 = ABS(seep-Sm2gw_grav_older(j))
                  IF ( diff2.GT.PCT_CHK ) THEN
                    icheck = 1
                    IF ( diff2.GT.maxdiff2 ) THEN
                      maxdiff2 = diff2
                      maxdiff_cell2 = icell
                    ENDIF
                  ENDIF
                ELSE
                  icheck = 1
                ENDIF
                IF ( diff.GT.maxdiff ) THEN
                  maxdiff = diff
                  maxdiff_cell = icell
                ENDIF
              ENDIF
            ENDIF
            Cell_drain_rate(icell) = Cell_drain_rate(icell)
     &                               + seep*Gvr2cell_conv(j)
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
! Get the remaining potet from the HRU and put it into the cell
! Unused_potet() is in inches
!-----------------------------------------------------------------------
        IF ( Unused_potet(ihru).GT.0.0 ) PETRATE(icol, irow)
     &       = PETRATE(icol, irow) + Unused_potet(ihru)*Gvr2cell_conv(j)
      ENDDO

      IF ( KKITER.EQ.Mxsziter ) THEN
! check if current iteration changed insignificantly or was oscillating
        IF ( icheck.EQ.1 ) nsignif = 1
        icheck = 0
      ENDIF
 
      IF ( icheck.EQ.0 ) THEN
        Szcheck = 0
        IF ( nsignif.EQ.1 ) THEN
          Stopcount = Stopcount + 1
          IF ( Prt_debug.EQ.1 ) WRITE (Logunt, *) 'Mxsziter reached',
     &         Stopcount, 'Change still significant in cell:',
     &         maxdiff_cell, maxdiff, 'Older difference:',
     &         maxdiff_cell2, maxdiff2
        ENDIF
        Iter_cnt(KKITER) = Iter_cnt(KKITER) + 1
      ELSE
        IF ( KKITER.GT.1 ) Sm2gw_grav_older = Sm2gw_grav_old
      ENDIF
      Sziters = Sziters + 1
      Maxgziter = KKITER

!-----------------------------------------------------------------------
! Bin precolation in cell_drain_rate
!-----------------------------------------------------------------------
! Set flag for UZF when PRMS sets FINF
      IGSFLOW = 1
      CALL Bin_percolation()

      prms2mfrun = 0

      END FUNCTION prms2mfrun

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION toStream_ET()
      USE GSFPRMS2MF, ONLY:Nhru, Nreach, Potet, Reach_latflow,
     &    Unused_potet, Hru_segment, Numreach_segment, Reach_id,
     &    Segment_pct_area, Basin_reach_latflow, Ssres_flow, Hru_actet,
     &    PCT_CHK, Sroff, Strm_seg_in, Hru2cfs, Nsegment, Active_hrus,
     &    Hru_route_order, Hru_type
      USE GSFCONVERT, ONLY:Sfr_conv
      USE GWFSFRMODULE, ONLY:STRM
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, j, k, iseg, num_rch, ii
!***********************************************************************
      toStream_ET = 1

      IF ( getvar('potet', 'potet', Nhru, 'real', Potet).NE.0 ) RETURN
      IF ( getvar('ssflow', 'ssres_flow', Nhru, 'real', Ssres_flow)
     &     .NE.0 ) RETURN
      IF ( getvar('soilzone', 'hru_actet', Nhru, 'real', Hru_actet)
     &     .NE.0 ) RETURN
      IF ( getvar('srunoff', 'sroff', Nhru, 'real', Sroff).NE.0 ) RETURN
      IF ( getvar('srunoff', 'strm_seg_in', Nsegment, 'real',
     &     Strm_seg_in).NE.0 ) RETURN

! DANGER if the HRU is not connected to a stream, don't go into the loop
! change Hru_segment to map HRUs to streams or determine in srunoff
! if hru_segment = 0, then not connected
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Hru_type(i).NE.1 ) CYCLE
        Unused_potet(i) = Potet(i) - Hru_actet(i)
        iseg = Hru_segment(i)
        ! this should be zero when cascades are being used, as all HRUs
        ! should cascade somewhere, this code is for non-cascade models
        IF ( iseg.GT.0 ) Strm_seg_in(iseg) = Strm_seg_in(iseg) +
     &                               Hru2cfs(i)*(Sroff(i)+Ssres_flow(i))
      ENDDO
      
      Basin_reach_latflow = 0.0      
      DO iseg = 1, Nsegment
        num_rch = Numreach_segment(iseg)
        DO j = 1, num_rch
          k = Reach_id(iseg, j)
!-----------------------------------------------------------------------
! Convert inches over the HRU to CFS
!-----------------------------------------------------------------------
          Reach_latflow(k) = Strm_seg_in(iseg)*Segment_pct_area(k)
          Basin_reach_latflow = Basin_reach_latflow + Reach_latflow(k)
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
! Reach_latflow() is in cfs
! convert the gain to the SFR reach to correct units
!-----------------------------------------------------------------------
      DO i = 1, Nreach
        STRM(12, i) = Reach_latflow(i)*Sfr_conv
      ENDDO

      toStream_ET = 0

      END FUNCTION toStream_ET

!***********************************************************************
! Bin percolation to reduce waves
!***********************************************************************
      SUBROUTINE Bin_percolation()
      USE GSFPRMS2MF, ONLY:Ngwcell, Excess, Cell_drain_rate, Net_sz2gw
      USE GSFCONVERT, ONLY:Cellarea, Gwc_row, Gwc_col
      USE GWFUZFMODULE, ONLY:FINF, VKS, FBINS, IUZFBND, NUZTOP, SURFDEP
      USE GLOBAL,       ONLY: NLAY, IBOUND, HNEW, BOTM
      IMPLICIT NONE
! Local Variables
      INTEGER :: icell, irow, icol, ij1, ij2, land
      REAL :: finfvks, finfprms, finf_temp, celtop
!***********************************************************************
      FINF = 0.0
      Excess = 0.0
      Net_sz2gw = 0.0
!-----------------------------------------------------------------------
! loop on cells
!-----------------------------------------------------------------------
      DO icell = 1, Ngwcell
        IF ( Cell_drain_rate(icell).GT.0.0 ) THEN
          irow = Gwc_row(icell)
          icol = Gwc_col(icell)
          land = IUZFBND(icol, irow)
          IF ( NUZTOP.EQ.1 ) THEN
            celtop = BOTM(icol, irow, 0) - 0.5 * SURFDEP
          ELSE
            celtop = BOTM(icol, irow, land-1) - 0.5 * SURFDEP
          END IF
!-----------------------------------------------------------------------
! VKS in L/T, FINF in L/T
!-----------------------------------------------------------------------
          finfvks = VKS(icol, irow)
          finfprms = Cell_drain_rate(icell)
          IF ( finfprms.GT.finfvks ) THEN
! reject part, as infiltration exceeds VKS, don't bin
            finf_temp = finfvks
! bin when water table is below celtop
          ELSE IF ( HNEW( icol, irow, land ).LT.celtop ) THEN
!-----------------------------------------------------------------------
! can accept all infiltration, bin to avoid too many waves.
!-----------------------------------------------------------------------
            finf_temp = finfprms
            IF ( finf_temp.LT.FBINS(1) ) THEN
              finf_temp = 0.0       ! reject small values
            ELSE
              IF ( finf_temp.LT.FBINS(25) ) THEN
                ij1 = 25
                ij2 = 1
              ELSE
                ij1 = 51
                ij2 = 25
              ENDIF
              DO WHILE ( ij1.GE.ij2 )
                IF ( finf_temp.GE.FBINS(ij1) ) THEN
                  finf_temp = FBINS(ij1)  ! set to bin value
                  EXIT
                ENDIF
                ij1 = ij1 - 1
              ENDDO
            ENDIF
          ELSE
            finf_temp = finfprms
          ENDIF
!-----------------------------------------------------------------------
! Put excess >VKS + bin difference in Excess array
!-----------------------------------------------------------------------
          Excess(icell) = finfprms - finf_temp
          FINF(icol, irow) = finf_temp
          Net_sz2gw = Net_sz2gw + finf_temp*Cellarea(icell)
        ENDIF
      ENDDO
      
      END SUBROUTINE Bin_percolation

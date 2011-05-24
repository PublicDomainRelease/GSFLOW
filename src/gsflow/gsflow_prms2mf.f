!***********************************************************************
!     Route PRMS gravity flow to MODFLOW cells
!***********************************************************************
      MODULE GSFPRMS2MF
      IMPLICIT NONE
!   Module Variables
      REAL, PARAMETER :: SZ_CHK = 0.00001
      DOUBLE PRECISION, PARAMETER :: PCT_CHK = 0.000005D0
!      INTEGER, SAVE :: Nreach
      INTEGER, SAVE :: NTRAIL_CHK, Nlayp1
      ! Number of stream reaches in each stream segment
      INTEGER, SAVE, ALLOCATABLE :: Numreach_segment(:), Hrucheck(:)
      REAL, SAVE, ALLOCATABLE :: Excess(:), Sm2gw_grav_older(:)
      REAL, SAVE, ALLOCATABLE :: Lake_area(:)
!   Declared Variables
      INTEGER, SAVE :: Stopcount
      REAL, SAVE :: Basin_reach_latflow, Net_sz2gw
!     INTEGER, SAVE, ALLOCATABLE :: Reach_id(:,:)
      REAL, SAVE, ALLOCATABLE :: Gw_rejected_grav(:)
!     REAL, SAVE, ALLOCATABLE :: Reach_latflow(:)
      REAL, SAVE, ALLOCATABLE :: Cell_drain_rate(:), Unused_potet(:)
      REAL, SAVE, ALLOCATABLE :: Segment_pct_area(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_hru_pct_adjusted(:)
!   Declared Variables from other modules - srunoff
      REAL, ALLOCATABLE :: Strm_seg_in(:), Hortonian_lakes(:)
!   Declared Parameters
      INTEGER, SAVE :: Mnsziter
      REAL, SAVE :: Szconverge
      INTEGER, SAVE, ALLOCATABLE :: Gvr_hru_id(:)
!     INTEGER, SAVE, ALLOCATABLE :: Local_reachid(:)
!     INTEGER, SAVE, ALLOCATABLE :: Reach_segment(:)
      INTEGER, SAVE, ALLOCATABLE :: Lake_hru_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_hru_pct(:)
      END MODULE GSFPRMS2MF

!     ******************************************************************
!     Mapping module to convert PRMS & MODFLOW states for use by GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_prms2mf()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: prms2mfdecl, prms2mfinit, prms2mfrun
!***********************************************************************
      gsflow_prms2mf = 0

      IF ( Process_flag==0 ) THEN
        gsflow_prms2mf = prms2mfrun()
      ELSEIF ( Process_flag==1 ) THEN
        gsflow_prms2mf = prms2mfdecl()
      ELSEIF ( Process_flag==2 ) THEN
        gsflow_prms2mf = prms2mfinit()
      ENDIF

      END FUNCTION gsflow_prms2mf

!***********************************************************************
!     prms2mfdecl - set up parameters
!   Declared Parameters
!     gvr_hru_id, gvr_cell_id, gvr_hru_pct
!     hru_area, mxsziter, szconverge, mnsziter
!***********************************************************************
      INTEGER FUNCTION prms2mfdecl()
      USE GSFPRMS2MF
      USE GSFCONVERT, ONLY: Nhrucell, Ngwcell
      USE PRMS_BASIN, ONLY: Nhru, Nsegment
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      prms2mfdecl = 1

      IF ( declmodule(
     &'$Id: gsflow_prms2mf.f 3116 2011-05-17 16:20:01Z rsregan $')
     &     .NE.0 ) RETURN

!      Nreach = getdim('nreach')
!      IF ( Nreach.EQ.-1 ) RETURN

! Declared Variables
      IF ( declvar('prms2mf', 'net_sz2gw', 'one', 1, 'real',
     &     'Net volumetric flow rate of gravity drainage from the'//
     &     ' soil zone to the unsaturated and saturated zones', 'L3/T',
     &     Net_sz2gw).NE.0 ) RETURN

      IF ( declvar('prms2mf', 'stopcount', 'one', 1, 'integer',
     &     'Number of times the mxsziter reached during a simulation',
     &     'none ',
     &     Stopcount).NE.0 ) RETURN

      ALLOCATE (Unused_potet(Nhru))
      IF ( declvar('prms2mf', 'unused_potet', 'nhru', Nhru, 'real',
     &     'Unsatisfied potential ET for UZF and MODFLOW', 'inches',
     &     Unused_potet).NE.0 ) RETURN
     
!     ALLOCATE (Reach_latflow(Nreach))
!     IF ( decl var('prms2mf', 'reach_latflow', 'nreach', Nreach, 'real',
!    &     'Lateral flow (surface runoff and interflow) into each'//
!    &     'stream reach', 'cfs',
!    &     Reach_latflow).NE.0 ) RETURN

!     ALLOCATE (Reach_id(Nreach, Nsegment))
!     IF ( decl var('prms2mf', 'reach_id', 'nsegment,nreach',
!    &     Nsegment*Nreach, 'integer',
!    &     'Mapping of reach id by segment id', 'none',
!    &     Reach_id).NE.0 ) RETURN

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

      !rsr, all reaches receive same precentage of flow to each segment
      ALLOCATE (Segment_pct_area(Nsegment))
      IF ( declvar('prms2mf', 'segment_pct_area', 'nsegment', Nsegment,
     &     'real',
     &     'Proportion of each segment that contributes flow to a'//
     &     ' stream reach',
     &     'decimal fraction',
     &     Segment_pct_area).NE.0 ) RETURN

      ALLOCATE (Gvr_hru_pct_adjusted(Nhrucell))
      IF ( declvar('prms2mf', 'gvr_hru_pct_adjusted', 'nhrucell',
     &     Nhrucell, 'real',
     &     'Proportion of the HRU area associated with each gravity'//
     &     ' reservoir adjusted to account for full HRU',
     &     'decimal fraction',
     &     Gvr_hru_pct_adjusted).NE.0 ) RETURN

! Declared Parameters
      IF ( declparam('prms2mf', 'szconverge', 'one', 'real',
     &     '1.0E-8', '1.0E-15', '1.0E-1',
     &     'Significant difference for checking soilzone states',
     &     'Significant difference for checking soilzone states',
     &     'inches').NE.0 ) RETURN

      IF ( declparam('prms2mf', 'mnsziter', 'one', 'integer',
     &     '4', '1', '200',
     &     'Minimum number of iterations soilzone states are computed',
     &     'Minimum number of iterations soilzone states are computed',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_id(Nhrucell))
      IF ( declparam('prms2mf', 'gvr_hru_id', 'nhrucell', 'integer',
     &     '1', 'bounded', 'nhru',
     &     'Corresponding HRU id of each GVR',
     &     'Index of the HRU assocated with each gravity reservoir',
     &     'none').NE.0 ) RETURN

      ALLOCATE (Gvr_hru_pct(Nhrucell))
      IF ( declparam('prms2mf', 'gvr_hru_pct', 'nhrucell', 'real',
     &     '0.0', '0.0', '1.0',
     &     'Proportion of the HRU associated with each GVR',
     &     'Proportion of the HRU area associated with each gravity'//
     &     ' reservoir',
     &     'decimal fraction').NE.0 ) RETURN

!     ALLOCATE (Local_reachid(Nreach))
!     IF ( decl param('prms2mf', 'local_reachid', 'nreach', 'integer',
!    &     '0', 'bounded', 'nreach',
!    &     'Map of the global reach ids to reach ids of each segment',
!    &     'Index of stream reach within a stream segment for each'//
!    &     ' stream reach',
!    &     'none').NE.0 ) RETURN

!     ALLOCATE (Reach_segment(Nreach))
!     IF ( decl param('prms2mf', 'reach_segment', 'nreach', 'integer',
!    &     '0', 'bounded', 'nsegment',
!    &     'Map of the stream reaches to the stream segments',
!    &     'Index of stream segment associate with each stream reach',
!    &     'none').NE.0 ) RETURN

      ALLOCATE (Numreach_segment(Nsegment))
!      IF ( decl param('prms2mf', 'numreach_segment', 'nsegment',
!     &     'integer', '0', 'bounded', 'nreach',
!     &     'Number of reaches in each segment',
!     &     'Number of stream reaches in each stream segment',
!     &     'none').NE.0 ) RETURN

      IF ( declparam('prms2mf', 'lake_hru_id', 'nhru', 'integer',
     +     '0', 'bounded', 'nhru',
     +     'Lake id associated with each HRU',
     +     'Index of the lake associated with each HRU; more than'//
     +     ' one HRU can be assigned to the same lake',
     +     'none').NE.0 ) RETURN

! Allocate arrays from other modules and local arrays
      ALLOCATE (Excess(Ngwcell), Strm_seg_in(Nsegment))
      ALLOCATE (Hortonian_lakes(Nhru), Hrucheck(Nhru))
      ALLOCATE (Sm2gw_grav_older(Nhrucell))

      prms2mfdecl = 0

      END FUNCTION prms2mfdecl

!***********************************************************************
!     prms2mfinit - Initialize PRMS2MF module - get parameter values
!***********************************************************************
      INTEGER FUNCTION prms2mfinit()
      USE GSFPRMS2MF
      USE GWFUZFMODULE, ONLY: NTRAIL, NWAV
      USE GWFSFRMODULE, ONLY: ISEG, NSS
      USE GWFLAKMODULE, ONLY: NLAKES
      USE GSFCONVERT, ONLY: Nhrucell
      USE GSFMODFLOW, ONLY: Have_lakes, Logunt
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type,
     +    Basin_area_inv, Hru_area, Nsegment, Nhru, Timestep
      USE GSFMODFLOW, ONLY: Mxsziter
      USE GLOBAL, ONLY:NLAY
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      INTRINSIC ABS, FLOAT, REAL, DBLE
! Local Variables
      INTEGER :: is, i, ii, ierr
!     INTEGER :: iseg, max_seg, irch
      DOUBLE PRECISION :: totalarea
!     INTEGER, ALLOCATABLE, DIMENSION(:) :: nseg_rch
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: hru_pct, newpct
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: temp_pct
!     REAL, ALLOCATABLE, DIMENSION(:) :: seg_area
!***********************************************************************
      prms2mfinit = 1

      Nlayp1 = NLAY + 1

      IF ( getparam('prms2mf', 'szconverge', 1, 'real', Szconverge)
     &     .NE.0 ) RETURN

      IF ( getparam('prms2mf', 'mnsziter', 1, 'integer', Mnsziter)
     &     .NE.0 ) RETURN
      IF ( Mnsziter<3 ) Mnsziter = 3
      WRITE (Logunt, *) 'szconverge =', Szconverge, 'mxsziter =',
     &                  Mxsziter, 'mnsziter =', Mnsziter
      WRITE (Logunt, *) 'Tolerance check for Gvr_hru_pct:', PCT_CHK

      IF ( getparam('prms2mf', 'gvr_hru_id', Nhrucell, 'integer',
     &     Gvr_hru_id).NE.0 ) RETURN

      IF ( getparam('prms2mf', 'gvr_hru_pct', Nhrucell, 'real',
     &     Gvr_hru_pct).NE.0 ) RETURN

!     IF ( get param('prms2mf', 'reach_segment', Nreach, 'integer',
!    &     Reach_segment).NE.0 ) RETURN

      IF ( Nsegment/=NSS ) THEN
        PRINT *, 'ERROR, nsegment must equal NSS', Nsegment, NSS
        STOP
      ENDIF

!  DANGER markstro - overriding the parameter Segment_pct_area to test
!                    precision issues
      !rsr, only need number of reaches per segment, divide inflow to
      !     segments by number of reaches
!     DO i = 1, Nreach
!       iseg = Reach_segment(i)
!       Segment_pct_area(i) = 1.0 / REAL(Numreach_segment(iseg))
!     ENDDO
!      IF ( get param('prms2mf', 'segment_pct_area', Nreach, 'real',
!     &     Segment_pct_area).NE.0 ) RETURN
!      IF ( get param('prms2mf', 'numreach_segment', Nsegment, 'integer',
!     &     Numreach_segment).NE.0 ) RETURN
      DO i = 1, Nsegment
        Numreach_segment(i) = ISEG(4, i)
        Segment_pct_area(i) = 1.0 / REAL(Numreach_segment(i))
      ENDDO

!     IF ( get param('prms2mf', 'local_reachid', Nreach, 'integer',
!    &     Local_reachid).NE.0 ) RETURN

      IF ( Have_lakes==1 ) THEN
        ALLOCATE (Lake_hru_id(Nhru))
        IF ( getparam('prms2mf', 'lake_hru_id', Nhru, 'integer',
     &       Lake_hru_id).NE.0 ) RETURN
        ALLOCATE ( Lake_area(NLAKES) )
        Lake_area = 0.0
      ENDIF

      IF ( Timestep==0 ) THEN
!       ALLOCATE (nseg_rch(Nsegment), seg_area(Nsegment))
!       DO i = 1, Nsegment
!         nseg_rch(i) = 0
!         seg_area(i) = 0.0
!       ENDDO
!       k = 0
!       DO i = 1, Nsegment
!         DO j = 1, Numreach_segment(i)
!           k = k + 1
!           Reach_id(j, i) = k
!           Reach_latflow(k) = 0.0
!         ENDDO
!       ENDDO
!       IF ( k/=Nreach ) THEN
!         PRINT *, 'nreach (', nreach,
!    +          ') should equal sum of number of reaches per segment (',
!    +          k, ')'
!         RETURN
!       ENDIF
!       max_seg = 0
!       Reach_id = 0
!       DO i = 1, Nreach
!         iseg = Reach_segment(i)
!         IF ( iseg.GT.max_seg ) max_seg = iseg
!         IF ( iseg.GT.Nsegment ) PRINT *,
!    &         'Problem with segment number', i, Nsegment, iseg
!         irch = Local_reachid(i)
!         seg_area(iseg) = seg_area(iseg) + Segment_pct_area(i)
!         IF ( irch.GT.Numreach_segment(iseg) ) PRINT *,
!    &         'Problem with segment reach id', i, irch,
!    &         Numreach_segment(iseg)
!         Reach_id(iseg, irch) = i
!         nseg_rch(iseg) = nseg_rch(iseg) + 1
!         Reach_latflow(i) = 0.0
!       ENDDO
!       IF ( max_seg.NE.Nsegment ) PRINT *,
!    &       'Problem with number of segments', Nsegment, max_seg

!       DO i = 1, Nsegment
!         IF ( nseg_rch(i).NE.Numreach_segment(i) ) PRINT *,
!    &         'Problem with number of reaches in a segment', i,
!    &         nseg_rch(i), Numreach_segment(i)
!         IF ( ABS(seg_area(i)-1.0).GT.PCT_CHK ) WRITE (Logunt, *)
!    &         'Possible issue with segment area percentages', i,
!    &         seg_area(i)
!       ENDDO

! way to adjust segment_pct_area, rsr
!       WRITE (839,*) '####'
!       WRITE (839,*) 'segment_pct_area 12'
!       WRITE (839,*) '1'
!       WRITE (839,*) 'nreach'
!       WRITE (839,*) Nreach
!       WRITE (839,*) '2'
!       DO i = 1, Nreach
!         iseg = Reach_segment(i)
!         Segment_pct_area(i) = Segment_pct_area(i) +
!    &            Segment_pct_area(i)*(1.-seg_area(iseg))/seg_area(iseg)
!         WRITE (839,'(f15.13)') Segment_pct_area(i)
!       ENDDO
!       seg_area = 0.0
!       DO i = 1, Nreach
!         iseg = Reach_segment(i)
!         seg_area(iseg) = seg_area(iseg) + Segment_pct_area(i)
!       ENDDO
!       WRITE (839,'(f15.13)') seg_area
!       STOP

        Cell_drain_rate = 0.0 ! dimension ngwcell

        ALLOCATE (hru_pct(Nhru), newpct(Nhru), temp_pct(Nhrucell))
        ierr = 0
        hru_pct = 0.0D0
        newpct = 0.0D0
        Unused_potet = 0.0
        Hrucheck = 1 !if land or swale
        DO i = 1, Nhru
          IF ( Hru_type(i)==0 ) THEN
            Hrucheck(i) = 0
          ELSEIF ( Hru_type(i)==2 ) THEN
            ! Lake package active if Have_lakes=1
            IF ( Have_lakes==0 ) THEN
              WRITE (*, 9001) i
              ierr = 1
! must separate condition as lake_hru_id not allocated if have_lakes=0
            ELSE
              IF ( Lake_hru_id(i)==0 ) THEN
                WRITE (*, 9001) i
                ierr = 1
              ELSE
                Lake_area(Lake_hru_id(i)) = Lake_area(Lake_hru_id(i))
     +                                      + Hru_area(i)
              ENDIF
              Hrucheck(i) = 0
            ENDIF
          ENDIF
        ENDDO
        IF ( ierr==1 ) STOP

        DO i = 1, Nhrucell
          temp_pct(i) = DBLE(Gvr_hru_pct(i))
          is = Gvr_hru_id(i)
          hru_pct(is) = hru_pct(is) + temp_pct(i)
        ENDDO
        Gw_rejected_grav = 0.0

! way to adjust gvr_hru_pct, rsr
!       WRITE (840,*) '####'
!       WRITE (840,*) 'gvr_hru_pct 12'
!       WRITE (840,*) '1'
!       WRITE (840,*) 'nhrucell'
!       WRITE (840,*) Nhrucell
!       WRITE (840,*) '2'
        DO i = 1, Nhrucell
          is = Gvr_hru_id(i)
          temp_pct(i) = temp_pct(i) +
     &                  temp_pct(i)*(1.0D0-hru_pct(is))/hru_pct(is)
          Gvr_hru_pct_adjusted(i) = SNGL(temp_pct(i))
          newpct(is) = newpct(is) + temp_pct(i)
!         WRITE (840,'(f15.13)') temp_pct(i)
        ENDDO
!       STOP

        totalarea = 0.0
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          IF ( ABS(newpct(i)-1.0D0)>PCT_CHK ) WRITE (Logunt, *)
     &         'Possible issue with GVR to HRU precentage, HRU:', i,
     &         hru_pct(i)
          totalarea = totalarea + newpct(i)*DBLE(Hru_area(i))
        ENDDO
        totalarea = totalarea*DBLE(Basin_area_inv)
        WRITE (Logunt, *)
     &     'Percentage difference between GVR mapping and basin area:',
     &     (totalarea-1.0)*100.0

        DEALLOCATE (hru_pct, newpct, temp_pct)
!       DEALLOCATE (nseg_rch, seg_area)

        Basin_reach_latflow = 0.0
        Net_sz2gw = 0.0
      ENDIF

      Stopcount = 0

      NTRAIL_CHK = NWAV - 3*NTRAIL + 1

      WRITE (Logunt, '(1X)')

      DEALLOCATE ( Gvr_hru_pct )

      prms2mfinit = 0

 9001 FORMAT ('ERROR, HRU:', I7, ' is specified as a lake (hru_type=2)',
     +        ' and lake_hru_id is specified as 0.', /,
     +        'The associated MODFLOW lake must be specified as the',
     +        ' value of lake_hru_id for this HRU.')

      END FUNCTION prms2mfinit

!***********************************************************************
!     prms2mfrun - Maps the PRMS results to MODFLOW cells
! NOTE:  This module assumes that the Sm2gw_grav variable is in inches.
!        It produces cell_drain in MODFLOW units.
!***********************************************************************
      INTEGER FUNCTION prms2mfrun()
      USE GSFPRMS2MF
      USE GSFCONVERT, ONLY: Gvr2cell_conv, Acre_inches_to_mfl3,
     &    Inch_to_mfl_t, Gwc_row, Gwc_col, Gvr_cell_id, Nhrucell
      USE GLOBAL, ONLY: IBOUND
!     USE GLOBAL, ONLY: IOUT
      USE GWFUZFMODULE, ONLY: IUZFBND, NWAVST, PETRATE, IGSFLOW
      USE GWFLAKMODULE, ONLY: RNF, EVAPLK, PRCPLK, NLAKES
      USE GSFMODFLOW, ONLY: Szcheck, Have_lakes, Logunt, KKITER,
     +    Mxsziter
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type,
     +    Hru_area, Nhru, Nsegment
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Potet
      USE PRMS_FLOWVARS, ONLY: Hru_actet
      USE PRMS_SOILZONE, ONLY: Sm2gw_grav_old, Sm2gw_grav, Lakein_sz
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: toStream, getvar
      EXTERNAL Bin_percolation
! Local Variables
      INTEGER :: irow, icol, ik, jk, ibndcheck, ii, ilake
      INTEGER :: j, icell, ihru, icheck, maxdiff_cell
      REAL :: seep
      REAL :: diff, diff2, maxdiff
!***********************************************************************
      prms2mfrun = 1

! gsflow sets to current iteration
      IF ( KKITER.LT.Mnsziter ) THEN
        icheck = -2
        Szcheck = 2
      ELSE
        icheck = 0
        Szcheck = 1
      ENDIF

!-----------------------------------------------------------------------
! Add runoff to stream reaches
!-----------------------------------------------------------------------
      IF ( toStream().NE.0 ) RETURN

!-----------------------------------------------------------------------
! Add runoff and precip to lakes
! Pass in hru_actet for the lake
!-----------------------------------------------------------------------
      IF ( Have_lakes==0 ) THEN
        DO ii = 1, Active_hrus
          j = Hru_route_order(ii)
          Unused_potet(j) = Potet(j) - Hru_actet(j)
        ENDDO
      ELSE
        IF ( getvar('srunoff', 'hortonian_lakes', Nhru, 'real',
     &       Hortonian_lakes).NE.0 ) RETURN
        RNF = 0.0
        PRCPLK = 0.0
        EVAPLK = 0.0
        DO ii = 1, Active_hrus
          j = Hru_route_order(ii)
          IF ( Hru_type(j)/=2 ) THEN
            Unused_potet(j) = Potet(j) - Hru_actet(j)
          ELSE
            ilake = Lake_hru_id(j)
            RNF(ilake) = RNF(ilake) + (Lakein_sz(j)+Hortonian_lakes(j))
     &                   *Hru_area(j)*Acre_inches_to_mfl3
            PRCPLK(ilake) = PRCPLK(ilake) + Hru_ppt(j)*Inch_to_mfl_t
     &                                      *Hru_area(j)
            EVAPLK(ilake) = EVAPLK(ilake) + Hru_actet(j)*Inch_to_mfl_t
     &                                      *Hru_area(j)
          ENDIF
        ENDDO
        DO ilake = 1, NLAKES
          PRCPLK(ilake) = PRCPLK(ilake)/Lake_area(ilake)
          EVAPLK(ilake) = EVAPLK(ilake)/Lake_area(ilake)
        ENDDO
      ENDIF      

!-----------------------------------------------------------------------
      PETRATE = 0.0 ! should just be active cells
      Cell_drain_rate = 0.0 ! should just be active cells

      maxdiff = 0.0
      maxdiff_cell = 0
      DO j = 1, Nhrucell
        ihru = Gvr_hru_id(j)
        IF ( Hrucheck(ihru)==0 ) CYCLE
        Gw_rejected_grav(j) = 0.0
        icell = Gvr_cell_id(j)
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)

        jk = 0
        ik = 1
        DO WHILE ( jk.EQ.0 .AND. ik<Nlayp1 )
          IF ( IBOUND(icol, irow, ik).GT.0 ) jk = 1
          ik = ik + 1
        ENDDO

        ibndcheck = 1
        IF ( jk.EQ.0 ) THEN
          ibndcheck = 0
        ELSEIF ( IUZFBND(icol, irow).EQ.0 ) THEN
          ibndcheck = 0
        ENDIF
!-----------------------------------------------------------------------
! If UZF cell is inactive OR if too many waves then dump water back into
! the soilzone
!-----------------------------------------------------------------------
        seep = Sm2gw_grav(j)
        IF ( seep.GT.0.0 ) THEN
          IF ( ibndcheck/=0 ) THEN
            IF ( NWAVST(icol, irow)<NTRAIL_CHK ) THEN
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
                  diff2 = ABS(seep-Sm2gw_grav_older(j))
                  IF ( diff2.GT.SZ_CHK ) THEN
                    icheck = 1
                    maxdiff = diff
                    maxdiff_cell = icell
                  ENDIF
                ENDIF
              ENDIF
              Cell_drain_rate(icell) = Cell_drain_rate(icell)
     &                                 + seep*Gvr2cell_conv(j)
            ELSE ! ELSEIF ( NWAVST(icol, irow).GE.NTRAIL_CHK ) THEN
!              WRITE (IOUT, *) '--WARNING-- Too many waves in UZF cell'
!              WRITE (IOUT, *) ' col =', icol, ' row =', irow, 'numwaves=',
!     &                        NTRAIL_CHK
!              PRINT *, '--WARNING-- Too many waves in UZF cell: col =',
!     &                 icol, 'row =', irow, 'cell=', icell, 'numwaves=',
!     &                 NTRAIL_CHK
              Gw_rejected_grav(j) = seep
            ENDIF
          ELSE
!            PRINT *, 'inactive uzf cell', icol, irow, seep, j, ihru
            Gw_rejected_grav(j) = seep
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
! Get the remaining potet from the HRU and put it into the cell
! Unused_potet() is in inches
!-----------------------------------------------------------------------
        IF ( Unused_potet(ihru).GT.0.0 ) THEN
          PETRATE(icol, irow) = PETRATE(icol, irow)
     &                          + Unused_potet(ihru)*Gvr2cell_conv(j)
          Unused_potet(ihru) = Unused_potet(ihru) -
     &                        Unused_potet(ihru)*Gvr_hru_pct_adjusted(j)
          IF ( Unused_potet(ihru).LT.0.0 ) Unused_potet(ihru) = 0.0
        ENDIF
        Sm2gw_grav_older(j) = Sm2gw_grav_old(j)
      ENDDO
! check if current iteration changed insignificantly or was oscillating
      IF ( icheck.EQ.1 ) THEN
        IF ( KKITER==Mxsziter ) THEN
          Stopcount = Stopcount + 1
          Szcheck = -1
          WRITE (Logunt, *) 'Mxsziter reached', Stopcount,
     &                      'Change still significant in cell:',
     &                      maxdiff_cell, maxdiff
        ENDIF
      ELSEIF ( KKITER==Mxsziter ) THEN
        Szcheck = 0
        IF ( icheck==-2 ) Szcheck = -2
      ELSEIF ( icheck==0 ) THEN
        Szcheck = 0
      ENDIF
 
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
      INTEGER FUNCTION toStream()
      USE GSFPRMS2MF, ONLY: Numreach_segment, Segment_pct_area,
     &    Basin_reach_latflow, Strm_seg_in
!     USE GSFPRMS2MF, ONLY: Reach_latflow, Reach_id
      USE GSFCONVERT, ONLY: Sfr_conv
      USE GWFSFRMODULE, ONLY: STRM
      USE PRMS_BASIN, ONLY: Nsegment
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: getvar
! Local Variables
      INTEGER :: j, k, iseg
!     INTEGER :: num_rch
      REAL :: latflow
!***********************************************************************
      toStream = 1

      IF ( getvar('srunoff', 'strm_seg_in', Nsegment, 'real',
     &     Strm_seg_in).NE.0 ) RETURN

      Basin_reach_latflow = 0.0
      k = 0
      DO iseg = 1, Nsegment
        latflow = Strm_seg_in(iseg)*Segment_pct_area(iseg)
        DO j = 1, Numreach_segment(iseg)
!         k = Reach_id(j, iseg)
          k = k + 1
!-----------------------------------------------------------------------
! Convert inches over the HRU to CFS
!-----------------------------------------------------------------------
!         if (k<1 ) print *, iseg, j, reach_id(iseg,j)
!         print *, iseg, j, reach_id(iseg,j)
!         Reach_latflow(k) = Strm_seg_in(iseg)*Segment_pct_area(iseg)
!         Reach_latflow(k) = latflow
!-----------------------------------------------------------------------
! latflow is in cfs
! convert the gain to the SFR reach to correct units
!-----------------------------------------------------------------------          
          STRM(12, k) = latflow*Sfr_conv
!         Basin_reach_latflow = Basin_reach_latflow + Reach_latflow(k)
          Basin_reach_latflow = Basin_reach_latflow + latflow
        ENDDO
      ENDDO

      toStream = 0

      END FUNCTION toStream

!***********************************************************************
! Bin percolation to reduce waves
!***********************************************************************
      SUBROUTINE Bin_percolation()
      USE GSFPRMS2MF, ONLY: Excess, Cell_drain_rate, Net_sz2gw
      USE GSFCONVERT, ONLY: Cellarea, Gwc_row, Gwc_col, Ngwcell
      USE GWFUZFMODULE, ONLY: FINF, VKS, FBINS, IUZFBND, NUZTOP, SURFDEP
      USE GLOBAL, ONLY: HNEW, BOTM
      IMPLICIT NONE
      INTRINSIC ABS
! Local Variables
      INTEGER :: icell, irow, icol, ij1, ij2, land, ij2m1
      REAL :: finfvks, finfprms, finf_temp, celtop
!***********************************************************************
      Net_sz2gw = 0.0
!-----------------------------------------------------------------------
! loop on cells
!-----------------------------------------------------------------------
      DO icell = 1, Ngwcell
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)
        IF ( Cell_drain_rate(icell).GT.0.0 ) THEN
          land = ABS(IUZFBND(icol, irow))
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
              ij2m1 = ij2 - 1
              DO WHILE ( ij1>ij2m1 )
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
        ELSE
          Excess(icell) = 0.0
          FINF(icol, irow) = 0.0
        ENDIF
      ENDDO

      END SUBROUTINE Bin_percolation

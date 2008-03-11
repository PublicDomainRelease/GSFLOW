!***********************************************************************
!     Computes cascade orders for HRUs and GWRs
!***********************************************************************
      MODULE PRMS_CASCADE
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: MSGUNT = 189
      INTEGER :: Nhru, Ncascade, Nsegment, Ndown, Ncascdgw, Ngw
      INTEGER :: Nhrucell
      INTEGER :: Casc_typ, Nsegmentp1, Iorder, Igworder, Order_flg
      INTEGER, ALLOCATABLE :: Hrus_in(:), Gwrs_in(:)
!   Computed Variables
      INTEGER, ALLOCATABLE :: Hru_down(:, :), Gwr_down(:, :)
      REAL, ALLOCATABLE :: Hru_down_pct(:, :), Cascade_area(:, :)
      REAL, ALLOCATABLE :: Hru_down_pctwt(:, :), Gwr_down_pct(:, :)
! hru_down_pct: Percentage of HRU area used to compute flow routed
!               to a down slope HRU or stream segment.
! hru_down_pctwt: HRU area percent, area weighted by down slope HRU
!                 area, used to compute flow routed to a down slope
!                 HRU or stream segment.
! gwr_down_pct: Percentage of GWR area used to compute flow routed
!               to a down slope cascade area or stream segment from
!               each cascade area of an GWR.
! cascade_area: Cascade area within an HRU.
! hru_down: Indices of the down slope HRUs or stream segments to
!           which the cascade area routes flow.
! gwr_down: Indices of the down slope GWRs to which the cascade
!           area routes flow.
!   Declared Variables
      INTEGER, ALLOCATABLE :: Hru_to_stream(:)
      INTEGER :: Outflow_flg
!   Declared Variables from other modules - basin
! WARNING, this module modifies Hru_route_order and Ncascade_hru
      INTEGER :: Prt_debug, Active_hrus, Active_gwrs
      INTEGER, ALLOCATABLE :: Hru_route_order(:), Ncascade_hru(:)
      INTEGER, ALLOCATABLE :: Gwr_route_order(:), Ncascade_gwr(:)
      REAL, ALLOCATABLE :: Gwres_area(:)
!   Declared Parameters
      INTEGER :: Cascade_flg
      CHARACTER(LEN=9) :: Model_mode ! Model_mode is a control parameter
! Model_mode a control parameter
      REAL :: Cascade_tol
      INTEGER, ALLOCATABLE :: Hru_up_id(:), Hru_strmseg_down_id(:)
      INTEGER, ALLOCATABLE :: Hru_down_id(:), Hru_type(:)
      INTEGER, ALLOCATABLE :: Gw_up_id(:), Gw_strmseg_down_id(:)
      INTEGER, ALLOCATABLE :: Gw_down_id(:)
      REAL, ALLOCATABLE :: Hru_area(:), Hru_pct_up(:), Gw_pct_up(:)
      END MODULE PRMS_CASCADE

!***********************************************************************
!     Main cascade routine
!***********************************************************************
      INTEGER FUNCTION cascade_prms(Arg)
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: cascdecl, cascinit
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
!***********************************************************************
      cascade_prms = 0

      IF ( Arg.EQ.'declare' ) THEN
        cascade_prms = cascdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        cascade_prms = cascinit()
      ENDIF

      END FUNCTION cascade_prms

!***********************************************************************
!     cascdecl - set up parameters for cascading flow
!   Declared Parameters
!     hru_up_id, hru_down_id, hru_pct_up, hru_strmseg_down_id
!     gw_up_id, gw_down_id, gw_pct_up, gw_strmseg_down_id
!     hru_area, cascade_tol, cascade_flg
!***********************************************************************
      INTEGER FUNCTION cascdecl()
      USE PRMS_CASCADE
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      cascdecl = 1

      IF ( declmodule(
     +'$Id: cascade_prms.f 3870 2008-02-13 20:57:05Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ncascade = getdim('ncascade')
      IF ( Ncascade.EQ.-1 ) RETURN

      Ncascdgw = getdim('ncascdgw')
      IF ( Ncascdgw.EQ.-1 ) RETURN

      Ngw = getdim('ngw')
      IF ( Ngw.EQ.-1 ) RETURN

      Nsegment = getdim('nsegment')
      IF ( Nsegment.EQ.-1 ) RETURN
      Nsegmentp1 = Nsegment + 1

      Order_flg = 1
      IF ( control_string(Model_mode, 'model_mode').NE.0 ) RETURN
      IF ( Model_mode(:6)=='GSFLOW' ) THEN
        Nhrucell = getdim('nhrucell')
        IF ( Nhrucell.EQ.-1 ) RETURN
        IF ( Nhrucell==Nhru ) Order_flg = 2
      ENDIF

      IF ( declvar('cascade', 'outflow_flg', 'one', 1, 'integer',
     +     'Flag to indicate if an HRU is connected to a far-field'//
     +     ' stream (0=no; 1=yes).', 'none',
     +     Outflow_flg).NE.0 ) RETURN

      IF ( Ncascade.GT.0 ) THEN
! declare HRU variables
        ALLOCATE (Hru_to_stream(Nhru))
        IF ( declvar('cascade', 'hru_to_stream', 'nhru', Nhru,
     +       'integer',
     +       'Flag to indicate if an HRU is connected to a stream'//
     +       ' (0=no; 1=yes).', 'none',
     +       Hru_to_stream).NE.0 ) RETURN

! declare HRU cascade parameters
        ALLOCATE (Hru_up_id(Ncascade))
        IF ( declparam('cascade', 'hru_up_id', 'ncascade', 'integer',
     +       '1', 'bounded', 'nhru',
     +       'Index of HRU containing cascade area.',
     +       'Index of HRU containing cascade area.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Hru_strmseg_down_id(Ncascade))
        IF ( declparam('cascade', 'hru_strmseg_down_id', 'ncascade',
     +       'integer', '0', 'bounded', 'nsegment',  !rsr, actually nsegmentp1
     +       'Stream segment index that cascade area contributes flow.',
     +       'Stream segment index that cascade area contributes flow.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Hru_down_id(Ncascade))
        IF ( declparam('cascade', 'hru_down_id', 'ncascade', 'integer',
     +       '1', 'bounded', 'nhru',
     +       'HRU index of down slope HRU.',
     +       'HRU index number of the down slope HRU to which the HRU'//
     +       ' subarea contributes flow. If 0, that cascade is leaves'//
     +       ' the basin and the cascade flow is apportioned to other'//
     +       ' cascades from the HRU',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Hru_pct_up(Ncascade))
        IF ( declparam('cascade', 'hru_pct_up', 'ncascade', 'real',
     +       '1.0', '0.0', '1.0',
     +       'Percentage of HRU area associated with cascade area.',
     +       'Percentage of HRU area used to compute flow contributed'//
     +       ' to a down slope HRU or stream segment for cascade area.',
     +       'decimal fraction').NE.0 ) RETURN

        IF ( declparam('cascade', 'cascade_tol', 'one', 'real',
     +       '5.0', '0.0', '99.0',
     +       'Cascade area below which a cascade is ignored.',
     +       'Cascade area below which a cascade is ignored.',
     +       'acres').NE.0 ) RETURN

        IF ( declparam('cascade', 'cascade_flg', 'one', 'integer',
     +       '0', '0', '1',
     +       'Flag to indicate cascade type',
     +       'Flag to indicate cascade type (0=allow many to many;'//
     +       ' 1=force one to one)',
     +       'none').NE.0 ) RETURN

      ENDIF

      IF ( Ncascdgw.GT.0 ) THEN
! declare GWR cascade parameters
        ALLOCATE (Gw_up_id(Ncascdgw))
        IF ( declparam('cascade', 'gw_up_id', 'ncascdgw', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'Index of GWR containing cascade area.',
     +       'Index of GWR containing cascade area.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Gw_strmseg_down_id(Ncascdgw))
        IF ( declparam('cascade', 'gw_strmseg_down_id', 'ncascdgw',
     +       'integer', '0', 'bounded', 'nsegment',
     +       'Stream segment index that cascade area contributes flow.',
     +       'Stream segment index that cascade area contributes flow.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Gw_down_id(Ncascdgw))
        IF ( declparam('cascade', 'gw_down_id', 'ncascdgw', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'GWR index of down slope GWR.',
     +       'GWR index number of the down slope GWR to which the GWR'//
     +       ' subarea contributes flow.',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Gw_pct_up(Ncascdgw))
        IF ( declparam('cascade', 'gw_pct_up', 'ncascdgw', 'real',
     +       '1.0', '0.0', '1.0',
     +       'Percentage of GWR area associated with cascade area.',
     +       'Percentage of GWR area used to compute flow contributed'//
     +       ' to a down slope GWR or stream segment for cascade area.',
     +       'decimal fraction').NE.0 ) RETURN

      ENDIF

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('cascade', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('cascade', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '2',
     +     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     +     'none').NE.0 ) RETURN

! Allocate arrays from other moodules
      ALLOCATE (Hru_route_order(Nhru), Ncascade_hru(Nhru))
      ALLOCATE (Gwr_route_order(Ngw), Ncascade_gwr(Ngw))
      ALLOCATE (Hrus_in(Nhru), Gwrs_in(Ngw), Gwres_area(Ngw))

      cascdecl = 0
      END FUNCTION cascdecl

!***********************************************************************
!     cascinit - Initialize cascade module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION cascinit()
      USE PRMS_CASCADE
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Functions
      EXTERNAL init_cascade, initgw_cascade
! Local Variables
      INTEGER :: i, j, k, ii
!***********************************************************************
      cascinit = 1

      IF ( getparam('cascade', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_gwrs', 1, 'integer', Active_gwrs)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'gwr_route_order', Ngw, 'integer',
     +     Gwr_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'gwres_area', Ngw, 'real', Gwres_area)
     +     .NE.0 ) RETURN

      IF ( getparam('cascade', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN

      IF ( Prt_debug.EQ.13 ) OPEN (MSGUNT, FILE='cascade_prms.msgs')
      cascinit = 0
      IF ( Ncascade.GT.0 ) THEN
        Casc_typ = 1
        CALL init_cascade(cascinit)
        IF ( cascinit.NE.0 ) RETURN
      ENDIF
      IF ( Ncascdgw.GT.0 ) THEN
        Casc_typ = 2
        CALL initgw_cascade(cascinit)
        IF ( cascinit.NE.0 ) RETURN
      ENDIF

      IF ( Prt_debug.EQ.13 ) THEN
        WRITE (MSGUNT, 9001) 
        k = 0
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          DO j = 1, Ncascade_hru(i)
            k = k + 1
            WRITE (MSGUNT,*) k, i, Hru_down(j, i),
     +                       Hru_down_pct(j, i)*100.0
          ENDDO
        ENDDO
        WRITE (MSGUNT, 9002) 
        k = 0
        DO ii = 1, Active_gwrs
          i = Gwr_route_order(ii)
          DO j = 1, Ncascade_gwr(i)
            k = k + 1
            WRITE (MSGUNT,*) k, i, Gwr_down(j, i),
     +                       Gwr_down_pct(j, i)*100.0
          ENDDO
        ENDDO
        CLOSE (MSGUNT)
      ENDIF


 9001 FORMAT (//, 18X, 'UP HRU', 4X, 'DOWN HRU    PERCENT')
 9002 FORMAT (//, 18X, 'UP GWR', 4X, 'DOWN GWR    PERCENT')

      END FUNCTION cascinit

!***********************************************************************
! Initialize cascading flow variables
!***********************************************************************
      SUBROUTINE init_cascade(Iret)
      USE PRMS_CASCADE
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      EXTERNAL order_hrus
      INTRINSIC ABS
! Arguments
      INTEGER, INTENT(OUT) :: Iret
! Local Variables
      INTEGER :: i, j, k, ii, kk, dnhru, kup, jdn, istrm
      REAL, ALLOCATABLE :: hru_pct(:)
      REAL :: pctchk, carea, pct
!***********************************************************************
      Iret = 1

! Cascade parameters
      IF ( getparam('cascade', 'hru_up_id', Ncascade, 'integer',
     +     Hru_up_id).NE.0 ) RETURN

      IF ( getparam('cascade', 'hru_strmseg_down_id', Ncascade,
     +     'integer', Hru_strmseg_down_id).NE.0 ) RETURN

      IF ( getparam('cascade', 'hru_down_id', Ncascade, 'integer',
     +     Hru_down_id).NE.0 ) RETURN

      IF ( getparam('cascade', 'hru_pct_up', Ncascade, 'real',
     +     Hru_pct_up).NE.0 ) RETURN

      IF ( getparam('cascade', 'cascade_tol', 1, 'real', Cascade_tol)
     +     .NE.0 ) RETURN

      IF ( getparam('cascade', 'cascade_flg', 1, 'integer', Cascade_flg)
     +     .NE.0 ) RETURN

      Iret = 0

      Ncascade_hru = 0
      Ndown = 1
      DO i = 1, Ncascade
        k = Hru_up_id(i)
        IF ( k.GT.0 ) THEN
          Ncascade_hru(k) = Ncascade_hru(k) + 1
          IF ( Ncascade_hru(k).GT.Ndown ) Ndown = Ncascade_hru(k)
        ENDIF
      ENDDO
!     print *, 'ndown', ndown
 
      IF ( Ncascdgw.GT.0 ) THEN
        IF ( getparam('cascade', 'gw_up_id', Ncascdgw, 'integer',
     +       Gw_up_id).NE.0 ) RETURN
        IF ( getparam('cascade', 'gw_down_id', Ncascdgw, 'integer',
     +       Gw_down_id).NE.0 ) RETURN
        IF ( getparam('cascade', 'gw_pct_up', Ncascdgw, 'real',
     +       Gw_pct_up).NE.0 ) RETURN
        Ncascade_gwr = 0
        DO i = 1, Ncascdgw
          k = Gw_up_id(i)
          IF ( k.GT.0 ) THEN
            Ncascade_gwr(k) = Ncascade_gwr(k) + 1
            IF ( Ncascade_gwr(k).GT.Ndown ) Ndown = Ncascade_gwr(k)
          ENDIF
        ENDDO
      ENDIF

! allocate HRU variables
      ALLOCATE (Hru_down(Ndown, Nhru))
      ALLOCATE (Hru_down_pct(Ndown, Nhru))
      ALLOCATE (Hru_down_pctwt(Ndown, Nhru))
      ALLOCATE (Cascade_area(Ndown, Nhru))
      Hru_down = 0
      Hru_down_pct = 0.0
      Hru_down_pctwt = 0.0
      Cascade_area = 0.0

      ALLOCATE (hru_pct(Nhru))
      hru_pct = 0.0
      Hru_to_stream = 0
      Ncascade_hru = 0
      Outflow_flg = 0

      DO i = 1, Ncascade
        kup = Hru_up_id(i)
        jdn = Hru_down_id(i)
        pct = Hru_pct_up(i)
        istrm = Hru_strmseg_down_id(i)

        IF ( kup.LT.1 ) THEN
          PRINT 9004, 'ignoring cascade because up HRU<1', i, kup, jdn,
     +                pct, istrm
        ELSEIF ( Hru_type(kup).EQ.0 ) THEN
          PRINT 9004, 'ignoring cascade as up HRU is inactive', i, kup,
     +                jdn, pct, istrm
        ELSEIF ( jdn.LT.0 .AND. istrm.LT.1 ) THEN
          IF ( jdn.NE.-999 ) PRINT 9004,
     +         'ignoring cascade as down HRU<0 & down segment<1', i,
     +         kup, jdn, pct, istrm
        ELSEIF ( Hru_type(kup).EQ.2 .AND. istrm.LT.1 .AND.
     +           pct.GT.0.0 ) THEN
          PRINT 9004, 'ignoring cascade because lake HRU cannot'//
     +                ' cascade to an HRU', i, kup, jdn, pct, istrm
        ELSE
          IF ( jdn.GT.0 .AND. istrm.LT.1 ) THEN
            IF ( Hru_type(jdn).EQ.0 ) THEN
              PRINT 9004, 'ignoring cascade as down HRU is inactive', i,
     +                    kup, jdn, pct, istrm
              CYCLE
            ENDIF
          ENDIF

          IF ( jdn.LT.0 ) THEN
            PRINT 9004, 'ignoring down HRU<0, set down HRU=0', i, kup,
     +                  jdn, pct, istrm
            jdn = 0
          ENDIF
          carea = pct*Hru_area(kup)
          ! get rid of small cascades, redistribute percentages
          IF ( carea.LT.Cascade_tol ) THEN
            IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9005) i, kup, jdn,
     +                                                  pct*100.0, carea
          ELSEIF ( Cascade_flg.EQ.1 ) THEN
            IF ( pct.GT.hru_pct(kup) ) THEN
              hru_pct(kup) = pct
              Ncascade_hru(kup) = 1
              Hru_down_pct(1, kup) = pct
              IF ( istrm.GT.0 ) THEN
                Hru_down(1, kup) = -istrm
              ELSE
                Hru_down(1, kup) = jdn
              ENDIF
            ENDIF
          ELSE
            hru_pct(kup) = hru_pct(kup) + pct
            Ncascade_hru(kup) = Ncascade_hru(kup) + 1
            kk = Ncascade_hru(kup)
            Hru_down_pct(kk, kup) = pct
            IF ( istrm.GT.0 ) THEN
              Hru_down(kk, kup) = -istrm
            ELSE
              Hru_down(kk, kup) = jdn
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      ! how do we route headwater HRUs to stream segment rather than
      ! across valleys**********************RSR???

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Ncascade_hru(i).GT.0 ) THEN
          pctchk = 0.0
          j = 1
          k = 1
!!DANGER FIX!! hru_down_id = 0, apportion to other cascades
          DO WHILE ( j.LE.Ncascade_hru(i) )
            pct = Hru_down_pct(k, i)
            Hru_down_pct(j, i) = pct + pct*(1.0-hru_pct(i))/hru_pct(i)
            pctchk = pctchk + Hru_down_pct(j, i)
            dnhru = Hru_down(k, i)
            DO kk = 1, j - 1
              IF ( dnhru.EQ.Hru_down(kk, i) ) THEN
                Hru_down_pct(kk, i) = Hru_down_pct(kk, i) +
     +                                Hru_down_pct(k, i)
                j = j - 1
                Ncascade_hru(i) = Ncascade_hru(i) - 1
                IF ( Prt_debug.EQ.13 ) THEN
                  IF ( dnhru.LT.0 ) THEN
! two cascades to same stream segment, combine
                    WRITE (MSGUNT, 9002) i, 'stream segment', ABS(dnhru)
                  ELSE
! two cascades to same HRU, combine
                    WRITE (MSGUNT, 9002) i, 'downslope HRU', dnhru
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            Cascade_area(j, i) = Hru_down_pct(j, i)*Hru_area(i)
            IF ( dnhru.LT.0 ) THEN
              Hru_to_stream(i) = 1
            ELSEIF ( dnhru.EQ.0 ) THEN
              Hru_to_stream(i) = 1
              Hru_down(j, i) = -Nsegmentp1
              Outflow_flg = 1
            ELSE
              Hru_down_pctwt(j, i) = Cascade_area(j, i)/Hru_area(dnhru)
            ENDIF
            IF ( j.LT.k ) Hru_down(j, i) = dnhru
            j = j + 1
            k = k + 1
          ENDDO
          IF ( ABS(pctchk-1.0).GT.5.0E-7 ) THEN
            PRINT *, 'Error, sum of contributing area for HRU:', i,
     +            ' accounts for different than 100% of HRU area:',
     +            pctchk
            Iret = 1
          ENDIF
        ENDIF
      ENDDO
      DEALLOCATE (hru_pct)

      CALL order_hrus(Iret)

      IF ( Prt_debug.EQ.13 ) THEN
        WRITE (MSGUNT, 9001)
        WRITE (MSGUNT, 9003) (Hru_route_order(i), i=1, Iorder)
      ENDIF

      IF ( putvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN
      IF ( putvar('basin', 'ncascade_hru', Nhru, 'integer',
     +     Ncascade_hru).NE.0 ) RETURN

 9001 FORMAT (/, 'HRU routing order:')
 9002 FORMAT ('*** WARNING, combined multiple cascade paths from HRU:',
     +        I5, ' to ', A, ':', I5)
 9003 FORMAT (12I6)
 9004 FORMAT ('*** WARNING, ', A, /, '    Cascade:', I5, '; up HRU:',
     +        I5, '; down HRU:', I5, '; up PCT:', F8.4,
     +        '; stream segment:', I5)
 9005 FORMAT ('*** WARNING, ignoring small cascade, carea<cascade_tol',
     +        /, '    Cascade:', I5, '; HRU up:', I5, '; HRU down:', I5,
     +        '; PCT up:', F8.2, '; cascade area:', F8.2)

      END SUBROUTINE init_cascade

!***********************************************************************
! order hrus allowing many to 1
!***********************************************************************
      SUBROUTINE order_hrus(Iret)
      USE PRMS_CASCADE, ONLY:Nhru, Prt_debug, Ncascade_hru, Hru_down,
     +    Hru_route_order, Casc_typ, Hrus_in, Iorder, Hru_type,
     +    Active_hrus, MSGUNT, Order_flg
      IMPLICIT NONE
      EXTERNAL up_tree
!     Arguments
      INTEGER, INTENT(OUT) :: Iret
!     Local Variables
      INTEGER, ALLOCATABLE :: hru_dwn(:), hru_up(:), roots(:), nxtgr2(:)
      INTEGER, ALLOCATABLE :: hrus_up_list(:, :), path(:), nxtgrp(:)
      INTEGER :: i, j, k, ii, nroots, circle_flg, ihru, npath, imx
      INTEGER :: nheads, irtrn
!-----------------------------------------------------------------------
      ALLOCATE (hru_dwn(Nhru), hru_up(Nhru), roots(Nhru), path(Nhru))
      ALLOCATE (nxtgrp(Nhru), nxtgr2(Nhru))
!     hru_dwn equals number of times an hru is a downslope hru
!     hru_up equals number of times an hru is an upslope hru
      imx = 0
      hru_dwn = 0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        DO k = 1, Ncascade_hru(i)
          j = Hru_down(k, i)
          IF ( j.GT.0 ) THEN
            hru_dwn(j) = hru_dwn(j) + 1
            IF ( hru_dwn(j)>imx ) imx = hru_dwn(j)
          ENDIF
        ENDDO
      ENDDO
!     print *, 'cascade max up', imx
      ALLOCATE (hrus_up_list(imx, Nhru))

      hru_dwn = 0
      hru_up = 0
      hrus_up_list = 0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        DO k = 1, Ncascade_hru(i)
          j = Hru_down(k, i)
          IF ( j.GT.0 ) THEN
            hru_dwn(j) = hru_dwn(j) + 1
            hru_up(i) = hru_up(i) + 1
            hrus_up_list(hru_dwn(j), j) = i
          ENDIF
        ENDDO
      ENDDO

      Hru_route_order = 0
      Iorder = 0
      nroots = 0
      Hrus_in = 0
      Iret = 0
      DO i = 1, Nhru
        IF ( Hru_type(i).EQ.0 ) CYCLE
!       add to order hru's that do not recieve cascading flow
        IF ( hru_dwn(i).EQ.0 ) THEN
          Iorder = Iorder + 1
          Hru_route_order(Iorder) = i
          Hrus_in(i) = 1
          IF ( Hru_type(i)==1 .AND. Ncascade_hru(i)==0 ) THEN
!           Iret = 1
            PRINT *, 'Error, head HRU does not cascade:', i
            IF ( Prt_debug==13 ) WRITE (MSGUNT, *)
     +           'Error, head HRU does not cascade:', i
          ENDIF
        ELSEIF ( hru_up(i).EQ.0 ) THEN
          nroots = nroots + 1
          roots(nroots) = i
          IF ( Hru_type(i)==1 .AND. Ncascade_hru(i)==0 ) THEN
!           Iret = 1
            PRINT *, 'Warning, root HRU does not cascade:', i
            IF ( Prt_debug==13 ) WRITE (MSGUNT, *)
     +           'Warning, root HRU does not cascade:', i
          ENDIF
        ENDIF
      ENDDO

      IF ( Prt_debug.EQ.13 ) THEN
        WRITE (MSGUNT, 9001) Iorder
        WRITE (MSGUNT, 9002) (Hru_route_order(i), i=1, Iorder)
        WRITE (MSGUNT, 9003) nroots
        WRITE (MSGUNT, 9002) (roots(i), i=1, nroots)
      ENDIF

!     IF ( Iret==1 ) RETURN

      IF ( Order_flg.EQ.1 ) THEN
        DO i = 1, nroots
          ihru = roots(i)
          path(1) = ihru
          npath = 1
          circle_flg = 0
          CALL up_tree(Nhru, ihru, hru_dwn, hrus_up_list, npath, path,
     +               Iorder, circle_flg, Casc_typ, imx)
          IF ( circle_flg.EQ.1 ) Iret = 1
        ENDDO
      ELSE
        nheads = Iorder
        ii = 0
        nxtgrp = 0
        DO i = 1, nheads
          ihru = Hru_route_order(i)
          DO j = 1, Ncascade_hru(ihru)
            k = Hru_down(j, ihru)
            IF ( k>0 ) THEN
              CALL in_hru_order(k, Iorder, irtrn)
              IF ( irtrn==0 ) THEN
                ii = ii + 1
                nxtgrp(ii) = k
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        DO WHILE ( ii>0 )
          nxtgr2 = 0
          nheads = ii
          ii = 0
          DO i = 1, nheads
            ihru = nxtgrp(i)
            DO j = 1, Ncascade_hru(ihru)
              k = Hru_down(j, ihru)
              IF ( k>0 ) THEN
                CALL in_hru_order(k, Iorder, irtrn)
                IF ( irtrn==0 ) THEN
                  ii = ii + 1
                  nxtgr2(ii) = k
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          nxtgrp = nxtgr2
        ENDDO
!       write(444,*) 'Iorder', Iorder, ii
!       write(444,*) 'nheads', nheads
!       write(444,'(20I7)') (Hru_route_order(i),i=1,Iorder)
      ENDIF

      IF ( Iorder.NE.Active_hrus ) THEN
        PRINT 9004, Iorder, Nhru, Active_hrus
        IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9004) Iorder, Nhru,
     +                                              Active_hrus
        DO i = 1, Nhru
          IF ( Hrus_in(i).EQ.0 ) THEN
            IF ( Hru_type(i).NE.0 ) THEN
              PRINT 9006, i
              IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9006) i
              Iret = 1
            ELSE
!             PRINT 9007, i
              IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9007) i
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      IF ( circle_flg.EQ.1 ) THEN
        PRINT 9005
        IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9005)
        Iret = 1
      ENDIF

      DEALLOCATE (hru_dwn, hru_up, hrus_up_list, path, nxtgrp, nxtgr2)

 9001 FORMAT (/, I5, ' HRUs that do not receive upslope flow (heads)')
 9002 FORMAT (12I6)
 9003 FORMAT (/, I5, ' HRUs that do not cascade to another HRU (roots)')
 9004 FORMAT (/, 'Warning, not all HRUs are included in the cascading',
     +        ' pattern, likely circle or inactive HRUs', //,
     +        'Number of HRUs in pattern:' I5, ', number of HRUs:', I5,
     +        ', Active HRUs:', I5, //, 'HRUs not in routing order:')
 9005 FORMAT (/, 'Error, circular HRU path found', /)
 9006 FORMAT (I5, ' missing')
 9007 FORMAT (I5, ' inactive')

      END SUBROUTINE order_hrus

!***********************************************************************
! Initialize cascading flow variables
!***********************************************************************
      SUBROUTINE initgw_cascade(Iret)
      USE PRMS_CASCADE
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      EXTERNAL order_gwrs
      INTRINSIC ABS
! Arguments
      INTEGER, INTENT(OUT) :: Iret
! Local Variables
      INTEGER :: i, j, k, ii, kk, dngwr, kup, jdn, istrm
      REAL :: pctchk, carea, pct
      REAL, ALLOCATABLE :: gwr_pct(:)
!***********************************************************************
      Iret = 1

! Cascade parameters
      IF ( getparam('cascade', 'gw_strmseg_down_id', Ncascdgw,
     +     'integer', Gw_strmseg_down_id).NE.0 ) RETURN

      IF ( getparam('cascade', 'cascade_tol', 1, 'real', Cascade_tol)
     +     .NE.0 ) RETURN

      IF ( getparam('cascade', 'cascade_flg', 1, 'integer', Cascade_flg)
     +     .NE.0 ) RETURN

      Iret = 0

! allocate GWR variables
      ALLOCATE (Gwr_down(Ndown, Ngw))
      ALLOCATE (Gwr_down_pct(Ndown, Ngw))
      Gwr_down = 0
      Gwr_down_pct = 0.0

      ALLOCATE (gwr_pct(Ngw))
      gwr_pct = 0.0
      Ncascade_gwr = 0

      DO i = 1, Ncascdgw
        kup = Gw_up_id(i)
        jdn = Gw_down_id(i)
        pct = Gw_pct_up(i)
        istrm = Gw_strmseg_down_id(i)

        IF ( kup.LT.1 ) THEN
          PRINT 9004, 'ignoring cascade because up GWR<1', i, kup, jdn,
     +                pct, istrm
        ELSEIF ( Hru_type(kup).EQ.0 ) THEN
          PRINT 9004, 'ignoring cascade as up GWR is inactive', i, kup,
     +                jdn, pct, istrm
        ELSEIF ( jdn.LT.0 .AND. istrm.LT.1 ) THEN
          IF ( jdn.NE.-999 ) PRINT 9004,
     +         'ignoring cascade as down GWR<0 & down segment<1', i,
     +         kup, jdn, pct, istrm
        ELSE
          IF ( jdn.GT.0 .AND. istrm.LT.1 ) THEN
            IF ( Hru_type(jdn).EQ.0 ) THEN
              PRINT 9004, 'ignoring cascade as down GWR is inactive', i,
     +                    kup, jdn, pct, istrm
              CYCLE
            ENDIF
          ENDIF

          IF ( jdn.LT.0 ) THEN
            PRINT 9004, 'ignoring down GWR<0, set down GWR=0', i, kup,
     +                  jdn, pct, istrm
            jdn = 0
          ENDIF         

          carea = Gw_pct_up(i)*Gwres_area(kup)
          ! get rid of small cascades, redistribute percentages
          IF ( carea.LT.Cascade_tol ) THEN
            IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9005) i, kup, jdn,
     +                                                  pct*100.0, carea
          ELSEIF ( Cascade_flg.EQ.1 ) THEN
            IF ( pct.GT.gwr_pct(kup) ) THEN
              gwr_pct(kup) = pct
              Ncascade_gwr(kup) = 1
              Gwr_down_pct(1, kup) = pct
              IF ( istrm.GT.0 ) THEN
                Gwr_down(1, kup) = -istrm
              ELSE
                Gwr_down(1, kup) = jdn
              ENDIF
            ENDIF
          ELSE
            gwr_pct(kup) = gwr_pct(kup) + pct
            Ncascade_gwr(kup) = Ncascade_gwr(kup) + 1
            kk = Ncascade_gwr(kup)
            Gwr_down_pct(kk, kup) = pct
            IF ( istrm.GT.0 ) THEN
              Gwr_down(kk, kup) = -istrm
            ELSE
              Gwr_down(kk, kup) = jdn
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      DO ii = 1, Active_gwrs
        i = Gwr_route_order(ii)
        IF ( Ncascade_gwr(i).GT.0 ) THEN
          pctchk = 0.0
          j = 1
          k = 1
          DO WHILE ( j.LE.Ncascade_gwr(i) )
            pct = Gwr_down_pct(k, i)
            Gwr_down_pct(j, i) = pct + pct*(1.0-gwr_pct(i))/gwr_pct(i)
            pctchk = pctchk + Gwr_down_pct(j, i)
            dngwr = Gwr_down(k, i)
            DO kk = 1, j - 1
              IF ( dngwr.EQ.Gwr_down(kk, i) ) THEN
                Gwr_down_pct(kk, i) = Gwr_down_pct(kk, i) +
     +                                Gwr_down_pct(k, i)
                j = j - 1
                Ncascade_gwr(i) = Ncascade_gwr(i) - 1
                IF ( Prt_debug.EQ.13 ) THEN
                  IF ( dngwr.LT.0 ) THEN
! two cascades to same stream segment, combine
                    WRITE (MSGUNT, 9002) i, 'stream segment', ABS(dngwr)
                  ELSE
                    WRITE (MSGUNT, 9002) i, 'downslope GWR', dngwr
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            IF ( j.LT.k ) Gwr_down(j, i) = dngwr
            j = j + 1
            k = k + 1
          ENDDO
          IF ( ABS(pctchk-1.0).GT.5.0E-7 ) THEN
            PRINT *, 'Error, sum of contributing area for GWR:', i,
     +            ' accounts for different than 100% of GWR area:',
     +            pctchk
            Iret = 1
          ENDIF
        ENDIF
      ENDDO
      DEALLOCATE (gwr_pct)

      CALL order_gwrs(Iret)
      
      IF ( Prt_debug.EQ.13 ) THEN
        WRITE (MSGUNT, 9001)
        WRITE (MSGUNT, 9003) (Gwr_route_order(i), i=1, Igworder)
      ENDIF

      IF ( putvar('basin', 'gwr_route_order', Ngw, 'integer',
     +     Gwr_route_order).NE.0 ) RETURN
      IF ( putvar('basin', 'ncascade_gwr', Ngw, 'integer', Ncascade_gwr)
     +     .NE.0 ) RETURN

 9001 FORMAT (/, 'GWR routing order:')
 9002 FORMAT ('Warning, combined multiple cascade paths from GWR', I5,
     +        ' to ', A, ':', I5)
 9003 FORMAT (12I6)
 9004 FORMAT ('*** WARNING, ', A, /, '    Cascade:', I5, '; up GWR:',
     +        I5, '; down GWR:', I5, '; up PCT:', F8.4,
     +        '; stream segment:', I5)
 9005 FORMAT ('*** WARNING, ignoring small cascade, carea<cascade_tol',
     +        /, '    Cascade:', I5, '; GWR up:', I5, '; GWR down:', I5,
     +        '; PCT up:', F8.2, '; cascade area:', F8.2)

      END SUBROUTINE initgw_cascade

!***********************************************************************
! order GWRs allowing many to 1
!***********************************************************************
      SUBROUTINE order_gwrs(Iret)
      USE PRMS_CASCADE, ONLY:Ncascade_gwr, Ngw, Gwr_down, Prt_debug,
     +    Gwr_route_order, Casc_typ, Gwrs_in, Igworder, Hru_type,
     +    Active_gwrs, MSGUNT
      IMPLICIT NONE
      EXTERNAL up_tree
!     Arguments
      INTEGER, INTENT(OUT) :: Iret
!     Local Variables
      INTEGER, ALLOCATABLE :: gwr_dwn(:), gwr_up(:), roots(:)
      INTEGER, ALLOCATABLE :: gwr_up_list(:, :), path(:)
      INTEGER :: i, j, k, ii, nroots, circle_flg, igwr, npath, imx
!-----------------------------------------------------------------------
      ALLOCATE (gwr_dwn(Ngw), gwr_up(Ngw), roots(Ngw), path(Ngw))
!     gwr_dwn equals number of times an GWR is a downslope GWR
!     gwr_up equals number of times an GWR is an upslope GWR
      imx = 0
      gwr_dwn = 0
      DO ii = 1, Active_gwrs
        i = Gwr_route_order(ii)
        DO k = 1, Ncascade_gwr(i)
          j = Gwr_down(k, i)
          IF ( j.GT.0 ) THEN
            gwr_dwn(j) = gwr_dwn(j) + 1
            IF ( gwr_dwn(j)>imx ) imx = gwr_dwn(j)
          ENDIF
        ENDDO
      ENDDO
      ALLOCATE (gwr_up_list(imx, Ngw))

      gwr_dwn = 0
      gwr_up = 0
      gwr_up_list = 0
      DO ii = 1, Active_gwrs
        i = Gwr_route_order(ii)
        DO k = 1, Ncascade_gwr(i)
          j = Gwr_down(k, i)
          IF ( j.GT.0 ) THEN
            gwr_dwn(j) = gwr_dwn(j) + 1
            gwr_up(i) = gwr_up(i) + 1
            gwr_up_list(gwr_dwn(j), j) = i
          ENDIF
        ENDDO
      ENDDO

      Gwr_route_order = 0
      Igworder = 0
      nroots = 0
      Gwrs_in = 0
      DO i = 1, Ngw
        IF ( Hru_type(i).EQ.0 ) CYCLE
!       add to order GWR's that do not recieve
        IF ( gwr_dwn(i).EQ.0 ) THEN
          Igworder = Igworder + 1
          Gwr_route_order(Igworder) = i
          Gwrs_in(i) = 1
        ELSEIF ( gwr_up(i).EQ.0 ) THEN
          nroots = nroots + 1
          roots(nroots) = i
        ENDIF
      ENDDO

      IF ( Prt_debug.EQ.13 ) THEN
        WRITE (MSGUNT, 9001) Igworder
        WRITE (MSGUNT, 9002) (Gwr_route_order(i), i=1, Igworder)
        WRITE (MSGUNT, 9003) nroots
        WRITE (MSGUNT, 9002) (roots(i), i=1, nroots)
      ENDIF
      PRINT *

      DO i = 1, nroots
        igwr = roots(i)
        path(1) = igwr
        npath = 1
        circle_flg = 0
        CALL up_tree(Ngw, igwr, gwr_dwn, gwr_up_list, npath, path,
     +               Igworder, circle_flg, Casc_typ, imx)
        IF ( circle_flg.EQ.1 ) Iret = 1
      ENDDO

      IF ( Igworder.NE.Active_gwrs ) THEN
        PRINT 9004, Igworder, Ngw
        IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9004) Igworder, Ngw
        DO i = 1, Ngw
          IF ( Gwrs_in(i).EQ.0 ) THEN
            IF ( Hru_type(i).NE.0 ) THEN
              PRINT 9006,  i
              IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9006) i
              Iret = 1
            ELSE
              PRINT 9007,  i
              IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9007) i
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      IF ( circle_flg.EQ.1 ) THEN
        PRINT 9005
        IF ( Prt_debug.EQ.13 ) WRITE (MSGUNT, 9005)
        Iret = 1
      ENDIF

      DEALLOCATE (gwr_dwn, gwr_up, gwr_up_list, path)

 9001 FORMAT (/, I5, ' GWRs that do not receive upslope flow (heads)')
 9002 FORMAT (12I6)
 9003 FORMAT (/, I5, ' GWRs that do not cascade to another GWR (roots)')
 9004 FORMAT (/, 'Warning, not all GWRs are included in the cascading',
     +        ' pattern, likely circle or inactive HRUs', //,
     +        'Number of GWRs in pattern:' I5, ', number of GWRs:', I5,
     +        /, 'GWRs not in routing order:')
 9005 FORMAT (/, 'Error, circular GWR path found', /)
 9006 FORMAT (I5, ' missing')
 9007 FORMAT (I5, ' inactive')

      END SUBROUTINE order_gwrs

!***********************************************************************
! Recursively walk up a tree of cascading spatial units
!***********************************************************************
      RECURSIVE SUBROUTINE up_tree(Num, N, Down_id, Up_list, Npath,
     +                             Path, Iorder, Circle_flg, Casc_typ,
     +                             Imx)
      IMPLICIT NONE
      EXTERNAL check_path, in_hru_order, in_gwr_order
! Arguments
      INTEGER, INTENT(IN) :: Num, N, Casc_typ, Imx
      INTEGER, INTENT(IN) :: Down_id(Num), Up_list(Imx, Num)
      INTEGER, INTENT(INOUT) :: Npath, Path(Num), Iorder, Circle_flg
! Local Variables
      INTEGER :: nup, i, parent, iret
!-----------------------------------------------------------------------
      IF ( Circle_flg.EQ.1 ) RETURN
      nup = Down_id(N)
      DO i = 1, nup
        Npath = Npath + 1
        parent = Up_list(i, N)
        Path(Npath) = parent
        CALL check_path(Npath, Path, Circle_flg, nup)
        CALL up_tree(Num, parent, Down_id, Up_list, Npath, Path, Iorder,
     +               Circle_flg, Casc_typ, Imx)
      ENDDO

      IF ( nup.EQ.0 ) CALL check_path(Npath, Path, Circle_flg, nup)
      IF ( Casc_typ.EQ.1 ) THEN
        CALL in_hru_order(N, Iorder, iret)
      ELSE
        CALL in_gwr_order(N, Iorder)
      ENDIF
      Npath = Npath - 1

      END SUBROUTINE up_tree

!***********************************************************************
! check for circular path
!***********************************************************************
      SUBROUTINE check_path(Npath, Path, Circle_flg, Nup)
      USE PRMS_CASCADE, ONLY:Prt_debug, MSGUNT
      IMPLICIT NONE
      INTRINSIC MIN
!     Arguments
      INTEGER, INTENT(IN) :: Npath, Path(Npath), Nup
      INTEGER, INTENT(OUT) :: Circle_flg
!     Local Variables
      INTEGER :: j, i, n
!-----------------------------------------------------------------------
      Circle_flg = 0
      DO j = 1, Npath - 1
        DO i = j + 1, Npath
          IF ( Path(j).EQ.Path(i) ) THEN
            PRINT *, 'Error, circular cascading path specified'
            PRINT *, Path
            Circle_flg = 1
          ENDIF
        ENDDO
      ENDDO

      IF ( Prt_debug.EQ.1 3.AND. (Circle_flg.EQ.1 .OR. Nup.EQ.0) ) THEN
        WRITE (MSGUNT, *) 'Cascade Path with', Npath, ' links'
        DO i = 1, Npath, 10
          n = MIN(Npath, i+9)
          WRITE (MSGUNT, 9001) (Path(j), j = i, n)
        ENDDO
      ENDIF
 9001 FORMAT (10I8)

      END SUBROUTINE check_path

!***********************************************************************
! check HRU order list
!***********************************************************************
      SUBROUTINE in_hru_order(Ihru, Iorder, Iret)
      USE PRMS_CASCADE, ONLY:Hru_route_order, Hrus_in
      IMPLICIT NONE
!     Arguments
      INTEGER, INTENT(IN) :: Ihru
      INTEGER, INTENT(INOUT) :: Iorder
      INTEGER, INTENT(OUT) :: Iret
!     Local Variables
      INTEGER :: j
!-----------------------------------------------------------------------
      Iret = 1
      DO j = 1, Iorder
        IF ( Hru_route_order(j).EQ.Ihru ) RETURN
      ENDDO
      Iorder = Iorder + 1
      Hru_route_order(Iorder) = Ihru
      Hrus_in(Ihru) = Ihru
      Iret = 0

      END SUBROUTINE in_hru_order

!***********************************************************************
! check GWR order list
!***********************************************************************
      SUBROUTINE in_gwr_order(Igwr, Iorder)
      USE PRMS_CASCADE, ONLY:Gwr_route_order, Gwrs_in
      IMPLICIT NONE
!     Arguments
      INTEGER, INTENT(IN) :: Igwr
      INTEGER, INTENT(INOUT) :: Iorder
!     Local Variables
      INTEGER :: j
!-----------------------------------------------------------------------
      DO j = 1, Iorder
        IF ( Gwr_route_order(j).EQ.Igwr ) RETURN
      ENDDO
      Iorder = Iorder + 1
      Gwr_route_order(Iorder) = Igwr
      Gwrs_in(Igwr) = 1

      END SUBROUTINE in_gwr_order

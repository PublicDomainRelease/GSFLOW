!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
      MODULE PRMS_TRANSP_TINDEX
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: Span_year(:)
      INTEGER, SAVE, ALLOCATABLE :: Transp_check(:), Transp_end_12(:)
      REAL, SAVE :: Freeze_temp
      REAL, SAVE, ALLOCATABLE :: Tmax_sum(:), Tmax_hru(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Transp_beg(:), Transp_end(:)
      REAL, SAVE, ALLOCATABLE :: Transp_tmax(:)
      END MODULE PRMS_TRANSP_TINDEX

!***********************************************************************
!     Main transp_tindex_prms routine
!***********************************************************************
      INTEGER FUNCTION transp_tindex_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decl_tindex, init_tindex, run_tindex
!***********************************************************************
      transp_tindex_prms = 0

      IF ( Process_flag==0 ) THEN
        transp_tindex_prms = run_tindex()
      ELSEIF ( Process_flag==1 ) THEN
        transp_tindex_prms = decl_tindex()
      ELSEIF ( Process_flag==2 ) THEN
        transp_tindex_prms = init_tindex()
      ENDIF

      END FUNCTION transp_tindex_prms

!***********************************************************************
!     decl_tindex - set up parameters for transpiration period determination
!***********************************************************************
      INTEGER FUNCTION decl_tindex()
      USE PRMS_TRANSP_TINDEX
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      decl_tindex = 1

      IF ( declmodule(
     +'$Id: transp_tindex_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

      ALLOCATE (Transp_beg(Nhru))
      IF ( declparam('tindex', 'transp_beg', 'nhru', 'integer',
     +     '4', '1', '12',
     +     'Month to begin testing for transpiration',
     +     'Month to begin summing tmaxf for each HRU; when sum is'//
     +     ' >= to transp_tmax, transpiration begins',
     +     'month').NE.0 ) RETURN

      ALLOCATE (Transp_end(Nhru))
      IF ( declparam('tindex', 'transp_end', 'nhru', 'integer',
     +     '10', '1', '12',
     +     'Month to stop transpiration period',
     +     'Month to stop transpiration computations;'//
     +     ' transpiration is computed thru end of previous month',
     +     'month').NE.0 ) RETURN

      ALLOCATE (Transp_tmax(Nhru))
      IF ( declparam('tindex', 'transp_tmax', 'nhru', 'real',
     +     '500.', '0.', '1000.',
     +     'Tmax index to determine start of transpiration',
     +     'Temperature index to determine the specific date of the'//
     +     ' start of the transpiration period.  Subroutine sums tmax'//
     +     ' for each HRU starting with the first day of month'//
     +     ' transp_beg.  When the sum exceeds this index,'//
     +     ' transpiration begins',
     +     'degrees').NE.0 ) RETURN

      decl_tindex = 0
      END FUNCTION decl_tindex

!***********************************************************************
!     init_transp - Initialize transp_tindex module - get parameter values,
!                   set initial transp_on switch
!***********************************************************************
      INTEGER FUNCTION init_tindex()
      USE PRMS_TRANSP_TINDEX
      USE PRMS_BASIN, ONLY: Nhru, Starttime
      USE PRMS_CLIMATEVARS, ONLY: Temp_units, Transp_on, Basin_transp_on
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: mo, day, i, motmp
!***********************************************************************
      init_tindex = 1

      ALLOCATE ( Tmax_sum(Nhru), Tmax_hru(Nhru), Transp_check(Nhru) )
      ALLOCATE ( Transp_end_12(Nhru), Span_year(Nhru) )
      Tmax_sum = 0.0
      Span_year = 0

      IF ( getparam('tindex', 'transp_beg', Nhru, 'integer', Transp_beg)
     +     .NE.0 ) RETURN

      IF ( getparam('tindex', 'transp_end', Nhru, 'integer', Transp_end)
     +     .NE.0 ) RETURN

      IF ( getparam('tindex', 'transp_tmax', Nhru, 'real', Transp_tmax)
     +     .NE.0 ) RETURN

      IF ( Temp_units==0 ) THEN
        Freeze_temp = 32.0
      ELSE
        Freeze_temp = 0.0
      ENDIF

      mo = Starttime(2)
      day = Starttime(3)
      motmp = mo + 12
      DO i = 1, Nhru
        Transp_check(i) = 0
        IF ( mo.EQ.Transp_beg(i) ) THEN
          IF ( day.GT.10 ) THEN
            Transp_on(i) = 1
          ELSE
            Transp_check(i) = 1
          ENDIF

        ELSEIF ( (Transp_end(i)-Transp_beg(i)).GT.0 ) THEN
          IF ( mo.GT.Transp_beg(i) .AND. mo.LT.Transp_end(i) )
     +      Transp_on(i) = 1

        ELSE
          IF ( Transp_end(i)<Transp_beg(i) ) Span_year(i) = 1
          Transp_end_12(i) = Transp_end(i) + 12
          IF ( mo>Transp_beg(i) .OR. motmp<Transp_end_12(i) )
     +         Transp_on(i) = 1

        ENDIF
        IF ( Transp_on(i).EQ.1 ) Basin_transp_on = 1
      ENDDO

      init_tindex = 0
      END FUNCTION init_tindex

!***********************************************************************
!      run_tindex - Keeps track of transpiration on or off
!***********************************************************************
      INTEGER FUNCTION run_tindex()
      USE PRMS_TRANSP_TINDEX
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Temp_units, Tmaxf, Tmaxc, Transp_on,
     +    Basin_transp_on
      USE PRMS_OBS, ONLY: Nowmonth, Nowday
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, j, motmp
!***********************************************************************
!******Set switch for active transpiration period

      IF ( Temp_units==0 ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Tmax_hru(i) = Tmaxf(i)
        ENDDO
      ELSE
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Tmax_hru(i) = Tmaxc(i)
        ENDDO
      ENDIF

      Basin_transp_on = 0
      motmp = Nowmonth + 12
      DO j = 1, Active_hrus
        i = Hru_route_order(j)

!******If in checking period, then for each day
!******sum max temp until greater than temperature index parameter,
!******at which time, turn transpiration switch on, check switch off

        IF ( Transp_check(i).EQ.1 ) THEN
            IF ( Tmax_hru(i).GT.Freeze_temp )
     +           Tmax_sum(i) = Tmax_sum(i) + Tmax_hru(i)
          IF ( Tmax_sum(i).GT.Transp_tmax(i) ) THEN
            Transp_on(i) = 1
            Transp_check(i) = 0
            Tmax_sum(i) = 0.
          ENDIF

!******Otherwise, check for month to turn check switch on or
!******transpiration switch off

        ELSEIF ( Nowday.EQ.1 ) THEN
          IF ( Nowmonth.EQ.Transp_beg(i) ) THEN
            Transp_check(i) = 1
            IF ( Tmax_hru(i).GT.Freeze_temp )
     +           Tmax_sum(i) = Tmax_sum(i) + Tmax_hru(i)

!******If transpiration switch on, check for end of period

          ELSEIF ( Transp_on(i).EQ.1 ) THEN
            IF ( Span_year(i)==0 ) THEN
              IF ( Nowmonth.EQ.Transp_end(i) ) Transp_on(i) = 0
            ELSE
              IF ( motmp.EQ.Transp_end_12(i) ) Transp_on(i) = 0
            ENDIF
          ENDIF
        ENDIF
        IF ( Transp_on(i).EQ.1 ) Basin_transp_on = 1
      ENDDO

      run_tindex = 0
      END FUNCTION run_tindex

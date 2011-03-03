!***********************************************************************
! Checks for missing values and qualitiy control of measured data
!***********************************************************************
      MODULE PRMS_OBS_ADJUST
      IMPLICIT NONE
!   Local Variables
      REAL, SAVE, ALLOCATABLE :: Tmax_prev(:), Tmin_prev(:)
      INTEGER, SAVE, ALLOCATABLE :: Tmax_cnt(:), Tmin_cnt(:)
      INTEGER, SAVE, ALLOCATABLE :: Nuse_tsta(:)
!   Declared Parameters
      INTEGER, SAVE :: Max_missing
      INTEGER, SAVE, ALLOCATABLE :: Hru_tlaps(:)
      END MODULE PRMS_OBS_ADJUST
!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs_adjust_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsadjdecl, obsadjinit, obsadjrun
!***********************************************************************
      obs_adjust_prms = 0

      IF ( Process_flag==0 ) THEN
        obs_adjust_prms = obsadjrun()
      ELSEIF ( Process_flag==1 ) THEN
        obs_adjust_prms = obsadjdecl()
      ELSEIF ( Process_flag==2 ) THEN
        obs_adjust_prms = obsadjinit()
      ENDIF

      END FUNCTION obs_adjust_prms

!***********************************************************************
!     obsadjdecl - makes public declarations
!   Declared Parameters
!     tmax_allrain, hru_tsta
!***********************************************************************
      INTEGER FUNCTION obsadjdecl()
      USE PRMS_OBS_ADJUST
      USE PRMS_MODULE, ONLY: Temp_flag
      USE PRMS_BASIN, ONLY: Nhru
      USE PRMS_CLIMATEVARS, ONLY: Ntemp
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      obsadjdecl = 1

      IF ( declmodule(
     +'$Id: obs_adjust_prms.f 2182 2010-11-26 19:23:44Z rsregan $'
     +)/=0 ) RETURN

      ALLOCATE (Tmin_prev(Ntemp), Tmax_prev(Ntemp))
      ALLOCATE (Tmin_cnt(Ntemp), Tmax_cnt(Ntemp))
      ALLOCATE (Nuse_tsta(Ntemp))

!   Declared Parameters
      IF ( declparam('obsadj', 'max_missing', 'one', 'integer',
     +     '3', '0', '10',
     +     'Maximum number of consecutive missing values allowed for'//
     +     ' any measured air temperature station; 0 = unlimited',
     +     'Maximum number of consecutive missing values allowed for'//
     +     ' any measured air temperature station; missing value set'//
     +     ' to last valid value; 0 = unlimited',
     +     'none')/=0 ) RETURN

      IF ( Temp_flag==3 ) THEN
        ! using temp_laps_prms
        ALLOCATE (Hru_tlaps(Nhru))
        IF ( declparam('obsadj', 'hru_tlaps', 'nhru', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'Index of lapse temperature station for HRU',
     +       'Index of the lapse temperature station used for lapse'//
     +       ' rate calculations',
     +       'none').NE.0 ) RETURN
      ENDIF

      obsadjdecl = 0

      END FUNCTION obsadjdecl

!***********************************************************************
!     obsadjinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsadjinit()
      USE PRMS_OBS_ADJUST
      USE PRMS_MODULE, ONLY: Temp_flag
      USE PRMS_BASIN, ONLY: Nhru, Starttime
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Tmax_allrain, Hru_tsta
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      INTEGER :: i
!***********************************************************************
      obsadjinit = 1

      IF ( getparam('obsadj', 'max_missing', 1, 'integer',
     +     Max_missing)/=0 ) RETURN
      IF ( Max_missing==0 ) Max_missing = 999
      Max_missing = Max_missing + 1

      IF ( Temp_flag==3 ) THEN
        IF ( getparam('obsadj', 'hru_tlaps', Nhru, 'integer', Hru_tlaps)
     +       /=0 ) RETURN
      ENDIF

      Nuse_tsta = 0
      DO i = 1, Nhru
        Nuse_tsta(Hru_tsta(i)) = 1
        IF ( Temp_flag==3 ) THEN
          IF ( Hru_tlaps(i).LT.1 ) Hru_tlaps(i) = 1
          IF ( Hru_tlaps(i)>Ntemp ) THEN
            PRINT *, 'ERROR, hru_tlaps>ntemp, HRU:', i
            RETURN
          ENDIF
          Nuse_tsta(Hru_tlaps(i)) = 1
        ENDIF
      ENDDO

      Tmax_cnt = 0
      Tmin_cnt = 0
      Tmax_prev = Tmax_allrain(Starttime(2))
      Tmin_prev = Tmax_prev

      obsadjinit = 0
      END FUNCTION obsadjinit

! **********************************************************************
!     obsadjrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsadjrun()
      USE PRMS_OBS_ADJUST
      USE PRMS_CLIMATEVARS, ONLY: Ntemp
      !WARNING can modify Tmax and Tmin variables
      USE PRMS_OBS, ONLY: Nowtime, Tmax, Tmin
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, k, kk, kkk
! **********************************************************************
      IF ( Ntemp>0 ) THEN
        kk = 0
        kkk = 0
        DO i = 1, Ntemp
          k = 0
          IF ( Nuse_tsta(i)>0 ) THEN
            IF ( Tmax(i)<-99.0 .OR. Tmax(i)>150.0 ) THEN
              Tmax_cnt(i) = Tmax_cnt(i) + 1
              IF ( Tmax_cnt(i)<Max_missing ) THEN
                PRINT 9002, 'tmax', Tmax(i), Tmax_prev(i), i, Nowtime
                Tmax(i) = Tmax_prev(i)
                k = 1
                kk = 1
              ELSE
                PRINT 9001, 'tmax', Tmax(i), i, Nowtime
                RETURN
              ENDIF
            ELSE
              Tmax_prev(i) = Tmax(i)
              Tmax_cnt(i) = 0
            ENDIF
            IF ( Tmin(i)<-99.0 .OR. Tmin(i)>150.0 ) THEN
              Tmin_cnt(i) = Tmin_cnt(i) + 1
              IF ( Tmin_cnt(i)<Max_missing ) THEN
                PRINT 9002, 'tmin', Tmin(i), Tmin_prev(i), i, Nowtime
                Tmin(i) = Tmin_prev(i)
                k = 1
                kkk = 1
              ELSE
                PRINT 9001, 'tmin', Tmin(i), i, Nowtime
                RETURN
              ENDIF
            ELSE
              Tmin_prev(i) = Tmin(i)
              Tmin_cnt(i) = 0
            ENDIF
          ENDIF
        ENDDO
        ! if all values good, reset _cnt variable
        IF ( kk==0 ) Tmax_cnt = 0
        IF ( kkk==0 ) Tmin_cnt = 0
      ENDIF

 9001 FORMAT (/, 'ERROR, too many bad temperatures, ', A, ':', F10.3,
     +        '; temperature station:', I3, ' Time:', I5, 2('/', I2.2),
     +        I3, 2(':', I2.2))
 9002 FORMAT (/, 'Warning, bad ', A, ' temperature value:', F10.3, /,
     +        ' temperature set to previous value:', F10.3,
     +        '; temperature station:', I3, /, ' Time:', I5,
     +        2('/', I2.2), I3, 2(':', I2.2))

      obsadjrun = 0
      END FUNCTION obsadjrun

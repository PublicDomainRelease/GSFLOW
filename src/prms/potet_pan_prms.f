!***********************************************************************
! Computes the potential evapotranspiration for each HRU using
! pan-evaporation data
!***********************************************************************
      MODULE PRMS_POTET_PAN
      IMPLICIT NONE
!   Local Variables
      REAL, SAVE, ALLOCATABLE :: Last_pan_evap(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_pansta(:)
      REAL, SAVE, ALLOCATABLE :: Epan_coef(:)
      END MODULE PRMS_POTET_PAN

!***********************************************************************
!     Main potet_pan_prms routine
!***********************************************************************
      INTEGER FUNCTION potet_pan_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: petdecl_pan, petinit_pan, petrun_pan
!***********************************************************************
      potet_pan_prms = 0

      IF ( Process_flag==0 ) THEN
        potet_pan_prms = petrun_pan()
      ELSEIF ( Process_flag==1 ) THEN
        potet_pan_prms = petdecl_pan()
      ELSEIF ( Process_flag==2 ) THEN
        potet_pan_prms = petinit_pan()
      ENDIF

      END FUNCTION potet_pan_prms

!***********************************************************************
!     petdecl - set up parameters for potential et computations
!   Declared Parameters
!     hru_area, hru_pansta
!***********************************************************************
      INTEGER FUNCTION petdecl_pan()
      USE PRMS_POTET_PAN
      USE PRMS_MODULE, ONLY: Model
      USE PRMS_BASIN, ONLY: Nhru
      USE PRMS_OBS, ONLY: Nevap
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      petdecl_pan = 1

      IF ( declmodule(
     +'$Id: potet_pan_prms.f 2240 2010-12-10 00:28:24Z rsregan $'
     +).NE.0 ) RETURN

      IF ( Nevap==0 .AND. Model/=99 ) THEN
        PRINT *, 'Error, potet_pan_prms module selected, but no',
     +           'pan evap data in the Data File'
        RETURN
      ENDIF

      ALLOCATE (Epan_coef(12))
      IF ( declparam('potet', 'epan_coef', 'nmonths', 'real',
     +     '1.0', '0.2', '3.0',
     +     'Evaporation pan coefficient', 'Evaporation pan coefficient',
     +     'none').NE.0 ) RETURN      

      ALLOCATE (Hru_pansta(Nhru))
      IF ( declparam('potet', 'hru_pansta', 'nhru', 'integer',
     +     '0', 'bounded', 'nevap',
     +     'Index of pan evaporation station for each HRU',
     +     'Index of pan evaporation station used to compute'//
     +     ' HRU potential evapotranspiration',
     +     'none').NE.0 ) RETURN      

      petdecl_pan = 0
      END FUNCTION petdecl_pan

!***********************************************************************
!     petinit - Initialize potet module - get parameter values
!***********************************************************************
      INTEGER FUNCTION petinit_pan()
      USE PRMS_POTET_PAN
      USE PRMS_BASIN, ONLY: Nhru
      USE PRMS_OBS, ONLY: Nevap
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i
!***********************************************************************
      petinit_pan = 1

      IF ( getparam('potet', 'epan_coef', 12, 'real', Epan_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'hru_pansta', Nhru, 'integer', Hru_pansta)
     +     .NE.0 ) RETURN

      DO i = 1, Nhru
        IF ( Hru_pansta(i)<1 .OR. Hru_pansta(i)>Nevap ) Hru_pansta = 1
      ENDDO

      ALLOCATE (Last_pan_evap(Nevap))
      Last_pan_evap = 0.0

      petinit_pan = 0
      END FUNCTION petinit_pan

!***********************************************************************
!      petrun - Computes potential et for each HRU each day
!***********************************************************************
      INTEGER FUNCTION petrun_pan()
      USE PRMS_POTET_PAN
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet
      USE PRMS_OBS, ONLY: Nowtime, Nowmonth, Pan_evap
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, k, j
!***********************************************************************
!****Compute potential et for each HRU using pan evaporation formulation
      Basin_potet = 0.0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        k = Hru_pansta(i)
        IF ( Pan_evap(k)<0.0 ) THEN
          PRINT *, 'Pan_evap<0, set to last value', k, Pan_evap(k),
     +             Nowtime
          Pan_evap(k) = Last_pan_evap(k)
        ENDIF
        Potet(i) = Pan_evap(k)*Epan_coef(Nowmonth)
        IF ( Potet(i)<0.0 ) Potet(i) = 0.0
        Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
      ENDDO
      Basin_potet = Basin_potet*Basin_area_inv
      Last_pan_evap = Pan_evap

      petrun_pan = 0
      END FUNCTION petrun_pan

!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1969)
!***********************************************************************
      MODULE PRMS_POTET_JH
      IMPLICIT NONE
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Jh_coef_hru(:), Jh_coef(:)
      END MODULE PRMS_POTET_JH

!***********************************************************************
!     Main potet_jh routine
!***********************************************************************
      INTEGER FUNCTION potet_jh_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: petdecl_jh, petinit_jh, petrun_jh
!***********************************************************************
      potet_jh_prms = 0

      IF ( Process_flag==0 ) THEN
        potet_jh_prms = petrun_jh()
      ELSEIF ( Process_flag==1 ) THEN
        potet_jh_prms = petdecl_jh()
      ELSEIF ( Process_flag==2 ) THEN
        potet_jh_prms = petinit_jh()
      ENDIF

      END FUNCTION potet_jh_prms

!***********************************************************************
!     petdecl - set up parameters for potential et computations
!   Declared Parameters
!     jh_coef, jh_coef_hru, hru_area
!***********************************************************************
      INTEGER FUNCTION petdecl_jh()
      USE PRMS_POTET_JH
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      petdecl_jh = 1

      IF ( declmodule(
     +'$Id: potet_jh_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

      ALLOCATE (Jh_coef(12))
      IF ( declparam('potet', 'jh_coef', 'nmonths', 'real',
     +     '.014', '.005', '.060',
     +     'Monthly air temp coefficient - Jensen-Haise',
     +     'Monthly air temperature coefficient used in Jensen'//
     +     '-Haise potential evapotranspiration computations, see'//
     +     ' PRMS manual for calculation method',
     +     'per degrees').NE.0 ) RETURN

      ALLOCATE (Jh_coef_hru(Nhru))
      IF ( declparam('potet', 'jh_coef_hru', 'nhru', 'real',
     +     '13.0', '5.0', '20.0',
     +     'HRU air temp coefficient - Jensen-Haise',
     +    'Air temperature coefficient used in Jensen-Haise potential'//
     +     ' evapotranspiration computations for each HRU.  See PRMS'//
     +     ' manual for calculation method',
     +     'per degrees').NE.0 ) RETURN

      petdecl_jh = 0
      END FUNCTION petdecl_jh

!***********************************************************************
!     petinit - Initialize potet module - get parameter values
!***********************************************************************
      INTEGER FUNCTION petinit_jh()
      USE PRMS_POTET_JH
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      petinit_jh = 1

      IF ( getparam('potet', 'jh_coef', 12, 'real', Jh_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('potet', 'jh_coef_hru', Nhru, 'real', Jh_coef_hru)
     +     .NE.0 ) RETURN

      petinit_jh = 0
      END FUNCTION petinit_jh

!***********************************************************************
!     petrun - computes potential et for each HRU each day
!***********************************************************************
      INTEGER FUNCTION petrun_jh()
      USE PRMS_POTET_JH
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area,
     +    Hru_route_order, Nhru
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf,
     +    Swrad
      USE PRMS_OBS, ONLY: Nowmonth
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, j
      REAL :: elh
!***********************************************************************
!******Compute potential et for each HRU using Jensen-Haise formulation
! 597.3 cal/gm at 0 C is the energy required to change the state of 
! water to vapor
! elh is the latent heat of vaporization (not including the *2.54)
      Basin_potet = 0.0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        elh = (597.3-(.5653*Tavgc(i)))*2.54
        Potet(i) = Jh_coef(Nowmonth)*(Tavgf(i)-Jh_coef_hru(i))*Swrad(i)
     +             /elh
        IF ( Potet(i).LT.0. ) Potet(i) = 0.
        Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
      ENDDO
      Basin_potet = Basin_potet*Basin_area_inv

      petrun_jh = 0
      END FUNCTION petrun_jh

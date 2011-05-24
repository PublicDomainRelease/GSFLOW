!***********************************************************************
! Computes the potential evapotranspiration using the Hamon
! formulation (Hamon, 1961); modification of potet_hamon_prms
!***********************************************************************
      MODULE PRMS_POTET_HAMON_HRU
      IMPLICIT NONE
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Hamon_coef(:)
      END MODULE PRMS_POTET_HAMON_HRU

!***********************************************************************
!     Main potet_hamon routine
!***********************************************************************
      INTEGER FUNCTION potet_hamon_hru_prms()
      USE PRMS_POTET_HAMON_HRU
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: pethrun, getparam, declmodule, declparam
!***********************************************************************
      potet_hamon_hru_prms = 1

      IF ( Process_flag==0 ) THEN
        potet_hamon_hru_prms = pethrun()

      ELSEIF ( Process_flag==1 ) THEN
        IF ( declmodule(
     +'$Id: potet_hamon_hru_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +     ).NE.0 ) RETURN

        ALLOCATE (Hamon_coef(12))
        IF ( declparam('potet', 'hamon_coef', 'nmonths', 'real',
     +       '.0055', '.004', '.008',
     +       'Monthly air temp coefficient - Hamon',
     +       'Monthly air temperature coefficient used in Hamon'//
     +       ' potential evapotranspiration computations',
     +       '????').NE.0 ) RETURN

      ELSEIF ( Process_flag==2 ) THEN
        IF ( getparam('potet', 'hamon_coef', 12, 'real', Hamon_coef)
     +       .NE.0 ) RETURN
      ENDIF

      potet_hamon_hru_prms = 0
      END FUNCTION potet_hamon_hru_prms

!***********************************************************************
!      pethrun - computes potential et for each HRU each day
!***********************************************************************
      INTEGER FUNCTION pethrun()
      USE PRMS_POTET_HAMON_HRU
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order,
     +    Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tavgc, Basin_potet, Potet
      USE PRMS_SOLTAB, ONLY: Soltab_sunhrs
      USE PRMS_OBS, ONLY: Nowmonth, Jday
      IMPLICIT NONE
      INTRINSIC EXP
! Local Variables
      INTEGER :: i, j
      REAL :: dyl, vpsat, vdsat
!***********************************************************************
!******Compute potential et for each HRU using Hamon formulation

      Basin_potet = 0.0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
! Convert daylength from hours to 12 hour multiple (equal day and night period)
        dyl = Soltab_sunhrs(Jday, i)/12.0
        vpsat = 6.108*EXP(17.26939*Tavgc(i)/(Tavgc(i)+237.3))
        vdsat = 216.7*vpsat/(Tavgc(i)+273.3)
        Potet(i) = Hamon_coef(Nowmonth)*dyl*dyl*vdsat
        IF ( Potet(i).LT.0.0 ) Potet(i) = 0.0
        Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
      ENDDO
      Basin_potet = Basin_potet*Basin_area_inv

      pethrun = 0
      END FUNCTION pethrun

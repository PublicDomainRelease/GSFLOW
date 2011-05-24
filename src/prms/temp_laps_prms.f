!***********************************************************************
! Distributes maximum and minimum temperatures to each HRU by computing
! a daily lapse rate with temperature data measured at two stations
!
! Variables needed from DATA FILE: tmax, tmin
!***********************************************************************
      MODULE PRMS_TEMP_LAPS
      IMPLICIT NONE
!   Local Variables
      REAL, SAVE, ALLOCATABLE :: Elfac(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_tlaps(:)
      END MODULE PRMS_TEMP_LAPS

!***********************************************************************
!     Main temp_laps routine
!***********************************************************************
      INTEGER FUNCTION temp_laps_prms()
      USE PRMS_TEMP_LAPS
      USE PRMS_MODULE, ONLY: Process_flag
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: tlapsinit, tlapsrun, declmodule, declparam
!***********************************************************************
      temp_laps_prms = 1

      IF ( Process_flag==0 ) THEN
        temp_laps_prms = tlapsrun()

      ELSEIF ( Process_flag==1 ) THEN
        IF ( declmodule(
     +'$Id: temp_laps_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +     ).NE.0 ) RETURN

        ALLOCATE (Hru_tlaps(Nhru))
        IF ( declparam('temp', 'hru_tlaps', 'nhru', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'Index of lapse temperature station for HRU',
     +       'Index of the lapse temperature station used for lapse'//
     +       ' rate calculateions',
     +       'none').NE.0 ) RETURN

      ELSEIF ( Process_flag==2 ) THEN
        temp_laps_prms = tlapsinit()
      ENDIF

      temp_laps_prms = 0
      END FUNCTION temp_laps_prms

!***********************************************************************
!     tlapsinit - Initialize temp_laps module - get parameter values,
!              compute elfac
!***********************************************************************
      INTEGER FUNCTION tlapsinit()
      USE PRMS_TEMP_LAPS
      USE PRMS_BASIN, ONLY: Nhru, Hru_elev, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Tsta_elev, Hru_tsta
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: j, k, l
      REAL :: tdiff
!***********************************************************************
      tlapsinit = 1

      IF ( getparam('temp', 'hru_tlaps', Nhru, 'integer', Hru_tlaps)
     +     .NE.0 ) RETURN

      ALLOCATE ( Elfac(Nhru) )

      DO j = 1, Nhru
        IF ( Hru_tlaps(j).LT.1 ) Hru_tlaps(j) = 1
        IF ( Hru_tlaps(j)>Ntemp ) THEN
          PRINT *, 'ERROR, hru_tlaps>ntemp, HRU:', j
          RETURN
        ENDIF
        k = Hru_tsta(j)
        l = Hru_tlaps(j)
        tdiff = Tsta_elev(l) - Tsta_elev(k)
        IF ( ABS(tdiff)<NEARZERO ) tdiff = 1.0
        Elfac(j) = (Hru_elev(j)-Tsta_elev(k))/tdiff
      ENDDO

      tlapsinit = 0
      END FUNCTION tlapsinit

!***********************************************************************
!     tlapsrun - Computes maximum, minumum and average temperature
!                for each HRU based on 2-station lapse rate
!***********************************************************************
      INTEGER FUNCTION tlapsrun()
      USE PRMS_TEMP_LAPS
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp,
     +    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf,
     +    Tavgc, Tmax_adj, Tmin_adj, Temp_units, Basin_tsta, Hru_tsta,
     +    Ntemp
      USE PRMS_OBS, ONLY: Nowtime, Tmax, Tmin
      IMPLICIT NONE
      REAL, EXTERNAL :: c_to_f, f_to_c
! Local Variables
      INTEGER :: j, k, l, jj
      REAL :: tmx, tmn, tcrx, tcrn, tmxsta, tmnsta
      REAL :: tmxtsta, tmntsta
!***********************************************************************
      Basin_tmax = 0.
      Basin_tmin = 0.
      Basin_temp = 0.

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        k = Hru_tsta(j)
        l = Hru_tlaps(j)
        tmxtsta = Tmax(k)
        tmntsta = Tmin(k)

        tmxsta = Tmax(l) - tmxtsta
        tmnsta = Tmin(l) - tmntsta

        tcrx = tmxsta*Elfac(j) + Tmax_adj(j)
        tcrn = tmnsta*Elfac(j) + Tmin_adj(j)
        tmx = tmxtsta + tcrx
        tmn = tmntsta + tcrn
        IF ( Temp_units.EQ.0 ) THEN
!         degrees F
          Tmaxf(j) = tmx
          Tminf(j) = tmn
          Tavgf(j) = (tmx+tmn)*0.5
          Tmaxc(j) = f_to_c(tmx)
          Tminc(j) = f_to_c(tmn)
          Tavgc(j) = f_to_c(Tavgf(j))
          Basin_temp = Basin_temp + Tavgf(j)*Hru_area(j)
        ELSE
!         degrees C
          Tmaxc(j) = tmx
          Tminc(j) = tmn
          Tavgc(j) = (tmx+tmn)*0.5
          Tmaxf(j) = c_to_f(tmx)
          Tminf(j) = c_to_f(tmn)
          Tavgf(j) = c_to_f(Tavgc(j))
          Basin_temp = Basin_temp + Tavgc(j)*Hru_area(j)
        ENDIF
        Basin_tmax = Basin_tmax + tmx*Hru_area(j)
        Basin_tmin = Basin_tmin + tmn*Hru_area(j)
      ENDDO

      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv
      Basin_temp = Basin_temp*Basin_area_inv

      Solrad_tmax = Tmax(Basin_tsta)
      Solrad_tmin = Tmin(Basin_tsta)

      tlapsrun = 0
      END FUNCTION tlapsrun

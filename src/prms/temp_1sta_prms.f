!***********************************************************************
! Distributes maximum and minimum temperatures to each HRU using
! temperature data measured at one station and an estimated monthly
! lapse rate
!
! Variables needed from DATA FILE: tmax, tmin
!***********************************************************************
      MODULE PRMS_TEMP
      IMPLICIT NONE
!   Local Variables
      REAL, SAVE, ALLOCATABLE :: Tcrn(:), Tcrx(:), Elfac(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Tmax_lapse(:), Tmin_lapse(:)
      END MODULE PRMS_TEMP

!***********************************************************************
!     Main temp_1sta routine
!***********************************************************************
      INTEGER FUNCTION temp_1sta_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: t1decl, t1init, t1run
!***********************************************************************
      temp_1sta_prms = 0

      IF ( Process_flag==0 ) THEN
        temp_1sta_prms = t1run()
      ELSEIF ( Process_flag==1 ) THEN
        temp_1sta_prms = t1decl()
      ELSEIF ( Process_flag==2 ) THEN
        temp_1sta_prms = t1init()
      ENDIF

      END FUNCTION temp_1sta_prms

!***********************************************************************
!     t1decl - set up parameters for temperature computations
!   Declared Parameters
!     tmax_lapse, tmin_lapse, tsta_elev, tmax_adj, tmin_adj
!     hru_tsta, hru_elev, hru_area, temp_units, basin_tsta
!***********************************************************************
      INTEGER FUNCTION t1decl()
      USE PRMS_TEMP
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      t1decl = 1

      IF ( declmodule(
     +'$Id: temp_1sta_prms.f 2254 2010-12-10 18:45:19Z rsregan $'
     +).NE.0 ) RETURN

      ALLOCATE (Tmax_lapse(12))
      IF ( declparam('temp', 'tmax_lapse', 'nmonths', 'real',
     +     '3.0', '-10.0', '10.0',
     +     'Monthly maximum temperature lapse rate',
     +     'Array of twelve values representing the change in'//
     +     ' maximum temperature per 1000 elev_units of'//
     +     ' elevation change for each month, January to December',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tmin_lapse(12))
      IF ( declparam('temp', 'tmin_lapse', 'nmonths', 'real',
     +     '3.0', '-10.0', '10.0',
     +     'Monthly minimum temperature lapse rate',
     +     'Array of twelve values representing the change in'//
     +     ' minimum temperture per 1000 elev_units of'//
     +     ' elevation change for each month, January to December',
     +     'degrees').NE.0 ) RETURN

      t1decl = 0
      END FUNCTION t1decl

!***********************************************************************
!     t1init - Initialize temp_1sta module - get parameter values,
!              compute elfac
!***********************************************************************
      INTEGER FUNCTION t1init()
      USE PRMS_TEMP
      USE PRMS_BASIN, ONLY: Nhru, Hru_elev, Starttime
      USE PRMS_CLIMATEVARS, ONLY: Tmax_adj, Tmin_adj, Tsta_elev,
     +    Hru_tsta, Ntemp
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: j, k
      REAL :: tmaxlaps, tminlaps
!***********************************************************************
      t1init = 1

      IF ( getparam('temp', 'tmin_lapse', 12, 'real', Tmin_lapse)
     +     .NE.0 ) RETURN

      IF ( getparam('temp', 'tmax_lapse', 12, 'real', Tmax_lapse)
     +     .NE.0 ) RETURN

      ALLOCATE (Tcrn(Nhru), Tcrx(Nhru), Elfac(Nhru))

      tmaxlaps = Tmax_lapse(Starttime(2))
      tminlaps = Tmin_lapse(Starttime(2))
      DO j = 1, Nhru
        k = Hru_tsta(j)
        Elfac(j) = (Hru_elev(j)-Tsta_elev(k))/1000.
        Tcrx(j) = tmaxlaps*Elfac(j) - Tmax_adj(j)
        Tcrn(j) = tminlaps*Elfac(j) - Tmin_adj(j)
      ENDDO

      t1init = 0
      END FUNCTION t1init

!***********************************************************************
!     t1run - Computes maximum, minumum and average temperature
!             for each HRU based on monthly lapse rate
!***********************************************************************
      INTEGER FUNCTION t1run()
      USE PRMS_TEMP
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order,
     +    Basin_area_inv, Nhru
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp,
     +    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf,
     +    Tavgc, Temp_units, Basin_tsta, Tmax_adj, Tmin_adj, Ntemp,
     +    Hru_tsta
      USE PRMS_OBS, ONLY: Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
      REAL, EXTERNAL :: c_to_f, f_to_c
! Local Variables
      INTEGER :: j, k, jj
      REAL :: tmx, tmn, tmaxlaps, tminlaps
!***********************************************************************
      Basin_tmax = 0.
      Basin_tmin = 0.
      Basin_temp = 0.

      tmaxlaps = Tmax_lapse(Nowmonth)
      tminlaps = Tmin_lapse(Nowmonth)
      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        k = Hru_tsta(j)
        IF ( Nowday.EQ.1 ) THEN
          Tcrx(j) = tmaxlaps*Elfac(j) - Tmax_adj(j)
          Tcrn(j) = tminlaps*Elfac(j) - Tmin_adj(j)
        ENDIF
        tmx = Tmax(k) - Tcrx(j)
        tmn = Tmin(k) - Tcrn(j)
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

      t1run = 0
      END FUNCTION t1run

!***********************************************************************
! Computes the potential evapotranspiration using the Penman-Monteith
! formulation according to Murray (1967), shown equation 13 in Irmak and others (2012)
!***********************************************************************
      MODULE PRMS_POTET_PM
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=8), SAVE :: MODNAME
        REAL, SAVE, ALLOCATABLE :: Tavgc_ante(:)
        ! Declared Variables
        REAL, SAVE, ALLOCATABLE :: Tempc_dewpt(:), Vp_actual(:), Vp_sat(:)
        REAL, SAVE, ALLOCATABLE :: Lwrad_net(:), Vp_slope(:) !, Net_radiation(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Pm_n_coef(:, :), Pm_d_coef(:, :) !, Hru_albedo_coef(:)
      END MODULE PRMS_POTET_PM

!***********************************************************************
      INTEGER FUNCTION potet_pm()
      USE PRMS_POTET_PM
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file, Init_vars_from_file
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order, Hru_elev_feet, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tmaxc, Tminc, Swrad
      USE PRMS_CLIMATE_HRU, ONLY: Humidity_hru, Windspeed_hru
      USE PRMS_SOLTAB, ONLY: Soltab_potsw
      USE PRMS_SET_TIME, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC LOG, SQRT, SNGL, DBLE
      INTEGER, EXTERNAL :: declvar, declparam, getparam
      REAL, EXTERNAL :: sat_vapor_press_poly
      EXTERNAL read_error, print_module, potet_pm_restart
! Local Variables
      INTEGER :: i, j
      REAL :: elh, temp, tavg, vp_deficit, heat_flux, a, b, c, psycnst, p_atm
      DOUBLE PRECISION :: lwrad
      CHARACTER(LEN=80), SAVE :: Version_potet
!***********************************************************************
      potet_pm = 0

      IF ( Process(:3)=='run' ) THEN
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          tavg = Tavgc(i)

          ! dry bulb temperature, relative humidity
          temp = (LOG(Humidity_hru(i)/100.0) + ((17.26939*tavg) / (237.3+tavg))) / 17.26939
          Tempc_dewpt(i) = (237.3*temp) / (1.0 - temp)

          ! Actual vapor pressure
          Vp_actual(i) = 0.1*sat_vapor_press_poly(Tempc_dewpt(i)) ! ea

          ! saturation vapor pressure deficit
          Vp_sat(i) = 0.1*(sat_vapor_press_poly(Tmaxc(i)) + sat_vapor_press_poly(Tminc(i))) * 0.5 ! es

          ! slope of the saturation vaport pressure v. air temperature
          Vp_slope(i) = (409.8*sat_vapor_press_poly(tavg)) / (tavg+237.3)**2 ! delta

          ! saturation vapor pressure deficit
          vp_deficit = Vp_sat(i) - Vp_actual(i)
          
          ! net long-wave
          lwrad = 4.903D-09*(DBLE(tavg)+273.16D0)*(0.34D0-0.14D0*DBLE(SQRT(Vp_actual(i))))&
      &           *(1.35D0*DBLE(Swrad(i)/Soltab_potsw(i,Jday))-0.35D0) ! what is solf Rs/Rso (? Rs = Swrad - Rso = potsw_soltab??)
          Lwrad_net(i) = SNGL(lwrad) ! net radiation = Rns - Rnl; Rns = (1.0-albedo)*swrad
          !Net_radiation(i) = Hru_albedo_coef(i)*Swrad(i) - Lwrad_net(i)
          
          ! heat flux density to the ground
          heat_flux = -4.2*(Tavgc_ante(i)-tavg) ! G

          ! atmospheric pressure for altitude, kPa
          p_atm = 101.3 - 0.003215*Hru_elev_feet(i)

          ! Latent heat of vaporization, calories/gram
          elh = 597.3 - 0.5653*tavg

          ! psychrometric constant, kilopascals per degrees C
          psycnst = 1006.0*p_atm/(0.622*elh)

          a = 0.408*Vp_slope(i)*(Lwrad_net(i)/23.88-heat_flux)
!          a = 0.408*Vp_slope(i)*(Net_radiation(i)/23.88-heat_flux)
          b = psycnst*(Pm_n_coef(i,Nowmonth)/(tavg+273.16))*Windspeed_hru(i)*vp_deficit
          c = Vp_slope(i) + psycnst*(1.0+Pm_d_coef(i,Nowmonth)*Windspeed_hru(i))
          Potet(i) = (a + b)/c
          IF ( Potet(i)<NEARZERO ) Potet(i) = 0.0
          Basin_potet = Basin_potet + DBLE( Potet(i)*Hru_area(i) )
          Tavgc_ante(i) = Tavgc(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet = '$Id: potet_pm.f90 7434 2015-06-10 22:15:43Z rsregan $'
        CALL print_module(Version_potet, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_pm'

        ! Declare Variables
        ALLOCATE ( Tempc_dewpt(Nhru) )
        IF ( declvar(MODNAME, 'tempc_dewpt', 'nhru', Nhru, 'real', &
     &     'Air temperature at dew point for each HRU', &
     &     'degrees Celsius', Tempc_dewpt)/=0 ) CALL read_error(3, 'tempc_dewpt')

        ALLOCATE ( Vp_actual(Nhru) )
        IF ( declvar(MODNAME, 'vp_actual', 'nhru', Nhru, 'real', &
     &     'Actual vapor pressure for each HRU', &
     &     'kilopascals', Vp_actual)/=0 ) CALL read_error(3, 'vp_actual')

        ALLOCATE ( Vp_sat(Nhru) )
        IF ( declvar(MODNAME, 'vp_sat', 'nhru', Nhru, 'real', &
     &     'Saturation vapor pressure for each HRU', &
     &     'kilopascals', Vp_sat)/=0 ) CALL read_error(3, 'vp_sat')

        ALLOCATE ( Vp_slope(Nhru) )
        IF ( declvar(MODNAME, 'vp_slope', 'nhru', Nhru, 'real', &
     &     'Slope of saturation vapor pressure versus air temperature curve for each HRU', &
     &     'kilopascals/degrees Celsius', Vp_slope)/=0 ) CALL read_error(3, 'Vp_slope')

        ALLOCATE ( Lwrad_net(Nhru) )
        IF ( declvar(MODNAME, 'lwrad_net', 'nhru', Nhru, 'real', &
     &     'Net long-wave radiation for each HRU', &
     &     'megajoules/m**2/day', Lwrad_net)/=0 ) CALL read_error(3, 'lwrad_net')

      !   ALLOCATE ( Net_radiation(Nhru) )
      !   IF ( declvar(MODNAME, 'net_radiation', 'nhru', Nhru, 'real', &
      !&     'Net radiation for each HRU', &
      !&     'megajoules/m**2/day', Net_radiation)/=0 ) CALL read_error(3, 'net_radiation')

        ! Allocate local arrays
        ALLOCATE ( Tavgc_ante(Nhru) )

        ! Declare Parameters
        ALLOCATE ( Pm_n_coef(Nhru,12) )
        IF ( declparam(MODNAME, 'pm_n_coef', 'nhru,nmonths', 'real', &
     &         '900.0', '850.0', '950.0', &
     &         'Penman-Monteith coefficient', &
     &         'Monthly (January to December) Penman-Monteith potential ET N temperauture coefficient for each HRU', &
     &         'degrees Celsius per day')/=0 ) CALL read_error(1, 'pm_n_coef')

        ALLOCATE ( Pm_d_coef(Nhru,12) )
        IF ( declparam(MODNAME, 'pm_d_coef', 'nhru,nmonths', 'real', &
     &       '0.34', '0.25', '0.45', &
     &       'Penman-Monteith coefficient', &
     &       'Monthly (January to December) Penman-Monteith potential ET D wind-speed coefficient for each HRU', &
     &       'seconds/meters')/=0 ) CALL read_error(1, 'pm_d_coef')

!        ALLOCATE ( Hru_albedo_coef(Nhru) )
!        IF ( declparam(MODNAME, 'hru_albedo_coef', 'nhru', 'real', &
!     &       '0.77', '0.0', '1.0', &
!     &       'Canopy albedo coefficient for each HRU', &
!     &       'Canopy albedo coefficient for each HRU', &
!     &       'decimal fraction')/=0 ) CALL read_error(1, 'hru_albedo_coef')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) THEN
          CALL potet_pm_restart(1)
        ELSE
          Tempc_dewpt = 0.0
          Vp_actual = 0.0
          Vp_sat = 0.0
          Vp_slope = 0.0
          Lwrad_net = 0.0
          Tavgc_ante = Tavgc
        ENDIF
        IF ( getparam(MODNAME, 'pm_n_coef', Nhru*12, 'real', Pm_n_coef)/=0 ) CALL read_error(2, 'pm_n_coef')
        IF ( getparam(MODNAME, 'pm_d_coef', Nhru*12, 'real', Pm_d_coef)/=0 ) CALL read_error(2, 'pm_d_coef')
        !IF ( getparam(MODNAME, 'hru_albedo_coef', Nhru, 'real', Hru_albedo_coef)/=0 ) CALL read_error(2, 'hru_albedo_coef')

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL potet_pm_restart(0)
      ENDIF

      END FUNCTION potet_pm

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_pm_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_POTET_PM
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=8) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Tempc_dewpt
        WRITE ( Restart_outunit ) Vp_actual
        WRITE ( Restart_outunit ) Vp_sat
        WRITE ( Restart_outunit ) Vp_slope
        WRITE ( Restart_outunit ) Lwrad_net
        WRITE ( Restart_outunit ) Tavgc_ante
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Tempc_dewpt
        READ ( Restart_inunit ) Vp_actual
        READ ( Restart_inunit ) Vp_sat
        READ ( Restart_inunit ) Vp_slope
        READ ( Restart_inunit ) Lwrad_net
        READ ( Restart_inunit ) Tavgc_ante
      ENDIF
      END SUBROUTINE potet_pm_restart

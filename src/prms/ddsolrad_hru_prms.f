!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a maximum temperature per degree-day relation;
! modification of ddsolrad_prms
!
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 is
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_DDH_SOLRAD
      IMPLICIT NONE
!   Declared Parameters
      REAL, SAVE :: Radadj_slope, Radadj_intcp
      REAL, SAVE, ALLOCATABLE :: Dday_slope(:), Dday_intcp(:)
      REAL, SAVE, ALLOCATABLE :: Tmax_index(:)
      END MODULE PRMS_DDH_SOLRAD

!***********************************************************************
!     Main ddsolrad routine
!***********************************************************************
      INTEGER FUNCTION ddsolrad_hru_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: ddhsoldecl, ddhsolinit, ddhsolrun
!***********************************************************************
      ddsolrad_hru_prms = 0

      IF ( Process_flag==0 ) THEN
        ddsolrad_hru_prms = ddhsolrun()
      ELSEIF ( Process_flag==1 ) THEN
        ddsolrad_hru_prms = ddhsoldecl()
      ELSEIF ( Process_flag==2 ) THEN
        ddsolrad_hru_prms = ddhsolinit()
      ENDIF

      END FUNCTION ddsolrad_hru_prms

!***********************************************************************
! ddhsoldecl - set up parameters for actual solar radiation computations
!   Declared Parameters
!     dday_slope, dday_intcp, radj_sppt, radj_wppt, basin_solsta
!     radadj_slope, radadj_intcp, radmax, ppt_rad_adj, rad_conv
!     hru_area, tmax_index, tmax_allrain, hru_solsta
!***********************************************************************
      INTEGER FUNCTION ddhsoldecl()
      USE PRMS_DDH_SOLRAD
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      ddhsoldecl = 1

      IF ( declmodule(
     +'$Id: ddsolrad_hru_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

! Declare Parameters
      ALLOCATE (Dday_slope(12))
      IF ( declparam('solrad', 'dday_slope', 'nmonths', 'real',
     +     '.4', '0.2', '0.7',
     +     'Slope in temperature degree-day relationship',
     +     'Coefficient in relationship: dd-coef ='//
     +     ' dday_intcp + dday_slope*(tmax)+1.',
     +     'dday/degree').NE.0 ) RETURN

      ALLOCATE (Dday_intcp(12))
      IF ( declparam('solrad', 'dday_intcp', 'nmonths', 'real',
     +     '-10.', '-60', '4.0',
     +     'Intercept in temperature degree-day relationship',
     +     'Intercept in relationship: dd-coef ='//
     +     ' dday_intcp + dday_slope*(tmax)+1.',
     +     'dday').NE.0 ) RETURN

      IF ( declparam('solrad', 'radadj_slope', 'one', 'real',
     +     '0.0', '0.0', '1.0',
     +     'Slope in temperature range adjustment to solar radiation',
     +     'Slope in equation: adj = radadj_intcp + radadj_slope *'//
     +     ' (tmax - tmax_index)',
     +     'dday/degree').NE.0 ) RETURN

      IF ( declparam('solrad', 'radadj_intcp', 'one', 'real',
     +     '1.0', '.0', '1.0',
     +   'Intercept in temperature range adjustment to solar radiation',
     +   'Intercept in equation:'//
     +   ' adj = radadj_intcp + radadj_slope*(tmax-tmax_index)',
     +   'dday').NE.0 ) RETURN

      ALLOCATE (Tmax_index(12))
      IF ( declparam('solrad', 'tmax_index', 'nmonths', 'real',
     +     '50.', '-10.', '110.',
     +     'Monthly index temperature',
     +     'Index temperature used to determine precipitation'//
     +     ' adjustments to solar radiation, deg F or C depending'//
     +     ' on units of data',
     +     'degrees').NE.0 ) RETURN

      ddhsoldecl = 0
      END FUNCTION ddhsoldecl

!***********************************************************************
! ddhsolinit - Initialize ddsolrad_hru module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION ddhsolinit()
      USE PRMS_DDH_SOLRAD
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      ddhsolinit = 1

      IF ( getparam('solrad', 'dday_slope', 12, 'real', Dday_slope)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'dday_intcp', 12, 'real', Dday_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radadj_slope', 1, 'real', Radadj_slope)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radadj_intcp', 1, 'real', Radadj_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'tmax_index', 12, 'real', Tmax_index)
     +     .NE.0 ) RETURN

      ddhsolinit = 0
      END FUNCTION ddhsolinit

!***********************************************************************
! ddhsolrun - Computes actual solar radiation on horizontal surface,
!             then determines values for each HRU.
!***********************************************************************
      INTEGER FUNCTION ddhsolrun()
      USE PRMS_DDH_SOLRAD
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Active_hrus,
     +    Hru_route_order, Print_debug, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Basin_potsw, Solrad_tmax,
     +    Tmax_allrain, Temp_units, Basin_obs_ppt,
     +    Ppt_rad_adj, Basin_solsta, Nsol, Orad, Basin_horad, Radmax,
     +    Radj_sppt, Radj_wppt, Swrad, Rad_conv, Hru_solsta
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Hru_cossl, Soltab_basinpotsw,
     +    Hemisphere
      USE PRMS_OBS, ONLY: Solrad, Nowtime, Nowmonth, Jday
      IMPLICIT NONE
      INTRINSIC INT
! Local Variables
      INTEGER :: j, kp, kp1, jj, k
      REAL :: dday, pptadj, radadj, tdif, ddayi
! Save Variables
      REAL, SAVE, DIMENSION(26) :: solf
      DATA solf/.20, .35, .45, .51, .56, .59, .62, .64, .655, .67, .682,
     +          .69, .70, .71, .715, .72, .722, .724, .726, .728, .73,
     +          .734, .738, .742, .746, .75/
!***********************************************************************
!rsr using julian day as the soltab arrays are filled by julian day
      Basin_horad = Soltab_basinpotsw(Jday)

      Orad = -999.0
      IF ( Nsol.GT.0 ) Orad = Solrad(Basin_solsta)*Rad_conv

      IF ( Orad.LT.NEARZERO .OR. Orad.GT.10000.0 ) THEN

        dday = (Dday_slope(Nowmonth)*Solrad_tmax) + Dday_intcp(Nowmonth)
     +         + 1.0
        IF ( dday.LT.1.0 ) dday = 1.0

        IF ( Basin_obs_ppt.LE.Ppt_rad_adj(Nowmonth) ) THEN
          pptadj = 1.0
        ELSEIF ( Solrad_tmax.GE.Tmax_index(Nowmonth) ) THEN
          tdif = Solrad_tmax - Tmax_index(Nowmonth)
          pptadj = Radadj_intcp + Radadj_slope*tdif
          IF ( pptadj.GT.1. ) pptadj = 1.0
        ELSE
          pptadj = Radj_wppt
          IF ( Solrad_tmax.GE.Tmax_allrain(Nowmonth) ) THEN
            IF ( Hemisphere==0 ) THEN ! Northern Hemisphere
              IF ( Jday<79 .OR. Jday>265 ) THEN ! Equinox
                pptadj = Radj_wppt
              ELSE
                pptadj = Radj_sppt
              ENDIF
            ELSE ! Southern Hemisphere
              IF ( Jday>79 .OR. Jday<265 ) THEN ! Equinox
                pptadj = Radj_wppt
              ELSE
                pptadj = Radj_sppt
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF ( dday.LT.26.0 ) THEN
          kp = INT(dday)
          ddayi = kp
          kp1 = kp + 1
          radadj = solf(kp) + ((solf(kp1)-solf(kp))*(dday-ddayi))
        ELSE
          radadj = Radmax
        ENDIF
        radadj = radadj*pptadj
        IF ( radadj.LT.0.2 ) radadj = 0.2
        Orad = radadj*Basin_horad
      ENDIF

      Basin_potsw = 0.0
      IF ( Nsol==0 ) THEN
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          Swrad(j) = Soltab_potsw(Jday, j)/Basin_horad*Orad/Hru_cossl(j)
          Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
        ENDDO
      ELSE
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_solsta(j)
          IF ( k==0 ) THEN
            Swrad(j) =Soltab_potsw(Jday,j)/Basin_horad*Orad/Hru_cossl(j)
          ELSEIF ( k>Nsol ) THEN
            Swrad(j) =Soltab_potsw(Jday,j)/Basin_horad*Orad/Hru_cossl(j)
          ELSEIF ( Solrad(k)>-NEARZERO ) THEN
            Swrad(j) = Solrad(k)*Rad_conv
          ELSE
            Swrad(j) =Soltab_potsw(Jday,j)/Basin_horad*Orad/Hru_cossl(j)
            IF ( Print_debug==1 ) THEN
              PRINT *, 'Warning, measured solar radiation missing for',
     +                 ' HRU:', j, ' Solar radiation station:', k
              PRINT *, ' Computed instead:', Swrad(j), ' date:', Nowtime
            ENDIF
          ENDIF
          Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
        ENDDO
      ENDIF
      Basin_potsw = Basin_potsw*Basin_area_inv

      ddhsolrun = 0
      END FUNCTION ddhsolrun

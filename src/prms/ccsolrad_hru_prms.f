!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a relation between solar radiation and cloud
! cover; modification of ccsolrad_prms
!
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 is
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_CCH_SOLRAD
      IMPLICIT NONE
!   Declared Parameters
      REAL, SAVE :: Crad_coef, Crad_exp
      REAL, SAVE, ALLOCATABLE :: Ccov_slope(:), Ccov_intcp(:)
      END MODULE PRMS_CCH_SOLRAD

!***********************************************************************
!     Main ccsolrad_hru routine
!***********************************************************************
      INTEGER FUNCTION ccsolrad_hru_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: cchsoldecl, cchsolinit, cchsolrun
!***********************************************************************
      ccsolrad_hru_prms = 0

      IF ( Process_flag==0 ) THEN
        ccsolrad_hru_prms = cchsolrun()

      ELSEIF ( Process_flag==1 ) THEN
        ccsolrad_hru_prms = cchsoldecl()
      ELSEIF ( Process_flag==2 ) THEN
        ccsolrad_hru_prms = cchsolinit()
      ENDIF

      END FUNCTION ccsolrad_hru_prms

!***********************************************************************
! cchsoldecl - set up parameters for actual solar radiation computations
!   Declared Parameters
!     ccov_slope, ccov_intcp, radj_sppt, radj_wppt, basin_solsta
!     crad_coef, crad_exp, radmax, ppt_rad_adj, rad_conv
!     hru_area, hru_solsta
!***********************************************************************
      INTEGER FUNCTION cchsoldecl()
      USE PRMS_CCH_SOLRAD
      USE PRMS_BASIN, ONLY: Nhru, Timestep
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam
!***********************************************************************
      cchsoldecl = 1

      IF ( declmodule(
     +'$Id: ccsolrad_hru_prms.f 2303 2011-01-03 21:47:53Z rsregan $'
     +).NE.0 ) RETURN

! Declare Parameters
      ALLOCATE (Ccov_slope(12))
      IF ( declparam('solrad', 'ccov_slope', 'nmonths', 'real',
     +     '-.13', '-0.5', '-.01',
     +     'Slope in temperature cloud cover relationship',
     +     'Coefficient in relationship: cloudcover ='//
     +     ' ccov_intcp + ccov_slope*(tmax-tmin)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Ccov_intcp(12))
      IF ( declparam('solrad', 'ccov_intcp', 'nmonths', 'real',
     +     '1.83', '0.0', '5.0',
     +     'Intercept in temperature cloud cover relationship',
     +     'Intercept in relationship: cloudcover ='//
     +     ' ccov_intcp + ccov_slope*(tmax-tmin)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('solrad', 'crad_coef', 'one', 'real',
     +     '0.4', '0.1', '0.7',
     +     'Coefficient in cloud cover-solar radiation relationship',
     +     'Coefficient(B) in Thompson(1976) equation:'//
     +     ' Solar radiation = B + (1.-B)*(1-cloudcover)**P'//
     +     ' Varies by region, contour map of values in reference.',
     +     'none').NE.0 ) RETURN

      IF ( declparam('solrad', 'crad_exp', 'one', 'real',
     +     '0.61', '0.2', '0.8',
     +     'Exponent in cloud cover-solar radiation relationship',
     +     'Exponent(P) in Thompson(1976) equation:'//
     +     ' Solar radiation = B +(1.-B)*(1-cloudcover)**P'//
     +     ' Author suggests value of 0.61.',
     +     'none').NE.0 ) RETURN

      cchsoldecl = 0
      END FUNCTION cchsoldecl

!***********************************************************************
! cchsolinit - Initialize ccsolrad_hru module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION cchsolinit()
      USE PRMS_CCH_SOLRAD
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      cchsolinit = 1

      IF ( getparam('solrad', 'ccov_slope', 12, 'real', Ccov_slope)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'ccov_intcp', 12, 'real', Ccov_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'crad_coef', 1, 'real', Crad_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'crad_exp', 1, 'real', Crad_exp)
     +     .NE.0 ) RETURN

      cchsolinit = 0
      END FUNCTION cchsolinit

!***********************************************************************
! cchsolrun - Computes actual solar radiation on horizontal surface,
!             then determines values for each HRU.
!***********************************************************************
      INTEGER FUNCTION cchsolrun()
      USE PRMS_CCH_SOLRAD
      USE PRMS_BASIN, ONLY: NEARZERO, Active_hrus, Hru_route_order,
     +    Hru_area, Basin_area_inv, Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Ppt_rad_adj, Basin_solsta, Nsol, Orad,
     +    Basin_horad, Basin_potsw, Swrad, Radmax, Radj_sppt, Radj_wppt,
     +    Rad_conv, Hru_solsta, Basin_obs_ppt, Solrad_tmin, Solrad_tmax
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl,
     +    Hemisphere
      USE PRMS_OBS, ONLY: Solrad, Nowtime, Jday, Nowmonth
      IMPLICIT NONE
! Local Variables
      INTEGER :: j, jj, k
      REAL :: ccov, pptadj, radadj
!***********************************************************************
!rsr using julian day as the soltab arrays are filled by julian day
      Basin_horad = Soltab_basinpotsw(Jday)

      Orad = -999.0
      IF ( Nsol.GT.0 ) Orad = Solrad(Basin_solsta)*Rad_conv

      IF ( Orad.LT.NEARZERO .OR. Orad.GT.10000.0 ) THEN

        ccov = Ccov_slope(Nowmonth)*(Solrad_tmax-Solrad_tmin)
     +         + Ccov_intcp(Nowmonth)
        IF ( ccov.LT.NEARZERO ) THEN
          ccov = 0.0
        ELSEIF ( ccov.GT.1.0 ) THEN
          ccov = 1.0
        ENDIF

        IF ( Basin_obs_ppt.LE.Ppt_rad_adj(Nowmonth) ) THEN
          pptadj = 1.0
        ELSE
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
        radadj = Crad_coef + (1.-Crad_coef)*((1.-ccov)**Crad_exp)
        IF ( radadj.GT.Radmax ) radadj = Radmax
        radadj = radadj*pptadj
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

      cchsolrun = 0
      END FUNCTION cchsolrun

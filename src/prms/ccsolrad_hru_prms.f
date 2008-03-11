!***********************************************************************
!  Module to calculate daily solar radiation from max-min
!  temperature-cloud cover relationship
!RSR: Warning, summer hard-coded as May to September
!RSR:          could use basin_transp
!***********************************************************************
      MODULE PRMS_CCH_SOLRAD
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 1.0E-15
      INTEGER :: Nhru, Nsol, Ntemp, Lday
!   Declared Variables
      REAL :: Orad, Basin_potsw
      REAL, ALLOCATABLE :: Swrad(:), Daily_swrad(:)
!   Undeclared Variables from other modules - soltab
      REAL, ALLOCATABLE :: Soltab_potsw(:, :)
      REAL, ALLOCATABLE :: Hru_cossl(:), Soltab_basinpotsw(:)
!   Declared Variables from other modules - obs
      REAL, ALLOCATABLE :: Solrad(:), Tmax(:), Tmin(:)
!   Declared Variables from other modules - precip
      REAL :: Basin_obs_ppt
!   Declared Variables from other modules - potet
!     INTEGER :: Basin_transp_on
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
!rsr  REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Basin_solsta, Basin_tsta
      INTEGER, ALLOCATABLE :: Hru_solsta(:)
      REAL :: Radj_sppt, Radj_wppt, Crad_coef, Crad_exp, Radmax
      REAL :: Rad_conv
!rsr  REAL, ALLOCATABLE :: Hru_area(:)
      REAL, ALLOCATABLE :: Ccov_slope(:), Ccov_intcp(:), Ppt_rad_adj(:)
      END MODULE PRMS_CCH_SOLRAD

!***********************************************************************
!     Main ccsolrad_hru routine
!***********************************************************************
      INTEGER FUNCTION ccsolrad_hru_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: cchsoldecl, cchsolinit, cchsolrun
!***********************************************************************
      ccsolrad_hru_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        ccsolrad_hru_prms = cchsolrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        ccsolrad_hru_prms = cchsoldecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
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
      IMPLICIT NONE
      INTRINSIC MAX
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: n
!***********************************************************************
      cchsoldecl = 1

      IF ( declmodule(
     +'$Id: ccsolrad_hru_prms.f 3920 2008-03-03 21:30:13Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nsol = getdim('nsol')
      IF ( Nsol.EQ.-1 ) RETURN
      n = MAX(Nsol, 1)
      ALLOCATE (Solrad(n))

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN
      ALLOCATE (Tmax(Ntemp), Tmin(Ntemp))

      IF ( declpri('cchsoldecl_lday', 1, 'integer', Lday).NE.0 ) RETURN

! Declare Variables
      ALLOCATE (Swrad(Nhru))
      IF ( declvar('solrad', 'swrad', 'nhru', Nhru, 'real',
     +     'Computed shortwave radiation for each HRU',
     +     'langleys',
     +     Swrad).NE.0 ) RETURN

      IF ( declvar('solrad', 'orad', 'one', 1, 'real',
     +     'Observed or computed solar radiation on a horizontal'//
     +     ' surface',
     +     'langleys',
     +     Orad).NE.0 ) RETURN

      ALLOCATE (Daily_swrad(Nhru))
      IF ( declvar('solrad', 'daily_swrad', 'nhru', Nhru, 'real',
     +     'Computed daily shortwave radiation for each HRU,'//
     +     ' equals swrad if daily timestep',
     +     'langleys',
     +     Daily_swrad).NE.0 ) RETURN

      IF ( declvar('solrad', 'basin_potsw', 'one', 1, 'real',
     +     'Potential shortwave radiation for the basin centroid',
     +     'langleys',
     +     Basin_potsw).NE.0 ) RETURN

! Declare Parameters
      ALLOCATE (Ccov_slope(MAXMO))
      IF ( declparam('solrad', 'ccov_slope', 'nmonths', 'real',
     +     '-.13', '-0.5', '-.01',
     +     'Slope in temperature cloud cover relationship',
     +     'Coefficient in relationship: cloudcover ='//
     +     ' ccov_intcp + ccov_slope*(tmax-tmin)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Ccov_intcp(MAXMO))
      IF ( declparam('solrad', 'ccov_intcp', 'nmonths', 'real',
     +     '1.83', '0.0', '5.0',
     +     'Intercept in temperature cloud cover relationship',
     +     'Intercept in relationship: cloudcover ='//
     +     ' ccov_intcp + ccov_slope*(tmax-tmin)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('solrad', 'radj_sppt', 'one', 'real',
     +     '0.44', '0.0', '1.0',
     +     'Adjustment to solar radiation on precip day - summer',
     +     'Adjustment factor for computed solar radiation for'//
     +     ' summer day with greater than ppt_rad_adj inches precip',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam('solrad', 'radj_wppt', 'one', 'real',
     +     '0.5', '0.0', '1.0',
     +     'Adjustment to solar radiation on precip day - winter',
     +     'Adjustment factor for computed solar radiation for'//
     +     ' winter day with greater than ppt_rad_adj inches precip',
     +     'decimal fraction').NE.0 ) RETURN

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

      IF ( declparam('solrad', 'radmax', 'one', 'real',
     +     '0.8', '0.1', '1.0',
     +     'Maximum percent of potential solar radiation (decimal)',
     +     'The maximum portion of the potential solar radiation'//
     +     ' that may reach the ground due to haze, dust, smog, etc.',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Ppt_rad_adj(MAXMO))
      IF ( declparam('solrad', 'ppt_rad_adj', 'nmonths', 'real',
     +     '0.02', '0.0', '0.5',
     +     'Radiation reduced if basin precip above this value',
     +     'If basin precip exceeds this value, radiation is'//
     +     ' mutiplied by summer or winter precip adjustment ',
     +     'inches').NE.0 ) RETURN

      IF ( declparam('solrad', 'rad_conv', 'one', 'real',
     +     '1.0', '0.1', '100.0',
     +     'Conversion factor to langleys for observed radiation',
     +     'Conversion factor to langleys for observed radiation',
     +     'none').NE.0) RETURN

      ALLOCATE (Hru_solsta(Nhru))
      IF ( Nsol>0 ) THEN
        IF ( declparam('solrad', 'basin_solsta', 'one', 'integer',
     +       '0', 'bounded', 'nsol',
     +       'Index of main solar radiation station',
     +       'Index of solar radiation station used to compute basin'//
     +       ' radiation values',
     +       'none').NE.0 ) RETURN

        IF ( declparam('solrad', 'hru_solsta', 'nhru', 'integer',
     +      '0', 'bounded', 'nsol',
     +      'Index of solar radiation station associated with each HRU',
     +      'Index of solar radiation station associated with each HRU',
     +      'none').NE.0 ) RETURN
      ENDIF

!rsr  ALLOCATE (Hru_area(Nhru))
!rsr  IF ( declparam('solrad', 'hru_area', 'nhru', 'real',
!rsr +     '1.0', '0.01', '1e+09',
!rsr +     'HRU area', 'Area of each HRU',
!rsr +     'acres').NE.0 ) RETURN

      IF ( declparam('solrad', 'basin_tsta', 'one', 'integer',
     +     '1', 'bounded', 'ntemp',
     +     'Index of main temperature station',
     +     'Index of temperature station used to compute basin'//
     +     ' temperature values',
     +     'none').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Soltab_potsw(MAXDAY, Nhru), Hru_route_order(Nhru))
      ALLOCATE (Hru_cossl(Nhru), Soltab_basinpotsw(MAXDAY))

      cchsoldecl = 0
      END FUNCTION cchsoldecl

!***********************************************************************
! cchsolinit - Initialize ccsolrad_hru module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION cchsolinit()
      USE PRMS_CCH_SOLRAD
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: nstep
!***********************************************************************
      cchsolinit = 1

      IF ( getparam('solrad', 'ccov_slope', MAXMO, 'real', Ccov_slope)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'ccov_intcp', MAXMO, 'real', Ccov_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radj_sppt', 1, 'real', Radj_sppt)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radj_wppt', 1, 'real', Radj_wppt)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'crad_coef', 1, 'real', Crad_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'crad_exp', 1, 'real', Crad_exp)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radmax', 1, 'real', Radmax)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'ppt_rad_adj', MAXMO, 'real',
     +     Ppt_rad_adj).NE.0 ) RETURN

      IF ( getparam('solrad', 'rad_conv', 1, 'real', Rad_conv)
     +     .NE.0 ) RETURN

      IF ( Nsol>0 ) THEN
        IF ( getparam('solrad', 'basin_solsta', 1, 'integer',
     +       Basin_solsta).NE.0 ) RETURN
!rsr    IF ( Basin_solsta.LT.1 ) Basin_solsta = 1

        IF ( getparam('solrad', 'hru_solsta', Nhru, 'integer',
     +       Hru_solsta).NE.0 ) RETURN
      ELSE
        Hru_solsta = 0
      ENDIF

      IF ( getparam('solrad', 'basin_tsta', 1, 'integer', Basin_tsta)
     +     .NE.0 ) RETURN
      IF ( Basin_tsta.LT.1 ) Basin_tsta = 1

!rsr  IF ( getparam('solrad', 'hru_area', Nhru, 'real', Hru_area)
!rsr +     .NE.0 ) RETURN

!rsr  IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
!rsr +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

!** get soltab variables once, instead of each time in run
      IF ( getvar('soltab', 'soltab_basinpotsw', MAXDAY, 'real',
     +     Soltab_basinpotsw).NE.0 ) RETURN

      IF ( getvar('soltab', 'soltab_potsw', MAXDAY*Nhru, 'real',
     +     Soltab_potsw).NE.0 ) RETURN

      IF ( getvar('soltab', 'hru_cossl', Nhru, 'real', Hru_cossl)
     +     .NE.0 ) RETURN

      nstep = getstep()
      IF ( nstep.EQ.0 ) THEN
        Swrad = 0.0
        Orad = 0.0
        Lday = 0
        Basin_potsw = 0.0
        Daily_swrad = 0.0
      ENDIF

      cchsolinit = 0
      END FUNCTION cchsolinit

!***********************************************************************
! cchsolrun - Computes actual solar radiation on horizontal surface,
!             then determines values for each HRU.
!***********************************************************************
      INTEGER FUNCTION cchsolrun()
      USE PRMS_CCH_SOLRAD
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: nowtime(6), mo, j, day, jday, jj, k
      REAL :: ccov, pptadj, radadj
!     INTEGER :: jsol
!***********************************************************************
      cchsolrun = 0

      CALL dattim('now', nowtime)
      mo = nowtime(2)
      day = nowtime(3)

      IF ( Lday.EQ.day ) RETURN

      Lday = day
      cchsolrun = 1

!rsr using julian day as the soltab arrays are filled by julian day
!     jsol = julian('now', 'solar')
      jday = julian('now', 'calendar')
      Basin_potsw = Soltab_basinpotsw(jday)

      Orad = -999.0
      IF ( Nsol.GT.0 ) THEN
        IF ( getvar('obs', 'solrad', Nsol, 'real', Solrad).NE.0 ) RETURN
        IF ( Basin_solsta>0 ) Orad = Solrad(Basin_solsta)*Rad_conv
      ENDIF

      IF ( Orad.LT.NEARZERO .OR. Orad.GT.10000.0 ) THEN
        IF ( getvar('obs', 'tmax', Ntemp, 'real', Tmax).NE.0 ) RETURN
        IF ( getvar('obs', 'tmin', Ntemp, 'real', Tmin).NE.0 ) RETURN
        ccov = Ccov_slope(mo)*(Tmax(Basin_tsta)-Tmin(Basin_tsta))
     +         + Ccov_intcp(mo)
        IF ( ccov.LT.NEARZERO ) THEN
          ccov = 0.0
        ELSEIF ( ccov.GT.1.0 ) THEN
          ccov = 1.0
        ENDIF

        IF ( getvar('precip', 'basin_obs_ppt', 1, 'real', Basin_obs_ppt)
     +       .NE.0 ) RETURN
        IF ( Basin_obs_ppt.LE.Ppt_rad_adj(mo) ) THEN
          pptadj = 1.0
        ELSE
          pptadj = Radj_wppt
!         IF ( get var('potet', 'basin_transp_on', 1, 'integer',
!    +         Basin_transp_on).NE.0 ) RETURN
!         IF ( Basin_transp_on.EQ.1 ) pptadj = Radj_sppt
          IF ( mo.GT.4 .AND. mo.LT.10 ) pptadj = Radj_sppt
        ENDIF
        radadj = Crad_coef + (1.-Crad_coef)*((1.-ccov)**Crad_exp)
        IF ( radadj.GT.Radmax ) radadj = Radmax
        radadj = radadj*pptadj
        Orad = radadj*Basin_potsw
      ENDIF

!rsr  Basin_potsw = 0.0
      IF ( Nsol==0 ) THEN
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          Swrad(j) = Soltab_potsw(jday, j)/Basin_potsw*Orad/Hru_cossl(j)
!rsr      Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
        ENDDO
      ELSE
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_solsta(j)
          IF ( k==0 .OR. k>Nsol ) THEN
           Swrad(j) = Soltab_potsw(jday,j)/Basin_potsw*Orad/Hru_cossl(j)
          ELSE
            Swrad(j) = Solrad(k)*Rad_conv
          ENDIF
!rsr      Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
        ENDDO
      ENDIF
!rsr  Basin_potsw = Basin_potsw*Basin_area_inv
      Daily_swrad = Swrad

      cchsolrun = 0
      END FUNCTION cchsolrun

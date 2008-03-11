!***********************************************************************
! Module to calculate daily solar radiation from maximum temperature
! degree-day relationship
!RSR: Warning, summer hard-coded as May to September
!RSR:          could use basin_transp
!***********************************************************************
      MODULE PRMS_DDH_SOLRAD
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 1.0E-15
      INTEGER :: Nhru, Nsol, Lday
!   Declared Variables
      REAL :: Orad, Basin_potsw
      REAL, ALLOCATABLE :: Swrad(:), Daily_swrad(:)
!   Undeclared Variables from other modules - soltab
      REAL, ALLOCATABLE :: Soltab_potsw(:, :)
      REAL, ALLOCATABLE :: Hru_cossl(:), Soltab_basinpotsw(:)
!   Declared Variables from other modules - temp
      REAL :: Solrad_tmax
!   Declared Variables from other modules - obs
      REAL, ALLOCATABLE :: Solrad(:)
!   Declared Variables from other modules - precip
      REAL :: Basin_obs_ppt
!   Declared Variables from other modules - potet
!     INTEGER :: Basin_transp_on
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
!rsr  REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Basin_solsta
      INTEGER, ALLOCATABLE :: Hru_solsta(:)
      REAL :: Radj_sppt, Radj_wppt, Radadj_slope, Radadj_intcp, Radmax
      REAL :: Rad_conv
!rsr  REAL, ALLOCATABLE :: Hru_area(:)
      REAL, ALLOCATABLE :: Ppt_rad_adj(:), Tmax_allrain(:)
      REAL, ALLOCATABLE :: Dday_slope(:), Dday_intcp(:), Tmax_index(:)
      END MODULE PRMS_DDH_SOLRAD

!***********************************************************************
!     Main ddsolrad routine
!***********************************************************************
      INTEGER FUNCTION ddsolrad_hru_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: ddhsoldecl, ddhsolinit, ddhsolrun
!***********************************************************************
      ddsolrad_hru_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        ddsolrad_hru_prms = ddhsolrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        ddsolrad_hru_prms = ddhsoldecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
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
      INTRINSIC MAX
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: n
!***********************************************************************
      ddhsoldecl = 1

      IF ( declmodule(
     +'$Id: ddsolrad_hru_prms.f 3920 2008-03-03 21:30:13Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nsol = getdim('nsol')
      IF ( Nsol.EQ.-1 ) RETURN
      n = MAX(Nsol, 1)
      ALLOCATE (Solrad(n))

      IF ( declpri('ddhsoldecl_lday', 1, 'integer', Lday).NE.0 ) RETURN

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
      ALLOCATE (Dday_slope(MAXMO))
      IF ( declparam('solrad', 'dday_slope', 'nmonths', 'real',
     +     '.4', '0.2', '0.7',
     +     'Slope in temperature degree-day relationship',
     +     'Coefficient in relationship: dd-coef ='//
     +     ' dday_intcp + dday_slope*(tmax)+1.',
     +     'dday/degree').NE.0 ) RETURN

      ALLOCATE (Dday_intcp(MAXMO))
      IF ( declparam('solrad', 'dday_intcp', 'nmonths', 'real',
     +     '-10.', '-60', '4.0',
     +     'Intercept in temperature degree-day relationship',
     +     'Intercept in relationship: dd-coef ='//
     +     ' dday_intcp + dday_slope*(tmax)+1.',
     +     'dday').NE.0 ) RETURN

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

      IF ( declparam('solrad', 'radmax', 'one', 'real',
     +     '0.8', '0.1', '1.0',
     +     'Maximum percent of potential solar radiation (decimal)',
     +     'The maximum portion of the potential solar radiation'//
     +     ' that may reach the ground due to haze, dust, smog, etc.',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Tmax_index(MAXMO))
      IF ( declparam('solrad', 'tmax_index', 'nmonths', 'real',
     +     '50.', '-10.', '110.',
     +     'Monthly index temperature',
     +     'Index temperature used to determine precipitation'//
     +     ' adjustments to solar radiation, deg F or C depending'//
     +     ' on units of data',
     +     'degrees').NE.0 ) RETURN

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

      ALLOCATE (Tmax_allrain(MAXMO))
      IF ( declparam('solrad', 'tmax_allrain', 'nmonths', 'real',
     +     '40.', '0.', '90.',
     +     'Precip all rain if HRU max temperature above this value',
     +     'If maximum temperature of an HRU is greater than or equal'//
     +     ' to this value (for each month, January to December),'//
     +     ' precipitation is assumed to be rain',
     +     'degrees').NE.0 ) RETURN

!rsr  ALLOCATE (Hru_area(Nhru))
!rsr  IF ( declparam('solrad', 'hru_area', 'nhru', 'real',
!rsr +     '1.0', '0.01', '1e+09',
!rsr +     'HRU area', 'Area of each HRU',
!rsr +     'acres').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Soltab_potsw(MAXDAY, Nhru), Hru_route_order(Nhru))
      ALLOCATE (Hru_cossl(Nhru), Soltab_basinpotsw(MAXDAY))

      ddhsoldecl = 0
      END FUNCTION ddhsoldecl

!***********************************************************************
! ddhsolinit - Initialize ddsolrad_hru module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION ddhsolinit()
      USE PRMS_DDH_SOLRAD
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: nstep
!***********************************************************************
      ddhsolinit = 1

      IF ( getparam('solrad', 'dday_slope', MAXMO, 'real', Dday_slope)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'dday_intcp', MAXMO, 'real', Dday_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radj_sppt', 1, 'real', Radj_sppt)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radj_wppt', 1, 'real', Radj_wppt)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radadj_slope', 1, 'real', Radadj_slope)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radadj_intcp', 1, 'real', Radadj_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radmax', 1, 'real', Radmax)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'tmax_index', MAXMO, 'real', Tmax_index)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'tmax_allrain', MAXMO, 'real',
     +     Tmax_allrain).NE.0 ) RETURN

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

      ddhsolinit = 0
      END FUNCTION ddhsolinit

!***********************************************************************
! ddhsolrun - Computes actual solar radiation on horizontal surface,
!             then determines values for each HRU.
!***********************************************************************
      INTEGER FUNCTION ddhsolrun()
      USE PRMS_DDH_SOLRAD
      IMPLICIT NONE
      INTRINSIC INT
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: nowtime(6), mo, j, day, kp, kp1, jday, jj, k
      REAL :: dday, pptadj, radadj, tdif, ddayi
!     INTEGER :: jsol
! Save Variables
      REAL, SAVE, DIMENSION(26) :: solf
      DATA solf/.20, .35, .45, .51, .56, .59, .62, .64, .655, .67, .682,
     +          .69, .70, .71, .715, .72, .722, .724, .726, .728, .73,
     +          .724, .738, .742, .746, .75/
!***********************************************************************
      ddhsolrun = 0

      CALL dattim('now', nowtime)
      mo = nowtime(2)
      day = nowtime(3)

      IF ( Lday.EQ.day ) RETURN

      Lday = day
      ddhsolrun = 1

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

        IF ( getvar('temp', 'solrad_tmax', 1, 'real', Solrad_tmax)
     +       .NE.0 ) RETURN
        dday = (Dday_slope(mo)*Solrad_tmax) + Dday_intcp(mo) + 1.
        IF ( dday.LT.1.0 ) dday = 1.0

        IF ( getvar('precip', 'basin_obs_ppt', 1, 'real', Basin_obs_ppt)
     +       .NE.0 ) RETURN
        IF ( Basin_obs_ppt.LE.Ppt_rad_adj(mo) ) THEN
          pptadj = 1.0
        ELSEIF ( Solrad_tmax.GE.Tmax_index(mo) ) THEN
          tdif = Solrad_tmax - Tmax_index(mo)
          pptadj = Radadj_intcp + Radadj_slope*tdif
          IF ( pptadj.GT.1. ) pptadj = 1.0
        ELSE
!         IF ( get var('potet', 'basin_transp_on', 1, 'integer',
!    +         Basin_transp_on).NE.0 ) RETURN
          pptadj = Radj_wppt
          IF ( Solrad_tmax.GE.Tmax_allrain(mo) ) THEN
!           IF ( Basin_transp_on.EQ.1 ) pptadj = Radj_sppt
            IF ( mo.GT.4 .AND. mo.LT.10 ) pptadj = Radj_sppt
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

      ddhsolrun = 0
      END FUNCTION ddhsolrun

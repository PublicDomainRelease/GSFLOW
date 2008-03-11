!***********************************************************************
! xyz_dist.f
!     Distribute temperatures and precip to HRU's using Lauren Hay's XYZ
!     methodology.
!                      Converted by Steve Markstrom
!                      Wed Feb 10 15:16:04 MST 1999
!              revised Wed Mar 17 15:48:52 MST 1999
!              revised Mon Aug 30 16:47:07 MDT 1999
!              revised Mon Aug 30 16:47:07 MDT 1999
!              revised Wed Mar  8 09:06:18 MST 2000
!              revised Thu Feb  3 10:00:00 MST 2005
!                 (version 1.3 check by markstro -- no declpri needed) !
! temp_nsta - number of temperature stations used
! temp_nuse(temp_nsta) - indicies of temp stations used
! rain_nsta - number of precip stations used
! rain_nuse (rain_nsta) - indicies of rain stations used
!rsr, note tmax_allsnow and tmax_allrain assumed to be in Fahrenheit
!***********************************************************************
      MODULE PRMS_XYZ_DIST
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: FEET2METERS = 0.304800, NEARZERO = 1.0E-15
      INTEGER :: Nhru, Ntemp, Nrain, Nform, Nlapse
      INTEGER :: Temp_nsta, Rain_nsta, Nowtime(6), Storm
      INTEGER, ALLOCATABLE :: Rain_nuse(:), Temp_nuse(:)
      REAL :: Basin_centroid_x, Basin_centroid_y
      REAL, ALLOCATABLE :: Meantmax(:), Meantmin(:), Meanppt(:)
      REAL, ALLOCATABLE :: Temp_meanx(:), Temp_meany(:), Temp_meanz(:)
      REAL, ALLOCATABLE :: Rain_meanx(:), Rain_meany(:), Rain_meanz(:)
!   Declared Variables
      INTEGER :: Is_rain_day
      REAL :: Basin_temp, Basin_tmax, Basin_tmin, Basin_ppt
      REAL :: Solrad_tmax, Solrad_tmin, Basin_rain, Basin_snow
      INTEGER, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      REAL, ALLOCATABLE :: Tmaxf(:), Tminf(:), Tavgf(:)
      REAL, ALLOCATABLE :: Tmaxc(:), Tminc(:), Tavgc(:)
      REAL, ALLOCATABLE :: Tempf(:), Tempc(:)
      REAL, ALLOCATABLE :: Hru_ppt(:), Hru_rain(:), Hru_snow(:)
      REAL, ALLOCATABLE :: Tmax_rain_sta(:), Tmin_rain_sta(:), Prmx(:)
!   Declared Variables from other modules - obs
      INTEGER :: Rain_day, Route_on
      INTEGER, ALLOCATABLE :: Form_data(:)
      REAL, ALLOCATABLE :: Tmax(:), Tmin(:), Ppt(:)
!   Declared Variables from other modules - basin
!dbg  INTEGER :: Prt_debug
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Temp_units, Elev_units, Precip_units, Conv_flag
      INTEGER :: Basin_tsta_hru
      INTEGER, ALLOCATABLE :: Rain_code(:)
      REAL, ALLOCATABLE :: Tmax_allrain(:), Adjmix_rain(:)
      REAL, ALLOCATABLE :: Adjust_rain(:), Adjust_snow(:)
      REAL, ALLOCATABLE :: Max_lapse(:, :), Min_lapse(:, :)
      REAL, ALLOCATABLE :: Ppt_lapse(:, :)
      REAL :: Solrad_elev, Tmax_allsnow
      REAL :: Tmax_add, Tmax_div, Tmin_add, Tmin_div, Ppt_add, Ppt_div
      REAL :: X_add, X_div, Y_add, Y_div, Z_add, Z_div
      INTEGER, ALLOCATABLE :: Tsta_nuse(:)
      INTEGER, ALLOCATABLE :: Psta_nuse(:), Psta_freq_nuse(:)
      REAL, ALLOCATABLE :: MRUx(:), MRUy(:)
!     Temp_STAx = tsta_x, Temp_STAy = tsta_y, Temp_STAelev = tsta_elev
      REAL, ALLOCATABLE :: Temp_STAelev(:)
      REAL, ALLOCATABLE :: Temp_STAx(:), Temp_STAy(:)
!     Rain_STAx = psta_x, Rain_STAy = psta_y, Rain_STAelev = psta_elev
      REAL, ALLOCATABLE :: Rain_STAelev(:)
      REAL, ALLOCATABLE :: Rain_STAx(:), Rain_STAy(:)
      REAL, ALLOCATABLE :: Tmax_adj(:), Tmin_adj(:)
      REAL, ALLOCATABLE :: MRUelev(:), Hru_area(:)
!     TmaxMTH = tsta_month_max, TminMTH = tsta_month_min
!     PptMTH = psta_month_ppt
      REAL, ALLOCATABLE :: TmaxMTH(:, :), TminMTH(:, :), PptMTH(:, :)
      END MODULE PRMS_XYZ_DIST

!***********************************************************************
!     Main xyz_dist routine
!***********************************************************************
      INTEGER FUNCTION xyz_dist(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: xyzdecl, xyzinit, xyzrun
!***********************************************************************
      xyz_dist = 0

      IF ( Arg.EQ.'run' ) THEN
        xyz_dist = xyzrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        xyz_dist = xyzdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        xyz_dist = xyzinit()
      ENDIF

      END FUNCTION xyz_dist

!***********************************************************************
!     xyzdecl - set up parameters for temperature computations
!   Declared Parameters
!     hru_x, hru_y, max_lapse, min_lapse, ppt_lapse, tsta_elev
!     tmax_adj, tmin_adj, tsta_x, tsta_y, psta_elev, psta_x, psta_y
!     tsta_nuse, psta_nuse, psta_freq_nuse, adjust_snow, adjust_rain
!     tsta_month_max, tsta_month_min, psta_month_ppt, rain_code
!     x_add, x_div, y_add, y_div, z_add, z_div, solrad_elev
!     tmin_add, tmin_div, tmax_add, tmax_div, ppt_add, ppt_div
!     tmax_allrain, tmax_allsnow, adjmix_rain, conv_flag, basin_tsta_hru
!     hru_elev, hru_area, temp_units
!***********************************************************************
      INTEGER FUNCTION xyzdecl()
      USE PRMS_XYZ_DIST
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      xyzdecl = 1

      IF ( declmodule(
     +'$Id: xyz_dist.f 3591 2007-10-10 16:10:07Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN
      ALLOCATE (Tmin(Ntemp), Tmax(Ntemp))

      Nrain = getdim('nrain')
      IF ( Nrain.EQ.-1 ) RETURN
      ALLOCATE (Ppt(Nrain))

      Nform = getdim('nform')
      IF ( Nform.EQ.-1 ) RETURN
      IF ( Nform.GT.0 ) ALLOCATE (Form_data(Nform))

      Nlapse = getdim('nlapse')
      IF ( Nlapse.EQ.-1 ) RETURN
      IF ( Nlapse.NE.3 ) THEN
        PRINT *, '*** xyz_dist expecting nlapse = 3', Nlapse
        RETURN
      ENDIF

      IF ( declvar('xyz_dist', 'solrad_tmax', 'one', 1, 'real',
     +     'Basin daily maximum temperature adjusted to elevation of'//
     +     ' solar radiation station',
     +     'degrees F',
     +     Solrad_tmax).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'solrad_tmin', 'one', 1, 'real',
     +     'Basin daily minimum temperature adjusted to elevation of'//
     +     ' solar radiation station',
     +     'degrees F',
     +     Solrad_tmin).NE.0 ) RETURN

      ALLOCATE (Tmaxf(Nhru))
      IF ( declvar('xyz_dist', 'tmaxf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily maximum temperature',
     +     'degrees F',
     +     Tmaxf).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'is_rain_day', 'one', 1, 'integer',
     +     'Is it raining in the basin',
     +     'none',
     +     Is_rain_day).NE.0 ) RETURN

      ALLOCATE (Tminf(Nhru))
      IF ( declvar('xyz_dist', 'tminf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily minimum temperature',
     +     'degrees F',
     +     Tminf).NE.0 ) RETURN

      ALLOCATE (Tavgf(Nhru))
      IF ( declvar('xyz_dist', 'tavgf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily average temperature',
     +     'degrees F',
     +     Tavgf).NE.0 ) RETURN

      ALLOCATE (Tempf(Nhru))
      IF ( declvar('xyz_dist', 'tempf', 'nhru', Nhru, 'real',
     +     'HRU adjusted temperature for timestep < 24',
     +     'degrees F',
     +     Tempf).NE.0 ) RETURN

      ALLOCATE (Tmaxc(Nhru))
      IF ( declvar('xyz_dist', 'tmaxc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily maximum temperature',
     +     'degrees Celsius',
     +     Tmaxc).NE.0 ) RETURN

      ALLOCATE (Tminc(Nhru))
      IF ( declvar('xyz_dist', 'tminc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily minimum temperature',
     +     'degrees Celsius',
     +     Tminc).NE.0 ) RETURN

      ALLOCATE (Tavgc(Nhru))
      IF ( declvar('xyz_dist', 'tavgc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily average temperature',
     +     'degrees Celsius',
     +     Tavgc).NE.0 ) RETURN

      ALLOCATE (Tempc(Nhru))
      IF ( declvar('xyz_dist', 'tempc', 'nhru', Nhru, 'real',
     +     'HRU adjusted temperature for timestep < 24',
     +     'degrees Celsius',
     +     Tempc).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'basin_tmax', 'one', 1, 'real',
     +     'Basin area-weighted daily maximum temperature',
     +     'degrees',
     +     Basin_tmax).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'basin_tmin', 'one', 1, 'real',
     +     'Basin area-weighted daily minimum temperature',
     +     'degrees',
     +     Basin_tmin).NE.0 ) RETURN

      ALLOCATE (Hru_ppt(Nhru))
      IF ( declvar('xyz_dist', 'hru_ppt', 'nhru', Nhru, 'real',
     +     'Adjusted precipitation on each HRU',
     +     'inches',
     +     Hru_ppt).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'basin_ppt', 'one', 1, 'real',
     +     'Area weighted adjusted average precip for basin',
     +     'inches',
     +     Basin_ppt).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'basin_temp', 'one', 1, 'real',
     +     'Basin area-weighted temperature for timestep < 24',
     +     'degrees',
     +     Basin_temp).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'basin_rain', 'one', 1, 'real',
     +     'Area weighted adjusted average rain for basin',
     +     'inches',
     +     Basin_rain).NE.0 ) RETURN

      IF ( declvar('xyz_dist', 'basin_snow', 'one', 1, 'real',
     +     'Area weighted adjusted average snow for basin',
     +     'inches',
     +     Basin_snow).NE.0 ) RETURN

! DANGER - Not sure what to do about this one.  For right now
!          I'm setting basin_ppt and basin_obs_ppt to the same
!          variable.  In the precip_prms module, basin_obs_ppt
!          seems to be the area weighted precip average before
!          the correction factor is applied.  In this module,
!          the correction "error" is applied to the station
!          precip rather than the hru precip.

      IF ( declvar('xyz_dist', 'basin_obs_ppt', 'one', 1, 'real',
     +     'Area weighted measured average precip for basin',
     +     'inches',
     +     Basin_ppt).NE.0 ) RETURN

      ALLOCATE (Newsnow(Nhru))
      IF ( declvar('xyz_dist', 'newsnow', 'nhru', Nhru, 'integer',
     +     'New snow on HRU (0=no; 1=yes)',
     +     'none',
     +     Newsnow).NE.0 ) RETURN

      ALLOCATE (Hru_rain(Nhru))
      IF ( declvar('xyz_dist', 'hru_rain', 'nhru', Nhru, 'real',
     +     'Computed rain on each HRU',
     +     'inches',
     +     Hru_rain).NE.0 ) RETURN

      ALLOCATE (Hru_snow(Nhru))
      IF ( declvar('xyz_dist', 'hru_snow', 'nhru', Nhru, 'real',
     +     'Computed snow on each HRU',
     +     'inches',
     +     Hru_snow).NE.0 ) RETURN

      ALLOCATE (Pptmix(Nhru))
      IF ( declvar('xyz_dist', 'pptmix', 'nhru', Nhru, 'integer',
     +     'Precipitation mixture (0=no; 1=yes)',
     +     'none',
     +     Pptmix).NE.0 ) RETURN

      ALLOCATE (Prmx(Nhru))
      IF ( declvar('xyz_dist', 'prmx', 'nhru', Nhru, 'real',
     +     'Proportion of rain in a mixed event',
     +     'decimal fraction',
     +     Prmx).NE.0 ) RETURN

      ALLOCATE (Tmax_rain_sta(Nrain))
      IF ( declvar('xyz_dist', 'tmax_rain_sta', 'nrain', Nrain, 'real',
     +     'Maximum temperature distributed to the precip stations',
     +     'degrees F',
     +     Tmax_rain_sta).NE.0 ) RETURN

      ALLOCATE (Tmin_rain_sta(Nrain))
      IF ( declvar('xyz_dist', 'tmin_rain_sta', 'nrain', Nrain, 'real',
     +     'Minimum temperature distributed to the precip stations',
     +     'degrees F',
     +     Tmin_rain_sta).NE.0 ) RETURN

! declare parameters
      ALLOCATE (MRUelev(Nhru))
      IF ( declparam('xyz_dist', 'hru_elev', 'nhru', 'real',
     +     '0.', '-300.', '30000',
     +     'HRU mean elevation', 'Mean elevation for each HRU',
     +     'elev_units').NE.0 ) RETURN

      ALLOCATE (MRUx(Nhru))
      IF ( declparam('xyz_dist', 'hru_x', 'nhru', 'real',
     +     '0.', '-10000000.', '10000000.',
     +     'X for each HRU (albers)', 'X for each HRU (albers)',
     +     'meters').NE.0 ) RETURN

      ALLOCATE (MRUy(Nhru))
      IF ( declparam('xyz_dist', 'hru_y', 'nhru', 'real',
     +     '0.', '-10000000.', '10000000.',
     +     'Y for each HRU (albers)', 'Y for each HRU (albers)',
     +     'meters').NE.0 ) RETURN

      ALLOCATE (Max_lapse(MAXLAPSE, MAXMO))
      IF ( declparam('xyz_dist', 'max_lapse', 'nlapse,nmonths', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Monthly maximum temperature lapse rate for each direction',
     +     'Monthly maximum temperature lapse rate for each direction'//
     +     ' (X, Y, and Z) for each month, January to December',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Min_lapse(MAXLAPSE, MAXMO))
      IF ( declparam('xyz_dist', 'min_lapse', 'nlapse,nmonths', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Monthly minimum temperature lapse rate for each direction',
     +     'Monthly minimum temperature lapse rate for each direction'//
     +     ' (X, Y, and Z) for each month, January to December',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Ppt_lapse(MAXLAPSE, MAXMO))
      IF ( declparam('xyz_dist', 'ppt_lapse', 'nlapse,nmonths', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'Precipitation lapse rate',
     +     'Precipitation lapse rate for each direction (X, Y, and Z)'//
     +     ' for each month, January to December',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Temp_STAelev(Ntemp))
      IF ( declparam('xyz_dist', 'tsta_elev', 'ntemp', 'real',
     +     '0', '-300.', '30000.',
     +     'Temperature station elevation',
     +     'Elevation of each temperature measurement station',
     +     'elev_units').NE.0 ) RETURN

      ALLOCATE (Temp_STAx(Ntemp))
      IF ( declparam('xyz_dist', 'tsta_x', 'ntemp', 'real',
     +     '0.', '-10000000.', '10000000.',
     +     'X for each temperature station (albers)',
     +     'Longitude (X) for each temperature measurement station in'//
     +     ' albers projection',
     +     'meters').NE.0 ) RETURN

      ALLOCATE (Temp_STAy(Ntemp))
      IF ( declparam('xyz_dist', 'tsta_y', 'ntemp', 'real',
     +     '0.', '-10000000.', '10000000.',
     +     'Y for each temperature station (albers)',
     +     'Latitude (Y) for each temperature measurement station in'//
     +     ' albers projection',
     +     'meters').NE.0 ) RETURN

      ALLOCATE (Rain_STAelev(Nrain))
      IF ( declparam('xyz_dist', 'psta_elev', 'nrain', 'real',
     +     '0', '-300.', '30000.',
     +     'Precip station elevation',
     +     'Elevation of each precip measurement station',
     +     'elev_units').NE.0 ) RETURN

      ALLOCATE (Rain_STAx(Nrain))
      IF ( declparam('xyz_dist', 'psta_x', 'nrain', 'real',
     +     '0.', '-10000000.', '10000000.',
     +     'X for each precip station (albers)',
     +     'Longitude (X) for each precipitation measurement station'//
     +     ' in albers projection',
     +     'meters').NE.0 ) RETURN

      ALLOCATE (Rain_STAy(Nrain))
      IF ( declparam('xyz_dist', 'psta_y', 'nrain', 'real',
     +     '0.', '-10000000.', '10000000.',
     +     'Y for each precip station (albers)',
     +     'Latitude (Y) for each precipitation measurement station'//
     +     ' in albers projection',
     +     'meters').NE.0 ) RETURN

      ALLOCATE (Tsta_nuse(Ntemp), Temp_nuse(Ntemp))
      IF ( declparam('xyz_dist', 'tsta_nuse', 'ntemp', 'integer',
     +     '1', '0', '1',
     +     '0 = station not used; 1 = station used',
     +     '0 = station not used; 1 = station used',
     +     'none').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'solrad_elev', 'one', 'real',
     +     '1000.0', '0.0', '10000.0',
     +     'Elevation of the solrad station used for the DD curves.',
     +     'Elevation of the solrad station used for the DD curves.',
     +     'meters').NE.0 ) RETURN

      ALLOCATE (Psta_nuse(Nrain), Rain_nuse(Nrain))
      IF ( declparam('xyz_dist', 'psta_nuse', 'nrain', 'integer',
     +     '1', '0', '1',
     +     'The subset of precip stations used in the distribution'//
     +     ' regression (0=station not used; 1=station used)',
     +     'The subset of precip stations used in the distribution'//
     +     ' regression (0=station not used; 1=station used)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Psta_freq_nuse(Nrain))
      IF ( declparam('xyz_dist', 'psta_freq_nuse', 'nrain', 'integer',
     +     '1', '0', '1',
     +     'The subset of precip stations used to determine if'//
     +     ' there is precip in the basin (0=station not used;'//
     +     ' 1=station used)',
     +     'The subset of precip stations used to determine if'//
     +     ' there is precip in the basin (0=station not used;'//
     +     ' 1=station used)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('xyz_dist', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'precip_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for observed precipitation',
     +     'Units for observed precipitation (0=inches; 1=mm)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'temp_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for observed temperature',
     +     'Units for observed temperature (0=Fahrenheit; 1=Celsius)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (TmaxMTH(Ntemp, MAXMO))
      IF ( declparam('xyz_dist', 'tsta_month_max', 'ntemp,nmonths',
     +     'real',
     +     '0.', '-100.', '200.',
     +     'Average monthly maximum temp at each station',
     +     'Average maximum temperature at each station for each'//
     +     ' month, January to December',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (TminMTH(Ntemp, MAXMO))
      IF ( declparam('xyz_dist', 'tsta_month_min', 'ntemp,nmonths',
     +     'real',
     +     '0.', '-100.', '200.',
     +     'Average monthly minimum temp at each station',
     +     'Average minimum temperature at each station for each'//
     +     ' month, January to December',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (PptMTH(Nrain, MAXMO))
      IF ( declparam('xyz_dist', 'psta_month_ppt', 'nrain,nmonths',
     +     'real',
     +     '0.', '0.', '200.',
     +     'Average monthly precip at each station',
     +     'Average precipitation at each station for each month,'//
     +     ' January to December',
     +     'precip_units').NE.0 ) RETURN

      ALLOCATE (Adjust_snow(MAXMO))
      IF ( declparam('xyz_dist', 'adjust_snow', 'nmonths', 'real',
     +     '0.01', '0.0', '1.0',
     +     'Downscaling % adjustment for snow',
     +     'Downscaling % adjustment for snow',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Adjust_rain(MAXMO))
      IF ( declparam('xyz_dist', 'adjust_rain', 'nmonths', 'real',
     +     '0.01', '0.0', '1.0',
     +     'Downscaling % adjustment for rain',
     +     'Downscaling % adjustment for rain',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Tmax_allrain(MAXMO))
      IF ( declparam('xyz_dist', 'tmax_allrain', 'nmonths', 'real',
     +     '40.', '0.', '90.',
     +     'Precip all rain if HRU max temperature above this value',
     +     'If maximum temperature of an HRU is greater than or equal'//
     +     ' to this value (for each month, January to December),'//
     +     ' precipitation is assumed to be rain,'//
     +     ' in deg C or F, depending on units of data',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE ( Rain_code(MAXMO))
      IF ( declparam('xyz_dist', 'rain_code', 'nmonths', 'integer',
     +     '2', '1', '5',
     +     'Code indicating rule for precip station use',
     +     'Code indicating rule for precip station use'//
     +     ' (1=only precip if the regression stations have precip;'//
     +     ' 2=only precip if any station in the basin has precip;'//
     +     ' 3=precip if xyz says so;'//
     +     ' 4=only precip if rain_day variable is set to 1;'//
     +     ' 5=only precip if psta_freq_nuse stations see precip)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'tmax_allsnow', 'one', 'real',
     +     '32.', '-10.', '40.',
     +     'Precip all snow if HRU max temperature below this value',
     +     'If HRU maximum temperature is less than or equal to this'//
     +     ' value, precipitation is assumed to be snow,'//
     +     ' in deg C or F, depending on units of data',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Adjmix_rain(MAXMO))
      IF ( declparam('xyz_dist', 'adjmix_rain', 'nmonths', 'real',
     +     '1.', '0.', '3.',
     +     'Adjustment factor for rain in a rain/snow mix',
     +     'Monthly factor to adjust rain proportion in a mixed'//
     +     ' rain/snow event',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'x_add', 'one', 'real',
     +     '0.0', '-10000000.0', '10000000.0',
     +     'X additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for the longitude (X) coordinate',
     +     'meters').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'x_div', 'one', 'real',
     +     '0.0', '-10000000.0', '10000000.0',
     +     'X divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for the longitude (X) coordinate',
     +     'meters').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'y_add', 'one', 'real',
     +     '0.0', '-10000000.0', '10000000.0',
     +     'Y additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for the latitude (Y) coordinate',
     +     'meters').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'y_div', 'one', 'real',
     +     '0.0', '-10000000.0', '10000000.0',
     +     'Y divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for the latitude (Y) coordinate',
     +     'meters').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'z_add', 'one', 'real',
     +     '0.0', '-10000000.0', '10000000.0',
     +     'Z additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for the elevation (Z) coordinate',
     +     'meters').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'z_div', 'one', 'real',
     +     '0.0', '-10000000.0', '10000000.0',
     +     'Z divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for the elevation (Z) coordinate',
     +     'meters').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'tmax_add', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Max temp additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for maximum temperature',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'tmax_div', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Max temp divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for maximum temperature',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'tmin_add', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Min temp additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for minimum temperature',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'tmin_div', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Min temp divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for minimum temperature',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'ppt_add', 'one', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'Precipitation additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for precipitation',
     +     'precip_units').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'ppt_div', 'one', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'Precipitation divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for precipitation',
     +     'precip_units').NE.0 ) RETURN

      ALLOCATE (Tmax_adj(Nhru))
      IF ( declparam('xyz_dist', 'tmax_adj', 'nhru', 'real',
     +     '0.0', '-10.', '10.0',
     +     'HRU maximum temperature adjustment',
     +     'Adjustment to maximum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Tmin_adj(Nhru))
      IF ( declparam('xyz_dist', 'tmin_adj', 'nhru', 'real',
     +     '0.0', '-10.', '10.0',
     +     'HRU minimum temperature adjustment',
     +     'Adjustment to minimum temperature for each HRU, estimated'//
     +     ' based on slope and aspect',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'elev_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Elevation units flag',
     +     'Flag to indicate the units of the elevation values'//
     +     ' (0=feet; 1=meters)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'basin_tsta_hru', 'one', 'integer',
     +     '0', 'bounded', 'nhru',
     +     'Index of HRU to use for basin temperature',
     +     'Index of HRU to use for basin temperature',
     +     'none').NE.0 ) RETURN

      IF ( declparam('xyz_dist', 'conv_flag', 'one', 'integer',
     +     '0', '0', '2',
     +     'Elevation conversion flag',
     +     'Elevation conversion flag (0=none, 1=ft to m, 2=m to ft)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_route_order(Nhru))
      ALLOCATE (Meantmax(MAXMO), Meantmin(MAXMO), Meanppt(MAXMO))
      ALLOCATE (Temp_meanx(MAXMO), Temp_meany(MAXMO), Temp_meanz(MAXMO))
      ALLOCATE (Rain_meanx(MAXMO), Rain_meany(MAXMO), Rain_meanz(MAXMO))

      xyzdecl = 0
      END FUNCTION xyzdecl

!***********************************************************************
!     xyzinit - Initialize xyz_dist module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION xyzinit()
      USE PRMS_XYZ_DIST
      IMPLICIT NONE
! Functions
      INCLUDE 'fmodules.inc'
      EXTERNAL mean_by_month
! Local Variables
      INTEGER :: i, m, ii
!***********************************************************************
      xyzinit = 1

! Initialize declared variables
      IF ( getstep().EQ.0 ) THEN
        Tmaxf = 0.0
        Tminf = 0.0
        Tavgf = 0.0
        Tmaxc = 0.0
        Tminc = 0.0
        Tavgc = 0.0
        Tempf = 0.0
        Tempc = 0.0
        Tmax_rain_sta = 0.0
        Tmin_rain_sta = 0.0
        Is_rain_day = 0
        Basin_ppt = 0.0
        Basin_rain = 0.0
        Basin_snow = 0.0
        Hru_ppt = 0.0
        Hru_rain = 0.0
        Hru_snow = 0.0
        Prmx = 0.0
        Pptmix = 0
        Newsnow = 0
        Solrad_tmax = 0.0
        Solrad_tmin = 0.0
        Basin_temp = 0.0
        Basin_tmax = 0.0
        Basin_tmin = 0.0
      ENDIF

      IF ( getparam ('xyz_dist', 'solrad_elev', 1, 'real', Solrad_elev)
     +     .NE.0) RETURN

      IF ( getparam('xyz_dist', 'hru_x', Nhru, 'real', MRUx)
     +     .NE.0) RETURN

      IF ( getparam('xyz_dist', 'hru_y', Nhru, 'real', MRUy)
     +     .NE.0) RETURN

      IF ( getparam('xyz_dist', 'hru_elev', Nhru, 'real',
     +     MRUelev).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'max_lapse', MAXLAPSE*MAXMO, 'real',
     +     Max_lapse).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'min_lapse', MAXLAPSE*MAXMO, 'real',
     +     Min_lapse).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'ppt_lapse', MAXLAPSE*MAXMO, 'real',
     +     Ppt_lapse).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tsta_x', Ntemp, 'real', Temp_STAx)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tsta_y', Ntemp, 'real', Temp_STAy)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tsta_elev', Ntemp, 'real',Temp_STAelev)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'psta_x', Nrain, 'real', Rain_STAx)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'psta_y', Nrain, 'real', Rain_STAy)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'psta_elev', Nrain, 'real',Rain_STAelev)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tsta_nuse', Ntemp, 'integer',Tsta_nuse)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'psta_nuse', Nrain, 'integer',Psta_nuse)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'psta_freq_nuse', Nrain, 'integer',
     +     Psta_freq_nuse).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'temp_units', 1, 'integer', Temp_units)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tsta_month_min', Ntemp*MAXMO, 'real',
     +     TminMTH).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tsta_month_max', Ntemp*MAXMO, 'real',
     +     TmaxMTH).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'psta_month_ppt', Nrain*MAXMO, 'real',
     +     PptMTH).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'adjust_snow', MAXMO, 'real',
     +     Adjust_snow).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'adjust_rain', MAXMO, 'real',
     +     Adjust_rain).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmax_allrain', MAXMO, 'real',
     +     Tmax_allrain).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmax_allsnow', 1, 'real', Tmax_allsnow)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'adjmix_rain', MAXMO, 'real',
     +     Adjmix_rain).NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'z_add', 1, 'real', Z_add)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'z_div', 1, 'real', Z_div)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'x_add', 1, 'real', X_add)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'x_div', 1, 'real', X_div)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'y_add', 1, 'real', Y_add)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'y_div', 1, 'real', Y_div)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmax_add', 1, 'real', Tmax_add)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmax_div', 1, 'real', Tmax_div)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmin_add', 1, 'real', Tmin_add)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmin_div', 1, 'real', Tmin_div)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'ppt_add', 1, 'real', Ppt_add)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'ppt_div', 1, 'real', Ppt_div)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmax_adj', Nhru, 'real', Tmax_adj)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'tmin_adj', Nhru, 'real', Tmin_adj)
     +     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'rain_code', MAXMO, 'integer',Rain_code)
     +     .NE.0 ) RETURN

!dbg  IF ( get var('basin', 'prt_debug', 1, 'integer', Prt_debug)
!dbg &     .NE.0 ) RETURN

      IF ( getparam('xyz_dist', 'conv_flag', 1, 'integer', Conv_flag)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

!
! Compute basin centroid
!
      Basin_centroid_x = 0.0
      Basin_centroid_y = 0.0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        Basin_centroid_x = Basin_centroid_x + (Hru_area(i)*MRUx(i))
        Basin_centroid_y = Basin_centroid_y + (Hru_area(i)*MRUy(i))
      ENDDO
      Basin_centroid_x = Basin_centroid_x*Basin_area_inv
      Basin_centroid_y = Basin_centroid_y*Basin_area_inv
!
! convert elevations from feet to meters
!
      IF ( Conv_flag.EQ.1 ) THEN
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          MRUelev(i) = MRUelev(i)*FEET2METERS
        ENDDO
        DO i = 1, Ntemp
          Temp_STAelev(i) = Temp_STAelev(i)*FEET2METERS
        ENDDO
        DO i = 1, Nrain
          Rain_STAelev(i) = Rain_STAelev(i)*FEET2METERS
        ENDDO
      ENDIF
!
! transform X and Y
!
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        MRUelev(i) = (MRUelev(i)+Z_add)/Z_div
        MRUx(i) = (MRUx(i)+X_add)/X_div
        MRUy(i) = (MRUy(i)+Y_add)/Y_div
      ENDDO

      Temp_nsta = 0
      DO i = 1, Ntemp
!       temp_STAelev(i) = (temp_STAelev(i)+Z_add)/Z_div
!       temp_STAx(i) = (temp_STAx(i)+X_add)/X_div
!       temp_STAy(i) = (temp_STAy(i)+Y_add)/Y_div
        IF ( Tsta_nuse(i).EQ.1 ) THEN
          Temp_nsta = Temp_nsta + 1
          Temp_nuse(Temp_nsta) = i
        ENDIF
      ENDDO

      Rain_nsta = 0
      DO i = 1, Nrain
!       rain_STAelev(i) = (rain_STAelev(i)+Z_add)/Z_div
!       rain_STAx(i) = (rain_STAx(i)+X_add)/X_div
!       rain_STAy(i) = (rain_STAy(i)+Y_add)/Y_div
        IF ( Psta_nuse(i).EQ.1 ) THEN
          Rain_nsta = Rain_nsta + 1
          Rain_nuse(Rain_nsta) = i
        ENDIF
      ENDDO
!
! calculate the station mean by month
!
      DO m = 1, MAXMO
        CALL mean_by_month(PptMTH(1, m), TminMTH(1, m), TmaxMTH(1, m),
     +                     Meanppt(m), Meantmax(m), Meantmin(m),
     +                     Rain_meanx(m), Rain_meany(m), Rain_meanz(m),
     +                     Temp_meanx(m), Temp_meany(m), Temp_meanz(m))
!dbg    IF ( Prt_debug.EQ.8 ) THEN
!dbg      PRINT *, m, Temp_meanx(m), Temp_meany(m), Temp_meanz(m)
!dbg      PRINT *, m, Rain_meanx(m), Rain_meany(m), Rain_meanz(m)
!dbg      PRINT *, Max_lapse(1, m), Max_lapse(2, m), Max_lapse(3, m)
!dbg      PRINT *, Min_lapse(1, m), Min_lapse(2, m), Min_lapse(3, m)
!dbg      PRINT *, Ppt_lapse(1, m), Ppt_lapse(2, m), Ppt_lapse(3, m)
!dbg    ENDIF
      ENDDO

      xyzinit = 0
      END FUNCTION xyzinit

!***********************************************************************
!     xyzrun - Temperature calculation
!               calculates daily max and min temperature
!               using data from available stations
!               Outputs a daily max and min Temperature by HRU elevation
!***********************************************************************
      INTEGER FUNCTION xyzrun()
      USE PRMS_XYZ_DIST
      IMPLICIT NONE
! Functions
      INCLUDE 'fmodules.inc'
      EXTERNAL xyz_temp_run, xyz_rain_run
! Local Variables
      INTEGER :: im
!***********************************************************************
      xyzrun = 1

      CALL dattim('now', Nowtime)
      im = Nowtime(2)

      IF ( deltim().LT.23.999D0 ) THEN
        Storm = 1
      ELSE
        Storm = 0
      ENDIF

      CALL xyz_temp_run(Max_lapse(1, im), Min_lapse(1, im), Meantmax(im)
     +                  , Meantmin(im), Temp_meanx(im), Temp_meany(im),
     +                  Temp_meanz(im))

      CALL xyz_rain_run(Ppt_lapse(1, im), Rain_meanx(im), Rain_meany(im)
     +                  , Rain_meanz(im), Meanppt(im), Tmax_allrain(im),
     +                  Adjmix_rain(im), Rain_code(im), Adjust_snow(im),
     +                  Adjust_rain(im))

      xyzrun = 0

      END FUNCTION xyzrun

!***********************************************************************
!     xyz_temp_run - Temperature calculation
!               calculates daily max and min temperature
!               using data from available stations
!               Outputs a daily max and min Temperature by HRU elevation
!***********************************************************************
      SUBROUTINE xyz_temp_run(Max_lapse, Min_lapse, Meantmax, Meantmin,
     +                        Temp_meanx, Temp_meany, Temp_meanz)
      USE PRMS_XYZ_DIST, ONLY:Nrain, Ntemp, MRUx, MRUy, MRUelev, Tmaxf,
     +    Tminf, Tavgf, Tmaxc, Tminc, Tavgc, Basin_tmax, Basin_tmin,
     +    Basin_area_inv, Hru_area, Tmax_rain_sta, Tmin_rain_sta,
     +    Temp_nuse, Tmin_add, Tmin_div, Tmax_add, Tmax_div, Tmin_adj,
     +    Tmax_adj, Temp_units, Temp_nsta, X_div, Y_div, Z_div, X_add,
     +    Y_add, Z_add, Temp_STAx, Storm, Temp_STAy, Temp_STAelev, Tmax,
     +    Tmin, Rain_STAelev, Rain_STAx, Rain_STAy, Basin_centroid_y,
     +    Basin_centroid_x, Solrad_tmax, Solrad_tmin, Solrad_elev,
     +    NEARZERO, Basin_temp, Tempc, Tempf, Basin_tsta_hru,
     +    Active_hrus, Hru_route_order
!dbg  USE PRMS_XYZ_DIST, ONLY:Prt_debug
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC FLOAT, ABS
      REAL, EXTERNAL :: f_to_c_xyz, c_to_f_xyz
! Arguments
!   Declared Parameters
      REAL, INTENT(IN) :: Max_lapse(MAXLAPSE), Min_lapse(MAXLAPSE)
!   Private Variables
      REAL, INTENT(IN) :: Meantmax, Meantmin
      REAL, INTENT(IN) :: Temp_meanx, Temp_meany, Temp_meanz
! Local Variables
      INTEGER :: i, j, ntmin, ntmax, ii
      REAL :: intmax, intmin, xmax, xmin, intercept, x1, stmax, stmin
      REAL :: sumtmin, sumtmax, xtmin, xtmax, ytmin
      REAL :: ytmax, ztmin, ztmax, tmax_hru, tmin_hru
      REAL :: zrain, xrain, yrain
      REAL :: zsolrad, xsolrad, ysolrad
      REAL :: maxlapse1, maxlapse2, maxlapse3
      REAL :: minlapse1, minlapse2, minlapse3
!***********************************************************************
      IF ( getvar('obs', 'tmax', Ntemp, 'real', Tmax).NE.0 ) RETURN
      IF ( getvar('obs', 'tmin', Ntemp, 'real', Tmin).NE.0 ) RETURN

      sumtmin = 0.0
      sumtmax = 0.0
      ntmin = 0
      ntmax = 0
      xtmin = 0.0
      xtmax = 0.0
      ytmin = 0.0
      ytmax = 0.0
      ztmin = 0.0
      ztmax = 0.0
      maxlapse1 = Max_lapse(1)
      maxlapse2 = Max_lapse(2)
      maxlapse3 = Max_lapse(3)
      minlapse1 = Min_lapse(1)
      minlapse2 = Min_lapse(2)
      minlapse3 = Min_lapse(3)

! Do not Transform the coordinates of the temp stations
! until after summing

      DO j = 1, Temp_nsta
        i = Temp_nuse(j)

        IF ( Tmax(i).GT.-55.0 ) THEN
          ntmax = ntmax + 1
!         sumtmax = sumtmax + ((Tmax(i)+Tmax_add)/Tmax_div)
          sumtmax = sumtmax + Tmax(i)
          xtmax = xtmax + Temp_STAx(i)
          ytmax = ytmax + Temp_STAy(i)
          ztmax = ztmax + Temp_STAelev(i)
        ENDIF

        IF ( Tmin(i).GT.-55.0 ) THEN
          ntmin = ntmin + 1
!         sumtmin = sumtmin + ((Tmin(i)+Tmin_add)/Tmin_div)
          sumtmin = sumtmin + Tmin(i)
          xtmin = xtmin + Temp_STAx(i)
          ytmin = ytmin + Temp_STAy(i)
          ztmin = ztmin + Temp_STAelev(i)
        ENDIF
      ENDDO
!
! calculate means
!
      IF ( ntmin.GT.0 ) THEN
        stmin = sumtmin/FLOAT(ntmin)
        xtmin = xtmin/FLOAT(ntmin)
        ytmin = ytmin/FLOAT(ntmin)
        ztmin = ztmin/FLOAT(ntmin)
        stmin = (stmin+Tmin_add)/Tmin_div
        xtmin = (xtmin+X_add)/X_div
        ytmin = (ytmin+Y_add)/Y_div
        ztmin = (ztmin+Z_add)/Z_div
      ELSE
! these are already transformed
        stmin = Meantmin
        xtmin = Temp_meanx
        ytmin = Temp_meany
        ztmin = Temp_meanz
      ENDIF

      IF ( ntmax.GT.0 ) THEN
        stmax = sumtmax/FLOAT(ntmax)
        xtmax = xtmax/FLOAT(ntmax)
        ytmax = ytmax/FLOAT(ntmax)
        ztmax = ztmax/FLOAT(ntmax)
        stmax = (stmax+Tmax_add)/Tmax_div
        xtmax = (xtmax+X_add)/X_div
        ytmax = (ytmax+Y_add)/Y_div
        ztmax = (ztmax+Z_add)/Z_div
      ELSE
! these are already transformed
        stmax = Meantmax
        xtmax = Temp_meanx
        ytmax = Temp_meany
        ztmax = Temp_meanz
      ENDIF
!
! adjust the values if not using all the stations
!
      IF ( ABS(Temp_meanz-ztmin).GT.NEARZERO ) THEN
        intercept = stmin - minlapse3*ztmin - minlapse1*xtmin -
     +              minlapse2*ytmin
        stmin = minlapse3*Temp_meanz + minlapse1*Temp_meanx +
     +          minlapse2*Temp_meany + intercept
      ENDIF
!
      IF ( ABS(Temp_meanz-ztmax).GT.NEARZERO ) THEN
        intercept = stmax - maxlapse3*ztmax - maxlapse1*xtmax -
     +              maxlapse2*ytmax
        stmax = maxlapse3*Temp_meanz + maxlapse1*Temp_meanx +
     +          maxlapse2*Temp_meany + intercept
      ENDIF
!
! now redistribute based on lapse rates redistribute to HRUs
!
      xmax = stmax
      xmin = stmin
!     --------------
      intmax = xmax -
     +         (maxlapse3*Temp_meanz) -
     +         (maxlapse1*Temp_meanx) -
     +         (maxlapse2*Temp_meany)
!
      intmin = xmin -
     +         (minlapse3*Temp_meanz) -
     +         (minlapse1*Temp_meanx) -
     +         (minlapse2*Temp_meany)
!
      Basin_tmax = 0.0
      Basin_tmin = 0.0
      Basin_temp = 0.0

!
!  Compute maximum temperature at XY centroid of basin at the elevation
!  of the solrad station used to develop the DD solrad curves.
!
      zsolrad = (Solrad_elev+Z_add)/Z_div
      xsolrad = (Basin_centroid_x+X_add)/X_div
      ysolrad = (Basin_centroid_y+Y_add)/Y_div
      Solrad_tmax = ((maxlapse1*xsolrad)+
     +              (maxlapse2*ysolrad)+
     +              (maxlapse3*zsolrad+intmax))*Tmax_div - Tmax_add
      Solrad_tmin = ((minlapse1*xsolrad)+
     +              (minlapse2*ysolrad)+
     +              (minlapse3*zsolrad+intmin))*Tmin_div - Tmin_add

!
!  Compute temperatures at precip stations.
!
      DO i = 1, Nrain
        zrain = (Rain_STAelev(i)+Z_add)/Z_div
        xrain = (Rain_STAx(i)+X_add)/X_div
        yrain = (Rain_STAy(i)+Y_add)/Y_div
        Tmax_rain_sta(i) = (maxlapse1*xrain) +
     +                     (maxlapse2*yrain) +
     +                     (maxlapse3*zrain) + intmax

        Tmin_rain_sta(i) = (minlapse1*xrain) +
     +                     (minlapse2*yrain) +
     +                     (minlapse3*zrain) + intmin

        Tmax_rain_sta(i) = (Tmax_rain_sta(i)*Tmax_div) - Tmax_add
        Tmin_rain_sta(i) = (Tmin_rain_sta(i)*Tmin_div) - Tmin_add

        IF ( Tmax_rain_sta(i).LT.Tmin_rain_sta(i) ) THEN
          x1 = Tmax_rain_sta(i)
          Tmax_rain_sta(i) = Tmin_rain_sta(i)
          Tmin_rain_sta(i) = x1
        ENDIF

      ENDDO

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
!
!  At this point, all temperatures are in the units
!  of the temperatures in the data file.
!
        tmax_hru = (maxlapse1*MRUx(i)) +
     +             (maxlapse2*MRUy(i)) +
     +             (maxlapse3*MRUelev(i)) + intmax

        tmin_hru = (minlapse1*MRUx(i)) +
     +             (minlapse2*MRUy(i)) +
     +             (minlapse3*MRUelev(i)) + intmin
!
!  Transform back
!
        tmax_hru = (tmax_hru*Tmax_div) - Tmax_add
        tmin_hru = (tmin_hru*Tmin_div) - Tmin_add
!
!  Temp adjustment by HRU
!
        tmax_hru = tmax_hru + Tmax_adj(i)
        tmin_hru = tmin_hru + Tmin_adj(i)

!
!  If max is less than min, switch
!
        IF ( tmax_hru.LT.tmin_hru ) THEN
          x1 = tmax_hru
          tmax_hru = tmin_hru
          tmin_hru = x1
        ENDIF
!
!  Now sort out units.
!
        IF ( Temp_units.EQ.0 ) THEN
!         degrees F
          Tmaxf(i) = tmax_hru
          Tminf(i) = tmin_hru
          Tavgf(i) = (tmax_hru+tmin_hru)*0.5
          Tmaxc(i) = f_to_c_xyz(Tmaxf(i))
          Tminc(i) = f_to_c_xyz(Tminf(i))
          Tavgc(i) = f_to_c_xyz(Tavgf(i))
          IF ( Storm.EQ.1 ) THEN
            Tempf(j) = Tavgf(i)
            Tempc(j) = f_to_c_xyz(Tempf(j))
            Basin_temp = Basin_temp + Tempf(j)*Hru_area(i)
          ENDIF
        ELSE
!         degrees C
          Tmaxc(i) = tmax_hru
          Tminc(i) = tmin_hru
          Tavgc(i) = (tmax_hru+tmin_hru)*0.5
          Tmaxf(i) = c_to_f_xyz(Tmaxc(i))
          Tminf(i) = c_to_f_xyz(Tminc(i))
          Tavgf(i) = c_to_f_xyz(Tavgc(i))
          IF ( Storm.EQ.1 ) THEN
            Tempc(j) =  Tavgc(i)
            Tempf(j) = c_to_f_xyz(Tempc(j))
            Basin_temp = Basin_temp + Tempc(j)*Hru_area(i)
          ENDIF
        ENDIF

        Basin_tmax = Basin_tmax + (tmax_hru*Hru_area(i))
        Basin_tmin = Basin_tmin + (tmin_hru*Hru_area(i))
      ENDDO

      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv
      IF ( Storm.EQ.1 ) Basin_temp = Basin_temp*Basin_area_inv

      IF ( Basin_tsta_hru.GT.0 ) THEN
        IF ( Temp_units.EQ.0 ) THEN
!         degrees F
          Solrad_tmax = Tmaxf(Basin_tsta_hru)
          Solrad_tmin = Tminf(Basin_tsta_hru)
        ELSE
!         degrees C
          Solrad_tmax = Tmaxc(Basin_tsta_hru)
          Solrad_tmin = Tminc(Basin_tsta_hru)
        ENDIF
      ENDIF

!dbg  IF ( Prt_debug.EQ.8 ) THEN
!dbg    PRINT 9001, Tmaxf
!dbg    PRINT 9002, Tminf
!dbg    PRINT 9003, Tavgf
!dbg    PRINT *, 'basin_tmax ', Basin_tmax
!dbg    PRINT *, 'basin_tmin ', Basin_tmin
!dbg  ENDIF

!dbg 9001 FORMAT (' tmaxf ', 10F7.3)
!dbg 9002 FORMAT (' tminf ', 10F7.3)
!dbg 9003 FORMAT (' tavgf ', 10F7.3)

      END SUBROUTINE xyz_temp_run

!***********************************************************************
      !rsr?? do we want any storm stuff in here?
!***********************************************************************
      SUBROUTINE xyz_rain_run(Ppt_lapse, Rain_meanx, Rain_meany,
     +                        Rain_meanz, Meanppt, Tmax_allrain,
     +                        Adjmix_rain, Rain_code, Adjust_snow,
     +                        Adjust_rain)
      USE PRMS_XYZ_DIST, ONLY:Tmaxf, Tminf, Hru_area, Hru_ppt, Hru_rain,
     +    Hru_snow, MRUx, MRUy, MRUelev, Basin_area_inv, Basin_rain,
     +    Rain_STAx, Rain_STAy, Rain_STAelev, Rain_nuse, Tmax_allsnow,
     +    Basin_ppt, Newsnow, Pptmix, Prmx, Ppt_add, Ppt_div, Rain_nsta,
     +    Nrain, Nform, Tmax_rain_sta, Tmin_rain_sta, Is_rain_day,
     +    Psta_freq_nuse, Rain_day, Form_data, Ppt, X_div, Y_div,
     +    Z_div, X_add, Y_add, Z_add, Nowtime, NEARZERO, Route_on,
     +    Basin_snow, Active_hrus, Hru_route_order
!dbg  USE PRMS_XYZ_DIST, ONLY:Prt_debug
      IMPLICIT NONE
! Functions
      INCLUDE 'fmodules.inc'
      INTRINSIC FLOAT, ABS
! Arguments
!   Declared Parameters
      INTEGER, INTENT(IN) :: Rain_code
      REAL, INTENT(IN) :: Adjust_snow, Adjust_rain, Ppt_lapse(MAXLAPSE)
      REAL, INTENT(IN) :: Tmax_allrain, Adjmix_rain
!   Undeclared Static Variables
      REAL, INTENT(IN) :: Rain_meanx, Rain_meany, Rain_meanz, Meanppt
! Local Variables
      INTEGER :: i, j, err_chk, nppt, nsta_used, iform, ii
!dbg  INTEGER :: iy, id, im
      REAL :: intppt, intercept, sppt, sumppt, xppt, yppt, zppt
      REAL :: pptlapse1, pptlapse2, pptlapse3
!***********************************************************************
      pptlapse1 = Ppt_lapse(1)
      pptlapse2 = Ppt_lapse(2)
      pptlapse3 = Ppt_lapse(3)

      IF ( getvar('obs', 'precip', Nrain, 'real', Ppt).NE.0 ) RETURN

      IF ( Nform.GT.0 ) THEN
        IF ( getvar('obs', 'form_data', Nform, 'integer', Form_data)
     +       .NE.0 ) RETURN
        iform = Form_data(1)
      ELSE
        iform = 0
      ENDIF

      IF ( getvar('obs', 'route_on', 1, 'integer', Route_on)
     +     .NE.0 ) RETURN
!
! Code to check the rain_code parameter to determine if it is
! raining in the basin.
!
      Is_rain_day = 0
      IF ( Rain_code.EQ.1 ) THEN
        DO j = 1, Rain_nsta
          i = Rain_nuse(j)
          IF ( Ppt(i).GT.0.0 ) Is_rain_day = 1
        ENDDO

      ELSEIF ( Rain_code.EQ.2 ) THEN
        DO i = 1, Nrain
          IF ( Ppt(i).GT.0.0 ) Is_rain_day = 1
        ENDDO

      ELSEIF ( Rain_code.EQ.3 ) THEN
        Is_rain_day = 1

      ELSEIF ( Rain_code.EQ.4 ) THEN
        IF ( getvar('obs', 'rain_day', 1, 'integer', Rain_day)
     +       .NE.0 ) RETURN
        IF ( Rain_day.EQ.1 ) Is_rain_day = 1

      ELSEIF ( Rain_code.EQ.5 ) THEN
        DO i = 1, Nrain
          IF ( Psta_freq_nuse(i).EQ.1 ) THEN
            IF ( Ppt(i).GT.0.0 ) Is_rain_day = 1
          ENDIF
        ENDDO
      ENDIF

!dbg  IF ( Prt_debug.EQ.8 ) THEN
!dbg    im = Nowtime(2)
!dbg    id = Nowtime(3)
!dbg    iy = Nowtime(1)
!dbg    PRINT *, id, im, iy, '   is_rain_day = ', Is_rain_day,
!dbg &        '     uncorrected CB ppt = ', Ppt
!dbg  ENDIF
!
! add adjust_snow and adjust_rain here
!
      IF ( Rain_code.EQ.1 ) THEN
        nsta_used = Rain_nsta
      ELSE
        nsta_used = Nrain
      ENDIF

      DO j = 1, nsta_used
        IF ( Rain_code.EQ.1 ) THEN
          i = Rain_nuse(j)
        ELSE
          i = j
        ENDIF

        IF ( Ppt(i).GT.-NEARZERO ) THEN

          IF ( Tmax_rain_sta(i).LE.Tmax_allsnow ) THEN
            err_chk = 1
          ELSEIF ( Tmin_rain_sta(i).GT.Tmax_allsnow .OR.
     +             Tmax_rain_sta(i).GE.Tmax_allrain ) THEN
            err_chk = 0
          ELSE
            err_chk = 1
          ENDIF

          IF ( err_chk.EQ.1 ) THEN
            Ppt(i) = (Ppt(i)*Adjust_snow) + Ppt(i)
          ELSE
            Ppt(i) = (Ppt(i)*Adjust_rain) + Ppt(i)
          ENDIF
        ENDIF

      ENDDO

!dbg  IF ( Prt_debug.EQ.8 ) PRINT *, id, im, iy,
!dbg &                            '     corrected CB precip = ', Ppt

      sumppt = 0.0
      nppt = 0
      xppt = 0.0
      yppt = 0.0
      zppt = 0.0

      DO j = 1, Rain_nsta
        i = Rain_nuse(j)
        IF ( Ppt(i).GE.0.0 ) THEN
          nppt = nppt + 1
!         sumppt = sumppt + ((Ppt(i)+Ppt_add)/Ppt_div)
          sumppt = sumppt + Ppt(i)
          xppt = xppt + Rain_STAx(i)
          yppt = yppt + Rain_STAy(i)
          zppt = zppt + Rain_STAelev(i)
        ENDIF
      ENDDO
!
! calculate means
!
      IF ( nppt.GT.0 ) THEN
        sppt = sumppt/FLOAT(nppt)
        xppt = xppt/FLOAT(nppt)
        yppt = yppt/FLOAT(nppt)
        zppt = zppt/FLOAT(nppt)
        sppt = (sppt+Ppt_add)/Ppt_div
        xppt = (xppt+X_add)/X_div
        yppt = (yppt+Y_add)/Y_div
        zppt = (zppt+Z_add)/Z_div
      ELSE
! these are already transformed
        sppt = Meanppt
        xppt = Rain_meanx
        yppt = Rain_meany
        zppt = Rain_meanz
      ENDIF

!dbg  IF ( Prt_debug.EQ.8 ) PRINT *, id, im, iy, sppt, xppt, yppt, zppt
!
! adjust the values if not using all the stations
!
      IF ( ABS(Rain_meanz-zppt).GT.NEARZERO ) THEN

        intercept = (sppt) -
     +              (pptlapse3*zppt) -
     +              (pptlapse1*xppt) -
     +              (pptlapse2*yppt)
        sppt = (pptlapse3*Rain_meanz) +
     +         (pptlapse1*Rain_meanx) +
     +         (pptlapse2*Rain_meany) + intercept

      ENDIF

!dbg  IF ( Prt_debug.EQ.8 ) PRINT *, id, im, iy, intercept, sppt

      xppt = sppt

      intppt = xppt -
     +         (pptlapse3*Rain_meanz) -
     +         (pptlapse1*Rain_meanx) -
     +         (pptlapse2*Rain_meany)

      Basin_ppt = 0.0
      Basin_rain = 0.0
      Basin_snow = 0.0

      !rsr, zero precip arrays
      Pptmix = 0
      Hru_ppt = 0.0
      Hru_rain = 0.0
      Hru_snow = 0.0
      Newsnow = 0
      Prmx = 0.0

!dbg  IF ( Prt_debug.EQ.8 ) PRINT *, ' im =', im, ' rain_meanz = ',
!dbg &                            Rain_meanz, '  intppt = ', intppt

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)

        IF ( Is_rain_day.NE.0 ) THEN
          Hru_ppt(i) = (pptlapse1*MRUx(i)) +
     +                 (pptlapse2*MRUy(i)) +
     +                 (pptlapse3*MRUelev(i)) + intppt

          Hru_ppt(i) = Hru_ppt(i)*Ppt_div - Ppt_add

          IF ( Hru_ppt(i).LT.NEARZERO ) Hru_ppt(i) = 0.0
        ENDIF

!******Zero precipitation on HRU

        IF ( Hru_ppt(i).LT.NEARZERO ) CYCLE

!******If observed temperature data are not available or if observed
!******form data are available and rain is explicitly specified then
!******precipitation is all rain.

        IF ( iform.EQ.2 ) THEN
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******If form data are available and snow is explicitly specified or if
!******maximum temperature is below or equal to the base temperature for
!******snow then precipitation is all snow

        ELSEIF ( iform.EQ.1 .OR. Tmaxf(i).LE.Tmax_allsnow ) THEN
          Hru_snow(i) = Hru_ppt(i)
          Newsnow(i) = 1

!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain

        ELSEIF ( Tminf(i).GE.Tmax_allsnow .OR.
     +           Tmaxf(i).GE.Tmax_allrain ) THEN
          Hru_rain(i) = Hru_ppt(i)
          Prmx(i) = 1.0

!******Otherwise precipitation is a mixture of rain and snow

        ELSE
          Prmx(i) = ((Tmaxf(i)-Tmax_allsnow)/(Tmaxf(i)-Tminf(i)))*
     +              Adjmix_rain

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain

          IF ( Prmx(i).GE.1.0 ) THEN  !rsr changed > to GE 1/8/2006
            Hru_rain(i) = Hru_ppt(i)

!******If not, it is a rain/snow mixture

          ELSE
            Pptmix(i) = 1
            Hru_rain(i) = Prmx(i)*Hru_ppt(i)
            Hru_snow(i) = Hru_ppt(i) - Hru_rain(i)
            Newsnow(i) = 1
          ENDIF
        ENDIF

        Basin_ppt = Basin_ppt + Hru_ppt(i)*Hru_area(i)
        Basin_rain = Basin_rain + Hru_rain(i)*Hru_area(i)
        Basin_snow = Basin_snow + Hru_snow(i)*Hru_area(i)

      ENDDO
      Basin_ppt = Basin_ppt*Basin_area_inv
      Basin_rain = Basin_rain*Basin_area_inv
      Basin_snow = Basin_snow*Basin_area_inv

!dbg  IF ( Prt_debug.EQ.8 ) THEN
!dbg    PRINT 9001, Pptmix
!dbg    PRINT 9002, Hru_rain
!dbg  ENDIF

!dbg 9001 FORMAT (' pptmix ', 20I3)
!dbg 9002 FORMAT (' hru_rain ', 10F7.3)

      END SUBROUTINE xyz_rain_run

!***********************************************************************
!     calculate the station mean by month
!***********************************************************************
      SUBROUTINE mean_by_month (PptMTH, TminMTH, TmaxMTH, Meanppt,
     +                          Meantmax, Meantmin, Rain_meanx,
     +                          Rain_meany, Rain_meanz, Temp_meanx,
     +                          Temp_meany, Temp_meanz)
      USE PRMS_XYZ_DIST, ONLY:Nrain, Ntemp, Rain_nsta, Rain_nuse,
     +    Temp_nsta, Temp_nuse, Rain_STAx, Rain_STAy, Rain_STAelev,
     +    Temp_STAx, Temp_STAy, Temp_STAelev, Ppt_add, Ppt_div,
     +    Tmin_add, Tmin_div, Tmax_add, Tmax_div, X_div, Y_div, Z_div,
     +    X_add, Y_add, Z_add
      IMPLICIT NONE
      INTRINSIC FLOAT
! Arguments
      REAL, INTENT(IN) :: PptMTH(Nrain), TminMTH(Ntemp), TmaxMTH(Ntemp)
      REAL, INTENT(OUT) :: Meanppt, Meantmax, Meantmin
      REAL, INTENT(OUT) :: Rain_meanx, Rain_meany, Rain_meanz
      REAL, INTENT(OUT) :: Temp_meanx, Temp_meany, Temp_meanz
! Local Variables
      INTEGER :: i, j
      REAL :: rain_n, temp_n
!***********************************************************************
      rain_n = 1.0/FLOAT(Rain_nsta)
      temp_n = 1.0/FLOAT(Temp_nsta)

      Meanppt = 0.0
      Meantmax = 0.0
      Meantmin = 0.0
      Rain_meanx = 0.0
      Rain_meany = 0.0
      Rain_meanz = 0.0
      Temp_meanx = 0.0
      Temp_meany = 0.0
      Temp_meanz = 0.0

      DO j = 1, Rain_nsta
        i = Rain_nuse(j)

!       PptMTH(i) = (PptMTH(i)+Ppt_add)/Ppt_div

        Meanppt = Meanppt + PptMTH(i)
        Rain_meanx = Rain_meanx + Rain_STAx(i)
        Rain_meany = Rain_meany + Rain_STAy(i)
        Rain_meanz = Rain_meanz + Rain_STAelev(i)
      ENDDO

      DO j = 1, Temp_nsta
        i = Temp_nuse(j)

!       TminMTH(i) = (TminMTH(i)+Tmin_add)/Tmin_div
!       TmaxMTH(i) = (TmaxMTH(i)+Tmax_add)/Tmax_div

        Meantmin = Meantmin + TminMTH(i)
        Meantmax = Meantmax + TmaxMTH(i)
        Temp_meanx = Temp_meanx + Temp_STAx(i)
        Temp_meany = Temp_meany + Temp_STAy(i)
        Temp_meanz = Temp_meanz + Temp_STAelev(i)
      ENDDO

      Meanppt = Meanppt*rain_n
      Meantmin = Meantmin*temp_n
      Meantmax = Meantmax*temp_n

      Rain_meanx = Rain_meanx*rain_n
      Rain_meany = Rain_meany*rain_n
      Rain_meanz = Rain_meanz*rain_n

      Temp_meanx = Temp_meanx*temp_n
      Temp_meany = Temp_meany*temp_n
      Temp_meanz = Temp_meanz*temp_n

      Meanppt = (Meanppt+Ppt_add)/Ppt_div
      Meantmin = (Meantmin+Tmin_add)/Tmin_div
      Meantmax = (Meantmax+Tmax_add)/Tmax_div

      Rain_meanx = (Rain_meanx+X_add)/X_div
      Rain_meany = (Rain_meany+Y_add)/Y_div
      Rain_meanz = (Rain_meanz+Z_add)/Z_div

      Temp_meanx = (Temp_meanx+X_add)/X_div
      Temp_meany = (Temp_meany+Y_add)/Y_div
      Temp_meanz = (Temp_meanz+Z_add)/Z_div

      END SUBROUTINE mean_by_month

!***********************************************************************
! Convert Fahrenheit to Celsius
!***********************************************************************
      REAL FUNCTION f_to_c_xyz(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: FIVE_NINTHS = 5.0/9.0
!***********************************************************************
      f_to_c_xyz = (Temp-32.0)*FIVE_NINTHS
      END FUNCTION f_to_c_xyz

!***********************************************************************
! Convert Celsius to Fahrenheit
!***********************************************************************
      REAL FUNCTION c_to_f_xyz(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: NINE_FIFTHS = 9.0/5.0
!***********************************************************************
      c_to_f_xyz = Temp*NINE_FIFTHS + 32.0
      END FUNCTION c_to_f_xyz

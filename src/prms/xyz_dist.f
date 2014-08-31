!***********************************************************************
! Determines the form of precipitation and distributes precipitation
! and temperatures to each HRU using a multiple linear regression of
! measured data from a group of measurement stations or from atmospheric
! model simulation
!
!                      Converted by Steve Markstrom
!                      Wed Feb 10 15:16:04 MST 1999
!              revised Wed Mar 17 15:48:52 MST 1999
!              revised Mon Aug 30 16:47:07 MDT 1999
!              revised Mon Aug 30 16:47:07 MDT 1999
!              revised Wed Mar  8 09:06:18 MST 2000
!              revised Thu Feb  3 10:00:00 MST 2005
! temp_nsta - number of temperature stations used
! temp_nuse(temp_nsta) - indicies of temperature stations used
! rain_nsta - number of precipitation stations used
! rain_nuse (rain_nsta) - indicies of rain stations used
!rsr, note tmax_allsnow and tmax_allrain assumed to be in Fahrenheit
!***********************************************************************
      MODULE PRMS_XYZ_DIST
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: MAXLAPSE = 3
      CHARACTER(LEN=8), PARAMETER :: MODNAME = 'xyz_dist'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Climate Distribuition'
      INTEGER, SAVE :: Nlapse, Temp_nsta, Rain_nsta
      INTEGER, SAVE, ALLOCATABLE :: Rain_nuse(:), Temp_nuse(:)
      DOUBLE PRECISION, SAVE :: Basin_centroid_x, Basin_centroid_y
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Meantmax(:), Meantmin(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Temp_meanx(:),Temp_meany(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Rain_meanx(:),Rain_meany(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Temp_meanz(:),Rain_meanz(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Meanppt(:)
      REAL, SAVE, ALLOCATABLE :: Precip_xyz(:)
      REAL, SAVE, ALLOCATABLE :: Temp_STAelev(:)
! transformed versions of these values
      REAL, SAVE, ALLOCATABLE :: MRUelev(:), Pstaelev(:)
      REAL, SAVE, ALLOCATABLE :: Pstax(:), Pstay(:)
      REAL, SAVE :: Solradelev
!   Declared Variables
      INTEGER, SAVE :: Is_rain_day
      REAL, SAVE, ALLOCATABLE :: Tmax_rain_sta(:), Tmin_rain_sta(:)
!   Declared Parameters
      INTEGER, SAVE :: Conv_flag
      REAL, SAVE, ALLOCATABLE :: Max_lapse(:, :), Min_lapse(:, :)
      REAL, SAVE, ALLOCATABLE :: Ppt_lapse(:, :)
      REAL, SAVE :: Solrad_elev
      REAL, SAVE :: Tmax_add, Tmax_div, Tmin_add, Tmin_div, Ppt_add
      REAL, SAVE :: X_add, X_div, Y_add, Y_div, Z_add, Z_div, Ppt_div
      INTEGER, SAVE, ALLOCATABLE :: Tsta_nuse(:)
      INTEGER, SAVE, ALLOCATABLE :: Psta_nuse(:), Psta_freq_nuse(:)
      REAL, SAVE, ALLOCATABLE :: MRUx(:), MRUy(:)
!     Temp_STAx = tsta_x, Temp_STAy = tsta_y
      REAL, SAVE, ALLOCATABLE :: Temp_STAx(:), Temp_STAy(:)
!     Rain_STAx = psta_x, Rain_STAy = psta_y
      REAL, SAVE, ALLOCATABLE :: Rain_STAx(:), Rain_STAy(:)
!     TmaxMTH = tsta_month_max, TminMTH = tsta_month_min
!     PptMTH = psta_month_ppt
      REAL, SAVE, ALLOCATABLE :: TmaxMTH(:, :), TminMTH(:, :)
      REAL, SAVE, ALLOCATABLE :: PptMTH(:, :)
      END MODULE PRMS_XYZ_DIST

!***********************************************************************
!     Main xyz_dist routine
!***********************************************************************
      INTEGER FUNCTION xyz_dist()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: xyzdecl, xyzinit, xyzrun, xyzsetdims
!***********************************************************************
      xyz_dist = 0

      IF ( Process(:3)=='run' ) THEN
        xyz_dist = xyzrun()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        xyz_dist = xyzsetdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        xyz_dist = xyzdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        xyz_dist = xyzinit()
      ENDIF

      END FUNCTION xyz_dist

!***********************************************************************
!     xyzsetdims - declares xyz_dist specific dimensions
!***********************************************************************
      INTEGER FUNCTION xyzsetdims()
      USE PRMS_XYZ_DIST, ONLY: Nlapse
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declfix
      EXTERNAL read_error
!***********************************************************************
      xyzsetdims = 1

      IF ( declfix('nlapse', 3, 3,
     +     'Number of lapse rates in X, Y, and Z directions')
     +     /=0 ) CALL read_error(7, 'nlapse')
      Nlapse = 3

      xyzsetdims = 0
      END FUNCTION xyzsetdims
!***********************************************************************

!***********************************************************************
!     xyzdecl - set up parameters for temperature computations
!   Declared Parameters
!     hru_x, hru_y, max_lapse, min_lapse, ppt_lapse, tsta_elev
!     tmax_adj, tmin_adj, tsta_x, tsta_y, psta_elev, psta_x, psta_y
!     tsta_nuse, psta_nuse, psta_freq_nuse, adjust_snow, adjust_rain
!     tsta_month_max, tsta_month_min, psta_month_ppt, rain_code
!     x_add, x_div, y_add, y_div, z_add, z_div, solrad_elev
!     tmin_add, tmin_div, tmax_add, tmax_div, ppt_add, ppt_div
!     tmax_allrain, tmax_allsnow, adjmix_rain, conv_flag
!     hru_elev, hru_area
!***********************************************************************
      INTEGER FUNCTION xyzdecl()
      USE PRMS_XYZ_DIST
      USE PRMS_MODULE, ONLY: Model, Nhru, Version_xyz_dist, Xyz_dist_nc
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Nrain
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: n
!***********************************************************************
      xyzdecl = 1

      Version_xyz_dist =
     +'$Id: xyz_dist.f 4572 2012-06-08 14:26:18Z rsregan $'
      Xyz_dist_nc = INDEX( Version_xyz_dist, 'Z' )
      n = INDEX( Version_xyz_dist, '.f' ) + 1
      IF ( declmodule(Version_xyz_dist(6:n), PROCNAME,
     +                Version_xyz_dist(n+2:Xyz_dist_nc))/=0 ) STOP

      IF ( declvar(MODNAME, 'is_rain_day', 'one', 1, 'integer',
     +     'Flag to indicate if it is raining anywhere in the basin',
     +     'none',
     +     Is_rain_day)/=0 ) CALL read_error(3, 'is_rain_day')

      ALLOCATE ( Tmax_rain_sta(Nrain) )
      IF ( declvar(MODNAME, 'tmax_rain_sta', 'nrain', Nrain, 'real',
     +     'Maximum temperature distributed to the precipitation'//
     +     ' measurement stations',
     +     'degrees F',
     +     Tmax_rain_sta)/=0 ) CALL read_error(3, 'tmax_rain_sta')

      ALLOCATE ( Tmin_rain_sta(Nrain) )
      IF ( declvar(MODNAME, 'tmin_rain_sta', 'nrain', Nrain, 'real',
     +     'Minimum temperature distributed to the precipitation'//
     +     ' measurement stations',
     +     'degrees F',
     +     Tmin_rain_sta)/=0 ) CALL read_error(3, 'tmin_rain_sta')

! declare parameters
      ALLOCATE ( MRUelev(Nhru) )
      ALLOCATE ( MRUx(Nhru) )
      IF ( declparam(MODNAME, 'hru_x', 'nhru', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X for each HRU (albers)',
     +     'Longitude (X) for each HRU in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'hru_x')

      ALLOCATE ( MRUy(Nhru) )
      IF ( declparam(MODNAME, 'hru_y', 'nhru', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y for each HRU (albers)',
     +     'Latitude (Y) for each HRU in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'hru_y')

      ALLOCATE ( Max_lapse(MAXLAPSE, 12) )
      IF ( declparam(MODNAME, 'max_lapse', 'nlapse,nmonths', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Monthly maximum temperature lapse rate for each direction',
     +     'Monthly (January to December) maximum air temperature'//
     +     ' lapse rate for each direction (X, Y, and Z)',
     +     'none')/=0 ) CALL read_error(1, 'max_lapse')

      ALLOCATE ( Min_lapse(MAXLAPSE, 12) )
      IF ( declparam(MODNAME, 'min_lapse', 'nlapse,nmonths', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Monthly minimum temperature lapse rate for each direction',
     +     'Monthly (January to December) minimum air temperature'//
     +     ' lapse rate for each direction (X, Y, and Z)',
     +     'none')/=0 ) CALL read_error(1, 'min_lapse')

      ALLOCATE ( Ppt_lapse(MAXLAPSE, 12) )
      IF ( declparam(MODNAME, 'ppt_lapse', 'nlapse,nmonths', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'Precipitation lapse rate',
     +     'Monthly (January to December) precipitation lapse rate'//
     +     ' for each direction (X, Y, and Z)',
     +     'none')/=0 ) CALL read_error(1, 'ppt_lapse')

      ALLOCATE ( Temp_STAx(Ntemp), Temp_STAelev(Ntemp) )
      IF ( declparam(MODNAME, 'tsta_x', 'ntemp', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X for each temperature station (albers)',
     +     'Longitude (X) for each temperature measurement station in'//
     +     ' albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'tsta_x')

      ALLOCATE ( Temp_STAy(Ntemp) )
      IF ( declparam(MODNAME, 'tsta_y', 'ntemp', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y for each temperature station (albers)',
     +     'Latitude (Y) for each temperature measurement station in'//
     +     ' albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'tsta_y')

      ALLOCATE ( Rain_STAx(Nrain), Pstaelev(Nrain) )
      IF ( declparam(MODNAME, 'psta_x', 'nrain', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X for each precipitation station (albers)',
     +     'Longitude (X) for each precipitation measurement station'//
     +     ' in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'psta_x')

      ALLOCATE ( Rain_STAy(Nrain) )
      IF ( declparam(MODNAME, 'psta_y', 'nrain', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y for each precipitation station (albers)',
     +     'Latitude (Y) for each precipitation measurement station'//
     +     ' in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'psta_y')

      ALLOCATE ( Tsta_nuse(Ntemp), Temp_nuse(Ntemp) )
      IF ( declparam(MODNAME, 'tsta_nuse', 'ntemp', 'integer',
     +     '1', '0', '1',
     +     '0 = station not used; 1 = station used',
     +     'The subset of temperature measurement stations used in'//
     +     ' the distribution regression (0=station not used;'//
     +     ' 1=station used)',
     +     'none')/=0 ) CALL read_error(1, 'tsta_nuse')

      IF ( declparam(MODNAME, 'solrad_elev', 'one', 'real',
     +     '1000.0', '0.0', '10000.0',
     +     'Elevation of the solrad station used for DD curves',
     +     'Elevation of the solar radiation station used for'//
     +     ' degree-day curves',
     +     'meters')/=0 ) CALL read_error(1, 'solrad_elev')

      ALLOCATE ( Psta_nuse(Nrain), Rain_nuse(Nrain) )
      IF ( declparam(MODNAME, 'psta_nuse', 'nrain', 'integer',
     +     '1', '0', '1',
     +     'The subset of precipitation stations used in the'//
     +     ' distribution regression (0=station not used;'//
     +     ' 1=station used)',
     +     'The subset of precipitation measurement stations used in'//
     +     ' the distribution regression (0=station not used;'//
     +     ' 1=station used)',
     +     'none')/=0 ) CALL read_error(1, 'psta_nuse')

      ALLOCATE (Psta_freq_nuse(Nrain) )
      IF ( declparam(MODNAME, 'psta_freq_nuse', 'nrain', 'integer',
     +     '1', '0', '1',
     +     'The subset of precipitation stations used to determine if'//
     +     ' there is distribution in the basin (0=station not used;'//
     +     ' 1=station used)',
     +     'The subset of precipitation measurement stations used to'//
     +     ' determine if there is precipitation in the basin'//
     +     ' (0=station not used; 1=station used)',
     +     'none')/=0 ) CALL read_error(1, 'psta_freq_nuse')

      ALLOCATE ( TmaxMTH(Ntemp, 12) )
      IF ( declparam(MODNAME, 'tsta_month_max', 'ntemp,nmonths',
     +     'real',
     +     '0.0', '-100.0', '200.0',
     +     'Average monthly (January to December) maximum air'//
     +     ' temperature at each station',
     +     'Average monthly (January to December) maximum air'//
     +     ' temperature at each temperature measurement station',
     +     'temp_units')/=0 ) CALL read_error(1, 'tsta_month_max')

      ALLOCATE ( TminMTH(Ntemp, 12) )
      IF ( declparam(MODNAME, 'tsta_month_min', 'ntemp,nmonths',
     +     'real',
     +     '0.0', '-100.0', '200.0',
     +     'Average monthly (January to December) minimum air'//
     +     ' temperature at each station',
     +     'Average monthly (January to December) minimum air'//
     +     ' temperature at each temperature measurement station',
     +     'temp_units')/=0 ) CALL read_error(1, 'tsta_month_min')

      ALLOCATE ( PptMTH(Nrain, 12) )
      IF ( declparam(MODNAME, 'psta_month_ppt', 'nrain,nmonths',
     +     'real',
     +     '0.0', '0.0', '200.0',
     +     'Average monthly precipitation at each station',
     +     'Average monthly (January to December) precipitation at'//
     +     ' each measurement station',
     +     'precip_units')/=0 ) CALL read_error(1, 'psta_month_ppt')

      IF ( declparam(MODNAME, 'x_add', 'one', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for the longitude (X) coordinate',
     +     'meters')/=0 ) CALL read_error(1, 'x_add')

      IF ( declparam(MODNAME, 'x_div', 'one', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for the longitude (X) coordinate',
     +     'meters')/=0 ) CALL read_error(1, 'x_div')

      IF ( declparam(MODNAME, 'y_add', 'one', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for the latitude (Y) coordinate',
     +     'meters')/=0 ) CALL read_error(1, 'y_add')

      IF ( declparam(MODNAME, 'y_div', 'one', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for the latitude (Y) coordinate',
     +     'meters')/=0 ) CALL read_error(1, 'y_div')

      IF ( declparam(MODNAME, 'z_add', 'one', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Z additive term for climate station transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for the elevation (Z) coordinate',
     +     'meters')/=0 ) CALL read_error(1, 'z_add')

      IF ( declparam(MODNAME, 'z_div', 'one', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Z divisor term for climate station transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for the elevation (Z) coordinate',
     +     'meters')/=0 ) CALL read_error(1, 'z_div')

      IF ( declparam(MODNAME, 'tmax_add', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Maximum temperature additive term for climate station'//
     +     ' transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for maximum temperature',
     +     'temp_units')/=0 ) CALL read_error(1, 'tmax_add')

      IF ( declparam(MODNAME, 'tmax_div', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Maximum temperature divisor term for climate station'//
     +     ' transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for maximum temperature',
     +     'temp_units')/=0 ) CALL read_error(1, 'tmax_div')

      IF ( declparam(MODNAME, 'tmin_add', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Minimum temperature additive term for climate station'//
     +     ' transform',
     +     'Mean value for the climate station transformation'//
     +     ' equation for minimum temperature',
     +     'temp_units')/=0 ) CALL read_error(1, 'tmin_add')

      IF ( declparam(MODNAME, 'tmin_div', 'one', 'real',
     +     '0.0', '-100.0', '100.0',
     +     'Minimum temperature divisor term for climate station'//
     +     ' transform',
     +     'Standard deviation for the climate station transformation'//
     +     ' equation for minimum temperature',
     +     'temp_units')/=0 ) CALL read_error(1, 'tmin_div')

      IF ( declparam(MODNAME, 'ppt_add', 'one', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'Precipitation additive term for climate station transform',
     +     'Mean value for the precipitation measurement station'//
     +     ' transformation equation',
     +     'precip_units')/=0 ) CALL read_error(1, 'ppt_add')

      IF ( declparam(MODNAME, 'ppt_div', 'one', 'real',
     +     '0.0', '-10.0', '10.0',
     +     'Precipitation divisor term for climate station transform',
     +     'Standard deviation for the precipitation measurement'//
     +     ' station transformation equation',
     +     'precip_units')/=0 ) CALL read_error(1, 'ppt_div')

      IF ( declparam(MODNAME, 'conv_flag', 'one', 'integer',
     +     '0', '0', '2',
     +     'Elevation conversion flag',
     +     'Elevation conversion flag (0=none, 1=feet to meters,'//
     +     ' 2=meters to feet)',
     +     'none')/=0 ) CALL read_error(1, 'conv_flag')

      ALLOCATE ( Meantmax(12), Meantmin(12), Meanppt(12) )
      ALLOCATE ( Temp_meanx(12), Temp_meany(12), Temp_meanz(12) )
      ALLOCATE ( Rain_meanx(12), Rain_meany(12), Rain_meanz(12) )
      ALLOCATE ( Precip_xyz(Nrain) )

      xyzdecl = 0
      END FUNCTION xyzdecl

!***********************************************************************
!     xyzinit - Initialize xyz_dist module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION xyzinit()
      USE PRMS_XYZ_DIST
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_BASIN, ONLY: Timestep, Hru_area, Basin_area_inv,
     +    Hru_elev, Active_hrus, Hru_route_order, FEET2METERS
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Nrain, Psta_elev, Tsta_elev
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL mean_by_month, read_error
! Local Variables
      INTEGER :: i, m, ii, div_err
!***********************************************************************
      xyzinit = 1

! Initialize declared variables
      IF ( Timestep==0 ) THEN
        Is_rain_day = 0
        Tmax_rain_sta = 0.0
        Tmin_rain_sta = 0.0
      ENDIF

      IF ( getparam (MODNAME, 'solrad_elev', 1, 'real', Solrad_elev)
     +     /=0 ) CALL read_error(2, 'solrad_elev')

      IF ( getparam(MODNAME, 'hru_x', Nhru, 'real', MRUx)
     +     /=0 ) CALL read_error(2, 'hru_x')

      IF ( getparam(MODNAME, 'hru_y', Nhru, 'real', MRUy)
     +     /=0 ) CALL read_error(2, 'hru_y')

      IF ( getparam(MODNAME, 'max_lapse', MAXLAPSE*12, 'real',
     +     Max_lapse)/=0 ) CALL read_error(2, 'max_lapse')

      IF ( getparam(MODNAME, 'min_lapse', MAXLAPSE*12, 'real',
     +     Min_lapse)/=0 ) CALL read_error(2, 'min_lapse')

      IF ( getparam(MODNAME, 'ppt_lapse', MAXLAPSE*12, 'real',
     +     Ppt_lapse)/=0 ) CALL read_error(2, 'ppt_lapse')

      IF ( getparam(MODNAME, 'tsta_x', Ntemp, 'real', Temp_STAx)
     +     /=0 ) CALL read_error(2, 'tsta_x')

      IF ( getparam(MODNAME, 'tsta_y', Ntemp, 'real', Temp_STAy)
     +     /=0 ) CALL read_error(2, 'tsta_y')

      IF ( getparam(MODNAME, 'psta_x', Nrain, 'real', Rain_STAx)
     +     /=0 ) CALL read_error(2, 'psta_x')

      IF ( getparam(MODNAME, 'psta_y', Nrain, 'real', Rain_STAy)
     +     /=0 ) CALL read_error(2, 'psta_y')

      IF ( getparam(MODNAME, 'tsta_nuse', Ntemp, 'integer',
     +     Tsta_nuse)/=0 ) CALL read_error(2, 'tsta_nuse')

      IF ( getparam(MODNAME, 'psta_nuse', Nrain, 'integer',
     +     Psta_nuse)/=0 ) CALL read_error(2, 'psta_nuse')

      IF ( getparam(MODNAME, 'psta_freq_nuse', Nrain, 'integer',
     +     Psta_freq_nuse)/=0 ) CALL read_error(2, 'psta_freq_nuse')

      IF ( getparam(MODNAME, 'tsta_month_min', Ntemp*12, 'real',
     +     TminMTH)/=0 ) CALL read_error(2, 'tsta_month_min')

      IF ( getparam(MODNAME, 'tsta_month_max', Ntemp*12, 'real',
     +     TmaxMTH)/=0 ) CALL read_error(2, 'tsta_month_max')

      IF ( getparam(MODNAME, 'psta_month_ppt', Nrain*12, 'real',
     +     PptMTH)/=0 ) CALL read_error(2, 'psta_month_ppt')

      IF ( getparam(MODNAME, 'z_add', 1, 'real', Z_add)
     +     /=0 ) CALL read_error(2, 'z_add')

      IF ( getparam(MODNAME, 'z_div', 1, 'real', Z_div)
     +     /=0 ) CALL read_error(2, 'z_div')

      IF ( getparam(MODNAME, 'x_add', 1, 'real', X_add)
     +     /=0 ) CALL read_error(2, 'x_add')

      IF ( getparam(MODNAME, 'x_div', 1, 'real', X_div)
     +     /=0 ) CALL read_error(2, 'x_div')

      IF ( getparam(MODNAME, 'y_add', 1, 'real', Y_add)
     +     /=0 ) CALL read_error(2, 'y_add')

      IF ( getparam(MODNAME, 'y_div', 1, 'real', Y_div)
     +     /=0 ) CALL read_error(2, 'y_div')

      IF ( getparam(MODNAME, 'tmax_add', 1, 'real', Tmax_add)
     +     /=0 ) CALL read_error(2, 'tmax_add')

      IF ( getparam(MODNAME, 'tmax_div', 1, 'real', Tmax_div)
     +     /=0 ) CALL read_error(2, 'tmax_div')

      IF ( getparam(MODNAME, 'tmin_add', 1, 'real', Tmin_add)
     +     /=0 ) CALL read_error(2, 'tmin_add')

      IF ( getparam(MODNAME, 'tmin_div', 1, 'real', Tmin_div)
     +     /=0 ) CALL read_error(2, 'tmin_div')

      IF ( getparam(MODNAME, 'ppt_add', 1, 'real', Ppt_add)
     +     /=0 ) CALL read_error(2, 'ppt_add')

      IF ( getparam(MODNAME, 'ppt_div', 1, 'real', Ppt_div)
     +     /=0 ) CALL read_error(2, 'ppt_div')

      IF ( getparam(MODNAME, 'conv_flag', 1, 'integer', Conv_flag)
     +     /=0 ) CALL read_error(2, 'conv_flag')

      div_err = 0
      IF ( Z_div==0.0 ) THEN
        PRINT *, 'ERROR, z_div cannot be 0.0'
        div_err = 1
      ENDIF
      IF ( X_div==0.0 ) THEN
        PRINT *, 'ERROR, x_div cannot be 0.0'
        div_err = 1
      ENDIF
      IF ( Y_div==0.0 ) THEN
        PRINT *, 'ERROR, y_div cannot be 0.0'
        div_err = 1
      ENDIF
      IF ( Ppt_div==0.0 ) THEN
        PRINT *, 'ERROR, ppt_div cannot be 0.0'
        div_err = 1
      ENDIF
      IF ( Tmin_div==0.0 ) THEN
        PRINT *, 'ERROR, tmin_div cannot be 0.0'
        div_err = 1
      ENDIF
      IF ( Tmax_div==0.0 ) THEN
        PRINT *, 'ERROR, tmax_div cannot be 0.0'
        div_err = 1
      ENDIF
      IF ( div_err==1 ) STOP
!
! Compute basin centroid
!
      Basin_centroid_x = 0.0D0
      Basin_centroid_y = 0.0D0
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
      IF ( Conv_flag==1 ) THEN
        DO i = 1, Nhru
          MRUelev(i) = Hru_elev(i)*FEET2METERS
        ENDDO
        DO i = 1, Ntemp
          Temp_STAelev(i) = Tsta_elev(i)*FEET2METERS
        ENDDO
        DO i = 1, Nrain
          Pstaelev(i) = Psta_elev(i)*FEET2METERS
        ENDDO
        Solradelev = Solrad_elev*FEET2METERS
      ELSE
        DO i = 1, Nhru
          MRUelev(i) = Hru_elev(i)
        ENDDO
        DO i = 1, Ntemp
          Temp_STAelev(i) = Tsta_elev(i)
        ENDDO
        DO i = 1, Nrain
          Pstaelev(i) = Psta_elev(i)
        ENDDO
        Solradelev = Solrad_elev
      ENDIF
!
! transform Z, X and Y
!
! transform values only once
      Basin_centroid_x = (Basin_centroid_x+X_add)/X_div
      Basin_centroid_y = (Basin_centroid_y+Y_add)/Y_div
      Solradelev = (Solradelev+Z_add)/Z_div

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        MRUelev(i) = (MRUelev(i)+Z_add)/Z_div
        MRUx(i) = (MRUx(i)+X_add)/X_div
        MRUy(i) = (MRUy(i)+Y_add)/Y_div
      ENDDO

      Temp_nsta = 0
      DO i = 1, Ntemp
!       Temp_STAelev(i) = (Temp_STAelev(i)+Z_add)/Z_div
!       temp_STAx(i) = (temp_STAx(i)+X_add)/X_div
!       temp_STAy(i) = (temp_STAy(i)+Y_add)/Y_div
        IF ( Tsta_nuse(i).EQ.1 ) THEN
          Temp_nsta = Temp_nsta + 1
          Temp_nuse(Temp_nsta) = i
        ENDIF
      ENDDO

      ALLOCATE ( Pstax(Nrain), Pstay(Nrain) )
      Rain_nsta = 0
      DO i = 1, Nrain
        Pstaelev(i) = (Pstaelev(i)+Z_add)/Z_div
        Pstax(i) = (Rain_STAx(i)+X_add)/X_div
        Pstay(i) = (Rain_STAy(i)+Y_add)/Y_div
        IF ( Psta_nuse(i)==1 ) THEN
          Rain_nsta = Rain_nsta + 1
          Rain_nuse(Rain_nsta) = i
        ENDIF
      ENDDO
      DEALLOCATE ( Psta_nuse, Tsta_nuse )
!
! calculate the station mean by month
!
      DO m = 1, 12
        CALL mean_by_month(PptMTH(1, m), TminMTH(1, m), TmaxMTH(1, m),
     +                     Meanppt(m), Meantmax(m), Meantmin(m),
     +                     Rain_meanx(m), Rain_meany(m), Rain_meanz(m),
     +                     Temp_meanx(m), Temp_meany(m), Temp_meanz(m))
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
      USE PRMS_CLIMATEVARS, ONLY: Adjmix_rain, Adjust_snow, Adjust_rain,
     +    Tmax_allrain
      USE PRMS_OBS, ONLY: Nowtime, Rain_code, Nowmonth
      IMPLICIT NONE
! Functions
      EXTERNAL xyz_temp_run, xyz_rain_run
!***********************************************************************
      xyzrun = 1

      CALL xyz_temp_run(Max_lapse(1, Nowmonth), Min_lapse(1, Nowmonth),
     +                  Meantmax(Nowmonth), Meantmin(Nowmonth),
     +                  Temp_meanx(Nowmonth), Temp_meany(Nowmonth),
     +                  Temp_meanz(Nowmonth))

      CALL xyz_rain_run(Ppt_lapse(1, Nowmonth), Rain_meanx(Nowmonth),
     +                  Rain_meany(Nowmonth), Rain_meanz(Nowmonth),
     +                  Meanppt(Nowmonth), Tmax_allrain(Nowmonth),
     +                  Adjmix_rain(Nowmonth), Rain_code(Nowmonth),
     +                  Adjust_snow(Nowmonth), Adjust_rain(Nowmonth))

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
      USE PRMS_XYZ_DIST, ONLY: MRUx, MRUy, Tmax_rain_sta, Solradelev,
     +    Tmin_rain_sta, Temp_nuse, Tmin_add, Tmin_div, Tmax_add,
     +    Tmax_div, Temp_nsta, X_div, Y_div, Z_div, X_add, Y_add, Z_add,
     +    Temp_STAx, Temp_STAy, Basin_centroid_y, Basin_centroid_x,
     +    MAXLAPSE, Pstaelev, Pstax, Pstay, MRUelev, Temp_STAelev
      USE PRMS_BASIN, ONLY: Basin_area_inv, Hru_area, Active_hrus,
     +    DNEARZERO, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp,
     +    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf,
     +    Tavgc, Tmin_adj, Tmax_adj, Psta_elev, Nrain
      USE PRMS_OBS, ONLY: Tmax, Tmin 
      IMPLICIT NONE
! Functions
      INTRINSIC FLOAT, ABS, SNGL
      EXTERNAL temp_set
      REAL, EXTERNAL :: c_to_f
! Arguments
!   Declared Parameters
      REAL, INTENT(IN) :: Max_lapse(MAXLAPSE), Min_lapse(MAXLAPSE)
!   Private Variables
      DOUBLE PRECISION, INTENT(IN) :: Meantmax, Meantmin
      DOUBLE PRECISION, INTENT(IN) :: Temp_meanx, Temp_meany, Temp_meanz
! Local Variables
      INTEGER :: i, j, ntmin, ntmax, ii
      REAL :: tmax_hru_sngl, tmin_hru_sngl
      DOUBLE PRECISION :: tmax_hru, tmin_hru, ztmax, ztmin
      DOUBLE PRECISION :: intmax, intmin, xmax, xmin, intercept, x1
      DOUBLE PRECISION :: sumtmin, sumtmax, xtmin, xtmax, ytmin
      DOUBLE PRECISION :: ytmax, stmax, stmin
      DOUBLE PRECISION :: zrain, xrain, yrain
      DOUBLE PRECISION :: maxlapse1, maxlapse2, maxlapse3
      DOUBLE PRECISION :: minlapse1, minlapse2, minlapse3
!***********************************************************************
      sumtmin = 0.0D0
      sumtmax = 0.0D0
      ntmin = 0
      ntmax = 0
      xtmin = 0.0D0
      xtmax = 0.0D0
      ytmin = 0.0D0
      ytmax = 0.0D0
      ztmin = 0.0D0
      ztmax = 0.0D0
      maxlapse1 = Max_lapse(1)
      maxlapse2 = Max_lapse(2)
      maxlapse3 = Max_lapse(3)
      minlapse1 = Min_lapse(1)
      minlapse2 = Min_lapse(2)
      minlapse3 = Min_lapse(3)

! Do not Transform the coordinates of the temperature stations
! until after summing

      DO j = 1, Temp_nsta
        i = Temp_nuse(j)

        IF ( Tmax(i)>-99.0 ) THEN
          ntmax = ntmax + 1
!         sumtmax = sumtmax + ((Tmax(i)+Tmax_add)/Tmax_div)
          sumtmax = sumtmax + Tmax(i)
          xtmax = xtmax + Temp_STAx(i)
          ytmax = ytmax + Temp_STAy(i)
          ztmax = ztmax + Temp_STAelev(i)
        ENDIF

        IF ( Tmin(i)>-99.0 ) THEN
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
      IF ( ntmin>0 ) THEN
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

      IF ( ntmax>0 ) THEN
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
      IF ( ABS(Temp_meanz-ztmin)>DNEARZERO ) THEN
        intercept = stmin - minlapse3*ztmin - minlapse1*xtmin -
     +              minlapse2*ytmin
        stmin = minlapse3*Temp_meanz + minlapse1*Temp_meanx +
     +          minlapse2*Temp_meany + intercept
      ENDIF
!
      IF ( ABS(Temp_meanz-ztmax)>DNEARZERO ) THEN
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
      Basin_tmax = 0.0D0
      Basin_tmin = 0.0D0
      Basin_temp = 0.0D0
!
!  Compute maximum temperature at XY centroid of basin at the elevation
!  of the solrad station used to develop the DD solrad curves.
!
      Solrad_tmax = ((maxlapse1*Basin_centroid_x)+
     +              (maxlapse2*Basin_centroid_y)+
     +              (maxlapse3*Solradelev+intmax))*Tmax_div - Tmax_add
      Solrad_tmin = ((minlapse1*Basin_centroid_x)+
     +              (minlapse2*Basin_centroid_y)+
     +              (minlapse3*Solradelev+intmin))*Tmin_div - Tmin_add

!
!  Compute temperatures at distribution stations.
!
      DO i = 1, Nrain
        zrain = Pstaelev(i)
        xrain = Pstax(i)
        yrain = Pstay(i)
        Tmax_rain_sta(i) = (maxlapse1*xrain) +
     +                     (maxlapse2*yrain) +
     +                     (maxlapse3*zrain) + intmax

        Tmin_rain_sta(i) = (minlapse1*xrain) +
     +                     (minlapse2*yrain) +
     +                     (minlapse3*zrain) + intmin

        Tmax_rain_sta(i) = (Tmax_rain_sta(i)*Tmax_div) - Tmax_add
        Tmin_rain_sta(i) = (Tmin_rain_sta(i)*Tmin_div) - Tmin_add

        IF ( Tmax_rain_sta(i)<Tmin_rain_sta(i) ) THEN
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
!  Temperature adjustment by HRU
!
        tmax_hru = tmax_hru + Tmax_adj(i)
        tmin_hru = tmin_hru + Tmin_adj(i)

!
!  If max is less than min, switch
!
        IF ( tmax_hru<tmin_hru ) THEN
          x1 = tmax_hru
          tmax_hru = tmin_hru
          tmin_hru = x1
        ENDIF
!
!  Now sort out units.
!
        tmax_hru_sngl = SNGL(tmax_hru)
        tmin_hru_sngl = SNGL(tmin_hru)
        CALL temp_set(i, tmax_hru_sngl, tmin_hru_sngl,Tmaxf(i),Tminf(i),
     +       Tavgf(i), Tmaxc(i), Tminc(i), Tavgc(i), Hru_area(i))
      ENDDO

      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv
      Basin_temp = Basin_temp*Basin_area_inv

      END SUBROUTINE xyz_temp_run

      SUBROUTINE xyz_rain_run(Ppt_lapse, Rain_meanx, Rain_meany,
     +                        Rain_meanz, Meanppt, Tmax_allrain,
     +                        Adjmix_rain, Rain_code, Adjust_snow,
     +                        Adjust_rain)
      USE PRMS_XYZ_DIST, ONLY: MRUx, MRUy, Rain_STAx, Rain_STAy,
     +    Rain_nuse, Ppt_add, Ppt_div, Rain_nsta, Tmax_rain_sta,
     +    Tmin_rain_sta, Is_rain_day, Psta_freq_nuse, X_div, Y_div,
     +    Z_div, X_add, Y_add, Z_add, Precip_xyz, MAXLAPSE, MRUelev
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Active_hrus,
     +    Hru_route_order, NEARZERO, DNEARZERO, MM2INCH
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tminf, Newsnow, Pptmix,
     +    Hru_ppt, Hru_rain, Hru_snow, Basin_rain, Tmax_allsnow,
     +    Basin_ppt, Prmx, Basin_snow, Psta_elev, Basin_obs_ppt, Nrain,
     +    Precip_units
      USE PRMS_OBS, ONLY: Precip, Nowtime, Rain_day
      IMPLICIT NONE
! Functions
      INTRINSIC FLOAT, ABS, SNGL
      EXTERNAL precip_form
! Arguments
!   Declared Parameters
      INTEGER, INTENT(IN) :: Rain_code
      REAL, INTENT(IN) :: Adjust_snow, Adjust_rain, Ppt_lapse(MAXLAPSE)
      REAL, INTENT(IN) :: Tmax_allrain, Adjmix_rain
!   Undeclared Static Variables
      DOUBLE PRECISION, INTENT(IN) :: Rain_meanx, Rain_meany
      DOUBLE PRECISION, INTENT(IN) :: Rain_meanz, Meanppt
! Local Variables
      INTEGER :: i, j, err_chk, nppt, nsta_used, ii
!dbg  INTEGER :: iy, id, im
      REAL :: ppt_sngl
      DOUBLE PRECISION :: intppt, intercept, sppt
      DOUBLE PRECISION :: sum_obs, sumppt, xppt, yppt, zppt
      DOUBLE PRECISION :: pptlapse1, pptlapse2, pptlapse3, ppt
!***********************************************************************
      pptlapse1 = Ppt_lapse(1)
      pptlapse2 = Ppt_lapse(2)
      pptlapse3 = Ppt_lapse(3)
!
! Code to check the rain_code parameter to determine if it is
! raining in the basin.
!
      Is_rain_day = 0
      IF ( Rain_code==1 ) THEN
        DO j = 1, Rain_nsta
          i = Rain_nuse(j)
          IF ( Precip(i)>0.0 ) Is_rain_day = 1
        ENDDO

      ELSEIF ( Rain_code==2 ) THEN
        DO i = 1, Nrain
          IF ( Precip(i)>0.0 ) Is_rain_day = 1
        ENDDO

      ELSEIF ( Rain_code==3 ) THEN
        Is_rain_day = 1

      ELSEIF ( Rain_code==4 ) THEN
        IF ( Rain_day==1 ) Is_rain_day = 1

      ELSEIF ( Rain_code==5 ) THEN
        DO i = 1, Nrain
          IF ( Psta_freq_nuse(i)==1 ) THEN
            IF ( Precip(i)>0.0 ) Is_rain_day = 1
          ENDIF
        ENDDO
      ENDIF
!
! add adjust_snow and adjust_rain here
!
      IF ( Rain_code==1 ) THEN
        nsta_used = Rain_nsta
      ELSE
        nsta_used = Nrain
      ENDIF

      Precip_xyz = Precip
      DO j = 1, nsta_used
        IF ( Rain_code==1 ) THEN
          i = Rain_nuse(j)
        ELSE
          i = j
        ENDIF

        IF ( Precip_xyz(i)<0.0 ) THEN
        !make sure negative precipitation values are < -99.0
          Precip_xyz(i) = -100.0
        ELSEIF ( Precip_xyz(i)>NEARZERO ) THEN
          IF ( Tmax_rain_sta(i)<=Tmax_allsnow ) THEN
            err_chk = 1
          ELSEIF ( Tmin_rain_sta(i)>Tmax_allsnow .OR.
     +             Tmax_rain_sta(i)>=Tmax_allrain ) THEN
            err_chk = 0
          ELSE
            err_chk = 1
          ENDIF

          IF ( err_chk==1 ) THEN
            Precip_xyz(i) = (Precip_xyz(i)*Adjust_snow) + Precip_xyz(i)
          ELSE
            Precip_xyz(i) = (Precip_xyz(i)*Adjust_rain) + Precip_xyz(i)
          ENDIF
        ELSE !ignore very small precipitation values
          Precip_xyz(i) = 0.0
        ENDIF

      ENDDO

      sumppt = 0.0D0
      nppt = 0
      xppt = 0.0D0
      yppt = 0.0D0
      zppt = 0.0D0

      DO j = 1, Rain_nsta
        i = Rain_nuse(j)
        IF ( Precip_xyz(i)>=0.0 ) THEN
          nppt = nppt + 1
!         sumppt = sumppt + ((Precip_xyz(i)+Ppt_add)/Ppt_div)
          sumppt = sumppt + Precip_xyz(i)
          xppt = xppt + Rain_STAx(i)
          yppt = yppt + Rain_STAy(i)
          zppt = zppt + Psta_elev(i)
        ENDIF
      ENDDO
!
! calculate means
!
      IF ( nppt>0 ) THEN
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
!
! adjust the values if not using all the stations
!
      IF ( ABS(Rain_meanz-zppt)>DNEARZERO ) THEN

        intercept = (sppt) -
     +              (pptlapse3*zppt) -
     +              (pptlapse1*xppt) -
     +              (pptlapse2*yppt)
        sppt = (pptlapse3*Rain_meanz) +
     +         (pptlapse1*Rain_meanx) +
     +         (pptlapse2*Rain_meany) + intercept

      ENDIF

      xppt = sppt

      intppt = xppt -
     +         (pptlapse3*Rain_meanz) -
     +         (pptlapse1*Rain_meanx) -
     +         (pptlapse2*Rain_meany)

      Basin_ppt = 0.0D0
      Basin_rain = 0.0D0
      Basin_snow = 0.0D0

      sum_obs = 0.0D0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        Pptmix(i) = 0
        Newsnow(i) = 0
        Hru_ppt(i) = 0.0
        Hru_rain(i) = 0.0
        Hru_snow(i) = 0.0
        Prmx(i) = 0.0

        IF ( Is_rain_day/=0 ) THEN
          ppt = (pptlapse1*MRUx(i)) + (pptlapse2*MRUy(i)) +
     +          (pptlapse3*MRUelev(i)) + intppt

          ppt = ppt*Ppt_div - Ppt_add

!******Ignore small amounts of precipitation on HRU
          IF ( ppt>DNEARZERO ) THEN
            IF ( Precip_units==1 ) ppt = ppt*MM2INCH
            ppt_sngl = SNGL(ppt)
            CALL precip_form(ppt_sngl, Hru_ppt(i), Hru_rain(i),
     +           Hru_snow(i), Tmaxf(i), Tminf(i), Pptmix(i),
     +           Newsnow(i), Prmx(i), Tmax_allrain, 1.0, 1.0,
     +           Adjmix_rain, Hru_area(i), sum_obs)

          ENDIF
        ENDIF
      ENDDO
      Basin_ppt = Basin_ppt*Basin_area_inv
      Basin_rain = Basin_rain*Basin_area_inv
      Basin_snow = Basin_snow*Basin_area_inv
      Basin_obs_ppt = sum_obs*Basin_area_inv

      END SUBROUTINE xyz_rain_run

!***********************************************************************
!     calculate the station mean by month
!***********************************************************************
      SUBROUTINE mean_by_month (PptMTH, TminMTH, TmaxMTH, Meanppt,
     +                          Meantmax, Meantmin, Rain_meanx,
     +                          Rain_meany, Rain_meanz, Temp_meanx,
     +                          Temp_meany, Temp_meanz)
      USE PRMS_XYZ_DIST, ONLY: Rain_nsta, Rain_nuse, Temp_nsta,
     +    Temp_nuse, Rain_STAx, Rain_STAy, Temp_STAx, Tmax_div,
     +    Temp_STAy, Ppt_add, Ppt_div, Tmin_add, Tmin_div, Tmax_add,
     +    X_div, Y_div, Z_div, X_add, Y_add, Z_add, Temp_STAelev
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Nrain, Tsta_elev, Psta_elev
      IMPLICIT NONE
      INTRINSIC FLOAT
! Arguments
      REAL, INTENT(IN) :: PptMTH(Nrain), TminMTH(Ntemp), TmaxMTH(Ntemp)
      DOUBLE PRECISION, INTENT(OUT) :: Meanppt, Meantmax, Meantmin
      DOUBLE PRECISION, INTENT(OUT) :: Rain_meanx, Rain_meany,Rain_meanz
      DOUBLE PRECISION, INTENT(OUT) :: Temp_meanx, Temp_meany,Temp_meanz
! Local Variables
      INTEGER :: i, j
      DOUBLE PRECISION :: rain_n, temp_n
!***********************************************************************
      rain_n = 1.0D0/FLOAT(Rain_nsta)
      temp_n = 1.0D0/FLOAT(Temp_nsta)

      Meanppt = 0.0D0
      Meantmax = 0.0D0
      Meantmin = 0.0D0
      Rain_meanx = 0.0D0
      Rain_meany = 0.0D0
      Rain_meanz = 0.0D0
      Temp_meanx = 0.0D0
      Temp_meany = 0.0D0
      Temp_meanz = 0.0D0

      DO j = 1, Rain_nsta
        i = Rain_nuse(j)

!       PptMTH(i) = (PptMTH(i)+Ppt_add)/Ppt_div

        Meanppt = Meanppt + PptMTH(i)
        Rain_meanx = Rain_meanx + Rain_STAx(i)
        Rain_meany = Rain_meany + Rain_STAy(i)
        Rain_meanz = Rain_meanz + Psta_elev(i)
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


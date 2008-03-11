!***********************************************************************
! Declares basin and HRU physical parameters and reservoirs
!***********************************************************************
      !put computation of ssres_area and gwres_area back in
      MODULE PRMS_BASIN
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Nhru, Nssr, Ngw, Nlakes, Ncascade, Ncascdgw
      CHARACTER(LEN=68) :: Versn_prms
!   Declared Variables
      INTEGER :: Xyz_flg, Prt_debug, Active_hrus, Active_gwrs
      REAL :: Land_area, Water_area, Basin_area_inv
      INTEGER, ALLOCATABLE :: Hru_route_order(:), Ncascade_hru(:)
      INTEGER, ALLOCATABLE :: Gwr_route_order(:), Ncascade_gwr(:)
      REAL, ALLOCATABLE :: Hru_perv(:), Hru_imperv(:), Ssres_area(:)
      REAL, ALLOCATABLE :: Hru_percent_perv(:), Gwres_area(:)
!   Declared Parameters
      INTEGER :: Print_debug !control parameter
!     INTEGER :: Elev_units
      REAL :: Basin_area
      INTEGER, ALLOCATABLE :: Hru_type(:)
      INTEGER, ALLOCATABLE :: Hru_ssres(:) !not needed if nhru=nssr
      INTEGER, ALLOCATABLE :: Hru_gwres(:) !not needed if nhru=ngw
      REAL, ALLOCATABLE :: Hru_area(:), Hru_percent_imperv(:)
!     REAL, ALLOCATABLE :: Hru_slope(:), Hru_elev(:)
      END MODULE PRMS_BASIN

!***********************************************************************
!     Main basin routine
!***********************************************************************
      INTEGER FUNCTION basin_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: basdecl, basinit
!***********************************************************************
      basin_prms = 0

      IF ( Arg.EQ.'declare' ) THEN
        basin_prms = basdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        basin_prms = basinit()
      ENDIF

      END FUNCTION basin_prms

!***********************************************************************
!     basdecl - set up parameters
!   Declared Parameters
!     print_debug, basin_area, hru_area, hru_percent_imperv, hru_type
!     hru_slope, hru_elev, elev_units
!***********************************************************************
      INTEGER FUNCTION basdecl()
      USE PRMS_BASIN
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      basdecl = 1

      Versn_prms =
     +'$Id: basin_prms.f 3871 2008-02-13 20:57:31Z rsregan $'
      IF ( declmodule(Versn_prms(:62)).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN
!     IF ( Nhru.GT.MAXHRU ) THEN
!       PRINT *, 'Error in basin_prms, MAXHRU < Nhru', MAXHRU, Nhru
!       RETURN
!     ENDIF

      Nssr = getdim('nssr')
      IF ( Nssr.EQ.-1 ) RETURN

      Ngw = getdim('ngw')
      IF ( Ngw.EQ.-1 ) RETURN
!     IF ( Ngw.GT.MAXGWR ) THEN
!       PRINT *, 'Error in basin_prms, MAXGWR < Ngw', MAXGWR, Ngw
!       RETURN
!     ENDIF

      Ncascade = getdim('ncascade')
      IF ( Ncascade.EQ.-1 ) RETURN

      Ncascdgw = getdim('ncascdgw')
      IF ( Ncascdgw.EQ.-1 ) RETURN

! Declared Variables
      ALLOCATE (Ssres_area(Nssr))
      IF ( declvar('basin', 'ssres_area', 'nssr', Nssr, 'real',
     +     'Area of each subsurface reservoir; computed by'//
     +     ' summing areas of HRUs that contribute to it',
     +     'acres',
     +     Ssres_area).NE.0 ) RETURN

      ALLOCATE (Gwres_area(Ngw))
      IF ( declvar('basin', 'gwres_area', 'ngw', Ngw, 'real',
     +     'Area of each groundwater reservoir.  Computed by'//
     +     ' summing areas of HRUs that contribute to it',
     +     'acres',
     +     Gwres_area).NE.0 ) RETURN

      ALLOCATE (Hru_perv(Nhru))
      IF ( declvar('basin', 'hru_perv', 'nhru', Nhru, 'real',
     +     'Pervious area of each HRU',
     +     'acres',
     +     Hru_perv).NE.0 ) RETURN

      ALLOCATE (Hru_imperv(Nhru))
      IF ( declvar('basin', 'hru_imperv', 'nhru', Nhru, 'real',
     +     'Impervious area of each HRU',
     +     'acres',
     +     Hru_imperv).NE.0 ) RETURN

      ALLOCATE (Hru_percent_perv(Nhru))
      IF ( declvar('basin', 'hru_percent_perv', 'nhru', Nhru, 'real',
     +     'Proportion of each HRU area that is pervious',
     +     'decimal fraction',
     +     Hru_percent_perv).NE.0 ) RETURN

      IF ( declvar('basin', 'land_area', 'one', 1, 'real',
     +     'Basin area composed of land.',
     +     'acres',
     +     Land_area).NE.0 ) RETURN

      IF ( declvar('basin', 'water_area', 'one', 1, 'real',
     +     'Basin area composed of water bodies',
     +     'acres',
     +     Water_area).NE.0 ) RETURN

      IF ( declvar('basin', 'basin_area_inv', 'one', 1, 'real',
     +     'Inverse of total basin area as sum of HRU areas',
     +     '1/acres',
     +     Basin_area_inv).NE.0 ) RETURN

! Declare variables used by modules that include cascade routing
      ALLOCATE (Hru_route_order(Nhru))
      IF ( declvar('basin', 'hru_route_order', 'nhru', Nhru,
     +     'integer',
     +     'Routing order for HRUs',
     +     'none',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( Ncascade>0 ) THEN
        ALLOCATE (Ncascade_hru(Nhru))
        IF ( declvar('basin', 'ncascade_hru', 'nhru', Nhru, 'integer',
     +       'Number of cascade areas in each HRU',
     +       'none',
     +       Ncascade_hru).NE.0 ) RETURN
      ENDIF

      ALLOCATE (Gwr_route_order(Ngw))
      IF ( declvar('basin', 'gwr_route_order', 'ngw', Ngw, 'integer',
     +     'Routing order for ground-water reservoirs',
     +     'none',
     +     Gwr_route_order).NE.0 ) RETURN

      IF ( Ncascade>0 ) THEN
        ALLOCATE (Ncascade_gwr(Ngw))
        IF ( declvar('basin', 'ncascade_gwr', 'ngw', Ngw, 'integer',
     +       'Number of cascade areas in each ground-water reservoir',
     +       'none',
     +       Ncascade_gwr).NE.0 ) RETURN
      ENDIF

      IF ( declvar('basin', 'xyz_flg', 'one', 1, 'integer',
     +     'Flag to indicate if XYZ climate distribution is being used',
     +     'none',
     +     Xyz_flg).NE.0 ) RETURN

      IF ( declvar('basin', 'prt_debug', 'one', 1, 'integer',
     +     'Flag to indicate type of debug output',
     +     'none',
     +     Prt_debug).NE.0 ) RETURN

      IF ( declvar('basin', 'active_hrus', 'one', 1, 'integer',
     +     'Number of active HRUs',
     +     'none',
     +     Active_hrus).NE.0 ) RETURN

      IF ( Ngw.EQ.Nhru ) THEN
        IF ( declvar('basin', 'active_gwrs', 'one', 1, 'integer',
     +       'Number of active GWRs',
     +       'none',
     +       Active_gwrs).NE.0 ) RETURN
      ENDIF

! Declared Parameters
      IF ( declparam('basin', 'basin_area', 'one', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'Basin area', 'Total basin area',
     +     'acres').NE.0 ) RETURN

!rsr: declare hru_elev, hru_slope, and elev_units to preserve values in
!     parameter file, not used in this module, so don't allocate
!     ALLOCATE (Hru_elev(Nhru))
      IF ( declparam('basin', 'hru_elev', 'nhru', 'real',
     +     '0.', '-300.', '30000',
     +     'HRU mean elevation', 'Mean elevation of each HRU',
     +     'elev_units').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('basin', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

!     ALLOCATE (Hru_slope(Nhru))
      IF ( declparam('basin', 'hru_slope', 'nhru', 'real',
     +     '0.0', '0.0', '10.0',
     +     'HRU slope',
     +     'Slope of each HRU, specified as change in vertical length'//
     +     ' divided by change in horizontal length',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_percent_imperv(Nhru))
      IF ( declparam('basin', 'hru_percent_imperv', 'nhru', 'real',
     +     '0.0', '0.0', '1.0',
     +     'HRU percent impervious',
     +     'Proportion of each HRU area that is impervious',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('basin', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '2',
     +     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('basin', 'elev_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Elevation units flag',
     +     'Flag to indicate the units of the elevation values'//
     +     ' (0=feet; 1=meters)',
     +     'none').NE.0 ) RETURN

      IF ( Nhru.NE.Nssr ) THEN
        ALLOCATE (Hru_ssres(Nhru))
        IF ( declparam('basin', 'hru_ssres', 'nhru', 'integer',
     +       '1', 'bounded', 'nssr',
     +       'Index of subsurface reservoir assigned to HRU',
     +       'Index of subsurface reservoir receiving excess water'//
     +       ' from HRU soil zone',
     +       'none').NE.0 ) RETURN
      ENDIF

      IF ( Nhru.NE.Ngw ) THEN
        ALLOCATE (Hru_gwres(Nhru))
        IF ( declparam('basin', 'hru_gwres', 'nhru', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'Index of groundwater reservoir assigned to HRU',
     +       'Index of groundwater reservoir receiving excess soil'//
     +       ' water from each HRU',
     +       'none').NE.0 ) RETURN
      ENDIF

      basdecl = 0
      END FUNCTION basdecl

!**********************************************************************
!     basinit - check for validity of basin parameters
!               and compute reservoir areas
!**********************************************************************
      INTEGER FUNCTION basinit()
      USE PRMS_BASIN
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      INTRINSIC ABS
! Local Variables
      CHARACTER(LEN=50) :: buffer
      INTEGER :: i, j, k, datetime(6)
      REAL :: totarea, diff, active_area
!**********************************************************************
      basinit = 1

      IF ( getparam('basin', 'basin_area', 1, 'real', Basin_area)
     +     .NE.0 ) RETURN

      IF ( getparam('basin', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getparam('basin', 'hru_percent_imperv', Nhru, 'real',
     +     Hru_percent_imperv).NE.0 ) RETURN

      ! debug print flag:
      ! 0=none; 1=water balances; 2=basin; 3=obs;
      ! 4=basin_sum; 5=soltab; 6=potet; 7=soil zone;
      ! 8=xyz; 9=snow; 10=grnampt; 11=krout; 12=GSF timing;
      ! 13=cascade messages
      IF ( control_integer(Print_debug, 'print_debug').NE.0 ) RETURN
      Prt_debug = Print_debug

      IF ( getparam('basin', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN

      Xyz_flg = 0    ! must be set to one in XYZ module
      totarea = 0.0
      Land_area = 0.0
      Water_area = 0.0
      Ncascade_hru = 0
      Ncascade_gwr = 0
      Hru_imperv = 0.0
      Hru_perv = 0.0
      Hru_percent_perv = 0.0
      Hru_route_order = 0
      Nlakes = 0

      j = 0
      DO i = 1, Nhru
        IF ( Hru_type(i).NE.0 ) THEN
          j = j + 1
          Hru_route_order(j) = i
          IF ( Hru_type(i).EQ.1 ) THEN
            Hru_imperv(i) = Hru_percent_imperv(i)*Hru_area(i)
            Hru_perv(i) = Hru_area(i) - Hru_imperv(i)
            Land_area = Land_area + Hru_area(i)
          ELSE
            IF ( Hru_percent_imperv(i).GT.0.0 ) PRINT *, 'Lake HRU:', i,
     +           ' specified as having impervious area, set to 0.0'
            Water_area = Water_area + Hru_area(i)
            Nlakes = Nlakes + 1
          ENDIF
        ENDIF
        totarea = totarea + Hru_area(i)
        Hru_percent_perv(i) = 1.0 - Hru_percent_imperv(i)
      ENDDO
      Active_hrus = j
      active_area = Land_area + Water_area

      IF ( Nssr.EQ.Nhru ) THEN
        Ssres_area = Hru_area
      ELSE
        IF ( getparam('basin', 'hru_ssres', Nhru, 'integer', Hru_ssres)
     +       .NE.0 ) RETURN
        Ssres_area = 0.0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          j = Hru_ssres(i)
          ! assume if hru_type is 2, SSR has zero area
          IF ( Hru_type(i).EQ.1 ) Ssres_area(j) = Ssres_area(j) +
     +                                            Hru_area(i)
        ENDDO
      ENDIF

      IF ( Ngw.EQ.Nhru ) THEN
        Gwres_area = Hru_area
        j = 0
        DO i = 1, Ngw
          IF ( Hru_type(i).NE.0 ) THEN
            j = j + 1
            Gwr_route_order(j) = i
          ENDIF
        ENDDO
        Active_gwrs = j
      ELSE
        DO i = 1, Ngw
          Gwr_route_order(i) = i
        ENDDO
        Active_gwrs = Ngw
        IF ( getparam('basin', 'hru_gwres', Nhru, 'integer', Hru_gwres)
     +       .NE.0 ) RETURN
        Gwres_area = 0.0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          j = Hru_gwres(i)
          Gwres_area(j) = Gwres_area(j) + Hru_area(i)
        ENDDO      
      ENDIF

      diff = (totarea-Basin_area)/Basin_area
      !basin_area not used anymore, so not important
!     IF ( ABS(diff).GT.0.01 ) THEN
!       WRITE (*, '(//,2(A,F15.7))') 'sum of HRUs =', totarea,
!    +                               ' basin_area =', Basin_area
!       CALL dpstr('Sum of hru areas is not equal to basin area', 0)
!       RETURN
!     ENDIF
!     Basin_area_inv = 1.0/totarea
      Basin_area_inv = 1.0/active_area

      IF ( Prt_debug.EQ.2 ) THEN
        PRINT *, ' HRU     Area'
        PRINT 9001, (i, Hru_area(i), i=1, Nhru)
        PRINT 9004, 'sum of HRUs = ', totarea, ' basin_area = ',
     +              Basin_area, 'hru_imperv = ', Hru_imperv,
     +             'hru_perv = ', Hru_perv, 'totalarea = ', totarea,
     +             'diff =', diff
      ENDIF

!     print out start and end times
      CALL opstr(' Surface Water and Energy Budgets Simulated by'//
     +           ' PRMS Version 2.'//Versn_prms(19:33))
      CALL dattim('start', datetime)
      WRITE (buffer, 9002) ' Start time: ', datetime
      CALL opstr(buffer(:32))
      CALL dattim('end', datetime)
      WRITE (buffer, 9002) ' End time:   ', datetime
      CALL opstr(buffer(:32))
      WRITE (buffer, 9003) ' Sum of HRUs:', totarea, '  Basin_area:',
     +                     Basin_area
      CALL opstr(buffer)

      basinit = 0

 9001 FORMAT (I4, F14.5)
 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
 9003 FORMAT (A, F12.2, A, F12.2)
 9004 FORMAT (A, F14.5)

      END FUNCTION basinit

!***********************************************************************
! Defines shared watershed-wide and HRU physical parameters and variables
!***********************************************************************
      MODULE PRMS_BASIN
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsfres, Nsegment, Nsegmentp1
      INTEGER, SAVE :: Numlakes, Timestep, Starttime(6), Endtime(6)
      REAL, PARAMETER :: NEARZERO = 1.0E-8
      DOUBLE PRECISION, PARAMETER :: DNEARZERO = 1.0D-12
      REAL, PARAMETER :: CFS2CMS_CONV = 0.028316847
      REAL, PARAMETER :: INCH2MM = 25.4, INCH2M = .0254, INCH2CM = 2.54
      REAL, SAVE :: Cfs2inches, Land_area, Water_area
      CHARACTER(LEN=68), SAVE :: Versn_prms
      INTEGER, SAVE, ALLOCATABLE :: Gwr_type(:)
!   Declared Variables
      INTEGER, SAVE :: Active_hrus, Active_gwrs
      REAL, SAVE :: Basin_area_inv, Basin_cfs, Basin_cms, Basin_stflow
      REAL, SAVE :: Basin_ssflow_cfs, Basin_gwflow_cfs, Basin_sroff_cfs
      INTEGER, SAVE, ALLOCATABLE :: Hru_route_order(:)
      INTEGER, SAVE, ALLOCATABLE :: Gwr_route_order(:)
      REAL, SAVE, ALLOCATABLE :: Hru_perv(:), Hru_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Hru_percent_perv(:), Gwres_area(:)
      REAL, SAVE, ALLOCATABLE :: Hru_percent_impv(:), Ssres_area(:)
!   Declared Parameters
      INTEGER, SAVE :: Print_debug !control parameter
!     INTEGER, SAVE :: Elev_units
      INTEGER, SAVE, ALLOCATABLE :: Hru_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Hru_ssres(:), Hru_gwres(:)
      REAL, SAVE :: Basin_area
      REAL, SAVE, ALLOCATABLE :: Hru_area(:), Hru_percent_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Hru_elev(:)
      END MODULE PRMS_BASIN

!***********************************************************************
!     Main basin routine
!***********************************************************************
      INTEGER FUNCTION basin_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basdecl, basinit
!***********************************************************************
      basin_prms = 0

      IF ( Process_flag==1 ) THEN
        basin_prms = basdecl()
      ELSEIF ( Process_flag==2 ) THEN
        basin_prms = basinit()
      ENDIF

      END FUNCTION basin_prms

!***********************************************************************
!     basdecl - set up parameters
!   Declared Parameters
!     print_debug, hru_area, hru_percent_imperv, hru_type, hru_elev
!***********************************************************************
      INTEGER FUNCTION basdecl()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Model
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, getdim, declparam, declvar
!***********************************************************************
      basdecl = 1

      Versn_prms =
     +'$Id: basin_prms.f 2447 2011-02-15 20:03:52Z rsregan $'
      IF ( declmodule(Versn_prms(:62)).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Nssr = getdim('nssr')
      IF ( Nssr.EQ.-1 ) RETURN

      Ngw = getdim('ngw')
      IF ( Ngw.EQ.-1 ) RETURN
      ! make sure nhru=nssr=ngw for GSFLOW mode
      IF ( Model==0 ) THEN
        IF ( Nssr/=Nhru ) PRINT *, 'Basin_prms:',
     +       ' nssr not equal nhru in GSFLOW model, nssr set to nhru'
        IF ( Ngw/=Nhru ) PRINT *, 'Basin_prms:',
     +       ' ngw not equal nhru in GSFLOW model, ngw set to nhru'
        Nssr = Nhru
        Ngw = Nhru
      ENDIF

      Nsegment = getdim('nsegment')
      IF ( Nsegment.EQ.-1 ) RETURN
      Nsegmentp1 = Nsegment + 1

      Nsfres = getdim('nsfres')
      IF ( Nsfres.EQ.-1 ) RETURN

! Declared Variables
      IF ( declvar('basin', 'basin_cfs', 'one', 1, 'real',
     +     'Streamflow from basin',
     +     'cfs',
     +     Basin_cfs).NE.0 ) RETURN

      IF ( declvar('basin', 'basin_cms', 'one', 1, 'real',
     +     'Streamflow from basin',
     +     'cms',
     +     Basin_cms).NE.0 ) RETURN

      IF ( declvar('basin', 'basin_stflow', 'one', 1, 'real',
     +     'Sum of basin_sroff, basin_ssflow and basin_gwflow for'//
     +     ' timestep',
     +     'inches',
     +     Basin_stflow).NE.0 ) RETURN

      IF ( declvar('basin', 'basin_sroff_cfs', 'one', 1, 'real',
     +     'Basin surface runoff for timestep ',
     +     'cfs',
     +     Basin_sroff_cfs).NE.0 ) RETURN

      IF ( declvar('basin', 'basin_ssflow_cfs', 'one', 1, 'real',
     +     'Basin subsurface flow for timestep',
     +     'cfs',
     +     Basin_ssflow_cfs).NE.0 ) RETURN

      IF ( declvar('basin', 'basin_gwflow_cfs', 'one', 1, 'real',
     +     'Basin ground-water flow for timestep',
     +     'cfs',
     +     Basin_gwflow_cfs).NE.0 ) RETURN

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

      ALLOCATE (Hru_percent_impv(Nhru))
      IF ( declvar('basin', 'hru_percent_impv', 'nhru', Nhru, 'real',
     +     'Proportion of each HRU area that is impervious',
     +     'decimal fraction',
     +     Hru_percent_impv).NE.0 ) RETURN

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

      ALLOCATE (Gwr_route_order(Ngw))
      IF ( declvar('basin', 'gwr_route_order', 'ngw', Ngw, 'integer',
     +     'Routing order for ground-water reservoirs',
     +     'none',
     +     Gwr_route_order).NE.0 ) RETURN

      IF ( declvar('basin', 'active_hrus', 'one', 1, 'integer',
     +     'Number of active HRUs',
     +     'none',
     +     Active_hrus).NE.0 ) RETURN

      IF ( declvar('basin', 'active_gwrs', 'one', 1, 'integer',
     +     'Number of active GWRs',
     +     'none',
     +     Active_gwrs).NE.0 ) RETURN

! Declared Parameters
      IF ( declparam('basin', 'basin_area', 'one', 'real',
     +     '0.0', '0.0', '1e+09',
     +     'Basin area', 'Total basin area',
     +     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('basin', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

!     IF ( decl param('basin', 'elev_units', 'one', 'integer',
!    +     '0', '0', '1',
!    +     'Elevation units flag',
!    +     'Flag to indicate the units of the elevation values'//
!    +     ' (0=feet; 1=meters)',
!    +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_elev(Nhru))
      IF ( declparam('basin', 'hru_elev', 'nhru', 'real',
     +     '0.0', '-1000.0', '30000.0',
     +     'HRU mean elevation', 'Mean elevation for each HRU',
     +     'elev_units').NE.0 ) RETURN

      ALLOCATE (Hru_percent_imperv(Nhru))
      IF ( declparam('basin', 'hru_percent_imperv', 'nhru', 'real',
     +     '0.0', '0.0', '0.99',
     +     'HRU percent impervious',
     +     'Proportion of each HRU area that is impervious',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru), Gwr_type(Ngw))
      IF ( declparam('basin', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '3',
     +     'HRU type',
     +     'Type of each HRU (0=inactive; 1=land; 2=lake; 3=swale)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_ssres(Nhru))
      IF ( Nhru.NE.Nssr .OR. Model==99 ) THEN
        IF ( declparam('basin', 'hru_ssres', 'nhru', 'integer',
     +       '1', 'bounded', 'nssr',
     +       'Index of subsurface reservoir assigned to HRU',
     +       'Index of subsurface reservoir receiving excess water'//
     +       ' from HRU soil zone',
     +       'none').NE.0 ) RETURN
      ENDIF

      ALLOCATE (Hru_gwres(Nhru))
      IF ( Nhru.NE.Ngw .OR. Model==99 ) THEN
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
!      USE PRMS_MODULE, ONLY: Lake_flag
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam, getstep, control_integer
      EXTERNAL dattim
      INTRINSIC ABS
      EXTERNAL check_imperv
! Local Variables
      CHARACTER(LEN=64) :: buffer
      INTEGER :: i, j, k
!     INTEGER :: ierr
      REAL :: totarea, active_area, diff, harea
!**********************************************************************
      basinit = 1

      CALL dattim('start', Starttime)
      CALL dattim('end', Endtime)

      IF ( getparam('basin', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getparam('basin', 'basin_area', 1, 'real', Basin_area)
     +     .NE.0 ) RETURN

      IF ( getparam('basin', 'hru_elev', Nhru, 'real', Hru_elev)
     +     .NE.0 ) RETURN

!     IF ( get param('basin', 'elev_units', 1, 'integer', Elev_units)
!    +     .NE.0 ) RETURN

      IF ( getparam('basin', 'hru_percent_imperv', Nhru, 'real',
     +     Hru_percent_imperv).NE.0 ) RETURN

      Timestep = getstep()
      IF ( Timestep==0 ) THEN
        Basin_cfs = 0.0
        Basin_cms = 0.0
        Basin_stflow = 0.0
        Basin_ssflow_cfs = 0.0
        Basin_sroff_cfs = 0.0
        Basin_gwflow_cfs = 0.0
      ENDIF

      ! debug print flag:
      ! 0=none; 1=water balances; 2=basin; 3=obs;
      ! 4=basin_sum; 5=soltab; 6=potet; 7=soil zone;
      ! 8=xyz; 9=snow; 10=grnampt; 11=krout; 12=GSF timing;
      ! 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug').NE.0 ) RETURN

      IF ( getparam('basin', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN
      Gwr_type = 1

      totarea = 0.0
      Land_area = 0.0
      Water_area = 0.0

      Numlakes = 0
!      ierr = 0
      j = 0
      Hru_route_order = 0
      DO i = 1, Nhru
        Hru_imperv(i) = 0.0
        Hru_perv(i) = 0.0
        Hru_percent_perv(i) = 0.0
        Hru_percent_impv(i) = 0.0

        harea = Hru_area(i)
        IF ( Hru_type(i).NE.0 ) THEN
          j = j + 1
          Hru_route_order(j) = i
          IF ( Hru_type(i).EQ.2 ) THEN
            Water_area = Water_area + harea
            Numlakes = Numlakes + 1
          ELSE
            CALL check_imperv(i, Hru_percent_imperv(i))
            Hru_imperv(i) = Hru_percent_imperv(i)*harea
            Hru_perv(i) = harea - Hru_imperv(i)
            Hru_percent_impv(i) = Hru_percent_imperv(i)
            Land_area = Land_area + harea
            Hru_percent_perv(i) = 1.0 - Hru_percent_imperv(i)
          ENDIF
        ENDIF
        totarea = totarea + harea
      ENDDO

!      IF ( Lake_flag==1 ) THEN
!        IF ( Numlakes>0 .AND. Nsfres==0 ) THEN
!          PRINT *,'Error, specified that lakes are present but nsfres=0'
!          ierr = 1
!        ENDIF
!        IF ( Numlakes/=Nsfres ) THEN
!          PRINT *, 'Error, number of lakes specified in hru_type'
!          PRINT *, ' does not equal nsfres:', Nsfres, ' numlakes:',
!     +             Numlakes
!          ierr = 1
!        ENDIF
!      ENDIF
!      IF ( ierr==1 ) RETURN
      diff = (totarea - Basin_area)/Basin_area
      IF ( Basin_area>0.0 .AND. ABS(diff)>0.01 )
     +     PRINT 9005, Basin_area, totarea, diff*100.0
      Active_hrus = j
      active_area = Land_area + Water_area

      IF ( Nssr.EQ.Nhru ) THEN
        Ssres_area = Hru_area
        DO i = 1, Nhru
          Hru_ssres(i) = i
        ENDDO
      ELSE
        IF ( getparam('basin', 'hru_ssres', Nhru, 'integer', Hru_ssres)
     +       .NE.0 ) RETURN
        Ssres_area = 0.0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          j = Hru_ssres(i)
          ! assume if hru_type is 2, SSR has zero area
          IF ( Hru_type(i).NE.2 )
     +         Ssres_area(j) = Ssres_area(j) + Hru_area(i)
        ENDDO
      ENDIF

      IF ( Ngw.EQ.Nhru ) THEN
        Gwres_area = Hru_area
        Active_gwrs = Active_hrus
        Gwr_route_order = Hru_route_order
        DO i = 1, Nhru
          Hru_gwres(i) = i
        ENDDO
      ELSE
        Gwres_area = 0.0
        DO i = 1, Ngw
          Gwr_route_order(i) = i
        ENDDO
        Active_gwrs = Ngw
        IF ( getparam('basin', 'hru_gwres', Nhru, 'integer', Hru_gwres)
     +       .NE.0 ) RETURN
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          j = Hru_gwres(i)
          Gwres_area(j) = Gwres_area(j) + Hru_area(i)
        ENDDO      
      ENDIF
!     Basin_area_inv = 1.0/totarea
      Basin_area_inv = 1.0/active_area
      Cfs2inches = Basin_area_inv*12.0*86400.0/43560.0

      IF ( Print_debug.EQ.2 ) THEN
        PRINT *, ' HRU     Area'
        PRINT 9001, (i, Hru_area(i), i=1, Nhru)
        PRINT 9004, 'Sum of HRU areas = ', totarea,
     +              'Active basin area = ', Active_area,
     +              'Sum of impervious area in basin = ', Hru_imperv,
     +              'Sum of pervious area in basin = ', Hru_perv
      ENDIF

!     print out start and end times
      CALL opstr(' Surface Water and Energy Budgets Simulated by'//
     +           ' PRMS Version 2.'//Versn_prms(19:33))
      WRITE (buffer, 9002) ' Start time: ', Starttime
      CALL opstr(buffer(:32))
      WRITE (buffer, 9002) ' End time:   ', Endtime
      CALL opstr(buffer(:32))
      WRITE (buffer, 9003) ' Sum of HRU areas:', totarea,
     +                     '  Active basin area:', Active_area
      CALL opstr(buffer)

      basinit = 0

 9001 FORMAT (I4, F14.5)
 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
 9003 FORMAT (A, F12.2, A, F12.2)
 9004 FORMAT (A, F14.5)
 9005 FORMAT ('WARNING, basin_area>1% different than sum of HRU areas',
     +        ': basin_area:', F12.3 , ' sum of HRU areas:', F12.3,
     +        ' percent diff:', F12.8)

      END FUNCTION basinit

!**********************************************************************
!     check impervious percent for validity
!**********************************************************************
      SUBROUTINE check_imperv(Ihru, Hru_percent_imperv)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(INOUT) :: Hru_percent_imperv
!**********************************************************************
      IF ( Hru_percent_imperv>0.99 ) THEN
        PRINT *, 'Warning, hru_percent_imperv > .99 for HRU:', Ihru,
     +           ' reset to .99, was:', Hru_percent_imperv
        Hru_percent_imperv = 0.99
      ENDIF

      END SUBROUTINE check_imperv

!***********************************************************************
! Declares and initializes climate parameters and variables
!***********************************************************************
      MODULE PRMS_CLIMATEVARS
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Ntemp, Nrain, Nsol
!   Declared Variables - Precip
      INTEGER, SAVE, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      REAL, SAVE :: Basin_ppt, Basin_obs_ppt, Basin_rain, Basin_snow
      REAL, SAVE, ALLOCATABLE :: Hru_ppt(:), Hru_rain(:), Hru_snow(:)
      REAL, SAVE, ALLOCATABLE :: Prmx(:)
!   Declared Variables - Temp
      REAL, SAVE :: Basin_temp, Basin_tmax, Basin_tmin
      REAL, SAVE :: Solrad_tmax, Solrad_tmin
      REAL, SAVE, ALLOCATABLE :: Tmaxf(:), Tminf(:), Tavgf(:)
      REAL, SAVE, ALLOCATABLE :: Tmaxc(:), Tminc(:), Tavgc(:)
!   Declared Variables - Transp
      INTEGER, SAVE :: Basin_transp_on
      INTEGER, SAVE, ALLOCATABLE :: Transp_on(:)
!   Declared Variables - Potetential ET
      REAL, SAVE :: Basin_potet
      REAL, SAVE, ALLOCATABLE :: Potet(:)
!   Declared Parameters and Variables - Solar Radiation
      INTEGER, SAVE :: Basin_solsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_solsta(:)
      REAL, SAVE :: Basin_horad, Basin_potsw, Orad, Radmax, Rad_conv
      REAL, SAVE :: Radj_sppt, Radj_wppt
      REAL, SAVE, ALLOCATABLE :: Swrad(:), Ppt_rad_adj(:)
!   Declared Parameters - Temp
      INTEGER, SAVE :: Temp_units, Basin_tsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_tsta(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_elev(:), Tmax_adj(:), Tmin_adj(:)
!   Declared Parameters - Precip
      INTEGER, SAVE :: Precip_units
      REAL, SAVE :: Tmax_allsnow
      REAL, SAVE, ALLOCATABLE :: Tmax_allrain(:), Psta_elev(:)
      REAL, SAVE, ALLOCATABLE :: Adjmix_rain(:), Adjust_snow(:)
      REAL, SAVE, ALLOCATABLE :: Adjust_rain(:)
      END MODULE PRMS_CLIMATEVARS

!***********************************************************************
!     Main climate_vars routine
!***********************************************************************
      INTEGER FUNCTION climate_vars_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: climatevarsdecl, climatevarsinit
!***********************************************************************
      climate_vars_prms = 0

      IF ( Process_flag==1 ) THEN
        climate_vars_prms = climatevarsdecl()
      ELSEIF ( Process_flag==2 ) THEN
        climate_vars_prms = climatevarsinit()
      ENDIF

      END FUNCTION climate_vars_prms

!***********************************************************************
!     climatevarsdecl - declare temperature variables and parameters
!   Declared Parameters
!     tsta_elev, tmax_adj, tmin_adj, temp_units, Basin_tsta
!***********************************************************************
      INTEGER FUNCTION climatevarsdecl()
      USE PRMS_CLIMATEVARS
      USE PRMS_MODULE, ONLY: Temp_flag, Precip_flag, Model
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, getdim, declvar, declparam
!***********************************************************************
      climatevarsdecl = 1

      IF ( declmodule(
     +'$Id: basin_prms.f 2447 2011-02-15 20:03:52Z rsregan $'
     +).NE.0 ) RETURN

      Ntemp = getdim('ntemp')
      IF ( Ntemp.EQ.-1 ) RETURN

      Nrain = getdim('nrain')
      IF ( Nrain.EQ.-1 ) RETURN

      Nsol = getdim('nsol')
      IF ( Nsol.EQ.-1 ) RETURN

      ALLOCATE (Tmaxf(Nhru))
      IF ( declvar('temp', 'tmaxf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily maximum temperature',
     +     'degrees F',
     +     Tmaxf).NE.0 ) RETURN

      ALLOCATE (Tminf(Nhru))
      IF ( declvar('temp', 'tminf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily minimum temperature',
     +     'degrees F',
     +     Tminf).NE.0 ) RETURN

      ALLOCATE (Tavgf(Nhru))
      IF ( declvar('temp', 'tavgf', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily average temperature',
     +     'degrees F',
     +     Tavgf).NE.0 ) RETURN

      ALLOCATE (Tmaxc(Nhru))
      IF ( declvar('temp', 'tmaxc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily maximum temperature',
     +     'degrees Celsius',
     +     Tmaxc).NE.0 ) RETURN

      ALLOCATE (Tminc(Nhru))
      IF ( declvar('temp', 'tminc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily minimum temperature',
     +     'degrees Celsius',
     +     Tminc).NE.0 ) RETURN

      ALLOCATE (Tavgc(Nhru))
      IF ( declvar('temp', 'tavgc', 'nhru', Nhru, 'real',
     +     'HRU adjusted daily average temperature',
     +     'degrees Celsius',
     +     Tavgc).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_tmax', 'one', 1, 'real',
     +     'Basin area-weighted daily maximum temperature',
     +     'degrees',
     +     Basin_tmax).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_tmin', 'one', 1, 'real',
     +     'Basin area-weighted daily minimum temperature',
     +     'degrees',
     +     Basin_tmin).NE.0 ) RETURN

      IF ( declvar('temp', 'basin_temp', 'one', 1, 'real',
     +     'Basin area-weighted average air temperature',
     +     'degrees',
     +     Basin_temp).NE.0 ) RETURN

      IF ( declvar('temp', 'solrad_tmax', 'one', 1, 'real',
     +     'Basin daily maximum temperature for use with solrad'//
     +     ' radiation',
     +     'degrees',
     +     Solrad_tmax).NE.0 ) RETURN

      IF ( declvar('temp', 'solrad_tmin', 'one', 1, 'real',
     +     'Basin daily minimum temperature for use with solrad'//
     +     ' radiation',
     +     'degrees',
     +     Solrad_tmin).NE.0 ) RETURN

      ALLOCATE (Tsta_elev(Ntemp))
      IF ( declparam('temp', 'tsta_elev', 'ntemp', 'real',
     +     '0', '-300.', '30000.',
     +     'Temperature station elevation',
     +     'Elevation of each temperature measurement station',
     +     'elev_units').NE.0 ) RETURN

      IF ( declparam('temp', 'temp_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for measured temperature',
     +     'Units for measured temperature (0=Fahrenheit; 1=Celsius)',
     +     'none').NE.0 ) RETURN

      IF ( Temp_flag<5 .OR. Model==99 ) THEN
        IF ( declparam('temp', 'basin_tsta', 'one', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'Index of main temperature station',
     +       'Index of temperature station used to compute basin'//
     +       ' temperature values',
     +       'none').NE.0 ) RETURN
      ENDIF

      IF ( Temp_flag<4 .OR. Model==99 ) THEN
        ALLOCATE (Hru_tsta(Nhru))
        IF ( declparam('temp', 'hru_tsta', 'nhru', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'Index of base temperature station for HRU',
     +       'Index of the base temperature station used for lapse'//
     +       ' rate calculations',
     +       'none').NE.0 ) RETURN
      ENDIF

      IF ( Temp_flag/=4 .OR. Model==99 ) THEN
        ALLOCATE (Tmax_adj(Nhru))
        IF ( declparam('temp', 'tmax_adj', 'nhru', 'real',
     +       '0.0', '-10.', '10.0',
     +       'HRU maximum temperature adjustment',
     +       'Adjustment to maximum temperature for each HRU,'//
     +       ' estimated based on slope and aspect',
     +       'degrees').NE.0 ) RETURN

        ALLOCATE (Tmin_adj(Nhru))
        IF ( declparam('temp', 'tmin_adj', 'nhru', 'real',
     +       '0.0', '-10.', '10.0',
     +       'HRU minimum temperature adjustment',
     +       'Adjustment to minimum temperature for each HRU,'//
     +       ' estimated based on slope and aspect',
     +       'degrees').NE.0 ) RETURN
      ENDIF

! PRECIP VARIABLES AND PARAMETERS
      ALLOCATE (Pptmix(Nhru))
      IF ( declvar('precip', 'pptmix', 'nhru', Nhru, 'integer',
     +     'Precipitation mixture (0=no; 1=yes)',
     +     'none',
     +     Pptmix).NE.0 ) RETURN

      ALLOCATE (Newsnow(Nhru))
      IF ( declvar('precip', 'newsnow', 'nhru', Nhru, 'integer',
     +     'New snow on HRU (0=no; 1=yes)',
     +     'none',
     +     Newsnow).NE.0 ) RETURN

      ALLOCATE (Prmx(Nhru))
      IF ( declvar('ide_dist', 'prmx', 'nhru', Nhru, 'real',
     +     'Proportion of rain in a mixed event',
     +     'decimal fraction',
     +     Prmx).NE.0 ) RETURN

      IF ( declvar('precip', 'basin_rain', 'one', 1, 'real',
     +     'Area weighted adjusted average rain for basin',
     +     'inches',
     +     Basin_rain).NE.0 ) RETURN

      IF ( declvar('precip', 'basin_snow', 'one', 1, 'real',
     +     'Area weighted adjusted average snow for basin',
     +     'inches',
     +     Basin_snow).NE.0 ) RETURN

      IF ( declvar('precip', 'basin_ppt', 'one', 1, 'real',
     +     'Area weighted adjusted average precip for basin',
     +     'inches',
     +     Basin_ppt).NE.0 ) RETURN

! DANGER - Not sure what to do about this one.  For right now
!          I'm setting basin_ppt and basin_obs_ppt to the same
!          variable.  In the precip_prms module, basin_obs_ppt
!          seems to be the area weighted precip average before
!          the correction factor is applied.  In other modules,
!          the correction "error" is applied to the station
!          precip rather than the hru precip.
      IF ( declvar('precip', 'basin_obs_ppt', 'one', 1, 'real',
     +     'Area weighted measured average precip for basin',
     +     'inches',
     +     Basin_obs_ppt).NE.0 ) RETURN

      ALLOCATE (Hru_ppt(Nhru))
      IF ( declvar('precip', 'hru_ppt', 'nhru', Nhru, 'real',
     +     'Adjusted precipitation on each HRU',
     +     'inches',
     +     Hru_ppt).NE.0 ) RETURN

      ALLOCATE (Hru_rain(Nhru))
      IF ( declvar('precip', 'hru_rain', 'nhru', Nhru, 'real',
     +     'Computed rain on each HRU',
     +     'inches',
     +     Hru_rain).NE.0 ) RETURN

      ALLOCATE (Hru_snow(Nhru))
      IF ( declvar('precip', 'hru_snow', 'nhru', Nhru, 'real',
     +     'Computed snow on each HRU',
     +     'inches',
     +     Hru_snow).NE.0 ) RETURN

      ALLOCATE (Tmax_allrain(12))
      IF ( declparam('precip', 'tmax_allrain', 'nmonths', 'real',
     +     '40.', '0.', '90.',
     +     'Precip all rain if HRU max temperature above this value',
     +     'If maximum temperature of an HRU is greater than or equal'//
     +     ' to this value (for each month, January to December),'//
     +     ' precipitation is assumed to be rain,'//
     +     ' in deg C or F, depending on units of data',
     +     'degrees').NE.0 ) RETURN

      IF ( declparam('precip', 'tmax_allsnow', 'one', 'real',
     +     '32.', '-10.', '40.',
     +     'Precip all snow if HRU max temperature below this value',
     +     'If HRU maximum temperature is less than or equal to this'//
     +     ' value, precipitation is assumed to be snow,'//
     +     ' in deg C or F, depending on units of data',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Adjmix_rain(12))
      IF ( declparam('precip', 'adjmix_rain', 'nmonths', 'real',
     +     '1.', '0.', '3.',
     +     'Adjustment factor for rain in a rain/snow mix',
     +     'Monthly factor to adjust rain proportion in a mixed'//
     +     ' rain/snow event',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( Precip_flag==6 .OR. Model==99 ) THEN
        ALLOCATE (Adjust_snow(12))
        IF ( declparam('xyz_dist', 'adjust_snow', 'nmonths', 'real',
     +       '0.01', '0.0', '1.0',
     +       'Downscaling fractional adjustment for snow',
     +       'Downscaling fractional adjustment for snow',
     +       'decimal fraction').NE.0 ) RETURN

        ALLOCATE (Adjust_rain(12))
        IF ( declparam('xyz_dist', 'adjust_rain', 'nmonths', 'real',
     +       '0.01', '0.0', '1.0',
     +       'Downscaling fractional adjustment for rain',
     +       'Downscaling fractional adjustment for rain',
     +       'decimal fraction').NE.0 ) RETURN
      ENDIF

      IF ( Precip_flag<5 .OR. Precip_flag==6 .OR. Model==99 ) THEN
        IF ( declparam('precip', 'precip_units', 'one', 'integer',
     +       '0', '0', '1',
     +       'Units for measured precipitation',
     +       'Units for measured precipitation (0=inches; 1=mm)',
     +       'none').NE.0 ) RETURN
      ENDIF

      IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Model==99 ) THEN
        ALLOCATE ( Psta_elev(Nrain) )
        IF ( declparam('precip', 'psta_elev', 'nrain', 'real',
     +       '0', '-300.', '30000.',
     +       'Precip station elevation',
     +       'Elevation of each precip measurement station',
     +       'elev_units').NE.0 ) RETURN
      ENDIF

! Solar Radiation variables and parameters
      ALLOCATE (Swrad(Nhru))
      IF ( declvar('solrad', 'swrad', 'nhru', Nhru, 'real',
     +     'Computed shortwave radiation for each HRU',
     +     'langleys',
     +     Swrad).NE.0 ) RETURN

      IF ( declvar('solrad', 'orad', 'one', 1, 'real',
     +     'Measured or computed solar radiation on a horizontal'//
     +     ' surface',
     +     'langleys',
     +     Orad).NE.0 ) RETURN

      IF ( declvar('solrad', 'basin_horad', 'one', 1, 'real',
     +     'Potential shortwave radiation for the basin centroid',
     +     'langleys',
     +     Basin_horad).NE.0 ) RETURN

      IF ( declvar('solrad', 'basin_potsw', 'one', 1, 'real',
     +     'Area-weighted average of potential shortwave radiation'//
     +     ' for the basin',
     +     'langleys',
     +     Basin_potsw).NE.0 ) RETURN

      ALLOCATE (Ppt_rad_adj(12))
      IF ( declparam('solrad', 'ppt_rad_adj', 'nmonths', 'real',
     +     '0.02', '0.0', '0.5',
     +     'Radiation reduced if basin precip above this value',
     +     'If basin precip exceeds this value, radiation is'//
     +     ' mutiplied by summer or winter precip adjustment ',
     +     'inches').NE.0 ) RETURN

      IF ( declparam('solrad', 'radmax', 'one', 'real',
     +     '0.8', '0.1', '1.0',
     +     'Maximum fraction of potential solar radiation (decimal)',
     +     'The maximum portion of the potential solar radiation'//
     +     ' that may reach the ground due to haze, dust, smog, etc.',
     +     'decimal fraction').NE.0 ) RETURN

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

      IF ( Nsol>0 .OR. Model==99 ) THEN
        IF ( declparam('solrad', 'rad_conv', 'one', 'real',
     +       '1.0', '0.1', '100.0',
     +       'Conversion factor to langleys for measured radiation',
     +       'Conversion factor to langleys for measured radiation',
     +       'none').NE.0 ) RETURN

        IF ( declparam('solrad', 'basin_solsta', 'one', 'integer',
     +       '0', 'bounded', 'nsol',
     +       'Index of main solar radiation station',
     +       'Index of solar radiation station used to compute basin'//
     +       ' radiation values',
     +       'none').NE.0 ) RETURN

        ALLOCATE (Hru_solsta(Nhru))
        IF ( declparam('solrad', 'hru_solsta', 'nhru', 'integer',
     +      '0', 'bounded', 'nsol',
     +      'Index of solar radiation station associated with each HRU',
     +      'Index of solar radiation station associated with each HRU',
     +      'none').NE.0 ) RETURN
      ENDIF

! Transpiration Variables
      ALLOCATE (Transp_on(Nhru))
      IF ( declvar('tindex', 'transp_on', 'nhru', Nhru, 'integer',
     +     'Switch indicating whether transpiration is occurring'//
     +     ' (0=no; 1=yes)',
     +     'none',
     +     Transp_on).NE.0 ) RETURN

      IF ( declvar('tindex', 'basin_transp_on', 'one', 1, 'integer',
     +     'Switch indicating whether transpiration is occurring'//
     +     ' anywhere in the basin (0=no; 1=yes)',
     +     'none',
     +     Basin_transp_on).NE.0 ) RETURN

! Potential ET Variables
      ALLOCATE (Potet(Nhru))
      IF ( declvar('potet', 'potet', 'nhru', Nhru, 'real',
     +     'Potential evapotranspiration on an HRU',
     +     'inches',
     +     Potet).NE.0 ) RETURN

      IF ( declvar('potet', 'basin_potet', 'one', 1, 'real',
     +     'Basin area-weighted average of potential et',
     +     'inches',
     +     Basin_potet).NE.0 ) RETURN

      climatevarsdecl = 0
      END FUNCTION climatevarsdecl

!***********************************************************************
!     CLIMATEVARSinit - Initialize climate_vars module - get parameter values
!***********************************************************************
      INTEGER FUNCTION climatevarsinit()
      USE PRMS_CLIMATEVARS
      USE PRMS_MODULE, ONLY: Temp_flag, Precip_flag
      USE PRMS_BASIN, ONLY: Timestep, Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      INTEGER :: j
!***********************************************************************
      climatevarsinit = 1

      IF ( getparam('temp', 'tsta_elev', Ntemp, 'real', Tsta_elev)
     +     .NE.0 ) RETURN

      IF ( Temp_flag/=4 ) THEN
        IF ( getparam('temp', 'tmax_adj', Nhru, 'real', Tmax_adj)
     +       .NE.0 ) RETURN

        IF ( getparam('temp', 'tmin_adj', Nhru, 'real', Tmin_adj)
     +       .NE.0 ) RETURN
      ENDIF

      IF ( getparam('temp', 'temp_units', 1, 'integer', Temp_units)
     +     .NE.0 ) RETURN

      IF ( Temp_flag<5 ) THEN
        IF ( getparam('temp', 'basin_tsta', 1, 'integer', Basin_tsta)
     +       .NE.0 ) RETURN
        IF ( Basin_tsta.LT.1 ) Basin_tsta = 1
        IF ( Basin_tsta>Ntemp ) THEN
          PRINT *, 'ERROR, basin_tsta>ntemp'
          RETURN
        ENDIF
      ENDIF

      IF ( Temp_flag<4 ) THEN
        IF ( getparam('temp', 'hru_tsta', Nhru, 'integer', Hru_tsta)
     +       .NE.0 ) RETURN
        DO j = 1, Nhru
          IF ( Hru_tsta(j)<1 ) Hru_tsta(j) = 1
          IF ( Hru_tsta(j)>Ntemp ) THEN
            PRINT *, 'ERROR, hru_tsta>ntemp, HRU:', j
            RETURN
          ENDIF
        ENDDO
      ENDIF

      IF ( getparam('precip', 'tmax_allrain', 12, 'real',
     +     Tmax_allrain).NE.0 ) RETURN

      IF ( getparam('precip', 'tmax_allsnow', 1, 'real', Tmax_allsnow)
     +     .NE.0 ) RETURN

      IF ( getparam('precip', 'adjmix_rain', 12, 'real', Adjmix_rain)
     +     .NE.0 ) RETURN

      IF ( Precip_flag==6 ) THEN
        IF ( getparam('precip', 'adjust_rain', 12, 'real', Adjust_rain)
     +       .NE.0 ) RETURN

        IF ( getparam('precip', 'adjust_snow', 12, 'real', Adjust_snow)
     +       .NE.0 ) RETURN
      ENDIF

      IF ( Precip_flag<5 .OR. Precip_flag==6 ) THEN
        IF ( getparam('precip', 'precip_units', 1, 'integer',
     +       Precip_units).NE.0 ) RETURN
      ENDIF

      IF ( Precip_flag==2 .OR. Precip_flag==6 ) THEN
        IF ( getparam('precip', 'psta_elev', Nrain, 'real', Psta_elev)
     +       .NE.0 ) RETURN
      ENDIF

      IF ( getparam('solrad', 'ppt_rad_adj', 12, 'real', Ppt_rad_adj)
     +     .NE.0 ) RETURN

      IF ( Nsol>0 ) THEN
        IF ( getparam('solrad', 'basin_solsta', 1, 'integer',
     +       Basin_solsta).NE.0 ) RETURN
        IF ( Basin_solsta.LT.1 .OR. Basin_solsta>Nsol ) Basin_solsta = 1

        IF ( getparam('solrad', 'rad_conv', 1, 'real', Rad_conv)
     +       .NE.0 ) RETURN

        IF ( getparam('solrad', 'hru_solsta', Nhru, 'integer',
     +       Hru_solsta).NE.0 ) RETURN
      ENDIF

      IF ( getparam('solrad', 'radmax', 1, 'real', Radmax)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radj_sppt', 1, 'real', Radj_sppt)
     +     .NE.0 ) RETURN

      IF ( getparam('solrad', 'radj_wppt', 1, 'real', Radj_wppt)
     +     .NE.0 ) RETURN

      IF ( Timestep==0 ) THEN
        Tmaxf = 0.0
        Tminf = 0.0
        Tavgf = 0.0
        Tmaxc = 0.0
        Tminc = 0.0
        Tavgc = 0.0
        Solrad_tmax = 0.0
        Solrad_tmin = 0.0
        Basin_temp = 0.0
        Basin_tmax = 0.0
        Basin_tmin = 0.0
        Pptmix = 0
        Newsnow = 0
        Prmx = 0.0
        Basin_ppt = 0.0
        Basin_obs_ppt = 0.0
        Basin_rain = 0.0
        Basin_snow = 0.0
        Hru_ppt = 0.0
        Hru_rain = 0.0
        Hru_snow = 0.0
        Swrad = 0.0
        Orad = 0.0
        Basin_horad = 0.0
        Basin_potsw = 0.0
        Transp_on = 0
        Basin_transp_on = 0
        Basin_potet = 0.0
        Potet = 0.0
      ENDIF

      climatevarsinit = 0
      END FUNCTION climatevarsinit

!***********************************************************************
! Convert Fahrenheit to Celsius
!***********************************************************************
      REAL FUNCTION f_to_c(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: FIVE_NINTHS = 5.0/9.0
!***********************************************************************
      f_to_c = (Temp-32.0)*FIVE_NINTHS
      END FUNCTION f_to_c

!***********************************************************************
! Convert Celsius to Fahrenheit
!***********************************************************************
      REAL FUNCTION c_to_f(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: NINE_FIFTHS = 9.0/5.0
!***********************************************************************
      c_to_f = Temp*NINE_FIFTHS + 32.0
      END FUNCTION c_to_f

!***********************************************************************
! Declares parameters and variables related to flows from soilzone_prms,
! smbal_prms, ssflow_prms, gwflow_casc_prms
!***********************************************************************
      MODULE PRMS_FLOWVARS
      IMPLICIT NONE
      INTEGER, PARAMETER :: DBGUNT = 295
!   Declared Variables
      ! soilzone
      REAL, SAVE :: Basin_ssflow
      REAL, SAVE :: Basin_actet, Basin_lakeevap, Basin_perv_et
      REAL, SAVE :: Basin_swale_et, Basin_soil_to_gw
      REAL, SAVE, ALLOCATABLE :: Hru_actet(:), Ssres_flow(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_gw(:), Soil_to_ssr(:)
      REAL, SAVE, ALLOCATABLE :: Ssr_to_gw(:)
      ! srunoff
      REAL, SAVE :: Basin_imperv_stor, Basin_imperv_evap, Basin_infil
      REAL, SAVE :: Basin_sroff, Strm_farfield
      REAL, SAVE, ALLOCATABLE :: Sroff(:)
      REAL, SAVE, ALLOCATABLE :: Hru_impervevap(:), Hru_impervstor(:)
      REAL, SAVE, ALLOCATABLE :: Infil(:), Hru_hortonian_cascadeflow(:)
      REAL, SAVE, ALLOCATABLE :: Strm_seg_in(:), Hortonian_lakes(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Soil_moist_max(:), Soil_rechr_max(:)
      REAL, SAVE, ALLOCATABLE :: Carea_max(:), Snowinfil_max(:)
      REAL, SAVE, ALLOCATABLE :: Imperv_stor_max(:)
      END MODULE PRMS_FLOWVARS

!***********************************************************************
!     Main prms_vars_flows routine
!***********************************************************************
      INTEGER FUNCTION flow_vars_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: flwvarsdecl, flwvarsinit
!***********************************************************************
      flow_vars_prms = 0

      IF ( Process_flag==1 ) THEN
        flow_vars_prms = flwvarsdecl()
      ELSEIF ( Process_flag==2 ) THEN
        flow_vars_prms = flwvarsinit()
      ENDIF

      END FUNCTION flow_vars_prms

!***********************************************************************
!     smdecl - set up parameters and variables for flow computations
!   Declared Parameters
!     soil_rechr_max, soil_moist_max, soil2gw_max
!***********************************************************************
      INTEGER FUNCTION flwvarsdecl()
      USE PRMS_FLOWVARS
      USE PRMS_MODULE, ONLY: Model, Ncascade, Ncascdgw
      USE PRMS_BASIN, ONLY: Nhru, Nssr, Nsegment
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      flwvarsdecl = 1

      IF ( declmodule(
     +'$Id: basin_prms.f 2447 2011-02-15 20:03:52Z rsregan $'
     +).NE.0 ) RETURN

! Declare Variables
      ALLOCATE (Ssr_to_gw(Nssr))
      IF ( declvar('soilzone', 'ssr_to_gw', 'nssr', Nssr, 'real',
     +     'Seepage from subsurface reservoir storage to'//
     +     ' its associated groundwater reservoir each time step',
     +     'inches',
     +     Ssr_to_gw).NE.0 ) RETURN

      ALLOCATE (Ssres_flow(Nssr))
      IF ( declvar('ssflow', 'ssres_flow', 'nssr', Nssr, 'real',
     +     'Outflow from each subsurface reservoir',
     +     'inches',
     +     Ssres_flow).NE.0 ) RETURN

      IF ( declvar('ssflow', 'basin_ssflow', 'one', 1, 'real',
     +     'Basin weighted average for subsurface reservoir outflow',
     +     'inches',
     +     Basin_ssflow).NE.0 ) RETURN

! soilzone
      IF ( declvar('soilzone', 'basin_swale_et', 'one', 1, 'real',
     +     'Basin area weighted average of ET from swales',
     +     'inches',
     +     Basin_swale_et).NE.0 ) RETURN

      ALLOCATE (Hru_actet(Nhru))
      IF ( declvar('soilzone', 'hru_actet', 'nhru', Nhru, 'real',
     +     'Actual evapotranspiration on HRU, pervious + impervious',
     +     'inches',
     +     Hru_actet).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_actet', 'one', 1, 'real',
     +     'Basin area weighted average of hru_actet',
     +     'inches',
     +     Basin_actet).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_perv_et', 'one', 1, 'real',
     +     'Basin area weighted average of pervious area ET',
     +     'inches',
     +     Basin_perv_et).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_lakeevap', 'one', 1, 'real',
     +     'Basin area weighted average of lake evaporation',
     +     'inches',
     +     Basin_lakeevap).NE.0 ) RETURN

      ALLOCATE (Soil_to_gw(Nhru))
      IF ( declvar('soilzone', 'soil_to_gw', 'nhru', Nhru, 'real',
     +     'Portion of excess soil water from an HRU that flows to'//
     +     ' its associated groundwater reservoir',
     +     'inches',
     +     Soil_to_gw).NE.0 ) RETURN

      ALLOCATE (Soil_to_ssr(Nhru))
      IF ( declvar('soilzone', 'soil_to_ssr', 'nhru', Nhru, 'real',
     +     'Portion of excess soil water from an HRU that flows to'//
     +     ' its associated subsurface reservoir',
     +     'inches',
     +     Soil_to_ssr).NE.0 ) RETURN

      IF ( declvar('soilzone', 'basin_soil_to_gw', 'one', 1, 'real',
     +     'Basin average excess soil water that flows directly to'//
     +     ' groundwater reservoirs',
     +     'inches',
     +     Basin_soil_to_gw).NE.0 ) RETURN

! srunoff
      ALLOCATE (Hru_impervevap(Nhru))
      IF ( declvar('srunoff', 'hru_impervevap', 'nhru', Nhru, 'real',
     +     'Evaporation from impervious area for each HRU',
     +     'inches',
     +     Hru_impervevap).NE.0 ) RETURN

      ALLOCATE (Hru_impervstor(Nhru))
      IF ( declvar('srunoff', 'hru_impervstor', 'nhru', Nhru, 'real',
     +     'Storage on impervious area for each HRU',
     +     'inches',
     +     Hru_impervstor).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_imperv_evap', 'one', 1, 'real',
     +     'Basin area-weighted average for evaporation from'//
     +     ' impervious area',
     +     'inches',
     +     Basin_imperv_evap).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_imperv_stor', 'one', 1, 'real',
     +     'Basin area-weighted average for storage on'//
     +     ' impervious area',
     +     'inches',
     +     Basin_imperv_stor).NE.0 ) RETURN

      ALLOCATE (Infil(Nhru))
      IF ( declvar('srunoff', 'infil', 'nhru', Nhru, 'real',
     +     'Amount of water infiltrating the soil on each HRU',
     +     'inches',
     +     Infil).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_infil', 'one', 1, 'real',
     +     'Basin area-weighted average for infiltration',
     +     'inches',
     +     Basin_infil).NE.0 ) RETURN

      IF ( Ncascade>0 .OR. Model==99 ) THEN
        ALLOCATE (Hru_hortonian_cascadeflow(Nhru))
        IF ( declvar('srunoff', 'hru_hortonian_cascadeflow', 'nhru',
     +       Nhru, 'real',
     +       'Cascading Hortonian surface runoff leaving each HRU',
     +       'inches',
     +       Hru_hortonian_cascadeflow).NE.0 ) RETURN

        ALLOCATE (Hortonian_lakes(Nhru))
        IF ( declvar('srunoff', 'hortonian_lakes', 'nhru', Nhru, 'real',
     +       'Surface runoff to lakes for each HRU',
     +       'inches',
     +       Hortonian_lakes).NE.0 ) RETURN
      ENDIF

      IF ( Ncascade>0 .OR. Ncascdgw>0 .OR. Model==99 ) THEN
        ALLOCATE (Strm_seg_in(Nsegment))
        IF ( declvar('srunoff', 'strm_seg_in', 'nsegment', Nsegment,
     +       'real',
     +       'Flow in stream segments as a result of cascading flow',
     +       'cfs', Strm_seg_in).NE.0 ) RETURN

        IF ( declvar('srunoff', 'strm_farfield', 'one', 1, 'real',
     +       'Flow out of basin as far-field flow',
     +       'cfs', Strm_farfield).NE.0 ) RETURN
      ENDIF

      IF ( declvar('srunoff', 'basin_sroff', 'one', 1, 'real',
     +     'Basin area-weighted average of surface runoff',
     +     'inches',
     +     Basin_sroff).NE.0 ) RETURN

      ALLOCATE (Sroff(Nhru))
      IF ( declvar('srunoff', 'sroff', 'nhru', Nhru, 'real',
     +     'Surface runoff to streams for each HRU',
     +     'inches',
     +     Sroff).NE.0 ) RETURN

! Declare Parameters
      ALLOCATE (Soil_moist_max(Nhru))
      IF ( declparam('soilzone', 'soil_moist_max', 'nhru', 'real',
     +     '6.', '0.', '20.',
     +     'Maximum value of water for soil zone',
     +     'Maximum available water holding capacity of soil profile.'//
     +     ' Soil profile is surface to bottom of rooting zone',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Soil_rechr_max(Nhru))
      IF ( declparam('soilzone', 'soil_rechr_max', 'nhru', 'real',
     +     '2.', '0.', '10.',
     +     'Maximum value for soil recharge zone',
     +     'Maximum value for soil recharge zone (upper portion'//
     +     ' of soil_moist where losses occur as both evaporation'//
     +     ' and transpiration).  Must be less than or equal to'//
     +     ' soil_moist',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Carea_max(Nhru))
      IF ( declparam('srunoff', 'carea_max', 'nhru', 'real',
     +     '.6', '0.', '1.0',
     +     'Maximum contributing area',
     +     'Maximum possible area contributing to surface runoff'//
     +     ' expressed as a portion of the HRU area',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Snowinfil_max(Nhru))
      IF ( declparam('srunoff', 'snowinfil_max', 'nhru', 'real',
     +     '2.', '0.', '20.',
     +     'Maximum snow infiltration per day',
     +     'Maximum snow infiltration per day',
     +     'inches/day').NE.0 ) RETURN

      ALLOCATE (Imperv_stor_max(Nhru))
      IF ( declparam('srunoff', 'imperv_stor_max', 'nhru', 'real',
     +     '0.', '0.', '10.',
     +     'HRU maximum impervious area retention storage',
     +     'Maximum impervious area retention storage for each HRU',
     +     'inches').NE.0 ) RETURN

      flwvarsdecl = 0
      END FUNCTION flwvarsdecl

!***********************************************************************
!     flwvarsinit - Initialize module - get parameter values,
!                   set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION flwvarsinit()
      USE PRMS_FLOWVARS
      USE PRMS_MODULE, ONLY: Ncascade, Ncascdgw
      USE PRMS_BASIN, ONLY: Print_debug, Hru_ssres, Timestep, Nhru, Nssr
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
!   Local Variables
      INTEGER :: i
!***********************************************************************
      flwvarsinit = 1

      IF ( Print_debug==7 ) OPEN (DBGUNT, FILE='soilzone_prms.dbg')

      IF ( getparam('soilzone', 'soil_moist_max', Nhru, 'real',
     +     Soil_moist_max).NE.0 ) RETURN

      IF ( getparam('soilzone', 'soil_rechr_max', Nhru, 'real',
     +     Soil_rechr_max).NE.0 ) RETURN

      ! Sanity checks for parameters
      DO i = 1, Nhru
        IF ( Soil_moist_max(i)<0.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, ' soil_moist_max specified < 0, set to 0',
     +         Soil_moist_max(i)
          Soil_moist_max(i) = 0.0
        ENDIF
        IF ( Soil_rechr_max(i)<0.0 ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, *)
     +         'HRU:', i, ' soil_rechr_max specified < 0, set to 0',
     +         Soil_rechr_max(i)
          Soil_rechr_max(i) = 0.0
        ENDIF
        IF ( Soil_rechr_max(i).GT.Soil_moist_max(i) ) THEN
          IF ( Print_debug==7 ) WRITE (DBGUNT, 9002)
     +         i, Soil_rechr_max(i), Soil_moist_max(i)
          Soil_rechr_max(i) = Soil_moist_max(i)
        ENDIF
      ENDDO

      IF ( getparam('srunoff', 'carea_max', Nhru, 'real', Carea_max)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'snowinfil_max', Nhru, 'real',
     +     Snowinfil_max).NE.0 ) RETURN

      IF ( getparam('srunoff', 'imperv_stor_max', Nhru, 'real',
     +     Imperv_stor_max).NE.0 ) RETURN

      IF ( Timestep.EQ.0 ) THEN
! initialize scalers
        Basin_perv_et = 0.0
        Basin_actet = 0.0
        Basin_lakeevap = 0.0
        Basin_swale_et = 0.0
        Basin_imperv_evap = 0.0
        Basin_imperv_stor = 0.0
        Basin_soil_to_gw = 0.0
        Basin_ssflow = 0.0
        Basin_infil = 0.0
        Basin_sroff = 0.0
! initialize arrays (dimensioned Nssr)
        Ssr_to_gw = 0.0
        Ssres_flow = 0.0
! initialize arrays (dimensioned Nhru)
        Hru_impervstor = 0.0
        Soil_to_gw = 0.0
        Soil_to_ssr = 0.0
        Hru_actet = 0.0
        Hru_impervevap = 0.0
        Infil = 0.0
        Sroff = 0.0
        IF ( Ncascade>0 ) THEN
          Hru_hortonian_cascadeflow = 0.0
          Hortonian_lakes = 0.0
        ENDIF
        IF ( Ncascade>0 .OR. Ncascdgw>0 ) THEN
          Strm_seg_in = 0.0
          Strm_farfield = 0.0
        ENDIF
      ENDIF

      flwvarsinit = 0

 9002 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_rechr_max > soil_moist_max,', /, 29X,
     +        'soil_rechr_max set to soil_moist_max')

      END FUNCTION flwvarsinit


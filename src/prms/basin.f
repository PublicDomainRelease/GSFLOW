!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
      MODULE PRMS_BASIN
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 1.0E-7, INCH2CM = 2.54
      DOUBLE PRECISION, PARAMETER :: DNEARZERO = 1.0D-12
      DOUBLE PRECISION, PARAMETER :: CFS2CMS_CONV = 0.028316847D0
      REAL, PARAMETER :: INCH2MM = 25.4, INCH2M = 0.0254
      REAL, PARAMETER :: MM2INCH = 1.0/INCH2MM
      REAL, PARAMETER :: FEET2METERS = 0.3048
      REAL, PARAMETER :: METERS2FEET = 1.0/FEET2METERS
      CHARACTER(LEN=5), PARAMETER :: MODNAME = 'basin'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Basin Definition'
      INTEGER, SAVE :: Numlakes, Timestep, Starttime(6), Endtime(6)
      INTEGER, SAVE :: Start_year, Start_month, Start_day
      INTEGER, SAVE :: End_year, End_month, End_day
      INTEGER, SAVE :: Active_hrus, Active_gwrs
      INTEGER, SAVE :: Hemisphere
      DOUBLE PRECISION, SAVE :: Cfs2inches, Land_area, Water_area
      DOUBLE PRECISION, SAVE :: Basin_area_inv, Basin_lat
      REAL, SAVE, ALLOCATABLE :: Hru_frac_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Gwres_area(:), Ssres_area(:)
      REAL, SAVE, ALLOCATABLE :: Hru_elev_feet(:), Hru_elev_meters(:)
      INTEGER, SAVE, ALLOCATABLE :: Gwr_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Hru_route_order(:)
      INTEGER, SAVE, ALLOCATABLE :: Gwr_route_order(:)
      REAL, SAVE, ALLOCATABLE :: Hru_perv(:), Hru_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open_max(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_clos_max(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_max(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_area(:)
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Hru_frac_perv(:), Dprst_frac_hru(:)
      DOUBLE PRECISION, SAVE :: Basin_cfs, Basin_cms
      DOUBLE PRECISION, SAVE :: Basin_ssflow_cfs, Basin_gwflow_cfs
      DOUBLE PRECISION, SAVE :: Basin_sroff_cfs, Basin_stflow_in
      DOUBLE PRECISION, SAVE :: Basin_stflow_out
!   Declared Parameters
      INTEGER, SAVE :: Elev_units
      INTEGER, SAVE, ALLOCATABLE :: Hru_type(:), Cov_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Hru_ssres(:), Hru_gwres(:)
      INTEGER, SAVE, ALLOCATABLE :: Lake_hru(:), Lake_hru_id(:) !not needed if no lakes
      REAL, SAVE :: Basin_area
      REAL, SAVE, ALLOCATABLE :: Hru_area(:), Hru_percent_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Hru_elev(:), Hru_lat(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_open(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area(:)
      END MODULE PRMS_BASIN

!***********************************************************************
!     Main basin routine
!***********************************************************************
      INTEGER FUNCTION basin()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basdecl, basinit
!***********************************************************************
      basin = 0

      IF ( Process(:4)=='decl' ) THEN
        basin = basdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        basin = basinit()
      ENDIF

      END FUNCTION basin

!***********************************************************************
!     basdecl - set up parameters
!   Declared Parameters
!     print_debug, hru_area, hru_percent_imperv, hru_type, hru_elev,
!     cov_type
!***********************************************************************
      INTEGER FUNCTION basdecl()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Model, Nhru, Ngw, Nssr, Nlake, Dprst_flag,
     +    Version_basin, Basin_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: n
!***********************************************************************
      basdecl = 1

      Version_basin =
     +'$Id: basin.f 5498 2013-03-14 21:49:27Z rsregan $'
      Basin_nc = INDEX( Version_basin, 'Z' )
      n = INDEX( Version_basin, '.f' ) + 1
      IF ( declmodule(Version_basin(6:n), PROCNAME,
     +                Version_basin(n+2:Basin_nc))/=0 ) STOP
! Declared Variables
      ALLOCATE ( Hru_frac_perv(Nhru) )
      IF ( declvar(MODNAME, 'hru_frac_perv', 'nhru', Nhru, 'real',
     +     'Fraction of HRU that is pervious',
     +     'decimal fraction',
     +     Hru_frac_perv)/=0 ) CALL read_error(3, 'hru_frac_perv')

      ALLOCATE ( Dprst_frac_hru(Nhru) )
      IF ( declvar(MODNAME, 'dprst_frac_hru', 'nhru', Nhru, 'real',
     +     'Fraction of HRU that has surface-depression storage',
     +     'decimal fraction',
     +     Dprst_frac_hru)/=0 ) CALL read_error(3, 'dprst_frac_hru')

      IF ( declvar(MODNAME, 'basin_cfs', 'one', 1, 'double',
     +     'Streamflow leaving the basin through the stream network',
     +     'cfs',
     +     Basin_cfs)/=0 ) CALL read_error(3, 'basin_cfs')

      IF ( declvar(MODNAME, 'basin_cms', 'one', 1, 'double',
     +     'Streamflow leaving the basin through the stream network',
     +     'cms',
     +     Basin_cms)/=0 ) CALL read_error(3, 'basin_cms')

      IF ( declvar(MODNAME, 'basin_stflow_in', 'one', 1, 'double',
     +     'Basin area-weighted average lateral flow entering'//
     +     ' the stream network',
     +     'inches',
     +     Basin_stflow_in)/=0 ) CALL read_error(3, 'basin_stflow_in')

      IF ( declvar(MODNAME, 'basin_stflow_out', 'one', 1, 'double',
     +     'Basin area-weighted average streamflow leaving through'//
     +     ' the stream network',
     +     'inches',
     +     Basin_stflow_out)/=0 ) CALL read_error(3, 'basin_stflow_out')

      IF ( declvar(MODNAME, 'basin_sroff_cfs', 'one', 1, 'double',
     +    'Surface runoff leaving the basin through the stream network',
     +     'cfs',
     +     Basin_sroff_cfs)/=0 ) CALL read_error(3, 'basin_sroff_cfs')

      IF ( declvar(MODNAME, 'basin_ssflow_cfs', 'one', 1, 'double',
     +     'Interflow leaving the basin through the stream network',
     +     'cfs',
     +     Basin_ssflow_cfs)/=0 ) CALL read_error(3, 'basin_ssflow')

      IF ( declvar(MODNAME, 'basin_gwflow_cfs', 'one', 1, 'double',
     +     'Groundwater flow leaving the basin through the'//
     +     ' stream network',
     +     'cfs',
     +     Basin_gwflow_cfs)/=0 ) CALL read_error(3, 'basin_gwflow_cfs')

      IF ( Dprst_flag>0 .OR. Model==99 ) THEN
        IF ( (Nhru/=Nssr .OR. Nhru/=Ngw) .AND. Model/=99 ) THEN
          PRINT *, 'Error, depression storage requires nhru=nssr=ngw'
          STOP
        ENDIF

        ALLOCATE ( Dprst_area_max(Nhru) )
        ALLOCATE (Dprst_area_open_max(Nhru), Dprst_area_clos_max(Nhru))
        ALLOCATE ( Dprst_area(Nhru) )
        IF ( declparam(MODNAME, 'dprst_area', 'nhru', 'real',
     +       '0.0', '0.0', '1.0E9',
     +       'Aggregate sum of surface depression areas of each HRU',
     +       'Aggregate sum of surface depression areas of each HRU',
     +       'acres')/=0 ) CALL read_error(1, 'dprst_area')

        ALLOCATE ( Dprst_frac_open(Nhru) )
        IF ( declparam(MODNAME, 'dprst_frac_open', 'nhru', 'real',
     +       '0.0', '0.0', '1.0',
     +       'Fraction of open surface depression storage area within'//
     +       ' an HRU that can generate surface runoff as a function'//
     +       ' of storage volume',
     +       'Fraction of open surface depression storage area within'//
     +       ' an HRU that can generate surface runoff as a function'//
     +       ' of storage volume',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_frac_open')
      ENDIF

! Declared Parameters
      IF ( declparam(MODNAME, 'basin_area', 'one', 'real',
     +     '0.0', '0.0', '1.0E9',
     +     'Area of basin', 'Area of basin',
     +     'acres')/=0 ) CALL read_error(1, 'basin_area')

      ALLOCATE ( Hru_area(Nhru) )
      IF ( declparam(MODNAME, 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1.0E9',
     +     'HRU area', 'Area of each HRU',
     +     'acres')/=0 ) CALL read_error(1, 'hru_area')

      IF ( declparam(MODNAME, 'elev_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Elevation units flag',
     +     'Flag to indicate the units of the elevation values'//
     +     ' (0=feet; 1=meters)',
     +     'none')/=0 ) CALL read_error(1, 'elev_units')

      ALLOCATE ( Hru_elev_feet(Nhru), Hru_elev_meters(Nhru) )
      ALLOCATE ( Hru_elev(Nhru) )
      IF ( declparam(MODNAME, 'hru_elev', 'nhru', 'real',
     +     '0.0', '-1000.0', '30000.0',
     +     'HRU mean elevation', 'Mean elevation for each HRU',
     +     'elev_units')/=0 ) CALL read_error(1, 'hru_elev')

      ALLOCATE ( Hru_lat(Nhru) )
      IF ( declparam(MODNAME, 'hru_lat', 'nhru', 'real',
     +     '40.0', '-90.0', '90.0',
     +     'HRU latitude', 'Latitude of each HRU',
     +     'degrees')/=0 ) CALL read_error(1, 'hru_lat')

      ALLOCATE ( Hru_percent_imperv(Nhru) )
      IF ( declparam(MODNAME, 'hru_percent_imperv', 'nhru', 'real',
     +     '0.0', '0.0', '0.999',
     +     'HRU percent impervious',
     +     'Fraction of each HRU area that is impervious',
     +     'decimal fraction')/=0 )
     +     CALL read_error(1, 'Hru_percent_imperv')

      ALLOCATE ( Hru_type(Nhru), Gwr_type(Nhru) )
      IF ( declparam(MODNAME, 'hru_type', 'nhru', 'integer',
     +     '1', '0', '3',
     +     'HRU type',
     +     'Type of each HRU (0=inactive; 1=land; 2=lake; 3=swale)',
     +     'none')/=0 ) CALL read_error(1, 'hru_type')

      ALLOCATE ( Cov_type(Nhru) )
      IF ( declparam(MODNAME, 'cov_type', 'nhru', 'integer',
     +     '3', '0', '3',
     +     'Cover type designation for HRU',
     +     'Vegetation cover type for each HRU (0=bare soil;'//
     +     ' 1 =grasses; 2=shrubs; 3=trees)',
     +     'none')/=0 ) CALL read_error(1, 'cov_type')

      ALLOCATE ( Hru_ssres(Nhru) )
      IF ( Nhru/=Nssr .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'hru_ssres', 'nhru', 'integer',
     +       '1', 'bounded', 'nssr',
     +       'Index of subsurface reservoir assigned to HRU',
     +       'Index of subsurface reservoir receiving excess water'//
     +       ' from HRU capillary reservoir (deprecated)',
     +       'none')/=0 ) CALL read_error(1, 'hru_ssres')
      ENDIF

      ALLOCATE ( Hru_gwres(Nhru) )
      IF ( Nhru/=Ngw .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'hru_gwres', 'nhru', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'Index of GWR assigned to HRU',
     +       'Index of GWR receiving soil-zone drainage from each'//
     +       ' associated HRU (deprecated)',
     +       'none')/=0 ) CALL read_error(1, 'hru_gwres')
      ENDIF

      IF ( Nlake>0 .OR. Model==99 ) THEN
        ALLOCATE ( Lake_hru_id(Nhru) )
        IF ( declparam(MODNAME, 'lake_hru_id', 'nhru', 'integer',
     +       '0', 'bounded', 'nhru',
     +       'Indentification number of the lake associated with'//
     +       ' an HRU',
     +       'Indentification number of the lake associated with'//
     +       ' an HRU; more than one HRU can be associated with'//
     +       ' each lake',
     +       'none')/=0 ) CALL read_error(1, 'lake_hru_id')
        ALLOCATE ( Lake_hru(Nlake), Lake_area(Nlake) )
        IF ( declparam(MODNAME, 'lake_hru', 'nlake', 'integer',
     +       '0', 'bounded', 'nhru',
     +       'Index of HRU for each lake HRU',
     +       'Index of HRU for each lake HRU',
     +       'none')/=0 ) CALL read_error(1, 'lake_hru')
      ENDIF

      ALLOCATE ( Ssres_area(Nssr), Gwres_area(Ngw) )
      ALLOCATE ( Hru_perv(Nhru), Hru_imperv(Nhru) )
      ALLOCATE ( Hru_frac_imperv(Nhru) )
! Variables used by modules that include cascade routing
      ALLOCATE ( Hru_route_order(Nhru), Gwr_route_order(Ngw) )

      basdecl = 0
      END FUNCTION basdecl

!**********************************************************************
!     basinit - check for validity of basin parameters
!               and compute reservoir areas
!**********************************************************************
      INTEGER FUNCTION basinit()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Nlake, Dprst_flag,
     +    Strmflow_flag, Print_debug, Versn_gsfprms
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam, getstep
      EXTERNAL dattim, opstr
      INTRINSIC ABS
! Local Variables
      CHARACTER(LEN=68) :: buffer
      INTEGER :: i, j, k, ierr
      REAL :: harea, tmp
      DOUBLE PRECISION :: totarea, active_area, diff
      DOUBLE PRECISION :: basin_imperv, basin_perv, basin_dprst
!**********************************************************************
      basinit = 1

      CALL dattim('start', Starttime)
      Start_year = Starttime(1)
      Start_month = Starttime(2)
      Start_day = Starttime(3)
      CALL dattim('end', Endtime)
      End_year = Endtime(1)
      End_month = Endtime(2)
      End_day = Endtime(3)

      IF ( getparam(MODNAME, 'hru_area', Nhru, 'real', Hru_area)
     +     /=0 ) CALL read_error(2, 'hru_area')
      IF ( getparam(MODNAME, 'basin_area', 1, 'real', Basin_area)
     +     /=0 ) CALL read_error(2, 'basin_area')
      IF ( getparam(MODNAME, 'hru_elev', Nhru, 'real', Hru_elev)
     +     /=0 ) CALL read_error(2, 'hru_elev')
      IF ( getparam(MODNAME, 'hru_lat', Nhru, 'real', Hru_lat)/=0 )
     +     CALL read_error(2, 'hru_lat')
      IF ( getparam(MODNAME, 'hru_type', Nhru, 'integer', Hru_type)
     +     /=0 ) CALL read_error(2, 'hru_type')
      Gwr_type = Hru_type
      IF ( getparam(MODNAME, 'cov_type', Nhru, 'integer',
     +     Cov_type)/=0 ) CALL read_error(2, 'cov_type')

      IF ( getparam(MODNAME, 'elev_units', 1, 'integer', Elev_units)
     +     /=0 ) CALL read_error(2, 'elev_units')

      IF ( getparam(MODNAME, 'hru_percent_imperv', Nhru, 'real',
     +     Hru_percent_imperv)/=0 )
     +     CALL read_error(2, 'hru_percent_imperv')

      Basin_lat = 0.0D0
      DO i = 1, Nhru
        Hru_frac_imperv(i) = Hru_percent_imperv(i)
        IF ( Hru_percent_imperv(i)<NEARZERO ) Hru_frac_imperv(i) = 0.0
        IF ( Hru_type(i)==1 .OR. Hru_type(i)==3 ) THEN
          IF ( Hru_frac_imperv(i)>0.999 ) THEN
            PRINT *, 'Warning, hru_percent_imperv > 0.999',
     +               ' hru_percent_imperv has been set to 0.999',
     +               ' Percent impervious:', Hru_frac_imperv(i),
     +               ' HRU:', i
            Hru_frac_imperv(i) = 0.999
          ENDIF
        ENDIF
        Hru_imperv(i) = Hru_frac_imperv(i)*Hru_area(i)
        IF ( Hru_imperv(i)<NEARZERO ) Hru_imperv(i) = 0.0
        Hru_perv(i) = Hru_area(i) - Hru_imperv(i)
        IF ( Elev_units==0 ) THEN
          Hru_elev_feet(i) = Hru_elev(i)
          Hru_elev_meters(i) = Hru_elev_feet(i)*FEET2METERS
        ELSE
          Hru_elev_meters(i) = Hru_elev(i)
          Hru_elev_feet(i) = Hru_elev_feet(i)*METERS2FEET
        ENDIF
        IF ( Hru_type(i)/=0 )
     +       Basin_lat =Basin_lat + Hru_lat(i)*Hru_area(i)

      ENDDO
      DEALLOCATE ( Hru_percent_imperv )

      Timestep = getstep()
      IF ( Timestep==0 ) THEN
        Basin_cfs = 0.0D0
        Basin_cms = 0.0D0
        Basin_stflow_in = 0.0D0
        Basin_stflow_out = 0.0D0
        Basin_ssflow_cfs = 0.0D0
        Basin_sroff_cfs = 0.0D0
        Basin_gwflow_cfs = 0.0D0
      ENDIF

      IF ( Dprst_flag>0 ) THEN
        IF ( getparam(MODNAME, 'dprst_frac_open', Nhru, 'real',
     +       Dprst_frac_open)/=0 )
     +       CALL read_error(2, 'dprst_frac_open')
        IF ( getparam(MODNAME, 'dprst_area', Nhru, 'real',
     +       Dprst_area)/=0 ) CALL read_error(2, 'dprst_area')
        Dprst_area_open_max = 0.0
        Dprst_area_clos_max = 0.0
        Dprst_frac_hru = 0.0
      ENDIF
      basin_dprst = 0.0D0

      totarea = 0.0D0
      Land_area = 0.0D0
      Water_area = 0.0D0
      basin_perv = 0.0D0
      basin_imperv = 0.0D0

      Numlakes = 0
      ierr = 0
      j = 0
      Hru_route_order = 0
      DO i = 1, Nhru
        Hru_frac_perv(i) = 0.0

        harea = Hru_area(i)
        totarea = totarea + harea

        IF ( Hru_type(i)==0 ) CYCLE
        IF ( harea<NEARZERO ) THEN
          ierr = 1
          PRINT *, 'ERROR, area for HRU:', i, ' specified as 0.0'
          CYCLE
        ENDIF
        j = j + 1
        Hru_route_order(j) = i

        ! GWR's cannot be swales or lakes
        IF ( Gwr_type(i)==3 .OR. Gwr_type(i)==2 ) Gwr_type(i) = 1

        ! Lake HRU
        IF ( Hru_type(i)==2 ) THEN
          Water_area = Water_area + harea
          Numlakes = Numlakes + 1
          CYCLE
        ENDIF

        ! regular or swale HRU
        Land_area = Land_area + harea

! added for depression storage calulations:
        IF ( Dprst_flag>0 ) THEN
          IF ( Dprst_area(i)<0.0 )
     +         PRINT *, 'Warning, dprst_area specified < 0 for HRU:', i,
     +                  ' value:', Dprst_area(i), ' set to 0'
          IF ( Dprst_area(i)<NEARZERO ) Dprst_area(i) = 0.0
          Dprst_area_max(i) = Dprst_area(i)
          IF ( Dprst_area_max(i)>NEARZERO ) THEN
            Hru_perv(i) = Hru_perv(i) - Dprst_area_max(i)
            tmp = Hru_perv(i)/harea
            !rsr, sanity check
            IF ( tmp<0.001) THEN
              PRINT *, 'Warning, hru_imperv+dprst_area_max > 0.999',
     +                 '*hru_area for HRU:', i
              PRINT *, '    hru_area:', harea, ' hru_imperv:',
     +                 Hru_imperv(i), ' dprst_area:', Dprst_area_max(i)
              PRINT *, '   (hru_imperv+dprst_area)/hru_area =',
     +                     (Hru_imperv(i)+Dprst_area_max(i))/harea
              PRINT *, '   Pervious area of HRU set to 0.001*hru_area;'
              PRINT*,'   Depression storage area reduced by this amount'
              Hru_perv(i) = 0.001*harea
              Dprst_area_max(i) = Dprst_area_max(i) - Hru_perv(i)
              PRINT *, ' Pervious area:', Hru_perv(i), ' Dprst area:',
     +                 Dprst_area_max(i)
!              ierr = 1
!              CYCLE
            ENDIF
            Dprst_area_open_max(i) =Dprst_area_max(i)*Dprst_frac_open(i)
            IF ( ABS(1.0-Dprst_frac_open(i))<NEARZERO )
     +           Dprst_area_open_max(i) = Dprst_area_max(i)
            Dprst_area_clos_max(i) =
     +                        Dprst_area_max(i) - Dprst_area_open_max(i)
            tmp =harea - Hru_perv(i) - Hru_imperv(i) - Dprst_area_max(i)
            IF ( ABS(tmp)>0.0 ) THEN
              IF ( Dprst_area_open_max(i)>0.0 ) THEN
                Dprst_area_open_max(i) = Dprst_area_open_max(i )+ tmp
              ELSE
                Dprst_area_clos_max(i) = Dprst_area_clos_max(i) + tmp
              ENDIF
            ENDIF
          ENDIF
          IF ( Dprst_area_open_max(i)<NEARZERO )
     +         Dprst_area_open_max(i) = 0.0
          IF ( Dprst_area_clos_max(i)<NEARZERO )
     +         Dprst_area_clos_max(i) = 0.0
          Dprst_area_max(i) = Dprst_area_open_max(i)
     +                        + Dprst_area_clos_max(i)
          Dprst_frac_hru(i) = Dprst_area_max(i)/harea
          IF ( Hru_frac_imperv(i)+Dprst_frac_hru(i)>0.999 ) THEN
            tmp = 0.999 - Hru_frac_imperv(i)
            Dprst_area_max(i) = tmp*Hru_area(i)
          ENDIF
          Dprst_frac_hru(i) = Dprst_area_max(i)/harea
          Hru_perv(i) = harea - Hru_imperv(i) - Dprst_area_max(i)
          basin_dprst = basin_dprst + Dprst_area_max(i)
        ENDIF
        Hru_frac_perv(i) = Hru_perv(i)/harea
        IF ( Hru_frac_perv(i)>0.9999 ) THEN
          Hru_frac_perv(i) = 1.0
          Hru_perv(i) = harea
          Hru_imperv(i) = 0.0
          Hru_frac_imperv(i) = 0.0
        ENDIF
        basin_perv = basin_perv + Hru_perv(i)
        basin_imperv = basin_imperv + Hru_imperv(i)
      ENDDO
      Active_hrus = j
      active_area = Land_area + Water_area
      IF ( Dprst_flag>0 ) DEALLOCATE ( Dprst_frac_open, Dprst_area )

      IF ( Numlakes/=Nlake ) THEN
        PRINT *, 'ERROR, number of lakes specified in hru_type'
        PRINT *, ' does not equal nlake:', Nlake, ' numlakes:',
     +           Numlakes
        STOP '***Correct or add nlake to the Parameter File***'
      ENDIF
      IF ( Strmflow_flag==2 .AND. Numlakes==0 ) THEN
        PRINT *, 'ERROR, simulation uses lake module but does not',
     +           ' have lakes'
        STOP
      ENDIF
      IF ( Nlake>0 ) THEN
        IF ( getparam(MODNAME, 'lake_hru_id', Nhru, 'integer',
     +       Lake_hru_id)/=0 ) CALL read_error(1, 'lake_hru_id')
        IF ( getparam(MODNAME, 'lake_hru', Nlake, 'real',
     +       Lake_hru)/=0 ) CALL read_error(2, 'lake_hru')
        DO i = 1, Nlake
          j = Lake_hru(i)
          IF ( j>0 ) THEN
            IF ( Lake_hru_id(j)==0 ) THEN
              Lake_hru_id(j) = i
            ELSEIF ( Lake_hru_id(j)/=i ) THEN
              PRINT *, 'ERROR, parameters lake_hru and lake_hru_id'//
     +                 ' in conflict, Lake:', i, ' HRU:', j
              ierr = 1
            ENDIF
          ELSEIF ( Strmflow_flag==2 ) THEN
            PRINT *, 'ERROR, specified strmflow_lake and lake_hru=0'
            PRINT *, '       for Lake:', i
            ierr = 1
          ENDIF
        ENDDO
        Lake_area = 0.0D0
        DO i = 1, Nhru
          IF ( Lake_hru_id(i)>Nlake ) THEN
            PRINT *, 'ERROR, lake_hru_id>nlake for HRU:', i,
     +               ' lake_hru_id:', lake_hru_id(i), ' nlake:', Nlake
            ierr = 1
          ELSEIF ( Hru_type(i)==2 ) THEN
            IF ( Lake_hru_id(i)>0 ) Lake_area(Lake_hru_id(i)) =
     +           Lake_area(Lake_hru_id(i)) + Hru_area(i)
          ENDIF
        ENDDO
      ENDIF
      IF ( Strmflow_flag==2 ) THEN
        DO i = 1, Nlake
          IF ( Hru_type(Lake_hru(i))/=2 ) THEN
            PRINT *, 'ERROR, HRU:', Lake_hru(i),
     +               ' specifed to be a lake by lake_hru but',
     +               ' hru_type not equal 2'
            ierr = 1
          ENDIF
        ENDDO
      ENDIF
      IF ( ierr==1 ) STOP

      IF ( Basin_area>0.0 ) THEN
        diff = (totarea - Basin_area)/Basin_area
        IF ( Basin_area>0.0 .AND. ABS(diff)>.01D0 .AND. ABS(diff)<.1D0 )
     +       PRINT 9005, Basin_area, totarea, diff*100.0D0
      ENDIF

      IF ( Nssr==Nhru ) THEN
        DO i = 1, Nhru
          Ssres_area(i) = Hru_area(i)
          Hru_ssres(i) = i
        ENDDO
      ELSE
        IF ( getparam(MODNAME, 'hru_ssres', Nhru, 'integer', Hru_ssres)
     +       /=0 ) CALL read_error(2, 'hru_ssres')
        Ssres_area = 0.0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          j = Hru_ssres(i)
          ! assume if hru_type is 2, SSR has zero area
          IF ( Hru_type(i)/=2 )
     +         Ssres_area(j) = Ssres_area(j) + Hru_area(i)
        ENDDO
      ENDIF

      IF ( Ngw==Nhru ) THEN
        Active_gwrs = Active_hrus
        Gwr_route_order = Hru_route_order
        DO i = 1, Nhru
          Gwres_area(i) = Hru_area(i)
          Hru_gwres(i) = i
        ENDDO
      ELSE
        Gwres_area = 0.0
        DO i = 1, Ngw
          Gwr_route_order(i) = i
        ENDDO
        Active_gwrs = Ngw
        IF ( getparam(MODNAME, 'hru_gwres', Nhru, 'integer', Hru_gwres)
     +       /=0 ) CALL read_error(2, 'hru_gwres')
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          j = Hru_gwres(i)
          Gwres_area(j) = Gwres_area(j) + Hru_area(i)
        ENDDO
      ENDIF
!     Basin_area_inv = 1.0D0/totarea
      IF ( active_area<DNEARZERO )STOP 'ERROR: no active area specified'
      Basin_area_inv = 1.0D0/active_area
      Cfs2inches = Basin_area_inv*12.0D0*86400.0D0/43560.0D0
      Basin_lat = Basin_lat*Basin_area_inv
      ! used in solrad modules to winter/summer radiation adjustment
      IF ( Basin_lat>0.0D0 ) THEN
        Hemisphere = 0 ! Northern
      ELSE
        Hemisphere = 1 ! Southern
      ENDIF

      IF ( Print_debug==2 ) THEN
        PRINT *, ' HRU     Area'
        PRINT ('(I4, F14.5)'), (i, Hru_area(i), i=1, Nhru)
        PRINT *, 'Sum of HRU areas      = ', totarea
        PRINT *, 'Active basin area     = ', active_area
        PRINT *, 'Impervious basin area = ', basin_imperv
        PRINT *, 'Pervious basin area   = ', basin_perv
        IF ( Dprst_flag>0 ) PRINT *, 'Depression storage basin area =',
     +                               basin_dprst
        PRINT *, ' '
      ENDIF

!     print out start and end times
      CALL opstr(' Surface Water and Energy Budgets Simulated by:')
      CALL opstr(' PRMS Version 3.0.5 Tag: '//Versn_gsfprms(20:34))
      WRITE (buffer, 9002) ' Start time: ', Starttime
      CALL opstr(buffer(:32))
      WRITE (buffer, 9002) ' End time:   ', Endtime
      CALL opstr(buffer(:32))
      WRITE (buffer, 9003) ' Sum of HRU areas:', totarea,
     +                     ' Active basin area:', active_area
      CALL opstr(buffer(:61))
      WRITE (buffer, 9003) ' Impervious basin area:', basin_imperv,
     +                     ' Pervious basin area:', basin_perv
      CALL opstr(buffer)
      IF ( Dprst_flag>0 ) THEN
        WRITE (buffer, 9003) ' Depression storage basin area:',
     +                       basin_dprst
        CALL opstr(buffer(:45))
      ENDIF
      CALL opstr(' ')

 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
 9003 FORMAT (A, F12.2, A, F12.2)
 9005 FORMAT ('WARNING, basin_area>1% different than sum of HRU areas',
     +        ': basin_area:', F12.3 , ' sum of HRU areas:', F12.3,
     +        ' percent diff:', F12.4, /)

      basinit = 0
      END FUNCTION basinit

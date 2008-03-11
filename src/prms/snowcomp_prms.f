!***********************************************************************
! Program to do snow energy balance computations
!***********************************************************************

! PRMS_SNOW module for defining externally available variables

      MODULE PRMS_SNOW

      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 1.0E-15, INCH2CM = 2.54
      INTEGER :: Nhru, Ndepl
      INTEGER, ALLOCATABLE :: Int_alb(:)
      REAL :: Deninv, Setden, Set1
      REAL, ALLOCATABLE :: Scrv(:), Pksv(:), Snowcov_areasv(:)
      REAL, ALLOCATABLE :: Salb(:), Slst(:), Acum(:), Amlt(:)
!   Declared Variables
      INTEGER, ALLOCATABLE :: Pptmix_nopack(:), Lst(:)
      INTEGER, ALLOCATABLE :: Iasw(:), Iso(:), Mso(:), Lso(:)
      REAL :: Basin_snowmelt, Basin_pweqv
      REAL :: Basin_snowcov, Basin_snowevap, Basin_pk_precip
      REAL, ALLOCATABLE :: Snowmelt(:), Snow_evap(:), Snowcov_area(:)
      REAL, ALLOCATABLE :: Albedo(:), Pk_temp(:), Pk_den(:), Tcal(:)
      REAL, ALLOCATABLE :: Pk_def(:), Pk_ice(:), Freeh2o(:), Pk_depth(:)
      REAL, ALLOCATABLE :: Pss(:), Pst(:), Snsv(:), Pk_precip(:)
      REAL, ALLOCATABLE :: Pkwater_equiv(:), Pkwater_ante(:)
!   Declared Variables from other modules - solrad
      REAL :: Orad, Basin_potsw
      REAL, ALLOCATABLE :: Swrad(:)
!   Declared Variables from other modules - precip
      INTEGER, ALLOCATABLE :: Pptmix(:), Newsnow(:)
      REAL :: Basin_ppt
      REAL, ALLOCATABLE :: Prmx(:)
!   Declared Variables from other modules - intcp
!     INTEGER, ALLOCATABLE :: Int_snow(:)
      REAL, ALLOCATABLE :: Net_ppt(:), Net_snow(:), Net_rain(:)
      REAL, ALLOCATABLE :: Hru_intcpevap(:)
!   Declared Variables from other modules - temp
      REAL, ALLOCATABLE :: Tmaxf(:), Tminf(:), Tavgc(:)
!   Declared Variables from other modules - potet
      INTEGER, ALLOCATABLE :: Transp_on(:)
      REAL, ALLOCATABLE :: Potet(:)
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug, Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER :: Melt_look, Melt_force
      INTEGER, ALLOCATABLE :: Hru_deplcrv(:), Cov_type(:)
      INTEGER, ALLOCATABLE :: Tstorm_mo(:), Hru_type(:)
      REAL :: Albset_rnm, Albset_rna, Albset_snm, Albset_sna
      REAL :: Emis_noppt, Potet_sublim, Freeh2o_cap
      REAL :: Den_init, Settle_const, Den_max, Tmax_allsnow
      REAL, ALLOCATABLE :: Rad_trncf(:), Hru_area(:), Snarea_thresh(:)
      REAL, ALLOCATABLE :: Cecn_coef(:), Covden_win(:)
      REAL, ALLOCATABLE :: Snarea_curve(:, :)

      END MODULE PRMS_SNOW

!***********************************************************************
!     Main snowcomp routine
!***********************************************************************
      INTEGER FUNCTION snowcomp_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: snodecl, snoinit, snorun
!***********************************************************************
      snowcomp_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        snowcomp_prms = snorun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        snowcomp_prms = snodecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        snowcomp_prms = snoinit()
      ENDIF

      END FUNCTION snowcomp_prms

!***********************************************************************
!     snodecl - set up parameters for snowmelt computations
!   Declared Parameters
!     den_init, settle_const, den_max, melt_look
!     melt_force, rad_trncf, hru_deplcrv, snarea_curve, snarea_thresh
!     albset_rnm, albset_rna, albset_snm, albset_sna, potet_sublim
!     emis_noppt, cecn_coef, freeh2o_cap, tstorm_mo, tmax_allsnow
!     hru_area, cov_type, covden_win, hru_type
!***********************************************************************
      INTEGER FUNCTION snodecl()
      USE PRMS_SNOW
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      snodecl = 1

      IF ( declmodule(
     +'$Id: snowcomp_prms.f 3917 2008-02-29 19:19:35Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ndepl = getdim('ndepl')
      IF ( Ndepl.EQ.-1 ) RETURN
      IF ( Ndepl.GT.MAXSNODPL ) THEN
        PRINT *, 'Error, MAXSNODPL < ndepl, snow', MAXSNODPL, Ndepl
        RETURN
      ENDIF

      ALLOCATE (Scrv(Nhru))
      IF ( declpri('snodecl_scrv', Nhru, 'real', Scrv)
     +     .NE.0 ) RETURN

      ALLOCATE (Pksv(Nhru))
      IF ( declpri('snodecl_pksv', Nhru, 'real', Pksv)
     +     .NE.0 ) RETURN

      ALLOCATE (Snowcov_areasv(Nhru))
      IF ( declpri('snodecl_snowcov_areasv', Nhru, 'real',
     +     Snowcov_areasv).NE.0 ) RETURN

      ALLOCATE (Salb(Nhru))
      IF ( declpri('snodecl_salb', Nhru, 'real', Salb)
     +     .NE.0 ) RETURN

      ALLOCATE (Slst(Nhru))
      IF ( declpri('snodecl_slst', Nhru, 'real', Slst)
     +     .NE.0 ) RETURN

      ALLOCATE (Int_alb(Nhru))
      IF ( declpri('snodecl_int_alb', Nhru, 'integer', Int_alb)
     +     .NE.0 ) RETURN

! declare variables
      ALLOCATE (Pk_precip(Nhru))
      IF ( declvar('snow', 'pk_precip', 'nhru', Nhru, 'real',
     +     'HRU precip added to snowpack',
     +     'inches',
     +     Pk_precip).NE.0 ) RETURN

      IF ( declvar('snow', 'basin_pk_precip', 'one', 1, 'real',
     +     'Basin area-weighted average precip added to snowpack',
     +     'inches',
     +     Basin_pk_precip).NE.0 ) RETURN

      ALLOCATE (Albedo(Nhru))
      IF ( declvar('snow', 'albedo', 'nhru', Nhru, 'real',
     +     'Snow surface albedo on an HRU' //
     +     ' or the fraction of radiation reflected from the' //
     +     ' snowpack surface',
     +     'decimal fraction',
     +     Albedo).NE.0 ) RETURN

      ALLOCATE (Pk_temp(Nhru))
      IF ( declvar('snow', 'pk_temp', 'nhru', Nhru, 'real',
     +     'Temperature of the snowpack on an HRU',
     +     'degrees',
     +     Pk_temp).NE.0 ) RETURN

      ALLOCATE (Pk_den(Nhru))
      IF ( declvar('snow', 'pk_den', 'nhru', Nhru, 'real',
     +     'Density of the snowpack on an HRU',
     +     'gm/cm3',
     +     Pk_den).NE.0 ) RETURN

      ALLOCATE (Tcal(Nhru))
      IF ( declvar('snow', 'tcal', 'nhru', Nhru, 'real',
     +     'Net snowpack energy balance on an HRU',
     +     'Langleys',
     +     Tcal).NE.0 ) RETURN

      ALLOCATE (Snow_evap(Nhru))
      IF ( declvar('snow', 'snow_evap', 'nhru', Nhru, 'real',
     +     'Evaporation and sublimation from snowpack on an HRU',
     +     'inches',
     +     Snow_evap).NE.0 ) RETURN

      ALLOCATE (Snowmelt(Nhru))
      IF ( declvar('snow', 'snowmelt', 'nhru', Nhru, 'real',
     +     'Snowmelt from snowpack on an HRU',
     +     'inches',
     +     Snowmelt).NE.0 ) RETURN

      IF ( declvar('snow', 'basin_snowmelt', 'one', 1, 'real',
     +     'Average snowmelt for total basin area',
     +     'inches',
     +     Basin_snowmelt).NE.0 ) RETURN

      IF ( declvar('snow', 'basin_pweqv', 'one', 1, 'real',
     +     'Average snowpack water equivalent for total basin area',
     +     'inches',
     +     Basin_pweqv).NE.0 ) RETURN

      ALLOCATE (Pkwater_equiv(Nhru))
      IF ( declvar('snow', 'pkwater_equiv', 'nhru', Nhru, 'real',
     +     'Snowpack water equivalent on an HRU',
     +     'inches',
     +     Pkwater_equiv).NE.0 ) RETURN

      ALLOCATE (Pkwater_ante(Nhru))
      IF ( declvar('snow', 'pkwater_ante', 'nhru', Nhru, 'real',
     +     'Antecedent snowpack water equivalent on an HRU',
     +     'inches',
     +     Pkwater_ante).NE.0 ) RETURN

      ALLOCATE (Snowcov_area(Nhru))
      IF ( declvar('snow', 'snowcov_area', 'nhru', Nhru, 'real',
     +     'Snow-covered area on an HRU',
     +     'decimal fraction',
     +     Snowcov_area).NE.0 ) RETURN

      IF ( declvar('snow', 'basin_snowevap', 'one', 1, 'real',
     +     'Average evaporation and sublimation for total basin area',
     +     'inches',
     +     Basin_snowevap).NE.0 ) RETURN

      IF ( declvar('snow', 'basin_snowcov', 'one', 1, 'real',
     +     'Average snow-covered area for total basin area',
     +     'decimal fraction',
     +     Basin_snowcov).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Pptmix_nopack(Nhru))
      IF ( declvar('snow', 'pptmix_nopack', 'nhru', Nhru, 'integer',
     +     'Indicator that a rain-snow mix event has occurred' //
     +     ' with no snowpack present on an HRU (1), otherwise (0)',
     +     'none',
     +     Pptmix_nopack).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Iasw(Nhru))
      IF ( declvar('snow', 'iasw', 'nhru', Nhru, 'integer',
     +     'Flag indicating that snow covered area is ' //
     +     ' interpolated between previous location on curve and' //
     +     ' maximum (1), or is on the defined curve (0)',
     +     'none',
     +     Iasw).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Iso(Nhru))
      IF ( declvar('snow', 'iso', 'nhru', Nhru, 'integer',
     +     'Flag to indicate if time is before (1) or after (2)'//
     +     ' the day to force melt season (melt_force)',
     +     'none',
     +     Iso).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Mso(Nhru))
      IF ( declvar('snow', 'mso', 'nhru', Nhru, 'integer',
     +     'Flag to indicate if time is before (1) or after (2)'//
     +     ' the first potential day for melt season (melt_look)',
     +     'none',
     +     Mso).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Lso(Nhru))
      IF ( declvar('snow', 'lso', 'nhru', Nhru, 'integer',
     +     'Counter for tracking the number of days the snowpack'//
     +     ' is at or above 0 degrees C',
     +     '# of iterations',
     +     Lso).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Lst(Nhru))
      IF ( declvar('snow', 'lst', 'nhru', Nhru, 'integer',
     +     'Flag indicating whether there was new snow that' //
     +     ' was insufficient to reset the albedo curve (1)' //
     +     ' (albset_snm or albset_sna), otherwise (0)',
     +     'none',
     +     Lst).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Pk_def(Nhru))
      IF ( declvar('snow', 'pk_def', 'nhru', Nhru, 'real',
     +     'Heat deficit, amount of heat necessary to make' //
     +     ' the snowpack isothermal at 0 degrees C',
     +     'Langleys',
     +     Pk_def).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Pk_ice(Nhru))
      IF ( declvar('snow', 'pk_ice', 'nhru', Nhru, 'real',
     +     'Amount of frozen water in the snowpack',
     +     'inches',
     +     Pk_ice).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Freeh2o(Nhru))
      IF ( declvar('snow', 'freeh2o', 'nhru', Nhru, 'real',
     +     'Free liquid water in the snowpack',
     +     'inches',
     +     Freeh2o).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Pk_depth(Nhru))
      IF ( declvar('snow', 'pk_depth', 'nhru', Nhru, 'real',
     +     'Depth of snowpack',
     +     'inches',
     +     Pk_depth).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Pss(Nhru))
      IF ( declvar('snow', 'pss', 'nhru', Nhru, 'real',
     +     'Previous pack water equivalent plus new ' //
     +     ' snow',
     +     'inches',
     +     Pss).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Pst(Nhru))
      IF ( declvar('snow', 'pst', 'nhru', Nhru, 'real',
     +     'While a snowpack exists, pst tracks the maximum' //
     +     ' snow water equivalent of that snowpack',
     +     'inches',
     +     Pst).NE.0 ) RETURN

      !rpayn commented
      ALLOCATE (Snsv(Nhru))
      IF ( declvar('snow', 'snsv', 'nhru', Nhru, 'real',
     +     'Tracks the cumulative amount of new snow until'//
     +     ' there is enough to reset the albedo curve'//
     +     ' (albset_snm or albset_sna)',
     +     'inches',
     +     Snsv).NE.0 ) RETURN

! declare parameters
      IF ( declparam('snow', 'den_init', 'one', 'real',
     +     '.10', '.01', '.5',
     +     'Initial density of new-fallen snow',
     +     'Initial density of new-fallen snow',
     +     'gm/cm3').NE.0 ) RETURN

      IF ( declparam('snow', 'settle_const', 'one', 'real',
     +     '.10', '.01', '.5',
     +     'Snowpack settlement time constant',
     +     'Snowpack settlement time constant',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam('snow', 'den_max', 'one', 'real',
     +     '.6', '.1', '.8',
     +     'Average maximum snowpack density',
     +     'Average maximum snowpack density',
     +     'gm/cm3').NE.0 ) RETURN

      IF ( declparam('snow', 'melt_look', 'one', 'integer',
     +     '90', '1', '366',
     +     'Julian date to start looking for spring snowmelt',
     +     'Julian date to start looking for spring snowmelt stage.'//
     +     ' Varies with region depending on length of time that'//
     +     ' permanent snowpack exists',
     +     'Julian day').NE.0 ) RETURN

      IF ( declparam('snow', 'melt_force', 'one', 'integer',
     +     '90', '1', '366',
     +     'Julian date to force snowpack to spring snowmelt stage',
     +     'Julian date to force snowpack to spring snowmelt stage;'//
     +     ' varies with region depending on length of time that'//
     +     ' permanent snowpack exists',
     +     'Julian day').NE.0 ) RETURN

      ALLOCATE (Rad_trncf(Nhru))
      IF ( declparam('snow', 'rad_trncf', 'nhru', 'real',
     +     '0.5', '0.', '1.0',
     +     'Solar radiation transmission coefficient',
     +     'Transmission coefficient for short-wave radiation through'//
     +     ' the winter vegetation canopy',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_deplcrv(Nhru))
      IF ( declparam('snow', 'hru_deplcrv', 'nhru', 'integer',
     +     '1', 'bounded', 'ndepl',
     +     'Index number for snowpack areal depletion curve',
     +     'Index number for the snowpack areal depletion curve'//
     +     ' associated with an HRU',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Snarea_curve(11, Ndepl))
      IF ( declparam('snow', 'snarea_curve', 'ndeplval', 'real',
     +     '1.0', '0.0', '1.0',
     +     'Snow area depletion curve values',
     +     'Snow area depletion curve values, 11 values for each '//
     +     ' curve (0 to 100 percent in 10 percent increments)',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Snarea_thresh(Nhru))
      IF ( declparam('snow', 'snarea_thresh', 'nhru', 'real',
     +     '50.', '0.', '200',
     +     'Maximum threshold water equivalent for snow depletion',
     +     'The maximum threshold snowpack water equivalent below'//
     +     ' which the snow-covered-area curve is applied. Varies'//
     +     ' with elevation.',
     +     'inches').NE.0 ) RETURN

      IF ( declparam('snow', 'albset_rnm', 'one', 'real',
     +     '.6', '0.', '1.0',
     +     'Albedo reset - rain,  melt stage',
     +     'Proportion of rain in a rain-snow precipitation event'//
     +     ' above which the snow albedo is not reset. Applied during'//
     +     ' the snowpack melt stage',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam('snow', 'albset_rna', 'one', 'real',
     +     '.8', '0.', '1.0',
     +     'Albedo reset - rain, accumulation stage',
     +     'Proportion of rain in a rain-snow precipitation event'//
     +     ' above which the snow albedo is not reset. Applied during'//
     +     ' the snowpack accumulation stage.',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam('snow', 'albset_snm', 'one', 'real',
     +     '.2', '.001', '1.',
     +     'Albedo reset - snow, melt stage',
     +     'Minimum snowfall, in water equivalent, needed to reset'//
     +     ' snow albedo during the snowpack melt stage',
     +     'inches').NE.0 ) RETURN

      IF ( declparam('snow', 'albset_sna', 'one', 'real',
     +     '.05', '.001', '1.',
     +     'Albedo reset - snow, accumulation stage',
     +     'Minimum snowfall, in water equivalent, needed to reset'//
     +     ' snow albedo during the snowpack accumulation stage',
     +     'inches').NE.0 ) RETURN

      IF ( declparam('snow', 'emis_noppt', 'one', 'real',
     +     '.757', '.757', '1.0',
     +     'Emissivity of air on days without precipitation',
     +     'Average emissivity of air on days without precipitation',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Cecn_coef(MAXMO))
      IF ( declparam('snow', 'cecn_coef', 'nmonths', 'real',
     +     '5.', '0.', '20.',
     +     'Convection condensation energy coefficient',
     +     'Convection condensation energy coefficient, varied monthly',
     +     'calories per degree C above 0').NE.0 ) RETURN

      IF ( declparam('snow', 'potet_sublim', 'one', 'real',
     +     '.5', '.1', '.75',
     +     'Proportion of potential ET that is sublimated from snow'//
     +     ' surface',
     +     'Proportion of potential ET that is sublimated from the'//
     +     ' snow surface',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam('snow', 'freeh2o_cap', 'one', 'real',
     +     '.05', '.01', '.2',
     +     'Free-water holding capacity of snowpack',
     +     'Free-water holding capacity of snowpack expressed as a' //
     +     ' decimal fraction of the frozen water content of the' //
     +     ' snowpack (pk_ice)', 
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('snow', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      IF ( declparam('snow', 'tmax_allsnow', 'one', 'real',
     +     '32.', '-10.', '40.',
     +     'Precip all snow if HRU max temperature below this value',
     +     'If maximum temperature of an HRU is less than or equal to'//
     +     ' this value, precipitation is assumed to be snow',
     +     'degrees').NE.0 ) RETURN

      ALLOCATE (Cov_type(Nhru))
      IF ( declparam('snow', 'cov_type', 'nhru', 'integer',
     +     '3', '0', '3',
     +     'Cover type designation for HRU',
     +     'Vegetation cover type designation for HRU'//
     +     ' (0=bare soil; 1=grasses; 2=shrubs; 3=trees)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Tstorm_mo(MAXMO))
      IF ( declparam('snow', 'tstorm_mo', 'nmonths', 'integer',
     +     '0', '0', '1',
     +     'Set to 1 if thunderstorms prevalent during month',
     +     'Monthly indicator for prevalent storm type (0=frontal'//
     +     ' storms prevalent; 1=convective storms prevalent)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Covden_win(Nhru))
      IF ( declparam('snow', 'covden_win', 'nhru', 'real',
     +     '.5', '0.', '1.0',
     +     'Winter vegetation cover density for major vegetation type',
     +     'Winter vegetation cover density for the major'//
     +     ' vegetation type on each HRU',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('snow', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '2',
     +     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     +     'none').NE.0 ) RETURN

! Allocate arrays for variables from other modules and local
      ALLOCATE (Swrad(Nhru), Pptmix(Nhru), Newsnow(Nhru), Prmx(Nhru))
      ALLOCATE (Net_ppt(Nhru), Net_snow(Nhru), Net_rain(Nhru))
      ALLOCATE (Hru_intcpevap(Nhru))
      ALLOCATE (Tmaxf(Nhru), Tminf(Nhru), Tavgc(Nhru))
      ALLOCATE (Transp_on(Nhru), Potet(Nhru), Hru_route_order(Nhru))
      ALLOCATE (Acum(MAXALB), Amlt(MAXALB))
!     ALLOCATE (Int_snow(Nhru))

      snodecl = 0
      END FUNCTION snodecl

!***********************************************************************
!     snoinit - Initialize snowcomp module - get parameter values,
!                compute initial values
!***********************************************************************
      INTEGER FUNCTION snoinit()
      USE PRMS_SNOW
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, nstep
! Save Variables
      REAL, SAVE :: acum_init(MAXALB), amlt_init(MAXALB)
      DATA acum_init/.80, .77, .75, .72, .70, .69, .68, .67, .66, .65,
     +     .64, .63, .62, .61, .60/
      DATA amlt_init/.72, .65, .60, .58, .56, .54, .52, .50, .48, .46,
     +     .44, .43, .42, .41, .40/
!***********************************************************************
      snoinit = 1

      IF ( getparam('snow', 'den_init', 1, 'real', Den_init)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'settle_const', 1, 'real', Settle_const)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'den_max', 1, 'real', Den_max)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'melt_look', 1, 'integer', Melt_look)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'melt_force', 1, 'integer', Melt_force)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'rad_trncf', Nhru, 'real', Rad_trncf)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'hru_deplcrv', Nhru, 'integer', Hru_deplcrv)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'snarea_curve', Ndepl*11, 'real',
     +     Snarea_curve).NE.0 ) RETURN

      IF ( getparam('snow', 'snarea_thresh', Nhru, 'real',
     +     Snarea_thresh).NE.0 ) RETURN

      IF ( getparam('snow', 'albset_rnm', 1, 'real', Albset_rnm)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'albset_rna', 1, 'real', Albset_rna)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'albset_sna', 1, 'real', Albset_sna)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'albset_snm', 1, 'real', Albset_snm)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'emis_noppt', 1, 'real', Emis_noppt)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'cecn_coef', MAXMO, 'real', Cecn_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'potet_sublim', 1, 'real', Potet_sublim)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'freeh2o_cap', 1, 'real', Freeh2o_cap)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'tmax_allsnow', 1, 'real', Tmax_allsnow)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'cov_type', Nhru, 'integer', Cov_type)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'tstorm_mo', MAXMO, 'integer', Tstorm_mo)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'covden_win', Nhru, 'real', Covden_win)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getparam('snow', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      nstep = getstep()
      IF ( nstep.EQ.0 ) THEN
        Pkwater_equiv = 0.0
        Pkwater_ante = 0.0
        Snowmelt = 0.0
        Snow_evap = 0.0
        Snowcov_area = 0.0
        Pptmix_nopack = 0
        Tcal = 0.0
        Iasw = 0
        Iso = 1
        Mso = 1
        Lso = 0
        Pk_def = 0.0
        Pk_temp = 0.0
        Pk_ice = 0.0
        Freeh2o = 0.0
        Pk_depth = 0.0
        Pss = 0.0
        Pst = 0.0
        Pk_den = 0.0
        Albedo = 0.0
        Snsv = 0.0
        Lst = 0
        Int_alb = 1
        Salb = 0.0
        Slst = 0.0
        Snowcov_areasv = 0.0
        Scrv = 0.0
        Pksv = 0.0
        Pk_precip = 0.0
        Basin_snowmelt = 0.0
        Basin_pweqv = 0.0
        Basin_snowevap = 0.0
        Basin_snowcov = 0.0
        Basin_pk_precip = 0.0
      ENDIF

      DO i = 1, MAXALB
        Acum(i) = acum_init(i)
        Amlt(i) = amlt_init(i)
      ENDDO

      !Deninv = 1./Den_init
      !Setden = Settle_const/Den_max
      !Set1 = 1./(1.+Settle_const)

      IF ( Prt_debug.EQ.1 ) THEN
        OPEN (196, FILE='snowcomp.wbal')
        WRITE (196, 9001)
      ENDIF

      snoinit = 0

 9001 FORMAT (
     + ' Year Day  Snow Bal  Snowpack  Snowmelt  Snowevap  Snowcovr')

      END FUNCTION snoinit

!***********************************************************************
!     snorun -
!***********************************************************************
      INTEGER FUNCTION snorun()
      USE PRMS_SNOW
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      EXTERNAL ppt_to_pack, snowcov, snalbedo, snowbal, snowevap
      REAL, EXTERNAL :: f_to_c_sno
      INTRINSIC ABS, SQRT
! Local Variables
      INTEGER :: i, j, k, mo, jwday, jday, niteda, nowtime(6)
      REAL :: trd, sw, effk, cst, temp, cals
      REAL :: tminc, tmaxc, emis, esv, swn, cec, dpt1
      REAL :: hrubal, bsnobal
      DOUBLE PRECISION :: timestep
!***********************************************************************
      snorun = 1

      ! Set the basin totals to 0 
      ! (recalculated at the end of the time step)
      Basin_snowmelt = 0.
      Basin_pweqv = 0.
      Basin_snowevap = 0.
      Basin_snowcov = 0.
      Basin_pk_precip = 0.
      bsnobal = 0.

      ! Snorun will terminate if the time step less 
      ! than a day (no storm mode)
      timestep = deltim()
      IF ( timestep.LT.23.999D0 ) THEN
        snorun = 0
        RETURN
      ENDIF

      ! Get the current month, julian day, and julian 
      ! water day for this time step
      CALL dattim('now', nowtime)
      mo = nowtime(2)
      jday = julian('now', 'calendar')
      jwday = julian('now', 'water')

      ! rsr: Orad is computed if no observed value is available
      IF ( getvar('solrad', 'orad', 1, 'real', Orad).NE.0 ) RETURN

      IF ( getvar('solrad', 'basin_potsw', 1, 'real', Basin_potsw)
     +     .NE.0 ) RETURN

      IF ( getvar('solrad', 'swrad', Nhru, 'real', Swrad)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'net_ppt', Nhru, 'real', Net_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'net_snow', Nhru, 'real', Net_snow)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'net_rain', Nhru, 'real', Net_rain)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'hru_intcpevap', Nhru, 'real', Hru_intcpevap)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'pptmix', Nhru, 'integer', Pptmix)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'newsnow', Nhru, 'integer', Newsnow)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'basin_ppt', 1, 'real', Basin_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'prmx', Nhru, 'real', Prmx)
     +     .NE.0 ) RETURN

      IF ( getvar('temp', 'tmaxf', Nhru, 'real', Tmaxf)
     +     .NE.0 ) RETURN

      IF ( getvar('temp', 'tminf', Nhru, 'real', Tminf)
     +     .NE.0 ) RETURN

      IF ( getvar('temp', 'tavgc', Nhru, 'real', Tavgc)
     +     .NE.0 ) RETURN

      IF ( getvar('potet', 'transp_on', Nhru, 'integer', Transp_on)
     +     .NE.0 ) RETURN

      IF ( getvar('potet', 'potet', Nhru, 'real', Potet)
     +     .NE.0 ) RETURN

!     IF ( get var('intcp', 'int_snow', Nhru, 'integer', Int_snow)
!    +     .NE.0 ) RETURN

      ! If it's the first julian day of the water year, several 
      ! variables need to be reset
      ! - reset the previous snow water eqivalent plus new snow to 0
      ! - reset flags to indicate it is not melt season or potetential 
      !   melt season
      ! - reset the counter for the number of days a snowpack is at 
      !   0 degC
      IF ( jwday.EQ.1 ) THEN
        Pss = 0.0 ! [inches]
        Iso = 1 ! [flag]
        Mso = 1 ! [flag]
        Lso = 0 ! [counter]
      ENDIF

      ! Calculate the ratio of observed radiation to potential radiation
      ! (used as a cumulative indicator of cloud cover)
      trd = Orad/Basin_potsw ! [dimensionless ratio]

      ! Loop through all the active HRUs, in routing order
      DO j = 1, Active_hrus
        i = Hru_route_order(j) ! [counter]

        ! HRU SET-UP - SET DEFAULT VALUES AND/OR BASE 
        !              CONDITIONS FOR THIS TIME PERIOD
        !**************************************************************

        ! Keep track of the pack water equivalent before it is changed 
        ! by precipitation during this time step
        Pkwater_ante(i) = Pkwater_equiv(i)

        ! By default, the precipitation added to snowpack, snowmelt,
        ! and snow evaporation are 0
        Pk_precip(i) = 0.0 ! [inches]
        Snowmelt(i) = 0.0 ! [inches]
        Snow_evap(i) = 0.0 ! [inches]

        ! By default, there has not been a mixed event without a 
        ! snowpack
        Pptmix_nopack(i) = 0 ! [flag]

        ! Skip the HRU if it is a lake
        IF ( Hru_type(i).EQ.2 ) CYCLE

        ! If the day of the water year is beyond the forced melt day 
        ! indicated by the parameter, then set the flag indicating 
        ! melt season
        IF ( jday.EQ.Melt_force ) Iso(i) = 2 ! [flag]

        ! If the day of the water year is beyond the first day to 
        ! look for melt season indicated by the parameter,
        ! then set the flag indicating to watch for melt season
        IF ( jday.EQ.Melt_look ) Mso(i) = 2 ! [flag]

!rsr    IF ( Pkwater_ante(i).LE.NEARZERO .AND. Newsnow(i).EQ.0
!rsr +       .AND. Int_snow(i).EQ.0 ) CYCLE

        ! Skip the HRU if there is no snowpack and no new snow
        IF ( Pkwater_ante(i).LT.NEARZERO .AND. Newsnow(i).EQ.0 ) CYCLE
        
        ! If there is no existing snow pack and there is new snow, the
        ! initial snow covered area is complete (1)
        IF ( Newsnow(i).EQ.1 .AND. Pkwater_ante(i).LT.NEARZERO )
     +       Snowcov_area(i) = 1.0 ! [fraction of area]

        ! HRU STEP 1 - DEAL WITH PRECIP AND ITS EFFECT ON THE WATER
        !              CONTENT AND HEAT CONTENT OF SNOW PACK
        !************************************************************
        
        ! If there is net precipitation on an existing snowpack, OR if
        ! there is any net snow, add the incoming water (or ice) and 
        ! heat (or heat deficit) to the snowpack
        IF ( (Pkwater_ante(i).GT.0.0.AND.Net_ppt(i).GT.0.0)
     +        .OR. Net_snow(i).GT.0. )
     +    CALL ppt_to_pack(Pptmix(i), Iasw(i), Tmaxf(i), Tminf(i),
     +                     Tavgc(i), Tmax_allsnow, Pkwater_equiv(i),
     +                     Net_rain(i), Pk_def(i), Pk_temp(i),
     +                     Pk_ice(i), Freeh2o(i), Snowcov_area(i),
     +                     Snowmelt(i), Pk_depth(i), Pss(i), Pst(i),
     +                     Net_snow(i), Pk_den(i), Freeh2o_cap,
     +                     Pptmix_nopack(i), Pk_precip(i))
!       CALL dpreal('pk_temp-ptp', Pk_temp(i), 1, 1)
!       CALL dpreal('tcal-ptp', Tcal(i), 1, 1)

        ! If there is still a snowpack
        IF ( Pkwater_equiv(i).GT.0.0 ) THEN

          ! HRU STEP 2 - CALCULATE THE NEW SNOW COVERED AREA
          !**********************************************************
          ! Compute snow-covered area if depletion curves are available
          IF ( Ndepl.GT.0 ) THEN
            ! use the snow depletion curve for the current HRU
            k = Hru_deplcrv(i)
            ! calculate the new snow covered area
            CALL snowcov(Iasw(i), Newsnow(i),
     +                   Snowcov_area(i), Snarea_curve(1, k),
     +                   Pkwater_equiv(i), Pst(i), Snarea_thresh(i),
     +                   Net_snow(i), Scrv(i), Pksv(i),
     +                   Snowcov_areasv(i))
          ENDIF
!         CALL dpreal('pk_temp-scov', Pk_temp(i), 1, 1)
!         CALL dpreal('tcal-scov', Tcal(i), 1, 1)

          ! HRU STEP 3 - COMPUTE THE NEW ALBEDO
          !**********************************************************

          ! Compute albedo if there is any snowpack
          CALL snalbedo(Newsnow(i), Iso(i), Lst(i), Snsv(i),
     +                  Prmx(i), Pptmix(i), Albset_rnm, Net_snow(i),
     +                  Albset_snm, Albset_rna, Albset_sna, Albedo(i),
     +                  Acum, Amlt, Int_alb(i), Salb(i), Slst(i))


!         CALL dpreal('pk_temp-alb', Pk_temp(i), 1, 1)
!         CALL dpreal('tcal-alb', Tcal(i), 1, 1)

          ! HRU STEP 4 - DETERMINE RADIATION FLUXES AND SNOWPACK
          !              STATES NECESSARY FOR ENERGY BALANCE
          !**********************************************************
          
          tminc = f_to_c_sno(Tminf(i)) ! [degrees C]
          tmaxc = f_to_c_sno(Tmaxf(i)) ! [degrees C]
          ! Set the emissivity of the air to the emissivity when there
          ! is no precipitation
          emis = Emis_noppt ! [fraction of radiation]
          ! If there is any precipitation in the basin, reset the
          ! emissivity to 1
          IF ( Basin_ppt.GT.0. ) emis = 1. ! [fraction of radiation]
          ! Save the current value of emissivity
          esv = emis ! [fraction of radiation]
          ! The incoming shortwave radiation is the HRU radiation
          ! adjusted by the albedo (some is reflected back into the
          ! atmoshphere) and the transmission coefficient (some is
          ! intercepted by the winter vegetative canopy)
          swn = Swrad(i)*(1.-Albedo(i))*Rad_trncf(i) ! [cal/cm^2] 
                                                     ! or [Langleys]
          ! Set the convection-condensation for a half-day interval
          cec = Cecn_coef(mo)*.5 ! [cal/(cm^2 degC)] 
                                 ! or [Langleys / degC]
          ! If the land cover is trees, reduce the convection-
          ! condensation parameter by half
          IF ( Cov_type(i).EQ.3 ) cec = cec*.5 ! [cal/(cm^2 degC)] 
                                               ! or [Langleys / degC]

          ! The snow depth depends on the previous snow pack water 
          ! equivalent plus the new net snow 
          Pss(i) = Pss(i) + Net_snow(i) ! [inches]
          ! Calculate the new snow depth (Riley et al. 1973)
          dpt1 = Pk_depth(i) + (Net_snow(i)/Den_init) +
     +           Settle_const * ((Pss(i)/Den_max) - Pk_depth(i))
!          dpt1 = ((Net_snow(i)*Deninv)+
!     +           (Setden*Pss(i))+Pk_depth(i))*Set1 ! [inches]
!RAPCOMMENT - CHANGED TO THE APPROPRIATE FINITE DIFFERENCE 
!             APPROXIMATION OF SNOW DEPTH    
          Pk_depth(i) = dpt1 ! [inches]
          ! Calculate the snowpack density
          Pk_den(i) = Pkwater_equiv(i)/dpt1 ! [inch water equiv. 
                                            !  / inch depth]
          ! The effective thermal conductivity is approximated
          ! (empirically) as 0.0077 times (snowpack density)^2 
          ! [cal / (sec g degC)] Therefore, the effective 
          ! conductivity term (inside the square root) in the 
          ! equation for conductive heat exchange can be 
          ! calculated as follows (0.0077*pk_den^2)/(pk_den*0.5)
          ! where 0.5 is the specific heat of ice [cal / (g degC)]
          ! this simplifies to the following
          effk = .0154*Pk_den(i) ! [unitless]
          ! 13751 is the number of seconds in 12 hours over pi
          ! So for a half day, to calculate the conductive heat
          ! exchange per cm snow per cm^2 area per degree
          ! temperature difference is the following
          ! In effect, multiplying cst times the temperature
          ! gradient gives the heatexchange by heat conducted
          ! (calories) per square cm of snowpack
          cst = Pk_den(i)*(SQRT(effk*13751.)) ! [cal/(cm^2 degC)]
                                              ! or [Langleys / degC]

          ! Check whether to force spring melt
          ! Spring melt is forced if time is before the melt-force
          ! day and after the melt-look day (parameters)
          ! If between these dates, the spring melt applies if the
          ! snowpack temperature is above or equal to 0
          ! for more than 4 cycles of the snorun function

          ! If before the first melt-force day
          IF ( Iso(i).EQ.1 ) THEN
            ! If after the first melt-look day
            IF ( Mso(i).EQ.2 ) THEN

              ! Melt season is determined by the number of days the
              ! snowpack is above 0 degrees C.  The first time that 
              ! the snowpack is isothermal at 0 degrees C for more 
              ! than 4 days is the beginning of snowmelt season.
              ! 2 options below (if-then, else)

              ! (1) The snowpack temperature is 0 degrees
              IF ( Pk_temp(i).GE.0. ) THEN
                ! Increment the number of days that the snowpack
                ! has been isothermal at 0 degrees C
                Lso(i) = Lso(i) + 1 ! [days]
                ! If the snowpack temperature has been 0 or greater
                ! for more than 4 cycles
                IF ( Lso(i).GT.4 ) THEN
                  ! Set the melt-force flag and reset counter
                  Iso(i) = 2 ! [flag]
                  Lso(i) = 0 ! [days]
                ENDIF

              ! (2) The snowpack temperature is less than 0 degrees
              ELSE
                ! Reset the counter for days snowpack temp is above 0
                Lso(i) = 0 ! [days]
              ENDIF
            ENDIF
          ENDIF

          ! Compute energy balance for night period
          ! niteda is a flag indicating nighttime (1) or daytime (2)
          ! set the flag indicating night time
          niteda = 1 ! [flag]
          ! no shortwave (solar) radiation at night
          sw = 0. ! [cal / cm^2] or [Langleys]
          ! temparature is halfway between the minimum and average temp
          ! for the day
          temp = (tminc+Tavgc(i))*.5
          ! calculate the night time energy balance
          CALL snowbal(niteda, Tstorm_mo(mo), Iasw(i),
     +                 temp, esv, Basin_ppt, trd, Emis_noppt,
     +                 Covden_win(i), cec, Pkwater_equiv(i), Pk_def(i),
     +                 Pk_temp(i), Pk_ice(i), Freeh2o(i), Freeh2o_cap,
     +                 Snowcov_area(i), Snowmelt(i), Pk_depth(i),
     +                 Pss(i), Pst(i), Pk_den(i), cst, cals, sw)
          ! track total heat flux from both night and day periods
          Tcal(i) = cals ! [cal/cm^2] or [Langleys]
!         CALL dpreal('pk_temp-nite', Pk_temp(i), 1, 1)
!         CALL dpreal('tcal-nite', Tcal(i), 1, 1)

          ! Compute energy balance for day period (if the snowpack
          ! still exists)
          IF ( Pkwater_equiv(i).GT.0. ) THEN
            ! set the flag indicating daytime
            niteda = 2 ! [flag]
            ! set shortwave radiation as calculated earlier
            sw = swn ! [cal/cm^2] or [Langleys]
            ! temparature is halfway between the maximum and average
            ! temp for the day
            temp = (tmaxc+Tavgc(i))*.5 ! [degrees C]
            CALL snowbal(niteda, Tstorm_mo(mo), Iasw(i),
     +                   temp, esv, Basin_ppt, trd, Emis_noppt,
     +                   Covden_win(i), cec, Pkwater_equiv(i),
     +                   Pk_def(i), Pk_temp(i), Pk_ice(i), Freeh2o(i),
     +                   Freeh2o_cap, Snowcov_area(i), Snowmelt(i),
     +                   Pk_depth(i), Pss(i), Pst(i), Pk_den(i),
     +                   cst, cals, sw)
          ! track total heat flux from both night and day periods
            Tcal(i) = Tcal(i) + cals ! [cal/cm^2] or [Langleys]
!           CALL dpreal('pk_temp-day', Pk_temp(i), 1, 1)
!           CALL dpreal('tcal-day', Tcal(i), 1, 1)
          ENDIF

          !  HRU STEP 5 - CALCULATE SNOWPACK LOSS TO EVAPORATION
          !********************************************************

          ! Compute snow evaporation (if there is still a snowpack)
          IF ( Pkwater_equiv(i).GT.0. ) THEN
            ! Snow can evaporate when transpiration is not occuring
            ! or when transpiration is occuring with cover types of
            ! bare soil or grass
            IF ( Transp_on(i).EQ.0 .OR.
     +           (Transp_on(i).EQ.1 .AND. Cov_type(i).LE.1) )
     +           CALL snowevap(Cov_type(i), Potet_sublim, Potet(i),
     +                        Snowcov_area(i), Hru_intcpevap(i),
     +                        Snow_evap(i), Pkwater_equiv(i), Pk_ice(i),
     +                        Pk_def(i), Freeh2o(i), Pk_temp(i))
          ENDIF

          !  HRU CLEAN-UP - ADJUST FINAL HRU SNOWPACK STATES AND
          !                 INCREMENT THE BASIN TOTALS
          !*********************************************************

          ! Final state of the snowpack depends on whether it still
          ! exists after all the processing above
          ! 2 options below (if-then, else)

          ! (1) Snow pack still exists
          IF ( Pkwater_equiv(i).GT.0. ) THEN
            ! Snowpack still exists
            Pk_depth(i) = Pkwater_equiv(i)/Pk_den(i)
            Pss(i) = Pkwater_equiv(i)
            ! If it is during the melt period and snowfall was
            ! insufficient to reset albedo, then reduce the cumulative
            ! new snow by the amount melted during the period
            ! (but don't let it be negative)
            IF ( Lst(i).GT.0 ) THEN
              Snsv(i) = Snsv(i) - Snowmelt(i)
              IF ( Snsv(i).LE.0. ) Snsv(i) = 0.
            ENDIF

          ! (2) Snow pack no longer exists
          ELSE
            ! Snowpack has been completely depleted, reset all states
            ! to no-snowpack values
            Pk_depth(i) = 0.
            Pss(i) = 0.
            Snsv(i) = 0.
            Lst(i) = 0
            Pst(i) = 0.
            Iasw(i) = 0
            Albedo(i) = 0.
            Pk_den(i) = 0.
            Snowcov_area(i) = 0.
            Pk_def(i) = 0.
            Pk_temp(i) = 0.
            Pk_ice(i) = 0.
            Freeh2o(i) = 0.
          ENDIF
        ENDIF
        ! Sum volumes for basin totals
        Basin_snowmelt = Basin_snowmelt + Snowmelt(i)*Hru_area(i)
        Basin_pweqv = Basin_pweqv + Pkwater_equiv(i)*Hru_area(i)
        Basin_snowevap = Basin_snowevap + Snow_evap(i)*Hru_area(i)
        Basin_snowcov = Basin_snowcov + Snowcov_area(i)*Hru_area(i)
        Basin_pk_precip = Basin_pk_precip + Pk_precip(i)*Hru_area(i)

        IF ( Prt_debug.EQ.1 ) THEN
          hrubal = Pkwater_ante(i) - Pkwater_equiv(i) - Snow_evap(i)
     +             - Snowmelt(i)
          IF ( Pptmix_nopack(i).EQ.1 ) THEN
            hrubal = hrubal + Net_snow(i)
          ELSE
            hrubal = hrubal + Net_ppt(i)
          ENDIF
          IF ( ABS(hrubal).GT.2.0E-5 ) THEN
            IF ( ABS(hrubal).GT.1.0E-4 ) THEN
              WRITE (196, *) 'Possible water balance error'
            ELSE
              WRITE (196, *) 'Hru snow rounding issue'
            ENDIF
            WRITE (196,*) i, hrubal, nowtime(1), nowtime(2), nowtime(3),
     +             Pkwater_ante(i), Pkwater_equiv(i), Snow_evap(i),
     +             Snowmelt(i), Net_ppt(i), Net_snow(i), Net_rain(i),
     +             Newsnow(i), Pptmix(i), Pptmix_nopack(i)
          ENDIF
          bsnobal = bsnobal + hrubal
        ENDIF
      ENDDO

      ! Area normalize basin totals
      Basin_snowmelt = Basin_snowmelt*Basin_area_inv
      Basin_pweqv = Basin_pweqv*Basin_area_inv
      Basin_snowevap = Basin_snowevap*Basin_area_inv
      Basin_snowcov = Basin_snowcov*Basin_area_inv
      Basin_pk_precip = Basin_pk_precip*Basin_area_inv

      IF ( Prt_debug.EQ.1 ) THEN
        IF ( ABS(bsnobal).GT.1.0E-4 ) THEN
          WRITE (196, *) 'Possible water balance error'
        ELSEIF ( ABS(bsnobal).GT.2.0E-5 ) THEN
          WRITE (196, *) 'Basin snow rounding issue', bsnobal, nowtime
        ENDIF
        WRITE (196, 9002) nowtime(1), jday, bsnobal, Basin_pweqv,
     +                    Basin_snowmelt, Basin_snowevap, Basin_snowcov
      ELSEIF ( Prt_debug.EQ.9 ) THEN
        PRINT 9001, jday, (Net_rain(i), i=1, Nhru)
        PRINT 9001, jday, (Net_snow(i), i=1, Nhru)
        PRINT 9001, jday, (Snowmelt(i), i=1, Nhru)
      ENDIF

      snorun = 0

 9001 FORMAT (I5, 177F6.3)
 9002 FORMAT (I5, I4, 5F10.5)

      END FUNCTION snorun

!***********************************************************************
!      Subroutine to add rain and/or snow to snowpack
!***********************************************************************
      SUBROUTINE ppt_to_pack(Pptmix, Iasw, Tmaxf, Tminf,
     +                       Tavgc, Tmax_allsnow, Pkwater_equiv,
     +                       Net_rain, Pk_def, Pk_temp, Pk_ice,
     +                       Freeh2o, Snowcov_area,
     +                       Snowmelt, Pk_depth, Pss, Pst,
     +                       Net_snow, Pk_den, Freeh2o_cap,
     +                       Pptmix_nopack, Pk_precip)
      USE PRMS_SNOW, ONLY:NEARZERO, INCH2CM
      IMPLICIT NONE
      REAL, EXTERNAL :: f_to_c_sno
      EXTERNAL calin
      INTRINSIC ABS
! Arguments
      INTEGER, INTENT(IN) :: Pptmix
      INTEGER, INTENT(OUT) :: Iasw, Pptmix_nopack
      REAL, INTENT(IN) :: Tmaxf, Tminf, Tavgc, Net_rain, Net_snow
      REAL, INTENT(IN) :: Tmax_allsnow, Freeh2o_cap
      REAL, INTENT(INOUT) :: Pkwater_equiv, Snowmelt, Freeh2o, Pk_precip
      REAL, INTENT(INOUT) :: Pk_def, Pk_ice, Pk_den, Snowcov_area
      REAL, INTENT(OUT) :: Pk_temp, Pk_depth, Pss, Pst
! Local Variables
      REAL :: train, tsnow, caln, pndz, calpr, calps
!***********************************************************************

      ! The temperature of precip will be different if it is mixed or
      ! all rain or snow 2 options below (if-then, else)

      ! (1) If precip is mixed...
      IF ( Pptmix.EQ.1 ) THEN
        ! If there is any rain, the rain temp is halfway between the max
        ! temperature and the allsnow temperature
        train = f_to_c_sno((Tmaxf+Tmax_allsnow)*0.5) ! [degrees C]

        ! Temperatures will be different, depending on if there is an
        ! existing snowpack or not

        ! If there is a snowpack, snow temperature is halfway between
        ! the minimum daily temperature and maximum temperature for
        ! which all precipitation is snow
        IF ( Pkwater_equiv.GT.0. ) THEN
          tsnow = f_to_c_sno((Tminf+Tmax_allsnow)*0.5) ! [degrees C]

        ! If there is no existing snowpack, snow temperature is the
        ! average temperature for the day
        ELSE
          tsnow = Tavgc ! [degrees C]
        ENDIF

      ! (2) If precip is all snow or all rain...
      ELSE
        ! If there is any rain, the rain temperature is the average
        ! temperature
        train = Tavgc ! [degrees C]
        ! If average temperature is close to freezing, the rain
        ! temperature is halfway between the maximum daily temperature
        ! and maximum temperature for which all precipitation is snow
        IF ( train.LT.NEARZERO )
     +       train = f_to_c_sno((Tmaxf+Tmax_allsnow)*0.5) ! [degrees C]
        ! If there is any snow, the snow temperature is the average
        ! temperature
        tsnow = Tavgc ! [degrees C]
      ENDIF

      ! Temperatures close to 0 are treated as zero
      IF ( train.LT.NEARZERO ) train = 0.0 ! [degrees C]
      IF ( tsnow.GT.-NEARZERO ) tsnow = 0.0 ! [degrees C]

      ! Leavesley comments...
      ! If snowpack already exists, add rain first, then add
      ! snow.  If no antecedent snowpack, rain is already taken care
      ! of, so start snowpack with snow.  This SUBROUTINE assumes
      ! that in a mixed event, the rain will be first and turn to
      ! snow as the temperature drops.

      ! Rain can only add to the snowpack if a previous snowpack
      ! exists, so rain or a mixed event is processed differently
      ! when a snowpack exists
      ! 2 options below (if-then, elseif)

      ! (1) If there is net rain on an existing snowpack...
      IF ( Pkwater_equiv.GT.0. ) THEN
        IF ( Net_rain.GT.0.0 ) THEN
          ! Add rain water to pack (rain on snow) and increment the
          ! precipitation on the snowpack by the rain water
          Pkwater_equiv = Pkwater_equiv + Net_rain ! [inches]
          Pk_precip = Pk_precip + Net_rain ! [inches]

          ! Incoming rain water carries heat that must be added to
          ! the snowpack.
          ! This heat could both warm the snowpack and melt snow.
          ! Handling of this heat depends on the current thermal
          ! condition of the snowpack.
          ! 2 options below (if-then, else)

          ! (1.1) If the snowpack is colder than freezing it has a
          ! heat deficit (requires heat to be brought to isothermal
          ! at 0 degC)...
          IF ( Pk_def.GT.0.0 ) THEN
            ! Calculate the number of calories given up per inch of
            ! rain when cooling it from the current rain temperature
            ! to 0 deg C and then freezing it (liquid to solid state
            ! latent heat)
            ! This calculation assumes a volume of an inch of rain
            ! over a square cm of area
            ! 80 cal come from freezing 1 cm3 at 0 C
            ! (latent heat of fusion is 80 cal/cm^3), 
            ! 1 cal from cooling 1cm3 for every degree C
            ! (specific heat of water is 1 cal/(cm^3 degC)), 
            ! convert from 1 cm depth over 1 square cm to 
            ! 1 inch depth over 1 square cm (INCH2CM = 2.54 cm/in)
            caln = (80.+train)*INCH2CM ! [cal / (in cm^2)]
            ! calculate the amount of rain in inches
            ! (at the current rain temperature) 
            ! needed to bring the snowpack to isothermal at 0
            pndz = Pk_def/caln ! [inches]

            ! The effect of rain on the snowpack depends on if there
            ! is not enough, enough, or more than enough heat in the
            ! rain to bring the snowpack to isothermal at 0 degC or not
            ! 3 options below (if-then, elseif, else)

            ! (1.1.1) Exactly enough rain to bring pack to isothermal...
            IF ( ABS(Net_rain-pndz).LT.NEARZERO ) THEN
              ! Heat deficit and temperature of the snowpack go to 0
              Pk_def = 0. ! [cal/cm^2]
              Pk_temp = 0. ! [degrees C]
              ! In the process of giving up its heat, all the net rain 
              ! freezes and becomes pack ice
              Pk_ice = Pk_ice + Net_rain ! [inches]

            ! (1.1.2) Rain not sufficient to bring pack to isothermal...
            ELSEIF ( Net_rain.LT.pndz ) THEN
              ! The snowpack heat deficit decreases by the heat provided
              ! by rain and a new snowpack temperature is calculated
              ! 1.27 is the specific heat of ice (0.5 cal/(cm^3 degC))
              ! times the conversion of cm to inches (2.54 cm/in)
              Pk_def = Pk_def - (caln*Net_rain) ! [cal/(in cm^3)]
              Pk_temp = -Pk_def/(Pkwater_equiv*1.27)
              ! All the net rain freezes and becomes pack ice
              Pk_ice = Pk_ice + Net_rain

            ! (1.1.3) Rain in excess of amount required to bring pack
            !         to isothermal...
            ELSE
              ! Heat deficit and temperature of the snowpack go to 0
              Pk_def = 0.
              Pk_temp = 0.
              ! The portion of net rain that brings the snowpack to
              ! isothermal freezes
              Pk_ice = Pk_ice + pndz
              ! The rest of the net rain becomes free water in the
              ! snowpack
              ! Note that there cannot be previous Freeh2o because the
              ! snowpack had a heat deficit (all water was ice) before
              ! this condition was reached.
              Freeh2o = Net_rain - pndz
              ! Calculate the excess heat per area added by the portion
              ! of rain that does not bring the snowpack to isothermal
              ! (using specific heat of water)
              calpr = train*(Net_rain-pndz)*INCH2CM ! [cal/cm^2]
              ! Add the new heat to the snow pack 
              ! (the heat in this excess rain will melt some of the
              ! pack ice when the water cools to 0 degC)
              CALL calin(calpr, Pkwater_equiv, Pk_def, Pk_temp,
     +                   Pk_ice, Freeh2o, Snowcov_area, Snowmelt,
     +                   Pk_depth, Pss, Pst, Iasw,
     +                   Pk_den, Freeh2o_cap)
            ENDIF

          ! (1.2) Rain on snowpack that is isothermal
          !       at 0 degC (no heat deficit)...
          ELSE
            ! All net rain is added to free water in the snowpack
            Freeh2o = Freeh2o + Net_rain
            ! Calculate the heat per area added by the rain
            ! (using specific heat of water)
            calpr = train*Net_rain*INCH2CM ! [cal/cm^2]
            ! Add the new heat to the snow pack 
            ! (the heat in rain will melt some of the pack ice when
            ! the water cools to 0 degC)
            CALL calin(calpr, Pkwater_equiv, Pk_def, Pk_temp,
     +                 Pk_ice, Freeh2o, Snowcov_area, Snowmelt,
     +                 Pk_depth, Pss, Pst, Iasw,
     +                 Pk_den, Freeh2o_cap)
          ENDIF
        ENDIF

      ! (2) If there is net rain but no snowpack, set flag for a mix
      !     on no snowpack.
      ELSEIF ( Net_rain.GT.0. ) THEN
        ! Be careful with the code here.
        ! If this subroutine is called when there is an all-rain day
        ! on no existing snowpack (currently, it will not), 
        ! then the flag here will be set inappropriately.
        Pptmix_nopack = 1 ! [flag]
      ENDIF

      ! At this point, the subroutine has handled all conditions
      ! where there is net rain, so if there is net snow
      ! (doesn't matter if there is a pack or not)...
      IF ( Net_snow.GT.0.0 ) THEN
        ! add the new snow to the pack water equivalent, precip, and ice
        Pkwater_equiv = Pkwater_equiv + Net_snow
        Pk_precip = Pk_precip + Net_snow
        Pk_ice = Pk_ice + Net_snow

        ! The temperature of the new snow will determine its effect on
        ! snowpack heat deficit
        ! 2 options below (if-then, else)
        
        ! (1) if the new snow is at 0 degC...
        IF ( tsnow.GE.0. ) THEN
          ! incoming snow does not change the overall heat content of
          ! the snowpack.
          ! However, the temperature will change, because the total heat
          ! content of the snowpack will be "spread out" among
          ! more snow.  Calculate the snow pack temperature from the
          ! heat deficit, specific heat of snow, 
          ! and the new total snowpack water content
          Pk_temp = -Pk_def/(Pkwater_equiv*1.27) ! [degrees C]

        ! (2) if the new snow is colder than 0 degC...
        ELSE
          ! calculate the amount of heat the new snow will absorb if
          ! warming it to 0C (negative number).
          ! This is the negative of the heat deficit of the new snow.
          calps = tsnow*Net_snow*1.27 ! [cal/cm^2]

          ! The heat to warm the new snow can come from different
          ! sources depending on the state of the snowpack
          ! 2 options below (if-then, else)

          ! (2.1) if there is free water in the pack
          !       (at least some of it is going to freeze)...
          IF ( Freeh2o.GT.0. ) THEN
            CALL caloss(calps, Pkwater_equiv, Pk_def, Pk_temp,
     +                  Pk_ice, Freeh2o)

          ! (2.2) if there is no free water (snow pack has a
          !       heat deficit greater than or equal to 0)...
          ELSE
            ! heat deficit increases because snow is colder than
            ! pack (minus a negative number = plus)
            ! and calculate the new pack temperature
            Pk_def = Pk_def - calps ! [cal/cm^2]
            Pk_temp = -Pk_def/(Pkwater_equiv*1.27) ! [degrees C]
          ENDIF
        ENDIF
      ENDIF

      END SUBROUTINE ppt_to_pack

!***********************************************************************
!      Subroutine to compute change in snowpack when a net loss in
!        heat energy has occurred.
!***********************************************************************
      SUBROUTINE caloss(Cal, Pkwater_equiv, Pk_def, Pk_temp,
     +                  Pk_ice, Freeh2o)
      USE PRMS_SNOW, ONLY:NEARZERO
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Cal, Pkwater_equiv
      REAL, INTENT(INOUT) :: Pk_def, Pk_ice, Freeh2o
      REAL, INTENT(OUT) :: Pk_temp
! Local Variables
      REAL :: calnd, dif
!***********************************************************************

      ! Loss of heat is handled differently if there is liquid water in
      ! the snowpack or not
      ! 2 options below (if-then, else)

      ! (1) No free water exists in pack
      IF ( Freeh2o.LT.NEARZERO ) THEN
        ! heat deficit increases because snow is colder than pack
        ! (minus a negative number = plus)
        Pk_def = Pk_def - Cal ! [cal/cm^2]

      ! (2) Free water exists in pack
      ELSE
        ! calculate the total amount of heat per area that can be
        ! released by free water freezing
        calnd = Freeh2o*203.2 ! [cal/cm^2]
        ! determine the difference between heat in free water and the 
        ! heat that can be absorbed by new snow (without melting)
        ! remember that cal is a negative number
        dif = Cal + calnd ! [cal/cm^2]

        ! The effect of freezing water depends on whether all or only
        ! part of the liquid water freezes
        ! 2 options below (if-then, else)

        ! (1) All free water freezes
        IF ( dif.LT.NEARZERO ) THEN
          ! if all the water freezes, then the remaining heat
          ! that can be absorbed by new snow (that which is not
          ! provided by freezing free water) becomes the new pack
          ! heat deficit
          IF ( dif.LT.0. ) Pk_def = -dif ! [cal/cm^2]
          ! free pack water becomes ice
          Pk_ice = Pk_ice + Freeh2o ! [inches]
          Freeh2o = 0. ! [inches]

        ! (2) Only part of free water freezes
        ELSE
          ! the calories absorbed by the new snow freezes some
          ! of the free water
          ! (increase in ice, decrease in free water)
          Pk_ice = Pk_ice + (-Cal/203.2) ! [inches]
          Freeh2o = Freeh2o - (-Cal/203.2) ! [inches]
          RETURN
        ENDIF
      ENDIF

      ! if there is still a snowpack, calculate the new temperature
      IF ( Pkwater_equiv.GT.0. ) Pk_temp = -Pk_def/(Pkwater_equiv*1.27) 
                                                          ! [degrees C]

      END SUBROUTINE caloss

!***********************************************************************
!      Subroutine to compute changes in snowpack when a net gain in
!        heat energy has occurred.
!***********************************************************************
      SUBROUTINE calin(Cal, Pkwater_equiv, Pk_def, Pk_temp,
     +                 Pk_ice, Freeh2o, Snowcov_area, Snowmelt,
     +                 Pk_depth, Pss, Pst, Iasw,
     +                 Pk_den, Freeh2o_cap)
      USE PRMS_SNOW, ONLY:NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(OUT) :: Iasw
      REAL, INTENT(IN) :: Cal, Freeh2o_cap
      REAL, INTENT(INOUT) :: Pkwater_equiv, Freeh2o, Snowcov_area
      REAL, INTENT(INOUT) :: Pk_def, Pk_temp, Pk_ice, Pk_den, Snowmelt
      REAL, INTENT(OUT) :: Pk_depth, Pss, Pst
! Local Variables
      REAL :: dif, pmlt, apmlt, apk_ice, pwcap
!***********************************************************************

      ! Calculate the difference between the incoming calories and the 
      ! calories needed to bring the pack to isothermal 
      ! at 0 (heat deficit)
      dif = Cal - Pk_def ! [cal/cm^2]

      ! The way incoming heat is handled depends on whether there is
      ! not enough, just enough, or more than enough heat to overcome
      ! the heat deficit of the snowpack.
      ! 3 choices below (if-then, elseif, else)

      ! (1) Not enough heat to overcome heat deficit...
      IF ( dif.LT.-NEARZERO ) THEN
        ! Reduce the heat deficit by the amount of incoming calories
        ! and adjust to the new temperature based on new heat deficit
        Pk_def = Pk_def - Cal ! [cal/cm^2]
        Pk_temp = -Pk_def/(Pkwater_equiv*1.27) ! [degrees C]

      ! (2) Just enough heat to overcome heat deficit...
      ELSEIF ( dif.LT.NEARZERO ) THEN
        ! Set temperature and heat deficit to zero
        Pk_temp = 0. ! [degrees C]
        Pk_def = 0. ! [cal/cm^2]

      ! (3) More than enough heat to overcome heat deficit
      !     (melt ice)...
      ELSE
        ! calculate the potential amount of snowmelt from excess
        ! heat in rain it takes 203.2 calories / (in cm^2) to melt snow
        ! (latent heat of fusion)
        pmlt = dif/203.2 ! [inches]
        ! Actual snowmelt can only come from snow covered area, so to
        ! calculate the actual potential snowmelt, the potential
        ! snowmelt from snowcovered area must be re-normalized to
        ! HRU area (rather than snowcover area)
        ! In effect, the potential snowmelt per area is reduced by the
        ! fraction of the watershed that is actually covered by snow
        apmlt = pmlt*Snowcov_area ! [inches]
        ! Set the heat deficit and temperature of the remaining
        ! snowpack to 0
        Pk_def = 0. ! [cal/cm^2]
        Pk_temp = 0. ! [degrees C]
        ! The only pack ice that is melted is in the snow covered area,
        ! so the pack ice needs to be re-normalized to the snowcovered
        ! area (rather than HRU area)
        ! In effect, the pack ice per area is increased by the fraction
        ! of the watershed that is actually covered by snow
        apk_ice = Pk_ice/Snowcov_area ! [inches]

        ! If snow is melting, the heat is handled based on whether all
        ! or only part of the pack ice melts
        ! 2 options below (if-then, else)

        ! (3.1) Heat applied to snow covered area is sufficient
        !       to melt all the ice in that snow pack...
        IF ( pmlt.GT.apk_ice ) THEN
          ! All pack water equivalent becomes meltwater
          Snowmelt = Snowmelt + Pkwater_equiv ! [inches]
          Pkwater_equiv = 0. ! [inches]
          Iasw = 0 ! snow area does not change
          ! Set all snowpack states to 0
          Snowcov_area = 0. ! [fraction of area]
          Pk_def = 0. ! [cal / cm^2]
          Pk_temp = 0. ! [degreees C]
          Pk_ice = 0. ! [inches]
          Freeh2o = 0. ! [inches]
          Pk_depth = 0. ! [inches]
          Pss = 0. ! [inches]
          Pst = 0. ! [inches]
          Pk_den = 0. ! [fraction of depth]

        ! (3.2) Heat only melts part of the ice in the snow pack...
        ELSE
          ! Remove actual melt from frozen water and add melt to
          ! free water
          Pk_ice = Pk_ice - apmlt ! [inches]
          Freeh2o = Freeh2o + apmlt ! [inches]
          ! Calculate the capacity of the snowpack to hold free water
          ! according to its current level of frozen water
          pwcap = Freeh2o_cap*Pk_ice ! [inches]
          ! Calculate the amount of free water in excess of the
          ! capacity to hold free water
          dif = Freeh2o - pwcap ! [inches]
          ! If there is more free water than the snowpack can hold,
          ! then there is going to be melt...
          IF ( dif.GT.0. ) THEN
            ! snowmelt increases by the excess free water
            Snowmelt = Snowmelt + dif ! [inches]
            ! free water is at the current capacity
            Freeh2o = pwcap ! [inches]
            ! total packwater decreases by the excess and a new depth
            ! is calculated based on density
            Pkwater_equiv = Pkwater_equiv - dif ! [inches]
            Pk_depth = Pkwater_equiv/Pk_den ! [inches]
            ! reset the previous-snowpack-plus-new-snow to the
            ! current pack water equivalent
            Pss = Pkwater_equiv ! [inches]
          ENDIF
        ENDIF
      ENDIF

      END SUBROUTINE calin

!***********************************************************************
!      Subroutine to compute snowpack albedo
!***********************************************************************
      SUBROUTINE snalbedo(Newsnow, Iso, Lst, Snsv,
     +                    Prmx, Pptmix, Albset_rnm, Net_snow,
     +                    Albset_snm, Albset_rna, Albset_sna, Albedo,
     +                    Acum, Amlt, Int_alb, Salb, Slst)
      IMPLICIT NONE
      INTRINSIC INT
      INCLUDE 'fmodules.inc'
! Arguments
      INTEGER, INTENT(IN) :: Newsnow, Iso, Pptmix
      INTEGER, INTENT(INOUT) :: Int_alb, Lst
      REAL, INTENT(IN) :: Albset_rnm, Albset_snm, Albset_rna, Albset_sna
      REAL, INTENT(IN) :: Prmx, Net_snow, Acum(MAXALB), Amlt(MAXALB)
      REAL, INTENT(INOUT) :: Salb, Slst, Snsv
      REAL, INTENT(OUT) :: Albedo
! Local Variables
      INTEGER :: l
!***********************************************************************

      ! The albedo is always reset to a new initial (high) value when
      ! there is new snow above a threshold (parameter).  Albedo 
      ! is then a function of the number of days since the last new snow
      ! Intermediate conditions apply when there is new snow
      ! below the threshold to reset the albedo to its highest value.
      ! The curve for albedo change (decreasing) is different for the
      ! snow accumulation season and the snow melt season.
      ! The albedo first depends on if there is no new snow during the
      ! current time step, if there is new snow during accumulation 
      ! season, or if there is new snow during melt season.
      ! 3 options below (if-then, elseif, else)

      ! (1) There is no new snow
      IF ( Newsnow.EQ.0 ) THEN
        ! If no new snow, check if there was previous new snow that
        ! was not sufficient to reset the albedo (Lst=1)
        ! Lst can only be greater than 0 during melt season (see below)
        IF ( Lst.GT.0 ) THEN
          ! Slst is the number of days (float) since the last
          ! new snowfall
          ! Set the albedo curve back three days from the number
          ! of days since the previous snowfall
          ! (see Salb assignment below)
          ! (note that "shallow new snow" indicates new snow that
          ! is insufficient to completely reset the albedo curve)
          ! In effect, a shallow new snow sets the albedo curve back
          ! a few days, rather than resetting it entirely.
          Slst = Salb - 3. ! [days]
          ! Make sure the number of days since last new snow
          ! isn't less than 1
          IF ( Slst.LT.1. ) Slst = 1. ! [days]
          ! If not in melt season
          IF ( Iso.NE.2 ) THEN
            ! Note that this code is unreachable in its current state.
            ! This code is only run during melt season due to the
            ! fact that Lst can only be set to 1 in the melt season.
            ! Therefore, Iso is always going to be equal to 2.
            ! Make sure the maximum point on the albedo curve is 5
            ! In effect, if there is any new snow, the albedo can
            ! only get so low in accumulation season, even if the
            ! new snow is insufficient to reset albedo entirely
            IF ( Slst.GT.5. ) Slst = 5. ! [days]
          ENDIF
          ! Reset the shallow new snow flag and cumulative shallow
          ! snow variable (see below)
          Lst = 0 ! [flag]
          Snsv = 0. ! [inches]
        ENDIF

      ! (2) New snow during the melt season
      ELSEIF ( Iso.EQ.2 ) THEN
! RAPCOMMENT - CHANGED TO ISO FROM MSO
  
        ! If there is too much rain in a precipitation mix,
        ! albedo will not be reset
        ! New snow changes albedo only if the percent rain
        ! is less than the threshold above which albedo is not reset
        IF ( Prmx.LT.Albset_rnm ) THEN
        
          ! If the percent rain doesn't prevent the albedo from
          ! being reset, then how the albedo changes depends on
          ! whether the snow amount is above or below the threshold
          ! for resetting albedo
          ! 2 options below (if-then, else)
          
          ! (2.1) If there is enough new snow to reset the albedo
          IF ( Net_snow.GT.Albset_snm ) THEN
            ! Reset number of days since last new snow to 0
            Slst = 0. ! [days]
            Lst = 0 ! [flag]
            ! Reset the saved new snow to 0
            Snsv = 0. ! [inches]
            
          ! (2.2) If there is not enough new snow this time period
          ! to reset the albedo on its own
          ELSE
            ! Snsv tracks the amount of snow that has fallen as long
            ! as the total new snow is not
            ! enough to reset the albedo.
            Snsv = Snsv + Net_snow ! [inches]
            
            ! Even if the new snow during this time period is
            ! insufficient to reset the albedo, it may still reset the
            ! albedo if it adds enough to previous shallow snow
            ! accumulation.  The change in Albedo depends on if the
            ! total amount of accumulated shallow snow has become enough
            ! to reset the albedo or not.
            ! 2 options below (if-then, else)
            
            ! (2.2.1) If accumulated shallow snow is enough to reset
            !         the albedo
            IF ( Snsv.GT.Albset_snm ) THEN
              ! Reset the albedo states.
              Slst = 0. ! [days]
              Lst = 0 ! [flag]
              Snsv = 0. ! [inches]
              
            ! (2.2.2) If the accumulated shallow snow is not enough to
            !         reset the albedo curve
            ELSE
              ! Salb records the number of days since the last new snow
              ! that reset albedo
              IF ( Lst.EQ.0 ) Salb = Slst ! [days]
              ! Reset the number of days since new snow
              Slst = 0. ! [days]
              ! set the flag indicating that there is shallow new snow
              ! (i.e. not enough new snow to reset albedo)
              Lst = 1 ! [flag]
            ENDIF
          ENDIF
        ENDIF

      ! (3) New snow during the accumulation season
      ELSE
        
        ! The change in albedo depends on if the precipitation is a mix,
        ! if the rain is above a threshold,  or if the snow is above
        ! a threshold.
        ! 4 options below (if-then, elseif, elseif, else)
        
        ! (3.1) If it is not a mixed event...
        IF ( Pptmix.LE.0 ) THEN
          ! During the accumulation season, the threshold for resetting
          ! the albedo does not apply if there is a snow-only event.
          ! Therefore, no matter how little snow there is, it will
          ! always reset the albedo curve the the maximum, if it
          ! occurs during the accumulation season.
          ! reset the time since last snow to 0
          Slst = 0. ! [days]
          ! there is no new shallow snow
          Lst = 0 ! [flag]
          
        ! (3.2) If it is a mixed event and the percent rain is above
        !       the threshold above which albedo is not reset...
        ELSEIF ( Prmx.GE.Albset_rna ) THEN
          ! there is no new shallow snow
          Lst = 0 ! [flag]
          ! albedo continues to decrease on the curve
          
        ! (3.3) If it is a mixed event and there is enough new snow
        !       to reset albedo...
        ELSEIF ( Net_snow.GE.Albset_sna ) THEN
          ! reset the albedo
          Slst = 0. ! [days]
          ! there is no new shallow snow
          Lst = 0 ! [flag]

        ! (3.4) If it is a mixed event and the new snow was not
        !       enough to reset the albedo...
        ELSE
          ! set the albedo curve back 3 days (increasing the albedo)
          Slst = Slst - 3. ! [days]
          ! Make sure the number of days since last new snow is not
          ! less than 0
          IF ( Slst.LT.0. ) Slst = 0. ! [days]
          ! Make sure the number of days since last new snow is not
          ! greater than 5
          ! In effect, if there is any new snow, the albedo can
          ! only get so low in accumulation season, even if the
          ! new snow is insufficient to reset albedo entirely
          IF ( Slst.GT.5. ) Slst = 5. ! [days]
          Lst = 0 ! [flag]
        ENDIF
        Snsv = 0. ! [inches]
      ENDIF
      ! At this point, the subroutine knows where on the curve the
      ! albedo should be based on current conditions and the
      ! new snow (determined by value of Slst variable)

      ! Get the integer value for days (or effective days)
      ! since last snowfall
      l = INT(Slst+0.5) ! [days]

      ! Increment the state variable for days since the
      ! last snowfall
      Slst = Slst + 1. ! [days]

      !******Compute albedo
      ! Albedo will only be different from the max (default value)
      ! if it has been more than 0 days since the last new snow
      ! capable of resetting the albedo.  If albedo is at the
      ! maximum, the maximum is different for accumulation and
      ! melt season.
      ! 3 options below (if-then, elseif, else)

      ! (1) It has been more than 0 days since the last new snow
      IF ( l.GT.0 ) THEN

        ! Albedo depends on whether it is currently on the
        ! accumulation season curve or on the melt season curve.
        ! 3 options below (if-then, elseif, else)

        ! (1.1) Currently using the melt season curve
        !       (Old snow - Spring melt period)...
        IF ( Int_alb.EQ.2 ) THEN
          ! Don't go past the last possible albedo value
          IF ( l.GT.MAXALB ) l = MAXALB ! [days]
          ! Get the albedo number from the melt season curve
          Albedo = Amlt(l) ! [fraction of radiation]

        ! (1.2) Currently using the accumulation season curve
        !       (Old snow - Winter accumulation period)...
        ! and not past the maximum curve index
        ELSEIF ( l.LE.MAXALB ) THEN
          ! Get the albedo number from the accumulation season curve
          Albedo = Acum(l) ! [fraction of radiation]
 
        ! (1.3) Currently using the accumulation season curve and
        !       past the maximum curve index...
        ELSE
          ! start using the the MELT season curve at 12 days
          ! previous to the current number of days since the last
          ! new snow
          l = l - 12 ! [days]
          ! keep using the melt season curve until its minimum 
          ! value (maximum index) is reached or until there is new snow 
          IF ( l.GT.MAXALB ) l = MAXALB ! [days]
          ! get the albedo value from the melt season curve
          Albedo = Amlt(l) ! [fraction of radiation]
        ENDIF

      ! (2) New snow has reset the albedo and it is melt season
      ELSEIF ( Iso.EQ.2 ) THEN
! RAPCOMMENT - CHANGED TO ISO FROM MSO      
        ! Set albedo to initial value during melt season
        Albedo = .81 ! [fraction of radiation]
        ! Int_alb is a flag to indicate use of the melt season curve (2)
        ! or accumulation season curve (1)
        ! Set flag to indicate melt season curve
        Int_alb = 2 ! [flag]

      ! (3) New snow has reset the albedo and it is accumulation season
      ELSE
        ! Set albedo to initial value during accumulation season
        Albedo = .91 ! [fraction of radiation]
        ! Set flag to indicate accumulation season curve
        Int_alb = 1 ! [flag]
      ENDIF

      END SUBROUTINE snalbedo

!***********************************************************************
!      Subroutine to compute energy balance of snowpack
!        1st call is for night period, 2nd call for day period
!***********************************************************************
      SUBROUTINE snowbal(Niteda, Tstorm_mo, Iasw,
     +                   Temp, Esv, Basin_ppt, Trd, Emis_noppt,
     +                   Covden_win, Cec, Pkwater_equiv, Pk_def,
     +                   Pk_temp, Pk_ice, Freeh2o, Freeh2o_cap,
     +                   Snowcov_area, Snowmelt, Pk_depth, Pss,
     +                   Pst, Pk_den, Cst, Cal, Sw)
      USE PRMS_SNOW, ONLY:NEARZERO
      IMPLICIT NONE
      EXTERNAL calin, caloss
! Arguments
      INTEGER, INTENT(IN) :: Niteda, Tstorm_mo
      INTEGER, INTENT(INOUT) :: Iasw
      REAL, INTENT(IN) :: Temp, Esv, Trd, Cec, Cst, Covden_win
      REAL, INTENT(IN) :: Emis_noppt, Basin_ppt, Freeh2o_cap, Sw
      REAL, INTENT(OUT) :: Pst, Pss, Cal
      REAL, INTENT(INOUT) :: Pk_den, Pk_def, Pk_temp, Pk_ice, Pk_depth
      REAL, INTENT(INOUT) :: Pkwater_equiv, Freeh2o
      REAL, INTENT(INOUT) :: Snowcov_area, Snowmelt
! Local Variables
      REAL :: air, ts, emis, sno, sky, can
      REAL :: cecsub, qcond, pk_defsub, pkt, pks
!***********************************************************************
      ! Calculate the potential long wave energy from air based on
      ! temperature (assuming perfect black-body emission)
      air = 0.585E-7*((Temp+273.16)**4.0) ! [cal/cm^2] or [Langleys]
      ! set emissivity, which is the fraction of perfect black-body
      ! emission that is actually applied
      emis = Esv ! [fraction of radiation]

      ! The snowpack surface temperature and long-wave radiation
      ! FROM the snowpack depend on the air temperature (effectively,
      ! snowpack temperature cannot be larger than 0 degC)
      ! 2 options below (if-then, else)

      ! (1) If the temperature is below freezing, surface snow
      !     temperature and long wave energy are determined
      !     by temperature...
      IF ( Temp.LT.0. ) THEN
        ts = Temp ! [degrees C]
        sno = air ! [cal/cm^2] or [Langleys]

      ! (2) If the temperature is at or above freezing, snow
      !     temperature and long wave energy are set to values 
      !     corresponding to a temperature of 0 degC...
      ELSE
        ts = 0. ! [degrees C]
        sno = 325.7 ! [cal/cm^2] or [Langleys]
      ENDIF

      ! If precipitation over the time period was due to
      ! convective thunderstorms, then the emissivity should be reset
      IF ( Basin_ppt.GT.0. ) THEN
        IF ( Tstorm_mo.EQ.1 ) THEN

          ! The emissivity of air depends on if it is day or night
          ! and the fraction of observed short wave radiation to
          ! potential short wave radiation is used as a surrogate
          ! to the duration of the convective storms
          ! 2 options below (if-then, else)

          ! (1) Night
          IF ( Niteda.EQ.1 ) THEN
            ! set the default emissivity
            emis = .85 ! [fraction of radiation]
            ! if observed radiation is greater than 1/3 potential
            ! radiation through the time period, then the emissivity
            ! is set to the "no precipitation" value
            IF ( Trd.GT..33 ) emis = Emis_noppt ![fraction of radiation]

          ! (2) Day
          ELSE
            ! if observed radiation is greater than 1/3 potential
            ! radiation but less than 1/2, then the emissivity is
            ! interpolated between 1.0 and 0.85
            ! if observed radiation is greater than 1/2 potential
            ! radiation, then the emissivity is interpolated between
            ! 0.85 and 0.75
            IF ( Trd.GT..33 ) emis = 1.29 - (.882*Trd)
                                              ! [fraction of radiation]
            IF ( Trd.GE..5 ) emis = .95 - (.2*Trd)
                                              ! [fraction of radiation]
          ENDIF
        ENDIF
      ENDIF

      ! Calculate the net incoming long wave radiation coming from the
      ! sky or canopy in the uncovered or covered portions of the
      ! snowpack, respectively.
      ! Note that the canopy is assumed to be a perfect blackbody
      ! (emissivity = 1) and the air has emissivity as determined
      ! from previous calculations
      sky = (1.-Covden_win)*((emis*air)-sno) ! [cal/cm^2] or [Langleys]
      can = Covden_win*(air-sno) ! [cal/cm^2] or [Langleys]
!RAPCOMMENT  - CHECK THE INTERECEPT MODULE FOR CHANGE.  What if the land
! cover is grass? Is this automatically covered by covden_win being zero
! if the cover type is grass?   

      ! If air temperature is above 0 degC then set the energy from
      ! condensation and convection, otherwise there is
      ! no energy from convection or condensation
      cecsub = 0. ! [cal/cm^2] or [Langleys]
      IF ( Temp.GT.0. ) THEN
        IF ( Basin_ppt.GT.0. ) cecsub = Cec*Temp ! [cal/cm^2] 
                                                 ! or [Langleys]
      ENDIF

      ! Total energy potentially available from atmosphere: longwave,
      ! shortwave, and condensation/convection
      Cal = sky + can + cecsub + Sw ! [cal/cm^2] or [Langleys]

      ! If the surface temperature of the snow is 0 degC, and there
      ! is net incoming energy, then energy conduction has to be from
      ! the surface into the snowpack.
      ! Therefore, the energy from the atmosphere is applied to the
      ! snowpack and subroutine terminates
      IF ( ts.GE.0. ) THEN
        IF ( Cal.GT.0. ) THEN
          CALL calin(Cal, Pkwater_equiv, Pk_def, Pk_temp,
     +               Pk_ice, Freeh2o, Snowcov_area, Snowmelt,
     +               Pk_depth, Pss, Pst, Iasw,
     +               Pk_den, Freeh2o_cap)
          RETURN
        ENDIF
      ENDIF

      ! If the program gets to this point, then either the surface
      ! temperature is less than 0 degC, or the total energy from the
      ! atmosphere is not providing energy to the snowpack
      
      ! Because the temperature of the surface of the snowpack is
      ! assumed to be  controlled by air temperature, there is a
      ! potential heat flux due to conduction between the deeper
      ! snowpack and its surface.
      ! Calculate conductive heat flux as a function of the
      ! temperature gradient then set new snowpack conditions
      ! depending on the direction of heat flow
      qcond = Cst*(ts-Pk_temp) ! [cal/cm^2] or [Langleys]
!RAPCOMMENT - The original equation in the paper implies that the
! this equation should be relative to the temperature gradient
! in degF, not degC (Anderson 1968).  Which is correct?

      ! The energy flow depends on the direction of conduction and the
      ! temperature of the surface of the snowpack.  The total energy
      ! from the atmosphere can only penetrate into the snow pack if
      ! the temperature gradient allows conduction from the surface
      ! into the snowpack.
      ! 4 options below (if-then, elseif, elseif, else)

      ! (1) Heat is conducted from the snowpack to the surface
      !     (atmospheric energy is NOT applied to snowpack)...
      IF ( qcond.LT.-NEARZERO ) THEN
        ! If the temperature of the snowpack is below 0 degC,
        ! add to the heat deficit.  Otherwise, remove heat
        ! from the 0 degC isothermal snow pack.
        IF ( Pk_temp.LT.0. ) THEN
          ! increase the heat deficit (minus a negative)
          ! and adjust temperature
          Pk_def = Pk_def - qcond ! [cal/cm^2] or [Langleys]
          Pk_temp = -Pk_def/(Pkwater_equiv*1.27) ! [degrees C]
        ELSE
          ! remove heat from the snowpack
          CALL caloss(qcond, Pkwater_equiv, Pk_def, Pk_temp,
     +                Pk_ice, Freeh2o)
        ENDIF
      ! Even though Cal is not applied to the snowpack under this
      ! condition, it maintains its value and the referencing code
      ! uses it to calculate the total energy balance of the snowpack.
      ! Right, now, Cal isn't used for anything outside this subroutine,
      ! but care should be taken if it is.

      ! (2)  There is no heat conduction, qcond = 0.0
      ELSEIF ( qcond.LT.NEARZERO ) THEN
        
        ! if the pack temperature is isothermal at 0 degC, then apply
        ! any incoming radiation, condensation (latent heat),
        ! and convection heat to the snowpack
        IF ( Pk_temp.GE.0. ) THEN
          ! It does not appear that the interior of the following if
          ! statement is reachable in its current form, because if these
          ! conditions are true, then the code for surface temperature=0
          ! and cal=positive number would have run and the subroutine
          ! will have terminated
          IF ( Cal.GT.0. ) CALL calin(Cal, Pkwater_equiv,Pk_def,Pk_temp,
     +                                Pk_ice, Freeh2o, Snowcov_area,
     +                                Snowmelt, Pk_depth, Pss, Pst,Iasw,
     +                                Pk_den, Freeh2o_cap)
        ENDIF

      ! (3) conduction is from the surface to the snowpack and the
      !     surface temperature is 0 degrees C...
      ELSEIF ( ts.GE.0. ) THEN
        ! note that Cal must be <= 0 for this condition to apply.
        ! Otherwise, the program wouldn't have gotten to this point.

        ! determine if the conductive heat is enough to overcome the
        ! current heat deficit
        pk_defsub = Pk_def - qcond
        IF ( pk_defsub.LT.0. ) THEN
          ! deficit is overcome and snowpack becomes
          ! isothermal at 0 degC
          Pk_def = 0. ! [cal/cm^2] or [Langleys]
          Pk_temp = 0. ! [degrees C]
        ELSE
          ! deficit is decreased by conducted heat and temperature
          ! is recalculated
          Pk_def = pk_defsub ! [cal/cm^2] or [Langleys]
          Pk_temp = -pk_defsub/(Pkwater_equiv*1.27) ! [degrees C]
        ENDIF

      ! (4) conduction is from the surface to the snowpack and the
      !     surface temperature is less than 0 degrees C...
      ELSE
        ! calculate the pack deficit if the snowpack was all at the
        ! surface temperature, then calculate how many calories to
        ! shift the pack to that deficit (pks will be a positive
        ! number because the conduction direction is from the surface
        ! into the snowpack)
        pkt = -ts*Pkwater_equiv*1.27 ! [cal/cm^2] or [Langleys]
        pks = Pk_def - pkt ! [cal/cm^2] or [Langleys]
        ! determine if the conducted heat is enough to shift the
        ! pack to the deficit relative to the surface temperature
        pk_defsub = pks - qcond ! [cal/cm^2] or [Langleys]

        ! The effect of incoming conducted heat depends on whether
        ! it is enough to bring the snowpack to the same temperature
        ! as the surface or not
        ! 2 options below (if-then, else)

        ! (4.1) There is enough conducted heat to bring the deep
        !       snowpack to the surface temperature...
        IF ( pk_defsub.LT.0. ) THEN
          ! there is enough conduction to change to the new pack deficit
          Pk_def = pkt ! [cal/cm^2] or [Langleys]
          Pk_temp = ts ! [degrees C]

        ! (4.2) There is not enough conducted heat to bring the deep
        !       snowpack to the surface temperature...
        ELSE
          ! the pack deficit doesn't make it all the way to the surface
          ! deficit, but is decreased relative to the conducted heat
          ! note that the next statement is equivalent to
          ! Pk_def = Pk_def - qcond
          Pk_def = pk_defsub + pkt ! [cal/cm^2] or [Langleys]
          Pk_temp = -Pk_def/(Pkwater_equiv*1.27) ! [degrees C]
        ENDIF
      ENDIF

      END SUBROUTINE snowbal

!***********************************************************************
!      Subroutine to compute evaporation from snowpack
!***********************************************************************
      SUBROUTINE snowevap(Cov_type, Potet_sublim, Potet, Snowcov_area,
     +                    Hru_intcpevap, Snow_evap, Pkwater_equiv,
     +                    Pk_ice, Pk_def, Freeh2o, Pk_temp)
      USE PRMS_SNOW, ONLY:NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Cov_type
      REAL, INTENT(IN) :: Potet_sublim, Potet
      REAL, INTENT(IN) :: Snowcov_area, Hru_intcpevap
      REAL, INTENT(INOUT) :: Pkwater_equiv, Pk_ice, Pk_def, Pk_temp
      REAL, INTENT(OUT) :: Snow_evap, Freeh2o
! Local Variables
      REAL :: ez, cal
!***********************************************************************

      ! Some of the calculated evaporation can come from interception
      ! rather than the snowpack.  Therefore, the effects of
      ! interception must be evaluated.
      ! 2 options below (if-then, else)
      
      ! (1) There is interception by shrubs or trees...
      IF ( Cov_type.GT.1 ) THEN
        ! the amount of evaporation affecting the snowpack is the
        ! total evaporation potential minus the evaporation from
        ! the interception storage
        ez = (Potet_sublim*Potet*Snowcov_area) - Hru_intcpevap
                                                            ! [inches]
        
      ! (2) There is no interception by shrubs or trees...
      ELSE
        ! There is no interception storage so all the potential
        ! evaporation affects the snowpack
        ez = Potet_sublim*Potet*Snowcov_area ! [inches]
      ENDIF

      ! The effects of evaporation depend on whether there is any
      ! potential for evaporation, and if the potential evapotation
      ! is enough to completely deplete the snow pack or not
      ! 3 options below (if-then, elseif, else)
      
      ! (1) There is no potential for evaporation...
      IF ( ez.LT.NEARZERO ) THEN
        Snow_evap = 0. ! [inches]

      ! (2) Enough potential evaporation to entirely deplete
      !     the snowpack...
      ELSEIF ( ez.GE.Pkwater_equiv ) THEN
        ! Set the evaporation to the pack water equivalent and set
        ! all snowpack variables to no-snowpack values
        Snow_evap = Pkwater_equiv ! [inches]
        Pkwater_equiv = 0. ! [inches]
        Pk_ice = 0. ! [inches]
        Pk_def = 0. ! [cal/cm^2]
        Freeh2o = 0. ! [inches]
        Pk_temp = 0. ! [degrees C]

      ! (3) Potential evaporation only partially depletes snowpack
      ELSE
        ! Evaporation depletes the amount of ice in the snowpack
        ! (sublimation)
        Pk_ice = Pk_ice - ez
        
        ! Change the pack conditions according to whether there is
        ! any ice left in the snowpack
        IF (Pk_ice.LT.0) THEN
!RAPCOMMENT - CHANGED TO CHECK FOR NEGATIVE PACK ICE
          ! If all pack ice is removed, then there cannot be a
          ! heat deficit
          Pk_ice = 0.
          Pk_def = 0.
          Pk_temp = 0.
        ELSE
          ! Calculate the amount of heat deficit that is removed
          ! by the sublimating ice
          ! Note that this only changes the heat deficit if the
          ! pack temperature is less than 0degC
          cal = Pk_temp*ez*1.27
          Pk_def = Pk_def + cal
        ENDIF
        ! Remove the evaporated water from the pack water equivalent
        Pkwater_equiv = Pkwater_equiv - ez
        Snow_evap = ez
      ENDIF

      END SUBROUTINE snowevap

!***********************************************************************
!      Subroutine to compute snow-covered area
!***********************************************************************
      SUBROUTINE snowcov(Iasw, Newsnow,
     +                   Snowcov_area, Snarea_curve, Pkwater_equiv, Pst,
     +                   Snarea_thresh, Net_snow, Scrv, Pksv,
     +                   Snowcov_areasv)
      IMPLICIT NONE
      INTRINSIC FLOAT, INT
! Arguments
      INTEGER, INTENT(IN) :: Newsnow
      INTEGER, INTENT(INOUT) :: Iasw
      REAL, INTENT(IN) :: Pkwater_equiv, Snarea_thresh, Net_snow
      REAL, INTENT(IN) :: Snarea_curve(11)
      REAL, INTENT(OUT) :: Snowcov_area
      REAL, INTENT(INOUT) :: Pst, Scrv, Pksv, Snowcov_areasv
! Local Variables
      INTEGER :: jdx, idx
      REAL :: ai, pcty, difx, frac, af, dify
!***********************************************************************
      ! Reset snowcover area to the maximum
      Snowcov_area = Snarea_curve(11) ! [fraction of area]
      
      ! Track the maximum pack water equivalent for the current
      ! snow pack
      IF ( Pkwater_equiv.GT.Pst ) Pst = Pkwater_equiv ! [inches]
      
      ! Set ai to the maximum packwater equivalent, but no higher than
      ! the threshold for complete snow cover
      ai = Pst ! [inches]
      IF ( ai.GE.Snarea_thresh ) ai = Snarea_thresh ! [inches]
      
      ! There are 3 potential conditions for the snow area curve:
      ! A. snow is accumulating and the pack is currently at its
      !    maximum level
      ! B. snow is depleting and the area is determined by the
      !    snow area curve
      ! C. new snow has occured on a depleting pack, temporarily
      !    resetting to 100% cover.
      ! For case (C), the snow covered area is linearly interpolated
      ! between 100% and the snow covered area before the new snow.
      ! In general, 1/4 of the new snow has to melt before the snow
      ! covered area goes below 100%, and then the remaining 3/4 has
      ! to melt to return to the previous snow covered area.

      ! First, the code decides whether snow is accumulating (A)
      ! or not (B/C).
      ! 2 options below (if-then, else)

      ! (1) The pack water equivalent is at the maximum
      IF ( Pkwater_equiv.GE.ai ) THEN
        ! Stay on the snow area curve (it will be at the maximum
        ! because the pack water equivalent is equal to ai
        ! and it can't be higher)
        Iasw = 0

      ! (2) The pack water equivalent is less than the maximum
      ELSE

        ! If the snowpack isn't accumulating to a new maximum,
        ! it is either on the curve (condition B above) or being
        ! interpolated between the previous place on the curve and
        ! 100% (condition C above)
        ! 2 options below (if-then, elseif)
        
        ! (2.1) There was new snow...
        IF ( Newsnow.NE.0 ) THEN
        
          ! New snow will always reset the snow cover to 100%.
          ! However, different states changes depending  on whether
          ! the previous snow area condition was on the curve or
          ! being interpolated between the curve and 100%
          ! 2 options below (if-then, else)
          
          ! (2.1.1) The snow area is being interpolated between 100%
          !         and a previous location on the curve...
          IF ( Iasw.GT.0 ) THEN
            ! The location on the interpolated line is based on how
            ! much of the new snow has melted.  Because the first 1/4
            ! of the new snow doesn't matter, it has to keep track of
            ! the current snow pack plus 3/4 of the new snow.
            Scrv = Scrv + (.75*Net_snow) ! [inches]
            ! Scrv = Pkwater_equiv - (.25*Net_snow) ! [inches]
!RAPCOMMENT - CHANGED TO INCREMENT THE SCRV VALUE IF ALREADY
!             INTERPOLATING BETWEEN CURVE AND 100%

          ! (2.1.2) The current snow area is on the curve...
          ELSE
            ! If switching from the snow area curve to interpolation
            ! between the curve and 100%, the current state of the snow
            ! pack has to be saved so that the interpolation can
            ! continue until back to the original conditions.
            ! First, set the flag to indicate interpolation between 100%
            ! and the previous area should be done
            Iasw = 1 ! [flag]
            ! Save the current snow covered area
            ! (before the new net snow)
            Snowcov_areasv = Snowcov_area ! [inches]
            ! Save the current pack water equivalent
            ! (before the new net snow)
            Pksv = Pkwater_equiv - Net_snow ! [inches]
            ! The location on the interpolated line is based on how much
            ! of the new snow has melted.  Because the first 1/4
            ! of the new snow doesn't matter, it has to keep track of
            ! the current snow pack plus 3/4 of the new snow.
            Scrv = Pkwater_equiv - (.25*Net_snow) ! [inches]
          ENDIF
          ! The subroutine terminates here because the snow covered area
          ! always starts at 100% if there is any new snow (no need to
          ! reset it from the maximum value set at the beginning of the
          ! subroutine).
          RETURN
          
        ! (2.2) There was no new snow, but the snow covered area is
        !       currently being interpolated between 100%
        !       from a previous new snow and the snow covered area
        !       before that previous new snow...
        ELSEIF ( Iasw.NE.0 ) THEN
          ! If the first 1/4 of the previous new snow has not melted,
          ! yet, then the snow covered area is still
          ! 100% and the subroutine can terminate.
          IF ( Pkwater_equiv.GT.Scrv ) RETURN
          
          ! At this point, the program is almost sure it is
          ! interpolating between the previous snow covered area and
          ! 100%, but it is possible that enough snow has melted to
          ! return to the snow covered area curve instead.
          ! 2 options below (if-then, else)
          
          ! (2.2.1) The snow pack still has a larger water equivalent
          !         than before the previous new snow.  I.e., new snow
          !         has not melted back to original area...
          IF ( Pkwater_equiv.GE.Pksv ) THEN
            ! Do the interpolation between 100% and the snow covered
            ! area before the previous new snow.
            
            ! Calculate the difference between the maximum snow
            ! covered area (remember that Snowcov_area is always
            ! set to the maximum value at this point) and the snow
            ! covered area before the last new snow.
            difx = Snowcov_area - Snowcov_areasv
            ! Calculate the difference between the water equivalent
            ! before the last new snow and the previous water
            ! equivalent plus 3/4 of the last new snow.
            ! In effect, get the value of 3/4 of the previous
            ! new snow.
            dify = Scrv - Pksv ! [inches]                       !gl1098
            
            ! If 3/4 of the previous new snow is significantly
            ! different from zero, then calculate the ratio of the
            ! unmelted amount of previous new snow in the snow pack
            ! to the value of 3/4 of previous new snow.
            ! In effect, this is the fraction of the previous new snow
            ! that determines the current interpolation
            ! of snow covered area.
            pcty = 0.0 ! [fraction]                             !gl1098
            IF ( dify.GT.0.00001 ) pcty = (Pkwater_equiv-Pksv)/dify
                                                           ! [fraction]
            ! Linearly interpolate the new snow covered area. 
            Snowcov_area = Snowcov_areasv + (pcty*difx)
                                                   ! [fraction of area]
            ! Terminate the subroutine     
            RETURN
          
          ! (2.2.2) The snow pack has returned to the snow water
          ! equivalent before the previous new snow. I.e. back to
          ! original area before new snow.
          ELSE
            ! Reset the flag to use the snow area curve
            Iasw = 0 ! [flag]
          ENDIF

        ENDIF

        ! If this subroutine is still running at this point, then the
        ! program knows that the snow covered area needs to be
        ! adjusted according to the snow covered area curve.  So at
        ! this point it must interpolate between points on the snow
        ! covered area curve (not the same as interpolating between
        ! 100% and the previous spot on the snow area depletion curve).

        ! Interpolate along snow area depletion curve

        ! calculate the ratio of the current packwater equivalent to 
        ! the maximum packwater equivalent for the given snowpack
        frac = Pkwater_equiv/ai ! [fraction]
        ! get the indeces (as integers) of the depletion curve that
        ! bracket the given frac (next highest and next lowest)
        idx = INT(10.*(frac+.2)) ! [index]
        jdx = idx - 1 ! [index]
        ! calculate the fraction of the distance (from the next lowest)
        ! the given frac is between the next highest and lowest
        ! curve values
        af = FLOAT(jdx-1)
        dify = (frac*10.) - af ! [fraction]
        ! calculate the difference in snow covered area represented
        ! by next highest and lowest curve values
        difx = Snarea_curve(idx) - Snarea_curve(jdx)
        ! linearly interpolate a snow covered area between those 
        ! represented by the next highest and lowest curve values
        Snowcov_area = Snarea_curve(jdx) + (dify*difx)

      ENDIF

      END SUBROUTINE snowcov

!***********************************************************************
! Convert Fahrenheit to Celsius
!***********************************************************************
      REAL FUNCTION f_to_c_sno(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
      REAL, PARAMETER :: FIVE_NINTHS = 5.0/9.0
!***********************************************************************
      f_to_c_sno = (Temp-32.0)*FIVE_NINTHS
      END FUNCTION f_to_c_sno

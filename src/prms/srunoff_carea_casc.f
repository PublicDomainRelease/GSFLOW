!***********************************************************************
!     Computes surface runoff and infiltration
!     version: 2.2 added cascading flow for infiltration and runoff
!***********************************************************************
      MODULE PRMS_SRUNOFF_CAREA_CASC
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 1.0E-15
      INTEGER :: Nhru, Ncascade, Nsegment, Nsegmentp1
      REAL :: Cfs_conv
      REAL, ALLOCATABLE :: Carea_dif(:)
!   Declared Variables
      REAL :: Basin_sroff, Basin_infil, Basin_imperv_evap
      REAL :: Basin_imperv_stor, Basin_sroff_down, Basin_hortonian_lakes
      REAL :: Basin_sroff_upslope, Basin_hortonian, Strm_farfield
      REAL :: Basin_sroff_farflow
      REAL, ALLOCATABLE :: Infil(:), Sroff(:), Hortonian_lakes(:)
      REAL, ALLOCATABLE :: Imperv_stor(:), Imperv_evap(:)
      REAL, ALLOCATABLE :: Upslope_hortonian(:), Strm_seg_in(:)
      REAL, ALLOCATABLE :: Hortonian_flow(:)
      REAL, ALLOCATABLE :: Hru_impervstor(:), Hru_impervevap(:)
!   Declared Variables from other modules - obs
      INTEGER :: Route_on
!   Declared Variables from other modules - intcp
      REAL, ALLOCATABLE :: Net_rain(:), Net_snow(:), Net_ppt(:)
      REAL, ALLOCATABLE :: Hru_intcpevap(:)
!   Declared Variables from other modules - smbal
      REAL, ALLOCATABLE :: Soil_moist(:), Soil_rechr(:)
!   Declared Variables from other modules - potet
      REAL, ALLOCATABLE :: Potet(:)
!   Declared Variables from other modules - snow
      INTEGER, ALLOCATABLE :: Pptmix_nopack(:)
      REAL, ALLOCATABLE :: Snow_evap(:), Snowmelt(:), Pkwater_equiv(:)
      REAL, ALLOCATABLE :: Snowcov_area(:), Pkwater_ante(:)
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug, Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:), Ncascade_hru(:)
      REAL :: Basin_area_inv
      REAL, ALLOCATABLE :: Hru_perv(:), Hru_imperv(:)
      REAL, ALLOCATABLE :: Hru_percent_perv(:)
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Hru_type(:)
      REAL, ALLOCATABLE :: Snowinfil_max(:), Soil_moist_max(:)
      REAL, ALLOCATABLE :: Imperv_stor_max(:), Carea_max(:), Hru_area(:)
      REAL, ALLOCATABLE :: Carea_min(:), Soil_rechr_max(:)
      REAL, ALLOCATABLE :: Hru_percent_imperv(:)
      END MODULE PRMS_SRUNOFF_CAREA_CASC

!***********************************************************************
!     Main srunoff_carea routine
!***********************************************************************
      INTEGER FUNCTION srunoff_carea_casc(Arg)
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srocascdecl, srocascinit, srocascrun
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
!***********************************************************************
      srunoff_carea_casc = 0

      IF ( Arg.EQ.'run' ) THEN
        srunoff_carea_casc = srocascrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        srunoff_carea_casc = srocascdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        srunoff_carea_casc = srocascinit()
      ENDIF

      END FUNCTION srunoff_carea_casc

!***********************************************************************
!     srocascdecl - set up parameters for surface runoff computations
!   Declared Parameters
!     carea_min, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max, soil_rechr_max, hru_percent_imperv
!***********************************************************************
      INTEGER FUNCTION srocascdecl()
      USE PRMS_SRUNOFF_CAREA_CASC
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      srocascdecl = 1

      IF ( declmodule(
     +'$Id: srunoff_carea_casc.f 3923 2008-03-05 20:08:22Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      Ncascade = getdim('ncascade')
      IF ( Ncascade.EQ.-1 ) RETURN

      Nsegment = getdim('nsegment')
      IF ( Nsegment.EQ.-1 ) RETURN
      Nsegmentp1 = Nsegment + 1

! srunoff variables
      ALLOCATE (Infil(Nhru))
      IF ( declvar('srunoff', 'infil', 'nhru', Nhru, 'real',
     +     'Amount of water infiltrating the soil on each HRU',
     +     'inches',
     +     Infil).NE.0 ) RETURN

      ALLOCATE (Sroff(Nhru))
      IF ( declvar('srunoff', 'sroff', 'nhru', Nhru, 'real',
     +     'Surface runoff to streams for each HRU',
     +     'inches',
     +     Sroff).NE.0 ) RETURN

      ALLOCATE (Hortonian_lakes(Nhru))
      IF ( declvar('srunoff', 'hortonian_lakes', 'nhru', Nhru, 'real',
     +     'Surface runoff to lakes for each HRU',
     +     'inches',
     +     Hortonian_lakes).NE.0 ) RETURN

      ALLOCATE (Imperv_stor(Nhru))
      IF ( declvar('srunoff', 'imperv_stor', 'nhru', Nhru, 'real',
     +     'Current storage on impervious area for each HRU',
     +     'inches',
     +     Imperv_stor).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_sroff', 'one', 1, 'real',
     +     'Basin area-weighted average of surface runoff',
     +     'inches',
     +     Basin_sroff).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_hortonian_lakes', 'one', 1, 'real',
     +     'Basin area-weighted average of Hortonian surface runoff'//
     +     ' to lakes',
     +     'inches',
     +     Basin_hortonian_lakes).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_infil', 'one', 1, 'real',
     +     'Basin area-weighted average for infiltration',
     +     'inches',
     +     Basin_infil).NE.0 ) RETURN

      ALLOCATE (Imperv_evap(Nhru))
      IF ( declvar('srunoff', 'imperv_evap', 'nhru', Nhru, 'real',
     +     'Evaporation from impervious area',
     +     'inches',
     +     Imperv_evap).NE.0 ) RETURN

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

! cascading variables and parameters
      ALLOCATE (Hortonian_flow(Nhru))
      IF ( declvar('srunoff', 'hortonian_flow', 'nhru', Nhru, 'real',
     +     'Hortonian surface runoff reaching stream network',
     +     'inches',
     +     Hortonian_flow).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_hortonian', 'one', 1, 'real',
     +     'Basin weighted average Hortonian runoff',
     +     'inches',
     +     Basin_Hortonian).NE.0 ) RETURN

      ALLOCATE (Upslope_hortonian(Nhru))
      IF ( declvar('srunoff', 'upslope_hortonian', 'nhru', Nhru,
     +     'real',
     +     'Hortonian surface runoff received from HRUs upslope',
     +     'inches',
     +     Upslope_hortonian).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_sroff_down', 'one', 1, 'real',
     +     'Basin area-weighted average of cascading surface runoff',
     +     'inches',
     +     Basin_sroff_down).NE.0 ) RETURN

      ALLOCATE (Strm_seg_in(Nsegment))
      IF ( declvar('srunoff', 'strm_seg_in', 'nsegment', Nsegment,
     +     'real',
     +     'Flow in stream segments as a result of cascading flow',
     +     'cfs',
     +     Strm_seg_in).NE.0 ) RETURN

      IF ( declvar('srunoff', 'strm_farfield', 'one', 1, 'real',
     +     'Flow out of basin as far-field flow',
     +     'cfs',
     +     Strm_farfield).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_sroff_upslope', 'one', 1, 'real',
     +     'Basin area-weighted average of cascading surface runoff'//
     +     ' received from HRUs up slope',
     +     'inches',
     +     Basin_sroff_upslope).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_sroff_farflow', 'one', 1, 'real',
     +     'Basin area-weighted average of cascading surface runoff'//
     +     ' to farfield',
     +     'inches',
     +     Basin_sroff_farflow).NE.0 ) RETURN

! original srunoff parameters
      ALLOCATE (Carea_min(Nhru))
      IF ( declparam('srunoff', 'carea_min', 'nhru', 'real',
     +     '.2', '0.', '1.0',
     +     'Minimum contributing area',
     +     'Minimum possible area contributing to surface runoff'//
     +     ' expressed as a portion of the HRU area',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Carea_max(Nhru))
      IF ( declparam('srunoff', 'carea_max', 'nhru', 'real',
     +     '.6', '0.', '1.0',
     +     'Maximum contributing area',
     +     'Maximum possible area contributing to surface runoff'//
     +     ' expressed as a portion of the HRU area',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Imperv_stor_max(Nhru))
      IF ( declparam('srunoff', 'imperv_stor_max', 'nhru', 'real',
     +     '0.', '0.', '10.',
     +     'HRU maximum impervious area retention storage',
     +     'Maximum impervious area retention storage for each HRU',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('srunoff', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('srunoff', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '2',
     +     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Soil_rechr_max(Nhru))
      IF ( declparam('srunoff', 'soil_rechr_max', 'nhru', 'real',
     +     '2.', '0.', '10.',
     +     'Maximum value for soil recharge zone',
     +     'Maximum value for soil recharge zone (upper portion'//
     +     ' of soil_moist where losses occur as both evaporation'//
     +     ' and transpiration).  Must be less than or equal to'//
     +     ' soil_moist',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Snowinfil_max(Nhru))
      IF ( declparam('srunoff', 'snowinfil_max', 'nhru', 'real',
     +     '2.', '0.', '20.',
     +     'Maximum snow infiltration per day',
     +     'Maximum snow infiltration per day',
     +     'inches/day').NE.0 ) RETURN

      ALLOCATE (Soil_moist_max(Nhru))
      IF ( declparam('srunoff', 'soil_moist_max', 'nhru', 'real',
     +     '6.', '0.', '20.',
     +     'Maximum value of water for soil zone',
     +     'Maximum available water holding capacity of soil profile.'//
     +     ' Soil profile is surface to bottom of rooting zone',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Hru_percent_imperv(Nhru))
      IF ( declparam('srunoff', 'hru_percent_imperv', 'nhru', 'real',
     +     '0.0', '0.0', '1.0',
     +     'HRU percent impervious',
     +     'Proportion of each HRU area that is impervious',
     +     'decimal fraction').NE.0 ) RETURN

! Allocate arrays for local variables and variables from other modules
      ALLOCATE (Soil_rechr(Nhru), Carea_dif(Nhru))
      ALLOCATE (Net_rain(Nhru), Net_snow(Nhru), Net_ppt(Nhru))
      ALLOCATE (Potet(Nhru), Pptmix_nopack(Nhru))
      ALLOCATE (Snow_evap(Nhru), Snowmelt(Nhru), Pkwater_equiv(Nhru))
      ALLOCATE (Snowcov_area(Nhru), Soil_moist(Nhru))
      ALLOCATE (Hru_perv(Nhru), Hru_imperv(Nhru), Pkwater_ante(Nhru))
      ALLOCATE (Hru_route_order(Nhru), Ncascade_hru(Nhru))
      ALLOCATE (Hru_percent_perv(Nhru), Hru_intcpevap(Nhru))

      srocascdecl = 0
      END FUNCTION srocascdecl

!***********************************************************************
!     srocascinit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srocascinit()
      USE PRMS_SRUNOFF_CAREA_CASC
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, ii
!***********************************************************************
      srocascinit = 1

! Carea parameters
      IF ( getparam('srunoff', 'carea_min', Nhru, 'real', Carea_min)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'carea_max', Nhru, 'real', Carea_max)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'imperv_stor_max', Nhru, 'real',
     +     Imperv_stor_max).NE.0 ) RETURN

      IF ( getparam('srunoff', 'soil_rechr_max', Nhru, 'real',
     +     Soil_rechr_max).NE.0 ) RETURN

      IF ( getparam('srunoff', 'snowinfil_max', Nhru, 'real',
     +     Snowinfil_max).NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'soil_moist_max', Nhru, 'real',
     +     Soil_moist_max).NE.0 ) RETURN

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'real', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'ncascade_hru', Nhru, 'real', Ncascade_hru)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'hru_percent_imperv', Nhru, 'real',
     +     Hru_percent_imperv).NE.0 ) RETURN

      IF ( getvar('basin', 'hru_percent_perv', Nhru, 'real',
     +     Hru_percent_perv).NE.0 ) RETURN

      IF ( getstep().EQ.0 ) THEN
        Imperv_stor = 0.0
        Imperv_evap = 0.0
        Hru_impervevap = 0.0
        Hru_impervstor = 0.0
        Infil = 0.0
        Sroff = 0.0
        Hortonian_lakes = 0.0
        Basin_sroff = 0.0
        Basin_hortonian_lakes = 0.0
        Basin_infil = 0.0
        Basin_imperv_evap = 0.0
        Basin_imperv_stor = 0.0
        Basin_hortonian = 0.0
        Hortonian_flow = 0.0
        Upslope_hortonian = 0.0
        Basin_sroff_down = 0.0
        Strm_seg_in = 0.0
        Strm_farfield = 0.0
        Basin_sroff_upslope = 0.0
        Basin_sroff_farflow = 0.0
      ENDIF

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        Carea_dif(i) = Carea_max(i) - Carea_min(i)
      ENDDO

      IF ( Prt_debug.EQ.1 ) THEN
        OPEN (197, FILE='srunoff_carea_casc.wbal')
        WRITE (197, 9001)
      ENDIF

      srocascinit = 0

 9001 FORMAT ('    Date     Water Bal     Robal      Sroff   Sroffdown',
     +        '  Srofflake  Infiltrat Impervevap Impervstor   Farflow')

      END FUNCTION srocascinit

!***********************************************************************
!     srocascrun - Computes surface runoff using contributing area
!                  computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srocascrun()
      USE PRMS_SRUNOFF_CAREA_CASC
      IMPLICIT NONE
      INTRINSIC ABS, SNGL
      INCLUDE 'fmodules.inc'
      EXTERNAL imperv_et_casc, compute_infil_casc, run_cascade_casc
! Local Variables
      INTEGER :: i, k
      INTEGER :: nowtime(6), day, mo
      REAL :: srp, sri, hru_sroff_down, runoff, harea, farflow
      REAL :: basin_robal, robal, basin_sroffp, basin_sroffi, last_stor
!***********************************************************************
      srocascrun = 1

      IF ( getvar('obs', 'route_on', 1, 'integer', Route_on)
     +     .NE.0 ) RETURN

      IF ( Route_on.EQ.1 ) THEN
        srocascrun = 0
        RETURN
      ENDIF

      IF ( getvar('intcp', 'net_rain', Nhru, 'real', Net_rain)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'net_snow', Nhru, 'real', Net_snow)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'net_ppt', Nhru, 'real', Net_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'hru_intcpevap', Nhru, 'real', Hru_intcpevap)
     +     .NE.0 ) RETURN

      IF ( getvar('smbal', 'soil_moist', Nhru, 'real', Soil_moist)
     +     .NE.0 ) RETURN

      IF ( getvar('potet', 'potet', Nhru, 'real', Potet).NE.0 ) RETURN

      IF ( getvar('snow', 'snow_evap', Nhru, 'real', Snow_evap)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'snowmelt', Nhru, 'real', Snowmelt)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'pkwater_equiv', Nhru, 'real', Pkwater_equiv)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'pkwater_ante', Nhru, 'real', Pkwater_ante)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'snowcov_area', Nhru, 'real', Snowcov_area)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'pptmix_nopack', Nhru, 'integer',
     +     Pptmix_nopack).NE.0 ) RETURN

      IF ( getvar('smbal', 'soil_rechr', Nhru, 'real', Soil_rechr)
     +     .NE.0 ) RETURN

! get pervious/impervious states in case they have been updated
      IF ( getvar('basin', 'hru_perv', Nhru, 'real', Hru_perv)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_imperv', Nhru, 'real', Hru_imperv)
     +     .NE.0 ) RETURN

      IF ( Prt_debug.EQ.1 ) THEN
        CALL dattim('now', nowtime)
        mo = nowtime(2)
        day = nowtime(3)
        basin_sroffi = 0.
        basin_sroffp = 0.
        basin_robal = 0.
      ENDIF
      Basin_sroff = 0.
      Basin_infil = 0.
      Basin_imperv_evap = 0.
      Basin_imperv_stor = 0.
      Basin_sroff_down = 0.
      Basin_sroff_upslope = 0.
      Basin_hortonian = 0.
      Basin_hortonian_lakes = 0.

      IF ( Ncascade.GT.0 ) THEN
        Cfs_conv = 43560.0/12.0/SNGL(deltim()*3600.0D0)
        Upslope_hortonian = 0.0
        Strm_seg_in = 0.0
        Strm_farfield = 0.0
        Basin_sroff_farflow = 0.
      ENDIF

      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        harea = Hru_area(i)

!rsr??? is Upslope_hortonian for whole hru or pervious only
        IF ( Hru_type(i).EQ.1 ) THEN
          Infil(i) = 0.0
          Hortonian_flow(i) = 0.0
          hru_sroff_down = 0.0
          farflow = 0.0
          last_stor = Imperv_stor(i)
          srp = 0.
          sri = 0.
          CALL compute_infil_casc(Pptmix_nopack(i), Carea_min(i),
     +         Carea_dif(i), Soil_moist(i), Soil_moist_max(i),
     +         Soil_rechr(i), Soil_rechr_max(i), Net_rain(i),
     +         Net_ppt(i), Hru_perv(i), Hru_imperv(i), Imperv_stor(i),
     +         Imperv_stor_max(i), Snowmelt(i), Snowinfil_max(i),
     +         Net_snow(i), Pkwater_equiv(i), Upslope_hortonian(i),
     +         srp, sri, Infil(i))

!******Comuute runoff for pervious and impervious area
! There may or may not be some pervious area for this HRU
          runoff = srp*Hru_perv(i) + sri*Hru_imperv(i)

!******Compute HRU weighted average (to units of inches/dt)

          Sroff(i) = runoff/harea

          IF ( Ncascade_hru(i).GT.0 .AND. Sroff(i).GT.0.0 )
     +         CALL run_cascade_casc(i, Ncascade_hru(i), Sroff(i),
     +                               hru_sroff_down, farflow)
          Hortonian_flow(i) = Sroff(i)
          Basin_sroff = Basin_sroff + Sroff(i)*harea
          Basin_sroff_upslope = Basin_sroff_upslope +
     +                          Upslope_hortonian(i)*harea

!******Compute basin weighted average, lakes not included

          Basin_infil = Basin_infil + Infil(i)*Hru_perv(i)
          Basin_sroff_down = Basin_sroff_down +
     +                       hru_sroff_down*harea
          Basin_sroff_farflow = Basin_sroff_farflow + farflow*harea
          Basin_hortonian = Basin_hortonian +
     +                      Hortonian_flow(i)*harea

!******Compute evaporation from impervious area

          IF ( Hru_imperv(i).GT.NEARZERO ) THEN
            CALL imperv_et_casc(Imperv_stor(i), Snow_evap(i), Potet(i),
     +                          Hru_intcpevap(i), Imperv_evap(i),
     +                          Snowcov_area(i))

            Hru_impervevap(i) = Imperv_evap(i)*Hru_percent_imperv(i)
            Basin_imperv_evap = Basin_imperv_evap +
     +                          Imperv_evap(i)*Hru_imperv(i)

            Hru_impervstor(i) = Imperv_stor(i)*Hru_percent_imperv(i)
            Basin_imperv_stor = Basin_imperv_stor +
     +                          Imperv_stor(i)*Hru_imperv(i)
          ENDIF
        ELSE
! HRU is a lake
!rsr, eventually add code for lake area less than hru_area
!     that includes soil_moist for percent of hru_area that is dry bank
          ! Sanity check
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i).GT.0.0 )
     +         PRINT *, 'carea lake error', Infil(i), Sroff(i),
     +               Imperv_stor(i), Imperv_evap(i)
          Hortonian_lakes(i) = Upslope_hortonian(i)
          Basin_hortonian_lakes = Basin_hortonian_lakes +
     +                            Hortonian_lakes(i)*harea
        ENDIF

        IF ( Prt_debug.EQ.1 .AND. Hru_type(i).EQ.1 ) THEN
          basin_sroffp = basin_sroffp + srp*Hru_perv(i)
          basin_sroffi = basin_sroffi + sri*Hru_imperv(i)

          robal = Snowmelt(i) + Upslope_hortonian(i) - Sroff(i)
     +            - hru_sroff_down - Infil(i)*Hru_percent_perv(i)
     +            + (last_stor-Imperv_stor(i)-Imperv_evap(i))
     +            *Hru_percent_imperv(i) - farflow
          IF ( Pptmix_nopack(i).EQ.1 .OR. (Pkwater_ante(i).LT.NEARZERO
     +         .AND.Pkwater_equiv(i).LT.NEARZERO) )
     +         robal = robal + Net_rain(i)
          basin_robal = basin_robal + robal
          IF ( ABS(robal).GT.1.0E-6 ) THEN
            IF ( ABS(robal).GT.1.0E-5 ) THEN
              WRITE (197, *) 'possible HRU water balance error'
            ELSE
              WRITE (197, *) 'HRU robal rounding issue'
            ENDIF
            WRITE (197, '(2I3,I4,19F9.5,I5)') mo, day, i, robal,
     +             Snowmelt(i), Upslope_hortonian(i), last_stor,
     +             hru_sroff_down, Infil(i), Sroff(i), Imperv_stor(i),
     +             Imperv_evap(i), Net_ppt(i), Pkwater_ante(i),
     +             Pkwater_equiv(i), Snow_evap(i), Net_snow(i), farflow,
     +             Net_rain(i), srp, sri, runoff, Pptmix_nopack(i)
          ENDIF
        ENDIF

      ENDDO

!******Compute basin weighted averages (to units of inches/dt)
      !rsr, should be land_area???
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
      Basin_sroff_down = Basin_sroff_down*Basin_area_inv
      Basin_sroff_farflow = Basin_sroff_farflow*Basin_area_inv
      Basin_imperv_evap = Basin_imperv_evap*Basin_area_inv
      Basin_imperv_stor = Basin_imperv_stor*Basin_area_inv
      Basin_infil = Basin_infil*Basin_area_inv
      Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
      Basin_hortonian = Basin_hortonian*Basin_area_inv

      IF ( Prt_debug.EQ.1 ) THEN
        basin_sroffp = basin_sroffp*Basin_area_inv
        basin_sroffi = basin_sroffi*Basin_area_inv
        robal = Basin_sroff - basin_sroffp - basin_sroffi +
     +          Basin_sroff_down + Basin_sroff_farflow
        WRITE (197, 9001) nowtime(1), mo, day, basin_robal, robal,
     +                    Basin_sroff, Basin_sroff_down,
     +                    Basin_hortonian_lakes, Basin_infil,
     +                    Basin_imperv_evap, Basin_imperv_stor,
     +                    Basin_sroff_farflow
        IF ( ABS(basin_robal).GT.1.0E-4 ) THEN
          WRITE (197, *) 'possible basin water balance error'
        ELSEIF ( ABS(basin_robal).GT.5.0E-6 ) THEN
          WRITE (197, *) 'basin_robal rounding issue'
        ENDIF
      ENDIF

      srocascrun = 0

 9001 FORMAT (I5, 2('/', I2.2), 9F11.5)

      END FUNCTION srocascrun

!***********************************************************************
!      Subroutine to compute evaporation from impervious area
!***********************************************************************
      SUBROUTINE imperv_et_casc(Imperv_stor, Snow_evap, Potet,
     +                          Hru_intcpevap, Imperv_evap, Sca)
      USE PRMS_SRUNOFF_CAREA_CASC, ONLY:NEARZERO
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Snow_evap, Potet, Sca, Hru_intcpevap
      REAL, INTENT(INOUT) :: Imperv_stor
      REAL, INTENT(OUT) :: Imperv_evap
! Local Variables
      REAL :: avail_et
!***********************************************************************
      Imperv_evap = 0.0
      IF ( Sca.LT.1.0 .AND. Imperv_stor.GT.NEARZERO ) THEN
        avail_et = Potet - Snow_evap - Hru_intcpevap
        IF ( avail_et.GE.Imperv_stor ) THEN
          Imperv_evap = Imperv_stor*(1.0-Sca)
        ELSE
          Imperv_evap = avail_et*(1.0-Sca)
        ENDIF
        Imperv_stor = Imperv_stor - Imperv_evap
      ENDIF

      END SUBROUTINE imperv_et_casc

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil_casc(Pptmix_nopack, Carea_min, Carea_dif,
     +           Soil_moist, Soil_moist_max, Soil_rechr, Soil_rechr_max,
     +           Net_rain, Net_ppt, Hru_perv, Hru_imperv, Imperv_stor,
     +           Imperv_stor_max, Snowmelt, Snowinfil_max, Net_snow,
     +           Pkwater_equiv, Upslope_hortonian, Srp, Sri, Infil)
      USE PRMS_SRUNOFF_CAREA_CASC, ONLY:NEARZERO
      IMPLICIT NONE
      EXTERNAL imperv_sroff_casc, perv_imperv_comp_casc
      EXTERNAL check_capacity_casc
! Arguments
      INTEGER, INTENT(IN) :: Pptmix_nopack
      REAL, INTENT(IN) :: Carea_min, Carea_dif, Soil_rechr_max
      REAL, INTENT(IN) :: Soil_moist_max, Soil_moist, Net_rain, Net_ppt
      REAL, INTENT(IN) :: Hru_perv, Hru_imperv, Imperv_stor_max
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow
      REAL, INTENT(IN) :: Pkwater_equiv, Soil_rechr, Upslope_hortonian
      REAL, INTENT(INOUT) :: Imperv_stor, Srp, Sri, Infil
! Local Variables
      REAL :: ppti, pptp, snri
!***********************************************************************
! compute runoff from cascading Hortonian flow
      IF ( Upslope_hortonian.GT.0.0 ) THEN
        pptp = Upslope_hortonian
        ppti = Upslope_hortonian
        CALL perv_imperv_comp_casc(pptp, ppti, Hru_perv, Hru_imperv,
     +                             Carea_min, Carea_dif, Soil_rechr,
     +                             Soil_rechr_max, Imperv_stor_max,
     +                             Imperv_stor, Srp, Sri, Infil)
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack.EQ.1 ) THEN
        pptp = Net_rain
        ppti = Net_rain
        CALL perv_imperv_comp_casc(pptp, ppti, Hru_perv, Hru_imperv,
     +                             Carea_min, Carea_dif, Soil_rechr,
     +                             Soil_rechr_max, Imperv_stor_max,
     +                             Imperv_stor, Srp, Sri, Infil)
      ENDIF

!******If precip on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt.GT.0.0 ) THEN

        IF ( Pkwater_equiv.GT.0.0 .OR. Net_ppt.LE.Net_snow ) THEN

!******Pervious area computations
          IF ( Hru_perv.GT.NEARZERO ) THEN
            Infil = Infil + Snowmelt
            IF ( Pkwater_equiv.GT.0.0 )
     +           CALL check_capacity_casc(Soil_moist_max, Soil_moist,
     +                                    Snowinfil_max, Srp, Infil)
          ENDIF

!******Impervious area computations
          IF ( Hru_imperv.GT.NEARZERO ) THEN
            CALL imperv_sroff_casc(Imperv_stor_max, Imperv_stor,
     +                             Snowmelt, snri)
            Sri = Sri + snri
          ENDIF

        ELSE
!******Snowmelt occurred and depleted the snowpack

          pptp = Snowmelt
          ppti = Snowmelt

          CALL perv_imperv_comp_casc(pptp, ppti, Hru_perv, Hru_imperv,
     +                               Carea_min, Carea_dif, Soil_rechr,
     +                               Soil_rechr_max, Imperv_stor_max,
     +                               Imperv_stor, Srp, Sri, Infil)

        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( Pkwater_equiv.LT.NEARZERO ) THEN

!      If no snowmelt and no snowpack but there was net snow then
!      snowpack was small and was lost to sublimation.

        IF ( Net_snow.LT.NEARZERO .AND. Net_rain.GT.NEARZERO ) THEN
! no snow, some rain
          pptp = Net_rain
          ppti = Net_rain

          CALL perv_imperv_comp_casc(pptp, ppti, Hru_perv, Hru_imperv,
     +                               Carea_min, Carea_dif, Soil_rechr,
     +                               Soil_rechr_max, Imperv_stor_max,
     +                               Imperv_stor, Srp, Sri, Infil)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.
      ELSEIF ( Infil.GT.0.0 ) THEN
        CALL check_capacity_casc(Soil_moist_max, Soil_moist,
     +                           Snowinfil_max, Srp, Infil)

      ENDIF

      END SUBROUTINE compute_infil_casc

!***********************************************************************
!***********************************************************************
      SUBROUTINE perv_imperv_comp_casc(Pptp, Ppti, Hru_perv, Hru_imperv,
     +                                 Carea_min, Carea_dif, Soil_rechr,
     +                                 Soil_rechr_max, Imperv_stor_max,
     +                                 Imperv_stor, Srp, Sri, Infil)
      USE PRMS_SRUNOFF_CAREA_CASC, ONLY:NEARZERO
      IMPLICIT NONE
      EXTERNAL perv_sroff_carea_casc, imperv_sroff_casc
! Arguments
      REAL, INTENT(IN) :: Pptp, Ppti, Hru_perv, Hru_imperv
      REAL, INTENT(IN) :: Carea_min, Carea_dif, Soil_rechr
      REAL, INTENT(IN) :: Soil_rechr_max, Imperv_stor_max
      REAL, INTENT(INOUT) :: Srp, Sri, Infil, Imperv_stor
! Local Variables
      REAL :: inp, snrp, snri
!***********************************************************************
!******Pervious area computations
      IF ( Pptp.GT.0.0 .AND. Hru_perv.GT.NEARZERO ) THEN
        CALL perv_sroff_carea_casc(Carea_min, Carea_dif, Soil_rechr,
     +                             Soil_rechr_max, Pptp, inp, snrp)
        Infil = Infil + inp
        Srp = Srp + snrp
      ENDIF

!******Impervious area computations
      IF ( Ppti.GT.0.0 .AND. Hru_imperv.GT.NEARZERO ) THEN
        CALL imperv_sroff_casc(Imperv_stor_max, Imperv_stor, Ppti, snri)
        Sri = Sri + snri
      ENDIF

      END SUBROUTINE perv_imperv_comp_casc

!***********************************************************************
!      Subroutine to compute runoff from pervious area using
!      contributing area computations
!***********************************************************************

      SUBROUTINE perv_sroff_carea_casc(Carea_min, Carea_dif, Soil_rechr,
     +                                 Soil_rechr_max, Pptp, Infil, Srp)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Carea_min, Carea_dif, Soil_rechr
      REAL, INTENT(IN) :: Soil_rechr_max, Pptp
      REAL, INTENT(OUT) :: Infil, Srp
! Local Variables
      REAL :: ca_percent
!***********************************************************************
      ca_percent = Carea_min + Carea_dif*(Soil_rechr/Soil_rechr_max)

      Srp = ca_percent*Pptp
      Infil = Pptp - Srp

      END SUBROUTINE perv_sroff_carea_casc

!***********************************************************************
!      Subroutine to compute runoff from impervious area
!***********************************************************************
      SUBROUTINE imperv_sroff_casc(Imperv_stor_max, Imperv_stor, Ppti,
     +                             Sri)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Imperv_stor_max, Ppti
      REAL, INTENT(INOUT) :: Imperv_stor
      REAL, INTENT(OUT) :: Sri
! Local Variables
      REAL :: avail_stor
!***********************************************************************
      avail_stor = Imperv_stor_max - Imperv_stor
      IF ( Ppti.GT.avail_stor ) THEN
        Imperv_stor = Imperv_stor_max
        Sri = Ppti - avail_stor
      ELSE
        Imperv_stor = Imperv_stor + Ppti
        Sri = 0.0
      ENDIF

      END SUBROUTINE imperv_sroff_casc

!***********************************************************************
! fill soil to soil_moist_max, if more than capacity restrict
! infiltration by snowinfil_max, with excess added to runoff
!***********************************************************************
      SUBROUTINE check_capacity_casc(Soil_moist_max, Soil_moist,
     +                               Snowinfil_max, Srp, Infil)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Soil_moist_max, Soil_moist, Snowinfil_max
      REAL, INTENT(INOUT) :: Srp, Infil
! Local Variables
      REAL :: capacity, excess
!***********************************************************************
      capacity = Soil_moist_max - Soil_moist
      excess = Infil - capacity
      IF ( excess.GT.Snowinfil_max ) THEN
        Srp = Srp + excess - Snowinfil_max
        Infil = Snowinfil_max + capacity
      ENDIF

      END SUBROUTINE check_capacity_casc

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_casc(Ihru, Ncascade_hru, Runoff,
     +                            Hru_sroff_down, Farflow)
      USE PRMS_SRUNOFF_CAREA_CASC, ONLY:Cfs_conv, Strm_seg_in,
     +    Upslope_hortonian, Nsegmentp1, Strm_farfield
      USE PRMS_CASCADE, ONLY:Hru_down, Hru_down_pct, Hru_down_pctwt,
     +    Cascade_area
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Ncascade_hru
      REAL, INTENT(INOUT) :: Runoff, Hru_sroff_down, Farflow
! Local Variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j.GT.0 ) THEN
          Upslope_hortonian(j) = Upslope_hortonian(j)
     +                           + Runoff*Hru_down_pctwt(k, Ihru)
          Hru_sroff_down = Hru_sroff_down + Runoff*Hru_down_pct(k, Ihru)

! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j.LT.0 ) THEN
          j = IABS(j)
          IF ( j.NE.Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) +
     +                       Runoff*Cfs_conv*Cascade_area(k, Ihru)
          ELSE
            Strm_farfield = Strm_farfield +
     +                      Runoff*Cfs_conv*Cascade_area(k, Ihru)
            Farflow = Farflow + Runoff*Hru_down_pct(k, Ihru)
          ENDIF
        ENDIF
      ENDDO

! reset Sroff as it accumulates flow to streams
      Runoff = Runoff - Hru_sroff_down - Farflow

      END SUBROUTINE run_cascade_casc

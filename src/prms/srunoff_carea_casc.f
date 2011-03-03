!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a linear
! variable-source-area method allowing for cascading flow; modification
! of srunoff_carea_prms
!
!
!     version: 2.2 added cascading flow for infiltration and runoff
!***********************************************************************
      MODULE PRMS_SRUNOFF_CAREA_CASC
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 197
      REAL, SAVE, ALLOCATABLE :: Carea_dif(:)
      REAL, SAVE, ALLOCATABLE :: Pkwater_last(:)
!   Declared Variables
      REAL, SAVE :: Basin_sroff_down, Basin_hortonian_lakes
      REAL, SAVE :: Basin_sroff_upslope, Basin_hortonian
      REAL, SAVE :: Basin_sroff_farflow
      REAL, SAVE, ALLOCATABLE :: Imperv_stor(:), Imperv_evap(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_hortonian(:)
      REAL, SAVE, ALLOCATABLE :: Hortonian_flow(:)
!   Declared Variables from other modules - smbal
      ! RSR: Caution, using values from previous time step
      REAL :: Basin_soil_moist
      REAL, ALLOCATABLE :: Soil_moist(:), Soil_rechr(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Carea_min(:)
      END MODULE PRMS_SRUNOFF_CAREA_CASC

!***********************************************************************
!     Main srunoff_carea routine
!***********************************************************************
      INTEGER FUNCTION srunoff_carea_casc()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srocascdecl, srocascinit, srocascrun
!***********************************************************************
      srunoff_carea_casc = 0

      IF ( Process_flag==0 ) THEN
        srunoff_carea_casc = srocascrun()
      ELSEIF ( Process_flag==1 ) THEN
        srunoff_carea_casc = srocascdecl()
      ELSEIF ( Process_flag==2 ) THEN
        srunoff_carea_casc = srocascinit()
      ENDIF

      END FUNCTION srunoff_carea_casc

!***********************************************************************
!     srocascdecl - set up parameters for surface runoff computations
!   Declared Parameters
!     carea_min, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max, soil_rechr_max
!***********************************************************************
      INTEGER FUNCTION srocascdecl()
      USE PRMS_SRUNOFF_CAREA_CASC
      USE PRMS_MODULE, ONLY: Model, Ncascade
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declvar, declparam
!***********************************************************************
      srocascdecl = 1

      IF ( declmodule(
     +'$Id: srunoff_carea_casc.f 2250 2010-12-10 17:24:28Z rsregan $'
     +).NE.0 ) RETURN

! srunoff variables
      ALLOCATE (Imperv_stor(Nhru))
      IF ( declvar('srunoff', 'imperv_stor', 'nhru', Nhru, 'real',
     +     'Current storage on impervious area for each HRU',
     +     'inches',
     +     Imperv_stor).NE.0 ) RETURN

      ALLOCATE (Imperv_evap(Nhru))
      IF ( declvar('srunoff', 'imperv_evap', 'nhru', Nhru, 'real',
     +     'Evaporation from impervious area',
     +     'inches',
     +     Imperv_evap).NE.0 ) RETURN

! cascading variables and parameters
      ALLOCATE (Upslope_hortonian(Nhru))
      IF ( Ncascade>0 .OR. Model==99 ) THEN
        IF ( declvar('srunoff', 'basin_hortonian_lakes', 'one', 1,
     +       'real',
     +       'Basin area-weighted average of Hortonian surface runoff'//
     +       ' to lakes',
     +       'inches',
     +       Basin_hortonian_lakes).NE.0 ) RETURN

        ALLOCATE (Hortonian_flow(Nhru))
        IF ( declvar('srunoff', 'hortonian_flow', 'nhru', Nhru, 'real',
     +       'Hortonian surface runoff reaching stream network',
     +       'inches',
     +       Hortonian_flow).NE.0 ) RETURN

        IF ( declvar('srunoff', 'basin_hortonian', 'one', 1, 'real',
     +       'Basin weighted average Hortonian runoff',
     +       'inches',
     +       Basin_Hortonian).NE.0 ) RETURN

        IF ( declvar('srunoff', 'upslope_hortonian', 'nhru', Nhru,
     +       'real',
     +       'Hortonian surface runoff received from HRUs upslope',
     +       'inches',
     +       Upslope_hortonian).NE.0 ) RETURN

        IF ( declvar('srunoff', 'basin_sroff_down', 'one', 1, 'real',
     +       'Basin area-weighted average of cascading surface runoff',
     +       'inches',
     +       Basin_sroff_down).NE.0 ) RETURN

        IF ( declvar('srunoff', 'basin_sroff_upslope', 'one', 1, 'real',
     +       'Basin area-weighted average of cascading surface runoff'//
     +       ' received from HRUs up slope',
     +       'inches',
     +       Basin_sroff_upslope).NE.0 ) RETURN

        IF ( declvar('srunoff', 'basin_sroff_farflow', 'one', 1, 'real',
     +       'Basin area-weighted average of cascading surface runoff'//
     +       ' to farfield',
     +       'inches',
     +       Basin_sroff_farflow).NE.0 ) RETURN

      ENDIF

! Declare parameters
      ALLOCATE (Carea_min(Nhru))
      IF ( declparam('srunoff', 'carea_min', 'nhru', 'real',
     +     '.2', '0.', '1.0',
     +     'Minimum contributing area',
     +     'Minimum possible area contributing to surface runoff'//
     +     ' expressed as a portion of the HRU area',
     +     'decimal fraction').NE.0 ) RETURN

! Allocate arrays for local variables and variables from other modules
      ALLOCATE ( Soil_moist(Nhru), Pkwater_last(Nhru) )
      ALLOCATE ( Soil_rechr(Nhru), Carea_dif(Nhru) )

      srocascdecl = 0
      END FUNCTION srocascdecl

!***********************************************************************
!     srocascinit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srocascinit()
      USE PRMS_SRUNOFF_CAREA_CASC
      USE PRMS_MODULE, ONLY: Ncascade
      USE PRMS_BASIN, ONLY: Hru_perv, Hru_percent_impv, Hru_imperv,
     +    Print_debug, NEARZERO, Hru_area, Hru_percent_perv,
     +    Basin_area_inv, Timestep, Nhru
      USE PRMS_FLOWVARS, ONLY: Carea_max, Hru_hortonian_cascadeflow
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i
!***********************************************************************
      srocascinit = 1

      IF ( Timestep==0 ) THEN
        DO i = 1, Nhru
          Imperv_stor(i) = 0.0
          Imperv_evap(i) = 0.0
          Upslope_hortonian(i) = 0.0
          IF ( Ncascade>0 ) Hortonian_flow(i) = 0.0
        ENDDO
        IF ( Ncascade>0 ) THEN
          Basin_hortonian_lakes = 0.0
          Basin_hortonian = 0.0
          Basin_sroff_upslope = 0.0
          Basin_sroff_farflow = 0.0
          Basin_sroff_down = 0.0
        ENDIF
      ENDIF

! Carea parameters
      IF ( getparam('srunoff', 'carea_min', Nhru, 'real', Carea_min)
     +     .NE.0 ) RETURN

      DO i = 1, Nhru
        Carea_dif(i) = Carea_max(i) - Carea_min(i)
      ENDDO

      IF ( Print_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='srunoff_carea_casc.wbal')
        IF ( Ncascade>0 ) THEN
          WRITE (BALUNT, 9002)
        ELSE
          WRITE (BALUNT, 9001)
        ENDIF
      ENDIF

      srocascinit = 0

 9001 FORMAT ('    Date     Water Bal     Robal      Sroff',
     +        '  Infiltrat  Impervevap Impervstor')
 9002 FORMAT ('    Date     Water Bal     Robal      Sroff   Sroffdown',
     +        '  Srofflake  Infiltrat Impervevap Impervstor    Farflow')

      END FUNCTION srocascinit

!***********************************************************************
!     srocascrun - Computes surface runoff using contributing area
!                  computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srocascrun()
      USE PRMS_SRUNOFF_CAREA_CASC
      USE PRMS_MODULE, ONLY: Ncascade
      USE PRMS_BASIN, ONLY: Print_debug, Active_hrus, Hru_route_order,
     +    Hru_perv, Hru_imperv, Hru_percent_impv, Hru_percent_perv,
     +    NEARZERO, Hru_area, Hru_type, Basin_area_inv, Nhru
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Hru_impervevap,
     +    Sroff, Basin_sroff, Infil, Soil_rechr_max,
     +    Snowinfil_max, Hru_impervstor, Hortonian_lakes,
     +    Basin_imperv_evap, Basin_imperv_stor, Basin_infil,
     +    Hru_hortonian_cascadeflow, Strm_seg_in, Strm_farfield,
     +    Imperv_stor_max
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_OBS, ONLY: Nowtime
      USE PRMS_INTCP, ONLY: Net_rain, Net_snow, Net_ppt, Hru_intcpevap
      USE PRMS_SNOW, ONLY: Pkwater_equiv, Pk_depth, Pptmix_nopack,
     +    Snow_evap, Snowcov_area, Snowmelt
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getvar
      EXTERNAL imperv_et, compute_infil_casc, run_cascade_carea
      EXTERNAL perv_sroff_carea_casc
! Local Variables
      INTEGER :: i, k, day, mo
      REAL :: srp, sri, runoff
      REAL :: hru_sroff_down, harea, farflow, avail_et
      REAL :: basin_robal, robal, basin_sroffp, basin_sroffi
      REAL :: last_stor, tmp, himperv, hperv
!***********************************************************************
      srocascrun = 1

      IF ( getvar('smbal', 'soil_moist', Nhru, 'real', Soil_moist)
     +     .NE.0 ) RETURN

      IF ( getvar('smbal', 'soil_rechr', Nhru, 'real', Soil_rechr)
     +     .NE.0 ) RETURN

      mo = Nowtime(2)
      day = Nowtime(3)
      IF ( Print_debug.EQ.1 ) THEN
        basin_sroffi = 0.
        basin_sroffp = 0.
        basin_robal = 0.
      ENDIF
      Basin_sroff = 0.
      Basin_infil = 0.
      Basin_imperv_evap = 0.
      Basin_imperv_stor = 0.

      IF ( Ncascade>0 ) THEN
        Basin_sroff_down = 0.
        Basin_sroff_upslope = 0.
        Basin_hortonian = 0.
        Basin_hortonian_lakes = 0.
!rsr??? is Upslope_hortonian for whole hru or pervious only
        Upslope_hortonian = 0.0
        Strm_seg_in = 0.0
        Strm_farfield = 0.0
        Basin_sroff_farflow = 0.
      ENDIF

      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        harea = Hru_area(i)
        runoff = 0.0

        IF ( Hru_type(i).NE.2 ) THEN
          Infil(i) = 0.0
          Pkwater_last(i) = Pkwater_equiv(i)
          last_stor = Imperv_stor(i)
          hperv = Hru_perv(i)
          himperv = Hru_imperv(i)
          srp = 0.
          sri = 0.


!******Compute runoff for pervious and impervious storage area
          CALL compute_infil_casc(Pptmix_nopack(i), Carea_min(i),
     +         Carea_dif(i), Soil_moist(i), Soil_moist_max(i),
     +         Soil_rechr(i), Soil_rechr_max(i), Net_rain(i),
     +         Net_ppt(i), himperv, Imperv_stor(i), Imperv_stor_max(i),
     +         Snowmelt(i), Snowinfil_max(i), Net_snow(i),
     +         Pkwater_equiv(i), srp, sri, Infil(i), Hru_type(i),
     +         Upslope_hortonian(i))

          avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)

          runoff = srp*hperv + sri*himperv
          Sroff(i) = runoff/harea

!******Compute HRU weighted average (to units of inches/dt)
          hru_sroff_down = 0.0
          farflow = 0.0

          IF ( Hru_type(i)==1 ) THEN
            IF ( Ncascade>0 ) THEN
              IF ( Ncascade_hru(i)>0 .AND. Sroff(i)>0.0 )
     +             CALL run_cascade_carea(i, Ncascade_hru(i), Sroff(i),
     +                  hru_sroff_down, farflow)
              Hru_hortonian_cascadeflow(i) = hru_sroff_down + farflow
              Hortonian_flow(i) = Sroff(i)
              Basin_sroff_upslope = Basin_sroff_upslope +
     +                              Upslope_hortonian(i)*harea
              Basin_sroff_down = Basin_sroff_down +
     +                           hru_sroff_down*harea
              Basin_sroff_farflow = Basin_sroff_farflow + farflow*harea
              Basin_hortonian = Basin_hortonian +
     +                          Hortonian_flow(i)*harea
            ENDIF
            Basin_sroff = Basin_sroff + Sroff(i)*harea

!******Compute basin weighted average, lakes not included

          ENDIF

          Basin_infil = Basin_infil + Infil(i)*hperv

!******Compute evaporation from impervious area

          IF ( himperv.GT.NEARZERO ) THEN
            tmp = avail_et/Hru_percent_impv(i)
            CALL imperv_et(Imperv_stor(i), tmp, Imperv_evap(i),
     +           Snowcov_area(i))
            Hru_impervevap(i) = Imperv_evap(i)*Hru_percent_impv(i)
            Basin_imperv_evap = Basin_imperv_evap +
     +                          Imperv_evap(i)*himperv

            Hru_impervstor(i) = Imperv_stor(i)*Hru_percent_impv(i)
            Basin_imperv_stor = Basin_imperv_stor +
     +                          Imperv_stor(i)*himperv
            avail_et = avail_et - Hru_impervevap(i)
            IF ( avail_et<0.0 ) THEN
            !rsr, sanity check
              IF ( avail_et<-1.0E-5 ) PRINT *,
     +             'avail_et<0 in srunoff imperv', i, mo, day, avail_et
              Hru_impervevap(i) = Hru_impervevap(i) + avail_et
              IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
              avail_et = 0.0
            ENDIF
          ENDIF
          IF ( Print_debug.EQ.1 ) THEN
            basin_sroffp = basin_sroffp + srp*hperv
            basin_sroffi = basin_sroffi + sri*himperv
            robal = -Sroff(i) - Infil(i)*Hru_percent_perv(i)
     +              + (last_stor-Imperv_stor(i)-Imperv_evap(i))
     +              *Hru_percent_impv(i)
            IF ( Ncascade>0 ) robal = robal + Upslope_hortonian(i)
     +                                - hru_sroff_down - farflow
            IF ( robal>NEARZERO ) THEN
              robal = robal + Snowmelt(i)
              IF ( Pptmix_nopack(i).EQ.1 ) THEN
                robal = robal + Net_rain(i)
              ELSEIF ( Pkwater_last(i)<NEARZERO ) THEN
                 robal = robal + Net_rain(i)
              ENDIF
              basin_robal = basin_robal + robal
              IF ( ABS(robal)>2.0E-5 ) THEN
                IF ( ABS(robal)>1.0E-4 ) THEN
                  WRITE (BALUNT, *) 'possible HRU water balance error'
                ELSE
                  WRITE (BALUNT, *) 'HRU robal rounding issue'
                ENDIF
                IF ( Ncascade>0 ) THEN
                  WRITE (BALUNT, '(2I3,I6,F10.6,18F9.5,I3)') mo, day, i,
     +                   robal, Snowmelt(i), Upslope_hortonian(i),
     +                   last_stor, hru_sroff_down, Infil(i), Sroff(i),
     +                   Imperv_stor(i), Imperv_evap(i), Net_ppt(i),
     +                   Pkwater_last(i), Pkwater_equiv(i),
     +                   Snow_evap(i), Net_snow(i), farflow,
     +                   Net_rain(i), srp, sri, runoff, Pptmix_nopack(i)
                ELSE
                  WRITE (BALUNT,'(2I3,I4,13F9.5,I5)') mo, day, i, robal,
     +                   Snowmelt(i), last_stor, Infil(i), Sroff(i),
     +                   Imperv_stor(i), Imperv_evap(i), Net_ppt(i),
     +                   Pkwater_last(i), Pkwater_equiv(i),
     +                   Snow_evap(i), Net_snow(i), Net_rain(i),
     +                   Pptmix_nopack(i)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSE
! HRU is a lake
!rsr, eventually add code for lake area less than hru_area
!     that includes soil_moist for percent of hru_area that is dry bank
          ! Sanity check
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i).GT.0.0 )
     +         PRINT *, 'carea lake error', Infil(i), Sroff(i),
     +               Imperv_stor(i), Imperv_evap(i), i
          IF ( Ncascade>0 ) THEN
            Hortonian_lakes(i) = Upslope_hortonian(i)
            Basin_hortonian_lakes = Basin_hortonian_lakes +
     +                              Hortonian_lakes(i)*harea
          ENDIF
        ENDIF

      ENDDO

!******Compute basin weighted averages (to units of inches/dt)
      !rsr, should be land_area???
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_imperv_evap = Basin_imperv_evap*Basin_area_inv
      Basin_imperv_stor = Basin_imperv_stor*Basin_area_inv
      Basin_infil = Basin_infil*Basin_area_inv
      IF ( Ncascade>0 ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_farflow = Basin_sroff_farflow*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
        Basin_hortonian = Basin_hortonian*Basin_area_inv
      ENDIF

      IF ( Print_debug.EQ.1 ) THEN
        basin_sroffp = basin_sroffp*Basin_area_inv
        basin_sroffi = basin_sroffi*Basin_area_inv
        robal = Basin_sroff - basin_sroffp - basin_sroffi
        IF ( Ncascade>0 ) THEN
          robal = robal + Basin_sroff_down + Basin_sroff_farflow
          WRITE (BALUNT, 9001) Nowtime(1), mo, day, basin_robal, robal,
     +                         Basin_sroff, Basin_sroff_down,
     +                         Basin_hortonian_lakes, Basin_infil,
     +                         Basin_imperv_evap, Basin_imperv_stor,
     +                         Basin_sroff_farflow
        ELSE
          WRITE (BALUNT, 9001) Nowtime(1), mo, day, basin_robal, robal,
     +                      Basin_sroff, Basin_infil, Basin_imperv_evap,
     +                      Basin_imperv_stor
        ENDIF
        IF ( ABS(basin_robal).GT.2.0E-4 ) THEN
          WRITE (BALUNT, *) 'possible basin water balance error'
        ELSEIF ( ABS(basin_robal).GT.2.0E-5 ) THEN
          WRITE (BALUNT, *) 'basin_robal rounding issue'
        ENDIF
      ENDIF

      srocascrun = 0

 9001 FORMAT (I5, 2('/', I2.2), 10F11.5)

      END FUNCTION srocascrun

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil_casc(Pptmix_nopack, Carea_min, Carea_dif,
     +           Soil_moist, Soil_moist_max, Soil_rechr, Soil_rechr_max,
     +           Net_rain, Net_ppt, Hru_imperv, Imperv_stor,
     +           Imperv_stor_max, Snowmelt, Snowinfil_max, Net_snow,
     +           Pkwater_equiv, Srp, Sri, Infil, Hru_type,
     +           Upslope_hortonian)
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      EXTERNAL imperv_sroff, perv_imperv_comp_casc, check_capacity
! Arguments
      INTEGER, INTENT(IN) :: Pptmix_nopack, Hru_type
      REAL, INTENT(IN) :: Carea_min, Carea_dif, Soil_rechr_max
      REAL, INTENT(IN) :: Soil_moist_max, Soil_moist, Net_rain, Net_ppt
      REAL, INTENT(IN) :: Hru_imperv, Imperv_stor_max
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
        CALL perv_imperv_comp_casc(pptp, ppti, Hru_imperv,
     +       Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +       Imperv_stor_max, Imperv_stor, Srp, Sri, Infil, Hru_type)
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack.EQ.1 ) THEN
        pptp = Net_rain
        ppti = Net_rain
        CALL perv_imperv_comp_casc(pptp, ppti, Hru_imperv,
     +       Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +       Imperv_stor_max, Imperv_stor, Srp, Sri, Infil, Hru_type)
      ENDIF

!******If precip on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt.GT.0.0 ) THEN

        IF ( Pkwater_equiv.GT.0.0 .OR. Net_ppt.LE.Net_snow ) THEN

!******Pervious area computations
          Infil = Infil + Snowmelt
          IF ( Infil>0 .AND. Hru_type==1 )
     +         CALL check_capacity(Soil_moist_max,
     +              Soil_moist, Snowinfil_max, Srp, Infil)

!******Impervious area computations
          IF ( Hru_imperv.GT.NEARZERO ) THEN
            CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Snowmelt,
     +           snri, Hru_type)
            Sri = Sri + snri
          ENDIF

        ELSE
!******Snowmelt occurred and depleted the snowpack

          pptp = Snowmelt
          ppti = Snowmelt

          CALL perv_imperv_comp_casc(pptp, ppti, Hru_imperv,
     +         Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +         Imperv_stor_max, Imperv_stor, Srp, Sri, Infil, Hru_type)

        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( Pkwater_equiv.LT.NEARZERO ) THEN

!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.

        IF ( Net_snow.LT.NEARZERO .AND. Net_rain.GT.NEARZERO ) THEN
! no snow, some rain
          pptp = Net_rain
          ppti = Net_rain

          CALL perv_imperv_comp_casc(pptp, ppti, Hru_imperv,
     +         Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +         Imperv_stor_max, Imperv_stor, Srp, Sri, Infil, Hru_type)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.
      ELSEIF ( Infil.GT.0.0 .AND. Hru_type==1 ) THEN
        CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max,
     +       Srp, Infil)

      ENDIF

      END SUBROUTINE compute_infil_casc

!***********************************************************************
!***********************************************************************
      SUBROUTINE perv_imperv_comp_casc(Pptp, Ppti, Hru_imperv,
     +           Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +           Imperv_stor_max, Imperv_stor, Srp, Sri, Infil,
     +           Hru_type)
      USE PRMS_BASIN, ONLY:NEARZERO
      IMPLICIT NONE
      EXTERNAL perv_sroff_carea_casc, imperv_sroff
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Pptp, Ppti, Hru_imperv
      REAL, INTENT(IN) :: Carea_min, Carea_dif, Soil_rechr
      REAL, INTENT(IN) :: Soil_rechr_max, Imperv_stor_max
      REAL, INTENT(INOUT) :: Srp, Sri, Infil, Imperv_stor
! Local Variables
      REAL :: inp, snrp, snri
!***********************************************************************
!******Pervious area computations
!     IF ( Pptp.GT.0.0 .AND. Hru_perv.GT.NEARZERO ) THEN
      snrp = 0.0
      IF ( Pptp.GT.0.0 ) THEN
        CALL perv_sroff_carea_casc(Carea_min, Carea_dif, Soil_rechr,
     +       Soil_rechr_max, Pptp, inp, snrp, Hru_type)
        Infil = Infil + inp
      ENDIF
      Srp = Srp + snrp

!******Impervious area computations
      IF ( Ppti.GT.0.0 .AND. Hru_imperv.GT.NEARZERO ) THEN
        CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, snri,
     +       Hru_type)
        Sri = Sri + snri
      ENDIF

      END SUBROUTINE perv_imperv_comp_casc

!***********************************************************************
!      Subroutine to compute runoff from pervious area using
!      contributing area computations
!***********************************************************************

      SUBROUTINE perv_sroff_carea_casc(Carea_min, Carea_dif, Soil_rechr,
     +           Soil_rechr_max, Pptp, Infil, Srp, Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Carea_min, Carea_dif, Soil_rechr
      REAL, INTENT(IN) :: Soil_rechr_max, Pptp
      REAL, INTENT(OUT) :: Infil, Srp
! Local Variables
      REAL :: ca_percent
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        ca_percent = Carea_min + Carea_dif*(Soil_rechr/Soil_rechr_max)
        Srp = ca_percent*Pptp
        Infil = Pptp - Srp
      ELSE ! Hru_type=3
        Srp = 0.0
        Infil = Pptp
      ENDIF

      END SUBROUTINE perv_sroff_carea_casc

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_carea(Ihru, Ncascade_hru, Runoff,
     +           Hru_sroff_down, Farflow)
      USE PRMS_SRUNOFF_CAREA_CASC, ONLY: Upslope_hortonian
      USE PRMS_BASIN, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_pct, Hru_down_pctwt,
     +    Cascade_area
      USE PRMS_OBS, ONLY: Cfs_conv
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

      END SUBROUTINE run_cascade_carea

!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a
! non-linear variable-source-area method allowing for cascading flow;
! modification of srunoff_smidx_prms
!
!     version: 2.2 added cascading flow for infiltration and runoff
!***********************************************************************
      MODULE PRMS_SRUNOFF_SMIDX_CASC
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 197
      REAL, SAVE, ALLOCATABLE :: Pkwater_last(:)
      REAL, SAVE :: Srp, Sri
!   Declared Variables
      REAL, SAVE :: Basin_sroff_down, Basin_hortonian_lakes
      REAL, SAVE :: Basin_sroff_upslope, Basin_hortonian
      REAL, SAVE :: Basin_sroff_farflow
      DOUBLE PRECISION, SAVE :: Basin_sroffi, Basin_sroffp
      REAL, SAVE, ALLOCATABLE :: Imperv_stor(:), Imperv_evap(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_hortonian(:)
      REAL, SAVE, ALLOCATABLE :: Hortonian_flow(:)
!   Declared Variables from other modules - smbal
      ! RSR: Caution, using values from previous time step
      REAL :: Basin_soil_moist
      REAL, ALLOCATABLE :: Soil_moist(:), Soil_rechr(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Smidx_coef(:), Smidx_exp(:)
      END MODULE PRMS_SRUNOFF_SMIDX_CASC

!***********************************************************************
!     Main srunoff_smidx routine
!***********************************************************************
      INTEGER FUNCTION srunoff_smidx_casc()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srosmcadecl, srosmcainit, srosmcarun
!***********************************************************************
      srunoff_smidx_casc = 0

      IF ( Process_flag==0 ) THEN
        srunoff_smidx_casc = srosmcarun()
      ELSEIF ( Process_flag==1 ) THEN
        srunoff_smidx_casc = srosmcadecl()
      ELSEIF ( Process_flag==2 ) THEN
        srunoff_smidx_casc = srosmcainit()
      ENDIF

      END FUNCTION srunoff_smidx_casc

!***********************************************************************
!     srosmcadecl - set up parameters for surface runoff computations
!   Declared Parameters
!     smidx_coef, smidx_exp, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max
!***********************************************************************
      INTEGER FUNCTION srosmcadecl()
      USE PRMS_SRUNOFF_SMIDX_CASC
      USE PRMS_MODULE, ONLY: Model, Ncascade
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      srosmcadecl = 1

      IF ( declmodule(
     +'$Id: srunoff_smidx_casc.f 3116 2011-05-17 16:20:01Z rsregan $'
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

      IF ( declvar('srunoff', 'basin_sroffi', 'one', 1, 'double',
     +     'Basin area-weighted average surface runoff from'//
     +     ' impervious areas',
     +     'inches',
     +      Basin_sroffi).NE.0 ) RETURN

      IF ( declvar('srunoff', 'basin_sroffp', 'one', 1, 'double',
     +     'Basin area-weighted average surface runoff from pervious'//
     +     ' areas',
     +     'inches',
     +     Basin_sroffp).NE.0 ) RETURN

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
      ALLOCATE (Smidx_coef(Nhru))
      IF ( declparam('srunoff', 'smidx_coef', 'nhru', 'real',
     +     '0.01', '0.0001', '1.0',
     +     'Coefficient in contributing area computations',
     +     'Coefficient in non-linear contributing area algorithm.'//
     +     ' Equation used is: contributing area = smidx_coef *'//
     +     ' 10.**(smidx_exp*smidx) where smidx is soil_moist +'//
     +     ' .5 * ppt_net',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Smidx_exp(Nhru))
      IF ( declparam('srunoff', 'smidx_exp', 'nhru', 'real',
     +     '0.3', '0.2', '0.8',
     +     'Exponent in contributing area computations',
     +     'Exponent in non-linear contributing area algorithm.'//
     +     ' Equation used is: contributing area = smidx_coef *'//
     +     ' 10.**(smidx_exp*smidx) where smidx is soil_moist +'//
     +     ' .5 * ppt_net',
     +     '1/inch').NE.0 ) RETURN


! Allocate arrays for local variables and variables from other modules
      ALLOCATE ( Soil_moist(Nhru) )
      ALLOCATE ( Soil_rechr(Nhru) )

      srosmcadecl = 0
      END FUNCTION srosmcadecl

!***********************************************************************
!     srosmcainit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srosmcainit()
      USE PRMS_SRUNOFF_SMIDX_CASC
      USE PRMS_MODULE, ONLY: Ncascade
      USE PRMS_BASIN, ONLY: Hru_perv, Hru_percent_impv, Hru_imperv,
     +    Print_debug, NEARZERO, Hru_area, Hru_percent_perv,
     +    Basin_area_inv, Timestep, Nhru
      USE PRMS_FLOWVARS, ONLY: Hru_hortonian_cascadeflow
      IMPLICIT NONE
      INTRINSIC EXP, LOG
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i
!***********************************************************************
      srosmcainit = 1

      IF ( Timestep==0 ) THEN
        DO i = 1, Nhru
          Imperv_stor(i) = 0.0
          Imperv_evap(i) = 0.0
          Upslope_hortonian(i) = 0.0
          IF ( Ncascade>0 ) Hortonian_flow(i) = 0.0
        ENDDO
        Basin_sroffi = 0.0D0
        Basin_sroffp = 0.0D0
        IF ( Ncascade>0 ) THEN
          Basin_hortonian_lakes = 0.0
          Basin_hortonian = 0.0
          Basin_sroff_upslope = 0.0
          Basin_sroff_farflow = 0.0
          Basin_sroff_down = 0.0
        ENDIF
      ENDIF

! Smidx parameters
      IF ( getparam('srunoff', 'smidx_coef', Nhru, 'real', Smidx_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('srunoff', 'smidx_exp', Nhru, 'real', Smidx_exp)
     +     .NE.0 ) RETURN


      IF ( Print_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='srunoff_smidx_casc.wbal')
        IF ( Ncascade>0 ) THEN
          WRITE (BALUNT, 9002)
        ELSE
          WRITE (BALUNT, 9001)
        ENDIF
      ENDIF

      srosmcainit = 0

 9001 FORMAT ('    Date     Water Bal     Robal      Sroff',
     +        '  Infiltrat  Impervevap Impervstor')
 9002 FORMAT ('    Date     Water Bal     Robal      Sroff   Sroffdown',
     +        '  Srofflake  Infiltrat Impervevap Impervstor    Farflow')

      END FUNCTION srosmcainit

!***********************************************************************
!     srosmcarun - Computes surface runoff using contributing area
!                  computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srosmcarun()
      USE PRMS_SRUNOFF_SMIDX_CASC
      USE PRMS_MODULE, ONLY: Ncascade
      USE PRMS_BASIN, ONLY: Print_debug, Active_hrus, Hru_route_order,
     +    Hru_perv, Hru_imperv, Hru_percent_impv, Hru_percent_perv,
     +    NEARZERO, Hru_area, Hru_type, Basin_area_inv, Nhru
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Hru_impervevap,
     +    Sroff, Basin_sroff, Infil, Carea_max, Snowinfil_max,
     +    Hru_impervstor, Basin_imperv_evap, Hortonian_lakes,
     +    Basin_imperv_stor, Basin_infil, Hru_hortonian_cascadeflow,
     +    Strm_seg_in, Strm_farfield, Imperv_stor_max
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_INTCP, ONLY: Hru_intcpevap, Net_rain, Net_snow, Net_ppt
      USE PRMS_SNOW, ONLY: Pkwater_equiv, Pk_depth, Pptmix_nopack,
     +    Snow_evap, Snowcov_area, Snowmelt
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getvar
      EXTERNAL imperv_et, compute_infil_smcas, run_cascade_smidx
      EXTERNAL perv_sroff_smidx_smcas
! Local Variables
      INTEGER :: i, k
      REAL :: hru_sroff_down, harea, farflow, avail_et
      REAL :: basin_robal, robal, srunoff
      REAL :: last_stor, himperv, hperv, runoff
!***********************************************************************
      srosmcarun = 1

      IF ( getvar('smbal', 'soil_moist', Nhru, 'real', Soil_moist)
     +     .NE.0 ) RETURN

      IF ( getvar('smbal', 'soil_rechr', Nhru, 'real', Soil_rechr)
     +     .NE.0 ) RETURN

      Basin_sroffi = 0.0D0
      Basin_sroffp = 0.0D0
      basin_robal = 0.0
      Basin_sroff = 0.
      Basin_infil = 0.
      Basin_imperv_evap = 0.
      Basin_imperv_stor = 0.

      IF ( Ncascade>0 ) THEN
        Basin_sroff_down = 0.
        Basin_sroff_upslope = 0.
        Basin_hortonian = 0.
        Basin_hortonian_lakes = 0.
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
          last_stor = Imperv_stor(i)
          hperv = Hru_perv(i)
          himperv = Hru_imperv(i)
          Srp = 0.
          Sri = 0.

!******Compute runoff for pervious and impervious
          CALL compute_infil_smcas(Pptmix_nopack(i), Smidx_coef(i),
     +         Smidx_exp(i), Soil_moist(i), Soil_moist_max(i),
     +         Carea_max(i), Net_rain(i), Net_ppt(i), himperv,
     +         Imperv_stor(i), Imperv_stor_max(i), Snowmelt(i),
     +         Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i),
     +         Infil(i), Hru_type(i), Upslope_hortonian(i))

          avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)

! There must be some pervious area for this HRU
          runoff = Srp*hperv + Sri*himperv
          srunoff = runoff/harea

!******Compute HRU weighted average (to units of inches/dt)
          hru_sroff_down = 0.0
          farflow = 0.0

          IF ( Hru_type(i)==1 ) THEN
            IF ( Ncascade>0 ) THEN
              IF ( Ncascade_hru(i)>0 .AND. srunoff>0.0 )
     +             CALL run_cascade_smidx(i, Ncascade_hru(i), srunoff,
     +                  hru_sroff_down, farflow)
              Hru_hortonian_cascadeflow(i) = hru_sroff_down + farflow
              Hortonian_flow(i) = srunoff
              Basin_sroff_upslope = Basin_sroff_upslope +
     +                              Upslope_hortonian(i)*harea
              Basin_sroff_down = Basin_sroff_down +
     +                           hru_sroff_down*harea
              Basin_sroff_farflow = Basin_sroff_farflow + farflow*harea
              Basin_hortonian = Basin_hortonian +
     +                          Hortonian_flow(i)*harea
            ENDIF
            Basin_sroff = Basin_sroff + srunoff*harea

          ENDIF

          Basin_infil = Basin_infil + Infil(i)*hperv

!******Compute evaporation from impervious area

          IF ( himperv.GT.NEARZERO ) THEN
            IF ( Imperv_stor(i)>0.0 ) THEN
              CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i),
     +             Snowcov_area(i))
              Hru_impervevap(i) = Imperv_evap(i)*Hru_percent_impv(i)
              avail_et = avail_et - Hru_impervevap(i)
              IF ( avail_et<0.0D0 ) THEN
                !rsr, sanity check
!               IF ( avail_et<-1.0E-5 )
                  PRINT*, 'avail_et<0 in srunoff imperv', i, Nowmonth,
     +                     Nowday, avail_et
                Hru_impervevap(i) = Hru_impervevap(i) + avail_et
                IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
                Imperv_evap(i) = Hru_impervevap(i)/Hru_percent_impv(i)
                Imperv_stor(i) = Imperv_stor(i)
     +                           - avail_et/Hru_percent_impv(i)
              ENDIF
            ELSE
              Imperv_evap(i) = 0.0
              Hru_impervevap(i) = 0.0
            ENDIF
            Basin_imperv_evap = Basin_imperv_evap +
     +                          Hru_impervevap(i)*harea
            Hru_impervstor(i) = Imperv_stor(i)*Hru_percent_impv(i)
            Basin_imperv_stor = Basin_imperv_stor +
     +                          Imperv_stor(i)*himperv
          ENDIF
          Basin_sroffp = Basin_sroffp + Srp*Hru_perv(i)
          Basin_sroffi = Basin_sroffi + Sri*himperv
          IF ( Print_debug.EQ.1 ) THEN
            robal = Snowmelt(i) - srunoff ! includes dprst runoff
     +              - Infil(i)*Hru_percent_perv(i) - Hru_impervevap(i)
     +              + (last_stor-Imperv_stor(i))*Hru_percent_impv(i)
            IF ( Net_ppt(i)>0.0 ) THEN
              IF ( Pptmix_nopack(i).EQ.1 ) THEN
                robal = robal + Net_rain(i)
              ELSEIF ( Snowmelt(i)<NEARZERO .AND.
     +                 Snow_evap(i)<NEARZERO .AND.
     +                 Pkwater_equiv(i)<NEARZERO) THEN
                robal = robal + Net_ppt(i)
              ELSEIF ( Snowmelt(i)<NEARZERO .AND.
     +                 Snow_evap(i)>0.0 .AND.
     +                 Pkwater_equiv(i)<NEARZERO .AND.
     +                 Net_snow(i)<NEARZERO ) THEN
                robal = robal + Net_ppt(i)
              ENDIF
            ENDIF
            IF ( Ncascade>0 ) robal = robal + Upslope_hortonian(i)
     +                                - hru_sroff_down - farflow

            basin_robal = basin_robal + robal
            IF ( ABS(robal)>1.0D-06 ) THEN
              
              IF ( ABS(robal)>1.0D-4 ) THEN
                WRITE (BALUNT, *) 'possible HRU water balance error'
              ELSE
                WRITE (BALUNT, *) 'HRU robal rounding issue'
              ENDIF
              IF ( Ncascade>0 ) THEN
                WRITE (BALUNT, '(2I3,I6,18F10.6,I3)') Nowmonth,
     +                 Nowday, i, robal, Snowmelt(i),
     +                 Upslope_hortonian(i), last_stor,
     +                 hru_sroff_down, Infil(i), srunoff,
     +                 Imperv_stor(i), Imperv_evap(i), Net_ppt(i),
     +                 Pkwater_equiv(i),  Snow_evap(i), Net_snow(i),
     +                 farflow,Net_rain(i), Srp, Sri, runoff,
     +                 Pptmix_nopack(i)
              ELSE
                WRITE (BALUNT,'(2I3,I4,14F10.7,I5)') Nowmonth, Nowday,
     +                 i, robal, Snowmelt(i), last_stor, Infil(i),
     +                 srunoff, Imperv_stor(i), Imperv_evap(i),
     +                 Hru_impervevap(i), Hru_percent_impv(i),
     +                 Net_ppt(i), Pkwater_equiv(i), Snow_evap(i),
     +                 Net_snow(i), Net_rain(i), Pptmix_nopack(i)
              ENDIF
            ENDIF
          ENDIF
          Sroff(i) = srunoff
        ELSE
! HRU is a lake
!rsr, eventually add code for lake area less than hru_area
!     that includes soil_moist for percent of hru_area that is dry bank
          ! Sanity check
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i).GT.0.0 )
     +         PRINT *, 'smidx lake error', Infil(i), Sroff(i),
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
      Basin_sroffp = Basin_sroffp*Basin_area_inv
      Basin_sroffi = Basin_sroffi*Basin_area_inv
      IF ( Ncascade>0 ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_farflow = Basin_sroff_farflow*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
        Basin_hortonian = Basin_hortonian*Basin_area_inv
      ENDIF

      IF ( Print_debug.EQ.1 ) THEN
        robal = Basin_sroff - Basin_sroffp - Basin_sroffi
        IF ( Ncascade>0 ) THEN
          robal = robal + Basin_sroff_down + Basin_sroff_farflow
          WRITE (BALUNT, 9001) Nowyear, Nowmonth, Nowday, basin_robal,
     +           robal, Basin_sroff, Basin_sroff_down,
     +           Basin_hortonian_lakes, Basin_infil, Basin_imperv_evap,
     +           Basin_imperv_stor, Basin_sroff_farflow
        ELSE
          WRITE (BALUNT, 9001) Nowyear, Nowmonth, Nowday, basin_robal,
     +           robal, Basin_sroff, Basin_infil, Basin_imperv_evap,
     +           Basin_imperv_stor, Basin_sroffp, Basin_sroffi
        ENDIF
        IF ( ABS(basin_robal).GT.2.0E-4 ) THEN
          WRITE (BALUNT, *) 'possible basin water balance error'
        ELSEIF ( ABS(basin_robal).GT.2.0E-5 ) THEN
          WRITE (BALUNT, *) 'basin_robal rounding issue'
        ENDIF
      ENDIF

      srosmcarun = 0

 9001 FORMAT (I5, 2('/', I2.2), 10F11.5)

      END FUNCTION srosmcarun

!***********************************************************************
!      Subroutine to compute evaporation from impervious area
!***********************************************************************
      SUBROUTINE imperv_et(Imperv_stor, Potet, Imperv_evap, Sca)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Potet, Sca
      REAL, INTENT(INOUT) :: Imperv_stor
      REAL, INTENT(OUT) :: Imperv_evap
!***********************************************************************
      IF ( Sca.LT.1.0 ) THEN
        IF ( Potet<Imperv_stor ) THEN
          Imperv_evap = Potet*(1.0-Sca)
        ELSE
          Imperv_evap = Imperv_stor*(1.0-Sca)
        ENDIF
        Imperv_stor = Imperv_stor - Imperv_evap
      ELSE
        Imperv_evap = 0.0
      ENDIF
      !rsr, sanity check
      IF ( Imperv_stor<0.0 ) Imperv_stor = 0.0

      END SUBROUTINE imperv_et

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil_smcas(Pptmix_nopack, Smidx_coef,
     +           Smidx_exp, Soil_moist, Soil_moist_max, Carea_max,
     +           Net_rain, Net_ppt, Hru_imperv, Imperv_stor,
     +           Imperv_stor_max, Snowmelt, Snowinfil_max, Net_snow,
     +           Pkwater_equiv, Infil, Hru_type, Upslope_hortonian)
      USE PRMS_SRUNOFF_SMIDX_CASC, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_sroff, perv_imperv_comp_smcas, check_capacity
! Arguments
      INTEGER, INTENT(IN) :: Pptmix_nopack, Hru_type
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Carea_max
      REAL, INTENT(IN) :: Soil_moist_max, Soil_moist, Net_rain, Net_ppt
      REAL, INTENT(IN) :: Hru_imperv, Imperv_stor_max
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow
      REAL, INTENT(IN) :: Pkwater_equiv, Upslope_hortonian
      REAL, INTENT(INOUT) :: Imperv_stor, Infil
! Local Variables
      REAL :: ppti, pptp, ptc, snri
!***********************************************************************
! compute runoff from cascading Hortonian flow
      IF ( Upslope_hortonian.GT.0.0 ) THEN
        ptc = Upslope_hortonian
        pptp = Upslope_hortonian
        ppti = Upslope_hortonian
        CALL perv_imperv_comp_smcas(ptc, pptp, ppti, Hru_imperv,
     +       Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +       Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack.EQ.1 ) THEN
        ptc = Net_rain
        pptp = Net_rain
        ppti = Net_rain
        CALL perv_imperv_comp_smcas(ptc, pptp, ppti, Hru_imperv,
     +       Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +       Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      ENDIF

!******If precip on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt.GT.0.0 ) THEN

        IF ( Pkwater_equiv>NEARZERO .OR.
     +       ABS(Net_ppt-Net_snow)<NEARZERO ) THEN

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

          ptc = Net_ppt
          pptp = Snowmelt
          ppti = Snowmelt

          CALL perv_imperv_comp_smcas(ptc, pptp, ppti, Hru_imperv,
     +         Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +         Imperv_stor_max, Imperv_stor, Infil, Hru_type)

        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( Pkwater_equiv.LT.NEARZERO ) THEN

!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.

        IF ( Net_snow.LT.NEARZERO .AND. Net_rain>0.0 ) THEN
! no snow, some rain
          ptc = Net_rain
          pptp = Net_rain
          ppti = Net_rain

          CALL perv_imperv_comp_smcas(ptc, pptp, ppti, Hru_imperv,
     +         Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +         Imperv_stor_max, Imperv_stor, Infil, Hru_type)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.
      ELSEIF ( Infil.GT.0.0 .AND. Hru_type==1 ) THEN
        CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max,
     +       Srp, Infil)

      ENDIF

      END SUBROUTINE compute_infil_smcas

!***********************************************************************
!***********************************************************************
      SUBROUTINE perv_imperv_comp_smcas(Ptc, Pptp, Ppti, Hru_imperv,
     +           Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +           Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      USE PRMS_SRUNOFF_SMIDX_CASC, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY:NEARZERO
      IMPLICIT NONE
      EXTERNAL perv_sroff_smidx_smcas, imperv_sroff
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Ptc, Pptp, Ppti, Hru_imperv
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Soil_moist, Carea_max
      REAL, INTENT(IN) :: Imperv_stor_max
      REAL, INTENT(INOUT) :: Infil, Imperv_stor
! Local Variables
      REAL :: inp, snrp, snri
!***********************************************************************
!******Pervious area computations
      IF ( Pptp.GT.0.0 ) THEN
        CALL perv_sroff_smidx_smcas(Smidx_coef, Smidx_exp, Soil_moist,
     +       Carea_max, Pptp, Ptc, inp, snrp, Hru_type)
        Infil = Infil + inp
        Srp = Srp + snrp
        IF ( Srp<0.0D0 ) Srp = 0.0D0
      ENDIF

!******Impervious area computations
      IF ( Ppti.GT.0.0 .AND. Hru_imperv.GT.NEARZERO ) THEN
        CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, snri,
     +       Hru_type)
        Sri = Sri + snri
      ENDIF

      END SUBROUTINE perv_imperv_comp_smcas

!***********************************************************************
!      Subroutine to compute runoff from pervious area using non-linear
!      contributing area computations
!***********************************************************************

      SUBROUTINE perv_sroff_smidx_smcas(Smidx_coef, Smidx_exp,
     +           Soil_moist, Carea_max, Pptp, Ptc, Infil, Srp, Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Soil_moist, Pptp, Ptc
      REAL, INTENT(IN) :: Carea_max
      REAL, INTENT(OUT) :: Infil, Srp
! Local Variables
      REAL :: smidx, ca_percent
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        smidx = Soil_moist + (.5*Ptc)
        ca_percent = Smidx_coef*10.**(Smidx_exp*smidx)
        IF ( ca_percent.GT.Carea_max ) ca_percent = Carea_max
        Srp = ca_percent*Pptp
        IF ( Srp<0.0 ) Srp = 0.0
        Infil = Pptp - Srp
      ELSE ! Hru_type=3
        Srp = 0.0
        Infil = Pptp
      ENDIF

      END SUBROUTINE perv_sroff_smidx_smcas

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_smidx(Ihru, Ncascade_hru, Runoff,
     +           Hru_sroff_down, Farflow)
      USE PRMS_SRUNOFF_SMIDX_CASC, ONLY: Upslope_hortonian
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
      IF ( Runoff<0.0D0 ) THEN
        IF ( Hru_sroff_down>ABS(Runoff) ) THEN
          Hru_sroff_down = Hru_sroff_down - Runoff
        ELSE
          DO k = 1, Ncascade_hru
            j = Hru_down(k, Ihru)
            IF ( Strm_seg_in(j)>ABS(Runoff) ) THEN
              Strm_seg_in(j) = Strm_seg_in(j) - Runoff
              EXIT
            ENDIF
          ENDDO
        ENDIF
        Runoff = 0.0D0
      ENDIF

      END SUBROUTINE run_cascade_smidx

!***********************************************************************
!      Subroutine to compute runoff from impervious area
!***********************************************************************
      SUBROUTINE imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, Sri,
     +           Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Imperv_stor_max, Ppti
      REAL, INTENT(INOUT) :: Imperv_stor
      REAL, INTENT(OUT) :: Sri
! Local Variables
      REAL :: avail_stor
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        avail_stor = Imperv_stor_max - Imperv_stor
        IF ( Ppti.GT.avail_stor ) THEN
          Imperv_stor = Imperv_stor_max
          Sri = Ppti - avail_stor
        ELSE
          Imperv_stor = Imperv_stor + Ppti
          Sri = 0.0
        ENDIF
      ELSE ! Hru_type=3
        Imperv_stor = Imperv_stor + Ppti
        Sri = 0.0
      ENDIF

      END SUBROUTINE imperv_sroff

!***********************************************************************
! fill soil to soil_moist_max, if more than capacity restrict
! infiltration by snowinfil_max, with excess added to runoff
!***********************************************************************
      SUBROUTINE check_capacity(Soil_moist_max, Soil_moist,
     +           Snowinfil_max, Srp, Infil)
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

      END SUBROUTINE check_capacity

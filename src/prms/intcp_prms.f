!***********************************************************************
! Computes volume of intercepted precipitation, evaporation from
! intercepted precipitation, and throughfall that reaches the soil or
! snowpack
!***********************************************************************
      MODULE PRMS_INTCP
      IMPLICIT NONE
      INTEGER, PARAMETER :: BALUNT = 198
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: Intcp_transp_on(:)
      DOUBLE PRECISION, SAVE :: Basin_changeover
!   Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Intcp_on(:), Intcp_form(:)
      REAL, SAVE :: Basin_net_ppt, Basin_intcp_stor, Basin_intcp_evap
      REAL, SAVE :: Last_intcp_stor
      REAL, SAVE, ALLOCATABLE :: Net_rain(:), Net_snow(:), Net_ppt(:)
      REAL, SAVE, ALLOCATABLE :: Intcp_stor(:), Intcp_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_intcpevap(:), Hru_intcpstor(:)
!   Declared Variables from other modules - snow
      REAL, ALLOCATABLE :: Pkwater_equiv(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Cov_type(:)
      REAL, SAVE :: Potet_sublim
      REAL, SAVE, ALLOCATABLE :: Covden_sum(:), Covden_win(:)
      REAL, SAVE, ALLOCATABLE :: Snow_intcp(:), Srain_intcp(:)
      REAL, SAVE, ALLOCATABLE :: Epan_coef(:), Wrain_intcp(:)
      END MODULE PRMS_INTCP

!***********************************************************************
!     Main intcp routine
!***********************************************************************
      INTEGER FUNCTION intcp_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: intdecl, intinit, intrun
!***********************************************************************
      intcp_prms = 0

      IF ( Process_flag==0 ) THEN
        intcp_prms = intrun()
      ELSEIF ( Process_flag==1 ) THEN
        intcp_prms = intdecl()
      ELSEIF ( Process_flag==2 ) THEN
        intcp_prms = intinit()
      ENDIF

      END FUNCTION intcp_prms

!***********************************************************************
!     intdecl - set up parameters for interception computations
!   Declared Parameters
!     snow_intcp, srain_intcp, wrain_intcp, potet_sublim, cov_type
!     covden_win, covden_sum, epan_coef, hru_area
!***********************************************************************
      INTEGER FUNCTION intdecl()
      USE PRMS_INTCP
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
!***********************************************************************
      intdecl = 1

      IF ( declmodule(
     +'$Id: intcp_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

      ALLOCATE (Net_rain(Nhru))
      IF ( declvar('intcp', 'net_rain', 'nhru', Nhru, 'real',
     +     'hru_rain minus interception',
     +     'inches',
     +     Net_rain).NE.0 ) RETURN

      ALLOCATE (Net_snow(Nhru))
      IF ( declvar('intcp', 'net_snow', 'nhru', Nhru, 'real',
     +     'hru_snow minus interception',
     +     'inches',
     +     Net_snow).NE.0 ) RETURN

      ALLOCATE (Net_ppt(Nhru))
      IF ( declvar('intcp', 'net_ppt', 'nhru', Nhru, 'real',
     +     'HRU precipitation (rain and/or snow) with'//
     +     ' interception removed',
     +     'inches',
     +     Net_ppt).NE.0 ) RETURN

      IF ( declvar('intcp', 'basin_net_ppt', 'one', 1, 'real',
     +     'Basin area-weighted average net_ppt',
     +     'inches',
     +     Basin_net_ppt).NE.0 ) RETURN

      ALLOCATE (Intcp_stor(Nhru))
      IF ( declvar('intcp', 'intcp_stor', 'nhru', Nhru, 'real',
     +     'Current interception storage on each HRU',
     +     'inches',
     +     Intcp_stor).NE.0 ) RETURN

      IF ( declvar('intcp', 'last_intcp_stor', 'one', 1, 'real',
     +     'Basin area-weighted average changeover interception',
     +     'inches',
     +     Last_intcp_stor).NE.0 ) RETURN

      IF ( declvar('intcp', 'basin_intcp_stor', 'one', 1, 'real',
     +     'Basin area-weighted average interception storage',
     +     'inches',
     +     Basin_intcp_stor).NE.0 ) RETURN

      ALLOCATE (Intcp_evap(Nhru))
      IF ( declvar('intcp', 'intcp_evap', 'nhru', Nhru, 'real',
     +     'Evaporation from interception on canopy of each HRU',
     +     'inches',
     +     Intcp_evap).NE.0 ) RETURN

      IF ( declvar('intcp', 'basin_intcp_evap', 'one', 1, 'real',
     +     'Basin area-weighted evaporation from interception',
     +     'inches',
     +     Basin_intcp_evap).NE.0 ) RETURN

      ALLOCATE (Hru_intcpevap(Nhru))
      IF ( declvar('intcp', 'hru_intcpevap', 'nhru', Nhru, 'real',
     +     'Evaporation from interception on each HRU',
     +     'inches',
     +     Hru_intcpevap).NE.0 ) RETURN

      ALLOCATE (Hru_intcpstor(Nhru))
      IF ( declvar('intcp', 'hru_intcpstor', 'nhru', Nhru, 'real',
     +     'Storage in canopy on each HRU',
     +     'inches',
     +     Hru_intcpstor).NE.0 ) RETURN

      ALLOCATE (Intcp_form(Nhru))
      IF ( declvar('intcp', 'intcp_form', 'nhru', Nhru, 'integer',
     +     'Form (rain or snow) of interception',
     +     'none',
     +     Intcp_form).NE.0 ) RETURN

      ALLOCATE (Intcp_on(Nhru))
      IF ( declvar('intcp', 'intcp_on', 'nhru', Nhru, 'integer',
     +     'Whether there is interception in the canopy (0=no;'//
     +     ' 1=yes)',
     +     'none',
     +     Intcp_on).NE.0 ) RETURN

! declare parameters
      ALLOCATE (Epan_coef(12))
      IF ( declparam('intcp', 'epan_coef', 'nmonths', 'real',
     +     '1.0', '0.2', '3.0',
     +     'Evaporation pan coefficient',
     +     'Evaporation pan coefficient',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Snow_intcp(Nhru))
      IF ( declparam('intcp', 'snow_intcp', 'nhru', 'real',
     +     '.1', '0.', '5.',
     +     'Snow interception storage capacity',
     +     'Snow interception storage capacity for the major'//
     +     ' vegetation type in each HRU',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Srain_intcp(Nhru))
      IF ( declparam('intcp', 'srain_intcp', 'nhru', 'real',
     +     '.1', '0.', '5.',
     +     'Summer rain interception storage capacity',
     +     'Summer rain interception storage capacity for the major'//
     +     ' vegetation type in each HRU',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Wrain_intcp(Nhru))
      IF ( declparam('intcp', 'wrain_intcp', 'nhru', 'real',
     +     '.1', '0.', '5.',
     +     'Winter rain interception storage capacity',
     +     'Winter rain interception storage capacity for the major'//
     +     ' vegetation type in the HRU',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Cov_type(Nhru))
      IF ( declparam('intcp', 'cov_type', 'nhru', 'integer',
     +     '3', '0', '3',
     +     'Cover type designation for HRU',
     +     'Vegetation cover type designation for HRU'//
     +     ' (0=bare soil; 1=grasses; 2=shrubs; 3=trees)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Covden_sum(Nhru))
      IF ( declparam('intcp', 'covden_sum', 'nhru', 'real',
     +     '.5', '0.', '1.0',
     +     'Summer vegetation cover density for major vegetation type',
     +     'Summer vegetation cover density for the major'//
     +     ' vegetation type on each HRU',
     +     'decimal fraction').NE.0 ) RETURN

      ALLOCATE (Covden_win(Nhru))
      IF ( declparam('intcp', 'covden_win', 'nhru', 'real',
     +     '.5', '0.', '1.0',
     +     'Winter vegetation cover density for major vegetation type',
     +     'Winter vegetation cover density for the major'//
     +     ' vegetation type on each HRU',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam('intcp', 'potet_sublim', 'one', 'real',
     +   '.5', '.1', '.75',
     +   'Proportion of potential ET that is sublimated from snow'//
     +   ' surface',
     +   'Proportion of potential ET that is sublimated from snow'//
     +   ' surface',
     +   'decimal fraction').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE ( Pkwater_equiv(Nhru) )

      intdecl = 0
      END FUNCTION intdecl

!***********************************************************************
!     intinit - Initialize intcp module - get parameter values,
!               set initial values.
!***********************************************************************
      INTEGER FUNCTION intinit()
      USE PRMS_INTCP
      USE PRMS_BASIN, ONLY: Print_debug, Hru_type, Nhru, Timestep
      USE PRMS_BASIN, ONLY: NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Transp_on
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i
!***********************************************************************
      intinit = 1

      IF ( getparam('intcp', 'snow_intcp', Nhru, 'real', Snow_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'wrain_intcp', Nhru, 'real', Wrain_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'srain_intcp', Nhru, 'real', Srain_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'cov_type', Nhru, 'integer', Cov_type)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'covden_sum', Nhru, 'real', Covden_sum)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'covden_win', Nhru, 'real', Covden_win)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'epan_coef', 12, 'real', Epan_coef)
     +     .NE.0 ) RETURN
      DO i = 1, 12
        IF ( Epan_coef(i)<NEARZERO ) THEN
          PRINT *, 'Warning, epan_coef specified as 0 for month:', i
          PRINT *, '         value changed to 1.0'
          Epan_coef(i) = 1.0
        ENDIF
      ENDDO

      IF ( getparam('intcp', 'potet_sublim', 1, 'real', Potet_sublim)
     +     .NE.0 ) RETURN

      IF ( Print_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='intcp.wbal')
        WRITE (BALUNT, 9001)
      ENDIF

      ALLOCATE (Intcp_transp_on(Nhru))
      DO i = 1, Nhru
        Intcp_transp_on(i) = Transp_on(i)
        IF ( Covden_win(i).GT.Covden_sum(i) ) THEN
          PRINT *, 'Warning, covden_win>covden_sum, HRU:', i,
     +             Covden_win(i), Covden_sum(i)
          PRINT *, ' Set covden_win to covden_sum'
          Covden_win(i) = Covden_sum(i)
        ENDIF
        IF ( Cov_type(i).EQ.0 .AND. (Covden_win(i).GT.0.0 .OR.
     +       Covden_sum(i).GT.0.0) ) THEN
!         PRINT *, 'Warning, cov_type=0 & cov_den not 0. HRU:', i,
!    +             ' winter:', Covden_win(i), ' summer:', Covden_sum(i)
!         PRINT *, 'They are set to 0.0'
          Covden_sum(i) = 0.0
          Covden_win(i) = 0.0
        ENDIF
        IF ( Cov_type(i).NE.0 .AND. Hru_type(i).EQ.2 ) THEN
          IF ( Print_debug==1 )
     +         PRINT *,  'Warning, cov_type must be 0 for lakes,',
     +                ' reset from:', Cov_type(i), ' to 0 for HRU:', i
          Cov_type(i) = 0
          Covden_sum(i) = 0.0
          Covden_win(i) = 0.0
        ENDIF
      ENDDO

      IF ( Timestep==0 ) THEN
        DO i = 1, Nhru
          Intcp_stor(i) = 0.0
          Intcp_on(i) = 0
          Intcp_form(i) = 0
          Intcp_evap(i) = 0.0
          Hru_intcpevap(i) = 0.0
          Hru_intcpstor(i) = 0.0
          Net_rain(i) = 0.0
          Net_snow(i) = 0.0
          Net_ppt(i) = 0.0
        ENDDO
        Basin_net_ppt = 0.0
        Basin_intcp_evap = 0.0
        Basin_intcp_stor = 0.0
        Last_intcp_stor = 0.0
      ENDIF

      intinit = 0

 9001 FORMAT ('    Date     Water Bal     Precip     Netppt  Intcpevap',
     +        '  Intcpstor  last_stor')

      END FUNCTION intinit

!***********************************************************************
!     intrun - Computes and keeps track of intercepted precipitation
!              and evaporation for each HRU
!***********************************************************************
      INTEGER FUNCTION intrun()
      USE PRMS_INTCP
      USE PRMS_BASIN, ONLY: Print_debug, Basin_area_inv, Active_hrus,
     +    Hru_route_order, Hru_type, Hru_area, NEARZERO, Nhru, DNEARZERO
! Newsnow and Pptmix can be modfied, WARNING!!!
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Hru_rain, Hru_ppt,
     +    Hru_snow, Basin_ppt, Transp_on, Potet
      USE PRMS_OBS, ONLY: Pan_evap, Nowtime, Nevap, Nowmonth, Nowday,
     +    Nowyear
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getvar
      EXTERNAL intercept
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, j
      REAL :: stor, cov, evrn, evsn, z, d, intcpevap, harea
      REAL :: diff, avail_et, last, evap
      REAL :: hrubal, delta_stor, delstor, pptbal, intcpstor
      REAL :: basin_last_stor, stor_last, changeover
!***********************************************************************
      intrun = 1

      IF ( getvar('snow', 'pkwater_equiv', Nhru, 'real', Pkwater_equiv)
     +     .NE.0 ) RETURN

      Basin_changeover = 0.0
      basin_last_stor = Basin_intcp_stor
      Basin_net_ppt = 0.
      Basin_intcp_evap = 0.
      Basin_intcp_stor = 0.

      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        harea = Hru_area(i)
        Net_rain(i) = Hru_rain(i)
        Net_snow(i) = Hru_snow(i)
        Net_ppt(i) = Hru_ppt(i)

        ! Lake or bare ground HRUs
        IF ( Hru_type(i).EQ.2 .OR. Cov_type(i).EQ.0 ) THEN
          Basin_net_ppt = Basin_net_ppt + Net_ppt(i)*harea
          CYCLE
        ENDIF

        stor_last = Hru_intcpstor(i)

!******Adjust interception amounts for changes in summer/winter cover
!******density

        IF ( Transp_on(i).EQ.1 ) THEN
          cov = Covden_sum(i)
        ELSE
          cov = Covden_win(i)
        ENDIF
        IF ( cov<NEARZERO ) cov = 0.0
        Intcp_form(i) = 0

        intcpstor = Intcp_stor(i)
        intcpevap = 0.0
        changeover = 0.0

!*****Determine the amount of interception from rain

!***** go from summer to winter cover density
        !rsr, changeovers for whole HRU
        IF ( Transp_on(i).EQ.0 .AND. Intcp_transp_on(i).EQ.1 ) THEN
          Intcp_transp_on(i) = 0
          IF ( intcpstor>0.0 ) THEN
            ! assume canopy storage change falls as throughfall
            diff = Covden_sum(i) - cov
            changeover = intcpstor*diff
            IF ( cov>0.0 ) THEN
              stor_last = stor_last - changeover
            ELSE
              PRINT *, 'covden_win=0 at winter changeover and has',
     +                 ' canopy storage', intcpstor, changeover, i
              stor_last = 0.0
              intcpstor = 0.0
              Intcp_on(i) = 0
            ENDIF
            basin_last_stor = basin_last_stor -
     +                        changeover*harea*Basin_area_inv
            Basin_changeover = Basin_changeover + changeover*harea
          ENDIF

!****** go from winter to summer cover density
        ELSEIF ( Transp_on(i).EQ.1 .AND. Intcp_transp_on(i).EQ.0 ) THEN
          Intcp_transp_on(i) = 1
          IF ( intcpstor>0.0 ) THEN
            diff = Covden_win(i) - cov
            IF ( -intcpstor*diff>NEARZERO ) THEN
              IF ( cov>0.0 ) THEN
                intcpstor = intcpstor*Covden_win(i)/cov
              ELSE
                PRINT *, 'intcp problem', i, intcpstor, Covden_win(i)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        avail_et = Potet(i)

!*****Determine the amount of interception from rain

        IF ( Transp_on(i).EQ.1 ) THEN
          stor = Srain_intcp(i)
        ELSE
          stor = Wrain_intcp(i)
        ENDIF
        IF ( Hru_rain(i).GT.0. .AND. cov>0.0 ) THEN

          IF ( Cov_type(i).GT.1 ) THEN
            CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i),
     +                     intcpstor, Net_rain(i))
          ELSEIF ( Cov_type(i).EQ.1 ) THEN
            !rsr, 03/24/2008 intercept rain on snow-free grass,
            !rsr             when not a mixed event
            IF ( Pkwater_equiv(i)<NEARZERO .AND.
     +           Hru_snow(i)<NEARZERO ) THEN
              CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i),
     +                       intcpstor, Net_rain(i))
              !rsr 03/24/2008
              !it was decided to leave the water in intcpstor rather
              !than put the water in the snowpack, as doing so for a
              !mixed event on grass with snow-free surface produces a
              !divide by zero in snowcomp_prms. Storage on grass will
              !eventually evaporate
            ENDIF
          ENDIF
        ENDIF
        Net_rain(i) = Net_rain(i) + changeover

!******Determine amount of interception from snow

        IF ( Hru_snow(i).GT.0. .AND. cov>0.0 ) THEN
          Intcp_form(i) = 1
          IF ( Cov_type(i).GT.1 ) THEN
            stor = Snow_intcp(i)
            CALL intercept(Hru_snow(i), stor, cov,
     +                     Intcp_on(i), intcpstor, Net_snow(i))
            IF ( Net_snow(i).LT.NEARZERO ) THEN   !rsr, added 3/9/2006
              Net_rain(i) = Net_rain(i) + Net_snow(i)
              Net_snow(i) = 0.0
              Newsnow(i) = 0
              Pptmix(i) = 0   ! reset to be sure it is zero
            ENDIF
          ENDIF
        ENDIF

        Net_ppt(i) = Net_rain(i) + Net_snow(i)

!******compute evaporation or sublimation of interception

        ! if precipitation assume no evaporation or sublimation
        IF ( Hru_ppt(i).LT.NEARZERO ) THEN
          IF ( Intcp_on(i).EQ.1 ) THEN

            evrn = avail_et/Epan_coef(Nowmonth)
            evsn = Potet_sublim*avail_et

            IF ( Nevap.GT.0 ) THEN
              IF ( Pan_evap(1).GT.-NEARZERO ) evrn = Pan_evap(1)
              IF ( evrn<0.0 ) evrn = 0.0
            ENDIF

!******Compute snow interception loss

            IF ( Intcp_form(i).EQ.1 ) THEN
              IF ( Basin_ppt.LT.NEARZERO ) THEN
                z = intcpstor - evsn
                IF ( z.GT.0.0 ) THEN
                  Intcp_on(i) = 1
                  intcpstor = z
                  intcpevap = evsn
                ELSE
                  intcpevap = intcpstor
                  intcpstor = 0.0
                  Intcp_on(i) = 0
                ENDIF
              ENDIF
!           ELSEIF ( Intcp_form(i).EQ.0 ) THEN
            ELSE
              d = intcpstor - evrn
              IF ( d.GT.0.0 ) THEN
                intcpstor = d
                intcpevap = evrn
                Intcp_on(i) = 1
              ELSE
                intcpevap = intcpstor
                intcpstor = 0.0
                Intcp_on(i) = 0
              ENDIF
            ENDIF
          ENDIF

        ENDIF

        evap = intcpevap*cov
        IF ( evap>avail_et ) THEN
          evap = avail_et
          last = intcpevap
          IF ( cov>0.0 ) THEN
            intcpevap = avail_et/cov
          ELSE
            intcpevap = 0.0
          ENDIF
          intcpstor = intcpstor + last - intcpevap
        ENDIF
        Intcp_evap(i) = intcpevap
        Hru_intcpevap(i) = intcpevap*cov
        Intcp_stor(i) = intcpstor
        Hru_intcpstor(i) = intcpstor*cov

        !rsr, question about depression storage for basin_net_ppt???
        !     my assumption is that cover density is for the whole HRU
        Basin_net_ppt = Basin_net_ppt + Net_ppt(i)*harea
        Basin_intcp_stor = Basin_intcp_stor + Hru_intcpstor(i)*harea
        Basin_intcp_evap = Basin_intcp_evap + Hru_intcpevap(i)*harea

        IF ( Print_debug.EQ.1 ) THEN
          delstor = Hru_intcpstor(i) - stor_last
          hrubal = Hru_rain(i) + Hru_snow(i) - Net_rain(i) - Net_snow(i)
     +             - delstor - Hru_intcpevap(i) + changeover
          IF ( ABS(hrubal)>1.0E-6 ) THEN
            IF ( ABS(hrubal).GT.1.0E-4 ) THEN
              WRITE (BALUNT, *) 'Possible HRU water balance error'
            ELSE
              WRITE (BALUNT, *) 'Interception HRU rounding issue'
            ENDIF
            WRITE (BALUNT,'(7I5,15F10.5)') i, Nowtime, hrubal,
     +            Net_rain(i), Net_snow(i), Hru_rain(i), Hru_snow(i),
     +            intcpstor, stor_last, intcpevap, Srain_intcp(i),
     +            Wrain_intcp(i), Snow_intcp(i), cov, delstor,
     +            Hru_intcpstor(i), changeover
          ENDIF
        ENDIF

      ENDDO

      Basin_net_ppt = Basin_net_ppt*Basin_area_inv
      Basin_intcp_stor = Basin_intcp_stor*Basin_area_inv
      Basin_intcp_evap = Basin_intcp_evap*Basin_area_inv
      Basin_changeover = Basin_changeover*Basin_area_inv
      Last_intcp_stor = Basin_changeover

      IF ( Print_debug.EQ.1 ) THEN
        delta_stor = Basin_intcp_stor - basin_last_stor
        pptbal = Basin_ppt - Basin_net_ppt - delta_stor -
     +           Basin_intcp_evap + Basin_changeover
        IF ( ABS(pptbal).GT.1.0E-4 ) THEN
          WRITE (BALUNT, *) 'Possible basin water balance error', pptbal
        ELSEIF ( ABS(pptbal).GT.1.0E-5 ) THEN
          WRITE (BALUNT, *) 'Interception basin rounding issue', pptbal
        ENDIF
        WRITE (BALUNT, 9001) Nowyear, Nowmonth, Nowday, pptbal,
     +        Basin_ppt, Basin_net_ppt, Basin_intcp_evap,
     +        Basin_intcp_stor, basin_last_stor, Basin_changeover
      ENDIF

      intrun = 0

 9001 FORMAT (I5, 2('/', I2.2), 8F11.5)
      END FUNCTION intrun

!***********************************************************************
!      Subroutine to compute interception of rain or snow
!***********************************************************************
      SUBROUTINE intercept(Precip, Stor_max, Cov, Intcp_on, Intcp_stor,
     +                     Net_precip)
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(OUT) :: Intcp_on
      REAL, INTENT(IN) :: Precip
      REAL, INTENT(IN) :: Stor_max, Cov
      REAL, INTENT(INOUT) :: Intcp_stor
      REAL, INTENT(OUT) :: Net_precip
! Local Variables
      REAL :: avail_stor
!***********************************************************************
      Intcp_on = 1

!rsr Note: avail_stor can be negative when wrain_intcp < snow_intcp
!          for mixed precipitation event
      avail_stor = Stor_max - Intcp_stor

      IF ( avail_stor<NEARZERO .OR. Precip>avail_stor ) THEN
        Intcp_stor = Stor_max
        Net_precip = Precip*(1.0-Cov) + (Precip-avail_stor)*Cov
      ELSE
        Intcp_stor = Intcp_stor + Precip
        Net_precip = Precip*(1.0-Cov)
      ENDIF

      END SUBROUTINE intercept

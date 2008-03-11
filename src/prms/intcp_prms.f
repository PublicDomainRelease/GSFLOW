!***********************************************************************
! Computes amount of intercepted rain and snow, and determines
! evaporation from interception
!***********************************************************************
      MODULE PRMS_INTCP
      IMPLICIT NONE
      INTEGER, PARAMETER :: BALUNT = 198
      REAL, PARAMETER :: NEARZERO = 1.0E-15
!   Local Variables
      INTEGER :: Nhru, Nevap
      INTEGER, ALLOCATABLE :: Intcp_transp_on(:)
!   Declared Variables
      INTEGER, ALLOCATABLE :: Intcp_on(:), Intcp_form(:)
!     INTEGER, ALLOCATABLE :: Int_snow(:)
      REAL :: Basin_net_ppt, Basin_intcp_stor, Basin_intcp_evap
      REAL, ALLOCATABLE :: Net_rain(:), Net_snow(:), Net_ppt(:)
      REAL, ALLOCATABLE :: Intcp_stor(:), Intcp_evap(:)
      REAL, ALLOCATABLE :: Hru_intcpevap(:), Hru_intcpstor(:)
!   Declared Variables from other modules - precip
! Newsnow and Pptmix can be modfied and uses putvar, WARNING!!!
      REAL :: Basin_ppt
      INTEGER, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      REAL, ALLOCATABLE :: Hru_rain(:), Hru_snow(:), Hru_ppt(:)
!   Declared Variables from other modules - potet
      INTEGER, ALLOCATABLE :: Transp_on(:)
      REAL, ALLOCATABLE :: Potet(:)
!   Declared Variables from other modules - obs
      REAL, ALLOCATABLE :: Pan_evap(:)
!   Declared Variables from other modules - snow
! Pkwater_equiv can be modfied and uses putvar, WARNING!!!
      REAL, ALLOCATABLE :: Pkwater_equiv(:)
!   Declared Variables from other modules - basin
      INTEGER :: Prt_debug, Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL :: Basin_area_inv
!   Declared Parameters
      INTEGER, ALLOCATABLE :: Cov_type(:), Hru_type(:)
      REAL :: Potet_sublim
      REAL, ALLOCATABLE :: Covden_sum(:), Covden_win(:), Epan_coef(:)
      REAL, ALLOCATABLE :: Snow_intcp(:), Srain_intcp(:), Wrain_intcp(:)
      REAL, ALLOCATABLE :: Hru_area(:)
      END MODULE PRMS_INTCP

!***********************************************************************
!     Main intcp routine
!***********************************************************************
      INTEGER FUNCTION intcp_prms(Arg)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: intdecl, intinit, intrun
!***********************************************************************
      intcp_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        intcp_prms = intrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        intcp_prms = intdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        intcp_prms = intinit()
      ENDIF

      END FUNCTION intcp_prms

!***********************************************************************
!     intdecl - set up parameters for interception computations
!   Declared Parameters
!     snow_intcp, srain_intcp, wrain_intcp, potet_sublim, cov_type
!     covden_win, covden_sum, epan_coef, hru_area, hru_type
!***********************************************************************
      INTEGER FUNCTION intdecl()
      USE PRMS_INTCP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      intdecl = 1

      IF ( declmodule(
     +'$Id: intcp_prms.f 3917 2008-02-29 19:19:35Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN
      Nevap = getdim('nevap')
      IF ( Nevap.EQ.-1 ) RETURN

      ALLOCATE (Intcp_transp_on(Nhru))
      IF ( declpri('intcp_intcp_transp_on', Nhru, 'integer',
     +     Intcp_transp_on).NE.0 ) RETURN

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

!rsr 1/12/05 int_snow is not used currently, to be implemented later
!     ALLOCATE (Int_snow(Nhru))
!     IF ( decl var('intcp', 'int_snow', 'nhru', Nhru, 'integer',
!    +     'Whether snow has fallen from the canopy (0=no;'//
!    +     ' 1=yes)',
!    +     'none',
!    + Int_snow).NE.0 ) RETURN

! declare parameters
      ALLOCATE (Epan_coef(MAXMO))
      IF ( declparam('intcp', 'epan_coef', 'nmonths', 'real',
     +     '1.0', '0.2', '3.0',
     +     'Evaporation pan coefficient',
     +     'Evaporation pan coefficient',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_area(Nhru))
      IF ( declparam('intcp', 'hru_area', 'nhru', 'real',
     +     '1.0', '0.01', '1e+09',
     +     'HRU area', 'Area of each HRU',
     +     'acres').NE.0 ) RETURN

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

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('intcp', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '2',
     +     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     +     'none').NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Transp_on(Nhru), Potet(Nhru), Hru_route_order(Nhru))
      ALLOCATE (Hru_rain(Nhru), Hru_snow(Nhru), Hru_ppt(Nhru))
      ALLOCATE (Pkwater_equiv(Nhru), Newsnow(Nhru), Pptmix(Nhru))
      IF ( Nevap.GT.0 ) ALLOCATE (Pan_evap(Nevap))

      intdecl = 0
      END FUNCTION intdecl

!***********************************************************************
!     intinit - Initialize intcp module - get parameter values,
!               set initial values.
!***********************************************************************
      INTEGER FUNCTION intinit()
      USE PRMS_INTCP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
! Local Variables
      INTEGER :: i, j, nstep
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

      IF ( getparam('intcp', 'epan_coef', MAXMO, 'real', Epan_coef)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'hru_area', Nhru, 'real', Hru_area)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'basin_area_inv', 1, 'real', Basin_area_inv)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'potet_sublim', 1, 'real', Potet_sublim)
     +     .NE.0 ) RETURN

      IF ( getparam('intcp', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'prt_debug', 1, 'integer', Prt_debug)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      nstep = getstep()
      IF ( nstep.EQ.0 ) THEN
        Intcp_stor = 0.0
        Intcp_on = 0
        Intcp_form = 0
        Intcp_evap = 0.0
        Hru_intcpevap = 0.0
        Hru_intcpstor = 0.0
        Intcp_transp_on = 0
!       Int_snow = 0
        Net_rain = 0.0
        Net_snow = 0.0
        Net_ppt = 0.0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Covden_win(i).GT.Covden_sum(i) ) PRINT *,
     +         'Warning, covden_win>covden_sum, HRU:', i
          IF ( Cov_type(i).EQ.0 .AND. (Covden_win(i).GT.0.0 .OR.
     +         Covden_sum(i).GT.0.0) ) PRINT *,
     +         'Warning, cov_type=0 & cov_den not 0. HRU:', i,
     +         ' winter:', Covden_win(i), ' summer:', Covden_sum(i)
          IF ( Cov_type(i).NE.0 .AND. Hru_type(i).EQ.2 ) THEN
            PRINT *, 'Error, cov_type/=0 & hru_type=2. HRU:', i
            RETURN
          ENDIF
        ENDDO
        Basin_net_ppt = 0.0
        Basin_intcp_evap = 0.0
        Basin_intcp_stor = 0.0
      ENDIF

      Intcp_transp_on(1) = -1

      IF ( Prt_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='intcp.wbal')
        WRITE (BALUNT, 9001)
      ENDIF

      intinit = 0

 9001 FORMAT ('    Date     Water Bal     Precip     Netppt  Intcpevap',
     +        '  Intcpstor   Snowstor  delstorck     pptck  sumintlast',
     +        '  last_stor')

      END FUNCTION intinit

!***********************************************************************
!     intrun - Computes and keeps track of intercepted precipitation
!              and evaporation for each HRU
!***********************************************************************
      INTEGER FUNCTION intrun()
      USE PRMS_INTCP
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
      EXTERNAL intercept
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, j, mo, day, iputpkflg, iputnsflg, nowtime(6)
      REAL :: stor, cov, evrn, evsn, z, d, intcpevap, intcpstor, harea
      REAL :: evap_changeover
      DOUBLE PRECISION :: dt
      REAL :: last_stor, pptck, delstorck, sumintlast, basin_snowstor
      REAL :: hrubal, delta_stor, snowstor, delstor, pptbal, int_last
!***********************************************************************
      iputpkflg = 0
      iputnsflg = 0
      intrun = 1

      IF ( getvar('precip', 'hru_rain', Nhru, 'real', Hru_rain)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'hru_snow', Nhru, 'real', Hru_snow)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'hru_ppt', Nhru, 'real', Hru_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'basin_ppt', 1, 'real', Basin_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('potet', 'transp_on', Nhru, 'integer', Transp_on)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'newsnow', Nhru, 'integer', Newsnow)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'pptmix', Nhru, 'integer', Pptmix)
     +     .NE.0 ) RETURN

      IF ( Nevap.GT.0 ) THEN
        IF ( getvar('obs', 'pan_evap', Nevap, 'real', Pan_evap)
     +       .NE.0 ) RETURN
      ENDIF

      IF ( getvar('potet', 'potet', Nhru, 'real', Potet)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'pkwater_equiv', Nhru, 'real', Pkwater_equiv)
     +     .NE.0 ) RETURN



      CALL dattim('now', nowtime)
      mo = nowtime(2)
      day = nowtime(3)

      dt = deltim()

      IF ( Intcp_transp_on(1).EQ.-1 ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Intcp_transp_on(i) = Transp_on(i)
        ENDDO
      ENDIF

      IF ( Prt_debug.EQ.1 ) THEN
        last_stor = Basin_intcp_stor
        pptck = 0.
        delstorck = 0.
        sumintlast = 0.
        basin_snowstor = 0.
      ENDIF
      Basin_net_ppt = 0.
      Basin_intcp_evap = 0.
      Basin_intcp_stor = 0.

      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        harea = Hru_area(i)
        IF ( Hru_type(i).EQ.2 ) THEN   ! Lake HRUs
          Net_rain(i) = Hru_rain(i)
          Net_snow(i) = Hru_snow(i)
          Net_ppt(i) = Hru_ppt(i)
          Basin_net_ppt = Basin_net_ppt + Net_ppt(i)*harea
          CYCLE
        ENDIF
        snowstor = 0.0

!******Adjust interception amounts for changes in summer/winter cover
!******density

!rsr 1/12/05 int_snow is not used currently, to be implemented later
!        Int_snow(i) = 0

        int_last = Intcp_stor(i)
        intcpstor = int_last
        intcpevap = 0.0
        evap_changeover = 0.0

        IF ( Transp_on(i).EQ.1 ) THEN
          cov = Covden_sum(i)
        ELSE
          cov = Covden_win(i)
        ENDIF

!*****Determine the amount of interception from rain
        IF ( Cov_type(i).EQ.0 ) THEN
          Net_rain(i) = Hru_rain(i)
          Net_snow(i) = Hru_snow(i)
          Net_ppt(i) = Hru_ppt(i)

        ELSE

!***** go from summer to winter cover density
          !rsr, evap_changeover and int_last are volumes
          IF ( Transp_on(i).EQ.0 .AND. Intcp_transp_on(i).EQ.1 ) THEN
            Intcp_transp_on(i) = 0
            IF ( intcpstor.GT.0. ) THEN
              evap_changeover = intcpstor*(Covden_sum(i)-cov)
              IF ( evap_changeover.LT.0.0 ) evap_changeover = 0.0
              int_last = int_last*Covden_sum(i)
              intcpstor = int_last - evap_changeover
              IF ( intcpstor.LT.NEARZERO ) THEN
                intcpstor = 0.0
                Intcp_on(i) = 0
              ENDIF
              IF ( cov.LT.NEARZERO ) THEN
                evap_changeover = evap_changeover + intcpstor
                intcpstor = 0.0
                Intcp_on(i) = 0
              ELSE
                intcpstor = intcpstor/cov     !covert back to depth
              ENDIF
            ENDIF

!****** go from winter to summer cover density
          ELSEIF ( Transp_on(i).EQ.1 .AND. Intcp_transp_on(i).EQ.0 )
     +             THEN
            Intcp_transp_on(i) = 1
            IF ( intcpstor.GT.0. ) THEN
              int_last = int_last*Covden_win(i)
              IF ( cov.LT.NEARZERO ) THEN
!*** if storage on winter canopy, with summer cover = 0, evap all stor
                evap_changeover = int_last
                intcpstor = 0.
                Intcp_on(i) = 0
              ELSE
                intcpstor = intcpstor*Covden_win(i)/cov
              ENDIF
            ENDIF
          ELSE
            int_last = int_last*cov
          ENDIF

!*****Determine the amount of interception from rain

          Net_rain(i) = Hru_rain(i)
          IF ( Hru_rain(i).GT.0. .AND. cov.GT.NEARZERO ) THEN
            Intcp_form(i) = 0

            IF ( Transp_on(i).EQ.1 ) THEN
              stor = Srain_intcp(i)
            ELSE
              stor = Wrain_intcp(i)
            ENDIF
            IF ( Cov_type(i).GT.1 ) THEN
              CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i),
     +                       intcpstor, Net_rain(i))
            ELSEIF ( Cov_type(i).EQ.1 ) THEN
              IF ( Pkwater_equiv(i).LE.0. .AND. Hru_snow(i).LE.0. ) THEN
                CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i),
     +                         intcpstor, Net_rain(i))
!rain on grass with snowpack and/or snowfall, storage added to snowpack
              ELSEIF ( intcpstor.GT.0. ) THEN
                iputpkflg = 1
                Pkwater_equiv(i) = Pkwater_equiv(i) + intcpstor*cov
                snowstor = snowstor + intcpstor*cov
!rsr, temporary print statement
!               PRINT *, 'rain on snow: updated pkwater in intcp',
!    +                intcpstor, cov, i
                intcpstor = 0.0
                Intcp_on(i) = 0
              ENDIF
            ELSE
              intcpstor = 0.0
              Intcp_on(i) = 0
            ENDIF
          ENDIF

!******Determine amount of interception from snow

          Net_snow(i) = Hru_snow(i)
          IF ( Hru_snow(i).GT.0. .AND. cov.GT.NEARZERO ) THEN
            Intcp_form(i) = 1
            IF ( Cov_type(i).GT.1 ) THEN
              CALL intercept(Hru_snow(i), Snow_intcp(i), cov,
     +                       Intcp_on(i), intcpstor, Net_snow(i))
              IF ( Net_snow(i).LT.NEARZERO ) THEN   !rsr, added 3/9/2006
                Newsnow(i) = 0
                Pptmix(i) = 0   ! reset to be sure it is zero
                iputnsflg = 1
              ENDIF
            ENDIF
          ENDIF

          Net_ppt(i) = Net_rain(i) + Net_snow(i)

!******compute evaporation or sublimation of interception

          IF ( Intcp_on(i).EQ.1 ) THEN

            IF ( dt.GT.23.999D0 .OR. Hru_ppt(i).LT.NEARZERO ) THEN

              evrn = Potet(i)/Epan_coef(mo)
              evsn = Potet_sublim*Potet(i)

              IF ( Nevap.GT.0 ) THEN
                IF ( Pan_evap(1).GT.-998.99 ) evrn = Pan_evap(1)
              ENDIF

!******Compute snow interception loss

              IF ( Intcp_form(i).EQ.1 ) THEN
                IF ( Basin_ppt.LT.NEARZERO ) THEN
                  z = intcpstor - evsn
                  IF ( z.GT.0. ) THEN
                    Intcp_on(i) = 1
                    intcpstor = z
                    intcpevap = evsn
                  ELSE
                    intcpevap = intcpstor
                    intcpstor = 0.
                    Intcp_on(i) = 0
                  ENDIF
                ENDIF
              ELSEIF ( Intcp_form(i).EQ.0 ) THEN
                d = intcpstor - evrn
                IF ( d.GT.0. ) THEN
                  intcpstor = d
                  intcpevap = evrn
                  Intcp_on(i) = 1
                ELSE
                  intcpevap = intcpstor
                  intcpstor = 0.
                  Intcp_on(i) = 0
                ENDIF
              ENDIF
            ENDIF
          ENDIF

        ENDIF

        IF ( cov.GT.0.0 .AND. evap_changeover.GT.0.0 ) THEN
          Intcp_evap(i) = intcpevap + evap_changeover/cov
        ELSE
          Intcp_evap(i) = intcpevap
        ENDIF
        Hru_intcpevap(i) = Intcp_evap(i)*cov
        Intcp_stor(i) = intcpstor
        Hru_intcpstor(i) = intcpstor*cov

        Basin_net_ppt = Basin_net_ppt + Net_ppt(i)*harea
        Basin_intcp_stor = Basin_intcp_stor + Hru_intcpstor(i)*harea
        Basin_intcp_evap = Basin_intcp_evap + Hru_intcpevap(i)*harea

        IF ( Prt_debug.EQ.1 ) THEN
          basin_snowstor = basin_snowstor + snowstor*harea
          delstor = Hru_intcpstor(i) - int_last
          sumintlast = sumintlast + int_last*harea
          delstorck = delstorck + delstor*harea
          hrubal = Hru_rain(i) + Hru_snow(i) - Net_rain(i) - Net_snow(i)
     +            - delstor - Hru_intcpevap(i) - snowstor
          pptck = pptck + (Hru_rain(i)+Hru_snow(i))*harea
          IF ( ABS(hrubal).GT.1.0E-5 ) THEN
            IF ( ABS(hrubal).GT.1.0E-4 ) THEN
              WRITE (BALUNT, *) 'possible HRU water balance error'
            ELSE
              WRITE (BALUNT, *) 'Interception HRU rounding issue'
            ENDIF
            WRITE (BALUNT,'(7I5,14F10.5)') i, nowtime, hrubal,
     +            Net_rain(i), Net_snow(i), Hru_rain(i), Hru_snow(i),
     +            intcpstor, int_last, intcpevap, Srain_intcp(i),
     +            Wrain_intcp(i), Snow_intcp(i), snowstor,
     +            evap_changeover, cov
          ENDIF
        ENDIF

      ENDDO

      Basin_net_ppt = Basin_net_ppt*Basin_area_inv
      Basin_intcp_stor = Basin_intcp_stor*Basin_area_inv
      Basin_intcp_evap = Basin_intcp_evap*Basin_area_inv

      IF ( Prt_debug.EQ.1 ) THEN
        basin_snowstor = basin_snowstor*Basin_area_inv
        delta_stor = Basin_intcp_stor - last_stor + basin_snowstor
        pptbal = Basin_ppt - Basin_net_ppt - delta_stor -
     +           Basin_intcp_evap
        pptck = pptck*Basin_area_inv
        delstorck = delstorck*Basin_area_inv - delta_stor
        sumintlast = sumintlast*Basin_area_inv
        IF ( ABS(pptbal).GT.1.0E-4 ) THEN
          WRITE (BALUNT, *) 'possible basin water balance error', pptbal
        ELSEIF ( ABS(pptbal).GT.1.0E-5 ) THEN
          WRITE (BALUNT, *) 'interception basin rounding issue', pptbal
        ENDIF
        WRITE (BALUNT, 9001) nowtime(1), mo, day, pptbal, Basin_ppt,
     +        Basin_net_ppt, Basin_intcp_evap, Basin_intcp_stor,
     +        basin_snowstor, delstorck, pptck, sumintlast, last_stor
      ENDIF

      IF ( iputpkflg.EQ.1 ) THEN
!       PRINT *, 'intcp put pkwater', nowtime
        IF ( putvar('snow', 'pkwater_equiv', Nhru, 'real',
     +       Pkwater_equiv).NE.0 ) RETURN
      ENDIF
      IF ( iputnsflg.EQ.1 ) THEN
!       PRINT *, 'intcp put newsnow and pptmix', nowtime
        IF ( putvar('precip', 'newsnow', Nhru, 'integer', Newsnow)
     +       .NE.0 ) RETURN
        IF ( putvar('precip', 'pptmix', Nhru, 'integer', Pptmix)
     +       .NE.0 ) RETURN
      ENDIF

      intrun = 0

 9001 FORMAT (I5, 2('/', I2.2), 10F11.5)
      END FUNCTION intrun

!***********************************************************************
!      Subroutine to compute interception of rain or snow
!***********************************************************************
      SUBROUTINE intercept(Precip, Stor_max, Cov, Intcp_on, Intcp_stor,
     +                     Net_precip)
      USE PRMS_INTCP, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(OUT) :: Intcp_on
      REAL, INTENT(IN) :: Precip, Stor_max, Cov
      REAL, INTENT(INOUT) :: Intcp_stor
      REAL, INTENT(OUT) :: Net_precip
! Local Variables
      REAL :: thrufall, avail_stor
!***********************************************************************
      Intcp_on = 1

!rsr Note: avail_stor can be negative when wrain_intcp < snow_intcp
!          for mixed precipitation event
      avail_stor = Stor_max - Intcp_stor

      IF ( avail_stor.LT.NEARZERO ) THEN
        thrufall = Precip
      ELSEIF ( Precip.GT.avail_stor ) THEN
        Intcp_stor = Stor_max
        thrufall = Precip - avail_stor
      ELSE
        Intcp_stor = Intcp_stor + Precip
        thrufall = 0.
      ENDIF

      Net_precip = (Precip*(1.-Cov)) + (thrufall*Cov)

!*** allow intcp_stor to exceed stor_max with small amounts of precip
      IF ( Net_precip.LT.0.000001 ) THEN
        Intcp_stor = Intcp_stor + Net_precip/Cov
        Net_precip = 0.0
      ENDIF

      END SUBROUTINE intercept

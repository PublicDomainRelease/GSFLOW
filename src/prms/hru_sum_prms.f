!***********************************************************************
! Sums values for daily, monthly, yearly and total flow
!***********************************************************************
      MODULE PRMS_HRUSUM
      IMPLICIT NONE
!   Local Variables
      INTEGER :: Modays(12), Nowtime(6)
      REAL :: Yrdays
!   Declared Variables
      REAL, ALLOCATABLE :: Hru_ppt_mo(:), Hru_net_ppt_mo(:)
      REAL, ALLOCATABLE :: Hru_potet_mo(:), Hru_actet_mo(:)
      REAL, ALLOCATABLE :: Hru_snowmelt_mo(:), Hru_sroff_mo(:)
!   Declared Variables from other modules - solrad
      REAL, ALLOCATABLE :: Swr(:)
!   Declared Variables from other modules - temp
      REAL, ALLOCATABLE :: Tmx(:), Tmn(:)
!   Declared Variables from other modules - precip
      REAL, ALLOCATABLE :: Hru_ppt(:)
!   Declared Variables from other modules - intcp
      REAL, ALLOCATABLE :: Nppt(:), Hru_intcpstor(:), Hru_intcpevap(:)
!   Declared Variables from other modules - potet
      INTEGER, ALLOCATABLE :: Transp_on(:)
      REAL, ALLOCATABLE :: Pet(:)
!   Declared Variables from other modules - smbal or soilzone
      REAL, ALLOCATABLE :: Aet(:), Smav(:), Soil_to_gw(:)
!   Declared Variables from other modules - srunoff
      REAL, ALLOCATABLE :: Infl(:), Sroff(:), Hru_impervstor(:)
!   Declared Variables from other modules - snow
      REAL, ALLOCATABLE :: Pweqv(:), Pk_den(:), Pk_temp(:)
      REAL, ALLOCATABLE :: Smlt(:), Albedo(:), Tcal(:)
!   Declared Variables from other modules - ssflow or soilzone
      REAL, ALLOCATABLE :: Soil_to_ssr(:)
!   Declared Variables from other modules - basin
      INTEGER :: Active_hrus
      INTEGER, ALLOCATABLE :: Hru_route_order(:)
      REAL, ALLOCATABLE :: Hru_percent_perv(:)
!   Declared Private Variables
      INTEGER :: Nhru
      REAL, ALLOCATABLE :: Hru_ppt_yr(:), Hru_net_ppt_yr(:)
      REAL, ALLOCATABLE :: Hru_potet_yr(:), Hru_actet_yr(:)
      REAL, ALLOCATABLE :: Hru_snowmelt_yr(:), Hru_sroff_yr(:)
      REAL, ALLOCATABLE :: Soil_to_gw_mo(:), Soil_to_ssr_mo(:)
      REAL, ALLOCATABLE :: Soil_to_gw_yr(:), Soil_to_ssr_yr(:)
      REAL, ALLOCATABLE :: Stor_last(:)
!   Declared Parameters
      INTEGER :: Pmo, Moyrsum
      INTEGER, ALLOCATABLE :: Hru_type(:)
      END MODULE PRMS_HRUSUM

!***********************************************************************
!     Main hru_sum routine
!***********************************************************************
      INTEGER FUNCTION hru_sum_prms(Arg)
      USE PRMS_HRUSUM
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: hsumbdecl, hsumbinit, hsumbrun
!***********************************************************************
      hru_sum_prms = 0

      IF ( Arg.EQ.'run' ) THEN
        hru_sum_prms = hsumbrun()
      ELSEIF ( Arg.EQ.'declare' ) THEN
        hru_sum_prms = hsumbdecl()
      ELSEIF ( Arg.EQ.'initialize' ) THEN
        hru_sum_prms = hsumbinit()
      ENDIF

      END FUNCTION hru_sum_prms

!***********************************************************************
!     hsumbdecl - set up basin summary parameters
!   Declared Parameters
!     pmo, moyrsum, hru_type
!***********************************************************************
      INTEGER FUNCTION hsumbdecl()
      USE PRMS_HRUSUM
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      hsumbdecl = 1

      IF ( declmodule(
     +'$Id: hru_sum_prms.f 3911 2008-02-28 19:44:50Z rsregan $'
     +).NE.0 ) RETURN

      Nhru = getdim('nhru')
      IF ( Nhru.EQ.-1 ) RETURN

      ALLOCATE (Hru_ppt_yr(Nhru), Hru_net_ppt_yr(Nhru))
      IF ( declpri('hsumb_hru_ppt_yr', Nhru, 'real', Hru_ppt_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('hsumb_hru_net_ppt_yr', Nhru, 'real', Hru_net_ppt_yr)
     +     .NE.0 ) RETURN
      ALLOCATE (Hru_potet_yr(Nhru), Hru_actet_yr(Nhru))
      IF ( declpri('hsumb_hru_potet_yr', Nhru, 'real', Hru_potet_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('hsumb_hru_actet_yr', Nhru, 'real', Hru_actet_yr)
     +     .NE.0 ) RETURN
      ALLOCATE (Hru_snowmelt_yr(Nhru), Hru_sroff_yr(Nhru))
      IF ( declpri('hsumb_hru_snowmelt_yr', Nhru, 'real',
     +     Hru_snowmelt_yr).NE.0 ) RETURN
      IF ( declpri('hsumb_hru_sroff_yr', Nhru, 'real', Hru_sroff_yr)
     +     .NE.0 ) RETURN
      ALLOCATE (Soil_to_gw_mo(Nhru), Soil_to_gw_yr(Nhru))
      IF ( declpri('hsumb_soil_to_gw_mo', Nhru, 'real', Soil_to_gw_mo)
     +     .NE.0 ) RETURN
      IF ( declpri('hsumb_soil_to_gw_yr', Nhru, 'real', Soil_to_gw_yr)
     +     .NE.0 ) RETURN
      ALLOCATE (Soil_to_ssr_mo(Nhru), Soil_to_ssr_yr(Nhru))
      IF ( declpri('hsumb_soil_to_ssr_mo', Nhru, 'real', Soil_to_ssr_mo)
     +     .NE.0 ) RETURN
      IF ( declpri('hsumb_soil_to_ssr_yr', Nhru, 'real', Soil_to_ssr_yr)
     +     .NE.0 ) RETURN
      ALLOCATE (Stor_last(Nhru))
      IF ( declpri('hsumb_stor_last', Nhru, 'real', Stor_last)
     +     .NE.0 ) RETURN

! Declare Parameters
      IF ( declparam('hru_sum', 'pmo', 'one', 'integer',
     +     '0', '0', '12',
     +     'Month to print HRU summary', 'Month to print HRU summary',
     +     'none').NE.0 ) RETURN

      IF ( declparam('hru_sum', 'moyrsum', 'one', 'integer',
     +     '0', '0', '1',
     +     'Switch for HRU monthly and yearly summary',
     +     'Switch for HRU monthly and yearly summary (0=off, 1=on)',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_type(Nhru))
      IF ( declparam('hru_sum', 'hru_type', 'nhru', 'integer',
     +     '1', '0', '2',
     +     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake)',
     +     'none').NE.0 ) RETURN

! Declare Variables
      ALLOCATE (Hru_ppt_mo(Nhru))
      IF ( declvar('hru_sum', 'hru_ppt_mo', 'nhru', Nhru, 'real',
     +     'HRU monthly precip',
     +     'inches',
     +     Hru_ppt_mo).NE.0 ) RETURN

      ALLOCATE (Hru_net_ppt_mo(Nhru))
      IF ( declvar('hru_sum', 'hru_net_ppt_mo', 'nhru', Nhru, 'real',
     +     'HRU monthly net precip',
     +     'inches',
     +     Hru_net_ppt_mo).NE.0 ) RETURN

      ALLOCATE (Hru_potet_mo(Nhru))
      IF ( declvar('hru_sum', 'hru_potet_mo', 'nhru', Nhru, 'real',
     +     'HRU monthly potential evapotranspiration',
     +     'inches',
     +     Hru_potet_mo).NE.0 ) RETURN

      ALLOCATE (Hru_actet_mo(Nhru))
      IF ( declvar('hru_sum', 'hru_actet_mo', 'nhru', Nhru, 'real',
     +     'HRU monthly computed evapotranspiration',
     +     'inches',
     +     Hru_actet_mo).NE.0 ) RETURN

      ALLOCATE (Hru_snowmelt_mo(Nhru))
      IF ( declvar('hru_sum', 'hru_snowmelt_mo', 'nhru', Nhru, 'real',
     +     'HRU monthly snowmelt',
     +     'inches',
     +     Hru_snowmelt_mo).NE.0 ) RETURN

      ALLOCATE (Hru_sroff_mo(Nhru))
      IF ( declvar('hru_sum', 'hru_sroff_mo', 'nhru', Nhru, 'real',
     +     'HRU monthly surface runoff',
     +     'inches',
     +     Hru_sroff_mo).NE.0 ) RETURN

! Allocate arrays for variables from other modules
      ALLOCATE (Swr(Nhru), Tmx(Nhru), Tmn(Nhru), Hru_ppt(Nhru))
      ALLOCATE (Nppt(Nhru), Hru_intcpstor(Nhru), Hru_intcpevap(Nhru))
      ALLOCATE (Transp_on(Nhru), Pet(Nhru), Hru_impervstor(Nhru))
      ALLOCATE (Aet(Nhru), Smav(Nhru), Soil_to_gw(Nhru))
      ALLOCATE (Infl(Nhru), Sroff(Nhru), Pweqv(Nhru))
      ALLOCATE (Hru_route_order(Nhru), Hru_percent_perv(Nhru))
      ALLOCATE (Pk_den(Nhru), Pk_temp(Nhru), Smlt(Nhru))
      ALLOCATE (Albedo(Nhru), Tcal(Nhru), Soil_to_ssr(Nhru))

      hsumbdecl = 0
      END FUNCTION hsumbdecl

!***********************************************************************
!     hsumbinit - Initialize basinsum module - get parameter values
!                set to zero
!***********************************************************************
      INTEGER FUNCTION hsumbinit()
      USE PRMS_HRUSUM
      IMPLICIT NONE
      INCLUDE 'fmodules.inc'
!***********************************************************************
      hsumbinit = 1

      IF ( getparam('hru_sum', 'pmo', 1, 'integer', Pmo)
     +     .NE.0 ) RETURN

      IF ( getparam('hru_sum', 'moyrsum', 1, 'integer', Moyrsum)
     +     .NE.0 ) RETURN

      IF ( getparam('hru_sum', 'hru_type', Nhru, 'integer', Hru_type)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'active_hrus', 1, 'integer', Active_hrus)
     +     .NE.0 ) RETURN

      IF ( getvar('basin', 'hru_route_order', Nhru, 'integer',
     +     Hru_route_order).NE.0 ) RETURN

      IF ( getvar('basin', 'hru_percent_perv', Nhru, 'real',
     +     Hru_percent_perv).NE.0 ) RETURN

      IF ( getstep().EQ.0 ) THEN
        Hru_ppt_mo = 0.0
        Hru_net_ppt_mo = 0.0
        Hru_potet_mo = 0.0
        Hru_actet_mo = 0.0
        Hru_snowmelt_mo = 0.0
        Hru_sroff_mo = 0.0
        Hru_ppt_yr = 0.0
        Hru_net_ppt_yr = 0.0
        Hru_potet_yr = 0.0
        Hru_actet_yr = 0.0
        Hru_snowmelt_yr = 0.0
        Hru_sroff_yr = 0.0
        Soil_to_gw_mo = 0.0
        Soil_to_gw_yr = 0.0
        Soil_to_ssr_mo = 0.0
        Soil_to_ssr_yr = 0.0
        Stor_last = 0.0
      ENDIF

      Modays(1) = 31
      Modays(3) = 31
      Modays(4) = 30
      Modays(5) = 31
      Modays(6) = 30
      Modays(7) = 31
      Modays(8) = 31
      Modays(9) = 30
      Modays(10) = 31
      Modays(11) = 30
      Modays(12) = 31
      CALL dattim('now', Nowtime)
      IF ( isleap(Nowtime(1)).EQ.1 ) THEN
        Yrdays = 366
        Modays(2) = 29
      ELSE
        Yrdays = 365
        Modays(2) = 28
      ENDIF

      hsumbinit = 0
      END FUNCTION hsumbinit

!***********************************************************************
!     hsumbrun - Computes summary values
!***********************************************************************
      INTEGER FUNCTION hsumbrun()
      USE PRMS_HRUSUM
      IMPLICIT NONE
      INTRINSIC FLOAT
      INCLUDE 'fmodules.inc'
! Local Variables
      CHARACTER(LEN=150) :: buffer
      INTEGER :: mo, day, wyday, jday, i, j
!     INTEGER :: endtime(6)
      REAL :: hruprt(24), ri, rmo, rdy, ryr, stor, wbal
!***********************************************************************
      IF ( Pmo.EQ.0 .AND. Moyrsum.EQ.0 ) THEN
        hsumbrun = 0
        RETURN
      ELSE
        hsumbrun = 1
      ENDIF

      CALL dattim('now', Nowtime)
!     CALL dattim('end', endtime)
      wyday = julian('now', 'water')
      jday = julian('now', 'calendar')
      mo = Nowtime(2)
      day = Nowtime(3)

      IF ( Moyrsum.EQ.1 ) THEN
        IF ( jday.EQ.1 ) THEN
          IF ( isleap(Nowtime(1)).EQ.1 ) THEN
            Yrdays = 366
            Modays(2) = 29
          ELSE
            Yrdays = 365
            Modays(2) = 28
          ENDIF
        ENDIF
        IF ( day.EQ.1 ) THEN
          Hru_ppt_mo = 0.0
          Hru_net_ppt_mo = 0.0
          Hru_potet_mo = 0.0
          Hru_actet_mo = 0.0
          Hru_snowmelt_mo = 0.0
          Hru_sroff_mo = 0.0
          Soil_to_gw_mo = 0.0
          Soil_to_ssr_mo = 0.0
        ENDIF
        IF ( wyday.EQ.1 ) THEN
          Hru_ppt_yr = 0.0
          Hru_net_ppt_yr = 0.0
          Hru_potet_yr = 0.0
          Hru_actet_yr = 0.0
          Hru_snowmelt_yr = 0.0
          Hru_sroff_yr = 0.0
          Soil_to_gw_yr = 0.0
          Soil_to_ssr_yr = 0.0
        ENDIF
      ENDIF

      IF ( getvar('potet', 'transp_on', Nhru, 'integer', Transp_on)
     +     .NE.0 ) RETURN

      IF ( getvar('solrad', 'swrad', Nhru, 'real', Swr)
     +     .NE.0 ) RETURN

      IF ( getvar('temp', 'tmaxf', Nhru, 'real', Tmx)
     +     .NE.0 ) RETURN

      IF ( getvar('temp', 'tminf', Nhru, 'real', Tmn)
     +     .NE.0 ) RETURN

      IF ( getvar('precip', 'hru_ppt', Nhru, 'real', Hru_ppt)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'net_ppt', Nhru, 'real', Nppt)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'hru_intcpstor', Nhru, 'real', Hru_intcpstor)
     +     .NE.0 ) RETURN

      IF ( getvar('intcp', 'hru_intcpevap', Nhru, 'real', Hru_intcpevap)
     +     .NE.0 ) RETURN

      IF ( getvar('potet', 'potet', Nhru, 'real',
     +     Pet).NE.0 ) RETURN

      IF ( getvar('smbal', 'hru_actet', Nhru, 'real',
     +     Aet).NE.0 ) RETURN

      IF ( getvar('smbal', 'soil_moist', Nhru, 'real',
     +     Smav).NE.0 ) RETURN

      IF ( getvar('srunoff', 'infil', Nhru, 'real',
     +     Infl).NE.0 ) RETURN

      IF ( getvar('srunoff', 'sroff', Nhru, 'real',
     +     Sroff).NE.0 ) RETURN

      IF ( getvar('snow', 'pkwater_equiv', Nhru, 'real', Pweqv)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'snowmelt', Nhru, 'real', Smlt)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'albedo', Nhru, 'real', Albedo)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'pk_temp', Nhru, 'real', Pk_temp)
     +     .NE.0 ) RETURN
       
      IF ( getvar('snow', 'pk_den', Nhru, 'real', Pk_den)
     +     .NE.0 ) RETURN

      IF ( getvar('snow', 'tcal', Nhru, 'real', Tcal)
     +     .NE.0 ) RETURN

      IF ( getvar('smbal', 'soil_to_gw', Nhru, 'real',
     +     Soil_to_gw).NE.0 ) RETURN

      IF ( getvar('smbal', 'soil_to_ssr', Nhru, 'real',
     +     Soil_to_ssr).NE.0 ) RETURN

      IF ( getvar('srunoff', 'hru_impervstor', Nhru, 'real',
     +     Hru_impervstor).NE.0 ) RETURN

      IF ( mo.EQ.Pmo ) THEN
        rdy = FLOAT(day)
        CALL opstr('   hru   day   swr   tmx   tmn  oppt  nppt   int '//
     +             ' inls   pet   aet  smav pweqv   den  pact   alb  '//
     +            'tcal  smlt   infl    sro   s2gw   s2ss  imst   wbal')
        DO i = 1, Nhru
          IF ( Hru_type(i).EQ.0 ) CYCLE
          hruprt(1) = i
          hruprt(2) = rdy
          hruprt(3) = Swr(i)
          hruprt(4) = Tmx(i)
          hruprt(5) = Tmn(i)
          hruprt(6) = Hru_ppt(i)
          hruprt(7) = Nppt(i)
          hruprt(8) = Hru_intcpstor(i)
          hruprt(9) = Hru_intcpevap(i)
          hruprt(10) = Pet(i)
          hruprt(11) = Aet(i)
          hruprt(12) = Smav(i)*Hru_percent_perv(i)
          hruprt(13) = Pweqv(i)
          hruprt(14) = Pk_den(i)
          hruprt(15) = Pk_temp(i)
          hruprt(16) = Albedo(i)
          hruprt(17) = Tcal(i)
          hruprt(18) = Smlt(i)
          hruprt(19) = Infl(i)
          hruprt(20) = Sroff(i)
          hruprt(21) = Soil_to_gw(i)
          hruprt(22) = Soil_to_ssr(i)
          hruprt(23) = Hru_impervstor(i)
          stor = Pweqv(i) + hruprt(12) + hruprt(8) + hruprt(23)
!Hru_actet includes perv_actet, imperv_evap, intcp_evap, and snow_evap
          wbal = Hru_ppt(i) + Stor_last(i) - stor - Aet(i)
     +           - Sroff(i) - Soil_to_gw(i) - Soil_to_ssr(i)
          hruprt(24) = wbal
          Stor_last(i) = stor
          WRITE (buffer, '(F6.0,F5.0,1X,16F6.2,4F7.4,F6.2,F7.4)')
     +           (hruprt(j), j=1, 24)
          CALL opstr(buffer)
        ENDDO
      ELSEIF ( Pmo.GT.0 ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Stor_last(i) = Pweqv(i) + Smav(i)*Hru_percent_perv(i)
     +                   + Hru_intcpstor(i) + Hru_impervstor(i)
        ENDDO
      ENDIF


      IF ( Moyrsum.EQ.1 ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Hru_ppt_mo(i) = Hru_ppt_mo(i) + Hru_ppt(i)
          Hru_net_ppt_mo(i) = Hru_net_ppt_mo(i) + Nppt(i)
          Hru_potet_mo(i) = Hru_potet_mo(i) + Pet(i)
          Hru_actet_mo(i) = Hru_actet_mo(i) + Aet(i)
          Hru_snowmelt_mo(i) = Hru_snowmelt_mo(i) + Smlt(i)
          Hru_sroff_mo(i) = Hru_sroff_mo(i) + Sroff(i)
          Soil_to_gw_mo(i) = Soil_to_gw_mo(i) + Soil_to_gw(i)
          Soil_to_ssr_mo(i) = Soil_to_ssr_mo(i) + Soil_to_ssr(i)
          Hru_ppt_yr(i) = Hru_ppt_yr(i) + Hru_ppt(i)
          Hru_net_ppt_yr(i) = Hru_net_ppt_yr(i) + Nppt(i)
          Hru_potet_yr(i) = Hru_potet_yr(i) + Pet(i)
          Hru_actet_yr(i) = Hru_actet_yr(i) + Aet(i)
          Hru_snowmelt_yr(i) = Hru_snowmelt_yr(i) + Smlt(i)
          Hru_sroff_yr(i) = Hru_sroff_yr(i) + Sroff(i)
          Soil_to_gw_yr(i) = Soil_to_gw_yr(i) + Soil_to_gw(i)
          Soil_to_ssr_yr(i) = Soil_to_ssr_yr(i) + Soil_to_ssr(i)
        ENDDO

        IF ( day.EQ.Modays(mo) ) THEN
          rmo = FLOAT(mo)
          CALL opstr('   hru   mo                    oppt   nppt     '//
     +   '        pet   aet        pweqv  2ssres  2gwres   smlt sroff')

          DO i = 1, Nhru
            IF ( Hru_type(i).EQ.0 ) CYCLE
            ri = FLOAT(i)
            WRITE (buffer, 9001) ri, rmo, Hru_ppt_mo(i),
     +                           Hru_net_ppt_mo(i), Hru_potet_mo(i),
     +                           Hru_actet_mo(i), Pweqv(i),
     +                           Soil_to_ssr_mo(i), Soil_to_gw_mo(i),
     +                           Hru_snowmelt_mo(i), Hru_sroff_mo(i)
            CALL opstr(buffer(:106))
          ENDDO
        ENDIF

        IF ( wyday.EQ.Yrdays ) THEN
          ryr = FLOAT(Nowtime(1))
          CALL opstr('   hru year                    oppt   nppt     '//
     +   '        pet   aet        pweqv  2ssres  2gwres   smlt sroff')

          DO i = 1, Nhru
            IF ( Hru_type(i).EQ.0 ) CYCLE
            ri = FLOAT(i)
            WRITE (buffer, 9001) ri, ryr, Hru_ppt_yr(i),
     +                           Hru_net_ppt_yr(i), Hru_potet_yr(i),
     +                           Hru_actet_yr(i), Pweqv(i),
     +                           Soil_to_ssr_yr(i), Soil_to_gw_yr(i),
     +                           Hru_snowmelt_yr(i), Hru_sroff_yr(i)
            CALL opstr(buffer(:106))
          ENDDO
        ENDIF
      ENDIF

      hsumbrun = 0

 9001 FORMAT (F7.0, F5.0, 16X, 2F7.2, F16.2, F6.2, F13.2, 2F8.2, 2F7.2)

      END FUNCTION hsumbrun

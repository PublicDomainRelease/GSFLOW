!***********************************************************************
! Computes daily, monthly, yearly, and total flow summaries of volumes
! and flows for each HRU
!***********************************************************************
      MODULE PRMS_HRUSUM
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Gwflg, Hrutot_flg
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Hru_ppt_mo(:), Hru_net_ppt_mo(:)
      REAL, SAVE, ALLOCATABLE :: Hru_potet_mo(:), Hru_actet_mo(:)
      REAL, SAVE, ALLOCATABLE :: Hru_snowmelt_mo(:), Hru_sroff_mo(:)
      REAL, SAVE, ALLOCATABLE :: Hru_timestep_outflow_tot(:)
!   Declared Private Variables
      REAL, SAVE, ALLOCATABLE :: Hru_ppt_yr(:), Hru_net_ppt_yr(:)
      REAL, SAVE, ALLOCATABLE :: Hru_potet_yr(:), Hru_actet_yr(:)
      REAL, SAVE, ALLOCATABLE :: Hru_snowmelt_yr(:), Hru_sroff_yr(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_gw_mo(:), Soil_to_ssr_mo(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_gw_yr(:), Soil_to_ssr_yr(:)
      REAL, SAVE, ALLOCATABLE :: Stor_last(:)
!   Declared Parameters
      INTEGER, SAVE :: Pmo, Moyrsum
      END MODULE PRMS_HRUSUM

!***********************************************************************
!     Main hru_sum routine
!***********************************************************************
      INTEGER FUNCTION hru_sum_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: hsumbdecl, hsumbinit, hsumbrun
!***********************************************************************
      hru_sum_prms = 0

      IF ( Process_flag==0 ) THEN
        hru_sum_prms = hsumbrun()
      ELSEIF ( Process_flag==1 ) THEN
        hru_sum_prms = hsumbdecl()
      ELSEIF ( Process_flag==2 ) THEN
        hru_sum_prms = hsumbinit()
      ENDIF

      END FUNCTION hru_sum_prms

!***********************************************************************
!     hsumbdecl - set up basin summary parameters
!   Declared Parameters
!     pmo, moyrsum, hru_area
!***********************************************************************
      INTEGER FUNCTION hsumbdecl()
      USE PRMS_HRUSUM
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, declpri, declvar
!***********************************************************************
      hsumbdecl = 1

      IF ( declmodule(
     +'$Id: hru_sum_prms.f 3116 2011-05-17 16:20:01Z rsregan $'
     +).NE.0 ) RETURN

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

      ALLOCATE (Hru_timestep_outflow_tot(Nhru))
      IF ( declvar('hru_sum', 'hru_timestep_outflow_tot', 'nhru', Nhru,
     +     'real',
     +     'Total outflow (sroff, interflow, gwflow) from each HRU',
     +     'cfs',
     +     Hru_timestep_outflow_tot).NE.0 ) RETURN

      hsumbdecl = 0
      END FUNCTION hsumbdecl

!***********************************************************************
!     hsumbinit - Initialize basinsum module - get parameter values
!                set to zero
!***********************************************************************
      INTEGER FUNCTION hsumbinit()
      USE PRMS_HRUSUM
      USE PRMS_MODULE, ONLY: Ncascdgw
      USE PRMS_BASIN, ONLY: Timestep, Nhru, Nssr, Ngw
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      INTEGER :: i
!***********************************************************************
      hsumbinit = 1

      IF ( getparam('hru_sum', 'pmo', 1, 'integer', Pmo)
     +     .NE.0 ) RETURN

      IF ( getparam('hru_sum', 'moyrsum', 1, 'integer', Moyrsum)
     +     .NE.0 ) RETURN

      IF ( Timestep==0 ) THEN
        DO i = 1, Nhru
          Hru_ppt_mo(i) = 0.0
          Hru_net_ppt_mo(i) = 0.0
          Hru_potet_mo(i) = 0.0
          Hru_actet_mo(i) = 0.0
          Hru_snowmelt_mo(i) = 0.0
          Hru_sroff_mo(i) = 0.0
          Hru_ppt_yr(i) = 0.0
          Hru_net_ppt_yr(i) = 0.0
          Hru_potet_yr(i) = 0.0
          Hru_actet_yr(i) = 0.0
          Hru_snowmelt_yr(i) = 0.0
          Hru_sroff_yr(i) = 0.0
          Soil_to_gw_mo(i) = 0.0
          Soil_to_gw_yr(i) = 0.0
          Soil_to_ssr_mo(i) = 0.0
          Soil_to_ssr_yr(i) = 0.0
          Stor_last(i) = 0.0
          Hru_timestep_outflow_tot(i) = 0.0
        ENDDO
      ENDIF

      Hrutot_flg = 0
      IF ( Nhru==Nssr .AND. Nhru==Ngw ) Hrutot_flg = 1
      Gwflg = 0
      IF ( Ncascdgw>0 .AND. Nhru==Ngw ) Gwflg = 1

      hsumbinit = 0
      END FUNCTION hsumbinit

!***********************************************************************
!     hsumbrun - Computes summary values
!***********************************************************************
      INTEGER FUNCTION hsumbrun()
      USE PRMS_HRUSUM
      USE PRMS_MODULE, ONLY: Ncascade
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type,
     +    Hru_percent_perv, Hru_area, Nhru, Nssr, Hru_gwres, Hru_ssres
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tminf, Hru_ppt, Transp_on,
     +    Potet, Swrad
      USE PRMS_FLOWVARS, ONLY: Soil_to_gw, Soil_to_ssr, Hru_impervstor,
     +    Hru_actet, Infil, Sroff, Ssres_flow, Hru_hortonian_cascadeflow
      USE PRMS_OBS, ONLY: Jday, Nowyear, Nowmonth, Nowday, Cfs_conv,
     +    Yrdays, Modays
      USE PRMS_INTCP, ONLY: Hru_intcpstor, Net_ppt, Hru_intcpevap
      USE PRMS_SNOW, ONLY: Tcal, Pk_den, Pk_temp, Pkwater_equiv,
     +    Snowmelt, Albedo
      USE PRMS_SOILZONE, ONLY: Hru_sz_cascadeflow, Soil_moist
      USE PRMS_GWFLOW_CASC, ONLY: Gwres_flow, Hru_gw_cascadeflow
      IMPLICIT NONE
      INTRINSIC FLOAT
      INTEGER, EXTERNAL :: julian
! Local Variables
      CHARACTER(LEN=150) :: buffer
      INTEGER :: wyday, i, j, k, ii, jj
      REAL :: hruprt(24), ri, rmo, rdy, ryr, stor, wbal
!***********************************************************************
      hsumbrun = 1

      IF ( Hrutot_flg==1 ) THEN
        IF ( Pmo.EQ.0 .AND. Moyrsum.EQ.0 ) THEN
          ! Cfs_conv converts acre-inches per timestep to cfs
          DO jj = 1, Active_hrus
            i = Hru_route_order(jj)
            j = Hru_gwres(i)
            k = Hru_ssres(i)
            Hru_timestep_outflow_tot(i) = Hru_area(i)*Cfs_conv*
     +         (Sroff(i)+Gwres_flow(j)+Ssres_flow(k))
            IF ( Ncascade>0 ) Hru_timestep_outflow_tot(i) =
     +           Hru_timestep_outflow_tot(i) + Hru_area(i)*Cfs_conv*
     +           (Hru_hortonian_cascadeflow(i)+Hru_sz_cascadeflow(i))
            IF ( Gwflg==1 ) Hru_timestep_outflow_tot(i) =
     +           Hru_timestep_outflow_tot(i) + Hru_area(i)*Cfs_conv*
     +           Hru_gw_cascadeflow(j)
          ENDDO
          hsumbrun = 0
          RETURN
        ENDIF
      ENDIF

      wyday = julian('now', 'water')

      IF ( Moyrsum.EQ.1 ) THEN
        IF ( Nowday.EQ.1 ) THEN
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Hru_ppt_mo(i) = 0.0
            Hru_net_ppt_mo(i) = 0.0
            Hru_potet_mo(i) = 0.0
            Hru_actet_mo(i) = 0.0
            Hru_snowmelt_mo(i) = 0.0
            Hru_sroff_mo(i) = 0.0
            Soil_to_gw_mo(i) = 0.0
            Soil_to_ssr_mo(i) = 0.0
          ENDDO
        ENDIF
        IF ( wyday.EQ.1 ) THEN
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Hru_ppt_yr(i) = 0.0
            Hru_net_ppt_yr(i) = 0.0
            Hru_potet_yr(i) = 0.0
            Hru_actet_yr(i) = 0.0
            Hru_snowmelt_yr(i) = 0.0
            Hru_sroff_yr(i) = 0.0
            Soil_to_gw_yr(i) = 0.0
            Soil_to_ssr_yr(i) = 0.0
          ENDDO
        ENDIF
      ENDIF

      IF ( Nowmonth.EQ.Pmo ) THEN
        rdy = FLOAT(Nowday)
        CALL opstr('   hru   day   swr   tmx   tmn  oppt  nppt   int '//
     +             ' inls   pet   aet  smav pweqv   den  pact   alb  '//
     +            'tcal  smlt   infl    sro   s2gw   s2ss  imst   wbal')
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          IF ( Hru_type(i).EQ.0 ) CYCLE
          hruprt(1) = i
          hruprt(2) = rdy
          hruprt(3) = Swrad(i)
          hruprt(4) = Tmaxf(i)
          hruprt(5) = Tminf(i)
          hruprt(6) = Hru_ppt(i)
          hruprt(7) = Net_ppt(i)
          hruprt(8) = Hru_intcpstor(i)
          hruprt(9) = Hru_intcpevap(i)
          hruprt(10) = Potet(i)
          hruprt(11) = Hru_actet(i)
          hruprt(12) = Soil_moist(i)*Hru_percent_perv(i)
          hruprt(13) = Pkwater_equiv(i)
          hruprt(14) = Pk_den(i)
          hruprt(15) = Pk_temp(i)
          hruprt(16) = Albedo(i)
          hruprt(17) = Tcal(i)
          hruprt(18) = Snowmelt(i)
          hruprt(19) = Infil(i)
          hruprt(20) = Sroff(i)
          hruprt(21) = Soil_to_gw(i)
          hruprt(22) = Soil_to_ssr(i)
          hruprt(23) = Hru_impervstor(i)
          stor = hruprt(13) + hruprt(12) + hruprt(8) + hruprt(23)
!Hru_actet includes perv_actet, imperv_evap, intcp_evap, and snow_evap
          wbal = Hru_ppt(i) + Stor_last(i) - stor - Hru_actet(i)
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
          Stor_last(i) = Pkwater_equiv(i)
     +                   + Soil_moist(i)*Hru_percent_perv(i)
     +                   + Hru_intcpstor(i) + Hru_impervstor(i)
        ENDDO
      ENDIF

      IF ( Moyrsum.EQ.1 ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Hru_ppt_mo(i) = Hru_ppt_mo(i) + Hru_ppt(i)
          Hru_net_ppt_mo(i) = Hru_net_ppt_mo(i) + Net_ppt(i)
          Hru_potet_mo(i) = Hru_potet_mo(i) + Potet(i)
          Hru_actet_mo(i) = Hru_actet_mo(i) + Hru_actet(i)
          Hru_snowmelt_mo(i) = Hru_snowmelt_mo(i) + Snowmelt(i)
          Hru_sroff_mo(i) = Hru_sroff_mo(i) + Sroff(i)
          Soil_to_gw_mo(i) = Soil_to_gw_mo(i) + Soil_to_gw(i)
          Soil_to_ssr_mo(i) = Soil_to_ssr_mo(i) + Soil_to_ssr(i)
          Hru_ppt_yr(i) = Hru_ppt_yr(i) + Hru_ppt(i)
          Hru_net_ppt_yr(i) = Hru_net_ppt_yr(i) + Net_ppt(i)
          Hru_potet_yr(i) = Hru_potet_yr(i) + Potet(i)
          Hru_actet_yr(i) = Hru_actet_yr(i) + Hru_actet(i)
          Hru_snowmelt_yr(i) = Hru_snowmelt_yr(i) + Snowmelt(i)
          Hru_sroff_yr(i) = Hru_sroff_yr(i) + Sroff(i)
          Soil_to_gw_yr(i) = Soil_to_gw_yr(i) + Soil_to_gw(i)
          Soil_to_ssr_yr(i) = Soil_to_ssr_yr(i) + Soil_to_ssr(i)
        ENDDO

        IF ( Nowday.EQ.Modays(Nowmonth) ) THEN
          rmo = FLOAT(Nowmonth)
          CALL opstr('   hru   mo                    oppt   nppt     '//
     +   '        pet   aet        pweqv  2ssres  2gwres   smlt sroff')

          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            IF ( Hru_type(i).EQ.0 ) CYCLE
            ri = FLOAT(i)
            WRITE (buffer, 9001) ri, rmo, Hru_ppt_mo(i),
     +                           Hru_net_ppt_mo(i), Hru_potet_mo(i),
     +                           Hru_actet_mo(i), Pkwater_equiv(i),
     +                           Soil_to_ssr_mo(i), Soil_to_gw_mo(i),
     +                           Hru_snowmelt_mo(i), Hru_sroff_mo(i)
            CALL opstr(buffer(:106))
          ENDDO
        ENDIF

        IF ( wyday.EQ.Yrdays ) THEN
          ryr = FLOAT(Nowyear)
          CALL opstr('   hru year                    oppt   nppt     '//
     +   '        pet   aet        pweqv  2ssres  2gwres   smlt sroff')

          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            IF ( Hru_type(i).EQ.0 ) CYCLE
            ri = FLOAT(i)
            WRITE (buffer, 9001) ri, ryr, Hru_ppt_yr(i),
     +                           Hru_net_ppt_yr(i), Hru_potet_yr(i),
     +                           Hru_actet_yr(i), Pkwater_equiv(i),
     +                           Soil_to_ssr_yr(i), Soil_to_gw_yr(i),
     +                           Hru_snowmelt_yr(i), Hru_sroff_yr(i)
            CALL opstr(buffer(:106))
          ENDDO
        ENDIF
      ENDIF

      hsumbrun = 0

 9001 FORMAT (F7.0, F5.0, 16X, 2F7.2, F16.2, F6.2, F13.2, 2F8.2, 2F7.2)

      END FUNCTION hsumbrun

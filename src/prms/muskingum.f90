!***********************************************************************
! Routes water between segments in the system using Muskingum routing
!
!   The Muskingum equation is described in 'Hydrology for Engineers', 3rd ed.
!   by Linsley, R.K, Kohler, M.A., and Paulhus, J.L.H., 1982 p. 275 and in 
!   'Water in Environmental Planning' by Dunne, T., and Leopold, L.B. 1978
!   p. 357.
!
!   Note that the Muskingum equation assumes a linear relation of storage
!   to the inflow/outflow relation and therefore the relation is the same
!   throughout the range of the hydrograph.  The route_time parameter in
!   the fixroute module is replaced by two new parameters, K_coef and 
!   x_coef, which are described below:
!
!   The Muskingum method is based on the equation: S = K[xI + (1 - x)O]
!     where S is storage, K is the storage coefficient, x is a coefficient
!     between 0 and .5, I is inflow, and O is outflow.
!
!   Solving for the outflow at day 2,O2; and knowing the inflow at day 1,
!   I1; the inflow at day 2,I2; and the outflow at day 1, O1; the storage
!   equation can be written as follows:
!
!        O2 = czero*I2 + cone*I1 + ctwo*O1
!
!     where czero = -((Kx - 12)    / (K - Kx + 12))
!           cone  =  (Kx + 12)     / (K - Kx + 12)
!           ctwo  =  (K - Kx - 12) / (K - Kx + 12)
!
!     assuming a time step of one day and K is in units of hours
!
!   This module is based on the "musroute.f" module. It differs in three
!   basic ways:
!
!   1. This module uses an internal routing time step of one hour.
!      The old muskingum module ran on the same daily time step as
!      the rest of PRMS. The problem with this is that there is no
!      ability to distinguish where the flood wave (front of the flow
!      change) within the segment. For example, if there is a series
!      of 4 1-day long segments, a flood wave will make it to the bottom
!      of these in 1 day. If the same system is modeled as 1 4-day long
!      segment, it will take 4 days.
!
!   2. The X parameter has been removed as a specified input and is now computed. To
!      my knowledge, no modeler had ever set this to anything other than the default
!      value (0.2) anyway. Always using the default value can lead to problems
!      with the C coffecients which can result in mass balance problems or negative
!      flow values.
!
!      To solve this problem, I assume that the C coefficients must
!      always be between 0 and 1. By setting the C coefficients equal to 0 and 1,
!      various limits on the time step (ts), X, and K can be determined. There are
!      two of these limits which are of interest:
!
!      When C0 = 0:
!             ts
!        K = -----
!             2X
!
!      When C2 = 0:
!            ts
!       K = -----
!           2(1-X)
!
!      Determining a value of K half way between these two limits (by averaging)
!      and solving for X using the quadratic formula results in:
!
!            1-sqrt(1-(ts/K))
!       X = ------------------
!                  2
!
!       So when ts is fixed at one hour and K is fixed as the average (or expected)
!       travel time corresponding to the segment (for each segment in the stream
!       network), a value of X can be computed (for each segment in the stream
!       network) which will result in both conservation of mass and non-negative
!       flows. Another benefit is that only one input parameter (K) needs to be
!       input to the module. 
!
!   3. If the travel time of a segment is less than or equal to the routing
!      time step (one hour), then the outflow of the segment is set to the
!      value of the inflow.
!
!***********************************************************************
      MODULE PRMS_MUSKINGUM
      IMPLICIT NONE
!   Local Variables
      DOUBLE PRECISION, PARAMETER :: ONE_24TH = 1.0D0 / 24.0D0
      INTEGER, SAVE, ALLOCATABLE :: Ts_i(:)
      REAL, SAVE, ALLOCATABLE :: Ts(:), C0(:), C1(:), C2(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Currinsum(:), Pastin(:), Pastout(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Outflow_ts(:), Inflow_ts(:)
      CHARACTER(LEN=9), SAVE :: MODNAME
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_segment_storage
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_delta_flow(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: K_coef(:), X_coef(:), Segment_flow_init(:)
      END MODULE PRMS_MUSKINGUM

!***********************************************************************
!     Main muskingum routine
!***********************************************************************
      INTEGER FUNCTION muskingum()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: muskingum_decl, muskingum_init, muskingum_run
      EXTERNAL :: muskingum_restart
!***********************************************************************
      muskingum = 0

      IF ( Process(:3)=='run' ) THEN
        muskingum  = muskingum_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        muskingum  = muskingum_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL muskingum_restart(1)
        muskingum = muskingum_init()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL muskingum_restart(0)
      ENDIF

      END FUNCTION muskingum

!***********************************************************************
!     muskingum_decl - Declare parameters and variables and allocate arrays
!   Declared Parameters
!     tosegment, hru_segment, obsin_segment, K_coef, x_coef
!***********************************************************************
      INTEGER FUNCTION muskingum_decl()
      USE PRMS_MUSKINGUM
      USE PRMS_MODULE, ONLY: Model, Nsegment, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_muskingum
!***********************************************************************
      muskingum_decl = 0

      Version_muskingum = '$Id: muskingum.f90 7588 2015-08-18 22:58:42Z rsregan $'
      CALL print_module(Version_muskingum, 'Streamflow Routing          ', 90)
      MODNAME = 'muskingum'

      IF ( Nsegment<1 ) THEN
        IF ( Model==99 ) THEN
          Nsegment = 1
        ELSE
          PRINT *, 'ERROR, muskingum module requires nsegment > 0, specified as:', Nsegment
          STOP
        ENDIF
      ENDIF

      ALLOCATE ( C1(Nsegment), C2(Nsegment), C0(Nsegment), Ts(Nsegment) )
      ALLOCATE ( Currinsum(Nsegment), Ts_i(Nsegment) )
      ALLOCATE ( Pastin(Nsegment), Pastout(Nsegment) )
      ALLOCATE ( Outflow_ts(Nsegment), Inflow_ts(Nsegment) )

      IF ( declvar(MODNAME, 'basin_segment_storage', 'one', 1, 'double', &
     &     'Basin area-weighted average storage in the stream network', &
     &     'inches', Basin_segment_storage)/=0 ) CALL read_error(3, 'basin_segment_storage')

      ALLOCATE ( Segment_delta_flow(Nsegment) )
      IF ( declvar(MODNAME, 'segment_delta_flow', 'nsegment', Nsegment, 'double', &
     &     'Cummulative flow in minus flow out for each stream segment', &
     &     'cfs', Segment_delta_flow)/=0 ) CALL read_error(3, 'segment_delta_flow')

      ALLOCATE ( K_coef(Nsegment) )
      IF ( declparam(MODNAME, 'K_coef', 'nsegment', 'real', &
     &     '1.0', '0.01', '24.0', &
     &     'Muskingum storage coefficient', &
     &     'Travel time of flood wave from one segment to the next downstream segment,'// &
     &     ' called the Muskingum storage coefficient; enter 1.0 for reservoirs,'// &
     &     ' diversions, and segment(s) flowing out of the basin', &
     &     'hours')/=0 ) CALL read_error(1, 'K_coef')

      ALLOCATE ( X_coef(Nsegment) )
      IF ( declparam(MODNAME, 'x_coef', 'nsegment', 'real', &
     &     '0.2', '0.0', '0.5', &
     &     'Routing weighting factor', &
     &     'The amount of attenuation of the flow wave, called the'// &
     &     ' Muskingum routing weighting factor; enter 0.0 for'// &
     &     ' reservoirs, diversions, and segment(s) flowing out of the basin', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'x_coef')

      IF ( Init_vars_from_file==0 ) THEN
        ALLOCATE ( Segment_flow_init(Nsegment) )
        IF ( declparam(MODNAME, 'segment_flow_init', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0E7', &
     &       'Initial flow in each stream segment', &
     &       'Initial flow in each stream segment', &
     &       'cfs')/=0 ) CALL read_error(1, 'x_coef')
      ENDIF

      END FUNCTION muskingum_decl

!***********************************************************************
!    muskingum_init - Get and check parameter values and initialize variables
!***********************************************************************
      INTEGER FUNCTION muskingum_init()
      USE PRMS_MUSKINGUM
      USE PRMS_MODULE, ONLY: Nsegment, Inputerror_flag, Parameter_check_flag, &
     &    Init_vars_from_file
      USE PRMS_BASIN, ONLY: NEARZERO, Basin_area_inv
      USE PRMS_FLOWVARS, ONLY: Seg_inflow, Seg_outflow
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      IMPLICIT NONE
      INTRINSIC ABS, NINT
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, ierr, ierr_flag
      REAL :: k, x, d, x_max
!***********************************************************************
      muskingum_init = 0

      IF ( getparam(MODNAME, 'K_coef', Nsegment, 'real', K_coef)/=0 ) CALL read_error(2, 'K_coef')
      IF ( getparam(MODNAME, 'x_coef', Nsegment, 'real', X_coef)/=0 ) CALL read_error(2, 'x_coef')
      IF ( Init_vars_from_file==0 ) THEN
        IF ( getparam(MODNAME, 'segment_flow_init',  Nsegment, 'real', Segment_flow_init)/=0 ) &
     &       CALL read_error(2,'segment_flow_init')
        Seg_inflow = 0.0D0
        DO i = 1, Nsegment
          Seg_outflow(i) = Segment_flow_init(i)
        ENDDO
        DEALLOCATE ( Segment_flow_init )
        Segment_delta_flow = 0.0D0
        Outflow_ts = 0.0D0
      ENDIF
!
!     Compute the three constants in the Muskingum routing equation based
!     on the values of K_coef and a routing period of 1 hour. See the notes
!     at the top of this file.

!  if c2 is <= 0.0 then  short travel time though reach (less daily
!  flows), thus outflow is mainly = inflow w/ small influence of previous
!  inflow. Therefore, keep c0 as is, and lower c1 by c2, set c2=0

!  if c0 is <= 0.0 then long travel time through reach (greater than daily
!  flows), thus mainly dependent on yesterdays flows.  Therefore, keep
!  c2 as is, reduce c1 by c0 and set c0=0
!
      C0 = 0.0
      C1 = 0.0
      C2 = 0.0
!make sure K>0
      Ts = 1.0
      ierr = 0
      Basin_segment_storage = 0.0D0
      DO i = 1, Nsegment
        Basin_segment_storage = Basin_segment_storage + Seg_outflow(i)
        ierr_flag = 0
        k = K_coef(i)
        x = X_coef(i)

! check the values of k and x to make sure that Muskingum routing is stable

        IF ( k<0.01 ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT *, 'ERROR, K_coef value < 0.01 for segment:', i, 'K_coef:', k
            ierr_flag = 1
          ELSE
            PRINT *, 'WARNING in muskingum: segment ', i, ' has K_coef < 0.01,', k, ', set to 0.01'
            !PRINT *, 'Outflow for this segment is set to the inflow and there will be no lag or attenuation'
            ierr = 1
          ENDIF
          k = 0.01

        ELSEIF ( k>24.0 ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT *, 'ERROR, K_coef value > 24.0 for segment:', i, 'K_coef:', k
            ierr_flag = 1
          ELSE
            PRINT *, 'WARNING in muskingum: segment ', i, ' has K_coef > 24.0,', k, ', set to 24.0'
            !PRINT *, 'Outflow for this segment is set to the maximum lag and attenuation'
            ierr = 1
          ENDIF
          Ts(i) = 24.0
          k = 24.0

        ELSEIF ( k<2.0 ) THEN
          IF ( k<1.0 ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT *, 'WARNING in muskingum: segment ', i, ' has K_coef < 1.0,', k, ' this may produce unstable results'
              ierr = 1
            ENDIF
          ENDIF
          Ts(i) = 1.0

        ELSEIF ( k<3.0 ) THEN
          Ts(i) = 2.0

        ELSEIF ( k<4.0 ) THEN
          Ts(i) = 3.0

        ELSEIF ( k<6.0 ) THEN
          Ts(i) = 4.0

        ELSEIF ( k<8.0 ) THEN
          Ts(i) = 6.0

        ELSEIF ( k<12.0 ) THEN
          Ts(i) = 8.0

        ELSEIF ( k<24.0 ) THEN
          Ts(i) = 12.0

        ELSE
          Ts(i) = 24.0

        ENDIF
        Ts_i(i) = NINT( Ts(i) )

        IF ( x>1.0 ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT *, 'ERROR, x_coef value > 1.0 for segment:', i, x
            ierr_flag = 1
          ELSE
            PRINT *, 'WARNING in muskingum: segment ', i, ' x_coef value > 1.0', x, ', set to 1.0'
            x = 1.0
            ierr = 1
          ENDIF
        ENDIF
        IF ( ierr_flag==1 ) Inputerror_flag = 1

!  x must be <= t/(2K) the C coefficents will be negative. Check for this for all segments
!  with Ts >= minimum Ts (1 hour).
        IF ( Ts(i)>0.1 ) THEN
          x_max = Ts(i) / (2.0 * k)
          IF ( x>x_max ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT *, 'ERROR, x_coef value is TOO LARGE for stable routing for segment:', i, x
              PRINT *, 'a maximum value of:', x_max, ' is suggested'
              Inputerror_flag = 1
              CYCLE
            ELSE
              PRINT *, 'WARNING in muskingum, segment:', i, ', x_coef value is TOO LARGE for stable routing'
              PRINT *, 'x_coef is set to the suggested maximum value:', x_max
              x = x_max
              ierr = 1
            ENDIF
          ENDIF
        ENDIF

        d = k - (k * x) + (0.5 * Ts(i))
        IF ( ABS(d)<NEARZERO ) THEN
          PRINT *, 'WARNING in muskingum, segment ', i, ' computed value d <', NEARZERO, ', set to 0.0001'
          d = 0.0001
        ENDIF
        C0(i) = (-(k * x) + (0.5 * Ts(i))) / d
        C1(i) = ((k * x) + (0.5 * Ts(i))) / d 
        C2(i) = (k - (k * x) - (0.5 * Ts(i))) / d

        ! the following commented out code was in the original musroute, but, not in Linsley and others
        ! or needed due to internal hourly time loop
! SHORT travel time
!        IF ( C2(i)<0.0 ) THEN
!          IF ( Parameter_check_flag>0 ) THEN
!            PRINT '(/,A)', 'WARNING, c2 < 0, set to 0, c1 set to c1 + c2'
!            PRINT *, '        old c2:', C2(i), 'old c1:', C1(i), 'new c1:', C1(i) + C2(i)
!          ENDIF
!          C1(i) = C1(i) + C2(i)
!          C2(i) = 0.0
!        ENDIF

! LONG travel time
!        IF ( C0(i)<0.0 ) THEN
!          IF ( Parameter_check_flag>0 ) THEN
!            PRINT '(/,A)', 'WARNING, c0 < 0, set to 0, c0 set to c1 + c0'
!            PRINT *, '      old c0:', C0(i), 'old c1:', C1(i), 'new c1:', C1(i) + C0(i)
!          ENDIF
!          C1(i) = C1(i) + C0(i)
!          C0(i) = 0.0
!        ENDIF

      ENDDO
      IF ( ierr==1 ) PRINT *, '***RECOMMEND THAT YOU FIX Muskingum parameters in your Parameter File***'
      DEALLOCATE ( K_coef, X_coef)

      Basin_segment_storage = Basin_segment_storage*Basin_area_inv/Cfs_conv

      END FUNCTION muskingum_init

!***********************************************************************
!     muskingum_run - Compute routing summary values
!***********************************************************************
      INTEGER FUNCTION muskingum_run()
      USE PRMS_MUSKINGUM
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Basin_area_inv, Obsin_segment, Segment_order, Tosegment
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_cms, Basin_gwflow_cfs, Basin_ssflow_cfs, &
     &    Basin_stflow_out, Basin_cfs, Basin_stflow_in, Basin_sroff_cfs, Seg_inflow, Seg_outflow, Seg_upstream_inflow, &
     &    Seg_lateral_inflow, Flow_out
      USE PRMS_OBS, ONLY: Streamflow_cfs
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Basin_sroff
      USE PRMS_GWFLOW, ONLY: Basin_gwflow
      IMPLICIT NONE
! Functions
      INTRINSIC MOD
! Local Variables
      INTEGER :: i, j, iorder, toseg, imod, tspd
      DOUBLE PRECISION :: area_fac, currin
!***********************************************************************
      muskingum_run = 0

!     SET yesterdays inflows and outflows into temp (past arrays)
!     values may be 0.0 as intial, > 0.0 for runtime and dynamic 
!     initial condtions. Then set outlfow and inflow for this time
!     step to 0.0
!
!     upstream_inflow and outflow will vary by hour
!     lateral_inflow and everything else will vary by day
!
!     Compute surface runoff, ssflow, and gwflow going to each segment
!     This is todays "seg_inflow" before additional water is routed to
!     a new (if any is routed)
!
!     For each HRU if the lateral flow for this HRU goes to the 
!     segment being evaluated (segment i) then sum flows
!
!     Do these calculations once for the current day, before the hourly
!     routing starts.
!
!       Out2   =      In2*C0    +        In1*C1    +          Out1*C2
!     Seg_outflow = Seg_inflow*Czero + Pastinflow*Cone + Pastoutflow*Ctwo
!       C0, C1, and C2: initialized in the "init" part of this module
!
      Pastin = Seg_inflow
      Pastout = Seg_outflow
      Seg_inflow = 0.0D0
      Seg_outflow = 0.0D0
      Inflow_ts = 0.0D0
      Currinsum = 0.0D0

! 24 hourly timesteps per day
      DO j = 1, 24

        Seg_upstream_inflow = 0.0D0
        DO i = 1, Nsegment
          iorder = Segment_order(i)

! current inflow to the segment is the time weighted average of the outflow
! of the upstream segments plus the lateral HRU inflow plus any gains.
          currin = Seg_lateral_inflow(iorder)
          IF ( Obsin_segment(iorder)>0 ) Seg_upstream_inflow(iorder) = Streamflow_cfs(Obsin_segment(iorder))
          currin = currin + Seg_upstream_inflow(iorder)
          Seg_inflow(iorder) = Seg_inflow(iorder) + currin
          Inflow_ts(iorder) = Inflow_ts(iorder) + currin
          Currinsum(iorder) = Currinsum(iorder) + Seg_upstream_inflow(iorder)

          ! Check to see if this segment is to be routed on this time step
          tspd = Ts_i(iorder)
          imod = MOD( j, tspd )
          IF ( imod==0 ) THEN
            Inflow_ts(iorder) = (Inflow_ts(iorder) / Ts(iorder))
! Compute routed streamflow
            !IF ( Ts_i(i)>0 ) THEN
! Muskingum routing equation
              Outflow_ts(iorder) = Inflow_ts(iorder)*C0(iorder) + Pastin(iorder)*C1(iorder) + Outflow_ts(iorder)*C2(iorder)
            !ELSE
! If travel time (K_coef paremter) is less than or equal to
! time step (one hour), then the outflow is equal to the inflow
! Outflow_ts is the value from last hour
              !Outflow_ts(iorder) = Inflow_ts(iorder)
            !ENDIF

            ! pastin is equal to the Inflow_ts on the previous routed timestep
            Pastin(iorder) = Inflow_ts(iorder)

! because the upstream inflow from streams is used, reset it to zero so new average
! can be computed next routing timestep.
            Inflow_ts(iorder) = 0.0D0
          ENDIF

          ! Seg_outflow (the mean daily flow rate for each segment) will be the average of the hourly values.
          Seg_outflow(iorder) = Seg_outflow(iorder) + Outflow_ts(iorder)
          ! pastout is equal to the Inflow_ts on the previous routed timestep
          Pastout(iorder) = Outflow_ts(iorder)

! Add current timestep's flow rate to sum the upstream flow rates.
! This can be thought of as a volume because it is a volumetric rate
! (cubic feet per second) over a time step of an hour. Down below when
! this value is used, it will be divided by the number of hours in the
! segment's simulation time step, giving the mean flow rate over that
! period of time.
          toseg = Tosegment(iorder)
          IF ( toseg>0 ) Seg_upstream_inflow(toseg) = Seg_upstream_inflow(toseg) + Outflow_ts(iorder)

        ENDDO ! segment

      ENDDO  ! timestep

      Basin_segment_storage = 0.0D0
      Flow_out = 0.0D0
      DO i = 1, Nsegment
        Seg_outflow(i) = Seg_outflow(i) * ONE_24TH
        Seg_inflow(i) = Seg_inflow(i) * ONE_24TH
        Seg_upstream_inflow(i) = Currinsum(i) * ONE_24TH
! Flow_out is the total flow out of the basin, which allows for multiple outlets
        IF ( Tosegment(i)==0 ) Flow_out = Flow_out + Seg_outflow(i)
        Segment_delta_flow(i) = Segment_delta_flow(i) + Seg_inflow(i) - Seg_outflow(i)
        Basin_segment_storage = Basin_segment_storage + Segment_delta_flow(i)
      ENDDO

      area_fac = Cfs_conv/Basin_area_inv
      Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow ! not equal to stflow_out if replacement flows
      Basin_cfs = Flow_out
      Basin_stflow_out = Basin_cfs / area_fac
      Basin_cms = Basin_cfs*CFS2CMS_CONV
      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac
      Basin_segment_storage = Basin_segment_storage/area_fac

      END FUNCTION muskingum_run

!***********************************************************************
!     muskingum_restart - write or read restart file
!***********************************************************************
      SUBROUTINE muskingum_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_MUSKINGUM
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Function
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=9) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_segment_storage
        WRITE ( Restart_outunit ) Segment_delta_flow
        WRITE ( Restart_outunit ) Outflow_ts
        WRITE ( Restart_outunit ) Pastin
        WRITE ( Restart_outunit ) Pastout
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_segment_storage
        READ ( Restart_inunit ) Segment_delta_flow
        READ ( Restart_inunit ) Outflow_ts
        READ ( Restart_inunit ) Pastin
        READ ( Restart_inunit ) Pastout
      ENDIF
      END SUBROUTINE muskingum_restart

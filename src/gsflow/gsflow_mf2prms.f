!***********************************************************************
! Transfer MODFLOW data to PRMS
!***********************************************************************
      MODULE GSFMF2PRMS
      IMPLICIT NONE
! Local Variables
      CHARACTER(LEN=14) :: MODNAME
      PARAMETER(MODNAME='gsflow_mf2prms')
      CHARACTER(LEN=26) PROCNAME
      PARAMETER(PROCNAME='GSFLOW integration')
! Declared Variables
      REAL, SAVE, ALLOCATABLE :: Gw2sm_grav(:)
      END MODULE GSFMF2PRMS

!     ******************************************************************
!     Mapping module to convert MODFLOW to PRMS states for use by GSFLOW
!   Declared Parameters
!     gvr_hru_id, gvr_cell_id
!     ******************************************************************
      INTEGER FUNCTION gsflow_mf2prms()
      USE GSFMF2PRMS
      USE GSFCONVERT, ONLY: Mfq2inch_conv, Gwc_col, Gwc_row, Nhrucell,
     &    Gvr_cell_id
      USE GSFPRMS2MF, ONLY: Hrucheck, Gvr_hru_id
      USE GWFUZFMODULE, ONLY: SEEPOUT
      USE PRMS_MODULE, ONLY: Process_flag, Version_gsflow_mf2prms,
     &    Gsflow_mf2prms_nc
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declmodule, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, irow, icol, icell
!***********************************************************************
      gsflow_mf2prms = 0

      IF ( Process_flag==0 ) THEN
        DO i = 1, Nhrucell
          IF ( Hrucheck(Gvr_hru_id(i))==1 ) THEN
            icell = Gvr_cell_id(i)
            irow = Gwc_row(icell)
            icol = Gwc_col(icell)
! SEEPOUT is a discharge L3/T
!            IF ( SEEPOUT(icol, irow)>0.0 ) THEN
              Gw2sm_grav(i) = SEEPOUT(icol, irow)*Mfq2inch_conv(i)
!            ELSE
!              Gw2sm_grav(i) = 0.0
!            ENDIF
          ENDIF
        ENDDO

      ELSEIF ( Process_flag==1 ) THEN
      Version_gsflow_mf2prms =
     &'$Id: gsflow_mf2prms.f 4262 2012-03-08 18:29:07Z rsregan $'
      Gsflow_mf2prms_nc = INDEX( Version_gsflow_mf2prms, 'Z' )
      i = INDEX( Version_gsflow_mf2prms, '.f' ) + 1
      IF ( declmodule(Version_gsflow_mf2prms(6:i), PROCNAME,
     +     Version_gsflow_mf2prms(i+2:Gsflow_mf2prms_nc))/=0 ) STOP

! Declared Variables
        ALLOCATE (Gw2sm_grav(Nhrucell))
        IF ( declvar(MODNAME, 'gw2sm_grav', 'nhrucell', Nhrucell,
     &       'real',
     &       'Groundwater discharge to gravity-flow reservoirs',
     &       'inches', Gw2sm_grav)/=0 )
     &       CALL read_error(3, 'gw2sm_grav')

      ELSEIF ( Process_flag==2 ) THEN
        IF ( Timestep==0 ) Gw2sm_grav = 0.0
      ENDIF

      END FUNCTION gsflow_mf2prms


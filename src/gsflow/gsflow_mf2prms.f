!***********************************************************************
! Transfer MODFLOW data to PRMS
!***********************************************************************
      MODULE GSFMF2PRMS
      IMPLICIT NONE
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Gw2sm_grav(:)
      END MODULE GSFMF2PRMS

!     ******************************************************************
!     Mapping module to convert MODFLOW to PRMS states for use by GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_mf2prms()
      USE GSFMF2PRMS
      USE PRMS_BASIN, ONLY: Timestep
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: mf2prmsdecl, mf2prmsrun
!***********************************************************************
      gsflow_mf2prms = 0

      IF ( Process_flag==0 ) THEN
        gsflow_mf2prms = mf2prmsrun()
      ELSEIF ( Process_flag==1 ) THEN
        gsflow_mf2prms = mf2prmsdecl()
      ELSEIF ( Process_flag==2 ) THEN
        IF ( Timestep==0 ) Gw2sm_grav = 0.0
      ENDIF

      END FUNCTION gsflow_mf2prms

!***********************************************************************
!     mf2prmsdecl - set up parameters
!   Declared Parameters
!     gvr_hru_id, gvr_cell_id
!***********************************************************************
      INTEGER FUNCTION mf2prmsdecl()
      USE GSFMF2PRMS
      USE GSFCONVERT, ONLY: Nhrucell
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declvar
!***********************************************************************
      mf2prmsdecl = 1

      IF ( declmodule(
     &'$Id: gsflow_mf2prms.f 3116 2011-05-17 16:20:01Z rsregan $')
     &     .NE.0 ) RETURN

! Declared Variables
      ALLOCATE (Gw2sm_grav(Nhrucell))
      IF ( declvar('mf2prms', 'gw2sm_grav', 'nhrucell', Nhrucell,
     &     'real', 'Ground-water discharge to gravity-flow reservoirs',
     &     'inches', Gw2sm_grav).NE.0 ) RETURN

      mf2prmsdecl = 0

      END FUNCTION mf2prmsdecl

!***********************************************************************
! mf2prmsrun: Maps MODFLOW results to PRMS HRU & gravity-flow cells
!***********************************************************************
      INTEGER FUNCTION mf2prmsrun()
      USE GSFMF2PRMS
      USE GSFPRMS2MF, ONLY:Hrucheck, Gvr_hru_id
      USE GSFCONVERT, ONLY:Mfq2inch_conv, Gwc_col, Gwc_row, Nhrucell,
     &    Gvr_cell_id
      USE GWFUZFMODULE, ONLY:SEEPOUT
      IMPLICIT NONE
      INTRINSIC SNGL
! Local Variables
      INTEGER :: i, irow, icol, icell
!***********************************************************************
      DO i = 1, Nhrucell
        IF ( Hrucheck(Gvr_hru_id(i))==1 ) THEN
          icell = Gvr_cell_id(i)
          irow = Gwc_row(icell)
          icol = Gwc_col(icell)
! SEEPOUT is a discharge L3/T
!          IF ( SEEPOUT(icol, irow)>0.0 ) THEN
            Gw2sm_grav(i) = SEEPOUT(icol, irow)*Mfq2inch_conv(i)
!          ELSE
!            Gw2sm_grav(i) = 0.0
!          ENDIF
        ENDIF
      ENDDO

      mf2prmsrun = 0

      END FUNCTION mf2prmsrun

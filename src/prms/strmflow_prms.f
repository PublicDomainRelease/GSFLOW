!***********************************************************************
! Computes daily streamflow as the sum of surface runoff,
! shallow-subsurface flow, and ground-water flow
!***********************************************************************
      INTEGER FUNCTION strmflow_prms()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule
! Functions
      INTEGER, EXTERNAL :: strmrun
!***********************************************************************
      strmflow_prms = 1

      IF ( Process_flag==0 ) THEN
        strmflow_prms = strmrun()
      ELSEIF ( Process_flag==1 ) THEN
        IF ( declmodule(
     +'$Id: strmflow_prms.f 2241 2010-12-10 00:37:28Z rsregan $'
     +).NE.0 ) RETURN
      ENDIF

      strmflow_prms = 0
      END FUNCTION strmflow_prms

!***********************************************************************
!     strmrun - Computes basin streamflow
!***********************************************************************
      INTEGER FUNCTION strmrun()
      USE PRMS_BASIN, ONLY: Basin_area_inv, CFS2CMS_CONV, Basin_cfs,
     +    Basin_cms, Basin_stflow, Basin_sroff_cfs, Basin_ssflow_cfs,
     +    Basin_gwflow_cfs
      USE PRMS_GWFLOW_CASC, ONLY: Basin_gwflow
      USE PRMS_FLOWVARS, ONLY: Basin_sroff, Basin_ssflow
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
! Local Variables
      REAL :: area_fac
!***********************************************************************
      area_fac = Cfs_conv/Basin_area_inv

!   Compute daily flow.
      Basin_stflow = Basin_sroff + Basin_gwflow + Basin_ssflow
      Basin_cfs = Basin_stflow*area_fac
      Basin_cms = Basin_cfs*CFS2CMS_CONV

      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac

      strmrun = 0
      END FUNCTION strmrun

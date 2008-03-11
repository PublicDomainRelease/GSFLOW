c **********************************************************************
c $Id: fmodules.inc 3755 2008-01-25 20:03:27Z rsregan $
c system include file for all fortran modules
c **********************************************************************

      INTEGER, EXTERNAL :: decldim, declfix, declparam, declvar, declpri
      DOUBLE PRECISION, EXTERNAL :: deltim, delnex, djulian
      INTEGER, EXTERNAL :: getdim, getparam, getstep
      INTEGER, EXTERNAL :: getvar, readvar, putvar
      INTEGER, EXTERNAL :: julian, isleap
      INTEGER, EXTERNAL :: getdataname, getoutname, getoutdirfile
      INTEGER, EXTERNAL :: declmodule, control_string, control_integer

      EXTERNAL dattim, opstr, dpint4, dpstr, dpdble, dpreal
      EXTERNAL decl_control

! fmodules.inc
!     MODULE PRMS_DATA
! Dimensions
        INTEGER, PARAMETER :: MAXHRU =    55000
        INTEGER, PARAMETER :: MAXGROUP =     50
        INTEGER, PARAMETER :: MAXMRU =      500
        INTEGER, PARAMETER :: MAXGWR =    55000
        INTEGER, PARAMETER :: MAXSSR =    55000
        INTEGER, PARAMETER :: MAXRADPL =    300
        INTEGER, PARAMETER :: MAXRAIN =      25
        INTEGER, PARAMETER :: MAXTEMP =      25
        INTEGER, PARAMETER :: MAXOBS =       25
        INTEGER, PARAMETER :: MAXSOL =       25
        INTEGER, PARAMETER :: MAXEVAP =      25
        INTEGER, PARAMETER :: MAXWIND =      25
        INTEGER, PARAMETER :: MAXHUMID =     25
        INTEGER, PARAMETER :: MAXSNODPL =    25
        INTEGER, PARAMETER :: MAXCASCADE = 5000
!       INTEGER, PARAMETER :: MAXDOWN =      25
        INTEGER, PARAMETER :: MAXHRUCELL =55000
        INTEGER, PARAMETER :: MAXSEGMENT =  500
        INTEGER, PARAMETER :: MAXREACH =   1000
        INTEGER, PARAMETER :: MAXGWCELL = 55000
        INTEGER, PARAMETER :: MAXSFR =      100
        INTEGER, PARAMETER :: MAXNSOS =      25
        INTEGER, PARAMETER :: MAXCHAN =     200
        INTEGER, PARAMETER :: MAXOFPL =     500
        INTEGER, PARAMETER :: MAXCHXS =      21
        INTEGER, PARAMETER :: MAXOFXS =      21
        INTEGER, PARAMETER :: MAXMOC =      250
        INTEGER, PARAMETER :: MAXCDELS =     60
        INTEGER, PARAMETER :: MAXGAIN =       5
        INTEGER, PARAMETER :: MAXGATE =       5
        INTEGER, PARAMETER :: MAXDIVERT =     5
        INTEGER, PARAMETER :: MAXTSTEP =     60
        INTEGER, PARAMETER :: MAXSUB =      100
!       INTEGER, PARAMETER :: MAXNODE =      20
!       INTEGER, PARAMETER :: MAXSC =        10
!       INTEGER, PARAMETER :: MAXRTDELAY =   20
!       INTEGER, PARAMETER :: MAXLAG =        5
! do not change the following PARAMETERS
        INTEGER, PARAMETER :: MAXFORM = 1
        INTEGER, PARAMETER :: MAXDAY = 366
        INTEGER, PARAMETER :: MAXMO = 12
        INTEGER, PARAMETER :: MAXLAPSE = 3
        INTEGER, PARAMETER :: MAXALB = 15
        INTEGER, PARAMETER :: MAXDEPLVAL = MAXSNODPL*11
        INTEGER, PARAMETER :: MAXSFR1 = MAXSFR + 1
! Constants
!       REAL, PARAMETER :: NEARZERO = 1.0E-15, SMALL = 0.000001
!       REAL, PARAMETER :: ONETHIRD = 1.0/3.0, TWOTHIRDS = 2.0/3.0
!       REAL, PARAMETER :: FOURTHIRDS = 4.0/3.0, FIVETHIRDS = 5.0/3.0
!       REAL, PARAMETER :: FIVE_NINTHS = 5.0/9.0, NINE_FIFTHS = 9.0/5.0
!       REAL, PARAMETER :: FEET2METERS = 0.304800, CFS2CMS = 0.028316844
!       REAL, PARAMETER :: INCH2CM = 2.54, INCH2MM = 25.4
!       REAL, PARAMETER :: TSTEPCHK = 23.999, CONVRG = 0.001
!       DOUBLE PRECISION, PARAMETER :: DTSTEPCHK = 23.999D0
!     END MODULE PRMS_DATA

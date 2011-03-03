c **********************************************************************
c $Id: fmodules.inc 1456 2010-02-23 17:42:39Z rsregan $
c system include file for all fortran modules
c **********************************************************************

      INTEGER, EXTERNAL :: decldim, declfix, declparam, declvar, declpri
      DOUBLE PRECISION, EXTERNAL :: deltim, delnex, djulian
      INTEGER, EXTERNAL :: getdim, getparam, getstep, declparam_u
      INTEGER, EXTERNAL :: getvar, readvar, putvar
      INTEGER, EXTERNAL :: julian, isleap
      INTEGER, EXTERNAL :: getdataname, getoutname, getoutdirfile
      INTEGER, EXTERNAL :: declmodule, control_string, control_integer
      INTEGER, EXTERNAL :: declparam_p

      EXTERNAL dattim, opstr, dpint4, dpstr, dpdble, dpreal
      EXTERNAL decl_control

! Dimensions
        INTEGER, PARAMETER :: MAXHRU =    55000
        INTEGER, PARAMETER :: MAXGWR =    55000
        INTEGER, PARAMETER :: MAXSSR =    55000
        INTEGER, PARAMETER :: MAXRAIN =      25
        INTEGER, PARAMETER :: MAXTEMP =      25
        INTEGER, PARAMETER :: MAXOBS =       25
        INTEGER, PARAMETER :: MAXSOL =       25
        INTEGER, PARAMETER :: MAXSNOW =      25
        INTEGER, PARAMETER :: MAXEVAP =      25
        INTEGER, PARAMETER :: MAXWIND =      25
        INTEGER, PARAMETER :: MAXHUMID =     25
        INTEGER, PARAMETER :: MAXSNODPL =    25
        INTEGER, PARAMETER :: MAXCASCADE = 5000
        INTEGER, PARAMETER :: MAXNHRUHRAP = 500
        INTEGER, PARAMETER :: MAXHRAPCELL = 500
        INTEGER, PARAMETER :: MAXDOWN =      10
        INTEGER, PARAMETER :: MAXHRUCELL =55000
        INTEGER, PARAMETER :: MAXSEGMENT =  500
        INTEGER, PARAMETER :: MAXSFR =      100
        INTEGER, PARAMETER :: MAXREACH =   1000
        INTEGER, PARAMETER :: MAXGWCELL = 55000
        INTEGER, PARAMETER :: MAXSUB =      100
! do not change the following PARAMETERS
        INTEGER, PARAMETER :: MAXFORM = 1
        INTEGER, PARAMETER :: MAXDAY = 366
        INTEGER, PARAMETER :: MAXMO = 12
        INTEGER, PARAMETER :: MAXLAPSE = 3
        INTEGER, PARAMETER :: MAXALB = 15
        INTEGER, PARAMETER :: MAXDEPLVAL = MAXSNODPL*11
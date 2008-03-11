      MODULE GWFUZFMODULE
        CHARACTER(LEN=64),PARAMETER :: Version_uzf =
     +'$Id: gwf2uzf1.f 3918 2008-02-29 22:41:29Z rsregan $'
        REAL,PARAMETER :: CLOSEZERO=1.0E-15
        DOUBLE PRECISION,PARAMETER :: NEARZERO=1.0D-30
        DOUBLE PRECISION,PARAMETER :: ZEROD15=1.0D-15, ZEROD9=1.0D-09
        DOUBLE PRECISION,PARAMETER :: ZEROD6=1.0D-06
        DOUBLE PRECISION,PARAMETER :: ZEROD7=1.0D-07
        INTEGER         ,PARAMETER :: IRUNBIG = 10000
        DOUBLE PRECISION,SAVE :: THETAB, FLUXB, FLUXHLD2
        INTEGER,SAVE,POINTER   ::NUZTOP, IUZFOPT, IRUNFLG, IETFLG, IUZM
        INTEGER,SAVE,POINTER   ::IUZFCB1, IUZFCB2, NTRAIL, NWAV, NSETS
        INTEGER,SAVE,POINTER   ::NUZGAG, NUZGAGAR, NUZCL, NUZRW, IGSFLOW
        INTEGER,SAVE,  DIMENSION(:),    POINTER :: ITRLSTH
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: IRUNBND, IUZFBND
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: IUZLIST, NWAVST
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: IUZHOLD
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: LTRLIT, LTRLST
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: ITRLIT, ITRLST
        REAL,   SAVE,POINTER   ::TOTRUNOFF, SURFDEP, SURFDEP1
        REAL,   SAVE,  DIMENSION(:),    POINTER :: FBINS
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: SEEPOUT, EXCESPP, VKS
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: REJ_INF
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: TO_CFP
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: EPS, THTS, THTI
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: PETRATE, ROOTDPTH
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: WCWILT, FINF
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: UZFETOUT, GWET
        DOUBLE PRECISION, SAVE, DIMENSION(:),  POINTER :: CUMUZVOL 
        DOUBLE PRECISION, SAVE, DIMENSION(:),  POINTER :: UZTSRAT
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: THTR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZFLWT, UZSTOR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZDPIT, UZDPST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZTHIT, UZTHST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZSPIT, UZSPST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZFLIT, UZFLST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: DELSTOR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZOLSFLX
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: HLDUZF
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:),POINTER :: UZTOTBAL
      TYPE GWFUZFTYPE
        INTEGER,     POINTER   ::NUZTOP, IUZFOPT, IRUNFLG, IETFLG, IUZM
        INTEGER,     POINTER   ::IUZFCB1, IUZFCB2, NTRAIL, NWAV, NSETS
        INTEGER,     POINTER   ::NUZGAG, NUZGAGAR, NUZCL, NUZRW, IGSFLOW
        INTEGER,       DIMENSION(:),    POINTER :: ITRLSTH
        INTEGER,       DIMENSION(:,:),  POINTER :: IRUNBND, IUZFBND
        INTEGER,       DIMENSION(:,:),  POINTER :: IUZLIST, NWAVST
        INTEGER,       DIMENSION(:,:),  POINTER :: IUZHOLD
        INTEGER,       DIMENSION(:,:),  POINTER :: LTRLIT, LTRLST
        INTEGER,       DIMENSION(:,:),  POINTER :: ITRLIT, ITRLST
        REAL,        POINTER   ::TOTRUNOFF, SURFDEP, SURFDEP1
        REAL,          DIMENSION(:),    POINTER :: FBINS
        REAL,          DIMENSION(:,:),  POINTER :: SEEPOUT, EXCESPP, VKS
        REAL,          DIMENSION(:,:),  POINTER :: REJ_INF
        REAL,          DIMENSION(:,:),  POINTER :: TO_CFP
        REAL,          DIMENSION(:,:),  POINTER :: EPS, THTS, THTI
        REAL,          DIMENSION(:,:),  POINTER :: PETRATE, ROOTDPTH
        REAL,          DIMENSION(:,:),  POINTER :: WCWILT, FINF
        REAL,          DIMENSION(:,:),  POINTER :: UZFETOUT, GWET
        DOUBLE PRECISION,       DIMENSION(:),  POINTER :: CUMUZVOL
        DOUBLE PRECISION,       DIMENSION(:),  POINTER :: UZTSRAT
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: THTR
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZFLWT, UZSTOR
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZDPIT, UZDPST
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZTHIT, UZTHST
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZSPIT, UZSPST
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZFLIT, UZFLST
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: DELSTOR
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZOLSFLX
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: HLDUZF
        DOUBLE PRECISION,       DIMENSION(:,:,:),POINTER :: UZTOTBAL
      END TYPE
      TYPE(GWFUZFTYPE), SAVE:: GWFUZFDAT(10)
      END MODULE GWFUZFMODULE

C
C-------SUBROUTINE GWF2UZF1AR
      SUBROUTINE GWF2UZF1AR(In, Iunitbcf, Iunitlpf, Iunithuf, Igrid)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR UNSATURATED FLOW, RECHARGE, AND ET
C     READ AND CHECK VARIABLES THAT REMAIN CONSTANT
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IOUT, ITRSS, ISSFLG, 
     +                        DELR, DELC, IBOUND
      USE GLOBAL,       ONLY: ITMUNI, LENUNI
      USE GWFLPFMODULE, ONLY: SCLPF=>SC2, LAYTYP
      USE GWFBCFMODULE, ONLY: SC1, SC2, LAYCON
      USE GWFHUFMODULE, ONLY: SC2HUF

      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER In, Iunitbcf, Iunitlpf, Iunithuf, Igrid
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION test
      INTEGER istart, istop, lloc, ivol
      INTEGER ibndflg, ichld, iflgbnd, igage, igunit, irhld, isyflg, 
     +        iuzcol, iuzflg, iuzlay, iuzopt, iuzrow, l, ncck, ncth, 
     +        nlth, nrck, nrnc, nrth, i
      REAL r, sy, fkmin, fkmax, range, finc
      CHARACTER(LEN=200) line
      CHARACTER(LEN=24) aname(6)
      DATA aname(1)/' AREAL EXTENT OF UZ FLOW'/
      DATA aname(2)/' ROUTING OVERLAND RUNOFF'/
      DATA aname(3)/' SATURATED WATER CONTENT'/
      DATA aname(4)/'   INITIAL WATER CONTENT'/
      DATA aname(5)/'    BROOKS-COREY EPSILON'/
      DATA aname(6)/'    SATURATED VERTICAL K'/
C     ------------------------------------------------------------------
      ALLOCATE (NUZTOP, IUZFOPT, IRUNFLG, IETFLG, IUZM)
      ALLOCATE (IUZFCB1, IUZFCB2, NTRAIL, NWAV, NSETS)
      ALLOCATE (NUZGAG, NUZGAGAR, NUZCL, NUZRW, TOTRUNOFF)
      ALLOCATE (SURFDEP,SURFDEP1,IGSFLOW)
C
C1------IDENTIFY PACKAGE AND INITIALIZE.
      WRITE (IOUT, 9001) In
 9001 FORMAT (1X, /' UZF1 -- UNSATURATED FLOW PACKAGE, VERSION 1.2', 
     +        ', 01/14/2006', /, 9X, 'INPUT READ FROM UNIT', I3)
      CALL URDCOM(In, IOUT, line)
      lloc = 1
      CALL URWORD(line, lloc, istart, istop, 2, NUZTOP, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IUZFOPT, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IRUNFLG, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IETFLG, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IUZFCB1, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IUZFCB2, r, IOUT, In)
C
C2------READ UNSATURATED FLOW FLAGS WHEN IUZFOPT IS GREATER THAN ZERO.
      IF ( IUZFOPT.GT.0 ) THEN
        CALL URWORD(line, lloc, istart, istop, 2, NTRAIL, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, NSETS, r, IOUT, In)
      END IF
      CALL URWORD(line, lloc, istart, istop, 2, NUZGAG, r, IOUT, In)
      i=1
      CALL URWORD(line, lloc, istart, istop, 3, i, SURFDEP, IOUT, In)
C
C3------CHECK FOR ERRORS.
      IF ( IUZFOPT.GT.2 ) THEN
        WRITE (IOUT, 9002)
 9002   FORMAT (//' VERTICAL FLOW IN VADOSE ZONE IS ', 
     +          'ACTIVE AND IUZFOPT IS NOT 1 OR 2 '
     +          //, ' PLEASE CHECK INPUT  --  SETTING UZF PACKAGE TO ', 
     +          'INACTIVE'///)
        In = 0
        RETURN
      ELSE IF ( IUZFOPT.LE.0 ) THEN
        WRITE (IOUT, 9003)
 9003   FORMAT (//' UNSATURATED FLOW IN VADOSE ZONE IS IGNORED ', 
     +          //'RECHARGE TO GROUND WATER IS EQUAL TO SPECIFIED ', 
     +          'INFILTRATION RATE MINUS REJECTED RECHARGE'///)
        NTRAIL = 1
        NSETS = 1
      END IF
      IF ( ABS(IUZFOPT).EQ.2 .AND. Iunitbcf.GT.0 ) THEN
        WRITE (IOUT, 9004) IUZFOPT
 9004   FORMAT (//'BCF PACKAGE IS ACTIVE AND IUZFOPT = ', I5, 
     +          ' -- '//' ABSOLUTE VALUE OF IUZFOPT MUST EQUAL 1 ', 
     +          'FOR BCF  --  SETTING UZF PACKAGE TO INACTIVE'///)
        In = 0
        RETURN
      END IF
      IF ( NTRAIL.LT.0 ) THEN
        WRITE (IOUT, 9005)
 9005   FORMAT (//' NUMBER OF TRAILING WAVES IS LESS THAN ZERO'//
     +          ' --SETTING NTRAIL TO A POSITIVE VALUE'///)
        NTRAIL = ABS(NTRAIL)
      END IF
      IF ( NTRAIL.EQ.0 ) THEN
        WRITE (IOUT, 9006)
 9006   FORMAT (//' VERTICAL FLOW IN VADOSE ZONE IS ', 
     +          'ACTIVE AND NUMBER OF TRAILING WAVES IS ZERO  -- '//
     +          ' PLEASE CHECK INPUT  --  SETTING UZF PACKAGE TO ', 
     +          'INACTIVE'///)
        In = 0
        RETURN
      END IF
      IF ( IUZFOPT.GT.0 .AND. NSETS.LT.20 ) THEN
        WRITE (IOUT, 9007)
 9007   FORMAT (//' VERTICAL FLOW THROUGH UNSATURATED ZONE IS ', 
     +          'ACTIVE AND NUMBER OF WAVE SETS IS LESS THAN 20-- ', 
     +          ' RESETTING THE NUMBER OF WAVE SETS TO BE 20'///)
        NSETS = 20
      END IF
!      IF ( ABS(IUZFOPT).EQ.2 .AND. Iunithuf.GT.0 ) THEN
!        WRITE (IOUT, 9008) IUZFOPT
! 9008   FORMAT (//' VERTICAL FLOW IN VADOSE ZONE IS ', 
!     +          'ACTIVE, HUF PACKAGE IS ACTIVE AND IUZFOPT = ', I5, 
!     +          ' -- '//' ABSOLUTE VALUE OF IUZFOPT MUST EQUAL 1 ', 
!     +          'FOR HUF  --  SETTING UZF PACKAGE TO INACTIVE'///)
!        In = 0
!        RETURN
!      END IF
C
C4------ALLOCATE SPACE FOR UNSATURATED FLOW.
      IUZM = 1
      NWAV = 1
      NUZCL = 1
      NUZRW = 1
      IF ( IUZFOPT.EQ.1 .OR. IUZFOPT.EQ.2 ) THEN
        IUZM = NROW*NCOL
        NWAV = NTRAIL*(NSETS+1)
        NUZCL = NCOL
        NUZRW = NROW
C
C5------SET DIMENSIONS FOR UNSATURATED ZONE ARRAYS TO 1 IF NO
C      UNSATURATED ZONE
      ELSE
        IUZM = 1
        NWAV = 1
        NUZCL = 1
        NUZRW = 1
      END IF
C
C6------CALCULATE SPACE USED FOR LISTING UNSATURATED MOISTURE PROFILES.
      IF ( NUZGAG.GT.0 ) THEN
        NUZGAGAR = NUZGAG
      ELSE
        NUZGAGAR = 1
      END IF
C
C7------ALLOCATE SPACE FOR ARRAYS AND ITIALIZE.
      ALLOCATE (VKS(NCOL,NROW))
      VKS = 0.0 
      ALLOCATE (EPS(NUZCL,NUZRW), THTS(NUZCL,NUZRW), THTI(NUZCL,NUZRW))
      EPS = 0.0
      THTS = 0.0
      THTI = 0.0
      TOTRUNOFF = 0.0
      SURFDEP1 = SURFDEP
      ALLOCATE (THTR(NUZCL,NUZRW))
      THTR = 0.0D0
      ALLOCATE (FINF(NCOL,NROW),PETRATE(NCOL,NROW),UZFETOUT(NCOL,NROW))
      ALLOCATE (GWET(NCOL,NROW))
      FINF = 0.0
      PETRATE = 0.0
      UZFETOUT = 0.0
      GWET = 0.0
      ALLOCATE (FBINS(52))
      FBINS = 0.0
      ALLOCATE (ROOTDPTH(NCOL,NROW))
      ROOTDPTH = 0.0
      ALLOCATE (WCWILT(NUZCL,NUZRW))
      WCWILT = 0.0
      ALLOCATE (IRUNBND(NCOL,NROW), IUZFBND(NCOL,NROW))
      IUZFBND = 0
      IRUNBND = 0
      ALLOCATE (SEEPOUT(NCOL,NROW), EXCESPP(NCOL,NROW))
      ALLOCATE (REJ_INF(NCOL,NROW))
      SEEPOUT = 0.0
      EXCESPP = 0.0
      REJ_INF = 0.0
      ALLOCATE (TO_CFP(NCOL,NROW))
      TO_CFP = 0.0
      ALLOCATE (IUZLIST(4, NUZGAGAR))
      IUZLIST = 0
      ALLOCATE (NWAVST(NUZCL,NUZRW))
      NWAVST = 1
      ALLOCATE (CUMUZVOL(5))
      CUMUZVOL = 0.0D0
      ALLOCATE (UZTSRAT(6))
      UZTSRAT = 0.0D0
      ALLOCATE (UZTOTBAL(NUZCL,NUZRW,7))
      UZTOTBAL = 0.0D0
crgn changed allocation 10/23/06
      ALLOCATE (UZFLWT(NCOL,NROW))
      UZFLWT = 0.0D0
      ALLOCATE (UZSTOR(NUZCL,NUZRW))
      UZSTOR = 0.0D0
      ALLOCATE (DELSTOR(NUZCL,NUZRW))
      DELSTOR = 0.0D0
      ALLOCATE (UZOLSFLX(NUZCL,NUZRW))
      UZOLSFLX = 0.0D0
      ALLOCATE (HLDUZF(NCOL,NROW))
      HLDUZF = 0.0D0
      ALLOCATE (IUZHOLD(2, NCOL*NROW))
      nrnc = 1
      DO irhld = 1, NROW
        DO ichld = 1, NCOL
          IUZHOLD(1, nrnc) = irhld
          IUZHOLD(2, nrnc) = ichld
          nrnc = nrnc + 1
        END DO
      END DO
      ALLOCATE (ITRLSTH(NWAV))
      ITRLSTH = 0
      ALLOCATE (UZDPIT(NWAV,IUZM))
      UZDPIT = 0.0D0
      ALLOCATE (UZDPST(NWAV,IUZM))
      UZDPST = 0.0D0
      ALLOCATE (UZTHIT(NWAV,IUZM))
      UZTHIT = 0.0D0
      ALLOCATE (UZTHST(NWAV,IUZM))
      UZTHST = 0.0D0
      ALLOCATE (UZSPIT(NWAV,IUZM))
      UZSPIT = 0.0D0
      ALLOCATE (UZSPST(NWAV,IUZM))
      UZSPST = 0.0D0
      ALLOCATE (UZFLIT(NWAV,IUZM))
      UZFLIT = 0.0D0
      ALLOCATE (UZFLST(NWAV,IUZM))
      UZFLST = 0.0
      ALLOCATE (LTRLIT(NWAV,IUZM))
      LTRLIT = 0.0
      ALLOCATE (LTRLST(NWAV,IUZM))
      LTRLST = 0.0
      ALLOCATE (ITRLIT(NWAV,IUZM))
      ITRLIT = 0.0
      ALLOCATE (ITRLST(NWAV,IUZM))
      ITRLST = 0.0
C
C8------PRINT OPTION CODE WHEN NUZTOP IS WITHIN SPECIFIED RANGE.
      IF ( IUZFOPT.LT.0 ) THEN
        iuzflg = 0
      ELSE
        iuzflg = 1
      END IF
C
C8b-----Set flag for determining if FINF will be provided by PRMS.
C       A value of zero means that FINF will not be set by PRMS.
      IGSFLOW = 0
      
      IF ( NUZTOP.GE.1 .AND. NUZTOP.LE.3 ) THEN
        IF ( NUZTOP.EQ.1 ) WRITE (IOUT, 9009)
 9009   FORMAT (' OPTION 1 -- RECHARGE IN UZF TO TOP LAYER ONLY ')
        IF ( NUZTOP.EQ.2 ) WRITE (IOUT, 9010)
 9010   FORMAT (' OPTION 2 -- RECHARGE IN UZF TO SPECIFIED NODE ', 
     +          'IN EACH VERTICAL COLUMN')
        IF ( NUZTOP.EQ.3 ) WRITE (IOUT, 9011)
 9011   FORMAT (' OPTION 3 -- RECHARGE IN UZF TO HIGHEST ACTIVE ', 
     +          'NODE IN EACH VERTICAL COLUMN')
C
C9------STOP SIMULATION IF NUZTOP IS NOT WITHIN SPECIFIED RANGE.
      ELSE
        WRITE (IOUT, 9012) NUZTOP
 9012   FORMAT (1X, 'ILLEGAL RECHARGE OPTION CODE IN UZF (NUZTOP = ', 
     +          I5, ') -- SIMULATION ABORTING')
        CALL USTOP(' ')
      END IF
C
C10-----READ IN BOUNDARY ARRAY FOR UNSATURATED FLOW.
      CALL U2DINT(IUZFBND, aname(1), NROW, NCOL, 0, In, IOUT)
C
C11-----READ STREAM AND LAKE ARRAY FOR ROUTING OVERLAND FLOW.
      IF ( IRUNFLG.GT.0 ) CALL U2DINT(IRUNBND, aname(2), NROW, NCOL, 0, 
     +                                In, IOUT)
C
C12-----READ VERTICAL HYDRAULIC CONDUCTIVITY FROM UZF INPUT FILE.
      IF ( ABS(IUZFOPT).EQ.1 ) THEN
        CALL U2DREL(VKS, aname(6), NROW, NCOL, 0, In, IOUT)
C
C13-----CHECK FOR ERRORS IN VERTICAL HYDRAULIC CONDUCTIVITY
        DO nrck = 1, NROW
          DO ncck = 1, NCOL
            iflgbnd = 0
            IF ( IUZFBND(ncck, nrck).GT.0 ) THEN
              iflgbnd = 1
              IF ( VKS(ncck, nrck).LT.CLOSEZERO ) THEN
                WRITE (IOUT, 9013) nrck, ncck
 9013           FORMAT (1X/, 'SATURATED VERTICAL K FOR CELL AT ROW ', 
     +                  I5, ', COL. ', I5, ' IS LESS THAN OR EQUAL TO ',
     +                  'ZERO-- SETTING UNSATURATED FLOW IN CELL TO ', 
     +                  'INACTIVE')
                iflgbnd = 0
              END IF
            END IF
            IF ( iflgbnd.EQ.0 ) IUZFBND(ncck, nrck) = 0
          END DO
        END DO
      END IF
      IF ( iuzflg.GT.0 ) THEN
C
C14-----READ BROOKS-COREY EPSILON ASSUMING IT IS CONSTANT THROUGHOUT
C        VERTICAL COLUMN.
        CALL U2DREL(EPS, aname(5), NUZRW, NUZCL, 0, In, IOUT)
C
C15-----READ SATURATED WATER CONTENT FOR UNSATURATED ZONE ASSUMING IT
C       IS CONSTANT THROUGHOUT VERTICAL COLUMN.
        CALL U2DREL(THTS, aname(3), NUZRW, NUZCL, 0, In, IOUT)
C
C16-----READ INITIAL WATER CONTENT FOR UNSATURATED ZONE ASSUMING IT
C         IS CONSTANT THROUGHOUT VERTICAL COLUMN. DO NOT READ
C         INITIAL WATER CONTENT IF PERIOD IS STEADY STATE.
        IF ( ISSFLG(1).EQ.0 ) CALL U2DREL(THTI, aname(4), NUZRW, NUZCL, 
     +       0, In, IOUT)
C
C17-----CHECK FOR ERROS IN EPS, THTS, AND THTI ARRAYS.
        DO nrck = 1, NUZRW
          DO ncck = 1, NUZCL
            iflgbnd = 0
            IF ( IUZFBND(ncck, nrck).GT.0 ) THEN
              iflgbnd = 1
              IF ( THTS(ncck, nrck).LT.CLOSEZERO ) THEN
                WRITE (IOUT, 9014) nrck, ncck, THTS(ncck, nrck)
 9014           FORMAT (1X/, 'SATURATED WATER CONTENT FOR CELL ', 
     +                  'AT ROW ', I5, ', COL. ', I5, 
     +                  ' IS LESS THAN OR EQUAL', 
     +                  ' TO ZERO-- SETTING UNSATURATED FLOW IN ', 
     +                  'CELL TO INACTIVE', F12.3)
                iflgbnd = 0
              END IF
              IF ( ISSFLG(1).EQ.0 ) THEN
                IF ( THTI(ncck, nrck).LT.CLOSEZERO ) THEN
                  WRITE (IOUT, 9015) nrck, ncck, THTI(ncck, nrck)
 9015             FORMAT (1X/, 'INITIAL WATER CONTENT FOR CELL AT ', 
     +                    'ROW ', I5, ', COL. ', I5, 
     +                    ' IS LESS THAN OR EQUAL', 
     +                    ' TO ZERO-- SETTING UNSATURATED FLOW IN CELL '
     +                    , 'TO INACTIVE', F12.3)
                  iflgbnd = 0
                END IF
              END IF
              IF ( EPS(ncck, nrck).LT.CLOSEZERO ) THEN
                WRITE (IOUT, 9016) nrck, ncck, EPS(ncck, nrck)
 9016           FORMAT (1X/, 'BROOKS-COREY EPSILON FOR CELL AT ROW ', 
     +                  I5, ', COL. ', I5, ' IS LESS THAN OR EQUAL TO ',
     +                  'ZERO-- SETTING UNSATURATED FLOW IN CELL TO ', 
     +                  'INACTIVE', F12.3)
                iflgbnd = 0
              END IF
            END IF
            IF ( iflgbnd.EQ.0 ) IUZFBND(ncck, nrck) = 0
          END DO
        END DO
C
C18-----COMPUTE RESIDUAL WATER CONTENT (THTR) FOR ALL UNSATURATED CELLS
C       AS THE DIFFERENCE BETWEEN SATURATED WATER CONTENT AND SPECIFIC
C       YIELD OF UPPERMOST ACTIVE LAYER.
        IF ( IUZFOPT.GT.0 .AND. ITRSS.NE.0 ) THEN
          DO nrth = 1, NROW
            isyflg = 1
            DO ncth = 1, NCOL
   
              iuzflg = IUZFBND(ncth, nrth)
              IF ( iuzflg.GT.0 ) THEN
                nlth = 1
                ibndflg = 0
                DO WHILE ( ibndflg.EQ.0 )
                  ibndflg = IBOUND(ncth, nrth, nlth)
                  IF ( ibndflg.LT.1 ) nlth = nlth + 1
                  IF ( nlth.GT.NLAY ) ibndflg = -1
                END DO
C
C19-----SPECIFIC YIELD IS STORAGE CAPACITY DIVIDED BY AREA OF MODEL CELL.
                IF ( ibndflg.GT.0 ) THEN                        
                  IF ( Iunitlpf.GT.0 ) THEN
                    IF ( LAYTYP(nlth).GT.0 ) THEN
C use LPF SC2, Iunitlpf>0
                      sy = SCLPF(ncth, nrth, nlth)/
     +                      (DELR(ncth)*DELC(nrth))
                    ELSE
                      WRITE (IOUT, 9017) nlth, ncth, nrth 
9017                  FORMAT(1X,'PROGRAM TERMINATED-LAYTYP IN LPF '
     +                       , 'PACKAGE MUST BE GREATER THAN ZERO FOR '
     +                       , 'UPPERMOST ACTIVE CELL',/1X
     +                       , 'SO SPECIFIC YIELD CAN BE USED FOR '
     +                       , 'COMPUTING RESIDUAL WATER CONTENT-- '
     +                       , 'CELL LAYER,ROW,COLUMN: ',3I5) 
                     CALL USTOP(' ')
                    END IF 
                  ELSE IF ( Iunitbcf.GT.0 ) THEN                
                    IF ( LAYCON(nlth).EQ.1 ) THEN
C use BCF SC1 always
                      sy = SC1(ncth, nrth, nlth)/(DELR(ncth)*DELC(nrth))
                    ELSE
C use BCF SC2, Iunitbcf>0
                      sy = SC2(ncth, nrth, nlth)/(DELR(ncth)*DELC(nrth))
                    END IF
                  ELSE IF ( Iunithuf.GT.0 ) THEN 
                    sy = SC2HUF(ncth, nrth)
                  END IF
                  IF ( sy.GT.0 ) THEN
                    isyflg = 1
                    THTR(ncth, nrth) = THTS(ncth, nrth) - sy
                  ELSE
                    isyflg = 0
                    THTR(ncth, nrth) = 0.0D0
                  END IF
                ELSE IF ( ibndflg.LT.1 ) THEN
                  isyflg = 0
                END IF
              ELSE
                THTR(ncth, nrth) = 0.0D0
              END IF
              test = (THTI(ncth, nrth)-THTR(ncth, nrth))
              IF ( test.LT.CLOSEZERO ) THTI(ncth,nrth) = THTR(ncth,nrth)
C
C20-----IF SPECIFIC YIELD IS 0 FOR UPPERMOST ACTIVE CELL AT COLUMN J
C       AND ROW I OR IF ALL LAYERS AT CELL ARE INACTIVE, SET
C       UNSATURATED FLOW AT CELL INACTIVE.
              IF ( isyflg.EQ.0 ) THEN
                WRITE (IOUT, 9018) nrth, ncth
 9018           FORMAT (1X/, 'SPECIFIC YIELD FOR UPPERMOST ACTIVE ', 
     +                  'CELL AT ROW ', I5, ', COL. ', I5, ' IS LESS ', 
     +                  'THAN OR EQUAL TO ZERO-- SETTING UNSATURATED ',
     +                  'FLOW IN CELL TO INACTIVE')
                IUZFBND(ncth, nrth) = 0
              END IF
            END DO
          END DO
        END IF   
      END IF
C
C21-----READ FILES FOR PRINTING TIME SERIES WATER CONTENT PROFILES.
      IF ( NUZGAG.GT.0 ) THEN
        WRITE (IOUT, 9019)
 9019   FORMAT (1X/, 'WATER CONTENT PROFILES WILL BE PRINTED TO ', 
     +          'SEPARATE FILES FOR SELECTED MODEL CELLS, AND ', 
     +          'TIME STEPS AS DEFINED BY OUTPUT CONTROL PACKAGE ', 
     +          //, 'SELECTED MODEL CELLS ARE: '/' ROW NUMBER ', 
     +          ' COLUMN NUMBER  FORTRAN UNIT NUMBER  ', 
     +          'OUTPUT OPTION'//)
        igage = 1
        DO WHILE ( igage.LE.NUZGAG )
          READ (In, *) IUZLIST(1, igage) 
          IF( IUZLIST(1, igage) .GE. 0 ) THEN 
            BACKSPACE In
            READ (In, *) (IUZLIST(l, igage), l=1, 4)
          ELSE
            IUZLIST(3, igage) = -1*IUZLIST(1, igage)
            IUZLIST(4, igage) = 4  
            IUZLIST(1, igage) = 0
            IUZLIST(2, igage) = 0            
          END IF            
          WRITE (IOUT, 9020) (IUZLIST(l, igage), l=1, 4)
 9020     FORMAT (1X, I7, 7X, I7, 12X, I7, 11X, I7)
C
C22-----DETERMINE IF ROW AND COLUMN NUMBERS ARE IN ACTIVE AREA OF
C        UNSATURATED FLOW.
          IF ( IUZLIST(4, igage) .LT. 4 ) THEN
            iuzrow = IUZLIST(1, igage)
            iuzcol = IUZLIST(2, igage)
            IF ( iuzrow.LT.1 ) THEN
              WRITE (IOUT, 9021) iuzrow, igage
 9021         FORMAT (1X/, 'WARNING--- ROW NUMBER ', I7,  
     +              ' FOR RECORD ',I7, ' IS NOT A VALID NUMBER', 
     +              ' NO OUTPUT WILL BE PRINTED TO THIS FILE'/)
              IUZLIST(3, igage) = 0
            END IF
            IF ( iuzcol.LT.1 ) THEN
              WRITE (IOUT, 9022) iuzcol, igage
 9022         FORMAT (1X/, 'WARNING--- COLUMN NUMBER ', I7, 
     +              ' FOR RECORD ', I7, ' IS NOT A VALID NUMBER', 
     +              ' NO OUTPUT WILL BE PRINTED TO THIS FILE'/)
              IUZLIST(3, igage) = 0
            END IF
            IF ( IUZFBND(iuzcol, iuzrow).LT.1 ) THEN
              WRITE (IOUT, 9023) igage
 9023         FORMAT (1X/, 'WARNING--- RECORD ', I7, ' IS NOT IN ', 
     +              'ACTIVE AREA OF UNSATURATED FLOW; ', 
     +              ' NO OUTPUT WILL BE PRINTED TO THIS FILE'/)
              IUZLIST(3, igage) = 0
            END IF
          END IF
          igage = igage + 1
        END DO
C
C23-----WRITE HEADER FILES FOR CELLS WITH SELECTED OUTPUT.
        DO igage = 1, NUZGAG
          iuzrow = IUZLIST(1, igage)
          iuzcol = IUZLIST(2, igage)
          igunit = IUZLIST(3, igage)
          iuzopt = IUZLIST(4, igage)
          IF (iuzopt .LT. 4 ) iuzlay = IUZFBND(iuzcol, iuzrow)
          IF ( igunit.GT.0 ) THEN
C
C24----GET VARIABLE OUTTYPE.
            SELECT CASE (iuzopt)
C
C25-----PRINT HEADER WHEN WRITING ONLY VOLUMES.
            CASE (1)
              WRITE (igunit, 9024) igage, iuzrow, iuzcol, iuzlay
C
C26-----PRINT HEADER WHEN WRITING VOLUMES AND RATES.
            CASE (2)
              WRITE (igunit, 9025) igage, iuzrow, iuzcol, iuzlay
C
C27-----PRINT HEADER FOR UNSATURATED-ZONE MOISTURE PROFILES
            CASE (3)
              WRITE (igunit, 9026) igage, iuzrow, iuzcol, iuzlay
            CASE (4)
              WRITE (igunit, 9027)
            END SELECT
          END IF
        END DO
      END IF
C
C28-----FORMATS
 9024 FORMAT (1X,'"LOCATION OF SPECIFIED CELL FOR PRINTING VOLUMES ', 
     +        'IN UNSATURATED ZONE: GAGE ', I4, ' ROW, COLUMN ', I4, 
     +        ',', I4, ' INITIAL LAYER ASSIGMENT ', I4,'"', /1X,
     +        '"DATA:  LAYER             TIME        GW-HEAD   ', 
     +        'UZ-THICKNESS CUM.-APL.-INF.   CUM.-INFILT.     CUM.-', 
     +        'RECH.    TOTAL-STOR.   STOR.-CHANGE    SURF.-LEAK. "')
 9025 FORMAT (1X,'"LOCATION OF SPECIFIED CELL FOR PRINTING VOLUMES ', 
     +      'AND RATES IN UNSATURATED ZONE: GAGE ', I4, 
     +      ' ROW, COLUMN ', I4, ',', I4, ' INITIAL LAYER ASSIGMENT ',
     +      I4,'"', /1X, '"DATA:  LAYER             TIME        ',
     +      'GW-HEAD   UZ-THICKNESS CUM.-APL.-INF.   CUM.-INFILT.   ', 
     +      '  CUM.-RECH.    TOTAL-STOR.   STOR.-CHANGE    SURF.-LEAK.',
     +      ' APL.-INF.-RATE    INFIL.-RATE     RECH.-RATE     ',
     +      'STOR.-RATE     SEEP.-RATE "', /)
 9026 FORMAT (1X,'"LOCATION OF SPECIFIED CELL FOR PRINTING VOLUMES ', 
     +        'IN UNSATURATED ZONE: GAGE ', I4, ' ROW, COLUMN ', I4, 
     +        ',', I4, ' INITIAL LAYER ASSIGMENT ', I4,'"', /1X, 
     +        '"DATA:  LAYER             TIME        GW-HEAD   ',
     +        'UZ-THICKNESS          DEPTH    WATER-CONT. "', /)
 9027 FORMAT (1X,'"UNSATURATED MASS BALANCE COMPONENTS FOR ENTIRE ',  
     +        'MODEL "',/1X,'"DATA:            TIME   APPLIED-INFIL.',
     +        '          RUNOFF   ACTUAL-INFIL.   SURFACE-LEAK.',
     +        '           UZ-ET           GW-ET',
     +        '   UZSTOR-CHANGE        RECHARGE "',/)
C
C29-----PRINT WARINING WHEN UNITS ARE UNDEFINED IN MODFLOW.
      IF ( ITMUNI.EQ.0 .OR. LENUNI .EQ. 0 ) THEN
        WRITE(IOUT,*)'****Units are undefined. This may cause ',
     +  'unfortunate results when using GSFLOW****'
      END IF
C
      IF ( Iunitlpf.GT.0 .OR. Iunithuf.GT.0 ) THEN 
        IF ( ABS(IUZFOPT).EQ.2 ) CALL SGWF2UZF1VKS(Iunithuf, Iunitlpf)
      END IF
      fkmax = 86400.0
      fkmin = 1.0E-4
      IF ( ITMUNI.EQ.1 ) THEN
        fkmax = fkmax/86400.0
        fkmin = fkmin/86400.0
      ELSE IF ( ITMUNI.EQ.2 ) THEN
        fkmax = fkmax/1440.0
        fkmin = fkmin/1440.0
      ELSE IF ( ITMUNI.EQ.3 ) THEN
        fkmax = fkmax/24.0  
        fkmin = fkmin/24.0
      ELSE IF ( ITMUNI.EQ.5 ) THEN
        fkmax = fkmax*365.0
        fkmin = fkmin*365.0
      END IF    
      IF ( LENUNI.EQ.1 ) THEN
        fkmax = fkmax/0.3048
        fkmin = fkmin/0.3048
      ELSE IF ( LENUNI.EQ.3 ) THEN
        fkmax = fkmax*100.0
        fkmin = fkmin*100.0
      END IF 
      range=LOG(fkmax)-LOG(fkmin)
      finc=range/50.0
      FBINS(1) = LOG(fkmin)
C
C30-----BIN INFILTRATION RATES.
      DO ivol = 2, 51
        FBINS(ivol) = FBINS(ivol-1)+ finc
        FBINS(ivol-1) = EXP(FBINS(ivol-1))
      END DO
      FBINS(51) = EXP(FBINS(51))
C
C31-----SAVE POINTERS FOR GRID AND RETURN.
      CALL SGWF2UZF1PSV(Igrid)
      RETURN
      END SUBROUTINE GWF2UZF1AR
C
C------SUBROUTINE SGWF2UZF1VKS
      SUBROUTINE SGWF2UZF1VKS(Iunithuf, Iunitlpf) 
C     ******************************************************************
C     ASSIGN SATURATED VERTICAL HYDRAULIC CONDUCTIVITY ARRAY 
C     (VKS) IN UZF TO EQUAL VERTICAL HYDRAULIC CONDUCTIVITY IN LAYER-
C     PROPERTY FLOW PACKAGE
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE, ONLY: VKS, IUZFBND
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IOUT, IBOUND, BOTM
      USE GWFLPFMODULE, ONLY: LAYTYP, LAYVKA, VKA, HK
      USE GWFHUFMODULE, ONLY: HGUVANI, NHUF, HKHUF=>HK, VKAH
      USE GWFLPFMODULE, ONLY: SCLPF=>SC2, LAYTYP
      IMPLICIT NONE
C    ------------------------------------------------------------------
C    SPECIFICATIONS:
C    ------------------------------------------------------------------
C    LOCAL VARIABLES
C    ------------------------------------------------------------------
      INTEGER krck, ncck, nrck, Iunithuf, Iunitlpf
      REAL Celthick
C    ******************************************************************
C
C1------CHECK TO SEE IF UPPERMOST ACTIVE CELL IS CONVERTABLE AND
C       SET VKS EQUAL TO VKALPF FOR CORRESPONDING MODEL CELL.
      krck = 1
      DO nrck = 1, NROW
        DO ncck = 1, NCOL
          krck = IUZFBND(ncck, nrck)
          IF ( krck.GT.0 ) THEN
            IF ( IBOUND(ncck, nrck, krck).GT.0 ) THEN
              IF ( Iunitlpf.GT.0 ) THEN
                IF ( LAYTYP(krck).LT.1 ) THEN
                  WRITE (IOUT, *) 
     +                       'PROGRAM TERMINATED-LAYTYP MUST BE GREATER'
     +                       , ' THAN ZERO WHEN IUZFOPT IS 2'
                  CALL USTOP(' ')
                END IF
                IF ( LAYVKA(krck).EQ.0 ) THEN
                  VKS(ncck, nrck) = VKA(ncck, nrck, krck)
                ELSE
                  VKS(ncck, nrck) = VKA(ncck, nrck, krck)
     +                              *HK(ncck, nrck, krck)
                END IF
              ELSE
                IF ( krck.GT.0 ) THEN
                  Celthick = BOTM(ncck, nrck, krck-1)-
     +                     BOTM(ncck, nrck, krck)
                END IF
                IF ( HGUVANI(NHUF).LE.0.0 ) THEN
                  VKS(ncck, nrck) = VKAH(ncck, nrck, krck)/(Celthick)
                ELSE
                  VKS(ncck, nrck) = HGUVANI(NHUF)*
     +                              HKHUF(ncck, nrck, krck)
                END IF
              END IF
            END IF
          END IF
        END DO
      END DO
C
C2------RETURN.
      RETURN
      END SUBROUTINE SGWF2UZF1VKS
C
C-------SUBROUTINE GWF2UZF1RP
      SUBROUTINE GWF2UZF1RP(In, Kkper, Igrid)
C     ******************************************************************
C     READ AND CHECK VARIABLES EACH STRESS PERIOD 
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IOUT, ISSFLG, IBOUND, 
     +                        HNEW, DELR, DELC, BOTM
      IMPLICIT NONE
C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER In, Kkper, Igrid
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
      DOUBLE PRECISION h
      REAL bottom, celtop, slen, width, etdpth, surfinf, surfpotet, top
      INTEGER ibdflg, ic, iflginit, il, ilay, ill, ir, iss, jk, l, ncck,
     +        nrck, nuzf
      CHARACTER(LEN=24) aname(4)
      DATA aname(1)/' AREAL INFILTRATION RATE'/
      DATA aname(2)/'                 ET RATE'/
      DATA aname(3)/'     ET EXTINCTION DEPTH'/
      DATA aname(4)/'EXTINCTION WATER CONTENT'/
C     -----------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2UZF1PNT(Igrid)
C      
C2------READ INFILTRATION RATES FOR UZF CELLS AT THE BEGINNING OF EACH
C       STRESS PERIOD.
      iss = ISSFLG(Kkper)
      iflginit = 0
      READ (In, *) nuzf
      IF ( nuzf.LT.0 ) THEN
        WRITE (IOUT, *) 'USING INFILTRATION RATE FROM PREVIOUS STRESS '
        WRITE (IOUT, *) 'PERIOD.', 'CURRENT PERIOD IS: ', Kkper
      ELSE
C
C3------READ IN ARRAY FOR INFILTRATION RATE.
        CALL U2DREL(FINF, aname(1), NROW, NCOL, 0, In, IOUT)
C
C4------CHECK FOR NEGATIVE INFILTRATION RATES.
        DO nrck = 1, NROW
          DO ncck = 1, NCOL
            IF ( IUZFBND(ncck, nrck).LT.1 ) THEN
              surfinf = FINF(ncck, nrck)
              IF ( surfinf.GT.0.0 ) FINF(ncck, nrck) = 0.0
            ELSE IF ( FINF(ncck, nrck).LT.0.0 ) THEN
              WRITE (IOUT, 9002) nrck, ncck
 9002         FORMAT (1X/, 'INFILTRATION RATE FOR CELL AT ROW ', I5, 
     +                ', COLUMN ', I5, ' IS LESS THAN ZERO ', 
     +                'ZERO-- SETTING RATE TO ZERO ')
              FINF(ncck, nrck) = 0.0
C
C5------SET INFILTRATION RATE TO SATURATED VERTICAL K WHEN RATE IS
C        GREATER THAN K AND ROUTE EXCESS WATER TO STREAM IF 
C        IRUNFLG IS NOT EQUAL TO ZERO.
            ELSE IF ( FINF(ncck, nrck).GT.VKS(ncck, nrck) ) THEN
               EXCESPP(ncck, nrck) =  (FINF(ncck, nrck) - 
     +                      VKS(ncck, nrck))*DELC(nrck)*DELR(ncck)
              FINF(ncck, nrck) = VKS(ncck, nrck)
            END IF
          END DO
        END DO
      END IF
      IF ( IETFLG.NE.0 ) THEN
        READ (In, *) nuzf
        IF ( nuzf.LT.0 ) THEN
          WRITE (IOUT, 9004) Kkper
 9004     FORMAT (1X/, 'USING ET RATE FROM PREVIOUS STRESS ', 
     +            'PERIOD. CURRENT PERIOD IS: ', I7)
        ELSE
C
C6------READ IN ARRAY FOR ET RATE.
          CALL U2DREL(PETRATE, aname(2), NROW, NCOL, 0, In, IOUT)
C
C7-----CHECK FOR NEGATIVE ET RATES.
          DO nrck = 1, NROW
            DO ncck = 1, NCOL
C8----ZERO ET ARRAY THAT IS PASSED TO PRMS.
              UZFETOUT(ncck, nrck) = 0.0
              IF ( IUZFBND(ncck, nrck).LT.1 ) THEN
                surfpotet = PETRATE(ncck, nrck)
                IF ( surfpotet.GT.0.0 ) PETRATE(ncck, nrck) = 0.0
              ELSE IF ( PETRATE(ncck, nrck).LT.0.0 ) THEN
                WRITE (IOUT, 9005) nrck, ncck
 9005           FORMAT (1X/, 'POTENTIAL ET RATE FOR CELL AT ROW ', I5, 
     +                  ', COLUMN ', I5, ' IS LESS THAN ZERO ', 
     +                  'ZERO-- SETTING RATE TO ZERO ')
                PETRATE(ncck, nrck) = 0.0
              END IF
            END DO
          END DO
        END IF
        READ (In, *) nuzf
        IF ( nuzf.LT.0 ) THEN
          WRITE (IOUT, 9006) Kkper
 9006     FORMAT (/1x, 'USING ET EXTINCTION DEPTH FROM PREVIOUS ',
     +            'STRESS PERIOD. CURRENT PERIOD IS: ', I7)
        ELSE
C
C9------READ IN ARRAY FOR ET EXTINCTION DEPTH.
          CALL U2DREL(ROOTDPTH, aname(3), NROW, NCOL, 0, In, IOUT)
C
C10-----CHECK FOR NEGATIVE ET EXTINCTION DEPTH.
          DO nrck = 1, NROW
            DO ncck = 1, NCOL
              IF ( IUZFBND(ncck, nrck).LT.1 ) THEN
                etdpth = ROOTDPTH(ncck, nrck)
                IF ( etdpth.GT.0.0 ) ROOTDPTH(ncck, nrck) = 0.0
              ELSE IF ( ROOTDPTH(ncck, nrck).LE.0.0 ) THEN
                WRITE (IOUT, 9007) nrck, ncck
 9007           FORMAT (1X/, 'ROOTING DEPTH FOR CELL AT ROW ', I5, 
     +                  ', COLUMN ', I5, ' IS LESS THAN OR EQUAL TO ', 
     +                  'ZERO-- SETTING DEPTH TO ONE ')
                ROOTDPTH(ncck, nrck) = 1.0
              END IF
            END DO
          END DO
        END IF
C
C11-----SKIP READING OF EXTINCTION WATER CONTENT ARRAY WHEN 
C         IUZFOPT IS ZERO.
        IF ( IUZFOPT.LE.0 ) THEN
          READ (In, *) nuzf
          IF ( nuzf.LT.0 ) THEN
            WRITE (IOUT, 9012) Kkper
 9012       FORMAT (/1X, 'USING EXTINCTION WATER CONTENT FROM PREVIOUS',
     +              ' STRESS PERIOD. CURRENT PERIOD IS: ', I7)
          ELSE
C
C12-----READ IN ARRAY FOR ET EXTINCTION DEPTH.
            CALL U2DREL(WCWILT, aname(4), NROW, NCOL, 0, In, IOUT)
C
C13-----CHECK FOR EXTINCTION WATER CONTENT LESS THAN RESIDUAL WATER
C         CONTENT.
            DO nrck = 1, NUZRW
              DO ncck = 1, NUZCL
                IF ( IUZFBND(ncck, nrck).LT.1 .AND. WCWILT(ncck, nrck)
     +               .GT.0 ) WCWILT(ncck, nrck) = 0.0
                IF ( iss.NE.0 .AND. IUZFBND(ncck, nrck).GT.0 ) THEN
                  IF ( WCWILT(ncck, nrck).LT.THTR(ncck, nrck)+ZEROD9 )
     +                 THEN
                    WRITE (IOUT, 9014) nrck, ncck
 9014               FORMAT (1X/, 'EXTINCTION WATER CONTENT FOR ', 
     +                      'CELL AT ROW ', I5, ', COLUMN ', I5, 
     +                      ' IS LESS THAN RESIDUAL WATER CONTENT', 
     +                      '-- SETTING EXTINCTION WATER ', 
     +                      'CONTENT EQUAL TO RESIDUAL WATER ', 
     +                      'CONTENT')
                    WCWILT(ncck, nrck) = THTR(ncck, nrck) + ZEROD9
                  END IF
                END IF
              END DO
            END DO
          END IF
        END IF
      END IF
C
C14------INITIALIZE UNSATURATED ZONE IF ACTIVE.
C
C15------SET FLAGS FOR STEADY STATE OR TRANSIENT SIMULATIONS.
      IF ( Kkper.GT.2 ) THEN
        iflginit = 0
      ELSE IF ( Kkper.EQ.1 ) THEN
        iflginit = 1
      ELSE
        IF ( iss.EQ.0 .AND. ISSFLG(Kkper-1).NE.0 )
     +       iflginit = 2
      END IF
      IF ( iflginit.GE.1 ) THEN
        DO l = 1, IUZM
          ir = IUZHOLD(1, l)
          ic = IUZHOLD(2, l)
C
C16-----SEARCH FOR UPPERMOST ACTIVE CELL.
          IF ( IUZFBND(ic, ir).GT.0 ) THEN
            il = 0 !rsr, added to be sure il is set
            IF ( NUZTOP.EQ.1 .OR. NUZTOP.EQ.2 ) THEN
              il = IUZFBND(ic, ir)
              IF ( il.GT.0 ) THEN
                IF ( IBOUND(ic, ir, il).LE.0 ) il = 0
              ELSE
                il = 0
              END IF
            ELSE IF ( NUZTOP.EQ.3 ) THEN
              ill = 1
              ibdflg = 1
              DO WHILE ( ibdflg.EQ.1 )
                IF ( IBOUND(ic, ir, ill).LT.0 ) THEN
                  ibdflg = 0
                  il = 0
                ELSE IF ( IBOUND(ic, ir, ill).EQ.0 ) THEN
                  ibdflg = 1
                ELSE IF ( IBOUND(ic, ir, ill).GT.0 ) THEN
                  ibdflg = 0
                  il = ill
                END IF
                IF ( ill.EQ.NLAY .AND. ibdflg.EQ.1 ) THEN
                  ibdflg = 0
                  il = NLAY
                END IF
                ill = ill + 1
              END DO
            END IF
C
C16B-----SEARCH FOR UPPER MOST ACTIVE CELL WITH A WATER LEVEL.
            ilay = il  !rsr, set here to be sure it has a value below
            IF ( il.GT.0 ) THEN
              IF ( IBOUND(ic, ir, il).GT.0 ) THEN
!rsr            ilay = il
                TOPCELL: DO WHILE ( ilay.LE.NLAY )
                  IF ( HNEW(ic, ir, ilay).LE.BOTM(ic,ir,ilay) ) THEN
                    ilay = ilay + 1
                  ELSE
                    EXIT TOPCELL
                  END IF
                END DO TOPCELL
              END IF
              IF ( ilay.LE.NLAY ) THEN
                il = ilay
                h = HNEW(ic, ir, il)
              ELSE
                h = DBLE(BOTM(ic,ir,NLAY))
              END IF
crgn changed HNEW(ic, ir, il) to h in next line.
              HLDUZF(ic,ir) = h
              IF ( IUZFOPT.GT.0 ) THEN
C
C17-----SET CELL TOP, LENGTH, WIDTH AND WATER TABLE ELEVATION.
                slen = DELC(ir) 
                width = DELR(ic) 
                celtop = BOTM(ic, ir, 0) - 0.5 * SURFDEP
C
C18-----SKIP IF CELL IS OUTSIDE ACTIVE BOUNDARY OR IS NOT WATER TABLE.
                IF ( il.LT.1 ) IUZFBND(ic, ir) = 0
C
C19-----INITIALIZE UZTHST ARRAY TO RESIDUAL WATER CONTENT.

                DO jk = 1, NWAV
                  UZTHST(jk, l) = THTR(ic, ir)
                END DO
C
C20-----INITIALIZE UNSATURATED ZONE ARRAYS FOR FIRST STRESS PERIOD.
                IF ( iflginit.EQ.1 ) THEN
                  IF ( celtop.GT.h ) THEN
                    UZDPST(1, l) = (celtop-h)
C
C21-----CALCULATE INITIAL WATER CONTENT AND FLUX IF STEADY STATE.
                    IF ( iss.NE.0 ) THEN
                      UZFLST(1, l) = FINF(ic, ir)
                      UZTHST(1, l) = (((UZFLST(1, l)/VKS(ic,ir))**
     +                  (1.0/EPS(ic,ir)))*(THTS(ic,ir)-THTR(ic,ir)))
     +                               + THTR(ic, ir)
                      top = UZTHST(1, l) - THTR(ic, ir)
                      IF ( UZTHST(1, l)-THTR(ic, ir).LT.0.0D0 )
     +                     UZTHST(1, l)=THTR(ic, ir)
C
C22-----SET INITIAL WATER CONTENT TO THTI AND CALCULATE FLUX IF 
C         TRANSIENT.
                    ELSE
                      UZTHST(1, l) = THTI(ic, ir)
                      top = UZTHST(1, l) - THTR(ic, ir)
                      IF ( top.LE.0.0 ) top = 0.0
                      IF ( top.GT.0.0 ) THEN
                        bottom = (THTS(ic, ir)-THTR(ic, ir))
                        UZFLST(1, l) = VKS(ic, ir)*(top/bottom)
     +                                 **EPS(ic, ir)
                      END IF
                    END IF
                    IF ( UZTHST(1, l).LT.THTR(ic, ir) ) UZTHST(1, l)
     +                   = THTR(ic, ir)
C
C23-----CALCULATE VOLUME OF WATER STORED IN UNSATURATED ZONE.
                    IF ( top.GT.0.0 ) THEN
                      IF ( iss.EQ.0 ) UZSTOR(ic, ir) = UZDPST(1, l)
     +                     *top*width*slen
                      UZSPST(1, l) = 0.0D0
                      UZOLSFLX(ic, ir) = UZFLST(1, l)
                    ELSE
                      UZSTOR(ic, ir) = 0.0D0
                      UZFLST(1, l) = 0.0D0
                      UZSPST(1, l) = 0.0D0
                      UZOLSFLX(ic, ir) = 0.0D0
                    END IF
C
C24-----IF NO UNSATURATED ZONE, SET ARRAY VALUES TO ZERO EXEPT WHEN
C         STEADY STATE, THEN SET UZFLST ARRAY TO INFILRATION RATE.
                  ELSE
                    IF ( iss.NE.0 ) THEN
                      UZFLST(1, l) = FINF(ic, ir)
                    ELSE
                      UZFLST(1, l) = 0.0D0
                    END IF
                    UZDPST(1, l) = 0.0D0
                    UZSPST(1, l) = 0.0D0
                    UZTHST(1, l) = THTR(ic, ir)
                    UZSTOR(ic, ir) = 0.0D0
cupdate        
                    UZOLSFLX(ic, ir) = FINF(ic, ir)
                  END IF
C
C25-----INITIALIZE ARRAYS FOR A TRANSIENT PERIOD THAT FOLLOWS A
C         STEADY STATE PERIOD IN STRESS PERIOD 1.
                ELSE IF ( iflginit.EQ.2 ) THEN
                  IF ( celtop.GT.h ) THEN
                    UZDPST(1, l) = celtop - h
C
C26-----CALCULATE INITIAL WATER CONTENT AND FLUX FROM STEADY STATE
C         SIMULATION.
                    IF ( UZFLST(1, l).LT.0.0D0 ) UZFLST(1, l) = 0.0D0
                    UZTHST(1, l) = (((UZFLST(1, l)/VKS(ic,ir))**
     +                  (1.0/EPS(ic,ir)))*(THTS(ic,ir)-THTR(ic,ir)))
     +                           + THTR(ic, ir)
                    IF ( UZTHST(1, l).LT.THTR(ic, ir) ) UZTHST(1, l)
     +                   = THTR(ic, ir)
                    top = UZTHST(1, l) - THTR(ic, ir)
                    IF ( top.LE.0.0 ) top = 0.0
                    IF ( top.LT.1.0E-5 ) UZFLST(1, l) = 0.0D0
                    IF ( top.GT.1.0E-5 ) THEN
                      UZSTOR(ic, ir) = UZDPST(1, l)*top*width*slen
                      UZSPST(1, l) = 0.0D0
                      UZOLSFLX(ic, ir) = UZFLST(1, l)
C
C27-----IF NO UNSATURATED ZONE, SET ARRAYS VALUES TO ZERO.
                    ELSE
                      UZSTOR(ic, ir) = 0.0D0
                      UZFLST(1, l) = 0.0D0
                      UZSPST(1, l) = 0.0D0
                      UZOLSFLX(ic, ir) = 0.0D0
                    END IF
                  ELSE
                    UZDPST(1, l) = 0.0D0
                    UZFLST(1, l) = 0.0D0
                    UZSPST(1, l) = 0.0D0
                    UZTHST(1, l) = THTR(ic, ir)
                    UZSTOR(ic, ir) = 0.0D0
                    UZOLSFLX(ic, ir) = 0.0D0
                  END IF
                END IF
              END IF
            END IF
          END IF
        END DO
      END IF
C
C28-----RETURN.
      RETURN
      END SUBROUTINE GWF2UZF1RP
C
C--------SUBROUTINE GWF2UZF1FM
! RGN added Iunitbcf and Iunitlpf 1/24/08
      SUBROUTINE GWF2UZF1FM(Kkper, Kkstp, Kkiter, Iunitsfr, Iunitlak, 
     +                      Iunitbcf, Iunitlpf, Iunithuf, Hdrybcf, 
     +                      Hdryhuf, Igrid)
C      SUBROUTINE GWF2UZF1FM(Kkper, Kkiter, Iunitsfr, Iunitlak, 
C     +                      Iunitcfp, Igrid)
C     SUBROUTINE GWF2UZF1FM(Kkper, Kkiter, Iunitsfr, Iunitlak, 
C    +                      Igrid)
C     ******************************************************************
C     COMPUTE UNSATURATED ZONE FLOW AND STORAGE, RECHARGE, ET, AND
C     SURFACE LEAKAGE AND ADD OR SUBTRACT TERMS RHS AND HCOF
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, HNEW, ISSFLG, DELR,
     +                        DELC, BOTM, IBOUND, HCOF, RHS,
     +                        ITMUNI
      USE GWFBASMODULE, ONLY: DELT
Cdep  added lake flags to suppress seepout and ET beneath a lake
      USE GWFLAKMODULE, ONLY: LKARR1, STGNEW
! RGN Added Hdry 1/24/08
      USE GWFLPFMODULE, ONLY: HDRY
Crgn  added conduit recharge factor to route water to conduits.
!       USE CFPMODULE,    ONLY: Mxnode, QCONDIR

      IMPLICIT NONE
C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER Kkper, Iunitsfr, Iunitlak, Igrid, Kkstp, Iunitcfp, Kkiter
      INTEGER Iunitbcf, Iunitlpf, Iunithuf
      REAL Hdrybcf, Hdryhuf
C     INTEGER Kkper, Iunitsfr, Iunitlak, Igrid, Kkstp, Kkiter
      !rsr KKITER and KKSTP not used
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
      REAL csep, epsilon, fks, rootdp, ths, wiltwc, s, x, c, etdp, etgw,
     +     trhs, thcof, celthick, csepmx, finfact, finfhold
!rsr  REAL finfact2
Cdep  REAL fin, fout, foutet
      INTEGER ibdflg, ic, il, ill, ir, iset, iss, iwav, l, numwaves,
     +        numcells, land, idelt, ik
!rsr  INTEGER irun, i
Cdep  added lake flags to suppress seepout and ET beneath a lake
      INTEGER lakflg, lakid
      DOUBLE PRECISION oldsflx, surflux, dlength, h, celtop, deltinc,
     +                 zoldist, totflux, etact, rateud, hld, htest1,
     +                 htest2, flength, width, thr, cellarea, fact,
     +                 totfluxtot, totetact
C     -----------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2UZF1PNT(Igrid)      
C
C2------LOOP THROUGH UNSATURATED ZONE FLOW CELLS.
      NSETS = 1
      numcells = NROW*NCOL
      iss = ISSFLG(Kkper)
      fact = 1.0D0
cdep  fin = 0.0
cdep  fout = 0.0
cdep  foutet = 0.0
      deltinc = DELT
      idelt = 1
      IF ( IETFLG.GT.0 ) THEN                      
        IF ( ITMUNI.EQ.1 ) THEN
          fact = 86400.0D0          
        ELSE IF ( ITMUNI.EQ.2 ) THEN
          fact = 1440.0D0           
        ELSE IF ( ITMUNI.EQ.3 ) THEN
          fact = 24.0D0             
        ELSE IF ( ITMUNI.EQ.4 ) THEN
          fact = 1.0D0              
        ELSE IF ( ITMUNI.EQ.5 ) THEN
          fact = 1.0D0/(365.0D0)    
        END IF                      
        idelt = INT(DELT/fact)   
        IF ( idelt.LT.1 )THEN
          idelt=1
          deltinc = DELT
        ELSE
          deltinc = DELT/idelt
        END IF    
      ELSE                          
        deltinc = DELT              
        idelt = 1                   
      END IF
      IF ( deltinc.GT.DELT ) THEN
        deltinc = DELT
        idelt = 1
      END IF
      DO l = 1, numcells
        etgw = 0.0
        c = 0.0
        etact = 0.0D0
        ir = IUZHOLD(1, l)
        ic = IUZHOLD(2, l)
        finfhold = FINF(ic,ir)
C set excess precipitation to zero for integrated (GSFLOW) simulation
        IF ( IGSFLOW.GT.0 ) Excespp(ic,ir) = 0.0
        land = IUZFBND(ic, ir)
        UZFETOUT(ic, ir) = 0.0
        totflux = 0.0D0
        totfluxtot = 0.0D0
        totetact = 0.0D0
!rsr    finfact = finfhold
        finfact = 0.0       !rsr, modification
C
C3------SEARCH FOR UPPERMOST ACTIVE CELL.
        IF ( IUZFBND(ic, ir).GT.0 ) THEN
          il = 0 !rsr, added to be sure il is set
          IF ( NUZTOP.EQ.1 .OR. NUZTOP.EQ.2 ) THEN
            il = IUZFBND(ic, ir)
            IF ( il.GT.0 ) THEN
              IF ( IBOUND(ic, ir, il).LT.1 ) il = 0
            ELSE
              il = 0
            END IF
          ELSE IF ( NUZTOP.EQ.3 ) THEN
            ill = 1
            ibdflg = 1
            DO WHILE ( ibdflg.EQ.1 )
              IF ( IBOUND(ic, ir, ill).LT.0 ) THEN
                ibdflg = 0
                il = 0
              ELSE IF ( IBOUND(ic, ir, ill).EQ.0 ) THEN
                ibdflg = 1
              ELSE IF ( IBOUND(ic, ir, ill).GT.0 ) THEN
                ibdflg = 0
                il = ill
              END IF
CRGN made il = 0 when all layers for column are inactive 2/21/08
              IF ( ill.EQ.NLAY .AND. ibdflg.EQ.1 ) THEN
                ibdflg = 0
                il = 0
              END IF
              ill = ill + 1
            END DO
          END IF
Cdep  added lake flags to suppress seepout and ET beneath a lake
          lakflg = 0
          lakid = 0
          IF ( Iunitlak.GT.0 .AND. il.GT.1 ) THEN
            lakid = LKARR1(ic,ir,il-1)
            IF ( lakid.GT.0 ) THEN
              IF( STGNEW(lakid).GT.BOTM(ic, ir, il-1) )
     +            lakflg = 1
            END IF
          END IF
Cdep  end change         
          IF ( il.GT.0 .AND. VKS(ic, ir).GT.NEARZERO ) THEN
            IF ( IBOUND(ic, ir, il).GT.0 ) THEN
              h = HNEW(ic, ir, il)
            ELSE IF ( IBOUND(ic, ir, il).LT.1 .AND. il.LT.NLAY ) THEN
              h = HNEW(ic, ir, il+1)
            ELSE
              h = BOTM(ic, ir, NLAY)
            END IF
            hld = HLDUZF(ic,ir)
! Added code to test for BCF or LPF 1/24/08
          IF ( Iunitbcf.NE.0 ) THEN
            IF ( ABS(SNGL(hld)-Hdrybcf).LT.CLOSEZERO ) hld = h
          ELSE IF ( Iunitlpf.NE.0 ) THEN
            IF ( ABS(SNGL(hld)-HDRY).LT.CLOSEZERO ) hld = h
          ELSE IF ( Iunithuf.NE.0 ) THEN
            IF ( ABS(SNGL(hld)-Hdryhuf).LT.CLOSEZERO ) hld = h
          END IF
            IF ( NUZTOP.EQ.1 ) THEN
              celtop = BOTM(ic, ir, 0) - 0.5 * SURFDEP
              celthick = BOTM(ic, ir, 0) - BOTM(ic, ir, 1)
            ELSE
              celtop = BOTM(ic, ir, land-1) - 0.5 * SURFDEP
              celthick = BOTM(ic, ir, land-1) - BOTM(ic, ir, land)
            END IF
            SEEPOUT(ic, ir) = 0.0
            flength = DELC(ir)
            width = DELR(ic)
            cellarea = flength*width
            fks = VKS(ic, ir)
Crgn added code to route water to conduits.
!rsr        finfact2 = 0.0
!            IF ( Iunitcfp.GT.0 ) THEN
!              irun = abs(IRUNBND(ic, ir))
!              i = irun - IRUNBIG
!              IF ( i.GT.0 .AND. i.LT.Mxnode ) THEN
!                finfact2 = QCONDIR(i)
!                IF ( finfact2.GT.1.0 ) finfact2 = 1.0
!                IF ( finfact2.LT.0.0 ) finfact2 = 0.0
!                TO_CFP(ic,ir) = cellarea*finfact2*FINF(ic,ir)
!                finfhold = FINF(ic,ir) - finfact2*FINF(ic,ir)
!                IF ( finfhold.LT.0.0 ) finfhold = 0.0
!              END IF 
!            END IF
            IF ( IUZFOPT.GT.0 ) THEN
              ths = THTS(ic, ir)
              thr = THTR(ic, ir)
              epsilon = EPS(ic, ir)
            END IF
            htest1 = h - celtop
            htest2 = hld - celtop
            IF ( htest1.LT.-CLOSEZERO .OR. htest2.LT.-CLOSEZERO ) THEN
              IF ( htest1.GE.-CLOSEZERO .AND. finfhold.GT.
     +             CLOSEZERO  ) THEN
                IF ( SURFDEP1.GT.CLOSEZERO ) THEN
                  finfact = finfhold - (finfhold/SURFDEP1)*
     +                    (h - celtop)
                END IF
                IF ( finfact.LT.0.0 ) finfact = 0.0
              ELSE
                finfact = finfhold
              END IF
              IF ( IUZFOPT.GT.0 .AND. iss.EQ.0 ) THEN
C
C4------RESET ALL UNSATRATED ZONE CELLS TO PREVIOUS CONDITIONS.
                iset = 1
                numwaves = NWAVST(ic, ir)
                IF ( htest2.GE.0.0D0 ) THEN
                  DO iwav = iset, iset + 5
                    UZTHST(iwav, l) = thr
                    UZDPST(iwav, l) = 0.0D0
                    UZSPST(iwav, l) = 0.0D0
                    UZFLST(iwav, l) = 0.0D0
                    ITRLST(iwav, l) = 0
                    LTRLST(iwav, l) = 0
                  END DO
                END IF
                DO iwav = iset, iset+numwaves-1
                  UZTHIT(iwav, l) = UZTHST(iwav, l)
                  UZDPIT(iwav, l) = UZDPST(iwav, l)
                  UZSPIT(iwav, l) = UZSPST(iwav, l)
                  UZFLIT(iwav, l) = UZFLST(iwav, l)
                  ITRLIT(iwav, l) = ITRLST(iwav, l)
                  LTRLIT(iwav, l) = LTRLST(iwav, l)
                END DO
C
C5------CALL UZFLOW TO ROUTE WAVES FOR LATEST ITERATION.
                IF ( htest1 .LT. 0.0D0 ) THEN
                   dlength = celtop - h
                ELSE
                  dlength = 0.0
                END IF
                zoldist = UZDPST(iset, l)
Cdep  added lake flag to suppress unsaturated ET beneath a lake
                IF ( IETFLG.GT.0 .AND. lakflg.NE.1 ) THEN
                  rateud = PETRATE(ic, ir)/ROOTDPTH(ic, ir)
                  rootdp = ROOTDPTH(ic, ir)
                  wiltwc = WCWILT(ic, ir)
                ELSE
                  rateud = 0.0D0
                  rootdp = 0.0
                  wiltwc = 0.0
                END IF
                IF ( dlength.LT.0.0D0 ) dlength = 0.0D0
                IF ( zoldist.LT.0.0D0 ) zoldist = 0.0D0
                surflux = finfact
                oldsflx = UZOLSFLX(ic, ir)
                DO ik = 1, idelt
                  totflux = 0.0D0
                  etact = 0.0D0
                  CALL UZFLOW2(l, surflux, dlength,zoldist,UZDPIT(:,l), 
     +                       UZTHIT(:,l), UZFLIT(:,l), UZSPIT(:,l), 
     +                       ITRLIT(:,l), LTRLIT(:,l), totflux, 
     +                       numwaves, thr, ths, fks, epsilon, oldsflx, 
     +                       iset, rateud, etact, wiltwc, rootdp, 
     +                       deltinc)
                  totfluxtot = totfluxtot + totflux
                  totetact = totetact + etact
                  oldsflx = surflux
                  zoldist = dlength
                END DO
                totflux = totfluxtot
                etact = totetact
                IF ( totflux.LT.0.0D0 ) totflux = 0.0D0
                RHS(ic, ir, il) = RHS(ic, ir, il)
     +                            - (totflux*cellarea/DELT)
              ELSE
                RHS(ic, ir, il) = RHS(ic, ir, il)
     +                            - (finfact*cellarea)
                etact = 0.0D0
                
              END IF
            ELSE 
              IF ( finfhold.GT.CLOSEZERO ) THEN
                IF ( SURFDEP1.GT.CLOSEZERO ) THEN
                  finfact = finfhold - (finfhold/SURFDEP1)*
     +                      (h - celtop)
                END IF
                IF ( finfact.GT.0.0 ) THEN 
                  IF ( SURFDEP1.GT.CLOSEZERO ) THEN
                    csep  = cellarea*finfhold/SURFDEP1
                    RHS(ic, ir, il) = RHS(ic, ir, il) -  
     +                                cellarea*finfhold - 
     +                                csep * celtop
                    HCOF(ic, ir, il) = HCOF(ic, ir, il) - csep
                    finfact = finfhold - (finfhold/SURFDEP1)*
     +                        (h - celtop)
                  ELSE 
                    RHS(ic, ir, il) = RHS(ic, ir, il) -
     +                                cellarea*finfhold
                  END IF         
                ELSE
                  finfact = 0.0
                  REJ_INF(ic,ir) = cellarea * finfhold
                END IF
              END IF
Cdep          fin = fin + finfact*cellarea
            END IF
C
C6------GROUNDWATER IS DISCHARGING TO LAND SURFACE.
         
            IF ( htest1.GE.-CLOSEZERO ) THEN
              csepmx = fks*cellarea/(0.5*celthick)
              csep = csepmx - (csepmx/SURFDEP)*((celtop+SURFDEP)-h)
              IF ( csep .GT. csepmx ) csep = csepmx
              SEEPOUT(ic, ir) = (h-celtop)*csep 
              IF ( SEEPOUT(ic, ir).GT.0.0 ) THEN
Cdep  added lake flag to suppress seepout beneath a lake
                IF ( lakid.NE.1 ) THEN
                  RHS(ic, ir, il) = RHS(ic, ir, il) - csep*celtop
                  HCOF(ic, ir, il) = HCOF(ic, ir, il) - csep
                ELSE 
                  SEEPOUT(ic, ir) = 0.0
                END IF
Cdep            fout = fout + SEEPOUT(ic, ir)
              ELSE
                SEEPOUT(ic, ir) = 0.0
              END IF
            ELSE
              SEEPOUT(ic, ir) = 0.0
            END IF
            REJ_INF(ic,ir) = cellarea * ( finfhold - finfact )
C7------CALCULATE ET DEMAND LEFT FOR GROUND WATER.
            IF ( IETFLG.GT.0 ) THEN
              etdp = celtop - ROOTDPTH(ic, ir)
              IF ( h.GT.etdp .AND. h.LT.celtop ) THEN
                s = celtop
                x = ROOTDPTH(ic, ir)
                c = PETRATE(ic, ir)- etact/DELT
                IF ( c.GT.0.0 ) THEN
                  c = c*cellarea
                ELSE
                  c = 0.0
                END IF
                etgw = (c*(h-(s-x))/x)
                IF ( etgw/cellarea+etact/DELT.GT.PETRATE(ic, ir)
     +                ) THEN
Cdep  added lake flag to suppress ET beneath a lake
                  IF ( lakid.NE.1 ) THEN
                    etgw = (PETRATE(ic, ir)-etact/DELT)*cellarea
                    IF ( etgw.lt.0.0 ) THEN
                      c = 0.0
                      etgw = 0.0
                    END IF
                    RHS(ic, ir, il) = RHS(ic, ir, il) + etgw
                  ELSE
                    etgw = 0.0
                    c = 0.0
                  END IF
Cdep  end change
                ELSE
Cdep  added lake flag to suppress ET beneath a lake
                  IF ( lakid.NE.1 ) THEN
                    trhs = c - c*s/x
                    thcof = -c/x
                  ELSE
                    trhs = 0.0
                    thcof = 0.0
                    c = 0.0
                  END IF
Cdep  end change
                  RHS(ic, ir, il) = RHS(ic, ir, il) + trhs
                  HCOF(ic, ir, il) = HCOF(ic, ir, il) + thcof
                  etgw = trhs-(thcof*h)
                END IF
              ELSE IF ( h.GE.celtop ) THEN
Cdep  added lake flag to suppress ET beneath a lake              
                IF ( lakid.NE.1 ) THEN
                  c = PETRATE(ic, ir) - etact/DELT
                ELSE
                  c = 0.0
                END IF
Cdep  end change
                IF ( c.GT.0.0 ) THEN
                  c = c*cellarea
                ELSE
                  c = 0.0
                END IF
                RHS(ic, ir, il) = RHS(ic, ir, il) + c
                etgw = c
              ELSE
                etgw = 0.0
              END IF
              UZFETOUT(ic, ir) = etact*cellarea + etgw*DELT
            END IF
cdep        foutet = foutet + etgw
          END IF
C          uzflwt(ic,ir) =  celtop-HNEW(ic, ir, il)
! RGN commented out next line (very bad). 1/24/08
!          IUZFBND(ic, ir) = il
        END IF
      END DO
C
C8------ADD OVERLAND FLOW TO STREAMS, LAKES AND CONDUITS. 
      IF ( IRUNFLG.GT.0 .AND. (Iunitsfr.GT.0.OR.
     +     Iunitlak.GT.0.OR.Iunitcfp.GT.0) )
     +     CALL SGWF2UZF1OLF(Iunitsfr, Iunitlak, Iunitcfp)
C     IF ( IRUNFLG.GT.0 .AND. (Iunitsfr.GT.0.OR.
C    +     Iunitlak.GT.0) )
C    +     CALL SGWF2UZF1OLF(Iunitsfr, Iunitlak)

C9------RETURN.
      RETURN
      END SUBROUTINE GWF2UZF1FM
C
C--------SUBROUTINE SGWF2UZF1OLF
crgn changed subroutine to add overland flow to conduits
      SUBROUTINE SGWF2UZF1OLF(Iunitsfr, Iunitlak, Iunitcfp)
C     SUBROUTINE SGWF2UZF1OLF(Iunitsfr, Iunitlak)
C     ******************************************************************
C     ASSIGN OVERLAND RUNOFF AS INFLOW TO STREAMS AND LAKES
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE, ONLY: IRUNBND, SEEPOUT, EXCESPP, TOTRUNOFF,
     +                        REJ_INF, TO_CFP, IRUNBIG, IGSFLOW
      USE GLOBAL,       ONLY: NCOL, NROW
      USE GWFSFRMODULE, ONLY: NSS, NSTRM, ISTRM, SEG, STRM
      USE GWFLAKMODULE, ONLY: NLAKES, OVRLNDRNF
!      USE CFPMODULE,    ONLY: QBDIR, Mxnode
      IMPLICIT NONE
C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER Iunitsfr, Iunitlak, Iunitcfp
C     INTEGER Iunitsfr, Iunitlak
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
      INTEGER i, ic, ir, nseg, irun
      REAL rchlen, seglen, gwrunof, seepout1
C     -----------------------------------------------------------------
C1-----INITIALIZE OVERLAND RUNOFF TO STREAMS AND LAKES TO ZERO.
      gwrunof = 0.0
      TOTRUNOFF = 0.0
C
C2-----OVERLAND RUNOFF TO STREAMS FROM UZF SEPARATE FROM 
C        OVERLAND RUNOFF SPECIFIED FOR STREAMS.
      IF ( Iunitsfr.GT.0 ) THEN
        DO i = 1, NSS
          SEG(26, i) = 0.0
        END DO
        DO i = 1, NSTRM
          STRM(24, i) = 0.0
        END DO
      END IF
      IF ( Iunitlak.GT.0 ) THEN
C
C3------OVERLAND RUNOFF TO LAKES SET TO ZERO EVEN IF
C        VALUE IS SPECIFIED FOR A LAKE.
        DO i = 1, NLAKES
          OVRLNDRNF(i) = 0.0
        END DO
      END IF
C     
C3A----IF PRMS SETS ACCEPTS SEEPOUT THEN RETURN
      IF ( IGSFLOW.NE.0 )RETURN
C
C4------LOOP THROUGH IRUNBND ARRAY AND ADD SEEPOUT PLUS EXCESSPP TO
C         CORRECT STREAM SEGMENT OR LAKE.
      DO ir = 1, NROW
        DO ic = 1, NCOL
crgn added rejected infiltration to runoff.
          seepout1 = SEEPOUT(ic, ir) + EXCESPP(ic, ir) + REJ_INF(ic, ir)
          TOTRUNOFF = TOTRUNOFF + seepout1
          IF ( seepout1.GT.0.0 ) THEN
            irun = IRUNBND(ic, ir)
            IF ( irun.GT.0 .AND. irun.LE.NSS .AND. Iunitsfr.GT.0 ) THEN
              SEG(26, irun) = SEG(26, irun) + seepout1
            ELSE IF ( irun.LT.0 .AND. ABS(irun).LE.NLAKES .AND.
     +                Iunitlak.GT.0 ) THEN
              OVRLNDRNF(ABS(irun)) = OVRLNDRNF(ABS(irun)) + seepout1
!            ELSE IF ( irun.GT.IRUNBIG .AND. ABS(irun-IRUNBIG).LE.Mxnode 
!     +                .AND. Iunitcfp.GT.0 ) THEN 
!              i = ABS(irun-IRUNBIG)
!              QBDIR(i) = QBDIR(i) + seepout1 + TO_CFP(ic,ir)
            END IF
          END IF
          SEEPOUT(ic, ir) = 0.0
        END DO
      END DO
C
C5------PROPORTION RUNOFF TO REACHES ON BASIS OF STREAM LENGTH.
      IF ( Iunitsfr.GT.0 ) THEN
        DO i = 1, NSTRM
          nseg = ISTRM(4, i)
          seglen = SEG(1, nseg)
          gwrunof = SEG(26, nseg)
          rchlen = STRM(1, i)
          STRM(24, i) = gwrunof*(rchlen/seglen)
        END DO
      END IF
C
C6-----RETURN.
      RETURN
      END SUBROUTINE SGWF2UZF1OLF
C
C------SUBROUTINE GWF2UZF1BD
! RGN added Iunitbcf and Iunitlpf 1/24/08
      SUBROUTINE GWF2UZF1BD(Kkstp, Kkper, Iunitlak, Iunitbcf, Iunitlpf, 
     +                      Iunithuf, Hdrybcf, Hdryhuf, Igrid)
C     SUBROUTINE GWF2UZF1BD(Kkstp, Kkper, Iunitlak, Igrid)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGETS FOR RECHARGE, ET, AND SURFACE LEAKAGE
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IOUT, ISSFLG, IBOUND, 
     +                        DELR, DELC, HNEW, BUFF, BOTM, 
     +                        ITMUNI
CDEP 05/04/2006
      USE GWFBASMODULE, ONLY: ICBCFL, IBUDFL, TOTIM, PERTIM, DELT, MSUM,
     +                        VBNM, VBVL, HNOFLO
Cdep  added lake flags to suppress seepout and ET beneath a lake
      USE GWFLAKMODULE, ONLY: LKARR1, STGNEW
! RGN Added Hdry 1/24/08
      USE GWFLPFMODULE, ONLY: HDRY
Crgn  added conduit recharge factor to route water to conduits.
!      USE CFPMODULE,    ONLY: Mxnode, QCONDIR
      IMPLICIT NONE
C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
Cdep  added IUNITLAK to INTEGER declare statement
      INTEGER Kkper, Kkstp, Igrid, Iunitlak, Iunitbcf, Iunitlpf 
      INTEGER Iunithuf
      REAL Hdrybcf, Hdryhuf
C     INTEGER Kkper, Kkstp, Igrid, Iunitlak
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
      DOUBLE PRECISION oldsflx, surflux, dlength, zoldist, totflux, fm, 
     +                 etact, rateud, htest1, htest2, h, hld, celtop,  
     +                 flength, width, uzstorhold, hdif,fluxdif, thr,  
     +                 cellarea, prcntdif, depthsave
      DOUBLE PRECISION small, acumdif, aratdif, unsatvol, unsatrat,
     +                 cumdiff, ratedif, fact, totetact, totfluxtot,
     +                 deltinc
      REAL avdpt, avwat, bigvl1, bigvl2, depthinc, epsilon, 
     +     s, x, c, etdp, etgw, eps_m1, ftheta1, ftheta2
      REAL fhold, fks, fminn, gcumin, gcumrch, gdelstor, gdlstr, ghdif, 
     +     ghnw, ginfltr, grchr, gseep, gseepr, guzstore, prcntercum,
     +     prcnterrat, ratin, ratout, cumapplinf
      REAL rin, rootdp, rout, ths, ratout2, fmax, totbet
      REAL csepmx, csep, finfact, finfhold, gcumapl, gaplinfltr
!rsr  REAL finfact2, error
      REAL totalwc, totrin, totrot, totvin, totvot, volet, volflwtb, 
     +     volinflt, wiltwc, zero, celthick
      INTEGER ibd, ibdflg, ibduzf, ic, ick, iftunit, igflg, ii, il, ill,
     +        iog, ir, iset, iss, iuzcol, iuzn, iuzopt, iuzrat, iuzrow, 
     +        j, jj, jk, land, numcells, nwavm1, nwaves, idelt, ik
      INTEGER k, kknt, l, loop, numwaves, numwavhld, nuzc, nuzr, jm1
      INTEGER IUZFB22
!rsr  INTEGER irun, i
Cdep  added lake flags to suppress seepout and ET beneath a lake
      INTEGER lakflg, lakid
      CHARACTER(LEN=16) textrch, textet, textexfl, textinf
      CHARACTER(LEN=16) uzsttext, uzettext, uzinftxt, txthold
      CHARACTER(LEN=17) val1, val2
      DATA textinf/'    UZF INFILTR.'/
      DATA textrch/'    UZF RECHARGE'/
      DATA textet/'           GW ET'/
      DATA textexfl/' SURFACE LEAKAGE'/
      DATA uzinftxt/'    INFILTRATION'/
      DATA uzsttext/'  STORAGE CHANGE'/
      DATA uzettext/'          UZF ET'/
      INTEGER laynum
      ALLOCATABLE laynum(:,:)
      ALLOCATE (laynum(NCOL, NROW))
C     -----------------------------------------------------------------
C
C1------SET POINTERS FOR CURRENT GRID.
      CALL SGWF2UZF1PNT(Igrid)
C      
C2------INITIALIZE CELL BY CELL FLOW TERM FLAG (IBD) AND
C       ACCUMULATORS (RATIN AND RATOUT).
      ibd = 0
      ibduzf = 0
      zero = 0.0
      ratin = zero
      ratout = zero
      ratout2 = zero
      totbet = zero
      NSETS = 1
      cumapplinf = 0.0
      fact = 1.0D0
      IF ( IETFLG.GT.0 ) THEN                               
        IF ( ITMUNI.EQ.1 ) THEN
          fact = 86400.0D0          
        ELSE IF ( ITMUNI.EQ.2 ) THEN
          fact = 1440.0D0           
        ELSE IF ( ITMUNI.EQ.3 ) THEN
          fact = 24.0D0             
        ELSE IF ( ITMUNI.EQ.4 ) THEN
          fact = 1.0D0              
        ELSE IF ( ITMUNI.EQ.5 ) THEN
          fact = 1.0D0/(365.0D0)    
        END IF                                  
        idelt = INT(DELT/fact)   
        IF ( idelt.LT.1 )THEN
          idelt=1
          deltinc = DELT
        ELSE
          deltinc = DELT/idelt
        END IF  
      ELSE                          
        deltinc = DELT              
        idelt = 1                   
      END IF
      IF ( deltinc.GT.DELT ) THEN
        deltinc = DELT
        idelt = 1
      END IF
      iss = ISSFLG(Kkper)
      IF ( IUZFCB1.NE.0 ) ibd = ICBCFL
      IF ( IUZFCB2.NE.0 ) ibduzf = ICBCFL
      IUZFCB1 = ABS(IUZFCB1)
      IUZFB22 = IUZFCB2
      IUZFCB2 = ABS(IUZFCB2)
C
C3------WRITE HEADER IF UNSATURATED STORAGE TERMS ARE SAVED.
C      IF(IBDUZF.EQ.1)
C      IF(IBDUZF.EQ.2)
C
C4------CLEAR BUFFERS.
CDEP 05/05/2006
      DO il = 1, NLAY
        DO ir = 1, NROW
          DO ic = 1, NCOL
            BUFF(ic, ir, il) = HNOFLO
            laynum(ic,ir) = 1
          END DO
        END DO
      END DO
      DO iuzrat = 1, 6
        UZTSRAT(iuzrat) = zero
      END DO
      numcells = NROW*NCOL
      DO l = 1, numcells
        ir = IUZHOLD(1, l)
        ic = IUZHOLD(2, l)
        finfhold = FINF(ic,ir)
Crgn added code to route water to conduits.
C moved cellarea calculation before conduit 4/9/07 
C set excess precipitation to zero for integrated (GSFLOW) simulation
        IF ( IGSFLOW.GT.0 ) Excespp(ic,ir) = 0.0
!rsr    finfact2 = 0.0
        flength = DELC(ir)
        width = DELR(ic)
        cellarea = width*flength
Crgn added code to route water to conduits.
!rsr    finfact2 = 0.0
!        IF ( Iunitcfp.GT.0 ) THEN
!          irun = abs(IRUNBND(ic, ir))
!          i = irun - IRUNBIG
!          IF ( i.GT.0 .AND. i.LT.Mxnode ) THEN
!            finfact2 = QCONDIR(i)
!            IF ( finfact2.GT.1.0 ) finfact2 = 1.0
!            IF ( finfact2.LT.0.0 ) finfact2 = 0.0
!            TO_CFP(ic,ir) = cellarea*finfact2*FINF(ic,ir)
!            finfhold = FINF(ic,ir) - finfact2*FINF(ic,ir)
!            IF ( finfhold.LT.0.0 ) finfhold = 0.0
!          END IF 
!        END IF
        etgw = zero
        c = zero
        etact = 0.0D0
        totfluxtot = 0.0D0
        totetact = 0.0D0
        il = 1
        land = IUZFBND(ic, ir)
        finfact = finfhold
        REJ_INF(ic, ir) = 0.0
C
C5-----SEARCH FOR UPPERMOST ACTIVE CELL.
        IF ( IUZFBND(ic, ir).GT.0 ) THEN
          IF ( NUZTOP.EQ.1 ) THEN
            il = 1
            IF ( IBOUND(ic, ir, il).LT.1 )il = 0
            IF ( IUZFBND(ic, ir).LT.1 ) il = 0
          ELSE IF ( NUZTOP.EQ.2 ) THEN
            il = IUZFBND(ic, ir)
            IF ( il.GT.0 ) THEN
              IF ( IBOUND(ic, ir, il).LT.1 ) il = 0
            ELSE
              il = 0
            END IF
C
C6------PRINT WARNING WHEN NUZTOP IS 1 OR 2 AND ALL LAYERS ARE INACTIVE.
            IF ( NUZTOP.EQ.1 .AND. il.EQ.0 ) THEN
              WRITE (IOUT, *) '***WARNING***NUZTOP IS 1 AND UPPERMOST', 
     +                        ' LAYER FOR ROW ', ir, ' AND COLUMN ', ic,
     +                        ' IS INACTIVE.'
              WRITE (IOUT, *) 'UNSATURATED FLOW WILL NOT BE ADDED TO ',
     +                        'AN ACTIVE LAYER-- SUGGEST CHANGING ',
     +                        'NUZTOP TO 3'
            ELSE IF ( NUZTOP.EQ.2 .AND. il.EQ.0 ) THEN
              WRITE (IOUT, *) '***WARNING***NUZTOP IS 2 AND SPECIFIED', 
     +                        ' LAYER FOR ROW ', ir, ' AND COLUMN ', ic,
     +                        ' IS INACTIVE.'
              WRITE (IOUT, *) 'UNSATURATED FLOW WILL NOT BE ADDED TO ',
     +                        'AN ACTIVE LAYER-- SUGGEST CHANGING ',
     +                        'NUZTOP TO 3'
            END IF
          ELSE IF ( NUZTOP.EQ.3 ) THEN
            ill = 1
            ibdflg = 1
            DO WHILE ( ibdflg.EQ.1 )
              IF ( IBOUND(ic, ir, ill).LT.0 ) THEN
                ibdflg = 0
                il = 0
              ELSE IF ( IBOUND(ic, ir, ill).EQ.0 ) THEN
                ibdflg = 1
              ELSE IF ( IBOUND(ic, ir, ill).GT.0 ) THEN
                ibdflg = 0
                il = ill
              END IF
CRGN made il = 0 when all layers for column are inactive 2/21/08
              IF ( ill.EQ.NLAY .AND. ibdflg.EQ.1 ) THEN
                ibdflg = 0
                il = 0
              END IF
              ill = ill + 1
            END DO
C
C7------PRINT WARNING WHEN NUZTOP IS 3 AND ALL LAYERS ARE INACTIVE.
            IF ( il.EQ.0 ) THEN
              WRITE (IOUT, *) '***WARNING***NUZTOP IS 3 AND ALL LAYERS '
     +                        , ' IN ROW ', ir, ' AND COLUMN ', ic, 
     +                        ' ARE', ' INACTIVE.'
              WRITE (IOUT, *) 
     +                       'UNSATURATED FLOW WILL NOT BE ADDED TO ANY'
     +                       , ' LAYER-- SOME WATER MAY FLOW PAST', 
     +                       ' LOWEST LAYER'
            END IF
          END IF
          laynum(ic, ir) = il
Cdep  added lake flags to suppress seepout and ET beneath a lake
          lakflg = 0
          lakid = 0
          IF ( Iunitlak.GT.0 .AND. il.GT.1 ) THEN
            lakid = LKARR1(ic,ir,il-1)
            IF ( lakid.GT.0 ) THEN
              IF( STGNEW(lakid).GT.BOTM(ic, ir, il-1) )
     +            lakflg = 1
            END IF
          END IF
Cdep  end change
          IF ( il.GT.0 .AND. VKS(ic, ir).GT.NEARZERO ) THEN
            IF ( IBOUND(ic, ir, il).GT.0 ) THEN
              h = HNEW(ic, ir, il)
            ELSE
              h = BOTM(ic, ir, NLAY)
            END IF
            hld = HLDUZF(ic, ir)
! Added code to test for BCF or LPF 1/24/08
            IF ( Iunitbcf.NE.0 ) THEN
              IF ( ABS(SNGL(hld)-Hdrybcf).LT.CLOSEZERO ) hld = h
            ELSE IF ( Iunitlpf.NE.0 ) THEN
              IF ( ABS(SNGL(hld)-HDRY).LT.CLOSEZERO ) hld = h
            ELSE IF ( Iunithuf.NE.0 ) THEN
              IF ( ABS(SNGL(hld)-Hdryhuf).LT.CLOSEZERO ) hld = h
            END IF
            HLDUZF(ic,ir) = h
            IF ( NUZTOP.EQ.1 ) THEN
              celtop = BOTM(ic, ir, 0) - 0.5 * SURFDEP
              celthick = BOTM(ic, ir, 0) - BOTM(ic, ir, 1)
            ELSE
              celtop = BOTM(ic, ir, land-1) - 0.5 * SURFDEP
              celthick = BOTM(ic, ir, land-1) - BOTM(ic, ir, land)
            END IF
            fks = VKS(ic, ir)
            etact = 0.0D0
C
C8------SET NWAVES TO 1 WHEN IUZFOPT IS NEGATIVE.
            IF ( IUZFOPT.GT.0 ) THEN
!rsr, set below          eps_m1 = EPS(ic, ir) - 1.0
              thr = THTR(ic, ir)
              epsilon = EPS(ic, ir)
              ths = THTS(ic, ir)
              fluxdif = ABS(finfhold-UZOLSFLX(ic, ir))
              nwaves = NWAVST(ic, ir)
            ELSE
              fluxdif = 0.0 !rsr, added to be sure fluxdif has a value
              nwaves = 1
            END IF
Cdep  Added lake flag to suppress unsaturated ET beneath a lake
            IF ( IETFLG.GT.0 .AND. lakflg.NE.1 ) THEN
              rateud = PETRATE(ic, ir)/ROOTDPTH(ic, ir)
              rootdp = ROOTDPTH(ic, ir)
              IF ( IUZFOPT.GT.0 ) wiltwc = WCWILT(ic, ir)
            ELSE
              rateud = 0.0D0
              wiltwc = 0.0
              rootdp = 0.0
            END IF
            iset = 1
            iuzn = 1
            htest1 = h - celtop
            htest2 = hld - celtop
            hdif = ABS(h-hld)
            IF ( htest1.GE.0.0D0 .AND. htest2.GE.0.0D0 ) THEN
Cdep  Added lake flag to suppress SEEPOUT beneath a lake
              IF ( lakflg.NE.1 ) THEN
                csepmx = fks*cellarea/(0.5*celthick)
                csep = csepmx - (csepmx/SURFDEP)*((celtop+SURFDEP)-h)
                IF ( csep .GT. csepmx ) csep = csepmx
                SEEPOUT(ic, ir) = (h-celtop)*csep 
              ELSE
                SEEPOUT(ic, ir) = 0.0
              END IF
              IF( SEEPOUT(ic, ir).LT.0.0 ) SEEPOUT(ic, ir) = 0.0
              IF ( ABS(IUZFOPT).GT.0 ) THEN
                IF ( finfhold.GT.CLOSEZERO ) THEN
                  IF ( SURFDEP1.GT.CLOSEZERO )THEN
                    finfact = finfhold - (finfhold/SURFDEP1)*
     +                    (h - celtop)
                  END IF
                  IF ( finfact.GT.0.0 ) THEN 
                    REJ_INF(ic,ir) = cellarea * 
     +                               ( finfhold - finfact )
                  ELSE
                    finfact = 0.0
                    REJ_INF(ic,ir) = cellarea * finfhold
                  END IF
                END IF
                UZFLWT(ic, ir) = finfact*cellarea*DELT
cupdate
                UZOLSFLX(ic, ir) = finfact
                totflux = 0.0D0
                DELSTOR(ic, ir) = 0.0D0
                UZSTOR(ic, ir) = 0.0D0
                UZDPST(1, l) = 0.0D0
                UZTHST(1, l) = thr
              END IF
C
C9------CALCULATE ET FROM GROUND WATER.
Cdep  Added lake flag to suppress ET beneath a lake
              IF ( IETFLG.GT.0 .AND. lakflg.NE.1 ) THEN
                c = PETRATE(ic, ir)
                IF ( c.GT.0.0 ) THEN
                  c = c*cellarea
                ELSE
                  c = 0.0
                END IF
                etgw = c
                IF ( etgw/cellarea.GT.PETRATE(ic, ir) ) THEN
                  etgw = PETRATE(ic, ir)*cellarea
                  c = etgw
                  IF ( c.lt.0.0 ) THEN
                    c = 0.0
                    etgw = 0.0
                  END IF
                END IF
                UZFETOUT(ic, ir) = etgw*DELT
                GWET(ic, ir) = etgw
              END IF
C
C10-----REMOVE ALL UNSATURATED ZONE WAVES AND CALCULATE CHANGE IN  
C         STORAGE WHEN WATER TABLE RISES TO LAND SURFACE.
            ELSE IF ( htest1.GE.0.0D0 .AND. htest2.LT.0.0D0 ) THEN
Cdep  Added lake flag to suppress SEEPOUT beneath a lake
              IF ( lakflg.NE.1 )THEN
                csepmx = fks*cellarea/(0.5*celthick)
                csep = csepmx - (csepmx/SURFDEP)*((celtop+SURFDEP)-h)
                IF ( csep .GT. csepmx ) csep = csepmx
                SEEPOUT(ic, ir) = (h-celtop)*csep 
              ELSE
                SEEPOUT(ic, ir) = 0.0
              END IF
              IF( SEEPOUT(ic, ir).LT.0.0 ) SEEPOUT(ic, ir) = 0.0
              IF ( finfhold.GT.CLOSEZERO ) THEN
                IF ( SURFDEP1.GT.CLOSEZERO ) THEN
                  finfact = finfhold - (finfhold/SURFDEP1)*
     +                     (h - celtop)
                END IF
                IF ( finfact.GT.0.0 ) THEN 
                  REJ_INF(ic,ir) = cellarea * ( finfhold - 
     +                               finfact )
                ELSE
                  finfact = 0.0
                  REJ_INF(ic,ir) = cellarea * finfhold
                END IF
              END IF
              IF ( IUZFOPT.GT.0 ) THEN
                IF ( iss.EQ.0 ) THEN
                  totflux = 0.0D0
                  DELSTOR(ic, ir) = -UZSTOR(ic, ir)
                  UZSTOR(ic, ir) = 0.0D0
                  dlength = 0.0D0
                  zoldist = UZDPST(iset, l)
                  IF ( dlength.LT.0.0 ) dlength = 0.0D0
                  IF ( zoldist.LT.0.0 ) zoldist = 0.0D0
                  oldsflx = UZOLSFLX(ic, ir)
                  surflux = finfact
                  numwaves = NWAVST(ic, ir)
                  DO ik = 1, idelt
                    totflux = 0.0D0
                    etact = 0.0D0
                    CALL UZFLOW2(l, surflux, dlength, zoldist, 
     +                         UZDPST(:,l), UZTHST(:,l), UZFLST(:,l), 
     +                         UZSPST(:,l), ITRLST(:,l), LTRLST(:,l), 
     +                         totflux, numwaves, thr, ths, fks, 
     +                         epsilon, oldsflx, iset, rateud, etact, 
     +                         wiltwc, rootdp, deltinc)
                    totfluxtot = totfluxtot + totflux
                    totetact = totetact + etact
                    oldsflx = surflux
                    zoldist = dlength
                  END DO
                  totflux = totfluxtot
                  etact = totetact
C
C11-----RESET WAVE CHARACTERISTICS.
                  UZDPST(iset, l) = 0.0D0
                  UZTHST(iset, l) = thr
                  UZFLST(iset, l) = 0.0D0
                  UZSPST(iset, l) = 0.0D0
                  ITRLST(iset, l) = 0
                  LTRLST(iset, l) = 0
                  NWAVST(ic, ir) = 1
                  DO ii = iset + 1, (iset+NWAV/iuzn) - 1
                    UZDPST(ii, l) = 0.0D0
                    UZTHST(ii, l) = thr
                    UZFLST(ii, l) = 0.0D0
                    UZSPST(ii, l) = 0.0D0
                    ITRLST(ii, l) = 0
                    LTRLST(ii, l) = 0
                  END DO
                  UZFLWT(ic, ir) = totflux*cellarea
                  UZOLSFLX(ic, ir) = finfact
                ELSE
                  UZFLWT(ic, ir) = finfact*cellarea*DELT
                  UZOLSFLX(ic, ir) = finfact
                END IF
              ELSE
                UZFLWT(ic, ir) = finfact*cellarea*DELT
                UZOLSFLX(ic, ir) = finfact
              END IF
C
C12-----CALCULATE ET DEMAND LEFT FOR GROUND WATER.
Cdep  Added lake flag to suppress ET beneath a lake
              IF ( IETFLG.GT.0 .AND. lakflg.NE.1 ) THEN
                c = PETRATE(ic, ir) - etact/DELT
                IF ( c.GT.0.0 ) THEN
                  c = c*cellarea
                ELSE
                  c = 0.0
                END IF
                etgw = c
                IF ( etgw/cellarea+etact/DELT.GT.PETRATE(ic, ir)
     +                ) THEN
                  etgw = (PETRATE(ic, ir)-etact/DELT)*cellarea
                  c = etgw
                  IF ( c.lt.0.0 ) THEN
                    c = 0.0
                    etgw = 0.0
                  END IF
                END IF
                UZFETOUT(ic, ir) = etgw*DELT + etact*cellarea
                GWET(ic, ir) = etgw
              END IF
C
C13-----UPDATE UNSATURATED ZONE WAVES WHEN THE WATER TABLE REMAINS
C         BELOW LAND SURFACE AND CALCULATE CHANGE IN STORAGE.         
            ELSE IF ( hdif.LT.1.0E-5 .AND. nwaves.EQ.1 .AND. 
     +                fluxdif.LT.1.0E-9 .AND. htest1.LT.2.0E-5 .AND. 
     +                IETFLG.EQ.0 ) THEN
              IF ( IUZFOPT.GT.0 ) THEN
                IF ( iss.EQ.0 ) THEN
                  DELSTOR(ic, ir) = 0.0D0
                  zoldist = celtop - hld
                  dlength = celtop - h
                  UZDPST(1, l) = dlength
                  UZSTOR(ic, ir) = UZDPST(1, l)*(UZTHST(1, l)-thr)
     +                             *cellarea
                END IF
                UZFLWT(ic, ir) = finfact*cellarea*DELT
                UZOLSFLX(ic, ir) = finfact
                totflux = finfhold*DELT
                REJ_INF(ic,ir) = 0.0
              END IF
            ELSE IF ( htest1.LT.0.0D0 .AND. htest2.LT.0.0D0 ) THEN
Cdep  Added lake flag to suppress SEEPOUT beneath a lake
              IF ( IUZFOPT.GT.0 ) THEN
                IF ( iss.EQ.0 ) THEN
                  dlength = celtop - h
                  zoldist = celtop - hld
                  totflux = 0.0D0
                  IF ( dlength.LT.0.0 ) dlength = 0.0D0
                  IF ( zoldist.LT.0.0 ) zoldist = 0.0D0
                  surflux = finfhold
                  oldsflx = UZOLSFLX(ic, ir)
                  numwaves = NWAVST(ic, ir)
                  DO ik = 1, idelt
                    totflux = 0.0D0
                    etact = 0.0D0
                    CALL UZFLOW2(l, surflux, dlength, zoldist, 
     +                         UZDPST(:,l), UZTHST(:,l), UZFLST(:,l), 
     +                         UZSPST(:,l), ITRLST(:,l), LTRLST(:,l), 
     +                         totflux, numwaves, thr, ths, fks, 
     +                         epsilon, oldsflx, iset, rateud, etact, 
     +                         wiltwc, rootdp, deltinc)
                    totfluxtot = totfluxtot + totflux
                    totetact = totetact + etact
                    oldsflx = surflux
                    zoldist = dlength
                  END DO
                  totflux = totfluxtot
                  etact = totetact
                  loop = 1
                  ick = 0
                  NWAVST(ic, ir) = numwaves
                  IF ( loop.GT.0 ) THEN
C
C15-----CALCULATE CHANGE IN UNSATURATED ZONE STORAGE WHEN WATER TABLE
C          RISES.
                    IF ( h.GT.hld ) THEN
                      fm = 0.0D0
                      depthsave = UZDPST(iset, l)
                      jj = iset
                      jk = iset + 1
                      DO WHILE ( jk.LE.iset+NWAVST(ic, ir)-1 )
                        IF ( celtop-UZDPST(jk, l).LE.h ) jj = jk
                        jk = jk + 1
                      END DO
                      jk = iset + 1
C
C16-----WATER TABLE RISES THROUGH WAVES.                           
                      IF ( jj.GE.jk ) THEN
                        DO j = iset, iset + NWAVST(ic, ir) - 1
                          ITRLSTH(j) = ITRLST(j, l)
                        END DO
                        numwavhld = NWAVST(ic, ir)
                        NWAVST(ic, ir) = NWAVST(ic, ir) - (jj-iset)
                        UZDPST(iset, l) = depthsave - (h-hld)
                        UZTHST(iset, l) = UZTHST(jj, l)
                        UZFLST(iset, l) = UZFLST(jj, l)
                        UZSPST(iset, l) = 0.0D0
                        ITRLST(iset, l) = 0
                        LTRLST(iset, l) = 0
                        k = iset + 1
                        DO j = jj + 1, iset + numwavhld - 1
                          UZDPST(k, l) = UZDPST(j, l)
                          UZTHST(k, l) = UZTHST(j, l)
                          UZFLST(k, l) = UZFLST(j, l)
                          UZSPST(k, l) = UZSPST(j, l)
                          ITRLST(k, l) = ITRLST(j, l)
                          LTRLST(k, l) = LTRLST(j, l)
                          k = k + 1
                        END DO
C
C17-----LOOP THROUGH NUMBER OF TRAIL WAVES INTERSECTED BY WATER TABLE.                       
                        eps_m1 = EPS(ic, ir) - 1.0 !rsr, set here rather than above to be sure it has a value
                        DO j = iset, jj + 1                  
                          IF ( j.EQ.jj+1 ) THEN
                            IF ( ITRLSTH(j).GT.0 ) THEN
C18-----LEAD TRAIL WAVE BELOW WATER TABLE AND FIRST TRAIL WAVE IS 
C         ABOVE WATER TABLE.                            
                              IF ( ITRLSTH(j).EQ.1 ) THEN
                                jm1 = j - 1
                                LTRLST(jm1, l) = 1
                                ITRLST(jm1, l) = 0
                                fhold = (UZTHST(jm1, l)-thr)
     +                                  /(ths-thr)
                                IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                IF ( ABS(UZTHST(jm1, l)-UZTHST(j-2, l))
     +                               .LT.CLOSEZERO ) THEN
                                  fhold = ((UZTHST(jm1, l)-thr)/
     +                                    (ths-thr))**epsilon
                                  IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                  UZSPST(jm1, l) = (epsilon*fks/
     +                                          (ths-thr))*fhold**eps_m1
                                ELSE
                                  fhold = ((UZTHST(j-2, l)-thr)/
     +                                    (ths-thr))**epsilon
                                  IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                  ftheta1 = fks*fhold
                                  fhold = ((UZTHST(jm1, l)-thr)/
     +                                    (ths-thr))**epsilon
                                  IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                  ftheta2 = fks*fhold
                                  UZSPST(jm1, l) = (ftheta1-ftheta2)
     +                                  /(UZTHST(j-2, l)-UZTHST(jm1, l))
                                END IF
                              ELSE
C
C19-----LEAD TRAIL WAVE BELOW WATER TABLE AND MULTIPLE TRAIL WAVES 
C         ABOVE WATER TABLE.
                                DO k = iset + 1, iset + ITRLSTH(j)
                                  LTRLST(k, l) = 1
                                  ITRLST(k, l) = 0
                                  fhold = (UZTHST(k, l)-thr)/(ths-thr)
                                  IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                  IF ( ABS(UZTHST(k, l)-UZTHST(k-1, l))
     +                                 .LT.CLOSEZERO ) THEN
                                    fhold = ((UZTHST(k, l)-thr)/
     +                                (ths-thr))**epsilon
                                    IF ( fhold.LT.CLOSEZERO )
     +                                   fhold = 0.0
                                    UZSPST(k, l)
     +                                = (epsilon*fks/(ths-thr))
     +                                *fhold**eps_m1
                                  ELSE
                                    fhold = ((UZTHST(k-1, l)-thr)
     +                                /(ths-thr))**epsilon
                                    IF ( fhold.LT.CLOSEZERO )
     +                                   fhold = 0.0
                                    ftheta1 = fks*fhold
                                    fhold = ((UZTHST(k, l)-thr)/
     +                                (ths-thr))**epsilon
                                    IF ( fhold.LT.CLOSEZERO )
     +                                   fhold = 0.0
                                    ftheta2 = fks*fhold
                                    UZSPST(k, l) = (ftheta1-ftheta2)
     +                                /(UZTHST(k-1, l)-UZTHST(k, l))
                                  END IF
                                END DO
                              END IF
                            END IF
                          ELSE IF ( j.NE.jj ) THEN
C
C20-----MULTIPLE TRAIL WAVES BELOW AND ABOVE WATER TABLE.
                            IF ( ITRLSTH(j).GT.jj-j+1 ) THEN
                              DO k = iset + 1, iset + ITRLSTH(j)
     +                          - (jj-j) - 1
                                LTRLST(k, l) = 1
                                ITRLST(k, l) = 0
                                fhold = (UZTHST(k, l)-thr)/(ths-thr)
                                IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                IF ( ABS(UZTHST(k, l)-UZTHST(k-1, l))
     +                               .LT.CLOSEZERO ) THEN
                                  fhold = ((UZTHST(k, l)-thr)/(ths-thr))
     +                              **epsilon
                                  IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                  UZSPST(k, l) = (epsilon*fks/(ths-thr))
     +                              *fhold**eps_m1
                                ELSE
                                  fhold = ((UZTHST(k-1, l)-thr)/
     +                              (ths-thr))**epsilon
                                  IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                  ftheta1 = fks*fhold
                                  fhold = ((UZTHST(k, l)-thr)/(ths-thr))
     +                              **epsilon
                                  IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                  ftheta2 = fks*fhold
                                  UZSPST(k, l) = (ftheta1-ftheta2)
     +                              /(UZTHST(k-1, l)-UZTHST(k, l))
                                END IF
                              END DO
                            END IF
C
C21-----ONLY ONE LEAD TRAIL AND ONE TRAIL WAVE BELOW WATER TABLE
C         AND THERE ARE MUTIPLE TRAIL WAVES IN SET ABOVE WATER TABLE.
                          ELSE IF ( ITRLSTH(j).GT.1 ) THEN
                            DO k = iset + 1, iset + ITRLSTH(j) - 1
                              LTRLST(k, l) = 1
                              ITRLST(k, l) = 0
                              IF ( ABS(UZTHST(k, l)-UZTHST(k-1, l))
     +                             .LT.CLOSEZERO ) THEN
                                fhold = ((UZTHST(k, l)-thr)/(ths-thr))
     +                                  **epsilon
                                IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                UZSPST(k, l) = (epsilon*fks/(ths-thr))
     +                            *fhold**eps_m1
                              ELSE
                                fhold = ((UZTHST(k-1, l)-thr)/(ths-thr))
     +                                  **epsilon
                                IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                ftheta1 = fks*fhold
                                fhold = ((UZTHST(k, l)-thr)/(ths-thr))
     +                                  **epsilon
                                IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                                ftheta2 = fks*fhold
                                UZSPST(k, l) = (ftheta1-ftheta2)
     +                            /(UZTHST(k-1, l)-UZTHST(k, l))
                              END IF
                            END DO
                          END IF
                        END DO
C
C22-----DETERMINE VOLUME OF WATER IN WAVES BELOW WATER TABLE. 
                        fm = 0.0D0
                        j = iset
                        DO WHILE ( j.LE.iset+NWAVST(ic, ir)-2 )
                          IF ( LTRLST(j, l).EQ.1 .AND. ITRLST(j+1, l)
     +                         .GT.0 ) THEN
                            k = j
                            DO WHILE ( k.LE.j+ITRLST(j+1, l)-1 )
                              fm = fm + (UZTHST(k, l)-thr)
     +                             *(UZDPST(k, l)-UZDPST(k+1, l))
                              k = k + 1
                            END DO
                            IF ( k.EQ.iset+NWAVST(ic, ir)-1 ) THEN
                              fm = fm + (UZTHST(k, l)-thr)*UZDPST(k, l)
                            ELSE IF ( iset+NWAVST(ic, ir)-1.GT.k+1 .AND.
     +                                ITRLST(k+2, l).GT.0 .AND. 
     +                                LTRLST(k+1, l).EQ.1 ) THEN
                              fm = fm + (UZTHST(k, l)-thr)
     +                             *(UZDPST(k, l)-UZDPST(k+1, l))
                            ELSE
                              fm = fm + (UZTHST(k, l)-thr)
     +                             *(UZDPST(k, l)-UZDPST(k+1, l))
                            END IF
                            j = k
                          ELSE
                            fm = fm + (UZTHST(j, l)-thr)
     +                           *(UZDPST(j, l)-UZDPST(j+1, l))
                          END IF
                          j = j + 1
                        END DO
                        IF ( j.EQ.iset+NWAVST(ic, ir)-1 ) fm = fm + 
     +                       (UZTHST(iset+NWAVST(ic,ir)-1, l)-thr)
     +                       *UZDPST(iset+NWAVST(ic, ir)-1, l)
C
C23-----COMPUTE VOLUME OF WATER BELOW WATER TABLE WHEN
C         NO WAVES ARE INTERSECTED.     
                      ELSE
                        UZDPST(iset, l) = celtop - h
                        fm = 0.0D0
                        j = iset
                        DO WHILE ( j.LE.iset+NWAVST(ic, ir)-2 )
                          IF ( LTRLST(j, l).EQ.1 .AND. ITRLST(j+1, l)
     +                         .GT.0 ) THEN
                            k = j
                            DO WHILE ( k.LE.j+ITRLST(j+1, l)-1 )
                              fm = fm + (UZTHST(k, l)-thr)
     +                             *(UZDPST(k, l)-UZDPST(k+1, l))
                              k = k + 1
                            END DO
                            IF ( k.EQ.iset+NWAVST(ic, ir)-1 ) THEN
                              fm = fm + (UZTHST(k, l)-thr)*UZDPST(k, l)
                            ELSE IF ( iset+NWAVST(ic, ir)-1.GT.k+1 .AND.
     +                                ITRLST(k+2, l).GT.0 .AND. 
     +                                LTRLST(k+1, l).EQ.1 ) THEN
                              fm = fm + (UZTHST(k, l)-thr)
     +                             *(UZDPST(k, l)-UZDPST(k+1, l))
                            ELSE
                              fm = fm + (UZTHST(k, l)-thr)
     +                             *(UZDPST(k, l)-UZDPST(k+1, l))
                            END IF
                            j = k
                          ELSE
                            fm = fm + (UZTHST(j, l)-thr)
     +                           *(UZDPST(j, l)-UZDPST(j+1, l))
                          END IF
                          j = j + 1
                        END DO
                        IF ( j.EQ.iset+NWAVST(ic, ir)-1 ) fm = fm + 
     +                       (UZTHST(iset+NWAVST(ic,ir)-1, l)-thr)
     +                       *UZDPST(iset+NWAVST(ic, ir)-1, l)
                      END IF
                      IF ( fm.LT.0.0 ) fm = 0.0D0
                      uzstorhold = UZSTOR(ic, ir)
                      UZSTOR(ic, ir) = fm*cellarea
                      DELSTOR(ic, ir) = UZSTOR(ic, ir) - uzstorhold

C
C24------CALCULATE CHANGE IN UNSATURATED ZONE STORAGE WHEN WATER 
C          TABLE DECLINES.
                    ELSE IF ( h.LE.hld ) THEN
                      fm = 0.0D0
                      j = iset
                      DO WHILE ( j.LE.iset+NWAVST(ic, ir)-2 )
                        IF ( LTRLST(j, l).EQ.1 .AND. ITRLST(j+1, l)
     +                       .GT.0 ) THEN
                          k = j
                          DO WHILE ( k.LE.j+ITRLST(j+1, l)-1 )
                            fm = fm + (UZTHST(k, l)-thr)
     +                           *(UZDPST(k, l)-UZDPST(k+1, l))
                            k = k + 1
                          END DO
                          IF ( k.EQ.iset+NWAVST(ic, ir)-1 ) THEN
                            fm = fm + (UZTHST(k, l)-thr)*UZDPST(k, l)
                          ELSE IF ( iset+NWAVST(ic, ir)-1.GT.k+1 .AND. 
     +                              ITRLST(k+2, l).GT.0 .AND. 
     +                              LTRLST(k+1, l).EQ.1 ) THEN
                            fm = fm + (UZTHST(k, l)-thr)
     +                           *(UZDPST(k, l)-UZDPST(k+1, l))
                          ELSE
                            fm = fm + (UZTHST(k, l)-thr)
     +                           *(UZDPST(k, l)-UZDPST(k+1, l))
                          END IF
                          j = k
                        ELSE
                          fm = fm + (UZTHST(j, l)-thr)
     +                         *(UZDPST(j, l)-UZDPST(j+1, l))
                        END IF
                        j = j + 1
                      END DO
                      IF ( j.EQ.iset+NWAVST(ic, ir)-1 ) fm = fm + 
     +                     (UZTHST(iset+NWAVST(ic,ir)-1, l)-thr)
     +                     *UZDPST(iset+NWAVST(ic, ir)-1, l)
                      uzstorhold = UZSTOR(ic, ir)
                      UZSTOR(ic, ir) = fm*cellarea
                      DELSTOR(ic, ir) = UZSTOR(ic, ir) - uzstorhold
                      UZDPST(iset, l) = celtop - h
                    END IF                    
                    UZFLWT(ic, ir) = totflux*cellarea
                    UZOLSFLX(ic, ir) = surflux
                  ELSE
                    UZFLWT(ic, ir) = 0.0D0
                    UZOLSFLX(ic, ir) = 0.0D0
                  END IF
                ELSE
                  UZFLWT(ic, ir) = finfact*cellarea*DELT
                  UZOLSFLX(ic, ir) = finfact
                END IF
              END IF
C
C25-----CALCULATE ET DEMAND LEFT FOR GROUND WATER.
Cdep  Added lake flag to suppress ET beneath a lake
              IF ( IETFLG.GT.0 .AND. lakflg.NE.1 ) THEN
                etdp = celtop - ROOTDPTH(ic, ir)
                IF ( h.GT.etdp ) THEN
                  s = celtop
                  x = ROOTDPTH(ic, ir)
                  c = PETRATE(ic, ir) - etact/DELT
                  IF ( c.GT.0.0 ) THEN
                    c = c*cellarea
                  ELSE
                    c = 0.0
                  END IF
                  etgw = c*(h-(s-x))/x
                ELSE
                  etgw = 0.0
                END IF
                c = etgw
                IF ( etgw/cellarea+etact/DELT.GT.PETRATE(ic, ir)
     +               ) THEN
                  etgw = (PETRATE(ic, ir)-etact/DELT)*cellarea
                  c = etgw
                  IF ( c.lt.0.0 ) THEN
                    c = 0.0
                    etgw = 0.0
                  END IF
                END IF           
                UZFETOUT(ic, ir) = etact*cellarea + etgw*DELT
                GWET(ic, ir) = etgw
              END IF
C
C26------UPDATE ALL VADOSE ZONE WAVES WHEN WATER TABLE
C          DROPS BELOW LAND SURFACE.
            ELSE IF ( htest1.LT.0.0D0 .AND. htest2.GE.0.0D0 ) THEN
Cdep  Added lake flag to suppress SEEPOUT beneath a lake
              IF ( IUZFOPT.GT.0 ) THEN
                IF ( iss.EQ.0 ) THEN
                  totflux = 0.0D0
                  DELSTOR(ic, ir) = 0.0D0
                  dlength = celtop - h
                  zoldist = 0.0D0
                  UZDPST(1, l) = 0.0D0
                  UZTHST(1, l) = thr
                  UZFLST(1, l) = 0.0D0
                  UZSPST(1, l) = 0.0D0
                  IF ( dlength.LT.0.0 ) dlength = 0.0D0
                  IF ( zoldist.LT.0.0 ) zoldist = 0.0D0
                  surflux = finfhold
                  finfact = finfhold
                  oldsflx = 0.0D0
                  numwaves = 1
                  DO ik = 1, idelt
                    totflux = 0.0D0
                    etact = 0.0D0
                    CALL UZFLOW2(l, surflux, dlength, zoldist, 
     +                         UZDPST(:,l), UZTHST(:,l), UZFLST(:,l), 
     +                         UZSPST(:,l), ITRLST(:,l), LTRLST(:,l), 
     +                         totflux, numwaves, thr, ths, fks, 
     +                         epsilon, oldsflx, iset, rateud, etact, 
     +                         wiltwc, rootdp, deltinc)
                    totfluxtot = totfluxtot + totflux
                    totetact = totetact + etact
                    oldsflx = surflux
                    zoldist = dlength
                  END DO
                  totflux = totfluxtot
                  etact = totetact
                  NWAVST(ic, ir) = numwaves
                  loop = 0
                  ick = 0
                  IF ( UZTHST(iset, l).GT.thr .OR. NWAVST(ic, ir).GT.1 )
     +                 ick = 1
                  IF ( surflux.GT.0.0D0 .OR. ick.EQ.1 ) loop = 1
                  IF ( loop.GT.0 ) THEN
                    fm = 0.0D0
                    j = iset
                    DO WHILE ( j.LE.iset+NWAVST(ic, ir)-2 )
                      IF ( LTRLST(j, l).EQ.1 .AND. ITRLST(j+1, l).GT.0 )
     +                     THEN
                        k = j
                        DO WHILE ( k.LE.j+ITRLST(j+1, l)-1 )
                          fm = fm + (UZTHST(k, l)-thr)
     +                         *(UZDPST(k, l)-UZDPST(k+1, l))
                          k = k + 1
                        END DO
                        IF ( k.EQ.iset+NWAVST(ic, ir)-1 ) THEN
                          fm = fm + (UZTHST(k, l)-thr)*UZDPST(k, l)
                        ELSE IF ( iset+NWAVST(ic, ir)-1.GT.k+1 .AND. 
     +                            ITRLST(k+2, l).GT.0 .AND. 
     +                            LTRLST(k+1, l).EQ.1 ) THEN
                          fm = fm + (UZTHST(k, l)-thr)
     +                         *(UZDPST(k, l)-UZDPST(k+1, l))
                        ELSE
                          fm = fm + (UZTHST(k, l)-thr)
     +                         *(UZDPST(k, l)-UZDPST(k+1, l))
                        END IF
                        j = k
                      ELSE
                        fm = fm + (UZTHST(j, l)-thr)
     +                       *(UZDPST(j, l)-UZDPST(j+1, l))
                      END IF
                      j = j + 1
                    END DO
                    IF ( j.EQ.iset+NWAVST(ic, ir)-1 ) fm = fm + 
     +                   (UZTHST(iset+NWAVST(ic,ir)-1, l)-thr)
     +                   *UZDPST(iset+NWAVST(ic, ir)-1, l)
                    uzstorhold = 0.0D0
                    UZSTOR(ic, ir) = fm*cellarea
                    DELSTOR(ic, ir) = UZSTOR(ic, ir) - uzstorhold
                  ELSE
                    UZSTOR(ic, ir) = 0.0D0
                  END IF
                  UZFLWT(ic, ir) = totflux*cellarea
                  UZOLSFLX(ic, ir) = surflux
                ELSE
                  UZFLWT(ic, ir) = finfact*cellarea*DELT
                  UZOLSFLX(ic, ir) = finfact
                END IF
              END IF
C
C27-----CALCULATE ET DEMAND LEFT FOR GROUND WATER.
C
Cdep  Added lake flag to suppress ET beneath a lake
              IF ( IETFLG.GT.0 .AND. lakflg.NE.1 ) THEN
                etdp = celtop - ROOTDPTH(ic, ir)
                IF ( h.GT.etdp ) THEN
                  s = celtop
                  x = ROOTDPTH(ic, ir)
                  c = PETRATE(ic, ir) - etact/DELT
                  IF ( c.GT.0.0 ) THEN
                    c = c*cellarea
                  ELSE
                    c = 0.0
                  END IF
                  etgw = c*(h-(s-x))/x
                ELSE
                  etgw = 0.0
                END IF
                c = etgw
                IF ( etgw/cellarea+etact/DELT.GT.PETRATE(ic, ir)
     +             ) THEN
                  etgw = (PETRATE(ic, ir)-etact/DELT)*cellarea
                  c = etgw
                  IF ( c.lt.0.0 ) THEN
                    c = 0.0
                    etgw = 0.0
                  END IF
                END IF      
                UZFETOUT(ic, ir) = etact*cellarea + etgw*DELT
                GWET(ic, ir) = etgw
              END IF
              IF ( IUZFOPT.GT.0 )UZDPST(iset, l) = celtop - h
            END IF
C
C28-----COMPUTE UNSATURATED ERROR FOR EACH CELL.
            IF ( IUZFOPT.GT.0 ) THEN
              IF ( iss.EQ.0 ) THEN
                volet = etact*cellarea
                volinflt = finfact*cellarea*DELT
                volflwtb = UZFLWT(ic, ir)
              ELSE
                volet = 0.0
                volinflt = finfact*cellarea*DELT
                volflwtb = finfact*cellarea*DELT
              END IF
!      error = volinflt - volflwtb - DELSTOR(ic, ir)
!      if(kkper.eq.6.and.kkstp.gt.164)then
!      write(iout,222)l,il,htest1,htest2,volinflt/delt,volflwtb/delt,
!     +    DELSTOR(ic, ir)
!      end if
!  222 format(2(1x,i6),1x,5(1x,e20.10))
              UZTOTBAL(ic, ir, 1) = UZTOTBAL(ic, ir, 1) + volinflt
              UZTOTBAL(ic, ir, 2) = UZTOTBAL(ic, ir, 2)
     +                              + DELSTOR(ic, ir)
              UZTOTBAL(ic, ir, 3) = UZTOTBAL(ic, ir, 3) + volflwtb
              UZTOTBAL(ic, ir, 4) = UZTOTBAL(ic, ir, 4) + volet
              UZTOTBAL(ic, ir, 7) = UZTOTBAL(ic, ir, 7) + volinflt +
     +                              Excespp(ic, ir) + rej_inf(ic, ir)
              fminn = MAX(ABS(UZTOTBAL(ic,ir,1)), ABS(UZTOTBAL(ic,ir,2))
     +                , ABS(UZTOTBAL(ic,ir,3)))
              IF ( fminn.LE.1.0E-9 ) THEN
                prcntdif = 0.0D0
              ELSE IF ( ABS(UZTOTBAL(ic,ir,1)-UZTOTBAL(ic,ir,3))
     +                  .LT.CLOSEZERO .AND. ABS(UZTOTBAL(ic,ir,1))
     +                  .GT.CLOSEZERO ) THEN
                prcntdif = 100.0D0*UZTOTBAL(ic,ir,2)/UZTOTBAL(ic, ir, 1)
              ELSE IF ( ABS(UZTOTBAL(ic,ir,1)-UZTOTBAL(ic,ir,3))
     +                  .LT.CLOSEZERO .AND. ABS(UZTOTBAL(ic,ir,1))
     +                  .GT.CLOSEZERO ) THEN
                prcntdif = 100.0D0*UZTOTBAL(ic, ir, 2)
              ELSE
                prcntdif = 100.0D0*(UZTOTBAL(ic, ir, 1)
     +                     -UZTOTBAL(ic, ir, 3)-UZTOTBAL(ic, ir, 2))
     +                     /(MAX(ABS(UZTOTBAL(ic,ir,1)), 
     +                     ABS(UZTOTBAL(ic,ir,2)), 
     +                     ABS(UZTOTBAL(ic,ir,3))))
              END IF
              UZTOTBAL(ic, ir, 5) = prcntdif
C
C29-----ACCUMULATE INFLOW AND OUTFLOW VOLUMES FROM CELLS.
              CUMUZVOL(1) = CUMUZVOL(1) + volinflt
              CUMUZVOL(2) = CUMUZVOL(2) + volet
              CUMUZVOL(3) = CUMUZVOL(3) + volflwtb
              CUMUZVOL(4) = CUMUZVOL(4) + DELSTOR(ic, ir)
              totbet = totbet + (UZFETOUT(ic, ir)/DELT-volet/DELT)
              cumapplinf = cumapplinf + cellarea*FINF(ic, ir) + 
     1                     EXCESPP(ic, ir)
              UZTSRAT(1) = UZTSRAT(1) + volinflt/DELT
              UZTSRAT(2) = UZTSRAT(2) + volet/DELT
              UZTSRAT(3) = UZTSRAT(3) + volflwtb/DELT
              UZTSRAT(4) = UZTSRAT(4) + DELSTOR(ic, ir)/DELT
              UZTSRAT(6) = UZTSRAT(6) + UZSTOR(ic, ir)/DELT
  
C
C30-----NO UNSATURATED ZONE AND GROUND WATER DISCHARGES TO SURFACE.
              UZTOTBAL(ic, ir, 6) = UZTOTBAL(ic, ir, 6)
     +                              + (SEEPOUT(ic, ir))*DELT
              CUMUZVOL(5) = CUMUZVOL(5) + (SEEPOUT(ic, ir))*DELT
              UZTSRAT(5) = UZTSRAT(5) + (SEEPOUT(ic, ir))
            END IF
          END IF
        END IF
        ratout = ratout + c
        IF ( IUZFOPT.GT.0 .AND. IUZFBND(ic,ir).GT.0 ) THEN
          IF ( iss.EQ.0 ) THEN
            ratin = ratin + UZFLWT(ic, ir)/DELT
          ELSE
            ratin = ratin + finfact*cellarea
          END IF
        ELSE
          ratin = ratin + finfhold*cellarea
        END IF
        ratout2 = ratout2 + (SEEPOUT(ic, ir))
      END DO
C
C31-----UPDATE RATES AND BUFFERS WITH ET FOR UZF OR MODFLOW BUDGET ITEMS.
C
      IF ( IETFLG.GT.0 ) THEN
        IF ( ibduzf.GE.1 ) THEN
          DO il = 1, NLAY
            DO ir = 1, NROW
              DO ic = 1, NCOL
                BUFF(ic, ir, il) = 0.0
                IF ( laynum(ic, ir).GT.0 
     +               .AND. IUZFBND(ic,ir).GT.0 ) THEN
                  ill = laynum(ic, ir)
                  IF ( IUZFB22.LT.0 ) THEN
                    BUFF(ic, ir, ill)= -UZFETOUT(ic, ir)/DELT
                  ELSE
                    BUFF(ic, ir, ill) = -GWET(ic, ir)
                  END IF
                END IF
              END DO
            END DO
          END DO
          IF ( IUZFB22.LT.0 ) THEN
            txthold = uzettext
          ELSE
            txthold = textet
          END IF
C
C32-----SAVE ET RATES TO UNFORMATTED FILE FOR UZF OR MODFLOW BUDGET ITEMS.
          CALL UBDSV3(Kkstp, Kkper, txthold,  
     +                IUZFCB2,BUFF, laynum, NUZTOP,
     +                NCOL, NROW,NLAY, IOUT, DELT,  
     +                PERTIM, TOTIM, IBOUND)
        END IF
      END IF
C
C33-----UPDATE RATES AND BUFFERS WITH GW ET FOR MODFLOW BUDGET ITEMS.
      IF ( IETFLG.GT.0 ) THEN
        IF ( ibd.GE.1 ) THEN
          DO il = 1, NLAY
            DO ir = 1, NROW
              DO ic = 1, NCOL
                BUFF(ic, ir, il) = 0.0
                IF ( laynum(ic, ir).GT.0 .AND. 
     +               IUZFBND(ic,ir).GT.0 ) THEN
                  ill = laynum(ic, ir)
                  BUFF(ic, ir, ill) = -GWET(ic, ir)
                END IF
              END DO
            END DO
          END DO
C
C34-----SAVE ET RATES TO UNFORMATTED FILE FOR MODFLOW BUDGET ITEMS.
          CALL UBUDSV(Kkstp, Kkper, textet, IUZFCB1, BUFF,
     +             NCOL, NROW, NLAY, IOUT)
        END IF
      END IF
      
C
C35-----UPDATE RATES AND BUFFERS FOR INFILTRATION.
        IF ( ibd.GE.1 .OR. ibduzf.GE.1 ) THEN
          DO il = 1, NLAY
            DO ir = 1, NROW
              DO ic = 1, NCOL
                BUFF(ic, ir, il) = 0.0
                IF ( laynum(ic, ir).GT.0 
     +               .AND. IUZFBND(ic,ir).GT.0 ) THEN
                  ill = laynum(ic, ir)
                  BUFF(ic, ir, ill)= UZOLSFLX(ic, ir)*DELC(ir)*DELR(ic)
                END IF
              END DO
            END DO
          END DO  
C   
C37-----SAVE INFILTRATION RATES TO UNFORMATTED FILE.
      IF ( ibd.GE.1 ) CALL UBUDSV(Kkstp, Kkper, textinf, IUZFCB1, BUFF, 
     +                            NCOL, NROW, NLAY, IOUT)
      IF ( ibduzf.GE.1 ) CALL UBDSV3(Kkstp, Kkper, textinf,  
     +                               IUZFCB2, BUFF, laynum, NUZTOP,
     +                               NCOL, NROW,NLAY, IOUT, DELT,  
     +                               PERTIM, TOTIM, IBOUND)
      END IF
C
C38-----UPDATE RATES AND BUFFERS FOR RECHARGE.
      IF ( ibd.GE.1 .OR. ibduzf.GE.1 ) THEN
        DO il = 1, NLAY
          DO ir = 1, NROW
            DO ic = 1, NCOL
              BUFF(ic, ir, il) = 0.0
              IF ( laynum(ic, ir).GT.0 
     +               .AND. IUZFBND(ic,ir).GT.0 ) THEN
                ill = laynum(ic, ir)
                IF ( IUZFOPT.GT.0 ) THEN
                  BUFF(ic, ir, ill) = UZFLWT(ic, ir)/DELT
                ELSE
                  BUFF(ic, ir, ill) = UZOLSFLX(ic, ir)*DELC(ir)*DELR(ic)
                END IF
              END IF
            END DO
          END DO
        END DO
      END IF
C
C39-----SAVE RECHARGE RATES TO UNFORMATTED FILE.
      IF ( ibd.GE.1 ) CALL UBUDSV(Kkstp, Kkper, textrch, IUZFCB1, BUFF, 
     +                            NCOL, NROW, NLAY, IOUT)
      IF ( ibduzf.GE.1 ) CALL UBDSV3(Kkstp, Kkper, textrch,  
     +                               IUZFCB2, BUFF, laynum, NUZTOP,
     +                               NCOL, NROW,NLAY, IOUT, DELT,  
     +                               PERTIM, TOTIM, IBOUND)
C
C40-----UPDATE RATES AND BUFFERS FOR SURFACE LEAKAGE RATES.
      IF ( ibd.GE.1 .OR. ibduzf.GE.1 ) THEN
        DO il = 1, NLAY
          DO ir = 1, NROW
            DO ic = 1, NCOL
              BUFF(ic, ir, il) = 0.0
              IF ( laynum(ic, ir).GT.0
     +               .AND. IUZFBND(ic,ir).GT.0 ) THEN
                ill = laynum(ic, ir)
                BUFF(ic, ir, ill) = -SEEPOUT(ic, ir)
              END IF
            END DO
          END DO
        END DO
      END IF
C41-----SAVE SURFACE LEAKAGE RATES TO UNFORMATTED FILE.
      IF ( ibd.GE.1 ) CALL UBUDSV(Kkstp, Kkper, textexfl, IUZFCB1, BUFF,
     +                            NCOL, NROW, NLAY, IOUT)
      IF ( ibduzf.GE.1 ) CALL UBDSV3(Kkstp, Kkper, textexfl,  
     +                               IUZFCB2, BUFF, laynum, NUZTOP,
     +                               NCOL, NROW, NLAY, IOUT, DELT,  
     +                               PERTIM, TOTIM, IBOUND)
C
C42-----PRINT RESULTS.
      bigvl1 = 9.99999E11
      bigvl2 = 9.99999E10
      small = 0.1D0
C
C43------MOVE TOTAL RECHARGE RATE INTO VBVL FOR PRINTING BY BAS6OT.
      rout = zero
      rin = ratin
      VBVL(4, MSUM) = rout
      VBVL(3, MSUM) = rin
C
C44------ADD RECHARGE FOR TIME STEP TO RECHARGE ACCUMULATOR IN VBVL.
      VBVL(2, MSUM) = VBVL(2, MSUM) + rout*DELT
      VBVL(1, MSUM) = VBVL(1, MSUM) + rin*DELT
C
C45-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS6OT.
      VBNM(MSUM) = textrch
C
C46-----INCREMENT BUDGET TERM COUNTER.
      MSUM = MSUM + 1
C
C47------MOVE TOTAL ET RATE INTO VBVL FOR PRINTING BY BAS6OT.
      rout = ratout
      rin = zero
      VBVL(4, MSUM) = rout
      VBVL(3, MSUM) = rin
C
C48------ADD ET FOR TIME STEP TO RECHARGE ACCUMULATOR IN VBVL.
      VBVL(2, MSUM) = VBVL(2, MSUM) + rout*DELT
      VBVL(1, MSUM) = VBVL(1, MSUM) + rin*DELT
C
C49-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS6OT.
      VBNM(MSUM) = textet
C
C50-----INCREMENT BUDGET TERM COUNTER.
      MSUM = MSUM + 1
C
C51------MOVE TOTAL SURFACE LEAKAGE RATE INTO VBVL FOR PRINTING 
C          BY BAS6OT.
      rout = ratout2
      rin = zero
      VBVL(4, MSUM) = rout
      VBVL(3, MSUM) = rin
C
C52------ADD SURFACE LEAKAGE FOR TIME STEP TO RECHARGE ACCUMULATOR 
C          IN VBVL.
      VBVL(2, MSUM) = VBVL(2, MSUM) + rout*DELT
      VBVL(1, MSUM) = VBVL(1, MSUM) + rin*DELT
C
C53-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS6OT.
      VBNM(MSUM) = textexfl
C
C54-----INCREMENT BUDGET TERM COUNTER.
      MSUM = MSUM + 1
C
C55-----UNSATURATED ZONE INFORMATION TO SEPARATE FILE WHEN REQUESTED
C        (IUZFCB2>0).
      prcntercum = 0.0
      prcnterrat = 0.0
      IF ( IUZFOPT.GT.0 ) THEN
        IF ( IBUDFL.GT.0 ) THEN
C
C56-----WRITE BUDGET INFORMATION FOR UNSATURATED ZONE TO MAIN LIST FILE.
          WRITE (IOUT, 9002) Kkstp, Kkper
          WRITE (IOUT, 9003)
C
C57-----PRINT INFLOW AND OUTFLOW VOLUMES AND RATES.
          IF ( ABS(CUMUZVOL(1)).LT.NEARZERO .AND. 
     +         (ABS(CUMUZVOL(1)).GE.bigvl1 .OR. ABS(CUMUZVOL(1))
     +         .LT.small) ) THEN
            WRITE (val1, '(1PE17.4)') CUMUZVOL(1)
          ELSE
            WRITE (val1, '(F17.4)') CUMUZVOL(1)
          END IF
          IF ( ABS(UZTSRAT(1)).LT.NEARZERO .AND. 
     +         (ABS(UZTSRAT(1)).GE.bigvl1 .OR. ABS(UZTSRAT(1)).LT.small)
     +         ) THEN
            WRITE (val2, '(1PE17.4)') UZTSRAT(1)
          ELSE
            WRITE (val2, '(F17.4)') UZTSRAT(1)
          END IF
          WRITE (IOUT, 9004) uzinftxt, val1, uzinftxt, val2
          WRITE (IOUT, 9005)
          IF ( ABS(CUMUZVOL(2)).LT.NEARZERO .AND. 
     +         (ABS(CUMUZVOL(2)).GE.bigvl1 .OR. ABS(CUMUZVOL(2))
     +         .LT.small) ) THEN
            WRITE (val1, '(1PE17.4)') CUMUZVOL(2)
          ELSE
            WRITE (val1, '(F17.4)') CUMUZVOL(2)
          END IF
          IF ( ABS(UZTSRAT(2)).LT.NEARZERO .AND. 
     +         (ABS(UZTSRAT(2)).GE.bigvl1 .OR. ABS(UZTSRAT(2)).LT.small)
     +         ) THEN
            WRITE (val2, '(1PE17.4)') UZTSRAT(2)
          ELSE
            WRITE (val2, '(F17.4)') UZTSRAT(2)
          END IF
          WRITE (IOUT, 9004) uzettext, val1, uzettext, val2
          IF ( ABS(CUMUZVOL(3)).LT.NEARZERO .AND. 
     +         (ABS(CUMUZVOL(3)).GE.bigvl1 .OR. ABS(CUMUZVOL(3))
     +         .LT.small) ) THEN
            WRITE (val1, '(1PE17.4)') CUMUZVOL(3)
          ELSE
            WRITE (val1, '(F17.4)') CUMUZVOL(3)
          END IF
          IF ( ABS(UZTSRAT(3)).LT.NEARZERO .AND. 
     +         (ABS(UZTSRAT(3)).GE.bigvl1 .OR. ABS(UZTSRAT(3)).LT.small)
     +         ) THEN
            WRITE (val2, '(1PE17.4)') UZTSRAT(3)
          ELSE
            WRITE (val2, '(F17.4)') UZTSRAT(3)
          END IF
          WRITE (IOUT, 9004) textrch, val1, textrch, val2
C
C58----CALCULATE DIFFERENCE AND ERROR BETWEEN INFLOW AND OUTFLOW.
          IF ( CUMUZVOL(4).GT.0.0 ) THEN
            totvin = CUMUZVOL(1)
            totvot = CUMUZVOL(2) + CUMUZVOL(3) + CUMUZVOL(4)
          ELSE
            totvin = CUMUZVOL(1) - CUMUZVOL(4)
            totvot = CUMUZVOL(2) + CUMUZVOL(3)
          END IF
          IF ( UZTSRAT(4).GT.0.0 ) THEN
            totrin = UZTSRAT(1)
            totrot = UZTSRAT(2) + UZTSRAT(3) + UZTSRAT(4)
          ELSE
            totrin = UZTSRAT(1) - UZTSRAT(4)
            totrot = UZTSRAT(2) + UZTSRAT(3)
          END IF
          IF ( ABS(totrin+totrot).GT.CLOSEZERO ) THEN
            fmax = totrin
            IF ( fmax.LT.totrot ) fmax = totrot
            prcnterrat = 100.*(totrin-totrot)/(fmax)
          ELSE
            prcnterrat = 0.0
          END IF
          IF ( ABS(totvin+totvot).GT.CLOSEZERO ) THEN
            prcntercum = 100.*(totvin-totvot)/(totvin+totvot)/2.0
          ELSE
            prcntercum = 0.0
          END IF
          cumdiff = CUMUZVOL(1) - (CUMUZVOL(2)+CUMUZVOL(3))
          ratedif = UZTSRAT(1) - (UZTSRAT(2)+UZTSRAT(3))
          acumdif = ABS(cumdiff)
          aratdif = ABS(ratedif)
          IF ( acumdif.LT.NEARZERO .AND. 
     +         (acumdif.GE.bigvl2 .OR. acumdif.LT.small) ) THEN
            WRITE (val1, '(1PE17.4)') cumdiff
          ELSE
            WRITE (val1, '(F17.4)') cumdiff
          END IF
          IF ( aratdif.LT.NEARZERO .AND. 
     +         (aratdif.GE.bigvl2 .OR. aratdif.LT.small) ) THEN
            WRITE (val2, '(1PE17.4)') ratedif
          ELSE
            WRITE (val2, '(F17.4)') ratedif
          END IF
          WRITE (IOUT, 9006) val1, val2
          WRITE (IOUT, 9007)
          unsatvol = ABS(CUMUZVOL(4))
          unsatrat = ABS(UZTSRAT(4))
          IF ( unsatvol.LT.NEARZERO .AND. 
     +         (unsatvol.GE.bigvl1 .OR. unsatvol.LT.small) ) THEN
            WRITE (val1, '(1PE17.4)') CUMUZVOL(4)
          ELSE
            WRITE (val1, '(F17.4)') CUMUZVOL(4)
          END IF
          IF ( unsatrat.LT.NEARZERO .AND. 
     +         (unsatrat.GE.bigvl1 .OR. unsatrat.LT.small) ) THEN
            WRITE (val2, '(1PE17.4)') UZTSRAT(4)
          ELSE
            WRITE (val2, '(F17.4)') UZTSRAT(4)
          END IF
          WRITE (IOUT, 9004) uzsttext, val1, uzsttext, val2
          WRITE (IOUT, 9008)
          WRITE (IOUT, 9009) prcntercum, prcnterrat

C
C59----PRINT TIME SERIES OF UZ FLOW AND STORAGE OR MOISTURE CONTENT
C        PROFILES FOR SELECTED CELLS.
        END IF
        IF ( NUZGAG.GT.0 ) THEN
C
C60----LOOP OVER GAGING STATIONS.
          iog = 1
          DO WHILE ( iog.LE.NUZGAG )
            iuzrow = IUZLIST(1, iog)
            iuzcol = IUZLIST(2, iog)
            iftunit =IUZLIST(3, iog)
            iuzopt = IUZLIST(4, iog)
            IF ( iuzopt .LT. 4 ) THEN
              il = IUZFBND(iuzcol, iuzrow)
            ELSE
              il = 0
            END IF
            IF ( il.GT.0 ) THEN
              ghnw = HNEW(iuzcol, iuzrow, il)
              celtop = BOTM(iuzcol, iuzrow, 0) - 0.5 * SURFDEP
              ghdif = celtop - ghnw
              gcumapl = UZTOTBAL(iuzcol, iuzrow, 7)
              gcumin = UZTOTBAL(iuzcol, iuzrow, 1)
              gcumrch = UZTOTBAL(iuzcol, iuzrow, 3)
              guzstore = UZSTOR(iuzcol, iuzrow)
              gdelstor = UZTOTBAL(iuzcol, iuzrow, 2)
C ginfltr should be changed to include water routed to conduits.
              ginfltr = UZOLSFLX(iuzcol, iuzrow)*
     +                  (DELC(iuzrow)*DELR(iuzcol))
              gaplinfltr = ginfltr + Excespp(iuzcol, iuzrow) + 
     +                     rej_inf(iuzcol, iuzrow) 
              grchr = UZFLWT(iuzcol, iuzrow)/DELT
              gdlstr = DELSTOR(iuzcol, iuzrow)/DELT
              gseep = UZTOTBAL(iuzcol, iuzrow, 6)
              gseepr = SEEPOUT(iuzcol, iuzrow)
            END IF
              IF ( iftunit.GT.0 ) THEN
C
C61-----GET OUTTYPE FOR SPECIFIED UNSATURATED ZONE CELL.
                SELECT CASE (iuzopt)
C
C62-----CASE 1: WRITE VOLUMES IN, OUT, AND IN STORAGE FOR A CELL.
                CASE (1)
                WRITE (iftunit, 9011) il, TOTIM, ghnw, ghdif, gcumapl, 
     +                       gcumin, gcumrch, guzstore, gdelstor, gseep
C
C63-----CASE 2: WRITE VOLUMES AND RATES FOR CELL.
                CASE (2)
                  WRITE (iftunit, 9012) il, TOTIM, ghnw, ghdif, gcumapl,
     +                                  gcumin, gcumrch, guzstore, 
     +                                  gdelstor, gseep, gaplinfltr, 
     +                                  ginfltr, grchr, gdlstr, gseepr
C
C64-----CASE 3: WRITE UNSATURATED-ZONE MOISTURE PROFILES.
                CASE (3)
C
C65-----TOTAL WATER CONTENT OVER SPECIFIED DEPTH.
                  IF ( iftunit.NE.0 ) THEN
                    igflg = 1
                    l = 1
                    DO WHILE ( l.LE.IUZM .AND. igflg.EQ.1 )
                      nuzr = IUZHOLD(1, l)
                      nuzc = IUZHOLD(2, l)
                      IF ( nuzr.EQ.iuzrow .AND. nuzc.EQ.iuzcol .AND. 
     +                     ghdif.GT.0.0 ) THEN
                        depthinc = ghdif/40.001D0
                        depthsave = depthinc
                        totalwc = 0.0
                        igflg = 0
                        kknt = 0
                        PROFILE: DO WHILE (depthsave-ghdif.LT.CLOSEZERO)
                          IF ( depthsave.LT.CLOSEZERO )EXIT PROFILE
                          kknt = kknt + 1
                          iset = 1
                          fm = 0.0D0
                          jj = iset
                          jk = iset + NWAVST(iuzcol, iuzrow) - 1
                          nwavm1 = jk
                          DO WHILE ( jk.GE.iset )
                            IF ( UZDPST(jk, l).LT.depthsave ) jj = jk
                            jk = jk - 1
                          END DO
                          IF ( jj.GT.iset ) THEN
                            fm = fm + UZTHST(jj-1, l)
     +                           *(depthsave-UZDPST(jj, l))
                            DO j = jj, nwavm1 - 1
                              fm = fm + UZTHST(j, l)
     +                             *(UZDPST(j, l)-UZDPST(j+1, l))
                            END DO
                            fm = fm + UZTHST(nwavm1, l)
     +                           *UZDPST(nwavm1, l)
                          ELSE
                            fm = fm + UZTHST(nwavm1, l)*depthsave
                          END IF
                          avdpt = depthsave
                          IF ( avdpt.GE.ghdif-depthinc ) THEN
                            avwat = UZTHST(1, l)
                            avdpt = ghdif
                          ELSE
                            avwat = (fm-totalwc)/depthinc
                          END IF
                          totalwc = fm
                          depthsave = depthsave + depthinc
                          IF ( kknt.EQ.1 ) THEN
                            WRITE ( iftunit, 9013 ) il, TOTIM, ghnw, 
     +                             ghdif, avdpt, avwat
                          ELSE
                            WRITE (iftunit, 9014) avdpt, avwat
                          END IF
                        END DO PROFILE
                      END IF
                      l = l + 1
                    END DO
                  END IF
C
C66-----CASE 4: WRITE TOTAL INFILTRATION, RUNOFF, ET AND RATES FOR
C                 ALL ACTIVE CELLS.
                  CASE (4)
                  WRITE (iftunit, 9016) TOTIM, cumapplinf, TOTRUNOFF, 
     1                   UZTSRAT(1), UZTSRAT(5), UZTSRAT(2), totbet, 
     2                   UZTSRAT(4), UZTSRAT(3)                 
                END SELECT
              END IF
            iog = iog + 1
          END DO
        END IF
      END IF
C
      IF ( IBUDFL.GT.0 ) WRITE (IOUT, 9015)
C67-----FORMATS.
 9002 FORMAT (1X//,'UNSATURATED ZONE PACKAGE VOLUMETRIC BUDGET FOR ', 
     +        ' TIME STEP ', I4, ' STRESS PERIOD ', I4, /2X, 78('-')//)
 9003 FORMAT (1X, /5X, 'CUMULATIVE VOLUMES', 6X, 'L**3', 7X, 
     +        'RATES FOR THIS TIME STEP', 6X, 'L**3/T'/5X, 18('-'), 17X,
     +        24('-')//11X, 'IN:', 38X, 'IN:'/11X, '---', 38X, '---')
 9004 FORMAT (1X, 3X, A16, ' =', A17, 6X, A16, ' =', A17)
 9005 FORMAT (1X, /10X, 'OUT:', 37X, 'OUT:'/10X, 4('-'), 37X, 4('-'))
 9006 FORMAT (1X, /12X, 'IN - OUT =', A, 14X, 'IN - OUT =', A)
 9007 FORMAT (1X, /10X, 'STORAGE:', 33X, 'STORAGE:'/10X, 8('-'), 33X, 
     +        8('-'))
 9008 FORMAT (1X, /1X, 
     +        'PERCENT DISCREPANCY IS DIFFERENCE BETWEEN IN-OUT', 
     +        ' MINUS CHANGE IN STORAGE', 1X, /1X, 
     +        'DIVIDED BY THE AVERAGE OF IN', ' AND OUT TIMES 100')
 9009 FORMAT (1X, /1X, 'PERCENT DISCREPANCY =', F15.2, 5X, 
     +        'PERCENT DISCREPANCY =', F15.2, ///)
 9011 FORMAT (9X, I5, 3X, 9(1PE14.7, 1X))
 9012 FORMAT (9X, I5, 3X, 14(1PE14.7, 1X))
 9013 FORMAT (9X, I5, 3X, 5(1PE14.7, 1X))
 9014 FORMAT (62X, 2(1PE14.7, 1X))
 9015 FORMAT (//)
 9016 FORMAT (9X, 1PE14.7, 1X, 9(2X,1PE14.7))
C
C68-----RELEASE MEMORY AND RETURN.
      DEALLOCATE (laynum)
      RETURN
      END SUBROUTINE GWF2UZF1BD
C
C-------SUBROUTINE UZFLOW2
      SUBROUTINE UZFLOW2(I, Surflux, Dlength, Zoldist, Depth, Theta, 
     +                   Flux, Speed, Itrwave, Ltrail, Totalflux, 
     +                   Numwaves, Thetar, Thetas, Fksat, Eps, Oldsflx, 
     +                   Jpnt, Rateud, Etout, Wiltwc, Rootdepth, DELT)
C     ******************************************************************
C     COMPUTE WAVE INTERACTION WITHIN AN UNSATURATED FLOW CELL
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE, ONLY: NWAV, THETAB, FLUXB, IETFLG, NEARZERO,
     +                        ZEROD6, ZEROD7
      USE GLOBAL,       ONLY: ITMUNI, LENUNI, IOUT
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER I, Jpnt, Numwaves, Itrwave(NWAV), Ltrail(NWAV)
      REAL Eps, Fksat, Rootdepth, Thetas, Wiltwc
      DOUBLE PRECISION Depth(NWAV), Theta(NWAV), Flux(NWAV), Speed(NWAV)
      DOUBLE PRECISION Dlength, Zoldist, Totalflux, Surflux, Oldsflx, 
     +                 Rateud, Etout, Thetar, DELT
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION ffcheck, feps2, feps, time, fm, dlength2, 
     +                 factor1, factor2
      REAL thetadif
      INTEGER itester, j, jj, jm1, itrailflg, numwavesd
C     ------------------------------------------------------------------
      time = 0.0D0
      Totalflux = 0.0D0
      factor1 = 1.0D0
      factor2 = 1.0D0
      feps = ZEROD7
      feps2 = ZEROD7
      IF ( ITMUNI.EQ.1 ) THEN
        factor1 = 1.0D0/86400.0D0
      ELSE IF ( ITMUNI.EQ.2 ) THEN
        factor1 = 1.0D0/1440.0D0
      ELSE IF ( ITMUNI.EQ.3 ) THEN
        factor1 = 1.0D0/24.0D0 
      ELSE IF ( ITMUNI.EQ.5 ) THEN
        factor1 = 365.0D0
      END IF    
      IF ( LENUNI.EQ.1 ) THEN
        factor2 = 1.0D0/0.3048
      ELSE IF ( LENUNI.EQ.3 ) THEN
        factor2 = 100.0D0
      END IF 
      feps = feps*factor1*factor2
      feps2 = feps2*factor1*factor2
      Etout = 0.0D0
      itrailflg = 0
      fm = 0.0D0
      Oldsflx = Flux(Jpnt+Numwaves-1)
C1------DETERMINE IF WATER TABLE IS RISING OR FALLING.
      IF ( (Dlength-Zoldist).LT.-feps ) THEN
        dlength2 = Dlength
        Dlength = Zoldist
      ELSE IF ( (Dlength-Zoldist).GT.feps ) THEN
        dlength2 = Zoldist + 1.0D0
        thetadif = ABS(Theta(Jpnt)-Thetar)
        IF ( thetadif.GT.1.0E-6 ) THEN
          DO j = Jpnt + Numwaves, Jpnt + 1, -1
            jm1 = j - 1
            Theta(j) = Theta(jm1)
            Flux(j) = Flux(jm1)
            Speed(j) = Speed(jm1)
            Depth(j) = Depth(jm1)
            Itrwave(j) = Itrwave(jm1)
            Ltrail(j) = Ltrail(jm1)
          END DO
          IF ( Theta(Jpnt+1).GT.Thetar ) THEN
            Speed(Jpnt+1) = Flux(Jpnt+1)/(Theta(Jpnt+1)-Thetar)
          ELSE
            Speed(Jpnt+1) = 0.0D0
          END IF
          Theta(Jpnt) = Thetar
          Flux(Jpnt) = 0.0D0
          Speed(Jpnt) = 0.0D0
          Depth(Jpnt) = Dlength
          Ltrail(Jpnt) = 0
          Numwaves = Numwaves + 1
          IF ( Numwaves.GE.NWAV ) THEN
            WRITE (*, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves, 
     +                   '   PROGRAM TERMINATED IN UZFLOW-1'
            WRITE (IOUT, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves,
     +              '   PROGRAM TERMINATED IN UZFLOW-1; INCREASE NSETS2'
            STOP
          END IF
        ELSE
          Depth(Jpnt) = Dlength
        END IF
      ELSE
        dlength2 = Zoldist + 1.0D0
      END IF
      fm = 0.0D0
      THETAB = Theta(Jpnt)
      FLUXB = Flux(Jpnt)
      Totalflux = 0.00D0
      itester = 0
      ffcheck = (Surflux-Flux(Jpnt+Numwaves-1))
C
C2------CREATE A NEW WAVE WHEN SURFACE FLUX CHANGES.
C         CALL TRAILWAVE2 IF SURFACE FLUX DECREASES.
C         CALL LEADWAVE2 IF SURFACE FLUX INCREASES.
      IF ( ffcheck.GT.feps2 .OR. ffcheck.LT.-feps2 ) THEN
        Numwaves = Numwaves + 1
        IF ( Numwaves.GE.NWAV ) THEN
          WRITE (*, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves, 
     +                 '   PROGRAM TERMINATED IN UZFLOW-2'
          WRITE (IOUT, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves,
     +              '   PROGRAM TERMINATED IN UZFLOW-2; INCREASE NSETS2'
          STOP
        END IF
      ELSE IF ( Numwaves.EQ.1 ) THEN
        itester = 1
      END IF
C
      IF ( Numwaves.GT.1 ) THEN
        IF ( ffcheck.LT.-feps2 ) THEN
          CALL TRAILWAVE2(Numwaves, I, Flux, Theta, Speed, Depth, 
     +                    Itrwave, Ltrail, Fksat, Eps, Thetas, Thetar, 
     +                    Surflux, Jpnt )
          itrailflg = 1
        END IF
        CALL LEADWAVE2(Numwaves, time, Totalflux, itester, Flux, 
     +                 Theta, Speed, Depth, Itrwave, Ltrail, Fksat, 
     +                 Eps, Thetas, Thetar, Surflux, Oldsflx, Jpnt, 
     +                 feps2, itrailflg, DELT)
      END IF
      IF ( itester.EQ.1 ) THEN
        Totalflux = Totalflux + (DELT-time)*Flux(Jpnt)
        time = 0.0D0
        itester = 0
      END IF
C
C3------CALCULATE VOLUME OF WATER BELOW WATER TABLE.
      IF ( dlength2.LT.Zoldist ) THEN
        j = 2
        jj = 1
        IF ( Depth(Jpnt+1).GT.dlength2 ) THEN
          DO WHILE ( j.LE.Numwaves )
            IF ( Depth(Jpnt+j-1).GE.dlength2 ) jj = j
            IF ( j.EQ.jj .AND. Depth(Jpnt+j).LT.dlength2 ) j = Numwaves
            j = j + 1
          END DO
        END IF
        IF ( jj.GT.1 .AND. Numwaves.GT.1 ) THEN
          fm = (Depth(Jpnt)-Depth(Jpnt+1))*(Theta(Jpnt)-Thetar)
          DO j = 2, jj - 1
            fm = fm + (Depth(Jpnt+j-1)-Depth(Jpnt+j))*(Theta(Jpnt+j-1)-
     +           Thetar)
          END DO
          fm = fm+(Theta(Jpnt+jj-1)-Thetar)*(Depth(Jpnt+jj-1)-dlength2)
        ELSE
          fm = (Depth(Jpnt)-dlength2)*(Theta(Jpnt)-Thetar)
        END IF
        Dlength = dlength2
        Totalflux = Totalflux + fm
        IF ( Totalflux.LT.NEARZERO ) Totalflux = 0.0D0
      END IF
C
C4------CALCULATE UNSATURATED ZONE ET.
      IF ( IETFLG.NE.0 ) THEN
        numwavesd = Numwaves
        CALL TRANSPIRATION(Numwaves, Flux, Theta, Speed, Depth, 
     +                    Itrwave, Ltrail, Fksat, Eps, Thetas, Thetar, 
     +                    Jpnt, DELT, Rateud, Etout, Wiltwc, 
     +                    Rootdepth, numwavesd) 
      END IF
C5-----RETURN.
      RETURN
      END SUBROUTINE UZFLOW2

C
C--------SUBROUTINE LEADWAVE2
      SUBROUTINE LEADWAVE2(Numwaves, Time, Totalflux, Itester, Flux, 
     +                     Theta, Speed, Depth, Itrwave, Ltrail, Fksat, 
     +                     Eps, Thetas, Thetar, Surflux, Oldsflx, Jpnt, 
     +                     Feps2, Itrailflg, DELT)
C     ******************************************************************
C     CREATE LEAD WAVE WHEN THE SURFACE FLUX INCREASES AND ROUTE WAVES.
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      USE GWFUZFMODULE, ONLY: NWAV, CLOSEZERO, NEARZERO, THETAB, FLUXB,
     +                        FLUXHLD2, ZEROD15, ZEROD9
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Itester, Jpnt, Numwaves, Itrailflg
      INTEGER Itrwave(NWAV), Ltrail(NWAV)
      REAL Eps, Fksat, Thetas
      DOUBLE PRECISION Depth(NWAV), Theta(NWAV), Flux(NWAV), Speed(NWAV)
      DOUBLE PRECISION Feps2, Totalflux, Surflux, Oldsflx, Thetar, Time,
     +                 DELT
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION ffcheck, bottomtime, shortest, fcheck, fhold
      DOUBLE PRECISION eps_m1, checktime(NWAV), timenew, feps3
      REAL big, comp1, comp2, diff, f7, f8, ftheta1, ftheta2
Crsr  REAL timedif
      INTEGER idif, iflag, iflag2, iflx, iremove, itrwaveb, j, jj, k, 
     +        kk, l, jpnwavesm1, jpntpkm1, jpntpkm2, more(NWAV)
C     ------------------------------------------------------------------
      feps3 = Feps2/2.0D0
      eps_m1 = DBLE(Eps) - 1.0D0
      f7 = 0.495
      f8 = 1.0 - f7
      big = 1.0E30
C
C1------INITIALIZE NEWEST WAVE.
      IF ( Itrailflg.EQ.0 ) THEN
        jpnwavesm1 = Jpnt + Numwaves - 1
        ffcheck = Surflux - Oldsflx
        IF ( ffcheck.GT.Feps2 ) THEN
          Flux(jpnwavesm1) = Surflux
          IF ( Flux(jpnwavesm1).LT.NEARZERO ) Flux(jpnwavesm1) = 0.0D0
          Theta(jpnwavesm1) = (((Flux(jpnwavesm1)/Fksat)**(1.0/Eps))*
     +                        (Thetas-Thetar)) + Thetar
          IF ( Theta(jpnwavesm1)-Theta(jpnwavesm1-1).GT.feps3 ) THEN
            Speed(jpnwavesm1) = (Flux(jpnwavesm1)-Flux(jpnwavesm1-1))/
     +                          (Theta(jpnwavesm1)-Theta(jpnwavesm1-1))
          ELSE
            Speed(jpnwavesm1) = 0.0D0
          END IF
          Depth(jpnwavesm1) = 0.0D0
          Ltrail(jpnwavesm1) = 0
          Itrwave(jpnwavesm1) = 0
        END IF
      END IF
C
C2------ROUTE ALL WAVES AND INTERCEPTION OF WAVES OVER TIME STEP.

      diff = 1.0
      iflx = 0
      FLUXHLD2 = Flux(Jpnt)
      IF ( Numwaves.EQ.0 ) Itester = 1
      DO WHILE ( diff.GT.1.0E-7 .AND. Itester.NE.1 )
        DO j = 1, Numwaves
          checktime(j) = 0.0D0
          more(j) = 0
        END DO
        j = 2
C
C3------CALCULATE TIME UNTIL A WAVE WILL OVERTAKE A WAVE AHEAD.
        DO WHILE ( j.LE.Numwaves )
          IF ( j.LT.Numwaves ) THEN
            IF ( Ltrail(Jpnt+j-1).NE.0 .AND. Itrwave(Jpnt+j).GT.0 ) THEN
              DO WHILE ( Ltrail(Jpnt+j-1).NE.0 .AND. 
     +                   Itrwave(Jpnt+j).GT.0)
                kk = j + Itrwave(Jpnt+j)
                IF ( j.GT.2 .AND. ABS(Speed(Jpnt+j-2)-
     +               Speed(Jpnt+j-1)).GT.CLOSEZERO ) THEN
                  checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+j-2))
     +                         /(Speed(Jpnt+j-2)-Speed(Jpnt+j-1))
                ELSE
                  checktime(j) = big
                END IF
                IF ( Numwaves.GT.kk ) THEN
                  jj = j
                  j = j + Itrwave(Jpnt+j) + 1
C
C4------LEAD WAVE INTERSECTS A TRAIL WAVE.
                  fhold = 0.0D0
                  IF ( ABS(Theta(Jpnt+jj-1)-Thetar).GT.CLOSEZERO )
     +                fhold = (f7*Theta(Jpnt+j-2)+f8*Theta(Jpnt+j-3)-
     +                        Thetar)/(Theta(Jpnt+jj-1)-Thetar)
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                    checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+jj-1)
     +                           *(fhold**eps_m1))/(Speed(Jpnt+jj-1)
     +                           *(fhold**eps_m1)-Speed(Jpnt+j-1))
                ELSE
                  j = j + 1
                END IF
              END DO
            ELSE IF ( ABS(Speed(Jpnt+j-2)-Speed(Jpnt+j-1)).GT.CLOSEZERO 
     +                .AND. j.NE.1 ) THEN
              checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+j-2))
     +                     /(Speed(Jpnt+j-2)-Speed(Jpnt+j-1))
            ELSE
              checktime(j) = big
            END IF
          ELSE IF ( ABS(Speed(Jpnt+j-2)-Speed(Jpnt+j-1)).GT.CLOSEZERO 
     +                .AND. j.NE.1 ) THEN
            checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+j-2))
     +                   /(Speed(Jpnt+j-2)-Speed(Jpnt+j-1))
          ELSE
            checktime(j) = big
          END IF
          j = j + 1
        END DO
        DO j = 2, Numwaves
          IF ( checktime(j).LT.NEARZERO ) checktime(j) = big
        END DO
C
C5------CALCULATE HOW LONG IT WILL TAKE BEFORE DEEPEST WAVE REACHES
C         WATER TABLE.
        IF ( Numwaves.GT.1 ) THEN
Cdep 
          IF ( Speed(Jpnt+1).GT.0.0D0 ) THEN
            bottomtime = (Depth(Jpnt)-Depth(Jpnt+1))/Speed(Jpnt+1)
            IF ( bottomtime.LT.0.0 ) bottomtime = 1.0D-12
          ELSE
            bottomtime = big
          END IF
        ELSE
          bottomtime = big
        END IF
C
C6------CALCULATE SHORTEST TIME FOR WAVE INTERCEPTION.
        shortest = DELT - Time
        DO j = Numwaves, 3, -1
          IF ( checktime(j).LE.shortest ) THEN
            more(j) = 1
            shortest = checktime(j)
            DO k = j + 1, Numwaves
              IF ( ABS(checktime(k)-checktime(j)).GT.CLOSEZERO ) more(k)
     +             = 0
            END DO
          END IF
        END DO
        IF ( Numwaves.EQ.2 ) shortest = DELT - Time
C
C7------CHECK IF DEEPEST WAVE REACHES WATER TABLE BEFORE WAVES
C         INTERCEPT EACH OTHER.
        iremove = 0
        timenew = Time
        fcheck = (Time+shortest) - DELT
        IF ( shortest.LT.1.0E-7 ) fcheck = -1.0D0
        IF ( bottomtime.LT.shortest .AND. Time+bottomtime.LE.DELT ) THEN
          j = 2
          DO WHILE ( j.LE.Numwaves )
C
C8--------ROUTE TRAILING WAVES.
            IF ( Itrwave(Jpnt+j-1).EQ.0 ) THEN
              Depth(Jpnt+j-1) = Depth(Jpnt+j-1) + Speed(Jpnt+j-1)
     +                          *bottomtime
            ELSE
              DO k = j, j + Itrwave(Jpnt+j-1) - 1
                Depth(Jpnt+k-1) = Depth(Jpnt+j-2)*((f7*Theta(Jpnt+k-1)
     +                            +f8*Theta(Jpnt+k-2)-Thetar)
     +                            /(Theta(Jpnt+j-2)-Thetar))**eps_m1
              END DO
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
          FLUXB = Flux(Jpnt+1)
          THETAB = Theta(Jpnt+1)
          iflx = 1
          itrwaveb = Itrwave(Jpnt+2)
          DO k = 2, Numwaves
            jpntpkm1 = Jpnt + k - 1
            jpntpkm2 = jpntpkm1 - 1
            Flux(jpntpkm2) = Flux(jpntpkm1)
            Theta(jpntpkm2) = Theta(jpntpkm1)
            Speed(jpntpkm2) = Speed(jpntpkm1)
            Depth(jpntpkm2) = Depth(jpntpkm1)
            Itrwave(jpntpkm2) = Itrwave(jpntpkm1)
            Ltrail(jpntpkm2) = Ltrail(jpntpkm1)
          END DO
          IF ( itrwaveb.EQ.1 ) THEN
            Itrwave(Jpnt+1) = 0
            Ltrail(Jpnt+1) = 1
            fhold = (Theta(Jpnt+1)-Thetar)/(Thetas-Thetar)
            IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
            Speed(Jpnt+1) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
C
C9------CONVERT TRAIL WAVES TO LEAD TRAIL WAVES.
          ELSE IF ( itrwaveb.GT.1 ) THEN
            DO k = Jpnt + 1, Jpnt + itrwaveb
              Itrwave(k) = 0
              Ltrail(k) = 1
              IF ( ABS(Theta(k)-Theta(k-1)).LT.CLOSEZERO ) THEN
                fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                Speed(k) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
              ELSE
                fhold = ((Theta(k-1)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta1 = Fksat*fhold
                fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta2 = Fksat*fhold
                Speed(k) = (ftheta1-ftheta2)/(Theta(k-1)-Theta(k))
              END IF
            END DO
          END IF
          iremove = 1
          timenew = Time + bottomtime
          Ltrail(Jpnt) = 0
          Speed(Jpnt) = 0.0D0
C
C10-----CHECK IF WAVES INTERCEPT BEFORE TIME STEP ENDS.
        ELSE IF ( fcheck.LT.0.0 .AND. Numwaves.GT.2 ) THEN
          j = 2
          DO WHILE ( j.LE.Numwaves )
            IF ( Itrwave(Jpnt+j-1).EQ.0 ) THEN
              Depth(Jpnt+j-1) = Depth(Jpnt+j-1) + Speed(Jpnt+j-1)
     +                          *shortest
            ELSE
C
C11-----ROUTE TRAIL WAVES.
              DO k = j, j + Itrwave(Jpnt+j-1) - 1
                Depth(Jpnt+k-1) = Depth(Jpnt+j-2)*((f7*Theta(Jpnt+k-1)
     +                            +f8*Theta(Jpnt+k-2)-Thetar)
     +                            /(Theta(Jpnt+j-2)-Thetar))**eps_m1
              END DO
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
C
C12-----REMOVE WAVES THAT HAVE BEEN INTERCEPTED AND UPDATE SPEED
C         OF COMBINED WAVE.
          j = 3
          l = j
          iflag = 0
          DO WHILE ( iflag.EQ.0 )          
            IF ( more(j).EQ.1 ) THEN
              l = j
C
C13-----CHECK IF INTERCEPTED WAVES ARE TRAIL WAVES. 
              IF ( Ltrail(Jpnt+j-1).NE.1 ) THEN
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(Jpnt+k-1) )
     +                   Itrwave(Jpnt+k-1) = Itrwave(Jpnt+k-1) - 1
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
                IF ( j.EQ.3 ) THEN
                  comp1 = ABS(Theta(Jpnt+j-1)-THETAB)
                  comp2 = ABS(Flux(Jpnt+j-1)-FLUXB)
                  IF ( comp1.LE.1.E-9 )
     +                 Theta(Jpnt+j-1) = THETAB - ZEROD9
                  IF ( comp2.LE.CLOSEZERO )
     +                 Flux(Jpnt+j-1) = FLUXB - ZEROD15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                              /(Theta(Jpnt+j-1)-THETAB)
                ELSE
                  comp1 = ABS(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                  comp2 = ABS(Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
                  IF ( comp1.LT.1.0E-9 ) Theta(Jpnt+j-1)
     +                 = Theta(Jpnt+j-3) - ZEROD9
                  IF ( comp2.LT.CLOSEZERO ) Flux(Jpnt+j-1)
     +                 = Flux(Jpnt+j-3) - ZEROD15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-Flux(Jpnt+j-3))/
     +                              (Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                END IF
C
C14-----CONVERT REMAINING TRAIL WAVES TO LEAD TRAIL WAVES WHEN
C         WHEN LEAD TRAIL WAVE INTERSECTS A LEAD WAVE.                
              ELSE IF ( Itrwave(Jpnt+j).GT.0 ) THEN
                IF ( ABS(Speed(Jpnt+j-2)).GT.CLOSEZERO ) THEN
                  DO k = Jpnt + j, Jpnt + j + Itrwave(Jpnt+j) - 1
                    Ltrail(k) = 1
                    Itrwave(k) = 0
                    IF ( ABS(Theta(k)-Theta(k-1)).LT.CLOSEZERO ) THEN
                      fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      Speed(k) = (Eps*Fksat/(Thetas-Thetar))*fhold
     +                           **eps_m1
                    ELSE
                      fhold = ((Theta(k-1)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      ftheta1 = Fksat*fhold
                      fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      ftheta2 = Fksat*fhold
                      Speed(k) = (ftheta1-ftheta2)/(Theta(k-1)-Theta(k))
                    END IF
                  END DO
                  Ltrail(Jpnt+j-1) = 0                 
                  IF ( j.EQ.3 ) THEN
                    comp1 = ABS(Theta(Jpnt+j-1)-THETAB)
                    comp2 = ABS(Flux(Jpnt+j-1)-FLUXB)
                    IF (comp1.LE.1.E-9)
     +                  Theta(Jpnt+j-1) = THETAB - ZEROD9
                    IF (comp2.LE.CLOSEZERO)
     +                  Flux(Jpnt+j-1) = FLUXB - ZEROD15
                    Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                                /(Theta(Jpnt+j-1)-THETAB)
                    IF ( Flux(Jpnt+j-1)-FLUXB.LT.0.0D0 ) THEN
                      fhold = (Theta(Jpnt+j-1)-Thetar)/(Thetas-Thetar)
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      Speed(Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                                  *fhold**eps_m1
                      Ltrail(Jpnt+j-1) = 1
                    ELSE
                      Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                                  /(Theta(Jpnt+j-1)-THETAB)
                    END IF
                  ELSE
                    comp1 = ABS(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                    comp2 = ABS(Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
                    IF ( comp1.LT.1.0E-9 ) Theta(Jpnt+j-1)
     +                   = Theta(Jpnt+j-3) - ZEROD9
                    IF ( comp2.LT.CLOSEZERO ) Flux(Jpnt+j-1)
     +                   = Flux(Jpnt+j-3) - ZEROD15
                    IF ( Flux(Jpnt+j-1)-Flux(Jpnt+j-3).LT.0.0D0 ) THEN
                      fhold = (Theta(Jpnt+j-1)-Thetar)/(Thetas-Thetar)
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      Speed(Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                                  *fhold**eps_m1
                      Ltrail(Jpnt+j-1) = 1
                    ELSE
                      Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
     +                                /(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                    END IF
                  END IF
                END IF
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(Jpnt+k-1) )
     +                   Itrwave(Jpnt+k-1) = Itrwave(Jpnt+k-1) - 1
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
                j = j + Itrwave(Jpnt+j+1) + 2
C
C15-----NON TRAIL WAVES INTERCEPTS A NON TRAIL WAVE. 
              ELSE
                Ltrail(Jpnt+j-1) = 0
                Itrwave(Jpnt+j) = 0
                IF ( j.EQ.3 ) THEN
                  comp1 = ABS(Theta(Jpnt+j-1)-THETAB)
                  comp2 = ABS(Flux(Jpnt+j-1)-FLUXB)
                  IF ( comp1.LE.1.E-9 )
     +                 Theta(Jpnt+j-1) = THETAB - ZEROD9
                  IF ( comp2.LE.CLOSEZERO )
     +                 Flux(Jpnt+j-1) = FLUXB - ZEROD15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                              /(Theta(Jpnt+j-1)-THETAB)
                ELSE
                  comp1 = ABS(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                  comp2 = ABS(Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
                  IF ( comp1.LT.1.0E-9 ) Theta(Jpnt+j-1)
     +                 = Theta(Jpnt+j-3) - ZEROD9
                  IF ( comp2.LT.CLOSEZERO ) Flux(Jpnt+j-1)
     +                 = Flux(Jpnt+j-3) - ZEROD15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-Flux(Jpnt+j-3))/
     +                              (Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                END IF
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(Jpnt+k-1) ) THEN
                      Itrwave(Jpnt+k-1) = Itrwave(Jpnt+k-1) - 1
                      IF ( Theta(Jpnt+j-1).LE.Theta(Jpnt+j-3) ) THEN
                        Ltrail(Jpnt+j-1) = 1
                        fhold = (Theta(Jpnt+j-1)-Thetar)/(Thetas-Thetar)
                        IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                        Speed(Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                                    *fhold**eps_m1
                      END IF
                    END IF
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
              END IF
C
C16-----UPDATE WAVES.
              DO k = l, Numwaves
                jpntpkm1 = Jpnt + k - 1
                jpntpkm2 = jpntpkm1 - 1
                Flux(jpntpkm2) = Flux(jpntpkm1)
                Theta(jpntpkm2) = Theta(jpntpkm1)
                Speed(jpntpkm2) = Speed(jpntpkm1)
                Depth(jpntpkm2) = Depth(jpntpkm1)
                Itrwave(jpntpkm2) = Itrwave(jpntpkm1)
                Ltrail(jpntpkm2) = Ltrail(jpntpkm1)
              END DO
              l = Numwaves + 1
              iremove = iremove + 1
            ELSE IF ( Itrwave(Jpnt+j-1).GT.0 ) THEN
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
            IF ( j.GT.Numwaves ) iflag = 1
          END DO
          timenew = timenew + shortest
C
C17-----CALCULATE TOTAL FLUX TO WATER TABLE FOR CONSTANT FLUX
C         DURING REMAINING TIME.
        ELSE
          j = 2
          DO WHILE ( j.LE.Numwaves )
            IF ( Itrwave(Jpnt+j-1).EQ.0 ) THEN
              Depth(Jpnt+j-1) = Depth(Jpnt+j-1) + Speed(Jpnt+j-1)
     +                          *(DELT-Time)
            ELSE
C
C18-----ROUTE TRAILING WAVES.
              DO k = j, j + Itrwave(Jpnt+j-1) - 1
                Depth(Jpnt+k-1) = Depth(Jpnt+j-2)*((f7*Theta(Jpnt+k-1)
     +                            +f8*Theta(Jpnt+k-2)-Thetar)
     +                            /(Theta(Jpnt+j-2)-Thetar))**eps_m1
              END DO
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
          timenew = DELT
        END IF
        Totalflux = Totalflux + FLUXHLD2*(timenew-Time)
        IF ( iflx.EQ.1 ) THEN
          FLUXHLD2 = Flux(Jpnt)
          iflx = 0
        END IF
C
C19-------REMOVE WAVES THAT WERE INTERCEPTED OR REACHED WATER TABLE.
        Numwaves = Numwaves - iremove
Crsr timedif = timenew - time
        Time = timenew
        diff = DELT - Time
        IF ( Numwaves.EQ.1 ) Itester = 1
      END DO
C
C20-----RETURN.
      RETURN
      END SUBROUTINE LEADWAVE2
C
C--------SUBROUTINE TRAILWAVE2
C
      SUBROUTINE TRAILWAVE2(Numwaves, I, Flux, Theta, Speed, Depth, 
     +                      Itrwave, Ltrail, Fksat, Eps, Thetas, Thetar,
     +                      Surflux, Jpnt)
      USE GWFUZFMODULE, ONLY: NWAV, NTRAIL, NEARZERO, THETAB, FLUXB,
     +                        FLUXHLD2, ZEROD6
      USE GLOBAL, ONLY: IOUT
C     ******************************************************************
C     INITIALIZE NEW SET OF TRAIL WAVES WHEN SURFACE FLUX DECREASES
C     VERSION 1.3:  June 20, 2007
C     ******************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Eps, Fksat, Thetas
      INTEGER I, Jpnt, Numwaves, Itrwave(NWAV), Ltrail(NWAV)
      DOUBLE PRECISION Depth(NWAV), Theta(NWAV), Flux(NWAV), Speed(NWAV)
      DOUBLE PRECISION Surflux, Thetar
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION smoist, smoistinc, ftrail, fhold, eps_m1, feps3
      REAL fnuminc
      INTEGER j, jj, jk, kk, numtrail2, jpnwavesm1, jpnwavesm2, jpntpjm1
C     ------------------------------------------------------------------
      eps_m1 = DBLE(Eps) - 1.0D0
      THETAB = Theta(Jpnt)
      FLUXB = Flux(Jpnt)
      numtrail2 = NTRAIL
      jpnwavesm1 = Jpnt + Numwaves - 1
      jpnwavesm2 = jpnwavesm1 - 1
      feps3 = ZEROD6
C1------INITIALIZE TRAILING WAVES.
      kk = 1
      FLUXHLD2 = Flux(Jpnt)
      IF ( Surflux.LT.NEARZERO ) Surflux = 0.0D0
      smoist = (((Surflux/Fksat)**(1.0/Eps))*(Thetas-Thetar)) + Thetar
      IF ( Theta(jpnwavesm2)-smoist.GT.feps3 ) THEN
        fnuminc = 0.0
        DO jk = 1, NTRAIL
          fnuminc = fnuminc + FLOAT(jk)
        END DO
        smoistinc = (Theta(jpnwavesm2)-smoist)/(fnuminc-1.0)
        jj = NTRAIL
        ftrail = NTRAIL + 1
        DO j = Numwaves, Numwaves + numtrail2 - 1
          IF ( j.GT.NWAV ) THEN
            WRITE (*, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves, 
     +                   '   PROGRAM TERMINATED IN TRAILWAVE2 UZF - 2'
            WRITE (IOUT, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves,
     +              '   PROGRAM TERMINATED IN UZFLOW-2; INCREASE NSETS2'
            STOP
          END IF
          jpntpjm1 = Jpnt + j - 1
          Ltrail(jpntpjm1) = 0
          Itrwave(jpntpjm1) = 0
          IF ( j.GT.Numwaves ) THEN
            Theta(jpntpjm1) = Theta(Jpnt+j-2)
     +                        - ((ftrail-FLOAT(jj))*smoistinc)
          ELSE
            Theta(jpntpjm1) = Theta(Jpnt+j-2) - feps3
          END IF
          jj = jj - 1
          IF ( Theta(jpntpjm1).LE.Thetar+feps3 ) Theta(jpntpjm1)
     +         = Thetar + feps3
          Flux(jpntpjm1) = Fksat*((Theta(jpntpjm1)-Thetar)
     +                     /(Thetas-Thetar))**Eps
          IF ( j.EQ.Numwaves ) THEN
            fhold = (Theta(jpntpjm1)-Thetar)/(Thetas-Thetar)
            IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
            Speed(jpntpjm1) = ((Eps*Fksat)/(Thetas-Thetar))*fhold
     +                        **eps_m1
          ELSE
            Speed(jpntpjm1) = 0.0D0
          END IF
          kk = kk + 1
          Depth(jpntpjm1) = 0.0D0
        END DO
        Itrwave(Jpnt+Numwaves) = numtrail2 - 1
        Ltrail(jpnwavesm1) = 1
        Numwaves = Numwaves + numtrail2 - 1
        IF ( Numwaves.GE.NWAV ) THEN
          WRITE (*, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves, 
     +                 '   PROGRAM TERMINATED IN UZFLOW-4'
          WRITE (IOUT, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves, 
     +              '   PROGRAM TERMINATED IN UZFLOW-4; INCREASE NSETS2'
          STOP
        END IF
      ELSE
        Ltrail(jpnwavesm1) = 1
        Theta(jpnwavesm1) = Theta(jpnwavesm2)
        Depth(jpnwavesm1) = 0.0D0
        fhold = (Theta(jpnwavesm1)-Thetar)/(Thetas-Thetar)
        IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
        Speed(jpnwavesm1) = ((Eps*Fksat)/(Thetas-Thetar))*fhold**eps_m1
        Flux(jpnwavesm1) = Fksat*((Theta(jpnwavesm1)-Thetar)
     +                     /(Thetas-Thetar))**Eps
        Theta(jpnwavesm1) = smoist
      END IF
C
C2------RETURN.
      RETURN
      END SUBROUTINE TRAILWAVE2
C
C--------SUBROUTINE TRANSPIRATION
      SUBROUTINE TRANSPIRATION(Numwaves, Flux, Theta, Speed, Depth, 
     +                        Itrwave, Ltrail, Fksat, Eps, Thetas, 
     +                        Thetar, Jpnt, Etime, Rateud, Etout, 
     +                        Wiltwc1, Rootdepth, Nwv)
C     ******************************************************************
C     REMOVE WATER FROM UNSATURATED ZONE CAUSED BY EVAPOTRANSPIRATION
C     ******************************************************************
      USE GWFUZFMODULE, ONLY: NWAV, NEARZERO, ZEROD6
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Jpnt, Numwaves, Nwv
      INTEGER Itrwave(NWAV), Ltrail(NWAV)
      REAL Eps, Fksat, Rootdepth, Thetas, Wiltwc1, Wiltwc
      DOUBLE PRECISION Depth(NWAV), Theta(NWAV), Flux(NWAV), Speed(NWAV)
      DOUBLE PRECISION Thetar, Etout, Rateud, Etime
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION diff, thetaout, fm, st, fhold, eps_m1
      DOUBLE PRECISION depth2, theta2, flux2, speed2, zero
      DIMENSION depth2(Nwv), theta2(Nwv), flux2(Nwv), speed2(Nwv)
      REAL feps, ftheta1, ftheta2
      INTEGER ihold, ii, inck, itrwaveyes, j, jhold, jk, kj, kk, numadd,
     +        ltrail2(Nwv), itrwave2(Nwv), icheckwilt, icheckitr, jkp1,
     +        kjm1
C     ------------------------------------------------------------------
C
C1------INITIALIZE VARIABLES.
      eps_m1 = DBLE(Eps) - 1.0D0
      zero = 1.0D-10
      icheckwilt = 0
      thetaout = Etime*Rateud
      IF ( thetaout.LE.zero ) RETURN
      Etout = 0.0D0
      feps = 1.0E-5
      DO ii = 1, Nwv
        depth2(ii) = Depth(ii)
        theta2(ii) = Theta(ii)
        flux2(ii) = Flux(ii)
        speed2(ii) = Speed(ii)
        ltrail2(ii) = Ltrail(ii)
        itrwave2(ii) = Itrwave(ii)
      END DO
      IF ( Wiltwc1.LT.Thetar ) Wiltwc1 = Thetar + .00001D0
      Wiltwc = Wiltwc1 - Thetar
      numadd = 0
      st = 0.0D0
C
C2------ONE WAVE IN PROFILE THAT IS SHALLOWER THAN ET EXTINCTION DEPTH.
      DO ii = Jpnt + Numwaves - 1, Jpnt, -1
        IF ( ii.EQ.Jpnt+Numwaves-1 ) THEN
          st = st + Depth(ii)*Theta(ii)
        ELSE
          st = st + (Depth(ii)-Depth(ii+1))*Theta(ii)
        END IF
      END DO
      IF ( Numwaves.EQ.1 .AND. Depth(Jpnt).LE.Rootdepth ) THEN
        IF ( (Theta(Jpnt)-thetaout).GT.Thetar+Wiltwc ) THEN
          Theta(Jpnt) = Theta(Jpnt) - thetaout
          Flux(Jpnt) = Fksat*((Theta(Jpnt)-Thetar)/(Thetas-Thetar))**Eps
        ELSE IF ( Theta(Jpnt).GT.Thetar+Wiltwc ) THEN
          Theta(Jpnt) = Thetar + Wiltwc
          Flux(Jpnt) = Fksat*((Theta(Jpnt)-Thetar)/(Thetas-Thetar))**Eps
        END IF
C
C3------MULTIPLE WAVES BUT SHALLOWEST WAVE IS DEEPER THAN ET EXTINCTION
C         DEPTH.
      ELSE IF ( Numwaves.GT.1 .AND. Depth(Jpnt+Numwaves-1).GT.Rootdepth
     +          ) THEN
        IF ( Theta(Jpnt+Numwaves-1)-thetaout.GT.Thetar+Wiltwc ) THEN
          Theta(Jpnt+Numwaves) = Theta(Jpnt+Numwaves-1) - thetaout
          numadd = 1
        ELSE IF ( Theta(Jpnt+Numwaves-1).GT.Thetar+Wiltwc ) THEN
          Theta(Jpnt+Numwaves) = Thetar + Wiltwc
          numadd = 1
        END IF
        IF ( numadd.EQ.1 ) THEN
          Flux(Jpnt+Numwaves) = Fksat*((Theta(Jpnt+Numwaves)-Thetar)
     +                          /(Thetas-Thetar))**Eps
          fhold = (Theta(Jpnt+Numwaves)-Thetar)/(Thetas-Thetar)
          IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
          Speed(Jpnt+Numwaves) = ((Eps*Fksat)/(Thetas-Thetar))*fhold
     +                           **eps_m1
          Depth(Jpnt+Numwaves) = Rootdepth
          Itrwave(Jpnt+Numwaves) = 0
          Ltrail(Jpnt+Numwaves) = 1
          Numwaves = Numwaves + 1
        END IF
C
C4------ONLY ONE WAVE IS DEEPER THAN ET EXTINCTION DEPTH.
      ELSE IF ( Numwaves.EQ.1 ) THEN
        IF ( (Theta(Jpnt)-thetaout).GT.Thetar+Wiltwc ) THEN
          Theta(Jpnt+1) = Theta(Jpnt) - thetaout
          Flux(Jpnt+1) = Fksat*((Theta(Jpnt+1)-Thetar)/(Thetas-Thetar))
     +                   **Eps
          Depth(Jpnt+1) = Rootdepth
          fhold = (Theta(Jpnt+1)-Thetar)/(Thetas-Thetar)
          IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
          Speed(Jpnt+1) = ((Eps*Fksat)/(Thetas-Thetar))*fhold**eps_m1
          Itrwave(Jpnt+1) = 0
          Ltrail(Jpnt+1) = 1
          Numwaves = Numwaves + 1
        ELSE IF ( Theta(Jpnt).GT.Thetar+Wiltwc ) THEN
          Theta(Jpnt+1) = Thetar + Wiltwc
          Flux(Jpnt+1) = Fksat*((Theta(Jpnt+1)-Thetar)/(Thetas-Thetar))
     +                   **Eps
          fhold = (Theta(Jpnt+1)-Thetar)/(Thetas-Thetar)
          IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
          Speed(Jpnt+1) = ((Eps*Fksat)/(Thetas-Thetar))*fhold**eps_m1
          Itrwave(Jpnt+1) = 0
          Ltrail(Jpnt+1) = 1
          Numwaves = Numwaves + 1
        END IF
      ELSE
C
C5------MULTIPLE WAVES AND ET EXTINCTION DEPTH FALL BETWEEN DEEPEST 
C         AND SHALLOWEST WAVE.
        IF ( Depth(Jpnt).GT.Rootdepth ) THEN
          j = Jpnt + Numwaves - 2
          jk = 0
C
C6------LOCATE ET EXTINCTION DEPTH BETWEEN WAVES.
          DO WHILE ( jk.EQ.0 )
            diff = Depth(j) - Rootdepth
            IF ( diff.LE.0.0 ) THEN
              j = j - 1
            ELSE
              jk = 1
            END IF
          END DO
          kk = j
          itrwaveyes = 0
          IF ( Theta(j).GT.Thetar+Wiltwc ) THEN
            DO WHILE ( kk.GE.Jpnt )
              IF ( Itrwave(kk).GT.0 ) THEN
                IF ( kk+Itrwave(kk)-1.GT.j ) THEN
                  itrwaveyes = kk
                  kk = Jpnt - 1
                END IF
              END IF
              kk = kk - 1
            END DO
C
C7------CREATE A NEW WAVE AT ET EXTINCTION DEPTH.
            IF ( ABS(diff).GT.feps ) THEN
              DO kj = Jpnt + Numwaves, j + 1, -1
                kjm1 = kj - 1
                Theta(kj) = Theta(kjm1)
                Speed(kj) = Speed(kjm1)
                Flux(kj) = Flux(kjm1)
                Depth(kj) = Depth(kjm1)
                Itrwave(kj) = Itrwave(kjm1)
                Ltrail(kj) = Ltrail(kjm1)
              END DO
              j = j + 1
              IF ( itrwaveyes.GT.0 ) THEN
                Theta(j) = ((Rootdepth/Depth(itrwaveyes-1))**(1.0D0/
     +                    eps_m1))*(Theta(itrwaveyes-1)-Thetar) + Thetar
                Flux(j) = Fksat*((Theta(j)-Thetar)/(Thetas-Thetar))**Eps
                fhold = (Theta(j)-Thetar)/(Thetas-Thetar)
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                Speed(j) = ((Eps*Fksat)/(Thetas-Thetar))*fhold**eps_m1

              END IF
              Depth(j) = Rootdepth
              Numwaves = Numwaves + 1
            END IF
            IF ( itrwaveyes.GT.0 ) THEN
              ihold = Itrwave(itrwaveyes)
              Itrwave(itrwaveyes) = j - itrwaveyes
              IF ( Itrwave(itrwaveyes).LT.0 ) Itrwave(itrwaveyes) = 0
C
C8------CONVERT TRAIL WAVES SPLIT BY ET EXTINCTION DEPTH TO
C         LEAD TRAIL WAVES IF WATER CONTENT ABOVE EXTINCTION WATER
C         CONTENT.
              DO kj = j + 1, j + 1 + ihold - Itrwave(itrwaveyes)
                Ltrail(kj) = 1
                Itrwave(kj) = 0
                IF ( ABS(Theta(kj)-Theta(kj-1)).LT.ZEROD6 ) THEN
                  fhold = ((Theta(kj)-Thetar)/(Thetas-Thetar))**Eps
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                  Speed(kj) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
                ELSE
                  fhold = ((Theta(kj-1)-Thetar)/(Thetas-Thetar))**Eps
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                  ftheta1 = Fksat*fhold
                  fhold = ((Theta(kj)-Thetar)/(Thetas-Thetar))**Eps
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                  ftheta2 = Fksat*fhold
                  Speed(kj) = (ftheta1-ftheta2)/(Theta(kj-1)-Theta(kj))
                END IF
              END DO
            END IF
            kk = j
          ELSE
            jhold = Jpnt + Numwaves - 1
            ii = j + 1
            DO WHILE ( ii.LT.Jpnt+Numwaves-1 )
              IF ( Theta(ii).GT.Thetar+Wiltwc ) THEN
                jhold = ii
                ii = Jpnt + Numwaves
              END IF
              ii = ii + 1
            END DO
            j = jhold
            kk = jhold
          END IF
        ELSE
          kk = Jpnt
        END IF
C
C9------ALL WAVES SHALLOWER THAN ET EXTINCTION DEPTH.        
        DO WHILE ( kk.LE.Jpnt+Numwaves-1 )
          inck = 0
          IF ( Itrwave(kk+1).EQ.0 ) THEN
            IF ( Theta(kk).GT.Thetar+Wiltwc ) THEN
              IF ( Theta(kk)-thetaout.GT.Thetar+Wiltwc ) THEN
                Theta(kk) = Theta(kk) - thetaout
              ELSE IF ( Theta(kk).GT.Thetar+Wiltwc ) THEN
                Theta(kk) = Thetar + Wiltwc
              END IF
              IF ( kk.EQ.Jpnt ) THEN
                Flux(kk) = Fksat*((Theta(kk)-Thetar)/(Thetas-Thetar))
     +                     **Eps
                Itrwave(kk) = 0
                Ltrail(kk) = 0
              ELSE IF ( Theta(kk-1).GE.Theta(kk) ) THEN
                IF ( ABS(Theta(kk)-Theta(kk-1)).LT.ZEROD6 ) THEN
                  fhold = ((Theta(kk)-Thetar)/(Thetas-Thetar))**Eps
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                  Speed(kk) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
                ELSE
                  fhold = ((Theta(kk-1)-Thetar)/(Thetas-Thetar))**Eps
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                  ftheta1 = Fksat*fhold
                  fhold = ((Theta(kk)-Thetar)/(Thetas-Thetar))**Eps
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                  ftheta2 = Fksat*fhold
                  Speed(kk) = (ftheta1-ftheta2)/(Theta(kk-1)-Theta(kk))
                END IF
                Flux(kk) = Fksat*((Theta(kk)-Thetar)/(Thetas-Thetar))
     +                     **Eps
                Ltrail(kk) = 1
                Itrwave(kk) = 0
              ELSE
                Flux(kk) = Fksat*((Theta(kk)-Thetar)/(Thetas-Thetar))
     +                     **Eps
                Speed(kk) = (Flux(kk)-Flux(kk-1))/(Theta(kk)-Theta(kk-1)
     +                      )
                Itrwave(kk) = 0
                Ltrail(kk) = 0
              END IF
            END IF
            IF ( Ltrail(kk).EQ.1 ) THEN
              IF ( ABS(Theta(kk)-Theta(kk-1)).LT.ZEROD6 ) THEN
                fhold = ((Theta(kk)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                Speed(kk) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
              ELSE
                fhold = ((Theta(kk-1)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta1 = Fksat*fhold
                fhold = ((Theta(kk)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta2 = Fksat*fhold
                Speed(kk) = (ftheta1-ftheta2)/(Theta(kk-1)-Theta(kk))
              END IF
            END IF
          ELSE
            inck = Itrwave(kk+1)
            icheckwilt = 0
            DO kj = kk, kk + inck
              IF ( Theta(kj)-thetaout.GT.Thetar+Wiltwc ) THEN
                Theta(kj) = Theta(kj) - thetaout
              ELSE IF ( Theta(kj).GT.Thetar+Wiltwc ) THEN
                Theta(kj) = Thetar + Wiltwc
                IF ( icheckwilt.EQ.0 ) icheckwilt = kj
              END IF
              IF ( ABS(Theta(kj)-Theta(kj-1)).LT.ZEROD6 .OR. kj.EQ.kk
     +             ) THEN
                fhold = ((Theta(kj)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                Speed(kj) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
                Flux(kj) = Fksat*((Theta(kj)-Thetar)/(Thetas-Thetar))
     +                     **Eps
                Ltrail(kj) = 1
                Itrwave(kj) = 0
              ELSE
                fhold = ((Theta(kj-1)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta1 = Fksat*fhold
                fhold = ((Theta(kj)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta2 = Fksat*fhold
                Speed(kj) = (ftheta1-ftheta2)/(Theta(kj-1)-Theta(kj))
                Flux(kj) = Fksat*((Theta(kj)-Thetar)/(Thetas-Thetar))
     +                     **Eps
                Ltrail(kj) = 1
                Itrwave(kj) = 0
              END IF
            END DO
          END IF
          kk = kk + inck + 1
        END DO
      END IF
C
C10-----CALCULATE ACTUAL ET.
      kj = Jpnt
      icheckitr = 0
      DO WHILE ( kj.LE.Jpnt+Numwaves-2 )
        IF ( Itrwave(kj).GT.0 ) icheckitr = kj + Itrwave(kj) - 1
        IF ( ABS(Theta(kj)-Theta(kj+1)).LT.ZEROD6 ) THEN
          IF ( Itrwave(kj+2).EQ.0 .AND. kj.GT.icheckitr ) THEN
            DO jk = kj + 1, Jpnt + Numwaves - 2
              jkp1 = jk + 1
              Theta(jk) = Theta(jkp1)
              Speed(jk) = Speed(jkp1)
              Flux(jk) = Flux(jkp1)
              Depth(jk) = Depth(jkp1)
              Itrwave(jk) = Itrwave(jkp1)
              Ltrail(jk) = Ltrail(jkp1)
            END DO
            kj = kj - 1
            Numwaves = Numwaves - 1
          END IF
        END IF
        kj = kj + 1
      END DO
      fm = 0.0D0
      DO ii = Jpnt + Numwaves - 1, Jpnt, -1
        IF ( ii.EQ.Jpnt+Numwaves-1 ) THEN
          fm = fm + Depth(ii)*Theta(ii)
        ELSE
          fm = fm + (Depth(ii)-Depth(ii+1))*Theta(ii)
        END IF
      END DO
C
C11-----SET ETOUT TO ZERO WHEN ET DEMAND LESS THAN ROUNDOFF ERROR.
      Etout = st - fm
      IF ( Etout.LT.0.0 ) THEN
        DO ii = 1, Nwv
          Depth(ii) = depth2(ii)
          Theta(ii) = theta2(ii)
          Flux(ii) = flux2(ii)
          Speed(ii) = speed2(ii)
          Ltrail(ii) = ltrail2(ii)
          Itrwave(ii) = itrwave2(ii)
        END DO
        Numwaves = Nwv
        Etout = 0.0D0
      END IF
C12-----RETURN.
      RETURN
      END SUBROUTINE TRANSPIRATION
C
C-------SUBROUTINE GWF2UZF1DA
      SUBROUTINE GWF2UZF1DA(Igrid)
C    Deallocate UZF DATA. 
      USE GWFUZFMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Igrid
C     ------------------------------------------------------------------
      DEALLOCATE (GWFUZFDAT(Igrid)%NUZTOP)
      DEALLOCATE (GWFUZFDAT(Igrid)%IUZFOPT)
      DEALLOCATE (GWFUZFDAT(Igrid)%IRUNFLG)
      DEALLOCATE (GWFUZFDAT(Igrid)%IETFLG)
      DEALLOCATE (GWFUZFDAT(Igrid)%IUZM)
      DEALLOCATE (GWFUZFDAT(Igrid)%IUZFCB1)
      DEALLOCATE (GWFUZFDAT(Igrid)%IUZFCB2)
      DEALLOCATE (GWFUZFDAT(Igrid)%NTRAIL)
      DEALLOCATE (GWFUZFDAT(Igrid)%NWAV)
      DEALLOCATE (GWFUZFDAT(Igrid)%NSETS)
      DEALLOCATE (GWFUZFDAT(Igrid)%IGSFLOW)
      DEALLOCATE (GWFUZFDAT(Igrid)%NUZGAG)
      DEALLOCATE (GWFUZFDAT(Igrid)%NUZGAGAR)
      DEALLOCATE (GWFUZFDAT(Igrid)%NUZCL)
      DEALLOCATE (GWFUZFDAT(Igrid)%NUZRW)
      DEALLOCATE (GWFUZFDAT(Igrid)%ITRLSTH)
      DEALLOCATE (GWFUZFDAT(Igrid)%IRUNBND)
      DEALLOCATE (GWFUZFDAT(Igrid)%IUZFBND)
      DEALLOCATE (GWFUZFDAT(Igrid)%IUZLIST)
      DEALLOCATE (GWFUZFDAT(Igrid)%NWAVST)
      DEALLOCATE (GWFUZFDAT(Igrid)%IUZHOLD)
      DEALLOCATE (GWFUZFDAT(Igrid)%LTRLIT)
      DEALLOCATE (GWFUZFDAT(Igrid)%LTRLST)
      DEALLOCATE (GWFUZFDAT(Igrid)%ITRLIT)
      DEALLOCATE (GWFUZFDAT(Igrid)%ITRLST)
      DEALLOCATE (GWFUZFDAT(Igrid)%TOTRUNOFF)
      DEALLOCATE (GWFUZFDAT(Igrid)%FBINS)
      DEALLOCATE (GWFUZFDAT(Igrid)%SEEPOUT)
      DEALLOCATE (GWFUZFDAT(Igrid)%EXCESPP)
      DEALLOCATE (GWFUZFDAT(Igrid)%VKS)
      DEALLOCATE (GWFUZFDAT(Igrid)%EPS)
      DEALLOCATE (GWFUZFDAT(Igrid)%THTS)
      DEALLOCATE (GWFUZFDAT(Igrid)%THTI)
      DEALLOCATE (GWFUZFDAT(Igrid)%PETRATE)
      DEALLOCATE (GWFUZFDAT(Igrid)%ROOTDPTH)
      DEALLOCATE (GWFUZFDAT(Igrid)%WCWILT)
      DEALLOCATE (GWFUZFDAT(Igrid)%FINF)
      DEALLOCATE (GWFUZFDAT(Igrid)%DELSTOR)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZOLSFLX)
      DEALLOCATE (GWFUZFDAT(Igrid)%HLDUZF)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZFETOUT)
      DEALLOCATE (GWFUZFDAT(Igrid)%GWET)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZTOTBAL)
      DEALLOCATE (GWFUZFDAT(Igrid)%CUMUZVOL)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZTSRAT)
      DEALLOCATE (GWFUZFDAT(Igrid)%THTR)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZFLWT)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZSTOR)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZDPIT)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZDPST)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZTHIT)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZTHST)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZSPIT)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZSPST)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZFLIT)
      DEALLOCATE (GWFUZFDAT(Igrid)%UZFLST)
      DEALLOCATE (GWFUZFDAT(Igrid)%REJ_INF)
      DEALLOCATE (GWFUZFDAT(Igrid)%TO_CFP)
      DEALLOCATE (GWFUZFDAT(Igrid)%SURFDEP)
      DEALLOCATE (GWFUZFDAT(Igrid)%SURFDEP1)
C
      END SUBROUTINE GWF2UZF1DA
C
C-------SUBROUTINE SGWF2UZF1PNT
      SUBROUTINE SGWF2UZF1PNT(Igrid)
C Set UZF pointers for grid.
      USE GWFUZFMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Igrid
C     ------------------------------------------------------------------
      NUZTOP=>GWFUZFDAT(Igrid)%NUZTOP
      IUZFOPT=>GWFUZFDAT(Igrid)%IUZFOPT
      IRUNFLG=>GWFUZFDAT(Igrid)%IRUNFLG
      IETFLG=>GWFUZFDAT(Igrid)%IETFLG
      IUZM=>GWFUZFDAT(Igrid)%IUZM
      IUZFCB1=>GWFUZFDAT(Igrid)%IUZFCB1
      IUZFCB2=>GWFUZFDAT(Igrid)%IUZFCB2
      NTRAIL=>GWFUZFDAT(Igrid)%NTRAIL
      NWAV=>GWFUZFDAT(Igrid)%NWAV
      NSETS=>GWFUZFDAT(Igrid)%NSETS
      IGSFLOW=>GWFUZFDAT(Igrid)%IGSFLOW
      NUZGAG=>GWFUZFDAT(Igrid)%NUZGAG
      NUZGAGAR=>GWFUZFDAT(Igrid)%NUZGAGAR
      NUZCL=>GWFUZFDAT(Igrid)%NUZCL
      NUZRW=>GWFUZFDAT(Igrid)%NUZRW
      ITRLSTH=>GWFUZFDAT(Igrid)%ITRLSTH
      IRUNBND=>GWFUZFDAT(Igrid)%IRUNBND
      IUZFBND=>GWFUZFDAT(Igrid)%IUZFBND
      IUZLIST=>GWFUZFDAT(Igrid)%IUZLIST
      NWAVST=>GWFUZFDAT(Igrid)%NWAVST
      IUZHOLD=>GWFUZFDAT(Igrid)%IUZHOLD
      LTRLIT=>GWFUZFDAT(Igrid)%LTRLIT
      LTRLST=>GWFUZFDAT(Igrid)%LTRLST
      ITRLIT=>GWFUZFDAT(Igrid)%ITRLIT
      ITRLST=>GWFUZFDAT(Igrid)%ITRLST
      TOTRUNOFF=>GWFUZFDAT(Igrid)%TOTRUNOFF
      FBINS=>GWFUZFDAT(Igrid)%FBINS
      SEEPOUT=>GWFUZFDAT(Igrid)%SEEPOUT
      EXCESPP=>GWFUZFDAT(Igrid)%EXCESPP
      VKS=>GWFUZFDAT(Igrid)%VKS
      EPS=>GWFUZFDAT(Igrid)%EPS
      THTS=>GWFUZFDAT(Igrid)%THTS
      THTI=>GWFUZFDAT(Igrid)%THTI
      PETRATE=>GWFUZFDAT(Igrid)%PETRATE
      ROOTDPTH=>GWFUZFDAT(Igrid)%ROOTDPTH
      WCWILT=>GWFUZFDAT(Igrid)%WCWILT
      FINF=>GWFUZFDAT(Igrid)%FINF
      DELSTOR=>GWFUZFDAT(Igrid)%DELSTOR
      UZOLSFLX=>GWFUZFDAT(Igrid)%UZOLSFLX
      HLDUZF=>GWFUZFDAT(Igrid)%HLDUZF
      UZFETOUT=>GWFUZFDAT(Igrid)%UZFETOUT
      GWET=>GWFUZFDAT(Igrid)%GWET
      UZTOTBAL=>GWFUZFDAT(Igrid)%UZTOTBAL
      CUMUZVOL=>GWFUZFDAT(Igrid)%CUMUZVOL
      UZTSRAT=>GWFUZFDAT(Igrid)%UZTSRAT     
      THTR=>GWFUZFDAT(Igrid)%THTR
      UZFLWT=>GWFUZFDAT(Igrid)%UZFLWT
      UZSTOR=>GWFUZFDAT(Igrid)%UZSTOR
      UZDPIT=>GWFUZFDAT(Igrid)%UZDPIT
      UZDPST=>GWFUZFDAT(Igrid)%UZDPST
      UZTHIT=>GWFUZFDAT(Igrid)%UZTHIT
      UZTHST=>GWFUZFDAT(Igrid)%UZTHST
      UZSPIT=>GWFUZFDAT(Igrid)%UZSPIT
      UZSPST=>GWFUZFDAT(Igrid)%UZSPST
      UZFLIT=>GWFUZFDAT(Igrid)%UZFLIT
      UZFLST=>GWFUZFDAT(Igrid)%UZFLST
      REJ_INF=>GWFUZFDAT(Igrid)%REJ_INF
      TO_CFP=>GWFUZFDAT(Igrid)%TO_CFP
      SURFDEP=>GWFUZFDAT(Igrid)%SURFDEP
      SURFDEP1=>GWFUZFDAT(Igrid)%SURFDEP1
C
      END SUBROUTINE SGWF2UZF1PNT
C
C-------SUBROUTINE SGWF2UZF1PSV
      SUBROUTINE SGWF2UZF1PSV(Igrid)
C Save UZF pointers for grid.
      USE GWFUZFMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Igrid
C     ------------------------------------------------------------------
      GWFUZFDAT(Igrid)%NUZTOP=>NUZTOP
      GWFUZFDAT(Igrid)%IUZFOPT=>IUZFOPT
      GWFUZFDAT(Igrid)%IRUNFLG=>IRUNFLG
      GWFUZFDAT(Igrid)%IETFLG=>IETFLG
      GWFUZFDAT(Igrid)%IUZM=>IUZM
      GWFUZFDAT(Igrid)%IUZFCB1=>IUZFCB1
      GWFUZFDAT(Igrid)%IUZFCB2=>IUZFCB2
      GWFUZFDAT(Igrid)%NTRAIL=>NTRAIL
      GWFUZFDAT(Igrid)%NWAV=>NWAV
      GWFUZFDAT(Igrid)%NSETS=>NSETS
      GWFUZFDAT(Igrid)%IGSFLOW=>IGSFLOW
      GWFUZFDAT(Igrid)%NUZGAG=>NUZGAG
      GWFUZFDAT(Igrid)%NUZGAGAR=>NUZGAGAR
      GWFUZFDAT(Igrid)%NUZCL=>NUZCL
      GWFUZFDAT(Igrid)%NUZRW=>NUZRW
      GWFUZFDAT(Igrid)%ITRLSTH=>ITRLSTH
      GWFUZFDAT(Igrid)%IRUNBND=>IRUNBND
      GWFUZFDAT(Igrid)%IUZFBND=>IUZFBND
      GWFUZFDAT(Igrid)%IUZLIST=>IUZLIST
      GWFUZFDAT(Igrid)%NWAVST=>NWAVST
      GWFUZFDAT(Igrid)%IUZHOLD=>IUZHOLD
      GWFUZFDAT(Igrid)%LTRLIT=>LTRLIT
      GWFUZFDAT(Igrid)%LTRLST=>LTRLST
      GWFUZFDAT(Igrid)%ITRLIT=>ITRLIT
      GWFUZFDAT(Igrid)%ITRLST=>ITRLST
      GWFUZFDAT(Igrid)%TOTRUNOFF=>TOTRUNOFF
      GWFUZFDAT(Igrid)%FBINS=>FBINS
      GWFUZFDAT(Igrid)%SEEPOUT=>SEEPOUT
      GWFUZFDAT(Igrid)%EXCESPP=>EXCESPP
      GWFUZFDAT(Igrid)%VKS=>VKS
      GWFUZFDAT(Igrid)%EPS=>EPS
      GWFUZFDAT(Igrid)%THTS=>THTS
      GWFUZFDAT(Igrid)%THTI=>THTI
      GWFUZFDAT(Igrid)%PETRATE=>PETRATE
      GWFUZFDAT(Igrid)%ROOTDPTH=>ROOTDPTH
      GWFUZFDAT(Igrid)%WCWILT=>WCWILT
      GWFUZFDAT(Igrid)%FINF=>FINF
      GWFUZFDAT(Igrid)%DELSTOR=>DELSTOR
      GWFUZFDAT(Igrid)%UZOLSFLX=>UZOLSFLX
      GWFUZFDAT(Igrid)%HLDUZF=>HLDUZF
      GWFUZFDAT(Igrid)%UZFETOUT=>UZFETOUT
      GWFUZFDAT(Igrid)%GWET=>GWET
      GWFUZFDAT(Igrid)%UZTOTBAL=>UZTOTBAL
      GWFUZFDAT(Igrid)%CUMUZVOL=>CUMUZVOL
      GWFUZFDAT(Igrid)%UZTSRAT=>UZTSRAT
      GWFUZFDAT(Igrid)%THTR=>THTR
      GWFUZFDAT(Igrid)%UZFLWT=>UZFLWT
      GWFUZFDAT(Igrid)%UZSTOR=>UZSTOR
      GWFUZFDAT(Igrid)%UZDPIT=>UZDPIT
      GWFUZFDAT(Igrid)%UZDPST=>UZDPST
      GWFUZFDAT(Igrid)%UZTHIT=>UZTHIT
      GWFUZFDAT(Igrid)%UZTHST=>UZTHST
      GWFUZFDAT(Igrid)%UZSPIT=>UZSPIT
      GWFUZFDAT(Igrid)%UZSPST=>UZSPST
      GWFUZFDAT(Igrid)%UZFLIT=>UZFLIT
      GWFUZFDAT(Igrid)%UZFLST=>UZFLST
      GWFUZFDAT(Igrid)%REJ_INF=>REJ_INF
      GWFUZFDAT(Igrid)%TO_CFP=>TO_CFP
      GWFUZFDAT(Igrid)%SURFDEP=>SURFDEP
      GWFUZFDAT(Igrid)%SURFDEP1=>SURFDEP1
C
      END SUBROUTINE SGWF2UZF1PSV

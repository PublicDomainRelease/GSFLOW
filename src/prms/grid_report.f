!***********************************************************************
!     Output PRMS variables by HRU in grid format
!***********************************************************************
      MODULE PRMS_GRID_REPORT
      IMPLICIT NONE
!   Module Variables
      INTEGER, SAVE :: Ngwcell, Nhrucell, Cellflg
      INTEGER, SAVE :: Endyr, Endmo, Endday, Lastyear, Totdays
      INTEGER, SAVE :: Yrdays, Yrreport, Totreport, Monreport, Mondays
      INTEGER, SAVE :: Begin_report, Begyr, Begmo, Begday, Nrow
      INTEGER, SAVE :: Prevyr, Prevmo, Prevday
      INTEGER, SAVE, ALLOCATABLE :: Totunit(:), Yrunit(:), Monunit(:)
      INTEGER, SAVE, ALLOCATABLE :: Nc_vars(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hruarea(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_tot(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_yr(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_mon(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Grid_var_mon(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Grid_var_yr(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Grid_var_tot(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Grid_var_cell(:)
      REAL, SAVE, ALLOCATABLE :: Grid_var(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Var_values(:)
      DOUBLE PRECISION, SAVE :: Conv_fac
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gvr_cell_pct_adjusted(:)
      CHARACTER(LEN=15), SAVE :: Gridfmt
      CHARACTER(LEN=24), SAVE, ALLOCATABLE :: Grid_var_name(:)
!   Declared Parameters
      INTEGER, SAVE :: Ncol, Prms_warmup
      INTEGER, SAVE :: Num_vars, Grid_output_type, Grid_units
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:), Gvr_hru_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
      END MODULE PRMS_GRID_REPORT

!     ******************************************************************
!     Gridded report module
!     ******************************************************************
      INTEGER FUNCTION grid_report()
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: prmsgrid_reportdecl, prmsgrid_reportinit
      INTEGER, EXTERNAL :: prmsgrid_reportclean, prmsgrid_reportrun
!***********************************************************************
      grid_report = 0

      IF ( Process_flag==0 ) THEN
        grid_report = prmsgrid_reportrun()
      ELSEIF ( Process_flag==1 ) THEN
        grid_report = prmsgrid_reportdecl()
      ELSEIF ( Process_flag==2 ) THEN
        grid_report = prmsgrid_reportinit()
      ELSEIF ( Process_flag==3 ) THEN
        grid_report = prmsgrid_reportclean()
      ENDIF

      END FUNCTION grid_report

!***********************************************************************
!     prmsgrid_reportdecl - declare parameters and variables
!***********************************************************************
      INTEGER FUNCTION prmsgrid_reportdecl()
      USE PRMS_GRID_REPORT
      USE PRMS_MODULE, ONLY: Model
      USE PRMS_BASIN, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declmodule, declparam, getdim
      INTEGER :: jj
!***********************************************************************
      prmsgrid_reportdecl = 1

      IF ( declmodule(
     +'$Id: grid_report.f 2535 2011-03-02 17:31:06Z rsregan $')
     +     .NE.0 ) RETURN

      Num_vars = 1  ! make general later
      ALLOCATE ( Grid_var_name(Num_vars), Nc_vars(Num_vars) )
      Grid_var_name = ''
      DO jj = 1, Num_vars
        Grid_var_name(jj) = 'recharge'  ! make general later
        Nc_vars(jj) = INDEX(Grid_var_name(jj),' ') - 1
      ENDDO
      ALLOCATE (Grid_var(Nhru, Num_vars))

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell==-1 ) RETURN
      Cellflg = 1
      IF ( Nhru/=Ngwcell .AND. Ngwcell/=0 ) Cellflg = 0

! Declared Parameters
      IF ( declparam('grid_report', 'grid_output_type', 'one','integer',
     +     '0', '0', '5',
     +     'Gridded output type',
     +     'Gridded output type (0=none; 1=monthly;'//
     +     ' 2=yearly; 3=total; 4=monthly and yearly; 5=all)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('grid_report', 'grid_units', 'one', 'integer',
     +     '0', '0', '3',
     +     'Units of gridded output results',
     +     'Units of gridded output results'//
     +     ' (0=inches/day; 1=feet/day; 2=cm/day; 3=meters/day)',
     +     'none').NE.0 ) RETURN

      IF ( declparam('grid_report', 'prms_warmup', 'one', 'integer',
     +     '1', '0', '12',
     +     'Number of years PRMS warmups before writing gridded output',
     +     'Number of years PRMS warmups before writing gridded output',
     +     'years').NE.0 ) RETURN

      IF ( declparam('grid_report', 'ncol', 'one', 'integer',
     +     '1', '1', '50000',
     +     'Number of columns for each row of the gridded output',
     +     'Number of columns for each row of the gridded output',
     +     'none').NE.0 ) RETURN

      IF ( Cellflg==0 .OR. Model==99 ) THEN
        Nhrucell = getdim('nhrucell')
        IF ( Nhrucell==-1 ) RETURN

        ALLOCATE (Gvr_cell_id(Nhrucell))
        IF ( declparam('gsfconv', 'gvr_cell_id', 'nhrucell', 'integer',
     +       '0', 'bounded', 'ngwcell',
     +     'Corresponding MODFLOW cell id of each GVR',
     +     'Index of the MODFLOW cell associated with each gravity'//
     +     ' reservoir',
     +     'none').NE.0 ) RETURN

        ALLOCATE (Gvr_cell_pct(Nhrucell))
        IF ( declparam('gsfconv', 'gvr_cell_pct', 'nhrucell', 'real',
     +       '0.0', '0.0', '1.0',
     +       'Proportion of the MODFLOW cell associated with each GVR',
     +       'Proportion of the MODFLOW cell area associated with'//
     +       ' each gravity reservoir',
     +       'decimal fraction').NE.0 ) RETURN 

        ALLOCATE (Gvr_hru_id(Nhrucell))
        IF ( declparam('prms2mf', 'gvr_hru_id', 'nhrucell', 'integer',
     +       '1', 'bounded', 'nhru',
     +       'Corresponding HRU id of each GVR',
     +       'Index of the HRU assocated with each gravity reservoir',
     +       'none').NE.0 ) RETURN
      ENDIF

      prmsgrid_reportdecl = 0

      END FUNCTION prmsgrid_reportdecl

!***********************************************************************
!     prmsgrid_reportinit - Initialize prmsgrid_report module - get parameter values
!***********************************************************************
      INTEGER FUNCTION prmsgrid_reportinit()
      USE PRMS_GRID_REPORT
      USE PRMS_BASIN, ONLY: Hru_area, Starttime, Endtime, Nhru
      IMPLICIT NONE
      INTRINSIC DBLE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, jj, is
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: cell_pct, temp_pct
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: newpct
!***********************************************************************
      prmsgrid_reportinit = 1

      IF ( getparam('grid_report', 'grid_output_type', 1, 'integer',
     +     Grid_output_type).NE.0 ) RETURN
      IF ( Grid_output_type==0 ) THEN
        prmsgrid_reportinit = 0
        RETURN
      ENDIF

      IF ( getparam('grid_report', 'ncol', 1, 'integer', Ncol)
     +     .NE.0 ) RETURN
      WRITE (Gridfmt, 9001) Ncol

      IF ( Cellflg==0 ) THEN
        Nrow = Ngwcell/Ncol
        ALLOCATE ( Grid_var_cell(Ngwcell) )
        IF ( getparam('grid_report', 'gvr_cell_id', Nhrucell, 'integer',
     +       Gvr_cell_id).NE.0 ) RETURN

        IF ( getparam('grid_report', 'gvr_cell_pct', Nhrucell, 'real',
     +       Gvr_cell_pct).NE.0 ) RETURN

        IF ( getparam('grid_report', 'gvr_hru_id', Nhrucell, 'integer',
     +       Gvr_hru_id).NE.0 ) RETURN

        ALLOCATE (temp_pct(Nhrucell), cell_pct(Ngwcell))
        cell_pct = 0.0D0
        DO i = 1, Nhrucell
          temp_pct(i) = DBLE(Gvr_cell_pct(i))
          is = Gvr_cell_id(i)
          cell_pct(is) = cell_pct(is) + temp_pct(i)
        ENDDO
        ALLOCATE (Gvr_cell_pct_adjusted(Nhrucell), newpct(Ngwcell))

        newpct = 0.0D0
        DO i = 1, Nhrucell
          is = Gvr_cell_id(i)
          temp_pct(i) = temp_pct(i) +
     +                  temp_pct(i)*(1.0D0-cell_pct(is))/cell_pct(is)
          Gvr_cell_pct_adjusted(i) = temp_pct(i)
          newpct(is) = newpct(is) + temp_pct(i)
        ENDDO

        DO i = 1, Ngwcell
          IF ( newpct(i)>0.0D0 ) THEN
            IF ( ABS(newpct(i)-1.0D0)>0.000005 ) PRINT *,
     +           'Possible issue with GVR to Cell precentage, Cell:', i,
     +           newpct(i)
          ENDIF
        ENDDO
        DEALLOCATE (temp_pct, cell_pct, newpct)
      ELSE
        Nrow = Nhru/Ncol
      ENDIF

      ALLOCATE ( Hruarea(Nhru) )
      DO i = 1, Nhru
        Hruarea(i) = DBLE(Hru_area(i))
      ENDDO

      Monreport = 0
      Yrreport = 0
      Totreport = 0

      IF ( Grid_output_type==1 .OR. Grid_output_type>3 ) THEN
        Monreport = 1
        ALLOCATE ( Grid_var_mon(Nhru,Num_vars) )
        ALLOCATE ( Basin_var_mon(Num_vars) )
        Grid_var_mon = 0.0D0
        Basin_var_mon = 0.0D0
        ALLOCATE ( Monunit(Num_vars) )
        DO jj = 1, Num_vars
          Monunit(jj) = 383 + 1
          OPEN (Monunit(jj), FILE=Grid_var_name(jj)(:Nc_vars(jj))
     +                            //'.monthy')
        ENDDO
      ENDIF

      IF ( Grid_output_type==2 .OR. Grid_output_type==5 ) THEN
        Yrreport = 1
        ALLOCATE ( Grid_var_yr(Nhru,Num_vars) )
        ALLOCATE ( Basin_var_yr(Num_vars) )
        Grid_var_yr = 0.0D0
        Basin_var_yr = 0.0D0
        ALLOCATE ( Yrunit(Num_vars) )
        DO jj = 1, Num_vars
          Yrunit(jj) = 483 + 1
          OPEN( Yrunit(jj), FILE=Grid_var_name(jj)(:Nc_vars(jj))
     +                           //'.yearly')
        ENDDO
      ENDIF

      IF ( Grid_output_type==3 .OR. Grid_output_type==5 ) THEN
        Totreport = 1
        ALLOCATE ( Grid_var_tot(Nhru,Num_vars) )
        ALLOCATE ( Basin_var_tot(Num_vars) )
        Grid_var_tot = 0.0D0
        Basin_var_tot = 0.0D0
        ALLOCATE ( Totunit(Num_vars) )
        DO jj = 1, Num_vars
          Totunit(jj) = 583 + 1
          OPEN (Totunit(jj), FILE=Grid_var_name(jj)(:Nc_vars(jj))
     +                       //'.total')
        ENDDO
      ENDIF

      IF ( getparam('grid_report', 'grid_units', 1, 'integer',
     +     Grid_units).NE.0 ) RETURN
      IF ( Grid_units==0 ) THEN
        Conv_fac = 1.0D0
      ELSEIF ( Grid_units==1 ) THEN
        Conv_fac = 1.0D0/12.0D0
      ELSEIF ( Grid_units==2 ) THEN
        Conv_fac = 2.54D0
      ELSE
        Conv_fac = 0.0254D0
      ENDIF

      IF ( getparam('grid_report', 'prms_warmup', 1, 'integer',
     +     Prms_warmup).NE.0 ) RETURN

      IF ( Prms_warmup>0 ) THEN
        Begin_report = 0
      ELSE
        Begin_report = 1
      ENDIF
      Begyr = Starttime(1) + Prms_warmup
      Begmo = Starttime(2)
      Begday = Starttime(3)
      Endyr = Endtime(1)
      IF ( Begyr>Endyr ) THEN
        PRINT *, 'ERROR, invalid prms_warmup value'
        PRINT *, '   warmup period longer than simulation time period'
        RETURN
      ENDIF
      Endmo = Endtime(2)
      Endday = Endtime(3)
      Lastyear = Begyr
      Totdays = 0
      Mondays = 0
      Yrdays = 0
      Prevyr = Begyr
      Prevmo = Begmo
      Prevday = Begday

      prmsgrid_reportinit = 0

 9001 FORMAT ('(',I8,'E10.3)')

      END FUNCTION prmsgrid_reportinit

!***********************************************************************
!     prmsgrid_reportrun - Outputs various PRMS variables in grid format
!***********************************************************************
      INTEGER FUNCTION prmsgrid_reportrun()
      USE PRMS_GRID_REPORT
      USE PRMS_BASIN, ONLY: Nhru, Active_hrus, Hru_route_order,
     +    Basin_area_inv
      USE PRMS_OBS, ONLY: Nowtime, Modays
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: getvar
! Local Variables
      INTEGER :: j, i, k, month, day, year, jj
      INTEGER :: ihru, icell, last_day
! Local static variables
      DOUBLE PRECISION factor
!***********************************************************************
      prmsgrid_reportrun = 0
      IF ( Grid_output_type==0 ) RETURN

      year = Nowtime(1)
      month = Nowtime(2)
      day = Nowtime(3)
      IF ( Begin_report==0 ) THEN
        IF ( year==Begyr .AND. month==Begmo .AND. day==Begday ) THEN
          Begin_report = 1
        ELSE
          RETURN
        ENDIF
      ENDIF

! check for last day of simulation
      last_day = 0
      IF ( year==Endyr ) THEN
        IF ( month==Endmo ) THEN
          IF ( day==Endday ) THEN
            last_day = 1
            Prevyr = year
            Prevmo = month
            Prevday = day
          ENDIF
        ENDIF
      ENDIF

      prmsgrid_reportrun = 1

      IF ( Yrreport==1 ) THEN
! check for first time step of the next year
        IF ( Lastyear/=year .OR. last_day==1 ) THEN
          IF ( (month==Begmo .AND. day==Begday) .OR. last_day==1 ) THEN
            Lastyear = year
            factor = Conv_fac/Yrdays
            Basin_var_yr = 0.0D0
            DO jj = 1, Num_vars
              DO j = 1, Active_hrus
                i = Hru_route_order(j)
                Grid_var_yr(i, jj) = Grid_var_yr(i, jj)*factor
                Basin_var_yr(jj) = Basin_var_yr(jj) +
     +                             Grid_var_yr(i, jj)*Hruarea(i)
              ENDDO
              Basin_var_yr(jj) = Basin_var_yr(jj)*Basin_area_inv

              WRITE (Yrunit(jj), 9002) Prevyr, Prevmo, Prevday,
     +                           ' Basin yearly mean:', Basin_var_yr(jj)

              IF ( Cellflg==1 ) THEN
                k = 1
                DO i = 1, Nrow
                  WRITE (Yrunit(jj), Gridfmt)
     +                  (Grid_var_yr(j,jj),j=k,k+Ncol-1)
                  k = k + Ncol
                ENDDO
              ELSE
                Grid_var_cell = 0.0D0
                DO k = 1, Nhrucell
                  ihru = Gvr_hru_id(k)
                  icell = Gvr_cell_id(k)
                  Grid_var_cell(icell) = Grid_var_cell(icell)
     +                   + Grid_var_yr(ihru,jj)*Gvr_cell_pct_adjusted(k)
                ENDDO
                k = 1
                DO i = 1, Nrow
                  WRITE (Yrunit(jj), Gridfmt)
     +                  (Grid_var_cell(j),j=k,k+Ncol-1)
                  k = k + Ncol
                ENDDO
              ENDIF
              WRITE (Yrunit(jj), 9003)
            ENDDO
            Grid_var_yr = 0.0D0
            Yrdays = 0
          ENDIF
        ENDIF
        Yrdays = Yrdays + 1
        Prevyr = year
        Prevmo = month
        Prevday = day
      ENDIF

!-----------------------------------------------------------------------
! need getvars for each variable (only can have short string)
      DO jj = 1, Num_vars
        IF ( getvar('grid_report',Grid_var_name(jj)(:Nc_vars(jj)),
     +              Nhru, 'real', Grid_var(1, jj)).NE.0 ) RETURN
      ENDDO

      DO jj = 1, Num_vars
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Totreport==1 ) Grid_var_tot(i, jj) =
     +                             Grid_var_tot(i, jj) + Grid_var(i, jj)
          IF ( Yrreport==1 ) Grid_var_yr(i,jj) =
     +                             Grid_var_yr(i, jj) + Grid_var(i, jj)
          IF ( Monreport==1 ) Grid_var_mon(i,jj) =
     +                             Grid_var_mon(i, jj) + Grid_var(i, jj)
        ENDDO
      ENDDO

      IF ( Monreport==1 ) THEN
        Mondays = Mondays + 1
! check for last day of current month
        IF ( day==modays(month) .OR. last_day==1 ) THEN
          factor = Conv_fac/Mondays
          Basin_var_mon = 0.0D0
          DO jj = 1, Num_vars
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Grid_var_mon(i, jj) = Grid_var_mon(i, jj)*factor
              Basin_var_mon(jj) = Basin_var_mon(jj) +
     +                            Grid_var_mon(i, jj)*Hruarea(i)
            ENDDO
            Basin_var_mon(jj) = Basin_var_mon(jj)*Basin_area_inv

            WRITE (Monunit(jj), 9002) year, month, day,
     +            ' Basin monthly mean:', Basin_var_mon(jj)
            IF ( Cellflg==1 ) THEN
              k = 1
              DO i = 1, Nrow
                WRITE (Monunit(jj), Gridfmt)
     +                (Grid_var_mon(j,jj),j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ELSE
              Grid_var_cell = 0.0D0
              DO k = 1, Nhrucell
                ihru = Gvr_hru_id(k)
                icell = Gvr_cell_id(k)
                Grid_var_cell(icell) = Grid_var_cell(icell)
     +                  + Grid_var_mon(ihru,jj)*Gvr_cell_pct_adjusted(k)
              ENDDO
              k = 1
              DO i = 1, Nrow
                WRITE (Monunit(jj), Gridfmt)
     +                (Grid_var_cell(j),j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ENDIF
            WRITE (Monunit(jj), 9003)
          ENDDO
          Grid_var_mon = 0.0D0
          Mondays = 0
        ENDIF
      ENDIF

      IF ( Totreport==1 ) THEN
        Totdays = Totdays + 1
! check for last day of simulation
        IF ( last_day==1 ) THEN
          factor = Conv_fac/Totdays
          Basin_var_tot = 0.0D0
          DO jj = 1, Num_vars
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Grid_var_tot(i, jj) = Grid_var_tot(i, jj)*factor
              Basin_var_tot(jj) = Basin_var_tot(jj) +
     +                            Grid_var_tot(i, jj)*Hruarea(i)
            ENDDO
            Basin_var_tot(jj) = Basin_var_tot(jj)*Basin_area_inv

            WRITE (Totunit(jj), 9002) year, month, day,
     +                      ' Basin simulation mean:', Basin_var_tot(jj)

            IF ( Cellflg==1 ) THEN
              k = 1
              DO i = 1, Nrow
                WRITE (Totunit(jj), Gridfmt)
     +                              (Grid_var_tot(j,jj),j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ELSE
              Grid_var_cell = 0.0D0
              DO k = 1, Nhrucell
                ihru = Gvr_hru_id(k)
                icell = Gvr_cell_id(k)
                Grid_var_cell(icell) = Grid_var_cell(icell) +
     +                    Grid_var_tot(ihru,jj)*Gvr_cell_pct_adjusted(k)
              ENDDO
              k = 1
              DO i = 1, Nrow
                WRITE (Totunit(jj), Gridfmt)
     +                (Grid_var_cell(j),j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF

 9002 FORMAT ('DATE:', I5, 2('/',I2.2), A, E12.3)
 9003 FORMAT ('######################', /)
      prmsgrid_reportrun = 0

      END FUNCTION prmsgrid_reportrun

!***********************************************************************
!     prmsgrid_reportclean - close files
!***********************************************************************
      INTEGER FUNCTION prmsgrid_reportclean()
      USE PRMS_GRID_REPORT, ONLY: Totreport, Monreport, Yrreport,
     +    Totunit, Monunit, Yrunit, Num_vars
      IMPLICIT NONE
      INTEGER :: jj
!***********************************************************************
      IF ( Totreport==1 ) THEN
        DO jj = 1, Num_vars
          CLOSE (Totunit(jj))
        ENDDO
      ENDIF
      IF ( Monreport==1 ) THEN
        DO jj = 1, Num_vars
          CLOSE (Monunit(jj))
        ENDDO
      ENDIF
      IF ( Yrreport==1 ) THEN
        DO jj = 1, Num_vars
          CLOSE (Yrunit(jj))
        ENDDO
      ENDIF
      prmsgrid_reportclean = 0
      END FUNCTION prmsgrid_reportclean

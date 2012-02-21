module PCG_MAIN
  ! ... FRONT END FOR PCG SOLVER
  ! ... last modified: R. L. Naff, July 2007
  ! ... 
  ! ... Calls subroutines in module pcg_solve
  ! ... 
  ! ... Preconditioner is modified incomplete cholesky for 5 or 7 point 
  ! ... stencil.
  ! ... 
  ! ... solve Ax=b
  ! ... A(nxn), x(nx1), b(nx1); A symmetric
  ! ... A is assumed to be be stored in compressed diagonal scheme.
  ! ... Only diagonals of upper triangle stored.
  ! ... Storage is row ordered, smallest row number to largest.
  ! ...
  ! ... Compressed diagonal storage arrays:
  ! ... 
  ! ... dd: diagonal column of A.
  ! ... dx: first off-diagonal column of a corresponing to x connection.
  ! ... dy: second off-diagonal column of a corresponing to y connection.
  ! ... dz: third off-diagonal column of a corresponing to z connection.
  ! ... 
  ! ... nx, ny, nz: x, y and z cell dimensions of grid
  ! ...
  ! ... PCG arrays:
  ! ... xx(:): initial guess for solution vector (zero if unspecified).
  ! ... rhs(:): right-hand-side vector, b.
  ! ... If xx present, solution vector returned in xx; 
  ! ... otherwise it is returned in rhs.
  ! ...
  ! ... Dimensions:
  ! ... dd, xx, rhs:  nx*ny*nz
  ! ... dx: nx*ny*nz if nx>1; otherwise 1
  ! ... dy: nx*ny*nz if ny>1; otherwise 1
  ! ... dz: nx*ny*nz if nz>1; otherwise 1
  ! ...
  ! ... Options:
  ! ...   Relaxation parameter: 0<=omega<=1
  ! ...   Incomplete Cholesky precondition with either 0 or 1 level of fill.
  ! ...
  ! ... See headers of modules pcg_solve and MiUDU for error messages 
  ! ... and warnings.
  ! ...
  ! ... err_stat and/or error codes
  ! ...   10: allocation error
  ! ...   15: pivot problem
  ! ...   20: dimension mismatch
  ! ...
  ! ... R.L. Naff
  ! ...
  ! ... version 1.0, 11/2007
  ! ...
  use common_parameters
  use common_solver_types
  ! ... common_solver_types: DD, DX, DY, DZ, dim, nx, ny, nz, n_rows
  use MiUDU
  use mat_vec_mult
  use pcg_solve
  implicit none
  integer, save :: fill_level, x_flag, pc_unit, ts_unit
  real(kind=kv), save :: omega, eps=small, eps_t
  character(len=32), dimension(:), pointer :: marker_name
  private; public :: PCG, PCG_init, PCG_fin
contains

  subroutine PCG_init(fill, n_x, n_y, n_z, d_d, d_x, d_y, d_z, &
       relax, flag_x, unit_ts, unit_pc, mrkr_name, err_stat)
    ! ... 
    ! ... Initialize PCG solver
    ! ... 
    ! ... fill: level of fill for iUDU preconditioner (0,1)
    ! ... fill_level=fill
    ! ... 
    ! ... n_x, n_y, n_z: x, y and z cell dimensions of grid
    ! ... 
    ! ... d_d: diagonal column of A.
    ! ... d_x: first off-diagonal column of a corresponing to x connection.
    ! ... d_y: second off-diagonal column of a corresponing to y connection.
    ! ... d_z: third off-diagonal column of a corresponing to z connection.
    ! ... 
    ! ... relax: relaxation parameter (0.0<=relax<=1.0); optional.
    ! ... omega=relax
    ! ... 
    ! ... flag_x=1: call CG_iter
    ! ... flag_x=2: call CG_iter_xt
    ! ... x_flag=flag_x
    ! ... 
    ! ... unit_ts>0: obtain and print elapsed time in solver.
    ! ... unit_ps>0: print convergence of inner iteration (CG_iter).
    ! ... ts_unit=unit_ts and pc_unit=unit_pc
    ! ... 
    ! ... mrkr_name(3): Allows up to three marker names corresponding to 
    ! ...                 markers; optional.
    ! ... 
    ! ... err_stat(2): returns error number plus info; optional.
    ! ... 
    ! ... argument list
    ! ... 
    integer, intent(in) :: fill, n_x, n_y, n_z, flag_x, unit_ts, unit_pc
    character(len=32), dimension(1:3), optional, intent(in), target :: &
         mrkr_name
    real(kind=kv), optional, intent(in) :: relax
    real(kind=kv), dimension(:), intent(in), target :: d_d, d_x, &
         d_y, d_z
    integer, dimension(1:2), intent(inout), optional :: err_stat
    ! ... 
    ! ... local variables
    ! ... 
    integer :: i, error
    ! ... 
    ! .....................................................................
    ! ... 
    error=0; n_rows=n_x*n_y*n_z
    ts_unit=unit_ts; pc_unit=unit_pc; x_flag=flag_x
    if (present(err_stat)) err_stat=0
    if (present(mrkr_name)) marker_name=>mrkr_name
    ! ...
    ! ... rotate and set dimensions and vectors
    ! ...
    dd=>d_d
    if (n_x>1.and.n_y>1.and.n_z>1) then
       dim=3
       nx=n_x; ny=n_y; nz=n_z
       dx=>d_x; dy=>d_y; dz=>d_z
    elseif (n_x>1.and.n_y>1.or.n_y>1.and.n_z>1.or.n_x>1.and.n_z>1) then
       dim=2
       if (n_z==1) then
          nx=n_x; ny=n_y; nz=n_z
          dx=>d_x; dy=>d_y; dz=>d_z
       elseif (n_y==1) then
          nx=n_x; ny=n_z; nz=n_y
          dx=>d_x; dy=>d_z; dz=>d_y
       else ! n_x=1
          nx=n_y; ny=n_z; nz=n_x
          dx=>d_y; dy=>d_z; dz=>d_x
       endif
    else
       dim=1
       if (n_x>1) then
          nx=n_x; ny=n_y; nz=n_z
          dx=>d_x; dy=>d_y; dz=>d_z
       elseif (n_y>1) then
          nx=n_y; ny=n_z; nz=n_x
          dx=>d_y; dy=>d_z; dz=>d_x
       else ! n_z>1          
          nx=n_z; ny=n_x; nz=n_y
          dx=>d_z; dy=>d_y; dz=>d_x
       endif
    endif
    ! ...
    ! ... Allocate arrays per fill level
    ! ...
    fill_level=fill
    allocate (diag(1:n_rows), stat=error)
    if (error/=0) then
       if (present(err_stat)) then
          err_stat=(/10,error/); return
       else
          write(*,500)
          call pcg_fin; stop
       endif
    endif
    if (fill_level==1) then
       select case(dim)
       case(3)
          ! ... 3-D
          allocate (dx0(1:n_rows),dy0(1:n_rows),dx1(1:n_rows), &
               dy1(1:n_rows),dz1(1:n_rows), stat=error)
          if (error/=0) then
             if (present(err_stat)) then
                err_stat=(/10,error/); return
             else
                write(*,500)
                call pcg_fin; stop
             endif
          endif
          dz0=>dz
          dx0=n0; dy0=n0; dx1=n0; dy1=n0; dz1=n0
       case(2)
          ! ... 2-D
          allocate (dx0(1:n_rows),dx1(1:n_rows), stat=error)
          if (error/=0) then
             if (present(err_stat)) then
                err_stat=(/10,error/); return
             else
                write(*,500)
                call pcg_fin; stop
             endif
          endif
          dy0=>dy
          dx0=n0; dx1=n0
       case(1)
          ! ... 1-D
          dx0=>dx
       end select
    endif
    ! ...
    ! ... relaxation parameter
    ! ... 
    if (present(relax)) then
       omega=relax
    else
       omega=0.99_kv
    endif
    ! ...
500 format(1x,'Allocation error in subroutine PCG_init')
    ! ...
  end subroutine PCG_init

  subroutine PCG(resid, convg_i, xx, max_iter, c_flag, convg_o, marker, &
       err_stat)
    ! ... 
    ! ... main entry point for pcg algorithm
    ! ... 
    ! ... resid: residual vector; return for solution if xx not present.
    ! ... xx: initial guess and solution vector; optional.
    ! ... 
    ! ... convg_i(in): convergence criterion, inner iteration.
    ! ... convg_i(out): L2 norm at convergence, inner iteration.
    ! ... max_iter(in): maximum allowed iterations; optional.
    ! ... max_iter(out): iterations required to convergence; optional.
    ! ...
    ! ... c_flag=1: use absolute convergence
    ! ... c_flag=2: use relative convergence
    ! ...
    ! ... convg_o(in): adaptive relative convergence flag for 
    ! ... extended cg algorithm
    ! ... convg_o(out): returns weighted L2 norm at entry to cg loop
    ! ... convg_o should be returned in conjunction with c_flag>1, but 
    ! ...    applies principally to the standard cg algorithm.
    ! ...
    ! ... marker(3): Allows up to three integer markers to be placed in 
    ! ...            ts_unit and/or pc_unit files; optional.
    ! ... 
    ! ... err_stat(2): returns error number plus info; optional.
    ! ... 
    ! ... argument list
    ! ... 
    real(kind=kv), dimension(:), intent(inout) :: resid
    real(kind=kv), intent(inout) :: convg_i
    real(kind=kv), dimension(:), optional, intent(inout), target :: xx
    integer, intent(inout), optional :: max_iter
    integer, intent(in), optional :: c_flag
    real(kind=kv), intent(inout), optional :: convg_o
    integer, dimension(1:3), optional, intent(in) :: marker
    integer, dimension(1:2), intent(inout), optional :: err_stat
    ! ... 
    ! ... local variables
    ! ... 
    integer :: ii, jj, kk
    real(kind=kv) :: eps_i, eps_o
    integer :: error, iter_max, iter_no, i, start_time, end_time, solv_time
    integer :: cum_time=0
    integer, external :: elapsed_time
    real(kind=kv), dimension(:), pointer :: X
    ! ... 
    ! .....................................................................
    ! ... 
    nullify (X)
    error=0; iter_no=0
    if (present(err_stat)) err_stat=0
    ! ... 
    ! ... initial array size checking
    ! ... 
    if (size(resid)/=size(dd)) then
       error=20
    elseif (present(xx)) then
       if (size(xx)/=size(dd)) error=20
    endif
    if (error/=0) then
       if (present(err_stat)) then
          err_stat=(/error,0/)
          return
       else
          write(*,520)
          call pcg_fin; stop
       endif
    endif
    ! ... 
    ! ... set diagonal per fill level
    ! ...
    if (fill_level==0) then
       call diag_factor_0(omega,error)
    else
       call diag_factor_1(omega,error)
    endif
    ! ... 
    if (error/=0) then
       call pcg_fin
       if (present(err_stat)) then
          err_stat=(/15,error/)
          return
       else
          if (error>0) then
             ii=(mod(error,nx*ny)-1)/ny+1
             jj=mod(mod(error,nx*ny)-1,ny)+1
             kk=(error-1)/(nx*ny)+1
             write(*,500) jj, ii, kk
          else
             ! ... negative error indicates negative pivot
             error=-error
             ii=(mod(error,nx*ny)-1)/ny+1
             jj=mod(mod(error,nx*ny)-1,ny)+1
             kk=(error-1)/(nx*ny)+1
             write(*,505) jj, ii, kk
          endif
          call pcg_fin; stop
       endif
    endif
    ! ... 
    if (present(max_iter)) then
       iter_max=max_iter
    else
       iter_max=max_iter_approx()
    end if
    ! ... print markers for time and/or progress files
    ! ... NOTE: Calling program should OPEN ts_unit and/or pc_unit
    if (ts_unit>0) call marker_print(ts_unit, marker, marker_name)
    if (pc_unit>0) call marker_print(pc_unit, marker, marker_name)
    ! ...
    ! ... solve Ax=rhs by pcg
    ! ...
    if (ts_unit>0) start_time=elapsed_time(2)
    if (present(xx)) then
       X=>xx
    else
       allocate (X(1:n_rows), stat=error)
       if (error/=0) then
          if (present(err_stat)) then
             err_stat=(/10,error/); return
          else
             write(*,515)
             call pcg_fin; stop
          endif
       endif
       X=n0
    endif
    ! ...
    ! ... adaptive relative convergence flag and threshold
    ! ...
    if (present(convg_o).and.x_flag==1.and.c_flag/=1) then
       eps_o=convg_o
    else
       ! ... turn off adaptive convergence
       eps_o=-n1
    endif
    ! ...
    allocate (x_dev(1:n_rows), r_dev(1:n_rows), stat=error)
    if (error/=0) then
       if (present(err_stat)) then
          err_stat=(/10,error/); return
       else
          write(*,515)
          call pcg_fin; stop
       endif
    endif
    ! ...
    if (x_flag==1) then 
       ! ...  standard cg algorithm 
       if (c_flag==1) then
          ! ... abs convergence
          eps_i=convg_i**2
          if (fill_level==0) then
             call CG_iter(iUDU_solve_0, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o, pc_unit)
          else
             call CG_iter(iUDU_solve_1, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o,  pc_unit)
          endif
          convg_i=sqrt(eps_i)
       else ! c_flag>1
          ! ...  rel convergence
          if (fill_level==0) then
             call CG_iter(iUDU_solve_0, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o,  pc_unit)
          else
             call CG_iter(iUDU_solve_1, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o,  pc_unit)
          endif
       endif
    else ! x_flag>1
       ! ...  extended cg algorithm 
       if (c_flag==1) then
          ! ... abs convergence
          eps_i=convg_i**2
          if (fill_level==0) then
             call CG_iter_xt(iUDU_solve_0, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o, pc_unit)
          else
             call CG_iter_xt(iUDU_solve_1, abs_convg, X, resid, iter_max, &
                  eps_i, eps_o, pc_unit)
          endif
          convg_i=sqrt(eps_i)
       else ! c_flag>1    
          ! ...  rel convergence
          if (fill_level==0) then
             call CG_iter_xt(iUDU_solve_0, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o, pc_unit)
          else
             call CG_iter_xt(iUDU_solve_1, rel_convg, X, resid, iter_max, &
                  convg_i, eps_o, pc_unit)
          endif
       endif
    endif
    ! ...
    if (present(convg_o)) convg_o=eps_o
    deallocate (x_dev,r_dev)
    ! ...
    if (present(max_iter)) max_iter=iter_max
    if (ts_unit>0) then
       solv_time=elapsed_time(2)-start_time
       cum_time=cum_time+solv_time
       write(unit=ts_unit,fmt=510) iter_max, real(solv_time)/1000., &
            real(cum_time)/1000. 
    endif
    if (.not.present(xx)) then
       resid=X
       deallocate(X)
    else
       nullify(X)
    endif
    ! ...
500 format(1x,'ARRAY DIAG CONTAINS A ZERO ELEMENT AT NODE',/, &
         1x,'mesh location: j=',i4,', i=',i4,', k=',i4,/, &
         1x,'***EXECUTION TERMINATED***')
505 format(1x,'ARRAY DIAG CONTAINS A NEGATIVE ELEMENT AT NODE',/, &
         1x,'mesh location: j=',i4,', i=',i4,', k=',i4,/, &
         1x,'***EXECUTION TERMINATED***')
510 format (2x,i6,' iterations required ',g12.5,' sec exection time',/, &
         2x,'cumulative time in solver:',g12.5,' sec')
515 format(1x,'Allocation error in subroutine PCG')
520 format(1x,'Dimension mismatch in subroutine PCG')
    ! ...
  end subroutine PCG

  subroutine PCG_fin
    ! ... 
    ! ... deallocate arrays, nullify pointers
    ! ... 
    if (associated(diag)) deallocate(diag)
    nullify (dd, dx, dy, dz)
    if (fill_level==1) then
       select case(dim)
       case(3)
          if (associated(dx0)) deallocate(dx0)
          if (associated(dy0)) deallocate(dy0)
          if (associated(dx1)) deallocate(dx1)
          if (associated(dy1)) deallocate(dy1)
          if (associated(dz1)) deallocate(dz1)
          nullify (dz0)
       case(2)
          if (associated(dx0)) deallocate (dx0)
          if (associated(dx1)) deallocate (dx1)
          nullify (dy0)
       case(1)
          nullify (dx0)
       end select
    endif
    if (ts_unit>0) close(unit=ts_unit)
    if (pc_unit>0) close(unit=pc_unit)
    ! ... 
  end subroutine PCG_fin

  subroutine marker_print(unit_no, marker, marker_name)
    ! ... Print markers for time and/or progress files.
    ! ... Only prints markers when marker variable has 
    ! ... non-zero content; see below.
    ! ... 
    ! ... argument list
    ! ... 
    integer, intent(in) :: unit_no
    integer, dimension(1:3), intent(in) :: marker
    character(len=32), dimension(1:3), intent(in) :: marker_name
    ! ... 
    if (marker(1)>0.and.marker(2)>0.and.marker(3)>0) then
       write (unit=unit_no, fmt=500) trim(marker_name(1)), marker(1), &
            trim(marker_name(2)), marker(2), trim(marker_name(3)), marker(3)
    elseif (marker(1)>0.and.marker(2)>0) then
       write (unit=unit_no, fmt=505) trim(marker_name(1)), marker(1), &
            trim(marker_name(2)), marker(2)
    elseif (marker(1)>0.and.marker(3)>0) then
       write (unit=unit_no, fmt=505) trim(marker_name(1)), marker(1), &
            trim(marker_name(3)), marker(3)
    elseif (marker(2)>0.and.marker(3)>0) then
       write (unit=unit_no, fmt=505) trim(marker_name(2)), marker(2), &
            trim(marker_name(3)), marker(3)
    elseif (marker(1)>0) then
       write (unit=unit_no, fmt=510) trim(marker_name(1)), marker(1)
    elseif (marker(2)>0) then
       write (unit=unit_no, fmt=510) trim(marker_name(2)), marker(2)
    elseif (marker(3)>0) then
       write (unit=unit_no, fmt=510) trim(marker_name(3)), marker(3)
    endif
500 format (/,1x,'AT ',A,1x,i6,', ',A,1x,i6,', ',A,1x,i6,/)
505 format (/,1x,'AT ',A,1x,i6,', ',A,1x,i6,/)
510 format (/,1x,'AT ',A,1x,i6,/)
    ! ... 
  end subroutine marker_print

  function max_iter_approx() result(iter_max)
    ! ... Approximated maximum number of iteration for PCG.
    ! ... result
    integer :: iter_max
    ! ... local variables
    integer :: ponent, a_iter, o_iter, a_nodes, o_nodes
    ! ...................................................................
    ponent=nint(log10(real(n_rows)))
    if (ponent==0) then
       iter_max=n_rows
       return
    endif
    a_nodes=10**ponent
    a_iter=10*2**ponent-10
    if (n_rows<a_nodes) then
       o_iter=10*2**(ponent-1)-10
       o_nodes=10**(ponent-1)
    else
       o_iter=10*2**(ponent+1)-10
       o_nodes=10**(ponent+1)
    endif
    iter_max=nint(real(n_rows-o_nodes)*real(a_iter-o_iter) &
         /real(a_nodes-o_nodes))+o_iter
    ! ...
  end function max_iter_approx

end module PCG_MAIN


function elapsed_time(t_opt) result(e_time)
  ! ... Purpose: calculate elapsed execution time, tenths of a second.
  ! ... Assumes that  execution time is no more than one month.
  ! ... First call sets reference time; subsequent calls return elapsed 
  ! ... time relative to reference time.
  ! ... Returns:
  ! ... t_opt=0: returns time in deci-seconds
  ! ... t_opt=1: returns time in centi-seconds
  ! ... t_opt=2: returns time in milli-seconds
  ! ... t_opt=other: returns time in seconds
  ! ... argument list
  ! ... 
  integer :: t_opt
  ! ... 
  ! ... result
  ! ... 
  integer :: e_time
  ! ... 
  ! ... local variables
  ! ... 
  integer, dimension(1:8) :: t_part
  integer, save, dimension(1:12) :: no_dapmo= &
       (/31,28,31,30,31,30,31,31,30,31,30,31/)
  integer, save :: mo_s, da_s, h_s, mi_s, sec_s, msec_s
  logical, save :: initial=.true., flip
  integer :: mo_d, da_d, h_d, mi_d, sec_d, msec_d, t_sec, t_msec
  ! .....................................................................
  ! ... year=>t_part(1); month=>t_part(2); day=>t_part(3); hour=>t_part(5)
  ! ... minute=>t_part(6); second=>t_part(7); msecond=>t_part(8)
  call date_and_time(values=t_part)
  if (initial) then
     initial=.false.
     mo_s=t_part(2); da_s=t_part(3); h_s=t_part(5)
     mi_s=t_part(6); sec_s=t_part(7); msec_s=t_part(8)
     if (mod(t_part(1),4)==0) no_dapmo(2)=29
     e_time=0
     return
  else
     flip=.false.
     t_sec=0
     mo_d=t_part(2)-mo_s-1
     da_d=t_part(3)-da_s-1
     h_d=t_part(5)-h_s-1
     mi_d=t_part(6)-mi_s-1
     sec_d=t_part(7)-sec_s-1
     msec_d=t_part(8)-msec_s
     if (mo_d/=-1) then
        t_sec=t_sec+(no_dapmo(mo_s)+da_d)*86400
        flip=.true.
     elseif (da_d>-1) then
        t_sec=t_sec+da_d*86400
        flip=.true.
     endif
     if (flip) then
        t_sec=t_sec+(24+h_d)*3600
     elseif (h_d>-1) then
        t_sec=t_sec+h_d*3600
        flip=.true.
     endif
     if (flip) then
        t_sec=t_sec+(60+mi_d)*60
     elseif (mi_d>-1) then
        t_sec=t_sec+mi_d*60
        flip=.true.
     endif
     if (flip) then
        t_sec=t_sec+60+sec_d
     elseif (sec_d>-1) then
        t_sec=t_sec+sec_d
        flip=.true.
     endif
     ! ... milliseconds
     if (flip) then
        t_msec=1000+msec_d
     else
        t_msec=msec_d
     endif
     ! ... add second and milliseconds to form time measure
     if (t_opt==0) then
        e_time=t_sec*10+t_msec/100
        if (mod(t_msec,100)>50) e_time=e_time+1
     elseif (t_opt==1) then
        e_time=t_sec*100+t_msec/10
        if (mod(t_msec,10)>5) e_time=e_time+1
     elseif (t_opt==2) then
        e_time=t_sec*1000+t_msec
     else
        e_time=t_sec+t_msec/1000
        if (mod(t_msec,1000)>500) e_time=e_time+1
     endif
     ! ... 
  endif
  ! ... 
end function elapsed_time

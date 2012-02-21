module PCG_SOLVE
  ! ... PRECONDITIONED CONJUGATE GRADIENT ALGORITHM MODULE
  ! ... last modified: R. L. Naff, July 2007
  ! ... This version uses an incomplete UDU with fill levels 0 and 1.
  ! ... 
  ! ... solve Ax=b
  ! ... A(nxn), x(nx1), b(nx1); A symmetric
  ! ... A is assumed to be be stored in compressed diagonal scheme.
  ! ... only diagonals of upper triangle stored.
  ! ... storage is row ordered, smallest row number to largest.
  ! ...
  ! ... compressed diagonal storage arrays:
  ! ... 
  ! ... dd: diagonal column of A.
  ! ... dx: first off-diagonal column of a corresponing to x connection.
  ! ... dy: second off-diagonal column of a corresponing to y connection.
  ! ... dz: third off-diagonal column of a corresponing to z connection.
  ! ... 
  ! ... nx, ny, nz: x, y and z cell dimensions of grid
  ! ...
  ! ... pcg arrays:
  ! ... X(:): initial guess for solution vector (zero if unspecified).
  ! ... res(:): initial residual: res=b-Ax_o
  ! ...
  ! ... dimensions:
  ! ... dd, xx, rhs:  nx*ny*nz
  ! ... dx: nx*ny*nz if nx>1; otherwise 1
  ! ... dy: nx*ny*nz if ny>1; otherwise 1
  ! ... dz: nx*ny*nz if nz>1; otherwise 1
  ! ...
  ! ... Preconditioner is modified incomplete cholesky for 5 or 7 point 
  ! ... stencil.
  ! ...
  ! ... R.L. Naff 
  ! ... version 1.0, 11/2007
  ! ...
  use common_parameters
  use common_solver_types
  use MiUDU
  ! ... XX: X=A^{-1}*rhs
  ! ... in: XX=rhs; out: XX=X
  use mat_vec_mult
  ! ... rhs, r_v: A*r_v=rhs
  implicit none
  real(kind=kv), dimension(:), pointer :: x_dev, r_dev
  real(kind=kv) :: nu_i
contains

  subroutine CG_iter(precond_fn, convg_fn, X, res, max_iter, eps_i, &
       eps_o, p_unit)
    ! ... preconditioned conjugate gradient algorith based on 
    ! ... Golub and Van Loan, 1983.
    ! ... 
    ! ... precond_fn: specify preconditioning function upon call.
    ! ...   iUDU_solve_0: zero-fill incomplete Cholesky.
    ! ...   iUDU_solve_1: one-fill incomplete Cholesky.
    ! ... X: unknown vector.
    ! ... res: initial residual; res=rhs-A*X_o, X_o: initial guess.
    ! ... max_iter (in): maximum allowed iterations.
    ! ... max_iter (out): iterations to convergence.
    ! ... eps_i (in): specifed convergence criterion, CG algorithm.
    ! ... eps_i (out): final L2 norm on leaving conjugate gradient loop.
    ! ... eps_o(in): flag to activate adaptive relative convergence and 
    ! ...            threshold for activation.
    ! ... eps_o(out): entry L2 norm 
    ! ... p_unit>0: print convergence progress to file associated with p_unit.
    ! ... 
    ! ... Preconditioning function: precond_fn
    ! ... i_val: dummy variable for preconditioning functions
    ! ... XX: preconditioner variable
    ! ... XX(in): RHS to A*x=r
    ! ... XX(out): approximate solution to A*x=r
    ! ... 
    ! ... argument list
    ! ... 
    integer, external :: precond_fn
    integer, intent(in) :: p_unit
    integer, intent(inout) :: max_iter
    real(kind=kv), intent(inout) :: eps_i
    real(kind=kv), intent(inout) :: eps_o
    real(kind=kv), intent(inout), dimension(:) :: X, res
    logical, external :: convg_fn
    ! ... 
    ! ... local variables
    ! ... 
    integer :: i, i_val
    real(kind=kv) :: mu, nu, nu_p, ratio_l
    real(kind=kv), save :: nu_f=n0
    ! ... 
    ! ...................................................................
    ! ... 
    if (p_unit>0) write(unit=p_unit,fmt=665) 
    ! ... i=0: x_dev=r_dev
    XX=>x_dev
    x_dev=res
    ! ... x_dev=M^{-1}res
    i_val=precond_fn()
    nu=dot_product(x_dev,res)
    nu_i=nu
    ! ... 
    ! ... adaptive convergence
    ! ... ratio_l: measure of linearity of problem; linear, ratio_l=1
    ! ... eps_o: on input, minimum threshold for adaptive convergence 
    ! ...        algorithm.
    ! ...
    if (eps_o>small) then
       ratio_l=nu_f/nu_i
       if (ratio_l<eps_i) then
          if (ratio_l>eps_o) then
             eps_i=ratio_l
          else
             eps_i=eps_o
          endif
       endif
    endif
    XX=>r_dev      
    if (p_unit>0) write(unit=p_unit,fmt=666) 0, nu
    i=0
    do ! max_iter loop
       i=i+1
       ! ... r_dev=A*x_dev
       call a_multiply(x_dev,r_dev)
       mu=nu/dot_product(x_dev,r_dev)
       ! ... update solution
       X=X+mu*x_dev
       ! ... update residuals
       res=res-mu*r_dev
       r_dev=res
       ! ... r_dev=M^{-1}res
       i_val=precond_fn()
       nu_p=nu
       nu=dot_product(res,r_dev)
       ! xxx if (nu<0) print*,'i=',i,'nu=',nu
       if (p_unit>0) write(unit=p_unit,fmt=666) i, nu
       if (convg_fn(eps_i,nu).or.i==max_iter) then
          eps_o=nu_i; eps_i=nu; nu_f=nu
          nullify(XX)
          if (i==max_iter) then
             max_iter=-i
          else
             max_iter=i
          endif
          return
       endif
       x_dev=r_dev+nu*x_dev/nu_p
    enddo
    ! ...
665 format(/tr3,'inner',tr10,'L2'/tr1,'iteration',tr7,'norm'/tr1,'---------',tr4,'-----------')
666 format(tr2,i4,tr8,1pe12.5)
    ! ...
  end subroutine CG_iter

  subroutine CG_iter_xt(precond_fn, convg_fn, X, res, max_iter, eps_i, &
       eps_o, p_unit)
    ! ... preconditioned conjugate gradient algorith based on 
    ! ... Golub and Van Loan, 1983.
    ! ... Extended version in which solution is updated one additional time.
    ! ... 
    ! ... precond_fn: specify preconditioning function upon call.
    ! ...   iUDU_solve_0: zero-fill incomplete Cholesky.
    ! ...   iUDU_solve_1: one-fill incomplete Cholesky.
    ! ... X: unknown vector.
    ! ... res: initial residual; res=rhs-A*X_o, X_o: initial guess.
    ! ... max_iter (in): maximum allowed iterations.
    ! ... max_iter (out): iterations to convergence.
    ! ... eps_i (in): specifed convergence criterion, CG algorithm.
    ! ... eps_i (out): final L2 norm on leaving conjugate gradient loop.
    ! ... eps_o: entry L2 norm 
    ! ... p_unit>0: print convergence progress to fine associated with p_unit.
    ! ... 
    ! ... Preconditioning function: precond_fn
    ! ... i_val: dummy variable for preconditioning functions
    ! ... XX: preconditioner variable
    ! ... XX(in): RHS to A*x=r
    ! ... XX(out): approximate solution to A*x=r
    ! ... 
    ! ... argument list
    ! ... 
    integer, external :: precond_fn
    integer, intent(in) :: p_unit
    integer, intent(inout) :: max_iter
    real(kind=kv), intent(inout) :: eps_i
    real(kind=kv), intent(out) :: eps_o
    real(kind=kv), intent(inout), dimension(:) :: X, res
    logical, external :: convg_fn
    ! ... 
    ! ... local variables
    ! ... 
    integer :: i, i_val
    real(kind=kv) :: mu, nu, nu_p
    ! ... 
    ! ...................................................................
    ! ... 
    if (p_unit>0) write(unit=p_unit,fmt=665) 
    ! ... i=0: x_dev=r_dev
    XX=>x_dev
    x_dev=res
    i_val=precond_fn()
    nu=dot_product(x_dev,res)
    nu_i=nu
    XX=>r_dev      
    if (p_unit>0) write(unit=p_unit,fmt=666) 0, nu
    do i=1, max_iter
       ! ... r_dev=A*x_dev
       call a_multiply(x_dev,r_dev)
       mu=nu/dot_product(x_dev,r_dev)
       ! ... update solution
       X=X+mu*x_dev
       if (convg_fn(eps_i,nu)) then
          max_iter=i; eps_o=nu_i; eps_i=nu
          nullify(XX)
          return
       endif
       ! ... update residuals
       res=res-mu*r_dev
       r_dev=res
       ! ... r_dev=M^{-1}res
       i_val=precond_fn()
       nu_p=nu
       nu=dot_product(res,r_dev)
       ! xxx if (nu<0) print*,'i=',i,'nu=',nu
       if (p_unit>0) write(unit=p_unit,fmt=666) i, nu
       x_dev=r_dev+nu*x_dev/nu_p
    enddo
    ! ... max_iter exit
    nullify(XX)
    max_iter=-max_iter; eps_o=nu_i; eps_i=nu
    ! ...
665 format(/tr3,'inner',tr10,'L2'/tr1,'iteration',tr7,'norm'/tr1,'---------',tr4,'-----------')
666 format(tr2,i4,tr8,1pe12.5)
    ! ...
  end subroutine CG_iter_xt

  function abs_convg(eps,nu)
    ! ... absolute convergence function
    ! ... based on eps<sqrt(dot_product(X_c,res));
    ! ... equivalently: eps**2<dot_product(X_c,res)
    ! ... eps must be squared prior to calling function
    real(kind=kv) :: eps,nu
    logical :: abs_convg
    abs_convg=.false.
    if (nu<=eps) abs_convg=.true.
  end function abs_convg

  function rel_convg(eps,nu)
    ! ... relative convergence function
    ! ... based on dot_product(X_c,res)_final<eps*dot_product(X_c,res)_initial
    real(kind=kv) :: eps,nu
    logical :: rel_convg
    rel_convg=.false.
    if (nu<=eps*nu_i) rel_convg=.true.
  end function rel_convg

end module PCG_SOLVE

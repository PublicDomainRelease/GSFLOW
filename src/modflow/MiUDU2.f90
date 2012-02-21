module MiUDU
  ! ... MODIFIED INCOMPLETE CHOLESKY (MIC) ALGORITHM.
  ! ... last modified: R. L. Naff, July 2007
  ! ... 
  ! ... Let M be the MIC approximation of A: then module contains
  ! ... subroutines to solve M*x=rhs.
  ! ... 
  ! ... non-zero errors:
  ! ...  error>0: nodal location of zero pivot returned.
  ! ...  error<0: negative of nodal location of negative piviot returned.
  ! ... 
  ! ... R.L. Naff 
  ! ... version 1.0, 11/2007
  ! ...
  use common_parameters
  use common_solver_types
  ! ... DD, DX, DY, DZ, dim, diag, dx0, dy0, dz0, dx1, dy1, dz1 
  implicit none
  real(kind=kv), dimension(:), pointer :: XX
  private; public :: diag_factor_0, diag_factor_1, iUDU_solve_0, &
       iUDU_solve_1, XX
contains

  !********************************************************************
  ! PRECONDITIONER FUNCTIONS
  !********************************************************************

  function iUDU_solve_0()
    ! ...
    ! ... Incomplete Cholesky inversion with zero fill
    ! ...
    ! ... argument list
    ! ...
    integer :: iUDU_solve_0
    ! ...
    ! ...................................................................
    iUDU_solve_0=0
    call U_tranpose_solve_0(XX)
    call U_solve_0(XX)
    iUDU_solve_0=1
  end function iUDU_solve_0
  
  function iUDU_solve_1()
    ! ...
    ! ... Incomplete Cholesky inversion with fill level 0ne
    ! ...
    ! ... argument list
    ! ...
    integer :: iUDU_solve_1
    ! ...
    ! ...................................................................
    iUDU_solve_1=0
    call U_tranpose_solve_1(XX)
    call U_solve_1(XX)
    iUDU_solve_1=1
  end function iUDU_solve_1

  !********************************************************************
  ! SUBROUTINES FOR FILL LEVEL ZERO
  !********************************************************************

  subroutine diag_factor_0(omega,error)
    ! ... MiUDU(0), modified incomplete UDU factorization with no fill.
    ! ... Diagonal factors for symmetric, incomplete u^tdu factorization.
    ! ... omega: relaxation factor for modified iUDU.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), intent(in) :: omega
    integer, intent(inout)  :: error
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy
    ! ...
    ! ...................................................................
    ! ...
    ! ... find diagonal entries
    ! ...
    error=0
    nxy=nx*ny
    diag(1)=DD(1)
    ! ... 
    select case(dim)
    case(3)
       ! ... 3D
       ! ... k=1, j=1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX(node-1)*(DX(node-1) &
               + omega*(DY(node-1)+DZ(node-1)))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
       ! ... k=1
       ! ... row j=1 excluded
       do node=nx+1, nxy
          diag(node)=DD(node)-(DX(node-1)*(DX(node-1) &
               + omega*(DY(node-1)+DZ(node-1)))/diag(node-1) &
               + DY(node-nx)*(DY(node-nx)+omega*(DX(node-nx) &
               + DZ(node-nx)))/diag(node-nx))
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
       ! ... layer k=1 excluded
       do node=nxy+1, n_rows
          diag(node)=DD(node)-(DX(node-1)*(DX(node-1) &
               + omega*(DY(node-1)+DZ(node-1)))/diag(node-1) &
               + DY(node-nx)*(DY(node-nx)+omega*(DX(node-nx) &
               + DZ(node-nx)))/diag(node-nx) &
               + DZ(node-nxy)*(DZ(node-nxy)+omega*(DX(node-nxy) &
               + DY(node-nxy)))/diag(node-nxy))
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    case(2)
       ! ... 2D, based on nx>1, ny>1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX(node-1)*(DX(node-1) &
               + omega*DY(node-1))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
       ! ... row j=1 excluded
       do node=nx+1, nxy
          diag(node)=DD(node)-(DX(node-1)*(DX(node-1) &
               + omega*DY(node-1))/diag(node-1) &
               + DY(node-nx)*(DY(node-nx) &
               + omega*DX(node-nx))/diag(node-nx))
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    case(1)
       ! ... 1D, based on nx>1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX(node-1)**2/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    end select
    ! ...
  end subroutine diag_factor_0

  subroutine U_tranpose_solve_0(rhs)
    ! ... MiUDU(0), modified incomplete UDU factorization with no fill.
    ! ... Invert transposed upper against rhs using elimination.
    ! ... Approximates Y in U^T*D*Y=RHS where Y is U*X.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... This subroutine modified to include diagonal D.
    ! ... Numbering order: x before y before z. 
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ...
    ! ....................................................................
    ! ...
    ! ... columns begin with first entry above diagional
    ! ...
    rhs(1)=rhs(1)/diag(1)
    ! ... 
    ! ... layer k=1, row j=1
    do node=2, nx
       rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1))/diag(node) 
    enddo
    ! ... layer k=1
    ! ... row j=1 excluded
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=nx+1, nx+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=nx+m+1, nxy, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX(node) &
               - rhs(node-nx+1)*DY(node-nx+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX(node+1) &
               - rhs(node-nx+2)*DY(node-nx+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX(node+2) &
               - rhs(node-nx+3)*DY(node-nx+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX(node+3) &
               - rhs(node-nx+4)*DY(node-nx+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX(node+4) &
               - rhs(node-nx+5)*DY(node-nx+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX(node+5) &
               - rhs(node-nx+6)*DY(node-nx+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX(node+6) &
               - rhs(node-nx+7)*DY(node-nx+7))/diag(node+7)
       enddo
    endif
    ! ... layer k=1 excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=nxy+1, nxy+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx) &
               - rhs(node-nxy)*DZ(node-nxy))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=nxy+m+1, n_rows, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX(node-1) &
               - rhs(node-nx)*DY(node-nx) &
               - rhs(node-nxy)*DZ(node-nxy))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX(node) &
               - rhs(node-nx+1)*DY(node-nx+1) &
               - rhs(node-nxy+1)*DZ(node-nxy+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX(node+1) &
               - rhs(node-nx+2)*DY(node-nx+2) &
               - rhs(node-nxy+2)*DZ(node-nxy+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX(node+2) &
               - rhs(node-nx+3)*DY(node-nx+3) &
               - rhs(node-nxy+3)*DZ(node-nxy+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX(node+3) &
               - rhs(node-nx+4)*DY(node-nx+4) &
               - rhs(node-nxy+4)*DZ(node-nxy+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX(node+4) &
               - rhs(node-nx+5)*DY(node-nx+5) &
               - rhs(node-nxy+5)*DZ(node-nxy+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX(node+5) &
               - rhs(node-nx+6)*DY(node-nx+6) &
               - rhs(node-nxy+6)*DZ(node-nxy+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX(node+6) &
               - rhs(node-nx+7)*DY(node-nx+7) &
               - rhs(node-nxy+7)*DZ(node-nxy+7))/diag(node+7)
       enddo
    endif
    ! ... 
  end subroutine U_tranpose_solve_0

  subroutine U_solve_0(rhs)
    ! ... MiUDU(0), modified incomplete UDU factorization with no fill.
    ! ... Invert upper against rhs using elimination.
    ! ... Approximates X in U*X=RHS.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ... 
    ! ....................................................................
    ! ...
    ! ... reverse order for accessing columns
    ! ...
    ! ... layer k=nz, row j=ny
    ! ... cell i=nx excluded
    do node=n_rows-1, n_rows-nx+1, -1
       rhs(node)=rhs(node)-rhs(node+1)*DX(node)/diag(node)
    enddo
    ! ... layer k=nz
    ! ... row j=ny excluded
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=n_rows-nx, n_rows-nx-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=n_rows-nx-m, n_rows-nxy+1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX(node-1) &
               + rhs(node+nx-1)*DY(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX(node-2) &
               + rhs(node+nx-2)*DY(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX(node-3) &
               + rhs(node+nx-3)*DY(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX(node-4) &
               + rhs(node+nx-4)*DY(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX(node-5) &
               + rhs(node+nx-5)*DY(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX(node-6) &
               + rhs(node+nx-6)*DY(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX(node-7) &
               + rhs(node+nx-7)*DY(node-7))/diag(node-7)
       enddo
    endif
    ! ... layer k=nz excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=n_rows-nxy, n_rows-nxy-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node) &
               + rhs(node+nxy)*DZ(node))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=n_rows-nxy-m, 1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX(node) &
               + rhs(node+nx)*DY(node) &
               + rhs(node+nxy)*DZ(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX(node-1) &
               + rhs(node+nx-1)*DY(node-1) &
               + rhs(node+nxy-1)*DZ(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX(node-2) &
               + rhs(node+nx-2)*DY(node-2) &
               + rhs(node+nxy-2)*DZ(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX(node-3) &
               + rhs(node+nx-3)*DY(node-3) &
               + rhs(node+nxy-3)*DZ(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX(node-4) &
               + rhs(node+nx-4)*DY(node-4) &
               + rhs(node+nxy-4)*DZ(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX(node-5) &
               + rhs(node+nx-5)*DY(node-5) &
               + rhs(node+nxy-5)*DZ(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX(node-6) &
               + rhs(node+nx-6)*DY(node-6) &
               + rhs(node+nxy-6)*DZ(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX(node-7) &
               + rhs(node+nx-7)*DY(node-7) &
               + rhs(node+nxy-7)*DZ(node-7))/diag(node-7)
       enddo
    endif
    ! ... 
  end subroutine U_solve_0

  !********************************************************************
  ! SUBROUTINES FOR FILL LEVEL ONE
  !********************************************************************

  subroutine diag_factor_1(omega,error)
    ! ... MiUDU(1), modified incomplete UDU factorization with fill level one.
    ! ... omega: relaxation factor for modified iUDU.
    ! ... Diagonal and off diagonal factors for incomplete factorization.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z. 
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... The MiUDU(1) does not always produce positive values for the diag
    ! ... array; for poorly conditioned problems, elements of diag may be 
    ! ... negative..
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), intent(in) :: omega
    integer, intent(inout) :: error
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy
    ! ...
    ! ...................................................................
    ! ...
    ! ... find scaled factors
    ! ...
    ! ... k=1, j=1, i=1
    error=0
    diag(1)=DD(1)
    select case(dim)
    case(3)
       ! ... DZ0=>DZ
       DX0(1)=DX(1); DY0(1)=DY(1)
       nxy=nx*ny
       ! ... k=1, j=1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*(DX1(node-1)+DZ1(node-1)))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node)
          DY0(node)=DY(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
          DZ1(node)=-DX0(node-1)*DZ0(node-1)/diag(node-1)
       enddo
       ! ... k=1
       ! ... row j=1 excluded
       do node=nx+1, nxy
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*(DX1(node-1)+DY1(node-1)+DZ1(node-1)))/diag(node-1) &
               -DX1(node-nx+1)*(DX1(node-nx+1) &
               +omega*(DX0(node-nx+1)+DZ0(node-nx+1)))/diag(node-nx+1) &
               -DY0(node-nx)*(DY0(node-nx) & 
               +omega*(DY1(node-nx)+DZ1(node-nx)))/diag(node-nx) !B
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node) &
               -DX1(node-nx+1)*DY0(node-nx+1)/diag(node-nx+1) 
          DY0(node)=DY(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
          DY1(node)=-DY0(node-nx)*DZ0(node-nx)/diag(node-nx) &
               -DX1(node-nx+1)*DZ1(node-nx+1)/diag(node-nx+1)
          DZ1(node)=-DX0(node-1)*DZ0(node-1)/diag(node-1)
       enddo
       ! ... layer k=1 excluded
       do node=nxy+1, n_rows
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*(DX1(node-1)+DY1(node-1)+DZ1(node-1)))/diag(node-1) &
               -DX1(node-nx+1)*(DX1(node-nx+1) &
                +omega*(DX0(node-nx+1)+DZ0(node-nx+1)))/diag(node-nx+1) &
               -DY0(node-nx)*(DY0(node-nx) & 
               +omega*(DY1(node-nx)+DZ1(node-nx)))/diag(node-nx) & !B
               -DY1(node-nxy+nx)*(DY1(node-nxy+nx) &
               +omega*(DX0(node-nxy+nx)+DY0(node-nxy+nx)))/diag(node-nxy+nx) & 
               -DZ1(node-nxy+1)*(DZ1(node-nxy+1) &
               +omega*(DX0(node-nxy+1)+DY0(node-nxy+1)))/diag(node-nxy+1) & !B
               -DZ0(node-nxy)*(DZ0(node-nxy) &
               +omega*(DX1(node-nxy)))/diag(node-nxy) !B
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node) &
               -DX1(node-nx+1)*DY0(node-nx+1)/diag(node-nx+1) &
               -DZ1(node-nxy+1)*DZ0(node-nxy+1)/diag(node-nxy+1) 
          DY0(node)=DY(node) &
               -DY1(node-nxy+nx)*DZ0(node-nxy+nx)/diag(node-nxy+nx)
          DZ0(node)=DZ(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1) &
               -DY1(node-nxy+nx)*DZ1(node-nxy+nx)/diag(node-nxy+nx)
          DY1(node)=-DY0(node-nx)*DZ0(node-nx)/diag(node-nx) &
               -DX1(node-nx+1)*DZ1(node-nx+1)/diag(node-nx+1)
          DZ1(node)=-DX0(node-1)*DZ0(node-1)/diag(node-1)
       enddo
    case(2)
       DX0(1)=DX(1)
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               +omega*DX1(node-1))/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node)
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
       enddo
       ! ... row j=1 excluded
       do node=nx+1, n_rows
          diag(node)=DD(node)-DX0(node-1)*(DX0(node-1) &
               + omega*DX1(node-1))/diag(node-1) &
               - DY0(node-nx)**2/diag(node-nx) &
               - DX1(node-nx+1)*(DX1(node-nx+1) &
               + omega*DX0(node-nx+1))/diag(node-nx+1) 
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
          DX0(node)=DX(node) &
               -DX1(node-nx+1)*DY0(node-nx+1)/diag(node-nx+1) 
          DX1(node)=-DX0(node-1)*DY0(node-1)/diag(node-1)
       enddo
    case(1)
       ! ... 1-D, based on nx>1
       ! ... cell i=1 excluded
       do node=2, nx
          diag(node)=DD(node)-DX0(node-1)**2/diag(node-1)
          if (diag(node)<mz) then
             if (diag(node)<n0) then
                error=-node
                return
             endif
             error=node
             return
          endif
       enddo
    end select
    ! ...
  end subroutine diag_factor_1

  subroutine U_tranpose_solve_1(rhs)
    ! ... MiUDU(1), modified incomplete UDU factorization with fill level one.
    ! ... Invert transposed upper against rhs using elimination.
    ! ... Approximates Y in U^T*D*Y=RHS where Y is U*X.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... This subroutine modified to include diagonal D.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ...
    ! ....................................................................
    ! ...
    ! ... columns begin with first entry above diagional
    ! ...
    rhs(1)=rhs(1)/diag(1)
    ! ... 
    ! ... layer k=1, row j=1
    do node=2, nx
       rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1))/diag(node)
    enddo
    ! ... layer k=1
    ! ... row j=1 excluded
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=nx+1, nx+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=nx+m+1, nxy, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX0(node) &
               - rhs(node-nx+2)*DX1(node-nx+2) &
               - rhs(node-nx+1)*DY0(node-nx+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX0(node+1) &
               - rhs(node-nx+3)*DX1(node-nx+3) &
               - rhs(node-nx+2)*DY0(node-nx+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX0(node+2) &
               - rhs(node-nx+4)*DX1(node-nx+4) &
               - rhs(node-nx+3)*DY0(node-nx+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX0(node+3) &
               - rhs(node-nx+5)*DX1(node-nx+5) &
               - rhs(node-nx+4)*DY0(node-nx+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX0(node+4) &
               - rhs(node-nx+6)*DX1(node-nx+6) &
               - rhs(node-nx+5)*DY0(node-nx+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX0(node+5) &
               - rhs(node-nx+7)*DX1(node-nx+7) &
               - rhs(node-nx+6)*DY0(node-nx+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX0(node+6) &
               - rhs(node-nx+8)*DX1(node-nx+8) &
               - rhs(node-nx+7)*DY0(node-nx+7))/diag(node+7)
       enddo
    endif
    ! ... layer k=1 excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=nxy+1, nxy+m
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx) &
               - rhs(node-nxy+nx)*DY1(node-nxy+nx) &
               - rhs(node-nxy+1)*DZ1(node-nxy+1) &
               - rhs(node-nxy)*DZ0(node-nxy))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=nxy+m+1, n_rows, mod_no
          rhs(node)=(rhs(node)-rhs(node-1)*DX0(node-1) &
               - rhs(node-nx+1)*DX1(node-nx+1) &
               - rhs(node-nx)*DY0(node-nx) &
               - rhs(node-nxy+nx)*DY1(node-nxy+nx) &
               - rhs(node-nxy+1)*DZ1(node-nxy+1) &
               - rhs(node-nxy)*DZ0(node-nxy))/diag(node)
          rhs(node+1)=(rhs(node+1)-rhs(node)*DX0(node) &
               - rhs(node-nx+2)*DX1(node-nx+2) &
               - rhs(node-nx+1)*DY0(node-nx+1) &
               - rhs(node-nxy+nx+1)*DY1(node-nxy+nx+1) &
               - rhs(node-nxy+2)*DZ1(node-nxy+2) &
               - rhs(node-nxy+1)*DZ0(node-nxy+1))/diag(node+1)
          rhs(node+2)=(rhs(node+2)-rhs(node+1)*DX0(node+1) &
               - rhs(node-nx+3)*DX1(node-nx+3) &
               - rhs(node-nx+2)*DY0(node-nx+2) &
               - rhs(node-nxy+nx+2)*DY1(node-nxy+nx+2) &
               - rhs(node-nxy+3)*DZ1(node-nxy+3) &
               - rhs(node-nxy+2)*DZ0(node-nxy+2))/diag(node+2)
          rhs(node+3)=(rhs(node+3)-rhs(node+2)*DX0(node+2) &
               - rhs(node-nx+4)*DX1(node-nx+4) &
               - rhs(node-nx+3)*DY0(node-nx+3) &
               - rhs(node-nxy+nx+3)*DY1(node-nxy+nx+3) &
               - rhs(node-nxy+4)*DZ1(node-nxy+4) &
               - rhs(node-nxy+3)*DZ0(node-nxy+3))/diag(node+3)
          rhs(node+4)=(rhs(node+4)-rhs(node+3)*DX0(node+3) &
               - rhs(node-nx+5)*DX1(node-nx+5) &
               - rhs(node-nx+4)*DY0(node-nx+4) &
               - rhs(node-nxy+nx+4)*DY1(node-nxy+nx+4) &
               - rhs(node-nxy+5)*DZ1(node-nxy+5) &
               - rhs(node-nxy+4)*DZ0(node-nxy+4))/diag(node+4)
          rhs(node+5)=(rhs(node+5)-rhs(node+4)*DX0(node+4) &
               - rhs(node-nx+6)*DX1(node-nx+6) &
               - rhs(node-nx+5)*DY0(node-nx+5) &
               - rhs(node-nxy+nx+5)*DY1(node-nxy+nx+5) &
               - rhs(node-nxy+6)*DZ1(node-nxy+6) &
               - rhs(node-nxy+5)*DZ0(node-nxy+5))/diag(node+5)
          rhs(node+6)=(rhs(node+6)-rhs(node+5)*DX0(node+5) &
               - rhs(node-nx+7)*DX1(node-nx+7) &
               - rhs(node-nx+6)*DY0(node-nx+6) &
               - rhs(node-nxy+nx+6)*DY1(node-nxy+nx+6) &
               - rhs(node-nxy+7)*DZ1(node-nxy+7) &
               - rhs(node-nxy+6)*DZ0(node-nxy+6))/diag(node+6)
          rhs(node+7)=(rhs(node+7)-rhs(node+6)*DX0(node+6) &
               - rhs(node-nx+8)*DX1(node-nx+8) &
               - rhs(node-nx+7)*DY0(node-nx+7) &
               - rhs(node-nxy+nx+7)*DY1(node-nxy+nx+7) &
               - rhs(node-nxy+8)*DZ1(node-nxy+8) &
               - rhs(node-nxy+7)*DZ0(node-nxy+7))/diag(node+7)
       enddo
    endif
    ! ... 
  end subroutine U_tranpose_solve_1

  subroutine U_solve_1(rhs)
    ! ... MiUDU(1), modified incomplete UDU factorization with no fill.
    ! ... Invert upper against rhs using elimination.
    ! ... Approximates X in U*X=RHS.
    ! ... 3-D algorithm for 7 point stencil.
    ! ... Numbering order: x before y before z.
    ! ... 2-D and 1-D cases, for above numbering order, follow automatically.
    ! ...
    ! ... argument list
    ! ...
    real(kind=kv), dimension(:), intent(inout) :: rhs
    ! ...
    ! ... local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    ! ... 
    ! ....................................................................
    ! ...
    ! ... reverse order for accessing columns
    ! ...
    ! ... layer k=nz, row j=ny
    ! ... cell i=nx excluded
    ! ... No contribution from DX1 as DX1(n_rows-nx+1)=0
    do node=n_rows-1, n_rows-nx+1, -1
       rhs(node)=rhs(node)-rhs(node+1)*DX0(node)/diag(node)
    enddo
    ! ... layer k=nz
    ! ... row j=ny excluded
    ! ... No contribution from DY1 as DY1(n_rows-nxy+nx)... DY1(n_rows-nxy+1)=0
    ! ... No contribution from DZ1 as DZ1(n_rows-nxy+1)=0
    nxy=nx*ny
    m=mod(nxy-nx, mod_no)
    if (m>0) then
       do node=n_rows-nx, n_rows-nx-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node))/diag(node)
       enddo
    endif
    if (nxy-nx>=mod_no) then
       do node=n_rows-nx-m, n_rows-nxy+1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX0(node-1) &
               + rhs(node+nx-2)*DX1(node-1) &
               + rhs(node+nx-1)*DY0(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX0(node-2) &
               + rhs(node+nx-3)*DX1(node-2) &
               + rhs(node+nx-2)*DY0(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX0(node-3) &
               + rhs(node+nx-4)*DX1(node-3) &
               + rhs(node+nx-3)*DY0(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX0(node-4) &
               + rhs(node+nx-5)*DX1(node-4) &
               + rhs(node+nx-4)*DY0(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX0(node-5) &
               + rhs(node+nx-6)*DX1(node-5) &
               + rhs(node+nx-5)*DY0(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX0(node-6) &
               + rhs(node+nx-7)*DX1(node-6) &
               + rhs(node+nx-6)*DY0(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX0(node-7) &
               + rhs(node+nx-8)*DX1(node-7) &
               + rhs(node+nx-7)*DY0(node-7))/diag(node-7)
       enddo
    endif
    ! ... layer k=nz excluded
    m=mod(n_rows-nxy, mod_no)
    if (m>0) then
       do node=n_rows-nxy, n_rows-nxy-m+1, -1
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node) &
               + rhs(node+nxy-nx)*DY1(node) &
               + rhs(node+nxy-1)*DZ1(node) &
               + rhs(node+nxy)*DZ0(node))/diag(node)
       enddo
    endif
    if (n_rows-nxy>=mod_no) then
       do node=n_rows-nxy-m, 1, -mod_no
          rhs(node)=rhs(node)-(rhs(node+1)*DX0(node) &
               + rhs(node+nx-1)*DX1(node) &
               + rhs(node+nx)*DY0(node) &
               + rhs(node+nxy-nx)*DY1(node) &
               + rhs(node+nxy-1)*DZ1(node) &
               + rhs(node+nxy)*DZ0(node))/diag(node)
          rhs(node-1)=rhs(node-1)-(rhs(node)*DX0(node-1) &
               + rhs(node+nx-2)*DX1(node-1) &
               + rhs(node+nx-1)*DY0(node-1) &
               + rhs(node+nxy-nx-1)*DY1(node-1) &
               + rhs(node+nxy-2)*DZ1(node-1) &
               + rhs(node+nxy-1)*DZ0(node-1))/diag(node-1)
          rhs(node-2)=rhs(node-2)-(rhs(node-1)*DX0(node-2) &
               + rhs(node+nx-3)*DX1(node-2) &
               + rhs(node+nx-2)*DY0(node-2) &
               + rhs(node+nxy-nx-2)*DY1(node-2) &
               + rhs(node+nxy-3)*DZ1(node-2) &
               + rhs(node+nxy-2)*DZ0(node-2))/diag(node-2)
          rhs(node-3)=rhs(node-3)-(rhs(node-2)*DX0(node-3) &
               + rhs(node+nx-4)*DX1(node-3) &
               + rhs(node+nx-3)*DY0(node-3) &
               + rhs(node+nxy-nx-3)*DY1(node-3) &
               + rhs(node+nxy-4)*DZ1(node-3) &
               + rhs(node+nxy-3)*DZ0(node-3))/diag(node-3)
          rhs(node-4)=rhs(node-4)-(rhs(node-3)*DX0(node-4) &
               + rhs(node+nx-5)*DX1(node-4) &
               + rhs(node+nx-4)*DY0(node-4) &
               + rhs(node+nxy-nx-4)*DY1(node-4) &
               + rhs(node+nxy-5)*DZ1(node-4) &
               + rhs(node+nxy-4)*DZ0(node-4))/diag(node-4)
          rhs(node-5)=rhs(node-5)-(rhs(node-4)*DX0(node-5) &
               + rhs(node+nx-6)*DX1(node-5) &
               + rhs(node+nx-5)*DY0(node-5) &
               + rhs(node+nxy-nx-5)*DY1(node-5) &
               + rhs(node+nxy-6)*DZ1(node-5) &
               + rhs(node+nxy-5)*DZ0(node-5))/diag(node-5)
          rhs(node-6)=rhs(node-6)-(rhs(node-5)*DX0(node-6) &
               + rhs(node+nx-7)*DX1(node-6) &
               + rhs(node+nx-6)*DY0(node-6) &
               + rhs(node+nxy-nx-6)*DY1(node-6) &
               + rhs(node+nxy-7)*DZ1(node-6) &
               + rhs(node+nxy-6)*DZ0(node-6))/diag(node-6)
          rhs(node-7)=rhs(node-7)-(rhs(node-6)*DX0(node-7) &
               + rhs(node+nx-8)*DX1(node-7) &
               + rhs(node+nx-7)*DY0(node-7) &
               + rhs(node+nxy-nx-7)*DY1(node-7) &
               + rhs(node+nxy-8)*DZ1(node-7) &
               + rhs(node+nxy-7)*DZ0(node-7))/diag(node-7)
       enddo
    endif
    ! ... 
  end subroutine U_solve_1

end module MiUDU

module MAT_VEC_MULT
  ! ... Subroutine for matrix-vector multiply: A*X=rhs
  ! ... 
  ! ... R.L. Naff 
  ! ... version 1.1, 08/2008
  ! ... openmp directives added
  ! ... 
  use common_parameters
  use common_solver_types
  ! ... DD, DX, DY, DZ, dim
  implicit none
  private; public ::  a_multiply
contains

  subroutine a_multiply(X_i, rhs)
    ! ...  Post multiply matrix A by vector B: AxB
    ! ...  
    ! ...  A is in sparse compressed diagonal format
    ! ...  for the upper triangle of a spd matrix.
    ! ...
    ! ...  argument list
    ! ...
    real(kind=kv), intent(in), dimension(:) :: X_i
    real(kind=kv), intent(out), dimension(:) :: rhs
    ! ...  
    ! ...  local variables
    ! ...
    integer :: node, nxy, m
    integer, save :: mod_no=8
    real(kind=kv) :: bnp0, bnp1, bnp2, bnp3, bnp4, bnp5, bnp6, bnp7
    !......................................................................
    select case(dim)
    case(3)
       nxy=nx*ny
       ! ... full 3-D
       ! ... top and bottom layer excluded (k=1,nz)
       m=mod(n_rows-2*nxy, mod_no)
       !$OMP PARALLEL DEFAULT(NONE) &
       !$OMP SHARED(rhs,X_i,DD,DX,DY,DZ) &
       !$OMP PRIVATE(node,bnp0,bnp1,bnp2,bnp3,bnp4,bnp5,bnp6,bnp7) &
       !$OMP FIRSTPRIVATE(m,mod_no,nxy,nx,n_rows)
       if (m>0) then
          !$OMP DO           
          do node=nxy+1, nxy+m
             rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + X_i(node+1)*DX(node)+X_i(node+nx)*DY(node) &
                  + X_i(node+nxy)*DZ(node) 
          enddo
          !$OMP END DO
       endif
       if (n_rows-2*nxy>=mod_no) then
          !$OMP DO           
          do node=nxy+m+1, n_rows-nxy, mod_no
             bnp0=X_i(node); bnp1=X_i(node+1); bnp2=X_i(node+2)
             bnp3=X_i(node+3); bnp4=X_i(node+4); bnp5=X_i(node+5)
             bnp6=X_i(node+6); bnp7=X_i(node+7)
             rhs(node)=bnp0*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + bnp1*DX(node)+X_i(node+nx)*DY(node) &
                  + X_i(node+nxy)*DZ(node) 
             rhs(node+1)=bnp1*DD(node+1)+bnp0*DX(node) &
                  + X_i(node-nx+1)*DY(node-nx+1) &
                  + X_i(node-nxy+1)*DZ(node-nxy+1)+bnp2*DX(node+1) &
                  + X_i(node+nx+1)*DY(node+1)+X_i(node+nxy+1)*DZ(node+1) 
             rhs(node+2)=bnp2*DD(node+2)+bnp1*DX(node+1) &
                  + X_i(node-nx+2)*DY(node-nx+2) &
                  + X_i(node-nxy+2)*DZ(node-nxy+2)+bnp3*DX(node+2) &
                  + X_i(node+nx+2)*DY(node+2)+X_i(node+nxy+2)*DZ(node+2) 
             rhs(node+3)=bnp3*DD(node+3)+bnp2*DX(node+2) &
                  + X_i(node-nx+3)*DY(node-nx+3) &
                  + X_i(node-nxy+3)*DZ(node-nxy+3)+bnp4*DX(node+3) &
                  + X_i(node+nx+3)*DY(node+3)+X_i(node+nxy+3)*DZ(node+3) 
             rhs(node+4)=bnp4*DD(node+4)+bnp3*DX(node+3) &
                  + X_i(node-nx+4)*DY(node-nx+4) &
                  + X_i(node-nxy+4)*DZ(node-nxy+4)+bnp5*DX(node+4) &
                  + X_i(node+nx+4)*DY(node+4)+X_i(node+nxy+4)*DZ(node+4) 
             rhs(node+5)=bnp5*DD(node+5)+bnp4*DX(node+4) &
                  + X_i(node-nx+5)*DY(node-nx+5) &
                  + X_i(node-nxy+5)*DZ(node-nxy+5)+bnp6*DX(node+5) &
                  + X_i(node+nx+5)*DY(node+5)+X_i(node+nxy+5)*DZ(node+5) 
             rhs(node+6)=bnp6*DD(node+6)+bnp5*DX(node+5) &
                  + X_i(node-nx+6)*DY(node-nx+6) &
                  + X_i(node-nxy+6)*DZ(node-nxy+6)+bnp7*DX(node+6) &
                  + X_i(node+nx+6)*DY(node+6)+X_i(node+nxy+6)*DZ(node+6) 
             rhs(node+7)=bnp7*DD(node+7)+bnp6*DX(node+6) &
                  + X_i(node-nx+7)*DY(node-nx+7) &
                  + X_i(node-nxy+7)*DZ(node-nxy+7)+X_i(node+8)*DX(node+7) &
                  + X_i(node+nx+7)*DY(node+7)+X_i(node+nxy+7)*DZ(node+7) 
          enddo
          !$OMP END DO
       endif
       ! ... bottom k layer:  k=1
       ! ... row j=1 excluded
       m=mod(nxy-nx, mod_no)
       if (m>0) then
          !$OMP DO           
          do node=nx+1, nx+m
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node+nxy)*DZ(node) &
                  + X_i(node-1)*DX(node-1)+X_i(node-nx)*DY(node-nx)
          enddo
          !$OMP END DO
       endif
       if (nxy-nx>=mod_no) then
          !$OMP DO           
          do node=nx+m+1, nxy, mod_no
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node+nxy)*DZ(node) &
                  + X_i(node-1)*DX(node-1)+X_i(node-nx)*DY(node-nx)
             rhs(node+1)=X_i(node+1)*DD(node+1)+X_i(node+2)*DX(node+1) &
                  + X_i(node+nx+1)*DY(node+1)+X_i(node+nxy+1)*DZ(node+1) &
                  + X_i(node)*DX(node)+X_i(node-nx+1)*DY(node-nx+1)
             rhs(node+2)=X_i(node+2)*DD(node+2)+X_i(node+3)*DX(node+2) &
                  + X_i(node+nx+2)*DY(node+2)+X_i(node+nxy+2)*DZ(node+2) &
                  + X_i(node+1)*DX(node+1)+X_i(node-nx+2)*DY(node-nx+2)
             rhs(node+3)=X_i(node+3)*DD(node+3)+X_i(node+4)*DX(node+3) &
                  + X_i(node+nx+3)*DY(node+3)+X_i(node+nxy+3)*DZ(node+3) &
                  + X_i(node+2)*DX(node+2)+X_i(node-nx+3)*DY(node-nx+3)
             rhs(node+4)=X_i(node+4)*DD(node+4)+X_i(node+5)*DX(node+4) &
                  + X_i(node+nx+4)*DY(node+4)+X_i(node+nxy+4)*DZ(node+4) &
                  + X_i(node+3)*DX(node+3)+X_i(node-nx+4)*DY(node-nx+4)
             rhs(node+5)=X_i(node+5)*DD(node+5)+X_i(node+6)*DX(node+5) &
                  + X_i(node+nx+5)*DY(node+5)+X_i(node+nxy+5)*DZ(node+5) &
                  + X_i(node+4)*DX(node+4)+X_i(node-nx+5)*DY(node-nx+5)
             rhs(node+6)=X_i(node+6)*DD(node+6)+X_i(node+7)*DX(node+6) &
                  + X_i(node+nx+6)*DY(node+6)+X_i(node+nxy+6)*DZ(node+6) &
                  + X_i(node+5)*DX(node+5)+X_i(node-nx+6)*DY(node-nx+6)
             rhs(node+7)=X_i(node+7)*DD(node+7)+X_i(node+8)*DX(node+7) &
                  + X_i(node+nx+7)*DY(node+7)+X_i(node+nxy+7)*DZ(node+7) &
                  + X_i(node+6)*DX(node+6)+X_i(node-nx+7)*DY(node-nx+7)
          enddo
          !$OMP END DO
       endif
       ! ... bottom k layer:  k=1, j=1
       ! ... cell i=1 excluded
       !$OMP DO           
       do node=2, nx
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node+nx)*DY(node)+X_i(node+nxy)*DZ(node) &
               + X_i(node-1)*DX(node-1)
       enddo
       !$OMP END DO
       ! ... bottom k layer:  k=1, j=1, i=1
       rhs(1)=X_i(1)*DD(1)+X_i(2)*DX(1)+X_i(nx+1)*DY(1)+X_i(nxy+1)*DZ(1)
       ! ... top k layer: k=nz
       ! ... row j=ny excluded
       if (m>0) then
          !$OMP DO           
          do node=n_rows-nxy+1, n_rows-nxy+m
             rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + X_i(node+1)*DX(node)+X_i(node+nx)*DY(node)
          enddo
          !$OMP END DO
       endif
       if (nxy-nx>=mod_no) then
          !$OMP DO           
          do node=n_rows-nxy+m+1, n_rows-nx, mod_no
             rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
                  + X_i(node+1)*DX(node)+X_i(node+nx)*DY(node)
             rhs(node+1)=X_i(node+1)*DD(node+1)+X_i(node)*DX(node) &
                  + X_i(node-nx+1)*DY(node-nx+1) &
                  + X_i(node-nxy+1)*DZ(node-nxy+1) &
                  + X_i(node+2)*DX(node+1)+X_i(node+nx+1)*DY(node+1)
             rhs(node+2)=X_i(node+2)*DD(node+2)+X_i(node+1)*DX(node+1) &
                  + X_i(node-nx+2)*DY(node-nx+2) &
                  + X_i(node-nxy+2)*DZ(node-nxy+2) &
                  + X_i(node+3)*DX(node+2)+X_i(node+nx+2)*DY(node+2)
             rhs(node+3)=X_i(node+3)*DD(node+3)+X_i(node+2)*DX(node+2) &
                  + X_i(node-nx+3)*DY(node-nx+3) &
                  + X_i(node-nxy+3)*DZ(node-nxy+3) &
                  + X_i(node+4)*DX(node+3)+X_i(node+nx+3)*DY(node+3)
             rhs(node+4)=X_i(node+4)*DD(node+4)+X_i(node+3)*DX(node+3) &
                  + X_i(node-nx+4)*DY(node-nx+4) &
                  + X_i(node-nxy+4)*DZ(node-nxy+4) &
                  + X_i(node+5)*DX(node+4)+X_i(node+nx+4)*DY(node+4)
             rhs(node+5)=X_i(node+5)*DD(node+5)+X_i(node+4)*DX(node+4) &
                  + X_i(node-nx+5)*DY(node-nx+5) &
                  + X_i(node-nxy+5)*DZ(node-nxy+5) &
                  + X_i(node+6)*DX(node+5)+X_i(node+nx+5)*DY(node+5)
             rhs(node+6)=X_i(node+6)*DD(node+6)+X_i(node+5)*DX(node+5) &
                  + X_i(node-nx+6)*DY(node-nx+6) &
                  + X_i(node-nxy+6)*DZ(node-nxy+6) &
                  + X_i(node+7)*DX(node+6)+X_i(node+nx+6)*DY(node+6)
             rhs(node+7)=X_i(node+7)*DD(node+7)+X_i(node+6)*DX(node+6) &
                  + X_i(node-nx+7)*DY(node-nx+7) &
                  + X_i(node-nxy+7)*DZ(node-nxy+7) &
                  + X_i(node+8)*DX(node+7)+X_i(node+nx+7)*DY(node+7)
          enddo
          !$OMP END DO
       endif
       ! ... top k layer: k=nz, j=ny
       ! ... cell i=nx excluded
       !$OMP DO           
       do node=n_rows-nx+1, n_rows-1
          rhs(node)=X_i(node)*DD(node)+X_i(node-1)*DX(node-1) &
               + X_i(node-nx)*DY(node-nx)+X_i(node-nxy)*DZ(node-nxy) &
               + X_i(node+1)*DX(node)
       enddo
       !$OMP END DO
       !$OMP END PARALLEL
       ! ... top k layer: k=nz, j=ny, i=nx
       rhs(n_rows)=X_i(n_rows)*DD(n_rows)+X_i(n_rows-1)*DX(n_rows-1) &
            + X_i(n_rows-nx)*DY(n_rows-nx)+X_i(n_rows-nxy)*DZ(n_rows-nxy)
    case(2)
       ! ... 2-D problem (based on nx>1, ny>1)
       ! ... exclude rows: j=1,ny
       m=mod(n_rows-2*nx, mod_no)
       !$OMP PARALLEL DEFAULT(NONE) &
       !$OMP SHARED(rhs,X_i,DD,DX,DY) PRIVATE(node) &
       !$OMP FIRSTPRIVATE(m,mod_no,nx,n_rows)
       if (m>0) then
          !$OMP DO           
          do node=nx+1, nx+m
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)
          enddo
          !$OMP END DO
       endif
       if (n_rows-2*nx>=mod_no) then
          !$OMP DO           
          do node=nx+m+1, n_rows-nx, mod_no
             rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
                  + X_i(node+nx)*DY(node)+X_i(node-1)*DX(node-1) &
                  + X_i(node-nx)*DY(node-nx)
             rhs(node+1)=X_i(node+1)*DD(node+1)+X_i(node+2)*DX(node+1) &
                  + X_i(node+nx+1)*DY(node+1)+X_i(node)*DX(node) &
                  + X_i(node-nx+1)*DY(node-nx+1)
             rhs(node+2)=X_i(node+2)*DD(node+2)+X_i(node+3)*DX(node+2) &
                  + X_i(node+nx+2)*DY(node+2)+X_i(node+1)*DX(node+1) &
                  + X_i(node-nx+2)*DY(node-nx+2)
             rhs(node+3)=X_i(node+3)*DD(node+3)+X_i(node+4)*DX(node+3) &
                  + X_i(node+nx+3)*DY(node+3)+X_i(node+2)*DX(node+2) &
                  + X_i(node-nx+3)*DY(node-nx+3)
             rhs(node+4)=X_i(node+4)*DD(node+4)+X_i(node+5)*DX(node+4) &
                  + X_i(node+nx+4)*DY(node+4)+X_i(node+3)*DX(node+3) &
                  + X_i(node-nx+4)*DY(node-nx+4)
             rhs(node+5)=X_i(node+5)*DD(node+5)+X_i(node+6)*DX(node+5) &
                  + X_i(node+nx+5)*DY(node+5)+X_i(node+4)*DX(node+4) &
                  + X_i(node-nx+5)*DY(node-nx+5)
             rhs(node+6)=X_i(node+6)*DD(node+6)+X_i(node+7)*DX(node+6) &
                  + X_i(node+nx+6)*DY(node+6)+X_i(node+5)*DX(node+5) &
                  + X_i(node-nx+6)*DY(node-nx+6)
             rhs(node+7)=X_i(node+7)*DD(node+7)+X_i(node+8)*DX(node+7) &
                  + X_i(node+nx+7)*DY(node+7)+X_i(node+6)*DX(node+6) &
                  + X_i(node-nx+7)*DY(node-nx+7)
          enddo
          !$OMP END DO
       endif
       ! ... j=1
       ! ... cell i=1 excluded
       !$OMP DO           
       do node=2, nx
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node+nx)*DY(node)+X_i(node-1)*DX(node-1)
       enddo
       !$OMP END DO
       ! ... j=1, i=1 
       rhs(1)=X_i(1)*DD(1)+X_i(2)*DX(1)+X_i(1+nx)*DY(1)
       ! ... j=ny
       ! ... cell i=nx excluded
       !$OMP DO           
       do node=n_rows-nx+1, n_rows-1
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node-1)*DX(node-1)+X_i(node-nx)*DY(node-nx)
       enddo
       !$OMP END DO
       !$OMP END PARALLEL
       ! ... j=ny, i=nx
       rhs(n_rows)=X_i(n_rows)*DD(n_rows)+X_i(n_rows-1)*DX(n_rows-1) &
            + X_i(n_rows-nx)*DY(n_rows-nx)
    case(1)
       ! ... 1-D problem (based on nx>1)
       ! ... exclude i=1, i=nx
       do node=2, nx-1
          rhs(node)=X_i(node)*DD(node)+X_i(node+1)*DX(node) &
               + X_i(node-1)*DX(node-1)
       enddo
       ! ... i=1
       rhs(1)=X_i(1)*DD(1)+X_i(2)*DX(1)
       ! ... i=nx
       rhs(nx)=X_i(nx)*DD(nx)+X_i(nx-1)*DX(nx-1)
    end select
    ! ... 
  end subroutine a_multiply

end module MAT_VEC_MULT

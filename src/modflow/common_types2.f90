! ... FILE COMMON_TYPES, PCGN SOLVER
! ... Modules for shared information 
! ...
! ... R.L. Naff 
! ...
! ... VERSION 1.0 NOVEMBER 2007
! ... 

module common_parameters
  implicit none
  ! ... kv: precision of variables used in assembly
  integer, parameter :: kv=selected_real_kind(p=10)
  ! ... common numbers
  real(kind=kv), parameter :: n0=0.0_kv, n1=1.0_kv, n2=2.0_kv, n3=3.0_kv, &
       n4=4.0_kv, n5=5.0_kv, n6=6.0_kv, n7=7.0_kv, n8=8.0_kv, n9=9.0_kv, &
       n10=10.0_kv, n100=100.0_kv
  ! ... common fractions
  real(kind=kv), parameter :: f2=0.5_kv, f3=n1/n3, f4=0.25_kv, f5=0.2_kv, &
       f6=n1/n6, f7=n1/n7, f8=0.125_kv, f9=n1/n9, f10=0.1_kv
  ! ... machine smallest number
  real(kind=kv), parameter :: machine_epsilon=epsilon(n0)
  real(kind=kv), parameter :: small=n100*machine_epsilon
  real(kind=kv), parameter :: MZ=tiny(n0)
end module common_parameters

module common_solver_types
  use common_parameters
  implicit none
  integer, save :: dim, nx, ny, nz, n_rows
  ! ... arrays
  real(kind=kv), dimension(:), pointer :: DD, DX, DY, DZ
  real(kind=kv), dimension(:), pointer :: dx0, dy0, dz0       
  real(kind=kv), dimension(:), pointer :: dx1, dy1, dz1
  real(kind=kv), dimension(:), pointer :: diag
end module common_solver_types

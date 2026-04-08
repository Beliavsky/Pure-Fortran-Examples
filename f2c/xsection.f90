program main
use iso_fortran_env, only : int64
use mt19937_runif, only : rng_seed, runif_1d
implicit none
integer, parameter :: dp = kind(1.0d0), n1 = 10, n2 = 20
real(dp) :: x(n1, n2)
integer :: i
call rng_seed(42_int64)
do i=1,n2
   x(:, i) = runif_1d(n1)
end do
print*,minval(x)
print*,minval(x(::2, ::3))
print*,minval(x(1:2, 2:4))
print*,minval(x(1:2:2, 2:4:2))
end program main

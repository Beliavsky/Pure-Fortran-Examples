module m
use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none
public :: dp, mult
contains
pure real(kind=dp) function mult(n, x) result(x2)
integer, intent(in) :: n
real(kind=dp), intent(in) :: x
x2 = real(n, dp) * x
end function mult
end module m

program main
use m, only: dp, mult
implicit none
print*,mult(3, 5.0_dp)
end program main
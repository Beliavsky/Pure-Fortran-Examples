module m
implicit none
contains
function sumsq(x) result(y)
real, intent(in) :: x(:)
real :: y
y = sum(x**2)
end function sumsq
end module m

program main
use m
implicit none
real :: x(2) = [10.0, 20.0], y(2) = [30.0, 40.0]
print*,sumsq(x-y)
print*,sumsq(x-[30.0, 40.0])
print*,sumsq(x-30.0)
print*,sumsq(x-30)
print*,sumsq([10.0, 20.0] - [30.0, 40.0])
end program main

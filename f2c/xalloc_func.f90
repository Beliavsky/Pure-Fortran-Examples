module m
implicit none
contains
function positive(x) result(y)
real, intent(in) :: x(:)
real, allocatable :: y(:)
y = pack(x, x > 0)
end function positive
end module m

program main
use m
implicit none
print*,positive([-10.0, 0.0, 10.0, 20.0])
end program main

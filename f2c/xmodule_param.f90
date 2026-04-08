module m
implicit none
real, parameter :: two = 2.0
contains
real function twice(x)
real, intent(in) :: x
twice = two*x
end function twice
end module m

program main
use m
implicit none
print*,twice(3.2)
end program main

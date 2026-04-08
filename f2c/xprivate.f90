module m1
implicit none
private
real :: x = 1.1
end module m1

program main
use m1
implicit none
real :: x = 2.2
print*,x
end program main
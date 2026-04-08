program main
implicit none
integer, parameter :: n1 = 3
real :: x(n1), y(n1) = [10.0, 20.0, 30.0]
integer :: ix(n1)
logical :: lx(n1), mask(n1) = [.true., .false., .true.]

x = sum(y)
print*,x
x = sum(y,1)
print*,x
x = product(y)
print*,x
x = product(y,1)
print*,x
x = maxval(y)
print*,x
x = maxval(y,1)
print*,x
x = minval(y)
print*,x
x = minval(y,1)
print*,x
x = y
print*,y
end program main

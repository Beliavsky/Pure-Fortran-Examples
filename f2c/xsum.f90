program main
implicit none
real :: x(3) = [10.0, 20.0, 30.0], y(2,3)
print*,sum(x)
print*,sum(x, x > 15.0)
! print*,sum(x, dim=1)
y(1,:) = x
y(2,:) = 10*x
print*,y
print*,sum(y)
end program main
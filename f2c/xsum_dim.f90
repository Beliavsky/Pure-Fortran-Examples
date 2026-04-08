program main
implicit none
real :: x(3) = [10.0, 20.0, 30.0], y(2,3)
print*,sum(x) ! overall sum
print*,sum(x, dim=1)
y(1,:) = x
y(2,:) = 10*x
print*,y
print*,sum(y)
print*,sum(y, dim=1)
print*,sum(y, dim=2)
end program main
program main
implicit none
real :: x(3) = [1.0, 2.0, 3.0], y(2,3)
print*,product(x)
print*,product(x, dim=1)
y(1,:) = x
y(2,:) = 10*x
print*,y
print*,product(y)
print*,product(y, dim=1)
print*,product(y, dim=2)
print*,product([real ::])
end program main
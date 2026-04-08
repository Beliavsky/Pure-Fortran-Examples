program main
implicit none
real :: x(3) = [10.0, 20.0, 30.0], y(2,3)
y(1,:) = x
y(2,:) = 10*x
print*,y(1,:)
print*,y(2,:)
end program main
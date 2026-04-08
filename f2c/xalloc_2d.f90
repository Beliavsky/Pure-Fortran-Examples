program main
implicit none
real, allocatable :: x(:,:), y(:,:)
allocate (x(2, 3), source = 4.5)
print*,x
y = 10*x
print*,y
print*,shape(x), shape(y)
print*,rank(x), rank(y)
end program main
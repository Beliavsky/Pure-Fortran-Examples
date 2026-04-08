implicit none
real :: x(2)
real, allocatable :: y(:)
allocate(y, mold=x)
print*,size(y)
end


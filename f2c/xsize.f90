program main
implicit none
real :: x(3, 4)
print*,size(x), shape(x)
print*,size(x(::2, ::3)), shape(x(::2, ::3))
print*,size(x(1:2, 2:4)), shape(x(1:2, 2:4))
print*,size(x(1:2:2, 2:4:2)), shape(x(1:2:2, 2:4:2))
end program main
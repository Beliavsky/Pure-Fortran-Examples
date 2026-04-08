program main
implicit none
real, parameter :: x(*) = [-0.7, -0.5, -0.3, 0.1, 0.5, 0.7]
print*,nint(x)
print*,anint(x)
print*,aint(x)
print*,ceiling(x)
print*,floor(x)
end program main
program main
implicit none
real, parameter :: r0(*) = [real ::]
print*,size(r0)
print*,shape(r0)
print*,rank(r0)
print*,sum(r0)
print*,product(r0)
end program main

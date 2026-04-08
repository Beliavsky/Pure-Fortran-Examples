program main
implicit none
type cc
   real :: x, y
end type cc
type(cc), allocatable :: z(:)
allocate (z(2))
z(1)%x = 3.0
z(1)%y = 4.0
print*,z(1)%x, z(1)%y
z(2)%x = 30.0
z(2)%y = 40.0
print*,z(2)
print*,z
end program main

program main
implicit none
type cc
   real :: x, y
end type cc
type(cc) :: z = cc(10.0, 20.0)
print*,z
z%x = 3.0
z%y = 4.0
print*,z
print*,z%x, z%y
end program main
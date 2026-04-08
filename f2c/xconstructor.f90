program main
implicit none
integer, parameter :: n = 3
integer :: i
real :: x = 5.6, y(n), z(n)
y = (/(x, i=1,n)/)
z = [(x, i=1,n)]
print*,y
print*,z
end program main

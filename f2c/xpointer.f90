program main
implicit none
real, target :: x, y(4)
real, pointer :: xp => null(), yp(:) => null(), z(:) => null()
xp => x
x = 2.3
print*,xp
xp = 10*xp
print*,xp, x
y = [10.0, 20.0, 30.0, 40.0]
yp => y(2:3)
yp = 10*yp
print*,yp, y
allocate (z(3))
z = [5.0, 10.0, 15.0]
print*,z
deallocate (z)
end program main

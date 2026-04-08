program data_scalars
implicit none

integer :: n
real :: x, y
character(len=12) :: name

data n /5/
data x /2.5/
data y /7.25/
data name /'fortran     '/

print *, "n    = ", n
print *, "x    = ", x
print *, "y    = ", y
print *, "name = [", trim(name), "]"

end program data_scalars

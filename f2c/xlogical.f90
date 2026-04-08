! f2c xlogical.f90
implicit none
integer, parameter :: n=3
logical :: x(n)
real :: y(n) = [10.0, 20.0, 30.0]
x = [.true., .false., .true.]
print*,x
print*,pack(y, x)
print*,sum(y)
print*,sum(y, mask=x)
print*,sum(y**2, mask=x)
print*,.true. .eqv. .true., .true. .eqv. .false.
end

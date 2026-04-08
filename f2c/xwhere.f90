program main
implicit none
logical :: mask(4)
real :: x(4), y(4)
mask = [.true., .false., .true., .false.]
x = [10.0,20.0,30.0,40.0]
y = -1.0
where (.not. mask) y = 0.0
print *, y
where (mask) y = x/y
print *, y
end program main

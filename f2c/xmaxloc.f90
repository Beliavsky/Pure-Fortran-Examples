program main
implicit none
real :: x(3) = [10.0, 30.0, 20.0]
integer :: i(1)
i = maxloc(x)
print*,i
i(1) = maxloc(x, dim=1)
print*,i
end program main
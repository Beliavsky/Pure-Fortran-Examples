program main
implicit none
integer, parameter :: dp = kind(1.0d0), n1 = 1000, n2 = 3
real(kind=dp) :: x(n1, n2), xr(n1-1, n2)
call random_number(x)
xr = x(2:, :)/x(:n1-1, :) - 1
print*,x(1,:)
print*,x(2,:)
print*,xr(1,:)
print*,x(2,:)/x(1,:) - 1 - xr(1,:)
end program main
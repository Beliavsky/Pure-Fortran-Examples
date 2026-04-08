module m
implicit none
contains
function period_sums(xx,nperiod) result(yy)
real, intent(in) :: xx(:)
integer, intent(in) :: nperiod
real :: yy(max(0,size(xx,1)-nperiod+1))
integer :: i, imin
do i=1,size(yy)
   imin = i
   yy(i) = sum(xx(imin:imin+nperiod-1))
end do
end function period_sums
end module m

program main
use m
implicit none
integer, parameter :: n1 = 5, n2 = 3, nperiod = 2
integer :: i, j
real :: xi, x(n1)
real, allocatable :: y(:)
do i=1,n1
   xi = real(10*i)
   do j=1,n2
      x(i) = xi**2
   end do
   print "(*(f10.1))", x(i)
end do
y = period_sums(x, nperiod)
print*
do i=1,size(y, 1)
   print "(*(f10.1))", y(i)
end do
end program main

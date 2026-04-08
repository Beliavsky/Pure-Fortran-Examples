module m
implicit none
contains
subroutine sub(x)
real, intent(in) :: x(:)
integer :: i
do i=1,size(x)
   print*,i,x(i)
end do
end subroutine sub

subroutine sub1(x)
real, intent(in) :: x(0:)
integer :: i
do i=lbound(x, 1), ubound(x, 1)
   print*,i,x(i)
end do
end subroutine sub1
end module m

program main
use m
implicit none
real :: x(-2:4)
integer :: i
do i=lbound(x, dim=1), ubound(x, dim=1)
   x(i) = 10.0*i
   print*,i,x(i)
end do
print*
call sub(x)
print*
call sub1(x)
end program main
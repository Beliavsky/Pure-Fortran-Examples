module m
implicit none
contains
pure function tail_real(xx) result(yy)
! return the last element of xx(:)
real, intent(in) :: xx(:)
real             :: yy
integer                   :: n
n = size(xx)
if (n > 0) then
   yy = xx(n)
else
   yy = -1.0
end if
end function tail_real
end module m

program main
use m
implicit none
print*,tail_real([real ::])
end program main

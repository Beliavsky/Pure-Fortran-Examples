module m
implicit none
contains
function repeat_char_vec(n,xx,alternate) result(yy)
integer, intent(in) :: n
character(len=*), intent(in) :: xx(:)
logical, intent(in), optional :: alternate
character(len=len(xx)) :: yy(n*size(xx))
integer :: i,j
if (present(alternate)) then
   if (.not. alternate) then
      yy = (/((xx(j),i=1,n),j=1,size(xx))/)
   else
      yy = (/(xx,i=1,n)/)
   end if
else
   yy = (/(xx,i=1,n)/)
end if
end function repeat_char_vec
end module m

program main
use m
implicit none
print*,repeat_char_vec(3, ["ab", "cd"])
print*,repeat_char_vec(3, ["ab", "cd"], alternate=.true.)
print*,repeat_char_vec(3, ["ab", "cd"], alternate=.false.)
end program main

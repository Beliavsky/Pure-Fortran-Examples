module m
implicit none
contains
function repeat_char_vec(n,xx) result(yy)
integer, intent(in) :: n
character(len=*), intent(in) :: xx(:)
character(len=len(xx)) :: yy(n*size(xx))
integer :: i
yy = (/(xx,i=1,n)/)
end function repeat_char_vec
end module m

program main
use m
implicit none
print*,repeat_char_vec(3, ["ab", "cd"])
end program main

module m
implicit none
contains
elemental function str(i,ndigits,prefix) result(ch)
integer, intent(in) :: i
integer, intent(in), optional :: ndigits
character(len=*), intent(in), optional :: prefix
character(len=32) :: ch
write(ch,"(i0)") i
if (present(prefix)) ch = trim(prefix) // trim(ch)
end function str

elemental function char_plus_int(c1,i) result(c12)
character(len=*), intent(in) :: c1
integer, intent(in) :: i
character(len=*), parameter :: cdelim_ = "_"
character(len=len(c1)+len(cdelim_)+20) :: c12
c12 = trim(c1) // cdelim_ // trim(str(i))
end function char_plus_int
end module m

program main
use m
implicit none
print*,char_plus_int("ab", 78)
end program main
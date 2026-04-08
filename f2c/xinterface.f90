module m
implicit none
interface twice
   module procedure twice_real_scalar, twice_real_vec, twice_char_scalar, twice_char_vec
end interface twice
contains
function twice_real_scalar(x) result(x2)
real, intent(in) :: x
real :: x2
x2 = 2*x
end function twice_real_scalar

function twice_real_vec(x) result(x2)
real, intent(in) :: x(:)
real :: x2(size(x))
x2 = 2*x
end function twice_real_vec

function twice_char_scalar(x) result(x2)
character (len=*), intent(in) :: x
character (len=2*len(x)) :: x2
x2 = x // x
end function twice_char_scalar

function twice_char_vec(x) result(x2)
character (len=*), intent(in) :: x(:)
character (len=2*len(x)) :: x2(size(x))
x2 = x // x
end function twice_char_vec
end module m

program main
use m
implicit none
print*,twice(3.2), twice([10.0, 20.0]), twice("abc")
print*,twice(["ab", "cd"])
end program main

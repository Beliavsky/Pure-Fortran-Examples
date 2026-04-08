module c_demo_mod
use iso_c_binding, only: c_int
implicit none
contains

subroutine print_twice(x) bind(c, name="print_twice")
! print twice the input integer
integer(c_int), value, intent(in) :: x
print *, "x        =", x
print *, "2*x      =", 2 * x
end subroutine print_twice

function add_one(x) result(y) bind(c, name="add_one")
! return x + 1 as a C-callable function
integer(c_int), value, intent(in) :: x
integer(c_int) :: y
y = x + 1
end function add_one

end module c_demo_mod

program main
use iso_c_binding, only: c_int
use c_demo_mod, only: print_twice, add_one
implicit none

integer(c_int) :: n

n = 7
call print_twice(n)
print *, "add_one(n) =", add_one(n)

end program main

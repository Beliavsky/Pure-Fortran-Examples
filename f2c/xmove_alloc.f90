program demo_move_alloc
implicit none

integer, allocatable :: a(:), b(:)
integer :: i

allocate(a(5))
do i = 1, size(a)
   a(i) = 10*i
end do

print *, "initial state"
print *, "allocated(a) =", allocated(a)
print *, "allocated(b) =", allocated(b)
print *, "a =", a
print *

call move_alloc(a, b)

print *, "after call move_alloc(a, b)"
print *, "allocated(a) =", allocated(a)
print *, "allocated(b) =", allocated(b)
print *, "b =", b
print *

allocate(a(3))
a = [1, 2, 3]

print *, "reallocate a"
print *, "a =", a
print *

call append_value(a, 99)

print *, "after append_value(a, 99)"
print *, "a =", a

contains

subroutine append_value(x, val)
integer, allocatable, intent(inout) :: x(:)
integer, intent(in) :: val
integer, allocatable :: temp(:)
integer :: n

if (allocated(x)) then
   n = size(x)
   allocate(temp(n + 1))
   temp(1:n) = x
else
   n = 0
   allocate(temp(1))
end if

temp(n + 1) = val
call move_alloc(temp, x)
end subroutine append_value

end program demo_move_alloc

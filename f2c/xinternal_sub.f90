module m
implicit none
contains
subroutine sub(x)
real, intent(in) :: x
call foo(x)
contains
subroutine foo(x)
real, intent(in) :: x
print*,"x =",x
end subroutine foo
end subroutine sub
end module m

program main
use m
implicit none
call sub(2.3)
end program main

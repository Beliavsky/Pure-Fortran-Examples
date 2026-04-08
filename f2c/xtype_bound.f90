module point_mod
implicit none
private
public :: point2d, make_point

type :: point2d
   real :: x = 0.0
   real :: y = 0.0
contains
   procedure :: shift
   procedure :: scale
   procedure :: sum_xy
end type point2d

contains

function make_point(x, y) result(p)
real, intent(in) :: x, y
type(point2d) :: p

p%x = x
p%y = y
end function make_point

subroutine shift(self, dx, dy)
class(point2d), intent(inout) :: self
real, intent(in) :: dx, dy

self%x = self%x + dx
self%y = self%y + dy
end subroutine shift

subroutine scale(self, a)
class(point2d), intent(inout) :: self
real, intent(in) :: a

self%x = a*self%x
self%y = a*self%y
end subroutine scale

real function sum_xy(self) result(total)
class(point2d), intent(in) :: self

total = self%x + self%y
end function sum_xy

end module point_mod

program main
use point_mod
implicit none

type(point2d) :: p

p = make_point(1.0, 2.0)
call p%shift(3.0, -1.0)
print *, p%x, p%y

call p%scale(10.0)
print *, p%x, p%y

print *, p%sum_xy()

end program main

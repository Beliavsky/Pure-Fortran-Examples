  module m
  implicit none
  interface set_optional
     module procedure set_optional_character
  end interface
  contains
  subroutine set_optional_character(xx,xdef,xopt)
  character(len=*), intent(out)          :: xx
  character(len=*), intent(in)           :: xdef
  character(len=*), intent(in), optional :: xopt
  if (present(xopt)) then
     xx = xopt
  else
     xx = xdef
  end if
  end subroutine set_optional_character

  subroutine p(xname, yname)
  character(len=*), intent(in), optional :: xname, yname
  character(len=100) :: xname_, yname_
  call set_optional(xname_, 'x', xname)
  call set_optional(yname_, 'y', yname)
  print *, trim(xname_), trim(yname_)
  end subroutine p
  end module m

program main
use m
implicit none
call p("a", "b")
end program main

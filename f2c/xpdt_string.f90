module string_box_mod
implicit none
private
public :: string_box_t, make_box

type :: string_box_t(n)
   integer, len :: n
   character(len=n) :: text
contains
   procedure :: show
   procedure :: upper_copy
end type string_box_t

contains

function make_box(s) result(box)
   character(len=*), intent(in) :: s
   type(string_box_t(n=len(s))) :: box

   box%text = s
end function make_box

subroutine show(this, name)
   class(string_box_t(*)), intent(in) :: this
   character(len=*), intent(in) :: name

   print *
   print *, trim(name)
   print *, "  n    =", this%n
   print *, "  text = [", this%text, "]"
end subroutine show

function upper_copy(this) result(out)
   class(string_box_t(*)), intent(in) :: this
   type(string_box_t(n=this%n)) :: out

   integer :: i
   character(len=1) :: ch

   do i = 1, this%n
      ch = this%text(i:i)
      if (ch >= "a" .and. ch <= "z") then
         out%text(i:i) = achar(iachar(ch) - iachar("a") + iachar("A"))
      else
         out%text(i:i) = ch
      end if
   end do
end function upper_copy

end module string_box_mod

program demo_string_box
use string_box_mod
implicit none

type(string_box_t(n=5)) :: a
type(string_box_t(n=:)), allocatable :: b
type(string_box_t(n=:)), allocatable :: c
character(len=:), allocatable :: msg

type(string_box_t(n=5)) :: a_upper
type(string_box_t(n=:)), allocatable :: b_upper
type(string_box_t(n=:)), allocatable :: c_upper

a%text = "hello"

msg = "parameterized derived types"
allocate(string_box_t(n=len(msg)) :: b)
b%text = msg

allocate(c, source=make_box("fortran pdt"))

call a%show("a: fixed length box")
call b%show("b: deferred length box allocated at runtime")
call c%show("c: box returned by make_box")

a_upper = a%upper_copy()
allocate(b_upper, source=b%upper_copy())
allocate(c_upper, source=c%upper_copy())

call a_upper%show("uppercase copy of a")
call b_upper%show("uppercase copy of b")
call c_upper%show("uppercase copy of c")

end program demo_string_box

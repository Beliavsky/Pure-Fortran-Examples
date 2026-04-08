program main
implicit none
type t
   real :: x(2)
end type t
type u
   type(t) :: z
end type u
type(u) :: uu
uu%z%x(1) = 100.0
print*,uu%z%x(1)
end program main
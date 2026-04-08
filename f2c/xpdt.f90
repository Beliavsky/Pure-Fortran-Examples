module pdt_mod
implicit none
private
public :: vector_t, show

integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)

type :: vector_t(k, n)
   integer, kind :: k = dp
   integer, len  :: n = 0
   real(kind=k) :: x(n)
end type vector_t

interface show
   module procedure show_sp
   module procedure show_dp
end interface show

contains

subroutine show_sp(this, name)
   type(vector_t(k=sp, n=*)), intent(in) :: this
   character(len=*), intent(in) :: name

   print *
   print *, trim(name)
   print *, "  kind parameter k   =", this%k
   print *, "  length parameter n =", this%n
   print *, "  values             =", this%x
end subroutine show_sp

subroutine show_dp(this, name)
   type(vector_t(k=dp, n=*)), intent(in) :: this
   character(len=*), intent(in) :: name

   print *
   print *, trim(name)
   print *, "  kind parameter k   =", this%k
   print *, "  length parameter n =", this%n
   print *, "  values             =", this%x
end subroutine show_dp

end module pdt_mod

program demo_pdt
use pdt_mod
implicit none

type(vector_t(k=kind(1.0),   n=3)) :: a
type(vector_t(k=kind(1.0d0), n=5)) :: b
type(vector_t(k=kind(1.0d0), n=:)), allocatable :: c

integer :: i

do i = 1, a%n
   a%x(i) = real(i, kind(1.0))
end do

do i = 1, b%n
   b%x(i) = 10.0d0 * real(i, kind(1.0d0))
end do

allocate(vector_t(k=kind(1.0d0), n=4) :: c)

do i = 1, c%n
   c%x(i) = 100.0d0 + real(i, kind(1.0d0))
end do

call show(a, "a: single precision, length 3")
call show(b, "b: double precision, length 5")
call show(c, "c: double precision, deferred length allocated as 4")

print *
print *, "sum(a%x) =", sum(a%x)
print *, "sum(b%x) =", sum(b%x)
print *, "sum(c%x) =", sum(c%x)

end program demo_pdt

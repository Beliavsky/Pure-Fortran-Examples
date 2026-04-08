module pdt_matrix_mod
implicit none
private
public :: matrix_t, make_matrix

type :: matrix_t(nrow, ncol)
   integer, len :: nrow, ncol
   real :: x(nrow, ncol)
contains
   procedure :: show
   procedure :: fill_seq
   procedure :: transpose_copy
end type matrix_t

contains

function make_matrix(a) result(mat)
   real, intent(in) :: a(:, :)
   type(matrix_t(nrow=size(a,1), ncol=size(a,2))) :: mat

   mat%x = a
end function make_matrix

subroutine show(this, name)
   class(matrix_t(*,*)), intent(in) :: this
   character(len=*), intent(in) :: name
   integer :: i

   print *
   print *, trim(name)
   print *, "  nrow =", this%nrow
   print *, "  ncol =", this%ncol
   print *, "  values:"
   do i = 1, this%nrow
      print *, this%x(i, :)
   end do
end subroutine show

subroutine fill_seq(this, start, step)
   class(matrix_t(*,*)), intent(inout) :: this
   real, intent(in) :: start, step
   integer :: i, j
   real :: val

   val = start
   do i = 1, this%nrow
      do j = 1, this%ncol
         this%x(i, j) = val
         val = val + step
      end do
   end do
end subroutine fill_seq

function transpose_copy(this) result(out)
   class(matrix_t(*,*)), intent(in) :: this
   type(matrix_t(nrow=this%ncol, ncol=this%nrow)) :: out

   out%x = transpose(this%x)
end function transpose_copy

end module pdt_matrix_mod

program demo_pdt_matrix
use pdt_matrix_mod
implicit none

type(matrix_t(nrow=2, ncol=3)) :: a
type(matrix_t(nrow=3, ncol=2)) :: at
type(matrix_t(nrow=:, ncol=:)), allocatable :: b
type(matrix_t(nrow=:, ncol=:)), allocatable :: bt

real :: raw(2, 2)

call a%fill_seq(1.0, 1.0)
call a%show("a: fixed 2 x 3 matrix")

at = a%transpose_copy()
call at%show("transpose of a")

allocate(matrix_t(nrow=3, ncol=4) :: b)
call b%fill_seq(10.0, 0.5)
call b%show("b: deferred-size allocatable matrix")

allocate(bt, source=b%transpose_copy())
call bt%show("transpose of b")

raw = reshape([1.0, 3.0, 2.0, 4.0], [2, 2])
deallocate(b)
allocate(b, source=make_matrix(raw))
call b%show("b reassigned from make_matrix(raw)")

print *
print *, "sum(a%x)  =", sum(a%x)
print *, "sum(at%x) =", sum(at%x)
print *, "sum(b%x)  =", sum(b%x)
print *, "sum(bt%x) =", sum(bt%x)

end program demo_pdt_matrix

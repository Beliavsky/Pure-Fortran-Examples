module pdt_matrix_kind_mod
implicit none
private
public :: sp, dp, matrix_t
public :: show, fill_seq, transpose_copy
public :: make_matrix_sp, make_matrix_dp

integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)

type :: matrix_t(k, nrow, ncol)
   integer, kind :: k = dp
   integer, len :: nrow, ncol
   real(kind=k) :: x(nrow, ncol)
end type matrix_t

interface show
   module procedure show_sp
   module procedure show_dp
end interface show

interface fill_seq
   module procedure fill_seq_sp
   module procedure fill_seq_dp
end interface fill_seq

interface transpose_copy
   module procedure transpose_copy_sp
   module procedure transpose_copy_dp
end interface transpose_copy

contains

function make_matrix_sp(a) result(mat)
   real(kind=sp), intent(in) :: a(:, :)
   type(matrix_t(k=sp, nrow=size(a,1), ncol=size(a,2))) :: mat

   mat%x = a
end function make_matrix_sp

function make_matrix_dp(a) result(mat)
   real(kind=dp), intent(in) :: a(:, :)
   type(matrix_t(k=dp, nrow=size(a,1), ncol=size(a,2))) :: mat

   mat%x = a
end function make_matrix_dp

subroutine show_sp(this, name)
   type(matrix_t(k=sp, nrow=*, ncol=*)), intent(in) :: this
   character(len=*), intent(in) :: name
   integer :: i

   print *
   print *, trim(name)
   print *, "  kind =", this%k, " nrow =", this%nrow, " ncol =", this%ncol
   do i = 1, this%nrow
      print *, this%x(i, :)
   end do
end subroutine show_sp

subroutine show_dp(this, name)
   type(matrix_t(k=dp, nrow=*, ncol=*)), intent(in) :: this
   character(len=*), intent(in) :: name
   integer :: i

   print *
   print *, trim(name)
   print *, "  kind =", this%k, " nrow =", this%nrow, " ncol =", this%ncol
   do i = 1, this%nrow
      print *, this%x(i, :)
   end do
end subroutine show_dp

subroutine fill_seq_sp(this, start, step)
   type(matrix_t(k=sp, nrow=*, ncol=*)), intent(inout) :: this
   real(kind=sp), intent(in) :: start, step
   integer :: i, j
   real(kind=sp) :: val

   val = start
   do i = 1, this%nrow
      do j = 1, this%ncol
         this%x(i, j) = val
         val = val + step
      end do
   end do
end subroutine fill_seq_sp

subroutine fill_seq_dp(this, start, step)
   type(matrix_t(k=dp, nrow=*, ncol=*)), intent(inout) :: this
   real(kind=dp), intent(in) :: start, step
   integer :: i, j
   real(kind=dp) :: val

   val = start
   do i = 1, this%nrow
      do j = 1, this%ncol
         this%x(i, j) = val
         val = val + step
      end do
   end do
end subroutine fill_seq_dp

function transpose_copy_sp(this) result(out)
   type(matrix_t(k=sp, nrow=*, ncol=*)), intent(in) :: this
   type(matrix_t(k=sp, nrow=this%ncol, ncol=this%nrow)) :: out

   out%x = transpose(this%x)
end function transpose_copy_sp

function transpose_copy_dp(this) result(out)
   type(matrix_t(k=dp, nrow=*, ncol=*)), intent(in) :: this
   type(matrix_t(k=dp, nrow=this%ncol, ncol=this%nrow)) :: out

   out%x = transpose(this%x)
end function transpose_copy_dp

end module pdt_matrix_kind_mod

program demo_pdt_matrix_kind
use pdt_matrix_kind_mod
implicit none

type(matrix_t(k=sp, nrow=2, ncol=3)) :: a_sp
type(matrix_t(k=sp, nrow=3, ncol=2)) :: at_sp
type(matrix_t(k=dp, nrow=2, ncol=2)) :: a_dp
type(matrix_t(k=dp, nrow=2, ncol=2)) :: at_dp

type(matrix_t(k=sp, nrow=:, ncol=:)), allocatable :: b_sp
type(matrix_t(k=sp, nrow=:, ncol=:)), allocatable :: bt_sp
type(matrix_t(k=dp, nrow=:, ncol=:)), allocatable :: b_dp
type(matrix_t(k=dp, nrow=:, ncol=:)), allocatable :: bt_dp

real(kind=sp) :: raw_sp(2, 2)
real(kind=dp) :: raw_dp(2, 3)

call fill_seq(a_sp, 1.0_sp, 1.0_sp)
call show(a_sp, "a_sp: single precision fixed 2 x 3")

at_sp = transpose_copy(a_sp)
call show(at_sp, "transpose of a_sp")

call fill_seq(a_dp, 10.0_dp, 0.5_dp)
call show(a_dp, "a_dp: double precision fixed 2 x 2")

at_dp = transpose_copy(a_dp)
call show(at_dp, "transpose of a_dp")

allocate(matrix_t(k=sp, nrow=3, ncol=4) :: b_sp)
call fill_seq(b_sp, 100.0_sp, 2.0_sp)
call show(b_sp, "b_sp: allocatable single precision 3 x 4")

allocate(bt_sp, source=transpose_copy(b_sp))
call show(bt_sp, "transpose of b_sp")

allocate(matrix_t(k=dp, nrow=3, ncol=2) :: b_dp)
call fill_seq(b_dp, 1000.0_dp, 10.0_dp)
call show(b_dp, "b_dp: allocatable double precision 3 x 2")

allocate(bt_dp, source=transpose_copy(b_dp))
call show(bt_dp, "transpose of b_dp")

raw_sp = reshape([1.0_sp, 3.0_sp, 2.0_sp, 4.0_sp], [2, 2])
deallocate(b_sp)
allocate(b_sp, source=make_matrix_sp(raw_sp))
call show(b_sp, "b_sp rebuilt from make_matrix_sp")

raw_dp = reshape([1.0_dp, 4.0_dp, 2.0_dp, 5.0_dp, 3.0_dp, 6.0_dp], [2, 3])
deallocate(b_dp)
allocate(b_dp, source=make_matrix_dp(raw_dp))
call show(b_dp, "b_dp rebuilt from make_matrix_dp")

print *
print *, "sum(a_sp%x)  =", sum(a_sp%x)
print *, "sum(at_sp%x) =", sum(at_sp%x)
print *, "sum(a_dp%x)  =", sum(a_dp%x)
print *, "sum(at_dp%x) =", sum(at_dp%x)
print *, "sum(b_sp%x)  =", sum(b_sp%x)
print *, "sum(bt_sp%x) =", sum(bt_sp%x)
print *, "sum(b_dp%x)  =", sum(b_dp%x)
print *, "sum(bt_dp%x) =", sum(bt_dp%x)

end program demo_pdt_matrix_kind

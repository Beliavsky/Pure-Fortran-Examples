module blas_part
! contains only dnrm2, dswap, idamax & dgemv

! this very much simplified blas module for use by toms 768 is by alan miller
! alan @ vic.cmis.csiro.au    url: www.ozemail.com.au/~milleraj

! latest revision - 19 january 1999

implicit none

integer, parameter, private   :: dp = selected_real_kind(14, 60)
real(dp), parameter, private :: zero = 0.0_dp, one = 1.0_dp

contains

function dnrm2(n, x, incx) result(fn_val)

! euclidean norm of the n-vector stored in x() with storage increment incx.
! if n <= 0 return with result = 0.
! if n >= 1 then incx must be >= 1

! c.l.lawson, 1978 jan 08
! modified to correct failure to update ix, 1/25/92.
! modified 3/93 to return if incx <= 0.
! this version by alan.miller @ vic.cmis.csiro.au
! latest revision - 22 january 1999

! four phase method using two built-in constants that are
! hopefully applicable to all machines.
!     cutlo = maximum of  sqrt(u/eps)  over all known machines.
!     cuthi = minimum of  sqrt(v)      over all known machines.
! where
!     eps = smallest no. such that eps + 1. > 1.
!     u   = smallest positive no.   (underflow limit)
!     v   = largest  no.            (overflow  limit)

! brief outline of algorithm..

! phase 1    scans zero components.
! move to phase 2 when a component is nonzero and <= cutlo
! move to phase 3 when a component is > cutlo
! move to phase 4 when a component is >= cuthi/m
! where m = n for x() real and m = 2*n for complex.

integer, intent(in)   :: n, incx
real(dp), intent(in) :: x(:)
real(dp)             :: fn_val

! local variables
integer   :: i, ix, j, next
real(dp) :: cuthi, cutlo, hitest, sum, xmax

if(n <= 0 .or. incx <= 0) then
  fn_val = zero
  return
end if

! set machine-dependent constants

cutlo = sqrt(tiny(one) / epsilon(one))
cuthi = sqrt(huge(one))

next = 1
sum = zero
i = 1
ix = 1
!                                                begin main loop
20 select case (next)
  case (1)
     if(abs(x(i)) > cutlo) go to 85
     next = 2
     xmax = zero
     go to 20

  case (2)
!                  phase 1.  sum is zero

     if(x(i) == zero) go to 200
     if(abs(x(i)) > cutlo) go to 85

!                               prepare for phase 2.   x(i) is very small.
     next = 3
     go to 105

  case (3)
!                  phase 2.  sum is small.
!                            scale to avoid destructive underflow.

     if(abs(x(i)) > cutlo) then
!                 prepare for phase 3.

       sum = (sum * xmax) * xmax
       go to 85
     end if

  case (4)
     go to 110
end select

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                    common code for phases 2 and 4.
!                    in phase 4 sum is large.  scale to avoid overflow.

110 if(abs(x(i)) <= xmax) go to 115
sum = one + sum * (xmax / x(i))**2
xmax = abs(x(i))
go to 200

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!                  phase 3.  sum is mid-range.  no scaling.

!    for real or d.p. set hitest = cuthi/n
!    for complex      set hitest = cuthi/(2*n)

85 hitest = cuthi / real(n, dp)

do j = ix, n
  if(abs(x(i)) >= hitest) go to 100
  sum = sum + x(i)**2
  i = i + incx
end do
fn_val = sqrt(sum)
return

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!                               prepare for phase 4.
!                               abs(x(i)) is very large
100 ix = j
next = 4
sum = (sum / x(i)) / x(i)
!                               set xmax; large if next = 4, small if next = 3
105 xmax = abs(x(i))

115 sum = sum + (x(i)/xmax)**2

200 ix = ix + 1
i = i + incx
if(ix <= n) go to 20

!             end of main loop.

!             compute square root and adjust for scaling.

fn_val = xmax * sqrt(sum)

return
end function dnrm2

subroutine dswap(n, x, incx, y, incy)

!    interchanges two vectors.

integer, intent(in)       :: n, incx, incy
real(dp), intent(in out) :: x(:), y(:)

! local variables
real(dp) :: temp(n)

if(n <= 0) return
if(incx == 1 .and. incy == 1) then
  temp = x(:n)
  x(:n) = y(:n)
  y(:n) = temp
  return
end if

temp = x(:n*incx:incx)
x(:n*incx:incx) = y(:n*incy:incy)
y(:n*incy:incy) = temp

return
end subroutine dswap

function idamax(n, x, incx) result(fn_val)

!    finds the index of element having max. absolute value.
!    jack dongarra, linpack, 3/11/78.
!    modified 3/93 to return if incx .le. 0.
!    modified 12/3/93, array(1) declarations changed to array(*)

integer, intent(in)   :: n, incx
real(dp), intent(in) :: x(:)
integer               :: fn_val

integer :: imax(1)

fn_val = 0
if(n < 1 .or. incx <= 0) return
fn_val = 1
if(n == 1) return
if(incx == 1) then
  imax = maxloc(abs(x(:n)))
else
  imax = maxloc(abs(x(:n*incx:incx)))
end if
fn_val = imax(1)

return
end function idamax

subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
! elf90 translation by alan miller   31-aug-1997

!    .. scalar arguments ..
real(dp), intent(in)         :: alpha, beta
integer, intent(in)           :: incx, incy, lda, m, n
character(len=1), intent(in) :: trans
!    .. array arguments ..
real(dp), intent(in)         :: a(:,:), x(:)
real(dp), intent(in out)     :: y(:)
!    ..

! purpose
! =======

! dgemv  performs one of the matrix-vector operations

!    y := alpha*a*x + beta*y,   or   y := alpha*a'*x + beta*y,

! where alpha and beta are scalars, x and y are vectors and a is an
! m by n matrix.

! parameters
! ==========

! trans  - character*1.
!          on entry, trans specifies the operation to be performed as
!          follows:

!             trans = 'n' or 'n'   y := alpha*a*x + beta*y.

!             trans = 't' or 't'   y := alpha*a'*x + beta*y.

!             trans = 'c' or 'c'   y := alpha*a'*x + beta*y.

!          unchanged on exit.

! m      - integer.
!          on entry, m specifies the number of rows of the matrix a.
!          m must be at least zero.
!          unchanged on exit.

! n      - integer.
!          on entry, n specifies the number of columns of the matrix a.
!          n must be at least zero.
!          unchanged on exit.

! alpha  - double precision.
!          on entry, alpha specifies the scalar alpha.
!          unchanged on exit.

! a      - double precision array of dimension ( lda, n ).
!          before entry, the leading m by n part of the array a must
!          contain the matrix of coefficients.
!          unchanged on exit.

! lda    - integer.
!          on entry, lda specifies the first dimension of a as declared
!          in the calling (sub) program. lda must be at least max( 1, m ).
!          unchanged on exit.

! x      - double precision array of dimension at least
!          ( 1 + ( n - 1 )*abs( incx ) ) when trans = 'n' or 'n'
!          and at least
!          ( 1 + ( m - 1 )*abs( incx ) ) otherwise.
!          before entry, the incremented array x must contain the
!          vector x.
!          unchanged on exit.

! incx   - integer.
!          on entry, incx specifies the increment for the elements of
!          x. incx must not be zero.
!          unchanged on exit.

! beta   - double precision.
!          on entry, beta specifies the scalar beta. when beta is
!          supplied as zero then y need not be set on input.
!          unchanged on exit.

! y      - double precision array of dimension at least
!          ( 1 + ( m - 1 )*abs( incy ) ) when trans = 'n' or 'n'
!          and at least
!          ( 1 + ( n - 1 )*abs( incy ) ) otherwise.
!          before entry with beta non-zero, the incremented array y
!          must contain the vector y. on exit, y is overwritten by the
!          updated vector y.

! incy   - integer.
!          on entry, incy specifies the increment for the elements of
!          y. incy must not be zero.
!          unchanged on exit.

! level 2 blas routine.

! -- written on 22-october-1986.
!    jack dongarra, argonne national lab.
!    jeremy du croz, nag central office.
!    sven hammarling, nag central office.
!    richard hanson, sandia national labs.

!    .. local scalars ..
real(dp) :: temp
integer   :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
!    .. executable statements ..

!    test the input parameters.

info = 0
if(trans /= 'n' .and. trans /= 'n' .and. trans /= 't' .and. trans /= 't' &
     .and. trans /= 'c' .and. trans /= 'c') then
  info = 1
else if(m < 0) then
  info = 2
else if(n < 0) then
  info = 3
else if(lda < max(1, m)) then
  info = 6
else if(incx == 0) then
  info = 8
else if(incy == 0) then
  info = 11
end if
if(info /= 0) then
  write(*, '(a, i4, a)') ' error number: ', info, ' in blas2 routine dgemv'
  return
end if

!    quick return if possible.

if((m == 0) .or. (n == 0) .or. ((alpha == zero) .and. (beta == one))) return

!    set lenx and leny, the lengths of the vectors x and y, and set
!    up the start points in x and y.

if(trans == 'n' .or. trans == 'n') then
  lenx = n
  leny = m
else
  lenx = m
  leny = n
end if
if(incx > 0) then
  kx = 1
else
  kx = 1 - (lenx - 1)*incx
end if
if(incy > 0) then
  ky = 1
else
  ky = 1 - (leny - 1)*incy
end if

!    start the operations. in this version the elements of a are
!    accessed sequentially with one pass through a.

!    first form y := beta*y.

if(beta /= one) then
  if(incy == 1) then
    if(beta == zero) then
      y(:leny) = zero
    else
      y(:leny) = beta*y(:leny)
    end if
  else
    iy = ky
    if(beta == zero) then
      do i = 1, leny
        y(iy) = zero
        iy = iy + incy
      end do
    else
      do i = 1, leny
        y(iy) = beta*y(iy)
        iy = iy + incy
      end do
    end if
  end if
end if
if(alpha == zero) return
if(trans == 'n' .or. trans == 'n') then

!       form y := alpha*a*x + y.

  jx = kx
  if(incy == 1) then
    do j = 1, n
      if(x(jx) /= zero) then
        temp = alpha*x(jx)
        y(1:m) = y(1:m) + temp*a(1:m, j)
      end if
      jx = jx + incx
    end do
  else
    do j = 1, n
      if(x(jx) /= zero) then
        temp = alpha*x(jx)
        iy = ky
        do i = 1, m
          y(iy) = y(iy) + temp*a(i, j)
          iy = iy + incy
        end do
      end if
      jx = jx + incx
    end do
  end if
else

!       form y := alpha*a'*x + y.

  jy = ky
  if(incx == 1) then
    do j = 1, n
      temp = dot_product(a(1:m, j), x(1:m))
      y(jy) = y(jy) + alpha*temp
      jy = jy + incy
    end do
  else
    do j = 1, n
      temp = zero
      ix = kx
      do i = 1, m
        temp = temp + a(i, j)*x(ix)
        ix = ix + incx
      end do
      y(jy) = y(jy) + alpha*temp
      jy = jy + incy
    end do
  end if
end if

return

!    end of dgemv.

end subroutine dgemv

end module blas_part

program test_blas_part
use blas_part, only: dnrm2, dswap, idamax, dgemv
implicit none

integer, parameter :: dp = selected_real_kind(14, 60)
integer :: nfail
logical, parameter :: show_values = .true.

nfail = 0

call test_dnrm2(nfail)
call test_dswap_unit(nfail)
call test_dswap_stride(nfail)
call test_idamax_unit(nfail)
call test_idamax_stride(nfail)
call test_dgemv_n(nfail)
call test_dgemv_t(nfail)

write(*, '(a)') ''
if(nfail == 0) then
  write(*, '(a)') 'all tests passed'
else
  write(*, '(a, i0)') 'number of failed tests: ', nfail
  error stop 'test failure'
end if

contains

subroutine test_dnrm2(nfail)
integer, intent(in out) :: nfail
real(dp) :: x(3), got, expect

x = [3.0_dp, 4.0_dp, 12.0_dp]
got = dnrm2(3, x, 1)
expect = 13.0_dp
call check_real('dnrm2 unit stride', got, expect, 1.0e-12_dp, nfail)
end subroutine test_dnrm2

subroutine test_dswap_unit(nfail)
integer, intent(in out) :: nfail
real(dp) :: x(3), y(3)

x = [1.0_dp, 2.0_dp, 3.0_dp]
y = [10.0_dp, 20.0_dp, 30.0_dp]
call dswap(3, x, 1, y, 1)
call check_real_vec('dswap unit stride x', x, [10.0_dp, 20.0_dp, 30.0_dp], 1.0e-12_dp, nfail)
call check_real_vec('dswap unit stride y', y, [1.0_dp, 2.0_dp, 3.0_dp], 1.0e-12_dp, nfail)
end subroutine test_dswap_unit

subroutine test_dswap_stride(nfail)
integer, intent(in out) :: nfail
real(dp) :: x(6), y(6)

x = [1.0_dp, -1.0_dp, 2.0_dp, -2.0_dp, 3.0_dp, -3.0_dp]
y = [10.0_dp, -10.0_dp, 20.0_dp, -20.0_dp, 30.0_dp, -30.0_dp]
call dswap(3, x, 2, y, 2)
call check_real_vec('dswap stride x', x, [10.0_dp, -1.0_dp, 20.0_dp, -2.0_dp, 30.0_dp, -3.0_dp], 1.0e-12_dp, nfail)
call check_real_vec('dswap stride y', y, [1.0_dp, -10.0_dp, 2.0_dp, -20.0_dp, 3.0_dp, -30.0_dp], 1.0e-12_dp, nfail)
end subroutine test_dswap_stride

subroutine test_idamax_unit(nfail)
integer, intent(in out) :: nfail
real(dp) :: x(5)
integer :: got

x = [1.0_dp, -7.0_dp, 3.0_dp, 6.0_dp, -2.0_dp]
got = idamax(5, x, 1)
call check_int('idamax unit stride', got, 2, nfail)
end subroutine test_idamax_unit

subroutine test_idamax_stride(nfail)
integer, intent(in out) :: nfail
real(dp) :: x(6)
integer :: got

x = [1.0_dp, 99.0_dp, -5.0_dp, 88.0_dp, 4.0_dp, 77.0_dp]
got = idamax(3, x, 2)
call check_int('idamax stride', got, 2, nfail)
end subroutine test_idamax_stride

subroutine test_dgemv_n(nfail)
integer, intent(in out) :: nfail
real(dp) :: a(2, 3), x(3), y(2), expect(2)

! a = [ [1, 2, 3],
!       [4, 5, 6] ]
a(:, 1) = [1.0_dp, 4.0_dp]
a(:, 2) = [2.0_dp, 5.0_dp]
a(:, 3) = [3.0_dp, 6.0_dp]
x = [1.0_dp, 2.0_dp, 3.0_dp]
y = [10.0_dp, 20.0_dp]
expect = [24.0_dp, 52.0_dp]
call dgemv('n', 2, 3, 1.0_dp, a, 2, x, 1, 1.0_dp, y, 1)
call check_real_vec('dgemv n', y, expect, 1.0e-12_dp, nfail)
end subroutine test_dgemv_n

subroutine test_dgemv_t(nfail)
integer, intent(in out) :: nfail
real(dp) :: a(2, 3), x(2), y(3), expect(3)

! a' * x = [31, 41, 51]
a(:, 1) = [1.0_dp, 4.0_dp]
a(:, 2) = [2.0_dp, 5.0_dp]
a(:, 3) = [3.0_dp, 6.0_dp]
x = [3.0_dp, 7.0_dp]
y = [1.0_dp, 1.0_dp, 1.0_dp]
expect = [32.0_dp, 42.0_dp, 52.0_dp]
call dgemv('t', 2, 3, 1.0_dp, a, 2, x, 1, 1.0_dp, y, 1)
call check_real_vec('dgemv t', y, expect, 1.0e-12_dp, nfail)
end subroutine test_dgemv_t

subroutine check_real(name, got, expect, tol, nfail)
character(len=*), intent(in) :: name
real(dp), intent(in) :: got, expect, tol
integer, intent(in out) :: nfail

if(abs(got - expect) <= tol) then
  write(*, '(a, a)') 'pass: ', trim(name)
  if(show_values) then
    write(*, '(a, es24.16)') '  got    = ', got
    write(*, '(a, es24.16)') '  expect = ', expect
  end if
else
  write(*, '(a, a)') 'fail: ', trim(name)
  write(*, '(a, es24.16)') '  got    = ', got
  write(*, '(a, es24.16)') '  expect = ', expect
  nfail = nfail + 1
end if
end subroutine check_real

subroutine check_real_vec(name, got, expect, tol, nfail)
character(len=*), intent(in) :: name
real(dp), intent(in) :: got(:), expect(:), tol
integer, intent(in out) :: nfail
integer :: i

if(size(got) /= size(expect)) then
  write(*, '(a, a)') 'fail: ', trim(name)
  write(*, '(a)') '  size mismatch'
  nfail = nfail + 1
  return
end if

if(all(abs(got - expect) <= tol)) then
  write(*, '(a, a)') 'pass: ', trim(name)
  if(show_values) then
    do i = 1, size(got)
      write(*, '(a, i0, a, es24.16, a, es24.16)') '  i=', i, ' got=', got(i), ' expect=', expect(i)
    end do
  end if
else
  write(*, '(a, a)') 'fail: ', trim(name)
  do i = 1, size(got)
    write(*, '(a, i0, a, es24.16, a, es24.16)') '  i=', i, ' got=', got(i), ' expect=', expect(i)
  end do
  nfail = nfail + 1
end if
end subroutine check_real_vec

subroutine check_int(name, got, expect, nfail)
character(len=*), intent(in) :: name
integer, intent(in) :: got, expect
integer, intent(in out) :: nfail

if(got == expect) then
  write(*, '(a, a)') 'pass: ', trim(name)
  if(show_values) then
    write(*, '(a, i0)') '  got    = ', got
    write(*, '(a, i0)') '  expect = ', expect
  end if
else
  write(*, '(a, a)') 'fail: ', trim(name)
  write(*, '(a, i0)') '  got    = ', got
  write(*, '(a, i0)') '  expect = ', expect
  nfail = nfail + 1
end if
end subroutine check_int

end program test_blas_part

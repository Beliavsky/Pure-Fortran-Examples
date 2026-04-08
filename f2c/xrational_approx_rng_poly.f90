module kind_mod
implicit none
integer, parameter :: dp = kind(1.0d0)
end module kind_mod

module local_rng_mod
use iso_fortran_env, only: int64, real64
use mt19937_runif, only: rng_seed, runif
implicit none
contains

subroutine seed_rng(seed)
integer, intent(in) :: seed

call rng_seed(int(seed, int64))
end subroutine seed_rng

subroutine local_random_number(x)
real(real64), intent(out) :: x

x = real(runif(), kind(x))
end subroutine local_random_number

end module local_rng_mod

module util_mod
use kind_mod, only: dp
use local_rng_mod, only: seed_rng, local_random_number
implicit none
contains

subroutine set_seed(seed)
integer, intent(in) :: seed

call seed_rng(seed)
end subroutine set_seed

real(kind=dp) function normal_rand()
real(kind=dp) :: u1
real(kind=dp) :: u2
real(kind=dp) :: pi

pi = acos(-1.0_dp)
call local_random_number(u1)
call local_random_number(u2)
if (u1 < 1.0e-12_dp) u1 = 1.0e-12_dp
normal_rand = sqrt(-2.0_dp*log(u1))*cos(2.0_dp*pi*u2)
end function normal_rand

pure real(kind=dp) function rmse(yhat, y)
real(kind=dp), intent(in) :: yhat(:)
real(kind=dp), intent(in) :: y(:)

rmse = sqrt(sum((yhat - y)**2)/real(size(y), dp))
end function rmse

subroutine solve_linear(a_in, b_in, x, info)
real(kind=dp), intent(in) :: a_in(:,:)
real(kind=dp), intent(in) :: b_in(:)
real(kind=dp), intent(out) :: x(:)
integer, intent(out) :: info

integer :: n
integer :: i
integer :: k
integer :: piv
real(kind=dp) :: a(size(a_in,1), size(a_in,2))
real(kind=dp) :: b(size(b_in))
real(kind=dp) :: factor
real(kind=dp) :: tmp
real(kind=dp) :: rowtmp(size(a_in,2))

n = size(b_in)
if (size(a_in,1) /= n .or. size(a_in,2) /= n .or. size(x) /= n) then
   info = -1
   return
end if

a = a_in
b = b_in
info = 0

do k = 1, n - 1
   piv = k
   do i = k + 1, n
      if (abs(a(i,k)) > abs(a(piv,k))) piv = i
   end do

   if (abs(a(piv,k)) < 1.0e-14_dp) then
      info = k
      return
   end if

   if (piv /= k) then
      rowtmp = a(k,:)
      a(k,:) = a(piv,:)
      a(piv,:) = rowtmp
      tmp = b(k)
      b(k) = b(piv)
      b(piv) = tmp
   end if

   do i = k + 1, n
      factor = a(i,k)/a(k,k)
      a(i,k:n) = a(i,k:n) - factor*a(k,k:n)
      b(i) = b(i) - factor*b(k)
   end do
end do

if (abs(a(n,n)) < 1.0e-14_dp) then
   info = n
   return
end if

x(n) = b(n)/a(n,n)
do i = n - 1, 1, -1
   x(i) = (b(i) - dot_product(a(i,i+1:n), x(i+1:n)))/a(i,i)
end do
end subroutine solve_linear

subroutine solve_least_squares(a, y, coef)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(in) :: y(:)
real(kind=dp), intent(out) :: coef(:)

integer :: info
real(kind=dp) :: ata(size(coef), size(coef))
real(kind=dp) :: aty(size(coef))

ata = matmul(transpose(a), a)
aty = matmul(transpose(a), y)
call solve_linear(ata, aty, coef, info)
if (info /= 0) error stop "solve_least_squares failed"
end subroutine solve_least_squares

end module util_mod

module true_func_mod
use kind_mod, only: dp
implicit none
contains

subroutine fill_true_values(x, y, k)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: y(size(x))
real(kind=dp), intent(in) :: k

integer :: i

do i = 1, size(x)
   y(i) = sin(k*x(i))
end do
end subroutine fill_true_values

end module true_func_mod

module fit_mod
use kind_mod, only: dp
use util_mod, only: solve_least_squares
implicit none
contains

subroutine scale_x(x, z)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: z(size(x))
real(kind=dp) :: xmin
real(kind=dp) :: xmax

xmin = minval(x)
xmax = maxval(x)
z = 2.0_dp*(x - xmin)/(xmax - xmin) - 1.0_dp
end subroutine scale_x

subroutine fit_cubic_poly(z, y, coef)
real(kind=dp), intent(in) :: z(:)
real(kind=dp), intent(in) :: y(:)
real(kind=dp), intent(out) :: coef(4)
real(kind=dp) :: a(size(z), 4)

a(:,1) = 1.0_dp
a(:,2) = z
a(:,3) = z**2
a(:,4) = z**3

call solve_least_squares(a, y, coef)
end subroutine fit_cubic_poly

subroutine eval_cubic_poly(z, coef, yhat)
real(kind=dp), intent(in) :: z(:)
real(kind=dp), intent(in) :: coef(4)
real(kind=dp), intent(out) :: yhat(size(z))

yhat = coef(1) + coef(2)*z + coef(3)*z**2 + coef(4)*z**3
end subroutine eval_cubic_poly

end module fit_mod

program main
use kind_mod, only: dp
use util_mod, only: set_seed, normal_rand, rmse
use true_func_mod, only: fill_true_values
use fit_mod, only: scale_x, fit_cubic_poly, eval_cubic_poly
implicit none

integer, parameter :: n = 300
integer, parameter :: seed = 123
real(kind=dp), parameter :: x_min = -4.0_dp
real(kind=dp), parameter :: x_max = 4.0_dp
real(kind=dp), parameter :: noise_sd = 0.15_dp
real(kind=dp), parameter :: k = 2.0_dp

integer :: i
real(kind=dp) :: coef(4)
real(kind=dp) :: x(n)
real(kind=dp) :: y(n)
real(kind=dp) :: y_poly(n)
real(kind=dp) :: y_true(n)
real(kind=dp) :: z(n)

call set_seed(seed)

do i = 1, n
   x(i) = x_min + (x_max - x_min)*real(i - 1, dp)/real(n - 1, dp)
end do

call fill_true_values(x, y_true, k)
do i = 1, n
   y(i) = y_true(i) + noise_sd*normal_rand()
end do

call scale_x(x, z)
call fit_cubic_poly(z, y, coef)
call eval_cubic_poly(z, coef, y_poly)

print *, "cubic polynomial fit with local MT RNG"
print *, "coef:"
print *, coef
print *, "rmse vs true =", rmse(y_poly, y_true)
print *, "rmse vs noisy data =", rmse(y_poly, y)
print *, "first five rows: x, y_true, y, y_poly"
do i = 1, 5
   print *, x(i), y_true(i), y(i), y_poly(i)
end do

end program main

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

subroutine random_number(x)
real(real64), intent(out) :: x

x = real(runif(), kind(x))
end subroutine random_number

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
real(kind=dp) :: u1, u2, pi

pi = acos(-1.0_dp)
call local_random_number(u1)
call local_random_number(u2)
if (u1 < 1.0e-12_dp) u1 = 1.0e-12_dp
normal_rand = sqrt(-2.0_dp*log(u1))*cos(2.0_dp*pi*u2)
end function normal_rand

pure real(kind=dp) function rmse(yhat, y)
real(kind=dp), intent(in) :: yhat(:), y(:)

rmse = sqrt(sum((yhat - y)**2)/real(size(y), dp))
end function rmse

pure real(kind=dp) function cubic_pos(x)
real(kind=dp), intent(in) :: x

if (x > 0.0_dp) then
   cubic_pos = x**3
else
   cubic_pos = 0.0_dp
end if
end function cubic_pos

subroutine solve_linear(a_in, b_in, x, info)
real(kind=dp), intent(in) :: a_in(:,:), b_in(:)
real(kind=dp), intent(out) :: x(:)
integer, intent(out) :: info

integer :: n, i, k, piv
real(kind=dp) :: a(size(a_in,1), size(a_in,2))
real(kind=dp) :: b(size(b_in))
real(kind=dp) :: factor, tmp
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

subroutine solve_least_squares(a, y, coef, lambda_diag)
real(kind=dp), intent(in) :: a(:,:), y(:)
real(kind=dp), intent(out) :: coef(:)
real(kind=dp), intent(in), optional :: lambda_diag(:)

integer :: m, i, info
real(kind=dp) :: ata(size(coef), size(coef))
real(kind=dp) :: aty(size(coef))

m = size(coef)
if (size(a,1) /= size(y) .or. size(a,2) /= m) then
   error stop 'solve_least_squares: dimension mismatch'
end if

ata = matmul(transpose(a), a)
aty = matmul(transpose(a), y)

if (present(lambda_diag)) then
   if (size(lambda_diag) /= m) then
      error stop 'solve_least_squares: bad lambda_diag size'
   end if
   do i = 1, m
      ata(i,i) = ata(i,i) + lambda_diag(i)
   end do
end if

call solve_linear(ata, aty, coef, info)
if (info /= 0) then
   error stop 'solve_least_squares: singular normal equations'
end if
end subroutine solve_least_squares

end module util_mod

module true_func_mod
use kind_mod, only: dp
implicit none
contains

real(kind=dp) function true_func_scalar(x, true_name, k, lam, c_lin, a_log, b_log)
real(kind=dp), intent(in) :: x, k, lam, c_lin, a_log, b_log
character(len=*), intent(in) :: true_name
real(kind=dp) :: u

select case (trim(true_name))
case ('sin')
   true_func_scalar = sin(k*x)
case ('cos')
   true_func_scalar = cos(k*x)
case ('sinc')
   u = k*x
   if (abs(u) < 1.0e-12_dp) then
      true_func_scalar = 1.0_dp
   else
      true_func_scalar = sin(u)/u
   end if
case ('exp_decay_sin')
   true_func_scalar = exp(-lam*abs(x))*sin(k*x)
case ('linear_plus_sin')
   true_func_scalar = c_lin*x + sin(k*x)
case ('logistic')
   true_func_scalar = 1.0_dp/(1.0_dp + exp(-a_log*(x - b_log)))
case default
   error stop 'true_func_scalar: unknown true_name'
end select
end function true_func_scalar

subroutine fill_true_values(x, y, true_name, k, lam, c_lin, a_log, b_log)
real(kind=dp), intent(in) :: x(:), k, lam, c_lin, a_log, b_log
real(kind=dp), intent(out) :: y(size(x))
character(len=*), intent(in) :: true_name
integer :: i

do i = 1, size(x)
   y(i) = true_func_scalar(x(i), true_name, k, lam, c_lin, a_log, b_log)
end do
end subroutine fill_true_values

end module true_func_mod

module fit_mod
use kind_mod, only: dp
use util_mod, only: cubic_pos, solve_least_squares, solve_linear
implicit none
contains

subroutine scale_x(x, z)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: z(size(x))
real(kind=dp) :: xmin, xmax

xmin = minval(x)
xmax = maxval(x)
if (xmax <= xmin) then
   error stop 'scale_x: xmax <= xmin'
end if
z = 2.0_dp*(x - xmin)/(xmax - xmin) - 1.0_dp
end subroutine scale_x

subroutine fit_cubic_poly(z, y, coef)
real(kind=dp), intent(in) :: z(:), y(:)
real(kind=dp), intent(out) :: coef(4)
real(kind=dp) :: a(size(z), 4)

a(:,1) = 1.0_dp
a(:,2) = z
a(:,3) = z**2
a(:,4) = z**3

call solve_least_squares(a, y, coef)
end subroutine fit_cubic_poly

subroutine eval_cubic_poly(z, coef, yhat)
real(kind=dp), intent(in) :: z(:), coef(4)
real(kind=dp), intent(out) :: yhat(size(z))

yhat = coef(1) + coef(2)*z + coef(3)*z**2 + coef(4)*z**3
end subroutine eval_cubic_poly

subroutine rational32(z, theta, yhat, den)
real(kind=dp), intent(in) :: z(:), theta(6)
real(kind=dp), intent(out) :: yhat(size(z)), den(size(z))

real(kind=dp) :: num(size(z)), den_safe(size(z))

num = theta(1) + theta(2)*z + theta(3)*z**2 + theta(4)*z**3
den = 1.0_dp + theta(5)*z + theta(6)*z**2
den_safe = sign(max(abs(den), 1.0e-12_dp), den)

yhat = num/den_safe
end subroutine rational32

subroutine rational_residuals(z, y, theta, den_floor, penalty_weight, r)
real(kind=dp), intent(in) :: z(:), y(:), theta(6), den_floor, penalty_weight
real(kind=dp), intent(out) :: r(2*size(z))

integer :: n, i
real(kind=dp) :: yhat(size(z)), den(size(z))

n = size(z)
call rational32(z, theta, yhat, den)

r(1:n) = y - yhat
do i = 1, n
   r(n+i) = penalty_weight*max(0.0_dp, den_floor - abs(den(i)))
end do
end subroutine rational_residuals

subroutine rational_jacobian(z, y, theta, den_floor, penalty_weight, r, jac)
real(kind=dp), intent(in) :: z(:), y(:), theta(6), den_floor, penalty_weight
real(kind=dp), intent(in) :: r(2*size(z))
real(kind=dp), intent(out) :: jac(2*size(z), 6)

integer :: j
real(kind=dp) :: h
real(kind=dp) :: theta1(6), r1(2*size(z))

do j = 1, 6
   theta1 = theta
   h = 1.0e-6_dp*(1.0_dp + abs(theta(j)))
   theta1(j) = theta1(j) + h
   call rational_residuals(z, y, theta1, den_floor, penalty_weight, r1)
   jac(:,j) = (r1 - r)/h
end do
end subroutine rational_jacobian

subroutine fit_rational32(z, y, theta0, den_floor, penalty_weight, theta, obj, iter_used)
real(kind=dp), intent(in) :: z(:), y(:), theta0(6), den_floor, penalty_weight
real(kind=dp), intent(out) :: theta(6), obj
integer, intent(out) :: iter_used

integer, parameter :: max_iter = 200
integer :: iter, j, info
real(kind=dp), parameter :: tol = 1.0e-10_dp
real(kind=dp) :: lambda, obj_new, step_norm
real(kind=dp) :: r(2*size(z)), r_new(2*size(z))
real(kind=dp) :: jac(2*size(z), 6)
real(kind=dp) :: a(6,6), g(6), rhs(6), delta(6), theta_new(6)

theta = theta0
lambda = 1.0e-3_dp

call rational_residuals(z, y, theta, den_floor, penalty_weight, r)
obj = dot_product(r, r)
iter_used = 0

do iter = 1, max_iter
   iter_used = iter

   call rational_jacobian(z, y, theta, den_floor, penalty_weight, r, jac)

   a = matmul(transpose(jac), jac)
   g = matmul(transpose(jac), r)
   do j = 1, 6
      a(j,j) = a(j,j) + lambda
   end do

   rhs = -g
   call solve_linear(a, rhs, delta, info)
   if (info /= 0) then
      lambda = min(lambda*10.0_dp, 1.0e12_dp)
      cycle
   end if

   step_norm = sqrt(sum(delta*delta))
   if (step_norm < tol*(1.0_dp + sqrt(sum(theta*theta)))) exit

   theta_new = theta + delta
   call rational_residuals(z, y, theta_new, den_floor, penalty_weight, r_new)
   obj_new = dot_product(r_new, r_new)

   if (obj_new < obj) then
      if (abs(obj - obj_new) < tol*(1.0_dp + obj)) then
         theta = theta_new
         obj = obj_new
         exit
      end if
      theta = theta_new
      r = r_new
      obj = obj_new
      lambda = max(lambda*0.3_dp, 1.0e-12_dp)
   else
      lambda = min(lambda*10.0_dp, 1.0e12_dp)
   end if
end do
end subroutine fit_rational32

subroutine make_knots(nknots, knots)
integer, intent(in) :: nknots
real(kind=dp), intent(out) :: knots(nknots)
integer :: j

do j = 1, nknots
   knots(j) = -1.0_dp + 2.0_dp*real(j, dp)/real(nknots + 1, dp)
end do
end subroutine make_knots

subroutine spline_basis(z, knots, b)
real(kind=dp), intent(in) :: z(:), knots(:)
real(kind=dp), intent(out) :: b(size(z), 4 + size(knots))
integer :: i, j, n, nknots

n = size(z)
nknots = size(knots)

b(:,1) = 1.0_dp
b(:,2) = z
b(:,3) = z**2
b(:,4) = z**3

do j = 1, nknots
   do i = 1, n
      b(i,4+j) = cubic_pos(z(i) - knots(j))
   end do
end do
end subroutine spline_basis

subroutine fit_penalized_cubic_spline(z, y, nknots, lambda, knots, coef)
real(kind=dp), intent(in) :: z(:), y(:), lambda
integer, intent(in) :: nknots
real(kind=dp), intent(out) :: knots(nknots), coef(4 + nknots)

real(kind=dp) :: b(size(z), 4 + nknots)
real(kind=dp) :: lambda_diag(4 + nknots)

call make_knots(nknots, knots)
call spline_basis(z, knots, b)

lambda_diag = 0.0_dp
if (nknots > 0) lambda_diag(5:4+nknots) = lambda

call solve_least_squares(b, y, coef, lambda_diag)
end subroutine fit_penalized_cubic_spline

subroutine eval_penalized_cubic_spline(z, knots, coef, yhat)
real(kind=dp), intent(in) :: z(:), knots(:), coef(4 + size(knots))
real(kind=dp), intent(out) :: yhat(size(z))

real(kind=dp) :: b(size(z), 4 + size(knots))

call spline_basis(z, knots, b)
yhat = matmul(b, coef)
end subroutine eval_penalized_cubic_spline

end module fit_mod

program main
use kind_mod, only: dp
use util_mod, only: set_seed, normal_rand, rmse
use true_func_mod, only: fill_true_values
use fit_mod, only: scale_x, fit_cubic_poly, eval_cubic_poly, fit_rational32, &
                   rational32, fit_penalized_cubic_spline, eval_penalized_cubic_spline
implicit none

integer, parameter :: n = 300
integer, parameter :: seed = 123
integer, parameter :: nknots = 10

real(kind=dp), parameter :: x_min = -4.0_dp
real(kind=dp), parameter :: x_max = 4.0_dp
real(kind=dp), parameter :: noise_sd = 0.15_dp

character(len=*), parameter :: true_name = 'sin'
real(kind=dp), parameter :: k = 2.0_dp
real(kind=dp), parameter :: lam_true = 0.4_dp
real(kind=dp), parameter :: c_lin = 0.25_dp
real(kind=dp), parameter :: a_log = 1.0_dp
real(kind=dp), parameter :: b_log = 0.0_dp

real(kind=dp), parameter :: den_floor = 0.15_dp
real(kind=dp), parameter :: penalty_weight = 10.0_dp

real(kind=dp), parameter :: spline_lambda = 1.0e-3_dp

integer :: i, iter_rat
real(kind=dp) :: obj_rat, min_abs_den

real(kind=dp) :: x(n), z(n), y_true(n), y(n)
real(kind=dp) :: y_rat(n), y_poly(n), y_spline(n), den(n)
real(kind=dp) :: theta0(6), theta_rat(6), coef_poly(4)
real(kind=dp) :: knots(nknots), coef_spline(4 + nknots)

call set_seed(seed)

do i = 1, n
   x(i) = x_min + (x_max - x_min)*real(i - 1, dp)/real(n - 1, dp)
end do

call fill_true_values(x, y_true, true_name, k, lam_true, c_lin, a_log, b_log)

do i = 1, n
   y(i) = y_true(i) + noise_sd*normal_rand()
end do

call scale_x(x, z)

theta0 = [0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]

call fit_rational32(z, y, theta0, den_floor, penalty_weight, theta_rat, obj_rat, iter_rat)
call rational32(z, theta_rat, y_rat, den)

call fit_cubic_poly(z, y, coef_poly)
call eval_cubic_poly(z, coef_poly, y_poly)

call fit_penalized_cubic_spline(z, y, nknots, spline_lambda, knots, coef_spline)
call eval_penalized_cubic_spline(z, knots, coef_spline, y_spline)

min_abs_den = minval(abs(den))

print *
print *, 'true function name: ', trim(true_name)
print *, 'k        = ', k
print *, 'lam_true = ', lam_true
print *, 'c_lin    = ', c_lin
print *, 'a_log    = ', a_log
print *, 'b_log    = ', b_log
print *, 'noise_sd = ', noise_sd
print *

print *, 'rational (3,2) fit:'
print *, 'iterations                 = ', iter_rat
print *, 'objective                  = ', obj_rat
print *, 'theta [a0,a1,a2,a3,b1,b2] = '
print *, theta_rat
print *, 'rmse vs true               = ', rmse(y_rat, y_true)
print *, 'rmse vs noisy data         = ', rmse(y_rat, y)
print *, 'min(abs(denominator))      = ', min_abs_den
print *

print *, 'cubic polynomial fit:'
print *, 'coef [c0,c1,c2,c3] = '
print *, coef_poly
print *, 'rmse vs true       = ', rmse(y_poly, y_true)
print *, 'rmse vs noisy data = ', rmse(y_poly, y)
print *

print *, 'penalized cubic spline fit:'
print *, 'nknots             = ', nknots
print *, 'spline_lambda      = ', spline_lambda
print *, 'rmse vs true       = ', rmse(y_spline, y_true)
print *, 'rmse vs noisy data = ', rmse(y_spline, y)
print *

print *, 'first five rows: x, y_true, y, y_rat, y_poly, y_spline'
do i = 1, min(5, n)
   print *, x(i), y_true(i), y(i), y_rat(i), y_poly(i), y_spline(i)
end do

end program main

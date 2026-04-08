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

subroutine rational32(z, theta, yhat, den)
real(kind=dp), intent(in) :: z(:)
real(kind=dp), intent(in) :: theta(6)
real(kind=dp), intent(out) :: yhat(size(z))
real(kind=dp), intent(out) :: den(size(z))
real(kind=dp) :: num(size(z))
real(kind=dp) :: den_safe(size(z))

num = theta(1) + theta(2)*z + theta(3)*z**2 + theta(4)*z**3
den = 1.0_dp + theta(5)*z + theta(6)*z**2
den_safe = sign(max(abs(den), 1.0e-12_dp), den)
 yhat = num/den_safe
end subroutine rational32

subroutine rational_residuals(z, y, theta, den_floor, penalty_weight, r)
real(kind=dp), intent(in) :: z(:)
real(kind=dp), intent(in) :: y(:)
real(kind=dp), intent(in) :: theta(6)
real(kind=dp), intent(in) :: den_floor
real(kind=dp), intent(in) :: penalty_weight
real(kind=dp), intent(out) :: r(2*size(z))

integer :: i
integer :: n
real(kind=dp) :: den(size(z))
real(kind=dp) :: yhat(size(z))

n = size(z)
call rational32(z, theta, yhat, den)
 r(1:n) = y - yhat
do i = 1, n
   r(n+i) = penalty_weight*max(0.0_dp, den_floor - abs(den(i)))
end do
end subroutine rational_residuals

subroutine rational_jacobian(z, y, theta, den_floor, penalty_weight, r, jac)
real(kind=dp), intent(in) :: z(:)
real(kind=dp), intent(in) :: y(:)
real(kind=dp), intent(in) :: theta(6)
real(kind=dp), intent(in) :: den_floor
real(kind=dp), intent(in) :: penalty_weight
real(kind=dp), intent(in) :: r(2*size(z))
real(kind=dp), intent(out) :: jac(2*size(z), 6)

integer :: j
real(kind=dp) :: h
real(kind=dp) :: r1(2*size(z))
real(kind=dp) :: theta1(6)

do j = 1, 6
   theta1 = theta
   h = 1.0e-6_dp*(1.0_dp + abs(theta(j)))
   theta1(j) = theta1(j) + h
   call rational_residuals(z, y, theta1, den_floor, penalty_weight, r1)
   jac(:,j) = (r1 - r)/h
end do
end subroutine rational_jacobian

end module fit_mod

program main
use kind_mod, only: dp
use util_mod, only: set_seed, normal_rand
use true_func_mod, only: fill_true_values
use fit_mod, only: scale_x, rational_residuals, rational_jacobian
implicit none

integer, parameter :: n = 300
integer, parameter :: seed = 123
real(kind=dp), parameter :: x_min = -4.0_dp
real(kind=dp), parameter :: x_max = 4.0_dp
real(kind=dp), parameter :: noise_sd = 0.15_dp
real(kind=dp), parameter :: k = 2.0_dp
real(kind=dp), parameter :: den_floor = 0.15_dp
real(kind=dp), parameter :: penalty_weight = 10.0_dp

integer :: i
real(kind=dp) :: a(6,6)
real(kind=dp) :: g(6)
real(kind=dp) :: jac(2*n, 6)
real(kind=dp) :: obj
real(kind=dp) :: r(2*n)
real(kind=dp) :: theta(6)
real(kind=dp) :: x(n)
real(kind=dp) :: y(n)
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
theta = [0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]

call rational_residuals(z, y, theta, den_floor, penalty_weight, r)
call rational_jacobian(z, y, theta, den_floor, penalty_weight, r, jac)
obj = dot_product(r, r)
g = matmul(transpose(jac), r)
a = matmul(transpose(jac), jac)

print *, "rational residual and Jacobian check with local MT RNG"
print *, "objective =", obj
print *, "g:"
print *, g
print *, "diag(a):"
print *, a(1,1), a(2,2), a(3,3), a(4,4), a(5,5), a(6,6)
print *, "first row of a:"
print *, a(1,1), a(1,2), a(1,3), a(1,4), a(1,5), a(1,6)
print *, "first row of jac:"
print *, jac(1,1), jac(1,2), jac(1,3), jac(1,4), jac(1,5), jac(1,6)
print *, "first five residuals:"
print *, r(1), r(2), r(3), r(4), r(5)

end program main

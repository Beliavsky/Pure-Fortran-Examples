program main
use iso_fortran_env, only : int64
use mt19937_runif, only : rng_seed, runif_1d
implicit none
integer, parameter :: dp = kind(1.0d0), n = 10**7
real(dp) :: x(n), xmean, xsd
call rng_seed(42_int64)
x = runif_1d(n)
xmean = sum(x) / n
xsd = sqrt(sum((x - xmean)**2) / (n - 1))
print *, xmean, xsd, xsd**2
end program main

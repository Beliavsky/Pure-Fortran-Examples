program main
use iso_fortran_env, only : int64
use mt19937_runif, only : rng_seed, runif, runif_1d
implicit none
integer, parameter :: dp = kind(1.0d0)
call rng_seed(42_int64)
print*,[runif(), runif_1d(3), runif()]
end program main

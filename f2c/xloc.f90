program main
use iso_fortran_env, only : int64
use mt19937_runif, only : rng_seed, runif_1d
implicit none
integer, parameter :: dp = kind(1.0d0), n1 = 3, n2 = 5
real(dp) :: x(n1, n2), xmean, xsd
integer :: i, imat(n1, n2)
call rng_seed(42_int64)
do i=1,n2
   x(:, i) = runif_1d(n1)
end do
imat = nint(5*x)
do i=1,n1
   print "(*(i4))", imat(i,:)
end do
print*,"minloc(imat):", minloc(imat)
print*,"maxloc(imat):", maxloc(imat)
print*,"minloc(imat, dim=1):", minloc(imat, dim=1)
print*,"minloc(imat, dim=2):", minloc(imat, dim=2)
print*,"maxloc(imat, dim=1):", maxloc(imat, dim=1)
print*,"maxloc(imat, dim=2):", maxloc(imat, dim=2)
print*,"findloc(imat, 3):", findloc(imat, 3)
print*,"findloc(imat, -1):", findloc(imat, -1)
print*,"findloc(imat, 3, dim=1):", findloc(imat, 3, dim=1)
print*,"findloc(imat, 3, dim=2):", findloc(imat, 3, dim=2)
print*,"minloc(imat, back=.true.):", minloc(imat, back=.true.)
print*,"maxloc(imat, back=.true.):", maxloc(imat, back=.true.)
print*,"minloc(imat, dim=1, back=.true.):", minloc(imat, dim=1, back=.true.)
print*,"minloc(imat, dim=2, back=.true.):", minloc(imat, dim=2, back=.true.)
print*,"maxloc(imat, dim=1, back=.true.):", maxloc(imat, dim=1, back=.true.)
print*,"maxloc(imat, dim=2, back=.true.):", maxloc(imat, dim=2, back=.true.)
print*,"findloc(imat, 3, back=.true.):", findloc(imat, 3, back=.true.)
print*,"findloc(imat, -1, back=.true.):", findloc(imat, -1, back=.true.)
print*,"findloc(imat, 3, dim=1, back=.true.):", findloc(imat, 3, dim=1, back=.true.)
print*,"findloc(imat, 3, dim=2, back=.true.):", findloc(imat, 3, dim=2, back=.true.)
end program main

module price_stats_mod
implicit none

integer, parameter :: dp = kind(1.0d0)

contains

function mean(x) result(mu)
! return the mean of a real vector
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: mu
mu = sum(x) / real(size(x), kind=dp)
end function mean

function sd(x) result(sig)
! return the sample standard deviation of a real vector
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: sig
real(kind=dp) :: mu
if (size(x) <= 1) then
   sig = 0.0_dp
else
   mu = mean(x)
   sig = sqrt(sum((x - mu)**2) / real(size(x) - 1, kind=dp))
end if
end function sd

end module price_stats_mod

program xstats
use price_stats_mod, only: dp, mean, sd
implicit none
integer, parameter :: n = 10**6
real(kind=dp) :: x(n)
call random_number(x)
print*,mean(x), sd(x)
end program xstats

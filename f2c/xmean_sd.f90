module m
implicit none
integer, parameter :: dp = kind(1.0d0)
contains
subroutine mean_sd(x, mean, sd, calc_mean)
! calculate the standard deviation of data, and the mean if not provided
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in out) :: mean
real(kind=dp), intent(out) :: sd
logical, intent(in), optional :: calc_mean
logical :: calc_mean_
integer :: n
n = size(x)
if (present(calc_mean)) then
   calc_mean_ = calc_mean
else
   calc_mean_ = .true.
end if
if (calc_mean_) then
   print*,"calculating mean"
   mean = sum(x)/max(1,n)
end if
if (n > 1) then
   sd = sqrt(sum((x-mean)**2)/(n-1))
else
   sd = -1.0_dp
end if
end subroutine mean_sd
end module m

program main
use m
implicit none
integer, parameter :: n = 10**6
real(kind=dp) :: x(n), mean, sd
character (len=*), parameter :: fmt_o = "(/,a)", fmt_stats ="('mean, sd = ',2f10.6)"
print*,"#obs:", n
call random_number(x)
print fmt_o, "call mean_sd(x, mean, sd, calc_mean = .true.)"
call mean_sd(x, mean, sd, calc_mean = .true.)
print fmt_stats,mean,sd
print fmt_o,"call mean_sd(x, mean, sd, calc_mean = .false.)"
call mean_sd(x, mean, sd, calc_mean = .false.)
print fmt_stats,mean,sd
print fmt_o,"call mean_sd(x, mean, sd)"
call mean_sd(x, mean, sd)
print fmt_stats,mean,sd
end program main

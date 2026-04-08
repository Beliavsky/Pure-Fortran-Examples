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

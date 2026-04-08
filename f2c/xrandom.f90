program main
implicit none
integer, parameter :: dp = kind(1.0d0), n = 10**7
real(kind=dp) :: x(n), xmean, xsd
call random_number(x)
xmean = sum(x)/n
xsd = sqrt(sum((x-xmean)**2)/(n-1))
print*,xmean, xsd, xsd**2
end program main
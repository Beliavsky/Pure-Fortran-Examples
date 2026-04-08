program xdate_and_time
! demonstrates the DATE_AND_TIME intrinsic subroutine
implicit none

character(len=8)  :: date
character(len=10) :: time
character(len=5)  :: zone
integer :: values(8)

call date_and_time(date, time, zone, values)

print '(a)', 'raw results from date_and_time:'
print '(a,a)', 'date = ', date
print '(a,a)', 'time = ', time
print '(a,a)', 'zone = ', zone
print *

print '(a)', 'decoded values array:'
print '(a,i0)', 'year         = ', values(1)
print '(a,i0)', 'month        = ', values(2)
print '(a,i0)', 'day          = ', values(3)
print '(a,i0)', 'utc offset   = ', values(4)
print '(a,i0)', 'hour         = ', values(5)
print '(a,i0)', 'minute       = ', values(6)
print '(a,i0)', 'second       = ', values(7)
print '(a,i0)', 'millisecond  = ', values(8)
print *

print '(a,i4.4,a,i2.2,a,i2.2)', 'formatted date: ', &
    values(1), '-', values(2), '-', values(3)

print '(a,i2.2,a,i2.2,a,i2.2,a,i3.3)', 'formatted time: ', &
    values(5), ':', values(6), ':', values(7), '.', values(8)

end program xdate_and_time

program demo_system_clock
use iso_fortran_env, only: int64, real64
implicit none

integer(int64) :: count0, count1, count2
integer(int64) :: rate, count_max
real(real64)   :: elapsed1, elapsed2
integer        :: i
real(real64)   :: x

call system_clock(count_rate=rate, count_max=count_max)
print *, "count_rate =", rate
print *, "count_max  =", count_max

call system_clock(count=count0)
print *, "initial count =", count0

! time a simple loop
x = 0.0_real64
call system_clock(count=count1)
do i = 1, 10000000
   x = x + sin(real(i, kind=real64))
end do
call system_clock(count=count2)

if (count2 >= count1) then
   elapsed1 = real(count2 - count1, kind=real64) / real(rate, kind=real64)
else
   ! handle wraparound
   elapsed1 = real(count_max - count1 + count2 + 1_int64, kind=real64) / &
      real(rate, kind=real64)
end if

print *, "result from loop =", x
print *, "count before loop =", count1
print *, "count after loop  =", count2
print *, "elapsed seconds   =", elapsed1

! show repeated calls
print *
print *, "successive clock readings:"
do i = 1, 5
   call system_clock(count=count0)
   print *, i, count0
end do

! time a second operation
call system_clock(count=count1)
call waste_time()
call system_clock(count=count2)

if (count2 >= count1) then
   elapsed2 = real(count2 - count1, kind=real64) / real(rate, kind=real64)
else
   elapsed2 = real(count_max - count1 + count2 + 1_int64, kind=real64) / &
      real(rate, kind=real64)
end if

print *
print *, "elapsed time in waste_time() =", elapsed2

end program demo_system_clock

subroutine waste_time()
use iso_fortran_env, only: real64
implicit none
integer      :: i
real(real64) :: y

y = 1.0_real64
do i = 1, 5000000
   y = sqrt(y + 0.000001_real64)
end do
print *, "dummy value =", y
end subroutine waste_time

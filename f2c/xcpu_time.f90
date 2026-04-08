program demo_cpu_time
implicit none

real :: t0, t1, elapsed
real :: x
integer :: i

call cpu_time(t0)

x = 0.0
do i = 1, 10000000
   x = x + sin(real(i))
end do

call cpu_time(t1)

elapsed = t1 - t0

print *, "result  =", x
print *, "t0      =", t0
print *, "t1      =", t1
print *, "cpu sec =", elapsed

call show_cpu_time()

end program demo_cpu_time

subroutine show_cpu_time()
implicit none

real :: t0, t1
integer :: i
real :: y

call cpu_time(t0)

y = 1.0
do i = 1, 5000000
   y = sqrt(y + 0.000001)
end do

call cpu_time(t1)

print *
print *, "inside show_cpu_time"
print *, "dummy   =", y
print *, "cpu sec =", t1 - t0

end subroutine show_cpu_time

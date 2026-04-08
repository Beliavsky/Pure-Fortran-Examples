program test_common
implicit none
integer :: n
real :: x, y, avg
common /stats/ x, y, avg, n

x = 10.0
y = 20.0
n = 2

call compute_avg
call print_avg

end program test_common

subroutine compute_avg
implicit none
integer :: n
real :: x, y, avg
common /stats/ x, y, avg, n

avg = (x + y) / n

end subroutine compute_avg

subroutine print_avg
implicit none
integer :: n
real :: x, y, avg
common /stats/ x, y, avg, n

print *, "x   = ", x
print *, "y   = ", y
print *, "n   = ", n
print *, "avg = ", avg

end subroutine print_avg

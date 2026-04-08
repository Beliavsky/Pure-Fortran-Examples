module common_stats_mod
   implicit none
   real :: x
   real :: y
   real :: avg
   integer :: n
end module common_stats_mod

program test_common
   use common_stats_mod, only: x, y, avg, n
implicit none

x = 10.0
y = 20.0
n = 2

call compute_avg
call print_avg

end program test_common

subroutine compute_avg
   use common_stats_mod, only: x, y, avg, n
implicit none

avg = (x + y) / n

end subroutine compute_avg

subroutine print_avg
   use common_stats_mod, only: x, y, avg, n
implicit none

print *, "x   = ", x
print *, "y   = ", y
print *, "n   = ", n
print *, "avg = ", avg

end subroutine print_avg

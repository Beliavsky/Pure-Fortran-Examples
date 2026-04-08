program main
implicit none
integer :: v(3) = [4, 9, 16], w(3) = [100, 200, 300]
print*,merge(10, 20, v > 5)
print*,merge(10, w, v > 5)
print*,merge(10, w, .true.)
print*,merge(10, w, .false.)
print*,merge(w, 10*w, v > 5)
end program main
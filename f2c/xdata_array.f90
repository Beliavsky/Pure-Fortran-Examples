program data_array
implicit none

integer :: i
integer :: a(5)

data a /10, 20, 30, 40, 50/

print *, "array a:"
do i = 1, size(a)
   print *, "a(", i, ") = ", a(i)
end do

end program data_array

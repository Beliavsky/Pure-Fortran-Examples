program data_matrix
implicit none

integer :: i
integer :: j
integer :: a(2,3)

data a /1, 2, 3, 4, 5, 6/

print *, "matrix a(2,3):"
do i = 1, 2
   print "(3(i4))", (a(i,j), j = 1, 3)
end do

print *
print *, "note: data fills arrays in column-major order"
print *, "so the columns are:"
print *, "column 1:", a(:,1)
print *, "column 2:", a(:,2)
print *, "column 3:", a(:,3)

end program data_matrix

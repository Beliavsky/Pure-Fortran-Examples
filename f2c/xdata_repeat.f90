program data_implied_do
implicit none

integer :: i
integer :: b(10)

data (b(i), i = 1, 10) /3*1, 4*2, 3*9/

print *, "array b:"
print *, b

end program data_implied_do

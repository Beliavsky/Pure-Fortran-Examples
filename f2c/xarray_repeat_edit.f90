implicit none
integer, parameter :: n = 3
real, dimension(n) :: x = [10.0, 20.0, 30.0]
integer :: i
x = 10*x
do i=1,n
   print*,i,x(i)
end do
print "(f7.2)",x
print "(2f7.2)",x
end

program main
implicit none
integer :: i, j
integer, parameter :: n1 = 3, n2 = 4
real :: x(3, 4)
forall (i=1:n1, j=1:n2) x(i,j) = real(i - j)
do i=1,n1
   print*,x(i,:)
end do
forall (i=1:n1, j=1:n2)
   x(i,j) = real(i + j)
end forall
do i=1,n1
   print*,x(i,:)
end do
end program main
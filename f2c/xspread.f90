program demo_spread
   implicit none
   integer :: v(3)
   integer :: m23(2,3)
   integer :: m32(3,2)
   integer :: a(2,3,3)
   integer :: temp23(2,3)
   integer :: i, k

   v = [1, 2, 3]

   print *, "example 1: spread a rank-1 array into rows"
   print *, "v = ", v

   m23 = spread(v, dim=1, ncopies=2)
   do i = 1, size(m23,1)
      print *, m23(i,:)
   end do
   print *

   print *, "example 2: spread a rank-1 array into columns"
   m32 = spread(v, dim=2, ncopies=2)
   do i = 1, size(m32,1)
      print *, m32(i,:)
   end do

end program demo_spread

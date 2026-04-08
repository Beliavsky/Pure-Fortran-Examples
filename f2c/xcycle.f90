program main
implicit none
integer :: i, j
do j=1,3
   iloop: do i=1,6
      if (mod(i,2) == 0) cycle iloop
      print*,i*j,(i*j)**2
   end do iloop
end do
end program main

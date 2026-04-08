program main
implicit none
integer :: i, j
logical :: is_prime
integer, parameter :: n = 10**2

do i = 2, n
   is_prime = .true.
   do j = 2, int(sqrt(real(i)))
      if (mod(i,j) == 0) then
         is_prime = .false.
         exit
      end if
   end do
   if (is_prime) then
      print*, i
   end if
end do

end program main
program main
implicit none
integer, parameter :: n = 10
integer :: i, j
real :: x
character (len=10) :: label
do i=1,n
   call random_number(x)
   j = nint(4*x)
   select case (j)
      case (0)  ; label = "small"
      case (1:3); label = "medium"
      case (4)  ; label = "big"
   end select
   print "(f8.4, i4, 1x, a)", x, j, trim(label)
end do
end program main
program main
implicit none
character (len=2) :: x(3)
integer :: i
x(1) = "a"
x(2) = " b"
x(3) = "cc"
do i=1,3
   print*,len_trim(x(i))
   x(i) = adjustl(x(i)) // "foo"
end do
print*,x
end program main
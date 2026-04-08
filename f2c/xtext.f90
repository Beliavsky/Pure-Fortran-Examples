program main
implicit none
character (len=3) :: s = "def", t(2) = ["ghi", "jkl"]
integer :: i
do i=1,len(s)
   print*,s(i:i), t(1)(i:i), t(2)(i:i)
end do
print*,s(2:3)
end program main
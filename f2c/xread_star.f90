program main
implicit none
integer :: iu, i
open (newunit=iu, file="nums.txt", action="read", status="old")
read (iu,*)
read (iu,*) i
print*,"i =",i
read (iu,*)
read (iu,*) i
print*,"i =",i
end program main

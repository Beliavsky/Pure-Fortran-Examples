program main
implicit none
integer :: ios, iu = 20
character (len=*), parameter :: xfile = "data.txt"
character (len=100) :: text = "abc"
open (unit=iu, file=xfile, action="write", status="replace")
write (iu,"(a)") text
close (iu)
open (unit=iu, file=xfile, action="read", status="old")
read (iu, "(a)", iostat = ios) text
if (ios == 0) then
   print*,trim(text)
else
   print*,"ios =",ios
end if
end program main

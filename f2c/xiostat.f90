program main
implicit none
integer :: i, ierr
character (len=10) :: text
text = "345"
read (text,*,iostat=ierr) i
if (ierr /= 0) then
   print*,"ierr =",ierr
else
   print*,"i =",i
end if
end program main

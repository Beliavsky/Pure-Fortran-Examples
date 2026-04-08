program xpause
! demonstrates the deleted pause statement
implicit none
integer :: i

print *, 'start of program'

do i = 1, 3
   print *, 'before pause, i =', i
   pause 'paused: resume execution'
   print *, 'after pause,  i =', i
   print *
end do

print *, 'end of program'

end program xpause

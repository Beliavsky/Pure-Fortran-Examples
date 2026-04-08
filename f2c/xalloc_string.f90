program main
implicit none
character (len=:), allocatable :: s
s = "abc"
print*,len(s)
s = "defgh"
print*,len(s)
print*,allocated(s)
deallocate(s)
print*,allocated(s)
end program main
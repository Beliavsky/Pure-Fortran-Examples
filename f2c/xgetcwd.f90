program main
implicit none
character (len=1000) :: cwd
call getcwd(cwd)
print*,"cwd = '" // trim(cwd) // "'"
end program main
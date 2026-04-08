subroutine sub()
implicit none
integer, save :: ncalls = 0
integer :: n = 0
n = n + 1
ncalls = ncalls + 1
print*,"in sub, n, ncalls =", n, ncalls
end subroutine sub

subroutine sub1()
implicit none
integer :: ncalls = 0
integer :: n = 0
n = n + 1
ncalls = ncalls + 1
print*,"in sub1, n, ncalls =", n, ncalls
end subroutine sub1

program main
implicit none
integer :: i
do i=1,3
   call sub()
   call sub1()
end do
end program main

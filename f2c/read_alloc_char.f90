module m
implicit none
contains
subroutine read_alloc_char(iu,xx,nx,print_vec,label,same_line)
! read the size of an allocatable array and then the array from the next line
integer          , intent(in)               :: iu    ! unit read
character (len=*), intent(out), allocatable :: xx(:) ! data read
integer          , intent(out), optional    :: nx    ! size(xx)
logical          , intent(in) , optional    :: print_vec
character (len=*), intent(in) , optional    :: label
logical          , intent(in) , optional    :: same_line
integer                                     :: i,nx_,ndum
read (iu,*) nx_
nx_ = max(0,nx_)
allocate (xx(nx_))
read (iu,*) xx
if (present(nx)) nx = nx_
write (*,"(a,100(1x,a))") "data:", (trim(xx(i)),i=1,size(xx))
end subroutine read_alloc_char
end module m

program main
use m
implicit none

integer :: iu,nx,i
character(len=5), allocatable :: xx(:)
character(len=5) :: expected(3)
logical :: ok

expected = [character(len=5) :: "red", "green", "blue"]

iu = 10
open (unit=iu,status="scratch",action="readwrite")

write (iu,*) size(expected)
write (iu,"(*(1x,a))") (trim(expected(i)), i=1,size(expected))
! write (iu,*) expected
rewind (iu)

call read_alloc_char(iu,xx,nx)

ok = allocated(xx)
ok = ok .and. nx == size(expected)
ok = ok .and. size(xx) == size(expected)
ok = ok .and. all(xx == expected)

print *
print *, "nx        =", nx
print *, "size(xx)  =", size(xx)
! print *, "expected  =", expected
write (*,"('expected:',*(1x,a))") (trim(expected(i)), i=1,size(expected))
write (*,"('read back:',*(1x,a))") (trim(xx(i)), i=1,size(xx))
! print *, "read back =", xx

if (ok) then
   print *, "test passed"
else
   print *, "test failed"
   error stop
end if
close (iu)
end program main

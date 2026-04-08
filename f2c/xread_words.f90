module m
implicit none
contains
subroutine read_words_line(iu,words,nread,label)
! read words from line, where the line has the # of words followed by the words
! n word_1 word_2 ... word_n
integer          , intent(in)               :: iu
character (len=*), intent(out), allocatable :: words(:)
integer          , intent(out), optional    :: nread
character (len=*), intent(in) , optional    :: label
character (len=10000)                       :: text
integer                                     :: ierr,nread_,nw
read (iu,"(a)") text
read (text,*,iostat=ierr) nw
if (ierr /= 0) then
   write (*,*) "in read_words_line, could not read integer from '" // trim(text) // "', STOPPING"
   stop
end if
if (nw > 0) then
   allocate (words(nw))
   read (text,*,iostat=ierr) nw,words
   if (ierr /= 0) then
      write (*,*) "in read_words_line, could not read",nw," words from '" // trim(text) // "', STOPPING"
      stop
   end if
else
   allocate (words(0))
end if
nread_ = size(words)
if (present(nread)) nread = nread_
if (present(label)) write (*,*) trim(label) // ":"
end subroutine read_words_line
end module m

program main
use m
implicit none
integer, parameter :: iu = 20
character (len=*), parameter :: xfile = "words.txt"
character (len=10), allocatable :: words(:)
integer :: nread
print*,"hi"
open (unit=iu,file=xfile,action="write",status="replace")
write (iu,"(i0,*(1x,a))") 3, "dog", "cat", "bird"
close (iu)
open (unit=iu,file=xfile,action="read",status="old")
call read_words_line(iu,words,nread,label="words from " // xfile)
print "(*(a10))", words
print*,"bye"
end program main

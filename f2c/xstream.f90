program demo_stream_io
implicit none

integer, parameter :: n = 5
integer :: u, ios, i
integer :: i4
real :: x
real :: a(n), b(n)
character(len=8) :: tag
integer :: pos_x

a = [10.0, 20.0, 30.0, 40.0, 50.0]
b = 0.0
i4 = 123456
x = 3.25
tag = 'example '

open(newunit=u, file='stream.bin', access='stream', form='unformatted', &
     status='replace', action='readwrite', iostat=ios)
if (ios /= 0) error stop 'open failed'

! write several objects with no record structure
write(u) i4
write(u) x
write(u) tag
write(u) a

! go back to start and read them
rewind(u)
read(u) i4
read(u) x
read(u) tag
read(u) b

print *, 'after first read'
print *, 'i4  =', i4
print *, 'x   =', x
print *, 'tag = ['//tag//']'
print *, 'b   =', b
print *

! overwrite x in place using pos=
! integer was written first, so x starts at byte storage_size(i4)/8 + 1
pos_x = storage_size(i4)/8 + 1
x = -9.5
write(u, pos=pos_x) x

! read x again directly
x = 0.0
read(u, pos=pos_x) x
print *, 'x after overwrite =', x
print *

! read only the array directly by computing its starting byte position
! bytes for integer + real + character(len=8)
read(u, pos=storage_size(i4)/8 + storage_size(x)/8 + len(tag) + 1) b
print *, 'array read directly with pos='
do i = 1, n
   print *, 'b(', i, ') =', b(i)
end do

close(u)

end program demo_stream_io

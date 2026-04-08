program demo_reshape
implicit none

integer :: a23(2,3)
integer :: a32(3,2)
integer :: a222(2,2,2)
integer :: b(2,4)

! example 1: reshape a vector into a 2 x 3 matrix
a23 = reshape([1,2,3,4,5,6], [2,3])

print *, "example 1: a23 = reshape([1,2,3,4,5,6], [2,3])"
print *, "a23(1,:) =", a23(1,:)
print *, "a23(2,:) =", a23(2,:)
print *, "a23 ="
call print_i2(a23)
print *

! example 2: reshape a vector into a 3 x 2 matrix
a32 = reshape([1,2,3,4,5,6], [3,2])

print *, "example 2: a32 = reshape([1,2,3,4,5,6], [3,2])"
print *, "a32(:,1) =", a32(:,1)
print *, "a32(:,2) =", a32(:,2)
print *, "a32 ="
call print_i2(a32)
print *

! example 3: reshape into a 3d array
a222 = reshape([1,2,3,4,5,6,7,8], [2,2,2])

print *, "example 3: a222 = reshape([1,2,3,4,5,6,7,8], [2,2,2])"
print *, "a222(:,:,1) ="
call print_i2(a222(:,:,1))
print *, "a222(:,:,2) ="
call print_i2(a222(:,:,2))
print *

! example 4: source too short, so use pad
b = reshape([1,2,3], [2,4], pad=[9,8])

print *, "example 4: reshape([1,2,3], [2,4], pad=[9,8])"
print *, "b ="
call print_i2(b)

contains

subroutine print_i2(x)
integer, intent(in) :: x(:,:)
integer :: i
do i = 1, size(x,1)
   print "(*(i6))", x(i,:)
end do
end subroutine print_i2

end program demo_reshape

program xreal8
! demonstrates the old real*8 declaration syntax
implicit none
real*8 :: x, y
real*4 :: z
integer*4 :: i
integer*8 :: j
complex*8 :: a
complex*16 :: b
a = cmplx(3.0, 4.0)
b = a
x = 1.25d0
y = 2.50d0
z = x + y

print *," a =", a
print *," b =", b
print *, "x =", x
print *, "y =", y
print *, "z =", z

print*,huge(i)
print*,huge(j)
end program xreal8

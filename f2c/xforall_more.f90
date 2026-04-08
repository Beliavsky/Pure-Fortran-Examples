program demo_forall
implicit none

integer, parameter :: n = 5
integer :: i, j
integer :: a(n), b(n), c(n), d(n)
integer :: m(3,3), t(3,3)

! example 1: simple forall statement
forall (i = 1:n) a(i) = i*i
print *, "example 1: a(i) = i*i"
print *, "a =", a
print *

! example 2: copy with an expression
forall (i = 1:n) b(i) = 10*a(i) + 1
print *, "example 2: b(i) = 10*a(i) + 1"
print *, "b =", b
print *

! example 3: masked forall statement
c = 0
forall (i = 1:n, mod(i,2) == 0) c(i) = -i
print *, "example 3: masked forall, only even i updated"
print *, "c =", c
print *

! example 4: forall construct with several assignments
forall (i = 1:n)
   d(i) = i
   a(i) = 100 + i
end forall
print *, "example 4: forall construct with multiple assignments"
print *, "d =", d
print *, "a =", a
print *

! example 5: build a matrix
forall (i = 1:3, j = 1:3) m(i,j) = 10*i + j
print *, "example 5: matrix m(i,j) = 10*i + j"
do i = 1, 3
   print "(3(i5))", m(i,:)
end do
print *

! example 6: transpose a matrix using forall
forall (i = 1:3, j = 1:3) t(i,j) = m(j,i)
print *, "example 6: transpose of m"
do i = 1, 3
   print "(3(i5))", t(i,:)
end do
print *

! example 7: upper triangular part only
m = 0
forall (i = 1:3, j = 1:3, i <= j) m(i,j) = i + j
print *, "example 7: fill upper triangle only"
do i = 1, 3
   print "(3(i5))", m(i,:)
end do

end program demo_forall

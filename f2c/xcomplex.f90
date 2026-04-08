program main
implicit none
complex :: z
z = (3.0, 4.0)
print*,z
z = cmplx(3.0, 4.0)
print*,z
print*,conjg(z)
print*,abs(z)
print*,z**2
print*,z%re, z%im
print*,real(z), aimag(z)
print*,sqrt((-1.0, 0.0))
end program main
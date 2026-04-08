program main
implicit none
integer, parameter :: n = 10**8
integer :: i, j, nprimes, largest
logical, allocatable :: is_prime(:)

allocate(is_prime(n))
is_prime = .true.
is_prime(1) = .false.

do i = 2, int(sqrt(real(n)))
   if (is_prime(i)) then
      do j = i*i, n, i
         is_prime(j) = .false.
      end do
   end if
end do

nprimes = 0
largest = 2

do i = 2, n
   if (is_prime(i)) then
      nprimes = nprimes + 1
      largest = i
   end if
end do

print *, "number of primes up to", n, "=", nprimes
print *, "largest prime =", largest

deallocate(is_prime)
end program main

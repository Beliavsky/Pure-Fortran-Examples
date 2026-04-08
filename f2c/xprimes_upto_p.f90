! transpiled by xp2f.py from xprimes_upto.py on 2026-02-24 22:01:07
program xprimes_upto
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none
   integer, parameter :: dp = real64
   integer :: n
   integer, allocatable :: p(:)
   
   n = 10 ** 6
   p = primes_upto(n)
   print *, size(p), "primes up to ", n
   print *, "last prime:", p(size(p))
   
   contains
   
   pure function primes_upto(n) result(primes_upto_result)
      integer, intent(in) :: n
      integer, allocatable :: primes_upto_result(:)
      integer :: cnt, i, k, limit
      logical, allocatable :: is_prime(:)
      integer, allocatable :: p(:)
      
      if (n < 2) then
         if (allocated(p)) deallocate(p)
         allocate(p(1:0))
         primes_upto_result = p
         return
      end if
      if (allocated(is_prime)) deallocate(is_prime)
      allocate(is_prime(1:n))
      is_prime = .true.
      is_prime(1) = .false.
      limit = int(sqrt(real(n, kind=dp)))
      do i = 2, limit
         if (is_prime(i)) then
            k = i * i
            do while (k <= n)
               is_prime(k) = .false.
               k = k + i
            end do
         end if
      end do
      cnt = 0
      do i = 2, n
         if (is_prime(i)) then
            cnt = cnt + 1
         end if
      end do
      if (allocated(p)) deallocate(p)
      allocate(p(1:cnt))
      cnt = 0
      do i = 2, n
         if (is_prime(i)) then
            cnt = cnt + 1
            p(cnt) = i
         end if
      end do
      primes_upto_result = p
   end function primes_upto
   
end program xprimes_upto

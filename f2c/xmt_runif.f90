module mt19937_runif

   use iso_fortran_env, only : int64, real64
   use mt19937_64, only : mt19937

   implicit none
   private

   public :: rng_seed
   public :: rng_seed_vector
   public :: runif
   public :: runif_1d
   public :: fill_runif_1d
   public :: fill_runif_2d

   integer(int64), parameter :: default_seed = 5489_int64

   type(mt19937), save :: rng
   logical, save :: rng_is_initialized = .false.

contains

   subroutine ensure_rng_initialized()
      if (.not. rng_is_initialized) then
         call rng%initialize(default_seed)
         rng_is_initialized = .true.
      end if
   end subroutine ensure_rng_initialized

   subroutine rng_seed(seed)
      integer(int64), intent(in) :: seed
      call rng%initialize(seed)
      rng_is_initialized = .true.
   end subroutine rng_seed

   subroutine rng_seed_vector(seed)
      integer(int64), intent(in) :: seed(:)
      call rng%initialize(seed)
      rng_is_initialized = .true.
   end subroutine rng_seed_vector

   real(real64) function runif()
      call ensure_rng_initialized()
      runif = rng%genrand64_real2()
   end function runif

   function runif_1d(n) result(x)
      integer, intent(in) :: n
      real(real64), allocatable :: x(:)
      integer :: i

      call ensure_rng_initialized()
      allocate(x(n))
      do i = 1, n
         x(i) = rng%genrand64_real2()
      end do
   end function runif_1d

   subroutine fill_runif_1d(x)
      real(real64), intent(out) :: x(:)
      integer :: i

      call ensure_rng_initialized()
      do i = 1, size(x)
         x(i) = rng%genrand64_real2()
      end do
   end subroutine fill_runif_1d

   subroutine fill_runif_2d(x)
      real(real64), intent(out) :: x(:, :)
      integer :: i
      integer :: j

      call ensure_rng_initialized()
      do j = 1, size(x, 2)
         do i = 1, size(x, 1)
            x(i, j) = rng%genrand64_real2()
         end do
      end do
   end subroutine fill_runif_2d

end module mt19937_runif


program main

   use iso_fortran_env, only : int64, real64
   use mt19937_runif, only : rng_seed, runif, runif_1d, fill_runif_1d

   implicit none

   real(real64), allocatable :: x(:)
   real(real64) :: y(3)

   call rng_seed(42_int64)
   print *, runif()

   x = runif_1d(3)
   print *, x

   call fill_runif_1d(y)
   print *, y

end program main

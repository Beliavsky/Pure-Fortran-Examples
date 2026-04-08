module in_mod
   use iso_fortran_env, only: real64
   implicit none
   private
   public :: operator(.in.)

   interface operator(.in.)
      module procedure in_integer
      module procedure in_real64
      module procedure in_character
      module procedure in_logical
   end interface

contains

   pure logical function in_integer(u, v)
   ! return any(u == v) for integer arguments
      integer, intent(in) :: u
      integer, intent(in) :: v(:)

      in_integer = any(u == v)
   end function in_integer

   pure logical function in_real64(u, v)
   ! return any(u == v) for real(kind=real64) arguments
      real(real64), intent(in) :: u
      real(real64), intent(in) :: v(:)

      in_real64 = any(u == v)
   end function in_real64

   pure logical function in_character(u, v)
   ! return any(u == v) for character arguments
      character(len=*), intent(in) :: u
      character(len=*), intent(in) :: v(:)

      in_character = any(u == v)
   end function in_character

   pure logical function in_logical(u, v)
   ! return any(u .eqv. v) for logical arguments
      logical, intent(in) :: u
      logical, intent(in) :: v(:)

      in_logical = any(u .eqv. v)
   end function in_logical

end module in_mod

program demo_in
   use iso_fortran_env, only: real64
   use in_mod, only: operator(.in.)
   implicit none

   integer :: ivals(5) = [1, 3, 5, 7, 9]
   real(real64) :: rvals(4) = [1.0_real64, 2.5_real64, 4.0_real64, 8.0_real64]
   character(len=5) :: names(3) = ['alice', 'bob  ', 'carol']
   logical :: flags(4) = [.true., .false., .false., .true.]

   print *, 5 .in. ivals
   print *, 6 .in. ivals

   print *, 2.5_real64 .in. rvals
   print *, 3.0_real64 .in. rvals

   print *, 'bob' .in. names
   print *, 'dave' .in. names

   print *, .true. .in. flags
   print *, .false. .in. flags

end program demo_in

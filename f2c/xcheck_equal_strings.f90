module m
implicit none
contains
elemental function default_logical(def,opt) result(tf)
! return opt if it is present, otherwise def (logical arguments and result)
logical, intent(in)           :: def
logical, intent(in), optional :: opt
logical                       :: tf
if (present(opt)) then
   tf = opt
else
   tf = def
end if
end function default_logical

subroutine check_equal_strings(xx,yy,xname,yname,caller,stop_error)
! check that each element of xx matches that of yy
character (len=*), intent(in)           :: xx(:),yy(:)
character (len=*), intent(in), optional :: xname,yname,caller
logical          , intent(in), optional :: stop_error
logical                                 :: stop_error_
character (len=100)                     :: xname_,yname_,call_msg
integer                                 :: n,ny,i,ibad
character (len=*), parameter            :: msg="in util_mod::check_equal_strings, "
stop_error_ = default_logical(.true.,stop_error)
stop_error_ = .true.
if (present(caller)) then
   call_msg = "called from " // trim(caller) // ","
else
   call_msg = ""
end if
! call set_optional(xname_,"x",xname)
! call set_optional(yname_,"y",yname)
xname_ = "x"
yname_ = "y"
! xname_ = default("x",xname)
! yname_ = default("y",yname)
n  = size(xx)
ny = size(yy)
if (n /= ny) then
   write (*,*) msg // trim(call_msg)," size(xx), size(yy) =",n,ny," must be equal"
   if (stop_error_) then
      write (*,*) "STOPPING"
      stop
   end if
end if
ibad = 0
do i=1,n
   if (xx(i) /= yy(i)) then
      ibad = i
      exit
   end if
end do
if (ibad /= 0) then
   write (*,*) msg // trim(call_msg)
   write (*,*) trim(xname_) // " =",(" " // trim(xx(i)),i=1,n)
   write (*,*) trim(yname_) // " =",(" " // trim(yy(i)),i=1,n)
   write (*,*) trim(xname_) // " not matching " // trim(yname_) // " for element ",ibad, &
               " '" // trim(xx(ibad)) // "' not equal to '" // trim(yy(ibad)) // "'"
   if (stop_error_) then
      write (*,*) "STOPPING"
      stop
   end if
end if
end subroutine check_equal_strings
end module m

program main
use m
implicit none
call check_equal_strings(["a"], ["b"])
end program main

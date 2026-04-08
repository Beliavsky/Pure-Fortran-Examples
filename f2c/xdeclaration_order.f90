module m
implicit none
contains

function f() result(c12)
character(len=*), parameter :: cdelim_ = "_"
character(len=len(cdelim_)) :: c12
c12 = cdelim_
end function f

end module m

program main
use m
implicit none
print "(a)", f()
end program main

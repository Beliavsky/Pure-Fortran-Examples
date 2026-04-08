program main
implicit none
type df
   real, allocatable :: x(:)
end type df
type(df) :: a
a%x = [10.0, 20.0]
print*,a%x
end program main
program main
implicit none
type df
   character (len=10), allocatable :: row_names(:), col_names(:)
   real, allocatable :: x(:,:)
end type df
type(df) :: a
allocate (a%x(3,2), source=0.0)
a%row_names = ["a", "b", "c"]
a%col_names = ["x1", "x2"]
print*,a%x
print*,a%row_names
print*,a%col_names
end program main
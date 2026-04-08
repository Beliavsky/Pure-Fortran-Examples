program main
implicit none
type df
   character (len=10), allocatable :: row_names(:), col_names(:)
   real, allocatable :: x(:,:)
end type df
type(df), allocatable :: a(:)
allocate (a(2))
allocate (a(1)%x(3,2), source=2.3)
a(1)%row_names = ["a", "b", "c"]
a(1)%col_names = ["x1", "x2"]
print*,a(1)%x
print*,a(1)%row_names
print*,a(1)%col_names
end program main
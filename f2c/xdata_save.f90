program data_counter_demo
implicit none

call show_count
call show_count
call show_count

end program data_counter_demo

subroutine show_count
implicit none

integer :: count
data count /0/

count = count + 1
print *, "count = ", count

end subroutine show_count

program xcommand_args
! demonstrates COMMAND_ARGUMENT_COUNT and GET_COMMAND_ARGUMENT
implicit none

integer :: narg, i, len_arg, status
character(len=100) :: arg

narg = command_argument_count()

print "(a,i0)", "number of command-line arguments = ", narg
print *

call get_command_argument(0, arg, len_arg, status)
print "(a)", "argument 0 (program name):"
print "(a)", "  value  = " // trim(arg)
print "(a,i0)", "  length = ", len_arg
print "(a,i0)", "  status = ", status
print *

do i = 1, narg
   call get_command_argument(i, arg, len_arg, status)
   print "(a,i0)", "argument ", i
   print "(a)", "  value  = " // trim(arg)
   print "(a,i0)", "  length = ", len_arg
   print "(a,i0)", "  status = ", status
   print *
end do

end program xcommand_args

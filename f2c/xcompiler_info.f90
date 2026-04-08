program xcompiler_info
use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
implicit none
character(len=:), allocatable :: version, options
version = compiler_version()
options = compiler_options()
print "(a)", "compiler_version():"
print "(a)", trim(version)
print "(a)", "compiler_options():"
print "(a)", trim(options)
end program xcompiler_info

use iso_fortran_env, only: output_unit
implicit none
write (unit=output_unit,fmt=*) "hi"
write (*,"(a)") "z"
write (*,fmt="(a)") "z"
write (unit=*,fmt="(a)") "z"
end

module m
implicit none
contains
function default_logical(def,opt) result(tf)
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

subroutine read_words_line(echo)
logical, intent(in) , optional :: echo
if (default_logical(.false.,echo)) then
   print*,"hi"
end if
end subroutine read_words_line
end module m

program main
implicit none
character (len=*), parameter :: newl = achar(10), &
                                lower_case = "abcdefghijklmnopqrstuvwxyz", &
                                upper_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
                                newline = char(10),bad_char="???"
print*,"abc", newl, lower_case, newline, upper_case
end program main


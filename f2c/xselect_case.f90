program xselect
implicit none
character (len=2) :: abbrev
character (len=20) :: state
abbrev = "MA"
select case (abbrev)
case ("MA")
   state = "Massachusetts"
case ("VA")
   state = "Virginia"
case ("WA")
   state = "Washington"
case default
   state = "???"
end select
print*,abbrev // " " // state
end program xselect

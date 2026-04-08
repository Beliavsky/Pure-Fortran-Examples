program xinquire_short
implicit none

character(len=*), parameter :: fname = "demo_inquire.txt"
integer :: u, ios
logical :: ex, op
character(len=256) :: name
character(len=20) :: access, form, action

print *, "before file exists:"
inquire(file=fname, exist=ex, opened=op, name=name)
print *, "  exist  =", ex
print *, "  opened =", op
print *, "  name   = [", trim(name), "]"

open(newunit=u, file=fname, status="replace", action="readwrite", &
     form="formatted", access="sequential", iostat=ios)
if (ios /= 0) error stop "open failed"

write(u, "(a)") "hello"

print *
print *, "after file exists and is open:"
inquire(file=fname, exist=ex, opened=op, name=name)
print *, "  exist  =", ex
print *, "  opened =", op
print *, "  name   = [", trim(name), "]"

print *
print *, "inquire on open unit:"
inquire(unit=u, opened=op, name=name, access=access, form=form, action=action)
print *, "  opened =", op
print *, "  name   = [", trim(name), "]"
print *, "  access = [", trim(access), "]"
print *, "  form   = [", trim(form), "]"
print *, "  action = [", trim(action), "]"

close(u)

end program xinquire_short

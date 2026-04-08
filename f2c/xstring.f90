program xstring_functions
  implicit none
  character(len=40) :: s
  character(len=40) :: t
  character(len=40) :: u
  integer :: p

  s = "  fortran strings  "
  t = "alpha,beta,gamma"

  print *, "s                = [" // s // "]"
  print *, "len(s)           = ", len(s)
  print *, "len_trim(s)      = ", len_trim(s)
  print *, "trim(s)          = [" // trim(s) // "]"
  print *, "adjustl(s)       = [" // adjustl(s) // "]"
  print *, "adjustr(s)       = [" // adjustr(s) // "]"
  print *, "repeat('*',5)    = [" // repeat("*",5) // "]"

  p = index(t,"beta")
  print *, "index(t,'beta')  = ", p

  p = scan(t,",")
  print *, "scan(t,',')      = ", p

  p = verify("12345","0123456789")
  print *, "verify('12345', digits) = ", p

  p = verify("12a45","0123456789")
  print *, "verify('12a45', digits) = ", p

  u = "hello world"
  print *, "u                = [" // u // "]"
  print *, "u(1:5)           = [" // u(1:5) // "]"
  print *, "u(7:)            = [" // u(7:) // "]"

  u(1:5) = "HELLO"
  print *, "after assignment = [" // u // "]"

  print *, "achar(65)        = [" // achar(65) // "]"
  print *, "iachar('A')      = ", iachar("A")

end program xstring_functions

program main
implicit none
real :: x, y(3) = [100.0, 200.0, 300.0]
x = 10.0
associate (x2 => 2*x, xp => x, yp => y(2:))
   xp = 3*x2
   print*,x2,xp,x,yp
   yp = 10*yp
   print*,y
end associate
end program main

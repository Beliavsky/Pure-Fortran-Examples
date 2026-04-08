program demo_backspace
implicit none

integer, parameter :: iu = 10
integer :: ios, cust, cust_next
real :: amt, amt_next, total

! Write a file of customer transactions.
open(iu, file="sales.txt", status="replace", action="write", form="formatted")
write(iu, "(i3,1x,f6.1)") 101, 10.0
write(iu, "(i3,1x,f6.1)") 101, 25.0
write(iu, "(i3,1x,f6.1)") 101,  5.0
write(iu, "(i3,1x,f6.1)") 205, 12.5
write(iu, "(i3,1x,f6.1)") 205,  7.5
write(iu, "(i3,1x,f6.1)") 310, 20.0
close(iu)

! Read the file and total sales by customer.
! We use BACKSPACE after reading one record too far.
open(iu, file="sales.txt", status="old", action="read", form="formatted")

print *, "customer totals"
do
   read(iu, "(i3,1x,f6.1)", iostat=ios) cust, amt
   if (ios /= 0) exit

   total = amt

   do
      read(iu, "(i3,1x,f6.1)", iostat=ios) cust_next, amt_next
      if (ios /= 0) exit

      if (cust_next /= cust) then
         backspace(iu)
         exit
      end if

      total = total + amt_next
   end do

   print "(i5,f8.1)", cust, total

   if (ios /= 0) exit
end do

close(iu)

end program demo_backspace

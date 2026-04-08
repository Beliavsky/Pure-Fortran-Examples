module date_mod
implicit none
private
public :: date, make_date
public :: operator(==), operator(/=), operator(<), operator(<=), operator(>), operator(>=)
public :: operator(+), operator(-)

type :: date
   integer :: year
   integer :: month
end type date

interface operator(==)
   module procedure date_eq
end interface

interface operator(/=)
   module procedure date_ne
end interface

interface operator(<)
   module procedure date_lt
end interface

interface operator(<=)
   module procedure date_le
end interface

interface operator(>)
   module procedure date_gt
end interface

interface operator(>=)
   module procedure date_ge
end interface

interface operator(+)
   module procedure date_plus_int
   module procedure int_plus_date
end interface

interface operator(-)
   module procedure date_minus_int
   module procedure date_minus_date
end interface

contains

function make_date(year, month) result(d)
integer, intent(in) :: year, month
type(date) :: d

if (month < 1 .or. month > 12) error stop "make_date: month must be between 1 and 12"
d%year = year
d%month = month
end function make_date

integer function month_index(d) result(idx)
type(date), intent(in) :: d

idx = 12*d%year + d%month - 1
end function month_index

function index_to_date(idx) result(d)
integer, intent(in) :: idx
type(date) :: d

d%year = idx / 12
d%month = mod(idx,12) + 1
if (d%month <= 0) then
   d%month = d%month + 12
   d%year = d%year - 1
end if
end function index_to_date

logical function date_eq(d1, d2)
type(date), intent(in) :: d1, d2

date_eq = d1%year == d2%year .and. d1%month == d2%month
end function date_eq

logical function date_ne(d1, d2)
type(date), intent(in) :: d1, d2

date_ne = .not. (d1 == d2)
end function date_ne

logical function date_lt(d1, d2)
type(date), intent(in) :: d1, d2

date_lt = month_index(d1) < month_index(d2)
end function date_lt

logical function date_le(d1, d2)
type(date), intent(in) :: d1, d2

date_le = month_index(d1) <= month_index(d2)
end function date_le

logical function date_gt(d1, d2)
type(date), intent(in) :: d1, d2

date_gt = month_index(d1) > month_index(d2)
end function date_gt

logical function date_ge(d1, d2)
type(date), intent(in) :: d1, d2

date_ge = month_index(d1) >= month_index(d2)
end function date_ge

function date_plus_int(d, n) result(ans)
type(date), intent(in) :: d
integer, intent(in) :: n
type(date) :: ans

ans = index_to_date(month_index(d) + n)
end function date_plus_int

function int_plus_date(n, d) result(ans)
integer, intent(in) :: n
type(date), intent(in) :: d
type(date) :: ans

ans = d + n
end function int_plus_date

function date_minus_int(d, n) result(ans)
type(date), intent(in) :: d
integer, intent(in) :: n
type(date) :: ans

ans = index_to_date(month_index(d) - n)
end function date_minus_int

integer function date_minus_date(d1, d2) result(nmonths)
type(date), intent(in) :: d1, d2

nmonths = month_index(d1) - month_index(d2)
end function date_minus_date

end module date_mod

program test_date_mod
use date_mod
implicit none

type(date) :: d1, d2, d3, d4, d5

d1 = make_date(2024, 11)
d2 = make_date(2025, 2)
d3 = make_date(2024, 11)

print *, "d1 = ", d1%year, d1%month
print *, "d2 = ", d2%year, d2%month
print *, "d3 = ", d3%year, d3%month
print *

print *, "comparison operators"
print *, "d1 == d3: ", d1 == d3
print *, "d1 /= d2: ", d1 /= d2
print *, "d1 <  d2: ", d1 <  d2
print *, "d1 <= d3: ", d1 <= d3
print *, "d2 >  d1: ", d2 >  d1
print *, "d2 >= d3: ", d2 >= d3
print *

d4 = d1 + 5
d5 = d2 - 7

print *, "date plus/minus integer operators"
print *, "d1 + 5 months = ", d4%year, d4%month
print *, "d2 - 7 months = ", d5%year, d5%month
d5 = 3 + d1
print *, "3 + d1        = ", d5%year, d5%month
print *

print *, "date minus date operator"
print *, "d2 - d1 in months = ", d2 - d1
print *, "d1 - d2 in months = ", d1 - d2
print *, "d3 - d1 in months = ", d3 - d1

end program test_date_mod

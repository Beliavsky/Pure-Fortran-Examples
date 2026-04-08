program demo_numeric_inquiry
implicit none

integer, parameter :: sp = selected_real_kind(p=6,  r=30)
integer, parameter :: dp = selected_real_kind(p=15, r=300)
integer, parameter :: i4 = selected_int_kind(9)
integer, parameter :: i8 = selected_int_kind(18)

real(sp) :: xsp
real(dp) :: xdp
integer(i4) :: i_small
integer(i8) :: i_big

xsp = 1.0_sp
xdp = 1.0_dp
i_small = 123_i4
i_big = 123_i8

print *, "selected kind values"
print *, "sp =", sp
print *, "dp =", dp
print *, "i4 =", i4
print *, "i8 =", i8
print *

print *, "real inquiry functions for single precision"
print *, "digits(xsp)      =", digits(xsp)
print *, "epsilon(xsp)     =", epsilon(xsp)
print *, "huge(xsp)        =", huge(xsp)
print *, "tiny(xsp)        =", tiny(xsp)
print *, "maxexponent(xsp) =", maxexponent(xsp)
print *, "minexponent(xsp) =", minexponent(xsp)
print *, "precision(xsp)   =", precision(xsp)
print *, "radix(xsp)       =", radix(xsp)
print *, "range(xsp)       =", range(xsp)
print *

print *, "real inquiry functions for double precision"
print *, "digits(xdp)      =", digits(xdp)
print *, "epsilon(xdp)     =", epsilon(xdp)
print *, "huge(xdp)        =", huge(xdp)
print *, "tiny(xdp)        =", tiny(xdp)
print *, "maxexponent(xdp) =", maxexponent(xdp)
print *, "minexponent(xdp) =", minexponent(xdp)
print *, "precision(xdp)   =", precision(xdp)
print *, "radix(xdp)       =", radix(xdp)
print *, "range(xdp)       =", range(xdp)
print *

print *, "integer inquiry functions"
print *, "bit_size(i_small) =", bit_size(i_small)
print *, "huge(i_small)     =", huge(i_small)
print *, "range(i_small)    =", range(i_small)
print *
print *, "bit_size(i_big)   =", bit_size(i_big)
print *, "huge(i_big)       =", huge(i_big)
print *, "range(i_big)      =", range(i_big)
print *

print *, "nearest examples"
print *, "xsp                =", xsp
print *, "nearest(xsp, 1.0)  =", nearest(xsp, 1.0_sp)
print *, "nearest(xsp,-1.0)  =", nearest(xsp,-1.0_sp)
print *, "spacing(xsp)       =", spacing(xsp)
print *
print *, "xdp                =", xdp
print *, "nearest(xdp, 1.0)  =", nearest(xdp, 1.0_dp)
print *, "nearest(xdp,-1.0)  =", nearest(xdp,-1.0_dp)
print *, "spacing(xdp)       =", spacing(xdp)
print *

print *, "exponent/set_exponent/fraction examples"
print *, "xdp =", xdp
print *, "exponent(10.0_dp)         =", exponent(10.0_dp)
print *, "fraction(10.0_dp)         =", fraction(10.0_dp)
print *, "set_exponent(0.75_dp, 10) =", set_exponent(0.75_dp, 10)
print *, "scale(1.0_dp, 5)          =", scale(1.0_dp, 5)
print *

print *, "selected_real_kind failure codes"
print *, "selected_real_kind(1000,1000) =", selected_real_kind(1000,1000)
print *

print *, "selected_int_kind failure code"
print *, "selected_int_kind(100) =", selected_int_kind(100)

end program demo_numeric_inquiry

program ximplicit_types
implicit double precision (a-h,o-z), integer(i-n)

x = 1.5d0
y = 2.0d0
sum = x + y

n = 4
m = 3
prod = n * m

average = sum / 2.0d0
ratio = prod / 2.0d0

print *, 'x       =', x
print *, 'y       =', y
print *, 'sum     =', sum
print *, 'n       =', n
print *, 'm       =', m
print *, 'prod    =', prod
print *, 'average =', average
print *, 'ratio   =', ratio

end program ximplicit_types

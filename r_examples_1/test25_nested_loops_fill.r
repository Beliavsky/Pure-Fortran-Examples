# test25_nested_loops_fill.r
n = 3
m = 4
a = matrix(0.0, nrow=n, ncol=m)
for (i in 1:n) {
  for (j in 1:m) {
    a[i,j] = i + 10*j
  }
}
print(a)

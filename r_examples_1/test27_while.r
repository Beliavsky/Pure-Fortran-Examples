# test27_while.r
x = 1.0
k = 0L
while (x < 100) {
  x = 1.5 * x + 1
  k = k + 1L
}
cat(k, x, "\n")

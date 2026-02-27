# test28_repeat_break.r
x = 1
k = 0L
repeat {
  x = 2*x
  k = k + 1L
  if (x >= 64) break
}
cat(k, x, "\n")

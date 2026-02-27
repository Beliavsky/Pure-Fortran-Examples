# test29_next.r
s = 0L
for (i in 1:10) {
  if (i %% 2 == 1) next
  s = s + i
}
cat(s, "\n")

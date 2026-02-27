# test35_apply.r
a = matrix(1:12, nrow=3, ncol=4)
r = apply(a, 1, sum)
c = apply(a, 2, sum)
cat(r, "\n")
cat(c, "\n")

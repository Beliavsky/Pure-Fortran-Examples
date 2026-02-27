# test22_diag.r
a = matrix(1:9, nrow=3, ncol=3)
d1 = diag(a)
d2 = diag(c(10, 20, 30))
cat(d1, "\n")
print(d2)

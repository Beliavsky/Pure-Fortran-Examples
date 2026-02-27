# test23_matmul.r
a = matrix(1:6, nrow=2, ncol=3)
b = matrix(1:12, nrow=3, ncol=4)
c = a %*% b
print(c)

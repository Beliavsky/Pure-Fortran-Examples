# test08_logical_ops.r
x = c(-2, -1, 0, 1, 2)
a = x > 0
b = (x >= -1) & (x <= 1)
c = (x < 0) | (x == 2)
cat(a, "\n")
cat(b, "\n")
cat(c, "\n")

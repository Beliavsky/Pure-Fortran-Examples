# test09_ifelse.r
x = -3:3
y = ifelse(x < 0, -x, x)
cat(y, "\n")

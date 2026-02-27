# test42_stopifnot.r
x = 1:5
stopifnot(length(x) == 5)
stopifnot(all(x > 0))
cat("ok\n")

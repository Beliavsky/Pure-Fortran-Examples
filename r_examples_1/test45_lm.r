# test45_lm.r
x = 1:5
y = c(1, 2, 1.3, 3.75, 2.25)
fit = lm(y ~ x)
cat(coef(fit), "\n")

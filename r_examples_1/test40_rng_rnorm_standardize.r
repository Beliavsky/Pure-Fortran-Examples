# test40_rng_rnorm_standardize.r
set.seed(1)
n = 6
x = rnorm(n)
z = (x - mean(x)) / sd(x)
cat(x, "\n")
cat(z, "\n")

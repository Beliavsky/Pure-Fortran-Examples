# test32_fun_vector.r
g = function(x) (x - mean(x)) / sqrt(mean((x - mean(x))^2))
x = c(1, 2, 3, 4, 5)
cat(g(x), "\n")

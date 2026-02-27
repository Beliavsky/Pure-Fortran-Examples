# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: rnorm and sample moments

pp_vec <- function(name, x) {
  cat(name, ":", paste(as.vector(x), collapse=" "), "\n", sep="")
}
pp_mat <- function(name, a) {
  cat(name, ":\n", sep="")
  for (i in seq_len(nrow(a))) cat(paste(a[i, ], collapse=" "), "\n", sep="")
}
pp_list_names <- function(name, x) {
  cat(name, ":", paste(names(x), collapse=" "), "\n", sep="")
}


set.seed(1)
x <- rnorm(1000)
m1 <- mean(x)
m2 <- var(x)
m3 <- mean((x - m1)^3)
m4 <- mean((x - m1)^4)
pp_vec("mean", sprintf("%.6f", m1))
pp_vec("var", sprintf("%.6f", m2))
pp_vec("m3", sprintf("%.6f", m3))
pp_vec("m4", sprintf("%.6f", m4))

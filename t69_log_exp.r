# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: log/exp and log1p/expm1

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


x <- c(1e-6, 1e-3, 0.1)
pp_vec("log(1+x)", sprintf("%.12f", log(1 + x)))
pp_vec("log1p(x)", sprintf("%.12f", log1p(x)))
pp_vec("exp(x)-1", sprintf("%.12f", exp(x) - 1))
pp_vec("expm1(x)", sprintf("%.12f", expm1(x)))

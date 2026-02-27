# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: abs/sign/pmax/pmin

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


x <- c(-3, -1, 0, 2)
y <- c(10, -10, 5, -5)
pp_vec("abs(x)", abs(x))
pp_vec("sign(y)", sign(y))
pp_vec("pmax(x,y)", pmax(x, y))
pp_vec("pmin(x,y)", pmin(x, y))

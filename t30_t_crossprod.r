# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: t(), %*%, crossprod

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


a <- matrix(1:6, nrow=2)
b <- matrix(10:21, nrow=3)
pp_mat("a", a)
pp_mat("b", b)
pp_mat("t(a)", t(a))
pp_mat("a %*% b", a %*% b)
pp_mat("crossprod(a)", crossprod(a))

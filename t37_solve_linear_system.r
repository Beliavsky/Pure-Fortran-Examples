# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: solve(A,b) for small system

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


a <- matrix(c(3,1,1, 1,3,1, 1,1,3), nrow=3, byrow=TRUE)
b <- c(1,2,3)
x <- solve(a, b)
pp_mat("a", a)
pp_vec("b", b)
pp_vec("x", x)

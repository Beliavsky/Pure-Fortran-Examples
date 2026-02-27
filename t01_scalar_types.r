# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Scalar types and coercion

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


n1 <- 4
n2 <- 4L
x <- 2.5
pp_vec("typeof(n1)", typeof(n1))
pp_vec("typeof(n2)", typeof(n2))
pp_vec("typeof(x)", typeof(x))
pp_vec("n1 + n2", n1 + n2)
pp_vec("as.integer(3.9)", as.integer(3.9))
pp_vec("as.numeric(3L)", as.numeric(3L))

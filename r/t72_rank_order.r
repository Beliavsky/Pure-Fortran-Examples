# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: rank and order

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


x <- c(10, 20, 10, 5)
pp_vec("rank", rank(x, ties.method="first"))
pp_vec("order", order(x))

# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: sort and order

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


x <- c(3, 1, 4, 1, 5)
pp_vec("sort(x)", sort(x))
pp_vec("sort(x,decreasing=TRUE)", sort(x, decreasing=TRUE))
pp_vec("order(x)", order(x))
pp_vec("x[order(x)]", x[order(x)])

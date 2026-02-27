# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Drop behavior for 1-row / 1-col subsets

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


a <- matrix(1:12, nrow=3)
r <- a[1, ]
c <- a[, 1]
pp_vec("typeof(r)", typeof(r))
pp_vec("length(r)", length(r))
pp_vec("typeof(c)", typeof(c))
pp_vec("length(c)", length(c))

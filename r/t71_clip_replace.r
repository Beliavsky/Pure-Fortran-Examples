# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Clipping via pmin/pmax and replacement

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


x <- c(-2, -1, 0, 1, 2, 3)
y <- pmin(pmax(x, 0), 2)
pp_vec("y", y)
x[x < 0] <- 0
x[x > 2] <- 2
pp_vec("x", x)

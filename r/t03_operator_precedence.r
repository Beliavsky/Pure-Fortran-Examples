# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Operator precedence

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


x <- 2
y <- 3
z <- 4
pp_vec("x + y * z", x + y * z)
pp_vec("(x + y) * z", (x + y) * z)
pp_vec("x^y^2", x^y^2)
pp_vec("(x^y)^2", (x^y)^2)

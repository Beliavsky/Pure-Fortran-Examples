# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: NA/NaN/Inf handling

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


x <- c(1, NA, NaN, Inf, -Inf, 2)
pp_vec("is.na", is.na(x))
pp_vec("is.nan", is.nan(x))
pp_vec("is.finite", is.finite(x))
pp_vec("sum(na.rm=TRUE)", sum(x, na.rm=TRUE))

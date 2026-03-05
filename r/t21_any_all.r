# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: any and all with NAs

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


x <- c(TRUE, NA, FALSE)
pp_vec("any(x)", any(x))
pp_vec("any(x,na.rm=TRUE)", any(x, na.rm=TRUE))
pp_vec("all(x)", all(x))
pp_vec("all(x,na.rm=TRUE)", all(x, na.rm=TRUE))

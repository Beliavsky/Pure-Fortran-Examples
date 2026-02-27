# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: stopifnot and tryCatch

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


safe_div <- function(a, b) {
  stopifnot(b != 0)
  a / b
}
ok <- tryCatch(safe_div(10, 2), error=function(e) {cat("error:", conditionMessage(e), "
"); NA})
bad <- tryCatch(safe_div(10, 0), error=function(e) {cat("error:", conditionMessage(e), "
"); NA})
pp_vec("ok", ok)
pp_vec("bad", bad)

# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: sweep for row/col operations

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
rs <- rowSums(a)
cs <- colSums(a)
pp_mat("a", a)
pp_mat("sweep_rows_minus", sweep(a, 1, rs, "-"))
pp_mat("sweep_cols_div", sweep(a, 2, cs, "/"))

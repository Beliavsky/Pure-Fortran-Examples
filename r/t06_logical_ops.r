# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Logical ops (&,|,!,&&,||)

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


a <- c(TRUE, FALSE, TRUE)
b <- c(FALSE, FALSE, TRUE)
pp_vec("a & b", a & b)
pp_vec("a | b", a | b)
pp_vec("!a", !a)
pp_vec("TRUE && FALSE", TRUE && FALSE)
pp_vec("TRUE || FALSE", TRUE || FALSE)

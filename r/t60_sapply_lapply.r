# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: lapply and sapply over list

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


lst <- list(1:3, 10:12, c(5,5,5))
s1 <- lapply(lst, sum)
s2 <- sapply(lst, sum)
pp_vec("unlist(lapply(sum))", unlist(s1))
pp_vec("sapply(sum)", s2)

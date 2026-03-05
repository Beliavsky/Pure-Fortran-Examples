# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: List creation and access

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


lst <- list(a=1, b=c(2,3), c=matrix(1:4, nrow=2))
pp_list_names("names", lst)
pp_vec("lst$a", lst$a)
pp_vec("lst$b", lst$b)
pp_mat("lst$c", lst$c)

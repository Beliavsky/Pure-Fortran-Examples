# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: seq and rep variants

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


pp_vec("1:5", 1:5)
pp_vec("seq(0,1,by=0.25)", seq(0, 1, by=0.25))
pp_vec("seq_len(4)", seq_len(4))
pp_vec("seq_along(c(9,8,7))", seq_along(c(9,8,7)))
pp_vec("rep(1:3, times=2)", rep(1:3, times=2))
pp_vec("rep(1:3, each=2)", rep(1:3, each=2))

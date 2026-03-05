# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Matrix indexing (drop and drop=FALSE)

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
pp_vec("a[2,]", a[2, ])
pp_vec("a[,3]", a[, 3])
b <- a[, 3, drop=FALSE]
pp_vec("dim(b)", dim(b))
pp_mat("b", b)

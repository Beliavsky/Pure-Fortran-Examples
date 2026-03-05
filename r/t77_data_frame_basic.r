# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: data.frame basics and column access

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


df <- data.frame(id=1:3, x=c(10,20,30), y=c(1.5, 2.5, 3.5))
pp_vec("nrow", nrow(df))
pp_vec("ncol", ncol(df))
pp_vec("df$x", df$x)
pp_vec("df[2,'y']", df[2, "y"])

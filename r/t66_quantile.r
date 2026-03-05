# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: quantile types default

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


x <- c(1,2,3,4,100)
q <- quantile(x, probs=c(0,0.25,0.5,0.75,1))
pp_vec("q", as.numeric(q))

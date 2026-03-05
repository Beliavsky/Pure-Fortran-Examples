# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Function returns list (multiple outputs)

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


h <- function(x) {
  list(sum=sum(x), mean=mean(x), n=length(x))
}
out <- h(1:5)
pp_list_names("names(out)", out)
pp_vec("out$sum", out$sum)
pp_vec("out$mean", out$mean)
pp_vec("out$n", out$n)

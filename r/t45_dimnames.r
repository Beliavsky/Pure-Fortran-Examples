# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: dimnames and indexing by names

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


a <- matrix(1:9, nrow=3)
rownames(a) <- c("r1","r2","r3")
colnames(a) <- c("c1","c2","c3")
pp_vec("rownames", rownames(a))
pp_vec("colnames", colnames(a))
pp_vec("a['r2','c3']", a["r2","c3"])

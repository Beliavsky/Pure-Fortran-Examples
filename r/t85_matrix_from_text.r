# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Read matrix from a string

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


txt <- "1 2 3
4 5 6
7 8 9
"
con <- textConnection(txt)
m <- matrix(scan(con, quiet=TRUE), nrow=3, byrow=TRUE)
close(con)
pp_mat("m", m)

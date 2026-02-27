# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: paste/paste0/sprintf

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


a <- 3
b <- 4.5
s1 <- paste("a", a, "b", b)
s2 <- paste0("a=", a, ",b=", b)
s3 <- sprintf("a=%d b=%.1f", a, b)
cat("s1:", s1, "
")
cat("s2:", s2, "
")
cat("s3:", s3, "
")

# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Vectorized vs loop should match

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


x <- 1:10
y1 <- x * x + 2 * x + 1
y2 <- numeric(length(x))
for (i in seq_along(x)) y2[i] <- x[i] * x[i] + 2 * x[i] + 1
pp_vec("all.equal", all.equal(y1, y2))

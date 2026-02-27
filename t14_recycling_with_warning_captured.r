# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Recycling with warning captured

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


x <- 1:5
y <- c(10, 20)
out <- tryCatch(
  x + y,
  warning=function(w) {
    cat("warning:", conditionMessage(w), "
", sep="")
    invokeRestart("muffleWarning")
  }
)
pp_vec("x+y", out)

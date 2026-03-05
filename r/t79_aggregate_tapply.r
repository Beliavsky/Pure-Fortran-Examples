# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: tapply and aggregate

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


x <- c(1,2,3,4,5,6)
g <- c("a","a","b","b","b","a")
t <- tapply(x, g, sum)
pp_vec("tapply_sum_a_b", as.numeric(t))
df <- data.frame(x=x, g=g)
agg <- aggregate(x ~ g, data=df, FUN=sum)
pp_vec("aggregate_g", agg$g)
pp_vec("aggregate_x", agg$x)

# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: data.frame subset and transform

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


df <- data.frame(a=1:6, b=c(1,1,2,2,3,3))
df2 <- subset(df, b >= 2)
pp_vec("df2$a", df2$a)
df$z <- df$a * 10 + df$b
pp_vec("df$z", df$z)

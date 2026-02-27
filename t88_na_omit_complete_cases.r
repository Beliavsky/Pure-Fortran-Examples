# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: na.omit and complete.cases

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


df <- data.frame(a=c(1,NA,3), b=c(10,20,NA))
pp_vec("complete.cases", complete.cases(df))
df2 <- na.omit(df)
pp_vec("nrow(na.omit)", nrow(df2))
pp_vec("df2$a", df2$a)
pp_vec("df2$b", df2$b)

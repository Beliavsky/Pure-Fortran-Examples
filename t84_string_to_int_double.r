# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Read int and doubles from a string

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


line <- "7 3.14 -2.5"
con <- textConnection(line)
i <- scan(con, what=integer(), n=1, quiet=TRUE)
x <- scan(con, what=double(), n=2, quiet=TRUE)
close(con)
pp_vec("i", i)
pp_vec("x", x)

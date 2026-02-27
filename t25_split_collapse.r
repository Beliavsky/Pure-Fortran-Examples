# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: strsplit and collapse

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


s <- "10,20,30"
parts <- strsplit(s, ",")[[1]]
pp_vec("parts", parts)
nums <- as.integer(parts)
pp_vec("nums", nums)
cat("collapse:", paste(nums, collapse="|"), "
")

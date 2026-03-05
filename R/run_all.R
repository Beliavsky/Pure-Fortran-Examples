# R -> Fortran transpiler test case
# Each script prints results using cat() for easier diffing.
# Base R only. ASCII only.
# Title: Run all tests and write .out files

files <- list.files(pattern="^t[0-9][0-9]_.*\\.r$")
cat("n_files:", length(files), "\n")
for (f in files) {
  out_file <- sub("\\.r$", ".out", f)
  cat("running:", f, "->", out_file, "\n")
  txt <- capture.output(source(f, echo=FALSE, print.eval=TRUE))
  writeLines(txt, out_file)
}
cat("done\n")

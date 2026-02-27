# test03_seq_colon.r
n = 5
a = 1:n
b = seq(1, n, by=1)
c = seq_len(n)
cat(a, "\n")
cat(b, "\n")
cat(c, "\n")

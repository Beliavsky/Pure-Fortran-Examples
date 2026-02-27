# test12_index_logical.r
x = 1:10
mask = (x %% 2) == 0
cat(x[mask], "\n")

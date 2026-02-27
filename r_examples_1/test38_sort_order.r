# test38_sort_order.r
x = c(3, 1, 4, 1, 5)
cat(sort(x), "\n")
idx = order(x)
cat(idx, "\n")
cat(x[idx], "\n")

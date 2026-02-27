# test34_list_return.r
stats2 = function(x) list(mean=mean(x), sd=sqrt(mean((x-mean(x))^2)))
x = c(1, 2, 4, 8)
r = stats2(x)
cat(r$mean, r$sd, "\n")

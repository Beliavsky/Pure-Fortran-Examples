# test33_fun_defaults.r
h = function(x, a=2.0, b=1.0) a*x + b
cat(h(3), "\n")
cat(h(3, a=10, b=-1), "\n")

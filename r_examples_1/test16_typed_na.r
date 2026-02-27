# test16_typed_na.r
a = c(1L, NA_integer_)
b = c(1.0, NA_real_)
c = c("x", NA_character_)
cat(typeof(a), "\n")
cat(typeof(b), "\n")
cat(typeof(c), "\n")

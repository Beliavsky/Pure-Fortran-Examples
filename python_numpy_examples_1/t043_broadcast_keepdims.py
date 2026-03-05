import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(12).reshape(3,4).astype(float)
m = a.mean(axis=1, keepdims=True)
b = a - m
print("a", a)
print("m", m)
print("b", b)
print("row_means_of_b", b.mean(axis=1))

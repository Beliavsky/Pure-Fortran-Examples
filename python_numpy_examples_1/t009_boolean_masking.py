import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(12).reshape(3,4)
mask = (a % 3) == 0
print("a", a)
print("mask", mask.astype(int))
print("a[mask]", a[mask])

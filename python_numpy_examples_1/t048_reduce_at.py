import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1,2,3,4,5,6], dtype=float)
idx = np.array([0, 2, 5])
y = np.add.reduceat(x, idx)
print("x", x)
print("idx", idx)
print("reduceat", y)

import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(10) * 10
idx = np.array([3, 0, 7, 7, 2])
print("a", a)
print("idx", idx)
print("a[idx]", a[idx])

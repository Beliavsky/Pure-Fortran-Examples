import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([0, 1, 2], dtype=np.int64)
b = np.array([1, 2, 3], dtype=np.int64)
assert (a > 0).any() is True
assert (a > 0).all() is False
assert (b > 0).all() is True
print("PASS")

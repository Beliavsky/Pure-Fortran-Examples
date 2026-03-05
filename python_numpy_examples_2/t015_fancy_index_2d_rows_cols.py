import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(16, dtype=np.int64).reshape(4, 4)
rows = np.array([0, 2, 3], dtype=np.int64)
cols = np.array([1, 1, 3], dtype=np.int64)
b = a[rows, cols]
assert (b == np.array([1, 9, 15], dtype=np.int64)).all()
print("PASS")

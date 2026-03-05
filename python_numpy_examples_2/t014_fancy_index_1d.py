import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([10, 11, 12, 13, 14], dtype=np.int64)
idx = np.array([4, 0, 3], dtype=np.int64)
b = a[idx]
assert (b == np.array([14, 10, 13], dtype=np.int64)).all()
print("PASS")

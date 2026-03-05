import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([0, 1, 1, 3, 3, 3], dtype=np.int64)
bc = np.bincount(a, minlength=5)
assert (bc == np.array([1, 2, 0, 3, 0], dtype=np.int64)).all()
print("PASS")

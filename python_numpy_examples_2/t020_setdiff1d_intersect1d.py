import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3, 4], dtype=np.int64)
b = np.array([3, 4, 5], dtype=np.int64)
sd = np.setdiff1d(a, b)
it = np.intersect1d(a, b)
assert (sd == np.array([1, 2], dtype=np.int64)).all()
assert (it == np.array([3, 4], dtype=np.int64)).all()
print("PASS")

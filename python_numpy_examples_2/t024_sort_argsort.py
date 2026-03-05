import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([3, 1, 2, 1], dtype=np.int64)
s = np.sort(a)
idx = np.argsort(a)
assert (s == np.array([1, 1, 2, 3], dtype=np.int64)).all()
assert (a[idx] == s).all()
print("PASS")

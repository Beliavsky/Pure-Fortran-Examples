import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([10, 13, 19, 20], dtype=np.int64)
d1 = np.diff(a, n=1)
d2 = np.diff(a, n=2)
assert (d1 == np.array([3, 6, 1], dtype=np.int64)).all()
assert (d2 == np.array([3, -5], dtype=np.int64)).all()
print("PASS")

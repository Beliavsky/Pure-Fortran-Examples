import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3], dtype=np.int64)
t = np.tile(a, 2)
r = np.repeat(a, 2)
assert (t == np.array([1, 2, 3, 1, 2, 3], dtype=np.int64)).all()
assert (r == np.array([1, 1, 2, 2, 3, 3], dtype=np.int64)).all()
print("PASS")

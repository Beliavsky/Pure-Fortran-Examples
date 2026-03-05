import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 5, 2], dtype=np.int64)
b = np.array([3, 4, 2], dtype=np.int64)
mx = np.maximum(a, b)
mn = np.minimum(a, b)
assert (mx == np.array([3, 5, 2], dtype=np.int64)).all()
assert (mn == np.array([1, 4, 2], dtype=np.int64)).all()
print("PASS")

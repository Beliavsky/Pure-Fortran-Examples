import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([-2, -1, 0, 1, 2], dtype=np.int64)
m = a > 0
b = a[m]
assert (m == np.array([False, False, False, True, True])).all()
assert (b == np.array([1, 2], dtype=np.int64)).all()
print("PASS")

import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 3, 5, 7], dtype=np.int64)
i0 = np.searchsorted(a, 0)
i1 = np.searchsorted(a, 4)
i2 = np.searchsorted(a, 7)
assert i0 == 0
assert i1 == 2
assert i2 == 3
print("PASS")

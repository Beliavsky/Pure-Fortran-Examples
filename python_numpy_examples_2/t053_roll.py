import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3, 4, 5], dtype=np.int64)
b = np.roll(a, 2)
c = np.roll(a, -1)
assert (b == np.array([4, 5, 1, 2, 3], dtype=np.int64)).all()
assert (c == np.array([2, 3, 4, 5, 1], dtype=np.int64)).all()
print("PASS")

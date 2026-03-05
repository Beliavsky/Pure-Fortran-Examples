import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([10, 20, 30, 40, 50], dtype=np.int64)
b = a[::-1]
c = a[::2]
assert (b == np.array([50, 40, 30, 20, 10], dtype=np.int64)).all()
assert (c == np.array([10, 30, 50], dtype=np.int64)).all()
print("PASS")

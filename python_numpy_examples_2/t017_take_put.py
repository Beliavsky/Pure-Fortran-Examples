import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([10, 20, 30, 40, 50], dtype=np.int64)
idx = np.array([0, 3, 4], dtype=np.int64)
b = np.take(a, idx)
np.put(a, np.array([1, 2], dtype=np.int64), np.array([200, 300], dtype=np.int64))
assert (b == np.array([10, 40, 50], dtype=np.int64)).all()
assert (a == np.array([10, 200, 300, 40, 50], dtype=np.int64)).all()
print("PASS")
